(**************************************************************************)
(*                                                                        *)
(*  The Major Major mailing list manager                                  *)
(*  Copyright (C) 2015   Peter Moylan                                     *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 3 of the License, or     *)
(*  (at your option) any later version.                                   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>. *)
(*                                                                        *)
(*  To contact author:   http://www.pmoylan.org   peter@pmoylan.org       *)
(*                                                                        *)
(**************************************************************************)

IMPLEMENTATION MODULE ListChecker;

        (********************************************************)
        (*                                                      *)
        (*                Mailing list manager                  *)
        (*         Module to handle mail coming in for          *)
        (*               the individual lists                   *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            23 May 2000                     *)
        (*  Last edited:        21 May 2015                     *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT
    (* type *)  CARD8, ADDRESS,
    (* proc *)  ADR;

IMPORT OS2, Strings;

FROM MailFetcher IMPORT
    (* type *)  MailUser,
    (* proc *)  RegisterMailUser, UnregisterMailUser,
                SetLoginInfo, FirstItem, DeleteFirstItem;

FROM AddressLists IMPORT
    (* const*)  EmailAddressSize,
    (* type *)  ListNameType, EmailAddress, AddressList, OptionType, OptionSet,
                BufferPointer,
    (* proc *)  NameMatch, CreateAddressList, DiscardAddressList,
                AddRecipient, RemoveRecipient, GetDigestMembers,
                MemberFlags, IsOnList, EncodeList, FirstOnList, NextOnList,
                EmptyList, MergeAndResave;

FROM Archives IMPORT
    (* type *)  Archive, TimeType,
    (* proc *)  FileListing, RegisterArchive, DeregisterArchive,
                SetLoggingLanguage, AddToArchive, AppendDigest;

FROM MailOut IMPORT
    (* proc *)  DeliverItem;

FROM LogLevel IMPORT
    (* type *)  LogLevelType;

FROM INIData IMPORT
    (* type *)  HINI, StringReadState,
    (* proc *)  OpenINIFile, CloseINIFile, ItemSize, INIGet, INIGetString,
                INIPut, INIPutBinary, INIDeleteKey, INIValid,
                GetStringList, NextString, CloseStringList;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  OpenLogContext, CloseLogContext, CreateLogID, DiscardLogID, LogTransaction;

FROM SplitScreen IMPORT
    (* proc *)  NotDetached, ClearScreen, SetBoundary,
                ReleaseScreen, RegainScreen;

FROM MyClock IMPORT
    (* proc *)  CurrentDateAndTime, CurrentTimeToString;

FROM TimeConv IMPORT
    (* proc *)  time;

FROM FileOps IMPORT
    (* const*)  FilenameLength, NoSuchChannel,
    (* type *)  ChanId, FilenameString,
    (* proc *)  Exists, OpenOldFile, CloseFile, DeleteFile, CopyFile,
                ReadRaw, ReadLine, FWriteChar, FWriteString, FWriteLn,
                FWriteLJCard;

FROM Languages IMPORT
    (* type *)  LangHandle, LangName,
    (* proc *)  UseLanguage, DropLanguage, LanguageCode, LWriteString,
                LWriteStringA, LWriteStringN, StrToBuffer, StrToBufferA,
                StrToBufferAB, StrToBufferN, StrToBufferAN;

FROM Misc IMPORT
    (* proc *)  OpenNewOutputFile, HeadMatch, ExtractEmailAddress,
                MakeNewFilename, AppendCard;

FROM MD5 IMPORT
    (* type *)  MD5_CTX, MD5_DigestType,
    (* proc *)  MD5Init, MD5Update, MD5Final, MD5DigestToString;

FROM Inet2Misc IMPORT
    (* proc *)  ConvertCard;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, DestroySemaphore, Wait, Signal;

FROM Timer IMPORT
    (* proc *)  TimedWait;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateTask, CreateTask1,
                CreateLock, DestroyLock, Obtain, Release;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);
    Tab = CHR(9);
    Space = ' ';
    CtrlZ = CHR(26);
    DefaultInterval = 3*60;           (* seconds *)
    AbbrevChars = 32;

TYPE
    AbbreviationType = ARRAY [0..AbbrevChars-1] OF CHAR;
    LineBuffer = ARRAY [0..1023] OF CHAR;

    (* A confirmation list holds all the pending subscription and       *)
    (* unsubscription requests that will not become firm unless the     *)
    (* proposed list member confirms within a given time limit.  If     *)
    (* the confirmation doesn't arrive, we discard the request.  The    *)
    (* list is kept in time order, earliest request first.              *)

    ConfirmationListPointer = POINTER TO
                                  RECORD
                                      next: ConfirmationListPointer;
                                      time: CARDINAL;
                                      code: ConfirmationCode;
                                      IsSubscription: BOOLEAN;
                                      flags: OptionSet;
                                      address: EmailAddress;
                                      lang: LangName;
                                  END (*RECORD*);

    (* In a ConfirmationList, the ConfTime field specifies the time     *)
    (* (in hours) we allow for a subscriber or unsubscriber to supply   *)
    (* a confirmation.                                                  *)

    ConfirmationList = RECORD
                           access: Lock;
                           ConfTime: CARDINAL;
                           head: ConfirmationListPointer;
                       END (*RECORD*);

    MailingList = POINTER TO MailingListInfo;

    (* For each mailing list, we keep the following information.        *)
    (*   next        next entry on the event list                       *)
    (*   access      critical section protection                        *)
    (*   interval    time between checks for this list (seconds)        *)
    (*   checktime   the next time a check is due      (seconds)        *)
    (*   RFC2919ID   (optional) this list's ID according to RFC 2919    *)
    (*   charset     the MIME charset for this list                     *)
    (*   ListID      this list's ID as a mail user                      *)
    (*   abbreviation  short form of list name                          *)
    (*   language    which language to use for messages.                *)
    (*   lang        the handle that specifies the language to use for  *)
    (*                 messages.                                        *)
    (*   LoginName   the POP3 login name; normally the same as the      *)
    (*                 list name, but there can be atypical cases.      *)
    (*   password    the POP3 password                                  *)
    (*   LogID       ID to use on the transaction log                   *)
    (*   Owners      e-mail addresses of the owners/moderators          *)
    (*   Members     the e-mail addresses on this list                  *)
    (*   OurName     textual name of the list                           *)
    (*   OurEmailAddress   e-mail address of the list                   *)
    (*   MailErrorsTo the e-mail address of the person who gets the     *)
    (*                  bounce messages.  Can be null.                  *)
    (*   Add2369Headers  add extra mail headers, see RFC 2369           *)
    (*   DMARCcompatible modify headers to satisfy DMARC.               *)
    (*   Moderated   TRUE iff this is a moderated list.                 *)
    (*   SuppressFrom TRUE iff we want to delete From: headers          *)
    (*   KillAttachments  TRUE if we want to remove all attachments.    *)
    (*   DigestTaskRunning  TRUE if we are running the digest task.     *)
    (*   ArchiveMessages  TRUE if we want to archive messages.          *)
    (*   archive     handle to the archive for this list.               *)
    (*   DigestSemaphore  semaphore on which the archiver signals       *)
    (*                    each time a new digest is created.            *)
    (*   DigestUsers count of the number of digest subscribers.         *)
    (*   MemberCount total number of members, including digest users.   *)
    (*   Leader      file name for material to tack onto the start of   *)
    (*                  every message (after the header lines).         *)
    (*   Trailer     file name for material to tack onto the tail of    *)
    (*                  every message.                                  *)
    (*   NonsubOption how to handle mail from nonsubscribers: 0=ignore, *)
    (*                1=send failure message, 2=accept                  *)
    (*   mark        flag used while refreshing the information         *)
    (*   obsolete    TRUE if this list should be removed                *)
    (*   SaveRejects TRUE iff we want to keep a copy of rejected and    *)
    (*                  filtered and ignored mail.                      *)
    (*   RejectDir   Directory to hold items to be saved if SavedRejects*)
    (*                  is true.  Does not end with a '\'.              *)
    (*   FilterProg  Script to run after receiving a mail item.         *)
    (*   NotifyOwnerMessage  Message to mail to list owners on          *)
    (*                      successful 'subscribe' by a new member      *)
    (*   NotifyOwnerUnsubMessage  As above, but for 'unsubscribe'       *)
    (*   WelcomeMessage  Message to mail on successful 'subscribe'      *)
    (*   DepartureMessage  Message to mail on successful 'unsubscribe'  *)
    (*   ConfirmationRequestMessage                                     *)
    (*               Message to send on 'subscribe' or 'unsubscribe'    *)
    (*                  when confirmations are required                 *)
    (*   Enabled     Permission for non-owner to execute some           *)
    (*                  controlled commands.                            *)
    (*   RequireConfirmation  TRUE if we require a confirmation for     *)
    (*               each subscribe or unsubscribe operation            *)
    (*   OwnersMayConfUnsub  If TRUE, a confirmation request is sent to *)
    (*               the owners as well as the requester on an          *)
    (*               'unsubscribe'                                      *)
    (*   Pending     List of operations that are waiting on a           *)
    (*                   confirmation.                                  *)
    (*                                                                  *)
    (* Each time we do a check, we add a new event record for time      *)
    (* (now + interval).  Sometimes we also force an early check.       *)

    MailingListInfo = RECORD
                          next: MailingList;
                          access: Lock;
                          interval: CARDINAL;
                          checktime: CARDINAL;
                          RFC2919ID: ARRAY [0..255] OF CHAR;
                          charset: ARRAY [0..63] OF CHAR;
                          ListID: MailUser;
                          abbreviation: AbbreviationType;
                          language, password: ARRAY [0..31] OF CHAR;
                          lang: LangHandle;
                          LogID: TransactionLogID;
                          Owners: AddressList;
                          Members: AddressList;
                          OurName, LoginName: ListNameType;
                          OurEmailAddress: EmailAddress;
                          Leader, Trailer: FilenameString;
                          MailErrorsTo: EmailAddress;
                          Add2369Headers: BOOLEAN;
                          DMARCcompatible: BOOLEAN;
                          Moderated: BOOLEAN;
                          SuppressFrom: BOOLEAN;
                          KillAttachments: BOOLEAN;
                          DigestTaskRunning: BOOLEAN;
                          ArchiveMessages: BOOLEAN;
                          archive: Archive;
                          DigestSemaphore: Semaphore;
                          DigestUsers, MemberCount: CARDINAL;
                          NonsubOption: CARD8;
                          mark, obsolete: BOOLEAN;
                          SaveRejects: BOOLEAN;
                          RejectDir: FilenameString;
                          FilterProg: FilenameString;
                          NotifyOwnerMessage: FilenameString;
                          NotifyOwnerUnsubMessage: FilenameString;
                          WelcomeMessage: FilenameString;
                          DepartureMessage: FilenameString;
                          ConfirmationRequestMessage: FilenameString;
                          Enabled: RECORD
                                       Subscribe1,
                                       Subscribe2,
                                       Unsubscribe2,
                                       Who           : BOOLEAN;
                                   END (*RECORD*);
                          RequireConfirmation: BOOLEAN;
                          OwnersMayConfUnsub: BOOLEAN;
                          Pending: ConfirmationList;
                      END (*RECORD*);

    AllListNodePtr = POINTER TO RECORD
                                    next: AllListNodePtr;
                                    this: MailingList;
                                END (*RECORD*);

VAR
    (* Our INI file name and mode. *)

    INIFileName: FilenameString;

    UseTNI: BOOLEAN;

    (* List of all lists. *)

    AllLists: RECORD
                  access: Lock;
                  head: AllListNodePtr;
                  count: CARDINAL;
              END (*RECORD*);

    (* The name of our e-mail domain. *)

    OurDomain: EmailAddress;

    (* The e-mail address of the administrator account. *)

    AdministratorAddress: EmailAddress;

    (* The language to use for administrator functions, e.g. logging, *)
    (* and the language to use when no language specified.            *)

    AdminLang, DefaultLang: LangHandle;

    (* The event list is a time-ordered list of things to check. *)

    EventList: RECORD
                   access: Lock;
                   head: MailingList;
               END (*RECORD*);

    (* Signal on this semaphore to trigger a check of the event list. *)

    CheckEventList: Semaphore;

    (* Message to send if someone is not subscribed, and its critical   *)
    (* section protection lock.                                         *)

    NotSubscribedMessage: FilenameString;
    NSMLock: Lock;

    (* Amount of detail to include in the transaction log. *)

    LogLevel: LogLevelType;

    (* Shutdown flags. *)

    ShutdownRequest: BOOLEAN;
    TaskDone: Semaphore;
    DigestTaskDone: Semaphore;

(************************************************************************)
(*                       SENDING A CANNED MESSAGE                       *)
(************************************************************************)

PROCEDURE FindFile (userlang: LangHandle;  logID: TransactionLogID;
                              VAR (*INOUT*) file: ARRAY OF CHAR): BOOLEAN;

    (* Expands the %L macro in "file", if present.  Uses userlang if    *)
    (* possible, and DefaultLang if the first attempt produces a        *)
    (* nonexistent file.  If the file still can't be found, logs an     *)
    (* error message and returns FALSE.                                 *)

    VAR langname: ARRAY [0..31] OF CHAR;

    (********************************************************************)

    PROCEDURE ExpandFileName(): BOOLEAN;

        (* The parameter 'file' might itself contain a '%L' macro, and  *)
        (* if so that is expanded to the langname string.  Returns TRUE *)
        (* iff the file exists.                                         *)

        VAR pos: CARDINAL;  found: BOOLEAN;

        BEGIN
            Strings.Capitalize (file);
            Strings.FindNext ('%L', file, 0, found, pos);
            IF found THEN
                Strings.Delete (file, pos, 2);
                Strings.Insert (langname, pos, file);
            END (*IF*);
            RETURN Exists(file);
        END ExpandFileName;

    (********************************************************************)

    VAR original: FilenameString;  success: BOOLEAN;
        message: ARRAY [0..255] OF CHAR;

    BEGIN        (* body of DecodeFileName *)
        Strings.Assign (file, original);
        LanguageCode (userlang, langname);
        success := ExpandFileName();
        IF NOT success THEN
            Strings.Assign (original, file);
            LanguageCode (DefaultLang, langname);
            success := ExpandFileName();
        END (*IF*);
        IF NOT success THEN
            Strings.Assign ("Missing message file ", message);
            Strings.Append (original, message);
            LogTransaction (logID, message);
        END (*IF*);
        RETURN success;
    END FindFile;

(************************************************************************)

PROCEDURE AppendFromFile (dstcid: ChanId;  L: MailingList;
                                    userlang: LangHandle;
                                    file: ARRAY OF CHAR;
                                    From, ExtraParam: ARRAY OF CHAR);

    (* Appends the contents of the named file to dstcid (an output file *)
    (* that is already open).  Parameter L is present to allow some     *)
    (* macro expansion of % codes in the file, and From allows %S       *)
    (* expansion.  ExtraParam is irrelevant in the majority of cases,   *)
    (* but is used where the caller has to transfer a value to be used  *)
    (* for the expansion of the %X macro.                               *)

    VAR srccid: ChanId;

    (********************************************************************)

    PROCEDURE ReadChar (VAR (*OUT*) ch: CHAR): BOOLEAN;

        VAR amount: CARDINAL;

        BEGIN
            ReadRaw (srccid, ch, SIZE(ch), amount);
            RETURN amount > 0;
        END ReadChar;

    (********************************************************************)

    VAR ch: CHAR;  continue, OptionFlag: BOOLEAN;

    BEGIN
        IF FindFile (userlang, L^.LogID, file) THEN
            srccid := OpenOldFile (file, FALSE, FALSE);
        ELSE
            srccid := NoSuchChannel;
        END (*IF*);
        IF srccid <> NoSuchChannel THEN
            OptionFlag := FALSE;  ch := CHR(0);
            REPEAT
                continue := ReadChar (ch);
                IF continue THEN
                    IF ch = CtrlZ THEN
                        continue := FALSE;
                    ELSE
                        IF OptionFlag THEN
                            CASE ch OF
                              | 'A': FWriteString (dstcid, AdministratorAddress);
                              | 'D': FWriteString (dstcid, OurDomain);
                              | 'H': FWriteLJCard (dstcid, L^.Pending.ConfTime);
                              | 'L': FWriteString (dstcid, L^.OurName);
                              | 'S': FWriteString (dstcid, From);
                              | 'X': FWriteString (dstcid, ExtraParam);
                              | '%': FWriteChar (dstcid, '%');
                              | ELSE
                                     FWriteChar (dstcid, '%');
                                     FWriteChar (dstcid, ch);
                            END (*CASE*);
                            OptionFlag := FALSE;
                        ELSIF ch = '%' THEN
                            OptionFlag := TRUE;
                        ELSE
                            FWriteChar (dstcid, ch);
                        END (*IF*);
                    END (*IF*);
                END (*IF*);
            UNTIL NOT continue;
            CloseFile (srccid);
        END (*IF*);

    END AppendFromFile;

(************************************************************************)

PROCEDURE SendFileToGroup (From: EmailAddress;  recipients: AddressList;
                        L: MailingList;  userlang: LangHandle;
                        file: FilenameString;
                        UltimatelyFrom,  ExtraParam: ARRAY OF CHAR);

    (* Sends the contents of 'file', which may include macros, to       *)
    (* destination 'To', using the configuration data of list L, and    *)
    (* using ExtraParam as the value of the %X macro.  The userlang     *)
    (* parameter is needed only to expand a %L in the file name.        *)

    VAR cid: ChanId;  dummy: CARDINAL;
        tempfile: FilenameString;
        To: EmailAddress;
        TimeBuffer: ARRAY [0..31] OF CHAR;

    BEGIN
        cid := OpenNewOutputFile (".\", ".###", tempfile);

        (* Write the reply. *)

        CurrentDateAndTime (TimeBuffer);
        FWriteString (cid, "Date: ");
        FWriteString (cid, TimeBuffer);
        FWriteLn (cid);
        FWriteString (cid, "From: ");
        FWriteString (cid, From);
        FWriteLn (cid);
        IF FirstOnList (recipients, 0, To) THEN
            FWriteString (cid, "To: ");
            FWriteString (cid, To);
            WHILE NextOnList (recipients, 0, To) DO
                FWriteString (cid, ", ");
                FWriteString (cid, To);
            END (*WHILE*);
            FWriteLn (cid);
        END (*IF*);
        AppendFromFile (cid, L, userlang, file, UltimatelyFrom, ExtraParam);
        CloseFile (cid);

        (* Reply done, send it. *)

        EVAL (DeliverItem (From, tempfile, dummy, 0,
                                  recipients, LogLevel, L^.LogID));
        DeleteFile (tempfile);

    END SendFileToGroup;

(************************************************************************)

PROCEDURE SendFromFile (From, To: EmailAddress;  L: MailingList;
                        userlang: LangHandle;
                        file: FilenameString;
                        PercentS, ExtraParam: ARRAY OF CHAR);

    (* Sends the contents of 'file', which may include macros, to       *)
    (* destination 'To', using the configuration data of list L, and    *)
    (* using ExtraParam as the value of the %X macro.  The userlang     *)
    (* parameter is needed only to expand a %L in the file name, and    *)
    (* the PercentS parameter is used to expand %S.  (Often, but        *)
    (* not always, PercentS is the same as From.)                       *)

    VAR recipients: AddressList;

    BEGIN
        recipients := CreateAddressList();
        AddRecipient (recipients, To, OptionSet{mayread});
        SendFileToGroup (From, recipients, L, userlang, file,
                                           PercentS, ExtraParam);
        DiscardAddressList (recipients);
    END SendFromFile;

(************************************************************************)

PROCEDURE SendCannedMessage (To: EmailAddress;  L: MailingList;
                             file: FilenameString;  language: LangHandle;
                             PercentS: EmailAddress);

    (* Sends the contents of 'file', which may include macros, to       *)
    (* destination 'To' from list L.  The PercentS parameter is for     *)
    (* expansion of the %S macro.                                       *)

    BEGIN
        SendFromFile (L^.OurEmailAddress, To, L, language, file,
                                                     PercentS, '');
    END SendCannedMessage;

(************************************************************************)

PROCEDURE SendCanned2 (To: AddressList;  L: MailingList;
                        file: FilenameString;  subscriber: ARRAY OF CHAR);

    (* Similar to SendCannedMessage, but the 'From:' address is the     *)
    (* Administrator address, and there could be multiple recipients.   *)

    BEGIN
        SendFileToGroup (AdministratorAddress, To, L, L^.lang,
                                            file, subscriber, subscriber);
    END SendCanned2;

(************************************************************************)
(*           ADDING AN ENTRY TO A 'TO BE CONFIRMED' LIST                *)
(************************************************************************)

PROCEDURE ClearObsoletePendingConfirmations
                           (VAR (*INOUT*) CL: ConfirmationList): CARDINAL;

    (* Removes expired entries on a confirmation list.  We assume that  *)
    (* the caller has exclusive access to this list.  Also returns the  *)
    (* current time, measured in hours from an arbitrary origin.        *)

    VAR current: ConfirmationListPointer;
        now: CARDINAL;

    BEGIN
        (* Current time, to nearest hour. *)

        now := (time() + 1800) DIV 3600;

        current := CL.head;
        WHILE (current <> NIL) AND (current^.time < now) DO
            CL.head := current^.next;
            DISPOSE (current);
            current := CL.head;
        END (*WHILE*);

        RETURN now;

    END ClearObsoletePendingConfirmations;

(********************************************************************************)

PROCEDURE AddToConfirmationList (VAR (*INOUT*) L: ConfirmationList;
                                    entry: ConfirmationListPointer);

    (* Inserts a new entry into a confirmation list.                    *)
    (* We assume that the caller has exclusive access to this list.     *)

    VAR previous, current, next: ConfirmationListPointer;

    BEGIN
        previous := NIL;
        current := L.head;
        LOOP
            IF current = NIL THEN EXIT(*LOOP*) END(*IF*);
            next := current^.next;
            IF current^.time > entry^.time THEN
                EXIT (*LOOP*);
            ELSE
                previous := current;
            END (*IF*);
            current := next;

        END (*LOOP*);

        (* We've found the right insertion point.  Now add the new list   *)
        (* entry between previous and current.                            *)

        IF previous = NIL THEN
            entry^.next := L.head;
            L.head := entry;
        ELSE
            entry^.next := previous^.next;
            previous^.next := entry;
        END (*IF*);

    END AddToConfirmationList;

(********************************************************************************)

PROCEDURE AddConfirmationEntry (ML: MailingList;
                                 code: ConfirmationCode;  IsSubscription: BOOLEAN;
                                 OptionFlags: OptionSet;  email: EmailAddress;
                                 userlang: LangHandle);

    (* Adds a new entry to the confirmation list for this mailing list.  *)

    VAR entry: ConfirmationListPointer;
        langname: LangName;

    BEGIN
        LanguageCode (userlang, langname);
        Obtain (ML^.Pending.access);
        NEW (entry);
        entry^.code := code;
        entry^.time := ClearObsoletePendingConfirmations(ML^.Pending)
                                                + ML^.Pending.ConfTime;
        entry^.IsSubscription := IsSubscription;
        entry^.flags := OptionFlags;
        entry^.address := email;
        entry^.lang := langname;
        AddToConfirmationList (ML^.Pending, entry);
        Release (ML^.Pending.access);
    END AddConfirmationEntry;

(************************************************************************)

PROCEDURE CreateConfirmationCode (subscriber: EmailAddress;
                                  VAR (*OUT*) code: ConfirmationCode);

    (* Creates a pseudo-random code to be used for confirmations.       *)

    VAR ctx: MD5_CTX;
        timestring: ARRAY [0..20] OF CHAR;
        result: MD5_DigestType;

    BEGIN
        ctx := MD5Init();
        CurrentTimeToString (timestring);
        MD5Update (ctx, timestring, LENGTH(timestring));
        MD5Update (ctx, subscriber, LENGTH(subscriber));
        MD5Final (ctx, result);
        MD5DigestToString (result, code);
    END CreateConfirmationCode;

(************************************************************************)

PROCEDURE SendConfirmationRequest (L: MailingList;
                                requester, subscriber: EmailAddress;
                                userlang: LangHandle;
                                IsSubscription: BOOLEAN;
                                VAR (*OUT*) code: ConfirmationCode): BOOLEAN;

    (* Sends an e-mail to the requester, and possibly also to the list  *)
    (* owners, asking for a confirmation of the (un)subscription of the *)
    (* specified subscriber.  This procedure also generates the random  *)
    (* code to be used for the confirmation.                            *)

    VAR filefound, CopyToOwners: BOOLEAN;
        command: ARRAY [0..1023] OF CHAR;

    BEGIN
        filefound := L^.ConfirmationRequestMessage[0] <> Nul;
        IF filefound THEN
            command := "confirm ";
            CopyToOwners := FALSE;
            IF NOT IsSubscription THEN
                Strings.Append ("un", command);
                CopyToOwners := L^.OwnersMayConfUnsub
                                  AND NOT IsOnList (requester, L^.Owners);
            END (*IF*);
            Strings.Append ("subscribe ", command);
            Strings.Append (L^.OurName, command);
            Strings.Append (" ", command);
            CreateConfirmationCode (subscriber, code);
            Strings.Append (code, command);
            SendFromFile (AdministratorAddress, requester, L, userlang,
                                L^.ConfirmationRequestMessage,
                                subscriber, command);
            IF CopyToOwners THEN
                SendFileToGroup (AdministratorAddress, L^.Owners, L, userlang,
                                  L^.ConfirmationRequestMessage,
                                  subscriber, command);
            END (*IF*);
        END (*IF*);
        RETURN filefound;
    END SendConfirmationRequest;

(************************************************************************)
(*                           LIST OPERATIONS                            *)
(************************************************************************)

PROCEDURE IdentifyList (listname: ARRAY OF CHAR;
                        VAR (*OUT*) ML: MailingList): BOOLEAN;

    (* Translates from list name (not case sensitive) to list.          *)
    (* Returns TRUE iff successful.                                     *)

    VAR NP: AllListNodePtr;

    BEGIN
        Obtain (AllLists.access);
        ML := NIL;  NP := AllLists.head;
        LOOP
            IF NP = NIL THEN
                Release (AllLists.access);
                RETURN FALSE;
            ELSIF NameMatch (listname, NP^.this^.OurName) THEN
                ML := NP^.this;
                Release (AllLists.access);
                RETURN TRUE;
            ELSE NP := NP^.next
            END (*IF*);
        END (*LOOP*);
    END IdentifyList;

(************************************************************************)

PROCEDURE ResaveList (ListName: ListNameType;  list: AddressList);

    (* Writes a "Members" list back to the INI file. *)

    VAR bufptr: BufferPointer;
        BufferSize: CARDINAL;
        hini: HINI;

    BEGIN
        EncodeList (list, bufptr, BufferSize);
        hini := OpenINIFile (INIFileName, UseTNI);
        INIPutBinary (hini, ListName, 'Members', bufptr^, BufferSize);
        CloseINIFile (hini);
        DEALLOCATE (bufptr, BufferSize);
    END ResaveList;

(************************************************************************)

PROCEDURE AddMember (language: LangHandle;  from: EmailAddress;
                     list: MailingList;
                     newmember: EmailAddress;  flags: OptionSet): BOOLEAN;

    (* Adds a new subscriber to a list. *)

    VAR buffer, message: ARRAY [0..255] OF CHAR;
        Self, MaySubscribe: BOOLEAN;

    BEGIN
        IF list = NIL THEN
            RETURN FALSE;
        ELSE
            Self := NameMatch (from, newmember);
            Obtain (list^.access);
            MaySubscribe := list^.Enabled.Subscribe2
                            OR (Self AND list^.Enabled.Subscribe1)
                            OR IsOnList (from, list^.Owners);
            IF MaySubscribe THEN
                AddRecipient (list^.Members, newmember, flags);
                ResaveList (list^.OurName, list^.Members);
                INC (list^.MemberCount);
                IF digestmember IN flags THEN
                    INC (list^.DigestUsers);
                END (*IF*);
            END (*IF*);
            Release (list^.access);
            IF MaySubscribe THEN
                Strings.Assign (" (", buffer);
                IF digestmember IN flags THEN
                    Strings.Append ("digest, ", buffer);
                    IF NOT (maywrite IN flags) THEN
                        Strings.Append ("readonly, ", buffer);
                    END (*IF*);
                ELSIF NOT (mayread IN flags) THEN
                    Strings.Append ("writeonly, ", buffer);
                ELSIF NOT (maywrite IN flags) THEN
                    Strings.Append ("readonly, ", buffer);
                END (*IF*);
                buffer[Strings.Length(buffer)-2] := Nul;
                StrToBufferAB (AdminLang, "ListChecker.log.subscribed",
                                            newmember, buffer, message);
            ELSE
                StrToBufferA (AdminLang,
                           "ListChecker.log.rejected", newmember, message);
            END (*IF*);
            LogTransaction (list^.LogID, message);
            IF MaySubscribe THEN
                IF list^.WelcomeMessage[0] <> Nul THEN
                    SendCannedMessage (newmember, list,
                                          list^.WelcomeMessage,
                                           language, newmember);
                END (*IF*);
                IF (list^.NotifyOwnerMessage[0] <> Nul)
                                       AND NOT EmptyList(list^.Owners) THEN
                    SendCanned2 (list^.Owners, list,
                                 list^.NotifyOwnerMessage, newmember);
                END (*IF*);
            END (*IF*);
            RETURN MaySubscribe;
        END (*IF*);
    END AddMember;

(************************************************************************)

PROCEDURE RemoveMember (language: LangHandle;  from: EmailAddress;
                        list: MailingList;  member: EmailAddress): BOOLEAN;

    (* Removes a subscriber from a list. *)

    VAR success: BOOLEAN;
        logmessage: ARRAY [0..255] OF CHAR;

    BEGIN
        IF list = NIL THEN
            success := FALSE;
        ELSE
            Obtain (list^.access);
            success := list^.Enabled.Unsubscribe2
                          OR NameMatch (from, member)
                          OR IsOnList (from, list^.Owners);
            IF success THEN
                IF RemoveRecipient (list^.Members, member) THEN
                    DEC (list^.DigestUsers);
                END (*IF*);
                DEC (list^.MemberCount);
                ResaveList (list^.OurName, list^.Members);
            END (*IF*);
            Release (list^.access);
            IF success THEN
                StrToBufferA (AdminLang, "ListChecker.log.unsubscribed",
                                                member, logmessage);
                LogTransaction (list^.LogID, logmessage);
                IF list^.DepartureMessage[0] <> Nul THEN
                    SendCannedMessage (member, list,
                                       list^.DepartureMessage, language, member);
                END (*IF*);
                IF (list^.NotifyOwnerUnsubMessage[0] <> Nul)
                                       AND NOT EmptyList(list^.Owners) THEN
                    SendCanned2 (list^.Owners, list,
                                 list^.NotifyOwnerUnsubMessage, member);
                END (*IF*);
            END (*IF*);
        END (*IF*);
        RETURN success;
    END RemoveMember;

(************************************************************************)

PROCEDURE ConfirmationRequired (list: MailingList): BOOLEAN;

    (* Returns TRUE iff this list is one that requires confirmations. *)

    BEGIN
        RETURN list^.RequireConfirmation;
    END ConfirmationRequired;

(************************************************************************)

PROCEDURE IsMember (member: EmailAddress;  list: MailingList): BOOLEAN;

    (* Returns TRUE iff member is already a member of the list. *)

    BEGIN
        RETURN IsOnList (member, list^.Members);
    END IsMember;

(************************************************************************)

PROCEDURE IsOwner (name: EmailAddress;  list: MailingList): BOOLEAN;

    (* Returns TRUE iff name is one of the list owners. *)

    BEGIN
        RETURN IsOnList (name, list^.Owners);
    END IsOwner;

(************************************************************************)

PROCEDURE MaySend (member: EmailAddress;  list: MailingList): BOOLEAN;

    (* Returns TRUE iff member is someone who is allowed to send        *)
    (* to the list.                                                     *)

    BEGIN
        RETURN (maywrite IN MemberFlags (member, list^.Members)) OR
                                IsOnList (member, list^.Owners);
    END MaySend;

(************************************************************************)
(*              REPORTING ON THE LIST OF ALL LIST MEMBERS               *)
(************************************************************************)

PROCEDURE WhoIsLegal (list: MailingList;  from: EmailAddress): BOOLEAN;

    (* Returns TRUE iff the 'who' command is enabled for this list      *)
    (* or 'from' is an owner of the list.                               *)

    BEGIN
        RETURN list^.Enabled.Who OR IsOnList (from, list^.Owners);
    END WhoIsLegal;

(************************************************************************)

PROCEDURE DoWho (cid: ChanId;  lang: LangHandle;  list: MailingList);

    (* Appends a list of list members to the output file cid. *)

    VAR success: BOOLEAN;  count: CARDINAL;
        name: EmailAddress;

    BEGIN
        LWriteStringA (lang, cid, "ListChecker.who.begin",
                                                list^.OurName);
        FWriteLn (cid);
        FWriteLn (cid);
        count := 0;
        success := FirstOnList (list^.Members, 0, name);
        WHILE success DO
            INC (count);
            FWriteString (cid, name);  FWriteLn (cid);
            success := NextOnList (list^.Members, 0, name);
        END (*WHILE*);
        FWriteLn (cid);
        LWriteStringN (lang, cid, "ListChecker.who.count",
                                                           count);
        FWriteLn (cid);
    END DoWho;

(************************************************************************)
(*                      MISCELLANEOUS REPORTING                         *)
(************************************************************************)

PROCEDURE MainLanguage (name: EmailAddress;
                             VAR (*OUT*) confident: BOOLEAN): LangHandle;

    (* Returns the "majority language" among all lists to which 'name'  *)
    (* is subscribed, breaking ties in an arbitrary way.  If there are  *)
    (* no such lists, we return the default language.  Parameter        *)
    (* 'confident' is returned as TRUE iff there is at least one        *)
    (* subscription, i.e. we didn't have to default.                    *)

    TYPE
        CandPtr = POINTER TO Candidate;
        Candidate = RECORD
                        prev, next: CandPtr;
                        lang: LangHandle;
                        count: CARDINAL;
                    END (*RECORD*);

    VAR p: AllListNodePtr;
        candidates, qprev, q, qnext: CandPtr;
        result: LangHandle;

    BEGIN
        (* Go through the list of all lists, and build the candidates   *)
        (* list by including all lists of which 'name' is a member.     *)

        candidates := NIL;

        Obtain (AllLists.access);
        p := AllLists.head;
        WHILE p <> NIL DO
            IF IsOnList (name, p^.this^.Members) THEN
                qprev := NIL;  q := candidates;
                LOOP
                    IF q = NIL THEN

                        (* Have reached end of list, create a new record. *)

                        NEW (q);
                        q^.prev := qprev;  q^.next := NIL;
                        q^.count :=  1;
                        q^.lang := p^.this^.lang;
                        IF qprev = NIL THEN
                            candidates := q;
                        ELSE
                            qprev^.next := q;
                        END (*IF*);
                        EXIT (*LOOP*);

                    ELSIF q^.lang = p^.this^.lang THEN

                        INC (q^.count);
                        WHILE (qprev <> NIL) AND (q^.count > qprev^.count) DO

                            (* Promote the current entry in the list. *)

                            qnext := q^.next;
                            q^.prev := qprev^.prev;
                            IF qprev^.prev = NIL THEN
                                candidates := q;
                            ELSE
                                qprev^.prev^.next := q;
                            END (*IF*);
                            qprev^.next := qnext;
                            IF qnext <> NIL THEN
                                qnext^.prev := qprev;
                            END (*IF*);
                            q^.next := qprev;
                            qprev^.prev := q;
                            qprev := q^.prev;

                        END (*WHILE*);
                        EXIT (*LOOP*);

                    ELSE
                        qprev := q;
                        q := qprev^.next;
                    END (*IF*);

                END (*LOOP*);

            END (*IF*);

            p := p^.next;

        END (*WHILE*);
        Release (AllLists.access);

        (* We have built the candidate list in such a way as to keep    *)
        (* it sorted, so the answer comes from the head of the list.    *)
        (* Once we have the answer, dispose of the candidate list.      *)

        confident := candidates <> NIL;
        IF confident THEN
            result := candidates^.lang;
        ELSE
            result := DefaultLang;
        END (*IF*);

        WHILE candidates <> NIL DO
            q := candidates;  candidates := q^.next;
            DISPOSE (q);
        END (*WHILE*);

        RETURN result;

    END MainLanguage;

(************************************************************************)

PROCEDURE ListLists (cid: ChanId;  lang: LangHandle;  name: EmailAddress);

    (* Appends to the output file cid the names of all lists to which   *)
    (* member 'name' is subscribed.  If 'name' is the empty string, we  *)
    (* list all the lists on the system.                                *)

    CONST NoFlags = OptionSet{};

    VAR p: AllListNodePtr;  count: CARDINAL;  ListAll: BOOLEAN;
        flags: OptionSet;

    BEGIN
        ListAll := name[0] = Nul;

        FWriteLn (cid);
        IF ListAll THEN
            LWriteString (lang, cid, "ListChecker.list.all");
        ELSE
            LWriteString (lang, cid, "ListChecker.list.subs");
        END (*IF*);
        FWriteLn (cid);
        FWriteLn (cid);
        count := 0;

        Obtain (AllLists.access);
        p := AllLists.head;
        WHILE p <> NIL DO
            IF ListAll THEN
                flags := NoFlags;
            ELSE
                flags := MemberFlags (name, p^.this^.Members);
            END (*IF*);
            IF ListAll OR (flags <> NoFlags) THEN
                FWriteString (cid, "    ");
                FWriteString (cid, p^.this^.OurName);
                IF NOT p^.this^.Enabled.Subscribe1 THEN
                    LWriteString (lang, cid, "ListChecker.listattr.restricted");
                END (*IF*);
                IF p^.this^.Moderated THEN
                    LWriteString (lang, cid, "ListChecker.listattr.moderated");
                END (*IF*);
                IF (flags <> NoFlags) AND (flags <> OptionSet{mayread,maywrite}) THEN
                    FWriteString (cid, " (");
                    IF digestmember IN flags THEN
                        FWriteString (cid, "digest");
                        IF NOT (maywrite IN flags) THEN
                            FWriteString (cid, " readonly");
                        END (*IF*);
                    ELSIF NOT (mayread IN flags) THEN
                        FWriteString (cid, "writeonly");
                    ELSIF NOT (maywrite IN flags) THEN
                        FWriteString (cid, "readonly");
                    END (*IF*);
                    FWriteString (cid, ")");
                END (*IF*);
                FWriteLn (cid);
                INC (count);
            END (*IF*);
            p := p^.next;
        END (*WHILE*);
        Release (AllLists.access);

        FWriteLn (cid);
        LWriteStringN (lang, cid, "ListChecker.list.count", count);
        FWriteLn (cid);
        FWriteLn (cid);

    END ListLists;

(************************************************************************)

PROCEDURE ReturnIndex (cid: ChanId;  lang: LangHandle;  list: MailingList);

    (* Appends a directory of the file repository for this list *)
    (* to the output file cid.                                  *)

    BEGIN
        FileListing (lang, cid, list^.OurName);
    END ReturnIndex;

(************************************************************************)
(*                           CONFIRMATIONS                              *)
(************************************************************************)

PROCEDURE CodeMatch (VAR (*IN*) code1, code2: ConfirmationCode): BOOLEAN;

    (* Checks two confirmation codes for equality. *)

    BEGIN
        RETURN Strings.Equal (code1, code2);
    END CodeMatch;

(************************************************************************)

PROCEDURE DoConfirmation (list: MailingList;  IsSubscription: BOOLEAN;
                                VAR (*OUT*) newlang: LangName;
                                VAR (*IN*) code: ConfirmationCode): BOOLEAN;

    (* Confirms a previously issued command.  The BOOLEAN argument says *)
    (* whether this is a subscription or an unsubscription.             *)

    VAR success, found: BOOLEAN;
        userlang: LangHandle;
        previous, current, next: ConfirmationListPointer;

    BEGIN
        success := FALSE;  found := FALSE;
        WITH list^.Pending DO
            Obtain (access);
            EVAL (ClearObsoletePendingConfirmations(list^.Pending));
            previous := NIL;  current := head;
            WHILE (current <> NIL) AND NOT found  DO
                found := (current^.IsSubscription = IsSubscription)
                                    AND CodeMatch (current^.code, code);
                next := current^.next;
                IF found THEN
                    (* Remove current^ from the list. *)
                    IF previous = NIL THEN
                        head := next;
                    ELSE
                        previous^.next := next;
                    END (*IF*);
                ELSE
                    previous := current;
                    current := next;
                END (*IF*);
            END (*WHILE*);
            Release (access);
        END (*WITH*);

        IF found THEN
            WITH current^ DO
                newlang := lang;
                userlang := UseLanguage ("MM", newlang);
                IF IsSubscription THEN
                    IF IsMember (address, list) THEN
                        success := FALSE;
                    ELSE
                        success := AddMember (userlang, address,
                                               list, address, flags);
                    END (*IF*);
                ELSE
                    success := RemoveMember (userlang, address,
                                                   list, address);
                END (*IF*);
            END (*WITH*);

            (* We have to drop the new language for now, or the count   *)
            (* of the number of users of this language will be out.     *)

            DropLanguage (userlang);
            DISPOSE (current);
        ELSE
            newlang[0] := CHR(0);
        END (*IF*);

        RETURN success;

    END DoConfirmation;

(************************************************************************)
(*                      CHECKING MAIL FOR ONE LIST                      *)
(************************************************************************)

PROCEDURE CopyMessage (VAR (*IN*) srcfile, directory: FilenameString);

    (* Copies the file "srcfile" to "directory".  *)

    VAR BaseName, dstfile: FilenameString;

    BEGIN
        BaseName := directory;
        IF BaseName[0] <> Nul THEN
            Strings.Append ('\', BaseName);
        END (*IF*);
        MakeNewFilename (BaseName, ".MSG", dstfile);
        EVAL (CopyFile (srcfile, dstfile));
    END CopyMessage;

(************************************************************************)

PROCEDURE UpdateSubjectLine (VAR (*INOUT*) buffer: LineBuffer;
                                           toinsert: AbbreviationType);

    (* Inserts the "toinsert" field into a Subject line. *)

    VAR srcpos: CARDINAL;
        reFlag: BOOLEAN;

    (********************************************************************)

    PROCEDURE Matchup (template: ARRAY OF CHAR): BOOLEAN;

        (* Checks whether template matches buffer[srcpos], modulo case, *)
        (* increments srcpos past the template if so.                   *)

        VAR j0, k: CARDINAL;

        BEGIN
            j0 := srcpos;  k := 0;
            LOOP
                IF (k > HIGH(template)) OR (template[k] = Nul) THEN
                    RETURN TRUE;
                ELSIF CAP(buffer[srcpos]) <> CAP(template[k]) THEN
                    srcpos := j0;
                    RETURN FALSE;
                ELSE
                    INC (srcpos);  INC(k);
                END (*IF*);
            END (*LOOP*);
        END Matchup;

    (********************************************************************)

    PROCEDURE SkipReAndSpaces;

        (* Skips past any initial "Re:", also space chars. *)

        BEGIN
            LOOP
                IF Matchup ("RE:") THEN
                    reFlag := TRUE;
                ELSIF buffer[srcpos] = Space THEN
                    INC (srcpos);
                ELSE
                    EXIT (*LOOP*);
                END (*IF*);
            END (*LOOP*);
        END SkipReAndSpaces;

    (********************************************************************)

    VAR result: LineBuffer;

    BEGIN
        result := "Subject: ";
        srcpos := 8;  reFlag := FALSE;
        SkipReAndSpaces;

        (* See whether there's an existing [...] prefix. *)

        IF buffer[srcpos] = '[' THEN
            INC (srcpos);
            IF Matchup(toinsert) THEN
                IF buffer[srcpos] = ']' THEN
                    INC (srcpos);
                END (*IF*);
            ELSE
                DEC (srcpos);
            END (*IF*);
        END (*IF*);

        SkipReAndSpaces;

        (* Reinsert the "Re:" if it was present. *);

        IF reFlag THEN
            Strings.Append ("Re: ", result);
        END (*IF*);

        (* Insert our own prefix. *)

        Strings.Append ('[', result);
        Strings.Append (toinsert, result);
        Strings.Append ('] ', result);

        (* Copy over the rest of the subject line. *)

        Strings.Delete (buffer, 0, srcpos);
        Strings.Append (buffer, result);
        Strings.Assign (result, buffer);

    END UpdateSubjectLine;

(************************************************************************)

PROCEDURE CheckForBoundaryCode (srccid, dstcid: ChanId;
                                VAR (*INOUT*) NextLine: LineBuffer;
                                VAR (*OUT*) boundary: LineBuffer);

    (* Checks this header line for a "boundary=..." field.  If found,   *)
    (* we discard this line (including its continuation lines).  If not *)
    (* found, we copy the input to the output.  On return, NextLine is  *)
    (* the first input line that doesn't belong to us.                  *)

    TYPE lineptr = POINTER TO OneLine;
         OneLine = RECORD
                       next: lineptr;
                       this: LineBuffer;
                   END (*RECORD*);

    VAR head, tail, p: lineptr;
        StillSearching, found: BOOLEAN;  pos: CARDINAL;
        quotechar: ARRAY [0..1] OF CHAR;

    BEGIN
        StillSearching := TRUE;

        (* Because we don't know in advance whether we will want to     *)
        (* write this line and its continuation lines to the output     *)
        (* file, save the lines in a linked list.                       *)

        NEW (head);
        head^.next := NIL;
        head^.this := NextLine;
        tail := head;
        Strings.FindNext (':', NextLine, 0, found, pos);
        Strings.Delete (NextLine, 0, pos+1);

        (* Now scan this line, plus any continuation lines, for a       *)
        (* boundary code.  If we find one we can stop checking, but we  *)
        (* still have to keep reading the input lines until we get to   *)
        (* the next non-continuation line.                              *)

        LOOP   (* once per line *)

            IF StillSearching THEN

                LOOP      (* once per item in the line *)

                    (* Strip leading spaces and tabs. *)

                    pos := 0;
                    WHILE (NextLine[pos] = Space) OR (NextLine[pos] = Tab) DO
                        INC (pos);
                    END (*WHILE*);
                    IF pos > 0 THEN
                        Strings.Delete (NextLine, 0, pos);
                    END (*IF*);

                    (* Nothing left on this line? *)

                    IF NextLine[0] = Nul THEN
                        EXIT (*LOOP*);
                    END (*IF*);

                    IF HeadMatch (NextLine, "boundary=") THEN

                        (* Success! *)

                        Strings.Delete (NextLine, 0, 9);
                        IF NextLine[0] = '"' THEN
                            quotechar[0] := NextLine[0];
                            quotechar[1] := Nul;
                            Strings.Delete (NextLine, 0, 1);
                            Strings.FindNext (quotechar, NextLine, 0, found, pos);
                            IF found THEN
                                NextLine[pos] := Nul;
                            END (*IF*);
                        END (*IF*);
                        boundary := NextLine;
                        Strings.Insert ('--', 0, boundary);
                        StillSearching := FALSE;
                        EXIT (*LOOP*);

                    ELSE

                        (* No luck so far, move to next field. *)

                        Strings.FindNext (';', NextLine, 0, found, pos);
                        IF found THEN
                            Strings.Delete (NextLine, 0, pos+1);
                        ELSE
                            EXIT (*LOOP*);
                        END (*IF*);

                    END (*IF*);

                END (*LOOP*);
            END (*IF*);

            (* Get the next line from the input, and continue   *)
            (* processing iff it's a continuation line.         *)

            ReadLine (srccid, NextLine);
            IF (NextLine[0] <> Space) AND (NextLine[0] <> Tab) THEN EXIT(*LOOP*) END(*IF*);
            IF StillSearching THEN
                NEW (p);
                p^.next := NIL;
                p^.this := NextLine;
                tail^.next := p;
                tail := p;
            END (*IF*);
        END (*LOOP*);

        (* If there wasn't any boundary code, write our saved *)
        (* lines to the output file.                          *)

        IF StillSearching THEN
            boundary[0] := Nul;
            p := head;
            REPEAT
                FWriteString (dstcid, p^.this);
                FWriteLn (dstcid);
                p := p^.next;
            UNTIL p = NIL;
        END (*IF*);

        (* Dispose of the lines we saved. *)

        WHILE head <> NIL DO
            p := head;  head := head^.next;
            DISPOSE (p);
        END (*WHILE*);

    END CheckForBoundaryCode;

(************************************************************************)

PROCEDURE AddListHeaders (cid: ChanId;  L: MailingList);

    (* Adds mail header lines as specified by RFC2369. *)

    VAR addr: EmailAddress;

    BEGIN
        IF L^.RFC2919ID[0] <> Nul THEN
            FWriteString (cid, "List-Id: <");
            FWriteString (cid, L^.RFC2919ID);
            FWriteString (cid, ">");
            FWriteLn (cid);
        END (*IF*);
        IF L^.Add2369Headers THEN
            FWriteString (cid, "List-Help: <mailto:");
            FWriteString (cid, AdministratorAddress);
            FWriteString (cid, "?body=HELP>");
            FWriteLn (cid);
            FWriteString (cid, "List-Unsubscribe: <mailto:");
            FWriteString (cid, AdministratorAddress);
            FWriteString (cid, "?body=unsubscribe%20");
            FWriteString (cid, L^.OurName);
            FWriteString (cid, ">");
            FWriteLn (cid);
            FWriteString (cid, "List-Subscribe: <mailto:");
            FWriteString (cid, AdministratorAddress);
            FWriteString (cid, "?body=subscribe%20");
            FWriteString (cid, L^.OurName);
            FWriteString (cid, ">");
            FWriteLn (cid);
            FWriteString (cid, "List-Post: <mailto:");
            FWriteString (cid, L^.OurEmailAddress);
            FWriteString (cid, ">");
            IF L^.Moderated THEN
                LWriteString (L^.lang, cid, "ListChecker.ismoderated");
            END (*IF*);
            FWriteLn (cid);
            IF FirstOnList (L^.Owners, 0, addr) THEN
                FWriteString (cid, "List-Owner: <mailto:");
                FWriteString (cid, addr);
                FWriteString (cid, ">");
                WHILE NextOnList (L^.Owners, 0, addr) DO
                    FWriteString (cid, ", <mailto:");
                    FWriteString (cid, addr);
                    FWriteString (cid, ">");
                END (*WHILE*);
                FWriteLn (cid);
            END (*IF*);
            IF L^.ArchiveMessages THEN
                FWriteString (cid, "List-Archive: <mailto:");
                FWriteString (cid, AdministratorAddress);
                FWriteString (cid, "?body=index%20");
                FWriteString (cid, L^.OurName);
                FWriteString (cid, ">");
                FWriteLn (cid);
            END (*IF*);
        END (*IF*);
    END AddListHeaders;

(************************************************************************)

PROCEDURE WriteEmailAddress (cid: ChanId;
                                VAR (*IN*) DisplayName: ARRAY OF CHAR;
                                VAR (*IN*) address: EmailAddress);

    (* Writes an e-mail address, with optional display name. *)

    BEGIN
        IF DisplayName[0] <> Nul THEN
            FWriteString (cid, DisplayName);
            FWriteChar (cid, ' ');
        END (*IF*);
        FWriteChar (cid, '<');
        FWriteString (cid, address);
        FWriteChar (cid, '>');
    END WriteEmailAddress;

(************************************************************************)

PROCEDURE ProcessItem (L: MailingList;  srccid, dstcid: ChanId;
                       VAR (*IN*) terminator: LineBuffer;
                       VAR (*OUT*) Sender: EmailAddress;
                       VAR (*OUT*) IllegalSender: BOOLEAN): BOOLEAN;

    (* Copies srccid to dstcid, modifying the header lines as needed    *)
    (* and removing attachments if that is desired.  This procedure is  *)
    (* called after the filter has been run, but before this item has   *)
    (* been sent to mailing list members.  We return IllegalSender=TRUE *)
    (* to tell the caller to abort the operation.  The function result  *)
    (* is TRUE iff we have stripped attachments.                        *)

    (* The "From" header line needs special treatment because of list   *)
    (* moderation:                                                      *)
    (*  - unmoderated list: retain the "From" header unchanged (but     *)
    (*      special treatment needed L^.SuppressFrom is TRUE or if      *)
    (*      L^.DMARCcompatible is TRUE)                                 *)
    (*  - message to be sent to moderator: copy the "From" address      *)
    (*      into an "X-Original-Sender" header, and also retain the     *)
    (*      original "From" line. Exception: suppress both if           *)
    (*      L^.SuppressFrom is TRUE.                                    *)
    (*  - message from moderator: drop the "From" address, since it's   *)
    (*      the moderator's address, but generate new "From" data       *)
    (*      using the X-Original-Sender information, and then treat     *)
    (*      that "From" data in the same way as for an unmoderated      *)
    (*      list.                                                       *)

    VAR buffer, boundary: LineBuffer;
        OriginalSender, OriginalReplyTo, AddrTemp: EmailAddress;
        DispTemp, DisplayName: LineBuffer;
        FromModerator, ToModerator, DropLine, AlreadyHaveLine, dummy: BOOLEAN;

    BEGIN
        IllegalSender := FALSE;
        AlreadyHaveLine := FALSE;
        DropLine := FALSE;
        boundary := "";
        OriginalSender := "";
        OriginalReplyTo := "";
        Sender := "";
        DisplayName := "";

        (* Copy the header, modifying the "Subject" and "Reply-To"      *)
        (* lines, and possibly the "From" line, and checking the        *)
        (* "Content-Type" line if we want to strip attachments.         *)

        LOOP
            IF AlreadyHaveLine THEN
                AlreadyHaveLine := FALSE;
            ELSE
                ReadLine (srccid, buffer);
            END (*IF*);
            IF (buffer[0] = Nul) OR (buffer[0] = CtrlZ) THEN EXIT(*LOOP*) END(*IF*);

            (* Continuation lines are either dropped or copied,         *)
            (* depending on the value of DropLine the last time around  *)
            (* this loop, but otherwise need no further processing.     *)

            IF (buffer[0] <> Space) AND (buffer[0] <> Tab) THEN

                DropLine := FALSE;

                IF HeadMatch(buffer, "Read-Receipt-To")
                          OR HeadMatch(buffer, "Return-Receipt-To")
                          OR HeadMatch(buffer, "Disposition-notification-to")
                          OR HeadMatch(buffer, "X-For-Moderation") THEN

                    (* We want to strip out "Read-Receipt-To" and       *)
                    (* "Return-Receipt-To" header lines unconditionally.*)
                    (* Any "X-For-Moderation" line should also be       *)
                    (* stripped out, although we might add such a line  *)
                    (* below, depending on whether this message is      *)
                    (* going to a moderator or coming from one.         *)

                    DropLine := TRUE;

                ELSIF HeadMatch (buffer, "Return-Path:") THEN

                    (* We assume that the Return-Path, if present, gives the    *)
                    (* most reliable indication of the sender's address.        *)

                    ExtractEmailAddress (buffer, 12, DispTemp, Sender);

                    (* Don't copy the Return-Path line into the output file,    *)
                    (* because it's now obsolete.                               *)

                    DropLine := TRUE;

                ELSIF HeadMatch (buffer, "Reply-To:") THEN

                    (* Drop the Reply-To header, but retain the         *)
                    (* information in case we need to reinsert it later. *)

                    ExtractEmailAddress (buffer, 9, DispTemp, OriginalReplyTo);
                    IF NOT L^.Moderated THEN
                        DisplayName := DispTemp;
                    END (*IF*);
                    DropLine := TRUE;

                ELSIF HeadMatch (buffer, "X-Original-Sender:") THEN

                    (* Remember the content of this line, then drop the line.   *)

                    ExtractEmailAddress (buffer, 18, DisplayName, OriginalSender);
                    DropLine := TRUE;

                ELSIF HeadMatch (buffer, "From:") THEN

                    (* The address in the "From:" line will be used only if we  *)
                    (* failed to get an address from the "Return-Path:".        *)

                    ExtractEmailAddress (buffer, 5, DispTemp, AddrTemp);
                    IF Sender[0] = Nul THEN
                        Sender := AddrTemp;
                    END (*IF*);
                    IF (NOT L^.Moderated) AND (DisplayName[0] = Nul) THEN
                        DisplayName := DispTemp;
                    END (*IF*);

                    (* Drop the "From" line, although we might decide below to  *)
                    (* restore it.                                              *)

                    DropLine := TRUE;

                ELSIF L^.KillAttachments AND HeadMatch(buffer, "Content-Type:")
                                        AND (boundary[0] = Nul) THEN

                    (* Scan the line for a BOUNDARY= field.  Note that the      *)
                    (* procedure CheckForBoundaryCode will copy this header     *)
                    (* line (preserving the charset specification) if there is  *)
                    (* no boundary code, but will kill it if this turns out to  *)
                    (* be a multipart message.                                  *)

                    CheckForBoundaryCode (srccid, dstcid, buffer, boundary);
                    AlreadyHaveLine := TRUE;
                    DropLine := TRUE;

                    (* We are not actually dropping the line, which is by now   *)
                    (* the header line following the Content-Type header.  The  *)
                    (* DropLine := TRUE means that we don't copy it this time,  *)
                    (* but the line remains to be processed the next time       *)
                    (* around the loop.                                         *)

                ELSIF HeadMatch (buffer, "Subject:") THEN

                    (* Modify the Subject line by inserting an abbreviation.    *)
                    (* (Removing any that's already there.)                     *)

                    IF L^.abbreviation[0] <> Nul THEN
                        UpdateSubjectLine (buffer, L^.abbreviation);
                    END (*IF*);

                END (*IF*);

            END (*IF*);

            (* Write the current line, unless the above logic has told us       *)
            (* to drop it.                                                      *)

            IF NOT DropLine THEN
                FWriteString (dstcid, buffer);
                FWriteLn (dstcid);
            END (*IF*);

        END (*LOOP*);

        (* Check the sender, but only if this is a top-level call. *)

        IF terminator[0] = CtrlZ THEN
            IF Sender[0] = Nul THEN
                Strings.Assign ("?", Sender);
            END (*IF*);
            IF OriginalSender[0] = Nul THEN
                OriginalSender := Sender;
            END (*IF*);
    
            (* Abort the operation if we discover that we don't approve     *)
            (* of the sender.                                               *)
    
            IF (L^.NonsubOption < 2) AND NOT MaySend (Sender, L) THEN
                IllegalSender := TRUE;
                RETURN FALSE;
            END (*IF*);
        END (*IF*);

        (* End of header.  Add extra header lines as needed, but only   *)
        (* if this is not a nested recursive call.                      *)

        IF terminator[0] = CtrlZ THEN

            FromModerator := L^.Moderated AND IsOnList (Sender, L^.Owners);
            ToModerator := L^.Moderated AND NOT FromModerator;

            IF L^.SuppressFrom THEN
                OriginalSender[0] := Nul;
                DisplayName[0] := Nul;
                OriginalReplyTo[0] := Nul;
            END (*IF*);

            (* Add a new "From:" header. *)

            FWriteString (dstcid, "From: ");
            IF L^.SuppressFrom THEN
                WriteEmailAddress (dstcid, L^.OurName, L^.OurEmailAddress);
            ELSIF ToModerator THEN
                WriteEmailAddress (dstcid, DisplayName, OriginalSender);
            ELSIF L^.DMARCcompatible THEN
                IF DisplayName[0] <> Nul THEN
                    FWriteString (dstcid, DisplayName);
                    FWriteString (dstcid, " via ");
                END (*IF*);
                WriteEmailAddress (dstcid, L^.OurName, L^.OurEmailAddress);
            ELSE
                WriteEmailAddress (dstcid, DisplayName, OriginalSender);
            END (*IF*);
            FWriteLn (dstcid);

            (* In some cases, add a "Sender:" header. *)
            (* Remark: this code violates the spirit of the mail RFCs, where Sender: *)
            (* has a totally different meaning; but there is no good way to be       *)
            (* both RFC-compatible and DMARC-compatible.                             *)

            IF L^.DMARCcompatible AND (OriginalSender[0] <> Nul) THEN
                FWriteString (dstcid, "Sender: ");
                WriteEmailAddress (dstcid, DisplayName, OriginalSender);
                FWriteLn (dstcid);
            END (*IF*);

            (* Put in the new "Reply-To:" header. *)

            FWriteString (dstcid, "Reply-To: ");
            IF ToModerator THEN
                WriteEmailAddress (dstcid, DisplayName, OriginalReplyTo);
            ELSE
                WriteEmailAddress (dstcid, L^.OurName, L^.OurEmailAddress);
            END (*IF*);
            FWriteLn (dstcid);

            (* A couple of extra headers if passing this to moderator. *)

            IF ToModerator THEN
                FWriteString (dstcid, "X-For-Moderation: ");
                FWriteString (dstcid, L^.OurName);
                FWriteLn (dstcid);
                FWriteString (dstcid, "X-Original-Sender: ");
                FWriteString (dstcid, OriginalSender);
                FWriteLn (dstcid);
            ELSE
                AddListHeaders (dstcid, L);
            END (*IF*);

        END (*IF*);

        (* End of header processing. *)

        (* A non-empty boundary code at this stage means that   *)
        (* we have to remove attachments.                       *)

        IF boundary[0] = Nul THEN

            (* A blank line separates header from body. *)
    
            FWriteLn (dstcid);

            (* Add in the leader material if any. *)

            IF L^.Leader[0] <> Nul THEN
                AppendFromFile (dstcid, L, L^.lang, L^.Leader, Sender, '');
            END (*IF*);

            LOOP
                ReadLine (srccid, buffer);
                IF (buffer[0] = CtrlZ) OR Strings.Equal (buffer, terminator) THEN
                    EXIT (*LOOP*);
                END (*IF*);
                FWriteString (dstcid, buffer);
                FWriteLn (dstcid);
            END (*LOOP*);
            RETURN FALSE;

        ELSE

            (* We want to remove attachments.  To begin with, skip to   *)
            (* the first boundary.                                      *)

            REPEAT
                ReadLine (srccid, buffer);
            UNTIL (buffer[0] = CtrlZ) OR Strings.Equal(buffer, terminator)
                                       OR Strings.Equal(buffer, boundary);

            (* Copy everything up to, but not including, the next       *)
            (* boundary.  A recursive method is necessary because       *)
            (* MIME allows nested attachments, and also because there   *)
            (* will be extra header lines to copy over.  We don't let   *)
            (* the recursive call overwrite sender information.         *)

            EVAL (ProcessItem (L, srccid, dstcid, boundary, AddrTemp, dummy));
            RETURN TRUE;

        END (*IF*);

    END ProcessItem;

(************************************************************************)

PROCEDURE DistributeItem (L: MailingList;
                              VAR (*IN*) filename: FilenameString);

    (* Sends this mail item to everyone on the list, or rejects it if   *)
    (* it turns out to be from a non-subscriber.                        *)

    VAR NewFilename, messagefile: FilenameString;
        message, message2: ARRAY [0..255] OF CHAR;
        srccid, dstcid: ChanId;
        terminator: LineBuffer;
        From: EmailAddress;
        reject, SendToModerator: BOOLEAN;
        count, failures: CARDINAL;

    BEGIN
        (* Because we want to modify some of the header lines, we       *)
        (* make a copy of the original file and send the copy.          *)

        dstcid := OpenNewOutputFile (".\", ".tmp", NewFilename);
        srccid := OpenOldFile (filename, FALSE, FALSE);
        terminator[0] := CtrlZ;
        terminator[1] := Nul;
        IF ProcessItem (L, srccid, dstcid, terminator, From, reject) THEN
            LWriteString (L^.lang, dstcid, "ListChecker.RemAttach");
            FWriteLn (dstcid);
        END (*IF*);
        CloseFile (srccid);

        (* Abort the operation? *)

        IF reject THEN
            CloseFile (dstcid);
            DeleteFile (NewFilename);
            IF L^.SaveRejects THEN
                CopyMessage (filename, L^.RejectDir);
            END (*IF*);
            IF L^.NonsubOption = 1 THEN
                Obtain (NSMLock);
                messagefile := NotSubscribedMessage;
                Release (NSMLock);
                SendCannedMessage (From, L, messagefile, L^.lang, From);
            END (*IF*);
            IF LogLevel > lognone THEN
                IF L^.NonsubOption = 1 THEN
                    StrToBufferA (AdminLang, "ListChecker.logmail.rejected",
                                                From, message);
                ELSE
                    StrToBufferA (AdminLang, "ListChecker.logmail.ignored",
                                                From, message);
                END (*IF*);
                LogTransaction (L^.LogID, message);
            END (*IF*);
            RETURN;
        END (*IF*);

        SendToModerator := L^.Moderated AND NOT IsOnList (From, L^.Owners);

        (* Add in the trailer material if any. *)

        IF (L^.Trailer[0] <> Nul) AND NOT SendToModerator THEN
            AppendFromFile (dstcid, L, L^.lang, L^.Trailer, From, '');
        END (*IF*);

        CloseFile (dstcid);

        failures := 0;
        IF SendToModerator THEN
            count := DeliverItem (From, NewFilename, failures, 0, L^.Owners,
                                                        LogLevel, L^.LogID);
        ELSE
            IF L^.MemberCount > L^.DigestUsers THEN
                count := DeliverItem (L^.MailErrorsTo, NewFilename, failures, 1,
                                               L^.Members, LogLevel, L^.LogID);
            ELSE
                count := 0;
            END (*IF*);
            IF L^.ArchiveMessages THEN
                AddToArchive (L^.archive, NewFilename);
            END (*IF*);
        END (*IF*);
        DeleteFile (NewFilename);
        IF SendToModerator THEN
            StrToBufferAN (AdminLang, "ListChecker.logmail.senttomoderators",
                                                   From, count, message);
        ELSE
            StrToBufferAN (AdminLang, "ListChecker.logmail.sent",
                                                   From, count, message);
        END (*IF*);
        IF failures > 0 THEN
            StrToBufferN (AdminLang, "ListChecker.logmail.failcount",
                                                     failures, message2);
            Strings.Append (message2, message);
        END (*IF*);
        LogTransaction (L^.LogID, message);

    END DistributeItem;

(************************************************************************)

PROCEDURE RunFilter (L: MailingList;  filename: FilenameString): CARDINAL;

    (* This procedure is to be invoked after a mail item has been       *)
    (* received but before it has been distributed to the addressees.   *)
    (* It returns the following codes:                                  *)
    (*    0    continue processing normally, i.e. deliver mail          *)
    (*    1    do not deliver the mail.                                 *)

    CONST ONLength = 256;

    VAR j, result: CARDINAL;
        ArgString: FilenameString;
        FailureObjectName: ARRAY [0..ONLength-1] OF CHAR;
        ExitStatus: OS2.RESULTCODES;

    BEGIN
        IF L^.FilterProg[0] = Nul THEN
            RETURN 0;
        END (*IF*);

        ArgString := "CMD /C ";
        Strings.Append (L^.FilterProg, ArgString);
        Strings.Append (" ", ArgString);
        Strings.Append (filename, ArgString);

        (* Special rule for ArgString: it must be terminated by two Nul *)
        (* characters, and the program name and arguments must also be  *)
        (* separated by a Nul.  We have to insert the separating Nul    *)
        (* after everything else has been done, otherwise it would mess *)
        (* up the Strings.Append operation.                             *)

        j := LENGTH(ArgString) + 1;
        IF j < FilenameLength THEN
            ArgString[j] := Nul;
        END (*IF*);
        ArgString[3] := Nul;

        ReleaseScreen;
        result := OS2.DosExecPgm (FailureObjectName, ONLength,
                                  OS2.EXEC_SYNC, ArgString, NIL,
                                  ExitStatus, "CMD.EXE");
        RegainScreen;

        (* Starting in background (code 457) is not an error. *)

        IF (result = 0) OR (result = 457) THEN
            result := ExitStatus.codeResult;
        ELSE
            result := 0;
        END (*IF*);
        RETURN result;

    END RunFilter;

(************************************************************************)

PROCEDURE HandleMailFor (L: MailingList);

    (* Checks whether this list has any pending mail, processes it if   *)
    (* so.                                                              *)

    VAR filename: FilenameString;
        logmessage: ARRAY [0..255] OF CHAR;

    BEGIN
        LOOP
            FirstItem (L^.ListID, filename, L^.LogID);
            IF filename[0] = Nul THEN EXIT(*LOOP*) END(*IF*);
            StrToBuffer (AdminLang, "ListChecker.processing.one", logmessage);
            LogTransaction (L^.LogID, logmessage);
            IF RunFilter(L, filename) = 0 THEN
                DistributeItem (L, filename);
            ELSE
                IF L^.SaveRejects THEN
                    CopyMessage (filename, L^.RejectDir);
                END (*IF*);
                IF LogLevel > lognone THEN
                    StrToBuffer (AdminLang, "ListChecker.rejectedbyfilter",
                                                logmessage);
                    LogTransaction (L^.LogID, logmessage);
                END (*IF*);
            END (*IF*);
            DeleteFirstItem (L^.ListID, L^.LogID);
        END (*LOOP*);
    END HandleMailFor;

(************************************************************************)
(*                  ADDING AN ITEM TO THE EVENT LIST                    *)
(************************************************************************)

PROCEDURE AddEvent (L: MailingList;  time: CARDINAL);

    (* Notes that we have to check list L again at 'time'. *)

    VAR previous, current: MailingList;

    BEGIN
        L^.checktime := time;
        Obtain (EventList.access);
        previous := NIL;  current := EventList.head;
        WHILE (current <> NIL) AND (time >= current^.checktime) DO
            previous := current;  current := current^.next;
        END (*LOOP*);
        IF previous = NIL THEN
            EventList.head := L;
        ELSE
            previous^.next := L;
        END (*IF*);
        L^.next := current;
        Release (EventList.access);
        Signal (CheckEventList);
    END AddEvent;

(************************************************************************)
(*                      DESTROYING A MAILING LIST                       *)
(************************************************************************)

PROCEDURE DeleteConfirmationList (VAR (*INOUT*) L: ConfirmationListPointer);

    (* Disposes of a confirmation list. *)

    VAR next: ConfirmationListPointer;

    BEGIN
        WHILE L <> NIL DO
            next := L^.next;  DISPOSE(L);  L := next;
        END (*WHILE*);
    END DeleteConfirmationList;

(************************************************************************)

PROCEDURE DiscardList (VAR (*INOUT*) L: MailingList);

    (* Disposes of an obsolete mailing list. *)

    VAR previous, current: AllListNodePtr;
        logmessage: ARRAY [0..255] OF CHAR;

    BEGIN
        StrToBufferA (AdminLang, "ListChecker.removing",
                                 L^.OurName, logmessage);
        LogTransaction (L^.LogID, logmessage);

        (* Unlink L from the master list. *)

        Obtain (AllLists.access);
        previous := NIL;  current := AllLists.head;
        WHILE (current <> NIL) AND (current^.this <> L) DO
            previous := current;
            current := current^.next;
        END (*WHILE*);
        IF current <> NIL THEN
            IF previous = NIL THEN
                AllLists.head := current^.next;
            ELSE
                previous^.next := current^.next;
            END (*IF*);
            DISPOSE (current);
        END (*IF*);
        DEC (AllLists.count);
        Release (AllLists.access);

        (* Destroy all components of L. *)

        WITH L^ DO
            DiscardAddressList(Owners);
            DiscardAddressList(Members);
            IF ArchiveMessages THEN
                DeregisterArchive (archive);
            END (*IF*);
            DestroySemaphore (DigestSemaphore);
            DestroyLock (access);
            UnregisterMailUser (ListID);
            DiscardLogID (LogID);
            WITH Pending DO
                Obtain (access);
                DeleteConfirmationList (head);
                DestroyLock (access);
            END (*WITH*);
            DropLanguage(lang);
        END (*WITH*);
        DISPOSE (L);

    END DiscardList;

(************************************************************************)
(*            THE TASK THAT SENDS MAIL TO DIGEST SUBSCRIBERS            *)
(************************************************************************)

PROCEDURE SendDigest (L: MailingList;  recipients: AddressList);

    VAR
        TimeBuffer: ARRAY [0..31] OF CHAR;
        cid: ChanId;  dummy: CARDINAL;
        filename: FilenameString;

    BEGIN
        cid := OpenNewOutputFile (".\", ".###", filename);

        (* Write the header lines. *)

        CurrentDateAndTime (TimeBuffer);
        FWriteString (cid, "Date: ");
        FWriteString (cid, TimeBuffer);
        FWriteLn (cid);
        FWriteString (cid, "From: ");
        FWriteString (cid, L^.OurEmailAddress);
        FWriteLn (cid);
        FWriteString (cid, "To: ");
        FWriteString (cid, L^.OurEmailAddress);
        FWriteLn (cid);
        FWriteString (cid, "Subject: ");
        LWriteStringA (L^.lang, cid, "ListChecker.digest.Subject", L^.OurName);
        FWriteLn (cid);
        FWriteString (cid, "MIME-version: 1.0");
        FWriteLn (cid);
        FWriteString (cid, "Content-type: text/plain; charset=");
        IF L^.charset[0] = Nul THEN
            FWriteString (cid, "us-ascii");
            FWriteLn (cid);
            FWriteString (cid, "Content-transfer-encoding: 7bit");
        ELSE
            FWriteString (cid, L^.charset);
            FWriteLn (cid);
            FWriteString (cid, "Content-transfer-encoding: 8bit");
        END (*IF*);
        FWriteLn (cid);
        AddListHeaders (cid, L);
        FWriteLn (cid);
        CloseFile (cid);

        (* Append the digest itself, send it, then delete the file. *)

        AppendDigest (L^.archive, filename);
        EVAL (DeliverItem (L^.MailErrorsTo, filename, dummy, 0,
                                  recipients, LogLevel, L^.LogID));
        DeleteFile (filename);

    END SendDigest;

(************************************************************************)

PROCEDURE DigestTask (ForList: ADDRESS);

    (* Runs as a separate task.  This task is asleep most of the time,  *)
    (* but is awakened by a semaphore Signal from the Archives module.  *)
    (* Note that we run a copy of this for every list, even those       *)
    (* with no archive subscribers, to simplify the job of keeping      *)
    (* track of which tasks have to be shut down.                       *)

    VAR L: MailingList;
        digestmembers: AddressList;

    BEGIN
        L := ForList;
        LOOP
            Wait (L^.DigestSemaphore);
            IF ShutdownRequest THEN
                EXIT (*LOOP*);
            END (*IF*);
            Obtain (L^.access);
            IF L^.obsolete THEN
                Release (L^.access);
                EXIT (*LOOP*);
            END (*IF*);
            IF GetDigestMembers (L^.Members, digestmembers) THEN
                SendDigest (L, digestmembers);
                DiscardAddressList (digestmembers);
            END (*IF*);
            IF L^.obsolete THEN
                Release (L^.access);
                EXIT (*LOOP*);
            END (*IF*);
            Release (L^.access);
            IF ShutdownRequest THEN
                EXIT (*LOOP*);
            END (*IF*);
        END (*LOOP*);

        Signal (DigestTaskDone);

    END DigestTask;

(************************************************************************)

PROCEDURE ShutdownDigestTasks;

    (* Terminates all instances of DigestTask.  We assume that the      *)
    (* global variable ShutdownRequest is already TRUE.                 *)

    VAR count: CARDINAL;   p: AllListNodePtr;  L: MailingList;

    BEGIN
        count := 0;
        WITH AllLists DO
            Obtain (access);
            p := head;
            WHILE p <> NIL DO
                L := p^.this;
                Obtain (L^.access);
                IF L^.DigestTaskRunning THEN
                    Signal (L^.DigestSemaphore);
                    INC (count);
                END (*IF*);
                Release (L^.access);
                p := p^.next;
            END (*WHILE*);
            Release (access);
        END (*WITH*);

        WHILE count > 0 DO
            Wait (DigestTaskDone);
            DEC (count);
        END (*WHILE*);

    END ShutdownDigestTasks;

(************************************************************************)
(*                      THE MAIN LIST CHECKER TASK                      *)
(************************************************************************)

PROCEDURE ListCheckerTask;

    (* Runs as a separate task.  Periodically checks all mailing lists  *)
    (* to see whether something new has come in, distributes it if so.  *)

    CONST DefaultCheckInterval = 30*1000;    (* thirty seconds *)

    VAR TimeToSleep, Now: CARDINAL;  TimedOut: BOOLEAN;
        L: MailingList;
        logmessage: ARRAY [0..255] OF CHAR;

    BEGIN
        TimeToSleep := DefaultCheckInterval;
        LOOP
            TimedWait (CheckEventList, TimeToSleep, TimedOut);
            IF ShutdownRequest THEN
                EXIT (*LOOP*);
            END (*IF*);
            LOOP
                Obtain (EventList.access);
                L := EventList.head;
                Now := time();
                IF (L = NIL) OR (L^.checktime > Now) THEN
                    Release (EventList.access);
                    EXIT (*LOOP*);
                END (*IF*);

                (* It's time to recheck this list. *)

                EventList.head := L^.next;
                Release (EventList.access);
                Obtain (L^.access);
                IF LogLevel >= logdebug THEN
                    StrToBuffer (AdminLang, "ListChecker.checkingnew", logmessage);
                    LogTransaction (L^.LogID, logmessage);
                END (*IF*);
                IF L^.obsolete THEN
                    IF L^.DigestTaskRunning THEN
                        Signal (L^.DigestSemaphore);
                        L^.DigestTaskRunning := FALSE;
                        Release (L^.access);
                        Wait (DigestTaskDone);
                    ELSE
                        Release (L^.access);
                    END (*IF*);
                    DiscardList (L);
                ELSE
                    HandleMailFor (L);
                    AddEvent (L, Now + L^.interval);
                    Release (L^.access);
                END (*IF*);
                IF ShutdownRequest THEN
                    EXIT (*LOOP*);
                END (*IF*);

            END (*LOOP*);

            IF L = NIL THEN
                TimeToSleep := DefaultCheckInterval;
            ELSE
                TimeToSleep := 1000*(L^.checktime - Now);
            END (*IF*);

        END (*LOOP*);

        Signal (TaskDone);

    END ListCheckerTask;

(************************************************************************)

PROCEDURE RecheckMailForAllLists;

    (* Forces a check to see whether there's new mail for any list. *)

    VAR Now: CARDINAL;  L: MailingList;

    BEGIN
        (* Modify the checktime of everything on the event list. *)

        Now := time();
        Obtain (EventList.access);
        L := EventList.head;
        WHILE L <> NIL DO
            L^.checktime := Now;
            L := L^.next;
        END (*WHILE*);
        Release (EventList.access);

        (* Wake up the list checker. *)

        Signal (CheckEventList);

    END RecheckMailForAllLists;

(************************************************************************)
(*                      CREATING A MAILING LIST                         *)
(************************************************************************)

PROCEDURE CreateList (ListName: ARRAY OF CHAR): MailingList;

    (* Creates a new empty mailing list. *)

    VAR L: MailingList;

    BEGIN
        NEW (L);
        WITH L^ DO
            next := NIL;
            CreateLock (access);
            interval := DefaultInterval;
            checktime := time();
            ListID := RegisterMailUser (ListName);
            Owners := CreateAddressList();
            Members := CreateAddressList();
            Strings.Assign ("", password);
            Strings.Assign (ListName, OurName);
            Strings.Assign (ListName, LoginName);
            Strings.Assign (ListName, OurEmailAddress);
            IF OurDomain[0] <> Nul THEN
                Strings.Append ("@", OurEmailAddress);
                Strings.Append (OurDomain, OurEmailAddress);
            END (*IF*);
            charset := "iso-8859-1";
            RFC2919ID[0] := Nul;
            MailErrorsTo[0] := Nul;
            Moderated := FALSE;
            SuppressFrom := FALSE;
            KillAttachments := FALSE;
            DigestTaskRunning := FALSE;
            ArchiveMessages := FALSE;
            CreateSemaphore (DigestSemaphore, 0);
            MemberCount := 0;
            DigestUsers := 0;
            Leader[0] := Nul;
            Trailer[0] := Nul;
            WITH Enabled DO
                Subscribe1 := TRUE;  Subscribe2 := TRUE;
                Unsubscribe2 := TRUE;  Who := TRUE;
            END (*WITH*);
            Add2369Headers := FALSE;
            mark := FALSE;  obsolete := FALSE;
            SaveRejects := FALSE;
            RejectDir[0] := Nul;
            NonsubOption := 0;
            FilterProg[0] := Nul;
            WelcomeMessage[0] := Nul;
            DepartureMessage[0] := Nul;
            ConfirmationRequestMessage[0] := Nul;
            language := "en";
            RequireConfirmation := FALSE;
            OwnersMayConfUnsub := FALSE;
            WITH Pending DO
                CreateLock (access);
                ConfTime := 0;
                head := NIL;
            END (*WITH*);
        END (*WITH*);

        RETURN L;

    END CreateList;

(************************************************************************)

PROCEDURE MakeNewList (ListName: ARRAY OF CHAR): MailingList;

    (* Creates a new empty mailing list, and adds it to the list of lists. *)

    VAR L: MailingList;
        NP: AllListNodePtr;

    BEGIN
        L := CreateList (ListName);
        Obtain (AllLists.access);
        NEW (NP);
        NP^.next := AllLists.head;
        NP^.this := L;
        AllLists.head := NP;
        INC (AllLists.count);
        Release (AllLists.access);
        RETURN L;
    END MakeNewList;

(************************************************************************)
(*                           LIST PROPERTIES                            *)
(************************************************************************)

PROCEDURE ListName (L: MailingList;  VAR (*OUT*) name: ListNameType);

    (* Returns the textual name of this list. *)

    BEGIN
        Obtain (L^.access);
        name := L^.OurName;
        Release (L^.access);
    END ListName;

(************************************************************************)

PROCEDURE ListLanguage (L: MailingList): LangHandle;

    (* Returns the language assigned to this list. *)

    VAR result: LangHandle;

    BEGIN
        Obtain (L^.access);
        result := L^.lang;
        Release (L^.access);
        RETURN result;
    END ListLanguage;

(************************************************************************)

PROCEDURE MailUserIDof (L: MailingList;
                    VAR (*OUT*) ListEmailAddress: EmailAddress): MailUser;

    (* Returns the mail user ID and e-mail address belonging to this list. *)

    VAR ID: MailUser;

    BEGIN
        Obtain (L^.access);
        ListEmailAddress := L^.OurEmailAddress;
        ID := L^.ListID;
        Release (L^.access);
        RETURN ID;
    END MailUserIDof;

(************************************************************************)

PROCEDURE LoadListOfNames (hini: HINI;  ListName, key: ARRAY OF CHAR;
                            result: AddressList;
                             VAR (*OUT*) Total, DigestUsers: CARDINAL);

    (* Loads a list of e-mail addresses from the INI file. *)

    VAR options: OptionSet;
        Name: EmailAddress;

    (********************************************************************)

    PROCEDURE GetOptions;

        (* Works out the options for one recipient. *)

        VAR k: CARDINAL;

        BEGIN
            options := OptionSet{mayread, maywrite};
            IF Name[0] = '(' THEN
                k := 1;
                LOOP
                    IF Name[k] = ')' THEN
                        INC (k);
                        EXIT (*LOOP*);
                    ELSIF CAP(Name[k]) = '-' THEN
                        EXCL (options, maywrite);
                        EXCL (options, mayread);
                    ELSIF CAP(Name[k]) = 'R' THEN
                        EXCL (options, maywrite);
                    ELSIF CAP(Name[k]) = 'W' THEN
                        EXCL (options, mayread);
                    ELSIF CAP(Name[k]) = 'D' THEN
                        INCL (options, digestmember);
                        EXCL (options, mayread);
                    ELSE
                        EXIT (*LOOP*);
                    END (*IF*);
                    INC (k);
                END (*LOOP*);

                (* Skip spaces after options. *)

                WHILE (k < EmailAddressSize) AND (Name[k] = Space) DO
                    INC (k);
                END (*WHILE*);

                (* Remove the option flags. *)

                Strings.Delete (Name, 0, k);

            END (*IF*);
        END GetOptions;

    (********************************************************************)

    VAR Name2, DisplayName: EmailAddress;
        state: StringReadState;

    BEGIN
        Total := 0;  DigestUsers := 0;
        GetStringList (hini, ListName, key, state);
        LOOP
            NextString (state, Name);
            IF Name[0] = Nul THEN
                EXIT (*LOOP*);
            END (*IF*);
            GetOptions;
            ExtractEmailAddress (Name, 0, DisplayName, Name2);
            AddRecipient (result, Name2, options);
            INC (Total);
            IF digestmember IN options THEN
                INC (DigestUsers);
            END (*IF*);
        END (*LOOP*);
        CloseStringList (state);
    END LoadListOfNames;

(************************************************************************)
(*             MIGRATION FROM KEY "names" TO KEY "Members"              *)
(*                                                                      *)
(*  This code first included in version 1.598, 3 Jul 2008.  It can be   *)
(*  retired after a few more releases.                                  *)
(*                                                                      *)
(************************************************************************)

PROCEDURE MigrateMembers (hini: HINI;  ListName: ListNameType);

    (* Merges the member names in the INI file entries (ListName,names) *)
    (* and (ListName,Members), puts the combined list back in the       *)
    (* "Members" section, and deletes the "names" section.              *)

    VAR namelist, Memberlist: AddressList;
        size, nameCount, MemberCount, dummy: CARDINAL;

    BEGIN
        IF ItemSize (hini, ListName, "names", size) AND (size > 0) THEN
            namelist := CreateAddressList();
            Memberlist := CreateAddressList();
            LoadListOfNames (hini, ListName, 'names', namelist,
                                                   nameCount, dummy);
            LoadListOfNames (hini, ListName, 'Members', Memberlist,
                                                   MemberCount, dummy);
            MergeAndResave (hini, ListName, Memberlist, namelist);
            DiscardAddressList (Memberlist);
            DiscardAddressList (namelist);
            INIDeleteKey (hini, ListName, "names");
        END (*IF*);
    END MigrateMembers;

(************************************************************************)
(*                        LOADING LIST PARAMETERS                       *)
(************************************************************************)

PROCEDURE SetLogID (L: MailingList);

    (* Creates a log ID from the abbreviation field. *)

    VAR j: [0..7];  finished: BOOLEAN;
        prefix: ARRAY [0..7] OF CHAR;
        logmessage: ARRAY [0..255] OF CHAR;

    BEGIN
        finished := FALSE;
        FOR j := 0 TO 7 DO
            finished := finished OR (L^.abbreviation[j] = Nul);
            IF finished THEN prefix[j] := Space
            ELSE prefix[j] := L^.abbreviation[j]
            END (*IF*);
        END (*FOR*);
        L^.LogID := CreateLogID (MMctx, prefix);
        StrToBufferA (AdminLang, "ListChecker.addinglist", L^.OurName, logmessage);
        LogTransaction (L^.LogID, logmessage);
    END SetLogID;

(************************************************************************)

PROCEDURE LoadList (hini: HINI;  ListName: ListNameType);

    (* Loads or reloads the list information from the INI file.  In the *)
    (* case of a new list, also puts the initial event for this list    *)
    (* onto the event list, and starts the digest task if needed.       *)

    VAR L: MailingList;
        NewList, bool, ArchiveExists, changed: BOOLEAN;
        TimeUnit: TimeType;
        dummy1, dummy2, TimeBetweenUpdates: CARDINAL;
        Owner: EmailAddress;
        logmessage: ARRAY [0..255] OF CHAR;

    BEGIN
        NewList := NOT IdentifyList (ListName, L);
        IF NewList THEN
            L := MakeNewList (ListName);
            MigrateMembers (hini, ListName);
        END (*IF*);
        TimeBetweenUpdates := 28;
        TimeUnit := days;
        changed := NewList;

        Obtain (L^.access);
        ArchiveExists := L^.ArchiveMessages;
        IF INIValid (hini) THEN

            IF NOT changed THEN
                EVAL (INIGet (hini, ListName, 'changed', changed));
            END (*IF*);

            IF changed THEN

                IF NOT NewList THEN
                    bool := FALSE;
                    INIPut (hini, ListName, 'changed', bool);
                    DiscardAddressList (L^.Owners);
                    DiscardAddressList (L^.Members);
                    L^.DigestUsers := 0;
                    L^.Owners := CreateAddressList();
                    L^.Members := CreateAddressList();
                END (*IF*);

                LoadListOfNames (hini, ListName, 'Owners', L^.Owners, dummy1, dummy2);

                (* This next operation is to deal with some obsolescent INI *)
                (* file entries.  'ListOwner' and 'Moderator' have now been *)
                (* replaced by 'Owners'.                                    *)

                IF NOT ItemSize (hini, ListName, 'Owners', dummy1) THEN
                    IF NOT INIGetString (hini, ListName, 'ListOwner', Owner) THEN
                        EVAL (INIGetString (hini, ListName, 'Moderator', Owner));
                    END (*IF*);
                    AddRecipient (L^.Owners, Owner, OptionSet{mayread, maywrite});
                END (*IF*);

                LoadListOfNames (hini, ListName, 'Members', L^.Members,
                                      L^.MemberCount, L^.DigestUsers);

                EVAL (INIGet (hini, ListName, 'interval', L^.interval));
                EVAL (INIGetString (hini, ListName, 'RFC2919ID', L^.RFC2919ID));
                EVAL (INIGet (hini, ListName, 'Add2369Headers', L^.Add2369Headers));
                IF NOT INIGet (hini, ListName, 'DMARCcompatible', L^.DMARCcompatible) THEN
                    L^.DMARCcompatible := FALSE;
                END (*IF*);
                EVAL (INIGetString (hini, ListName, 'Abbreviation', L^.abbreviation));
                IF NOT INIGetString (hini, ListName, 'charset', L^.charset) THEN
                    L^.charset := "iso-8859-1";
                END (*IF*);
                EVAL (INIGet (hini, ListName, 'ArcEnabled', L^.ArchiveMessages));
                EVAL (INIGet (hini, ListName, 'ArcInterval', TimeBetweenUpdates));
                EVAL (INIGet (hini, ListName, 'ArcTimeUnit', TimeUnit));
                EVAL (INIGetString (hini, ListName, 'LoginName', L^.LoginName));
                EVAL (INIGetString (hini, ListName, 'Password', L^.password));
                EVAL (INIGetString (hini, ListName, 'MailErrorsTo', L^.MailErrorsTo));
                EVAL (INIGet (hini, ListName, 'KillAttachments', L^.KillAttachments));
                EVAL (INIGet (hini, ListName, 'IsModerated', L^.Moderated));
                EVAL (INIGet (hini, ListName, 'SuppressFrom', L^.SuppressFrom));
                EVAL (INIGetString (hini, ListName, 'Leader', L^.Leader));
                EVAL (INIGetString (hini, ListName, 'Trailer', L^.Trailer));
                EVAL (INIGet (hini, ListName, 'Nonsub', L^.NonsubOption));
                EVAL (INIGetString (hini, ListName, 'FilterProg', L^.FilterProg));
                EVAL (INIGetString (hini, ListName, 'NotifyOwnerMessage', L^.NotifyOwnerMessage));
                EVAL (INIGetString (hini, ListName, 'NotifyOwnerUnsubMessage', L^.NotifyOwnerUnsubMessage));
                EVAL (INIGetString (hini, ListName, 'WelcomeMessage', L^.WelcomeMessage));
                EVAL (INIGetString (hini, ListName, 'DepartureMessage', L^.DepartureMessage));
                IF NOT INIGetString (hini, ListName, 'ConfReqMessage', L^.ConfirmationRequestMessage) THEN
                    Strings.Assign ("Canned\en\SampleConfReqMessage.txt", L^.ConfirmationRequestMessage);
                END (*IF*);
                IF INIGet (hini, ListName, 'ControlledSubs', bool) THEN
                    L^.Enabled.Subscribe1 := NOT bool;
                    L^.Enabled.Subscribe2 := NOT bool;
                    INIDeleteKey (hini, ListName, 'ControlledSubs');
                END (*IF*);
                WITH L^.Enabled DO
                    EVAL (INIGet (hini, ListName, 'EnableSubscribe', Subscribe1));
                    EVAL (INIGet (hini, ListName, 'EnableLongSubscribe', Subscribe2));
                    EVAL (INIGet (hini, ListName, 'EnableLongUnsubscribe', Unsubscribe2));
                    EVAL (INIGet (hini, ListName, 'EnableWho', Who));
                END (*WITH*);
                EVAL (INIGet (hini, ListName, 'RequireConfirmation', L^.RequireConfirmation));
                EVAL (INIGet (hini, ListName, 'OwnersMayConfUnsub', L^.OwnersMayConfUnsub));
                IF NOT INIGet (hini, ListName, 'ConfTime', L^.Pending.ConfTime) THEN
                    L^.Pending.ConfTime := 24;
                END (*IF*);
                SetLoginInfo (L^.ListID, L^.LoginName, L^.password);

                EVAL (INIGet (hini, ListName, 'SaveRejects', L^.SaveRejects));
                EVAL (INIGetString (hini, ListName, 'RejectDir', L^.RejectDir));
                EVAL (INIGetString (hini, ListName, 'language', L^.language));
                L^.lang := UseLanguage ("MM", L^.language);

            END (*IF changed*);

        END (*IF*);

        L^.mark := TRUE;   (* to note that this list is still active. *)

        IF NewList THEN
            SetLogID (L);
        ELSIF changed THEN
            StrToBufferA (AdminLang, "ListChecker.refreshinglist",
                                                 L^.OurName, logmessage);
            LogTransaction (L^.LogID, logmessage);
        END (*IF*);

        IF L^.ArchiveMessages <> ArchiveExists THEN
            IF ArchiveExists THEN
                DeregisterArchive (L^.archive);
                L^.ArchiveMessages := FALSE;
            ELSE
                L^.archive := RegisterArchive (ListName, L^.lang, TimeBetweenUpdates,
                                    TimeUnit, L^.DigestSemaphore, L^.LogID);
            END (*IF*);
        END (*IF*);

        (* Put the new list onto the event list. *)

        IF NewList THEN
            AddEvent (L, L^.checktime);
            L^.DigestTaskRunning := CreateTask1 (DigestTask, 3, "digest", L);
        END (*IF*);

        Release (L^.access);

    END LoadList;

(************************************************************************)

PROCEDURE RegisterAllLists;

    (* Refreshes the list information for all lists in the INI file.    *)
    (* Note that this can be called while a list operation is in        *)
    (* progress, so we need to be careful about critical sections.      *)

    TYPE BufferIndex = [0..65535];

    VAR hini: HINI;
        Name: ListNameType;
        p: AllListNodePtr;
        state: StringReadState;
        app: ARRAY [0..4] OF CHAR;

    BEGIN
        app := "$SYS";
        hini := OpenINIFile (INIFileName, UseTNI);
        IF NOT INIValid (hini) THEN
            CloseINIFile (hini);
            RETURN;
        END (*IF*);
        Obtain (NSMLock);
        IF NOT INIGetString (hini, app, 'NotSubscribed', NotSubscribedMessage) THEN
            NotSubscribedMessage := "Canned\en\NotSubscribed.txt";
        END (*IF*);
        Release (NSMLock);

        app[0] := Nul;
        GetStringList (hini, app, app, state);
        LOOP
            NextString (state, Name);
            IF Name[0] = Nul THEN
                EXIT (*LOOP*);
            END (*IF*);
            IF NOT Strings.Equal (Name, '$SYS') THEN
                LoadList (hini, Name);
            END (*IF*);
        END (*LOOP*);
        CloseStringList (state);
        CloseINIFile (hini);

        (* At this stage all active lists have their 'mark' flag set.   *)
        (* Use this to check for obsolete lists.                        *)

        Obtain (AllLists.access);
        p := AllLists.head;
        WHILE p <> NIL DO
            IF NOT p^.this^.mark THEN
                p^.this^.obsolete := TRUE;
            END (*IF*);
            p^.this^.mark := FALSE;
            p := p^.next;
        END (*WHILE*);
        Release (AllLists.access);

    END RegisterAllLists;

(************************************************************************)
(*                           INITIALISATION                             *)
(************************************************************************)

PROCEDURE SetININame (name: ARRAY OF CHAR;  TNImode: BOOLEAN);

    (* The caller specifies the INI file name and mode.  *)

    BEGIN
        Strings.Assign (name, INIFileName);
        UseTNI := TNImode;
    END SetININame;

(************************************************************************)

PROCEDURE SetOurDomainName (domain: EmailAddress;
                             DetailOfLogging: LogLevelType);

    (* The caller specifies to us what our e-mail domain is, and        *)
    (* notifies us how detailed the logging should be.                  *)

    BEGIN
        Strings.Assign (domain, OurDomain);
        LogLevel := DetailOfLogging;
    END SetOurDomainName;

(************************************************************************)

PROCEDURE SetAdministratorAddress (admin: EmailAddress;
                                   lang, default: LangHandle);

    (* The caller specifies to us the e-mail address of the             *)
    (* administrator account for this copy of Major Major, the language *)
    (* to be used for administrator mail, and the language to be used   *)
    (* when no language is specified.                                   *)

    BEGIN
        Strings.Assign (admin, AdministratorAddress);
        AdminLang := lang;
        DefaultLang := default;
        SetLoggingLanguage (lang);
    END SetAdministratorAddress;

(************************************************************************)

BEGIN
    MMctx := OpenLogContext();
    AdministratorAddress := "";
    LogLevel := logsummary;
    IF NotDetached() THEN
        ClearScreen;
        SetBoundary(2, 30);
    END (*IF*);
    ShutdownRequest := FALSE;
    CreateSemaphore (TaskDone, 0);
    CreateSemaphore (DigestTaskDone, 0);
    NotSubscribedMessage := "";
    CreateLock (NSMLock);
    WITH AllLists DO
        CreateLock (access);
        head:= NIL;
        count := 0;
    END (*WITH*);
    WITH EventList DO
        head := NIL;
        CreateLock (access);
    END (*WITH*);
    CreateSemaphore (CheckEventList, 0);
    EVAL(CreateTask (ListCheckerTask, 3, "list checker"));
FINALLY
    ShutdownRequest := TRUE;
    ShutdownDigestTasks;
    Signal (CheckEventList);
    Wait (TaskDone);
    CloseLogContext (MMctx);
END ListChecker.

