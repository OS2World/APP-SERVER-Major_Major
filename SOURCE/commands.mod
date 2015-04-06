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

IMPLEMENTATION MODULE Commands;

        (********************************************************)
        (*                                                      *)
        (*                Mailing list manager                  *)
        (*         Handler for list administrator commands      *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            5 July 2000                     *)
        (*  Last edited:        22 May 2012                     *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (*  The code in procedure DoCommand can possibly be     *)
        (*  improved, by removing duplications.                 *)
        (*                                                      *)
        (********************************************************)


IMPORT SYSTEM, Strings;

FROM ListChecker IMPORT
    (* type *)  MailingList, ConfirmationCode,
    (* proc *)  CreateList, IdentifyList, IsOwner, IsMember, AddMember, RemoveMember,
                ConfirmationRequired, SendConfirmationRequest, AddConfirmationEntry,
                DoConfirmation, MainLanguage, ListName, ListLanguage,
                AppendFromFile, WhoIsLegal, DoWho, ListLists, ReturnIndex,
                MailUserIDof, SetAdministratorAddress;

FROM MMV IMPORT
    (* const*)  version;

FROM Archives IMPORT
    (* proc *)  AppendFromArchive;

FROM AddressLists IMPORT
    (* type *)  OptionType, OptionSet, ListNameType, EmailAddress, AddressList,
    (* proc *)  CreateAddressList, AddRecipient, DiscardAddressList;

FROM Languages IMPORT
    (* type *)  LangHandle, LangName,
    (* proc *)  LWriteString, LWriteStringA, LWriteStringAB,
                UseLanguage, DropLanguage, LanguageCode;

FROM MailFetcher IMPORT
    (* type *)  MailUser;

FROM MailOut IMPORT
    (* proc *)  DeliverItem;

FROM LogLevel IMPORT
    (* type *)  LogLevelType;

FROM MyClock IMPORT
    (* proc *)  CurrentDateAndTime;

FROM TransLog IMPORT
    (* type *)  TransactionLogID;

FROM Misc IMPORT
    (* proc *)  OpenNewOutputFile;

FROM FileOps IMPORT
    (* type *)  FilenameString, ChanId,
    (* proc *)  DeleteFile, CloseFile, FWriteString, FWriteLn;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    (* A Requester record keeps track of the state of one command       *)
    (* session, i.e. the processing of one e-mail to the list admin     *)
    (* software.                                                        *)
    (*     address      the client's e-mail address                     *)
    (*     replycid     channel id for the file in which we're          *)
    (*                  constructing the response to be sent back       *)
    (*     filename     name of that reply file                         *)
    (*     lang         language to use for replies                     *)
    (*     HaveLang     FALSE iff we don't yet have a clear idea of     *)
    (*                    what lang should be                           *)
    (*     HaveAddedLang TRUE iff we have explicitly loaded a language  *)
    (*                    for this user, as distinct from using one     *)
    (*                    that was already loaded                       *)
    (*     HeaderWritten TRUE iff we have already written the header    *)
    (*                    in the reply we are constructing              *)
    (*     valid        records that we've received at least one        *)
    (*                   valid command                                  *)

    Requester = POINTER TO
                      RECORD
                          address:  EmailAddress;
                          replycid: ChanId;
                          filename: FilenameString;
                          lang:     LangHandle;
                          HaveLang: BOOLEAN;
                          HaveAddedLang: BOOLEAN;
                          HeaderWritten: BOOLEAN;
                          valid:    BOOLEAN;
                      END (*RECORD*);

(************************************************************************)

VAR
    (* The e-mail address of this list manager. *)

    AdministratorAddress: EmailAddress;

    (* The MIME character set to use when composing replies. *)

    MIMEcharset: ARRAY [0..63] OF CHAR;

    (* The name of the help file. *)

    HelpFile: FilenameString;

    (* The name of the explanatory file to send when someone tries to   *)
    (* send commands in a non-plain-text format.                        *)

    PlainTextFile: FilenameString;

    (* A pseudo-list for sending mail out from the administrator. *)

    DummyAdminList: MailingList;

    (* The amount of detail we want in the transaction log. *)

    LogLevel: LogLevelType;

    (* A flag to say whether we'll reply to a message with no valid     *)
    (* commands.  Setting this FALSE is a way to discourage spammers.   *)

    ReplyToInvalidMessage: BOOLEAN;

(************************************************************************)
(*                       WRITING THE REPLY HEADER                       *)
(************************************************************************)

PROCEDURE WriteHeader (R: Requester);

    (* Writes the header of the reply back to the requester. *)

    VAR
        cid: ChanId;
        TimeBuffer: ARRAY [0..31] OF CHAR;

    BEGIN
        cid := R^.replycid;
        CurrentDateAndTime (TimeBuffer);
        FWriteString (cid, "Date: ");
        FWriteString (cid, TimeBuffer);
        FWriteLn (cid);
        FWriteString (cid, "From: ");
        FWriteString (cid, AdministratorAddress);
        FWriteLn (cid);
        FWriteString (cid, "To: ");
        FWriteString (cid, R^.address);
        FWriteLn (cid);
        FWriteString (cid, "Subject: ");
        LWriteString (R^.lang, cid, "Commands.reply.Subject");
        FWriteLn (cid);
        FWriteString (cid, "MIME-version: 1.0");
        FWriteLn (cid);
        FWriteString (cid, "Content-type: text/plain; charset=");
        FWriteString (cid, MIMEcharset);
        FWriteLn (cid);
        FWriteString (cid, "Content-transfer-encoding: 8bit");
        FWriteLn (cid);
        FWriteString (cid, "List-Help: <mailto:");
        FWriteString (cid, AdministratorAddress);
        FWriteString (cid, "?body=HELP>");
        FWriteLn (cid);
        FWriteLn (cid);
        R^.HeaderWritten := TRUE;
    END WriteHeader;

(************************************************************************)
(*                        REGISTERING A REQUESTER                       *)
(************************************************************************)

PROCEDURE CreateRequester (VAR (*IN*) address: EmailAddress): Requester;

    (* Creates a new Requester record. *)

    VAR S: Requester;
        cid: ChanId;

    BEGIN
        NEW (S);
        S^.address := address;
        cid := OpenNewOutputFile (".\", ".###", S^.filename);
        S^.replycid := cid;
        S^.HeaderWritten := FALSE;
        S^.valid := FALSE;

        (* Try to work out what language to use. *)

        S^.lang := MainLanguage(address, S^.HaveLang);
        S^.HaveAddedLang := FALSE;

        RETURN S;

    END CreateRequester;

(************************************************************************)

PROCEDURE CloseRequester (VAR (*INOUT*) R: Requester;
                                       LogID: TransactionLogID): BOOLEAN;

    (* The "end of session" operation on a Requester record.  Returns   *)
    (* TRUE if we sent a reply, FALSE if we ignored the mail.           *)

    VAR recipient: AddressList;  dummy: CARDINAL;
        replied: BOOLEAN;

    BEGIN
        replied := FALSE;
        IF ReplyToInvalidMessage AND NOT R^.valid THEN
            IF NOT R^.HeaderWritten THEN
                WriteHeader (R);
            END (*IF*);
            LWriteString (R^.lang, R^.replycid, "Commands.reply.NoneValid");
            FWriteLn (R^.replycid);
            replied := TRUE;
        END (*IF*);

        CloseFile (R^.replycid);

        IF R^.valid OR ReplyToInvalidMessage THEN

            (* Send the reply letter to the client. *)

            recipient := CreateAddressList();
            AddRecipient (recipient, R^.address, OptionSet{mayread});
            EVAL (DeliverItem (AdministratorAddress, R^.filename, dummy, 0,
                                          recipient, LogLevel, LogID));
            DiscardAddressList (recipient);
            replied := TRUE;

        END (*IF*);

        DeleteFile (R^.filename);

        IF R^.HaveAddedLang THEN
            DropLanguage (R^.lang);
        END (*IF*);

        DISPOSE (R);

        RETURN replied;

    END CloseRequester;

(************************************************************************)

(*
PROCEDURE ContinueWithBadMessage (R: Requester): BOOLEAN;

    (* Returns TRUE if we're willing to accept invalid commands.        *)
    (* Otherwise, marks this requester session as "do not reply" and    *)
    (* returns FALSE.                                                   *)

    BEGIN
        IF ReplyToInvalidMessage THEN
            R^.valid := FALSE;
        END (*IF*);
        RETURN ReplyToInvalidMessage;
    END ContinueWithBadMessage;
*)

(************************************************************************)
(*                     MISCELLANEOUS PROCEDURES                         *)
(************************************************************************)

PROCEDURE ArgSplit (VAR (*IN*) arg: ARRAY OF CHAR;
                    VAR (*OUT*) part1, part2: ARRAY OF CHAR);

    (* Splits at the first space in arg: part1 is everything before     *)
    (* the space, part2 is everything after it.  If there is no space   *)
    (* then part2 is the empty string.                                  *)

    VAR pos: CARDINAL;  found: BOOLEAN;

    BEGIN
        Strings.FindNext (' ', arg, 0, found, pos);
        Strings.Assign (arg, part1);
        IF found THEN
            Strings.Extract (arg, pos+1, LENGTH(arg)-pos, part2);
            part1[pos] := Nul;
        ELSE
            part2[0] := Nul;
        END (*IF*);
    END ArgSplit;

(************************************************************************)

PROCEDURE PlainTextNeeded (U: Requester;  VAR (*IN*) contenttype: ARRAY OF CHAR);

    (* Sends a reply saying that the message should have been of type text/plain. *)

    BEGIN
        IF NOT U^.HeaderWritten THEN
            WriteHeader (U);
        END (*IF*);
        LWriteStringA (U^.lang, U^.replycid, "Commands.reply.NotPlainText", contenttype);
        FWriteLn (U^.replycid);
        IF PlainTextFile[0] = Nul THEN
            FWriteString (U^.replycid, "This mailing list processor expects plain text commands,");
            FWriteLn (U^.replycid);
            FWriteString (U^.replycid, "and is not configured to accept HTML or other nonstandard");
            FWriteLn (U^.replycid);
            FWriteString (U^.replycid, "rubbish.  Please configure your e-mail software to send plain text.");
            FWriteLn (U^.replycid);
        ELSE
            AppendFromFile (U^.replycid, DummyAdminList, U^.lang, PlainTextFile, '', '');
        END (*IF*);
    END PlainTextNeeded;

(************************************************************************)
(*                   THE INDIVIDUAL COMMAND HANDLERS                    *)
(************************************************************************)

PROCEDURE NoCommand (U: Requester;  dummy: MailingList;
                                    VAR (*IN*) arg: ARRAY OF CHAR);

    (* Handles the empty command. *)

    VAR pos: CARDINAL;  found: BOOLEAN;

    BEGIN
        Strings.FindNext (' ', arg, 0, found, pos);
        IF found THEN
            arg[pos] := Nul;
        END (*IF*);
        LWriteStringA (U^.lang, U^.replycid, "Commands.reply.NoCommand", arg);
        FWriteLn (U^.replycid);
        FWriteLn (U^.replycid);
    END NoCommand;

(************************************************************************)

PROCEDURE EndCommand (U: Requester;  dummy: MailingList;
                                     VAR (*IN*) arg: ARRAY OF CHAR);

    (* Handles the "end" command. *)

    BEGIN
        arg[0] := arg[0];
        LWriteString (U^.lang, U^.replycid, "Commands.reply.End");
        FWriteLn (U^.replycid);
    END EndCommand;

(************************************************************************)

PROCEDURE GetCommand (U: Requester;  ML: MailingList;
                                     VAR (*IN*) filename: ARRAY OF CHAR);

    (* Handles the "get listname filename" command. *)

    VAR listname: ListNameType;

    BEGIN
        IF IsMember (U^.address, ML) OR IsOwner (U^.address, ML) THEN
            ListName (ML, listname);
            IF NOT AppendFromArchive (U^.replycid, listname, filename) THEN
                LWriteString (U^.lang, U^.replycid, "Commands.nosuchfile");
            END (*IF*);
        ELSE
            LWriteString (U^.lang, U^.replycid, "Commands.notamember");
        END (*IF*);
        FWriteLn (U^.replycid);
    END GetCommand;

(************************************************************************)

PROCEDURE HelpCommand (U: Requester;  dummy: MailingList;
                                      VAR (*IN*) arg: ARRAY OF CHAR);

    (* Handles the "help" command. *)

    BEGIN
        arg[0] := arg[0];
        IF HelpFile[0] = Nul THEN
            LWriteString (U^.lang, U^.replycid, "Commands.nohelpfile");
            FWriteLn (U^.replycid);
        ELSE
            AppendFromFile (U^.replycid, DummyAdminList, U^.lang, HelpFile, '', '');
        END (*IF*);
    END HelpCommand;

(************************************************************************)

PROCEDURE Index (U: Requester;  ML: MailingList;
                                     VAR (*IN*) arg: ARRAY OF CHAR);

    (* Handles the "index" command. *)

    VAR cid: ChanId;

    BEGIN
        arg[0] := arg[0];          (* to avoid a compiler warning *)
        cid := U^.replycid;
        IF IsMember (U^.address, ML) OR IsOwner (U^.address, ML) THEN
            ReturnIndex (cid, U^.lang, ML);
        ELSE
            LWriteString (U^.lang, cid, "Commands.notamember");  FWriteLn(cid);
        END (*IF*);
    END Index;

(************************************************************************)

PROCEDURE LangCommand (U: Requester;  dummy: MailingList;
                                      VAR (*IN*) langname: ARRAY OF CHAR);

    (* Handles the "lang" command. *)

    BEGIN
        IF U^.HaveAddedLang THEN
            DropLanguage (U^.lang);
        END (*IF*);
        U^.lang := UseLanguage("MM", langname);
        U^.HaveAddedLang := TRUE;
        U^.HaveLang := TRUE;
    END LangCommand;

(************************************************************************)

PROCEDURE ListsCommand (U: Requester;  dummy: MailingList;
                                       VAR (*IN*) arg: ARRAY OF CHAR);

    (* Handles the "lists" command. *)

    BEGIN
        arg[0] := arg[0];
        ListLists (U^.replycid, U^.lang, "");
    END ListsCommand;

(************************************************************************)

PROCEDURE Subscribe (U: Requester;  ML: MailingList;
                                    VAR (*IN*) arg: ARRAY OF CHAR);

    (* Handles the "subscribe" command. *)

    VAR cid: ChanId;
        flags: OptionSet;
        listname: ListNameType;
        optioncode, username: EmailAddress;
        code: ConfirmationCode;

    BEGIN
        ListName (ML, listname);

        (* Extract the options, if any. *)

        flags := OptionSet {mayread, maywrite};
        LOOP
            ArgSplit (arg, optioncode, username);
            Strings.Capitalize (optioncode);
            IF Strings.Equal (optioncode, 'READONLY') THEN
                EXCL (flags, maywrite);
            ELSIF Strings.Equal (optioncode, 'WRITEONLY') THEN
                EXCL (flags, mayread);
            ELSIF Strings.Equal (optioncode, 'DIGEST') THEN
                INCL (flags, digestmember);
                EXCL (flags, mayread);
            ELSE
                Strings.Assign (arg, username);
                EXIT (*LOOP*);
            END (*IF*);
            Strings.Assign (username, arg);
        END (*LOOP*);

        IF username[0] = Nul THEN
            username := U^.address;
        END (*IF*);

        cid := U^.replycid;
        IF IsMember (username, ML) THEN
            LWriteStringAB (U^.lang, cid, "Commands.alreadysubscribed", username, listname);
            FWriteLn(cid);
        ELSIF flags = OptionSet{} THEN
            LWriteString (U^.lang, cid, "Commands.ImpossibleCombination");
            FWriteLn(cid);
        ELSIF (flags = OptionSet{maywrite}) AND NOT IsOwner(U^.address, ML) THEN
            LWriteString (U^.lang, cid, "Commands.subscribe.nowriteonly");
            FWriteLn(cid);
        ELSIF ConfirmationRequired (ML) THEN
            IF SendConfirmationRequest (ML, U^.address, username,
                                            U^.lang, TRUE, code) THEN
                AddConfirmationEntry (ML, code, TRUE, flags, username, U^.lang);
                FWriteLn(cid);
                LWriteString (U^.lang, cid, "Commands.subscribe.ConfMailed");
            ELSE
                LWriteString (U^.lang, cid, "Commands.configerror");
            END (*IF*);
            FWriteLn(cid);
        ELSIF AddMember (U^.lang, U^.address, ML, username, flags) THEN
            LWriteStringAB (U^.lang, cid, "Commands.subscribe.success",
                                            username, listname);
            IF digestmember IN flags THEN
                FWriteString (cid, " digest");
            END (*IF*);
            IF NOT (mayread IN flags) AND NOT (digestmember IN flags) THEN
                FWriteString (cid, " (writeonly)");
            ELSIF NOT (maywrite IN flags) THEN
                FWriteString (cid, " (readonly)");
            END (*IF*);
            FWriteLn(cid);
        ELSE
            LWriteString (U^.lang, cid, "Commands.subscribe.restricted");
            FWriteLn(cid);
        END (*IF*);
    END Subscribe;

(************************************************************************)

PROCEDURE Unsubscribe (U: Requester;  ML: MailingList;
                                      VAR (*IN*) arg: ARRAY OF CHAR);

    (* Handles the "unsubscribe" command. *)

    VAR cid: ChanId;
        username: EmailAddress;
        listname: ListNameType;
        code: ConfirmationCode;

    BEGIN
        Strings.Assign (arg, username);
        IF username[0] = Nul THEN
            Strings.Assign (U^.address, username);
        END (*IF*);
        cid := U^.replycid;
        IF NOT IsMember(username, ML) THEN
            ListName (ML, listname);
            LWriteStringAB (U^.lang, cid, "Commands.unsubscribe.notamember",
                                            username, listname);
            FWriteLn(cid);
        ELSIF ConfirmationRequired (ML) THEN
            IF SendConfirmationRequest (ML, U^.address, username, U^.lang,
                                                        FALSE, code) THEN
                AddConfirmationEntry (ML, code, FALSE, OptionSet{}, username, U^.lang);
                LWriteString (U^.lang, cid, "Commands.unsubscribe.ConfMailed");
            ELSE
                LWriteString (U^.lang, cid, "Commands.configerror");
            END (*IF*);
        ELSIF RemoveMember (U^.lang, U^.address, ML, username) THEN
            LWriteString (U^.lang, cid, "Commands.unsubscribe.success");
            FWriteLn(cid);
        ELSE
            LWriteString (U^.lang, cid, "Commands.unsubscribe.failure");
            FWriteLn(cid);
        END (*IF*);
    END Unsubscribe;

(************************************************************************)

PROCEDURE Version (U: Requester;  dummy: MailingList;
                                     VAR (*IN*) arg: ARRAY OF CHAR);

    (* Handles the "version" command. *)

    BEGIN
        arg[0] := arg[0];
        LWriteStringA (U^.lang, U^.replycid, "Commands.version", version);
        FWriteLn (U^.replycid);
    END Version;

(************************************************************************)

PROCEDURE Which (U: Requester;  dummy: MailingList;
                                     VAR (*IN*) arg: ARRAY OF CHAR);

    (* Handles the "which" command. *)

    BEGIN
        arg[0] := arg[0];
        ListLists (U^.replycid, U^.lang, U^.address);
    END Which;

(************************************************************************)

PROCEDURE Who (U: Requester;  ML: MailingList;
                                     VAR (*IN*) dummy: ARRAY OF CHAR);

    (* Handles the "who" command. *)

    VAR cid: ChanId;
        listname: ListNameType;

    BEGIN
        cid := U^.replycid;
        IF NOT WhoIsLegal(ML, U^.address) THEN
            ListName (ML, listname);
            LWriteStringA (U^.lang, cid, "Commands.who.unavailable", listname);
            FWriteLn(cid);
        ELSIF IsMember (U^.address, ML) OR IsOwner (U^.address, ML) THEN
            DoWho (cid, U^.lang, ML);
        ELSE
            LWriteString (U^.lang, cid, "Commands.notamember");
            FWriteLn(cid);
        END (*IF*);
    END Who;

(************************************************************************)
(*               CONFIRMING A SUBSCRIPTION OR UNSUBSCRIPTION            *)
(************************************************************************)

PROCEDURE ConfirmSubscription (U: Requester;  IsSubscription: BOOLEAN;
                                    VAR (*IN*) arg: ARRAY OF CHAR);

    (* Confirms a subscription if the second argument is TRUE, or an    *)
    (* unsubscription if the second argument is FALSE.  The string      *)
    (* argument names the list and the confirmation code.               *)

    VAR ML: MailingList;  cid: ChanId;
        oldlang, newlang: LangName;
        listname: EmailAddress;
        code: ConfirmationCode;

    BEGIN
        ArgSplit (arg, listname, code);
        cid := U^.replycid;
        IF listname[0] = Nul THEN
            LWriteString (U^.lang, cid, "Commands.ListNameNeeded");
            FWriteLn(cid);
        ELSIF NOT IdentifyList(listname, ML) THEN
            LWriteStringA (U^.lang, cid, "Commands.nosuchlist", listname);
            FWriteLn (cid);
        ELSIF DoConfirmation (ML, IsSubscription, newlang, code) THEN
            LanguageCode (U^.lang, oldlang);
            IF NOT Strings.Equal (newlang, oldlang) THEN
                LangCommand (U, ML, newlang);
            END (*IF*);
            LWriteString (U^.lang, cid, "Commands.confirm.success");
            FWriteLn(cid);
        ELSE
            LWriteString (U^.lang, cid, "Commands.confirm.failure");
            FWriteLn(cid);
        END (*IF*);
    END ConfirmSubscription;

(************************************************************************)
(*                     CLASSIFICATION OF THE COMMANDS                   *)
(************************************************************************)

TYPE
    CommandType = (Cconfirm, Cend, Cget, Chelp, Cindex, Clang, Clists, Csubscribe,
                   Cunscribe, Cversion, Cwhich, Cwhocommand, Cnocommand);
    CommandProc = PROCEDURE (Requester, MailingList, VAR (*IN*) ARRAY OF CHAR);
    CommandArray = ARRAY CommandType OF CommandProc;
    BoolArray = ARRAY CommandType OF BOOLEAN;

CONST
    AllCommands = "CONFIRM$END$GET$HELP$INDEX$LANG$LISTS$SUBSCRIBE$UNSUBSCRIBE$VERSION$WHICH$WHO$$";
    Handler = CommandArray {NoCommand, EndCommand, GetCommand,
                            HelpCommand, Index, LangCommand,
                            ListsCommand, Subscribe, Unsubscribe,
                            Version, Which, Who, NoCommand};
    NeedList = BoolArray {FALSE, FALSE, TRUE,
                            FALSE, TRUE, FALSE,
                            FALSE, TRUE, TRUE,
                            FALSE, FALSE, TRUE, FALSE};

(************************************************************************)

PROCEDURE ParseCommand (VAR (*INOUT*) command: ARRAY OF CHAR): CommandType;

    (* Works out what command this is, deletes the command keyword      *)
    (* from its argument so that only the arguments are left.           *)

    (********************************************************************)

    PROCEDURE StripHead (j: CARDINAL);

        (* Deletes the first j characters, plus any spaces after that. *)

        BEGIN
            WHILE command[j] = ' ' DO
                INC(j);
            END (*WHILE*);
            IF j > 0 THEN
            Strings.Delete (command, 0, j);
            END (*IF*);
        END StripHead;

    (********************************************************************)

    VAR j, k: CARDINAL;
        result: CommandType;

    BEGIN
        StripHead (0);
        result := MIN(CommandType);
        k := 0;
        LOOP
            j := 0;
            LOOP
                IF AllCommands[k] = '$' THEN
                    (* We have a match. *)
                    EXIT (*LOOP*);
                ELSIF CAP(command[j]) <> AllCommands[k] THEN
                    (* Definite mismatch. *)
                    EXIT (*LOOP*);
                ELSE
                    INC (j);  INC(k);
                END (*IF*);
            END (*LOOP*);

            IF AllCommands[k] = '$' THEN
                (* We have a match. *)
                EXIT (*LOOP*);
            ELSIF result = MAX(CommandType) THEN
                (* We have run out of possibilities. *)
                EXIT (*LOOP*);
            ELSE
                (* Move to next possibility. *);
                REPEAT
                    INC (k);
                UNTIL AllCommands[k] = '$';
                INC (k);
                INC (result);
            END (*IF*);

        END (*LOOP*);

        (* Extra check: it is possible that the "match" we have found   *)
        (* is merely a match with a leading substring of another word.  *)
        (* (Note that no valid command is a leading substring of any    *)
        (* other valid command, so we don't have to worry about that    *)
        (* case.)  We must therefore check to see whether the supposed  *)
        (* command is followed by a space or end-of-line.               *)

        IF (command[j] <> ' ') AND (command[j] <> Nul) THEN
            result := Cnocommand;
        END (*IF*);

        StripHead (j);
        RETURN result;

    END ParseCommand;

(************************************************************************)

PROCEDURE DoCommand (R: Requester;  command: ARRAY OF CHAR;
                                            ErrOption: CARDINAL): BOOLEAN;

    (* Parses and executes the command.  The value of ErrOption         *)
    (* controls how we handle invalid commands:                         *)
    (*                                                                  *)
    (*      0    execute the command if valid, ignore it if not,        *)
    (*           return TRUE iff the command was syntactically valid.   *)
    (*           This is for the case where we're checking whether      *)
    (*           there is a command in the Subject line.                *)
    (*                                                                  *)
    (*      1    execute the command if valid, no error message if not, *)
    (*           return TRUE if the command was the last command to be  *)
    (*           processed (as in case 2) or if the command was not     *)
    (*           syntactically valid.                                   *)
    (*                                                                  *)
    (*      2    execute the command if valid, give an error message    *)
    (*           if not, return TRUE iff this means we should process   *)
    (*           no more commands (i.e. we found an 'end' command).     *)

    VAR ML: MailingList;  cid: ChanId;

    (********************************************************************)

    PROCEDURE DetectList(): BOOLEAN;

        (* Extracts a listname from the head of the command string. *)
        (* Returns FALSE if no valid listname found; in this case   *)
        (* we have already generated the error message for the user.*)

        VAR result: BOOLEAN;
            listname: ListNameType;
            params: FilenameString;

        BEGIN
            result := FALSE;
            ArgSplit (command, listname, params);
            IF listname[0] = Nul THEN
                LWriteString (R^.lang, cid, "Commands.ListNameNeeded");
                FWriteLn(cid);
            ELSIF NOT IdentifyList(listname, ML) THEN
                LWriteStringA (R^.lang, cid, "Commands.nosuchlist", listname);
                FWriteLn (cid);
            ELSE
                Strings.Assign (params, command);
                result := TRUE;
            END (*IF*);
            RETURN result;
        END DetectList;

    (********************************************************************)

    CONST
        NilList = SYSTEM.CAST (MailingList, NIL);

    VAR C: CommandType;
        original: ARRAY [0..1023] OF CHAR;
        result: BOOLEAN;

    BEGIN
        Strings.Assign (command, original);
        cid := R^.replycid;
        C := ParseCommand (command);
        ML := NilList;

        (* The 'confirm' command is a special case, in that it has      *)
        (* a more complicated syntax than the others, so we treate it   *)
        (* separately.                                                  *)

        IF C = Cconfirm THEN

            IF NOT R^.HeaderWritten THEN
                WriteHeader (R);
            END (*IF*);

            (* The subcommand may only be 'subscribe' or 'unsubscribe'. *)

            C := ParseCommand (command);
            IF (C <> Csubscribe) AND (C <> Cunscribe) THEN
                C := Cnocommand;
            END (*IF*);
            result := C <> Cnocommand;

            IF ErrOption = 0 THEN

                IF result THEN
                    FWriteString (cid, "==> ");
                    FWriteString (cid, original);
                    FWriteLn (cid);
                    R^.valid := TRUE;
                    ConfirmSubscription (R, C = Csubscribe, command);
                END (*IF*);

            ELSIF ErrOption = 1 THEN

                IF result THEN
                    FWriteString (cid, "==> ");
                    FWriteString (cid, original);
                    FWriteLn (cid);
                    R^.valid := TRUE;
                    ConfirmSubscription (R, C = Csubscribe, command);
                END (*IF*);
                result := NOT result;

            ELSIF ErrOption = 2 THEN

                FWriteString (cid, "==> ");
                FWriteString (cid, original);
                FWriteLn (cid);
                IF result THEN
                    R^.valid := TRUE;
                    ConfirmSubscription (R, C = Csubscribe, command);
                ELSE
                    NoCommand (R, ML, command);
                END (*IF*);
                result := FALSE;

            ELSE

                LWriteString (R^.lang, cid, "Commands.obsolete");
                FWriteLn (cid);
                result := FALSE;

            END (*IF*);

        ELSIF C = Clang THEN

            (* We treat 'lang' differently because we'd like it *)
            (* to influence the Subject: line in the header,    *)
            (* if the header hasn't already been written.       *)

            LangCommand (R, NIL, command);

            IF NOT R^.HeaderWritten THEN
                WriteHeader (R);
            END (*IF*);
            FWriteString (cid, "==> ");
            FWriteString (cid, original);
            FWriteLn (cid);
            FWriteLn (cid);

            R^.valid := TRUE;
            result := (ErrOption = 0);

        ELSE

            (* All commands except 'confirm' and 'lang'. *)

            result := C <> Cnocommand;

            IF (ErrOption = 2) OR (C <> Cnocommand) THEN
                IF NOT R^.HeaderWritten THEN
                    WriteHeader (R);
                END (*IF*);
                FWriteString (cid, "==> ");
                FWriteString (cid, original);
                FWriteLn (cid);
            END (*IF*);
            IF C <> Cnocommand THEN
                R^.valid := TRUE;
            END (*IF*);

            IF (ErrOption = 2) OR (C <> Cnocommand) THEN
                IF NOT NeedList[C] OR DetectList() THEN
                    IF (ML <> NilList) AND NOT R^.HaveLang THEN
                        R^.lang := ListLanguage(ML);
                        R^.HaveLang := TRUE;
                    END (*IF*);
                    Handler[C] (R, ML, command);
                    FWriteLn (cid);
                END (*IF*);
            END (*IF*);

            IF ErrOption = 1 THEN
                result := (NOT result) OR (C = Cend);
            ELSIF ErrOption = 2 THEN
                result := C = Cend;
            END (*IF*);

        END (*IF*);

        RETURN result;

    END DoCommand;

(************************************************************************)
(*                          INITIALISATION                              *)
(************************************************************************)

PROCEDURE SetOurName (username: EmailAddress;  lang, default: LangHandle;
                                      LogDetail: LogLevelType;
                                        acceptinvalid: BOOLEAN): MailUser;

    (* The caller specifies to us the e-mail username for the list      *)
    (* administrator, i.e. the robot that actually receives the         *)
    (* commands to subscribe, etc.; the language to be used for         *)
    (* administrator functions such as logging; and the language to be  *)
    (* used when no language is specified. The LogDetail parameter      *)
    (* specifies how much detail we want in the transaction log, and    *)
    (* acceptinvalid is FALSE iff we want to ignore messages with no    *)
    (* valid commands.                                                  *)

    VAR result: MailUser;

    BEGIN
        DummyAdminList := CreateList (username);
        result := MailUserIDof (DummyAdminList, AdministratorAddress);
        SetAdministratorAddress (AdministratorAddress, lang, default);
        LogLevel := LogDetail;
        ReplyToInvalidMessage := acceptinvalid;
        RETURN result;
    END SetOurName;

(************************************************************************)

PROCEDURE SetHelpFileName (VAR (*IN*) helpname, plaintextname: ARRAY OF CHAR);

    (* The caller specifies to us the name of two help files.  The      *)
    (* first is the one to use in response to the HELP command.  The    *)
    (* second is for when someone tries to send commands embedded in    *)
    (* a message type that is not plain text.                           *)

    BEGIN
        Strings.Assign (helpname, HelpFile);
        Strings.Assign (plaintextname, PlainTextFile);
    END SetHelpFileName;

(************************************************************************)

BEGIN
    HelpFile := "";
    PlainTextFile := "";
    LogLevel := lognone;
    MIMEcharset := "iso-8859-1";
    ReplyToInvalidMessage := TRUE;
END Commands.

