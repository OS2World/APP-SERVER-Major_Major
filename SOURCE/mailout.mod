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

IMPLEMENTATION MODULE MailOut;

        (********************************************************)
        (*                                                      *)
        (*                Mailing list manager                  *)
        (*           Module to handle outbound mail             *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            12 June 2000                    *)
        (*  Last edited:        30 March 2015                   *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


(*
FROM TransLog IMPORT LogTransaction;   (* while debugging *)
FROM Inet2Misc IMPORT ConvertCard;     (* while debugging *)
*)

IMPORT OS2, FileSys, Strings;

FROM SYSTEM IMPORT
    (* type *)  CARD8, BOOL8;

FROM TransLog IMPORT
    (* type *)  TransactionLogID;

FROM AddressLists IMPORT
    (* type *)  EmailAddress, AddressList,
    (* proc *)  FirstOnList, NextOnList, FirstGroup, NextGroup,
                EmptyList, DiscardAddressList;

FROM LogLevel IMPORT
    (* type *)  LogLevelType;

FROM SMTPOut IMPORT
    (* proc *)  MailOneMessage;

FROM Misc IMPORT
    (* proc *)  OpenNewOutputFile, MakeNewFilename;

FROM FileOps IMPORT
    (* type *)  ChanId, FilenameString,
    (* proc *)  OpenOldFile, CloseFile, ReadRaw, WriteRaw,
                FWriteChar, FWriteString;

(************************************************************************)

CONST
    Nul = CHR(0);

TYPE

    (* While relay mail is waiting to be sent it is stored as a file    *)
    (* which starts with the following details.                         *)
    (*   4 bytes   format version, value 'V000'                         *)
    (*   4 bytes   send time                                            *)
    (*   1 byte    retry number                                         *)
    (*   1 byte    Boolean notify-on-failure flag                       *)
    (*   variable  sender (character string)                            *)
    (*   variable  recipient list, bounded by () and comma-separated    *)
    (*                                                                  *)
    (* The message content starts immediately after this.               *)

    PreambleType = RECORD
                       version: ARRAY [0..3] OF CHAR;
                       sendtime: CARDINAL;
                       RetryNumber: CARD8;
                       NotifyOnFailure: BOOL8;
                   END (*RECORD*);

(************************************************************************)

VAR
    (* Flag to say that we send outbound mail via an SMTP server.       *)
    (* If this flag is false, we send mail out by putting it in         *)
    (* Weasel's "forward" directory.                                    *)

    UseSMTP: BOOLEAN;

    (* Directory that holds outbound mail.  *)

    ForwardDirName: FilenameString;

    (* Event semaphore to tell Weasel to look at its forward directory. *)

    ForceOnlineCheck: OS2.HEV;

(************************************************************************)
(*                          FILE OPERATIONS                             *)
(************************************************************************)

PROCEDURE WriteNameList (fid: ChanId;  option: CARDINAL;
                                             list: AddressList): CARDINAL;

    (* Writes a comma-separated list of names, enclosed in parentheses. *)
    (* Returns the number of recipients.  The options are               *)
    (*       0      send to everyone on the recipients list             *)
    (*       1      send only to the 'mayread' recipients               *)

    VAR name: EmailAddress;  found: BOOLEAN;  count: CARDINAL;

    BEGIN
        count := 0;
        found := FirstOnList (list, option, name);
        FWriteChar (fid, '(');
        WHILE found DO
            INC (count);
            FWriteString (fid, name);
            found := NextOnList (list, option, name);
            IF found THEN
                FWriteChar (fid, ',');
            END (*IF*);
        END (*WHILE*);
        FWriteChar (fid, ')');
        RETURN count;
    END WriteNameList;

(************************************************************************)

PROCEDURE WritePreamble (cid: ChanId;
                        VAR (*IN*) sender: ARRAY OF CHAR;
                        option: CARDINAL;
                        recipients: AddressList): CARDINAL;

    (* Writes the preamble details.  Returns the number of recipients.  *)

    VAR preamble: PreambleType;

    BEGIN
        (* Write the preamble to the new file. *)

        WITH preamble DO
            version := "V000";
            sendtime := 0;
            RetryNumber := 0;
            NotifyOnFailure := NOT Strings.Equal (sender, "<>");
        END (*WITH*);
        WriteRaw (cid, preamble, SIZE(preamble));
        WriteRaw (cid, sender, LENGTH(sender));
        RETURN WriteNameList (cid, option, recipients);

    END WritePreamble;

(************************************************************************)

PROCEDURE AppendBody (srcname: ARRAY OF CHAR;  dstcid: ChanId);

    (* Copies from a source file to the destination.  The caller should *)
    (* have already opened the destination file, but not the source file.*)

    VAR srccid: ChanId;  NumberRead: CARDINAL;
        buffer: ARRAY [0..2047] OF CHAR;

    BEGIN
        (* Open the source file and position it at the right place. *)

        srccid := OpenOldFile (srcname, FALSE, TRUE);

        (* Copy across the message body. *)

        LOOP
            ReadRaw (srccid, buffer, 2048, NumberRead);
            IF NumberRead = 0 THEN EXIT(*LOOP*) END(*IF*);
            WriteRaw (dstcid, buffer, NumberRead);
        END (*LOOP*);

        CloseFile (srccid);

    END AppendBody;

(************************************************************************)
(*              ADDING A JOB TO THE QUEUE OF OUTBOUND MAIL              *)
(************************************************************************)

(*
PROCEDURE ReportCount (LogID: TransactionLogID;  message: ARRAY OF CHAR;  count: CARDINAL);

    (* For debugging: writes message and count to log file. *)

    VAR logline: ARRAY [0..127] OF CHAR;
        pos: CARDINAL;

    BEGIN
        Strings.Assign (message, logline);
        pos := Strings.Length (logline);
        ConvertCard (count, logline, pos);
        logline[pos] := Nul;
        LogTransaction (LogID, logline);
    END ReportCount;
*)

(************************************************************************)

PROCEDURE DoDelivery (sender: EmailAddress;
                            VAR (*IN*) sourcefile: FilenameString;
                            VAR (*OUT*) failures: CARDINAL;
                            option: CARDINAL;
                            recipients: AddressList;
                            LogLevel: LogLevelType;
                            LogID: TransactionLogID): CARDINAL;

    (* Delivers the mail in sourcefile to the recipients, with sender   *)
    (* identified as the originator of the mail.  The options are       *)
    (*       0      send to everyone on the recipients list             *)
    (*       1      send only to the 'mayread' recipients               *)
    (* The return value is the number of recipients for whom the        *)
    (* delivery was successful.  The failures parameter counts those to *)
    (* whom we were unable to deliver the message.                      *)

    VAR TmpFile, FinalFile: FilenameString;
        cid: ChanId;
        count, tries: CARDINAL;  done: BOOLEAN;

    BEGIN
        IF sender[0] = Nul THEN
            Strings.Assign ("<>", sender);
        END (*IF*);
        IF UseSMTP THEN
            count := MailOneMessage (sender, sourcefile, failures, option,
                                            recipients, LogLevel, LogID);
        ELSE
            cid := OpenNewOutputFile (ForwardDirName, ".tmp", TmpFile);
            count := WritePreamble (cid, sender, option, recipients);
            (*ReportCount (LogID, "After WritePreamble, count = ", count);*)
            AppendBody (sourcefile, cid);
            CloseFile (cid);
            tries := 0;
            REPEAT
                MakeNewFilename (ForwardDirName, ".fwd", FinalFile);
                FileSys.Rename (TmpFile, FinalFile, done);
                INC (tries);
            UNTIL done OR (tries > 99);
            IF done THEN
                failures := 0;
            ELSE
                failures := count;
                count := 0;
            END (*IF*);
            OS2.DosPostEventSem (ForceOnlineCheck);
            OS2.DosResetEventSem (ForceOnlineCheck, tries);
        END (*IF*);
        RETURN count;
    END DoDelivery;

(************************************************************************)

PROCEDURE DeliverItem (sender: EmailAddress;
                            VAR (*IN*) sourcefile: FilenameString;
                            VAR (*OUT*) failures: CARDINAL;
                            option: CARDINAL;
                            recipients: AddressList;
                            LogLevel: LogLevelType;
                            LogID: TransactionLogID): CARDINAL;

    (* Delivers the mail in sourcefile to the recipients, with sender   *)
    (* identified as the originator of the mail.  The options are       *)
    (*       0      send to everyone on the recipients list             *)
    (*       1      send only to the 'mayread' recipients               *)
    (* The return value is the number of recipients for whom the        *)
    (* delivery was successful.  The failures parameter counts those to *)
    (* whom we were unable to deliver the message.                      *)

    VAR successes, lost, count, groupsize: CARDINAL;
        dest: AddressList;

    BEGIN
        successes := 0;
        failures := 0;
        dest := FirstGroup (option, recipients, groupsize);
        WHILE NOT EmptyList (dest) DO
            count := DoDelivery (sender, sourcefile, lost, option,
                                                  dest, LogLevel, LogID);
            IF count > 0 THEN
                INC (successes, count);
            END (*IF*);
            IF lost > 0 THEN
                INC (failures, lost);
            END (*IF*);
            DiscardAddressList (dest);
            dest := NextGroup (option, recipients, groupsize);
        END (*WHILE*);
        DiscardAddressList (dest);
        RETURN successes;
    END DeliverItem;

(************************************************************************)
(*                        INITIALISATION                                *)
(************************************************************************)

PROCEDURE SetLocalOperation (root: ARRAY OF CHAR);

    (* To be called during program initialisation or reinitialisation.  *)
    (* The caller specifies the name of the mail root directory.  If    *)
    (* root is the empty string, or if this routine is never called,    *)
    (* we use SMTP instead to send mail.                                *)

    BEGIN
        UseSMTP := root[0] = Nul;
        IF NOT UseSMTP THEN
            Strings.Assign (root, ForwardDirName);
            Strings.Append ("forward\", ForwardDirName);
        END (*IF*);
    END SetLocalOperation;

(************************************************************************)

CONST semName = "\SEM32\WEASEL\FORWARDMAIL";

BEGIN
    UseSMTP := TRUE;
    ForceOnlineCheck := 0;
    IF OS2.DosOpenEventSem (semName, ForceOnlineCheck) = OS2.ERROR_SEM_NOT_FOUND THEN
        OS2.DosCreateEventSem (semName, ForceOnlineCheck, OS2.DC_SEM_SHARED, FALSE);
    END (*IF*);
FINALLY
    OS2.DosCloseEventSem (ForceOnlineCheck);
END MailOut.

