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

IMPLEMENTATION MODULE ListAdmin;

        (********************************************************)
        (*                                                      *)
        (*                Mailing list manager                  *)
        (*         Module to look after mail to list admin      *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            21 May 2000                     *)
        (*  Last edited:        28 June 2014                    *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings, OS2;

FROM Commands IMPORT
    (* type *)  Requester,
    (* proc *)  SetOurName, CreateRequester, CloseRequester, DoCommand,
                PlainTextNeeded;

FROM MailFetcher IMPORT
    (* type *)  MailUser,
    (* proc *)  SetLoginInfo, FirstItem, DeleteFirstItem;

FROM LogLevel IMPORT
    (* type *)  LogLevelType;

FROM AddressLists IMPORT
    (* type *)  EmailAddress;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer, StrToBufferA;

FROM Misc IMPORT
    (* proc *)  HeadMatch, ExtractEmailAddress;

FROM Inet2Misc IMPORT
    (* proc *)  StringMatch;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  LogTransaction;

FROM SplitScreen IMPORT
    (* proc *)  ReleaseScreen, RegainScreen;

FROM FileOps IMPORT
    (* const*)  FilenameLength,
    (* type *)  ChanId, FilenameString,
    (* proc *)  OpenOldFile, CloseFile, ReadLine;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

(********************************************************************************)

CONST
    Nul = CHR(0);
    CtrlZ = CHR(26);

TYPE
    LineBuffer = ARRAY [0..1023] OF CHAR;

VAR
    (* MailFetcher ID for the administrator. *)

    MU: MailUser;

    (* The language to use for transaction logging. *)

    OurLang: LangHandle;

    (* The amount of logging to do. *)

    LogLevel: LogLevelType;

    (* Filter for admin mail. *)

    AdminFilterProg: FilenameString;

(************************************************************************)

PROCEDURE CheckForBoundaryCode (srccid: ChanId;
                                VAR (*INOUT*) NextLine: LineBuffer;
                                VAR (*OUT*) boundary: LineBuffer);

    (* Checks this header line, and its continuation lines, for a       *)
    (* "boundary=..." field.  On entry, we have already scanned past    *)
    (* the first semicolon.  On return, NextLine is the first input     *)
    (* line that doesn't belong to us.                                  *)

    CONST Tab = CHR(8);

    VAR StillSearching, found: BOOLEAN;  pos: CARDINAL;
        quotechar: ARRAY [0..1] OF CHAR;

    BEGIN
        StillSearching := TRUE;

        (* Now scan this line, plus any continuation lines, for a       *)
        (* boundary code.  If we find one we can stop checking, but we  *)
        (* still have to keep reading the input lines until we get to   *)
        (* the next non-continuation line.                              *)

        LOOP   (* once per line *)

            IF StillSearching THEN

                LOOP      (* once per item in the line *)

                    (* Strip leading spaces and tabs. *)

                    pos := 0;
                    WHILE (NextLine[pos] = ' ') OR (NextLine[pos] = Tab) DO
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
                        quotechar[0] := NextLine[0];
                        quotechar[1] := Nul;
                        Strings.Delete (NextLine, 0, 1);
                        Strings.FindNext (quotechar, NextLine, 0, found, pos);
                        IF found THEN
                            NextLine[pos] := Nul;
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
            IF (NextLine[0] <> ' ') AND (NextLine[0] <> Tab) THEN EXIT(*LOOP*) END(*IF*);

        END (*LOOP*);

    END CheckForBoundaryCode;

(********************************************************************************)

PROCEDURE ProcessRequests (LogID: TransactionLogID;  filename: FilenameString);

    (* Processes one incoming mail item for the list administrator. *)

    VAR buffer: LineBuffer;

    (********************************************************************)

    PROCEDURE SpaceStrip (VAR (*INOUT*) buffer: ARRAY OF CHAR);

        (* Removes leading and trailing spaces from buffer. *)

        VAR j: CARDINAL;

        BEGIN
            (* Strip leading spaces. *);

            j := 0;
            WHILE buffer[j] = ' ' DO
                INC(j);
            END (*WHILE*);
            IF j > 0 THEN
                Strings.Delete (buffer, 0, j);
            END (*IF*);

            (* Strip trailing spaces. *);

            j := Strings.Length(buffer);
            WHILE (j > 0) AND (buffer[j-1] = ' ') DO
                DEC(j);
            END (*WHILE*);
            buffer[j] := Nul;

        END SpaceStrip;

    (********************************************************************)

    VAR cid: ChanId;
        subject, contenttype, boundarycode, DisplayName: LineBuffer;
        from: EmailAddress;
        req: Requester;
        pos: CARDINAL;
        found, Done, HaveSenderAddress,
             bufferisloaded, commandinsubject: BOOLEAN;

    BEGIN
        cid := OpenOldFile (filename, FALSE, FALSE);
        subject := "";
        boundarycode := "";
        from := "";
        contenttype := "text/plain";
        bufferisloaded := FALSE;
        HaveSenderAddress := FALSE;

        (* Read the header, extracting the sender address, subject, and content *)
        (* type.  All other header lines are ignored.                           *)

        LOOP
            IF NOT bufferisloaded THEN
                ReadLine (cid, buffer);
            END (*IF*);
            bufferisloaded := FALSE;    (* default after processing this line. *)
            IF (buffer[0] = Nul) OR (buffer[0] = CtrlZ) THEN EXIT(*LOOP*) END(*IF*);
            IF HeadMatch (buffer, "Content-type:") THEN
                Strings.Delete (buffer, 0, 13);
                Strings.FindNext (';', buffer, 0, found, pos);
                Strings.Assign (buffer, contenttype);
                IF found THEN
                    contenttype[pos] := Nul;
                END (*IF*);
                SpaceStrip (contenttype);
                IF HeadMatch (contenttype, "multipart") THEN
                    CheckForBoundaryCode (cid, buffer, boundarycode);
                    bufferisloaded := TRUE;
                END (*IF*);
            ELSIF HeadMatch (buffer, "Subject:") THEN
                Strings.Delete (buffer, 0, 8);
                Strings.Assign (buffer, subject);
                SpaceStrip (subject);
            ELSIF HeadMatch (buffer, "Return-Path:") THEN

                (* We assume that the Return-Path, if present, gives the    *)
                (* most reliable indication of the sender's address.        *)

                ExtractEmailAddress (buffer, 12, DisplayName, from);
                HaveSenderAddress := TRUE;

            ELSIF NOT HaveSenderAddress THEN

                (* We check the Reply-To and From headers only if there was     *)
                (* no Return-Path; and, in that case, we give priority to the   *)
                (* Reply-To if present.                                         *)

                IF HeadMatch (buffer, "Reply-To:") THEN
                    ExtractEmailAddress (buffer, 9, DisplayName, from);
                    HaveSenderAddress := TRUE;
                ELSIF HeadMatch (buffer, "From:") THEN
                    ExtractEmailAddress (buffer, 5, DisplayName, from);
                END (*IF*);

            END (*IF*);

        END (*LOOP*);

        (* If the sender address is empty - which can happen, for example,      *)
        (* if this message is a 'postmaster' bounce - ignore the message.       *)

        IF from[0] <> Nul THEN

            req := CreateRequester (from);

            (* Allow the option of a command in the Subject line, including *)
            (* the possibility of a command following a 'Re:'.              *)

            IF HeadMatch (subject, "Re:") THEN
                Strings.Delete (subject, 0, 3);
                SpaceStrip (subject);
            END (*IF*);
            commandinsubject := DoCommand (req, subject, 0);

            (* Check for bodies that are not text/plain. *)

            IF NOT StringMatch (contenttype, "text/plain") THEN

                (*continue := ContinueWithBadMessage(req);*)
                PlainTextNeeded (req, contenttype);
                IF boundarycode[0] <> Nul THEN

                    (* Skip to the boundary code, and then continue skipping    *)
                    (* until we have passed a blank line.                       *)

                    REPEAT
                        ReadLine (cid, buffer);
                        Done := (buffer[0] = CtrlZ) OR StringMatch(buffer, boundarycode);
                    UNTIL Done;
                    IF buffer[0] <> CtrlZ THEN
                        REPEAT
                            ReadLine (cid, buffer);
                        UNTIL (buffer[0] = CtrlZ) OR (buffer[0] = Nul);
                    END (*IF*);
                END (*IF*);

            END (*IF*);

            (* Process the body of the message, skipping lines that begin   *)
            (* with '>'.   If we have found a syntactically valid command   *)
            (* in the Subject: line then we terminate the body processing   *)
            (* as soon as we meet a line that can't be a command.  If not,  *)
            (* we continue processing until we meet an 'end' command, or    *)
            (* end of message, or a line starting with '-'.                 *)

            IF commandinsubject THEN
                pos := 1;
            ELSE
                pos := 2;
            END (*IF*);

            REPEAT
                ReadLine (cid, buffer);
                SpaceStrip (buffer);
                Done := (buffer[0] = CtrlZ) OR (buffer[0] = '-');
                IF NOT Done AND (buffer[0] <> Nul) THEN
                    IF NOT (buffer[0] = '>') THEN
                        Done := DoCommand (req, buffer, pos);
                    END (*IF*);
                END (*IF*);
            UNTIL Done;

            IF CloseRequester (req, LogID) THEN
                StrToBufferA (OurLang, "ListAdmin.requestfrom", from, buffer);
            ELSE
                StrToBufferA (OurLang, "ListChecker.logmail.ignored", from, buffer);
            END (*IF*);
            LogTransaction (LogID, buffer);

        END (*IF*);

        CloseFile (cid);

    END ProcessRequests;

(************************************************************************)

PROCEDURE RunFilter (filename: FilenameString): CARDINAL;

    (* This procedure is to be invoked after a mail item has been       *)
    (* received but before it has been processed.                       *)
    (* It returns the following codes:                                  *)
    (*    0    continue processing normally, i.e. execute its commands  *)
    (*    1    ignore the mail.                                         *)

    CONST ONLength = 256;

    VAR j, result: CARDINAL;
        ArgString: FilenameString;
        FailureObjectName: ARRAY [0..ONLength-1] OF CHAR;
        ExitStatus: OS2.RESULTCODES;

    BEGIN
        IF AdminFilterProg[0] = Nul THEN
            RETURN 0;
        END (*IF*);

        ArgString := "CMD /C ";
        Strings.Append (AdminFilterProg, ArgString);
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

(********************************************************************************)

PROCEDURE ProcessAdminRequests (LogID: TransactionLogID);

    (* Called periodically to see whether there's any mail for the list         *)
    (* administrator, processes it if so.                                       *)

    VAR filename: FilenameString;
        logmessage: ARRAY [0..255] OF CHAR;

    BEGIN
        LOOP
            FirstItem (MU, filename, LogID);
            IF filename[0] = Nul THEN EXIT(*LOOP*) END(*IF*);
            IF RunFilter(filename) = 0 THEN
                ProcessRequests (LogID, filename);
            ELSE
                IF LogLevel > lognone THEN
                    StrToBuffer (OurLang, "ListChecker.rejectedbyfilter",
                                                logmessage);
                    LogTransaction (LogID, logmessage);
                END (*IF*);
            END (*IF*);
            DeleteFirstItem (MU, LogID);
        END (*LOOP*);
    END ProcessAdminRequests;

(********************************************************************************)
(*                             INITIALISATION                                   *)
(********************************************************************************)

PROCEDURE SetAdminName (accountname, loginname: EmailAddress;
                         adminpassword: ARRAY OF CHAR;
                          AdminLang, DefaultLang: LangHandle;
                           LogDetail: LogLevelType;
                             Filter: FilenameString;  acceptinvalid: BOOLEAN);

    (* Sets the name and password for the e-mail account to be used as the      *)
    (* administrator account.  Also specifies the language for admin messages,  *)
    (* the default user language, how detailed the transaction log should be,   *)
    (* the name of the optional filter for admin main, and whether we're        *)
    (* willing to respond to invalid input.                                     *)

    BEGIN
        MU := SetOurName (accountname, AdminLang,
                                   DefaultLang, LogDetail, acceptinvalid);
        OurLang := AdminLang;
        LogLevel := LogDetail;
        AdminFilterProg := Filter;
        SetLoginInfo (MU, loginname, adminpassword);
    END SetAdminName;

(********************************************************************************)

END ListAdmin.

