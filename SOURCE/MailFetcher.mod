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

IMPLEMENTATION MODULE MailFetcher;

        (********************************************************)
        (*                                                      *)
        (*                Mailing list manager                  *)
        (*             Module to pick up the mail               *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            21 May 2000                     *)
        (*  Last edited:        8 July 2008                     *)
        (*  Status:             Basic structure OK              *)
        (*                                                      *)
        (********************************************************)

IMPORT Strings, FileSys, OS2;

FROM MailOut IMPORT
    (* proc *)  SetLocalOperation;

FROM POPClient IMPORT
    (* type *)  POP3User,
    (* proc *)  RegisterPOP3User, SetPOP3LoginDetails, DiscardPOP3User,
                FetchFirstMessage, DeleteFirstMessage;

FROM FileOps IMPORT
    (* type *)  FilenameString, DirectoryEntry,
    (* proc *)  FirstDirEntry, NextDirEntry, DirSearchDone;

FROM TransLog IMPORT
    (* type *)  TransactionLogID;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    (* Descriptor for one item of incoming mail.                        *)
    (*    next        next item on the list                             *)
    (*    FileName    the file holding the mail text                    *)

    MailItem = POINTER TO MailItemRecord;
    MailItemRecord = RECORD
                         next: MailItem;
                         FileName: FilenameString;
                     END (*RECORD*);

    (* Information we keep for one mail username.                       *)
    (*    POPuser       this user's ID within the POP3 system, if       *)
    (*                     we're using POP3 to fetch mail               *)
    (*    DirName       this user's mail directory                      *)
    (*    FirstMessage  head of the queue of incoming mail              *)

    MailUser = POINTER TO
                   RECORD
                       POPuser: POP3User;
                       DirName: FilenameString;
                       FirstMessage: MailItem;
                   END (*RECORD*);

VAR
    (* Root directory for the incoming mail files. *)

    MailRoot: FilenameString;

    (* Flag to say that we receive incoming mail by POP3.  If this      *)
    (* flag is false, we get mail from a local directory.               *)

    UsePOP: BOOLEAN;

(************************************************************************)
(*                  CREATING A STATUS RECORD FOR ONE USER               *)
(************************************************************************)

PROCEDURE RegisterMailUser (name: ARRAY OF CHAR): MailUser;

    VAR result: MailUser;

    BEGIN
        NEW (result);
        WITH result^ DO
            IF UsePOP THEN
                POPuser := RegisterPOP3User();
            ELSE
                DirName := MailRoot;
                Strings.Append (name, DirName);
                Strings.Append ('\', DirName);
            END (*IF*);
            FirstMessage := NIL;
        END (*WITH*);
        RETURN result;
    END RegisterMailUser;

(************************************************************************)

PROCEDURE SetLoginInfo (MU: MailUser;  UserName, NewPassword: ARRAY OF CHAR);

    (* Sets the POP3 username and password for the mail account. *)

    BEGIN
        WITH MU^ DO
            IF UsePOP THEN
                SetPOP3LoginDetails (POPuser, UserName, NewPassword);
            END (*IF*);
        END (*WITH*);
    END SetLoginInfo;

(************************************************************************)

PROCEDURE DiscardMailQueue (VAR (*INOUT*) head: MailItem);

    (* Throws away the collected list of waiting mail. *)

    VAR next: MailItem;

    BEGIN
        WHILE head <> NIL DO
            next := head^.next;
            DISPOSE (head);
            head := next;
        END (*WHILE*);
    END DiscardMailQueue;

(************************************************************************)

PROCEDURE UnregisterMailUser (VAR (*INOUT*) MU: MailUser);

    (* Removes our stored data for this user. *)

    BEGIN
        DiscardMailQueue (MU^.FirstMessage);
        IF UsePOP THEN
            DiscardPOP3User (MU^.POPuser);
        END (*IF*);
        DISPOSE (MU);
    END UnregisterMailUser;

(************************************************************************)
(*              CHECKING FOR NEW FILES IN A MAIL DIRECTORY              *)
(************************************************************************)

PROCEDURE CheckMailDirectory (user: MailUser);

    (* Checks all non-hidden *.MSG files in the user's mail directory,  *)
    (* and adds them to the list of incoming mail for this user.        *)

    VAR mask, filename: FilenameString;
        D: DirectoryEntry;
        found: BOOLEAN;
        previous, current, this: MailItem;

    BEGIN
        mask := user^.DirName;
        Strings.Append ("*.MSG", mask);
        found := FirstDirEntry (mask, FALSE, FALSE, D);
        WHILE found DO
            filename := user^.DirName;
            Strings.Append (D.name, filename);
            previous := NIL;  current := user^.FirstMessage;
            LOOP
                IF current = NIL THEN EXIT(*LOOP*) END(*IF*);
                IF Strings.Equal (filename, current^.FileName) THEN EXIT(*LOOP*) END(*IF*);
                previous := current;  current := current^.next;
            END (*LOOP*);
            IF current = NIL THEN
                (* Add new message to tail of list. *)
                NEW (this);
                WITH this^ DO
                    FileName := filename;
                    next := NIL;
                END (*WITH*);
                IF previous = NIL THEN
                    user^.FirstMessage := this;
                ELSE
                    previous^.next := this;
                END (*IF*);
            END (*IF*);

            found := NextDirEntry(D);
        END (*WHILE*);
        DirSearchDone (D);

    END CheckMailDirectory;

(************************************************************************)
(*                     RETURNING THE LIST OF MAIL                       *)
(************************************************************************)

PROCEDURE CheckForNewMail (user: MailUser;
                           LogID: TransactionLogID);

    (* Refreshes the list of incoming mail. *)

    VAR this: MailItem;

    BEGIN
        IF UsePOP THEN
            NEW (this);
            this^.next := NIL;
            FetchFirstMessage (user^.POPuser, this^.FileName, LogID);
            IF this^.FileName[0] = Nul THEN
                DISPOSE (this);
            END (*IF*);
            user^.FirstMessage := this;
        ELSE
            CheckMailDirectory (user);
        END (*IF*);
    END CheckForNewMail;

(************************************************************************)

PROCEDURE FirstItem (user: MailUser;
                     VAR (*OUT*) filename: ARRAY OF CHAR;
                     LogID: TransactionLogID);

    (* Returns the name of the first incoming mail item for this user.  *)

    VAR result: MailItem;

    BEGIN
        IF user^.FirstMessage = NIL THEN
            CheckForNewMail (user, LogID);
        END (*IF*);
        result := user^.FirstMessage;
        IF result = NIL THEN
            filename[0] := Nul;
        ELSE
            Strings.Assign (result^.FileName, filename);
        END (*IF*);
    END FirstItem;

(************************************************************************)

PROCEDURE DeleteFirstItem (user: MailUser;  LogID: TransactionLogID);

    (* Removes one item from the system.  This includes deleting its    *)
    (* disk file.                                                       *)

    VAR item: MailItem;  done: BOOLEAN;

    BEGIN
        item := user^.FirstMessage;
        IF item <> NIL THEN
            user^.FirstMessage := item^.next;
            FileSys.Remove (item^.FileName, done);
            DISPOSE (item);
            IF UsePOP THEN
                DeleteFirstMessage (user^.POPuser, LogID);
            END (*IF*);
        END (*IF*);
    END DeleteFirstItem;

(************************************************************************)
(*                           INITIALISATION                             *)
(************************************************************************)

PROCEDURE EnableLocalOperation (maildir, ourdomain: ARRAY OF CHAR);

    (* Sets us up to send/receive mail by looking in a local disk       *)
    (* directory.  If this procedure is not called, we use POP3 to      *)
    (* fetch mail and SMTP to send it.                                  *)

    BEGIN
        UsePOP := FALSE;
        Strings.Assign (maildir, MailRoot);
        IF ourdomain[0] <> Nul THEN
            Strings.Append (ourdomain, MailRoot);
            Strings.Append ('\', MailRoot);
        END (*IF*);
        SetLocalOperation (maildir);
    END EnableLocalOperation;

(************************************************************************)

PROCEDURE DisableLocalOperation;

    (* Sets us up to send/receive mail by using POP3 to      *)
    (* fetch mail and SMTP to send it.                       *)

    BEGIN
        UsePOP := TRUE;
        SetLocalOperation ("");
    END DisableLocalOperation;

(************************************************************************)

BEGIN
    UsePOP := TRUE;
END MailFetcher.

