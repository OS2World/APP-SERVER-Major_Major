(**************************************************************************)
(*                                                                        *)
(*  Admin program for the Major Major mailing list manager                *)
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

IMPLEMENTATION MODULE Misc;

        (************************************************************)
        (*                                                          *)
        (*               Miscellaneous operations                   *)
        (*                                                          *)
        (*    Started:        13 June 2000                          *)
        (*    Last edited:    29 June 2014                          *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)


IMPORT Strings, FileSys, OS2;

FROM Conversions IMPORT
    (* proc *)  CardinalToString, StringToCardinal;

FROM FileOps IMPORT
    (* type *)  ChanId, FilenameString,
    (* proc *)  OpenNewFile1;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, Obtain, Release;

(************************************************************************)

CONST
    Nul = CHR(0);
    Tab = CHR(9);
    Space = ' ';

VAR
    (* String used in creating a unique file name. *)

    NextName: ARRAY [0..7] OF CHAR;
    NextNameLock: Lock;

(********************************************************************************)
(*                          STRING PROCESSING                                   *)
(********************************************************************************)

PROCEDURE HeadMatch (VAR (*IN*) buffer: ARRAY OF CHAR;
                                                    head: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff head is a leading substring of buffer, modulo   *)
    (* alphabetic case.                                                 *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        LOOP
            IF (j > HIGH(head)) OR (j > HIGH(buffer)) OR (head[j] = Nul) THEN
                RETURN TRUE;
            ELSIF CAP(buffer[j]) <> CAP(head[j]) THEN
                RETURN FALSE;
            ELSE
                INC (j);
            END (*IF*);
        END (*LOOP*);
    END HeadMatch;

(********************************************************************************)
(*                          PARSING AN E-MAIL ADDRESS                           *)
(********************************************************************************)

(********************************************************************************)
(* The syntax of an e-mail address is described in RFC2822 as follows.          *)
(* Exception: I am not allowing for obsolete syntax, because I can see no       *)
(* good reason for permitting it.                                               *)
(*

from            =       "From:" mailbox-list CRLF

sender          =       "Sender:" mailbox CRLF

reply-to        =       "Reply-To:" address-list CRLF

path            =       ([CFWS] "<" ([CFWS] / addr-spec) ">" [CFWS])

mailbox-list    =       (mailbox *("," mailbox))

address-list    =       (address *("," address))

address         =       mailbox / group

mailbox         =       name-addr / addr-spec

name-addr       =       [display-name] angle-addr

angle-addr      =       [CFWS] "<" addr-spec ">" [CFWS]

group           =       display-name ":" [mailbox-list / CFWS] ";"
                        [CFWS]

display-name    =       phrase

addr-spec       =       local-part "@" domain

local-part      =       dot-atom / quoted-string

domain          =       dot-atom / domain-literal

domain-literal  =       [CFWS] "[" *([FWS] dcontent) [FWS] "]" [CFWS]

dcontent        =       dtext / quoted-pair

dtext           =       NO-WS-CTL /     ; Non white space controls
                        %d33-90 /       ; The rest of the US-ASCII
                        %d94-126        ;  characters not including "[",
                                        ;  "]", or "\"

FWS             =       ([*WSP CRLF] 1*WSP)   ; Folding white space

ctext           =       NO-WS-CTL /     ; Non white space controls

                        %d33-39 /       ; The rest of the US-ASCII
                        %d42-91 /       ;  characters not including "(",
                        %d93-126        ;  ")", or "\"

ccontent        =       ctext / quoted-pair / comment

comment         =       "(" *([FWS] ccontent) [FWS] ")"

CFWS            =       *([FWS] comment) (([FWS] comment) / FWS)

NO-WS-CTL       =       %d1-8 /         ; US-ASCII control characters
                        %d11 /          ;  that do not include the
                        %d12 /          ;  carriage return, line feed,
                        %d14-31 /       ;  and white space characters
                        %d127

text            =       %d1-9 /         ; Characters excluding CR and LF
                        %d11 /
                        %d12 /
                        %d14-127

specials        =       "(" / ")" /     ; Special characters used in
                        "<" / ">" /     ;  other parts of the syntax
                        "[" / "]" /
                        ":" / ";" /
                        "@" / "\" /
                        "," / "." /
                        DQUOTE

==============================================================
For our present purposes we want to handle only single e-mail addresses. That
means that we can neglect groups and neglect lists of recipients. That simplifies
the syntax as follows.
==============================================================

from            =       "From:" mailbox CRLF

sender          =       "Sender:" mailbox CRLF

reply-to        =       "Reply-To:" mailbox CRLF

mailbox         =       name-addr / addr-spec

name-addr       =       [display-name] angle-addr

angle-addr      =       [CFWS] "<" addr-spec ">" [CFWS]

display-name    =       phrase

addr-spec       =       local-part "@" domain

local-part      =       dot-atom / quoted-string

domain          =       dot-atom / domain-literal

domain-literal  =       [CFWS] "[" *([FWS] dcontent) [FWS] "]" [CFWS]

dcontent        =       dtext / quoted-pair

dtext           =       NO-WS-CTL /     ; Non white space controls
                        %d33-90 /       ; The rest of the US-ASCII
                        %d94-126        ;  characters not including "[",
                                        ;  "]", or "\"

FWS             =       ([*WSP CRLF] 1*WSP)   ; Folding white space

ctext           =       NO-WS-CTL /     ; Non white space controls

                        %d33-39 /       ; The rest of the US-ASCII
                        %d42-91 /       ;  characters not including "(",
                        %d93-126        ;  ")", or "\"

ccontent        =       ctext / quoted-pair / comment

comment         =       "(" *([FWS] ccontent) [FWS] ")"

CFWS            =       *([FWS] comment) (([FWS] comment) / FWS)

NO-WS-CTL       =       %d1-8 /         ; US-ASCII control characters
                        %d11 /          ;  that do not include the
                        %d12 /          ;  carriage return, line feed,
                        %d14-31 /       ;  and white space characters
                        %d127

text            =       %d1-9 /         ; Characters excluding CR and LF
                        %d11 /
                        %d12 /
                        %d14-127

specials        =       "(" / ")" /     ; Special characters used in
                        "<" / ">" /     ;  other parts of the syntax
                        "[" / "]" /
                        ":" / ";" /
                        "@" / "\" /
                        "," / "." /
                        DQUOTE


*)
(********************************************************************************)

PROCEDURE LegalSyntax (str: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff str is a syntactically valid e-mail address according   *)
    (* to the rules of RFC2822.                                                 *)

    VAR valid: BOOLEAN;

    BEGIN
        valid := TRUE;
        RETURN valid;
    END LegalSyntax;

(********************************************************************************)

PROCEDURE ExtractEmailAddress (source: ARRAY OF CHAR;  skip: CARDINAL;
                                         VAR (*OUT*) displayname: ARRAY OF CHAR;
                                         VAR (*OUT*) result: ARRAY OF CHAR);

    (* Extracts an e-mail address from source, skipping the first 'skip' chars. *)

    VAR srcpos, pos1, pos2: CARDINAL;  found: BOOLEAN;

    (****************************************************************************)

    PROCEDURE SkipSpaces;

        (* Increments srcpos to move past space and tab characters. *)

        BEGIN
            WHILE (srcpos <= HIGH(source)) AND
                     ((source[srcpos] = Space) OR (source[srcpos] = Tab)) DO
                INC (srcpos);
            END (*WHILE*);
        END SkipSpaces;

    (****************************************************************************)

    BEGIN
        (* Skip leading spaces and any leading string in quotes. *)

        srcpos := skip;  pos2 := 0;
        SkipSpaces;
        IF (srcpos <= HIGH(source)) AND (source[srcpos] = '"') THEN
            INC (srcpos);
            REPEAT
                INC (srcpos);
            UNTIL (srcpos > HIGH(source)) OR (source[srcpos-1] = '"');
        END (*IF*);
        SkipSpaces;

        (* We recognise two kinds of address syntax.  If there are <> brackets  *)
        (* in the string, then the result is what's inside the brackets.        *)
        (* Otherwise, the address is the leading substring up to the first      *)
        (* space character.                                                     *)

        displayname[0] := Nul;
        Strings.FindNext ('<', source, srcpos, found, pos1);
        IF found THEN
            IF pos1 > srcpos THEN
                Strings.Extract (source, srcpos, pos1-srcpos, displayname);
            END (*IF*);
            INC (pos1);
            Strings.FindNext ('>', source, pos1, found, pos2);
        END (*IF*);

        IF NOT found THEN
            pos1 := srcpos;
            Strings.FindNext (' ', source, pos1, found, pos2);
            IF NOT found THEN
                pos2 := Strings.Length (source);
            END (*IF*);
        END (*IF*);

        IF pos2 > pos1 THEN
            Strings.Extract (source, pos1, pos2-pos1, result);
        ELSE
            result[0] := Nul;
        END (*IF*);

        (* Remove trailing whitespace from display name. *)

        pos2 := Strings.Length(displayname);
        IF pos2 > 0 THEN
            REPEAT
                DEC (pos2);
            UNTIL (pos2 = 0) OR ((displayname[pos2] <> Space)
                                 AND (displayname[pos2] <> Tab));
            displayname[pos2+1] := Nul;
        END (*IF*);

    END ExtractEmailAddress;

(************************************************************************)
(*                    NUMERIC/STRING CONVERSIONS                        *)
(************************************************************************)

PROCEDURE ConvertCard (number: CARDINAL;  VAR (*OUT*) result: ARRAY OF CHAR;
                                          VAR (*INOUT*) pos: CARDINAL);

    (* Converts number to decimal, left justified starting at result[pos].      *)
    (* On return pos is updated to the next unused array index.                 *)

    VAR j: CARDINAL;  buffer: ARRAY [0..15] OF CHAR;

    BEGIN
        CardinalToString (number, buffer, SIZE(buffer));
        j := 0;
        WHILE buffer[j] = ' ' DO INC(j);  END(*WHILE*);
        WHILE (pos <= HIGH(result)) AND (j < SIZE(buffer)) DO
            result[pos] := buffer[j];  INC(pos);  INC(j);
        END (*WHILE*);
    END ConvertCard;

(********************************************************************************)

PROCEDURE AppendCard (number: CARDINAL;  VAR (*INOUT*) string: ARRAY OF CHAR);

    (* Converts number to decimal, appends the result to the string. *)

    VAR pos: CARDINAL;

    BEGIN
        pos := LENGTH(string);
        ConvertCard (number, string, pos);
        IF pos <= HIGH (string) THEN
            string[pos] := Nul;
        END (*IF*);
    END AppendCard;

(********************************************************************************)

PROCEDURE WinSetDlgItemCard (hwnd: OS2.HWND;  idItem, value: CARDINAL);

    (* Sets a cardinal field in a dialogue. *)

    VAR Buffer: ARRAY [0..15] OF CHAR;  j: CARDINAL;

    BEGIN
        CardinalToString (value, Buffer, 15);
        Buffer[15] := Nul;
        j := 0;
        WHILE Buffer[j] = ' ' DO INC(j) END(*WHILE*);
        IF j > 0 THEN
            Strings.Delete (Buffer, 0, j);
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, idItem, Buffer);
    END WinSetDlgItemCard;

(**************************************************************************)

PROCEDURE WinQueryDlgItemCard (hwnd: OS2.HWND;  idItem: CARDINAL;
                                 VAR (*OUT*) result: CARDINAL);

    (* Reads back the value in a cardinal field in a dialogue. *)

    VAR Buffer: ARRAY [0..15] OF CHAR;

    BEGIN
        OS2.WinQueryDlgItemText (hwnd, idItem, 15, Buffer);
        result := StringToCardinal (Buffer);
    END WinQueryDlgItemCard;

(************************************************************************)
(*                    CREATING A UNIQUE FILENAME                        *)
(************************************************************************)

PROCEDURE MakeUniqueName (VAR (*OUT*) name: ARRAY OF CHAR);

    (* Generates a unique 8-character string.  The argument must of     *)
    (* course be big enough to take at least 8 characters.              *)

    (********************************************************************)

    PROCEDURE Increment (N: CARDINAL);

        (* Increments NextName[N], with carry as appropriate. *)

        BEGIN
            IF NextName[N] = '9' THEN
                NextName[N] := 'A';
            ELSIF NextName[N] = 'Z' THEN
                NextName[N] := '0';
                IF N > 0 THEN
                    Increment (N-1);
                END (*IF*);
            ELSE
                INC (NextName[N]);
            END (*IF*);
        END Increment;

    (********************************************************************)

    BEGIN
        Obtain (NextNameLock);
        Strings.Assign (NextName, name);
        Increment (7);
        Release (NextNameLock);
    END MakeUniqueName;

(************************************************************************)

PROCEDURE MakeNewFilename (BaseName, tail: ARRAY OF CHAR;
                       VAR (*OUT*) NewName: FilenameString);

    (* Creates a file name of the form BaseNamexxxtail, where xxx is    *)
    (* chosen such that a file of that name does not already exist.     *)
    (* Note that BaseName and tail can include punctuation.             *)

    VAR UName: ARRAY [0..7] OF CHAR;

    BEGIN
        REPEAT
            MakeUniqueName (UName);
            Strings.Assign (BaseName, NewName);
            Strings.Append (UName, NewName);
            Strings.Append (tail, NewName);
        UNTIL NOT FileSys.Exists(NewName);
    END MakeNewFilename;

(************************************************************************)

PROCEDURE OpenNewOutputFile (BaseName, tail: ARRAY OF CHAR;
                       VAR (*OUT*) NewName: FilenameString): ChanId;

    (* Creates a file name of the form BaseNamexxxtail, where xxx is    *)
    (* chosen such that a file of that name does not already exist, and *)
    (* opens that file.                                                 *)
    (* Note that BaseName and tail can include punctuation.             *)

    VAR cid: ChanId;  duplication: BOOLEAN;

    BEGIN
        REPEAT
            MakeNewFilename (BaseName, tail, NewName);
            cid := OpenNewFile1 (NewName, duplication);
        UNTIL NOT duplication;
        RETURN cid;
    END OpenNewOutputFile;

(************************************************************************)
(*                           INITIALISATION                             *)
(************************************************************************)

BEGIN
    NextName := "00000000";
    CreateLock (NextNameLock);
END Misc.

