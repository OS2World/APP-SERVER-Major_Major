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

IMPLEMENTATION MODULE PSDialogue;

        (********************************************************)
        (*                                                      *)
        (*            Admin program for MajorMajor              *)
        (* Dialogue for editing the SMTP authentication options *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            19 October 2000                 *)
        (*  Last edited:        5 April 2010                    *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT OS2, DID, Strings;

FROM SYSTEM IMPORT LOC, INT16;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM RINIData IMPORT
    (* proc *)  OpenINIFile, CloseINIFile, INIFetch, INIGetString,
                INIPut, INIPutString, INIDeleteKey;

FROM INIData IMPORT
    (* proc *)  SetInitialWindowPosition, StoreWindowPosition;

FROM Names IMPORT
    (* type *)  FilenameString;

(************************************************************************)

CONST
    Nul = CHR(0);  CR = CHR(13);  LF = CHR(10);

TYPE
    NameString = ARRAY [0..31] OF CHAR;

VAR
    (* INI file name and mode. *)

    INIFileName: FilenameString;
    UseTNI: BOOLEAN;

    (* Our window handle. *)

    hwnd: OS2.HWND;

    (* Authentication option: 0=none, 1=AUTH, 2=POP before SMTP. *)

    AuthOption: CARDINAL;

    (* The POP server's name, addresses, and port number. *)

    PSHostname: ARRAY [0..511] OF CHAR;
    PSPort: CARDINAL;

    (* Username and password for logging in to the server. *)

    PSUsername, PSPassword: NameString;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        StrToBuffer (lang, "PSDialogue.title", stringval);
        OS2.WinSetWindowText (hwnd, stringval);
        StrToBuffer (lang, "PSDialogue.AuthMethod", stringval);
        OS2.WinSetDlgItemText (hwnd, DID.MethodTitle, stringval);
        StrToBuffer (lang, "PSDialogue.none", stringval);
        OS2.WinSetDlgItemText (hwnd, DID.AuthNone, stringval);
        StrToBuffer (lang, "PSDialogue.SMTPAUTH", stringval);
        OS2.WinSetDlgItemText (hwnd, DID.AuthSMTPAUTH, stringval);
        StrToBuffer (lang, "PSDialogue.POPbeforeSMTP", stringval);
        OS2.WinSetDlgItemText (hwnd, DID.AuthPOPbeforeSMTP, stringval);
        StrToBuffer (lang, "PSDialogue.username", stringval);
        OS2.WinSetDlgItemText (hwnd, DID.PSUsernameLabel, stringval);
        StrToBuffer (lang, "PSDialogue.password", stringval);
        OS2.WinSetDlgItemText (hwnd, DID.PSPasswordLabel, stringval);
        StrToBuffer (lang, "PSDialogue.portlabel1", stringval);
        OS2.WinSetDlgItemText (hwnd, DID.PSPortLabel1, stringval);
        StrToBuffer (lang, "PSDialogue.portlabel2", stringval);
        OS2.WinSetDlgItemText (hwnd, DID.PSPortLabel2, stringval);
        StrToBuffer (lang, "PSDialogue.hostname", stringval);
        OS2.WinSetDlgItemText (hwnd, DID.PSHostnameLabel, stringval);
    END SetLanguage;

(************************************************************************)
(*                      DEALING WITH INI FILE DATA                      *)
(************************************************************************)

PROCEDURE LoadINIData (hwnd: OS2.HWND);

    (* Loads the POP-before-SMTP parameters from the INI file. *)

    (********************************************************************)

    PROCEDURE GetItem (name: ARRAY OF CHAR;
                            VAR (*OUT*) variable: ARRAY OF LOC): BOOLEAN;

        BEGIN
            RETURN INIFetch ("$SYS", name, variable);
        END GetItem;

    (********************************************************************)

    PROCEDURE GetString (name: ARRAY OF CHAR;
                            VAR (*OUT*) variable: ARRAY OF CHAR): BOOLEAN;

        BEGIN
            RETURN INIGetString ("$SYS", name, variable);
        END GetString;

    (********************************************************************)

    VAR UsePOPbeforeSMTP, opened: BOOLEAN;

    BEGIN
        AuthOption := 0;
        PSPort := 110;
        PSUsername := "";
        PSPassword := "";
        PSHostname := "";

        opened := OpenINIFile (INIFileName, UseTNI);

        IF NOT (opened AND GetItem ("AuthOption", AuthOption)) THEN

            (* Check for obsolete option. *)

            IF opened AND GetItem ("POPbeforeSMTP", UsePOPbeforeSMTP) THEN
                AuthOption := 2;
                INIDeleteKey ("$SYS", "POPbeforeSMTP");
            ELSE
                AuthOption := 0;
            END (*IF*);
        END (*IF*);

        IF AuthOption = 0 THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.PSUsername), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.PSPassword), FALSE);
        ELSE
            OS2.WinSendDlgItemMsg (hwnd, DID.EnableSMTPAuthentication, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(1), NIL);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.PSUsername), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.PSPassword), TRUE);
        END (*IF*);

        IF AuthOption = 2 THEN
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.PSHostnameLabel), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.PSHostname), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.PSPortLabel1), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.PSPort), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.PSPortLabel2), TRUE);
        ELSE
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.PSHostnameLabel), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.PSHostname), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.PSPortLabel1), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.PSPort), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.PSPortLabel2), FALSE);
        END (*IF*);

        IF AuthOption = 0 THEN
            OS2.WinSendDlgItemMsg (hwnd, DID.AuthNone, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(1), NIL);
        ELSIF AuthOption = 1 THEN
            OS2.WinSendDlgItemMsg (hwnd, DID.AuthSMTPAUTH, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(1), NIL);
        ELSE
            AuthOption := 2;
            OS2.WinSendDlgItemMsg (hwnd, DID.AuthPOPbeforeSMTP, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(1), NIL);
        END (*IF*);

        IF NOT (opened AND GetItem ("Port", PSPort)) THEN
            EVAL (INIFetch ('$SYS', 'POPport', PSPort));
        END (*IF*);
        IF NOT GetString ("PSUsername", PSUsername) THEN
            EVAL (INIGetString ('$SYS', 'AdminName', PSUsername));
        END (*IF*);
        IF NOT GetString ("PSPassword", PSPassword) THEN
            EVAL (INIGetString ('$SYS', 'AdminPassword', PSPassword));
        END (*IF*);
        IF NOT GetString ("PSHostname", PSHostname) THEN
            EVAL (INIGetString ('$SYS', 'POPServer', PSHostname));
        END (*IF*);

        IF opened THEN
            CloseINIFile;
        END (*IF*);

        OS2.WinSetDlgItemText (hwnd, DID.PSHostname, PSHostname);
        OS2.WinSetDlgItemShort (hwnd, DID.PSPort, PSPort, FALSE);
        OS2.WinSetDlgItemText (hwnd, DID.PSUsername, PSUsername);
        OS2.WinSetDlgItemText (hwnd, DID.PSPassword, PSPassword);

    END LoadINIData;

(************************************************************************)

PROCEDURE StoreData (hwnd: OS2.HWND);

    (* Updates our global variables from the dialogue, also stores      *)
    (* the updated values in our INI file.                              *)

    VAR temp: INT16;

    BEGIN
        IF OpenINIFile (INIFileName, UseTNI) THEN

            INIPut ("$SYS", "AuthOption", AuthOption);

            OS2.WinQueryDlgItemText (hwnd, DID.PSHostname, 512, PSHostname);
            INIPutString ("$SYS", "PSHostname", PSHostname);

            OS2.WinQueryDlgItemShort (hwnd, DID.PSPort, temp, FALSE);
            PSPort := temp;
            INIPut ("$SYS", "PSPort", PSPort);

            OS2.WinQueryDlgItemText (hwnd, DID.PSUsername, 32, PSUsername);
            INIPutString ("$SYS", "PSUsername", PSUsername);

            OS2.WinQueryDlgItemText (hwnd, DID.PSPassword, 32, PSPassword);
            INIPutString ("$SYS", "PSPassword", PSPassword);

            CloseINIFile;
        END (*IF*);

    END StoreData;

(************************************************************************)
(*                       THE SETUP DIALOGUE                             *)
(************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    (* Message handler for the setup dialogue. *)

    VAR ButtonID, NotificationCode: CARDINAL;

    BEGIN
        CASE msg OF
           |  OS2.WM_INITDLG:
                   LoadINIData (hwnd);
                   RETURN NIL;

           |  OS2.WM_CONTROL:

                   NotificationCode := OS2.ULONGFROMMP(mp1);
                   ButtonID := NotificationCode MOD 65536;
                   NotificationCode := NotificationCode DIV 65536;
                   IF (NotificationCode = OS2.BN_CLICKED) AND
                            (OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, ButtonID,
                                                OS2.BM_QUERYCHECK, NIL, NIL)) > 0) THEN
                       CASE ButtonID OF
                         | DID.AuthDummy:
                              (* no action needed *)
                              RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                         | DID.AuthNone:
                              AuthOption := 0;
                         | DID.AuthSMTPAUTH:
                              AuthOption := 1;
                         | DID.AuthPOPbeforeSMTP:
                              AuthOption := 2;
                         | ELSE
                              RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                       END (*CASE*);

                       IF AuthOption = 0 THEN
                           OS2.WinSendDlgItemMsg (hwnd, DID.EnableSMTPAuthentication, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(0), NIL);
                           OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.PSUsername), FALSE);
                           OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.PSPassword), FALSE);
                       ELSE
                           OS2.WinSendDlgItemMsg (hwnd, DID.EnableSMTPAuthentication, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(1), NIL);
                           OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.PSUsername), TRUE);
                           OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.PSPassword), TRUE);
                       END (*IF*);

                       IF AuthOption = 2 THEN
                           OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.PSHostnameLabel), TRUE);
                           OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.PSHostname), TRUE);
                           OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.PSPortLabel1), TRUE);
                           OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.PSPort), TRUE);
                           OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.PSPortLabel2), TRUE);
                       ELSE
                           OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.PSHostnameLabel), FALSE);
                           OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.PSHostname), FALSE);
                           OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.PSPortLabel1), FALSE);
                           OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.PSPort), FALSE);
                           OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.PSPortLabel2), FALSE);
                       END (*IF*);

                       RETURN NIL;

                   ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*IF*);

           |  OS2.WM_COMMAND:

                   IF OS2.SHORT1FROMMP(mp1) = DID.PSDone THEN
                       StoreData (hwnd);
                   END (*IF*);
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

           |  OS2.WM_CLOSE:
                   StoreData (hwnd);
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

        ELSE    (* default *)
           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);

    END DialogueProc;

(************************************************************************)

PROCEDURE Edit (owner: OS2.HWND;  lang: LangHandle);

    (* Runs the dialogue that edits the SMTP authentication options. *)

    BEGIN
        hwnd := OS2.WinLoadDlg(OS2.HWND_DESKTOP, owner,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.PSDialogue,                (* dialogue ID *)
                       NIL);                 (* creation parameters *)

        SetInitialWindowPosition (hwnd, "ADMIN.INI",
                                  "PSDialogue", UseTNI);
        SetLanguage (lang);
        OS2.WinShowWindow (hwnd, TRUE);

        OS2.WinProcessDlg(hwnd);
        StoreWindowPosition (hwnd, "ADMIN.INI", "PSDialogue", UseTNI);
        OS2.WinDestroyWindow (hwnd);
    END Edit;

(************************************************************************)

PROCEDURE SetINIFileName (name: ARRAY OF CHAR;  TNImode: BOOLEAN);

    (* Sets the INI file name and mode. *)

    BEGIN
        Strings.Assign (name, INIFileName);
        UseTNI := TNImode;
    END SetINIFileName;

(**************************************************************************)

END PSDialogue.

