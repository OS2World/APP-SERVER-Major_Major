(**************************************************************************)
(*                                                                        *)
(*  Admin program for the Major Major mailing list manager                *)
(*  Copyright (C) 2019   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE AdminDialogue;

        (****************************************************************)
        (*                                                              *)
        (*                Admin program for MajorMajor                  *)
        (*                     The Admin dialogue                       *)
        (*                                                              *)
        (*    Started:        22 June 2000                              *)
        (*    Last edited:    28 September 2019                         *)
        (*    Status:         Working                                   *)
        (*                                                              *)
        (****************************************************************)


IMPORT OS2, DID, Strings, CommonSettings, Page1, ELpage1;

FROM SYSTEM IMPORT
    (* type *)  ADDRESS, CARD16,
    (* proc *)  CAST, ADR;

FROM RINIData IMPORT
    (* proc *)  OpenINIFile, CloseINIFile, INIFetch, INIPut,
                INIGetString, INIPutString;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM MiscFuncs IMPORT
    (* proc *)  EVAL;

FROM Names IMPORT
    (* type *)  FilenameString;

(************************************************************************)

CONST
    Nul = CHR(0);
    EmailAddressSize = 256;
    PasswordSize = 32;
    FilenameLength = OS2.CCHMAXPATHCOMP;

TYPE
    EmailAddress = ARRAY [0..EmailAddressSize-1] OF CHAR;

VAR
    INIFileName: FilenameString;

    OurPageID: CARDINAL;
    pagehandle, hwndParent: OS2.HWND;
    ChangeInProgress: BOOLEAN;

    OldReplyOption: BOOLEAN;
    OldAdminName, OldAdminLoginName: EmailAddress;
    OldAdminPassword, OldAdminLanguage, OldDefaultLanguage:
                                       ARRAY [0..PasswordSize-1] OF CHAR;
    OldAdminFilter: ARRAY [0..FilenameLength-1] OF CHAR;
    OldMIMEcharset: ARRAY [0..64] OF CHAR;
    OldAdminCheckInterval: CARDINAL;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        StrToBuffer (lang, "AdminPage.tab", stringval);
        OS2.WinSendMsg (OS2.WinWindowFromID(hwndParent,DID.notebook), OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,OurPageID), ADR(stringval));
        StrToBuffer (lang, "AdminPage.account", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.AdminAccountLabel, stringval);
        StrToBuffer (lang, "AdminPage.username", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.AdminUsernameLabel, stringval);
        StrToBuffer (lang, "AdminPage.loginname", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.AdminLoginnameLabel, stringval);
        StrToBuffer (lang, "AdminPage.password", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.AdminPasswordLabel, stringval);
        StrToBuffer (lang, "AdminPage.errorresponse", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.AdminErrorResponse, stringval);
        StrToBuffer (lang, "ELpage1.filter", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.AdminFilterLabel, stringval);
        StrToBuffer (lang, "ELopt2.accept", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.AdminReply, stringval);
        StrToBuffer (lang, "ELopt2.ignore", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.AdminIgnore, stringval);
        StrToBuffer (lang, "AdminPage.timebetween", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.AdminTimeLabel, stringval);
        StrToBuffer (lang, "AdminPage.hours", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.AdminHourLabel, stringval);
        StrToBuffer (lang, "AdminPage.minutes", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.AdminMinuteLabel, stringval);
        StrToBuffer (lang, "AdminPage.seconds", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.AdminSecondLabel, stringval);
        StrToBuffer (lang, "AdminPage.AdminLang", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.AdminLangLabel, stringval);
        StrToBuffer (lang, "AdminPage.DefaultLang", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.AdminDefaultLangLabel, stringval);
        StrToBuffer (lang, "AdminPage.MIMEcharset", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.AdminMIMELabel, stringval);
    END SetLanguage;

(************************************************************************)
(*                   OPERATIONS ON DIALOGUE ELEMENTS                    *)
(************************************************************************)

PROCEDURE EnablePOP3Fields (hwnd: OS2.HWND);

    (* Enables/disables some of the fields depending on whether the     *)
    (* "Weasel on this machine" option is active.                       *)

    (* NB This procedure disabled for now because I don't seem to be    *)
    (* able to capture the "page selected" event.                       *)

    BEGIN
        (*
        IF Page1.WeaselOnThisMachine() THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd,
                                             DID.AdminLoginName), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd,
                                             DID.AdminPassword), FALSE);
        ELSE
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd,
                                             DID.AdminLoginName), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd,
                                             DID.AdminPassword), TRUE);
        END (*IF*);
        *)
    END EnablePOP3Fields;

(************************************************************************)
(*                     LOADING DATA FROM THE INI FILE                   *)
(************************************************************************)

PROCEDURE LoadValues (hwnd: OS2.HWND);

    (* Fills the dialogue elements on this page with data from the INI file,    *)
    (* or loads default values if they're not in the INI file.                  *)

    VAR val, Interval: CARDINAL;  NewLang: LangHandle;
        stringval: EmailAddress;

    BEGIN
        EVAL (OpenINIFile(INIFileName));

        (* Administrator name. *)

        IF INIGetString ('$SYS', 'AdminName', stringval) THEN
            OldAdminName := stringval;
        ELSE
            stringval := "MajorMajor";
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.AdminName, stringval);

        OldAdminLoginName := stringval;
        EVAL (INIGetString ('$SYS', 'AdminLoginName', OldAdminLoginName));
        OS2.WinSetDlgItemText (hwnd, DID.AdminLoginName, OldAdminLoginName);

        (* Administrator password. *)

        IF INIGetString ('$SYS', 'AdminPassword', stringval) THEN
            Strings.Assign (stringval, OldAdminPassword);
        ELSE
            stringval := "";
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.AdminPassword, stringval);

        (* Handling of faulty command input. *)

        IF NOT INIFetch ('$SYS', "ReplyToErrors", OldReplyOption) THEN
            OldReplyOption := TRUE;
        END (*IF*);
        IF OldReplyOption THEN
            OS2.WinSendDlgItemMsg (hwnd, DID.AdminReply, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(TRUE)), NIL);
        ELSE
            OS2.WinSendDlgItemMsg (hwnd, DID.AdminIgnore, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(TRUE)), NIL);
        END (*IF*);

        (* Admin Filter. *)

        IF INIGetString ('$SYS', 'AdminFilter', stringval) THEN
            Strings.Assign (stringval, OldAdminFilter);
        ELSE
            stringval := "";
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.AdminFilter, stringval);

        (* Check interval. *)

        IF INIFetch ('$SYS', 'AdminCheckInterval', val) THEN
            OldAdminCheckInterval := val;
        ELSE
            val := 30*60;
        END (*IF*);
        Interval := val;

        (* Set the values on the spin buttons. *)

        OS2.WinSendDlgItemMsg (hwnd, DID.AdminSpinSS, OS2.SPBM_SETLIMITS,
                     OS2.MPFROMSHORT(59), OS2.MPFROMSHORT(0));
        OS2.WinSendDlgItemMsg (hwnd, DID.AdminSpinSS, OS2.SPBM_SETCURRENTVALUE,
                     OS2.MPFROMSHORT(Interval MOD 60), NIL);
        Interval := Interval DIV 60;
        OS2.WinSendDlgItemMsg (hwnd, DID.AdminSpinMM, OS2.SPBM_SETLIMITS,
                     OS2.MPFROMSHORT(59), OS2.MPFROMSHORT(0));
        OS2.WinSendDlgItemMsg (hwnd, DID.AdminSpinMM, OS2.SPBM_SETCURRENTVALUE,
                     OS2.MPFROMSHORT(Interval MOD 60), NIL);
        Interval := Interval DIV 60;
        IF Interval > 99 THEN
            Interval := 99;
        END (*IF*);
        OS2.WinSetDlgItemShort (hwnd, DID.AdminSpinHH, Interval, FALSE);
        OS2.WinSendDlgItemMsg (hwnd, DID.AdminSpinHH, OS2.SPBM_SETLIMITS,
                     OS2.MPFROMSHORT(99), OS2.MPFROMSHORT(0));
        OS2.WinSendDlgItemMsg (hwnd, DID.AdminSpinHH, OS2.SPBM_SETCURRENTVALUE,
                     OS2.MPFROMSHORT(Interval), NIL);

        (* Default language. *)

        IF INIGetString ('$SYS', 'DefaultLanguage', stringval) THEN
            Strings.Assign (stringval, OldDefaultLanguage);
        ELSE
            stringval := "en";
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.DefaultLanguage, stringval);
        ELpage1.SetDefaultLanguage (stringval);

        (* Admin language. *)

        IF INIGetString ('$SYS', 'AdminLanguage', stringval) THEN
            Strings.Assign (stringval, OldAdminLanguage);
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.AdminLanguage, stringval);
        IF CommonSettings.ChangeLanguageTo(stringval) THEN
            CommonSettings.CurrentLanguage (NewLang, stringval);
            SetLanguage (NewLang);
        END (*IF*);

        (* Admin MIME character set. *)

        IF INIGetString ('$SYS', 'MIMEcharset', stringval) THEN
            Strings.Assign (stringval, OldMIMEcharset);
        ELSE
            stringval := "iso-8859-1";
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.MIMEcharset, stringval);

        CloseINIFile;
        EnablePOP3Fields (hwnd);

    END LoadValues;

(************************************************************************)
(*                      STORING DATA TO THE INI FILE                    *)
(************************************************************************)

PROCEDURE SpaceStrip (VAR (*INOUT*) stringval: ARRAY OF CHAR);

    VAR j: CARDINAL;

    BEGIN
        WHILE stringval[0] = ' ' DO
            Strings.Delete (stringval, 0, 1);
        END (*WHILE*);
        j := Strings.Length (stringval);
        LOOP
            IF j = 0 THEN EXIT(*LOOP*) END(*IF*);
            DEC(j);
            IF stringval[j] = ' ' THEN
                stringval[j] := Nul;
            ELSE
                EXIT (*LOOP*);
            END(*IF*);
        END (*LOOP*);
    END SpaceStrip;

(************************************************************************)

PROCEDURE QueryButton (hwnd: OS2.HWND;  B: CARDINAL): CARDINAL;

    BEGIN
        RETURN OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, B,
                                              OS2.BM_QUERYCHECK, NIL, NIL));
    END QueryButton;

(************************************************************************)

PROCEDURE StoreData (hwnd: OS2.HWND);

    (* Stores the values on the admin data page back into the INI file.  *)

    VAR interval, N: CARDINAL;  ReplyOption: BOOLEAN;
        stringval: ARRAY [0..514] OF CHAR;

    BEGIN
        EVAL (OpenINIFile (INIFileName));

        (* Administrator name.  We strip leading and trailing spaces.  *)

        OS2.WinQueryDlgItemText (hwnd, DID.AdminName, EmailAddressSize, stringval);
        SpaceStrip (stringval);
        IF NOT Strings.Equal (stringval, OldAdminName) THEN
            INIPutString ('$SYS', 'AdminName', stringval);
        END (*IF*);

        (* Administrator POP3 login name. *)

        OS2.WinQueryDlgItemText (hwnd, DID.AdminLoginName, EmailAddressSize, stringval);
        SpaceStrip (stringval);
        IF NOT Strings.Equal (stringval, OldAdminLoginName) THEN
            INIPutString ('$SYS', 'AdminLoginName', stringval);
        END (*IF*);

        (* Administrator password. *)

        OS2.WinQueryDlgItemText (hwnd, DID.AdminPassword, PasswordSize+1, stringval);
        IF NOT Strings.Equal (stringval, OldAdminPassword) THEN
            INIPutString ('$SYS', 'AdminPassword', stringval);
        END (*IF*);

        (* Handling of faulty command input. *)

        ReplyOption := QueryButton (hwnd, DID.AdminReply) > 0;
        IF ReplyOption <> OldReplyOption THEN
            INIPut ('$SYS', "ReplyToErrors", ReplyOption);
        END (*IF*);

        (* Admin Filter. *)

        OS2.WinQueryDlgItemText (hwnd, DID.AdminFilter, FilenameLength+1, stringval);
        IF NOT Strings.Equal (stringval, OldAdminFilter) THEN
            INIPutString ('$SYS', 'AdminFilter', stringval);
        END (*IF*);

        (* Check Interval. *)

        interval := 0;  N := 0;
        OS2.WinSendDlgItemMsg (hwnd, DID.AdminSpinHH, OS2.SPBM_QUERYVALUE,
                               ADR(interval), OS2.MPFROM2SHORT(0,OS2.SPBQ_DONOTUPDATE));
        OS2.WinSendDlgItemMsg (hwnd, DID.AdminSpinMM, OS2.SPBM_QUERYVALUE,
                               ADR(N), OS2.MPFROM2SHORT(0,OS2.SPBQ_DONOTUPDATE));
        interval := 60*interval + N;
        OS2.WinSendDlgItemMsg (hwnd, DID.AdminSpinSS, OS2.SPBM_QUERYVALUE,
                               ADR(N), OS2.MPFROM2SHORT(0,OS2.SPBQ_DONOTUPDATE));
        interval := 60*interval + N;

        IF interval <> OldAdminCheckInterval THEN
            INIPut ('$SYS', 'AdminCheckInterval', interval);
        END (*IF*);

        (* Default Language. *)

        OS2.WinQueryDlgItemText (hwnd, DID.DefaultLanguage, PasswordSize+1, stringval);
        IF NOT Strings.Equal (stringval, OldDefaultLanguage) THEN
            INIPutString ('$SYS', 'DefaultLanguage', stringval);
        END (*IF*);

        (* Admin language. *)

        OS2.WinQueryDlgItemText (hwnd, DID.AdminLanguage, PasswordSize+1, stringval);
        IF NOT Strings.Equal (stringval, OldAdminLanguage) THEN
            INIPutString ('$SYS', 'AdminLanguage', stringval);
        END (*IF*);

        (* Admin MIME character set. *)

        OS2.WinQueryDlgItemText (hwnd, DID.MIMEcharset, PasswordSize+1, stringval);
        IF NOT Strings.Equal (stringval, OldMIMEcharset) THEN
            INIPutString ('$SYS', 'MIMEcharset', stringval);
        END (*IF*);

        CloseINIFile;

    END StoreData;

(**************************************************************************)
(*                      THE DIALOGUE PROCEDURE                            *)
(**************************************************************************)

PROCEDURE ["SysCall"] DialogueProc (hwnd: OS2.HWND;  msg: OS2.ULONG;
                                      mp1, mp2: OS2.MPARAM): OS2.MRESULT;

    VAR id, code: OS2.USHORT;
        value: CARDINAL;  NewLang: LangHandle;
        langval: ARRAY [0..PasswordSize] OF CHAR;

    BEGIN
        CASE msg OF
           | OS2.WM_INITDLG:

                 pagehandle := hwnd;
                 LoadValues (hwnd);
                 RETURN NIL;

           | OS2.WM_PRESPARAMCHANGED:

                 IF ChangeInProgress THEN
                     RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                 ELSE
                     ChangeInProgress := TRUE;
                     CommonSettings.UpdateFontFrom (hwnd, CommonSettings.MainNotebook);
                     ChangeInProgress := FALSE;
            (*RETURN OS2.WinDefDlgProc (hwnd, msg, mp1, mp2);*)
                     RETURN NIL;
                 END (*IF*);

           | OS2.WM_CONTROL:

                 id := OS2.USHORT1FROMMP (mp1);
                 code := OS2.USHORT2FROMMP (mp1);
                 value := OS2.ULONGFROMMP(mp2);
                 IF (id = DID.DefaultLanguage) AND (code = OS2.EN_CHANGE) THEN
                     OS2.WinQueryDlgItemText (hwnd, DID.DefaultLanguage,
                                              PasswordSize+1, langval);
                     ELpage1.SetDefaultLanguage (langval);
                 ELSIF (id = DID.AdminLanguage) AND (code = OS2.EN_CHANGE) THEN
                     OS2.WinQueryDlgItemText (hwnd, DID.AdminLanguage,
                                              PasswordSize+1, langval);
                     IF CommonSettings.ChangeLanguageTo(langval) THEN
                         CommonSettings.CurrentLanguage (NewLang, langval);
                         SetLanguage (NewLang);
                         OS2.WinPostMsg (hwndParent, CommonSettings.LANGCHANGED,
                                OS2.MPFROMLONG(1), OS2.MPFROMLONG(0));
                     END (*IF*);
                 ELSIF code = OS2.BKN_PAGESELECTED THEN
                     EnablePOP3Fields (hwnd);
                     RETURN NIL;
                 END (*IF*);

        ELSE    (* default *)
            RETURN OS2.WinDefDlgProc (hwnd, msg, mp1, mp2);
        END (*CASE*);

        (* Catch-all for cases not covered above. *)

        RETURN OS2.WinDefDlgProc (hwnd, msg, mp1, mp2);

    END DialogueProc;

(************************************************************************)

PROCEDURE Create (notebook: OS2.HWND;  VAR (*OUT*) PageID: CARDINAL): OS2.HWND;

    (* Creates the dialogue box. *)

    VAR Label: ARRAY [0..31] OF CHAR;

    BEGIN
        hwndParent := OS2.WinQueryWindow (notebook, OS2.QW_PARENT);
        pagehandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,          (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.AdminDialogue,            (* dialogue ID *)
                       NIL);                 (* creation parameters *)

        PageID := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         NIL, OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE, OS2.BKA_LAST)));
        OurPageID := PageID;
        Label := "Admin";
        OS2.WinSendMsg (notebook, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(Label));
        OS2.WinSendMsg (notebook, OS2.BKM_SETPAGEWINDOWHWND,
                        CAST(ADDRESS,PageID), CAST(ADDRESS,pagehandle));
        OS2.WinSetWindowPos (pagehandle, 0, 0, 0, 0, 0, OS2.SWP_MOVE);
        RETURN pagehandle;
    END Create;

(**************************************************************************)

PROCEDURE SetFont (VAR (*IN*) name: CommonSettings.FontName);

    (* Sets the font of the text on this page. *)

    CONST bufsize = CommonSettings.FontNameSize;

    BEGIN
        OS2.WinSetPresParam (pagehandle, OS2.PP_FONTNAMESIZE, bufsize, name);
    END SetFont;

(**************************************************************************)

PROCEDURE SetINIFileName (name: ARRAY OF CHAR);

    (* Sets the INI file name and mode. *)

    BEGIN
        Strings.Assign (name, INIFileName);
    END SetINIFileName;

(**************************************************************************)

BEGIN
    INIFileName := "";
    OldAdminName := "";
    OldAdminLoginName := "";
    OldDefaultLanguage := "";
    OldAdminLanguage := "";
    OldAdminFilter := "";
    OldMIMEcharset := "";
    OldAdminCheckInterval := 0;
    ChangeInProgress := FALSE;
END AdminDialogue.

