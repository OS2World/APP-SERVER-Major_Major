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

IMPLEMENTATION MODULE ELpage1;

        (************************************************************)
        (*                                                          *)
        (*              Admin program for MajorMajor                *)
        (*           Page 1 of the "edit list" notebook             *)
        (*                                                          *)
        (*    Started:        18 June 2000                          *)
        (*    Last edited:    8 February 2009                       *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)


FROM SYSTEM IMPORT
    (* type *)  CARD8, ADDRESS,
    (* proc *)  CAST, ADR;

IMPORT OS2, DID, Strings, INIData, RINIData, OneLine, CommonSettings;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM Names IMPORT
    (* type *)  FilenameString;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);
    NameLength = 256;
    AbbreviationLength = 32;
    FontNameSize = 256;

TYPE
    Username = ARRAY [0..NameLength-1] OF CHAR;
    AbbreviationType = ARRAY [0..AbbreviationLength] OF CHAR;
    FontName = ARRAY [0..FontNameSize-1] OF CHAR;

VAR
    INIFileName: FilenameString;
    UseTNI: BOOLEAN;

    pagehandle, notebookhandle: OS2.HWND;
    PageID: CARDINAL;
    ListName, OldLoginName: Username;
    OldPassword, OldLanguage, DefaultLanguage: ARRAY [0..32] OF CHAR;
    OldAbbreviation: AbbreviationType;
    OldInterval: CARDINAL;
    OldErrorMessRecipient: Username;
    OldFilterProg: FilenameString;
    ChangeInProgress: BOOLEAN;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        StrToBuffer (lang, "Page1.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(stringval));
        StrToBuffer (lang, "ELpage1.LoginName", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.LoginNameLabel, stringval);
        StrToBuffer (lang, "ELpage1.password", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.ELpage1PasswordLabel, stringval);
        StrToBuffer (lang, "ELpage1.abbrev", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.ELpage1AbbrevLabel, stringval);
        StrToBuffer (lang, "ELpage1.checkinterval", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.ELpage1IntervalLabel, stringval);
        StrToBuffer (lang, "ELpage1.errormess", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.ELpage1ErrmesLabel, stringval);
        StrToBuffer (lang, "ELpage1.discard", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.ErrorsIgnore, stringval);
        StrToBuffer (lang, "ELpage1.mailto", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.ErrorsMailTo, stringval);
        StrToBuffer (lang, "ELpage1.filter", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.ELpage1FilterLabel, stringval);
        StrToBuffer (lang, "ELpage1.language", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.ELpage1LangLabel, stringval);
    END SetLanguage;

(**************************************************************************)
(*                        DIALOGUE PROCEDURES                             *)
(**************************************************************************)

PROCEDURE LoadListData (hwnd: OS2.HWND);

    (* Fills the dialogue windows with data from the INI file, or       *)
    (* loads default values if they're not in the INI file.             *)

    VAR Interval: CARDINAL;
        flag, opened: BOOLEAN;
        strval: ARRAY [0..63] OF CHAR;

    BEGIN
        opened := RINIData.OpenINIFile(INIFileName, UseTNI);

        (* Login name. *)

        IF NOT (opened AND RINIData.INIGetString (ListName, "LoginName", OldLoginName)) THEN
            Strings.Assign (ListName, OldLoginName);
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.LoginName, OldLoginName);

        (* POP3 password. *)

        IF NOT (opened AND RINIData.INIGetString (ListName, "Password", OldPassword)) THEN
            Strings.Assign ("", OldPassword);
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.POP3password, OldPassword);

        (* Abbreviated list name. *)

        IF opened AND RINIData.INIGetString (ListName, "Abbreviation", OldAbbreviation) THEN
            OS2.WinSetDlgItemText (hwnd, DID.ListAbbreviation, OldAbbreviation);
        ELSE
            Strings.Assign (ListName, OldAbbreviation);
            OS2.WinSetDlgItemText (hwnd, DID.ListAbbreviation, OldAbbreviation);
            OldAbbreviation[0] := Nul;
        END (*IF*);

        (* List update interval. *)

        IF opened AND RINIData.INIFetch (ListName, "interval", Interval) THEN
            OldInterval := Interval;
        ELSE
            Interval := 10*60;
            OldInterval := 0;
        END (*IF*);

        OS2.WinSendDlgItemMsg (hwnd, DID.CheckTimeS, OS2.SPBM_SETLIMITS,
                     OS2.MPFROMSHORT(59), OS2.MPFROMSHORT(0));
        OS2.WinSendDlgItemMsg (hwnd, DID.CheckTimeS, OS2.SPBM_SETCURRENTVALUE,
                     OS2.MPFROMSHORT(Interval MOD 60), NIL);
        Interval := Interval DIV 60;
        OS2.WinSendDlgItemMsg (hwnd, DID.CheckTimeM, OS2.SPBM_SETLIMITS,
                     OS2.MPFROMSHORT(59), OS2.MPFROMSHORT(0));
        OS2.WinSendDlgItemMsg (hwnd, DID.CheckTimeM, OS2.SPBM_SETCURRENTVALUE,
                     OS2.MPFROMSHORT(Interval MOD 60), NIL);
        Interval := Interval DIV 60;
        IF Interval > 99 THEN
            Interval := 99;
        END (*IF*);
        OS2.WinSetDlgItemShort (hwnd, DID.CheckTimeH, Interval, FALSE);
        OS2.WinSendDlgItemMsg (hwnd, DID.CheckTimeH, OS2.SPBM_SETLIMITS,
                     OS2.MPFROMSHORT(99), OS2.MPFROMSHORT(0));
        OS2.WinSendDlgItemMsg (hwnd, DID.CheckTimeH, OS2.SPBM_SETCURRENTVALUE,
                     OS2.MPFROMSHORT(Interval), NIL);

        (* Place to mail error messages. *)

        IF NOT (opened AND RINIData.INIGetString (ListName, "MailErrorsTo", OldErrorMessRecipient)) THEN
            OldErrorMessRecipient[0] := Nul;
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.ErrorMessageRecipient, OldErrorMessRecipient);

        flag := OldErrorMessRecipient[0] <> Nul;
        OS2.WinSendDlgItemMsg (hwnd, DID.ErrorsMailTo, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(flag)), NIL);
        OS2.WinSendDlgItemMsg (hwnd, DID.ErrorsIgnore, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(1-ORD(flag)), NIL);
        OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.ErrorMessageRecipient), flag);

        (* Filter. *)

        IF NOT (opened AND RINIData.INIGetString (ListName, "FilterProg", OldFilterProg)) THEN
            Strings.Assign ("", OldFilterProg);
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.FilterProg, OldFilterProg);

        (* Language. *)

        IF opened AND RINIData.INIGetString (ListName, "language", strval) THEN
            Strings.Assign (strval, OldLanguage);
        ELSE
            Strings.Assign ("", OldLanguage);
            Strings.Assign (DefaultLanguage, strval);
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.ListLanguage, strval);

        IF opened THEN
            RINIData.CloseINIFile;
        END (*IF*);

    END LoadListData;

(************************************************************************)

PROCEDURE QueryButton (hwnd: OS2.HWND;  B: CARDINAL): CARDINAL;

    BEGIN
        RETURN OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, B,
                                              OS2.BM_QUERYCHECK, NIL, NIL));
    END QueryButton;

(**************************************************************************)

PROCEDURE StoreData (hwnd: OS2.HWND): BOOLEAN;

    (* Saves the list data back to the INI file.  The hwnd parameter    *)
    (* identifies the frame of this page.  Returns TRUE if any of the   *)
    (* data have changed.                                               *)

    VAR abbrev: AbbreviationType;
        N, count: CARDINAL;
        stringval: ARRAY [0..511] OF CHAR;
        changed: BOOLEAN;

    BEGIN
        changed := FALSE;

        (* Login name. *)

        OS2.WinQueryDlgItemText (hwnd, DID.LoginName, 257, stringval);
        IF NOT Strings.Equal (stringval, OldLoginName) THEN
            RINIData.INIPutString (ListName, "LoginName", stringval);
            changed := TRUE;
        END (*IF*);

        (* Password. *)

        OS2.WinQueryDlgItemText (hwnd, DID.POP3password, 33, stringval);
        IF NOT Strings.Equal (stringval, OldPassword) THEN
            RINIData.INIPutString (ListName, "Password", stringval);
            changed := TRUE;
        END (*IF*);

        (* Abbreviated list name. *)

        OS2.WinQueryDlgItemText (hwnd, DID.ListAbbreviation,
                                        AbbreviationLength+1, abbrev);
        IF NOT Strings.Equal (abbrev, OldAbbreviation) THEN
            RINIData.INIPutString (ListName, "Abbreviation", abbrev);
            changed := TRUE;
        END (*IF*);

        (* Read the interval. *)

        count := 0;  N := 0;
        OS2.WinSendDlgItemMsg (hwnd, DID.CheckTimeH, OS2.SPBM_QUERYVALUE,
                               ADR(count), OS2.MPFROM2SHORT(0,OS2.SPBQ_DONOTUPDATE));
        OS2.WinSendDlgItemMsg (hwnd, DID.CheckTimeM, OS2.SPBM_QUERYVALUE,
                               ADR(N), OS2.MPFROM2SHORT(0,OS2.SPBQ_DONOTUPDATE));
        count := 60*count + N;
        OS2.WinSendDlgItemMsg (hwnd, DID.CheckTimeS, OS2.SPBM_QUERYVALUE,
                               ADR(N), OS2.MPFROM2SHORT(0,OS2.SPBQ_DONOTUPDATE));
        count := 60*count + N;
        IF count <> OldInterval THEN
            RINIData.INIPut (ListName, "interval", count);
            changed := TRUE;
        END (*IF*);

        (* Address that error messages go to. *)

        IF QueryButton (hwnd, DID.ErrorsMailTo) > 0 THEN
            OS2.WinQueryDlgItemText (hwnd, DID.ErrorMessageRecipient, 512, stringval);
        ELSE
            stringval[0] := Nul;
        END (*IF*);
        IF NOT Strings.Equal (stringval, OldErrorMessRecipient) THEN
            RINIData.INIPutString (ListName, 'MailErrorsTo', stringval);
            changed := TRUE;
        END (*IF*);

        (* Filter. *)

        OS2.WinQueryDlgItemText (hwnd, DID.FilterProg, 512, stringval);
        IF NOT Strings.Equal (stringval, OldFilterProg) THEN
            RINIData.INIPutString (ListName, "FilterProg", stringval);
            changed := TRUE;
        END (*IF*);

        (* Language. *)

        OS2.WinQueryDlgItemText (hwnd, DID.ListLanguage, 33, stringval);
        IF NOT Strings.Equal (stringval, OldLanguage) THEN
            RINIData.INIPutString (ListName, "language", stringval);
            changed := TRUE;
        END (*IF*);

        RETURN changed;

    END StoreData;

(************************************************************************)
(*                    THE LIST PROPERTIES DIALOGUE                      *)
(************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    (* Message handler for the setup dialogue. *)

    VAR ButtonID, NotificationCode: CARDINAL;

    BEGIN
        CASE msg OF
           |  OS2.WM_INITDLG:
                   OS2.WinSetWindowPos (hwnd, 0, 0, 0, 0, 0, OS2.SWP_MOVE);
                   LoadListData (hwnd);
                   RETURN NIL;

           |  OS2.WM_CONTROL:

                   NotificationCode := OS2.ULONGFROMMP(mp1);
                   ButtonID := NotificationCode MOD 65536;
                   NotificationCode := NotificationCode DIV 65536;
                   IF NotificationCode = OS2.BN_CLICKED THEN
                       CASE ButtonID OF
                         | DID.ErrorsIgnore:
                              OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd,
                                                           DID.ErrorMessageRecipient), FALSE);
                              RETURN NIL;
                         | DID.ErrorsMailTo:
                              OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd,
                                                           DID.ErrorMessageRecipient), TRUE);
                              RETURN NIL;
                       ELSE
                              RETURN OS2.WinDefDlgProc (hwnd, msg, mp1, mp2);
                       END (*CASE*);
                   END (*IF*);

           |  OS2.WM_PRESPARAMCHANGED:

                   IF ChangeInProgress THEN
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   ELSE
                       ChangeInProgress := TRUE;
                       CommonSettings.UpdateFontFrom (hwnd, CommonSettings.ListNotebook);
                       ChangeInProgress := FALSE;
                       RETURN NIL;
                   END (*IF*);

        ELSE
            RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);

        (* Catch any cases not handled above. *)

        RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

    END DialogueProc;

(************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND;  name: ARRAY OF CHAR): OS2.HWND;

    (* Creates page 1 and adds it to the notebook. *)

    BEGIN
        notebookhandle := notebook;
        Strings.Assign (name, ListName);
        pagehandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.ELpage1,                (* dialogue ID *)
                       NIL);                 (* creation parameters *)
        PageID := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         NIL, OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE, OS2.BKA_LAST)));
        OS2.WinSendMsg (notebook, OS2.BKM_SETPAGEWINDOWHWND,
                        CAST(ADDRESS,PageID), CAST(ADDRESS,pagehandle));
        RETURN pagehandle;
    END CreatePage;

(**************************************************************************)

PROCEDURE SetDefaultLanguage (VAR (*IN*) language: ARRAY OF CHAR);

    (* Sets the default language for new lists. *)

    BEGIN
        Strings.Assign (language, DefaultLanguage);
    END SetDefaultLanguage;

(**************************************************************************)

PROCEDURE SetFont (VAR (*IN*) name: CommonSettings.FontName);

    (* Sets the font of the text on this page. *)

    CONST bufsize = CommonSettings.FontNameSize;

    BEGIN
        OS2.WinSetPresParam (pagehandle, OS2.PP_FONTNAMESIZE, bufsize, name);
    END SetFont;

(**************************************************************************)

PROCEDURE SetINIFileName (name: ARRAY OF CHAR;  TNImode: BOOLEAN);

    (* Sets the INI file name and mode. *)

    BEGIN
        Strings.Assign (name, INIFileName);
        UseTNI := TNImode;
    END SetINIFileName;

(**************************************************************************)

BEGIN
    ChangeInProgress := FALSE;
    DefaultLanguage := "en";
END ELpage1.

