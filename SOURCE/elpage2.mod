(**************************************************************************)
(*                                                                        *)
(*  Admin program for the Major Major mailing list manager                *)
(*  Copyright (C) 2017   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE ELpage2;

        (************************************************************)
        (*                                                          *)
        (*              Admin program for MajorMajor                *)
        (*           Page 2 of the "edit list" notebook             *)
        (*                                                          *)
        (*    Started:        24 October 2000                       *)
        (*    Last edited:    11 June 2017                          *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)


FROM SYSTEM IMPORT
    (* type *)  CARD8, ADDRESS,
    (* proc *)  CAST, ADR;

IMPORT OS2, DID, Strings, RINIData, About, CommonSettings;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM Names IMPORT
    (* type *)  FilenameString;

FROM MiscFuncs IMPORT
    (* proc *)  EVAL;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);
    NameLength = 256;

TYPE
    NameString = ARRAY [0..NameLength] OF CHAR;

VAR
    INIFileName: FilenameString;
    UseTNI: BOOLEAN;

    pagehandle, notebookhandle: OS2.HWND;
    PageID: CARDINAL;

    Oldcharset, OldRFC2919ID, ListName: NameString;

    OldAdd2369Headers,
    OldKillAttachments,
    OldIsModerated,
    OldSuppressFrom,
    OldDMARCcompatible,
    OldEnableSubscribe,
    OldEnableLongSubscribe,
    OldEnableLongUnsubscribe,
    OldEnableWho             : BOOLEAN;

    ChangeInProgress: BOOLEAN;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        StrToBuffer (lang, "ELpage2.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(stringval));
        StrToBuffer (lang, "ELpage2.KillAttachments", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.KillAttachments, stringval);
        StrToBuffer (lang, "ELpage2.IsModerated", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.IsModerated, stringval);
        StrToBuffer (lang, "ELpage2.SuppressFrom", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.SuppressFrom, stringval);
        StrToBuffer (lang, "ELpage2.DMARCcompatible", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.DMARCcompatible, stringval);
        StrToBuffer (lang, "ELpage2.EnableControlled", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.EnableControlled, stringval);
        StrToBuffer (lang, "ELpage2.EnableSubscribe", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.EnableSubscribe, stringval);
        StrToBuffer (lang, "ELpage2.EnableLongSubscribe", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.EnableLongSubscribe, stringval);
        StrToBuffer (lang, "ELpage2.EnableLongUnsubscribe", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.EnableLongUnsubscribe, stringval);
        StrToBuffer (lang, "ELpage2.EnableWho", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.EnableWho, stringval);
        StrToBuffer (lang, "ELpage2.charset", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.CharsetLabel, stringval);
        StrToBuffer (lang, "ELpage2.RFC2919ID", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.RFC2919IDLabel, stringval);
        StrToBuffer (lang, "ELpage2.Add2369Headers", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.Add2369Headers, stringval);
    END SetLanguage;

(**************************************************************************)
(*                        DIALOGUE PROCEDURES                             *)
(**************************************************************************)

PROCEDURE LoadListData (hwnd: OS2.HWND);

    (* Fills the dialogue windows with data from the INI file, or       *)
    (* loads default values if they're not in the INI file.             *)

    VAR opened, bool: BOOLEAN;

    BEGIN
        OldIsModerated := FALSE;
        OldSuppressFrom := FALSE;
        OldEnableSubscribe := FALSE;
        OldEnableLongSubscribe := TRUE;
        OldEnableLongUnsubscribe := TRUE;
        OldEnableWho := TRUE;

        opened := RINIData.OpenINIFile(INIFileName, UseTNI);

        (* Kill attachments. *)

        bool := TRUE;
        IF (opened AND RINIData.INIFetch (ListName, "KillAttachments", bool)) THEN
            OldKillAttachments := bool;
        ELSE
            OldKillAttachments := FALSE;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.KillAttachments, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(bool)), NIL);

        (* Moderated. *)

        IF NOT (opened AND RINIData.INIFetch (ListName, "IsModerated", bool)) THEN
            bool := FALSE;
        END (*IF*);
        OldIsModerated := bool;
        OS2.WinSendDlgItemMsg (hwnd, DID.IsModerated, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(bool)), NIL);

        (* Suppress sender address. *)

        IF NOT (opened AND RINIData.INIFetch (ListName, "SuppressFrom", bool)) THEN
            bool := FALSE;
        END (*IF*);
        OldSuppressFrom := bool;
        OS2.WinSendDlgItemMsg (hwnd, DID.SuppressFrom, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(bool)), NIL);

        (* DMARC compatible. *)

        IF NOT (opened AND RINIData.INIFetch (ListName, "DMARCcompatible", bool)) THEN
            bool := FALSE;
        END (*IF*);
        OldDMARCcompatible := bool;
        OS2.WinSendDlgItemMsg (hwnd, DID.DMARCcompatible, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(bool)), NIL);

        (* Enabling specific commands. *)

        IF opened AND RINIData.INIFetch (ListName, "ControlledSubs", bool) THEN
            bool := NOT bool;
            RINIData.INIDeleteKey (ListName, "ControlledSubs");
        ELSIF NOT (opened AND RINIData.INIFetch (ListName, "EnableSubscribe", bool)) THEN
            bool := TRUE;
        END (*IF*);
        OldEnableSubscribe := bool;
        OS2.WinSendDlgItemMsg (hwnd, DID.EnableSubscribe, OS2.BM_SETCHECK,
                                          OS2.MPFROMSHORT(ORD(bool)), NIL);

        IF NOT (opened AND RINIData.INIFetch (ListName, "EnableLongSubscribe", bool)) THEN
            bool := TRUE;
        END (*IF*);
        OldEnableLongSubscribe := bool;
        OS2.WinSendDlgItemMsg (hwnd, DID.EnableLongSubscribe, OS2.BM_SETCHECK,
                                          OS2.MPFROMSHORT(ORD(bool)), NIL);

        IF NOT (opened AND RINIData.INIFetch (ListName, "EnableLongUnsubscribe", bool)) THEN
            bool := TRUE;
        END (*IF*);
        OldEnableLongUnsubscribe := bool;
        OS2.WinSendDlgItemMsg (hwnd, DID.EnableLongUnsubscribe, OS2.BM_SETCHECK,
                                          OS2.MPFROMSHORT(ORD(bool)), NIL);

        IF NOT (opened AND RINIData.INIFetch (ListName, "EnableWho", bool)) THEN
            bool := TRUE;
        END (*IF*);
        OldEnableWho := bool;
        OS2.WinSendDlgItemMsg (hwnd, DID.EnableWho, OS2.BM_SETCHECK,
                                          OS2.MPFROMSHORT(ORD(bool)), NIL);

        (* Default character set. *)

        IF opened AND RINIData.INIGetString (ListName, "charset", Oldcharset) THEN
            OS2.WinSetDlgItemText (hwnd, DID.charset, Oldcharset);
        ELSE
            Oldcharset := "";
            OS2.WinSetDlgItemText (hwnd, DID.charset, "iso-8859-1");
        END (*IF*);

        (* RFC2919 list ID. *)

        IF NOT (opened AND RINIData.INIGetString (ListName, "RFC2919ID", OldRFC2919ID)) THEN
            Strings.Assign ("", OldRFC2919ID);
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.RFC2919ID, OldRFC2919ID);

        IF opened AND RINIData.INIFetch (ListName, "Add2369Headers", bool) THEN
            OldAdd2369Headers := bool;
        ELSE
            OldAdd2369Headers := FALSE;
            bool := TRUE;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.Add2369Headers, OS2.BM_SETCHECK,
                                          OS2.MPFROMSHORT(ORD(bool)), NIL);

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

    VAR bool, changed: BOOLEAN;
        stringval: NameString;

    BEGIN
        changed := FALSE;

        bool := QueryButton (hwnd, DID.KillAttachments) > 0;
        IF bool <> OldKillAttachments THEN
            changed := TRUE;
            RINIData.INIPut (ListName, "KillAttachments", bool);
        END (*IF*);

        bool := QueryButton (hwnd, DID.IsModerated) > 0;
        IF bool <> OldIsModerated THEN
            changed := TRUE;
            RINIData.INIPut (ListName, "IsModerated", bool);
        END (*IF*);

        bool := QueryButton (hwnd, DID.SuppressFrom) > 0;
        IF bool <> OldSuppressFrom THEN
            changed := TRUE;
            RINIData.INIPut (ListName, "SuppressFrom", bool);
        END (*IF*);

        bool := QueryButton (hwnd, DID.DMARCcompatible) > 0;
        IF bool <> OldDMARCcompatible THEN
            changed := TRUE;
            RINIData.INIPut (ListName, "DMARCcompatible", bool);
        END (*IF*);

        bool := QueryButton (hwnd, DID.EnableSubscribe) > 0;
        IF bool <> OldEnableSubscribe THEN
            changed := TRUE;
            RINIData.INIPut (ListName, 'EnableSubscribe', bool);
        END (*IF*);

        bool := QueryButton (hwnd, DID.EnableLongSubscribe) > 0;
        IF bool <> OldEnableLongSubscribe THEN
            changed := TRUE;
            RINIData.INIPut (ListName, 'EnableLongSubscribe', bool);
        END (*IF*);

        bool := QueryButton (hwnd, DID.EnableLongUnsubscribe) > 0;
        IF bool <> OldEnableLongUnsubscribe THEN
            changed := TRUE;
            RINIData.INIPut (ListName, 'EnableLongUnsubscribe', bool);
        END (*IF*);

        bool := QueryButton (hwnd, DID.EnableWho) > 0;
        IF bool <> OldEnableWho THEN
            changed := TRUE;
            RINIData.INIPut (ListName, 'EnableWho', bool);
        END (*IF*);

        (* Default character set. *)

        OS2.WinQueryDlgItemText (hwnd, DID.charset, 512, stringval);
        IF NOT Strings.Equal (stringval, Oldcharset) THEN
            RINIData.INIPutString (ListName, "charset", stringval);
            changed := TRUE;
        END (*IF*);

        (* RFC2919 list ID. *)

        OS2.WinQueryDlgItemText (hwnd, DID.RFC2919ID, 512, stringval);
        IF NOT Strings.Equal (stringval, OldRFC2919ID) THEN
            RINIData.INIPutString (ListName, "RFC2919ID", stringval);
            changed := TRUE;
        END (*IF*);

        bool := QueryButton (hwnd, DID.Add2369Headers) > 0;
        IF bool <> OldAdd2369Headers THEN
            changed := TRUE;
            RINIData.INIPut (ListName, 'Add2369Headers', bool);
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

    BEGIN
        IF msg = OS2.WM_INITDLG THEN
            OS2.WinSetWindowPos (hwnd, 0, 0, 0, 0, 0, OS2.SWP_MOVE);
            LoadListData (hwnd);
            RETURN NIL;

        ELSIF msg = OS2.WM_PRESPARAMCHANGED THEN

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
        END (*IF*);

    END DialogueProc;

(************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND;  name: ARRAY OF CHAR): OS2.HWND;

    (* Creates page 2 and adds it to the notebook. *)

    BEGIN
        notebookhandle := notebook;
        Strings.Assign (name, ListName);
        pagehandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.ELpage2,                (* dialogue ID *)
                       NIL);                 (* creation parameters *)
        PageID := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         NIL, OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE, OS2.BKA_LAST)));
        OS2.WinSendMsg (notebook, OS2.BKM_SETPAGEWINDOWHWND,
                        CAST(ADDRESS,PageID), CAST(ADDRESS,pagehandle));
        RETURN pagehandle;
    END CreatePage;

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
END ELpage2.

