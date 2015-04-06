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

IMPLEMENTATION MODULE ELopt2;

        (************************************************************)
        (*                                                          *)
        (*              Admin program for MajorMajor                *)
        (*     The "options 2" page of the "edit list" notebook     *)
        (*                                                          *)
        (*    Started:        12 October 2003                       *)
        (*    Last edited:    8 February 2009                       *)
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

FROM Misc IMPORT
    (* proc *)  WinSetDlgItemCard, WinQueryDlgItemCard;

FROM Names IMPORT
    (* type *)  FilenameString;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

(************************************************************************)

CONST
    Nul = CHR(0);
    NameLength = 256;
    DefaultConfTime = 24;    (* hours *)

VAR
    INIFileName: FilenameString;
    UseTNI: BOOLEAN;

    pagehandle, notebookhandle: OS2.HWND;
    PageID: CARDINAL;
    ListName: ARRAY [0..NameLength-1] OF CHAR;
    OldRequireConfirmation, OldOwnersMayConfUnsub,
                            OldSaveRejects, ChangeInProgress: BOOLEAN;
    OldConfTime: CARDINAL;
    OldNonsubOption: CARD8;
    OldRejectDir: FilenameString;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        StrToBuffer (lang, "ELopt2.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(stringval));
        StrToBuffer (lang, "ELopt2.confbox", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.ELopt2ConfBox, stringval);
        StrToBuffer (lang, "ELopt2.RequireConfirmation", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.RequireConfirmation, stringval);
        StrToBuffer (lang, "ELopt2.ConfTime", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.ConfTimeLabel, stringval);
        StrToBuffer (lang, "ELopt2.ConfHours", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.ConfHoursLabel, stringval);
        StrToBuffer (lang, "ELopt2.OwnersMayConfUnsub", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.OwnersMayConfUnsub, stringval);
        StrToBuffer (lang, "ELopt2.nonsub", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.NonsubBox, stringval);
        StrToBuffer (lang, "ELopt2.ignore", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.NonsubIgnore, stringval);
        StrToBuffer (lang, "ELopt2.accept", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.NonsubAccept, stringval);
        StrToBuffer (lang, "ELopt2.reject", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.NonsubReject, stringval);
        StrToBuffer (lang, "ELopt2.saverefused", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.SaveRejects, stringval);
    END SetLanguage;

(**************************************************************************)
(*                        DIALOGUE PROCEDURES                             *)
(**************************************************************************)

PROCEDURE LoadListData (hwnd: OS2.HWND);

    (* Fills the dialogue windows with data from the INI file, or       *)
    (* loads default values if they're not in the INI file.             *)

    VAR opened, bool: BOOLEAN;  card: CARDINAL;  dir: FilenameString;

    BEGIN
        OldRequireConfirmation := FALSE;
        OldConfTime := DefaultConfTime;

        opened := RINIData.OpenINIFile(INIFileName, UseTNI);

        (* Require confirmation for this list. *)

        IF opened AND RINIData.INIFetch (ListName, "RequireConfirmation", bool) THEN
            OldRequireConfirmation := bool;
        ELSE
            bool := FALSE;
            OldRequireConfirmation := bool;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.RequireConfirmation, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(bool)), NIL);

        (* Time limit for the arrival of the confirmation. *)

        card := DefaultConfTime;
        IF opened THEN
            EVAL (RINIData.INIFetch (ListName, "ConfTime", card));
        END (*IF*);
        OldConfTime := card;
        WinSetDlgItemCard (hwnd, DID.ConfTime, card);

        IF bool THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.ConfTime), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.OwnersMayConfUnsub), TRUE);
        ELSE
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.ConfTime), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.OwnersMayConfUnsub), FALSE);
        END (*IF*);

        (* List owners may confirm 'unsubscribe'. *)

        IF opened AND RINIData.INIFetch (ListName, "OwnersMayConfUnsub", bool) THEN
            OldOwnersMayConfUnsub := bool;
        ELSE
            bool := TRUE;
            OldOwnersMayConfUnsub := FALSE;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.OwnersMayConfUnsub, OS2.BM_SETCHECK,
                                        OS2.MPFROMSHORT(ORD(bool)), NIL);

        (* Disposition of mail from non-subscribers. *)

        IF NOT (opened AND RINIData.INIFetch (ListName, "Nonsub", OldNonsubOption)) THEN
            OldNonsubOption := 0;
        END (*IF*);

        CASE OldNonsubOption OF
           | 0:  OS2.WinSendDlgItemMsg (hwnd, DID.NonsubIgnore, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(TRUE)), NIL);
           | 1:  OS2.WinSendDlgItemMsg (hwnd, DID.NonsubReject, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(TRUE)), NIL);
           | 2:  OS2.WinSendDlgItemMsg (hwnd, DID.NonsubAccept, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(TRUE)), NIL);
        END (*CASE*);

        (* Save a copy of refused mail. *)

        IF opened AND RINIData.INIFetch (ListName, "SaveRejects", bool) THEN
            OldSaveRejects := bool;
        ELSE
            bool := FALSE;
            OldSaveRejects := bool;
        END (*IF*);

        IF bool THEN
            OS2.WinSendDlgItemMsg (hwnd, DID.SaveRejects, OS2.BM_SETCHECK,
                                        OS2.MPFROMSHORT(ORD(TRUE)), NIL);
        ELSE
            OS2.WinSendDlgItemMsg (hwnd, DID.SaveRejects, OS2.BM_SETCHECK,
                                        OS2.MPFROMSHORT(ORD(FALSE)), NIL);
        END (*IF*);

        (* Directory for the saved rejects. *)

        IF opened AND RINIData.INIGetString (ListName, "RejectDir", dir) THEN
            OldRejectDir := dir;
        ELSE
            dir := "refused";
            OldRejectDir := "";
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.RejectDir, dir);

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

    VAR bool, changed: BOOLEAN;  card: CARDINAL;
        NonsubOption: CARD8;  stringval: FilenameString;

    BEGIN
        changed := FALSE;

        (* Require confirmation for this list. *)

        bool := QueryButton (hwnd, DID.RequireConfirmation) > 0;
        IF bool <> OldRequireConfirmation THEN
            changed := TRUE;
            RINIData.INIPut (ListName, "RequireConfirmation", bool);
        END (*IF*);

        (* Time limit for the arrival of the confirmation. *)

        WinQueryDlgItemCard (hwnd, DID.ConfTime, card);
        IF card <> OldConfTime THEN
            changed := TRUE;
            RINIData.INIPut (ListName, "ConfTime", card);
        END (*IF*);

        (* Owners may confirm 'unsubscribe'. *)

        bool := QueryButton (hwnd, DID.OwnersMayConfUnsub) > 0;
        IF bool <> OldOwnersMayConfUnsub THEN
            changed := TRUE;
            RINIData.INIPut (ListName, "OwnersMayConfUnsub", bool);
        END (*IF*);

        (* Disposition of mail from non-subscribers. *)

        IF QueryButton (hwnd, DID.NonsubAccept) > 0 THEN
            NonsubOption := 2;
        ELSIF QueryButton (hwnd, DID.NonsubReject) > 0 THEN
            NonsubOption := 1;
        ELSE
            NonsubOption := 0;
        END (*IF*);
        IF NonsubOption <> OldNonsubOption THEN
            RINIData.INIPut (ListName, "Nonsub", NonsubOption);
            changed := TRUE;
        END (*IF*);

        (* Save refused mail. *)

        bool := QueryButton (hwnd, DID.SaveRejects) > 0;
        IF bool <> OldSaveRejects THEN
            changed := TRUE;
            RINIData.INIPut (ListName, "SaveRejects", bool);
        END (*IF*);

        (* Directory for the saved rejects. *)

        OS2.WinQueryDlgItemText (hwnd, DID.RejectDir, 512, stringval);

        (* Ensure that the directory name does not end with '\' or      *)
        (* '/', except in the special case where that's the first char. *)

        card := Strings.Length (stringval);
        IF card > 1 THEN
            DEC (card);
            IF (stringval[card] = '\') OR (stringval[card] = '/') THEN
                stringval[card] := Nul;
            END (*IF*);
        END (*IF*);

        IF NOT Strings.Equal (stringval, OldRejectDir) THEN
            RINIData.INIPutString (ListName, "RejectDir", stringval);
            changed := TRUE;
            RINIData.MakeDirectory (stringval);
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

    (********************************************************************)

    PROCEDURE EnableWindow (button, entryfield: CARDINAL);

        (* Enables subwindow 'entryfield' if button is in the 'ON'      *)
        (* state, disables it otherwise.                                *)

        BEGIN
            IF OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, button,
                              OS2.BM_QUERYCHECK, NIL, NIL)) > 0 THEN
                OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, entryfield), TRUE);
            ELSE
                OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, entryfield), FALSE);
            END (*IF*);
        END EnableWindow;

    (********************************************************************)

    VAR NotificationCode, ButtonID: CARDINAL;

    BEGIN
        IF msg = OS2.WM_INITDLG THEN
            OS2.WinSetWindowPos (hwnd, 0, 0, 0, 0, 0, OS2.SWP_MOVE);
            LoadListData (hwnd);
            EnableWindow (DID.RequireConfirmation, DID.ConfTime);
            EnableWindow (DID.SaveRejects, DID.RejectDir);
            RETURN NIL;

        ELSIF msg = OS2.WM_CONTROL THEN
            NotificationCode := OS2.ULONGFROMMP(mp1);
            ButtonID := NotificationCode MOD 65536;
            NotificationCode := NotificationCode DIV 65536;
            IF NotificationCode = OS2.BN_CLICKED THEN
                IF ButtonID = DID.RequireConfirmation THEN
                    EnableWindow (DID.RequireConfirmation, DID.ConfTime);
                    EnableWindow (DID.RequireConfirmation, DID.OwnersMayConfUnsub);
                    RETURN NIL;
                ELSIF ButtonID = DID.SaveRejects THEN
                    EnableWindow (DID.SaveRejects, DID.RejectDir);
                    RETURN NIL;
                END (*IF*);
            END (*IF*);

        ELSIF msg = OS2.WM_PRESPARAMCHANGED THEN

            IF NOT ChangeInProgress THEN
                ChangeInProgress := TRUE;
                CommonSettings.UpdateFontFrom (hwnd, CommonSettings.ListNotebook);
                ChangeInProgress := FALSE;
                RETURN NIL;
            END (*IF*);

        END (*IF*);

        (* Catch all cases not handled above. *)

        RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

    END DialogueProc;

(************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND;  name: ARRAY OF CHAR): OS2.HWND;

    (* Creates this page and adds it to the notebook. *)

    BEGIN
        notebookhandle := notebook;
        Strings.Assign (name, ListName);
        pagehandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.ELopt2,                (* dialogue ID *)
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
END ELopt2.

