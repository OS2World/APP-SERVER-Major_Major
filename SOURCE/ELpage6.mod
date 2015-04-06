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

IMPLEMENTATION MODULE ELpage6;

        (************************************************************)
        (*                                                          *)
        (*              Admin program for MajorMajor                *)
        (*           Page 6 of the "edit list" notebook             *)
        (*                                                          *)
        (*    Started:        20 January 2001                       *)
        (*    Last edited:    10 February 2009                      *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)


FROM SYSTEM IMPORT
    (* type *)  CARD8, ADDRESS,
    (* proc *)  CAST, ADR;

IMPORT OS2, DID, Strings, RINIData, About, CommonSettings, FileSys;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM Names IMPORT
    (* type *)  FilenameString;

FROM Misc IMPORT
    (* proc *)  WinSetDlgItemCard, WinQueryDlgItemCard;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

(************************************************************************)

CONST
    Nul = CHR(0);
    NameLength = 256;

TYPE
    TimeUnitType = (days, months);
    UnitName = ARRAY [0..31] OF CHAR;

VAR
    INIFileName: FilenameString;
    UseTNI: BOOLEAN;

    pagehandle, notebookhandle: OS2.HWND;
    PageID: CARDINAL;
    ListName: ARRAY [0..NameLength-1] OF CHAR;
    TimeUnitPtrs: ARRAY [0..1] OF POINTER TO UnitName;
    ArchiveRoot: FilenameString;

    ChangeInProgress: BOOLEAN;

    OldArcEnabled: BOOLEAN;
    OldArcInterval: CARDINAL;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        StrToBuffer (lang, "ELpage6.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(stringval));
        StrToBuffer (lang, "ELpage6.ArcEnabled", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.ArcEnabled, stringval);
        StrToBuffer (lang, "ELpage6.NewArchive", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.NewArchiveLabel, stringval);
        StrToBuffer (lang, "ELpage6.days", stringval);
        Strings.Assign (stringval, TimeUnitPtrs[0]^);
        StrToBuffer (lang, "ELpage6.months", stringval);
        Strings.Assign (stringval, TimeUnitPtrs[1]^);
        OS2.WinSendDlgItemMsg (pagehandle, DID.ArcTimeUnit, OS2.SPBM_SETARRAY,
                     ADR(TimeUnitPtrs), OS2.MPFROMSHORT(2));
    END SetLanguage;

(**************************************************************************)
(*                        DIALOGUE PROCEDURES                             *)
(**************************************************************************)

PROCEDURE LoadListData (hwnd: OS2.HWND);

    (* Fills the dialogue windows with data from the INI file, or       *)
    (* loads default values if they're not in the INI file.             *)

    VAR opened, bool: BOOLEAN;  card: CARDINAL;
        unit: TimeUnitType;

    BEGIN
        OldArcEnabled := FALSE;
        OldArcInterval := 28;

        opened := RINIData.OpenINIFile(INIFileName, UseTNI);

        (* Enable archiving. *)

        bool := FALSE;
        EVAL (opened AND RINIData.INIFetch (ListName, "ArcEnabled", bool));
        OldArcEnabled := bool;
        OS2.WinSendDlgItemMsg (hwnd, DID.ArcEnabled, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(bool)), NIL);

        (* Interval between making new archive file. *)

        card := 28;
        EVAL (opened AND RINIData.INIFetch (ListName, "ArcInterval", card));
        OldArcInterval := card;
        WinSetDlgItemCard (hwnd, DID.ArcInterval, card);
        IF NOT RINIData.INIFetch (ListName, "ArcTimeUnit", unit) THEN
            unit := days;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.ArcTimeUnit, OS2.SPBM_SETARRAY,
                     ADR(TimeUnitPtrs), OS2.MPFROMSHORT(2));
        OS2.WinSendDlgItemMsg (hwnd, DID.ArcTimeUnit, OS2.SPBM_SETCURRENTVALUE,
                     OS2.MPFROMSHORT(ORD(unit)), NIL);

        IF bool THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.ArcInterval), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.ArcTimeUnit), TRUE);
        ELSE
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.ArcInterval), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.ArcTimeUnit), FALSE);
        END (*IF*);

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
        dirname: FilenameString;
        unit: TimeUnitType;

    BEGIN
        changed := FALSE;

        (* Enable archiving. *)

        bool := QueryButton (hwnd, DID.ArcEnabled) > 0;
        IF bool <> OldArcEnabled THEN
            changed := TRUE;
            RINIData.INIPut (ListName, "ArcEnabled", bool);
        END (*IF*);

        (* Interval between making new archive file. *)

        WinQueryDlgItemCard (hwnd, DID.ArcInterval, card);
        IF card <> OldArcInterval THEN
            changed := TRUE;
            RINIData.INIPut (ListName, "ArcInterval", card);
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.ArcTimeUnit, OS2.SPBM_QUERYVALUE,
                     ADR(unit), NIL);
        RINIData.INIPut (ListName, "ArcTimeUnit", unit);

        (* If archiving enabled, ensure that there's a directory for it. *)

        IF bool THEN
            Strings.Assign (ArchiveRoot, dirname);
            EVAL (FileSys.CreateDirectory(dirname));
            Strings.Append ("\", dirname);
            Strings.Append (ListName, dirname);
            EVAL (FileSys.CreateDirectory(dirname));
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

    VAR NotificationCode, ButtonID: CARDINAL;

    BEGIN
        IF msg = OS2.WM_INITDLG THEN
            OS2.WinSetWindowPos (hwnd, 0, 0, 0, 0, 0, OS2.SWP_MOVE);
            LoadListData (hwnd);
            RETURN NIL;

        ELSIF msg = OS2.WM_CONTROL THEN
            NotificationCode := OS2.ULONGFROMMP(mp1);
            ButtonID := NotificationCode MOD 65536;
            NotificationCode := NotificationCode DIV 65536;
            IF (NotificationCode = OS2.BN_CLICKED) AND (ButtonID = DID.ArcEnabled) THEN
                IF OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.ArcEnabled,
                                  OS2.BM_QUERYCHECK, NIL, NIL)) > 0 THEN
                    OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.ArcInterval), TRUE);
                    OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.ArcTimeUnit), TRUE);
                ELSE
                    OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.ArcInterval), FALSE);
                    OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.ArcTimeUnit), FALSE);
                END (*IF*);
                RETURN NIL;
            ELSE
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            END (*IF*);

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

    (* Creates page 6 and adds it to the notebook. *)

    BEGIN
        notebookhandle := notebook;
        Strings.Assign (name, ListName);
        pagehandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.ELpage6,                (* dialogue ID *)
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

PROCEDURE SetArchiveRoot (dir: ARRAY OF CHAR);

    (* Sets the root directory for archives. *)

    BEGIN
        Strings.Assign (dir, ArchiveRoot);
    END SetArchiveRoot;

(**************************************************************************)

BEGIN
    ChangeInProgress := FALSE;
    Strings.Assign ("archives", ArchiveRoot);
    NEW (TimeUnitPtrs[0]);
    TimeUnitPtrs[0]^ := "days";
    NEW (TimeUnitPtrs[1]);
    TimeUnitPtrs[1]^ := "months";
FINALLY
    DISPOSE (TimeUnitPtrs[0]);
    DISPOSE (TimeUnitPtrs[1]);
END ELpage6.

