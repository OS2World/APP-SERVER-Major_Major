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

IMPLEMENTATION MODULE EditList;

        (************************************************************)
        (*                                                          *)
        (*              Admin program for MajorMajor                *)
        (*             Dialogue to edit a single list               *)
        (*                                                          *)
        (*    Started:        24 August 2000                        *)
        (*    Last edited:    24 April 2012                         *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)


FROM SYSTEM IMPORT
    (* type *)  CARD16, ADDRESS,
    (* proc *)  CAST;

IMPORT OS2, OS2RTL, Strings, DID, PMInit, CommonSettings, RINIData, INIData,
       ELpage1, ELpage2, ELopt2,
       ELpage4, ELpage5, Message1, Message2, ELpage6;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBufferA;

FROM INIData IMPORT
    (* proc *)  SetInitialWindowPosition, StoreWindowPosition;

FROM LowLevel IMPORT
    (* proc *)  IAND;

FROM Names IMPORT
    (* type *)  FilenameString;

(**************************************************************************)

CONST
    Nul = CHR(0);
    NameLength = 256;

VAR
    INIFileName: FilenameString;
    UseTNI: BOOLEAN;

    ListName: ARRAY [0..NameLength-1] OF CHAR;
    SwitchData : OS2.SWCNTRL;     (* switch entry data *)
    pagehandle: ARRAY [1..9] OF OS2.HWND;
    ChangeInProgress: BOOLEAN;
    NewStyle: BOOLEAN;
    PageFont, TabFontName: CommonSettings.FontName;
    AdminINI: ARRAY [0..9] OF CHAR;

(**************************************************************************)

PROCEDURE SetPageFonts;

    (* Changes the font of the notebook pages to the font recorded in the *)
    (* INI file as belonging to this notebook.                            *)

    VAR NewFontName: CommonSettings.FontName;

    BEGIN
        CommonSettings.CurrentFont (CommonSettings.ListNotebook, NewFontName);
        IF NOT Strings.Equal (NewFontName, PageFont) THEN
            PageFont := NewFontName;
            ELpage1.SetFont (PageFont);
            ELpage2.SetFont (PageFont);
            ELopt2.SetFont (PageFont);
            ELpage4.SetFont (PageFont);
            ELpage5.SetFont (PageFont);
            Message1.SetFont (PageFont);
            Message2.SetFont (PageFont);
            ELpage6.SetFont (PageFont);
        END (*IF*);
    END SetPageFonts;

(**************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Sets the language for all the notebook pages. *)

    BEGIN
        ELpage1.SetLanguage (lang);
        ELpage2.SetLanguage (lang);
        ELopt2.SetLanguage (lang);
        ELpage4.SetLanguage (lang);
        ELpage5.SetLanguage (lang);
        Message1.SetLanguage (lang);
        Message2.SetLanguage (lang);
        ELpage6.SetLanguage (lang);
    END SetLanguage;

(**************************************************************************)

PROCEDURE SetINIname;

    (* Sets the INI file name and mode for all the notebook pages. *)

    BEGIN
        ELpage1.SetINIFileName(INIFileName, UseTNI);
        ELpage2.SetINIFileName(INIFileName, UseTNI);
        ELopt2.SetINIFileName(INIFileName, UseTNI);
        ELpage4.SetINIFileName(INIFileName, UseTNI);
        ELpage5.SetINIFileName(INIFileName, UseTNI);
        Message1.SetINIFileName(INIFileName, UseTNI);
        Message2.SetINIFileName(INIFileName, UseTNI);
        ELpage6.SetINIFileName(INIFileName, UseTNI);
    END SetINIname;

(**************************************************************************)

PROCEDURE MakeNotebookNewStyle (hwnd: OS2.HWND;  NewStyle: BOOLEAN);

    (* Change to Warp 3 or Warp 4 notebook style. *)

    CONST
        OldStyleFlags = OS2.BKS_BACKPAGESBR + OS2.BKS_MAJORTABBOTTOM
                + OS2.BKS_ROUNDEDTABS + OS2.BKS_TABTEXTCENTER
                + OS2.BKS_STATUSTEXTCENTER + OS2.BKS_SPIRALBIND;
        NewStyleFlags = OS2.BKS_TABBEDDIALOG + OS2.BKS_MAJORTABTOP + OS2.BKS_BACKPAGESTR;

    VAR style: CARDINAL;

    BEGIN
        style := OS2.WinQueryWindowULong (hwnd, OS2.QWL_STYLE);
        style := IAND (style, 0FFFF0000H);
        IF NewStyle THEN
            INC (style, NewStyleFlags);
        ELSE
            INC (style, OldStyleFlags);
        END (*IF*);
        OS2.WinSetWindowULong (hwnd, OS2.QWL_STYLE, style);
    END MakeNotebookNewStyle;

(**************************************************************************)

PROCEDURE InitialiseNotebook (hwnd: OS2.HWND);

    (* hwnd is the handle of the notebook control. *)

    VAR hini: INIData.HINI;  swp: OS2.SWP;  scale, z: CARDINAL;
        app: ARRAY [0..4] OF CHAR;

    BEGIN
        SetINIname;
        MakeNotebookNewStyle (hwnd, NewStyle);
        hini := INIData.OpenINIFile (AdminINI, UseTNI);
        app := "Font";
        IF NOT INIData.INIGetString (hini, app, "ListNotebookTabs", TabFontName) THEN
            TabFontName := "8.Helv";
        END (*IF*);
        INIData.CloseINIFile (hini);
        OS2.WinSetPresParam (hwnd, OS2.PP_FONTNAMESIZE,CommonSettings.FontNameSize, TabFontName);
        OS2.WinQueryWindowPos (hwnd, swp);
        IF swp.cx > MAX(CARD16) THEN
            swp.cx := MAX(CARD16);
        END (*IF*);
        scale := 2*swp.cx DIV 13;
        z := 5*scale DIV 12;
        OS2.WinSendMsg (hwnd, OS2.BKM_SETDIMENSIONS,
             OS2.MPFROM2SHORT(scale,z), OS2.MPFROMSHORT(OS2.BKA_MAJORTAB));
        OS2.WinSendMsg (hwnd, OS2.BKM_SETNOTEBOOKCOLORS,
                        CAST(ADDRESS,00FFFFAAH), CAST(ADDRESS,OS2.BKA_BACKGROUNDPAGECOLOR));
        OS2.WinSendMsg (hwnd, OS2.BKM_SETNOTEBOOKCOLORS,
                        CAST(ADDRESS,0055DBFFH(*0080DBAAH*)), CAST(ADDRESS,OS2.BKA_BACKGROUNDMAJORCOLOR));
        pagehandle[1] := ELpage1.CreatePage(hwnd, ListName);
        pagehandle[2] := ELpage2.CreatePage(hwnd, ListName);
        pagehandle[3] := ELopt2.CreatePage(hwnd, ListName);
        pagehandle[4] := 0;
        pagehandle[5] := ELpage4.CreatePage(hwnd, ListName);
        pagehandle[6] := ELpage5.CreatePage(hwnd, ListName);
        pagehandle[7] := Message1.CreatePage(hwnd, ListName);
        pagehandle[8] := Message2.CreatePage(hwnd, ListName);
        pagehandle[9] := ELpage6.CreatePage(hwnd, ListName);
        SetPageFonts;
        OS2.WinShowWindow (hwnd, TRUE);

    END InitialiseNotebook;

(**************************************************************************)
(*                WINDOW PROCEDURE FOR SUBCLASSED CASE                    *)
(**************************************************************************)

PROCEDURE ["SysCall"] SubWindowProc (hwnd     : OS2.HWND;
                                     msg      : OS2.ULONG;
                                     mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    (* Window procedure to intercept some of the things that happen in  *)
    (* the notebook subwindow.  We want this here mainly so that we can *)
    (* detect a new font dropped on the notebook tabs.  If the message  *)
    (* is something we don't want to deal with here, we pass it         *)
    (* to the parent window procedure.                                  *)

    VAR OldWndProc: OS2.PFNWP;
        owner: OS2.HWND;  hini: INIData.HINI;
        length, AttrFound: CARDINAL;
        NewFontName: CommonSettings.FontName;
        app: ARRAY [0..4] OF CHAR;

    BEGIN
        OldWndProc := CAST (OS2.PFNWP, OS2.WinQueryWindowPtr (hwnd, OS2.QWL_USER));
        owner := OS2.WinQueryWindow(hwnd,OS2.QW_OWNER);

        (* Because of the interaction between subclassing and DragText, *)
        (* some messages will go lost if we use the obvious strategy of *)
        (* sending them through to OldWndProc.  To get around this, we  *)
        (* have to send those messages directly to the target window.   *)

        IF (msg = OS2.WM_BUTTON2DOWN) OR (msg = OS2.DM_DRAGOVER)
                   OR (msg = OS2.DM_DRAGLEAVE) OR (msg = OS2.DM_DROP) THEN

            RETURN OS2.WinSendMsg (owner, msg, mp1, mp2);

        (* Check for font or colour change. *)

        ELSIF msg = OS2.WM_PRESPARAMCHANGED THEN

            IF ChangeInProgress THEN
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            ELSE
                ChangeInProgress := TRUE;
                length := OS2.WinQueryPresParam (hwnd, OS2.PP_FONTNAMESIZE, 0,
                                             AttrFound, CommonSettings.FontNameSize, NewFontName,
                                              0(*OS2.QPF_NOINHERIT*));
                IF length < CommonSettings.FontNameSize THEN
                    NewFontName[length] := Nul;
                END (*IF*);

                IF NOT Strings.Equal (NewFontName, TabFontName) THEN
                    TabFontName := NewFontName;
                    hini := INIData.OpenINIFile (AdminINI, UseTNI);
                    app := "Font";
                    INIData.INIPutString (hini, app, "ListNotebookTabs", TabFontName);
                    INIData.CloseINIFile (hini);
                    OS2.WinSetPresParam (hwnd, OS2.PP_FONTNAMESIZE,CommonSettings.FontNameSize, TabFontName);
                END (*IF*);
                ChangeInProgress := FALSE;
                RETURN NIL;
            END (*IF*);

        END (*IF*);

        RETURN OldWndProc (hwnd, msg, mp1, mp2);

    END SubWindowProc;

(**************************************************************************)
(*                   WINDOW PROCEDURE FOR MAIN DIALOGUE                   *)
(**************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    VAR changed, c1: BOOLEAN;
        bookwin: OS2.HWND;

    BEGIN
        CASE msg OF
           |  OS2.WM_INITDLG:
                   SetInitialWindowPosition (hwnd, AdminINI,
                                             "ListFrame", UseTNI);
                   bookwin := OS2.WinWindowFromID (hwnd, DID.ListNotebook);
                   InitialiseNotebook (bookwin);
                   OS2.WinSetWindowPtr (bookwin, OS2.QWL_USER,
                               CAST(ADDRESS,OS2.WinSubclassWindow (bookwin,
                                                           SubWindowProc)));

           |  CommonSettings.FONTCHANGED:

                   IF ChangeInProgress THEN
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   ELSE
                       ChangeInProgress := TRUE;
                       SetPageFonts;
                       ChangeInProgress := FALSE;
                       RETURN NIL;
                   END (*IF*);

           |  OS2.WM_CLOSE:
                   StoreWindowPosition (hwnd, AdminINI,
                                        "ListFrame", UseTNI);
                   IF RINIData.OpenINIFile (INIFileName, UseTNI) THEN
                       changed := ELpage1.StoreData (pagehandle[1]);
                       c1 := ELpage2.StoreData (pagehandle[2]);
                       changed := changed OR c1;
                       c1 := ELopt2.StoreData (pagehandle[3]);
                       changed := changed OR c1;
                       c1 := ELpage4.StoreData (pagehandle[5]);
                       changed := changed OR c1;
                       c1 := ELpage5.StoreData (pagehandle[6]);
                       changed := changed OR c1;
                       c1 := Message1.StoreData (pagehandle[7]);
                       changed := changed OR c1;
                       c1 := Message2.StoreData (pagehandle[8]);
                       changed := changed OR c1;
                       c1 := ELpage6.StoreData (pagehandle[9]);
                       changed := changed OR c1;
                       IF changed THEN
                           RINIData.INIPut (ListName, 'changed', changed);
                       END (*IF*);
                       RINIData.CloseINIFile;
                   END (*IF*);
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

        ELSE    (* default *)
           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);
        RETURN NIL;
    END DialogueProc;

(**************************************************************************)

PROCEDURE Edit (owner: OS2.HWND;  name: ARRAY OF CHAR;
                              lang: LangHandle;  W4Style: BOOLEAN);

    (* Edit the properties of the mailing list "name".  *)

    VAR hwnd: OS2.HWND;
        pid: OS2.PID;  tid: OS2.TID;
        title: ARRAY [0..NameLength-1] OF CHAR;

    BEGIN
        NewStyle := W4Style;
        PageFont := "";
        TabFontName := "";
        Strings.Assign (name, ListName);
        hwnd := OS2.WinLoadDlg(OS2.HWND_DESKTOP, owner,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.ListFrame,        (* dialogue ID *)
                       NIL);                (* creation parameters *)
        StrToBufferA (lang, "EditList.title", name, title);
        OS2.WinSetWindowText (hwnd, title);
        SetLanguage (lang);

        (* Put us on the visible task list. *)

        OS2.WinQueryWindowProcess (hwnd, pid, tid);
        SwitchData.hwnd := hwnd;
        WITH SwitchData DO
            hwndIcon      := 0;
            hprog         := 0;
            idProcess     := pid;
            idSession     := 0;
            uchVisibility := OS2.SWL_VISIBLE;
            fbJump        := OS2.SWL_JUMPABLE;
            szSwtitle     := "MajorMajor lists notebook";
            bProgType     := 0;
        END (*WITH*);
        OS2.WinCreateSwitchEntry (PMInit.OurHab(), SwitchData);

        OS2.WinProcessDlg(hwnd);
        OS2.WinDestroyWindow (hwnd);

    END Edit;

(**************************************************************************)

PROCEDURE SetINIFileName (name: ARRAY OF CHAR;  TNImode: BOOLEAN);

    (* Sets the INI file name and mode. *)

    BEGIN
        Strings.Assign (name, INIFileName);
        UseTNI := TNImode;
    END SetINIFileName;

(**************************************************************************)

BEGIN
    AdminINI := "Admin.INI";
    ChangeInProgress := FALSE;
END EditList.

