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

IMPLEMENTATION MODULE ListsPage;

        (****************************************************************)
        (*                                                              *)
        (*                Admin program for MajorMajor                  *)
        (*           The "lists" page of the settings notebook          *)
        (*                                                              *)
        (*    Started:        16 June 2000                              *)
        (*    Last edited:    29 September 2019                         *)
        (*    Status:         OK                                        *)
        (*                                                              *)
        (****************************************************************)


FROM SYSTEM IMPORT ADDRESS, CAST, ADR;

IMPORT OS2, OS2RTL, DID, Strings, EditList, OneLine, CommonSettings;

FROM MiscFuncs IMPORT
    (* type *)  CharArrayPointer;

FROM RINIData IMPORT
    (* type *)  StringReadState,
    (* proc *)  OpenINIFile, CloseINIFile,
                INIDeleteApp, INIRenameApp,
                GetStringList, NextString, CloseStringList;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM Names IMPORT
    (* type *)  FilenameString;

(**************************************************************************)

CONST
    Nul = CHR(0);
    NameLength = 256;

    semName = "\SEM32\MAJOR\UPDATED";

VAR
    INIFileName: FilenameString;
    UseTNI: BOOLEAN;

    pagehandle, notebookhandle: OS2.HWND;
    OurPageID: CARDINAL;
    changehev: OS2.HEV;
    ChangeInProgress: BOOLEAN;
    NewStyle: BOOLEAN;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        StrToBuffer (lang, "Lists.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,OurPageID), ADR(stringval));
        StrToBuffer (lang, "Lists.title", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.ListsTitle, stringval);
        StrToBuffer (lang, "Lists.AddButton", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.AddListButton, stringval);
        StrToBuffer (lang, "Lists.EditButton", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.EditListButton, stringval);
        StrToBuffer (lang, "Lists.RenameButton", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.RenameListButton, stringval);
        StrToBuffer (lang, "Lists.DeleteButton", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.DeleteListButton, stringval);
    END SetLanguage;

(**************************************************************************)
(*                        DIALOGUE PROCEDURES                             *)
(**************************************************************************)

PROCEDURE LoadValues (hwnd: OS2.HWND);

    (* Fills the listbox with all list names in the INI file.  *)

    VAR name: ARRAY [0..NameLength-1] OF CHAR;
        state: StringReadState;

    BEGIN
        IF OpenINIFile (INIFileName) THEN

            (* Pick up the list of all list names. *)

            GetStringList ("", "", state);

            LOOP
                NextString (state, name);
                IF name[0] = Nul THEN
                    EXIT (*LOOP*);
                END (*IF*);

                IF name[0] <> '$' THEN

                    (* Add name to the listbox. *)

                    OS2.WinSendDlgItemMsg (hwnd, DID.ListList, OS2.LM_INSERTITEM,
                             OS2.MPFROMSHORT(OS2.LIT_SORTASCENDING), ADR(name));

                END (*IF*);

            END (*LOOP*);

            CloseStringList (state);
            CloseINIFile;
        END (*IF*);

    END LoadValues;

(**************************************************************************)

PROCEDURE StoreData (hwnd: OS2.HWND);

    (* A do-nothing procedure - INI file updating is done as part of    *)
    (* the button processing.                                           *)

    BEGIN
    END StoreData;

(************************************************************************)

PROCEDURE NotifyAChange;

    (* Posts a public event semaphore to say that there has been a      *)
    (* change in our INI data.                                          *)

    BEGIN
        OS2.DosPostEventSem (changehev);
    END NotifyAChange;

(************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    VAR ButtonID, NotificationCode: CARDINAL;
        index: INTEGER;
        lang: LangHandle;
        listwindow: OS2.HWND;
        oldname, name: ARRAY [0..NameLength-1] OF CHAR;
        stringval: ARRAY [0..255] OF CHAR;

    BEGIN
        IF msg = OS2.WM_INITDLG THEN
            OS2.WinSetWindowPos (hwnd, 0, 0, 0, 0, 0, OS2.SWP_MOVE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.EditListButton), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.RenameListButton), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteListButton), FALSE);
            LoadValues (hwnd);
            RETURN NIL;

        ELSIF msg = OS2.WM_COMMAND THEN

            CommonSettings.CurrentLanguage (lang, stringval);
            listwindow := OS2.WinWindowFromID(hwnd,DID.ListList);
            index := OS2.LONGFROMMR(
                      OS2.WinSendDlgItemMsg (hwnd, DID.ListList, OS2.LM_QUERYSELECTION, NIL, NIL));
            OS2.WinSendDlgItemMsg (hwnd, DID.ListList, OS2.LM_QUERYITEMTEXT,
                          OS2.MPFROM2SHORT(index, NameLength), ADR(name));
            CASE OS2.SHORT1FROMMP(mp1) OF

              | DID.AddListButton:
                   name := "";
                   StrToBuffer (lang, "Lists.EnterName", stringval);
                   OneLine.Edit (hwnd, stringval, name, UseTNI);
                   IF name[0] <> Nul THEN
                       IF index = OS2.LIT_NONE THEN
                           index := 0;
                       ELSE
                           INC(index);
                       END (*IF*);
                       OS2.WinSendDlgItemMsg (hwnd, DID.ListList, OS2.LM_INSERTITEM,
                              OS2.MPFROMSHORT(index), ADR(name));
                       OS2.WinSendDlgItemMsg (hwnd, DID.ListList, OS2.LM_SELECTITEM,
                              OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));
                       EditList.Edit(listwindow, name, lang, NewStyle);
                       NotifyAChange;
                   END (*IF*);

              | DID.EditListButton:
                   EditList.Edit(listwindow, name, lang, NewStyle);
                   NotifyAChange;

              | DID.RenameListButton:
                   OS2.WinSendMsg (listwindow, OS2.LM_QUERYITEMTEXT,
                                   OS2.MPFROM2USHORT(index, 32), ADR(name));
                   Strings.Assign (name, oldname);
                   StrToBuffer (lang, "Lists.NewName", stringval);
                   OneLine.Edit (hwnd, stringval, name, UseTNI);
                   IF NOT Strings.Equal (name, oldname) THEN
                       IF OpenINIFile (INIFileName) THEN
                           INIRenameApp (oldname, name);
                           CloseINIFile;
                       END (*IF*);
                       OS2.WinSendMsg (listwindow, OS2.LM_SETITEMTEXT,
                                   OS2.MPFROMUSHORT(index), ADR(name));
                       NotifyAChange;
                   END (*IF*);

              | DID.DeleteListButton:
                   OS2.WinSendMsg (listwindow, OS2.LM_QUERYITEMTEXT,
                                   OS2.MPFROM2USHORT(index, 32), ADR(name));
                   IF OpenINIFile (INIFileName) THEN
                       INIDeleteApp (name);
                       CloseINIFile;
                   END (*IF*);
                   OS2.WinSendDlgItemMsg (hwnd, DID.ListList, OS2.LM_DELETEITEM,
                                          OS2.MPFROMSHORT(index), NIL);
                   OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.EditListButton), FALSE);
                   OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.RenameListButton), FALSE);
                   OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteListButton), FALSE);
                   NotifyAChange;

            END (*CASE*);
            RETURN NIL;

        ELSIF msg = OS2.WM_CONTROL THEN

            NotificationCode := OS2.ULONGFROMMP(mp1);
            ButtonID := NotificationCode MOD 65536;
            NotificationCode := NotificationCode DIV 65536;
            IF ButtonID = DID.ListList THEN
                IF NotificationCode = OS2.LN_SELECT THEN
                    OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.EditListButton), TRUE);
                    OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.RenameListButton), TRUE);
                    OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteListButton), TRUE);
                    RETURN NIL;
                ELSIF NotificationCode = OS2.LN_ENTER THEN
                    (* Treat this one as if the edit button had been clicked. *)
                    OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                          OS2.MPFROMSHORT(DID.EditListButton), NIL);
                    RETURN NIL;
                ELSE
                    RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                END (*IF*);
            ELSE
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            END (*IF*);

        ELSIF msg = OS2.WM_PRESPARAMCHANGED THEN

            IF ChangeInProgress THEN
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            ELSE
                ChangeInProgress := TRUE;
                CommonSettings.UpdateFontFrom (hwnd, CommonSettings.MainNotebook);
                ChangeInProgress := FALSE;
                RETURN NIL;
            END (*IF*);

        ELSE
            RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);
    END DialogueProc;

(**************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND;  W4style: BOOLEAN;
                                VAR (*OUT*) PageID: CARDINAL): OS2.HWND;

    (* Creates the alias page and adds it to the notebook. *)

    VAR Label: ARRAY [0..31] OF CHAR;

    BEGIN
        NewStyle := W4style;
        notebookhandle := notebook;
        pagehandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.MailingLists,                (* dialogue ID *)
                       NIL);                 (* creation parameters *)
        OurPageID := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         NIL, OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE, OS2.BKA_LAST)));
        PageID := OurPageID;
        Label := "Lists";
        OS2.WinSendMsg (notebook, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(Label));
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

PROCEDURE SetINIFileName (name: ARRAY OF CHAR);

    (* Sets the INI file name. *)

    BEGIN
        Strings.Assign (name, INIFileName);
        EditList.SetINIFileName (name);
    END SetINIFileName;

(**************************************************************************)

BEGIN
    NewStyle := TRUE;
    ChangeInProgress := FALSE;
    changehev := 0;
    IF OS2.DosOpenEventSem (semName, changehev) = OS2.ERROR_SEM_NOT_FOUND THEN
        OS2.DosCreateEventSem (semName, changehev, OS2.DC_SEM_SHARED, FALSE);
    END (*IF*);
FINALLY
    OS2.DosCloseEventSem(changehev);
END ListsPage.

