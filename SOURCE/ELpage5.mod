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

IMPLEMENTATION MODULE ELpage5;

        (************************************************************)
        (*                                                          *)
        (*              Admin program for MajorMajor                *)
        (*      The "owners" page of the "edit list" notebook       *)
        (*                                                          *)
        (*    Started:        11 January 2001                       *)
        (*    Last edited:    29 September 2019                     *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)


FROM SYSTEM IMPORT
    (* type *)  CARD8, ADDRESS,
    (* proc *)  CAST, ADR;

IMPORT OS2, DID, Strings, RINIData, OneLine, About, CommonSettings;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM MiscFuncs IMPORT
    (* type *)  CharArrayPointer;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM Names IMPORT
    (* type *)  FilenameString;

(************************************************************************)

CONST
    Nul = CHR(0);
    NameLength = 256;

VAR
    INIFileName: FilenameString;
    UseTNI: BOOLEAN;

    pagehandle, notebookhandle: OS2.HWND;
    PageID: CARDINAL;
    ListName: ARRAY [0..NameLength-1] OF CHAR;
    ListChanged, ChangeInProgress: BOOLEAN;
    ourlang: LangHandle;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        ourlang := lang;
        StrToBuffer (lang, "ELpage5.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(stringval));
        StrToBuffer (lang, "ELpage5.title", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.OwnersPageTitle, stringval);
        StrToBuffer (lang, "ELpage5.note1", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.OwnersNote1, stringval);
        StrToBuffer (lang, "ELpage5.note2", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.OwnersNote2, stringval);
        StrToBuffer (lang, "Lists.AddButton", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.AddOwner, stringval);
        StrToBuffer (lang, "ELpage4.ReviseButton", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.ReviseOwner, stringval);
        StrToBuffer (lang, "Lists.DeleteButton", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.DeleteOwner, stringval);
    END SetLanguage;

(**************************************************************************)
(*                        DIALOGUE PROCEDURES                             *)
(**************************************************************************)

PROCEDURE UpdateOwnerField;

    (* Checks for the obsolete "Moderator" and "ListOwner" keys, moves  *)
    (* value if any to "Owners".                                        *)

    VAR Name: ARRAY [0..NameLength-1] OF CHAR;

    BEGIN
        IF NOT RINIData.KeyExists (ListName, "Owners") THEN
            IF NOT RINIData.INIGetString (ListName, "ListOwner", Name) THEN
                IF NOT RINIData.INIGetString (ListName, "Moderator", Name) THEN
                    Name := "";
                END (*IF*);
            END (*IF*);
            RINIData.INIDeleteKey (ListName, "ListOwner");
            RINIData.INIDeleteKey (ListName, "Moderator");
            RINIData.INIPutString (ListName, "Owners", Name);
        END (*IF*);
    END UpdateOwnerField;

(************************************************************************)

PROCEDURE LoadExpansion (hwnd: OS2.HWND);

    (* Fills the owner box for the current list with data from the INI  *)
    (* file.                                                            *)

    VAR name: ARRAY [0..NameLength-1] OF CHAR;
        state: RINIData.StringReadState;

    BEGIN
        IF RINIData.OpenINIFile(INIFileName) THEN
            UpdateOwnerField;

            RINIData.GetStringList (ListName, "Owners", state);
            RINIData.NextString (state, name);
            WHILE name[0] <> Nul DO

                (* Add name to the listbox. *)

                OS2.WinSendDlgItemMsg (hwnd, DID.listownerbox, OS2.LM_INSERTITEM,
                         OS2.MPFROMSHORT(OS2.LIT_SORTASCENDING), ADR(name));
                RINIData.NextString (state, name);

            END (*WHILE*);

            RINIData.CloseStringList (state);
            RINIData.CloseINIFile;
        END (*IF*);

    END LoadExpansion;

(************************************************************************)

PROCEDURE StoreData (hwnd: OS2.HWND): BOOLEAN;

    (* Saves the list data back to the INI file.  The hwnd parameter    *)
    (* identifies the frame of this page.  Returns TRUE if any of the   *)
    (* data have changed.                                               *)

    VAR name: ARRAY [0..NameLength-1] OF CHAR;
        j, k, N, count: CARDINAL;
        bufptr: CharArrayPointer;
        BufferSize: CARDINAL;

    BEGIN
        IF ListChanged THEN

            (* Work out how much buffer space we need for the big list. *)

            BufferSize := 2;
            count := OS2.ULONGFROMMR(OS2.WinSendDlgItemMsg (hwnd, DID.listownerbox,
                                                            OS2.LM_QUERYITEMCOUNT, NIL, NIL));
            IF count > 0 THEN
                FOR N := 0 TO count-1 DO
                    INC (BufferSize,
                         OS2.ULONGFROMMR(OS2.WinSendDlgItemMsg (hwnd, DID.listownerbox,
                                                        OS2.LM_QUERYITEMTEXTLENGTH,
                                                        OS2.MPFROMUSHORT(N), NIL)) + 1);
                END (*FOR*);
            END (*IF*);

            (* Create the string buffer. *)

            ALLOCATE (bufptr, BufferSize);

            j := 0;

            (* Store the list of names. *)

            IF count > 0 THEN
                FOR N := 0 TO count-1 DO
                    OS2.WinSendDlgItemMsg (hwnd, DID.listownerbox, OS2.LM_QUERYITEMTEXT,
                                        OS2.MPFROM2SHORT(N, NameLength), ADR(name));
                    k := 0;
                    LOOP
                        IF j >= BufferSize THEN EXIT(*LOOP*); END(*IF*);
                        bufptr^[j] := name[k];
                        INC (j);
                        IF (k >= NameLength) OR (name[k] = Nul) THEN EXIT(*LOOP*) END(*IF*);
                        INC (k);
                    END (*LOOP*);

                END (*FOR*);
            END (*IF*);

            IF j < BufferSize THEN
                bufptr^[j] := Nul;  INC(j);
            END (*IF*);

            RINIData.INIPutBinary (ListName, "Owners", bufptr^, j);
            DEALLOCATE (bufptr, BufferSize);

        END (*IF ListChanged*);

        RETURN ListChanged;

    END StoreData;

(************************************************************************)
(*                         THE WINDOW DIALOGUE                          *)
(************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    (* Message handler for the setup dialogue. *)

    VAR ButtonID, NotificationCode: CARDINAL;
        index: INTEGER;
        listwindow: OS2.HWND;
        name: ARRAY [0..NameLength-1] OF CHAR;
        stringval: ARRAY [0..255] OF CHAR;

    BEGIN
        CASE msg OF
           |  OS2.WM_INITDLG:
                   OS2.WinSetWindowPos (hwnd, 0, 0, 0, 0, 0, OS2.SWP_MOVE);
                   LoadExpansion (hwnd);
                   OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.ReviseOwner), FALSE);
                   OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteOwner), FALSE);
                   RETURN NIL;

           |  OS2.WM_COMMAND:

                   listwindow := OS2.WinWindowFromID(hwnd,DID.listownerbox);
                   index := OS2.LONGFROMMR(
                             OS2.WinSendDlgItemMsg (hwnd, DID.listownerbox, OS2.LM_QUERYSELECTION, NIL, NIL));

                   CASE OS2.SHORT1FROMMP(mp1) OF

                     | DID.AddOwner:

                       name := "";
                       StrToBuffer (ourlang, "ELpage4.EnterName", stringval);
                       OneLine.Edit (hwnd, stringval, name, UseTNI);
                       IF name[0] <> Nul THEN
                           ListChanged := TRUE;
                           IF index = OS2.LIT_NONE THEN
                               index := 0;
                           ELSE
                               INC(index);
                           END (*IF*);
                           OS2.WinSendDlgItemMsg (hwnd, DID.listownerbox, OS2.LM_INSERTITEM,
                                  OS2.MPFROMSHORT(index), ADR(name));
                           OS2.WinSendDlgItemMsg (hwnd, DID.listownerbox, OS2.LM_SELECTITEM,
                                  OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));
                       END (*IF*);

                     | DID.ReviseOwner:

                       OS2.WinSendDlgItemMsg (hwnd, DID.listownerbox, OS2.LM_QUERYITEMTEXT,
                                OS2.MPFROM2SHORT(index, NameLength), ADR(name));
                       StrToBuffer (ourlang, "ELpage4.NewName", stringval);
                       OneLine.Edit (hwnd, stringval, name, UseTNI);
                       IF name[0] <> Nul THEN
                           ListChanged := TRUE;
                           OS2.WinSendDlgItemMsg (hwnd, DID.listownerbox, OS2.LM_SETITEMTEXT,
                                  OS2.MPFROMSHORT(index), ADR(name));
                       END (*IF*);

                     | DID.DeleteOwner:

                       ListChanged := TRUE;
                       OS2.WinSendDlgItemMsg (hwnd, DID.listownerbox, OS2.LM_DELETEITEM,
                                              OS2.MPFROMSHORT(index), NIL);
                       OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.ReviseOwner), FALSE);
                       OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteOwner), FALSE);

                   ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*CASE*);
                   RETURN NIL;

           |  OS2.WM_CONTROL:

                   NotificationCode := OS2.ULONGFROMMP(mp1);
                   ButtonID := NotificationCode MOD 65536;
                   NotificationCode := NotificationCode DIV 65536;
                   IF ButtonID = DID.listownerbox THEN
                       IF NotificationCode = OS2.LN_SELECT THEN
                           OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.ReviseOwner), TRUE);
                           OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteOwner), TRUE);
                           RETURN NIL;
                       ELSIF NotificationCode = OS2.LN_ENTER THEN
                           (* Treat this one as if the revise button had been clicked. *)
                           OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                                 OS2.MPFROMSHORT(DID.ReviseOwner), NIL);
                           RETURN NIL;
                       ELSE
                           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                       END (*IF*);
                   ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
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
                       DID.ELpage5,                (* dialogue ID *)
                       NIL);                 (* creation parameters *)
        ListChanged := FALSE;
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

PROCEDURE SetINIFileName (name: ARRAY OF CHAR);

    (* Sets the INI file name. *)

    BEGIN
        Strings.Assign (name, INIFileName);
    END SetINIFileName;

(**************************************************************************)

BEGIN
    ChangeInProgress := FALSE;
END ELpage5.

