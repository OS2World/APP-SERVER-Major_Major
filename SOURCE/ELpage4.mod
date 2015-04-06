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

IMPLEMENTATION MODULE ELpage4;

        (************************************************************)
        (*                                                          *)
        (*              Admin program for MajorMajor                *)
        (*      The "members" page of the "edit list" notebook      *)
        (*                                                          *)
        (*    Started:        25 August 2000                        *)
        (*    Last edited:    5 April 2010                          *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)


FROM SYSTEM IMPORT
    (* type *)  CARD8, ADDRESS,
    (* proc *)  CAST, ADR;

IMPORT OS2, DID, Strings, RINIData, OneLine, About, CommonSettings;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer, StrToBufferN;

FROM Inet2Misc IMPORT
    (* type *)  CharArrayPointer,
    (* proc *)  ConvertCard;

FROM Names IMPORT
    (* type *)  FilenameString;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

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
    MemberCount: CARDINAL;
    ourlang: LangHandle;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE DisplayMemberCount (hwnd: OS2.HWND);

    (* Displays the value of global variable MemberCount. *)

    VAR buffer: ARRAY [0..63] OF CHAR;

    BEGIN
        StrToBufferN (ourlang, "ELpage4.total", MemberCount, buffer);
        OS2.WinSetDlgItemText (hwnd, DID.MemberCount, buffer);
    END DisplayMemberCount;

(**************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        ourlang := lang;
        StrToBuffer (lang, "ELpage4.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(stringval));
        StrToBuffer (lang, "ELpage4.title", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.MembersPageTitle, stringval);
        StrToBuffer (lang, "Lists.AddButton", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.Addmember, stringval);
        StrToBuffer (lang, "ELpage4.ReviseButton", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.Revisemember, stringval);
        StrToBuffer (lang, "Lists.DeleteButton", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.Deletemember, stringval);
        DisplayMemberCount (pagehandle);
    END SetLanguage;

(**************************************************************************)
(*                        DIALOGUE PROCEDURES                             *)
(**************************************************************************)

PROCEDURE LoadNamesFromKey (hwnd: OS2.HWND;  key: ARRAY OF CHAR);

    (* Adds entries to the namebox for the current list from the "key"  *)
    (* section of the INI file.                                         *)

    VAR name: ARRAY [0..NameLength-1] OF CHAR;
        state: RINIData.StringReadState;

    BEGIN
        RINIData.GetStringList (ListName, key, state);
        RINIData.NextString (state, name);
        WHILE name[0] <> Nul DO

            (* Add name to the listbox. *)

            OS2.WinSendDlgItemMsg (hwnd, DID.namebox, OS2.LM_INSERTITEM,
                     OS2.MPFROMSHORT(OS2.LIT_SORTASCENDING), ADR(name));
            INC (MemberCount);
            RINIData.NextString (state, name);

        END (*WHILE*);

        RINIData.CloseStringList (state);

    END LoadNamesFromKey;

(************************************************************************)

PROCEDURE LoadExpansion (hwnd: OS2.HWND);

    (* Fills the namebox for the current list with data from the INI    *)
    (* file.                                                            *)

    BEGIN
        MemberCount := 0;
        IF RINIData.OpenINIFile(INIFileName, UseTNI) THEN
            LoadNamesFromKey (hwnd, "names");
            LoadNamesFromKey (hwnd, "Members");
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
            count := OS2.ULONGFROMMR(OS2.WinSendDlgItemMsg (hwnd, DID.namebox,
                                                            OS2.LM_QUERYITEMCOUNT, NIL, NIL));
            IF count > 0 THEN
                FOR N := 0 TO count-1 DO
                    INC (BufferSize,
                         OS2.ULONGFROMMR(OS2.WinSendDlgItemMsg (hwnd, DID.namebox,
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
                    OS2.WinSendDlgItemMsg (hwnd, DID.namebox, OS2.LM_QUERYITEMTEXT,
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

            RINIData.INIPutBinary (ListName, "Members", bufptr^, j);
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
                   DisplayMemberCount (hwnd);
                   OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.Revisemember), FALSE);
                   OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.Deletemember), FALSE);
                   RETURN NIL;

           |  OS2.WM_COMMAND:

                   listwindow := OS2.WinWindowFromID(hwnd,DID.namebox);
                   index := OS2.LONGFROMMR(
                             OS2.WinSendDlgItemMsg (hwnd, DID.namebox, OS2.LM_QUERYSELECTION, NIL, NIL));

                   CASE OS2.SHORT1FROMMP(mp1) OF

                     | DID.Addmember:

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
                           OS2.WinSendDlgItemMsg (hwnd, DID.namebox, OS2.LM_INSERTITEM,
                                  OS2.MPFROMSHORT(index), ADR(name));
                           OS2.WinSendDlgItemMsg (hwnd, DID.namebox, OS2.LM_SELECTITEM,
                                  OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));
                           INC (MemberCount);
                           DisplayMemberCount (hwnd);
                       END (*IF*);

                     | DID.Revisemember:

                       OS2.WinSendDlgItemMsg (hwnd, DID.namebox, OS2.LM_QUERYITEMTEXT,
                                OS2.MPFROM2SHORT(index, NameLength), ADR(name));
                       StrToBuffer (ourlang, "ELpage4.NewName", stringval);
                       OneLine.Edit (hwnd, stringval, name, UseTNI);
                       IF name[0] <> Nul THEN
                           ListChanged := TRUE;
                           OS2.WinSendDlgItemMsg (hwnd, DID.namebox, OS2.LM_SETITEMTEXT,
                                  OS2.MPFROMSHORT(index), ADR(name));
                       END (*IF*);

                     | DID.Deletemember:

                       ListChanged := TRUE;
                       OS2.WinSendDlgItemMsg (hwnd, DID.namebox, OS2.LM_DELETEITEM,
                                              OS2.MPFROMSHORT(index), NIL);
                       OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.Revisemember), FALSE);
                       OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.Deletemember), FALSE);
                       DEC (MemberCount);
                       DisplayMemberCount (hwnd);

                   ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*CASE*);
                   RETURN NIL;

           |  OS2.WM_CONTROL:

                   NotificationCode := OS2.ULONGFROMMP(mp1);
                   ButtonID := NotificationCode MOD 65536;
                   NotificationCode := NotificationCode DIV 65536;
                   IF ButtonID = DID.namebox THEN
                       IF NotificationCode = OS2.LN_SELECT THEN
                           OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.Revisemember), TRUE);
                           OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.Deletemember), TRUE);
                           RETURN NIL;
                       ELSIF NotificationCode = OS2.LN_ENTER THEN
                           (* Treat this one as if the revise button had been clicked. *)
                           OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                                 OS2.MPFROMSHORT(DID.Revisemember), NIL);
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
                       DID.ELpage4,                (* dialogue ID *)
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

PROCEDURE SetINIFileName (name: ARRAY OF CHAR;  TNImode: BOOLEAN);

    (* Sets the INI file name and mode. *)

    BEGIN
        Strings.Assign (name, INIFileName);
        UseTNI := TNImode;
    END SetINIFileName;

(**************************************************************************)

BEGIN
    ChangeInProgress := FALSE;
END ELpage4.

