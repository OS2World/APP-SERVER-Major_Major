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

IMPLEMENTATION MODULE FilesPage;

        (****************************************************************)
        (*                                                              *)
        (*                  Admin program for MajorMajor                *)
        (*          Logging and archiving page of the notebook          *)
        (*                                                              *)
        (*    Started:        9 February 2009                           *)
        (*    Last edited:    10 February 2009                          *)
        (*    Status:         OK                                        *)
        (*                                                              *)
        (****************************************************************)


IMPORT Conversions, PMInit;    (* while debugging *)

FROM SYSTEM IMPORT ADDRESS, CARD8, INT16, CAST, ADR;

IMPORT OS2, OS2RTL, CommonSettings, ELpage6;

IMPORT DID, Strings;

FROM Remote IMPORT
    (* proc *)  OurIPAddress;

FROM RINIData IMPORT
    (* proc *)  OpenINIFile, CloseINIFile, INIFetch, INIPut,
                INIGetString, INIPutString, INIDeleteKey,
                OurDirectory, RemoteOperation;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM Sockets IMPORT
    (* const*)  AF_INET;

FROM NetDB IMPORT
    (* type *)  HostEntPtr,
    (* proc *)  gethostname, gethostbyname, gethostbyaddr;

FROM Inet2Misc IMPORT
    (* proc *)  EVAL, IPToString;

FROM Names IMPORT
    (* type *)  FilenameString;

(**************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    DirectoryString = ARRAY [0..511] OF CHAR;

VAR
    INIFileName: FilenameString;
    UseTNI: BOOLEAN;

    pagehandle, notebookhandle: OS2.HWND;
    PageID: CARDINAL;
    ourlanguage: LangHandle;

    ChangeInProgress: BOOLEAN;

    (* Original values of the ini variables. *)

    OldTransLevel: CARD8;
    OldTransLogFile: FilenameString;
    OldArchiveDir: FilenameString;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        ourlanguage := lang;
        StrToBuffer (lang, "Files.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(stringval));
        StrToBuffer (lang, "Files.enablelogging", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.TranslogBox, stringval);
        StrToBuffer (lang, "Files.translogto", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.TranslogLabel, stringval);
        StrToBuffer (lang, "Files.disk", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.DiskLog, stringval);
        StrToBuffer (lang, "Files.screen", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.ScreenLog, stringval);
        StrToBuffer (lang, "Files.logfilename", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.TransLogFileLabel, stringval);
        StrToBuffer (lang, "Files.archivedir", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.ArchiveDirLabel, stringval);
    END SetLanguage;

(************************************************************************)
(*                     LOADING DATA FROM THE INI FILE                   *)
(************************************************************************)

PROCEDURE LoadValues (hwnd: OS2.HWND);

    (* Fills the dialogue elements on page 1 with data from the INI file,       *)
    (* or loads default values if they're not in the INI file.                  *)

    VAR val0: CARD8;
        opened: BOOLEAN;

    BEGIN
        opened := OpenINIFile (INIFileName, UseTNI);

        (* Transaction logging. *)

        IF opened AND INIFetch ('$SYS', 'TransLevel', val0) THEN
            OldTransLevel := val0;
        ELSE
            val0 := 2;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.DiskLog, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(ODD(val0))), NIL);
        OS2.WinSendDlgItemMsg (hwnd, DID.ScreenLog, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(val0>1)), NIL);

        (* Log file name. *)

        IF opened AND INIGetString ('$SYS', 'TransLogFile', OldTransLogFile) THEN
            OS2.WinSetDlgItemText (hwnd, DID.TransLogFile, OldTransLogFile);
        ELSE
            OldTransLogFile := "";
            OS2.WinSetDlgItemText (hwnd, DID.TransLogFile, "MAJOR.LOG");
        END (*IF*);
        IF ODD(val0) THEN
            OS2.WinEnableWindow(
                  OS2.WinWindowFromID(hwnd, DID.TransLogFile), TRUE);
        ELSE
            OS2.WinEnableWindow(
                  OS2.WinWindowFromID(hwnd, DID.TransLogFile), FALSE);
        END (*IF*);

        (* Archive directory. *)

        IF opened AND INIGetString ('$SYS', 'ArchiveDir', OldArchiveDir) THEN
            OS2.WinSetDlgItemText (hwnd, DID.ArchiveDir, OldArchiveDir);
        ELSE
            OldArchiveDir := "";
            OS2.WinSetDlgItemText (hwnd, DID.ArchiveDir, "archives");
        END (*IF*);

        IF opened THEN
            CloseINIFile;
        END (*IF*);

    END LoadValues;

(************************************************************************)
(*                      STORING DATA TO THE INI FILE                    *)
(************************************************************************)

PROCEDURE QueryButton (hwnd: OS2.HWND;  B: CARDINAL): CARDINAL;

    BEGIN
        RETURN OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, B,
                                              OS2.BM_QUERYCHECK, NIL, NIL));
    END QueryButton;

(**************************************************************************)

PROCEDURE StoreData (hwnd: OS2.HWND);

    (* Stores the values on page 1 back into the INI file.  *)

    VAR opened: BOOLEAN;
        val8: CARD8;  N: CARDINAL;
        stringval: DirectoryString;

    BEGIN
        opened := OpenINIFile (INIFileName, UseTNI);

        (* Transaction logging. *)

        val8 := QueryButton (hwnd, DID.DiskLog)
                  + 2 * QueryButton (hwnd, DID.ScreenLog);
        IF val8 <> OldTransLevel THEN
            INIPut ('$SYS', 'TransLevel', val8);
        END (*IF*);

        (* Log file name. *)

        IF ODD(val8) THEN
            OS2.WinQueryDlgItemText (hwnd, DID.TransLogFile, 512, stringval);
        ELSE
            stringval[0] := Nul;
        END (*IF*);
        IF NOT Strings.Equal (stringval, OldTransLogFile) THEN
            INIPutString ("$SYS", "TransLogFile", stringval);
        END (*IF*);

        (* Archive directory. *)

        OS2.WinQueryDlgItemText (hwnd, DID.ArchiveDir, 512, stringval);
        IF NOT Strings.Equal (stringval, OldArchiveDir) THEN
            N := LENGTH(stringval);
            IF N > 0 THEN
                DEC(N);
            END (*IF*);
            WHILE (stringval[N] = '\') OR (stringval[N] = '/') DO
                stringval[N] := Nul;
                IF N > 0 THEN
                    DEC(N);
                END (*IF*);
            END (*WHILE*);
            INIPutString ("$SYS", "ArchiveDir", stringval);
        END (*IF*);

        IF opened THEN
            CloseINIFile;
        END (*IF*);

    END StoreData;

(************************************************************************)
(*                        DIALOGUE PROCEDURE                            *)
(************************************************************************)

PROCEDURE ["SysCall"] DialogueProc (hwnd: OS2.HWND;  msg: OS2.ULONG;
                                      mp1, mp2: OS2.MPARAM): OS2.MRESULT;

    VAR ControlID, NotificationCode: CARDINAL;
        stringval: FilenameString;

    BEGIN
        IF msg = OS2.WM_INITDLG THEN

            LoadValues (hwnd);
            RETURN NIL;

        ELSIF msg = OS2.WM_PRESPARAMCHANGED THEN

            IF ChangeInProgress THEN
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            ELSE
                ChangeInProgress := TRUE;
                CommonSettings.UpdateFontFrom (hwnd, CommonSettings.MainNotebook);
                ChangeInProgress := FALSE;
                RETURN NIL;
            END (*IF*);

        ELSIF msg = OS2.WM_CONTROL THEN

            NotificationCode := OS2.ULONGFROMMP(mp1);
            ControlID := NotificationCode MOD 65536;
            NotificationCode := NotificationCode DIV 65536;
            IF (NotificationCode = OS2.BN_CLICKED)
                           AND (ControlID = DID.DiskLog) THEN
                IF QueryButton(hwnd,DID.DiskLog) > 0 THEN
                    OS2.WinEnableWindow(
                          OS2.WinWindowFromID(hwnd, DID.TransLogFile), TRUE);
                ELSE
                    OS2.WinEnableWindow(
                          OS2.WinWindowFromID(hwnd, DID.TransLogFile), FALSE);
                END (*IF*);
                RETURN NIL;
            ELSIF (NotificationCode = OS2.EN_CHANGE)
                           AND (ControlID = DID.ArchiveDir) THEN
                OS2.WinQueryDlgItemText (hwnd, DID.ArchiveDir, 512, stringval);
                ELpage6.SetArchiveRoot (stringval);
            ELSE
                RETURN OS2.WinDefDlgProc (hwnd, msg, mp1, mp2);
            END (*IF*);

        END (*CASE*);

        RETURN OS2.WinDefDlgProc (hwnd, msg, mp1, mp2);

    END DialogueProc;

(**************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND): OS2.HWND;

    (* Creates the files page and adds it to the notebook. *)

    VAR Label: ARRAY [0..31] OF CHAR;

    BEGIN
        notebookhandle := notebook;
        pagehandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,        (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.FilesPage,       (* dialogue ID *)
                       NIL);                (* creation parameters *)
        PageID := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         NIL,
                         OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE,
                                           OS2.BKA_LAST)));
        Label := "Files";
        OS2.WinSendMsg (notebook, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(Label));
        OS2.WinSendMsg (notebook, OS2.BKM_SETPAGEWINDOWHWND,
                        CAST(ADDRESS,PageID), CAST(ADDRESS,pagehandle));
        OS2.WinSetWindowPos (pagehandle, 0, 0, 0, 0, 0, OS2.SWP_MOVE);
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
    OldTransLevel := 0;
END FilesPage.

