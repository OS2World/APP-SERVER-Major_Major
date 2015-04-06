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

IMPLEMENTATION MODULE Dialp3;

        (************************************************************)
        (*                                                          *)
        (*              Admin program for MajorMajor                *)
        (*              Page 3 of the admin notebook                *)
        (*                                                          *)
        (*    Started:        5 September 2000                      *)
        (*    Last edited:    8 February 2009                       *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)


FROM SYSTEM IMPORT
    (* type *)  CARD8, ADDRESS,
    (* proc *)  CAST, ADR;

IMPORT OS2, DID, Strings, RINIData, CommonSettings;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM Names IMPORT
    (* type *)  FilenameIndex, FilenameString;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

VAR
    INIFileName: FilenameString;
    UseTNI: BOOLEAN;

    pagehandle, notebookhandle: OS2.HWND;
    PageID: CARDINAL;
    OldNotSubscribedMessage: FilenameString;
    OldHelpFile: FilenameString;
    OldPlainTextFile: FilenameString;
    ChangeInProgress: BOOLEAN;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        StrToBuffer (lang, "Messages.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(stringval));
        StrToBuffer (lang, "Messages.title", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.MessagesTitle, stringval);
        StrToBuffer (lang, "Messages.nonsub", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.MessagesNonsubLabel, stringval);
        StrToBuffer (lang, "Messages.help", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.MessagesHelpLabel, stringval);
        StrToBuffer (lang, "Messages.notplaintext", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.MessagesNotPlainLabel, stringval);
    END SetLanguage;

(************************************************************************)
(*                    LOADING AND STORING INI DATA                      *)
(************************************************************************)

PROCEDURE LoadData (hwnd: OS2.HWND);

    (* Fills the dialogue windows with data from the INI file, or       *)
    (* loads default values if they're not in the INI file.             *)

    VAR stringval: FilenameString;
        opened: BOOLEAN;

    BEGIN
        opened := RINIData.OpenINIFile (INIFileName, UseTNI);

        (* "You are not subscribed" message. *)

        IF opened AND RINIData.INIGetString ('$SYS', 'NotSubscribed', stringval) THEN
            Strings.Assign (stringval, OldNotSubscribedMessage);
        ELSE
            stringval := "Canned\%L\NotSubscribed.txt";
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.NotSubscribed, stringval);

        (* Response to help command. *)

        IF opened AND RINIData.INIGetString ('$SYS', 'HelpFile', stringval) THEN
            Strings.Assign (stringval, OldHelpFile);
        ELSE
            stringval := "Canned\%L\DefaultHelp.txt";
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.HelpFile, stringval);

        (* Response to mail not in plain text format. *)

        IF opened AND RINIData.INIGetString ('$SYS', 'PlainTextFile', stringval) THEN
            Strings.Assign (stringval, OldPlainTextFile);
        ELSE
            stringval := "Canned\%L\DefaultPlainTextFile.txt";
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.PlainTextFile, stringval);

        IF opened THEN
            RINIData.CloseINIFile;
        END (*IF*);

    END LoadData;

(************************************************************************)

PROCEDURE StoreData (hwnd: OS2.HWND);

    (* Saves the list data back to the INI file.  The hwnd parameter    *)
    (* identifies the frame of this page.                               *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        IF RINIData.OpenINIFile (INIFileName, UseTNI) THEN

            (* "Not subscribed" message. *)

            OS2.WinQueryDlgItemText (hwnd, DID.NotSubscribed, MAX(FilenameIndex)+1, stringval);
            IF NOT Strings.Equal (stringval, OldNotSubscribedMessage) THEN
                RINIData.INIPutString ('$SYS', 'NotSubscribed', stringval);
            END (*IF*);

            (* Response to HELP command. *)

            OS2.WinQueryDlgItemText (hwnd, DID.HelpFile, MAX(FilenameIndex)+1, stringval);
            IF NOT Strings.Equal (stringval, OldHelpFile) THEN
                RINIData.INIPutString ('$SYS', 'HelpFile', stringval);
            END (*IF*);

            (* Response to mail not in plain text format. *)

            OS2.WinQueryDlgItemText (hwnd, DID.PlainTextFile, MAX(FilenameIndex)+1, stringval);
            IF NOT Strings.Equal (stringval, OldPlainTextFile) THEN
                RINIData.INIPutString ('$SYS', 'PlainTextFile', stringval);
            END (*IF*);

            RINIData.CloseINIFile;
        END (*IF*);

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
            LoadData (hwnd);
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

        ELSE
            RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);
    END DialogueProc;

(************************************************************************)

PROCEDURE Create (notebook: OS2.HWND): OS2.HWND;

    (* Creates page 3 and adds it to the notebook. *)

    VAR Label: ARRAY [0..31] OF CHAR;

    BEGIN
        notebookhandle := notebook;
        pagehandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.MessagePage,              (* dialogue ID *)
                       NIL);                 (* creation parameters *)
        PageID := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         NIL, OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE, OS2.BKA_LAST)));
        Label := "Messages";
        OS2.WinSendMsg (notebook, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(Label));
        OS2.WinSendMsg (notebook, OS2.BKM_SETPAGEWINDOWHWND,
                        CAST(ADDRESS,PageID), CAST(ADDRESS,pagehandle));
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

PROCEDURE SetINIFileName (name: ARRAY OF CHAR;  TNImode: BOOLEAN);

    (* Sets the INI file name and mode. *)

    BEGIN
        Strings.Assign (name, INIFileName);
        UseTNI := TNImode;
    END SetINIFileName;

(**************************************************************************)

BEGIN
    OldNotSubscribedMessage := "";
    OldHelpFile := "";
    OldPlainTextFile := "";
    ChangeInProgress := FALSE;
END Dialp3.

