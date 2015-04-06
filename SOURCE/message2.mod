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

IMPLEMENTATION MODULE Message2;

        (************************************************************)
        (*                                                          *)
        (*              Admin program for MajorMajor                *)
        (*     The "messages 2" page of the "edit list" notebook    *)
        (*                                                          *)
        (*    Started:        9 June 2004                           *)
        (*    Last edited:    10 March 2012                         *)
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
    OldWelcomeMessage, OldDepartureMessage,
                           OldLeaderName, OldFooterName: FilenameString;
    ChangeInProgress: BOOLEAN;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        StrToBuffer (lang, "ELMessage.tab2", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(stringval));
        StrToBuffer (lang, "ELMessage.title", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.MessagePage2Title, stringval);
        StrToBuffer (lang, "ELMessage.welcome", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.WelcomeLabel, stringval);
        StrToBuffer (lang, "ELMessage.departure", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.DepartureLabel, stringval);
        StrToBuffer (lang, "ELMessage.leader", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.LeaderLabel, stringval);
        StrToBuffer (lang, "ELMessage.footer", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.FooterLabel, stringval);
    END SetLanguage;

(**************************************************************************)
(*                        DIALOGUE PROCEDURES                             *)
(**************************************************************************)

PROCEDURE LoadListData (hwnd: OS2.HWND);

    (* Fills the dialogue windows with data from the INI file, or       *)
    (* loads default values if they're not in the INI file.             *)

    VAR opened: BOOLEAN;  name: FilenameString;

    BEGIN
        opened := RINIData.OpenINIFile(INIFileName, UseTNI);

        (* Welcome message. *)

        IF opened AND RINIData.INIGetString (ListName, "WelcomeMessage", name) THEN
            OldWelcomeMessage := name;
        ELSE
            Strings.Assign ("Canned\%L\SampleWelcomeMessage.txt", name);
            OldWelcomeMessage[0] := Nul;
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.WelcomeMessage, name);

        (* Departure message. *)

        IF opened AND RINIData.INIGetString (ListName, "DepartureMessage", name) THEN
            OldDepartureMessage := name;
        ELSE
            Strings.Assign ("", name);
            OldDepartureMessage[0] := Nul;
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.DepartureMessage, name);

        (* Leader file. *)

        IF opened AND RINIData.INIGetString (ListName, "Leader", name) THEN
            OldLeaderName := name;
        ELSE
            Strings.Assign ("Canned\%L\SampleLeaderFile.txt", name);
            OldLeaderName[0] := Nul;
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.LeaderName, name);

        (* Footer file. *)

        IF opened AND RINIData.INIGetString (ListName, "Trailer", name) THEN
            OldFooterName := name;
        ELSE
            Strings.Assign ("Canned\%L\HowToUnsubscribe.txt", name);
            OldFooterName[0] := Nul;
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.FooterName, name);

        IF opened THEN
            RINIData.CloseINIFile;
        END (*IF*);

    END LoadListData;

(************************************************************************)

PROCEDURE StoreData (hwnd: OS2.HWND): BOOLEAN;

    (* Saves the list data back to the INI file.  The hwnd parameter    *)
    (* identifies the frame of this page.  Returns TRUE if any of the   *)
    (* data have changed.                                               *)

    VAR stringval: ARRAY [0..511] OF CHAR;
        changed: BOOLEAN;

    BEGIN
        changed := FALSE;

        (* Welcome message. *)

        OS2.WinQueryDlgItemText (hwnd, DID.WelcomeMessage, 512, stringval);
        IF NOT Strings.Equal (stringval, OldWelcomeMessage) THEN
            changed := TRUE;
            RINIData.INIPutString (ListName, "WelcomeMessage", stringval);
        END (*IF*);

        (* Departure message. *)

        OS2.WinQueryDlgItemText (hwnd, DID.DepartureMessage, 512, stringval);
        IF NOT Strings.Equal (stringval, OldDepartureMessage) THEN
            RINIData.INIPutString (ListName, "DepartureMessage", stringval);
            changed := TRUE;
        END (*IF*);

        (* Leader file. *)

        OS2.WinQueryDlgItemText (hwnd, DID.LeaderName, 512, stringval);
        IF NOT Strings.Equal (stringval, OldLeaderName) THEN
            RINIData.INIPutString (ListName, "Leader", stringval);
            changed := TRUE;
        END (*IF*);

        (* Footer file. *)

        OS2.WinQueryDlgItemText (hwnd, DID.FooterName, 512, stringval);
        IF NOT Strings.Equal (stringval, OldFooterName) THEN
            RINIData.INIPutString (ListName, "Trailer", stringval);
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

    (* Creates this page and adds it to the notebook. *)

    BEGIN
        notebookhandle := notebook;
        Strings.Assign (name, ListName);
        pagehandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.ELMessage2,                (* dialogue ID *)
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
END Message2.

