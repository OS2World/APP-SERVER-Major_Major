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

IMPLEMENTATION MODULE OpeningDialogue;

        (****************************************************************)
        (*                                                              *)
        (*                  PM Setup for Major Major                    *)
        (*                    Initial dialogue box                      *)
        (*                                                              *)
        (*    Started:        20 May 2002                               *)
        (*    Last edited:    1 October 2019                            *)
        (*    Status:         Working                                   *)
        (*                                                              *)
        (****************************************************************)

IMPORT SYSTEM, OS2, OS2RTL, DID, Remote, BigFrame, PMInit, CommonSettings;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM RINIData IMPORT
    (* proc *)  SetRemote;

FROM MiscFuncs IMPORT
    (* proc *)  EVAL, SetINIorTNIname;

(************************************************************************)

VAR
    UseTNI: BOOLEAN;

    RemoteFlag: BOOLEAN;
    NotebookOpen: BOOLEAN;
    SwitchData: OS2.SWCNTRL;     (* switch entry data *)
    pagehandle: OS2.HWND;

(************************************************************************)

PROCEDURE SetLabels (lang: LangHandle);

    (* Displays the button labels in the specified language. *)

    VAR stringval: ARRAY [0..255] OF CHAR;

    BEGIN
        StrToBuffer (lang, "Opening.title", stringval);
        OS2.WinSetWindowText (pagehandle, stringval);
        StrToBuffer (lang, "Opening.local", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.LocalButton, stringval);
        StrToBuffer (lang, "Opening.remote", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.RemoteButton, stringval);
        StrToBuffer (lang, "Opening.setup", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.SetupButton, stringval);
        StrToBuffer (lang, "Opening.go", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.GoButton, stringval);
        StrToBuffer (lang, "Opening.exit", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.ExitButton, stringval);
    END SetLabels;

(************************************************************************)

PROCEDURE ["SysCall"] MainDialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    VAR ButtonID, NotificationCode: CARDINAL;
        lang: LangHandle;
        message: ARRAY [0..255] OF CHAR;

    BEGIN
        CASE msg OF
           |  OS2.WM_CLOSE:
                   OS2.WinPostMsg(hwnd, OS2.WM_QUIT, NIL, NIL);

           |  OS2.WM_COMMAND:

                   CASE OS2.SHORT1FROMMP(mp1) OF

                     | DID.SetupButton:
                          OS2.WinSetDlgItemText (hwnd, DID.Status, "");
                          Remote.OpenSetupDialogue (hwnd);

                     | DID.GoButton:
                          IF NOT RemoteFlag OR Remote.ConnectToServer (hwnd, DID.Status) THEN
                              CommonSettings.CurrentLanguage (lang, message);
                              StrToBuffer (lang, "Opening.status.loading", message);
                              OS2.WinSetDlgItemText (hwnd, DID.Status, message);
                              SetRemote (RemoteFlag);
                              NotebookOpen := TRUE;
                              BigFrame.OpenBigFrame (hwnd, UseTNI);
                              IF RemoteFlag THEN
                                  EVAL(Remote.ExecCommand ('Q'));
                              END (*IF*);
                              Remote.SaveRemoteFlag (RemoteFlag);
                              OS2.WinPostMsg(hwnd, OS2.WM_QUIT, NIL, NIL);
                          END (*IF*);

                     | DID.ExitButton:
                          Remote.SaveRemoteFlag (RemoteFlag);
                          OS2.WinPostMsg(hwnd, OS2.WM_QUIT, NIL, NIL);

                   END (*CASE*);

           |  OS2.WM_CONTROL:
                   NotificationCode := OS2.ULONGFROMMP(mp1);
                   ButtonID := NotificationCode MOD 65536;
                   NotificationCode := NotificationCode DIV 65536;
                   IF NotificationCode = OS2.BN_CLICKED THEN
                       CASE ButtonID OF
                         | DID.RemoteButton:
                              RemoteFlag := TRUE;
                              OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.SetupButton), TRUE);
                         | DID.LocalButton:
                              RemoteFlag := FALSE;
                              OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.SetupButton), FALSE);
                       END (*CASE*);

                   END (*IF*);

           |  OS2.WM_MINMAXFRAME:
                   (* Do we ever receive this message? *)

                   IF NotebookOpen THEN
                       BigFrame.ShowWindow;
                       RETURN NIL;
                   ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*IF*);

        ELSE    (* default must call WinDefWindowProc() *)
           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);

        RETURN NIL;

    END MainDialogueProc;

(**************************************************************************)

PROCEDURE CreateMainDialogue (LocalRemote: CARDINAL;  TNImode: BOOLEAN);

    (* Creates the main dialogue box. *)

    VAR SetupButtonWindow: OS2.HWND;
        pid: OS2.PID;  tid: OS2.TID;
        lang: LangHandle;
        stringval: ARRAY [0..31] OF CHAR;

    BEGIN
        UseTNI := TNImode;
        NotebookOpen := FALSE;
        pagehandle := OS2.WinLoadDlg(OS2.HWND_DESKTOP,    (* parent *)
                       OS2.HWND_DESKTOP,   (* owner *)
                       MainDialogueProc,   (* dialogue procedure *)
                       0,                  (* use resources in EXE *)
                       DID.FirstWindow,    (* dialogue ID *)
                       NIL);               (* creation parameters *)

        (* Put us on the visible task list. *)

        OS2.WinQueryWindowProcess (pagehandle, pid, tid);
        SwitchData.hwnd := pagehandle;
        WITH SwitchData DO
            hwndIcon      := 0;
            hprog         := 0;
            idProcess     := pid;
            idSession     := 0;
            uchVisibility := OS2.SWL_VISIBLE;
            fbJump        := OS2.SWL_JUMPABLE;
            szSwtitle     := "Major Major Admin";
            bProgType     := 0;
        END (*WITH*);
        OS2.WinCreateSwitchEntry (PMInit.OurHab(), SwitchData);

        SetINIorTNIname ("MAJOR", UseTNI, stringval);
        CommonSettings.SetINIFileName (stringval);
        Remote.SetInitialWindowPosition (pagehandle, "Opening");
        CommonSettings.CurrentLanguage (lang, stringval);
        RemoteFlag := Remote.InitialSetup(lang,
                        "Major Major", "Admin", "C:\Apps\Major", UseTNI);
        SetLabels (lang);

        (* Set the local/remote check buttons correctly. *)

        OS2.WinSendDlgItemMsg (pagehandle, DID.LocalButton, OS2.BM_SETCHECK,
                         OS2.MPFROMSHORT(1-ORD(RemoteFlag)), NIL);
        OS2.WinSendDlgItemMsg (pagehandle, DID.RemoteButton, OS2.BM_SETCHECK,
                         OS2.MPFROMSHORT(ORD(RemoteFlag)), NIL);
        SetupButtonWindow := OS2.WinWindowFromID(pagehandle, DID.SetupButton);

        (* The following coding is admittedly a little weird, but it    *)
        (* was the only way I could get it to work correctly.  For some *)
        (* reason making the second parameter a variable gives          *)
        (* unreliable results.                                          *)

        IF RemoteFlag THEN
            OS2.WinEnableWindow (SetupButtonWindow, TRUE);
        ELSE
            OS2.WinEnableWindow (SetupButtonWindow, FALSE);
        END (*IF*);

        IF LocalRemote = 1 THEN
            OS2.WinSendMsg (pagehandle, OS2.WM_CONTROL,
                   OS2.MPFROM2SHORT(DID.LocalButton, OS2.BN_CLICKED), NIL);
        ELSIF LocalRemote = 2 THEN
            OS2.WinSendMsg (pagehandle, OS2.WM_CONTROL,
                   OS2.MPFROM2SHORT(DID.RemoteButton, OS2.BN_CLICKED), NIL);
        END (*IF*);

        IF LocalRemote > 0 THEN
            OS2.WinSendMsg (pagehandle, OS2.WM_COMMAND,
                                 OS2.MPFROMSHORT(DID.GoButton), NIL);
        END (*IF*);

        OS2.WinProcessDlg(pagehandle);
        Remote.StoreWindowPosition (pagehandle, "Opening", TRUE);
        OS2.WinDestroyWindow (pagehandle);

    END CreateMainDialogue;

(**************************************************************************)

END OpeningDialogue.

