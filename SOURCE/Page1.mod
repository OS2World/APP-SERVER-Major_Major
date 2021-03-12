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

IMPLEMENTATION MODULE Page1;

        (****************************************************************)
        (*                                                              *)
        (*                  Admin program for MajorMajor                *)
        (*                     Page 1 of the notebook                   *)
        (*                                                              *)
        (*    Started:        16 June 2000                              *)
        (*    Last edited:    29 September 2019                         *)
        (*    Status:         Working                                   *)
        (*                                                              *)
        (****************************************************************)


IMPORT Conversions, PMInit;    (* while debugging *)

FROM SYSTEM IMPORT ADDRESS, CARD8, INT16, CAST, ADR;

IMPORT OS2, OS2RTL, PSDialogue, CommonSettings;

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

FROM LogLevel IMPORT
    (* type *)  LogLevelType;

FROM Inet2Misc IMPORT
    (* proc *)  IPToString;

FROM MiscFuncs IMPORT
    (* proc *)  EVAL;

FROM Names IMPORT
    (* type *)  FilenameString;

(**************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    DirectoryString = ARRAY [0..511] OF CHAR;

VAR
    INIFileName: FilenameString;

    pagehandle, notebookhandle: OS2.HWND;
    OurPageID: CARDINAL;
    ourlanguage: LangHandle;

    MailrootDirPresent: BOOLEAN;
    ServerIsWeasel: BOOLEAN;
    ChangeInProgress: BOOLEAN;
    LogDetail: LogLevelType;

    (* Original values of the ini variables. *)

    OldWeaselDir, OldMailDomain: DirectoryString;
    OldSMTPServer: DirectoryString;
    OldSMTPPort: CARDINAL;
    OldPOPServer: DirectoryString;
    OldPOPPort: CARDINAL;
    OldBatchLimit: CARDINAL;

(************************************************************************)
(*                        DEFAULT MAIL DOMAIN                           *)
(************************************************************************)

PROCEDURE GetServerHostName (VAR (*OUT*) textualname: ARRAY OF CHAR);

    (* Creates a host record with as official an answer as possible for *)
    (* the IP address and name of the host on which MM is running.      *)

    VAR HostInfo: HostEntPtr;
        numaddr: CARDINAL;  remote: BOOLEAN;

    BEGIN
        numaddr := 0;
        textualname[0] := Nul;
        HostInfo := NIL;
        remote := RemoteOperation();

        (* We use gethostname in preference to gethostid, because it    *)
        (* seems that gethostid can return a left-over number that's    *)
        (* not necessarily still valid.                                 *)

        IF NOT remote AND NOT gethostname(textualname, SIZE(textualname)) THEN
            HostInfo := gethostbyname (textualname);
        END (*IF*);

        IF (HostInfo = NIL) THEN
            numaddr := OurIPAddress (remote);
            HostInfo := gethostbyaddr (numaddr, SIZE(CARDINAL), AF_INET);
        END (*IF*);

        (* We've got as much data as we're going to get from the        *)
        (* nameserver.  Take the "most official" part of it.            *)

        IF HostInfo <> NIL THEN

            (* Official name *)

            IF HostInfo^.h_name <> NIL THEN
                Strings.Assign (HostInfo^.h_name^, textualname);
            END (*IF*);

            (* First IP address *)

            IF (HostInfo^.h_addr_list <> NIL)
                        AND (HostInfo^.h_addr_list^[0] <> NIL) THEN
                numaddr := HostInfo^.h_addr_list^[0]^;
            END (*IF*);
        END (*IF*);

        (* If we still don't have a textual result, after all that,     *)
        (* use the numeric IP address as a last resort.                 *)

        IF textualname[0] = Nul THEN
            IPToString (numaddr, TRUE, textualname);
        END (*IF*);

    END GetServerHostName;

(************************************************************************)

PROCEDURE WeaselOnThisMachine(): BOOLEAN;

    (* Returns TRUE iff the mail server is Weasel on this machine. *)

    BEGIN
        RETURN ServerIsWeasel;
    END WeaselOnThisMachine;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        ourlanguage := lang;
        StrToBuffer (lang, "Page1.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,OurPageID), ADR(stringval));
        StrToBuffer (lang, "Page1.mailserver", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.Page1ServerLabel, stringval);
        StrToBuffer (lang, "Page1.Weaselhere", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.ServerIsWeasel, stringval);
        StrToBuffer (lang, "Page1.otherserver", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.ServerIsNotWeasel, stringval);
        StrToBuffer (lang, "Page1.domain", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.Page1DomainLabel, stringval);
        StrToBuffer (lang, "Page1.BatchLimit", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.BatchLimitLabel, stringval);
        StrToBuffer (lang, "Page1.Weaseldir", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.RootDirBox, stringval);
        StrToBuffer (lang, "Page1.SMTPserver", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.SMTPGroup, stringval);
        StrToBuffer (lang, "Page1.hostname", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.SMTPServerLabel, stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.POPServerLabel, stringval);
        StrToBuffer (lang, "Page1.port", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.SMTPPortLabel, stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.POPPortLabel, stringval);
        StrToBuffer (lang, "Page1.useauthent", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.EnableSMTPAuthentication, stringval);
        StrToBuffer (lang, "Page1.POPserver", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.POPGroup, stringval);
    END SetLanguage;

(************************************************************************)
(*                     LOADING DATA FROM THE INI FILE                   *)
(************************************************************************)

PROCEDURE LoadValues (hwnd: OS2.HWND);

    (* Fills the dialogue elements on page 1 with data from the INI file,       *)
    (* or loads default values if they're not in the INI file.                  *)

    VAR opened, found, ExtraLogging: BOOLEAN;
        val1, pos, AuthOption: CARDINAL;
        stringval: DirectoryString;
        HostName: DirectoryString;

    BEGIN
        HostName[0] := Nul;
        opened := OpenINIFile (INIFileName);

        (* Mail domain name. *)

        IF opened AND INIGetString ('$SYS', 'MailDomain', stringval) THEN
            OldMailDomain := stringval;
        ELSE
            GetServerHostName (stringval);
            Strings.Assign (stringval, HostName);
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.MailDomain, stringval);

        (* Batch size limit. *)

        IF opened AND INIFetch ('$SYS', 'BatchLimit', val1) THEN
            OldBatchLimit := val1;
        ELSE
            val1 := 50;
        END (*IF*);
        OS2.WinSetDlgItemShort (hwnd, DID.BatchLimit, val1, FALSE);

        (* Mail root directory. *)

        MailrootDirPresent := INIGetString ('$SYS', 'MailRoot', stringval);
        IF NOT MailrootDirPresent THEN
            stringval := ".\";
        END (*IF*);

        (* Weasel directory. *)

        IF opened AND INIGetString ('$SYS', 'WeaselDir', stringval) THEN
            OldWeaselDir := stringval;
        ELSIF MailrootDirPresent THEN
            (* stringval still holds the mail root information. *)
            pos := Strings.Length (stringval);
            IF pos > 1 THEN
                DEC (pos, 2);
                Strings.FindPrev ('\', stringval, pos, found, pos);
                IF NOT found THEN
                    Strings.FindPrev ('/', stringval, pos, found, pos);
                END (*IF*);
                IF found THEN
                    stringval[pos] := '\';
                    stringval[pos+1] := Nul;
                END (*IF*);
            END (*IF*);
        ELSE
            OurDirectory (stringval);
            IF stringval[0] = Nul THEN
                stringval := '.';
            END (*IF*);
            Strings.Append ("\", stringval);
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.WeaselDir, stringval);

        (* SMTP Server. *)

        IF opened AND INIGetString ('$SYS', 'SMTPServer', stringval) THEN
            OldSMTPServer := stringval;
        ELSIF HostName[0] <> Nul THEN
            stringval := HostName;
        ELSE
            GetServerHostName (stringval);
            HostName := stringval;
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.SMTPServer, stringval);
        IF opened AND INIFetch ('$SYS', 'SMTPport', val1) THEN
            OldSMTPPort := val1;
        ELSE
            val1 := 25;
        END (*IF*);
        OS2.WinSetDlgItemShort (hwnd, DID.SMTPport, val1, FALSE);

        (* Enable SMTP authentication, formerly known as POP-before-SMTP option. *)

        AuthOption := 0;
        EVAL (opened AND INIFetch ("$SYS", "AuthOption", AuthOption));
        IF AuthOption = 0 THEN
            OS2.WinSendDlgItemMsg (hwnd, DID.EnableSMTPAuthentication, OS2.BM_SETCHECK,
                      OS2.MPFROMSHORT(0), NIL);
        ELSE
            OS2.WinSendDlgItemMsg (hwnd, DID.EnableSMTPAuthentication, OS2.BM_SETCHECK,
                      OS2.MPFROMSHORT(1), NIL);
        END (*IF*);

        (* POP Server. *)

        IF opened AND INIGetString ('$SYS', 'POPServer', stringval) THEN
            OldPOPServer := stringval;
        ELSIF HostName[0] <> Nul THEN
            stringval := HostName;
        ELSE
            GetServerHostName (stringval);
            HostName := stringval;
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.POPServer, stringval);
        IF opened AND INIFetch ('$SYS', 'POPport', val1) THEN
            OldPOPPort := val1;
        ELSE
            val1 := 110;
        END (*IF*);
        OS2.WinSetDlgItemShort (hwnd, DID.POPport, val1, FALSE);

        (* Extra logging. *)

        IF opened AND INIFetch ('$SYS', 'LogDetail', LogDetail) THEN
            ExtraLogging := LogDetail > logsummary;
        ELSE
            ExtraLogging := FALSE;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.LogDetail, OS2.BM_SETCHECK,
                      OS2.MPFROMSHORT(ORD(ExtraLogging)), NIL);

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

    VAR j, val32: CARDINAL;
        opened: BOOLEAN;
        val16: INT16;
        stringval: DirectoryString;

    BEGIN
        opened := OpenINIFile (INIFileName);

        (* Mail server. *)

        ServerIsWeasel := QueryButton (hwnd, DID.ServerIsWeasel) > 0;
        INIPut ('$SYS', 'LocalWeasel', ServerIsWeasel);

        (* Mail domain name. *)

        OS2.WinQueryDlgItemText (hwnd, DID.MailDomain, 512, stringval);
        IF NOT Strings.Equal (stringval, OldMailDomain) THEN
            INIPutString ('$SYS', 'MailDomain', stringval);
            Strings.Assign (stringval, OldMailDomain);
        END (*IF*);

        (* Batch size limit. *)

        OS2.WinQueryDlgItemShort (hwnd, DID.BatchLimit, val16, FALSE);
        val32 := val16;
        IF val32 <> OldBatchLimit THEN
            INIPut ('$SYS', 'BatchLimit', val32);
        END (*IF*);

        (* Weasel directory.  Make sure that WeaselDir has no leading or  *)
        (* trailing spaces, and that it ends with a trailing '\'.         *)

        OS2.WinQueryDlgItemText (hwnd, DID.WeaselDir, 512, stringval);
        WHILE stringval[0] = ' ' DO
            Strings.Delete (stringval, 0, 1);
        END (*WHILE*);
        j := Strings.Length (stringval);
        LOOP
            IF j = 0 THEN EXIT(*LOOP*) END(*IF*);
            DEC(j);
            IF stringval[j] = ' ' THEN stringval[j] := Nul END(*IF*);
        END (*LOOP*);

        j := Strings.Length (stringval);
        IF (j = 0) OR ((stringval[j-1] <> '/') AND (stringval[j-1] <> '\')) THEN
            Strings.Append ('\', stringval);
        END (*IF*);

        IF NOT Strings.Equal (stringval, OldWeaselDir) THEN
            INIPutString ('$SYS', 'WeaselDir', stringval);
            Strings.Assign (stringval, OldWeaselDir);
        END (*IF*);

        (* SMTP Server. *)

        OS2.WinQueryDlgItemText (hwnd, DID.SMTPServer, 512, stringval);
        IF NOT Strings.Equal (stringval, OldSMTPServer) THEN
            INIPutString ('$SYS', 'SMTPServer', stringval);
        END (*IF*);
        OS2.WinQueryDlgItemShort (hwnd, DID.SMTPport, val16, FALSE);
        val32 := val16;
        IF val32 <> OldSMTPPort THEN
            INIPut ('$SYS', 'SMTPport', val32);
        END (*IF*);

        (* Use SMTP authentication.  Turning this off will set the      *)
        (* INI variable AuthOption to zero.                             *)

        IF QueryButton (hwnd, DID.EnableSMTPAuthentication) = 0 THEN
            val32 := 0;
            INIPut ('$SYS', 'AuthOption', val32);
        END (*IF*);

        (* POP Server. *)

        OS2.WinQueryDlgItemText (hwnd, DID.POPServer, 512, stringval);
        IF NOT Strings.Equal (stringval, OldPOPServer) THEN
            INIPutString ('$SYS', 'POPServer', stringval);
        END (*IF*);
        OS2.WinQueryDlgItemShort (hwnd, DID.POPport, val16, FALSE);
        val32 := val16;
        IF val32 <> OldPOPPort THEN
            INIPut ('$SYS', 'POPport', val32);
        END (*IF*);

        (* Extra logging. *)

        IF QueryButton (hwnd, DID.LogDetail) > 0 THEN
            LogDetail := logdetailed;
        ELSE
            LogDetail := logsummary;
        END (*IF*);
        INIPut ('$SYS', 'LogDetail', LogDetail);

        IF opened THEN
            CloseINIFile;
        END (*IF*);

    END StoreData;

(************************************************************************)
(*                        DIALOGUE PROCEDURE                            *)
(************************************************************************)

PROCEDURE ClearObsoleteINIentry (owner: OS2.HWND);

    (* The first time the Weasel directory is seen, and the obsolete    *)
    (* MailRoot INI entry is still present, give a warning that the     *)
    (* Weasel directory might not be correct.                           *)

    BEGIN
        OS2.WinMessageBox (OS2.HWND_DESKTOP, owner,
                            "The 'Weasel directory' entry has been deduced"
                            + " from other information. Please check that it"
                            + " is correct, and change it if necessary."
                            + " If you don't use Weasel, you may ignore this"
                            + " message.",
                            "INI data updated", 0,
                             OS2.MB_OK + OS2.MB_WARNING);
        IF OpenINIFile (INIFileName) THEN
            INIDeleteKey ("$SYS", "MailRoot");
            CloseINIFile;
        END (*IF*);
        MailrootDirPresent := FALSE;
    END ClearObsoleteINIentry;

(************************************************************************)

PROCEDURE ShowWeaselGroup (hwnd: OS2.HWND;  Show: BOOLEAN);

    (* Shows some dialogue box elements and hides others, depending on  *)
    (* the "Show" parameter.                                            *)

    BEGIN
        IF Show THEN
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SMTPGroup), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SMTPServerLabel), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SMTPServer), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SMTPPortLabel), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SMTPport), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.EnableSMTPAuthentication), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.POPGroup), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.POPServerLabel), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.POPServer), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.POPPortLabel), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.POPport), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.RootDirBox), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.WeaselDir), TRUE);
        ELSE
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.RootDirBox), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.WeaselDir), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SMTPGroup), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SMTPServerLabel), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SMTPServer), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SMTPPortLabel), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.SMTPport), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.EnableSMTPAuthentication), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.POPGroup), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.POPServerLabel), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.POPServer), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.POPPortLabel), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.POPport), TRUE);
        END (*IF*);
    END ShowWeaselGroup;

(************************************************************************)

PROCEDURE ["SysCall"] DialogueProc (hwnd: OS2.HWND;  msg: OS2.ULONG;
                                      mp1, mp2: OS2.MPARAM): OS2.MRESULT;

    VAR ButtonID, NotificationCode, AuthOption: CARDINAL;
        opened: BOOLEAN;

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
            ButtonID := NotificationCode MOD 65536;
            NotificationCode := NotificationCode DIV 65536;
            IF NotificationCode = OS2.BN_CLICKED THEN
                CASE ButtonID OF
                  | DID.ServerIsWeasel:
                       ServerIsWeasel := TRUE;
                       ShowWeaselGroup (hwnd, TRUE);
                       IF MailrootDirPresent THEN
                           ClearObsoleteINIentry(hwnd);
                       END (*IF*);
                       RETURN NIL;
                  | DID.ServerIsNotWeasel:
                       ServerIsWeasel := FALSE;
                       ShowWeaselGroup (hwnd, FALSE);
                       RETURN NIL;
                  | DID.EnableSMTPAuthentication:
                       IF QueryButton (hwnd, ButtonID) > 0 THEN
                           PSDialogue.Edit (hwnd, ourlanguage);
                           opened := OpenINIFile (INIFileName);
                           AuthOption := 0;
                           EVAL (opened AND INIFetch ("$SYS", "AuthOption", AuthOption));
                           IF opened THEN
                               CloseINIFile;
                           END (*IF*);
                           IF AuthOption = 0 THEN
                               OS2.WinSendDlgItemMsg (hwnd, DID.EnableSMTPAuthentication, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(0), NIL);
                           ELSE
                               OS2.WinSendDlgItemMsg (hwnd, DID.EnableSMTPAuthentication, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(1), NIL);
                           END (*IF*);
                       END (*IF*);
                       RETURN NIL;
                ELSE
                       RETURN OS2.WinDefDlgProc (hwnd, msg, mp1, mp2);
                END (*CASE*);
            END (*IF*);

        END (*CASE*);

        RETURN OS2.WinDefDlgProc (hwnd, msg, mp1, mp2);

    END DialogueProc;

(**************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND;
                       VAR (*OUT*) PageID: CARDINAL): OS2.HWND;

    (* Creates page 1 and adds it to the notebook. *)

    VAR Label: ARRAY [0..31] OF CHAR;

    BEGIN
        notebookhandle := notebook;
        pagehandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.page1,                (* dialogue ID *)
                       NIL);                 (* creation parameters *)
        OurPageID := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         NIL,
                         OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE,
                                           OS2.BKA_FIRST)));
        PageID := OurPageID;
        Label := "Basic";
        OS2.WinSendMsg (notebook, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(Label));
        OS2.WinSendMsg (notebook, OS2.BKM_SETPAGEWINDOWHWND,
                        CAST(ADDRESS,PageID), CAST(ADDRESS,pagehandle));
        OS2.WinSetWindowPos (pagehandle, 0, 0, 0, 0, 0, OS2.SWP_MOVE);

        (* Server option. *)

        IF NOT (OpenINIFile(INIFileName)
                AND INIFetch ('$SYS', 'LocalWeasel', ServerIsWeasel)) THEN
            ServerIsWeasel := TRUE;
        END (*IF*);
        OS2.WinSendDlgItemMsg (pagehandle, DID.ServerIsWeasel, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(ServerIsWeasel)), NIL);
        OS2.WinSendDlgItemMsg (pagehandle, DID.ServerIsNotWeasel, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(1-ORD(ServerIsWeasel)), NIL);
        CloseINIFile;
        IF ServerIsWeasel THEN
            ShowWeaselGroup (pagehandle, TRUE);
        ELSE
            ShowWeaselGroup (pagehandle, FALSE);
        END (*IF*);
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
        PSDialogue.SetINIFileName (name);
    END SetINIFileName;

(**************************************************************************)

BEGIN
    MailrootDirPresent := FALSE;
    ServerIsWeasel := FALSE;
    ChangeInProgress := FALSE;
    OldBatchLimit := 0;
    OldWeaselDir := "No Weasel directory has been set";
    OldMailDomain := "No mail domain has been set";
    OldSMTPServer := "";
    OldPOPServer := "";
    OldSMTPPort := 0;
    OldPOPPort := 0;
END Page1.

