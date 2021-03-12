(**************************************************************************)
(*                                                                        *)
(*  The Major Major mailing list manager                                  *)
(*  Copyright (C) 2020   Peter Moylan                                     *)
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

MODULE Major;

        (********************************************************)
        (*                                                      *)
        (*                Mailing list manager                  *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            20 May 2000                     *)
        (*  Last edited:        19 September 2020               *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT SYSTEM, OS2, ListChecker, INIData, Commands, POPClient, SMTPOut,
       TextIO, Strings;

FROM ListAdmin IMPORT
    (* proc *)  SetAdminName, ProcessAdminRequests;

FROM Archives IMPORT
    (* proc *)  SetArchiveRoot;

FROM LogLevel IMPORT
    (* type *)  LogLevelType;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  SetDefaultLanguage, UseLanguage, StrToBuffer, StrToBufferA;

FROM AddressLists IMPORT
    (* type *)  EmailAddress,
    (* proc *)  SetBatchLimit;

FROM MailFetcher IMPORT
    (* proc *)  EnableLocalOperation, DisableLocalOperation;

FROM FileOps IMPORT
    (* type *)  FilenameString,
    (* proc *)  Exists;

FROM OS2Sem IMPORT
    (* type *)  SemKind,
    (* proc *)  WaitOnSemaphore;

FROM ProgName IMPORT
    (* proc *)  GetProgramName;

FROM Exceptq IMPORT
    (* proc *)  InstallExceptq, UninstallExceptq;

FROM Names IMPORT
    (* type *)  UserName, PassString;

FROM SplitScreen IMPORT
    (* proc *)  NotDetached, WriteStringAt, WriteString, WriteLn;

FROM IOChan IMPORT
    (* type *)  ChanId;

FROM ProgramArgs IMPORT
    (* proc *)  ArgChan, IsArgPresent;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM Timer IMPORT
    (* proc *)  TimedWait, Sleep;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, Wait, Signal;

FROM TaskControl IMPORT
    (* proc *)  CreateTask;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  StartTransactionLogging, CreateLogID, LogTransaction, LogTransactionL;

FROM CtrlC IMPORT
    (* proc *)  SetBreakHandler, IsCtrlCException;

(********************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    LongFilenameString = ARRAY [0..513] OF CHAR;

VAR
    (* Our INI file name, and a flag to enable TNI mode. *)

    INIFileName: FilenameString;

    UseTNI: BOOLEAN;

    (* Flag to say that we may write messages to the screen. *)

   ScreenEnabled: BOOLEAN;

    (* Flag to say Exceptq has been installed. *)

    ExceptqActive: BOOLEAN;

    (* Flag to say that the user has typed Ctrl/C. *)

    ShutdownInProgress: BOOLEAN;

    (* Flag to say that we're getting mail directly from a      *)
    (* Weasel mail directory, rather than fetching via POP.     *)

    ServerIsWeasel: BOOLEAN;

    (* Event semaphore to trigger updater task. *)

    UpdaterFlag: OS2.HEV;

    (* Semaphore that we use to start the shutdown process. *)

    CommenceShutdown: Semaphore;

    (* Termination semaphore used in shutdown processing. *)

    TaskDone: Semaphore;

    (* Semaphore to trigger a new check for admin requests. *)

    DoAdminCheck: Semaphore;

    (* Time between checks for new admin requests. *)

    SampleTime: CARDINAL;

    (* Event flag by which external program can force us to re-check all lists. *)

    ForceFreshCheck: OS2.HEV;

    (* Event flag by which another process can request us to shut down. *)

    ShutdownSignal: OS2.HEV;

    (* Flag to force debug logging. *)

    ExtraLogging: BOOLEAN;

    (* Language for logging, etc. *)

    AdminLang: LangHandle;

    (* Log ID for when we are loading INI data. *)

    INILoadLogID: TransactionLogID;

(********************************************************************************)
(*                              SHUTDOWN OPERATIONS                             *)
(********************************************************************************)

PROCEDURE ShutterDowner;

    (* A task whose job is to shut down the program. *)

    (* Unsolved mystery: I could not make Ctrl/C shut down the program      *)
    (* reliably until I included the Sleep() call.  Experiments suggests    *)
    (* that the actual sleep duration is not critical.                      *)
    (* I should take a closer look at my TimedWait implementation.          *)

    VAR LogID: TransactionLogID;
        logmessage: ARRAY [0..255] OF CHAR;

    BEGIN
        LogID := CreateLogID (ListChecker.MMctx, "        ");
        Wait (CommenceShutdown);
        ShutdownInProgress := TRUE;
        StrToBuffer (AdminLang, "Major.wantshutdown", logmessage);
        LogTransaction (LogID, logmessage);
        Sleep (100);
        Signal (DoAdminCheck);                      (* to terminate RunManager *)
        Signal (TaskDone);
    END ShutterDowner;

(********************************************************************************)

PROCEDURE ["C"] ControlCHandler(): BOOLEAN;

    (* Intercepts a Ctrl/C from the keyboard. *)

    BEGIN
        Signal (CommenceShutdown);
        RETURN TRUE;
    END ControlCHandler;

(********************************************************************************)

PROCEDURE ExternalShutdownRequestDetector;

    (* Runs as a separate task.  Responds to the public event semaphore that    *)
    (* requests a Major Major shutdown.                                         *)

    CONST semName = "\SEM32\MAJOR\SHUTDOWN";

    BEGIN
        ShutdownSignal := 0;
        IF OS2.DosOpenEventSem (semName, ShutdownSignal) = OS2.ERROR_SEM_NOT_FOUND THEN
            OS2.DosCreateEventSem (semName, ShutdownSignal, OS2.DC_SEM_SHARED, FALSE);
        END (*IF*);

        WHILE NOT ShutdownInProgress DO
            WaitOnSemaphore (event, ShutdownSignal);
            Signal (CommenceShutdown);
        END (*WHILE*);

        OS2.DosCloseEventSem(ShutdownSignal);
        Signal (TaskDone);
        (*WriteString ("ExternalShutdownRequestDetector terminating");  WriteLn;*)

    END ExternalShutdownRequestDetector;

(********************************************************************************)
(*                          COMMAND LINE PARAMETER PARSING                      *)
(********************************************************************************)

PROCEDURE FetchCommandLineParameters(VAR (*OUT*) UseTNI: BOOLEAN): BOOLEAN;

    (* Normally we don't have any command line parameters, but this procedure   *)
    (* looks for the 'x' parameter to give extra logging and the 't' to         *)
    (* enable use of the TNI file.                                              *)

    TYPE CharNumber = [0..255];

    VAR j, TNIoption: CARDINAL;
        args: ChanId;
        ExtraLogging: BOOLEAN;
        Options: ARRAY CharNumber OF CHAR;

    (****************************************************************************)

    PROCEDURE SkipBlanks;

        BEGIN
            LOOP
                IF Options[j] <> ' ' THEN EXIT(*LOOP*) END(*IF*);
                IF j = MAX(CharNumber) THEN
                    Options[j] := CHR(0);  EXIT (*LOOP*);
                ELSE
                    INC (j);
                END (*IF*);
            END (*LOOP*);
        END SkipBlanks;

    (****************************************************************************)

    BEGIN
        ExtraLogging := FALSE;
        TNIoption := 2;              (* meaning "no decision yet" *)
        args := ArgChan();
        IF IsArgPresent() THEN
            TextIO.ReadString (args, Options);
            j := 0;  SkipBlanks;
            LOOP
                CASE CAP(Options[j]) OF
                    CHR(0):   EXIT (*LOOP*);
                  | 'I':      INC (j);
                              TNIoption := 0;
                  | 'T':      INC (j);
                              TNIoption := 1;
                  | 'X':      INC (j);  ExtraLogging := TRUE;
                ELSE
                              INC (j);
                END (*CASE*);
            END (*LOOP*);
        END (*IF*);
        IF TNIoption < 2 THEN
            UseTNI := TNIoption <> 0;
        ELSIF NOT INIData.ChooseDefaultINI("Major", UseTNI) THEN
            UseTNI := FALSE;
        END (*IF*);
        RETURN ExtraLogging;
    END FetchCommandLineParameters;

(********************************************************************************)
(*                              LOADING INI DATA                                *)
(********************************************************************************)

PROCEDURE ResolveRelativePath (VAR (*IN*) BaseDir: LongFilenameString;
                               VAR (*INOUT*) Dir2: LongFilenameString);

    (* Modifies Dir2, if necessary, to allow for the possibility that   *)
    (* it is specified as a path relative to BaseDir.                   *)

    (* On entry, both BaseDir and Dir2 end with a '\'. On exit, the     *)
    (* updated Dir2 must end with a '\'.                                *)

    VAR result: LongFilenameString;
        pos: CARDINAL;  found: BOOLEAN;
        Drive: ARRAY [0..1] OF CHAR;

    BEGIN
        (* Check whether Dir2 has an explicit drive specification.      *)
        (* Note: an explicit drive letter plus a relative path is a     *)
        (* logically unacceptable combination in this context, so we    *)
        (* are free to give arbitrary treatment to such a thing.        *)

        Drive[1] := ':';
        IF Dir2[1] = ':' THEN
            Drive[0] := CAP(Dir2[0]);
            Strings.Delete (Dir2, 0, 2);
        ELSE
            Drive[0] := Nul;
        END (*IF*);

        (* The rules we want to apply here are:                     *)
        (* - if Dir2 is an absolute path, then use it and ignore    *)
        (*    BaseDir, except for one detail: if Dir2 does not      *)
        (*   specify a drive, then use the BaseDir drive.           *)
        (* - otherwise append Dir2 to BaseDir, with a linking '\'   *)
        (*   if needed.  In this case we might need to correct for  *)
        (*   the presence of '.' and '..' specifications.           *)

        (* Check whether Dir2 path is relative or absolute. *)

        IF (Dir2[0] = '/') OR (Dir2[0] = '\') THEN

            (* Absolute path.  Insert drive specification if needed *)
            (* but otherwise leave Dir2 unchanged.                  *)

            IF (Drive[0] = Nul) AND (BaseDir[1] = ':') THEN
                Drive[0] := CAP(BaseDir[0]);
            END (*IF*);

            IF Drive[0] <> Nul THEN
                Strings.Insert (Drive, 0, Dir2);
            END (*IF*);

        ELSE
            (* Relative path for Dir2.  Interpret it as relative    *)
            (* to BaseDir.                                          *)

            (* Handle ".." directory specifications. *)

            Strings.Assign (BaseDir, result);
            pos := LENGTH(result);
            IF (pos = 0) OR
                ((result[pos-1] <> '/') AND (result[pos-1] <> '\')) THEN
                result[pos] := '\';
                result[pos+1] := Nul;
            END (*IF*);

            WHILE Dir2[0] = '.' DO
                Strings.Delete (Dir2, 0, 1);
                IF Dir2[0] = '.' THEN
                    Strings.Delete (Dir2, 0, 1);

                    (* Go up one level in the file system. *)

                    (* Invariant: result ends with the '\' character.  *)

                    pos := LENGTH(result);
                    IF pos > 1 THEN
                        DEC (pos, 2);
                        Strings.FindPrev ('\', BaseDir, pos, found, pos);
                        IF NOT found THEN
                            Strings.FindPrev ('/', BaseDir, pos, found, pos);
                        END (*IF*);
                        IF found THEN
                            result[pos+1] := Nul;
                        END (*IF*);
                    END (*IF*);
                END (*IF*);

                IF (Dir2[0] = '\') OR (Dir2[0] = '/') THEN
                    Strings.Delete (Dir2, 0, 1);
                END (*IF*);

            END (*WHILE*);

            IF (Dir2[0] = '\') OR (Dir2[0] = '/') THEN
                Strings.Delete (Dir2, 0, 1);
            END (*IF*);
            Strings.Append (Dir2, result);
            Strings.Assign (result, Dir2);

        END (*IF*);

    END ResolveRelativePath;

(********************************************************************************)

PROCEDURE LoadINIData (LogID: TransactionLogID;  ExtraLogging: BOOLEAN);

    (* Loads setup parameters from "major.ini". *)

    CONST Nul = CHR(0);

    VAR hini: INIData.HINI;
        AdminName, AdminLoginName, MailDomain: EmailAddress;
        AdminPassword: ARRAY [0..31] OF CHAR;
        DefaultLanguage, AdminLanguage: ARRAY [0..31] OF CHAR;
        OurDir, MailRoot, ArchiveDir, WeaselDir, ININame: LongFilenameString;
        POPServer, SMTPServer, PSServer, message: ARRAY [0..511] OF CHAR;
        DefaultLang: LangHandle;
        POPport, SMTPport, PSPort, AuthOption, BatchLimit: CARDINAL;
        LogDetail: LogLevelType;
        LogLevel: SYSTEM.CARD8;
        Multidomain, ReplyToErrors: BOOLEAN;
        LogFileName, HelpFileName, PlainTextFileName, AdminFilter: FilenameString;
        UsePOPbeforeSMTP, UseWTNI: BOOLEAN;
        PSUsername: UserName;
        PSPassword: PassString;
        appSYS: ARRAY [0..4] OF CHAR;

    BEGIN
        appSYS := "$SYS";
        LogLevel := 2;
        IF ExtraLogging THEN
            LogDetail := logdebug;
        ELSE
            LogDetail := logsummary;
        END (*IF*);
        BatchLimit := 50;
        POPport := 110;  SMTPport := 25;
        ReplyToErrors := TRUE;
        ServerIsWeasel := FALSE;
        UsePOPbeforeSMTP := FALSE;  PSPort := 110;  AuthOption := 0;
        DefaultLanguage := "en";
        AdminLanguage := DefaultLanguage;
        INIData.OurDirectory (OurDir);
        hini := INIData.OpenINIFile (INIFileName);
        IF INIData.INIValid (hini) THEN
            IF NOT INIData.INIGet (hini, appSYS, "LocalWeasel", ServerIsWeasel) THEN
                ServerIsWeasel := FALSE;
            END (*IF*);
            IF NOT INIData.INIGet (hini, appSYS, "BatchLimit", BatchLimit) THEN
                BatchLimit := 50;
            END (*IF*);
            IF NOT INIData.INIGet (hini, appSYS, "ExtraLogging", LogDetail) THEN
                LogDetail := logdebug;
            END (*IF*);
            IF NOT INIData.INIGet (hini, appSYS, "TransLevel", LogLevel) THEN
                LogLevel := 2;
            END (*IF*);
            IF NOT INIData.INIGetString (hini, appSYS, "TransLogFile", LogFileName) THEN
                LogFileName := "MAJOR.LOG";
            END (*IF*);
            IF NOT INIData.INIGet (hini, appSYS, "ArchiveDir", ArchiveDir) THEN
                ArchiveDir := "archives.LOG";
            END (*IF*);
            IF ExtraLogging THEN
                LogDetail := logdebug;
            ELSIF LogLevel = 0 THEN
                LogDetail := lognone;
            ELSIF NOT INIData.INIGet (hini, appSYS, "LogDetail", LogDetail) THEN
                LogDetail := logsummary;
            END (*IF*);
            IF NOT INIData.INIGet (hini, appSYS, "AdminCheckInterval", SampleTime) THEN
                SampleTime := 30*60;
            END (*IF*);
            IF NOT INIData.INIGetString (hini, appSYS, "AdminName", AdminName) THEN
                AdminName := "MajorMajor";
            END (*IF*);
            IF NOT INIData.INIGetString (hini, appSYS, "AdminLoginName", AdminLoginName) THEN
                AdminLoginName := AdminName;
            END (*IF*);
            IF NOT INIData.INIGetString (hini, appSYS, "AdminPassword", AdminPassword) THEN
                AdminPassword := "";
            END (*IF*);
            IF NOT INIData.INIGetString (hini, appSYS, "AdminFilter", AdminFilter) THEN
                AdminFilter := "";
            END (*IF*);
            IF NOT INIData.INIGet (hini, appSYS, "ReplyToErrors", ReplyToErrors) THEN
                ReplyToErrors := TRUE;
            END (*IF*);
            IF NOT INIData.INIGetString (hini, appSYS, "HelpFile", HelpFileName) THEN
                HelpFileName := "Canned\en\DefaultHelp.txt";
            END (*IF*);
            IF NOT INIData.INIGetString (hini, appSYS, "PlainTextFile", PlainTextFileName) THEN
                HelpFileName := "Canned\en\DefaultPlainTextFile.txt";
            END (*IF*);
            IF NOT INIData.INIGetString (hini, appSYS, "MailDomain", MailDomain) THEN
                MailDomain := "[127.0.0.1]";
            END (*IF*);
            IF NOT INIData.INIGetString (hini, appSYS, 'MailRoot', MailRoot) THEN
                MailRoot := OurDir;
                Strings.Append ("\MailRoot\", MailRoot);
            END (*IF*);
            IF NOT INIData.INIGetString (hini, appSYS, 'WeaselDir', WeaselDir) THEN
                WeaselDir := "";
            END (*IF*);
            IF NOT INIData.INIGetString (hini, appSYS, "POPServer", POPServer) THEN
                Strings.Assign ("[127.0.0.1]", POPServer);
            END (*IF*);
            EVAL (INIData.INIGet (hini, appSYS, "POPport", POPport));
            IF NOT INIData.INIGetString (hini, appSYS, "SMTPServer", SMTPServer) THEN
                Strings.Assign ("[127.0.0.1]", SMTPServer);
            END (*IF*);
            EVAL (INIData.INIGet (hini, appSYS, "SMTPport", SMTPport));

            IF NOT INIData.INIGet (hini, appSYS, 'AuthOption', AuthOption) THEN
                EVAL(INIData.INIGet (hini, appSYS, "UsePOPbeforeSMTP", UsePOPbeforeSMTP));
                IF UsePOPbeforeSMTP THEN
                    AuthOption := 2;
                ELSE
                    AuthOption := 0;
                END (*IF*);
            END (*IF*);
            IF NOT INIData.INIGetString (hini, appSYS, "PSServer", PSServer) THEN
                Strings.Assign ("[127.0.0.1]", PSServer);
            END (*IF*);
            EVAL (INIData.INIGet (hini, appSYS, "PSPort", PSPort));
            IF NOT INIData.INIGetString (hini, appSYS, "PSUsername", PSUsername) THEN
                Strings.Assign ("", PSUsername);
            END (*IF*);
            IF NOT INIData.INIGetString (hini, appSYS, "PSPassword", PSPassword) THEN
                Strings.Assign ("", PSPassword);
            END (*IF*);
            EVAL (INIData.INIGetString (hini, appSYS, 'DefaultLanguage', DefaultLanguage));
            EVAL (INIData.INIGetString (hini, appSYS, 'AdminLanguage', AdminLanguage));

            INIData.CloseINIFile (hini);
        END (*IF*);

        SetBatchLimit (BatchLimit);
        SampleTime := 1000*SampleTime;   (* convert seconds to milliseconds *)
        SetDefaultLanguage (DefaultLanguage);
        SetArchiveRoot (ArchiveDir);
        DefaultLang := UseLanguage ("MM", DefaultLanguage);
        AdminLang := UseLanguage ("MM", AdminLanguage);
        SMTPOut.SetOurHostname (MailDomain);

        StartTransactionLogging (ListChecker.MMctx, LogFileName, LogLevel);

        IF ServerIsWeasel THEN

            ResolveRelativePath (OurDir, WeaselDir);
            Multidomain := FALSE;
            IF WeaselDir[0] <> Nul THEN

                (* Remark: Admin has ensured that WeaselDir ends with   *)
                (* the '\' character.                                   *)

                ININame := WeaselDir;

                (* The decision to use TNI here should depend on Weasel,    *)
                (* not on our TNI variable.                                 *)

                OS2.DosSetCurrentDir (WeaselDir);
                IF NOT INIData.ChooseDefaultINI("Weasel", UseWTNI) THEN
                    UseWTNI := UseTNI;
                END (*IF*);
                OS2.DosSetCurrentDir (OurDir);

                IF UseWTNI THEN
                    Strings.Append ("weasel.tni", ININame);
                ELSE
                    Strings.Append ("weasel.ini", ININame);
                END (*IF*);
                hini := INIData.OpenINIFile (ININame);
                IF INIData.INIValid (hini) THEN
                    EVAL (INIData.INIGet (hini, appSYS, "MultiDomainEnabled", Multidomain));
                    IF NOT INIData.INIGetString (hini, appSYS, "MailRoot", MailRoot) THEN
                        MailRoot := "";
                    END (*IF*);
                    INIData.CloseINIFile (hini);
                    ResolveRelativePath (WeaselDir, MailRoot);
                END (*IF*);

            END (*IF*);

            IF MailRoot[0] = Nul THEN
                MailRoot := OurDir;
                Strings.Append ("\MailRoot\", MailRoot);
            END (*IF*);

            Strings.Assign ("Log file is ", message);
            Strings.Append (LogFileName, message);
            LogTransaction (LogID, message);
            IF Exists (MailRoot) THEN

                Strings.Assign ("Mail root directory is ", message);
                Strings.Append (MailRoot, message);
            ELSE
                Strings.Assign ("ERROR: Mail directory ", message);
                Strings.Append (MailRoot, message);
                Strings.Append (" does not exist", message);
            END (*IF*);
            LogTransaction (LogID, message);

            IF Multidomain THEN
                EnableLocalOperation (MailRoot, MailDomain);
            ELSE
                EnableLocalOperation (MailRoot, "");
            END (*IF*);

        ELSE

            (* Server is not local Weasel. *)

            POPClient.SetServerAddress (POPServer, POPport, AdminLang);
            SMTPOut.SetSMTPAddress (SMTPServer, SMTPport, AdminLang);
            SMTPOut.SetPSParameters (AuthOption, PSServer, PSPort,
                                           PSUsername, PSPassword);
            DisableLocalOperation;
        END (*IF*);

        ListChecker.SetOurDomainName (MailDomain, LogDetail);
        SetAdminName (AdminName, AdminLoginName, AdminPassword,
                                AdminLang, DefaultLang, LogDetail,
                                AdminFilter, ReplyToErrors);
        Commands.SetHelpFileName (HelpFileName, PlainTextFileName);

    END LoadINIData;

(********************************************************************************)
(*                           MAIN SYSTEM ADMINISTRATOR                          *)
(********************************************************************************)

PROCEDURE RunManager;

    (* Here is where we run the main system administrator.  It periodically     *)
    (* checks for mail addressed to MajorMajor, and processes the requests.     *)
    (* Meanwhile, operations on the various lists are being handled by tasks    *)
    (* in other modules.                                                        *)

    VAR LogID: TransactionLogID;
        dummy: BOOLEAN;
        pos1: CARDINAL;
        buffer: ARRAY [0..79] OF CHAR;
        logmessage: ARRAY [0..255] OF CHAR;

    BEGIN
        LogID := CreateLogID (ListChecker.MMctx, "        ");

        IF ScreenEnabled THEN
            IF UseTNI THEN
                buffer := "[T]";
            ELSE
                buffer := "[I]";
            END (*IF*);
            WriteStringAt (0, 0, buffer);
            GetProgramName (buffer);
            WriteStringAt (0, 3, buffer);
            StrToBuffer (AdminLang, "Major.CtrlC", logmessage);
            pos1 := 79 - LENGTH(logmessage);
            WriteStringAt (0, (pos1 + LENGTH(buffer)) DIV 2 - 13,
                                           "(C) 2001-2020 Peter Moylan");
            WriteStringAt (0, pos1, logmessage);
        ELSE
            GetProgramName (buffer);
        END (*IF*);

        StrToBufferA (AdminLang, "Major.started", buffer, logmessage);
        LogTransaction (LogID, logmessage);

        logmessage := "exceptq support is ";
        IF ExceptqActive THEN
            Strings.Append ("present", logmessage);
        ELSE
            Strings.Append ("absent", logmessage);
        END (*IF*);
        LogTransaction (LogID, logmessage);
        IF UseTNI THEN
            LogTransactionL (LogID, "Getting configuration data from Major.tni");
        ELSE
            LogTransactionL (LogID, "Getting configuration data from Major.ini");
        END (*IF*);

        WHILE NOT ShutdownInProgress DO
            ProcessAdminRequests (LogID);
            TimedWait (DoAdminCheck, SampleTime, dummy);
        END (*WHILE*);

        (* End of operation, log a shutdown message. *)

        StrToBuffer (AdminLang, "Major.finishing", logmessage);
        LogTransaction (LogID, logmessage);

    END RunManager;

(********************************************************************************)
(*                   TASK TO FORCE A RE-CHECK OF ALL LISTS                      *)
(********************************************************************************)

PROCEDURE NewMailChecker;

    (* Runs as a separate task.  Forces a re-check of all mailing lists each    *)
    (* time a public event semaphore tells us that there's been a change.       *)
    (* (But we do nothing if ServerIsWeasel is FALSE.)                          *)

    CONST semName = "\SEM32\WEASEL\RECEIVED";

    VAR result: CARDINAL;
        LogID: TransactionLogID;
        logmessage: ARRAY [0..255] OF CHAR;

    BEGIN
        LogID := CreateLogID (ListChecker.MMctx, "        ");
        ForceFreshCheck := 0;
        result := OS2.DosOpenEventSem (semName, ForceFreshCheck);
        IF result = OS2.ERROR_SEM_NOT_FOUND THEN
            result := OS2.DosCreateEventSem (semName, ForceFreshCheck, OS2.DC_SEM_SHARED, FALSE);
        END (*IF*);

        WHILE NOT ShutdownInProgress DO
            WaitOnSemaphore (event, ForceFreshCheck);
            Sleep (500);
            IF ServerIsWeasel THEN
                IF ExtraLogging THEN
                    StrToBuffer (AdminLang, "Major.checkingnew", logmessage);
                    LogTransaction (LogID, logmessage);
                END (*IF*);
                Signal (DoAdminCheck);
                IF NOT ShutdownInProgress THEN
                    ListChecker.RecheckMailForAllLists;
                END (*IF*);
            END (*IF*);
        END (*WHILE*);

        OS2.DosCloseEventSem(ForceFreshCheck);
        Signal (TaskDone);

    END NewMailChecker;

(********************************************************************************)
(*                  TASK TO KEEP THE MAILING LISTS UP-TO-DATE                   *)
(********************************************************************************)

PROCEDURE ListUpdater;

    (* Runs as a separate task.  Recreates all the mailing lists each time      *)
    (* a public event semaphore tells us that there's been a change.            *)

    CONST semName = "\SEM32\MAJOR\UPDATED";

    BEGIN
        UpdaterFlag := 0;
        IF OS2.DosOpenEventSem (semName, UpdaterFlag) = OS2.ERROR_SEM_NOT_FOUND THEN
            OS2.DosCreateEventSem (semName, UpdaterFlag, OS2.DC_SEM_SHARED, FALSE);
        END (*IF*);

        WHILE NOT ShutdownInProgress DO
            WaitOnSemaphore (event, UpdaterFlag);
            IF NOT ShutdownInProgress THEN
                LoadINIData (INILoadLogID, ExtraLogging);
                ListChecker.RegisterAllLists;
            END (*IF*);
        END (*WHILE*);

        OS2.DosCloseEventSem(UpdaterFlag);
        Signal (TaskDone);

    END ListUpdater;

(********************************************************************************)
(*                                 MAIN PROGRAM                                 *)
(********************************************************************************)

PROCEDURE main;

    (* We make this a separate procedure to keep the Exceptq stuff  *)
    (* in one place.                                                *)

    VAR exRegRec: OS2.EXCEPTIONREGISTRATIONRECORD;

    BEGIN
        ExceptqActive := InstallExceptq (exRegRec);
        RunManager;
        IF ExceptqActive THEN
            UninstallExceptq (exRegRec);
        END (*IF*);
    END main;

(********************************************************************************)

BEGIN
    ScreenEnabled := NotDetached();
    ShutdownInProgress := FALSE;
    ServerIsWeasel := FALSE;
    ExtraLogging := FetchCommandLineParameters(UseTNI);
    IF UseTNI THEN
        INIFileName := "MAJOR.TNI";
    ELSE
        INIFileName := "MAJOR.INI";
    END (*IF*);
    ListChecker.SetININame (INIFileName);
    INILoadLogID := CreateLogID (ListChecker.MMctx, "INILoad ");
    LoadINIData (INILoadLogID, ExtraLogging);
    ListChecker.RegisterAllLists;
    CreateSemaphore (DoAdminCheck, 0);
    CreateSemaphore (TaskDone, 0);
    EVAL (CreateTask (ListUpdater, 2, "update"));
    CreateSemaphore (CommenceShutdown, 0);
    EVAL (SetBreakHandler (ControlCHandler));
    EVAL (CreateTask (ShutterDowner, 2, "shutterdowner"));
    EVAL (CreateTask (NewMailChecker, 2, "check mail in"));
    EVAL (CreateTask (ExternalShutdownRequestDetector, 2, "shutdown"));
    main;
    OS2.DosPostEventSem (UpdaterFlag);          (* to terminate ListUpdater *)
    OS2.DosPostEventSem (ForceFreshCheck);      (* to terminate NewMailChecker *)
    OS2.DosPostEventSem (ShutdownSignal);       (* external shutdown detector *)
    Wait (TaskDone);
    Wait (TaskDone);
    Wait (TaskDone);
    Wait (TaskDone);
END Major.

