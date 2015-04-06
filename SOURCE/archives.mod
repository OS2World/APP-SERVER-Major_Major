(**************************************************************************)
(*                                                                        *)
(*  The Major Major mailing list manager                                  *)
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

IMPLEMENTATION MODULE Archives;

        (********************************************************)
        (*                                                      *)
        (*                Mailing list manager                  *)
        (*         Module to deal with the archive files        *)
        (*               for the mailing lists                  *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            12 January 2000                 *)
        (*  Last edited:        22 December 2013                *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings, OS2;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId, FilenameString, DirectoryEntry,
    (* proc *)  FirstDirEntry, NextDirEntry, DirSearchDone,
                OpenNewFile, OpenOldFile, OpenAtEnd, CloseFile, MoveFile,
                GetFileSize, DeleteFile,
                ReadRaw, ReadLine, WriteRaw,
                FWriteChar, FWriteString, FWriteLn,
                FWriteCard64, FWriteZCard;

FROM AddressLists IMPORT
    (* type *)  ListNameType, AddressList;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  LWriteStringA, LWriteStringN, StrToBufferA, StrToBufferAB;

FROM TimeConv IMPORT
    (* proc *)  millisecs, SubDateDays;

FROM SysClock IMPORT
    (* type *)  DateTime,
    (* proc *)  GetClock;

FROM MyClock IMPORT
    (* proc *)  AppendDateString;

FROM Misc IMPORT
    (* proc *)  HeadMatch;

FROM Types IMPORT
    (* type *)  CARD64;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  LogTransaction;

FROM Timer IMPORT
    (* proc *)  TimedWait;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, Wait, Signal;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release, CreateTask;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);
    SeparatorFileName = 'archives/separator';

TYPE
    (* An archive record has the fields                          *)
    (*    access         critical section lock                   *)
    (*    lang           handle for the list language            *)
    (*    TimePerUpdate  time between making archive files       *)
    (*    TimeUnit       unit of TimePerUpdate                   *)
    (*    DigestSemaphore  semaphore on which we signal each     *)
    (*                     time we've created a new digest file  *)
    (*    directory      directory to hold the archive           *)
    (*    currentfile    filename of the partially built archive *)
    (*    DigestFile     most recent complete archive            *)
    (*    count          number of articles in currentfile       *)
    (*    LogID          transaction log ID                      *)
    (*    obsolete       to tell the scheduler to remove this    *)
    (*                      record                               *)

    Archive = POINTER TO
                       RECORD
                           access: Lock;
                           lang: LangHandle;
                           TimePerUpdate: CARDINAL;
                           TimeUnit: TimeType;
                           DigestSemaphore: Semaphore;
                           directory: FilenameString;
                           currentfile: FilenameString;
                           DigestFile: FilenameString;
                           count: CARDINAL;
                           LogID: TransactionLogID;
                           obsolete: BOOLEAN;
                       END (*RECORD*);

    (* An event list record has the fields                      *)
    (*    next       next item on the list                      *)
    (*    daystogo   number of days before this event is due    *)
    (*    archive    the archive to be updated on this event    *)
    (* The 'daystogo' information is stored in incremental form *)
    (* i.e. it's the number of days beyond the due date for     *)
    (* the previous event on the list.                          *)

    EventListPtr = POINTER TO
                       RECORD
                           next: EventListPtr;
                           daystogo: INTEGER;
                           archive: Archive;
                       END (*RECORD*);

VAR
    (* The root of the archive directory. *)

    ArchiveRoot: FilenameString;

    (* The event list. *)

    EventList: RECORD
                   access: Lock;
                   head: EventListPtr;
               END (*RECORD*);

    (* We signal on this semaphore to wake up the task that checks      *)
    (* the event list.                                                  *)

    CheckEventList: Semaphore;

    (* The language to use for administrator functions, e.g. logging. *)

    AdminLang: LangHandle;

    (* Variables associated with shutdown. *)

    ShutdownRequest: BOOLEAN;
    TaskDone: Semaphore;

(************************************************************************)
(*                         DIRECTORY LISTING                            *)
(************************************************************************)

PROCEDURE WriteDate (cid: ChanId;  datecode, timecode: CARDINAL);

    (* Decodes a file date and time. *)

    VAR day, month, year, minute, hour: CARDINAL;

    BEGIN
        day := datecode MOD 32;  datecode := datecode DIV 32;
        month := datecode MOD 16;  year := 1980 + datecode DIV 16;

        FWriteString (cid, "  ");
        FWriteZCard (cid, year, 4);
        FWriteChar (cid, '-');
        FWriteZCard (cid, month, 2);
        FWriteChar (cid, '-');
        FWriteZCard (cid, day, 2);

        FWriteString (cid, "  ");

        timecode := timecode DIV 32;
        minute := timecode MOD 64;  hour := timecode DIV 64;
        FWriteZCard (cid, hour, 2);
        FWriteChar (cid, ':');
        FWriteZCard (cid, minute, 2);

    END WriteDate;

(************************************************************************)

PROCEDURE FileListing (lang: LangHandle;  cid: ChanId;
                                          ListName: ListNameType);

    (* Returns a directory listing of the directory holding the *)
    (* archives for this list.                                  *)

    VAR D: DirectoryEntry;
        mask: FilenameString;
        count: CARDINAL;  found, sendit: BOOLEAN;

    BEGIN
        LWriteStringA (lang, cid, "Archives.list.heading", ListName);
        FWriteLn (cid);
        FWriteLn (cid);
        count := 0;
        Strings.Assign ("archives\", mask);
        Strings.Append (ListName, mask);
        Strings.Append ("\*", mask);
        found := FirstDirEntry (mask, FALSE, FALSE, D);
        WHILE found DO
            sendit := D.name[0] <> '.';
            IF NOT sendit THEN
                IF D.name[1] <> '.' THEN
                    sendit := (D.name[1] <> Nul);
                ELSE
                    sendit := D.name[2] <> Nul;
                END (*IF*);
            END (*IF*);
            IF sendit THEN
                INC (count);
                WriteDate (cid, D.datePkd, D.timePkd);
                FWriteCard64 (cid, D.size, 12);
                FWriteString (cid, "      ");
                FWriteString (cid, D.name);
                FWriteLn (cid);
            END (*IF*);
            found := NextDirEntry (D);
        END (*WHILE*);
        DirSearchDone (D);
        FWriteLn (cid);
        LWriteStringN (lang, cid, "Archives.list.count", count);
        FWriteLn (cid);
    END FileListing;

(************************************************************************)
(*                            SENDING A FILE                            *)
(************************************************************************)

PROCEDURE AppendFromArchive (dstcid: ChanId;
                       VAR (*IN*) listname, file: ARRAY OF CHAR): BOOLEAN;

    (* Appends the contents of the named file to dstcid (an output file *)
    (* that is already open).  Returns FALSE if the file is not         *)
    (* available.                                                       *)

    VAR srccid: ChanId;  exists: BOOLEAN;

    (********************************************************************)

    VAR filename: FilenameString;
        Buffer: ARRAY [0..2048] OF CHAR;
        amount: CARDINAL;

    BEGIN
        Strings.Assign ("archives\", filename);
        Strings.Append (listname, filename);
        Strings.Append ("\", filename);
        Strings.Append (file, filename);
        srccid := OpenOldFile (filename, FALSE, TRUE);
        exists := srccid <> NoSuchChannel;
        IF exists THEN
            REPEAT
                ReadRaw (srccid, Buffer, SIZE(Buffer), amount);
                IF amount > 0 THEN
                    WriteRaw (dstcid, Buffer, amount);
                END (*IF*);
            UNTIL amount = 0;
            CloseFile (srccid);
        END (*IF*);

        RETURN exists;

    END AppendFromArchive;

(************************************************************************)
(*               ADDING AN ARCHIVE EVENT TO THE EVENT LIST              *)
(************************************************************************)

PROCEDURE ScheduleEvent (A: Archive;  TimeToGo: CARDINAL);

    (* Creates a new event and inserts it into the event list. *)

    VAR Now: DateTime;

    (********************************************************************)

    PROCEDURE LastDayInMonth(): CARDINAL;

        (* Returns the last day number in Now.month. *)

        TYPE MonthTable = ARRAY [1..12] OF CARDINAL;
        CONST Table = MonthTable{31,28,31,30,31,30,31,31,30,31,30,31};

        VAR result: CARDINAL;

        BEGIN
            result := Table[Now.month];
            IF (Now.month = 2) AND (Now.year MOD 4 = 0) THEN
                INC (result);
            END (*IF*);
            RETURN result;
        END LastDayInMonth;

    (********************************************************************)

    VAR E, previous, current: EventListPtr;

    BEGIN
        NEW (E);
        E^.next := NIL;
        IF A^.TimeUnit = months THEN
            GetClock (Now);
            E^.daystogo := LastDayInMonth() - Now.day;
            WHILE TimeToGo > 0 DO
                IF Now.month = 12 THEN
                    Now.month := 1;
                    INC (Now.year);
                ELSE
                    INC (Now.month);
                END (*IF*);
                INC (E^.daystogo, LastDayInMonth());
                DEC (TimeToGo);
            END (*WHILE*);
        ELSE
            E^.daystogo := TimeToGo;
        END (*IF*);
        E^.archive := A;

        (* Now find the right insertion point on the event list. *)

        Obtain (EventList.access);
        previous := NIL;  current := EventList.head;
        LOOP
            IF (current = NIL) OR (current^.daystogo > E^.daystogo) THEN
                EXIT (*LOOP*);
            END (*IF*);
            DEC (E^.daystogo, current^.daystogo);
            previous := current;  current := current^.next;
        END (*LOOP*);

        (* Perform the insertion. *)

        IF previous = NIL THEN
            EventList.head := E;
        ELSE
            previous^.next := E;
        END (*IF*);
        E^.next := current;
        IF current <> NIL THEN
            DEC (current^.daystogo, E^.daystogo);
        END (*IF*);

        Release (EventList.access);

    END ScheduleEvent;

(************************************************************************)
(*              ACTIONS WHEN THE ARCHIVING TIME IS REACHED              *)
(************************************************************************)

PROCEDURE FinalClose (VAR (*INOUT*) A: Archive);

    (* To close an archive, we first call DeregisterArchive, which in   *)
    (* effect tells the scheduler to do the removal of the data         *)
    (* structure.  The scheduler then calls this procedure.             *)

    BEGIN
        IF A <> NIL THEN
            DestroyLock (A^.access);
            DISPOSE (A);
        END (*IF*);
    END FinalClose;

(************************************************************************)

PROCEDURE StartNewArchiveFile (A: Archive);

    (* This procedure is called when it is time to make a new archive   *)
    (* file.  We perform the following actions:                         *)
    (*  - rename the present temporary archive file to a name based on  *)
    (*    the current date, or delete the temporary file if it is empty *)
    (*  - if A^.obsolete is set, remove A from the system               *)
    (*  - if A^.obsolete is not set, create a new (initially empty)     *)
    (*    temporary archive file, and put an entry back on the event    *)
    (*    list, to ensure that this procedure will again be called      *)
    (*    when it is time to do this job again.                         *)

    VAR NewName: FilenameString;
        moved, MustRemove: BOOLEAN;
        cid: ChanId;
        buffer: ARRAY [0..255] OF CHAR;

    BEGIN
        Obtain (A^.access);

        moved := FALSE;
        IF A^.count = 0 THEN
            DeleteFile (A^.currentfile);
        ELSE
            Strings.Assign (A^.directory, NewName);
            Strings.Append ('\', NewName);
            AppendDateString (NewName);
            Strings.Append (".arc", NewName);

            moved := MoveFile (A^.currentfile, NewName);
            IF moved THEN
                StrToBufferA (AdminLang, "Archives.log.created",
                                                  NewName, buffer);
            ELSE
                StrToBufferAB (AdminLang, "Archives.log.createfail",
                                         A^.currentfile, NewName, buffer);
            END (*IF*);
            LogTransaction (A^.LogID, buffer);

        END (*IF*);

        MustRemove := A^.obsolete;

        IF MustRemove THEN
            Release (A^.access);
            FinalClose (A);
        ELSE
            Strings.Assign (A^.directory, A^.currentfile);
            Strings.Append ("\arc.tmp", A^.currentfile);
            A^.count := 0;

            (* It is desirable to create the new file right now, even   *)
            (* though it will be an initially empty file, so that the   *)
            (* creation date is correctly recorded.                     *)

            cid := OpenNewFile (A^.currentfile, FALSE);
            CloseFile (cid);
            ScheduleEvent (A, A^.TimePerUpdate);
            IF moved THEN
                A^.DigestFile := NewName;
                Signal (A^.DigestSemaphore);
            END (*IF*);
            Release (A^.access);
        END (*IF*);

    END StartNewArchiveFile;

(************************************************************************)
(*                    INITIATING ARCHIVING OF A LIST                    *)
(************************************************************************)

PROCEDURE AgeOfFile (filename: FilenameString;  unit: TimeType): CARDINAL;

    (* Returns the age, in the specified time unit, of this file.       *)
    (* A negative age will be reported as zero.                         *)

    VAR D: DirectoryEntry;  datecode, age: CARDINAL;
        diff: INTEGER;
        FileDate, Now: DateTime;

    BEGIN
        IF FirstDirEntry (filename, FALSE, FALSE, D) THEN
            GetClock (Now);
            FileDate := Now;

            (* Important: for our current purposes, we must work with   *)
            (* the file creation date, not the "last write" date.  We   *)
            (* want to know the total age, not the age since the last   *)
            (* modification.                                            *)

            datecode := D.dateCre;
            WITH FileDate DO
                day := datecode MOD 32;  datecode := datecode DIV 32;
                month := datecode MOD 16;  year := 1980 + datecode DIV 16;
            END (*WITH*);
            IF unit = days THEN
                age := SubDateDays (Now, FileDate);
            ELSIF unit = months THEN
                diff := 12*(Now.year - FileDate.year)
                                        + Now.month - FileDate.month;
                IF diff < 0 THEN
                    age := 0;
                ELSE
                    age := diff;
                END (*IF*);
            ELSE
                (* A case we don't yet handle. *)
                age := 0;
            END (*IF*);
        ELSE
            age := 0;
        END (*IF*);
        DirSearchDone (D);
        RETURN age;
    END AgeOfFile;

(************************************************************************)

PROCEDURE RegisterArchive (listname: ARRAY OF CHAR;
                           lang: LangHandle;
                           TimeBetweenUpdates: CARDINAL;
                           TimeUnit: TimeType;
                           DigestSignal: Semaphore;
                           LogID: TransactionLogID): Archive;

    (* Returns a handle to the archive belonging to this list. *)

    VAR A: Archive;  delay: CARDINAL;  size: CARD64;

    BEGIN
        NEW (A);
        CreateLock (A^.access);
        A^.lang := lang;
        A^.TimePerUpdate := TimeBetweenUpdates;
        A^.TimeUnit := TimeUnit;
        Strings.Assign ("archives\", A^.currentfile);
        Strings.Append (listname, A^.currentfile);
        Strings.Assign (A^.currentfile, A^.directory);
        Strings.Append ("\arc.tmp", A^.currentfile);
        A^.DigestFile := "";

        size := GetFileSize (A^.currentfile);
        IF (size.high = 0) AND (size.low = 0) THEN
            A^.count := 0;
        ELSE

            (* We don't know the exact count, but all that really       *)
            (* matters is that we remember that it's nonzero.           *)

            A^.count := 1;
        END (*IF*);
        A^.LogID := LogID;
        A^.obsolete := FALSE;
        A^.DigestSemaphore := DigestSignal;

        delay := AgeOfFile (A^.currentfile, A^.TimeUnit);
        IF delay > A^.TimePerUpdate THEN

            (* Archiving is overdue, so make the file right now *)
            (* rather than waiting until midnight.              *)

            StartNewArchiveFile (A);

        ELSE
            ScheduleEvent (A, A^.TimePerUpdate - delay);
        END (*IF*);

        RETURN A;

    END RegisterArchive;

(************************************************************************)
(*                        CLOSING AN ARCHIVE                            *)
(************************************************************************)

PROCEDURE DeregisterArchive (VAR (*INOUT*) A: Archive);

    (* Stops archiving for this archive.  The files that have already   *)
    (* been created are retained, but the caller will not longer be     *)
    (* able to call AddToArchive.                                       *)

    VAR previous, current: EventListPtr;

    BEGIN
        IF A <> NIL THEN

            (* First pull this archive off the event list. *)

            Obtain (EventList.access);
            previous := NIL;  current := EventList.head;
            WHILE (current <> NIL) AND (current^.archive <> A) DO
                previous := current;  current := current^.next;
            END (*WHILE*);

            IF current <> NIL THEN
                IF current^.next <> NIL THEN
                    INC (current^.next^.daystogo, current^.daystogo);
                END (*IF*);
                IF previous = NIL THEN
                    EventList.head := current^.next;
                ELSE
                    previous^.next := current^.next;
                END (*IF*);
                DISPOSE (current);
            END (*IF*);

            Release (EventList.access);

            (* Next, mark it as obsolete and put it back on the *)
            (* head of the event list.  The scheduler will look *)
            (* after the final deletion of A.                   *)

            Obtain (A^.access);
            A^.obsolete := TRUE;
            Release (A^.access);
            ScheduleEvent (A, 0);
            A := NIL;

        END (*IF*);

    END DeregisterArchive;

(************************************************************************)
(*                      ADDING A FILE TO AN ARCHIVE                     *)
(************************************************************************)

PROCEDURE ConvHex (ch: CHAR;  VAR (*OUT*) val: CARDINAL): BOOLEAN;

    (* If ch is a hexadecimal digit, returns its value in val and       *)
    (* returns TRUE.  Otherwise returns FALSE and val is undefined.     *)

    TYPE CharSet = SET OF CHAR;

    BEGIN
        ch := CAP(ch);
        IF ch IN CharSet{'0'..'9'} THEN
            val := ORD(ch) - ORD('0');
            RETURN TRUE;
        ELSIF ch IN CharSet{'A'..'F'} THEN
            val := ORD(ch) - ORD('A') + 10;
            RETURN TRUE;
        ELSE
            RETURN FALSE;
        END (*IF*);
    END ConvHex;

(************************************************************************)

PROCEDURE GetHex2 (line: ARRAY OF CHAR;  pos: CARDINAL;
                                         VAR (*OUT*) val: CHAR): BOOLEAN;

    (* If line[pos..pos+1] contains a hexadecimal number, returns the   *)
    (* value in val and returns TRUE.  Otherwise returns FALSE and      *)
    (* val is left unaltered.                                           *)

    VAR v1, v2: CARDINAL;  result: BOOLEAN;

    BEGIN
        v2 := 0;
        result := ConvHex(line[pos], v1) AND ConvHex(line[pos+1], v2);
        IF result THEN
            val := CHR(16*v1 + v2);
        END (*IF*);
        RETURN result;
    END GetHex2;

(************************************************************************)

PROCEDURE CopyLine (line: ARRAY OF CHAR;  dst: ChanId;  QPdecode: BOOLEAN);

    (* Writes a line of text to a file.  If QPdecode is TRUE, replaces  *)
    (* quoted-printable codes by 8-bit encoding.                        *)

    VAR len, pos: CARDINAL;  found, AddEOL: BOOLEAN;

    BEGIN
        AddEOL := TRUE;
        len := Strings.Length (line);

        IF QPdecode THEN

            (* First remove trailing spaces. *)

            WHILE (len > 0) AND (line[len-1] = ' ') DO
                DEC (len);  line[len] := Nul;
            END (*WHILE*);

            (* '=' as last character means soft line break. *)

            IF (len > 0) AND (line[len-1] = '=') THEN
                DEC (len);  line[len] := Nul;
                AddEOL := FALSE;
            END (*IF*);

            (* Now the actual quoted-printable decoding.  *)

            pos := 0;
            REPEAT
                Strings.FindNext ('=', line, pos, found, pos);
                IF found THEN
                    INC (pos);
                    IF GetHex2 (line, pos, line[pos-1]) THEN
                        Strings.Delete (line, pos, 2);
                        DEC (len, 2);
                    END (*IF*);
                END (*IF*);
            UNTIL NOT found;

        END (*IF*);

        (* Write what's left over after the conversion. *)

        IF len > 0 THEN
            FWriteString (dst, line);
        END (*IF*);

        IF AddEOL THEN
            FWriteLn (dst);
        END (*IF*);

    END CopyLine;

(************************************************************************)

PROCEDURE AppendFile (src, dst: FilenameString);

    (* Appends src to the end of dst.  In the process we remove the     *)
    (* 'uninteresting' header lines from src, and undo quoted-printable *)
    (* encoding if any.                                                 *)

    CONST CtrlZ = CHR(26);

    VAR srccid, dstcid: ChanId;  QPdecode: BOOLEAN;
        pos: CARDINAL;
        buffer: ARRAY [0..1023] OF CHAR;

    BEGIN
        QPdecode := FALSE;
        dstcid := OpenAtEnd (dst);
        IF dstcid <> NoSuchChannel THEN
            srccid := OpenOldFile (src, FALSE, FALSE);
            IF srccid <> NoSuchChannel THEN

                (* Read the header lines, throw away most of them. *)

                LOOP
                    ReadLine (srccid, buffer);
                    IF (buffer[0] = Nul) OR (buffer[0] = CtrlZ) THEN EXIT(*LOOP*) END(*IF*);
                    IF HeadMatch (buffer, "Content-Transfer-Encoding:") THEN
                        Strings.Capitalize (buffer);
                        Strings.FindNext ('QUOTED-PRINTABLE', buffer, 0, QPdecode, pos);
                    ELSIF HeadMatch (buffer, "From:")
                             OR HeadMatch (buffer, "Date:")
                             OR HeadMatch (buffer, "Subject:") THEN
                        FWriteString (dstcid, buffer);
                        FWriteLn (dstcid);
                    END (*IF*);
                END (*LOOP*);
                FWriteLn (dstcid);

                (* Now we're in the body of the message. *)

                LOOP
                    ReadLine (srccid, buffer);
                    IF buffer[0] = CtrlZ THEN EXIT(*LOOP*) END(*IF*);
                    CopyLine (buffer, dstcid, QPdecode);
                END (*LOOP*);

                CloseFile (srccid);
            END (*IF*);
            CloseFile (dstcid);
        END (*IF*);
    END AppendFile;

(************************************************************************)

PROCEDURE AddToArchive (A: Archive;  filename: FilenameString);

    (* Adds the given file to the archive. *)

    BEGIN
        Obtain (A^.access);
        IF A^.count > 0 THEN
            OS2.DosCopy (SeparatorFileName, A^.currentfile, OS2.DCPY_APPEND);
        END (*IF*);
        AppendFile (filename, A^.currentfile);
        INC (A^.count);
        Release (A^.access);
    END AddToArchive;

(************************************************************************)

PROCEDURE AppendDigest (A: Archive;  target: FilenameString);

    (* Appends the contents of the current digest to target.    *)

    BEGIN
        Obtain (A^.access);
        IF A^.DigestFile[0] <> Nul THEN
            OS2.DosCopy (A^.DigestFile, target, OS2.DCPY_APPEND);
        END (*IF*);
        Release (A^.access);
    END AppendDigest;

(************************************************************************)
(*                      HANDLING AN EVENTLIST EVENT                     *)
(************************************************************************)

PROCEDURE HandleEvent (VAR (*INOUT*) E: EventListPtr);

    (* The event to be handled is the arrival of the time to make a     *)
    (* new archive file.  We rename the present temporary archive file  *)
    (* and start a new one.  In addition, we put an entry back onto     *)
    (* the event list.                                                  *)

    VAR A: Archive;

    BEGIN
        A := E^.archive;
        DISPOSE (E);
        StartNewArchiveFile (A);
    END HandleEvent;

(************************************************************************)
(*                      THE MAIN LIST CHECKER TASK                      *)
(************************************************************************)

PROCEDURE ListCheckerTask;

    (* Runs as a separate task.  Wakes itself up each day, at half past *)
    (* midnight, to see whether something is scheduled to be done.      *)

    CONST
        MillisecondsPerDay = 24 * 60 * 60 * 1000;
        TimePastMidnight = 30 * 60 * 1000;

    VAR TimeToSleep, Now: CARDINAL;  TimedOut: BOOLEAN;
        L: EventListPtr;

    BEGIN
        Now := millisecs() MOD MillisecondsPerDay;
        IF Now > TimePastMidnight THEN
            TimeToSleep := MillisecondsPerDay + TimePastMidnight - Now;
        ELSE
            TimeToSleep := TimePastMidnight - Now;
        END (*IF*);

        LOOP
            (* Wait until half past midnight. *)

            TimedWait (CheckEventList, TimeToSleep, TimedOut);
            IF ShutdownRequest THEN
                EXIT (*LOOP*);
            END (*IF*);

            Obtain (EventList.access);
            L := EventList.head;
            IF L <> NIL THEN
                IF L^.daystogo > 0 THEN
                    DEC (L^.daystogo);
                END (*IF*);
                WHILE (L <> NIL) AND (L^.daystogo = 0) DO
                    EventList.head := L^.next;
                    Release (EventList.access);
                    HandleEvent (L);
                    Obtain (EventList.access);
                    L := EventList.head;
                END (*WHILE*);
            END (*IF*);
            Release (EventList.access);

            Now := millisecs() MOD MillisecondsPerDay;
            TimeToSleep := MillisecondsPerDay - Now + TimePastMidnight;

        END (*LOOP*);

        Signal (TaskDone);

    END ListCheckerTask;

(************************************************************************)
(*                          INITIALISATION                              *)
(************************************************************************)

PROCEDURE SetLoggingLanguage (lang: LangHandle);

    (* Sets the language to be used for the transaction log. *)

    BEGIN
        AdminLang := lang;
    END SetLoggingLanguage;

(************************************************************************)

PROCEDURE SetArchiveRoot (root: ARRAY OF CHAR);

    (* Sets the root of the archive directory tree. *)

    BEGIN
        Strings.Assign (root, ArchiveRoot);
    END SetArchiveRoot;

(************************************************************************)

BEGIN
    ShutdownRequest := FALSE;
    Strings.Assign ("archives", ArchiveRoot);
    CreateSemaphore (TaskDone, 0);
    WITH EventList DO
        CreateLock (access);
        head := NIL;
    END (*WITH*);
    CreateSemaphore (CheckEventList, 0);
    EVAL(CreateTask (ListCheckerTask, 3, "archive checker"));
FINALLY
    ShutdownRequest := TRUE;
    Signal (CheckEventList);
    Wait (TaskDone);
END Archives.

