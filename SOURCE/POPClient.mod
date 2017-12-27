(**************************************************************************)
(*                                                                        *)
(*  The Major Major mailing list manager                                  *)
(*  Copyright (C) 2017   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE POPClient;

        (********************************************************)
        (*                                                      *)
        (*        Fetches inbound mail from a POP3 server       *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            7 August 2000                   *)
        (*  Last edited:        11 June 2017                    *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings;

FROM Names IMPORT
    (* type *)  HostName;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  LogTransaction;

FROM FileOps IMPORT
    (* type *)  ChanId, FilenameString,
    (* proc *)  FWriteString, FWriteLn, CloseFile;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM SBuffers IMPORT
    (* type *)  SBuffer,
    (* proc *)  CreateSBuffer, CloseSBuffer, PositiveResponse,
                SendLine, FlushOutput, GetLine, GetLastLine;

FROM Sockets IMPORT
    (* const*)  AF_INET, SOCK_STREAM, AF_UNSPEC, NotASocket,
    (* type *)  Socket, SockAddr,
    (* proc *)  socket, connect, soclose;

FROM Internet IMPORT
    (* const*)  Zero8,
    (* proc *)  inet_addr;

FROM NetDB IMPORT
    (* type *)  HostEntPtr,
    (* proc *)  gethostbyname;

FROM Inet2Misc IMPORT
    (* proc *)  Swap2, WaitForSocket, NameIsNumeric;

FROM Misc IMPORT
    (* proc *)  OpenNewOutputFile;

FROM SplitScreen IMPORT
    (* proc *)  WriteString, WriteLn;

FROM Conversions IMPORT
    (* proc *)  CardinalToStringLJ;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);  CR = CHR(13);  LF = CHR(10);

TYPE
    ResponseBufferSubscript = [0..127];

    (* State of a user.                                                 *)
    (*    LoggedIn       the user is logged into the POP3 server        *)
    (*    socket         socket in use for talking to the server        *)
    (*    SB             SBuffer for server communications              *)
    (*    loginname      username for POP3 login                        *)
    (*    password       POP3 password                                  *)
    (*    MessageNumber  number of current message                      *)
    (*    MessageCount   number of messages on server                   *)
    (*    ResponseBuffer buffers data from the server                   *)
    (*    RBpos          where we're up to in the response buffer       *)
    (*    RBlength       number of characters in the response buffer    *)

    POP3User = POINTER TO
                   RECORD
                       LoggedIn: BOOLEAN;
                       socket: Socket;
                       SB: SBuffer;
                       loginname: ARRAY [0..255] OF CHAR;
                       password: ARRAY [0..31] OF CHAR;
                       MessageNumber: CARDINAL;
                       MessageCount: CARDINAL;
                       ResponseBuffer:
                              ARRAY ResponseBufferSubscript OF CHAR;
                       RBpos, RBlength: CARDINAL;
                   END (*RECORD*);

(************************************************************************)

VAR
    (* Admin language for log messages.  *)

    OurLang: LangHandle;

    (* IP address and POP port number of the POP3 server. *)

    IPaddress, POPport: CARDINAL;

    (* IPaddressKnown is TRUE iff IPaddress holds an address that we    *)
    (* consider to be valid.                                            *)

    IPaddressKnown: BOOLEAN;

    (* The POP3 hostname, used only if IPaddressKnown is FALSE. *)

    POPhost: HostName;

(************************************************************************)
(*                            SOCKET INPUT                              *)
(************************************************************************)

PROCEDURE OKReply (U: POP3User): BOOLEAN;

    (* Gets response line from server, checks for '+'. *)

    VAR LostConnection: BOOLEAN;

    BEGIN
        RETURN PositiveResponse (U^.SB, LostConnection);
    END OKReply;

(************************************************************************)
(*                           SOCKET OUTPUT                              *)
(************************************************************************)

PROCEDURE SendCommand (U: POP3User;  part1, part2: ARRAY OF CHAR): BOOLEAN;

    (* Sends a line to the server consisting of part1, a space char,    *)
    (* part2, and finally a CRLF.  If part2 is the empty string then    *)
    (* the space character and part2 are omitted.                       *)

    VAR buffer: ARRAY [0..511] OF CHAR;  sent: CARDINAL;  success: BOOLEAN;

    BEGIN
        Strings.Assign (part1, buffer);
        IF part2[0] <> Nul THEN
            Strings.Append (' ', buffer);
            Strings.Append (part2, buffer);
        END (*IF*);
        success := SendLine (U^.SB, buffer, sent);
        INC (sent, FlushOutput (U^.SB));
        RETURN success AND (sent = Strings.Length(buffer)+2);
    END SendCommand;

(************************************************************************)

PROCEDURE ExecCommand (U: POP3User;  part1, part2: ARRAY OF CHAR): BOOLEAN;

    (* Like SendCommand, but also checks for a positive reply. *)

    BEGIN
        RETURN SendCommand (U, part1, part2) AND OKReply(U);
    END ExecCommand;

(************************************************************************)
(*                           LOGIN / LOGOUT                             *)
(************************************************************************)

PROCEDURE EstablishConnection(): Socket;

    (* Opens a connection, returns a socket number.  The returned value *)
    (* is NotASocket if the attempt fails.                              *)

    VAR s: Socket;  peer: SockAddr;
        p: HostEntPtr;

    BEGIN
        IF NOT IPaddressKnown THEN
            p := gethostbyname (POPhost);
            IF (p = NIL) OR (p^.h_addr_list = NIL)
                         OR (p^.h_addr_list^[0] = NIL) THEN
                IPaddress := 0;
            ELSE
                IPaddress := p^.h_addr_list^[0]^;
            END (*IF*);
        END (*IF*);
        IF IPaddress <> 0 THEN
            s := socket (AF_INET, SOCK_STREAM, AF_UNSPEC);

            IF s <> NotASocket THEN

                (* Socket open, connect to the server. *)

                WITH peer DO
                    family := AF_INET;
                    WITH in_addr DO
                        port := Swap2(POPport);
                        addr := IPaddress;
                        zero := Zero8;
                    END (*WITH*);
                END (*WITH*);

                IF connect (s, peer, SIZE(peer)) THEN
                    soclose(s);
                    s := NotASocket;
                END (*IF*);

            END (*IF*);

        ELSE
            s := NotASocket;
        END (*IF*);

        RETURN s;

    END EstablishConnection;

(************************************************************************)

PROCEDURE Logout (U: POP3User);

    (* Logs out of the POP3 server. *)

    BEGIN
        IF U^.LoggedIn THEN
            EVAL (ExecCommand (U, "QUIT", ""));
            CloseSBuffer (U^.SB);
            U^.socket := NotASocket;
            U^.MessageCount := 0;
            U^.LoggedIn := FALSE;
        END (*IF*);
    END Logout;

(************************************************************************)

PROCEDURE GetMessageCount (U: POP3User): BOOLEAN;

    (* Works out how many messages are on the server. *)

    TYPE CharSet = SET OF CHAR;
    CONST Digits = CharSet {'0'..'9'};

    VAR line: ARRAY [0..511] OF CHAR;
        j, result: CARDINAL;  success: BOOLEAN;

    BEGIN
        result := 0;
        success := SendCommand (U, "STAT", "") AND GetLine(U^.SB, line);
        IF success THEN
            j := 0;
            WHILE (line[j] <> Nul) AND (line[j] <> ' ') DO
                INC (j);
            END (*WHILE*);
            WHILE line[j] = ' ' DO
                INC (j);
            END (*WHILE*);
            WHILE line[j] IN Digits DO
                result := 10*result + ORD(line[j]) - ORD('0');
                INC (j);
            END (*WHILE*);
        END (*IF*);
        U^.MessageCount := result;
        RETURN success;
    END GetMessageCount;

(************************************************************************)

PROCEDURE Login (U: POP3User;  LogID: TransactionLogID);

    (* Connects to the POP3 server, logs in, works out how many         *)
    (* messages are on the server.                                      *)

    VAR success: BOOLEAN;
        ErrorMessage: ARRAY [0..255] OF CHAR;

    BEGIN
        U^.LoggedIn := FALSE;
        success := FALSE;
        U^.socket := EstablishConnection();
        IF U^.socket = NotASocket THEN
            StrToBuffer (OurLang, "POPClient.NoConnection", ErrorMessage);
            LogTransaction (LogID, ErrorMessage);
        ELSE
            U^.LoggedIn := TRUE;
            U^.SB := CreateSBuffer (U^.socket, FALSE);
            IF OKReply(U) THEN
                IF ExecCommand (U, "USER", U^.loginname) THEN
                    IF ExecCommand (U, "PASS", U^.password) THEN
                        success := GetMessageCount(U);
                    ELSE
                        StrToBuffer (OurLang, "POPClient.BadPassword", ErrorMessage);
                        LogTransaction (LogID, ErrorMessage);
                    END (*IF*);
                ELSE
                    StrToBuffer (OurLang, "POPClient.UserRejected", ErrorMessage);
                    LogTransaction (LogID, ErrorMessage);
                END (*IF*);
            ELSE
                StrToBuffer (OurLang, "POPClient.NoOKreply", ErrorMessage);
                LogTransaction (LogID, ErrorMessage);
            END (*IF*);
            IF NOT success THEN
                GetLastLine (U^.SB, ErrorMessage);
                LogTransaction (LogID, ErrorMessage);
                Logout (U);
            END (*IF*);
        END (*IF*);
        U^.MessageNumber := 1;
    END Login;

(************************************************************************)
(*                  CREATING AND DESTROYING USER RECORDS                *)
(************************************************************************)

PROCEDURE RegisterPOP3User(): POP3User;

    (* Creates a new user record. *)

    VAR result: POP3User;

    BEGIN
        NEW (result);
        WITH result^ DO
            LoggedIn := FALSE;
            socket := NotASocket;
            loginname[0] := Nul;
            password := "pass";
            MessageNumber := 0;
            MessageCount := 0;
            RBpos := 0;  RBlength := 0;
        END (*WITH*);
        RETURN result;
    END RegisterPOP3User;

(************************************************************************)

PROCEDURE SetPOP3LoginDetails (U: POP3User;
                               LoginName, NewPassword: ARRAY OF CHAR);

    (* Sets the POP3 username and password for the mail account. *)

    BEGIN
        Strings.Assign (LoginName, U^.loginname);
        Strings.Assign (NewPassword, U^.password);
    END SetPOP3LoginDetails;

(************************************************************************)

PROCEDURE DiscardPOP3User (VAR (*INOUT*) U: POP3User);

    (* Deletes a user record. *)

    BEGIN
        IF U^.LoggedIn THEN
            Logout (U);
        END (*IF*);
        DISPOSE (U);
    END DiscardPOP3User;

(************************************************************************)
(*                       FETCHING A MESSAGE                             *)
(************************************************************************)

PROCEDURE FetchFirstMessage (U: POP3User;  VAR (*OUT*) filename: FilenameString;
                             LogID: TransactionLogID);

    (* Fetches one message from the server, creates a local file to     *)
    (* hold it.  If there's no message, filename is set to the null     *)
    (* string.                                                          *)

    VAR Nbuf: ARRAY [0..31] OF CHAR;
        linebuffer: ARRAY [0..1023] OF CHAR;
        cid: ChanId;

    BEGIN
        filename[0] := Nul;
        IF NOT U^.LoggedIn THEN
            Login (U, LogID);
        END (*IF*);
        IF U^.MessageNumber > U^.MessageCount THEN
            Logout (U);
            RETURN;
        END (*IF*);
        CardinalToStringLJ (U^.MessageNumber, Nbuf);
        IF ExecCommand (U, "RETR", Nbuf) THEN
            cid := OpenNewOutputFile (".\", ".tmp", filename);
            LOOP
                IF NOT GetLine (U^.SB, linebuffer) THEN
                    EXIT (*LOOP*);
                END (*IF*);
                IF (linebuffer[0] = '.') AND (linebuffer[1] = Nul) THEN
                    EXIT (*LOOP*);
                END (*IF*);
                FWriteString (cid, linebuffer);
                FWriteLn (cid);
            END (*LOOP*);
            CloseFile (cid);
        END (*IF*);
    END FetchFirstMessage;

(************************************************************************)

PROCEDURE DeleteFirstMessage (U: POP3User;  LogID: TransactionLogID);

    (* Tells the server to delete the message just fetched. *)

    VAR Nbuf: ARRAY [0..31] OF CHAR;
        ErrorMessage: ARRAY [0..1023] OF CHAR;

    BEGIN
        CardinalToStringLJ (U^.MessageNumber, Nbuf);
        IF ExecCommand (U, "DELE", Nbuf) THEN
            INC (U^.MessageNumber);
            IF U^.MessageNumber > U^.MessageCount THEN
                Logout (U);
            END (*IF*);
        ELSE
            ErrorMessage := "Could not delete message.";
            LogTransaction (LogID, ErrorMessage);
            GetLastLine (U^.SB, ErrorMessage);
            LogTransaction (LogID, ErrorMessage);
        END (*IF*);
    END DeleteFirstMessage;

(************************************************************************)
(*                           INITIALISATION                             *)
(************************************************************************)

PROCEDURE SetServerAddress (hostname: ARRAY OF CHAR;  port: CARDINAL;
                                            AdminLang: LangHandle);

    (* The caller specifies which POP3 server and port to use, *)
    (* also what language to use for log messages.             *)

    VAR fault: BOOLEAN;
        message: ARRAY [0..255] OF CHAR;

    BEGIN
        OurLang := AdminLang;
        Strings.Assign (hostname, POPhost);
        IF NameIsNumeric (hostname) THEN
            IPaddress := inet_addr (hostname);
            IPaddressKnown := TRUE;
            fault := IPaddress = 0;
        ELSE
            IPaddressKnown := FALSE;
            fault := hostname[0] = Nul;
        END (*IF*);
        IF fault THEN
            StrToBuffer (OurLang, "POPClient.NoAddress", message);
            WriteString (message);
            WriteLn;
            HALT;
        END (*IF*);
        POPport := port;
    END SetServerAddress;

(************************************************************************)

BEGIN
    IPaddress := 0;  IPaddressKnown := FALSE;
    POPhost := '';
    POPport := 110;
END POPClient.

