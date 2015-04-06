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

IMPLEMENTATION MODULE SMTPOut;

        (********************************************************)
        (*                                                      *)
        (* Sends outbound mail by sending it to an SMTP server  *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            5 August 2000                   *)
        (*  Last edited:        23 May 2012                     *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings;

FROM SMTPLogin IMPORT
    (* proc *)  DoPOPLogin, DoLogin;

FROM SBuffers IMPORT
    (* type *)  SBuffer,
    (* proc *)  CreateSBuffer, CloseSBuffer, SendLine, SendChar,
                SendRaw, SendEOL, PositiveResponse, GetLastLine,
                SetTimeout, FlushOutput;

FROM MXCheck IMPORT
    (* proc *)  DoMXLookup;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer, StrToBufferA;

FROM Sockets IMPORT
    (* const*)  AF_INET, SOCK_STREAM, AF_UNSPEC, NotASocket,
    (* type *)  Socket, SockAddr,
    (* proc *)  socket, connect, soclose, gethostid;

FROM Internet IMPORT
    (* const*)  Zero8,
    (* proc *)  inet_addr;

FROM NetDB IMPORT
    (* type *)  HostEntPtr,
    (* proc *)  (*gethostname,*) gethostbyname(*, gethostbyaddr*);

FROM AddressLists IMPORT
    (* type *)  EmailAddress, AddressList,
    (* proc *)  FirstOnList, NextOnList, FirstGroup, NextGroup,
                EmptyList, DiscardAddressList;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  LogTransaction, LogTransactionL;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId, FilenameString,
    (* proc *)  OpenOldFile, CloseFile, ReadRaw;

FROM InetUtilities IMPORT
    (* proc *)  NameIsNumeric;

FROM Inet2Misc IMPORT
    (* proc *)  Swap2, Swap4, IPToString;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM LogLevel IMPORT
    (* proc *)  LogLevelType;

FROM Names IMPORT
    (* type *)  HostNameIndex, HostName, UserName, PassString;

(************************************************************************)

VAR
    (* Language to use for log messages. *)

    OurLang: LangHandle;

    (* Name of the local host. *)

    LocalHost: HostName;

    (* Relay host for our outbound mail. *)

    ForwardRelayHost: HostName;
    ForwardRelayPort: CARDINAL;

    (* Authentication for outbound relay mail.  We have the choice of  *)
    (* using the SMTP AUTH command or POP-before-SMTP authentication;  *)
    (* it would not be sensible to use both.  PSUsername and           *)
    (* PSPassword are the username and password for authentication.    *)

    UseSMTPAUTH: BOOLEAN;
    UsePOPbeforeSMTP: BOOLEAN;
    PSUsername: UserName;
    PSPassword: PassString;

    (* POP3 host for POP-before-SMTP authentication. *)

    PSIPAddress: CARDINAL;
    PSPort: CARDINAL;

(************************************************************************)
(*            THE PROCEDURES THAT DELIVER THE OUTGOING MAIL             *)
(************************************************************************)

PROCEDURE ConnectToHost (IPaddress: CARDINAL;  SMTPport: CARDINAL;
                      VAR (*INOUT*) FailureReason: ARRAY OF CHAR): Socket;

    (* Tries to open a connection to the specified host.  Returns the   *)
    (* value NotASocket if we don't succeed; in this case, the string   *)
    (* FailureReason might be updated.                                  *)
    (* IPaddress is in network byte order.                              *)

    VAR s: Socket;  peer: SockAddr;

    BEGIN
        IF IPaddress <> 0 THEN
            s := socket (AF_INET, SOCK_STREAM, AF_UNSPEC);

            IF s = NotASocket THEN
                Strings.Assign ("Can't allocate socket", FailureReason);
            ELSE

                (* Socket open, connect to the client. *)

                WITH peer DO
                    family := AF_INET;
                    WITH in_addr DO
                        port := Swap2(SMTPport);
                        addr := IPaddress;
                        zero := Zero8;
                    END (*WITH*);
                END (*WITH*);

                IF connect (s, peer, SIZE(peer)) THEN

                    Strings.Assign ("Failed to connect", FailureReason);
                    soclose(s);
                    s := NotASocket;

                END (*IF*);
            END (*IF*);

        ELSE

            Strings.Assign ("500 Unknown host", FailureReason);
            s := NotASocket;

        END (*IF*);

        RETURN s;

    END ConnectToHost;

(************************************************************************)

PROCEDURE SendCommand (SB: SBuffer;  command: ARRAY OF CHAR;
                         VAR (*OUT*) ConnectionLost: BOOLEAN): BOOLEAN;

    (* Sends a command, returns TRUE if the command was sent OK and     *)
    (* a positive response was returned.                                *)

    BEGIN
        ConnectionLost := NOT SendLine (SB, command);
        FlushOutput (SB);
        RETURN (NOT ConnectionLost) AND PositiveResponse(SB, ConnectionLost);
    END SendCommand;

(************************************************************************)

PROCEDURE SendFile (SB: SBuffer;  name: FilenameString;
                         VAR (*OUT*) ConnectionLost: BOOLEAN): BOOLEAN;

    (* Sends the file, returns TRUE if it was successfully transmitted  *)
    (* and a positive response was returned.                            *)

    CONST BufferSize = 8192;
          LF = CHR(10);

    VAR success, MoreToGo, AtEOL: BOOLEAN;
        cid: ChanId;  amount: CARDINAL;
        buffer: ARRAY [0..BufferSize-1] OF CHAR;

    BEGIN
        AtEOL := TRUE;
        cid := OpenOldFile (name, FALSE, TRUE);
        success := cid <> NoSuchChannel;
        MoreToGo := TRUE;
        WHILE success AND MoreToGo DO
            ReadRaw (cid, buffer, BufferSize, amount);
            MoreToGo := amount > 0;
            IF MoreToGo THEN
                AtEOL := buffer[amount-1] = LF;
                success := SendRaw (SB, buffer, amount);
            END (*IF*);
        END (*WHILE*);
        CloseFile (cid);
        IF NOT AtEOL THEN
            success := success AND SendEOL(SB);
        END (*IF*);

        success := success AND SendChar (SB, '.') AND SendEOL (SB);
        FlushOutput (SB);
        ConnectionLost := NOT success;
        RETURN success AND PositiveResponse (SB, ConnectionLost);

    END SendFile;

(************************************************************************)
(*                       DELIVERING ONE ITEM                            *)
(************************************************************************)

PROCEDURE DeliverDeLetter (VAR (*INOUT*) from: EmailAddress;
                           recipients: AddressList;
                           VAR (*IN*) file: FilenameString;
                           IPaddress, port: CARDINAL;
                           LogIt: BOOLEAN;
                           LogID: TransactionLogID): CARDINAL;

    (* Sends one mail item to the recipients via the server at the  *)
    (* specified IP address.The file is not deleted.                *)
    (* Returns a count of the number of recipients for whom         *)
    (* delivery was successful.                                     *)

    VAR success, MoreToGo, ConnectionLost: BOOLEAN;
        s: Socket;  count: CARDINAL;
        Buffer: ARRAY [0..511] OF CHAR;
        forwardpath: EmailAddress;
        SB: SBuffer;

    VAR logmessage, failuremessage: ARRAY [0..255] OF CHAR;

    BEGIN
        count := 0;

        (* Ensure that the "from" addresses is delimited  *)
        (* by angle brackets.                             *)

        IF from[0] <> '<' THEN
            Strings.Insert ('<', 0, from);
            Strings.Append ('>', from);
        END (*IF*);

        (* If required for authentication, do a POP login before the SMTP operation. *)

        IF UsePOPbeforeSMTP THEN
            DoPOPLogin (PSIPAddress, PSPort, PSUsername, PSPassword,
                                                         LogIt, LogID);
        END (*IF*);

        (* Connect to a host and send the EHLO or HELO command, plus    *)
        (* authentication if needed.                                    *)

        s := ConnectToHost (IPaddress, port, failuremessage);
        SB := CreateSBuffer (s, TRUE);
        SetTimeout (SB, 150);
        success := (s <> NotASocket) AND PositiveResponse(SB, ConnectionLost);
        IF success THEN
            SetTimeout (SB, 900);
            success := DoLogin (SB, LocalHost, UseSMTPAUTH,
                                PSUsername, PSPassword, ConnectionLost,
                                LogIt, LogID);
            IF NOT success THEN
                StrToBuffer (OurLang, "SMTPOut.CantLogin", logmessage);
                LogTransaction (LogID, logmessage);
                GetLastLine (SB, failuremessage);
                LogTransaction (LogID, failuremessage);
            END (*IF*);
        ELSE
            StrToBuffer (OurLang, "SMTPOut.CantConnect", logmessage);
            LogTransaction (LogID, logmessage);
            LogTransaction (LogID, failuremessage);
        END (*IF*);

        IF success THEN

            (* We are now logged in. *)

            Buffer := "MAIL FROM: ";
            Strings.Append (from, Buffer);
            success := SendCommand (SB, Buffer, ConnectionLost);
            IF LogIt THEN
                LogTransaction (LogID, Buffer);
            END (*IF*);

            (* The next step is counted as a success if at least one    *)
            (* recipient is accepted.  This way the list remains        *)
            (* functional even if some members are unreachable.         *)

            IF success THEN
                IF LogIt THEN
                    GetLastLine (SB, Buffer);
                    LogTransaction (LogID, Buffer);
                END (*IF*);
                MoreToGo := FirstOnList (recipients, 0, forwardpath);
                success := FALSE;
                WHILE MoreToGo DO
                    IF forwardpath[0] <> '<' THEN
                        Strings.Insert ('<', 0, forwardpath);
                        Strings.Append ('>', forwardpath);
                    END (*IF*);
                    Buffer := "RCPT TO: ";
                    Strings.Append (forwardpath, Buffer);
                    IF LogIt THEN
                        LogTransaction (LogID, Buffer);
                    END (*IF*);
                    IF SendCommand (SB, Buffer, ConnectionLost) THEN
                        INC (count);
                        success := TRUE;
                        IF LogIt THEN
                            GetLastLine (SB, Buffer);
                            LogTransaction (LogID, Buffer);
                        END (*IF*);
                    ELSE
                        StrToBufferA (OurLang, "SMTPOut.CommandRejected",
                                               Buffer, logmessage);
                        LogTransaction (LogID, logmessage);
                        GetLastLine (SB, failuremessage);
                        LogTransaction (LogID, failuremessage);
                    END (*IF*);
                    MoreToGo := NextOnList (recipients, 0, forwardpath);
                END (*WHILE*);
            ELSE
                StrToBufferA (OurLang, "SMTPOut.CommandRejected",
                                       "MAIL FROM", logmessage);
                LogTransaction (LogID, logmessage);
                GetLastLine (SB, failuremessage);
                LogTransaction (LogID, failuremessage);
            END (*IF*);

            IF success THEN
                success := SendCommand (SB, "DATA", ConnectionLost)
                             AND SendFile (SB, file, ConnectionLost);
            END;

            IF ConnectionLost THEN
                LogTransactionL (LogID, "Connection lost");
            ELSIF NOT success THEN
                StrToBufferA (OurLang, "SMTPOut.CommandRejected",
                                       "MAIL FROM", logmessage);
                LogTransaction (LogID, logmessage);
                GetLastLine (SB, failuremessage);
                LogTransaction (LogID, failuremessage);
            END (*IF*);

            (* We should try to log out even if the above failed. *)

            EVAL (SendCommand (SB, "QUIT", ConnectionLost));

        END (*IF*);

        CloseSBuffer (SB);

        IF success THEN
            RETURN count;
        ELSE
            RETURN 0;
        END (*IF*);

    END DeliverDeLetter;

(************************************************************************)

PROCEDURE SendToGroup (VAR (*IN*) sender: EmailAddress;
                               VAR (*IN*) sourcefile: FilenameString;
                                  recipients: AddressList;
                                  LogIt: BOOLEAN;
                                  LogID: TransactionLogID): CARDINAL;

    (* Try to send the mail to all recipients.  If necessary and we *)
    (* get multiple possible addresses for our mail relay domain,   *)
    (* we try the multiple addresses in order of preference.        *)
    (* Returns a count of the number of recipients for whom         *)
    (* delivery was successful.                                     *)

    CONST Max = 31;

    VAR j, count: CARDINAL;
        address: ARRAY [0..Max] OF CARDINAL;

    BEGIN
        count := 0;
        IF DoMXLookup (ForwardRelayHost, address) = 0 THEN
            j := 0;
            REPEAT
                INC (count, DeliverDeLetter (sender, recipients, sourcefile,
                                             address[j], ForwardRelayPort,
                                             LogIt, LogID));
                INC (j);
            UNTIL (count > 0) OR (j > Max) OR (address[j] = 0);
        END (*IF*);

        RETURN count;

    END SendToGroup;

(************************************************************************)

PROCEDURE MailOneMessage (VAR (*IN*) sender: EmailAddress;
                          VAR (*IN*) sourcefile: FilenameString;
                          VAR (*OUT*) failures: CARDINAL;
                                     option: CARDINAL;
                                     recipients: AddressList;
                                     LogLevel: LogLevelType;
                                     LogID: TransactionLogID): CARDINAL;

    (* Sends one mail item, possibly to multiple recipients.            *)
    (* The message file is not deleted, regardless of success or        *)
    (* failure; we leave that decision up to the caller.  The value     *)
    (* returned is the number of recipients for whom delivery was made, *)
    (* and the failures parameter returns the number of recipients      *)
    (* for whom we were unable to deliver the message.                  *)
    (* The options are                                                  *)
    (*       0      send to everyone on the recipients list             *)
    (*       1      send only to the 'mayread' recipients               *)

    VAR dest: AddressList;  count, successes, groupsize: CARDINAL;
        LogIt: BOOLEAN;

    BEGIN
        LogIt := LogLevel >= logdetailed;
        successes := 0;  failures := 0;

        (* Break up the recipient list into small sublists, so as to    *)
        (* avoid the risk of specifying more recipients than the        *)
        (* server will accept.                                          *)

        dest := FirstGroup (option, recipients, groupsize);
        WHILE NOT EmptyList (dest) DO
            count := SendToGroup (sender, sourcefile, dest, LogIt, LogID);
            IF count > 0 THEN
                INC (successes, count);
            END (*IF*);
            IF count < groupsize THEN
                INC (failures, groupsize-count);
            END (*IF*);
            DiscardAddressList (dest);
            dest := NextGroup (option, recipients, groupsize);
        END (*WHILE*);
        DiscardAddressList (dest);
        RETURN successes;
    END MailOneMessage;

(************************************************************************)
(*                           INITIALISATION                             *)
(************************************************************************)

(*
PROCEDURE GetOurHostName (VAR (*OUT*) textualname: ARRAY OF CHAR);

    (* Creates a host record with as official an answer as possible for *)
    (* the IP address and name of the host on which we're running.      *)

    CONST Nul = CHR(0);

    VAR HostInfo: HostEntPtr;
        numaddr: CARDINAL;

    BEGIN
        numaddr := 0;
        textualname[0] := Nul;

        (* We use gethostname in preference to gethostid, because it    *)
        (* seems that gethostid can return a left-over number that's    *)
        (* not necessarily still valid.                                 *)

        HostInfo := NIL;
        IF NOT gethostname(textualname, MAX(HostNameIndex)+1) THEN
            HostInfo := gethostbyname (textualname);
        END (*IF*);

        IF (HostInfo = NIL) THEN
            numaddr := Swap4(gethostid());
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

    END GetOurHostName;
*)

(************************************************************************)

PROCEDURE SetOurHostname (VAR (*IN*) name: ARRAY OF CHAR);

    (* The caller specifies the argument we should use in the HELO      *)
    (* and EHLO commands.                                               *)

    BEGIN
        Strings.Assign (name, LocalHost);
    END SetOurHostname;

(************************************************************************)

PROCEDURE SetSMTPAddress (SMTPHostname: ARRAY OF CHAR;
                          SMTPport: CARDINAL;  lang: LangHandle);

    (* The caller specifies which SMTP server and port to use,  and     *)
    (* what language to use for log messages.                           *)

    BEGIN
        Strings.Assign (SMTPHostname, ForwardRelayHost);
        ForwardRelayPort := SMTPport;
        OurLang := lang;
    END SetSMTPAddress;

(************************************************************************)

PROCEDURE SetPSParameters (UseAUTHoption: CARDINAL;
                           hostname: ARRAY OF CHAR;  port: CARDINAL;
                           username, password: ARRAY OF CHAR);

    (* The caller specifies the parameters to user for authentication   *)
    (* of outbound relay mail.  UseAUTHoption has the meaning:          *)
    (*      0    don't use authentication                               *)
    (*      1    use the SMTP AUTH command                              *)
    (*      2    use POP-before-SMTP                                    *)
    (* The hostname and port are relevant only in case 2.               *)

    VAR p: HostEntPtr;

    BEGIN
        UseSMTPAUTH := UseAUTHoption = 1;
        UsePOPbeforeSMTP := UseAUTHoption = 2;
        IF NameIsNumeric (hostname) THEN
            PSIPAddress := inet_addr (hostname);
        ELSE
            p := gethostbyname (hostname);
            IF (p = NIL) OR (p^.h_addr_list = NIL)
                         OR (p^.h_addr_list^[0] = NIL) THEN
                PSIPAddress := 0;
            ELSE
                PSIPAddress := p^.h_addr_list^[0]^;
            END (*IF*);
        END (*IF*);
        PSPort := port;
        Strings.Assign (username, PSUsername);
        Strings.Assign (password, PSPassword);
    END SetPSParameters;

(************************************************************************)

BEGIN
    (*GetOurHostName (LocalHost);*)
    Strings.Assign ("localhost", LocalHost);
    Strings.Assign ("[127.0.0.1]", ForwardRelayHost);
    ForwardRelayPort := 25;
    UseSMTPAUTH := FALSE;
    UsePOPbeforeSMTP := FALSE;
END SMTPOut.

