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

IMPLEMENTATION MODULE AddressLists;

        (********************************************************)
        (*                                                      *)
        (*         Maintains lists of e-mail addresses          *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            5 August 2000                   *)
        (*  Last edited:        30 March 2015                   *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT
    (* type *)  CARD32;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  OpenINIFile, INIPutBinary, CloseINIFile;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    NameListPtr = POINTER TO NameListInfo;
    AddressList = POINTER TO
                          RECORD
                              access: Lock;
                              head, tail, curr: NameListPtr;
                          END (*RECORD*);

    (* Linear list of e-mail addresses. *)

    NameListInfo = RECORD
                       next: NameListPtr;
                       flags: OptionSet;
                       name: EmailAddress;
                   END (*RECORD*);

(************************************************************************)

VAR BatchLimit: CARDINAL;

    (* Maximum size of a group when we break up an address list     *)
    (* into groups.                                                 *)

(************************************************************************)
(*                       STRING COMPARISON                              *)
(************************************************************************)

PROCEDURE Compare (A, B: ARRAY OF CHAR): INTEGER;

    (* Compares A and B, modulo alphabetic case.  Returns -1 if A < B,  *)
    (* 0 if A = B, +1 if A > B.                                         *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        LOOP
            IF (j > HIGH(A)) OR (A[j] = Nul) THEN
                IF (j > HIGH(B)) OR (B[j] = Nul) THEN
                    RETURN 0;
                ELSE
                    RETURN -1;
                END (*IF*);
            ELSIF (j > HIGH(B)) OR (B[j] = Nul) THEN
                RETURN +1;
            ELSIF CAP(A[j]) < CAP(B[j]) THEN
                RETURN -1;
            ELSIF CAP(A[j]) > CAP(B[j]) THEN
                RETURN +1;
            ELSE
                INC(j);
            END (*IF*);
        END (*LOOP*);
    END Compare;

(************************************************************************)

PROCEDURE NameMatch (A, B: ARRAY OF CHAR): BOOLEAN;

    (* Checks for A = B, modulo alphabetic case. *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        LOOP
            IF (j > HIGH(A)) OR (A[j] = Nul) THEN
                RETURN (j > HIGH(B)) OR (B[j] = Nul);
            ELSIF (j > HIGH(B)) OR (B[j] = Nul) THEN
                RETURN FALSE;
            ELSIF CAP(A[j]) <> CAP(B[j]) THEN
                RETURN FALSE;
            ELSE
                INC(j);
            END (*IF*);
        END (*LOOP*);
    END NameMatch;

(************************************************************************)
(*                   OPERATIONS ON ADDRESS LISTS                        *)
(************************************************************************)

PROCEDURE CreateAddressList(): AddressList;

    (* Creates a new empty list. *)

    VAR result: AddressList;

    BEGIN
        NEW (result);
        WITH result^ DO
            CreateLock (access);
            head := NIL;
            curr := NIL;
            tail := NIL;
        END (*WITH*);
        RETURN result;
    END CreateAddressList;

(************************************************************************)

PROCEDURE DiscardAddressList (VAR (*INOUT*) list: AddressList);

    (* Disposes of an existing list. *)

    VAR p, q: NameListPtr;

    BEGIN
        IF list <> NIL THEN
            Obtain (list^.access);
            p := list^.head;
            WHILE p <> NIL DO
                q := p;  p := p^.next;  DISPOSE(q);
            END (*WHILE*);
            Release (list^.access);
            DestroyLock (list^.access);
            DISPOSE (list);
        END (*IF*);
    END DiscardAddressList;

(************************************************************************)

PROCEDURE EmptyList (list: AddressList): BOOLEAN;

    (* Returns TRUE iff list has no entries. *)

    VAR result: BOOLEAN;

    BEGIN
        IF list = NIL THEN
            result := TRUE;
        ELSE
            Obtain (list^.access);
            result := list^.head = NIL;
            Release (list^.access);
        END (*IF*);
        RETURN result;
    END EmptyList;

(************************************************************************)

PROCEDURE AddRecipient (list: AddressList;
                        VAR (*IN*) address: EmailAddress;  flags: OptionSet);

    (* Adds a new e-mail address to the list. *)

    VAR current: NameListPtr;

    BEGIN
        NEW (current);
        current^.next := NIL;
        current^.flags := flags;
        current^.name := address;
        WITH list^ DO
            Obtain (access);
            IF head = NIL THEN
                head := current;
            ELSE
                tail^.next := current;
            END (*IF*);
            tail := current;
            Release (access);
        END (*WITH*);
    END AddRecipient;

(************************************************************************)

PROCEDURE RemoveRecipient (list: AddressList;
                        VAR (*IN*) address: EmailAddress): BOOLEAN;

    (* Removes an e-mail address from the list.  Returns TRUE iff  *)
    (* this was a digest member of the list.                       *)

    VAR previous, current: NameListPtr;
        WasDigestMember: BOOLEAN;

    BEGIN
        WasDigestMember := FALSE;
        WITH list^ DO
            Obtain (access);
            previous := NIL;  current := head;
            WHILE (current <> NIL) AND NOT NameMatch(current^.name, address) DO
                previous := current;
                current := current^.next;
            END (*WHILE*);
            IF current <> NIL THEN
                WasDigestMember := digestmember IN current^.flags;
                IF current = curr THEN
                    curr := curr^.next;
                END (*IF*);
                IF current = tail THEN
                    tail := previous;
                END (*IF*);
                IF previous = NIL THEN
                    head := current^.next;
                ELSE
                    previous^.next := current^.next;
                END (*IF*);
                DISPOSE (current);
            END (*IF*);
            Release (access);
        END (*WITH*);
        RETURN WasDigestMember;
    END RemoveRecipient;

(************************************************************************)

PROCEDURE GetDigestMembers (list: AddressList;
                            VAR (*OUT*) result: AddressList): BOOLEAN;

    (* The result is a copy of a subset of the list, being only the     *)
    (* digest members of the list.  If there are no digest members,     *)
    (* no result list is created and the function result is FALSE.      *)

    VAR p, q: NameListPtr;

    BEGIN
        result := CreateAddressList();
        Obtain (list^.access);
        p := list^.head;
        WHILE p <> NIL DO
            IF digestmember IN p^.flags THEN
                NEW (q);
                q^ := p^;
                q^.next := NIL;
                IF result^.tail = NIL THEN
                    result^.head := q;
                ELSE
                    result^.tail^.next := q;
                END (*IF*);
                result^.tail := q;
            END (*IF*);
            p := p^.next;
        END (*WHILE*);
        Release (list^.access);
        IF result^.head = NIL THEN
            DiscardAddressList (result);
            RETURN FALSE;
        ELSE
            RETURN TRUE;
        END (*IF*);
    END GetDigestMembers;

(************************************************************************)

PROCEDURE IsOnList (member: EmailAddress;  list: AddressList): BOOLEAN;

    (* Returns TRUE iff member is already a member of the list. *)

    VAR current: NameListPtr;

    BEGIN
        WITH list^ DO
            Obtain (access);
            current := head;
            WHILE (current <> NIL) AND NOT NameMatch(current^.name, member) DO
                current := current^.next;
            END (*WHILE*);
            Release (access);
        END (*WITH*);
        RETURN current <> NIL;
    END IsOnList;

(************************************************************************)

PROCEDURE MemberFlags (member: EmailAddress;  list: AddressList): OptionSet;

    (* Returns the options applicable to this list member, or the empty *)
    (* set if 'member' is not a list member.                            *)

    VAR current: NameListPtr;  success: BOOLEAN;  result: OptionSet;

    BEGIN
        success := FALSE;
        WITH list^ DO
            Obtain (access);
            current := head;
            WHILE (NOT success) AND (current <> NIL) DO
                IF NameMatch(current^.name, member) THEN
                    success := TRUE;
                ELSE
                    current := current^.next;
                END (*IF*);
            END (*WHILE*);
            IF success THEN
                result := current^.flags;
            ELSE
                result := OptionSet {};
            END (*IF*);
            Release (access);
        END (*WITH*);
        RETURN result;
    END MemberFlags;

(************************************************************************)

PROCEDURE FirstOnList (list: AddressList;  option: CARDINAL;
                       VAR (*IN*) address: EmailAddress): BOOLEAN;

    (* Sets address to the first entry on the list, returns FALSE if    *)
    (* there is no such entry.  The options are:                        *)
    (*      0   unconditional check                                     *)
    (*      1   'mayread' members only                                  *)

    VAR found: BOOLEAN;

    BEGIN
        WITH list^ DO
            Obtain (access);
            curr := head;
            IF option > 0 THEN
                WHILE (curr <> NIL) AND NOT (mayread IN curr^.flags) DO
                    curr := curr^.next;
                END (*WHILE*);
            END (*IF*);
            found := curr <> NIL;
            IF found THEN
                address := curr^.name;
                curr := curr^.next;
            END (*IF*);
            Release (access);
        END (*WITH*);
        RETURN found;
    END FirstOnList;

(************************************************************************)

PROCEDURE NextOnList (list: AddressList;  option: CARDINAL;
                      VAR (*IN*) address: EmailAddress): BOOLEAN;

    (* Sets address to the next entry on the list, returns FALSE if     *)
    (* there is no such entry.  Same options as for FirstOnList.        *)

    VAR found: BOOLEAN;

    BEGIN
        WITH list^ DO
            Obtain (access);
            IF option > 0 THEN
                WHILE (curr <> NIL) AND NOT (mayread IN curr^.flags) DO
                    curr := curr^.next;
                END (*WHILE*);
            END (*IF*);
            found := curr <> NIL;
            IF found THEN
                address := curr^.name;
                curr := curr^.next;
            END (*IF*);
            Release (access);
        END (*WITH*);
        RETURN found;
    END NextOnList;

(************************************************************************)

PROCEDURE SetBatchLimit (limit: CARDINAL);

    (* Sets the limit on the number of list members that can be         *)
    (* selected to be sent mail at the same time.  According to the     *)
    (* mail RFCs this should be no more than 100.                       *)

    BEGIN
        IF limit > 100 THEN limit := 100 END(*IF*);
        BatchLimit := limit;
    END SetBatchLimit;

(************************************************************************)

PROCEDURE CopyPart (option: CARDINAL;  list: AddressList;
                                VAR (*OUT*) count: CARDINAL): AddressList;

    (* Returns a copy of a subset of the list, so that we can in effect *)
    (* split up the list so as to avoid sending to too many addresses   *)
    (* at the same time.  Assumption: the caller has locked the list.   *)
    (* For options, see procedure FirstOnList.                          *)

    VAR result: AddressList;  p, current: NameListPtr;

    BEGIN
        NEW (result);
        WITH result^ DO
            CreateLock (access);
            head := NIL;
            curr := NIL;
            tail := NIL;
        END (*WITH*);
        count := 0;
        p := list^.curr;
        IF option > 0 THEN
            WHILE (p <> NIL) AND NOT (mayread IN p^.flags) DO
                p := p^.next;
            END (*WHILE*);
        END (*IF*);
        IF p <> NIL THEN
            NEW (result^.head);
            current := result^.head;
            result^.curr := current;
            current^.name := p^.name;
            current^.flags := p^.flags;
            p := p^.next;  count := 1;
            WHILE (p <> NIL) AND (count < BatchLimit) DO
                IF (option = 0) OR (mayread IN p^.flags) THEN
                    INC (count);
                    NEW (current^.next);
                    current := current^.next;
                    current^.name := p^.name;
                    current^.flags := p^.flags;
                END (*IF*);
                p := p^.next;
            END (*WHILE*);
            list^.curr := p;
            current^.next := NIL;
            result^.tail := current;
        END (*IF*);
        RETURN result;
    END CopyPart;

(************************************************************************)

PROCEDURE FirstGroup (option: CARDINAL;  list: AddressList;
                            VAR (*OUT*) groupsize: CARDINAL): AddressList;

    (* For details, see procedure NextGroup below.  *)

    VAR result: AddressList;

    BEGIN
        WITH list^ DO
            Obtain (access);
            curr := head;
            result := CopyPart (option, list, groupsize);
            Release (access);
        END (*WITH*);
        RETURN result;
    END FirstGroup;

(************************************************************************)

PROCEDURE NextGroup (option: CARDINAL;  list: AddressList;
                            VAR (*OUT*) groupsize: CARDINAL): AddressList;

    (* Returns a copy of a subset of the list, so that we can in effect *)
    (* split up the list so as to avoid sending to too many addresses   *)
    (* at the same time.  Note that these procedures produce extra      *)
    (* lists; the caller must later call DiscardAddressList.            *)
    (* For the options, see procedure FirstOnList.                      *)
    (* The groupsize parameter returns the size of the result list.     *)

    VAR result: AddressList;

    BEGIN
        WITH list^ DO
            Obtain (access);
            result := CopyPart (option, list, groupsize);
            Release (access);
        END (*WITH*);
        RETURN result;
    END NextGroup;

(************************************************************************)

PROCEDURE EncodeList (list: AddressList;  VAR (*OUT*) bufptr: BufferPointer;
                                          VAR (*OUT*) BufferSize: CARDINAL);

    (* Puts the list into a buffer in a form suitable for writing   *)
    (* back to the INI file.                                        *)

    VAR index: BufferIndex;

    (********************************************************************)

    PROCEDURE FlagSize (flags: OptionSet): CARDINAL;

        (* Works out how much space would be needed for encoding the flags. *)

        VAR count: CARDINAL;

        BEGIN
            count := 3;      (* most common case. *)

            IF mayread IN flags THEN
                IF maywrite IN flags THEN
                    count := 0;
                ELSIF digestmember IN flags THEN
                    count := 4;
                END (*IF*);
            END (*IF*);

            RETURN count;

        END FlagSize;

    (********************************************************************)

    PROCEDURE PutFlags (flags: OptionSet);

        (* Stores the code for flags at bufptr^[index], updates index. *)
        (* Possible flags are (maywrite, mayread, digestmember).       *)

        VAR code: ARRAY [0..1] OF CHAR;

        BEGIN
            IF mayread IN flags THEN
                IF maywrite IN flags THEN
                    (* TT?   normal case, no flags needed, ignore digest code *)
                    code := '';
                ELSIF digestmember IN flags THEN
                    (* FTT   passive read-only digest member *)
                    code := 'RD';
                ELSE
                    (* FTF   readonly member *)
                    code := 'R';
                END (*IF*);
            ELSIF maywrite IN flags THEN
                (* TF? *)
                IF digestmember IN flags THEN
                    (* TFT   normal digest case *)
                    code := 'D';
                ELSE
                    (* TFF   writeonly member *)
                    code := 'W';
                END (*IF*);
            ELSE
                (* FF?   inactive member, regardless of digest option *)
                code := '-';
            END (*IF*);

            IF code[0] <> Nul THEN
                bufptr^[index] := '(';  INC (index);
                bufptr^[index] := code[0];  INC (index);
                IF code[1] <> Nul THEN
                    bufptr^[index] := code[1];  INC (index);
                END (*IF*);
                bufptr^[index] := ')';  INC (index);
            END (*IF*);

        END PutFlags;

    (********************************************************************)

    VAR j: [0..EmailAddressSize];
        p: NameListPtr;

    BEGIN
        Obtain (list^.access);

        (* Start by calculating how much space to allocate. *)

        BufferSize := 1;  p := list^.head;
        WHILE p <> NIL DO
            INC (BufferSize, FlagSize(p^.flags) + LENGTH(p^.name) + 1);
            p := p^.next;
        END (*WHILE*);
        ALLOCATE (bufptr, BufferSize);

        (* Now put the names into bufptr^. *)

        index := 0;  p := list^.head;
        WHILE p <> NIL DO
            PutFlags (p^.flags);
            j := 0;
            WHILE (j < EmailAddressSize) AND (p^.name[j] <> Nul) DO
                bufptr^[index] := p^.name[j];
                INC (index);  INC (j);
            END (*WHILE*);
            bufptr^[index] := Nul;  INC(index);
            p := p^.next;
        END (*WHILE*);
        bufptr^[index] := Nul;

        Release (list^.access);

    END EncodeList;

(************************************************************************)

PROCEDURE SortList (VAR (*INOUT*) list: AddressList);

    (* Sorts the list in alphabetic order. *)

    VAR previous, current, next: NameListPtr;
        changed: BOOLEAN;

    BEGIN
        (* Often the list is already sorted or almost sorted, and       *)
        (* in any case this procedure is rarely called, so a ripple     *)
        (* sort is sufficient.                                          *)

        REPEAT
            changed := FALSE;
            previous := NIL;  current := list^.head;
            WHILE (current <> NIL) AND (current^.next <> NIL) DO
                next := current^.next;
                IF Compare (current^.name, next^.name) > 0 THEN
                    (* Swap current^ and next^. *)
                    IF previous = NIL THEN
                        list^.head := next;
                    ELSE
                        previous^.next := next;
                    END (*IF*);
                    current^.next := next^.next;
                    IF current^.next = NIL THEN
                        list^.tail := current;
                    END (*IF*);
                    next^.next := current;
                    previous := next;
                    changed := TRUE;
                ELSE
                    previous := current;
                    current := current^.next;
                END (*IF*);
            END (*WHILE*);
        UNTIL NOT changed;
    END SortList;

(************************************************************************)

PROCEDURE MergeAndResave (hini: HINI;  ListName:ListNameType;
                          VAR (*INOUT*) list1, list2: AddressList);

    (* Combines two lists, so that list1 ends up holding the original   *)
    (* contents of both lists, and list2 ends up empty. Duplicate       *)
    (* entries are removed.  The combined list is then written back     *)
    (* as the "Members" entry of the INI file.                          *)

    VAR aux, previous, current, temp: NameListPtr;
        code: INTEGER;
        bufptr: BufferPointer;
        BufferSize: CARDINAL;

    BEGIN
        SortList (list1);
        SortList (list2);

        (* In the following loop, "current" is out current      *)
        (* position in the combined list being built up, and    *)
        (* "aux" is the head of the list of entries that        *)
        (* haven't yet been incorporated into the main list.    *)

        previous := NIL;  current := list1^.head;
        aux := list2^.head;  list2^.head := NIL;

        WHILE aux <> NIL DO

            (* Run through the main list until we reach a point   *)
            (* where the head of the aux list should be inserted. *)

            LOOP
                IF current = NIL THEN
                    code := 1;
                ELSE
                    code := Compare (current^.name, aux^.name);
                END (*IF*);
                IF code >= 0 THEN EXIT(*LOOP*) END (*IF*);
                previous := current;
                current := current^.next;
            END (*LOOP*);

            IF code = 0 THEN

                (* Duplicate entry, discard it apart from using flags. *)

                current^.flags := current^.flags * aux^.flags;
                temp := aux;
                aux := aux^.next;
                DISPOSE (temp);

            ELSE

                (* We've reached an insertion point.  Swap the  *)
                (* aux list with the tail of the main list.     *)

                IF previous = NIL THEN
                    list1^.head := aux;
                ELSE
                    previous^.next := aux;
                END (*IF*);
                aux := current;

            END (*IF*);

        END (*WHILE*);

        EncodeList (list1, bufptr, BufferSize);
        INIPutBinary (hini, ListName, 'Members', bufptr^, BufferSize);
        DEALLOCATE (bufptr, BufferSize);

    END MergeAndResave;

(************************************************************************)

(*
PROCEDURE OldMergeAndResave (hini: HINI;  ListName:ListNameType;
                          VAR (*INOUT*) list1, list2: AddressList);

    (* Combines two lists, so that list1 ends up holding the original   *)
    (* contents of both lists, and list2 ends up empty. Duplicate       *)
    (* entries are removed.  The combined list is then written back     *)
    (* as the "Members" entry of the INI file.                          *)

    VAR previous, current, p2: NameListPtr;
        code: INTEGER;
        bufptr: BufferPointer;
        BufferSize: CARDINAL;

    BEGIN
        SortList (list1);
        SortList (list2);
        previous := NIL;  current := list1^.head;
        p2 := list2^.head;
        WHILE p2 <> NIL DO

            IF current = NIL THEN
                code := 1;
            ELSE
                code := Compare (current^.name, p2^.name);
            END (*IF*);

            IF code = 0 THEN

                (* Duplicate entry, discard it apart from using flags. *)

                current^.flags := current^.flags * p2^.flags;
                list2^.head := p2^.next;
                DISPOSE (p2);

            ELSIF code > 0 THEN

                (* Take the first list2 entry, insert it ahead of current. *)

                IF previous = NIL THEN
                    list1^.head := p2;
                ELSE
                    previous^.next := p2;
                END (*IF*);
                list2^.head := p2^.next;
                previous := p2;
                p2^.next := current;

            ELSE

                previous := current;
                current := current^.next;

            END (*IF*);
            p2 := list2^.head;
        END (*WHILE*);

        EncodeList (list1, bufptr, BufferSize);
        INIPutBinary (hini, ListName, 'Members', bufptr^, BufferSize);
        DEALLOCATE (bufptr, BufferSize);

    END OldMergeAndResave;
*)

(************************************************************************)

BEGIN
    BatchLimit := 50;
END AddressLists.
