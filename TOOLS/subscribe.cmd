/****************************************************************/
/*        Adds a new member to a Major Major mailing list       */
/*                                                              */
/*      Author:       Peter Moylan (peter@ee.newcastle.edu.au)  */
/*      Started:      6 May 2001                                */
/*      Revised:      9 Nov 2003 Dave Saville (dave@deezee.org) */
/*      Last revised: 27 Jul 2008 Peter Moylan                  */
/*                                                              */
/*  Usage:                                                      */
/*         subscribe listname emailaddress [mode]               */
/*                                                              */
/* In subscribe mode it checks to see if that email address     */
/* is already subscribed and exits with an error if it is.      */
/*                                                              */
/* In Delete & Modify modes it removes the record, in the case  */
/* of modify then treats it as subscribe.                       */
/*                                                              */
/* Addresses can be specified with (x) prefix. But the checking */
/* is done against the plain address. This means that the same  */
/* address cannot be in more than once - so you can't have:     */
/* (D)foo@bar.com                                               */
/* foo@bar.com                                                  */
/*                                                              */
/* It also now gives MajorMajor a kick after updating the ini   */
/* file.                                                        */
/*                                                              */
/*  Installation:                                               */
/*         Put this file in the directory containing MAJOR.INI  */
/*                                                              */
/****************************************************************/

call RxFuncAdd 'SysLoadFuncs', 'RexxUtil', 'SysLoadFuncs'
call SysLoadFuncs

PARSE ARG listname newmember mode .
listname = Strip(listname)
newmember = Strip(newmember)
mode = Strip(mode)

IF (listname = '') | (newmember = '') THEN DO
  SAY "Usage: subscribe listname newmember [mode]"
  SAY "Mode can be Subscribe (default), Delete or Modify"
  EXIT 1
END

IF mode = '' THEN
  mode = "S"
ELSE
  mode = Translate(Substr(mode, 1, 1))

ptr1 = 1
found = 0

/* Switch our working directory to the program directory */

PARSE SOURCE . . SrcDir
j = LASTPOS('\', SrcDir)
IF j > 0 THEN SrcDir = LEFT(SrcDir, j-1)
WrkDir = DIRECTORY()
CALL DIRECTORY SrcDir

IF SysIni( 'MAJOR.INI', listname, 'ALL:', 'dummy.' ) = 'ERROR:' THEN DO
  SAY 'List 'listname' does not exist'
  EXIT 1
END

/* Fetch the list of current members. */

names = SysIni( 'MAJOR.INI', listname, 'Members' )

IF names = 'ERROR:' THEN
  names = '0'X
ELSE DO


  /* strip the (x) if present */

  IF Substr(newmember, 1, 1) = "(" THEN
     newmember2 = Translate(Substr(newmember, pos(")", newmember) + 1))
  ELSE
     newmember2 = Translate(newmember)

  DO WHILE ptr1 < Length(names)

    ptr2 = Pos('0'X, Substr(names, ptr1)) + ptr1 - 1
    name1 = Substr(names, ptr1, ptr2 - ptr1)

    /* strip the (x) if present */

    IF substr(name1, 1, 1) = "(" THEN
      name2 = Translate(Substr(name1, pos(")", name1) + 1))
    ELSE
      name2 = Translate(name1)

    IF name2 = newmember2 THEN DO

      IF mode = 'S' THEN DO
        SAY "Sorry "||newmember||" exists."
        EXIT 1
      END
      ELSE DO
        names = DelStr(names, ptr1, ptr2 - ptr1 + 1)
        found = 1
        Leave
      END
    END

    ptr1 = ptr2 + 1
  END
END

IF mode <> 'S' & found = 0 THEN DO
  say "Sorry "||newmember||" not found"
  EXIT 1
END

/* Update the ini file. We don't bother sorting the list,         */
/* because this will be done the next time that Admin.exe is run. */
/* (And, anyway, it's not vital that the list be sorted.)         */

Select
  When mode = 'S' THEN DO
    CALL SysINI 'MAJOR.INI', listname, 'Members', newmember||'0'X||names
    CALL SysINI 'MAJOR.INI', listname, 'changed', '01'X
    SAY listname 'member 'newmember' added'
  END
  When mode = 'D' THEN DO
    CALL SysINI 'MAJOR.INI', listname, 'Members', names
    CALL SysINI 'MAJOR.INI', listname, 'changed', '01'X
    SAY listname 'member 'newmember' deleted'
  END
  When mode = 'M' THEN DO
    CALL SysINI 'MAJOR.INI', listname, 'Members', newmember||'0'X||names
    CALL SysINI 'MAJOR.INI', listname, 'changed', '01'X
    SAY listname 'member 'newmember' Modified'
  END
  Otherwise
    NOP
END

CALL DIRECTORY WrkDir

/* Tell Major Major that some lists have been updated. */

CALL RxFuncAdd "rxuinit", "rxu", "rxuinit"
CALL rxuinit

SemName = "\SEM32\MAJOR\UPDATED"

IF RxOpenEventSem(hev, SemName) \= 0 THEN
    rc = RxCreateEventSem( hev ,'Shared', SemName, 'Reset')

CALL RxPostEventSem hev
CALL RxResetEventSem hev
CALL RxCloseEventSem hev
CALL RxuTerm

EXIT 0
