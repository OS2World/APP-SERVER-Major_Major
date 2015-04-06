/****************************************************************/
/*        Lists all members of a Major Major mailing list       */
/*                                                              */
/*      Author:       Peter Moylan (peter@ee.newcastle.edu.au)  */
/*      Started:      3 May 2001                                */
/*      Last revised: 27 July7 2008                             */
/*                                                              */
/*  Usage:                                                      */
/*         listmembers listname                                 */
/*                                                              */
/*  Installation:                                               */
/*         Put this file in the directory containing MAJOR.INI, */
/*         or execute it from that directory.                   */
/*                                                              */
/****************************************************************/

CALL RxFuncAdd SysLoadFuncs, rexxutil, sysloadfuncs
CALL SysLoadFuncs

PARSE ARG listname
listname = Strip(listname)
IF listname = '' THEN
    DO
        SAY "Usage: listmembers listname"
        EXIT 0
    END

/* Fetch the list of list members. */

names = SysIni( 'MAJOR.INI', listname, 'Members' )
IF names = 'ERROR:' then
    DO
         SAY 'List 'listname' does not exist, or has no members'
         EXIT
    END

/* Strip out the individual names and write them. */

SAY 'Members of list 'listname
SAY '===================='

DO WHILE (names \= '') & (names \= '0'X)
     PARSE VALUE names WITH name1 '0'X names
     SAY '    'name1
END /* DO */

EXIT 0
