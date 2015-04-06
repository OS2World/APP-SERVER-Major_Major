/****************************************************************/
/* Tells Major Major to reread the lists from the INI file.     */
/* Normally Admin.EXE looks after this detail, but I wrote      */
/* the Rexx script when creating a mailing list via Telnet,     */
/* where it wasn't feasible to run Admin.EXE.                   */
/*                                                              */
/*       Programmer:      P. Moylan                             */
/*       Last modified:   18 October 2000                       */
/*                                                              */
/* Prerequisite: for this to work you must have RXU.DLL in      */
/* your LIBPATH.  RXU, a Rexx library written by Dave Boll, is  */
/* available from Hobbes with the name rxu1a.zip.               */
/****************************************************************/

call RxFuncAdd 'SysLoadFuncs', 'RexxUtil', 'SysLoadFuncs'
call SysLoadFuncs

/* Tell Major Major that some lists have been updated. */

call rxfuncadd 'rxuinit','rxu','rxuinit'
call rxuinit
SemName = "\SEM32\MAJOR\UPDATED"
if RxOpenEventSem(hev, SemName) \= 0 then
    rc = RxCreateEventSem( hev ,'Shared', SemName, 'Reset')
call RxPostEventSem hev
call RxResetEventSem hev
call RxCloseEventSem hev
call RxuTerm

exit

