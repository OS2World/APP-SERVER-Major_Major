/****************************************************************/
/*          Signals a global event semaphore to tell            */
/*                 Major Major to shut down.                    */
/*                                                              */
/*       Programmer:      P. Moylan                             */
/*       Last modified:   8 July 2002                           */
/*                                                              */
/* Prerequisite: for this to work you must have RXU.DLL in      */
/* your LIBPATH.  RXU, a Rexx library written by Dave Boll, is  */
/* available from Hobbes with the name rxu1a.zip.               */
/****************************************************************/

call rxfuncadd 'rxuinit','rxu','rxuinit'
call rxuinit
SemName = "\SEM32\MAJOR\SHUTDOWN"
if RxOpenEventSem(hev, SemName) \= 0 then
    rc = RxCreateEventSem( hev ,'Shared', SemName, 'Reset')
call RxPostEventSem hev
call RxResetEventSem hev
call RxCloseEventSem hev
call RxuTerm

exit

