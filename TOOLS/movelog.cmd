/************************************************************************/
/*                                                                      */
/*             Script to archive the Major Major log file               */
/*                                                                      */
/*   If you have enabled logging to disk, Major Major writes the log    */
/*   to a file called MAJOR.LOG.  (More precisely, it writes to a       */
/*   temporary log file called MAJOR.$$$, and then appends the          */
/*   temporary log to the main log approximately once every 15 minutes. */
/*   This reduces the complications that would be caused if you tried   */
/*   to read a file that Major Major has open in write-only mode.)      */
/*                                                                      */
/*   This script moves the log files, with renaming, to the 'LOGS'      */
/*   subdirectory.  It will not attempt to move nonexistent files, so   */
/*   it is safe to use whether or not you have enabled logging.         */
/*                                                                      */
/*   Note, however, that the destination directory for the archived     */
/*   files is hard-coded.  If you want to use a different directory,    */
/*   you will have to modify line 57 of this script.                    */
/*                                                                      */
/*   Programmer:      P. Moylan                                         */
/*   Last modified:   7 August 2003                                     */
/*                                                                      */
/*   Installation:                                                      */
/*      Put this file in the same directory as MAJOR.INI (normally      */
/*      the main Major Major directory).  Alternatively, create a       */
/*      program object whose working directory is the directory that    */
/*      contains MAJOR.INI.  Also create a LOGS subdirectory to hold    */
/*      the archived files.                                             */
/*                                                                      */
/*   Usage:    MoveLog        (with no parameters)                      */
/*                                                                      */
/*   This, however, is for a single execution.  To make this script     */
/*   really useful you should arrange to run it periodically, using     */
/*   any of the cron-like utilities for OS/2.  My own approach is to    */
/*   create a program object for this file, and use the 'schedule'      */
/*   feature of DragText to run it on the first day of every month.     */
/*                                                                      */
/************************************************************************/

call RxFuncAdd 'SysLoadFuncs', 'RexxUtil', 'SysLoadFuncs'
call SysLoadFuncs

call MoveFile 'MAJOR.LOG'
EXIT 0

/****************************************************************/

MoveFile: PROCEDURE

    /* Move one file.  */

    PARSE ARG srcfile

    IF STREAM(srcfile, 'C', 'QUERY EXISTS') \= '' THEN DO
        dstfile = DATE('S')||'.log'
        '@RENAME 'srcfile' 'dstfile
        '@MOVE 'dstfile 'logs >nul'
    END /* do */
    RETURN

