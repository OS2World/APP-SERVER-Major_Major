/****************************************************************/
/*             Creation of MajorMajor desktop folder            */
/****************************************************************/

/* Register with REXX API extensions. */
Call RxFuncAdd 'SysLoadFuncs', 'RexxUtil', 'SysLoadFuncs'
Call SysLoadFuncs

/* Get creation disposition. */
Call SysCls
Say 'Major Major for OS/2 - Installation Script.'
Say
Say 'Creates desktop folder.'
Say
CreateCollision = 'Update'

Call CreateObjects
Exit

CreateObject: procedure
    Parse Arg Class, Title, Location, Setup, Collision
    Say 'Creating ['Title']'
    rc = SysCreateObject( Class, Title, Location, Setup, Collision )
    If rc <> 1 Then
        Say ' > failed to create ['Title' | 'Class'] at location ['Location']'
    return rc

CreateShadow: procedure
    Parse Arg Class, Location
    Say 'Creating Shadow ['Class']'
    rc = SysCreateShadow( Class, Location )
    If rc <> 1 Then
        Say ' > failed to create ['Class'] at location ['Location']'
    return rc

CreateObjects:

current_dir = directory()
previous_dir = left(current_dir, ( length(current_dir) - 4 ))

/***************/
/* MAIN FOLDER */

rc = CreateObject( 'WPFolder',,
    'Major Major',,
    '<WP_DESKTOP>',,
    'NOPRINT=YES;'||,
        'DEFAULTVIEW=CONTENTS;'||,
        'SELFCLOSE=1;'||,
        'ICONVIEW=NONGRID,NORMAL;'||,
        'DETAILSVIEW=MINI;'||,
        'TREEVIEW=LINES,NORMAL;'||,
        'ALWAYSSORT=YES;'||,
        'ICONFILE='||current_dir||'\folder.ico;'||,
        'OBJECTID=<MajorMajor_folder>',,
    CreateCollision )

/*******************/
/* PROGRAM OBJECTS */

  rc = CreateObject( 'WPProgram',,
      'Major Major list manager',,
      '<MajorMajor_folder>',,
      'NOPRINT=YES;'||,
          'DEFAULTVIEW=RUNNING;'||,
          'EXENAME='||current_dir||'\MAJOR.EXE;'||,
          'STARTUPDIR='||current_dir||';'||,
          'MINIMIZED=YES;'||,
          'OBJECTID=<MajorMajor_Program>',,
      CreateCollision )

  rc = CreateObject( 'WPProgram',,
      'Configuration program',,
      '<MajorMajor_folder>',,
      'NOPRINT=YES;'||,
          'DEFAULTVIEW=RUNNING;'||,
          'EXENAME='||current_dir||'\ADMIN.EXE;'||,
          'STARTUPDIR='||current_dir||';'||,
          'OBJECTID=<MajorMajor_Admin>',,
      CreateCollision )

  rc = CreateObject( 'WPProgram',,
      'Major Major Manual',,
      '<MajorMajor_folder>',,
      'NOPRINT=YES;'||,
          'DEFAULTVIEW=RUNNING;'||,
          'EXENAME='||'VIEW.EXE;'||,
          'PARAMETERS='||current_dir||'\MAJOR.INF;'||,
          'STARTUPDIR='||current_dir||';'||,
          'MINIMIZED=NO;'||,
          'OBJECTID=<MajorMajor_Manual>',,
      CreateCollision )

  rc = CreateObject( 'WPURL',,
      'Updates',,
      '<MajorMajor_folder>',,
      'NOPRINT=YES;'||,
          'URL=http://www.pmoylan.org/pages/os2/major.html',,
      CreateCollision )

return
