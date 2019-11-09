@echo off
set dd_input=%1.pas
set dd_listing=%1.lis
set dd_output=
rem set dd_listing=*stdout*
set dd_pcode=%1.prr
set dd_dbginfo=%1.dbginfo
set dd_prd=c:\bin\pascal.messages
set dd_tracef=*stdout*
copy %1.prr %1.prralt /y >nul
pcint prr=c:\bin\pascal1.prr inc=c:\bin\paslibx,c:\bin\passcan debug=n
set dd_input=
set dd_listing=
set dd_pcode=
set dd_dbginfo=
set dd_tracef=
@echo on
