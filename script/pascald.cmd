@echo off
rem
rem ********************************************************
rem * script to call the New Stanford Pascal Compiler
rem * usage: pascal <sourcename> ...
rem * without file extension, which must be .pas
rem ********************************************************
rem * env variable PASDIR has to be set to directory
rem * containing the compiler files
rem * which must be visible using the PATH variable
rem ********************************************************
rem
set dd_input=%1.pas
set dd_listing=%1.lis
set dd_output=
rem set dd_listing=*stdout*
set dd_pcode=%1.pcode
set dd_pcode1=%1.pcode1
set dd_pcode2=%1.pcode2
set dd_pcode3=%1.pcode3
set dd_dbginfo=%1.dbginfo
set dd_prd=%PASDIR%\pascal.messages
set dd_tracef=*stdout*
if exist %1.pcode copy %1.pcode %1.pcodealt /y >nul
if exist %1.pcode1 copy %1.pcode1 %1.pcode1alt /y >nul
if exist %1.pcode2 copy %1.pcode2 %1.pcode2alt /y >nul
if exist %1.pcode3 copy %1.pcode3 %1.pcode3alt /y >nul
pcint pcode=%PASDIR%\pascal1.pcode inc=%PASDIR%\paslibx,%PASDIR%\passcan pas=%PASDIR%\pascal1.pas out=%PASDIR%\pascal1.pcodelis debug=y list=y
set dd_input=
set dd_listing=
set dd_pcode=
set dd_pcode1=
set dd_pcode2=
set dd_pcode3=
set dd_dbginfo=
set dd_tracef=
@echo on
