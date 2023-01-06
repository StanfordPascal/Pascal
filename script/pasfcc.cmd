@echo off
rem
rem ********************************************************
rem * script to call the New Stanford Pascal Formatter
rem * usage: pasf <sourcename> ...
rem * without file extension, which must be .pas
rem ********************************************************
rem * env variable PASDIR has to be set to directory
rem * containing the compiler files
rem * which must be visible using the PATH variable
rem ********************************************************
rem
rem ********************************************************
rem changed 11.04.2021 - Opp
rem reading .pas - writing .new - when success, rename
rem if there is an error, no rename
rem ********************************************************
set dd_eingabe=%1.pas
set dd_ausgabe=%1.new
set dd_listing=%1.lis
set dd_output=
pcint pcode=%PASDIR%\pasform.pcode inc=%PASDIR%\paslibx,%PASDIR%\passcan pas=%PASDIR%\pasform.pas out=%PASDIR%\pasform.lis debug=n / C+
if errorlevel 4 goto :err
if exist %1.bak erase %1.bak
rename %1.pas %1.bak
rename %1.new %1.pas
:err
@echo on
