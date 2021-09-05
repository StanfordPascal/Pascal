@echo off
rem
rem ********************************************************
rem * script to run New Stanford Pascal programs (with debug)
rem * usage: prund <programname>
rem *    or: prund <programname> <list-of-modules>
rem *    or: prund <programname> <list-of-modules> <run-time-parms>
rem *    or: prund <programname> , <run-time-parms>
rem * without file extension
rem ********************************************************
rem * env variable PASDIR has to be set to directory
rem * containing the compiler files
rem * which must be visible using the PATH variable
rem ********************************************************
rem
pcint pcode=%1.pcode inc=%PASDIR%\paslibx,%PASDIR%\pasutils,%2 pas=%1.pas out=%1.pcodelis debug=y sconst=3000000 %3 %4 %5 %6 %7 %8 %9
@echo off
