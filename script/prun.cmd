@echo off
rem
rem ********************************************************
rem * script to run New Stanford Pascal programs
rem * usage: prun <programname>
rem *    or: prun <programname> <list-of-modules>
rem *    or: prun <programname> <list-of-modules> <run-time-parms>
rem *    or: prun <programname> , <run-time-parms>
rem * without file extension
rem ********************************************************
rem * env variable PASDIR has to be set to directory
rem * containing the compiler files
rem * which must be visible using the PATH variable
rem ********************************************************
rem
pcint pcode=%1.pcode inc=%PASDIR%\paslibx,%PASDIR%\pasutils,%2 pas=%1.pas out=%1.pcodelis sconst=3000000 %3 %4 %5 %6 %7 %8 %9
@echo off
