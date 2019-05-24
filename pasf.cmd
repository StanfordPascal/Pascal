@echo off
if exist %1.bak del %1.bak
copy %1.pas %1.bak
set dd_eingabe=%1.bak
set dd_ausgabe=%1.pas
set dd_listing=%1.lis
set dd_output=
pcint prr=pasform.prr inc=paslibx,passcan pas=pasform.pas out=pasform.lis debug=n
@echo on
