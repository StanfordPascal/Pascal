@echo off
if exist %1.bak del %1.bak
copy %1.pas %1.bak
set dd_eingabe=%1.bak
set dd_ausgabe=%1.pas
pcint prr=pasforma.prr pas=pasforma.pas out=pasforma.lis
@echo on
