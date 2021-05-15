@echo off
rem ********************************************************
rem Aenderung am 11.04.2021 - Opp
rem Lesen Pas - Schreiben New - danach im Erfolgsfall rename
rem Wenn Fehler gemeldet wird, kein Umbenennen
rem ********************************************************
set dd_eingabe=%1.pas
set dd_ausgabe=%1.new
set dd_listing=%1.lis
set dd_output=
pcint pcode=pasform.pcode inc=paslibx,passcan pas=pasform.pas out=pasform.lis debug=n
if errorlevel 4 goto :err
if exist %1.bak erase %1.bak
rename %1.pas %1.bak
rename %1.new %1.pas
:err
@echo on
