@echo off
if "%1" == "" goto :noparm
if not exist %1 goto :fehler
if exist copyrdr.tmp del copyrdr.tmp
rexx copyrdr.rex %1 %2 %3 %4
if not exist copyrdr.tmp pause
copy copyrdr.tmp+%1 copyrdr.tmp
nc -w3 127.0.0.1 3505 <copyrdr.tmp
goto :fertig
:fehler
echo Datei %1 nicht vorhanden
goto :fertig
:noparm
rexx copyrdr.rex
goto :fertig
:fertig
