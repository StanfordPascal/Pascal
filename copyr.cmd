@echo off
if "%1" == "" goto :noparm
if "%2" == "" goto :noparm
if not exist %1 goto :fehler
if exist copyr.tmp del copyr.tmp
rexx copyr.rex %1 %2
if not exist copyr.tmp pause
copy copyr.tmp+%1 copyr.tmp
nc -w1 127.0.0.1 3505 <copyr.tmp
goto :fertig
:fehler
echo Datei %1 nicht vorhanden
goto :fertig
:noparm
rexx copyr.rex
goto :fertig
:fertig
