@echo off
if "%1" == "" goto :noparm
if "%2" == "" goto :noparm
if not exist %1 goto :fehler
if exist copymvs.tmp del copymvs.tmp
if exist copymvs2.tmp del copymvs2.tmp
rexx copymvs.rex %1 %2
if not exist copymvs.tmp pause
copy copymvs.tmp+%1+copymvs2.tmp copymvs.tmp
nc -w1 127.0.0.1 3505 <copymvs.tmp
goto :fertig
:fehler
echo Datei %1 nicht vorhanden
goto :fertig
:noparm
rexx copymvs.rex
goto :fertig
:fertig
