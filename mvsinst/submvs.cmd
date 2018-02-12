@echo off
if "%1" == "" goto :noparm
if not exist %1 goto :fehler
nc -w1 127.0.0.1 3505 <%1
goto :fertig
:fehler
echo Datei %1 nicht vorhanden
goto :fertig
:noparm
echo usage SUBMVS dateiname (fertiger Job fr TK4- MVS)
goto :fertig
:fertig
