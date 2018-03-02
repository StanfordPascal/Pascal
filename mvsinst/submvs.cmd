@echo off
if "%1" == "" goto :noparm
if not exist %1 goto :fehler
nc -w1 127.0.0.1 3505 <%1
echo Job %1 submitted to TK4- successfully
goto :fertig
:fehler
echo File %1 does not exist
goto :fertig
:noparm
echo Submit Job to TK4- MVS
echo one parameter needed: name of file containing JCL
goto :fertig
:fertig
