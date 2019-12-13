@echo off
if not exist %1 goto :fehler
set dd_binfile=%1
set dd_txtfile=%2
call prun xconvert - / -x
goto :ende
:fehler
echo Datei %1 existiert nicht
:ende
