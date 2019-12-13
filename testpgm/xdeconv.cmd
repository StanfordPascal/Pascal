@echo off
if not exist %1 goto :fehler
set dd_txtfile=%1
set dd_binfile=%2
call prun xconvert - / -c
goto :ende
:fehler
echo Datei %1 existiert nicht
:ende
