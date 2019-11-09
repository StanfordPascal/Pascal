@echo off
set dd_input=%1.pas
set dd_output=
set dd_prr=%1.prrp5
pcint prr=p5.prr inc=paslibx pas=p5.pas out=p5.prrlis debug=n
set dd_input=
set dd_output=
set dd_prr=
@echo on
