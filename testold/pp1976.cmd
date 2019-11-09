@echo off
set dd_input=%1.pas
set dd_output=%1.lis1976
set dd_prr=%1.prr1976
pcint prr=pas1976.prr pas=pas1976.pas out=pas1976.prrlis debug=n
set dd_input=
set dd_output=
set dd_prr=
@echo on
