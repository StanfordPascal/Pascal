if exist %1.bak del %1.bak
copy %1.pas %1.bak
set dd:eingabe=%1.bak
set dd:ausgabe=%1.pas
pcint prr=pasform.prr pas=pasform.pas out=pasform.lis
