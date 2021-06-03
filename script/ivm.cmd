set dd:ivmodell=%1.ivmodell
set dd:oevmodel=%2.oevmodel
pcint prr=ivmoded.prr inc=paslibx,pasutils pas=ivmoded.pas out=ivmoded.prrlis / %1
set dd:ivmodell=
set dd:oevmodel=
