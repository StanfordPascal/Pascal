set dd_inpfile=%1
set dd_drucker=pdokk.lis
pcint prr=pdokkneu.prr inc=paslibx,pasutils pas=pdokkneu.pas out=pdokkneu.prrlis debug=n list=j
set dd_inpfile=
set dd_drucker=
call pr2 pdokk.lis
