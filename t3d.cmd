set dd:input=%1.prr
set dd:prr=%1.xxx
set dd:asmout=%1.asmout
set dd:dbginfo=%1.dbginfo
set dd:tracef=*stdout*
pcint prr=pascal3.prr inc=paslibx,pasutils,pascalvs pas=pascal3.pas out=pascal3.prrlis debug=y
set dd:input=
set dd:prr=
set dd:asmout=
set dd:dbginfo=
set dd:tracef=
