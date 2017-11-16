set dd:input=%1.pas
set dd:listing=%1.lis
set dd:prr=%1.prr
set dd:dbginfo=%1.dbginfo
set dd:prd=pascal.messages
copy %1.prr %1.prralt /y >nul
ipmd pcint prr=pascal1.prr pas=pascal1.pas out=pascal1.prrlis debug=n
set dd:input=
set dd:listing=
set dd:prr=
set dd:dbginfo=
