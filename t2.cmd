set dd:input=%1.prr
set dd:prr=%1.xxx
set dd:asmout=%1.asmout
set dd:dbginfo=%1.dbginfo
set dd:tracef=*stdout*
pcint prr=pascal2.prr inc=paslibx,%2 pas=pascal2.pas out=pascal2.prrlis debug=n
set dd:input=
set dd:prr=
set dd:asmout=
set dd:dbginfo=
set dd:tracef=
