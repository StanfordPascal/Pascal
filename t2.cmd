set dd_input=%1.prr
set dd_prr=%1.xxx
set dd_asmout=%1.asmout
set dd_dbginfo=%1.dbginfo
set dd_tracef=*stdout*
pcint prr=pascal2.prr inc=paslibx,%2 pas=pascal2.pas out=pascal2.prrlis debug=n
set dd_input=
set dd_prr=
set dd_asmout=
set dd_dbginfo=
set dd_tracef=
