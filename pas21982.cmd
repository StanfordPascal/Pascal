set dd_input=%1.prr
set dd_prr=%1.xxx
set dd_asmout=%1.asmout
set dd_prd=%1.dbginfo
set dd_tracef=*stdout*
pcint prr=pcod1982.prr inc=paslibx,%2 pas=pcod1982.pas out=pcod1982.prrlis debug=n
set dd_input=
set dd_prr=
set dd_asmout=
set dd_dbginfo=
set dd_tracef=
