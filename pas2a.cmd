set dd_input=%1.prr
set dd_objcode=%1.xxx
set dd_asmout=%1.asmout
set dd_dbginfo=%1.dbginfo
set dd_tracef=pascal2a.tracef
pcint prr=pascal2a.prr inc=paslibx,%2 pas=pascal2a.pas out=pascal2a.prrlis debug=n
set dd_input=
set dd_objcode=
set dd_asmout=
set dd_dbginfo=
set dd_tracef=
