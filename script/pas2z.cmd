set dd_input=%1.prr
set dd_objcode=%1.xxx
set dd_asmout=%1.asmout
set dd_dbginfo=%1.dbginfo
set dd_tracef=pascal2z.tracef
pcint prr=pascal2z.prr inc=paslibx,%2 pas=pascal2z.pas out=pascal2z.prrlis debug=n
set dd_input=
set dd_objcode=
set dd_asmout=
set dd_dbginfo=
set dd_tracef=
