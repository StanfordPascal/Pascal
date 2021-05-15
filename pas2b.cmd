set dd_pcode=%1.pcode
set dd_pcode1=%1.pcode1
set dd_objcode=%1.xxx
set dd_asmout=%1.asmout
set dd_dbginfo=%1.dbginfo
set dd_tracef=pascal2b.tracef
pcint pcode=pascal2b.pcode inc=paslibx,%2 pas=pascal2b.pas out=pascal2b.pcodelis debug=n
set dd_input=
set dd_objcode=
set dd_asmout=
set dd_dbginfo=
set dd_tracef=
