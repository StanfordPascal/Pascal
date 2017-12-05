program TESTFIL1 ( INPUT , OUTPUT ) ;


var CH : CHAR ;


begin (* HAUPTPROGRAMM *)
  RESET ( INPUT ) ;
  while not EOF do
    begin
      CH := INPUT -> ;
      WRITE ( OUTPUT , CH ) ;
      if not EOLN then
        begin
          GET ( INPUT ) ;
          continue ;
        end (* then *) ;
      repeat
        WRITELN ;
        GET ( INPUT ) ;
      until EOF or not EOLN ;
    end (* while *)
end (* HAUPTPROGRAMM *) .
