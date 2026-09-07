program TESTPARM ( OUTPUT ) ;


var I : INTEGER ;


begin (* HAUPTPROGRAMM *)
  with OSPARM -> do
    begin
      WRITELN ( 'plength = ' , PLENGTH ) ;
      WRITE ( 'pstring = ' );
      for I := 1 to PLENGTH do
        WRITE ( PSTRING [ I ] ) ;
      WRITELN
    end (* with *)
end (* HAUPTPROGRAMM *) .
