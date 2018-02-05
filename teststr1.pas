program TESTSTR1 ( OUTPUT ) ;


var S : STRING ( 20 ) ;
    I : INTEGER ;


begin (* HAUPTPROGRAMM *)
  S := 'Bernd' ;
  WRITELN ( 'maxlength = ' , MAXLENGTH ( S ) ) ;
  WRITELN ( 'length    = ' , LENGTH ( S ) ) ;
  for I := 1 to LENGTH ( S ) do
    WRITE ( S [ I ] , ' ' ) ;
  WRITELN
end (* HAUPTPROGRAMM *) .
