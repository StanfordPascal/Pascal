program TESTFUNC ( OUTPUT ) ;


var X : INTEGER ;


begin (* HAUPTPROGRAMM *)
  X := RANDOM ( 25 ) ;
  X := RANDOM ( X ) ;
  X := RANDOM ( X + 5 ) ;
  WRITELN ( X ) ;
end (* HAUPTPROGRAMM *) .
