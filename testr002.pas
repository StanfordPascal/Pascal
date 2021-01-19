program TESTR002 ( OUTPUT ) ;


var R : REAL ;


begin (* HAUPTPROGRAMM *)
  R := 0.5 ;
  WRITELN ( R < 0 , ' expexted false' ) ;
  WRITELN ( R > 0 , ' expexted true' ) ;
  WRITELN ( R = 0 , ' expexted false' ) ;
  R := - 0.5 ;
  WRITELN ( R < 0 , ' expexted true' ) ;
  WRITELN ( R > 0 , ' expexted false' ) ;
  WRITELN ( R = 0 , ' expexted false' ) ;
  R := 0.0 ;
  WRITELN ( R < 0 , ' expexted false' ) ;
  WRITELN ( R > 0 , ' expexted false' ) ;
  WRITELN ( R = 0 , ' expexted true' ) ;
end (* HAUPTPROGRAMM *) .
