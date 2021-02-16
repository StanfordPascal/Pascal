program COMPERRE ( OUTPUT ) ;

//*****
//$A+  
//*****



var TEST : STRING ( 10 ) ;
    B : BOOLEAN ;
    C : CHAR ( 6 ) := 'BERND' ;
    C3 : CHAR ( 3 ) := 'ABC' ;
    C1 : CHAR := 'X' ;


begin (* HAUPTPROGRAMM *)
  TEST := 'W' ;
  B := ( TEST = 'W' ) ;
  WRITELN ( B , ' should be true' ) ;
  B := ( TEST = 'W' ) or ( TEST = 'S' ) ;
  WRITELN ( B , ' should be true' ) ;
  B := ( TEST = 'W' ) or ( TEST = 'S' ) or ( TEST = '' ) ;
  WRITELN ( B , ' should be true' ) ;
  B := ( TEST = '' ) ;
  WRITELN ( B , ' should be false' ) ;
  B := ( TEST = C ) ;
  WRITELN ( B , ' should be false' ) ;
  B := ( 'B' = C ) ;
  WRITELN ( B , ' should be false' ) ;
  B := ( C1 = C ) ;
  WRITELN ( B , ' should be false' ) ;
  B := ( C = 'B' ) ;
  WRITELN ( B , ' should be false' ) ;
  B := ( C = C3 ) ;
  WRITELN ( B , ' should be false' ) ;
  B := ( C = C1 ) ;
  WRITELN ( B , ' should be false' ) ;
end (* HAUPTPROGRAMM *) .
