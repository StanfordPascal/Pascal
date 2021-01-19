program COMPERRD ( OUTPUT ) ;

//*****
//$A+
//*****



var X24 : CHAR ( 24 ) ;
    X40 : CHAR ( 40 ) ;
    B : BOOLEAN ;
    S24 : STRING ( 24 ) ;
    S40 : STRING ( 40 ) ;


begin (* HAUPTPROGRAMM *)
  X24 := 'Bernd' ;
  X40 := X24 ;
  S24 := 'Bernd' ;
  S40 := 'Bernd' ;
  B := '' < 'A    '
  WRITELN ( B , ' empty constant left - true expected' ) ;
  B := 'A    ' < '';
  WRITELN ( B , ' empty constant right - false expected' ) ;
  B := 'Bernd   ' = 'Bernd' ;
  WRITELN ( B , ' constants long before short - true expected' ) ;
  B := 'Bernd' = 'Bernd   ' ;
  WRITELN ( B , ' constants short before long - true expected' ) ;
  B := X24 = 'Bernd  ' ;
  WRITELN ( B , ' x24 and short constant - true expected' ) ;
  B := STR ( X24 ) = 'Be' || 'rnd ' ;
  WRITELN ( B , ' x24 and concat of constants - true expected' ) ;
  B := 'Be' || 'rnd ' = X24 ;
  WRITELN ( B , ' x24 and concat of constants - true expected' ) ;
  B := X24 = 'Bernd                                 ' ;
  WRITELN ( B , ' x24 and long constant - true expected' ) ;
  B := X40 = X24 ;
  WRITELN ( B , ' x40 and x24 - true expected' ) ;
  B := X24 = X40 ;
  WRITELN ( B , ' x24 and x40 - true expected' ) ;
  B := S40 = S24 ;
  WRITELN ( B , ' s40 and s24 - true expected' ) ;
  B := S24 = S40 ;
  WRITELN ( B , ' s24 and s40 - true expected' ) ;
  S24 := 'Bernd' ;
  S40 := 'Bernd ' ;
  B := S40 = S24 ;
  WRITELN ( B , ' s40 and s24 - false expected' ) ;
  B := S24 = S40 ;
  WRITELN ( B , ' s24 and s40 - false expected' ) ;
end (* HAUPTPROGRAMM *) .
