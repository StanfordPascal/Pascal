program TESTSQR ( OUTPUT ) ;

//*****
//$A+
//*****



const ZWEIR = - 2.0e1 ;
      ZWEI = - 20 ;


var R : REAL := ZWEIR ;
    I : INTEGER := ZWEI ;


begin (* HAUPTPROGRAMM *)
  I := ABS ( I ) ;
  WRITELN ( I ) ;
  I := SQR ( I ) ;
  WRITELN ( I ) ;
  R := ABS ( R ) ;
  WRITELN ( R ) ;
  R := SQR ( R ) ;
  WRITELN ( R ) ;
  WRITELN ( 'expo (400)   = ' , EXPO ( R ) ) ;
  WRITELN ( 'expo (400/I) = ' , EXPO ( I ) ) ;
  WRITELN ( 'expo (255/I) = ' , EXPO ( 255 ) ) ;
  WRITELN ( 'expo (255)   = ' , EXPO ( 255.0 ) ) ;
  WRITELN ( 'expo (256)   = ' , EXPO ( 256.0 ) ) ;
  WRITELN ( 'expo (257)   = ' , EXPO ( 257.0 ) ) ;
  WRITELN ( 'expo (4096)  = ' , EXPO ( 4096.0 ) ) ;
  WRITELN ( 'expo (0)     = ' , EXPO ( 0.0 ) ) ;
  WRITELN ( 'expo (-255)  = ' , EXPO ( - 255.0 ) ) ;
  WRITELN ( 'expo (-256)  = ' , EXPO ( - 256.0 ) ) ;
  WRITELN ( 'expo (-257)  = ' , EXPO ( - 257.0 ) ) ;
  WRITELN ( 'expo (-4096) = ' , EXPO ( - 4096.0 ) ) ;
end (* HAUPTPROGRAMM *) .
