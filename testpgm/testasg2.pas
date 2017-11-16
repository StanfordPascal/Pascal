program TESTASG2 ( OUTPUT ) ;


var F : TEXT ;
    FNAME : array [ 1 .. 80 ] of CHAR ;


begin (* HAUPTPROGRAMM *)
  FNAME := 'TESTAUSG FIXED (RECFM F LRECL 80' ;
  ASSIGN ( F , ADDR ( FNAME ) , 40 ) ;
  REWRITE ( F ) ;
  WRITELN ( F , 'TESTAUSGABE' ) ;
  CLOSE ( F ) ;
  FNAME := 'TESTAUSG VARIABLE (RECFM V' ;
  ASSIGN ( F , ADDR ( FNAME ) , 40 ) ;
  REWRITE ( F ) ;
  WRITELN ( F , 'TESTAUSGABE' ) ;
  CLOSE ( F ) ;
end (* HAUPTPROGRAMM *) .
