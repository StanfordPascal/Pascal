program TESTASGN ( OUTPUT ) ;


var F : TEXT ;
    FNAME : array [ 1 .. 80 ] of CHAR ;


begin (* HAUPTPROGRAMM *)
  FNAME := 'c:\\work\\pascal\\work\\testausgabe.dat' ;
  ASSIGN ( F , ADDR ( FNAME ) , 40 ) ;
  REWRITE ( F ) ;
  WRITELN ( F , 'TESTAUSGABE' ) ;
  CLOSE ( F ) ;
  FNAME := 'c:\\work\\pascal\\work\\testausg2.dat' ;
  ASSIGN ( F , ADDR ( FNAME ) , 40 ) ;
  REWRITE ( F ) ;
  WRITELN ( F , 'TESTAUSGABE' ) ;
  CLOSE ( F ) ;
end (* HAUPTPROGRAMM *) .
