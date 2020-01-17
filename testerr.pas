program TESTERR ( OUTPUT ) ;

//*****
//$A+
//*****



var IFILE : TEXT ;
    I : INTEGER ;


begin (* HAUPTPROGRAMM *)
  REWRITE ( IFILE ) ;
  WRITELN ( IFILE , 'A' ) ;
  RESET ( IFILE ) ;
  READ ( IFILE , I ) ;
  $ERROR ( 999 ) ;
end (* HAUPTPROGRAMM *) .
