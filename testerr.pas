program TESTERR ( OUTPUT ) ;

//*****
//$A+  
//*****



var IFILE : TEXT ;
    I : INTEGER ;
    R : REAL ;


begin (* HAUPTPROGRAMM *)
  WRITELN ( 'testen real einlesen' ) ;
  READLN ( R ) ;
  WRITELN ( 'gelesen: ' , R : 1 : 9 ) ;
  I := 12340 ;
  WRITELN ( I * 0.001 : 7 : 2 ) ;
  if FALSE then
    begin
      REWRITE ( IFILE ) ;
      WRITELN ( IFILE , 'A' ) ;
      RESET ( IFILE ) ;
      READ ( IFILE , I ) ;
      $ERROR ( 999 ) ;
    end (* then *) ;
end (* HAUPTPROGRAMM *) .
