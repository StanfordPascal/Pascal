program TESTSET4 ( OUTPUT ) ;

(********)
(*$A+   *)
(********)



type FARBE = ( GELB , ROT , GRUEN , BLAU ) ;
     SUB1 = 0 .. 255 ;


var X : FARBE ;
    XSET : set of FARBE ;
    CSET : set of FARBE ;
    SETA , SETB : set of SUB1 ;


begin (* HAUPTPROGRAMM *)
  XSET := [ ROT , GRUEN ] ;
  CSET := [ GELB , ROT , GRUEN , BLAU ] ;
  WRITELN ( 'should be true:  ' , XSET <= CSET ) ;
  WRITELN ( 'should be true:  ' , XSET <= XSET ) ;
  WRITELN ( 'should be false: ' , XSET >= CSET ) ;
  WRITELN ( 'should be true:  ' , XSET = XSET ) ;
  WRITELN ( 'should be false: ' , XSET = CSET ) ;
  WRITELN ( 'should be true:  ' , XSET = ( CSET * XSET ) ) ;

  //******************************************************************
  // WRITELN ( 'compile error:   ' , XSET < CSET ) ;                  
  //******************************************************************

  SETA := [ 2 , 3 , 5 , 7 ] ;
  SETB := [ 1 , 2 , 3 , 4 , 5 , 7 , 200 ] ;
  WRITELN ( 'should be true:  ' , SETA <= SETB ) ;
  WRITELN ( 'should be true:  ' , SETA <= SETA ) ;
  WRITELN ( 'should be false: ' , SETA >= SETB ) ;
  WRITELN ( 'should be true:  ' , SETA = SETA ) ;
  WRITELN ( 'should be false: ' , SETA = SETB ) ;
  WRITELN ( 'should be true:  ' , SETA = ( SETB * SETA ) ) ;

  //******************************************************************
  // WRITELN ( 'compile error:   ' , SETA < SETB ) ;                  
  //******************************************************************

end (* HAUPTPROGRAMM *) .
