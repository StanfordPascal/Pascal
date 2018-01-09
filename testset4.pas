program TESTSET4 ( OUTPUT ) ;

(********)
(*$A+   *)
(********)



type FARBE = ( GELB , ROT , GRUEN , BLAU ) ;


var X : FARBE ;
    XSET : set of FARBE ;
    CSET : set of FARBE ;


begin (* HAUPTPROGRAMM *)
  XSET := [ ROT , GRUEN ] ;
  CSET := [ GELB , ROT , GRUEN , BLAU ] ;
  WRITELN ( 'should be true:  ' , XSET <= CSET ) ;
  WRITELN ( 'should be true:  ' , XSET <= XSET ) ;
  WRITELN ( 'should be false: ' , XSET >= CSET ) ;
  WRITELN ( 'compile error:   ' , XSET < CSET ) ;
end (* HAUPTPROGRAMM *) .
