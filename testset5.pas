program TESTSET5 ( OUTPUT ) ;

(********)
(*$A+   *)
(********)



type FARBE = ( GELB , ROT , GRUEN , BLAU ) ;


var XSET : set of FARBE ;
    CSET : set of FARBE ;



procedure PRINTSET ( X : set of FARBE ; Y : 0 .. 15 ) ;

   var F : FARBE ;

   begin (* PRINTSET *)
     WRITE ( 'test printset' ) ;
     WRITELN ;
     WRITE ( 'set = ' ) ;
     for F := GELB to BLAU do
       if F in X then
         WRITE ( F : 6 ) ;
     WRITELN ;
     WRITE ( 'y   = ' , Y ) ;
     WRITELN ;
   end (* PRINTSET *) ;



begin (* HAUPTPROGRAMM *)
  XSET := [ ROT , GRUEN ] ;
  CSET := [ GELB , ROT , GRUEN , BLAU ] ;
  PRINTSET ( XSET , 2 ) ;
  PRINTSET ( CSET , 4 ) ;

  //******************************************************************
  // range error:                                                     
  //******************************************************************
  // PRINTSET ( CSET , 30 ) ;                                         

end (* HAUPTPROGRAMM *) .
