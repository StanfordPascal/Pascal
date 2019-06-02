program TESTVC2 ( OUTPUT ) ;

//**********************************************************************
//$A+                                                                   
//**********************************************************************



var STAB : array [ 1 .. 20 ] of STRING ( 25 ) ;
    N : INTEGER ;
    M : INTEGER ;


begin (* HAUPTPROGRAMM *)
  STAB [ 1 ] := 'AB' ;
  STAB [ 2 ] := 'CD' ;
  STAB [ 3 ] := STAB [ 1 ] || STAB [ 2 ] ;
  N := 2 ;
  M := 3 ;
  STAB [ 4 ] := STAB [ M ] || STAB [ N ] ;
  STAB [ 5 ] := SUBSTR ( STAB [ N - 1 ] , 1 , 1 ) || STAB [ N ] ||
                SUBSTR ( STAB [ N ] , 1 , 1 ) ;
  STAB [ 5 ] := SUBSTR ( STAB [ N - 1 ] , 1 , 1 ) || STAB [ N ] ||
                SUBSTR ( STAB [ N ] , 1 , 1 ) || STAB [ N ] ;
  WRITELN ( 'stab/4  = <' , STAB [ 4 ] , '>' ) ;
  WRITELN ( 'stab/4  = <' , STAB [ M ] || STAB [ N ] , '>' ) ;
  WRITELN ( 'stab/5  = <' , STAB [ 5 ] , '>' ) ;
  WRITELN ( 'stab/5a = <' , SUBSTR ( STAB [ N - 1 ] , 1 , 1 ) , '>' ) ;
  WRITELN ( 'stab/5b = <' , SUBSTR ( STAB [ N - 1 ] , 1 , 1 ) || STAB [
            N ] , '>' ) ;
  WRITELN ( 'stab/5  = <' , SUBSTR ( STAB [ N - 1 ] , 1 , 1 ) || STAB [
            N ] || SUBSTR ( STAB [ N ] , 1 , 1 ) || STAB [ N ] , '>' )
            ;
end (* HAUPTPROGRAMM *) .
