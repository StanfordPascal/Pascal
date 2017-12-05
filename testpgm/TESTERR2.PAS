program TESTERR2 ( OUTPUT ) ;

(********)
(*$A+   *)
(********)



var I : INTEGER ;
    CH : CHAR ;


type ALFA = array [ 1 .. 40 ] of CHAR ;


static ABC : ALFA ;


begin (* HAUPTPROGRAMM *)
  WRITELN ( 'abc = ' , ABC ) ;
  I := 40 ;
  CH := '/' ;
  MEMSET ( ADDR ( ABC ) , CH , I ) ;
  WRITELN ( 'abc = ' , ABC ) ;
  MEMSET ( ADDR ( ABC ) , '*' , I ) ;
  WRITELN ( 'abc = ' , ABC ) ;
  MEMSET ( ADDR ( ABC ) , '*' , 40 ) ;
  WRITELN ( 'abc = ' , ABC ) ;
  MEMSET ( ADDR ( ABC ) , '*' , SIZEOF ( ABC ) ) ;
  WRITELN ( 'abc = ' , ABC ) ;
  MEMSET ( ADDR ( ABC ) , '*' , SIZEOF ( ALFA ) ) ;
  WRITELN ( 'abc = ' , ABC ) ;
end (* HAUPTPROGRAMM *) .
