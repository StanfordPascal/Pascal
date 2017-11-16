program TESTLSTR ( OUTPUT ) ;

(********)
(*$A+   *)
(********)



var ZEILE : array [ 1 .. 200 ] of CHAR ;
    I : INTEGER ;


begin (* HAUPTPROGRAMM *)
  ZEILE := 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
           'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'
           'cccccccccccccccccccccccccccccccccccc'
           'dddddddddddddddddddddddddddddddddddd'
           'eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee' ;
  WRITELN ( ZEILE ) ;
  MEMSET ( ADDR ( ZEILE ) , 'b' , 200 ) ;
  WRITELN ( ZEILE ) ;
  for I := 1 to 200 do
    ZEILE [ I ] := '=' ;
  WRITELN ( ZEILE ) ;
end (* HAUPTPROGRAMM *) .
