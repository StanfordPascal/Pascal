program TESTLSTR ( OUTPUT ) ;

(********)
(*$A+   *)
(********)



const NL = x'0a' ;
      LONG_STRING = 'this is a long string' x'0a'    // nl should be
      'which occupies' x'0a'                         // possible
      'more than one source line' ;                  // here ...


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
  WRITELN ( LONG_STRING ) ;
  ZEILE := 'this is a long string' || NL ||  // nl goes well with
           'which occupies' || NL ||         // concatenation
           'more than one source line' ;     // here ...
  WRITELN ( ZEILE ) ;
end (* HAUPTPROGRAMM *) .
