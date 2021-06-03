program TESTSPA ( OUTPUT ) ;

type R = record
           X : CHAR ( 80 ) ;
         end ;

var TAB : array [ 1 .. 40000 ] of R ;
    SZ : INTEGER ;

begin (* HAUPTPROGRAMM *)
  SZ := SIZEOF ( TAB ) ;
  MEMSET ( ADDR ( TAB ) , ' ' , SZ ) ;
  WRITELN ( 'size = ' , SZ )
end (* HAUPTPROGRAMM *) .
