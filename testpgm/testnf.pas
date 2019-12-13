program TESTNF ( OUTPUT ) ;


var S1 , S2 , S3 : STRING ( 100 ) ;


begin (* HAUPTPROGRAMM *)
  S1 := 'Bernd Oppolzer' ;
  S2 := LEFT ( S1 , 5 ) ;
  S3 := RIGHT ( S1 , 8 ) ;
  WRITELN ( '<' , S1 , '>' ) ;
  WRITELN ( '<' , S2 , '>' ) ;
  WRITELN ( '<' , S3 , '>' ) ;
  S1 := RIGHT ( S1 , 50 ) ;
  S2 := LEFT ( S2 , 20 ) ;
  S3 := LEFT ( S3 , 4 ) ;
  WRITELN ( '<' , S1 , '>' ) ;
  WRITELN ( '<' , S2 , '>' ) ;
  WRITELN ( '<' , S3 , '>' ) ;
  WRITELN ( 'index     = ' , INDEX ( S1 , 'e' ) ) ;
  WRITELN ( 'lastindex = ' , LASTINDEX ( S1 , 'e' ) ) ;
  WRITELN ( 'index     = ' , INDEX ( S1 , 'er' ) ) ;
  WRITELN ( 'lastindex = ' , LASTINDEX ( S1 , 'er' ) ) ;
  WRITELN ( 'index     = ' , INDEX ( S1 , 'Oppo' ) ) ;
  WRITELN ( 'lastindex = ' , LASTINDEX ( S1 , 'Oppo' ) ) ;
  WRITELN ( 'index     = ' , INDEX ( S1 , '     Bernd' ) ) ;
  WRITELN ( 'lastindex = ' , LASTINDEX ( S1 , '     Bernd' ) ) ;
  WRITELN ( 'index     = ' , INDEX ( S2 , 'Bernd' ) ) ;
  WRITELN ( 'lastindex = ' , LASTINDEX ( S2 , 'Bernd' ) ) ;
  WRITELN ( 'index     = ' , INDEX ( S1 , '          ' ) ) ;
  WRITELN ( 'lastindex = ' , LASTINDEX ( S1 , '          ' ) ) ;
end (* HAUPTPROGRAMM *) .
