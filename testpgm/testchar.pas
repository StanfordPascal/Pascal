program TESTCHAR ( OUTPUT ) ;


const TESTKONST = 'Bernd' 'Oppolzer' ;
      S_VS_1 = X'1b' '&l12D' X'0d0a' ;
      X = 15 ;
      Y = 1.2 ;


var CH : CHAR ;


begin (* HAUPTPROGRAMM *)
  CH := '''' ;
  WRITELN ( CH ) ;
  WRITELN ( 'sizeof string const  = ' , SIZEOF ( S_VS_1 ) ) ;
  WRITELN ( 'const                = ' , S_VS_1 ) ;
  WRITELN ( 'sizeof string const  = ' , SIZEOF ( TESTKONST ) ) ;
  WRITELN ( 'const                = ' , TESTKONST ) ;
  WRITELN ( 'sizeof integer const = ' , SIZEOF ( X ) , X ) ;
  WRITELN ( 'sizeof real const    = ' , SIZEOF ( Y ) , Y ) ;
end (* HAUPTPROGRAMM *) .
