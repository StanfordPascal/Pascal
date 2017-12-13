program TESTCHAR ( OUTPUT ) ;


const TESTKONST = 'Bernd ' 'Oppolzer' ;
      S_VS_1 = X'1b' '&l12D' X'0d0a' ;
      X = 15 ;
      Y = 1.2 ;


var CH : CHAR ;
    CH2 : CHAR ( 25 ) ;
    CH3 : CHAR ( 12 ) ;



procedure TESTWRITE ( X : CHAR ( 30 ) ) ;

   begin (* TESTWRITE *)
     WRITELN ( 'testwrite: x = ' , X ) ;
   end (* TESTWRITE *) ;



begin (* HAUPTPROGRAMM *)
  CH := '''' ;
  WRITELN ( CH ) ;
  WRITELN ( 'sizeof string const  = ' , SIZEOF ( S_VS_1 ) ) ;
  WRITELN ( 'const                = ' , S_VS_1 ) ;
  WRITELN ( 'sizeof string const  = ' , SIZEOF ( TESTKONST ) ) ;
  WRITELN ( 'const                = ' , TESTKONST ) ;
  WRITELN ( 'sizeof integer const = ' , SIZEOF ( X ) , X ) ;
  WRITELN ( 'sizeof real const    = ' , SIZEOF ( Y ) , Y ) ;
  WRITELN ( 'Tests neue char-Datentypen:' ) ;
  CH := 'A' ;
  CH3 := CH ;
  CH3 := TESTKONST ;
  CH2 := CH3 ;
  TESTWRITE ( CH2 ) ;
end (* HAUPTPROGRAMM *) .
