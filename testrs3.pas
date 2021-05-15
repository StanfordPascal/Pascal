program TESTRS3 ( INPUT , OUTPUT ) ;


var S : STRING ( 80 ) ;
    C : CHAR ;
    I1 , I2 : INTEGER ;
    KOMMA : CHAR ;


begin (* HAUPTPROGRAMM *)
  WRITELN ( 'bitte string eingeben:' ) ;
  WRITELN ( 'c komma i1 komma i2 - oder weniger' ) ;
  READLN ( S ) ;
  READSTR ( S , C , KOMMA , I1 , KOMMA , I2 ) ;
  WRITELN ( C , I1 , I2 )
end (* HAUPTPROGRAMM *) .
