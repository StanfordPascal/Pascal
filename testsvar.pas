program TESTSVAR ( OUTPUT ) ;


type CHAR10 = array [ 1.. 10 ] of CHAR ;
     CHAR30 = array [ 1.. 30 ] of CHAR ;
     CHAR21 = array [ 20.. 40 ] of CHAR ;


var F10 : CHAR10 ;
    F30 : CHAR30 ;
    F21 : CHAR21 ;


begin (* HAUPTPROGRAMM *)
  F10 := 'Oppolzer' ;
  F30 := F10 ;
  WRITELN ( F30 ) ;
  F21 := 'Teststring Length 21' ;
  F30 := F21 ;
  WRITELN ( F30 ) ;
  F21 := F30 ;
end (* HAUPTPROGRAMM *) .
