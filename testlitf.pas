program TESTLITF ( OUTPUT ) ;

//**********************************************************************
//$A+
//**********************************************************************



var C1 : CHAR ( 20 ) ;
    C2 : CHAR ( 20 ) ;
    C3 : CHAR ( 20 ) ;


begin (* HAUPTPROGRAMM *)

  //******************************************************************
  // test error 257 in pass 2
  // error disappears, if statement sequence is changed ???
  //******************************************************************

  C1 := 'Bernd' ;
  C2 := 'Oppolzer' ;
  C3 := 'Oppolzer' ;
end (* HAUPTPROGRAMM *) .
