program TESTLIT ( OUTPUT ) ;

//**********************************************************************
//$A+
//**********************************************************************



var C1 : CHAR ( 20 ) ;
    C2 : CHAR ( 20 ) ;


begin (* HAUPTPROGRAMM *)

  //******************************************************************
  // test error 257 in pass 2
  // error disappears, if statement sequence is changed ???
  //******************************************************************

  C2 := 'Oppolzer' ;
  C1 := 'Bernd' ;
end (* HAUPTPROGRAMM *) .
