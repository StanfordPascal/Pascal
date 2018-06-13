program TESTSTR3 ( OUTPUT ) ;

//**********************************************************************
//$A+                                                                   
//**********************************************************************



var C1 : CHAR ( 20 ) ;
    C2 : CHAR ( 20 ) ;
    CL1 : CHAR ( 500 ) ;
    CL2 : CHAR ( 500 ) ;
    VC1 : STRING ( 20 ) ;
    VC2 : STRING ( 40 ) ;
    X : INTEGER ;


begin (* HAUPTPROGRAMM *)

  //******************************************************************
  // tests stringrange                                                
  // no error at compile time                                         
  // pcint reports runtime error stringsize, which is OK IMO          
  //******************************************************************

  if FALSE then
    VC1 := 'zu langer string fuer C1' ;

  //******************************************************************
  // tests stringrange                                                
  // pcint reports runtime error stringsize                           
  //******************************************************************

  VC2 := 'zu langer string fuer C1' ;
  VC1 := VC2 ;
end (* HAUPTPROGRAMM *) .
