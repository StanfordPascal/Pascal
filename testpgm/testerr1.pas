program TESTERR1 ( OUTPUT ) ;

//**********************************************************************
//$A+                                                                   
//**********************************************************************



var FTEXT : array [ 1 .. 120 ] of CHAR ;
    CP1 , CP2 , CP3 : -> CHAR ;
    INFOPOS , L : INTEGER ;


begin (* HAUPTPROGRAMM *)
  INFOPOS := 25 ;
  L := 28 ;
  CP1 := ADDR ( FTEXT ) ;
  CP2 := PTRADD ( ADDR ( FTEXT ) , 32 ) ;

  //******************************************************************
  // das folgende Statement erzeugt P-CODE ADA,                       
  // dieser wird durch Pascal2 falsch Åbersetzt (AH ...)              
  //******************************************************************

  CP3 := PTRADD ( ADDR ( FTEXT ) , L - INFOPOS ) ;
  WRITELN ( 'cp1, cp2, cp3 = ' , CP1 : 10 , CP2 : 10 , CP3 : 10 ) ;
end (* HAUPTPROGRAMM *) .
