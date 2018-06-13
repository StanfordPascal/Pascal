program TESTMCMP ( OUTPUT ) ;

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
  // tests memcmp konstant laenge = 20                                
  //******************************************************************

  C1 := 'Bernd' ;
  C2 := 'Oppolzer' ;
  X := MEMCMP ( ADDR ( C1 ) , ADDR ( C2 ) , 20 ) ;
  WRITELN ( 'memcmp (' , C1 , ',' , C2 , ') = ' , X ) ;
  C1 := 'Bernd' ;
  C2 := 'Bernd   ' ;
  X := MEMCMP ( ADDR ( C1 ) , ADDR ( C2 ) , 20 ) ;
  WRITELN ( 'memcmp (' , C1 , ',' , C2 , ') = ' , X ) ;
  C1 := 'Oppolzer' ;
  C2 := 'Bernd' ;
  X := MEMCMP ( ADDR ( C1 ) , ADDR ( C2 ) , 20 ) ;
  WRITELN ( 'memcmp (' , C1 , ',' , C2 , ') = ' , X ) ;

  //******************************************************************
  // tests memcmp konstant laenge = 500                               
  //******************************************************************

  MEMSET ( ADDR ( CL1 ) , 'A' , 500 ) ;
  MEMSET ( ADDR ( CL2 ) , 'B' , 500 ) ;
  X := MEMCMP ( ADDR ( CL1 ) , ADDR ( CL2 ) , 500 ) ;
  WRITELN ( 'memcmp (' , CL1 , ',' , CL2 , ') = ' , X ) ;
  MEMSET ( ADDR ( CL1 ) , 'A' , 500 ) ;
  MEMSET ( ADDR ( CL2 ) , 'A' , 500 ) ;
  X := MEMCMP ( ADDR ( CL1 ) , ADDR ( CL2 ) , 500 ) ;
  WRITELN ( 'memcmp (' , CL1 , ',' , CL2 , ') = ' , X ) ;
  MEMSET ( ADDR ( CL1 ) , 'B' , 500 ) ;
  MEMSET ( ADDR ( CL2 ) , 'A' , 500 ) ;
  X := MEMCMP ( ADDR ( CL1 ) , ADDR ( CL2 ) , 500 ) ;
  WRITELN ( 'memcmp (' , CL1 , ',' , CL2 , ') = ' , X ) ;

  //******************************************************************
  // tests memcmp variabel lang                                       
  //******************************************************************

  C1 := 'Bernd' ;
  C2 := 'Oppolzer' ;
  X := 20 ;
  X := MEMCMP ( ADDR ( C1 ) , ADDR ( C2 ) , X ) ;
  WRITELN ( 'memcmp (' , C1 , ',' , C2 , ') = ' , X ) ;
  C1 := 'Bernd' ;
  C2 := 'Bernd' ;
  X := 20 ;
  X := MEMCMP ( ADDR ( C1 ) , ADDR ( C2 ) , X ) ;
  WRITELN ( 'memcmp (' , C1 , ',' , C2 , ') = ' , X ) ;
  C1 := 'Oppolzer' ;
  C2 := 'Bernd' ;
  X := 20 ;
  X := MEMCMP ( ADDR ( C1 ) , ADDR ( C2 ) , X ) ;
  WRITELN ( 'memcmp (' , C1 , ',' , C2 , ') = ' , X ) ;
end (* HAUPTPROGRAMM *) .
