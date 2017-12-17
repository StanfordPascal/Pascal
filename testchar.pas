program TESTCHAR ( OUTPUT ) ;

//**********************************************************************
//$A+                                                                   
//**********************************************************************



const TESTKONST = 'Bernd ' 'Oppolzer' ;
      S_VS_1 = X'1b' '&l12D' X'0d0a' ;
      X = 15 ;
      Y = 1.2 ;


type CHAR25 = array [ 1 .. 25 ] of CHAR ;
     CHAR30 = array [ 1 .. 30 ] of CHAR ;


var CH : CHAR ;
    CH2 : CHAR ( 25 ) ;
    CH3 : CHAR ( 14 ) ;
    CH4 : CHAR ( 25 ) ;
    CH5 : CHAR25 ;
    CH6 : CHAR30 ;
    D1 : DECIMAL ( 7 ) ;
    D2 : DECIMAL ( 15 , 2 ) ;
    D3 : DECIMAL ( 7 ) ;
    D4 : DECIMAL ( 25 , 0 ) ;
    S1 : STRING ( 254 ) ;
    S2 : STRING ( 3000 ) ;
    V1 : VARCHAR ( 254 ) ;
    V2 : VARCHAR ( 3000 ) ;

    //************************************
    // IFALSCH : INTEGER ( 2 ) ;          
    // D3 : DECIMAL ;                     
    // S4 : STRING ;                      
    // V4 : VARCHAR ;                     
    // D5 : DECIMAL ( 7 , 13 ) ;          
    // D6 : DECIMAL ( 50 , 0 ) ;          
    // S3 : STRING ( 0 ) ;                
    // V3 : VARCHAR ( 0 ) ;               
    //************************************

    TESTCP : -> CHAR ;



procedure TESTWRITE ( X : CHAR ( 30 ) ) ;

   begin (* TESTWRITE *)
     WRITELN ( 'testwrite: x = <' , X , '>' ) ;
   end (* TESTWRITE *) ;



procedure TESTWRITE2 ( X : CHAR30 ) ;

   begin (* TESTWRITE2 *)
     WRITELN ( 'testwrite2: x = <' , X , '>' ) ;
   end (* TESTWRITE2 *) ;



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
  WRITELN ( 'test1: ch = ' , CH ) ;
  CH3 := CH ;
  WRITELN ( 'test1: ch3 = <' , CH3 , '>' ) ;
  CH3 := TESTKONST ;
  CH2 := CH3 ;
  D2 := 1234.56 ;
  S1 := 'das ist ein String' ;
  CH5 := CH2 ;
  TESTWRITE ( CH2 ) ;
  TESTWRITE ( CH5 ) ;
  CH5 := 'das ist ein String' ;
  CH2 := CH5 ;
  TESTWRITE2 ( CH5 ) ;
  WRITELN ( 'test1: ch2 = <' , CH2 , '>' ) ;
end (* HAUPTPROGRAMM *) .
