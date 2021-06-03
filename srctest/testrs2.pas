program TESTRS2 ( INPUT , OUTPUT ) ;

//**********************************************************
//$X+                                                       
//**********************************************************



var S : STRING ( 100 ) ;
    CH : CHAR ;
    I , J : INTEGER ;
    S1 : STRING ( 100 ) ;
    CC : CHAR ( 10 ) ;
    H : 0 .. 2000 ;
    F : CHAR ( 10 ) ;
    V : STRING ( 100 ) ;
    R : REAL ;
    B1 : BOOLEAN ;
    B2 : BOOLEAN ;


begin (* HAUPTPROGRAMM *)

  //**************
  // first tests  
  //**************

  WRITELN ( 'first tests' ) ;
  S := '36 24  123.45678ABCDEFGHIJKLMNOPQRSTUVWXYZ' ;
  WRITELN ( 'source   s  = ' , S ) ;
  READSTR ( S , CH : 2 , H : 4 , R : 8 , F : 6 , V ) ;
  WRITELN ( 'ch          = ' , CH ) ;
  WRITELN ( 'h           = ' , H : 1 ) ;
  WRITELN ( 'r           = ' , R : 1 : 7 ) ;
  WRITELN ( 'f           = ' , F ) ;
  WRITELN ( 'v           = ' , V ) ;

  //********************************
  // tests from pascal/vs brochure  
  //********************************

  WRITELN ( 'test pascal/vs example' ) ;
  S := '36 245ABCDEFGHIJK' ;
  READSTR ( S , I , J : 3 , CH , CC : 5 , S1 ) ;
  WRITELN ( 'source   s  = ' , S ) ;
  WRITELN ( 'i           = ' , I : 1 ) ;
  WRITELN ( 'j           = ' , J : 1 ) ;
  WRITELN ( 'ch          = ' , CH ) ;
  WRITELN ( 'cc          = ' , CC ) ;
  WRITELN ( 's1          = ' , S1 ) ;
  WRITELN ( 'length(s1)  = ' , LENGTH ( S1 ) : 1 ) ;

  //********************************
  // tests with boolean             
  //********************************

  WRITELN ( 'test with boolean' ) ;
  S := '36 245FALABCDETFGHIJK' ;
  READSTR ( S , I , J : 3 , CH , B2 : 3 , CC : 5 , B1 , S1 ) ;
  WRITELN ( 'source   s  = ' , S ) ;
  WRITELN ( 'i           = ' , I : 1 ) ;
  WRITELN ( 'j           = ' , J : 1 ) ;
  WRITELN ( 'ch          = ' , CH ) ;
  WRITELN ( 'cc          = ' , CC ) ;
  WRITELN ( 's1          = ' , S1 ) ;
  WRITELN ( 'length(s1)  = ' , LENGTH ( S1 ) : 1 ) ;
  WRITELN ( 'b1          = ' , B1 ) ;
  WRITELN ( 'b2          = ' , B2 ) ;

  //*************
  // more tests  
  //*************

  WRITELN ( 'more tests' ) ;
  S := '  1024   27' ;
  READSTR ( S , I , H ) ;
  WRITELN ( 'source   s  = ' , S ) ;
  WRITELN ( 'i           = ' , I ) ;
  WRITELN ( 'h           = ' , H ) ;
  READSTR ( S , I : 5 , H : 5 ) ;
  WRITELN ( 'source   s  = ' , S ) ;
  WRITELN ( 'i           = ' , I ) ;
  WRITELN ( 'h           = ' , H ) ;
  READSTR ( S , I : 4 , H : 4 ) ;
  WRITELN ( 'source   s  = ' , S ) ;
  WRITELN ( 'i           = ' , I ) ;
  WRITELN ( 'h           = ' , H ) ;
  READSTR ( S , I : 3 , H : 3 ) ;
  WRITELN ( 'source   s  = ' , S ) ;
  WRITELN ( 'i           = ' , I ) ;
  WRITELN ( 'h           = ' , H ) ;
end (* HAUPTPROGRAMM *) .
