program COMPERRD ( OUTPUT ) ;

//***** 
//$A+,X+
//***** 



var P : ANYPTR ;
    X24 : CHAR ( 24 ) ;
    X40 : CHAR ( 40 ) ;
    CH : CHAR ;
    B : BOOLEAN ;
    S24 : STRING ( 24 ) ;
    S40 : STRING ( 40 ) ;
    V250 : STRING ( 250 ) ;
    V1000 : CHAR ( 1000 ) ;
    V1000A : CHAR ( 1000 ) ;

    //**********************************
    // static X1000 : CHAR ( 1000 ) ;   
    //        X1000A : CHAR ( 1000 ) ;  
    //**********************************




procedure TEST_COMPARE1 ;

   begin (* TEST_COMPARE1 *)
     CH := 'X' ;
     B := CH < 'A    ' ;
     WRITELN ( B , 'testcase 008:'
               ' one char variable left - false expected' : 60 ) ;
     B := 'B    ' < CH ;
     WRITELN ( B , 'testcase 009:'
               ' one char variable right - true expected' : 60 ) ;
   end (* TEST_COMPARE1 *) ;



begin (* HAUPTPROGRAMM *)
  P := NIL ;
  S24 := 'Hugo' ;
  WRITELN ( 'testcase 001:' ' concat: <' , S24 , '>' ) ;
  S24 := 'x' || 'Hugo' || 'x' ;
  WRITELN ( 'testcase 002: concat: <' , S24 , '>' ) ;
  S24 := 'Hugo' || '' ;
  WRITELN ( 'testcase 003: concat: <' , S24 , '>' ) ;
  S24 := 'Hu' || 'go' || 'x' ;
  WRITELN ( 'testcase 004: concat: <' , S24 , '>' ) ;
  S24 := 'Hu' || 'go' || '' ;
  WRITELN ( 'testcase 005: concat: <' , S24 , '>' ) ;
  S24 := '' || 'Hugo' || '' ;
  WRITELN ( 'testcase 006: concat: <' , S24 , '>' ) ;
  V250 := 'Bernd ' || S24 || ' Oppolzer' ;
  WRITELN ( 'testcase 007: concat: <' , V250 , '>' ) ;
  TEST_COMPARE1 ;

  //**********************************************************
  // err:  V1000 := 'Bernd ' || STR ( X24 ) || ' Oppolzer' ;  
  //**********************************************************

  X24 := 'Bernd' ;
  X40 := X24 ;
  S24 := 'Bernd' ;
  S40 := 'Bernd' ;
  CH := 'A' ;

  //****************************
  // char and string constants  
  //****************************

  B := '' < 'A    ' ;
  WRITELN ( B , 'testcase 010:' ' empty constant left - true expected'
            : 60 ) ;
  B := 'A    ' < '' ;
  WRITELN ( B , 'testcase 011:'
            ' empty constant right - false expected' : 60 ) ;
  B := ' ' < 'A    ' ;
  WRITELN ( B , 'testcase 012:'
            ' one char constant left - true expected' : 60 ) ;
  B := 'A    ' < ' ' ;
  WRITELN ( B , 'testcase 013:'
            ' one char constant right - false expected' : 60 ) ;

  //********************************
  // short strings and char arrays  
  //********************************

  B := '' < X24 ;
  WRITELN ( B , 'testcase 014:' ' empty constant left - true expected'
            : 60 ) ;
  B := X24 < '' ;
  WRITELN ( B , 'testcase 015:'
            ' empty constant right - false expected' : 60 ) ;
  B := ' ' < X24 ;
  WRITELN ( B , 'testcase 016:'
            ' one char constant left - true expected' : 60 ) ;
  B := X24 < ' ' ;
  WRITELN ( B , 'testcase 017:'
            ' one char constant right - false expected' : 60 ) ;

  //****************************
  // char vars and char arrays  
  //****************************

  B := CH < X24 ;
  WRITELN ( B , 'testcase 018:'
            ' one char variable left - true expected' : 60 ) ;
  B := X24 < CH ;
  WRITELN ( B , 'testcase 019:'
            ' one char variable right - false expected' : 60 ) ;

  //********************************
  // char arrays of different size  
  //********************************

  B := 'Bernd   ' = 'Bernd' ;
  WRITELN ( B , 'testcase 020:'
            ' constants long before short - true expected' : 60 ) ;
  B := 'Bernd' = 'Bernd   ' ;
  WRITELN ( B , 'testcase 021:'
            ' constants short before long - true expected' : 60 ) ;
  B := X24 = 'Bernd  ' ;
  WRITELN ( B , 'testcase 022:'
            ' x24 and short constant - true expected' : 60 ) ;
  B := STR ( X24 ) = 'Be' || 'rnd ' ;
  WRITELN ( B , 'testcase 023:'
            ' x24 and concat of constants - true expected' : 60 ) ;
  B := 'Be' || 'rnd ' = X24 ;
  WRITELN ( B , 'testcase 024:'
            ' x24 and concat of constants - true expected' : 60 ) ;
  B := X24 = 'Bernd                                 ' ;
  WRITELN ( B , 'testcase 025:'
            ' x24 and long constant - true expected' : 60 ) ;
  B := X40 = X24 ;
  WRITELN ( B , 'testcase 026:' ' x40 and x24 - true expected' : 60 ) ;
  B := X24 = X40 ;
  WRITELN ( B , 'testcase 027:' ' x24 and x40 - true expected' : 60 ) ;

  //**********
  // strings  
  //**********

  B := S40 = S24 ;
  WRITELN ( B , 'testcase 028:' ' s40 and s24 - true expected' : 60 ) ;
  B := S24 = S40 ;
  WRITELN ( B , 'testcase 029:' ' s24 and s40 - true expected' : 60 ) ;
  S24 := 'Bernd' ;
  S40 := 'Bernd ' ;
  B := S40 = S24 ;
  WRITELN ( B , 'testcase 030:'
            ' s40 and s24 - true is ok (trailing blanks)' : 60 ) ;
  B := S24 = S40 ;
  WRITELN ( B , 'testcase 031:'
            ' s24 and s40 - true is ok (trailing blanks)' : 60 ) ;

  //**********                     
  // try compare carr of large size
  //**********                     

  MEMSET ( ADDR ( V1000 ) , ' ' , 1000 ) ;
  MEMSET ( ADDR ( V1000A ) , ' ' , 1000 ) ;
  V1000A [ 1 ] := 'A' ;
  B := V1000 = V1000A ;
  WRITELN ( B , 'testcase 032:' ' v1000 and v1000a - false expected' :
            60 ) ;
end (* HAUPTPROGRAMM *) .
