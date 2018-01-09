program TESTSVAR ( OUTPUT ) ;

(************************************************)
(*$A+                                           *)
(*                                              *)
(*                                              *)
(************************************************)



type CHAR10 = array [ 1 .. 10 ] of CHAR ;
     CHAR30 = array [ 1 .. 30 ] of CHAR ;
     CHAR21 = array [ 20 .. 40 ] of CHAR ;
     CHARL = array [ 1 .. 4000 ] of CHAR ;


var F10 : CHAR10 ;   // should be char (10), maybe ??
    F30 : CHAR30 ;   // char (30)
    F21 : CHAR21 ;   /* char (21)                  */
    L : INTEGER ;
    CH : CHAR ;
    BUF : CHARL ;
    P : ANYPTR ;


begin (* HAUPTPROGRAMM *)
  F10 := 'Oppolzer' ;
  F30 := F10 ;
  WRITELN ( F30 ) ;
  F21 := 'Teststring Length 21' ;
  F30 := F21 ;
  WRITELN ( F30 ) ;
  F30 := 'Test123456Test123456Test123456' ;

  //******************************************************************
  // F21 := F30 ;                                                     
  //******************************************************************

  L := 21 ;
  CH := '*' ;
  MEMSET ( ADDR ( F21 ) , x'00' , 21 ) ;
  MEMSET ( ADDR ( F21 ) , CHR ( 0 ) , 21 ) ;
  MEMSET ( ADDR ( F21 ) , ' ' , 21 ) ;
  MEMSET ( ADDR ( F21 ) , ' ' , L ) ;
  WRITELN ( '<' , F21 , '>' ) ;
  MEMSET ( ADDR ( F21 ) , '$' , L ) ;
  WRITELN ( '<' , F21 , '>' ) ;
  MEMSET ( ADDR ( F21 ) , CH , L ) ;
  WRITELN ( '<' , F21 , '>' ) ;
  MEMSET ( ADDR ( BUF ) , CH , 2000 ) ;
  P := ADDR ( BUF ) ;
  P := PTRADD ( P , 2000 ) ;
  MEMSET ( P , CH , 2000 ) ;
  WRITELN ( '<' , BUF , '>' ) ;
  L := 2000 ;
  MEMSET ( ADDR ( BUF ) , CH , L ) ;
  WRITELN ( '<' , BUF , '>' ) ;
  P := ADDR ( BUF ) ;
  P := PTRADD ( P , L ) ;
  MEMSET ( P , CH , L ) ;
  WRITELN ( '<' , BUF , '>' ) ;
  MEMCPY ( ADDR ( F21 ) , ADDR ( F30 ) , 21 ) ;
  WRITELN ( F21 ) ;
  L := 21 ;
  MEMCPY ( ADDR ( F21 ) , ADDR ( F30 ) , L ) ;
  WRITELN ( F21 ) ;
end (* HAUPTPROGRAMM *) .
