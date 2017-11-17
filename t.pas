program TESTSVAR ( OUTPUT ) ;

(************************************************)
(*                                              *)
(*                                              *)
(*                                              *)
(************************************************)



type CHAR10 = array [ 1 .. 10 ] of CHAR ;
     CHAR30 = array [ 1 .. 30 ] of CHAR ;
     CHAR21 = array [ 20 .. 40 ] of CHAR ;


var F10 : CHAR10 ;   // should be char (10), maybe ??
    F30 : CHAR30 ;   // char (30)
    F21 : CHAR21 ;   /* char (21)                  */
    L : INTEGER ;
    CH : CHAR ;


begin (* HAUPTPROGRAMM *)
  F10 := 'Oppolzer' ;
  F30 := F10 ;
  WRITELN ( F30 ) ;
  F21 := 'Teststring Length 21' ;
  F30 := F21 ;
  WRITELN ( F30 ) ;
  F30 := 'Test123456Test123456Test123456' ;
  F21 := F30 ;
  L := 21 ;
  CH := '*' ;
  MEMSET ( ADDR ( F21 ) , x'00' , 21 ) ;
  MEMSET ( ADDR ( F21 ) , CHR ( 0 ) , 21 ) ;
  MEMSET ( ADDR ( F21 ) , ' ' , 21 ) ;
  MEMSET ( ADDR ( F21 ) , ' ' , L ) ;
  MEMSET ( ADDR ( F21 ) , CH , L ) ;
  MEMCPY ( ADDR ( F21 ) , ADDR ( F30 ) , 21 ) ;
  WRITELN ( F21 ) ;
  L := 21 ;
  MEMCPY ( ADDR ( F21 ) , ADDR ( F30 ) , L ) ;
  WRITELN ( F21 ) ;
end (* HAUPTPROGRAMM *) .
