program SPLITF ( INPF , OUTPUT ) ;


type CHAR80 = array [ 1 .. 80 ] of CHAR ;
     CHAR20 = array [ 1 .. 20 ] of CHAR ;
     CHAR8 = array [ 1 .. 8 ] of CHAR ;


var INPF : TEXT ;
    OUTF : TEXT ;
    OUTFNAME : CHAR20 ;
    ZEILE : CHAR80 ;
    ENDEKENNUNG : CHAR80 ;
    FNO : INTEGER ;
    CFNO : CHAR8 ;



procedure INTTOSTR ( CP : VOIDPTR ; LEN : INTEGER ; VAL : INTEGER ;
                   ZEROES : BOOLEAN ) ;

   EXTERNAL ;



begin (* HAUPTPROGRAMM *)
  MEMSET ( ADDR ( ENDEKENNUNG ) , '*' , 80 ) ;
  ENDEKENNUNG [ 1 ] := '(' ;
  ENDEKENNUNG [ 80 ] := ')' ;
  OUTFNAME := 'outf0001.txt' ;
  FNO := 1 ;
  ASSIGN ( OUTF , ADDR ( OUTFNAME ) , 20 ) ;
  WRITELN ( 'assign: ' , OUTFNAME ) ;
  REWRITE ( OUTF ) ;
  repeat
    READLN ( INPF , ZEILE ) ;
    WRITELN ( 'testout: ' , ZEILE ) ;
    if ZEILE = ENDEKENNUNG then
      begin
        CLOSE ( OUTF ) ;
        FNO := FNO + 1 ;
        INTTOSTR ( ADDR ( CFNO ) , 8 , FNO , TRUE ) ;
        MEMCPY ( ADDR ( OUTFNAME [ 5 ] ) , ADDR ( CFNO [ 5 ] ) , 4 ) ;
        ASSIGN ( OUTF , ADDR ( OUTFNAME ) , 20 ) ;
        WRITELN ( 'assign: ' , OUTFNAME ) ;
        REWRITE ( OUTF ) ;
      end (* then *) ;
    WRITELN ( OUTF , ZEILE ) ;
  until EOF ( INPF ) ;
  CLOSE ( OUTF ) ;
end (* HAUPTPROGRAMM *) .
