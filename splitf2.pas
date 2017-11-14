program SPLITF ( INPF , OUTPUT ) ;


type CHAR80 = array [ 1 .. 80 ] of CHAR ;
     CHAR20 = array [ 1 .. 20 ] of CHAR ;
     CHAR10 = array [ 1 .. 10 ] of CHAR ;
     CHAR8 = array [ 1 .. 8 ] of CHAR ;


var INPF : TEXT ;
    OUTF : TEXT ;
    ZEILE : CHAR80 ;
    ENDEKENNUNG : CHAR80 ;
    FIRST : BOOLEAN ;



procedure INTTOSTR ( CP : VOIDPTR ; LEN : INTEGER ; VAL : INTEGER ;
                   ZEROES : BOOLEAN ) ;

   EXTERNAL ;



procedure ASSIGN_NEXT_FILE ;

   var I : INTEGER ;
       DUMMY10 : CHAR10 ;
       FILENAME : CHAR8 ;
       FILETYPE : CHAR8 ;
       OUTFNAME : CHAR20 ;
       CH : CHAR ;

   begin (* ASSIGN_NEXT_FILE *)
     READ ( INPF , DUMMY10 , CH ) ;
     READSYMB ( INPF , ADDR ( FILENAME ) , 8 ) ;
     READ ( INPF , CH ) ;
     READSYMB ( INPF , ADDR ( FILETYPE ) , 8 ) ;
     READLN ( INPF ) ;
     OUTFNAME := ' ' ;
     WRITELN ( 'filename = <' , FILENAME , '>' ) ;
     WRITELN ( 'filetype = <' , FILETYPE , '>' ) ;
     UNPACK ( FILENAME , OUTFNAME , 1 ) ;
     I := 1 ;
     while OUTFNAME [ I ] <> ' ' do
       I := I + 1 ;
     OUTFNAME [ I ] := '.' ;
     I := I + 1 ;
     UNPACK ( FILETYPE , OUTFNAME , I ) ;
     WRITELN ( 'outfname = <' , OUTFNAME , '>' ) ;
     ASSIGN ( OUTF , ADDR ( OUTFNAME ) , 20 ) ;
     WRITELN ( 'assign: ' , OUTFNAME ) ;
     READLN ( INPF , ZEILE ) ;
   end (* ASSIGN_NEXT_FILE *) ;



begin (* HAUPTPROGRAMM *)
  MEMSET ( ADDR ( ENDEKENNUNG ) , '-' , 80 ) ;
  FIRST := TRUE ;
  repeat
    READLN ( INPF , ZEILE ) ;
    if ZEILE = ENDEKENNUNG then
      begin
        if not FIRST then
          CLOSE ( OUTF ) ;
        FIRST := FALSE ;
        ASSIGN_NEXT_FILE ;
        REWRITE ( OUTF ) ;
        continue ;
      end (* then *) ;
    if FIRST then
      begin
        WRITELN ( '+++ Fehler in 1. Eingabezeile' ) ;
        HALT
      end (* then *) ;
    WRITELN ( OUTF , ZEILE ) ;
  until EOF ( INPF ) ;
  CLOSE ( OUTF ) ;
end (* HAUPTPROGRAMM *) .
