program TESTINTS ( OUTPUT ) ;


var X : CHAR ( 8 ) ;
    I : INTEGER ;



procedure INTTOSTR ( CP : VOIDPTR ; LEN : INTEGER ; VAL : INTEGER ;
                   ZEROES : BOOLEAN ) ;

   var BUFFER : array [ 1 .. 20 ] of CHAR ;
       MINUS : BOOLEAN ;
       LETZT : INTEGER ;
       I : INTEGER ;
       LIMIT : INTEGER ;
       LENX : INTEGER ;
       POSX : INTEGER ;

   begin (* INTTOSTR *)
     if VAL < 0 then
       begin
         VAL := - VAL ;
         MINUS := TRUE
       end (* then *)
     else
       MINUS := FALSE ;
     I := 20 ;
     BUFFER := ' ' ;
     while VAL > 0 do
       begin
         LETZT := VAL MOD 10 ;
         BUFFER [ I ] := CHR ( ORD ( '0' ) + LETZT ) ;
         I := I - 1 ;
         VAL := VAL DIV 10 ;
       end (* while *) ;
     LIMIT := 20 - LEN ;
     if ZEROES then
       while I > LIMIT do
         begin
           BUFFER [ I ] := '0' ;
           I := I - 1 ;
         end (* while *) ;
     if MINUS then
       begin
         if ZEROES then
           I := I + 1 ;
         BUFFER [ I ] := '-' ;
         I := I - 1 ;
       end (* then *) ;
     LENX := 20 - I ;
     POSX := LEN - LENX ;
     MEMSET ( CP , ' ' , LEN ) ;
     MEMCPY ( PTRADD ( CP , POSX ) , ADDR ( BUFFER [ I + 1 ] ) , LENX )
              ;
   end (* INTTOSTR *) ;



begin (* HAUPTPROGRAMM *)
  I := 25 ;
  INTTOSTR ( ADDR ( X ) , 8 , I , TRUE ) ;
  WRITELN ( 'result inttostr = ' , X , ' expected: 00000025' ) ;
  I := - 25 ;
  INTTOSTR ( ADDR ( X ) , 8 , I , TRUE ) ;
  WRITELN ( 'result inttostr = ' , X , ' expected: -0000025' ) ;
  I := 25 ;
  INTTOSTR ( ADDR ( X ) , 8 , I , FALSE ) ;
  WRITELN ( 'result inttostr = ' , X , ' expected:       25' ) ;
  I := - 25 ;
  INTTOSTR ( ADDR ( X ) , 8 , I , FALSE ) ;
  WRITELN ( 'result inttostr = ' , X , ' expected:      -25' ) ;
end (* HAUPTPROGRAMM *) .
