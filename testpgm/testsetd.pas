program TESTSETD ;

//**************************************************
// better code generation for const parameters      
// check correct code generation for card function  
//**************************************************



const STRICH = '-----------------------------------------------------'
      ;


type DIAG = set of - 7 .. 7 ;
     SHORT = - 10000 .. 10000 ;


var Z : DIAG ;
    VAR1 : INTEGER := 6 ;
    LINECNT : INTEGER ;
    C : INTEGER ;
    S : STRING ( 80 ) ;



procedure WRITE_SET ( const S : DIAG ; P : VOIDPTR ; SHOWDUMP : BOOLEAN
                    ) ;

   var I : INTEGER ;
       CP : -> CHAR ;
       SP : -> SHORT ;
       LEN : INTEGER ;

   begin (* WRITE_SET *)
     if SHOWDUMP then
       begin
         CP := PTRCAST ( ADDR ( S ) ) ;
         SP := PTRCAST ( ADDR ( S ) ) ;
         LEN := SP -> ;
         WRITE ( 'len: ' , LEN : 5 , ' ' ) ;
         SP := PTRADD ( SP , 2 ) ;
         WRITE ( 'offs: ' , SP -> : 5 , ' ' ) ;
         CP := PTRADD ( CP , 4 ) ;
         WRITE ( 'dump: ' ) ;
         for I := 1 to LEN do
           begin
             WRITE ( ORD ( CP -> ) : 1 , ' ' ) ;
             CP := PTRADD ( CP , 1 )
           end (* for *) ;
         WRITELN ;
       end (* then *) ;
     WRITE ( 'Addr: ' , P , ' ' ) ;
     WRITE ( 'Inh.: ' ) ;
     for I := - 7 to 7 do
       if I in S then
         WRITE ( I : 1 , ' ' ) ;
     WRITELN
   end (* WRITE_SET *) ;



procedure SETCALL ( S : DIAG ; X : INTEGER ) ;

   begin (* SETCALL *)
     WRITE ( 'start setcall - ' ) ;
     WRITE_SET ( S , ADDR ( S ) , FALSE ) ;
     if X < 9 then
       SETCALL ( S + [ X ] , X + 1 ) ;
     WRITE ( 'endof setcall - ' ) ;
     WRITE_SET ( S , ADDR ( S ) , FALSE ) ;
   end (* SETCALL *) ;



procedure SETCALL2 ( const S : DIAG ; X : INTEGER ) ;

   begin (* SETCALL2 *)
     WRITE ( 'start setcall2 - ' ) ;
     WRITE_SET ( S , ADDR ( S ) , FALSE ) ;
     if X < 9 then
       SETCALL2 ( S + [ X ] , X + 1 ) ;
     WRITE ( 'endof setcall2 - ' ) ;
     WRITE_SET ( S , ADDR ( S ) , FALSE ) ;
   end (* SETCALL2 *) ;



procedure TESTSTR ( const X : STRING ) ;

   begin (* TESTSTR *)
     WRITELN ( X )
   end (* TESTSTR *) ;



begin (* HAUPTPROGRAMM *)
  S := STRICH ;
  TESTSTR ( S ) ;
  LINECNT := 99 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  WRITELN ( 'test with small set as a constant' ) ;
  Z := [ 1 , 2 , 3 , 4 , 5 ] ;
  WRITELN ( 'should show 1 to 5 ' ) ;
  WRITE_SET ( Z , ADDR ( Z ) , FALSE ) ;
  WRITELN ( STRICH ) ;
  LINECNT := 106 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  WRITELN ( 'test set after setcall by value' ) ;
  Z := [ 1 , 2 , 3 , 4 , 5 ] ;
  SETCALL ( Z , VAR1 ) ;
  WRITELN ( 'should show 1 to 5 ' ) ;
  WRITE_SET ( Z , ADDR ( Z ) , FALSE ) ;
  WRITELN ( STRICH ) ;
  LINECNT := 114 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  WRITELN ( 'test set after setcall2 by const' ) ;
  Z := [ 1 , 2 , 3 , 4 , 5 ] ;
  SETCALL2 ( Z , VAR1 ) ;
  WRITELN ( 'should show 1 to 5 ' ) ;
  WRITE_SET ( Z , ADDR ( Z ) , FALSE ) ;
  WRITELN ( STRICH ) ;
  LINECNT := 122 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  WRITELN ( 'test card function' ) ;
  Z := [ 1 , 2 , 3 , 4 , 5 ] ;
  C := CARD ( Z ) ;
  WRITELN ( 'should show card (Z) = 5 ' ) ;
  WRITELN ( C ) ;
  WRITELN ( STRICH ) ;
  LINECNT := 130 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  WRITELN ( 'test card function' ) ;
  C := CARD ( [ 6 , 4 , 8 , 12 , 7 , 9 , 2 , 5 , 11 , 13 , 23 , 56 ] )
       ;
  WRITELN ( 'should show card (...) = 12' ) ;
  WRITELN ( C ) ;
  WRITELN ( STRICH ) ;
  LINECNT := 138 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  WRITELN ( 'test card function' ) ;
  Z := [ 1 , 2 , 3 , 4 , 5 ] ;
  C := CARD ( [ 6 , 4 , 8 , 12 , 7 , 9 , 2 , 5 , 11 , 13 , 23 , 56 ] +
       Z ) ;
  WRITELN ( 'should show card (...) = 14' ) ;
  WRITELN ( C ) ;
  WRITELN ( STRICH ) ;
  LINECNT := 147 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  WRITELN ( 'test card function' ) ;
  Z := [ 1 , 2 , 3 , 4 , 5 ] ;
  C := CARD ( [ 6 , 4 , 8 , 12 , 7 , 9 , 2 , 5 , 11 , 13 , 23 , 56 ] -
       Z ) ;
  WRITELN ( 'should show card (...) = 9' ) ;
  WRITELN ( C ) ;
  WRITELN ( STRICH ) ;
  LINECNT := 156 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  WRITELN ( 'test card function' ) ;
  Z := [ 1 , 2 , 3 , 4 , 5 ] ;
  C := CARD ( Z + [ 6 , 4 , 8 , 12 , 7 , 9 , 2 , 5 , 11 , 13 , 23 , 56
       ] + Z ) ;
  WRITELN ( 'should show card (...) = 14' ) ;
  WRITELN ( C ) ;
  WRITELN ( STRICH ) ;
end (* HAUPTPROGRAMM *) .
