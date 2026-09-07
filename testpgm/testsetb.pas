program TESTSETB ;


const STRICH = '-----------------------------------------------------'
      ;
      CONSTY : set of 36 .. 38 =
      [ 37 ] ;
      CONSTN : set of - 38 .. - 36 =
      [ - 37 ] ;


type SETX = set of - 100 .. 100 ;
     SETZ = set of - 100 .. 300 ;
     SHORT = - 10000 .. 10000 ;


var X : SETX :=
        [ ] ;
    Z : SETZ ;
    INTVAR1 : INTEGER ;
    INTVAR2 : INTEGER ;
    INTVAR3 : INTEGER ;
    Y : set of 36 .. 38 =
        [ 37 ] ;
    LINECNT : INTEGER ;



procedure WRITE_SET ( const S : SETZ ; SHOWDUMP : BOOLEAN ) ;

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
     WRITE ( 'Inh.: ' ) ;
     for I := - 100 to 300 do
       if I in S then
         WRITE ( I : 1 , ' ' ) ;
     WRITELN
   end (* WRITE_SET *) ;



begin (* HAUPTPROGRAMM *)
  WRITELN ( STRICH ) ;
  LINECNT := 65 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  WRITELN ( 'test with small set as a constant' ) ;
  Z := CONSTN ;
  WRITELN ( 'should show -37' ) ;
  WRITE_SET ( Z , FALSE ) ;
  WRITELN ( STRICH ) ;
  LINECNT := 72 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  WRITELN ( 'test with small set as a constant' ) ;
  Z := CONSTY ;
  WRITELN ( 'should show 37' ) ;
  WRITE_SET ( Z , FALSE ) ;
  WRITELN ( STRICH ) ;
  LINECNT := 79 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  WRITELN ( 'test with small set' ) ;
  Z := Y ;
  WRITELN ( 'should show 37' ) ;
  WRITE_SET ( Z , FALSE ) ;
  WRITELN ( STRICH ) ;
  LINECNT := 86 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  WRITELN ( 'test with set built from int constant' ) ;
  X := [ - 10 , 20 ] ;
  Z := [ 41 ] + [ 57 ] ;
  WRITELN ( 'should show 41 and 57' ) ;
  WRITE_SET ( Z , FALSE ) ;
  WRITELN ( STRICH ) ;
  LINECNT := 94 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  Z := [ 52 ] ;
  WRITELN ( 'should show 52' ) ;
  WRITE_SET ( Z , FALSE ) ;
  WRITELN ( STRICH ) ;
  LINECNT := 100 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  WRITELN ( 'test with set built from int variable' ) ;
  INTVAR1 := 35 ;
  INTVAR2 := 40 ;
  INTVAR3 := 43 ;
  Z := [ INTVAR1 ] ;
  WRITELN ( 'should show 35' ) ;
  WRITE_SET ( Z , FALSE ) ;
  WRITELN ( STRICH ) ;
  LINECNT := 110 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  Z := [ INTVAR1 , INTVAR2 ] ;
  WRITELN ( 'should show 35, 40' ) ;
  WRITE_SET ( Z , FALSE ) ;
  WRITELN ( STRICH ) ;
  LINECNT := 116 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  Z := [ INTVAR1 .. INTVAR2 ] ;
  WRITELN ( 'should show all elements 35 to 40' ) ;
  WRITE_SET ( Z , FALSE ) ;
  WRITELN ( STRICH ) ;
  LINECNT := 122 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  Z := [ 20 , INTVAR1 ] ;
  WRITELN ( 'should show 20, 35' ) ;
  WRITE_SET ( Z , FALSE ) ;
  WRITELN ( STRICH ) ;
  LINECNT := 128 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  Z := [ 20 , INTVAR1 .. INTVAR2 ] ;
  WRITELN ( 'should show 20 and all elements 35 to 40' ) ;
  WRITE_SET ( Z , FALSE ) ;
  WRITELN ( STRICH ) ;
  LINECNT := 134 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  Z := X + [ INTVAR1 ] ;
  WRITELN ( 'should show -10, 20, 35 ' ) ;
  WRITE_SET ( Z , FALSE ) ;
  WRITELN ( STRICH ) ;
  LINECNT := 140 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  Z := X + [ INTVAR1 .. INTVAR2 ] ;
  WRITELN ( 'should show -10, 20 and all elements 35 to 40' ) ;
  WRITE_SET ( Z , FALSE ) ;
  WRITELN ( STRICH ) ;
  LINECNT := 146 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  Z := [ 12 , INTVAR1 , 20 , INTVAR2 .. INTVAR3 , 50 ] ;
  WRITELN ( 'should show 12, 20, 35, 50 and elements 40 to 43' ) ;
  WRITE_SET ( Z , FALSE ) ;
  WRITELN ( STRICH ) ;
  LINECNT := 152 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  Z := [ 12 , 20 , 50 ] + [ INTVAR1 .. INTVAR2 ] ;
  WRITELN ( 'should show 12, 20, 50 and all elements 35 to 40' ) ;
  WRITE_SET ( Z , FALSE ) ;
  WRITELN ( STRICH ) ;
  LINECNT := 158 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  Z := [ INTVAR1 .. INTVAR2 ] + [ 12 , 20 , 50 ] ;
  WRITELN ( 'should show 12, 20, 50 and all elements 35 to 40' ) ;
  WRITE_SET ( Z , FALSE ) ;
  WRITELN ( STRICH ) ;
end (* HAUPTPROGRAMM *) .
