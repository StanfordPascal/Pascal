program TESTSETC ;


const STRICH = '-----------------------------------------------------'
      ;


type DIAG = set of - 7 .. 7 ;
     SHORT = - 10000 .. 10000 ;


var Z : DIAG ;
    VAR1 : INTEGER ;
    LINECNT : INTEGER ;



procedure WRITE_SET ( const S : DIAG ; SHOWDUMP : BOOLEAN ) ;

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
     for I := - 7 to 7 do
       if I in S then
         WRITE ( I : 1 , ' ' ) ;
     WRITELN
   end (* WRITE_SET *) ;



begin (* HAUPTPROGRAMM *)
  WRITELN ( STRICH ) ;
  LINECNT := 54 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  WRITELN ( 'test with small set as a constant' ) ;
  Z := [ - 2 , 2 ] ;
  WRITELN ( 'should show -2, 2' ) ;
  WRITE_SET ( Z , FALSE ) ;
  WRITELN ( STRICH ) ;
  LINECNT := 61 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  WRITELN ( 'test union' ) ;
  VAR1 := - 3 ;
  Z := Z + [ VAR1 ] ;
  WRITELN ( 'should show -3, -2, 2' ) ;
  WRITE_SET ( Z , FALSE ) ;
  WRITELN ( STRICH ) ;
  LINECNT := 68 ;
  WRITE ( 'Zeile ' , LINECNT : 1 , ': ' ) ;
  WRITELN ( 'test othe union' ) ;
  VAR1 := 3 ;
  Z := Z + [ VAR1 ] ;
  WRITELN ( 'should show -3, -2, 2, 3' ) ;
  WRITE_SET ( Z , FALSE ) ;
  WRITELN ( STRICH ) ;
end (* HAUPTPROGRAMM *) .
