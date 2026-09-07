program TESTSET9 ;


const STRICH = '-----------------------------------------------------'
      ;


type SETX = set of - 100 .. 100 ;
     SETY = set of 100 .. 200 ;
     SETZ = set of - 100 .. 300 ;
     SHORT = - 10000 .. 10000 ;


var X : SETX :=
        [ ] ;
    Y : SETY :=
        [ ] ;
    Z : SETZ ;
    X2 : SETX :=
         [ - 100 , - 50 , 0 , 50 , 100 ] ;
    Y2 : SETY :=
         [ 100 , 110 , 120 , 130 ] ;
    Z2 : SETZ ;



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
  Y := [ ] ;
  X := [ - 10 , 20 ] ;
  WRITELN ( 'should show -10 and 20' ) ;
  WRITE_SET ( X , TRUE ) ;
  WRITELN ( 'should show 120 and 150' ) ;
  Y := [ 120 , 150 ] ;
  WRITE_SET ( Y , TRUE ) ;
  WRITELN ( 'should show X2 = [ - 100 , - 50 , 0 , 50 , 100 ] ' ) ;
  WRITE_SET ( X2 , TRUE ) ;
  WRITELN ( 'should show Y2 = [ 100 , 110 , 120 , 130 ] ' ) ;
  WRITE_SET ( Y2 , TRUE ) ;
  WRITELN ( STRICH ) ;
  WRITELN ( 'test union of X and Y' ) ;
  WRITE_SET ( X , FALSE ) ;
  WRITE_SET ( Y , FALSE ) ;
  Z := X + Y ;
  WRITELN ( 'should show -10, 20, 120, 150 - when ZUN is ok' ) ;
  WRITE_SET ( Z , FALSE ) ;
  WRITELN ( STRICH ) ;
  WRITELN ( 'test union of X2 and Y2' ) ;
  WRITE_SET ( X2 , FALSE ) ;
  WRITE_SET ( Y2 , FALSE ) ;
  Z2 := X2 + Y2 ;
  WRITELN ( 'should show 8 values - when ZUN is ok' ) ;
  WRITE_SET ( Z2 , FALSE ) ;
  WRITELN ( STRICH ) ;
  WRITELN ( 'test intersect of X2 and Y2' ) ;
  WRITE_SET ( X2 , FALSE ) ;
  WRITE_SET ( Y2 , FALSE ) ;
  Z2 := X2 * Y2 ;
  WRITELN ( 'should only show 100 - when ZIS is ok' ) ;
  WRITE_SET ( Z2 , FALSE ) ;
  WRITELN ( STRICH ) ;
  WRITELN ( 'test difference X2 - Y2' ) ;
  WRITE_SET ( X2 , FALSE ) ;
  WRITE_SET ( Y2 , FALSE ) ;
  Z2 := X2 - Y2 ;
  WRITELN ( 'should show -100, -50, 0, 50 - when ZDI is ok' ) ;
  WRITE_SET ( Z2 , FALSE ) ;
  WRITELN ( STRICH ) ;
end (* HAUPTPROGRAMM *) .
