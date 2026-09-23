program TESTSETE ;

//**************************************************
// better code generation for const parameters      
// check correct code generation for card function  
//**************************************************



type DIAG = set of - 7 .. 7 ;
     SHORT = - 10000 .. 10000 ;


var Z : DIAG ;
    Z2 : DIAG ;
    Z3 : DIAG ;
    RES : BOOLEAN ;



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



begin (* HAUPTPROGRAMM *)
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test with small set as a constant' ) ;
  Z := [ 1 , 2 , 3 , 4 , 5 ] ;
  WRITELN ( 'should show 1 to 5 ' ) ;
  WRITE_SET ( Z , ADDR ( Z ) , FALSE ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE ( 5 ) : 1 , ': ' ) ;
  WRITELN ( 'test set after setcall by value' ) ;
  Z2 := [ 1 , 3 , 4 ] ;
  WRITELN ( 'should show 1, 3, 4 ' ) ;
  WRITE_SET ( Z2 , ADDR ( Z2 ) , FALSE ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE ( RES ) : 1 , ': ' ) ;
  WRITELN ( 'test set comparison (inclusion <=)' ) ;
  WRITE_SET ( Z2 , ADDR ( Z2 ) , FALSE ) ;
  WRITE_SET ( Z , ADDR ( Z ) , FALSE ) ;
  RES := Z2 <= Z ;
  WRITELN ( 'should show true' ) ;
  WRITELN ( RES ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE ( 2 + 3 ) : 1 , ': ' ) ;
  WRITELN ( 'test set comparison (inclusion <=)' ) ;
  WRITE_SET ( Z , ADDR ( Z ) , FALSE ) ;
  WRITE_SET ( Z2 , ADDR ( Z2 ) , FALSE ) ;
  RES := Z <= Z2 ;
  WRITELN ( 'should show false' ) ;
  WRITELN ( RES ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test set comparison (equal)' ) ;
  WRITE_SET ( Z2 , ADDR ( Z2 ) , FALSE ) ;
  WRITE_SET ( Z , ADDR ( Z ) , FALSE ) ;
  RES := Z2 = Z ;
  WRITELN ( 'should show false' ) ;
  WRITELN ( RES ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test set comparison (not equal)' ) ;
  WRITE_SET ( Z2 , ADDR ( Z2 ) , FALSE ) ;
  WRITE_SET ( Z , ADDR ( Z ) , FALSE ) ;
  RES := Z2 <> Z ;
  WRITELN ( 'should show true' ) ;
  WRITELN ( RES ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test set comparison (inclusion >=)' ) ;
  WRITE_SET ( Z2 , ADDR ( Z2 ) , FALSE ) ;
  WRITE_SET ( Z , ADDR ( Z ) , FALSE ) ;
  RES := Z2 >= Z ;
  WRITELN ( 'should show false' ) ;
  WRITELN ( RES ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test set comparison (inclusion >=)' ) ;
  WRITE_SET ( Z , ADDR ( Z ) , FALSE ) ;
  WRITE_SET ( Z2 , ADDR ( Z2 ) , FALSE ) ;
  RES := Z >= Z2 ;
  WRITELN ( 'should show true' ) ;
  WRITELN ( RES ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test set comparison (equal)' ) ;
  Z3 := [ 4 , 4 , 4 , 3 , 1 ] ;
  WRITE_SET ( Z2 , ADDR ( Z2 ) , FALSE ) ;
  WRITE_SET ( Z3 , ADDR ( Z3 ) , FALSE ) ;
  RES := Z2 = Z3 ;
  WRITELN ( 'should show true' ) ;
  WRITELN ( RES ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test set comparison (inclusion <=)' ) ;
  RES := [ 1 .. 5 ] <= [ - 5 .. - 2 ] ;
  WRITELN ( 'should show false' ) ;
  WRITELN ( RES ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test set comparison (inclusion <=)' ) ;
  RES := [ - 5 .. - 2 ] <= [ 1 .. 5 ] ;
  WRITELN ( 'should show false' ) ;
  WRITELN ( RES ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test set comparison (inclusion <=)' ) ;
  RES := [ 1 .. 5 ] <= [ 100 .. 150 ] ;
  WRITELN ( 'should show false' ) ;
  WRITELN ( RES ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test set comparison (inclusion <=)' ) ;
  RES := [ 100 .. 150 ] <= [ 1 .. 5 ] ;
  WRITELN ( 'should show false' ) ;
  WRITELN ( RES ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test set comparison (inclusion <=)' ) ;
  RES := [ ] <= [ 100 .. 150 ] ;
  WRITELN ( 'should show true' ) ;
  WRITELN ( RES ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test set comparison (inclusion <=)' ) ;
  RES := [ 1 .. 5 ] >= [ - 5 .. - 2 ] ;
  WRITELN ( 'should show false' ) ;
  WRITELN ( RES ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test set comparison (inclusion <=)' ) ;
  RES := [ - 5 .. - 2 ] >= [ 1 .. 5 ] ;
  WRITELN ( 'should show false' ) ;
  WRITELN ( RES ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test set comparison (inclusion <=)' ) ;
  RES := [ 1 .. 5 ] >= [ 100 .. 150 ] ;
  WRITELN ( 'should show false' ) ;
  WRITELN ( RES ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test set comparison (inclusion <=)' ) ;
  RES := [ 100 .. 150 ] >= [ 1 .. 5 ] ;
  WRITELN ( 'should show false' ) ;
  WRITELN ( RES ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test set comparison (inclusion <=)' ) ;
  RES := [ 100 .. 150 ] >= [ ] ;
  WRITELN ( 'should show true' ) ;
  WRITELN ( RES ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test set comparison (strict inclusion <)' ) ;
  WRITE_SET ( Z2 , ADDR ( Z2 ) , FALSE ) ;
  WRITE_SET ( Z , ADDR ( Z ) , FALSE ) ;
  RES := Z2 < Z ;
  WRITELN ( 'should show true' ) ;
  WRITELN ( RES ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test set comparison (strict inclusion >)' ) ;
  WRITE_SET ( Z2 , ADDR ( Z2 ) , FALSE ) ;
  WRITE_SET ( Z , ADDR ( Z ) , FALSE ) ;
  RES := Z2 > Z ;
  WRITELN ( 'should show false' ) ;
  WRITELN ( RES ) ;
  WRITELN ( '--------------------------------------------------' ) ;
end (* HAUPTPROGRAMM *) .
