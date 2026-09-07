program TESTSETA ;


type SETX = set of 0 .. 100 ;
     SETY = set of 100 .. 200 ;
     SHORT = - 10000 .. 10000 ;
     SETC = set of CHAR ;
     SETA = set of 'A' .. 'Z' ;


var X : SETX :=
        [ ] ;
    Y : SETY :=
        [ ] ;
    X2 : SETX :=
         [ 0 , 50 , 100 ] ;
    Y2 : SETY :=
         [ 110 , 120 , 130 ] ;
    C1 : SETC :=
         [ '0' , '1' , 'A' , 'X' , 'E' ] ;
    C2 : SETA :=
         [ 'B' , 'L' , 'Z' , 'A' ] ;
    C3 : SETC ;



procedure WRITE_SET ( const S : set of 0 .. 300 ) ;

   var I : INTEGER ;
       CP : -> CHAR ;
       SP : -> SHORT ;
       LEN : INTEGER ;

   begin (* WRITE_SET *)
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
     WRITE ( 'Inh.: ' ) ;
     for I := - 100 to 300 do
       if I in S then
         WRITE ( I : 1 , ' ' ) ;
     WRITELN
   end (* WRITE_SET *) ;



procedure WRITE_SETC ( const S : SETC ) ;

   var I : INTEGER ;
       CP : -> CHAR ;
       SP : -> SHORT ;
       LEN : INTEGER ;
       C : CHAR ;

   begin (* WRITE_SETC *)
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
     WRITE ( 'Inh.: ' ) ;
     for C := CHR ( 0 ) to CHR ( 255 ) do
       if C in S then
         WRITE ( C : 1 , ' ' ) ;
     WRITELN
   end (* WRITE_SETC *) ;



begin (* HAUPTPROGRAMM *)
  Y := [ ] ;
  X := [ 1 , 20 ] ;
  WRITE_SET ( X ) ;
  Y := [ 120 , 150 ] ;
  WRITE_SET ( Y ) ;
  WRITE_SET ( X2 ) ;
  WRITE_SET ( Y2 ) ;
  C3 := C1 ;
  WRITE_SETC ( C3 ) ;
  C3 := C2 ;
  WRITE_SETC ( C3 ) ;
  C3 := C2 + C1 ;
  WRITE_SETC ( C3 ) ;
  C3 := C2 * C1 ;
  WRITE_SETC ( C3 ) ;
  C3 := C2 - C1 ;
  WRITE_SETC ( C3 ) ;
end (* HAUPTPROGRAMM *) .
