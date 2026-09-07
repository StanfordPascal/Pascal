program TESTSET8 ;


type SETX = set of - 500 .. 500 ;


var X : set of 0 .. 120 ;
    Y : set of 0 .. 200 ;
    Z : set of 0 .. 300 ;
    ZL : set of 0 .. 30000 ;
    S1 : SETX :=
         [ - 50 , 150 ] ;
    S2 : set of - 100 .. 100 :=
         [ - 70 , 70 ] ;
    S3 : set of - 30 .. 120 :=
         [ - 10 , 80 ] ;
    SC : set of CHAR :=
         [ '0' .. '9' ] ;
    SA : set of 'A' .. 'Z' :=
         [ 'J' .. 'N' ] ;



procedure WRITE_SETV ( S : set of 0 .. 300 ) ;

   var I : INTEGER ;
       CP : -> CHAR ;

   begin (* WRITE_SETV *)
     CP := PTRCAST ( ADDR ( S ) ) ;
     WRITE ( 'Dump: ' ) ;
     for I := 1 to 38 do
       begin
         WRITE ( ORD ( CP -> ) : 1 , ' ' ) ;
         CP := PTRADD ( CP , 1 )
       end (* for *) ;
     WRITELN ;
     for I := 0 to 300 do
       if I in S then
         WRITE ( I : 1 , ' ' ) ;
     WRITELN
   end (* WRITE_SETV *) ;



procedure WRITE_SET ( const S : set of 0 .. 300 ) ;

   var I : INTEGER ;
       CP : -> CHAR ;

   begin (* WRITE_SET *)
     CP := PTRCAST ( ADDR ( S ) ) ;
     WRITE ( 'Dump: ' ) ;
     for I := 1 to 38 do
       begin
         WRITE ( ORD ( CP -> ) : 1 , ' ' ) ;
         CP := PTRADD ( CP , 1 )
       end (* for *) ;
     WRITELN ;
     for I := 0 to 300 do
       if I in S then
         WRITE ( I : 1 , ' ' ) ;
     WRITELN
   end (* WRITE_SET *) ;



procedure WRITE_SETL ( const S : set of 0 .. 30000 ) ;

   var I : INTEGER ;

   begin (* WRITE_SETL *)
     for I := 0 to 30000 do
       if I in S then
         WRITE ( I : 1 , ' ' ) ;
     WRITELN
   end (* WRITE_SETL *) ;



procedure WRITE_SET2 ( const S : SETX ) ;

   var I : INTEGER ;

   begin (* WRITE_SET2 *)
     for I := - 500 to 500 do
       if I in S then
         WRITE ( I : 1 , ' ' ) ;
     WRITELN
   end (* WRITE_SET2 *) ;



procedure WRITE_SETC ( const S : set of CHAR ) ;

   var C : CHAR ;

   begin (* WRITE_SETC *)
     for C := CHR ( 0 ) to CHR ( 255 ) do
       if C in S then
         WRITE ( ORD ( C ) : 1 , ' ' ) ;
     WRITELN
   end (* WRITE_SETC *) ;



begin (* HAUPTPROGRAMM *)
  X := [ 30 , 40 , 50 ] ;
  WRITE_SET ( X ) ;
  WRITE_SETV ( X ) ;
  Y := [ 70 , 120 , 150 ] ;
  WRITE_SET ( Y ) ;
  Z := X + Y ;
  WRITE_SET ( Z ) ;
  Z := Z - [ 150 ] ;
  WRITE_SET ( Z ) ;
  X := Z ;
  WRITE_SET ( X ) ;
  S2 := [ - 70 , 70 ] ;
  S3 := [ - 10 , 80 ] ;
  WRITE_SET2 ( S1 ) ;
  WRITE_SET2 ( S2 ) ;
  WRITE_SET2 ( S3 ) ;
  WRITE_SETC ( SC ) ;
  WRITE_SETC ( SA ) ;
  if FALSE then
    begin
      ZL := [ 1 , 1000 , 10000 , 20000 , 30000 ] ;
      WRITE_SETL ( ZL ) ;
    end (* then *)
end (* HAUPTPROGRAMM *) .
