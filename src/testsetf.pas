program TESTSETF ;

//**************************************************
// better code generation for const parameters      
// check correct code generation for card function  
//**************************************************



type DIAG = set of - 7 .. 7 ;
     SHORT = - 10000 .. 10000 ;
     SET1 = set of SHORT ;
     SET2 = set of CHAR ;
     FARBE = ( ROT , GELB , GRUEN , BLAU ) ;
     SET_FARBE = set of FARBE ;


var Z : DIAG ;
    Z2 : DIAG ;
    VAL : INTEGER := 6 ;
    X : SET2 ;
    Y : SET1 ;
    T1 : CHAR ;
    T2 : SHORT ;
    SF : SET_FARBE := ALL ( SET_FARBE ) ;
    F : FARBE ;



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
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test set after setcall by value' ) ;
  Z2 := [ 1 , 3 , 4 ] ;
  WRITELN ( 'should show 1, 3, 4 ' ) ;
  WRITE_SET ( Z2 , ADDR ( Z2 ) , FALSE ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  if FALSE then
    begin
      WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
      WRITELN ( 'wrong inclusion of element to set' ) ;
      Z2 := Z2 + 7 ;
      WRITELN ( 'should show 1, 3, 4, 7' ) ;
      WRITE_SET ( Z2 , ADDR ( Z2 ) , FALSE ) ;
      WRITELN ( '--------------------------------------------------' )
                ;
    end (* then *) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'correct inclusion of element to set' ) ;
  Z2 := Z2 + [ 7 ] ;
  WRITELN ( 'should show 1, 3, 4, 7' ) ;
  WRITE_SET ( Z2 , ADDR ( Z2 ) , FALSE ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  if FALSE then
    begin
      WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
      WRITELN ( 'wrong inclusion of element to set' ) ;
      Z2 := Z2 + VAL ;
      WRITELN ( 'should show 1, 3, 4, 6, 7' ) ;
      WRITE_SET ( Z2 , ADDR ( Z2 ) , FALSE ) ;
      WRITELN ( '--------------------------------------------------' )
                ;
    end (* then *) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'correct inclusion of element to set' ) ;
  Z2 := Z2 + [ VAL ] ;
  WRITELN ( 'should show 1, 3, 4, 6, 7' ) ;
  WRITE_SET ( Z2 , ADDR ( Z2 ) , FALSE ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test all expression suggested by N. Wirth' ) ;
  Z2 := ALL ( DIAG ) ;
  WRITELN ( 'should show all diag values (-7 .. 7)' ) ;
  WRITE_SET ( Z2 , ADDR ( Z2 ) , FALSE ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  if FALSE then
    begin
      WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
      WRITELN ( 'test all expression - error' ) ;
      Z2 := ALL ( 5 ) ;
      WRITELN ( 'should show all diag values (-7 .. 7)' ) ;
      WRITE_SET ( Z2 , ADDR ( Z2 ) , FALSE ) ;
      WRITELN ( '--------------------------------------------------' )
                ;
      WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
      WRITELN ( 'test all expression - error' ) ;
      Z2 := ALL ( INTEGER ) ;
      WRITELN ( 'should show all diag values (-7 .. 7)' ) ;
      WRITE_SET ( Z2 , ADDR ( Z2 ) , FALSE ) ;
      WRITELN ( '--------------------------------------------------' )
                ;
    end (* then *) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test all expression' ) ;
  X := ALL ( SET2 ) ;
  WRITELN ( 'x should now contain all characters' ) ;
  for T1 := CHR ( 0 ) to CHR ( 255 ) do
    if not ( T1 in X ) then
      WRITELN ( 'not found: ' , ORD ( T1 ) ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test all expression' ) ;
  Y := ALL ( SET1 ) ;
  WRITELN ( 'y should now contain all shorts from -10000 to +10000' ) ;
  for T2 := - 10000 to 10000 do
    if not ( T2 in Y ) then
      WRITELN ( 'not found: ' , T2 ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test all expression with scalar type' ) ;
  WRITELN ( 'should show 4 colors' ) ;
  SF := ALL ( SET_FARBE ) ;
  for F := ROT to BLAU do
    if F in SF then
      WRITE ( F , ' ' ) ;
  WRITELN ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test new include procedure' ) ;
  WRITELN ( 'should show 3' ) ;
  Z := [ ] ;
  INCLUDE ( Z , 3 ) ;
  WRITE_SET ( Z , ADDR ( Z ) , FALSE ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test new include procedure' ) ;
  WRITELN ( 'should show 1 and 3' ) ;
  INCLUDE ( Z , 1 ) ;
  WRITE_SET ( Z , ADDR ( Z ) , FALSE ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test new include procedure' ) ;
  WRITELN ( 'should show 1, 3 and 4' ) ;
  INCLUDE ( Z , 4 ) ;
  WRITE_SET ( Z , ADDR ( Z ) , FALSE ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test new exclude procedure' ) ;
  WRITELN ( 'should show 1 and 4' ) ;
  EXCLUDE ( Z , 3 ) ;
  WRITE_SET ( Z , ADDR ( Z ) , FALSE ) ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test new include procedure' ) ;
  WRITELN ( 'should show gelb' ) ;
  SF := [ ] ;
  INCLUDE ( SF , GELB ) ;
  for F := ROT to BLAU do
    if F in SF then
      WRITE ( F , ' ' ) ;
  WRITELN ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test new include procedure' ) ;
  WRITELN ( 'should show gelb and blau' ) ;
  INCLUDE ( SF , BLAU ) ;
  for F := ROT to BLAU do
    if F in SF then
      WRITE ( F , ' ' ) ;
  WRITELN ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test new exclude procedure' ) ;
  WRITELN ( 'should show blau' ) ;
  EXCLUDE ( SF , GELB ) ;
  for F := ROT to BLAU do
    if F in SF then
      WRITE ( F , ' ' ) ;
  WRITELN ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test new for expression' ) ;
  WRITELN ( 'should show -2, 0, 1, 3, 5' ) ;
  Z := [ - 2 , 0 , 1 , 3 , 5 ] ;
  for VAL in Z do
    WRITE ( VAL : 1 , ' ' ) ;
  WRITELN ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test new for expression' ) ;
  WRITELN ( 'should show nothing (empty set)' ) ;
  Z := [ ] ;
  for VAL in Z do
    WRITE ( VAL : 1 , ' ' ) ;
  WRITELN ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test new for expression' ) ;
  WRITELN ( 'should show 1, 4, 6' ) ;
  INCLUDE ( Z , 1 ) ;
  INCLUDE ( Z , 4 ) ;
  INCLUDE ( Z , 6 ) ;
  for VAL in Z do
    WRITE ( VAL : 1 , ' ' ) ;
  WRITELN ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test new for expression' ) ;
  WRITELN ( 'should show blau' ) ;
  EXCLUDE ( SF , GELB ) ;
  for F in SF do
    WRITE ( F , ' ' ) ;
  WRITELN ;
  WRITELN ( '--------------------------------------------------' ) ;
  WRITE ( 'Zeile ' , SRCLINE : 1 , ': ' ) ;
  WRITELN ( 'test new for expression' ) ;
  WRITELN ( 'should show 4 colors' ) ;
  SF := ALL ( SET_FARBE ) ;
  for F in SF do
    WRITE ( F , ' ' ) ;
  WRITELN ;
  WRITELN ( '--------------------------------------------------' ) ;
end (* HAUPTPROGRAMM *) .
