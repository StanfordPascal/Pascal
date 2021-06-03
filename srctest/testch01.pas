program TESTCH01 ( INPUT , OUTPUT ) ;

//$A+

var IX : INTEGER ;



function IX_INT ( X : CHAR ; Y : CHAR ) : INTEGER ;

//**************************************************
// ermittle Index fuer internes Feld aus externem
// ab Position 26 (3. zeile) werden die Felder
// der 1. Zeile (a1, b1 bis h1) abgelegt.
// ..xxxxxxxx....yyyyyyyy....zzzzzzzz....
// das waeren also demnach die 1., 2. und 3. Zeile
// ab interner Position 24
//**************************************************


   var I1 , I2 : INTEGER ;

   begin (* IX_INT *)
     I1 := ( ORD ( Y ) - ORD ( '1' ) ) * 12 ;
     I2 := ORD ( X ) - ORD ( 'a' ) ;
     IX_INT := I1 + I2 + 26 ;
   end (* IX_INT *) ;



function IX_EXT ( X : INTEGER ) : STRING ;

//*****************************************************
// Rueckgabe eines Strings aus zwei Buchstaben
// z.B. a1 - oder Leerstring, wenn kein "echtes" Feld
//*****************************************************


   const S1 = 'abcdefgh' ;
         S2 = '12345678' ;

   var S : STRING ( 2 ) ;
       I1 , I2 : INTEGER ;

   begin (* IX_EXT *)
     S := '' ;
     if X < 26 then
       begin
         IX_EXT := S ;
         return
       end (* then *) ;
     I1 := X MOD 12 - 1 ;
     I2 := X DIV 12 - 1 ;
     if ( I1 < 1 ) or ( I1 > 8 ) or ( I2 < 1 ) or ( I2 > 8 ) then
       return ;
     S := S1 [ I1 ] || S2 [ I2 ] ;
     IX_EXT := S ;
   end (* IX_EXT *) ;



begin (* HAUPTPROGRAMM *)
  TERMIN ( INPUT ) ;
  TERMOUT ( OUTPUT ) ;
  WRITELN ( 'bitte position eingeben (a1 = 26, ende mit 0):' ) ;
  repeat
    READLN ( IX ) ;
    WRITELN ( IX , ' ergibt position ' , IX_EXT ( IX ) ) ;
  until IX = 0
end (* HAUPTPROGRAMM *) .
