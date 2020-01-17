program TESTRDI2 ( INPUT , OUTPUT ) ;

//**********************************************************
//$X+,A+
//**********************************************************



var I : INTEGER ;
    ENDE : BOOLEAN ;



procedure TERMIN ( var X : TEXT ) ;

   EXTERNAL ;



procedure TERMOUT ( var X : TEXT ) ;

   EXTERNAL ;



begin (* HAUPTPROGRAMM *)
  TERMIN ( INPUT ) ;
  TERMOUT ( OUTPUT ) ;

  //*******************************
  // test input from console file
  //*******************************

  ENDE := FALSE ;
  repeat
    WRITELN ( 'zum testen ints einlesen bis eoln' ) ;
    ENDE := false ;
    WRITELN ( 'ende durch eingabe von 4711' ) ;
    repeat
      READ ( I ) ;
      WRITELN ( 'gelesen: ' , I ) ;
      if I = 4711 then
        ENDE := TRUE ;
    until EOLN ;
    WRITELN ( 'eoln gefunden' ) ;
  until ENDE ;
end (* HAUPTPROGRAMM *) .
