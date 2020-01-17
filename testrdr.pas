program TESTRDR ( INPUT , OUTPUT ) ;

//**********************************************************
//$X+,A+                                                    
//**********************************************************



var R : REAL ;
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
    WRITELN ( 'zum testen reals einlesen bis eoln' ) ;
    ENDE := FALSE ;
    WRITELN ( 'ende durch eingabe von 4711' ) ;
    repeat
      READ ( R ) ;
      WRITELN ( 'gelesen: ' , R : 20 : 7 ) ;
      if R = 4711 then
        ENDE := TRUE ;
    until EOLN ;
    WRITELN ( 'eoln gefunden' ) ;
  until ENDE ;
  ENDE := FALSE ;
  repeat
    WRITELN ( 'zum testen reals einlesen bis eoln' ) ;
    WRITELN ( 'feste laenge 5' ) ;
    ENDE := FALSE ;
    WRITELN ( 'ende durch eingabe von 4711' ) ;
    repeat
      READ ( R : 5 ) ;
      WRITELN ( 'gelesen: ' , R : 20 : 7 ) ;
      if R = 4711 then
        ENDE := TRUE ;
    until EOLN ;
    WRITELN ( 'eoln gefunden' ) ;
  until ENDE ;
end (* HAUPTPROGRAMM *) .
