program TESTVS ( EINGABE , AUSGABE ) ;

(********)
(*$A+   *)
(********)



type CHAR8 = array [ 1 .. 8 ] of CHAR ;
     CHAR10 = array [ 1 .. 10 ] of CHAR ;


var EINGABE , AUSGABE : TEXT ;
    ZAHL : INTEGER ;
    SAVETIME : CHAR8 ;
    DATE8 , TIME8 : CHAR8 ;
    DATE10 , TIME10 : CHAR10 ;
    I : INTEGER ;
    F : REAL ;



procedure TEST ( X : REAL ; T : CHAR10 ) ;

   begin (* TEST *)
     WRITELN ( 'test: ' , T , X : 7 : 0 , X : 10 : 5 , X : 15 , X ) ;
   end (* TEST *) ;



begin (* HAUPTPROGRAMM *)
  TERMIN ( EINGABE ) ;
  TERMOUT ( AUSGABE ) ;
  WRITELN ( AUSGABE , 'Bitte Zahl:' ) ;
  READ ( EINGABE , ZAHL ) ;
  WRITELN ( AUSGABE , 'Zahl + 1 = ' , ZAHL + 1 ) ;
  DATETIME ( DATE8 , TIME8 ) ;
  DATTIM10 ( DATE10 , TIME10 ) ;
  WRITELN ( AUSGABE , 'datum8 ...: ' , DATE8 , ' ' , TIME8 ) ;
  WRITELN ( AUSGABE , 'datum10 ..: ' , DATE10 , ' ' , TIME10 ) ;
  SAVETIME := TIME8 ;
  repeat
    DATETIME ( DATE8 , TIME8 ) ;
  until TIME8 <> SAVETIME ;
  DATTIM10 ( DATE10 , TIME10 ) ;
  WRITELN ( AUSGABE , 'datum8 ...: ' , DATE8 , ' ' , TIME8 ) ;
  WRITELN ( AUSGABE , 'datum10 ..: ' , DATE10 , ' ' , TIME10 ) ;
  if TRUE then
    begin
      WRITELN ( AUSGABE , 'close-Aufruf fuer Eingabe folgt' ) ;
      CLOSE ( EINGABE ) ;
      WRITELN ( AUSGABE , 'close-Aufruf fuer Ausgabe folgt' ) ;
      CLOSE ( AUSGABE ) ;
      WRITELN ( OUTPUT , 'Ausgabe ueber File Eingabe ' ) ;
      WRITELN ( OUTPUT , 'muesste nach Close klappen ' ) ;
      WRITELN ( EINGABE , 'Ausgabe ueber File Eingabe ' ) ;
      WRITELN ( EINGABE , 'muesste nach Close klappen ' ) ;
    end (* then *) ;
  TEST ( 5.1 , 'x=5.1' ) ;
  TEST ( 5.0 , 'x=5.0' ) ;
  TEST ( 5 , 'x=5' ) ;
  F := 5.1 ;
  TEST ( F , 'x=f' ) ;
  I := 5 ;
  TEST ( I , 'x=i' ) ;
  HALT
end (* HAUPTPROGRAMM *) .
