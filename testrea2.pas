program TESTREA2 ( INPUT , OUTPUT ) ;


var E1 : CHAR ( 10 ) ;
    V1 : STRING ( 10 ) ;


begin (* HAUPTPROGRAMM *)
  TERMIN ( INPUT ) ;
  WRITELN ( 'Test Eingabe feste CHARs (bis ENDE)' ) ;
  repeat
    READ ( E1 ) ;
    WRITELN ( 'Eingabe = <' , E1 , '> eoln = ' , EOLN ) ;
    if EOLN then
      READLN
  until E1 = 'ENDE' ;
  WRITELN ( 'Test Eingabe var. CHARs (bis ENDE)' ) ;
  repeat
    READ ( V1 ) ;
    WRITELN ( 'Eingabe = <' , V1 , '> eoln = ' , EOLN ) ;
    if EOLN then
      READLN
  until V1 = 'ENDE' ;
end (* HAUPTPROGRAMM *) .
