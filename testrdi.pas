program TESTRDI ( INPUT , OUTPUT ) ;

//**********************************************************
// Offene Punkte:
// * wieso falsche Anzahl Parms bei Aufruf $PASRDI ?
// * Testen lt. Beispielen aus Pascal/VS Handbuch
// * erweitern Pascal-Loesung auf FKT. RDH und RDY
// * CHK Anweisungen reinkompilieren nach RDH und RDY
// * Uebertragen nach CMS
// * evtl. Problem mit GET (muss RLN ausloesen)
// * anstelle von EXIT-Funktion Run-Time-Error (CHK?)
// * Run-Time-Error auf Mainframe anders aufbereiten
// * PASSNAP an PASSNAPC angleichen
// * auch Aufbereitung aendern, wenn SNAP ausgeschaltet ist
// * weiter: andere READ-Funktionen
// - Read BOOLEAN
// - Read SKALAR
//**********************************************************
//$X+
//**********************************************************



type BYTE = packed record
                     CONT : 0 .. 100 ;
                   end ;


var I : INTEGER ;
    X : INTEGER ;
    ENDE : BOOLEAN ;
    IFILE : TEXT ;
    J : INTEGER ;
    S : STRING ( 100 ) ;
    CC : CHAR ( 10 ) ;
    Y : BYTE ;
    CH : CHAR ;
    H : 0 .. 20000 ;



function $PASRDI ( var F : TEXT ; WIDTH : INTEGER ) : INTEGER ;

   EXTERNAL ;



begin (* HAUPTPROGRAMM *)
  REWRITE ( IFILE ) ;
  for I := 1 to 100 do
    begin
      WRITE ( IFILE , I * I * I : 8 ) ;
      if I MOD 5 = 0 then
        WRITELN ( IFILE )
    end (* for *) ;
  WRITELN ( 'reading from ifile using read' ) ;
  RESET ( IFILE ) ;
  for X := 1 to 100 do
    begin
      READ ( IFILE , I ) ;
      WRITE ( I : 8 ) ;
      if X MOD 10 = 0 then
        WRITELN ;
    end (* for *) ;
  WRITELN ( 'reading from ifile using new read function' ) ;
  RESET ( IFILE ) ;
  for X := 1 to 100 do
    begin
      I := $PASRDI ( IFILE , - 1 ) ;
      WRITE ( I : 8 ) ;
      if X MOD 10 = 0 then
        WRITELN ;
    end (* for *) ;
  WRITELN ( 'reading from ifile using width = 4' ) ;
  RESET ( IFILE ) ;
  for X := 1 to 200 do
    begin
      I := $PASRDI ( IFILE , 4 ) ;
      WRITE ( I : 8 ) ;
      if X MOD 10 = 0 then
        WRITELN ;
    end (* for *) ;
  WRITELN ( 'reading from ifile using width = 3' ) ;
  RESET ( IFILE ) ;
  for X := 1 to 200 do
    begin
      I := $PASRDI ( IFILE , 3 ) ;
      WRITE ( I : 8 ) ;
      if X MOD 10 = 0 then
        WRITELN ;
    end (* for *) ;

  //**************************************
  // Tests read example
  // from Pascal VS manual
  //**************************************

  WRITELN ( 'tests read example from Pascal/VS manual' ) ;
  REWRITE ( IFILE ) ;
  WRITELN ( IFILE , '36 24 ABCDEFGHIJKLMNOPQRSTUVWXYZ' ) ;
  RESET ( IFILE ) ;
  READLN ( IFILE , I , J , CH , CC , S ) ;
  WRITELN ( 'i          = ' , I ) ;
  WRITELN ( 'j          = ' , J ) ;
  WRITELN ( 'ch         = ' , CH ) ;
  WRITELN ( 'cc         = ' , CC ) ;
  WRITELN ( 's          = ' , S ) ;
  WRITELN ( 'length (s) = ' , LENGTH ( S ) ) ;

  //**************************************
  // Tests formatted read example
  // from Pascal VS manual
  //**************************************

  WRITELN ( 'tests formatted read example from Pascal/VS manual' ) ;
  REWRITE ( IFILE ) ;
  WRITELN ( IFILE , '36 24 ABCDEFGHIJKLMNOPQRSTUVWXYZ' ) ;
  RESET ( IFILE ) ;
  READLN ( IFILE , I : 4 , J : 10 , CH : J , CC , S ) ;
  WRITELN ( 'i          = ' , I ) ;
  WRITELN ( 'j          = ' , J ) ;
  WRITELN ( 'ch         = ' , CH ) ;
  WRITELN ( 'cc         = ' , CC ) ;
  WRITELN ( 's          = ' , S ) ;
  WRITELN ( 'length (s) = ' , LENGTH ( S ) ) ;

  //**************************************
  // Tests formatted read example
  // from Pascal VS manual
  //**************************************

  WRITELN ( 'tests integer reads with subranges (shorter length)' ) ;
  REWRITE ( IFILE ) ;
  WRITELN ( IFILE , '47110971234' ) ;
  RESET ( IFILE ) ;
  READLN ( IFILE , H : 4 , Y . CONT : 3 ) ;
  WRITELN ( 'h          = ' , H ) ;
  WRITELN ( 'y          = ' , Y . CONT ) ;

  //**************************************
  // only to test what the compiler does
  //**************************************

  if FALSE then
    READ ( IFILE , I , CH , I , CH , I ) ;

  //*******************************
  // test input from console file
  //*******************************

  ENDE := FALSE ;
  repeat
    WRITELN ( 'zum testen ints einlesen bis eoln' ) ;
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
