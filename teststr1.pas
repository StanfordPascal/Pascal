program TESTSTR1 ( OUTPUT ) ;

//**********************************************************************
//$A+
//**********************************************************************



var S : STRING ( 20 ) ;
    C : CHAR ( 20 ) ;
    I : INTEGER ;
    C1 : CHAR ;
    C2 : CHAR ;
    C20 : CHAR ( 20 ) ;
    C20A : CHAR ( 20 ) ;
    VC20 : STRING ( 20 ) ;
    VC200 : STRING ( 200 ) ;


begin (* HAUPTPROGRAMM *)
  C := 'Bernd' ;
  WRITELN ( 'c         = ' , C ) ;
  S := 'Oppolzer' ;
  WRITELN ( 'maxlength = ' , MAXLENGTH ( S ) ) ;
  WRITELN ( 'length    = ' , LENGTH ( S ) ) ;
  C := S ;
  WRITELN ( 'c         = ' , C ) ;
  for I := 1 to LENGTH ( S ) do
    WRITE ( S [ I ] , ' ' ) ;
  WRITELN ;

  //******************************************************************
  // tests aus testvarc
  //******************************************************************

  C1 := 'A' ;
  C2 := C1 ;
  C20 := 'Test' ;
  VC20 := STR ( 'A' ) ;
  WRITE ( 'VC20 nach Zuweisung STR (''A'') ........: ' ) ;
  WRITELN ( '<' , VC20 , '>' ) ;
  VC20 := STR ( C1 ) ;
  WRITE ( 'VC20 nach Zuweisung STR (C1) .........: ' ) ;
  WRITELN ( '<' , VC20 , '>' ) ;
  VC20 := 'A' ;
  WRITE ( 'VC20 nach Zuweisung ''A'' ..............: ' ) ;
  WRITELN ( '<' , VC20 , '>' ) ;
  VC20 := 'Test Varchar' ;
  WRITE ( 'VC20 nach Zuweisung const char array .: ' ) ;
  WRITELN ( '<' , VC20 , '>' ) ;

  //******************************************************************
  // ist nicht zulaessig, muss Konstante sein oder Funktion STR
  //******************************************************************
  // VC20 := C20 ;  /* sollte Fehler sein */
  //******************************************************************
  // so korrekt
  //******************************************************************

  VC20 := STR ( C20 ) ;
  WRITE ( 'VC20 nach Zuweisung STR (C20) ........: ' ) ;
  WRITELN ( '<' , VC20 , '>' ) ;

  //******************************************************************
  // muss erlaubt sein, mit Blanks auffuellen wie gehabt ...
  //******************************************************************

  VC20 := 'Bernd' ;
  WRITE ( 'VC20 nach Zuweisung Bernd ............: ' ) ;
  WRITELN ( '<' , VC20 , '>' ) ;
  C20 := VC20 ;
  WRITELN ( 'C20 nach Zuweisung VC20 ..............: ' , C20 ) ;
  C20 := 'Bernd ' || 'Oppolzer' ;
  WRITELN ( 'C20 nach Zuweisung String Expression .: ' , C20 ) ;

  //******************************************************************
  // Verkettung usw.
  //******************************************************************

  VC200 := VC20 || VC20 ;
  WRITE ( 'VC200 nach Verkettung ................: ' ) ;
  WRITELN ( '<' , VC200 , '>' ) ;
  VC200 := VC20 || ' Oppolzer' ;
  WRITE ( 'VC200 nach Verkettung ................: ' ) ;
  WRITELN ( '<' , VC200 , '>' ) ;
  VC200 := VC20 || ' Oppolzer' || ' Leinfelden' ;
  WRITE ( 'VC200 nach Verkettung ................: ' ) ;
  WRITELN ( '<' , VC200 , '>' ) ;
  VC200 := VC20 || ( ' Oppolzer' || ' Leinfelden' ) ;
  WRITE ( 'VC200 nach Verkettung (Klammer hinten): ' ) ;
  WRITELN ( '<' , VC200 , '>' ) ;
  VC200 := ( VC20 || ' Oppolzer' ) || ' Leinfelden' ;
  WRITE ( 'VC200 nach Verkettung ................: ' ) ;
  WRITELN ( '<' , VC200 , '>' ) ;
  VC200 := ( ( VC20 || ' Oppolzer' ) || ' Leinfelden' ) ;
  WRITE ( 'VC200 nach Verkettung ................: ' ) ;
  WRITELN ( '<' , VC200 , '>' ) ;
  VC200 := VC20 || STR ( ' Oppolzer' ) || STR ( ' Leinfelden' ) ;
  WRITE ( 'VC200 nach Verkettung (mit STR Funk.).: ' ) ;
  WRITELN ( '<' , VC200 , '>' ) ;
  VC200 := VC20 || ' ' || VC20 ;
  WRITE ( 'VC200 nach Verkettung (mit Blank) ....: ' ) ;
  WRITELN ( '<' , VC200 , '>' ) ;
  VC200 := VC20 || ' dazwischen ' || VC20 ;
  WRITE ( 'VC200 nach Verkettung ................: ' ) ;
  WRITELN ( '<' , VC200 , '>' ) ;
  VC200 := VC20 || ' dazwischen ' || STR ( C20 ) ;
  WRITE ( 'VC200 nach Verkettung ................: ' ) ;
  WRITELN ( '<' , VC200 , '>' ) ;
  VC200 := STR ( C20 ) || ' dazwischen ' || VC20 ;
  WRITE ( 'VC200 nach Verkettung ................: ' ) ;
  WRITELN ( '<' , VC200 , '>' ) ;

  //******************************************************************
  // Zuweisungen einfach oder Expression
  //******************************************************************

  VC20 := 'Test-String' ;
  WRITE ( 'VC20 nach Zuweisung Konstante ........: ' ) ;
  WRITELN ( '<' , VC20 , '>' ) ;
  VC200 := VC20 ;
  WRITE ( 'VC200 nach Zuweisung VC20 ............: ' ) ;
  WRITELN ( '<' , VC200 , '>' ) ;
  VC200 := VC20 || ' mit Verkettung' ;
  WRITE ( 'VC200 nach Zuweisung Expression ......: ' ) ;
  WRITELN ( '<' , VC200 , '>' ) ;
  VC20 := 'Bernd' ;
end (* HAUPTPROGRAMM *) .
