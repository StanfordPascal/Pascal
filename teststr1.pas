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
  if TRUE then
    begin
      C1 := 'A' ;
      C2 := C1 ;
      C20 := 'Test' ;
      VC20 := STR ( 'A' ) ;
      WRITE ( 'VC20 nach Zuweisung STR (''A'') ........: ' ) ;
      WRITELN ( '<' , VC20 , '>' ) ;
      VC20 := 'Bernd Oppolzer' ;
      WRITE ( 'VC20 nach Zuweisung mit L„nge 2 ......: ' ) ;
      WRITELN ( '<' , VC20 : 2 , '>' ) ;
      WRITE ( 'VC20 nach Zuweisung ohne L„nge .......: ' ) ;
      WRITELN ( '<' , VC20 , '>' ) ;
      WRITE ( 'VC20 nach Zuweisung mit L„nge des Str.: ' ) ;
      WRITELN ( '<' , VC20 : LENGTH ( VC20 ) , '>' ) ;

  /******************************************************************/
  /* ---                                                            */
  /*                                                                */
  /*      VC20 := STR ( C1 ) ;                                      */
  /*      WRITE ( 'VC20 nach Zuweisung STR (C1) .........: ' ) ;    */
  /*      WRITELN ( '<' , VC20 , '>' ) ;                            */
  /*      VC20 := 'A' ;                                             */
  /*      WRITE ( 'VC20 nach Zuweisung ''A'' ..............: ' ) ;  */
  /*      WRITELN ( '<' , VC20 , '>' ) ;                            */
  /*      VC20 := 'Test Varchar' ;                                  */
  /*      WRITE ( 'VC20 nach Zuweisung const char array .: ' ) ;    */
  /*      WRITELN ( '<' , VC20 , '>' ) ;                            */
  /*      VC20 := STR ( C20 ) ;                                     */
  /*      WRITE ( 'VC20 nach Zuweisung STR (C20) ........: ' ) ;    */
  /*      WRITELN ( '<' , VC20 , '>' ) ;                            */
  /*      VC20 := 'Bernd' ;                                         */
  /*      WRITE ( 'VC20 nach Zuweisung Bernd ............: ' ) ;    */
  /*      WRITELN ( '<' , VC20 , '>' ) ;                            */
  /*      C20 := VC20 ;                                             */
  /*      WRITELN ( 'C20 nach Zuweisung VC20 ..............: ' , C2 */
  /*0 ) ;                                                           */
  /*      C20 := 'Bernd ' || 'Oppolzer' ;                           */
  /*      WRITELN ( 'C20 nach Zuweisung String Expression .: ' , C2 */
  /*0 ) ;                                                           */
  /*                                                                */
  /*      ---                                                       */
  /******************************************************************/

    end (* then *) ;
end (* HAUPTPROGRAMM *) .
