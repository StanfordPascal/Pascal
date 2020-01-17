program TESTREAD ( INPFILE , OUTPUT ) ;

//*******************************************
// korrekt nach Modifikation RDS und RDV     
// EOLN wird korrekt signalisiert            
// kein READLN nach EOLN noetig              
// Ergebnis von READ (RDS und RDV)           
// nach EOF nicht mehr verwenden             
//*******************************************



var INPFILE : TEXT ;
    CH10 : CHAR ( 10 ) ;
    S10 : STRING ( 10 ) ;


begin (* HAUPTPROGRAMM *)
  WRITELN ;
  WRITELN ( 'lesen string (10)' ) ;
  WRITELN ;
  RESET ( INPFILE ) ;
  READ ( INPFILE , S10 ) ;
  while not EOF ( INPFILE ) do
    begin
      WRITELN ( 'gelesen: <' , S10 , '> eoln = ' , EOLN ( INPFILE ) ) ;
      READ ( INPFILE , S10 ) ;
    end (* while *) ;
  WRITELN ;
  WRITELN ( 'lesen char (10)' ) ;
  WRITELN ;
  RESET ( INPFILE ) ;
  READ ( INPFILE , CH10 ) ;
  while not EOF ( INPFILE ) do
    begin
      WRITELN ( 'gelesen: <' , CH10 , '> eoln = ' , EOLN ( INPFILE ) )
                ;
      READ ( INPFILE , CH10 ) ;
    end (* while *) ;
  WRITELN ;
  WRITELN ( 'lesen char (10)' ) ;
  WRITELN ;
  RESET ( INPFILE ) ;
  READ ( INPFILE , CH10 ) ;
  while not EOF ( INPFILE ) do
    begin
      WRITELN ( 'gelesen: <' , CH10 , '> eoln = ' , EOLN ( INPFILE ) )
                ;
      READ ( INPFILE , CH10 ) ;
    end (* while *) ;
  WRITELN ;
  WRITELN ( 'lesen string (10)' ) ;
  WRITELN ;
  RESET ( INPFILE ) ;
  READ ( INPFILE , S10 ) ;
  while not EOF ( INPFILE ) do
    begin
      WRITELN ( 'gelesen: <' , S10 , '> eoln = ' , EOLN ( INPFILE ) ) ;
      READ ( INPFILE , S10 ) ;
    end (* while *) ;
end (* HAUPTPROGRAMM *) .
