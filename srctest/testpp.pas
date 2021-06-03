program TESTREAD ( INPUTFILE , OUTPUT ) ;

//*******************************************
// korrekt nach Modifikation RDS und RDV     
// EOLN wird korrekt signalisiert            
// kein READLN nach EOLN noetig              
// Ergebnis von READ (RDS und RDV)           
// nach EOF nicht mehr verwenden             
//*******************************************



var INPUTFILE : TEXT ;
    CH10 : CHAR ( 10 ) ;
    S10 : STRING ( 10 ) ;


begin (* HAUPTPROGRAMM *)
  WRITELN ;
  WRITELN ( 'lesen string (10)' ) ;
  WRITELN ;
  RESET ( INPUTFILE ) ;
  READ ( INPUTFILE , S10 ) ;
  while not EOF ( INPUTFILE ) do
    begin
      WRITELN ( 'gelesen: <' , S10 , '> eoln = ' , EOLN ( INPUTFILE ) )
                ;
      READ ( INPUTFILE , S10 ) ;
    end (* while *) ;
  WRITELN ;
  WRITELN ( 'lesen char (10)' ) ;
  WRITELN ;
  RESET ( INPUTFILE ) ;
  READ ( INPUTFILE , CH10 ) ;
  while not EOF ( INPUTFILE ) do
    begin
      WRITELN ( 'gelesen: <' , CH10 , '> eoln = ' , EOLN ( INPUTFILE )
                ) ;
      READ ( INPUTFILE , CH10 ) ;
    end (* while *) ;
  WRITELN ;
  WRITELN ( 'lesen char (10)' ) ;
  WRITELN ;
  RESET ( INPUTFILE ) ;
  READ ( INPUTFILE , CH10 ) ;
  while not EOF ( INPUTFILE ) do
    begin
      WRITELN ( 'gelesen: <' , CH10 , '> eoln = ' , EOLN ( INPUTFILE )
                ) ;
      READ ( INPUTFILE , CH10 ) ;
    end (* while *) ;
  WRITELN ;
  WRITELN ( 'lesen string (10)' ) ;
  WRITELN ;
  RESET ( INPUTFILE ) ;
  READ ( INPUTFILE , S10 ) ;
  while not EOF ( INPUTFILE ) do
    begin
      WRITELN ( 'gelesen: <' , S10 , '> eoln = ' , EOLN ( INPUTFILE ) )
                ;
      READ ( INPUTFILE , S10 ) ;
    end (* while *) ;
end (* HAUPTPROGRAMM *) .
