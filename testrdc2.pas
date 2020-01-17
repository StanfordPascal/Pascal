program TESTRDC2 ( INPFILE , OUTPUT ) ;

//*******************************************
// Testen RDC
//$A+
//*******************************************



var INPFILE : TEXT ;
    CH : CHAR ;
    COLS : INTEGER ;
    DONE : BOOLEAN ;
    C10 : CHAR ( 10 ) ;
    S10 : STRING ( 10 ) ;


begin (* HAUPTPROGRAMM *)
  WRITELN ( 'input lines until line contains $' ) ;
  TERMIN ( INPFILE ) ;
  DONE := FALSE ;
  repeat
    for COLS := 1 to 10 do
      begin
        READ ( INPFILE , CH ) ;
        WRITELN ( 'found: ' , CH , ORD ( CH ) : 4 , ' eof = ' , EOF (
                  INPFILE ) , ' eoln = ' , EOLN ( INPFILE ) ) ;
        if CH = '$' then
          DONE := TRUE
      end (* for *) ;
    READLN ( INPFILE ) ;
  until DONE ;
  WRITELN ( 'input fixed char strings until line contains $' ) ;
  DONE := FALSE ;
  repeat
    READLN ( INPFILE , C10 ) ;
    WRITELN ( 'found: ' , LENGTH ( C10 ) : 4 , ' <' , C10 , '>' ) ;
    DONE := INDEX ( C10 , '$' ) <> 0 ;
  until DONE ;
  WRITELN ( 'input strings until line contains $' ) ;
  DONE := FALSE ;
  repeat
    READLN ( INPFILE , S10 ) ;
    WRITELN ( 'found: ' , LENGTH ( S10 ) : 4 , ' <' , S10 , '>' ) ;
    DONE := INDEX ( S10 , '$' ) <> 0 ;
  until DONE ;
end (* HAUPTPROGRAMM *) .
