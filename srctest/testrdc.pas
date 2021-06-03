program TESTRDC ( INPFILE , OUTPUT ) ;

//*******************************************
// Testen RDC                                
//$A+                                        
//*******************************************



var INPFILE : TEXT ;
    CH : CHAR ;
    COLS : INTEGER ;


begin (* HAUPTPROGRAMM *)
  WRITELN ;
  WRITELN ( 'lesen mit dauernd read' ) ;
  WRITELN ;
  RESET ( INPFILE ) ;
  READ ( INPFILE , CH ) ;
  while not EOF ( INPFILE ) do
    begin
      WRITELN ( 'gelesen: ' , CH , ORD ( CH ) : 4 , ' file char: ' ,
                INPFILE -> , ORD ( INPFILE -> ) : 4 , ' eof = ' , EOF (
                INPFILE ) , ' eoln = ' , EOLN ( INPFILE ) ) ;
      READ ( INPFILE , CH ) ;
    end (* while *) ;
  WRITELN ;
  WRITELN ( 'lesen mit read und readln' ) ;
  WRITELN ;
  RESET ( INPFILE ) ;
  while TRUE do
    begin
      if EOF ( INPFILE ) then
        break ;
      if EOLN ( INPFILE ) then
        begin
          READLN ( INPFILE ) ;
          WRITELN ( '--------------------------------------------' ) ;
          continue ;
        end (* then *) ;
      READ ( INPFILE , CH ) ;
      WRITELN ( 'gelesen: ' , CH , ORD ( CH ) : 4 , ' eof = ' , EOF (
                INPFILE ) , ' eoln = ' , EOLN ( INPFILE ) ) ;
    end (* while *) ;
  WRITELN ;
  WRITELN ( 'lesen mit read und readln - max 10 cols' ) ;
  WRITELN ;
  RESET ( INPFILE ) ;
  COLS := 0 ;
  while TRUE do
    begin
      if EOF ( INPFILE ) then
        break ;
      if EOLN ( INPFILE ) or ( COLS >= 10 ) then
        begin
          READLN ( INPFILE ) ;
          WRITELN ( '--------------------------------------------' ) ;
          COLS := 0 ;
          continue
        end (* then *) ;
      READ ( INPFILE , CH ) ;
      WRITELN ( 'gelesen: ' , CH , ORD ( CH ) : 4 , ' eof = ' , EOF (
                INPFILE ) , ' eoln = ' , EOLN ( INPFILE ) ) ;
      COLS := COLS + 1 ;
    end (* while *) ;
end (* HAUPTPROGRAMM *) .
