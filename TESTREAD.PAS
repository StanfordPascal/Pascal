program TESTREAD ( INPFILE , OUTPUT ) ;


var INPFILE : TEXT ;
    CH : CHAR ;
    CH10 : CHAR ( 10 ) ;
    S10 : STRING ( 10 ) ;


begin (* HAUPTPROGRAMM *)
  WRITELN ;
  WRITELN ( 'lesen einzelne char' ) ;
  WRITELN ;
  RESET ( INPFILE ) ;
  while not EOF ( INPFILE ) do
    begin
      READ ( INPFILE , CH ) ;
      WRITELN ( 'gelesen: ' , CH , ORD ( CH ) : 4 , ' eof = ' , EOF (
                INPFILE ) , ' eoln = ' , EOLN ( INPFILE ) ) ;
    end (* while *) ;
  WRITELN ;
  WRITELN ( 'lesen char (10)' ) ;
  WRITELN ;
  RESET ( INPFILE ) ;
  while not EOF ( INPFILE ) do
    begin
      READ ( INPFILE , CH10 ) ;
      WRITELN ( 'gelesen: <' , CH10 , '> eoln = ' , EOLN ( INPFILE ) )
                ;
      if EOLN ( INPFILE ) then
        READLN ( INPFILE ) ;
    end (* while *) ;
  WRITELN ;
  WRITELN ( 'lesen string (10)' ) ;
  WRITELN ;
  RESET ( INPFILE ) ;
  while not EOF ( INPFILE ) do
    begin
      READ ( INPFILE , S10 ) ;
      WRITELN ( 'gelesen: <' , S10 , '> eoln = ' , EOLN ( INPFILE ) ) ;
      if EOLN ( INPFILE ) then
        READLN ( INPFILE ) ;
    end (* while *)
end (* HAUPTPROGRAMM *) .
