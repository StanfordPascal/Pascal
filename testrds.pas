program TESTRDS ( INPFILE , OUTPUT ) ;


var INPFILE : TEXT ;
    C20 : CHAR ( 20 ) ;


begin (* HAUPTPROGRAMM *)
  RESET ( INPFILE ) ;
  READ ( INPFILE , C20 ) ;
  while not EOF ( INPFILE ) do
    begin
      WRITELN ( 'gelesen: <' , C20 , '> eoln = ' , EOLN ( INPFILE ) ) ;
      READ ( INPFILE , C20 ) ;
      WRITELN ( 'eof = ' , EOF ( INPFILE ) ) ;
    end (* while *) ;
  WRITELN ( 'Dateiende festgestellt' ) ;
end (* HAUPTPROGRAMM *) .
