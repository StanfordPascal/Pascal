program TESTRDV ( INPFILE , OUTPUT ) ;


var INPFILE : TEXT ;
    S10 : STRING ( 10 ) ;


begin (* HAUPTPROGRAMM *)
  RESET ( INPFILE ) ;
  READ ( INPFILE , S10 ) ;
  while not EOF ( INPFILE ) do
    begin
      WRITELN ( 'gelesen: <' , S10 , '> eoln = ' , EOLN ( INPFILE ) ) ;
      READ ( INPFILE , S10 ) ;
    end (* while *)
end (* HAUPTPROGRAMM *) .
