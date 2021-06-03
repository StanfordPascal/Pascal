program TESTCPY2 ( OUTPUT , INP1 , OUT2 ) ;


var INP1 : TEXT ;
    OUT2 : TEXT ;
    CV : STRING ( 80 ) ;
    ZEILNR : INTEGER ;
    LAENGE : INTEGER ;
    LMAX : INTEGER ;
    COUT : STRING ( 80 ) ;


begin (* HAUPTPROGRAMM *)
  RESET ( INP1 ) ;
  REWRITE ( OUT2 ) ;
  ZEILNR := 0 ;
  while not EOF ( INP1 ) do
    begin
      READLN ( INP1 , CV ) ;
      ZEILNR := ZEILNR + 1 ;
      LMAX := MAXLENGTH ( CV ) ;
      LAENGE := LENGTH ( CV ) ;
      WRITELN ( 'Zeile ' , ZEILNR , ' MaxL ' , LMAX , ' Laenge ' ,
                LAENGE ) ;
      COUT := RTRIM ( CV ) ;
      WRITELN ( 'Laenge der Ausgabezeile = ' , LENGTH ( COUT ) ) ;
      WRITELN ( OUT2 , COUT )
    end (* while *) ;
  CLOSE ( OUT2 ) ;
end (* HAUPTPROGRAMM *) .
