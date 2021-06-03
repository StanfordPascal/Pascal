program TESTCPY3 ( OUTPUT , INP1 , OUT1 ) ;


var INP1 : TEXT ;
    OUT1 : TEXT ;
    CF : CHAR ( 80 ) ;
    ZEILNR : INTEGER ;
    LAENGE : INTEGER ;
    LMAX : INTEGER ;


begin (* HAUPTPROGRAMM *)
  RESET ( INP1 ) ;
  REWRITE ( OUT1 ) ;
  ZEILNR := 0 ;
  while not EOF ( INP1 ) do
    begin
      READLN ( INP1 , CF ) ;
      ZEILNR := ZEILNR + 1 ;
      LMAX := MAXLENGTH ( CF ) ;
      LAENGE := LENGTH ( CF ) ;
      WRITELN ( 'Zeile ' , ZEILNR , ' MaxL ' , LMAX , ' Laenge ' ,
                LAENGE ) ;
      WRITELN ( OUT1 , CF )
    end (* while *) ;
  CLOSE ( OUT1 ) ;
end (* HAUPTPROGRAMM *) .
