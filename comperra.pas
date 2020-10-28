program CHECKPPV ( INPUT , OUTPUT ) ;


var SUMTAGE : INTEGER ;
    SUMRBTR : DECIMAL ( 11 , 2 ) ;
    SUMVBTR : DECIMAL ( 11 , 2 ) ;
    SUMIRBTR : INTEGER ;
    SUMIVBTR : INTEGER ;
    SUMTAGE09 : INTEGER ;
    SUMRBTR09 : DECIMAL ( 11 , 2 ) ;
    SUMVBTR09 : DECIMAL ( 11 , 2 ) ;
    ZEILE : STRING ( 100 ) ;
    TAGE : INTEGER ;
    RBTR : DECIMAL ( 11 , 2 ) ;
    VBTR : DECIMAL ( 11 , 2 ) ;
    IRBTR : INTEGER ;
    VRBTR : INTEGER ;  // should be IVBTR
    STAGE : STRING ( 4 ) ;
    SRBTR : STRING ( 11 ) ;
    SVBTR : STRING ( 11 ) ;
    LEIART : STRING ( 3 ) ;
    xerr : array (.1..14.) of char;


begin (* HAUPTPROGRAMM *)
  SUMTAGE := 0 ;
  SUMRBTR := 0.0 ;
  SUMVBTR := 0.0 ;
  SUMIRBTR := 0 ;
  SUMIVBTR := 0 ;
  SUMTAGE09 := 0 ;
  SUMRBTR09 := 0.0 ;
  SUMVBTR09 := 0.0 ;
  repeat
    READLN ( ZEILE ) ;
    STAGE := SUBSTR ( ZEILE , 31 , 4 ) ;
    SRBTR := SUBSTR ( ZEILE , 42 , 11 ) ;
    SVBTR := SUBSTR ( ZEILE , 53 , 11 ) ;
    LEIART := SUBSTR ( ZEILE , 39 , 3 ) ;
    READSTR ( STAGE , TAGE ) ;
    SRBTR [ 9 ] := '.' ;
    SVBTR [ 9 ] := '.' ;
    READSTR ( SRBTR , RBTR ) ;
    READSTR ( SVBTR , VBTR ) ;
    SRBTR := DELETE ( SRBTR , 9 , 1 ) ;
    SVBTR := DELETE ( SVBTR , 9 , 1 ) ;
    READSTR ( SRBTR , IRBTR ) ;
    READSTR ( SVBTR , IVBTR ) ;
    if FALSE then
      begin
        WRITELN ( ZEILE ) ;
        WRITELN ( TAGE , RBTR , VBTR )
      end (* then *) ;
    SUMTAGE := SUMTAGE + TAGE ;
    SUMRBTR := SUMRBTR + RBTR ;
    SUMVBTR := SUMVBTR + VBTR ;
    SUMIRBTR := SUMIRBTR + IRBTR ;
    SUMIVBTR := SUMIVBTR + IVBTR ;
    if LEFT ( LEIART , 1 ) = '0' then
      begin
        SUMTAGE09 := SUMTAGE09 + TAGE ;
        SUMRBTR09 := SUMRBTR09 + RBTR ;
        SUMVBTR09 := SUMVBTR09 + VBTR ;
      end (* then *) ;
  until EOF ( INPUT ) ;
  WRITELN ( 'TAGE    = ' , SUMTAGE ) ;
  WRITELN ( 'RBTR    = ' , SUMRBTR ) ;
  WRITELN ( 'VBTR    = ' , SUMVBTR ) ;
  WRITELN ( 'RBTR.I  = ' , SUMIRBTR ) ;
  WRITELN ( 'VBTR.I  = ' , SUMIVBTR ) ;
  WRITELN ( 'TAGE.09 = ' , SUMTAGE09 ) ;
  WRITELN ( 'RBTR.09 = ' , SUMRBTR09 ) ;
  WRITELN ( 'VBTR.09 = ' , SUMVBTR09 ) ;
end (* HAUPTPROGRAMM *) .
