program TESTSPL ( INFILE , OUTFILE , OUTPUT ) ;


var INFILE : TEXT ;
    OUTFILE : TEXT ;
    ZEILE : STRING ( 5000 ) ;
    ZEILE_ALT : STRING ( 5000 ) ;
    ZNEU : STRING ( 1000 ) ;
    LINENO : INTEGER ;
    LNO1 : CHAR ( 6 ) ;
    LNO2 : STRING ( 6 ) ;
    IX1 , IX2 , L1 , L2 : INTEGER ;
    ANFANG : BOOLEAN ;



procedure INTTOSTR ( CP : VOIDPTR ; LEN : INTEGER ; VAL : INTEGER ;
                   ZEROES : BOOLEAN ) ;

   EXTERNAL ;



begin (* HAUPTPROGRAMM *)
  RESET ( INFILE ) ;
  REWRITE ( OUTFILE ) ;
  LINENO := 1 ;
  ANFANG := TRUE ;
  INTTOSTR ( ADDR ( LNO1 ) , 6 , LINENO , FALSE ) ;
  LNO2 := STR ( LNO1 ) ;
  LNO2 := TRIM ( LNO2 ) ;
  ZEILE_ALT := '' ;
  while not EOF ( INFILE ) do
    begin
      READLN ( INFILE , ZEILE ) ;
      ZEILE := ZEILE_ALT || ZEILE ;
      WRITELN ( 'neue Zeile: ' , ZEILE ) ;
      if ANFANG then
        begin
          IX1 := INDEX ( ZEILE , LNO2 ) ;
          L1 := LENGTH ( LNO2 ) ;
          ANFANG := FALSE ;
        end (* then *) ;
      repeat
        LINENO := LINENO + 1 ;
        INTTOSTR ( ADDR ( LNO1 ) , 6 , LINENO , FALSE ) ;
        LNO2 := STR ( LNO1 ) ;
        LNO2 := TRIM ( LNO2 ) ;
        IX2 := INDEX ( ZEILE , LNO2 ) ;
        L2 := LENGTH ( LNO2 ) ;
        if IX2 <> 0 then
          begin
            ZNEU := SUBSTR ( ZEILE , IX1 + L1 , IX2 - IX1 - L1 ) ;
            WRITELN ( 'gefunden fuer ' , LNO2 , ': ' , ZNEU ) ;
            WRITELN ( OUTFILE , ZNEU ) ;
            ZEILE := SUBSTR ( ZEILE , IX2 ) ;
            IX1 := 1 ;
            L1 := L2 ;
          end (* then *)
        else
          begin
            WRITELN ( 'nichts gefunden fuer ' , LNO2 ) ;
            ZEILE_ALT := ZEILE ;
            IX1 := 1 ;
            LINENO := LINENO - 1 ;
          end (* else *)
      until IX2 = 0 ;
    end (* while *)
end (* HAUPTPROGRAMM *) .
