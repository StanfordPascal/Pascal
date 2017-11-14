program TESTRDS ( INPUT , OUTPUT ) ;


var ANZ : INTEGER ;
    ZEILE : array [ 1 .. 120 ] of CHAR ;
    CATCH : array [ 1 .. 40 ] of CHAR ;
    I : INTEGER ;


begin (* HAUPTPROGRAMM *)
  RESET ( INPUT ) ;
  ANZ := 0 ;
  CATCH := 'A123456789B123456789C123456789D123456789' ;
  while ( not EOF ( INPUT ) ) and ( ANZ < 2000 ) do
    begin
      READLN ( INPUT , ZEILE ) ;
      WRITE ( ' ' , 'Zeile = ' ) ;
      for I := 1 to 60 do
        WRITE ( ZEILE [ I ) ) ;
      WRITELN ;
      WRITELN ( ' ' , ORD ( ZEILE [ 120 ] ) , ' --- should be 64 ' ) ;
      WRITELN ( ' ' , 'Anz = ' , ANZ , ' catch = ' , CATCH ) ;
      ANZ := ANZ + 1 ;
    end (* while *)
end (* HAUPTPROGRAMM *) .
