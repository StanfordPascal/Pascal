program DECOD ( INPUT , OUTPUT ) ;


var I : INTEGER ;
    C1 : CHAR ;
    C2 : CHAR ;
    CH : CHAR ;
    HEX : packed array [ 0 .. 15 ] of CHAR ;


begin (* HAUPTPROGRAMM *)
  RESET ( INPUT ) ;
  HEX := '0123456789ABCDEF' ;
  I := 0 ;
  while not ( EOF ( INPUT ) ) do
    begin
      READ ( INPUT , CH ) ;
      C1 := HEX [ ORD ( CH ) DIV 16 ] ;
      C2 := HEX [ ORD ( CH ) MOD 16 ] ;
      WRITE ( OUTPUT , C1 , C2 ) ;
      I := I + 2 ;
      if ( EOLN ( INPUT ) ) then
        begin
          WRITELN ( OUTPUT , '#' ) ;
          I := 0 ;
          READ ( INPUT , CH ) ;

  (******************************)
  (* BLANK NACH EOLN UEBERLESEN *)
  (******************************)

        end (* then *) ;
      if I >= 60 then
        begin
          WRITELN ( OUTPUT ) ;
          I := 0 ;
        end (* then *) ;
    end (* while *) ;
end (* HAUPTPROGRAMM *) .
