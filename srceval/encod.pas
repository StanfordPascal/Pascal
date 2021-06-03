program ENCOD ( INPUT , OUTPUT ) ;


var I : INTEGER ;
    CH : CHAR ;
    I2 : INTEGER ;
    NL : BOOLEAN ;


begin (* HAUPTPROGRAMM *)
  NL := FALSE ;
  while not ( EOF ( INPUT ) ) do
    begin
      READ ( INPUT , CH ) ;
      if ( CH >= '0' ) and ( CH <= '9' ) then
        I := ORD ( CH ) - ORD ( '0' )
      else
        if ( CH >= 'A' ) and ( CH <= 'F' ) then
          I := ORD ( CH ) - ORD ( 'A' ) + 10
        else
          if CH = '#' then
            begin
              I := - 1 ;
              NL := TRUE
            end (* then *)
          else
            I := - 1 ;
      if I >= 0 then
        begin
          READ ( INPUT , CH ) ;
          if ( CH >= '0' ) and ( CH <= '9' ) then
            I2 := ORD ( CH ) - ORD ( '0' )
          else
            if ( CH >= 'A' ) and ( CH <= 'F' ) then
              I2 := ORD ( CH ) - ORD ( 'A' ) + 10 ;
          if NL then
            begin
              WRITELN ( OUTPUT ) ;
              NL := FALSE
            end (* then *) ;
          WRITE ( OUTPUT , CHR ( I * 16 + I2 ) ) ;
        end (* then *)
    end (* while *) ;
end (* HAUPTPROGRAMM *) .
