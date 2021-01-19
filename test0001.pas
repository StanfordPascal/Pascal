program CONVERT ( OUTPUT ) ;


const ADDIN = 32 ;
      MULBY = 1.8 ;
      LOW = 0 ;
      HIGH = 39 ;
      SEPARATOR = '--------------' ;


var DEGREE : LOW .. HIGH ;


begin (* HAUPTPROGRAMM *)
  WRITELN ( SEPARATOR ) ;
  for DEGREE := LOW to HIGH do
    begin
      WRITE ( DEGREE , 'c' , ROUND ( DEGREE * MULBY + ADDIN ) , 'f' ) ;
      if ODD ( DEGREE ) then
        WRITELN
    end (* for *) ;
  WRITELN ( SEPARATOR ) ;
end (* HAUPTPROGRAMM *) .
