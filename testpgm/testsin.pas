program TESTSIN ( OUTPUT ) ;

(********)
(*$A+   *)
(********)



var X : REAL ;


begin (* HAUPTPROGRAMM *)
  X := SIN ( 3.141592654 / 2 ) ;
  X := 1.5 ;
  X := SIN ( X ) ;
  WRITELN ( 'SIN (1.5) = ' , X : 15 : 7 ) ;
  WRITELN ( 'SIN (1.0) = ' , SIN ( 1.0 ) : 15 : 7 ) ;
  WRITELN ( 'SIN (pi/2) = ' , SIN ( 3.141592654 / 2 ) : 15 : 7 ) ;
  X := 0 ;
  while X <= 5.0 do
    begin
      WRITELN ( 'SIN (' , X : 1 : 1 , ') = ' , SIN ( X ) : 15 : 7 ) ;
      X := ROUNDX ( X + 0.2 , - 1 ) ;
    end (* while *)
end (* HAUPTPROGRAMM *) .
