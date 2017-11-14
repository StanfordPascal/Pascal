program TESTCHK ( OUTPUT ) ;

(********)
(*$A+   *)
(********)



type SUBRG = 10 .. 30 ;


var X : SUBRG ;
    T : array [ SUBRG ] of INTEGER ;


begin (* HAUPTPROGRAMM *)
  X := 10 ;
  X := X * 5 ;
  T [ X ] := 27
end (* HAUPTPROGRAMM *) .
