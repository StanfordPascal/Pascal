program CRASH2 ( OUTPUT ) ;


type CHARX = array [ 1.. CMAX ] of CHAR ;


var C : CHAR ;


begin (* HAUPTPROGRAMM *)
  C := 'X' ;
  if C = 'X' then
    C := 'A' ;
  else
    C := 'B' ;
end (* HAUPTPROGRAMM *) .
