

//*******************************************
// program egrepeat                          
// compute h(n) = 1 + 1/2 + 1/3 + ... + 1/n  
//*******************************************

program EGREPEAT ( INPUT , OUTPUT ) ;


var N : INTEGER ;
    H : REAL := 0.0 ;


begin (* HAUPTPROGRAMM *)
  READ ( N ) ;
  WRITE ( N ) ;
  repeat
    H := H + 1 / N ;
    N := N - 1
  until N = 0 ;
  WRITELN ( H : 20 )
end (* HAUPTPROGRAMM *) .
