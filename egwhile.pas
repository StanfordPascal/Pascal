

//*******************************************
// program egwhile                           
// compute h(n) = 1 + 1/2 + 1/3 + ... + 1/n  
//*******************************************

program EGWHILE ( INPUT , OUTPUT ) ;


var N : INTEGER ;
    H : REAL := 0.0 ;


begin (* HAUPTPROGRAMM *)
  READ ( N ) ;
  WRITE ( N ) ;
  while N > 0 do
    begin
      H := H + 1 / N ;
      N := N - 1
    end (* while *) ;
  WRITELN ( H : 20 )
end (* HAUPTPROGRAMM *) .
