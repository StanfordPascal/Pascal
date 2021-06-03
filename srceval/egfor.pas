

//*******************************************
// program egfor                             
// compute h(n) = 1 + 1/2 + 1/3 + ... + 1/n  
//*******************************************

program EGFOR ( INPUT , OUTPUT ) ;


var I , N : INTEGER ;
    H : REAL := 0.0 ;


begin (* HAUPTPROGRAMM *)
  READ ( N ) ;
  WRITE ( N ) ;
  for I := N DOWNTO 1 do
    H := H + 1 / I ;
  WRITELN ( H : 20 )
end (* HAUPTPROGRAMM *) .
