program SUMMING ( OUTPUT ) ;

//********************************************************
// compute 1 - 1/2 + 1/3 - 1/4 ... + 1/9999 - 1/10000     
// 4 ways                                                 
// 1) left to right, in succession                        
// 2) left to right, all pos and neg terms, then subtract 
// 2) right to left, in succession                        
// 2) right to left, all pos and neg terms, then subtract 
//********************************************************



const LIMIT = 10000 ;


var S1 , S2P , S2N , S3 , S4P , S4N : REAL := 0 ;
    LRP , LRN , RLP , RLN : REAL ;
    I : INTEGER ;


begin (* HAUPTPROGRAMM *)
  for I := 1 to LIMIT DIV 2 do
    begin
      LRP := 1 / ( 2 * I - 1 ) ;               // left to right, pos
      LRN := 1 / ( 2 * I ) ;                   // left to right, neg
      RLP := 1 / ( ( LIMIT + 1 ) - 2 * I ) ;   // right to left, pos
      RLN := 1 / ( ( LIMIT + 2 ) - 2 * I ) ;   // right to left, neg
      S1 := S1 + LRP - LRN ;
      S2P := S2P + LRP ;
      S2N := S2N + LRN ;
      S3 := S3 + RLP - RLN ;
      S4P := S4P + RLP ;
      S4N := S4N + RLN ;
    end (* for *) ;
  WRITELN ( S1 : 20 , S2P - S2N : 20 ) ;
  WRITELN ( S3 : 20 , S4P - S4N : 20 ) ;
end (* HAUPTPROGRAMM *) .
