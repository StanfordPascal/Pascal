program COSINE ( INPUT , OUTPUT ) ;

//*************************************************
// compute the cosine using the expansion:         
// cos(x) = 1 - x**2/(2*1) + x**4/(4*3*2*1) - ...  
//*************************************************



const EPS = 1E-14 ;


var X , SX , S , T : REAL ;
    I , K , N : INTEGER ;


begin (* HAUPTPROGRAMM *)
  READLN ( N ) ;
  for I := 1 to N do
    begin
      READLN ( X ) ;
      T := 1 ;
      K := 0 ;
      S := 1 ;
      SX := SQR ( X ) ;
      while ABS ( T ) > EPS * ABS ( S ) do
        begin
          K := K + 2 ;
          T := - T * SX / ( K * ( K - 1 ) ) ;
          S := S + T
        end (* while *) ;
      WRITELN ( 'input = ' , X : 20 , ' cos = ' , S : 20 , ' iter = ' ,
                K DIV 2 ) ;
    end (* for *)
end (* HAUPTPROGRAMM *) .
