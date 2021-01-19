program GRAPH1 ( OUTPUT ) ;

//***************************************
// graphic representation of a function  
// f(x) = exp(-x) * sin(2*pi*x)          
//***************************************



const D = 0.0625 ;    // 1 / 16, 16 lines for interval [x, x+1]
      S = 40 ;        // 40 character widths for interval [y, y+1]
      H = 34 ;        // character position of x-axis
      C = 6.28318 ;   // 2 * pi
      LIM = 32 ;


var X , Y : REAL ;
    I , N : INTEGER ;


begin (* HAUPTPROGRAMM *)
  for I := 0 to LIM do
    begin
      X := D * I ;
      Y := EXP ( - X ) * SIN ( C * X ) ;
      N := ROUND ( S * Y ) + H ;
      WRITELN ( '*' : N )
    end (* for *)
end (* HAUPTPROGRAMM *) .
