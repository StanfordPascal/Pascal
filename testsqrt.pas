program TESTSQRT ( OUTPUT ) ;


var I : INTEGER ;
    R : REAL ;


begin (* HAUPTPROGRAMM *)
  for I := 1 to 20 do
    begin
      R := I ;
      WRITELN ( 'the sqrt of ' , I : 4 , ' is ' , SQRT ( R ) : 15 : 13
                ) ;
    end (* for *)
end (* HAUPTPROGRAMM *) .
