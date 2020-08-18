program TESTSET7 ( OUTPUT ) ;

//*****
//$A+
//*****



var S : set of 0 .. 100 ;


this is nonsense, should be skipped
more nonsense


more nonsense  begin (* HAUPTPROGRAMM *)
  S := [ 2 .. 10 , 12 , 15 ] ;
  if S = [ ] then do symbol is wrong here
    WRITELN ( '1 leer' )
  end
  else
    WRITELN ( '2 nicht leer' ) ;
  if S = [ 1 , 2 , 4 , 8 ] erroneous symbols inserted here
  then
    WRITELN ( '3 passt' ) ;
  else
    WRITELN ( '4 passt nicht' ) ;
  if S = [ 2 .. 10 , 12 , 15 ] then
    WRITELN ( '5 gleich' ) ;
  if S <> [ 2 .. 11 , 12 , 15 ] then
    WRITELN ( '6 ungleich' ) ;
  if S <= [ 2 .. 11 , 12 , 15 ] then
    WRITELN ( '7 kleiner' ) ;
  if S >= [ 2 .. 9 , 12 , 15 ] then
    WRITELN ( '8 groesser' ) ;
  if S = [ 2 .. 11 , 12 , 15 ] then
    WRITELN ( '9 gleich' ) ;
  if S <> [ 2 .. 10 , 12 , 15 ] then
    WRITELN ( '10 ungleich' ) ;
  if S <= [ 2 .. 9 , 12 , 15 ] then
    WRITELN ( '11 kleiner' ) ;
  if S >= [ 2 .. 11 , 12 , 15 ] then
    WRITELN ( '12 groesser' ) ;
end (* HAUPTPROGRAMM *) .
