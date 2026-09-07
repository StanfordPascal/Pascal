program WIEOFT ;


var S : STRING ( 100 ) ;
    N : INTEGER ;
    I : INTEGER ;


begin (* HAUPTPROGRAMM *)
  WRITELN ( 'was soll ich schreiben?' ) ;
  READLN ( S ) ;
  WRITELN ( 'wie oft soll ich schreiben?' ) ;
  READLN ( N ) ;
  for I := 1 to N do
    WRITELN ( I : 5 , ' ' : 5 , S )
end (* HAUPTPROGRAMM *) .
