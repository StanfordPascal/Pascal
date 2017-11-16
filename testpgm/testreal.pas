program TESTREAL ( OUTPUT ) ;

(********)
(*$a+   *)
(********)



var X : REAL ;
    Y : REAL ;
    M : INTEGER ;


begin (* HAUPTPROGRAMM *)
  X := 1.5 ;
  Y := SIN ( X ) ;
  WRITELN ( 'SIN (1.5) = ' , Y : 17 : 12 ) ;
  WRITELN ( 'SIN (1.0) = ' , SIN ( 1.0 ) : 17 : 12 ) ;
  Y := X + Y ;
  WRITELN ( 'y at stage 1 = ' , Y : 17 : 12 ) ;
  Y := Y * X ;
  WRITELN ( 'y at stage 2 = ' , Y : 17 : 12 ) ;
  Y := Y - X ;
  WRITELN ( 'y at stage 3 = ' , Y : 17 : 12 ) ;
  M := 12 ;
  Y := Y + M ;
  WRITELN ( 'y at stage 4 = ' , Y : 17 : 12 ) ;
  Y := Y / X ;
  WRITELN ( 'y at stage 5 = ' , Y : 17 : 12 ) ;
end (* HAUPTPROGRAMM *) .
