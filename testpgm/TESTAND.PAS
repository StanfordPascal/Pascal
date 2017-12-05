program TESTAND ( OUTPUT ) ;

(********)
(*$A+   *)
(********)



var X , Y : INTEGER ;
    A , B : BOOLEAN ;


begin (* HAUPTPROGRAMM *)
  X := 0X0E ;
  Y := 0X1D ;
  A := FALSE ;
  B := TRUE ;
  WRITELN ( 'x        ' , X ) ;
  WRITELN ( 'y        ' , Y ) ;
  WRITELN ( 'x and y  ' , X and Y ) ;
  WRITELN ( 'x and y  ' , X & Y ) ;
  WRITELN ( 'x or y   ' , X or Y ) ;
  WRITELN ( 'x or y   ' , X | Y ) ;
  WRITELN ( 'x xor y  ' , X xor Y ) ;
  WRITELN ( 'not x    ' , not X ) ;
  WRITELN ( 'not y    ' , not Y ) ;
  WRITELN ( 'a        ' , A ) ;
  WRITELN ( 'b        ' , B ) ;
  WRITELN ( 'a and b  ' , A and B ) ;
  WRITELN ( 'a and b  ' , A & B ) ;
  WRITELN ( 'a or b   ' , A or B ) ;
  WRITELN ( 'a or b   ' , A | B ) ;
  WRITELN ( 'a xor b  ' , A xor B ) ;
  WRITELN ( 'not a    ' , not A ) ;
  WRITELN ( 'not b    ' , not B ) ;
end (* HAUPTPROGRAMM *) .
