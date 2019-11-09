program TESTCASE ( OUTPUT ) ;


var X : INTEGER ;


begin (* HAUPTPROGRAMM *)
  for X := - 5 to 5 do
    case X of
      - 5 : WRITELN ( 'minus 5' ) ;
      - 4 : WRITELN ( 'minus 4' ) ;
      - 3 : WRITELN ( 'minus 3' ) ;
      - 2 : WRITELN ( 'minus 2' ) ;
      - 1 : WRITELN ( 'minus 1' ) ;
      0 : WRITELN ( 'null' ) ;
      1 : WRITELN ( 'plus 1' ) ;
      2 : WRITELN ( 'plus 2' ) ;
      3 : WRITELN ( 'plus 3' ) ;
      4 : WRITELN ( 'plus 4' ) ;
      5 : WRITELN ( 'plus 5' ) ;
      otherwise
        WRITELN ( '???' )
    end (* case *)
end (* HAUPTPROGRAMM *) .
