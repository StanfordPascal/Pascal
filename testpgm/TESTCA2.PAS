program TESTCA2 ( OUTPUT ) ;


type CHAR8 = packed array [ 1 .. 8 ] of CHAR ;


var WORT : CHAR8 ;


begin (* HAUPTPROGRAMM *)
  WORT := 'BERND' ;
  for I := 1 to 8 do
    begin
      case WORT [ I ] of
        'A' .. 'C' :
          WRITELN ( 'ABC' ) ;
        'D' .. 'F' :
          WRITELN ( 'DEF' ) ;
        'G' .. 'Z' :
          WRITELN ( 'other letter' ) ;
        ' ' : WRITELN ( 'blank' ) ;
        otherwise
          WRITELN ( 'other char' )
      end (* case *)
    end (* for *)
end (* HAUPTPROGRAMM *) .
