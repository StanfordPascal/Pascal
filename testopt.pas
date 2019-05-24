program TESTOPT ( INPUT , OUTPUT ) ;

//************************************************
//$A+                                             
//************************************************



var I : INTEGER ;
    S : CHAR ( 20 ) ;
    C : CHAR ;


begin (* HAUPTPROGRAMM *)
  S := 'Oberasbach' ;
  WRITELN ( 'length = ' , LENGTH ( S ) ) ;
  WRITELN ( 'trimmed length = ' , LENGTH ( TRIM ( S ) ) ) ;
  for I := 1 to LENGTH ( S ) do
    if ( S [ I ] = 'a' ) or ( S [ I ] = 'b' ) or ( S [ I ] = 'c' ) then
      WRITELN ( I , '-tes Zeichen ist a, b oder c' ) ;
  for I := 1 to LENGTH ( S ) do
    if S [ I ] in [ 'a' , 'b' , 'c' ] then
      WRITELN ( I , '-tes Zeichen ist a, b oder c' ) ;
  for I := 1 to LENGTH ( S ) do
    begin
      C := S [ I ] ;
      if ( C = 'a' ) or ( C = 'b' ) or ( C = 'c' ) then
        WRITELN ( I , '-tes Zeichen ist a, b oder c' ) ;
    end (* for *)
end (* HAUPTPROGRAMM *) .
