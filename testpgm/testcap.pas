program TESTCAP ( OUTPUT ) ;


var X : STRING ( 80 ) ;
    CA : CHAR ( 80 ) := 'Initialwert' ;
    CH : CHAR := 'X' ;



procedure TEST ( const S : STRING ) ;

   begin (* TEST *)
     WRITELN ( 'show: ' , S ) ;
   end (* TEST *) ;



begin (* HAUPTPROGRAMM *)
  X := STR ( CA ) ;
  WRITELN ( X ) ;
  X := CA ;
  WRITELN ( X ) ;
  X := CH ;
  WRITELN ( X ) ;
  TEST ( 'string konstante' ) ;
  X := 'string variable' ;
  TEST ( X ) ;
  CA := 'char array' ;
  TEST ( STR ( CA ) ) ;
  TEST ( CA ) ;
  TEST ( CH ) ;
  CA := 'string variable' ;
  if STR ( CA ) = X then
    WRITELN ( 'gleich' ) ;
  if X = CA then
    WRITELN ( 'gleich' ) ;
  return ;
  if CA = X then
    WRITELN ( 'gleich' ) ;
end (* HAUPTPROGRAMM *) .
