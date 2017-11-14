program COPYTEXT ( EING , OUTPUT ) ;

(**********************************************)
(*   This program copies the characters       *)
(*   and line structure of the textfile       *)
(*   input to the textfile output.            *)
(**********************************************)



var CH : CHAR ;
    EING : TEXT ;


begin (* HAUPTPROGRAMM *)
  // RESET ( EING ) ;
  if true then
  writeln ('+++ Anfang:',
           ' EOF = ', EOF (EING),
           ' EOLN = ', EOLN (EING));
  while not EOF ( EING ) do
    begin
      while not EOLN ( EING ) do
        begin
          READ ( EING , CH ) ;
          WRITE ( OUTPUT , CH )
        end (* while *) ;
      READLN ( EING ) ;
      WRITELN ( OUTPUT , '***');
    end (* while *)
end (* HAUPTPROGRAMM *) .
