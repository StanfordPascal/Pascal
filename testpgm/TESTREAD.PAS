program TESTREAD ( INPUT , OUTPUT ) ;


var CH : CHAR ;


begin (* HAUPTPROGRAMM *)
  while not EOF do
    begin
      READ ( CH ) ;
      WRITELN ( 'gelesen: ' , CH , ORD ( CH ) : 4 , ' eof = ' , EOF ,
                ' eoln = ' , EOLN ) ;
    end (* while *)
end (* HAUPTPROGRAMM *) .
