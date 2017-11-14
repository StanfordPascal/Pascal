program COPYREAL ( F , G , OUTPUT ) ;


var F , G : FILE of REAL ;
    X : REAL ;
    TESTS : array [ 1 .. 10 ] of CHAR ;
    I : INTEGER ;


begin (* HAUPTPROGRAMM *)
  // rewrite (output);
  // ohne rewrite: Testausgabe fehlt bei CMS
  TESTS := 'TestAusg' ;
  for I := 1 to 10 do
    begin
      OUTPUT -> := TESTS [ I ] ;
      PUT ( OUTPUT ) ;
    end (* for *) ;
  WRITELN ( OUTPUT ) ;
  WRITELN ( 'rewrite f' ) ;
  REWRITE ( F ) ;
  X := 1.0 ;
  while X <= 10.0 do
    begin
      WRITE ( F , X ) ;
      WRITELN ( 'ausgabe auf f mit write: ' , X : 10 : 5 ) ;
      F -> := X ;
      PUT ( F ) ;
      WRITELN ( 'ausgabe auf f mit put:   ' , X : 10 : 5 ) ;
      X := X + 0.5 ;
    end (* while *) ;
  WRITELN ( 'reset f' ) ;
  RESET ( F ) ;
  WRITELN ( 'rewrite g' ) ;
  REWRITE ( G ) ;
  while not EOF ( F ) do
    begin
      WRITELN ( 'put (g) erfolgt' ) ;
      G -> := F -> ;
      PUT ( G ) ;
      WRITELN ( 'f -> enthaelt: ' , F -> : 10 : 5 ) ;
      WRITELN ( 'auch g ->' ) ;
      GET ( F ) ;
      WRITELN ( 'get (f) fuer naechsten satz' ) ;
    end (* while *)
end (* HAUPTPROGRAMM *) .
