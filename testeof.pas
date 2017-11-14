program TESTEOF ( INPUT , OUTPUT ) ;


var ZAHL , ZAHL1 , ZAHL2 : INTEGER ;


begin (* HAUPTPROGRAMM *)
  while TRUE do
    begin
      if EOF ( INPUT ) then
        break ;
      WRITELN ;
      WRITELN ( 'Bitte erste Testzahl eingeben:' ) ;
      READ ( ZAHL1 ) ;
      WRITELN ( 'Zahl1 = ' , ZAHL1 ) ;
      if EOF ( INPUT ) then
        break ;
      WRITELN ( 'Bitte zweite Testzahl eingeben:' ) ;
      READ ( ZAHL2 ) ;
      WRITELN ( 'Zahl2 = ' , ZAHL2 ) ;
      if EOF ( INPUT ) then
        break ;
    end (* while *) ;
  WRITELN ( 'EOF INPUT gefunden' ) ;
  WRITELN ( 'EOF INPUT gefunden' ) ;
  WRITELN ( 'EOF INPUT gefunden' ) ;
  WRITELN ( 'EOF INPUT gefunden' ) ;
  WRITELN ( 'EOF INPUT gefunden' ) ;
end (* HAUPTPROGRAMM *) .
