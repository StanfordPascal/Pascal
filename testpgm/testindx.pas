program TESTINDX ( INPUT , OUTPUT ) ;


const S1 = 'numerisch / Standard = 0     Splitfeldhilfe' ;
      S2 = 'numerisch   Splitfeldhilfe' ;
      SSUCH = 'Splitfeldhilfe' ;
      S3 = '    abcabcabcdef   ' ;
      S4 = '    abcabcdef      ' ;
      SSUCH2 = 'abcabcdef' ;


var S : STRING ( 60 ) ;


begin (* HAUPTPROGRAMM *)
  WRITELN ( 'index 1 = ' , INDEX ( S2 , SSUCH ) ) ;
  WRITELN ( 'index 2 = ' , INDEX ( S1 , SSUCH ) ) ;
  S := S1 || ' ' ;
  WRITELN ( 'index 3 = ' , INDEX ( S , SSUCH ) ) ;
  WRITELN ( 'index 4 = ' , INDEX ( S3 , SSUCH2 ) ) ;
  WRITELN ( 'index 5 = ' , INDEX ( S4 , SSUCH2 ) ) ;
end (* HAUPTPROGRAMM *) .
