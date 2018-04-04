program TESTRDR ( INPUT , OUTPUT ) ;


var R : REAL ;


begin (* HAUPTPROGRAMM *)
  repeat
    WRITELN ( 'bitte real-zahl eingeben:' ) ;
    READ ( R ) ;
    WRITELN ( 'gelesen: ' , R ) ;
  until R = 0.0
end (* HAUPTPROGRAMM *) .
