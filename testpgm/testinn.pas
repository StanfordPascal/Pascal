program TESTINN ( OUTPUT ) ;

(********)
(*$A+   *)
(********)



var CH : CHAR ;
    X : INTEGER ;
    W : INTEGER ;
    C : array [ 10 .. 20 ] of INTEGER ;


begin (* HAUPTPROGRAMM *)
  WRITELN ( 'index?' ) ;
  READ ( X ) ;
  WRITELN ( 'wert?' ) ;
  READ ( W ) ;
  C [ X ] := W ;
  repeat
    READ ( CH ) ;
    if CH in [ '*' , 'C' , 'D' , 'F' , 'H' , 'K' , 'M' , 'P' , 'S' ]
    then
      WRITELN ( CH , ': char ok' )
    else
      WRITELN ( CH , ': char nok' )
  until CH = ' ' ;
end (* HAUPTPROGRAMM *) .
