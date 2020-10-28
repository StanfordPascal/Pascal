program TESTRDB ( INPUT , OUTPUT ) ;


type WIDTH = 0 .. 9 ;


var B : BOOLEAN ;
    B1 : BOOLEAN ;
    B2 : BOOLEAN ;
    W : WIDTH ;
    S : STRING ( 200 ) ;



procedure TERMIN ( var X : TEXT ) ;

   EXTERNAL ;



begin (* HAUPTPROGRAMM *)
  TERMIN ( INPUT ) ;
  B := TRUE ;
  WRITELN ( '-' , B : 10 , '-' , not B : 10 , '-' ) ;
  WRITELN ( '-' , B , '-' , not B , '-' ) ;
  WRITELN ( '-' , B : 3 , '-' , not B : 3 , '-' ) ;
  WRITELN ( '-' , B : 1 , '-' , not B : 1 , '-' ) ;
  repeat
    WRITELN ( 'bitte zweimal Boolean eingeben: ' ) ;
    READLN ( B1 , B2 ) ;
    WRITELN ( '-' , B1 : 1 , '-' , B2 : 1 , '-' ) ;
    WRITELN ( 'bitte zweimal Boolean in Laenge 6 eingeben: ' ) ;
    READLN ( B1 : 6 , B2 : 6 ) ;
    WRITELN ( '-' , B1 : 1 , '-' , B2 : 1 , '-' ) ;
    WRITELN ( 'bitte String eingeben fuer READSTR Boolean Laenge 6' ) ;
    READLN ( S ) ;
    READSTR ( S , B : 6 ) ;
    WRITELN ( '-' , B : 1 , '-' ) ;
    WRITELN ( 'bitte Eingabelaenge eingeben (0..9) ... Ende mit 0:' ) ;
    READLN ( W ) ;
    WRITELN ( 'bitte Boolean in Laenge ' , W : 1 , ' eingeben:' ) ;
    READLN ( B : W ) ;
    WRITELN ( '-' , B : 1 , '-' ) ;
    if W <> 0 then
      WRITELN ( 'nochmal von vorne' ) ;
  until W = 0
end (* HAUPTPROGRAMM *) .
