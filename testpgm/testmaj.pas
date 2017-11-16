program TESTMAJ ( OUTPUT ) ;


var KLEINBUCHST : set of CHAR ;
    WORT : array [ 1 .. 10 ] of CHAR ;
    I : INTEGER ;



function MAJOR ( CH : CHAR ) : CHAR ;

(******************************************)
(*   SETZT KLEINBUCHSTABEN IN GROSSE UM   *)
(******************************************)


   begin (* MAJOR *)
     if CH in KLEINBUCHST then
       MAJOR := CHR ( ORD ( CH ) - ORD ( 'a' ) + ORD ( 'A' ) )
     else
       MAJOR := CH
   end (* MAJOR *) ;



begin (* HAUPTPROGRAMM *)
  KLEINBUCHST := [ 'a' .. 'i' , 'j' .. 'r' , 's' .. 'z' ] ;
  repeat
    WRITELN ( 'bitte wort eingeben:' ) ;
    READLN ( WORT ) ;
    for I := 1 to 10 do
      WORT [ I ] := MAJOR ( WORT [ I ] ) ;
    WRITELN ( 'wort neu = ' , WORT ) ;
  until WORT = 'ENDE' ;
end (* HAUPTPROGRAMM *) .
