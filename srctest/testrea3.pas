program TESTREA3 ( INPUT , OUTPUT ) ;

//*****
//$A+  
//*****



var ISTR : CHAR ( 10 ) ;
    I : INTEGER ;
    R : REAL ;
    CH : CHAR ;



procedure TERMIN ( var X : TEXT ) ;

   EXTERNAL ;



procedure TERMOUT ( var X : TEXT ) ;

   EXTERNAL ;



function IVALSTR ( CP : -> CHAR ; LEN : INTEGER ) : INTEGER ;

   EXTERNAL ;



begin (* HAUPTPROGRAMM *)
  TERMIN ( INPUT ) ;
  TERMOUT ( OUTPUT ) ;
  WRITELN ;
  WRITELN ( 'testen read real mit laenge' ) ;
  WRITELN ;
  repeat
    if TRUE then
      begin
        WRITELN ( 'bitte einen Buchstaben eingeben:' ) ;
        READLN ( CH ) ;
        WRITE ( 'ch = ' , CH : 1 ) ;
        WRITELN ;
        WRITELN ( 'bitte noch einen Buchstaben eingeben:' ) ;
        READLN ( CH ) ;
        WRITE ( 'ch = ' , CH : 1 ) ;
        WRITELN ;
      end (* then *) ;
    WRITELN ( 'bitte eine Zahl eingeben:' ) ;
    READLN ( R ) ;
    WRITE ( 'R = ' , R : 15 : 7 ) ;
    WRITELN ;
    WRITELN ( 'bitte noch eine Zahl eingeben:' ) ;
    READLN ( R ) ;
    WRITE ( 'R = ' , R : 15 : 7 ) ;
    WRITELN ;
    WRITELN ( 'bitte eine Zahl eingeben als Laenge fuer READ:' ) ;
    READLN ( ISTR ) ;
    I := IVALSTR ( ADDR ( ISTR ) , 10 ) ;
    WRITELN ( 'ende mit 0' ) ;
    WRITELN ( 'bitte Zahl eingeben, gelesen wird in Laenge ' , I : 1 )
              ;
    READ ( R : I ) ;
    READLN ;
    WRITE ( 'R = ' , R : 15 : 7 ) ;
    WRITELN ;
  until R = 0.0
end (* HAUPTPROGRAMM *) .
