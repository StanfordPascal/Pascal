program TESTREA7 ( INPUT , OUTPUT ) ;

//*****
//$A+  
//*****



type SHORT = - 1000 .. 1000 ;


var ISTR : CHAR ( 10 ) ;
    I : INTEGER ;
    I2 : INTEGER ;
    CH : CHAR ;
    ISHORT : SHORT ;



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
  WRITELN ( 'testen read mit laenge' ) ;
  WRITELN ;
  repeat
    if FALSE then
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
    READLN ( I ) ;
    WRITE ( 'i = ' , I : 1 ) ;
    WRITELN ;
    WRITELN ( 'bitte noch eine Zahl eingeben:' ) ;
    READLN ( I ) ;
    WRITE ( 'i = ' , I : 1 ) ;
    WRITELN ;
    WRITELN ( 'bitte ishort eingeben:' ) ;
    READLN ( ISHORT ) ;
    WRITE ( 'ishort = ' , ISHORT : 1 ) ;
    WRITELN ;
    WRITELN ( 'bitte eine Zahl eingeben als Laenge fuer READ:' ) ;
    READLN ( ISTR ) ;
    I := IVALSTR ( ADDR ( ISTR ) , 10 ) ;
    WRITELN ( 'ende mit 0' ) ;
    WRITELN ( 'bitte zweimal Zahl eingeben, gelesen wird in Laenge ' ,
              I : 1 ) ;
    READ ( I2 : I ) ;
    WRITELN ( 'read mit Laenge erfolgreich, vor readln' ) ;
    READLN ;
    WRITE ( 'i2 = ' , I2 : 1 ) ;
    WRITELN ;
    READ ( I2 : I ) ;
    READLN ;
    WRITE ( 'i2 = ' , I2 : 1 ) ;
    WRITELN ;
  until I2 = 0
end (* HAUPTPROGRAMM *) .
