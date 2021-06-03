program TESTREAD ( INPUT , OUTPUT ) ;

//*****
//$A+
//*****



var CH : CHAR ;
    ISTR : CHAR ( 10 ) ;
    I : INTEGER ;
    C10 : CHAR ( 10 ) ;
    S10 : STRING ( 10 ) ;



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
  s10 := ' ' ;
  repeat
    WRITELN ( 'bitte eine zahl eingeben als laenge fuer read:' ) ;
    READLN ( ISTR ) ;
    I := IVALSTR ( ADDR ( ISTR ) , 10 ) ;
    WRITELN ( 'bitte text eingeben (ende mit $):' ) ;
    READLN ( CH : I , C10 : I , S10 : I ) ;
    WRITE ( 'i = ' , I : 1 , ' ' ) ;
    WRITE ( 'ch = ' , CH , ' ' ) ;
    WRITE ( 'c10 = <' , C10 , '> ' ) ;
    WRITE ( 's10 = <' , S10 , '> ' ) ;
    WRITELN ;
  until INDEX ( S10 , '$' ) <> 0
end (* HAUPTPROGRAMM *) .
