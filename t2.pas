program TESTINP ( INPUT , OUTPUT , EINGABE ) ;

          /*
   das ist ein kommentar
   und noch einer, diesmal etwas laenger
   und ein dritter
          */


var ZAHL1 : INTEGER ;
    /* kommentar in einer zeile */
    ZAHL2 : INTEGER ;
    /* kommentar
       der in der naechsten zeile weitergeht */
    ZAHL3 : INTEGER ;
    CH : CHAR ;
    EINGABE : TEXT ;
    I : INTEGER ;


// ein c++ kommentar
// noch ein c++ kommentar


begin (* HAUPTPROGRAMM *)
  WRITELN ( 'eof/input = ' , EOF ( INPUT ) ) ;
  WRITELN ( 'eof/output = ' , EOF ( OUTPUT ) ) ;
  // sind am Anfang undefiniert
  CH := INPUT -> ;   // dieser Kommentar in derselben Zeile
  WRITELN ( 'ORD (INPUT ->) = ' , ORD ( CH ) ) ;
  CH := EINGABE -> ;
  WRITELN ( 'ORD (EINGABE ->) = ' , ORD ( CH ) ) ;
  WRITELN ( 'bitte zahl1 eingeben' ) ;
  READ ( ZAHL1 ) ;
  WRITELN ( 'bitte zahl2 eingeben' ) ;
  READ ( ZAHL2 ) ;
  WRITELN ( 'bitte zahl3 eingeben' ) ;
  READ ( ZAHL3 ) ;
  WRITELN ( 'zahl1 = ' , ZAHL1 ) ;
  WRITELN ( 'zahl2 = ' , ZAHL2 ) ;
  WRITELN ( 'zahl3 = ' , ZAHL3 ) ;
  READ ( EINGABE , ZAHL1 ) ;
  WRITELN ( 'EINGABE:ZAHL1 = ' , ZAHL1 ) ;
  WRITE ( 'PROGRAMMENDE (OHNE WRITELN)' ) ;
  WRITELN ;
  WRITELN ( 'UND NOCH NE ZEILE MIT WRITELN DAHINTER' ) ;

    //**************************************************************
    // ein c++ kommentar
    // noch ein c++ kommentar
    //**************************************************************

  if FALSE then
    begin
      for I := 1 to 20 do
        WRITELN ( 'es klappt, i = ' , I : 4 ) ;
      for CH := 'A' to 'Z' do
        WRITELN ( 'ch = ' , CH , ' ord = ' , ORD ( CH ) ) ;
    end (* then *)
end (* HAUPTPROGRAMM *) .
