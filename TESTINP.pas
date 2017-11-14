program TESTINP ( INPUT , OUTPUT , EINGABE ) ;

(********)
(*$K-,A+*)
(********)



var ZAHL1 : INTEGER ;
    ZAHL2 : INTEGER ;
    ZAHL3 : INTEGER ;
    CH : CHAR ;
    EINGABE : TEXT ;
    I : INTEGER ;


begin (* HAUPTPROGRAMM *)
  WRITELN ( 'eof/input = ' , EOF ( INPUT ) ) ;
  WRITELN ( 'eof/output = ' , EOF ( OUTPUT ) ) ;
  CH := INPUT -> ;
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
  if FALSE then
    begin
      for I := 1 to 20 do
        WRITELN ( 'es klappt, i = ' , I : 4 ) ;
      for CH := 'A' to 'Z' do
        WRITELN ( 'ch = ' , CH , ' ord = ' , ORD ( CH ) ) ;
    end (* then *)
end (* HAUPTPROGRAMM *) .
