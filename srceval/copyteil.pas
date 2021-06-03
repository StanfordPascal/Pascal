program COPYTEIL ( INPFILE , OUTFILE ) ;

//************************************************
// Programm kopiert Binaerdateien                 
// verwendet CSP RDD                              
//************************************************
// war in PCINT noch nicht implementiert (11.2019)
// CSP RDD verwendet fread, fuellt allerdings     
// am Ende zu kurzen Buffer mit Hex Nullen auf    
// dadurch wird Zieldatei immer auf 1024 ohne     
// Rest teilbare Laenge verlaengert               
//************************************************
// eof erst, wenn fread Ergebnis 0 bringt.        
//************************************************
// Abhilfe: spaeter Funktion schreiben, die       
// tatsaechliche gelesene Laenge der Lesefunktion 
// abfragbar macht (Ergebnis von fread).          
// Das kann im letzten fread weniger als die      
// angeforderte Satzlaenge sein.                  
//************************************************



const BUFSIZE = 1024 ;


type BUFFER = CHAR ( BUFSIZE ) ;


var INPFILE : FILE of BUFFER ;
    OUTFILE : FILE of BUFFER ;
    X : BUFFER ;
    SATZZAHL : INTEGER ;


begin (* HAUPTPROGRAMM *)
  RESET ( INPFILE ) ;
  REWRITE ( OUTFILE ) ;
  SATZZAHL := 0 ;
  while not EOF ( INPFILE ) do
    begin
      READ ( INPFILE , X ) ;
      WRITE ( OUTFILE , X ) ;
      SATZZAHL := SATZZAHL + 1 ;
      if SATZZAHL MOD 100000 = 0 then
        WRITELN ( SATZZAHL , ' Buffer ausgegeben' ) ;
    end (* while *) ;
  WRITELN ( SATZZAHL , ' Buffer ausgegeben' ) ;
end (* HAUPTPROGRAMM *) .
