program TESTCEIN ( OUTPUT ) ;


var C : CHAR ;



procedure WAIT ;

(**********************************)
(*   WARTET DARAUF, DASS EINE     *)
(*   EINGABE ERFOLGT.             *)
(**********************************)


   var I : INTEGER ;
       C : CHAR ;

   begin (* WAIT *)
     WRITELN ( 'Start Funktion Wait' ) ;
     READLN ;
     WRITELN ( 'Ende Funktion Wait' ) ;
   end (* WAIT *) ;



procedure READY ;

(************************************)
(*   READY-MELDUNG, DIE QUITTIERT   *)
(*   WERDEN MUSS.                   *)
(************************************)


   begin (* READY *)
     WRITELN ;
     WRITELN ( 'FUNKTION AUSGEFUEHRT (ENTER DRUECKEN)' ) ;
     WAIT
   end (* READY *) ;



begin (* HAUPTPROGRAMM *)
  if TRUE then
    begin
      TERMIN ( INPUT ) ;
      TERMOUT ( OUTPUT )
    end (* then *) ;
  CLRSCRN ;
  WRITELN ( 'lesen bis zum dollarzeichen ...' ) ;
  WRITELN ( 'erst mal initialisierung ...' ) ;
  WRITELN ( 'start einleseschleife ...' ) ;
  RESET ( INPUT ) ;
  repeat
    READ ( C ) ;
    WRITELN ( 'gelesen: <' , C , '> ord = ' , ORD ( C ) : 3 ) ;
    if EOLN then
      begin
        WRITELN ( '*eoln*' ) ;
        READLN ;
        WRITELN ( 'readln ausgefuehrt' ) ;
        if EOF then
          WRITELN ( '*eof*' ) ;
      end (* then *)
  until ( C = '$' ) or EOF ;
end (* HAUPTPROGRAMM *) .
