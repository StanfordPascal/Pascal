program TESTIEIN ( OUTPUT ) ;


var X : INTEGER ;



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



procedure IEIN ( var X : INTEGER ) ;

(***************************************)
(*   NICHTNEGATIVEN INTEGER EINLESEN   *)
(***************************************)


   var COUNT , CV : INTEGER ;
       C : CHAR ;

   begin (* IEIN *)
     WRITELN ( 'Start Funktion IEIN' ) ;
     COUNT := 0 ;
     X := 0 ;
     repeat
       READ ( C ) ;
       if FALSE then
         WRITELN ( 'gelesen: ' , ORD ( C ) , ' eoln = ' , EOLN ) ;
     until ( C <> ' ' ) or EOLN ;
     while ( C in [ '0' .. '9' ] ) do
       begin
         case C of
           '0' : CV := 0 ;
           '1' : CV := 1 ;
           '2' : CV := 2 ;
           '3' : CV := 3 ;
           '4' : CV := 4 ;
           '5' : CV := 5 ;
           '6' : CV := 6 ;
           '7' : CV := 7 ;
           '8' : CV := 8 ;
           '9' : CV := 9
         end (* case *) ;
         X := ( X * 10 ) + CV ;
         COUNT := COUNT + 1 ;
         if not EOLN then
           READ ( C )
         else
           C := ' ' ;
         if FALSE then
           WRITELN ( 'gelesen: ' , ORD ( C ) , ' eoln = ' , EOLN ) ;
       end (* while *) ;
     READLN ;
     WRITELN ( 'Ende Funktion IEIN' ) ;
   end (* IEIN *) ;



begin (* HAUPTPROGRAMM *)
  TERMIN ( INPUT ) ;
  TERMOUT ( OUTPUT ) ;
  WRITELN ( 'erst mal initialisierung' ) ;
  READY ;
  CLRSCRN ;
  repeat
    WRITELN ( 'eingabe von ints, bis eingabe = 33 ' ) ;
    IEIN ( X ) ;
    WRITELN ( 'gelesen: ' , X ) ;
    READY ;
    CLRSCRN ;
  until X = 33 ;
end (* HAUPTPROGRAMM *) .
