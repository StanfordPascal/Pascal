program MSFEDIT ( INPUT , OUTPUT , DRUCKER , MSFILE ) ;

(***************************************************************)
(*                                                             *)
(*   PROGRAMM MSFEDIT - EDITIEREN EINER MODAL-SPLIT-           *)
(*                      FUNKTION INTERAKTIV                    *)
(*                                                             *)
(*   ERSTELLT VON BERND OPPOLZER IM RAHMEN EINER STUDIEN-      *)
(*   ARBEIT 1984                                               *)
(*                                                             *)
(*   DAS PROGRAMM ERLAUBT ES, EINE MODAL-SPLIT-FUNKTION        *)
(*   INTERAKTIV ZU ERSTELLEN UND ZU VERAENDERN.                *)
(*                                                             *)
(*   DAS PROGRAMM BIETET FOLGENDE MOEGLICHKEITEN:              *)
(*                                                             *)
(*   - EINGEBEN BZW. AENDERN DER MODAL-SPLIT-FUNKTION          *)
(*   - AUSGABE DER STUETZPUNKTE AUF BILDSCHIRM / DRUCKER       *)
(*   - AUSGABE EINER WERTETABELLE AUF BILDSCHIRM / DRUCKER     *)
(*   - AUSGABE EINER GRAPHIK AUF BILDSCHIRM / DRUCKER          *)
(*   - MODAL-SPLIT-FUNKTION AUF FILE 'MSFILE' ABSPEICHERN      *)
(*     (UPDATE)                                                *)
(*                                                             *)
(***************************************************************)



const PZZ = 72 ;
      BZZ = 12 ;
      GRKLD = 64 ;
      LEERKOMM = '                         ' ;

      (**********************************************)
      (*                                            *)
      (*   PROGRAMMKONSTANTEN:                      *)
      (*   -------------------                      *)
      (*                                            *)
      (*   PZZ:   ZEILENZAHL DER DRUCKERS           *)
      (*   BZZ:   FREIE ZEILEN AUF BILDSCHIRM       *)
      (*   GRKLD: ORD(GROSS-A) - ORD(KLEIN-A)       *)
      (*                                            *)
      (**********************************************)



type HTEXT = packed array [ 1 .. 25 ] of CHAR ;

     (**********************************************)
     (*   DARSTELLUNG DER FUNKTION ERFOLGT DURCH   *)
     (*   EINE REIHE VON STUETZPUNKTEN. DIESE      *)
     (*   WERDEN ZUR LAUFZEIT DES MSF-EDITORS      *)
     (*   IN EINER POINTERKETTE ABGELEGT.          *)
     (*   BEMERKENSWERT IST, DASS DIE X-WERTE      *)
     (*   ALS INTEGERS DEFINIERT SIND. DIE         *)
     (*   X-WERTE WERDEN ALS TAUSENDSTEL ABGELEGT. *)
     (*   SO WERDEN PROBLEME MIT DER RECHEN-       *)
     (*   GENAUIGKEIT UMGANGEN.                    *)
     (**********************************************)

     PVERW = -> PUNKT ;
     PUNKT = record
               X : INTEGER ;
               Y : REAL ;
               NEXT : PVERW
             end ;


var ZEILZAHL : INTEGER ;
    SEITENZAHL : INTEGER ;
    UEBERLZAHL : INTEGER ;
    UPDATE : BOOLEAN ;
    ABBRUCH : BOOLEAN ;
    WAHL : INTEGER ;
    DRUCKER : TEXT ;
    MSFILE : TEXT ;
    ANKER : PVERW ;
    FNAME : packed array [ 1 .. 8 ] of CHAR ;

    (***************************************************************)
    (*   BEDEUTUNG DER WICHTIGSTEN VARIABLEN:                      *)
    (*                                                             *)
    (*   ZEILZAHL, SEITENZAHL, UEBERLZAHL:                         *)
    (*    - HILFSVARIABLE ZUR STEUERUNG DES DRUCKER/BILDSCHIRM-    *)
    (*      OUTPUTS.                                               *)
    (*    - UPDATE, ABBRUCH: GLOBALE SCHALTER FUER DIE STEUERUNG   *)
    (*    - DRUCKER:         FILE FUER DRUCKAUSGABE                *)
    (*    - MSFILE:          FILE, DER DIE MS-FUNKTION AUFNIMMT    *)
    (*    - ANKER:           ANKER FUER KETTE DER STUETZPUNKTE     *)
    (*    - FNAME:           EXTERNER NAME DES MS-FILES            *)
    (***************************************************************)




function TOUPPER ( C : CHAR ) : CHAR ;

   EXTERNAL ;



procedure WAIT ;

(**********************************)
(*   WARTET DARAUF, DASS EINE     *)
(*   EINGABE ERFOLGT.             *)
(**********************************)


   var C : CHAR ;

   begin (* WAIT *)
     READLN
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



procedure UEBERSCHRIFT ;

(************************************************************)
(*   GIBT UEBERSCHRIFT AUF DRUCKFILE AUS (SEITENZAEHLUNG)   *)
(************************************************************)


   begin (* UEBERSCHRIFT *)
     SEITENZAHL := SEITENZAHL + 1 ;
     WRITELN ( DRUCKER , '1A U S G A B E   D E R   M O D A L - ' ,
               'S P L I T - F U N K T I O N               NAME : ' ,
               FNAME , ' ' : 13 , 'SEITE' , SEITENZAHL : 4 ) ;
     WRITELN ( DRUCKER ) ;
     WRITELN ( DRUCKER ) ;
     ZEILZAHL := 5
   end (* UEBERSCHRIFT *) ;



procedure DRUCKEZEILE ;

(********************************************************)
(*   ANALOG WRITELN FUER DRUCKFILE, ABER MIT PRUEFUNG   *)
(*   AUF SEITENUEBERLAUF.                               *)
(********************************************************)


   begin (* DRUCKEZEILE *)
     WRITELN ( DRUCKER ) ;
     ZEILZAHL := ZEILZAHL + 1 ;
     if ZEILZAHL > PZZ - 10 then
       UEBERSCHRIFT
   end (* DRUCKEZEILE *) ;



procedure IEIN ( var X : INTEGER ) ;

(***************************************)
(*   NICHTNEGATIVEN INTEGER EINLESEN   *)
(***************************************)


   var COUNT , CV : INTEGER ;
       C : CHAR ;

   begin (* IEIN *)
     COUNT := 0 ;
     X := 0 ;
     repeat
       READ ( C )
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
         if EOLN then
           C := ' '
         else
           READ ( C )
       end (* while *) ;
     READLN
   end (* IEIN *) ;



procedure TEIN ( var X : HTEXT ; var L : INTEGER ) ;

(***************************************)
(*   TEXT 25-STELLIG EINLESEN          *)
(***************************************)


   var COUNT : INTEGER ;
       C : CHAR ;

   begin (* TEIN *)
     X := LEERKOMM ;
     L := 0 ;
     repeat
       READ ( C ) ;
       if not EOLN or ( C <> ' ' ) then
         L := 1
     until ( C <> ' ' ) or EOLN ;
     COUNT := 1 ;
     X [ 1 ] := TOUPPER ( C ) ;
     while not EOLN and ( COUNT < 25 ) do
       begin
         READ ( C ) ;
         COUNT := COUNT + 1 ;
         X [ COUNT ] := TOUPPER ( C ) ;
         L := COUNT
       end (* while *) ;
     READLN
   end (* TEIN *) ;



function JNFRAGE ( JZWANG : BOOLEAN ) : BOOLEAN ;

(*********************************************)
(*   JA/NEIN-ABFRAGE                         *)
(*   JZWANG GIBT AN, WAS ANGENOMMEN WIRD,    *)
(*   WENN WEDER J NOCH N VORLIEGT.           *)
(*********************************************)


   var T : HTEXT ;
       L : INTEGER ;

   begin (* JNFRAGE *)
     TEIN ( T , L ) ;
     if JZWANG then
       JNFRAGE := ( T [ 1 ] = 'J' )
     else
       JNFRAGE := ( T [ 1 ] <> 'N' )
   end (* JNFRAGE *) ;



procedure READREAL ( var R : REAL ; var STATUS : INTEGER ) ;

(**************************************************************)
(*   EINLESEN VON REAL-WERTEN IN DER DARSTELLUNG  Z(N).Z(M)   *)
(*                                                            *)
(*   STATUS 0 : ALLES OK                                      *)
(*   STATUS 1 : END OF LINE ERREICHT BEIM LESEN               *)
(*   STATUS 2 : FALSCHE ZEICHEN IN REAL-ZAHL ENTHALTEN        *)
(**************************************************************)


   var WERT : INTEGER ;
       SKALA : INTEGER ;
       CH : CHAR ;
       PUNKTGEFUNDEN : BOOLEAN ;

   begin (* READREAL *)
     WERT := 0 ;
     SKALA := 1 ;
     PUNKTGEFUNDEN := FALSE ;
     if STATUS = 1 then
       READ ( CH )
     else
       CH := ' ' ;
     STATUS := 0 ;
     while ( CH = ' ' ) and not EOLN do
       READ ( CH ) ;
     if CH = ' ' then
       STATUS := 1
     else
       begin
         repeat
           if CH in [ '0' .. '9' ] then
             begin
               WERT := 10 * WERT + ORD ( CH ) - ORD ( '0' ) ;
               if PUNKTGEFUNDEN then
                 SKALA := SKALA * 10
             end (* then *)
           else
             if CH = '.' then
               begin
                 if PUNKTGEFUNDEN then
                   STATUS := 2
                 else
                   PUNKTGEFUNDEN := TRUE
               end (* then *)
             else
               if CH <> ' ' then
                 STATUS := 2 ;
           if not EOLN then
             READ ( CH )
           else
             CH := ' '
         until ( CH = ' ' ) or ( STATUS <> 0 ) ;
       end (* else *) ;
     if STATUS = 0 then
       R := WERT / SKALA
     else
       begin
         R := 0.0 ;
         if not EOLN then
           READLN
       end (* else *)
   end (* READREAL *) ;



procedure INIT ;

(******************************************************)
(*   LIEST DIE MS-FUNKTION VON DEM FILE MSFILE EIN.   *)
(*   DIE DARSTELLUNG ENTSPRICHT DER DER MODELL-       *)
(*   DATEIEN FUER IV UND OEV (IN DEN ERSTEN SECHS     *)
(*   STELLEN STEHT EINE KENNUNG). ES GIBT HIER        *)
(*   JEDOCH NUR EINE ART VON DATENSAETZEN.            *)
(*   SIE WERDEN DURCH 'FPUNKT' GEKENNZEICHNET.        *)
(******************************************************)


   var KENN : packed array [ 1 .. 6 ] of CHAR ;
       DUMMY : PUNKT ;
       P : PVERW ;


   procedure READKENN ;

   (***************************************************)
   (*  LESEN DER KENNUNG IN DEN ERSTEN SECHS SPALTEN  *)
   (***************************************************)


      var I : INTEGER ;

      begin (* READKENN *)
        for I := 1 to 6 do
          READ ( MSFILE , KENN [ I ] )
      end (* READKENN *) ;


   begin (* INIT *)
     REWRITE ( DRUCKER ) ;
     SEITENZAHL := 0 ;
     ZEILZAHL := 0 ;
     RESET ( MSFILE ) ;
     ANKER := NIL ;
     READKENN ;
     while KENN <> '*ENDE*' do
       begin
         READ ( MSFILE , DUMMY . X ) ;
         READ ( MSFILE , DUMMY . Y ) ;
         DUMMY . NEXT := NIL ;
         READLN ( MSFILE ) ;

     (************************)
     (*  AUFBAUEN DER KETTE  *)
     (************************)

         if ANKER = NIL then
           begin
             NEW ( ANKER ) ;
             P := ANKER
           end (* then *)
         else
           begin
             NEW ( P -> . NEXT ) ;
             P := P -> . NEXT
           end (* else *) ;
         P -> := DUMMY ;
         READKENN
       end (* while *)
   end (* INIT *) ;



procedure UPD ;

(**************************************************)
(*   AUSGEBEN DER KETTE AUF DATEI (ANALOG INIT)   *)
(**************************************************)


   var P : PVERW ;

   begin (* UPD *)
     REWRITE ( MSFILE ) ;
     P := ANKER ;
     while P <> NIL do
       begin
         WRITE ( MSFILE , 'FPUNKT' ) ;
         WRITE ( MSFILE , P -> . X : 10 ) ;
         WRITE ( MSFILE , '   ' ) ;
         WRITE ( MSFILE , P -> . Y ) ;
         WRITELN ( MSFILE ) ;
         P := P -> . NEXT
       end (* while *) ;
     WRITELN ( MSFILE , '*ENDE*' ) ;
     UPDATE := FALSE ;
     READY
   end (* UPD *) ;



procedure SEARCHX ( XSUCH : INTEGER ; var GEFUNDEN : BOOLEAN ; var P :
                  PVERW ; var PVOR : PVERW ) ;

(******************************************************)
(*   SEARCHX - SUCHT IN DER KETTE NACH EINEM X-WERT   *)
(*                                                    *)
(*   BEDEUTUNG DER PARAMETER:                         *)
(*                                                    *)
(*   XSUCH     - SUCHARGUMENT                         *)
(*   GEFUNDEN  - GIBT AN, OB ELEMENT VORHANDEN IST    *)
(*               ODER NICHT                           *)
(*   P, PVOR   - ENTHALTEN IN JEDEM FALL (D.H.        *)
(*               UNABHAENGIG VON 'GEFUNDEN') DIE      *)
(*               POINTER, DIE AUF DAS ELEMENT VOR     *)
(*               DEM GESUCHTEN BZW. DAS DANACH        *)
(*               VERWEISEN.                           *)
(******************************************************)


   begin (* SEARCHX *)
     GEFUNDEN := FALSE ;
     P := ANKER ;
     PVOR := NIL ;
     while ( P <> NIL ) and not GEFUNDEN do
       begin
         if P -> . X < XSUCH then
           begin
             PVOR := P ;
             P := P -> . NEXT
           end (* then *)
         else
           GEFUNDEN := TRUE
       end (* while *) ;
     if P <> NIL then
       GEFUNDEN := ( P -> . X = XSUCH )
     else
       GEFUNDEN := FALSE ;
   end (* SEARCHX *) ;



procedure SEARCHY ( YSUCH : REAL ; var P : PVERW ; var PVOR : PVERW ) ;

(******************************************************)
(*   SEARCHY - SUCHT IN DER KETTE NACH EINEM Y-WERT   *)
(*                                                    *)
(*   BEDEUTUNG DER PARAMETER:                         *)
(*                                                    *)
(*   YSUCH     - SUCHARGUMENT                         *)
(*   P, PVOR   - ENTHALTEN IN JEDEM FALL (D.H.        *)
(*               UNABHAENGIG VON 'GEFUNDEN') DIE      *)
(*               POINTER, DIE AUF DAS ELEMENT VOR     *)
(*               DEM GESUCHTEN BZW. DAS DANACH        *)
(*               VERWEISEN.                           *)
(******************************************************)


   var GEFUNDEN : BOOLEAN ;

   begin (* SEARCHY *)
     GEFUNDEN := FALSE ;
     P := ANKER ;
     PVOR := NIL ;
     while ( P <> NIL ) and not GEFUNDEN do
       begin
         if P -> . Y > YSUCH then
           begin
             PVOR := P ;
             P := P -> . NEXT
           end (* then *)
         else
           GEFUNDEN := TRUE
       end (* while *) ;
   end (* SEARCHY *) ;



function MODALSPLIT ( XSUCH : REAL ) : REAL ;

(****************************************************)
(*   ERRECHNET ANHAND DER KETTE VON STUETZPUNKTEN   *)
(*   DEN FUNKTIONSWERT FUER EIN BEL. ARGUMENT.      *)
(*   ZWISCHEN DEN STUETZPUNKTEN WIRD LINEAR         *)
(*   INTERPOLIERT.                                  *)
(****************************************************)


   var XINT : INTEGER ;
       P , PVOR : PVERW ;
       GEFUNDEN : BOOLEAN ;

   begin (* MODALSPLIT *)
     if XSUCH > 10.0 then
       XSUCH := 10.0 ;
     if XSUCH < 0.0 then
       XSUCH := 0.0 ;
     XINT := ROUND ( XSUCH * 1000 ) ;
     SEARCHX ( XINT , GEFUNDEN , P , PVOR ) ;
     if PVOR = NIL then
       MODALSPLIT := P -> . Y
     else
       if P = NIL then
         MODALSPLIT := PVOR -> . Y
       else
         begin
           MODALSPLIT := ( P -> . Y - PVOR -> . Y ) / ( P -> . X - PVOR
                         -> . X ) * ( XINT - P -> . X ) + P -> . Y
         end (* else *)
   end (* MODALSPLIT *) ;



function MSPRUECK ( YSUCH : REAL ) : REAL ;

(****************************************************)
(*   ERRECHNET ANHAND DER KETTE VON STUETZPUNKTEN   *)
(*   DAS ARGUMENT FUER EINEN BEL. FUNKTIONSWERT.    *)
(*   WENN DER FUNKTIONSWERT NICHT VORKOMMT, WIRD    *)
(*   - 1000.0 UEBERGEBEN.                           *)
(****************************************************)


   var XINT : INTEGER ;
       P , PVOR : PVERW ;
       GEFUNDEN : BOOLEAN ;

   begin (* MSPRUECK *)
     if YSUCH > 100.0 then
       YSUCH := 100.0 ;
     if YSUCH < 0.0 then
       YSUCH := 0.0 ;
     SEARCHY ( YSUCH , P , PVOR ) ;
     if PVOR = NIL then
       MSPRUECK := - 1000.0
     else
       if P = NIL then
         MSPRUECK := - 1000.0
       else
         begin
           MSPRUECK := ( ( P -> . X - PVOR -> . X ) / ( P -> . Y - PVOR
                       -> . Y ) * ( YSUCH - P -> . Y ) + P -> . X ) /
                       1000
         end (* else *)
   end (* MSPRUECK *) ;



procedure PUNKTEIN ( var X : INTEGER ; var Y : REAL ; var SFEHLER :
                   BOOLEAN ) ;

(******************************************************)
(*   PUNKTEIN LIEST EINEN PUNKT VON DER KONSOLE EIN.  *)
(*   MITHILFE VON 'READREAL' WERDEN ZWEI WERTE FUER   *)
(*   X UND Y EINGELESEN. DIE WERTE WERDEN AUF EIN-    *)
(*   HALTUNG DER BEREICHE GEPRUEFT.                   *)
(*   ( 0 < X < 10, 0 < Y < 100 )                      *)
(*   NACH DEN BEIDEN ZAHLEN DARF NICHTS MEHR KOMMEN.  *)
(******************************************************)


   var XREAL : REAL ;
       OK : BOOLEAN ;
       STATUS : INTEGER ;
       CH : CHAR ;

   begin (* PUNKTEIN *)
     STATUS := 0 ;
     OK := FALSE ;
     SFEHLER := FALSE ;
     repeat
       repeat
         READREAL ( XREAL , STATUS ) ;
         if STATUS = 1 then
           begin
             WRITELN ( 'EINGABE UNVOLLSTAENDIG. BITTE FORTSETZEN.' ) ;
           end (* then *)
       until ( STATUS = 0 ) or ( STATUS = 2 ) ;
       if STATUS = 2 then
         begin
           WRITE ( 'FALSCHE EINGABE. ' ) ;
           OK := TRUE ;
           SFEHLER := TRUE
         end (* then *)
       else
         begin
           if ( XREAL < 0.0 ) or ( XREAL > 10.0 ) then
             begin
               WRITE ( 'X-WERT UNZULAESSIG. ' ) ;
               OK := TRUE ;
               SFEHLER := TRUE
             end (* then *)
           else
             begin
               X := ROUND ( XREAL * 1000 ) ;
               repeat
                 READREAL ( Y , STATUS ) ;
                 if STATUS = 1 then
                   begin
                     WRITELN (
                           'EINGABE UNVOLLSTAENDIG. BITTE FORTSETZEN.'
                               ) ;
                   end (* then *)
               until ( STATUS = 0 ) or ( STATUS = 2 ) ;
               if STATUS = 2 then
                 begin
                   WRITE ( 'FALSCHE EINGABE. ' ) ;
                   OK := TRUE ;
                   SFEHLER := TRUE
                 end (* then *)
               else
                 begin
                   if ( Y < 0 ) or ( Y > 100 ) then
                     begin
                       WRITE ( 'Y-WERT UNZULAESSIG. ' ) ;
                       OK := TRUE ;
                       SFEHLER := TRUE
                     end (* then *)
                   else
                     OK := TRUE
                 end (* else *)
             end (* else *)
         end (* else *)
     until OK ;
     CH := ' ' ;
     while ( CH = ' ' ) and not EOLN do
       READ ( CH ) ;
     if CH <> ' ' then
       begin
         WRITE ( 'KEIN ZEILENENDE GEFUNDEN. ' ) ;
         SFEHLER := TRUE
       end (* then *)
   end (* PUNKTEIN *) ;



procedure PXEIN ( var X : INTEGER ; var SFEHLER : BOOLEAN ) ;

(******************************************************)
(*   PXEIN LIEST EINEN X-WERT VON DER KONSOLE EIN.    *)
(*   MITHILFE VON 'READREAL' WIRD EIN WERT FUER       *)
(*   X EINGELESEN. DER WERT WIRD AUF EINHALTUNG DES   *)
(*   BEREICHS GEPRUEFT ( 0 < X < 10 ).                *)
(*   NACH DER ZAHL DARF NICHTS MEHR KOMMEN.           *)
(******************************************************)


   var XREAL : REAL ;
       OK : BOOLEAN ;
       STATUS : INTEGER ;
       CH : CHAR ;

   begin (* PXEIN *)
     STATUS := 0 ;
     OK := FALSE ;
     SFEHLER := FALSE ;
     repeat
       repeat
         READREAL ( XREAL , STATUS ) ;
         if STATUS = 1 then
           begin
             WRITELN ( 'EINGABE UNVOLLSTAENDIG. BITTE FORTSETZEN.' ) ;
           end (* then *)
       until ( STATUS = 0 ) or ( STATUS = 2 ) ;
       if STATUS = 2 then
         begin
           WRITE ( 'FALSCHE EINGABE. ' ) ;
           OK := TRUE ;
           SFEHLER := TRUE
         end (* then *)
       else
         begin
           if ( XREAL < 0.0 ) or ( XREAL > 10.0 ) then
             begin
               WRITE ( 'X-WERT UNZULAESSIG. ' ) ;
               OK := TRUE ;
               SFEHLER := TRUE
             end (* then *)
           else
             begin
               X := ROUND ( XREAL * 1000 ) ;
               OK := TRUE
             end (* else *)
         end (* else *)
     until OK ;
     CH := ' ' ;
     while ( CH = ' ' ) and not EOLN do
       READ ( CH ) ;
     if CH <> ' ' then
       begin
         WRITE ( 'KEIN ZEILENENDE GEFUNDEN. ' ) ;
         SFEHLER := TRUE
       end (* then *)
   end (* PXEIN *) ;



procedure KETTAUS ( var ZAEHL : INTEGER ) ;

(*************************************************)
(*   KETTAUS GIBT SOVIEL WIE MOEGLICH VON DER    *)
(*   AUGENBLICKLICHEN KETTE AUF DEM BILDSCHIRM   *)
(*   AUS. 'UEBERLZAHL' GIBT AN, WIEVIELE         *)
(*   PUNKTE AM ANFANG UEBERLESEN WERDEN SOLLEN.  *)
(*   'UEBERLZAHL' KANN DURCH DIE EINGABEN        *)
(*   'V','R' UND 'T' VERAENDERT WERDEN           *)
(*   (SIEHE PROCEDURE EDITIEREN)                 *)
(*   AM ENDE WIRD ANGEZEIGT, OB DAS ENDE DER     *)
(*   KETTE ERREICHT WURDE ODER OB NOCH ETWAS     *)
(*   UEBRIG IST.                                 *)
(*   DIE VARIABLE ZAEHL GIBT AN, WIEVIEL         *)
(*   ZEILEN AUSGEGEBEN WURDEN. AUF DIESE WEISE   *)
(*   KANN DIE NACHFOLGENDE AUSGABE               *)
(*   'BITTE KOMMANDO EINGEBEN:' IMMER AUF DIE    *)
(*   SELBE ZEILE DES SCHIRMS ERFOLGEN.           *)
(*************************************************)


   var P : PVERW ;
       L : INTEGER ;

   begin (* KETTAUS *)
     L := 0 ;
     P := ANKER ;
     while P <> NIL do
       begin
         L := L + 1 ;
         P := P -> . NEXT
       end (* while *) ;
     if UEBERLZAHL >= L then
       UEBERLZAHL := L - 1 ;
     if UEBERLZAHL < 0 then
       UEBERLZAHL := 0 ;
     P := ANKER ;
     WRITELN ;
     WRITE ( 'LISTE DER STUETZPUNKTE DER MODAL-SPLIT-FUNKTION' ) ;
     WRITE ( ' (AB' , UEBERLZAHL + 1 : 3 , '. PUNKT)' ) ;
     WRITELN ;
     WRITELN ;
     L := UEBERLZAHL ;
     while ( L > 0 ) and ( P <> NIL ) do
       begin
         P := P -> . NEXT ;
         L := L - 1
       end (* while *) ;
     ZAEHL := 0 ;
     while ( P <> NIL ) and ( ZAEHL < BZZ - 1 ) do
       begin
         ZAEHL := ZAEHL + 1 ;
         WRITE ( 'PUNKT (' ) ;
         WRITE ( P -> . X / 1000 : 8 : 3 ) ;
         WRITE ( ' ,' ) ;
         WRITE ( P -> . Y : 10 : 3 ) ;
         WRITELN ( ' )' ) ;
         P := P -> . NEXT
       end (* while *) ;
     WRITELN ;
     if P = NIL then
       WRITELN ( '--- ENDE ---' )
     else
       WRITELN ( '--- ES GIBT NOCH WEITERE PUNKTE ---' ) ;
   end (* KETTAUS *) ;



procedure DISPKETTE ;

(******************************************)
(*   GIBT DIE STUETZPUNKTE DER FUNKTION   *)
(*   AUF BILDSCHIRM AUS                   *)
(******************************************)


   var P : PVERW ;

   begin (* DISPKETTE *)
     P := ANKER ;
     CLRSCRN ;
     WRITE ( 'LISTE DER STUETZPUNKTE DER MODAL-SPLIT-FUNKTION' ) ;
     WRITELN ;
     WRITELN ;
     while ( P <> NIL ) do
       begin
         WRITE ( 'PUNKT (' ) ;
         WRITE ( P -> . X / 1000 : 8 : 3 ) ;
         WRITE ( ' ,' ) ;
         WRITE ( P -> . Y : 10 : 3 ) ;
         WRITELN ( ' )' ) ;
         P := P -> . NEXT
       end (* while *) ;
     WRITELN ;
     READY
   end (* DISPKETTE *) ;



procedure DISPTAB ;

(***********************************************)
(*   GIBT EINE WERTETABELLE DER FUNKTION AUF   *)
(*   BILDSCHIRM AUS. ZU BEGINN WIRD DIE        *)
(*   SCHRITTWEITE DER ARGUMENTE EINGELESEN.    *)
(*   DIE SCHRITTWEITE MUSS ZWISCHEN 0.005      *)
(*   UND 1.0 LIEGEN.                           *)
(***********************************************)


   var SW : REAL ;
       X : REAL ;

   begin (* DISPTAB *)
     repeat
       WRITELN ( 'BITTE EINGABE DER SCHRITTWEITE:' ) ;
       READLN ( SW ) ;
       if ( SW < 0.005 ) or ( SW > 1.0 ) then
         WRITELN ( 'FALSCHE SCHRITTWEITE ( 0.005 <= SW <= 1.0 )' ) ;
     until ( SW >= 0.005 ) and ( SW <= 1.0 ) ;
     CLRSCRN ;
     WRITE ( 'WERTETABELLE DER MODAL-SPLIT-FUNKTION' ) ;
     WRITELN ;
     WRITELN ;
     X := 0.0 ;
     while ( X < 10.0005 ) do
       begin
         WRITE ( 'PUNKT (' ) ;
         WRITE ( X : 8 : 3 ) ;
         WRITE ( ' ,' ) ;
         WRITE ( MODALSPLIT ( X ) : 10 : 3 ) ;
         WRITELN ( ' )' ) ;
         X := X + SW
       end (* while *) ;
     WRITELN ;
     READY
   end (* DISPTAB *) ;



procedure DISPGRAPH ;

(********************************************************)
(*   GIBT AUF DEM BILDSCHIRM EIN SCHAUBILD DER MODAL-   *)
(*   SPLIT-FUNKTION AUS. DAS SCHAUBILD WIRD MIT         *)
(*   STERNEN GEZEICHNET. ES WIRD ALLERDINGS NUR         *)
(*   FOLGENDER BEREICH ANGEZEIGT:                       *)
(*                                                      *)
(*   0.0 <= X <= 7.5                                    *)
(*   0   <= Y <= 100                                    *)
(********************************************************)


   var CFELD : array [ 1 .. 100 , 1 .. 20 ] of CHAR ;
       X1 , Y1 , X , Y : INTEGER ;
       XSUCH , YSUCH , XREAL , YREAL : REAL ;

   begin (* DISPGRAPH *)
     for X := 1 to 100 do
       for Y := 1 to 20 do
         CFELD [ X , Y ] := ' ' ;
     for X := 1 to 100 do
       begin
         XSUCH := X / 10 - 0.05 ;
         YREAL := MODALSPLIT ( XSUCH ) ;
         YREAL := YREAL - 0.001 ;
         Y := TRUNC ( YREAL / 5 ) + 1 ;
         if Y < 1 then
           Y := 1
         else
           if Y > 20 then
             Y := 20 ;
         CFELD [ X , Y ] := '*'
       end (* for *) ;
     for Y := 1 to 20 do
       begin
         YSUCH := Y * 5 - 2.5 ;
         XREAL := MSPRUECK ( YSUCH ) ;
         if XREAL >= 0.0 then
           begin
             X := TRUNC ( XREAL * 10 ) + 1 ;
             if X < 1 then
               X := 1
             else
               if X > 100 then
                 X := 100 ;
             CFELD [ X , Y ] := '*'
           end (* then *)
       end (* for *) ;
     CLRSCRN ;
     for Y := 20 DOWNTO 1 do
       begin
         Y1 := Y ;
         if Y1 MOD 2 = 0 then
           WRITE ( Y1 * 5 : 3 , ' ' )
         else
           WRITE ( '  - ' ) ;
         for X := 1 to 75 do
           begin
             X1 := X ;
             WRITE ( CFELD [ X1 , Y1 ] )
           end (* for *) ;
         WRITELN
       end (* for *) ;
     WRITE ( '    ' ) ;
     for X := 1 to 75 do
       begin
         X1 := X ;
         if X1 = 99 then
           WRITE ( '1' )
         else
           if X1 = 100 then
             WRITE ( '0' )
           else
             if X1 MOD 10 = 0 then
               WRITE ( X1 DIV 10 : 1 )
             else
               WRITE ( '.' )
       end (* for *) ;
     WRITELN ;
     WAIT
   end (* DISPGRAPH *) ;



procedure PRINTGRAPH ;

(********************************************************)
(*   GIBT AUF DEM DRUCKER EIN SCHAUBILD DER MODAL-      *)
(*   SPLIT-FUNKTION AUS. DAS SCHAUBILD WIRD MIT         *)
(*   STERNEN GEZEICHNET. ES WIRD ALLERDINGS NUR         *)
(*   FOLGENDER BEREICH ANGEZEIGT:                       *)
(*                                                      *)
(*   0.0 <= X <= 6.0                                    *)
(*   0   <= Y <= 100                                    *)
(********************************************************)


   var CFELD : array [ 1 .. 120 , 1 .. 50 ] of CHAR ;
       X1 , Y1 , X , Y : INTEGER ;
       XSUCH , YSUCH , XREAL , YREAL : REAL ;

   begin (* PRINTGRAPH *)
     for X := 1 to 120 do
       for Y := 1 to 50 do
         CFELD [ X , Y ] := ' ' ;
     for X := 1 to 120 do
       begin
         XSUCH := X / 20 - 0.025 ;
         YREAL := MODALSPLIT ( XSUCH ) ;
         YREAL := YREAL - 0.001 ;
         Y := TRUNC ( YREAL / 2 ) + 1 ;
         if Y < 1 then
           Y := 1
         else
           if Y > 50 then
             Y := 50 ;
         CFELD [ X , Y ] := '*'
       end (* for *) ;
     for Y := 1 to 50 do
       begin
         YSUCH := Y * 2 - 1.0 ;
         XREAL := MSPRUECK ( YSUCH ) ;
         if XREAL >= 0.0 then
           begin
             X := TRUNC ( XREAL * 20 ) + 1 ;
             if X < 1 then
               X := 1
             else
               if X > 120 then
                 X := 120 ;
             CFELD [ X , Y ] := '*'
           end (* then *)
       end (* for *) ;
     UEBERSCHRIFT ;
     WRITE ( DRUCKER , ' ' , 'GRAPHISCHE DARSTELLUNG ' ,
             'DER MODAL-SPLIT-FUNKTION' ) ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     WRITE ( DRUCKER , ' ANTEIL OEV IN PROZENT' ) ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     ZEILZAHL := 0 ;
     for Y := 50 DOWNTO 1 do
       begin
         Y1 := Y ;
         if Y1 MOD 5 = 0 then
           WRITE ( DRUCKER , ' ' , Y1 * 2 : 3 , ' ' )
         else
           WRITE ( DRUCKER , '   - ' ) ;
         for X := 1 to 120 do
           begin
             X1 := X ;
             WRITE ( DRUCKER , CFELD [ X1 , Y1 ] )
           end (* for *) ;
         DRUCKEZEILE
       end (* for *) ;
     WRITE ( DRUCKER , '     ' ) ;
     for X := 1 to 120 do
       begin
         X1 := X ;
         if X1 MOD 20 = 0 then
           WRITE ( DRUCKER , X1 DIV 20 : 1 )
         else
           WRITE ( DRUCKER , '.' )
       end (* for *) ;
     DRUCKEZEILE ;
     WRITE ( DRUCKER , ' ' : 65 , 'REISEZEITVERHAELTNIS RZOEV/RZIV' ) ;
     DRUCKEZEILE ;
     READY
   end (* PRINTGRAPH *) ;



procedure PRINTTAB ;

(***********************************************)
(*   GIBT EINE WERTETABELLE DER FUNKTION AUF   *)
(*   DEM DRUCKER AUS. ZU BEGINN WIRD DIE       *)
(*   SCHRITTWEITE DER ARGUMENTE EINGELESEN.    *)
(*   DIE SCHRITTWEITE MUSS ZWISCHEN 0.005      *)
(*   UND 1.0 LIEGEN.                           *)
(***********************************************)


   var SW : REAL ;
       X : REAL ;

   begin (* PRINTTAB *)
     repeat
       WRITELN ( 'BITTE EINGABE DER SCHRITTWEITE:' ) ;
       READLN ( SW ) ;
       if ( SW < 0.005 ) or ( SW > 1.0 ) then
         WRITELN ( 'FALSCHE SCHRITTWEITE ( 0.005 <= SW <= 1.0 )' ) ;
     until ( SW >= 0.005 ) and ( SW <= 1.0 ) ;
     UEBERSCHRIFT ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     WRITE ( DRUCKER , ' ' , 'WERTETABELLE DER MODAL-SPLIT-FUNKTION' )
             ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     X := 0.0 ;
     while ( X < 10.0005 ) do
       begin
         WRITE ( DRUCKER , ' PUNKT (' ) ;
         WRITE ( DRUCKER , X : 8 : 3 ) ;
         WRITE ( DRUCKER , ' ,' ) ;
         WRITE ( DRUCKER , MODALSPLIT ( X ) : 10 : 3 ) ;
         WRITE ( DRUCKER , ' )' ) ;
         DRUCKEZEILE ;
         if ZEILZAHL > PZZ - 14 then
           begin
             UEBERSCHRIFT ;
             DRUCKEZEILE ;
             DRUCKEZEILE ;
             WRITE ( DRUCKER , ' ' ,
                     'WERTETABELLE DER MODAL-SPLIT-FUNKTION' ) ;
             DRUCKEZEILE ;
             DRUCKEZEILE ;
           end (* then *) ;
         X := X + SW
       end (* while *) ;
     DRUCKEZEILE ;
     READY
   end (* PRINTTAB *) ;



procedure PRINTKETTE ;

(******************************************)
(*   GIBT DIE STUETZPUNKTE DER FUNKTION   *)
(*   AUF DRUCKER AUS                      *)
(******************************************)


   var P : PVERW ;

   begin (* PRINTKETTE *)
     P := ANKER ;
     UEBERSCHRIFT ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     WRITE ( DRUCKER , ' ' ,
             'LISTE DER STUETZPUNKTE DER MODAL-SPLIT-FUNKTION' ) ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     while ( P <> NIL ) do
       begin
         WRITE ( DRUCKER , ' PUNKT (' ) ;
         WRITE ( DRUCKER , P -> . X / 1000 : 8 : 3 ) ;
         WRITE ( DRUCKER , ' ,' ) ;
         WRITE ( DRUCKER , P -> . Y : 10 : 3 ) ;
         WRITE ( DRUCKER , ' )' ) ;
         DRUCKEZEILE ;
         if ZEILZAHL > PZZ - 14 then
           begin
             UEBERSCHRIFT ;
             DRUCKEZEILE ;
             DRUCKEZEILE ;
             WRITE ( DRUCKER , ' ' ,
                     'LISTE DER STUETZPUNKTE DER MODAL-SPLIT-FUNKTION'
                     ) ;
             DRUCKEZEILE ;
             DRUCKEZEILE ;
           end (* then *) ;
         P := P -> . NEXT
       end (* while *) ;
     DRUCKEZEILE ;
     WRITE ( DRUCKER , ' *** ENDE DER TABELLE ***' ) ;
     DRUCKEZEILE ;
     READY
   end (* PRINTKETTE *) ;



procedure EDITIEREN ;

(*********************************************************)
(*                                                       *)
(*   DIE PROZEDUR EDITIEREN ERMOEGLICHT DAS EINGEBEN,    *)
(*   LOESCHEN UND AENDERN VON STUETZPUNKTEN DER FUNK-    *)
(*   TION. DIE STUETZPUNKTE WERDEN AUF DEM BILDSCHIRM    *)
(*   ANGEZEIGT. DER BENUTZER KANN DURCH DIE EINGABE      *)
(*   VON KOMMANDOS DIESE STUETZPUNKTE MANIPULIEREN.      *)
(*                                                       *)
(*   ES GIBT FOLGENDE KOMMANDOS:                         *)
(*                                                       *)
(*   E = ENDE (VERLASSEN DES EDITIERUNGS-MODUS)          *)
(*   I = EINFUEGEN NEUER PUNKTE                          *)
(*       FORMAT: I <X> <Y>                               *)
(*   D = LOESCHEN VON PUNKTEN                            *)
(*       FORMAT: D <X>                                   *)
(*   C = AENDERN VON PUNKTEN                             *)
(*       FORMAT: C <X> <Y>                               *)
(*   V = BLAETTERN VORWAERTS                             *)
(*   R = BLAETTERN RUECKWAERTS                           *)
(*   T = AN DEN ANFANG GEHEN (TOP)                       *)
(*   ? = AUSGABE DIESER INFORMATION                      *)
(*                                                       *)
(*********************************************************)


   var CH : CHAR ;
       Z : INTEGER ;
       ZAEHL : INTEGER ;
       SFEHLER : BOOLEAN ;


   procedure INSERTPUNKT ( var SFEHLER : BOOLEAN ) ;

   (************************************************************)
   (*   ERLEDIGT DAS EINFUEGEN EINES PUNKTES (KOMMANDO I)      *)
   (*                                                          *)
   (*   NACHDEM EIN KOMMANDO I ERKANNT WURDE, WERDEN NOCH      *)
   (*   AUF DERSELBEN ZEILE ZWEI WERTE FUER X UND Y EINGE-     *)
   (*   LESEN. FOLGENDE FEHLER WERDEN ERKANNT UND BEHANDELT:   *)
   (*                                                          *)
   (*   - PUNKT IST SCHON VORHANDEN.                           *)
   (*   - Y-WERT IST UNZULAESSIG.                              *)
   (************************************************************)


      var X1 : INTEGER ;
          Y1 : REAL ;
          P , PVOR : PVERW ;
          DUMMY : PVERW ;
          GEFUNDEN : BOOLEAN ;


      procedure GRENZEN ;

      (************************************)
      (*   ZEIGT DIE MOEGLICHEN GRENZEN   *)
      (*   FUER DEN Y-WERT AN.            *)
      (************************************)


         begin (* GRENZEN *)
           WRITE ( '( ' ) ;
           if P <> NIL then
             WRITE ( P -> . Y : 8 : 3 , ' <= ' ) ;
           WRITE ( 'Y' ) ;
           if PVOR <> NIL then
             WRITE ( ' <= ' , PVOR -> . Y : 8 : 3 ) ;
           WRITE ( ' ). ' )
         end (* GRENZEN *) ;


      begin (* INSERTPUNKT *)
        PUNKTEIN ( X1 , Y1 , SFEHLER ) ;
        if not SFEHLER then
          begin
            SEARCHX ( X1 , GEFUNDEN , P , PVOR ) ;
            if GEFUNDEN then
              begin
                SFEHLER := TRUE ;
                WRITE ( 'PUNKT IST SCHON VORHANDEN. ' )
              end (* then *)
            else
              begin
                SFEHLER := FALSE ;
                if PVOR <> NIL then
                  if Y1 > PVOR -> . Y then
                    begin
                      WRITE ( 'FALSCHER Y-WERT ' ) ;
                      SFEHLER := TRUE ;
                      GRENZEN
                    end (* then *) ;
                if not SFEHLER then
                  begin
                    if P <> NIL then
                      if Y1 < P -> . Y then
                        begin
                          WRITE ( 'FALSCHER Y-WERT ' ) ;
                          SFEHLER := TRUE ;
                          GRENZEN
                        end (* then *) ;
                    if not SFEHLER then
                      begin
                        NEW ( DUMMY ) ;
                        DUMMY -> . X := X1 ;
                        DUMMY -> . Y := Y1 ;
                        DUMMY -> . NEXT := P ;
                        if PVOR = NIL then
                          ANKER := DUMMY
                        else
                          PVOR -> . NEXT := DUMMY
                      end (* then *)
                  end (* then *)
              end (* else *)
          end (* then *)
      end (* INSERTPUNKT *) ;


   procedure DELETEPUNKT ( var SFEHLER : BOOLEAN ) ;

   (************************************************************)
   (*   ERLEDIGT DAS LOESCHEN EINES PUNKTES (KOMMANDO D)       *)
   (*                                                          *)
   (*   NACHDEM EIN KOMMANDO D ERKANNT WURDE, WIRD NOCH        *)
   (*   AUF DERSELBEN ZEILE EIN WERT FUER X EINGELESEN.        *)
   (*   FOLGENDER FEHLER WIRD ERKANNT UND BEHANDELT:           *)
   (*                                                          *)
   (*   - PUNKT IST NICHT VORHANDEN.                           *)
   (************************************************************)


      var X1 : INTEGER ;
          P , PVOR : PVERW ;
          GEFUNDEN : BOOLEAN ;

      begin (* DELETEPUNKT *)
        PXEIN ( X1 , SFEHLER ) ;
        if not SFEHLER then
          begin
            SEARCHX ( X1 , GEFUNDEN , P , PVOR ) ;
            if not GEFUNDEN then
              begin
                SFEHLER := TRUE ;
                WRITE ( 'PUNKT IST NICHT VORHANDEN. ' )
              end (* then *)
            else
              begin
                SFEHLER := FALSE ;
                if PVOR = NIL then
                  ANKER := P -> . NEXT
                else
                  PVOR -> . NEXT := P -> . NEXT ;
              end (* else *)
          end (* then *)
      end (* DELETEPUNKT *) ;


   procedure CHANGEPUNKT ( var SFEHLER : BOOLEAN ) ;

   (************************************************************)
   (*   ERLEDIGT DAS AENDERN EINES PUNKTES (KOMMANDO C)        *)
   (*                                                          *)
   (*   NACHDEM EIN KOMMANDO C ERKANNT WURDE, WERDEN NOCH      *)
   (*   AUF DERSELBEN ZEILE ZWEI WERTE FUER X UND Y EINGE-     *)
   (*   LESEN. FOLGENDE FEHLER WERDEN ERKANNT UND BEHANDELT:   *)
   (*                                                          *)
   (*   - PUNKT IST NICHT VORHANDEN.                           *)
   (*   - Y-WERT IST UNZULAESSIG.                              *)
   (************************************************************)


      var X1 : INTEGER ;
          Y1 : REAL ;
          PAKT , P , PVOR : PVERW ;
          DUMMY : PVERW ;
          GEFUNDEN : BOOLEAN ;


      procedure GRENZEN ;

      (************************************)
      (*   ZEIGT DIE MOEGLICHEN GRENZEN   *)
      (*   FUER DEN Y-WERT AN.            *)
      (************************************)


         begin (* GRENZEN *)
           WRITE ( '( ' ) ;
           if P <> NIL then
             WRITE ( P -> . Y : 8 : 3 , ' <= ' ) ;
           WRITE ( 'Y' ) ;
           if PVOR <> NIL then
             WRITE ( ' <= ' , PVOR -> . Y : 8 : 3 ) ;
           WRITE ( ' ). ' )
         end (* GRENZEN *) ;


      begin (* CHANGEPUNKT *)
        PUNKTEIN ( X1 , Y1 , SFEHLER ) ;
        if not SFEHLER then
          begin
            SEARCHX ( X1 , GEFUNDEN , P , PVOR ) ;
            if not GEFUNDEN then
              begin
                SFEHLER := TRUE ;
                WRITE ( 'PUNKT IST NICHT VORHANDEN. ' )
              end (* then *)
            else
              begin
                PAKT := P ;
                P := P -> . NEXT ;
                SFEHLER := FALSE ;
                if PVOR <> NIL then
                  if Y1 > PVOR -> . Y then
                    begin
                      WRITE ( 'FALSCHER Y-WERT ' ) ;
                      SFEHLER := TRUE ;
                      GRENZEN
                    end (* then *) ;
                if not SFEHLER then
                  begin
                    if P <> NIL then
                      if Y1 < P -> . Y then
                        begin
                          WRITE ( 'FALSCHER Y-WERT ' ) ;
                          SFEHLER := TRUE ;
                          GRENZEN
                        end (* then *) ;
                    if not SFEHLER then
                      PAKT -> . Y := Y1
                  end (* then *)
              end (* else *)
          end (* then *)
      end (* CHANGEPUNKT *) ;


   begin (* EDITIEREN *)
     UPDATE := TRUE ;
     UEBERLZAHL := 0 ;
     ABBRUCH := FALSE ;
     repeat

     (***********************************************************)
     (*   KETTAUS LIEFERT DIE ANZAHL DER AUSGEGEBENEN ZEILEN.   *)
     (*   AUF DIESE ART KANN DURCH DIE AUSGABE VON LEERZEILEN   *)
     (*   ERREICHT WERDEN, DASS DER TEXT 'BITTE KOMMANDO        *)
     (*   EINGEBEN:' IMMER AUF DERSELBEN ZEILE ERSCHEINT        *)
     (***********************************************************)

       CLRSCRN ;
       WRITELN ( 'EDITIEREN DER MODAL-SPLIT-FUNKTION' ) ;
       KETTAUS ( ZAEHL ) ;
       for Z := ZAEHL + 1 to BZZ do
         WRITELN ;
       WRITELN ( 'BITTE KOMMANDO EINGEBEN:' ) ;
       repeat
         SFEHLER := FALSE ;
         repeat
           READ ( CH )
         until ( CH <> ' ' ) or EOLN ;

     (************************************)
     (*   KOMMANDO-BUCHSTABEN EINLESEN   *)
     (************************************)

         if EOLN and ( CH = ' ' ) then
           begin
             WRITE ( 'Leere Eingabe nicht erlaubt. ' ) ;
             SFEHLER := TRUE ;
             READLN ;
           end (* then *)
         else
           begin
             CH := TOUPPER ( CH ) ;
             if not ( CH in [ 'E' , 'T' , 'V' , 'R' , 'I' , 'D' , 'C' ,
             '?' ] ) then
               begin
                 WRITE ( 'FALSCHE EINGABE. ' ) ;
                 SFEHLER := TRUE ;
                 READLN ;
               end (* then *)
             else
               begin

     (***************************)
     (*   KOMMANDO ABARBEITEN   *)
     (***************************)

                 case CH of
                   'E' : begin
                           ABBRUCH := TRUE ;
                         end (* tag/ca *) ;
                   'V' : UEBERLZAHL := UEBERLZAHL + 5 ;
                   'R' : begin
                           UEBERLZAHL := UEBERLZAHL - 5 ;
                           if UEBERLZAHL < 0 then
                             UEBERLZAHL := 0
                         end (* tag/ca *) ;
                   'T' : UEBERLZAHL := 0 ;
                   'I' : INSERTPUNKT ( SFEHLER ) ;
                   'D' : DELETEPUNKT ( SFEHLER ) ;
                   'C' : CHANGEPUNKT ( SFEHLER ) ;
                   '?' : begin
                           CLRSCRN ;
                           WRITELN ( 'LISTE DER MOEGLICHEN KOMMANDOS:'
                                     ) ;
                           WRITELN ;
                           WRITELN (
                           'E = ENDE (VERLASSEN DES EDITIERUNGS-MODUS'
                                     ) ;
                           WRITELN ( 'I = EINFUEGEN NEUER PUNKTE' ) ;
                           WRITELN ( '    FORMAT: I <X> <Y>' ) ;
                           WRITELN ( 'D = LOESCHEN VON PUNKTEN' ) ;
                           WRITELN ( '    FORMAT: D <X>' ) ;
                           WRITELN ( 'C = AENDERN VON PUNKTEN' ) ;
                           WRITELN ( '    FORMAT: C <X> <Y>' ) ;
                           WRITELN ( 'V = BLAETTERN VORWAERTS' ) ;
                           WRITELN ( 'R = BLAETTERN RUECKWAERTS' ) ;
                           WRITELN ( 'T = AN DEN ANFANG GEHEN (TOP)' )
                                     ;
                           WRITELN ( '? = AUSGABE DIESER INFORMATION' )
                                     ;
                           WRITELN ;
                           WRITELN ( 'BITTE ENTER-TASTE DRUECKEN' ) ;
                           WAIT
                         end (* tag/ca *)
                 end (* case *) ;
                 READLN ;
               end (* else *)
           end (* else *) ;
         if SFEHLER then
           WRITELN ( 'Bitte Eingabe wiederholen.' ) ;
       until not SFEHLER
     until ABBRUCH
   end (* EDITIEREN *) ;



begin (* HAUPTPROGRAMM *)
  FNAME := ' ' ;
  MOVEPARM ( ADDR ( FNAME ) , 8 ) ;
  TOUPPERS ( ADDR ( FNAME ) , 8 ) ;
  UPDATE := FALSE ;
  TERMIN ( INPUT ) ;
  TERMOUT ( OUTPUT ) ;
  INIT ;
  WRITELN ;
  WRITELN ( 'INIT FERTIG' ) ;
  WRITELN ;
  WRITELN ( 'BITTE ENTER-TASTE DRUECKEN' ) ;
  WAIT ;
  repeat
    ABBRUCH := FALSE ;
    repeat
      CLRSCRN ;
      WRITELN ( 'TAETIGKEITSWAHL:' ) ;
      WRITELN ;
      WRITELN ( '(1)----MODAL-SPLIT-FUNKTION NEU EINGEBEN' ) ;
      WRITELN ( '(2)----MODAL-SPLIT-FUNKTION AENDERN' ) ;
      WRITELN ( '(3)----AUSGABE STUETZPUNKTE AUF BILDSCHIRM' ) ;
      WRITELN ( '(4)----AUSGABE STUETZPUNKTE AUF DRUCKER' ) ;
      WRITELN ( '(5)----AUSGABE TABELLE AUF BILDSCHIRM' ) ;
      WRITELN ( '(6)----AUSGABE TABELLE AUF DRUCKER' ) ;
      WRITELN ( '(7)----AUSGABE SCHAUBILD AUF BILDSCHIRM' ) ;
      WRITELN ( '(8)----AUSGABE SCHAUBILD AUF DRUCKER' ) ;
      WRITELN ( '(9)----UPDATE' ) ;
      WRITELN ;
      WRITELN ( 'BITTE GEWUENSCHTE TAETIGKEIT EINGEBEN:' ) ;
      IEIN ( WAHL ) ;
      if ( WAHL <> 0 ) and ( WAHL <= 9 ) then
        begin
          if ( WAHL >= 5 ) and ( WAHL <= 8 ) and ( ANKER = NIL ) then
            begin
              WRITELN ( 'FUNKTION NICHT MOEGLICH' ,
                        ' (KEINE STUETZPUNKTE VORHANDEN).' ,
                        ' BITTE ENTER DRUECKEN. ' ) ;
              WAIT
            end (* then *)
          else
            case WAHL of
              1 : begin
                    ANKER := NIL ;
                    EDITIEREN
                  end (* tag/ca *) ;
              2 : EDITIEREN ;
              3 : DISPKETTE ;
              4 : PRINTKETTE ;
              5 : DISPTAB ;
              6 : PRINTTAB ;
              7 : DISPGRAPH ;
              8 : PRINTGRAPH ;
              9 : UPD
            end (* case *)
        end (* then *)
    until WAHL = 0 ;
    if UPDATE then
      begin
        WRITELN ( 'ENDE OHNE UPDATE (J/N) ?' ) ;
        ABBRUCH := JNFRAGE ( TRUE )
      end (* then *)
    else
      ABBRUCH := TRUE
  until ABBRUCH ;
  if SEITENZAHL <> 0 then
    begin
      WRITELN ( DRUCKER ) ;
      WRITELN ( DRUCKER , '1' )
    end (* then *) ;
end (* HAUPTPROGRAMM *) .
