program IVMODED ( INPUT , OUTPUT , DRUCKER , IVMODELL , OEVMODELL ) ;

(******************************************************************)
(*                                                                *)
(*   PROGRAMM ZUM EDITIEREN EINES IV-NETZES.                      *)
(*                                                                *)
(*   ERSTELLT IM RAHMEN EINER STUDIENARBEIT 1984 VON              *)
(*   BERND OPPOLZER.                                              *)
(*                                                                *)
(*   STAND: 5.10.84                                               *)
(*                                                                *)
(*   DATEIEN:   INPUT     - KONSOL-EINGABE                        *)
(*              OUTPUT    - BILDSCHIRM-AUSGABE                    *)
(*              DRUCKER   - DRUCKER-AUSGABE                       *)
(*              IVMODELL  - IV-MODELL-DATEI                       *)
(*              OEVMODELL - OEV-MODELL-DATEI                      *)
(*                                                                *)
(*   NACH DEM STARTEN ERSTELLT DER EDITOR AUS DEN IN DER          *)
(*   IV-MODELLDATEI STEHENDEN INFORMATIONEN IM HAUPTSPEICHER      *)
(*   DAS IV-MODELL. DIESES KANN VOM BENUTZER VERAENDERT UND       *)
(*   BEI BEDARF WIEDER AUF DIE DATEI AUSGEGEBEN WERDEN.           *)
(*   DIE BEDIENERFUEHRUNG GESCHIEHT MENUEGESTEUERT.               *)
(*                                                                *)
(******************************************************************)



const BMAX = 2000 ;
      VPMAX = 10000 ;
      STRMAX = 1000 ;
      KATMAX = 8 ;
      PZZ = 72 ;
      GRKLD = 64 ;
      LEERKOMM = '                         ' ;

      (**********************************************)
      (*                                            *)
      (*   PROGRAMMKONSTANTEN:                      *)
      (*   -------------------                      *)
      (*                                            *)
      (*   BMAX   :  MAX. ANZAHL BEZIRKE            *)
      (*   VPMAX  :  MAX. ANZAHL KNOTENPUNKTE       *)
      (*   STRMAX :  MAX. ANZAHL STRASSEN           *)
      (*   KATMAX :  ANZ. DER MOEGL. KATEGORIEN     *)
      (*   PZZ    :  ANZAHL ZEILEN DES DRUCKERS     *)
      (*   GRKLD  :  ORD(GROSS-A) - ORD(KLEIN-A)    *)
      (*                                            *)
      (**********************************************)



type BTYP = 0 .. BMAX ;
     VPTYP = 0 .. VPMAX ;
     STRTYP = 0 .. STRMAX ;

     (*****************************************)
     (*   SUBRANGE-TYPEN (SIEHE KONSTANTEN)   *)
     (*****************************************)

     STAT = 0 .. 4 ;
     SITUATIONEN = ( HAUPT , NORMAL , SPAET ) ;
     HTEXT = packed array [ 1 .. 25 ] of CHAR ;

     (***********************)
     (*   DIV. HILFSTYPEN   *)
     (***********************)

     APTYP = -> ANSCHLUSS ;
     ANSCHLUSS = record
                   VKPUNKT : INTEGER ;
                   NEXT : APTYP ;
                   ANFAHRZ : array [ SITUATIONEN ] of INTEGER ;
                   INORTS : BOOLEAN ;
                 end ;

     (****************************************)
     (*   LISTE VON ANSCHLUESSEN JE BEZIRK   *)
     (*   MIT ANFAHRZEITEN JE VERKEHRS-      *)
     (*   SITUATION.                         *)
     (****************************************)

     POSITION = record
                  X : INTEGER ;
                  Y : INTEGER
                end ;
     KATEGORIEN = 0 .. KATMAX ;
     STPTYP = -> STPUNKT ;
     STPUNKT = record
                 VKPUNKT : INTEGER ;
                 NEXT : STPTYP ;
                 KAT : KATEGORIEN ;
                 INNERORTS : BOOLEAN ;
                 ZEITZUSCHL : array [ SITUATIONEN ] of INTEGER ;
                 DISTANZ : INTEGER ;
                 WERTEOK : BOOLEAN ;
                 VKPGEPRUEFT : BOOLEAN ;
               end ;

     (*************************************************************)
     (*                                                           *)
     (*   TYPEN ZUR DARSTELLUNG VON STRASSEN                      *)
     (*                                                           *)
     (*   STPUNKT - ENTHAELT DIE RELEVANTEN INFORMATIONEN         *)
     (*             FUER EINEN STRECKENPUNKT; UNTER ANDEREM       *)
     (*             DIE NUMMER DES KNOTENPUNKTES, DIE             *)
     (*             KATEGORIE, MOEGLICHE STAUWARTEZEITEN SOWIE    *)
     (*             DIE DISTANZ, AUSSERDEM ZWEI BOOLEAN-FELDER,   *)
     (*             DIE BEI DER BEARBEITUNG DER STRASSE ZUR       *)
     (*             MARKIERUNG UNVOLLSTAENDIGER ELEMENTE BE-      *)
     (*             NOETIGT WERDEN.                               *)
     (*                                                           *)
     (*   STRASSE - ENTHAELT DIE STRASSENBEZEICHNUNG UND          *)
     (*             ZWEI POINTER, DIE AUF DAS ANFANG BZW.         *)
     (*             DAS ENDE DER KETTE DER STRASSENELEMENTE       *)
     (*             (STPUNKT) WEISEN.                             *)
     (*                                                           *)
     (*************************************************************)

     STRASSE = record
                 ANFANG : STPTYP ;
                 ENDE : STPTYP ;
                 KENNUNG : CHAR ;
                 NUMMER : INTEGER ;
                 RICHTUNG : ( HIN , RUECK )
               end ;


var SITTEXTE : array [ SITUATIONEN ] of packed array [ 1 .. 6 ] of CHAR
               ;
    SITMIN : SITUATIONEN ;
    SITMAX : SITUATIONEN ;
    MODNAME : packed array [ 1 .. 8 ] of CHAR ;
    RICHTIG : BOOLEAN ;
    UPDATE : BOOLEAN ;
    KENNMENGE : set of CHAR ;
    RICHTMENGE : set of CHAR ;
    ZEILZAHL : INTEGER ;
    SEITENZAHL : INTEGER ;
    DELKETTE : STPTYP ;
    BZAHL : BTYP ;

    (**************************)
    (*  ANZAHL BEZIRKE        *)
    (**************************)

    VPZAHL : VPTYP ;

    (**************************)
    (*  ANZAHL KNOTENPUNKTE   *)
    (**************************)

    STRZAHL : STRTYP ;

    (**************************)
    (*  ANZAHL STRASSEN       *)
    (**************************)

    DRUCKER : TEXT ;
    IVMODELL , OEVMODELL : TEXT ;
    ANSCHLUESSE : array [ BTYP ] of APTYP ;
    BTRANSFER : array [ BTYP ] of INTEGER ;

    (*************************)
    (*  BEZIRKSCODIERUNG     *)
    (*************************)

    VPKOORD : array [ VPTYP ] of POSITION ;
    VPTRANSFER : array [ VPTYP ] of INTEGER ;

    (****************************)
    (*  KNOTENPUNKT-CODIERUNG   *)
    (****************************)

    STRASSENNETZ : array [ STRTYP ] of STRASSE ;

    (****************************)
    (*  STRASSENNETZ            *)
    (****************************)

    X , ELAENGE : INTEGER ;



function TOUPPER ( C : CHAR ) : CHAR ;

   EXTERNAL ;



procedure WAIT ;

(*******************************************)
(*   WARTET AUF DRUECKEN DER ENTER-TASTE   *)
(*******************************************)


   var C : CHAR ;

   begin (* WAIT *)
     READLN
   end (* WAIT *) ;



procedure READY ;

(****************************************)
(*   READY-MELDUNG (WEITER MIT ENTER)   *)
(****************************************)


   begin (* READY *)
     WRITELN ;
     WRITELN ( 'FUNKTION AUSGEFUEHRT (ENTER DRUECKEN)' ) ;
     WAIT
   end (* READY *) ;



procedure QUITTIEREN ;

(****************************************)
(*   QUITTIEREN    (WEITER MIT ENTER)   *)
(****************************************)


   begin (* QUITTIEREN *)
     WRITELN ;
     WRITELN ( 'BITTE ENTER DRUECKEN' ) ;
     WAIT
   end (* QUITTIEREN *) ;



procedure UEBERSCHRIFT ;

(**************************************)
(*   UEBERSCHRIFT FUER DRUCKAUSGABE   *)
(**************************************)


   begin (* UEBERSCHRIFT *)
     SEITENZAHL := SEITENZAHL + 1 ;
     WRITELN ( DRUCKER , '1A U S G A B E   D E R   I V - M O D E L L '
               , 'P A R A M E T E R           MODELLNAME : ' , MODNAME
               , ' ' : 15 , 'SEITE' , SEITENZAHL : 4 ) ;
     WRITELN ( DRUCKER ) ;
     WRITELN ( DRUCKER ) ;
     ZEILZAHL := 5
   end (* UEBERSCHRIFT *) ;



procedure DRUCKEZEILE ;

(*****************************************************************)
(*   AUSGABE EINER DRUCKZEILE MIT PRUEFUNG AUF SEITENUEBERLAUF   *)
(*****************************************************************)


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



procedure IEIN2 ( var X : INTEGER ; var COUNT : INTEGER ) ;

(***************************************)
(*   NICHTNEGATIVEN INTEGER EINLESEN   *)
(***************************************)


   var CV : INTEGER ;
       C : CHAR ;

   begin (* IEIN2 *)
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
   end (* IEIN2 *) ;



procedure TEIN ( var X : HTEXT ; var L : INTEGER ) ;

(***************************************)
(*   TEXT 25-STELLIG EINLESEN          *)
(***************************************)


   var COUNT : INTEGER ;
       C : CHAR ;

   begin (* TEIN *)
     X := '                         ' ;
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



function STELLEN ( I : INTEGER ) : INTEGER ;

(************************************************************)
(*   LIEFERT DIE ANZAHL DER STELLEN EINER INTEGER-GROESSE   *)
(************************************************************)


   begin (* STELLEN *)
     STELLEN := 1 ;
     if I > 9 then
       STELLEN := 2 ;
     if I > 99 then
       STELLEN := 3 ;
     if I > 999 then
       STELLEN := 4 ;
     if I > 9999 then
       STELLEN := 5 ;
     if I > 99999 then
       STELLEN := 6 ;
     if I > 999999 then
       STELLEN := 7 ;
     if I > 9999999 then
       STELLEN := 8 ;
     if I > 99999999 then
       STELLEN := 9 ;
     if I > 999999999 then
       STELLEN := 10 ;
   end (* STELLEN *) ;



procedure LEERZEILE ( ANZAHL : INTEGER ) ;

(**********************************)
(*   GIBT ANZAHL LEERZEILEN AUS   *)
(**********************************)


   begin (* LEERZEILE *)
     while ANZAHL > 0 do
       begin
         WRITELN ;
         ANZAHL := ANZAHL - 1
       end (* while *)
   end (* LEERZEILE *) ;



function JNFRAGE ( JZWANG : BOOLEAN ) : BOOLEAN ;

(***********************************************)
(*   JA/NEIN-ABFRAGE                           *)
(*                                             *)
(*   JZWANG LEGT FEST, WAS BEI EINGABE VON     *)
(*   WEDER J NOCH N GESCHIEHT.                 *)
(***********************************************)


   var T : HTEXT ;
       L : INTEGER ;

   begin (* JNFRAGE *)
     TEIN ( T , L ) ;
     if JZWANG then
       JNFRAGE := ( T [ 1 ] = 'J' )
     else
       JNFRAGE := ( T [ 1 ] <> 'N' )
   end (* JNFRAGE *) ;



function GLEICHSTR ( X1 : STRASSE ; X2 : STRASSE ) : BOOLEAN ;

(*********************************************************)
(*   PRUEFUNG AUF GLEICHHEIT VON STRASSENBEZEICHNUNGEN   *)
(*********************************************************)


   begin (* GLEICHSTR *)
     GLEICHSTR := ( X1 . KENNUNG = X2 . KENNUNG ) and ( X1 . NUMMER =
                  X2 . NUMMER ) and ( X1 . RICHTUNG = X2 . RICHTUNG )
   end (* GLEICHSTR *) ;



function KLEINERSTR ( X1 : STRASSE ; X2 : STRASSE ) : BOOLEAN ;

(***********************************************************)
(*   PRUEFUNG GROESSER/KLEINER BEI STRASSENBEZEICHNUNGEN   *)
(***********************************************************)


   begin (* KLEINERSTR *)
     if X1 . KENNUNG <> X2 . KENNUNG then
       KLEINERSTR := X1 . KENNUNG < X2 . KENNUNG
     else
       if X1 . NUMMER <> X2 . NUMMER then
         KLEINERSTR := X1 . NUMMER < X2 . NUMMER
       else
         KLEINERSTR := X1 . RICHTUNG < X2 . RICHTUNG
   end (* KLEINERSTR *) ;



procedure NEWSTP ( var X : STPTYP ) ;

(*********************************************)
(*   EIGENE SPEICHERVERWALTUNG STPTYP: NEW   *)
(*                                           *)
(*   DIE NICHT MEHR BENOETIGEN ELEMENTE      *)
(*   WERDEN IN EINER SPEZIELLEN KETTE        *)
(*   (DELKETTE) GESAMMELT. DIE PROZEDUR      *)
(*   NEWSTP PRUEFT ERST, OB DORT ELEMENTE    *)
(*   VORHANDEN SIND. WENN JA, WERDEN SIE     *)
(*   WIEDERVERWENDET.                        *)
(*********************************************)


   begin (* NEWSTP *)
     if DELKETTE = NIL then
       begin
         NEW ( X ) ;
         X -> . NEXT := NIL
       end (* then *)
     else
       begin
         X := DELKETTE ;
         DELKETTE := DELKETTE -> . NEXT ;
         X -> . NEXT := NIL
       end (* else *)
   end (* NEWSTP *) ;



procedure DISPOSESTP ( var X : STPTYP ) ;

(*************************************************)
(*   EIGENE SPEICHERVERWALTUNG STPTYP: DISPOSE   *)
(*************************************************)


   begin (* DISPOSESTP *)
     X -> . NEXT := DELKETTE ;
     DELKETTE := X ;
     X := NIL
   end (* DISPOSESTP *) ;



procedure DISPKETTE ( var X : STPTYP ) ;

(**************************************)
(*   DISPOSE UEBER EINE GANZE KETTE   *)
(**************************************)


   var Y : STPTYP ;

   begin (* DISPKETTE *)
     while X <> NIL do
       begin
         Y := X -> . NEXT ;
         DISPOSESTP ( X ) ;
         X := Y
       end (* while *)
   end (* DISPKETTE *) ;



procedure INIT ;

(*************************************************************)
(*   DIESE PROZEDUR LIEST DAS IV-MODELL VON DER DATEI EIN.   *)
(*   ZUM AUFBAU DER DATEI SIEHE DOKUMENTATION.               *)
(*   HIER SEI NUR SOVIEL GESAGT:                             *)
(*   IN DEN ERSTEN SECHS SPALTEN DER DATEI STEHT EINE        *)
(*   KENNUNG, DIE ANGIBT, WELCHE DATENSTRUKTUR DURCH DIE     *)
(*   INFORMATIONEN IN DER ZEILE REPRAESENTIERT WIRD.         *)
(*   DIE INFORMATIONEN BESTEHEN WEITGEHEND AUS INTEGER-      *)
(*   GROESSEN.                                               *)
(*************************************************************)


   var I : INTEGER ;
       KENN : packed array [ 1 .. 6 ] of CHAR ;
       VKNUMMER : INTEGER ;
       BP , BPX : APTYP ;
       STR : STRASSE ;
       STP : STPTYP ;
       CH : CHAR ;
       IOKENNZ : CHAR ;
       SIT : SITUATIONEN ;


   procedure READKENN ;

      var I : INTEGER ;

      begin (* READKENN *)
        for I := 1 to 6 do
          READ ( IVMODELL , KENN [ I ] )
      end (* READKENN *) ;


   begin (* INIT *)
     REWRITE ( DRUCKER ) ;
     SEITENZAHL := 0 ;
     RESET ( IVMODELL ) ;

     (********************************************************)
     (*   FELDER USW., DIE DAS IV-MODELL AUFNEHMEN SOLLEN,   *)
     (*   VORBESETZEN.                                       *)
     (********************************************************)

     for I := 1 to BMAX do
       begin
         BTRANSFER [ I ] := 0 ;
         ANSCHLUESSE [ I ] := NIL
       end (* for *) ;
     for I := 1 to VPMAX do
       begin
         VPTRANSFER [ I ] := 0 ;
         VPKOORD [ I ] . X := 0 ;
         VPKOORD [ I ] . Y := 0
       end (* for *) ;
     STR . ANFANG := NIL ;
     STR . ENDE := NIL ;
     STR . KENNUNG := ' ' ;
     STR . NUMMER := 0 ;
     STR . RICHTUNG := HIN ;
     for I := 1 to STRMAX do
       STRASSENNETZ [ I ] := STR ;

     (************************)
     (*   BEZIRKE EINLESEN   *)
     (************************)

     BZAHL := 0 ;
     READKENN ;
     while KENN = 'BTFILE' do
       begin
         BZAHL := BZAHL + 1 ;
         READ ( IVMODELL , BTRANSFER [ BZAHL ] ) ;
         READLN ( IVMODELL ) ;
         READKENN
       end (* while *) ;

     (****************************)
     (*   ANSCHLUESSE EINLESEN   *)
     (****************************)

     for I := 1 to BZAHL do
       begin
         BPX := NIL ;
         BP := NIL ;
         READ ( IVMODELL , VKNUMMER ) ;
         while VKNUMMER <> 0 do
           begin
             if BPX = NIL then
               begin
                 NEW ( BPX ) ;
                 BP := BPX
               end (* then *)
             else
               begin
                 NEW ( BPX -> . NEXT ) ;
                 BPX := BPX -> . NEXT
               end (* else *) ;
             BPX -> . VKPUNKT := VKNUMMER ;
             BPX -> . NEXT := NIL ;
             for SIT := SITMIN to SITMAX do
               READ ( IVMODELL , BPX -> . ANFAHRZ [ SIT ] ) ;
             IOKENNZ := ' ' ;
             if not EOLN ( IVMODELL ) then
               repeat
                 READ ( IVMODELL , IOKENNZ )
               until ( IOKENNZ <> ' ' ) or EOLN ( IVMODELL ) ;
             if IOKENNZ = ' ' then
               IOKENNZ := 'A' ;
             BPX -> . INORTS := IOKENNZ = 'I' ;
             READLN ( IVMODELL ) ;
             READKENN ;
             READ ( IVMODELL , VKNUMMER )
           end (* while *) ;
         ANSCHLUESSE [ I ] := BP ;
         READLN ( IVMODELL ) ;
         READKENN
       end (* for *) ;

     (*****************************)
     (*   KNOTENPUNKTE EINLESEN   *)
     (*****************************)

     VPZAHL := 0 ;
     while KENN = 'KNOTEN' do
       begin
         VPZAHL := VPZAHL + 1 ;
         READ ( IVMODELL , VPTRANSFER [ VPZAHL ] , VPKOORD [ VPZAHL ] .
                X , VPKOORD [ VPZAHL ] . Y ) ;
         READLN ( IVMODELL ) ;
         READKENN
       end (* while *) ;

     (******************************)
     (*   STRASSENDATEN EINLESEN   *)
     (******************************)

     STRZAHL := 0 ;
     while KENN = 'STRASS' do
       begin
         repeat
           READ ( IVMODELL , CH )
         until CH <> ' ' ;
         STR . KENNUNG := CH ;
         READ ( IVMODELL , STR . NUMMER ) ;
         repeat
           READ ( IVMODELL , CH )
         until CH <> ' ' ;
         if CH = 'H' then
           STR . RICHTUNG := HIN
         else
           STR . RICHTUNG := RUECK ;
         STP := NIL ;
         STR . ANFANG := NIL ;
         STR . ENDE := NIL ;
         READLN ( IVMODELL ) ;
         READKENN ;
         while KENN = 'STRECK' do
           begin
             if STP = NIL then
               begin
                 NEWSTP ( STP ) ;
                 STR . ANFANG := STP
               end (* then *)
             else
               begin
                 NEWSTP ( STP -> . NEXT ) ;
                 STP := STP -> . NEXT ;
                 STR . ENDE := STP
               end (* else *) ;
             READ ( IVMODELL , STP -> . VKPUNKT ) ;
             READ ( IVMODELL , STP -> . DISTANZ ) ;
             READ ( IVMODELL , STP -> . KAT ) ;
             for SIT := SITMIN to SITMAX do
               READ ( IVMODELL , STP -> . ZEITZUSCHL [ SIT ] ) ;
             IOKENNZ := ' ' ;
             if not EOLN ( IVMODELL ) then
               repeat
                 READ ( IVMODELL , IOKENNZ )
               until ( IOKENNZ <> ' ' ) or EOLN ( IVMODELL ) ;
             if IOKENNZ = ' ' then
               IOKENNZ := 'A' ;
             STP -> . INNERORTS := IOKENNZ = 'I' ;
             STP -> . NEXT := NIL ;
             STP -> . WERTEOK := TRUE ;
             STP -> . VKPGEPRUEFT := TRUE ;
             READLN ( IVMODELL ) ;
             READKENN
           end (* while *) ;
         STRZAHL := STRZAHL + 1 ;
         STRASSENNETZ [ STRZAHL ] := STR
       end (* while *) ;

     (************************************************)
     (*   WENN DAS DATEIENDE NICHT ERREICHT WURDE,   *)
     (*   FEHLEN ENTWEDER ZEILEN ODER ES SIND ZU     *)
     (*   VIELE (KANN NUR DURCH MANIPULATION AN      *)
     (*   DER MODELLDATEI AUFTRETEN).                *)
     (************************************************)

     if KENN <> '*ENDE*' then
       begin
         WRITELN ;
         WRITELN ( '*** FALSCHER AUFBAU DER MODELLDATEI ***' ) ;
         WRITELN ( '*** ABBRUCH !! ************************' ) ;
         WRITELN ;
         HALT
       end (* then *)
   end (* INIT *) ;



procedure UPD ;

(*******************************************)
(*   ANALOG DER PROZEDUR INIT WERDEN       *)
(*   HIER DIE DATEN IM RICHTIGEN FORMAT    *)
(*   AUF DIE MODELLDATEI AUSGEGEBEN.       *)
(*******************************************)


   var I , J : INTEGER ;
       BP : APTYP ;
       SP : STPTYP ;
       STR : STRASSE ;
       SIT : SITUATIONEN ;

   begin (* UPD *)
     UPDATE := TRUE ;
     REWRITE ( IVMODELL ) ;
     for I := 1 to BZAHL do
       WRITELN ( IVMODELL , 'BTFILE' , BTRANSFER [ I ] : 10 ) ;
     for I := 1 to BZAHL do
       begin
         BP := ANSCHLUESSE [ I ] ;
         while BP <> NIL do
           begin
             WRITE ( IVMODELL , 'ANSCHL' , BP -> . VKPUNKT : 10 ) ;
             for SIT := SITMIN to SITMAX do
               WRITE ( IVMODELL , BP -> . ANFAHRZ [ SIT ] : 10 ) ;
             if BP -> . INORTS then
               WRITE ( IVMODELL , 'I' : 5 )
             else
               WRITE ( IVMODELL , 'A' : 5 ) ;
             WRITELN ( IVMODELL ) ;
             BP := BP -> . NEXT
           end (* while *) ;
         WRITE ( IVMODELL , 'ANSCHL' ) ;
         WRITE ( IVMODELL , 0 : 10 ) ;
         for SIT := SITMIN to SITMAX do
           WRITE ( IVMODELL , 0 : 10 ) ;
         WRITE ( IVMODELL , 'A' : 5 ) ;
         WRITELN ( IVMODELL )
       end (* for *) ;
     for I := 1 to VPZAHL do
       WRITELN ( IVMODELL , 'KNOTEN' , VPTRANSFER [ I ] : 10 , VPKOORD
                 [ I ] . X : 10 , VPKOORD [ I ] . Y : 10 ) ;
     for I := 1 to STRZAHL do
       begin
         STR := STRASSENNETZ [ I ] ;
         WRITE ( IVMODELL , 'STRASS' , STR . KENNUNG : 10 , STR .
                 NUMMER : 10 ) ;
         if STR . RICHTUNG = HIN then
           WRITELN ( IVMODELL , 'H' : 10 )
         else
           WRITELN ( IVMODELL , 'R' : 10 ) ;
         SP := STR . ANFANG ;
         while SP <> NIL do
           begin
             WRITE ( IVMODELL , 'STRECK' ) ;
             WRITE ( IVMODELL , SP -> . VKPUNKT : 10 ) ;
             WRITE ( IVMODELL , SP -> . DISTANZ : 10 ) ;
             WRITE ( IVMODELL , SP -> . KAT : 10 ) ;
             for SIT := SITMIN to SITMAX do
               WRITE ( IVMODELL , SP -> . ZEITZUSCHL [ SIT ] : 10 ) ;
             if SP -> . INNERORTS then
               WRITE ( IVMODELL , 'I' : 5 )
             else
               WRITE ( IVMODELL , 'A' : 5 ) ;
             WRITELN ( IVMODELL ) ;
             SP := SP -> . NEXT
           end (* while *)
       end (* for *) ;
     WRITELN ( IVMODELL , '*ENDE*' ) ;
     READY
   end (* UPD *) ;



procedure INITBEZIRKE ;

(*******************************************)
(*   DAS OEV-MODELL IST PRINZIPIELL        *)
(*   GENAUSO AUFGEBAUT WIE DAS IV-MODELL.  *)
(*   MIT DIESER PROZEDUR KOENNEN           *)
(*   DIE BEZIRKE VOM OEV-MODELL IN DAS     *)
(*   IV-MODELL UEBERNOMMEN WERDEN.         *)
(*******************************************)


   var KENN : packed array [ 1 .. 6 ] of CHAR ;
       ZAHL : INTEGER ;
       STATUS : STAT ;


   procedure INSERTB ( NUMMER : INTEGER ; var STATUS : STAT ) ;

   (************************)
   (*   BEZIRK EINFUEGEN   *)
   (************************)


      var I , J : BTYP ;

      begin (* INSERTB *)
        if BZAHL = BMAX - 1 then
          STATUS := 2
        else
          begin
            I := 1 ;
            while ( BTRANSFER [ I ] < NUMMER ) and ( I <= BZAHL ) do
              I := I + 1 ;
            if I > BZAHL then
              begin
                BTRANSFER [ I ] := NUMMER ;
                BZAHL := BZAHL + 1 ;
                ANSCHLUESSE [ I ] := NIL ;
                STATUS := 0
              end (* then *)
            else
              if ( BTRANSFER [ I ] <> NUMMER ) then
                begin
                  for J := BZAHL DOWNTO I do
                    begin
                      BTRANSFER [ J + 1 ] := BTRANSFER [ J ] ;
                      ANSCHLUESSE [ J + 1 ] := ANSCHLUESSE [ J ] ;
                    end (* for *) ;
                  BTRANSFER [ I ] := NUMMER ;
                  BZAHL := BZAHL + 1 ;
                  ANSCHLUESSE [ I ] := NIL ;
                  STATUS := 0
                end (* then *)
              else
                STATUS := 1
          end (* else *)
      end (* INSERTB *) ;


   procedure READKENN ;

      var I : INTEGER ;

      begin (* READKENN *)
        for I := 1 to 6 do
          READ ( OEVMODELL , KENN [ I ] )
      end (* READKENN *) ;


   begin (* INITBEZIRKE *)
     CLRSCRN ;
     WRITELN ( 'BEZIRKE VOM OEV-MODELL-FILE EINLESEN' ) ;
     WRITELN ;
     RESET ( OEVMODELL ) ;
     READKENN ;
     while ( KENN <> 'BTFILE' ) and ( KENN <> '*ENDE*' ) do
       begin
         READLN ( OEVMODELL ) ;
         READKENN
       end (* while *) ;
     if KENN = '*ENDE*' then
       WRITELN ( 'KEINE BEZIRKE IM OEV-MODELL VORHANDEN.' )
     else
       while KENN = 'BTFILE' do
         begin
           READ ( OEVMODELL , ZAHL ) ;
           INSERTB ( ZAHL , STATUS ) ;
           case STATUS of
             0 : WRITE ( 'BEZIRK IN IV-MODELL UEBERNOMMEN:' ) ;
             1 : WRITE ( 'BEZIRK SCHON VORHANDEN:         ' ) ;
             2 : WRITE ( 'UEBERLAUF, FELDER ZU KLEIN DEFINIERT !!' )
           end (* case *) ;
           if STATUS <> 2 then
             WRITE ( ZAHL : 10 ) ;
           WRITELN ;
           READLN ( OEVMODELL ) ;
           READKENN
         end (* while *) ;
     READY ;
   end (* INITBEZIRKE *) ;



procedure STRZUW ( var X1 : STRASSE ; X2 : STRASSE ) ;

(******************************************************)
(*   DER STRASSE X1 WIRD DIE STRASSE X2 ZUGEWIESEN,   *)
(*   WOBEI DIE KETTE DER KNOTENPUNKTE IHREN ORT       *)
(*   VERAENDERT (SICHERN DER ALTEN KETTE VOR          *)
(*   AENDERUNG)                                       *)
(******************************************************)


   var STP , STP2 : STPTYP ;

   begin (* STRZUW *)
     X1 := X2 ;
     STP2 := X2 . ANFANG ;
     STP := NIL ;
     while STP2 <> NIL do
       begin
         if STP = NIL then
           begin
             NEWSTP ( STP ) ;
             X1 . ANFANG := STP
           end (* then *)
         else
           begin
             NEWSTP ( STP -> . NEXT ) ;
             STP := STP -> . NEXT ;
             X1 . ENDE := STP
           end (* else *) ;
         STP -> := STP2 -> ;
         STP2 := STP2 -> . NEXT
       end (* while *)
   end (* STRZUW *) ;



procedure SBEZAUSG ( var TF : TEXT ; STR : STRASSE ; STELLENZAHL :
                   INTEGER ) ;

(************************************)
(*   STRASSENBEZEICHNUNG AUSGEBEN   *)
(************************************)


   var ZAHL : INTEGER ;
       RICHT : CHAR ;

   begin (* SBEZAUSG *)
     WRITE ( TF , STR . KENNUNG ) ;
     ZAHL := STELLEN ( STR . NUMMER ) ;
     WRITE ( TF , STR . NUMMER : ZAHL + 1 ) ;
     if STR . RICHTUNG = HIN then
       RICHT := 'H'
     else
       RICHT := 'R' ;
     WRITE ( TF , ' ' , RICHT ) ;
     if STELLENZAHL > ZAHL + 4 then
       WRITE ( TF , ' ' : STELLENZAHL - ZAHL - 4 )
   end (* SBEZAUSG *) ;



procedure SBEZEINL ( var KEN : CHAR ; var NUMMER : INTEGER ; var RICHT
                   : CHAR ; var STATUS : STAT ) ;

(***********************************)
(*   STRASSENBEZEICHUNG EINLESEN   *)
(***********************************)


   var T : HTEXT ;
       L : INTEGER ;
       I : INTEGER ;
       OK : BOOLEAN ;

   begin (* SBEZEINL *)
     STATUS := 0 ;
     repeat
       OK := FALSE ;
       TEIN ( T , L ) ;
       if T = LEERKOMM then
         begin
           STATUS := 1 ;
           OK := TRUE
         end (* then *)
       else
         begin
           KEN := T [ 1 ] ;
           if KEN in KENNMENGE then
             begin
               I := 2 ;
               NUMMER := 0 ;
               while ( T [ I ] = ' ' ) and ( I < 25 ) do
                 I := I + 1 ;
               while ( T [ I ] >= '0' ) and ( T [ I ] <= '9' ) and ( I
               < 25 ) do
                 begin
                   NUMMER := 10 * NUMMER + ORD ( T [ I ] ) - ORD ( '0'
                             ) ;
                   I := I + 1
                 end (* while *) ;
               if ( T [ I ] >= '0' ) and ( T [ I ] <= '9' ) then
                 NUMMER := 10 * NUMMER + ORD ( T [ I ] ) - ORD ( '0' )
                           ;
               while ( T [ I ] = ' ' ) and ( I < 25 ) do
                 I := I + 1 ;
               RICHT := T [ I ] ;
               OK := ( NUMMER > 0 ) and ( NUMMER < 10000 ) and ( RICHT
                     in RICHTMENGE )
             end (* then *)
         end (* else *) ;
       if not OK then
         WRITELN ( 'FALSCHE EINGABE, BITTE WIEDERHOLEN !' )
     until OK
   end (* SBEZEINL *) ;



procedure SEARCHB ( BEZ : INTEGER ; var IND : BTYP ; var STATUS : STAT
                  ) ;

(********************************************)
(*   INTERNE NUMMER EINES BEZIRKES SUCHEN   *)
(********************************************)


   var I : BTYP ;

   begin (* SEARCHB *)
     I := 1 ;
     while ( BTRANSFER [ I ] < BEZ ) and ( I <= BZAHL ) do
       I := I + 1 ;
     if I > BZAHL then
       STATUS := 1
     else
       if BTRANSFER [ I ] = BEZ then
         begin
           IND := I ;
           STATUS := 0
         end (* then *)
       else
         STATUS := 1
   end (* SEARCHB *) ;



procedure SEARCHVP ( VPNUMMER : INTEGER ; var IND : VPTYP ; var STATUS
                   : STAT ) ;

(*************************************************)
(*   INTERNE NUMMER EINES KNOTENPUNKTES SUCHEN   *)
(*************************************************)


   var I : VPTYP ;

   begin (* SEARCHVP *)
     I := 1 ;
     while ( VPTRANSFER [ I ] < VPNUMMER ) and ( I <= VPZAHL ) do
       I := I + 1 ;
     if I > VPZAHL then
       STATUS := 1
     else
       if VPTRANSFER [ I ] = VPNUMMER then
         begin
           IND := I ;
           STATUS := 0
         end (* then *)
       else
         STATUS := 1
   end (* SEARCHVP *) ;



procedure SEARCHANS ( BIND : BTYP ; VPIND : VPTYP ; var STATUS : STAT )
                    ;

(*******************************)
(*   ANSCHLUSS VORHANDEN ?     *)
(*******************************)


   var AKT : APTYP ;
       SWITCH : BOOLEAN ;

   begin (* SEARCHANS *)
     SWITCH := FALSE ;
     AKT := ANSCHLUESSE [ BIND ] ;
     while ( AKT <> NIL ) do
       begin
         if AKT -> . VKPUNKT = VPTRANSFER [ VPIND ] then
           SWITCH := TRUE ;
         AKT := AKT -> . NEXT
       end (* while *) ;
     if SWITCH then
       STATUS := 0
     else
       STATUS := 1
   end (* SEARCHANS *) ;



procedure SEARCHSTR ( KEN : CHAR ; STRNUMMER : INTEGER ; RICHT : CHAR ;
                    var SIND : STRTYP ; var STATUS : STAT ) ;

(******************************)
(*   SUCHT EINE STRASSE       *)
(******************************)


   var SHILF : STRASSE ;

   begin (* SEARCHSTR *)
     SHILF . KENNUNG := KEN ;
     SHILF . NUMMER := STRNUMMER ;
     SHILF . ANFANG := NIL ;
     SHILF . ENDE := NIL ;
     if RICHT = 'H' then
       SHILF . RICHTUNG := HIN
     else
       SHILF . RICHTUNG := RUECK ;
     SIND := 1 ;
     while KLEINERSTR ( STRASSENNETZ [ SIND ] , SHILF ) and ( SIND <=
     STRZAHL ) do
       SIND := SIND + 1 ;
     if SIND > STRZAHL then
       STATUS := 1
     else
       if not GLEICHSTR ( STRASSENNETZ [ SIND ] , SHILF ) then
         STATUS := 1
       else
         STATUS := 0
   end (* SEARCHSTR *) ;



procedure INSERTANS ( BNUMMER , VPNUMMER : INTEGER ; var STATUS : STAT
                    ) ;

(***************************)
(*   ANSCHLUSS EINFUEGEN   *)
(***************************)


   var BIND : BTYP ;
       VPIND : VPTYP ;
       P : APTYP ;
       X : INTEGER ;
       OK : BOOLEAN ;
       SIT : SITUATIONEN ;
       T : HTEXT ;
       L : INTEGER ;
       TESTCH : CHAR ;
       EINGABEOK : BOOLEAN ;

   begin (* INSERTANS *)
     SEARCHB ( BNUMMER , BIND , STATUS ) ;
     if STATUS <> 0 then
       STATUS := 2
     else
       begin
         SEARCHVP ( VPNUMMER , VPIND , STATUS ) ;
         if STATUS <> 0 then
           STATUS := 3
         else
           begin
             SEARCHANS ( BIND , VPIND , STATUS ) ;
             if STATUS <> 0 then
               begin
                 NEW ( P ) ;
                 P -> . VKPUNKT := VPNUMMER ;
                 P -> . NEXT := ANSCHLUESSE [ BIND ] ;
                 repeat
                   OK := FALSE ;
                   WRITELN ( 'INNERORTS / AUSSERORTS ?' ) ;
                   repeat
                     TEIN ( T , L ) ;
                     TESTCH := T [ 1 ] ;
                     EINGABEOK := ( L <= 1 ) and ( ( TESTCH = 'A' ) or
                                  ( TESTCH = 'I' ) ) ;
                     if not EINGABEOK then
                       begin
                         WRITELN (
                            'EINGABE FALSCH, NUR -I- UND -A- ERLAUBT.'
                                   ) ;
                         WRITELN ( 'BITTE EINGABE WIEDERHOLEN.' )
                       end (* then *)
                   until EINGABEOK ;
                   P -> . INORTS := TESTCH = 'I' ;
                   WRITELN ( 'EINGABE DER ANFAHRZEITEN' ) ;
                   for SIT := SITMIN to SITMAX do
                     begin
                       WRITELN ( 'ANFAHRZEIT(' , SITTEXTE [ SIT ] ,
                                 '):' ) ;
                       IEIN ( X ) ;
                       P -> . ANFAHRZ [ SIT ] := X
                     end (* for *) ;
                   WRITELN ( 'EINGABEN O.K. (J/N) ?' ) ;
                   OK := JNFRAGE ( TRUE )
                 until OK ;
                 ANSCHLUESSE [ BIND ] := P ;
                 STATUS := 0
               end (* then *)
             else
               STATUS := 1
           end (* else *)
       end (* else *)
   end (* INSERTANS *) ;



procedure INSERTVP ( VPNUMMER : INTEGER ; var STATUS : STAT ) ;

(*********************************************)
(*   FUEGT EINEN VERKEHRS-KNOTEN-PUNKT EIN   *)
(*********************************************)


   var I , J , X , Y : INTEGER ;

   begin (* INSERTVP *)
     if VPZAHL >= VPMAX - 1 then
       STATUS := 2
     else
       begin
         I := 1 ;
         while ( VPTRANSFER [ I ] < VPNUMMER ) and ( I <= VPZAHL ) do
           I := I + 1 ;
         if I > VPZAHL then
           begin
             VPTRANSFER [ I ] := VPNUMMER ;
             VPZAHL := VPZAHL + 1 ;
             STATUS := 0
           end (* then *)
         else
           if ( VPTRANSFER [ I ] <> VPNUMMER ) then
             begin
               for J := VPZAHL DOWNTO I do
                 begin
                   VPKOORD [ J + 1 ] := VPKOORD [ J ] ;
                   VPTRANSFER [ J + 1 ] := VPTRANSFER [ J ]
                 end (* for *) ;
               VPTRANSFER [ I ] := VPNUMMER ;
               VPZAHL := VPZAHL + 1 ;
               STATUS := 0
             end (* then *)
           else
             STATUS := 1 ;
         if STATUS = 0 then
           begin
             WRITELN ( 'X-POSITION:' ) ;
             IEIN ( X ) ;
             WRITELN ( 'Y-POSITION:' ) ;
             IEIN ( Y ) ;
             VPKOORD [ I ] . X := X ;
             VPKOORD [ I ] . Y := Y
           end (* then *)
       end (* else *)
   end (* INSERTVP *) ;



procedure STRBEARB ( var STR : STRASSE ; var OK : BOOLEAN ; NEU :
                   BOOLEAN ) ;

(********************************)
(*   BEARBEITEN EINER STRASSE   *)
(********************************)


   var ENDE : BOOLEAN ;
       ABBRUCH : BOOLEAN ;


   procedure STRBEKOPF ( STR : STRASSE ) ;

   (**************************)
   (*   KOPFZEILE AUSGEBEN   *)
   (**************************)


      begin (* STRBEKOPF *)
        WRITE ( 'BEARBEITEN DER STRASSE ' ) ;
        SBEZAUSG ( OUTPUT , STR , 0 ) ;
        WRITELN
      end (* STRBEKOPF *) ;


   procedure STRAUSGABE ( STR : STRASSE ) ;

   (****************************)
   (*   STRASSENZUG AUSGEBEN   *)
   (****************************)


      var AZPOS : INTEGER ;
          X : STPTYP ;
          SIT : SITUATIONEN ;


      procedure KNAUSGABE ( KN : INTEGER ) ;

      (*********************************************)
      (*   KNOTEN INNERHALB STRASSENZUG AUSGEBEN   *)
      (*********************************************)


         var ST : INTEGER ;

         begin (* KNAUSGABE *)
           ST := STELLEN ( KN ) ;
           if AZPOS + ST >= 75 then
             begin
               WRITELN ;
               WRITE ( '  ' , KN : ST ) ;
               AZPOS := 2 + ST
             end (* then *)
           else
             begin
               WRITE ( KN : ST ) ;
               AZPOS := AZPOS + ST
             end (* else *)
         end (* KNAUSGABE *) ;


      begin (* STRAUSGABE *)
        if STR . ANFANG = NIL then
          WRITELN ( '--- STRASSE IST LEER ---' )
        else
          begin
            WRITE ( '( ' ) ;
            AZPOS := 2 ;
            X := STR . ANFANG ;
            repeat
              KNAUSGABE ( X -> . VKPUNKT ) ;
              if not X -> . WERTEOK then
                begin
                  WRITE ( '''' ) ;
                  AZPOS := AZPOS + 1
                end (* then *) ;
              if AZPOS < 75 then
                begin
                  WRITE ( '  ' ) ;
                  AZPOS := AZPOS + 2
                end (* then *) ;
              X := X -> . NEXT
            until X = STR . ENDE ;
            KNAUSGABE ( X -> . VKPUNKT ) ;
            X -> . WERTEOK := TRUE ;
            X -> . DISTANZ := 0 ;
            X -> . KAT := 0 ;
            X -> . INNERORTS := FALSE ;
            for SIT := SITMIN to SITMAX do
              X -> . ZEITZUSCHL [ SIT ] := 0 ;
            WRITELN ( ' )' )
          end (* else *)
      end (* STRAUSGABE *) ;


   procedure SKOMM ( var STR : STRASSE ; var ENDE : BOOLEAN ; var
                   ABBRUCH : BOOLEAN ) ;

   (**************************************************)
   (*                                                *)
   (*   KOMMANDO ZUR STRASSENBEARBEITUNG EINLESEN,   *)
   (*   PRUEFEN UND AUSFUEHREN                       *)
   (*                                                *)
   (*   DIE KOMMANDOS WERDEN SYNTAKTISCH GEPRUEFT.   *)
   (*   GGF. WIRD EINE NEUEINGABE ANGEFORDERT.       *)
   (*                                                *)
   (*   WENN EIN KOMMANDO NICHT IN EINE ZEILE        *)
   (*   PASST, KANN DER REST IN EINE ZWEITE          *)
   (*   GESCHRIEBEN WERDEN. DAS UNVOLLSTAENDIGE      *)
   (*   KOMMANDO WIRD ABGESCHICKT. NACH EINER        *)
   (*   MELDUNG DES PROGRAMMS KANN DER REST          *)
   (*   EINGEGEBEN WERDEN.                           *)
   (*                                                *)
   (*   ES WIRD DARAUF GEACHTET, DASS ALLE           *)
   (*   KOMMANDOS, DIE ZUR AUSFUEHRUNG GELANGEN,     *)
   (*   FOLGENDE BEDINGUNGEN NICHT VERLETZEN:        *)
   (*                                                *)
   (*   1.) EIN KNOTENPUNKT DARF NUR EINMAL IN       *)
   (*       EINER STRASSE VORKOMMEN                  *)
   (*                                                *)
   (*   2.) WENN AM STRASSENVERLAUF ETWAS GE-        *)
   (*       AENDERT WIRD, MUSS SICHERGESTELLT        *)
   (*       SEIN, DASS IM ANSCHLUSS AN DIESE         *)
   (*       AENDERUNG FUER DIE BETREFFENDEN          *)
   (*       ABSCHNITTE NEUE WERTE EINGEGEBEN         *)
   (*       WERDEN (WERTEOK WIRD GGF. AUF            *)
   (*       FALSE GESETZT.                           *)
   (*                                                *)
   (**************************************************)


      type SYMBOL = ( FALSCH , EOLINE , KLAMMAUF , KLAMMZU , SCHRAEG ,
                    KOMMANDO , KNZAHL , ZUGROSS ) ;

      var OK : BOOLEAN ;
          KOMMSET : set of CHAR ;
          SY : SYMBOL ;
          LASTCH : CHAR ;
          KOMMCH : CHAR ;
          ERGZAHL : INTEGER ;
          ZPOS : INTEGER ;
          KETTE1 : STPTYP ;
          X : STPTYP ;


      procedure NEXTCH ( var CH : CHAR ) ;

      (**********************************)
      (*   NAECHSTES ZEICHEN EINLESEN   *)
      (**********************************)


         begin (* NEXTCH *)
           if not EOLN then
             READ ( CH )
           else
             CH := ' '
         end (* NEXTCH *) ;


      procedure HILFE ;

      (******************************************)
      (*   HELPTEXTE AUSGEBEN FUER S-KOMMANDO   *)
      (******************************************)


         begin (* HILFE *)
           WRITELN ( 'MOEGLICHE KOMMANDOS:' ) ;
           WRITELN ;
           WRITELN ( 'A = ANHAENGEN EINER LISTE (HINTEN)' ) ;
           WRITELN ( 'C = MARKIEREN EINER TEILLISTE ZUM AENDERN' ) ;
           WRITELN ( 'D = LOESCHEN EINER TEILLISTE' ) ;
           WRITELN ( 'E = ENDE MIT ABSPEICHERN' ) ;
           WRITELN ( 'G = NEUANLAGE AUS GEGENRICHTUNG' ) ;
           WRITELN (
                   'I = EINFUEGEN EINER TEILLISTE HINTER GEG. ELEMENT'
                     ) ;
           WRITELN ( 'N = NEUANLAGE EINER LISTE' ) ;
           WRITELN ( 'P = ANHAENGEN EINER LISTE (VORNE)' ) ;
           WRITELN ( 'X = ENDE OHNE ABSPEICHERN' ) ;
           WRITELN ;
           WRITELN ( 'BITTE ENTER DRUECKEN' ) ;
         end (* HILFE *) ;


      procedure SFEHLER ( I : INTEGER ) ;

      (******************************************************)
      (*   FEHLERMELDUNG BEI FALSCHEM S-KOMMANDO AUSGEBEN   *)
      (******************************************************)


         var Z : INTEGER ;

         begin (* SFEHLER *)
           if ( I >= 2 ) then
             begin
               Z := ZPOS - 1 ;
               if Z > 1 then
                 WRITELN ( '!' : Z )
               else
                 WRITELN ( '!' )
             end (* then *) ;
           case I of
             1 : WRITE ( 'LEERZEILE NICHT ERLAUBT. ' ) ;
             2 : WRITE ( 'KOMMANDO (A,C,D,E,G,I,N,P,X) ERWARTET. ' ) ;
             3 : WRITE ( 'KLAMMER-AUF ERWARTET. ' ) ;
             4 : WRITE ( 'KLAMMER-ZU ERWARTET. ' ) ;
             5 : WRITE ( 'LEERE LISTE NICHT ERLAUBT. ' ) ;
             6 : WRITE ( 'KNOTENPUNKT BEREITS VORHANDEN. ' ) ;
             7 : WRITE ( 'EINELEMENTIGE LISTE NICHT ERLAUBT. ' ) ;
             8 : WRITE ( 'BEI LEERER LISTE NICHT MOEGLICH. ' ) ;
             9 : WRITE ( 'TEILLISTE NICHT VORHANDEN. ' ) ;
             10 : WRITE ( 'FEHLER BEI LAENGENVERGLEICH. ' ) ;
             11 : WRITE ( 'KNOTENPUNKT 0 NICHT ERLAUBT. ' ) ;
             12 : WRITE ( 'SCHRAEGSTRICH ERWARTET. ' ) ;
             13 : WRITE ( 'KNOTENPUNKT (ZAHL) ERWARTET. ' ) ;
             14 : WRITE ( 'KNOTEN NICHT IN LISTE ENTHALTEN. ' ) ;
             15 : WRITE ( 'GEGENRICHTUNG NICHT VORHANDEN. ' ) ;
           end (* case *) ;
           if I <> 2 then
             WRITELN ( 'BITTE EINGABE WIEDERHOLEN !' )
           else
             begin
               WRITELN ;
               HILFE
             end (* else *)
         end (* SFEHLER *) ;


      procedure FIND ( var SY : SYMBOL ; var KOMMCH : CHAR ; var
                     ERGZAHL : INTEGER ; var LASTCH : CHAR ; var ZPOS :
                     INTEGER ) ;

      (****************************************************)
      (*   NAECHSTES SYMBOL INNERHALB S-KOMMANDO SUCHEN   *)
      (****************************************************)


         var CH : CHAR ;
             LAENGE : INTEGER ;

         begin (* FIND *)
           CH := LASTCH ;
           if ( CH = ' ' ) and not EOLN then
             repeat
               READ ( CH ) ;
               ZPOS := ZPOS + 1
             until ( CH <> ' ' ) or EOLN ;
           CH := TOUPPER ( CH ) ;
           if CH = ' ' then
             begin
               SY := EOLINE ;
               ZPOS := 0
             end (* then *)
           else
             begin
               if CH = '(' then
                 begin
                   SY := KLAMMAUF ;
                   NEXTCH ( LASTCH ) ;
                   ZPOS := ZPOS + 1
                 end (* then *)
               else
                 if CH = ')' then
                   begin
                     SY := KLAMMZU ;
                     NEXTCH ( LASTCH ) ;
                     ZPOS := ZPOS + 1
                   end (* then *)
                 else
                   if CH = '/' then
                     begin
                       SY := SCHRAEG ;
                       NEXTCH ( LASTCH ) ;
                       ZPOS := ZPOS + 1
                     end (* then *)
                   else
                     if CH in KOMMSET then
                       begin
                         KOMMCH := CH ;
                         SY := KOMMANDO ;
                         NEXTCH ( LASTCH ) ;
                         ZPOS := ZPOS + 1
                       end (* then *)
                     else
                       if ( CH >= '0' ) and ( CH <= '9' ) then
                         begin
                           SY := KNZAHL ;
                           LAENGE := 1 ;
                           ERGZAHL := ORD ( CH ) - ORD ( '0' ) ;
                           repeat
                             NEXTCH ( CH ) ;
                             ZPOS := ZPOS + 1 ;
                             if ( CH >= '0' ) and ( CH <= '9' ) then
                               begin
                                 ERGZAHL := ERGZAHL * 10 + ORD ( CH ) -
                                            ORD ( '0' ) ;
                                 LAENGE := LAENGE + 1
                               end (* then *)
                           until ( CH < '0' ) or ( CH > '9' ) or (
                           LAENGE > 8 ) ;
                           if LAENGE <= 8 then
                             LASTCH := CH
                           else
                             SY := ZUGROSS
                         end (* then *)
                       else
                         begin
                           SY := FALSCH ;
                           NEXTCH ( LASTCH ) ;
                           ZPOS := ZPOS + 1
                         end (* else *)
             end (* else *)
         end (* FIND *) ;


      procedure KETTEIN ( var ANKER : STPTYP ; PRUEFUNG : BOOLEAN ;
                        ZWKETTE : STPTYP ; var OK : BOOLEAN ) ;

      (****************************************************)
      (*   KETTE VON KONTENPUNKTEN EINLESEN. WENN DIE     *)
      (*   VARIABLE -PRUEFUNG- = TRUE IST, WIRD DIE       *)
      (*   KETTE GEPRUEFT. INNERHALB EINER KETTE MUSS     *)
      (*   EIN KNOTENPUNKT EINDEUTIG SEIN. ZUSAETZLICH    *)
      (*   KANN UEBER DEN PARAMETER ZWKETTE EINE ZWEITE   *)
      (*   KETTE UEBERGEBEN WERDEN. WENN DAS DER FALL     *)
      (*   IST, SO WIRD AUCH GEPRUEFT, OB DIE NEU         *)
      (*   EINGEGEBENEN KNOTENPUNKTE AUCH IN DER ZWEITEN  *)
      (*   KETTE NICHT VORHANDEN SIND. (SINNVOLL DANN,    *)
      (*   WENN EINE KETTE AN EINE ZWEITE ANGEFUEGT       *)
      (*   WERDEN SOLL)                                   *)
      (****************************************************)


         var X , Y : STPTYP ;
             TESTP : STPTYP ;
             SCHONDA : BOOLEAN ;
             ABBRUCH : BOOLEAN ;
             SIT : SITUATIONEN ;

         begin (* KETTEIN *)
           OK := TRUE ;
           ANKER := NIL ;
           repeat
             ABBRUCH := FALSE ;
             FIND ( SY , KOMMCH , ERGZAHL , LASTCH , ZPOS ) ;
             while SY = EOLINE do
               begin
                 WRITELN ( 'EINGABE UNVOLLSTAENDIG. BITTE FORTSETZEN:'
                           ) ;
                 ZPOS := 0 ;
                 READ ( LASTCH ) ;
                 FIND ( SY , KOMMCH , ERGZAHL , LASTCH , ZPOS )
               end (* while *) ;
             if SY = KNZAHL then
               begin
                 if ERGZAHL = 0 then
                   begin
                     ABBRUCH := TRUE ;
                     SFEHLER ( 11 )
                   end (* then *)
                 else
                   begin
                     SCHONDA := FALSE ;
                     if PRUEFUNG then
                       if ANKER <> NIL then
                         begin
                           TESTP := ANKER ;
                           while TESTP <> NIL do
                             begin
                               SCHONDA := SCHONDA or ( TESTP -> .
                                          VKPUNKT = ERGZAHL ) ;
                               TESTP := TESTP -> . NEXT
                             end (* while *)
                         end (* then *) ;
                     if ZWKETTE <> NIL then
                       begin
                         TESTP := ZWKETTE ;
                         while TESTP <> NIL do
                           begin
                             SCHONDA := SCHONDA or ( TESTP -> . VKPUNKT
                                        = ERGZAHL ) ;
                             TESTP := TESTP -> . NEXT
                           end (* while *)
                       end (* then *) ;
                     if SCHONDA then
                       begin
                         ABBRUCH := TRUE ;
                         SFEHLER ( 6 )
                       end (* then *)
                     else
                       begin
                         NEWSTP ( Y ) ;
                         if ANKER = NIL then
                           begin
                             ANKER := Y ;
                             X := Y
                           end (* then *)
                         else
                           begin
                             X -> . NEXT := Y ;
                             X := X -> . NEXT
                           end (* else *) ;
                         X -> . VKPUNKT := ERGZAHL ;
                         X -> . NEXT := NIL ;
                         X -> . DISTANZ := 0 ;
                         X -> . KAT := 0 ;
                         X -> . INNERORTS := FALSE ;
                         for SIT := SITMIN to SITMAX do
                           X -> . ZEITZUSCHL [ SIT ] := 0 ;
                         X -> . WERTEOK := FALSE ;
                         X -> . VKPGEPRUEFT := FALSE
                       end (* else *)
                   end (* else *)
               end (* then *)
           until ( SY <> KNZAHL ) or ABBRUCH ;
           if ABBRUCH then
             OK := FALSE
         end (* KETTEIN *) ;


      procedure SUCHKLAMA ( var OK : BOOLEAN ) ;

      (***********************************************)
      (*   KLAMMER-AUF SUCHEN INNERHALB S-KOMMANDO   *)
      (***********************************************)


         begin (* SUCHKLAMA *)
           OK := TRUE ;
           FIND ( SY , KOMMCH , ERGZAHL , LASTCH , ZPOS ) ;
           while SY = EOLINE do
             begin
               WRITELN ( 'EINGABE UNVOLLSTAENDIG. BITTE FORTSETZEN.' )
                         ;
               ZPOS := 0 ;
               READ ( LASTCH ) ;
               FIND ( SY , KOMMCH , ERGZAHL , LASTCH , ZPOS )
             end (* while *) ;
           if SY <> KLAMMAUF then
             begin
               SFEHLER ( 3 ) ;
               OK := FALSE
             end (* then *)
         end (* SUCHKLAMA *) ;


      procedure SUCHSCHRAEG ( var OK : BOOLEAN ) ;

      (*************************************************)
      (*   SCHRAEGSTRICH SUCHEN INNERHALB S-KOMMANDO   *)
      (*************************************************)


         begin (* SUCHSCHRAEG *)
           OK := TRUE ;
           FIND ( SY , KOMMCH , ERGZAHL , LASTCH , ZPOS ) ;
           while SY = EOLINE do
             begin
               WRITELN ( 'EINGABE UNVOLLSTAENDIG. BITTE FORTSETZEN.' )
                         ;
               ZPOS := 0 ;
               READ ( LASTCH ) ;
               FIND ( SY , KOMMCH , ERGZAHL , LASTCH , ZPOS )
             end (* while *) ;
           if SY <> SCHRAEG then
             begin
               SFEHLER ( 12 ) ;
               OK := FALSE
             end (* then *)
         end (* SUCHSCHRAEG *) ;


      procedure SUCHZAHL ( var OK : BOOLEAN ) ;

      (******************************************************)
      (*   ZAHL (KNOTENPUNKT) SUCHEN INNERHALB S-KOMMANDO   *)
      (******************************************************)


         begin (* SUCHZAHL *)
           OK := TRUE ;
           FIND ( SY , KOMMCH , ERGZAHL , LASTCH , ZPOS ) ;
           while SY = EOLINE do
             begin
               WRITELN ( 'EINGABE UNVOLLSTAENDIG. BITTE FORTSETZEN.' )
                         ;
               ZPOS := 0 ;
               READ ( LASTCH ) ;
               FIND ( SY , KOMMCH , ERGZAHL , LASTCH , ZPOS )
             end (* while *) ;
           if SY <> KNZAHL then
             begin
               SFEHLER ( 13 ) ;
               OK := FALSE
             end (* then *)
         end (* SUCHZAHL *) ;


      function KETTLAENGE ( X : STPTYP ) : INTEGER ;

      (**************************************)
      (*   ERMITTELT DIE LAENGE DER KETTE   *)
      (**************************************)


         var K : INTEGER ;

         begin (* KETTLAENGE *)
           K := 0 ;
           while X <> NIL do
             begin
               X := X -> . NEXT ;
               K := K + 1
             end (* while *) ;
           KETTLAENGE := K
         end (* KETTLAENGE *) ;


      procedure SUCHTEILSTR ( GESAMT : STPTYP ; KETTE1 : STPTYP ; var
                            DANF : STPTYP ; var DWORK : STPTYP ; var OK
                            : BOOLEAN ) ;

      (******************************************)
      (*                                        *)
      (*   SUCHT TEILKETTE IN GEGEBENER KETTE   *)
      (*                                        *)
      (*   BEDEUTUNG DER PARAMETER:             *)
      (*                                        *)
      (*   GESAMT: ANKER DER ZU DURCHSUCHENDEN  *)
      (*           KETTE                        *)
      (*   KETTE1: ANKER DER GESUCHTEN TEIL-    *)
      (*           KETTE                        *)
      (*   DANF:   ANFANG DER GEFUNDENEN TEIL-  *)
      (*           KETTE (NUR BEI OK=TRUE)      *)
      (*   DWORK:  ENDE DER GEFUNDENEN TEIL-    *)
      (*           KETTE (NUR BEI OK=TRUE)      *)
      (*   OK:     TEILKETTE VORHANDEN (J/N)    *)
      (*                                        *)
      (******************************************)


         var X : STPTYP ;

         begin (* SUCHTEILSTR *)
           OK := TRUE ;
           if GESAMT = NIL then
             begin
               SFEHLER ( 8 ) ;
               OK := FALSE
             end (* then *)
           else
             begin
               DWORK := GESAMT ;
               DANF := NIL ;
               while ( DWORK -> . VKPUNKT <> KETTE1 -> . VKPUNKT ) and
               ( DWORK -> . NEXT <> NIL ) do
                 begin
                   DANF := DWORK ;
                   DWORK := DWORK -> . NEXT
                 end (* while *) ;
               if DWORK -> . VKPUNKT <> KETTE1 -> . VKPUNKT then
                 begin
                   SFEHLER ( 9 ) ;
                   OK := FALSE
                 end (* then *)
               else
                 begin
                   X := KETTE1 ;
                   while ( DWORK -> . VKPUNKT = X -> . VKPUNKT ) and (
                   DWORK -> . NEXT <> NIL ) and ( X -> . NEXT <> NIL )
                   do
                     begin
                       DWORK := DWORK -> . NEXT ;
                       X := X -> . NEXT
                     end (* while *) ;
                   if DWORK -> . VKPUNKT <> X -> . VKPUNKT then
                     begin
                       SFEHLER ( 9 ) ;
                       OK := FALSE
                     end (* then *)
                   else
                     if X -> . NEXT <> NIL then
                       begin
                         SFEHLER ( 9 ) ;
                         OK := FALSE
                       end (* then *)
                 end (* else *)
             end (* else *)
         end (* SUCHTEILSTR *) ;


      procedure TKETTLOESCH ;

      (****************************************************)
      (*   LOESCHT GEGEBENE TEILKETTE (FALLS VORHANDEN)   *)
      (*   DIE FREIWERDENDEN ELEMENTE WERDEN DER SPEI-    *)
      (*   CHERVERWALTUNG WIEDER ZUR VERFUEGUNG GESTELLT. *)
      (****************************************************)


         var LANF , DANF , DWORK : STPTYP ;
             SIT : SITUATIONEN ;

         begin (* TKETTLOESCH *)
           if KETTLAENGE ( KETTE1 ) > KETTLAENGE ( STR . ANFANG ) - 2
           then
             begin
               SFEHLER ( 10 ) ;
               OK := FALSE
             end (* then *)
           else
             begin
               SUCHTEILSTR ( STR . ANFANG , KETTE1 , DANF , DWORK , OK
                             ) ;
               if OK then
                 begin
                   if DWORK <> STR . ENDE then
                     begin
                       if DANF = NIL then
                         begin
                           LANF := STR . ANFANG ;
                           STR . ANFANG := DWORK -> . NEXT ;
                           DWORK -> . NEXT := NIL ;
                           DISPKETTE ( LANF )
                         end (* then *)
                       else
                         begin
                           LANF := DANF -> . NEXT ;
                           DANF -> . NEXT := DWORK -> . NEXT ;
                           DANF -> . WERTEOK := FALSE ;
                           DANF -> . DISTANZ := 0 ;
                           DANF -> . KAT := 0 ;
                           DANF -> . INNERORTS := FALSE ;
                           for SIT := SITMIN to SITMAX do
                             DANF -> . ZEITZUSCHL [ SIT ] := 0 ;
                           DWORK -> . NEXT := NIL ;
                           DISPKETTE ( LANF )
                         end (* else *)
                     end (* then *)
                   else
                     begin
                       LANF := DANF -> . NEXT ;
                       DANF -> . NEXT := NIL ;
                       DANF -> . WERTEOK := TRUE ;
                       DANF -> . DISTANZ := 0 ;
                       DANF -> . KAT := 0 ;
                       DANF -> . INNERORTS := FALSE ;
                       for SIT := SITMIN to SITMAX do
                         DANF -> . ZEITZUSCHL [ SIT ] := 0 ;
                       STR . ENDE := DANF ;
                       DISPKETTE ( LANF )
                     end (* else *)
                 end (* then *)
             end (* else *)
         end (* TKETTLOESCH *) ;


      procedure TKETTMODIF ;

      (*********************************************************)
      (*   KENNZEICHNET GEGEBENE TEILKETTE FUER MODIFIKATION   *)
      (*                                                       *)
      (*   DIE WERTE IN DER TEILKETTE SOLLEN GEAENDERT WERDEN. *)
      (*   DAZU WIRD DIE VARIABLE WERTEOK AUF FALSE GESETZT.   *)
      (*   IN DER PROZEDUR STRWERTEIN WERDEN DANN DIE NEUEN    *)
      (*   WERTE VOM BENUTZER EINGEGEBEN.                      *)
      (*********************************************************)


         var DANF , DWORK : STPTYP ;

         begin (* TKETTMODIF *)
           if KETTLAENGE ( KETTE1 ) > KETTLAENGE ( STR . ANFANG ) then
             begin
               SFEHLER ( 10 ) ;
               OK := FALSE
             end (* then *)
           else
             begin
               SUCHTEILSTR ( STR . ANFANG , KETTE1 , DANF , DWORK , OK
                             ) ;
               if OK then
                 begin
                   if DANF = NIL then
                     DANF := STR . ANFANG
                   else
                     DANF := DANF -> . NEXT ;
                   while DANF <> DWORK do
                     begin
                       DANF -> . WERTEOK := FALSE ;
                       DANF := DANF -> . NEXT
                     end (* while *)
                 end (* then *)
             end (* else *)
         end (* TKETTMODIF *) ;


      procedure SKOMMN ;

      (******************************************************)
      (*                                                    *)
      (*   S-KOMMANDO N                                     *)
      (*                                                    *)
      (*   FORMAT:    N  <LISTE>                            *)
      (*                                                    *)
      (*   WOBEI <LISTE> EINE IN RUNDE KLAMMERN EINGE-      *)
      (*   SCHLOSSENE LISTE VON KNOTENPUNKTEN IST.          *)
      (*                                                    *)
      (*   DIE IN BEARBEITUNG BEFINDLICHE STRASSE WIRD      *)
      (*   MIT DER LISTE <LISTE> NEU ANGELEGT.              *)
      (*                                                    *)
      (******************************************************)


         begin (* SKOMMN *)
           KETTE1 := NIL ;
           SUCHKLAMA ( OK ) ;
           if OK then
             begin
               KETTEIN ( KETTE1 , TRUE , NIL , OK ) ;
               if OK then
                 begin
                   OK := ( SY = KLAMMZU ) ;
                   if not OK then
                     SFEHLER ( 4 )
                   else
                     begin
                       if KETTE1 = NIL then
                         begin
                           SFEHLER ( 5 ) ;
                           OK := FALSE
                         end (* then *)
                       else
                         if KETTE1 -> . NEXT = NIL then
                           begin
                             SFEHLER ( 7 ) ;
                             OK := FALSE
                           end (* then *)
                         else
                           begin
                             DISPKETTE ( STR . ANFANG ) ;
                             STR . ANFANG := KETTE1 ;
                             X := KETTE1 ;
                             while X -> . NEXT <> NIL do
                               X := X -> . NEXT ;
                             STR . ENDE := X ;
                             KETTE1 := NIL
                           end (* else *)
                     end (* else *)
                 end (* then *)
             end (* then *) ;
           DISPKETTE ( KETTE1 )
         end (* SKOMMN *) ;


      procedure SKOMMG ;

      (******************************************************)
      (*                                                    *)
      (*   S-KOMMANDO G                                     *)
      (*                                                    *)
      (*   FORMAT:    G                                     *)
      (*                                                    *)
      (*   DIE IN BEARBEITUNG BEFINDLICHE STRASSE WIRD      *)
      (*   MIT DEN DATEN DER GEGENRICHTUNG NEU ANGELEGT.    *)
      (*                                                    *)
      (******************************************************)


         var STRGEGEN : STRASSE ;
             KEN : CHAR ;
             STRNUMMER : INTEGER ;
             RICHT : CHAR ;
             SIND : STRTYP ;
             STATUS : STAT ;
             P , Q , PNEU : STPTYP ;
             SIT : SITUATIONEN ;

         begin (* SKOMMG *)
           STRGEGEN := STR ;
           if STRGEGEN . RICHTUNG = HIN then
             begin
               STRGEGEN . RICHTUNG := RUECK ;
               RICHT := 'R'
             end (* then *)
           else
             begin
               STRGEGEN . RICHTUNG := HIN ;
               RICHT := 'H'
             end (* else *) ;
           KEN := STRGEGEN . KENNUNG ;
           STRNUMMER := STRGEGEN . NUMMER ;
           SEARCHSTR ( KEN , STRNUMMER , RICHT , SIND , STATUS ) ;
           OK := TRUE ;
           if STATUS <> 0 then
             begin
               SFEHLER ( 15 ) ;
               OK := FALSE
             end (* then *)
           else
             begin
               NEU := FALSE ;
               KETTE1 := STR . ANFANG ;
               DISPKETTE ( KETTE1 ) ;
               STR . ENDE := NIL ;
               STRGEGEN := STRASSENNETZ [ SIND ] ;
               P := STRGEGEN . ANFANG ;
               Q := P -> . NEXT ;
               while Q -> . NEXT <> NIL do
                 begin
                   P := Q ;
                   Q := Q -> . NEXT
                 end (* while *) ;
               NEWSTP ( STR . ANFANG ) ;
               PNEU := STR . ANFANG ;
               PNEU -> := P -> ;
               PNEU -> . VKPUNKT := Q -> . VKPUNKT ;
               PNEU -> . WERTEOK := FALSE ;
               while P <> STRGEGEN . ANFANG do
                 begin
                   Q := P ;
                   P := STRGEGEN . ANFANG ;
                   while P -> . NEXT <> Q do
                     P := P -> . NEXT ;
                   NEWSTP ( PNEU -> . NEXT ) ;
                   PNEU := PNEU -> . NEXT ;
                   PNEU -> := P -> ;
                   PNEU -> . VKPUNKT := Q -> . VKPUNKT ;
                   PNEU -> . WERTEOK := FALSE ;
                 end (* while *) ;
               NEWSTP ( PNEU -> . NEXT ) ;
               PNEU := PNEU -> . NEXT ;
               STR . ENDE := PNEU ;
               PNEU -> . VKPUNKT := P -> . VKPUNKT ;
               PNEU -> . NEXT := NIL ;
               PNEU -> . KAT := 0 ;
               PNEU -> . INNERORTS := FALSE ;
               for SIT := SITMIN to SITMAX do
                 PNEU -> . ZEITZUSCHL [ SIT ] := 0 ;
               PNEU -> . DISTANZ := 0 ;
               PNEU -> . WERTEOK := TRUE ;
               PNEU -> . VKPGEPRUEFT := TRUE ;
             end (* else *)
         end (* SKOMMG *) ;


      procedure SKOMMA ;

      (******************************************************)
      (*                                                    *)
      (*   S-KOMMANDO A                                     *)
      (*                                                    *)
      (*   FORMAT:    A  <LISTE>                            *)
      (*                                                    *)
      (*   WOBEI <LISTE> EINE IN RUNDE KLAMMERN EINGE-      *)
      (*   SCHLOSSENE LISTE VON KNOTENPUNKTEN IST.          *)
      (*                                                    *)
      (*   DIE LISTE <LISTE> WIRD AN DIE AKTUELLE LISTE     *)
      (*   ANGEHAENGT (HINTEN).                             *)
      (*                                                    *)
      (******************************************************)


         begin (* SKOMMA *)
           KETTE1 := NIL ;
           SUCHKLAMA ( OK ) ;
           if OK then
             begin
               KETTEIN ( KETTE1 , TRUE , STR . ANFANG , OK ) ;
               if OK then
                 begin
                   OK := ( SY = KLAMMZU ) ;
                   if not OK then
                     SFEHLER ( 4 )
                   else
                     begin
                       if KETTE1 = NIL then
                         begin
                           SFEHLER ( 5 ) ;
                           OK := FALSE
                         end (* then *)
                       else
                         begin
                           if STR . ANFANG <> NIL then
                             begin
                               STR . ENDE -> . NEXT := KETTE1 ;
                               STR . ENDE -> . WERTEOK := FALSE ;
                               X := KETTE1 ;
                               while X -> . NEXT <> NIL do
                                 X := X -> . NEXT ;
                               STR . ENDE := X
                             end (* then *)
                           else
                             begin
                               STR . ANFANG := KETTE1 ;
                               X := KETTE1 ;
                               while X -> . NEXT <> NIL do
                                 X := X -> . NEXT ;
                               STR . ENDE := X
                             end (* else *) ;
                           KETTE1 := NIL
                         end (* else *)
                     end (* else *)
                 end (* then *)
             end (* then *) ;
           DISPKETTE ( KETTE1 )
         end (* SKOMMA *) ;


      procedure SKOMMP ;

      (******************************************************)
      (*                                                    *)
      (*   S-KOMMANDO P                                     *)
      (*                                                    *)
      (*   FORMAT:    P  <LISTE>                            *)
      (*                                                    *)
      (*   WOBEI <LISTE> EINE IN RUNDE KLAMMERN EINGE-      *)
      (*   SCHLOSSENE LISTE VON KNOTENPUNKTEN IST.          *)
      (*                                                    *)
      (*   DIE LISTE <LISTE> WIRD DER AKTUELLEN LISTE       *)
      (*   VORANGESTELLT.                                   *)
      (*                                                    *)
      (******************************************************)


         begin (* SKOMMP *)
           KETTE1 := NIL ;
           SUCHKLAMA ( OK ) ;
           if OK then
             begin
               KETTEIN ( KETTE1 , TRUE , STR . ANFANG , OK ) ;
               if OK then
                 begin
                   OK := ( SY = KLAMMZU ) ;
                   if not OK then
                     SFEHLER ( 4 )
                   else
                     begin
                       if KETTE1 = NIL then
                         begin
                           SFEHLER ( 5 ) ;
                           OK := FALSE
                         end (* then *)
                       else
                         begin
                           if STR . ANFANG <> NIL then
                             begin
                               X := KETTE1 ;
                               while X -> . NEXT <> NIL do
                                 X := X -> . NEXT ;
                               X -> . NEXT := STR . ANFANG ;
                               STR . ANFANG := KETTE1
                             end (* then *)
                           else
                             begin
                               STR . ANFANG := KETTE1 ;
                               X := KETTE1 ;
                               while X -> . NEXT <> NIL do
                                 X := X -> . NEXT ;
                               STR . ENDE := X
                             end (* else *)
                         end (* else *) ;
                       KETTE1 := NIL
                     end (* else *)
                 end (* then *)
             end (* then *) ;
           DISPKETTE ( KETTE1 )
         end (* SKOMMP *) ;


      procedure SKOMMD ;

      (******************************************************)
      (*                                                    *)
      (*   S-KOMMANDO D                                     *)
      (*                                                    *)
      (*   FORMAT:    D  <LISTE>                            *)
      (*                                                    *)
      (*   WOBEI <LISTE> EINE IN RUNDE KLAMMERN EINGE-      *)
      (*   SCHLOSSENE LISTE VON KNOTENPUNKTEN IST.          *)
      (*                                                    *)
      (*   DIE TEILLISTE <LISTE> WIRD - SOFERN VORHANDEN -  *)
      (*   AUS DER AKTUELLEN LISTE GELOESCHT.               *)
      (*                                                    *)
      (******************************************************)


         begin (* SKOMMD *)
           KETTE1 := NIL ;
           SUCHKLAMA ( OK ) ;
           if OK then
             begin
               KETTEIN ( KETTE1 , TRUE , NIL , OK ) ;
               if OK then
                 begin
                   OK := ( SY = KLAMMZU ) ;
                   if not OK then
                     SFEHLER ( 4 )
                   else
                     begin
                       if KETTE1 = NIL then
                         begin
                           SFEHLER ( 5 ) ;
                           OK := FALSE
                         end (* then *)
                       else
                         TKETTLOESCH
                     end (* else *)
                 end (* then *)
             end (* then *) ;
           DISPKETTE ( KETTE1 )
         end (* SKOMMD *) ;


      procedure SKOMMI ;

      (******************************************************)
      (*                                                    *)
      (*   S-KOMMANDO I                                     *)
      (*                                                    *)
      (*   FORMAT:    I  ( <KNOTEN> / <EINFUEGUNG> )        *)
      (*                                                    *)
      (*   <KNOTEN>     : EIN KNOTENPUNKT                   *)
      (*   <EINFUEGUNG> : MEHRERE KNOTENPUNKTE              *)
      (*                                                    *)
      (*   DIE KNOTEN AUS <EINFUEGUNG> WERDEN IN DER        *)
      (*   AKTUELLEN LISTE HINTER DEM KNOTEN <KNOTEN>       *)
      (*   - FALLS VORHANDEN UND ERLAUBT - EINGEFUEGT.      *)
      (*                                                    *)
      (******************************************************)


         var SKNOTEN : INTEGER ;
             Y : STPTYP ;
             SIT : SITUATIONEN ;

         begin (* SKOMMI *)
           KETTE1 := NIL ;
           SUCHKLAMA ( OK ) ;
           if OK then
             begin
               SUCHZAHL ( OK ) ;
               if OK then
                 begin
                   SKNOTEN := ERGZAHL ;
                   X := STR . ANFANG ;
                   OK := FALSE ;
                   while ( X <> NIL ) and not OK do
                     begin
                       OK := ( X -> . VKPUNKT = SKNOTEN ) ;
                       if not OK then
                         X := X -> . NEXT
                     end (* while *) ;
                   if not OK then
                     SFEHLER ( 14 )
                   else
                     begin
                       SUCHSCHRAEG ( OK ) ;
                       if OK then
                         begin
                           KETTEIN ( KETTE1 , TRUE , STR . ANFANG , OK
                                     ) ;
                           if OK then
                             begin
                               OK := ( SY = KLAMMZU ) ;
                               if not OK then
                                 SFEHLER ( 4 )
                               else
                                 begin
                                   if KETTE1 = NIL then
                                     begin
                                       SFEHLER ( 5 ) ;
                                       OK := FALSE
                                     end (* then *)
                                   else
                                     begin
                                       X -> . WERTEOK := FALSE ;
                                       X -> . DISTANZ := 0 ;
                                       X -> . KAT := 0 ;
                                       X -> . INNERORTS := FALSE ;
                                       for SIT := SITMIN to SITMAX do
                                         X -> . ZEITZUSCHL [ SIT ] := 0
                                                   ;
                                       Y := X -> . NEXT ;
                                       X -> . NEXT := KETTE1 ;
                                       while X -> . NEXT <> NIL do
                                         X := X -> . NEXT ;
                                       X -> . NEXT := Y ;
                                       if X -> . NEXT = NIL then
                                         STR . ENDE := X ;
                                       KETTE1 := NIL
                                     end (* else *)
                                 end (* else *)
                             end (* then *)
                         end (* then *)
                     end (* else *)
                 end (* then *)
             end (* then *) ;
           DISPKETTE ( KETTE1 )
         end (* SKOMMI *) ;


      procedure SKOMMC ;

      (******************************************************)
      (*                                                    *)
      (*   S-KOMMANDO C                                     *)
      (*                                                    *)
      (*   FORMAT:    C  <LISTE>                            *)
      (*                                                    *)
      (*   WOBEI <LISTE> EINE IN RUNDE KLAMMERN EINGE-      *)
      (*   SCHLOSSENE LISTE VON KNOTENPUNKTEN IST.          *)
      (*                                                    *)
      (*   DIE WERTE DER TEILLISTE <LISTE> WERDEN           *)
      (*   - SOFERN VORHANDEN - FUER DIE AENDERUNG          *)
      (*   MARKIERT.                                        *)
      (*                                                    *)
      (******************************************************)


         begin (* SKOMMC *)
           KETTE1 := NIL ;
           SUCHKLAMA ( OK ) ;
           if OK then
             begin
               KETTEIN ( KETTE1 , TRUE , NIL , OK ) ;
               if OK then
                 begin
                   OK := ( SY = KLAMMZU ) ;
                   if not OK then
                     SFEHLER ( 4 )
                   else
                     begin
                       if KETTE1 = NIL then
                         begin
                           SFEHLER ( 5 ) ;
                           OK := FALSE
                         end (* then *)
                       else
                         TKETTMODIF
                     end (* else *)
                 end (* then *)
             end (* then *) ;
           DISPKETTE ( KETTE1 )
         end (* SKOMMC *) ;


      begin (* SKOMM *)
        KOMMSET := [ 'X' , 'G' , 'P' , 'N' , 'A' , 'D' , 'I' , 'C' ,
                   'E' , '?' ] ;
        ENDE := FALSE ;
        repeat
          OK := FALSE ;
          ZPOS := 0 ;
          READ ( LASTCH ) ;
          FIND ( SY , KOMMCH , ERGZAHL , LASTCH , ZPOS ) ;
          if SY = EOLINE then
          begin
            OK := TRUE ; readln
          end
          else
            begin
              if SY <> KOMMANDO then
              begin
                SFEHLER ( 2 ) ; readln
              end
              else
                begin
                  case KOMMCH of
                    '?' : HILFE ;
                    'P' : SKOMMP ;
                    'N' : SKOMMN ;
                    'A' : SKOMMA ;
                    'D' : SKOMMD ;
                    'I' : SKOMMI ;
                    'C' : SKOMMC ;
                    'G' : SKOMMG ;
                    'E' : begin
                            OK := TRUE ;
                            ENDE := TRUE
                          end (* tag/ca *) ;
                    'X' : begin
                            OK := TRUE ;
                            ABBRUCH := TRUE
                          end (* tag/ca *)
                  end (* case *) ;
                  READLN
                end (* else *)
            end (* else *)
        until OK ;
      end (* SKOMM *) ;


   procedure STRWERTEIN ( var STR : STRASSE ; var OK : BOOLEAN ; NEU :
                        BOOLEAN ) ;

   (**********************************************************)
   (*                                                        *)
   (*   STRASSENWERTE EINLESEN.                              *)
   (*                                                        *)
   (*   FUER ALLE DIEJENIGEN STRASSENABSCHNITTE, DIE SICH    *)
   (*   IN IRGENDEINER WEISE GEAENDERT HABEN, MUESSEN        *)
   (*   NEUE WERTE EINGEGEBEN WERDEN. DIESER SACHVERHALT     *)
   (*   KANN UEBER DIE FELDER -VKPGEPRUEFT- BZW. -WERTEOK-   *)
   (*   FESTGESTELLT WERDEN.                                 *)
   (*                                                        *)
   (*   IM EINZELNEN WIRD FOLGENDES GEMACHT:                 *)
   (*                                                        *)
   (*   1.) BEI -VKPGEPRUEFT- = FALSE:                       *)
   (*       ES WIRD GEPRUEFT, OB DIE KNOTENPUNKTE            *)
   (*       BEREITS DEFINIERT SIND. WENN NICHT, WERDEN       *)
   (*       DIE KNOTENPUNKTE NEU ANGELEGT; DIE DAZU          *)
   (*       NOTWENDIGEN ANGABEN (KOORDINATEN) WERDEN         *)
   (*       VOM BEDIENER ANGEFORDERT.                        *)
   (*                                                        *)
   (*   2.) DIE ANGABEN ZU DISTANZ, KATEGORIE USW.           *)
   (*       WERDEN FUER JEDEN STRECKENABSCHNITT NEU          *)
   (*       ANGEFORDERT, FUER DEN -WERTEOK- = FALSE IST.     *)
   (*                                                        *)
   (**********************************************************)


      var LAUF : STPTYP ;
          IND : VPTYP ;
          STATUS : STAT ;
          KATEG : INTEGER ;
          T : HTEXT ;
          L : INTEGER ;
          TESTCH : CHAR ;
          EINGABEOK : BOOLEAN ;
          VPNUMMER : INTEGER ;
          VPNUMVON : INTEGER ;
          VPNUMBIS : INTEGER ;
          SIT : SITUATIONEN ;

      begin (* STRWERTEIN *)
        CLRSCRN ;
        WRITELN ( 'UEBERPRUEFEN DER STRASSENDATEN AUF ' ,
                  'VOLLSTAENDIGKEIT' ) ;
        WRITELN ;
        OK := TRUE ;
        if STR . ANFANG = NIL then
          OK := FALSE
        else
          if STR . ANFANG = STR . ENDE then
            OK := FALSE
          else
            if STR . ENDE = NIL then
              begin
                WRITELN ( 'PROGRAMMFEHLER' ) ;
                HALT
              end (* then *)
            else
              begin
                LAUF := STR . ANFANG ;
                while LAUF <> NIL do
                  begin
                    if not LAUF -> . VKPGEPRUEFT then
                      begin
                        VPNUMMER := LAUF -> . VKPUNKT ;
                        SEARCHVP ( VPNUMMER , IND , STATUS ) ;
                        if STATUS = 0 then
                          WRITELN ( 'KNOTENPUNKT ' , VPNUMMER : STELLEN
                                    ( VPNUMMER ) , ' SCHON VORHANDEN' )
                        else
                          begin
                            WRITELN ( 'KNOTENPUNKT ' , VPNUMMER :
                                      STELLEN ( VPNUMMER ) ,
                                      ' WIRD NEU ANGELEGT' ) ;
                            INSERTVP ( VPNUMMER , STATUS ) ;
                            if STATUS <> 0 then
                              begin
                                WRITELN ( 'FEHLER' , STATUS ,
                                          ' BEI VKP-ANLAGE, ABBRUCH!' )
                                          ;
                                HALT
                              end (* then *)
                          end (* else *) ;
                        LAUF -> . VKPGEPRUEFT := TRUE
                      end (* then *) ;
                    LAUF := LAUF -> . NEXT
                  end (* while *) ;
                WRITELN ;
                LAUF := STR . ANFANG ;
                while LAUF <> NIL do
                  begin
                    if not LAUF -> . WERTEOK then
                      begin
                        VPNUMVON := LAUF -> . VKPUNKT ;
                        VPNUMBIS := LAUF -> . NEXT -> . VKPUNKT ;
                        WRITELN ( 'BITTE WERTE FUER STRECKE VON ' ,
                                  VPNUMVON : STELLEN ( VPNUMVON ) ,
                                  ' NACH ' , VPNUMBIS : STELLEN (
                                  VPNUMBIS ) , ' EINGEBEN.' ) ;
                        WRITE ( 'DISTANZ:' ) ;
                        if not NEU then
                          WRITE ( ' ' : 20 , '(ALTER WERT: ' , LAUF ->
                                  . DISTANZ : STELLEN ( LAUF -> .
                                  DISTANZ ) , ')' ) ;
                        WRITELN ;
                        IEIN2 ( X , ELAENGE ) ;
                        if NEU or ( ELAENGE <> 0 ) then
                          LAUF -> . DISTANZ := X ;
                        repeat
                          WRITE ( 'KATEGORIE:' ) ;
                          if not NEU then
                            WRITE ( ' ' : 18 , '(ALTER WERT: ' , LAUF
                                    -> . KAT : STELLEN ( LAUF -> . KAT
                                    ) , ')' ) ;
                          WRITELN ;
                          KATEG := LAUF -> . KAT ;
                          IEIN2 ( X , ELAENGE ) ;
                          if NEU or ( ELAENGE <> 0 ) then
                            KATEG := X ;
                          if not ( KATEG in [ 1 .. KATMAX ] ) then
                            WRITELN (
                                'FALSCHE EINGABE, BITTE WIEDERHOLEN !'
                                      )
                        until KATEG in [ 1 .. KATMAX ] ;
                        LAUF -> . KAT := KATEG ;
                        WRITE ( 'INNERORTS / AUSSERORTS ?' ) ;
                        if not NEU then
                          begin
                            WRITE ( ' ' : 4 ) ;
                            WRITE ( '(ALTER WERT: ' ) ;
                            if LAUF -> . INNERORTS then
                              WRITE ( 'I' )
                            else
                              WRITE ( 'A' ) ;
                            WRITE ( ')' )
                          end (* then *) ;
                        WRITELN ;
                        repeat
                          TEIN ( T , L ) ;
                          TESTCH := T [ 1 ] ;
                          EINGABEOK := ( L <= 1 ) and ( ( TESTCH = 'A'
                                       ) or ( TESTCH = 'I' ) or ( (
                                       TESTCH = ' ' ) and not NEU ) ) ;
                          if not EINGABEOK then
                            begin
                              WRITELN (
                            'EINGABE FALSCH, NUR -I- UND -A- ERLAUBT.'
                                        ) ;
                              WRITELN ( 'BITTE EINGABE WIEDERHOLEN.' )
                            end (* then *)
                        until EINGABEOK ;
                        if TESTCH = 'I' then
                          LAUF -> . INNERORTS := TRUE ;
                        if TESTCH = 'A' then
                          LAUF -> . INNERORTS := FALSE ;
                        for SIT := SITMIN to SITMAX do
                          begin
                            WRITE ( 'ZEITZUSCHLAG(' , SITTEXTE [ SIT ]
                                    , '):' ) ;
                            if not NEU then
                              WRITE ( '       (ALTER WERT: ' , LAUF ->
                                      . ZEITZUSCHL [ SIT ] : STELLEN (
                                      LAUF -> . ZEITZUSCHL [ SIT ] ) ,
                                      ')' ) ;
                            WRITELN ;
                            IEIN2 ( X , ELAENGE ) ;
                            if NEU or ( ELAENGE <> 0 ) then
                              LAUF -> . ZEITZUSCHL [ SIT ] := X
                          end (* for *) ;
                        LAUF -> . WERTEOK := TRUE
                      end (* then *) ;
                    LAUF := LAUF -> . NEXT
                  end (* while *)
              end (* else *)
      end (* STRWERTEIN *) ;


   begin (* STRBEARB *)
     repeat
       ENDE := FALSE ;
       ABBRUCH := FALSE ;
       CLRSCRN ;
       STRBEKOPF ( STR ) ;
       WRITELN ;
       STRAUSGABE ( STR ) ;
       WRITELN ;
       WRITELN ( 'BITTE S-KOMMANDO EINGEBEN:' ) ;
       SKOMM ( STR , ENDE , ABBRUCH ) ;
       if ABBRUCH then
         begin
           ENDE := TRUE ;
           OK := FALSE
         end (* then *)
       else
         if ENDE then
           begin
             STRWERTEIN ( STR , OK , NEU ) ;
             if not OK then
               begin
                 WRITE ( 'STRASSE FORMAL NICHT KORREKT. ' ) ;
                 WRITELN ( 'BEARBEITUNG FORTSETZEN.' ) ;
                 ENDE := FALSE
               end (* then *) ;
             QUITTIEREN
           end (* then *)
     until ENDE ;
     CLRSCRN ;
     WRITE ( 'ENDE BEARBEITUNG DER STRASSE ' ) ;
     SBEZAUSG ( OUTPUT , STR , 0 ) ;
     WRITELN ;
     WRITELN
   end (* STRBEARB *) ;



procedure INSERTSTR ( KEN : CHAR ; STRNUMMER : INTEGER ; RICHT : CHAR ;
                    var STATUS : STAT ) ;

(******************************)
(*   FUEGT EINE STRASSE EIN   *)
(******************************)


   var I , J : INTEGER ;
       SHILF : STRASSE ;
       OK : BOOLEAN ;

   begin (* INSERTSTR *)
     SHILF . KENNUNG := KEN ;
     SHILF . NUMMER := STRNUMMER ;
     SHILF . ANFANG := NIL ;
     SHILF . ENDE := NIL ;
     if RICHT = 'H' then
       SHILF . RICHTUNG := HIN
     else
       SHILF . RICHTUNG := RUECK ;
     if STRZAHL >= STRMAX - 1 then
       STATUS := 2
     else
       begin
         I := 1 ;
         while KLEINERSTR ( STRASSENNETZ [ I ] , SHILF ) and ( I <=
         STRZAHL ) do
           I := I + 1 ;
         if I > STRZAHL then
           begin
             STRASSENNETZ [ I ] := SHILF ;
             STRZAHL := STRZAHL + 1 ;
             STATUS := 0
           end (* then *)
         else
           if not GLEICHSTR ( STRASSENNETZ [ I ] , SHILF ) then
             begin
               for J := STRZAHL DOWNTO I do
                 STRASSENNETZ [ J + 1 ] := STRASSENNETZ [ J ] ;
               STRASSENNETZ [ I ] := SHILF ;
               STRZAHL := STRZAHL + 1 ;
               STATUS := 0
             end (* then *)
           else
             STATUS := 1 ;
         if STATUS = 0 then
           begin
             STRBEARB ( STRASSENNETZ [ I ] , OK , TRUE ) ;
             if not OK then
               begin
                 STATUS := 3 ;
                 if I = STRZAHL then
                   STRZAHL := STRZAHL - 1
                 else
                   begin
                     for J := I to ( STRZAHL - 1 ) do
                       STRASSENNETZ [ J ] := STRASSENNETZ [ J + 1 ] ;
                     STRZAHL := STRZAHL - 1
                   end (* else *)
               end (* then *)
           end (* then *)
       end (* else *)
   end (* INSERTSTR *) ;



procedure VPEIN ( VPNUMMER : INTEGER ) ;

(*****************************)
(*   KNOTENPUNKT EINFUEGEN   *)
(*****************************)


   var STATUS : STAT ;

   begin (* VPEIN *)
     INSERTVP ( VPNUMMER , STATUS ) ;
     case STATUS of
       0 : WRITELN ( 'KNOTENPUNKT AUFGENOMMEN' ) ;
       1 : WRITELN ( 'KNOTENPUNKT SCHON VORHANDEN' ) ;
       2 : WRITELN ( 'UEBERLAUF: FELDER ZU KLEIN DIMENSIONIERT' ) ;
     end (* case *)
   end (* VPEIN *) ;



procedure ANSEIN ( BNUMMER : INTEGER ; VPNUMMER : INTEGER ) ;

(***************************)
(*   ANSCHLUSS EINFUEGEN   *)
(***************************)


   var STATUS : STAT ;

   begin (* ANSEIN *)
     INSERTANS ( BNUMMER , VPNUMMER , STATUS ) ;
     case STATUS of
       0 : WRITELN ( 'ANSCHLUSS AUFGENOMMEN' ) ;
       1 : WRITELN ( 'ANSCHLUSS SCHON VORHANDEN' ) ;
       2 : WRITELN ( 'BEZIRK NICHT VORHANDEN' ) ;
       3 : WRITELN ( 'KNOTENPUNKT NICHT VORHANDEN' ) ;
     end (* case *)
   end (* ANSEIN *) ;



procedure STREIN ( KEN : CHAR ; STRNUMMER : INTEGER ; RICHT : CHAR ) ;

(*************************)
(*   STRASSE EINFUEGEN   *)
(*************************)


   var STATUS : STAT ;

   begin (* STREIN *)
     INSERTSTR ( KEN , STRNUMMER , RICHT , STATUS ) ;
     case STATUS of
       0 : WRITELN ( 'STRASSE AUFGENOMMEN' ) ;
       1 : WRITELN ( 'STRASSE SCHON VORHANDEN' ) ;
       2 : WRITELN ( 'UEBERLAUF; FELDER ZU KLEIN DIMENSIONIERT' ) ;
       3 : WRITELN ( 'ABBRUCH DURCH BENUTZER; STRASSE WIRD ENTFERNT' )
     end (* case *)
   end (* STREIN *) ;



procedure VPEINGABE ;

(********************************)
(*   STEUERUNG KNOTEN-EINGABE   *)
(********************************)


   var VPNUMMER : INTEGER ;

   begin (* VPEINGABE *)
     repeat
       CLRSCRN ;
       WRITELN ( 'EINGABE VON KNOTENPUNKTEN' ) ;
       WRITELN ;
       WRITELN ;
       WRITELN ( 'NUMMER DES NEUEN KNOTENPUNKTES:' ) ;
       IEIN ( VPNUMMER ) ;
       if VPNUMMER <> 0 then
         begin
           VPEIN ( VPNUMMER ) ;
           LEERZEILE ( 7 ) ;
           WRITELN ( 'NOCH EIN KNOTENPUNKT (J/N) ?' ) ;
           if JNFRAGE ( FALSE ) then
             VPNUMMER := 1
           else
             VPNUMMER := 0
         end (* then *)
     until VPNUMMER = 0
   end (* VPEINGABE *) ;



procedure ANSEINGABE ;

(***********************************)
(*   STEUERUNG ANSCHLUSS-EINGABE   *)
(***********************************)


   var BNUMMER : INTEGER ;
       VPNUMMER : INTEGER ;
       BIND : BTYP ;
       STATUS : STAT ;

   begin (* ANSEINGABE *)
     repeat
       CLRSCRN ;
       WRITELN ( 'EINGABE VON ANSCHLUESSEN' ) ;
       WRITELN ;
       WRITELN ( 'NUMMER DES BEZIRKS:' ) ;
       IEIN ( BNUMMER ) ;
       if BNUMMER <> 0 then
         begin
           SEARCHB ( BNUMMER , BIND , STATUS ) ;
           if STATUS = 0 then
             begin
               WRITELN ( 'NUMMER DES KNOTENPUNKTES:' ) ;
               IEIN ( VPNUMMER ) ;
               if VPNUMMER <> 0 then
                 ANSEIN ( BNUMMER , VPNUMMER )
             end (* then *)
           else
             WRITELN ( 'BEZIRK NICHT VORHANDEN' ) ;
           WRITELN ;
           WRITELN ( 'NOCH EIN ANSCHLUSS (J/N) ?' ) ;
           if JNFRAGE ( FALSE ) then
             BNUMMER := 1
           else
             BNUMMER := 0
         end (* then *) ;
     until BNUMMER = 0
   end (* ANSEINGABE *) ;



procedure STREINGABE ;

(**********************************)
(*   STEUERUNG STRASSEN-EINGABE   *)
(**********************************)


   var STRNUMMER : INTEGER ;
       KEN : CHAR ;
       RICHT : CHAR ;
       STATUS : STAT ;

   begin (* STREINGABE *)
     repeat
       CLRSCRN ;
       WRITELN ( 'EINGABE VON STRASSEN' ) ;
       WRITELN ;
       WRITELN ;
       WRITELN ( 'BEZEICHNUNG DER STRASSE (Z.B. B 27 H, A 81 R):' ) ;
       SBEZEINL ( KEN , STRNUMMER , RICHT , STATUS ) ;
       if STATUS = 0 then
         begin
           STREIN ( KEN , STRNUMMER , RICHT ) ;
           LEERZEILE ( 10 ) ;
           WRITELN ( 'NOCH EINE STRASSE (J/N) ?' ) ;
           if JNFRAGE ( FALSE ) then
             STRNUMMER := 1
           else
             STRNUMMER := 0
         end (* then *)
       else
         STRNUMMER := 0
     until STRNUMMER = 0
   end (* STREINGABE *) ;



procedure KOAEND ( VPNUMMER : INTEGER ) ;

(***********************************************)
(*   KOORDINATEN EINES KNOTENPUNKTES AENDERN   *)
(***********************************************)


   var STATUS : STAT ;
       VPIND : VPTYP ;

   begin (* KOAEND *)
     SEARCHVP ( VPNUMMER , VPIND , STATUS ) ;
     if STATUS = 0 then
       begin
         WRITELN ( 'EINGABE DER GEAENDERTEN KOORDINATEN' ) ;
         WRITELN ( 'X-POSITION:     (ALTER WERT:' , VPKOORD [ VPIND ] .
                   X : 6 , ')' ) ;
         IEIN2 ( X , ELAENGE ) ;
         if ELAENGE <> 0 then
           VPKOORD [ VPIND ] . X := X ;
         WRITELN ( 'Y-POSITION:     (ALTER WERT:' , VPKOORD [ VPIND ] .
                   Y : 6 , ')' ) ;
         IEIN2 ( X , ELAENGE ) ;
         if ELAENGE <> 0 then
           VPKOORD [ VPIND ] . Y := X
       end (* then *) ;
     case STATUS of
       0 : WRITELN ( 'KOORDINATEN DES KNOTENS GEAENDERT' ) ;
       1 : WRITELN ( 'KNOTENPUNKT NICHT VORHANDEN' ) ;
     end (* case *)
   end (* KOAEND *) ;



procedure KOORDAEND ;

(**********************************)
(*   KNOTEN-KOORDINATEN AENDERN   *)
(**********************************)


   var VPNUMMER : INTEGER ;

   begin (* KOORDAEND *)
     repeat
       CLRSCRN ;
       WRITELN ( 'AENDERUNG VON KNOTEN-KOORDINATEN' ) ;
       WRITELN ;
       WRITELN ;
       WRITELN ( 'NUMMER DES KNOTENPUNKTES:' ) ;
       IEIN ( VPNUMMER ) ;
       if VPNUMMER <> 0 then
         begin
           KOAEND ( VPNUMMER ) ;
           LEERZEILE ( 7 ) ;
           WRITELN ( 'NOCH EINE AENDERUNG (J/N) ?' ) ;
           if JNFRAGE ( FALSE ) then
             VPNUMMER := 1
           else
             VPNUMMER := 0
         end (* then *)
     until VPNUMMER = 0
   end (* KOORDAEND *) ;



procedure ANFAEND ( BIND : BTYP ; VPNUMMER : INTEGER ) ;

(****************************)
(*   ANFAHRZEITEN AENDERN   *)
(****************************)


   var STATUS : STAT ;
       VPIND : VPTYP ;
       AKT : APTYP ;
       OK : BOOLEAN ;
       SIT : SITUATIONEN ;
       T : HTEXT ;
       L : INTEGER ;
       TESTCH : CHAR ;
       EINGABEOK : BOOLEAN ;

   begin (* ANFAEND *)
     SEARCHVP ( VPNUMMER , VPIND , STATUS ) ;
     if STATUS <> 0 then
       STATUS := 1
     else
       begin
         SEARCHANS ( BIND , VPIND , STATUS ) ;
         if STATUS <> 0 then
           STATUS := 2
         else
           begin
             AKT := ANSCHLUESSE [ BIND ] ;
             while AKT -> . VKPUNKT <> VPNUMMER do
               AKT := AKT -> . NEXT ;
             repeat
               OK := FALSE ;
               WRITE ( 'INORTS / AUSSERORTS ?' ) ;
               WRITE ( ' ' : 4 ) ;
               WRITE ( '(ALTER WERT: ' ) ;
               if AKT -> . INORTS then
                 WRITE ( 'I' )
               else
                 WRITE ( 'A' ) ;
               WRITE ( ')' ) ;
               WRITELN ;
               repeat
                 TEIN ( T , L ) ;
                 TESTCH := T [ 1 ] ;
                 EINGABEOK := ( L <= 1 ) and ( ( TESTCH = 'A' ) or (
                              TESTCH = 'I' ) or ( TESTCH = ' ' ) ) ;
                 if not EINGABEOK then
                   begin
                     WRITELN (
                            'EINGABE FALSCH, NUR -I- UND -A- ERLAUBT.'
                               ) ;
                     WRITELN ( 'BITTE EINGABE WIEDERHOLEN.' )
                   end (* then *)
               until EINGABEOK ;
               if TESTCH = 'I' then
                 AKT -> . INORTS := TRUE ;
               if TESTCH = 'A' then
                 AKT -> . INORTS := FALSE ;
               WRITELN ( 'EINGABE DER GEAENDERTEN ANFAHRZEITEN' ) ;
               for SIT := SITMIN to SITMAX do
                 begin
                   WRITELN ( 'ANFAHRZEIT(' , SITTEXTE [ SIT ] ,
                             '):      (ALTER WERT:' , AKT -> . ANFAHRZ
                             [ SIT ] : 6 , ')' ) ;
                   IEIN2 ( X , ELAENGE ) ;
                   if ELAENGE <> 0 then
                     AKT -> . ANFAHRZ [ SIT ] := X
                 end (* for *) ;
               WRITELN ( 'EINGABEN O.K. (J/N) ?' ) ;
               OK := JNFRAGE ( TRUE )
             until OK
           end (* else *)
       end (* else *) ;
     case STATUS of
       0 : WRITELN ( 'ANSCHLUSSDATEN GEAENDERT' ) ;
       1 : WRITELN ( 'KNOTENPUNKT NICHT VORHANDEN' ) ;
       2 : WRITELN ( 'ANSCHLUSS NICHT VORHANDEN' ) ;
     end (* case *)
   end (* ANFAEND *) ;



procedure ANFAHRAEND ;

(**************************************)
(*   AENDERN DATEN BEI ANSCHLUESSEN   *)
(**************************************)


   var BNUMMER : INTEGER ;
       VPNUMMER : INTEGER ;
       BIND : BTYP ;
       STATUS : STAT ;

   begin (* ANFAHRAEND *)
     repeat
       CLRSCRN ;
       WRITELN ( 'AENDERN VON ANSCHLUSSDATEN (ANFAHRZ. UND I/A-KENNZ)'
                 ) ;
       WRITELN ;
       WRITELN ( 'NUMMER DES BEZIRKS:' ) ;
       IEIN ( BNUMMER ) ;
       if BNUMMER <> 0 then
         begin
           SEARCHB ( BNUMMER , BIND , STATUS ) ;
           if STATUS = 0 then
             begin
               WRITELN ( 'NUMMER DES KNOTENPUNKTES:' ) ;
               IEIN ( VPNUMMER ) ;
               if VPNUMMER <> 0 then
                 ANFAEND ( BIND , VPNUMMER )
             end (* then *)
           else
             WRITELN ( 'BEZIRK NICHT VORHANDEN' ) ;
           WRITELN ;
           WRITELN ( 'NOCH EINE AENDERUNG (J/N) ?' ) ;
           if JNFRAGE ( FALSE ) then
             BNUMMER := 1
           else
             BNUMMER := 0
         end (* then *) ;
     until BNUMMER = 0
   end (* ANFAHRAEND *) ;



procedure STRAEND ( KEN : CHAR ; STRNUMMER : INTEGER ; RICHT : CHAR ) ;

(*****************************)
(*   AENDERN EINER STRASSE   *)
(*****************************)


   var STATUS : STAT ;
       SIND : STRTYP ;
       STRSAVE : STRASSE ;
       OK : BOOLEAN ;

   begin (* STRAEND *)
     SEARCHSTR ( KEN , STRNUMMER , RICHT , SIND , STATUS ) ;
     if STATUS = 0 then
       begin
         STRZUW ( STRSAVE , STRASSENNETZ [ SIND ] ) ;
         STRBEARB ( STRASSENNETZ [ SIND ] , OK , FALSE ) ;
         if not OK then
           begin
             STATUS := 2 ;
             STRZUW ( STRASSENNETZ [ SIND ] , STRSAVE )
           end (* then *) ;
         DISPKETTE ( STRSAVE . ANFANG )
       end (* then *) ;
     case STATUS of
       0 : WRITELN ( 'STRASSENDATEN GEAENDERT' ) ;
       1 : WRITELN ( 'STRASSE NICHT VORHANDEN' ) ;
       2 : WRITELN ( 'ABBRUCH DURCH BENUTZER; ' ,
                     'STRASSE BLEIBT UNVERAENDERT.' )
     end (* case *)
   end (* STRAEND *) ;



procedure STRAENDERN ;

(**************************************)
(*   STEUERUNG AENDERN VON STRASSEN   *)
(**************************************)


   var STRNUMMER : INTEGER ;
       KEN : CHAR ;
       RICHT : CHAR ;
       STATUS : STAT ;

   begin (* STRAENDERN *)
     repeat
       CLRSCRN ;
       WRITELN ( 'STRASSENDATEN AENDERN' ) ;
       WRITELN ;
       WRITELN ;
       WRITELN ( 'BEZEICHNUNG DER STRASSE (Z.B. B 27 H, A 81 R):' ) ;
       SBEZEINL ( KEN , STRNUMMER , RICHT , STATUS ) ;
       if STATUS = 0 then
         begin
           STRAEND ( KEN , STRNUMMER , RICHT ) ;
           LEERZEILE ( 10 ) ;
           WRITELN ( 'NOCH EINE STRASSE AENDERN (J/N) ?' ) ;
           if JNFRAGE ( FALSE ) then
             STRNUMMER := 1
           else
             STRNUMMER := 0
         end (* then *)
       else
         STRNUMMER := 0
     until STRNUMMER = 0
   end (* STRAENDERN *) ;



procedure VPLOE ( VPNUMMER : INTEGER ) ;

(************************************)
(*   LOESCHEN EINES KNOTENPUNKTES   *)
(************************************)


   var STATUS : STAT ;
       VPIND : VPTYP ;
       SWITCH : BOOLEAN ;
       I : INTEGER ;
       BNUMMER : INTEGER ;
       BP : APTYP ;
       STR : STRASSE ;
       STP : STPTYP ;

   begin (* VPLOE *)
     SEARCHVP ( VPNUMMER , VPIND , STATUS ) ;
     if STATUS = 0 then
       begin
         SWITCH := FALSE ;
         for I := 1 to BZAHL do
           begin
             BP := ANSCHLUESSE [ I ] ;
             BNUMMER := BTRANSFER [ I ] ;
             while BP <> NIL do
               begin
                 if BP -> . VKPUNKT = VPNUMMER then
                   begin
                     WRITELN ( 'ANSCHLUSS (' , BNUMMER : 10 , ',' ,
                               VPNUMMER : 10 , ') VORHANDEN.' ) ;
                     SWITCH := TRUE
                   end (* then *) ;
                 BP := BP -> . NEXT
               end (* while *)
           end (* for *) ;
         for I := 1 to STRZAHL do
           begin
             STR := STRASSENNETZ [ I ] ;
             STP := STR . ANFANG ;
             while STP <> NIL do
               begin
                 if STP -> . VKPUNKT = VPNUMMER then
                   begin
                     WRITE ( 'KNOTENPUNKT' , VPNUMMER : 10 ,
                             ' IN STRASSE ' ) ;
                     SBEZAUSG ( OUTPUT , STR , 0 ) ;
                     WRITELN ( ' VORHANDEN.' ) ;
                     SWITCH := TRUE ;
                     STP := NIL
                   end (* then *)
                 else
                   STP := STP -> . NEXT
               end (* while *)
           end (* for *) ;
         if SWITCH then
           STATUS := 2
         else
           begin
             if VPIND = VPZAHL then
               begin
                 VPTRANSFER [ VPZAHL ] := 0 ;
                 VPKOORD [ VPZAHL ] . X := 0 ;
                 VPKOORD [ VPZAHL ] . Y := 0 ;
                 VPZAHL := VPZAHL - 1
               end (* then *)
             else
               begin
                 for I := VPIND to VPZAHL - 1 do
                   begin
                     VPTRANSFER [ I ] := VPTRANSFER [ I + 1 ] ;
                     VPKOORD [ I ] := VPKOORD [ I + 1 ] ;
                   end (* for *) ;
                 VPTRANSFER [ VPZAHL ] := 0 ;
                 VPKOORD [ VPZAHL ] . X := 0 ;
                 VPKOORD [ VPZAHL ] . Y := 0 ;
                 VPZAHL := VPZAHL - 1
               end (* else *)
           end (* else *)
       end (* then *) ;
     case STATUS of
       0 : WRITELN ( 'KNOTENPUNKT WURDE GELOESCHT' ) ;
       1 : WRITELN ( 'KNOTENPUNKT NICHT VORHANDEN' ) ;
       2 : WRITELN ( 'KNOTENPUNKT DARF NICHT GELOESCHT WERDEN' ) ;
     end (* case *)
   end (* VPLOE *) ;



procedure VPLOESCHEN ;

(*************************************)
(*   STEUERUNG LOESCHEN VON KNOTEN   *)
(*************************************)


   var VPNUMMER : INTEGER ;

   begin (* VPLOESCHEN *)
     repeat
       CLRSCRN ;
       WRITELN ( 'LOESCHEN VON KNOTENPUNKTEN' ) ;
       WRITELN ;
       WRITELN ;
       WRITELN ( 'NUMMER DES ZU LOESCHENDEN KNOTENPUNKTES:' ) ;
       IEIN ( VPNUMMER ) ;
       if VPNUMMER <> 0 then
         begin
           VPLOE ( VPNUMMER ) ;
           LEERZEILE ( 7 ) ;
           WRITELN ( 'NOCH EINE LOESCHUNG (J/N) ?' ) ;
           if JNFRAGE ( FALSE ) then
             VPNUMMER := 1
           else
             VPNUMMER := 0
         end (* then *)
     until VPNUMMER = 0
   end (* VPLOESCHEN *) ;



procedure ANSLOE ( BIND : BTYP ; VPNUMMER : INTEGER ) ;

(****************************)
(*   ANSCHLUSS LOESCHEN     *)
(****************************)


   var STATUS : STAT ;
       VPIND : VPTYP ;
       AKT : APTYP ;
       VOR : APTYP ;

   begin (* ANSLOE *)
     SEARCHVP ( VPNUMMER , VPIND , STATUS ) ;
     if STATUS <> 0 then
       STATUS := 1
     else
       begin
         SEARCHANS ( BIND , VPIND , STATUS ) ;
         if STATUS <> 0 then
           STATUS := 2
         else
           begin
             VOR := NIL ;
             AKT := ANSCHLUESSE [ BIND ] ;
             while AKT -> . VKPUNKT <> VPNUMMER do
               begin
                 VOR := AKT ;
                 AKT := AKT -> . NEXT
               end (* while *) ;
             if VOR = NIL then
               ANSCHLUESSE [ BIND ] := AKT -> . NEXT
             else
               VOR -> . NEXT := AKT -> . NEXT
           end (* else *)
       end (* else *) ;
     case STATUS of
       0 : WRITELN ( 'ANSCHLUSS GELOESCHT' ) ;
       1 : WRITELN ( 'KNOTENPUNKT NICHT VORHANDEN' ) ;
       2 : WRITELN ( 'ANSCHLUSS NICHT VORHANDEN' ) ;
     end (* case *)
   end (* ANSLOE *) ;



procedure ANSLOESCHEN ;

(*******************************************)
(*   STEUERUNG LOESCHEN VON ANSCHLUESSEN   *)
(*******************************************)


   var BNUMMER : INTEGER ;
       VPNUMMER : INTEGER ;
       BIND : BTYP ;
       STATUS : STAT ;

   begin (* ANSLOESCHEN *)
     repeat
       CLRSCRN ;
       WRITELN ( 'LOESCHEN VON ANSCHLUESSEN' ) ;
       WRITELN ;
       WRITELN ;
       WRITELN ( 'NUMMER DES BEZIRKS:' ) ;
       IEIN ( BNUMMER ) ;
       if BNUMMER <> 0 then
         begin
           SEARCHB ( BNUMMER , BIND , STATUS ) ;
           if STATUS = 0 then
             begin
               WRITELN ( 'NUMMER DES KNOTENPUNKTES:' ) ;
               IEIN ( VPNUMMER ) ;
               if VPNUMMER <> 0 then
                 ANSLOE ( BIND , VPNUMMER )
             end (* then *)
           else
             WRITELN ( 'BEZIRK NICHT VORHANDEN' ) ;
           LEERZEILE ( 2 ) ;
           WRITELN ( 'NOCH EINE LOESCHUNG (J/N) ?' ) ;
           if JNFRAGE ( FALSE ) then
             BNUMMER := 1
           else
             BNUMMER := 0
         end (* then *) ;
     until BNUMMER = 0
   end (* ANSLOESCHEN *) ;



procedure STRLOE ( KEN : CHAR ; STRNUMMER : INTEGER ; RICHT : CHAR ) ;

(*************************)
(*   STRASSE LOESCHEN    *)
(*************************)


   var STATUS : STAT ;
       SIND : STRTYP ;
       LEERSTR : STRASSE ;
       I : INTEGER ;

   begin (* STRLOE *)
     LEERSTR . ANFANG := NIL ;
     LEERSTR . ENDE := NIL ;
     LEERSTR . KENNUNG := ' ' ;
     LEERSTR . NUMMER := 0 ;
     LEERSTR . RICHTUNG := HIN ;
     SEARCHSTR ( KEN , STRNUMMER , RICHT , SIND , STATUS ) ;
     if STATUS = 0 then
       begin
         DISPKETTE ( STRASSENNETZ [ SIND ] . ANFANG ) ;
         if SIND = STRZAHL then
           begin
             STRASSENNETZ [ STRZAHL ] := LEERSTR ;
             STRZAHL := STRZAHL - 1
           end (* then *)
         else
           begin
             for I := SIND to STRZAHL - 1 do
               STRASSENNETZ [ I ] := STRASSENNETZ [ I + 1 ] ;
             STRASSENNETZ [ STRZAHL ] := LEERSTR ;
             STRZAHL := STRZAHL - 1
           end (* else *)
       end (* then *) ;
     case STATUS of
       0 : WRITELN ( 'STRASSE WURDE GELOESCHT' ) ;
       1 : WRITELN ( 'STRASSE NICHT VORHANDEN' ) ;
     end (* case *)
   end (* STRLOE *) ;



procedure STRLOESCHEN ;

(***************************************)
(*   STEUERUNG LOESCHEN VON STRASSEN   *)
(***************************************)


   var STRNUMMER : INTEGER ;
       KEN : CHAR ;
       RICHT : CHAR ;
       STATUS : STAT ;

   begin (* STRLOESCHEN *)
     repeat
       CLRSCRN ;
       WRITELN ( 'LOESCHEN VON STRASSEN' ) ;
       WRITELN ;
       WRITELN ;
       WRITELN ( 'BEZEICHNUNG DER STRASSE (Z.B. B 27 H, A 81 R):' ) ;
       SBEZEINL ( KEN , STRNUMMER , RICHT , STATUS ) ;
       if STATUS = 0 then
         begin
           STRLOE ( KEN , STRNUMMER , RICHT ) ;
           LEERZEILE ( 10 ) ;
           WRITELN ( 'NOCH EINE STRASSE LOESCHEN (J/N) ?' ) ;
           if JNFRAGE ( FALSE ) then
             STRNUMMER := 1
           else
             STRNUMMER := 0
         end (* then *)
       else
         STRNUMMER := 0
     until STRNUMMER = 0
   end (* STRLOESCHEN *) ;



procedure ANSAUSG ;

(****************************)
(*   ANSCHLUESSE AUSGEBEN   *)
(****************************)


   var I , J : INTEGER ;
       AKT : APTYP ;
       SIT : SITUATIONEN ;

   begin (* ANSAUSG *)
     WRITE ( 'ANSCHLUESSE:' ) ;
     WRITE ( ' ' : 11 ) ;
     WRITE ( 'IO/AO:' ) ;
     WRITE ( ' ' : 4 ) ;
     WRITELN ( 'ANFAHRZEITEN:' ) ;
     WRITELN ;
     for I := 1 to BZAHL do
       begin
         AKT := ANSCHLUESSE [ I ] ;
         while AKT <> NIL do
           begin
             WRITE ( '(' , BTRANSFER [ I ] : 8 , '/' , AKT -> . VKPUNKT
                     : 8 , ')' ) ;
             if AKT -> . INORTS then
               WRITE ( 'IO' : 9 )
             else
               WRITE ( 'AO' : 9 ) ;
             WRITE ( '     (' ) ;
             for SIT := SITMIN to SITMAX do
               begin
                 WRITE ( SITTEXTE [ SIT ] , ':' , AKT -> . ANFAHRZ [
                         SIT ] : 6 ) ;
                 if SIT < SITMAX then
                   WRITE ( ' /' ) ;
               end (* for *) ;
             WRITE ( ' )' ) ;
             WRITELN ;
             AKT := AKT -> . NEXT ;
           end (* while *)
       end (* for *)
   end (* ANSAUSG *) ;



procedure ANSAUSGDRUCK ;

(****************************************)
(*   ANSCHLUESSE AUSGEBEN AUF DRUCKER   *)
(****************************************)


   var I : INTEGER ;
       AKT : APTYP ;
       SIT : SITUATIONEN ;

   begin (* ANSAUSGDRUCK *)
     UEBERSCHRIFT ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     WRITE ( DRUCKER , ' ANSCHLUESSE:' ) ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     WRITE ( DRUCKER , ' ' : 30 , 'INNER-/' ) ;
     DRUCKEZEILE ;
     WRITE ( DRUCKER , ' BEZIRKS-NR    KNOTENPUNKT    AUSSERORTS' ) ;
     WRITE ( DRUCKER , ' ' : 10 ) ;
     WRITE ( DRUCKER , 'ANFAHRZEITEN JE VERKEHRSSITUATION' ) ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     for I := 1 to BZAHL do
       begin
         AKT := ANSCHLUESSE [ I ] ;
         if AKT <> NIL then
           begin
             if ZEILZAHL > PZZ - 15 then
               begin
                 UEBERSCHRIFT ;
                 DRUCKEZEILE ;
                 DRUCKEZEILE ;
                 WRITE ( DRUCKER , ' ' : 30 , 'INNER-/' ) ;
                 DRUCKEZEILE ;
                 WRITE ( DRUCKER ,
                         ' BEZIRKS-NR    KNOTENPUNKT    AUSSERORTS' ) ;
                 WRITE ( DRUCKER , ' ' : 10 ) ;
                 WRITE ( DRUCKER , 'ANFAHRZEITEN JE VERKEHRSSITUATION'
                         ) ;
                 DRUCKEZEILE ;
                 DRUCKEZEILE ;
               end (* then *) ;
             DRUCKEZEILE
           end (* then *) ;
         while AKT <> NIL do
           begin
             WRITE ( DRUCKER , ' ' , BTRANSFER [ I ] : 10 , '     ' ,
                     AKT -> . VKPUNKT : 10 ) ;
             if AKT -> . INORTS then
               WRITE ( DRUCKER , 'IO' : 14 )
             else
               WRITE ( DRUCKER , 'AO' : 14 ) ;
             WRITE ( DRUCKER , ' ' : 10 ) ;
             for SIT := SITMIN to SITMAX do
               begin
                 WRITE ( DRUCKER , SITTEXTE [ SIT ] , ':' , AKT -> .
                         ANFAHRZ [ SIT ] : 6 , ' ' : 8 )
               end (* for *) ;
             DRUCKEZEILE ;
             AKT := AKT -> . NEXT ;
           end (* while *)
       end (* for *)
   end (* ANSAUSGDRUCK *) ;



procedure VPAUSG ;

(*****************************)
(*   KNOTENPUNKTE AUSGEBEN   *)
(*****************************)


   var I , J : INTEGER ;

   begin (* VPAUSG *)
     WRITELN ( 'KNOTENPUNKTE:' ) ;
     WRITELN ;
     J := 1 ;
     for I := 1 to VPZAHL do
       begin
         WRITE ( '(' , I : 10 , ':' , VPTRANSFER [ I ] : 10 , ' /' ,
                 VPKOORD [ I ] . X : 10 , ',' , VPKOORD [ I ] . Y : 10
                 , ' /) ' ) ;
         J := J + 1 ;
         if J > 1 then
           begin
             WRITELN ;
             J := 1
           end (* then *)
       end (* for *)
   end (* VPAUSG *) ;



procedure VPAUSGDRUCK ;

(*****************************************)
(*   KNOTENPUNKTE AUSGEBEN AUF DRUCKER   *)
(*****************************************)


   var I : INTEGER ;

   begin (* VPAUSGDRUCK *)
     UEBERSCHRIFT ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     WRITE ( DRUCKER , ' KNOTENPUNKTE:' ) ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     WRITE ( DRUCKER , ' KNOTENPUNKT-NUMMER' , ' ' : 5 ) ;
     WRITE ( DRUCKER , ' KNOTENPUNKT-KOORDINATEN' ) ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     for I := 1 to VPZAHL do
       begin
         if ZEILZAHL > PZZ - 12 then
           begin
             UEBERSCHRIFT ;
             DRUCKEZEILE ;
             DRUCKEZEILE ;
             WRITE ( DRUCKER , ' KNOTENPUNKT-NUMMER' , ' ' : 5 ) ;
             WRITE ( DRUCKER , ' KNOTENPUNKT-KOORDINATEN' ) ;
             DRUCKEZEILE ;
             DRUCKEZEILE
           end (* then *) ;
         WRITE ( DRUCKER , ' ' , VPTRANSFER [ I ] : 18 , '      (' ,
                 VPKOORD [ I ] . X : 10 , ' ,' , VPKOORD [ I ] . Y : 10
                 , ' )' ) ;
         DRUCKEZEILE ;
       end (* for *)
   end (* VPAUSGDRUCK *) ;



procedure STRAUSG ;

(*************************)
(*   STRASSEN AUSGEBEN   *)
(*************************)


   var I : INTEGER ;
       STR : STRASSE ;
       STP : STPTYP ;
       SIT : SITUATIONEN ;

   begin (* STRAUSG *)
     WRITELN ( 'STRASSEN:' ) ;
     WRITELN ;
     for I := 1 to STRZAHL do
       begin
         STR := STRASSENNETZ [ I ] ;
         SBEZAUSG ( OUTPUT , STR , 15 ) ;
         WRITE ( ' (' ) ;
         STP := STR . ANFANG ;
         while STP <> NIL do
           begin
             WRITE ( STP -> . VKPUNKT : 10 ) ;
             if STP -> . NEXT <> NIL then
               begin
                 WRITE ( ' /' , STP -> . DISTANZ : 10 , ' /' ) ;
                 WRITE ( ' KAT:' , STP -> . KAT : 4 , ' /' ) ;
                 if STP -> . INNERORTS then
                   WRITE ( ' IO /' )
                 else
                   WRITE ( ' AO /' ) ;
                 for SIT := SITMIN to SITMAX do
                   begin
                     WRITE ( STP -> . ZEITZUSCHL [ SIT ] : 5 ) ;
                     if SIT = SITMAX then
                       WRITE ( ' /' )
                     else
                       WRITE ( ' ,' )
                   end (* for *) ;
                 WRITELN ;
                 WRITE ( ' ' : 17 )
               end (* then *)
             else
               begin
                 WRITE ( ' )' ) ;
                 WRITELN ;
                 WRITELN
               end (* else *) ;
             STP := STP -> . NEXT
           end (* while *)
       end (* for *)
   end (* STRAUSG *) ;



procedure STRAUSGDRUCK ;

(*************************************)
(*   STRASSEN AUSGEBEN AUF DRUCKER   *)
(*************************************)


   var I : INTEGER ;
       STR : STRASSE ;
       STP : STPTYP ;
       SIT : SITUATIONEN ;

   begin (* STRAUSGDRUCK *)
     UEBERSCHRIFT ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     WRITE ( DRUCKER , ' STRASSEN:' ) ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     WRITE ( DRUCKER , ' ' , ' ' : 59 , 'INNER-/' ) ;
     WRITE ( DRUCKER , ' ' : 14 , 'STAUZEITEN' ) ;
     DRUCKEZEILE ;
     WRITE ( DRUCKER , ' STRASSENBEZEICHNUNG' ) ;
     WRITE ( DRUCKER , '    KNOTENPUNKT' ) ;
     WRITE ( DRUCKER , '  ENTFERNUNG' ) ;
     WRITE ( DRUCKER , '  KATEGORIE' ) ;
     WRITE ( DRUCKER , '  AUSSERORTS' ) ;
     WRITE ( DRUCKER , '      ' ) ;
     for SIT := SITMIN to SITMAX do
       WRITE ( DRUCKER , SITTEXTE [ SIT ] , '  ' ) ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     for I := 1 to STRZAHL do
       begin
         STR := STRASSENNETZ [ I ] ;
         WRITE ( DRUCKER , ' ' ) ;
         SBEZAUSG ( DRUCKER , STR , 19 ) ;
         STP := STR . ANFANG ;
         while STP <> NIL do
           begin
             WRITE ( DRUCKER , ' ' : 5 ) ;
             WRITE ( DRUCKER , STP -> . VKPUNKT : 10 ) ;
             if STP -> . NEXT <> NIL then
               begin
                 WRITE ( DRUCKER , '  ' , STP -> . DISTANZ : 10 , '  '
                         ) ;
                 WRITE ( DRUCKER , STP -> . KAT : 9 , ' ' : 3 ) ;
                 if STP -> . INNERORTS then
                   WRITE ( DRUCKER , 'IO' : 9 , ' ' : 3 )
                 else
                   WRITE ( DRUCKER , 'AO' : 9 , ' ' : 3 ) ;
                 for SIT := SITMIN to SITMAX do
                   begin
                     WRITE ( DRUCKER , STP -> . ZEITZUSCHL [ SIT ] : 8
                             ) ;
                   end (* for *) ;
                 DRUCKEZEILE ;
                 if ZEILZAHL > PZZ - 12 then
                   begin
                     UEBERSCHRIFT ;
                     DRUCKEZEILE ;
                     DRUCKEZEILE ;
                     WRITE ( DRUCKER , ' ' , ' ' : 59 , 'INNER-/' ) ;
                     WRITE ( DRUCKER , ' ' : 14 , 'STAUZEITEN' ) ;
                     DRUCKEZEILE ;
                     WRITE ( DRUCKER , ' STRASSENBEZEICHNUNG' ) ;
                     WRITE ( DRUCKER , '    KNOTENPUNKT' ) ;
                     WRITE ( DRUCKER , '  ENTFERNUNG' ) ;
                     WRITE ( DRUCKER , '  KATEGORIE' ) ;
                     WRITE ( DRUCKER , '  AUSSERORTS' ) ;
                     WRITE ( DRUCKER , '      ' ) ;
                     for SIT := SITMIN to SITMAX do
                       WRITE ( DRUCKER , SITTEXTE [ SIT ] , '  ' ) ;
                     DRUCKEZEILE ;
                     DRUCKEZEILE ;
                   end (* then *) ;
                 WRITE ( DRUCKER , ' ' : 20 )
               end (* then *)
             else
               begin
                 DRUCKEZEILE ;
                 if ZEILZAHL > PZZ - 12 then
                   begin
                     UEBERSCHRIFT ;
                     DRUCKEZEILE ;
                     DRUCKEZEILE ;
                     WRITE ( DRUCKER , ' ' , ' ' : 59 , 'INNER-/' ) ;
                     WRITE ( DRUCKER , ' ' : 14 , 'STAUZEITEN' ) ;
                     DRUCKEZEILE ;
                     WRITE ( DRUCKER , ' STRASSENBEZEICHNUNG' ) ;
                     WRITE ( DRUCKER , '    KNOTENPUNKT' ) ;
                     WRITE ( DRUCKER , '  ENTFERNUNG' ) ;
                     WRITE ( DRUCKER , '  KATEGORIE' ) ;
                     WRITE ( DRUCKER , '  AUSSERORTS' ) ;
                     WRITE ( DRUCKER , '      ' ) ;
                     for SIT := SITMIN to SITMAX do
                       WRITE ( DRUCKER , SITTEXTE [ SIT ] , '  ' ) ;
                     DRUCKEZEILE ;
                     DRUCKEZEILE ;
                   end (* then *) ;
                 DRUCKEZEILE
               end (* else *) ;
             STP := STP -> . NEXT
           end (* while *)
       end (* for *)
   end (* STRAUSGDRUCK *) ;



procedure BAUSG ;

(************************)
(*   BEZIRKE AUSGEBEN   *)
(************************)


   var I , J : INTEGER ;

   begin (* BAUSG *)
     WRITELN ( 'BEZIRKE:' ) ;
     WRITELN ;
     J := 1 ;
     for I := 1 to BZAHL do
       begin
         WRITE ( ' (' , I : 10 , ':' , BTRANSFER [ I ] : 10 , ')' ) ;
         J := J + 1 ;
         if J > 3 then
           begin
             WRITELN ;
             J := 1
           end (* then *)
       end (* for *) ;
     WRITELN
   end (* BAUSG *) ;



procedure BAUSGDRUCK ;

(************************************)
(*   BEZIRKE AUSGEBEN AUF DRUCKER   *)
(************************************)


   var I , J : INTEGER ;

   begin (* BAUSGDRUCK *)
     UEBERSCHRIFT ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     WRITE ( DRUCKER , ' BEZIRKE:' ) ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     J := 1 ;
     for I := 1 to BZAHL do
       begin
         if ( ZEILZAHL > PZZ - 12 ) and ( J = 1 ) then
           begin
             UEBERSCHRIFT ;
             DRUCKEZEILE ;
             DRUCKEZEILE ;
             WRITE ( DRUCKER , ' BEZIRKE:' ) ;
             DRUCKEZEILE ;
             DRUCKEZEILE ;
           end (* then *) ;
         WRITE ( DRUCKER , ' ' , BTRANSFER [ I ] : 8 ) ;
         J := J + 1 ;
         if J > 10 then
           begin
             DRUCKEZEILE ;
             J := 1
           end (* then *)
       end (* for *) ;
     DRUCKEZEILE
   end (* BAUSGDRUCK *) ;



procedure LISTING ;

(*************************)
(*   STEUERUNG LISTING   *)
(*************************)


   var WAHL : INTEGER ;

   begin (* LISTING *)
     repeat
       repeat
         CLRSCRN ;
         WRITELN ( 'LISTING:' ) ;
         WRITELN ;
         WRITELN ( '( 1)-BEZIRKE--------------AUF BILDSCHIRM' ) ;
         WRITELN ( '( 2)-BEZIRKE-----------------AUF DRUCKER' ) ;
         WRITELN ( '( 3)-KNOTENPUNKTE---------AUF BILDSCHIRM' ) ;
         WRITELN ( '( 4)-KNOTENPUNKTE------------AUF DRUCKER' ) ;
         WRITELN ( '( 5)-ANSCHLUESSE----------AUF BILDSCHIRM' ) ;
         WRITELN ( '( 6)-ANSCHLUESSE-------------AUF DRUCKER' ) ;
         WRITELN ( '( 7)-STRASSEN-------------AUF BILDSCHIRM' ) ;
         WRITELN ( '( 8)-STRASSEN----------------AUF DRUCKER' ) ;
         WRITELN ( '( 9)-ALLES----------------AUF BILDSCHIRM' ) ;
         WRITELN ( '(10)-ALLES-------------------AUF DRUCKER' ) ;
         WRITELN ;
         WRITELN ( 'AUSWAHL EINGEBEN: ' ) ;
         IEIN ( WAHL ) ;
       until ( WAHL >= 0 ) and ( WAHL <= 10 ) ;
       if WAHL <> 0 then
         case WAHL of
           1 : begin
                 CLRSCRN ;
                 BAUSG ;
                 READY
               end (* tag/ca *) ;
           2 : begin
                 DRUCKEZEILE ;
                 BAUSGDRUCK ;
                 READY
               end (* tag/ca *) ;
           3 : begin
                 CLRSCRN ;
                 VPAUSG ;
                 READY
               end (* tag/ca *) ;
           4 : begin
                 DRUCKEZEILE ;
                 VPAUSGDRUCK ;
                 READY
               end (* tag/ca *) ;
           5 : begin
                 CLRSCRN ;
                 ANSAUSG ;
                 READY
               end (* tag/ca *) ;
           6 : begin
                 DRUCKEZEILE ;
                 ANSAUSGDRUCK ;
                 READY
               end (* tag/ca *) ;
           7 : begin
                 CLRSCRN ;
                 STRAUSG ;
                 READY
               end (* tag/ca *) ;
           8 : begin
                 DRUCKEZEILE ;
                 STRAUSGDRUCK ;
                 READY
               end (* tag/ca *) ;
           9 : begin
                 CLRSCRN ;
                 BAUSG ;
                 WAIT ;
                 CLRSCRN ;
                 VPAUSG ;
                 WAIT ;
                 CLRSCRN ;
                 ANSAUSG ;
                 WAIT ;
                 CLRSCRN ;
                 STRAUSG ;
                 WAIT
               end (* tag/ca *) ;
           10 : begin
                  BAUSGDRUCK ;
                  VPAUSGDRUCK ;
                  ANSAUSGDRUCK ;
                  STRAUSGDRUCK ;
                  READY
                end (* tag/ca *)
         end (* case *)
     until WAHL = 0
   end (* LISTING *) ;



procedure PARAMAEND ;

(*****************************)
(*   STEUERUNG AENDERUNGEN   *)
(*****************************)


   var WAHL : INTEGER ;

   begin (* PARAMAEND *)
     repeat
       repeat
         CLRSCRN ;
         WRITELN ( 'MODELLPARAMETER AENDERN:' ) ;
         WRITELN ;
         WRITELN ( '(1)-KNOTEN-KOORDINATEN--------AENDERN' ) ;
         WRITELN ( '(2)-ANSCHLUSSDATEN------------AENDERN' ) ;
         WRITELN ( '(3)-STRASSENDATEN-------------AENDERN' ) ;
         WRITELN ;
         WRITELN ( 'AUSWAHL EINGEBEN: ' ) ;
         IEIN ( WAHL ) ;
       until ( WAHL >= 0 ) and ( WAHL <= 3 ) ;
       if WAHL <> 0 then
         case WAHL of
           1 : KOORDAEND ;
           2 : ANFAHRAEND ;
           3 : STRAENDERN ;
         end (* case *)
     until WAHL = 0
   end (* PARAMAEND *) ;



procedure CONTROL ;

(***********************************)
(*   TAETIGKEITSWAHL, GRUNDMENUE   *)
(***********************************)


   var WAHL : INTEGER ;
       WARNUNG : BOOLEAN ;
       FERTIG : BOOLEAN ;

   begin (* CONTROL *)
     WARNUNG := FALSE ;
     repeat
       repeat
         repeat
           CLRSCRN ;
           WRITELN ( 'TAETIGKEITSWAHL:' ) ;
           WRITELN ;
           WRITELN ( '( 1)-KNOTENPUNKTE-------------EINGEBEN' ) ;
           WRITELN ( '( 2)-ANSCHLUESSE--------------EINGEBEN' ) ;
           WRITELN ( '( 3)-STRASSEN-----------------EINGEBEN' ) ;
           WRITELN ( '( 4)-BEZIRKE-(OEV-FILE)-------EINLESEN' ) ;
           WRITELN ( '( 5)-PARAMETER-----------------AENDERN' ) ;
           WRITELN ( '( 6)-KNOTENPUNKTE-------------LOESCHEN' ) ;
           WRITELN ( '( 7)-ANSCHLUESSE--------------LOESCHEN' ) ;
           WRITELN ( '( 8)-STRASSEN-----------------LOESCHEN' ) ;
           WRITELN ( '( 9)----------------------------UPDATE' ) ;
           WRITELN ( '(10)---------------------------LISTING' ) ;
           WRITELN ;
           WRITELN ( 'AUSWAHL EINGEBEN: ' ) ;
           IEIN ( WAHL ) ;
         until ( WAHL >= 0 ) and ( WAHL <= 10 ) ;
         if WAHL <> 0 then
           begin
             if WAHL <= 8 then
               WARNUNG := TRUE ;
             case WAHL of
               1 : VPEINGABE ;
               2 : ANSEINGABE ;
               3 : STREINGABE ;
               4 : INITBEZIRKE ;
               5 : PARAMAEND ;
               6 : VPLOESCHEN ;
               7 : ANSLOESCHEN ;
               8 : STRLOESCHEN ;
               9 : begin
                     WARNUNG := FALSE ;
                     UPD
                   end (* tag/ca *) ;
               10 : LISTING ;
             end (* case *)
           end (* then *)
       until WAHL = 0 ;
       FERTIG := TRUE ;
       if WARNUNG then
         begin
           WRITELN ( 'ENDE OHNE UPDATE (J/N) ?' ) ;
           FERTIG := JNFRAGE ( TRUE )
         end (* then *)
     until FERTIG
   end (* CONTROL *) ;



procedure KONSISTENZ ( var RICHTIG : BOOLEAN ) ;

(*******************************************************************)
(*                                                                 *)
(*   KONSISTENZPRUEFUNG BEIM EINSTIEG IN DEN NETZEDITOR            *)
(*                                                                 *)
(*   FOLGENDE KONSISTENZBEDINGUNGEN WERDEN GEPRUEFT:               *)
(*                                                                 *)
(*   - BEZIRKE MUESSEN EINDEUTIG UND AUFSTEIGEND SEIN.             *)
(*   - BEZIRKE UEBER BZAHL MUESSEN GLEICH 0 SEIN.                  *)
(*   - KNOTENPUNKTE MUESSEN EINDEUTIG UND AUFSTEIGEND SEIN.        *)
(*   - KNOTENPUNKTE UEBER VPZAHL MUESSEN GLEICH NULL SEIN.         *)
(*   - INNERHALB LISTE VON ANSCHLUESSEN MUSS KNOTENPUNKT           *)
(*        EINDEUTIG SEIN.                                          *)
(*   - JEDER KNOTENPUNKT IN EINER LISTE VON ANSCHLUESSEN MUSS      *)
(*        DEFINIERT SEIN.                                          *)
(*   - STRASSEN MUESSEN AUFSTEIGEND (IM SINNE DER DURCH DIE        *)
(*        FUNKTION KLEINERSTR VORGEGEBENEN ORDNUNG) UND            *)
(*        EINDEUTIG SEIN.                                          *)
(*   - BEZIRKSNUMMERN DUERFEN NUR POSITIV SEIN.                    *)
(*   - KNOTENPUNKTE DUERFEN NUR POSITIV SEIN.                      *)
(*   - STRASSENBEZEICHNUNGEN DUERFEN NUR DIE BUCHSTABEN            *)
(*        A,B,K,L UND S, EINE BIS ZU 8-STELLIGE ZAHL UND DIE       *)
(*        RICHTUNG (H ODER R) ENTHALTEN.                           *)
(*   - PRO STRASSE MUESSEN MINDESTENS ZWEI PUNKTE ENTHALTEN        *)
(*        SEIN; ZYKLEN SIND VERBOTEN.                              *)
(*   - JEDER KNOTENPUNKT IN EINER STRASSE MUSS DEFINIERT SEIN.     *)
(*                                                                 *)
(*******************************************************************)


   var I : INTEGER ;
       BKOPF , BPTR , BPTR2 : APTYP ;
       STR : STRASSE ;
       STP : STPTYP ;
       VPTEST : INTEGER ;
       VPIND : VPTYP ;
       STATUS : STAT ;
       ERSTERFEHLER : BOOLEAN ;


   procedure FEHLER ( NR : INTEGER ) ;

   (**********************************************************)
   (*  GIBT FEHLERTEXT UND WEITERE INFORMATIONEN AUS,        *)
   (*  FALLS FEHLER BEI KONSISTENZPRUEFUNG FESTGESTELLT      *)
   (*  WURDE.                                                *)
   (**********************************************************)


      begin (* FEHLER *)
        RICHTIG := FALSE ;
        if ERSTERFEHLER then
          begin
            WRITELN ( 'KONSISTENZPRUEFUNG:' ) ;
            WRITELN ;
            ERSTERFEHLER := FALSE
          end (* then *) ;
        if ( NR = 5 ) or ( NR = 6 ) then
          WRITE ( 'IN ANSCHLUSS ZU' , BTRANSFER [ I ] : 10 , ' : ' ) ;
        if ( NR = 17 ) or ( NR = 18 ) then
          begin
            WRITE ( 'IN STRASSE ' ) ;
            SBEZAUSG ( OUTPUT , STR , 14 ) ;
            WRITE ( ' : ' )
          end (* then *) ;
        case NR of
          1 : WRITELN ( 'BEZIRKE NICHT EINDEUTIG ' ,
                        'ODER NICHT AUFSTEIGEND' ) ;
          2 : WRITELN ( 'BEZIRKE UEBER BZAHL UNGLEICH 0' ) ;
          3 : WRITELN ( 'KNOTENPUNKTE NICHT EINDEUTIG ' ,
                        'ODER NICHT AUFSTEIGEND' ) ;
          4 : WRITELN ( 'KNOTENPUNKTE UEBER VPZAHL UNGLEICH 0' ) ;
          5 : WRITELN ( 'KNOTENPUNKT NICHT VORHANDEN' ) ;
          6 : WRITELN ( 'KNOTENPUNKT NICHT EINDEUTIG' ) ;
          9 : WRITELN ( 'STRASSEN NICHT EINDEUTIG ' ,
                        'ODER NICHT AUFSTEIGEND' ) ;
          14 : WRITELN ( 'BEZIRK KLEINER 0 NICHT ERLAUBT' ) ;
          15 : WRITELN ( 'KNOTENPUNKT KLEINER 0 NICHT ERLAUBT' ) ;
          16 : WRITELN ( 'STRASSENNAME UNZULAESSIG' ) ;
          17 : WRITELN ( 'STRASSENAUFBAU UNZULAESSIG' ) ;
          18 : WRITELN ( 'KNOTENPUNKT NICHT VORHANDEN' ) ;
        end (* case *)
      end (* FEHLER *) ;


   begin (* KONSISTENZ *)
     RICHTIG := TRUE ;
     ERSTERFEHLER := TRUE ;
     if BZAHL > 0 then
       if BTRANSFER [ 1 ] <= 0 then
         FEHLER ( 14 ) ;
     for I := 2 to BZAHL do
       if BTRANSFER [ I ] <= 0 then
         FEHLER ( 14 )
       else
         if BTRANSFER [ I ] <= BTRANSFER [ I - 1 ] then
           FEHLER ( 1 ) ;
     for I := BZAHL + 1 to BMAX do
       if BTRANSFER [ I ] <> 0 then
         FEHLER ( 2 ) ;
     if VPZAHL > 0 then
       if VPTRANSFER [ 1 ] <= 0 then
         FEHLER ( 15 ) ;
     for I := 2 to VPZAHL do
       if VPTRANSFER [ I ] <= 0 then
         FEHLER ( 15 )
       else
         if VPTRANSFER [ I ] <= VPTRANSFER [ I - 1 ] then
           FEHLER ( 3 ) ;
     for I := VPZAHL + 1 to VPMAX do
       if VPTRANSFER [ I ] <> 0 then
         FEHLER ( 4 ) ;
     for I := 1 to BZAHL do
       begin
         BKOPF := ANSCHLUESSE [ I ] ;
         if BKOPF <> NIL then
           begin
             BPTR := BKOPF ;
             while BPTR <> NIL do
               begin
                 VPTEST := BPTR -> . VKPUNKT ;
                 SEARCHVP ( VPTEST , VPIND , STATUS ) ;
                 if STATUS <> 0 then
                   FEHLER ( 5 ) ;
                 BPTR2 := BKOPF ;
                 while BPTR2 <> BPTR do
                   begin
                     if BPTR2 -> . VKPUNKT = VPTEST then
                       FEHLER ( 6 ) ;
                     BPTR2 := BPTR2 -> . NEXT
                   end (* while *) ;
                 BPTR := BPTR -> . NEXT
               end (* while *)
           end (* then *)
       end (* for *) ;
     if STRZAHL > 0 then
       begin
         STR := STRASSENNETZ [ 1 ] ;
         if not ( STR . KENNUNG in KENNMENGE ) then
           FEHLER ( 16 )
         else
           if not ( STR . NUMMER > 0 ) then
             FEHLER ( 16 )
       end (* then *) ;
     for I := 2 to STRZAHL do
       begin
         STR := STRASSENNETZ [ I ] ;
         if not ( STR . KENNUNG in KENNMENGE ) then
           FEHLER ( 16 )
         else
           if not ( STR . NUMMER > 0 ) then
             FEHLER ( 16 )
           else
             if not KLEINERSTR ( STRASSENNETZ [ I - 1 ] , STR ) then
               FEHLER ( 9 )
       end (* for *) ;
     for I := 1 to STRZAHL do
       begin
         STR := STRASSENNETZ [ I ] ;
         if ( STR . ANFANG = NIL ) or ( STR . ENDE = NIL ) or ( STR .
         ANFANG = STR . ENDE ) then
           FEHLER ( 17 )
         else
           begin
             STP := STR . ANFANG ;
             while STP <> NIL do
               begin
                 VPTEST := STP -> . VKPUNKT ;
                 SEARCHVP ( VPTEST , VPIND , STATUS ) ;
                 if STATUS <> 0 then
                   FEHLER ( 18 ) ;
                 STP := STP -> . NEXT
               end (* while *)
           end (* else *)
       end (* for *)
   end (* KONSISTENZ *) ;



procedure BELEGUNG ;

(****************************************************)
(*   AUSGABE INFORMATIONEN UEBER SPEICHERBELEGUNG   *)
(****************************************************)


   begin (* BELEGUNG *)
     WRITELN ( 'SPEICHERBELEGUNG:' ) ;
     WRITELN ;
     WRITE ( 'KNOTENPUNKTE:' ) ;
     WRITE ( VPZAHL : 5 ) ;
     WRITE ( ' VON' ) ;
     WRITE ( VPMAX : 5 ) ;
     WRITE ( '; ' ) ;
     WRITE ( VPZAHL / VPMAX * 100 : 6 : 2 ) ;
     WRITELN ( ' PROZENT' ) ;
     WRITE ( 'STRASSEN:    ' ) ;
     WRITE ( STRZAHL : 5 ) ;
     WRITE ( ' VON' ) ;
     WRITE ( STRMAX : 5 ) ;
     WRITE ( '; ' ) ;
     WRITE ( STRZAHL / STRMAX * 100 : 6 : 2 ) ;
     WRITELN ( ' PROZENT' ) ;
     WRITE ( 'BEZIRKE:     ' ) ;
     WRITE ( BZAHL : 5 ) ;
     WRITE ( ' VON' ) ;
     WRITE ( BMAX : 5 ) ;
     WRITE ( '; ' ) ;
     WRITE ( BZAHL / BMAX * 100 : 6 : 2 ) ;
     WRITELN ( ' PROZENT' ) ;
     WRITELN
   end (* BELEGUNG *) ;



begin (* HAUPTPROGRAMM *)
  MODNAME := ' ' ;
  MOVEPARM ( ADDR ( MODNAME ) , 8 ) ;
  TOUPPERS ( ADDR ( MODNAME ) , 8 ) ;

  (***********************************************)
  (*   ACHTUNG: FUNCTION PARMS IST NONSTANDARD   *)
  (***********************************************)

  UPDATE := FALSE ;
  DELKETTE := NIL ;
  KENNMENGE := [ 'A' , 'B' , 'K' , 'L' , 'S' ] ;
  RICHTMENGE := [ 'H' , 'R' ] ;
  SITMIN := HAUPT ;
  SITMAX := SPAET ;
  SITTEXTE [ HAUPT ] := 'HAUPT ' ;
  SITTEXTE [ NORMAL ] := 'NORMAL' ;
  SITTEXTE [ SPAET ] := 'SPAET ' ;
  TERMOUT ( OUTPUT ) ;
  TERMIN ( INPUT ) ;
  SEITENZAHL := 0 ;
  ZEILZAHL := 0 ;
  INIT ;
  WRITELN ;
  WRITELN ( 'INIT FERTIG' ) ;
  WRITELN ;
  BELEGUNG ;
  KONSISTENZ ( RICHTIG ) ;
  if RICHTIG then
    WRITELN ( 'KONSISTENZPRUEFUNG FEHLERFREI' )
  else
    begin
      WRITELN ;
      WRITELN ( 'KONSISTENZPRUEFUNG FEHLERHAFT' )
    end (* else *) ;
  WRITELN ;
  WRITELN ( 'BITTE ENTER DRUECKEN' ) ;
  WAIT ;
  CONTROL ;
  if SEITENZAHL <> 0 then
    begin
      WRITELN ( DRUCKER ) ;
      WRITELN ( DRUCKER , '1' )
    end (* then *) ;
  if UPDATE then
    begin
      CLRSCRN ;
      BELEGUNG
    end (* then *)
end (* HAUPTPROGRAMM *) .
