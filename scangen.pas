program SCANGEN ( INPUT , OUTPUT , QUELLE , AUTOM , MUSTER ) ;

(********************************************************************)
(*                                                                  *)
(*   PROJEKT:  AUTOMATISCHES ERZEUGEN VON ENDLICHEN AUTOMATEN       *)
(*             AUS REGULAEREN AUSDRUECKEN.                          *)
(*                                                                  *)
(*   DURCHGEFUEHRT IM RAHMEN DES SOFTWARE-PRAKTIKUM II              *)
(*             IM WS 1980 / 81 VON                                  *)
(*                                                                  *)
(*                      THOMAS KRIMMER                              *)
(*                      ECKART MESSERSCHMIED                        *)
(*                      BERND OPPOLZER                              *)
(*                      BERND RUHLAND                               *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*   AENDERUNG AM 15.11.1996 (OPPOLZER):                            *)
(*                                                                  *)
(*   ERWEITERUNG IN RICHTUNG SCANNERGENERATOR:                      *)
(*                                                                  *)
(*   - UM DIE VERSCHIEDENEN SYMBOLE UNTERSCHEIDEN ZU KOENNEN,       *)
(*     WIRD AUF DER OBERSTEN EBENE (STIEFE = 0) DIE PROZEDUR        *)
(*     OPTIMIERUNG NICHT AUFGERUFEN, DADURCH UNTERSCHIEDLICHE       *)
(*     ENDZUSTAENDE.                                                *)
(*                                                                  *)
(*   - IN DEN PRODUKTIONEN WURDE DAS FELD TERMNR HINZUGEFUEGT,      *)
(*     DAMIT ERKENNBAR WIRD, WELCHE ENDZUSTAENDE ZU WELCHEN         *)
(*     TERMEN GEHOEREN.                                             *)
(*                                                                  *)
(*   - DIE PROZEDUR DETAUTOMAT WURDE ABGEAENDERT, UM DIESES         *)
(*     FELD TERMNR UNBESCHADET DURCH DEN UMBAUMECHANISMUS           *)
(*     DURCHZUFUEHREN.                                              *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*   AENDERUNG AM 16.11.1996 (OPPOLZER):                            *)
(*                                                                  *)
(*   SCANNERGENERATOR IST FERTIG:                                   *)
(*                                                                  *)
(*   - ERZEUGEN EINES C-PROGRAMMS, DAS Z.B. EIN GANZES PROGRAMM     *)
(*     EINLIEST UND DIE SYMBOLE ZURUECKGIBT.                        *)
(*                                                                  *)
(*   - ERSATZDARSTELLUNGEN ERLAUBEN (Z.B. \N, \0, \XFF USW.)        *)
(*                                                                  *)
(*   - BEI TRANSITIVER HUELLE AUSSER * AUCH + ZULASSEN; DER         *)
(*     ENTSPRECHENDE TERM MUSS BEI + MINDESTENS EINMAL VORHANDEN    *)
(*     SEIN.                                                        *)
(*                                                                  *)
(********************************************************************)


type PRODVER = -> PRODUKTION ;
     PRODUKTION = record
                    LINKS : INTEGER ;
                    TERMINAL : CHAR ;
                    RECHTS : INTEGER ;
                    TERMNR : INTEGER ;
                    NEXT : PRODVER
                  end ;

     (**********************************************************)
     (*                                                        *)
     (*   DIE ERZEUGTEN AUTOMATEN WERDEN INTERN DURCH          *)
     (*   ZWEI LISTEN DARGESTELLT. DIE ERSTE, AUFGEBAUT        *)
     (*   AUS ELEMENTEN VOM TYP 'PRODUKTION', ENTHAELT         *)
     (*   DIE ZUSTANDSUEBERGAENGE DES AUTOMATEN IN DER         *)
     (*   FORM  <ZUSTAND-LINKS> <EINGELESENES-ZEICHEN>         *)
     (*   <ZUSTAND-RECHTS>. DIE ZUSTAENDE WERDEN DURCH         *)
     (*   'INTEGER'-GROESSEN DARGESTELLT, DER ANFANGS-         *)
     (*   ZUSTAND IST 1.                                       *)
     (*                                                        *)
     (*   DIE ZWEITE KETTE ENTHAELT DIE ZULAESSIGEN            *)
     (*   ENDZUSTAENDE. FUER DIE TERMINALZEICHEN IST           *)
     (*   DER VOLLE ZEICHENSATZ DER ZIELMASCHINE (IM           *)
     (*   VORLIEGENDEN FALL: TR440) ZULAESSIG, ALSO DER        *)
     (*   DATENTYP 'CHAR'.                                     *)
     (*                                                        *)
     (*   DAS PROGRAMM IST IN DREI GROSSE, WEITGEHEND          *)
     (*   VONEINANDER UNABHAENGIGE MODULE AUFGETEILT.          *)
     (*   IHRE AUFGABE WIRD WIE FOLGT BESCHRIEBEN:             *)
     (*                                                        *)
     (*   1. DER ERSTE TEIL PRUEFT DEN EINGEGEBENEN            *)
     (*      REGULAEREN AUSDRUCK AUF FORMALE KORREKT-          *)
     (*      HEIT UND ERZEUGT GLEICHZEITIG ALS ERSTES          *)
     (*      ZWISCHENERGEBNIS EINEN INDETERMINISTISCHEN        *)
     (*      ENDLICHEN AUTOMATEN IN DER OBEN BESCHRIEBENEN     *)
     (*      DARSTELLUNGSFORM ('PROCEDURE SYNTAXPRUEFUNG').    *)
     (*                                                        *)
     (*   2. DER ZWEITE TEIL MACHT AUS DEM INDETERMI-          *)
     (*      NISTISCHEN AUTOMATEN EINEN DETERMINISTISCHEN      *)
     (*      ('PROCEDURE DETAUTOMAT').                         *)
     (*                                                        *)
     (*   3. DER DRITTE TEIL SCHLIESSLICH GENERIERT EIN        *)
     (*      PASCAL-PROGRAMM, DAS DEN DETERMINISTISCHEN        *)
     (*      AUTOMATEN SIMULIERT                               *)
     (*      ('PROCEDURE AUTOMGENERATOR').                     *)
     (*                                                        *)
     (*   DIE INFORMATIONSUEBERGABE VON EINEM TEIL ZUM         *)
     (*   NAECHSTEN ERFOLGT EINFACH DADURCH, DASS DER          *)
     (*   ANFANGSBEZUG DER BEIDEN LISTEN (DER ANKER)           *)
     (*   ALS PARAMETER UEBERGEBEN WIRD.                       *)
     (*                                                        *)
     (**********************************************************)

     ZUSTVERW = -> ZUSTAND ;
     ZUSTAND = record
                 WERT : INTEGER ;
                 TERMNR : INTEGER ;
                 LINK : ZUSTVERW
               end ;

var QUELLE , AUTOM , MUSTER : TEXT ;
    PRODKETTE : PRODVER ;
    OK : BOOLEAN ;
    ENDZUSTAENDE : ZUSTVERW ;
    RESERVE1 : PRODVER ;
    RESERVE2 : ZUSTVERW ;
    AKZUSTAND : INTEGER ;

    (**********************************************************)
    (*                                                        *)
    (*   IM HAUPTPROGRAMM DEFINIERTE VARIABLEN:               *)
    (*                                                        *)
    (*   QUELLE          - DER TEXTFILE, VON DEM DER          *)
    (*                     REGULAERE AUSDRUCK EINGE-          *)
    (*                     LESEN WIRD                         *)
    (*                                                        *)
    (*                     TR440-ANSCHLUSSKONVENTIONEN:       *)
    (*                                                        *)
    (*                       #STARTE,...,                     *)
    (*                        DATEI=(QUELLE)-<DATEINAME>      *)
    (*                                                        *)
    (*   AUTOM           - DER TEXTFILE, IN DEM DAS           *)
    (*                     GENERIERTE PASCAL-PROGRAMM         *)
    (*                     ABGELEGT WIRD.                     *)
    (*                                                        *)
    (*   PRODKETTE       - GLOBALER ANKER FUER DIE            *)
    (*                     KETTE DER UEBERGAENGE              *)
    (*                     (ODER AUCH PRODUKTIONEN)           *)
    (*                                                        *)
    (*   OK              - BOOLE'SCHE VARIABLE,               *)
    (*                     AM ANFANG TRUE,                    *)
    (*                     SOBALD BEI DER SYNTAXPRUEFUNG      *)
    (*                     EIN FEHLER ERKANNT WIRD,           *)
    (*                     AUF FALSE GESETZT (STEUERT         *)
    (*                     DIE CODEGENERIERUNG IN AB-         *)
    (*                     HAENGIGKEIT VON ERKANNTEN          *)
    (*                     FEHLERN).                          *)
    (*                                                        *)
    (*   ENDZUSTAENDE    - WIE PRODKETTE, NUR FUER DIE        *)
    (*                     KETTE DER ENDZUSTAENDE.            *)
    (*                                                        *)
    (*   RESERVE1        - GLOBALER ANKER FUER RESERVE-       *)
    (*                     KETTE DER ZUSTANDSUEBERGAENGE,     *)
    (*                     BEDEUTUNG SIEHE PROZEDUREN         *)
    (*                     'NEW1', 'DISPOSE1'.                *)
    (*                                                        *)
    (*   RESERVE2        - WIE RESERVE1, NUR FUER DIE         *)
    (*                     RESERVEKETTE DER ENDZUSTAENDE.     *)
    (*                                                        *)
    (*   AKZUSTAND       - GLOBALER PEGEL FUER DEN HOECH-     *)
    (*                     STEN VERWENDETEN ZUSTAND           *)
    (*                     (ZUR ERINNERUNG:                   *)
    (*                          ZUSTAND = INTEGER).           *)
    (*                                                        *)
    (**********************************************************)



function ISTELLEN ( W : INTEGER ) : INTEGER ;

   var STELLEN : INTEGER ;

   begin (* ISTELLEN *)
     STELLEN := 0 ;
     if W < 0 then
       begin
         W := - W ;
         STELLEN := 1
       end (* then *) ;
     if W > 99999 then
       STELLEN := STELLEN + 6
     else
       if W > 9999 then
         STELLEN := STELLEN + 5
       else
         if W > 999 then
           STELLEN := STELLEN + 4
         else
           if W > 99 then
             STELLEN := STELLEN + 3
           else
             if W > 9 then
               STELLEN := STELLEN + 2
             else
               STELLEN := STELLEN + 1 ;
     ISTELLEN := STELLEN
   end (* ISTELLEN *) ;


procedure NEW1 ( var PTR : PRODVER ) ;

(************************************************)
(*                                              *)
(*   EIGENE FREISPEICHERVERWALTUNG (HEAP):      *)
(*                                              *)
(*   FALLS NOCH FREIE ELEMENTE IN RESERVE-      *)
(*   KETTE, HOLE ELEMENT VON DORT,              *)
(*   ANSONSTEN ZURUECKFUEHREN AUF STANDARD-     *)
(*   'NEW'.                                     *)
(*                                              *)
(************************************************)


   begin (* NEW1 *)
     if RESERVE1 <> NIL then
       begin
         PTR := RESERVE1 ;
         RESERVE1 := RESERVE1 -> . NEXT ;
         PTR -> . NEXT := NIL
       end (* then *)
     else
       begin
         NEW ( PTR ) ;
         PTR -> . NEXT := NIL
       end (* else *)
   end (* NEW1 *) ;


procedure DISPOSE1 ( var PTR : PRODVER ) ;

(************************************************)
(*                                              *)
(*   EIGENE FREISPEICHERVERWALTUNG:             *)
(*                                              *)
(*   LEGE NICHT MEHR BENOETIGTES ELEMENT        *)
(*   AUF RESERVE-KETTE, DAMIT EVTL. FOLGENDE    *)
(*   'NEW1'-AUFRUFE SICH DORT BEDIENEN          *)
(*   KOENNEN.                                   *)
(*                                              *)
(************************************************)


   var P1 : PRODVER ;

   begin (* DISPOSE1 *)
     P1 := PTR ;
     while PTR -> . NEXT <> NIL do
       PTR := PTR -> . NEXT ;
     PTR -> . NEXT := RESERVE1 ;
     RESERVE1 := P1 ;
     PTR := NIL
   end (* DISPOSE1 *) ;


procedure NEW2 ( var PTR : ZUSTVERW ) ;

(**************************************)
(*                                    *)
(*   SIEHE KOMMENTAR ZU 'NEW1'        *)
(*                                    *)
(**************************************)


   begin (* NEW2 *)
     if RESERVE2 <> NIL then
       begin
         PTR := RESERVE2 ;
         RESERVE2 := RESERVE2 -> . LINK ;
         PTR -> . TERMNR := 0 ;
         PTR -> . LINK := NIL
       end (* then *)
     else
       begin
         NEW ( PTR ) ;
         PTR -> . TERMNR := 0 ;
         PTR -> . LINK := NIL
       end (* else *)
   end (* NEW2 *) ;


procedure DISPOSE2 ( var PTR : ZUSTVERW ) ;

(**************************************)
(*                                    *)
(*   SIEHE KOMMENTAR ZU 'DISPOSE1'    *)
(*                                    *)
(**************************************)


   var P1 : ZUSTVERW ;

   begin (* DISPOSE2 *)
     P1 := PTR ;
     while PTR -> . LINK <> NIL do
       PTR := PTR -> . LINK ;
     PTR -> . LINK := RESERVE2 ;
     RESERVE2 := P1 ;
     PTR := NIL
   end (* DISPOSE2 *) ;


procedure CHAROUT ( var F : TEXT ; CH : CHAR ; DICHT : BOOLEAN ) ;

   const HEXKETTE = '0123456789abcdef' ;

   var ANZAHL : INTEGER ;

   begin (* CHAROUT *)
     WRITE ( F , '''' ) ;
     ANZAHL := 2 ;
     if CH = CHR ( 0 ) then
       WRITE ( F , '\0' )
     else
       if CH = CHR ( 0 ) then
         WRITE ( F , '\0' )
       else
         if CH = CHR ( 10 ) then
           WRITE ( F , '\n' )
         else
           if CH = CHR ( 9 ) then
             WRITE ( F , '\t' )
           else
             if ( CH < ' ' ) or ( CH > CHR ( 127 ) ) then
               begin
                 ANZAHL := 4 ;
                 WRITE ( F , '\x' ) ;
                 WRITE ( F , HEXKETTE [ ORD ( CH ) DIV 16 + 1 ] ) ;
                 WRITE ( F , HEXKETTE [ ORD ( CH ) MOD 16 + 1 ] ) ;
               end (* then *)
             else
               if CH = '''' then
                 WRITE ( F , '\''' )
               else
                 if CH = '\' then
                   WRITE ( F , '\\' )
                 else
                   if CH = '"' then
                     WRITE ( F , '\"' )
                   else
                     if CH = '?' then
                       WRITE ( F , '\?' )
                     else
                       begin
                         WRITE ( F , CH ) ;
                         ANZAHL := 1
                       end (* else *) ;
     WRITE ( F , '''' ) ;
     if not DICHT then
       while ANZAHL <= 4 do
         begin
           WRITE ( F , ' ' ) ;
           ANZAHL := ANZAHL + 1
         end (* while *) ;
   end (* CHAROUT *) ;


procedure TESTDRUCK ( PRODKETTE : PRODVER ) ;

(****************************)
(*                          *)
(*   ENTFAELLT.             *)
(*                          *)
(****************************)


   begin (* TESTDRUCK *)
     WRITELN ;
     WRITELN ( 'S-Zust  Z          E-Zust    TermNr' ) ;
     WRITELN ( '-----------------------------------' ) ;
     while PRODKETTE <> NIL do
       begin
         if PRODKETTE -> . LINKS <> MAXINT then
           begin
             WRITE ( PRODKETTE -> . LINKS : 6 ) ;
             WRITE ( '  ' ) ;
             CHAROUT ( OUTPUT , PRODKETTE -> . TERMINAL , FALSE ) ;
             WRITE ( ' -> ' ) ;
             WRITE ( PRODKETTE -> . RECHTS : 6 ) ;
             WRITE ( PRODKETTE -> . TERMNR : 10 ) ;
             WRITELN
           end (* then *) ;
         PRODKETTE := PRODKETTE -> . NEXT
       end (* while *) ;
     WRITELN ;
   end (* TESTDRUCK *) ;


procedure ENDDRUCK ( ANKER : ZUSTVERW ) ;

(****************************)
(*                          *)
(*   ENTFAELLT.             *)
(*                          *)
(****************************)


   begin (* ENDDRUCK *)
     WRITELN ;
     WRITELN ( 'E-Zust  TermNr' ) ;
     WRITELN ( '--------------' ) ;
     while ANKER <> NIL do
       begin
         WRITE ( ANKER -> . WERT : 6 ) ;
         WRITE ( ANKER -> . TERMNR : 8 ) ;
         WRITELN ;
         ANKER := ANKER -> . LINK
       end (* while *) ;
     WRITELN ;
   end (* ENDDRUCK *) ;


function ENDZUST_TERMNR ( ANKER : ZUSTVERW ; PRODKETTE : PRODVER ) :
                        BOOLEAN ;

(****************************)
(*                          *)
(*   ENTFAELLT.             *)
(*                          *)
(****************************)


   var PLAUF : PRODVER ;
       TERMNR : INTEGER ;
       OK : BOOLEAN ;
       ZUST : INTEGER ;

   begin (* ENDZUST_TERMNR *)
     OK := TRUE ;
     while ANKER <> NIL do
       begin
         ZUST := ANKER -> . WERT ;
         TERMNR := - 1 ;
         PLAUF := PRODKETTE ;
         while PLAUF <> NIL do
           begin
             if ( PLAUF -> . LINKS > 0 ) and ( PLAUF -> . RECHTS = ZUST
             ) then
               begin
                 if TERMNR = - 1 then
                   TERMNR := PLAUF -> . TERMNR
                 else
                   if TERMNR <> PLAUF -> . TERMNR then
                     OK := FALSE
               end (* then *) ;
             PLAUF := PLAUF -> . NEXT ;
           end (* while *) ;
         if TERMNR <= 0 then
           OK := FALSE ;
         ANKER -> . TERMNR := TERMNR ;
         ANKER := ANKER -> . LINK
       end (* while *) ;
     ENDZUST_TERMNR := OK ;
   end (* ENDZUST_TERMNR *) ;


procedure SYNTAXPRUEFUNG ( var PKETTE : PRODVER ; var OK : BOOLEAN ;
                         var EZUSTAND : ZUSTVERW ; var AKZUSTAND :
                         INTEGER ) ;

(********************************************************************)
(*                                                                  *)
(*   PROZEDUR 'SYNTAXPRUEFUNG', TEIL 1.                             *)
(*   AUFGABEN (WIE BEREITS OBEN GESAGT):                            *)
(*                                                                  *)
(*   UEBERPRUEFEN DER FORMALEN KORREKTHEIT DES EINGEGEBENEN         *)
(*   REGULAEREN AUSDRUCKS UND GENERIEREN EINES AEQUIVALENTEN        *)
(*   ENDLICHEN (ABER IM ALLGEMEINEN NICHTDETERMINISTISCHEN)         *)
(*   AUTOMATEN.                                                     *)
(*                                                                  *)
(*   METHODE DER SYNTAXANALYSE: RECURSIVE DESCENT.                  *)
(*                                                                  *)
(*   ZUGRUNDELIEGENDE GRAMMATIK FUER REGULAERE AUSDRUECKE:          *)
(*                                                                  *)
(*      AUSDRUCK -> AUSDRUCK / TERM            *** ALTERNATIVE ***  *)
(*      AUSDRUCK -> TERM                                            *)
(*      TERM     -> TERM  FAKTOR             *** KONKATENATION ***  *)
(*      TERM     -> FAKTOR                                          *)
(*      FAKTOR   -> F2 *                     *** TRANS. HUELLE ***  *)
(*      FAKTOR   -> F2 +                     *** HUELLE, MIND. 1 *  *)
(*      FAKTOR   -> F2                                              *)
(*      F2       -> ' STRING '                                      *)
(*      F2       -> ( AUSDRUCK )                                    *)
(*      F2       -> ' BUCHST ' .. ' BUCHST '       *** VON-BIS ***  *)
(*                                                                  *)
(*   WOBEI STRING EIN STRING GEMAESS PASCAL-SYNTAX SEIN KANN,       *)
(*   BUCHST EIN ELEMENT VOM TYP 'CHAR'                              *)
(*   (ALSO: APOSTROPH WIRD IM STRING DURCH ZWEI APO-                *)
(*          STROPHE DARGESTELLT;                                    *)
(*          DAS BEDINGT AUCH, DASS IN DER PRODUKTION                *)
(*          TERM -> TERM  FAKTOR EIN LEERZEICHEN                    *)
(*          DAZWISCHENSTEHEN MUSS)                                  *)
(*                                                                  *)
(*   DIE EINGABEN SIND FORMATFREI.                                  *)
(*                                                                  *)
(*   NAEHERES SIEHE AUSFUEHRLICHE BENUTZERBESCHREIBUNG              *)
(*                                                                  *)
(********************************************************************)


   label 999 ;

         (**************************************)
         (*                                    *)
         (*   GLOBALES LABEL 999 FUER DEN      *)
         (*   FALL, DASS BEIM LESEN DER        *)
         (*   QUELLE AN UNZULAESSIGER STELLE   *)
         (*   DATEIENDE FESTGESTELLT WIRD.     *)
         (*     (FEHLERMELDUNG)                *)
         (*                                    *)
         (**************************************)


   type FEHLERPTR = -> FEHLERMELDUNG ;
        FEHLERMELDUNG = record
                          NUMMER , PLATZ : INTEGER ;
                          NAECHST : FEHLERPTR
                        end ;

        (**********************************************************)
        (*                                                        *)
        (*   FUER DAS DRUCKPROTOKOLL:                             *)
        (*                                                        *)
        (*   UNTER JEDER ZEILE AUS DER QUELLE WERDEN FEHLER-      *)
        (*   MELDUNGEN IM KLARTEXT AUSGEGEBEN UND EIN SENK-       *)
        (*   RECHTER STRICH UNTER DIE FEHLERSTELLE. IN DER        *)
        (*   HIER DEFINIERTEN VERKETTETEN LISTE WERDEN DIE        *)
        (*   FEHLERELEMENTE FUER EINE ZEILE, BESTEHEND AUS        *)
        (*   FEHLERNUMMER UND FEHLERORT, ABGELEGT.                *)
        (*                                                        *)
        (*   BEIM ZEILENWECHSEL WIRD DIESE LISTE DANN AUS-        *)
        (*   GEWERTET (SIEHE PROCEDURE 'FEHLERAUSGABE').          *)
        (*                                                        *)
        (**********************************************************)


   var AZUSTAND : ZUSTVERW ;
       STELLE : INTEGER ;
       CH : CHAR ;
       FEANFANG , FKETTE : FEHLERPTR ;
       KETTE : PRODVER ;
       ANFANG : BOOLEAN ;
       FEZAHL : INTEGER ;
       EOLNMOEGLICH : BOOLEAN ;
       PSEUDOPROD : PRODUKTION ;

       (****************************)
       (*   NEUE VARIABLEN 11.96   *)
       (****************************)

       AKTTERMNR : INTEGER ;

       (**********************************************************)
       (*                                                        *)
       (*   BEDEUTUNG DER ZU 'SYNTAXPRUEFUNG' LOKALEN            *)
       (*   VARIABLEN:                                           *)
       (*                                                        *)
       (*   AZUSTAND        - FUER JEDES NONTERMINALSYMBOL       *)
       (*                     DER OBEN BESCHRIEBENEN GRAMMA-     *)
       (*                     TIK EXISTIERT BEI DER METHODE      *)
       (*                     DES 'RECURSIVE DESCENT' EINE       *)
       (*                     PROZEDUR. DIESE PROZEDUREN         *)
       (*                     BEKOMMEN ALS SEMANTISCHE EIN-      *)
       (*                     GANGSINFORMATION EINE KETTE        *)
       (*                     VON ZUSTAENDEN, IN DENEN SICH      *)
       (*                     DER AUTOMAT AN DIESER STELLE       *)
       (*                     BEFINDEN KANN. DER BEZUG AUF       *)
       (*                     DIESE KETTE STEHT IN 'AZUSTAND'.   *)
       (*                                                        *)
       (*   STELLE          - DIE LESEPOSITION INNERHALB         *)
       (*                     EINER ZEILE.                       *)
       (*                                                        *)
       (*   CH              - VARIABLE ZUM EINLESEN DES          *)
       (*                     NAECHSTEN ZEICHENS.                *)
       (*                                                        *)
       (*   FEANFANG,FKETTE - HILFSPOINTER ZUR MANIPULATION      *)
       (*                     DER FEHLERKETTE. (SIEHE OBEN       *)
       (*                     BZW. PROZEDUR 'FEHLERAUSGABE').    *)
       (*                                                        *)
       (*   KETTE           - HILFSPOINTER, UM AUGENBLICKLICHE   *)
       (*                     SCHREIBPOSITION BEI PRODKETTE      *)
       (*                     ZU MERKEN.                         *)
       (*                                                        *)
       (*   ANFANG          - BOOLE'SCHE VARIABLE, UM AM         *)
       (*                     ANFANG KUENSTLICH DIE ZEILEN-      *)
       (*                     WECHSEL-ROUTINE AUFZURUFEN         *)
       (*                     (IN PROZEDUR 'READQ').             *)
       (*                     IST NOETIG, UM DIE ERSTE           *)
       (*                     'LINENUMBER'-AUSGABE ZU ER-        *)
       (*                     MOEGLICHEN.                        *)
       (*                                                        *)
       (*   FEZAHL          - ZAHL DER FEHLER, DIE WAEHREND      *)
       (*                     DER SYNTAXPRUEFUNG AUFTRETEN.      *)
       (*                                                        *)
       (*   EOLNMOEGLICH    - BOOLE'SCHE VARIABLE,               *)
       (*                     WIRD AUF -FALSE- GESETZT, WENN     *)
       (*                     SYNTAKTISCH KEIN ZEILENENDE ZU-    *)
       (*                     LAESSIG IST; SO KANN DIE LESE-     *)
       (*                     PROZEDUR 'READQ' DANN EINEN        *)
       (*                     FEHLER MELDEN.                     *)
       (*                                                        *)
       (*   PSEUDOPROD      - PSEUDO-UEBERGANG,                  *)
       (*                     DIENT ALS BREMSE BEIM SORTIE-      *)
       (*                     REN DER PRODUKTIONEN (ZUSTAND-     *)
       (*                     LINKS = ZUSTAND-RECHTS =           *)
       (*                     MAXINT)                            *)
       (*                                                        *)
       (**********************************************************)



   procedure EZSORT ( var EZKETTE : ZUSTVERW ) ;

   (************************************************)
   (*                                              *)
   (*   AUFSTEIGENDES SORTIEREN DER                *)
   (*   ENDZUSTAENDE (INNERHALB DER                *)
   (*   VERKETTETEN LISTE).                        *)
   (*                                              *)
   (************************************************)


      var NEUKETTE , NEU1 , PUFFER , MINIMUMVOR , MINIMUM , P1VOR , P1
          : ZUSTVERW ;

      begin (* EZSORT *)
        NEW2 ( NEUKETTE ) ;
        NEUKETTE -> . LINK := NIL ;
        NEU1 := NEUKETTE ;
        NEW2 ( PUFFER ) ;
        PUFFER -> . LINK := EZKETTE ;

        (************************************************)
        (*                                              *)
        (*   DIE IN DER LISTE 'EZKETTE' ABGELEGTEN      *)
        (*   ENDZUSTAENDE SOLLEN IN DER RICHTIGEN       *)
        (*   REIHENFOLGE IN EINE LISTE 'NEUKETTE'       *)
        (*   ABGELEGT WERDEN. DAZU WIRD ZUERST AN       *)
        (*   'EZKETTE' VORNE EIN ZUSAETZLICHES ELE-     *)
        (*   MENT ANGEHAENGT, DESGL. WIRD EIN AN-       *)
        (*   FANGS-ELEMENT IN NEUKETTE ABGELEGT.        *)
        (*   (DAS HILFT BEI DER BEHANDLUNG VON          *)
        (*   SONDERFAELLEN).                            *)
        (*                                              *)
        (*   IM ANSCHLUSS DARAN WIRD IN DEN BEIDEN      *)
        (*   GESCHACHTELTEN WHILE-SCHLEIFEN DAS         *)
        (*   JEWEILS KLEINSTE ELEMENT AUS DER           *)
        (*   'EZKETTE' HERAUSGEHOLT, DORT AUSGE-        *)
        (*   KETTET UND IN DIE 'NEUKETTE' EIN-          *)
        (*   GEFUEGT.                                   *)
        (*                                              *)
        (*   AM SCHLUSS WERDEN DIE BEIDEN AM ANFANG     *)
        (*   EINGEFUEGTEN (ZUSATZ-)ELEMENTE WIEDER      *)
        (*   HERAUSGENOMMEN UND MITTELS 'DISPOSE2'      *)
        (*   WIEDER FREIGEGEBEN.                        *)
        (*                                              *)
        (************************************************)

        while EZKETTE <> NIL do
          begin
            MINIMUMVOR := PUFFER ;
            MINIMUM := EZKETTE ;
            P1VOR := EZKETTE ;
            P1 := EZKETTE -> . LINK ;
            while P1 <> NIL do
              begin
                if P1 -> . WERT < MINIMUM -> . WERT then
                  begin
                    MINIMUM := P1 ;
                    MINIMUMVOR := P1VOR
                  end (* then *) ;
                P1 := P1 -> . LINK ;
                P1VOR := P1VOR -> . LINK
              end (* while *) ;
            NEU1 -> . LINK := MINIMUM ;
            if MINIMUM = EZKETTE then
              EZKETTE := EZKETTE -> . LINK ;
            MINIMUMVOR -> . LINK := MINIMUM -> . LINK ;
            MINIMUM -> . LINK := NIL ;
            NEU1 := NEU1 -> . LINK
          end (* while *) ;
        EZKETTE := NEUKETTE -> . LINK ;
        NEUKETTE -> . LINK := NIL ;
        DISPOSE2 ( NEUKETTE ) ;
        DISPOSE2 ( PUFFER )
      end (* EZSORT *) ;


   procedure OPTIMIERUNG ( KETTANFANG : PRODVER ; var EZUSTAND :
                         ZUSTVERW ) ;

   (***************************************************************)
   (*                                                             *)
   (*   MIT DER PROZEDUR 'OPTIMIERUNG' WIRD BEREITS WAEHREND DER  *)
   (*   SYNTAXPRUEFUNG (PHASE 1) DIE ANZAHL DER ZUSTAENDE MINI-   *)
   (*   MIERT. VORGEHENSWEISE WIE FOLGT:                          *)
   (*                                                             *)
   (*   FUER JEDE SYNTAKTISCHE VARIABLE (Z.B. TERM) WIRD          *)
   (*   ALS SEMANTISCHE INFORMATION EINE KETTE VON ANFANGS-       *)
   (*   ZUSTAENDEN MITGEGEBEN (ALS PARAMETER). DIE ZUGEHOERIGE    *)
   (*   PROZEDUR UEBERPRUEFT DIE SYNTAX UND LIEFERT AUSSER-       *)
   (*   DEM ALS AUSGABE EINE KETTE VON ENDZUSTAENDEN. IN          *)
   (*   ABHAENGIGKEIT VON DER BEDEUTUNG DER JEW. SYNTAX-          *)
   (*   VARIABLEN WERDEN DIESE ZUSTAENDE DANN WEITER VER-         *)
   (*   ARBEITET. BEI DER SYNTAXVARIABLEN 'AUSDRUCK' PASSIERT     *)
   (*   FOLGENDES:                                                *)
   (*                                                             *)
   (*   WEGEN DER ALTERNATIVE ( TERM / TERM ) MUESSEN DIE         *)
   (*   VON DEN BEIDEN TERMEN ERZEUGTEN MENGEN VON END-           *)
   (*   ZUSTAENDEN ZUSAMMENGEFASST WERDEN. DIESE MENGEN           *)
   (*   SIND IM ALLGEMEINEN DISJUNKT. UM NUN DIE MAECHTIG-        *)
   (*   KEIT DIESER MENGEN ZU REDUZIEREN, FASST MAN ALLE DIE      *)
   (*   ENDZUSTAENDE, DIE BIS JETZT NOCH NIRGENDS AUF DER         *)
   (*   LINKEN SEITE EINER PRODUKTION VORGEKOMMEN SIND (ODER,     *)
   (*   UM DAS BILD DES AUTOMATEN ZU VERWENDEN, VON DENEN         *)
   (*   NOCH KEINE PFEILE -UEBERGAENGE- AUSGEHEN) ZU EINEM        *)
   (*   ZUSTAND ZUSAMMEN.                                         *)
   (*                                                             *)
   (*   TESTS HABEN GEZEIGT, DASS DIESES VERFAHREN BEREITS        *)
   (*   IN DER ERSTEN PHASE DIE ANZAHL DER ERZEUGTEN ZU-          *)
   (*   STAENDE GANZ WESENTLICH REDUZIERT, WEIL DIE ANZAHL        *)
   (*   DER ENDZUSTAENDE IN DIE NACHFOLGENDE BERECHNUNG WIEDER    *)
   (*   STARK MIT EINGEHT. EIN FRUEHZEITIG ALS UEBERFLUESSIG      *)
   (*   ERKANNTER NEUER ZUSTAND REDUZIERT DEN AUFWAND GANZ        *)
   (*   ERHEBLICH.                                                *)
   (*                                                             *)
   (***************************************************************)


      var KETT1 : PRODVER ;
          ZUST1 , ZUSTVOR : ZUSTVERW ;
          EZWERT : INTEGER ;
          LINKSGEFUNDEN : BOOLEAN ;
          EZANFANG , EZKETTE : ZUSTVERW ;

      begin (* OPTIMIERUNG *)
        NEW2 ( ZUST1 ) ;

        (*****************************************************)
        (*                                                   *)
        (*   PSEUDO-ELEMENTE EINFUEGEN (SIEHE EZSORT).       *)
        (*                                                   *)
        (*****************************************************)

        ZUST1 -> . LINK := EZUSTAND ;
        EZUSTAND := ZUST1 ;
        ZUSTVOR := EZUSTAND ;
        ZUST1 := ZUST1 -> . LINK ;
        NEW2 ( EZKETTE ) ;
        EZKETTE -> . LINK := NIL ;
        EZANFANG := EZKETTE ;

        (*****************************************************)
        (*                                                   *)
        (*   IN DEN NUN FOLGENDEN WHILE-SCHLEIFEN WERDEN     *)
        (*   DIE ENDZUSTAENDE, DIE NIRGENDS LINKS VOR-       *)
        (*   KOMMEN, AUS DER KETTE 'EZUSTAND' AUSGEKETTET    *)
        (*   UND IN DIE 'EZANFANG'-KETTE UEBERTRAGEN.        *)
        (*                                                   *)
        (*   (DIE LISTEN WERDEN HIER -UND AUCH WEITER        *)
        (*   OBEN- DURCH IHREN ANKER, D.H. DEN BEZUG         *)
        (*   AUF DEN ANFANG DER KETTE IDENTIFIZIERT.)        *)
        (*                                                   *)
        (*****************************************************)

        while ZUST1 <> NIL do
          begin
            EZWERT := ZUST1 -> . WERT ;
            KETT1 := KETTANFANG ;
            LINKSGEFUNDEN := FALSE ;
            while ( KETT1 <> NIL ) and not LINKSGEFUNDEN do
              begin
                LINKSGEFUNDEN := ( KETT1 -> . LINKS = EZWERT ) ;
                KETT1 := KETT1 -> . NEXT
              end (* while *) ;
            if not LINKSGEFUNDEN then
              begin
                EZKETTE -> . LINK := ZUST1 ;
                EZKETTE := EZKETTE -> . LINK ;
                ZUSTVOR -> . LINK := ZUST1 -> . LINK ;
                EZKETTE -> . LINK := NIL ;
                ZUST1 := ZUSTVOR -> . LINK
              end (* then *)
            else
              begin
                ZUST1 := ZUST1 -> . LINK ;
                ZUSTVOR := ZUSTVOR -> . LINK
              end (* else *)
          end (* while *) ;

        (*******************************************)
        (*                                         *)
        (*   PSEUDO-ELEMENTE HERAUSNEHMEN.         *)
        (*                                         *)
        (*******************************************)

        EZKETTE := EZANFANG ;
        EZANFANG := EZANFANG -> . LINK ;
        EZKETTE -> . LINK := NIL ;
        DISPOSE2 ( EZKETTE ) ;
        ZUST1 := EZUSTAND ;
        EZUSTAND := EZUSTAND -> . LINK ;
        ZUST1 -> . LINK := NIL ;
        DISPOSE2 ( ZUST1 ) ;
        if EZANFANG <> NIL then
          begin
            if EZANFANG -> . LINK <> NIL then
              begin

        (************************************************)
        (*                                              *)
        (*   FALLS MEHRERE ENDZUSTAENDE VORHANDEN       *)
        (*   SIND, DIE LINKS NIRGENDS VORKOMMEN,        *)
        (*   KOENNEN DIESE ZUSTAENDE ZUSAMMENGEFASST    *)
        (*   WERDEN. DER ERSTE AUS DER KETTE            *)
        (*   'EZANFANG' WIRD AUSGEWAEHLT UND DIE        *)
        (*   ANDEREN WERDEN UEBERALL DORT, WO SIE       *)
        (*   IN DEN PRODUKTIONEN AUFTAUCHEN, DURCH      *)
        (*   DIESEN ERSETZT.                            *)
        (*                                              *)
        (************************************************)

                KETT1 := KETTANFANG ;
                while KETT1 <> NIL do
                  begin
                    if KETT1 -> . LINKS <> - 1 then
                      begin
                        EZKETTE := EZANFANG -> . LINK ;
                        while EZKETTE <> NIL do
                          begin
                            if KETT1 -> . RECHTS = EZKETTE -> . WERT
                            then
                              KETT1 -> . RECHTS := EZANFANG -> . WERT ;
                            EZKETTE := EZKETTE -> . LINK
                          end (* while *)
                      end (* then *) ;
                    KETT1 := KETT1 -> . NEXT
                  end (* while *) ;

        (*******************************************)
        (*                                         *)
        (*   PEGEL FUER ZUSTAENDE ERNIEDRIGEN,     *)
        (*   FALLS MOEGLICH (D.H. FALLS DIE        *)
        (*   HOECHSTEN VORKOMMENDEN ZUSTAENDE      *)
        (*   ZU DEN UEBERFLUESSIGEN GEHOEREN)      *)
        (*                                         *)
        (*******************************************)

                EZKETTE := EZANFANG -> . LINK ;
                while EZKETTE <> NIL do
                  begin
                    if EZKETTE -> . WERT = AKZUSTAND then
                      begin
                        AKZUSTAND := AKZUSTAND - 1 ;
                        EZKETTE := EZANFANG -> . LINK
                      end (* then *)
                    else
                      EZKETTE := EZKETTE -> . LINK
                  end (* while *) ;
                DISPOSE2 ( EZANFANG -> . LINK )

        (*********************************)
        (*                               *)
        (*   LOESCHEN DES UEBERFLUES-    *)
        (*   SIGEN ZUSTANDSELEMENTS.     *)
        (*                               *)
        (*********************************)

              end (* then *) ;
            EZANFANG -> . LINK := EZUSTAND ;
            EZUSTAND := EZANFANG

        (**************************************)
        (*                                    *)
        (*   ANHAENGEN DES UEBRIGGEBLIE-      *)
        (*   BENEN ZUSTANDS AN DIE LISTE      *)
        (*   'EZUSTAND'.                      *)
        (*                                    *)
        (**************************************)

          end (* then *)
      end (* OPTIMIERUNG *) ;


   procedure ERROR ( I : INTEGER ) ;

   (*****************************************************)
   (*                                                   *)
   (*   MIT DER PROZEDUR 'ERROR' WIRD EIN FEHLER-       *)
   (*   ELEMENT (FEHLERART UND FEHLERSTELLE) IN         *)
   (*   DIE FEHLERKETTE EINGEFUEGT.                     *)
   (*                                                   *)
   (*   DIE GLOBALE VARIABLE 'OK' (BOOLEAN) WIRD        *)
   (*   AUF -FALSE- GESETZT, ALLERDINGS NUR,            *)
   (*   WENN ES SICH UM EINE FEHLERNUMMER (PARA-        *)
   (*   METER 'I') KLEINER 11 HANDELT (FEHLER 11        *)
   (*   IST EINE WARNUNG, SIEHE AUCH PROZEDUR           *)
   (*   'FEHLERAUSGABE').                               *)
   (*                                                   *)
   (*****************************************************)


      begin (* ERROR *)
        if I < 11 then
          begin
            FEZAHL := FEZAHL + 1 ;
            OK := FALSE
          end (* then *) ;
        FKETTE -> . NUMMER := I ;
        FKETTE -> . PLATZ := STELLE ;
        if FKETTE -> . NAECHST = NIL then
          begin
            NEW ( FKETTE -> . NAECHST ) ;
            FKETTE -> . NAECHST -> . NAECHST := NIL
          end (* then *) ;
        FKETTE := FKETTE -> . NAECHST ;
        FKETTE -> . NUMMER := 0
      end (* ERROR *) ;


   procedure PRODAUS ( X : PRODUKTION ) ;

   (************************************************)
   (*                                              *)
   (*   WIE BEREITS OBEN ERWAEHNT, STELLT          *)
   (*   'KETTE' DIE AKTUELLE SCHREIBPOSITION       *)
   (*   IN DER KETTE DER PRODUKTIONEN (UEBER-      *)
   (*   GAENGE) DAR. MITTELS 'PRODAUS' WIRD        *)
   (*   EIN NEU ERMITTELTER UEBERGANG AN DIE       *)
   (*   KETTE ANGEHAENGT.                          *)
   (*                                              *)
   (************************************************)


      begin (* PRODAUS *)
        NEW1 ( KETTE -> . NEXT ) ;
        KETTE := KETTE -> . NEXT ;
        KETTE -> := X ;
        KETTE -> . NEXT := NIL
      end (* PRODAUS *) ;


   procedure GETS ( var PTR : ZUSTVERW ; var EOK : BOOLEAN ; var I :
                  INTEGER ) ;

   (************************************************)
   (*                                              *)
   (*   'GETS' DIENT DAZU, AUS EINER KETTE         *)
   (*   VON ZUSTAENDEN DAS NAECHSTE ELEMENT        *)
   (*   HERAUSZULESEN.                             *)
   (*                                              *)
   (*   DER AUGENBLICKLICHE ZEIGERSTAND WIRD       *)
   (*   IN 'PTR' UEBERGEBEN; 'EOK' ZEIGT AN,       *)
   (*   OB DAS ENDE DER KETTE ERREICHT IST         *)
   (*   UND 'I' ENTHAELT DEN GELESENEN ZUSTAND.    *)
   (*                                              *)
   (*   DIE PROZEDUREN 'GETS' UND 'PUTS' WERDEN    *)
   (*   BEI DER SYNTAXANALYSE BENOETIGT, UM DIE    *)
   (*   SEMANTISCHE INFORMATION (DIE JA AUS EINE   *)
   (*   MENGE VON ANFANGS- ODER ENDZUSTAENDEN      *)
   (*   BESTEHT) VON EINER SYNTAKT. VARIABLEN      *)
   (*   (BZW. DER ZUGEORDNETEN PROZEDUR) ZUR       *)
   (*   NAECHSTEN WEITERZUREICHEN.                 *)
   (*                                              *)
   (************************************************)


      begin (* GETS *)
        I := PTR -> . WERT ;
        PTR := PTR -> . LINK ;
        EOK := ( PTR = NIL )
      end (* GETS *) ;


   procedure PUTS ( var PTR : ZUSTVERW ; I : INTEGER ) ;

   (************************************************)
   (*                                              *)
   (*   'PUTS' HAENGT EINEN ZUSTAND (INTEGER)      *)
   (*   AN DIE ANGEGEBENE KETTE AN                 *)
   (*                                              *)
   (*   (SIEHE AUCH KOMMENTAR ZU 'GETS')           *)
   (*                                              *)
   (************************************************)


      begin (* PUTS *)
        NEW2 ( PTR -> . LINK ) ;
        PTR := PTR -> . LINK ;
        PTR -> . WERT := I ;
        PTR -> . LINK := NIL
      end (* PUTS *) ;


   procedure NEUZUSTAND ;

   (**************************************)
   (*                                    *)
   (*   NEUEN ZUSTAND ERMITTELN          *)
   (*   (PEGEL HOCHSETZEN)               *)
   (*                                    *)
   (**************************************)


      begin (* NEUZUSTAND *)
        AKZUSTAND := AKZUSTAND + 1
      end (* NEUZUSTAND *) ;


   procedure EOFTEST ;

   (**************************************)
   (*                                    *)
   (*   'EOFTEST' WIRD NACH DEM LESEN    *)
   (*   EINES ZEICHENS AUFGERUFEN.       *)
   (*   FALLS DATEIENDE FESTGESTELLT     *)
   (*   WIRD, WIRD EIN FEHLER NUMMER 6   *)
   (*   AUSGEGEBEN (SIEHE 'ERROR')       *)
   (*   UND ZU DEM GLOBALEN LABEL        *)
   (*   999 VERZWEIGT.                   *)
   (*                                    *)
   (**************************************)


      begin (* EOFTEST *)
        if EOF ( QUELLE ) then
          begin
            ERROR ( 6 ) ;
            goto 999
          end (* then *)
      end (* EOFTEST *) ;


   procedure FEHLERAUSGABE ;

   (*****************************************************)
   (*                                                   *)
   (*   MIT 'FEHLERAUSGABE' WIRD DIE FEHLERKETTE        *)
   (*   (SIEHE PROZEDUR 'ERROR') AUSGEWERTET.           *)
   (*   (AUFRUF ERFOLGT BEI ZEILENWECHSEL UND AM        *)
   (*   ENDE).                                          *)
   (*                                                   *)
   (*   FALLS DIE KETTE NICHT LEER IST (FKETTE ->.      *)
   (*   NUMMER <> 0), WIRD UNTER DIE QUELLZEILE         *)
   (*   AN DEN DURCH FKETTE->.PLATZ BEZEICHNETEN        *)
   (*   STELLEN EIN SENKRECHTER STRICH AUS-             *)
   (*   GEGEBEN. DESGLEICHEN WIRD FUER JEDEN            *)
   (*   AUFGETRETENEN FEHLER EIN FEHLERTEXT AUS-        *)
   (*   GEGEBEN. DER AM WEITESTEN OBEN STEHENDE         *)
   (*   FEHLERTEXT BEZIEHT SICH AUF DEN AM WEITES-      *)
   (*   TEN LINKS STEHENDEN STRICH (AEHNLICH            *)
   (*   PS&POP).                                        *)
   (*                                                   *)
   (*   DIE FEHLER UND IHRE BEDEUTUNG SIND AUS          *)
   (*   DER UNTENSTEHENDEN CASE-ANWEISUNG ERSICHT-      *)
   (*   LICH.                                           *)
   (*                                                   *)
   (*   AM ENDE WIRD DIE FEHLERKETTE WIEDER GELEERT     *)
   (*   (INDEM DAS ERSTE ELEMENT DIE FEHLER-NUMMER      *)
   (*   NULL BEKOMMT). INTERESSANT IST HIER, DASS       *)
   (*   DIE EIGENTLICHE KETTE NICHT ZERSTOERT WIRD,     *)
   (*   SONDERN FUER JEDE ZEILE WIEDERVERWENDET WIRD.   *)
   (*   DER BELEGTE TEIL DER KETTE WIRD DURCH EIN       *)
   (*   FEHLERELEMENT MIT FEHLERNUMMER NULL BEGRENZT.   *)
   (*                                                   *)
   (*****************************************************)


      var PL : INTEGER ;

      begin (* FEHLERAUSGABE *)
        FKETTE := FEANFANG ;
        PL := 0 ;
        if FKETTE -> . NUMMER <> 0 then
          WRITE ( '   ' ) ;
        while FKETTE -> . NUMMER <> 0 do
          begin
            while PL < FKETTE -> . PLATZ do
              begin
                WRITE ( ' ' ) ;
                PL := PL + 1
              end (* while *) ;
            WRITE ( '!' ) ;
            PL := PL + 1 ;
            FKETTE := FKETTE -> . NAECHST
          end (* while *) ;
        if FKETTE <> FEANFANG then
          begin
            WRITELN ;
            FKETTE := FEANFANG
          end (* then *) ;
        while FKETTE -> . NUMMER <> 0 do
          begin
            if FKETTE -> . NUMMER < 11 then
              WRITE ( '***FEHLER: ' )
            else
              WRITE ( '**WARNUNG: ' ) ;
            case FKETTE -> . NUMMER of
              1 : begin
                    WRITELN ( 'STRING MIT LAENGE NULL' ) ;
                    WRITELN ( ' ' : 11 , 'NICHT ZULAESSIG' )
                  end (* tag/ca *) ;
              2 : begin
                    WRITELN ( 'ZWEITER PUNKT FEHLT' ) ;
                    WRITELN ( ' ' : 11 ,
                              'BEI KONSTRUKTION MIT ''..''  ' )
                  end (* tag/ca *) ;
              3 : begin
                    WRITELN ( 'HOCHKOMMA FEHLT NACH ''..''  ' ) ;
                    WRITELN ( ' ' : 11 ,
                              'ANNAHME: HOCHKOMMA WURDE VERGESSEN' )
                  end (* tag/ca *) ;
              5 : begin
                    WRITELN ( 'BEI KONSTRUKTION MIT ''..''' ) ;
                    WRITELN ( ' ' : 11 , 'SIND VORNE UND HINTEN' ) ;
                    WRITELN ( ' ' : 11 ,
                              'NUR EINZELNE ZEICHEN ZULAESSIG' ) ;
                    WRITELN ( ' ' : 11 , 'RESTSTRING WIRD UEBERLESEN' )
                  end (* tag/ca *) ;
              6 : begin
                    WRITELN ( 'REGULAERER AUSDRUCK UNVOLLSTAENDIG' ) ;
                    WRITELN ( ' ' : 11 ,
                              '(HOCHKOMMA ODER KLAMMER ZU FEHLT)' )
                  end (* tag/ca *) ;
              7 : begin
                    WRITELN ( 'HOCHKOMMA ODER ''('' ERWARTET;' ) ;
                    WRITELN ( ' ' : 11 ,
                              'ANNAHME: HOCHKOMMA WURDE VERGESSEN' )
                  end (* tag/ca *) ;
              9 : begin
                    WRITELN ( '''/'' ERWARTET;' ) ;
                    WRITELN ( ' ' : 11 , 'ZEICHEN WIRD UEBERLESEN' )
                  end (* tag/ca *) ;
              10 : begin
                     WRITELN ( 'ZEILENENDE UNZULAESSIG' ) ;
                     WRITELN ( ' ' : 11 , 'INNERHALB VON HOCHKOMMA''S'
                               )
                   end (* tag/ca *) ;
              11 : begin
                     WRITELN ( 'BEI KONSTRUKTION MIT ''..''' ) ;
                     WRITELN ( ' ' : 11 , 'MUSS ERSTES ZEICHEN' ) ;
                     WRITELN ( ' ' : 11 , 'KLEINER SEIN ALS ZWEITES;' )
                               ;
                     WRITELN ( ' ' : 11 ,
                               'ANDERE REIHENFOLGE WIRD ANGENOMMEN' )
                   end (* tag/ca *) ;
              12 : begin
                     WRITELN ( 'Falsche Hex-Ziffer' ) ;
                   end (* tag/ca *) ;
            end (* case *) ;
            FKETTE := FKETTE -> . NAECHST
          end (* while *) ;
        FKETTE := FEANFANG ;
        FKETTE -> . NUMMER := 0
      end (* FEHLERAUSGABE *) ;


   procedure READQ ( var CH : CHAR ) ;

   (*****************************************************)
   (*                                                   *)
   (*   DIE PROZEDUR 'READQ' IST DIE ZENTRALE EIN-      *)
   (*   GABEPROZEDUR. SIE IST (LEIDER) VON DER VER-     *)
   (*   WENDETEN PASCAL-IMPLEMENTIERUNG ABHAENGIG.      *)
   (*                                                   *)
   (*   FOLGENDE ANFORDERUNGEN WERDEN AN 'READQ' GE-    *)
   (*   STELLT:                                         *)
   (*                                                   *)
   (*   1.) DIE JEWEILIGE POSITION IM SATZ MUSS         *)
   (*       IN EINER INTEGER-VARIABLEN 'STELLE'         *)
   (*       ZUR VERFUEGUNG STEHEN.                      *)
   (*                                                   *)
   (*   2.) FORTLAUFENDES LESEN UEBER DAS ZEILEN-       *)
   (*       ENDE HINAUS IST MOEGLICH; DIE PROZEDUR      *)
   (*       VERANLASST SELBSTSTAENDIG DIE NOETIGEN      *)
   (*       AKTIVITAETEN BEI ZEILENWECHSEL, ALS DA      *)
   (*       SIND:                                       *)
   (*          - ZEILENVORSCHUB AUF PROTOKOLL           *)
   (*          - FEHLERAUSGABE.                         *)
   (*                                                   *)
   (*   3.) EINGELESENE ZEICHEN WERDEN AUF DEM          *)
   (*       OUTPUT-FILE PROTOKOLLIERT.                  *)
   (*                                                   *)
   (*****************************************************)


      var CH1 , CH2 : CHAR ;
          WERT1 , WERT : INTEGER ;
          NOCHMAL : BOOLEAN ;
          DURCHLAUF : INTEGER ;

      begin (* READQ *)
        DURCHLAUF := 1 ;
        repeat
          NOCHMAL := FALSE ;
          if not EOLN ( QUELLE ) and not ANFANG then
            begin
              STELLE := STELLE + 1 ;
              READ ( QUELLE , CH ) ;
              WRITE ( CH )
            end (* then *)
          else
            begin
              if DURCHLAUF > 1 then
                ERROR ( 10 ) ;
              if not EOLNMOEGLICH then
                ERROR ( 10 ) ;
              WRITELN ;
              FEHLERAUSGABE ;
              STELLE := 0 ;
              if ANFANG then
                begin
                  WRITE ( ' ' ) ;
                  STELLE := STELLE + 1 ;
                  ANFANG := FALSE
                end (* then *) ;

        (******************************)
        (* EOLN, EIN BLANK UEBERLESEN *)
        (******************************)

              READ ( QUELLE , CH ) ;
              if not EOF ( QUELLE ) then
                WRITE ( '   ' , CH )
            end (* else *) ;
          case DURCHLAUF of
            1 : if CH = '\' then
                  begin
                    NOCHMAL := TRUE ;
                    DURCHLAUF := DURCHLAUF + 1 ;
                  end (* then *) ;
            2 : begin
                  if CH = 'n' then
                    CH := CHR ( 10 )
                  else
                    if CH = 't' then
                      CH := CHR ( 9 )
                    else
                      if CH = '0' then
                        CH := CHR ( 0 )
                      else
                        if CH = 'x' then
                          begin
                            NOCHMAL := TRUE ;
                            DURCHLAUF := DURCHLAUF + 1 ;
                          end (* then *)
                end (* tag/ca *) ;
            3 : begin
                  CH1 := CH ;
                  NOCHMAL := TRUE ;
                  DURCHLAUF := DURCHLAUF + 1 ;
                end (* tag/ca *) ;
            4 : begin
                  CH2 := CH ;
                  WERT := 0 ;
                  if ( CH1 >= 'a' ) and ( CH1 <= 'f' ) then
                    CH1 := CHR ( ORD ( CH1 ) - ORD ( 'a' ) + ORD ( 'A'
                           ) ) ;
                  if ( CH1 >= '0' ) and ( CH1 <= '9' ) then
                    WERT := ORD ( CH1 ) - ORD ( '0' )
                  else
                    if ( CH1 >= 'A' ) and ( CH1 <= 'F' ) then
                      WERT := ORD ( CH1 ) - ORD ( 'A' ) + 10
                    else
                      ERROR ( 12 ) ;
                  WERT1 := WERT ;
                  WERT := 0 ;
                  if ( CH2 >= 'a' ) and ( CH2 <= 'f' ) then
                    CH2 := CHR ( ORD ( CH2 ) - ORD ( 'a' ) + ORD ( 'A'
                           ) ) ;
                  if ( CH2 >= '0' ) and ( CH2 <= '9' ) then
                    WERT := ORD ( CH2 ) - ORD ( '0' )
                  else
                    if ( CH2 >= 'A' ) and ( CH2 <= 'F' ) then
                      WERT := ORD ( CH2 ) - ORD ( 'A' ) + 10
                    else
                      ERROR ( 12 ) ;
                  WERT1 := WERT1 * 16 + WERT ;
                  CH := CHR ( WERT1 ) ;
                end (* tag/ca *) ;
          end (* case *)
        until not NOCHMAL ;
      end (* READQ *) ;


   procedure FIND ( var CH : CHAR ) ;

   (*******************************************)
   (*                                         *)
   (*   'FIND' LIEST NAECHSTES ZEICHEN UN-    *)
   (*   GLEICH LEERZEICHEN EIN (BEI DATEI-    *)
   (*   ENDE WIRD EBENFALLS ABGEBROCHEN).     *)
   (*                                         *)
   (*   'FIND' RUFT ZUM LESEN SELBSTVER-      *)
   (*   STAENDLICH DIE PROZEDUR 'READQ'       *)
   (*   AUF.                                  *)
   (*                                         *)
   (*******************************************)


      begin (* FIND *)
        repeat
          READQ ( CH )
        until ( CH <> ' ' ) or EOF ( QUELLE ) ;
        if EOF ( QUELLE ) then
          CH := ' '
      end (* FIND *) ;


   procedure AUSDRUCK ( AZUSTAND : ZUSTVERW ; var EZUSTAND : ZUSTVERW ;
                      STIEFE : INTEGER ) ;

   (**********************************************************)
   (*                                                        *)
   (*   AN DIESER STELLE BEGINNT NUN DIE EIGENTLICHE SYN-    *)
   (*   TAXPRUEFUNG. SIE ERFOLGT NACH DER METHODE DES RE-    *)
   (*   KURSIVEN ABSTIEGS, BEI DER FUER JEDE SYNTAKTISCHE    *)
   (*   VARIABLE (JEDES NONTERMINAL) EINE PROZEDUR EXI-      *)
   (*   STIERT, DIE DIESES NONTERMINAL ERKENNT. DIESE PRO-   *)
   (*   ZEDUREN RUFEN SICH (UNTER UMSTAENDEN REKURSIV)       *)
   (*   ENTSPRECHEND DER ZUGRUNDELIEGENDEN GRAMMATIK AUF.    *)
   (*   DIE STRUKTUR DES KELLERS STELLT DABEI EIN ABBILD     *)
   (*   DER JEWEILIGEN SYNTAKTISCHEN STRUKTUR DAR.           *)
   (*                                                        *)
   (*   DIE PROZEDUREN SIND WIE FOLGT GESCHACHTELT (DIE      *)
   (*   ZUGRUNDELIEGENDE GRAMMATIK STEHT WEITER OBEN IN      *)
   (*   EINEM AEHNLICHEN KASTEN):                            *)
   (*                                                        *)
   (*   1. AUSDRUCK                                          *)
   (*                                                        *)
   (*   1.1 TERM                                             *)
   (*                                                        *)
   (*   1.1.1 FAKTOR                                         *)
   (*                                                        *)
   (*   1.1.1.1 F2                                           *)
   (*                                                        *)
   (*   (1.1.1.1.1 S3 - NUR HILFSPROZEDUR ZU F2, KEINE       *)
   (*                   LOKALEN VARIABLEN )                  *)
   (*                                                        *)
   (**********************************************************)


      var ENDE : BOOLEAN ;
          EZ1 , EZ2 : ZUSTVERW ;
          KETTANFANG : PRODVER ;


      procedure TERM ( AZUSTAND : ZUSTVERW ; var EZUSTAND : ZUSTVERW )
                     ;

         var AZ1 , EZ1 : ZUSTVERW ;


         procedure FAKTOR ( AZUSTAND : ZUSTVERW ; var EZUSTAND :
                          ZUSTVERW ) ;

            var SKETTE , X , PRODV , RKETTE , ENDKETTE : PRODVER ;
                EOK : BOOLEAN ;
                TERMC : CHAR ;
                TERMNR : INTEGER ;
                I , RIGHT , N , ANZ , Z1 , Z2 : INTEGER ;
                PROD : PRODUKTION ;
                AZ , EZ : ZUSTVERW ;
                CHSTERN : CHAR ;


            procedure F2 ( AZUSTAND : ZUSTVERW ; var EZUSTAND :
                         ZUSTVERW ) ;

               label 99 ;

               var OP , A1 , A2 : CHAR ;
                   I : INTEGER ;
                   PROD : PRODUKTION ;
                   EOK , ENDF2 : BOOLEAN ;
                   SKETTE : PRODVER ;
                   AKETTE : ZUSTVERW ;
                   ANZAHL , Z1 : INTEGER ;
                   ENDE5 , FEHLER7 : BOOLEAN ;


               procedure S3 ;

               (*****************************************************)
               (*                                                   *)
               (*   DIE PROZEDUR 'S3' IST EINE HILFSPROZEDUR FUER   *)
               (*   'F2'. ALLE VARIABLEN WERDEN UEBER SEITEN-       *)
               (*   EFFEKT UEBERGEBEN. URSPRUENGLICH WAR ES GAR     *)
               (*   NICHT VORGESEHEN, FUER 'S3' EINE PROZEDUR ZU    *)
               (*   VERWENDEN - ENTSPRECHEND WAERE ES AUCH OHNE     *)
               (*   WEITERS MOEGLICH, 'S3' AN DEN BEIDEN AUFRUF-    *)
               (*   STELLEN EINZUKOPIEREN.                          *)
               (*                                                   *)
               (*   'S3' LEISTET FOLGENDES: ES WIRD VON 'F2' DANN   *)
               (*   AUFGERUFEN, WENN FESTSTEHT, DASS ES SICH BEI    *)
               (*   DER RECHTEN SEITE VON - F2 -> ... - UM EINEN    *)
               (*   STRING HANDELT (PASCAL-STRING).                 *)
               (*                                                   *)
               (*   DAS KANN ENTWEDER DANN DER FALL SEIN, WENN      *)
               (*   DAS ZWEITE VON 'F2' GELESENE ZEICHEN NACH DEM   *)
               (*   ERSTEN HOCHKOMMA KEIN HOCHKOMMA IST (DANN IST   *)
               (*   DER STRING LAENGER ALS 1) ODER WENN NACH DIE-   *)
               (*   SEM ZWEITEN HOCHKOMMA NOCH EINES FOLGT (DANN    *)
               (*   HAT DER STRING ALS ZWEITES ZEICHEN EIN HOCH-    *)
               (*   KOMMA).                                         *)
               (*                                                   *)
               (*   DIE VARIABLE 'A1' ENTHAELT DAS ERSTE ZEICHEN    *)
               (*   DES STRINGS. 'S3' MACHT NUN FOLGENDES:          *)
               (*                                                   *)
               (*   1.) FUER JEDEN DER ANFANGSZUSTAENDE, DIE 'F2'   *)
               (*       ALS PARAMETER BEKOMMEN HAT (AZUSTAND BZW.   *)
               (*       AKETTE) LEGT 'S3' EINE PRODUKTION DER       *)
               (*       FORM                                        *)
               (*                                                   *)
               (*             ANF.-ZUSTAND ->  'A1'  NEU-ZUSTAND    *)
               (*                                                   *)
               (*       IN DIE KETTE DER PRODUKTIONEN AB.           *)
               (*                                                   *)
               (*   2.) ENTSPRECHEND LIEST 'S3' DANN WEITER UND     *)
               (*       LEGT FUER JEDEN BUCHSTABEN INNERHALB DES    *)
               (*       STRINGS EINE PRODUKTIOEN DER FORM           *)
               (*                                                   *)
               (*          VORH.-NEUZUSTAND -> 'CH' NEU-ZUSTAND     *)
               (*                                                   *)
               (*       AB (SOLANGE, BIS DAS ENDE DES STRINGS       *)
               (*       ERKANNT WURDE - BOOLEAN-VARIABLE 'ENDF2').  *)
               (*                                                   *)
               (*   EIN BEISPIEL:                                   *)
               (*                                                   *)
               (*   VOR ERKENNEN DES STRINGS 'TR440' WAREN ALS      *)
               (*   ANF.ZUSTAENDE DIE ZUSTAENDE 5 UND 7 MITGE-      *)
               (*   GEBEN WORDEN. 'S3' ERZEUGT NUN FOLGENDE PRO-    *)
               (*   DUKTIONEN:                                      *)
               (*                                                   *)
               (*         5  -> 'T'  8                              *)
               (*         7  -> 'T'  8                              *)
               (*         8  -> 'R'  9                              *)
               (*         9  -> '4' 10                              *)
               (*        10  -> '4' 11                              *)
               (*        11  -> '0' 12                              *)
               (*                                                   *)
               (*   AUSSERDEM WIRD DER ENDZUSTAND 12 IN DIE KETTE   *)
               (*   DER ZULAESSIGEN ENDZUSTAENDE (ERGEBNIS-         *)
               (*   PARAMETER VON 'F2') AUFGENOMMEN.                *)
               (*                                                   *)
               (*****************************************************)


                  begin (* S3 *)
                    NEUZUSTAND ;
                    PROD . RECHTS := AKZUSTAND ;
                    PROD . TERMINAL := A1 ;
                    AKETTE := AZUSTAND ;
                    repeat
                      GETS ( AKETTE , EOK , Z1 ) ;
                      PROD . LINKS := Z1 ;
                      PROD . TERMNR := AKTTERMNR ;
                      PRODAUS ( PROD )
                    until EOK ;
                    ENDF2 := FALSE ;
                    repeat
                      EOLNMOEGLICH := FALSE ;
                      PROD . LINKS := AKZUSTAND ;
                      NEUZUSTAND ;
                      PROD . RECHTS := AKZUSTAND ;
                      PROD . TERMINAL := CH ;
                      PROD . TERMNR := AKTTERMNR ;
                      PRODAUS ( PROD ) ;
                      READQ ( CH ) ;
                      EOFTEST ;
                      if CH = '''' then
                        begin
                          EOLNMOEGLICH := TRUE ;
                          READQ ( CH ) ;
                          ENDF2 := ( CH <> '''' ) or EOF ( QUELLE )
                        end (* then *)
                    until ENDF2 ;
                    if ( CH = ' ' ) and not EOF ( QUELLE ) then
                      FIND ( CH ) ;
                    NEW2 ( EZUSTAND ) ;
                    EZUSTAND -> . WERT := AKZUSTAND ;
                    EZUSTAND -> . LINK := NIL
                  end (* S3 *) ;

               begin (* F2 *)
                 FEHLER7 := FALSE ;

                 (***************************************************)
                 (*                                                 *)
                 (*   HIER BEGINNT DIE PROZEDUR 'F2'. ZUM BESSEREN  *)
                 (*   UEBERBLICK HIER NOCH EINMAL DIE PROZEDUR-     *)
                 (*   DEKLARATION (SIEHE WEITER OBEN):              *)
                 (*                                                 *)
                 (*   PROCEDURE F2 ( AZUSTAND: ZUSTVERW;            *)
                 (*                  VAR EZUSTAND: ZUSTVERW);       *)
                 (*                                                 *)
                 (*   SOWIE DIE DEFINITION VON F2 AUS DER           *)
                 (*   GRAMMATIK:                                    *)
                 (*                                                 *)
                 (*   F2 -> ' STRING '                              *)
                 (*   F2 -> ( AUSDRUCK )                            *)
                 (*   F2 -> ' BUCHST ' .. ' BUCHST '                *)
                 (*                                                 *)
                 (*   ZUM VERSTAENDNIS VON F2 MUSS MAN AUCH DIE     *)
                 (*   PROZEDUR S3 HERANZIEHEN (SIEHE OBIGEN         *)
                 (*   KASTEN). SIE WIRD IMMER DANN AUFGERUFEN, SO-  *)
                 (*   BALD FESTSTEHT, DASS ES SICH UM EINEN STRING  *)
                 (*   HANDELT (D.H. BEIM LESEN DES ZWEITEN TERMI-   *)
                 (*   NALS NACH DEM APOSTROPH)                      *)
                 (*                                                 *)
                 (***************************************************)

                 if ( CH <> '''' ) and ( CH <> '(' ) then
                   begin
                     ERROR ( 7 ) ;
                     OP := CH ;
                     FEHLER7 := TRUE ;
                     CH := ''''
                   end (* then *) ;

                 (**************************************)
                 (*                                    *)
                 (*   FALLS DAS ERSTE ZEICHEN WEDER    *)
                 (*   KLAMMER AUF NOCH APOSTROPH       *)
                 (*   IST, WIRD EIN FEHLER NUMMER 7    *)
                 (*   GEMELDET. ES WIRD ANGENOMMEN,    *)
                 (*   DASS EIN APOSTROPH VERGESSEN     *)
                 (*   WURDE.                           *)
                 (*                                    *)
                 (**************************************)

                 if CH = '''' then
                   begin
                     EOLNMOEGLICH := FALSE ;
                     A1 := ' ' ;
                     A2 := ' ' ;
                     if not FEHLER7 then
                       READQ ( CH )
                     else
                       CH := OP ;
                     EOFTEST ;
                     if CH = '''' then
                       begin
                         READQ ( CH ) ;
                         EOFTEST ;
                         if CH <> '''' then
                           begin
                             ERROR ( 1 ) ;
                             goto 99
                           end (* then *)
                       end (* then *) ;

                 (************************************************)
                 (*                                              *)
                 (*   ALS ERSTES WIRD DER FALL (CH = '''')       *)
                 (*   ABGEHANDELT. DIE (GLOBALE) VARIABLE        *)
                 (*   EOLNMOEGLICH (:BOOLEAN), DIE ANZEIGT,      *)
                 (*   OB EIN ZEILENENDE KOMMEN DARF, WIRD        *)
                 (*   AUF 'FALSE' GESETZT. FALLS KEIN            *)
                 (*   FEHLER 7 VORLAG, WIRD EIN ZEICHEN          *)
                 (*   GELESEN (ANDERNFALLS WIRD DAS DORT         *)
                 (*   VORGEFUNDENE GENOMMEN). WENN DAS           *)
                 (*   GELESENE ZEICHEN WIEDER EIN APOSTROPH      *)
                 (*   IST, DANN MUSS NOCH EIN WEITERER APO-      *)
                 (*   STROPH FOLGEN, DA DER LEERE STRING         *)
                 (*   NICHT ZULAESSIG IST (FEHLER 1).            *)
                 (*                                              *)
                 (************************************************)

                     A1 := CH ;
                     READQ ( CH ) ;
                     EOFTEST ;
                     if CH = '''' then
                       begin
                         EOLNMOEGLICH := TRUE ;
                         READQ ( CH ) ;
                         if CH = '''' then
                           S3
                         else
                           begin
                             if ( CH = ' ' ) and not EOF ( QUELLE )
                             then
                               FIND ( CH ) ;

                 (************************************************)
                 (*                                              *)
                 (*   DAS ERSTE GELESENE ZEICHEN WIRD            *)
                 (*   IN A1 ABGELEGT. EIN WEITERES ZEICHEN       *)
                 (*   WIRD GELESEN. HANDELT ES SICH HIER         *)
                 (*   NICHT UM EINEN APOSTROPH (ELSE-ZWEIG;      *)
                 (*   SIEHE WEITER UNTEN), SO KANN AN DIESER     *)
                 (*   STELLE S3 AUFGERUFEN WERDEN. DAS GILT      *)
                 (*   AUCH DANN, WENN NACH DEM EINGELESENEN      *)
                 (*   APOSTROPH WIEDER EIN APOSTROPH FOLGT       *)
                 (*   (SIEHE UNMITTELBAR HIER UEBER DEM          *)
                 (*   KASTEN). ANDERNFALLS (D.H. WENN NUR        *)
                 (*   EIN APOSTROPH KAM) GILT ES NOCH ZU         *)
                 (*   PRUEFEN, OB EINE KONSTRUKTION MIT          *)
                 (*   '..' VORLIEGT. DAS GESCHIEHT IM            *)
                 (*   ANSCHLUSS AN DIESEN KASTEN.                *)
                 (*                                              *)
                 (************************************************)

                             if CH = '.' then
                               begin
                                 READQ ( CH ) ;
                                 EOFTEST ;
                                 if CH <> '.' then
                                   ERROR ( 2 ) ;
                                 if CH <> '''' then
                                   FIND ( CH ) ;
                                 EOFTEST ;
                                 EOLNMOEGLICH := FALSE ;
                                 if CH <> '''' then
                                   ERROR ( 3 )
                                 else
                                   begin
                                     READQ ( CH ) ;
                                     EOFTEST
                                   end (* else *) ;
                                 if CH = '''' then
                                   begin
                                     READQ ( CH ) ;
                                     EOFTEST ;
                                     if CH <> '''' then
                                       begin
                                         ERROR ( 1 ) ;
                                         goto 99
                                       end (* then *)
                                   end (* then *) ;
                                 A2 := CH ;
                                 READQ ( CH ) ;
                                 EOFTEST ;
                                 EOLNMOEGLICH := TRUE ;
                                 if CH <> '''' then
                                   begin
                                     ERROR ( 5 ) ;
                                     ENDE5 := FALSE ;
                                     repeat
                                       READQ ( CH ) ;
                                       EOFTEST ;
                                       if CH = '''' then
                                         begin
                                           READQ ( CH ) ;
                                           EOFTEST ;
                                           ENDE5 := ( CH <> '''' )
                                         end (* then *)
                                     until ENDE5 ;
                                     if CH = ' ' then
                                       FIND ( CH )
                                   end (* then *)
                                 else
                                   FIND ( CH ) ;
                                 if A2 < A1 then
                                   begin
                                     ERROR ( 11 ) ;
                                     OP := A1 ;
                                     A1 := A2 ;
                                     A2 := OP
                                   end (* then *) ;

                 (***************************************************)
                 (*                                                 *)
                 (*   HIER WERDEN NUN, FUER DEN FALL EINER KON-     *)
                 (*   STRUKTION MIT '..', ALLE MOEGLICHEN FEHLER    *)
                 (*   ERKANNT UND BEHANDELT. DIES SIND (ENTSPRE-    *)
                 (*   CHEND DER REIHENFOLGE IM PROGRAMM):           *)
                 (*                                                 *)
                 (*   FEHLER 2: ES FOLGT NACH EINEM PUNKT KEIN      *)
                 (*             ZWEITER                             *)
                 (*   FEHLER 3: NACH '..' FOLGT KEIN APOSTROPH      *)
                 (*   FEHLER 1: NACH '..' FOLGT LEERER STRING       *)
                 (*   FEHLER 5: NACH '..' FOLGT STRING MIT MEHR ALS *)
                 (*             EINEM ZEICHEN (RESTSTRING WIRD UEBER*)
                 (*             LESEN)                              *)
                 (*   FEHLER 11 (WARNUNG):                          *)
                 (*             DAS ERSTE ZEICHEN IST GROESSER ALS  *)
                 (*             DAS ZWEITE, ANDERE REIHENFOLGE WIRD *)
                 (*             ANGENOMMEN (HAT KEINEN EINFLUSS AUF *)
                 (*             DIE CODEERZEUGUNG)                  *)
                 (*                                                 *)
                 (*   DIE BEIDEN ZEICHEN STEHEN IN A1 UND A2.       *)
                 (*                                                 *)
                 (***************************************************)

                                 PROD . LINKS := - 1 ;
                                 PROD . TERMINAL := ' ' ;
                                 PROD . RECHTS := 0 ;
                                 PROD . TERMNR := AKTTERMNR ;
                                 PRODAUS ( PROD ) ;
                                 SKETTE := KETTE ;
                                 ANZAHL := 0 ;
                                 NEUZUSTAND ;
                                 PROD . RECHTS := AKZUSTAND ;
                                 PROD . TERMINAL := A1 ;
                                 AKETTE := AZUSTAND ;
                                 repeat
                                   GETS ( AKETTE , EOK , Z1 ) ;
                                   PROD . LINKS := Z1 ;
                                   PROD . TERMNR := AKTTERMNR ;
                                   PRODAUS ( PROD ) ;
                                   ANZAHL := ANZAHL + 1
                                 until EOK ;
                                 PROD . TERMINAL := A2 ;
                                 PROD . TERMNR := AKTTERMNR ;
                                 PRODAUS ( PROD ) ;
                                 SKETTE -> . RECHTS := ANZAHL ;
                                 NEW2 ( EZUSTAND ) ;
                                 EZUSTAND -> . WERT := AKZUSTAND ;
                                 EZUSTAND -> . LINK := NIL
                               end (* then *)
                             else

                 (***************************************************)
                 (*                                                 *)
                 (*   ES WIRD INNERHALB DER KETTE DER PRODUKTIO-    *)
                 (*   NEN EIN SOGENANNTES VON-BIS-ELEMENT ERZEUGT.  *)
                 (*                                                 *)
                 (*   DARGESTELLT WERDEN SOLL FOLGENDE SITUATION:   *)
                 (*                                                 *)
                 (*   AUSGEHEND VON EINER MENGE VON STARTZUSTAENDEN *)
                 (*   SOLL BEI ALLEN ZEICHEN, DIE IN DER WERTIG-    *)
                 (*   KEIT ZWISCHEN A1 UND A2 LIEGEN, EIN UEBER-    *)
                 (*   GANG IN EINEN (END-)ZUSTAND ERFOLGEN. DAZU    *)
                 (*   MUSS DARGESTELLT WERDEN:                      *)
                 (*     A) DIE MENGE DER STARTZUSTAENDE             *)
                 (*     B) DIE GRENZEN DES BEREICHS (A1 UND A2)     *)
                 (*     C) DER ENDZUSTAND.                          *)
                 (*                                                 *)
                 (*   DAS GESCHIEHT NUN WIE FOLGT:                  *)
                 (*                                                 *)
                 (*   ZUERST KOMMT EIN ELEMENT, DAS LINKS EINE -1   *)
                 (*   TRAEGT (ALS KENNUNG); RECHTS STEHT DIE        *)
                 (*   MAECHTIGKEIT DER MENGE DER STARTZUSTAENDE.    *)
                 (*    ( -ANZAHL- )                                 *)
                 (*                                                 *)
                 (*   DANACH FOLGEN -ANZAHL- ELEMENTE, DIE LINKS    *)
                 (*   EINEN DER STARTZUSTAENDE TRAGEN, IN DER       *)
                 (*   MITTE STEHT DAS ERSTE ZEICHEN DES BEREICHS    *)
                 (*   (DAS KLEINERE) UND RECHTS STEHT DER END-      *)
                 (*   ZUSTAND.                                      *)
                 (*                                                 *)
                 (*   WAS JETZT NOCH FEHLT, IST DIE OBERE GRENZE    *)
                 (*   DES BEREICHS. SIE WIRD IN EINER ZUSAETZ-      *)
                 (*   LICHEN PRODUKTION ANGEGEBEN. DIESE HAT DIE    *)
                 (*   FORM:                                         *)
                 (*        LINKS  - EINER DER STARTZUSTAENDE        *)
                 (*                 (DER ABER SCHON MAL DA WAR)     *)
                 (*        MITTE  - DIE OBERGRENZE (DAS GROESSERE   *)
                 (*                 ZEICHEN)                        *)
                 (*        RECHTS - NOCH MAL DER ENDZUSTAND.        *)
                 (*                                                 *)
                 (*   (INSGESAMT ALSO -ANZAHL- + 2 ELEMENTE)        *)
                 (*                                                 *)
                 (***************************************************)

                               begin
                                 NEUZUSTAND ;
                                 PROD . RECHTS := AKZUSTAND ;
                                 PROD . TERMINAL := A1 ;
                                 AKETTE := AZUSTAND ;
                                 repeat
                                   GETS ( AKETTE , EOK , Z1 ) ;
                                   PROD . LINKS := Z1 ;
                                   PROD . TERMNR := AKTTERMNR ;
                                   PRODAUS ( PROD )
                                 until EOK ;
                                 NEW2 ( EZUSTAND ) ;
                                 EZUSTAND -> . WERT := AKZUSTAND ;
                                 EZUSTAND -> . LINK := NIL

                 (************************************************)
                 (*                                              *)
                 (*   ANDERER FALL: ES LIEGT NUR EIN             *)
                 (*   ZEICHEN VOR                                *)
                 (*                                              *)
                 (*   ES WIRD FUER JEDEN STARTZUSTAND            *)
                 (*   EINE PRODUKTION DER FORM                   *)
                 (*     LINKS  - STARTZUSTAND                    *)
                 (*     MITTE  - GELESENES ZEICHEN               *)
                 (*     RECHTS - ENDZUSTAND                      *)
                 (*                       ABGELEGT.              *)
                 (*                                              *)
                 (************************************************)

                               end (* else *)
                           end (* else *)
                       end (* then *)
                     else
                       S3 ;

                 (*********************************)
                 (*                               *)
                 (*   OBEN ERWAEHNT. ELSE-ZWEIG   *)
                 (*                               *)
                 (*********************************)

                     99 :
                     EOLNMOEGLICH := TRUE ;
                     if ( CH = ' ' ) and not EOF ( QUELLE ) then
                       FIND ( CH )

                 (**************************)
                 (*                        *)
                 (*   BLANKS UEBERLESEN.   *)
                 (*                        *)
                 (**************************)

                   end (* then *)
                 else
                   begin
                     FIND ( CH ) ;
                     EOFTEST ;
                     AUSDRUCK ( AZUSTAND , EZUSTAND , STIEFE + 1 ) ;
                     EOFTEST ;
                     FIND ( CH )

                 (*******************************************)
                 (*                                         *)
                 (*   ANDERNFALLS (ES HANDELT SICH UM       *)
                 (*   EINE KLAMMER-KONSTRUKTION):           *)
                 (*                                         *)
                 (*   REKURSIVER AUFRUF DER PROZEDUR        *)
                 (*   AUSDRUCK (NACH UEBERLESEN DER         *)
                 (*   KLAMMER).                             *)
                 (*                                         *)
                 (*******************************************)

                   end (* else *)
               end (* F2 *) ;

            begin (* FAKTOR *)
              RKETTE := KETTE ;

              (*****************************************************)
              (*                                                   *)
              (*   HIER BEGINNT DIE PROZEDUR -FAKTOR-.             *)
              (*                                                   *)
              (*   ZUM BESSEREN UEBERBLICK NOCH EINMAL DIE         *)
              (*   DEKLARATION:                                    *)
              (*                                                   *)
              (*   PROCEDURE FAKTOR ( AZUSTAND: ZUSTVERW;          *)
              (*                      VAR EZUSTAND: ZUSTVERW );    *)
              (*                                                   *)
              (*   UND DIE ENTSPRECHENDE DEFINITION AUS DER        *)
              (*   GRAMMATIK:                                      *)
              (*                                                   *)
              (*   FAKTOR -> F2                                    *)
              (*   FAKTOR -> F2 *                                  *)
              (*   FAKTOR -> F2 +                                  *)
              (*                                                   *)
              (*   DIE FUNKTIONSWEISE DER PROZEDUR IST IM ERSTEN   *)
              (*   FALL SEHR SCHNELL ERKLAERT: ZUERST WIRD DER     *)
              (*   ANFANGSSTAND DER KETTE ZWISCHENGESPEICHERT      *)
              (*   (BRAUCHT MAN, FALLS EIN STERN FOLGT). DANN      *)
              (*   ERFOLGT EIN AUFRUF VON F2 (MIT DER ENTSPRE-     *)
              (*   CHENDEN VERSORGUNG; DIE PARAMETER WERDEN EIN-   *)
              (*   FACH DURCHGEREICHT). DA ANSCHLIESSEND KEIN      *)
              (*   STERN FOLGT, WIRD DER REST DER PROZEDUR         *)
              (*   UEBERSPRUNGEN.                                  *)
              (*                                                   *)
              (*   FALLS EIN STERN FOLGT, MUESSEN NOCH EINIGE      *)
              (*   MANIPULATIONEN AN DER ERZEUGTEN KETTE UND AN    *)
              (*   DER MENGE DER ENDZUSTAENDE VORGENOMMEN WER-     *)
              (*   DEN. (SIEHE WEITER UNTEN)                       *)
              (*                                                   *)
              (*****************************************************)

              F2 ( AZUSTAND , EZUSTAND ) ;
              if ( CH = '*' ) or ( CH = '+' ) then
                begin
                  CHSTERN := CH ;
                  if not OK then
                    FIND ( CH )
                  else
                    begin

              (******************************************************)
              (*                                                    *)
              (*   FALLS BIS DAHIN SCHON EIN FEHLER BEI DER SYNTAX- *)
              (*   PRUEFUNG AUFGETRETEN WAR, WIRD AUSSER DEM UEBER- *)
              (*   LESEN DES STERNS NICHTS WEITER GEMACHT.          *)
              (*                                                    *)
              (*   IM ANDERN FALL MUSS EIN ZYKLUS IN DIE PRODUK-    *)
              (*   TIONEN EINGEBAUT WERDEN.                         *)
              (*                                                    *)
              (*   EIN BEISPIEL:                                    *)
              (*                                                    *)
              (*      'A' / 'BC'*                                   *)
              (*                                                    *)
              (*   BIS VOR DEM ERKENNEN DES STERNS WURDEN FOLGENDE  *)
              (*   PRODUKTIONEN ERZEUGT:                            *)
              (*                                                    *)
              (*      1  ->  'A'  2                                 *)
              (*      1  ->  'B'  3                                 *)
              (*      3  ->  'C'  4.                                *)
              (*                                                    *)
              (*   ES DARF JETZT KEIN UEBERGANG DER FORM            *)
              (*      3  ->  'C'  1                                 *)
              (*   ERZEUGT WERDEN (DENN SONST KOENNTE JA AUCH       *)
              (*   'BCA' ERKANNT WERDEN). ES MUSS VIELMEHR          *)
              (*   DER ZUSTAND 4 DIE ROLLE DES ZUSTANDES 1          *)
              (*   IN DEM TEILTERM 'BC' UEBERNEHMEN, D.H.           *)
              (*   ES MUSS EIN UEBERGANG                            *)
              (*      4  ->  'B'  3                                 *)
              (*   ERZEUGT WERDEN.                                  *)
              (*                                                    *)
              (*   ALLGEMEIN GESPROCHEN: DER ENDZUSTAND MUSS DIE    *)
              (*   SELBEN UEBERGAENGE ERHALTEN WIE EINER AUS DER    *)
              (*   MENGE DER STARTZUSTAENDE (ABER NUR SOLCHE, DIE   *)
              (*   INNERHALB DES TEILTERMS, AUF DIE SICH DER STERN  *)
              (*   BEZIEHT, ERZEUGT WURDEN - DESHALB AUCH DAS       *)
              (*   ABSPEICHERN DES STANDES DER KETTE VOR DER TERM-  *)
              (*   ERKENNUNG IN DIE VARIABLE 'RKETTE'.              *)
              (*                                                    *)
              (*   AUSSERDEM MUESSEN ALLE STARTZUSTAENDE IN DIE     *)
              (*   MENGE DER ENDZUSTAENDE UEBERNOMMEN WERDEN        *)
              (*   (DA JA DER STERN AUCH DIE MOEGLICHKEIT           *)
              (*   -NULL MAL- MIT EINSCHLIESST)                     *)
              (*                                                    *)
              (*   NOCH ETWAS KOMPLIZIERTER GESTALTET SICH DAS      *)
              (*   PROBLEM, WENN DIE NEUEN PRODUKTIONEN IN EINEN    *)
              (*   VON-BIS-BLOCK EINGEBUNDEN WERDEN MUESSEN.        *)
              (*   FALLS EIN VON-BIS-BLOCK AUF DER LINKEN SEITE     *)
              (*   DIE STARTZUSTAENDE ENTHAELT, SO MUESSEN DIE      *)
              (*   ENDZUSTAENDE (SOFERN SIE VON DEN START-          *)
              (*   ZUSTAENDEN VERSCHIEDEN SIND) IN DIESEN BLOCK     *)
              (*   AUFGENOMMEN WERDEN. DAS ERFORDERT NATUERLICH     *)
              (*   AUCH MANIPULATIONEN AN DEM ERSTEN ELEMENT DES    *)
              (*   BLOCKS (STEUER-ELEMENT); Z.B. MUSS DIE ANZAHL    *)
              (*   HOCHGESETZT WERDEN.                              *)
              (*                                                    *)
              (******************************************************)

                      FIND ( CH ) ;
                      ENDKETTE := KETTE ;
                      Z2 := AZUSTAND -> . WERT ;
                      repeat
                        RKETTE := RKETTE -> . NEXT ;
                        if RKETTE -> . LINKS = Z2 then
                          begin
                            PROD := RKETTE -> ;
                            EZ := EZUSTAND ;
                            repeat
                              GETS ( EZ , EOK , Z1 ) ;
                              if Z1 <> Z2 then
                                begin
                                  PROD . LINKS := Z1 ;
                                  PROD . TERMNR := AKTTERMNR ;
                                  PRODAUS ( PROD )
                                end (* then *)
                            until EOK

              (*******************************************)
              (*                                         *)
              (*   EINFACHER FALL                        *)
              (*                                         *)
              (*******************************************)

                          end (* then *)
                        else
                          if RKETTE -> . LINKS = - 1 then
                            begin
                              SKETTE := RKETTE ;
                              ANZ := SKETTE -> . RECHTS ;
                              RKETTE := RKETTE -> . NEXT ;
                              repeat
                                if RKETTE -> . LINKS = Z2 then
                                  begin
                                    N := 0 ;
                                    X := RKETTE -> . NEXT ;
                                    RIGHT := RKETTE -> . RECHTS ;
                                    TERMC := RKETTE -> . TERMINAL ;
                                    TERMNR := RKETTE -> . TERMNR ;
                                    EZ := EZUSTAND ;
                                    repeat
                                      GETS ( EZ , EOK , Z1 ) ;
                                      if Z1 <> Z2 then
                                        begin
                                          N := N + 1 ;
                                          NEW1 ( PRODV ) ;
                                          PRODV -> . LINKS := Z1 ;
                                          PRODV -> . TERMINAL := TERMC
                                                   ;
                                          PRODV -> . RECHTS := RIGHT ;
                                          PRODV -> . TERMNR := TERMNR ;
                                          RKETTE -> . NEXT := PRODV ;
                                          RKETTE := RKETTE -> . NEXT
                                        end (* then *)
                                    until EOK ;
                                    RKETTE -> . NEXT := X ;
                                    SKETTE -> . RECHTS := SKETTE -> .
                                                   RECHTS + N ;
                                    RKETTE := RKETTE -> . NEXT
                                  end (* then *) ;
                                ANZ := ANZ - 1
                              until ANZ <= 0

              (*******************************************)
              (*                                         *)
              (*   KOMPLIZIERTERER FALL                  *)
              (*                                         *)
              (*******************************************)

                            end (* then *)
                      until RKETTE = ENDKETTE ;

              (*******************************************)
              (*                                         *)
              (*   ANHAENGEN DER ANFANGS-                *)
              (*   ZUSTAENDE AN DIE KETTE                *)
              (*   DER ENDZUSTAENDE                      *)
              (*                                         *)
              (*******************************************)

                      if CHSTERN = '*' then
                        begin
                          AZ := AZUSTAND ;
                          EZ := EZUSTAND ;
                          while EZ -> . LINK <> NIL do
                            EZ := EZ -> . LINK ;
                          repeat
                            GETS ( AZ , EOK , I ) ;
                            PUTS ( EZ , I )
                          until EOK
                        end (* then *) ;
                    end (* else *)
                end (* then *)
            end (* FAKTOR *) ;

         begin (* TERM *)
           FAKTOR ( AZUSTAND , EZ1 ) ;

           (*****************************************************)
           (*                                                   *)
           (*   HIER BEGINNT DIE PROZEDUR TERM.                 *)
           (*                                                   *)
           (*   ZUM BESSEREN UEBERBLICK HIER NOCH EINMAL DIE    *)
           (*   DEKLARATION:                                    *)
           (*                                                   *)
           (*   PROCEDURE TERM ( AZUSTAND: ZUSTVERW;            *)
           (*                    VAR EZUSTAND: ZUSTVERW);       *)
           (*                                                   *)
           (*   UND DIE DEFINITION AUS DER GRAMMATIK:           *)
           (*                                                   *)
           (*   TERM  ->  TERM  FAKTOR                          *)
           (*   TERM  ->  FAKTOR                                *)
           (*                                                   *)
           (*   (EINFACHER:  TERM  ->  FAKTOR  FAKTOR * )       *)
           (*                                                   *)
           (*   DIE PROZEDUR TERM LAESST SICH SEHR SCHNELL      *)
           (*   BESCHREIBEN: SOLANGE WEDER ')' NOCH '/' VOR-    *)
           (*   LIEGT, STATT DESSEN ABER '(' ODER '''' UND      *)
           (*   NOCH KEIN DATEIENDE ERREICHT IST, HANDELT ES    *)
           (*   SICH UM EINE KONKATENATION (ANEINANDERHAENGEN   *)
           (*   VON -FAKTOREN-)                                 *)
           (*                                                   *)
           (*   IN DIESEM FALL WIRD EINFACH FOLGENDES GE-       *)
           (*   MACHT:                                          *)
           (*                                                   *)
           (*   DIE ENDZUSTAENDE DES ERSTEN -FAKTORS- GEBEN     *)
           (*   DIE ANFANGSZUSTAENDE DES ZWEITEN. SOFERN DER    *)
           (*   ERSTE NICHT DER ERSTE IN DIESEM TERM UEBER-     *)
           (*   HAUPT WAR, WERDEN SEINE ANFANGSZUSTAENDE        *)
           (*   NICHT MEHR BENOETIGT, SIE KOENNEN DAHER AUF     *)
           (*   DIE RESERVEKETTE GELEGT WERDEN (SIEHE           *)
           (*   DISPOSE2)                                       *)
           (*                                                   *)
           (*****************************************************)

           while ( CH <> '/' ) and ( CH <> ')' ) and not EOF ( QUELLE )
           and ( ( CH = '''' ) or ( CH = '(' ) ) do
             begin
               if OK then
                 begin
                   AZ1 := EZ1 ;
                   EZ1 := NIL
                 end (* then *)
               else
                 AZ1 := AZUSTAND ;
               FAKTOR ( AZ1 , EZ1 ) ;
               if AZ1 <> AZUSTAND then
                 DISPOSE2 ( AZ1 )
             end (* while *) ;
           EZUSTAND := EZ1
         end (* TERM *) ;

      begin (* AUSDRUCK *)
        KETTANFANG := KETTE ;

        (*****************************************************)
        (*                                                   *)
        (*   HIER BEGINNT DIE PROZEDUR -AUSDRUCK-.           *)
        (*                                                   *)
        (*   ZUM BESSEREN UEBERBLICK HIER NOCH EINMAL DIE    *)
        (*   DEKLARATION:                                    *)
        (*                                                   *)
        (*   PROCEDURE AUSDRUCK ( AZUSTAND: ZUSTVERW;        *)
        (*                        VAR EZUSTAND: ZUSTVERW;    *)
        (*                        STIEFE: INTEGER );         *)
        (*                                                   *)
        (*   UND DIE DEFINITION AUS DER GRAMMATIK:           *)
        (*                                                   *)
        (*   AUSDRUCK  ->  AUSDRUCK / TERM                   *)
        (*   AUSDRUCK  ->  TERM                              *)
        (*                                                   *)
        (*   (EINFACHER:  AUSDRUCK -> TERM ( / TERM ) *      *)
        (*                                                   *)
        (*   STIEFE BEDEUTET HIERBEI DIE VERSCHACHTELUNGS-   *)
        (*   TIEFE. SIE WIRD BEI JEDEM AUFRUF UM EINS        *)
        (*   HOCHGEZAEHLT UND DIENT ZUR UNTERSCHIEDLICHEN    *)
        (*   FEHLERBEHANDLUNG, JE NACHDEM, OB MAN SICH AUF   *)
        (*   DER OBERSTEN EBENE BEFINDET ODER NICHT.         *)
        (*                                                   *)
        (*****************************************************)

        ENDE := FALSE ;
        if STIEFE = 0 then
          AKTTERMNR := AKTTERMNR + 1 ;
        TERM ( AZUSTAND , EZUSTAND ) ;
        EZ2 := EZUSTAND ;
        ENDE := EOF ( QUELLE ) or ( ( CH = ')' ) and ( STIEFE <> 0 ) )
                ;

        (********************************************************)
        (*                                                      *)
        (*   IN DER PROZEDUR -AUSDRUCK- WIRD FOLGENDES          *)
        (*   GEMACHT:                                           *)
        (*                                                      *)
        (*   DIE AUFRUFE VON -TERM- ERFOLGEN IMMER MIT DEN-     *)
        (*   SELBEN ANFANGSZUSTAENDEN; DIE SICH DARAUS ER-      *)
        (*   GEBENDEN ENDZUSTAENDE WERDEN ZUSAMMENGEFASST.      *)
        (*                                                      *)
        (*   STIEFE DIENT ZUR STEUERUNG DER FEHLER- UND         *)
        (*   ENDEBEHANDLUNG.                                    *)
        (*                                                      *)
        (*   DIE PROZEDUR -OPTIMIERUNG- BERUHT AUF DEM          *)
        (*   PRINZIP, MEHRERE VERSCHIEDENE ENDZUSTAENDE EINES   *)
        (*   AUSDRUCKS, DIE AUF DER LINKEN SEITE NICHT VOR-     *)
        (*   KOMMEN, ZU EINEM ZUSTAND ZUSAMMENZUFASSEN.         *)
        (*   DASS DAS NICHTS AUSMACHT, IST KLAR UND DIE         *)
        (*   VEREINFACHUNG DADURCH IST GEWALTIG.                *)
        (*                                                      *)
        (*   (DIE PROZEDUR -OPTIMIERUNG- IST WEITER OBEN,       *)
        (*   BEI IHRER DEKLARATION, BESCHRIEBEN.)               *)
        (*                                                      *)
        (********************************************************)

        while not ENDE do
          begin
            if CH = '/' then
              begin
                FIND ( CH ) ;
                if STIEFE = 0 then
                  AKTTERMNR := AKTTERMNR + 1 ;
                TERM ( AZUSTAND , EZ1 ) ;
                if OK then
                  begin
                    while EZ2 -> . LINK <> NIL do
                      EZ2 := EZ2 -> . LINK ;
                    EZ2 -> . LINK := EZ1
                  end (* then *)
              end (* then *)
            else
              begin
                ERROR ( 9 ) ;
                if ( CH <> ')' ) or ( STIEFE = 0 ) then
                  FIND ( CH ) ;
                if ( CH = '''' ) or ( CH = '(' ) then
                  begin
                    if STIEFE = 0 then
                      AKTTERMNR := AKTTERMNR + 1 ;
                    TERM ( AZUSTAND , EZ1 )
                  end (* then *)
              end (* else *) ;
            ENDE := EOF ( QUELLE ) or ( ( CH = ')' ) and ( STIEFE <> 0
                    ) )
          end (* while *) ;
        KETTANFANG := KETTANFANG -> . NEXT ;
        if ( EZUSTAND -> . LINK <> NIL ) and ( STIEFE > 0 ) then
          OPTIMIERUNG ( KETTANFANG , EZUSTAND )
      end (* AUSDRUCK *) ;

   begin (* SYNTAXPRUEFUNG *)
     EOLNMOEGLICH := TRUE ;

     (*****************************************************)
     (*                                                   *)
     (*   RUMPF DER PROZEDUR 'SYNTAXPRUEFUNG'.            *)
     (*                                                   *)
     (*   BEVOR DIE SYNTAXPRUEFUNG DURCH DEN AUFRUF       *)
     (*   'AUSDRUCK(AZUSTAND,EZUSTAND,0)' GESTARTET       *)
     (*   WERDEN KANN, MUESSEN EINIGE INITIALISIE-        *)
     (*   RUNGEN VORGENOMMEN WERDEN. ZUM BEISPIEL:        *)
     (*                                                   *)
     (*    - EOLN IST MOEGLICH,                           *)
     (*    - FEHLERZAHL IST NULL,                         *)
     (*    - ANFANG (SIEHE READQ)                         *)
     (*    - AKZUSTAND IST NULL (ZUSTANDSPEGEL)           *)
     (*    - BILDEN DES ANFANGSZUSTANDES (EINS),          *)
     (*      D.H. ERZEUG. EINES ELEM. AN 'AZUSTAND'.      *)
     (*    - INITIALISIEREN DER FEHLER-KETTE.             *)
     (*    - INITIALISIEREN DER PROD.-KETTE UND           *)
     (*      ANHAENGEN EINER PSEUDO-PRODUKTION            *)
     (*      (MAXINT -> 'X' MAXINT), DIE BEIM             *)
     (*      SORTIEREN AN DIE LETZTE STELLE KOMMT         *)
     (*      UND SOMIT ALS BREMSE FUER TEIL 2             *)
     (*      DIENEN KANN.                                 *)
     (*    - OK IST TRUE,                                 *)
     (*    - UEBERLESEN DER ERSTEN SPACES BIS ZUM         *)
     (*      ERSTEN RICHTIGEN ZEICHEN.                    *)
     (*                                                   *)
     (*   HINTER DEM AUFRUF VON 'AUSDRUCK (...)'          *)
     (*   STEHT DAS GLOBALE LABEL 999 FUER DEN            *)
     (*   DATEIENDE-FEHLERFALL.                           *)
     (*                                                   *)
     (*   AKTIVITAETEN ZUM SCHLUSS:                       *)
     (*                                                   *)
     (*    - PKETTE UM EINES WEITERSETZEN WEGEN DES       *)
     (*      LEEREN ELEMENTS AM ANFANG.                   *)
     (*    - FEHLERAUSGABE FUER LETZTE ZEILE.             *)
     (*    - FALLS KEIN FEHLER, MELDUNG UND               *)
     (*      SORTIEREN DER ENDZUSTAENDE,                  *)
     (*      ANSONSTEN MELDUNG UEBER ANZAHL DER FEHLER.   *)
     (*                                                   *)
     (*****************************************************)

     FEZAHL := 0 ;
     ANFANG := TRUE ;
     AKZUSTAND := 0 ;
     NEW2 ( AZUSTAND ) ;
     NEUZUSTAND ;
     AZUSTAND -> . WERT := AKZUSTAND ;
     AZUSTAND -> . LINK := NIL ;
     NEW ( FEANFANG ) ;
     FEANFANG -> . NUMMER := 0 ;
     FEANFANG -> . NAECHST := NIL ;
     FKETTE := FEANFANG ;
     NEW1 ( PKETTE ) ;
     KETTE := PKETTE ;
     PSEUDOPROD . LINKS := MAXINT ;
     PSEUDOPROD . RECHTS := MAXINT ;
     PSEUDOPROD . TERMINAL := 'X' ;
     AKTTERMNR := 0 ;
     PSEUDOPROD . TERMNR := AKTTERMNR ;
     PRODAUS ( PSEUDOPROD ) ;
     OK := TRUE ;
     FIND ( CH ) ;
     AUSDRUCK ( AZUSTAND , EZUSTAND , 0 ) ;
     999 :
     PKETTE := PKETTE -> . NEXT ;
     WRITELN ;
     FEHLERAUSGABE ;
     WRITELN ;
     if FEZAHL = 0 then
       begin
         WRITELN ( 'KEINE SYNTAKTISCHEN FEHLER' ) ;
         EZSORT ( EZUSTAND )
       end (* then *)
     else
       WRITELN ( 'DER REGULAERE AUSDRUCK ENTHAELT ' , FEZAHL : 3 ,
                 ' SYNTAXFEHLER' )
   end (* SYNTAXPRUEFUNG *) ;


procedure PRODSORT ( var PRODKETTE : PRODVER ) ;

(***************************************************************)
(*                                                             *)
(*   DIE PROZEDUR -PRODSORT- DIENT DAZU, DIE ERZEUGTEN         *)
(*   PRODUKTIONEN ENTSPRECHEND DEN ANFORDERUNGEN DES           *)
(*   TEILS 2 (PROCEDURE DETAUTOMAT) ZU SORTIEREN.              *)
(*                                                             *)
(*   DAZU DIENT EINE ALS FUNCTION DEFINIERTE KLEINER-          *)
(*   RELATION FUER PRODUKTIONEN (SIEHE UNTEN)                  *)
(*                                                             *)
(***************************************************************)


   var BLOCKKETTE : PRODVER ;
       NEUKETTE , NEU1 , PUFFER , MINIMUMVOR , MINIMUM , P1VOR , P1 :
                                                   PRODVER ;


   procedure BLOCKAUSLESEN ( var PRKETTE , BLKETTE : PRODVER ) ;

   (*****************************************************)
   (*                                                   *)
   (*   DIE PROZEDUR 'BLOCKAUSLESEN' DIENT DAZU,        *)
   (*   VOR DEM SORTIEREN DER PRODUKTIONEN (ER-         *)
   (*   FORDERLICH FUER TEIL 2) DIE VON-BIS-            *)
   (*   BLOECKE AUS DER KETTE DER PRODUKTIONEN          *)
   (*   HERAUSZUNEHMEN UND IN EINE EIGENE KETTE         *)
   (*   ('BLKETTE') ABZULEGEN.                          *)
   (*                                                   *)
   (*   DER AUFBAU DER VON-BIS-BLOECKE IST UNTER        *)
   (*   DER PROZEDUR 'F2' BESCHRIEBEN.                  *)
   (*                                                   *)
   (*   DIE 'PRKETTE' IST DER EINGANGSPARAMETER;        *)
   (*   DIE VON-BIS-BLOECKE KOMMEN NACH 'BLKETTE'       *)
   (*   UND DER REST, ALSO DIE NORMALEN PRO-            *)
   (*   DUKTIONEN, BLEIBEN IN 'PRKETTE'.                *)
   (*                                                   *)
   (*****************************************************)


      var P1 , P2 , P1VOR : PRODVER ;
          ANZAHL , I : INTEGER ;

      begin (* BLOCKAUSLESEN *)
        NEW1 ( BLKETTE ) ;
        P2 := BLKETTE ;
        P2 -> . NEXT := NIL ;
        NEW1 ( P1 ) ;
        P1 -> . LINKS := 0 ;
        P1 -> . NEXT := PRKETTE ;
        PRKETTE := P1 ;

        (**************************************)
        (*                                    *)
        (*   PSEUDO-ELEMENTE EINFUEGEN.       *)
        (*                                    *)
        (**************************************)

        while P1 <> NIL do
          begin
            if P1 -> . LINKS = - 1 then
              begin

        (*****************************************************)
        (*                                                   *)
        (*   VON-BIS-ELEMENT, GEKENNZEICHNET DURCH -1,       *)
        (*   AUS DER KETTE HERAUSNEHMEN UND IN DIE NEUE      *)
        (*   KETTE 'BLKETTE' MITTELS 'P2' AUFNEHMEN.         *)
        (*                                                   *)
        (*   DIE LAENGE DES VON-BIS-ELEMENTS ERGIBT          *)
        (*   SICH AUS 'ANZAHL' (RECHTE SEITE + 2).           *)
        (*                                                   *)
        (*****************************************************)

                P2 -> . NEXT := P1 ;
                ANZAHL := P1 -> . RECHTS + 2 ;
                for I := 1 to ANZAHL do
                  P2 := P2 -> . NEXT ;
                P1VOR -> . NEXT := P2 -> . NEXT ;
                P1 := P1VOR -> . NEXT ;
                P2 -> . NEXT := NIL
              end (* then *)
            else
              begin
                P1VOR := P1 ;
                P1 := P1 -> . NEXT
              end (* else *)
          end (* while *) ;

        (**************************************)
        (*                                    *)
        (*   PSEUDO-ELEMENTE HERAUSNEHMEN     *)
        (*   UND LOESCHEN (DISPOSE1).         *)
        (*                                    *)
        (**************************************)

        P1 := BLKETTE ;
        BLKETTE := BLKETTE -> . NEXT ;
        P2 := PRKETTE ;
        PRKETTE := PRKETTE -> . NEXT ;
        P1 -> . NEXT := NIL ;
        DISPOSE1 ( P1 ) ;
        P2 -> . NEXT := NIL ;
        DISPOSE1 ( P2 )
      end (* BLOCKAUSLESEN *) ;


   function KLEINER ( P1 , P2 : PRODVER ) : BOOLEAN ;

   (*******************************************)
   (*                                         *)
   (*   DEFINITION EINER KLEINER-RELATION     *)
   (*   FUER PRODUKTIONEN.                    *)
   (*                                         *)
   (*   DIE DREI TEILE HABEN FOLGENDE         *)
   (*   PRIORITAET ENACH RECHTS ABFALLEND):   *)
   (*                                         *)
   (*   ZUST-LINKS / TERMINAL / ZUST-RECHTS   *)
   (*                                         *)
   (*******************************************)


      begin (* KLEINER *)
        if P1 -> . LINKS <> P2 -> . LINKS then
          KLEINER := P1 -> . LINKS < P2 -> . LINKS
        else
          if P1 -> . TERMINAL <> P2 -> . TERMINAL then
            KLEINER := P1 -> . TERMINAL < P2 -> . TERMINAL
          else
            KLEINER := P1 -> . RECHTS < P2 -> . RECHTS
      end (* KLEINER *) ;

   begin (* PRODSORT *)
     BLOCKAUSLESEN ( PRODKETTE , BLOCKKETTE ) ;

     (********************************************************)
     (*                                                      *)
     (*   DIE PROZEDUR -PRODSORT- IST, AUSSER DEM AUFRUF     *)
     (*   VON -BLOCKAUSLESEN- AM ANFANG, LOGISCH GENAUSO     *)
     (*   AUFGEBAUT WIE DIE PROZEDUR -EZSORT-, DIE WEITER    *)
     (*   OBEN (ZIEMLICH AM ANFANG) BESCHRIEBEN IST.         *)
     (*                                                      *)
     (*   AUF EINE DETAILLIERTE BESCHREIBUNG HIER KANN       *)
     (*   ALSO VERZICHTET WERDEN.                            *)
     (*                                                      *)
     (*   (EINZIGER UNTERSCHIED: WO BEI EZSORT 'X < Y'       *)
     (*   STEHT, STEHT HIER -KLEINER ( X,Y )-).              *)
     (*                                                      *)
     (********************************************************)

     NEW1 ( NEUKETTE ) ;
     NEUKETTE -> . NEXT := NIL ;
     NEU1 := NEUKETTE ;
     NEW1 ( PUFFER ) ;
     PUFFER -> . NEXT := PRODKETTE ;
     while PRODKETTE <> NIL do
       begin
         MINIMUMVOR := PUFFER ;
         MINIMUM := PRODKETTE ;
         P1VOR := PRODKETTE ;
         P1 := PRODKETTE -> . NEXT ;
         while P1 <> NIL do
           begin
             if KLEINER ( P1 , MINIMUM ) then
               begin
                 MINIMUM := P1 ;
                 MINIMUMVOR := P1VOR
               end (* then *) ;
             P1 := P1 -> . NEXT ;
             P1VOR := P1VOR -> . NEXT
           end (* while *) ;
         NEU1 -> . NEXT := MINIMUM ;
         if MINIMUM = PRODKETTE then
           PRODKETTE := PRODKETTE -> . NEXT ;
         MINIMUMVOR -> . NEXT := MINIMUM -> . NEXT ;
         MINIMUM -> . NEXT := NIL ;
         NEU1 := NEU1 -> . NEXT
       end (* while *) ;
     PRODKETTE := NEUKETTE -> . NEXT ;
     NEUKETTE -> . NEXT := NIL ;
     DISPOSE1 ( NEUKETTE ) ;
     DISPOSE1 ( PUFFER ) ;
     if BLOCKKETTE <> NIL then
       begin
         NEUKETTE := BLOCKKETTE ;
         while NEUKETTE -> . NEXT <> NIL do
           NEUKETTE := NEUKETTE -> . NEXT ;
         NEUKETTE -> . NEXT := PRODKETTE ;
         PRODKETTE := BLOCKKETTE
       end (* then *)
   end (* PRODSORT *) ;


procedure AUTOMGENERATOR ( ENDZUSTANKER : ZUSTVERW ; PRODANKER :
                         PRODVER ) ;

(********************************************************************)
(*                                                                  *)
(*   PROCEDURE AUTOMGENERATOR - TEIL 3                              *)
(*                                                                  *)
(*   DIE PROZEDUR 'AUTOMGENERATOR' STELLT DEN (DYNAMISCH) DRITTEN   *)
(*   UND LETZTEN TEIL DES PROGRAMMES DAR. SIE ERZEUGT AUS DEN       *)
(*   (NACH LINKS,TERMINAL,RECHTS) SORTIERTEN PRODUKTIONEN UND       *)
(*   DER KETTE DER ENDZUSTAENDE DAS PASCAL-PROGRAMM, DAS DEN        *)
(*   AUTOMATEN SIMULIERT.                                           *)
(*                                                                  *)
(*   BEDEUTUNG DER PARAMETER:                                       *)
(*                                                                  *)
(*   ENDZUSTANKER - ANKER FUER DIE KETTE DER ENDZUSTAENDE           *)
(*                                                                  *)
(*   PRODANKER    - ANKER FUER DIE KETTE DER PRODUKTIONEN.          *)
(*                                                                  *)
(*   ES WIRD EIN PASCAL-PROGRAMM 'AUTOMAT' ERZEUGT, DAS ZEICHEN-    *)
(*   WEISE VOM FILE INPUT LIEST UND DIE DURCH DEN REGULAEREN        *)
(*   AUSDRUCK BESCHRIEBENEN WOERTER ERKENNT. FALLS DAS EINGE-       *)
(*   GEBENE WORT NICHT IN DER DURCH DEN REG. AUSDRUCK BE-           *)
(*   SCHRIEBENEN REGULAEREN SPRACHE ENTHALTEN IST, GIBT DAS         *)
(*   PROGRAMM EINE FEHLERMELDUNG AUS (ZUM FRUEHESTMOEGLICHEN        *)
(*   ZEITPUNKT); DER EVTL. VORHANDENE REST DER EINGABE BLEIBT       *)
(*   UNBERUECKSICHTIGT. ANSONSTEN KOMMT EINE MELDUNG 'OK'.          *)
(*                                                                  *)
(*   IM PROGRAMM AUTOMAT WIRD DER AUGENBLICKLICHE ZUSTAND           *)
(*   DURCH EINE INTEGER-VARIABLE 'ZUSTAND' DARGESTELLT. DAS         *)
(*   PROGRAMM BESTEHT IM PRINZIP AUS ZWEISTUFIG GESCHACHTELTEN      *)
(*   CASE-ANWEISUNGEN. DIE ERSTE CASE-ANWEISUNG BEZIEHT SICH        *)
(*   AUF DIE MOEGLICHEN ZUSTAENDE; DIE CASE-ANWEISUNGEN DER         *)
(*   ZWEITEN STUFE (INNERHALB DER ERSTEN) GEBEN FUER JEDEN          *)
(*   ZUSTAND AN, WAS BEI WELCHEM EINGELESENEN ZEICHEN ZU MACHEN     *)
(*   IST (BZW. WELCHES DER NEUE ZUSTAND IST). DABEI WERDEN          *)
(*   SELBSTVERSTAENDLICH VORHER DIE EINGELESENEN ZEICHEN            *)
(*   (MITTELS EINER 'IF EINGABEZEICHEN IN ... '-ANWEISUNG)          *)
(*   AUF ZULAESSIGKEIT GEPRUEFT.                                    *)
(*                                                                  *)
(********************************************************************)


   var PRODZEIGER , HILFSPRODZ , PRODNACHFOLGER , HPRODZ : PRODVER ;
       ENDZUSTZEIGER : ZUSTVERW ;
       HILFSZUSTVAR , Z10 : INTEGER ;
       ZEICHENMENGE : set of CHAR ;
       ANFANGSELEM , ENDELEM , KLEINSTES , GROESSTES , ANFANGSCHAR ,
       TESTCHAR : CHAR ;
       ANFANG : BOOLEAN ;

       (*****************************************************)
       (*                                                   *)
       (*   BEI DEN VARIABLEN HANDELT ES SICH HAUPTSAECH-   *)
       (*   LICH UM HILFSVARIABLEN, DIE UNTEN (BEI DER      *)
       (*   ANWENDUNG) BESCHRIEBEN WERDEN. EINE AUSNAHME    *)
       (*   BILDET VIELLEICHT DIE VARIABLE 'Z10' (SIEHE     *)
       (*   AUCH PROZEDUR 'ZEILENDE'). SIE DIENT DAZU,      *)
       (*   DIE ANZAHL DER AUFRUFE VON 'ZEILENDE' FEST-     *)
       (*   ZUHALTEN, DAMIT NACH ZEHN 'ZEILENDE'-AUFRUFEN   *)
       (*   EIN ZEILENVORSCHUB ERZEUGT WERDEN KANN.         *)
       (*                                                   *)
       (*****************************************************)



   procedure ZEILENDE ( EINRUECK : BOOLEAN ) ;

   (*****************************************************)
   (*                                                   *)
   (*   (SIEHE AUCH VARIABLE 'Z10')                     *)
   (*   BEI JEDEM ZEHNTEN AUFRUF VON 'ZEILENDE'         *)
   (*   WIRD EIN ZEILENVORSCHUB ERZEUGT (UND ZWAR       *)
   (*   JE NACH WERT DES PARAMETER EINRUECK MIT         *)
   (*   ANSCHLIESSENDEM EINRUECKEN ODER OHNE).          *)
   (*                                                   *)
   (*   DIE PROZEDUR 'ZEILENDE' DIENT DAZU, BEI         *)
   (*   KOMPLEXEREN AUSDRUECKEN DER FORM ('A','B',      *)
   (*   'C','D','E', ... ) NACH ZEHN ELEMENTEN          *)
   (*   EINEN ZEILENWECHSEL ZU VERANLASSEN, UM DIE      *)
   (*   LAENGE DER QUELLZEILEN (IM PROGRAMM             *)
   (*   'AUTOMAT') AUF EIN VERNUENFTIGES MASS ZU        *)
   (*   BESCHRAENKEN. (VERWENDUNG MIT EINRUECKEN BEI    *)
   (*   AUSDRUECKEN DER FORM 'IF EINGABEZEICHEN IN      *)
   (*    (' ... ')'; OHNE EINRUECKEN BEI CASE-LABELS    *)
   (*   'CASE EINGABEZEICHEN OF                         *)
   (*   'A','B','C', ... )                              *)
   (*                                                   *)
   (*****************************************************)


      begin (* ZEILENDE *)
        if Z10 >= 10 then
          begin
            WRITELN ( AUTOM ) ;
            if EINRUECK then
              WRITE ( AUTOM , '                     ' ) ;
            Z10 := 0 ;
          end (* then *) ;
        Z10 := Z10 + 1 ;
      end (* ZEILENDE *) ;


   procedure WRITECHAR ( CH : CHAR ) ;

   (*****************************************************)
   (*                                                   *)
   (*   SCHREIBT EIN ZEICHEN IN DER EXTERNEN            *)
   (*   PASCAL-DARSTELLUNG AUF DEN FILE 'AUTOM'         *)
   (*   (D.H. IN DAS ZU ERZEUGENDE PASCAL-PROGRAMM).    *)
   (*   DIES BEINHALTET ZUM EINEN DAS EINSCHLIESSEN     *)
   (*   IN APOSTROPHE, ZUM ANDEREN DAS DARSTELLEN       *)
   (*   EINES APOSTROPHES DURCH ZWEI ('''')             *)
   (*                                                   *)
   (*****************************************************)


      begin (* WRITECHAR *)
        WRITE ( AUTOM , '''' ) ;
        if CH = '''' then
          WRITE ( AUTOM , '''' ) ;
        WRITE ( AUTOM , CH , '''' )
      end (* WRITECHAR *) ;


   function ECHTENDE ( ZUSTAND : INTEGER ) : BOOLEAN ;

   (*****************************************************)
   (*                                                   *)
   (*   DIE FUNCTION 'ECHTENDE' DIENT DAZU, ABZU-       *)
   (*   PRUEFEN, OB EIN GEGEBENER ZUSTAND AUF           *)
   (*   DER LINKEN SEITE DER PRODUKTIONEN               *)
   (*   NIRGENDS VORKOMMT. BESONDERHEIT: DIE            *)
   (*   KETTE DER PRODUKTIONEN WIRD NUR AB DEM          *)
   (*   DURCH 'PRODZEIGER' EINGESTELLTEN ELEMENT        *)
   (*   DURCHLAUFEN. DA DIE ENDZUSTAENDE SORTIERT       *)
   (*   VORLIEGEN, GENUEGT ES, DEN 'PRODZEIGER'         *)
   (*   AM ANFANG AUF 'PRODANKER' ZU LEGEN UND          *)
   (*   DANN DIE FUNCTION 'ECHTENDE' SUKZESSIVE         *)
   (*   IN DER REIHENFOLGE DER ENDZUSTAENDE AUF-        *)
   (*   ZURUFEN.                                        *)
   (*                                                   *)
   (*   ES WIRD HIER AUCH (ZURECHT) VORAUSGESETZT,      *)
   (*   DASS DIE PRODUKTIONEN NACH DER LINKEN           *)
   (*   SEITE (IN HOECHSTER PRIORITAET) SORTIERT        *)
   (*   VORLIEGEN.                                      *)
   (*                                                   *)
   (*****************************************************)


      begin (* ECHTENDE *)
        while PRODZEIGER -> . LINKS < ZUSTAND do
          PRODZEIGER := PRODZEIGER -> . NEXT ;
        ECHTENDE := ZUSTAND <> PRODZEIGER -> . LINKS
      end (* ECHTENDE *) ;

   begin (* AUTOMGENERATOR *)
     REWRITE ( AUTOM ) ;
     WRITELN ( AUTOM , 'PROGRAM AUTOMAT(INPUT,OUTPUT);' ) ;
     WRITELN ( AUTOM , 'VAR EINGABEZEICHEN,FEHLERZEICHEN:CHAR;' ) ;
     WRITELN ( AUTOM , '   ZUSTAND:INTEGER;' ) ;
     WRITELN ( AUTOM , '   FEHLER:BOOLEAN;' ) ;
     WRITELN ( AUTOM , 'BEGIN' ) ;
     WRITELN ( AUTOM , 'READ(EINGABEZEICHEN);' ) ;
     PRODZEIGER := PRODANKER ;
     WRITELN ( AUTOM , 'ZUSTAND:=' , PRODZEIGER -> . LINKS , ';' ) ;
     WRITELN ( AUTOM , 'FEHLER:=FALSE;' ) ;
     WRITELN ( AUTOM , 'READ(EINGABEZEICHEN);' ) ;
     WRITELN ( AUTOM , 'WHILE (NOT EOF(INPUT))   AND (NOT FEHLER) DO' )
               ;
     WRITELN ( AUTOM , 'BEGIN' ) ;
     WRITELN ( AUTOM , 'WRITE(EINGABEZEICHEN);' ) ;
     WRITELN ( AUTOM , 'CASE ZUSTAND OF' ) ;
     ENDZUSTZEIGER := ENDZUSTANKER ;

     (********************************************************)
     (*                                                      *)
     (*   BIS HIERHER WURDEN DIE ERSTEN ZEILEN DES ZU        *)
     (*   ERZEUGENDEN PASCAL-PROGRAMMS IN DIE DATEI          *)
     (*   'AUTOM' AUSGEGEBEN. DER REIHE NACH PASSIERT        *)
     (*   FOLGENDES (IM PROGRAMM AUTOMAT):                   *)
     (*                                                      *)
     (*   - KOPFZEILE (MIT FILES INPUT UND OUTPUT)           *)
     (*   - DEKLARATION DER VARIABLEN                        *)
     (*     ZWEI CHAR-VARIABLEN: EINGABEZEICHEN UND          *)
     (*                          FEHLERZEICHEN               *)
     (*     INTEGER-VARIABLE ZUSTAND                         *)
     (*     BOOLE'SCHE VARIABLE FEHLER                       *)
     (*   - 'BEGIN'                                          *)
     (*   - UEBERLESEN DES BLANKS AM ANFANG                  *)
     (*     (BESONDERHEIT DES TR440-PASCAL-LAUFZEITSYSTEMS)  *)
     (*   - ZUSTANDSVARIABLE BESETZEN (KLEINSTER ZUSTAND     *)
     (*     IST ZUGLEICH ANFANGSZUSTAND)                     *)
     (*   - 'FEHLER := FALSE'                                *)
     (*   - ERSTES ZEICHEN LESEN                             *)
     (*   - WHILE-SCHLEIFE                                   *)
     (*     (WIRD FUER JEDES EINGABEZEICHEN DURCHLAUFEN)     *)
     (*   - 'BEGIN'                                          *)
     (*   - EINGELESENES ZEICHEN AUSGEBEN                    *)
     (*   - GLOBALES CASE ('ZUSTAND')                        *)
     (*                                                      *)
     (*   NEBENBEI WURDEN DER 'PRODZEIGER' AUF DEN           *)
     (*   'PRODANKER' UND DER 'ENDZUSTZEIGER' AUF DEN        *)
     (*   'ENDZUSTANKER' GESETZT.                            *)
     (*                                                      *)
     (********************************************************)

     while ENDZUSTZEIGER <> NIL do
       begin
         if ECHTENDE ( ENDZUSTZEIGER -> . WERT ) then
           begin
             WRITE ( AUTOM , ENDZUSTZEIGER -> . WERT , ': BEGIN ' ) ;
             WRITELN ( AUTOM , 'FEHLER := TRUE;' ) ;
             WRITELN ( AUTOM , 'FEHLERZEICHEN:=EINGABEZEICHEN' ) ;
             WRITELN ( AUTOM , 'END;' )
           end (* then *) ;
         ENDZUSTZEIGER := ENDZUSTZEIGER -> . LINK
       end (* while *) ;

     (********************************************************)
     (*                                                      *)
     (*   (SIEHE AUCH BESCHREIBUNG PROZEDUR 'ECHTENDE')      *)
     (*   DAMIT SICHERGESTELLT IST, DASS FUER JEDEN ZU-      *)
     (*   STAND, DER AUFTRETEN KANN, EIN CASE-LABEL          *)
     (*   VORHANDEN IST, MUSS HIER FUER JEDEN (END-)ZU-      *)
     (*   STAND, DER NIRGENDS LINKS VORKOMMT, EIN FEHLER-    *)
     (*   AUSGANG ANGELEGT WERDEN.                           *)
     (*                                                      *)
     (*   DIE ERZEUGTE BEFEHLS-FOLGE HAT FOLGENDE FORM       *)
     (*   (ANNAHME: 4 IST ENDZUSTAND, VON DEM KEIN UEBER-    *)
     (*   GANG AUSGEHT):                                     *)
     (*                                                      *)
     (*            4: BEGIN                                  *)
     (*   FEHLER := TRUE;                                    *)
     (*   FEHLERZEICHEN:=EINGABEZEICHEN                      *)
     (*   END;                                               *)
     (*                                                      *)
     (*   ANSCHLIESSEND WIRD DER 'PRODZEIGER' WIEDER AUF     *)
     (*   'PRODANKER' GESETZT.                               *)
     (*                                                      *)
     (********************************************************)

     PRODZEIGER := PRODANKER ;
     while PRODZEIGER -> . LINKS <> MAXINT do
       begin
         HILFSZUSTVAR := PRODZEIGER -> . LINKS ;
         WRITE ( AUTOM , HILFSZUSTVAR , ':' ) ;
         PRODNACHFOLGER := PRODZEIGER -> . NEXT ;

     (***************************************************************)
     (*                                                             *)
     (*   DIE HIER BEGINNENDE WHILE-SCHLEIFE WIRD FUER JEDEN        *)
     (*   ZUSTAND AUFGERUFEN, DER AUF DER LINKEN SEITE VORKOMMT     *)
     (*   UND GIBT DEN DAZUGEHOERIGEN ZWEIG DES CASE-STATEMENTS IN  *)
     (*   DIE DATEI 'AUTOM' AUS.                                    *)
     (*                                                             *)
     (*   IM GROBEN WERDEN HIER ZWEI FAELLE UNTERSCHIEDEN:          *)
     (*   ZUM EINEN DER FALL, DASS ZU DEM AKTUELLEN ZUSTAND         *)
     (*   NUR EINE PRODUKTION VORHANDEN IST. DIESER FALL            *)
     (*   WIRD IN DEM ELSE-ZWEIG (WEITER UNTEN) BEHANDELT. DER      *)
     (*   ANDERE FALL, DASS NAEMLICH MEHRERE PRODUKTIONEN VOR-      *)
     (*   HANDEN SIND, GESTALTET SICH ETWAS KOMPLIZIERTER           *)
     (*   (HILFSZUSTVAR = PRODNACHFOLGER->.LINKS).                  *)
     (*                                                             *)
     (*   IN DIESEM FALL SOLL NAEMLICH DER CASE-ZWEIG WIEDERUM      *)
     (*   DURCH EIN CASE-STATEMENT DARGESTELLT WERDEN, WAS DIE      *)
     (*   SCHWIERIGKEIT MIT SICH BRINGT, DASS STANDARD-PASCAL       *)
     (*   FUER DAS CASE-STATEMENT KEINEN ELSE-FALL VORSIEHT. MAN    *)
     (*   MUSS ALSO VORHER GEEIGNET PRUEFEN, OB AUCH EIN IN         *)
     (*   DEM CASE-STATEMENT BEHANDELTER FALL VORLIEGT.             *)
     (*                                                             *)
     (*   DIES GESCHIEHT DURCH DIE ABFRAGE 'IF EINGABEZEICHEN       *)
     (*   IN ...'. DIE ANSCHLIESSENDE SET-KONSTANTE WIRD            *)
     (*   IN DER UNTEN FOLGENDEN WHILE-SCHLEIFE ERZEUGT.            *)
     (*   SIEHE DAZU DEN FOLGENDEN KOMMENTAR.                       *)
     (*                                                             *)
     (***************************************************************)

         if HILFSZUSTVAR = PRODNACHFOLGER -> . LINKS then
           begin
             WRITELN ( AUTOM , 'BEGIN ' ) ;
             WRITE ( AUTOM , 'IF EINGABEZEICHEN IN (.' ) ;
             HILFSPRODZ := PRODZEIGER ;
             Z10 := 0 ;
             ANFANG := TRUE ;
             while HILFSPRODZ -> . LINKS = HILFSZUSTVAR do
               begin
                 if ANFANG then
                   ANFANG := FALSE
                 else
                   WRITE ( AUTOM , ',' ) ;
                 ZEILENDE ( TRUE ) ;
                 ANFANGSELEM := HILFSPRODZ -> . TERMINAL ;
                 HPRODZ := HILFSPRODZ ;
                 while ( HPRODZ -> . LINKS = HPRODZ -> . NEXT -> .
                 LINKS ) and ( HPRODZ -> . NEXT -> . TERMINAL = SUCC (
                 HPRODZ -> . TERMINAL ) ) do
                   HPRODZ := HPRODZ -> . NEXT ;
                 ENDELEM := HPRODZ -> . TERMINAL ;
                 HILFSPRODZ := HPRODZ -> . NEXT ;
                 if ANFANGSELEM = ENDELEM then
                   WRITECHAR ( ANFANGSELEM )
                 else
                   begin
                     WRITECHAR ( ANFANGSELEM ) ;
                     WRITE ( AUTOM , '..' ) ;
                     WRITECHAR ( ENDELEM )
                   end (* else *)
               end (* while *) ;

     (********************************************************)
     (*                                                      *)
     (*   ZU BEGINN WIRD 'HILFSPRODZ' AUF 'PRODZEIGER',      *)
     (*   'Z10' AUF NULL UND 'ANFANG' AUF TRUE GESETZT.      *)
     (*                                                      *)
     (*   DIE WHILE-SCHLEIFE WIRD SOLANGE DURCHLAUFEN,       *)
     (*   BIS EIN NEUER ZUSTAND AUF DER LINKEN SEITE         *)
     (*   AUFTRITT (VARIABLE 'HILFSPRODZ').                  *)
     (*                                                      *)
     (*   AUSSER AM ANFANG WIRD IMMER EIN KOMMA AUSGE-       *)
     (*   GEBEN. ES WIRD VERSUCHT, DIE BUCHSTABEN AUS DEN    *)
     (*   PRODUKTIONEN SOWEIT ALS MOEGLICH ZU GRUPPIEREN,    *)
     (*   D.H. TEILAUSDRUECKE DER FORM 'X' .. 'Y' ZU         *)
     (*   BILDEN. DIE PRODUKTIONEN-KETTE WIRD, AUSGEHEND     *)
     (*   VOM STAND VON 'HILFSPRODZ', MIT DER VARIABLEN      *)
     (*   'HPRODZ' SOLANGE DURCHLAUFEN, BIS EINE LUECKE      *)
     (*   IN DER MENGE DER BUCHSTABEN AUFTRITT (SIEHE        *)
     (*   BEDINGUNG MIT SUCC (...) ). WENN MEHRERE ZU-       *)
     (*   SAMMENHAENGENDE ELEMENTE (MEHR ALS EINS) GEFUN-    *)
     (*   DEN WURDEN, WIRD EIN AUSDRUCK 'X' .. 'Y' AUS-      *)
     (*   GEGEBEN, SONST NUR EIN EINZELNES CHARACTER.        *)
     (*                                                      *)
     (********************************************************)

             WRITE ( AUTOM , '.) THEN' ) ;
             WRITELN ( AUTOM ) ;
             WRITELN ( AUTOM , 'CASE EINGABEZEICHEN OF' ) ;
             while ( HILFSZUSTVAR = PRODZEIGER -> . LINKS ) do
               begin
                 WRITECHAR ( PRODZEIGER -> . TERMINAL ) ;
                 Z10 := 0 ;
                 while ( HILFSZUSTVAR = PRODZEIGER -> . NEXT -> . LINKS
                 ) and ( PRODZEIGER -> . NEXT -> . RECHTS = PRODZEIGER
                 -> . RECHTS ) do
                   begin
                     ZEILENDE ( FALSE ) ;
                     PRODZEIGER := PRODZEIGER -> . NEXT ;
                     WRITE ( AUTOM , ',' ) ;
                     WRITECHAR ( PRODZEIGER -> . TERMINAL )
                   end (* while *) ;
                 WRITE ( AUTOM , ': ZUSTAND := ' , PRODZEIGER -> .
                         RECHTS ) ;
                 PRODZEIGER := PRODZEIGER -> . NEXT ;
                 if HILFSZUSTVAR = PRODZEIGER -> . LINKS then
                   WRITE ( AUTOM , ';' ) ;
                 WRITELN ( AUTOM )
               end (* while *) ;

     (***************************************************************)
     (*                                                             *)
     (*   FUER DAS EIGENTLICHE CASE-STATEMENT WIRD DIE KETTE DER    *)
     (*   PRODUKTIONEN NOCHMALS, DIESMAL MIT DER VARIABLEN          *)
     (*   'PRODZEIGER' DURCHLAUFEN.                                 *)
     (*                                                             *)
     (*   BEI DER CASE-ANWEISUNG KANN KEINE GRUPPIERUNG DER         *)
     (*   FORM 'X' .. 'Y' ERFOLGEN; DESHALB MUSSTE MAN SICH         *)
     (*   HIER DARAUF BESCHRAENKEN, PRODUKTIONEN, DIE NACH-         *)
     (*   EINANDER KOMMEN UND DENSELBEN FOLGEZUSTAND (RECHTS)       *)
     (*   TRAGEN, ZUSAMMENZUFASSEN (SIEHE INNERE WHILE-SCHLEIFE).   *)
     (*                                                             *)
     (*   DER VARIABLEN 'ZUSTAND' WIRD DER FOLGEZUSTAND ZUGE-       *)
     (*   WIESEN.                                                   *)
     (*                                                             *)
     (***************************************************************)

             WRITELN ( AUTOM , 'END' ) ;
             WRITELN ( AUTOM , 'ELSE BEGIN FEHLER:= TRUE;' ) ;
             WRITELN ( AUTOM , 'FEHLERZEICHEN:=EINGABEZEICHEN' ) ;
             WRITELN ( AUTOM , 'END' ) ;
             WRITE ( AUTOM , 'END' ) ;
           end (* then *)
         else
           begin
             WRITE ( AUTOM , 'IF EINGABEZEICHEN =''' ) ;
             if PRODZEIGER -> . TERMINAL = '''' then
               WRITE ( AUTOM , '''' ) ;
             WRITELN ( AUTOM , PRODZEIGER -> . TERMINAL , ''' ' ) ;
             WRITELN ( AUTOM , '  THEN ZUSTAND := ' , PRODZEIGER -> .
                       RECHTS ) ;
             PRODZEIGER := PRODZEIGER -> . NEXT ;
             WRITELN ( AUTOM , '  ELSE BEGIN' ) ;
             WRITELN ( AUTOM , '    FEHLER := TRUE;' ) ;
             WRITELN ( AUTOM , '    FEHLERZEICHEN := EINGABEZEICHEN' )
                       ;
             WRITE ( AUTOM , '  END' )

     (********************************************************)
     (*                                                      *)
     (*   ELSE-ZWEIG:                                        *)
     (*                                                      *)
     (*   WENN ES SICH NUR UM EINE MOEGLICHE PRODUKTION      *)
     (*   HANDELT, WERDEN STATEMENTS DER FOLGENDEN FORM      *)
     (*   AUSGEGEBEN:                                        *)
     (*                                                      *)
     (*            4: IF EINGABEZEICHEN = 'X'                *)
     (*     THEN ZUSTAND := 5                                *)
     (*     ELSE BEGIN                                       *)
     (*       FEHLER := TRUE;                                *)
     (*       FEHLERZEICHEN := EINGABEZEICHEN                *)
     (*     END;                                             *)
     (*                                                      *)
     (*                                                      *)
     (********************************************************)

           end (* else *) ;
         if PRODZEIGER -> . LINKS <> MAXINT then
           WRITE ( AUTOM , ';' ) ;
         WRITELN ( AUTOM ) ;
       end (* while *) ;
     WRITELN ( AUTOM , 'END;' ) ;
     WRITELN ( AUTOM , 'IF EOLN(INPUT) THEN' ) ;
     WRITELN ( AUTOM , 'BEGIN READLN;' ) ;
     WRITELN ( AUTOM , 'WRITELN;' ) ;
     WRITELN ( AUTOM , 'END;' ) ;
     WRITELN ( AUTOM , 'READ(EINGABEZEICHEN);' ) ;
     WRITELN ( AUTOM , 'END;' ) ;

     (*******************************************)
     (*                                         *)
     (*   ENDE DER WHILE-SCHLEIFE.              *)
     (*                                         *)
     (*******************************************)

     WRITELN ( AUTOM , 'WRITELN;' ) ;
     WRITELN ( AUTOM , 'IF FEHLER THEN' ) ;
     WRITE ( AUTOM , 'IF FEHLERZEICHEN IN  (.' ) ;
     PRODZEIGER := PRODANKER ;
     ZEICHENMENGE := [ PRODZEIGER -> . TERMINAL ] ;
     KLEINSTES := PRODZEIGER -> . TERMINAL ;
     GROESSTES := PRODZEIGER -> . TERMINAL ;
     PRODZEIGER := PRODZEIGER -> . NEXT ;
     while PRODZEIGER -> . LINKS <> MAXINT do
       begin
         if not ( PRODZEIGER -> . TERMINAL in ZEICHENMENGE ) then
           begin
             ZEICHENMENGE := ZEICHENMENGE + [ PRODZEIGER -> . TERMINAL
                             ] ;
             if KLEINSTES > PRODZEIGER -> . TERMINAL then
               KLEINSTES := PRODZEIGER -> . TERMINAL ;
             if GROESSTES < PRODZEIGER -> . TERMINAL then
               GROESSTES := PRODZEIGER -> . TERMINAL ;
           end (* then *) ;
         PRODZEIGER := PRODZEIGER -> . NEXT ;
       end (* while *) ;
     TESTCHAR := KLEINSTES ;
     while TESTCHAR <= GROESSTES do
       begin
         if TESTCHAR in ZEICHENMENGE then
           begin
             WRITECHAR ( TESTCHAR ) ;
             ANFANGSCHAR := TESTCHAR ;
             while SUCC ( TESTCHAR ) in ZEICHENMENGE do
               TESTCHAR := SUCC ( TESTCHAR ) ;
             if TESTCHAR <> ANFANGSCHAR then
               begin
                 WRITE ( AUTOM , '..' ) ;
                 WRITECHAR ( TESTCHAR )
               end (* then *) ;
             if TESTCHAR < GROESSTES then
               WRITE ( AUTOM , ',' ) ;
             ZEILENDE ( TRUE )
           end (* then *) ;
         TESTCHAR := SUCC ( TESTCHAR )
       end (* while *) ;

     (***************************************************************)
     (*                                                             *)
     (*   FEHLERANALYSE:                                            *)
     (*                                                             *)
     (*   BEIM AUFTRETEN EINES FEHLER WIRD ZWISCHEN ZWEI FAELLEN    *)
     (*   UNTERSCHIEDEN:                                            *)
     (*     1. DAS ZEICHEN IST UEBERHAUPT NICHT IM ALPHABET         *)
     (*        ENTHALTEN.                                           *)
     (*     2. DER AUTOMAT HAELT NICHT IN EINEM ENDZUSTAND.         *)
     (*                                                             *)
     (*   WEGEN DER VOLLSTAENDIGKEIT DER UEBERGAENGE IN EINEM       *)
     (*   DETERMINISTISCHEN AUTOMATEN (D.H. FUER JEDES PAAR         *)
     (*   <ZUSTAND>,<ZEICHEN> GIBT ES EINEN FOLGEZUSTAND) IST DIE   *)
     (*   ZWEITE FEHLERMELDUNG AUCH IN DEM FALL ANGEBRACHT,         *)
     (*   DASS UNTER EINEM GEGEBENEN ZUSTAND FUER DAS EINGELESENE   *)
     (*   ZEICHEN KEIN UEBERGANG EXISTIERT.                         *)
     (*                                                             *)
     (*   UM DIE FEHLERANALYSE DURCHFUEHREN ZU KOENNEN, MUSS        *)
     (*   MAN ABPRUEFEN, OB DAS FEHLERZEICHEN ZU DEM ALPHABET       *)
     (*   GEHOERT ODER NICHT. DAS ALPHABET SIND DABEI ALLE          *)
     (*   DIE ZEICHEN, DIE IRGENDWO IN DER KETTE DER PRODUKTIONEN   *)
     (*   VORKOMMEN.                                                *)
     (*                                                             *)
     (*   MAN MUSS ALSO EINE ABFRAGE 'IF FEHLERZEICHEN IN           *)
     (*   ... ' EINBAUEN. DAZU WIRD ALS ERSTES DIE 'ZEICHENMENGE'   *)
     (*   (JETZT IST DAMIT DIE VARIABLE DES VORLIEGENDEN PRO-       *)
     (*   GRAMMS GEMEINT) AUFGEBAUT, UND ZWAR IN DER OBEREN         *)
     (*   WHILE-SCHLEIFE. GLEICHZEITIG DAZU WERDEN ZWEI VARIABLE    *)
     (*   'GROESSTES' UND 'KLEINSTES' MIT DEN ENTSPRECHENDEN        *)
     (*   ELEMENTEN AUS DER 'ZEICHENMENGE' BESETZT. DAMIT KANN      *)
     (*   DANN (IN DER FOLGENDEN WHILE-SCHLEIFE) DER SET-           *)
     (*   AUSDRUCK ERZEUGT WERDEN (ANALOG OBEN).                    *)
     (*                                                             *)
     (***************************************************************)

     WRITE ( AUTOM , '.) THEN' ) ;
     WRITELN ( AUTOM ) ;
     WRITE ( AUTOM , 'WRITELN' ) ;
     WRITE ( AUTOM , '(''**** FEHLER: AUTOMAT HAELT NICHT IN' ,
             ' EINEM ENDZUSTAND'')' ) ;
     WRITELN ( AUTOM ) ;
     WRITELN ( AUTOM ,
              'ELSE WRITELN(''**** FEHLER: FALSCHES EINGABEZEICHEN'')'
               ) ;
     ENDZUSTZEIGER := ENDZUSTANKER ;
     while ENDZUSTZEIGER <> NIL do
       begin
         WRITELN ( AUTOM , 'ELSE IF ZUSTAND=' , ENDZUSTZEIGER -> . WERT
                   , ' THEN WRITELN(''OK'')' ) ;
         ENDZUSTZEIGER := ENDZUSTZEIGER -> . LINK
       end (* while *) ;
     WRITE ( AUTOM , 'ELSE WRITELN' ) ;
     WRITE ( AUTOM , '(''**** FEHLER: AUTOMAT HAELT NICHT IN' ,
             ' EINEM ENDZUSTAND'')' ) ;

     (************************************************)
     (*                                              *)
     (*   IN DEM NUN FOLGENDEN KOMMENTAR IST ALS     *)
     (*   BEISPIEL EIN ERZEUGTES PROGRAMM ANGE-      *)
     (*   GEBEN. DER EINGEGEBENE REGULAERE AUS-      *)
     (*   DRUCK SAH SO AUS:                          *)
     (*                                              *)
     (*      'ABC' * 'D' / 'EF'                      *)
     (*                                              *)
     (*   AN DIESEM BEISPIEL SIND ALLE OBEN ER-      *)
     (*   WAEHNTEN KONSTRUKTE ZU ERKENNEN.           *)
     (*                                              *)
     (************************************************)

     WRITELN ( AUTOM ) ;
     WRITELN ( AUTOM , 'END.' ) ;
   end (* AUTOMGENERATOR *) ;


procedure DETAUTOMAT ( var PRODANKER : PRODVER ; var ENDANKER :
                     ZUSTVERW ; MAXZUSTAND : INTEGER ) ;

(********************************************************************)
(*                                                                  *)
(*   PROZEDUR DETAUTOMAT. TEIL 2                                    *)
(*                                                                  *)
(*   AUFGABE (WIE BEREITS OBEN GESAGT):                             *)
(*   UMWANDELN DES NICHTDETERMINISTISCHEN AUTOMATEN IN EINEN        *)
(*   AEQUIVALENTEN DETERMINISTISCHEN AUTOMATEN.                     *)
(*                                                                  *)
(*   METHODE: DIE LEXIKOGRAPHISCH ANGEORDNETEN PRODUKTIONEN         *)
(*            (ORDNUNGSPRIORITAET: ZUSTAND LINKS / TERMINAL /       *)
(*            ZUSTAND RECHTS) WERDEN AUF NICHTDETERMINISMEN         *)
(*            UNTERSUCHT. TRITT EIN NICHTDETERMINISMUS AUF,         *)
(*            SO WERDEN AM ENDE DER PRODUKTIONSKETTE DIE            *)
(*            BENOETIGTEN NEUEN PRODUKTIONEN ANGEHAENGT UND DIE     *)
(*            NICHTDETERMINISTISCHEN IN DER PRODUKTIONSKETTE        *)
(*            BESEITIGT.                                            *)
(*                                                                  *)
(*            FUER JEDEN NEU SO ERZEUGTEN ZUSTAND WIRD EIN          *)
(*            VERZEICHNIS ANGELEGT, WELCHE ZUSTAENDE ER ZU-         *)
(*            SAMMENFASST.                                          *)
(*                                                                  *)
(*   BEDEUTUNG DER FORMALEN PARAMETER:                              *)
(*                                                                  *)
(*   - VAR PRODANKER : ANKER FUER DIE PRODUKTIONSKETTE              *)
(*                                                                  *)
(*   - VAR ENDANKER  : ANKER FUER DIE KETTE DER ENDZUSTAENDE        *)
(*                                                                  *)
(*   - MAXZUSTAND    : INTEGER-VARIABLE, DIE DIE HOECHSTE IN        *)
(*                     PROZEDUR 'SYNTAXPRUEFUNG' VERGEBENE          *)
(*                     ZUSTANDSNUMMER ALS EINGABEPARAMETER          *)
(*                     ERHAELT.                                     *)
(*                                                                  *)
(*                                                                  *)
(********************************************************************)


   const SETSIZE = 48 ;
         SETSIZEMINUS1 = 47 ;

   type ZUSTAND = 0 .. SETSIZEMINUS1 ;
        MENGE = set of ZUSTAND ;
        BLOCKVERWEIS = -> BLOCK ;
        BLOCK = record
                  NEXT : BLOCKVERWEIS ;
                  SATZ : MENGE
                end ;
        INDETVERWEIS = -> INDETZUSTAND ;
        INDETZUSTAND = record
                         NEXT : INDETVERWEIS ;
                         ELEMENTE : BLOCKVERWEIS ;
                         ZUSTAND : INTEGER
                       end ;

        (**********************************************************)
        (*                                                        *)
        (*   BEI DEN HIER NEU AUFGEBAUTEN ZUSTAENDEN MUSS FEST-   *)
        (*   GEHALTEN WERDEN, WELCHE ZUSTAENDE SIE ZUSAMMEN-      *)
        (*   FASSEN. DESWEGEN WIRD EINE KETTE VON RECORDS VOM     *)
        (*   TYP 'INDETZUSTAND' AUFGEBAUT.                        *)
        (*                                                        *)
        (*   JEDER RECORD STELLT EINEN NEUEN ZUSTAND DAR. DIE     *)
        (*   ZUSTAENDE, DIE DER NEUE ZUSTAND IN SICH VEREINIGT    *)
        (*   HAT, WERDEN AN JEDEN RECORD ALS POINTERKETTE,        *)
        (*   DEREN ELEMENTE VOM TYP 'BLOCK' SIND, ANGEHAENGT.     *)
        (*   JEWEILS 48 ZUSTAENDE (KONSTANTE 'SETSIZE') WERDEN    *)
        (*   DABEI IN EINEM 'BLOCK' ZUSAMMENGEFASST.              *)
        (*                                                        *)
        (*   DIE DATENSTRUKTUR HAT ALSO FOLGENDES AUSSEHEN:       *)
        (*                                                        *)
        (*   INDETZUSTAND : - ZUSTAND  : NUMMER DES NEUEN         *)
        (*                               ZUSTANDES                *)
        (*                  - ELEMENTE : VERWEIS AUF DIE          *)
        (*                               ZUGEHOERIGE KETTE        *)
        (*                               VON BLOECKEN, DIE        *)
        (*                               DIE IN DIESEM ZU-        *)
        (*                               STAND ZUSAMMENGE-        *)
        (*                               FASSTEN ZUSTAENDE        *)
        (*                               ENTHALTEN.               *)
        (*                  - NEXT     : VERWEIS AUF DEN          *)
        (*                               NAECHSTEN 'INDET-        *)
        (*                               ZUSTAND'.                *)
        (*                                                        *)
        (*   BLOCK        : - NEXT     : VERWEIS AUF DEN          *)
        (*                               NAECHSTEN BLOCK.         *)
        (*                  - SATZ     : SET OF 0..SETSIZEMINUS1, *)
        (*                               IN DEM DIE ZU-           *)
        (*                               STAENDE AUFGEFUEHRT      *)
        (*                               SIND, DIE DURCH DEN      *)
        (*                               NEUEN ZUSTAND DAR-       *)
        (*                               GESTELLT WERDEN.         *)
        (*                               DIE REIHENFOLGE DER      *)
        (*                               BLOECKE ENTSCHEIDET      *)
        (*                               UEBER DIE NUMMERN-       *)
        (*                               BEREICHE DER SIE         *)
        (*                               BEINHALTENDEN ZU-        *)
        (*                               STANDSNUMMERN :          *)
        (*                               1. BLOCK: 0 - SETSIZE-1  *)
        (*                               2. BLOCK: SETSIZE        *)
        (*                                       - 2*SETSIZE-1    *)
        (*                                 .                      *)
        (*                                 .                      *)
        (*                               N-TER BLOCK:             *)
        (*                                 (N-1)*SETSIZE   -      *)
        (*                                    N *SETSIZE-1.       *)
        (*                                                        *)
        (*   DURCH DIESE KONSTRUKTION IST GEWAEHRLEISTET,         *)
        (*   DASS DAS PROGRAMM KEINE OBERGRENZE FUER DIE          *)
        (*   ANZAHL DER ZUSTAENDE BESITZT (MIT AUSNAHME           *)
        (*   DER MASCHINENABHAENGIGEN KONSTANTE MAXINT).          *)
        (*   DIES GILT SOWOHL ABSOLUT ALS AUCH FUER DIE           *)
        (*   DURCH EINEN ZUSTAND REPRAESENTIERTEN                 *)
        (*   'RECHTEN SEITEN' VON NICHTDETERMINISTISCHEN          *)
        (*   PRODUKTIONEN.                                        *)
        (*                                                        *)
        (**********************************************************)


   var MAXINDETZUST : INTEGER ;
       SCHLUSS , EINS , ZWEI : PRODVER ;
       ENDSCHLUSS : ZUSTVERW ;
       INDETANKER : INDETVERWEIS ;
       SETANKER : BLOCKVERWEIS ;
       LOESCHE : ZUSTVERW ;

       (********************************************************)
       (*                                                      *)
       (*   BEDEUTUNG DER ZUR PROZEDUR 'DETAUTOMAT' LOKALEN    *)
       (*   VARIABLEN:                                         *)
       (*                                                      *)
       (*   - MAXINDETZUST : IST MIT DER ALS NAECHSTEN ZU      *)
       (*                    VERGEBENDEN ZUSTANDSNUMMER        *)
       (*                    BELEGT (BISHER GROESSTE VER-      *)
       (*                    GEBENE ZUSTANDSNUMMER PLUS 1)     *)
       (*   - SCHLUSS      : ANKER, DER AUF DAS ENDE DER       *)
       (*                    IN PROZEDUR 'SYNTAXPRUEFUNG'      *)
       (*                    ERZEUGTEN KETTE ZEIGT.            *)
       (*   - EINS,ZWEI    : ZWEI LAUFVARIABLEN FUER DIE       *)
       (*                    PRODUKTIONSKETTE. EINS MAR-       *)
       (*                    KIERT DEN ERSTEN RECORD EINER     *)
       (*                    FOLGE VON NICHTDETERMINISTI-      *)
       (*                    SCHEN PRODUKTIONEN. ZWEI ZEIGT    *)
       (*                    AUF DEN, DEM LETZTEN ZU EINS      *)
       (*                    GEHOERENDEN NICHTDETERMINIST.     *)
       (*                    RECORD, FOLGENDEN.                *)
       (*   - ENDESCHLUSS  : ZEIGT AUF DEN LETZTEN RECORD      *)
       (*                    DER ENDZUSTANDSKETTE. DIENT       *)
       (*                    ZUM ANHAENGEN EINER BREMSE AM     *)
       (*                    ENDE DER KETTE.                   *)
       (*   - INDETANKER   : ANKER FUER DIE KETTE DER NEUEN    *)
       (*                    DETERMINISTISCHEN ZUSTAENDE,      *)
       (*                    DIE DIE ALTEN NICHTDETERMI-       *)
       (*                    NISMEN ERSETZEN.                  *)
       (*   - SETANKER     : ANKER FUER DIE KETTE VON ZU-      *)
       (*                    STAENDEN, DIE VON EINEM NEUEN     *)
       (*                    ZUSTAND ZUSAMMENGEFASST WERDEN.   *)
       (*   - LOESCHE      : LAUFVARIABLE FUER DIE KETTE DER   *)
       (*                    ENDZUSTAENDE. DIENT ZUM           *)
       (*                    LOESCHEN DER BREMSE AM ENDE DER   *)
       (*                    KETTE.                            *)
       (*                                                      *)
       (********************************************************)



   function EQUAL2 ( EINS , ZWEI : PRODVER ) : BOOLEAN ;

   (*******************************************)
   (*                                         *)
   (*   FUNKTION 'EQUAL2' :                   *)
   (*                                         *)
   (*   SIE LIEFERT DEN WERT TRUE, WENN       *)
   (*   DIE BEIDEN UEBERGEBENEN RECORDS       *)
   (*   EINEN NICHTDETERMINISMUS AUFWEISEN    *)
   (*   (DAS HEISST : LINKS UND TERMINAL      *)
   (*   MUESSEN DIE GLEICHEN WERTE ENT-       *)
   (*   HALTEN)                               *)
   (*                                         *)
   (*******************************************)


      begin (* EQUAL2 *)
        EQUAL2 := ( EINS -> . LINKS = ZWEI -> . LINKS ) and ( EINS -> .
                  TERMINAL = ZWEI -> . TERMINAL )
      end (* EQUAL2 *) ;


   procedure BAUESET ( var SETANKER : BLOCKVERWEIS ; MAX : INTEGER ) ;

   (*******************************************)
   (*                                         *)
   (*   PROZEDUR BAUESET:                     *)
   (*                                         *)
   (*   SIE BAUT EINE KETTE ENT-              *)
   (*   SPRECHENDER LAENGE AUF, DIE ZUR       *)
   (*   ABSPEICHERUNG DER ZUSTAENDE           *)
   (*   DIENT, DIE DURCH EINEN NEUEN          *)
   (*   ABGEDECKT WERDEN MUESSEN.             *)
   (*                                         *)
   (*******************************************)


      var NEUSET : BLOCKVERWEIS ;

      begin (* BAUESET *)
        SETANKER := NIL ;
        while MAX > 0 do
          begin
            NEW ( NEUSET ) ;
            NEUSET -> . NEXT := SETANKER ;
            SETANKER := NEUSET ;
            MAX := MAX - SETSIZE
          end (* while *)
      end (* BAUESET *) ;


   function VERGLEICHESET ( VERWEIS1 , VERWEIS2 : BLOCKVERWEIS ) :
                          BOOLEAN ;

   (*******************************************)
   (*                                         *)
   (*   FUNKTION VERGLEICHESET:               *)
   (*                                         *)
   (*   SIE LIEFERT DEN WERT TRUE,            *)
   (*   WENN DIE SICH ENTSPRECHENDEN          *)
   (*   SETS DER UEBERGEBENEN KETTEN          *)
   (*   UEBEREINSTIMMEN.                      *)
   (*                                         *)
   (*******************************************)


      var GLEICH : BOOLEAN ;

      begin (* VERGLEICHESET *)
        repeat
          GLEICH := VERWEIS1 -> . SATZ = VERWEIS2 -> . SATZ ;
          VERWEIS1 := VERWEIS1 -> . NEXT ;
          VERWEIS2 := VERWEIS2 -> . NEXT
        until ( not GLEICH ) or ( VERWEIS1 = NIL ) ;
        VERGLEICHESET := GLEICH
      end (* VERGLEICHESET *) ;


   procedure AUSHAENGE ( var RETTEANFANG : PRODVER ; var RETTE :
                       PRODVER ; var ENDE : PRODVER ) ;

   (*******************************************)
   (*                                         *)
   (*   PROZEDUR AUSHAENGE:                   *)
   (*                                         *)
   (*   SIE HAENGT NICHT MEHR BENOE-          *)
   (*   TIGTE OBJEKTE VOM TYP 'PRODUK-        *)
   (*   TION' AUS DER KETTE AUS UND           *)
   (*   STELLT SIE DER EIGENEN FREI-          *)
   (*   SPEICHERVERWALTUNG ZUR VER-           *)
   (*   FUEGUNG.                              *)
   (*                                         *)
   (*   INHALT DER FORMALEN PARAMETER:        *)
   (*                                         *)
   (*   - RETTEANFANG : (ZEIGT AUF)           *)
   (*         LETZTES ELEMENT VOR             *)
   (*         DEN AUSZUHAENGENDEN             *)
   (*         ELEMENTEN.                      *)
   (*   - RETTE : LETZTES ELEMENT             *)
   (*         DER AUSZUHAENGENDEN             *)
   (*         ELEMENTE.                       *)
   (*   - ENDE : ERSTES ELEMENT NACH          *)
   (*         DEN AUSZUHAENGENDEN             *)
   (*         ELEMENTEN.                      *)
   (*                                         *)
   (*******************************************)


      begin (* AUSHAENGE *)
        RETTE -> . NEXT := NIL ;
        DISPOSE1 ( RETTEANFANG -> . NEXT ) ;
        RETTEANFANG -> . NEXT := ENDE
      end (* AUSHAENGE *) ;


   procedure NICHTDET ( var PRODANKER : PRODVER ; var ENDANKER :
                      ZUSTVERW ; var INDETANKER : INDETVERWEIS ; ANFANG
                      : PRODVER ; var ENDE : PRODVER ; var ALTENDE :
                      PRODVER ; var SCHLUSSANKER : ZUSTVERW ; var
                      MAXINDETZUST : INTEGER ; MAXZUSTAND : INTEGER ;
                      var SETANKER : BLOCKVERWEIS ) ;

   (*****************************************************************)
   (*                                                               *)
   (*   PROZEDUR NICHTDET :                                         *)
   (*                                                               *)
   (*   DIE AUFGABE DIESER PROZEDUR IST ES, EINEN ERKANNTEN         *)
   (*   INDETERMINISMUS ZU BESEITIGEN.                              *)
   (*                                                               *)
   (*   BEDEUTUNG DER FORMALEN PARAMETER :                          *)
   (*                                                               *)
   (*   - VAR PRODANKER    : ANKER FUER DIE PRODUKTIONSKETTE        *)
   (*   - VAR ENDANKER     : ANKER FUER DIE KETTE DER               *)
   (*                        ENDZUSTAENDE                           *)
   (*   - VAR INDETANKER   : ANKER FUER DIE KETTE DER ERSATZ-       *)
   (*                        ZUSTAENDE                              *)
   (*   -     ANFANG       : ZEIGT AUF DIE ERSTE PRODUKTION         *)
   (*                        DES INDETERMINISMUS.                   *)
   (*   - VAR ENDE         : ZEIGT BEIM AUFRUF VON 'NICHTDET'       *)
   (*                        AUF DIE ZWEITE PRODUKTION DES          *)
   (*                        INDETERMINISMUS.                       *)
   (*                        BEIM RUECKSPRUNG IN DIE PROZEDUR       *)
   (*                        'DETAUTOMAT' ZEIGT 'ENDE' AUF          *)
   (*                        DIE ERSTE DEM INDETERMINISMUS          *)
   (*                        FOLGENDE PRODUKTION.                   *)
   (*   - VAR ALTENDE      : ZEIGT BEIM AUFRUF VON 'NICHTDET'       *)
   (*                        AUF DIE BREMSE DER PRODUKTIONEN-       *)
   (*                        KETTE. SIE DIENT ZUM VEREINFACHTEN     *)
   (*                        ANHAENGEN NEUER PRODUKTIONEN.          *)
   (*   - VAR SCHLUSSANKER : ANKER DER AUF DIE BREMSE DER           *)
   (*                        ENDZUSTAENDE ZEIGT.                    *)
   (*   - VAR MAXINDETZUST : GROSSTE BISHER VERGEBENE ZU-           *)
   (*                        STANDSNUMMER.                          *)
   (*   -     MAXZUSTAND   : GROESSTE IN PROZEDUR 'SYNTAX-          *)
   (*                        PRUEFUNG' VERGEBENE ZUSTANDS-          *)
   (*                        NUMMER.                                *)
   (*   - VAR SETANKER     : ANKER FUER DIE AKTUELL ZU              *)
   (*                        FUELLENDE BLOCKKETTE.                  *)
   (*                                                               *)
   (*   VORGEHENSWEISE :                                            *)
   (*                                                               *)
   (*   IN EINE LISTE VON SETS ( ANKER : VAR SETANKER )             *)
   (*   WERDEN DIE ZUSTAENDE EINGETRAGEN, DIE BEI DEM INDETER-      *)
   (*   MINISMUS DEN FOLGEZUSTAND BILDEN ( KOMPONENTE 'RECHTS'      *)
   (*   DES REKORDS 'PRODUKTION' ). SODANN WIRD IN DER PRO-         *)
   (*   ZEDUR 'SUCHEZUSTAND' UEBERPRUEFT, OB DIESE ZUSTANDS-        *)
   (*   KOMBINATION SCHON VORHANDEN IST. WENN NICHT, WIRD           *)
   (*   DIESE KOMBINATION IN DIE KETTE DER ERSATZZUSTAENDE          *)
   (*   AUFGENOMMEN UND AM ENDE DER PRODUKTIONENKETTE WERDEN        *)
   (*   DIE NEU BENOETIGTEN PRODUKTIONEN ANGEHAENGT.                *)
   (*   AM ENDE DER PROZEDUR WERDEN DIE ALTEN NICHT MEHR            *)
   (*   BENOETIGTEN PRODUKTIONEN, DIE JA INDETERMINIISTISCH         *)
   (*   WAREN, GELOESCHT UND DIE ENTSPRECHENDEN RECORDS IN DIE      *)
   (*   FREISPEICHERVERWALTUNGSLISTE EINGEFUEGT.                    *)
   (*                                                               *)
   (*****************************************************************)


      var RETTE , SUCHZUSTAND , RETTEANFANG : PRODVER ;
          ALT , ENDEZUST : BOOLEAN ;
          NUMMER : INTEGER ;
          WORKTERMNR : INTEGER ;
          TERMNR : INTEGER ;

          (*****************************************************)
          (*                                                   *)
          (*   BEDEUTUNG DER ZUR PROZEDUR 'NICHTDET'           *)
          (*   LOKALEN VARIABLEN :                             *)
          (*                                                   *)
          (*   - RETTE        : ZEIGT AUF DIE LETZTE           *)
          (*                    PRODUKTION DES IN-             *)
          (*                    DETERMINISMUS.                 *)
          (*   - SUCHEZUSTAND : HILSANKER ZUM AUFFINDEN        *)
          (*                    VON ZUSTAENDEN                 *)
          (*   - RETTEANFANG  : ZEIGT AUF DIE ERSTE            *)
          (*                    PRODUKTION DES INDE-           *)
          (*                    TERMINISMUS.                   *)
          (*   - ALT          : BOOLESCHE VARIABLE,            *)
          (*                    DIE DEN WERT 'TRUE'            *)
          (*                    BEKOMMT, WENN DIE              *)
          (*                    NEUE ZUSTANSKOMBI-             *)
          (*                    NATION SCHON VOR-              *)
          (*                    HANDEN IST.                    *)
          (*   - ENDEZUST     : BOOLESCHE VARIABLE,            *)
          (*                    DIE DEN WERT 'TRUE'            *)
          (*                    BEKOMMT, WENN DER              *)
          (*                    NEUE ZUSTAND EIN END-          *)
          (*                    ZUSTAND SEIN MUSS.             *)
          (*   - NUMMER       : TRAEGT DIE NUMMER DES          *)
          (*                    ERSATZZUSTANDES. WIRD          *)
          (*                    NUR GEBRAUCHT, WENN            *)
          (*                    DIE VARIABLE 'ALT'             *)
          (*                    DEN WERT 'TRUE' BE-            *)
          (*                    KOMMT.                         *)
          (*                                                   *)
          (*****************************************************)



      procedure FUELLESET ( var SETANKER : BLOCKVERWEIS ; START , ENDE
                          : PRODVER ; INDETANKER : INDETVERWEIS ;
                          MAXZUSTAND : INTEGER ) ;

      (*****************************************************)
      (*                                                   *)
      (*   PROZEDUR FUELLESET :                            *)
      (*   DIESE PROZEDUR DIENT ZUM AUFFUELLEN             *)
      (*   DER SETS MIT DEN RECHTEN SEITEN DES             *)
      (*   INDETERMINISMUS.                                *)
      (*                                                   *)
      (*   BEDEUTUNG DER FORMALEN PARAMETER UND            *)
      (*   DER LOKALEN VARIABLEN :                         *)
      (*                                                   *)
      (*   - VAR SETANKER    : ANKER DER ZU                *)
      (*                       FUELLENDEN SET-             *)
      (*                       KETTE.                      *)
      (*   -     START, ENDE : ERSTE, BZW. LETZTE          *)
      (*                       PRODUKTION DES IN-          *)
      (*                       DETERMINISMUS.              *)
      (*   -     INDETANKER  : ANKER FUER DIE              *)
      (*                       KETTE DER ERSATZ-           *)
      (*                       ZUSTAENDE.                  *)
      (*   -     MAXZUSTAND  : GROESSTE ZUSTANDS-          *)
      (*                       NUMMER, DIE IN              *)
      (*                       PROZEDUR 'SYNTAX-           *)
      (*                       PRUEFUNG' VERGEBEN          *)
      (*                       WURDE.                      *)
      (*   -     LAUFANKER   : LOKALE HILFSVARI-           *)
      (*                       ABLE ZUM DURCHLAU-          *)
      (*                       FEN DER POINTER-            *)
      (*                       KETTE.                      *)
      (*   -     LAUF        : LOKALE ZAEHLVARI-           *)
      (*                       ABLE.                       *)
      (*   -     RECHTS      : LOKALE VARIABLE             *)
      (*                       ZUM ZWISCHENSPEI-           *)
      (*                       CHERN DER RECHTEN           *)
      (*                       SEITEN.                     *)
      (*                                                   *)
      (*****************************************************)


         var LAUFANKER : BLOCKVERWEIS ;
             LAUF , RECHTS : INTEGER ;


         procedure BILDENICHTDETSET ( var SETANKER : BLOCKVERWEIS ;
                                    NICHTDETZUSTAND : INTEGER ;
                                    INDETANKER : INDETVERWEIS ) ;

         (*****************************************************)
         (*                                                   *)
         (*   PROZEDUR BILDENICHTDETSET  :                    *)
         (*                                                   *)
         (*   DIESE PROZEDUR DIENT ZUM VEREINIGEN             *)
         (*   ZWEIER ERSATZZUSTAENDE.                         *)
         (*                                                   *)
         (*   DABEI WIRD ZUERST DER EINE ZUSTAND IN           *)
         (*   DER KETTE DER ERSATZZUSTAENDE GESUCHT           *)
         (*   UND DANN DESSEN ZUSTAENDE IN DEN AN-            *)
         (*   DEREN ERSATZZUSTAND UEBERNOMMEN                 *)
         (*   ( PROZEDUR VEREINIGE ).                         *)
         (*                                                   *)
         (*****************************************************)



            procedure VEREINIGE ( ADDOBJEKT : BLOCKVERWEIS ; var
                                ERGEBNIS : BLOCKVERWEIS ) ;

            (**************************************)
            (*                                    *)
            (*   PROZEDUR VEREINIGE :             *)
            (*                                    *)
            (*   ZWEI KETTEN GLEICHER             *)
            (*   LAENGE, JEWEILS VOM TYP          *)
            (*   'BLOCKVERWEIS', WERDEN           *)
            (*   KOMPONENTENWEISE AD-             *)
            (*   DIERT. DAS ERGEBNIS              *)
            (*   STEHT IM PARAMETER               *)
            (*   'ERGEBNIS'.                      *)
            (*                                    *)
            (**************************************)


               var LAUF : BLOCKVERWEIS ;

               begin (* VEREINIGE *)
                 LAUF := ERGEBNIS ;
                 repeat
                   LAUF -> . SATZ := LAUF -> . SATZ + ADDOBJEKT -> .
                                     SATZ ;
                   LAUF := LAUF -> . NEXT ;
                   ADDOBJEKT := ADDOBJEKT -> . NEXT
                 until LAUF = NIL
               end (* VEREINIGE *) ;

            begin (* BILDENICHTDETSET *)
              while INDETANKER -> . ZUSTAND <> NICHTDETZUSTAND do
                INDETANKER := INDETANKER -> . NEXT ;
              if INDETANKER -> . ZUSTAND = NICHTDETZUSTAND then
                VEREINIGE ( INDETANKER -> . ELEMENTE , SETANKER )
            end (* BILDENICHTDETSET *) ;

         begin (* FUELLESET *)
           LAUFANKER := SETANKER ;

           (*****************************************************)
           (*                                                   *)
           (*   RUMPF DER PROZEDUR 'FUELLESET' .                *)
           (*                                                   *)
           (*   DIE EINTRAGUNG DER FOLGEZUSTAENDE               *)
           (*   GLIEDERT SICH IN MEHRERE PHASEN.                *)
           (*   ZUERST WIRD DIE SETLISTE GELOESCHT,             *)
           (*   UND DANN WERDEN SCHRITT FUER SCHRITT            *)
           (*   DIE ZUSTAENDE EINGETRAGEN.                      *)
           (*                                                   *)
           (*****************************************************)

           repeat
             LAUFANKER -> . SATZ := [ ] ;
             LAUFANKER := LAUFANKER -> . NEXT
           until LAUFANKER = NIL ;

           (**************************************)
           (*                                    *)
           (*   IN DER REPEATSCHLEIFE            *)
           (*   WERDEN NUN DIE EINZU-            *)
           (*   TRAGENDEN ZUSTAENDE              *)
           (*   NACHEINANDER DURCHLAU-           *)
           (*   FEN.                             *)
           (*                                    *)
           (**************************************)

           repeat
             LAUFANKER := SETANKER ;
             RECHTS := START -> . RECHTS ;

           (**************************************)
           (*                                    *)
           (*   BEI DEN EINZUTRAGENDEN           *)
           (*   ZUSTAENDEN GIBT ES ZWEI          *)
           (*   ARTEN :                          *)
           (*                                    *)
           (*   - EINFACHER FALL :               *)
           (*     ZUSTAENDE, DIE BEREITS         *)
           (*     IN PROZEDUR 'SYNTAX-           *)
           (*     PRUEFUNG' VERWENDUNG           *)
           (*     FANDEN. DIESE ZUSTAEN-         *)
           (*     DE SIND KEINE ERSATZ-          *)
           (*     ZUSTAENDE UND KOENNEN          *)
           (*     DIREKT UEBERNOMMEN             *)
           (*     WERDEN.                        *)
           (*                                    *)
           (*   - SCHWIERIGER FALL :             *)
           (*     ZUSTAENDE, DIE IN TEIL         *)
           (*     ZWEI ( PROZEDUR 'DET-          *)
           (*     AUTOMAT' ) ZUM ERSTEN-         *)
           (*     MAL BENUTZT WERDEN.            *)
           (*     SIE SIND ERSATZZU-             *)
           (*     IN DER KETTE DURCH DIE         *)
           (*     VON IHNEN REPRAESEN-           *)
           (*     TIERTEN ZUSTAENDE ER-          *)
           (*     SETZT WERDEN, DA SONST         *)
           (*     BEI GEWISSEN INDETER-          *)
           (*     MINISMEN KEIN ABBRUCH          *)
           (*     DER UMWANDLUNG ER-             *)
           (*     REICHT WERDEN KANN.            *)
           (*                                    *)
           (**************************************)

             if RECHTS > MAXZUSTAND then

           (*********************************)
           (*                               *)
           (*   SCHWIERIGER FALL            *)
           (*                               *)
           (*********************************)

               BILDENICHTDETSET ( SETANKER , RECHTS , INDETANKER )
             else

           (*********************************)
           (*                               *)
           (*   EINFACHER FALL              *)
           (*                               *)
           (*********************************)

               begin
                 for LAUF := 1 to ( RECHTS - 1 ) DIV SETSIZE do
                   LAUFANKER := LAUFANKER -> . NEXT ;
                 LAUFANKER -> . SATZ := LAUFANKER -> . SATZ + [ RECHTS
                                        MOD SETSIZE ]
               end (* else *) ;
             START := START -> . NEXT
           until START = ENDE -> . NEXT
         end (* FUELLESET *) ;


      procedure SUCHEZUSTAND ( var INDETANKER : INDETVERWEIS ; var
                             SETANKER : BLOCKVERWEIS ; var ALT :
                             BOOLEAN ; MAXINDETZUST , MAXZUSTAND :
                             INTEGER ; var NUMMER : INTEGER ) ;

      (*****************************************************)
      (*                                                   *)
      (*   PROZEDUR SUCHEZUSTAND :                         *)
      (*                                                   *)
      (*   DIESE PROZEDUR HAT DIE AUFGABE                  *)
      (*   FESTZUSTELLEN, OB DIE NEUE ZUSTANDS-            *)
      (*   KOMBINATION SCHON VORHANDEN IST UND             *)
      (*   WENN NICHT , SIE AN DIE KETTE DER               *)
      (*   ERSATZZUSTAENDE ANZUHAENGEN.                    *)
      (*                                                   *)
      (*****************************************************)


         var INDETLAUF , NEUINDETZUST : INDETVERWEIS ;

         begin (* SUCHEZUSTAND *)
           ALT := FALSE ;
           INDETLAUF := INDETANKER ;
           while ( not ALT ) and ( INDETLAUF -> . ZUSTAND <> MAXINT )
           do
             begin
               ALT := VERGLEICHESET ( INDETLAUF -> . ELEMENTE ,
                      SETANKER ) ;
               NUMMER := INDETLAUF -> . ZUSTAND ;
               INDETLAUF := INDETLAUF -> . NEXT
             end (* while *) ;
           if not ALT then
             begin
               NEW ( NEUINDETZUST ) ;
               NEUINDETZUST -> . NEXT := INDETANKER ;
               INDETANKER := NEUINDETZUST ;
               INDETANKER -> . ELEMENTE := SETANKER ;
               INDETANKER -> . ZUSTAND := MAXINDETZUST ;
               BAUESET ( SETANKER , MAXZUSTAND )
             end (* then *)
         end (* SUCHEZUSTAND *) ;


      function ENDZUST ( ENDANKER : ZUSTVERW ; ZUSTAND : INTEGER ; var
                       WORKTERMNR : INTEGER ) : BOOLEAN ;

      (*******************************************)
      (*                                         *)
      (*   FUNKTION ENDZUST:                     *)
      (*                                         *)
      (*   SIE LIEFERT DEN WERT TRUE,            *)
      (*   WENN DER WERT DES FORMALEN            *)
      (*   PARAMETERS 'ZUSTAND' IN DER           *)
      (*   LISTE DER ENDZUSTAENDE ENT-           *)
      (*   HALTEN IST.                           *)
      (*                                         *)
      (*******************************************)


         var X : BOOLEAN ;

         begin (* ENDZUST *)
           WORKTERMNR := 0 ;
           while ZUSTAND > ENDANKER -> . WERT do
             ENDANKER := ENDANKER -> . LINK ;
           X := ( ZUSTAND = ENDANKER -> . WERT ) ;
           if X then
             WORKTERMNR := ENDANKER -> . TERMNR ;
           ENDZUST := X ;
         end (* ENDZUST *) ;


      procedure ANHAENGE ( var SUCHZUSTAND : PRODVER ; ZUSTNUMMER ,
                         NEUNUMMER : INTEGER ; var ANFANG : PRODVER ;
                         ENDANKER : ZUSTVERW ) ;

      (*****************************************************)
      (*                                                   *)
      (*   PROZEDUR ANHAENGE :                             *)
      (*                                                   *)
      (*   DIESE PROZEDUR DIENT ZUM ANHAENGEN              *)
      (*   DER NEU BENOETIGTEN PRODUKTIONEN AN             *)
      (*   DIE PRODUKTIONENKETTE.                          *)
      (*   DABEI WIRD PRO AUFRUF JEWEILS EIN               *)
      (*   FOLGEZUSTAND DES INDETERMINISMUS                *)
      (*   BEHANDELT.                                      *)
      (*                                                   *)
      (*   BEDEUTUNG DER FORMALEN PARAMETER                *)
      (*   UND DER LOKALEN VARIABLEN :                     *)
      (*                                                   *)
      (*   - VAR SUCHZUSTAND : GLOBALER ANKER,             *)
      (*                       DER DIE PRODUKTIO-          *)
      (*                       NENKETTE DURCHWAN-          *)
      (*                       DERT BIS DIE ENT-           *)
      (*                       SPRECHENDE ZU-              *)
      (*                       STANDSNUMMER IN             *)
      (*                       DER KOMPONENTE              *)
      (*                       'LINKS' GEFUNDEN WIRD.      *)
      (*                       DA DIE PRODUKTIO-           *)
      (*                       NEN GEORDNET SIND,          *)
      (*                       KANN BEIM NAECHS-           *)
      (*                       TEN AUFRUF DER              *)
      (*                       PROZEDUR MIT DEM            *)
      (*                       AKTUELLEN WERT DER          *)
      (*                       VARIABLEN 'SUCHZU-          *)
      (*                       STAND' WEITER GE-           *)
      (*                       ARBEITET WERDEN.            *)
      (*   -     ZUSTNUMMER  : AUSGANGSZUSTAND             *)
      (*                       ( LINKE SEITE DER           *)
      (*                       PRODUKTION )                *)
      (*                       DER ERSETZT WERDEN          *)
      (*                       MUSS.                       *)
      (*   -     NEUNUMMER   : ZUSTANDSNUMMER,             *)
      (*                       DIE DEN ALTEN AUS-          *)
      (*                       GANGSZUSTAND                *)
      (*                       ( FORMALER PARAMETER)       *)
      (*                       ERSETZT.                    *)
      (*   - VAR ANFANG      : ZEIGT AUF DAS ELE-          *)
      (*                       MENT DER PRODUK-            *)
      (*                       TIONENKETTE, AB             *)
      (*                       DEM ( EINSCHLIES-           *)
      (*                       LICH ) DIE NEUEN            *)
      (*                       PRODUKTIONEN EIN-           *)
      (*                       SORTIERT WERDEN             *)
      (*                       MUESSEN.                    *)
      (*                       DIES GESCHIEHT IN           *)
      (*                       DER UEBLICHEN LEXI-         *)
      (*                       KOGRAPHISCHEN AN-           *)
      (*                       ORDNUNG LINKS /             *)
      (*                       TERMINAL / RECHTS.          *)
      (*   -     SUCHSTELLE  : LOKALE VARIABLE ZUM         *)
      (*                       AUFFINDEN DER PRO-          *)
      (*                       DUKTION IN DER KET-         *)
      (*                       TE, VOR DER EINGE-          *)
      (*                       FUEGT WERDEN MUSS.          *)
      (*   -     NEUPROD     : LOKALER HILFS-              *)
      (*                       POINTER ZUM DY-             *)
      (*                       NAMISCHEN ERZEUGEN          *)
      (*                       VON PRODUKTIONS-            *)
      (*                       RECORDS.                    *)
      (*                                                   *)
      (*****************************************************)


         var SUCHSTELLE , NEUPROD : PRODVER ;
             WORKTERMNR : INTEGER ;


         function GROESSER ( VERWEIS1 , VERWEIS2 : PRODVER ) : BOOLEAN
                           ;

         (**************************************)
         (*                                    *)
         (*   FUNKTION GROESSER :              *)
         (*                                    *)
         (*   DIESE FUNKTION HAT               *)
         (*   DEN BOOLESCHEN WERT              *)
         (*   'TRUE', WENN DIE GROES-          *)
         (*   SER RELATION FUER DIE            *)
         (*   FORMALEN PARAMETER               *)
         (*   'VERWEIS1' UND 'VER-             *)
         (*   WEIS2' ZUTRIFFT.                 *)
         (*   ORDNUNGSPRIORITAET :             *)
         (*   ( ABSTEIGEND )                   *)
         (*   LINKS / TERMINAL /               *)
         (*   RECHTS.                          *)
         (*   DIE KOMPONENTE 'LINKS'           *)
         (*   IST MIT AUSNAHME DER             *)
         (*   BREMSE IMMER GLEICH.             *)
         (*                                    *)
         (**************************************)


            begin (* GROESSER *)
              if VERWEIS2 -> . RECHTS = MAXINT then
                GROESSER := FALSE
              else
                GROESSER := ( VERWEIS1 -> . TERMINAL > VERWEIS2 -> .
                            TERMINAL ) or ( ( VERWEIS1 -> . TERMINAL =
                            VERWEIS2 -> . TERMINAL ) and ( VERWEIS1 ->
                            . RECHTS > VERWEIS2 -> . RECHTS ) )
            end (* GROESSER *) ;


         function EQUAL ( VERWEIS1 , VERWEIS2 : PRODVER ) : BOOLEAN ;

         (**************************************)
         (*                                    *)
         (*   FUNKTION EQUAL :                 *)
         (*                                    *)
         (*   DIESE FUNKTION LIE-              *)
         (*   FERT DEN WERT 'TRUE',            *)
         (*   WENN DIE BEIDEN FORMALEN         *)
         (*   PARAMETER 'VERWEIS1' UND         *)
         (*   'VERWEIS2' IN DEN KOMPO-         *)
         (*   NENTEN 'TERMINAL' UND            *)
         (*   'RECHTS' UEBEREIN-               *)
         (*   STIMMEN.                         *)
         (*                                    *)
         (**************************************)


            begin (* EQUAL *)
              EQUAL := ( VERWEIS1 -> . TERMINAL = VERWEIS2 -> .
                       TERMINAL ) and ( VERWEIS1 -> . RECHTS = VERWEIS2
                       -> . RECHTS )
            end (* EQUAL *) ;

         begin (* ANHAENGE *)
           while ZUSTNUMMER > SUCHZUSTAND -> . LINKS do
             SUCHZUSTAND := SUCHZUSTAND -> . NEXT ;
           SUCHSTELLE := ANFANG ;
           while ZUSTNUMMER = SUCHZUSTAND -> . LINKS do
             begin
               NEW1 ( NEUPROD ) ;
               while GROESSER ( SUCHZUSTAND , SUCHSTELLE ) do
                 SUCHSTELLE := SUCHSTELLE -> . NEXT ;
               if not EQUAL ( SUCHZUSTAND , SUCHSTELLE ) then
                 begin
                   NEUPROD -> . LINKS := SUCHSTELLE -> . LINKS ;
                   NEUPROD -> . RECHTS := SUCHSTELLE -> . RECHTS ;
                   NEUPROD -> . TERMINAL := SUCHSTELLE -> . TERMINAL ;
                   NEUPROD -> . NEXT := SUCHSTELLE -> . NEXT ;
                   NEUPROD -> . TERMNR := SUCHSTELLE -> . TERMNR ;
                   SUCHSTELLE -> . LINKS := NEUNUMMER ;
                   SUCHSTELLE -> . NEXT := NEUPROD ;
                   SUCHSTELLE -> . RECHTS := SUCHZUSTAND -> . RECHTS ;
                   SUCHSTELLE -> . TERMINAL := SUCHZUSTAND -> .
                                               TERMINAL ;
                   SUCHSTELLE -> . TERMNR := 0 ;
                   if ENDZUST ( ENDANKER , SUCHSTELLE -> . RECHTS ,
                   WORKTERMNR ) then
                     SUCHSTELLE -> . TERMNR := WORKTERMNR ;
                 end (* then *)
               else
                 DISPOSE1 ( NEUPROD ) ;
               SUCHZUSTAND := SUCHZUSTAND -> . NEXT
             end (* while *)
         end (* ANHAENGE *) ;

      begin (* NICHTDET *)
        repeat

        (*****************************************************)
        (*                                                   *)
        (*   RUMPF DER PROZEDUR 'NICHTDET' .                 *)
        (*                                                   *)
        (*   IN DER REPEATSCHLEIFE WIRD DIE                  *)
        (*   LAENGE DES INDETERMINISMUS FESTGE-              *)
        (*   STELLT. DANN WIRD DIE NEUE ZUSTANDS-            *)
        (*   KOMBINATION AUFGEBAUT UND FESTGE-               *)
        (*   STELLT, OB SIE SCHON VORHANDEN IST.             *)
        (*                                                   *)
        (*****************************************************)

          RETTE := ENDE ;
          ENDE := ENDE -> . NEXT
        until not EQUAL2 ( ANFANG , ENDE ) ;
        FUELLESET ( SETANKER , ANFANG , RETTE , INDETANKER , MAXZUSTAND
                    ) ;
        SUCHEZUSTAND ( INDETANKER , SETANKER , ALT , MAXINDETZUST ,
                       MAXZUSTAND , NUMMER ) ;
        RETTEANFANG := ANFANG ;
        if not ALT then

        (**************************************)
        (*                                    *)
        (*   DIE ZUSTANDSKOMBINATION          *)
        (*   IST NOCH NICHT VOR-              *)
        (*   HANDEN.                          *)
        (*                                    *)
        (*   ALLE PRODUKTIONEN, DIE           *)
        (*   EINEN ZUSTAND AUS DIE-           *)
        (*   SER KOMBINATION ALS              *)
        (*   AUSGANGSZUSTAND ( KOM-           *)
        (*   PONENTE 'LINKS' ) BE-            *)
        (*   SITZEN, MUESSEN AN DIE           *)
        (*   PRODUKTIONSKETTE ANGE-           *)
        (*   FUEGT WERDEN, WOBEI DIE          *)
        (*   LINKE KOMPONENTE DURCH           *)
        (*   DEN NEUEN DIESE KOM-             *)
        (*   BINATION REPRAESENTIER-          *)
        (*   ENDEN ZUSTAND ERSETZT            *)
        (*   WIRD.                            *)
        (*                                    *)
        (*   BEISPIEL :                       *)
        (*                                    *)
        (*   SEIEN FOLGENDE PRO-              *)
        (*   DUKTIONEN TEIL DER PRO-          *)
        (*   DUKTIONENKETTE :                 *)
        (*                                    *)
        (*     3  -> 'B'  4                   *)
        (*     3  -> 'B'  5                   *)
        (*     4  -> 'A'  6                   *)
        (*     4  -> 'B'  6                   *)
        (*     5  -> 'A'  7                   *)
        (*     5  -> 'B'  6                   *)
        (*     6  -> ..                       *)
        (*                                    *)
        (*   DANN MUESSEN, WENN DIE           *)
        (*   KOMBINATION 4,5 NOCH             *)
        (*   NICHT VORLIEGT, AM EN-           *)
        (*   DE DER PRODUKTIONEN-             *)
        (*   KETTE FOLGENDE PRODUK-           *)
        (*   TIONEN ANGEHAENGT WER-           *)
        (*   DEN :                            *)
        (*                                    *)
        (*    23  -> 'A'  6                   *)
        (*    23  -> 'A'  7                   *)
        (*    23  -> 'B'  6                   *)
        (*                                    *)
        (*   ( DER REPRAESENTIERENDE          *)
        (*   ZUSTAND SEI 23. )                *)
        (*                                    *)
        (*   DABEI DARF NATUERLICH            *)
        (*   JEDE PRODUKTION NUR              *)
        (*   EINMAL ANGEHAENGT WER-           *)
        (*   DEN. ALSO NICHT                  *)
        (*    23  -> 'B'  6                   *)
        (*   ZWEIMAL ANHAENGEN ( AUS          *)
        (*   4 UND AUS 5 ), SONDERN           *)
        (*   NUR EINMAL.                      *)
        (*   DABEI WIRD AUCH UEBER-           *)
        (*   PRUEFT, OB DER NEUE ZU-          *)
        (*   STAND EIN ENDZUSTAND             *)
        (*   SEIN MUSS UND BEI BE-            *)
        (*   DARF WIRD DANN DER ZU-           *)
        (*   STAND IN DIE KETTE DER           *)
        (*   ENDZUSTAENDE EINSOR-             *)
        (*   TIERT.                           *)
        (*                                    *)
        (*   DER NEU AUFTRETENDE IN-          *)
        (*   DETERMINISMUS WIRD DANN          *)
        (*   SPAETER BESEITIGT, WENN          *)
        (*   DIE VARIABLE 'EINS' DER          *)
        (*   PROZEDUR 'DETAUTOMAT'            *)
        (*   DIESE STELLE IN DER              *)
        (*   KETTE ERREICHT.                  *)
        (*                                    *)
        (*   DIE NEUE ZUSTANDSNUMMER          *)
        (*   MUSS AUCH IN DIE RECHTE          *)
        (*   SEITE DER ERSTEN PRODUK-         *)
        (*   TION DES INDETERMINIMUS          *)
        (*   GESPEICHERT WERDEN, WEIL         *)
        (*   DIESE PRODUKTION DEN             *)
        (*   GANZEN INDETERMINISMUS           *)
        (*   ERSETZEN SOLL.                   *)
        (*                                    *)
        (**************************************)

          begin
            ENDEZUST := FALSE ;
            TERMNR := 0 ;
            SUCHZUSTAND := PRODANKER ;
            while ANFANG <> ENDE do
              begin
                ENDEZUST := ENDEZUST or ENDZUST ( ENDANKER , ANFANG ->
                            . RECHTS , WORKTERMNR ) ;
                if ( WORKTERMNR <> 0 ) and ( TERMNR = 0 ) then
                  TERMNR := WORKTERMNR ;
                ANHAENGE ( SUCHZUSTAND , ANFANG -> . RECHTS ,
                           MAXINDETZUST , ALTENDE , ENDANKER ) ;
                ANFANG := ANFANG -> . NEXT
              end (* while *) ;
            if ENDEZUST then
              begin
                NEW2 ( SCHLUSSANKER -> . LINK ) ;
                SCHLUSSANKER -> . WERT := MAXINDETZUST ;
                SCHLUSSANKER -> . TERMNR := TERMNR ;
                SCHLUSSANKER := SCHLUSSANKER -> . LINK ;
                SCHLUSSANKER -> . WERT := MAXINT
              end (* then *) ;
            while ALTENDE -> . RECHTS <> MAXINT do
              ALTENDE := ALTENDE -> . NEXT ;
            RETTEANFANG -> . RECHTS := MAXINDETZUST ;
            RETTEANFANG -> . TERMNR := TERMNR ;
            MAXINDETZUST := MAXINDETZUST + 1
          end (* then *)
        else

        (**************************************)
        (*                                    *)
        (*   ZUSTANDSKOMBINATION              *)
        (*   SCHON VORHANDEN :                *)
        (*                                    *)
        (*   ES MUSS DIE NUMMER DES           *)
        (*   ALTEN REPRAESENTANTEN IN         *)
        (*   DIE RECHTE SEITE DER             *)
        (*   ERSTEN PRODUKTION DES            *)
        (*   INDETERMINISMUS ABGE-            *)
        (*   SPEICHERT WERDEN.                *)
        (*                                    *)
        (**************************************)

          RETTEANFANG -> . RECHTS := NUMMER ;

        (**************************************)
        (*                                    *)
        (*   UNABHAENGIG DAVON, OB DIE        *)
        (*   ZUSTANDSKOMBINATION SCHON        *)
        (*   VORHANDEN WAR, WERDEN DIE        *)
        (*   INDETERMINISTISCHEN PRO-         *)
        (*   DUKTIONEN JETZT MIT AUS-         *)
        (*   NAHME DER MODIFIZIERTEN          *)
        (*   ERSTEN, AUS DER PRODUK-          *)
        (*   TIONENKETTE ENTFERNT UND         *)
        (*   DER EIGENEN FREISPEICHER-        *)
        (*   VERWALTUNG ZUR VERFUEGUNG        *)
        (*   GESTELLT.                        *)
        (*                                    *)
        (**************************************)

        AUSHAENGE ( RETTEANFANG , RETTE , ENDE )
      end (* NICHTDET *) ;


   procedure NESTROY ( var ANKER : PRODVER ) ;

   (*****************************************************************)
   (*                                                               *)
   (*   PROZEDUR NESTROY:                                           *)
   (*                                                               *)
   (*   DIESE PROZEDUR WIRD ALS ERSTE VON 'DETAUTOMAT' AUFGERUFEN   *)
   (*   UND DIENT ZUR AUFLOESUNG DER VON-BIS-ELEMENTE, DIE AM ANFANG*)
   (*   DER PRODUKTIONENKETTE STEHEN, IN NORMALE ELEMENTARPRODUKTIO-*)
   (*   NEN, DIE DANN AN DER RICHTIGEN STELLE EINSORTIERT WERDEN. DE*)
   (*   FORMALE PARAMETER IST DER ANKER DER PRODUKTIONENKETTE.      *)
   (*                                                               *)
   (*   BEDEUTUNG DER LOKALEN VARIABLEN :                           *)
   (*                                                               *)
   (*   - ENDESOKO       : ZEIGT AUF DAS ERSTE ELEMENT DER          *)
   (*                      PRODUKTIONENKETTE, DAS DEN SONDER-       *)
   (*                      KONSTRUKTIONEN ( VON -BIS-ELE-           *)
   (*                      MENTE ) FOLGT.                           *)
   (*   - DURCHLAUF      : HILFSPOINTER ZUM DURCHLAUFEN DER         *)
   (*                      SONDERKONSTRUKTIONEN.                    *)
   (*   - ENDESTRUK      : ZEIGT AUF DIE LETZTE PRODUKTION          *)
   (*                      DER AKTUELL BEHANDELTEN SONDER-          *)
   (*                      KONSTRUKTION.                            *)
   (*   - EINFUEGBEREICH : HILFSPOINTER ZUM AUFFINDEN DER           *)
   (*                      EINFUEGSTELLE FUER DIE ELEMENTAR-        *)
   (*                      PRODUKTIONEN.                            *)
   (*   - ZAEHL          : HILFSVARIABLE ZUM ZAEHLEN BEI DI-        *)
   (*                      VERSEN FOR-SCHLEIFEN.                    *)
   (*                                                               *)
   (*****************************************************************)


      var ENDESOKO , DURCHLAUF , ENDESTRUK , EINFUEGBEREICH : PRODVER ;
          ZAEHL : INTEGER ;


      procedure SCHNELL ;

      (*****************************************************)
      (*                                                   *)
      (*   EINFUEGEN DER ELEMENTAREN SONDER-               *)
      (*   KONSTRUKTIONEN IN EINEM ZUG, DA                 *)
      (*   AUSGANGSZUSTAND ( KOMPONENTE 'LINKS' )          *)
      (*   NOCH KEINER ELEMENTARPRODUKTION ALS             *)
      (*   LINKE SEITE ANGEHOERT.                          *)
      (*                                                   *)
      (*****************************************************)


         var ZEICHEN : CHAR ;
             NEU , ALT : PRODVER ;

         begin (* SCHNELL *)
           NEW1 ( ALT ) ;

           (**************************************)
           (*                                    *)
           (*   ZWISCHENSPEICHERUNG DER          *)
           (*   DEN NEUEN ELEMENTARPRO-          *)
           (*   DUKTIONEN FOLGENDEN              *)
           (*   PRODUKTION                       *)
           (*                                    *)
           (**************************************)

           ALT -> . LINKS := EINFUEGBEREICH -> . LINKS ;
           ALT -> . RECHTS := EINFUEGBEREICH -> . RECHTS ;
           ALT -> . TERMINAL := EINFUEGBEREICH -> . TERMINAL ;
           ALT -> . TERMNR := EINFUEGBEREICH -> . TERMNR ;
           ALT -> . NEXT := EINFUEGBEREICH -> . NEXT ;

           (**************************************)
           (*                                    *)
           (*   AUFBAUEN UND ANHAENGEN           *)
           (*   DER KETTE VON NEUEN ELE-         *)
           (*   MENTARPRODUKTIONEN.              *)
           (*                                    *)
           (**************************************)

           NEU := EINFUEGBEREICH ;
           NEU -> . LINKS := DURCHLAUF -> . LINKS ;
           NEU -> . RECHTS := DURCHLAUF -> . RECHTS ;
           NEU -> . TERMINAL := DURCHLAUF -> . TERMINAL ;
           NEU -> . TERMNR := DURCHLAUF -> . TERMNR ;
           ZEICHEN := DURCHLAUF -> . TERMINAL ;
           while ZEICHEN <> ENDESTRUK -> . TERMINAL do
             begin
               ZEICHEN := SUCC ( ZEICHEN ) ;
               NEW1 ( NEU -> . NEXT ) ;
               NEU := NEU -> . NEXT ;
               NEU -> . TERMINAL := ZEICHEN ;
               NEU -> . LINKS := DURCHLAUF -> . LINKS ;
               NEU -> . RECHTS := DURCHLAUF -> . RECHTS ;
               NEU -> . TERMNR := DURCHLAUF -> . TERMNR ;
             end (* while *) ;

           (**************************************)
           (*                                    *)
           (*   ANHAENGEN DER ZWISCHEN-          *)
           (*   GESPEICHERTEN RESTKETTE.         *)
           (*                                    *)
           (**************************************)

           NEU -> . NEXT := ALT
         end (* SCHNELL *) ;


      procedure LANGSAM ;

      (*****************************************************)
      (*                                                   *)
      (*   EINFUEGEN DER ELEMENTAREN SONDERKON-            *)
      (*   STRUKTIONSPRODUKTIONEN NACH UND NACH,           *)
      (*   DA AUSGANGSZUSTAND SCHON ELEMENTAR-             *)
      (*   PRODUKTIONEN ALS LINKE SEITE ANGE-              *)
      (*   HOERT.                                          *)
      (*                                                   *)
      (*****************************************************)


         var SUCHZEICHEN : CHAR ;
             NEU : PRODVER ;


         procedure SUCHE ;

         (**************************************)
         (*                                    *)
         (*   DIESE PROZEDUR SUCHT             *)
         (*   DIE PRODUKTION, VOR              *)
         (*   DER EINGEFUEGT WER-              *)
         (*   DEN MUSS.                        *)
         (*   ( VAR. EINFUEGBEREICH )          *)
         (*                                    *)
         (**************************************)


            begin (* SUCHE *)
              while ( DURCHLAUF -> . LINKS = EINFUEGBEREICH -> . LINKS
              ) and ( ( EINFUEGBEREICH -> . TERMINAL < SUCHZEICHEN ) or
              ( ( SUCHZEICHEN = EINFUEGBEREICH -> . TERMINAL ) and (
              EINFUEGBEREICH -> . RECHTS < DURCHLAUF -> . RECHTS ) ) )
              do
                EINFUEGBEREICH := EINFUEGBEREICH -> . NEXT
            end (* SUCHE *) ;


         function EQUAL : BOOLEAN ;

         (**************************************)
         (*                                    *)
         (*   DIESE FUNKTION WIRD              *)
         (*   'TRUE', WENN DIE PRO-            *)
         (*   DUKTION NACH DER EIN-            *)
         (*   FUEGSTELLE DIE GLEICHE           *)
         (*   IST, WIE DIE EINZU-              *)
         (*   FUEGENDE.                        *)
         (*                                    *)
         (**************************************)


            begin (* EQUAL *)
              EQUAL := ( EINFUEGBEREICH -> . LINKS = DURCHLAUF -> .
                       LINKS ) and ( EINFUEGBEREICH -> . RECHTS =
                       DURCHLAUF -> . RECHTS ) and ( SUCHZEICHEN =
                       EINFUEGBEREICH -> . TERMINAL )
            end (* EQUAL *) ;

         begin (* LANGSAM *)
           SUCHZEICHEN := DURCHLAUF -> . TERMINAL ;

           (*********************************)
           (*                               *)
           (*   ERSTES ZEICHEN              *)
           (*                               *)
           (*********************************)

           SUCHE ;
           if not EQUAL then

           (**************************************)
           (*                                    *)
           (*   PRODUKTION NOCH NICHT            *)
           (*   VORHANDEN. DAS EINFUEGEN         *)
           (*   ERFOLGT MITTELS DES              *)
           (*   TRICKS VON WIRTH.                *)
           (*                                    *)
           (**************************************)

             begin
               NEW1 ( NEU ) ;
               NEU -> . LINKS := EINFUEGBEREICH -> . LINKS ;
               NEU -> . RECHTS := EINFUEGBEREICH -> . RECHTS ;
               NEU -> . NEXT := EINFUEGBEREICH -> . NEXT ;
               NEU -> . TERMINAL := EINFUEGBEREICH -> . TERMINAL ;
               NEU -> . TERMNR := EINFUEGBEREICH -> . TERMNR ;
               EINFUEGBEREICH -> . NEXT := NEU ;
               EINFUEGBEREICH -> . TERMINAL := SUCHZEICHEN ;
               EINFUEGBEREICH -> . LINKS := DURCHLAUF -> . LINKS ;
               EINFUEGBEREICH -> . RECHTS := DURCHLAUF -> . RECHTS ;
               EINFUEGBEREICH -> . TERMNR := DURCHLAUF -> . TERMNR ;
             end (* then *) ;

           (**************************************)
           (*                                    *)
           (*   BEARBEITUNG DER REST-            *)
           (*   LICHEN ZEICHEN.                  *)
           (*   PRO DURCHLAUF DER                *)
           (*   WHILE-SCHLEIFE WIRD              *)
           (*   EIN TERMINALES ZEI-              *)
           (*   CHEN BEARBEITET.                 *)
           (*                                    *)
           (**************************************)

           while SUCHZEICHEN <> ENDESTRUK -> . TERMINAL do
             begin
               SUCHZEICHEN := SUCC ( SUCHZEICHEN ) ;
               SUCHE ;
               if not EQUAL then
                 begin
                   NEW1 ( NEU ) ;
                   NEU -> . LINKS := EINFUEGBEREICH -> . LINKS ;
                   NEU -> . RECHTS := EINFUEGBEREICH -> . RECHTS ;
                   NEU -> . NEXT := EINFUEGBEREICH -> . NEXT ;
                   NEU -> . TERMINAL := EINFUEGBEREICH -> . TERMINAL ;
                   NEU -> . TERMNR := EINFUEGBEREICH -> . TERMNR ;
                   EINFUEGBEREICH -> . NEXT := NEU ;
                   EINFUEGBEREICH -> . TERMINAL := SUCHZEICHEN ;
                   EINFUEGBEREICH -> . LINKS := DURCHLAUF -> . LINKS ;
                   EINFUEGBEREICH -> . RECHTS := DURCHLAUF -> . RECHTS
                                                 ;
                   EINFUEGBEREICH -> . TERMNR := DURCHLAUF -> . TERMNR
                 end (* then *)
             end (* while *)
         end (* LANGSAM *) ;

      begin (* NESTROY *)
        ENDESOKO := ANKER ;

        (*****************************************************)
        (*                                                   *)
        (*   RUMPF DER PROZESUR NESTROY .                    *)
        (*                                                   *)
        (*   DIE SONDERKONSTRUKTIONEN WERDEN                 *)
        (*   SEQUENTIELL ABGEARBEITET. PRO                   *)
        (*   DURCHLAUF DURCH DIE ZWEITE WHILE-               *)
        (*   SCHLEIFE WIRD EINE SONDERKONSTRUK-              *)
        (*   TION ABGEHANDELT.                               *)
        (*                                                   *)
        (*****************************************************)

        while ENDESOKO -> . LINKS = - 1 do
          for ZAEHL := 1 to ENDESOKO -> . RECHTS + 2 do
            ENDESOKO := ENDESOKO -> . NEXT ;
        DURCHLAUF := ANKER ;
        while DURCHLAUF <> ENDESOKO do
          begin
            ENDESTRUK := DURCHLAUF ;
            for ZAEHL := 1 to DURCHLAUF -> . RECHTS + 1 do
              ENDESTRUK := ENDESTRUK -> . NEXT ;
            for ZAEHL := 1 to DURCHLAUF -> . RECHTS do
              begin
                DURCHLAUF := DURCHLAUF -> . NEXT ;
                EINFUEGBEREICH := ENDESOKO ;
                while EINFUEGBEREICH -> . LINKS < DURCHLAUF -> . LINKS
                do
                  EINFUEGBEREICH := EINFUEGBEREICH -> . NEXT ;
                if EINFUEGBEREICH -> . LINKS = DURCHLAUF -> . LINKS
                then

        (**************************************)
        (*                                    *)
        (*   AUSGANGSZUSTAND SCHON            *)
        (*   VORHANDEN.                       *)
        (*                                    *)
        (**************************************)

                  LANGSAM
                else

        (**************************************)
        (*                                    *)
        (*   AUSGANGSZUSTAND NICHT            *)
        (*   VORHANDEN                        *)
        (*                                    *)
        (**************************************)

                  SCHNELL
              end (* for *) ;

        (**************************************)
        (*                                    *)
        (*   FREIGABE DER SONDERKON-          *)
        (*   STRUKTIONSPRODUKTIONEN           *)
        (*   FUER DIE EIGENE FREI-            *)
        (*   SPEICHERVERWALTUNG.              *)
        (*                                    *)
        (**************************************)

            for ZAEHL := 1 to 2 do
              DURCHLAUF := DURCHLAUF -> . NEXT ;
            ENDESTRUK -> . NEXT := NIL ;
            DISPOSE1 ( ANKER ) ;
            ANKER := DURCHLAUF ;
          end (* while *)
      end (* NESTROY *) ;


   procedure REINIGE ( var ANKER : PRODVER ; MAXZUSTAND : INTEGER ; var
                     ENDANKER : ZUSTVERW ) ;

   (*****************************************************************)
   (*                                                               *)
   (*   PROZEDUR REINIGE :                                          *)
   (*                                                               *)
   (*   IN DIESER PROZEDUR WERDEN PRODUKTIONEN, DIE NICHT           *)
   (*   BENOETIGT WERDEN, GELOESCHT.                                *)
   (*                                                               *)
   (*   DAS SIND ALLE PRODUKTIONEN, DEREN LINKE SEITE NIR-          *)
   (*   GENDS RECHTS STEHT MIT AUSNAHME DERJENIGEN MIT DEM          *)
   (*   STARTZUSTAND 1 .                                            *)
   (*                                                               *)
   (*   BEDEUTUNG DER FORMALEN PARAMETER UND LOKALEN                *)
   (*   VARIABLEN :                                                 *)
   (*                                                               *)
   (*   - VAR ANKER           : GLOBALER ANKER DER PRODUK-          *)
   (*                           TIONENKETTE                         *)
   (*   -     MAXZUSTAND      : DIE NAECHSTE ZU VERGEBENDE          *)
   (*                           ZUSTANDSNUMMER.                     *)
   (*   - VAR ENDANKER        : GLOBALER ANKER DER ENDZUSTAENDE     *)
   (*   -     SETANKER        : ANKER FUER DIE KETTE DER AKTUELLEN  *)
   (*                           RECHTEN SEITEN                      *)
   (*   -     HILF            : HILFSVARIABLE ZUR ZWISCHEN-         *)
   (*                           SPEICHERUNG UND DURCHWANDERUNG      *)
   (*                           VON SETKETTEN.                      *)
   (*   -     ALTSET          : ANKER FUER DIE KETTE DER            *)
   (*                           ALTEN RECHTEN SEITEN                *)
   (*   -     SETLAUF         : HILFSANKER ZUM DURCHWANDERN         *)
   (*                           VON SETKETTEN.                      *)
   (*   -   LIGRENZE,REGRENZE : HILFSPOINTER ZUM MARKIEREN          *)
   (*                           DER AUSZUHAENGENDEN                 *)
   (*                           PRODUKTIONEN.                       *)
   (*   -     PRODLAUF        : LAUFVARIABLE FUER DIE KETTE         *)
   (*                           DER PRODUKTIONEN                    *)
   (*   -   ALT,ZAEHL,ZUSTAND : HILFSVARIABLEN ZUM LOESCHEN NICHT   *)
   (*                           MEHR BENOETIGTER ENDZUSTAENDE.      *)
   (*   -     LOESCHE         : BOOLESCHE VARIABLE, DIE ANZEIGT     *)
   (*                           OB EIN ZUSTAND GELOESCHT WERDEN KANN*)
   (*   -     ENDLAUF         : HILFSVARIABLE ZUM DURCHLAUFEN       *)
   (*                           DER ENDZUSTANDSKETTE                *)
   (*                                                               *)
   (*   VORGEHENSWEISE :                                            *)
   (*                                                               *)
   (*   DIE FOLGEZUSTAENDE ALLER PRODUKTIONEN WERDEN IN EINE        *)
   (*   SETLISTE ( GENAUSO WIE BEI DEN ERSATZZUSTAENDEN )           *)
   (*   EINGETRAGEN. DANN WIRD FUER JEDE LINKE SEITE ( AUSSER DEM   *)
   (*   STARTZUSTAND 1 ) UEBERPRUEFT, OB SIE IN DER LISTE ENTHALTEN *)
   (*   IST. WENN NICHT, WERDEN DIESE PRODUKTIONEN GELOESCHT.       *)
   (*   HAT MAN SO ALLE PRODUKTIONEN DURCHLAUFEN, WIRD DIE LISTE    *)
   (*   DER AKTUELLEN PRODUKTIONEN MIT DER LISTE DES VORHERIGEN     *)
   (*   DURCHLAUFS VERGLICHEN. SIND DIE LISTEN GLEICH, WIRD DIE     *)
   (*   SCHLEIFE BEENDET. IM ANDEREN FALL BEGINNT DER PROZESS AUFS  *)
   (*   NEUE. DIES GESCHIEHT SO LANGE, BIS DIE BEIDEN LISTEN        *)
   (*   GLEICH SIND.                                                *)
   (*   DIESES VERFAHREN IST NICHT VOLLKOMMEN, SO BLEIBEN           *)
   (*   PRODUKTIONEN DER FORM  5  ->  'A'  5  ERHALTEN, AUCH WENN   *)
   (*   DER ZUSTAND 5 NIE ERREICHT WERDEN KANN.                     *)
   (*                                                               *)
   (*   AM ENDE WIRD DANN AUCH DIE LISTE DER ENDZUSTAENDE GESAEUBERT*)
   (*   INDEM ALLE ZUSTAENDE, DIE AUSSER DEM STARTZUSTAND,          *)
   (*   NIRGENDS RECHTS STEHEN, GESTRICHEN WERDEN.                  *)
   (*                                                               *)
   (*****************************************************************)


      var SETANKER , HILF , ALTSET , SETLAUF : BLOCKVERWEIS ;
          LIGRENZE , REGRENZE , PRODLAUF : PRODVER ;
          ALT , ZAEHL , ZUSTAND : INTEGER ;
          LOESCHE : BOOLEAN ;
          ENDLAUF : ZUSTVERW ;

      begin (* REINIGE *)
        BAUESET ( SETANKER , MAXZUSTAND - 1 ) ;
        BAUESET ( ALTSET , MAXZUSTAND - 1 ) ;
        HILF := ALTSET ;
        while HILF <> NIL do
          begin

        (**************************************)
        (*                                    *)
        (*   BILDEN EINER LEEREN VER-         *)
        (*   GLEICHSSETLISTE.                 *)
        (*                                    *)
        (**************************************)

            HILF -> . SATZ := [ ] ;
            HILF := HILF -> . NEXT
          end (* while *) ;
        repeat
          SETLAUF := SETANKER ;
          while SETLAUF <> NIL do
            begin
              SETLAUF -> . SATZ := [ ] ;
              SETLAUF := SETLAUF -> . NEXT
            end (* while *) ;
          PRODLAUF := ANKER ;
          while PRODLAUF -> . LINKS <> MAXINT do

        (**************************************)
        (*                                    *)
        (*   FUELLEN DER SETLISTE             *)
        (*   MIT DEN RECHTEN SEITEN.          *)
        (*                                    *)
        (**************************************)

            begin
              ZUSTAND := PRODLAUF -> . RECHTS ;
              SETLAUF := SETANKER ;
              for ZAEHL := 1 to ( ZUSTAND - 1 ) DIV SETSIZE do
                SETLAUF := SETLAUF -> . NEXT ;
              SETLAUF -> . SATZ := SETLAUF -> . SATZ + [ ZUSTAND MOD
                                   SETSIZE ] ;
              PRODLAUF := PRODLAUF -> . NEXT
            end (* while *) ;
          PRODLAUF := ANKER ;
          while PRODLAUF -> . LINKS = 1 do

        (**************************************)
        (*                                    *)
        (*   STARTZUSTAND UEBERSPRINGEN       *)
        (*                                    *)
        (**************************************)

            PRODLAUF := PRODLAUF -> . NEXT ;
          while PRODLAUF -> . LINKS <> MAXINT do

        (**************************************)
        (*                                    *)
        (*   UEBERPRUEFEN OB LINKE            *)
        (*   SEITE AUCH RECHTS STEHT          *)
        (*   UND EVENTUELL LOESCHEN.          *)
        (*                                    *)
        (**************************************)

            begin
              SETLAUF := SETANKER ;
              for ZAEHL := 1 to ( PRODLAUF -> . LINKS - 1 ) DIV SETSIZE
              do
                SETLAUF := SETLAUF -> . NEXT ;
              LOESCHE := not ( PRODLAUF -> . LINKS MOD SETSIZE in
                         SETLAUF -> . SATZ ) ;
              LIGRENZE := PRODLAUF ;
              ZUSTAND := PRODLAUF -> . LINKS ;
              repeat
                PRODLAUF := PRODLAUF -> . NEXT ;
              until ZUSTAND <> PRODLAUF -> . LINKS ;
              if LOESCHE then
                begin
                  REGRENZE := PRODLAUF -> . NEXT ;
                  LIGRENZE -> . LINKS := PRODLAUF -> . LINKS ;
                  LIGRENZE -> . RECHTS := PRODLAUF -> . RECHTS ;
                  LIGRENZE -> . TERMINAL := PRODLAUF -> . TERMINAL ;
                  LIGRENZE -> . TERMNR := PRODLAUF -> . TERMNR ;
                  AUSHAENGE ( LIGRENZE , PRODLAUF , REGRENZE ) ;
                  PRODLAUF := LIGRENZE
                end (* then *)
            end (* while *) ;
          HILF := ALTSET ;

        (**************************************)
        (*                                    *)
        (*   TAUSCH VOR VERGLEICH             *)
        (*                                    *)
        (**************************************)

          ALTSET := SETANKER ;
          SETANKER := HILF ;
        until VERGLEICHESET ( ALTSET , SETANKER ) ;

        (**************************************)
        (*                                    *)
        (*   BEREINIGEN DER LISTE DER         *)
        (*   ENDZUSTAENDE                     *)
        (*                                    *)
        (**************************************)

        ENDLAUF := ENDANKER ;
        if ENDLAUF -> . WERT = 1 then
          ENDLAUF := ENDLAUF -> . LINK ;
        while ENDLAUF <> NIL do
          begin
            ALT := 0 ;
            SETLAUF := SETANKER ;
            for ZAEHL := 1 to ( ENDLAUF -> . WERT - 1 ) DIV SETSIZE -
            ALT do
              SETLAUF := SETLAUF -> . NEXT ;
            ALT := ( ENDLAUF -> . WERT - 1 ) DIV SETSIZE ;
            LOESCHE := not ( ENDLAUF -> . WERT MOD SETSIZE in SETLAUF
                       -> . SATZ ) ;
            if LOESCHE then
              begin
                ENDLAUF -> . WERT := ENDLAUF -> . LINK -> . WERT ;
                ENDLAUF -> . TERMNR := ENDLAUF -> . LINK -> . TERMNR ;
                ENDLAUF -> . LINK := ENDLAUF -> . LINK -> . LINK
              end (* then *)
            else
              ENDLAUF := ENDLAUF -> . LINK
          end (* while *)
      end (* REINIGE *) ;

   begin (* DETAUTOMAT *)
     NESTROY ( PRODANKER ) ;

     (**********************************************************)
     (*                                                        *)
     (*   RUMPF DER PROZEDUR 'DETAUTOMAT'                      *)
     (*                                                        *)
     (*   BEVOR DAS DURCHSUCHEN DER PRODUKTIONSKETTE AUF       *)
     (*   NICHTDETERMINISMEN BEGONNEN WERDEN KANN, SIND        *)
     (*   EINIGE VORARBEITEN ZU VERRICHTEN:                    *)
     (*                                                        *)
     (*   - AUFLOESEN DER 'VON-BIS-ELEMENTE' UND EIN-          *)
     (*     SORTIERUNG DERSELBEN IN DIE PRODUKTIONSKETTE       *)
     (*     (PROZEDUR NESTROY).                                *)
     (*   - 'SCHLUSS' AUF BREMSE SETZEN.                       *)
     (*   - MIT 'ENDESCHLUSS' BREMSE ERZEUGEN.                 *)
     (*   - INDETANKER INITIALISIEREN (BREMSE).                *)
     (*   - ERSTEN SET AUFBAUEN.                               *)
     (*                                                        *)
     (*   WIRD EIN NICHTDETERMINISTISCHER ZUSTAND ENTDECKT,    *)
     (*   SO WIRD DIE PROZEDUR 'NICHTDET' AUFGERUFEN.          *)
     (*   DIESE PROZEDUR UEBERNIMMT DIE VOLLSTAENDIGE          *)
     (*   BEHANDLUNG DIESES FALLS.                             *)
     (*                                                        *)
     (*   ABSCHLUSSBEHANDLUNG:                                 *)
     (*                                                        *)
     (*   - BESEITIGEN DER BREMSE BEI DEN ENDZUSTAENDEN.       *)
     (*   - BESEITIGEN VON PRODUKTIONEN, DIE NICHT MEHR        *)
     (*     BENOETIGT WERDEN (LEICHEN),                        *)
     (*     I.E. PRODUKTIONEN, DEREN LINKE SEITE ('LINKS')     *)
     (*     NIRGENDS RECHTS STEHT, MIT AUSNAHME DES START-     *)
     (*     ZUSTANDS (PROZEDUR 'REINIGE').                     *)
     (*                                                        *)
     (**********************************************************)

     MAXINDETZUST := MAXZUSTAND + 1 ;
     SCHLUSS := PRODANKER ;
     while SCHLUSS -> . LINKS <> MAXINT do
       SCHLUSS := SCHLUSS -> . NEXT ;
     ENDSCHLUSS := ENDANKER ;
     while ENDSCHLUSS -> . LINK <> NIL do
       ENDSCHLUSS := ENDSCHLUSS -> . LINK ;
     NEW2 ( ENDSCHLUSS -> . LINK ) ;
     ENDSCHLUSS := ENDSCHLUSS -> . LINK ;
     ENDSCHLUSS -> . WERT := MAXINT ;
     NEW ( INDETANKER ) ;
     INDETANKER -> . ELEMENTE := NIL ;
     INDETANKER -> . ZUSTAND := MAXINT ;
     INDETANKER -> . NEXT := NIL ;
     BAUESET ( SETANKER , MAXZUSTAND ) ;
     ZWEI := PRODANKER ;
     while ZWEI -> . NEXT <> NIL do
       begin
         EINS := ZWEI ;
         ZWEI := ZWEI -> . NEXT ;
         if EQUAL2 ( EINS , ZWEI ) then
           begin
             NICHTDET ( PRODANKER , ENDANKER , INDETANKER , EINS , ZWEI
                        , SCHLUSS , ENDSCHLUSS , MAXINDETZUST ,
                        MAXZUSTAND , SETANKER ) ;
           end (* then *)
       end (* while *) ;
     LOESCHE := ENDANKER ;
     while LOESCHE -> . LINK -> . WERT <> MAXINT do
       LOESCHE := LOESCHE -> . LINK ;
     DISPOSE2 ( LOESCHE -> . LINK ) ;
     REINIGE ( PRODANKER , MAXINDETZUST , ENDANKER )
   end (* DETAUTOMAT *) ;


procedure AUTOMGENC ( ENDZUSTANKER : ZUSTVERW ; PRODANKER : PRODVER ) ;

(**********************************************************)
(*                                                        *)
(*   NEUE GENERATORFUNKTION:                              *)
(*                                                        *)
(*   HIER WIRD DAS PROGRAMM ERZEUGT, DAS DIE              *)
(*   SCANNERFUNKTION ENTHAELT. DAS PROGRAMM IST           *)
(*   IN ANSI-C.                                           *)
(*                                                        *)
(*   AUS DER DATEI 'MUSTER' WIRD EIN PROGRAMMGERUEST      *)
(*   (SKELETON) GEHOLT, DAS DURCH DIESE PROZEDUR          *)
(*   MIT LEBEN GEFUELLT WIRD.                             *)
(*                                                        *)
(**********************************************************)


   var ZEILE : packed array [ 1 .. 80 ] of CHAR ;


   procedure WORK1 ;

      var ZLAUF : ZUSTVERW ;
          PLAUF : PRODVER ;
          GEFUNDEN : BOOLEAN ;
          ZUST : INTEGER ;
          ZUSTALT : INTEGER ;
          VORHANDEN : BOOLEAN ;
          CH1 , CH2 : CHAR ;
          PSUCH , PSUCH2 : PRODVER ;
          PSUCH2ALT : PRODVER ;
          ZUSTNEU : INTEGER ;
          LAUFANZ : INTEGER ;
          I : INTEGER ;
          ANZAHL : INTEGER ;
          MINZUST : INTEGER ;

      begin (* WORK1 *)
        ZLAUF := ENDZUSTANKER ;
        VORHANDEN := FALSE ;

        (************************************************)
        (*   CASE-LABELS FUER ENDZUSTAENDE, DIE LINKS   *)
        (*   NIRGENDS AUFTAUCHEN.                       *)
        (************************************************)

        while ZLAUF <> NIL do
          begin
            GEFUNDEN := FALSE ;
            PLAUF := PRODANKER ;
            while ( PLAUF -> . LINKS <> MAXINT ) and not GEFUNDEN do
              begin
                GEFUNDEN := ( PLAUF -> . LINKS = ZLAUF -> . WERT ) ;
                if not GEFUNDEN then
                  PLAUF := PLAUF -> . NEXT ;
              end (* while *) ;
            if not GEFUNDEN then
              begin
                VORHANDEN := TRUE ;
                WRITE ( AUTOM , ' ' : 9 ) ;
                WRITE ( AUTOM , 'case ' ) ;
                WRITE ( AUTOM , ZLAUF -> . WERT : ISTELLEN ( ZLAUF -> .
                        WERT ) ) ;
                WRITE ( AUTOM , ':' ) ;
                WRITELN ( AUTOM )
              end (* then *) ;
            ZLAUF := ZLAUF -> . LINK
          end (* while *) ;
        if VORHANDEN then
          begin
            WRITE ( AUTOM , ' ' : 12 ) ;
            WRITE ( AUTOM , 'zust = -1;' ) ;
            WRITELN ( AUTOM ) ;
            WRITE ( AUTOM , ' ' : 12 ) ;
            WRITE ( AUTOM , 'break;' ) ;
            WRITELN ( AUTOM )
          end (* then *) ;

        (************************************************)
        (*   KENNZEICHNEN DER BEREITS ABGEARBEITETEN    *)
        (*   PRODUKTIONEN UEBER DIE TERMNR              *)
        (************************************************)

        PLAUF := PRODANKER ;
        while ( PLAUF -> . LINKS <> MAXINT ) do
          begin
            PLAUF -> . TERMNR := 0 ;
            PLAUF := PLAUF -> . NEXT ;
          end (* while *) ;

        (************************************************)
        (*   IMMER DANN AKTIV WERDEN, WENN DER          *)
        (*   ZUSTAND WECHSELT                           *)
        (************************************************)

        PLAUF := PRODANKER ;
        ZUSTALT := - 1 ;
        while ( PLAUF -> . LINKS <> MAXINT ) do
          begin
            ZUST := PLAUF -> . LINKS ;
            if ZUST <> ZUSTALT then
              begin
                WRITE ( AUTOM , ' ' : 9 ) ;
                WRITE ( AUTOM , 'case ' ) ;
                WRITE ( AUTOM , ZUST : ISTELLEN ( ZUST ) ) ;
                WRITE ( AUTOM , ':' ) ;
                WRITELN ( AUTOM ) ;

        (************************************************)
        (*   SUCHEN NACH LAEUFEN MIT MEHR ALS 8         *)
        (*   LINKS AUFEINANDERFOLGENDEN TERMINALS       *)
        (*   UND RECHTS GLEICHEN ZUSTAENDEN.            *)
        (************************************************)

                PSUCH := PLAUF ;
                PSUCH2 := PSUCH ;
                LAUFANZ := 1 ;
                while PSUCH2 -> . LINKS = ZUST do
                  begin
                    PSUCH2ALT := PSUCH2 ;
                    PSUCH2 := PSUCH2 -> . NEXT ;
                    if ( PSUCH2 -> . LINKS <> ZUST ) or ( PSUCH2 -> .
                    TERMINAL <> SUCC ( PSUCH2ALT -> . TERMINAL ) ) or (
                    PSUCH2 -> . RECHTS <> PSUCH -> . RECHTS ) then
                      begin
                        if LAUFANZ >= 8 then
                          begin
                            CH1 := PSUCH -> . TERMINAL ;
                            CH2 := CHR ( ORD ( PSUCH -> . TERMINAL ) +
                                   LAUFANZ - 1 ) ;
                            ZUSTNEU := PSUCH -> . RECHTS ;
                            for I := 1 to LAUFANZ do
                              begin
                                PSUCH -> . TERMNR := 1 ;
                                PSUCH := PSUCH -> . NEXT ;
                              end (* for *) ;
                            WRITE ( AUTOM , ' ' : 12 ) ;
                            WRITE ( AUTOM , 'if (ch >= ' ) ;
                            CHAROUT ( AUTOM , CH1 , TRUE ) ;
                            WRITE ( AUTOM , ' && ch <= ' ) ;
                            CHAROUT ( AUTOM , CH2 , TRUE ) ;
                            WRITE ( AUTOM , ')' ) ;
                            WRITELN ( AUTOM ) ;
                            WRITE ( AUTOM , ' ' : 15 ) ;
                            WRITE ( AUTOM , 'zust = ' ) ;
                            WRITE ( AUTOM , ZUSTNEU : ISTELLEN (
                                    ZUSTNEU ) ) ;
                            WRITE ( AUTOM , ';' ) ;
                            WRITELN ( AUTOM ) ;
                            WRITE ( AUTOM , ' ' : 12 ) ;
                            WRITE ( AUTOM , 'else' ) ;
                            WRITELN ( AUTOM ) ;
                          end (* then *) ;
                        PSUCH := PSUCH2 ;
                        LAUFANZ := 1
                      end (* then *)
                    else
                      LAUFANZ := LAUFANZ + 1 ;
                  end (* while *) ;

        (************************************************)
        (*   JETZT WIRD DER REST MIT SWITCH ABGEHAN-    *)
        (*   DELT.                                      *)
        (************************************************)

                ANZAHL := 0 ;
                MINZUST := MAXINT ;
                PSUCH := PLAUF ;
                while PSUCH -> . LINKS = ZUST do
                  begin
                    if PSUCH -> . TERMNR = 0 then
                      begin
                        ANZAHL := ANZAHL + 1 ;
                        if PSUCH -> . RECHTS < MINZUST then
                          MINZUST := PSUCH -> . RECHTS ;
                      end (* then *) ;
                    PSUCH := PSUCH -> . NEXT
                  end (* while *) ;
                if ANZAHL = 0 then
                  begin
                    WRITE ( AUTOM , ' ' : 15 ) ;
                    WRITE ( AUTOM , 'zust = -1;' ) ;
                    WRITELN ( AUTOM ) ;
                  end (* then *)
                else
                  begin
                    WRITE ( AUTOM , ' ' : 12 ) ;
                    WRITE ( AUTOM , 'switch (ch)' ) ;
                    WRITELN ( AUTOM ) ;
                    WRITE ( AUTOM , ' ' : 12 ) ;
                    WRITE ( AUTOM , '{' ) ;
                    WRITELN ( AUTOM ) ;
                    PSUCH := PLAUF ;
                    while PSUCH -> . LINKS = ZUST do
                      begin
                        if PSUCH -> . TERMNR = 0 then
                          begin
                            WRITE ( AUTOM , ' ' : 15 ) ;
                            WRITE ( AUTOM , 'case ' ) ;
                            CHAROUT ( AUTOM , PSUCH -> . TERMINAL ,
                                      TRUE ) ;
                            WRITE ( AUTOM , ': zust = ' ) ;
                            WRITE ( AUTOM , PSUCH -> . RECHTS :
                                    ISTELLEN ( PSUCH -> . RECHTS ) ) ;
                            WRITE ( AUTOM , '; break;' ) ;
                            WRITELN ( AUTOM ) ;
                          end (* then *) ;
                        PSUCH := PSUCH -> . NEXT
                      end (* while *) ;
                    WRITE ( AUTOM , ' ' : 15 ) ;
                    WRITE ( AUTOM , 'default: zust = -1; break;' ) ;
                    WRITELN ( AUTOM ) ;
                    WRITE ( AUTOM , ' ' : 12 ) ;
                    WRITE ( AUTOM , '}' ) ;
                    WRITELN ( AUTOM ) ;
                  end (* else *) ;
                WRITE ( AUTOM , ' ' : 12 ) ;
                WRITE ( AUTOM , 'break;' ) ;
                WRITELN ( AUTOM ) ;
                ZUSTALT := ZUST ;
              end (* then *) ;
            PLAUF := PLAUF -> . NEXT ;
          end (* while *) ;
      end (* WORK1 *) ;


   procedure WORKA ;

      var ZLAUF : ZUSTVERW ;
          PLAUF : PRODVER ;
          GEFUNDEN : BOOLEAN ;
          ZUST : INTEGER ;
          ZUSTALT : INTEGER ;
          VORHANDEN : BOOLEAN ;
          CH1 , CH2 : CHAR ;
          PSUCH , PSUCH2 : PRODVER ;
          PSUCH2ALT : PRODVER ;
          ZUSTNEU : INTEGER ;
          LAUFANZ : INTEGER ;
          I : INTEGER ;
          ANZAHL : INTEGER ;
          MINZUST : INTEGER ;

      begin (* WORKA *)
        ZLAUF := ENDZUSTANKER ;
        VORHANDEN := FALSE ;

        (************************************************)
        (*   CASE-LABELS FUER ENDZUSTAENDE, DIE LINKS   *)
        (*   NIRGENDS AUFTAUCHEN.                       *)
        (************************************************)

        while ZLAUF <> NIL do
          begin
            GEFUNDEN := FALSE ;
            PLAUF := PRODANKER ;
            while ( PLAUF -> . LINKS <> MAXINT ) and not GEFUNDEN do
              begin
                GEFUNDEN := ( PLAUF -> . LINKS = ZLAUF -> . WERT ) ;
                if not GEFUNDEN then
                  PLAUF := PLAUF -> . NEXT ;
              end (* while *) ;
            if not GEFUNDEN then
              begin
                WRITE ( AUTOM , ' ' : 6 ) ;
                WRITE ( AUTOM , ZLAUF -> . WERT : 1 ) ;
                WRITE ( AUTOM , ': zust := -1; ' ) ;
                WRITELN ( AUTOM )
              end (* then *) ;
            ZLAUF := ZLAUF -> . LINK
          end (* while *) ;

        (************************************************)
        (*   KENNZEICHNEN DER BEREITS ABGEARBEITETEN    *)
        (*   PRODUKTIONEN UEBER DIE TERMNR              *)
        (************************************************)

        PLAUF := PRODANKER ;
        while ( PLAUF -> . LINKS <> MAXINT ) do
          begin
            PLAUF -> . TERMNR := 0 ;
            PLAUF := PLAUF -> . NEXT ;
          end (* while *) ;

        (************************************************)
        (*   IMMER DANN AKTIV WERDEN, WENN DER          *)
        (*   ZUSTAND WECHSELT                           *)
        (************************************************)

        PLAUF := PRODANKER ;
        ZUSTALT := - 1 ;
        while ( PLAUF -> . LINKS <> MAXINT ) do
          begin
            ZUST := PLAUF -> . LINKS ;
            if ZUST <> ZUSTALT then
              begin
                WRITE ( AUTOM , ' ' : 6 ) ;
                WRITE ( AUTOM , ZUST : 1 ) ;
                WRITE ( AUTOM , ': begin' ) ;
                WRITELN ( AUTOM ) ;

        (************************************************)
        (*   SUCHEN NACH LAEUFEN MIT MEHR ALS 8         *)
        (*   LINKS AUFEINANDERFOLGENDEN TERMINALS       *)
        (*   UND RECHTS GLEICHEN ZUSTAENDEN.            *)
        (************************************************)

                PSUCH := PLAUF ;
                PSUCH2 := PSUCH ;
                LAUFANZ := 1 ;
                while PSUCH2 -> . LINKS = ZUST do
                  begin
                    PSUCH2ALT := PSUCH2 ;
                    PSUCH2 := PSUCH2 -> . NEXT ;
                    if ( PSUCH2 -> . LINKS <> ZUST ) or ( PSUCH2 -> .
                    TERMINAL <> SUCC ( PSUCH2ALT -> . TERMINAL ) ) or (
                    PSUCH2 -> . RECHTS <> PSUCH -> . RECHTS ) then
                      begin
                        if LAUFANZ >= 8 then
                          begin
                            CH1 := PSUCH -> . TERMINAL ;
                            CH2 := CHR ( ORD ( PSUCH -> . TERMINAL ) +
                                   LAUFANZ - 1 ) ;
                            ZUSTNEU := PSUCH -> . RECHTS ;
                            for I := 1 to LAUFANZ do
                              begin
                                PSUCH -> . TERMNR := 1 ;
                                PSUCH := PSUCH -> . NEXT ;
                              end (* for *) ;
                            WRITE ( AUTOM , ' ' : 8 ) ;
                            WRITE ( AUTOM , 'if (ch >= ' ) ;
                            CHAROUT ( AUTOM , CH1 , TRUE ) ;
                            WRITE ( AUTOM , ') and (ch <= ' ) ;
                            CHAROUT ( AUTOM , CH2 , TRUE ) ;
                            WRITE ( AUTOM , ') then ' ) ;
                            WRITELN ( AUTOM ) ;
                            WRITE ( AUTOM , ' ' : 10 ) ;
                            WRITE ( AUTOM , 'zust := ' ) ;
                            WRITE ( AUTOM , ZUSTNEU : 1 ) ;
                            WRITELN ( AUTOM ) ;
                            WRITE ( AUTOM , ' ' : 8 ) ;
                            WRITE ( AUTOM , 'else' ) ;
                            WRITELN ( AUTOM ) ;
                          end (* then *) ;
                        PSUCH := PSUCH2 ;
                        LAUFANZ := 1
                      end (* then *)
                    else
                      LAUFANZ := LAUFANZ + 1 ;
                  end (* while *) ;

        (************************************************)
        (*   JETZT WIRD DER REST MIT SWITCH ABGEHAN-    *)
        (*   DELT.                                      *)
        (************************************************)

                ANZAHL := 0 ;
                MINZUST := MAXINT ;
                PSUCH := PLAUF ;
                while PSUCH -> . LINKS = ZUST do
                  begin
                    if PSUCH -> . TERMNR = 0 then
                      begin
                        ANZAHL := ANZAHL + 1 ;
                        if PSUCH -> . RECHTS < MINZUST then
                          MINZUST := PSUCH -> . RECHTS ;
                      end (* then *) ;
                    PSUCH := PSUCH -> . NEXT
                  end (* while *) ;
                if ANZAHL = 0 then
                  begin
                    WRITE ( AUTOM , ' ' : 10 ) ;
                    WRITE ( AUTOM , 'zust := -1;' ) ;
                    WRITELN ( AUTOM ) ;
                  end (* then *)
                else
                  begin
                    WRITE ( AUTOM , ' ' : 8 ) ;
                    WRITE ( AUTOM , 'case ch of' ) ;
                    WRITELN ( AUTOM ) ;
                    PSUCH := PLAUF ;
                    while PSUCH -> . LINKS = ZUST do
                      begin
                        if PSUCH -> . TERMNR = 0 then
                          begin
                            WRITE ( AUTOM , ' ' : 10 ) ;
                            CHAROUT ( AUTOM , PSUCH -> . TERMINAL ,
                                      TRUE ) ;
                            WRITE ( AUTOM , ': zust := ' ) ;
                            WRITE ( AUTOM , PSUCH -> . RECHTS :
                                    ISTELLEN ( PSUCH -> . RECHTS ) ) ;
                            WRITE ( AUTOM , ';' ) ;
                            WRITELN ( AUTOM ) ;
                          end (* then *) ;
                        PSUCH := PSUCH -> . NEXT
                      end (* while *) ;
                    WRITE ( AUTOM , ' ' : 10 ) ;
                    WRITE ( AUTOM , 'otherwise zust := -1' ) ;
                    WRITELN ( AUTOM ) ;
                    WRITE ( AUTOM , ' ' : 8 ) ;
                    WRITE ( AUTOM , 'end' ) ;
                    WRITELN ( AUTOM ) ;
                  end (* else *) ;
                WRITE ( AUTOM , ' ' : 6 ) ;
                WRITE ( AUTOM , 'end;' ) ;
                WRITELN ( AUTOM ) ;
                ZUSTALT := ZUST ;
              end (* then *) ;
            PLAUF := PLAUF -> . NEXT ;
          end (* while *) ;
      end (* WORKA *) ;


   procedure WORK2 ;

      var ZLAUF : ZUSTVERW ;

      begin (* WORK2 *)
        ZLAUF := ENDZUSTANKER ;
        while ZLAUF <> NIL do
          begin
            WRITE ( AUTOM , ' ' : 6 ) ;
            WRITE ( AUTOM , 'case ' ) ;
            WRITE ( AUTOM , ZLAUF -> . WERT : ISTELLEN ( ZLAUF -> .
                    WERT ) ) ;
            WRITE ( AUTOM , ': symbolnr = ' ) ;
            WRITE ( AUTOM , ZLAUF -> . TERMNR : ISTELLEN ( ZLAUF -> .
                    TERMNR ) ) ;
            WRITE ( AUTOM , '; break;' ) ;
            WRITELN ( AUTOM ) ;
            ZLAUF := ZLAUF -> . LINK
          end (* while *) ;
      end (* WORK2 *) ;

   begin (* AUTOMGENC *)
     RESET ( MUSTER ) ;
     while not EOF ( MUSTER ) do
       begin
         READLN ( MUSTER , ZEILE ) ;
         if ( ZEILE [ 1 ] = '-' ) and ( ZEILE [ 3 ] = '-' ) then
           begin
             case ZEILE [ 2 ] of
               '1' : WORK1 ;
               '2' : WORK2 ;
               'A' : WORKA ;
             end (* case *)
           end (* then *)
         else
           WRITELN ( AUTOM , ZEILE ) ;
       end (* while *) ;
   end (* AUTOMGENC *) ;

begin (* HAUPTPROGRAMM *)
  RESET ( QUELLE ) ;
  REWRITE ( AUTOM ) ;
  RESERVE1 := NIL ;
  RESERVE2 := NIL ;
  SYNTAXPRUEFUNG ( PRODKETTE , OK , ENDZUSTAENDE , AKZUSTAND ) ;
  if OK then
    begin
      WRITELN ;
      WRITELN ;
      WRITELN ( 'Testdruck vor Sortierung:' ) ;
      TESTDRUCK ( PRODKETTE ) ;
      PRODSORT ( PRODKETTE ) ;
      WRITELN ;
      WRITELN ( 'Testdruck nach Sortierung:' ) ;
      TESTDRUCK ( PRODKETTE ) ;
      WRITELN ;
      WRITELN ( 'Enddruck nach Sortierung:' ) ;
      ENDDRUCK ( ENDZUSTAENDE ) ;
      if not ENDZUST_TERMNR ( ENDZUSTAENDE , PRODKETTE ) then
        begin
          WRITELN ;
          WRITELN ( '+++ Fehler bei ENDZUST_TERMNR !' ) ;
        end (* then *)
      else
        begin
          WRITELN ;
          WRITELN ( 'Enddruck nach ENDZUST_TERMNR:' ) ;
          ENDDRUCK ( ENDZUSTAENDE ) ;
          DETAUTOMAT ( PRODKETTE , ENDZUSTAENDE , AKZUSTAND ) ;
          WRITELN ;
          WRITELN ( 'Testdruck nach Umwandlung:' ) ;
          TESTDRUCK ( PRODKETTE ) ;
          WRITELN ;
          WRITELN ( 'Enddruck nach Umwandlung:' ) ;
          ENDDRUCK ( ENDZUSTAENDE ) ;
          AUTOMGENC ( ENDZUSTAENDE , PRODKETTE ) ;
        end (* else *) ;
    end (* then *) ;
end (* HAUPTPROGRAMM *) .
