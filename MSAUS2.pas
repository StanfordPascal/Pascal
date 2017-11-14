program MSAUS2 ( INPUT , OUTPUT , OEVMODELL , IVMODELL , MSFILE ,
                 EINGABE , AUSGABE , NEUFILE , TESTDRUCK ) ;

(****************************************************************)
(*                                                              *)
(*   PROGRAMM MSAUS2 - MODAL-SPLIT-AUSWERTUNG                   *)
(*                                                              *)
(*   STAND: JUNI 1985                                           *)
(*                                                              *)
(*   ERSTELLT 1984 IM RAHMEN EINER STUDIENARBEIT VON            *)
(*   BERND OPPOLZER                                             *)
(*                                                              *)
(*   WEITERENTWICKELT 1985 IM RAHMEN EINER DIPLOMARBEIT         *)
(*   VON BERND OPPOLZER                                         *)
(*                                                              *)
(*   DAS AUSWERTUNGSPROGRAMM MSAUS2 BERECHNET AUS JE EINEM      *)
(*   OEV- UND EINEM IV-MODELL DIE REISEZEITEN JE VERKEHRS-      *)
(*   ZELLENBEZOGENER QUELLE-ZIEL-RELATION UND ERMITTELT         *)
(*   AUS DEM REISEZEITVERHAELTNIS UEBER EINE VORGEGEBENE        *)
(*   MODAL-SPLIT-FUNKTION DEN ZU ERWARTENDEN OEV-ANTEIL         *)
(*   ODER DIE MODAL-SPLIT-AENDERUNG.                            *)
(*                                                              *)
(*   DIE ERMITTLUNG DES ZU ERWARTENDEN OEV-ANTEILS LEISTET      *)
(*   DER MODUS 1. ZUR VERSORGUNG MIT PARAMETERN SIEHE DIE       *)
(*   EXEC-2-PROZEDUR MSAUS2.                                    *)
(*                                                              *)
(*   DIE ERMITTLUNG DER MODAL-SPLIT-AENDERUNG BEI AENDERUNG     *)
(*   DES MODELLS WIRD UEBER DIE MODI 2 UND 3 ABGEWICKELT.       *)
(*   DABEI WIRD IM MODUS 2 DAS ALTE MODELL ZUR BERECHNUNG       *)
(*   HERANGEZOGEN, IM MODUS 3 DAS NEUE. DIE AUSGABE VON         *)
(*   MODUS 2 WIRD ZUR EINGABE VON MODUS 3.                      *)
(*                                                              *)
(*   DIE ERWEITERUNG INNERHALB DER DIPLOMARBEIT BEZIEHT         *)
(*   SICH AUF DIE ERMITTLUNG DER MODAL-SPLIT-AENDERUNG          *)
(*   (ALSO MODI 2 UND 3). DIE ERMITTLUNG DES OEV-ANTEILS        *)
(*   (MODUS 1) BLEIBT UNVERAENDERT.                             *)
(*                                                              *)
(*                                                              *)
(*   DIE BEDEUTUNG DER ANGESPROCHENEN DATEIEN:                  *)
(*                                                              *)
(*   - INPUT,OUTPUT: KONSOL-EIN-/AUSGABE (INPUT WIRD NICHT      *)
(*                   VERWENDET).                                *)
(*   - OEVMODELL,IVMODELL:                                      *)
(*                   MODELLDATEIEN, DIE DIE MODELLDATEN         *)
(*                   ENTHALTEN.                                 *)
(*   - MSFILE:       ENTHAELT DIE MODAL-SPLIT-FUNKTION.         *)
(*   - EINGABE:      EINGABEDATEN; PRO QUELLE-ZIEL-RELATION     *)
(*                   EINE ZEILE. DIE SATZSTRUKTUR DER           *)
(*                   EINGABEDATEI IST UNTERSCHIEDLICH;          *)
(*                   SIEHE PROZEDUREN MODUS1, MODUS2, MODUS3.   *)
(*   - AUSGABE:      IM MODUS 1 UND 3 DIE DRUCKAUSGABE DER      *)
(*                   ERGEBNISSE; IM MODUS 2 DIE AUSGABE DER     *)
(*                   ZWISCHENERGEBNISSE FUER DEN MODUS 3.       *)
(*   - NEUFILE:      (NUR IM MODUS 3) DIE AUSGABE GEWISSER      *)
(*                   ENDRESULTATE FUER EINE SPAETERE WEITER-    *)
(*                   VERARBEITUNG                               *)
(*   - TESTDRUCK:    FALLS -TESTMODUS- = TRUE IST, WERDEN       *)
(*                   HIER DIE GEFUNDENEN WEGE BEI DEN           *)
(*                   ROUTENSUCHEN AUSGEGEBEN.                   *)
(*                                                              *)
(*                                                              *)
(*   AENDERUNG AM 21.1.85 / OPPOLZER                            *)
(*                                                              *)
(*   IN DER OEV-ROUTENSUCHE WIRD DIE ERMITTLUNG DER WARTE-      *)
(*   ZEITEN FUER EINSTEIGEN UND UMSTEIGEN WIE FOLGT GE-         *)
(*   AENDERT:                                                   *)
(*                                                              *)
(*   BEI UEBERSCHREITEN EINER (DURCH KONSTANTEN) FESTGELEGTEN   *)
(*   GRENZE FUER DIE BEDIENUNGSZEITINTERVALLE WIRD EIN          *)
(*   KONSTANTER WERT FUER DIE WARTEZEITEN BERECHNET (NICHT      *)
(*   NULL WIE BISHER).                                          *)
(*                                                              *)
(*                                                              *)
(*   AENDERUNG AM 19.4.85 / OPPOLZER                            *)
(*                                                              *)
(*   WEGEN SPEICHERPLATZPROBLEMEN BEI GROESSEREN MODELLEN       *)
(*   WIRD DIE OEV-SUCHTABELLE NICHT MEHR ALS ARRAY IMPLE-       *)
(*   MENTIERT (WOBEI VIELE FELDER NICHT BENOETIGT WERDEN),      *)
(*   SONDERN ALS ZWEIDIMENSIONALE ZEIGERSTRUKTUR. NUR DIE       *)
(*   FELDER, DIE IM BOOLEAN-TEILFELD -VORHANDEN- DEN            *)
(*   WERT TRUE ERHALTEN (KOENNEN), WERDEN TATSAECHLICH          *)
(*   ABGELEGT. ZU DIESEM ZWECK WURDE DIE DEFINITION DES         *)
(*   TYPS -OEVSUCHELEM- WIE FOLGT ERWEITERT:                    *)
(*                                                              *)
(*   HSTEIGEN    : INDEX DER FELD-EIGENEN HALTESTELLE           *)
(*   LINIEEIGEN  : INDEX DER FELD-EIGENEN LINIE                 *)
(*   NEXTSPALTE  : NAECHSTES FELD DERSELBEN SPALTE              *)
(*   NEXTZEILE   : NAECHSTES FELD DERSELBEN ZEILE               *)
(*                                                              *)
(****************************************************************)



const BMAX = 500 ;
      VPMAX = 600 ;
      STRMAX = 300 ;
      HMAX = 1000 ;
      LMAX = 250 ;

      (****************************************************************)
      (*                                                              *)
      (*   PROGRAMMKONSTANTEN, DIE DIE GROESSE DER INTERNEN FELDER    *)
      (*   FESTLEGEN:                                                 *)
      (*                                                              *)
      (*   BMAX:    MAXIMALE ANZAHL DER BEZIRKE (DIE BEZIRKE AUS      *)
      (*            OEV- UND IV-MODELL WERDEN GEMISCHT)               *)
      (*   VPMAX:   MAXIMALE ANZAHL DER VERKEHRSKNOTENPUNKTE          *)
      (*   STRMAX:  MAXIMALE ANZAHL STRASSEN                          *)
      (*   HMAX:    MAXIMALE ANZAHL HALTESTELLEN                      *)
      (*   LMAX:    MAXIMALE ANZAHL LINIEN                            *)
      (*                                                              *)
      (*   ES IST DARAUF ZU ACHTEN, DASS DIE GROESSENANGABEN MIT      *)
      (*   DENEN AUS DEN MODELLEDITOREN UEBEREINSTIMMEN. BMAX         *)
      (*   SOLLTE SO GROSS SEIN, DASS DIE VEREINIGUNG DER BEZIRKS-    *)
      (*   MENGEN AUS OEV- UND IV-MODELL IN DER TABELLE PLATZ         *)
      (*   FINDET.                                                    *)
      (*                                                              *)
      (****************************************************************)

      DIFF = 3 ;
      KATMAX = 8 ;

      (****************************************************************)
      (*                                                              *)
      (*   WEITERE KONSTANTEN FUER DIE MODELLE:                       *)
      (*                                                              *)
      (*   DIFF:    ANZAHL DER TAGESZEITEN.                           *)
      (*            FOLGENDE ZUORDNUNG WIRD GETROFFEN:                *)
      (*              1 = HAUPTVERKEHR,                               *)
      (*              2 = NORMALVERKEHR,                              *)
      (*              3 = SPAETVERKEHR.                               *)
      (*   KATMAX:  ANZAHL DER KATEGORIEN FUER STRASSEN               *)
      (*            (DIE KATEGORIE LEGT DIE MOEGLICHE GESCHWIN-       *)
      (*            DIGKEIT FEST)                                     *)
      (*                                                              *)
      (****************************************************************)

      EINFAKT = 0.5 ;
      FWFAKT = 1.0 ;
      KOMFORT = 2.0 ;
      UMFAKT = 0.5 ;
      BHOGREIN = 12.0 ;
      BHOGRUM = 12.0 ;

      (****************************************************************)
      (*                                                              *)
      (*   KONSTANTEN, DIE FUER DIE OEV-ROUTENSUCHE VON BEDEUTUNG     *)
      (*   SIND:                                                      *)
      (*                                                              *)
      (*   EINFAKT:  FAKTOR FUER DIE GEWICHTUNG DER EINSTEIGE-        *)
      (*             WARTEZEIT.                                       *)
      (*   FWFAKT:   FAKTOR FUER DIE GEWICHTUNG DES FUSSWEGES.        *)
      (*   KOMFORT:  MASS FUER DIE ABSTIMMUNG DES FAHRPLANS           *)
      (*             HINSICHTLICH UMSTEIGEWARTEZEITEN.                *)
      (*   UMFAKT:   FAKTOR FUER DIE GEWICHTUNG DER UMSTEIGE-         *)
      (*             WARTEZEIT.                                       *)
      (*   BHOGREIN: OBERGRENZE DER BEDIENUNGSZEITINTERVALLE  FUER    *)
      (*             EINSTEIGEWARTEZEITEN. BIS ZU DIESEM WERT         *)
      (*             WIRD ZUR ERMITTLUNG DER WARTEZEIT DAS BE-        *)
      (*             DIENUNGSZEITINTERVALL HERANGEZOGEN. BEI          *)
      (*             GROESSEREN WERTEN WIRD DAFUER DER WERT           *)
      (*             -BHOGREIN- HERANGEZOGEN.                         *)
      (*   BHOGRUM:  ANALOG -BHOGREIN- FUER UMSTEIGEWARTEZEITEN.      *)
      (*                                                              *)
      (****************************************************************)

      IOGESCHW = 30.0 ;
      AOGESCHW = 60.0 ;
      TABZ = 18 ;
      MSUNTEN = 12.5 ;
      MSOBEN = 80.0 ;
      MSKONSTANTE = 0.2 ;
      PKWBESETZ = 1.3 ;
      GRKLD = 64 ;
      LEERKOMM = '                         ' ;

      (****************************************************************)
      (*                                                              *)
      (*   UEBRIGE KONSTANTEN:                                        *)
      (*                                                              *)
      (*   IOGESCHW:  DIESE GESCHWINDIGKEIT WIRD ALS STANDARDWERT     *)
      (*              FUER ANSCHLUESSE INNERORTS ANGENOMMEN.          *)
      (*              (WIRD BENOETIGT ZUR ERMITTLUNG DER STRECKEN-    *)
      (*              LAENGE AUF ANSCHLUESSEN).                       *)
      (*   AOGESCHW:  ANALOG -IOGESCHW- FUER AUSSERORTS.              *)
      (*   TABZ:      ANZAHL TABELLENZEILEN                           *)
      (*              FUER TABELLE DER REISEZEITGEWINNE.              *)
      (*   MSUNTEN:   UNTERGRENZE FUER OEV-PROZENTSATZ BEI DER        *)
      (*              MODAL-SPLIT-FUNKTION BEI NEUEM BERECHNUNGS-     *)
      (*              MODUS.                                          *)
      (*   MSOBEN:    OBERGRENZE FUER OEV-PROZENTSATZ BEI NEUEM       *)
      (*              BERECHNUNGSMODUS.                               *)
      (*   MSKONSTANTE:                                               *)
      (*              STEIGUNG DER MODAL-SPLIT-FUNKTION IM            *)
      (*              LINEAREN BEREICH BEI NEUEM BERECHNUNGSMODUS.    *)
      (*   PKWBESETZ:                                                 *)
      (*              PKW-BESETZUNGSGRAD (PERSONEN PRO FAHRZEUG).     *)
      (*   GRKLD:     ORD(GROSS-A)-ORD(KLEIN-A)                       *)
      (*                                                              *)
      (****************************************************************)



type MSFPVERW = -> MSFPUNKT ;

     (***********************************************************)
     (*   TYPEN FUER DIE DARSTELLUNG DER MODAL-SPLIT-FUNKTION   *)
     (*   IM ALTEN BERECHNUNGSMODUS:                            *)
     (*   IN EINER POINTERKETTE WERDEN DIE STUETZPUNKTE DER     *)
     (*   FUNKTION ABGELEGT, GEORDNET NACH X-WERTEN.            *)
     (***********************************************************)

     MSFPUNKT = record
                  X : INTEGER ;
                  Y : REAL ;
                  NEXT : MSFPVERW
                end ;

     (*****************************************)
     (*   SUBRANGE-TYPEN (SIEHE KONSTANTEN)   *)
     (*****************************************)

     BTYP = 0 .. BMAX ;
     VPTYP = 0 .. VPMAX ;
     STRTYP = 0 .. STRMAX ;
     HTYP = 0 .. HMAX ;
     LTYP = 0 .. LMAX ;

     (***********************)
     (*   DIV. HILFSTYPEN   *)
     (***********************)

     STAT = 0 .. 4 ;
     SITUATIONEN = ( HAUPT , NORMAL , SPAET ) ;
     HTEXT = packed array [ 1 .. 25 ] of CHAR ;
     FNAME = packed array [ 1 .. 8 ] of CHAR ;

     (*****************************************************************)
     (*************************************************************** *)
     (*****************************************************************)
     (*   HIER BEGINNEN DIE DATENTYPEN, DIE ZUR AUFNAHME DES IV-MODEL *)
     (*LS                                                             *)
     (*   DIENEN: KNOTENPUNKTE, ANSCHLUESSE, STRASSEN USW.            *)
     (*                                                               *)
     (*************************************************************** *)
     (*****************************************************************)
     (*****************************************************************)

     APTYP = -> ANSCHLUSS ;

     (****************************************)
     (*   LISTE VON ANSCHLUESSEN JE BEZIRK   *)
     (****************************************)

     ANSCHLUSS = record
                   ANSPUNKT : INTEGER ;
                   NEXT : APTYP ;
                   ANFAHRZ : array [ SITUATIONEN ] of INTEGER ;
                   INORTS : BOOLEAN ;
                 end ;

     (***************************************************)
     (*   POSITION VON KNOTENPUNKTEN UND HALTESTELLEN   *)
     (***************************************************)

     POSITION = record
                  X : INTEGER ;
                  Y : INTEGER
                end ;
     KATEGORIEN = 0 .. KATMAX ;

     (*****************************************************************)
     (*                                                               *)
     (*   TYPEN ZUR DARSTELLUNG VON STRASSEN                          *)
     (*                                                               *)
     (*   -STPUNKT- ENTHAELT DIE ANGABEN FUER EINEN STRECKEN-         *)
     (*   ABSCHNITT, NAEMLICH:                                        *)
     (*                                                               *)
     (*   VKPUNKT:      DER AUSGANGSKNOTENPUNKT                       *)
     (*   NEXT:         VERWEIS AUF DEN NAECHSTEN KNOTEN              *)
     (*   KAT:          DIE KATEGORIE DES STRECKENABSCHNITTS          *)
     (*   INNERORTS:    GIBT AN, OB STRECKENABSCHNITT INNERORTS       *)
     (*                 ODER AUSSERORTS VERLAEUFT.                    *)
     (*   GESCHWABSCHL: GESCHWINDIGKEITSABSCHLAEGE JE VERK.-SIT       *)
     (*                 AUF DIESEM STRECKENABSCHNITT.                 *)
     (*   DISTANZ:      DIE DISTANZ ZUM NAECHSTEN KNOTEN.             *)
     (*   WERTEOK UND VKPGEPRUEFT:                                    *)
     (*                 HILFSFELDER FUER DIE EDITIERUNG, SIND         *)
     (*                 NORMALERWEISE TRUE.                           *)
     (*   ZEITABANFANG, BELEGT, ZEITBISHIER:                          *)
     (*                 FELDER FUER DIE ROUTENSUCHE. HIER WERDEN      *)
     (*                 ZWISCHENZEITEN GESPEICHERT.                   *)
     (*                                                               *)
     (*   -STRASSE- ENTHAELT DIE ANGABEN JE STRASSE, NAEMLICH:        *)
     (*                                                               *)
     (*   DIE KENNUNG IN DEN FELDERN -KENNUNG-, -NUMMER- UND          *)
     (*   -RICHTUNG- SOWIE DEN ANFANG UND DAS ENDE DER KETTE          *)
     (*   IN DEN FELDERN -ANFANG- UND -ENDE-.                         *)
     (*                                                               *)
     (*****************************************************************)

     STPTYP = -> STPUNKT ;
     STPUNKT = record
                 VKPUNKT : INTEGER ;
                 NEXT : STPTYP ;
                 KAT : KATEGORIEN ;
                 INNERORTS : BOOLEAN ;
                 GESCHWABSCHL : array [ SITUATIONEN ] of INTEGER ;
                 DISTANZ : INTEGER ;
                 WERTEOK : BOOLEAN ;
                 VKPGEPRUEFT : BOOLEAN ;
                 ZEITABANFANG : REAL ;
                 BELEGT : BOOLEAN ;
                 ZEITBISHIER : REAL ;
                 VKPSTART : INTEGER ;
               end ;
     STRASSE = record
                 ANFANG : STPTYP ;
                 ENDE : STPTYP ;
                 KENNUNG : CHAR ;
                 NUMMER : INTEGER ;
                 RICHTUNG : ( HIN , RUECK )
               end ;

     (***************************************************************)
     (*   HIER BEGINNEN DIE DATENTYPEN, DIE ZUR AUFNAHME DES        *)
     (*   OEV-MODELLS DIENEN: EINBINDUNGEN, TEILSTRECKEN, LINIEN.   *)
     (***************************************************************)

     EPTYP = -> EINB ;

     (****************************************)
     (*   LISTE VON EINBINDUNGEN JE BEZIRK   *)
     (*                                      *)
     (*   PRO EINBINDUNG WIRD ABGELEGT:      *)
     (*   DIE HALTESTELLE UND                *)
     (*   DIE FUSSWEGDAUER IN MINUTEN        *)
     (****************************************)

     EINB = record
              HSTELLE : INTEGER ;
              NEXT : EPTYP ;
              FW : INTEGER
            end ;

     (***********************************************************)
     (*   LISTE VON TEILSTRECKEN JE AUSGANGSHALTESTELLE         *)
     (*                                                         *)
     (*   JE TEILSTRECKE WIRD ABGELEGT:                         *)
     (*   DIE ZIELHALTESTELLE,                                  *)
     (*   DIE DURCHSCHNITTLICHE FAHRZEIT (WIRD ERMITTELT),      *)
     (*   DIE MENGE VON LINIEN AUF DIESER TEILSTRECKE           *)
     (*      (WIRD ERMITTELT),                                  *)
     (*   DIE ANZAHL DER LINIEN AUF DIESER TEILSTRECKE          *)
     (*      (WIRD EBENFALLS ERMITTELT).                        *)
     (*                                                         *)
     (*   DAS FELD SBEL WIRD HIER NICHT BENUTZT.                *)
     (***********************************************************)

     HPTYP = -> HALTSTELLE ;
     HALTSTELLE = record
                    HSTELLE : INTEGER ;
                    SFAHRZEIT : REAL ;
                    SLINSET : set of LTYP ;
                    SANZLIN : INTEGER ;
                    NEXT : HPTYP ;
                    SBEL : INTEGER
                  end ;

     (***********************************************)
     (*   DARSTELLUNG VON LINIEN                    *)
     (*                                             *)
     (*   DIE LINIEN WERDEN ALS GESCHLOSSENE        *)
     (*   RINGLISTE VON LINIENPUNKTEN DARGESTELLT.  *)
     (*   JE LINIENPUNKT WIRD ABGELEGT:             *)
     (*   DIE HALTESTELLE,                          *)
     (*   DIE FAHRZEIT AUF DER TEILSTRECKE.         *)
     (*                                             *)
     (*   DAS FELD LBEL WIRD HIER NICHT BENUTZT.    *)
     (***********************************************)

     LEPTYP = -> LINIENELEMENT ;
     LINIENELEMENT = record
                       HSTELLE : INTEGER ;
                       STRECKE : LEPTYP ;
                       FZ : INTEGER ;
                       LBEL : INTEGER
                     end ;

     (*****************************************************************)
     (*************************************************************** *)
     (*****************************************************************)
     (*   DIE NUN FOLGENDEN TYPEN WERDEN ALS HILFSTYPEN BEI DEN       *)
     (*                                                               *)
     (*   ROUTENSUCHE-ALGORITHMEN GEBRAUCHT. SIE ENTHALTEN GEFUNDENE  *)
     (*                                                               *)
     (*   WEGE, ZWISCHENERGEBNISSE USW. (SIEHE -IVROUTSUCH-,          *)
     (*                                                               *)
     (*   -OEVROUTSUCH-)                                              *)
     (*                                                               *)
     (*************************************************************** *)
     (*****************************************************************)
     (*****************************************************************)

     OEVSUCHP = -> OEVSUCHELEM ;
     OEVSUCHELEM = record
                     HSTEIGEN : HTYP ;
                     LINIEEIGEN : LTYP ;
                     VORHANDEN : BOOLEAN ;
                     BELEGT : BOOLEAN ;
                     HSTALT : HTYP ;
                     LINIEALT : LTYP ;
                     HSTEING : HTYP ;
                     EINGEST : BOOLEAN ;
                     BERBEDHF : REAL ;
                     BERWARTEZEIT : REAL ;
                     ZEITWARTNEU : REAL ;
                     ZEITEINF : REAL ;
                     ZEITGES : REAL ;
                     NEXTZEILE : OEVSUCHP ;
                     NEXTSPALTE : OEVSUCHP
                   end ;
     OEVKOPFFELD = record
                     MARKIERT : BOOLEAN ;
                     NEUMARKIERT : BOOLEAN ;
                     ZEITBELEGT : BOOLEAN ;
                     ZEITGESMIN : REAL
                   end ;

     (*****************************************************************)
     (*                                                               *)
     (*   FUER DIE OEV-ROUTENSUCHE WIRD EIN ZWEIDIMENSIONALES ARRAY   *)
     (*   IM HAUPTSPEICHER AUFGEBAUT, DAS JE GEORDNETEM PAAR AUS      *)
     (*   HALTESTELLE UND LINIE EIN ELEMENT VOM TYP -OEVSUCHELEM-     *)
     (*   ENTHAELT. DIE BEDEUTUNG DER EINZELNEN KOMPONENTEN KANN      *)
     (*   HIER NUR KURZ SKIZZIERT WERDEN; GENAUER STEHT DAS IN DER    *)
     (*   DOKUMENTATION UND IN DER PROZEDUR -OEVROUTSUCH-.            *)
     (*                                                               *)
     (*   (NACHTRAG JUNI 1985: DAS ZWEIDIMENSIONALE ARRAY WIRD        *)
     (*   AUS SPEICHERPLATZGRUENDEN NICHT MEHR ALS SOLCHES DAR-       *)
     (*   GESTELLT, SONDERN DURCH EINE POINTERSTRUKTUR. DABEI         *)
     (*   WERDEN NUR SOLCHE ELEMENTE ABGELEGT, DIE TATSAECHLICH       *)
     (*   AUCH BENOETIGT WERDEN. SIEHE HIERZU PROZEDUR                *)
     (*   -OEVSUCHTABINIT).                                           *)
     (*                                                               *)
     (*   VORHANDEN:     GIBT AN, OB DAS ELEMENT UEBERHAUPT           *)
     (*                  VORKOMMT (D.H. OB DIE INDEX-HALTESTELLE      *)
     (*                  VON DER INDEX-LINIE BEFAHREN WIRD).          *)
     (*   BELEGT:        GIBT AN, OB DAS ELEMENT BELEGT IST.          *)
     (*   HSTALT, LINIEALT:                                           *)
     (*                  ENTHALTEN DAS LETZTE ELEMENT, VON DEM        *)
     (*                  AUS DIESES ERREICHT WURDE                    *)
     (*                  (ENTWEDER ANDERE HALTESTELLE BEI NORMALER    *)
     (*                  FAHRT ODER ANDERE LINIE BEIM UMSTEIGEN)      *)
     (*   HSTEING:       HALTESTELLE, BEI DER EINGESTIEGEN WURDE.     *)
     (*   EINGEST:       GIBT AN, OB DORT EINGESTIEGEN ODER           *)
     (*                  UMGESTIEGEN WURDE.                           *)
     (*   BERBEDHF:      BISHER BERECHNETES BEDIENUNGSZEITINTERVALL   *)
     (*   BERWARTEZEIT:  BISHER BERECHNETE WARTEZEIT                  *)
     (*   ZEITWARTNEU:   NEU HINZUGEKOMMENE WARTEZEIT                 *)
     (*   ZEITEINF:      NEU HINZUGEKOMMENE FAHRZEIT (ODER FUSSWEG)   *)
     (*   ZEITGES:       ZEIT GESAMT BIS HIERHER.                     *)
     (*                                                               *)
     (*   (NACHTRAG JUNI 1985:                                        *)
     (*   AUS DEM OBEN ERWAEHNTEN GRUND MUSSTE DIE RECORDSTRUKTUR     *)
     (*   DIESER FELDER UM FOLGENDE KOMPONENTEN ERWEITERT WERDEN:     *)
     (*                                                               *)
     (*   HSTEIGEN:      DIE NUMMER DER INDEX-HALTESTELLE DIESES      *)
     (*                  RECORDS,                                     *)
     (*   LINIEEIGEN:    DIE NUMMER DER INDEX-LINIE,                  *)
     (*   NEXTZEILE:     VERWEIS AUF DAS NAECHSTE ELEMENT DERSELBEN   *)
     (*                  ZEILE,                                       *)
     (*   NEXTSPALTE:    VERWEIS AUF DAS NAECHSTE ELEMENT DERSELBEN   *)
     (*                  SPALTE.)                                     *)
     (*                                                               *)
     (*   JE HALTESTELLE GIBT ES FUER DIESES 2-DIMENSIONALE ARRAY     *)
     (*   NOCH EINEN KOPFEINTRAG (-OEVKOPFFELD-). DIE BEDEUTUNG       *)
     (*   DER DORT ENTHALTENEN FELDER:                                *)
     (*                                                               *)
     (*   MARKIERT, NEUMARKIERT:                                      *)
     (*                  HILFSFELDER FUER DIE ROUTENSUCHE.            *)
     (*   ZEITBELEGT:    GIBT AN, OB DAS FELD -ZEITGESMIN- BELEGT     *)
     (*                  IST ODER NICHT.                              *)
     (*   ZEITGESMIN:    DIE MINIMALE ZEIT FUER DIESE HALTESTELLE     *)
     (*                  (D.H. DIE KLEINSTE ZEIT -ZEITGES- AUS        *)
     (*                  DIESER SPALTE VON -OEVSUCHTAB-)              *)
     (*                                                               *)
     (*****************************************************************)

     IVSUCHELEM = record
                    BELEGT : BOOLEAN ;
                    MARKIERT : BOOLEAN ;
                    NEUMARKIERT : BOOLEAN ;
                    ZEITGES : REAL
                  end ;
     SLISTTYP = -> SLISTELEM ;
     SLISTELEM = record
                   VPSTRASSE : STRTYP ;
                   VPSTRPOINT : STPTYP ;
                   NEXT : SLISTTYP
                 end ;

     (*****************************************************************)
     (*                                                               *)
     (*   FUER DIE IV-ROUTENSUCHE WIRD EINE TABELLE JE KNOTENPUNKT    *)
     (*   ANGELEGT, DEREN ELEMENTE VOM TYP -IVSUCHELEM- SIND.         *)
     (*   DIE BEDEUTUNG DER FELDER:                                   *)
     (*                                                               *)
     (*   BELEGT:        GIBT AN, OB DAS FELD BELEGT IST.             *)
     (*   MARKIERT, NEUMARKIERT:                                      *)
     (*                  HILFSFELDER FUER DIE IV-ROUTENSUCHE.         *)
     (*   ZEITGES:       DIE (MINIMALE) ZEIT FUER DAS ERREICHEN       *)
     (*                  DIESES KNOTENPUNKTES.                        *)
     (*                                                               *)
     (*   ZUR BESCHLEUNIGUNG DER IV-ROUTENSUCHE WIRD JE KNOTEN-       *)
     (*   PUNKT EINE LISTE AUS ELEMENTEN VOM TYP -SLISTELEM-          *)
     (*   ANGELEGT. JEDES ELEMENT VERWEIST AUF EINE STRASSE,          *)
     (*   DIE DEN BETREFFENDEN KNOTENPUNKT ENTHAELT                   *)
     (*   (-VPSTRASSE-) UND INNERHALB DER STRASSE AUF DEN             *)
     (*   ENTSPRECHENDEN KNOTENPUNKT (-VPSTRPOINT-).                  *)
     (*                                                               *)
     (*****************************************************************)

     TABSTEUELEM = record
                     UNTERGRENZE : REAL ;
                     UGDAZU : BOOLEAN
                   end ;
     TABSTEUFELDTYP = array [ 1 .. TABZ ] of TABSTEUELEM ;
     TABFELDELEM = record
                     FAHRTENANZAHL : INTEGER ;
                     ZEITGEWINN : REAL
                   end ;
     TABFELDTYP = array [ 1 .. TABZ ] of TABFELDELEM ;

     (*****************************************************************)
     (*                                                               *)
     (*   DIE HIER BESCHRIEBENEN TYPEN DIENEN ZUR AUFNAHME DER        *)
     (*   INFORMATIONEN FUER DIE TABELLE DER REISEZEITGEWINNE,        *)
     (*   DIE AUF DEM LETZTEN BLATT BEI MODUS 3 ANGEDRUCKT WIRD.      *)
     (*                                                               *)
     (*   JE ZEILE DIESER TABELLE WIRD FESTGELEGT:                    *)
     (*                                                               *)
     (*    - DIE UNTERGRENZE DES REISEZEITGEWINNS FUER DIE            *)
     (*      BETREFFENDE ZEILE (UNTERGRENZE),                         *)
     (*    - DIE ANGABE, OB DIE UNTERGRENZE ZUM INTERVALL DAZU-       *)
     (*      GEHOERT ODER NICHT (GESCHLOSSENES BZW. OFFENES           *)
     (*      INTERVALL),                                              *)
     (*    - DIE FAHRTENANZAHL IN DER BETREFFENDEN ZEITGEWINN-        *)
     (*      KLASSE,                                                  *)
     (*    - UND DER ZEITGEWINN (SUMMARISCH) IN DER BETREFFENDEN      *)
     (*      ZEILE.                                                   *)
     (*                                                               *)
     (*****************************************************************)



var SITTEXTE : array [ SITUATIONEN ] of packed array [ 1 .. 6 ] of CHAR
               ;
    SITMIN : SITUATIONEN ;
    SITMAX : SITUATIONEN ;
    SITAKTUELL : SITUATIONEN ;
    TAGESZEIT : 1 .. DIFF ;
    RICHTIG : BOOLEAN ;
    TESTMODUS : BOOLEAN ;
    MODWAHL : INTEGER ;

    (****************************************************)
    (*   VERSCHIEDENE HILFSVARIABLEN                    *)
    (*                                                  *)
    (*   DIE BEDEUTUNG DER WICHTIGSTEN:                 *)
    (*   KLEINBUCHST:  ALLE KLEINEN BUCHSTABEN          *)
    (*   SITAKTUELL:   GUELTIGE VERKEHRSSITUATION       *)
    (*   TAGESZEIT:    (WIE SITAKTUELL)                 *)
    (*   TESTMODUS:    NORMALERWEISE FALSE.             *)
    (*                 BEI TRUE WERDEN DIE GEFUNDENEN   *)
    (*                 WEGE IN DIE DATEI -TESTDRUCK-    *)
    (*                 AUSGEGEBEN.                      *)
    (*   MODWAHL:      AKTUELLER MODUS; KOMMT AUS       *)
    (*                 DEN STARTE-PARAMETERN            *)
    (****************************************************)

    SX : INTEGER ;
    SSTRING : packed array [ 1 .. 80 ] of CHAR ;

    (****************************************************)
    (*   SSTRING ENTHAELT DIE STARTE-PARAMETER AUS      *)
    (*   DER -MSAUS2 EXEC-. SIE WERDEN DURCH DIE        *)
    (*   (NONSTANDARD-)PROZEDUR PARMS GEHOLT.           *)
    (*   SSTRING ENTHAELT AN PROGRAMMRELEVANTEN         *)
    (*   INFORMATIONEN:                                 *)
    (*   - DEN MODUS,                                   *)
    (*   - DIE TAGESZEIT                                *)
    (*   SOWIE, FUER DIE UEBERSCHRIFT:                  *)
    (*   - DIE NAMEN DER BETEILIGTEN MODELL-DATEIEN     *)
    (****************************************************)

    OEVNAME , IVNAME , MSFNAME : FNAME ;
    OEVALTNAME , IVALTNAME , OEVNEUNAME , IVNEUNAME : FNAME ;
    OEVQBEZALTVORH : BOOLEAN ;
    OEVQBEZALT : BTYP ;
    IVQBEZALTVORH : BOOLEAN ;
    IVQBEZALT : BTYP ;

    (******************************************************)
    (*   DIE WERTE VOM TYP -FNAME- ENTHALTEN DIE NAMEN    *)
    (*   DER BETEILIGTEN MODELL-DATEIEN FUER DIE          *)
    (*   UEBERSCHRIFT (AUS SSTRING).                      *)
    (*   DIE FELDER OEVQBEZALTVORH BIS IVQBEZALT DIENEN   *)
    (*   ZUR BESCHLEUNIGUNG DER ROUTENSUCHE BEI           *)
    (*   AUFEINANDERFOLGENDEN AUFRUFEN MIT GLEICHEM       *)
    (*   QUELLBEZIRK.                                     *)
    (******************************************************)

    KENNMENGE : set of CHAR ;
    RICHTMENGE : set of CHAR ;
    ZEILZAHL : INTEGER ;
    SEITENZAHL : INTEGER ;

    (******************************************************)
    (*   WEITERE HILFSVARIABLEN:                          *)
    (*                                                    *)
    (*   - KENNMENGE:   MENGE DER KENNUNGEN               *)
    (*                  (A,B,K,L,S)                       *)
    (*   - RICHTMENGE:  MENGE DER RICHTUNGEN (H UND R)    *)
    (*   - ZEILZAHL, SEITENZAHL:                          *)
    (*                  FUER DRUCKAUSGABE.                *)
    (******************************************************)

    VMAX : array [ KATEGORIEN ] of INTEGER ;

    (***********************************************)
    (*   MAXIMALE GESCHWINDIGKEITEN JE KATEGORIE   *)
    (***********************************************)

    BZAHL : BTYP ;

    (********************************************************)
    (*   ANZAHL BEZIRKE                                     *)
    (*   AB HIER: VARIABLEN ZUR AUFNAHME DES IV-MODELLS     *)
    (********************************************************)

    VPZAHL : VPTYP ;

    (***************************)
    (*   ANZAHL KNOTENPUNKTE   *)
    (***************************)

    STRZAHL : STRTYP ;

    (***************************)
    (*   ANZAHL STRASSEN       *)
    (***************************)

    ANSCHLUESSE : array [ BTYP ] of APTYP ;
    BTRANSFER : array [ BTYP ] of INTEGER ;

    (*****************************************)
    (*   BEZIRKSBEZOGENE DATEN:              *)
    (*   ANSCHLUESSE, BEZIRKSCODIERUNG       *)
    (*****************************************)

    VPKOORD : array [ VPTYP ] of POSITION ;
    VPSTRLISTE : array [ VPTYP ] of SLISTTYP ;
    VPTRANSFER : array [ VPTYP ] of INTEGER ;
    IVSUCHTAB : array [ VPTYP ] of IVSUCHELEM ;

    (***************************************************)
    (*   KNOTENPUNKT-BEZOGENE DATEN:                   *)
    (*   KOORDINATEN, SLISTE, CODIERUNG, SUCHTABELLE   *)
    (***************************************************)

    STRASSENNETZ : array [ STRTYP ] of STRASSE ;

    (*****************************)
    (*   STRASSENNETZ            *)
    (*****************************)

    HZAHL : HTYP ;

    (********************************************************)
    (*   ANZAHL HALTESTELLEN                                *)
    (*   AB HIER: VARIABLEN, DIE DAS OEV-MODELL AUFNEHMEN   *)
    (********************************************************)

    LZAHL : LTYP ;

    (***************************)
    (*   ANZAHL LINIEN         *)
    (***************************)

    EINBIND : array [ BTYP ] of EPTYP ;

    (****************************************)
    (*   LISTE DER EINBINDUNGEN JE BEZIRK   *)
    (****************************************)

    HPOS : array [ HTYP ] of POSITION ;
    HTEXTE : array [ HTYP ] of HTEXT ;
    STRECKENNETZ : array [ HTYP ] of HPTYP ;
    OEVSUCHSPALTE : array [ HTYP ] of OEVSUCHP ;
    OEVSUCHZEILE : array [ LTYP ] of OEVSUCHP ;
    OEVKOPFTAB : array [ HTYP ] of OEVKOPFFELD ;
    HTRANSFER : array [ HTYP ] of INTEGER ;

    (**********************************************************)
    (*   HALTESTELLENBEZOGENE DATEN:                          *)
    (*   KOORDINATEN, TEXTE, TEILSTRECKEN, SUCHTABELLE MIT    *)
    (*   KOPFEINTRAG, HALTESTELLENCODIERUNG.                  *)
    (**********************************************************)

    LINIEN : array [ LTYP ] of LEPTYP ;
    BH : array [ LTYP , 1 .. DIFF ] of INTEGER ;
    LTRANSFER : array [ LTYP ] of INTEGER ;

    (*************************************************)
    (*   LINIENBEZOGENE DATEN:                       *)
    (*   ANKER DER JEW. RINGLISTE, TABELLE DER       *)
    (*   BEDIENUNGSZEITINTERVALLE, LINIENCODIERUNG.  *)
    (*************************************************)

    ANKER : MSFPVERW ;

    (*********************************************)
    (*   ANKER FUER DIE KETTE DER STUETZPUNKTE   *)
    (*   DER MODAL-SPLIT-FUNKTION.               *)
    (*********************************************)

    MSFILE : TEXT ;
    AUSGABE : TEXT ;
    TESTDRUCK : TEXT ;
    NEUFILE : TEXT ;
    EINGABE : TEXT ;
    IVMODELL : TEXT ;
    OEVMODELL : TEXT ;

    (*******************************************************)
    (*   DEKLARATION DER BETEILIGTEN FILES ALS TEXTFILES   *)
    (*******************************************************)




procedure WRITEREAL ( var AUSGABE : TEXT ; WERT : REAL ; STELLEN :
                    INTEGER ; NACHKOMMA : INTEGER ) ;

(*********************************************************)
(*                                                       *)
(*   DIE PROZEDUR WRITEREAL DIENT DAZU, REAL-ZAHLEN      *)
(*   KORREKT AUSZUGEBEN.                                 *)
(*                                                       *)
(*   PROBLEM: DURCH EINE EIGENART DES IBM-COMPILERS      *)
(*            WERDEN REAL-WERTE, DIE GENAU GLEICH        *)
(*            NULL SIND, IMMER IN DER FORM  0.0          *)
(*            AUSGEGEBEN, UNABHAENGIG VON DER ANGE-      *)
(*            GEBENEN LAENGE.                            *)
(*                                                       *)
(*   DIE PROZEDUR WRITEREAL LOEST DIESES PROBLEM,        *)
(*   INDEM SIE REAL-ZAHLEN AUF DIE DURCH DEN             *)
(*   PARAMETER -STELLEN- VORGEGEBENE GENAUIGKEIT         *)
(*   RUNDET UND IM FALLE NULL SELBST DAS VORGESEHENE     *)
(*   DRUCKBILD ERZEUGT.                                  *)
(*                                                       *)
(*********************************************************)


   var ZEHNER : INTEGER ;
       INTWERT : INTEGER ;
       I : INTEGER ;

   begin (* WRITEREAL *)
     if ABS ( WERT ) > 10.0 then
       WRITE ( AUSGABE , WERT : STELLEN : NACHKOMMA )
     else
       begin
         ZEHNER := 1 ;
         for I := 1 to NACHKOMMA do
           ZEHNER := ZEHNER * 10 ;
         INTWERT := ROUND ( WERT * ZEHNER ) ;
         WERT := INTWERT / ZEHNER ;
         if WERT <> 0.0 then
           WRITE ( AUSGABE , WERT : STELLEN : NACHKOMMA )
         else
           begin
             WRITE ( AUSGABE , '0.' : STELLEN - NACHKOMMA ) ;
             for I := 1 to NACHKOMMA do
               WRITE ( AUSGABE , '0' )
           end (* else *)
       end (* else *)
   end (* WRITEREAL *) ;



procedure MSFINIT ;

(******************************************************)
(*                                                    *)
(*   LIEST DIE MS-FUNKTION VON DEM FILE MSFILE EIN.   *)
(*   DIE DARSTELLUNG ENTSPRICHT DER DER MODELL-       *)
(*   DATEIEN FUER IV UND OEV (IN DEN ERSTEN SECHS     *)
(*   STELLEN STEHT EINE KENNUNG). ES GIBT HIER        *)
(*   JEDOCH NUR EINE ART VON DATENSAETZEN.            *)
(*   SIE WERDEN DURCH 'FPUNKT' GEKENNZEICHNET.        *)
(*                                                    *)
(******************************************************)


   var KENN : packed array [ 1 .. 6 ] of CHAR ;
       DUMMY : MSFPUNKT ;
       P : MSFPVERW ;


   procedure READKENN ;

   (***************************************************)
   (*  LESEN DER KENNUNG IN DEN ERSTEN SECHS SPALTEN  *)
   (***************************************************)


      var I : INTEGER ;

      begin (* READKENN *)
        for I := 1 to 6 do
          READ ( MSFILE , KENN [ I ] )
      end (* READKENN *) ;


   begin (* MSFINIT *)
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
   end (* MSFINIT *) ;



procedure SEARCHX ( XSUCH : INTEGER ; var GEFUNDEN : BOOLEAN ; var P :
                  MSFPVERW ; var PVOR : MSFPVERW ) ;

(******************************************************)
(*                                                    *)
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
(*                                                    *)
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



procedure SEARCHY ( YSUCH : REAL ; var P : MSFPVERW ; var PVOR :
                  MSFPVERW ) ;

(******************************************************)
(*                                                    *)
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
(*                                                    *)
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
(*                                                  *)
(*   ERRECHNET ANHAND DER KETTE VON STUETZPUNKTEN   *)
(*   DEN FUNKTIONSWERT FUER EIN BEL. ARGUMENT.      *)
(*   ZWISCHEN DEN STUETZPUNKTEN WIRD LINEAR         *)
(*   INTERPOLIERT.                                  *)
(*                                                  *)
(****************************************************)


   var XINT : INTEGER ;
       P , PVOR : MSFPVERW ;
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
(*                                                  *)
(*   ERRECHNET ANHAND DER KETTE VON STUETZPUNKTEN   *)
(*   DAS ARGUMENT FUER EINEN BEL. FUNKTIONSWERT.    *)
(*   WENN DER FUNKTIONSWERT NICHT VORKOMMT, WIRD    *)
(*   - 1000.0 UEBERGEBEN.                           *)
(*                                                  *)
(****************************************************)


   var P , PVOR : MSFPVERW ;

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



function BRTRANS ( X : INTEGER ) : BTYP ;

(*****************************************************)
(*   EXTERNE NUMMER -> INTERNE NUMMER VON BEZIRKEN   *)
(*****************************************************)


   var K , I , J : INTEGER ;

   begin (* BRTRANS *)
     I := 1 ;
     J := BZAHL ;
     BRTRANS := 0 ;
     repeat
       K := ( I + J ) DIV 2 ;
       if X > BTRANSFER [ K ] then
         I := K + 1
       else
         J := K - 1
     until ( BTRANSFER [ K ] = X ) or ( I > J ) ;
     if BTRANSFER [ K ] = X then
       BRTRANS := K
   end (* BRTRANS *) ;



function HRTRANS ( X : INTEGER ) : HTYP ;

(*********************************************************)
(*   EXTERNE NUMMER -> INTERNE NUMMER BEI HALTESTELLEN   *)
(*********************************************************)


   var K , I , J : INTEGER ;

   begin (* HRTRANS *)
     I := 1 ;
     J := HZAHL ;
     HRTRANS := 0 ;
     repeat
       K := ( I + J ) DIV 2 ;
       if X > HTRANSFER [ K ] then
         I := K + 1
       else
         J := K - 1
     until ( HTRANSFER [ K ] = X ) or ( I > J ) ;
     if HTRANSFER [ K ] = X then
       HRTRANS := K
     else
       begin
         WRITELN ( 'DATENFEHLER, ABBRUCH !!' ) ;
         HALT
       end (* else *)
   end (* HRTRANS *) ;



procedure OEVINIT ;

(****************************************************************)
(*                                                              *)
(*   OEV-MODELL VON FILES EINLESEN                              *)
(*                                                              *)
(*   DIE MODELLPARAMETER WERDEN VOM FILE -OEVMODELL- EINGE-     *)
(*   LESEN. DIE ERSTEN 6 STELLEN JEDER ZEILE ENTHALTEN EINE     *)
(*   KENNUNG, DIE ANGIBT, UM WELCHE ART VON PARAMETERN ES       *)
(*   SICH IN DEM BETREFFENDEN SATZ HANDELT.                     *)
(*                                                              *)
(*   SIEHE AUCH PROZEDUR INIT IM PROGRAMM OEVMODED.             *)
(*                                                              *)
(****************************************************************)


   var I , K : INTEGER ;
       KENN : packed array [ 1 .. 6 ] of CHAR ;
       CH : CHAR ;
       IX : INTEGER ;


   procedure READKENN ;

   (*********************************************************)
   (*   LIEST KENNUNG IM OEV-MODELLFILE (ERSTE 6 STELLEN)   *)
   (*********************************************************)


      var I : INTEGER ;

      begin (* READKENN *)
        for I := 1 to 6 do
          READ ( OEVMODELL , KENN [ I ] )
      end (* READKENN *) ;


   procedure LOADE ;

   (**************************)
   (*   EINBINDUNGEN LADEN   *)
   (**************************)


      var AKT , AKT1 , DUMMY : EPTYP ;
          I : INTEGER ;
          EFILE : EINB ;

      begin (* LOADE *)
        for I := 1 to BZAHL do
          begin
            READ ( OEVMODELL , EFILE . HSTELLE ) ;
            READ ( OEVMODELL , EFILE . FW ) ;
            AKT := NIL ;
            if EFILE . HSTELLE <> 0 then
              begin
                NEW ( DUMMY ) ;
                DUMMY -> := EFILE ;
                DUMMY -> . HSTELLE := HRTRANS ( DUMMY -> . HSTELLE ) ;
                AKT := DUMMY ;
                AKT1 := AKT ;
                READLN ( OEVMODELL ) ;
                READKENN ;
                READ ( OEVMODELL , EFILE . HSTELLE ) ;
                READ ( OEVMODELL , EFILE . FW ) ;
                while EFILE . HSTELLE <> 0 do
                  begin
                    NEW ( DUMMY ) ;
                    DUMMY -> := EFILE ;
                    DUMMY -> . HSTELLE := HRTRANS ( DUMMY -> . HSTELLE
                                          ) ;
                    AKT1 -> . NEXT := DUMMY ;
                    AKT1 := AKT1 -> . NEXT ;
                    READLN ( OEVMODELL ) ;
                    READKENN ;
                    READ ( OEVMODELL , EFILE . HSTELLE ) ;
                    READ ( OEVMODELL , EFILE . FW ) ;
                  end (* while *) ;
                AKT1 -> . NEXT := NIL ;
                EINBIND [ I ] := AKT ;
                READLN ( OEVMODELL ) ;
                READKENN ;
              end (* then *)
            else
              begin
                EINBIND [ I ] := NIL ;
                READLN ( OEVMODELL ) ;
                READKENN ;
              end (* else *)
          end (* for *)
      end (* LOADE *) ;


   procedure LOADS ;

   (**************************)
   (*   STRECKENNETZ LADEN   *)
   (**************************)


      var AKT , AKT1 , DUMMY : HPTYP ;
          I : INTEGER ;
          SFILE : HALTSTELLE ;

      begin (* LOADS *)
        for I := 1 to HZAHL do
          begin
            READ ( OEVMODELL , SFILE . HSTELLE ) ;
            READ ( OEVMODELL , SFILE . SBEL ) ;
            AKT := NIL ;
            if SFILE . HSTELLE <> 0 then
              begin
                NEW ( DUMMY ) ;
                DUMMY -> := SFILE ;
                DUMMY -> . HSTELLE := HRTRANS ( DUMMY -> . HSTELLE ) ;
                DUMMY -> . SFAHRZEIT := 0.0 ;
                DUMMY -> . SLINSET := [ ] ;
                DUMMY -> . SANZLIN := 0 ;
                AKT := DUMMY ;
                AKT1 := AKT ;
                READLN ( OEVMODELL ) ;
                READKENN ;
                READ ( OEVMODELL , SFILE . HSTELLE ) ;
                READ ( OEVMODELL , SFILE . SBEL ) ;
                while SFILE . HSTELLE <> 0 do
                  begin
                    NEW ( DUMMY ) ;
                    DUMMY -> := SFILE ;
                    DUMMY -> . HSTELLE := HRTRANS ( DUMMY -> . HSTELLE
                                          ) ;
                    DUMMY -> . SFAHRZEIT := 0.0 ;
                    DUMMY -> . SLINSET := [ ] ;
                    DUMMY -> . SANZLIN := 0 ;
                    AKT1 -> . NEXT := DUMMY ;
                    AKT1 := AKT1 -> . NEXT ;
                    READLN ( OEVMODELL ) ;
                    READKENN ;
                    READ ( OEVMODELL , SFILE . HSTELLE ) ;
                    READ ( OEVMODELL , SFILE . SBEL )
                  end (* while *) ;
                AKT1 -> . NEXT := NIL ;
                STRECKENNETZ [ I ] := AKT ;
                READLN ( OEVMODELL ) ;
                READKENN ;
              end (* then *)
            else
              begin
                STRECKENNETZ [ I ] := NIL ;
                READLN ( OEVMODELL ) ;
                READKENN ;
              end (* else *)
          end (* for *)
      end (* LOADS *) ;


   procedure LOADL ;

   (********************)
   (*   LINIEN LADEN   *)
   (********************)


      var AKT , AKT1 , DUMMY : LEPTYP ;
          I : INTEGER ;
          LFILE : LINIENELEMENT ;

      begin (* LOADL *)
        for I := 1 to LZAHL do
          begin
            READ ( OEVMODELL , LFILE . HSTELLE ) ;
            READ ( OEVMODELL , LFILE . FZ ) ;
            READ ( OEVMODELL , LFILE . LBEL ) ;
            AKT := NIL ;
            if LFILE . HSTELLE <> 0 then
              begin
                NEW ( DUMMY ) ;
                DUMMY -> := LFILE ;
                DUMMY -> . HSTELLE := HRTRANS ( DUMMY -> . HSTELLE ) ;
                AKT := DUMMY ;
                AKT1 := AKT ;
                READLN ( OEVMODELL ) ;
                READKENN ;
                READ ( OEVMODELL , LFILE . HSTELLE ) ;
                READ ( OEVMODELL , LFILE . FZ ) ;
                READ ( OEVMODELL , LFILE . LBEL ) ;
                while LFILE . HSTELLE <> 0 do
                  begin
                    NEW ( DUMMY ) ;
                    DUMMY -> := LFILE ;
                    DUMMY -> . HSTELLE := HRTRANS ( DUMMY -> . HSTELLE
                                          ) ;
                    AKT1 -> . STRECKE := DUMMY ;
                    AKT1 := AKT1 -> . STRECKE ;
                    READLN ( OEVMODELL ) ;
                    READKENN ;
                    READ ( OEVMODELL , LFILE . HSTELLE ) ;
                    READ ( OEVMODELL , LFILE . FZ ) ;
                    READ ( OEVMODELL , LFILE . LBEL ) ;
                  end (* while *) ;
                AKT1 -> . STRECKE := AKT ;
                LINIEN [ I ] := AKT ;
                READLN ( OEVMODELL ) ;
                READKENN ;
              end (* then *)
            else
              begin
                LINIEN [ I ] := NIL ;
                READLN ( OEVMODELL ) ;
                READKENN ;
              end (* else *)
          end (* for *)
      end (* LOADL *) ;


   begin (* OEVINIT *)
     for I := 0 to HMAX do
       begin
         HTRANSFER [ I ] := 0 ;
         HTEXTE [ I ] := LEERKOMM ;
         HPOS [ I ] . X := 0 ;
         HPOS [ I ] . Y := 0 ;
         STRECKENNETZ [ I ] := NIL
       end (* for *) ;
     RESET ( OEVMODELL ) ;
     HZAHL := 0 ;
     READKENN ;
     while not ( KENN <> 'HTFILE' ) do
       begin
         HZAHL := HZAHL + 1 ;
         READ ( OEVMODELL , HTRANSFER [ HZAHL ] ) ;
         READ ( OEVMODELL , HPOS [ HZAHL ] . X ) ;
         READ ( OEVMODELL , HPOS [ HZAHL ] . Y ) ;
         CH := ' ' ;
         while ( CH = ' ' ) and not EOLN ( OEVMODELL ) do
           READ ( OEVMODELL , CH ) ;
         HTEXTE [ HZAHL ] [ 1 ] := CH ;
         IX := 1 ;
         while not ( EOLN ( OEVMODELL ) ) and ( IX < 25 ) do
           begin
             READ ( OEVMODELL , CH ) ;
             IX := IX + 1 ;
             HTEXTE [ HZAHL ] [ IX ] := CH ;
           end (* while *) ;
         READLN ( OEVMODELL ) ;
         READKENN ;
       end (* while *) ;
     LOADS ;
     for I := 0 to LMAX do
       begin
         LTRANSFER [ I ] := 0 ;
         LINIEN [ I ] := NIL
       end (* for *) ;
     LZAHL := 0 ;
     while not ( KENN <> 'LTFILE' ) do
       begin
         LZAHL := LZAHL + 1 ;
         READ ( OEVMODELL , LTRANSFER [ LZAHL ] ) ;
         READLN ( OEVMODELL ) ;
         READKENN
       end (* while *) ;
     for I := 1 to LZAHL do
       begin
         for K := 1 to DIFF do
           begin
             READ ( OEVMODELL , BH [ I , K ] ) ;
             READLN ( OEVMODELL ) ;
             READKENN
           end (* for *)
       end (* for *) ;
     LOADL ;
     for I := 0 to BMAX do
       begin
         BTRANSFER [ I ] := 0 ;
         EINBIND [ I ] := NIL
       end (* for *) ;
     BZAHL := 0 ;
     while not ( KENN <> 'BTFILE' ) do
       begin
         BZAHL := BZAHL + 1 ;
         READ ( OEVMODELL , BTRANSFER [ BZAHL ] ) ;
         READLN ( OEVMODELL ) ;
         READKENN
       end (* while *) ;
     LOADE ;
     if KENN <> '*ENDE*' then
       begin
         WRITELN ;
         WRITELN ( '*** FALSCHER AUFBAU DER OEV-MODELLDATEI ***' ) ;
         WRITELN ( '*** ABBRUCH *******************************' ) ;
         WRITELN ;
         HALT
       end (* then *)
   end (* OEVINIT *) ;



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



procedure SEARCHH ( HALT : INTEGER ; var IND : HTYP ; var STATUS : STAT
                  ) ;

(***********************************************)
(*   INTERNE NUMMER EINER HALTESTELLE SUCHEN   *)
(***********************************************)


   var I : HTYP ;

   begin (* SEARCHH *)
     I := 1 ;
     while ( HTRANSFER [ I ] < HALT ) and ( I <= HZAHL ) do
       I := I + 1 ;
     if I > HZAHL then
       STATUS := 1
     else
       if HTRANSFER [ I ] = HALT then
         begin
           IND := I ;
           STATUS := 0
         end (* then *)
       else
         STATUS := 1
   end (* SEARCHH *) ;



function OEVSUCHTAB ( H : HTYP ; L : LTYP ) : OEVSUCHP ;

(****************************************************)
(*                                                  *)
(*   DIE FUNCTION OEVSUCHTAB WIRD UEBERALL DORT     *)
(*   EINGESETZT, WO IN DER ALTEN VERSION EINE       *)
(*   KOMPONENTE DES 2-DIMENSIONALEN ARRAYS          *)
(*   ANGESPROCHEN WURDE. SIE LIEFERT AUS DER        *)
(*   (IN DER NEUEN VERSION) DURCH EINE POINTER-     *)
(*   STRUKTUR REALISIERTEN MATRIX DEN VERWEIS       *)
(*   AUF DAS DURCH DIE PARAMETER -H- (HALTESTELLE)  *)
(*   UND -L- (LINIE) INDIZIERTE ELEMENT.            *)
(*                                                  *)
(****************************************************)


   var OEVP : OEVSUCHP ;

   begin (* OEVSUCHTAB *)
     OEVP := OEVSUCHSPALTE [ H ] ;
     while OEVP -> . LINIEEIGEN <> L do
       OEVP := OEVP -> . NEXTSPALTE ;
     OEVSUCHTAB := OEVP
   end (* OEVSUCHTAB *) ;



procedure OEVSUCHTABLOE ;

(****************************************************)
(*                                                  *)
(*   NACH EINER ERFOLGTEN ROUTENSUCHE WIRD DIE      *)
(*   SUCHTABELLE DURCH DIESE PROZEDUR WIEDER        *)
(*   IN DEN URZUSTAND ZURUECKVERSETZT. DIE FELDER   *)
(*   -VORHANDEN- BLEIBEN UNVERAENDERT (ALLERDINGS   *)
(*   NICHT IN DER ZEILE FUER DIE FIKTIVE LINIE      *)
(*   0; DORT GIBT -VORHANDEN- JA DIE EXISTENZ       *)
(*   EINER EINBINDUNG FUER DIESE HALTESTELLE IN     *)
(*   DEN QUELLBEZIRK AN).                           *)
(*                                                  *)
(****************************************************)


   var H : HTYP ;
       OEVP : OEVSUCHP ;

   begin (* OEVSUCHTABLOE *)
     for H := 1 to HZAHL do
       begin
         OEVKOPFTAB [ H ] . MARKIERT := FALSE ;
         OEVKOPFTAB [ H ] . NEUMARKIERT := FALSE ;
         OEVKOPFTAB [ H ] . ZEITBELEGT := FALSE ;
         OEVKOPFTAB [ H ] . ZEITGESMIN := 0.0 ;
         OEVP := OEVSUCHSPALTE [ H ] ;
         while OEVP <> NIL do
           begin
             if OEVP -> . LINIEEIGEN = 0 then
               OEVP -> . VORHANDEN := FALSE ;
             OEVP -> . BELEGT := FALSE ;
             OEVP -> . HSTALT := 0 ;
             OEVP -> . LINIEALT := 0 ;
             OEVP -> . HSTEING := 0 ;
             OEVP -> . EINGEST := FALSE ;
             OEVP -> . BERBEDHF := 0.0 ;
             OEVP -> . BERWARTEZEIT := 0.0 ;
             OEVP -> . ZEITWARTNEU := 0.0 ;
             OEVP -> . ZEITEINF := 0.0 ;
             OEVP -> . ZEITGES := 0.0 ;
             OEVP := OEVP -> . NEXTSPALTE
           end (* while *)
       end (* for *)
   end (* OEVSUCHTABLOE *) ;



procedure OEVSUCHTABINIT ;

(**************************************************************)
(*                                                            *)
(*   DIE PROZEDUR OEVSUCHTABINIT ERSTELLT DIE FUER DIE        *)
(*   OEV-ROUTENSUCHE NOTWENDIGE ZEIGERSTRUKTUR, DIE           *)
(*   IN DER NEUEN VERSION AN DIE STELLE DES (ALTEN)           *)
(*   ZWEIDIMENSIONALEN ARRAYS GETRETEN IST.                   *)
(*                                                            *)
(*   WEGEN PLATZPROBLEMEN IM (VIRTUELLEN) HAUPTSPEICHER       *)
(*   UND AUFGRUND DER ERKENNTNIS, DASS VON DEN VIELEN         *)
(*   KOMPONENTEN DES ARRAYS -OEVSUCHTAB- (ANZAHL =            *)
(*   MAX. HALTESTELLENANZAHL * MAX. LINIENANZAHL) NUR         *)
(*   DIEJENIGEN TATSAECHLICH GEBRAUCHT WERDEN, DEREN          *)
(*   INDEX-LINIE AUF DER INDEX-HALTESTELLE VERKEHRT,          *)
(*   WERDEN NUN IN DER NEUEN VERSION NUR NOCH DIE TATSAECH-   *)
(*   LICH BENOETIGTEN KOMPONENTEN ABGELEGT.                   *)
(*                                                            *)
(*   DABEI WIRD JE ZEILE UND JE SPALTE DES URSPRUENGLICHEN    *)
(*   ARRAYS EINE POINTERKETTE ABGELEGT, IN DER JEDOCH NUR     *)
(*   DIE ELEMENTE ENTHALTEN SIND, DIE TATSAECHLICH BENOETIGT  *)
(*   WERDEN. JEDE KOMPONENTE BEKOMMT ZU DIESEM ZWECK VIER     *)
(*   ZUSAETZLICHE FELDER:                                     *)
(*                                                            *)
(*    - HSTEIGEN   : DIE INDEX-HALTESTELLE                    *)
(*    - LINIEEIGEN : DIE INDEX-LINIE                          *)
(*    - NEXTZEILE  : VERWEIS AUF DAS NAECHSTE ELEMENT         *)
(*                   DERSELBEN ZEILE                          *)
(*    - NEXTSPALTE : VERWEIS AUF DAS NAECHSTE ELEMENT         *)
(*                   DERSELBEN SPALTE.                        *)
(*                                                            *)
(*   DER ANKER JEDER SOLCHEN POINTERKETTE WIRD IN ZWEI        *)
(*   ARRAYS -OEVSUCHZEILE- UND -OEVSUCHSPALTE- ABGELEGT.      *)
(*   -OEVSUCHZEILE- ENTHAELT DIE ZEILEN (EINE JE LINIE) UND   *)
(*   -OEVSUCHSPALTE- DIE SPALTEN (EINE JE HALTESTELLE).       *)
(*                                                            *)
(*   DIE PROZEDUR OEVSUCHTABINIT ERSTELLT NUN - AUSGEHEND     *)
(*   VON DEN EINGELESENEN LINIENDATEN - DIESE STRUKTUR,       *)
(*   INDEM FUER JEDES AUFTRETENDE PAAR AUS LINIE UND          *)
(*   HALTESTELLE EIN ELEMENT IN DIE STRUKTUR EINGEFUEGT       *)
(*   WIRD. DIES GESCHIEHT IN DER PROZEDUR -OEVSTINSERT-,      *)
(*   DEREN BESCHREIBUNG IM NAECHSTEN KASTEN FOLGT.            *)
(*                                                            *)
(**************************************************************)


   var L : LTYP ;
       P : LEPTYP ;
       Q : LEPTYP ;
       H : HTYP ;


   procedure OEVSTINSERT ( H : HTYP ; L : LTYP ) ;

   (*************************************************************)
   (*                                                           *)
   (*   DIE PROZEDUR OEVSTINSERT FUEGT EIN ELEMENT IN DIE       *)
   (*   OBEN BESCHRIEBENE ZEIGERSTRUKTUR EIN.                   *)
   (*                                                           *)
   (*   ZUNAECHST WIRD MIT -NEW- EIN NEUES ELEMENT DER          *)
   (*   GEWUENSCHTEN STRUKTUR ERZEUGT. GEWISSE FELDER           *)
   (*   DIESES ELEMENTES WERDEN VORBESETZT (DIE VORBESETZUNG    *)
   (*   DER RESTLICHEN ERFOLGT DURCH EINEN NACHFOLGENDEN        *)
   (*   AUFRUF VON -OEVSUCHTABLOE-). DANACH WIRD DIESES         *)
   (*   ELEMENT IN DIE KETTE DER ENTSPRECHENDEN SPALTE UND      *)
   (*   IN DIE KETTE DER ENTSPRECHENDEN ZEILE EINGEFUEGT.       *)
   (*                                                           *)
   (*   DABEI SIND BESTIMMTE SONDERFAELLE ZU BERUECKSICHTIGEN:  *)
   (*                                                           *)
   (*   - DIE SPALTE (BZW. ZEILE) IST LEER,                     *)
   (*   - DAS ELEMENT IST (INNERHALB DER SPALTE BZW. ZEILE)     *)
   (*     GROESSER ALS DAS GROESSTE ODER KLEINER ALS DAS        *)
   (*     KLEINSTE BISHER VORHANDENE,                           *)
   (*   - DAS ELEMENT IST BEREITS VORHANDEN.                    *)
   (*                                                           *)
   (*************************************************************)


      var P : OEVSUCHP ;
          Q : OEVSUCHP ;
          QVOR : OEVSUCHP ;
          ABBRUCH : BOOLEAN ;

      begin (* OEVSTINSERT *)
        NEW ( P ) ;
        P -> . HSTEIGEN := H ;
        P -> . LINIEEIGEN := L ;
        P -> . NEXTSPALTE := NIL ;
        P -> . NEXTZEILE := NIL ;
        P -> . VORHANDEN := TRUE ;
        Q := OEVSUCHSPALTE [ H ] ;
        if Q = NIL then
          OEVSUCHSPALTE [ H ] := P
        else
          if Q -> . LINIEEIGEN > L then
            begin
              OEVSUCHSPALTE [ H ] := P ;
              P -> . NEXTSPALTE := Q
            end (* then *)
          else
            begin
              ABBRUCH := FALSE ;
              QVOR := Q ;
              while ( Q <> NIL ) and not ABBRUCH do
                begin
                  if Q -> . LINIEEIGEN < L then
                    begin
                      QVOR := Q ;
                      Q := Q -> . NEXTSPALTE
                    end (* then *)
                  else
                    ABBRUCH := TRUE
                end (* while *) ;
              if Q <> NIL then
                begin
                  if Q -> . LINIEEIGEN <> L then
                    begin
                      QVOR -> . NEXTSPALTE := P ;
                      P -> . NEXTSPALTE := Q
                    end (* then *)
                end (* then *)
              else
                begin
                  QVOR -> . NEXTSPALTE := P ;
                  P -> . NEXTSPALTE := Q
                end (* else *)
            end (* else *) ;
        Q := OEVSUCHZEILE [ L ] ;
        if Q = NIL then
          OEVSUCHZEILE [ L ] := P
        else
          if Q -> . HSTEIGEN > H then
            begin
              OEVSUCHZEILE [ L ] := P ;
              P -> . NEXTZEILE := Q
            end (* then *)
          else
            begin
              ABBRUCH := FALSE ;
              QVOR := Q ;
              while ( Q <> NIL ) and not ABBRUCH do
                begin
                  if Q -> . HSTEIGEN < H then
                    begin
                      QVOR := Q ;
                      Q := Q -> . NEXTZEILE
                    end (* then *)
                  else
                    ABBRUCH := TRUE
                end (* while *) ;
              if Q <> NIL then
                begin
                  if Q -> . HSTEIGEN <> H then
                    begin
                      QVOR -> . NEXTZEILE := P ;
                      P -> . NEXTZEILE := Q
                    end (* then *)
                end (* then *)
              else
                begin
                  QVOR -> . NEXTZEILE := P ;
                  P -> . NEXTZEILE := Q
                end (* else *)
            end (* else *) ;
      end (* OEVSTINSERT *) ;


   begin (* OEVSUCHTABINIT *)
     for H := 1 to HZAHL do
       OEVSTINSERT ( H , 0 ) ;
     for L := 1 to LZAHL do
       begin
         P := LINIEN [ L ] ;
         Q := P ;
         repeat
           H := Q -> . HSTELLE ;
           OEVSTINSERT ( H , L ) ;
           Q := Q -> . STRECKE
         until Q = P
       end (* for *) ;
     OEVSUCHTABLOE
   end (* OEVSUCHTABINIT *) ;



procedure OEVROUTSUCH ( QBEZ : BTYP ; ZBEZ : BTYP ; var ZEITK : REAL ;
                      var GEFUNDEN : BOOLEAN ) ;

(**********************************************************)
(*                                                        *)
(*   HIER BEGINNT DIE OEV-ROUTENSUCHE. SIE WIRD AUS-      *)
(*   FUEHRLICH IN DER DOKUMENTATION ERLAEUTERT.           *)
(*                                                        *)
(*   (NACHTRAG JUNI 1985:                                 *)
(*   UEBERALL DA, WO IN DER FOLGE VON DEM 2-DIMENS.       *)
(*   ARRAY -OEVSUCHTAB- DIE REDE IST, IST DIE             *)
(*   IN DER NEUEN VERSION AN SEINE STELLE GETRETENE       *)
(*   ZEIGERSTRUKTUR GEMEINT. SIEHE HIERZU KOMMENTAR       *)
(*   ZU PROZEDUR OEVSUCHTABINIT)                          *)
(*                                                        *)
(*   EINE WICHTIGE ROLLE BEI DER OEV-ROUTENSUCHE SPIELT   *)
(*   DAS FELD -OEVSUCHTAB- (2-DIMENSIONALE TABELLE        *)
(*   UEBER ALLE HALTESTELLEN UND ALLE LINIEN). DER        *)
(*   AUFBAU DIESES FELDES IST IN DEN TYPEN-DEKLARA-       *)
(*   TIONEN BESCHRIEBEN.                                  *)
(*                                                        *)
(*   IM PRINZIP LAEUFT DIE ROUTENSUCHE WIE FOLGT AB:      *)
(*                                                        *)
(*   IM ERSTEN SCHRITT WERDEN ALLE DIE HALTESTELLEN       *)
(*   MARKIERT, DIE IN DEN QUELLBEZIRK EINGEBUNDEN SIND.   *)
(*                                                        *)
(*   IM ZWEITEN SCHRITT WERDEN VON JEDER MARKIERTEN       *)
(*   HALTESTELLE AUS ALLE FAHRTEN MIT ALLEN LINIEN        *)
(*   DURCHGEFUEHRT, WOBEI DIE ZEITEN BERECHNET WERDEN     *)
(*   (ZUNAECHST OHNE UMSTEIGEN). DIE DABEI NEU            *)
(*   BEFAHRENEN HALTESTELLEN WERDEN NEU MARKIERT.         *)
(*   DIE MARKIERUNGEN DER BEARBEITETEN HALTESTELLEN       *)
(*   WERDEN GELOESCHT. SCHON HIER WERDEN NUR DIE          *)
(*   FAHRTEN BERUECKSICHTIGT, DIE DIE KUERZESTE           *)
(*   ZEIT FUER EINE HALTESTELLE LIEFERN.                  *)
(*                                                        *)
(*   DIESER ZWEITE SCHRITT WIRD SOLANGE WIEDERHOLT,       *)
(*   BIS KEINE HALTESTELLEN MEHR NEU HINZUKOMMEN.         *)
(*   AUS DEN ZEITEN FUER DIE ZIELHALTESTELLEN LAESST      *)
(*   SICH DANN DIE MINIMALE ZEIT ERMITTELN.               *)
(*                                                        *)
(*   BEACHTENSWERT IST, DASS - AUSGEHEND VON EINEM        *)
(*   QUELLBEZIRK - ALLE FAHRTEN IN ALLE RICHTUNGEN        *)
(*   ERMITTELT WERDEN. BEI AUFEINANDERFOLGENDEN           *)
(*   AUFRUFEN MIT DEMSELBEN QUELLBEZIRK MUSS ALSO         *)
(*   FAST NICHTS MEHR GEMACHT WERDEN, DA DIE DATEN        *)
(*   IN DER SUCHTABELLE IMMER NOCH STIMMEN. AUS           *)
(*   DIESEM GRUND SOLLTEN DIE ANFORDERUNGEN NACH          *)
(*   QUELLBEZIRKEN SORTIERT ERFOLGEN. DER ZEITGEWINN      *)
(*   DURCH DIESE EINFACHE MASSNAHME IST IMMENS.           *)
(*                                                        *)
(**********************************************************)


   var EP : EPTYP ;
       MAXEP : EPTYP ;
       SUCHELEM : OEVSUCHELEM ;
       OEVP : OEVSUCHP ;
       ZEIT : REAL ;
       UEBRIG : BOOLEAN ;
       HST : HTYP ;
       LINSET : set of LTYP ;


   procedure SUCHEZEITEN ( HST : HTYP ; H1 : HTYP ; H2 : HTYP ; var
                         BEDHF : REAL ; var FZ : REAL ) ;

   (**************************************************)
   (*                                                *)
   (*   DIE PROZEDUR SUCHEZEITEN ERMITTELT DIE       *)
   (*   FAHRZEIT UND DIE BEDIENUNGSZEITINTERVALLE    *)
   (*   AUF EINER VORGEGEBENEN STRECKE.              *)
   (*                                                *)
   (*   PARAMETER:                                   *)
   (*                                                *)
   (*   HST    - HALTESTELLE, BEI DER DIE FAHRT      *)
   (*            BEGONNEN WURDE                      *)
   (*   H1     - AUSGANGSHALTESTELLE DER STRECKE     *)
   (*   H2     - ZIELHALTESTELLE DER STRECKE         *)
   (*   BEDHF  - ENTHAELT DAS RESULTIERENDE          *)
   (*            BEDIENUNGSZEITINTERVALL             *)
   (*   FZ     - ENTHAELT DIE FAHRZEIT               *)
   (*                                                *)
   (*   SEITENEFFEKT AUF GLOBALE VARIABLE LINSET     *)
   (*   (DEKLARIERT IN -OEVROUTSUCH-). SIE WIRD      *)
   (*   WIE EINE OWN-VARIABLE VERWENDET.             *)
   (*                                                *)
   (*   DIE RESULTIERENDE FAHRZEIT IST DIE           *)
   (*   MITTLERE FAHRZEIT ALLER AUF DEM STRECKEN-    *)
   (*   ABSCHNITT VERKEHRENDEN LINIEN (WIE VON DER   *)
   (*   PROZEDUR OEVZEITRECHNUNG ERMITTELT).         *)
   (*                                                *)
   (*   DIE BERECHNUNG DES BEDIENUNGSZEIT-           *)
   (*   INTERVALLS GESTALTET SICH ETWAS KOMPLI-      *)
   (*   ZIERT. ES WERDEN HIERZU ALLE LINIEN          *)
   (*   HERANGEZOGEN, DIE AUF DER AKTUELLEN          *)
   (*   FAHRT VON DER STARTHALTESTELLE BIS           *)
   (*   ZUM AKTUELLEN STRECKENABSCHNITT DIE          *)
   (*   GANZE ZEIT PARALLEL LAUFEN. DIE ERMITTLUNG   *)
   (*   DIESER LINIEN GESCHIEHT UEBER DIE FORT-      *)
   (*   LAUFENDE SCHNITTBILDUNG ALLER STRECKEN-      *)
   (*   BEZOGENEN LINIENMENGEN. ALLE DIESE LINIEN    *)
   (*   TRAGEN DANN ZUR ERMITTLUNG DES BEDIENUNGS-   *)
   (*   ZEITINTERVALLS BEI.                          *)
   (*                                                *)
   (*   DABEI WERDEN DIE KEHRWERTE DER BEDIENUNGS-   *)
   (*   ZEITINTERVALLE (EIN MASS FUER DIE FAHRTEN    *)
   (*   PRO ZEITEINHEIT) ADDIERT UND ERGEBEN DEN     *)
   (*   KEHRWERT DES GESAMT-BEDIENUNGSZEIT-          *)
   (*   INTERVALLS.                                  *)
   (*                                                *)
   (**************************************************)


      var ST : HPTYP ;
          LS : set of LTYP ;
          L : LTYP ;
          ERSTER : BOOLEAN ;

      begin (* SUCHEZEITEN *)
        BEDHF := 0.0 ;
        ERSTER := TRUE ;
        ST := STRECKENNETZ [ H1 ] ;
        while ST -> . HSTELLE <> H2 do
          ST := ST -> . NEXT ;
        FZ := ST -> . SFAHRZEIT ;
        if H1 = HST then
          LINSET := ST -> . SLINSET ;
        LS := ST -> . SLINSET * LINSET ;
        for L := 1 to LZAHL do
          begin
            if L in LS then
              begin
                if ERSTER then
                  begin
                    ERSTER := FALSE ;
                    BEDHF := BH [ L , TAGESZEIT ]
                  end (* then *)
                else
                  begin
                    BEDHF := 1 / BEDHF ;
                    BEDHF := BEDHF + 1 / BH [ L , TAGESZEIT ] ;
                    BEDHF := 1 / BEDHF
                  end (* else *)
              end (* then *)
          end (* for *) ;
      end (* SUCHEZEITEN *) ;


   procedure LINAUFB ( LIN : LTYP ; HST : HTYP ) ;

   (******************************************************)
   (*                                                    *)
   (*   IN DER PROZEDUR LINAUFB WIRD - AUSGEHEND VON     *)
   (*   EINER NEU ANGEFAHRENEN HALTESTELLE - AUF         *)
   (*   EINER BESTIMMTEN LINIE EINE FAHRT IN BEIDE       *)
   (*   RICHTUNGEN AUSGEFUEHRT. DIE DABEI ENTSTEHENDEN   *)
   (*   TABELLENELEMENTE WERDEN ZUNAECHST NUR TEMPORAER  *)
   (*   IN DEM LOKALEN FELD SUCHZ (DAS EINER ZEILE       *)
   (*   DER SUCHTABELLE ENTSPRICHT) ABGELEGT. ERST AM    *)
   (*   ENDE WIRD ENTSCHIEDEN, WELCHE DER NEUEN ELE-     *)
   (*   MENTE AUS -SUCHZ- IN DIE TABELLE -OEVSUCHTAB-    *)
   (*   UEBERTRAGEN WERDEN (DIE MIT KLEINEREN GESAMT-    *)
   (*   ZEITEN).                                         *)
   (*                                                    *)
   (******************************************************)


      var SUCHZ : array [ HTYP ] of OEVSUCHELEM ;
          LEERSUCH : OEVSUCHELEM ;
          SUCHELEM : OEVSUCHELEM ;
          OEVP : OEVSUCHP ;
          P , Q : LEPTYP ;
          H , H1 , H2 : HTYP ;
          BHNEU , FZ , EW , UW , WZNEU : REAL ;

      begin (* LINAUFB *)
        LEERSUCH . VORHANDEN := FALSE ;
        LEERSUCH . BELEGT := FALSE ;
        LEERSUCH . HSTALT := 0 ;
        LEERSUCH . LINIEALT := 0 ;
        LEERSUCH . HSTEING := 0 ;
        LEERSUCH . EINGEST := FALSE ;
        LEERSUCH . BERBEDHF := 0.0 ;
        LEERSUCH . BERWARTEZEIT := 0.0 ;
        LEERSUCH . ZEITWARTNEU := 0.0 ;
        LEERSUCH . ZEITEINF := 0.0 ;
        LEERSUCH . ZEITGES := 0.0 ;
        for H := 0 to HMAX do
          SUCHZ [ H ] := LEERSUCH ;
        P := LINIEN [ LIN ] ;
        while P -> . HSTELLE <> HST do
          P := P -> . STRECKE ;
        Q := P ;
        OEVP := OEVSUCHTAB ( HST , LIN ) ;
        SUCHZ [ HST ] := OEVP -> ;
        repeat
          H1 := Q -> . HSTELLE ;
          H2 := Q -> . STRECKE -> . HSTELLE ;
          SUCHELEM := SUCHZ [ H1 ] ;
          SUCHELEM . HSTALT := H1 ;
          SUCHELEM . LINIEALT := LIN ;
          SUCHEZEITEN ( HST , H1 , H2 , BHNEU , FZ ) ;
          if BHNEU > SUCHELEM . BERBEDHF then
            begin
              if SUCHELEM . EINGEST then
                if BHNEU = 0.0 then
                  WZNEU := 10000.0
                else
                  begin
                    if BHNEU <= BHOGREIN then
                      EW := EINFAKT * BHNEU / 2
                    else
                      EW := EINFAKT * BHOGREIN / 2 ;
                    WZNEU := EW
                  end (* else *)
              else
                begin
                  if BHNEU <= BHOGRUM then
                    UW := KOMFORT * UMFAKT * BHNEU / 2
                  else
                    UW := KOMFORT * UMFAKT * BHOGRUM / 2 ;
                  WZNEU := UW
                end (* else *) ;
              SUCHELEM . BERBEDHF := BHNEU ;
              SUCHELEM . ZEITWARTNEU := WZNEU - SUCHELEM . BERWARTEZEIT
                                        ;
              SUCHELEM . BERWARTEZEIT := WZNEU
            end (* then *)
          else
            SUCHELEM . ZEITWARTNEU := 0.0 ;
          SUCHELEM . ZEITEINF := FZ ;
          SUCHELEM . ZEITGES := SUCHELEM . ZEITGES + SUCHELEM .
                                ZEITEINF + SUCHELEM . ZEITWARTNEU ;
          if SUCHZ [ H2 ] . BELEGT then
            begin
              if SUCHZ [ H2 ] . ZEITGES > SUCHELEM . ZEITGES then
                SUCHZ [ H2 ] := SUCHELEM
            end (* then *)
          else
            SUCHZ [ H2 ] := SUCHELEM ;
          Q := Q -> . STRECKE
        until Q = P ;
        for H := 0 to HMAX do
          begin
            if SUCHZ [ H ] . BELEGT then
              begin
                OEVP := OEVSUCHTAB ( H , LIN ) ;
                if OEVP -> . BELEGT then
                  begin
                    if SUCHZ [ H ] . ZEITGES < OEVP -> . ZEITGES then
                      begin
                        OEVP -> . VORHANDEN := SUCHZ [ H ] . VORHANDEN
                                               ;
                        OEVP -> . BELEGT := SUCHZ [ H ] . BELEGT ;
                        OEVP -> . HSTALT := SUCHZ [ H ] . HSTALT ;
                        OEVP -> . LINIEALT := SUCHZ [ H ] . LINIEALT ;
                        OEVP -> . HSTEING := SUCHZ [ H ] . HSTEING ;
                        OEVP -> . EINGEST := SUCHZ [ H ] . EINGEST ;
                        OEVP -> . BERBEDHF := SUCHZ [ H ] . BERBEDHF ;
                        OEVP -> . BERWARTEZEIT := SUCHZ [ H ] .
                                                  BERWARTEZEIT ;
                        OEVP -> . ZEITWARTNEU := SUCHZ [ H ] .
                                                 ZEITWARTNEU ;
                        OEVP -> . ZEITEINF := SUCHZ [ H ] . ZEITEINF ;
                        OEVP -> . ZEITGES := SUCHZ [ H ] . ZEITGES ;
                        OEVKOPFTAB [ H ] . NEUMARKIERT := TRUE
                      end (* then *)
                  end (* then *)
                else
                  begin
                    OEVP -> . VORHANDEN := SUCHZ [ H ] . VORHANDEN ;
                    OEVP -> . BELEGT := SUCHZ [ H ] . BELEGT ;
                    OEVP -> . HSTALT := SUCHZ [ H ] . HSTALT ;
                    OEVP -> . LINIEALT := SUCHZ [ H ] . LINIEALT ;
                    OEVP -> . HSTEING := SUCHZ [ H ] . HSTEING ;
                    OEVP -> . EINGEST := SUCHZ [ H ] . EINGEST ;
                    OEVP -> . BERBEDHF := SUCHZ [ H ] . BERBEDHF ;
                    OEVP -> . BERWARTEZEIT := SUCHZ [ H ] .
                                              BERWARTEZEIT ;
                    OEVP -> . ZEITWARTNEU := SUCHZ [ H ] . ZEITWARTNEU
                                             ;
                    OEVP -> . ZEITEINF := SUCHZ [ H ] . ZEITEINF ;
                    OEVP -> . ZEITGES := SUCHZ [ H ] . ZEITGES ;
                    OEVKOPFTAB [ H ] . NEUMARKIERT := TRUE
                  end (* else *)
              end (* then *)
          end (* for *)
      end (* LINAUFB *) ;


   procedure HSTAUFB ( HST : HTYP ) ;

   (******************************************************)
   (*                                                    *)
   (*   FUER EINE NEU HINZUGEKOMMENE ODER VERAENDERTE    *)
   (*   HALTESTELLE WIRD ZUNAECHST DIE KLEINSTE          *)
   (*   ZEIT ERMITTELT UND DIESE - FALLS NOETIG -        *)
   (*   IN DEN KOPFEINTRAG DER ENTSPRECHENDEN            *)
   (*   HALTESTELLE UEBERTRAGEN. WENN SICH DABEI         *)
   (*   EINE VERBESSERUNG DER ZEIT ERGIBT (ODER          *)
   (*   WENN DIE HALTESTELLE BIS DAHIN NOCH GAR          *)
   (*   NICHT BETRETEN WURDE), MUESSEN DIE AUF DIESER    *)
   (*   HALTESTELLE VERKEHRENDEN LINIEN MIT DER          *)
   (*   PROZEDUR LINAUFB DURCHGERECHNET WERDEN           *)
   (*   (AUSSER DER, DURCH DIE DIE HALTESTELLE           *)
   (*   ANGEFAHREN WURDE).                               *)
   (*                                                    *)
   (******************************************************)


      var ZGMIN : REAL ;
          MINBELEGT : BOOLEAN ;
          LBEHANDELN : BOOLEAN ;
          SUCHELEM : OEVSUCHELEM ;
          L , LIN : LTYP ;
          OEVP : OEVSUCHP ;

      begin (* HSTAUFB *)
        MINBELEGT := FALSE ;
        OEVP := OEVSUCHSPALTE [ HST ] ;
        while OEVP <> NIL do
          begin
            L := OEVP -> . LINIEEIGEN ;
            if OEVP -> . VORHANDEN then
              if OEVP -> . BELEGT then
                begin
                  if not MINBELEGT then
                    begin
                      LIN := L ;
                      ZGMIN := OEVP -> . ZEITGES ;
                      MINBELEGT := TRUE
                    end (* then *) ;
                  if OEVP -> . ZEITGES < ZGMIN then
                    begin
                      LIN := L ;
                      ZGMIN := OEVP -> . ZEITGES
                    end (* then *)
                end (* then *) ;
            OEVP := OEVP -> . NEXTSPALTE
          end (* while *) ;
        LBEHANDELN := FALSE ;
        if not OEVKOPFTAB [ HST ] . ZEITBELEGT then
          begin
            OEVKOPFTAB [ HST ] . ZEITGESMIN := ZGMIN ;
            OEVKOPFTAB [ HST ] . ZEITBELEGT := TRUE ;
            LBEHANDELN := TRUE
          end (* then *)
        else
          if OEVKOPFTAB [ HST ] . ZEITGESMIN > ZGMIN then
            begin
              OEVKOPFTAB [ HST ] . ZEITGESMIN := ZGMIN ;
              LBEHANDELN := TRUE
            end (* then *) ;
        if LBEHANDELN then
          begin
            OEVP := OEVSUCHSPALTE [ HST ] ;
            while OEVP <> NIL do
              begin
                L := OEVP -> . LINIEEIGEN ;
                if ( L <> LIN ) and ( L <> 0 ) then
                  begin
                    if BH [ L , TAGESZEIT ] > 0 then
                      begin
                        if OEVP -> . VORHANDEN then
                          begin
                            SUCHELEM . VORHANDEN := TRUE ;
                            SUCHELEM . BELEGT := TRUE ;
                            SUCHELEM . HSTALT := HST ;
                            SUCHELEM . LINIEALT := LIN ;
                            SUCHELEM . HSTEING := HST ;
                            SUCHELEM . EINGEST := ( LIN = 0 ) ;
                            SUCHELEM . BERBEDHF := 0.0 ;
                            SUCHELEM . BERWARTEZEIT := 0.0 ;
                            SUCHELEM . ZEITWARTNEU := 0.0 ;
                            SUCHELEM . ZEITEINF := 0.0 ;
                            SUCHELEM . ZEITGES := ZGMIN ;
                            if not OEVP -> . BELEGT then
                              begin
                                OEVP -> . VORHANDEN := SUCHELEM .
                                                   VORHANDEN ;
                                OEVP -> . BELEGT := SUCHELEM . BELEGT ;
                                OEVP -> . HSTALT := SUCHELEM . HSTALT ;
                                OEVP -> . LINIEALT := SUCHELEM .
                                                   LINIEALT ;
                                OEVP -> . HSTEING := SUCHELEM . HSTEING
                                                   ;
                                OEVP -> . EINGEST := SUCHELEM . EINGEST
                                                   ;
                                OEVP -> . BERBEDHF := SUCHELEM .
                                                   BERBEDHF ;
                                OEVP -> . BERWARTEZEIT := SUCHELEM .
                                                   BERWARTEZEIT ;
                                OEVP -> . ZEITWARTNEU := SUCHELEM .
                                                   ZEITWARTNEU ;
                                OEVP -> . ZEITEINF := SUCHELEM .
                                                   ZEITEINF ;
                                OEVP -> . ZEITGES := SUCHELEM . ZEITGES
                                                   ;
                                LINAUFB ( L , HST )
                              end (* then *)
                            else
                              begin
                                if ZGMIN < OEVP -> . ZEITGES then
                                  begin
                                    OEVP -> . VORHANDEN := SUCHELEM .
                                                   VORHANDEN ;
                                    OEVP -> . BELEGT := SUCHELEM .
                                                   BELEGT ;
                                    OEVP -> . HSTALT := SUCHELEM .
                                                   HSTALT ;
                                    OEVP -> . LINIEALT := SUCHELEM .
                                                   LINIEALT ;
                                    OEVP -> . HSTEING := SUCHELEM .
                                                   HSTEING ;
                                    OEVP -> . EINGEST := SUCHELEM .
                                                   EINGEST ;
                                    OEVP -> . BERBEDHF := SUCHELEM .
                                                   BERBEDHF ;
                                    OEVP -> . BERWARTEZEIT := SUCHELEM
                                                   . BERWARTEZEIT ;
                                    OEVP -> . ZEITWARTNEU := SUCHELEM .
                                                   ZEITWARTNEU ;
                                    OEVP -> . ZEITEINF := SUCHELEM .
                                                   ZEITEINF ;
                                    OEVP -> . ZEITGES := SUCHELEM .
                                                   ZEITGES ;
                                    LINAUFB ( L , HST )
                                  end (* then *)
                              end (* else *)
                          end (* then *)
                      end (* then *)
                  end (* then *) ;
                OEVP := OEVP -> . NEXTSPALTE
              end (* while *)
          end (* then *)
      end (* HSTAUFB *) ;


   procedure ARBEITEN ( var UEBRIG : BOOLEAN ) ;

   (*******************************************)
   (*                                         *)
   (*   DIE PROZEDUR ARBEITEN UEBERTRAEGT     *)
   (*   DIE NEUEN MARKIERUNGEN IN DIE FELDER  *)
   (*   UND MARKIERT UND LOESCHT DIE          *)
   (*   NEUMARKIERUNGEN. FUER DIE DANN        *)
   (*   MARKIERTEN HALTESTELLEN MUSS EINE     *)
   (*   BERECHNUNG MIT -HSTAUFB- DURCHGE-     *)
   (*   FUEHRT WERDEN. DABEI ENTSTEHEN GGF.   *)
   (*   NEUE NEUMARKIERUNGEN. DIE BOOLEAN-    *)
   (*   VARIABLE -UEBRIG- GIBT AN, OB IM      *)
   (*   AKTUELLEN DURCHGANG NOCH ETWAS        *)
   (*   BERECHNET WURDE, OB ALSO NOCH         *)
   (*   NEUMARKIERUNGEN UEBRIG SIND.          *)
   (*   WENN -UEBRIG- = FALSE IST, IST        *)
   (*   DIE BERECHNUNG BEENDET.               *)
   (*                                         *)
   (*******************************************)


      var H : HTYP ;

      begin (* ARBEITEN *)
        UEBRIG := FALSE ;
        for H := 1 to HZAHL do
          if OEVKOPFTAB [ H ] . NEUMARKIERT then
            begin
              OEVKOPFTAB [ H ] . MARKIERT := TRUE ;
              OEVKOPFTAB [ H ] . NEUMARKIERT := FALSE
            end (* then *) ;
        for H := 1 to HZAHL do
          begin
            if OEVKOPFTAB [ H ] . MARKIERT then
              begin
                UEBRIG := TRUE ;
                HSTAUFB ( H ) ;
                OEVKOPFTAB [ H ] . MARKIERT := FALSE
              end (* then *)
          end (* for *)
      end (* ARBEITEN *) ;


   procedure WEGDRUCK ( E : EINB ) ;

   (***********************************************************)
   (*                                                         *)
   (*   DIE PROZEDUR WEGDRUCK GIBT - FALLS -TESTMODUS-        *)
   (*   = TRUE IST - DIE GEFUNDENEN WEGE IN DIE DATEI         *)
   (*   -TESTDRUCK- AUS. DA BEI DER ROUTENSUCHE UNTER         *)
   (*   UMSTAENDEN DIE BEFAHRENEN TABELLENELEMENTE            *)
   (*   ZWISCHEN START- UND ZIELHALTESTELLE ODER ZWEI         *)
   (*   UMSTEIGESTATIONEN UEBERSCHRIEBEN WURDEN, WIRD         *)
   (*   FUER DIE WEGAUSGABE NOCHMALS DIE (ABGEWANDELTE)       *)
   (*   PROZEDUR LINAUFB AUFGERUFEN (SIE HEISST HIER          *)
   (*   WDLINAUFB), DIE DIE DAZWISCHENSTEHENDEN WEG-          *)
   (*   ELEMENTE IM LOKALEN FELD -SUCHZ- WIEDER ERSTELLT.     *)
   (*                                                         *)
   (***********************************************************)


      var HST : HTYP ;
          SUCHZ : array [ HTYP ] of OEVSUCHELEM ;
          OEVP : OEVSUCHP ;
          LIN : LTYP ;
          H : HTYP ;
          L : LTYP ;
          HEND : HTYP ;
          FZ : REAL ;
          FUSSW : REAL ;
          WEGAUSGEGEBEN : BOOLEAN ;


      procedure WDLINAUFB ( LIN : LTYP ; HST : HTYP ) ;

      (******************************************************)
      (*                                                    *)
      (*   SIEHE KOMMENTAR ZU -LINAUFB-                     *)
      (*                                                    *)
      (******************************************************)


         var LEERSUCH : OEVSUCHELEM ;
             SUCHELEM : OEVSUCHELEM ;
             OEVP : OEVSUCHP ;
             P , Q : LEPTYP ;
             H , H1 , H2 : HTYP ;
             BHNEU , FZ , EW , UW , WZNEU : REAL ;

         begin (* WDLINAUFB *)
           LEERSUCH . VORHANDEN := FALSE ;
           LEERSUCH . BELEGT := FALSE ;
           LEERSUCH . HSTALT := 0 ;
           LEERSUCH . LINIEALT := 0 ;
           LEERSUCH . HSTEING := 0 ;
           LEERSUCH . EINGEST := FALSE ;
           LEERSUCH . BERBEDHF := 0.0 ;
           LEERSUCH . BERWARTEZEIT := 0.0 ;
           LEERSUCH . ZEITWARTNEU := 0.0 ;
           LEERSUCH . ZEITEINF := 0.0 ;
           LEERSUCH . ZEITGES := 0.0 ;
           for H := 0 to HMAX do
             SUCHZ [ H ] := LEERSUCH ;
           P := LINIEN [ LIN ] ;
           while P -> . HSTELLE <> HST do
             P := P -> . STRECKE ;
           Q := P ;
           OEVP := OEVSUCHTAB ( HST , LIN ) ;
           SUCHZ [ HST ] := OEVP -> ;
           repeat
             H1 := Q -> . HSTELLE ;
             H2 := Q -> . STRECKE -> . HSTELLE ;
             SUCHELEM := SUCHZ [ H1 ] ;
             SUCHELEM . HSTALT := H1 ;
             SUCHELEM . LINIEALT := LIN ;
             SUCHEZEITEN ( HST , H1 , H2 , BHNEU , FZ ) ;
             if BHNEU > SUCHELEM . BERBEDHF then
               begin
                 if SUCHELEM . EINGEST then
                   if BHNEU = 0.0 then
                     WZNEU := 10000.0
                   else
                     begin
                       if BHNEU <= BHOGREIN then
                         EW := EINFAKT * BHNEU / 2
                       else
                         EW := EINFAKT * BHOGREIN / 2 ;
                       WZNEU := EW
                     end (* else *)
                 else
                   begin
                     if BHNEU <= BHOGRUM then
                       UW := KOMFORT * UMFAKT * BHNEU / 2
                     else
                       UW := KOMFORT * UMFAKT * BHOGRUM / 2 ;
                     WZNEU := UW
                   end (* else *) ;
                 SUCHELEM . BERBEDHF := BHNEU ;
                 SUCHELEM . ZEITWARTNEU := WZNEU - SUCHELEM .
                                           BERWARTEZEIT ;
                 SUCHELEM . BERWARTEZEIT := WZNEU
               end (* then *)
             else
               SUCHELEM . ZEITWARTNEU := 0.0 ;
             SUCHELEM . ZEITEINF := FZ ;
             SUCHELEM . ZEITGES := SUCHELEM . ZEITGES + SUCHELEM .
                                   ZEITEINF + SUCHELEM . ZEITWARTNEU ;
             SUCHELEM . HSTEIGEN := H2 ;
             SUCHELEM . LINIEEIGEN := LIN ;
             if SUCHZ [ H2 ] . BELEGT then
               begin
                 if SUCHZ [ H2 ] . ZEITGES > SUCHELEM . ZEITGES then
                   SUCHZ [ H2 ] := SUCHELEM
               end (* then *)
             else
               SUCHZ [ H2 ] := SUCHELEM ;
             Q := Q -> . STRECKE
           until Q = P
         end (* WDLINAUFB *) ;


      begin (* WEGDRUCK *)
        HST := E . HSTELLE ;
        FZ := OEVKOPFTAB [ E . HSTELLE ] . ZEITGESMIN ;
        WEGAUSGEGEBEN := FALSE ;
        OEVP := OEVSUCHSPALTE [ HST ] ;
        while OEVP <> NIL do
          begin
            LIN := OEVP -> . LINIEEIGEN ;
            if LIN <> 0 then
              begin
                SUCHELEM := OEVP -> ;
                if SUCHELEM . BELEGT then
                  if ( SUCHELEM . LINIEALT = LIN ) and not
                  WEGAUSGEGEBEN then
                    begin
                      WEGAUSGEGEBEN := TRUE ;
                      WRITE ( TESTDRUCK , ' ' , ' ' : 56 ) ;
                      FUSSW := E . FW * FWFAKT ;
                      WRITEREAL ( TESTDRUCK , FUSSW , 7 , 2 ) ;
                      WRITEREAL ( TESTDRUCK , FZ + FUSSW , 7 , 2 ) ;
                      WRITELN ( TESTDRUCK ) ;
                      H := HST ;
                      L := LIN ;
                      repeat
                        OEVP := OEVSUCHTAB ( H , L ) ;
                        SUCHELEM := OEVP -> ;
                        HEND := SUCHELEM . HSTEING ;
                        WDLINAUFB ( L , HEND ) ;
                        repeat
                          SUCHELEM := SUCHZ [ H ] ;
                          WRITE ( TESTDRUCK , ' ' , HTEXTE [ H ] ) ;
                          WRITE ( TESTDRUCK , HTRANSFER [ H ] : 6 ) ;
                          WRITE ( TESTDRUCK , LTRANSFER [ L ] : 6 ) ;
                          WRITE ( TESTDRUCK , HTRANSFER [ SUCHELEM .
                                  HSTALT ] : 6 ) ;
                          WRITE ( TESTDRUCK , LTRANSFER [ SUCHELEM .
                                  LINIEALT ] : 6 ) ;
                          if SUCHELEM . LINIEALT = L then
                            WRITEREAL ( TESTDRUCK , SUCHELEM . ZEITEINF
                                        , 7 , 2 )
                          else
                            WRITE ( TESTDRUCK , ' ' : 7 ) ;
                          if SUCHELEM . ZEITWARTNEU <> 0.0 then
                            WRITEREAL ( TESTDRUCK , SUCHELEM .
                                        ZEITWARTNEU , 7 , 2 )
                          else
                            WRITE ( TESTDRUCK , ' ' : 7 ) ;
                          WRITEREAL ( TESTDRUCK , SUCHELEM . ZEITGES ,
                                      7 , 2 ) ;
                          WRITELN ( TESTDRUCK ) ;
                          H := SUCHELEM . HSTALT ;
                        until ( H = HEND ) ;
                        L := SUCHELEM . LINIEALT ;
                      until L = 0 ;
                      FUSSW := SUCHELEM . ZEITGES ;
                      WRITE ( TESTDRUCK , ' ' , ' ' : 56 ) ;
                      WRITEREAL ( TESTDRUCK , FUSSW , 7 , 2 ) ;
                      WRITE ( TESTDRUCK , '   0.00' ) ;
                      WRITELN ( TESTDRUCK ) ;
                      WRITELN ( TESTDRUCK ) ;
                    end (* then *)
              end (* then *) ;
            OEVP := OEVP -> . NEXTSPALTE
          end (* while *) ;
        if not WEGAUSGEGEBEN then
          begin
            WRITE ( TESTDRUCK , ' ' , ' ' : 56 ) ;
            FUSSW := E . FW * FWFAKT ;
            WRITEREAL ( TESTDRUCK , FUSSW , 7 , 2 ) ;
            WRITEREAL ( TESTDRUCK , FZ + FUSSW , 7 , 2 ) ;
            WRITELN ( TESTDRUCK ) ;
            FUSSW := FZ ;
            WRITE ( TESTDRUCK , ' ZU FUSS UEBER HALTESTELLE' ) ;
            WRITE ( TESTDRUCK , HTRANSFER [ HST ] : 6 ) ;
            WRITELN ( TESTDRUCK , ' ' , HTEXTE [ HST ] ) ;
            WRITE ( TESTDRUCK , ' ' , ' ' : 56 ) ;
            WRITEREAL ( TESTDRUCK , FUSSW , 7 , 2 ) ;
            WRITE ( TESTDRUCK , '   0.00' ) ;
            WRITELN ( TESTDRUCK ) ;
            WRITELN ( TESTDRUCK ) ;
          end (* then *)
      end (* WEGDRUCK *) ;


   begin (* OEVROUTSUCH *)
     if OEVQBEZALTVORH and ( OEVQBEZALT = QBEZ ) then
       begin

     (**********************************************************)
     (*   WENN DER VORHERGEHENDE QUELLBEZIRK (-OEVQBEZALT-)    *)
     (*   GLEICH DEM AKTUELLEN IST, BRAUCHT DIE SUCHTABELLE    *)
     (*   NICHT NEU AUFGEBAUT ZU WERDEN. DIE ERGEBNISSE        *)
     (*   KOENNEN DANN DIREKT ABGELESEN WERDEN. LEDIGLICH      *)
     (*   DIE ZEIT FUER DEN FUSSWEG IM ZIELBEZIRK IST NOCH     *)
     (*   ZU BERUECKSICHTIGEN. NUR DANN, WENN DIE QUELL-       *)
     (*   BEZIRKE SICH GEAENDERT HABEN, WIRD DIE SUCHTABELLE   *)
     (*   MIT -OEVSUCHTABLOE- GELOESCHT UND NEU AUFGEBAUT.     *)
     (**********************************************************)

         GEFUNDEN := FALSE ;
         ZEITK := 0.0
       end (* then *)
     else
       begin
         OEVSUCHTABLOE ;
         GEFUNDEN := FALSE ;
         ZEITK := 0.0 ;

     (**********************************************************)
     (*   ZUERST WERDEN DIE IN DEN QUELLBEZIRK EINGEBUNDENEN   *)
     (*   HALTESTELLEN IN DIE SUCHTABELLE AUFGENOMMEN          *)
     (**********************************************************)

         EP := EINBIND [ QBEZ ] ;
         while EP <> NIL do
           begin
             SUCHELEM . VORHANDEN := TRUE ;
             SUCHELEM . BELEGT := TRUE ;
             SUCHELEM . HSTALT := 0 ;
             SUCHELEM . LINIEALT := 0 ;
             SUCHELEM . HSTEING := 0 ;
             SUCHELEM . EINGEST := TRUE ;
             SUCHELEM . BERBEDHF := 0.0 ;
             SUCHELEM . BERWARTEZEIT := 0.0 ;
             SUCHELEM . ZEITWARTNEU := 0.0 ;
             SUCHELEM . ZEITEINF := EP -> . FW * FWFAKT ;
             SUCHELEM . ZEITGES := EP -> . FW * FWFAKT ;
             OEVP := OEVSUCHTAB ( EP -> . HSTELLE , 0 ) ;
             OEVP -> . VORHANDEN := SUCHELEM . VORHANDEN ;
             OEVP -> . BELEGT := SUCHELEM . BELEGT ;
             OEVP -> . HSTALT := SUCHELEM . HSTALT ;
             OEVP -> . LINIEALT := SUCHELEM . LINIEALT ;
             OEVP -> . HSTEING := SUCHELEM . HSTEING ;
             OEVP -> . EINGEST := SUCHELEM . EINGEST ;
             OEVP -> . BERBEDHF := SUCHELEM . BERBEDHF ;
             OEVP -> . BERWARTEZEIT := SUCHELEM . BERWARTEZEIT ;
             OEVP -> . ZEITWARTNEU := SUCHELEM . ZEITWARTNEU ;
             OEVP -> . ZEITEINF := SUCHELEM . ZEITEINF ;
             OEVP -> . ZEITGES := SUCHELEM . ZEITGES ;
             OEVKOPFTAB [ EP -> . HSTELLE ] . NEUMARKIERT := TRUE ;
             EP := EP -> . NEXT
           end (* while *) ;

     (***************************************************)
     (*   IN DIESER SCHLEIFE WIRD DIE EIGENTLICHE       *)
     (*   ARBEIT GELEISTET (SIEHE PROZEDUR ARBEITEN)    *)
     (***************************************************)

         repeat
           ARBEITEN ( UEBRIG ) ;
         until not UEBRIG
       end (* else *) ;

     (***************************************************)
     (*   DIE SUCHTABELLE IST GEFUELLT. JETZT MUESSEN   *)
     (*   NUR NOCH DIE FUSSWEGE IM ZIELBEZIRK BERUECK-  *)
     (*   SICHTIGT WERDEN.                              *)
     (***************************************************)

     EP := EINBIND [ ZBEZ ] ;
     MAXEP := EP ;
     HST := 0 ;
     while EP <> NIL do
       begin
         if OEVKOPFTAB [ EP -> . HSTELLE ] . ZEITBELEGT then
           begin
             ZEIT := OEVKOPFTAB [ EP -> . HSTELLE ] . ZEITGESMIN + EP
                     -> . FW * FWFAKT ;
             if ( ZEIT < ZEITK ) or ( HST = 0 ) then
               begin
                 GEFUNDEN := TRUE ;
                 ZEITK := ZEIT ;
                 MAXEP := EP ;
                 HST := EP -> . HSTELLE
               end (* then *)
           end (* then *) ;
         EP := EP -> . NEXT
       end (* while *) ;

     (*************************************************)
     (*   NUR DIE EINBINDUNG, DIE ZU DER KLEINSTEN    *)
     (*   GESAMTZEIT FUEHRT, WIRD BERUECKSICHTIGT.    *)
     (*************************************************)

     if TESTMODUS then
       begin
         WRITE ( 'OEV-ROUTENSUCHE: ' ) ;
         WRITE ( BTRANSFER [ QBEZ ] : 6 , ' -> ' , BTRANSFER [ ZBEZ ] :
                 6 ) ;
         WRITE ( '    ZEIT=' ) ;
         WRITEREAL ( OUTPUT , ZEITK , 8 , 2 ) ;
         WRITE ( ' MIN' ) ;
         WRITELN ;
         if GEFUNDEN then
           begin
             WRITELN ( TESTDRUCK ) ;
             WRITE ( TESTDRUCK , ' OEV-ROUTSUCH :' ) ;
             WRITE ( TESTDRUCK , ' VON BEZIRK' , BTRANSFER [ QBEZ ] : 6
                     ) ;
             WRITE ( TESTDRUCK , ' NACH BEZIRK' , BTRANSFER [ ZBEZ ] :
                     6 ) ;
             WRITELN ( TESTDRUCK ) ;
             WEGDRUCK ( MAXEP -> )
           end (* then *)
         else
           begin
             WRITELN ( TESTDRUCK ) ;
             WRITE ( TESTDRUCK , ' OEV-ROUTSUCH :' ) ;
             WRITELN ( TESTDRUCK ) ;
             WRITELN ( TESTDRUCK ) ;
             WRITE ( TESTDRUCK , ' VON BEZIRK' , BTRANSFER [ QBEZ ] : 6
                     ) ;
             WRITE ( TESTDRUCK , ' NACH BEZIRK' , BTRANSFER [ ZBEZ ] :
                     6 ) ;
             WRITE ( TESTDRUCK , ' EXISTIERT KEIN WEG' ) ;
             WRITELN ( TESTDRUCK ) ;
             WRITELN ( TESTDRUCK )
           end (* else *)
       end (* then *) ;

     (*********************************************************)
     (*   DIE FELDER, DIE DEN ALTEN QUELLBEZIRK ENTHALTEN,    *)
     (*   WERDEN NEU BESETZT (SEITENEFFEKTE!)                 *)
     (*********************************************************)

     OEVQBEZALT := QBEZ ;
     OEVQBEZALTVORH := TRUE
   end (* OEVROUTSUCH *) ;



procedure OEVKONSISTENZ ( var RICHTIG : BOOLEAN ) ;

(*****************************************************************)
(*                                                               *)
(*   KONSISTENZPRUEFUNG NACH DEM LADEN DES OEV-MODELLS           *)
(*                                                               *)
(*   FOLGENDE KONSISTENZBEDINGUNGEN WERDEN GEPRUEFT:             *)
(*                                                               *)
(*   - BEZIRKE MUESSEN EINDEUTIG UND AUFSTEIGEND SEIN.           *)
(*   - BEZIRKE UEBER BZAHL MUESSEN UNGLEICH 0 SEIN.              *)
(*   - HALTESTELLEN MUESSEN EINDEUTIG UND AUFSTEIGEND SEIN.      *)
(*   - HALTESTELLEN UEBER HZAHL MUESSEN GLEICH 0 SEIN.           *)
(*   - IN EINER LISTE VON EINBINDUNGEN MUESSEN DIE HALTESTELLEN  *)
(*     EINDEUTIG SEIN.                                           *)
(*   - JEDE HALTESTELLE IN EINER LISTE VON EINBINDUNGEN MUSS     *)
(*     DEFINIERT SEIN.                                           *)
(*   - IN EINER LISTE VON STRECKEN MUESSEN DIE ZIELHALTESTELLEN  *)
(*     EINDEUTIG SEIN.                                           *)
(*   - QUELL- UND ZIELHALTESTELLEN BEI STRECKEN MUESSEN VER-     *)
(*     SCHIEDEN SEIN.                                            *)
(*   - JEDE ZIELHALTESTELLE IN EINER LISTE VON STRECKEN MUSS     *)
(*     DEFINIERT SEIN.                                           *)
(*   - LINIENNUMMERN MUESSEN AUFSTEIGEND UND EINDEUTIG SEIN.     *)
(*   - LINIENNUMMERN UEBER LZAHL MUESSEN GLEICH 0 SEIN.          *)
(*   - BEI DER LISTE VON HALTESTELLEN AUF EINER LINIE MUSS       *)
(*     ES SICH UM EINE RINGLISTE HANDELN (KEIN POINTER DARF      *)
(*     NIL SEIN)                                                 *)
(*   - JEDE HALTESTELLE AUF EINER LINIEN-RINGLISTE MUSS DEFI-    *)
(*     NIERT SEIN.                                               *)
(*   - JEDE TEILSTRECKE EINER LINIE MUSS IM STRECKENNETZ         *)
(*     DEFINIERT SEIN.                                           *)
(*   - BEZIRKSNUMMERN MUESSEN POSITIV SEIN.                      *)
(*   - HALTESTELLEN MUESSEN POSITIV SEIN.                        *)
(*   - LINIENNUMMERN MUESSEN POSITIV SEIN.                       *)
(*                                                               *)
(*****************************************************************)


   var I : INTEGER ;
       HKOPF , HPTR , HPTR2 : HPTYP ;
       EKOPF , EPTR , EPTR2 : EPTYP ;
       LKOPF , LPTR : LEPTYP ;
       HTEST , HTEST2 : INTEGER ;
       HIND : HTYP ;
       STATUS : STAT ;
       OK : BOOLEAN ;
       ERSTERFEHLER : BOOLEAN ;


   procedure FEHLER ( NR : INTEGER ) ;

   (***********************************************************)
   (*   FALLS BEI DER KONSISTENZPRUEFUNG FEHLER AUFTRETEN,    *)
   (*   SO WERDEN HIER DIE FEHLERTEXTE AUSGEGEBEN;            *)
   (*   GEGEBENENFALLS MIT ZUSAETZLICHEN ERLAEUTERUNGEN.      *)
   (***********************************************************)


      begin (* FEHLER *)
        RICHTIG := FALSE ;
        if ERSTERFEHLER then
          begin
            WRITELN ( TESTDRUCK , 'OEV-KONSISTENZPRUEFUNG:' ) ;
            WRITELN ( TESTDRUCK ) ;
            ERSTERFEHLER := FALSE
          end (* then *) ;
        if ( NR = 5 ) or ( NR = 6 ) then
          WRITE ( TESTDRUCK , 'IN EINBINDUNG ZU' , BTRANSFER [ I ] : 6
                  , ' : ' ) ;
        if ( NR = 7 ) or ( NR = 8 ) or ( NR = 17 ) then
          WRITE ( TESTDRUCK , 'IN STRECKENN. ZU' , HTRANSFER [ I ] : 6
                  , ' : ' ) ;
        if ( NR >= 11 ) and ( NR <= 13 ) then
          WRITE ( TESTDRUCK , 'IN LINIE' , LTRANSFER [ I ] : 6 , ' ' :
                  8 , ' : ' ) ;
        case NR of
          1 : WRITELN ( TESTDRUCK , 'BEZIRKE NICHT EINDEUTIG ' ,
                        'ODER NICHT AUFSTEIGEND' ) ;
          2 : WRITELN ( TESTDRUCK , 'BEZIRKE UEBER BZAHL UNGLEICH 0' )
                        ;
          3 : WRITELN ( TESTDRUCK , 'HALTESTELLEN NICHT EINDEUTIG ' ,
                        'ODER NICHT AUFSTEIGEND' ) ;
          4 : WRITELN ( TESTDRUCK ,
                        'HALTESTELLEN UEBER HZAHL UNGLEICH 0' ) ;
          5 : WRITELN ( TESTDRUCK , 'HALTESTELLE NICHT VORHANDEN' ) ;
          6 : WRITELN ( TESTDRUCK , 'HALTESTELLE NICHT EINDEUTIG' ) ;
          7 : WRITELN ( TESTDRUCK , 'ZIELHALTESTELLE NICHT VORHANDEN' )
                        ;
          8 : WRITELN ( TESTDRUCK , 'ZIELHALTESTELLE NICHT EINDEUTIG' )
                        ;
          9 : WRITELN ( TESTDRUCK , 'LINIEN NICHT EINDEUTIG ' ,
                        'ODER NICHT AUFSTEIGEND' ) ;
          10 : WRITELN ( TESTDRUCK , 'LINIEN UEBER HZAHL UNGLEICH 0' )
                         ;
          11 : WRITELN ( TESTDRUCK , 'RINGLISTE NICHT GESCHLOSSEN' ) ;
          12 : WRITELN ( TESTDRUCK , 'HALTESTELLE NICHT VORHANDEN' ) ;
          13 : WRITELN ( TESTDRUCK ,
                         'TEILSTRECKE NICHT IN STRECKENNETZ' ) ;
          14 : WRITELN ( TESTDRUCK , 'BEZIRK KLEINER 0 NICHT ERLAUBT' )
                         ;
          15 : WRITELN ( TESTDRUCK ,
                         'HALTESTELLE KLEINER 0 NICHT ERLAUBT' ) ;
          16 : WRITELN ( TESTDRUCK , 'LINIE KLEINER 0 NICHT ERLAUBT' )
                         ;
          17 : WRITELN ( TESTDRUCK ,
                         'QUELL- UND ZIELHALTESTELLE GLEICH' ) ;
        end (* case *) ;
        if NR = 11 then
          begin
            WRITELN ( TESTDRUCK , 'ABBRUCH !' ) ;
            HALT
          end (* then *)
      end (* FEHLER *) ;


   begin (* OEVKONSISTENZ *)
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
     if HZAHL > 0 then
       if HTRANSFER [ 1 ] <= 0 then
         FEHLER ( 15 ) ;
     for I := 2 to HZAHL do
       if HTRANSFER [ I ] <= 0 then
         FEHLER ( 15 )
       else
         if HTRANSFER [ I ] <= HTRANSFER [ I - 1 ] then
           FEHLER ( 3 ) ;
     for I := HZAHL + 1 to HMAX do
       if HTRANSFER [ I ] <> 0 then
         FEHLER ( 4 ) ;
     for I := 1 to BZAHL do
       begin
         EKOPF := EINBIND [ I ] ;
         if EKOPF <> NIL then
           begin
             EPTR := EKOPF ;
             while EPTR <> NIL do
               begin
                 HTEST := HTRANSFER [ EPTR -> . HSTELLE ] ;
                 SEARCHH ( HTEST , HIND , STATUS ) ;
                 if STATUS <> 0 then
                   FEHLER ( 5 ) ;
                 EPTR2 := EKOPF ;
                 while EPTR2 <> EPTR do
                   begin
                     if EPTR2 -> . HSTELLE = HTEST then
                       FEHLER ( 6 ) ;
                     EPTR2 := EPTR2 -> . NEXT
                   end (* while *) ;
                 EPTR := EPTR -> . NEXT
               end (* while *)
           end (* then *)
       end (* for *) ;
     for I := 1 to HZAHL do
       begin
         HKOPF := STRECKENNETZ [ I ] ;
         if HKOPF <> NIL then
           begin
             HPTR := HKOPF ;
             while HPTR <> NIL do
               begin
                 HTEST := HTRANSFER [ HPTR -> . HSTELLE ] ;
                 if HTEST = HTRANSFER [ I ] then
                   FEHLER ( 17 ) ;
                 SEARCHH ( HTEST , HIND , STATUS ) ;
                 if STATUS <> 0 then
                   FEHLER ( 7 ) ;
                 HPTR2 := HKOPF ;
                 while HPTR2 <> HPTR do
                   begin
                     if HPTR2 -> . HSTELLE = HTEST then
                       FEHLER ( 8 ) ;
                     HPTR2 := HPTR2 -> . NEXT
                   end (* while *) ;
                 HPTR := HPTR -> . NEXT
               end (* while *)
           end (* then *)
       end (* for *) ;
     if LZAHL > 0 then
       if LTRANSFER [ 1 ] <= 0 then
         FEHLER ( 16 ) ;
     for I := 2 to LZAHL do
       if LTRANSFER [ I ] <= 0 then
         FEHLER ( 16 )
       else
         if LTRANSFER [ I ] <= LTRANSFER [ I - 1 ] then
           FEHLER ( 9 ) ;
     for I := LZAHL + 1 to LMAX do
       if LTRANSFER [ I ] <> 0 then
         FEHLER ( 10 ) ;
     for I := 1 to LZAHL do
       begin
         LKOPF := LINIEN [ I ] ;
         LPTR := LKOPF ;
         repeat
           if LPTR = NIL then
             FEHLER ( 11 ) ;
           HTEST := HTRANSFER [ LPTR -> . HSTELLE ] ;
           SEARCHH ( HTEST , HIND , STATUS ) ;
           if STATUS <> 0 then
             FEHLER ( 12 ) ;
           HTEST2 := LPTR -> . STRECKE -> . HSTELLE ;
           HKOPF := STRECKENNETZ [ HIND ] ;
           OK := FALSE ;
           while ( HKOPF <> NIL ) and not OK do
             begin
               OK := ( HKOPF -> . HSTELLE = HTEST2 ) ;
               HKOPF := HKOPF -> . NEXT
             end (* while *) ;
           if not OK then
             FEHLER ( 13 ) ;
           LPTR := LPTR -> . STRECKE
         until LPTR = LKOPF ;
       end (* for *)
   end (* OEVKONSISTENZ *) ;



procedure OEVZEITRECHNUNG ;

(*********************************************************)
(*                                                       *)
(*   HIER WERDEN (ZU BEGINN DES PROGRAMMS, D.H. NACH     *)
(*   DEM LADEN DER OEV-PARAMETER) JE TEILSTRECKE         *)
(*   FOLGENDE INFORMATIONEN BERECHNET:                   *)
(*                                                       *)
(*   - DIE MITTLERE FAHRZEIT (ARITHMETISCHES MITTEL      *)
(*     AUS ALLEN LINIENFAHRZEITEN)                       *)
(*   - DIE ANZAHL DER BEFAHRENDEN LINIEN                 *)
(*   - DIE MENGE DER BEFAHRENDEN LINIEN (SET OF LTYP)    *)
(*                                                       *)
(*   DIESE INFORMATIONEN WERDEN BEI DER ROUTENSUCHE      *)
(*   BENOETIGT UND IM STRECKENNETZ GESPEICHERT.          *)
(*                                                       *)
(*********************************************************)


   var L : LTYP ;
       P , Q : LEPTYP ;
       H1 , H2 : HTYP ;
       ST : HPTYP ;
       FAHRZ , BEDHF : REAL ;

   begin (* OEVZEITRECHNUNG *)
     for L := 1 to LZAHL do
       begin
         if BH [ L , TAGESZEIT ] > 0 then
           begin
             P := LINIEN [ L ] ;
             Q := P ;
             repeat
               H1 := Q -> . HSTELLE ;
               H2 := Q -> . STRECKE -> . HSTELLE ;
               FAHRZ := Q -> . FZ ;
               BEDHF := BH [ L , TAGESZEIT ] ;
               ST := STRECKENNETZ [ H1 ] ;
               while ST <> NIL do
                 begin
                   if ST -> . HSTELLE <> H2 then
                     ST := ST -> . NEXT
                   else
                     begin
                       if ST -> . SANZLIN = 0 then
                         begin
                           ST -> . SFAHRZEIT := FAHRZ ;
                           ST -> . SLINSET := [ L ] ;
                           ST -> . SANZLIN := 1
                         end (* then *)
                       else
                         begin
                           ST -> . SFAHRZEIT := ST -> . SFAHRZEIT * ST
                                                -> . SANZLIN ;
                           ST -> . SFAHRZEIT := ST -> . SFAHRZEIT +
                                                FAHRZ ;
                           ST -> . SANZLIN := ST -> . SANZLIN + 1 ;
                           ST -> . SFAHRZEIT := ST -> . SFAHRZEIT / ST
                                                -> . SANZLIN ;
                           ST -> . SLINSET := ST -> . SLINSET + [ L ]
                         end (* else *) ;
                       ST := NIL
                     end (* else *)
                 end (* while *) ;
               Q := Q -> . STRECKE
             until Q = P ;
           end (* then *)
       end (* for *)
   end (* OEVZEITRECHNUNG *) ;



procedure OEVVORBESETZEN ;

(*************************************************)
(*   VORBESETZUNGEN IM OEV-BEREICH               *)
(*                                               *)
(*   VORBESETZT WERDEN:                          *)
(*   - DIE SUCH- UND KOPFTABELLE                 *)
(*   - DIE DATEN UEBER DEN ALTEN QUELLBEZIRK     *)
(*************************************************)


   var I : HTYP ;
       K : LTYP ;
       LEERKOPF : OEVKOPFFELD ;

   begin (* OEVVORBESETZEN *)
     OEVQBEZALT := 0 ;
     OEVQBEZALTVORH := FALSE ;
     LEERKOPF . MARKIERT := FALSE ;
     LEERKOPF . NEUMARKIERT := FALSE ;
     LEERKOPF . ZEITBELEGT := FALSE ;
     LEERKOPF . ZEITGESMIN := 0.0 ;
     for I := 0 to HMAX do
       begin
         OEVKOPFTAB [ I ] := LEERKOPF ;
         OEVSUCHSPALTE [ I ] := NIL ;
       end (* for *) ;
     for K := 0 to LMAX do
       OEVSUCHZEILE [ K ] := NIL ;
   end (* OEVVORBESETZEN *) ;



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



function VPTRANS ( X : INTEGER ) : VPTYP ;

(*********************************************************)
(*   EXTERNE NUMMER -> INTERNE NUMMER BEI KNOTENPUNKTEN  *)
(*********************************************************)


   var K , I , J : INTEGER ;

   begin (* VPTRANS *)
     I := 1 ;
     J := VPZAHL ;
     VPTRANS := 0 ;
     repeat
       K := ( I + J ) DIV 2 ;
       if X > VPTRANSFER [ K ] then
         I := K + 1
       else
         J := K - 1
     until ( VPTRANSFER [ K ] = X ) or ( I > J ) ;
     if VPTRANSFER [ K ] = X then
       VPTRANS := K
     else
       begin
         WRITELN ( 'DATENFEHLER, ABBRUCH !!' ) ;
         HALT
       end (* else *)
   end (* VPTRANS *) ;



procedure IVINIT ;

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


   var BZAHLW : BTYP ;
       BTRANSFERW : array [ BTYP ] of INTEGER ;
       ANSCHLUESSEW : array [ BTYP ] of APTYP ;
       I : INTEGER ;
       KENN : packed array [ 1 .. 6 ] of CHAR ;
       VKNUMMER : INTEGER ;
       BP , BPX : APTYP ;
       BEZ : INTEGER ;
       BIND : BTYP ;
       STATUS : STAT ;
       STR : STRASSE ;
       STP : STPTYP ;
       CH : CHAR ;
       IOKENNZ : CHAR ;
       SIT : SITUATIONEN ;


   procedure READKENN ;

   (*************************************************)
   (*   LIEST DIE KENNUNG IN DEN ERSTEN 6 SPALTEN   *)
   (*************************************************)


      var I : INTEGER ;

      begin (* READKENN *)
        for I := 1 to 6 do
          READ ( IVMODELL , KENN [ I ] )
      end (* READKENN *) ;


   procedure INSERTB ( NUMMER : INTEGER ; AP : APTYP ; var STATUS :
                     STAT ) ;

   (**************************************************)
   (*   BEZIRKE EINFUEGEN                            *)
   (*   IST DANN NOETIG, WENN BEZIRK IM OEV-MODELL   *)
   (*   NICHT VORHANDEN IST.                         *)
   (**************************************************)


      var I , J : BTYP ;

      begin (* INSERTB *)
        if BZAHL = BMAX - 1 then
          begin
            WRITELN ( 'UEBERLAUF BEZIRKSTABELLE, ABBRUCH !!' ) ;
            HALT
          end (* then *)
        else
          begin
            I := 1 ;
            while ( BTRANSFER [ I ] < NUMMER ) and ( I <= BZAHL ) do
              I := I + 1 ;
            if I > BZAHL then
              begin
                BTRANSFER [ I ] := NUMMER ;
                BZAHL := BZAHL + 1 ;
                ANSCHLUESSE [ I ] := AP ;
                EINBIND [ I ] := NIL ;
                STATUS := 0
              end (* then *)
            else
              if ( BTRANSFER [ I ] <> NUMMER ) then
                begin
                  for J := BZAHL DOWNTO I do
                    begin
                      BTRANSFER [ J + 1 ] := BTRANSFER [ J ] ;
                      ANSCHLUESSE [ J + 1 ] := ANSCHLUESSE [ J ] ;
                      EINBIND [ J + 1 ] := EINBIND [ J ] ;
                    end (* for *) ;
                  BTRANSFER [ I ] := NUMMER ;
                  BZAHL := BZAHL + 1 ;
                  ANSCHLUESSE [ I ] := AP ;
                  EINBIND [ I ] := NIL ;
                  STATUS := 0
                end (* then *)
              else
                STATUS := 1
          end (* else *)
      end (* INSERTB *) ;


   begin (* IVINIT *)
     RESET ( IVMODELL ) ;

     (********************************************************)
     (*   FELDER USW., DIE DAS IV-MODELL AUFNEHMEN SOLLEN,   *)
     (*   VORBESETZEN.                                       *)
     (********************************************************)

     for I := 1 to BMAX do
       begin
         BTRANSFERW [ I ] := 0 ;
         ANSCHLUESSEW [ I ] := NIL ;
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

     (*****************************************)
     (*   BEZIRKE EINLESEN,                   *)
     (*   ZUNAECHST NUR IN TEMPORAERES FELD   *)
     (*****************************************)

     BZAHLW := 0 ;
     READKENN ;
     while KENN = 'BTFILE' do
       begin
         BZAHLW := BZAHLW + 1 ;
         READ ( IVMODELL , BTRANSFERW [ BZAHLW ] ) ;
         READLN ( IVMODELL ) ;
         READKENN
       end (* while *) ;

     (*****************************************)
     (*   ANSCHLUESSE EINLESEN,               *)
     (*   ZUNAECHST NUR IN TEMPORAERES FELD   *)
     (*****************************************)

     for I := 1 to BZAHLW do
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
             BPX -> . ANSPUNKT := VKNUMMER ;
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
         ANSCHLUESSEW [ I ] := BP ;
         READLN ( IVMODELL ) ;
         READKENN
       end (* for *) ;

     (*************************************************)
     (*                                               *)
     (*   HIER WERDEN NUN DIE BEZIRKE IN DIE ECHTE    *)
     (*   BEZIRKSTABELLE UEBERTRAGEN. IN DER          *)
     (*   REGEL WERDEN SIE DORT SCHON VOR-            *)
     (*   HANDEN SEIN (AUS DEM OEV-MODELL). IN        *)
     (*   DIESEN FAELLEN WIRD LEDIGLICH DAS FELD      *)
     (*   -ANSCHLUESSE- MIT DEM ENTSPRECHENDEN        *)
     (*   WERT BESETZT. ANSONSTEN WIRD DER            *)
     (*   BEZIRK AN DER RICHTIGEN STELLE NEU EINGE-   *)
     (*   FUEGT (PROZEDUR INSERTB). DIE BEZIRKS-      *)
     (*   TABELLE MUSS GROSS GENUG SEIN, UM SOWOHL    *)
     (*   DIE IV-BEZIRKE ALS AUCH DIE OEV-BEZRIKE     *)
     (*   AUFNEHMEN ZU KOENNEN.                       *)
     (*                                               *)
     (*************************************************)

     for I := 1 to BZAHLW do
       begin
         BEZ := BTRANSFERW [ I ] ;
         SEARCHB ( BEZ , BIND , STATUS ) ;
         if STATUS = 0 then
           begin
             ANSCHLUESSE [ BIND ] := ANSCHLUESSEW [ I ]
           end (* then *)
         else
           begin
             INSERTB ( BEZ , ANSCHLUESSEW [ I ] , STATUS ) ;
             if STATUS <> 0 then
               begin
                 WRITELN ( 'FEHLER IN PROZ. IVINIT, ABBRUCH !!' ) ;
                 HALT
               end (* then *)
           end (* else *)
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
                 NEW ( STP ) ;
                 STR . ANFANG := STP
               end (* then *)
             else
               begin
                 NEW ( STP -> . NEXT ) ;
                 STP := STP -> . NEXT ;
                 STR . ENDE := STP
               end (* else *) ;
             READ ( IVMODELL , STP -> . VKPUNKT ) ;
             STP -> . VKPUNKT := VPTRANS ( STP -> . VKPUNKT ) ;
             READ ( IVMODELL , STP -> . DISTANZ ) ;
             READ ( IVMODELL , STP -> . KAT ) ;
             for SIT := SITMIN to SITMAX do
               READ ( IVMODELL , STP -> . GESCHWABSCHL [ SIT ] ) ;
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
             STP -> . BELEGT := TRUE ;
             STP -> . ZEITABANFANG := 0.0 ;
             STP -> . ZEITBISHIER := 0.0 ;
             STP -> . VKPSTART := 0 ;
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
         WRITELN ( '*** FALSCHER AUFBAU DER IV-MODELLDATEI ***' ) ;
         WRITELN ( '*** ABBRUCH !! ***************************' ) ;
         WRITELN ;
         HALT
       end (* then *)
   end (* IVINIT *) ;



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



procedure IVROUTSUCH ( QBEZ : BTYP ; ZBEZ : BTYP ; var ZEITG : REAL ;
                     var AOKM : REAL ; var IOKM : REAL ; var GESAMTKM :
                     REAL ; var GEFUNDEN : BOOLEAN ) ;

(**********************************************************)
(*                                                        *)
(*   HIER BEGINNT DIE IV-ROUTENSUCHE. SIE WIRD AUS-       *)
(*   FUEHRLICH IN DER DOKUMENTATION ERLAEUTERT.           *)
(*                                                        *)
(*   BEI DER IV-ROUTENSUCHE WERDEN - AUSGEHEND VOM        *)
(*   QUELLBEZIRK - ALLE STRASSEN IN ALLE RICHTUNGEN       *)
(*   BEFAHREN, WOBEI IN DEN ERWEITERUNGSFELDERN DER       *)
(*   STRECKENBESCHREIBUNGEN DIE FAHRZEITEN AUFSUMMIERT    *)
(*   WERDEN.                                              *)
(*                                                        *)
(*   IM PRINZIP LAEUFT DIE ROUTENSUCHE WIE FOLGT AB:      *)
(*                                                        *)
(*   IM ERSTEN SCHRITT WERDEN NUR DIE STRASSEN BEFAHREN,  *)
(*   DIE EINEN ANSCHLUSS AN DEN QUELLBEZIRK HABEN.        *)
(*   FALLS DABEI EIN KNOTEN BEFAHREN WIRD, DER            *)
(*   KREUZUNGSPUNKT IST (UND DIE NEUE ZEIT FUER DAS       *)
(*   ERREICHEN DIESES KNOTENS KLEINER IST ALS DIE         *)
(*   BISHER VORHANDENE), WIRD DIESER KNOTEN MARKIERT,     *)
(*   SO DASS IM NAECHSTEN SCHRITT AUF DER KREUZENDEN      *)
(*   STRASSE WEITERGEFAHREN WERDEN KANN.                  *)
(*                                                        *)
(*   IM ZWEITEN SCHRITT WERDEN VON JEDEM MARKIERTEN       *)
(*   KNOTEN AUS DIE FAHRTEN WEITERGEFUEHRT. DABEI         *)
(*   WERDEN JEDOCH NUR SOLCHE FAHRTEN DURCHGEFUEHRT,      *)
(*   DIE ZU KUERZEREN ZEITEN FUER DIE KNOTENPUNKTE        *)
(*   FUEHREN. EVTL. NEU HINZUKOMMENDE KREUZUNGSPUNKTE     *)
(*   WERDEN - WIE SCHON IM ERSTEN SCHRITT - MARKIERT.     *)
(*                                                        *)
(*   DIESER ZWEITE SCHRITT WIRD SOLANGE WIEDERHOLT,       *)
(*   BIS KEINE NEUEN KREUZUNGSPUNKTE MEHR HINZUKOMMEN.    *)
(*   DIE ZEITEN DER KNOTEN, DIE EINEN ANSCHLUSS IN DEN    *)
(*   ZIELBEZIRK BESITZEN, LIEFERN DANN ZUSAMMEN MIT       *)
(*   DEN ANFAHRZEITEN DIESES ANSCHLUSSES DIE GESAMTZEIT.  *)
(*                                                        *)
(*   BEACHTENSWERT IST, DASS - AUSGEHEND VON EINEM        *)
(*   QUELLBEZIRK - ALLE FAHRTEN IN ALLE RICHTUNGEN        *)
(*   ERMITTELT WERDEN. BEI AUFEINANDERFOLGENDEN           *)
(*   AUFRUFEN MIT DEMSELBEN QUELLBEZIRK MUSS ALSO         *)
(*   FAST NICHTS MEHR GEMACHT WERDEN, DA DIE DATEN        *)
(*   IN DEN STRECKENBESCHREIBUNGEN IMMER NOCH STIMMEN.    *)
(*   AUS DIESEM GRUND SOLLTEN DIE ANFORDERUNGEN NACH      *)
(*   QUELLBEZIRKEN SORTIERT ERFOLGEN. DER ZEITGEWINN      *)
(*   DURCH DIESE EINFACHE MASSNAHME IST IMMENS.           *)
(*                                                        *)
(**********************************************************)


   var ZEIT : REAL ;
       AP : APTYP ;
       VP : VPTYP ;
       APFOUND : APTYP ;
       VPFOUND : VPTYP ;
       UEBRIG : BOOLEAN ;
       S : SLISTTYP ;
       STP : STPTYP ;


   procedure IVSUCHTABLOE ;

   (*******************************************************)
   (*                                                     *)
   (*   DIE INFORMATIONEN FUER DIE IV-ROUTENSUCHE SIND    *)
   (*                                                     *)
   (*   1. DIE IVSUCHTABELLE.                             *)
   (*                                                     *)
   (*      DABEI HANDELT ES SICH UM EINE EINDIMENS.       *)
   (*      TABELLE MIT EINEM ELEMENT PRO KNOTENPUNKT.     *)
   (*      DORT WERDEN DIE MARKIERUNGEN DER KNOTEN-       *)
   (*      PUNKTE GEFUEHRT, UND, FALLS SIE BEREITS        *)
   (*      BEFAHREN WURDEN, DIE ZEIT.                     *)
   (*                                                     *)
   (*   2. DIE ZUSAETZLICHEN INFORMATIONEN IN DEN         *)
   (*      STRECKENBESCHREIBUNGEN.                        *)
   (*                                                     *)
   (*      JE STRECKENPUNKT WIRD DIE ZEIT ABGELEGT,       *)
   (*      DIE BIS ZU DIESEM STRECKENPUNKT BENOETIGT      *)
   (*      WURDE. ZUSAETZLICH WIRD DIE NUMMER DES         *)
   (*      KNOTENPUNKTES ABGELEGT, AN DEM AUF DIE         *)
   (*      BETREFFENDE STRASSE AUFGEFAHREN WURDE.         *)
   (*      DAMIT LAESST SICH (IN DER PROZEDUR             *)
   (*      WEGDRUCK) DER GEFAHRENE WEG REKONSTRUIEREN.    *)
   (*                                                     *)
   (*   DIESE INFORMATIONEN WERDEN HIER IN DER PROZEDUR   *)
   (*   IVSUCHTABLOE GELOESCHT BZW. AUF IHREN ANFANGS-    *)
   (*   WERT ZURUECKGESETZT. DAS MUSS DANN GEMACHT        *)
   (*   WERDEN, WENN DER QUELLBEZIRK WECHSELT UND         *)
   (*   DEMZUFOLGE DIE OBENGENANNTEN INFORMATIONEN        *)
   (*   NEU BERECHNET WERDEN MUESSEN (SIEHE RUMPF         *)
   (*   DER PROZEDUR IVROUTSUCH).                         *)
   (*                                                     *)
   (*******************************************************)


      var VP : VPTYP ;
          LEERSUCH : IVSUCHELEM ;
          I : STRTYP ;
          STP : STPTYP ;

      begin (* IVSUCHTABLOE *)
        LEERSUCH . BELEGT := FALSE ;
        LEERSUCH . MARKIERT := FALSE ;
        LEERSUCH . NEUMARKIERT := FALSE ;
        LEERSUCH . ZEITGES := 0.0 ;
        for VP := 1 to VPZAHL do
          IVSUCHTAB [ VP ] := LEERSUCH ;
        for I := 1 to STRZAHL do
          begin
            STP := STRASSENNETZ [ I ] . ANFANG ;
            while STP <> NIL do
              begin
                STP -> . BELEGT := FALSE ;
                STP -> . ZEITBISHIER := 0.0 ;
                STP -> . VKPSTART := 0 ;
                STP := STP -> . NEXT
              end (* while *)
          end (* for *)
      end (* IVSUCHTABLOE *) ;


   procedure ARBEITEN ( var UEBRIG : BOOLEAN ) ;

   (*******************************************************)
   (*                                                     *)
   (*   DIE PROZEDUR ARBEITEN ERLEDIGT DIE EIGENTLICHE    *)
   (*   ARBEIT BEI DER ROUTENSUCHE; SIE FUEHRT NAEMLICH   *)
   (*   EINEN SCHRITT DER IV-ROUTENSUCHE (WIE OBEN        *)
   (*   DEFINIERT) AUS.                                   *)
   (*                                                     *)
   (*   AUSGANGSSITUATION IST DIE, DASS EINIGE KNOTEN-    *)
   (*   PUNKTE EINEN VERMERK IM FELD NEUMARKIERT TRAGEN.  *)
   (*   DAS SIND KNOTEN, DIE IM LETZTEN DURCHLAUF DAS     *)
   (*   ERSTE MAL BELEGT WURDEN ODER MIT EINER KLEINE-    *)
   (*   REN ZEIT ALS BISHER ERREICHBAR WURDEN.            *)
   (*                                                     *)
   (*   DIESE KNOTEN WERDEN NUN MARKIERT; DIE NEUE        *)
   (*   ZEIT WIRD IN ALLE STRECKENBESCHREIBUNGEN          *)
   (*   UEBERTRAGEN, IN DENEN DIESER KNOTEN ENTHALTEN     *)
   (*   IST (LEICHT AUFZUFINDEN UEBER DIE S-LISTE,        *)
   (*   SIEHE DATENSTRUKTUREN).                           *)
   (*                                                     *)
   (*   IM ZWEITEN TEIL WERDEN FUER ALLE DERART MAR-      *)
   (*   KIERTEN KNOTEN DIE FAHRTEN AUF DEN ENTSPRECHENDEN *)
   (*   STRASSEN DURCHGEFUEHRT, UND ZWAR SO WEIT, BIS     *)
   (*   AN EINER STELLE EINE KLEINERE ZEIT FUER EINEN     *)
   (*   KNOTENPUNKT BEREITS VORHANDEN IST. VON DA AN      *)
   (*   BRINGT ES NICHTS MEHR, DIE STRASSE WEITER         *)
   (*   ZU VERFOLGEN, DA DANN KEINE GERINGEREN ZUGANGS-   *)
   (*   ZEITEN FUER DIE FOLGENDEN KNOTEN MEHR ERREICHT    *)
   (*   WERDEN KOENNEN. FALLS EINE STRASSE ALLERDINGS     *)
   (*   DAS ERSTE MAL BETRETEN WURDE, WIRD SIE BIS        *)
   (*   ZUM ENDE DURCHFAHREN.                             *)
   (*                                                     *)
   (*   NACHDEM EIN KNOTENPUNKT ABGEARBEITET WURDE,       *)
   (*   WIRD SEINE MARKIERUNG GELOESCHT.                  *)
   (*                                                     *)
   (*   KNOTENPUNKTE, DIE BEI DIESEM SCHRITT DAS ERSTE    *)
   (*   MAL ODER MIT EINER KLEINEREN GESAMTZEIT BEFAH-    *)
   (*   REN WURDEN, WERDEN NEU MARKIERT, UM BEIM          *)
   (*   NAECHSTEN DURCHLAUF BERUECKSICHTIGT WERDEN ZU     *)
   (*   KOENNEN.                                          *)
   (*                                                     *)
   (*   DIE VARIABLE UEBRIG GIBT AN, OB NOCH NEUMAR-      *)
   (*   KIERUNGEN VORHANDEN WAREN. WENN SIE FALSE IST,    *)
   (*   IST DIE BERECHNUNG ZU ENDE UND ALLE WEGE VOM      *)
   (*   VORGEGEBENEN QUELLBEZIRK AUS SIND ERMITTELT.      *)
   (*                                                     *)
   (*******************************************************)


      var VP : VPTYP ;
          VPNEU : VPTYP ;
          S : SLISTTYP ;
          STP : STPTYP ;
          ABBRUCH : BOOLEAN ;
          ZEIT : REAL ;

      begin (* ARBEITEN *)
        UEBRIG := FALSE ;
        for VP := 1 to VPZAHL do
          begin

        (**********************************)
        (*  AB HIER WERDEN DIE NEU        *)
        (*  MARKIERTEN KNOTEN MARKIERT.   *)
        (**********************************)

            if IVSUCHTAB [ VP ] . NEUMARKIERT then
              begin
                UEBRIG := TRUE ;
                IVSUCHTAB [ VP ] . MARKIERT := TRUE ;
                IVSUCHTAB [ VP ] . NEUMARKIERT := FALSE ;
                S := VPSTRLISTE [ VP ] ;
                ZEIT := IVSUCHTAB [ VP ] . ZEITGES ;
                while S <> NIL do
                  begin
                    STP := S -> . VPSTRPOINT ;
                    if not STP -> . BELEGT or ( ZEIT < STP -> .
                    ZEITBISHIER ) then
                      begin
                        STP -> . ZEITBISHIER := ZEIT ;
                        STP -> . BELEGT := TRUE ;
                        STP -> . VKPSTART := VP
                      end (* then *) ;
                    S := S -> . NEXT
                  end (* while *)
              end (* then *)
          end (* for *) ;
        if UEBRIG then
          begin
            for VP := 1 to VPZAHL do
              begin

        (*****************************************)
        (*  VON DEN MARKIERTEN KNOTEN AUS WIRD   *)
        (*  VERSUCHT, EINE FAHRT AUF DEN BETEI-  *)
        (*  LIGTEN STRASSEN DURCHZUFUEHREN.      *)
        (*****************************************)

                if IVSUCHTAB [ VP ] . MARKIERT then
                  begin
                    S := VPSTRLISTE [ VP ] ;
                    while S <> NIL do
                      begin
                        ZEIT := IVSUCHTAB [ VP ] . ZEITGES ;
                        STP := S -> . VPSTRPOINT ;
                        ABBRUCH := STP -> . NEXT = NIL ;
                        while not ABBRUCH do
                          begin
                            ZEIT := ZEIT - STP -> . ZEITABANFANG ;
                            STP := STP -> . NEXT ;
                            ZEIT := ZEIT + STP -> . ZEITABANFANG ;
                            VPNEU := STP -> . VKPUNKT ;
                            if IVSUCHTAB [ VPNEU ] . BELEGT then
                              ABBRUCH := ( IVSUCHTAB [ VPNEU ] .
                                         ZEITGES <= ZEIT ) ;
                            if not ABBRUCH then
                              begin
                                STP -> . BELEGT := TRUE ;
                                STP -> . ZEITBISHIER := ZEIT ;
                                STP -> . VKPSTART := VP ;
                                IVSUCHTAB [ VPNEU ] . NEUMARKIERT :=
                                                   TRUE ;
                                IVSUCHTAB [ VPNEU ] . BELEGT := TRUE ;
                                IVSUCHTAB [ VPNEU ] . ZEITGES := ZEIT ;
                                ABBRUCH := ( STP -> . NEXT = NIL )
                              end (* then *)
                          end (* while *) ;
                        S := S -> . NEXT
                      end (* while *) ;
                    IVSUCHTAB [ VP ] . MARKIERT := FALSE
                  end (* then *)
              end (* for *)
          end (* then *)
      end (* ARBEITEN *) ;


   procedure WEGDRUCK ( VP : VPTYP ) ;

   (***************************************************)
   (*                                                 *)
   (*   DIE PROZEDUR WEGDRUCK BEKOMMT ALS PARAMETER   *)
   (*   DEN LETZTEN KNOTENPUNKT (DER, DER EINEN       *)
   (*   ANSCHLUSS AN DEN ZIELBEZIRK BESITZT UND       *)
   (*   BEI DEM DIE FAHRT BEENDET WURDE). SIE         *)
   (*   VERFOLGT VON DA AUS DEN WEG ZURUECK BIS       *)
   (*   ZUM AUSGANGSBEZIRK UND DRUCKT DIE DABEI       *)
   (*   ENTSTEHENDEN INFORMATIONEN (VOR ALLEM         *)
   (*   DIE STRASSENBEZEICHNUNGEN DER BEFAHRENEN      *)
   (*   STRASSEN UND DIE ZEITEN).                     *)
   (*                                                 *)
   (*   SIE WIRD NUR DANN AUFGERUFEN, WENN DIE        *)
   (*   VARIABLE -TESTMODUS- (IM HAUPTPROGRAMM        *)
   (*   DEFINIERT) TRUE IST.                          *)
   (*                                                 *)
   (***************************************************)


      var ABBRUCH : BOOLEAN ;
          S : SLISTTYP ;
          STP : STPTYP ;

      begin (* WEGDRUCK *)
        WRITELN ( TESTDRUCK , ' IV-ROUTSUCH : VON BEZIRK' , BTRANSFER [
                  QBEZ ] : 6 , ' NACH BEZIRK' , BTRANSFER [ ZBEZ ] : 6
                  ) ;
        WRITELN ( TESTDRUCK ) ;
        WRITE ( TESTDRUCK , ' ' : 25 ) ;
        WRITEREAL ( TESTDRUCK , ZEITG , 8 , 2 ) ;
        WRITELN ( TESTDRUCK ) ;
        repeat
          S := VPSTRLISTE [ VP ] ;
          ABBRUCH := FALSE ;
          while ( S <> NIL ) and not ABBRUCH do
            begin
              STP := S -> . VPSTRPOINT ;
              if STP -> . VKPUNKT <> STP -> . VKPSTART then
                ABBRUCH := TRUE
              else
                S := S -> . NEXT
            end (* while *) ;
          if not ABBRUCH then
            begin
              WRITELN ( 'PROGRAMMFEHLER' ) ;
              HALT
            end (* then *)
          else
            begin
              WRITE ( TESTDRUCK , ' ' ) ;
              WRITE ( TESTDRUCK , VPTRANSFER [ STP -> . VKPUNKT ] : 6 )
                      ;
              WRITE ( TESTDRUCK , '   ' ) ;
              if STP -> . VKPSTART <> 0 then
                SBEZAUSG ( TESTDRUCK , STRASSENNETZ [ S -> . VPSTRASSE
                           ] , 15 )
              else
                WRITE ( TESTDRUCK , ' ' : 15 ) ;
              WRITEREAL ( TESTDRUCK , STP -> . ZEITBISHIER , 8 , 2 ) ;
              WRITELN ( TESTDRUCK ) ;
              VP := STP -> . VKPSTART
            end (* else *)
        until VP = 0 ;
        WRITELN ( TESTDRUCK ) ;
        WRITE ( TESTDRUCK , ' GEFAHRENE PKW-KM:' ) ;
        WRITEREAL ( TESTDRUCK , GESAMTKM , 7 , 3 ) ;
        WRITE ( TESTDRUCK , '      IO:' ) ;
        WRITEREAL ( TESTDRUCK , IOKM , 7 , 3 ) ;
        WRITE ( TESTDRUCK , '      AO:' ) ;
        WRITEREAL ( TESTDRUCK , AOKM , 7 , 3 ) ;
        WRITELN ( TESTDRUCK ) ;
        WRITELN ( TESTDRUCK )
      end (* WEGDRUCK *) ;


   procedure KMRECHNUNG ( AP : APTYP ; var AOKM : REAL ; var IOKM :
                        REAL ) ;

   (***************************************************)
   (*                                                 *)
   (*   BERECHNET GEFAHRENE PKW-KILOMETER             *)
   (*                                                 *)
   (*   DIE STRECKENLAENGE BEI DEN ANSCHLUESSEN       *)
   (*   WIRD UEBER DIE BENOETIGTE ZEIT UND DIE        *)
   (*   (ALS KONSTANTEN DEFINIERTEN) STANDARD-        *)
   (*   GESCHWINDIGKEITEN FUER ANSCHLUESSE            *)
   (*   (INNERORTS UND AUSSERORTS) ERMITTELT.         *)
   (*                                                 *)
   (*   WIE BEI DER PROCEDURE -WEGDRUCK- WIRD         *)
   (*   DER GEFAHRENE WEG REKONSTRUIERT UND           *)
   (*   DABEI AUS DEN STRECKENBESCHREIBUNGEN          *)
   (*   DIE WEGLAENGE AUFSUMMIERT. EIN UMWEGFAKTOR    *)
   (*   IST SOMIT NICHT ZU BERUECKSICHTIGEN.          *)
   (*                                                 *)
   (***************************************************)


      var ABBRUCH : BOOLEAN ;
          S : SLISTTYP ;
          STP : STPTYP ;
          STP2 : STPTYP ;
          STR : STRTYP ;
          VPS : VPTYP ;
          VP : VPTYP ;

      begin (* KMRECHNUNG *)
        AOKM := 0.0 ;
        IOKM := 0.0 ;
        if AP -> . INORTS then
          IOKM := IOKM + IOGESCHW * AP -> . ANFAHRZ [ SITAKTUELL ] /
                  3600
        else
          AOKM := AOKM + AOGESCHW * AP -> . ANFAHRZ [ SITAKTUELL ] /
                  3600 ;
        VP := VPTRANS ( AP -> . ANSPUNKT ) ;
        repeat
          S := VPSTRLISTE [ VP ] ;
          ABBRUCH := FALSE ;
          while ( S <> NIL ) and not ABBRUCH do
            begin
              STP := S -> . VPSTRPOINT ;
              STR := S -> . VPSTRASSE ;
              if STP -> . VKPUNKT <> STP -> . VKPSTART then
                ABBRUCH := TRUE
              else
                S := S -> . NEXT
            end (* while *) ;
          if not ABBRUCH then
            begin
              WRITELN ( 'PROGRAMMFEHLER' ) ;
              HALT
            end (* then *)
          else
            begin
              VPS := STP -> . VKPSTART ;
              if VPS <> 0 then
                begin
                  STP2 := STRASSENNETZ [ STR ] . ANFANG ;
                  while STP2 -> . VKPUNKT <> VPS do
                    STP2 := STP2 -> . NEXT ;
                  while STP2 <> STP do
                    begin
                      if STP2 -> . INNERORTS then
                        IOKM := IOKM + STP2 -> . DISTANZ / 1000
                      else
                        AOKM := AOKM + STP2 -> . DISTANZ / 1000 ;
                      STP2 := STP2 -> . NEXT
                    end (* while *)
                end (* then *)
              else
                begin
                  AP := ANSCHLUESSE [ QBEZ ] ;
                  while VPTRANS ( AP -> . ANSPUNKT ) <> VP do
                    AP := AP -> . NEXT ;
                  if AP -> . INORTS then
                    IOKM := IOKM + IOGESCHW * AP -> . ANFAHRZ [
                            SITAKTUELL ] / 3600
                  else
                    AOKM := AOKM + AOGESCHW * AP -> . ANFAHRZ [
                            SITAKTUELL ] / 3600 ;
                end (* else *) ;
              VP := VPS
            end (* else *)
        until VP = 0 ;
      end (* KMRECHNUNG *) ;


   begin (* IVROUTSUCH *)
     AOKM := 0.0 ;
     IOKM := 0.0 ;
     GESAMTKM := 0.0 ;
     if IVQBEZALTVORH and ( QBEZ = IVQBEZALT ) then

     (*****************************************************)
     (*   FALLS DER NEUE QUELLBEZIRK GLEICH DEM ALTEN     *)
     (*   IST, SIND DIE WERTE IN DER SUCHTABELLE UND IN   *)
     (*   DEN STRECKENBESCHREIBUNGEN NOCH GUELTIG. DIE    *)
     (*   OPTIMALE ROUTE KANN DANN DIREKT ABGELESEN WER-  *)
     (*   DEN.                                            *)
     (*****************************************************)

       begin
         GEFUNDEN := FALSE ;
         ZEITG := 0.0
       end (* then *)
     else

     (*****************************************************)
     (*   ANSONSTEN WERDEN DIE INFORMATIONEN MITTELS      *)
     (*   -IVSUCHTABLOE- GELOESCHT UND VON DEM NEUEN      *)
     (*   QUELLBEZIRK AUS NEU ERMITTELT.                  *)
     (*****************************************************)

       begin
         IVSUCHTABLOE ;
         GEFUNDEN := FALSE ;
         ZEITG := 0.0 ;
         AP := ANSCHLUESSE [ QBEZ ] ;
         while AP <> NIL do
           begin

     (*****************************************************)
     (*   DAZU WERDEN ZUERST DIE KNOTENPUNKTE, DIE EINEN  *)
     (*   ANSCHLUSS AN DEN QUELLBEZIRK BESITZEN, NEU      *)
     (*   MARKIERT. DIE ANFAHRZEITEN WERDEN IN DIE        *)
     (*   STRECKENBESCHREIBUNGEN UEBERTRAGEN.             *)
     (*****************************************************)

             VP := VPTRANS ( AP -> . ANSPUNKT ) ;
             ZEIT := AP -> . ANFAHRZ [ SITAKTUELL ] / 60 ;
             IVSUCHTAB [ VP ] . NEUMARKIERT := TRUE ;
             IVSUCHTAB [ VP ] . BELEGT := TRUE ;
             IVSUCHTAB [ VP ] . ZEITGES := ZEIT ;
             S := VPSTRLISTE [ VP ] ;
             while S <> NIL do
               begin
                 STP := S -> . VPSTRPOINT ;
                 STP -> . ZEITBISHIER := ZEIT ;
                 STP -> . BELEGT := TRUE ;
                 STP -> . VKPSTART := 0 ;
                 S := S -> . NEXT
               end (* while *) ;
             AP := AP -> . NEXT
           end (* while *) ;

     (*****************************************************)
     (*   HIER WIRD DANN DIE SCHLEIFE BETRETEN, DIE DIE   *)
     (*   PROZEDUR ARBEITEN SOLANGE AUFRUFT, BIS NICHTS   *)
     (*   MEHR UEBRIG IST.                                *)
     (*****************************************************)

         repeat
           ARBEITEN ( UEBRIG ) ;
         until not UEBRIG
       end (* else *) ;
     VPFOUND := 0 ;
     APFOUND := NIL ;
     AP := ANSCHLUESSE [ ZBEZ ] ;
     while AP <> NIL do
       begin

     (*****************************************************)
     (*   DERJENIGE KNOTEN MIT ANSCHLUSS AN DEN ZIELBE-   *)
     (*   ZIRK WIRD GESUCHT, DER - ZUSAMMEN MIT DER AN-   *)
     (*   FAHRZEIT - DIE KLEINSTE GESAMTZEIT LIEFERT.     *)
     (*****************************************************)

         VP := VPTRANS ( AP -> . ANSPUNKT ) ;
         if IVSUCHTAB [ VP ] . BELEGT then
           begin
             ZEIT := AP -> . ANFAHRZ [ SITAKTUELL ] / 60 + IVSUCHTAB [
                     VP ] . ZEITGES ;
             if not GEFUNDEN or ( ZEIT < ZEITG ) then
               begin
                 GEFUNDEN := TRUE ;
                 ZEITG := ZEIT ;
                 VPFOUND := VP ;
                 APFOUND := AP
               end (* then *)
           end (* then *) ;
         AP := AP -> . NEXT
       end (* while *) ;

     (*****************************************************)
     (*   DIE INFORMATION UEBER DEN ALTEN QUELLBEZIRK     *)
     (*   WIRD NEU BELEGT. MIT DER PROZEDUR -KMRECHNUNG-  *)
     (*   WIRD DIE LAENGE DES GEFAHRENEN WEGES BESTIMMT   *)
     (*   (FUER DIE ERMITTLUNG DER SALDO-PKW-KILOMETER).  *)
     (*   FALLS DIE VARIABLE -TESTMODUS- = TRUE IST,      *)
     (*   WIRD DIE PROZEDUR WEGDRUCK AUFGERUFEN.          *)
     (*****************************************************)

     IVQBEZALTVORH := TRUE ;
     IVQBEZALT := QBEZ ;
     if GEFUNDEN then
       begin
         KMRECHNUNG ( APFOUND , AOKM , IOKM ) ;
         GESAMTKM := AOKM + IOKM
       end (* then *) ;
     if TESTMODUS then
       begin
         WRITE ( 'IV-ROUTENSUCHE:  ' ) ;
         WRITE ( BTRANSFER [ QBEZ ] : 6 , ' -> ' , BTRANSFER [ ZBEZ ] :
                 6 ) ;
         WRITE ( '    ZEIT=' ) ;
         WRITEREAL ( OUTPUT , ZEITG , 8 , 2 ) ;
         WRITE ( ' MIN' ) ;
         WRITELN ;
         if GEFUNDEN then
           WEGDRUCK ( VPFOUND )
       end (* then *)
   end (* IVROUTSUCH *) ;



procedure IVKONSISTENZ ( var RICHTIG : BOOLEAN ) ;

(*******************************************************************)
(*                                                                 *)
(*   IV-KONSISTENZPRUEFUNG NACH LADEN DES IV-MODELLS.              *)
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
       SIT : SITUATIONEN ;


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
            WRITELN ( TESTDRUCK , 'IV-KONSISTENZPRUEFUNG:' ) ;
            WRITELN ( TESTDRUCK ) ;
            ERSTERFEHLER := FALSE
          end (* then *) ;
        if ( NR = 5 ) or ( NR = 6 ) then
          WRITE ( TESTDRUCK , 'IN ANSCHLUSS ZU' , BTRANSFER [ I ] : 10
                  , ' : ' ) ;
        if ( NR >= 17 ) and ( NR <= 20 ) then
          begin
            WRITE ( TESTDRUCK , 'IN STRASSE ' ) ;
            SBEZAUSG ( TESTDRUCK , STR , 14 ) ;
            WRITE ( TESTDRUCK , ' : ' )
          end (* then *) ;
        case NR of
          1 : WRITELN ( TESTDRUCK , 'BEZIRKE NICHT EINDEUTIG ' ,
                        'ODER NICHT AUFSTEIGEND' ) ;
          2 : WRITELN ( TESTDRUCK , 'BEZIRKE UEBER BZAHL UNGLEICH 0' )
                        ;
          3 : WRITELN ( TESTDRUCK , 'KNOTENPUNKTE NICHT EINDEUTIG ' ,
                        'ODER NICHT AUFSTEIGEND' ) ;
          4 : WRITELN ( TESTDRUCK ,
                        'KNOTENPUNKTE UEBER VPZAHL UNGLEICH 0' ) ;
          5 : WRITELN ( TESTDRUCK , 'KNOTENPUNKT NICHT VORHANDEN' ) ;
          6 : WRITELN ( TESTDRUCK , 'KNOTENPUNKT NICHT EINDEUTIG' ) ;
          9 : WRITELN ( TESTDRUCK , 'STRASSEN NICHT EINDEUTIG ' ,
                        'ODER NICHT AUFSTEIGEND' ) ;
          14 : WRITELN ( TESTDRUCK , 'BEZIRK KLEINER 0 NICHT ERLAUBT' )
                         ;
          15 : WRITELN ( TESTDRUCK ,
                         'KNOTENPUNKT KLEINER 0 NICHT ERLAUBT' ) ;
          16 : WRITELN ( TESTDRUCK , 'STRASSENNAME UNZULAESSIG' ) ;
          17 : WRITELN ( TESTDRUCK , 'STRASSENAUFBAU UNZULAESSIG' ) ;
          18 : WRITELN ( TESTDRUCK , 'KNOTENPUNKT NICHT VORHANDEN' ) ;
          19 : WRITELN ( TESTDRUCK , 'FALSCHE KATEGORIE' ) ;
          20 : WRITELN ( TESTDRUCK , 'UNZULAESSIGER GESCHW.-ABSCHLAG' )
                         ;
        end (* case *)
      end (* FEHLER *) ;


   begin (* IVKONSISTENZ *)
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
                 VPTEST := BPTR -> . ANSPUNKT ;
                 SEARCHVP ( VPTEST , VPIND , STATUS ) ;
                 if STATUS <> 0 then
                   FEHLER ( 5 ) ;
                 BPTR2 := BKOPF ;
                 while BPTR2 <> BPTR do
                   begin
                     if BPTR2 -> . ANSPUNKT = VPTEST then
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
                 VPTEST := VPTRANSFER [ STP -> . VKPUNKT ] ;
                 SEARCHVP ( VPTEST , VPIND , STATUS ) ;
                 if STATUS <> 0 then
                   FEHLER ( 18 ) ;
                 if STP -> . NEXT <> NIL then
                   begin
                     if not ( STP -> . KAT in [ 1 .. KATMAX ] ) then
                       FEHLER ( 19 ) ;
                     for SIT := SITMIN to SITMAX do
                       begin
                         if ( STP -> . GESCHWABSCHL [ SIT ] < 0 ) or (
                         STP -> . GESCHWABSCHL [ SIT ] >= 100 ) then
                           FEHLER ( 20 )
                       end (* for *)
                   end (* then *) ;
                 STP := STP -> . NEXT
               end (* while *)
           end (* else *)
       end (* for *)
   end (* IVKONSISTENZ *) ;



procedure IVZEITRECHNUNG ;

(****************************************************)
(*                                                  *)
(*   UM DIE ZEITBERECHNUNG BEI DER IV-ROUTENSUCHE   *)
(*   ZU BESCHLEUNIGEN, WERDEN HIER (NACH DEM        *)
(*   LADEN DES IV-MODELLS) DIE FAHRZEITEN FUER      *)
(*   JEDE STRASSE AB ANFANG BERECHNET. DAFUER       *)
(*   WERDEN DIE FELDER -DISTANZ- UND -KAT-          *)
(*   BENOETIGT. DIE FAHRZEIT VON EINEM KNOTEN ZU    *)
(*   EINEM ANDEREN ERGIBT SICH AUS DER DIFFERENZ    *)
(*   DER BEIDEN FELDER -ZEITABANFANG-.              *)
(*                                                  *)
(****************************************************)


   var I : STRTYP ;
       ZEIT , FAHRZ , GESCHW : REAL ;
       STP : STPTYP ;

   begin (* IVZEITRECHNUNG *)
     for I := 1 to STRZAHL do
       begin
         ZEIT := 0.0 ;
         STP := STRASSENNETZ [ I ] . ANFANG ;
         STP -> . ZEITABANFANG := ZEIT ;
         while STP -> . NEXT <> NIL do
           begin
             GESCHW := VMAX [ STP -> . KAT ] * ( 100 - STP -> .
                       GESCHWABSCHL [ SITAKTUELL ] ) / 100 ;
             FAHRZ := ( 3.6 * STP -> . DISTANZ ) / ( 60 * GESCHW ) ;
             ZEIT := ZEIT + FAHRZ ;
             STP := STP -> . NEXT ;
             STP -> . ZEITABANFANG := ZEIT
           end (* while *)
       end (* for *)
   end (* IVZEITRECHNUNG *) ;



procedure SLISTERSTELLEN ;

(*******************************************************)
(*                                                     *)
(*   EBENFALLS ZUR BESCHLEUNIGUNG DER IV-ROUTENSUCHE   *)
(*   DIENT DIE STRASSENLISTE. JE KNOTENPUNKT WERDEN    *)
(*   IN EINER VERKETTETEN LISTE ALLE STRASSEN          *)
(*   ABGELEGT, AUF DENEN DIESER KNOTENPUNKT LIEGT.     *)
(*   DIE STRASSEN WERDEN DURCH IHREN INDEX IM FELD     *)
(*   -STRASSENNETZ- VOM TYP STRTYP IDENTIFIZIERT.      *)
(*   ZUSAETZLICH WIRD NOCH (ZUM SCHNELLEN AUFFINDEN    *)
(*   DES KONTENPUNKTES INNERHALB DER STRECKEN-         *)
(*   BESCHREIBUNG) EIN POINTER VOM TYP STPTYP MIT      *)
(*   ABGELEGT, DER AUF DIESEN STRECKENPUNKT INNERHALB  *)
(*   DER STRECKENBESCHREIBUNG DER BETREFFENDEN         *)
(*   STRASSE VERWEIST.                                 *)
(*                                                     *)
(*******************************************************)


   var VP : VPTYP ;
       I : STRTYP ;
       STP : STPTYP ;
       S : SLISTTYP ;

   begin (* SLISTERSTELLEN *)
     for VP := 1 to VPZAHL do
       VPSTRLISTE [ VP ] := NIL ;
     for I := 1 to STRZAHL do
       begin
         STP := STRASSENNETZ [ I ] . ANFANG ;
         while STP <> NIL do
           begin
             VP := STP -> . VKPUNKT ;
             if VPSTRLISTE [ VP ] = NIL then
               begin
                 NEW ( S ) ;
                 VPSTRLISTE [ VP ] := S
               end (* then *)
             else
               begin
                 S := VPSTRLISTE [ VP ] ;
                 while S -> . NEXT <> NIL do
                   S := S -> . NEXT ;
                 NEW ( S -> . NEXT ) ;
                 S := S -> . NEXT
               end (* else *) ;
             S -> . NEXT := NIL ;
             S -> . VPSTRASSE := I ;
             S -> . VPSTRPOINT := STP ;
             STP := STP -> . NEXT
           end (* while *)
       end (* for *)
   end (* SLISTERSTELLEN *) ;



procedure IVVORBESETZEN ;

(*******************************************************)
(*   VORBESETZUNGEN IM IV-BEREICH                      *)
(*                                                     *)
(*   VORBESETZT WERDEN:                                *)
(*   - DIE DATEN UEBER DEN ALTEN QUELLBEZIRK           *)
(*   - DIE GESCHWINDIGKEITEN JE KATEGORIE (VMAX)       *)
(*******************************************************)


   begin (* IVVORBESETZEN *)
     IVQBEZALT := 0 ;
     IVQBEZALTVORH := FALSE ;
     KENNMENGE := [ 'A' , 'B' , 'K' , 'L' , 'S' ] ;
     RICHTMENGE := [ 'H' , 'R' ] ;
     SITMIN := HAUPT ;
     SITMAX := SPAET ;
     SITTEXTE [ NORMAL ] := 'NORMAL' ;
     SITTEXTE [ HAUPT ] := 'HAUPT ' ;
     SITTEXTE [ SPAET ] := 'SPAET ' ;
     VMAX [ 1 ] := 130 ;
     VMAX [ 2 ] := 120 ;
     VMAX [ 3 ] := 100 ;
     VMAX [ 4 ] := 80 ;
     VMAX [ 5 ] := 70 ;
     VMAX [ 6 ] := 60 ;
     VMAX [ 7 ] := 50 ;
     VMAX [ 8 ] := 40 ;
   end (* IVVORBESETZEN *) ;



procedure NEXTWORT ( var WORT : FNAME ) ;

(*********************************************)
(*   DIE PROZEDUR NEXTWORT LIEST AUS DEM     *)
(*   START-STRING SSTRING (DORT STEHT DIE    *)
(*   PARAMETERVERSORGUNG) DAS NAECHSTE       *)
(*   BIS ZU 8-STELLIGE WORT (IN DER REGEL    *)
(*   DER NAME EINES MODELLFILES).            *)
(*********************************************)


   var I : INTEGER ;

   begin (* NEXTWORT *)
     WORT := '        ' ;
     while SSTRING [ SX ] = ' ' do
       SX := SX + 1 ;
     I := 1 ;
     repeat
       if I <= 8 then
         WORT [ I ] := SSTRING [ SX ] ;
       I := I + 1 ;
       SX := SX + 1
     until SSTRING [ SX ] = ' '
   end (* NEXTWORT *) ;



procedure MODUS1 ;

(***********************************************************)
(*                                                         *)
(*   DIE PROZEDUR MODUS1 DIENT ZUR ABWICKLUNG DER          *)
(*   BERECHNUNG IM MODUS 1.                                *)
(*                                                         *)
(*   AUS DER PARAMETER-VERSORGUNG BEI AUFRUF DES           *)
(*   PROGRAMMS WERDEN DIE NAMEN DER BETEILIGTEN            *)
(*   MODELLE GEHOLT. DANACH WIRD DIE EINGABEDATEI          *)
(*   SATZ FUER SATZ GELESEN.                               *)
(*                                                         *)
(*   IN EINEM SATZ BEFINDEN SICH FOLGENDE ANGABEN:         *)
(*                                                         *)
(*   QUELLBEZIRK   ZIELBEZIRK   GESAMTVERKEHR.             *)
(*                                                         *)
(*   WENN DIE BEZIRKE IM JEWEILIGEN MODELL VORHANDEN       *)
(*   SIND, WIRD UEBER DIE OEV-ROUTENSUCHE UND DIE          *)
(*   IV-ROUTENSUCHE DER JEWEILS KUERZESTE WEG ERMITTELT.   *)
(*   UEBER DAS REISEZEITVERHAELTNIS WIRD - MITHILFE        *)
(*   DER MODAL-SPLIT-FUNKTION - DER ZU ERWARTENDE          *)
(*   OEV- BZW. IV-ANTEIL ERMITTELT.                        *)
(*                                                         *)
(*   FOLGENDE INFORMATIONEN WERDEN AUSGEDRUCKT:            *)
(*                                                         *)
(*   - DIE EINGABEDATEN,                                   *)
(*   - DIE REISEZEITEN, DAS REISEZEITVERHAELTNIS,          *)
(*   - DER OEV-PROZENTSATZ ANHAND DER MS-FUNKTION SOWIE    *)
(*   - DIE OEV- UND IV-ANTEILE.                            *)
(*                                                         *)
(***********************************************************)


   var QUELLE , ZIEL , GV : INTEGER ;


   procedure FEHLERERKLAERUNG ;

   (**************************************************)
   (*   GIBT AM UNTEREN BLATTRAND DES ERGEBNIS-      *)
   (*   LISTINGS EINE ERLAEUTERUNG DER MOEGLICHER-   *)
   (*   WEISE AUFGETRETENEN FEHLER AUS               *)
   (**************************************************)


      begin (* FEHLERERKLAERUNG *)
        WRITELN ( AUSGABE ) ;
        WRITELN ( AUSGABE ) ;
        WRITE ( AUSGABE , ' ' ) ;
        WRITE ( AUSGABE , '(FEHLERKENNZEICHEN: ' ) ;
        WRITE ( AUSGABE , '1 = QUELL- UND ZIELBEZIRK GLEICH, ' ) ;
        WRITE ( AUSGABE , '2 = QUELLBEZIRK NICHT VORHANDEN, ' ) ;
        WRITE ( AUSGABE , '3 = ZIELBEZIRK NICHT VORHANDEN)' ) ;
        WRITELN ( AUSGABE )
      end (* FEHLERERKLAERUNG *) ;


   procedure VERARBEITEN ;

      var OEVGEFUNDEN , IVGEFUNDEN : BOOLEAN ;
          FEHLER : INTEGER ;
          QBEZ , ZBEZ : BTYP ;
          OEVZEIT , IVZEIT : REAL ;
          RZV : REAL ;
          OEVPROZ : REAL ;
          OEVANTEIL , IVANTEIL : INTEGER ;
          STATUS : STAT ;
          DUMMYAO , DUMMYIO , DUMMYGESAMT : REAL ;


      procedure UEBERSCHRIFT ;

      (***************************************************)
      (*   GIBT EINE UEBERSCHRIFT AUF DEN LISTFILE AUS.  *)
      (*   IN DER UEBERSCHRIFT STEHEN UNTER ANDEREM DIE  *)
      (*   NAMEN DER BETEILIGTEN FILES, ALSO DIE         *)
      (*   MODELLNAMEN.                                  *)
      (***************************************************)


         begin (* UEBERSCHRIFT *)
           if SEITENZAHL <> 0 then
             FEHLERERKLAERUNG ;
           WRITE ( AUSGABE , '1' ) ;
           WRITE ( AUSGABE ,
                   'M O D A L - S P L I T - A U S W E R T U N G' ) ;
           WRITE ( AUSGABE , ' ' : 26 ) ;
           SEITENZAHL := SEITENZAHL + 1 ;
           WRITE ( AUSGABE , ' ' : 48 , 'SEITE' , SEITENZAHL : 4 ) ;
           WRITELN ( AUSGABE ) ;
           WRITELN ( AUSGABE ) ;
           WRITE ( AUSGABE , ' ' ) ;
           WRITE ( AUSGABE ,
                   'ERMITTLUNG DER OEV- UND IV-ANTEILE ANHAND DES' ) ;
           WRITE ( AUSGABE , ' REISEZEITVERHAELTNISSES' ) ;
           WRITELN ( AUSGABE ) ;
           WRITELN ( AUSGABE ) ;
           WRITE ( AUSGABE , ' ' ) ;
           WRITE ( AUSGABE , 'OEV-MODELL: ' , OEVNAME , ' ' : 6 ) ;
           WRITE ( AUSGABE , 'IV-MODELL: ' , IVNAME , ' ' : 6 ) ;
           WRITE ( AUSGABE , 'MODAL-SPLIT-FUNKTION: ' , MSFNAME , ' ' :
                   6 ) ;
           WRITE ( AUSGABE , 'TAGESZEIT: ' , SITTEXTE [ SITAKTUELL ] )
                   ;
           WRITELN ( AUSGABE ) ;
           WRITELN ( AUSGABE ) ;
           WRITELN ( AUSGABE ) ;
           WRITE ( AUSGABE , ' ' ) ;
           WRITE ( AUSGABE , '  QUELLBEZ   ZIELBEZ  GES-VERK' ) ;
           WRITE ( AUSGABE , '  OEV-ZEIT   IV-ZEIT       RZV' ) ;
           WRITE ( AUSGABE , '  OEV-PROZ   OEV-ANT    IV-ANT' ) ;
           WRITE ( AUSGABE , '      FEHLER' ) ;
           WRITELN ( AUSGABE ) ;
           WRITELN ( AUSGABE ) ;
           ZEILZAHL := 9
         end (* UEBERSCHRIFT *) ;


      procedure DRUCKEZEILE ;

         begin (* DRUCKEZEILE *)
           WRITELN ( AUSGABE ) ;
           ZEILZAHL := ZEILZAHL + 1 ;
           if ZEILZAHL > 61 then
             UEBERSCHRIFT
         end (* DRUCKEZEILE *) ;


      begin (* VERARBEITEN *)
        if SEITENZAHL = 0 then
          UEBERSCHRIFT ;
        OEVGEFUNDEN := TRUE ;
        IVGEFUNDEN := TRUE ;
        FEHLER := 0 ;
        if QUELLE = ZIEL then
          FEHLER := 1
        else
          begin
            SEARCHB ( QUELLE , QBEZ , STATUS ) ;
            if STATUS <> 0 then
              FEHLER := 2
            else
              begin
                SEARCHB ( ZIEL , ZBEZ , STATUS ) ;
                if STATUS <> 0 then
                  FEHLER := 3
                else
                  begin
                    if EINBIND [ QBEZ ] = NIL then
                      OEVGEFUNDEN := FALSE
                    else
                      begin
                        if EINBIND [ ZBEZ ] = NIL then
                          OEVGEFUNDEN := FALSE
                        else
                          begin
                            OEVROUTSUCH ( QBEZ , ZBEZ , OEVZEIT ,
                                          OEVGEFUNDEN ) ;
                          end (* else *)
                      end (* else *) ;
                    if ANSCHLUESSE [ QBEZ ] = NIL then
                      IVGEFUNDEN := FALSE
                    else
                      begin
                        if ANSCHLUESSE [ ZBEZ ] = NIL then
                          IVGEFUNDEN := FALSE
                        else
                          begin
                            IVROUTSUCH ( QBEZ , ZBEZ , IVZEIT , DUMMYAO
                                         , DUMMYIO , DUMMYGESAMT ,
                                         IVGEFUNDEN ) ;
                          end (* else *)

        (**********************************)
        (*   DIE VARIABLEN DUMMY..        *)
        (*   NEHMEN DIE STRECKENLAENGEN   *)
        (*   AUF, DIE IM MODUS 1 NICHT    *)
        (*   GEBRAUCHT WERDEN.            *)
        (**********************************)

                      end (* else *)
                  end (* else *)
              end (* else *)
          end (* else *) ;
        if FEHLER <> 0 then
          begin
            OEVGEFUNDEN := FALSE ;
            IVGEFUNDEN := FALSE
          end (* then *) ;
        WRITE ( AUSGABE , ' ' ) ;
        WRITE ( AUSGABE , QUELLE : 10 ) ;
        WRITE ( AUSGABE , ZIEL : 10 ) ;
        WRITE ( AUSGABE , GV : 10 ) ;
        if OEVGEFUNDEN then
          WRITEREAL ( AUSGABE , OEVZEIT , 10 , 2 )
        else
          if FEHLER = 0 then
            WRITE ( AUSGABE , '      -NF-' )
          else
            WRITE ( AUSGABE , ' ' : 10 ) ;
        if IVGEFUNDEN then
          WRITEREAL ( AUSGABE , IVZEIT , 10 , 2 )
        else
          if FEHLER = 0 then
            WRITE ( AUSGABE , '      -NF-' )
          else
            WRITE ( AUSGABE , ' ' : 10 ) ;
        if IVGEFUNDEN and OEVGEFUNDEN then
          begin
            RZV := OEVZEIT / IVZEIT ;
            OEVPROZ := MODALSPLIT ( RZV ) ;
            OEVANTEIL := ROUND ( GV * OEVPROZ / 100 ) ;
            IVANTEIL := GV - OEVANTEIL ;
            WRITEREAL ( AUSGABE , RZV , 10 , 4 ) ;
            WRITEREAL ( AUSGABE , OEVPROZ , 10 , 2 ) ;
            WRITE ( AUSGABE , OEVANTEIL : 10 ) ;
            WRITE ( AUSGABE , IVANTEIL : 10 ) ;
          end (* then *)
        else
          WRITE ( AUSGABE , ' ' : 40 ) ;
        if FEHLER <> 0 then
          WRITE ( AUSGABE , FEHLER : 10 , ')' ) ;
        DRUCKEZEILE ;
      end (* VERARBEITEN *) ;


   begin (* MODUS1 *)
     NEXTWORT ( OEVNAME ) ;
     NEXTWORT ( IVNAME ) ;
     NEXTWORT ( MSFNAME ) ;
     repeat
       READ ( EINGABE , QUELLE ) ;
       if QUELLE <> 0 then
         begin
           READ ( EINGABE , ZIEL ) ;
           if ZIEL <> 0 then
             begin
               READ ( EINGABE , GV ) ;
               if GV <> 0 then
                 VERARBEITEN
             end (* then *)
         end (* then *) ;
       READLN ( EINGABE )
     until EOF ( EINGABE ) ;
     while ZEILZAHL <= 61 do
       begin
         ZEILZAHL := ZEILZAHL + 1 ;
         WRITELN ( AUSGABE )
       end (* while *) ;
     FEHLERERKLAERUNG ;
   end (* MODUS1 *) ;



procedure MODUS2 ;

(************************************************************)
(*                                                          *)
(*   DIE PROZEDUREN MODUS2 UND MODUS3 ERLEDIGEN GEMEIN-     *)
(*   SAM DIE ERMITTLUNG DER MODAL-SPLIT-AENDERUNG.          *)
(*                                                          *)
(*   DIE MODAL-SPLIT-AENDERUNG ERGIBT SICH AUS DER          *)
(*   AENDERUNG DES REISEZEITVERHAELTNISSES. DESHALB         *)
(*   WERDEN ZUERST DIE REISEZEITEN IM ALTEN MODELL          *)
(*   BERECHNET. DAS LEISTET DER MODUS 2.                    *)
(*                                                          *)
(*   DIE AUSGABEN VON MODUS 2 WERDEN ZU DEN EINGABEN VON    *)
(*   MODUS 3. MODUS 3 BERECHNET DAS REISEZEITVERHAELTNIS    *)
(*   FUER DIE NEUEN MODELLE UND DARAUS - ZUSAMMEN MIT       *)
(*   DEN ANGABEN AUS MODUS 2 - DIE MODAL-SPLIT-AENDERUNG.   *)
(*                                                          *)
(*   AUFBAU DER EINGABEDATEI IM MODUS 2:                    *)
(*                                                          *)
(*   QUELLBEZIRK  ZIELBEZIRK  OEV-IST-ALT  IV-IST-ALT.      *)
(*                                                          *)
(*   ANHAND DER ALTEN NETZMODELLE WERDEN DIE REISEZEITEN    *)
(*   UND DIE GEFAHRENEN PKW-KILOMETER ERMITTELT UND IN      *)
(*   DIE ZWISCHENDATEI AUSGEGEBEN.                          *)
(*                                                          *)
(*   FOLGENDE INFORMATIONEN WERDEN AUSGEGEBEN:              *)
(*                                                          *)
(*   - DIE BOOLEAN-VARIABLEN -IVGEFUNDEN- UND               *)
(*     -OEVGEFUNDEN-,                                       *)
(*   - DIE EINGABEDATEN,                                    *)
(*   - GGF. DIE REISEZEITEN IN DEN ALTEN MODELLEN           *)
(*     UND DIE GEFAHRENEN PKW-KILOMETER INNERORTS UND       *)
(*     AUSSERORTS (ZUR BERECHNUNG DES SALDOS)               *)
(*                                                          *)
(*   DIE AUSGABE IST GLEICHZEITIG EINGABE FUER MODUS 3.     *)
(*                                                          *)
(************************************************************)


   var QUELLE , ZIEL , GV : INTEGER ;
       IV , OEV : INTEGER ;


   procedure VERARBEITEN ;

      var OEVGEFUNDEN , IVGEFUNDEN : BOOLEAN ;
          FEHLER : INTEGER ;
          QBEZ , ZBEZ : BTYP ;
          OEVZEIT , IVZEIT : REAL ;
          STATUS : STAT ;
          KMAO , KMIO , KMGESAMT : REAL ;

      begin (* VERARBEITEN *)
        OEVGEFUNDEN := TRUE ;
        IVGEFUNDEN := TRUE ;
        FEHLER := 0 ;
        if QUELLE = ZIEL then
          FEHLER := 1
        else
          begin
            SEARCHB ( QUELLE , QBEZ , STATUS ) ;
            if STATUS <> 0 then
              FEHLER := 2
            else
              begin
                SEARCHB ( ZIEL , ZBEZ , STATUS ) ;
                if STATUS <> 0 then
                  FEHLER := 3
                else
                  begin
                    if EINBIND [ QBEZ ] = NIL then
                      OEVGEFUNDEN := FALSE
                    else
                      begin
                        if EINBIND [ ZBEZ ] = NIL then
                          OEVGEFUNDEN := FALSE
                        else
                          begin
                            OEVROUTSUCH ( QBEZ , ZBEZ , OEVZEIT ,
                                          OEVGEFUNDEN ) ;
                          end (* else *)
                      end (* else *) ;
                    if ANSCHLUESSE [ QBEZ ] = NIL then
                      IVGEFUNDEN := FALSE
                    else
                      begin
                        if ANSCHLUESSE [ ZBEZ ] = NIL then
                          IVGEFUNDEN := FALSE
                        else
                          begin
                            IVROUTSUCH ( QBEZ , ZBEZ , IVZEIT , KMAO ,
                                         KMIO , KMGESAMT , IVGEFUNDEN )
                                         ;
                          end (* else *)
                      end (* else *)
                  end (* else *)
              end (* else *)
          end (* else *) ;
        if FEHLER <> 0 then
          begin
            OEVGEFUNDEN := FALSE ;
            IVGEFUNDEN := FALSE
          end (* then *) ;

        (************************************)
        (*   DIE BOOLEAN-WERTE WERDEN       *)
        (*   ALS NULL UND EINS AUSGEGEBEN   *)
        (************************************)

        if OEVGEFUNDEN then
          WRITE ( AUSGABE , 1 : 4 )
        else
          WRITE ( AUSGABE , 0 : 4 ) ;
        if IVGEFUNDEN then
          WRITE ( AUSGABE , 1 : 4 )
        else
          WRITE ( AUSGABE , 0 : 4 ) ;
        WRITE ( AUSGABE , QUELLE : 10 ) ;
        WRITE ( AUSGABE , ZIEL : 10 ) ;
        WRITE ( AUSGABE , OEV : 10 ) ;
        WRITE ( AUSGABE , IV : 10 ) ;
        if OEVGEFUNDEN then
          WRITE ( AUSGABE , OEVZEIT : 12 : 6 ) ;
        if IVGEFUNDEN then
          begin
            WRITE ( AUSGABE , IVZEIT : 12 : 6 ) ;
            WRITE ( AUSGABE , KMAO : 12 : 6 ) ;
            WRITE ( AUSGABE , KMIO : 12 : 6 ) ;
            WRITE ( AUSGABE , KMGESAMT : 12 : 6 )
          end (* then *) ;
        WRITELN ( AUSGABE ) ;
      end (* VERARBEITEN *) ;


   begin (* MODUS2 *)
     repeat
       READ ( EINGABE , QUELLE ) ;
       if QUELLE <> 0 then
         begin
           READ ( EINGABE , ZIEL ) ;
           if ZIEL <> 0 then
             begin
               READ ( EINGABE , OEV ) ;
               READ ( EINGABE , IV ) ;
               GV := IV + OEV ;
               if GV <> 0 then
                 VERARBEITEN
             end (* then *)
         end (* then *) ;
       READLN ( EINGABE )
     until EOF ( EINGABE ) ;
   end (* MODUS2 *) ;



procedure MODUS3 ;

(************************************************************)
(*                                                          *)
(*   DIE PROZEDUREN MODUS2 UND MODUS3 ERLEDIGEN GEMEIN-     *)
(*   SAM DIE ERMITTLUNG DER MODAL-SPLIT-AENDERUNG.          *)
(*                                                          *)
(*   DIE IN MODUS2 ERZEUGTEN AUSGABEN WERDEN - IN EINEM     *)
(*   ZWEITEN LAUF - IN MODUS3 GELESEN UND UNTER ZUHILFE-    *)
(*   NAHME DER NEUEN NETZMODELLE WIRD DIE MODAL-SPLIT-      *)
(*   AENDERUNG ERMITTELT.                                   *)
(*                                                          *)
(*   AUFBAU DER EINGABEDATEI:                               *)
(*                                                          *)
(*   (WIE AUSGABE AUS MODUS2)                               *)
(*                                                          *)
(*   DIE BOOLEAN-VARIABLEN -IVGEFUNDEN- UND -OEVGEFUNDEN-,  *)
(*   DIE EINGABEDATEN AUS MODUS2 (INCL. OEV-ALT UND         *)
(*   IV-ALT), GGF. DIE REISEZEITEN UND DIE GEFAHRENEN       *)
(*   PKW-KILOMETER.                                         *)
(*                                                          *)
(*   ANHAND DER NEUEN NETZMODELLE WERDEN DIE NEUEN          *)
(*   REISEZEITEN ERMITTELT. AUS DEN ALTEN UND NEUEN         *)
(*   REISEZEITEN WIRD DIE AENDERUNG DES REISEZEITVER-       *)
(*   HAELTNISSES ERRECHNET UND DARAUS UEBER DIE STEIGUNG    *)
(*   DER MODAL-SPLIT-FUNKTION (CONST MSKONSTANTE) DIE       *)
(*   AENDERUNG DES OEV-PROZENTSATZES. DAMIT KOENNEN         *)
(*   NUN DIE ANTEILE OEV-NEU UND IV-NEU ERRECHNET WERDEN.   *)
(*                                                          *)
(*   AUS DEN ALTEN (EINGEGEBENEN) OEV- UND IV-WERTEN        *)
(*   UND DEN NEUEN LASSEN SICH DIE WERTE OEV-VERLAGERT,     *)
(*   OEV-VERBLEIBEND UND OEV-INDUZIERT BERECHNEN            *)
(*   (SIEHE DOKUMENTATION). DIESE WERTE WERDEN AUF          *)
(*   QUELLBEZIRKEBENE UND INSGESAMT SUMMIERT UND            *)
(*   IN SUMMENZEILEN AUSGEGEBEN. EBENFALLS SUMMIERT         *)
(*   WERDEN DIE GEFAHRENEN PKW-KILOMETER, GETRENNT NACH     *)
(*   INNERORTS UND AUSSERORTS, SO DASS DER PKW-SALDO        *)
(*   BERECHNET WERDEN KANN. DIE ZEITGEWINNE IM              *)
(*   VERBLEIBENDEN VERKEHR OEV WERDEN AUSSERDEM IN          *)
(*   EINER TABELLE (NACH REISEZEITKLASSEN) GESPEICHERT      *)
(*   UND AM SCHLUSS AUF EINEM SEPARATEN BLATT AUSGEGEBEN.   *)
(*                                                          *)
(*   FOLGENDE INFORMATIONEN WERDEN AUF DEM DRUCKER          *)
(*   JE SATZ DER EINGABEDATEI AUSGEGEBEN:                   *)
(*                                                          *)
(*   - QUELL- UND ZIELBEZIRK,                               *)
(*   - OEV-ALT-IST UND IV-ALT-IST (AUS DEN EINGABEDATEN),   *)
(*   - DIE ERMITTELTEN REISEZEITEN-ALT,                     *)
(*   - DAS ALTE REISEZEITVERHAELTNIS,                       *)
(*   - DIE ERMITTELTEN REISEZEITEN-NEU,                     *)
(*   - DAS NEUE REISEZEITVERHAELTNIS,                       *)
(*   - DIE AENDERUNG DES OEV-PROZENTSATZES,                 *)
(*   - OEV-NEU-IST UND IV-NEU-IST,                          *)
(*   - DER VERKEHR OEV-VERLAGERT,                           *)
(*   - DER VERKEHR OEV-VERBLEIBEND,                         *)
(*   - DER VERKEHR OEV-INDUZIERT.                           *)
(*                                                          *)
(************************************************************)


   var QUELLE , ZIEL , GV : INTEGER ;
       IV , OEV : INTEGER ;
       OEVGEFALT , IVGEFALT : BOOLEAN ;
       OEVZEITALT , IVZEITALT : REAL ;
       TEST : INTEGER ;
       KMAOALT , KMIOALT , KMGESAMTALT : REAL ;
       KMAO , KMIO : REAL ;
       SALDOPKWAO , SALDOPKWIO : REAL ;
       TABSTEUFELD : TABSTEUFELDTYP ;
       TABFELD : TABFELDTYP ;
       SOEVALT , SIVALT , SOEVIST , SIVIST : INTEGER ;
       SOEVVERL , SOEVVERBL , SOEVIND : INTEGER ;
       SGESOEVALT , SGESIVALT , SGESOEVIST , SGESIVIST : INTEGER ;
       SGESOEVVERL , SGESOEVVERBL , SGESOEVIND : INTEGER ;
       QALT : INTEGER ;
       QALTVORHANDEN : BOOLEAN ;


   procedure ERLAEUTERUNG ;

   (**************************************************)
   (*   GIBT AM UNTEREN BLATTRAND DES ERGEBNIS-      *)
   (*   LISTINGS EINE ERLAEUTERUNG DER VERWENDETEN   *)
   (*   ABKUERZUNGEN AUS.                            *)
   (**************************************************)


      begin (* ERLAEUTERUNG *)
        while ZEILZAHL < 64 do
          begin
            WRITELN ( AUSGABE ) ;
            ZEILZAHL := ZEILZAHL + 1
          end (* while *) ;
        WRITE ( AUSGABE , ' ' ) ;
        WRITE ( AUSGABE , 'ERLAEUTERUNGEN: ' ) ;
        WRITE ( AUSGABE , 'OEV=OEFFENTL. VERKEHR, ' ) ;
        WRITE ( AUSGABE , 'IV=INDIVIDUALVERK., ' ) ;
        WRITE ( AUSGABE , 'RZV=REISEZEITVERH., ' ) ;
        WRITE ( AUSGABE , 'VERL=VERLAGERT, ' ) ;
        WRITE ( AUSGABE , 'VERBL=VERBLEIBEND, IND=INDUZIERT.' ) ;
        WRITELN ( AUSGABE )
      end (* ERLAEUTERUNG *) ;


   procedure TABFELDINIT ;

   (*************************************************)
   (*                                               *)
   (*   HIER WIRD DAS TABELLENFELD MIT NULLWERTEN   *)
   (*   VORBESETZT. AUSSERDEM WIRD DAS TABELLEN-    *)
   (*   STEUERFELD BESETZT, DAS ANGIBT, WELCHE      *)
   (*   GRENZEN FUER DIE EINZELNEN TABELLENZEILEN   *)
   (*   GELTEN. DABEI WIRD JE TABELLENZEILE DIE     *)
   (*   UNTERGRENZE ABGELEGT UND EIN BOOLEAN-       *)
   (*   MERKMAL, DAS ANGIBT, OB DIE UNTERGRENZE     *)
   (*   ZU DEM ENTSPRECHENDEN INTERVALL DAZU-       *)
   (*   GEHOERT ODER NICHT. DIE ANGABEN ZU DER      *)
   (*   OBERGRENZE ERGEBEN SICH AUS DER UNTER-      *)
   (*   GRENZE DER NAECHSTEN TABELLENZEILE.         *)
   (*                                               *)
   (*   ES IST BEI EVTL. AENDERUNGEN DARAUF ZU      *)
   (*   ACHTEN, DASS DIE UNTERGRENZEN AUFSTEIGEND   *)
   (*   ANGEGEBEN WERDEN MUESSEN.                   *)
   (*                                               *)
   (*************************************************)


      var I : INTEGER ;

      begin (* TABFELDINIT *)
        for I := 1 to TABZ do
          begin
            TABFELD [ I ] . FAHRTENANZAHL := 0 ;
            TABFELD [ I ] . ZEITGEWINN := 0.0 ;
          end (* for *) ;
        TABSTEUFELD [ 1 ] . UNTERGRENZE := - 999999.9 ;
        TABSTEUFELD [ 1 ] . UGDAZU := TRUE ;
        TABSTEUFELD [ 2 ] . UNTERGRENZE := - 9.0 ;
        TABSTEUFELD [ 2 ] . UGDAZU := TRUE ;
        TABSTEUFELD [ 3 ] . UNTERGRENZE := - 7.0 ;
        TABSTEUFELD [ 3 ] . UGDAZU := TRUE ;
        TABSTEUFELD [ 4 ] . UNTERGRENZE := - 5.0 ;
        TABSTEUFELD [ 4 ] . UGDAZU := TRUE ;
        TABSTEUFELD [ 5 ] . UNTERGRENZE := - 3.0 ;
        TABSTEUFELD [ 5 ] . UGDAZU := TRUE ;
        TABSTEUFELD [ 6 ] . UNTERGRENZE := - 1.0 ;
        TABSTEUFELD [ 6 ] . UGDAZU := TRUE ;
        TABSTEUFELD [ 7 ] . UNTERGRENZE := 0.0 ;
        TABSTEUFELD [ 7 ] . UGDAZU := TRUE ;
        TABSTEUFELD [ 8 ] . UNTERGRENZE := 0.0 ;
        TABSTEUFELD [ 8 ] . UGDAZU := FALSE ;
        TABSTEUFELD [ 9 ] . UNTERGRENZE := 1.0 ;
        TABSTEUFELD [ 9 ] . UGDAZU := TRUE ;
        TABSTEUFELD [ 10 ] . UNTERGRENZE := 3.0 ;
        TABSTEUFELD [ 10 ] . UGDAZU := TRUE ;
        TABSTEUFELD [ 11 ] . UNTERGRENZE := 5.0 ;
        TABSTEUFELD [ 11 ] . UGDAZU := TRUE ;
        TABSTEUFELD [ 12 ] . UNTERGRENZE := 7.0 ;
        TABSTEUFELD [ 12 ] . UGDAZU := TRUE ;
        TABSTEUFELD [ 13 ] . UNTERGRENZE := 9.0 ;
        TABSTEUFELD [ 13 ] . UGDAZU := TRUE ;
        TABSTEUFELD [ 14 ] . UNTERGRENZE := 11.0 ;
        TABSTEUFELD [ 14 ] . UGDAZU := TRUE ;
        TABSTEUFELD [ 15 ] . UNTERGRENZE := 13.0 ;
        TABSTEUFELD [ 15 ] . UGDAZU := TRUE ;
        TABSTEUFELD [ 16 ] . UNTERGRENZE := 15.0 ;
        TABSTEUFELD [ 16 ] . UGDAZU := TRUE ;
        TABSTEUFELD [ 17 ] . UNTERGRENZE := 17.0 ;
        TABSTEUFELD [ 17 ] . UGDAZU := TRUE ;
        TABSTEUFELD [ 18 ] . UNTERGRENZE := 19.0 ;
        TABSTEUFELD [ 18 ] . UGDAZU := TRUE ;
      end (* TABFELDINIT *) ;


   function TABINDEX ( ZG : REAL ) : INTEGER ;

   (*******************************************)
   (*                                         *)
   (*   DIE FUNCTION TABINDEX LIEFERT BEI     *)
   (*   EINGABE DES REISEZEITGEWINNS (-ZG-)   *)
   (*   DEN INDEX DER ENTSPRECHENDEN          *)
   (*   TABELLENZEILE.                        *)
   (*                                         *)
   (*******************************************)


      var TI : INTEGER ;
          TERG : INTEGER ;

      begin (* TABINDEX *)
        TERG := 1 ;
        for TI := 1 to TABZ do
          begin
            if TABSTEUFELD [ TI ] . UGDAZU then
              begin
                if ZG >= TABSTEUFELD [ TI ] . UNTERGRENZE then
                  TERG := TI
              end (* then *)
            else
              begin
                if ZG > TABSTEUFELD [ TI ] . UNTERGRENZE then
                  TERG := TI
              end (* else *)
          end (* for *) ;
        TABINDEX := TERG
      end (* TABINDEX *) ;


   procedure KOPFZEILEN ;

   (*******************************************)
   (*                                         *)
   (*   MIT DIESER PROZEDUR WERDEN DIE        *)
   (*   KOPFZEILEN AUSGEGEBEN. SIE ENTHALTEN  *)
   (*   AUSSER ALLGEMEINEN INFORMATIONEN DIE  *)
   (*   SEITENZAHL UND DIE MODELLNAMEN.       *)
   (*                                         *)
   (*******************************************)


      begin (* KOPFZEILEN *)
        WRITE ( AUSGABE , '1' ) ;
        WRITE ( AUSGABE , 'M O D A L - S P L I T - A U S W E R T U N G'
                ) ;
        WRITE ( AUSGABE , ' ' : 26 ) ;
        SEITENZAHL := SEITENZAHL + 1 ;
        WRITE ( AUSGABE , ' ' : 48 , 'SEITE' , SEITENZAHL : 4 ) ;
        WRITELN ( AUSGABE ) ;
        WRITELN ( AUSGABE ) ;
        WRITE ( AUSGABE , ' ' ) ;
        WRITE ( AUSGABE ,
                'ERMITTLUNG DER MODAL-SPLIT-AENDERUNG ANHAND EINES' ) ;
        WRITE ( AUSGABE , ' NEUEN NETZMODELLS' ) ;
        WRITELN ( AUSGABE ) ;
        WRITELN ( AUSGABE ) ;
        WRITE ( AUSGABE , ' ' ) ;
        WRITE ( AUSGABE , 'OEV-MODELL ALT: ' , OEVALTNAME , ' ' : 6 ) ;
        WRITE ( AUSGABE , 'IV-MODELL ALT:  ' , IVALTNAME , ' ' : 12 ) ;
        WRITE ( AUSGABE , 'TAGESZEIT: ' , SITTEXTE [ SITAKTUELL ] ) ;
        WRITELN ( AUSGABE ) ;
        WRITE ( AUSGABE , ' ' ) ;
        WRITE ( AUSGABE , 'OEV-MODELL NEU: ' , OEVNEUNAME , ' ' : 6 ) ;
        WRITE ( AUSGABE , 'IV-MODELL NEU:  ' , IVNEUNAME ) ;
        WRITELN ( AUSGABE ) ;
        WRITELN ( AUSGABE ) ;
        WRITELN ( AUSGABE ) ;
      end (* KOPFZEILEN *) ;


   procedure TABAUSGABE ;

   (***************************************************)
   (*                                                 *)
   (*   IN DIESER PROZEDUR ERFOLGEN DIE AUSGABEN      *)
   (*   DER LETZTEN DRUCKERSEITE, ALSO DIE            *)
   (*   TABELLE DER REISEZEITGEWINNE, DIE SUMMEN-     *)
   (*   WERTE FUER DIE VERSCHIEDENEN VERKEHRSARTEN    *)
   (*   SOWIE DIE SALDO-PKW-KILOMETER INNERORTS       *)
   (*   UND AUSSERORTS.                               *)
   (*                                                 *)
   (***************************************************)


      var I : INTEGER ;
          DURCHSCHNITT : REAL ;


      procedure GRENZAUSG ( I : INTEGER ) ;

      (*******************************************)
      (*                                         *)
      (*   DIESE PROZEDUR DIENT DAZU, BEI DER    *)
      (*   AUSGABE DER TABELLE DER REISEZEIT-    *)
      (*   GEWINNE DIE INTERVALLGRENZEN DER      *)
      (*   ENTSPRECHENDEN TABELLENZEILE AUSZU-   *)
      (*   GEBEN (-I- IST INDEX DER TAB-ZEILE)   *)
      (*                                         *)
      (*******************************************)


         begin (* GRENZAUSG *)
           if I <> 1 then
             WRITEREAL ( AUSGABE , TABSTEUFELD [ I ] . UNTERGRENZE , 4
                         , 1 )
           else
             WRITE ( AUSGABE , ' ' : 4 ) ;
           if I <> 1 then
             begin
               if TABSTEUFELD [ I ] . UGDAZU then
                 WRITE ( AUSGABE , ' <= ' )
               else
                 WRITE ( AUSGABE , ' <  ' )
             end (* then *)
           else
             WRITE ( AUSGABE , ' ' : 4 ) ;
           WRITE ( AUSGABE , 'TDIFF' ) ;
           if I <> TABZ then
             begin
               if TABSTEUFELD [ I + 1 ] . UGDAZU then
                 WRITE ( AUSGABE , ' <  ' )
               else
                 WRITE ( AUSGABE , ' <= ' )
             end (* then *)
           else
             WRITE ( AUSGABE , ' ' : 4 ) ;
           if I <> TABZ then
             WRITEREAL ( AUSGABE , TABSTEUFELD [ I + 1 ] . UNTERGRENZE
                         , 4 , 1 )
           else
             WRITE ( AUSGABE , ' ' : 4 ) ;
         end (* GRENZAUSG *) ;


      begin (* TABAUSGABE *)
        KOPFZEILEN ;
        WRITE ( AUSGABE , ' ' : 31 ) ;
        WRITE ( AUSGABE , 'REISEZEITGEWINNE IM VERBLEIBENDEN' ) ;
        WRITE ( AUSGABE , ' VERKEHR OEV' ) ;
        WRITELN ( AUSGABE ) ;
        WRITE ( AUSGABE , ' ' : 38 ) ;
        WRITE ( AUSGABE , ' - NACH REISEZEITKLASSEN - ' ) ;
        WRITELN ( AUSGABE ) ;
        WRITELN ( AUSGABE ) ;
        WRITELN ( AUSGABE ) ;
        WRITE ( AUSGABE , ' ' : 31 ) ;
        WRITE ( AUSGABE , 'REISEZEITINTERVALL      ' ) ;
        WRITE ( AUSGABE , 'ANZAHL       ' ) ;
        WRITE ( AUSGABE , 'REISEZEITGEWINN      ' ) ;
        WRITE ( AUSGABE , 'REISEZEITGEWINN' ) ;
        WRITELN ( AUSGABE ) ;
        WRITE ( AUSGABE , ' ' : 31 ) ;
        WRITE ( AUSGABE , '                        ' ) ;
        WRITE ( AUSGABE , 'FAHRTEN      ' ) ;
        WRITE ( AUSGABE , '         GESAMT      ' ) ;
        WRITE ( AUSGABE , '   DURCHSCHNITT' ) ;
        WRITELN ( AUSGABE ) ;
        WRITELN ( AUSGABE ) ;
        for I := 1 to TABZ do
          begin
            WRITE ( AUSGABE , ' ' : 28 ) ;
            GRENZAUSG ( I ) ;
            WRITE ( AUSGABE , TABFELD [ I ] . FAHRTENANZAHL : 13 ) ;
            WRITEREAL ( AUSGABE , TABFELD [ I ] . ZEITGEWINN , 21 , 2 )
                        ;
            if TABFELD [ I ] . FAHRTENANZAHL = 0 then
              DURCHSCHNITT := 0.0
            else
              DURCHSCHNITT := TABFELD [ I ] . ZEITGEWINN / TABFELD [ I
                              ] . FAHRTENANZAHL ;
            WRITEREAL ( AUSGABE , DURCHSCHNITT , 21 , 2 ) ;
            WRITELN ( AUSGABE ) ;
            WRITELN ( AUSGABE )
          end (* for *) ;
        WRITELN ( AUSGABE ) ;
        WRITELN ( AUSGABE ) ;
        WRITE ( AUSGABE , ' ' : 31 ) ;
        WRITE ( AUSGABE , 'SUMME OEV-ALT GESAMT           :  ' ) ;
        WRITE ( AUSGABE , SGESOEVALT : 16 ) ;
        WRITELN ( AUSGABE ) ;
        WRITE ( AUSGABE , ' ' : 31 ) ;
        WRITE ( AUSGABE , 'SUMME IV-ALT GESAMT            :  ' ) ;
        WRITE ( AUSGABE , SGESIVALT : 16 ) ;
        WRITELN ( AUSGABE ) ;
        WRITE ( AUSGABE , ' ' : 31 ) ;
        WRITE ( AUSGABE , 'SUMME OEV-NEU GESAMT           :  ' ) ;
        WRITE ( AUSGABE , SGESOEVIST : 16 ) ;
        WRITELN ( AUSGABE ) ;
        WRITE ( AUSGABE , ' ' : 31 ) ;
        WRITE ( AUSGABE , 'SUMME IV-NEU GESAMT            :  ' ) ;
        WRITE ( AUSGABE , SGESIVIST : 16 ) ;
        WRITELN ( AUSGABE ) ;
        WRITE ( AUSGABE , ' ' : 31 ) ;
        WRITE ( AUSGABE , 'SUMME OEV-VERLAGERT GESAMT     :  ' ) ;
        WRITE ( AUSGABE , SGESOEVVERL : 16 ) ;
        WRITELN ( AUSGABE ) ;
        WRITE ( AUSGABE , ' ' : 31 ) ;
        WRITE ( AUSGABE , 'SUMME OEV-VERBLEIBEND GESAMT   :  ' ) ;
        WRITE ( AUSGABE , SGESOEVVERBL : 16 ) ;
        WRITELN ( AUSGABE ) ;
        WRITE ( AUSGABE , ' ' : 31 ) ;
        WRITE ( AUSGABE , 'SUMME OEV-INDUZIERT GESAMT     :  ' ) ;
        WRITE ( AUSGABE , SGESOEVIND : 16 ) ;
        WRITELN ( AUSGABE ) ;
        WRITELN ( AUSGABE ) ;
        WRITE ( AUSGABE , ' ' : 31 ) ;
        WRITE ( AUSGABE , 'SALDO-PERSONEN-KM INNERORTS    :  ' ) ;
        WRITEREAL ( AUSGABE , SALDOPKWIO , 16 , 3 ) ;
        WRITELN ( AUSGABE ) ;
        WRITE ( AUSGABE , ' ' : 31 ) ;
        WRITE ( AUSGABE , 'SALDO-PERSONEN-KM AUSSERORTS   :  ' ) ;
        WRITEREAL ( AUSGABE , SALDOPKWAO , 16 , 3 ) ;
        WRITELN ( AUSGABE ) ;
        WRITE ( AUSGABE , ' ' : 31 ) ;
        WRITE ( AUSGABE , 'SALDO-PKW-KILOMETER INNERORTS  :  ' ) ;
        WRITEREAL ( AUSGABE , SALDOPKWIO / PKWBESETZ , 16 , 3 ) ;
        WRITELN ( AUSGABE ) ;
        WRITE ( AUSGABE , ' ' : 31 ) ;
        WRITE ( AUSGABE , 'SALDO-PKW-KILOMETER AUSSERORTS :  ' ) ;
        WRITEREAL ( AUSGABE , SALDOPKWAO / PKWBESETZ , 16 , 3 ) ;
        WRITELN ( AUSGABE ) ;
        WRITELN ( AUSGABE , '1' ) ;
      end (* TABAUSGABE *) ;


   procedure UEBERSCHRIFT ;

   (************************************************)
   (*   GIBT DIE UEBERSCHRIFT FUER DEN DRUCKFILE   *)
   (*   AUS (IM BEREICH DER EINZELZEILEN)          *)
   (************************************************)


      begin (* UEBERSCHRIFT *)
        if SEITENZAHL <> 0 then
          ERLAEUTERUNG ;
        KOPFZEILEN ;
        WRITE ( AUSGABE , ' ' ) ;
        WRITE ( AUSGABE , ' QUELL-  ZIEL-   OEV-    IV-' ) ;
        WRITE ( AUSGABE , '  OEVZEIT   IVZEIT' ) ;
        WRITE ( AUSGABE , '   RZV-ALT' ) ;
        WRITE ( AUSGABE , '  OEVZEIT   IVZEIT' ) ;
        WRITE ( AUSGABE , '   RZV-NEU   DELTA   OEV-    IV-' ) ;
        WRITE ( AUSGABE , '   OEV-   OEV-   OEV-' ) ;
        WRITELN ( AUSGABE ) ;
        WRITE ( AUSGABE , ' ' ) ;
        WRITE ( AUSGABE , '   BEZ.   BEZ.    ALT    ALT' ) ;
        WRITE ( AUSGABE , '      ALT      ALT' ) ;
        WRITE ( AUSGABE , '          ' ) ;
        WRITE ( AUSGABE , '      NEU      NEU' ) ;
        WRITE ( AUSGABE , '             -PROZ    NEU    NEU' ) ;
        WRITE ( AUSGABE , '   VERL  VERBL    IND' ) ;
        WRITELN ( AUSGABE ) ;
        WRITELN ( AUSGABE ) ;
        WRITELN ( AUSGABE ) ;
        ZEILZAHL := 12
      end (* UEBERSCHRIFT *) ;


   procedure DRUCKEZEILE ;

      begin (* DRUCKEZEILE *)
        WRITELN ( AUSGABE ) ;
        ZEILZAHL := ZEILZAHL + 1 ;
        if ZEILZAHL > 59 then
          UEBERSCHRIFT ;
      end (* DRUCKEZEILE *) ;


   procedure SUMMENZEILE ;

   (**********************************)
   (*   HIER WIRD EINE SUMMENZEILE   *)
   (*   JE QUELLBEZIRK AUSGEGEBEN    *)
   (**********************************)


      begin (* SUMMENZEILE *)
        WRITELN ( AUSGABE ) ;
        ZEILZAHL := ZEILZAHL + 1 ;
        WRITE ( AUSGABE , ' ' ) ;
        WRITE ( AUSGABE , 'SUMME' , QALT : 7 , ': ' ) ;
        WRITE ( AUSGABE , SOEVALT : 7 ) ;
        WRITE ( AUSGABE , SIVALT : 7 ) ;
        WRITE ( AUSGABE , ' ' : 64 ) ;
        WRITE ( AUSGABE , SOEVIST : 7 ) ;
        WRITE ( AUSGABE , SIVIST : 7 ) ;
        WRITE ( AUSGABE , SOEVVERL : 7 ) ;
        WRITE ( AUSGABE , SOEVVERBL : 7 ) ;
        WRITE ( AUSGABE , SOEVIND : 7 ) ;
        WRITELN ( AUSGABE ) ;
        ZEILZAHL := ZEILZAHL + 1 ;
        WRITELN ( AUSGABE ) ;
        ZEILZAHL := ZEILZAHL + 1 ;
        if ZEILZAHL > 58 then
          UEBERSCHRIFT ;
      end (* SUMMENZEILE *) ;


   procedure VERARBEITEN ;

   (**************************************)
   (*   LEISTET DIE EIGENTLICHE ARBEIT   *)
   (**************************************)


      var OEVGEFUNDEN , IVGEFUNDEN : BOOLEAN ;
          FEHLER : INTEGER ;
          QBEZ , ZBEZ : BTYP ;
          OEVZEIT , IVZEIT : REAL ;
          RZV , RZVALT : REAL ;
          OEVPROZ , OEVPROZALT : REAL ;
          OEVANTEILIST , IVANTEILIST : INTEGER ;
          STATUS : STAT ;
          KMAONEU , KMIONEU , KMGESAMTNEU : REAL ;
          REDUKTION , ZEITDIFFERENZ : REAL ;
          OEVVERLAGERT , OEVVERBLEIBEND , OEVINDUZIERT : INTEGER ;
          ZGEWINN : REAL ;
          TABI : INTEGER ;
          ZGI : INTEGER ;
          DELTAOEVPROZ : REAL ;


      function STOERUNG ( NS , AI , AS : REAL ) : REAL ;

      (***********************************************)
      (*                                             *)
      (*   DIE FUNCTION STOERUNG UEBERTRAEGT DIE     *)
      (*   RELATION ALT-IST ZU ALT-SOLL AUF DEN      *)
      (*   WERT NEU-SOLL MITHILFE EINER VERHAELTNIS- *)
      (*   RECHNUNG. DIE TERMINI ALT-IST, ALT-SOLL   *)
      (*   USW. BEZIEHEN SICH HIERBEI AUF DIE OEV-   *)
      (*   ANTEILE.                                  *)
      (*                                             *)
      (*   WENN ALT-IST KLEINER IST ALS ALT-SOLL,    *)
      (*   SO WIRD NEU-SOLL ENTSPRECHEND VER-        *)
      (*   KLEINERT.                                 *)
      (*                                             *)
      (*   ANDERNFALLS DARF DER OEV-ANTEIL NICHT     *)
      (*   EINFACH VERGROESSERT WERDEN, DA SONST     *)
      (*   ANTEILE UEBER 100 PROZENT AUFTRETEN       *)
      (*   KOENNTEN. IN DIESEN FAELLEN WIRD DAHER    *)
      (*   MIT DEN IV-ANTEILEN GERECHNET. DER        *)
      (*   WERT IV-NEU-SOLL WIRD ENTSPRECHEND        *)
      (*   DEM VERHAELTNIS DER ALTEN WERTE VER-      *)
      (*   KLEINERT.                                 *)
      (*                                             *)
      (*   (NACHTRAG JUNI 1985:                      *)
      (*   WIRD BEIM NEUEN BERECHNUNGSMODUS NICHT    *)
      (*   MEHR BENOETIGT)                           *)
      (*                                             *)
      (***********************************************)


         var A , B : REAL ;

         begin (* STOERUNG *)
           if ( AS = 0 ) or ( AS = 1 ) then
             STOERUNG := 0
           else
             begin
               A := NS * AI / AS ;
               B := 1 - ( 1 - NS ) * ( 1 - AI ) / ( 1 - AS ) ;
               if AI <= AS then
                 STOERUNG := A
               else
                 STOERUNG := B
             end (* else *)
         end (* STOERUNG *) ;


      begin (* VERARBEITEN *)
        if SEITENZAHL = 0 then
          UEBERSCHRIFT ;
        OEVGEFUNDEN := TRUE ;
        IVGEFUNDEN := TRUE ;
        FEHLER := 0 ;
        if QUELLE = ZIEL then
          FEHLER := 1
        else
          begin
            SEARCHB ( QUELLE , QBEZ , STATUS ) ;
            if STATUS <> 0 then
              FEHLER := 2
            else
              begin
                SEARCHB ( ZIEL , ZBEZ , STATUS ) ;
                if STATUS <> 0 then
                  FEHLER := 3
                else
                  begin
                    if EINBIND [ QBEZ ] = NIL then
                      OEVGEFUNDEN := FALSE
                    else
                      begin
                        if EINBIND [ ZBEZ ] = NIL then
                          OEVGEFUNDEN := FALSE
                        else
                          begin
                            OEVROUTSUCH ( QBEZ , ZBEZ , OEVZEIT ,
                                          OEVGEFUNDEN ) ;
                          end (* else *)
                      end (* else *) ;
                    if ANSCHLUESSE [ QBEZ ] = NIL then
                      IVGEFUNDEN := FALSE
                    else
                      begin
                        if ANSCHLUESSE [ ZBEZ ] = NIL then
                          IVGEFUNDEN := FALSE
                        else
                          begin
                            IVROUTSUCH ( QBEZ , ZBEZ , IVZEIT , KMAONEU
                                         , KMIONEU , KMGESAMTNEU ,
                                         IVGEFUNDEN ) ;
                          end (* else *)
                      end (* else *)
                  end (* else *)
              end (* else *)
          end (* else *) ;
        if FEHLER <> 0 then
          begin
            OEVGEFUNDEN := FALSE ;
            IVGEFUNDEN := FALSE
          end (* then *) ;
        WRITE ( AUSGABE , ' ' ) ;
        WRITE ( AUSGABE , QUELLE : 7 ) ;
        WRITE ( AUSGABE , ZIEL : 7 ) ;
        WRITE ( AUSGABE , OEV : 7 ) ;
        WRITE ( AUSGABE , IV : 7 ) ;
        OEVPROZALT := OEV / ( OEV + IV ) * 100 ;
        if OEVGEFALT then
          WRITEREAL ( AUSGABE , OEVZEITALT , 9 , 2 )
        else
          WRITE ( AUSGABE , '     -NF-' ) ;
        if IVGEFALT then
          WRITEREAL ( AUSGABE , IVZEITALT , 9 , 2 )
        else
          WRITE ( AUSGABE , '     -NF-' ) ;
        if IVGEFALT and OEVGEFALT then
          begin
            RZVALT := OEVZEITALT / IVZEITALT ;
            WRITEREAL ( AUSGABE , RZVALT , 10 , 4 )
          end (* then *)
        else
          WRITE ( AUSGABE , ' ' : 10 ) ;
        if OEVGEFUNDEN then
          WRITEREAL ( AUSGABE , OEVZEIT , 9 , 2 )
        else
          WRITE ( AUSGABE , '     -NF-' ) ;
        if IVGEFUNDEN then
          WRITEREAL ( AUSGABE , IVZEIT , 9 , 2 )
        else
          WRITE ( AUSGABE , '     -NF-' ) ;
        if IVGEFUNDEN and OEVGEFUNDEN then
          begin
            RZV := OEVZEIT / IVZEIT ;
            WRITEREAL ( AUSGABE , RZV , 10 , 4 )
          end (* then *)
        else
          WRITE ( AUSGABE , ' ' : 10 ) ;

        (*************************************)
        (*   AB HIER FOLGT DIE EIGENTLICHE   *)
        (*   BERECHNUNG DER NEUEN ANTEILE    *)
        (*   SOWIE DER WERTE OEV-VERLAGERT,  *)
        (*   OEV-VERBLEIBEND UND OEV-        *)
        (*   INDUZIERT.                      *)
        (*************************************)

        if OEVGEFALT and IVGEFALT and OEVGEFUNDEN and IVGEFUNDEN then
          begin
            DELTAOEVPROZ := MSKONSTANTE * ( RZVALT - RZV ) * 100 ;
            WRITEREAL ( AUSGABE , DELTAOEVPROZ , 8 , 2 ) ;
            OEVPROZ := OEVPROZALT + DELTAOEVPROZ ;
            if ( OEVPROZALT <= MSOBEN ) and ( OEVPROZALT >= MSUNTEN )
            then
              begin
                if OEVPROZ > MSOBEN then
                  OEVPROZ := MSOBEN ;
                if OEVPROZ < MSUNTEN then
                  OEVPROZ := MSUNTEN ;
              end (* then *)
            else
              begin
                if OEVPROZ > 100.0 then
                  OEVPROZ := 100.0 ;
                if OEVPROZ < 0.0 then
                  OEVPROZ := 0.0
              end (* else *) ;
            OEVANTEILIST := ROUND ( GV * OEVPROZ / 100 ) ;
            IVANTEILIST := GV - OEVANTEILIST ;
            WRITE ( AUSGABE , OEVANTEILIST : 7 ) ;
            WRITE ( AUSGABE , IVANTEILIST : 7 ) ;
            OEVVERLAGERT := OEVANTEILIST - OEV ;
            if OEVVERLAGERT > 0 then
              OEVVERBLEIBEND := OEV
            else
              OEVVERBLEIBEND := OEVANTEILIST ;
            if OEVZEITALT > OEVZEIT then
              begin
                REDUKTION := 1.0 ;
                ZEITDIFFERENZ := OEVZEITALT - OEVZEIT ;
                if ZEITDIFFERENZ < 5.0 then
                  REDUKTION := ZEITDIFFERENZ / 5.0 ;
                OEVINDUZIERT := ROUND ( OEVVERBLEIBEND * ZEITDIFFERENZ
                                * REDUKTION / OEVZEIT ) ;
              end (* then *)
            else
              OEVINDUZIERT := 0 ;
            WRITE ( AUSGABE , OEVVERLAGERT : 7 ) ;
            WRITE ( AUSGABE , OEVVERBLEIBEND : 7 ) ;
            WRITE ( AUSGABE , OEVINDUZIERT : 7 ) ;

        (*************************************)
        (*   SUMMIERUNG AUF QUELLBEZIRKS-    *)
        (*   EBENE.                          *)
        (*************************************)

            SOEVALT := SOEVALT + OEV ;
            SIVALT := SIVALT + IV ;
            SOEVIST := SOEVIST + OEVANTEILIST ;
            SIVIST := SIVIST + IVANTEILIST ;
            SOEVVERL := SOEVVERL + OEVVERLAGERT ;
            SOEVVERBL := SOEVVERBL + OEVVERBLEIBEND ;
            SOEVIND := SOEVIND + OEVINDUZIERT ;

        (*************************************)
        (*   AUFSUMMIEREN DER GEFAHRENEN     *)
        (*   KILOMETER FUER PKW-SALDO.       *)
        (*************************************)

            KMIO := KMIONEU * IVANTEILIST - KMIOALT * IV ;
            KMAO := KMAONEU * IVANTEILIST - KMAOALT * IV ;
            if ABS ( KMIO ) < 0.0005 then
              KMIO := 0.0 ;
            if ABS ( KMAO ) < 0.0005 then
              KMAO := 0.0 ;
            SALDOPKWIO := SALDOPKWIO + KMIO ;
            SALDOPKWAO := SALDOPKWAO + KMAO ;

        (*************************************)
        (*   SUMMIEREN DER FAHRTENANZAHL     *)
        (*   UND DES ZEITGEWINNS IN DIE      *)
        (*   ENTSPRECHENDE TABELLENZEILE     *)
        (*   DER TABELLE DER REISEZEITGE-    *)
        (*   WINNE.                          *)
        (*************************************)

            ZGEWINN := OEVZEITALT - OEVZEIT ;
            ZGI := ROUND ( ZGEWINN * 1000 ) ;
            ZGEWINN := ZGI / 1000 ;
            TABI := TABINDEX ( ZGEWINN ) ;
            TABFELD [ TABI ] . FAHRTENANZAHL := TABFELD [ TABI ] .
                                                FAHRTENANZAHL +
                                                OEVVERBLEIBEND ;
            TABFELD [ TABI ] . ZEITGEWINN := TABFELD [ TABI ] .
                                             ZEITGEWINN + ZGEWINN *
                                             OEVVERBLEIBEND ;

        (*************************************)
        (*   AUSGABE FUER TESTZWECKE IN DIE  *)
        (*   DATEI -TESTDRUCK-               *)
        (*************************************)

            if TESTMODUS then
              begin
                WRITE ( TESTDRUCK , ' OEV-REISEZEITGEWINN:       ' ) ;
                WRITE ( TESTDRUCK , '  MIN:' ) ;
                WRITEREAL ( TESTDRUCK , ZGEWINN , 13 , 2 ) ;
                WRITE ( TESTDRUCK , '   ANZ:' , OEVVERBLEIBEND : 10 ) ;
                WRITELN ( TESTDRUCK ) ;
                WRITE ( TESTDRUCK , ' VERLAGERTE PKW-KILOMETER:  ' ) ;
                WRITE ( TESTDRUCK , '   IO:' ) ;
                WRITEREAL ( TESTDRUCK , KMIO , 14 , 3 ) ;
                WRITE ( TESTDRUCK , '   AO:' ) ;
                WRITEREAL ( TESTDRUCK , KMAO , 14 , 3 ) ;
                WRITELN ( TESTDRUCK ) ;
                WRITELN ( TESTDRUCK )
              end (* then *) ;

        (***********************************)
        (*   AUSGABE DER NEUEN IST-WERTE   *)
        (*   AUF DEN FILE NEUFILE ZUR      *)
        (*   WEITERVERARBEITUNG.           *)
        (***********************************)

            WRITE ( NEUFILE , QUELLE : 10 ) ;
            WRITE ( NEUFILE , ZIEL : 10 ) ;
            WRITE ( NEUFILE , OEVANTEILIST : 10 ) ;
            WRITE ( NEUFILE , IVANTEILIST : 10 ) ;
            WRITELN ( NEUFILE )
          end (* then *) ;
        DRUCKEZEILE ;
      end (* VERARBEITEN *) ;


   begin (* MODUS3 *)
     REWRITE ( NEUFILE ) ;

     (*************************************)
     (*   EINLESEN DER MODELLNAMEN,       *)
     (*   INITIALISIEREN DER TABELLE,     *)
     (*   NULLSETZEN DER SUMMENFELDER.    *)
     (*************************************)

     NEXTWORT ( OEVALTNAME ) ;
     NEXTWORT ( IVALTNAME ) ;
     NEXTWORT ( OEVNEUNAME ) ;
     NEXTWORT ( IVNEUNAME ) ;
     SALDOPKWIO := 0.0 ;
     SALDOPKWAO := 0.0 ;
     TABFELDINIT ;
     SGESOEVALT := 0 ;
     SGESIVALT := 0 ;
     SGESOEVIST := 0 ;
     SGESIVIST := 0 ;
     SGESOEVVERL := 0 ;
     SGESOEVVERBL := 0 ;
     SGESOEVIND := 0 ;
     SOEVALT := 0 ;
     SIVALT := 0 ;
     SOEVIST := 0 ;
     SIVIST := 0 ;
     SOEVVERL := 0 ;
     SOEVVERBL := 0 ;
     SOEVIND := 0 ;

     (*************************************)
     (*   DURCH DEN VERGLEICH MIT -QALT-  *)
     (*   WIRD GEPRUEFT, OB SICH DER      *)
     (*   QUELLBEZIRK VERAENDERT HAT      *)
     (*   (FUER SUMMENBILDUNG)            *)
     (*************************************)

     QALT := 0 ;
     QALTVORHANDEN := FALSE ;
     repeat
       READ ( EINGABE , TEST ) ;
       OEVGEFALT := ( TEST = 1 ) ;
       READ ( EINGABE , TEST ) ;
       IVGEFALT := ( TEST = 1 ) ;
       READ ( EINGABE , QUELLE ) ;
       if QUELLE <> 0 then
         begin
           if QALTVORHANDEN and ( QUELLE <> QALT ) then
             begin

     (*************************************)
     (*   WENN JA, WIRD EINE SUMMENZEILE  *)
     (*   AUSGEGEBEN, DIE SUMMENFELDER    *)
     (*   WERDEN ZU DEN ENDSUMMENWERTEN   *)
     (*   DAZUADDIERT UND WIEDER AUF NULL *)
     (*   GESTELLT.                       *)
     (*************************************)

               SUMMENZEILE ;
               SGESOEVALT := SGESOEVALT + SOEVALT ;
               SGESIVALT := SGESIVALT + SIVALT ;
               SGESOEVIST := SGESOEVIST + SOEVIST ;
               SGESIVIST := SGESIVIST + SIVIST ;
               SGESOEVVERL := SGESOEVVERL + SOEVVERL ;
               SGESOEVVERBL := SGESOEVVERBL + SOEVVERBL ;
               SGESOEVIND := SGESOEVIND + SOEVIND ;
               SOEVALT := 0 ;
               SIVALT := 0 ;
               SOEVIST := 0 ;
               SIVIST := 0 ;
               SOEVVERL := 0 ;
               SOEVVERBL := 0 ;
               SOEVIND := 0 ;
             end (* then *) ;
           QALT := QUELLE ;
           QALTVORHANDEN := TRUE ;

     (*************************************)
     (*   DIE WERTE AUS MODUS 2 WERDEN    *)
     (*   EINGELESEN. DANACH FOLGT DIE    *)
     (*   EIGENTLICHE VERARBEITUNG.       *)
     (*************************************)

           READ ( EINGABE , ZIEL ) ;
           if ZIEL <> 0 then
             begin
               READ ( EINGABE , OEV ) ;
               READ ( EINGABE , IV ) ;
               GV := IV + OEV ;
               if OEVGEFALT then
                 READ ( EINGABE , OEVZEITALT ) ;
               if IVGEFALT then
                 begin
                   READ ( EINGABE , IVZEITALT ) ;
                   READ ( EINGABE , KMAOALT ) ;
                   READ ( EINGABE , KMIOALT ) ;
                   READ ( EINGABE , KMGESAMTALT )
                 end (* then *) ;
               if GV <> 0 then
                 VERARBEITEN
             end (* then *)
         end (* then *) ;
       READLN ( EINGABE )
     until EOF ( EINGABE ) ;

     (*************************************)
     (*   AM SCHLUSS KOMMT NOCH EINE      *)
     (*   SUMMENZEILE FUER DEN LETZTEN    *)
     (*   QUELLBEZIRK. DANACH FOLGT DIE   *)
     (*   AUSGABE DES SUMMENBLATTS.       *)
     (*************************************)

     SUMMENZEILE ;
     SGESOEVALT := SGESOEVALT + SOEVALT ;
     SGESIVALT := SGESIVALT + SIVALT ;
     SGESOEVIST := SGESOEVIST + SOEVIST ;
     SGESIVIST := SGESIVIST + SIVIST ;
     SGESOEVVERL := SGESOEVVERL + SOEVVERL ;
     SGESOEVVERBL := SGESOEVVERBL + SOEVVERBL ;
     SGESOEVIND := SGESOEVIND + SOEVIND ;
     ERLAEUTERUNG ;
     TABAUSGABE
   end (* MODUS3 *) ;



begin (* HAUPTPROGRAMM *)
  SSTRING := ' ' ;
  MOVEPARM ( ADDR ( SSTRING ) , 80 ) ;
  TOUPPERS ( ADDR ( SSTRING ) , 80 ) ;

  (******************************************************)
  (*   ACHTUNG: FUNKTION PARMS IST MASCHINENABHAENGIG   *)
  (******************************************************)

  SX := 1 ;
  while SSTRING [ SX ] = ' ' do
    SX := SX + 1 ;
  MODWAHL := ORD ( SSTRING [ SX ] ) - ORD ( '0' ) ;
  SX := SX + 1 ;
  while SSTRING [ SX ] = ' ' do
    SX := SX + 1 ;
  TAGESZEIT := ORD ( SSTRING [ SX ] ) - ORD ( '0' ) ;
  SX := SX + 1 ;

  (**********************************************)
  (*  DER MODUS UND DIE TAGESZEIT WERDEN AUS    *)
  (*  DEM PARAMETERSTRING GEHOLT.               *)
  (**********************************************)

  TESTMODUS := TRUE ;
  TERMOUT ( OUTPUT ) ;
  TERMIN ( INPUT ) ;
  RESET ( EINGABE ) ;
  REWRITE ( AUSGABE ) ;
  ZEILZAHL := 0 ;
  SEITENZAHL := 0 ;

  (*****************************************)
  (*   DER REIHE NACH WERDEN EINGELESEN:   *)
  (*     - DIE MODAL-SPLIT-FUNKTION        *)
  (*     - DAS OEV-MODELL                  *)
  (*     - DAS IV-MODELL.                  *)
  (*****************************************)

  WRITELN ;
  if MODWAHL = 1 then
    begin
      MSFINIT ;
      WRITELN ( 'MSF-INIT FERTIG' )
    end (* then *) ;
  OEVVORBESETZEN ;
  OEVINIT ;
  WRITELN ( 'OEV-INIT FERTIG' ) ;
  OEVKONSISTENZ ( RICHTIG ) ;
  if not RICHTIG then
    begin
      WRITELN ( 'OEV-KONSISTENZPRUEFUNG FEHLERHAFT, ABBRUCH !!' ) ;
      HALT
    end (* then *) ;
  IVVORBESETZEN ;
  IVINIT ;
  WRITELN ( 'IV-INIT FERTIG' ) ;
  IVKONSISTENZ ( RICHTIG ) ;
  if not RICHTIG then
    begin
      WRITELN ( 'IV-KONSISTENZPRUEFUNG FEHLERHAFT, ABBRUCH !!' ) ;
      HALT
    end (* then *) ;
  WRITELN ;

  (***************************************)
  (*   VERSCHIEDENE VORAB-BERECHNUNGEN   *)
  (***************************************)

  OEVSUCHTABINIT ;
  case TAGESZEIT of
    1 : SITAKTUELL := HAUPT ;
    2 : SITAKTUELL := NORMAL ;
    3 : SITAKTUELL := SPAET
  end (* case *) ;
  OEVZEITRECHNUNG ;
  IVZEITRECHNUNG ;
  SLISTERSTELLEN ;

  (*************************************)
  (*   VERARBEITUNG ENTSPRECHEND DEM   *)
  (*   VORGEGEBENEN MODUS              *)
  (*************************************)

  case MODWAHL of
    1 : MODUS1 ;
    2 : MODUS2 ;
    3 : MODUS3 ;
  end (* case *) ;
  WRITELN
end (* HAUPTPROGRAMM *) .
