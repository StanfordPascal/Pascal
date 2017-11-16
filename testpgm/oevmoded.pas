program OEVMODED ( INPUT , OUTPUT , DRUCKER , OEVMODELL ) ;

(********************************************************************)
(*                                                                  *)
(*    PROGRAMM ZUM EDITIEREN EINES OEV-NETZES.                      *)
(*                                                                  *)
(*    AUTOR: HERBERT MEGERLE                                        *)
(*                                                                  *)
(*    ERGAENZT UND ERWEITERT 1984 IM RAHMEN EINER STUDIEN-          *)
(*    ARBEIT VON BERND OPPOLZER.                                    *)
(*                                                                  *)
(*    STAND VOM 5.10.84                                             *)
(*                                                                  *)
(********************************************************************)



const HMAX = 1000 ;
      LMAX = 200 ;
      BMAX = 2000 ;
      PZZ = 72 ;
      DIFF = 3 ;
      LEERKOMM = '                         ' ;

      (**********************************************)
      (*                                            *)
      (*   PROGRAMMKONSTANTEN:                      *)
      (*   -------------------                      *)
      (*                                            *)
      (*   HMAX:  MAX. ANZAHL HALTESTELLEN          *)
      (*   LMAX:   "     "    LINIEN                *)
      (*   BMAX:   "     "    BEZIRKE               *)
      (*   PZZ:   ZEILENZAHL DER DRUCKERS           *)
      (*   DIFF:  ANZAHL VERSCHIED. TAGESZEITEN     *)
      (*                                            *)
      (**********************************************)



type HTYP = 0 .. HMAX ;
     LTYP = 0 .. LMAX ;
     BTYP = 0 .. BMAX ;
     HTEXT = packed array [ 1 .. 25 ] of CHAR ;
     EPTYP = -> EINB ;
     EINB = record
              HSTELLE : INTEGER ;
              NEXT : EPTYP ;
              FW : INTEGER
            end ;

     (*************************************)
     (*   DARSTELLUNG VON EINBINDUNGEN:   *)
     (*                                   *)
     (*   LISTE ALLER HALTESTELLEN, DIE   *)
     (*   IN EINEN BEZIRK EINGEBUNDEN     *)
     (*   SIND (SOWIE FUSSWEG).           *)
     (*************************************)

     HPTYP = -> HALTSTELLE ;
     HALTSTELLE = record
                    HSTELLE : INTEGER ;
                    NEXT : HPTYP ;
                    SBEL : INTEGER
                  end ;

     (*************************************)
     (*   DARSTELLUNG VON STRECKEN:       *)
     (*                                   *)
     (*   LISTE ALLER HALTESTELLEN, DIE   *)
     (*   AUSGEHEND VON EINER GEGEBENEN   *)
     (*   HALTESTELLE DIREKT ZU           *)
     (*   ERREICHEN SIND.                 *)
     (*************************************)

     LEPTYP = -> LINIENELEMENT ;
     LINIENELEMENT = record
                       HSTELLE : INTEGER ;
                       STRECKE : LEPTYP ;
                       FZ : INTEGER ;
                       LBEL : INTEGER
                     end ;

     (*************************************)
     (*   DARSTELLUNG VON LINIEN:         *)
     (*                                   *)
     (*   RINGFOERMIGE LISTE ALLER AUF    *)
     (*   DER LINIE LIEGENDEN HALTE-      *)
     (*   STELLEN MIT FAHRZEITEN.         *)
     (*************************************)

     STAT = 1 .. 4 ;
     HPOSTYP = record
                 X : INTEGER ;
                 Y : INTEGER
               end ;


var ZEILZAHL : INTEGER ;
    SEITENZAHL : INTEGER ;
    MODNAME : packed array [ 1 .. 8 ] of CHAR ;
    RICHTIG : BOOLEAN ;
    UPDATE : BOOLEAN ;
    HZAHL : HTYP ;

    (**************************)
    (*  ANZAHL HALTESTELLEN   *)
    (**************************)

    LZAHL : LTYP ;

    (**************************)
    (*  ANZAHL LINIEN         *)
    (**************************)

    BZAHL : BTYP ;

    (**************************)
    (*  ANZAHL BEZIRKE        *)
    (**************************)

    DRUCKER : TEXT ;
    OEVMODELL : TEXT ;
    STRECKENNETZ : array [ HTYP ] of HPTYP ;
    HPOS : array [ HTYP ] of HPOSTYP ;
    HTEXTE : array [ HTYP ] of HTEXT ;
    HTRANSFER : array [ HTYP ] of INTEGER ;

    (**************************************************)
    (*  STRECKENNETZ,                                 *)
    (*  HALTESTELLENNUMMERN, -TEXTE UND -POSITIONEN   *)
    (**************************************************)

    LINIEN : array [ LTYP ] of LEPTYP ;
    BH : array [ LTYP , 1 .. DIFF ] of INTEGER ;
    LTRANSFER : array [ LTYP ] of INTEGER ;

    (******************************************************************)
    (*  LINIENNUMMERN, -BED.ZEITINTERVALLE UND ANKER DER RINGLISTEN   *)
    (******************************************************************)

    EINBIND : array [ BTYP ] of EPTYP ;
    BTRANSFER : array [ BTYP ] of INTEGER ;

    (**************************************)
    (*  BEZIRKSNUMMERN UND EINBINDUNGEN   *)
    (**************************************)




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
     WRITELN ( DRUCKER , '1A U S G A B E   D E R   O E V - M O ' ,
               'D E L L ' , 'P A R A M E T E R           MODELLNAME : '
               , MODNAME , ' ' : 15 , 'SEITE' , SEITENZAHL : 4 ) ;
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
     X [ 1 ] := TOUPPER ( ( C ) ) ;
     while not EOLN and ( COUNT < 25 ) do
       begin
         READ ( C ) ;
         COUNT := COUNT + 1 ;
         X [ COUNT ] := TOUPPER ( ( C ) ) ;
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



procedure INIT ;

(****************************************************************)
(*                                                              *)
(*   OEV-MODELL VON FILE EINLESEN                               *)
(*                                                              *)
(*   DIE MODELLPARAMETER SIND (IM UNTERSCHIED ZU DER UR-        *)
(*   SPRUENGLICHEN PROGRAMMVERSION) AUF EINEM EINZIGEN          *)
(*   TEXTFILE ABGELEGT. DIE ERSTEN 6 STELLEN JEDER ZEILE        *)
(*   ENTHALTEN EINE KENNUNG, DIE ANGIBT, UM WELCHE ART VON      *)
(*   PARAMETER ES SICH IN DEM BETREFFENDEN SATZ HANDELT.        *)
(*   DIE KENNUNGEN WURDEN ENTSPRECHEND DEN URSPRUENGLICHEN      *)
(*   DATEINAMEN FESTGELEGT.                                     *)
(*                                                              *)
(****************************************************************)


   var I , K : INTEGER ;
       KENN : packed array [ 1 .. 6 ] of CHAR ;
       CH : CHAR ;
       IX : INTEGER ;


   procedure READKENN ;

   (***************************************************)
   (*   LIEST KENNUNG IM OEV-FILE (ERSTE 6 STELLEN)   *)
   (***************************************************)


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


   begin (* INIT *)
     REWRITE ( DRUCKER ) ;
     SEITENZAHL := 0 ;
     ZEILZAHL := 0 ;
     for I := 1 to HMAX do
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
     for I := 1 to LMAX do
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
     for I := 1 to BMAX do
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
         WRITELN ( 'FALSCHER AUFBAU DER MODELLDATEI' ) ;
         WRITELN ( '**** ABBRUCH ****' ) ;
         WRITELN ;
         HALT
       end (* then *)
   end (* INIT *) ;



procedure UPD ;

(******************************************************)
(*   OEV-MODELL AUF FILES ABSPEICHERN - ANALOG INIT   *)
(******************************************************)


   var I , K : INTEGER ;


   procedure SAVEE ;

   (********************************)
   (*   EINBINDUNGEN ABSPEICHERN   *)
   (********************************)


      var AKT : EPTYP ;
          I : INTEGER ;
          EFILE : EINB ;
          EFILE0 : EINB ;

      begin (* SAVEE *)
        EFILE0 . HSTELLE := 0 ;
        EFILE0 . FW := 0 ;
        for I := 1 to BZAHL do
          begin
            AKT := EINBIND [ I ] ;
            while AKT <> NIL do
              begin
                EFILE := AKT -> ;
                WRITE ( OEVMODELL , 'EFILE ' ) ;
                WRITE ( OEVMODELL , EFILE . HSTELLE : 10 ) ;
                WRITE ( OEVMODELL , EFILE . FW : 10 ) ;
                WRITELN ( OEVMODELL ) ;
                AKT := AKT -> . NEXT
              end (* while *) ;
            WRITE ( OEVMODELL , 'EFILE ' ) ;
            WRITE ( OEVMODELL , EFILE0 . HSTELLE : 10 ) ;
            WRITE ( OEVMODELL , EFILE0 . FW : 10 ) ;
            WRITELN ( OEVMODELL )
          end (* for *)
      end (* SAVEE *) ;


   procedure SAVES ;

   (********************************)
   (*   STRECKENNETZ ABSPEICHERN   *)
   (********************************)


      var AKT : HPTYP ;
          I : INTEGER ;
          SFILE : HALTSTELLE ;
          SFILE0 : HALTSTELLE ;

      begin (* SAVES *)
        SFILE0 . HSTELLE := 0 ;
        SFILE0 . SBEL := 0 ;
        for I := 1 to HZAHL do
          begin
            AKT := STRECKENNETZ [ I ] ;
            while AKT <> NIL do
              begin
                SFILE := AKT -> ;
                SFILE . SBEL := 0 ;
                WRITE ( OEVMODELL , 'SFILE ' ) ;
                WRITE ( OEVMODELL , SFILE . HSTELLE : 10 ) ;
                WRITE ( OEVMODELL , SFILE . SBEL : 10 ) ;
                WRITELN ( OEVMODELL ) ;
                AKT := AKT -> . NEXT
              end (* while *) ;
            WRITE ( OEVMODELL , 'SFILE ' ) ;
            WRITE ( OEVMODELL , SFILE0 . HSTELLE : 10 ) ;
            WRITE ( OEVMODELL , SFILE0 . SBEL : 10 ) ;
            WRITELN ( OEVMODELL )
          end (* for *)
      end (* SAVES *) ;


   procedure SAVEL ;

   (**************************)
   (*   LINIEN ABSPEICHERN   *)
   (**************************)


      var AKT , AKT1 : LEPTYP ;
          I : INTEGER ;
          LFILE : LINIENELEMENT ;
          LFILE0 : LINIENELEMENT ;

      begin (* SAVEL *)
        LFILE0 . HSTELLE := 0 ;
        LFILE0 . FZ := 0 ;
        LFILE0 . LBEL := 0 ;
        for I := 1 to LZAHL do
          begin
            AKT := LINIEN [ I ] ;
            AKT1 := AKT ;
            while AKT <> NIL do
              begin
                LFILE := AKT -> ;
                LFILE . LBEL := 0 ;
                WRITE ( OEVMODELL , 'LFILE ' ) ;
                WRITE ( OEVMODELL , LFILE . HSTELLE : 10 ) ;
                WRITE ( OEVMODELL , LFILE . FZ : 10 ) ;
                WRITE ( OEVMODELL , LFILE . LBEL : 10 ) ;
                WRITELN ( OEVMODELL ) ;
                AKT := AKT -> . STRECKE ;
                if AKT = AKT1 then
                  AKT := NIL
              end (* while *) ;
            WRITE ( OEVMODELL , 'LFILE ' ) ;
            WRITE ( OEVMODELL , LFILE0 . HSTELLE : 10 ) ;
            WRITE ( OEVMODELL , LFILE0 . FZ : 10 ) ;
            WRITE ( OEVMODELL , LFILE0 . LBEL : 10 ) ;
            WRITELN ( OEVMODELL )
          end (* for *)
      end (* SAVEL *) ;


   begin (* UPD *)
     UPDATE := TRUE ;
     REWRITE ( OEVMODELL ) ;
     for I := 1 to HZAHL do
       begin
         WRITE ( OEVMODELL , 'HTFILE' ) ;
         WRITE ( OEVMODELL , HTRANSFER [ I ] : 10 ) ;
         WRITE ( OEVMODELL , HPOS [ I ] . X : 10 ) ;
         WRITE ( OEVMODELL , HPOS [ I ] . Y : 10 ) ;
         WRITE ( OEVMODELL , '     ' ) ;
         WRITE ( OEVMODELL , HTEXTE [ I ] ) ;
         WRITELN ( OEVMODELL )
       end (* for *) ;
     SAVES ;
     for I := 1 to LZAHL do
       begin
         WRITE ( OEVMODELL , 'LTFILE' ) ;
         WRITE ( OEVMODELL , LTRANSFER [ I ] : 10 ) ;
         WRITELN ( OEVMODELL )
       end (* for *) ;
     for I := 1 to LZAHL do
       begin
         for K := 1 to DIFF do
           begin
             WRITE ( OEVMODELL , 'BHFILE' ) ;
             WRITE ( OEVMODELL , BH [ I , K ] : 10 ) ;
             WRITELN ( OEVMODELL )
           end (* for *)
       end (* for *) ;
     SAVEL ;
     for I := 1 to BZAHL do
       begin
         WRITE ( OEVMODELL , 'BTFILE' ) ;
         WRITE ( OEVMODELL , BTRANSFER [ I ] : 10 ) ;
         WRITELN ( OEVMODELL )
       end (* for *) ;
     SAVEE ;
     WRITELN ( OEVMODELL , '*ENDE*' )
   end (* UPD *) ;



procedure LOEL ;

(***********************)
(*   LINIEN LOESCHEN   *)
(***********************)


   var I , J , K , LNR : INTEGER ;

   begin (* LOEL *)
     WRITELN ;
     repeat
       WRITELN ( 'GIB LINIENNUMMER:' ) ;
       IEIN ( LNR ) ;
       if LNR > 0 then
         begin
           I := 1 ;
           while ( LTRANSFER [ I ] <> LNR ) and ( I <= LZAHL ) do
             I := I + 1 ;
           if I > LZAHL then
             WRITELN ( 'LINIE WAR SCHON NICHT VORHANDEN !' )
           else
             if LTRANSFER [ I ] = LNR then
               begin
                 for J := I to ( LZAHL - 1 ) do
                   begin
                     for K := 1 to DIFF do
                       BH [ J , K ] := BH [ J + 1 , K ] ;
                     LINIEN [ J ] := LINIEN [ J + 1 ] ;
                     LTRANSFER [ J ] := LTRANSFER [ J + 1 ]
                   end (* for *) ;
                 LZAHL := LZAHL - 1 ;
                 WRITELN ( 'LINIE NR.' , LNR , ' GELOESCHT' )
               end (* then *)
         end (* then *) ;
       WRITELN
     until LNR = 0
   end (* LOEL *) ;



procedure LOEE ;

(*****************************)
(*   EINBINDUNGEN LOESCHEN   *)
(*****************************)


   var BEZ , HALT2 : INTEGER ;


   procedure LE ( BEZ , HALT2 : INTEGER ) ;

   (********************************)
   (*   EINE EINBINDUNG LOESCHEN   *)
   (********************************)


      var AKT , AKT1 : EPTYP ;
          I : INTEGER ;
          SWITCH : BOOLEAN ;

      begin (* LE *)
        I := 1 ;
        while ( BTRANSFER [ I ] < BEZ ) and ( I <= BZAHL ) do
          I := I + 1 ;
        if I > BZAHL then
          WRITELN ( 'EINBIND. WAR SCHON NICHT VORHANDEN' )
        else
          begin
            AKT := EINBIND [ I ] ;
            if AKT <> NIL then
              begin
                if AKT -> . HSTELLE = HALT2 then
                  begin
                    EINBIND [ I ] := AKT -> . NEXT ;
                    WRITELN ( 'EINBIND. GELOESCHT' )
                  end (* then *)
                else
                  begin
                    AKT1 := AKT ;
                    AKT := AKT -> . NEXT ;
                    SWITCH := FALSE ;
                    while ( AKT <> NIL ) and ( not SWITCH ) do
                      begin
                        if AKT -> . HSTELLE = HALT2 then
                          SWITCH := TRUE
                        else
                          begin
                            AKT1 := AKT ;
                            AKT := AKT -> . NEXT
                          end (* else *)
                      end (* while *) ;
                    if AKT <> NIL then
                      begin
                        AKT1 -> . NEXT := AKT -> . NEXT ;
                        WRITELN ( 'EINBIND. GELOESCHT' )
                      end (* then *)
                    else
                      WRITELN ( 'EINBIND. WAR SCHON NICHT VORHANDEN' )
                  end (* else *)
              end (* then *)
            else
              WRITELN ( 'EINBIND. WAR SCHON NICHT VORHANDEN' )
          end (* else *)
      end (* LE *) ;


   begin (* LOEE *)
     repeat
       WRITELN ;
       WRITELN ( 'ZU LOESCHENDE EINBIND. EINGEBEN !' ) ;
       WRITELN ( 'BEZIRK:' ) ;
       IEIN ( BEZ ) ;
       if BEZ <> 0 then
         begin
           WRITELN ( 'HALTESTELLE:' ) ;
           IEIN ( HALT2 ) ;
           if HALT2 <> 0 then
             begin
               LE ( BEZ , HALT2 )
             end (* then *)
         end (* then *)
     until ( BEZ = 0 )
   end (* LOEE *) ;



procedure LOES ;

(*************************)
(*   STRECKEN LOESCHEN   *)
(*************************)


   var HALT1 , HALT2 : INTEGER ;


   procedure LS ( HALT1 , HALT2 : INTEGER ) ;

   (*****************************)
   (*   EINE STRECKE LOESCHEN   *)
   (*****************************)


      var AKT , AKT1 : HPTYP ;
          I : INTEGER ;
          SWITCH : BOOLEAN ;

      begin (* LS *)
        I := 1 ;
        while ( HTRANSFER [ I ] < HALT1 ) and ( I <= HZAHL ) do
          I := I + 1 ;
        if I > HZAHL then
          WRITELN ( 'STRECKE WAR SCHON NICHT VORHANDEN' )
        else
          begin
            AKT := STRECKENNETZ [ I ] ;
            if AKT <> NIL then
              begin
                if AKT -> . HSTELLE = HALT2 then
                  begin
                    STRECKENNETZ [ I ] := AKT -> . NEXT ;
                    WRITELN ( 'STRECKE GELOESCHT' )
                  end (* then *)
                else
                  begin
                    AKT1 := AKT ;
                    AKT := AKT -> . NEXT ;
                    SWITCH := FALSE ;
                    while ( AKT <> NIL ) and ( not SWITCH ) do
                      begin
                        if AKT -> . HSTELLE = HALT2 then
                          SWITCH := TRUE
                        else
                          begin
                            AKT1 := AKT ;
                            AKT := AKT -> . NEXT
                          end (* else *)
                      end (* while *) ;
                    if AKT <> NIL then
                      begin
                        AKT1 -> . NEXT := AKT -> . NEXT ;
                        WRITELN ( 'STRECKE GELOESCHT' )
                      end (* then *)
                    else
                      WRITELN ( 'STRECKE WAR SCHON NICHT VORHANDEN' )
                  end (* else *)
              end (* then *)
            else
              WRITELN ( 'STRECKE WAR SCHON NICHT VORHANDEN' )
          end (* else *)
      end (* LS *) ;


   function LCHECK ( HALT1 , HALT2 : INTEGER ) : BOOLEAN ;

   (*******************************************)
   (*   STRECKE NOCH IN LINIEN VORHANDEN ?    *)
   (*******************************************)


      var AKT1 , AKT2 : LEPTYP ;
          I : INTEGER ;
          SWITCH : BOOLEAN ;

      begin (* LCHECK *)
        LCHECK := TRUE ;
        for I := 1 to LZAHL do
          begin
            AKT1 := LINIEN [ I ] ;
            AKT2 := AKT1 ;
            SWITCH := FALSE ;
            while ( not SWITCH ) and ( AKT1 <> NIL ) do
              begin
                if AKT1 -> . HSTELLE = HALT1 then
                  begin
                    if AKT1 -> . STRECKE -> . HSTELLE = HALT2 then
                      SWITCH := TRUE
                    else
                      begin
                        AKT1 := AKT1 -> . STRECKE ;
                        if AKT1 = AKT2 then
                          AKT1 := NIL
                      end (* else *)
                  end (* then *)
                else
                  begin
                    AKT1 := AKT1 -> . STRECKE ;
                    if AKT1 = AKT2 then
                      AKT1 := NIL
                  end (* else *)
              end (* while *) ;
            if SWITCH then
              begin
                LCHECK := FALSE ;
                WRITELN ( 'STRECKE NOCH IN LINIE:' , LTRANSFER [ I ] ,
                          ' VORHANDEN !' )
              end (* then *)
          end (* for *)
      end (* LCHECK *) ;


   begin (* LOES *)
     repeat
       WRITELN ;
       WRITELN ( 'ZU LOESCHENDE STRECKE EINGEBEN !' ) ;
       WRITELN ( 'AUSGANGSHALTESTELLE:' ) ;
       IEIN ( HALT1 ) ;
       if HALT1 <> 0 then
         begin
           WRITELN ( 'ZIELHALTESTELLE:' ) ;
           IEIN ( HALT2 ) ;
           if HALT2 <> 0 then
             begin
               if LCHECK ( HALT1 , HALT2 ) then
                 LS ( HALT1 , HALT2 )
               else
                 WRITELN ( 'KEINE LOESCHUNG !' )
             end (* then *)
         end (* then *)
     until ( HALT1 = 0 )
   end (* LOES *) ;



procedure LOEB ;

(*****************************************)
(*   BEZIRKE MIT EINBINDUNGEN LOESCHEN   *)
(*****************************************)


   var BEZ : INTEGER ;


   procedure LB ( BEZ : INTEGER ) ;

      var I , J : INTEGER ;

      begin (* LB *)
        I := 1 ;
        while ( BTRANSFER [ I ] <> BEZ ) and ( I <= BZAHL ) do
          I := I + 1 ;
        if I > BZAHL then
          WRITELN ( 'BEZIRK WAR SCHON NICHT VORHANDEN' )
        else
          begin
            for J := I to ( BZAHL - 1 ) do
              begin
                BTRANSFER [ J ] := BTRANSFER [ J + 1 ] ;
                EINBIND [ J ] := EINBIND [ J + 1 ]
              end (* for *) ;
            BZAHL := BZAHL - 1 ;
            WRITELN ( 'BEZIRK NR.' , BEZ : 10 , ' GELOESCHT' )
          end (* else *)
      end (* LB *) ;


   begin (* LOEB *)
     repeat
       WRITELN ;
       WRITELN ( 'ZU LOESCHENDER BEZIRK:' ) ;
       IEIN ( BEZ ) ;
       if BEZ <> 0 then
         LB ( BEZ )
     until BEZ = 0
   end (* LOEB *) ;



procedure LOEH ;

(*****************************)
(*   HALTESTELLEN LOESCHEN   *)
(*****************************)


   var HALT : INTEGER ;


   procedure LH ( HALT : INTEGER ) ;

   (*********************************)
   (*   EINE HALTESTELLE LOESCHEN   *)
   (*********************************)


      var I , J : INTEGER ;
          T : HTEXT ;

      begin (* LH *)
        I := 1 ;
        while ( HTRANSFER [ I ] <> HALT ) and ( I <= HZAHL ) do
          I := I + 1 ;
        if I > HZAHL then
          WRITELN ( 'HALTESTELLE WAR SCHON NICHT VORHANDEN' )
        else
          begin
            T := HTEXTE [ I ] ;
            for J := I to ( HZAHL - 1 ) do
              begin
                HPOS [ J ] := HPOS [ J + 1 ] ;
                HTEXTE [ J ] := HTEXTE [ J + 1 ] ;
                HTRANSFER [ J ] := HTRANSFER [ J + 1 ] ;
                STRECKENNETZ [ J ] := STRECKENNETZ [ J + 1 ]
              end (* for *) ;
            HZAHL := HZAHL - 1 ;
            WRITELN ( 'HALTESTELLE NR.' , HALT : 10 , ' (' , T ,
                      ') GELOESCHT' ) ;
          end (* else *)
      end (* LH *) ;


   function ECHECK ( HALT : INTEGER ) : BOOLEAN ;

   (*******************************************)
   (*   HALTESTELLE NOCH IN EINBINDUNGEN ?    *)
   (*******************************************)


      var AKT : EPTYP ;
          SWITCH : BOOLEAN ;
          I : INTEGER ;

      begin (* ECHECK *)
        ECHECK := TRUE ;
        for I := 1 to BZAHL do
          begin
            AKT := EINBIND [ I ] ;
            SWITCH := FALSE ;
            while AKT <> NIL do
              begin
                if AKT -> . HSTELLE = HALT then
                  SWITCH := TRUE ;
                AKT := AKT -> . NEXT
              end (* while *) ;
            if SWITCH then
              begin
                WRITELN ( 'EINBINDUNG: (' , BTRANSFER [ I ] , '/' ,
                          HALT , ')' ) ;
                ECHECK := FALSE
              end (* then *)
          end (* for *) ;
        WRITELN
      end (* ECHECK *) ;


   function SCHECK ( HALT : INTEGER ) : BOOLEAN ;

   (*******************************************)
   (*   HALTESTELLE NOCH IN STRECKENNETZ ?    *)
   (*******************************************)


      var AKT : HPTYP ;
          SWITCH : BOOLEAN ;
          I : INTEGER ;

      begin (* SCHECK *)
        SCHECK := TRUE ;
        for I := 1 to HZAHL do
          begin
            AKT := STRECKENNETZ [ I ] ;
            SWITCH := FALSE ;
            while AKT <> NIL do
              begin
                if AKT -> . HSTELLE = HALT then
                  SWITCH := TRUE ;
                AKT := AKT -> . NEXT
              end (* while *) ;
            if SWITCH then
              begin
                WRITELN ( 'STRECKE VON:' , HTRANSFER [ I ] , ' NACH:' ,
                          HALT ) ;
                SCHECK := FALSE
              end (* then *) ;
            if ( HTRANSFER [ I ] = HALT ) and ( STRECKENNETZ [ I ] <>
            NIL ) then
              begin
                AKT := STRECKENNETZ [ I ] ;
                SCHECK := FALSE ;
                while AKT <> NIL do
                  begin
                    WRITELN ( 'STRECKE VON:' , HALT , ' NACH:' , AKT ->
                              . HSTELLE ) ;
                    AKT := AKT -> . NEXT
                  end (* while *)
              end (* then *)
          end (* for *) ;
        WRITELN
      end (* SCHECK *) ;


   begin (* LOEH *)
     repeat
       WRITELN ;
       WRITELN ( 'ZU LOESCHENDE HALTESTELLE:' ) ;
       IEIN ( HALT ) ;
       if HALT <> 0 then
         begin
           if SCHECK ( HALT ) and ECHECK ( HALT ) then
             begin
               LH ( HALT )
             end (* then *)
           else
             WRITELN ( 'KEINE LOESCHUNG ERFOLGT' )
         end (* then *)
     until HALT = 0
   end (* LOEH *) ;



procedure INSERTL ( NUMMER : INTEGER ; var BHI : INTEGER ; LP : LEPTYP
                  ; var STATUS : STAT ) ;

(***********************)
(*   LINIE EINFUEGEN   *)
(***********************)


   var I , J : LTYP ;

   begin (* INSERTL *)
     if LZAHL = LMAX - 1 then
       STATUS := 3
     else
       begin
         I := 1 ;
         while ( LTRANSFER [ I ] < NUMMER ) and ( I <= LZAHL ) do
           I := I + 1 ;
         if I > LZAHL then
           begin
             BHI := I ;
             LTRANSFER [ I ] := NUMMER ;
             LZAHL := LZAHL + 1 ;
             LINIEN [ I ] := LP ;
             STATUS := 1
           end (* then *)
         else
           if LTRANSFER [ I ] <> NUMMER then
             begin
               for J := LZAHL DOWNTO I do
                 begin
                   LTRANSFER [ J + 1 ] := LTRANSFER [ J ] ;
                   LINIEN [ J + 1 ] := LINIEN [ J ]
                 end (* for *) ;
               BHI := I ;
               LTRANSFER [ I ] := NUMMER ;
               LZAHL := LZAHL + 1 ;
               LINIEN [ I ] := LP ;
               STATUS := 1
             end (* then *)
           else
             STATUS := 2
       end (* else *)
   end (* INSERTL *) ;



procedure INSERTB ( NUMMER : INTEGER ; var STATUS : STAT ) ;

(************************)
(*   BEZIRK EINFUEGEN   *)
(************************)


   var I , J : BTYP ;

   begin (* INSERTB *)
     if BZAHL = BMAX - 1 then
       STATUS := 3
     else
       begin
         I := 1 ;
         while ( BTRANSFER [ I ] < NUMMER ) and ( I <= BZAHL ) do
           I := I + 1 ;
         if I > BZAHL then
           begin
             BTRANSFER [ I ] := NUMMER ;
             BZAHL := BZAHL + 1 ;
             EINBIND [ I ] := NIL ;
             STATUS := 1
           end (* then *)
         else
           if ( BTRANSFER [ I ] <> NUMMER ) then
             begin
               for J := BZAHL DOWNTO I do
                 begin
                   BTRANSFER [ J + 1 ] := BTRANSFER [ J ] ;
                   EINBIND [ J + 1 ] := EINBIND [ J ]
                 end (* for *) ;
               BTRANSFER [ I ] := NUMMER ;
               BZAHL := BZAHL + 1 ;
               EINBIND [ I ] := NIL ;
               STATUS := 1
             end (* then *)
           else
             STATUS := 2
       end (* else *)
   end (* INSERTB *) ;



procedure INSERTH ( NUMMER : INTEGER ; var STATUS : STAT ) ;

(*****************************)
(*   HALTESTELLE EINFUEGEN   *)
(*****************************)


   var I , J : HTYP ;
       X , Y : INTEGER ;
       T : HTEXT ;
       L : INTEGER ;

   begin (* INSERTH *)
     if HZAHL = HMAX - 1 then
       STATUS := 3
     else
       begin
         I := 1 ;
         while ( HTRANSFER [ I ] < NUMMER ) and ( I <= HZAHL ) do
           I := I + 1 ;
         if I > HZAHL then
           begin
             HTRANSFER [ I ] := NUMMER ;
             HZAHL := HZAHL + 1 ;
             STATUS := 1
           end (* then *)
         else
           if ( HTRANSFER [ I ] <> NUMMER ) then
             begin
               for J := HZAHL DOWNTO I do
                 begin
                   HPOS [ J + 1 ] := HPOS [ J ] ;
                   HTEXTE [ J + 1 ] := HTEXTE [ J ] ;
                   HTRANSFER [ J + 1 ] := HTRANSFER [ J ] ;
                   STRECKENNETZ [ J + 1 ] := STRECKENNETZ [ J ]
                 end (* for *) ;
               HTRANSFER [ I ] := NUMMER ;
               HZAHL := HZAHL + 1 ;
               STRECKENNETZ [ I ] := NIL ;
               STATUS := 1
             end (* then *)
           else
             STATUS := 2 ;
         if STATUS = 1 then
           begin
             WRITELN ( 'HALTEST.-BEZEICHUNG (25-STELLIG):' ) ;
             TEIN ( T , L ) ;
             WRITELN ( 'X-POSITION:' ) ;
             IEIN ( X ) ;
             WRITELN ( 'Y-POSITION:' ) ;
             IEIN ( Y ) ;
             HPOS [ I ] . X := X ;
             HPOS [ I ] . Y := Y ;
             HTEXTE [ I ] := T
           end (* then *)
       end (* else *)
   end (* INSERTH *) ;



procedure NEUBEZ ( NUMMER : INTEGER ; var STATUS : STAT ) ;

(************************)
(*   BEZIRK AUFNEHMEN   *)
(************************)


   begin (* NEUBEZ *)
     INSERTB ( NUMMER , STATUS ) ;
     case STATUS of
       1 : WRITELN ( 'BEZIRK AUFGENOMMEN' ) ;
       2 : WRITELN ( 'BEZIRK SCHON VORHANDEN' ) ;
       3 : WRITELN ( 'UEBERLAUF:FELDER ZU KLEIN DIMENSIONIERT' )
     end (* case *)
   end (* NEUBEZ *) ;



procedure NEUHALT ( NUMMER : INTEGER ; var STATUS : STAT ) ;

(*****************************)
(*   HALTESTELLE AUFNEHMEN   *)
(*****************************)


   begin (* NEUHALT *)
     INSERTH ( NUMMER , STATUS ) ;
     case STATUS of
       1 : WRITELN ( 'HALTESTELLE AUFGENOMMEN' ) ;
       2 : WRITELN ( 'HALTESTELLE SCHON VORHANDEN' ) ;
       3 : WRITELN ( 'UEBERLAUF:FELDER ZU KLEIN DIMENSIONIERT' )
     end (* case *)
   end (* NEUHALT *) ;



procedure EDB ;

(***********************)
(*   BEZIRK EINGEBEN   *)
(***********************)


   label 2 ;

   var X : INTEGER ;
       STATUS : STAT ;

   begin (* EDB *)
     2 :
     WRITELN ;
     WRITELN ;
     WRITELN ( 'NR. DES BEZIRKES:' ) ;
     IEIN ( X ) ;
     if ( X >= 1 ) then
       begin
         NEUBEZ ( X , STATUS ) ;
         goto 2
       end (* then *)
   end (* EDB *) ;



procedure EDH ;

(****************************)
(*   HALTESTELLE EINGEBEN   *)
(****************************)


   label 2 ;

   var X : INTEGER ;
       STATUS : STAT ;

   begin (* EDH *)
     2 :
     WRITELN ;
     WRITELN ;
     WRITELN ( 'NR. DER HALTESTELLE:' ) ;
     IEIN ( X ) ;
     if ( X >= 1 ) then
       begin
         NEUHALT ( X , STATUS ) ;
         goto 2
       end (* then *)
   end (* EDH *) ;



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
       STATUS := 2
     else
       if BTRANSFER [ I ] = BEZ then
         begin
           IND := I ;
           STATUS := 1
         end (* then *)
       else
         STATUS := 2
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
       STATUS := 2
     else
       if HTRANSFER [ I ] = HALT then
         begin
           IND := I ;
           STATUS := 1
         end (* then *)
       else
         STATUS := 2
   end (* SEARCHH *) ;



procedure SEARCHE ( BIND : BTYP ; HIND : HTYP ; var STATUS : STAT ) ;

(*******************************)
(*   EINBINDUNG VORHANDEN ?    *)
(*******************************)


   var AKT : EPTYP ;
       SWITCH : BOOLEAN ;

   begin (* SEARCHE *)
     SWITCH := TRUE ;
     AKT := EINBIND [ BIND ] ;
     while ( AKT <> NIL ) do
       begin
         if AKT -> . HSTELLE = HTRANSFER [ HIND ] then
           SWITCH := FALSE ;
         AKT := AKT -> . NEXT
       end (* while *) ;
     if SWITCH then
       STATUS := 2
     else
       STATUS := 1
   end (* SEARCHE *) ;



procedure SEARCHS ( AIND : HTYP ; EIND : HTYP ; var STATUS : STAT ) ;

(****************************)
(*   STRECKE VORHANDEN ?    *)
(****************************)


   var AKT : HPTYP ;
       SWITCH : BOOLEAN ;

   begin (* SEARCHS *)
     SWITCH := TRUE ;
     AKT := STRECKENNETZ [ AIND ] ;
     while ( AKT <> NIL ) do
       begin
         if AKT -> . HSTELLE = HTRANSFER [ EIND ] then
           SWITCH := FALSE ;
         AKT := AKT -> . NEXT
       end (* while *) ;
     if SWITCH then
       STATUS := 2
     else
       STATUS := 1
   end (* SEARCHS *) ;



procedure INSERTE ( BEZ , HALT , FWZ : INTEGER ; var STATUS : STAT ) ;

(****************************)
(*   EINBINDUNG EINFUEGEN   *)
(****************************)


   var P : EPTYP ;
       BIND : BTYP ;
       HIND : HTYP ;
       STATUS1 , STATUS2 , STATUS3 : STAT ;

   begin (* INSERTE *)
     SEARCHB ( BEZ , BIND , STATUS1 ) ;
     SEARCHH ( HALT , HIND , STATUS2 ) ;
     if ( STATUS1 = 1 ) and ( STATUS2 = 1 ) then
       begin
         SEARCHE ( BIND , HIND , STATUS3 ) ;
         if STATUS3 = 2 then
           begin
             NEW ( P ) ;
             P -> . HSTELLE := HALT ;
             P -> . NEXT := EINBIND [ BIND ] ;
             P -> . FW := FWZ ;
             EINBIND [ BIND ] := P ;
             STATUS := 1
           end (* then *)
         else
           STATUS := 4
       end (* then *)
     else
       if STATUS1 = 2 then
         STATUS := 2
       else
         STATUS := 3
   end (* INSERTE *) ;



procedure INSERTS ( AHALT , EHALT : INTEGER ; var STATUS : STAT ) ;

(*************************)
(*   STRECKE EINFUEGEN   *)
(*************************)


   var P : HPTYP ;
       AIND , EIND : HTYP ;
       STATUS1 , STATUS2 , STATUS3 : STAT ;

   begin (* INSERTS *)
     SEARCHH ( AHALT , AIND , STATUS1 ) ;
     SEARCHH ( EHALT , EIND , STATUS2 ) ;
     if ( STATUS1 = 1 ) and ( STATUS2 = 1 ) then
       begin
         SEARCHS ( AIND , EIND , STATUS3 ) ;
         if STATUS3 = 2 then
           begin
             NEW ( P ) ;
             P -> . HSTELLE := EHALT ;
             P -> . NEXT := STRECKENNETZ [ AIND ] ;
             STRECKENNETZ [ AIND ] := P ;
             STATUS := 1
           end (* then *)
         else
           STATUS := 4
       end (* then *)
     else
       if STATUS1 = 2 then
         STATUS := 2
       else
         STATUS := 3
   end (* INSERTS *) ;



procedure NEUE ( BEZ , HALT : INTEGER ; FWZ : INTEGER ; var STATUS :
               STAT ) ;

(****************************)
(*   EINBINDUNG AUFNEHMEN   *)
(****************************)


   begin (* NEUE *)
     INSERTE ( BEZ , HALT , FWZ , STATUS ) ;
     case STATUS of
       1 : WRITELN ( 'EINBINDUNG AUFGENOMMEN' ) ;
       2 : WRITELN ( 'BEZIRK NICHT VORHANDEN' ) ;
       3 : WRITELN ( 'HALTESTELLE NICHT VORHANDEN' ) ;
       4 : WRITELN ( 'EINBINDUNG SCHON VORHANDEN' )
     end (* case *)
   end (* NEUE *) ;



procedure NEUSTRECKE ( AHALT , EHALT : INTEGER ; var STATUS : STAT ) ;

(*************************)
(*   STRECKE AUFNEHMEN   *)
(*************************)


   begin (* NEUSTRECKE *)
     INSERTS ( AHALT , EHALT , STATUS ) ;
     case STATUS of
       1 : WRITELN ( 'STRECKE AUFGENOMMEN' ) ;
       2 : WRITELN ( 'AUSGANGSHALTESTELLE NICHT VORHANDEN' ) ;
       3 : WRITELN ( 'ZIELHALTESTELLE NICHT VORHANDEN' ) ;
       4 : WRITELN ( 'STRECKE SCHON VORHANDEN' )
     end (* case *)
   end (* NEUSTRECKE *) ;



procedure EDE ;

(***************************)
(*   EINBINDUNG EINGEBEN   *)
(***************************)


   label 3 ;

   var X , Y , Z : INTEGER ;
       STATUS : STAT ;

   begin (* EDE *)
     3 :
     WRITELN ;
     WRITELN ;
     WRITELN ( 'NR. DES BEZIRKES:' ) ;
     IEIN ( X ) ;
     if X > 0 then
       begin
         WRITELN ( 'NR. DER HALTESTELLE:' ) ;
         IEIN ( Y ) ;
         if Y > 0 then
           begin
             WRITELN ( 'FUSSWEG:' ) ;
             IEIN ( Z ) ;
             NEUE ( X , Y , Z , STATUS ) ;
             goto 3
           end (* then *)
       end (* then *)
   end (* EDE *) ;



procedure EDS ;

(************************)
(*   STRECKE EINGEBEN   *)
(************************)


   label 3 ;

   var X , Y : INTEGER ;
       STATUS : STAT ;

   begin (* EDS *)
     3 :
     WRITELN ;
     WRITELN ;
     WRITELN ( 'NR. DER AUSGANGSHALTESTELLE:' ) ;
     IEIN ( X ) ;
     if X > 0 then
       begin
         WRITELN ( 'NR. DER ZIELHALTESTELLE:' ) ;
         IEIN ( Y ) ;
         if Y > 0 then
           begin
             NEUSTRECKE ( X , Y , STATUS ) ;
             goto 3
           end (* then *)
       end (* then *)
   end (* EDS *) ;



procedure EDL ;

(************************************)
(*   LINIE EINGEBEN UND AUFNEHMEN   *)
(************************************)


   label 7 , 8 ;

   var LNR , AHALT , NHALT , BHI , K , L : INTEGER ;
       NP , AP , LP : LEPTYP ;
       STATUS : STAT ;
       BH2 : array [ 1 .. DIFF ] of INTEGER ;


   function LOK ( LP : LEPTYP ) : BOOLEAN ;

   (*******************************)
   (*   LINIE FORMAL KORREKT ?    *)
   (*******************************)


      var HALT1 , HALT2 : HTYP ;
          STATUS : STAT ;
          AKT1 , AKT2 : LEPTYP ;

      begin (* LOK *)
        AKT1 := LP ;
        AKT2 := LP -> . STRECKE ;
        LOK := TRUE ;
        repeat
          SEARCHH ( AKT1 -> . HSTELLE , HALT1 , STATUS ) ;
          SEARCHH ( AKT2 -> . HSTELLE , HALT2 , STATUS ) ;
          SEARCHS ( HALT1 , HALT2 , STATUS ) ;
          if STATUS = 2 then
            begin
              LOK := FALSE ;
              WRITELN ( 'STRECKE: (' , AKT1 -> . HSTELLE , '/' , AKT2
                        -> . HSTELLE , ') FEHLT !' )
            end (* then *) ;
          AKT1 := AKT2 ;
          AKT2 := AKT2 -> . STRECKE
        until AKT1 = LP ;
        WRITELN
      end (* LOK *) ;


   procedure FAHRZEIT ;

   (****************************)
   (*   FAHRTZEITEN EINGEBEN   *)
   (****************************)


      var AP1 , AP2 : LEPTYP ;

      begin (* FAHRZEIT *)
        AP1 := LP ;
        AP2 := LP -> . STRECKE ;
        repeat
          WRITELN ( 'FAHRZEIT VON' , AP1 -> . HSTELLE , ' NACH' , AP2
                    -> . HSTELLE , ' :' ) ;
          IEIN ( AP1 -> . FZ ) ;
          AP1 := AP2 ;
          AP2 := AP2 -> . STRECKE
        until AP1 = LP ;
        WRITELN
      end (* FAHRZEIT *) ;


   procedure LSTRECKE ( var HALT : INTEGER ) ;

   (*************************************************)
   (*   NAECHSTE HALTESTELLE EINER LINIE EINGEBEN   *)
   (*************************************************)


      label 1 ;

      var HIND : HTYP ;
          STATUS : STAT ;

      begin (* LSTRECKE *)
        1 :
        WRITELN ( 'NAECHSTE HALTESTELLE:' ) ;
        IEIN ( HALT ) ;
        if HALT <> 0 then
          begin
            SEARCHH ( HALT , HIND , STATUS ) ;
            if STATUS = 2 then
              begin
                WRITELN ( 'HALTESTELLE NICHT VORHANDEN' ) ;
                WRITELN ( 'LETZTE EINGABE UNGUELTIG !' ) ;
                goto 1
              end (* then *)
          end (* then *)
      end (* LSTRECKE *) ;


   begin (* EDL *)
     8 :
     WRITELN ;
     WRITELN ;
     WRITELN ( 'LINIENEINGABE:' ) ;
     WRITELN ;
     LSTRECKE ( AHALT ) ;
     if AHALT > 0 then
       begin
         NEW ( LP ) ;
         LP -> . HSTELLE := AHALT ;
         LP -> . STRECKE := NIL ;
         LSTRECKE ( NHALT ) ;
         AP := LP ;
         while NHALT > 0 do
           begin
             NEW ( NP ) ;
             NP -> . HSTELLE := NHALT ;
             AP -> . STRECKE := NP ;
             AP := AP -> . STRECKE ;
             AP -> . STRECKE := NIL ;
             LSTRECKE ( NHALT )
           end (* while *) ;
         if ( AP -> . HSTELLE = LP -> . HSTELLE ) and ( LP <> AP ) then
           begin
             AP -> . STRECKE := LP -> . STRECKE ;
             LP := AP ;
             WRITELN ;
             if not LOK ( LP ) then
               goto 8 ;
             7 :
             WRITELN ( 'LINIENNUMMER:' ) ;
             IEIN ( LNR ) ;
             if LNR > 0 then
               begin
                 for K := 1 to DIFF do
                   begin
                     WRITELN ( 'BEDHFKT(' , K , '):' ) ;
                     IEIN ( BH2 [ K ] )
                   end (* for *) ;
                 INSERTL ( LNR , BHI , LP , STATUS ) ;
                 case STATUS of
                   1 : begin
                         WRITELN ( 'LINIE AUFGENOMMEN' ) ;
                         WRITELN ;
                         if BHI = LZAHL then
                           begin
                             for K := 1 to DIFF do
                               BH [ BHI , K ] := BH2 [ K ]
                           end (* then *)
                         else
                           begin
                             for L := LZAHL - 1 DOWNTO BHI do
                               for K := 1 to DIFF do
                                 BH [ L + 1 , K ] := BH [ L , K ] ;
                             for K := 1 to DIFF do
                               BH [ BHI , K ] := BH2 [ K ]
                           end (* else *) ;
                         FAHRZEIT
                       end (* tag/ca *) ;
                   2 : begin
                         WRITELN ( 'LINIE SCHON VORHANDEN' ) ;
                         goto 7
                       end (* tag/ca *) ;
                   3 : begin
                         WRITELN ( 'FELDER ZU KLEIN DIMENSIONIERT' )
                       end (* tag/ca *)
                 end (* case *)
               end (* then *)
           end (* then *)
       end (* then *) ;
     if AHALT > 0 then
       goto 8
   end (* EDL *) ;



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



procedure HAUSG ;

(*****************************)
(*   HALTESTELLEN AUSGEBEN   *)
(*****************************)


   var I , J : INTEGER ;

   begin (* HAUSG *)
     WRITELN ( 'HALTESTELLEN:' ) ;
     WRITELN ;
     J := 1 ;
     for I := 1 to HZAHL do
       begin
         WRITE ( ' (' , I : 10 , ':' , HTRANSFER [ I ] : 10 , ' /' ,
                 HPOS [ I ] . X : 10 , ',' , HPOS [ I ] . Y : 10 ,
                 ' / ' , HTEXTE [ I ] , ')' ) ;
         J := J + 1 ;
         if J > 1 then
           begin
             WRITELN ;
             J := 1
           end (* then *)
       end (* for *)
   end (* HAUSG *) ;



procedure HAUSGDRUCK ;

(*****************************************)
(*   HALTESTELLEN AUSGEBEN AUF DRUCKER   *)
(*****************************************)


   var I : INTEGER ;

   begin (* HAUSGDRUCK *)
     UEBERSCHRIFT ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     WRITE ( DRUCKER , ' HALTESTELLEN:' ) ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     WRITE ( DRUCKER , ' HST-NUMMER' ) ;
     WRITE ( DRUCKER , '    HALTESTELLEN-BEZEICHNUNG ' ) ;
     WRITE ( DRUCKER , '    HALTEST.-KOORDINATEN' ) ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     for I := 1 to HZAHL do
       begin
         WRITE ( DRUCKER , ' ' , HTRANSFER [ I ] : 10 , '    ' , HTEXTE
                 [ I ] , '    (' , HPOS [ I ] . X : 10 , ' ,' , HPOS [
                 I ] . Y : 10 , ' )' ) ;
         if ZEILZAHL <= PZZ - 12 then
           DRUCKEZEILE
         else
           begin
             DRUCKEZEILE ;
             UEBERSCHRIFT ;
             DRUCKEZEILE ;
             DRUCKEZEILE ;
             WRITE ( DRUCKER , ' HST-NUMMER' ) ;
             WRITE ( DRUCKER , '    HALTESTELLEN-BEZEICHNUNG ' ) ;
             WRITE ( DRUCKER , '    HALTEST.-KOORDINATEN' ) ;
             DRUCKEZEILE ;
             DRUCKEZEILE ;
           end (* else *)
       end (* for *)
   end (* HAUSGDRUCK *) ;



procedure LAUSG ;

(***********************)
(*   LINIEN AUSGEBEN   *)
(***********************)


   var I , J , K : INTEGER ;
       AP1 , AP2 : LEPTYP ;

   begin (* LAUSG *)
     WRITELN ( 'LINIEN:' ) ;
     WRITELN ;
     for I := 1 to LZAHL do
       begin
         WRITELN ;
         WRITELN ;
         WRITELN ( 'LINIE NR.' , LTRANSFER [ I ] : 12 , ' :' ) ;
         WRITELN ;
         for K := 1 to DIFF do
           WRITELN ( 'BEDHFKT (' , K : 2 , '):' , BH [ I , K ] : 10 ) ;
         WRITELN ;
         J := 1 ;
         AP1 := LINIEN [ I ] ;
         AP2 := AP1 ;
         repeat
           if J > 3 then
             begin
               WRITELN ;
               J := 1
             end (* then *) ;
           WRITE ( ' (' , AP2 -> . HSTELLE : 10 , '/' , AP2 -> . FZ :
                   10 , ')' ) ;
           AP2 := AP2 -> . STRECKE ;
           J := J + 1
         until AP2 = AP1 ;
         WRITELN ;
       end (* for *)
   end (* LAUSG *) ;



procedure LAUSGDRUCK ;

(***********************************)
(*   LINIEN AUSGEBEN AUF DRUCKER   *)
(***********************************)


   var I , K : INTEGER ;
       AP1 , AP2 : LEPTYP ;
       HIND : HTYP ;
       STATUS : STAT ;

   begin (* LAUSGDRUCK *)
     UEBERSCHRIFT ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     WRITE ( DRUCKER , ' LINIEN:' ) ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     for I := 1 to LZAHL do
       begin
         if ZEILZAHL > PZZ - 18 then
           UEBERSCHRIFT ;
         DRUCKEZEILE ;
         DRUCKEZEILE ;
         WRITE ( DRUCKER , ' LINIE NR.' , LTRANSFER [ I ] : 12 , ' :' )
                 ;
         DRUCKEZEILE ;
         DRUCKEZEILE ;
         for K := 1 to DIFF do
           begin
             WRITE ( DRUCKER , ' BEDIENUNGSZEITINTERVALL (' , K : 2 ,
                     ') :' , BH [ I , K ] : 10 ) ;
             DRUCKEZEILE
           end (* for *) ;
         if ZEILZAHL > PZZ - 15 then
           UEBERSCHRIFT ;
         DRUCKEZEILE ;
         WRITE ( DRUCKER , ' ' , 'HST-NUMMER' , '    ' ) ;
         WRITE ( DRUCKER , 'HALTESTELLEN-BEZEICHNUNG ' , '  ' ) ;
         WRITE ( DRUCKER , 'FAHRZEITEN' ) ;
         DRUCKEZEILE ;
         DRUCKEZEILE ;
         AP1 := LINIEN [ I ] ;
         AP2 := AP1 ;
         repeat
           WRITE ( DRUCKER , ' ' , AP2 -> . HSTELLE : 10 , '    ' ) ;
           SEARCHH ( AP2 -> . HSTELLE , HIND , STATUS ) ;
           WRITE ( DRUCKER , HTEXTE [ HIND ] , '  ' , AP2 -> . FZ : 10
                   ) ;
           DRUCKEZEILE ;
           AP2 := AP2 -> . STRECKE ;
         until AP2 = AP1 ;
         WRITE ( DRUCKER , ' ' , AP2 -> . HSTELLE : 10 , '    ' ) ;
         SEARCHH ( AP2 -> . HSTELLE , HIND , STATUS ) ;
         WRITE ( DRUCKER , HTEXTE [ HIND ] ) ;
         DRUCKEZEILE ;
         DRUCKEZEILE
       end (* for *)
   end (* LAUSGDRUCK *) ;



procedure EAUSG ;

(*****************************)
(*   EINBINDUNGEN AUSGEBEN   *)
(*****************************)


   var I , J : INTEGER ;
       AKT : EPTYP ;

   begin (* EAUSG *)
     WRITELN ( 'EINBINDUNGEN:' ) ;
     WRITELN ;
     for I := 1 to BZAHL do
       begin
         J := 1 ;
         AKT := EINBIND [ I ] ;
         while AKT <> NIL do
           begin
             WRITE ( ' (' , BTRANSFER [ I ] : 10 , '/' , AKT -> .
                     HSTELLE : 10 , '/' , AKT -> . FW : 10 , ')' ) ;
             AKT := AKT -> . NEXT ;
             J := J + 1 ;
             if J > 2 then
               begin
                 WRITELN ;
                 J := 1
               end (* then *)
           end (* while *) ;
         if J <> 1 then
           WRITELN
       end (* for *)
   end (* EAUSG *) ;



procedure EAUSGDRUCK ;

(*****************************************)
(*   EINBINDUNGEN AUSGEBEN AUF DRUCKER   *)
(*****************************************)


   var I : INTEGER ;
       AKT : EPTYP ;
       ERSTER : BOOLEAN ;
       STATUS : STAT ;
       HIND : HTYP ;

   begin (* EAUSGDRUCK *)
     UEBERSCHRIFT ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     WRITE ( DRUCKER , ' EINBINDUNGEN:' ) ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     WRITE ( DRUCKER , ' BEZ-NUMMER    ' ) ;
     WRITE ( DRUCKER , 'HST-NUMMER    HALTESTELLEN-BEZEICHNUNG ' ) ;
     WRITE ( DRUCKER , '   FUSSWEG' ) ;
     DRUCKEZEILE ;
     for I := 1 to BZAHL do
       begin
         AKT := EINBIND [ I ] ;
         ERSTER := TRUE ;
         while AKT <> NIL do
           begin
             if ERSTER then
               begin
                 ERSTER := FALSE ;
                 DRUCKEZEILE ;
                 WRITE ( DRUCKER , ' ' , BTRANSFER [ I ] : 10 , '    '
                         ) ;
               end (* then *)
             else
               WRITE ( DRUCKER , ' ' : 15 ) ;
             WRITE ( DRUCKER , AKT -> . HSTELLE : 10 , '    ' ) ;
             SEARCHH ( AKT -> . HSTELLE , HIND , STATUS ) ;
             WRITE ( DRUCKER , HTEXTE [ HIND ] , '  ' , AKT -> . FW : 8
                     ) ;
             DRUCKEZEILE ;
             if ZEILZAHL > PZZ - 12 then
               begin
                 UEBERSCHRIFT ;
                 DRUCKEZEILE ;
                 DRUCKEZEILE ;
                 WRITE ( DRUCKER , ' BEZ-NUMMER    ' ) ;
                 WRITE ( DRUCKER ,
                         'HST-NUMMER    HALTESTELLEN-BEZEICHNUNG ' ) ;
                 WRITE ( DRUCKER , '   FUSSWEG' ) ;
                 DRUCKEZEILE ;
                 ERSTER := TRUE ;
               end (* then *) ;
             AKT := AKT -> . NEXT ;
           end (* while *)
       end (* for *)
   end (* EAUSGDRUCK *) ;



procedure SAUSG ;

(*****************************)
(*   STRECKENNETZ AUSGEBEN   *)
(*****************************)


   var I , J : INTEGER ;
       AKT : HPTYP ;

   begin (* SAUSG *)
     WRITELN ( 'STRECKENNETZ:' ) ;
     WRITELN ;
     for I := 1 to HZAHL do
       begin
         J := 1 ;
         AKT := STRECKENNETZ [ I ] ;
         while AKT <> NIL do
           begin
             WRITE ( ' (' , HTRANSFER [ I ] : 10 , '/' , AKT -> .
                     HSTELLE : 10 , ')' ) ;
             AKT := AKT -> . NEXT ;
             J := J + 1 ;
             if J > 3 then
               begin
                 WRITELN ;
                 J := 1
               end (* then *)
           end (* while *) ;
         if J <> 1 then
           WRITELN
       end (* for *)
   end (* SAUSG *) ;



procedure SAUSGDRUCK ;

(*****************************************)
(*   STRECKENNETZ AUSGEBEN AUF DRUCKER   *)
(*****************************************)


   var I : INTEGER ;
       AKT : HPTYP ;
       ERSTER : BOOLEAN ;
       HIND : HTYP ;
       STATUS : STAT ;

   begin (* SAUSGDRUCK *)
     UEBERSCHRIFT ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     WRITE ( DRUCKER , ' STRECKENNETZ:' ) ;
     DRUCKEZEILE ;
     DRUCKEZEILE ;
     WRITE ( DRUCKER , '  QUELL-HST    HALTESTELLEN-BEZEICHNUNG ' ) ;
     WRITE ( DRUCKER , ' ' : 10 ) ;
     WRITE ( DRUCKER , '  ZIEL-HST    HALTESTELLEN-BEZEICHNUNG ' ) ;
     DRUCKEZEILE ;
     for I := 1 to HZAHL do
       begin
         ERSTER := TRUE ;
         AKT := STRECKENNETZ [ I ] ;
         while AKT <> NIL do
           begin
             if ERSTER then
               begin
                 if ZEILZAHL > PZZ - 15 then
                   begin
                     UEBERSCHRIFT ;
                     DRUCKEZEILE ;
                     DRUCKEZEILE ;
                     WRITE ( DRUCKER ,
                            '  QUELL-HST    HALTESTELLEN-BEZEICHNUNG '
                             ) ;
                     WRITE ( DRUCKER , ' ' : 10 ) ;
                     WRITE ( DRUCKER ,
                             '  ZIEL-HST    HALTESTELLEN-BEZEICHNUNG '
                             ) ;
                     DRUCKEZEILE
                   end (* then *) ;
                 DRUCKEZEILE ;
                 WRITE ( DRUCKER , ' ' , HTRANSFER [ I ] : 10 , '    '
                         , HTEXTE [ I ] ) ;
                 WRITE ( DRUCKER , ' ' : 10 ) ;
                 ERSTER := FALSE
               end (* then *)
             else
               WRITE ( DRUCKER , ' ' : 50 ) ;
             SEARCHH ( AKT -> . HSTELLE , HIND , STATUS ) ;
             WRITE ( DRUCKER , AKT -> . HSTELLE : 10 , '    ' , HTEXTE
                     [ HIND ] ) ;
             DRUCKEZEILE ;
             AKT := AKT -> . NEXT ;
           end (* while *) ;
       end (* for *)
   end (* SAUSGDRUCK *) ;



procedure HKOORDAEND ;

(***************************************)
(*   HALTESTELLENKOORDINATEN AENDERN   *)
(***************************************)


   var I , HALT , X , Y : INTEGER ;
       T : HTEXT ;
       L : INTEGER ;

   begin (* HKOORDAEND *)
     repeat
       WRITELN ( 'HALTESTELLENKOORDINATEN UND -TEXTE AENDERN:' ) ;
       WRITELN ;
       WRITELN ( 'HALTESTELLE:' ) ;
       IEIN ( HALT ) ;
       if HALT > 0 then
         begin
           I := 1 ;
           while ( HTRANSFER [ I ] <> HALT ) and ( I <= HZAHL ) do
             I := I + 1 ;
           if I <= HZAHL then
             begin
               WRITELN ;
               WRITELN ( 'ALT:' , HPOS [ I ] . X : 10 , ' /' , HPOS [ I
                         ] . Y : 10 , ' (' , HTEXTE [ I ] , ')' ) ;
               WRITELN ;
               WRITELN ( 'X:' ) ;
               IEIN ( X ) ;
               WRITELN ( 'Y:' ) ;
               IEIN ( Y ) ;
               WRITELN ( 'TEXT:' ) ;
               TEIN ( T , L ) ;
               HPOS [ I ] . X := X ;
               HPOS [ I ] . Y := Y ;
               if L <> 0 then
                 HTEXTE [ I ] := T ;
               WRITELN ;
               WRITELN ( 'NEU:' , HPOS [ I ] . X : 10 , ' /' , HPOS [ I
                         ] . Y : 10 , ' (' , HTEXTE [ I ] , ')' )
             end (* then *)
           else
             WRITELN ( 'HALTESTELLE NICHT VORHANDEN' )
         end (* then *) ;
       WRITELN
     until HALT <= 0
   end (* HKOORDAEND *) ;



procedure EINBFWAEND ;

(*********************************************)
(*   FUSSWEGDAUER VON EINBINDUNGEN AENDERN   *)
(*********************************************)


   var HALT , BEZ , I : INTEGER ;
       STATE : BOOLEAN ;
       AKT : EPTYP ;

   begin (* EINBFWAEND *)
     repeat
       STATE := TRUE ;
       WRITELN ( 'FUSSWEGZEITEN VON EINBINDUNGEN AENDERN:' ) ;
       WRITELN ( 'BEZIRK:' ) ;
       IEIN ( BEZ ) ;
       if BEZ > 0 then
         begin
           WRITELN ( 'HALTESTELLE:' ) ;
           IEIN ( HALT ) ;
           if HALT > 0 then
             begin
               I := 1 ;
               while ( BTRANSFER [ I ] <> BEZ ) and ( I <= BZAHL ) do
                 I := I + 1 ;
               if I <= BZAHL then
                 begin
                   AKT := EINBIND [ I ] ;
                   while ( AKT <> NIL ) do
                     begin
                       if AKT -> . HSTELLE = HALT then
                         begin
                           STATE := FALSE ;
                           WRITELN ( 'ALT:' , AKT -> . FW ) ;
                           WRITELN ( 'NEU:' ) ;
                           IEIN ( AKT -> . FW )
                         end (* then *) ;
                       AKT := AKT -> . NEXT
                     end (* while *)
                 end (* then *)
             end (* then *)
         end (* then *) ;
       if ( STATE ) and ( ( BEZ <> 0 ) and ( HALT <> 0 ) ) then
         WRITELN ( 'EINBINDUNG NICHT VORHANDEN' ) ;
       WRITELN
     until BEZ <= 0
   end (* EINBFWAEND *) ;



procedure LBHAEND ;

(**************************************************)
(*   BEDIENUNGSHAEUFIGKEITEN VON LINIEN AENDERN   *)
(**************************************************)


   var I , J , LINIE : INTEGER ;

   begin (* LBHAEND *)
     repeat
       WRITELN ( 'BEDIENUNGSHAEUFIGKEIT VON LINIEN AENDERN:' ) ;
       WRITELN ( 'LINIE:' ) ;
       IEIN ( LINIE ) ;
       if LINIE > 0 then
         begin
           I := 1 ;
           while ( LTRANSFER [ I ] <> LINIE ) and ( I <= LZAHL ) do
             I := I + 1 ;
           if I <= LZAHL then
             begin
               WRITELN ;
               WRITELN ( 'ALT:' ) ;
               for J := 1 to DIFF do
                 WRITELN ( 'BH(' , J , '):' , BH [ I , J ] ) ;
               WRITELN ;
               WRITELN ( 'NEU:' ) ;
               for J := 1 to DIFF do
                 begin
                   WRITELN ( 'BH(' , J , '):' ) ;
                   IEIN ( BH [ I , J ] )
                 end (* for *)
             end (* then *)
           else
             WRITELN ( 'LINIE NICHT VORHANDEN' )
         end (* then *) ;
       WRITELN
     until LINIE <= 0
   end (* LBHAEND *) ;



procedure LFZAEND ;

(**************************************)
(*   FAHRTZEITEN VON LINIEN AENDERN   *)
(**************************************)


   var I , LINIE : INTEGER ;
       AP , AP1 , AP2 : LEPTYP ;

   begin (* LFZAEND *)
     repeat
       WRITELN ( 'FAHRTZEITEN VON LINIE AENDERN:' ) ;
       WRITELN ( 'LINIE:' ) ;
       IEIN ( LINIE ) ;
       if LINIE > 0 then
         begin
           I := 1 ;
           while ( LTRANSFER [ I ] <> LINIE ) and ( I <= LZAHL ) do
             I := I + 1 ;
           if I <= LZAHL then
             begin
               AP := LINIEN [ I ] ;
               AP1 := AP ;
               AP2 := AP1 -> . STRECKE ;
               repeat
                 WRITELN ( 'FAHRZEIT VON' , AP1 -> . HSTELLE , ' NACH'
                           , AP2 -> . HSTELLE , ' :' ) ;
                 WRITELN ( 'ALT:' , AP1 -> . FZ ) ;
                 WRITELN ( 'NEU:' ) ;
                 IEIN ( AP1 -> . FZ ) ;
                 AP1 := AP2 ;
                 AP2 := AP2 -> . STRECKE
               until AP1 = AP
             end (* then *)
           else
             WRITELN ( 'LINIE NICHT VORHANDEN' )
         end (* then *) ;
       WRITELN
     until LINIE <= 0
   end (* LFZAEND *) ;



procedure LST ;

(*****************************)
(*   SYSTEMELEMENTE LISTEN   *)
(*****************************)


   label 6 ;

   var WAHL : INTEGER ;

   begin (* LST *)
     6 :
     CLRSCRN ;
     WRITELN ( 'LISTING:' ) ;
     WRITELN ;
     WRITELN ( '(1)-HALTESTELLEN---- AUF BILDSCHIRM' ) ;
     WRITELN ( '(2)-STRECKENNETZ---- AUF BILDSCHIRM' ) ;
     WRITELN ( '(3)-LINIEN---------- AUF BILDSCHIRM' ) ;
     WRITELN ( '(4)-BEZIRKE--------- AUF BILDSCHRIM' ) ;
     WRITELN ( '(5)-EINBINDUNGEN---- AUF BILDSCHIRM' ) ;
     WRITELN ( '(6)-HALTESTELLEN------- AUF DRUCKER' ) ;
     WRITELN ( '(7)-STRECKENNETZ------- AUF DRUCKER' ) ;
     WRITELN ( '(8)-LINIEN------------- AUF DRUCKER' ) ;
     WRITELN ( '(9)-BEZIRKE------------ AUF DRUCKER' ) ;
     WRITELN ( '(10)-EINBINDUNGEN------ AUF DRUCKER' ) ;
     WRITELN ( '(11)-ALLES---------- AUF BILDSCHIRM' ) ;
     WRITELN ( '(12)-ALLES---------- AUF    DRUCKER' ) ;
     WRITELN ;
     WRITELN ( 'AUSWAHL EINGEBEN:' ) ;
     IEIN ( WAHL ) ;
     if ( WAHL < 0 ) or ( WAHL > 12 ) then
       goto 6 ;
     case WAHL of
       0 : ;
       1 : begin
             CLRSCRN ;
             HAUSG ;
             READY
           end (* tag/ca *) ;
       2 : begin
             CLRSCRN ;
             SAUSG ;
             READY
           end (* tag/ca *) ;
       3 : begin
             CLRSCRN ;
             LAUSG ;
             READY
           end (* tag/ca *) ;
       4 : begin
             CLRSCRN ;
             BAUSG ;
             READY
           end (* tag/ca *) ;
       5 : begin
             CLRSCRN ;
             EAUSG ;
             READY
           end (* tag/ca *) ;
       6 : begin
             DRUCKEZEILE ;
             HAUSGDRUCK ;
             READY
           end (* tag/ca *) ;
       7 : begin
             DRUCKEZEILE ;
             SAUSGDRUCK ;
             READY
           end (* tag/ca *) ;
       8 : begin
             DRUCKEZEILE ;
             LAUSGDRUCK ;
             READY
           end (* tag/ca *) ;
       9 : begin
             DRUCKEZEILE ;
             BAUSGDRUCK ;
             READY
           end (* tag/ca *) ;
       10 : begin
              DRUCKEZEILE ;
              EAUSGDRUCK ;
              READY
            end (* tag/ca *) ;
       11 : begin
              CLRSCRN ;
              HAUSG ;
              WAIT ;
              CLRSCRN ;
              SAUSG ;
              WAIT ;
              CLRSCRN ;
              LAUSG ;
              WAIT ;
              CLRSCRN ;
              BAUSG ;
              WAIT ;
              CLRSCRN ;
              EAUSG ;
              WAIT ;
              CLRSCRN
            end (* tag/ca *) ;
       12 : begin
              HAUSGDRUCK ;
              SAUSGDRUCK ;
              LAUSGDRUCK ;
              BAUSGDRUCK ;
              EAUSGDRUCK ;
              READY
            end (* tag/ca *)
     end (* case *) ;
     if WAHL <> 0 then
       goto 6
   end (* LST *) ;



procedure AENDERN ;

(*********************************************)
(*   PARAMETER VON SYSTEMELEMENTEN AENDERN   *)
(*********************************************)


   var WAHL : INTEGER ;

   begin (* AENDERN *)
     WAHL := 1 ;
     while WAHL <> 0 do
       begin
         repeat
           CLRSCRN ;
           WRITELN ( 'MODELLPARAMETER AENDERN:' ) ;
           WRITELN ;
           WRITELN ( '(1)-HALTESTELLENKOORDINATEN/-TEXTE' ) ;
           WRITELN ( '(2)-EINBINDUNG (FUSSWEG)' ) ;
           WRITELN ( '(3)-LINIEN (BEDIENUNGSHAEUFIGKEIT)' ) ;
           WRITELN ( '(4)-LINIEN (FAHRTZEITEN)' ) ;
           WRITELN ;
           WRITELN ( 'AUSWAHL EINGEBEN:' ) ;
           IEIN ( WAHL )
         until ( WAHL >= 0 ) and ( WAHL < 5 ) ;
         case WAHL of
           0 : ;
           1 : begin
                 CLRSCRN ;
                 HKOORDAEND
               end (* tag/ca *) ;
           2 : begin
                 CLRSCRN ;
                 EINBFWAEND
               end (* tag/ca *) ;
           3 : begin
                 CLRSCRN ;
                 LBHAEND
               end (* tag/ca *) ;
           4 : begin
                 CLRSCRN ;
                 LFZAEND
               end (* tag/ca *)
         end (* case *)
       end (* while *)
   end (* AENDERN *) ;



procedure AMASKE ( var WAHL : INTEGER ) ;

(************************************************)
(*   GRUNDMENUE AUSGEBEN UND AUSWAHL EINLESEN   *)
(************************************************)


   label 4 ;

   begin (* AMASKE *)
     4 :
     CLRSCRN ;
     WRITELN ( 'TAETIGKEITSWAHL:' ) ;
     WRITELN ;
     WRITELN ( '(1)-HALTESTELLEN--------EINGEBEN' ) ;
     WRITELN ( '(2)-STRECKEN------------EINGEBEN' ) ;
     WRITELN ( '(3)-LINIEN--------------EINGEBEN' ) ;
     WRITELN ( '(4)-LINIEN--------------LOESCHEN' ) ;
     WRITELN ( '(5)-STRECKEN------------LOESCHEN' ) ;
     WRITELN ( '(6)-HALTESTELLEN--------LOESCHEN' ) ;
     WRITELN ( '(7)-BEZIRKE-------------EINGEBEN' ) ;
     WRITELN ( '(8)-EINBINDUNG----------EINGEBEN' ) ;
     WRITELN ( '(9)-EINBINDUNG----------LOESCHEN' ) ;
     WRITELN ( '(10)-BEZIRKE------------LOESCHEN' ) ;
     WRITELN ( '(11)-PARAMETER----------AENDERN' ) ;
     WRITELN ( '(12)--------------------LISTING' ) ;
     WRITELN ( '(13)--------------------UPDATE' ) ;
     WRITELN ;
     WRITELN ( 'AUSWAHL EINGEBEN:' ) ;
     IEIN ( WAHL ) ;
     if ( WAHL < 0 ) or ( WAHL > 13 ) then
       goto 4
   end (* AMASKE *) ;



procedure CONTROL ;

(*****************************************)
(*   GEWUENSCHTE TAETIGKEIT AUSFUEHREN   *)
(*****************************************)


   label 5 ;

   var WAHL : INTEGER ;
       FERTIG , WARNUNG : BOOLEAN ;

   begin (* CONTROL *)
     WARNUNG := FALSE ;
     5 :
     AMASKE ( WAHL ) ;
     if ( WAHL <> 0 ) and ( WAHL <= 11 ) then
       WARNUNG := TRUE ;
     case WAHL of
       0 : ;
       1 : begin
             CLRSCRN ;
             WRITELN ( 'HALTESTELLEN EINGEBEN:' ) ;
             WRITELN ;
             EDH
           end (* tag/ca *) ;
       2 : begin
             CLRSCRN ;
             WRITELN ( 'STRECKEN EINGEBEN:' ) ;
             WRITELN ;
             EDS
           end (* tag/ca *) ;
       3 : begin
             CLRSCRN ;
             WRITELN ( 'LINIEN EINGEBEN:' ) ;
             WRITELN ;
             EDL
           end (* tag/ca *) ;
       4 : begin
             CLRSCRN ;
             WRITELN ( 'LINIEN LOESCHEN:' ) ;
             WRITELN ;
             LOEL
           end (* tag/ca *) ;
       5 : begin
             CLRSCRN ;
             WRITELN ( 'STRECKEN LOESCHEN:' ) ;
             WRITELN ;
             LOES
           end (* tag/ca *) ;
       6 : begin
             CLRSCRN ;
             WRITELN ( 'HALTESTELLEN LOESCHEN:' ) ;
             WRITELN ;
             LOEH
           end (* tag/ca *) ;
       7 : begin
             CLRSCRN ;
             WRITELN ( 'BEZIRKE EINGEBEN:' ) ;
             WRITELN ;
             EDB
           end (* tag/ca *) ;
       8 : begin
             CLRSCRN ;
             WRITELN ( 'EINBINDUNGEN EINGEBEN:' ) ;
             WRITELN ;
             EDE
           end (* tag/ca *) ;
       9 : begin
             CLRSCRN ;
             WRITELN ( 'EINBINDUNGEN LOESCHEN:' ) ;
             WRITELN ;
             LOEE
           end (* tag/ca *) ;
       10 : begin
              CLRSCRN ;
              WRITELN ( 'BEZIRKE LOESCHEN:' ) ;
              WRITELN ;
              LOEB
            end (* tag/ca *) ;
       11 : AENDERN ;
       12 : LST ;
       13 : begin
              WARNUNG := FALSE ;
              UPD ;
              READY
            end (* tag/ca *) ;
     end (* case *) ;
     if WAHL <> 0 then
       goto 5 ;
     FERTIG := TRUE ;
     if WARNUNG then
       begin
         WRITELN ( 'ENDE OHNE UPDATE (J/N) ?' ) ;
         FERTIG := JNFRAGE ( TRUE )
       end (* then *) ;
     if not FERTIG then
       goto 5 ;
   end (* CONTROL *) ;



procedure KONSISTENZ ( var RICHTIG : BOOLEAN ) ;

(*****************************************************************)
(*                                                               *)
(*   KONSISTENZPRUEFUNG BEIM EINSTIEG IN DEN NETZEDITOR          *)
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
            WRITELN ( 'KONSISTENZPRUEFUNG:' ) ;
            WRITELN ;
            ERSTERFEHLER := FALSE
          end (* then *) ;
        if ( NR = 5 ) or ( NR = 6 ) then
          WRITE ( 'IN EINBINDUNG ZU' , BTRANSFER [ I ] : 6 , ' : ' ) ;
        if ( NR = 7 ) or ( NR = 8 ) then
          WRITE ( 'IN STRECKENN. ZU' , HTRANSFER [ I ] : 6 , ' : ' ) ;
        if ( NR >= 11 ) and ( NR <= 13 ) then
          WRITE ( 'IN LINIE' , LTRANSFER [ I ] : 6 , ' ' : 8 , ' : ' )
                  ;
        case NR of
          1 : WRITELN ( 'BEZIRKE NICHT EINDEUTIG ' ,
                        'ODER NICHT AUFSTEIGEND' ) ;
          2 : WRITELN ( 'BEZIRKE UEBER BZAHL UNGLEICH 0' ) ;
          3 : WRITELN ( 'HALTESTELLEN NICHT EINDEUTIG ' ,
                        'ODER NICHT AUFSTEIGEND' ) ;
          4 : WRITELN ( 'HALTESTELLEN UEBER HZAHL UNGLEICH 0' ) ;
          5 : WRITELN ( 'HALTESTELLE NICHT VORHANDEN' ) ;
          6 : WRITELN ( 'HALTESTELLE NICHT EINDEUTIG' ) ;
          7 : WRITELN ( 'ZIELHALTESTELLE NICHT VORHANDEN' ) ;
          8 : WRITELN ( 'ZIELHALTESTELLE NICHT EINDEUTIG' ) ;
          9 : WRITELN ( 'LINIEN NICHT EINDEUTIG ' ,
                        'ODER NICHT AUFSTEIGEND' ) ;
          10 : WRITELN ( 'LINIEN UEBER LZAHL UNGLEICH 0' ) ;
          11 : WRITELN ( 'RINGLISTE NICHT GESCHLOSSEN' ) ;
          12 : WRITELN ( 'HALTESTELLE NICHT VORHANDEN' ) ;
          13 : WRITELN ( 'TEILSTRECKE NICHT IN STRECKENNETZ' ) ;
          14 : WRITELN ( 'BEZIRK KLEINER 0 NICHT ERLAUBT' ) ;
          15 : WRITELN ( 'HALTESTELLE KLEINER 0 NICHT ERLAUBT' ) ;
          16 : WRITELN ( 'LINIE KLEINER 0 NICHT ERLAUBT' ) ;
        end (* case *) ;
        if NR = 11 then
          begin
            WRITELN ( 'ABBRUCH !' ) ;
            HALT
          end (* then *)
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
                 HTEST := EPTR -> . HSTELLE ;
                 SEARCHH ( HTEST , HIND , STATUS ) ;
                 if STATUS <> 1 then
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
                 HTEST := HPTR -> . HSTELLE ;
                 SEARCHH ( HTEST , HIND , STATUS ) ;
                 if STATUS <> 1 then
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
           HTEST := LPTR -> . HSTELLE ;
           SEARCHH ( HTEST , HIND , STATUS ) ;
           if STATUS <> 1 then
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
   end (* KONSISTENZ *) ;



procedure BELEGUNG ;

(****************************************************)
(*   AUSGABE INFORMATIONEN UEBER SPEICHERBELEGUNG   *)
(****************************************************)


   begin (* BELEGUNG *)
     WRITELN ( 'SPEICHERBELEGUNG:' ) ;
     WRITELN ;
     WRITE ( 'HALTESTELLEN:' ) ;
     WRITE ( HZAHL : 5 ) ;
     WRITE ( ' VON' ) ;
     WRITE ( HMAX : 5 ) ;
     WRITE ( '; ' ) ;
     WRITE ( HZAHL / HMAX * 100 : 6 : 2 ) ;
     WRITELN ( ' PROZENT' ) ;
     WRITE ( 'LINIEN:      ' ) ;
     WRITE ( LZAHL : 5 ) ;
     WRITE ( ' VON' ) ;
     WRITE ( LMAX : 5 ) ;
     WRITE ( '; ' ) ;
     WRITE ( LZAHL / LMAX * 100 : 6 : 2 ) ;
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
  TERMIN ( INPUT ) ;
  TERMOUT ( OUTPUT ) ;
  SEITENZAHL := 0 ;
  INIT ;
  WRITELN ;
  WRITELN ( 'INIT FERTIG, Modellname = ' , MODNAME ) ;
  WRITELN ;
  BELEGUNG ;
  KONSISTENZ ( RICHTIG ) ;
  if RICHTIG then
    WRITELN ( 'KONSISTENZPRUEFUNG FEHLERFREI' )
  else
    begin
      WRITELN ;
      WRITELN ( 'KONSISTENZPRUEFUNG FEHLERHAFT' ) ;
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
