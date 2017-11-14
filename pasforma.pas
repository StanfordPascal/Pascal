program PASFORM ( OUTPUT , EINGABE , AUSGABE , TRACE ) ;

(********************************************************************)
(*                                                                  *)
(*   PASCAL-FORMATIERER                                             *)
(*                                                                  *)
(*   STAND: JULI 1990                                               *)
(*   AUTOR: BERND OPPOLZER                                          *)
(*                                                                  *)
(*   UNTERSCHIEDE PASCAL/VS ZU TURBO/3:                             *)
(*                                                                  *)
(*   INSYMBOL MUSS AUSSER BLANKS BEI TURBO/3                        *)
(*   AUCH CHR(10) UND CHR(13) UEBERLESEN.                           *)
(*   TERMIN/TERMOUT BEI PASCAL/VS.                                  *)
(*   ASSIGN/CLOSE BEI TURBO/3.                                      *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*   ANPASSUNGEN FUER FPC AM 05.10.2013:                            *)
(*                                                                  *)
(*   ARRAY-KLAMMERN IMMER ALS [ ] AUSGEBEN                          *)
(*                                                                  *)
(*   POINTER-SYMBOLE IMMER ALS ^                                    *)
(*                                                                  *)
(*   CHARACTER-RANGE MIT RUECKSICHT AUF EBCDIC ...                  *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*   -- NEUE VERSION VON INSYMBOL, DIE AN STANFORD PASCAL           *)
(*      ANGEPASST IST                                               *)
(*                                                                  *)
(*   -- KOMMENTARE BESSER ABHANDELN WIE FOLGT:                      *)
(*                                                                  *)
(*   A) ZUNAECHST GIBT ES VIER SORTEN - ODER FUENF:                 *)
(*      1) # SIND ZU UEBERLESEN                                     *)
(*      2) SOLCHE MIT "..."                                         *)
(*      3) DANN DIE DREI UEBLICHEN                                  *)
(*                                                                  *)
(*   B) KOMMENTARE INNERHALB VON STATEMENTS BEHALTEN WIR BEI        *)
(*                                                                  *)
(*   C) KOMMENTARE AUSSERHALB KASTELN WIR EIN                       *)
(*                                                                  *)
(*   D) WICHTIG: AUCH SOLCHE, DIE UEBER MEHRERE ZEILEN GEHEN        *)
(*                                                                  *)
(*   E) REKURSIVE KOMMENTARE MUESSEN MOEGLICH SEIN                  *)
(*                                                                  *)
(*   F) ANDERE LOGIK BEI CASE                                       *)
(*                                                                  *)
(*   G) HINTER ZEILE 50 NICHT WEITER EINRUECKEN !!                  *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*   OFFEN:                                                         *)
(*                                                                  *)
(*   * WO BLEIBEN DIE LEERZEILEN HINTER DEN KOMMENTARKAESTEN        *)
(*                                                                  *)
(*   * ANDERS EINRUECKEN BEI VARIANTEN RECORDS                      *)
(*                                                                  *)
(*   * NICHT ALLE KOMMENTARE UEBERLESEN HINTER BEGIN/END USW.       *)
(*                                                                  *)
(*   * HINTER ZEILE 50 NICHT WEITER EINRUECKEN !!!                  *)
(*                                                                  *)
(*   * FEHLER BEI PASCAL2, WAS IST DA LOS ??                        *)
(*     KOMMENTAR NACH BEGIN WIRD UEBERLESEN, ABER DA WIRD           *)
(*     NUR DER ERSTE TEIL EINGELESEN; DER REST WIRD DANN            *)
(*     ALS NORMALES STATEMENT INTERPRETIERT ...                     *)
(*     KOMMC.UEBERLESEN USW. BESSER MACHEN ...                      *)
(*   * BEI KURZEN KOMMENTAREN STIMMEN DIE KAESTCHEN NICHT           *)
(*                                                                  *)
(*   * KLEINBUCHSTABEN AUCH KLEIN LASSEN                            *)
(*                                                                  *)
(*   * EINLESEN LAENGERE RECORDS, MINDESTENS 80, VERMUTLICH 120     *)
(*                                                                  *)
(*   * REKURSIVE KOMMENTARE, KOMMENTARKAESTEN UEBERHAUPT            *)
(*                                                                  *)
(*   * CONST FUER STRUKTUREN NACH VAR WIRD NICHT AKZEPTIERT         *)
(*                                                                  *)
(*   * KOMMENTARE VOLLSTAENDIG AUSGEBEN UND AUFTEILEN               *)
(*                                                                  *)
(*   * " KOMMENTARE MIT LAENGE KLEINER VIER KOMPLETT IGNORIEREN     *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*   ANPASSUNGEN am 31.08.2016                                      *)
(*                                                                  *)
(*   Kommentare anders eingerueckt                                  *)
(*   Neuzeile bei komplexen consts (Stanford-Erweiterung)           *)
(*                                                                  *)
(********************************************************************)



const VERKETT2 = '|' ;
      MAXLOUTPUT = 72 ;
      MAXLINPUT = 160 ;
      MAXEINR = 51 ;
      MAXIDSIZE = 20 ;


type WORT = array [ 1 .. 100 ] of CHAR ;
     SWORT = packed array [ 1 .. 10 ] of CHAR ;
     FELD6 = packed array [ 1 .. 6 ] of CHAR ;
     PWORT = record
               NAME : packed array [ 1 .. MAXIDSIZE ] of CHAR ;
               LAENGE : INTEGER
             end ;
     SYMBOL = ( EOFSY , IDENTIFIER , ZAHL , STRIPU , ZKETTE , SONDER ,
              POINTERSY , ECKKLAMMAUF , ECKKLAMMZU , GLEICH , ZUWEISUNG
              , KLAMMAUF , DOPU , KLAMMZU , ANDSY , ARRAYSY , BEGINSY ,
              BREAKSY , CASESY , CONSTSY , CONTINUESY , DOSY , ELSESY ,
              ENDSY , FORSY , FUNCTIONSY , GOTOSY , IFSY , INSY ,
              LABELSY , LOCALSY , MODULESY , NOTSY , OFSY , ORSY ,
              OTHERWISESY , OVERLAYSY , PACKEDSY , PROCEDURESY ,
              PROGRAMSY , RECORDSY , REPEATSY , RETURNSY , SETSY ,
              SQLBEGINSY , SQLENDSY , SQLVARSY , STATICSY , THENSY ,
              TOSY , TYPESY , UNTILSY , VARSY , WHILESY , WITHSY ,
              XORSY ) ;
     KOMMCTL = record
                 KOMMP : WORT ;
                 KOMML : INTEGER ;
                 ENDOFKOMM : BOOLEAN ;
                 ANZKOMM : INTEGER ;
                 ZUSTAND : INTEGER ;
                 KOMMTYPE : CHAR ;
                 NURSTERNE : BOOLEAN ;
                 KOMMSTATUS : INTEGER ;
                 UEBERLESEN : INTEGER ;
                 KOMM_VOR_PROC : BOOLEAN ;
               end ;


var EINGABE , AUSGABE : TEXT ;
    ZZAUS : INTEGER ;
    BLANKSYMBOLE : set of SYMBOL ;
    STERMSYMBOLE : set of SYMBOL ;
    TTERMSYMBOLE : set of SYMBOL ;
    WORTSYMBOLE : set of SYMBOL ;
    EXPRSYMBOLE : set of SYMBOL ;
    W1 : WORT ;
    INC : INTEGER ;
    DICHT : BOOLEAN ;
    COMPOUNDNZ : BOOLEAN ;
    BLANKSVORHANDEN : BOOLEAN ;
    NOMAJOR : BOOLEAN ;
    EINR : INTEGER ;
    EINRKOMM : INTEGER ;
    MODUS : INTEGER ;
    WTAB : array [ ANDSY .. XORSY ] of SWORT ;
    WTABSQL : array [ 1 .. 30 ] of SWORT ;
    ANZSQLWORTE : INTEGER ;
    W1ENDE , W1INDEX : INTEGER ;
    EOFILE : BOOLEAN ;
    SONDERZEICHEN , ISTARTSET , IWEITERSET , HEXZIFFERN , ZIFFERN : set
                                                   of CHAR ;
    S : SYMBOL ;
    OUTPOINTER : INTEGER ;
    INPOINTER : INTEGER ;
    NICHTLESEN : INTEGER ;
    CH1 , CH2 : CHAR ;
    ZIFFERNGELESEN : BOOLEAN ;
    NULLSTATE : BOOLEAN ;
    KOMMLAENGE : INTEGER ;
    ENDEKASTEN : CHAR ;
    ENDEKOMMEIN : INTEGER ;
    ZZAUSVOR : INTEGER ;
    INSQLSTATE : BOOLEAN ;
    SQLHOSTV : BOOLEAN ;
    TRACE : TEXT ;

    (*******************************************************)
    (*  KOMMC: AUFZEICHNEN DES LAUFENDEN KOMMENTAR-STATUS  *)
    (*******************************************************)

    KOMMC : KOMMCTL ;



procedure HALTX ;

   begin (* HALTX *)
     EXIT ( 8 )
   end (* HALTX *) ;



function MAJOR ( CH : CHAR ) : CHAR ;

   begin (* MAJOR *)
     if CH in [ 'a' .. 'i' , 'j' .. 'r' , 's' .. 'z' ] then
       MAJOR := CHR ( ORD ( CH ) - ORD ( 'a' ) + ORD ( 'A' ) )
     else
       MAJOR := CH
   end (* MAJOR *) ;



function MINOR ( CH : CHAR ) : CHAR ;

   begin (* MINOR *)
     if CH in [ 'A' .. 'I' , 'J' .. 'R' , 'S' .. 'Z' ] then
       MINOR := CHR ( ORD ( CH ) - ORD ( 'A' ) + ORD ( 'a' ) )
     else
       MINOR := CH
   end (* MINOR *) ;



procedure RESWRD ( var SYERG : SYMBOL ) ;

   var TESTWORT : SWORT ;
       I : INTEGER ;
       SY : SYMBOL ;
       WORTENDE : INTEGER ;

   begin (* RESWRD *)
     SYERG := IDENTIFIER ;
     TESTWORT := '' ;
     if W1ENDE <= 10 then
       WORTENDE := W1ENDE
     else
       WORTENDE := 10 ;
     for I := 1 to WORTENDE do
       TESTWORT [ I ] := W1 [ I ] ;
     for SY := ANDSY to XORSY do
       begin
         if TESTWORT = WTAB [ SY ] then
           SYERG := SY
       end (* for *)
   end (* RESWRD *) ;



function RESWRDSQL : BOOLEAN ;

   var TESTWORT : SWORT ;
       I : INTEGER ;
       RW : BOOLEAN ;
       WORTENDE : INTEGER ;

   begin (* RESWRDSQL *)
     RW := FALSE ;
     TESTWORT := '' ;
     if W1ENDE <= 10 then
       WORTENDE := W1ENDE
     else
       WORTENDE := 10 ;
     for I := 1 to WORTENDE do
       TESTWORT [ I ] := W1 [ I ] ;
     for I := 1 to ANZSQLWORTE do
       if TESTWORT = WTABSQL [ I ] then
         RW := TRUE ;
     RESWRDSQL := RW
   end (* RESWRDSQL *) ;



procedure READCH ( var CH : CHAR ) ;

   begin (* READCH *)
     EOFILE := FALSE ;
     if EOF ( EINGABE ) then
       begin
         EOFILE := TRUE ;
         CH := ' '
       end (* then *)
     else
       begin
         if EOLN ( EINGABE ) then
           INPOINTER := 0 ;
         if INPOINTER > MAXLINPUT then
           begin
             READLN ( EINGABE ) ;
             INPOINTER := 0 ;
             CH := ' '
           end (* then *)
         else
           begin
             if EOF ( EINGABE ) then
               begin
                 EOFILE := TRUE ;
                 CH := ' '
               end (* then *)
             else
               begin
                 INPOINTER := INPOINTER + 1 ;
                 READ ( EINGABE , CH )
               end (* else *)
           end (* else *)
       end (* else *) ;
     if not NOMAJOR then
       CH := MAJOR ( CH )
   end (* READCH *) ;



procedure LIES ( var CH : CHAR ) ;

   begin (* LIES *)
     if NICHTLESEN = 2 then
       begin
         CH := CH1 ;
         CH1 := CH2 ;
         NICHTLESEN := 1
       end (* then *)
     else
       if NICHTLESEN = 1 then
         begin
           CH := CH1 ;
           NICHTLESEN := 0
         end (* then *)
       else
         begin
           READCH ( CH ) ;
           if ZIFFERNGELESEN and ( CH = '.' ) then
             begin
               READCH ( CH2 ) ;
               if not ( CH2 in ZIFFERN ) then
                 begin
                   CH1 := CH ;
                   CH := ' ' ;
                   NICHTLESEN := 2
                 end (* then *)
               else
                 begin
                   CH1 := CH2 ;
                   NICHTLESEN := 1
                 end (* else *)
             end (* then *)
         end (* else *) ;
     W1INDEX := W1INDEX + 1 ;
     W1 [ W1INDEX ] := CH ;
     ZIFFERNGELESEN := ( CH in ZIFFERN ) ;
   end (* LIES *) ;



procedure LIESKOMM ( var CH : CHAR ) ;

   begin (* LIESKOMM *)
     READCH ( CH ) ;
     W1INDEX := W1INDEX + 1 ;
     W1 [ W1INDEX ] := CH ;
     ZIFFERNGELESEN := ( CH in ZIFFERN ) ;
   end (* LIESKOMM *) ;



procedure NEUZEILEKOMM ( BLANKS : BOOLEAN ) ;

   var EIN : INTEGER ;

   begin (* NEUZEILEKOMM *)
     EIN := EINRKOMM ;
     if EIN > MAXEINR then
       EIN := MAXEINR ;
     WRITELN ( AUSGABE ) ;
     ZZAUS := ZZAUS + 1 ;
     if BLANKS then
       begin
         if EIN > 0 then
           WRITE ( AUSGABE , ' ' : EIN ) ;
         OUTPOINTER := EIN
       end (* then *)
     else
       OUTPOINTER := 0 ;
     BLANKSVORHANDEN := FALSE ;
   end (* NEUZEILEKOMM *) ;



procedure KOMMENDEKASTEN ;

   var I : INTEGER ;
       KOMMTYPE : CHAR ;

   begin (* KOMMENDEKASTEN *)
     if ENDEKASTEN <> ' ' then
       begin
         KOMMTYPE := ENDEKASTEN ;
         if ENDEKOMMEIN <> 0 then
           WRITE ( AUSGABE , ' ' : ENDEKOMMEIN ) ;
         case KOMMTYPE of
           ')' : WRITE ( AUSGABE , '(*' ) ;
           '/' : WRITE ( AUSGABE , '/*' ) ;
           '}' : WRITE ( AUSGABE , '(*' ) ;
           '"' : WRITE ( AUSGABE , '"' ) ;
         end (* case *) ;
         for I := 1 to KOMMLAENGE - 4 do
           WRITE ( AUSGABE , '*' ) ;
         if KOMMTYPE = '"' then
           WRITE ( AUSGABE , '**' ) ;
         case KOMMTYPE of
           ')' : WRITE ( AUSGABE , '*)' ) ;
           '/' : WRITE ( AUSGABE , '*/' ) ;
           '}' : WRITE ( AUSGABE , '*)' ) ;
           '"' : WRITE ( AUSGABE , '"' ) ;
         end (* case *) ;
         WRITELN ( AUSGABE ) ;
         ZZAUS := ZZAUS + 1 ;
         ENDEKASTEN := ' '
       end (* then *) ;
   end (* KOMMENDEKASTEN *) ;



procedure READKOMM ( var KOMMC : KOMMCTL ) ;

   var CH : CHAR ;
       I : INTEGER ;

   begin (* READKOMM *)
     KOMMC . KOMML := 0 ;
     case KOMMC . KOMMTYPE of
       ')' : begin
               KOMMC . KOMMP [ 1 ] := '(' ;
               KOMMC . KOMMP [ 2 ] := '*' ;
               KOMMC . KOMML := 2 ;
             end (* tag/ca *) ;
       '/' : begin
               KOMMC . KOMMP [ 1 ] := '/' ;
               KOMMC . KOMMP [ 2 ] := '*' ;
               KOMMC . KOMML := 2 ;
             end (* tag/ca *) ;
       '}' : begin
               KOMMC . KOMMP [ 1 ] := '(' ;
               KOMMC . KOMMP [ 2 ] := '*' ;
               KOMMC . KOMML := 2 ;
             end (* tag/ca *) ;
       '"' : begin
               KOMMC . KOMMP [ 1 ] := '"' ;
               KOMMC . KOMML := 1 ;
             end (* tag/ca *) ;
     end (* case *) ;

     (************************************************)
     (*   HIER WERDEN DIE ERSTEN 100 ZEICHEN         *)
     (*   IN DEN KOMMPUFFER EINGETRAGEN              *)
     (************************************************)

     KOMMC . ENDOFKOMM := FALSE ;
     case KOMMC . KOMMTYPE of
       ')' : begin
               repeat
                 READCH ( CH ) ;
                 KOMMC . KOMML := KOMMC . KOMML + 1 ;
                 KOMMC . KOMMP [ KOMMC . KOMML ] := CH ;
                 case KOMMC . ZUSTAND of
                   1 : begin
                         if CH = '*' then
                           KOMMC . ZUSTAND := 2
                         else
                           if CH = '(' then
                             KOMMC . ZUSTAND := 3
                       end (* tag/ca *) ;
                   2 : begin
                         if CH = '*' then
                           KOMMC . ZUSTAND := 2
                         else
                           if CH = ')' then
                             begin
                               KOMMC . ANZKOMM := KOMMC . ANZKOMM - 1 ;
                               if KOMMC . ANZKOMM <= 0 then
                                 KOMMC . ENDOFKOMM := TRUE
                               else
                                 KOMMC . ZUSTAND := 1
                             end (* then *)
                           else
                             KOMMC . ZUSTAND := 1
                       end (* tag/ca *) ;
                   3 : begin
                         if CH = '*' then
                           begin
                             KOMMC . ZUSTAND := 1 ;
                             KOMMC . ANZKOMM := KOMMC . ANZKOMM + 1
                           end (* then *)
                         else
                           KOMMC . ZUSTAND := 1
                       end (* tag/ca *) ;
                 end (* case *)
               until ( KOMMC . KOMML >= 98 ) or KOMMC . ENDOFKOMM or
               EOLN ( EINGABE ) ;
             end (* tag/ca *) ;
       '/' : begin
               repeat
                 READCH ( CH ) ;
                 KOMMC . KOMML := KOMMC . KOMML + 1 ;
                 KOMMC . KOMMP [ KOMMC . KOMML ] := CH ;
                 case KOMMC . ZUSTAND of
                   1 : begin
                         if CH = '*' then
                           KOMMC . ZUSTAND := 2
                         else
                           if CH = '/' then
                             KOMMC . ZUSTAND := 3
                       end (* tag/ca *) ;
                   2 : begin
                         if CH = '*' then
                           KOMMC . ZUSTAND := 2
                         else
                           if CH = '/' then
                             begin
                               KOMMC . ANZKOMM := KOMMC . ANZKOMM - 1 ;
                               if KOMMC . ANZKOMM <= 0 then
                                 KOMMC . ENDOFKOMM := TRUE
                               else
                                 KOMMC . ZUSTAND := 1
                             end (* then *)
                           else
                             KOMMC . ZUSTAND := 1
                       end (* tag/ca *) ;
                   3 : begin
                         if CH = '*' then
                           begin
                             KOMMC . ZUSTAND := 1 ;
                             KOMMC . ANZKOMM := KOMMC . ANZKOMM + 1
                           end (* then *)
                         else
                           KOMMC . ZUSTAND := 1
                       end (* tag/ca *) ;
                 end (* case *)
               until ( KOMMC . KOMML >= 98 ) or KOMMC . ENDOFKOMM or
               EOLN ( EINGABE ) ;
             end (* tag/ca *) ;
       '}' : begin
               repeat
                 READCH ( CH ) ;
                 KOMMC . KOMML := KOMMC . KOMML + 1 ;
                 KOMMC . KOMMP [ KOMMC . KOMML ] := CH ;
                 if CH = '}' then
                   begin
                     KOMMC . ANZKOMM := KOMMC . ANZKOMM - 1 ;
                     if KOMMC . ANZKOMM <= 0 then
                       KOMMC . ENDOFKOMM := TRUE ;
                     KOMMC . KOMMP [ KOMMC . KOMML ] := '*' ;
                     KOMMC . KOMML := KOMMC . KOMML + 1 ;
                     KOMMC . KOMMP [ KOMMC . KOMML ] := ')' ;
                   end (* then *)
                 else
                   if CH = '{' then
                     KOMMC . ANZKOMM := KOMMC . ANZKOMM + 1
               until ( KOMMC . KOMML >= 98 ) or KOMMC . ENDOFKOMM or
               EOLN ( EINGABE ) ;
             end (* tag/ca *) ;
       '"' : begin
               repeat
                 READCH ( CH ) ;
                 KOMMC . KOMML := KOMMC . KOMML + 1 ;
                 KOMMC . KOMMP [ KOMMC . KOMML ] := CH ;
                 KOMMC . ENDOFKOMM := ( CH = '"' ) ;
               until ( KOMMC . KOMML >= 98 ) or KOMMC . ENDOFKOMM or
               EOLN ( EINGABE ) ;
             end (* tag/ca *) ;
     end (* case *) ;
     if KOMMC . ENDOFKOMM then
       begin
         KOMMC . NURSTERNE := TRUE ;
         for I := 2 to KOMMC . KOMML - 1 do
           if KOMMC . KOMMP [ I ] <> '*' then
             begin
               KOMMC . NURSTERNE := FALSE ;
               break
             end (* then *)
       end (* then *) ;

     (************************************************)
     (*   TRACEAUSGABE                               *)
     (************************************************)
     (*                                              *)
     (*      WRITELN ( TRACE , KOMMC . KOMML : 4 ,   *)
     (*                KOMMC . ENDOFKOMM ,           *)
     (*                KOMMC . ANZKOMM : 4 ,         *)
     (*                KOMMC . ZUSTAND : 4 , ' ' ,   *)
     (*                KOMMC . KOMMTYPE , ' ' ,      *)
     (*                KOMMC . KOMMP ) ;             *)
     (*      WRITELN ( TRACE ,                       *)
     (*                'KOMMSTATUS NACH READKOMM = ' *)
     (*                , KOMMC . KOMMSTATUS ) ;      *)
     (*                                              *)
     (************************************************)
     (*   W1INDEX AUF NULL, WEIL NICHTS RELEVANTES   *)
     (*   GELESEN WURDE                              *)
     (************************************************)

     W1INDEX := 0 ;

     (************************************************)
     (*   MUSS MAN MACHEN, WEIL READCH VERWENDET     *)
     (*   WIRD                                       *)
     (************************************************)

   end (* READKOMM *) ;



procedure WRITEKOMM ( var KOMMC : KOMMCTL ; KOMMLAENGE : INTEGER ) ;

   var I : INTEGER ;
       X : INTEGER ;
       Y : INTEGER ;
       CH1 : CHAR ;
       CH2 : CHAR ;
       CH3 : CHAR ;
       FILLCH : CHAR ;

   begin (* WRITEKOMM *)
     while TRUE do
       begin
         if KOMMC . KOMML < KOMMLAENGE then
           begin
             if KOMMC . NURSTERNE then
               FILLCH := '*'
             else
               FILLCH := ' ' ;
             for I := KOMMC . KOMML + 1 to KOMMLAENGE do
               KOMMC . KOMMP [ I ] := FILLCH ;
             X := KOMMLAENGE ;
             case KOMMC . KOMMTYPE of
               ')' : begin
                       if KOMMC . ENDOFKOMM then
                         begin
                           KOMMC . KOMMP [ KOMMC . KOMML ] := FILLCH ;
                           KOMMC . KOMMP [ KOMMC . KOMML - 1 ] :=
                                                   FILLCH
                         end (* then *) ;
                       KOMMC . KOMMP [ X ] := ')' ;
                       KOMMC . KOMMP [ X - 1 ] := '*' ;
                     end (* tag/ca *) ;
               '/' : begin
                       if KOMMC . ENDOFKOMM then
                         begin
                           KOMMC . KOMMP [ KOMMC . KOMML ] := FILLCH ;
                           KOMMC . KOMMP [ KOMMC . KOMML - 1 ] :=
                                                   FILLCH
                         end (* then *) ;
                       KOMMC . KOMMP [ X ] := '/' ;
                       KOMMC . KOMMP [ X - 1 ] := '*' ;
                     end (* tag/ca *) ;
               '}' : begin
                       if KOMMC . ENDOFKOMM then
                         begin
                           KOMMC . KOMMP [ KOMMC . KOMML ] := FILLCH ;
                           KOMMC . KOMMP [ KOMMC . KOMML - 1 ] :=
                                                   FILLCH
                         end (* then *) ;
                       KOMMC . KOMMP [ X ] := ')' ;
                       KOMMC . KOMMP [ X - 1 ] := '*' ;
                     end (* tag/ca *) ;
               '"' : begin
                       if KOMMC . ENDOFKOMM then
                         begin
                           KOMMC . KOMMP [ KOMMC . KOMML ] := FILLCH
                         end (* then *) ;
                       KOMMC . KOMMP [ X ] := '"' ;
                     end (* tag/ca *) ;
             end (* case *) ;
           end (* then *) ;
         if KOMMC . KOMML > KOMMLAENGE then
           begin
             case KOMMC . KOMMTYPE of
               ')' : begin
                       X := KOMMLAENGE - 2 ;
                       CH1 := KOMMC . KOMMP [ X ] ;
                       CH2 := KOMMC . KOMMP [ X + 1 ] ;
                       CH3 := KOMMC . KOMMP [ X + 2 ] ;
                       KOMMC . KOMMP [ X ] := ' ' ;
                       KOMMC . KOMMP [ X + 1 ] := '*' ;
                       KOMMC . KOMMP [ X + 2 ] := ')' ;
                     end (* tag/ca *) ;
               '/' : begin
                       X := KOMMLAENGE - 2 ;
                       CH1 := KOMMC . KOMMP [ X ] ;
                       CH2 := KOMMC . KOMMP [ X + 1 ] ;
                       CH3 := KOMMC . KOMMP [ X + 2 ] ;
                       KOMMC . KOMMP [ X ] := ' ' ;
                       KOMMC . KOMMP [ X + 1 ] := '*' ;
                       KOMMC . KOMMP [ X + 2 ] := '/' ;
                     end (* tag/ca *) ;
               '}' : begin
                       X := KOMMLAENGE - 2 ;
                       CH1 := KOMMC . KOMMP [ X ] ;
                       CH2 := KOMMC . KOMMP [ X + 1 ] ;
                       CH3 := KOMMC . KOMMP [ X + 2 ] ;
                       KOMMC . KOMMP [ X ] := ' ' ;
                       KOMMC . KOMMP [ X + 1 ] := '*' ;
                       KOMMC . KOMMP [ X + 2 ] := ')' ;
                     end (* tag/ca *) ;
               '"' : begin
                       X := KOMMLAENGE ;
                       CH1 := KOMMC . KOMMP [ X ] ;
                       KOMMC . KOMMP [ X ] := '"' ;
                     end (* tag/ca *) ;
             end (* case *) ;
           end (* then *) ;

     /***************************************/
     /* wg. transport von sourcen zum MVS   */
     /* via iebgener kommentare ab spalte 1 */
     /* nicht mit schraegstrich - stern     */
     /***************************************/

         if KOMMC . KOMMTYPE = '/' then
           if EINRKOMM = 0 then
             begin
               KOMMC . KOMMP [ 1 ] := '(' ;
               KOMMC . KOMMP [ KOMMLAENGE ] := ')' ;
             end (* then *) ;
         for I := 1 to KOMMLAENGE do
           WRITE ( AUSGABE , KOMMC . KOMMP [ I ] ) ;
         if KOMMC . KOMML > KOMMLAENGE then
           begin
             NEUZEILEKOMM ( TRUE ) ;
             case KOMMC . KOMMTYPE of
               ')' : begin
                       KOMMC . KOMMP [ 1 ] := '(' ;
                       KOMMC . KOMMP [ 2 ] := '*' ;
                       KOMMC . KOMMP [ 3 ] := CH1 ;
                       KOMMC . KOMMP [ 4 ] := CH2 ;
                       KOMMC . KOMMP [ 5 ] := CH3 ;
                       X := 5 ;
                     end (* tag/ca *) ;
               '/' : begin
                       KOMMC . KOMMP [ 1 ] := '/' ;
                       KOMMC . KOMMP [ 2 ] := '*' ;
                       KOMMC . KOMMP [ 3 ] := CH1 ;
                       KOMMC . KOMMP [ 4 ] := CH2 ;
                       KOMMC . KOMMP [ 5 ] := CH3 ;
                       X := 5 ;
                     end (* tag/ca *) ;
               '}' : begin
                       KOMMC . KOMMP [ 1 ] := '(' ;
                       KOMMC . KOMMP [ 2 ] := '*' ;
                       KOMMC . KOMMP [ 3 ] := CH1 ;
                       KOMMC . KOMMP [ 4 ] := CH2 ;
                       KOMMC . KOMMP [ 5 ] := CH3 ;
                       X := 5 ;
                     end (* tag/ca *) ;
               '"' : begin
                       KOMMC . KOMMP [ 1 ] := '"' ;
                       KOMMC . KOMMP [ 2 ] := CH1 ;
                       X := 2 ;
                     end (* tag/ca *) ;
             end (* case *) ;
             Y := KOMMLAENGE + 1 ;
             for Y := KOMMLAENGE + 1 to KOMMC . KOMML do
               begin
                 X := X + 1 ;
                 KOMMC . KOMMP [ X ] := KOMMC . KOMMP [ Y ] ;
               end (* for *) ;
             KOMMC . KOMML := X ;
             continue ;
           end (* then *) ;
         if KOMMC . ENDOFKOMM then
           break ;
         NEUZEILEKOMM ( TRUE ) ;
         READKOMM ( KOMMC ) ;
       end (* while *)
   end (* WRITEKOMM *) ;



procedure KOMMENTAR ( var KOMMC : KOMMCTL ) ;

(********************************************************************)
(*                                                                  *)
(*   OKTOBER 2011: KOMMENTARLOGIK NEU - ZUSTANDSGETRIEBEN           *)
(*                                                                  *)
(*   KOMMKASTEN: BOOLEAN - LEGT FEST, OB WIR AKTUELL KASTEN         *)
(*   WOLLEN ODER NICHT                                              *)
(*                                                                  *)
(*   KOMMZUSTAND: ZUSTAND DER KOMMENTARBEARBEITUNG                  *)
(*                                                                  *)
(*   0 = AUSSERHALB VON KOMMENTAREN                                 *)
(*   1 = INNERHALB KOMMENTARKASTEN-BEARBEITUNG                      *)
(*   2 = NOCH UNKLAR                                                *)
(*                                                                  *)
(*   EINRKOMM: POSITION, AB DER KOMMENTAR BEGINNT                   *)
(*                                                                  *)
(*   ZUNAECHST EINLESEN DES KOMMENTARS ODER DER ERSTEN              *)
(*   100 ZEICHEN IN DEN BUFFER KOMMC.KOMMP                          *)
(*   WENN SICH DANN HERAUSSTELLT, DASS ES BEREITS EIN               *)
(*   KOMMENTARKASTEN-ANFANG IST, DESSEN LAENGE FESTHALTEN           *)
(*   UND DEN ZUSTAND BEIBEHALTEN                                    *)
(*                                                                  *)
(*   WENN NICHT: KOMMENTAR ENTSPRECHEND STUECKELN;                  *)
(*   MAXIMALE LAENGE ERGIBT SICH AUS EINRKOMM                       *)
(*                                                                  *)
(********************************************************************)


   var I : INTEGER ;
       DICHTSAVE : BOOLEAN ;
       KASTENEINFUEGEN : BOOLEAN ;

   begin (* KOMMENTAR *)
     KASTENEINFUEGEN := FALSE ;
     KOMMC . ENDOFKOMM := FALSE ;
     KOMMC . ANZKOMM := 1 ;
     KOMMC . ZUSTAND := 1 ;
     KOMMC . NURSTERNE := FALSE ;
     READKOMM ( KOMMC ) ;

     (************************************************)
     (*   ABKLAEREN, OB KOMMENTAR UEBERLESEN WERDEN  *)
     (*   MUSS                                       *)
     (************************************************)

     DICHTSAVE := DICHT ;
     if KOMMC . UEBERLESEN <> 0 then
       DICHT := TRUE ;
     if KOMMC . KOMMTYPE = '"' then
       if KOMMC . KOMML <= 4 then
         DICHT := TRUE ;

     (************************************************)
     (*   DICHT = KOMMENTAR UEBERLESEN               *)
     (************************************************)

     if DICHT then
       begin
         while not KOMMC . ENDOFKOMM do
           READKOMM ( KOMMC )
       end (* then *)
     else
       begin
         if KOMMC . KOMM_VOR_PROC then
           begin
             NEUZEILEKOMM ( FALSE ) ;
             NEUZEILEKOMM ( FALSE ) ;
             NEUZEILEKOMM ( FALSE ) ;
             NEUZEILEKOMM ( FALSE ) ;
             KOMMC . KOMM_VOR_PROC := FALSE ;
           end (* then *) ;
         case KOMMC . KOMMSTATUS of
           0 : begin
                 if MODUS > 2 then
                   begin
                     NEUZEILEKOMM ( FALSE ) ;
                     NEUZEILEKOMM ( TRUE ) ;
                   end (* then *) ;
                 if MODUS = 2 then
                   NEUZEILEKOMM ( TRUE ) ;

     (************************************************)
     (*   SCHAUEN, OB KASTEN EINGEFUEGT WERDEN MUSS  *)
     (************************************************)

                 KASTENEINFUEGEN := not KOMMC . NURSTERNE ;

     (************************************************)
     (*   WENN KOMMENTAR NICHT PASST, WIRD AUF       *)
     (*   JEDEN FALL EIN KASTEN EINGEFUEGT           *)
     (************************************************)

                 if KOMMC . KOMML > 72 - EINRKOMM then
                   KASTENEINFUEGEN := TRUE ;

     (************************************************)
     (*   NEUER KOMMC.KOMMSTATUS ABH. VOM KASTEN     *)
     (************************************************)

                 if KASTENEINFUEGEN then
                   KOMMC . KOMMSTATUS := 1
                 else
                   KOMMC . KOMMSTATUS := 2 ;

     (************************************************)
     (*   KOMMLAENGE FESTLEGEN                       *)
     (************************************************)

                 KOMMLAENGE := KOMMC . KOMML ;
                 if not KOMMC . ENDOFKOMM then
                   KOMMLAENGE := KOMMLAENGE + 3 ;
                 if KOMMLAENGE < 10 then
                   KOMMLAENGE := 10 ;
                 if KOMMC . KOMML > 72 - EINRKOMM then
                   KOMMLAENGE := 72 - EINRKOMM ;
               end (* tag/ca *) ;
           1 : begin
                 if MODUS >= 2 then
                   NEUZEILEKOMM ( TRUE ) ;
               end (* tag/ca *) ;
           2 : begin
                 if MODUS >= 2 then
                   NEUZEILEKOMM ( TRUE ) ;
               end (* tag/ca *) ;
         end (* case *) ;

     (************************************************)
     (*   AUSGEBEN DES KOMMENTARS                    *)
     (************************************************)

         if KASTENEINFUEGEN then
           begin
             ENDEKASTEN := KOMMC . KOMMTYPE ;
             ENDEKOMMEIN := EINRKOMM ;
             case KOMMC . KOMMTYPE of
               ')' : WRITE ( AUSGABE , '(*' ) ;
               '/' : WRITE ( AUSGABE , '/*' ) ;
               '}' : WRITE ( AUSGABE , '(*' ) ;
               '"' : WRITE ( AUSGABE , '"' ) ;
             end (* case *) ;
             for I := 1 to KOMMLAENGE - 4 do
               WRITE ( AUSGABE , '*' ) ;
             if KOMMC . KOMMTYPE = '"' then
               WRITE ( AUSGABE , '**' ) ;
             case KOMMC . KOMMTYPE of
               ')' : WRITE ( AUSGABE , '*)' ) ;
               '/' : WRITE ( AUSGABE , '*/' ) ;
               '}' : WRITE ( AUSGABE , '*)' ) ;
               '"' : WRITE ( AUSGABE , '"' ) ;
             end (* case *) ;
             NEUZEILEKOMM ( TRUE ) ;
           end (* then *) ;
         WRITEKOMM ( KOMMC , KOMMLAENGE ) ;
       end (* else *) ;
     DICHT := DICHTSAVE ;
   end (* KOMMENTAR *) ;



procedure INSYMBOL ( var S : SYMBOL ) ;

   var CH : CHAR ;
       KOMMENTFOUND : BOOLEAN ;
       ZIFFERNLESEN : BOOLEAN ;

   begin (* INSYMBOL *)
     NULLSTATE := FALSE ;
     repeat
       KOMMENTFOUND := FALSE ;
       if W1INDEX = 0 then
         LIES ( CH )
       else
         begin
           W1INDEX := W1ENDE + 1 ;
           W1 [ 1 ] := W1 [ W1INDEX ] ;
           CH := W1 [ W1INDEX ] ;
           W1INDEX := 1
         end (* else *) ;

     (************************************************)
     (*   BLANKS USW. UEBERLESEN                     *)
     (************************************************)

       while ( ( CH = ' ' ) or ( CH = CHR ( 10 ) ) or ( CH = CHR ( 13 )
       ) or ( CH = '#' ) ) and not EOFILE do
         begin

     (************************************************)
     (*   NACH EINER LEERZEILE KOMMENTARE            *)
     (*   NICHT MEHR UEBERLESEN                      *)
     (************************************************)

           if KOMMC . UEBERLESEN = 2 then
             if INPOINTER = 1 then
               KOMMC . UEBERLESEN := 0 ;
           W1INDEX := 0 ;
           LIES ( CH )
         end (* while *) ;
       if CH = ' ' then
         S := EOFSY
       else
         if CH in ISTARTSET then
           begin
             S := IDENTIFIER ;
             if CH in [ 'B' , 'X' ] then
               begin
                 LIES ( CH ) ;
                 if CH = '''' then
                   begin
                     repeat
                       NOMAJOR := TRUE ;
                       repeat
                         LIESKOMM ( CH )
                       until CH = '''' ;
                       NOMAJOR := FALSE ;
                       LIES ( CH )
                     until CH <> '''' ;
                     S := ZKETTE
                   end (* then *)
               end (* then *)
             else
               LIES ( CH ) ;
             if S = IDENTIFIER then
               begin
                 while CH in IWEITERSET do
                   LIES ( CH ) ;
                 W1ENDE := W1INDEX - 1 ;
                 RESWRD ( S )
               end (* then *)
           end (* then *)
         else
           if CH in ( ZIFFERN + [ '#' ] ) then
             begin

     (************************************************)
     (*   auch 0x... hex darstellung zulassen        *)
     (*   Exponent E gross oder klein schreiben      *)
     (************************************************)

               repeat
                 ZIFFERNLESEN := TRUE ;
                 if CH = '0' then
                   begin
                     LIES ( CH ) ;
                     if not ( CH in ( ZIFFERN + [ 'e' , 'E' , '.' , 'X'
                     , 'x' ] ) ) then
                       break ;
                     if CH in [ 'X' , 'x' ] then
                       begin
                         repeat
                           LIES ( CH )
                         until not ( CH in ( ZIFFERN + [ 'A' .. 'F' ,
                         'a' .. 'f' ] ) ) ;
                         break ;
                       end (* then *) ;
                     if CH in [ 'e' , 'E' , '.' ] then
                       ZIFFERNLESEN := FALSE ;
                   end (* then *) ;
                 if ZIFFERNLESEN then
                   repeat
                     LIES ( CH )
                   until not ( CH in ZIFFERN ) ;
                 if CH = '.' then
                   repeat
                     LIES ( CH )
                   until not ( CH in ZIFFERN ) ;
                 if ( CH = 'E' ) or ( CH = 'e' ) then
                   begin
                     LIES ( CH ) ;
                     if ( CH = '+' ) or ( CH = '-' ) then
                       LIES ( CH ) ;
                     repeat
                       LIES ( CH )
                     until not ( CH in ZIFFERN )
                   end (* then *)
               until TRUE ;
               S := ZAHL
             end (* then *)
           else
             if CH = '$' then
               begin
                 repeat
                   LIES ( CH )
                 until not ( CH in HEXZIFFERN ) ;
                 S := ZAHL
               end (* then *)
             else
               if CH = '''' then
                 begin
                   repeat
                     NOMAJOR := TRUE ;
                     repeat
                       LIESKOMM ( CH )
                     until CH = '''' ;
                     NOMAJOR := FALSE ;
                     LIES ( CH )
                   until CH <> '''' ;
                   S := ZKETTE
                 end (* then *)
               else
                 begin
                   if CH = '{' then
                     begin
                       NOMAJOR := TRUE ;
                       KOMMC . KOMMTYPE := '}' ;
                       KOMMENTAR ( KOMMC ) ;
                       NOMAJOR := FALSE ;
                       KOMMENTFOUND := TRUE
                     end (* then *)
                   else
                     if CH = '"' then
                       begin
                         NOMAJOR := TRUE ;
                         KOMMC . KOMMTYPE := '"' ;
                         KOMMENTAR ( KOMMC ) ;
                         NOMAJOR := FALSE ;
                         KOMMENTFOUND := TRUE
                       end (* then *)
                     else
                       if CH = '(' then
                         begin
                           LIES ( CH ) ;
                           if CH = '*' then
                             begin
                               NOMAJOR := TRUE ;
                               KOMMC . KOMMTYPE := ')' ;
                               KOMMENTAR ( KOMMC ) ;
                               NOMAJOR := FALSE ;
                               KOMMENTFOUND := TRUE
                             end (* then *)
                           else
                             begin
                               S := KLAMMAUF ;
                               if ( CH = '.' ) or ( CH = '/' ) then
                                 begin
                                   S := ECKKLAMMAUF ;
                                   LIES ( CH ) ;
                                 end (* then *)
                             end (* else *)
                         end (* then *)
                       else
                         if CH = '/' then
                           begin
                             LIES ( CH ) ;
                             if CH = '*' then
                               begin
                                 NOMAJOR := TRUE ;
                                 KOMMC . KOMMTYPE := '/' ;
                                 KOMMENTAR ( KOMMC ) ;
                                 NOMAJOR := FALSE ;
                                 KOMMENTFOUND := TRUE
                               end (* then *)
                             else
                               begin
                                 S := SONDER ;
                                 if CH = ')' then
                                   begin
                                     S := ECKKLAMMZU ;
                                     LIES ( CH ) ;
                                   end (* then *)
                               end (* else *)
                           end (* then *)
                         else
                           begin
                             S := SONDER ;
                             if ( CH in SONDERZEICHEN ) then
                               case CH of
                                 '@' : begin
                                         S := POINTERSY ;
                                         LIES ( CH )
                                       end (* tag/ca *) ;
                                 '[' : begin
                                         S := ECKKLAMMAUF ;
                                         LIES ( CH )
                                       end (* tag/ca *) ;
                                 ']' : begin
                                         S := ECKKLAMMZU ;
                                         LIES ( CH )
                                       end (* tag/ca *) ;
                                 '^' : begin
                                         S := POINTERSY ;
                                         LIES ( CH )
                                       end (* tag/ca *) ;
                                 '*' : begin
                                         LIES ( CH )
                                       end (* tag/ca *) ;
                                 ')' : begin
                                         LIES ( CH ) ;
                                         S := KLAMMZU
                                       end (* tag/ca *) ;
                                 '-' : begin
                                         LIES ( CH ) ;
                                         if CH = '>' then
                                           begin
                                             S := POINTERSY ;
                                             LIES ( CH )
                                           end (* then *)
                                       end (* tag/ca *) ;
                                 '+' : begin
                                         LIES ( CH )
                                       end (* tag/ca *) ;
                                 '=' : begin
                                         LIES ( CH ) ;
                                         S := GLEICH
                                       end (* tag/ca *) ;
                                 ':' : begin
                                         LIES ( CH ) ;
                                         S := DOPU ;
                                         if CH = '=' then
                                           begin
                                             S := ZUWEISUNG ;
                                             LIES ( CH )
                                           end (* then *)
                                       end (* tag/ca *) ;
                                 ';' : begin
                                         LIES ( CH ) ;
                                         S := STRIPU
                                       end (* tag/ca *) ;
                                 ',' : begin
                                         LIES ( CH )
                                       end (* tag/ca *) ;
                                 '.' : begin
                                         LIES ( CH ) ;
                                         if CH = '.' then
                                           LIES ( CH )
                                         else
                                           if CH = ')' then
                                             begin
                                               S := ECKKLAMMZU ;
                                               LIES ( CH )
                                             end (* then *)
                                       end (* tag/ca *) ;
                                 '>' : begin
                                         LIES ( CH ) ;
                                         if CH = '=' then
                                           LIES ( CH )
                                       end (* tag/ca *) ;
                                 '<' : begin
                                         LIES ( CH ) ;
                                         if CH = '=' then
                                           LIES ( CH )
                                         else
                                           if CH = '>' then
                                             LIES ( CH )
                                       end (* tag/ca *) ;
                                 '!' : begin
                                         LIES ( CH ) ;
                                         if CH = '!' then
                                           LIES ( CH )
                                       end (* tag/ca *) ;
                                 VERKETT2 :
                                   begin
                                     LIES ( CH ) ;
                                     if CH = VERKETT2 then
                                       LIES ( CH )
                                   end (* tag/ca *) ;
                                 '%' : begin
                                         if not DICHT then
                                           if KOMMC . KOMMSTATUS = 1
                                           then
                                             begin
                                               NEUZEILEKOMM ( FALSE ) ;
                                               KOMMENDEKASTEN ;
                                               KOMMC . KOMMSTATUS := 0
                                                   ;
                                               if MODUS > 2 then
                                                 NEUZEILEKOMM ( FALSE )
                                                   ;
                                             end (* then *)
                                           else
                                             begin
                                               NEUZEILEKOMM ( FALSE ) ;
                                               if MODUS > 2 then
                                                 NEUZEILEKOMM ( FALSE )
                                                   ;
                                             end (* else *)
                                         else
                                           NEUZEILEKOMM ( FALSE ) ;
                                         WRITE ( AUSGABE , CH ) ;
                                         NOMAJOR := TRUE ;
                                         repeat
                                           READCH ( CH ) ;
                                           WRITE ( AUSGABE , CH ) ;
                                         until EOLN ( EINGABE ) or (
                                         INPOINTER = 0 ) ;
                                         NOMAJOR := FALSE ;
                                         OUTPOINTER := MAXLOUTPUT + 1 ;
                                         KOMMENTFOUND := TRUE ;
                                         W1INDEX := 0
                                       end (* tag/ca *)
                               end (* case *)
                             else
                               LIES ( CH )
                           end (* else *)
                 end (* else *)
     until not KOMMENTFOUND ;
     KOMMC . UEBERLESEN := 0 ;
     W1ENDE := W1INDEX - 1 ;
   end (* INSYMBOL *) ;



procedure NEUZEILE ( BLANKS : BOOLEAN ) ;

   var EIN : INTEGER ;

   begin (* NEUZEILE *)
     EIN := EINR ;
     if EIN > MAXEINR then
       EIN := MAXEINR ;
     if not DICHT then
       begin
         if KOMMC . KOMMSTATUS = 1 then
           begin
             NEUZEILEKOMM ( FALSE ) ;
             KOMMENDEKASTEN ;
           end (* then *)
         else
           if KOMMC . KOMMSTATUS = 2 then
             begin
               WRITELN ( AUSGABE ) ;
               ZZAUS := ZZAUS + 1 ;
             end (* then *) ;
       end (* then *) ;
     KOMMC . KOMMSTATUS := 0 ;
     WRITELN ( AUSGABE ) ;
     ZZAUS := ZZAUS + 1 ;
     if BLANKS then
       begin
         if EIN > 0 then
           WRITE ( AUSGABE , ' ' : EIN ) ;
         OUTPOINTER := EIN
       end (* then *)
     else
       OUTPOINTER := 0 ;
     BLANKSVORHANDEN := FALSE ;
   end (* NEUZEILE *) ;



procedure PLATZZEILE ( PLATZBEDARF : INTEGER ) ;

   var EIN : INTEGER ;

   begin (* PLATZZEILE *)
     EIN := EINR ;
     if EIN > MAXEINR then
       EIN := MAXEINR ;
     if PLATZBEDARF > MAXLOUTPUT then
       begin
         WRITELN ( '+++ Zkette passt nicht in Zeile' ) ;
         HALTX
       end (* then *)
     else
       if EIN + PLATZBEDARF < MAXLOUTPUT then
         NEUZEILE ( TRUE )
       else
         begin
           WRITELN ( AUSGABE ) ;
           ZZAUS := ZZAUS + 1 ;
           if MAXLOUTPUT - PLATZBEDARF - 1 > 0 then
             WRITE ( AUSGABE , ' ' : MAXLOUTPUT - PLATZBEDARF - 1 ) ;
           OUTPOINTER := MAXLOUTPUT - PLATZBEDARF - 1 ;
           BLANKSVORHANDEN := FALSE ;
         end (* else *)
   end (* PLATZZEILE *) ;



procedure OUTSYMBOL ( S : SYMBOL ) ;

   var I : INTEGER ;
       PLATZBEDARF : INTEGER ;
       CH : CHAR ;

   begin (* OUTSYMBOL *)
     if not DICHT then
       if KOMMC . KOMMSTATUS > 0 then
         begin
           if KOMMC . KOMMSTATUS = 1 then
             begin
               NEUZEILEKOMM ( FALSE ) ;
               KOMMENDEKASTEN ;
             end (* then *)
           else
             if KOMMC . KOMMSTATUS = 2 then
               begin
                 WRITELN ( AUSGABE ) ;
                 ZZAUS := ZZAUS + 1 ;
               end (* then *) ;
           if MODUS > 2 then
             NEUZEILE ( TRUE )
           else
             if not DICHT then
               WRITE ( AUSGABE , ' ' )
         end (* then *) ;
     KOMMC . KOMMSTATUS := 0 ;

     (*****************************)
     (* SYMBOLE GGF. MODIFIZIEREN *)
     (*****************************)

     if S in [ POINTERSY , ECKKLAMMAUF , ECKKLAMMZU ] then
       begin
         case S of
           POINTERSY :
             begin
               W1 [ 3 ] := W1 [ W1INDEX ] ;
               W1 [ 1 ] := '-' ;
               W1 [ 2 ] := '>' ;
               W1ENDE := 2 ;
             end (* tag/ca *) ;
           ECKKLAMMAUF :
             begin
               W1 [ 2 ] := W1 [ W1INDEX ] ;
               W1 [ 1 ] := '[' ;
               W1ENDE := 1 ;
             end (* tag/ca *) ;
           ECKKLAMMZU :
             begin
               W1 [ 2 ] := W1 [ W1INDEX ] ;
               W1 [ 1 ] := ']' ;
               W1ENDE := 1 ;
             end (* tag/ca *)
         end (* case *)
       end (* then *) ;
     if not DICHT then
       begin
         PLATZBEDARF := OUTPOINTER + W1ENDE + 1 ;
         if ( S = DOPU ) and INSQLSTATE then
           PLATZBEDARF := PLATZBEDARF + 8 ;
         if PLATZBEDARF > MAXLOUTPUT then
           PLATZZEILE ( PLATZBEDARF - OUTPOINTER ) ;
         if not INSQLSTATE then
           if S in WORTSYMBOLE then
             for I := 1 to W1ENDE do
               begin
                 CH := W1 [ I ] ;
                 CH := MINOR ( CH ) ;
                 W1 [ I ] := CH ;
               end (* for *) ;
         if ZZAUSVOR = ZZAUS then
           if not SQLHOSTV then
             WRITE ( AUSGABE , ' ' )
           else
             OUTPOINTER := OUTPOINTER - 1 ;
         ZZAUSVOR := ZZAUS ;
         for I := 1 to W1ENDE do
           WRITE ( AUSGABE , W1 [ I ] ) ;
         SQLHOSTV := ( S = DOPU ) and INSQLSTATE ;
         OUTPOINTER := OUTPOINTER + W1ENDE + 1
       end (* then *)
     else
       begin
         PLATZBEDARF := OUTPOINTER + W1ENDE ;
         if ( BLANKSVORHANDEN and ( S in BLANKSYMBOLE ) ) then
           PLATZBEDARF := PLATZBEDARF + 1 ;
         if PLATZBEDARF > MAXLOUTPUT then
           PLATZZEILE ( PLATZBEDARF - OUTPOINTER ) ;
         if ( BLANKSVORHANDEN and ( S in BLANKSYMBOLE ) ) then
           begin
             WRITE ( AUSGABE , ' ' ) ;
             OUTPOINTER := OUTPOINTER + 1
           end (* then *) ;
         if not INSQLSTATE then
           if S in WORTSYMBOLE then
             for I := 1 to W1ENDE do
               begin
                 CH := W1 [ I ] ;
                 CH := MINOR ( CH ) ;
                 W1 [ I ] := CH ;
               end (* for *) ;
         for I := 1 to W1ENDE do
           WRITE ( AUSGABE , W1 [ I ] ) ;
         OUTPOINTER := OUTPOINTER + W1ENDE ;
         BLANKSVORHANDEN := ( S in BLANKSYMBOLE )
       end (* else *)
   end (* OUTSYMBOL *) ;



procedure STRIPUTEST ;

   begin (* STRIPUTEST *)
     if S = STRIPU then
       begin
         OUTSYMBOL ( S ) ;
         repeat
           INSYMBOL ( S )
         until S <> STRIPU
       end (* then *)
   end (* STRIPUTEST *) ;



procedure KOMM_ZWISCHEN_PROCS ( BLOCKLEVEL : INTEGER ) ;

   var EINRKOMMSAVE : INTEGER ;

   begin (* KOMM_ZWISCHEN_PROCS *)
     EINRKOMMSAVE := EINRKOMM ;
     if BLOCKLEVEL <= 2 then
       begin
         KOMMC . KOMM_VOR_PROC := TRUE ;
         EINRKOMM := 0 ;
       end (* then *) ;
     if S = STRIPU then
       begin
         OUTSYMBOL ( S ) ;
         repeat
           INSYMBOL ( S ) ;
         until S <> STRIPU
       end (* then *) ;
     KOMMC . KOMM_VOR_PROC := FALSE ;
     EINRKOMM := EINRKOMMSAVE ;
   end (* KOMM_ZWISCHEN_PROCS *) ;



procedure SQLSTATE ;

   var EINRSAVE : INTEGER ;
       EINRZWEI : INTEGER ;

   begin (* SQLSTATE *)
     KOMMC . UEBERLESEN := 1 ;
     EINRSAVE := EINR ;
     EINR := EINR + 9 ;
     EINRZWEI := EINR ;
     OUTSYMBOL ( S ) ;
     INSQLSTATE := TRUE ;
     INSYMBOL ( S ) ;
     EINR := EINR + W1ENDE + 1 ;
     OUTSYMBOL ( S ) ;
     INSYMBOL ( S ) ;
     repeat
       if RESWRDSQL then
         begin

     (************************************************)
     (*   W1ENDE ENTHAELT DIE ANZAHL DER ZEICHEN     *)
     (*   DES ZULETZT GELESENEN WORTES.              *)
     (************************************************)

           EINR := EINRZWEI ;
           NEUZEILE ( TRUE ) ;
           EINR := EINR + W1ENDE + 1
         end (* then *) ;
       OUTSYMBOL ( S ) ;
       INSYMBOL ( S ) ;
       if S = EOFSY then
         begin
           WRITELN ( '+++ Warnung: Dateiende innerhalb SQL-Einschub.' )
                     ;
           HALTX
         end (* then *) ;
       if S = STRIPU then
         WRITELN ( '+++ Warnung: Strichpunkt in SQL-Einschub.' ) ;
     until ( S = SQLENDSY ) or ( S = STRIPU ) ;
     EINR := EINRZWEI ;
     EINR := EINR - 9 ;
     INSQLSTATE := FALSE ;
     if S <> STRIPU then
       begin
         NEUZEILE ( TRUE ) ;
         OUTSYMBOL ( S ) ;
         INSYMBOL ( S ) ;
       end (* then *) ;
     STRIPUTEST ;
     EINR := EINRSAVE ;
     KOMMC . UEBERLESEN := 0 ;
   end (* SQLSTATE *) ;



procedure CSIMPSTATE ;

   var EINRSAVE : INTEGER ;


   procedure CSFEHLERTEST ;

      begin (* CSFEHLERTEST *)
        if S in WORTSYMBOLE - EXPRSYMBOLE - STERMSYMBOLE then
          begin
            OUTSYMBOL ( S ) ;
            WRITELN ( '+++ Syntaxfehler festgestellt.' ) ;
            HALTX
          end (* then *)
      end (* CSFEHLERTEST *) ;


   begin (* CSIMPSTATE *)
     EINRSAVE := EINR ;
     repeat
       OUTSYMBOL ( S ) ;
       INSYMBOL ( S ) ;
       CSFEHLERTEST
     until ( S = ZUWEISUNG ) or ( S = KLAMMAUF ) or ( S in STERMSYMBOLE
     ) ;
     if not ( S in STERMSYMBOLE ) then
       begin
         OUTSYMBOL ( S ) ;
         EINR := OUTPOINTER ;
         INSYMBOL ( S ) ;
         while not ( S in STERMSYMBOLE ) do
           begin
             CSFEHLERTEST ;
             OUTSYMBOL ( S ) ;
             INSYMBOL ( S )
           end (* while *)
       end (* then *) ;
     STRIPUTEST ;
     EINR := EINRSAVE
   end (* CSIMPSTATE *) ;



procedure CTYPE ;

   var EINRSAVE : INTEGER ;
       ANZKLAMAUF : INTEGER ;


   procedure FIELDLIST ( VORSCHUB : BOOLEAN ) ;

      var EINRS2 : INTEGER ;
          VS : BOOLEAN ;

      begin (* FIELDLIST *)
        VS := VORSCHUB ;
        while ( S <> ENDSY ) and ( S <> KLAMMZU ) do
          begin
            if VS then
              NEUZEILE ( TRUE ) ;
            VS := TRUE ;
            if S = CASESY then
              begin
                repeat
                  OUTSYMBOL ( S ) ;
                  INSYMBOL ( S )
                until S = OFSY ;
                OUTSYMBOL ( S ) ;
                EINR := EINR + INC ;
                INSYMBOL ( S ) ;
                repeat
                  if ( S <> ENDSY ) and ( S <> KLAMMZU ) then
                    begin
                      if S = STRIPU then
                        begin
                          NEUZEILE ( TRUE ) ;
                          OUTSYMBOL ( S ) ;
                          INSYMBOL ( S )
                        end (* then *)
                      else
                        begin
                          NEUZEILE ( TRUE ) ;
                          repeat
                            OUTSYMBOL ( S ) ;
                            INSYMBOL ( S ) ;
                          until S = KLAMMAUF ;
                          EINRS2 := EINR ;
                          EINR := EINR + INC ;
                          NEUZEILE ( TRUE ) ;
                          OUTSYMBOL ( S ) ;
                          EINR := EINR + INC ;
                          INSYMBOL ( S ) ;
                          repeat
                            if S <> KLAMMZU then
                              FIELDLIST ( FALSE ) ;
                            if S <> KLAMMZU then
                              NEUZEILE ( TRUE )
                          until S = KLAMMZU ;
                          OUTSYMBOL ( S ) ;
                          INSYMBOL ( S ) ;
                          STRIPUTEST ;
                          EINR := EINRS2
                        end (* else *)
                    end (* then *)
                until ( S = ENDSY ) or ( S = KLAMMZU ) ;
                EINR := EINR - INC
              end (* then *)
            else
              begin
                repeat
                  OUTSYMBOL ( S ) ;
                  INSYMBOL ( S )
                until S = DOPU ;
                OUTSYMBOL ( S ) ;
                INSYMBOL ( S ) ;
                CTYPE ;
                STRIPUTEST
              end (* else *)
          end (* while *)
      end (* FIELDLIST *) ;


   begin (* CTYPE *)
     if S <> ENDSY then
       begin
         EINRSAVE := EINR ;
         EINR := OUTPOINTER ;
         ANZKLAMAUF := 0 ;
         if S = KLAMMAUF then
           begin
             TTERMSYMBOLE := TTERMSYMBOLE - [ KLAMMZU ] ;
             ANZKLAMAUF := ANZKLAMAUF + 1 ;
           end (* then *) ;
         while not ( S in TTERMSYMBOLE ) do
           begin
             OUTSYMBOL ( S ) ;
             INSYMBOL ( S ) ;
             if S = KLAMMAUF then
               begin
                 TTERMSYMBOLE := TTERMSYMBOLE - [ KLAMMZU ] ;
                 ANZKLAMAUF := ANZKLAMAUF + 1 ;
               end (* then *) ;
             if S = KLAMMZU then
               begin
                 ANZKLAMAUF := ANZKLAMAUF - 1 ;
                 if ANZKLAMAUF < 0 then
                   TTERMSYMBOLE := TTERMSYMBOLE + [ KLAMMZU ] ;
               end (* then *) ;
           end (* while *) ;
         TTERMSYMBOLE := TTERMSYMBOLE + [ KLAMMZU ] ;
         if S = STRIPU then
           begin
             OUTSYMBOL ( S ) ;
             INSYMBOL ( S ) ;
             EINR := EINRSAVE ;
           end (* then *)
         else
           if S = RECORDSY then
             begin
               EINR := EINRSAVE ;
               EINRSAVE := EINR ;
               EINR := OUTPOINTER ;
               OUTSYMBOL ( S ) ;
               EINR := EINR + INC ;
               INSYMBOL ( S ) ;
               FIELDLIST ( TRUE ) ;
               EINR := EINR - INC ;
               NEUZEILE ( TRUE ) ;
               OUTSYMBOL ( S ) ;
               INSYMBOL ( S ) ;
               STRIPUTEST ;
               EINR := EINRSAVE ;
             end (* then *)
           else
             EINR := EINRSAVE
       end (* then *)
   end (* CTYPE *) ;



procedure CSTATE ( AUFNEUEZEILE : BOOLEAN ; KENNUNG : FELD6 ) ;

   var EINRSAVE : INTEGER ;
       LAUFX : INTEGER ;

   begin (* CSTATE *)
     if ( S = BEGINSY ) and not COMPOUNDNZ then
       AUFNEUEZEILE := FALSE ;
     if AUFNEUEZEILE then
       NEUZEILE ( TRUE ) ;

     (*********************************)
     (* leeres statement              *)
     (*********************************)

     if S = STRIPU then
       begin
         OUTSYMBOL ( S ) ;
         INSYMBOL ( S ) ;
         return
       end (* then *) ;

     (*********************************)
     (* prozeduraufruf                *)
     (*********************************)

     if S = IDENTIFIER then
       begin
         CSIMPSTATE ;
         return
       end (* then *) ;

     (*********************************)
     (* break (neu)                   *)
     (*********************************)

     if S = BREAKSY then
       begin
         CSIMPSTATE ;
         return
       end (* then *) ;

     (*********************************)
     (* continue (neu)                *)
     (*********************************)

     if S = CONTINUESY then
       begin
         CSIMPSTATE ;
         return
       end (* then *) ;

     (*********************************)
     (* return (neu)                  *)
     (*********************************)

     if S = RETURNSY then
       begin
         CSIMPSTATE ;
         return
       end (* then *) ;

     (*********************************)
     (* sql-Einschub                  *)
     (*********************************)

     if S = SQLBEGINSY then
       begin
         SQLSTATE ;
         return
       end (* then *) ;

     (*********************************)
     (* Statement mit label           *)
     (*********************************)

     if S = ZAHL then
       begin
         repeat
           OUTSYMBOL ( S ) ;
           INSYMBOL ( S )
         until S = DOPU ;
         OUTSYMBOL ( S ) ;
         INSYMBOL ( S ) ;
         CSTATE ( TRUE , '      ' ) ;
         return
       end (* then *) ;

     (*********************************)
     (* andere, je nach typ           *)
     (*********************************)

     if S = BEGINSY then
       begin
         OUTSYMBOL ( S ) ;
         if COMPOUNDNZ then
           EINR := EINR + INC ;
         INSYMBOL ( S ) ;
         repeat
           CSTATE ( TRUE , '      ' )
         until ( S in STERMSYMBOLE ) ;
         if COMPOUNDNZ then
           begin
             EINR := EINR - INC ;
             NEUZEILE ( TRUE ) ;
           end (* then *) ;
         OUTSYMBOL ( S ) ;
         if KENNUNG <> '      ' then
           begin
             if OUTPOINTER + 13 < MAXLOUTPUT then
               begin
                 WRITE ( AUSGABE , ' (* ' ) ;
                 for LAUFX := 1 to 6 do
                   if KENNUNG [ LAUFX ] <> ' ' then
                     WRITE ( AUSGABE , KENNUNG [ LAUFX ] ) ;
                 WRITE ( AUSGABE , ' *)' ) ;
                 OUTPOINTER := OUTPOINTER + 13
               end (* then *) ;
             KOMMC . UEBERLESEN := 2
           end (* then *) ;
         INSYMBOL ( S ) ;
         STRIPUTEST
       end (* then *)
     else
       if S = CASESY then
         begin
           repeat
             OUTSYMBOL ( S ) ;
             INSYMBOL ( S )
           until S = OFSY ;
           OUTSYMBOL ( S ) ;
           EINR := EINR + INC ;
           INSYMBOL ( S ) ;
           while not ( S in ( STERMSYMBOLE - [ ELSESY , OTHERWISESY ] )
           ) do
             begin
               NEUZEILE ( TRUE ) ;
               if S = ELSESY then
                 begin
                   OUTSYMBOL ( S ) ;
                   INSYMBOL ( S ) ;
                   EINR := EINR + INC ;
                   NEUZEILE ( TRUE ) ;
                   CSTATE ( FALSE , 'else/c' ) ;
                   EINR := EINR - INC ;
                 end (* then *)
               else
                 if S = OTHERWISESY then
                   begin
                     OUTSYMBOL ( S ) ;
                     INSYMBOL ( S ) ;
                     EINR := EINR + INC ;
                     NEUZEILE ( TRUE ) ;
                     CSTATE ( FALSE , 'otherw' ) ;
                     EINR := EINR - INC ;
                   end (* then *)
                 else
                   begin
                     while S <> DOPU do
                       begin
                         OUTSYMBOL ( S ) ;
                         INSYMBOL ( S )
                       end (* while *) ;
                     OUTSYMBOL ( S ) ;
                     if OUTPOINTER - EINR <= 8 then
                       begin
                         EINRSAVE := EINR ;
                         EINR := OUTPOINTER ;
                       end (* then *)
                     else
                       begin
                         EINRSAVE := EINR ;
                         EINR := EINR + INC ;
                         NEUZEILE ( TRUE ) ;
                       end (* else *) ;
                     INSYMBOL ( S ) ;
                     CSTATE ( FALSE , 'tag/ca' ) ;
                     EINR := EINRSAVE ;
                   end (* else *)
             end (* while *) ;
           EINR := EINR - INC ;
           NEUZEILE ( TRUE ) ;
           OUTSYMBOL ( S ) ;
           if OUTPOINTER + 11 < MAXLOUTPUT then
             begin
               WRITE ( AUSGABE , ' (* case *)' ) ;
               OUTPOINTER := OUTPOINTER + 11
             end (* then *) ;
           KOMMC . UEBERLESEN := 2 ;
           INSYMBOL ( S ) ;
           STRIPUTEST
         end (* then *)
       else
         if ( S = FORSY ) or ( S = WHILESY ) or ( S = WITHSY ) then
           begin
             if S = FORSY then
               KENNUNG := 'for   ' ;
             if S = WHILESY then
               KENNUNG := 'while ' ;
             if S = WITHSY then
               KENNUNG := 'with  ' ;
             repeat
               OUTSYMBOL ( S ) ;
               INSYMBOL ( S )
             until S = DOSY ;
             OUTSYMBOL ( S ) ;
             EINR := EINR + INC ;
             INSYMBOL ( S ) ;
             CSTATE ( TRUE , KENNUNG ) ;
             STRIPUTEST ;
             EINR := EINR - INC ;
           end (* then *)
         else
           if S = GOTOSY then
             CSIMPSTATE
           else
             if S = IFSY then
               begin
                 repeat
                   OUTSYMBOL ( S ) ;
                   INSYMBOL ( S )
                 until S = THENSY ;
                 OUTSYMBOL ( S ) ;
                 EINR := EINR + INC ;
                 INSYMBOL ( S ) ;
                 CSTATE ( TRUE , 'then  ' ) ;
                 EINR := EINR - INC ;
                 if S = ELSESY then
                   begin
                     NEUZEILE ( TRUE ) ;
                     OUTSYMBOL ( S ) ;
                     EINR := EINR + INC ;
                     INSYMBOL ( S ) ;
                     CSTATE ( TRUE , 'else  ' ) ;
                     STRIPUTEST ;
                     EINR := EINR - INC ;
                   end (* then *)
                 else
                   STRIPUTEST
               end (* then *)
             else
               if S = REPEATSY then
                 begin
                   OUTSYMBOL ( S ) ;
                   EINR := EINR + INC ;
                   INSYMBOL ( S ) ;
                   repeat
                     CSTATE ( TRUE , '      ' )
                   until ( S in STERMSYMBOLE ) ;
                   EINR := EINR - INC ;
                   NEUZEILE ( TRUE ) ;
                   repeat
                     OUTSYMBOL ( S ) ;
                     INSYMBOL ( S )
                   until ( S in STERMSYMBOLE ) ;
                   STRIPUTEST
                 end (* then *)
               else
                 begin
                   if NULLSTATE then
                     begin
                       WRITELN ( '+++ Syntaxfehler festgestellt.' ) ;
                       HALTX
                     end (* then *)
                   else
                     NULLSTATE := TRUE
                 end (* else *)
   end (* CSTATE *) ;



procedure CBODY ( FUELLWORT : PWORT ; BLOCKLEVEL : INTEGER ) ;

   var I : INTEGER ;
       EINRKOMMSAVE : INTEGER ;

   begin (* CBODY *)
     if S = BEGINSY then
       begin
         EINRKOMMSAVE := EINRKOMM ;
         NEUZEILE ( TRUE ) ;
         OUTSYMBOL ( S ) ;
         WRITE ( AUSGABE , ' (* ' ) ;
         for I := 1 to FUELLWORT . LAENGE do
           WRITE ( AUSGABE , FUELLWORT . NAME [ I ] ) ;
         WRITE ( AUSGABE , ' *)' ) ;
         EINR := EINR + INC ;
         EINRKOMM := EINR ;
         KOMMC . UEBERLESEN := 2 ;
         INSYMBOL ( S ) ;
         repeat
           CSTATE ( TRUE , '      ' )
         until ( S in STERMSYMBOLE ) ;
         EINR := EINR - INC ;
         NEUZEILE ( TRUE ) ;
         OUTSYMBOL ( S ) ;
         WRITE ( AUSGABE , ' (* ' ) ;
         for I := 1 to FUELLWORT . LAENGE do
           WRITE ( AUSGABE , FUELLWORT . NAME [ I ] ) ;
         WRITE ( AUSGABE , ' *)' ) ;
         KOMMC . UEBERLESEN := 2 ;
         INSYMBOL ( S ) ;
         KOMM_ZWISCHEN_PROCS ( BLOCKLEVEL ) ;
         EINRKOMM := EINRKOMMSAVE ;
       end (* then *)
     else
       begin
         EINRKOMMSAVE := EINRKOMM ;
         EINRKOMM := EINR ;
         CSTATE ( TRUE , '      ' ) ;
         EINRKOMM := EINRKOMMSAVE ;
       end (* else *)
   end (* CBODY *) ;



procedure CBLOCK ( FUELLWORT : PWORT ; BLOCKLEVEL : INTEGER ) ;

   var EINRSAVE : INTEGER ;
       EINRKOMMSAVE : INTEGER ;
       KLAMMZ : INTEGER ;
       I : INTEGER ;
       PROCWORT : PWORT ;
       SALT : SYMBOL ;
       INCR : INTEGER ;
       PROC_VORH : BOOLEAN ;

   begin (* CBLOCK *)
     EINRKOMMSAVE := EINRKOMM ;
     EINRKOMM := EINR ;
     while TRUE do
       begin
         if S = LABELSY then
           begin
             NEUZEILE ( FALSE ) ;
             if BLOCKLEVEL = 1 then
               NEUZEILE ( FALSE ) ;
             NEUZEILE ( TRUE ) ;
             OUTSYMBOL ( S ) ;
             EINRSAVE := EINR ;
             EINR := OUTPOINTER ;
             EINRKOMMSAVE := EINRKOMM ;
             EINRKOMM := OUTPOINTER ;
             INSYMBOL ( S ) ;
             while S <> STRIPU do
               begin
                 OUTSYMBOL ( S ) ;
                 INSYMBOL ( S )
               end (* while *) ;
             OUTSYMBOL ( S ) ;
             INSYMBOL ( S ) ;
             EINR := EINRSAVE ;
             continue ;
           end (* then *) ;
         if S = CONSTSY then
           begin
             NEUZEILE ( FALSE ) ;
             if BLOCKLEVEL = 1 then
               NEUZEILE ( FALSE ) ;
             NEUZEILE ( TRUE ) ;
             OUTSYMBOL ( S ) ;
             EINRKOMM := OUTPOINTER ;
             INSYMBOL ( S ) ;
             EINR := EINR + 6 ;
             while S = IDENTIFIER do
               begin
                 INCR := 0 ;
                 repeat
                   OUTSYMBOL ( S ) ;

     (************************************************)
     (* wenn nach gleichheitszeichen eine runde oder *)
     (* eckige klammer folgt, dann neuzeile          *)
     (************************************************)

                   SALT := S ;
                   INSYMBOL ( S ) ;
                   if ( SALT = GLEICH ) and ( ( S = KLAMMAUF ) or ( S =
                   ECKKLAMMAUF ) ) then
                     begin
                       NEUZEILE ( TRUE ) ;
                       INCR := INCR + 2 ;
                       EINR := EINR + 2 ;
                     end (* then *) ;
                 until S = STRIPU ;
                 OUTSYMBOL ( S ) ;
                 EINR := EINR - INCR ;
                 INSYMBOL ( S ) ;
                 if S = IDENTIFIER then
                   NEUZEILE ( TRUE )
               end (* while *) ;
             EINR := EINR - 6 ;
             continue ;
           end (* then *) ;
         if S = TYPESY then
           begin
             NEUZEILE ( FALSE ) ;
             if BLOCKLEVEL = 1 then
               NEUZEILE ( FALSE ) ;
             NEUZEILE ( TRUE ) ;
             OUTSYMBOL ( S ) ;
             EINRKOMM := OUTPOINTER ;
             INSYMBOL ( S ) ;
             EINR := EINR + 5 ;
             while S = IDENTIFIER do
               begin
                 repeat
                   OUTSYMBOL ( S ) ;
                   INSYMBOL ( S )
                 until S = GLEICH ;
                 OUTSYMBOL ( S ) ;
                 INSYMBOL ( S ) ;
                 CTYPE ;
                 if S = IDENTIFIER then
                   NEUZEILE ( TRUE )
               end (* while *) ;
             EINR := EINR - 5 ;
             continue ;
           end (* then *) ;
         if S = SQLVARSY then
           begin
             NEUZEILE ( FALSE ) ;
             if BLOCKLEVEL = 1 then
               NEUZEILE ( FALSE ) ;
             NEUZEILE ( TRUE ) ;
             OUTSYMBOL ( S ) ;
             EINRKOMM := OUTPOINTER ;
             INSYMBOL ( S ) ;
             EINR := EINR + 7 ;
             while S = IDENTIFIER do
               begin
                 repeat
                   OUTSYMBOL ( S ) ;
                   INSYMBOL ( S )
                 until S = DOPU ;
                 OUTSYMBOL ( S ) ;
                 INSYMBOL ( S ) ;
                 CTYPE ;
                 if S = IDENTIFIER then
                   NEUZEILE ( TRUE )
               end (* while *) ;
             EINR := EINR - 7 ;
             continue ;
           end (* then *) ;
         if S = VARSY then
           begin
             NEUZEILE ( FALSE ) ;
             if BLOCKLEVEL = 1 then
               NEUZEILE ( FALSE ) ;
             NEUZEILE ( TRUE ) ;
             OUTSYMBOL ( S ) ;
             EINRKOMM := OUTPOINTER ;
             INSYMBOL ( S ) ;
             EINR := EINR + 4 ;
             while S = IDENTIFIER do
               begin
                 repeat
                   OUTSYMBOL ( S ) ;
                   INSYMBOL ( S )
                 until S = DOPU ;
                 OUTSYMBOL ( S ) ;
                 INSYMBOL ( S ) ;
                 CTYPE ;
                 if S = IDENTIFIER then
                   NEUZEILE ( TRUE )
               end (* while *) ;
             EINR := EINR - 4 ;
             continue ;
           end (* then *) ;
         if S = STATICSY then
           begin
             NEUZEILE ( FALSE ) ;
             if BLOCKLEVEL = 1 then
               NEUZEILE ( FALSE ) ;
             NEUZEILE ( TRUE ) ;
             OUTSYMBOL ( S ) ;
             EINRKOMM := OUTPOINTER ;
             INSYMBOL ( S ) ;
             EINR := EINR + 7 ;
             while S = IDENTIFIER do
               begin
                 repeat
                   OUTSYMBOL ( S ) ;
                   INSYMBOL ( S )
                 until S = DOPU ;
                 OUTSYMBOL ( S ) ;
                 INSYMBOL ( S ) ;
                 CTYPE ;
                 if S = IDENTIFIER then
                   NEUZEILE ( TRUE )
               end (* while *) ;
             EINR := EINR - 7 ;
             continue ;
           end (* then *) ;
         break ;
       end (* while *) ;
     PROC_VORH := FALSE ;
     while ( S = PROCEDURESY ) or ( S = FUNCTIONSY ) or ( S = OVERLAYSY
     ) or ( S = LOCALSY ) do
       begin
         if not PROC_VORH then
           if BLOCKLEVEL = 1 then
             NEUZEILE ( FALSE ) ;
         PROC_VORH := TRUE ;
         NEUZEILE ( FALSE ) ;
         NEUZEILE ( FALSE ) ;
         NEUZEILE ( TRUE ) ;
         KLAMMZ := 0 ;
         EINRSAVE := EINR ;
         EINRKOMM := EINR ;
         while ( S = OVERLAYSY ) or ( S = LOCALSY ) do
           begin
             OUTSYMBOL ( S ) ;
             INSYMBOL ( S ) ;
           end (* while *) ;
         OUTSYMBOL ( S ) ;
         INSYMBOL ( S ) ;
         PROCWORT . NAME := '' ;
         for I := 1 to W1ENDE do
           if I <= MAXIDSIZE then
             PROCWORT . NAME [ I ] := W1 [ I ] ;
         if W1ENDE > MAXIDSIZE then
           PROCWORT . LAENGE := MAXIDSIZE
         else
           PROCWORT . LAENGE := W1ENDE ;
         repeat
           OUTSYMBOL ( S ) ;
           INSYMBOL ( S ) ;
           if S = KLAMMAUF then
             begin
               KLAMMZ := KLAMMZ + 1 ;
               if KLAMMZ = 1 then
                 begin
                   EINR := OUTPOINTER
                 end (* then *)
             end (* then *) ;
           if S = KLAMMZU then
             KLAMMZ := KLAMMZ - 1
         until ( S = STRIPU ) and ( KLAMMZ = 0 ) ;
         OUTSYMBOL ( S ) ;
         INSYMBOL ( S ) ;
         EINR := EINRSAVE ;
         EINR := EINR + 3 ;
         CBLOCK ( PROCWORT , BLOCKLEVEL + 1 ) ;
         EINR := EINR - 3 ;
         if BLOCKLEVEL = 1 then
           NEUZEILE ( FALSE ) ;
       end (* while *) ;
     NEUZEILE ( FALSE ) ;
     if ( BLOCKLEVEL = 1 ) or PROC_VORH then
       NEUZEILE ( FALSE ) ;
     CBODY ( FUELLWORT , BLOCKLEVEL ) ;
     EINRKOMM := EINRKOMMSAVE
   end (* CBLOCK *) ;



procedure PROG ;

   var S : SYMBOL ;

   begin (* PROG *)
     repeat
       INSYMBOL ( S ) ;
       OUTSYMBOL ( S ) ;
       if S = ENDSY then
         KOMMC . UEBERLESEN := 2
     until EOFILE
   end (* PROG *) ;



procedure CHANGE ;

   var EINRSAVE : INTEGER ;
       I : INTEGER ;
       MAINWORT : PWORT ;

   begin (* CHANGE *)
     MAINWORT . NAME := 'HAUPTPROGRAMM  ' ;
     MAINWORT . LAENGE := 13 ;
     EINRSAVE := EINR ;
     INSYMBOL ( S ) ;
     OUTSYMBOL ( S ) ;
     if ( S = PROGRAMSY ) or ( S = MODULESY ) then
       begin
         MAINWORT . NAME := 'HAUPTPROGRAMM  ' ;
         MAINWORT . LAENGE := 13
       end (* then *)
     else
       begin
         INSYMBOL ( S ) ;
         MAINWORT . NAME := '' ;
         for I := 1 to W1ENDE do
           if I <= MAXIDSIZE then
             MAINWORT . NAME [ I ] := W1 [ I ] ;
         if W1ENDE > MAXIDSIZE then
           MAINWORT . LAENGE := MAXIDSIZE
         else
           MAINWORT . LAENGE := W1ENDE ;
         OUTSYMBOL ( S )
       end (* else *) ;
     INSYMBOL ( S ) ;
     repeat
       OUTSYMBOL ( S ) ;
       if S = KLAMMAUF then
         begin
           EINR := OUTPOINTER ;
           repeat
             INSYMBOL ( S ) ;
             OUTSYMBOL ( S )
           until S = KLAMMZU ;
         end (* then *) ;
       INSYMBOL ( S )
     until S = STRIPU ;
     OUTSYMBOL ( S ) ;
     INSYMBOL ( S ) ;
     EINR := EINRSAVE ;
     CBLOCK ( MAINWORT , 1 ) ;
     OUTSYMBOL ( S ) ;
     INSYMBOL ( S ) ;
     if S <> EOFSY then
       WRITELN ( '+++ Warnung: Es wurde nicht die' ,
                 ' ganze Datei verarbeitet.' )
   end (* CHANGE *) ;



procedure VORBESETZEN ;

   begin (* VORBESETZEN *)
     WTAB [ ANDSY ] := 'AND       ' ;
     WTAB [ ARRAYSY ] := 'ARRAY     ' ;
     WTAB [ BEGINSY ] := 'BEGIN     ' ;
     WTAB [ BREAKSY ] := 'BREAK     ' ;
     WTAB [ CASESY ] := 'CASE      ' ;
     WTAB [ CONSTSY ] := 'CONST     ' ;
     WTAB [ CONTINUESY ] := 'CONTINUE  ' ;
     WTAB [ DOSY ] := 'DO        ' ;
     WTAB [ ELSESY ] := 'ELSE      ' ;
     WTAB [ ENDSY ] := 'END       ' ;
     WTAB [ FORSY ] := 'FOR       ' ;
     WTAB [ FUNCTIONSY ] := 'FUNCTION  ' ;
     WTAB [ GOTOSY ] := 'GOTO      ' ;
     WTAB [ IFSY ] := 'IF        ' ;
     WTAB [ INSY ] := 'IN        ' ;
     WTAB [ LABELSY ] := 'LABEL     ' ;
     WTAB [ LOCALSY ] := 'LOCAL     ' ;
     WTAB [ MODULESY ] := 'MODULE    ' ;
     WTAB [ NOTSY ] := 'NOT       ' ;
     WTAB [ OFSY ] := 'OF        ' ;
     WTAB [ ORSY ] := 'OR        ' ;
     WTAB [ OTHERWISESY ] := 'OTHERWISE ' ;
     WTAB [ OVERLAYSY ] := 'OVERLAY   ' ;
     WTAB [ PACKEDSY ] := 'PACKED    ' ;
     WTAB [ PROCEDURESY ] := 'PROCEDURE ' ;
     WTAB [ PROGRAMSY ] := 'PROGRAM   ' ;
     WTAB [ RECORDSY ] := 'RECORD    ' ;
     WTAB [ REPEATSY ] := 'REPEAT    ' ;
     WTAB [ RETURNSY ] := 'RETURN    ' ;
     WTAB [ SETSY ] := 'SET       ' ;
     WTAB [ SQLBEGINSY ] := 'SQLBEGIN  ' ;
     WTAB [ SQLENDSY ] := 'SQLEND    ' ;
     WTAB [ SQLVARSY ] := 'SQLVAR    ' ;
     WTAB [ STATICSY ] := 'STATIC    ' ;
     WTAB [ THENSY ] := 'THEN      ' ;
     WTAB [ TOSY ] := 'TO        ' ;
     WTAB [ TYPESY ] := 'TYPE      ' ;
     WTAB [ UNTILSY ] := 'UNTIL     ' ;
     WTAB [ VARSY ] := 'VAR       ' ;
     WTAB [ WHILESY ] := 'WHILE     ' ;
     WTAB [ WITHSY ] := 'WITH      ' ;
     WTAB [ XORSY ] := 'XOR       ' ;
     WTABSQL [ 1 ] := 'CONNECT   ' ;
     WTABSQL [ 2 ] := 'DECLARE   ' ;
     WTABSQL [ 3 ] := 'DELETE    ' ;
     WTABSQL [ 4 ] := 'FROM      ' ;
     WTABSQL [ 5 ] := 'GROUP     ' ;
     WTABSQL [ 6 ] := 'HAVING    ' ;
     WTABSQL [ 7 ] := 'INSERT    ' ;
     WTABSQL [ 8 ] := 'INTO      ' ;
     WTABSQL [ 9 ] := 'ORDER     ' ;
     WTABSQL [ 10 ] := 'SELECT    ' ;
     WTABSQL [ 11 ] := 'SET       ' ;
     WTABSQL [ 12 ] := 'UNION     ' ;
     WTABSQL [ 13 ] := 'UPDATE    ' ;
     WTABSQL [ 14 ] := 'VALUES    ' ;
     WTABSQL [ 15 ] := 'WHERE     ' ;
     ANZSQLWORTE := 15 ;
   end (* VORBESETZEN *) ;



begin (* HAUPTPROGRAMM *)
  REWRITE ( TRACE ) ;
  MODUS := 4 ;
  KOMMC . KOMML := 0 ;
  KOMMC . ENDOFKOMM := FALSE ;
  KOMMC . ANZKOMM := 0 ;
  KOMMC . ZUSTAND := 0 ;
  KOMMC . KOMMTYPE := ' ' ;
  KOMMC . NURSTERNE := FALSE ;
  KOMMC . KOMMSTATUS := 0 ;
  KOMMC . KOMM_VOR_PROC := FALSE ;

  (***********************************************)
  (*   HIER UNTERSCHIED PASCAL/VS ZU TURBO/3     *)
  (*   TERMIN (INPUT) ; TERMOUT (OUTPUT) ;       *)
  (*   WIRD BENOETIGT BEI PASCAL/VS              *)
  (***********************************************)

  COMPOUNDNZ := TRUE ;
  if MODUS > 4 then
    begin
      COMPOUNDNZ := FALSE ;
      MODUS := MODUS - 2
    end (* then *) ;
  DICHT := ( MODUS = 1 ) or ( MODUS = 3 ) ;
  VORBESETZEN ;
  STERMSYMBOLE := [ EOFSY , STRIPU , ELSESY , ENDSY , OTHERWISESY ,
                  UNTILSY ] ;
  TTERMSYMBOLE := [ EOFSY , STRIPU , ENDSY , RECORDSY , KLAMMZU ] ;
  BLANKSYMBOLE := [ IDENTIFIER , ZAHL , ANDSY , ARRAYSY , BEGINSY ,
                  BREAKSY , CASESY , CONSTSY , CONTINUESY , DOSY ,
                  ELSESY , ENDSY , FORSY , FUNCTIONSY , GOTOSY , IFSY ,
                  INSY , LABELSY , LOCALSY , MODULESY , NOTSY , OFSY ,
                  ORSY , OTHERWISESY , OVERLAYSY , PACKEDSY ,
                  PROCEDURESY , PROGRAMSY , RECORDSY , REPEATSY ,
                  RETURNSY , SETSY , SQLBEGINSY , SQLENDSY , SQLVARSY ,
                  STATICSY , THENSY , TOSY , TYPESY , UNTILSY , VARSY ,
                  WHILESY , WITHSY , XORSY ] ;
  WORTSYMBOLE := [ ANDSY , ARRAYSY , BEGINSY , BREAKSY , CASESY ,
                 CONSTSY , CONTINUESY , DOSY , ELSESY , ENDSY , FORSY ,
                 FUNCTIONSY , GOTOSY , IFSY , INSY , LABELSY , LOCALSY
                 , MODULESY , NOTSY , OFSY , ORSY , OTHERWISESY ,
                 OVERLAYSY , PACKEDSY , PROCEDURESY , PROGRAMSY ,
                 RECORDSY , REPEATSY , RETURNSY , SETSY , SQLBEGINSY ,
                 SQLENDSY , SQLVARSY , STATICSY , THENSY , TOSY ,
                 TYPESY , UNTILSY , VARSY , WHILESY , WITHSY , XORSY ]
                 ;
  EXPRSYMBOLE := [ ANDSY , INSY , NOTSY , ORSY , XORSY ] ;
  SONDERZEICHEN := [ '@' , '^' , '*' , ')' , '-' , '+' , '=' , ':' ,
                   ';' , ',' , '.' , '/' , '>' , '<' , '!' , '%' , '['
                   , ']' , VERKETT2 ] ;
  ISTARTSET := [ 'A' .. 'I' , 'J' .. 'R' , 'S' .. 'Z' , '$' ] ;
  IWEITERSET := [ 'A' .. 'I' , 'J' .. 'R' , 'S' .. 'Z' , '0' .. '9' ,
                '_' ] ;
  ZIFFERN := [ '0' .. '9' ] ;
  HEXZIFFERN := [ '0' .. '9' , 'A' .. 'F' ] ;
  NICHTLESEN := 0 ;
  EINR := 0 ;
  EINRKOMM := 0 ;
  INC := 2 ;
  NOMAJOR := FALSE ;
  BLANKSVORHANDEN := FALSE ;
  KOMMC . UEBERLESEN := 0 ;
  ENDEKASTEN := ' ' ;
  ENDEKOMMEIN := 0 ;
  KOMMLAENGE := 0 ;
  INSQLSTATE := FALSE ;
  SQLHOSTV := FALSE ;
  ZZAUS := 0 ;
  ZZAUSVOR := - 1 ;
  RESET ( EINGABE ) ;
  REWRITE ( AUSGABE ) ;
  W1INDEX := 0 ;
  INPOINTER := 0 ;
  if EINR <> 0 then
    WRITE ( AUSGABE , ' ' : EINR ) ;
  OUTPOINTER := EINR ;
  if MODUS < 3 then
    PROG
  else
    CHANGE ;
  WRITELN ( AUSGABE ) ;

  (***********************************************)
  (*   HIER UNTERSCHIED PASCAL/VS ZU TURBO/3     *)
  (*   CLOSE (EINGABE) ; CLOSE (AUSGABE) ;       *)
  (*   WIRD BENOETIGT BEI TURBO/3                *)
  (***********************************************)

  WRITELN ( ZZAUS + 1 : 6 , ' Zeilen ausgegeben.' ) ;
end (* HAUPTPROGRAMM *) .
