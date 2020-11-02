program PASFORM ( OUTPUT , EINGABE , LISTING , AUSGABE , TRACEF ) ;

(********************************************************************)
(*$D+,U-                                                            *)
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
(*   Historie zeitlich absteigend ...                               *)
(********************************************************************)
(*                                                                  *)
(*   Neue Version im August 2019                                    *)
(*                                                                  *)
(*   - Kommentare am Zeilenende (ohne Kasten) immer mit //          *)
(*                                                                  *)
(*   - durch mindestens ein Blank absetzen                          *)
(*                                                                  *)
(*   - wenn der Kommentar ueber Spalte 72 hinausreicht,             *)
(*     in naechster Zeile an gleicher Position fortsetzen           *)
(*                                                                  *)
(*   - wenn in derselben Zeile noch etwas kommt, dann               *)
(*     auf jeden Fall in neuer Zeile beginnen                       *)
(*                                                                  *)
(*   (inspiriert durch den Versuch, den P5-Source zu bearbeiten)    *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*   Neue Version im November 2017                                  *)
(*                                                                  *)
(*   - Verwendung des Scanners PASSCAN (analog Compiler)            *)
(*                                                                  *)
(*   - zusaetzlich werden hier die Symbole SQLBEGIN, SQLEND,        *)
(*     SQLVAR und OVERLAY verarbeitet                               *)
(*                                                                  *)
(*   - Kommentarbearbeitung (fast) wie vorher                       *)
(*                                                                  *)
(*   - neu: C++ Kommentare werden unterstuetzt                      *)
(*                                                                  *)
(*   - neu: Korrekturen bei Kommentaren ueber mehrere Zeilen        *)
(*                                                                  *)
(*   - Ausgabe von Protokoll auf Datei LISTING                      *)
(*                                                                  *)
(*   - Korrektur: Blank eingefuegt bei Symbol INTDOTDOT             *)
(*                                                                  *)
(*   - neu: Kommentare hinter Definitionen usw. bleiben stehen      *)
(*     d.h.: Kommentare, denen in derselben Zeile ein anderes       *)
(*     Symbol (ausser Separator) vorangeht, werden anders           *)
(*     behandelt als bisher. Sie behalten idealerweise ihre         *)
(*     Position. Wenn von der letzten Zeile her ein solcher         *)
(*     Kommentar bereits vorhanden ist, wird versucht, den          *)
(*     neuen Kommentar ebenfalls an dieser Position                 *)
(*     auszurichten                                                 *)
(*                                                                  *)
(*   - kein Abbruch bei Fehler, idealerweise keine Endlos-          *)
(*     schleife, sondern Diagnose wie beim Compiler und             *)
(*     weiterarbeiten                                               *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*   ANPASSUNGEN am 31.08.2016                                      *)
(*                                                                  *)
(*   Kommentare anders eingerueckt                                  *)
(*   Neuzeile bei komplexen consts (Stanford-Erweiterung)           *)
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



const VERSION = '11.2017' ;
      VERKETT2 = '|' ;
      MAXLSIZE = 120 ;
      MAXLOUTPUT = 72 ;
      MAXLINPUT = 160 ;
      MAXEINR = 51 ;
      MAXIDSIZE = 20 ;
      MAXSETL = 252 ;
      MAXSTRL = 254 ;
      REALLNGTH = 20 ;
      MAXRW = 60 ;
      MAXRWLEN = 9 ;

      //**********************************************************
      // longest reserved word has length = 9                     
      // controls size of table frw                               
      //**********************************************************

      IDLNGTH = 20 ;
      MAXINT = 2147483647 ;
      DIGMAX = 19 ;


type WORT = array [ 1 .. 100 ] of CHAR ;
     ALPHA = array [ 1 .. IDLNGTH ] of CHAR ;
     SWORT = packed array [ 1 .. 10 ] of CHAR ;
     FELD6 = packed array [ 1 .. 6 ] of CHAR ;
     SET_CHAR = set of CHAR ;
     PWORT = record
               NAME : packed array [ 1 .. MAXIDSIZE ] of CHAR ;
               LAENGE : INTEGER
             end ;
     SSP = -> XSTRCON ;
     SETSTRING = array [ 1 .. MAXSETL ] of CHAR ;
     CSTCLASS = ( XINT , REEL , PSET , STRG ) ;
     XSTRCON = record
                 LENGTH : INTEGER ;
                 case TAG : CHAR of
                   'S' :
                     ( SSTR : array [ 1 .. MAXSTRL ] of CHAR ) ;
                   'P' :
                     ( PSTR : SETSTRING )
               end ;
     XCONSTANT = record
                   STRTYPE : CHAR ;
                   case CSTCLASS of
                     XINT :
                       ( IVAL : INTEGER ) ;
                     REEL :
                       ( RVAL : array [ 1 .. REALLNGTH ] of CHAR ) ;
                     PSET :
                       ( SETMIN : INTEGER ;
                         SETMAX : INTEGER ;
                         SETOFFS : INTEGER ;
                         PVAL : SSP ) ;
                     STRG :
                       ( SVAL : SSP )
                 end ;

     //************************************************************
     // muss mit Def. beim Scanner                                 
     // uebereinstimmen                                            
     //************************************************************

     SYMB = ( SYMB_EOF , SYMB_UNKNOWN , EOLCHAR , SEPARATOR , COMMENT1
            , COMMENT2 , COMMENT3 , COMMENT4 , COMMENT5 , STRINGCONST ,
            HEXSTRINGCONST , BINSTRINGCONST , INTCONST , INTDOTDOT ,
            INTRBRACK , REALCONST , IDENT , SYLPARENT , SYRPARENT ,
            SYLBRACK , SYRBRACK , SYCOMMA , SYSEMICOLON , SYARROW ,
            SYPERIOD , SYDOTDOT , SYCOLON , SYPLUS , SYMINUS , SYMULT ,
            SYSLASH , SYEQOP , SYNEOP , SYGTOP , SYLTOP , SYGEOP ,
            SYLEOP , SYOROP , SYANDOP , SYASSIGN , SYCONCAT , SYAND ,
            SYDIV , SYMOD , SYOR , SYXOR , SYIN , SYNOT , SYLABEL ,
            SYCONST , SYTYPE , SYVAR , SYFUNC , SYPROG , SYPROC , SYSET
            , SYPACKED , SYARRAY , SYRECORD , SYFILE , SYFORWARD ,
            SYBEGIN , SYIF , SYCASE , SYREPEAT , SYWHILE , SYFOR ,
            SYWITH , SYGOTO , SYEND , SYELSE , SYUNTIL , SYOF , SYDO ,
            SYTO , SYDOWNTO , SYTHEN , SYFRTRN , SYEXTRN , SYOTHERWISE
            , SYBREAK , SYCONTINUE , SYRETURN , SYMODULE , SYLOCAL ,
            SYSTATIC , SYSQLBEGIN , SYSQLEND , SYSQLVAR , SYOVERLAY ,
            NOTUSED ) ;
     SYMSET = set of SYMB ;

     //************************************************************
     // struktur, die den Zustand fuer die                         
     // kommentarroutinen festhaelt                                
     //************************************************************

     KOMMSTATUS = record
                    LASTSCBLINE : INTEGER ;    // to check symbols
                    LINENR : INTEGER ;         // zeilennr aus scb
                    LINEPOS : INTEGER ;        // linepos scb
                    SYMB_VOR_KOMM : BOOLEAN ;  // symb in zeile vorh.
                    KOMM_VOR_PROC : BOOLEAN ;  // vor prozedur ?
                    UEBERLESEN : INTEGER ;     // ueberlesen ?
                    NEUZEILE_VORM : BOOLEAN ;  // neuzeile-Vormerkung
                    EINRRECHTS : INTEGER ;     // einrueckung rechts
                    EINRRECHTS_SAVE : INTEGER ;
                    RECHTSLINE : INTEGER
                  end ;
     KOMM_CONT = record
                   KOMMTYPE : CHAR ;           // welcher typ
                   FIRST_IN_LINE : BOOLEAN ;   // komm erst in zeile
                   LAST_IN_LINE : BOOLEAN ;    // komm letzt in zeile
                   KOMM_NLINES : INTEGER ;     // anzahl lines
                   KOMM_WIDTH : INTEGER ;      // breite des komm.
                   KOMM_WIDTH_BR : INTEGER ;   // breite brutto
                   KLINE_ANKER : -> KOMMLINE ; // erste kommzeile
                   KLINE_AKT : -> KOMMLINE ;   // aktuelle kommzeile
                   with Y : KOMMSTATUS         // include status
                 end ;
     KOMMLINE = record
                  L : STRING ( 120 ) ;       // inhalt ohne komm.z
                  NURSTERNE : BOOLEAN ;      // nur sterne ?
                  NEXT : -> KOMMLINE         // naechste zeilen
                end ;

     //************************************************************
     // zentraler Scan-Block                                       
     //************************************************************
     // muss mit Def. beim Scanner                                 
     // uebereinstimmen                                            
     //************************************************************

     CHAR32 = array [ 1 .. 32 ] of CHAR ;
     SOURCELINE = array [ 1 .. MAXLSIZE ] of CHAR ;
     SCAN_ERRCLASS = 'A' .. 'Z' ;
     SCANF_PTR = ANYPTR ;
     SCANFT_PTR = ANYPTR ;
     OPTIONS_PTR = -> COMP_OPTIONS ;
     SCAN_BLOCK = record
                    MODUS : INTEGER ;        // modus of scanner
                    DATEIENDE : INTEGER ;    // end of file indicator
                    ENDOFLINE : BOOLEAN ;    // end of line indicator
                    SLINE : SOURCELINE ;     // stored source line
                    LINENR : INTEGER ;       // line number of symbol
                    LINEPOS : INTEGER ;      // line position of symb
                    LINELEN : INTEGER ;      // line length
                    SKIPPING : BOOLEAN ;     // parser is skipping
                    LOOKAHEAD : CHAR ;       // lookahead character
                    SYMBOLNR : SYMB ;        // symbol read
                    SYMBOL : SOURCELINE ;    // characters of symb
                    LSYMBOL : INTEGER ;      // no of chars in symb
                    MAXLSYMBOL : INTEGER ;   //
                    UFZAHL : INTEGER ;       // no of undef errors
                    SFZAHL : INTEGER ;       // no of severe errors
                    FEZAHL : INTEGER ;       // no of errors
                    WAZAHL : INTEGER ;       // no of warnings
                    INZAHL : INTEGER ;       // no of informations
                    FEANFANG : SCANF_PTR ;   // anchor to err list
                    FEAKT : SCANF_PTR ;      // actual err elem
                    FTTAB : SCANFT_PTR ;     // error text table
                    FTTABA : SCANFT_PTR ;    // same for applic.
                    OPTLINE : SOURCELINE ;   // options line
                    POPT : OPTIONS_PTR ;     // ptr to opt struct

     /******************************************/
     /* felder fuer sofortige Protokollausgabe */
     /******************************************/

                    PROTOUT : BOOLEAN ;        // switch for prot out
                    TERMOUT : BOOLEAN ;        // switch for term out
                    FEAKT_ALT : SCANF_PTR ;    // old feakt
                    LINEINFO : CHAR32 ;        // line information
                    LINEINFO_SIZE : INTEGER ;  // size of lineinfo

     /******************************************/
     /* felder fuer ueberschrift               */
     /******************************************/

                    LINECOUNT : INTEGER ;      // linecount f. heading
                    HEADLINE : SOURCELINE ;    // header line
                    HEADLINE_SIZE : INTEGER ;  // size of header line
                    PAGENR : INTEGER ;         // page number
                  end ;

     //************************************************************
     // Optionen fuer Compiler                                     
     //************************************************************
     // muss mit Def. beim Scanner                                 
     // uebereinstimmen                                            
     //************************************************************

     COMP_OPTIONS = record
                      LMARGIN : INTEGER ;
                      RMARGIN : INTEGER ;
                      PAGESIZE : INTEGER ;
                      LIST : BOOLEAN ;
                      PRCODE : BOOLEAN ;
                      GET_STAT : BOOLEAN ;
                      SAVEREGS : BOOLEAN ;
                      SAVEFPRS : BOOLEAN ;
                      DEBUG : BOOLEAN ;
                      MWARN : BOOLEAN ;
                      DEBUG_LEV : 0 .. 9 ;
                      NOPACKING : BOOLEAN ;
                      NESTCOMM : BOOLEAN ;
                      WARNING : BOOLEAN ;
                      ASSEMBLE : BOOLEAN ;
                      ASMVERB : BOOLEAN ;
                      CTROPTION : BOOLEAN ;
                    end ;


var EINGABE : TEXT ;
    AUSGABE : TEXT ;
    LISTING : TEXT ;
    UPSHIFT : array [ CHAR ] of CHAR ;
    MXINT2 : INTEGER ;
    MXINT10 : INTEGER ;
    MXINT16 : INTEGER ;
    ZZAUS : INTEGER ;

    /*********************************************/
    /* compiler options in new opt structure     */
    /*********************************************/

    OPT : COMP_OPTIONS ;
    SCB : SCAN_BLOCK ;

    (*******************************************************)
    (* RETURNED BY SOURCE PROGRAM SCANNER = PASSCAN        *)
    (*                                                     *)
    (* SY       - symbol read                              *)
    (* SYLENGTH - length of symbol or constant             *)
    (* VAL      - constant (if symbol was constant)        *)
    (*                                                     *)
    (* more symbols computed by insymbol locally           *)
    (*******************************************************)

    SY : SYMB ;
    SYLENGTH : INTEGER ;
    VAL : XCONSTANT ;
    LINECNT : INTEGER ;
    ID : ALPHA ;

    (*******************************************************)
    (* weitere rueckgabe von insymbol:                     *)
    (* w1 und w1ende - fuer outsymbol usw.                 *)
    (* nullstate     - false fuer empty stmt kontrolle     *)
    (*******************************************************)

    W1 : WORT ;
    W1ENDE : INTEGER ;
    NULLSTATE : BOOLEAN ;

    (*******************************************************)
    (* symbol sets                                         *)
    (*******************************************************)

    BLANKSYMBOLE : set of SYMB ;
    STERMSYMBOLE : set of SYMB ;
    TTERMSYMBOLE : set of SYMB ;
    WORTSYMBOLE : set of SYMB ;
    EXPRSYMBOLE : set of SYMB ;
    INC : INTEGER ;
    DICHT : BOOLEAN ;
    COMPOUNDNZ : BOOLEAN ;
    BLANKSVORHANDEN : BOOLEAN ;
    EINR : INTEGER ;
    EINRKOMM : INTEGER ;
    VERARB_MODUS : INTEGER ;
    WTABSQL : array [ 1 .. 30 ] of SWORT ;
    ANZSQLWORTE : INTEGER ;
    EOFILE : BOOLEAN ;
    S : SYMB ;
    OUTPOINTER : INTEGER ;
    INPOINTER : INTEGER ;
    NICHTLESEN : INTEGER ;
    ZZAUSVOR : INTEGER ;
    INSQLSTATE : BOOLEAN ;
    SQLHOSTV : BOOLEAN ;
    TRACEF : TEXT ;

    (*******************************************************)
    (*  KOMMC: AUFZEICHNEN DES LAUFENDEN KOMMENTAR-STATUS  *)
    (*******************************************************)

    KOMMC : KOMM_CONT ;


const BLANKID : ALPHA = '            ' ;
      HEXTAB : array [ 0 .. 15 ] of CHAR = '0123456789abcdef' ;
      LOW_LETTERS : SET_CHAR =
      [ 'a' .. 'i' , 'j' .. 'r' , 's' .. 'z' ] ;
      UP_LETTERS : SET_CHAR =
      [ 'A' .. 'I' , 'J' .. 'R' , 'S' .. 'Z' ] ;
      HEX_CHARS : SET_CHAR =
      [ 'a' .. 'f' , 'A' .. 'F' , '0' .. '9' ] ;
      BIN_CHARS : SET_CHAR =
      [ '0' .. '1' ] ;
      CONSTBEGSYS : SYMSET =
      [ SYPLUS , SYMINUS , INTCONST , REALCONST , STRINGCONST , IDENT ]
        ;
      SIMPTYPEBEGSYS : SYMSET =
      [ SYPLUS , SYMINUS , INTDOTDOT , INTCONST , REALCONST ,
        STRINGCONST , IDENT , SYLPARENT ] ;
      TYPEBEGSYS : SYMSET =
      [ SYARROW , SYPACKED , SYARRAY , SYRECORD , SYSET , SYFILE ,
        SYPLUS , SYMINUS , INTCONST , REALCONST , STRINGCONST , IDENT ,
        SYLPARENT ] ;
      TYPEDELS : SYMSET =
      [ SYARRAY , SYRECORD , SYSET , SYFILE , SYPACKED ] ;
      BLOCKBEGSYS : SYMSET =
      [ SYLABEL , SYCONST , SYTYPE , SYVAR , SYSTATIC , SYPROC , SYFUNC
        , SYLOCAL , SYBEGIN ] ;
      SELECTSYS : SYMSET =
      [ SYARROW , SYPERIOD , SYLBRACK , SYLPARENT ] ;
      FACBEGSYS : SYMSET =
      [ INTCONST , REALCONST , STRINGCONST , IDENT , SYLPARENT ,
        SYLBRACK , SYNOT ] ;
      STATBEGSYS : SYMSET =
      [ SYBEGIN , SYGOTO , SYIF , SYWHILE , SYREPEAT , SYFOR , SYWITH ,
        SYCASE , SYBREAK , SYCONTINUE , SYRETURN ] ;
      PROCCALLENDSYS : SYMSET =
      [ SYLPARENT , SYSEMICOLON , SYEND , SYELSE , SYUNTIL ] ;
      FACTOROPS : SYMSET =
      [ SYMULT , SYSLASH , SYDIV , SYMOD , SYAND ] ;
      TERMOPS : SYMSET =
      [ SYPLUS , SYMINUS , SYOR , SYXOR ] ;
      EXPROPS : SYMSET =
      [ SYEQOP , SYNEOP , SYGTOP , SYLTOP , SYGEOP , SYLEOP , SYIN ] ;

      (*********************************************************)
      (*   table of reserved symbols                           *)
      (*********************************************************)

      RW : array [ 1 .. MAXRW ] of ALPHA =

      (*********************************************************)
      (*   new reserved symbols in the 2011 version:           *)
      (*   break, return, continue                             *)
      (*********************************************************)

      ( 'IF          ' , 'DO          ' , 'OF          ' ,
        'TO          ' , 'IN          ' , 'OR          ' ,
        'END         ' , 'FOR         ' , 'VAR         ' ,
        'DIV         ' , 'MOD         ' , 'SET         ' ,
        'AND         ' , 'NOT         ' , 'XOR         ' ,
        'THEN        ' , 'ELSE        ' , 'WITH        ' ,
        'GOTO        ' , 'CASE        ' , 'TYPE        ' ,
        'FILE        ' , 'BEGIN       ' , 'UNTIL       ' ,
        'WHILE       ' , 'ARRAY       ' , 'CONST       ' ,
        'LABEL       ' , 'LOCAL       ' , 'BREAK       ' ,
        'REPEAT      ' , 'RECORD      ' , 'DOWNTO      ' ,
        'PACKED      ' , 'RETURN      ' , 'MODULE      ' ,
        'STATIC      ' , 'SQLEND      ' , 'SQLVAR      ' ,
        'FORWARD     ' , 'PROGRAM     ' , 'FORTRAN     ' ,
        'OVERLAY     ' , 'EXTERNAL    ' , 'FUNCTION    ' ,
        'CONTINUE    ' , 'SQLBEGIN    ' , 'PROCEDURE   ' ,
        'OTHERWISE   ' , '            ' , '            ' ,
        '            ' , '            ' , '            ' ,
        '            ' , '            ' , '            ' ,
        '            ' , '            ' , '            ' ) ;
      FRW : array [ 1 .. 12 ] of 1 .. MAXRW =

      (**********************************************************)
      (*  1  2  3    4    5    6    7    8    9   10   11   12  *)
      (**********************************************************)

      ( 1 , 1 , 7 , 16 , 23 , 31 , 40 , 44 , 48 , 50 , - 1 , - 1 ) ;

      (*********************************************************)
      (*   symbole zu RW Tabelle                               *)
      (*********************************************************)

      RSY : array [ 1 .. MAXRW ] of SYMB =
      ( SYIF , SYDO , SYOF , SYTO , SYIN , SYOR , SYEND , SYFOR , SYVAR
        , SYDIV , SYMOD , SYSET , SYAND , SYNOT , SYXOR , SYTHEN ,
        SYELSE , SYWITH , SYGOTO , SYCASE , SYTYPE , SYFILE , SYBEGIN ,
        SYUNTIL , SYWHILE , SYARRAY , SYCONST , SYLABEL , SYLOCAL ,
        SYBREAK , SYREPEAT , SYRECORD , SYDOWNTO , SYPACKED , SYRETURN
        , SYMODULE , SYSTATIC , SYSQLEND , SYSQLVAR , SYFORWARD ,
        SYPROG , SYFRTRN , SYOVERLAY , SYEXTRN , SYFUNC , SYCONTINUE ,
        SYSQLBEGIN , SYPROC , SYOTHERWISE , NOTUSED , NOTUSED , NOTUSED
        , NOTUSED , NOTUSED , NOTUSED , NOTUSED , NOTUSED , NOTUSED ,
        NOTUSED , NOTUSED ) ;



procedure PASSCANR ( var SCANINP : TEXT ; var SCANOUT : TEXT ; var SCB
                   : SCAN_BLOCK ; var CH : CHAR ) ;

   EXTERNAL ;



procedure PASSCANS ( var SCANOUT : TEXT ; var SCB : SCAN_BLOCK ) ;

   EXTERNAL ;



procedure PASSCANL ( var SCANINP : TEXT ; var SCANOUT : TEXT ; var SCB
                   : SCAN_BLOCK ; ALLES : BOOLEAN ) ;

   EXTERNAL ;



procedure PASSCANE ( var SCB : SCAN_BLOCK ; ERRLEVEL : CHAR ; ERRCLASS
                   : CHAR ; I : INTEGER ; INFO : CHAR32 ; ZEILNR :
                   INTEGER ; PLATZ : INTEGER ) ;

   EXTERNAL ;



procedure PASSCANF ( var SCB : SCAN_BLOCK ; WHICHTABLE : CHAR ;
                   ERRCLASS : SCAN_ERRCLASS ; ERRNUM : INTEGER ; ERRMSG
                   : SOURCELINE ; ERRMSGSIZE : INTEGER ) ;

   EXTERNAL ;



procedure PASSCAN ( var SCANINP : TEXT ; var SCANOUT : TEXT ; var SCB :
                  SCAN_BLOCK ; DO_COMMENT : BOOLEAN ) ;

   EXTERNAL ;



procedure HALTX ;

   begin (* HALTX *)
     EXIT ( 8 )
   end (* HALTX *) ;



procedure ERROR ( ERRNO : INTEGER ) ;

   begin (* ERROR *)
     WRITE ( '+++ FRM' , ERRNO : - 3 , ': ' ) ;
     case ERRNO of
       1 : WRITELN ( 'String does not fit into target line' ) ;
       2 : WRITELN ( 'EOF found when processing SQL statement' ) ;
       3 : WRITELN ( 'Syntax error detected' ) ;
       4 : WRITELN ( 'Syntax error detected' ) ;
       5 : WRITELN ( 'Empty statement not expected here' ) ;
       otherwise
         WRITELN ( 'unknown error' ) ;
     end (* case *) ;
     HALTX
   end (* ERROR *) ;



function MINOR ( CH : CHAR ) : CHAR ;

   begin (* MINOR *)
     if CH in [ 'A' .. 'I' , 'J' .. 'R' , 'S' .. 'Z' ] then
       MINOR := CHR ( ORD ( CH ) - ORD ( 'A' ) + ORD ( 'a' ) )
     else
       MINOR := CH
   end (* MINOR *) ;



procedure WRITEHEXBYTE ( var F : TEXT ; I : INTEGER ) ;

   begin (* WRITEHEXBYTE *)
     WRITE ( F , HEXTAB [ I DIV 16 ] , HEXTAB [ I MOD 16 ] ) ;
   end (* WRITEHEXBYTE *) ;



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



procedure NEUZEILEKOMM ( EIN : INTEGER ) ;

   begin (* NEUZEILEKOMM *)
     if EIN > MAXEINR then
       EIN := MAXEINR ;
     WRITELN ( AUSGABE ) ;
     ZZAUS := ZZAUS + 1 ;
     if EIN > 0 then
       begin
         WRITE ( AUSGABE , ' ' : EIN ) ;
         OUTPOINTER := EIN
       end (* then *)
     else
       OUTPOINTER := 0 ;
     BLANKSVORHANDEN := FALSE ;
   end (* NEUZEILEKOMM *) ;



procedure KOMMSTERNZEILE ;

   var I : INTEGER ;

   begin (* KOMMSTERNZEILE *)
     case KOMMC . KOMMTYPE of
       ')' : WRITE ( AUSGABE , '(*' ) ;
       '/' : if EINRKOMM = 0 then
               WRITE ( AUSGABE , '(*' )
             else
               WRITE ( AUSGABE , '/*' ) ;
       '}' : WRITE ( AUSGABE , '(*' ) ;
       '"' : WRITE ( AUSGABE , '"' ) ;
       '+' : WRITE ( AUSGABE , '//' ) ;
     end (* case *) ;
     for I := 1 to KOMMC . KOMM_WIDTH do
       WRITE ( AUSGABE , '*' ) ;
     if KOMMC . KOMMTYPE in [ '"' , '+' ] then
       WRITE ( AUSGABE , '**' ) ;
     case KOMMC . KOMMTYPE of
       ')' : WRITE ( AUSGABE , '*)' ) ;
       '/' : if EINRKOMM = 0 then
               WRITE ( AUSGABE , '*)' )
             else
               WRITE ( AUSGABE , '*/' ) ;
       '}' : WRITE ( AUSGABE , '*)' ) ;
       '"' : WRITE ( AUSGABE , '"' ) ;
       '+' : ;
     end (* case *) ;
   end (* KOMMSTERNZEILE *) ;



procedure READKOMM ( var KOMMC : KOMM_CONT ) ;

   var CH : CHAR ;
       I : INTEGER ;
       KLINE : -> KOMMLINE ;       // zeiger auf kommentarzeile
       ENDOFKOMM : BOOLEAN ;       // kommentarende erreicht
       ANZKOMM : INTEGER ;         // schachtelungstiefe kommentar
       ZUSTAND : INTEGER ;         // zustand fuer endlichen automat
       KOMMP : STRING ( 120 ) ;    // teil des kommentars
       KOMML : INTEGER ;           // akt. Laenge kommp

   begin (* READKOMM *)

     //**************************************************
     //   neu: es werden solange kommentare in die       
     //   neue komm_cont struktur eingelesen,            
     //   bis ein anderes Symbol kommt oder eine         
     //   leerzeile, die den Kommentar beendet           
     //**************************************************
     //   hier: auch rekursive Kommentare werden         
     //   grundsaetzlich akzeptiert (keine option!)      
     //**************************************************

     ENDOFKOMM := FALSE ;
     ANZKOMM := 1 ;
     ZUSTAND := 1 ;

     //**************************************************
     //   muss hier am Anfang weggespeichert werden,     
     //   weil es sich am ende evtl. aendert ...         
     //   symb_vor_komm wird beim weiterlesen am         
     //   ende des kommentars evtl. auf true gesetzt     
     //**************************************************

     if KOMMC . KLINE_ANKER = NIL then
       begin
         KOMMC . FIRST_IN_LINE := not KOMMC . SYMB_VOR_KOMM ;
         KOMMC . EINRRECHTS := KOMMC . LINEPOS - 1 ;
         if KOMMC . EINRRECHTS < OUTPOINTER - 1 then
           KOMMC . EINRRECHTS := OUTPOINTER - 1
       end (* then *) ;

     //**************************************************
     //   schleife, die solange laueft, bis keine        
     //   kommentare mehr gefunden werden -              
     //   oder leerzeile nach kommentar                  
     //**************************************************

     while not ENDOFKOMM do
       begin
         KOMMC . LAST_IN_LINE := FALSE ;
         KOMMP := REPEATSTR ( ' ' , 120 ) ;
         KOMML := 0 ;
         CH := SCB . LOOKAHEAD ;
         while not SCB . ENDOFLINE do
           begin
             KOMML := KOMML + 1 ;
             KOMMP [ KOMML ] := CH ;
             case KOMMC . KOMMTYPE of
               ')' : begin
                       case ZUSTAND of
                         1 : begin
                               if CH = '*' then
                                 ZUSTAND := 2
                               else
                                 if CH = '(' then
                                   ZUSTAND := 3
                             end (* tag/ca *) ;
                         2 : begin
                               if CH = '*' then
                                 ZUSTAND := 2
                               else
                                 if CH = ')' then
                                   begin
                                     ANZKOMM := ANZKOMM - 1 ;
                                     if ANZKOMM <= 0 then
                                       ENDOFKOMM := TRUE
                                     else
                                       ZUSTAND := 1
                                   end (* then *)
                                 else
                                   ZUSTAND := 1
                             end (* tag/ca *) ;
                         3 : begin
                               if CH = '*' then
                                 begin
                                   ZUSTAND := 1 ;
                                   ANZKOMM := ANZKOMM + 1
                                 end (* then *)
                               else
                                 ZUSTAND := 1
                             end (* tag/ca *) ;
                       end (* case *) ;
                     end (* tag/ca *) ;
               '/' : begin
                       case ZUSTAND of
                         1 : begin
                               if CH = '*' then
                                 ZUSTAND := 2
                               else
                                 if CH = '/' then
                                   ZUSTAND := 3
                             end (* tag/ca *) ;
                         2 : begin
                               if CH = '*' then
                                 ZUSTAND := 2
                               else
                                 if CH = '/' then
                                   begin
                                     ANZKOMM := ANZKOMM - 1 ;
                                     if ANZKOMM <= 0 then
                                       ENDOFKOMM := TRUE
                                     else
                                       ZUSTAND := 1
                                   end (* then *)
                                 else
                                   ZUSTAND := 1
                             end (* tag/ca *) ;
                         3 : begin
                               if CH = '*' then
                                 begin
                                   ZUSTAND := 1 ;
                                   ANZKOMM := ANZKOMM + 1
                                 end (* then *)
                               else
                                 ZUSTAND := 1
                             end (* tag/ca *) ;
                       end (* case *)
                     end (* tag/ca *) ;
               '}' : begin
                       if CH = '}' then
                         begin
                           ANZKOMM := ANZKOMM - 1 ;
                           if ANZKOMM <= 0 then
                             ENDOFKOMM := TRUE ;
                         end (* then *)
                       else
                         if CH = '{' then
                           ANZKOMM := ANZKOMM + 1
                     end (* tag/ca *) ;
               '"' : begin
                       ENDOFKOMM := ( CH = '"' ) ;
                     end (* tag/ca *) ;
               '+' : ;
             end (* case *) ;
             if KOMML >= 120 then
               break ;
             if ENDOFKOMM then
               break ;
             PASSCANR ( EINGABE , LISTING , SCB , CH ) ;
           end (* while *) ;

     //**************************************************
     //   zeilenende ist auch kommentarende bei          
     //   c++ kommentar                                  
     //**************************************************

         if SCB . ENDOFLINE then
           if KOMMC . KOMMTYPE = '+' then
             begin
               ENDOFKOMM := TRUE ;
               KOMMC . LAST_IN_LINE := TRUE
             end (* then *) ;

     //**************************************************
     // wenn Endekommentar, dann bei Kommentar hinten    
     // die Endezeichen wegmachen                        
     // Achtung: letztes Zeichen wurde noch gar nicht    
     // abgelegt                                         
     //**************************************************

         if ENDOFKOMM then
           case KOMMC . KOMMTYPE of
             '/' , ')' :
               KOMML := KOMML - 2 ;
             '}' , '"' :
               KOMML := KOMML - 1 ;
             '+' :
           end (* case *) ;
         if KOMMC . UEBERLESEN = 0 then
           begin

     //**************************************************
     // Kommentar in Komm_Cont Struktur eintragen        
     //**************************************************

             if FALSE then
               begin
                 WRITELN ( TRACEF , 'before new kline' ) ;
                 WRITELN ( TRACEF , 'komm_nlines   = ' , KOMMC .
                           KOMM_NLINES ) ;
                 WRITELN ( TRACEF , 'komml         = ' , KOMML ) ;
                 WRITELN ( TRACEF , 'kommp         = ' , '<' , KOMMP ,
                           '>' ) ;
               end (* then *) ;
             NEW ( KLINE ) ;
             if KOMMC . KLINE_ANKER = NIL then
               begin
                 KOMMC . KLINE_ANKER := KLINE ;
                 KOMMC . KLINE_AKT := KLINE
               end (* then *)
             else
               begin
                 KOMMC . KLINE_AKT -> . NEXT := KLINE ;
                 KOMMC . KLINE_AKT := KLINE
               end (* else *) ;
             KLINE -> . NEXT := NIL ;
             KLINE -> . L := SUBSTR ( KOMMP , 1 , KOMML ) ;
             KOMMC . KOMM_NLINES := KOMMC . KOMM_NLINES + 1 ;

     //**************************************************
     //   schauen, ob nur sterne enthalten sind          
     //   c++ kommentar                                  
     //**************************************************

             with KLINE -> do
               begin
                 NURSTERNE := TRUE ;
                 for I := 1 to LENGTH ( L ) do
                   if not ( L [ I ] in [ '*' , ' ' ] ) then
                     begin
                       NURSTERNE := FALSE ;
                       break
                     end (* then *)
               end (* with *)
           end (* then *)
         else
           KOMMC . UEBERLESEN := 0 ;

     //**************************************************
     //   wenn ende des kommentars nicht erreicht,       
     //   denn zeilenende, also scan wg. neuer zeile     
     //**************************************************

         if not ENDOFKOMM then
           begin
             PASSCANR ( EINGABE , LISTING , SCB , CH ) ;
             SCB . LOOKAHEAD := CH
           end (* then *)
       end (* while *) ;

     //**************************************************
     //   naechstes zeichen                              
     //   lesen und nach scb.lookahead                   
     //**************************************************

     if not SCB . ENDOFLINE then
       begin
         repeat
           PASSCANR ( EINGABE , LISTING , SCB , CH ) ;
           SCB . LOOKAHEAD := CH ;
           if not ( CH in [ ' ' , x'0a' , '#' ] ) then
             break ;
           if SCB . ENDOFLINE then
             begin
               KOMMC . LAST_IN_LINE := TRUE ;
               PASSCANR ( EINGABE , LISTING , SCB , CH ) ;
               SCB . LOOKAHEAD := CH ;
               break
             end (* then *)
         until FALSE
       end (* then *)
     else
       begin
         PASSCANR ( EINGABE , LISTING , SCB , CH ) ;
         SCB . LOOKAHEAD := CH
       end (* else *) ;

     //**************************************************
     //   traceausgaben                                  
     //**************************************************

     if FALSE then
       begin
         WRITELN ( TRACEF , '--------------------------------------'
                   '--------------------------------------' ) ;
         WRITELN ( TRACEF , 'kommtype      = ' , KOMMC . KOMMTYPE ) ;
         WRITELN ( TRACEF , 'komm_nlines   = ' , KOMMC . KOMM_NLINES )
                   ;
         WRITELN ( TRACEF , 'komm_width    = ' , KOMMC . KOMM_WIDTH ) ;
         WRITELN ( TRACEF , 'kline_anker   = ' , KOMMC . KLINE_ANKER )
                   ;
         WRITELN ( TRACEF , 'kline_akt     = ' , KOMMC . KLINE_AKT ) ;
         WRITELN ( TRACEF , 'scb.Lookahead = ' , SCB . LOOKAHEAD ) ;
         WRITELN ( TRACEF , 'scb.endofline = ' , SCB . ENDOFLINE ) ;
         KLINE := KOMMC . KLINE_ANKER ;
         while KLINE <> NIL do
           begin
             with KLINE -> do
               begin
                 WRITELN ( TRACEF , 'length/kline  = ' , LENGTH ( L ) )
                           ;
                 WRITELN ( TRACEF , 'kline         = ' , '<' , L , '>'
                           ) ;
                 WRITELN ( TRACEF , 'nursterne     = ' , NURSTERNE ) ;
               end (* with *) ;
             KLINE := KLINE -> . NEXT ;
           end (* while *) ;
       end (* then *)
   end (* READKOMM *) ;



procedure WRITEKOMM ( var KOMMC : KOMM_CONT ) ;

   var KLINE : -> KOMMLINE ;
       EINR_LOCAL : INTEGER ;
       BREITE : INTEGER ;
       REST : INTEGER ;
       ENDEKASTEN : BOOLEAN ;
       LENKOMM : INTEGER ;

   begin (* WRITEKOMM *)

     //************************************************************
     // zunaechst mal die maximale breite der                      
     // kommentare ermitteln                                       
     //************************************************************

     KOMMC . NEUZEILE_VORM := FALSE ;
     KOMMC . KOMM_WIDTH := 0 ;
     KLINE := KOMMC . KLINE_ANKER ;
     while KLINE <> NIL do
       begin
         BREITE := LENGTH ( KLINE -> . L ) ;
         if KOMMC . KOMM_WIDTH < BREITE then
           KOMMC . KOMM_WIDTH := BREITE ;
         KLINE := KLINE -> . NEXT ;
       end (* while *) ;
     case KOMMC . KOMMTYPE of
       ')' : KOMMC . KOMM_WIDTH_BR := KOMMC . KOMM_WIDTH + 4 ;
       '/' : KOMMC . KOMM_WIDTH_BR := KOMMC . KOMM_WIDTH + 4 ;
       '}' : KOMMC . KOMM_WIDTH_BR := KOMMC . KOMM_WIDTH + 4 ;
       '"' : KOMMC . KOMM_WIDTH_BR := KOMMC . KOMM_WIDTH + 2 ;
       '+' : KOMMC . KOMM_WIDTH_BR := KOMMC . KOMM_WIDTH + 2 ;
     end (* case *) ;

     //************************************************************
     // daten zum kommentar anzeigen                               
     //************************************************************

     if FALSE then
       begin
         WRITELN ( TRACEF , '--------------------------------------'
                   '--------------------------------------' ) ;
         WRITELN ( TRACEF , 'Status am Anfang von Writekomm -------'
                   '--------------------------------------' ) ;
         WRITELN ( TRACEF , 'einrkomm      = ' , EINRKOMM : 4 ) ;
         WRITELN ( TRACEF , 'einrrechts    = ' , KOMMC . EINRRECHTS ) ;
         WRITELN ( TRACEF , 'first.line    = ' , KOMMC . FIRST_IN_LINE
                   ) ;
         WRITELN ( TRACEF , 'last.line     = ' , KOMMC . LAST_IN_LINE )
                   ;
         WRITELN ( TRACEF , 'scb.Lookahead = ' , SCB . LOOKAHEAD ) ;
         WRITELN ( TRACEF , 'scb.endofline = ' , SCB . ENDOFLINE ) ;
         WRITELN ( TRACEF , 'outpointer    = ' , OUTPOINTER ) ;
         WRITELN ( TRACEF , 'kommtype      = ' , KOMMC . KOMMTYPE ) ;
         WRITELN ( TRACEF , 'komm_nlines   = ' , KOMMC . KOMM_NLINES )
                   ;
         WRITELN ( TRACEF , 'komm_width    = ' , KOMMC . KOMM_WIDTH ) ;
         WRITELN ( TRACEF , 'komm_width_br = ' , KOMMC . KOMM_WIDTH_BR
                   ) ;
         WRITELN ( TRACEF , 'kline_anker   = ' , KOMMC . KLINE_ANKER )
                   ;
         WRITELN ( TRACEF , 'kline_akt     = ' , KOMMC . KLINE_AKT ) ;
         WRITELN ( TRACEF , 'scb.Lookahead = ' , SCB . LOOKAHEAD ) ;
         WRITELN ( TRACEF , 'scb.endofline = ' , SCB . ENDOFLINE ) ;
         KLINE := KOMMC . KLINE_ANKER ;
         while KLINE <> NIL do
           begin
             with KLINE -> do
               begin
                 WRITELN ( TRACEF , 'length/kline  = ' , LENGTH ( L ) )
                           ;
                 WRITELN ( TRACEF , 'kline         = ' , '<' , L , '>'
                           ) ;
                 WRITELN ( TRACEF , 'nursterne     = ' , NURSTERNE ) ;
               end (* with *) ;
             KLINE := KLINE -> . NEXT ;
           end (* while *) ;
       end (* then *) ;

     //************************************************************
     // pruefen ob kommentar noch reinpasst in zeile               
     //************************************************************

     if not KOMMC . FIRST_IN_LINE then
       begin
         if OUTPOINTER + 1 + KOMMC . KOMM_WIDTH_BR > MAXLOUTPUT then
           KOMMC . FIRST_IN_LINE := TRUE
         else
           begin
             WRITE ( AUSGABE , ' ' ) ;
             OUTPOINTER := OUTPOINTER + 1 ;
           end (* else *)
       end (* then *) ;

     //************************************************************
     // wenn kommentar auf neue zeile, dann evtl. kasten           
     // sonst blanks bis einrrechts                                
     //************************************************************

     if KOMMC . FIRST_IN_LINE then
       begin
         KLINE := KOMMC . KLINE_ANKER ;
         if not KLINE -> . NURSTERNE then
           begin
             NEUZEILEKOMM ( 0 ) ;
             ENDEKASTEN := TRUE ;
             NEUZEILEKOMM ( EINRKOMM ) ;
             KOMMSTERNZEILE ;
             NEUZEILEKOMM ( EINRKOMM ) ;
           end (* then *)
         else
           begin
             NEUZEILEKOMM ( 0 ) ;
             ENDEKASTEN := FALSE ;
             NEUZEILEKOMM ( EINRKOMM ) ;
           end (* else *) ;
       end (* then *)
     else
       begin
         ENDEKASTEN := FALSE ;

     //************************************************************
     // wenn zuletzt gespeicherte einrueckung rechts               
     // aus letzter zeile stammt, dann diese beibehalten           
     // auch fuer spaeter                                          
     // sonst neue einrueckung setzen und diese behalten           
     //************************************************************

         if KOMMC . LAST_IN_LINE then
           begin
             if KOMMC . RECHTSLINE = KOMMC . LINENR - 1 then
               begin
                 KOMMC . EINRRECHTS := KOMMC . EINRRECHTS_SAVE ;
                 KOMMC . RECHTSLINE := KOMMC . LINENR
               end (* then *)
             else
               begin
                 KOMMC . EINRRECHTS_SAVE := KOMMC . EINRRECHTS ;
                 KOMMC . RECHTSLINE := KOMMC . LINENR
               end (* else *) ;
             EINR_LOCAL := KOMMC . EINRRECHTS - OUTPOINTER + 1 ;
             if EINR_LOCAL > 0 then
               begin
                 WRITE ( AUSGABE , ' ' : EINR_LOCAL ) ;
                 OUTPOINTER := OUTPOINTER + EINR_LOCAL
               end (* then *)
           end (* then *)
         else
           begin
             LENKOMM := 0 ;
             KLINE := KOMMC . KLINE_ANKER ;
             if KLINE <> NIL then
               LENKOMM := LENGTH ( KLINE -> . L ) + 4 ;
             if OUTPOINTER + 1 + LENKOMM < MAXLOUTPUT then
               begin
                 WRITE ( AUSGABE , ' ' ) ;
                 OUTPOINTER := OUTPOINTER + 1
               end (* then *)
             else
               begin
                 EINR_LOCAL := KOMMC . EINRRECHTS ;
                 NEUZEILEKOMM ( EINR_LOCAL ) ;
               end (* else *)
           end (* else *)
       end (* else *) ;

     //************************************************************
     // kommentar ausgeben                                         
     //************************************************************

     KLINE := KOMMC . KLINE_ANKER ;
     while KLINE <> NIL do
       begin
         with KLINE -> do
           begin
             OUTPOINTER := OUTPOINTER + 2 ;
             case KOMMC . KOMMTYPE of
               ')' : WRITE ( AUSGABE , '(*' ) ;
               '/' : WRITE ( AUSGABE , '/*' ) ;
               '}' : WRITE ( AUSGABE , '(*' ) ;
               '"' : begin
                       WRITE ( AUSGABE , '"' ) ;
                       OUTPOINTER := OUTPOINTER - 1 ;
                     end (* tag/ca *) ;
               '+' : WRITE ( AUSGABE , '//' ) ;
             end (* case *) ;
             WRITE ( AUSGABE , L ) ;
             OUTPOINTER := OUTPOINTER + LENGTH ( L ) ;
             REST := KOMMC . KOMM_WIDTH - LENGTH ( L ) ;
             if REST > 0 then
               WRITE ( AUSGABE , ' ' : REST ) ;
             OUTPOINTER := OUTPOINTER + 2 ;
             case KOMMC . KOMMTYPE of
               ')' : WRITE ( AUSGABE , '*)' ) ;
               '/' : WRITE ( AUSGABE , '*/' ) ;
               '}' : WRITE ( AUSGABE , '*)' ) ;
               '"' : begin
                       WRITE ( AUSGABE , '"' ) ;
                       OUTPOINTER := OUTPOINTER - 1 ;
                     end (* tag/ca *) ;
               '+' : OUTPOINTER := OUTPOINTER - 2 ;
             end (* case *) ;
           end (* with *) ;
         if KLINE -> . NEXT <> NIL then
           begin
             if not KOMMC . FIRST_IN_LINE then
               EINR_LOCAL := KOMMC . EINRRECHTS
             else
               EINR_LOCAL := EINRKOMM ;
             NEUZEILEKOMM ( EINR_LOCAL ) ;
           end (* then *) ;
         KLINE := KLINE -> . NEXT ;
       end (* while *) ;

     //************************************************************
     // ggf. kommentarendekasten ausgeben                          
     //************************************************************

     if ENDEKASTEN then
       begin
         NEUZEILEKOMM ( EINRKOMM ) ;
         KOMMSTERNZEILE ;
       end (* then *) ;

     //************************************************************
     // neue zeile, wenn kommentar am zeilenende war               
     //************************************************************

     if KOMMC . FIRST_IN_LINE then
       begin
         WRITELN ( AUSGABE ) ;
         ZZAUS := ZZAUS + 1 ;
         KOMMC . NEUZEILE_VORM := TRUE ;
       end (* then *)
     else
       begin
         if KOMMC . LAST_IN_LINE then
           KOMMC . NEUZEILE_VORM := TRUE ;
       end (* else *)
   end (* WRITEKOMM *) ;



procedure WORK_KOMMENTAR ( var KOMMC : KOMM_CONT ) ;

(********************************************************************)
(*                                                                  *)
(*   August 2019 - KOMMENTARLOGIK komplett neu                      *)
(*                                                                  *)
(********************************************************************)


   begin (* WORK_KOMMENTAR *)
     if DICHT then
       return ;

     //*****************************************************
     //   wenn Kommentar vor prozedur, dann entsprechend    
     //   viele Leerzeilen vor kommentar                    
     //*****************************************************

     if KOMMC . KOMM_VOR_PROC then
       begin
         NEUZEILEKOMM ( 0 ) ;
         NEUZEILEKOMM ( 0 ) ;
         NEUZEILEKOMM ( 0 ) ;
         NEUZEILEKOMM ( 0 ) ;
         KOMMC . KOMM_VOR_PROC := FALSE ;
         KOMMC . FIRST_IN_LINE := TRUE ;
       end (* then *) ;

     //**********************************************
     //   AUSGEBEN DES KOMMENTARS                    
     //**********************************************

     WRITEKOMM ( KOMMC ) ;
   end (* WORK_KOMMENTAR *) ;



procedure INSYMBOL ;

(**************************************************************)
(*                                                            *)
(*   READ NEXT BASIS SYMB OF SOURCE PROGRAM AND RETURN        *)
(*   ITS DESCRIPTION IN THE GLOBAL VARIABLES                  *)
(*   SY, OP, ID, VAL AND SYLENGTH                             *)
(*                                                            *)
(*------------------------------------------------------------*)
(*                                                            *)
(*   REWORKED 24.10.2011 - BERND OPPOLZER                     *)
(*                                                            *)
(*   ADDED THE FOLLOWING SYMB SPELLINGS:                      *)
(*                                                            *)
(*   (. AND .) AS ANOTHER POSSIBILITY FOR [ ] AND (/ /)       *)
(*                                                            *)
(*   -> AS AN ALTERNATIVE FOR @                               *)
(*                                                            *)
(*   COMMENTS ALSO LIKE THIS: /* ... COMMENT ... */           *)
(*                                                            *)
(**************************************************************)


   var I , K : INTEGER ;
       DIGIT : array [ 1 .. 40 ] of CHAR ;
       XSTRING : array [ 1 .. MAXSTRL ] of CHAR ;


   procedure MODSTRING ( STRTYPE : CHAR ; var L : INTEGER ) ;

      var IX : INTEGER ;
          LNEU : INTEGER ;
          L1 : INTEGER ;
          X : INTEGER ;
          X2 : INTEGER ;
          INEU : INTEGER ;
          SX : INTEGER ;
          TX : INTEGER ;

      begin (* MODSTRING *)
        if FALSE then
          begin
            WRITE ( TRACEF , 'MODSTRING: ' , STRTYPE , ' ' , L : 1 ,
                    ' <' ) ;
            for I := 1 to L do
              WRITE ( TRACEF , XSTRING [ I ] ) ;
            WRITELN ( TRACEF , '>' ) ;
          end (* then *) ;
        case STRTYPE of
          'C' : begin
                  SX := 1 ;
                  TX := 0 ;
                  while SX <= L do
                    begin
                      if ( XSTRING [ SX ] <> '''' ) or ( SX = L ) then
                        begin
                          TX := TX + 1 ;
                          if TX < SX then
                            XSTRING [ TX ] := XSTRING [ SX ] ;
                          SX := SX + 1
                        end (* then *)
                      else
                        if XSTRING [ SX + 1 ] = '''' then
                          begin
                            TX := TX + 1 ;
                            if TX < SX then
                              XSTRING [ TX ] := XSTRING [ SX ] ;
                            SX := SX + 2
                          end (* then *)
                    end (* while *) ;
                  L := TX ;
                end (* tag/ca *) ;
          'B' : begin

        /**********************/
        /* remove underscores */
        /**********************/

                  LNEU := 0 ;
                  for X := 1 to L do
                    if XSTRING [ X ] <> '_' then
                      begin
                        LNEU := LNEU + 1 ;
                        if LNEU < X then
                          XSTRING [ LNEU ] := XSTRING [ X ] ;
                      end (* then *) ;
                  L := LNEU ;

        /**************************************/
        /* l = new length without underscores */
        /**************************************/

                  IX := 1 ;
                  LNEU := ( L + 7 ) DIV 8 ;

        /********************************************/
        /* lneu = length of converted target string */
        /********************************************/

                  L1 := L MOD 8 ;
                  if L1 = 0 then
                    L1 := 8 ;
                  for X := 1 to LNEU do
                    begin
                      INEU := 0 ;
                      for X2 := 1 to L1 do
                        begin
                          INEU := INEU * 2 ;
                          if XSTRING [ IX ] = '1' then
                            INEU := INEU + 1 ;
                          IX := IX + 1 ;
                        end (* for *) ;
                      L1 := 8 ;
                      XSTRING [ X ] := CHR ( INEU ) ;
                    end (* for *) ;
                  L := LNEU ;
                end (* tag/ca *) ;
          'X' : begin
                  LNEU := 0 ;
                  for X := 1 to L do
                    if XSTRING [ X ] <> '_' then
                      begin
                        LNEU := LNEU + 1 ;
                        if LNEU < X then
                          XSTRING [ LNEU ] := XSTRING [ X ] ;
                      end (* then *) ;
                  L := LNEU ;

        /**************************************/
        /* l = new length without underscores */
        /**************************************/

                  IX := 1 ;
                  LNEU := ( L + 1 ) DIV 2 ;

        /********************************************/
        /* lneu = length of converted target string */
        /********************************************/

                  L1 := L MOD 2 ;
                  if L1 = 0 then
                    L1 := 2 ;
                  for X := 1 to LNEU do
                    begin
                      INEU := 0 ;
                      for X2 := 1 to L1 do
                        begin
                          INEU := INEU * 16 ;
                          if XSTRING [ IX ] in [ '1' .. '9' ] then
                            INEU := INEU + ORD ( XSTRING [ IX ] ) - ORD
                                    ( '0' )
                          else
                            if XSTRING [ IX ] in [ 'A' .. 'F' ] then
                              INEU := INEU + ORD ( XSTRING [ IX ] ) -
                                      ORD ( 'A' ) + 10
                            else
                              if XSTRING [ IX ] in [ 'a' .. 'f' ] then
                                INEU := INEU + ORD ( XSTRING [ IX ] ) -
                                        ORD ( 'a' ) + 10 ;
                          IX := IX + 1 ;
                        end (* for *) ;
                      L1 := 2 ;
                      XSTRING [ X ] := CHR ( INEU ) ;
                    end (* for *) ;
                  L := LNEU ;
                end (* tag/ca *)
        end (* case *) ;
        if FALSE then
          begin
            WRITE ( TRACEF , 'ENDE MODS: ' , STRTYPE , ' ' , L : 1 ,
                    ' <' ) ;
            for I := 1 to L do
              if STRTYPE = 'C' then
                WRITE ( TRACEF , XSTRING [ I ] )
              else
                WRITEHEXBYTE ( TRACEF , ORD ( XSTRING [ I ] ) ) ;
            WRITELN ( TRACEF , '>' ) ;
          end (* then *)
      end (* MODSTRING *) ;


   procedure CHECK_KOMMENTARVORH ;

   //****************************************************************
   // bei neuem kommentar(teil) pruefen,                             
   // ob evtl. seit letztem kommentar(teil)                          
   // eine leerzeile vorhanden ist oder war -                        
   // falls ja, diesen bestehenden kommentar abschliessen            
   //****************************************************************


      begin (* CHECK_KOMMENTARVORH *)
        if SCB . LINENR - KOMMC . LINENR > 1 then
          if KOMMC . KOMM_NLINES > 0 then
            begin
              WORK_KOMMENTAR ( KOMMC ) ;
              KOMMC . KOMM_NLINES := 0 ;
              KOMMC . KLINE_ANKER := NIL ;
              KOMMC . KLINE_AKT := NIL ;
            end (* then *)
      end (* CHECK_KOMMENTARVORH *) ;


   begin (* INSYMBOL *)
     VAL . IVAL := 0 ;
     VAL . STRTYPE := ' ' ;

     //************************************************************
     // kommentarinformationen hier auf anfangszustand             
     // ggf. mehrere kommentare zusammenfassen                     
     //************************************************************

     KOMMC . KOMM_NLINES := 0 ;
     KOMMC . KLINE_ANKER := NIL ;
     KOMMC . KLINE_AKT := NIL ;

     (**********************************************************)
     (*   schleife, z.b. wg. blanks und kommentaren            *)
     (**********************************************************)

     while TRUE do
       begin

     (**********************************************************)
     (*   scanner aufrufen (externes modul)                    *)
     (**********************************************************)

         PASSCAN ( EINGABE , LISTING , SCB , FALSE ) ;

     (**********************************************************)
     (*   variablen sy und sylength setzen (rueckg. scanner)   *)
     (**********************************************************)

         SY := SCB . SYMBOLNR ;
         SYLENGTH := SCB . LSYMBOL ;
         LINECNT := SCB . LINENR ;
         if FALSE then
           WRITELN ( TRACEF , 'nach passcan: sy = ' , SY : 20 ,
                     ' zeile/spalte = ' , SCB . LINENR : 5 , SCB .
                     LINEPOS : 5 , KOMMC . LINENR : 5 ) ;

     //************************************************************
     // check, if still on same line                               
     // (compare, if lastscbline is still valid)                   
     // if not, reset switch "symb_vor_komm"                       
     // if symbol found which is no comment,                       
     // set switch "symb_vor_komm"                                 
     //************************************************************

         if KOMMC . LASTSCBLINE <> SCB . LINENR then
           begin
             KOMMC . LASTSCBLINE := SCB . LINENR ;
             KOMMC . SYMB_VOR_KOMM := FALSE ;
           end (* then *) ;
         if not ( SY in [ SEPARATOR , COMMENT1 , COMMENT2 , COMMENT3 ,
         COMMENT4 , COMMENT5 ] ) then
           begin
             KOMMC . SYMB_VOR_KOMM := TRUE ;
           end (* then *) ;

     (*****************************************************)
     (*   look what has to be done depending on symbol    *)
     (*   (some symbols need additional work)             *)
     (*****************************************************)

         case SY of

     (**********************************************************)
     (*   separator und kommentare ignorieren und nochmal      *)
     (**********************************************************)

           SEPARATOR :
             continue ;
           COMMENT1 :
             begin
               CHECK_KOMMENTARVORH ;
               KOMMC . KOMMTYPE := '/' ;
               KOMMC . LINENR := SCB . LINENR ;
               KOMMC . LINEPOS := SCB . LINEPOS - 2 ;
               READKOMM ( KOMMC ) ;
               continue ;
             end (* tag/ca *) ;
           COMMENT2 :
             begin
               CHECK_KOMMENTARVORH ;
               KOMMC . KOMMTYPE := ')' ;
               KOMMC . LINENR := SCB . LINENR ;
               KOMMC . LINEPOS := SCB . LINEPOS - 2 ;
               READKOMM ( KOMMC ) ;
               continue ;
             end (* tag/ca *) ;
           COMMENT3 :
             begin
               CHECK_KOMMENTARVORH ;
               KOMMC . KOMMTYPE := '}' ;
               KOMMC . LINENR := SCB . LINENR ;
               KOMMC . LINEPOS := SCB . LINEPOS - 2 ;
               READKOMM ( KOMMC ) ;
               continue ;
             end (* tag/ca *) ;
           COMMENT4 :
             begin
               CHECK_KOMMENTARVORH ;
               KOMMC . KOMMTYPE := '"' ;
               KOMMC . LINENR := SCB . LINENR ;
               KOMMC . LINEPOS := SCB . LINEPOS - 2 ;
               READKOMM ( KOMMC ) ;
               continue ;
             end (* tag/ca *) ;
           COMMENT5 :
             begin
               CHECK_KOMMENTARVORH ;
               KOMMC . KOMMTYPE := '+' ;
               KOMMC . LINENR := SCB . LINENR ;
               KOMMC . LINEPOS := SCB . LINEPOS - 2 ;
               READKOMM ( KOMMC ) ;
               continue ;
             end (* tag/ca *) ;

     (**********************************************************)
     (*   ident in grossbuchstaben und gegen tabelle der       *)
     (*   reservierten worte abchecken                         *)
     (**********************************************************)

           IDENT : begin
                     ID := ' ' ;
                     K := SYLENGTH ;
                     if K > IDLNGTH then
                       K := IDLNGTH ;
                     MEMCPY ( ADDR ( ID ) , ADDR ( SCB . SYMBOL ) , K )
                              ;
                     for I := 1 to K do
                       ID [ I ] := UPSHIFT [ ID [ I ] ] ;
                     MEMCPY ( ADDR ( SCB . SYMBOL ) , ADDR ( ID ) , K )
                              ;

     (**********************************************************)
     (*   maxrwlen = laenge des laengsten reservierten wortes  *)
     (*   die tabelle frw ist nur so lang                      *)
     (**********************************************************)

                     if K <= MAXRWLEN then
                       for I := FRW [ K ] to FRW [ K + 1 ] - 1 do
                         if RW [ I ] = ID then
                           begin
                             SY := RSY [ I ] ;
                             break ;
                           end (* then *) ;
                   end (* tag/ca *) ;

     (**********************************************************)
     (*   stringconst nacharbeiten wg. doppelter               *)
     (*   hochkommas z.B.                                      *)
     (**********************************************************)

           STRINGCONST :
             begin
               K := SYLENGTH - 2 ;
               MEMCPY ( ADDR ( XSTRING ) , ADDR ( SCB . SYMBOL [ 2 ] )
                        , K ) ;
               MODSTRING ( 'C' , K ) ;
               VAL . STRTYPE := ' ' ;
               SYLENGTH := K ;
               if SYLENGTH = 0 then
                 VAL . IVAL := ORD ( ' ' )
               else
                 if SYLENGTH = 1 then
                   VAL . IVAL := ORD ( XSTRING [ 1 ] )
                 else
                   begin
                     if SYLENGTH > MAXSTRL then
                       begin
                         ERROR ( 398 ) ;
                         SYLENGTH := MAXSTRL
                       end (* then *) ;
                     NEW ( VAL . SVAL ) ;
                     VAL . SVAL -> . TAG := 'S' ;
                     VAL . SVAL -> . LENGTH := SYLENGTH ;
                     VAL . SVAL -> . SSTR := XSTRING ;
                   end (* else *)
             end (* tag/ca *) ;

     (**********************************************************)
     (*   hex stringconst umcodieren                           *)
     (**********************************************************)

           HEXSTRINGCONST :
             begin
               K := SYLENGTH - 3 ;
               MEMCPY ( ADDR ( XSTRING ) , ADDR ( SCB . SYMBOL [ 3 ] )
                        , K ) ;
               MODSTRING ( 'X' , K ) ;
               VAL . STRTYPE := 'X' ;
               SYLENGTH := K ;
               if SYLENGTH = 0 then
                 begin
                   VAL . IVAL := ORD ( ' ' ) ;
                   SYLENGTH := 1 ;
                 end (* then *)
               else
                 if SYLENGTH = 1 then
                   VAL . IVAL := ORD ( XSTRING [ 1 ] )
                 else
                   begin
                     if SYLENGTH > MAXSTRL then
                       begin
                         ERROR ( 398 ) ;
                         SYLENGTH := MAXSTRL
                       end (* then *) ;
                     NEW ( VAL . SVAL ) ;
                     VAL . SVAL -> . TAG := 'S' ;
                     VAL . SVAL -> . LENGTH := SYLENGTH ;
                     VAL . SVAL -> . SSTR := XSTRING ;
                   end (* else *) ;
               SY := STRINGCONST ;
             end (* tag/ca *) ;

     (**********************************************************)
     (*   bin stringconst umcodieren                           *)
     (**********************************************************)

           BINSTRINGCONST :
             begin
               K := SYLENGTH - 3 ;
               MEMCPY ( ADDR ( XSTRING ) , ADDR ( SCB . SYMBOL [ 3 ] )
                        , K ) ;
               MODSTRING ( 'B' , K ) ;
               VAL . STRTYPE := 'X' ;
               SYLENGTH := K ;
               if SYLENGTH = 0 then
                 begin
                   VAL . IVAL := ORD ( ' ' ) ;
                   SYLENGTH := 1 ;
                 end (* then *)
               else
                 if SYLENGTH = 1 then
                   VAL . IVAL := ORD ( XSTRING [ 1 ] )
                 else
                   begin
                     if SYLENGTH > MAXSTRL then
                       begin
                         ERROR ( 398 ) ;
                         SYLENGTH := MAXSTRL
                       end (* then *) ;
                     NEW ( VAL . SVAL ) ;
                     VAL . SVAL -> . TAG := 'S' ;
                     VAL . SVAL -> . LENGTH := SYLENGTH ;
                     VAL . SVAL -> . SSTR := XSTRING ;
                   end (* else *) ;
               SY := STRINGCONST ;
             end (* tag/ca *) ;

     (**********************************************************)
     (*   intconst kann hex oder binaer sein ...               *)
     (**********************************************************)

           INTCONST , INTDOTDOT :
             begin
               DIGIT := ' ' ;
               K := SYLENGTH ;
               if SY = INTDOTDOT then
                 K := K - 2 ;
               if K > SIZEOF ( DIGIT ) then
                 K := SIZEOF ( DIGIT ) ;
               MEMCPY ( ADDR ( DIGIT ) , ADDR ( SCB . SYMBOL ) , K ) ;
               VAL . IVAL := 0 ;

     (***********************************************)
     (*   if hex const, translate to integer / ival *)
     (***********************************************)

               if ( DIGIT [ 2 ] = 'X' ) or ( DIGIT [ 2 ] = 'x' ) then
                 begin
                   with VAL do
                     for I := 3 to K do
                       if IVAL <= MXINT16 then
                         case DIGIT [ I ] of
                           '0' .. '9' :
                             IVAL := IVAL * 16 + ORD ( DIGIT [ I ] ) -
                                     ORD ( '0' ) ;
                           'A' .. 'F' :
                             IVAL := IVAL * 16 + ORD ( DIGIT [ I ] ) -
                                     ORD ( 'A' ) + 10 ;
                           'a' .. 'f' :
                             IVAL := IVAL * 16 + ORD ( DIGIT [ I ] ) -
                                     ORD ( 'a' ) + 10 ;
                           '_' : ;
                         end (* case *)
                       else
                         begin
                           ERROR ( 203 ) ;
                           IVAL := 0 ;
                           break
                         end (* else *)
                 end (* then *)
               else

     (***********************************************)
     (*   if bin const, translate to integer / ival *)
     (***********************************************)

                 if ( DIGIT [ 2 ] = 'B' ) or ( DIGIT [ 2 ] = 'b' ) then
                   begin
                     with VAL do
                       for I := 3 to K do
                         if IVAL <= MXINT2 then
                           case DIGIT [ I ] of
                             '0' : IVAL := IVAL * 2 ;
                             '1' : IVAL := IVAL * 2 + 1 ;
                             '_' : ;
                           end (* case *)
                         else
                           begin
                             ERROR ( 203 ) ;
                             IVAL := 0 ;
                             break
                           end (* else *)
                   end (* then *)
                 else
                   begin

     (*************************)
     (*   normal int constant *)
     (*************************)

                     with VAL do
                       for I := 1 to K do
                         if DIGIT [ I ] <> '_' then
                           if IVAL <= MXINT10 then
                             IVAL := IVAL * 10 + ( ORD ( DIGIT [ I ] )
                                     - ORD ( '0' ) )
                           else
                             begin
                               ERROR ( 203 ) ;
                               IVAL := 0 ;
                               break
                             end (* else *)
                   end (* else *)
             end (* tag/ca *) ;

     (**********************************)
     (*   realconst ...                *)
     (**********************************)

           REALCONST :
             begin
               DIGIT := ' ' ;
               K := SYLENGTH ;
               if K > SIZEOF ( DIGIT ) then
                 K := SIZEOF ( DIGIT ) ;
               MEMCPY ( ADDR ( DIGIT ) , ADDR ( SCB . SYMBOL ) , K ) ;
               VAL . RVAL := ' ' ;
               if K <= DIGMAX then
                 for I := 2 to K + 1 do
                   VAL . RVAL [ I ] := DIGIT [ I - 1 ]
               else
                 begin
                   ERROR ( 203 ) ;
                   UNPACK ( '0.0' , VAL . RVAL , 2 )
                 end (* else *)
             end (* tag/ca *) ;
           otherwise
             
         end (* case *) ;

     (**********************************************************)
     (*   endlosschleife jetzt beenden - wenn nicht vorher     *)
     (*   schon mit continue eine wiederholung angefordert     *)
     (*   wurde                                                *)
     (**********************************************************)

         break ;
       end (* while *) ;

     (**********************************************************)
     (*   debug trace                                          *)
     (**********************************************************)

     if FALSE then
       begin
         WRITELN ( TRACEF ) ;
         WRITELN ( TRACEF , 'rueckgabe vom scanner:' ) ;
         WRITELN ( TRACEF , 'sy       = ' , SY ) ;
         WRITELN ( TRACEF , 'sylength = ' , SYLENGTH : 1 ) ;
       end (* then *) ;

     (**********************************************************)
     (*   hier kommentare ausgeben                             *)
     (**********************************************************)

     if KOMMC . KOMM_NLINES > 0 then
       WORK_KOMMENTAR ( KOMMC ) ;

     (**********************************************************)
     (*   unexpected eof is a fatal error                      *)
     (**********************************************************)

     KOMMC . UEBERLESEN := 0 ;
     NULLSTATE := FALSE ;
     W1 := ' ' ;
     W1ENDE := 0 ;
     if SY <> SYMB_EOF then
       begin
         W1ENDE := SCB . LSYMBOL ;
         if W1ENDE > 100 then
           W1ENDE := 100 ;
         MEMCPY ( ADDR ( W1 ) , ADDR ( SCB . SYMBOL ) , W1ENDE ) ;
         if SY = INTDOTDOT then
           begin

     (************************************************************)
     (*   insert blank into int ..                               *)
     (************************************************************)

             W1ENDE := W1ENDE + 1 ;
             W1 [ W1ENDE - 2 ] := ' ' ;
             W1 [ W1ENDE ] := '.' ;
           end (* then *)
       end (* then *) ;
   end (* INSYMBOL *) ;



procedure NEUZEILE ( BLANKS : BOOLEAN ) ;

   var EIN : INTEGER ;

   begin (* NEUZEILE *)
     EIN := EINR ;
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

     //************************************************************
     // neuzeile_vorm loeschen                                     
     // von kommentar getriggerte leerzeile                        
     // soll nur geschrieben werden, wenn nicht schon              
     // eine andere leerzeile sowieso erzeugt wird                 
     //************************************************************

     KOMMC . NEUZEILE_VORM := FALSE ;
   end (* NEUZEILE *) ;



procedure PLATZZEILE ( PLATZBEDARF : INTEGER ) ;

   var EIN : INTEGER ;

   begin (* PLATZZEILE *)
     EIN := EINR ;
     if EIN > MAXEINR then
       EIN := MAXEINR ;
     if PLATZBEDARF > MAXLOUTPUT then
       ERROR ( 1 )
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



procedure OUTSYMBOL ( S : SYMB ) ;

   var I : INTEGER ;
       PLATZBEDARF : INTEGER ;
       CH : CHAR ;

   begin (* OUTSYMBOL *)

     //************************************************************
     // neuzeile angefordert wg. KOmmentar                         
     //************************************************************

     if KOMMC . NEUZEILE_VORM then
       begin
         NEUZEILE ( TRUE ) ;
         KOMMC . NEUZEILE_VORM := FALSE ;
       end (* then *) ;

     (*****************************)
     (* SYMBOLE GGF. MODIFIZIEREN *)
     (*****************************)

     case S of
       SYARROW :
         begin
           W1 [ 1 ] := '-' ;
           W1 [ 2 ] := '>' ;
           W1ENDE := 2 ;
         end (* tag/ca *) ;
       SYLBRACK :
         begin
           W1 [ 1 ] := '[' ;
           W1ENDE := 1 ;
         end (* tag/ca *) ;
       SYRBRACK :
         begin
           W1 [ 1 ] := ']' ;
           W1ENDE := 1 ;
         end (* tag/ca *) ;
       otherwise
         
     end (* case *) ;
     if not DICHT then
       begin
         PLATZBEDARF := OUTPOINTER + W1ENDE + 1 ;
         if ( S = SYCOLON ) and INSQLSTATE then
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
         SQLHOSTV := ( S = SYCOLON ) and INSQLSTATE ;
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
     if S = SYSEMICOLON then
       begin
         OUTSYMBOL ( S ) ;
         repeat
           INSYMBOL ;
           S := SY
         until S <> SYSEMICOLON
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
     if S = SYSEMICOLON then
       begin
         OUTSYMBOL ( S ) ;
         repeat
           INSYMBOL ;
           S := SY ;
         until S <> SYSEMICOLON
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
     INSYMBOL ;
     S := SY ;
     EINR := EINR + W1ENDE + 1 ;
     OUTSYMBOL ( S ) ;
     INSYMBOL ;
     S := SY ;
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
       INSYMBOL ;
       S := SY ;
       if S = SYMB_EOF then
         ERROR ( 2 ) ;
       if S = SYSEMICOLON then
         WRITELN ( '+++ Warnung: Strichpunkt in SQL-Einschub.' ) ;
     until ( S = SYSQLEND ) or ( S = SYSEMICOLON ) ;
     EINR := EINRZWEI ;
     EINR := EINR - 9 ;
     INSQLSTATE := FALSE ;
     if S <> SYSEMICOLON then
       begin
         NEUZEILE ( TRUE ) ;
         OUTSYMBOL ( S ) ;
         INSYMBOL ;
         S := SY ;
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
            ERROR ( 3 ) ;
          end (* then *)
      end (* CSFEHLERTEST *) ;


   begin (* CSIMPSTATE *)
     EINRSAVE := EINR ;
     repeat
       OUTSYMBOL ( S ) ;
       INSYMBOL ;
       S := SY ;
       CSFEHLERTEST
     until ( S = SYASSIGN ) or ( S = SYLPARENT ) or ( S in STERMSYMBOLE
     ) ;
     if not ( S in STERMSYMBOLE ) then
       begin
         OUTSYMBOL ( S ) ;
         EINR := OUTPOINTER ;
         INSYMBOL ;
         S := SY ;
         while not ( S in STERMSYMBOLE ) do
           begin
             CSFEHLERTEST ;
             OUTSYMBOL ( S ) ;
             INSYMBOL ;
             S := SY
           end (* while *)
       end (* then *) ;
     STRIPUTEST ;
     EINR := EINRSAVE
   end (* CSIMPSTATE *) ;



procedure CTYPE ( var MORE_LINES : BOOLEAN ) ;

   var EINRSAVE : INTEGER ;
       ANZKLAMAUF : INTEGER ;
       ZZAUS_START : INTEGER ;


   procedure FIELDLIST ( VORSCHUB : BOOLEAN ) ;

      var EINRS2 : INTEGER ;
          VS : BOOLEAN ;
          DUMMYB : BOOLEAN ;

      begin (* FIELDLIST *)
        VS := VORSCHUB ;
        while ( S <> SYEND ) and ( S <> SYRPARENT ) do
          begin
            if VS then
              NEUZEILE ( TRUE ) ;
            VS := TRUE ;
            if S = SYCASE then
              begin
                repeat
                  OUTSYMBOL ( S ) ;
                  INSYMBOL ;
                  S := SY
                until S = SYOF ;
                OUTSYMBOL ( S ) ;
                EINR := EINR + INC ;
                INSYMBOL ;
                S := SY ;
                repeat
                  if ( S <> SYEND ) and ( S <> SYRPARENT ) then
                    begin
                      if S = SYSEMICOLON then
                        begin
                          NEUZEILE ( TRUE ) ;
                          OUTSYMBOL ( S ) ;
                          INSYMBOL ;
                          S := SY
                        end (* then *)
                      else
                        begin
                          NEUZEILE ( TRUE ) ;
                          repeat
                            OUTSYMBOL ( S ) ;
                            INSYMBOL ;
                            S := SY ;
                          until S = SYLPARENT ;
                          EINRS2 := EINR ;
                          EINR := EINR + INC ;
                          NEUZEILE ( TRUE ) ;
                          OUTSYMBOL ( S ) ;
                          EINR := EINR + INC ;
                          INSYMBOL ;
                          S := SY ;
                          repeat
                            if S <> SYRPARENT then
                              FIELDLIST ( FALSE ) ;
                            if S <> SYRPARENT then
                              NEUZEILE ( TRUE )
                          until S = SYRPARENT ;
                          OUTSYMBOL ( S ) ;
                          INSYMBOL ;
                          S := SY ;
                          STRIPUTEST ;
                          EINR := EINRS2
                        end (* else *)
                    end (* then *)
                until ( S = SYEND ) or ( S = SYRPARENT ) ;
                EINR := EINR - INC
              end (* then *)
            else
              begin
                repeat
                  OUTSYMBOL ( S ) ;
                  INSYMBOL ;
                  S := SY
                until S = SYCOLON ;
                OUTSYMBOL ( S ) ;
                INSYMBOL ;
                S := SY ;
                CTYPE ( DUMMYB ) ;
                STRIPUTEST
              end (* else *)
          end (* while *)
      end (* FIELDLIST *) ;


   begin (* CTYPE *)
     MORE_LINES := FALSE ;
     if S <> SYEND then
       begin
         EINRSAVE := EINR ;
         EINR := OUTPOINTER ;

     //**********************************************
     // store zzaus to check, if more than one line  
     // written during type processing               
     //**********************************************

         ZZAUS_START := ZZAUS ;
         ANZKLAMAUF := 0 ;
         if S = SYLPARENT then
           begin
             TTERMSYMBOLE := TTERMSYMBOLE - [ SYRPARENT ] ;
             ANZKLAMAUF := ANZKLAMAUF + 1 ;
           end (* then *) ;
         while not ( S in TTERMSYMBOLE ) do
           begin
             OUTSYMBOL ( S ) ;
             INSYMBOL ;
             S := SY ;
             if S = SYLPARENT then
               begin
                 TTERMSYMBOLE := TTERMSYMBOLE - [ SYRPARENT ] ;
                 ANZKLAMAUF := ANZKLAMAUF + 1 ;
               end (* then *) ;
             if S = SYRPARENT then
               begin
                 ANZKLAMAUF := ANZKLAMAUF - 1 ;
                 if ANZKLAMAUF < 0 then
                   TTERMSYMBOLE := TTERMSYMBOLE + [ SYRPARENT ] ;
               end (* then *) ;
           end (* while *) ;
         TTERMSYMBOLE := TTERMSYMBOLE + [ SYRPARENT ] ;
         if S = SYSEMICOLON then
           begin
             OUTSYMBOL ( S ) ;
             INSYMBOL ;
             S := SY ;
             EINR := EINRSAVE ;
           end (* then *)
         else
           if S = SYRECORD then
             begin
               EINR := EINRSAVE ;
               EINRSAVE := EINR ;
               EINR := OUTPOINTER ;
               OUTSYMBOL ( S ) ;
               EINR := EINR + INC ;
               INSYMBOL ;
               S := SY ;
               FIELDLIST ( TRUE ) ;
               EINR := EINR - INC ;
               NEUZEILE ( TRUE ) ;
               OUTSYMBOL ( S ) ;
               INSYMBOL ;
               S := SY ;
               STRIPUTEST ;
               EINR := EINRSAVE ;
             end (* then *)
           else
             EINR := EINRSAVE ;
         if ZZAUS > ZZAUS_START then
           MORE_LINES := TRUE ;
       end (* then *)
   end (* CTYPE *) ;



procedure CSTATE ( FSYS : SYMSET ; AUFNEUEZEILE : BOOLEAN ; KENNUNG :
                 FELD6 ) ;

   var EINRSAVE : INTEGER ;
       LAUFX : INTEGER ;

   begin (* CSTATE *)
     if ( S = SYBEGIN ) and not COMPOUNDNZ then
       AUFNEUEZEILE := FALSE ;
     if AUFNEUEZEILE then
       NEUZEILE ( TRUE ) ;

     (*********************************)
     (* leeres statement              *)
     (*********************************)

     if S = SYSEMICOLON then
       begin
         OUTSYMBOL ( S ) ;
         INSYMBOL ;
         S := SY ;
         return
       end (* then *) ;

     (*********************************)
     (* prozeduraufruf                *)
     (*********************************)

     if S = IDENT then
       begin
         CSIMPSTATE ;
         return
       end (* then *) ;

     (*********************************)
     (* break (neu)                   *)
     (*********************************)

     if S = SYBREAK then
       begin
         CSIMPSTATE ;
         return
       end (* then *) ;

     (*********************************)
     (* continue (neu)                *)
     (*********************************)

     if S = SYCONTINUE then
       begin
         CSIMPSTATE ;
         return
       end (* then *) ;

     (*********************************)
     (* return (neu)                  *)
     (*********************************)

     if S = SYRETURN then
       begin
         CSIMPSTATE ;
         return
       end (* then *) ;

     (*********************************)
     (* sql-Einschub                  *)
     (*********************************)

     if S = SYSQLBEGIN then
       begin
         SQLSTATE ;
         return
       end (* then *) ;

     (*********************************)
     (* Statement mit label           *)
     (*********************************)

     if S = INTCONST then
       begin
         repeat
           OUTSYMBOL ( S ) ;
           INSYMBOL ;
           S := SY
         until S = SYCOLON ;
         OUTSYMBOL ( S ) ;
         INSYMBOL ;
         S := SY ;
         CSTATE ( FSYS , TRUE , '      ' ) ;
         return
       end (* then *) ;

     (*********************************)
     (* andere, je nach typ           *)
     (*********************************)

     if S = SYBEGIN then
       begin
         OUTSYMBOL ( S ) ;
         if COMPOUNDNZ then
           EINR := EINR + INC ;
         INSYMBOL ;
         S := SY ;
         repeat
           CSTATE ( FSYS , TRUE , '      ' )
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
         INSYMBOL ;
         S := SY ;
         STRIPUTEST
       end (* then *)
     else
       if S = SYCASE then
         begin
           repeat
             OUTSYMBOL ( S ) ;
             INSYMBOL ;
             S := SY
           until S = SYOF ;
           OUTSYMBOL ( S ) ;
           EINR := EINR + INC ;
           INSYMBOL ;
           S := SY ;
           while not ( S in ( STERMSYMBOLE - [ SYELSE , SYOTHERWISE ] )
           ) do
             begin
               NEUZEILE ( TRUE ) ;
               if S = SYELSE then
                 begin
                   OUTSYMBOL ( S ) ;
                   INSYMBOL ;
                   S := SY ;
                   EINR := EINR + INC ;
                   NEUZEILE ( TRUE ) ;
                   CSTATE ( FSYS , FALSE , 'else/c' ) ;
                   EINR := EINR - INC ;
                 end (* then *)
               else
                 if S = SYOTHERWISE then
                   begin
                     OUTSYMBOL ( S ) ;
                     INSYMBOL ;
                     S := SY ;
                     EINR := EINR + INC ;
                     NEUZEILE ( TRUE ) ;
                     CSTATE ( FSYS , FALSE , 'otherw' ) ;
                     EINR := EINR - INC ;
                   end (* then *)
                 else
                   begin
                     while S <> SYCOLON do
                       begin
                         OUTSYMBOL ( S ) ;
                         INSYMBOL ;
                         S := SY
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
                     INSYMBOL ;
                     S := SY ;
                     CSTATE ( FSYS , FALSE , 'tag/ca' ) ;
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
           INSYMBOL ;
           S := SY ;
           STRIPUTEST
         end (* then *)
       else
         if ( S = SYFOR ) or ( S = SYWHILE ) or ( S = SYWITH ) then
           begin
             if S = SYFOR then
               KENNUNG := 'for   ' ;
             if S = SYWHILE then
               KENNUNG := 'while ' ;
             if S = SYWITH then
               KENNUNG := 'with  ' ;
             repeat
               OUTSYMBOL ( S ) ;
               INSYMBOL ;
               S := SY
             until S = SYDO ;
             OUTSYMBOL ( S ) ;
             EINR := EINR + INC ;
             INSYMBOL ;
             S := SY ;
             CSTATE ( FSYS , TRUE , KENNUNG ) ;
             STRIPUTEST ;
             EINR := EINR - INC ;
           end (* then *)
         else
           if S = SYGOTO then
             CSIMPSTATE
           else
             if S = SYIF then
               begin
                 repeat
                   OUTSYMBOL ( S ) ;
                   INSYMBOL ;
                   S := SY
                 until S = SYTHEN ;
                 OUTSYMBOL ( S ) ;
                 EINR := EINR + INC ;
                 INSYMBOL ;
                 S := SY ;
                 CSTATE ( FSYS , TRUE , 'then  ' ) ;
                 EINR := EINR - INC ;
                 if S = SYELSE then
                   begin
                     NEUZEILE ( TRUE ) ;
                     OUTSYMBOL ( S ) ;
                     EINR := EINR + INC ;
                     INSYMBOL ;
                     S := SY ;
                     CSTATE ( FSYS , TRUE , 'else  ' ) ;
                     STRIPUTEST ;
                     EINR := EINR - INC ;
                   end (* then *)
                 else
                   STRIPUTEST
               end (* then *)
             else
               if S = SYREPEAT then
                 begin
                   OUTSYMBOL ( S ) ;
                   EINR := EINR + INC ;
                   INSYMBOL ;
                   S := SY ;
                   repeat
                     CSTATE ( FSYS , TRUE , '      ' )
                   until ( S in STERMSYMBOLE ) ;
                   EINR := EINR - INC ;
                   NEUZEILE ( TRUE ) ;
                   repeat
                     OUTSYMBOL ( S ) ;
                     INSYMBOL ;
                     S := SY
                   until ( S in STERMSYMBOLE ) ;
                   STRIPUTEST
                 end (* then *)
               else
                 begin
                   if NULLSTATE then
                     ERROR ( 5 )
                   else
                     NULLSTATE := TRUE
                 end (* else *)
   end (* CSTATE *) ;



procedure CBODY ( FSYS : SYMSET ; FUELLWORT : PWORT ; BLOCKLEVEL :
                INTEGER ) ;

   var I : INTEGER ;
       EINRKOMMSAVE : INTEGER ;

   begin (* CBODY *)
     if S = SYBEGIN then
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
         INSYMBOL ;
         S := SY ;
         repeat
           CSTATE ( FSYS , TRUE , '      ' )
         until ( S in STERMSYMBOLE ) ;
         EINR := EINR - INC ;
         NEUZEILE ( TRUE ) ;
         OUTSYMBOL ( S ) ;
         WRITE ( AUSGABE , ' (* ' ) ;
         for I := 1 to FUELLWORT . LAENGE do
           WRITE ( AUSGABE , FUELLWORT . NAME [ I ] ) ;
         WRITE ( AUSGABE , ' *)' ) ;
         KOMMC . UEBERLESEN := 2 ;
         INSYMBOL ;
         S := SY ;
         EINRKOMM := EINRKOMMSAVE ;
         KOMM_ZWISCHEN_PROCS ( BLOCKLEVEL ) ;
       end (* then *)
     else
       if S in [ SYEXTRN , SYFRTRN , SYFORWARD ] then
         begin
           EINRKOMMSAVE := EINRKOMM ;
           NEUZEILE ( TRUE ) ;
           EINR := EINR + INC ;
           EINRKOMM := EINR ;
           OUTSYMBOL ( S ) ;
           INSYMBOL ;
           S := SY ;
           if S in [ IDENT , SYFRTRN , STRINGCONST ] then
             begin
               OUTSYMBOL ( S ) ;
               INSYMBOL ;
               S := SY ;
             end (* then *) ;
           if S = STRINGCONST then
             begin
               OUTSYMBOL ( S ) ;
               INSYMBOL ;
               S := SY ;
             end (* then *) ;
           if S = SYSEMICOLON then
             begin
               OUTSYMBOL ( S ) ;
               INSYMBOL ;
               S := SY ;
             end (* then *) ;
           EINR := EINR - INC ;
           EINRKOMM := EINRKOMMSAVE ;
           KOMM_ZWISCHEN_PROCS ( BLOCKLEVEL ) ;
         end (* then *)
       else
         ERROR ( 4 )
   end (* CBODY *) ;



procedure CBLOCK ( FSYS : SYMSET ; FUELLWORT : PWORT ; BLOCKLEVEL :
                 INTEGER ) ;

   var EINRSAVE : INTEGER ;
       EINRKOMMSAVE : INTEGER ;
       KLAMMZ : INTEGER ;
       I : INTEGER ;
       PROCWORT : PWORT ;
       SALT : SYMB ;
       INCR : INTEGER ;
       PROC_VORH : BOOLEAN ;
       DUMMYB : BOOLEAN ;


   procedure HANDLE_CONST ;

      var EINRSAVE : INTEGER ;
          TYPE_MORE_LINES : BOOLEAN ;

      begin (* HANDLE_CONST *)
        NEUZEILE ( FALSE ) ;
        if BLOCKLEVEL = 1 then
          NEUZEILE ( FALSE ) ;
        NEUZEILE ( TRUE ) ;
        OUTSYMBOL ( S ) ;
        EINRKOMM := OUTPOINTER ;
        INSYMBOL ;
        S := SY ;
        EINR := EINR + 6 ;
        while S = IDENT do
          begin
            INCR := 0 ;
            repeat
              OUTSYMBOL ( S ) ;
              SALT := S ;
              INSYMBOL ;
              S := SY ;
            until S in [ SYCOLON , SYEQOP , SYASSIGN , SYSEMICOLON ] ;

        //**********************************************
        // wenn Doppelpunkt folgt, dann hat die         
        // Konstante einen Typ                          
        //**********************************************

            EINRSAVE := EINR ;
            if S = SYCOLON then
              begin
                OUTSYMBOL ( S ) ;
                EINR := OUTPOINTER ;
                INSYMBOL ;
                S := SY ;
                CTYPE ( TYPE_MORE_LINES ) ;
                if not TYPE_MORE_LINES then
                  EINR := EINRSAVE ;
              end (* then *) ;
            repeat
              OUTSYMBOL ( S ) ;

        //**********************************************
        // wenn nach gleichheitszeichen eine runde oder 
        // eckige klammer folgt, dann neuzeile          
        //**********************************************

              SALT := S ;
              INSYMBOL ;
              S := SY ;
              if ( SALT in [ SYEQOP , SYASSIGN ] ) and ( S in [
              SYLPARENT , SYLBRACK ] ) then
                begin
                  NEUZEILE ( TRUE ) ;
                  INCR := INCR + 2 ;
                  EINR := EINR + 2 ;
                end (* then *) ;
            until S = SYSEMICOLON ;
            OUTSYMBOL ( S ) ;
            EINR := EINRSAVE ;
            INSYMBOL ;
            S := SY ;
            if S = IDENT then
              NEUZEILE ( TRUE )
          end (* while *) ;
        EINR := EINR - 6 ;
      end (* HANDLE_CONST *) ;


   procedure HANDLE_VAR ;

      var EINRSAVE : INTEGER ;

      begin (* HANDLE_VAR *)
        NEUZEILE ( FALSE ) ;
        if BLOCKLEVEL = 1 then
          NEUZEILE ( FALSE ) ;
        NEUZEILE ( TRUE ) ;
        OUTSYMBOL ( S ) ;
        EINRKOMM := OUTPOINTER ;
        INSYMBOL ;
        S := SY ;
        EINR := EINR + 4 ;
        while S = IDENT do
          begin
            INCR := 0 ;
            repeat
              OUTSYMBOL ( S ) ;
              SALT := S ;
              INSYMBOL ;
              S := SY ;
            until S in [ SYCOLON , SYEQOP , SYASSIGN , SYSEMICOLON ] ;
            if S <> SYCOLON then
              ERROR ( 6 ) ;
            EINRSAVE := EINR ;
            OUTSYMBOL ( S ) ;
            EINR := OUTPOINTER ;
            INSYMBOL ;
            S := SY ;
            CTYPE ( DUMMYB ) ;

        //**********************************************
        // nach CTYPE ist S entweder =, :=              
        // oder IDENT (oder eines der Symbole,          
        // das einen neuen Abschnitt einleitet)         
        //**********************************************

            if ( S in [ SYEQOP , SYASSIGN ] ) then
              begin
                repeat
                  OUTSYMBOL ( S ) ;

        //**********************************************
        // wenn nach gleichheitszeichen eine runde oder 
        // eckige klammer folgt, dann neuzeile          
        //**********************************************

                  SALT := S ;
                  INSYMBOL ;
                  S := SY ;
                  if ( SALT in [ SYEQOP , SYASSIGN ] ) and ( S in [
                  SYLPARENT , SYLBRACK ] ) then
                    begin
                      NEUZEILE ( TRUE ) ;
                      INCR := INCR + 2 ;
                      EINR := EINR + 2 ;
                    end (* then *) ;
                until S = SYSEMICOLON ;

        //**********************************************
        // hier weiterlesen, weil die globale schleife  
        // IDENT oder eines der Wortsymbole erwartet    
        //**********************************************

                OUTSYMBOL ( S ) ;
                INSYMBOL ;
                S := SY ;
              end (* then *) ;
            EINR := EINRSAVE ;
            if S = IDENT then
              NEUZEILE ( TRUE )
          end (* while *) ;
        EINR := EINR - 4 ;
      end (* HANDLE_VAR *) ;


   procedure HANDLE_SQLVAR ;

      begin (* HANDLE_SQLVAR *)
        NEUZEILE ( FALSE ) ;
        if BLOCKLEVEL = 1 then
          NEUZEILE ( FALSE ) ;
        NEUZEILE ( TRUE ) ;
        OUTSYMBOL ( S ) ;
        EINRKOMM := OUTPOINTER ;
        INSYMBOL ;
        S := SY ;
        EINR := EINR + 7 ;
        while S = IDENT do
          begin
            repeat
              OUTSYMBOL ( S ) ;
              INSYMBOL ;
              S := SY
            until S = SYCOLON ;
            OUTSYMBOL ( S ) ;
            INSYMBOL ;
            S := SY ;
            CTYPE ( DUMMYB ) ;
            if S = IDENT then
              NEUZEILE ( TRUE )
          end (* while *) ;
        EINR := EINR - 7 ;
      end (* HANDLE_SQLVAR *) ;


   procedure HANDLE_STATIC ;

      var EINRSAVE : INTEGER ;

      begin (* HANDLE_STATIC *)
        NEUZEILE ( FALSE ) ;
        if BLOCKLEVEL = 1 then
          NEUZEILE ( FALSE ) ;
        NEUZEILE ( TRUE ) ;
        OUTSYMBOL ( S ) ;
        EINRKOMM := OUTPOINTER ;
        INSYMBOL ;
        S := SY ;
        EINR := EINR + 7 ;
        while S = IDENT do
          begin
            INCR := 0 ;
            repeat
              OUTSYMBOL ( S ) ;
              SALT := S ;
              INSYMBOL ;
              S := SY ;
            until S in [ SYCOLON , SYEQOP , SYASSIGN , SYSEMICOLON ] ;
            if S <> SYCOLON then
              ERROR ( 6 ) ;
            EINRSAVE := EINR ;
            OUTSYMBOL ( S ) ;
            EINR := OUTPOINTER ;
            INSYMBOL ;
            S := SY ;
            CTYPE ( DUMMYB ) ;

        //**********************************************
        // nach CTYPE ist S entweder =, :=              
        // oder IDENT (oder eines der Symbole,          
        // das einen neuen Abschnitt einleitet)         
        //**********************************************

            if ( S in [ SYEQOP , SYASSIGN ] ) then
              begin
                repeat
                  OUTSYMBOL ( S ) ;

        //**********************************************
        // wenn nach gleichheitszeichen eine runde oder 
        // eckige klammer folgt, dann neuzeile          
        //**********************************************

                  SALT := S ;
                  INSYMBOL ;
                  S := SY ;
                  if ( SALT in [ SYEQOP , SYASSIGN ] ) and ( S in [
                  SYLPARENT , SYLBRACK ] ) then
                    begin
                      NEUZEILE ( TRUE ) ;
                      INCR := INCR + 2 ;
                      EINR := EINR + 2 ;
                    end (* then *) ;
                until S = SYSEMICOLON ;

        //**********************************************
        // hier weiterlesen, weil die globale schleife  
        // IDENT oder eines der Wortsymbole erwartet    
        //**********************************************

                OUTSYMBOL ( S ) ;
                INSYMBOL ;
                S := SY ;
              end (* then *) ;
            EINR := EINRSAVE ;
            if S = IDENT then
              NEUZEILE ( TRUE )
          end (* while *) ;
        EINR := EINR - 7 ;
      end (* HANDLE_STATIC *) ;


   begin (* CBLOCK *)
     EINRKOMMSAVE := EINRKOMM ;
     EINRKOMM := EINR ;
     while TRUE do
       begin
         if S = SYLABEL then
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
             INSYMBOL ;
             S := SY ;
             while S <> SYSEMICOLON do
               begin
                 OUTSYMBOL ( S ) ;
                 INSYMBOL ;
                 S := SY
               end (* while *) ;
             OUTSYMBOL ( S ) ;
             INSYMBOL ;
             S := SY ;
             EINR := EINRSAVE ;
             continue ;
           end (* then *) ;
         if S = SYTYPE then
           begin
             NEUZEILE ( FALSE ) ;
             if BLOCKLEVEL = 1 then
               NEUZEILE ( FALSE ) ;
             NEUZEILE ( TRUE ) ;
             OUTSYMBOL ( S ) ;
             EINRKOMM := OUTPOINTER ;
             INSYMBOL ;
             S := SY ;
             EINR := EINR + 5 ;
             while S = IDENT do
               begin
                 repeat
                   OUTSYMBOL ( S ) ;
                   INSYMBOL ;
                   S := SY
                 until S = SYEQOP ;
                 OUTSYMBOL ( S ) ;
                 INSYMBOL ;
                 S := SY ;
                 CTYPE ( DUMMYB ) ;
                 if S = IDENT then
                   NEUZEILE ( TRUE )
               end (* while *) ;
             EINR := EINR - 5 ;
             continue ;
           end (* then *) ;
         if S = SYCONST then
           begin
             HANDLE_CONST ;
             continue ;
           end (* then *) ;
         if S = SYSQLVAR then
           begin
             HANDLE_SQLVAR ;
             continue ;
           end (* then *) ;
         if S = SYVAR then
           begin
             HANDLE_VAR ;
             continue ;
           end (* then *) ;
         if S = SYSTATIC then
           begin
             HANDLE_STATIC ;
             continue ;
           end (* then *) ;
         break ;
       end (* while *) ;
     PROC_VORH := FALSE ;
     while ( S = SYPROC ) or ( S = SYFUNC ) or ( S = SYOVERLAY ) or ( S
     = SYLOCAL ) do
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
         while ( S = SYOVERLAY ) or ( S = SYLOCAL ) do
           begin
             OUTSYMBOL ( S ) ;
             INSYMBOL ;
             S := SY ;
           end (* while *) ;
         OUTSYMBOL ( S ) ;
         INSYMBOL ;
         S := SY ;
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
           INSYMBOL ;
           S := SY ;
           if S = SYLPARENT then
             begin
               KLAMMZ := KLAMMZ + 1 ;
               if KLAMMZ = 1 then
                 begin
                   EINR := OUTPOINTER
                 end (* then *)
             end (* then *) ;
           if S = SYRPARENT then
             KLAMMZ := KLAMMZ - 1
         until ( S = SYSEMICOLON ) and ( KLAMMZ = 0 ) ;
         OUTSYMBOL ( S ) ;
         INSYMBOL ;
         S := SY ;
         EINR := EINRSAVE ;
         EINR := EINR + 3 ;
         CBLOCK ( FSYS , PROCWORT , BLOCKLEVEL + 1 ) ;
         EINR := EINR - 3 ;
         if BLOCKLEVEL = 1 then
           NEUZEILE ( FALSE ) ;
       end (* while *) ;
     NEUZEILE ( FALSE ) ;
     if ( BLOCKLEVEL = 1 ) or PROC_VORH then
       NEUZEILE ( FALSE ) ;
     CBODY ( FSYS , FUELLWORT , BLOCKLEVEL ) ;
     EINRKOMM := EINRKOMMSAVE
   end (* CBLOCK *) ;



procedure PROG ;

   var S : SYMB ;

   begin (* PROG *)
     repeat
       INSYMBOL ;
       S := SY ;
       OUTSYMBOL ( S ) ;
       if S = SYEND then
         KOMMC . UEBERLESEN := 2
     until EOFILE
   end (* PROG *) ;



procedure CHANGE ( FSYS : SYMSET ) ;

   var EINRSAVE : INTEGER ;
       I : INTEGER ;
       MAINWORT : PWORT ;

   begin (* CHANGE *)
     MAINWORT . NAME := 'HAUPTPROGRAMM  ' ;
     MAINWORT . LAENGE := 13 ;
     EINRSAVE := EINR ;
     INSYMBOL ;
     S := SY ;
     OUTSYMBOL ( S ) ;
     if ( S = SYPROG ) or ( S = SYMODULE ) then
       begin
         MAINWORT . NAME := 'HAUPTPROGRAMM  ' ;
         MAINWORT . LAENGE := 13
       end (* then *)
     else
       begin
         INSYMBOL ;
         S := SY ;
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
     INSYMBOL ;
     S := SY ;
     repeat
       OUTSYMBOL ( S ) ;
       if S = SYLPARENT then
         begin
           EINR := OUTPOINTER ;
           repeat
             INSYMBOL ;
             S := SY ;
             OUTSYMBOL ( S )
           until S = SYRPARENT ;
         end (* then *) ;
       INSYMBOL ;
       S := SY
     until S = SYSEMICOLON ;
     OUTSYMBOL ( S ) ;
     INSYMBOL ;
     S := SY ;
     EINR := EINRSAVE ;
     CBLOCK ( FSYS , MAINWORT , 1 ) ;
     OUTSYMBOL ( S ) ;
     if TRUE then
       begin
         INSYMBOL ;
         S := SY ;
         if S <> SYMB_EOF then
           WRITELN ( '+++ Warnung: Es wurde nicht die' ,
                     ' ganze Datei verarbeitet.' )
       end (* then *)
   end (* CHANGE *) ;



procedure VORBESETZEN ;

   const ORDCHMAX = 255 ;

   var CH : CHAR ;

   begin (* VORBESETZEN *)
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

     (***************************************)
     (*   useful initializations            *)
     (***************************************)

     for CH := CHR ( 0 ) to CHR ( ORDCHMAX ) do
       UPSHIFT [ CH ] := CH ;

     (***************************************)
     (*   an old comment told about some    *)
     (*   troubles with upshift, because    *)
     (*   some meaningful chars (to pascal) *)
     (*   are in the letter range 'a' to    *)
     (*   'z', but are no letters ...       *)
     (*   see the EBCDIC letter gaps.       *)
     (*   I avoided those problems by       *)
     (*   the three loops below             *)
     (*   - bernd oppolzer (2016)           *)
     (***************************************)

     for CH := 'a' to 'i' do
       begin
         UPSHIFT [ CH ] := CHR ( ORD ( CH ) - ORD ( 'a' ) + ORD ( 'A' )
                           ) ;
       end (* for *) ;
     for CH := 'j' to 'r' do
       begin
         UPSHIFT [ CH ] := CHR ( ORD ( CH ) - ORD ( 'a' ) + ORD ( 'A' )
                           ) ;
       end (* for *) ;
     for CH := 's' to 'z' do
       begin
         UPSHIFT [ CH ] := CHR ( ORD ( CH ) - ORD ( 'a' ) + ORD ( 'A' )
                           ) ;
       end (* for *) ;

     (***************************************)
     (*   useful initializations            *)
     (***************************************)

     MXINT2 := MAXINT DIV 2 ;
     MXINT10 := MAXINT DIV 10 ;
     MXINT16 := MAXINT DIV 16 ;
     NULLSTATE := FALSE ;
   end (* VORBESETZEN *) ;



procedure INIT_SCANNER ;

   var CTEMP : array [ 1 .. 16 ] of CHAR ;

   begin (* INIT_SCANNER *)

     (****************************************)
     (* default values for compiler options  *)
     (* and: init control blocks for scanner *)
     (****************************************)

     MEMSET ( ADDR ( SCB ) , CHR ( 0 ) , SIZEOF ( SCB ) ) ;
     MEMSET ( ADDR ( OPT ) , CHR ( 0 ) , SIZEOF ( OPT ) ) ;
     OPT . LMARGIN := 0 ;
     OPT . RMARGIN := 80 ;
     OPT . PAGESIZE := 72 ;
     OPT . LIST := TRUE ;
     OPT . PRCODE := TRUE ;
     OPT . GET_STAT := TRUE ;
     OPT . SAVEREGS := TRUE ;
     OPT . SAVEFPRS := TRUE ;
     OPT . DEBUG := TRUE ;
     OPT . MWARN := FALSE ;
     OPT . DEBUG_LEV := 2 ;
     OPT . NOPACKING := FALSE ;
     OPT . NESTCOMM := FALSE ;
     OPT . WARNING := TRUE ;
     OPT . ASSEMBLE := FALSE ;
     OPT . ASMVERB := FALSE ;
     OPT . CTROPTION := FALSE ;
     SCB . MAXLSYMBOL := MAXLSIZE ;
     SCB . MODUS := 1 ;
     SCB . SYMBOLNR := SYMB_UNKNOWN ;
     SCB . SLINE := ' ' ;
     SCB . LINENR := 0 ;
     SCB . LINEPOS := 1 ;
     SCB . LINELEN := 0 ;
     SCB . SKIPPING := FALSE ;
     SCB . FEANFANG := NIL ;
     SCB . FTTAB := NIL ;
     SCB . FTTABA := NIL ;
     SCB . POPT := ADDR ( OPT ) ;
     SCB . FEAKT := NIL ;
     SCB . FEAKT_ALT := NIL ;

     (********************************************)
     (* listing is printed by scanner, too       *)
     (* so all needed information has to be      *)
     (* provided in the scanner control block    *)
     (********************************************)
     (* and terminal output in case of error     *)
     (********************************************)

     SCB . PROTOUT := TRUE ;
     SCB . TERMOUT := TRUE ;
     SCB . LINEINFO := ' ' ;
     SCB . LINEINFO_SIZE := 0 ;
     SCB . HEADLINE := '1LINE #   Pascal Source '
                       'Formatting Program - Version of MM.YYYY'
                       '    hh:mm:ss  DD/MM/YYYY' ;
     CTEMP := VERSION ;
     MEMCPY ( ADDR ( SCB . HEADLINE [ 57 ] ) , ADDR ( CTEMP ) , 7 ) ;
     MEMCPY ( ADDR ( SCB . HEADLINE [ 68 ] ) , ADDR ( TIME ) , 8 ) ;
     MEMCPY ( ADDR ( SCB . HEADLINE [ 78 ] ) , ADDR ( DATE ) , 10 ) ;
     SCB . HEADLINE_SIZE := 100 ;

     (****************************************)
     (* linecount high to force heading      *)
     (* on first insymbol call               *)
     (****************************************)

     SCB . LINECOUNT := 100 ;
   end (* INIT_SCANNER *) ;



begin (* HAUPTPROGRAMM *)
  REWRITE ( TRACEF ) ;
  VERARB_MODUS := 4 ;
  KOMMC . LASTSCBLINE := - 1 ;
  KOMMC . KOMMTYPE := ' ' ;
  KOMMC . KOMM_VOR_PROC := FALSE ;
  KOMMC . LINENR := - 1 ;
  KOMMC . LINEPOS := - 1 ;
  KOMMC . KOMM_NLINES := 0 ;
  KOMMC . KOMM_WIDTH := 0 ;
  KOMMC . KLINE_ANKER := NIL ;
  KOMMC . KLINE_AKT := NIL ;
  KOMMC . NEUZEILE_VORM := FALSE ;

  (***********************************************)
  (*   HIER UNTERSCHIED PASCAL/VS ZU TURBO/3     *)
  (*   TERMIN (INPUT) ; TERMOUT (OUTPUT) ;       *)
  (*   WIRD BENOETIGT BEI PASCAL/VS              *)
  (***********************************************)

  COMPOUNDNZ := TRUE ;
  if VERARB_MODUS > 4 then
    begin
      COMPOUNDNZ := FALSE ;
      VERARB_MODUS := VERARB_MODUS - 2
    end (* then *) ;
  DICHT := ( VERARB_MODUS = 1 ) or ( VERARB_MODUS = 3 ) ;
  VORBESETZEN ;
  STERMSYMBOLE := [ SYMB_EOF , SYSEMICOLON , SYELSE , SYEND ,
                  SYOTHERWISE , SYUNTIL ] ;
  TTERMSYMBOLE := [ SYMB_EOF , SYSEMICOLON , SYEND , SYRECORD ,
                  SYRPARENT , SYEQOP , SYASSIGN ] ;
  BLANKSYMBOLE := [ IDENT , INTCONST , SYAND , SYARRAY , SYBEGIN ,
                  SYBREAK , SYCASE , SYCONST , SYCONTINUE , SYDO ,
                  SYELSE , SYEND , SYFOR , SYFUNC , SYGOTO , SYIF ,
                  SYIN , SYLABEL , SYLOCAL , SYMODULE , SYNOT , SYOF ,
                  SYOR , SYOTHERWISE , SYOVERLAY , SYPACKED , SYPROC ,
                  SYPROG , SYRECORD , SYREPEAT , SYRETURN , SYSET ,
                  SYSQLBEGIN , SYSQLEND , SYSQLVAR , SYSTATIC , SYTHEN
                  , SYTO , SYTYPE , SYUNTIL , SYVAR , SYWHILE , SYWITH
                  , SYXOR ] ;
  WORTSYMBOLE := [ SYAND , SYARRAY , SYBEGIN , SYBREAK , SYCASE ,
                 SYCONST , SYCONTINUE , SYDO , SYELSE , SYEND , SYFOR ,
                 SYFUNC , SYGOTO , SYIF , SYIN , SYLABEL , SYLOCAL ,
                 SYMODULE , SYNOT , SYOF , SYOR , SYOTHERWISE ,
                 SYOVERLAY , SYPACKED , SYPROC , SYPROG , SYRECORD ,
                 SYREPEAT , SYRETURN , SYSET , SYSQLBEGIN , SYSQLEND ,
                 SYSQLVAR , SYSTATIC , SYTHEN , SYTO , SYTYPE , SYUNTIL
                 , SYVAR , SYWHILE , SYWITH , SYXOR ] ;
  EXPRSYMBOLE := [ SYAND , SYIN , SYNOT , SYOR , SYXOR ] ;
  INIT_SCANNER ;
  NICHTLESEN := 0 ;
  EINR := 0 ;
  EINRKOMM := 0 ;
  INC := 2 ;
  BLANKSVORHANDEN := FALSE ;
  KOMMC . UEBERLESEN := 0 ;
  INSQLSTATE := FALSE ;
  SQLHOSTV := FALSE ;
  ZZAUS := 0 ;
  ZZAUSVOR := - 1 ;
  RESET ( EINGABE ) ;
  REWRITE ( AUSGABE ) ;
  INPOINTER := 0 ;
  OUTPOINTER := EINR ;
  if VERARB_MODUS < 3 then
    PROG
  else
    CHANGE ( BLOCKBEGSYS + STATBEGSYS - [ SYCASE ] ) ;
  WRITELN ( AUSGABE ) ;

  (***********************************************)
  (*   HIER UNTERSCHIED PASCAL/VS ZU TURBO/3     *)
  (*   CLOSE (EINGABE) ; CLOSE (AUSGABE) ;       *)
  (*   WIRD BENOETIGT BEI TURBO/3                *)
  (***********************************************)

  WRITELN ( ZZAUS + 1 : 6 , ' Zeilen ausgegeben.' ) ;
end (* HAUPTPROGRAMM *) .
