module PASSCAN ;

(********************************************************************)
(*$D-,N+,A-                                                         *)
(********************************************************************)
(*                                                                  *)
(*         S T A N F O R D   P A S C A L   C O M P I L E R          *)
(*                                                                  *)
(*              OPPOLZER VERSION                                    *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*   This module is the new Source Program Scanner,                 *)
(*   which is not written by hand, but instead it is generated      *)
(*   by a tool which was written by some students of the            *)
(*   Stuttgart University (including myself) in 1980. I had         *)
(*   to enhance it in 1996 to make a usable product out of          *)
(*   this students' work.                                           *)
(*                                                                  *)
(*   The input to this tool is a large enhanced regular             *)
(*   expression, which defines all symbols known to the             *)
(*   compiler.                                                      *)
(*                                                                  *)
(*   In other situations when I used this tool, I defined the       *)
(*   whole syntax to this tool, including comments. This time       *)
(*   I decided only to define the comment starting symbols and      *)
(*   to handle the comments by hand-written logic inserted into     *)
(*   the skeleton file of the generator.                            *)
(*                                                                  *)
(*   I enhanced the tool a little bit, so that it was usable        *)
(*   for the purposes of the Pascal compiler:                       *)
(*                                                                  *)
(*   a) now Pascal can be generated, not only C                     *)
(*                                                                  *)
(*   b) the features to generate listings while scanning the        *)
(*      source are enhanced, so that the method fits better         *)
(*      to the needs of the Pascal compiler                         *)
(*                                                                  *)
(*   Bernd Oppolzer / September 2017                                *)
(*                                                                  *)
(********************************************************************)



const MAXLSIZE = 120 ;
      MAXERRNO = 999 ;


type CHARPTR = -> CHAR ;

     /***********************************/
     /* vom Scanner-Generator erzeugt:  */
     /* skalarer Typ fuer Symbole       */
     /***********************************/
     /* muss mit Def. beim Compiler     */
     /* uebereinstimmen                 */
     /***********************************/

     SYMB = ( SYMB_EOF , SYMB_UNKNOWN , EOLCHAR , SEPARATOR , COMMENT1
            , COMMENT2 , COMMENT3 , COMMENT4 , COMMENT5 , STRINGCONST ,
            HEXSTRINGCONST , BINSTRINGCONST , INTCONST , INTDOTDOT ,
            REALCONST , IDENT , SYLPARENT , SYRPARENT , SYLBRACK ,
            SYRBRACK , SYCOMMA , SYSEMICOLON , SYARROW , SYPERIOD ,
            SYDOTDOT , SYCOLON , SYPLUS , SYMINUS , SYMULT , SYSLASH ,
            SYEQOP , SYNEOP , SYGTOP , SYLTOP , SYGEOP , SYLEOP ,
            SYOROP , SYANDOP , SYASSIGN ) ;

     /***********************************/
     /* Ende generierte Deklaration     */
     /***********************************/

     CHAR32 = array [ 1 .. 32 ] of CHAR ;
     SOURCELINE = array [ 1 .. MAXLSIZE ] of CHAR ;
     SCAN_CODETAB = array [ CHAR ] of CHAR ;

     /***********************************/
     /* fuer Fehlertexte                */
     /***********************************/

     SCANFT_PTR = -> SCAN_FTTAB1 ;
     SCAN_ERRCLASS = 'A' .. 'Z' ;
     SCAN_FTEXT = record
                    FTEXT : CHARPTR ;
                    FTEXT_LEN : INTEGER
                  end ;
     SCAN_FTTAB2 = array [ 1 .. MAXERRNO ] of SCAN_FTEXT ;
     SCAN_FTTAB1 = array [ SCAN_ERRCLASS ] of SCAN_FTTAB2 ;

     /******************************************/
     /* Liste der Fehler pro Source-Zeile usw. */
     /******************************************/

     SCANF_PTR = -> SCAN_FEHLER ;
     SCAN_FEHLER = record
                     ERRLEVEL : CHAR ;
                     ERRCLASS : CHAR ;
                     NUMMER : INTEGER ;
                     INFO : CHAR32 ;
                     ZEILNR : INTEGER ;
                     POSITION : INTEGER ;
                     NAECHST : SCANF_PTR ;
                   end ;

     /***********************************/
     /* zentraler Scan-Block            */
     /***********************************/
     /* muss mit Def. beim Compiler     */
     /* uebereinstimmen                 */
     /***********************************/

     OPTIONS_PTR = -> COMP_OPTIONS ;
     SCAN_BLOCK = record
                    MODUS : INTEGER ;
                    DATEIENDE : INTEGER ;
                    ENDOFLINE : BOOLEAN ;
                    SLINE : SOURCELINE ;
                    LINENR : INTEGER ;
                    LINEPOS : INTEGER ;
                    LINELEN : INTEGER ;
                    LOOKAHEAD : CHAR ;
                    SYMBOLNR : SYMB ;
                    SYMBOL : SOURCELINE ;
                    LSYMBOL : INTEGER ;
                    MAXLSYMBOL : INTEGER ;
                    UFZAHL : INTEGER ;
                    SFZAHL : INTEGER ;
                    FEZAHL : INTEGER ;
                    WAZAHL : INTEGER ;
                    INZAHL : INTEGER ;
                    FEANFANG : SCANF_PTR ;
                    FEAKT : SCANF_PTR ;
                    FTTAB : SCANFT_PTR ;
                    FTTABA : SCANFT_PTR ;
                    OPTLINE : SOURCELINE ;
                    POPT : OPTIONS_PTR ;

     /******************************************/
     /* felder fuer sofortige Protokollausgabe */
     /******************************************/

                    PROTOUT : BOOLEAN ;
                    TERMOUT : BOOLEAN ;
                    FEAKT_ALT : SCANF_PTR ;
                    LINEINFO : CHAR32 ;
                    LINEINFO_SIZE : INTEGER ;

     /******************************************/
     /* felder fuer ueberschrift               */
     /******************************************/

                    LINECOUNT : INTEGER ;
                    HEADLINE : SOURCELINE ;
                    HEADLINE_SIZE : INTEGER ;
                    PAGENR : INTEGER ;
                  end ;

     /***********************************/
     /* Optionen fuer Compiler          */
     /***********************************/
     /* muss mit Def. beim Compiler     */
     /* uebereinstimmen                 */
     /***********************************/

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


static SCANNER_INIT : INTEGER ;
       SCAN_CODE : SCAN_CODETAB ;

       /**********************************************/
       /* Tabelle SCAN_CODE wird beim ersten Aufruf  */
       /* des Scanners umgebungsabhaengig korrekt    */
       /* gefuellt                                   */
       /**********************************************/




local function TOUPPER ( C : CHAR ) : CHAR ;

   begin (* TOUPPER *)
     if C in [ 'a' .. 'i' , 'j' .. 'r' , 's' .. 'z' ] then
       C := CHR ( ORD ( C ) - ORD ( 'a' ) + ORD ( 'A' ) ) ;
     TOUPPER := C ;
   end (* TOUPPER *) ;



local procedure SCAN_TOUPPER ( var SCB : SCAN_BLOCK ) ;

   var I : INTEGER ;

   begin (* SCAN_TOUPPER *)
     for I := 1 to SCB . LSYMBOL do
       SCB . SYMBOL [ I ] := TOUPPER ( SCB . SYMBOL [ I ] ) ;
   end (* SCAN_TOUPPER *) ;



local procedure INIT_SCAN_CODE ;

   type TRANSLATE_TAB = array [ CHAR ] of INTEGER ;

   const EBCDIC_TO_ASCII : TRANSLATE_TAB =
         ( 0X00 , 0X01 , 0X02 , 0X03 , 0XEC , 0X09 , 0XCA , 0X7F , 0XE2
           , 0XD2 , 0XD3 , 0X0B , 0X0C , 0X0D , 0X0E , 0XA9 ,

         /******************************************************/
         /*   Beim Host: \n = 0x15 => 0x0a                     */
         /******************************************************/
         /*   0x10, 0x11, 0x12, 0x13, 0xef, 0xc5, 0x08, 0xcb,  */
         /******************************************************/


           0X10 , 0X11 , 0X12 , 0X13 , 0XEF , 0X0A , 0X08 , 0XCB , 0X18
           , 0X19 , 0XDC , 0XD8 , 0X1C , 0X1D , 0X1E , 0X1F ,

         /******************************************************/
         /*   0xb7, 0xb8, 0xb9, 0xbb, 0xc4, 0x0a, 0x17, 0x1b,  */
         /******************************************************/


           0XB7 , 0XB8 , 0XB9 , 0XBB , 0XC4 , 0XC5 , 0X17 , 0X1B , 0XCC
           , 0XCD , 0XCF , 0XD0 , 0XD1 , 0X05 , 0X06 , 0X07 , 0XD9 ,
           0XDA , 0X16 , 0XDD , 0XDE , 0XDF , 0XE0 , 0X04 , 0XE3 , 0XE5
           , 0XE9 , 0XEB , 0XB0 , 0XB1 , 0X9E , 0X1A , 0X20 , 0XC9 ,
           0X83 , 0X7B , 0X85 , 0XA0 , 0XF2 , 0X86 , 0X87 , 0XA4 , 0X8E
           , 0X2E , 0X3C , 0X28 , 0X2B , 0X21 , 0X26 , 0X82 , 0X88 ,
           0X89 , 0X8A , 0XA1 , 0X8C , 0X8B , 0X8D , 0X7E , 0X9A , 0X24
           , 0X2A , 0X29 , 0X3B , 0X5E , 0X2D , 0X2F , 0XB2 , 0X5B ,
           0XB4 , 0XB5 , 0XB6 , 0X8F , 0X80 , 0XA5 , 0X94 , 0X2C , 0X25
           , 0X5F , 0X3E , 0X3F , 0XBA , 0X90 , 0XBC , 0XBD , 0XBE ,
           0XF3 , 0XC0 , 0XC1 , 0XC2 , 0X60 , 0X3A , 0X23 , 0X15 , 0X27
           , 0X3D , 0X22 , 0XC3 , 0X61 , 0X62 , 0X63 , 0X64 , 0X65 ,
           0X66 , 0X67 , 0X68 , 0X69 , 0XAE , 0XAF , 0XC6 , 0XC7 , 0XC8
           , 0XF1 , 0XF8 , 0X6A , 0X6B , 0X6C , 0X6D , 0X6E , 0X6F ,
           0X70 , 0X71 , 0X72 , 0XA6 , 0XA7 , 0X91 , 0XCE , 0X92 , 0X0F
           , 0XE6 , 0XE1 , 0X73 , 0X74 , 0X75 , 0X76 , 0X77 , 0X78 ,
           0X79 , 0X7A , 0XAD , 0XA8 , 0XD4 , 0XD5 , 0XD6 , 0XD7 , 0X9B
           , 0X9C , 0X9D , 0XFA , 0X9F , 0X40 , 0X14 , 0XAC , 0XAB ,
           0XFC , 0XAA , 0XB3 , 0XE4 , 0XFE , 0XBF , 0XE7 , 0X84 , 0X41
           , 0X42 , 0X43 , 0X44 , 0X45 , 0X46 , 0X47 , 0X48 , 0X49 ,
           0XE8 , 0X93 , 0X7C , 0X95 , 0XA2 , 0XED , 0X81 , 0X4A , 0X4B
           , 0X4C , 0X4D , 0X4E , 0X4F , 0X50 , 0X51 , 0X52 , 0XEE ,
           0X96 , 0X7D , 0X97 , 0XA3 , 0X98 , 0X99 , 0XF0 , 0X53 , 0X54
           , 0X55 , 0X56 , 0X57 , 0X58 , 0X59 , 0X5A , 0XFD , 0XF5 ,
           0X5C , 0XF7 , 0XF6 , 0XF9 , 0X30 , 0X31 , 0X32 , 0X33 , 0X34
           , 0X35 , 0X36 , 0X37 , 0X38 , 0X39 , 0XDB , 0XFB , 0X5D ,
           0XF4 , 0XEA , 0XFF ) ;

   var C : CHAR ;

   begin (* INIT_SCAN_CODE *)
     if ORD ( '0' ) = 0X30 then
       for C := CHR ( 0 ) to CHR ( 255 ) do
         SCAN_CODE [ C ] := C
     else
       for C := CHR ( 0 ) to CHR ( 255 ) do
         SCAN_CODE [ C ] := CHR ( EBCDIC_TO_ASCII [ C ] )
   end (* INIT_SCAN_CODE *) ;



local procedure SCALE_LINE ( var SCANOUT : TEXT ; var SCB : SCAN_BLOCK
                           ) ;

   begin (* SCALE_LINE *)
     WRITE ( SCANOUT , ' ' : 8 ) ;
     if SCB . LINEINFO_SIZE > 0 then
       WRITE ( SCANOUT , ' ' : SCB . LINEINFO_SIZE + 1 ) ;
     WRITELN ( SCANOUT , '....5...10....5...20....5...30....5'
               '...40....5...50....5...60....5...70..' ) ;
   end (* SCALE_LINE *) ;



local procedure CHECK_HEADING ( var SCANOUT : TEXT ; var SCB :
                              SCAN_BLOCK ) ;

   var PO : OPTIONS_PTR ;

   begin (* CHECK_HEADING *)
     SCB . LINECOUNT := SCB . LINECOUNT + 1 ;
     PO := SCB . POPT ;
     if SCB . LINECOUNT > PO -> . PAGESIZE - 6 then
       begin
         if SCB . PAGENR > 0 then
           SCALE_LINE ( SCANOUT , SCB ) ;
         SCB . PAGENR := SCB . PAGENR + 1 ;
         WRITELN ( SCANOUT , SCB . HEADLINE : SCB . HEADLINE_SIZE ,
                   ' PAGE ' , SCB . PAGENR : 1 ) ;
         WRITELN ( SCANOUT ) ;
         SCALE_LINE ( SCANOUT , SCB ) ;
         SCB . LINECOUNT := 3 ;
       end (* then *) ;
   end (* CHECK_HEADING *) ;



local procedure SCAN_FEHLER_AUSGEBEN ( var SCANOUT : TEXT ; var SCB :
                                     SCAN_BLOCK ; FELAUF : SCANF_PTR )
                                     ;

   var ERRCLASS : CHAR ;
       NUMMER : INTEGER ;
       FTEXT_REC : SCAN_FTEXT ;
       FTEXT : array [ 1 .. 120 ] of CHAR ;
       L : INTEGER ;

   begin (* SCAN_FEHLER_AUSGEBEN *)
     ERRCLASS := FELAUF -> . ERRCLASS ;
     NUMMER := FELAUF -> . NUMMER ;
     if ( SCB . FTTABA = NIL ) and ( SCB . FTTAB = NIL ) then
       begin
         WRITELN ( SCANOUT , '*** no msg text table installed ***' ) ;
         return ;
       end (* then *) ;
     if SCB . FTTABA <> NIL then
       begin
         FTEXT_REC := SCB . FTTABA -> [ ERRCLASS ] [ NUMMER ] ;
         if FTEXT_REC . FTEXT <> NIL then
           begin

     /**********************************/
     /* hier fehlt noch Platzhalter %s */
     /**********************************/

             L := FTEXT_REC . FTEXT_LEN ;
             if L > 120 then
               L := 120 ;
             FTEXT := ' ' ;
             MEMCPY ( ADDR ( FTEXT ) , FTEXT_REC . FTEXT , L ) ;
             WRITELN ( SCANOUT , FTEXT : L ) ;
             return ;
           end (* then *)
       end (* then *) ;
     if SCB . FTTAB <> NIL then
       begin
         FTEXT_REC := SCB . FTTAB -> [ ERRCLASS ] [ NUMMER ] ;
         if FTEXT_REC . FTEXT <> NIL then
           begin

     /**********************************/
     /* hier fehlt noch Platzhalter %s */
     /**********************************/

             L := FTEXT_REC . FTEXT_LEN ;
             if L > 120 then
               L := 120 ;
             FTEXT := ' ' ;
             MEMCPY ( ADDR ( FTEXT ) , FTEXT_REC . FTEXT , L ) ;
             WRITELN ( SCANOUT , FTEXT : L ) ;
             return ;
           end (* then *)
       end (* then *) ;
     WRITELN ( SCANOUT , '*** no msg text found ***' ) ;
   end (* SCAN_FEHLER_AUSGEBEN *) ;



local procedure PROT_ZEILE_AUSG ( var SCANOUT : TEXT ; var SCB :
                                SCAN_BLOCK ; ALLES : BOOLEAN ; ISTERM :
                                BOOLEAN ) ;

   var ZEILENNR : INTEGER ;
       PL : INTEGER ;
       FELAUF : SCANF_PTR ;
       ZEILE_ENTH_FEHLER : BOOLEAN ;

   begin (* PROT_ZEILE_AUSG *)
     ZEILENNR := SCB . LINENR ;
     ZEILE_ENTH_FEHLER := SCB . FEAKT <> SCB . FEAKT_ALT ;
     if ALLES or ZEILE_ENTH_FEHLER then
       begin
         if not ISTERM then
           CHECK_HEADING ( SCANOUT , SCB ) ;
         WRITE ( SCANOUT , ZEILENNR : 7 , ' ' ) ;
         if not ISTERM then
           if SCB . LINEINFO_SIZE > 0 then
             WRITE ( SCANOUT , SCB . LINEINFO : SCB . LINEINFO_SIZE ,
                     ' ' ) ;
         WRITELN ( SCANOUT , SCB . SLINE : SCB . LINELEN )
       end (* then *) ;
     if ZEILE_ENTH_FEHLER then
       begin
         PL := 0 ;
         if not ISTERM then
           CHECK_HEADING ( SCANOUT , SCB ) ;
         WRITE ( SCANOUT , ' ' : 6 ) ;
         if not ISTERM then
           if SCB . LINEINFO_SIZE > 0 then
             WRITE ( SCANOUT , ' ' : SCB . LINEINFO_SIZE + 1 ) ;
         if SCB . FEAKT_ALT = NIL then
           FELAUF := SCB . FEANFANG
         else
           FELAUF := SCB . FEAKT_ALT -> . NAECHST ;
         while TRUE do
           begin
             if FELAUF = NIL then
               break ;
             if PL < FELAUF -> . POSITION then
               begin
                 WRITE ( SCANOUT , ' ' : FELAUF -> . POSITION - PL ) ;
                 PL := FELAUF -> . POSITION ;
               end (* then *) ;
             WRITE ( SCANOUT , '!' ) ;
             PL := PL + 1 ;
             FELAUF := FELAUF -> . NAECHST ;
           end (* while *) ;
         WRITELN ( SCANOUT ) ;
         if SCB . FEAKT_ALT = NIL then
           FELAUF := SCB . FEANFANG
         else
           FELAUF := SCB . FEAKT_ALT -> . NAECHST ;
         while TRUE do
           begin
             if FELAUF = NIL then
               break ;
             if not ISTERM then
               CHECK_HEADING ( SCANOUT , SCB ) ;
             case FELAUF -> . ERRLEVEL of
               'S' : WRITE ( SCANOUT , '    +++ Severe Error ' , FELAUF
                             -> . ERRCLASS , FELAUF -> . NUMMER : - 3 ,
                             ': ' ) ;
               'F' : WRITE ( SCANOUT , '    +++ Error ' , FELAUF -> .
                             ERRCLASS , FELAUF -> . NUMMER : - 3 , ': '
                             ) ;
               'W' : WRITE ( SCANOUT , '    ** Warning ' , FELAUF -> .
                             ERRCLASS , FELAUF -> . NUMMER : - 3 , ': '
                             ) ;
               'I' : WRITE ( SCANOUT , '    ** Inform. ' , FELAUF -> .
                             ERRCLASS , FELAUF -> . NUMMER : - 3 , ': '
                             ) ;
               otherwise
                 WRITE ( SCANOUT , '    ** Level-' , FELAUF -> .
                         ERRLEVEL , ' ' , FELAUF -> . ERRCLASS , FELAUF
                         -> . NUMMER : - 3 , ': ' ) ;
             end (* case *) ;
             SCAN_FEHLER_AUSGEBEN ( SCANOUT , SCB , FELAUF ) ;
             FELAUF := FELAUF -> . NAECHST ;
           end (* while *) ;
         if not ISTERM then
           CHECK_HEADING ( SCANOUT , SCB ) ;
         WRITELN ( SCANOUT ) ;
       end (* then *)
   end (* PROT_ZEILE_AUSG *) ;



procedure PASSCANS ( var SCANOUT : TEXT ; var SCB : SCAN_BLOCK ) ;

(************************************************)
(*                                              *)
(*   Summary ausgeben                           *)
(*                                              *)
(************************************************)


   var ERSTAUSG : BOOLEAN ;
       SCALE : INTEGER ;

   begin (* PASSCANS *)
     if SCB . UFZAHL + SCB . SFZAHL + SCB . FEZAHL + SCB . WAZAHL = 0
     then
       begin
         WRITELN ( SCANOUT , '      No Errors, no Warnings.' ) ;
       end (* then *)
     else
       begin
         ERSTAUSG := TRUE ;
         SCALE := 8 ;
         if SCB . UFZAHL <> 0 then
           begin
             WRITE ( SCANOUT , SCB . UFZAHL : SCALE ) ;
             WRITE ( SCANOUT , ' unknown' ) ;
             WRITE ( SCANOUT , ' Error' ) ;
             if SCB . UFZAHL <> 1 then
               WRITE ( SCANOUT , 's' ) ;
             ERSTAUSG := FALSE ;
             SCALE := 1 ;
           end (* then *) ;
         if SCB . SFZAHL <> 0 then
           begin
             if not ERSTAUSG then
               WRITE ( SCANOUT , ', ' ) ;
             WRITE ( SCANOUT , SCB . SFZAHL : SCALE ) ;
             WRITE ( SCANOUT , ' severe' ) ;
             WRITE ( SCANOUT , ' Error' ) ;
             if SCB . SFZAHL <> 1 then
               WRITE ( SCANOUT , 's' ) ;
             ERSTAUSG := FALSE ;
             SCALE := 1 ;
           end (* then *) ;
         if SCB . FEZAHL <> 0 then
           begin
             if not ERSTAUSG then
               WRITE ( SCANOUT , ', ' ) ;
             WRITE ( SCANOUT , SCB . FEZAHL : SCALE ) ;
             WRITE ( SCANOUT , ' Error' ) ;
             if SCB . FEZAHL <> 1 then
               WRITE ( SCANOUT , 's' ) ;
             ERSTAUSG := FALSE ;
             SCALE := 1 ;
           end (* then *) ;
         if SCB . WAZAHL <> 0 then
           begin
             if not ERSTAUSG then
               WRITE ( SCANOUT , ', ' ) ;
             WRITE ( SCANOUT , SCB . WAZAHL : SCALE ) ;
             WRITE ( SCANOUT , ' Warning' ) ;
             if SCB . WAZAHL <> 1 then
               WRITE ( SCANOUT , 's' ) ;
             ERSTAUSG := FALSE ;
             SCALE := 1 ;
           end (* then *) ;
         WRITELN ( SCANOUT , '.' ) ;
       end (* else *) ;
   end (* PASSCANS *) ;



procedure PASSCANL ( var SCANINP : TEXT ; var SCANOUT : TEXT ; var SCB
                   : SCAN_BLOCK ; ALLES : BOOLEAN ) ;

(************************************************)
(*                                              *)
(*   source_listing beruht auf der alten        *)
(*   Funktion fehlerausgabe.                    *)
(*                                              *)
(*   Die Eingabedatei wird nochmal gelesen      *)
(*   und ausgegeben; die Fehler werden          *)
(*   ueber den ganzen Compile-Lauf hinweg       *)
(*   in der Fehlerliste gesammelt und hier      *)
(*   in den Source-Text eingestreut.            *)
(*                                              *)
(*   Schalter alles gibt an, ob auch Zeilen     *)
(*   ohne Fehler ausgegeben werden sollen       *)
(*                                              *)
(************************************************)


   var PL : INTEGER ;
       FESAVE : SCANF_PTR ;
       ZEILE_ENTH_FEHLER : BOOLEAN ;
       PO : OPTIONS_PTR ;

   begin (* PASSCANL *)
     RESET ( SCANINP ) ;
     PO := SCB . POPT ;
     SCB . LINECOUNT := PO -> . PAGESIZE + 1 ;
     SCB . HEADLINE := 'S O U R C E   L I S T I N G' ;
     SCB . HEADLINE_SIZE := 50 ;
     SCB . LINENR := 0 ;
     SCB . FEAKT := SCB . FEANFANG ;
     while TRUE do
       begin
         if EOF ( SCANINP ) then
           break ;
         READLN ( SCANINP , SCB . SLINE ) ;
         SCB . LINENR := SCB . LINENR + 1 ;
         PROT_ZEILE_AUSG ( SCANOUT , SCB , ALLES , FALSE ) ;
       end (* while *) ;
     PASSCANS ( SCANOUT , SCB ) ;
   end (* PASSCANL *) ;



local procedure SCAN_READC ( var SCANINP : TEXT ; var SCANOUT : TEXT ;
                           var SCB : SCAN_BLOCK ; var CH : CHAR ) ;

   begin (* SCAN_READC *)
     while TRUE do
       begin
         if SCB . LINEPOS < SCB . LINELEN then
           begin
             SCB . ENDOFLINE := FALSE ;
             SCB . LINEPOS := SCB . LINEPOS + 1 ;
             CH := SCB . SLINE [ SCB . LINEPOS ] ;
             return ;
           end (* then *) ;
         if SCB . LINEPOS = SCB . LINELEN then
           begin
             SCB . ENDOFLINE := TRUE ;
             SCB . LINEPOS := SCB . LINEPOS + 1 ;
             CH := ' ' ;
             return ;
           end (* then *) ;

     /******************************************/
     /* letzte zeile ausgeben, wenn gewuenscht */
     /******************************************/

         if SCB . LINENR > 0 then
           begin
             if SCB . PROTOUT then
               PROT_ZEILE_AUSG ( SCANOUT , SCB , TRUE , FALSE ) ;
             if SCB . TERMOUT then
               PROT_ZEILE_AUSG ( OUTPUT , SCB , FALSE , TRUE ) ;
           end (* then *) ;

     /******************************************/
     /* neue Zeile einlesen                    */
     /******************************************/

         if EOF ( SCANINP ) then
           begin
             SCALE_LINE ( SCANOUT , SCB ) ;
             SCB . DATEIENDE := 1 ;
             SCB . SLINE := ' ' ;
             SCB . LINENR := SCB . LINENR + 1 ;
             SCB . LINEPOS := 1 ;
             SCB . LINELEN := 0 ;
             CH := ' ' ;
             return ;
           end (* then *) ;
         READLN ( SCANINP , SCB . SLINE ) ;
         SCB . LINENR := SCB . LINENR + 1 ;
         SCB . LINEPOS := 0 ;
         SCB . LINELEN := MAXLSIZE ;
         SCB . FEAKT_ALT := SCB . FEAKT ;
         while TRUE do
           begin
             if SCB . LINELEN = 0 then
               break ;
             if SCB . SLINE [ SCB . LINELEN ] <> ' ' then
               break ;
             SCB . LINELEN := SCB . LINELEN - 1 ;
           end (* while *)
       end (* while *)
   end (* SCAN_READC *) ;



procedure PASSCANE ( var SCB : SCAN_BLOCK ; ERRLEVEL : CHAR ; ERRCLASS
                   : CHAR ; I : INTEGER ; INFO : CHAR32 ; ZEILNR :
                   INTEGER ; PLATZ : INTEGER ) ;

(***********************************************************)
(*                                                         *)
(* Fehler vom Compiler melden                              *)
(*                                                         *)
(* ERRLEVEL gibt die Schwere des Fehlers an                *)
(* ERRCLASS und I (FehlerNr) identifizieren den Fehler     *)
(* ZEILNR und PLATZ geben die Fehlerstelle im Source an    *)
(* Mit INFO kann man zusaetzliche Info mitgeben            *)
(* (Ausbaustufe)                                           *)
(*                                                         *)
(***********************************************************)


   var PWORK : SCANF_PTR ;
       PSAVE : SCANF_PTR ;

   begin (* PASSCANE *)

     /*****************************************************/
     /*   Fehlerelement in die Fehlerkette einfuegen      */
     /*****************************************************/

     case ERRLEVEL of
       'S' : SCB . SFZAHL := SCB . SFZAHL + 1 ;
       'F' : SCB . FEZAHL := SCB . FEZAHL + 1 ;
       'W' : SCB . WAZAHL := SCB . WAZAHL + 1 ;
       'I' : SCB . INZAHL := SCB . INZAHL + 1 ;
       'O' : ;
       otherwise
         SCB . UFZAHL := SCB . UFZAHL + 1 ;
     end (* case *) ;
     if SCB . FEANFANG = NIL then
       begin

     /*****************************************************/
     /*   Fehlerliste leer, erstes Element einfuegen      */
     /*****************************************************/

         SCB . FEANFANG := ALLOC ( SIZEOF ( SCAN_FEHLER ) ) ;
         SCB . FEAKT := SCB . FEANFANG ;
         SCB . FEAKT -> . ERRLEVEL := ERRLEVEL ;
         SCB . FEAKT -> . ERRCLASS := ERRCLASS ;
         SCB . FEAKT -> . NUMMER := I ;
         SCB . FEAKT -> . INFO := INFO ;
         SCB . FEAKT -> . ZEILNR := ZEILNR ;
         SCB . FEAKT -> . POSITION := PLATZ ;
         SCB . FEAKT -> . NAECHST := NIL ;
       end (* then *)
     else
       begin
         if ( SCB . FEAKT -> . ZEILNR < ZEILNR ) or ( ( SCB . FEAKT ->
         . ZEILNR = ZEILNR ) and ( SCB . FEAKT -> . POSITION <= PLATZ )
         ) then
           begin

     /*****************************************************/
     /*   neues Element ganz hinten einfuegen             */
     /*****************************************************/

             SCB . FEAKT -> . NAECHST := ALLOC ( SIZEOF ( SCAN_FEHLER )
                                         ) ;
             SCB . FEAKT := SCB . FEAKT -> . NAECHST ;
             SCB . FEAKT -> . ERRLEVEL := ERRLEVEL ;
             SCB . FEAKT -> . ERRCLASS := ERRCLASS ;
             SCB . FEAKT -> . NUMMER := I ;
             SCB . FEAKT -> . INFO := INFO ;
             SCB . FEAKT -> . ZEILNR := ZEILNR ;
             SCB . FEAKT -> . POSITION := PLATZ ;
             SCB . FEAKT -> . NAECHST := NIL ;
           end (* then *)
         else
           begin
             PWORK := SCB . FEANFANG ;
             if ( PWORK -> . ZEILNR < ZEILNR ) or ( ( PWORK -> . ZEILNR
             = ZEILNR ) and ( PWORK -> . POSITION <= PLATZ ) ) then
               begin

     /*****************************************************/
     /*   Element irgendwo in der Mitte einfuegen         */
     /*****************************************************/

                 PWORK := SCB . FEANFANG ;
                 while ( PWORK -> . NAECHST -> . ZEILNR < ZEILNR ) or (
                 ( PWORK -> . NAECHST -> . ZEILNR = ZEILNR ) and (
                 PWORK -> . NAECHST -> . POSITION <= PLATZ ) ) do
                   PWORK := PWORK -> . NAECHST ;
                 PSAVE := PWORK -> . NAECHST ;
                 PWORK -> . NAECHST := ALLOC ( SIZEOF ( SCAN_FEHLER ) )
                                       ;
                 PWORK := PWORK -> . NAECHST ;
                 PWORK -> . ERRLEVEL := ERRLEVEL ;
                 PWORK -> . ERRCLASS := ERRCLASS ;
                 PWORK -> . NUMMER := I ;
                 PWORK -> . INFO := INFO ;
                 PWORK -> . ZEILNR := ZEILNR ;
                 PWORK -> . POSITION := PLATZ ;
                 PWORK -> . NAECHST := PSAVE ;
               end (* then *)
             else
               begin

     /*****************************************************/
     /*   neues Element ganz vorne einfuegen              */
     /*****************************************************/

                 PSAVE := SCB . FEANFANG ;
                 PWORK := ALLOC ( SIZEOF ( SCAN_FEHLER ) ) ;
                 PWORK -> . ERRLEVEL := ERRLEVEL ;
                 PWORK -> . ERRCLASS := ERRCLASS ;
                 PWORK -> . NUMMER := I ;
                 PWORK -> . INFO := INFO ;
                 PWORK -> . ZEILNR := ZEILNR ;
                 PWORK -> . POSITION := PLATZ ;
                 PWORK -> . NAECHST := PSAVE ;
                 SCB . FEANFANG := PWORK ;
               end (* else *)
           end (* else *)
       end (* else *)
   end (* PASSCANE *) ;



local function OPTIONS ( var SCANINP : TEXT ; var SCANOUT : TEXT ; var
                       SCB : SCAN_BLOCK ; COMMENTTYPE : INTEGER ) :
                       BOOLEAN ;

(***********************************)
(*   CCH = COMMENT TERMINATOR CH   *)
(***********************************)


   type SET_CHAR = set of CHAR ;

   var CCH : CHAR ;
       SCANCH : CHAR ;
       OPTCH : CHAR ;
       TERMCH : CHAR ;
       FERTIG : BOOLEAN ;
       OPTIX : INTEGER ;
       X , Y : INTEGER ;

   const LOW_LETTERS : SET_CHAR =
         [ 'a' .. 'i' , 'j' .. 'r' , 's' .. 'z' ] ;
         UP_LETTERS : SET_CHAR =
         [ 'A' .. 'I' , 'J' .. 'R' , 'S' .. 'Z' ] ;


   procedure WRITEOPT ;

      begin (* WRITEOPT *)
        OPTIX := OPTIX + 1 ;
        if OPTIX <= SIZEOF ( SOURCELINE ) then
          SCB . OPTLINE [ OPTIX ] := SCANCH ;
      end (* WRITEOPT *) ;


   function DECNUM : INTEGER ;

      var NUM : INTEGER ;

      begin (* DECNUM *)
        NUM := 0 ;
        SCAN_READC ( SCANINP , SCANOUT , SCB , SCANCH ) ;
        SCB . LOOKAHEAD := SCANCH ;
        WRITEOPT ;
        while SCANCH in [ '0' .. '9' ] do
          begin
            NUM := NUM * 10 + ORD ( SCANCH ) - ORD ( '0' ) ;
            SCAN_READC ( SCANINP , SCANOUT , SCB , SCANCH ) ;
            SCB . LOOKAHEAD := SCANCH ;
            WRITEOPT ;
          end (* while *) ;
        DECNUM := NUM
      end (* DECNUM *) ;


   begin (* OPTIONS *)
     OPTIX := 0 ;
     SCB . OPTLINE := '' ;
     case COMMENTTYPE of
       1 : begin
             TERMCH := '*' ;
             CCH := '/'
           end (* tag/ca *) ;
       2 : begin
             TERMCH := '*' ;
             CCH := ')'
           end (* tag/ca *) ;
       3 : begin
             TERMCH := '}' ;
             CCH := '{'
           end (* tag/ca *) ;
       4 : begin
             TERMCH := '"' ;
             CCH := '"'
           end (* tag/ca *) ;
       5 : begin
             TERMCH := ' ' ;
             CCH := '+'
           end (* tag/ca *) ;
     end (* case *) ;
     SCANCH := SCB . LOOKAHEAD ;
     WRITEOPT ;
     while TRUE do
       begin
         FERTIG := FALSE ;
         if COMMENTTYPE in [ 3 , 4 ] then
           FERTIG := ( SCANCH = TERMCH )
         else
           if COMMENTTYPE = 5 then
             FERTIG := SCB . ENDOFLINE
           else
             if SCANCH = TERMCH then
               begin
                 SCAN_READC ( SCANINP , SCANOUT , SCB , SCANCH ) ;
                 SCB . LOOKAHEAD := SCANCH ;
                 FERTIG := ( SCANCH = CCH ) ;
                 if not FERTIG then
                   continue ;
               end (* then *) ;
         if FERTIG then
           begin
             SCAN_READC ( SCANINP , SCANOUT , SCB , SCANCH ) ;
             SCB . LOOKAHEAD := SCANCH ;
             OPTIONS := TRUE ;
             break ;
           end (* then *) ;
         if SCANCH = ' ' then
           begin
             repeat
               SCAN_READC ( SCANINP , SCANOUT , SCB , SCANCH ) ;
               SCB . LOOKAHEAD := SCANCH ;
             until SCB . ENDOFLINE or ( SCANCH <> ' ' ) ;
             if not SCB . ENDOFLINE and ( SCANCH <> ' ' ) and ( SCANCH
             <> TERMCH ) then
               PASSCANE ( SCB , 'W' , 'S' , 5 , ' ' , SCB . LINENR ,
                          SCB . LINEPOS + 1 ) ;
             OPTIONS := FALSE ;
             break ;
           end (* then *) ;
         if not ( SCANCH in LOW_LETTERS + UP_LETTERS ) then
           begin
             if SCANCH <> ',' then
               PASSCANE ( SCB , 'W' , 'S' , 3 , ' ' , SCB . LINENR ,
                          SCB . LINEPOS + 1 ) ;
             SCAN_READC ( SCANINP , SCANOUT , SCB , SCANCH ) ;
             SCB . LOOKAHEAD := SCANCH ;
             WRITEOPT ;
             continue ;
           end (* then *) ;
         OPTCH := SCANCH ;
         if OPTCH in LOW_LETTERS then
           OPTCH := CHR ( ORD ( OPTCH ) - ORD ( 'a' ) + ORD ( 'A' ) ) ;
         SCAN_READC ( SCANINP , SCANOUT , SCB , SCANCH ) ;
         SCB . LOOKAHEAD := SCANCH ;
         WRITEOPT ;
         case SCANCH of
           '+' , '-' :
             begin
               SCAN_READC ( SCANINP , SCANOUT , SCB , SCANCH ) ;
               SCB . LOOKAHEAD := SCANCH ;
               WRITEOPT ;
             end (* tag/ca *) ;
           '0' , '9' :
             begin
               SCAN_READC ( SCANINP , SCANOUT , SCB , SCANCH ) ;
               SCB . LOOKAHEAD := SCANCH ;
               WRITEOPT ;
             end (* tag/ca *) ;
           '(' : begin
                   X := DECNUM ;
                   if SCANCH = ',' then
                     Y := DECNUM ;
                   if SCANCH = ')' then
                     begin
                       SCAN_READC ( SCANINP , SCANOUT , SCB , SCANCH )
                                    ;
                       SCB . LOOKAHEAD := SCANCH ;
                       WRITEOPT ;
                     end (* then *)
                   else
                     PASSCANE ( SCB , 'W' , 'S' , 4 , ' ' , SCB .
                                LINENR , SCB . LINEPOS + 1 ) ;
                 end (* tag/ca *) ;
           otherwise
             begin
               
             end (* otherw *)
         end (* case *) ;
       end (* while *) ;
   end (* OPTIONS *) ;



local procedure COMMENT ( var SCANINP : TEXT ; var SCANOUT : TEXT ; var
                        SCB : SCAN_BLOCK ; COMMENTTYPE : INTEGER ) ;

(***********************************)
(*   CCH = COMMENT TERMINATOR CH   *)
(***********************************)


   var TERMCH : CHAR ;
       CCH : CHAR ;
       SCANCH : CHAR ;
       FERTIG : BOOLEAN ;

   begin (* COMMENT *)
     SCB . OPTLINE := '' ;
     if SCB . LOOKAHEAD = '$' then
       begin
         SCAN_READC ( SCANINP , SCANOUT , SCB , SCANCH ) ;
         SCB . LOOKAHEAD := SCANCH ;
         FERTIG := OPTIONS ( SCANINP , SCANOUT , SCB , COMMENTTYPE ) ;
         if FERTIG then
           return
       end (* then *) ;
     case COMMENTTYPE of
       1 : begin
             TERMCH := '*' ;
             CCH := '/'
           end (* tag/ca *) ;
       2 : begin
             TERMCH := '*' ;
             CCH := ')'
           end (* tag/ca *) ;
       3 : begin
             TERMCH := '}' ;
             CCH := '{'
           end (* tag/ca *) ;
       4 : begin
             TERMCH := '"' ;
             CCH := '"'
           end (* tag/ca *) ;
       5 : begin
             TERMCH := ' ' ;
             CCH := '+'
           end (* tag/ca *) ;
     end (* case *) ;

     (*****************************************)
     (*   SET TERMCH - EXPECTED COMMENT       *)
     (*   TERMINATING CHARACTER               *)
     (*****************************************)

     SCANCH := SCB . LOOKAHEAD ;
     while TRUE do
       begin
         FERTIG := FALSE ;
         if COMMENTTYPE in [ 3 , 4 ] then
           FERTIG := ( SCANCH = TERMCH )
         else
           if COMMENTTYPE = 5 then
             FERTIG := SCB . ENDOFLINE
           else
             if SCANCH = TERMCH then
               begin
                 SCAN_READC ( SCANINP , SCANOUT , SCB , SCANCH ) ;
                 SCB . LOOKAHEAD := SCANCH ;
                 FERTIG := ( SCANCH = CCH ) ;
                 if not FERTIG then
                   continue ;
               end (* then *) ;
         if FERTIG then
           begin
             SCAN_READC ( SCANINP , SCANOUT , SCB , SCANCH ) ;
             SCB . LOOKAHEAD := SCANCH ;
             break ;
           end (* then *) ;

     (*****************************************)
     (*   ONLY ALLOW NESTING OF COMMENTS      *)
     (*   OF SAME TYPE                        *)
     (*****************************************)

         if SCB . POPT -> . NESTCOMM then
           case COMMENTTYPE of
             1 : begin
                   if SCANCH = '/' then
                     begin
                       SCAN_READC ( SCANINP , SCANOUT , SCB , SCANCH )
                                    ;
                       SCB . LOOKAHEAD := SCANCH ;
                       if SCANCH = '*' then
                         begin
                           COMMENT ( SCANINP , SCANOUT , SCB , 1 ) ;
                           continue
                         end (* then *) ;
                     end (* then *)
                 end (* tag/ca *) ;
             2 : begin
                   if SCANCH = '(' then
                     begin
                       SCAN_READC ( SCANINP , SCANOUT , SCB , SCANCH )
                                    ;
                       SCB . LOOKAHEAD := SCANCH ;
                       if SCANCH = '*' then
                         begin
                           COMMENT ( SCANINP , SCANOUT , SCB , 2 ) ;
                           continue ;
                         end (* then *) ;
                     end (* then *)
                 end (* tag/ca *) ;
             3 : begin
                   if SCANCH = '{' then
                     begin
                       COMMENT ( SCANINP , SCANOUT , SCB , 3 ) ;
                       continue
                     end (* then *) ;
                 end (* tag/ca *) ;
             4 : begin
                   if SCANCH = '"' then
                     begin
                       COMMENT ( SCANINP , SCANOUT , SCB , 4 ) ;
                       continue ;
                     end (* then *) ;
                 end (* tag/ca *) ;
             otherwise
               
           end (* case *) ;
         SCAN_READC ( SCANINP , SCANOUT , SCB , SCANCH ) ;
         SCB . LOOKAHEAD := SCANCH ;
       end (* while *) ;
   end (* COMMENT *) ;



procedure PASSCANF ( var SCB : SCAN_BLOCK ; WHICHTABLE : CHAR ;
                   ERRCLASS : SCAN_ERRCLASS ; ERRNUM : INTEGER ; ERRMSG
                   : SOURCELINE ; ERRMSGSIZE : INTEGER ) ;

(***********************************************************)
(*                                                         *)
(* Uebergabe eines Fehlertextes zum Eintrag                *)
(* in eine der internen Fehlertext-Tabellen, die           *)
(* am Scan-Block haengen                                   *)
(*                                                         *)
(* SCB = Scan Block                                        *)
(* WHICHTABLE = S(ystem) or A(nwendung)                    *)
(* ERRCLASS und ERRNUM = Keys fuer den Fehlertext          *)
(* ERRMSG und ERRMSGSIZE = der Fehlertext                  *)
(*                                                         *)
(***********************************************************)


   var PFTAB : SCANFT_PTR ;
       C : SCAN_ERRCLASS ;
       N : INTEGER ;
       L : INTEGER ;

   begin (* PASSCANF *)
     case WHICHTABLE of
       'S' : PFTAB := SCB . FTTAB ;
       'A' : PFTAB := SCB . FTTABA ;
       otherwise
         return
     end (* case *) ;
     if PFTAB = NIL then
       begin
         PFTAB := ALLOC ( SIZEOF ( SCAN_FTTAB1 ) ) ;
         for C := 'A' to 'Z' do
           for N := 1 to MAXERRNO do
             with PFTAB -> [ C ] [ N ] do
               begin
                 FTEXT := NIL ;
                 FTEXT_LEN := 0
               end (* with *) ;
         case WHICHTABLE of
           'S' : SCB . FTTAB := PFTAB ;
           'A' : SCB . FTTABA := PFTAB ;
         end (* case *) ;
       end (* then *) ;
     if ( ERRNUM < 1 ) or ( ERRNUM > MAXERRNO ) then
       return ;
     with PFTAB -> [ ERRCLASS ] [ ERRNUM ] do
       begin
         L := ERRMSGSIZE ;
         if L > 120 then
           L := 120 ;
         FTEXT := ALLOC ( L ) ;
         FTEXT_LEN := L ;
         MEMCPY ( FTEXT , ADDR ( ERRMSG ) , L )
       end (* with *)
   end (* PASSCANF *) ;



procedure PASSCAN ( var SCANINP : TEXT ; var SCANOUT : TEXT ; var SCB :
                  SCAN_BLOCK ) ;

(***********************************************************)
(*                                                         *)
(* die eigentliche Scanner-Prozedur                        *)
(*                                                         *)
(***********************************************************)


   var ALTZUST : INTEGER ;
       ZUST : INTEGER ;
       CH : CHAR ;
       CASE_FOUND : BOOLEAN ;


   procedure SCANNER2 ( ALTZUST : INTEGER ; var SCB : SCAN_BLOCK ) ;

      begin (* SCANNER2 *)

        /***************************************************/
        /*   Symbol setzen abh. vom Endzustand             */
        /***************************************************/

        if SCB . DATEIENDE <> 0 then
          SCB . SYMBOLNR := SYMB_EOF
        else
          case ALTZUST of
            2 : SCB . SYMBOLNR := EOLCHAR ;
            3 : SCB . SYMBOLNR := SEPARATOR ;
            5 : SCB . SYMBOLNR := COMMENT1 ;
            7 : SCB . SYMBOLNR := COMMENT2 ;
            8 : SCB . SYMBOLNR := COMMENT3 ;
            9 : SCB . SYMBOLNR := COMMENT4 ;
            11 : SCB . SYMBOLNR := COMMENT5 ;
            22 : SCB . SYMBOLNR := HEXSTRINGCONST ;
            29 : SCB . SYMBOLNR := BINSTRINGCONST ;
            32 : SCB . SYMBOLNR := INTCONST ;
            36 : SCB . SYMBOLNR := INTCONST ;
            38 : SCB . SYMBOLNR := INTCONST ;
            42 : SCB . SYMBOLNR := INTCONST ;
            44 : SCB . SYMBOLNR := INTCONST ;
            47 : SCB . SYMBOLNR := INTDOTDOT ;
            50 : SCB . SYMBOLNR := REALCONST ;
            63 : SCB . SYMBOLNR := REALCONST ;
            65 : SCB . SYMBOLNR := REALCONST ;
            66 : SCB . SYMBOLNR := REALCONST ;
            67 : SCB . SYMBOLNR := IDENT ;
            68 : SCB . SYMBOLNR := IDENT ;
            70 : SCB . SYMBOLNR := SYRPARENT ;
            71 : SCB . SYMBOLNR := SYLBRACK ;
            75 : SCB . SYMBOLNR := SYRBRACK ;
            79 : SCB . SYMBOLNR := SYCOMMA ;
            80 : SCB . SYMBOLNR := SYSEMICOLON ;
            82 : SCB . SYMBOLNR := SYARROW ;
            85 : SCB . SYMBOLNR := SYDOTDOT ;
            87 : SCB . SYMBOLNR := SYPLUS ;
            89 : SCB . SYMBOLNR := SYMULT ;
            91 : SCB . SYMBOLNR := SYEQOP ;
            93 : SCB . SYMBOLNR := SYNEOP ;
            97 : SCB . SYMBOLNR := SYGEOP ;
            99 : SCB . SYMBOLNR := SYLEOP ;
            100 : SCB . SYMBOLNR := SYOROP ;
            101 : SCB . SYMBOLNR := SYANDOP ;
            103 : SCB . SYMBOLNR := SYASSIGN ;
            104 : SCB . SYMBOLNR := SYLPARENT ;
            105 : SCB . SYMBOLNR := SYMINUS ;
            106 : SCB . SYMBOLNR := SYPERIOD ;
            107 : SCB . SYMBOLNR := SYSLASH ;
            108 : SCB . SYMBOLNR := INTCONST ;
            109 : SCB . SYMBOLNR := INTCONST ;
            110 : SCB . SYMBOLNR := SYCOLON ;
            111 : SCB . SYMBOLNR := SYLTOP ;
            112 : SCB . SYMBOLNR := SYGTOP ;
            113 : SCB . SYMBOLNR := IDENT ;
            114 : SCB . SYMBOLNR := IDENT ;
            115 : SCB . SYMBOLNR := IDENT ;
            116 : SCB . SYMBOLNR := IDENT ;
            117 : SCB . SYMBOLNR := STRINGCONST ;
          end (* case *) ;
        if FALSE then
          WRITELN ( 'zust = ' , ALTZUST , ' symb = ' , SCB . SYMBOLNR :
                    20 , ' ' , SCB . SYMBOL : SCB . LSYMBOL ) ;

        /***************************************************/
        /*   Umsetzen in Grossbuchstaben, wo gefordert     */
        /***************************************************/

        case SCB . SYMBOLNR of
        end (* case *) ;

        /***************************************************/
        /*   Uebersetzen Keywords                          */
        /***************************************************/

        case SCB . SYMBOLNR of
        end (* case *) ;

        /*****************************************/
        /* ausgabe fuer -H- zunaechst ausgesetzt */
        /*****************************************/

      end (* SCANNER2 *) ;


   procedure INIT ( var SCB : SCAN_BLOCK ) ;

      const MSG1 = 'symbol not known to source program scanner' ;
            MSG2 = 'unexpected end of source file' ;
            MSG3 = 'unexpected char in options string' ;
            MSG4 = 'closing paranthese expected in options string' ;
            MSG5 = 'the rest of the options string will be ignored' ;

      var C : SCAN_ERRCLASS ;
          N : INTEGER ;

      begin (* INIT *)
        SCB . FTTAB := ALLOC ( SIZEOF ( SCAN_FTTAB1 ) ) ;
        for C := 'A' to 'Z' do
          for N := 1 to MAXERRNO do
            with SCB . FTTAB -> [ C ] [ N ] do
              begin
                FTEXT := NIL ;
                FTEXT_LEN := 0
              end (* with *) ;
        PASSCANF ( SCB , 'S' , 'S' , 1 , MSG1 , SIZEOF ( MSG1 ) ) ;
        PASSCANF ( SCB , 'S' , 'S' , 2 , MSG2 , SIZEOF ( MSG2 ) ) ;
        PASSCANF ( SCB , 'S' , 'S' , 3 , MSG3 , SIZEOF ( MSG3 ) ) ;
        PASSCANF ( SCB , 'S' , 'S' , 4 , MSG4 , SIZEOF ( MSG4 ) ) ;
        PASSCANF ( SCB , 'S' , 'S' , 5 , MSG5 , SIZEOF ( MSG5 ) ) ;
      end (* INIT *) ;


   procedure SCAN0000 ;

      begin (* SCAN0000 *)
        case ZUST of
          1 : begin
                if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '1' ] ) and (
                SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                  ZUST := 109
                else
                  if ( SCAN_CODE [ CH ] >= SCAN_CODE [ 'C' ] ) and (
                  SCAN_CODE [ CH ] <= SCAN_CODE [ 'W' ] ) then
                    ZUST := 67
                  else
                    if ( SCAN_CODE [ CH ] >= SCAN_CODE [ 'c' ] ) and (
                    SCAN_CODE [ CH ] <= SCAN_CODE [ 'w' ] ) then
                      ZUST := 67
                    else
                      case CH of
                        X'0A' : ZUST := 3 ;
                        X'0D' : ZUST := 2 ;
                        ' ' : ZUST := 3 ;
                        '"' : ZUST := 9 ;
                        '&' : ZUST := 101 ;
                        '''' : ZUST := 12 ;
                        '(' : ZUST := 104 ;
                        ')' : ZUST := 70 ;
                        '*' : ZUST := 89 ;
                        '+' : ZUST := 87 ;
                        ',' : ZUST := 79 ;
                        '-' : ZUST := 105 ;
                        '.' : ZUST := 106 ;
                        '/' : ZUST := 107 ;
                        '0' : ZUST := 108 ;
                        ':' : ZUST := 110 ;
                        ';' : ZUST := 80 ;
                        '<' : ZUST := 111 ;
                        '=' : ZUST := 91 ;
                        '>' : ZUST := 112 ;
                        '@' : ZUST := 82 ;
                        'A' : ZUST := 67 ;
                        'B' : ZUST := 113 ;
                        'X' : ZUST := 114 ;
                        'Y' : ZUST := 67 ;
                        'Z' : ZUST := 67 ;
                        '[' : ZUST := 71 ;
                        ']' : ZUST := 75 ;
                        '^' : ZUST := 82 ;
                        'a' : ZUST := 67 ;
                        'b' : ZUST := 115 ;
                        'x' : ZUST := 116 ;
                        'y' : ZUST := 67 ;
                        'z' : ZUST := 67 ;
                        '{' : ZUST := 8 ;
                        '|' : ZUST := 100 ;
                        otherwise
                          ZUST := - 1 ;
                      end (* case *) ;
              end (* tag/ca *) ;
          3 : begin
                case CH of
                  X'0A' : ZUST := 3 ;
                  ' ' : ZUST := 3 ;
                  otherwise
                    ZUST := - 1 ;
                end (* case *) ;
              end (* tag/ca *) ;
          otherwise
            CASE_FOUND := FALSE
        end (* case *)
      end (* SCAN0000 *) ;


   procedure SCAN0010 ;

      begin (* SCAN0010 *)
        case ZUST of
          12 : begin
                 if ( SCAN_CODE [ CH ] >= X'01' ) and ( SCAN_CODE [ CH
                 ] <= X'09' ) then
                   ZUST := 13
                 else
                   if ( SCAN_CODE [ CH ] >= X'0b' ) and ( SCAN_CODE [
                   CH ] <= SCAN_CODE [ '&' ] ) then
                     ZUST := 13
                   else
                     if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '(' ] ) and (
                     SCAN_CODE [ CH ] <= X'ff' ) then
                       ZUST := 13
                     else
                       case CH of
                         '''' : ZUST := 117 ;
                         otherwise
                           ZUST := - 1 ;
                       end (* case *) ;
               end (* tag/ca *) ;
          13 : begin
                 if ( SCAN_CODE [ CH ] >= X'01' ) and ( SCAN_CODE [ CH
                 ] <= X'09' ) then
                   ZUST := 13
                 else
                   if ( SCAN_CODE [ CH ] >= X'0b' ) and ( SCAN_CODE [
                   CH ] <= SCAN_CODE [ '&' ] ) then
                     ZUST := 13
                   else
                     if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '(' ] ) and (
                     SCAN_CODE [ CH ] <= X'ff' ) then
                       ZUST := 13
                     else
                       case CH of
                         '''' : ZUST := 117 ;
                         otherwise
                           ZUST := - 1 ;
                       end (* case *) ;
               end (* tag/ca *) ;
          17 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 19
                 else
                   case CH of
                     'A' : ZUST := 19 ;
                     'B' : ZUST := 19 ;
                     'C' : ZUST := 19 ;
                     'D' : ZUST := 19 ;
                     'E' : ZUST := 19 ;
                     'F' : ZUST := 19 ;
                     'a' : ZUST := 19 ;
                     'b' : ZUST := 19 ;
                     'c' : ZUST := 19 ;
                     'd' : ZUST := 19 ;
                     'e' : ZUST := 19 ;
                     'f' : ZUST := 19 ;
                     otherwise
                       ZUST := - 1 ;
                   end (* case *) ;
               end (* tag/ca *) ;
          19 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 19
                 else
                   case CH of
                     '''' : ZUST := 22 ;
                     'A' : ZUST := 19 ;
                     'B' : ZUST := 19 ;
                     'C' : ZUST := 19 ;
                     'D' : ZUST := 19 ;
                     'E' : ZUST := 19 ;
                     'F' : ZUST := 19 ;
                     '_' : ZUST := 20 ;
                     'a' : ZUST := 19 ;
                     'b' : ZUST := 19 ;
                     'c' : ZUST := 19 ;
                     'd' : ZUST := 19 ;
                     'e' : ZUST := 19 ;
                     'f' : ZUST := 19 ;
                     otherwise
                       ZUST := - 1 ;
                   end (* case *) ;
               end (* tag/ca *) ;
          otherwise
            CASE_FOUND := FALSE
        end (* case *)
      end (* SCAN0010 *) ;


   procedure SCAN0020 ;

      begin (* SCAN0020 *)
        case ZUST of
          20 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 21
                 else
                   case CH of
                     'A' : ZUST := 21 ;
                     'B' : ZUST := 21 ;
                     'C' : ZUST := 21 ;
                     'D' : ZUST := 21 ;
                     'E' : ZUST := 21 ;
                     'F' : ZUST := 21 ;
                     'a' : ZUST := 21 ;
                     'b' : ZUST := 21 ;
                     'c' : ZUST := 21 ;
                     'd' : ZUST := 21 ;
                     'e' : ZUST := 21 ;
                     'f' : ZUST := 21 ;
                     otherwise
                       ZUST := - 1 ;
                   end (* case *) ;
               end (* tag/ca *) ;
          21 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 21
                 else
                   case CH of
                     '''' : ZUST := 22 ;
                     'A' : ZUST := 21 ;
                     'B' : ZUST := 21 ;
                     'C' : ZUST := 21 ;
                     'D' : ZUST := 21 ;
                     'E' : ZUST := 21 ;
                     'F' : ZUST := 21 ;
                     '_' : ZUST := 20 ;
                     'a' : ZUST := 21 ;
                     'b' : ZUST := 21 ;
                     'c' : ZUST := 21 ;
                     'd' : ZUST := 21 ;
                     'e' : ZUST := 21 ;
                     'f' : ZUST := 21 ;
                     otherwise
                       ZUST := - 1 ;
                   end (* case *) ;
               end (* tag/ca *) ;
          24 : begin
                 case CH of
                   '0' : ZUST := 26 ;
                   '1' : ZUST := 26 ;
                   otherwise
                     ZUST := - 1 ;
                 end (* case *) ;
               end (* tag/ca *) ;
          26 : begin
                 case CH of
                   '''' : ZUST := 29 ;
                   '0' : ZUST := 26 ;
                   '1' : ZUST := 26 ;
                   '_' : ZUST := 27 ;
                   otherwise
                     ZUST := - 1 ;
                 end (* case *) ;
               end (* tag/ca *) ;
          27 : begin
                 case CH of
                   '0' : ZUST := 28 ;
                   '1' : ZUST := 28 ;
                   otherwise
                     ZUST := - 1 ;
                 end (* case *) ;
               end (* tag/ca *) ;
          28 : begin
                 case CH of
                   '''' : ZUST := 29 ;
                   '0' : ZUST := 28 ;
                   '1' : ZUST := 28 ;
                   '_' : ZUST := 27 ;
                   otherwise
                     ZUST := - 1 ;
                 end (* case *) ;
               end (* tag/ca *) ;
          otherwise
            CASE_FOUND := FALSE
        end (* case *)
      end (* SCAN0020 *) ;


   procedure SCAN0030 ;

      begin (* SCAN0030 *)
        case ZUST of
          31 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 32
                 else
                   ZUST := - 1 ;
               end (* tag/ca *) ;
          32 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 32
                 else
                   case CH of
                     '_' : ZUST := 31 ;
                     otherwise
                       ZUST := - 1 ;
                   end (* case *) ;
               end (* tag/ca *) ;
          34 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 36
                 else
                   case CH of
                     'A' : ZUST := 36 ;
                     'B' : ZUST := 36 ;
                     'C' : ZUST := 36 ;
                     'D' : ZUST := 36 ;
                     'E' : ZUST := 36 ;
                     'F' : ZUST := 36 ;
                     'a' : ZUST := 36 ;
                     'b' : ZUST := 36 ;
                     'c' : ZUST := 36 ;
                     'd' : ZUST := 36 ;
                     'e' : ZUST := 36 ;
                     'f' : ZUST := 36 ;
                     otherwise
                       ZUST := - 1 ;
                   end (* case *) ;
               end (* tag/ca *) ;
          36 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 36
                 else
                   case CH of
                     'A' : ZUST := 36 ;
                     'B' : ZUST := 36 ;
                     'C' : ZUST := 36 ;
                     'D' : ZUST := 36 ;
                     'E' : ZUST := 36 ;
                     'F' : ZUST := 36 ;
                     '_' : ZUST := 37 ;
                     'a' : ZUST := 36 ;
                     'b' : ZUST := 36 ;
                     'c' : ZUST := 36 ;
                     'd' : ZUST := 36 ;
                     'e' : ZUST := 36 ;
                     'f' : ZUST := 36 ;
                     otherwise
                       ZUST := - 1 ;
                   end (* case *) ;
               end (* tag/ca *) ;
          37 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 38
                 else
                   case CH of
                     'A' : ZUST := 38 ;
                     'B' : ZUST := 38 ;
                     'C' : ZUST := 38 ;
                     'D' : ZUST := 38 ;
                     'E' : ZUST := 38 ;
                     'F' : ZUST := 38 ;
                     'a' : ZUST := 38 ;
                     'b' : ZUST := 38 ;
                     'c' : ZUST := 38 ;
                     'd' : ZUST := 38 ;
                     'e' : ZUST := 38 ;
                     'f' : ZUST := 38 ;
                     otherwise
                       ZUST := - 1 ;
                   end (* case *) ;
               end (* tag/ca *) ;
          38 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 38
                 else
                   case CH of
                     'A' : ZUST := 38 ;
                     'B' : ZUST := 38 ;
                     'C' : ZUST := 38 ;
                     'D' : ZUST := 38 ;
                     'E' : ZUST := 38 ;
                     'F' : ZUST := 38 ;
                     '_' : ZUST := 37 ;
                     'a' : ZUST := 38 ;
                     'b' : ZUST := 38 ;
                     'c' : ZUST := 38 ;
                     'd' : ZUST := 38 ;
                     'e' : ZUST := 38 ;
                     'f' : ZUST := 38 ;
                     otherwise
                       ZUST := - 1 ;
                   end (* case *) ;
               end (* tag/ca *) ;
          otherwise
            CASE_FOUND := FALSE
        end (* case *)
      end (* SCAN0030 *) ;


   procedure SCAN0040 ;

      begin (* SCAN0040 *)
        case ZUST of
          40 : begin
                 case CH of
                   '0' : ZUST := 42 ;
                   '1' : ZUST := 42 ;
                   otherwise
                     ZUST := - 1 ;
                 end (* case *) ;
               end (* tag/ca *) ;
          42 : begin
                 case CH of
                   '0' : ZUST := 42 ;
                   '1' : ZUST := 42 ;
                   '_' : ZUST := 43 ;
                   otherwise
                     ZUST := - 1 ;
                 end (* case *) ;
               end (* tag/ca *) ;
          43 : begin
                 case CH of
                   '0' : ZUST := 44 ;
                   '1' : ZUST := 44 ;
                   otherwise
                     ZUST := - 1 ;
                 end (* case *) ;
               end (* tag/ca *) ;
          44 : begin
                 case CH of
                   '0' : ZUST := 44 ;
                   '1' : ZUST := 44 ;
                   '_' : ZUST := 43 ;
                   otherwise
                     ZUST := - 1 ;
                 end (* case *) ;
               end (* tag/ca *) ;
          otherwise
            CASE_FOUND := FALSE
        end (* case *)
      end (* SCAN0040 *) ;


   procedure SCAN0050 ;

      begin (* SCAN0050 *)
        case ZUST of
          50 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 50
                 else
                   ZUST := - 1 ;
               end (* tag/ca *) ;
          52 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 57
                 else
                   case CH of
                     '+' : ZUST := 53 ;
                     '-' : ZUST := 55 ;
                     otherwise
                       ZUST := - 1 ;
                   end (* case *) ;
               end (* tag/ca *) ;
          53 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 54
                 else
                   ZUST := - 1 ;
               end (* tag/ca *) ;
          54 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 118
                 else
                   ZUST := - 1 ;
               end (* tag/ca *) ;
          55 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 56
                 else
                   ZUST := - 1 ;
               end (* tag/ca *) ;
          56 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 119
                 else
                   ZUST := - 1 ;
               end (* tag/ca *) ;
          57 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 120
                 else
                   ZUST := - 1 ;
               end (* tag/ca *) ;
          59 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 60
                 else
                   ZUST := - 1 ;
               end (* tag/ca *) ;
          otherwise
            CASE_FOUND := FALSE
        end (* case *)
      end (* SCAN0050 *) ;


   procedure SCAN0060 ;

      begin (* SCAN0060 *)
        case ZUST of
          60 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 60
                 else
                   case CH of
                     'E' : ZUST := 61 ;
                     otherwise
                       ZUST := - 1 ;
                   end (* case *) ;
               end (* tag/ca *) ;
          61 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 66
                 else
                   case CH of
                     '+' : ZUST := 62 ;
                     '-' : ZUST := 64 ;
                     otherwise
                       ZUST := - 1 ;
                   end (* case *) ;
               end (* tag/ca *) ;
          62 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 63
                 else
                   ZUST := - 1 ;
               end (* tag/ca *) ;
          63 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 63
                 else
                   ZUST := - 1 ;
               end (* tag/ca *) ;
          64 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 65
                 else
                   ZUST := - 1 ;
               end (* tag/ca *) ;
          65 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 65
                 else
                   ZUST := - 1 ;
               end (* tag/ca *) ;
          66 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 66
                 else
                   ZUST := - 1 ;
               end (* tag/ca *) ;
          67 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 68
                 else
                   if ( SCAN_CODE [ CH ] >= SCAN_CODE [ 'A' ] ) and (
                   SCAN_CODE [ CH ] <= SCAN_CODE [ 'Z' ] ) then
                     ZUST := 68
                   else
                     if ( SCAN_CODE [ CH ] >= SCAN_CODE [ 'a' ] ) and (
                     SCAN_CODE [ CH ] <= SCAN_CODE [ 'z' ] ) then
                       ZUST := 68
                     else
                       case CH of
                         '$' : ZUST := 68 ;
                         '_' : ZUST := 68 ;
                         otherwise
                           ZUST := - 1 ;
                       end (* case *) ;
               end (* tag/ca *) ;
          68 : begin
                 if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                 SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                   ZUST := 68
                 else
                   if ( SCAN_CODE [ CH ] >= SCAN_CODE [ 'A' ] ) and (
                   SCAN_CODE [ CH ] <= SCAN_CODE [ 'Z' ] ) then
                     ZUST := 68
                   else
                     if ( SCAN_CODE [ CH ] >= SCAN_CODE [ 'a' ] ) and (
                     SCAN_CODE [ CH ] <= SCAN_CODE [ 'z' ] ) then
                       ZUST := 68
                     else
                       case CH of
                         '$' : ZUST := 68 ;
                         '_' : ZUST := 68 ;
                         otherwise
                           ZUST := - 1 ;
                       end (* case *) ;
               end (* tag/ca *) ;
          otherwise
            CASE_FOUND := FALSE
        end (* case *)
      end (* SCAN0060 *) ;


   procedure SCAN0100 ;

      begin (* SCAN0100 *)
        case ZUST of
          104 : begin
                  case CH of
                    '*' : ZUST := 7 ;
                    '.' : ZUST := 71 ;
                    '/' : ZUST := 71 ;
                    otherwise
                      ZUST := - 1 ;
                  end (* case *) ;
                end (* tag/ca *) ;
          105 : begin
                  case CH of
                    '>' : ZUST := 82 ;
                    otherwise
                      ZUST := - 1 ;
                  end (* case *) ;
                end (* tag/ca *) ;
          106 : begin
                  case CH of
                    ')' : ZUST := 75 ;
                    '.' : ZUST := 85 ;
                    otherwise
                      ZUST := - 1 ;
                  end (* case *) ;
                end (* tag/ca *) ;
          107 : begin
                  case CH of
                    ')' : ZUST := 75 ;
                    '*' : ZUST := 5 ;
                    '/' : ZUST := 11 ;
                    otherwise
                      ZUST := - 1 ;
                  end (* case *) ;
                end (* tag/ca *) ;
          108 : begin
                  if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                  SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                    ZUST := 109
                  else
                    case CH of
                      '.' : ZUST := 121 ;
                      'B' : ZUST := 40 ;
                      'E' : ZUST := 52 ;
                      'X' : ZUST := 34 ;
                      '_' : ZUST := 31 ;
                      'b' : ZUST := 40 ;
                      'x' : ZUST := 34 ;
                      otherwise
                        ZUST := - 1 ;
                    end (* case *) ;
                end (* tag/ca *) ;
          109 : begin
                  if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                  SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                    ZUST := 109
                  else
                    case CH of
                      '.' : ZUST := 121 ;
                      'E' : ZUST := 52 ;
                      '_' : ZUST := 31 ;
                      otherwise
                        ZUST := - 1 ;
                    end (* case *) ;
                end (* tag/ca *) ;
          otherwise
            CASE_FOUND := FALSE
        end (* case *)
      end (* SCAN0100 *) ;


   procedure SCAN0110 ;

      begin (* SCAN0110 *)
        case ZUST of
          110 : begin
                  case CH of
                    '=' : ZUST := 103 ;
                    otherwise
                      ZUST := - 1 ;
                  end (* case *) ;
                end (* tag/ca *) ;
          111 : begin
                  case CH of
                    '=' : ZUST := 99 ;
                    '>' : ZUST := 93 ;
                    otherwise
                      ZUST := - 1 ;
                  end (* case *) ;
                end (* tag/ca *) ;
          112 : begin
                  case CH of
                    '=' : ZUST := 97 ;
                    otherwise
                      ZUST := - 1 ;
                  end (* case *) ;
                end (* tag/ca *) ;
          113 : begin
                  if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                  SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                    ZUST := 68
                  else
                    if ( SCAN_CODE [ CH ] >= SCAN_CODE [ 'A' ] ) and (
                    SCAN_CODE [ CH ] <= SCAN_CODE [ 'Z' ] ) then
                      ZUST := 68
                    else
                      if ( SCAN_CODE [ CH ] >= SCAN_CODE [ 'a' ] ) and
                      ( SCAN_CODE [ CH ] <= SCAN_CODE [ 'z' ] ) then
                        ZUST := 68
                      else
                        case CH of
                          '$' : ZUST := 68 ;
                          '''' : ZUST := 24 ;
                          '_' : ZUST := 68 ;
                          otherwise
                            ZUST := - 1 ;
                        end (* case *) ;
                end (* tag/ca *) ;
          114 : begin
                  if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                  SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                    ZUST := 68
                  else
                    if ( SCAN_CODE [ CH ] >= SCAN_CODE [ 'A' ] ) and (
                    SCAN_CODE [ CH ] <= SCAN_CODE [ 'Z' ] ) then
                      ZUST := 68
                    else
                      if ( SCAN_CODE [ CH ] >= SCAN_CODE [ 'a' ] ) and
                      ( SCAN_CODE [ CH ] <= SCAN_CODE [ 'z' ] ) then
                        ZUST := 68
                      else
                        case CH of
                          '$' : ZUST := 68 ;
                          '''' : ZUST := 17 ;
                          '_' : ZUST := 68 ;
                          otherwise
                            ZUST := - 1 ;
                        end (* case *) ;
                end (* tag/ca *) ;
          115 : begin
                  if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                  SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                    ZUST := 68
                  else
                    if ( SCAN_CODE [ CH ] >= SCAN_CODE [ 'A' ] ) and (
                    SCAN_CODE [ CH ] <= SCAN_CODE [ 'Z' ] ) then
                      ZUST := 68
                    else
                      if ( SCAN_CODE [ CH ] >= SCAN_CODE [ 'a' ] ) and
                      ( SCAN_CODE [ CH ] <= SCAN_CODE [ 'z' ] ) then
                        ZUST := 68
                      else
                        case CH of
                          '$' : ZUST := 68 ;
                          '''' : ZUST := 24 ;
                          '_' : ZUST := 68 ;
                          otherwise
                            ZUST := - 1 ;
                        end (* case *) ;
                end (* tag/ca *) ;
          116 : begin
                  if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                  SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                    ZUST := 68
                  else
                    if ( SCAN_CODE [ CH ] >= SCAN_CODE [ 'A' ] ) and (
                    SCAN_CODE [ CH ] <= SCAN_CODE [ 'Z' ] ) then
                      ZUST := 68
                    else
                      if ( SCAN_CODE [ CH ] >= SCAN_CODE [ 'a' ] ) and
                      ( SCAN_CODE [ CH ] <= SCAN_CODE [ 'z' ] ) then
                        ZUST := 68
                      else
                        case CH of
                          '$' : ZUST := 68 ;
                          '''' : ZUST := 17 ;
                          '_' : ZUST := 68 ;
                          otherwise
                            ZUST := - 1 ;
                        end (* case *) ;
                end (* tag/ca *) ;
          117 : begin
                  case CH of
                    '''' : ZUST := 13 ;
                    otherwise
                      ZUST := - 1 ;
                  end (* case *) ;
                end (* tag/ca *) ;
          118 : begin
                  if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                  SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                    ZUST := 118
                  else
                    case CH of
                      '.' : ZUST := 59 ;
                      otherwise
                        ZUST := - 1 ;
                    end (* case *) ;
                end (* tag/ca *) ;
          119 : begin
                  if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                  SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                    ZUST := 119
                  else
                    case CH of
                      '.' : ZUST := 59 ;
                      otherwise
                        ZUST := - 1 ;
                    end (* case *) ;
                end (* tag/ca *) ;
          otherwise
            CASE_FOUND := FALSE
        end (* case *)
      end (* SCAN0110 *) ;


   procedure SCAN0120 ;

      begin (* SCAN0120 *)
        case ZUST of
          120 : begin
                  if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                  SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                    ZUST := 120
                  else
                    case CH of
                      '.' : ZUST := 59 ;
                      otherwise
                        ZUST := - 1 ;
                    end (* case *) ;
                end (* tag/ca *) ;
          121 : begin
                  if ( SCAN_CODE [ CH ] >= SCAN_CODE [ '0' ] ) and (
                  SCAN_CODE [ CH ] <= SCAN_CODE [ '9' ] ) then
                    ZUST := 50
                  else
                    case CH of
                      '.' : ZUST := 47 ;
                      otherwise
                        ZUST := - 1 ;
                    end (* case *) ;
                end (* tag/ca *) ;
          otherwise
            CASE_FOUND := FALSE
        end (* case *)
      end (* SCAN0120 *) ;


   begin (* PASSCAN *)
     if SCANNER_INIT = 0 then
       begin
         SCANNER_INIT := 1 ;

     /*********************************************/
     /* einmalige initialisierungs aktivitaeten   */
     /* wenn statischer schalter null ist         */
     /*********************************************/

         INIT ( SCB ) ;
         INIT_SCAN_CODE
       end (* then *) ;
     SCB . SYMBOL := '' ;
     if SCB . MODUS > 0 then
       begin
         SCAN_READC ( SCANINP , SCANOUT , SCB , CH ) ;
         SCB . LOOKAHEAD := CH ;
         SCB . MODUS := 0
       end (* then *)
     else
       CH := SCB . LOOKAHEAD ;
     if SCB . DATEIENDE <> 0 then
       begin
         SCB . DATEIENDE := SCB . DATEIENDE + 1 ;
         if SCB . DATEIENDE > 5 then
           PASSCANE ( SCB , 'F' , 'S' , 1 , ' ' , SCB . LINENR , SCB .
                      LINEPOS ) ;
         SCB . SYMBOLNR := SYMB_EOF ;
         SCB . LSYMBOL := 1 ;
         SCB . SYMBOL [ 1 ] := CH ;
         return ;
       end (* then *) ;
     SCB . SYMBOLNR := SYMB_UNKNOWN ;
     SCB . LSYMBOL := 1 ;
     SCB . SYMBOL [ 1 ] := CH ;
     ZUST := 1 ;
     while ZUST > 0 do
       begin
         ALTZUST := ZUST ;
         CASE_FOUND := TRUE ;
         if ZUST in [ 2 , 5 , 7 , 8 , 9 , 11 , 22 , 29 , 47 , 70 , 71 ,
         75 , 79 , 80 , 82 , 85 , 87 , 89 , 91 , 93 , 97 , 99 , 100 ,
         101 , 103 ] then
           ZUST := - 1
         else
           case ZUST DIV 10 of
             0 : SCAN0000 ;
             1 : SCAN0010 ;
             2 : SCAN0020 ;
             3 : SCAN0030 ;
             4 : SCAN0040 ;
             5 : SCAN0050 ;
             6 : SCAN0060 ;
             10 : SCAN0100 ;
             11 : SCAN0110 ;
             12 : SCAN0120 ;
           end (* case *) ;
         if not CASE_FOUND then
           begin
             if FALSE then
               WRITELN ( 'zust not found line/Pos = ' , SCB . LINENR :
                         1 , '/' , SCB . LINEPOS : 1 , ' zust = ' ,
                         ZUST : 1 ) ;
             PASSCANE ( SCB , 'F' , 'S' , 2 , ' ' , SCB . LINENR , SCB
                        . LINEPOS ) ;
           end (* then *) ;
         if ZUST > 0 then
           begin
             SCAN_READC ( SCANINP , SCANOUT , SCB , CH ) ;
             if SCB . DATEIENDE <> 0 then
               break ;
             SCB . LOOKAHEAD := CH ;
             if SCB . LSYMBOL < SCB . MAXLSYMBOL - 1 then
               begin
                 SCB . LSYMBOL := SCB . LSYMBOL + 1 ;
                 SCB . SYMBOL [ SCB . LSYMBOL ] := CH ;
               end (* then *)
           end (* then *)
       end (* while *) ;
     if SCB . LSYMBOL <= 1 then
       begin
         if SCB . DATEIENDE = 0 then
           begin
             SCAN_READC ( SCANINP , SCANOUT , SCB , CH ) ;
             SCB . LOOKAHEAD := CH ;
             SCB . SYMBOLNR := SYMB_UNKNOWN ;
           end (* then *)
         else
           SCB . SYMBOLNR := SYMB_EOF
       end (* then *)
     else
       begin
         SCB . SYMBOL [ SCB . LSYMBOL ] := ' ' ;
         SCB . LSYMBOL := SCB . LSYMBOL - 1 ;
         SCANNER2 ( ALTZUST , SCB ) ;
         case SCB . SYMBOLNR of
           COMMENT1 :
             COMMENT ( SCANINP , SCANOUT , SCB , 1 ) ;
           COMMENT2 :
             COMMENT ( SCANINP , SCANOUT , SCB , 2 ) ;
           COMMENT3 :
             COMMENT ( SCANINP , SCANOUT , SCB , 3 ) ;
           COMMENT4 :
             COMMENT ( SCANINP , SCANOUT , SCB , 4 ) ;
           COMMENT5 :
             COMMENT ( SCANINP , SCANOUT , SCB , 5 ) ;
           otherwise
             
         end (* case *)
       end (* else *)
   end (* PASSCAN *) ;



begin (* HAUPTPROGRAMM *)
  
end (* HAUPTPROGRAMM *) .
