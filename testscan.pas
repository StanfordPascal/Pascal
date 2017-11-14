program TESTSCAN ( INPUT , OUTPUT , SCANINP , PROTOKOL ) ;

(********************************************************************)
(*$A+                                                               *)
(********************************************************************)
(* * zeichensatz-tabelle auf char umst. / geht derzeit nicht        *)
(* * integer hex konstanten werden nicht korrekt verarbeitet        *)
(* * syrange korrekt implementieren                                 *)
(* * fehler symb_unknown am ende ?                                  *)
(* * verschiedene Kommentartypen implementieren (verschachtelt)     *)
(* * umstellen auf Module                                           *)
(* - siehe auch Compiler-Probleme in Datei PLAN.TXT                 *)
(********************************************************************)



const MAXLSIZE = 120 ;

      /******************************************************/
      /*   Beim Host: \n = 0x15 => 0x0a                     */
      /******************************************************/
      /*   0x10, 0x11, 0x12, 0x13, 0xef, 0xc5, 0x08, 0xcb,  */
      /******************************************************/
      /******************************************************/
      /*   0xb7, 0xb8, 0xb9, 0xbb, 0xc4, 0x0a, 0x17, 0x1b,  */
      /******************************************************/

      EBCDIC_TO_ASCII = X'00010203_EC09CA7F_E2D2D30B_0C0D0EA9'
      X'10111213_EF0A08CB_1819DCD8_1C1D1E1F'
      X'B7B8B9BB_C4C5171B_CCCDCFD0_D1050607'
      X'D9DA16DD_DEDFE004_E3E5E9EB_B0B19E1A'
      X'20C9837B_85A0F286_87A48E2E_3C282B21'
      X'26828889_8AA18C8B_8D7E9A24_2A293B5E'
      X'2D2FB25B_B4B5B68F_80A5942C_255F3E3F'
      X'BA90BCBD_BEF3C0C1_C2603A23_15273D22'
      X'C3616263_64656667_6869AEAF_C6C7C8F1'
      X'F86A6B6C_6D6E6F70_7172A6A7_91CE920F'
      X'E6E17374_75767778_797AADA8_D4D5D6D7'
      X'9B9C9DFA_9F4014AC_ABFCAAB3_E4FEBFE7'
      X'84414243_44454647_4849E893_7C95A2ED'
      X'814A4B4C_4D4E4F50_5152EE96_7D97A398'
      X'99F05354_55565758_595AFDF5_5CF7F6F9'
      X'30313233_34353637_3839DBFB_5DF4' ;


type SOURCELINE = array [ 1 .. MAXLSIZE ] of CHAR ;
     CHAR32 = array [ 1 .. 32 ] of CHAR ;
     SYMB = ( SYMB_EOF , SYMB_UNKNOWN , EOLCHAR , SEPARATOR , COMMENT1
            , COMMENT2 , COMMENT3 , COMMENT4 , COMMENT5 , STRING ,
            HEXSTRING , BITSTRING , INUMBER , RNUMBER , IDENT , SYKLAUF
            , SYKLZU , SYECKAUF , SYECKZU , SYPOINTER , SYPUNKT ,
            SYRANGE , SYDOPU , SYKOMMA , SYSTRIPU , SYPLUS , SYMINUS ,
            SYMULT , SYSLASH , SYEQOP , SYNEOP , SYGTOP , SYLTOP ,
            SYGEOP , SYLEOP , SYOROP , SYANDOP , SYASSIGN ) ;

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
                    FEANFANG : ANYPTR ;
                    FEAKT : ANYPTR ;
                    FTTAB : ANYPTR ;
                    FTTABA : ANYPTR ;
                    POPT : OPTIONS_PTR ;

     /******************************************/
     /* felder fuer sofortige Protokollausgabe */
     /******************************************/

                    PROTOUT : BOOLEAN ;
                    TERMOUT : BOOLEAN ;
                    FEAKT_ALT : ANYPTR ;
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


var SCB : SCAN_BLOCK ;
    OPT : COMP_OPTIONS ;
    SCANINP : TEXT ;
    PROTOKOL : TEXT ;
    INFO : CHAR32 ;
    I : INTEGER ;



procedure PASSCANS ( var SCANOUT : TEXT ; var SCB : SCAN_BLOCK ) ;

   EXTERNAL ;



procedure PASSCANL ( var SCANINP : TEXT ; var SCANOUT : TEXT ; var SCB
                   : SCAN_BLOCK ; ALLES : BOOLEAN ) ;

   EXTERNAL ;



procedure PASSCANE ( var SCB : SCAN_BLOCK ; ERRLEVEL : CHAR ; ERRCLASS
                   : CHAR ; I : INTEGER ; INFO : CHAR32 ; ZEILNR :
                   INTEGER ; PLATZ : INTEGER ) ;

   EXTERNAL ;



procedure PASSCAN ( var SCANINP : TEXT ; var SCANOUT : TEXT ; var SCB :
                  SCAN_BLOCK ) ;

   EXTERNAL ;



begin (* HAUPTPROGRAMM *)
  MEMSET ( ADDR ( SCB ) , CHR ( 0 ) , SIZEOF ( SCB ) ) ;
  MEMSET ( ADDR ( OPT ) , CHR ( 0 ) , SIZEOF ( OPT ) ) ;
  REWRITE ( PROTOKOL ) ;
  SCB . MAXLSYMBOL := MAXLSIZE ;
  SCB . MODUS := 1 ;
  SCB . SYMBOLNR := SYMB_UNKNOWN ;
  SCB . SLINE := ' ' ;
  SCB . LINENR := 0 ;
  SCB . LINEPOS := 1 ;
  SCB . LINELEN := 0 ;
  SCB . FEANFANG := NIL ;
  SCB . FTTAB := NIL ;
  SCB . FTTABA := NIL ;
  SCB . POPT := ADDR ( OPT ) ;
  SCB . FEAKT := NIL ;
  SCB . FEAKT_ALT := NIL ;
  SCB . PROTOUT := TRUE ;
  SCB . TERMOUT := TRUE ;
  SCB . LINEINFO := 'HUGOXXXX 001' ;
  SCB . LINEINFO_SIZE := 11 ;
  SCB . HEADLINE := '1Test Ueberschrift fuer Pascal Scanner' ;
  SCB . HEADLINE_SIZE := 61 ;
  SCB . LINECOUNT := 100 ;
  OPT . PAGESIZE := 72 ;
  while SCB . SYMBOLNR <> SYMB_EOF do
    begin
      PASSCAN ( SCANINP , PROTOKOL , SCB ) ;
      if FALSE then
        if SCB . SYMBOLNR <> SEPARATOR then
          WRITELN ( 'pos = ' , SCB . LINENR : 4 , '/' , SCB . LINEPOS :
                    3 , ' symb = ' , SCB . SYMBOLNR : 20 , ' ' , SCB .
                    SYMBOL : SCB . LSYMBOL ) ;
      if SCB . SYMBOLNR = SYMB_UNKNOWN then
        begin
          PASSCANE ( SCB , 'F' , 'S' , 1 , ' ' , SCB . LINENR , SCB .
                     LINEPOS ) ;
        end (* then *) ;
      if FALSE then
        begin
          if SCB . SYMBOLNR = IDENT then
            begin
              INFO := ' ' ;
              for I := 1 to 32 do
                INFO [ I ] := SCB . SYMBOL [ I ] ;
              PASSCANE ( SCB , 'I' , 'A' , 1 , INFO , SCB . LINENR ,
                         SCB . LINEPOS ) ;
            end (* then *) ;
        end (* then *)
    end (* while *) ;
  if FALSE then
    PASSCANL ( SCANINP , PROTOKOL , SCB , TRUE ) ;
  PASSCANS ( PROTOKOL , SCB ) ;
  PASSCANS ( OUTPUT , SCB ) ;
end (* HAUPTPROGRAMM *) .
