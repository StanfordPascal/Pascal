program XCOMP ( INPUT , OUTPUT , INFILE1 , INFILE2 ) ;

(***************************************************************)
(*                                                             *)
(*   XCOMP.C                                                   *)
(*                                                             *)
(*   Vergleich zweier Dateien analog DIFF;                     *)
(*   erzeugt Eingabe fuer XUPD                                 *)
(*                                                             *)
(*   angepasst und ueberarbeitet / xs95opp / 28.6.1998         *)
(*                                                             *)
(***************************************************************)
(*                                                             *)
(*   Offene Punkte:                                            *)
(*                                                             *)
(*   - abbrechen, wenn Synchro nicht klappt                    *)
(*                                                             *)
(***************************************************************)
(***************************************************************)
(*                                                             *)
(*   XCOMP <file1> <file2> [optionen]                          *)
(*                                                             *)
(*   <file1> und <file2> sind die beiden zu vergleichenden     *)
(*   Dateien.                                                  *)
(*                                                             *)
(*   Moegliche Optionen werden durch Eingabe von               *)
(*   XCOMP ohne Parameter angezeigt (siehe auch                *)
(*   Funktion cmndformat)                                      *)
(*                                                             *)
(*   Alte Funktionen ohne Garantie:                            *)
(*                                                             *)
(*   ------ following for update deck generation only ------   *)
(*                                                             *)
(*   -U[<update deck>]                                         *)
(*                   Generate update deck to make file1        *)
(*                   into file2; <update deck> defaults to     *)
(*                   the <name of file1>.UPT                   *)
(*   -sx,y           Generate internal sequence numbers        *)
(*                   starting with x and increment by y.       *)
(*                   Default if -s is omitted is 1,1           *)
(*                   if -s is specified without x and y        *)
(*                   then x = 10000; y defaults to x           *)
(*   -x              File1 already contains sequence numbers   *)
(*                                                             *)
(*   ------ following for aide with editor shells only -----   *)
(*                                                             *)
(*   -i              Use a sequence file                       *)
(*                   (generated from UPDATE command)           *)
(*                   name is <name of file1>.SEQ               *)
(*                   will be deleted                           *)
(*                                                             *)
(*   ------ neue bzw. noch gueltige Optionen ---------------   *)
(*   ------ nicht alle werden in der Pascal-Version --------   *)
(*   ------ bzw. im CMS funktionieren ----------------------   *)
(*                                                             *)
(*   Folgende Optionen sind moeglich:                          *)
(*                                                             *)
(*   -a              Vergleiche ALLIANZ-Testfalldateien        *)
(*                                                             *)
(*   -b              Einlesen mit fgets, nicht binaer          *)
(*                                                             *)
(*   -cx,y           Nur Spalten x bis y vergleichen           *)
(*                   (evtl. auch -v setzen)                    *)
(*                                                             *)
(*   -dx,y           Spalten x bis y nicht vergleichen         *)
(*                                                             *)
(*   -ex             Environment, d.h. auch die Umgebung       *)
(*                   einer Differenz anzeigen (# lines)        *)
(*                                                             *)
(*   -mn             Resync on n lines (default is 2)          *)
(*                                                             *)
(*   -t              Ignore case                               *)
(*                                                             *)
(*   -v              Trailing Blanks ignorieren                *)
(*                                                             *)
(*   -w              Ignore white space between words          *)
(*                                                             *)
(*   -*              Begrenzungszeichen in Vergleichsliste     *)
(*                                                             *)
(*                                                             *)
(***************************************************************)



const MAXLEN = 1024 ;
      VERSION = 'XCOMP (Pascal Version 1.1) - 20.10.2016' ;


type OSPARM_TYPE = array [ 1 .. 254 ] of CHAR ;
     CHAR3 = array [ 1 .. 3 ] of CHAR ;
     CHAR64 = array [ 1 .. 64 ] of CHAR ;
     ZEILE = array [ 1 .. MAXLEN ] of CHAR ;
     VOIDPTR = -> INTEGER ;

     /************************************************/
     /*   s_line:                                    */
     /*                                              */
     /*   next    - Pointer to the next line         */
     /*   seqnum  - Current sequence number          */
     /*   len     - Length of the current line       */
     /*   text    - Pointer to the current line      */
     /************************************************/

     LINEPTR = -> S_LINE ;
     S_LINE = record
                NEXT : LINEPTR ;
                LINENO : INTEGER ;
                SEQNUM : INTEGER ;
                LEN : INTEGER ;
                TEXT : ZEILE ;
              end ;

     /************************************************/
     /*   s_fanker:                                  */
     /*                                              */
     /*   curr     - Ptr to the current line buffer  */
     /*   head     - Ptr to the head of the list     */
     /*   tail     - Ptr to the last line in storage */
     /*   cline    - Line number of current          */
     /*   hline    - Line number of head             */
     /*   tline    - Line number of tail             */
     /************************************************/

     S_FANKER = record
                  FILENO : INTEGER ;
                  FILEH : CHAR ;
                  CURR : LINEPTR ;
                  CURR_SAVE : LINEPTR ;
                  HEAD : LINEPTR ;
                  TAIL : LINEPTR ;
                  CLINE : INTEGER ;
                  CLINE_SAVE : INTEGER ;
                  HLINE : INTEGER ;
                  TLINE : INTEGER ;

     /******************************************/
     /*  char *fbuffer;                        */
     /*  char *fbuf2;                          */
     /*  int lbuf2;                            */
     /*  char *cp2;                            */
     /******************************************/

                end ;

     /************************************************/
     /*   s_options:                                 */
     /*                                              */
     /*   edit      - Edit-File erzeugen ?           */
     /*   white     - ignore whitespace              */
     /*   case      - ignore case                    */
     /*   limits    - Begrenzungszeichen in Liste    */
     /*   colstart  - colstart fuer Vergleich        */
     /*   colend    - colende fuer Vergleich         */
     /*   ncolstart - colstart fuer Vergleich        */
     /*   ncolend   - colende fuer Vergleich         */
     /*   alli...   - ALLIANZ-Testfall               */
     /************************************************/

     S_OPTIONS = record
                   EDIT : BOOLEAN ;
                   TRAIL : BOOLEAN ;
                   WHITE : BOOLEAN ;
                   BINARY : BOOLEAN ;
                   ICASE : BOOLEAN ;
                   LIMITS : BOOLEAN ;
                   COLSTART : INTEGER ;
                   COLEND : INTEGER ;
                   NCOLSTART : INTEGER ;
                   NCOLEND : INTEGER ;
                   MINMATCH : INTEGER ;
                   ALLI_TESTFALL : BOOLEAN ;
                   ENVIRON : INTEGER ;
                 end ;

     /************************************************/
     /*   s_seqinfo:                                 */
     /*                                              */
     /*   seqtype  - Type of sequencing              */
     /*   seqstrt  - Start SeqNumber                 */
     /*   seqincr  - Increment SeqNumber             */
     /*   seqcur   - Current SeqNumber               */
     /*   seqname  - Name des SEQ-Files              */
     /*   lastmseq - last matched SeqNumber          */
     /************************************************/

     S_SEQINFO = record
                   SEQTYPE : INTEGER ;
                   SEQSTRT : INTEGER ;
                   SEQINCR : INTEGER ;
                   SEQCUR : INTEGER ;
                   SEQNAME : array [ 1 .. 258 ] of CHAR ;
                   LASTMSEQ : INTEGER ;
                 end ;


var PARM_FEHLER : BOOLEAN ;
    INFILE1 : TEXT ;
    INFILE2 : TEXT ;
    OPT : S_OPTIONS ;
    SEQ : S_SEQINFO ;
    DIFFERENT : BOOLEAN ;
    A : S_FANKER ;
    B : S_FANKER ;
    TRACE : BOOLEAN ;
    FCB1 : VOIDPTR ;
    FCB2 : VOIDPTR ;
    X : INTEGER ;
    PSTRING : OSPARM_TYPE ;



procedure DUMP ( PVON : VOIDPTR ; PBIS : VOIDPTR ) ;

(*********************************************************)
(*  Speicherbereich von PVON bis PBIS hexadezimal        *)
(*  ausgeben                                             *)
(*********************************************************)


   var P1 : VOIDPTR ;
       P2 : VOIDPTR ;
       MOD1 : INTEGER ;
       MOD2 : INTEGER ;


   procedure DUMPCHAR ( CH : CHAR ) ;

      begin (* DUMPCHAR *)
        if CH in [ 'a' .. 'i' , 'j' .. 'r' , 's' .. 'z' , 'A' .. 'I' ,
        'J' .. 'R' , 'S' .. 'Z' , '0' .. '9' ] then
          WRITE ( CH )
        else
          WRITE ( '.' )
      end (* DUMPCHAR *) ;


   procedure DUMPZEILE ( ADR : VOIDPTR ; P1 : VOIDPTR ; P2 : VOIDPTR )
                       ;

      type INT2PTR = record
                       case INTEGER of
                         0 :
                           ( P : VOIDPTR ) ;
                         1 :
                           ( I : INTEGER ) ;
                     end ;

      var IP : INT2PTR ;
          CH : -> CHAR ;
          I : INTEGER ;

      begin (* DUMPZEILE *)
        CH := PTRCAST ( ADR ) ;
        WRITE ( ADR , ': ' ) ;
        IP . I := ADR -> ;
        if ( PTRDIFF ( ADR , P1 ) < 0 ) or ( PTRDIFF ( ADR , P2 ) > 4 )
        then
          WRITE ( '........ ' )
        else
          WRITE ( IP . P , ' ' ) ;
        ADR := PTRADD ( ADR , 4 ) ;
        IP . I := ADR -> ;
        if ( PTRDIFF ( ADR , P1 ) < 0 ) or ( PTRDIFF ( ADR , P2 ) > 4 )
        then
          WRITE ( '........ ' )
        else
          WRITE ( IP . P , ' ' ) ;
        ADR := PTRADD ( ADR , 4 ) ;
        IP . I := ADR -> ;
        if ( PTRDIFF ( ADR , P1 ) < 0 ) or ( PTRDIFF ( ADR , P2 ) > 4 )
        then
          WRITE ( '........ ' )
        else
          WRITE ( IP . P , ' ' ) ;
        ADR := PTRADD ( ADR , 4 ) ;
        IP . I := ADR -> ;
        if ( PTRDIFF ( ADR , P1 ) < 0 ) or ( PTRDIFF ( ADR , P2 ) > 4 )
        then
          WRITE ( '........ ' )
        else
          WRITE ( IP . P , ' ' ) ;
        WRITE ( ' *' ) ;
        for I := 1 to 16 do
          begin
            DUMPCHAR ( CH -> ) ;
            CH := PTRADD ( CH , 1 )
          end (* for *) ;
        WRITELN ( '*' ) ;
      end (* DUMPZEILE *) ;


   begin (* DUMP *)
     WRITELN ( 'Dump Speicherbereich von ' , PVON , ' bis ' , PBIS ) ;
     P1 := PTRADD ( PVON , - 16 ) ;
     MOD1 := PTR2INT ( P1 ) MOD 16 ;
     P1 := PTRADD ( P1 , 16 - MOD1 ) ;
     P2 := PTRADD ( PBIS , 15 ) ;
     MOD2 := PTR2INT ( P2 ) MOD 16 ;
     P2 := PTRADD ( P2 , - MOD2 ) ;
     while PTRDIFF ( P1 , P2 ) < 0 do
       begin
         DUMPZEILE ( P1 , PVON , PBIS ) ;
         P1 := PTRADD ( P1 , 16 ) ;
       end (* while *) ;
   end (* DUMP *) ;



procedure CHECKFILE ( FCB : VOIDPTR ) ;

   var PBUFNO : -> CHAR ;
       PBUFCB : -> VOIDPTR ;
       PBUF : VOIDPTR ;
       BUFPTR : VOIDPTR ;
       PSIZE : VOIDPTR ;
       BUFSIZE : INTEGER ;
       BUFSIZE1 : INTEGER ;
       BUFSIZE2 : INTEGER ;
       CHKPTR : INTEGER ;
       CHKPTR2 : INTEGER ;

   begin (* CHECKFILE *)
     PBUFCB := PTRADD ( FCB , 56 ) ;
     PBUFNO := PTRCAST ( PBUFCB ) ;
     PBUF := PBUFCB -> ;
     PBUFCB := PTRCAST ( PBUF ) ;
     BUFPTR := PBUFCB -> ;
     PSIZE := PTRADD ( PBUFCB , 4 ) ;
     BUFSIZE := PSIZE -> ;
     BUFSIZE1 := BUFSIZE DIV 65536 ;
     BUFSIZE2 := BUFSIZE MOD 65536 ;
     WRITELN ( '*** checkfile +++ pbuf = ' , PBUF , ' bufptr = ' ,
               BUFPTR ) ;
     CHKPTR := PTR2INT ( BUFPTR ) ;
     CHKPTR2 := PTR2INT ( PBUF ) MOD 16777216 + 8 ;
     if ( BUFSIZE1 <> ORD ( PBUFNO -> ) ) or ( CHKPTR < CHKPTR2 ) or (
     CHKPTR > CHKPTR2 + ( BUFSIZE1 - 1 ) * BUFSIZE2 ) then
       begin
         WRITELN ( '+++ bufno von infile = ' , ORD ( PBUFNO -> ) ) ;
         WRITELN ( '+++ bufsize1 = ' , BUFSIZE1 ) ;
         WRITELN ( '+++ bufsize2 = ' , BUFSIZE2 ) ;
         WRITELN ( '+++ pbuf = ' , PBUF ) ;
         WRITELN ( '+++ bufptr = ' , BUFPTR ) ;
         WRITELN ( '+++ fcb des files:' ) ;
         DUMP ( FCB , PTRADD ( FCB , 132 ) ) ;
         WRITELN ( '+++ anfang buffer von infile:' ) ;
         DUMP ( PBUF , PTRADD ( PBUF , 64 ) ) ;
         EXIT ( 1999 ) ;
       end (* then *)
   end (* CHECKFILE *) ;



function TOLOWER ( X : CHAR ) : CHAR ;

   begin (* TOLOWER *)
     if X in [ 'A' .. 'I' , 'J' .. 'R' , 'S' .. 'Z' ] then
       TOLOWER := CHR ( ORD ( X ) - ORD ( 'A' ) + ORD ( 'a' ) )
     else
       TOLOWER := X
   end (* TOLOWER *) ;



procedure INITFANKER ( var X : S_FANKER ; DATEI : INTEGER ) ;

   begin (* INITFANKER *)
     X . FILENO := DATEI ;
     X . FILEH := CHR ( ORD ( '0' ) + DATEI ) ;
     X . CURR := NIL ;
     X . HEAD := NIL ;
     X . TAIL := NIL ;
     X . CLINE := 0 ;
     X . CLINE_SAVE := 0 ;
     X . HLINE := 0 ;
     X . TLINE := 0 ;
   end (* INITFANKER *) ;



procedure TRACE1 ( X : CHAR64 ) ;

   begin (* TRACE1 *)
     return ;
     WRITELN ( 'Trace1: ' , X ) ;
   end (* TRACE1 *) ;



procedure TRACE2 ( FILEH : CHAR ; L : LINEPTR ) ;

   var I : INTEGER ;

   begin (* TRACE2 *)
     return ;
     WRITELN ( 'gelesen von Datei ' , FILEH , ' (' , L , '):' ) ;
     for I := 1 to L -> . LEN do
       WRITE ( L -> . TEXT [ I ] ) ;
     WRITELN ;
   end (* TRACE2 *) ;



procedure TESTPUT ;

   begin (* TESTPUT *)
     return ;
     WRITELN ( 'Testput' ) ;
   end (* TESTPUT *) ;



function READLINE ( var X : S_FANKER ) : INTEGER ;

   type SHOWPTR = record
                    case BOOLEAN of
                      FALSE :
                        ( X1 : LINEPTR ) ;
                      TRUE :
                        ( X2 : INTEGER )
                  end ;

   var RC : INTEGER ;
       NEWLINE : LINEPTR ;
       L : INTEGER ;
       LAENGE : INTEGER ;
       GELESEN : BOOLEAN ;
       ANZ : INTEGER ;
       TEMP_LEN : INTEGER ;
       EOFX : BOOLEAN ;
       Z : SHOWPTR ;

   begin (* READLINE *)

     /************************************************/
     /*   Zeile einlesen aus ALLI-Testfalldatei      */
     /************************************************/

     if OPT . ALLI_TESTFALL then
       begin

     /***********************/
     /* zunaechst weglassen */
     /***********************/

         GELESEN := FALSE
       end (* then *)

     /************************************************/
     /*   Zeile einlesen binaer                      */
     /************************************************/

     else
       if OPT . BINARY then
         begin

     /***********************/
     /* zunaechst weglassen */
     /***********************/

           GELESEN := FALSE
         end (* then *)

     /************************************************/
     /*   Zeile einlesen normal Text                 */
     /************************************************/

       else
         begin
           if X . FILEH = '1' then
             EOFX := EOF ( INFILE1 )
           else
             EOFX := EOF ( INFILE2 ) ;
           if not EOFX then
             begin

     /************************************************/
     /*   WRITELN ( 'checkfile vor alloc' ) ;        */
     /*   if FCB1 <> NIL then                        */
     /*     CHECKFILE ( FCB1 ) ;                     */
     /*   if FCB2 <> NIL then                        */
     /*     CHECKFILE ( FCB2 )                       */
     /************************************************/

               NEWLINE := ALLOC ( SIZEOF ( S_LINE ) ) ;

     /************************************************/
     /*   WRITELN ( 'checkfile nach alloc' ) ;       */
     /*   if FCB1 <> NIL then                        */
     /*     CHECKFILE ( FCB1 ) ;                     */
     /*   if FCB2 <> NIL then                        */
     /*     CHECKFILE ( FCB2 )                       */
     /************************************************/

               with NEWLINE -> do
                 begin
                   NEXT := NIL ;
                   LINENO := 0 ;
                   SEQNUM := 0 ;
                   LEN := 0 ;
                   if X . FILEH = '1' then
                     begin
                       READ ( INFILE1 , TEXT ) ;
                       READLN ( INFILE1 ) ;
                     end (* then *)
                   else
                     begin
                       READ ( INFILE2 , TEXT ) ;
                       READLN ( INFILE2 ) ;
                     end (* else *)
                 end (* with *) ;
               GELESEN := TRUE
             end (* then *)
           else
             begin
               GELESEN := FALSE ;
               NEWLINE := NIL ;
             end (* else *) ;
           if GELESEN then
             begin
               LAENGE := MAXLEN ;
               while LAENGE > 0 do
                 begin
                   if NEWLINE -> . TEXT [ LAENGE ] <> ' ' then
                     break ;
                   LAENGE := LAENGE - 1 ;
                 end (* while *) ;
               NEWLINE -> . LEN := LAENGE ;
             end (* then *) ;
         end (* else *) ;
     if GELESEN then
       begin
         if X . TAIL = NIL then
           begin
             X . HEAD := NEWLINE ;
             X . TLINE := 1 ;
             X . HLINE := 1 ;
           end (* then *)
         else
           begin
             X . TAIL -> . NEXT := NEWLINE ;
             X . TLINE := X . TLINE + 1 ;
           end (* else *) ;
         X . TAIL := NEWLINE ;
         NEWLINE -> . LINENO := X . TLINE ;
       end (* then *) ;
     RC := 4 ;
     if GELESEN then
       RC := 0 ;
     READLINE := RC
   end (* READLINE *) ;



procedure REPORT_LINE ( LINENO : INTEGER ; PATTERN : CHAR3 ; var BUF :
                      ZEILE ; LEN : INTEGER ; LIMITS : BOOLEAN ) ;

   var I : INTEGER ;

   begin (* REPORT_LINE *)
     WRITE ( LINENO : 6 , ' ' , PATTERN , ' ' ) ;
     if LIMITS then
       WRITE ( '*' ) ;
     for I := 1 to LEN do
       WRITE ( BUF [ I ] ) ;
     if LIMITS then
       WRITE ( '*' ) ;
     WRITELN ;
   end (* REPORT_LINE *) ;



procedure REPORT ;

   var STATE : INTEGER ;
       LINENO : INTEGER ;
       P : LINEPTR ;
       ALAST : LINEPTR ;

       /***************************************************/
       /*   Pointer to the last unmatched lines in file a */
       /***************************************************/

       BLAST : LINEPTR ;

       /****************/
       /*   and file b */
       /****************/

       SEQ1 : INTEGER ;
       SEQ2 : INTEGER ;
       SEQ3 : INTEGER ;
       SEQ4 : INTEGER ;
       LINENOA : INTEGER ;
       LINENOB : INTEGER ;
       PA : LINEPTR ;
       PB : LINEPTR ;
       ENV : INTEGER ;
       ABWEICHUNG : BOOLEAN ;
       DUMMYRC : INTEGER ;

   const LINE1 : CHAR64 =
    '================================================================'
         ;
         LINE2 : CHAR64 =
    '----------------------------------------------------------------'
         ;

   begin (* REPORT *)
     if not OPT . EDIT then
       begin
         WRITELN ;
         WRITELN ( LINE1 ) ;

     /***************************************************/
     /*   Ausgabe, wenn von links etwas (mehr) da ist   */
     /***************************************************/

         if A . HEAD <> A . CURR then
           begin

     /***************************************************/
     /*   head ist evtl. nicht die Stelle der           */
     /*   Abweichung, sondern schon vorher, siehe       */
     /*   environ. Dann werden ein paar Zeilen mehr     */
     /*   ausgegeben, aber mit Punkten (weil dort       */
     /*   keine Abweichung ist), entsprechend auch      */
     /*   nach der Abweichung. Die echte Abweichung     */
     /*   wird mit <<< gekennzeichnet                   */
     /***************************************************/

             ABWEICHUNG := FALSE ;
             LINENO := A . HLINE ;
             P := A . HEAD ;
             while ( P <> NIL ) and ( P <> A . CURR ) do
               begin
                 if ( LINENO >= A . CLINE_SAVE - OPT . ENVIRON ) then
                   begin
                     if P = A . CURR_SAVE then
                       ABWEICHUNG := TRUE ;
                     if not ABWEICHUNG then
                       begin
                         REPORT_LINE ( LINENO , '...' , P -> . TEXT , P
                                       -> . LEN , OPT . LIMITS ) ;
                       end (* then *)
                     else
                       begin
                         REPORT_LINE ( LINENO , '<<<' , P -> . TEXT , P
                                       -> . LEN , OPT . LIMITS ) ;
                       end (* else *)
                   end (* then *) ;
                 LINENO := LINENO + 1 ;
                 P := P -> . NEXT
               end (* while *) ;

     /***************************************************/
     /*   ggf. ein paar Zeilen nach der Abweichung      */
     /*   ausgeben, mit Punkten                         */
     /***************************************************/

             ENV := 0 ;
             LINENO := A . CLINE ;
             P := A . CURR ;
             while ( ENV < OPT . ENVIRON ) and ( P <> NIL ) do
               begin
                 REPORT_LINE ( LINENO , '...' , P -> . TEXT , P -> .
                               LEN , OPT . LIMITS ) ;
                 if P -> . NEXT = NIL then
                   begin
                     DUMMYRC := READLINE ( A ) ;
                   end (* then *) ;
                 ENV := ENV + 1 ;
                 LINENO := LINENO + 1 ;
                 P := P -> . NEXT
               end (* while *) ;
           end (* then *) ;

     /***************************************************/
     /*   Trennstrich, wenn links und rechts vorh.      */
     /***************************************************/

         if ( ( A . CLINE_SAVE <> A . CLINE ) and ( B . CLINE_SAVE <> B
         . CLINE ) ) or ( OPT . ENVIRON > 0 ) then
           begin
             WRITELN ( LINE2 ) ;
           end (* then *) ;

     /***************************************************/
     /*   Ausgabe, wenn von rechts etwas (mehr) da ist  */
     /***************************************************/

         ABWEICHUNG := FALSE ;
         if B . HEAD <> B . CURR then
           begin
             LINENO := B . HLINE ;
             P := B . HEAD ;
             while ( P <> NIL ) and ( P <> B . CURR ) do
               begin
                 if ( LINENO >= B . CLINE_SAVE - OPT . ENVIRON ) then
                   begin
                     if P = B . CURR_SAVE then
                       ABWEICHUNG := TRUE ;
                     if not ABWEICHUNG then
                       begin
                         REPORT_LINE ( LINENO , '...' , P -> . TEXT , P
                                       -> . LEN , OPT . LIMITS ) ;
                       end (* then *)
                     else
                       begin
                         REPORT_LINE ( LINENO , '>>>' , P -> . TEXT , P
                                       -> . LEN , OPT . LIMITS ) ;
                       end (* else *)
                   end (* then *) ;
                 LINENO := LINENO + 1 ;
                 P := P -> . NEXT
               end (* while *) ;
             ENV := 0 ;
             LINENO := B . CLINE ;
             P := B . CURR ;
             while ( ENV < OPT . ENVIRON ) and ( P <> NIL ) do
               begin
                 REPORT_LINE ( LINENO , '...' , P -> . TEXT , P -> .
                               LEN , OPT . LIMITS ) ;
                 if P -> . NEXT = NIL then
                   begin
                     DUMMYRC := READLINE ( B ) ;
                   end (* then *) ;
                 ENV := ENV + 1 ;
                 LINENO := LINENO + 1 ;
                 P := P -> . NEXT
               end (* while *) ;
           end (* then *) ;
       end (* then *) ;
   end (* REPORT *) ;



function COMPX ( L1 : LINEPTR ; L2 : LINEPTR ) : BOOLEAN ;

   var SBUF1 : ZEILE ;
       SBUF2 : ZEILE ;
       SBUFK1 : CHAR64 ;
       SBUFK2 : CHAR64 ;
       I : INTEGER ;
       IPOS : INTEGER ;
       IREST : INTEGER ;
       ILEN1 : INTEGER ;
       ILEN2 : INTEGER ;
       CH : CHAR ;

   begin (* COMPX *)
     if ( L1 -> . LEN <= 64 ) and ( L2 -> . LEN <= 64 ) then
       begin
         SBUFK1 := ' ' ;
         SBUFK2 := ' ' ;
         I := 0 ;
         for IPOS := OPT . COLSTART to OPT . COLEND do
           begin
             if ( IPOS >= OPT . NCOLSTART ) and ( IPOS <= OPT . NCOLEND
             ) then
               continue ;
             if ( IPOS > L1 -> . LEN ) then
               break ;
             CH := L1 -> . TEXT [ IPOS ] ;
             if CH <> ' ' then
               begin
                 I := I + 1 ;
                 if OPT . ICASE then
                   SBUFK1 [ I ] := TOLOWER ( CH )
                 else
                   SBUFK1 [ I ] := CH ;
               end (* then *)
           end (* for *) ;
         ILEN1 := I ;
         I := 0 ;
         for IPOS := OPT . COLSTART to OPT . COLEND do
           begin
             if ( IPOS >= OPT . NCOLSTART ) and ( IPOS <= OPT . NCOLEND
             ) then
               continue ;
             if ( IPOS > L2 -> . LEN ) then
               break ;
             CH := L2 -> . TEXT [ IPOS ] ;
             if CH <> ' ' then
               begin
                 I := I + 1 ;
                 if OPT . ICASE then
                   SBUFK2 [ I ] := TOLOWER ( CH )
                 else
                   SBUFK2 [ I ] := CH ;
               end (* then *)
           end (* for *) ;
         ILEN2 := I ;
         if ILEN1 <> ILEN2 then
           begin
             COMPX := FALSE ;
             return
           end (* then *) ;
         COMPX := ( SBUFK1 = SBUFK2 ) ;
       end (* then *)
     else
       begin
         I := 0 ;
         for IPOS := OPT . COLSTART to OPT . COLEND do
           begin
             if ( IPOS >= OPT . NCOLSTART ) and ( IPOS <= OPT . NCOLEND
             ) then
               continue ;
             if ( IPOS > L1 -> . LEN ) then
               break ;
             CH := L1 -> . TEXT [ IPOS ] ;
             if CH <> ' ' then
               begin
                 I := I + 1 ;
                 if OPT . ICASE then
                   SBUF1 [ I ] := TOLOWER ( CH )
                 else
                   SBUF1 [ I ] := CH ;
               end (* then *)
           end (* for *) ;
         ILEN1 := I ;
         I := 0 ;
         for IPOS := OPT . COLSTART to OPT . COLEND do
           begin
             if ( IPOS >= OPT . NCOLSTART ) and ( IPOS <= OPT . NCOLEND
             ) then
               continue ;
             if ( IPOS > L2 -> . LEN ) then
               break ;
             CH := L2 -> . TEXT [ IPOS ] ;
             if CH <> ' ' then
               begin
                 I := I + 1 ;
                 if OPT . ICASE then
                   SBUF2 [ I ] := TOLOWER ( CH )
                 else
                   SBUF2 [ I ] := CH ;
               end (* then *)
           end (* for *) ;
         ILEN2 := I ;
         if ILEN1 <> ILEN2 then
           begin
             COMPX := FALSE ;
             return
           end (* then *) ;
         for IREST := ILEN1 + 1 to MAXLEN do
           SBUF1 [ IREST ] := ' ' ;
         for IREST := ILEN2 + 1 to MAXLEN do
           SBUF2 [ IREST ] := ' ' ;
         COMPX := ( SBUF1 = SBUF2 ) ;
       end (* else *)
   end (* COMPX *) ;



function COMPARE_LINE : BOOLEAN ;

(******************************************)
(*   cea/ceb sind die Endespalten der     *)
(*   Dateien A und B                      *)
(******************************************)


   var CEA , CEB : INTEGER ;
       MATCH : BOOLEAN ;
       I : INTEGER ;

   begin (* COMPARE_LINE *)
     if ( A . CURR = NIL ) or ( B . CURR = NIL ) then
       begin
         COMPARE_LINE := ( A . CURR = NIL ) and ( B . CURR = NIL ) ;
         return
       end (* then *) ;
     if OPT . WHITE then
       begin
         COMPARE_LINE := COMPX ( A . CURR , B . CURR ) ;
         return
       end (* then *) ;
     if A . CURR -> . LEN < OPT . COLEND then
       CEA := A . CURR -> . LEN
     else
       CEA := OPT . COLEND ;
     if B . CURR -> . LEN < OPT . COLEND then
       CEB := B . CURR -> . LEN
     else
       CEB := OPT . COLEND ;
     if CEA <> CEB then
       begin
         COMPARE_LINE := FALSE ;
         return
       end (* then *) ;

     /****************************************/
     /*   Die Laengen sind gleich;           */
     /*   jetzt werden die Inhalte ver-      */
     /*   glichen.                           */
     /****************************************/

     if CEA <= OPT . COLSTART then
       begin
         COMPARE_LINE := TRUE ;
         return
       end (* then *) ;
     for I := OPT . COLSTART to CEA do
       begin
         if ( I >= OPT . NCOLSTART ) and ( I <= OPT . NCOLEND ) then
           continue ;
         if OPT . ICASE then
           MATCH := ( TOLOWER ( A . CURR -> . TEXT [ I ] ) = TOLOWER (
                    B . CURR -> . TEXT [ I ] ) )
         else
           MATCH := ( A . CURR -> . TEXT [ I ] = B . CURR -> . TEXT [ I
                    ] ) ;
         if not MATCH then
           break ;
       end (* for *) ;
     COMPARE_LINE := MATCH ;
     return
   end (* COMPARE_LINE *) ;



procedure MARK ( var X : S_FANKER ) ;

(*****************************************************************)
(*   Alles, was vor der aktuellen Position liegt, ist nicht      *)
(*   mehr interessant; deshalb werden die Zeilen zwischen        *)
(*   head und curr weggeworfen (free); head wird auf curr        *)
(*   positioniert.                                               *)
(*****************************************************************)
(*   neu am 07.10.2014: ein paar Zeilen behalten fuer das        *)
(*   Anzeigen des Environments bei Abweichungen                  *)
(*****************************************************************)


   var P : LINEPTR ;

   begin (* MARK *)
     while ( X . HEAD <> X . TAIL ) and ( X . HEAD -> . NEXT <> NIL )
     and ( X . HEAD -> . LINENO < X . CLINE - OPT . ENVIRON ) do
       begin

     /***************************/
     /*   Speicher freigeben    */
     /***************************/

         P := X . HEAD -> . NEXT ;
         FREE ( X . HEAD ) ;
         X . HEAD := P ;
       end (* while *) ;
     if X . HEAD <> NIL then
       begin
         X . HLINE := X . HEAD -> . LINENO ;
       end (* then *)
   end (* MARK *) ;



function MOVECURR ( var X : S_FANKER ) : BOOLEAN ;

   var RC : INTEGER ;

   begin (* MOVECURR *)

     /********************************************************/
     /*   wenn schon eof, raus mit Fehler 8                  */
     /********************************************************/

     if ( X . CURR = NIL ) and ( X . TAIL <> NIL ) then
       begin
         MOVECURR := TRUE ;
         return
       end (* then *) ;

     /********************************************************/
     /*   wenn noch nichts gelesen, eine Zeile lesen         */
     /********************************************************/

     if X . TAIL = NIL then
       begin
         RC := READLINE ( X ) ;
         if RC <> 0 then
           begin
             X . CURR := NIL ;
             X . CLINE := 1 ;
             MOVECURR := TRUE ;
             return
           end (* then *) ;
         X . CURR := X . HEAD ;
         X . CLINE := 1 ;
         MOVECURR := FALSE ;
         return
       end (* then *) ;

     /********************************************************/
     /*   wenn kein weiterer Satz an Tail dranhaengt,        */
     /*   nochmal lesen                                      */
     /********************************************************/

     if X . CURR = X . TAIL then
       begin
         RC := READLINE ( X ) ;
       end (* then *) ;

     /********************************************************/
     /*   wenn kein weiterer Satz an Tail dranhaengt,        */
     /*   sind wir am Dateiende                              */
     /********************************************************/

     if X . CURR = X . TAIL then
       begin
         X . CURR := NIL ;
         X . CLINE := X . CLINE + 1 ;
         MOVECURR := TRUE ;
         return
       end (* then *) ;

     /********************************************************/
     /*   weitersetzen auf naechsten Satz                    */
     /********************************************************/

     X . CURR := X . CURR -> . NEXT ;
     X . CLINE := X . CLINE + 1 ;
     MOVECURR := FALSE ;
   end (* MOVECURR *) ;



procedure BACKTRACK ( var X : S_FANKER ; var XLINES : INTEGER ) ;

   var CLINE : INTEGER ;

   begin (* BACKTRACK *)
     CLINE := X . CLINE + 1 ;
     if X . CURR_SAVE <> NIL then
       begin
         XLINES := CLINE - X . CURR_SAVE -> . LINENO ;
         X . CURR := X . CURR_SAVE ;
         X . CLINE := X . CURR_SAVE -> . LINENO ;
       end (* then *)
     else
       begin
         XLINES := 0 ;
         X . CURR := NIL ;
       end (* else *)
   end (* BACKTRACK *) ;



function CHECKFULLMATCH ( var X : S_FANKER ; var Y : S_FANKER ) :
                        BOOLEAN ;

   var MATCH : BOOLEAN ;
       N : INTEGER ;
       SAVEXCUR , SAVEYCUR : LINEPTR ;
       SAVEXLINE , SAVEYLINE : INTEGER ;
       DUMMYX , DUMMYY : BOOLEAN ;

   begin (* CHECKFULLMATCH *)
     SAVEXCUR := X . CURR ;
     SAVEYCUR := Y . CURR ;
     SAVEXLINE := X . CLINE ;
     SAVEYLINE := Y . CLINE ;
     MATCH := COMPARE_LINE ;
     N := OPT . MINMATCH - 1 ;
     while MATCH and ( N <> 0 ) do
       begin
         DUMMYX := MOVECURR ( X ) ;
         DUMMYY := MOVECURR ( Y ) ;
         MATCH := COMPARE_LINE ;
         N := N - 1 ;
       end (* while *) ;
     X . CURR := SAVEXCUR ;
     X . CLINE := SAVEXLINE ;
     Y . CURR := SAVEYCUR ;
     Y . CLINE := SAVEYLINE ;
     CHECKFULLMATCH := MATCH ;
   end (* CHECKFULLMATCH *) ;



function SEARCH ( var X : S_FANKER ; var Y : S_FANKER ) : BOOLEAN ;

   var COUNT : INTEGER ;
       MATCH : BOOLEAN ;
       EOFY : BOOLEAN ;
       EOFX : BOOLEAN ;

   begin (* SEARCH *)
     EOFY := MOVECURR ( Y ) ;
     EOFX := ( X . CURR_SAVE = NIL ) ;
     if EOFX and EOFY then
       begin
         SEARCH := TRUE ;
         return ;
       end (* then *) ;
     BACKTRACK ( X , COUNT ) ;
     MATCH := CHECKFULLMATCH ( X , Y ) ;
     COUNT := COUNT - 1 ;
     while ( COUNT > 0 ) and not MATCH do
       begin
         EOFX := MOVECURR ( X ) ;
         COUNT := COUNT - 1 ;
         MATCH := CHECKFULLMATCH ( X , Y ) ;
       end (* while *) ;
     SEARCH := MATCH ;
   end (* SEARCH *) ;



procedure SUCHE_GLEICH ;

   var ADVANCEB : BOOLEAN ;
       MATCH : BOOLEAN ;

   begin (* SUCHE_GLEICH *)
     ADVANCEB := FALSE ;
     A . CURR_SAVE := A . CURR ;
     B . CURR_SAVE := B . CURR ;
     A . CLINE_SAVE := A . CLINE ;
     B . CLINE_SAVE := B . CLINE ;
     while TRUE do
       begin
         ADVANCEB := not ADVANCEB ;
         if ADVANCEB then
           begin
             MATCH := SEARCH ( A , B ) ;
           end (* then *)
         else
           begin
             MATCH := SEARCH ( B , A ) ;
           end (* else *) ;
         if MATCH then
           break ;
       end (* while *) ;
     REPORT ;
   end (* SUCHE_GLEICH *) ;



procedure SUCHE_UNGLEICH ;

   var MATCH : BOOLEAN ;
       EOFA : BOOLEAN ;
       EOFB : BOOLEAN ;

   begin (* SUCHE_UNGLEICH *)
     A . CURR_SAVE := NIL ;
     B . CURR_SAVE := NIL ;
     A . CLINE_SAVE := - 1 ;
     B . CLINE_SAVE := - 1 ;
     while TRUE do
       begin
         EOFA := MOVECURR ( A ) ;
         EOFB := MOVECURR ( B ) ;
         if EOFA and EOFB then
           break ;
         MARK ( A ) ;
         MARK ( B ) ;
         MATCH := COMPARE_LINE ;

     /**********************************/
     /*   Sequence Number merken       */
     /**********************************/

         if MATCH then
           SEQ . LASTMSEQ := A . CURR -> . SEQNUM ;
         if not MATCH then
           break ;
       end (* while *)
   end (* SUCHE_UNGLEICH *) ;



function COMPARE_FILES : BOOLEAN ;

   var MATCH : BOOLEAN ;
       DIFFERENT : BOOLEAN ;

   begin (* COMPARE_FILES *)
     MATCH := TRUE ;
     DIFFERENT := FALSE ;
     while TRUE do
       begin
         if MATCH then
           begin
             SUCHE_UNGLEICH ;
             MATCH := FALSE ;
           end (* then *)
         else
           begin
             DIFFERENT := TRUE ;
             SUCHE_GLEICH ;
             MATCH := TRUE ;
           end (* else *) ;
         if ( A . CURR = NIL ) and ( B . CURR = NIL ) then
           break ;
       end (* while *) ;
     COMPARE_FILES := DIFFERENT ;
   end (* COMPARE_FILES *) ;



procedure ERROR ( ERRNO : INTEGER ) ;

   begin (* ERROR *)
     case ERRNO of
       1 : WRITELN ( '+++ Falsches Argument (beginnt nicht mit Minus)'
                     ) ;
       2 : WRITELN ( '+++ Unbekannte Option' ) ;
       3 : WRITELN ( '+++ Falsches Zeichen in Option' ) ;
       4 : WRITELN ( '+++ Nur eine Suboption (Zahl) erlaubt' ) ;
       5 : WRITELN ( '+++ Zwei mit Komma getrennte Zahlen erforderlich'
                     ) ;
     end (* case *) ;
     PARM_FEHLER := TRUE ;
   end (* ERROR *) ;



procedure CHKPARM ( LENGTH : INTEGER ; STRING : OSPARM_TYPE ; var OPT :
                  S_OPTIONS ; var SEQ : S_SEQINFO ) ;

(********************************************************)
(*   Parameter abarbeiten                               *)
(*   fuer einzelne Parameter Prozedur CHKPARM2          *)
(*   aufrufen                                           *)
(********************************************************)


   var I : INTEGER ;
       INPARM : BOOLEAN ;
       CH : CHAR ;
       PARM : OSPARM_TYPE ;
       X : INTEGER ;


   procedure CHKPARM2 ( PARM : OSPARM_TYPE ; var OPT : S_OPTIONS ; var
                      SEQ : S_SEQINFO ) ;


      procedure READPARM ( PARM : OSPARM_TYPE ; ANZAHL : INTEGER ; var
                         ERG1 : INTEGER ; var ERG2 : INTEGER ) ;

         var CP : -> CHAR ;
             CH : CHAR ;
             WERT1 : INTEGER ;
             WERT2 : INTEGER ;
             WERTFELD : INTEGER ;

         begin (* READPARM *)
           WERT1 := 0 ;
           WERT2 := 0 ;
           CP := ADDR ( PARM [ 2 ] ) ;
           WERTFELD := 1 ;
           while CP -> <> ' ' do
             begin
               CH := CP -> ;
               case CH of
                 '0' .. '9' :
                   begin
                     if WERTFELD = 1 then
                       WERT1 := WERT1 * 10 + ORD ( CH ) - ORD ( '0' )
                     else
                       WERT2 := WERT2 * 10 + ORD ( CH ) - ORD ( '0' )
                   end (* tag/ca *) ;
                 ',' : if ANZAHL = 1 then
                         ERROR ( 4 )
                       else
                         WERTFELD := 2 ;
                 otherwise
                   ERROR ( 3 )
               end (* case *) ;
               CP := PTRADD ( CP , 1 ) ;
             end (* while *) ;
           if ANZAHL = 2 then
             if WERTFELD = 1 then
               ERROR ( 5 ) ;
           ERG1 := WERT1 ;
           if ANZAHL = 2 then
             ERG2 := WERT2 ;
         end (* READPARM *) ;


      begin (* CHKPARM2 *)
        case PARM [ 1 ] of
          'T' : OPT . ICASE := TRUE ;
          'A' : OPT . ALLI_TESTFALL := TRUE ;
          'V' : OPT . TRAIL := TRUE ;
          'W' : OPT . WHITE := TRUE ;
          'B' : OPT . BINARY := FALSE ;
          '*' : OPT . LIMITS := TRUE ;
          'C' : begin
                  READPARM ( PARM , 2 , OPT . COLSTART , OPT . COLEND )
                             ;
                end (* tag/ca *) ;
          'D' : begin
                  READPARM ( PARM , 2 , OPT . NCOLSTART , OPT . NCOLEND
                             ) ;
                end (* tag/ca *) ;
          'E' : begin
                  READPARM ( PARM , 1 , OPT . ENVIRON , OPT . ENVIRON )
                             ;
                end (* tag/ca *) ;
          'M' : begin
                  READPARM ( PARM , 1 , OPT . MINMATCH , OPT . MINMATCH
                             ) ;
                  WRITELN ( 'eingelesen: minmatch : ' , OPT . MINMATCH
                            ) ;
                end (* tag/ca *) ;
          'L' : begin

        /******************/
        /* ausgabedatei ? */
        /******************/

                  
                end (* tag/ca *) ;
          'U' : begin

        /*****************/
        /* lstfilename ? */
        /*****************/

                  OPT . EDIT := TRUE ;
                end (* tag/ca *) ;
          'X' : begin
                  TRACE := TRUE ;
                end (* tag/ca *) ;
          'S' : begin

        /***********/
        /* userseq */
        /***********/

                  
                end (* tag/ca *) ;
          'Y' : begin

        /***********/
        /* fileseq */
        /***********/

                  
                end (* tag/ca *) ;
          otherwise
            ERROR ( 2 )
        end (* case *) ;
      end (* CHKPARM2 *) ;


   begin (* CHKPARM *)
     I := 1 ;
     INPARM := FALSE ;
     while I <= LENGTH do
       begin
         CH := STRING [ I ] ;
         if INPARM then
           begin
             if CH = ' ' then
               begin
                 INPARM := FALSE ;
                 CHKPARM2 ( PARM , OPT , SEQ ) ;
               end (* then *)
             else
               begin
                 X := X + 1 ;
                 PARM [ X ] := CH ;
               end (* else *) ;
             I := I + 1 ;
           end (* then *)
         else
           begin
             if CH = ' ' then
               begin
                 I := I + 1 ;
                 continue
               end (* then *) ;
             if CH <> '-' then
               begin
                 ERROR ( 1 ) ;
                 EXIT ( 8 ) ;
               end (* then *) ;
             INPARM := TRUE ;
             X := 0 ;
             PARM := '  ' ;
             I := I + 1 ;
           end (* else *)
       end (* while *) ;
     if INPARM then
       CHKPARM2 ( PARM , OPT , SEQ ) ;
   end (* CHKPARM *) ;



begin (* HAUPTPROGRAMM *)
  TRACE := FALSE ;
  PARM_FEHLER := FALSE ;
  FCB1 := NIL ;
  FCB2 := NIL ;
  WRITELN ;
  WRITELN ( VERSION ) ;
  WRITELN ( 'Vergleich Datei 1 <<< ' , 'mit Datei 2 >>>' ) ;

  /*****************************************/
  /* moegliche Erweiterung:                */
  /* Standardfunktion zur typgerechten     */
  /* Initialisierung einer Struktur        */
  /*****************************************/
  "INIT ( SEQ ) ;                           "

  SEQ . SEQTYPE := 0 ;
  SEQ . SEQSTRT := 1 ;
  SEQ . SEQINCR := 1 ;
  SEQ . SEQCUR := 0 ;
  SEQ . LASTMSEQ := 0 ;

  "**************"
  "INIT ( OPT ) ;"
  "**************"

  OPT . EDIT := FALSE ;
  OPT . WHITE := FALSE ;
  OPT . TRAIL := FALSE ;
  OPT . BINARY := FALSE ;
  OPT . ICASE := FALSE ;
  OPT . LIMITS := FALSE ;
  OPT . COLSTART := 1 ;
  OPT . COLEND := MAXLEN ;
  OPT . NCOLSTART := - 1 ;
  OPT . NCOLEND := - 1 ;
  OPT . MINMATCH := 2 ;
  OPT . ALLI_TESTFALL := FALSE ;
  OPT . ENVIRON := 0 ;

  /********************************************************/
  /*   Testen bzw. Erweiterung                            */
  /*   Command Line Options abarbeiten                    */
  /*   siehe Coding aus XCOMP.C                           */
  /********************************************************/
  /*   Achtung bei MVS:                                   */
  /*   der OSPARM ist tatsaechlich physisch nur so        */
  /*   lang wie LENGTH angibt, also muss er in ein        */
  /*   Hilfsfeld kopiert werden (zeichenweise);           */
  /*   weiterarbeiten dann mit dem Hilfsfeld              */
  /********************************************************/

  if OSPARM <> NIL then
    begin
      with OSPARM -> do
        begin
          WRITELN ( 'Eingelesene Parameter: ' ) ;
          WRITELN ( 'parm: length = ' , LENGTH : 3 ) ;
          PSTRING := ' ' ;
          WRITE ( 'parm: <' ) ;
          for X := 1 to LENGTH do
            begin
              WRITE ( STRING [ X ] ) ;
              PSTRING [ X ] := STRING [ X ] ;
            end (* for *) ;
          WRITELN ( '>' ) ;
          CHKPARM ( LENGTH , PSTRING , OPT , SEQ ) ;
        end (* with *) ;
    end (* then *)
  else
    WRITELN ( 'parm: osparm is nil' ) ;
  if PARM_FEHLER then
    EXIT ( 8 ) ;

  /********************************************************/
  /*   hier geht der eigentliche Vergleich los            */
  /********************************************************/

  INITFANKER ( A , 1 ) ;
  INITFANKER ( B , 2 ) ;
  DIFFERENT := COMPARE_FILES ;
  if not DIFFERENT then
    begin
      WRITELN ( 'Datei 1 und Datei 2 sind identisch.' ) ;
    end (* then *) ;
  CHKHEAP ( 0 ) ;
end (* HAUPTPROGRAMM *) .
