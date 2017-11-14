program PDOKKHP ( INPFILE , OUTPUT , DRUCKER ) ;

(**************************************************)
(*                                                *)
(*   PDOKK - Print DOKumente Konzept              *)
(*                                                *)
(*   Programm zum Ausgeben von Texten             *)
(*   mit eigenen Drucksteuersequenzen             *)
(*                                                *)
(*   Bernd Oppolzer - Oppolzer Informatik         *)
(*                                                *)
(**************************************************)
(*                                                *)
(*   Uraltes BASIC-Programm, vermutlich 1980      *)
(*   irgendwann auf HP-Laserjet angepasst         *)
(*   2017 auf Stanford Pascal umgeschrieben       *)
(*                                                *)
(**************************************************)



const MAXLZEILE = 250 ;
      S_ESC = X'1b' ;
      S_LF = X'0a' ;
      S_CR = X'0d' ;
      S_VS_0 = X'0d' ;
      S_VS_1 = X'1b' '&l12D' X'0d0a' ;
      S_VS_2 = X'1b' '&l6D' X'0d0a' ;
      S_VS_3 = X'1b' '&l4D' X'0d0a' ;
      S_VS_4 = X'1b' '&l3D' X'0d0a' ;
      S_SCHATTEIN = X'1b' '(s3B' ;
      S_SCHATTAUS = X'1b' '(s0B' ;
      S_CPI10 = X'1b' '(s10H' ;
      S_CPI12 = X'1b' '(s13H' ;
      S_TRANSP = X'1b' '&p1X' ;
      S_C24 = X'1b' '&p1X' X'18' ;
      S_C25 = X'1b' '&p1X' X'19' ;
      S_CPI10VS2 = X'1b' '&l6D' X'0d0a' ;
      S_CPI12VS2 = X'1b' '&l6C' X'0d0a' ;
      CPI10ER = 10 ;
      CPI12ER = 13 ;


type BYTE = record
              CONT : CHAR ;
            end ;
     ZEILREC = array [ 1 .. MAXLZEILE ] of CHAR ;
     CHAR16 = array [ 1 .. 16 ] of CHAR ;
     STRING16 = record
                  LEN : INTEGER ;
                  DATA : CHAR16 ;
                end ;
     STATUSREC = record
                   LEER : INTEGER ;
                   INTAB : BOOLEAN ;
                   ER : INTEGER ;
                   NDRUCK : BOOLEAN ;
                   STARTSEITE : INTEGER ;
                   ENDESEITE : INTEGER ;
                   ZZGRENZE : INTEGER ;
                   ZZ : INTEGER ;
                   SCHATT : BOOLEAN ;
                   UNTERSTR : BOOLEAN ;
                   CPI : INTEGER ;
                   LINEF : INTEGER ;
                   HEADING : ZEILREC ;
                   SEITEXT : ZEILREC ;
                   L_SEITEXT : INTEGER ;
                   SEITE : INTEGER ;
                   BALKENDR : BOOLEAN ;
                   VS : array [ 0 .. 4 ] of STRING16 ;
                 end ;


var INPFILE : TEXT ;
    DRUCKER : FILE of BYTE ;
    ZEILE : ZEILREC ;
    STATUS : STATUSREC ;
    ZLEN : INTEGER ;
    WEITER : BOOLEAN ;

    /*****************************************/
    /*   evtl. startseite und endeseite      */
    /*   ueber parameter einlesen            */
    /*****************************************/




function ISTELLEN ( W : INTEGER ) : INTEGER ;

   var STELLEN : INTEGER ;

   begin (* ISTELLEN *)
     STELLEN := 0 ;
     if W < 0 then
       begin
         W := - W ;
         STELLEN := 1
       end (* then *) ;
     if W > 999999999 then
       STELLEN := STELLEN + 10
     else
       if W > 99999999 then
         STELLEN := STELLEN + 9
       else
         if W > 9999999 then
           STELLEN := STELLEN + 8
         else
           if W > 999999 then
             STELLEN := STELLEN + 7
           else
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



function READLINE ( var F : TEXT ; var Z : ZEILREC ) : INTEGER ;

   var X : INTEGER ;

   begin (* READLINE *)
     if EOF ( F ) then
       begin
         Z := ' ' ;
         READLINE := - 1 ;
         return ;
       end (* then *) ;
     READLN ( F , Z ) ;
     X := MAXLZEILE ;
     while X > 0 do
       begin
         if Z [ X ] <> ' ' then
           break ;
         X := X - 1 ;
       end (* while *) ;
     READLINE := X ;
   end (* READLINE *) ;



procedure SHIFT ( var Z : ZEILREC ; COUNT : INTEGER ) ;

(***************************************************)
(*   schiebt Inhalt in Z nach links                *)
(*   um "count" stellen                            *)
(***************************************************)


   var I : INTEGER ;
       LPOS : INTEGER ;

   begin (* SHIFT *)
     LPOS := SIZEOF ( ZEILREC ) - COUNT ;
     for I := 1 to LPOS do
       Z [ I ] := Z [ I + COUNT ] ;
     for I := LPOS + 1 to SIZEOF ( ZEILREC ) do
       Z [ I ] := ' ' ;
   end (* SHIFT *) ;



function NEXT_CH ( var Z : ZEILREC ; POS : INTEGER ) : INTEGER ;

   var I : INTEGER ;
       CPOS : INTEGER ;

   begin (* NEXT_CH *)
     CPOS := - 1 ;
     for I := POS to SIZEOF ( ZEILREC ) do
       if Z [ I ] <> ' ' then
         begin
           CPOS := I ;
           break
         end (* then *) ;
     NEXT_CH := CPOS ;
   end (* NEXT_CH *) ;



function NEXT_BLANK ( var Z : ZEILREC ; POS : INTEGER ) : INTEGER ;

   var I : INTEGER ;
       CPOS : INTEGER ;

   begin (* NEXT_BLANK *)
     CPOS := - 1 ;
     for I := POS to SIZEOF ( ZEILREC ) do
       if Z [ I ] = ' ' then
         begin
           CPOS := I ;
           break
         end (* then *) ;
     NEXT_BLANK := CPOS ;
   end (* NEXT_BLANK *) ;



function S_LEN ( var Z : ZEILREC ) : INTEGER ;

   var L : INTEGER ;

   begin (* S_LEN *)
     L := SIZEOF ( ZEILREC ) ;
     while L > 0 do
       begin
         if Z [ L ] <> ' ' then
           break ;
         L := L - 1 ;
       end (* while *) ;
     S_LEN := L ;
   end (* S_LEN *) ;



function VAL ( var Z : ZEILREC ) : INTEGER ;

   var I : INTEGER ;
       LVAL : INTEGER ;
       MINUS : BOOLEAN ;

   begin (* VAL *)
     LVAL := 0 ;
     MINUS := FALSE ;
     for I := 1 to SIZEOF ( ZEILREC ) do
       case Z [ I ] of
         '0' .. '9' :
           LVAL := LVAL * 10 + ORD ( Z [ I ] ) - ORD ( '0' ) ;
         '+' : if I <> 1 then
                 break ;
         '-' : if I <> 1 then
                 break
               else
                 MINUS := TRUE ;
         otherwise
           break ;
       end (* case *) ;
     if MINUS then
       LVAL := - LVAL ;
     VAL := LVAL ;
   end (* VAL *) ;



function INSTR ( var Z : ZEILREC ; L1 : INTEGER ; SUCH : CHAR16 ; L2 :
               INTEGER ) : INTEGER ;

   var I : INTEGER ;
       I2 : INTEGER ;

   begin (* INSTR *)
     for I := 1 to L1 do
       for I2 := 1 to L2 do
         if Z [ I ] = SUCH [ I2 ] then
           begin
             INSTR := I ;
             return
           end (* then *) ;
     INSTR := 0 ;
   end (* INSTR *) ;



procedure AUSGABE_ZEILREC ( X : ZEILREC ; L : INTEGER ) ;

   var I : INTEGER ;
       B : BYTE ;

   begin (* AUSGABE_ZEILREC *)
     for I := 1 to L do
       begin
         B . CONT := X [ I ] ;
         WRITE ( DRUCKER , B ) ;
       end (* for *) ;
   end (* AUSGABE_ZEILREC *) ;



procedure AUSGABE_CHAR16 ( X : CHAR16 ; L : INTEGER ) ;

   var I : INTEGER ;
       B : BYTE ;

   begin (* AUSGABE_CHAR16 *)
     for I := 1 to L do
       begin
         B . CONT := X [ I ] ;
         WRITE ( DRUCKER , B ) ;
       end (* for *) ;
   end (* AUSGABE_CHAR16 *) ;



procedure AUSGABE_CHAR ( X : CHAR ; ANZ : INTEGER ) ;

   var I : INTEGER ;
       B : BYTE ;

   begin (* AUSGABE_CHAR *)
     for I := 1 to ANZ do
       begin
         B . CONT := X ;
         WRITE ( DRUCKER , B ) ;
       end (* for *) ;
   end (* AUSGABE_CHAR *) ;



procedure AUSGABE_INT ( ZAHL : INTEGER ; STELLEN : INTEGER ) ;

   var REST , QUOT , X , SCALE : INTEGER ;
       B : BYTE ;
       C : CHAR ;

   begin (* AUSGABE_INT *)
     SCALE := 1 ;
     for X := 2 to STELLEN do
       SCALE := SCALE * 10 ;
     REST := ZAHL ;
     for X := STELLEN DOWNTO 1 do
       begin
         QUOT := REST DIV SCALE ;
         if ZAHL < SCALE then
           C := ' '
         else
           C := CHR ( ORD ( '0' ) + QUOT ) ;
         B . CONT := C ;
         WRITE ( DRUCKER , B ) ;
         REST := REST MOD SCALE ;
         SCALE := SCALE DIV 10 ;
       end (* for *) ;
   end (* AUSGABE_INT *) ;



procedure VORSCHUB ( V : INTEGER ; DRUCKE0 : BOOLEAN ) ;

   var VORSCHUB : CHAR16 ;
       L_VORSCHUB : INTEGER ;

   begin (* VORSCHUB *)
     L_VORSCHUB := STATUS . VS [ V ] . LEN ;
     VORSCHUB := STATUS . VS [ V ] . DATA ;
     if ( V > 0 ) or DRUCKE0 then
       AUSGABE_CHAR16 ( VORSCHUB , L_VORSCHUB ) ;
   end (* VORSCHUB *) ;



procedure UEBERSCHRIFT ;

(**************************************************)
(*                                                *)
(*   AUSDRUCKEN DER UEBERSCHRIFTSZEILEN           *)
(*                                                *)
(**************************************************)


   var TEST : INTEGER ;
       PLATZ_HEADER : INTEGER ;

   begin (* UEBERSCHRIFT *)
     if STATUS . SEITE > STATUS . ENDESEITE then
       return ;
     if STATUS . NDRUCK then
       if STATUS . SEITE >= STATUS . STARTSEITE then
         STATUS . NDRUCK := FALSE ;
     if STATUS . ZZ <> 1000 then
       begin
         AUSGABE_CHAR ( CHR ( 12 ) , 1 ) ;
       end (* then *) ;
     STATUS . ZZ := STATUS . LEER ;
     while STATUS . ZZ >= 4 do
       begin
         VORSCHUB ( 4 , FALSE ) ;
         STATUS . ZZ := STATUS . ZZ - 4
       end (* while *) ;
     VORSCHUB ( STATUS . ZZ , FALSE ) ;
     STATUS . ZZ := 0 ;

     /**********************/
     /* UEBERS$=SPACE$(60) */
     /**********************/

     if STATUS . SEITE = 0 then
       return ;
     TEST := STATUS . SEITE MOD 2 ;
     if STATUS . NDRUCK then
       begin
         WRITELN ( 'Seite ' , STATUS . SEITE , ' wird ueberlesen' ) ;
       end (* then *)
     else
       begin
         WRITELN ( 'Seite ' , STATUS . SEITE , ' wird gerade gedruckt'
                   ) ;
       end (* else *) ;

     /*************************************************/
     /*  IF TEST%=0 THEN 4100                         */
     /*  AENDERN, FALLS SEITENZAHLEN WECHSELN SOLLEN  */
     /*************************************************/

     AUSGABE_CHAR ( ' ' , STATUS . ER ) ;
     if STATUS . L_SEITEXT > 0 then
       begin
         PLATZ_HEADER := 60 - STATUS . L_SEITEXT - 1 - ISTELLEN (
                         STATUS . SEITE ) ;
         AUSGABE_ZEILREC ( STATUS . HEADING , PLATZ_HEADER ) ;
         AUSGABE_ZEILREC ( STATUS . SEITEXT , STATUS . L_SEITEXT ) ;
         AUSGABE_INT ( STATUS . SEITE , ISTELLEN ( STATUS . SEITE ) + 1
                       ) ;
       end (* then *)
     else
       begin
         PLATZ_HEADER := 60 ;
         AUSGABE_ZEILREC ( STATUS . HEADING , PLATZ_HEADER ) ;
       end (* else *) ;
     AUSGABE_CHAR ( CHR ( 13 ) , 1 ) ;
     if STATUS . BALKENDR then
       begin
         AUSGABE_CHAR ( ' ' , STATUS . ER ) ;
         AUSGABE_CHAR ( '_' , 60 ) ;
       end (* then *) ;
     STATUS . ZZ := STATUS . ZZ + 8 ;
     VORSCHUB ( 4 , FALSE ) ;
     VORSCHUB ( 4 , FALSE ) ;
     if STATUS . SEITE <> 0 then
       STATUS . SEITE := STATUS . SEITE + 1
   end (* UEBERSCHRIFT *) ;



function DO_CONTROL ( var Z : ZEILREC ; var ZLEN : INTEGER ) : BOOLEAN
                    ;

   var X : INTEGER ;
       CPINEU : INTEGER ;

   begin (* DO_CONTROL *)
     DO_CONTROL := TRUE ;

     /*****************************************/
     /* steuerbefehl beginnt mit Punkt        */
     /* wenn Punkt in Spalte 1, dann normale  */
     /* zeile; spalte 2 wird neue spalte 1    */
     /* und zurueck                           */
     /*****************************************/

     if Z [ 2 ] = '.' then
       begin
         SHIFT ( Z , 1 ) ;
         ZLEN := ZLEN - 1 ;
         return ;
       end (* then *) ;

     /*****************************************/
     /* steuerbefehle interpretieren;         */
     /* wenn unbekannter steuerbefehl,        */
     /* dann normale zeile                    */
     /*****************************************/
     /* P = neue Seite                        */
     /* L = Anzahl Line Feed Setzen           */
     /* Z = Zeilen pro Seite                  */
     /* E = Einrueckzeichen                   */
     /* X = Leerschritte am Blattanfang       */
     /* T = Indikator "in Tabelle"            */
     /* H = Header definieren                 */
     /* C = char pro inch setzen              */
     /* B = Indikator "Balkendruck"           */
     /* S = Seitenzahl usw. fuer Ueberschr.   */
     /*****************************************/

     case Z [ 2 ] of
       'P' : begin
               if STATUS . SEITE > STATUS . ENDESEITE then
                 begin
                   DO_CONTROL := FALSE ;
                   return ;
                 end (* then *) ;
               UEBERSCHRIFT ;
               Z := ' ' ;
             end (* tag/ca *) ;
       'L' : begin
               X := NEXT_CH ( Z , 3 ) ;
               SHIFT ( Z , X - 1 ) ;
               STATUS . LINEF := VAL ( Z ) ;
               Z := ' ' ;
             end (* tag/ca *) ;
       'Z' : begin
               X := NEXT_CH ( Z , 3 ) ;
               SHIFT ( Z , X - 1 ) ;
               STATUS . ZZGRENZE := VAL ( Z ) ;
               Z := ' ' ;
             end (* tag/ca *) ;
       'E' : begin
               X := NEXT_CH ( Z , 3 ) ;
               SHIFT ( Z , X - 1 ) ;
               STATUS . ER := VAL ( Z ) ;
               Z := ' ' ;
             end (* tag/ca *) ;
       'X' : begin
               X := NEXT_CH ( Z , 3 ) ;
               SHIFT ( Z , X - 1 ) ;
               STATUS . LEER := VAL ( Z ) ;
               Z := ' ' ;
             end (* tag/ca *) ;
       'T' : begin
               X := NEXT_CH ( Z , 3 ) ;
               SHIFT ( Z , X - 1 ) ;
               STATUS . INTAB := ( VAL ( Z ) <> 0 ) ;
               Z := ' ' ;
             end (* tag/ca *) ;
       'H' : begin
               X := NEXT_CH ( Z , 3 ) ;
               SHIFT ( Z , X - 1 ) ;
               STATUS . HEADING := Z ;
               Z := ' ' ;
             end (* tag/ca *) ;
       'C' : begin
               X := NEXT_CH ( Z , 3 ) ;
               SHIFT ( Z , X - 1 ) ;
               CPINEU := VAL ( Z ) ;
               if STATUS . CPI <> CPINEU then
                 begin
                   STATUS . CPI := CPINEU ;

     /**************************************************/
     /*                                                */
     /*   SPEZIALITAET FUER HP-LASER                   */
     /*                                                */
     /*   CPI12-SCHRIFT IST KLEINER ALS CPI10          */
     /*   DAHER ANDERER ZEILENVORSCHUB                 */
     /*                                                */
     /*   ESC & l 6 D  - 6 ZEILEN PRO ZOLL             */
     /*                                                */
     /*   ESC & l 6 C  - 6/48 ZOLL VORSCHUB            */
     /*                                                */
     /**************************************************/

                   case STATUS . CPI of
                     10 : begin
                            AUSGABE_CHAR16 ( S_CPI10 , SIZEOF ( S_CPI10
                                             ) ) ;
                            STATUS . VS [ 2 ] . DATA := S_CPI10VS2 ;
                            STATUS . VS [ 2 ] . LEN := SIZEOF (
                                                   S_CPI10VS2 ) ;
                            STATUS . ER := CPI10ER ;
                          end (* tag/ca *) ;
                     12 : begin
                            AUSGABE_CHAR16 ( S_CPI12 , SIZEOF ( S_CPI12
                                             ) ) ;
                            STATUS . VS [ 2 ] . DATA := S_CPI12VS2 ;
                            STATUS . VS [ 2 ] . LEN := SIZEOF (
                                                   S_CPI12VS2 ) ;
                            STATUS . ER := CPI12ER ;
                          end (* tag/ca *) ;
                     otherwise
                       
                   end (* case *)
                 end (* then *) ;
               Z := ' ' ;
             end (* tag/ca *) ;
       'B' : begin
               X := NEXT_CH ( Z , 3 ) ;
               SHIFT ( Z , X - 1 ) ;
               STATUS . BALKENDR := ( VAL ( Z ) <> 0 ) ;
               Z := ' ' ;
             end (* tag/ca *) ;
       'S' : begin
               X := NEXT_CH ( Z , 3 ) ;
               SHIFT ( Z , X - 1 ) ;
               ZLEN := ZLEN - ( X - 1 ) ;
               STATUS . SEITE := VAL ( Z ) ;
               X := NEXT_BLANK ( Z , 1 ) ;
               STATUS . SEITEXT := ' ' ;
               STATUS . L_SEITEXT := 0 ;
               if X > 0 then
                 begin
                   X := NEXT_CH ( Z , X ) ;
                   if X > 0 then
                     begin
                       SHIFT ( Z , X - 1 ) ;
                       ZLEN := ZLEN - ( X - 1 ) ;
                       STATUS . SEITEXT := Z ;
                       STATUS . L_SEITEXT := ZLEN ;
                     end (* then *)
                 end (* then *) ;
               Z := ' ' ;
             end (* tag/ca *) ;
       otherwise
         return
     end (* case *) ;
   end (* DO_CONTROL *) ;



procedure WORK_ZEILE ( var Z : ZEILREC ; ZLEN : INTEGER ) ;

   var UNTERZ : ZEILREC ;
       UNTER_EX : BOOLEAN ;
       I : INTEGER ;
       IU : INTEGER ;

   begin (* WORK_ZEILE *)
     AUSGABE_CHAR ( ' ' , STATUS . ER ) ;
     IU := 0 ;
     UNTERZ := ' ' ;
     UNTER_EX := FALSE ;
     for I := 1 to ZLEN do
       begin
         case Z [ I ] of
           '#' : begin
                   STATUS . UNTERSTR := not STATUS . UNTERSTR ;
                 end (* tag/ca *) ;
           '@' : begin
                   STATUS . SCHATT := not STATUS . SCHATT ;
                   if STATUS . SCHATT then
                     AUSGABE_CHAR16 ( S_SCHATTEIN , SIZEOF (
                                      S_SCHATTEIN ) )
                   else
                     AUSGABE_CHAR16 ( S_SCHATTAUS , SIZEOF (
                                      S_SCHATTAUS ) )
                 end (* tag/ca *) ;
           X'18' : begin
                     AUSGABE_CHAR16 ( S_C24 , SIZEOF ( S_C24 ) )
                   end (* tag/ca *) ;
           X'19' : begin
                     AUSGABE_CHAR16 ( S_C25 , SIZEOF ( S_C25 ) )
                   end (* tag/ca *) ;
           otherwise
             begin
               AUSGABE_CHAR ( Z [ I ] , 1 ) ;
               IU := IU + 1 ;
               if STATUS . UNTERSTR then
                 begin
                   UNTERZ [ IU ] := '_' ;
                   UNTER_EX := TRUE
                 end (* then *)
             end (* otherw *) ;
         end (* case *) ;
       end (* for *) ;
     if UNTER_EX then
       begin
         AUSGABE_CHAR ( CHR ( 13 ) , 1 ) ;
         AUSGABE_CHAR ( ' ' , STATUS . ER ) ;
         AUSGABE_ZEILREC ( UNTERZ , IU ) ;
       end (* then *) ;
   end (* WORK_ZEILE *) ;



begin (* HAUPTPROGRAMM *)
  RESET ( INPFILE ) ;
  REWRITE ( DRUCKER ) ;
  with STATUS do
    begin
      VS [ 0 ] . DATA := S_VS_0 ;
      VS [ 0 ] . LEN := SIZEOF ( S_VS_0 ) ;
      VS [ 1 ] . DATA := S_VS_1 ;
      VS [ 1 ] . LEN := SIZEOF ( S_VS_1 ) ;
      VS [ 2 ] . DATA := S_VS_2 ;
      VS [ 2 ] . LEN := SIZEOF ( S_VS_2 ) ;
      VS [ 3 ] . DATA := S_VS_3 ;
      VS [ 3 ] . LEN := SIZEOF ( S_VS_3 ) ;
      VS [ 4 ] . DATA := S_VS_4 ;
      VS [ 4 ] . LEN := SIZEOF ( S_VS_4 ) ;

  /*********************************************/
  /* leerschritte am blattanfang               */
  /*********************************************/

      LEER := 3 ;

  /*********************************************/
  /* zunaechst ausserhalb Tabelle              */
  /*********************************************/

      INTAB := FALSE ;

  /*********************************************/
  /* 10 ZEICHEN EINRUECKEN                     */
  /*********************************************/

      ER := 10 ;

  /*********************************************/
  /* ZUNAECHST DRUCKEN                         */
  /*********************************************/

      NDRUCK := FALSE ;
      STARTSEITE := 1 ;
      ENDESEITE := 9999 ;

  /*********************************************/
  /* ZEILENVORSCHUBGRENZE = 120 HALBZEILEN     */
  /*********************************************/

      ZZGRENZE := 120 ;

  /*********************************************/
  /* AM ANFANG NEUE SEITE                      */
  /*********************************************/

      ZZ := 1000 ;

  /*********************************************/
  /* FETTDRUCK AUS                             */
  /* Unterstreichung aus                       */
  /*********************************************/

      SCHATT := FALSE ;
      UNTERSTR := FALSE ;

  /*********************************************/
  /* 10 CHARACTERS PER INCH                    */
  /*********************************************/

      CPI := 10 ;

  /*********************************************/
  /* ZEILENVORSCHUB 1 ZEILE = 2 HALBZEILEN     */
  /*********************************************/

      LINEF := 2 ;

  /*********************************************/
  /* UEBERSCHRIFT LEER                         */
  /*********************************************/

      HEADING := '' ;

  /*********************************************/
  /* SEITENTEXT LEER                           */
  /*********************************************/

      SEITEXT := '' ;
      L_SEITEXT := 0 ;

  /*********************************************/
  /* SEITENZAHL EINS                           */
  /*********************************************/

      SEITE := 1 ;

  /*********************************************/
  /* BALKENDRUCK JA                            */
  /*********************************************/

      BALKENDR := TRUE
    end (* with *) ;
  while TRUE do
    begin

  /*********************************************/
  /* Zeile einlesen                            */
  /*********************************************/

      ZLEN := READLINE ( INPFILE , ZEILE ) ;
      if ZLEN < 0 then
        break ;

  /*********************************************/
  /* Steuerbefehl beginnt mit Punkt            */
  /* DO_CONTROL meldet FALSE, wenn             */
  /* Verarbeitung abgebrochen werden soll      */
  /* DO_CONTROL schickt leere Eingabezeile,    */
  /* wenn Steuerbefehl verarbeitet wurde       */
  /*********************************************/

      if ZEILE [ 1 ] = '.' then
        begin
          WEITER := DO_CONTROL ( ZEILE , ZLEN ) ;
          if not WEITER then
            break ;
          if ZEILE = ' ' then
            continue
        end (* then *) ;

  /*********************************************/
  /* Vorschub auf Zeilenzaehler aufrechnen     */
  /* und Kontrolle, ob evtl. Ueberschrift      */
  /* noetig ist                                */
  /*********************************************/

      STATUS . ZZ := STATUS . ZZ + STATUS . LINEF ;
      if STATUS . ZZGRENZE <> 0 then
        begin
          if STATUS . ZZ >= STATUS . ZZGRENZE then
            begin
              if STATUS . SEITE > STATUS . ENDESEITE then
                break ;
              UEBERSCHRIFT ;
            end (* then *)
        end (* then *) ;

  /***********************************************************/
  /*  spezialitaeten fuer fettdruck oder unterstreichen      */
  /***********************************************************/

      if STATUS . UNTERSTR or ( INSTR ( ZEILE , ZLEN , '#@' X'1819' , 4
      ) <> 0 ) then
        begin
          WORK_ZEILE ( ZEILE , ZLEN ) ;
          VORSCHUB ( STATUS . LINEF , TRUE ) ;
        end (* then *)
      else
        begin
          AUSGABE_CHAR ( ' ' , STATUS . ER ) ;
          AUSGABE_ZEILREC ( ZEILE , ZLEN ) ;
          VORSCHUB ( STATUS . LINEF , TRUE ) ;
        end (* else *)
    end (* while *)
end (* HAUPTPROGRAMM *) .
