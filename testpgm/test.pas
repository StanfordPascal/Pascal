program SCANERG ( INPUT , OUTPUT , SCANINP ) ;

//****************************************************************
//$A+,D-,N+           ...
//$A-,D-,N+
/*$A-,D-,N+*/
/* nest comm  /*    /*    */     */                             */
/*$A-,D-,N+   /*    /*    */     */                             */
/*$A-;D-,N+   /*    /*    */     */                             */
/*$M(1,79-,D+                                                   */
//****************************************************************
// * zeilenposition usw. mitfuehren
// * scan_code fuer ebcdic geeignet machen
// - listing mit syntaxfehlern usw.
// - scangenc entsprechend anpassen
//****************************************************************


const MAXLSIZE = 120 ;

type SOURCELINE = array [ 1 .. MAXLSIZE ] of CHAR ;
     SYMB = ( SYMB_EOF , SYMB_UNKNOWN , SEPARATOR , COMMENT , STRING ,
            IDENT , SYALL , SYOR , SYIGNORE , SYUPPER , SYKEY ,
            SYASSIGN , SYKOMMA , SYKLAMMAUF , SYKLAMMZU , SYSLASH ,
            SYRANGE , SYPLUS , SYMINUS , SYMULT , SYSTRIPU , SYEQUAL )
            ;
     $  %
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
                  end ;

var SCB : SCAN_BLOCK ;
    SCANINP : TEXT ;


    %


function TOUPPER ( C : CHAR ) : CHAR ;

   begin (* TOUPPER *)
     if C in [ 'a' .. 'i' , 'j' .. 'r' , 's' .. 'z' ] then
       C := CHR ( ORD ( C ) - ORD ( 'a' ) + ORD ( 'A' ) ) ;
     TOUPPER := C ;
   end (* TOUPPER *) ;


procedure SCAN_TOUPPER ( var SCB : SCAN_BLOCK ) ;

   var I : INTEGER ;

   begin (* SCAN_TOUPPER *)
     for I := 1 to SCB . LSYMBOL do
       SCB . SYMBOL [ I ] := TOUPPER ( SCB . SYMBOL [ I ] ) ;
   end (* SCAN_TOUPPER *) ;


function SCAN_CODE ( C : CHAR ) : INTEGER ;

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

   begin (* SCAN_CODE *)
     if ORD ( '0' ) = 0X30 then

     /*************************************************/
     /* that means: charset of target machine = ascii */
     /*************************************************/

       SCAN_CODE := ORD ( C )
     else
       SCAN_CODE := EBCDIC_TO_ASCII [ C ] ;
   end (* SCAN_CODE *) ;


procedure SCAN_PANIC ( X : INTEGER ; Y , Z : VOIDPTR ) ;

   begin (* SCAN_PANIC *)
     WRITELN ( 'Scan-Panic ErrorCode = ' , X ) ;
     EXIT ( X )
   end (* SCAN_PANIC *) ;


procedure SCAN_READC ( var SCB : SCAN_BLOCK ; var CH : CHAR ) ;

   begin (* SCAN_READC *)
     while TRUE do
       begin
         if SCB . LINEPOS < SCB . LINELEN then
           begin
             SCB . LINEPOS := SCB . LINEPOS + 1 ;
             CH := SCB . SLINE [ SCB . LINEPOS ] ;
             SCB . ENDOFLINE := FALSE ;
             return ;
           end (* then *) ;
         if SCB . LINEPOS = SCB . LINELEN then
           begin
             SCB . LINEPOS := SCB . LINEPOS + 1 ;
             CH := ' ' ;
             SCB . ENDOFLINE := TRUE ;
             return ;
           end (* then *) ;
         if EOF ( SCANINP ) then
           begin
             SCB . DATEIENDE := 1 ;
             SCB . SLINE := ' ' ;
             SCB . LINENR := SCB . LINENR + 1 ;
             SCB . LINEPOS := 1 ;
             SCB . LINELEN := 0 ;
             return ;
           end (* then *) ;
         READLN ( SCANINP , SCB . SLINE ) ;
         SCB . LINENR := SCB . LINENR + 1 ;
         SCB . LINEPOS := 0 ;
         SCB . LINELEN := MAXLSIZE ;
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


procedure SCANNER ( var SCB : SCAN_BLOCK ) ;

   var ALTZUST : INTEGER ;
       ZUST : INTEGER ;
       CH : CHAR ;


   procedure SCANNER2 ( ALTZUST : INTEGER ; var SCB : SCAN_BLOCK ) ;

      begin (* SCANNER2 *)

        /***************************************************/
        /*   Symbol setzen abh. vom Endzustand             */
        /***************************************************/

        if SCB . DATEIENDE <> 0 then
          SCB . SYMBOLNR := SYMB_EOF
        else
          case ALTZUST of
            2 : SCB . SYMBOLNR := SEPARATOR ;
            10 : SCB . SYMBOLNR := COMMENT ;
            15 : SCB . SYMBOLNR := IDENT ;
            16 : SCB . SYMBOLNR := IDENT ;
            18 : SCB . SYMBOLNR := SYASSIGN ;
            19 : SCB . SYMBOLNR := SYKOMMA ;
            20 : SCB . SYMBOLNR := SYKLAMMAUF ;
            21 : SCB . SYMBOLNR := SYKLAMMZU ;
            24 : SCB . SYMBOLNR := SYRANGE ;
            25 : SCB . SYMBOLNR := SYPLUS ;
            26 : SCB . SYMBOLNR := SYMINUS ;
            27 : SCB . SYMBOLNR := SYMULT ;
            28 : SCB . SYMBOLNR := SYSTRIPU ;
            29 : SCB . SYMBOLNR := SYEQUAL ;
            30 : SCB . SYMBOLNR := SYSLASH ;
            32 : SCB . SYMBOLNR := STRING ;
          end (* case *) ;
        if FALSE then
          WRITELN ( 'zust = ' , ALTZUST , ' symb = ' , SCB . SYMBOLNR :
                    20 , ' ' , SCB . SYMBOL : SCB . LSYMBOL ) ;

        /***************************************************/
        /*   Umsetzen in Grossbuchstaben, wo gefordert     */
        /***************************************************/

        case SCB . SYMBOLNR of
          IDENT : SCAN_TOUPPER ( SCB ) ;
        end (* case *) ;

        /***************************************************/
        /*   Uebersetzen Keywords                          */
        /***************************************************/

        case SCB . SYMBOLNR of
          IDENT : begin
                    if SCB . SYMBOL = 'ALL' then
                      SCB . SYMBOLNR := SYALL
                    else
                      if SCB . SYMBOL = 'OR' then
                        SCB . SYMBOLNR := SYOR
                      else
                        if SCB . SYMBOL = 'IGNORE' then
                          SCB . SYMBOLNR := SYIGNORE
                        else
                          if SCB . SYMBOL = 'UPPER' then
                            SCB . SYMBOLNR := SYUPPER
                          else
                            if SCB . SYMBOL = 'KEY' then
                              SCB . SYMBOLNR := SYKEY
                  end (* tag/ca *) ;
        end (* case *) ;

        /*****************************************/
        /* ausgabe fuer -G- zunaechst ausgesetzt */
        /*****************************************/

      end (* SCANNER2 *) ;

   begin (* SCANNER *)
     SCB . SYMBOL := '' ;
     if SCB . MODUS > 0 then
       begin
         SCAN_READC ( SCB , CH ) ;
         SCB . LOOKAHEAD := CH ;
         SCB . MODUS := 0
       end (* then *)
     else
       CH := SCB . LOOKAHEAD ;
     if SCB . DATEIENDE <> 0 then
       begin
         SCB . DATEIENDE := SCB . DATEIENDE + 1 ;
         if SCB . DATEIENDE > 5 then
           SCAN_PANIC ( 1 , NIL , NIL ) ;
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
         case ZUST of
           10 : ZUST := - 1 ;
           18 : ZUST := - 1 ;
           19 : ZUST := - 1 ;
           20 : ZUST := - 1 ;
           21 : ZUST := - 1 ;
           24 : ZUST := - 1 ;
           25 : ZUST := - 1 ;
           26 : ZUST := - 1 ;
           27 : ZUST := - 1 ;
           28 : ZUST := - 1 ;
           29 : ZUST := - 1 ;
           1 : begin
                 if ( SCAN_CODE ( CH ) >= SCAN_CODE ( 'A' ) ) and (
                 SCAN_CODE ( CH ) <= SCAN_CODE ( 'Z' ) ) then
                   ZUST := 15
                 else
                   if ( SCAN_CODE ( CH ) >= SCAN_CODE ( 'a' ) ) and (
                   SCAN_CODE ( CH ) <= SCAN_CODE ( 'z' ) ) then
                     ZUST := 15
                   else
                     case CH of
                       ' ' : ZUST := 2 ;
                       '''' : ZUST := 11 ;
                       '(' : ZUST := 20 ;
                       ')' : ZUST := 21 ;
                       '*' : ZUST := 27 ;
                       '+' : ZUST := 25 ;
                       ',' : ZUST := 19 ;
                       '-' : ZUST := 26 ;
                       '.' : ZUST := 23 ;
                       '/' : ZUST := 30 ;
                       ':' : ZUST := 17 ;
                       ';' : ZUST := 28 ;
                       '=' : ZUST := 29 ;
                       otherwise
                         ZUST := - 1 ;
                     end (* case *) ;
               end (* tag/ca *) ;
           2 : begin
                 case CH of
                   ' ' : ZUST := 2 ;
                   otherwise
                     ZUST := - 1 ;
                 end (* case *) ;
               end (* tag/ca *) ;
           4 : begin
                 if ( SCAN_CODE ( CH ) >= 1 ) and ( SCAN_CODE ( CH ) <=
                 SCAN_CODE ( ')' ) ) then
                   ZUST := 5
                 else
                   if ( SCAN_CODE ( CH ) >= SCAN_CODE ( '+' ) ) and (
                   SCAN_CODE ( CH ) <= 255 ) then
                     ZUST := 5
                   else
                     case CH of
                       '*' : ZUST := 31 ;
                       otherwise
                         ZUST := - 1 ;
                     end (* case *) ;
               end (* tag/ca *) ;
           5 : begin
                 if ( SCAN_CODE ( CH ) >= 1 ) and ( SCAN_CODE ( CH ) <=
                 SCAN_CODE ( ')' ) ) then
                   ZUST := 5
                 else
                   if ( SCAN_CODE ( CH ) >= SCAN_CODE ( '+' ) ) and (
                   SCAN_CODE ( CH ) <= 255 ) then
                     ZUST := 5
                   else
                     case CH of
                       '*' : ZUST := 31 ;
                       otherwise
                         ZUST := - 1 ;
                     end (* case *) ;
               end (* tag/ca *) ;
           7 : begin
                 if ( SCAN_CODE ( CH ) >= 1 ) and ( SCAN_CODE ( CH ) <=
                 SCAN_CODE ( ')' ) ) then
                   ZUST := 8
                 else
                   if ( SCAN_CODE ( CH ) >= SCAN_CODE ( '+' ) ) and (
                   SCAN_CODE ( CH ) <= 255 ) then
                     ZUST := 8
                   else
                     case CH of
                       '*' : ZUST := 31 ;
                       otherwise
                         ZUST := - 1 ;
                     end (* case *) ;
               end (* tag/ca *) ;
           8 : begin
                 if ( SCAN_CODE ( CH ) >= 1 ) and ( SCAN_CODE ( CH ) <=
                 SCAN_CODE ( ')' ) ) then
                   ZUST := 8
                 else
                   if ( SCAN_CODE ( CH ) >= SCAN_CODE ( '+' ) ) and (
                   SCAN_CODE ( CH ) <= 255 ) then
                     ZUST := 8
                   else
                     case CH of
                       '*' : ZUST := 31 ;
                       otherwise
                         ZUST := - 1 ;
                     end (* case *) ;
               end (* tag/ca *) ;
           9 : begin
                 case CH of
                   '*' : ZUST := 9 ;
                   '/' : ZUST := 10 ;
                   otherwise
                     ZUST := - 1 ;
                 end (* case *) ;
               end (* tag/ca *) ;
           11 : begin
                  if ( SCAN_CODE ( CH ) >= 1 ) and ( SCAN_CODE ( CH )
                  <= 9 ) then
                    ZUST := 12
                  else
                    if ( SCAN_CODE ( CH ) >= 11 ) and ( SCAN_CODE ( CH
                    ) <= SCAN_CODE ( '&' ) ) then
                      ZUST := 12
                    else
                      if ( SCAN_CODE ( CH ) >= SCAN_CODE ( '(' ) ) and
                      ( SCAN_CODE ( CH ) <= 255 ) then
                        ZUST := 12
                      else
                        case CH of
                          '''' : ZUST := 32 ;
                          otherwise
                            ZUST := - 1 ;
                        end (* case *) ;
                end (* tag/ca *) ;
           12 : begin
                  if ( SCAN_CODE ( CH ) >= 1 ) and ( SCAN_CODE ( CH )
                  <= 9 ) then
                    ZUST := 12
                  else
                    if ( SCAN_CODE ( CH ) >= 11 ) and ( SCAN_CODE ( CH
                    ) <= SCAN_CODE ( '&' ) ) then
                      ZUST := 12
                    else
                      if ( SCAN_CODE ( CH ) >= SCAN_CODE ( '(' ) ) and
                      ( SCAN_CODE ( CH ) <= 255 ) then
                        ZUST := 12
                      else
                        case CH of
                          '''' : ZUST := 32 ;
                          otherwise
                            ZUST := - 1 ;
                        end (* case *) ;
                end (* tag/ca *) ;
           15 : begin
                  if ( SCAN_CODE ( CH ) >= SCAN_CODE ( '0' ) ) and (
                  SCAN_CODE ( CH ) <= SCAN_CODE ( '9' ) ) then
                    ZUST := 16
                  else
                    if ( SCAN_CODE ( CH ) >= SCAN_CODE ( 'A' ) ) and (
                    SCAN_CODE ( CH ) <= SCAN_CODE ( 'Z' ) ) then
                      ZUST := 16
                    else
                      if ( SCAN_CODE ( CH ) >= SCAN_CODE ( 'a' ) ) and
                      ( SCAN_CODE ( CH ) <= SCAN_CODE ( 'z' ) ) then
                        ZUST := 16
                      else
                        case CH of
                          '_' : ZUST := 16 ;
                          otherwise
                            ZUST := - 1 ;
                        end (* case *) ;
                end (* tag/ca *) ;
           16 : begin
                  if ( SCAN_CODE ( CH ) >= SCAN_CODE ( '0' ) ) and (
                  SCAN_CODE ( CH ) <= SCAN_CODE ( '9' ) ) then
                    ZUST := 16
                  else
                    if ( SCAN_CODE ( CH ) >= SCAN_CODE ( 'A' ) ) and (
                    SCAN_CODE ( CH ) <= SCAN_CODE ( 'Z' ) ) then
                      ZUST := 16
                    else
                      if ( SCAN_CODE ( CH ) >= SCAN_CODE ( 'a' ) ) and
                      ( SCAN_CODE ( CH ) <= SCAN_CODE ( 'z' ) ) then
                        ZUST := 16
                      else
                        case CH of
                          '_' : ZUST := 16 ;
                          otherwise
                            ZUST := - 1 ;
                        end (* case *) ;
                end (* tag/ca *) ;
           17 : begin
                  case CH of
                    '=' : ZUST := 18 ;
                    otherwise
                      ZUST := - 1 ;
                  end (* case *) ;
                end (* tag/ca *) ;
           23 : begin
                  case CH of
                    '.' : ZUST := 24 ;
                    otherwise
                      ZUST := - 1 ;
                  end (* case *) ;
                end (* tag/ca *) ;
           30 : begin
                  case CH of
                    '*' : ZUST := 4 ;
                    otherwise
                      ZUST := - 1 ;
                  end (* case *) ;
                end (* tag/ca *) ;
           31 : begin
                  if ( SCAN_CODE ( CH ) >= 1 ) and ( SCAN_CODE ( CH )
                  <= SCAN_CODE ( ')' ) ) then
                    ZUST := 7
                  else
                    if ( SCAN_CODE ( CH ) >= SCAN_CODE ( '0' ) ) and (
                    SCAN_CODE ( CH ) <= 255 ) then
                      ZUST := 7
                    else
                      case CH of
                        '*' : ZUST := 9 ;
                        '+' : ZUST := 7 ;
                        ',' : ZUST := 7 ;
                        '-' : ZUST := 7 ;
                        '.' : ZUST := 7 ;
                        '/' : ZUST := 10 ;
                        otherwise
                          ZUST := - 1 ;
                      end (* case *) ;
                end (* tag/ca *) ;
           32 : begin
                  case CH of
                    '''' : ZUST := 12 ;
                    otherwise
                      ZUST := - 1 ;
                  end (* case *) ;
                end (* tag/ca *) ;
           otherwise
             begin
               SCAN_PANIC ( 2 , NIL , NIL )
             end (* otherw *)
         end (* case *) ;
         if ZUST > 0 then
           begin
             SCAN_READC ( SCB , CH ) ;
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
     SCB . SYMBOL [ SCB . LSYMBOL ] := ' ' ;
     SCB . LSYMBOL := SCB . LSYMBOL - 1 ;
     SCANNER2 ( ALTZUST , SCB ) ;
   end (* SCANNER *) ;

begin (* HAUPTPROGRAMM *)
  MEMSET ( ADDR ( SCB ) , CHR ( 0 ) , SIZEOF ( SCB ) ) ;
  SCB . MAXLSYMBOL := MAXLSIZE ;
  SCB . MODUS := 1 ;
  SCB . SYMBOLNR := SYMB_UNKNOWN ;
  SCB . SLINE := ' ' ;
  SCB . LINENR := 0 ;
  SCB . LINEPOS := 1 ;
  SCB . LINELEN := 0 ;
  while SCB . SYMBOLNR <> SYMB_EOF do
    begin
      SCANNER ( SCB ) ;
      WRITELN ( 'pos = ' , SCB . LINENR : 4 , '/' , SCB . LINEPOS : 3 ,
                ' symb = ' , SCB . SYMBOLNR : 20 , ' ' , SCB . SYMBOL :
                SCB . LSYMBOL ) ;
    end (* while *)
end (* HAUPTPROGRAMM *) .
