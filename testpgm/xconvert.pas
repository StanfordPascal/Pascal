program XCONVERT ( TXTFILE , BINFILE , OUTPUT ) ;


const BUFSIZE = 32 ;
      BUFSIZE_CHAR = 64 ;
      CONVTAB : array [ 0 .. 15 ] of CHAR = 'qwertzuiopasdfgh' ;


type BYTE = record
              CONT : CHAR ;
            end ;
     TRANSFER = ( TO_TEXT , TO_BINARY ) ;


var TXTFILE : TEXT ;
    BINFILE : FILE of BYTE ;
    PARM_MODE : TRANSFER ;
    BUFBIN : CHAR ( BUFSIZE ) ;
    BUFCHAR : CHAR ( BUFSIZE_CHAR ) ;
    I : INTEGER ;
    CH : CHAR ;
    ZEILEN : INTEGER ;
    BYTES : INTEGER ;
    ZEILVAR : STRING ( BUFSIZE_CHAR ) ;
    DECONVTAB : array [ 'a' .. 'z' ] of INTEGER ;



procedure BUILD_DECONVTAB ;

   var C : CHAR ;

   begin (* BUILD_DECONVTAB *)
     for C := 'a' to 'z' do
       DECONVTAB [ C ] := 0 ;
     for I := 0 to 15 do
       DECONVTAB [ CONVTAB [ I ] ] := I ;
   end (* BUILD_DECONVTAB *) ;



function CHKPARMS : BOOLEAN ;

/***********************************/
/* check run time parms via osparm */
/* option -x or -c                 */
/* -x = xconvert to coded          */
/* -c = xconvert to uncoded        */
/***********************************/


   var SX : INTEGER ;
       STATUS : INTEGER ;

   begin (* CHKPARMS *)
     if OSPARM = NIL then
       begin
         PARM_MODE := TO_TEXT ;
         CHKPARMS := TRUE
       end (* then *)
     else
       begin
         with OSPARM -> do
           begin
             STATUS := 1 ;
             for SX := 1 to PLENGTH do
               begin
                 case STATUS of
                   1 : if PSTRING [ SX ] = ' ' then
                         STATUS := 1
                       else
                         if PSTRING [ SX ] = '-' then
                           STATUS := 2
                         else
                           STATUS := 99 ;
                   2 : if PSTRING [ SX ] = ' ' then
                         STATUS := 99
                       else
                         begin
                           STATUS := 4 ;
                           if PSTRING [ SX ] = 'c' then
                             PARM_MODE := TO_BINARY
                           else
                             if PSTRING [ SX ] = 'x' then
                               PARM_MODE := TO_TEXT
                             else
                               STATUS := 99 ;
                         end (* else *) ;
                   4 : if PSTRING [ SX ] = ' ' then
                         STATUS := 1
                       else
                         STATUS := 99 ;
                 end (* case *)
               end (* for *) ;
           end (* with *) ;
         CHKPARMS := STATUS <> 99 ;
       end (* else *) ;
   end (* CHKPARMS *) ;



function ENDOFFILE : BOOLEAN ;

   begin (* ENDOFFILE *)
     ENDOFFILE := EOF ( BINFILE )
   end (* ENDOFFILE *) ;



function GETFILE : CHAR ;

   begin (* GETFILE *)
     GETFILE := BINFILE -> . CONT ;
     GET ( BINFILE ) ;
   end (* GETFILE *) ;



procedure CONV_TXT ( var CBIN : CHAR ; var CCHAR : CHAR ) ;

   var X1 , X2 : INTEGER ;
       CP1 , CP2 : -> CHAR ;

   begin (* CONV_TXT *)
     X1 := ORD ( CBIN ) ;
     X2 := X1 MOD 16 ;
     X1 := X1 DIV 16 ;
     CP1 := ADDR ( CCHAR ) ;
     CP2 := PTRADD ( CP1 , 1 ) ;
     CP1 -> := CONVTAB [ X1 ] ;
     CP2 -> := CONVTAB [ X2 ] ;
   end (* CONV_TXT *) ;



procedure CONV_BIN ( var CCHAR : CHAR ; var CBIN : CHAR ) ;

   var X1 , X2 : INTEGER ;
       CP1 , CP2 : -> CHAR ;
       X : INTEGER ;

   begin (* CONV_BIN *)
     CP1 := ADDR ( CCHAR ) ;
     CP2 := PTRADD ( CP1 , 1 ) ;
     X1 := DECONVTAB [ CP1 -> ] ;
     X2 := DECONVTAB [ CP2 -> ] ;
     X := X1 * 16 + X2 ;
     CBIN := CHR ( X ) ;
   end (* CONV_BIN *) ;



procedure BUFBIN_AUSG ( COUNT : INTEGER ) ;

   begin (* BUFBIN_AUSG *)
     BUFCHAR := ' ' ;
     for I := 1 to COUNT do
       CONV_TXT ( BUFBIN [ I ] , BUFCHAR [ 2 * I - 1 ] ) ;
     WRITELN ( TXTFILE , RTRIM ( BUFCHAR ) ) ;
   end (* BUFBIN_AUSG *) ;



procedure BUFCHAR_AUSG ( COUNT : INTEGER ) ;

   var CH : CHAR ;

   begin (* BUFCHAR_AUSG *)
     for I := 1 to COUNT do
       begin
         CONV_BIN ( BUFCHAR [ 2 * I - 1 ] , CH ) ;
         BINFILE -> . CONT := CH ;
         PUT ( BINFILE ) ;
       end (* for *) ;
   end (* BUFCHAR_AUSG *) ;



begin (* HAUPTPROGRAMM *)
  if not CHKPARMS then
    begin
      WRITELN ( '+++ Fehlerhafter Parameter (-c oder -x)' ) ;
      EXIT ( 8 )
    end (* then *) ;
  BUILD_DECONVTAB ;
  case PARM_MODE of
    TO_TEXT :
      begin
        ZEILEN := 0 ;
        BYTES := 0 ;
        RESET ( BINFILE ) ;
        REWRITE ( TXTFILE ) ;
        I := 0 ;
        MEMSET ( ADDR ( BUFBIN ) , ' ' , SIZEOF ( BUFBIN ) ) ;
        while not ENDOFFILE do
          begin
            CH := GETFILE ;
            I := I + 1 ;
            BUFBIN [ I ] := CH ;
            if I >= SIZEOF ( BUFBIN ) then
              begin
                BUFBIN_AUSG ( I ) ;
                BYTES := BYTES + I ;
                ZEILEN := ZEILEN + 1 ;
                MEMSET ( ADDR ( BUFBIN ) , ' ' , SIZEOF ( BUFBIN ) ) ;
                I := 0 ;
              end (* then *) ;
          end (* while *) ;
        if I > 0 then
          begin
            BUFBIN_AUSG ( I ) ;
            BYTES := BYTES + I ;
            ZEILEN := ZEILEN + 1 ;
          end (* then *) ;
        WRITELN ( '*** ' , BYTES : 1 , ' Bytes gelesen' ) ;
        WRITELN ( '*** ' , ZEILEN : 1 , ' Zeilen ausgegeben' ) ;
        WRITELN ( '*** Datei erfolgreich codiert' ) ;
        CLOSE ( BINFILE ) ;
        CLOSE ( TXTFILE ) ;
      end (* tag/ca *) ;
    TO_BINARY :
      begin
        ZEILEN := 0 ;
        BYTES := 0 ;
        RESET ( TXTFILE ) ;
        REWRITE ( BINFILE ) ;
        while not EOF ( TXTFILE ) do
          begin
            READLN ( TXTFILE , ZEILVAR ) ;
            ZEILEN := ZEILEN + 1 ;
            BUFCHAR := ZEILVAR ;
            BUFCHAR_AUSG ( LENGTH ( ZEILVAR ) DIV 2 ) ;
            BYTES := BYTES + LENGTH ( ZEILVAR ) DIV 2
          end (* while *) ;
        WRITELN ( '*** ' , ZEILEN : 1 , ' Zeilen gelesen' ) ;
        WRITELN ( '*** ' , BYTES : 1 , ' Bytes ausgegeben' ) ;
        WRITELN ( '*** Datei erfolgreich decodiert' ) ;
      end (* tag/ca *)
  end (* case *)
end (* HAUPTPROGRAMM *) .
