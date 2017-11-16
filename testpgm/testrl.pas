program TESTRL ( INPUT , OUTPUT , SCANINP ) ;

(********************************************)
(*$A+                                       *)
(********************************************)
(*  scan_code fuer ebcdic geeignet machen   *)
(*  zeilenposition usw. mitfuehren          *)
(*  listing mit syntaxfehlern usw.          *)
(*  scangenc entsprechend anpassen          *)
(********************************************)



const MAXLSIZE = 120 ;


type SOURCELINE = array [ 1 .. MAXLSIZE ] of CHAR ;
     SYMB = ( SYMB_EOF , SYMB_UNKNOWN , SEPARATOR , COMMENT , STRING ,
            IDENT , SYALL , SYOR , SYIGNORE , SYUPPER , SYKEY ,
            SYASSIGN , SYKOMMA , SYKLAMMAUF , SYKLAMMZU , SYSLASH ,
            SYRANGE , SYPLUS , SYMINUS , SYMULT , SYSTRIPU , SYEQUAL )
            ;
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
    CH : CHAR ;



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



begin (* HAUPTPROGRAMM *)
  MEMSET ( ADDR ( SCB ) , CHR ( 0 ) , SIZEOF ( SCB ) ) ;
  SCB . MAXLSYMBOL := MAXLSIZE ;
  SCB . MODUS := 1 ;
  SCB . SYMBOLNR := SYMB_UNKNOWN ;
  SCB . SLINE := ' ' ;
  SCB . LINENR := 0 ;
  SCB . LINEPOS := 1 ;
  SCB . LINELEN := 0 ;
  while SCB . DATEIENDE = 0 do
    begin
      SCAN_READC ( SCB , CH ) ;
      WRITE ( CH ) ;
      if SCB . ENDOFLINE then
        WRITELN
    end (* while *) ;
end (* HAUPTPROGRAMM *) .
