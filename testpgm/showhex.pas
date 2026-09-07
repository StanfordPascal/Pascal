program SHOWHEX ( INPUT , INPTEXT , INPBIN , OUTPUT ) ;


const BUFLEN = 256 ;


type BYTE = record
              CONT : CHAR ;
            end ;
     FILEMODE = ( MODE_TEXT , MODE_BINARY ) ;


var I : INTEGER ;
    CH : CHAR ;
    BUFFER : CHAR ( BUFLEN ) ;
    LINE : INTEGER ;
    POS : INTEGER ;
    INPTEXT : TEXT ;
    INPBIN : FILE of BYTE ;
    PARM_MODE : FILEMODE ;
    PARM_EBCDIC : BOOLEAN := FALSE ;
    PARM_FILENAME : STRING ( 250 ) ;
    PARM_RECL : INTEGER := 32 ;



procedure CHKPARMS ;

/***********************************/
/* check run time parms via osparm */
/* option -b or -t (bin or text)   */
/* and filename                    */
/***********************************/


   var SX : INTEGER ;
       STATUS : INTEGER ;

   begin (* CHKPARMS *)
     if OSPARM = NIL then
       begin
         PARM_MODE := MODE_TEXT ;
         PARM_FILENAME := '' ;
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
                           begin
                             STATUS := 3 ;
                             PARM_FILENAME := STR ( PSTRING [ SX ] ) ;
                           end (* else *) ;
                   2 : if PSTRING [ SX ] = ' ' then
                         STATUS := 99
                       else
                         if PSTRING [ SX ] = '-' then
                           STATUS := 99
                         else
                           begin
                             STATUS := 4 ;
                             if PSTRING [ SX ] = 'b' then
                               PARM_MODE := MODE_BINARY
                             else
                               if PSTRING [ SX ] = 'e' then
                                 PARM_EBCDIC := TRUE
                               else
                                 if PSTRING [ SX ] = 'l' then
                                   begin
                                     STATUS := 5 ;
                                     PARM_RECL := 0 ;
                                   end (* then *)
                                 else
                                   if PSTRING [ SX ] = 't' then
                                     PARM_MODE := MODE_TEXT
                                   else
                                     STATUS := 99 ;
                           end (* else *) ;
                   3 : if PSTRING [ SX ] = ' ' then
                         STATUS := 1
                       else
                         begin
                           PARM_FILENAME := PARM_FILENAME || STR (
                                            PSTRING [ SX ] ) ;
                           STATUS := 3 ;
                         end (* else *) ;
                   4 : if PSTRING [ SX ] = ' ' then
                         STATUS := 1
                       else
                         STATUS := 99 ;
                   5 : if PSTRING [ SX ] = ' ' then
                         STATUS := 1
                       else
                         if PSTRING [ SX ] in [ '0' .. '9' ] then
                           begin
                             PARM_RECL := PARM_RECL * 10 + ORD ( (
                                          PSTRING [ SX ] ) ) - ORD (
                                          '0' ) ;
                             if PARM_RECL > BUFLEN then
                               begin
                                 WRITELN (
           '+++ Fehler: Parameter fuer Satzlaenge zu gross (> BUFLEN)'
                                           ) ;
                                 EXIT ( 12 )
                               end (* then *)
                           end (* then *)
                         else
                           STATUS := 99 ;
                 end (* case *)
               end (* for *) ;
           end (* with *) ;
       end (* else *) ;
   end (* CHKPARMS *) ;



procedure DUMPCHAR ( CH : CHAR ) ;

   begin (* DUMPCHAR *)
     if PARM_EBCDIC then
       begin
         case CH of
           X'40' : WRITE ( ' ' ) ;
           X'4B' : WRITE ( '.' ) ;
           X'6B' : WRITE ( ',' ) ;
           X'60' : WRITE ( '-' ) ;
           otherwise
             begin
               if ( CH >= X'F0' ) and ( CH <= x'F9' ) then
                 begin
                   CH := CHR ( ORD ( '0' ) + ORD ( CH ) - 0xf0 ) ;
                   WRITE ( CH ) ;
                   return ;
                 end (* then *) ;
               if ( CH >= X'C1' ) and ( CH <= x'C9' ) then
                 begin
                   CH := CHR ( ORD ( 'A' ) + ORD ( CH ) - 0xc1 ) ;
                   WRITE ( CH ) ;
                   return ;
                 end (* then *) ;
               if ( CH >= X'D1' ) and ( CH <= x'D9' ) then
                 begin
                   CH := CHR ( ORD ( 'J' ) + ORD ( CH ) - 0xd1 ) ;
                   WRITE ( CH ) ;
                   return ;
                 end (* then *) ;
               if ( CH >= X'E2' ) and ( CH <= x'E9' ) then
                 begin
                   CH := CHR ( ORD ( 'S' ) + ORD ( CH ) - 0xe2 ) ;
                   WRITE ( CH ) ;
                   return ;
                 end (* then *) ;
               if ( CH >= X'81' ) and ( CH <= x'89' ) then
                 begin
                   CH := CHR ( ORD ( 'a' ) + ORD ( CH ) - 0x81 ) ;
                   WRITE ( CH ) ;
                   return ;
                 end (* then *) ;
               if ( CH >= X'91' ) and ( CH <= x'99' ) then
                 begin
                   CH := CHR ( ORD ( 'j' ) + ORD ( CH ) - 0x91 ) ;
                   WRITE ( CH ) ;
                   return ;
                 end (* then *) ;
               if ( CH >= X'a2' ) and ( CH <= x'a9' ) then
                 begin
                   CH := CHR ( ORD ( 's' ) + ORD ( CH ) - 0xa2 ) ;
                   WRITE ( CH ) ;
                   return ;
                 end (* then *) ;
               WRITE ( '.' ) ;
             end (* otherw *)
         end (* case *) ;
         return ;
       end (* then *) ;
     if CH in [ 'a' .. 'i' , 'j' .. 'r' , 's' .. 'z' , 'A' .. 'I' , 'J'
     .. 'R' , 'S' .. 'Z' , '0' .. '9' , ' ' , ',' , '.' , '-' , ';' ,
     ':' , '_' , '!' , '"' , '>' , '<' , '$' , '%' , '&' , '/' , '(' ,
     ')' , '=' , '?' , '+' , '*' , '#' , '*' ] then
       WRITE ( CH )
     else
       WRITE ( '.' )
   end (* DUMPCHAR *) ;



function ENDOFFILE : BOOLEAN ;

   begin (* ENDOFFILE *)
     if PARM_MODE = MODE_TEXT then
       ENDOFFILE := EOF ( INPTEXT )
     else
       ENDOFFILE := EOF ( INPBIN )
   end (* ENDOFFILE *) ;



function ENDOFLINE : BOOLEAN ;

   begin (* ENDOFLINE *)
     if PARM_MODE = MODE_TEXT then
       ENDOFLINE := EOLN ( INPTEXT )
     else
       ENDOFLINE := FALSE
   end (* ENDOFLINE *) ;



function GETFILE : CHAR ;

   begin (* GETFILE *)
     if PARM_MODE = MODE_TEXT then
       begin
         GETFILE := INPTEXT -> ;
         GET ( INPTEXT ) ;
       end (* then *)
     else
       begin
         GETFILE := INPBIN -> . CONT ;
         GET ( INPBIN ) ;
       end (* else *)
   end (* GETFILE *) ;



procedure BUFFER_AUSG ;

   var C1 : CHAR ;
       C2 : CHAR ;
       START , ENDE : INTEGER ;

   const HEX : packed array [ 0 .. 15 ] of CHAR = '0123456789abcdef' ;

   begin (* BUFFER_AUSG *)
     START := 1 ;
     ENDE := I ;
     if POS = 1 then
       WRITELN ( 'Line ' , LINE : 1 ) ;
     WRITE ( 'Pos ' , POS : - 7 , ': ' ) ;
     for I := START to ENDE do
       begin
         CH := BUFFER [ I ] ;
         C1 := HEX [ ORD ( CH ) DIV 16 ] ;
         C2 := HEX [ ORD ( CH ) MOD 16 ] ;
         WRITE ( C1 , C2 ) ;
         if I MOD 4 = 0 then
           WRITE ( ' ' ) ;
       end (* for *) ;
     WRITELN ;
     WRITE ( ' to ' , POS + ENDE - 1 : - 7 , ': ' ) ;
     for I := START to ENDE do
       begin
         DUMPCHAR ( BUFFER [ I ] ) ;
         WRITE ( ' ' ) ;
         if I MOD 4 = 0 then
           WRITE ( ' ' ) ;
       end (* for *) ;
     WRITELN ;
   end (* BUFFER_AUSG *) ;



begin (* HAUPTPROGRAMM *)
  CHKPARMS ;
  LINE := 1 ;
  POS := 1 ;
  I := 0 ;
  MEMSET ( ADDR ( BUFFER ) , ' ' , BUFLEN ) ;
  if PARM_MODE = MODE_TEXT then
    RESET ( INPTEXT )
  else
    RESET ( INPBIN ) ;
  while not ENDOFFILE do
    begin
      if not ENDOFLINE then
        begin
          CH := GETFILE ;
          I := I + 1 ;
          BUFFER [ I ] := CH ;
        end (* then *) ;
      if ENDOFLINE or ( I >= PARM_RECL ) then
        begin
          BUFFER_AUSG ;

  (******************************)
  (* BLANK NACH EOLN UEBERLESEN *)
  (******************************)

          if ENDOFLINE then
            begin
              if not ENDOFFILE then
                begin
                  CH := GETFILE ;
                end (* then *) ;
              LINE := LINE + 1 ;
              POS := 1 ;
            end (* then *)
          else
            begin
              POS := POS + PARM_RECL
            end (* else *) ;
          MEMSET ( ADDR ( BUFFER ) , ' ' , BUFLEN ) ;
          I := 0 ;
        end (* then *) ;
    end (* while *) ;
  if I > 0 then
    BUFFER_AUSG ;
end (* HAUPTPROGRAMM *) .
