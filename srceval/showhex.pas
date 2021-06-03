program SHOWHEX ( INPUT , INPTEXT , INPBIN , OUTPUT ) ;


const BUFLEN = 32 ;


type BYTE = record
              CONT : CHAR ;
            end ;
     FILEMODE = ( MODE_TEXT , MODE_BINARY ) ;


var I : INTEGER ;
    CH : CHAR ;
    BUFFER : array [ 1 .. BUFLEN ] of CHAR ;
    LINE : INTEGER ;
    POS : INTEGER ;
    INPTEXT : TEXT ;
    INPBIN : FILE of BYTE ;
    X : INTEGER ;
    PARM_MODE : FILEMODE ;
    PARM_FILENAME : array [ 1 .. 250 ] of CHAR ;
    PARM_LF : INTEGER ;



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
         PARM_FILENAME := ' ' ;
         PARM_LF := 0 ;
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
                             PARM_FILENAME := ' ' ;
                             PARM_FILENAME [ 1 ] := PSTRING [ SX ] ;
                             PARM_LF := 1 ;
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
                               if PSTRING [ SX ] = 't' then
                                 PARM_MODE := MODE_TEXT
                               else
                                 STATUS := 99 ;
                           end (* else *) ;
                   3 : if PSTRING [ SX ] = ' ' then
                         STATUS := 1
                       else
                         begin
                           PARM_LF := PARM_LF + 1 ;
                           PARM_FILENAME [ PARM_LF ] := PSTRING [ SX ]
                                                   ;
                           STATUS := 3 ;
                         end (* else *) ;
                   4 : if PSTRING [ SX ] = ' ' then
                         STATUS := 1
                       else
                         STATUS := 99 ;
                 end (* case *)
               end (* for *) ;
           end (* with *) ;
       end (* else *) ;
   end (* CHKPARMS *) ;



procedure DUMPCHAR ( CH : CHAR ) ;

   begin (* DUMPCHAR *)
     if CH in [ 'a' .. 'i' , 'j' .. 'r' , 's' .. 'z' , 'A' .. 'I' , 'J'
     .. 'R' , 'S' .. 'Z' , '0' .. '9' , ' ' , ',' , '.' , '-' , ';' ,
     ':' , '_' , '!' , '"' , '>' , '<' , '$' , '%' , '&' , '/' , '(' ,
     ')' , '=' , '?' , '+' , '*' , '#' , '*' ] then
       WRITE ( CH )
     else
       WRITE ( '.' )
   end (* DUMPCHAR *) ;



function ISTELLEN ( I : INTEGER ) : INTEGER ;

   var IMAX : INTEGER ;
       IST : INTEGER ;

   begin (* ISTELLEN *)
     I := ABS ( I ) ;
     IMAX := 1000000000 ;
     IST := 10 ;
     while ( I < IMAX ) and ( IMAX > 1 ) do
       begin
         IMAX := IMAX DIV 10 ;
         IST := IST - 1
       end (* while *) ;
     ISTELLEN := IST
   end (* ISTELLEN *) ;



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
     X := ISTELLEN ( POS + ENDE - 1 ) ;
     if X < 3 then
       X := 3 ;
     WRITE ( 'Pos ' , POS : X , ': ' ) ;
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
     WRITE ( ' to ' , POS + ENDE - 1 : X , ': ' ) ;
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
      if ENDOFLINE or ( I >= BUFLEN ) then
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
              POS := POS + BUFLEN ;
            end (* else *) ;
          MEMSET ( ADDR ( BUFFER ) , ' ' , BUFLEN ) ;
          I := 0 ;
        end (* then *) ;
    end (* while *) ;
  if I > 0 then
    BUFFER_AUSG ;
end (* HAUPTPROGRAMM *) .
