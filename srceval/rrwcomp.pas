program RRWCOMP ( SOURCE , LISTING ) ;

//**************************************************************
//                                                              
// rrwcomp = syntax check and analyzing tool                    
// for resident report writer reports                           
//                                                              
// bernd oppolzer - 02.2020                                     
//                                                              
//**************************************************************
// offene Punkte:                                               
//                                                              
// * Zeilennummern der Eingabe mitfuehren                       
// * Liste der verwendeten und vorgefundenen Namen und Attribute
// * via Cache                                                  
// * Keys fuer Namenscache muessen Key-Namensraum enthalten     
//   weil Label und Felder gleich benannt sein koennen          
// * Variablen ueberall in Namenscache eintragen                
// * Cache am Ende ausgeben                                     
// * Referenzliste                                              
// * Definitionen in Referenzliste separat                      
// * Variablen innerhalb von Funktionsaufrufen parsen           
// * Variablen an allen Stellen parsen (DO, Indexwerte, ...)    
// - Aufsetzen AVLTREE mit START oder so klappt nicht           
// - Pruefungen rein und Fehlermeldungen erzeugen               
//**************************************************************



type NAMEKEY = record
                 NAMECLASS : CHAR ;
                 NAME : CHAR ( 10 )
               end ;
     NAMEINFO = record
                  DEFTYPE : CHAR ;
                  DEFLENGTH : INTEGER ;
                  DEFSCALE : INTEGER ;
                  DEFDIM : INTEGER ;
                  SLINENR : INTEGER ;
                end ;
     USEKEY = record
                NAMECLASS : CHAR ;
                NAME : CHAR ( 10 ) ;
                IS_DEF : CHAR ;
                USE_SLINENR : CHAR ( 5 ) ;
                USE_SEQNO : CHAR ( 3 ) ;
              end ;
     USEINFO = record
                 USEKIND : CHAR ;
               end ;


var SOURCE : TEXT ;
    LISTING : TEXT ;

    //***********************************
    // global variables for source scan  
    //***********************************

    STLABEL : CHAR ( 8 ) ;
    SLINE : STRING ( 100 ) ;
    SLINENR : INTEGER ;
    ST : STRING ( 1000 ) ;
    SIX : INTEGER ;
    NEST : INTEGER ;

    //*************************************************
    // global variables for name tree and use tree     
    //*************************************************

    PNAMCACHE : VOIDPTR ;
    PUSECACHE : VOIDPTR ;



function AVLCACHE ( FUNKCODE : CHAR ( 8 ) ;   // Funktionscode
                  PHANDLE : VOIDPTR ;         // Cachehandle
                  var SEQKEY : VOIDPTR ;      // seq. Keyposition,
                  var PKEY : VOIDPTR ;        // Zeiger auf Key
                  var LKEY : INTEGER ;        // Laenge Key
                  var PDAT : VOIDPTR ;        // Zeiger auf Daten
                  var LDAT : INTEGER )        // Laenge Daten
                  : INTEGER ;

   EXTERNAL ;



procedure ERROR ( ERRNO : INTEGER ) ;

   begin (* ERROR *)
     case ERRNO of
       1 : WRITELN ( LISTING , '+++ Falscher Operand links' ) ;
       2 : WRITELN ( LISTING , '+++ Fehler in Operand 1 von DO' ) ;
       3 : WRITELN ( LISTING , '+++ Fehler in Operand 2 von DO' ) ;
       4 : WRITELN ( LISTING , '+++ Fehler in Operand 3 von DO' ) ;
       5 : WRITELN ( LISTING , '+++ Gleichheitszeichen fehlt' ) ;
       otherwise
         WRITELN ( LISTING , '+++ unbekannte fehlernummer ' , ERRNO : 1
                   ) ;
     end (* case *)
   end (* ERROR *) ;



function ITOS ( VAL : INTEGER ; L : INTEGER ) : STRING ;

   var BUFFER : STRING ( 30 ) ;
       I : INTEGER ;
       MINUS : BOOLEAN ;
       LETZT : INTEGER ;

   begin (* ITOS *)
     BUFFER := REPEATSTR ( ' ' , 30 ) ;
     I := 30 ;
     if VAL < 0 then
       begin
         VAL := - VAL ;
         MINUS := TRUE
       end (* then *)
     else
       MINUS := FALSE ;
     while VAL > 0 do
       begin
         LETZT := VAL MOD 10 ;
         BUFFER [ I ] := CHR ( ORD ( '0' ) + LETZT ) ;
         I := I - 1 ;
         VAL := VAL DIV 10 ;
       end (* while *) ;
     if MINUS then
       begin
         BUFFER [ I ] := '-' ;
         I := I - 1 ;
       end (* then *) ;
     ITOS := RIGHT ( BUFFER , L ) ;
   end (* ITOS *) ;



procedure NAME_INTO_CACHE ( CL : CHAR ; NAME : CHAR ( 10 ) ; INFO :
                          NAMEINFO ; USEKIND : CHAR ) ;

//********************************************************
// inserting name into cache                              
// class = namespace                                      
// needed because names for def and labels may overlap    
// key is built by concatenating cl and name              
// data is nameinfo (different for different namespaces)  
//********************************************************


   var CACHENAME : CHAR ( 8 ) ;
       SEQKEY : VOIDPTR ;
       PKEY : VOIDPTR ;
       LKEY : INTEGER ;
       PDAT : VOIDPTR ;
       LDAT : INTEGER ;
       RC : INTEGER ;
       NKEY : NAMEKEY ;
       NINFO : -> NAMEINFO ;
       UKEY : USEKEY ;
       NEWUI : USEINFO ;
       SEQNO : INTEGER ;

   begin (* NAME_INTO_CACHE *)
     if CL = ' ' then
       return ;
     if NAME = ' ' then
       return ;
     NKEY . NAMECLASS := CL ;
     NKEY . NAME := NAME ;
     WRITELN ( '*** trying to write into name cache: ' , CL , '-' ,
               NAME ) ;

     //***********************************
     // create cache if pnamcache is nil  
     //***********************************

     if PNAMCACHE = NIL then
       begin
         PKEY := ADDR ( CACHENAME ) ;
         CACHENAME := 'CANAME' ;
         RC := AVLCACHE ( 'CREATE' , NIL , SEQKEY , PKEY , LKEY , PDAT
               , LDAT ) ;
         if RC <> 0 then
           begin
             WRITELN ( '+++ error when creating name cache - rc = ' ,
                       RC : 1 ) ;
             EXIT ( 20 )
           end (* then *) ;
         PNAMCACHE := PDAT ;
       end (* then *) ;

     //***********************************
     // create cache if pusecache is nil  
     //***********************************

     if PUSECACHE = NIL then
       begin
         PKEY := ADDR ( CACHENAME ) ;
         CACHENAME := 'CAUSE' ;
         RC := AVLCACHE ( 'CREATE' , NIL , SEQKEY , PKEY , LKEY , PDAT
               , LDAT ) ;
         if RC <> 0 then
           begin
             WRITELN ( '+++ error when creating name cache - rc = ' ,
                       RC : 1 ) ;
             EXIT ( 20 )
           end (* then *) ;
         PUSECACHE := PDAT ;
       end (* then *) ;

     //*************************************************************
     // read name entry from cache                                  
     //*************************************************************

     PKEY := ADDR ( NKEY ) ;
     LKEY := SIZEOF ( NKEY ) ;
     PDAT := NIL ;
     LDAT := 0 ;
     RC := AVLCACHE ( 'GET' , PNAMCACHE , SEQKEY , PKEY , LKEY , PDAT ,
           LDAT ) ;

     //****************************************************
     // if entry exists try to update meta information     
     //****************************************************

     if RC = 0 then
       begin
         NINFO := PDAT ;
         if NINFO -> . DEFTYPE = ' ' then
           if INFO . DEFTYPE <> ' ' then
             NINFO -> := INFO ;
       end (* then *)
     else

     //****************************************************
     // otherwise add entry to cache                       
     //****************************************************

       begin
         PKEY := ADDR ( NKEY ) ;
         LKEY := SIZEOF ( NKEY ) ;
         PDAT := ADDR ( INFO ) ;
         LDAT := SIZEOF ( INFO ) ;
         RC := AVLCACHE ( 'PUT' , PNAMCACHE , SEQKEY , PKEY , LKEY ,
               PDAT , LDAT ) ;
         if RC <> 0 then
           begin
             WRITELN ( '+++ error when writing into name cache - rc = '
                       , RC : 1 ) ;
             EXIT ( 20 )
           end (* then *)
         else
           begin
             WRITELN ( '*** inserting name into name cache: ' , CL ,
                       '-' , NAME ) ;
           end (* else *)
       end (* else *) ;

     //*************************************************************
     // read usage entry from cache                                 
     // if found increment sequence number                          
     // and add another entry                                       
     //*************************************************************

     UKEY . NAMECLASS := CL ;
     UKEY . NAME := NAME ;
     if USEKIND = 'D' then
       UKEY . IS_DEF := 'J'
     else
       UKEY . IS_DEF := 'N' ;
     UKEY . USE_SLINENR := ITOS ( SLINENR , 5 ) ;
     SEQNO := 1 ;
     UKEY . USE_SEQNO := ITOS ( SEQNO , 3 ) ;
     PKEY := ADDR ( UKEY ) ;
     LKEY := SIZEOF ( UKEY ) ;
     PDAT := NIL ;
     LDAT := 0 ;
     RC := AVLCACHE ( 'GET' , PUSECACHE , SEQKEY , PKEY , LKEY , PDAT ,
           LDAT ) ;
     while RC = 0 do
       begin
         SEQNO := SEQNO + 1 ;
         UKEY . USE_SEQNO := ITOS ( SEQNO , 3 ) ;
         RC := AVLCACHE ( 'GET' , PUSECACHE , SEQKEY , PKEY , LKEY ,
               PDAT , LDAT ) ;
       end (* while *) ;

     //***********************
     // add entry into cache  
     //***********************

     NEWUI . USEKIND := USEKIND ;
     PKEY := ADDR ( UKEY ) ;
     LKEY := SIZEOF ( UKEY ) ;
     PDAT := ADDR ( NEWUI ) ;
     LDAT := SIZEOF ( NEWUI ) ;
     RC := AVLCACHE ( 'PUT' , PUSECACHE , SEQKEY , PKEY , LKEY , PDAT ,
           LDAT ) ;
     if RC <> 0 then
       begin
         WRITELN ( '+++ error when writing into usage cache - rc = ' ,
                   RC : 1 ) ;
         EXIT ( 20 )
       end (* then *)
     else
       begin
         WRITELN ( '*** inserting name into usage cache: ' , CL , '-' ,
                   NAME ) ;
       end (* else *) ;
   end (* NAME_INTO_CACHE *) ;



procedure INDEX_INTO_CACHE ( IXNAME : CHAR ( 10 ) ) ;

   var IXINFO : NAMEINFO ;

   begin (* INDEX_INTO_CACHE *)
     if IXNAME [ 1 ] in [ 'A' .. 'Z' ] then
       begin
         IXINFO . DEFTYPE := ' ' ;
         IXINFO . DEFLENGTH := 0 ;
         IXINFO . DEFSCALE := 0 ;
         IXINFO . DEFDIM := 0 ;
         IXINFO . SLINENR := SLINENR ;
         NAME_INTO_CACHE ( 'D' , IXNAME , IXINFO , 'X' ) ;
       end (* then *)
   end (* INDEX_INTO_CACHE *) ;



procedure EMPTY ;

   begin (* EMPTY *)
     WRITELN ( LISTING , SLINENR : 5 , ': ' , 'EMPTY   ' , SLINE ) ;
   end (* EMPTY *) ;



procedure COMMENT ;

   begin (* COMMENT *)
     WRITELN ( LISTING , SLINENR : 5 , ': ' , 'COMMENT ' , SLINE ) ;
   end (* COMMENT *) ;



procedure CONTROL ;

   begin (* CONTROL *)
     WRITELN ( LISTING , SLINENR : 5 , ': ' , 'CONTROL ' , SLINE ) ;
   end (* CONTROL *) ;



procedure PARSE_PARM ( const ST : STRING ; var SIX : INTEGER ; var
                     PARM_IX : INTEGER ) ;

   begin (* PARSE_PARM *)
     PARM_IX := 0 ;
     if SIX > LENGTH ( ST ) then
       return ;
     while SIX <= LENGTH ( ST ) do
       begin
         if ST [ SIX ] <> ' ' then
           break ;
         SIX := SIX + 1 ;
       end (* while *) ;
     if SIX > LENGTH ( ST ) then
       return ;
     PARM_IX := SIX ;
   end (* PARSE_PARM *) ;



procedure PARSE_NAMES ( const ST : STRING ; var SIX : INTEGER ; var
                      PARM_IX : INTEGER ; USEKIND : CHAR ) ;

//*******************************************
// parse a string starting from six          
// and enter all names found into the cache  
// with the given usekind                    
//*******************************************


   var KW : CHAR ( 10 ) ;
       KWIX : INTEGER ;
       WORD_FOUND : BOOLEAN ;
       INSTRING : BOOLEAN ;
       WORDINFO : NAMEINFO ;

   begin (* PARSE_NAMES *)
     PARM_IX := 0 ;
     repeat
       KW := ' ' ;
       KWIX := 0 ;
       if SIX > LENGTH ( ST ) then
         return ;
       while SIX <= LENGTH ( ST ) do
         begin
           if ST [ SIX ] <> ' ' then
             break ;
           SIX := SIX + 1 ;
         end (* while *) ;
       if SIX > LENGTH ( ST ) then
         return ;
       if PARM_IX = 0 then
         PARM_IX := SIX ;
       WORD_FOUND := TRUE ;
       INSTRING := FALSE ;
       while SIX <= LENGTH ( ST ) do
         begin
           if KWIX = 0 then
             begin
               if ST [ SIX ] = '''' then
                 begin
                   WORD_FOUND := FALSE ;
                   INSTRING := TRUE ;
                   SIX := SIX + 1 ;
                   break
                 end (* then *)
               else
                 if not ( ST [ SIX ] in [ 'A' .. 'Z' ] ) then
                   begin
                     WORD_FOUND := FALSE ;
                     SIX := SIX + 1 ;
                     break
                   end (* then *)
             end (* then *)
           else
             begin
               if not ( ST [ SIX ] in [ 'A' .. 'Z' , '_' , '0' .. '9' ]
               ) then
                 break
             end (* else *) ;
           KWIX := KWIX + 1 ;
           KW [ KWIX ] := ST [ SIX ] ;
           SIX := SIX + 1 ;
         end (* while *) ;
       if INSTRING then
         while SIX <= LENGTH ( ST ) do
           begin
             if ST [ SIX ] <> '''' then
               begin
                 SIX := SIX + 1 ;
                 continue ;
               end (* then *) ;
             INSTRING := FALSE ;
             SIX := SIX + 1 ;
             if SIX > LENGTH ( ST ) then
               break ;
             if ST [ SIX ] = '''' then
               begin
                 SIX := SIX + 1 ;
                 INSTRING := TRUE ;
                 continue ;
               end (* then *) ;
             break ;
           end (* while *) ;
       if WORD_FOUND then
         begin
           WORDINFO . DEFTYPE := ' ' ;
           WORDINFO . DEFLENGTH := 0 ;
           WORDINFO . DEFSCALE := 0 ;
           WORDINFO . DEFDIM := 0 ;
           WORDINFO . SLINENR := SLINENR ;
           NAME_INTO_CACHE ( 'D' , KW , WORDINFO , USEKIND ) ;
         end (* then *) ;
       if SIX > LENGTH ( ST ) then
         return ;
     until FALSE
   end (* PARSE_NAMES *) ;



procedure PARSE_ASSIGN ( const ST : STRING ; var SIX : INTEGER ) ;

   begin (* PARSE_ASSIGN *)
     if SIX > LENGTH ( ST ) then
       return ;
     while SIX <= LENGTH ( ST ) do
       begin
         if ST [ SIX ] <> ' ' then
           break ;
         SIX := SIX + 1 ;
       end (* while *) ;
     if SIX > LENGTH ( ST ) then
       ERROR ( 5 ) ;
     if ST [ SIX ] <> '=' then
       ERROR ( 5 ) ;
     SIX := SIX + 1 ;
   end (* PARSE_ASSIGN *) ;



procedure PARSE_COMPARE ( const ST : STRING ; var SIX : INTEGER ; var
                        COP : CHAR ( 2 ) ) ;

   begin (* PARSE_COMPARE *)
     COP := ' ' ;
     if SIX > LENGTH ( ST ) then
       return ;
     while SIX <= LENGTH ( ST ) do
       begin
         if ST [ SIX ] <> ' ' then
           break ;
         SIX := SIX + 1 ;
       end (* while *) ;
     if SIX > LENGTH ( ST ) then
       return ;
     COP [ 1 ] := ST [ SIX ] ;
     SIX := SIX + 1 ;
     if COP [ 1 ] in [ 'E' , 'G' , 'L' , 'N' ] then
       begin
         if SIX > LENGTH ( ST ) then
           return ;
         COP [ 2 ] := ST [ SIX ] ;
         SIX := SIX + 1 ;
       end (* then *) ;
     if SIX > LENGTH ( ST ) then
       return ;
     while SIX <= LENGTH ( ST ) do
       begin
         if ST [ SIX ] <> ' ' then
           break ;
         SIX := SIX + 1 ;
       end (* while *) ;
   end (* PARSE_COMPARE *) ;



procedure PARSE_DECIMAL ( const ST : STRING ; var SIX : INTEGER ; var W
                        : CHAR ( 30 ) ; var I1 : INTEGER ; var I2 :
                        INTEGER ) ;

   var WIX : INTEGER ;
       PIX : INTEGER ;

   begin (* PARSE_DECIMAL *)
     I1 := 0 ;
     I2 := 0 ;
     W := ' ' ;
     WIX := 0 ;
     if SIX > LENGTH ( ST ) then
       return ;
     while SIX <= LENGTH ( ST ) do
       begin
         if ST [ SIX ] <> ' ' then
           break ;
         SIX := SIX + 1 ;
       end (* while *) ;
     if SIX > LENGTH ( ST ) then
       return ;
     while SIX <= LENGTH ( ST ) do
       begin
         if not ( ST [ SIX ] in [ '0' .. '9' , '.' ] ) then
           break ;
         WIX := WIX + 1 ;
         W [ WIX ] := ST [ SIX ] ;
         SIX := SIX + 1 ;
       end (* while *) ;
     PIX := INDEX ( W , '.' ) ;
     if PIX = 0 then
       begin
         READSTR ( STR ( W ) , I1 ) ;
         I2 := 0
       end (* then *)
     else
       begin
         READSTR ( SUBSTR ( W , 1 , PIX - 1 ) , I1 ) ;
         READSTR ( SUBSTR ( W , PIX + 1 ) , I2 ) ;
       end (* else *) ;
     if SIX > LENGTH ( ST ) then
       return ;
     if ST [ SIX ] = ',' then
       SIX := SIX + 1 ;
   end (* PARSE_DECIMAL *) ;



procedure PARSE_POSITIVE ( const ST : STRING ; var SIX : INTEGER ; var
                         V : INTEGER ) ;

   var WIX : INTEGER ;
       W : CHAR ( 10 ) ;

   begin (* PARSE_POSITIVE *)
     V := 0 ;
     W := ' ' ;
     WIX := 0 ;
     if SIX > LENGTH ( ST ) then
       return ;
     while SIX <= LENGTH ( ST ) do
       begin
         if ST [ SIX ] <> ' ' then
           break ;
         SIX := SIX + 1 ;
       end (* while *) ;
     if SIX > LENGTH ( ST ) then
       return ;
     while SIX <= LENGTH ( ST ) do
       begin
         if not ( ST [ SIX ] in [ '0' .. '9' ] ) then
           break ;
         WIX := WIX + 1 ;
         W [ WIX ] := ST [ SIX ] ;
         SIX := SIX + 1 ;
       end (* while *) ;
     READSTR ( STR ( W ) , V ) ;
     if SIX > LENGTH ( ST ) then
       return ;
     if ST [ SIX ] = ',' then
       SIX := SIX + 1 ;
   end (* PARSE_POSITIVE *) ;



procedure PARSE_WORD_OR_NUMBER ( const ST : STRING ; var SIX : INTEGER
                               ; var W : CHAR ( 10 ) ) ;

   var WIX : INTEGER ;

   begin (* PARSE_WORD_OR_NUMBER *)
     W := ' ' ;
     WIX := 0 ;
     if SIX > LENGTH ( ST ) then
       return ;
     while SIX <= LENGTH ( ST ) do
       begin
         if ST [ SIX ] <> ' ' then
           break ;
         SIX := SIX + 1 ;
       end (* while *) ;
     if SIX > LENGTH ( ST ) then
       return ;
     while SIX <= LENGTH ( ST ) do
       begin
         if not ( ST [ SIX ] in [ 'A' .. 'Z' , '_' , '0' .. '9' ] )
         then
           break ;
         WIX := WIX + 1 ;
         W [ WIX ] := ST [ SIX ] ;
         SIX := SIX + 1 ;
       end (* while *) ;
   end (* PARSE_WORD_OR_NUMBER *) ;



procedure PARSE_WORD ( const ST : STRING ; var SIX : INTEGER ; var KW :
                     CHAR ( 10 ) ) ;

   var KWIX : INTEGER ;

   begin (* PARSE_WORD *)
     KW := ' ' ;
     KWIX := 0 ;
     if SIX > LENGTH ( ST ) then
       return ;
     while SIX <= LENGTH ( ST ) do
       begin
         if ST [ SIX ] <> ' ' then
           break ;
         SIX := SIX + 1 ;
       end (* while *) ;
     if SIX > LENGTH ( ST ) then
       return ;
     while SIX <= LENGTH ( ST ) do
       begin
         if KWIX = 0 then
           begin
             if not ( ST [ SIX ] in [ 'A' .. 'Z' ] ) then
               break
           end (* then *)
         else
           begin
             if not ( ST [ SIX ] in [ 'A' .. 'Z' , '_' , '0' .. '9' ] )
             then
               break
           end (* else *) ;
         KWIX := KWIX + 1 ;
         KW [ KWIX ] := ST [ SIX ] ;
         SIX := SIX + 1 ;
       end (* while *) ;
   end (* PARSE_WORD *) ;



procedure PARSE_TYPE ( const ST : STRING ; var SIX : INTEGER ; var TP :
                     CHAR ) ;

   var COMMA : INTEGER ;

   begin (* PARSE_TYPE *)
     TP := ' ' ;
     if SIX > LENGTH ( ST ) then
       return ;
     COMMA := 0 ;
     while SIX <= LENGTH ( ST ) do
       begin
         if ST [ SIX ] = ',' then
           begin
             COMMA := COMMA + 1 ;
             if COMMA > 1 then
               return
             else
               begin
                 SIX := SIX + 1 ;
                 continue
               end (* else *)
           end (* then *) ;
         if ST [ SIX ] <> ' ' then
           break ;
         SIX := SIX + 1 ;
       end (* while *) ;
     if SIX > LENGTH ( ST ) then
       return ;
     TP := ST [ SIX ] ;
     SIX := SIX + 1 ;
     while SIX <= LENGTH ( ST ) do
       begin
         if ST [ SIX ] <> ' ' then
           break ;
         SIX := SIX + 1 ;
       end (* while *) ;
   end (* PARSE_TYPE *) ;



procedure PARSE_FUNCTION ( const ST : STRING ; var SIX : INTEGER ; var
                         FSTR : STRING ) ;

//*******************************************************
// parses a function call                                
// variables inside function calls have to be parsed     
//*******************************************************


   var SIX_START : INTEGER ;
       L : INTEGER ;
       INSTRING : BOOLEAN ;
       FIX : INTEGER ;
       DUMMY_PARM_IX : INTEGER ;

   begin (* PARSE_FUNCTION *)
     FSTR := '' ;
     SIX_START := SIX ;
     if SIX > LENGTH ( ST ) then
       return ;
     while SIX <= LENGTH ( ST ) do
       begin
         if ST [ SIX ] = '(' then
           break ;
         SIX := SIX + 1 ;
       end (* while *) ;
     SIX := SIX + 1 ;
     FIX := SIX ;
     INSTRING := FALSE ;
     while SIX <= LENGTH ( ST ) do
       begin
         if ST [ SIX ] = '''' then
           INSTRING := not INSTRING ;
         if not INSTRING then
           if ST [ SIX ] = ')' then
             break ;
         SIX := SIX + 1 ;
       end (* while *) ;
     SIX := SIX + 1 ;
     L := SIX - SIX_START ;
     FSTR := SUBSTR ( ST , SIX_START , L ) ;
     FIX := FIX - SIX_START + 1 ;
     PARSE_NAMES ( FSTR , FIX , DUMMY_PARM_IX , 'F' ) ;
   end (* PARSE_FUNCTION *) ;



procedure PARSE_OPERAND ( const ST : STRING ; var SIX : INTEGER ; var
                        OPTYPE : CHAR ; var VARI : CHAR ( 10 ) ; var IX
                        : CHAR ( 10 ) ; var CSTR : STRING ) ;

//*******************************************************
// parses an operand                                     
// can be a variable and an optional index (optype = V)  
// or a constant string  (optype = N or S)               
//*******************************************************


   var SIX_START : INTEGER ;
       INSTRING : BOOLEAN ;
       L : INTEGER ;

   begin (* PARSE_OPERAND *)
     VARI := ' ' ;
     IX := ' ' ;
     CSTR := '' ;
     OPTYPE := ' ' ;
     if SIX > LENGTH ( ST ) then
       return ;
     while SIX <= LENGTH ( ST ) do
       begin
         if ST [ SIX ] <> ' ' then
           break ;
         SIX := SIX + 1 ;
       end (* while *) ;
     if SIX > LENGTH ( ST ) then
       return ;

     //********************************************
     // function call                              
     //********************************************

     if ST [ SIX ] = '&' then
       begin
         OPTYPE := 'F' ;
         PARSE_FUNCTION ( ST , SIX , CSTR )
       end (* then *)
     else

     //********************************************
     // variable - maybe with index                
     //********************************************

       if ST [ SIX ] in [ 'A' .. 'Z' ] then
         begin
           OPTYPE := 'V' ;
           PARSE_WORD ( ST , SIX , VARI ) ;
           if SIX > LENGTH ( ST ) then
             return ;
           while SIX <= LENGTH ( ST ) do
             begin
               if ST [ SIX ] <> ' ' then
                 break ;
               SIX := SIX + 1 ;
             end (* while *) ;
           if SIX > LENGTH ( ST ) then
             return ;
           if ST [ SIX ] <> '(' then
             return ;
           SIX := SIX + 1 ;
           while SIX <= LENGTH ( ST ) do
             begin
               if ST [ SIX ] <> ' ' then
                 break ;
               SIX := SIX + 1 ;
             end (* while *) ;
           if SIX > LENGTH ( ST ) then
             return ;
           PARSE_WORD_OR_NUMBER ( ST , SIX , IX ) ;
           while SIX <= LENGTH ( ST ) do
             begin
               if ST [ SIX ] <> ' ' then
                 break ;
               SIX := SIX + 1 ;
             end (* while *) ;
           if SIX > LENGTH ( ST ) then
             return ;
           if ST [ SIX ] <> ')' then
             return ;
           SIX := SIX + 1 ;
         end (* then *)
       else

     //*******************
     // numeric constant  
     //*******************

         if ST [ SIX ] in [ '0' .. '9' ] then
           begin
             OPTYPE := 'N' ;
             SIX_START := SIX ;
             SIX := SIX + 1 ;
             while SIX <= LENGTH ( ST ) do
               begin
                 if ST [ SIX ] in [ '0' .. '9' , '.' ] then
                   begin
                     SIX := SIX + 1 ;
                     continue ;
                   end (* then *) ;
                 break ;
               end (* while *) ;
             L := SIX - SIX_START ;
             CSTR := SUBSTR ( ST , SIX_START , L ) ;
           end (* then *)
         else

     //******************
     // string constant  
     //******************

           if ST [ SIX ] = '''' then
             begin
               OPTYPE := 'S' ;
               SIX_START := SIX ;
               SIX := SIX + 1 ;
               INSTRING := TRUE ;
               while SIX <= LENGTH ( ST ) do
                 begin
                   if ST [ SIX ] <> '''' then
                     begin
                       SIX := SIX + 1 ;
                       continue ;
                     end (* then *) ;
                   INSTRING := FALSE ;
                   SIX := SIX + 1 ;
                   if SIX > LENGTH ( ST ) then
                     break ;
                   if ST [ SIX ] = '''' then
                     begin
                       SIX := SIX + 1 ;
                       INSTRING := TRUE ;
                       continue ;
                     end (* then *) ;
                   break ;
                 end (* while *) ;
               if not INSTRING then
                 begin
                   L := SIX - SIX_START ;
                   CSTR := SUBSTR ( ST , SIX_START , L ) ;
                 end (* then *) ;
             end (* then *) ;
     while SIX <= LENGTH ( ST ) do
       begin
         if ST [ SIX ] <> ' ' then
           break ;
         SIX := SIX + 1 ;
       end (* while *) ;
     if SIX > LENGTH ( ST ) then
       return ;
     if ST [ SIX ] = ',' then
       SIX := SIX + 1 ;
   end (* PARSE_OPERAND *) ;



function CHECK_KW ( KW : CHAR ( 10 ) ) : INTEGER ;

   var I : INTEGER ;

   const KW_TAB : array [ 1 .. 21 ] of CHAR ( 10 ) =
         ( 'DEF' , 'DETAIL' , 'DO' , 'EDIT' , 'EDITF' , 'END' ,
           'EXECUTE' , 'EXIT' , 'FORMAT' , 'GOTO' , 'HEAD' , 'IF' ,
           'PRINT' , 'PRINTF' , 'REPORT' , 'SELECT' , 'SORT' , 'SUM' ,
           'TOTAL' , 'UPRO' , 'WRITE' ) ;

   begin (* CHECK_KW *)
     for I := 1 to 21 do
       if KW = KW_TAB [ I ] then
         begin
           CHECK_KW := I ;
           return
         end (* then *) ;
     CHECK_KW := 0 ;
   end (* CHECK_KW *) ;



procedure DO_DEF ;

//**********************************************************
// DEF Statement                                            
// Feldname und Attribute                                   
// werden in Liste uebernommen                              
//**********************************************************


   var NAME10 : CHAR ( 10 ) ;
       TP : CHAR ;
       WL : CHAR ( 30 ) ;
       L : INTEGER ;
       SC : INTEGER ;
       DIM : INTEGER ;
       DEFINFO : NAMEINFO ;

   begin (* DO_DEF *)
     PARSE_WORD ( ST , SIX , NAME10 ) ;
     PARSE_TYPE ( ST , SIX , TP ) ;
     PARSE_DECIMAL ( ST , SIX , WL , L , SC ) ;
     PARSE_POSITIVE ( ST , SIX , DIM ) ;
     DEFINFO . DEFTYPE := TP ;
     DEFINFO . DEFLENGTH := L ;
     DEFINFO . DEFSCALE := SC ;
     DEFINFO . DEFDIM := DIM ;
     DEFINFO . SLINENR := SLINENR ;
     NAME_INTO_CACHE ( 'D' , NAME10 , DEFINFO , 'D' ) ;
     if SC = 0 then
       WRITELN ( LISTING , SLINENR : 5 , ': ' , '........' , STLABEL ,
                 ' ' : NEST , 'DEF ' , NAME10 , ' ' , TP , ' ' , L : 3
                 , ' ' : 2 , DIM : 4 )
     else
       WRITELN ( LISTING , SLINENR : 5 , ': ' , '........' , STLABEL ,
                 ' ' : NEST , 'DEF ' , NAME10 , ' ' , TP , ' ' , L : 3
                 , '.' , SC : 1 , DIM : 4 ) ;
   end (* DO_DEF *) ;



procedure DO_DETAIL ;

   begin (* DO_DETAIL *)
     WRITELN ( LISTING , SLINENR : 5 , ': ' , 'DETAIL' : - 8 , ST ) ;
   end (* DO_DETAIL *) ;



procedure DO_DO ;

//**********************************************************
// DO Statement                                             
// Schleifenzaehler plus 3 Operanden                        
// von, bis und Schrittweite                                
//**********************************************************


   var OPTYPE : CHAR ;
       VARI : CHAR ( 10 ) ;
       IX : CHAR ( 10 ) ;
       CSTR : STRING ( 300 ) ;
       NAME10 : CHAR ( 10 ) ;
       OPER1 : STRING ( 300 ) ;
       OPER2 : STRING ( 300 ) ;
       OPER3 : STRING ( 300 ) ;
       LOOPINFO : NAMEINFO ;

   begin (* DO_DO *)
     OPER1 := '' ;
     OPER2 := '' ;
     OPER3 := '' ;

     //*****************************
     // Laufvariable                
     //*****************************

     PARSE_WORD ( ST , SIX , NAME10 ) ;
     LOOPINFO . DEFTYPE := ' ' ;
     LOOPINFO . DEFLENGTH := 0 ;
     LOOPINFO . DEFSCALE := 0 ;
     LOOPINFO . DEFDIM := 0 ;
     LOOPINFO . SLINENR := SLINENR ;
     NAME_INTO_CACHE ( 'D' , NAME10 , LOOPINFO , 'L' ) ;

     //*****************************
     // Zuweisungs-Operator         
     //*****************************

     PARSE_ASSIGN ( ST , SIX ) ;

     //*****************
     // Operand 1       
     //*****************

     PARSE_OPERAND ( ST , SIX , OPTYPE , VARI , IX , CSTR ) ;
     case OPTYPE of
       'S' , 'F' :
         ERROR ( 2 ) ;
       'N' : OPER1 := CSTR ;
       otherwise
         begin
           OPER1 := TRIM ( VARI ) ;
           LOOPINFO . DEFTYPE := ' ' ;
           LOOPINFO . DEFLENGTH := 0 ;
           LOOPINFO . DEFSCALE := 0 ;
           LOOPINFO . DEFDIM := 0 ;
           LOOPINFO . SLINENR := SLINENR ;
           NAME_INTO_CACHE ( 'D' , VARI , LOOPINFO , 'E' ) ;
           if IX <> ' ' then
             begin
               OPER1 := OPER1 || '(' || TRIM ( IX ) || ')' ;
               INDEX_INTO_CACHE ( IX )
             end (* then *) ;
         end (* otherw *)
     end (* case *) ;

     //*****************
     // Operand 2       
     //*****************

     PARSE_OPERAND ( ST , SIX , OPTYPE , VARI , IX , CSTR ) ;
     case OPTYPE of
       'S' , 'F' :
         ERROR ( 3 ) ;
       'N' : OPER2 := CSTR ;
       otherwise
         begin
           OPER2 := TRIM ( VARI ) ;
           LOOPINFO . DEFTYPE := ' ' ;
           LOOPINFO . DEFLENGTH := 0 ;
           LOOPINFO . DEFSCALE := 0 ;
           LOOPINFO . DEFDIM := 0 ;
           LOOPINFO . SLINENR := SLINENR ;
           NAME_INTO_CACHE ( 'D' , VARI , LOOPINFO , 'E' ) ;
           if IX <> ' ' then
             begin
               OPER2 := OPER2 || '(' || TRIM ( IX ) || ')' ;
               INDEX_INTO_CACHE ( IX )
             end (* then *) ;
         end (* otherw *)
     end (* case *) ;

     //*****************
     // Operand 3       
     //*****************

     PARSE_OPERAND ( ST , SIX , OPTYPE , VARI , IX , CSTR ) ;
     case OPTYPE of
       'S' , 'F' :
         ERROR ( 4 ) ;
       'N' : OPER3 := CSTR ;
       otherwise
         begin
           OPER3 := TRIM ( VARI ) ;
           LOOPINFO . DEFTYPE := ' ' ;
           LOOPINFO . DEFLENGTH := 0 ;
           LOOPINFO . DEFSCALE := 0 ;
           LOOPINFO . DEFDIM := 0 ;
           LOOPINFO . SLINENR := SLINENR ;
           NAME_INTO_CACHE ( 'D' , VARI , LOOPINFO , 'E' ) ;
           if IX <> ' ' then
             begin
               OPER3 := OPER3 || '(' || TRIM ( IX ) || ')' ;
               INDEX_INTO_CACHE ( IX )
             end (* then *) ;
         end (* otherw *)
     end (* case *) ;
     WRITE ( LISTING , SLINENR : 5 , ': ' , '........' , STLABEL , ' '
             : NEST , 'DO ' , RTRIM ( NAME10 ) , ' = ' , OPER1 , ',' ,
             OPER2 ) ;
     if OPER3 <> '' then
       WRITELN ( LISTING , ',' , OPER3 )
     else
       WRITELN ( LISTING ) ;
     NEST := NEST + 3 ;
   end (* DO_DO *) ;



procedure DO_EDIT ;

   begin (* DO_EDIT *)
     WRITELN ( LISTING , SLINENR : 5 , ': ' , 'EDIT' : - 8 , ST ) ;
   end (* DO_EDIT *) ;



procedure DO_EDITF ;

   begin (* DO_EDITF *)
     WRITELN ( LISTING , SLINENR : 5 , ': ' , 'EDITF' : - 8 , ST ) ;
   end (* DO_EDITF *) ;



procedure DO_END ;

//**********************************************************
// keine weiteren Parameter                                 
//**********************************************************


   begin (* DO_END *)
     NEST := NEST - 3 ;
     WRITELN ( LISTING , SLINENR : 5 , ': ' , '........' , STLABEL ,
               ' ' : NEST , 'END ' ) ;
   end (* DO_END *) ;



procedure DO_EXECUTE ;

   begin (* DO_EXECUTE *)
     WRITELN ( LISTING , SLINENR : 5 , ': ' , 'EXECUTE' : - 8 , ST ) ;
   end (* DO_EXECUTE *) ;



procedure DO_EXIT ;

//**********************************************************
// keine weiteren Parameter                                 
//**********************************************************


   begin (* DO_EXIT *)
     WRITELN ( LISTING , SLINENR : 5 , ': ' , '........' , STLABEL ,
               ' ' : NEST , 'EXIT ' ) ;
   end (* DO_EXIT *) ;



procedure DO_FORMAT ;

   begin (* DO_FORMAT *)
     WRITELN ( LISTING , SLINENR : 5 , ': ' , 'FORMAT' : - 8 , ST ) ;
   end (* DO_FORMAT *) ;



procedure DO_GOTO ;

//**********************************************************
// GOTO Statement                                           
// Name (Sprungziel)                                        
// wird in Liste uebernommen                                
//**********************************************************


   var NAME10 : CHAR ( 10 ) ;
       GOTOINFO : NAMEINFO ;

   begin (* DO_GOTO *)
     PARSE_WORD ( ST , SIX , NAME10 ) ;
     NAME10 [ 9 ] := ' ' ;
     NAME10 [ 10 ] := ' ' ;
     GOTOINFO . DEFTYPE := ' ' ;
     GOTOINFO . DEFLENGTH := 0 ;
     GOTOINFO . DEFSCALE := 0 ;
     GOTOINFO . DEFDIM := 0 ;
     GOTOINFO . SLINENR := SLINENR ;
     NAME_INTO_CACHE ( 'L' , NAME10 , GOTOINFO , 'G' ) ;
     WRITELN ( LISTING , SLINENR : 5 , ': ' , '........' , STLABEL ,
               ' ' : NEST , 'GOTO ' , NAME10 ) ;
   end (* DO_GOTO *) ;



procedure DO_HEAD ;

   begin (* DO_HEAD *)
     WRITELN ( LISTING , SLINENR : 5 , ': ' , 'HEAD' : - 8 , ST ) ;
   end (* DO_HEAD *) ;



procedure DO_IF ;

//**********************************************************
// IF Statement                                             
// zwei Operanden (Variable plus Index oder Konstante)      
// Vergleichsoperator                                       
// GOTO plus Sprungziel                                     
//**********************************************************


   var OPTYPE : CHAR ;
       VARI : CHAR ( 10 ) ;
       IX : CHAR ( 10 ) ;
       CSTR : STRING ( 300 ) ;
       COMPOP : CHAR ( 2 ) ;
       NAME10 : CHAR ( 10 ) ;
       OPLEFT : STRING ( 300 ) ;
       OPRIGHT : STRING ( 300 ) ;
       IFINFO : NAMEINFO ;

   begin (* DO_IF *)

     //*****************
     // linker Operand  
     //*****************

     PARSE_OPERAND ( ST , SIX , OPTYPE , VARI , IX , CSTR ) ;
     case OPTYPE of
       'S' , 'N' , 'F' :
         OPLEFT := CSTR ;
       otherwise
         begin
           OPLEFT := TRIM ( VARI ) ;
           IFINFO . DEFTYPE := ' ' ;
           IFINFO . DEFLENGTH := 0 ;
           IFINFO . DEFSCALE := 0 ;
           IFINFO . DEFDIM := 0 ;
           IFINFO . SLINENR := SLINENR ;
           NAME_INTO_CACHE ( 'D' , VARI , IFINFO , 'I' ) ;
           if IX <> ' ' then
             begin
               OPLEFT := OPLEFT || '(' || TRIM ( IX ) || ')' ;
               INDEX_INTO_CACHE ( IX )
             end (* then *) ;
         end (* otherw *)
     end (* case *) ;

     //*****************
     // Vergleichs-Op   
     //*****************

     PARSE_COMPARE ( ST , SIX , COMPOP ) ;

     //*****************
     // rechter Operand 
     //*****************

     PARSE_OPERAND ( ST , SIX , OPTYPE , VARI , IX , CSTR ) ;
     case OPTYPE of
       'S' , 'N' , 'F' :
         OPRIGHT := CSTR ;
       otherwise
         begin
           OPRIGHT := TRIM ( VARI ) ;
           IFINFO . DEFTYPE := ' ' ;
           IFINFO . DEFLENGTH := 0 ;
           IFINFO . DEFSCALE := 0 ;
           IFINFO . DEFDIM := 0 ;
           IFINFO . SLINENR := SLINENR ;
           NAME_INTO_CACHE ( 'D' , VARI , IFINFO , 'I' ) ;
           if IX <> ' ' then
             begin
               OPRIGHT := OPRIGHT || '(' || TRIM ( IX ) || ')' ;
               INDEX_INTO_CACHE ( IX )
             end (* then *) ;
         end (* otherw *)
     end (* case *) ;

     //*****************
     // muss goto sein  
     //*****************

     PARSE_WORD ( ST , SIX , NAME10 ) ;

     //*****************
     // Sprungziel      
     //*****************

     PARSE_WORD ( ST , SIX , NAME10 ) ;
     NAME10 [ 9 ] := ' ' ;
     NAME10 [ 10 ] := ' ' ;
     IFINFO . DEFTYPE := ' ' ;
     IFINFO . DEFLENGTH := 0 ;
     IFINFO . DEFSCALE := 0 ;
     IFINFO . DEFDIM := 0 ;
     IFINFO . SLINENR := SLINENR ;
     NAME_INTO_CACHE ( 'L' , NAME10 , IFINFO , 'I' ) ;
     WRITELN ( LISTING , SLINENR : 5 , ': ' , '........' , STLABEL ,
               ' ' : NEST , 'IF ' , OPLEFT , ' ' , RTRIM ( COMPOP ) ,
               ' ' , OPRIGHT , ' GOTO ' , NAME10 ) ;
   end (* DO_IF *) ;



procedure DO_PRINT ;

   begin (* DO_PRINT *)
     WRITELN ( LISTING , SLINENR : 5 , ': ' , 'PRINT' : - 8 , ST ) ;
   end (* DO_PRINT *) ;



procedure DO_PRINTF ;

   begin (* DO_PRINTF *)
     WRITELN ( LISTING , SLINENR : 5 , ': ' , 'PRINTF' : - 8 , ST ) ;
   end (* DO_PRINTF *) ;



procedure DO_REPORT ;

//**********************************************************
// REPORT Statement                                         
// Parameter wird derzeit nicht weiter untersucht           
//**********************************************************


   var PARM_IX : INTEGER ;

   begin (* DO_REPORT *)
     PARSE_PARM ( ST , SIX , PARM_IX ) ;
     WRITELN ( LISTING , SLINENR : 5 , ': ' , '........' , STLABEL ,
               ' ' : NEST , 'REPORT ' , SUBSTR ( ST , PARM_IX ) ) ;
   end (* DO_REPORT *) ;



procedure DO_SELECT ;

   begin (* DO_SELECT *)
     WRITELN ( LISTING , SLINENR : 5 , ': ' , 'SELECT' : - 8 , ST ) ;
   end (* DO_SELECT *) ;



procedure DO_SORT ;

//**********************************************************
// SORT Statement                                           
// Felder in Parameter nach Feldliste                       
//**********************************************************


   var PARM_IX : INTEGER ;

   begin (* DO_SORT *)
     PARSE_NAMES ( ST , SIX , PARM_IX , 'S' ) ;
     WRITELN ( LISTING , SLINENR : 5 , ': ' , '........' , STLABEL ,
               ' ' : NEST , 'SORT ' , SUBSTR ( ST , PARM_IX ) ) ;
   end (* DO_SORT *) ;



procedure DO_SUM ;

   begin (* DO_SUM *)
     WRITELN ( LISTING , SLINENR : 5 , ': ' , 'SUM' : - 8 , ST ) ;
   end (* DO_SUM *) ;



procedure DO_TOTAL ;

   begin (* DO_TOTAL *)
     WRITELN ( LISTING , SLINENR : 5 , ': ' , 'TOTAL' : - 8 , ST ) ;
   end (* DO_TOTAL *) ;



procedure DO_UPRO ;

   begin (* DO_UPRO *)
     WRITELN ( LISTING , SLINENR : 5 , ': ' , 'UPRO' : - 8 , ST ) ;
   end (* DO_UPRO *) ;



procedure DO_WRITE ;

//**********************************************************
// WRITE Statement                                          
// Felder in Parameter nach Feldliste                       
//**********************************************************


   var PARM_IX : INTEGER ;

   begin (* DO_WRITE *)
     PARSE_NAMES ( ST , SIX , PARM_IX , 'W' ) ;
     WRITELN ( LISTING , SLINENR : 5 , ': ' , '........' , STLABEL ,
               ' ' : NEST , 'WRITE ' , SUBSTR ( ST , PARM_IX ) ) ;
   end (* DO_WRITE *) ;



procedure DO_ASSIGNMENT ;

//**********************************************************
// Assignment                                               
// links Variable                                           
// rechts zwei Operanden und Operatorzeichen                
// oder auch nur ein Operand                                
//**********************************************************


   var OPTYPE : CHAR ;
       VARI : CHAR ( 10 ) ;
       IX : CHAR ( 10 ) ;
       CSTR : STRING ( 300 ) ;
       COMPOP : CHAR ( 2 ) ;
       OPLEFT : STRING ( 300 ) ;
       OPRIGHT : STRING ( 300 ) ;
       OPRIGHT2 : STRING ( 300 ) ;
       ASGNINFO : NAMEINFO ;

   begin (* DO_ASSIGNMENT *)

     //********************************
     // parse from beginning           
     //********************************

     SIX := 1 ;

     //*****************
     // linker Operand  
     //*****************

     PARSE_OPERAND ( ST , SIX , OPTYPE , VARI , IX , CSTR ) ;
     case OPTYPE of
       'S' , 'N' , 'F' :
         ERROR ( 1 ) ;
       otherwise
         begin
           OPLEFT := TRIM ( VARI ) ;
           ASGNINFO . DEFTYPE := ' ' ;
           ASGNINFO . DEFLENGTH := 0 ;
           ASGNINFO . DEFSCALE := 0 ;
           ASGNINFO . DEFDIM := 0 ;
           ASGNINFO . SLINENR := SLINENR ;
           NAME_INTO_CACHE ( 'D' , VARI , ASGNINFO , 'A' ) ;
           if IX <> ' ' then
             begin
               OPLEFT := OPLEFT || '(' || TRIM ( IX ) || ')' ;
               INDEX_INTO_CACHE ( IX )
             end (* then *) ;
         end (* otherw *)
     end (* case *) ;

     //*****************************
     // Zuweisungs-Operator         
     //*****************************

     PARSE_ASSIGN ( ST , SIX ) ;

     //**********************
     // rechter Operand 1    
     //**********************

     PARSE_OPERAND ( ST , SIX , OPTYPE , VARI , IX , CSTR ) ;
     case OPTYPE of
       'S' , 'N' , 'F' :
         OPRIGHT := CSTR ;
       otherwise
         begin
           OPRIGHT := TRIM ( VARI ) ;
           ASGNINFO . DEFTYPE := ' ' ;
           ASGNINFO . DEFLENGTH := 0 ;
           ASGNINFO . DEFSCALE := 0 ;
           ASGNINFO . DEFDIM := 0 ;
           ASGNINFO . SLINENR := SLINENR ;
           NAME_INTO_CACHE ( 'D' , VARI , ASGNINFO , 'E' ) ;
           if IX <> ' ' then
             begin
               OPRIGHT := OPRIGHT || '(' || TRIM ( IX ) || ')' ;
               INDEX_INTO_CACHE ( IX )
             end (* then *) ;
         end (* otherw *)
     end (* case *) ;

     //*****************************
     // aritmetischer Operator      
     //*****************************

     PARSE_COMPARE ( ST , SIX , COMPOP ) ;

     //**********************
     // rechter Operand 2    
     //**********************

     PARSE_OPERAND ( ST , SIX , OPTYPE , VARI , IX , CSTR ) ;
     case OPTYPE of
       'S' , 'N' , 'F' :
         OPRIGHT2 := CSTR ;
       otherwise
         begin
           OPRIGHT2 := TRIM ( VARI ) ;
           ASGNINFO . DEFTYPE := ' ' ;
           ASGNINFO . DEFLENGTH := 0 ;
           ASGNINFO . DEFSCALE := 0 ;
           ASGNINFO . DEFDIM := 0 ;
           ASGNINFO . SLINENR := SLINENR ;
           NAME_INTO_CACHE ( 'D' , VARI , ASGNINFO , 'E' ) ;
           if IX <> ' ' then
             begin
               OPRIGHT2 := OPRIGHT2 || '(' || TRIM ( IX ) || ')' ;
               INDEX_INTO_CACHE ( IX )
             end (* then *) ;
         end (* otherw *)
     end (* case *) ;
     WRITELN ( LISTING , SLINENR : 5 , ': ' , '........' , STLABEL ,
               ' ' : NEST , OPLEFT , ' = ' , OPRIGHT , ' ' , RTRIM (
               COMPOP ) , ' ' , OPRIGHT2 ) ;
   end (* DO_ASSIGNMENT *) ;



procedure PARSE ( const ST : STRING ) ;

   var KW : CHAR ( 10 ) ;
       KWIX : INTEGER ;

   begin (* PARSE *)
     SIX := 1 ;
     PARSE_WORD ( ST , SIX , KW ) ;
     KWIX := CHECK_KW ( KW ) ;
     case KWIX of
       1 : DO_DEF ;
       2 : DO_DETAIL ;
       3 : DO_DO ;
       4 : DO_EDIT ;
       5 : DO_EDITF ;
       6 : DO_END ;
       7 : DO_EXECUTE ;
       8 : DO_EXIT ;
       9 : DO_FORMAT ;
       10 : DO_GOTO ;
       11 : DO_HEAD ;
       12 : DO_IF ;
       13 : DO_PRINT ;
       14 : DO_PRINTF ;
       15 : DO_REPORT ;
       16 : DO_SELECT ;
       17 : DO_SORT ;
       18 : DO_SUM ;
       19 : DO_TOTAL ;
       20 : DO_UPRO ;
       21 : DO_WRITE ;
       otherwise
         DO_ASSIGNMENT
     end (* case *) ;
   end (* PARSE *) ;



procedure STATEMENT ;

   var STMT : CHAR ( 72 ) ;
       IX : INTEGER ;
       LABELINFO : NAMEINFO ;

   begin (* STATEMENT *)

     //**********************************************************
     // folgezeilen gekennzeichnet durch zeichen ungleich blank  
     // in spalte 72                                             
     // sammeln statement komplett in variable st (1000 lang)    
     //**********************************************************

     STMT := SLINE ;
     ST := '' ;
     while STMT [ 72 ] <> ' ' do
       begin
         ST := ST || LEFT ( STMT , 71 ) ;
         if FALSE then
           WRITELN ( LISTING , SLINENR : 5 , ': ' , 'STMT-P  ' , STMT )
                     ;
         READLN ( SOURCE , SLINE ) ;
         SLINENR := SLINENR + 1 ;
       end (* while *) ;
     ST := ST || RTRIM ( STMT ) ;
     if FALSE then
       WRITELN ( LISTING , SLINENR : 5 , ': ' , 'STMT    ' , STMT ) ;

     //*****************************************************
     // wenn label in spalte 1, extrahieren                 
     // andernfalls ist label blank                         
     // label ist lt. beschreibung auf 8 Zeichen limitiert  
     //*****************************************************

     if ST [ 1 ] <> ' ' then
       begin
         IX := INDEX ( ST , ' ' ) ;
         if IX > 9 then
           STLABEL := LEFT ( ST , 8 )
         else
           STLABEL := LEFT ( ST , IX - 1 ) ;
         ST := SUBSTR ( ST , IX + 1 ) ;

     //**********************************************************
     // Name (Sprungziel)                                        
     // in Liste uebernehmen                                     
     //**********************************************************

         LABELINFO . DEFTYPE := 'L' ;
         LABELINFO . DEFLENGTH := 0 ;
         LABELINFO . DEFSCALE := 0 ;
         LABELINFO . DEFDIM := 0 ;
         LABELINFO . SLINENR := SLINENR ;
         NAME_INTO_CACHE ( 'L' , STLABEL , LABELINFO , 'D' ) ;
       end (* then *)
     else
       begin
         STLABEL := ' ' ;
       end (* else *) ;

     //**********************************************
     // uebrig bleiben stlabel (evtl. blank) und st  
     // st auf beiden seiten getrimmt                
     // st muss jetzt geparst werden                 
     //**********************************************

     ST := LTRIM ( ST ) ;
     PARSE ( ST ) ;
   end (* STATEMENT *) ;



procedure NAMENSLISTE ;

   var SEQKEY : VOIDPTR ;
       PKEY : VOIDPTR ;
       LKEY : INTEGER ;
       PDAT : VOIDPTR ;
       LDAT : INTEGER ;
       NKEY : -> NAMEKEY ;
       NINFO : -> NAMEINFO ;
       RC : INTEGER ;

   begin (* NAMENSLISTE *)
     PKEY := NIL ;
     LKEY := 0 ;
     PDAT := NIL ;
     LDAT := 0 ;
     SEQKEY := NIL ;
     RC := AVLCACHE ( 'GFIRST' , PNAMCACHE , SEQKEY , PKEY , LKEY ,
           PDAT , LDAT ) ;
     while RC = 0 do
       begin
         NKEY := PKEY ;
         NINFO := PDAT ;
         WRITE ( LISTING , NKEY -> . NAMECLASS , ' ' , NKEY -> . NAME )
                 ;
         WRITE ( LISTING , ' - ' ) ;
         WRITE ( LISTING , NINFO -> . DEFTYPE ) ;
         WRITE ( LISTING , NINFO -> . DEFLENGTH : 5 ) ;
         WRITE ( LISTING , NINFO -> . DEFSCALE : 3 ) ;
         WRITE ( LISTING , NINFO -> . DEFDIM : 5 ) ;
         WRITE ( LISTING , NINFO -> . SLINENR : 7 ) ;
         WRITELN ( LISTING ) ;
         RC := AVLCACHE ( 'GNEXT' , PNAMCACHE , SEQKEY , PKEY , LKEY ,
               PDAT , LDAT ) ;
       end (* while *) ;
   end (* NAMENSLISTE *) ;



procedure USAGELISTE ;

   var SEQKEY : VOIDPTR ;
       PKEY : VOIDPTR ;
       LKEY : INTEGER ;
       PDAT : VOIDPTR ;
       LDAT : INTEGER ;
       UKEY : -> USEKEY ;
       UINFO : -> USEINFO ;
       RC : INTEGER ;

   begin (* USAGELISTE *)
     PKEY := NIL ;
     LKEY := 0 ;
     PDAT := NIL ;
     LDAT := 0 ;
     SEQKEY := NIL ;
     RC := AVLCACHE ( 'GFIRST' , PUSECACHE , SEQKEY , PKEY , LKEY ,
           PDAT , LDAT ) ;
     while RC = 0 do
       begin
         UKEY := PKEY ;
         UINFO := PDAT ;
         WRITE ( LISTING , UKEY -> . NAMECLASS , ' ' , UKEY -> . NAME ,
                 UKEY -> . USE_SLINENR : 7 , UKEY -> . USE_SEQNO : 3 )
                 ;
         WRITE ( LISTING , ' - ' ) ;
         WRITE ( LISTING , UINFO -> . USEKIND ) ;
         WRITELN ( LISTING ) ;
         RC := AVLCACHE ( 'GNEXT' , PUSECACHE , SEQKEY , PKEY , LKEY ,
               PDAT , LDAT ) ;
       end (* while *) ;
   end (* USAGELISTE *) ;



procedure REFERENZLISTE ;

   var SEQKEY : VOIDPTR ;
       PKEY : VOIDPTR ;
       LKEY : INTEGER ;
       PDAT : VOIDPTR ;
       LDAT : INTEGER ;
       NKEY : -> NAMEKEY ;
       NINFO : -> NAMEINFO ;
       RC : INTEGER ;
       OLDCLASS : CHAR ;


   procedure REF_VERWENDUNG ( CL : CHAR ; NAME : CHAR ( 10 ) ) ;

      var SEQKEY : VOIDPTR ;
          PKEY : VOIDPTR ;
          LKEY : INTEGER ;
          PDAT : VOIDPTR ;
          LDAT : INTEGER ;
          UKEY : USEKEY ;
          RKEY : -> USEKEY ;
          UINFO : -> USEINFO ;
          RC : INTEGER ;
          IPOS : INTEGER ;

      begin (* REF_VERWENDUNG *)
        WRITE ( LISTING , ' ' : 2 ) ;
        IPOS := 0 ;
        UKEY . NAMECLASS := CL ;
        UKEY . NAME := NAME ;
        UKEY . USE_SLINENR := ITOS ( 0 , 5 ) ;
        UKEY . USE_SEQNO := ITOS ( 0 , 3 ) ;
        PKEY := ADDR ( UKEY ) ;
        LKEY := SIZEOF ( UKEY ) ;
        PDAT := NIL ;
        LDAT := 0 ;
        SEQKEY := NIL ;
        WRITELN ( 'test: keys before start = ' , CL , ' ' , NAME ) ;
        RC := AVLCACHE ( 'START ' , PUSECACHE , SEQKEY , PKEY , LKEY ,
              PDAT , LDAT ) ;
        WRITELN ( 'test: rc after start = ' , RC ) ;
        WRITELN ( 'test: seqkey after start = ' , SEQKEY ) ;
        RC := AVLCACHE ( 'GFIRST' , PUSECACHE , SEQKEY , PKEY , LKEY ,
              PDAT , LDAT ) ;
        while RC = 0 do
          begin
            RKEY := PKEY ;
            UINFO := PDAT ;
            if RKEY -> . NAMECLASS > CL then
              break ;
            if RKEY -> . NAMECLASS = CL then
              begin
                if RKEY -> . NAME > NAME then
                  break ;
                if RKEY -> . NAME = NAME then
                  begin
                    IPOS := IPOS + 1 ;
                    case IPOS of
                      1 : if UINFO -> . USEKIND <> 'D' then
                            begin
                              WRITE ( LISTING , ' ' : 8 ) ;
                              IPOS := 2 ;
                            end (* then *) ;
                      2 : WRITE ( LISTING , ' ' : 2 ) ;
                      otherwise
                        if OLDCLASS = 'D' then
                          begin
                            if ( IPOS - 1 ) MOD 6 = 1 then
                              begin
                                WRITELN ( LISTING ) ;
                                WRITE ( LISTING , ' ' : 42 )
                              end (* then *)
                          end (* then *)
                        else
                          begin
                            if ( IPOS - 1 ) MOD 8 = 1 then
                              begin
                                WRITELN ( LISTING ) ;
                                WRITE ( LISTING , ' ' : 26 )
                              end (* then *)
                          end (* else *) ;
                    end (* case *) ;
                    WRITE ( LISTING , RKEY -> . USE_SLINENR : 5 , UINFO
                            -> . USEKIND ) ;
                  end (* then *)
              end (* then *) ;
            RC := AVLCACHE ( 'GNEXT' , PUSECACHE , SEQKEY , PKEY , LKEY
                  , PDAT , LDAT ) ;
          end (* while *)
      end (* REF_VERWENDUNG *) ;


   begin (* REFERENZLISTE *)
     OLDCLASS := ' ' ;
     PKEY := NIL ;
     LKEY := 0 ;
     PDAT := NIL ;
     LDAT := 0 ;
     SEQKEY := NIL ;
     RC := AVLCACHE ( 'GFIRST' , PNAMCACHE , SEQKEY , PKEY , LKEY ,
           PDAT , LDAT ) ;
     while RC = 0 do
       begin
         NKEY := PKEY ;
         NINFO := PDAT ;
         if OLDCLASS <> NKEY -> . NAMECLASS then
           begin
             OLDCLASS := NKEY -> . NAMECLASS ;
             case OLDCLASS of
               'D' : begin
                       WRITELN ( LISTING ,
                               'Liste der Variablen (RRW und ACCURAT)'
                                 ) ;
                       WRITELN ( LISTING ) ;
                       WRITE ( LISTING ,
                               'Name        Typ Laenge Nachk Dim  ' ,
                               ' Defin  Verwendung' ) ;
                       WRITELN ( LISTING ) ;
                     end (* tag/ca *) ;
               'L' : begin
                       WRITELN ( LISTING ) ;
                       WRITELN ( LISTING ,
                                 'Liste der Label (Sprungziele)' ) ;
                       WRITELN ( LISTING ) ;
                       WRITE ( LISTING , 'Name        Typ   ' ,
                               ' Defin  Verwendung' ) ;
                       WRITELN ( LISTING ) ;
                     end (* tag/ca *) ;
             end (* case *)
           end (* then *) ;
         WRITE ( LISTING , NKEY -> . NAME , ' ' : 2 ) ;
         WRITE ( LISTING , NINFO -> . DEFTYPE ) ;
         if NINFO -> . DEFTYPE = 'L' then
           WRITE ( LISTING , ' ' : 3 )
         else
           if NINFO -> . DEFTYPE = ' ' then
             WRITE ( LISTING , '-------------------' )
           else
             begin
               WRITE ( LISTING , NINFO -> . DEFLENGTH : 9 ) ;
               WRITE ( LISTING , NINFO -> . DEFSCALE : 6 ) ;
               WRITE ( LISTING , NINFO -> . DEFDIM : 4 ) ;
             end (* else *) ;
         REF_VERWENDUNG ( NKEY -> . NAMECLASS , NKEY -> . NAME ) ;
         WRITELN ( LISTING ) ;
         RC := AVLCACHE ( 'GNEXT' , PNAMCACHE , SEQKEY , PKEY , LKEY ,
               PDAT , LDAT ) ;
       end (* while *) ;
   end (* REFERENZLISTE *) ;



begin (* HAUPTPROGRAMM *)
  RESET ( SOURCE ) ;
  REWRITE ( LISTING ) ;
  WRITELN ( LISTING ) ;
  WRITELN ( LISTING , '==========================================' ) ;
  WRITELN ( LISTING , 'Protokoll des eingelesenen Reports' ) ;
  WRITELN ( LISTING , '==========================================' ) ;
  WRITELN ( LISTING ) ;
  SLINENR := 0 ;
  PNAMCACHE := NIL ;
  PUSECACHE := NIL ;
  NEST := 1 ;
  while not EOF ( SOURCE ) do
    begin
      READLN ( SOURCE , SLINE ) ;
      SLINENR := SLINENR + 1 ;
      if LENGTH ( SLINE ) < 1 then
        EMPTY
      else
        if LEFT ( SLINE , 1 ) = '*' then
          COMMENT
        else
          if LEFT ( SLINE , 2 ) = '..' then
            CONTROL
          else
            STATEMENT ;
    end (* while *) ;

  //***************************
  // Ausgabe des Namenscaches  
  //***************************

  if FALSE then
    begin
      WRITELN ( LISTING ) ;
      WRITELN ( LISTING , '=========================================='
                ) ;
      WRITELN ( LISTING , 'Ausgabe der Liste der Namen' ) ;
      WRITELN ( LISTING , '=========================================='
                ) ;
      WRITELN ( LISTING ) ;
      NAMENSLISTE ;
    end (* then *) ;

  //********************************
  // Ausgabe des Verwendungscaches  
  //********************************

  if FALSE then
    begin
      WRITELN ( LISTING ) ;
      WRITELN ( LISTING , '=========================================='
                ) ;
      WRITELN ( LISTING , 'Ausgabe Verwendungsnachweis' ) ;
      WRITELN ( LISTING , '=========================================='
                ) ;
      WRITELN ( LISTING ) ;
      USAGELISTE ;
    end (* then *) ;

  //***************************************************************
  // Ausgabe einer Referenzliste (Namen plus Verwendungsnachweis)  
  //***************************************************************

  WRITELN ( LISTING ) ;
  WRITELN ( LISTING , '==========================================' ) ;
  WRITELN ( LISTING , 'Ausgabe Referenzliste' ) ;
  WRITELN ( LISTING , '==========================================' ) ;
  WRITELN ( LISTING ) ;
  REFERENZLISTE ;
end (* HAUPTPROGRAMM *) .
