program COBFORM ( SOURCE , NSOURCE , LISTING ) ;

//***************************************************************
//                                                               
// Source Code Formatter for COBOL                               
// Bernd Oppolzer - work started 02.2020                         
//                                                               
//***************************************************************
// to do list:                                                   
// - write metadata for files to new output file                 
// - output remarks 1:6 depending on (external) switch - no const
// - compute data offsets depending on (external) switch         
// - control if level numbers are inconsistent                   
// - control if END-IF terminates IF with missing END-EVALUATE   
// - better formatting of level items (01, 05, 10 etc.)          
// - insert THEN keyword on multi-line IFs                       
//***************************************************************
// solved problems:                                              
// * skip line on statement starters (MOVE, NEXT SENTENCE)       
// * solve problems with string constant including WRITE         
// * correct formatting of multi-level IFs with ELSE             
// * proper stacking of statement types                          
// * solve problem with breakdown on LEICP82A                    
// * solve problem with wrong indentation on OPEN                
// * do major on keywords                                        
// * print errors to listing                                     
// * modify output on IF conditions                              
// * test: control problems with missing END-EVALUATE            
// * correct indentation on EVALUATE statement                   
// * correct indentation on PERFORM statement                    
// * remove blank lines after PERFORM                            
// * statements INITIALIZE, INSPECT.                             
// * correct indentation on READ statement                       
// * correct indentation on WRITE statement                      
// * check_outputline should do different on multi line output   
// * better indentation on SEARCH statement                      
// * skip lines at AND and OR inside WHEN clauses                
// * better indentation on EXEC CICS statements                  
//***************************************************************



const MAXLEVEL = 20 ;
      WIDTH_ZONE_B = 61 ;
      DATAPOS1 = 38 ;
      DATAPOS2 = 52 ;
      REMARKS_1_6 = TRUE ;
      TRACELVL = 0 ;


type STATUS_TYPE = ( INITST , VORCOBOL , INCOBOL , NACHCOBOL ) ;
     CWORD = record
               ID : STRING ( 15 ) ;
               CLASS : CHAR ;
               TAGS : STRING ( 8 )
             end ;
     GLOBAL_STATUS = record
                       ICNT_PRE : INTEGER ;
                       ICNT_COB : INTEGER ;
                       ICNT_POST : INTEGER ;
                       OCNT_PRE : INTEGER ;
                       OCNT_COB : INTEGER ;
                       OCNT_POST : INTEGER ;

     //********************************************
     // div_flag and sect_flag are set according   
     // to the division and section actually       
     // processed                                  
     //********************************************

                       DIV_FLAG : CHAR ;
                       SECT_FLAG : CHAR ;

     //********************************************
     // variable punkt_gef is set after work_level 
     // call and tells if next statement which     
     // starts with a number is to be handled as   
     // a new level statement or not               
     //********************************************

                       PUNKT_GEF : BOOLEAN ;

     //********************************************
     // variables from here handled by check_word  
     // inside procedure work_normal_stmt          
     //********************************************

                       INDENT : INTEGER ;
                       INDENT_INCR : INTEGER ;
                       INDENT_ZERO : BOOLEAN ;
                       INDENT_EJECT : BOOLEAN ;
                       STMT_TYPE : CHAR ;
                       STMT_SPECIAL : CHAR ;
                       STMT_LEVEL : INTEGER ;
                       STMT_STATUS : INTEGER ;
                       STMT_TEMP : STRING ( 5000 ) ;
                       ZKOMM1 : CHAR ( 6 ) ;
                       NEW_STATEMENT : CHAR ;
                       NR_SYMB_IN_STMT : INTEGER ;
                       STMT_STACK : array [ 1 .. MAXLEVEL ] of CHAR ;
                       STMT_INDENT : array [ 1 .. MAXLEVEL ] of INTEGER
                                     ;
                       THEN_FOUND : BOOLEAN ;
                     end ;


var SOURCE , NSOURCE , LISTING : TEXT ;
    LINENO : INTEGER ;
    ZEILE : STRING ( 100 ) ;
    STATUS : STATUS_TYPE ;
    OLDSTATUS : STATUS_TYPE ;
    LEVEL_TAB : array [ 1 .. MAXLEVEL ] of INTEGER ;
    ACTLEVEL : INTEGER ;
    I : INTEGER ;
    GS : GLOBAL_STATUS ;
    ERRNO : INTEGER ;


const CWANZ = 60 ;
      CWTAB : array [ 1 .. CWANZ ] of CWORD =
      (                                             //
        ( 'ADD            ' , 'C' , '      ' ) ,    //
        ( 'AFTER          ' , 'P' , '      ' ) ,    //
        ( 'AT             ' , 'X' , '      ' ) ,    //
        ( 'CLOSE          ' , 'C' , '      ' ) ,    //
        ( 'COMPUTE        ' , 'C' , '      ' ) ,    //
        ( 'DELETE         ' , 'C' , 'X     ' ) ,    //
        ( 'DISPLAY        ' , 'C' , '      ' ) ,    //
        ( 'DIVIDE         ' , 'C' , '      ' ) ,    //
        ( 'EJECT          ' , 'C' , '      ' ) ,    //
        ( 'ELSE           ' , 'C' , '      ' ) ,    //
        ( 'END-EVALUATE   ' , 'C' , '      ' ) ,    //
        ( 'END-EXEC       ' , 'C' , '      ' ) ,    //
        ( 'END-IF         ' , 'C' , '      ' ) ,    //
        ( 'END-PERFORM    ' , 'C' , '      ' ) ,    //
        ( 'END-READ       ' , 'C' , '      ' ) ,    //
        ( 'END-SEARCH     ' , 'C' , '      ' ) ,    //
        ( 'END-WRITE      ' , 'C' , '      ' ) ,    //
        ( 'EVALUATE       ' , 'C' , 'E     ' ) ,    //
        ( 'EXEC           ' , 'C' , ' X    ' ) ,    //
        ( 'GO             ' , 'C' , '      ' ) ,    //
        ( 'IF             ' , 'C' , 'II    ' ) ,    //
        ( 'INITIALIZE     ' , 'C' , '      ' ) ,    //
        ( 'INSPECT        ' , 'C' , '      ' ) ,    //
        ( 'INVALID        ' , 'X' , '      ' ) ,    //
        ( 'MOVE           ' , 'C' , '      ' ) ,    //
        ( 'MULTIPLY       ' , 'C' , '      ' ) ,    //
        ( 'NEXT           ' , 'C' , '  R   ' ) ,    //
        ( 'OPEN           ' , 'C' , '      ' ) ,    //
        ( 'PERFORM        ' , 'C' , 'PP    ' ) ,    //
        ( 'READ           ' , 'C' , 'XR    ' ) ,    //
        ( 'REWRITE        ' , 'C' , 'X     ' ) ,    //
        ( 'SEARCH         ' , 'C' , ' S    ' ) ,    //
        ( 'SET            ' , 'C' , '      ' ) ,    //
        ( 'START          ' , 'C' , 'X     ' ) ,    //
        ( 'STOP           ' , 'C' , '      ' ) ,    //
        ( 'SUBTRACT       ' , 'C' , '      ' ) ,    //
        ( 'UNTIL          ' , 'P' , '      ' ) ,    //
        ( 'VARYING        ' , 'P' , '      ' ) ,    //
        ( 'WHEN           ' , 'C' , ' 1    ' ) ,    //
        ( 'WRITE          ' , 'C' , 'XW    ' ) ,    //
        ( '               ' , 'C' , '      ' ) ,    //
        ( '               ' , 'C' , '      ' ) ,    //
        ( '               ' , 'C' , '      ' ) ,    //
        ( '               ' , 'C' , '      ' ) ,    //
        ( '               ' , 'C' , '      ' ) ,    //
        ( '               ' , 'C' , '      ' ) ,    //
        ( '               ' , 'C' , '      ' ) ,    //
        ( '               ' , 'C' , '      ' ) ,    //
        ( '               ' , 'C' , '      ' ) ,    //
        ( '               ' , 'C' , '      ' ) ,    //
        ( '               ' , 'C' , '      ' ) ,    //
        ( '               ' , 'C' , '      ' ) ,    //
        ( '               ' , 'C' , '      ' ) ,    //
        ( '               ' , 'C' , '      ' ) ,    //
        ( '               ' , 'C' , '      ' ) ,    //
        ( '               ' , 'C' , '      ' ) ,    //
        ( '               ' , 'C' , '      ' ) ,    //
        ( '               ' , 'C' , '      ' ) ,    //
        ( '               ' , 'C' , '      ' ) ,    //
        ( '               ' , 'C' , '      ' )      //
        ) ;



function UPPER ( C : CHAR ) : CHAR ;

   begin (* UPPER *)
     if C in [ 'a' .. 'i' , 'j' .. 'r' , 's' .. 'z' ] then
       C := CHR ( ORD ( C ) - ORD ( 'a' ) + ORD ( 'A' ) ) ;
     UPPER := C
   end (* UPPER *) ;



procedure S_UPPER ( var S : STRING ) ;

   begin (* S_UPPER *)
     for I := 1 to LENGTH ( S ) do
       S [ I ] := UPPER ( S [ I ] ) ;
   end (* S_UPPER *) ;



procedure INTTOSTR ( CP : VOIDPTR ; LEN : INTEGER ; VAL : INTEGER ;
                   ZEROES : BOOLEAN ) ;

   var BUFFER : array [ 1 .. 20 ] of CHAR ;
       MINUS : BOOLEAN ;
       LETZT : INTEGER ;
       I : INTEGER ;
       LIMIT : INTEGER ;
       LENX : INTEGER ;
       POSX : INTEGER ;

   begin (* INTTOSTR *)
     if VAL < 0 then
       begin
         VAL := - VAL ;
         MINUS := TRUE
       end (* then *)
     else
       MINUS := FALSE ;
     I := 20 ;
     BUFFER := ' ' ;
     while VAL > 0 do
       begin
         LETZT := VAL MOD 10 ;
         BUFFER [ I ] := CHR ( ORD ( '0' ) + LETZT ) ;
         I := I - 1 ;
         VAL := VAL DIV 10 ;
       end (* while *) ;
     LIMIT := 20 - LEN ;
     if ZEROES then
       while I > LIMIT do
         begin
           BUFFER [ I ] := '0' ;
           I := I - 1 ;
         end (* while *) ;
     if MINUS then
       begin
         if ZEROES then
           I := I + 1 ;
         BUFFER [ I ] := '-' ;
         I := I - 1 ;
       end (* then *) ;
     LENX := 20 - I ;
     POSX := LEN - LENX ;
     if POSX < 0 then
       begin
         I := I - POSX ;
         POSX := 0 ;
       end (* then *) ;
     MEMSET ( CP , ' ' , LEN ) ;
     MEMCPY ( PTRADD ( CP , POSX ) , ADDR ( BUFFER [ I + 1 ] ) , LENX )
              ;
   end (* INTTOSTR *) ;



procedure ERR_CHECK ( ERRNO : INTEGER ) ;

   var ERRS : STRING ( 80 ) ;

   begin (* ERR_CHECK *)
     if ERRNO = 0 then
       return ;
     case ERRNO of
       1 : ERRS := '+++ Cobol Word in Area A' ;
       2 : ERRS := '+++ SECTION expected' ;
       3 : ERRS := '+++ no ELSE expected here' ;
       4 : ERRS := '+++ no END-IF expected here' ;
       5 : ERRS := '+++ no END-EVALUATE expected here' ;
       6 : ERRS := '+++ no WHEN expected here (no EVALUATE)' ;
       7 : ERRS := '+++ no END-PERFORM expected here' ;
       8 : ERRS := '+++ no END-READ expected here' ;
       9 : ERRS := '+++ no END-WRITE expected here' ;
       10 : ERRS := '+++ no END-SEARCH expected here' ;
       otherwise
         begin
           ERRS := '+++ unknown error code xxxx' ;
           INTTOSTR ( ADDR ( ERRS [ 24 ] ) , 4 , ERRNO , TRUE ) ;
         end (* otherw *)
     end (* case *) ;
     WRITELN ( LISTING , ERRS ) ;
     WRITELN ( ERRS ) ;
   end (* ERR_CHECK *) ;



function COMP_COBOL ( const Z : STRING ) : STRING ;

//***************************************************
// compress cobol lines, that is:                    
// remove multiple blanks but not inside of strings  
// and: apply upper to chars outside of strings      
//***************************************************


   var SNEU : STRING ( 80 ) ;
       SNEU2 : STRING ( 80 ) ;
       IX : INTEGER ;
       STATUS : INTEGER ;
       I : INTEGER ;
       DO_WRITE : BOOLEAN ;
       X : CHAR ;

   begin (* COMP_COBOL *)
     SNEU := Z ;
     SNEU2 := REPEATSTR ( ' ' , 80 ) ;
     IX := 0 ;
     STATUS := 1 ;
     for I := 1 to LENGTH ( SNEU ) do
       begin
         DO_WRITE := TRUE ;
         X := SNEU [ I ] ;
         case X of
           ' ' : case STATUS of
                   1 : begin
                         STATUS := 1 ;
                         DO_WRITE := FALSE
                       end (* tag/ca *) ;
                   2 : STATUS := 2 ;
                   3 : STATUS := 1 ;
                   4 : STATUS := 1 ;
                 end (* case *) ;
           '''' : case STATUS of
                    1 : STATUS := 2 ;
                    2 : STATUS := 4 ;
                    3 : STATUS := 2 ;
                    4 : STATUS := 2 ;
                  end (* case *) ;
           otherwise
             case STATUS of
               1 : STATUS := 3 ;
               2 : STATUS := 2 ;
               3 : STATUS := 3 ;
               4 : STATUS := 3 ;
             end (* case *) ;
         end (* case *) ;
         if DO_WRITE then
           begin
             IX := IX + 1 ;
             if STATUS = 2 then
               SNEU2 [ IX ] := X
             else
               if X = '.' then
                 if GS . DIV_FLAG = 'P' then
                   begin
                     SNEU2 [ IX ] := ' ' ;
                     IX := IX + 1 ;
                     SNEU2 [ IX ] := '.' ;
                     IX := IX + 1 ;
                     SNEU2 [ IX ] := ' ' ;
                   end (* then *)
                 else
                   SNEU2 [ IX ] := '.'
               else
                 SNEU2 [ IX ] := UPPER ( X )
           end (* then *)
       end (* for *) ;
     SNEU := SUBSTR ( SNEU2 , 1 , IX ) ;
     COMP_COBOL := RTRIM ( SNEU )
   end (* COMP_COBOL *) ;



procedure WORK_PROCESS ( var ZCNEU : CHAR ( 72 ) ; ZC : CHAR ( 72 ) ) ;

//**************************
// Bearbeite Process-Zeile  
//**************************


   begin (* WORK_PROCESS *)
     ZCNEU := COMPRESS ( ZC ) ;
   end (* WORK_PROCESS *) ;



function NEXT_BLANK ( const S : STRING ; X : INTEGER ) : INTEGER ;

//**********************************************************
// search next blank in string s, starting from position x  
// return zero if no more blank found                       
//**********************************************************


   var INSTRING : BOOLEAN ;
       DELIM : CHAR ;

   begin (* NEXT_BLANK *)
     if X > 0 then
       begin
         INSTRING := FALSE ;
         while X <= LENGTH ( S ) do
           if not INSTRING then
             case S [ X ] of
               ' ' : begin
                       NEXT_BLANK := X ;
                       return
                     end (* tag/ca *) ;
               '''' : begin
                        INSTRING := TRUE ;
                        DELIM := '''' ;
                        X := X + 1 ;
                      end (* tag/ca *) ;
               '"' : begin
                       INSTRING := TRUE ;
                       DELIM := '"' ;
                       X := X + 1 ;
                     end (* tag/ca *) ;
               otherwise
                 X := X + 1
             end (* case *)
           else
             begin
               if S [ X ] = DELIM then
                 INSTRING := FALSE ;
               X := X + 1
             end (* else *)
       end (* then *) ;
     NEXT_BLANK := 0 ;
     return
   end (* NEXT_BLANK *) ;



function NEXT_NONBLANK ( const S : STRING ; X : INTEGER ) : INTEGER ;

//**************************************************************
// search next non-blank in string s, starting from position x  
// return zero if no more non-blank character found             
//**************************************************************


   begin (* NEXT_NONBLANK *)
     if X > 0 then
       while X <= LENGTH ( S ) do
         if S [ X ] <> ' ' then
           begin
             NEXT_NONBLANK := X ;
             return
           end (* then *)
         else
           X := X + 1 ;
     NEXT_NONBLANK := 0 ;
     return
   end (* NEXT_NONBLANK *) ;



procedure PARSE_DATA ( const COBSTMT : STRING ; var POS0 : INTEGER ;
                     var POS1 : INTEGER ; var POS2 : INTEGER ) ;

//*************************************************
// look for first attribute after variable (pos0)  
// return attribute position in pos1               
// and look, if there is a VALUE attribute         
// return that in pos2                             
//*************************************************


   var KVALUE : CHAR ( 6 ) ;
       FERTIG : BOOLEAN ;
       X : INTEGER ;

   begin (* PARSE_DATA *)
     KVALUE := 'VALUE ' ;
     POS0 := 0 ;
     POS1 := 0 ;
     POS2 := 0 ;
     X := INDEX ( COBSTMT , ' ' ) ;
     X := NEXT_NONBLANK ( COBSTMT , X ) ;
     POS0 := X ;
     if X = 0 then
       return ;
     X := NEXT_BLANK ( COBSTMT , X ) ;
     X := NEXT_NONBLANK ( COBSTMT , X ) ;
     POS1 := X ;
     repeat
       X := NEXT_BLANK ( COBSTMT , X ) ;
       X := NEXT_NONBLANK ( COBSTMT , X ) ;
       if X = 0 then
         FERTIG := TRUE
       else
         begin
           if X < LENGTH ( COBSTMT ) - 6 then
             if MEMCMP ( ADDR ( COBSTMT [ X ] ) , ADDR ( KVALUE ) , 6 )
             = 0 then
               begin
                 POS2 := X ;
                 FERTIG := TRUE
               end (* then *)
         end (* else *)
     until FERTIG ;
   end (* PARSE_DATA *) ;



function BLANKS_LEVEL ( X : INTEGER ; ZONE : CHAR ) : STRING ;

//**********************************************************
// generate blanks depending on level                       
// x = computed nesting level                               
// zone = zone a or b                                       
// if x > 1 and zone a, additional indentation needed       
//**********************************************************


   var N : STRING ( 80 ) ;

   begin (* BLANKS_LEVEL *)
     N := '' ;
     while X > 2 do
       begin
         N := N || '    ' ;
         X := X - 1
       end (* while *) ;
     if X > 1 then
       if ZONE = 'A' then
         N := N || '    ' ;
     BLANKS_LEVEL := N
   end (* BLANKS_LEVEL *) ;



function NUMMER2 ( X : INTEGER ) : STRING ;

//*******************************
// level number with two digits  
//*******************************


   var N : STRING ( 2 ) ;

   begin (* NUMMER2 *)
     N := '00' ;
     N [ 1 ] := CHR ( ORD ( '0' ) + X DIV 10 ) ;
     N [ 2 ] := CHR ( ORD ( '0' ) + X MOD 10 ) ;
     NUMMER2 := N ;
   end (* NUMMER2 *) ;



procedure WORK_LEVEL ( const COBSTMT : STRING ; var ZLEVEL : INTEGER ;
                     var POS0 : INTEGER ; var POS1 : INTEGER ; var POS2
                     : INTEGER ; var PUNKT : BOOLEAN ) ;

//*********************************************
// parse data definition                       
// starting with level number                  
// and update information in vector level_tab  
//*********************************************


   var LEVEL : INTEGER ;
       S : STRING ( 70 ) ;

   begin (* WORK_LEVEL *)
     if ACTLEVEL = 0 then
       begin
         LEVEL_TAB [ 1 ] := 1 ;
         ACTLEVEL := 1 ;
       end (* then *) ;
     S := RTRIM ( COBSTMT ) ;
     PUNKT := RIGHT ( S , 1 ) = '.' ;
     if FALSE then
       WRITELN ( GS . ZKOMM1 , S ) ;

     //**************************************************************
     // ersetzt durch readstr:                                       
     // LEVEL := ORD ( S [ 1 ] ) - ORD ( '0' ) ;                     
     // LEVEL := LEVEL * 10 + ORD ( S [ 2 ] ) - ORD ( '0' ) ;        
     //**************************************************************

     READSTR ( S , LEVEL ) ;
     ZLEVEL := LEVEL ;

     //********************************
     // parse data definition          
     //********************************

     PARSE_DATA ( S , POS0 , POS1 , POS2 ) ;
     if FALSE then
       WRITELN ( '*pos   ' , POS0 , POS1 , POS2 ) ;

     //***************
     // sonderfaelle  
     //***************

     if LEVEL = 77 then
       begin
         ACTLEVEL := 1 ;
         return
       end (* then *) ;
     if LEVEL = 88 then
       return ;
     if FALSE then
       WRITELN ( '*level ' , LEVEL , ACTLEVEL , LEVEL_TAB [ ACTLEVEL ]
                 ) ;

     //**********************************
     // neues level = bestehendes level  
     //**********************************

     if LEVEL_TAB [ ACTLEVEL ] = LEVEL then
       return ;

     //*******************************************
     // neues level ist groesser als bestehendes  
     //*******************************************

     if LEVEL_TAB [ ACTLEVEL ] < LEVEL then
       begin
         ACTLEVEL := ACTLEVEL + 1 ;
         LEVEL_TAB [ ACTLEVEL ] := LEVEL ;
         return
       end (* then *) ;

     //************************************************
     // neues level ist kleiner als bestehendes        
     // 02.03.2020:                                    
     // koennte aber sein, dass das neue level         
     // noch gar nicht da war und nur ein bisheriges   
     // umgetauft wird. Beispiel:                      
     //   01 UPRO200-DATA.                             
     //      05  UPRO200-OUTPUT.                       
     //        15 UPRO200-O-SATZ       PIC X(31900).   
     //        15 UPRO200-O-SATZ-L     PIC 9(7).       
     //        15 UPRO200-O-REST       PIC X(31900).   
     //        15 UPRO200-O-REST-L     PIC 9(7).       
     //        10 UPRO200-O-RCOD       PIC 9(2).       
     //        10 UPRO200-O-RTEX       PIC X(50).      
     // sieht richtig sch... aus, gibt's aber          
     //************************************************

     while LEVEL_TAB [ ACTLEVEL ] > LEVEL do
       ACTLEVEL := ACTLEVEL - 1 ;
     if LEVEL_TAB [ ACTLEVEL ] < LEVEL then
       ACTLEVEL := ACTLEVEL + 1 ;
     LEVEL_TAB [ ACTLEVEL ] := LEVEL ;
   end (* WORK_LEVEL *) ;



procedure WORK_FD ( const COBSTMT : STRING ) ;

//**************************************
// Bearbeite FD (nichts weiter zu tun)  
//**************************************


   begin (* WORK_FD *)
     ACTLEVEL := 0 ;
     if FALSE then
       WRITELN ( GS . ZKOMM1 , COBSTMT ) ;
   end (* WORK_FD *) ;



procedure WORK_DATA_SECTION ( const COBSTMT : STRING ) ;

//********************************************
// Bearbeite SECTION innerhalb DATA DIVISION  
// nur Flag setzen                            
//********************************************


   begin (* WORK_DATA_SECTION *)
     if FALSE then
       WRITELN ( GS . ZKOMM1 , COBSTMT ) ;
     if INDEX ( COBSTMT , 'FILE ' ) <> 0 then
       GS . SECT_FLAG := 'F'
     else
       if INDEX ( COBSTMT , 'WORKING-STORAGE ' ) <> 0 then
         GS . SECT_FLAG := 'W'
       else
         if INDEX ( COBSTMT , 'LINKAGE ' ) <> 0 then
           GS . SECT_FLAG := 'L'
         else
           GS . SECT_FLAG := 'U'
   end (* WORK_DATA_SECTION *) ;



procedure CHECK_WORD_1 ( var WORD : STRING ; var COBWORD : BOOLEAN ;
                       var PUNKT : BOOLEAN ; var ICW : INTEGER ;
                       SPECIAL : CHAR ) ;

//**********************************************************
// checks for period and statement starters                 
//**********************************************************


   var LWORD : INTEGER ;
       ICW_TEMP : INTEGER ;

   begin (* CHECK_WORD_1 *)
     ICW := 0 ;
     COBWORD := FALSE ;
     PUNKT := FALSE ;
     if WORD = '' then
       return ;

     //********************************
     // punkt kommt einzeln :-)        
     //********************************

     if WORD = '.' then
       begin
         PUNKT := TRUE ;
         return ;
       end (* then *) ;

     //********************************
     // punkt haengt hinten dran,      
     // sollte nicht mehr vorkommen    
     //********************************

     LWORD := LENGTH ( WORD ) ;
     if RIGHT ( WORD , 1 ) = '.' then
       begin
         if LWORD > 1 then
           WORD := LEFT ( WORD , LWORD - 1 )
         else
           WORD := '' ;
         PUNKT := TRUE ;
       end (* then *) ;

     //***********************************************
     // as long as inside EXEC CICS (special = 'X')   
     // only END-EXEC starts a new statement          
     //***********************************************

     if SPECIAL = 'X' then
       if WORD <> STR ( 'END-EXEC' ) then
         return ;

     //***********************************************
     // check for cobol words = statement starters    
     //***********************************************
     // the word is not a statement starter if it is  
     // encountered inside of a special statement     
     // mentioned in tags [3] ... e.g. read ... next  
     //***********************************************

     for ICW_TEMP := 1 to CWANZ do
       begin
         if CWTAB [ ICW_TEMP ] . ID = ' ' then
           break ;
         if ( RTRIM ( CWTAB [ ICW_TEMP ] . ID ) = WORD ) and ( CWTAB [
         ICW_TEMP ] . CLASS = 'C' ) then
           if ( SPECIAL = ' ' ) or ( CWTAB [ ICW_TEMP ] . TAGS [ 3 ] <>
           SPECIAL ) then
             begin
               COBWORD := TRUE ;
               ICW := ICW_TEMP ;
               break
             end (* then *)
       end (* for *)
   end (* CHECK_WORD_1 *) ;



procedure WORK_PARAGRAPH_SECT ( const COBSTMT : STRING ) ;

   var WORD : STRING ( 70 ) ;
       FEHLER : BOOLEAN ;
       X1 : INTEGER ;
       X : INTEGER ;
       COBWORD : BOOLEAN ;
       PUNKT : BOOLEAN ;
       ICW : INTEGER ;

   begin (* WORK_PARAGRAPH_SECT *)
     if TRACELVL >= 1 then
       begin
         WRITELN ( 'start work_paragraph_sect' ) ;
         WRITELN ( 'cobstmt = <' , COBSTMT , '>' ) ;
       end (* then *) ;
     FEHLER := FALSE ;
     X1 := 1 ;
     while TRUE do
       begin
         X := NEXT_BLANK ( COBSTMT , X1 ) ;
         if X = 0 then
           WORD := SUBSTR ( COBSTMT , X1 )
         else
           WORD := SUBSTR ( COBSTMT , X1 , X - X1 ) ;
         if TRACELVL >= 1 then
           begin
             WRITELN ( 'word = <' , WORD , '>' ) ;
           end (* then *) ;

     //****************************************************
     // check1 checks for period and statement starters    
     // if statement starters (cobword) and not first      
     // in line, then the first part of the line is        
     // written, and the rest is processed                 
     //****************************************************

         CHECK_WORD_1 ( WORD , COBWORD , PUNKT , ICW , ' ' ) ;
         if COBWORD then
           begin
             FEHLER := TRUE ;
             ERRNO := 1 ;
             break
           end (* then *) ;
         if X = 0 then
           break ;
         X1 := X + 1 ;
         X := NEXT_BLANK ( COBSTMT , X1 ) ;
         if X = 0 then
           WORD := SUBSTR ( COBSTMT , X1 )
         else
           WORD := SUBSTR ( COBSTMT , X1 , X - X1 ) ;
         CHECK_WORD_1 ( WORD , COBWORD , PUNKT , ICW , ' ' ) ;
         if PUNKT then
           break ;
         if WORD <> 'SECTION' then
           begin
             FEHLER := TRUE ;
             ERRNO := 2
           end (* then *) ;
         break
       end (* while *) ;

     //***************************************************
     // indent is always zero, when new paragraph starts  
     //***************************************************

     GS . INDENT := 0 ;
     GS . STMT_LEVEL := 0 ;
     if TRACELVL >= 1 then
       begin
         WRITELN ( 'end work_paragraph_sect' ) ;
       end (* then *) ;
   end (* WORK_PARAGRAPH_SECT *) ;



procedure OUT_SRCLINE ( OUTF : TEXT ; ZEILE : CHAR ( 72 ) ) ;

   var S : STRING ( 72 ) ;
       X : STRING ( 6 ) ;

   begin (* OUT_SRCLINE *)
     S := RTRIM ( ZEILE ) ;
     if TRACELVL >= 1 then
       begin
         WRITELN ( 'start out_srcline' ) ;
         WRITELN ( 'zeile = <' , S , '>' ) ;
       end (* then *) ;
     if REMARKS_1_6 then
       WRITELN ( OUTF , S )
     else
       begin
         if LENGTH ( S ) > 6 then
           begin
             X := SUBSTR ( S , 1 , 6 ) ;
             if ( X = ' PROCE' ) or ( X = 'ANFANG' ) or ( X = 'ENDE  '
             ) or ( LEFT ( X , 5 ) = '-INC ' ) or ( LEFT ( X , 5 ) =
             '+INC ' ) then
               WRITELN ( OUTF , S )
             else
               WRITELN ( OUTF , ' ' : 6 , SUBSTR ( S , 7 ) ) ;
           end (* then *)
         else
           WRITELN ( OUTF )
       end (* else *) ;
     GS . OCNT_COB := GS . OCNT_COB + 1 ;
     if TRACELVL >= 1 then
       begin
         WRITELN ( 'ende out_srcline' ) ;
       end (* then *) ;
   end (* OUT_SRCLINE *) ;



procedure DO_OUTPUT ( var OUTF : TEXT ; ZCNEU : CHAR ( 72 ) ; ZCNEU2 :
                    CHAR ( 72 ) ; BL : INTEGER ; INDENT_EJECT : BOOLEAN
                    ) ;

   var I : INTEGER ;

   begin (* DO_OUTPUT *)
     if TRACELVL >= 1 then
       begin
         WRITELN ( 'start do_output' ) ;
         WRITELN ( 'zcneu = <' , ZCNEU , '>' ) ;
         WRITELN ( 'zcneu2 = <' , ZCNEU2 , '>' ) ;
       end (* then *) ;
     if INDENT_EJECT then
       BL := BL + 4 ;
     for I := 1 to BL do
       WRITELN ( OUTF ) ;
     GS . OCNT_COB := GS . OCNT_COB + BL ;
     if TRACELVL >= 1 then
       begin
         WRITELN ( 'indent = ' , GS . INDENT : 1 ) ;
         WRITE ( 'level = ' , GS . STMT_LEVEL : 1 ) ;
         for I := 1 to GS . STMT_LEVEL do
           WRITE ( GS . STMT_STACK [ I ] : 2 ) ;
         WRITELN ;
       end (* then *) ;
     if not INDENT_EJECT then
       begin
         OUT_SRCLINE ( OUTF , ZCNEU ) ;
         if ZCNEU2 <> ' ' then
           OUT_SRCLINE ( OUTF , ZCNEU2 ) ;
       end (* then *) ;
     if TRACELVL >= 1 then
       begin
         WRITE ( 'level = ' , GS . STMT_LEVEL : 1 ) ;
         for I := 1 to GS . STMT_LEVEL do
           WRITE ( GS . STMT_STACK [ I ] : 2 ) ;
         WRITELN ;
       end (* then *) ;
     if TRACELVL >= 1 then
       begin
         WRITELN ( 'ende do_output' ) ;
       end (* then *) ;
   end (* DO_OUTPUT *) ;



function SUCHE_TRENNSTELLE ( const COBSTMT : STRING ;        //
                           MAXTRENN : INTEGER ) :            //
                           INTEGER ;                         //

   var CH : CHAR ;
       TRENN : INTEGER ;
       INSTRING : BOOLEAN ;

   begin (* SUCHE_TRENNSTELLE *)
     I := 1 ;
     TRENN := MAXTRENN ;
     INSTRING := FALSE ;
     while I < MAXTRENN do
       begin
         CH := COBSTMT [ I ] ;
         if CH = '''' then
           INSTRING := not INSTRING ;
         if not INSTRING and ( CH = ' ' ) then
           TRENN := I ;
         I := I + 1 ;
       end (* while *) ;
     SUCHE_TRENNSTELLE := TRENN ;
   end (* SUCHE_TRENNSTELLE *) ;



procedure CHECK_OUTPUTLINE ( ZONE : CHAR ;                   //
                           const COBSTMT : STRING ;          //
                           INDENT : INTEGER ;                //
                           var ZCNEU : CHAR ( 72 ) ;         //
                           var ZCNEU2 : CHAR ( 72 ) ) ;      //

   var TEIL1 : STRING ( 70 ) ;
       TEIL2 : STRING ( 70 ) ;
       X : INTEGER ;
       MAXL : INTEGER ;
       INDENT_PLUS : INTEGER ;

   begin (* CHECK_OUTPUTLINE *)
     if ZONE = 'A' then
       begin
         MAXL := 65 ;
         INDENT_PLUS := 1 ;
       end (* then *)
     else
       begin
         MAXL := 61 ;
         INDENT_PLUS := 5 ;
       end (* else *) ;
     ZCNEU2 := '' ;
     if TRACELVL >= 1 then
       begin
         WRITELN ( 'start check_outputline' ) ;
         WRITELN ( 'cobstmt          = <' , COBSTMT , '>' ) ;
         WRITELN ( 'length (cobstmt) = ' , LENGTH ( COBSTMT ) ) ;
         WRITELN ( 'indent           = ' , INDENT ) ;
         WRITELN ( 'zone             = ' , ZONE ) ;
         WRITELN ( 'indent_plus      = ' , INDENT_PLUS ) ;
       end (* then *) ;

     //**************************
     // platz reicht fuer zeile  
     //**************************

     if LENGTH ( COBSTMT ) + INDENT <= MAXL then
       ZCNEU := GS . ZKOMM1 || REPEATSTR ( ' ' , INDENT + INDENT_PLUS )
                || COBSTMT

     //****************************************************************
     // platz reicht nicht                                             
     // dann unterschiedliche logik, je nach proc division oder nicht  
     //****************************************************************

     else
       if GS . DIV_FLAG = 'P' then
         begin
           X := SUCHE_TRENNSTELLE ( COBSTMT , MAXL - INDENT - 1 ) ;
           if TRACELVL >= 1 then
             WRITELN ( 'trennstelle pruefen !!!' ) ;
           TEIL1 := SUBSTR ( COBSTMT , 1 , X - 1 ) ;
           TEIL2 := SUBSTR ( COBSTMT , X + 1 ) ;
           ZCNEU := GS . ZKOMM1 || REPEATSTR ( ' ' , INDENT +
                    INDENT_PLUS ) || TEIL1 ;
           ZCNEU2 := GS . ZKOMM1 || REPEATSTR ( ' ' , INDENT +
                     INDENT_PLUS ) || TEIL2
         end (* then *)
       else
         begin
           X := INDEX ( COBSTMT , ' VALUE ' ) ;
           if X = 0 then
             X := 31 ;
           TEIL1 := SUBSTR ( COBSTMT , 1 , X - 1 ) ;
           TEIL2 := SUBSTR ( COBSTMT , X + 1 ) ;
           ZCNEU := GS . ZKOMM1 || REPEATSTR ( ' ' , INDENT +
                    INDENT_PLUS ) || TEIL1 ;
           ZCNEU2 := GS . ZKOMM1 || REPEATSTR ( ' ' , INDENT +
                     INDENT_PLUS ) || TEIL2
         end (* else *) ;
     if TRACELVL >= 1 then
       begin
         WRITELN ( 'zcneu = <' , ZCNEU , '>' ) ;
         WRITELN ( 'zcneu2 = <' , ZCNEU2 , '>' ) ;
         WRITELN ( 'end check_outputline' ) ;
       end (* then *) ;
   end (* CHECK_OUTPUTLINE *) ;



procedure MODIFY_INDENT ;

//******************************************************
// if indentation is required, do the needed actions    
// and set indentation for the following lines          
//******************************************************


   begin (* MODIFY_INDENT *)
     if GS . INDENT_ZERO then
       GS . INDENT := 0
     else
       GS . INDENT := GS . INDENT + GS . INDENT_INCR ;
     GS . INDENT_INCR := 0 ;
     GS . INDENT_ZERO := FALSE ;
   end (* MODIFY_INDENT *) ;



procedure STACK_STATEMENT ;

//******************************************************
// put statement type and new indentation               
// into statement stack                                 
//******************************************************


   var WORK_INDENT : INTEGER ;

   begin (* STACK_STATEMENT *)
     if GS . NEW_STATEMENT <> ' ' then
       begin
         WORK_INDENT := 0 ;
         if GS . STMT_LEVEL > 0 then
           WORK_INDENT := GS . STMT_INDENT [ GS . STMT_LEVEL ] ;
         GS . STMT_LEVEL := GS . STMT_LEVEL + 1 ;
         GS . STMT_STACK [ GS . STMT_LEVEL ] := GS . NEW_STATEMENT ;
         GS . STMT_INDENT [ GS . STMT_LEVEL ] := WORK_INDENT + GS .
                                                 INDENT_INCR ;
       end (* then *) ;
     GS . NEW_STATEMENT := ' ' ;
   end (* STACK_STATEMENT *) ;



function UNSTACK_STATEMENT ( const STYPES : STRING ) : BOOLEAN ;

//******************************************************
// remove statement from statement stack                
// and reset indentation to prior value                 
//******************************************************


   var AKT_STMT : CHAR ;
       OK : BOOLEAN ;

   begin (* UNSTACK_STATEMENT *)
     OK := FALSE ;
     while not OK and ( GS . STMT_LEVEL > 0 ) do
       begin
         AKT_STMT := GS . STMT_STACK [ GS . STMT_LEVEL ] ;
         OK := INDEX ( STYPES , AKT_STMT ) > 0 ;
         if not OK then
           GS . STMT_LEVEL := GS . STMT_LEVEL - 1 ;
       end (* while *) ;
     if OK then
       begin
         if GS . STMT_LEVEL > 0 then
           GS . STMT_LEVEL := GS . STMT_LEVEL - 1 ;
         if GS . STMT_LEVEL > 0 then
           GS . INDENT := GS . STMT_INDENT [ GS . STMT_LEVEL ]
         else
           GS . INDENT := 0
       end (* then *) ;
     UNSTACK_STATEMENT := OK ;
   end (* UNSTACK_STATEMENT *) ;



function CHECK_STATEMENT ( const STYPES : STRING ) : INTEGER ;

//******************************************************
// look for last statement of certain type in           
// statement stack (for example evaluate)               
// to resume indentation of that statement              
// (for example for subsequent when clause)             
//******************************************************


   var AKT_STMT : CHAR ;
       OK : BOOLEAN ;
       WORK_LEVEL : INTEGER ;

   begin (* CHECK_STATEMENT *)
     OK := FALSE ;
     WORK_LEVEL := GS . STMT_LEVEL ;
     if TRACELVL >= 1 then
       begin
         WRITELN ( 'start check_statement' ) ;
         WRITELN ( 'stypes = <' , STYPES , '>' ) ;
         WRITELN ( 'level  = ' , WORK_LEVEL ) ;
       end (* then *) ;
     repeat
       AKT_STMT := GS . STMT_STACK [ WORK_LEVEL ] ;
       OK := INDEX ( STYPES , AKT_STMT ) > 0 ;
       if not OK then
         WORK_LEVEL := WORK_LEVEL - 1 ;
     until ( WORK_LEVEL = 0 ) or OK ;
     CHECK_STATEMENT := WORK_LEVEL ;
     if TRACELVL >= 1 then
       begin
         WRITELN ( 'ende check_statement' ) ;
         WRITELN ( 'level  = ' , WORK_LEVEL ) ;
       end (* then *) ;
   end (* CHECK_STATEMENT *) ;



procedure FORCE_NEWLINE ( var OUTF : TEXT ; MOD_INDENT : BOOLEAN ) ;

   var ZCNEU : CHAR ( 72 ) ;
       ZCNEU2 : CHAR ( 72 ) ;

   begin (* FORCE_NEWLINE *)
     if TRACELVL >= 1 then
       begin
         WRITELN ( 'start force_newline' ) ;
         WRITELN ( 'stmt_temp = <' , GS . STMT_TEMP , '>' ) ;
       end (* then *) ;

     //****************************************************
     // check if output fits into current line             
     // or if additional line is needed                    
     //****************************************************

     CHECK_OUTPUTLINE ( 'B' , GS . STMT_TEMP , GS . INDENT , ZCNEU ,
                        ZCNEU2 ) ;
     DO_OUTPUT ( OUTF , ZCNEU , ' ' , 0 , FALSE ) ;
     if ZCNEU2 = ' ' then
       GS . STMT_TEMP := ''
     else
       GS . STMT_TEMP := TRIM ( ZCNEU2 ) ;
     if MOD_INDENT then
       MODIFY_INDENT ;
     if TRACELVL >= 1 then
       begin
         WRITELN ( 'stmt_temp = <' , GS . STMT_TEMP , '>' ) ;
         WRITELN ( 'ende force_newline' ) ;
       end (* then *) ;
   end (* FORCE_NEWLINE *) ;



procedure WORK_NORMAL_STMT ( var OUTF : TEXT ; var COBSTMT : STRING ) ;

//***************************
// work on normal statement  
// check indentation etc.    
//***************************


   var WORD : STRING ( 70 ) ;
       POS_WORD : INTEGER ;
       POS_BLANK : INTEGER ;
       COBWORD : BOOLEAN ;
       PUNKT : BOOLEAN ;
       ICW : INTEGER ;


   procedure ADD_TO_LINE ;

      begin (* ADD_TO_LINE *)
        if GS . STMT_TEMP = '' then
          GS . STMT_TEMP := WORD
        else
          GS . STMT_TEMP := GS . STMT_TEMP || ' ' || WORD ;
      end (* ADD_TO_LINE *) ;


   function CHECKLENGTH ( const S1 : STRING ; const S2 : STRING ) :
                        INTEGER ;

      var X : INTEGER ;

      begin (* CHECKLENGTH *)
        X := LENGTH ( S1 ) ;
        if X <> 0 then
          X := X + 1 ;
        X := X + LENGTH ( S2 ) ;
        CHECKLENGTH := X
      end (* CHECKLENGTH *) ;


   procedure WORK_STMT_READ ;

      begin (* WORK_STMT_READ *)
        if ( WORD = 'AT' ) or ( WORD = 'INVALID' ) then
          begin
            GS . INDENT_INCR := 3 ;
            FORCE_NEWLINE ( OUTF , TRUE ) ;
          end (* then *)
        else
          if ( WORD = 'END' ) or ( WORD = 'KEY' ) then
            begin
              GS . INDENT_INCR := 3 ;
              GS . NEW_STATEMENT := 'R' ;
              STACK_STATEMENT ;
            end (* then *) ;
        ADD_TO_LINE
      end (* WORK_STMT_READ *) ;


   procedure WORK_STMT_SEARCH ;

      begin (* WORK_STMT_SEARCH *)
        if WORD = 'AT' then
          begin
            GS . INDENT_INCR := 3 ;
            FORCE_NEWLINE ( OUTF , TRUE ) ;
          end (* then *)
        else
          if WORD = 'END' then
            begin
              GS . INDENT_INCR := 3 ;
            end (* then *) ;
        ADD_TO_LINE
      end (* WORK_STMT_SEARCH *) ;


   procedure WORK_STMT_WRITE ;

      begin (* WORK_STMT_WRITE *)
        if WORD = 'INVALID' then
          begin
            GS . INDENT_INCR := 3 ;
            FORCE_NEWLINE ( OUTF , TRUE ) ;
          end (* then *)
        else
          if WORD = 'KEY' then
            begin
              GS . INDENT_INCR := 3 ;
              GS . NEW_STATEMENT := 'W' ;
              STACK_STATEMENT ;
            end (* then *) ;
        ADD_TO_LINE
      end (* WORK_STMT_WRITE *) ;


   procedure WORK_STMT_PERFORM ;

   //**********************************************
   // handle perform                               
   // stacking inline perform statement only if    
   // second symbol is not varying or until;       
   // if varying or until appears later, line feed 
   //**********************************************


      begin (* WORK_STMT_PERFORM *)
        if ( WORD = 'VARYING' ) or ( WORD = 'UNTIL' ) or ( WORD =
        'AFTER' ) then
          if GS . NR_SYMB_IN_STMT = 2 then
            begin
              GS . INDENT_INCR := 3 ;
              GS . NEW_STATEMENT := 'P' ;
              STACK_STATEMENT ;
              if TRACELVL >= 1 then
                begin
                  WRITELN ( 'stack statement because of PERFORM' ) ;
                  WRITELN ( 'indent = ' , GS . INDENT : 1 ) ;
                  WRITE ( 'level = ' , GS . STMT_LEVEL : 1 ) ;
                  for I := 1 to GS . STMT_LEVEL do
                    WRITE ( GS . STMT_STACK [ I ] : 2 ) ;
                  WRITE ( ' / ' ) ;
                  for I := 1 to GS . STMT_LEVEL do
                    WRITE ( GS . STMT_INDENT [ I ] : 2 ) ;
                  WRITELN ;
                end (* then *) ;
            end (* then *)
          else
            FORCE_NEWLINE ( OUTF , FALSE ) ;
        ADD_TO_LINE
      end (* WORK_STMT_PERFORM *) ;


   procedure WORK_STMT_EXEC ;

   //**********************************************
   // EXEC CICS                                    
   //**********************************************


      begin (* WORK_STMT_EXEC *)
        if TRACELVL >= 1 then
          begin
            WRITELN ;
            WRITELN ( 'start work_stmt__exec' ) ;
            WRITELN ( 'NR_SYMB_IN_STMT = ' , GS . NR_SYMB_IN_STMT ) ;
            WRITELN ( 'word = ' , WORD ) ;
          end (* then *) ;
        if GS . NR_SYMB_IN_STMT = 3 then
          begin
            GS . INDENT_INCR := 5 ;
            FORCE_NEWLINE ( OUTF , TRUE ) ;
            GS . INDENT_INCR := GS . INDENT_INCR + LENGTH ( WORD ) + 1
                                ;
          end (* then *) ;
        if GS . NR_SYMB_IN_STMT > 4 then
          begin
            if LEFT ( WORD , 1 ) <> '(' then
              FORCE_NEWLINE ( OUTF , TRUE ) ;
          end (* then *) ;
        ADD_TO_LINE ;
        if TRACELVL >= 1 then
          begin
            WRITELN ( 'end work_stmt__exec' ) ;
            WRITELN ;
          end (* then *)
      end (* WORK_STMT_EXEC *) ;


   procedure WORK_STMT_IF ;

   //**********************************************
   // simple method of beautifying IF conditions:  
   // add a new line before every AND / OR         
   //**********************************************


      begin (* WORK_STMT_IF *)
        if ( WORD = 'AND' ) or ( WORD = 'OR' ) then
          FORCE_NEWLINE ( OUTF , FALSE ) ;
        ADD_TO_LINE
      end (* WORK_STMT_IF *) ;


   procedure WORK_STMT_WHEN ;

   //**********************************************
   // simple method of beautifying WHEN conditions:
   // add a new line before every AND / OR         
   //**********************************************


      begin (* WORK_STMT_WHEN *)
        if ( WORD = 'AND' ) or ( WORD = 'OR' ) then
          FORCE_NEWLINE ( OUTF , FALSE ) ;
        ADD_TO_LINE
      end (* WORK_STMT_WHEN *) ;


   procedure WORK_STMT_SYMBOL ;

   //*************************************
   // work on statement symbols           
   // normally: add symbol to line,       
   // but when inside certain statements  
   // do something more sophisticated     
   //*************************************


      begin (* WORK_STMT_SYMBOL *)
        GS . NR_SYMB_IN_STMT := GS . NR_SYMB_IN_STMT + 1 ;
        case GS . STMT_SPECIAL of
          'I' : WORK_STMT_IF ;
          'P' : WORK_STMT_PERFORM ;
          'R' : WORK_STMT_READ ;
          'S' : WORK_STMT_SEARCH ;
          'W' : WORK_STMT_WRITE ;
          'X' : WORK_STMT_EXEC ;
          '1' : WORK_STMT_WHEN ;
          otherwise
            ADD_TO_LINE
        end (* case *)
      end (* WORK_STMT_SYMBOL *) ;


   procedure WORK_PUNKT ;

   //****************************************************
   // handle full stop                                   
   // terminates statement                               
   // forces new line                                    
   // sets indent to zero                                
   //****************************************************


      begin (* WORK_PUNKT *)
        GS . STMT_TEMP := GS . STMT_TEMP || '.' ;
        FORCE_NEWLINE ( OUTF , TRUE ) ;
        GS . STMT_STATUS := 0 ;
        GS . INDENT_ZERO := TRUE ;
        GS . STMT_LEVEL := 0 ;
        GS . STMT_SPECIAL := ' ' ;
      end (* WORK_PUNKT *) ;


   procedure WORK_NEWSTMT ;

   //****************************************************
   // new statement starts                               
   // check indentation requirements based on keyword    
   //****************************************************


      var OK : BOOLEAN ;
          WORK_LEVEL : INTEGER ;

      begin (* WORK_NEWSTMT *)
        GS . NR_SYMB_IN_STMT := 0 ;
        if TRACELVL >= 1 then
          begin
            WRITELN ( 'start work_newstmt' ) ;
            WRITELN ( 'indent = ' , GS . INDENT : 1 ) ;
            WRITE ( 'level = ' , GS . STMT_LEVEL : 1 ) ;
            for I := 1 to GS . STMT_LEVEL do
              WRITE ( GS . STMT_STACK [ I ] : 2 ) ;
            WRITE ( ' / ' ) ;
            for I := 1 to GS . STMT_LEVEL do
              WRITE ( GS . STMT_INDENT [ I ] : 2 ) ;
            WRITELN ;
          end (* then *) ;
        if GS . STMT_STATUS = 1 then
          FORCE_NEWLINE ( OUTF , TRUE ) ;

        //*****************************
        // status := inside statement  
        // check indentation           
        //*****************************

        GS . STMT_STATUS := 1 ;
        GS . NEW_STATEMENT := ' ' ;
        MODIFY_INDENT ;
        GS . STMT_TYPE := CWTAB [ ICW ] . TAGS [ 1 ] ;

        //********************************
        // replace eject by 4 line feeds  
        //********************************

        if WORD = 'EJECT' then
          GS . INDENT_EJECT := TRUE ;

        //*****************
        // exec cics etc.  
        //*****************

        if WORD = 'EXEC' then
          begin
            GS . INDENT_INCR := 5 ;
            GS . NEW_STATEMENT := 'X' ;
          end (* then *) ;

        //***********************************************
        // reduce indentation on end-exec                
        //***********************************************

        if WORD = 'END-EXEC' then
          begin
            OK := UNSTACK_STATEMENT ( 'X' ) ;
            if not OK then
              ERRNO := 11
          end (* then *) ;

        //******************************
        // handle if                    
        //******************************

        if WORD = 'IF' then
          begin
            GS . INDENT_INCR := 3 ;
            GS . NEW_STATEMENT := 'I' ;
          end (* then *) ;

        //******************************
        // handle evaluate              
        //******************************

        if WORD = 'EVALUATE' then
          begin
            GS . INDENT_INCR := 3 ;
            GS . NEW_STATEMENT := 'V' ;
          end (* then *) ;

        //******************************
        // handle search                
        //******************************

        if WORD = 'SEARCH' then
          begin
            GS . INDENT_INCR := 3 ;
            GS . NEW_STATEMENT := 'S' ;
          end (* then *) ;

        //******************************
        // indentation of else keyword  
        //******************************

        if WORD = 'ELSE' then
          begin
            OK := UNSTACK_STATEMENT ( 'I' ) ;
            if not OK then
              ERRNO := 3
            else
              begin
                GS . INDENT_INCR := 3 ;
                GS . NEW_STATEMENT := 'E' ;
              end (* else *) ;
          end (* then *) ;

        //******************************
        // indentation of when keyword  
        //******************************

        if WORD = 'WHEN' then
          begin
            WORK_LEVEL := CHECK_STATEMENT ( 'VS' ) ;
            if WORK_LEVEL = 0 then
              ERRNO := 6
            else
              begin
                GS . STMT_LEVEL := WORK_LEVEL ;
                GS . INDENT := GS . STMT_INDENT [ GS . STMT_LEVEL ] ;
                GS . INDENT_INCR := 3 ;
                if TRACELVL >= 1 then
                  begin
                    WRITELN ( 'found WHEN' ) ;
                    WRITELN ( 'indent = ' , GS . INDENT : 1 ) ;
                    WRITE ( 'level = ' , GS . STMT_LEVEL : 1 ) ;
                    for I := 1 to GS . STMT_LEVEL do
                      WRITE ( GS . STMT_STACK [ I ] : 2 ) ;
                    WRITE ( ' / ' ) ;
                    for I := 1 to GS . STMT_LEVEL do
                      WRITE ( GS . STMT_INDENT [ I ] : 2 ) ;
                    WRITELN ;
                  end (* then *) ;
              end (* else *) ;
          end (* then *) ;

        //***********************************************
        // reduce indentation on end-if                  
        //***********************************************

        if WORD = 'END-IF' then
          begin
            OK := UNSTACK_STATEMENT ( 'IE' ) ;
            if not OK then
              ERRNO := 4
          end (* then *) ;

        //***********************************************
        // reduce indentation on end-read                
        //***********************************************

        if WORD = 'END-READ' then
          begin
            OK := UNSTACK_STATEMENT ( 'R' ) ;
            if not OK then
              ERRNO := 8
          end (* then *) ;

        //***********************************************
        // reduce indentation on end-write               
        //***********************************************

        if WORD = 'END-WRITE' then
          begin
            OK := UNSTACK_STATEMENT ( 'W' ) ;
            if not OK then
              ERRNO := 9
          end (* then *) ;

        //***********************************************
        // reduce indentation on end-evaluate            
        //***********************************************

        if WORD = 'END-EVALUATE' then
          begin
            OK := UNSTACK_STATEMENT ( 'V' ) ;
            if not OK then
              ERRNO := 5 ;
          end (* then *) ;

        //***********************************************
        // reduce indentation on end-search              
        //***********************************************

        if WORD = 'END-SEARCH' then
          begin
            OK := UNSTACK_STATEMENT ( 'S' ) ;
            if not OK then
              ERRNO := 10 ;
          end (* then *) ;

        //***********************************************
        // reduce indentation on end-perform             
        //***********************************************

        if WORD = 'END-PERFORM' then
          begin
            OK := UNSTACK_STATEMENT ( 'P' ) ;
            if not OK then
              ERRNO := 7 ;
          end (* then *) ;

        //***********************************************
        // stack new statement                           
        //***********************************************

        STACK_STATEMENT ;

        //*********************
        // set statement flag  
        //*********************

        case CWTAB [ ICW ] . TAGS [ 2 ] of
          'I' : GS . STMT_SPECIAL := 'I' ;    // IF
          'P' : GS . STMT_SPECIAL := 'P' ;    // PERFORM
          'R' : GS . STMT_SPECIAL := 'R' ;    // READ
          'S' : GS . STMT_SPECIAL := 'S' ;    // SEARCH
          'W' : GS . STMT_SPECIAL := 'W' ;    // WRITE
          'X' : GS . STMT_SPECIAL := 'X' ;    // EXEC
          '1' : GS . STMT_SPECIAL := '1' ;    // WHEN
          otherwise
            GS . STMT_SPECIAL := ' ' ;
        end (* case *) ;
        if TRACELVL >= 1 then
          begin
            WRITELN ( 'indent = ' , GS . INDENT : 1 ) ;
            WRITE ( 'level = ' , GS . STMT_LEVEL : 1 ) ;
            for I := 1 to GS . STMT_LEVEL do
              WRITE ( GS . STMT_STACK [ I ] : 2 ) ;
            WRITE ( ' / ' ) ;
            for I := 1 to GS . STMT_LEVEL do
              WRITE ( GS . STMT_INDENT [ I ] : 2 ) ;
            WRITELN ;
            WRITELN ( 'ende work_newstmt' ) ;
          end (* then *) ;
      end (* WORK_NEWSTMT *) ;


   begin (* WORK_NORMAL_STMT *)
     if TRACELVL >= 1 then
       begin
         WRITELN ( 'start work_normal_stmt' ) ;
         WRITELN ( 'cobstmt = <' , COBSTMT , '>' )
       end (* then *) ;

     //****************************************************
     // this loop processes all the symbols from a single  
     // zone b source line inside the procedure division   
     // and passes them one by one to the correct          
     // processing routine                                 
     //****************************************************

     POS_WORD := 1 ;
     while TRUE do
       begin
         POS_BLANK := NEXT_BLANK ( COBSTMT , POS_WORD ) ;
         if POS_BLANK = 0 then
           WORD := SUBSTR ( COBSTMT , POS_WORD )
         else
           WORD := SUBSTR ( COBSTMT , POS_WORD , POS_BLANK - POS_WORD )
                   ;
         if TRACELVL >= 1 then
           begin
             WRITELN ;
             WRITELN ( '*** handle symbol word = <' , WORD , '> ***' )
                       ;
           end (* then *) ;
         CHECK_WORD_1 ( WORD , COBWORD , PUNKT , ICW , GS .
                        STMT_SPECIAL ) ;
         if PUNKT then
           WORK_PUNKT
         else
           begin
             if COBWORD then
               WORK_NEWSTMT ;
             if CHECKLENGTH ( GS . STMT_TEMP , WORD ) > 61 then
               FORCE_NEWLINE ( OUTF , FALSE ) ;
             WORK_STMT_SYMBOL
           end (* else *) ;
         if TRACELVL >= 1 then
           begin
             WRITELN ( 'cobword  = ' , COBWORD ) ;
             WRITELN ( 'punkt    = ' , PUNKT ) ;
             WRITELN ( 'symbnr   = ' , GS . NR_SYMB_IN_STMT ) ;
           end (* then *) ;
         if POS_BLANK = 0 then
           break ;
         POS_WORD := POS_BLANK + 1
       end (* while *) ;
     if TRACELVL >= 1 then
       begin
         WRITELN ( 'cobstmt = <' , COBSTMT , '>' ) ;
         WRITELN ( 'end work_normal_stmt' ) ;
       end (* then *) ;
   end (* WORK_NORMAL_STMT *) ;



procedure WORK_DATALINES ( var COBSTMT : STRING ; var PUNKT : BOOLEAN )
                         ;

//************************************
// bearbeite Zeilen unterhalb von FD  
// nichts weiter zu tun               
//************************************


   var COBX : STRING ( 70 ) ;

   begin (* WORK_DATALINES *)
     PUNKT := FALSE ;
     if FALSE then
       WRITELN ( NSOURCE , '*fdlines: div = ' , GS . DIV_FLAG ,
                 ' sect = ' , GS . SECT_FLAG , ' actlevel = ' ,
                 ACTLEVEL ) ;
     if ACTLEVEL > 0 then
       begin
         if ACTLEVEL > 1 then
           COBX := BLANKS_LEVEL ( ACTLEVEL , 'B' )
         else
           COBX := BLANKS_LEVEL ( ACTLEVEL , 'A' ) ;
         COBX := COBX || TRIM ( COBSTMT ) ;
         PUNKT := RIGHT ( COBX , 1 ) = '.' ;
         if LENGTH ( COBX ) <= WIDTH_ZONE_B then
           COBSTMT := COBX
         else
           begin
             COBX := SUBSTR ( COBX , 5 ) ;
             if LENGTH ( COBX ) <= WIDTH_ZONE_B then
               COBSTMT := COBX
           end (* else *)
       end (* then *)
   end (* WORK_DATALINES *) ;



procedure WORK_FILES ( const COBSTMT : STRING ; var IS_SELECT : BOOLEAN
                     ) ;

//***********************************
// Bearbeite Zeilen innerhalb FILES  
// SELECT speziell                   
//***********************************


   begin (* WORK_FILES *)
     if FALSE then
       WRITELN ( GS . ZKOMM1 , COBSTMT ) ;
     IS_SELECT := LEFT ( COBSTMT , 7 ) = 'SELECT ' ;
   end (* WORK_FILES *) ;



procedure WORK_OTHER_SECTIONS ( const COBSTMT : STRING ) ;

//***********************************************************
// Bearbeite SECTIONs ausserhalb von DATA und PROC DIVISION  
//***********************************************************


   begin (* WORK_OTHER_SECTIONS *)
     if FALSE then
       WRITELN ( GS . ZKOMM1 , COBSTMT ) ;
   end (* WORK_OTHER_SECTIONS *) ;



procedure WORK_SPECIAL ( const COBSTMT : STRING ) ;

//**************************
// Bearbeite SPECIAL-NAMES  
//**************************


   begin (* WORK_SPECIAL *)
     if FALSE then
       WRITELN ( GS . ZKOMM1 , COBSTMT ) ;
   end (* WORK_SPECIAL *) ;



procedure WORK_FILE_CONTROL ( const COBSTMT : STRING ) ;

//*************************
// Bearbeite FILE-CONTROL  
//*************************


   begin (* WORK_FILE_CONTROL *)
     if FALSE then
       WRITELN ( GS . ZKOMM1 , COBSTMT ) ;
   end (* WORK_FILE_CONTROL *) ;



procedure WORK_PROGRAM_ID ( const COBSTMT : STRING ) ;

//***********************
// Bearbeite PROGRAM-ID  
//***********************


   begin (* WORK_PROGRAM_ID *)
     if FALSE then
       WRITELN ( GS . ZKOMM1 , COBSTMT ) ;
   end (* WORK_PROGRAM_ID *) ;



procedure WORK_UNDEF ( const COBSTMT : STRING ) ;

   begin (* WORK_UNDEF *)
     if FALSE then
       WRITELN ( GS . ZKOMM1 , COBSTMT ) ;
     WRITELN ( NSOURCE , '*udef:' ) ;
   end (* WORK_UNDEF *) ;



procedure MODIFY_LEVEL ( ZONE : CHAR ; var COBSTMT : STRING ; XLEVEL :
                       INTEGER ; XPOS0 : INTEGER ; XPOS1 : INTEGER ;
                       XPOS2 : INTEGER ) ;

//***************************************************************
// modify data definition                                        
// that is: insert blanks, so that pic and value                 
// is always at same column position (if possible)               
// desired column positions are constants datapos1 and datapos2  
//***************************************************************


   var SNEU : STRING ( 70 ) ;
       COB1 : STRING ( 70 ) ;
       COB2 : STRING ( 70 ) ;
       COB3 : STRING ( 70 ) ;
       COBX : STRING ( 200 ) ;
       STARTPOS : INTEGER ;
       L1 : INTEGER ;
       L2 : INTEGER ;

   begin (* MODIFY_LEVEL *)
     if FALSE then
       begin
         WRITELN ( NSOURCE , '*modlevel ' , ACTLEVEL , XLEVEL ) ;
         WRITELN ( NSOURCE , '*level ' , XPOS0 , XPOS1 , XPOS2 ) ;
       end (* then *) ;
     SNEU := BLANKS_LEVEL ( ACTLEVEL , ZONE ) ;
     SNEU := SNEU || NUMMER2 ( XLEVEL ) || '  ' ;
     if XPOS0 = 0 then
       COBSTMT := SNEU
     else
       if XPOS1 = 0 then
         COBSTMT := SNEU || SUBSTR ( COBSTMT , XPOS0 )
       else
         if XPOS2 = 0 then
           begin
             COB1 := SUBSTR ( COBSTMT , XPOS0 , XPOS1 - XPOS0 ) ;
             COB2 := SUBSTR ( COBSTMT , XPOS1 ) ;
             COB1 := TRIM ( COB1 ) ;
             COB2 := TRIM ( COB2 ) ;
             STARTPOS := 12 + ( ACTLEVEL - 1 ) * 4 ;
             L1 := DATAPOS1 - STARTPOS ;
             if FALSE then
               begin
                 WRITELN ( NSOURCE , '*cob1 = <' , COB1 , '>' ) ;
                 WRITELN ( NSOURCE , '*cob2 = <' , COB2 , '>' ) ;
                 WRITELN ( NSOURCE , '*startpos = ' , STARTPOS ) ;
                 WRITELN ( NSOURCE , '*l1 = ' , L1 ) ;
               end (* then *) ;
             if LENGTH ( COB1 ) < L1 then
               COB1 := LEFT ( COB1 , L1 )
             else
               COB1 := COB1 || ' ' ;
             COBX := SNEU || COB1 || COB2 ;
             if LENGTH ( COBX ) > WIDTH_ZONE_B then
               COBSTMT := SNEU || SUBSTR ( COBSTMT , XPOS0 )
             else
               COBSTMT := COBX
           end (* then *)
         else
           begin
             COB1 := SUBSTR ( COBSTMT , XPOS0 , XPOS1 - XPOS0 ) ;
             COB2 := SUBSTR ( COBSTMT , XPOS1 , XPOS2 - XPOS1 ) ;
             COB3 := SUBSTR ( COBSTMT , XPOS2 ) ;
             COB1 := TRIM ( COB1 ) ;
             COB2 := TRIM ( COB2 ) ;
             COB3 := TRIM ( COB3 ) ;
             STARTPOS := 12 + ( ACTLEVEL - 1 ) * 4 ;
             L1 := DATAPOS1 - STARTPOS ;
             if LENGTH ( COB1 ) < L1 then
               COB1 := LEFT ( COB1 , L1 )
             else
               COB1 := COB1 || ' ' ;
             L2 := DATAPOS2 - DATAPOS1 ;
             if LENGTH ( COB2 ) < L2 then
               COB2 := LEFT ( COB2 , L2 )
             else
               COB2 := COB2 || ' ' ;
             if FALSE then
               begin
                 WRITELN ( NSOURCE , '*sneu = <' , SNEU , '>' ) ;
                 WRITELN ( NSOURCE , '*cob1 = <' , COB1 , '>' ) ;
                 WRITELN ( NSOURCE , '*cob2 = <' , COB2 , '>' ) ;
                 WRITELN ( NSOURCE , '*cob3 = <' , COB3 , '>' ) ;
               end (* then *) ;
             COBX := SNEU || COB1 || COB2 || COB3 ;
             if LENGTH ( COBX ) > WIDTH_ZONE_B then
               begin
                 COBX := SNEU || COB1 || TRIM ( COB2 ) || ' ' || COB3 ;
                 if LENGTH ( COBX ) > WIDTH_ZONE_B then
                   COBSTMT := SNEU || SUBSTR ( COBSTMT , XPOS0 )
                 else
                   COBSTMT := COBX
               end (* then *)
             else
               COBSTMT := COBX
           end (* else *)
   end (* MODIFY_LEVEL *) ;



procedure WORK_ZONEA ( var ZCNEU : CHAR ( 72 ) ;      // ergebn.zeile 1
                     var ZCNEU2 : CHAR ( 72 ) ;       // ergebn.zeile 2
                     ZC : CHAR ( 72 ) ;               // eingabezeile
                     var BLANK_LINES : INTEGER ) ;    // leerzeilen

//******************************************
// Bearbeite Zeile mit Zone A Content       
//******************************************


   var COBSTMT : STRING ( 70 ) ;
       XPOS0 : INTEGER ;
       XPOS1 : INTEGER ;
       XPOS2 : INTEGER ;
       XLEVEL : INTEGER ;

   begin (* WORK_ZONEA *)
     BLANK_LINES := 0 ;
     GS . ZKOMM1 := LEFT ( ZC , 6 ) ;
     COBSTMT := COMP_COBOL ( SUBSTR ( ZC , 8 ) ) ;
     case GS . DIV_FLAG of

     //*****************************************
     // generate content in procedure division  
     //*****************************************

       'P' : begin
               WORK_PARAGRAPH_SECT ( COBSTMT ) ;
               if ERRNO = 0 then
                 if RIGHT ( COBSTMT , 2 ) = ' .' then
                   COBSTMT := DELETE ( COBSTMT , LENGTH ( COBSTMT ) - 1
                              , 1 ) ;
             end (* tag/ca *) ;

     //********************************************************
     // generate content in data division                      
     // most interesting: lines which start with level number  
     //********************************************************

       'D' : begin
               if GS . PUNKT_GEF and ( COBSTMT [ 1 ] in [ '0' .. '9' ]
               ) then
                 begin
                   WORK_LEVEL ( COBSTMT , XLEVEL , XPOS0 , XPOS1 ,
                                XPOS2 , GS . PUNKT_GEF ) ;
                   MODIFY_LEVEL ( 'A' , COBSTMT , XLEVEL , XPOS0 ,
                                  XPOS1 , XPOS2 ) ;
                 end (* then *)
               else
                 if LEFT ( COBSTMT , 3 ) = 'FD ' then
                   begin
                     WORK_FD ( COBSTMT ) ;
                     COBSTMT := SUBSTR ( COBSTMT , 4 ) ;
                     COBSTMT := TRIM ( COBSTMT ) ;
                     COBSTMT := 'FD  ' || COBSTMT ;
                     GS . PUNKT_GEF := TRUE ;
                   end (* then *)
                 else
                   if INDEX ( COBSTMT , 'SECTION' ) <> 0 then
                     begin
                       WORK_DATA_SECTION ( COBSTMT ) ;
                       BLANK_LINES := 1 ;
                       GS . PUNKT_GEF := TRUE ;
                     end (* then *)
                   else
                     if ( INDEX ( COBSTMT , 'DIVISION' ) <> 0 ) and (
                     LEFT ( COBSTMT , 4 ) = 'PROC' ) then
                       begin
                         ACTLEVEL := 0 ;
                         COBSTMT := 'PROCEDURE DIVISION.' ;
                         GS . DIV_FLAG := 'P' ;
                         GS . SECT_FLAG := ' ' ;
                         BLANK_LINES := 1 ;
                         GS . PUNKT_GEF := FALSE ;
                       end (* then *)
                     else
                       WORK_DATALINES ( COBSTMT , GS . PUNKT_GEF )
             end (* tag/ca *) ;

     //*****************************************************
     // other: certain special lines which start in area a  
     //*****************************************************

       otherwise
         begin
           if INDEX ( COBSTMT , 'DIVISION' ) <> 0 then
             begin
               ACTLEVEL := 0 ;
               BLANK_LINES := 1 ;
               if LEFT ( COBSTMT , 2 ) = 'ID' then
                 begin
                   COBSTMT := 'IDENTIFICATION DIVISION.' ;
                   GS . DIV_FLAG := 'I' ;
                 end (* then *)
               else
                 if LEFT ( COBSTMT , 3 ) = 'ENV' then
                   begin
                     COBSTMT := 'ENVIRONMENT DIVISION.' ;
                     GS . DIV_FLAG := 'I' ;
                   end (* then *)
                 else
                   if LEFT ( COBSTMT , 4 ) = 'DATA' then
                     begin
                       COBSTMT := 'DATA DIVISION.' ;
                       GS . DIV_FLAG := 'D' ;
                       GS . PUNKT_GEF := TRUE ;
                     end (* then *)
                   else
                     if LEFT ( COBSTMT , 4 ) = 'PROC' then
                       begin
                         COBSTMT := 'PROCEDURE DIVISION.' ;
                         GS . DIV_FLAG := 'P' ;
                         GS . SECT_FLAG := ' '
                       end (* then *)
             end (* then *)
           else
             if INDEX ( COBSTMT , 'SECTION' ) <> 0 then
               begin
                 WORK_OTHER_SECTIONS ( COBSTMT ) ;
                 BLANK_LINES := 1
               end (* then *)
             else
               if INDEX ( COBSTMT , 'SPECIAL-NAMES' ) <> 0 then
                 WORK_SPECIAL ( COBSTMT )
               else
                 if INDEX ( COBSTMT , 'FILE-CONTROL' ) <> 0 then
                   WORK_FILE_CONTROL ( COBSTMT )
                 else
                   if INDEX ( COBSTMT , 'PROGRAM-ID' ) <> 0 then
                     WORK_PROGRAM_ID ( COBSTMT )
                   else
                     WORK_UNDEF ( COBSTMT )
         end (* otherw *)
     end (* case *) ;

     //****************************************************
     // check if output fits into current line             
     // or if additional line is needed                    
     //****************************************************

     ZCNEU2 := ' ' ;
     CHECK_OUTPUTLINE ( 'A' , COBSTMT , GS . INDENT , ZCNEU , ZCNEU2 )
                        ;
   end (* WORK_ZONEA *) ;



procedure WORK_ZONEB ( var OUTF : TEXT ;              // Ausgabedatei
                     var ZCNEU : CHAR ( 72 ) ;        // ergebn.zeile 1
                     var ZCNEU2 : CHAR ( 72 ) ;       // ergebn.zeile 2
                     ZC : CHAR ( 72 ) ;               // eingabezeile
                     var BLANK_LINES : INTEGER ;      // leerzeilen
                     var OUTP : BOOLEAN ) ;           // ausgeben

//******************************************
// Bearbeite Zeile mit Zone B Content       
//******************************************


   var COBSTMT : STRING ( 70 ) ;
       IS_SELECT : BOOLEAN ;
       XPOS0 : INTEGER ;
       XPOS1 : INTEGER ;
       XPOS2 : INTEGER ;
       XLEVEL : INTEGER ;

   begin (* WORK_ZONEB *)
     BLANK_LINES := 0 ;
     OUTP := TRUE ;
     GS . ZKOMM1 := LEFT ( ZC , 6 ) ;
     COBSTMT := COMP_COBOL ( SUBSTR ( ZC , 12 ) ) ;
     case GS . DIV_FLAG of

     //*****************************************
     // generate content in procedure division  
     //*****************************************

       'P' : begin
               WORK_NORMAL_STMT ( OUTF , COBSTMT ) ;
               OUTP := FALSE ;
             end (* tag/ca *) ;

     //********************************************************
     // generate content in data division                      
     // most interesting: lines which start with level number  
     //********************************************************

       'D' : begin
               if GS . PUNKT_GEF and ( COBSTMT [ 1 ] in [ '0' .. '9' ]
               ) then
                 begin
                   WORK_LEVEL ( COBSTMT , XLEVEL , XPOS0 , XPOS1 ,
                                XPOS2 , GS . PUNKT_GEF ) ;
                   MODIFY_LEVEL ( 'B' , COBSTMT , XLEVEL , XPOS0 ,
                                  XPOS1 , XPOS2 ) ;
                 end (* then *)
               else
                 WORK_DATALINES ( COBSTMT , GS . PUNKT_GEF )
             end (* tag/ca *) ;

     //***********************************************
     // other: lines which begin in area b are lines  
     // which deal with files                         
     //***********************************************

       otherwise
         begin
           WORK_FILES ( COBSTMT , IS_SELECT ) ;
           if IS_SELECT then
             BLANK_LINES := 1
           else
             COBSTMT := '   ' || COBSTMT
         end (* otherw *) ;
     end (* case *) ;

     //****************************************************
     // check if output fits into current line             
     // or if additional line is needed                    
     //****************************************************

     CHECK_OUTPUTLINE ( 'B' , COBSTMT , GS . INDENT , ZCNEU , ZCNEU2 )
   end (* WORK_ZONEB *) ;



procedure WORK ( OUTF : TEXT ; const ZEILE : STRING ) ;

   var ZC : CHAR ( 72 ) ;
       ZCNEU : CHAR ( 72 ) ;
       ZCNEU2 : CHAR ( 72 ) ;
       DONE : BOOLEAN ;
       I : INTEGER ;
       BL : INTEGER ;
       OUTP : BOOLEAN ;

   begin (* WORK *)
     DONE := FALSE ;
     if LENGTH ( ZEILE ) > 72 then
       ZC := LEFT ( ZEILE , 72 )
     else
       ZC := ZEILE ;

     //***************************************
     // i = position of first non-blank char  
     //***************************************

     I := 1 ;
     while I < 73 do
       if ZC [ I ] = ' ' then
         begin
           I := I + 1 ;
           continue
         end (* then *)
       else
         break ;

     //***********************
     // handle process cards  
     //***********************

     if ( I >= 2 ) and ( I < 60 ) then
       if SUBSTR ( ZC , I , 7 ) = 'PROCESS' then
         begin
           WORK_PROCESS ( ZCNEU , ZC ) ;
           OUT_SRCLINE ( OUTF , ZCNEU ) ;
           DONE := TRUE
         end (* then *) ;

     //***************************
     // handle comment            
     //***************************

     if not DONE then
       if ZC [ 7 ] = '*' then
         begin
           if GS . STMT_TEMP <> '' then
             FORCE_NEWLINE ( OUTF , FALSE ) ;
           ZCNEU := ZC ;
           OUT_SRCLINE ( OUTF , ZCNEU ) ;
           DONE := TRUE
         end (* then *) ;

     //******************************************
     // handle ca-vollie include cards           
     //******************************************

     if not DONE then
       if ( LEFT ( ZC , 4 ) = '+INC' ) or ( LEFT ( ZC , 4 ) = '-INC' )
       then
         begin
           if GS . STMT_TEMP <> '' then
             begin
               FORCE_NEWLINE ( OUTF , TRUE ) ;
               GS . STMT_STATUS := 0 ;
             end (* then *) ;
           ZCNEU := ZC ;
           OUT_SRCLINE ( OUTF , ZCNEU ) ;
           DONE := TRUE
         end (* then *) ;

     //********************************************
     // handle cobol card with zone a content      
     //********************************************

     if not DONE then
       if SUBSTR ( ZC , 8 , 4 ) <> ' ' then
         begin
           if GS . STMT_TEMP <> '' then
             FORCE_NEWLINE ( OUTF , TRUE ) ;
           GS . STMT_STATUS := 0 ;
           WORK_ZONEA ( ZCNEU , ZCNEU2 , ZC , BL ) ;
           DO_OUTPUT ( OUTF , ZCNEU , ZCNEU2 , BL , FALSE ) ;
           DONE := TRUE
         end (* then *) ;

     //********************************************
     // handle blank line                          
     //********************************************

     if not DONE then
       if SUBSTR ( ZC , 8 ) = ' ' then
         begin
           if GS . STMT_TEMP <> '' then
             begin
               if TRACELVL >= 1 then
                 begin
                   WRITELN ( '*** force_newline bei blank line' ) ;
                   WRITELN ( 'stmt_temp = <' , GS . STMT_TEMP , '>' ) ;
                 end (* then *) ;
               FORCE_NEWLINE ( OUTF , FALSE ) ;
             end (* then *)
           else
             begin
               if TRACELVL >= 1 then
                 begin
                   WRITELN ( '*** out_srcline bei blank line' ) ;
                 end (* then *) ;
               ZCNEU := ZC ;
               OUT_SRCLINE ( OUTF , ZCNEU )
             end (* else *) ;
           DONE := TRUE
         end (* then *) ;

     //********************************************
     // handle cobol card with zone b content      
     //********************************************

     if not DONE then
       begin
         if TRACELVL >= 1 then
           begin
             WRITELN ( '*** handle cobol card with zone b content' ) ;
             WRITELN ( '*** zc = <' , ZC , '>' ) ;
           end (* then *) ;
         GS . INDENT_EJECT := FALSE ;
         WORK_ZONEB ( OUTF , ZCNEU , ZCNEU2 , ZC , BL , OUTP ) ;
         if OUTP then
           DO_OUTPUT ( OUTF , ZCNEU , ZCNEU2 , BL , GS . INDENT_EJECT )
                       ;
         DONE := TRUE
       end (* then *) ;
   end (* WORK *) ;



function CHECKTYPE ( ST : STATUS_TYPE ; const Z : STRING ) :
                   STATUS_TYPE ;

   var NEWST : STATUS_TYPE ;

   begin (* CHECKTYPE *)
     NEWST := ST ;
     if ST in [ INITST , VORCOBOL ] then
       begin
         if ( INDEX ( Z , ' PROCESS ' ) > 0 ) or ( INDEX ( Z ,
         ' DIVISION' ) > 0 ) then
           NEWST := INCOBOL
         else
           NEWST := VORCOBOL
       end (* then *)
     else
       if ST = INCOBOL then
         begin
           if Z <> '' then
             if ( LEFT ( Z , 2 ) = '//' ) or ( LEFT ( Z , 2 ) = '/*' )
             then
               NEWST := NACHCOBOL
         end (* then *)
       else
         NEWST := ST ;
     CHECKTYPE := NEWST
   end (* CHECKTYPE *) ;



begin (* HAUPTPROGRAMM *)
  if FALSE then
    begin

  //*************
  // some tests  
  //*************

      ZEILE := 'test line' ;
      S_UPPER ( ZEILE ) ;
      WRITELN ( ZEILE ) ;
    end (* then *) ;

  //********************************************
  // here goes the main program                 
  //********************************************

  STATUS := INITST ;
  for I := 1 to MAXLEVEL do
    LEVEL_TAB [ I ] := 0 ;
  ACTLEVEL := 0 ;

  //********************************************
  // init status variables (in struct gs)       
  //********************************************

  GS . ICNT_PRE := 0 ;
  GS . OCNT_PRE := 0 ;
  GS . ICNT_COB := 0 ;
  GS . OCNT_COB := 0 ;
  GS . ICNT_POST := 0 ;
  GS . OCNT_POST := 0 ;
  GS . PUNKT_GEF := TRUE ;
  GS . DIV_FLAG := ' ' ;
  GS . SECT_FLAG := ' ' ;
  GS . INDENT := 0 ;
  GS . INDENT_INCR := 0 ;
  GS . INDENT_ZERO := FALSE ;
  GS . INDENT_EJECT := FALSE ;
  GS . ZKOMM1 := ' ' ;
  GS . STMT_TYPE := ' ' ;
  GS . STMT_SPECIAL := ' ' ;
  GS . STMT_LEVEL := 0 ;
  GS . STMT_STATUS := 0 ;
  GS . STMT_TEMP := '' ;
  GS . NEW_STATEMENT := ' ' ;
  GS . NR_SYMB_IN_STMT := 0 ;
  for I := 1 to MAXLEVEL do
    begin
      GS . STMT_STACK [ I ] := ' ' ;
      GS . STMT_INDENT [ I ] := 0 ;
    end (* for *) ;
  GS . THEN_FOUND := FALSE ;

  //********************************************
  // open files                                 
  //********************************************

  RESET ( SOURCE ) ;
  REWRITE ( NSOURCE ) ;
  REWRITE ( LISTING ) ;
  WRITELN ( '*** COBOL Source Code Formatter' ) ;
  WRITELN ( '*** ' , DATE , ' ' , TIME ) ;
  LINENO := 0 ;
  OLDSTATUS := STATUS ;
  while not EOF ( SOURCE ) do
    begin
      READLN ( SOURCE , ZEILE ) ;
      LINENO := LINENO + 1 ;
      STATUS := CHECKTYPE ( STATUS , ZEILE ) ;
      if STATUS <> OLDSTATUS then
        begin
          WRITELN ( LISTING ) ;
          WRITELN ( LISTING , '*** COBOL Source Code Formatter *** ' ,
                    ' ' : 20 , DATE , ' ' , TIME ) ;
          WRITELN ( LISTING ) ;
          case STATUS of
            VORCOBOL :
              WRITELN ( LISTING , '--- Pre-COBOL Part ---' ) ;
            INCOBOL :
              WRITELN ( LISTING , '--- COBOL Part ---' ) ;
            NACHCOBOL :
              WRITELN ( LISTING , '--- Post-COBOL Part ---' ) ;
          end (* case *) ;
          WRITELN ( LISTING ) ;
          OLDSTATUS := STATUS ;
        end (* then *) ;
      case STATUS of
        VORCOBOL :
          begin
            WRITELN ( NSOURCE , ZEILE ) ;
            GS . ICNT_PRE := GS . ICNT_PRE + 1 ;
            GS . OCNT_PRE := GS . OCNT_PRE + 1 ;
          end (* tag/ca *) ;
        INCOBOL :
          begin
            GS . ICNT_COB := GS . ICNT_COB + 1 ;
            ERRNO := 0 ;
            WORK ( NSOURCE , ZEILE ) ;
            ERR_CHECK ( ERRNO ) ;
          end (* tag/ca *) ;
        NACHCOBOL :
          begin
            WRITELN ( NSOURCE , ZEILE ) ;
            GS . ICNT_POST := GS . ICNT_POST + 1 ;
            GS . OCNT_POST := GS . OCNT_POST + 1 ;
          end (* tag/ca *) ;
        otherwise
          
      end (* case *) ;
      WRITELN ( LISTING , LINENO : 6 , ': ' , ZEILE ) ;
    end (* while *) ;
  WRITELN ( LISTING ) ;
  WRITELN ( LISTING , '*** read pre-COBOL  : ' , GS . ICNT_PRE : 6 ,
            ' lines' ) ;
  WRITELN ( LISTING , '*** read COBOL      : ' , GS . ICNT_COB : 6 ,
            ' lines' ) ;
  WRITELN ( LISTING , '*** read post-COBOL : ' , GS . ICNT_POST : 6 ,
            ' lines' ) ;
  WRITELN ( LISTING , '*** outp pre-COBOL  : ' , GS . OCNT_PRE : 6 ,
            ' lines' ) ;
  WRITELN ( LISTING , '*** outp COBOL      : ' , GS . OCNT_COB : 6 ,
            ' lines' ) ;
  WRITELN ( LISTING , '*** outp post-COBOL : ' , GS . OCNT_POST : 6 ,
            ' lines' ) ;
  WRITELN ;
  WRITELN ( '*** read pre-COBOL  : ' , GS . ICNT_PRE : 6 , ' lines' ) ;
  WRITELN ( '*** read COBOL      : ' , GS . ICNT_COB : 6 , ' lines' ) ;
  WRITELN ( '*** read post-COBOL : ' , GS . ICNT_POST : 6 , ' lines' )
            ;
  WRITELN ( '*** outp pre-COBOL  : ' , GS . OCNT_PRE : 6 , ' lines' ) ;
  WRITELN ( '*** outp COBOL      : ' , GS . OCNT_COB : 6 , ' lines' ) ;
  WRITELN ( '*** outp post-COBOL : ' , GS . OCNT_POST : 6 , ' lines' )
            ;
end (* HAUPTPROGRAMM *) .
