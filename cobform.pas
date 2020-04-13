program COBFORM ( SOURCE , NSOURCE , LISTING ) ;

//***************************************************************
// Source Code Formatter for COBOL                               
// Bernd Oppolzer - 02.2020                                      
//***************************************************************
// to do list:                                                   
// * skip line on statement starters (MOVE, NEXT SENTENCE)       
// * solve problems with string constant including WRITE         
// * correct formatting of multi-level IFs with ELSE             
// * proper stacking of statement types                          
// * solve problem with breakdown on LEICP82A                    
// * solve problem with wrong indentation on OPEN                
// * do major on keywords                                        
// * print errors to listing                                     
// - correct indentation on READ statement                       
// - correct indentation on EVALUATE statement                   
// - modify output on IF conditions                              
// - test: control problems with missing END-EVALUATE            
// - insert THEN keyword on multi-line IFs                       
// - output remarks 1:6 depending on (external) switch - no const
// - compute data offsets depending on (external) switch         
// - control if END-IF terminates IF with missing END-EVALUATE   
// - control if level numbers are inconsistent                   
// - better formatting of level items (01, 05, 10 etc.)          
//***************************************************************



const MAXLEVEL = 20 ;
      WIDTH_ZONE_B = 61 ;
      DATAPOS1 = 38 ;
      DATAPOS2 = 52 ;
      REMARKS_1_6 = FALSE ;


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
                       STMT_TEMP : STRING ( 500 ) ;
                       NEW_STATEMENT : CHAR ;
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
    DIV_FLAG : CHAR ;
    SECT_FLAG : CHAR ;
    LEVEL_TAB : array [ 1 .. MAXLEVEL ] of INTEGER ;
    ACTLEVEL : INTEGER ;
    I : INTEGER ;
    GS : GLOBAL_STATUS ;
    ERRNO : INTEGER ;


const CWANZ = 40 ;
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
        ( 'EVALUATE       ' , 'C' , 'E     ' ) ,    //
        ( 'EXEC           ' , 'C' , '      ' ) ,    //
        ( 'GO             ' , 'C' , '      ' ) ,    //
        ( 'IF             ' , 'C' , 'II    ' ) ,    //
        ( 'INVALID        ' , 'X' , '      ' ) ,    //
        ( 'MOVE           ' , 'C' , '      ' ) ,    //
        ( 'MULTIPLY       ' , 'C' , '      ' ) ,    //
        ( 'NEXT           ' , 'C' , '  R   ' ) ,    //
        ( 'OPEN           ' , 'C' , '      ' ) ,    //
        ( 'PERFORM        ' , 'C' , 'P     ' ) ,    //
        ( 'READ           ' , 'C' , 'XR    ' ) ,    //
        ( 'REWRITE        ' , 'C' , 'X     ' ) ,    //
        ( 'START          ' , 'C' , 'X     ' ) ,    //
        ( 'STOP           ' , 'C' , '      ' ) ,    //
        ( 'SUBTRACT       ' , 'C' , '      ' ) ,    //
        ( 'UNTIL          ' , 'P' , '      ' ) ,    //
        ( 'VARYING        ' , 'P' , '      ' ) ,    //
        ( 'WHEN           ' , 'E' , '      ' ) ,    //
        ( 'WRITE          ' , 'C' , 'X     ' ) ,    //
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



procedure WORK_LEVEL ( ZKOMM1 : CHAR ( 6 ) ; const COBSTMT : STRING ;
                     var ZLEVEL : INTEGER ; var POS0 : INTEGER ; var
                     POS1 : INTEGER ; var POS2 : INTEGER ; var PUNKT :
                     BOOLEAN ) ;

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
       WRITELN ( ZKOMM1 , S ) ;

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



procedure WORK_FD ( ZKOMM1 : CHAR ( 6 ) ; const COBSTMT : STRING ) ;

//**************************************
// Bearbeite FD (nichts weiter zu tun)  
//**************************************


   begin (* WORK_FD *)
     ACTLEVEL := 0 ;
     if FALSE then
       WRITELN ( ZKOMM1 , COBSTMT ) ;
   end (* WORK_FD *) ;



procedure WORK_DATA_SECTION ( ZKOMM1 : CHAR ( 6 ) ; const COBSTMT :
                            STRING ) ;

//********************************************
// Bearbeite SECTION innerhalb DATA DIVISION  
// nur Flag setzen                            
//********************************************


   begin (* WORK_DATA_SECTION *)
     if FALSE then
       WRITELN ( ZKOMM1 , COBSTMT ) ;
     if INDEX ( COBSTMT , 'FILE ' ) <> 0 then
       SECT_FLAG := 'F'
     else
       if INDEX ( COBSTMT , 'WORKING-STORAGE ' ) <> 0 then
         SECT_FLAG := 'W'
       else
         if INDEX ( COBSTMT , 'LINKAGE ' ) <> 0 then
           SECT_FLAG := 'L'
         else
           SECT_FLAG := 'U'
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
     if WORD = '' then
       return ;

     //********************************
     // punkt wegmachen                
     //********************************

     PUNKT := FALSE ;
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
     // check for cobol words = statement starters    
     //***********************************************
     // the word is not a statement starter if it is  
     // encountered inside of a special statement     
     // mentioned in tags [3] ... e.g. read ... next  
     //***********************************************

     for ICW_TEMP := 1 to CWANZ do
       if ( RTRIM ( CWTAB [ ICW_TEMP ] . ID ) = WORD ) and ( CWTAB [
       ICW_TEMP ] . CLASS = 'C' ) then
         if ( SPECIAL = ' ' ) or ( CWTAB [ ICW_TEMP ] . TAGS [ 3 ] <>
         SPECIAL ) then
           begin
             COBWORD := TRUE ;
             ICW := ICW_TEMP ;
             break
           end (* then *)
   end (* CHECK_WORD_1 *) ;



procedure WORK_PARAGRAPH_SECT ( ZKOMM1 : CHAR ( 6 ) ; const COBSTMT :
                              STRING ) ;

   var WORD : STRING ( 70 ) ;
       FEHLER : BOOLEAN ;
       X1 : INTEGER ;
       X : INTEGER ;
       COBWORD : BOOLEAN ;
       PUNKT : BOOLEAN ;
       ICW : INTEGER ;

   begin (* WORK_PARAGRAPH_SECT *)
     if FALSE then
       WRITELN ( ZKOMM1 , COBSTMT ) ;
     FEHLER := FALSE ;
     X1 := 1 ;
     while TRUE do
       begin
         X := NEXT_BLANK ( COBSTMT , X1 ) ;
         if X = 0 then
           WORD := SUBSTR ( COBSTMT , X1 )
         else
           WORD := SUBSTR ( COBSTMT , X1 , X - X1 ) ;

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
         if PUNKT then
           break ;
         if X = 0 then
           break ;
         X1 := X + 1 ;
         X := NEXT_BLANK ( COBSTMT , X1 ) ;
         if X = 0 then
           WORD := SUBSTR ( COBSTMT , X1 )
         else
           WORD := SUBSTR ( COBSTMT , X1 , X - X1 ) ;
         if WORD <> 'SECTION.' then
           begin
             FEHLER := TRUE ;
             ERRNO := 2
           end (* then *) ;
         break
       end (* while *) ;

     //***************************************************
     // indent is always zero, when new paragraph starts  
     //***************************************************

     if not FEHLER then
       begin
         GS . INDENT := 0 ;
         GS . STMT_LEVEL := 0 ;
       end (* then *) ;
   end (* WORK_PARAGRAPH_SECT *) ;



procedure OUT_SRCLINE ( OUTF : TEXT ; ZEILE : CHAR ( 72 ) ) ;

   var S : STRING ( 72 ) ;
       X : STRING ( 6 ) ;

   begin (* OUT_SRCLINE *)
     S := RTRIM ( ZEILE ) ;
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
   end (* OUT_SRCLINE *) ;



procedure DO_OUTPUT ( var OUTF : TEXT ; ZCNEU : CHAR ( 72 ) ; ZCNEU2 :
                    CHAR ( 72 ) ; BL : INTEGER ; INDENT_EJECT : BOOLEAN
                    ) ;

   var I : INTEGER ;

   begin (* DO_OUTPUT *)
     if INDENT_EJECT then
       BL := BL + 4 ;
     for I := 1 to BL do
       WRITELN ( OUTF ) ;
     GS . OCNT_COB := GS . OCNT_COB + BL ;
     if FALSE then
       begin
         WRITELN ( OUTF , '** indent = ' , GS . INDENT : 1 ) ;
         WRITE ( OUTF , '** level = ' , GS . STMT_LEVEL : 1 ) ;
         for I := 1 to GS . STMT_LEVEL do
           WRITE ( OUTF , GS . STMT_STACK [ I ] : 2 ) ;
         WRITELN ( OUTF ) ;
       end (* then *) ;
     if not INDENT_EJECT then
       begin
         OUT_SRCLINE ( OUTF , ZCNEU ) ;
         if ZCNEU2 <> ' ' then
           OUT_SRCLINE ( OUTF , ZCNEU2 ) ;
       end (* then *) ;
     if FALSE then
       begin
         WRITE ( OUTF , '** level = ' , GS . STMT_LEVEL : 1 ) ;
         for I := 1 to GS . STMT_LEVEL do
           WRITE ( OUTF , GS . STMT_STACK [ I ] : 2 ) ;
         WRITELN ( OUTF ) ;
       end (* then *) ;
   end (* DO_OUTPUT *) ;



procedure CHECK_OUTPUTLINE ( ZONE : CHAR ; ZKOMM1 : CHAR ( 6 ) ; const
                           COBSTMT : STRING ; INDENT : INTEGER ; var
                           ZCNEU : CHAR ( 72 ) ; var ZCNEU2 : CHAR ( 72
                           ) ) ;

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
     if LENGTH ( COBSTMT ) + INDENT <= MAXL then
       ZCNEU := ZKOMM1 || REPEATSTR ( ' ' , INDENT + INDENT_PLUS ) ||
                COBSTMT
     else
       begin
         X := INDEX ( COBSTMT , ' VALUE ' ) ;
         if X = 0 then
           X := 31 ;
         TEIL1 := SUBSTR ( COBSTMT , 1 , X - 1 ) ;
         TEIL2 := SUBSTR ( COBSTMT , X + 1 ) ;
         ZCNEU := ZKOMM1 || REPEATSTR ( ' ' , INDENT + INDENT_PLUS ) ||
                  TEIL1 ;
         ZCNEU2 := ZKOMM1 || REPEATSTR ( ' ' , INDENT + INDENT_PLUS )
                   || TEIL2
       end (* else *)
   end (* CHECK_OUTPUTLINE *) ;



procedure CHECK_OUTPUT_B ( ZKOMM1 : CHAR ( 6 ) ;      // komm left
                         const COBSTMT : STRING ;     // statement
                         var ZCNEU : CHAR ( 72 ) ;    // ergebn.zeile 1
                         var ZCNEU2 : CHAR ( 72 ) ) ; // ergebn.zeile 2

   begin (* CHECK_OUTPUT_B *)

     //******************************************************
     // check if output fits into current line               
     // or if additional line is needed                      
     //******************************************************

     ZCNEU2 := ' ' ;
     CHECK_OUTPUTLINE ( 'B' , ZKOMM1 , COBSTMT , GS . INDENT , ZCNEU ,
                        ZCNEU2 ) ;
   end (* CHECK_OUTPUT_B *) ;



procedure MODIFY_INDENT ;

   begin (* MODIFY_INDENT *)

     //******************************************************
     // if indentation is required, do the needed actions    
     // and set indentation for the following lines          
     //******************************************************

     if GS . INDENT_ZERO then
       GS . INDENT := 0
     else
       GS . INDENT := GS . INDENT + GS . INDENT_INCR ;
     GS . INDENT_INCR := 0 ;
     GS . INDENT_ZERO := FALSE ;
   end (* MODIFY_INDENT *) ;



procedure STACK_STATEMENT ;

   begin (* STACK_STATEMENT *)
     if GS . NEW_STATEMENT <> ' ' then
       begin
         GS . STMT_LEVEL := GS . STMT_LEVEL + 1 ;
         GS . STMT_STACK [ GS . STMT_LEVEL ] := GS . NEW_STATEMENT ;
         GS . STMT_INDENT [ GS . STMT_LEVEL ] := GS . INDENT_INCR ;
       end (* then *) ;
     GS . NEW_STATEMENT := ' ' ;
   end (* STACK_STATEMENT *) ;



procedure UNSTACK_STATEMENT ( const STYPES : STRING ) ;

   var AKT_STMT : CHAR ;

   begin (* UNSTACK_STATEMENT *)
     repeat
       AKT_STMT := GS . STMT_STACK [ GS . STMT_LEVEL ] ;
       if GS . INDENT > 0 then
         GS . INDENT := GS . INDENT - GS . STMT_INDENT [ GS .
                        STMT_LEVEL ] ;
       GS . STMT_LEVEL := GS . STMT_LEVEL - 1 ;
     until ( GS . STMT_LEVEL = 0 ) or ( INDEX ( STYPES , AKT_STMT ) > 0
     ) ;
   end (* UNSTACK_STATEMENT *) ;



procedure WORK_NORMAL_STMT ( var OUTF : TEXT ; ZKOMM1 : CHAR ( 6 ) ;
                           var COBSTMT : STRING ) ;

//***************************
// work on normal statement  
// check indentation etc.    
//***************************


   var WORD : STRING ( 70 ) ;
       POS_WORD : INTEGER ;
       POS_BLANK : INTEGER ;
       PERFORM_FOUND : BOOLEAN ;
       FIRST : BOOLEAN ;
       COBWORD : BOOLEAN ;
       COBTEIL1 : STRING ( 70 ) ;
       ZCNEU : CHAR ( 72 ) ;
       ZCNEU2 : CHAR ( 72 ) ;
       PUNKT : BOOLEAN ;
       ICW : INTEGER ;


   procedure CHECK_WORD_2 ;

   //***********************************************
   // handle statement starters                     
   // modification of indent is deferred from       
   // prior statements ... is to be executed only   
   // on statement starters                         
   //***********************************************


      begin (* CHECK_WORD_2 *)
        if FALSE then
          begin
            WRITELN ( OUTF , '** check_word2 - word  = ' , WORD ) ;
            WRITE ( OUTF , '** check_word2 - level  = ' , GS .
                    STMT_LEVEL : 1 ) ;
            for I := 1 to GS . STMT_LEVEL do
              WRITE ( OUTF , GS . STMT_STACK [ I ] : 2 ) ;
            WRITELN ( OUTF ) ;
          end (* then *) ;
        if COBWORD then
          begin
            MODIFY_INDENT ;
            if FALSE then
              if ( GS . STMT_TYPE = 'I' ) and not GS . THEN_FOUND then
                begin
                  DO_OUTPUT ( OUTF , '           THEN' , ' ' , 0 ,
                              FALSE ) ;
                end (* then *) ;
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
              GS . INDENT_INCR := 5 ;
            if WORD = 'END-EXEC' then
              if GS . INDENT > 0 then
                GS . INDENT := GS . INDENT - 5 ;

        //******************************
        // handle if                    
        //******************************

            if WORD = 'IF' then
              begin
                GS . INDENT_INCR := 3 ;
                GS . NEW_STATEMENT := 'I' ;
              end (* then *) ;
            if WORD = 'EVALUATE' then
              begin
                GS . INDENT_INCR := 3 ;
                GS . NEW_STATEMENT := 'V' ;
              end (* then *) ;

        //******************************
        // handle perform               
        //******************************

            if WORD = 'PERFORM' then
              PERFORM_FOUND := TRUE ;

        //******************************
        // indentation of else keyword  
        //******************************

            if WORD = 'ELSE' then
              begin
                UNSTACK_STATEMENT ( 'I' ) ;
                GS . INDENT_INCR := 3 ;
                GS . NEW_STATEMENT := 'E' ;
              end (* then *) ;

        //***********************************************
        // reduce indentation on end-if and end-perform  
        //***********************************************

            if WORD = 'END-IF' then
              UNSTACK_STATEMENT ( 'IE' ) ;
            if WORD = 'END-PERFORM' then
              UNSTACK_STATEMENT ( 'P' ) ;
            if WORD = 'END-EVALUATE' then
              UNSTACK_STATEMENT ( 'V' ) ;
          end (* then *)
        else

        //***********************************************
        // handle words (not statement starters)         
        // specific to certain statements                
        // P = perform                                   
        // E = evaluate                                  
        // I = if                                        
        // X = I/O statements                            
        //***********************************************

          case GS . STMT_TYPE of
            'P' : begin
                    if PERFORM_FOUND then
                      begin
                        for ICW := 1 to CWANZ do
                          if ( RTRIM ( CWTAB [ ICW ] . ID ) = WORD )
                          and ( CWTAB [ ICW ] . CLASS = 'P' ) then
                            begin
                              GS . INDENT_INCR := 3 ;
                              GS . NEW_STATEMENT := 'P' ;
                              break
                            end (* then *) ;
                      end (* then *) ;
                    PERFORM_FOUND := FALSE ;
                  end (* tag/ca *) ;
            'E' : begin

        //******************************
        // indentation of when keyword  
        //******************************

                    if WORD = 'WHEN' then
                      begin
                        if GS . STMT_STACK [ GS . STMT_LEVEL ] = 'W'
                        then
                          UNSTACK_STATEMENT ( 'W' ) ;
                        GS . INDENT_INCR := 3 ;
                        GS . NEW_STATEMENT := 'W' ;
                      end (* then *) ;
                  end (* tag/ca *) ;
            'I' : begin

        //******************************
        // indentation of then keyword  
        //******************************

                    if WORD = 'THEN' then
                      begin
                        if GS . INDENT > 0 then
                          GS . INDENT := GS . INDENT - 3 ;
                        GS . INDENT_INCR := 3 ;
                        GS . THEN_FOUND := TRUE ;
                      end (* then *) ;
                  end (* tag/ca *) ;
            'X' : begin
                    
                  end (* tag/ca *) ;
            otherwise
              begin
                
              end (* otherw *)
          end (* case *) ;

        //*******************************************
        // indentation is zero when period is found  
        //*******************************************

        if PUNKT then
          begin
            GS . INDENT_ZERO := TRUE ;
            GS . STMT_LEVEL := 0 ;
          end (* then *) ;
      end (* CHECK_WORD_2 *) ;


   procedure FORCE_NEWLINE ;

      begin (* FORCE_NEWLINE *)
        if FALSE then
          WRITELN ( '1: ' , ZKOMM1 , COBSTMT ) ;

        //****************************************************
        // check if output fits into current line             
        // or if additional line is needed                    
        //****************************************************

        CHECK_OUTPUT_B ( ZKOMM1 , GS . STMT_TEMP , ZCNEU , ZCNEU2 ) ;
        DO_OUTPUT ( OUTF , ZCNEU , ZCNEU2 , 0 , FALSE ) ;
        MODIFY_INDENT ;
      end (* FORCE_NEWLINE *) ;


   procedure ADD_TO_LINE ;

      begin (* ADD_TO_LINE *)
        GS . STMT_TEMP := GS . STMT_TEMP || ' ' || WORD ;
      end (* ADD_TO_LINE *) ;


   procedure WORK_IF ;

      begin (* WORK_IF *)
        if POS_BLANK <> 0 then
          FORCE_NEWLINE ;
      end (* WORK_IF *) ;


   procedure WORK_READ ;

      var LF : BOOLEAN ;

      begin (* WORK_READ *)
        WRITELN ( 'read: status   = ' , GS . STMT_STATUS ) ;
        WRITELN ( 'read: stmt (1) = <' , GS . STMT_TEMP , '>' ) ;
        WRITELN ( 'read: cobstmt  = <' , COBSTMT , '>' ) ;
        WRITELN ( 'read: word     = <' , WORD , '>' ) ;
        WRITELN ( 'read: pos_word = ' , POS_WORD ) ;
        WRITELN ( '-------------------------------------------' ) ;
        LF := FALSE ;
        case GS . STMT_STATUS of
          0 : begin
                GS . STMT_TEMP := SUBSTR ( COBSTMT , 1 , POS_WORD +
                                  LENGTH ( WORD ) ) ;
                if WORD = 'READ' then
                  GS . STMT_STATUS := 1
              end (* tag/ca *) ;
          1 : if WORD = 'AT' then
                begin
                  GS . INDENT_INCR := 3 ;
                  GS . STMT_STATUS := 2 ;
                  LF := TRUE
                end (* then *)
              else
                if WORD = 'INVALID' then
                  begin
                    GS . INDENT_INCR := 3 ;
                    GS . STMT_STATUS := 2 ;
                    LF := TRUE
                  end (* then *) ;
          otherwise
            GS . INDENT_INCR := 3 ;
        end (* case *) ;
        if LF then
          FORCE_NEWLINE
        else
          ADD_TO_LINE ;
        if LENGTH ( COBSTMT ) > LENGTH ( WORD ) then
          begin
            COBSTMT := SUBSTR ( COBSTMT , POS_WORD + LENGTH ( WORD ) +
                       1 ) ;
            POS_WORD := 1
          end (* then *)
        else
          begin
            COBSTMT := '' ;
            POS_BLANK := 0
          end (* else *) ;
        WRITELN ( 'read: cobstmt  = <' , COBSTMT , '>' ) ;
        WRITELN ( 'read: word     = <' , WORD , '>' ) ;
        WRITELN ( 'read: pos_word = ' , POS_WORD ) ;
        WRITELN ( 'read: status   = ' , GS . STMT_STATUS ) ;
        WRITELN ( 'read: stmt (2) = <' , GS . STMT_TEMP , '>' ) ;
        WRITELN ( '===========================================' ) ;
      end (* WORK_READ *) ;


   begin (* WORK_NORMAL_STMT *)

     //*****************************************
     // some global variables set from here     
     //*****************************************

     PERFORM_FOUND := FALSE ;

     //*****************************************
     // check if indentation has to be changed  
     // after this statement                    
     //*****************************************

     if FALSE then
       WRITELN ( '1: ' , ZKOMM1 , COBSTMT ) ;
     POS_WORD := 1 ;
     FIRST := TRUE ;
     while TRUE do
       begin
         if FALSE then
           begin
             WRITELN ( 'special  = <' , GS . STMT_SPECIAL , '>' ) ;
             WRITELN ( 'cobstmt  = <' , COBSTMT , '>' ) ;
             WRITELN ( 'pos_word = ' , POS_WORD : 1 ) ;
           end (* then *) ;
         POS_BLANK := NEXT_BLANK ( COBSTMT , POS_WORD ) ;
         if POS_BLANK = 0 then
           WORD := SUBSTR ( COBSTMT , POS_WORD )
         else
           WORD := SUBSTR ( COBSTMT , POS_WORD , POS_BLANK - POS_WORD )
                   ;
         if FALSE then
           WRITELN ( 'word     = <' , WORD , '>' ) ;

     //****************************************************
     // check1 checks for period and statement starters    
     // if statement starters (cobword) and not first      
     // in line, then the first part of the line is        
     // written, and the rest is processed                 
     //****************************************************

         GS . NEW_STATEMENT := ' ' ;
         CHECK_WORD_1 ( WORD , COBWORD , PUNKT , ICW , GS .
                        STMT_SPECIAL ) ;

     //****************************************************
     // work on certain statements                         
     // for example READ and IF                            
     //****************************************************

         if COBWORD then
           begin
             GS . STMT_STATUS := 0 ;
             GS . STMT_TEMP := '' ;
             case CWTAB [ ICW ] . TAGS [ 2 ] of
               'I' : GS . STMT_SPECIAL := ' ' ;
               'R' : GS . STMT_SPECIAL := 'R' ;
               otherwise
                 GS . STMT_SPECIAL := ' ' ;
             end (* case *)
           end (* then *) ;

     //****************************************************
     // do the default work                                
     //****************************************************

         if COBWORD and not FIRST then
           begin
             COBTEIL1 := SUBSTR ( COBSTMT , 1 , POS_WORD - 1 ) ;

     //****************************************************
     // check if output fits into current line             
     // or if additional line is needed                    
     //****************************************************

             CHECK_OUTPUT_B ( ZKOMM1 , COBTEIL1 , ZCNEU , ZCNEU2 ) ;
             DO_OUTPUT ( OUTF , ZCNEU , ZCNEU2 , 0 , FALSE ) ;
             MODIFY_INDENT ;
             COBSTMT := SUBSTR ( COBSTMT , POS_WORD ) ;
             if FALSE then
               WRITELN ( '2: ' , ZKOMM1 , COBSTMT ) ;
             PERFORM_FOUND := FALSE ;
             FIRST := TRUE ;
             POS_WORD := 1 ;
           end (* then *)
         else
           begin
             case GS . STMT_SPECIAL of
               'I' : WORK_IF ;
               'R' : WORK_READ ;
               otherwise
                 begin

     //****************************************************
     // check2 checks what sort of statement               
     // especially if the statement must be stacked,       
     // for example IF, EVALUATE, PERFORM etc.             
     //****************************************************

                   CHECK_WORD_2 ;
                   STACK_STATEMENT ;
                   FIRST := FALSE ;
                   if POS_BLANK <> 0 then
                     POS_WORD := POS_BLANK + 1
                 end (* otherw *)
             end (* case *) ;
             if POS_BLANK = 0 then
               break ;
           end (* else *)
       end (* while *) ;
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
       WRITELN ( NSOURCE , '*fdlines: div = ' , DIV_FLAG , ' sect = ' ,
                 SECT_FLAG , ' actlevel = ' , ACTLEVEL ) ;
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



procedure WORK_FILES ( ZKOMM1 : CHAR ( 6 ) ; const COBSTMT : STRING ;
                     var IS_SELECT : BOOLEAN ) ;

//***********************************
// Bearbeite Zeilen innerhalb FILES  
// SELECT speziell                   
//***********************************


   begin (* WORK_FILES *)
     if FALSE then
       WRITELN ( ZKOMM1 , COBSTMT ) ;
     IS_SELECT := LEFT ( COBSTMT , 7 ) = 'SELECT ' ;
   end (* WORK_FILES *) ;



procedure WORK_OTHER_SECTIONS ( ZKOMM1 : CHAR ( 6 ) ; const COBSTMT :
                              STRING ) ;

//***********************************************************
// Bearbeite SECTIONs ausserhalb von DATA und PROC DIVISION  
//***********************************************************


   begin (* WORK_OTHER_SECTIONS *)
     if FALSE then
       WRITELN ( ZKOMM1 , COBSTMT ) ;
   end (* WORK_OTHER_SECTIONS *) ;



procedure WORK_SPECIAL ( ZKOMM1 : CHAR ( 6 ) ; const COBSTMT : STRING )
                       ;

//**************************
// Bearbeite SPECIAL-NAMES  
//**************************


   begin (* WORK_SPECIAL *)
     if FALSE then
       WRITELN ( ZKOMM1 , COBSTMT ) ;
   end (* WORK_SPECIAL *) ;



procedure WORK_FILE_CONTROL ( ZKOMM1 : CHAR ( 6 ) ; const COBSTMT :
                            STRING ) ;

//*************************
// Bearbeite FILE-CONTROL  
//*************************


   begin (* WORK_FILE_CONTROL *)
     if FALSE then
       WRITELN ( ZKOMM1 , COBSTMT ) ;
   end (* WORK_FILE_CONTROL *) ;



procedure WORK_PROGRAM_ID ( ZKOMM1 : CHAR ( 6 ) ; const COBSTMT :
                          STRING ) ;

//***********************
// Bearbeite PROGRAM-ID  
//***********************


   begin (* WORK_PROGRAM_ID *)
     if FALSE then
       WRITELN ( ZKOMM1 , COBSTMT ) ;
   end (* WORK_PROGRAM_ID *) ;



procedure WORK_UNDEF ( ZKOMM1 : CHAR ( 6 ) ; const COBSTMT : STRING ) ;

   begin (* WORK_UNDEF *)
     if FALSE then
       WRITELN ( ZKOMM1 , COBSTMT ) ;
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


   var ZKOMM1 : CHAR ( 6 ) ;
       COBSTMT : STRING ( 70 ) ;
       XPOS0 : INTEGER ;
       XPOS1 : INTEGER ;
       XPOS2 : INTEGER ;
       XLEVEL : INTEGER ;

   begin (* WORK_ZONEA *)
     BLANK_LINES := 0 ;
     ZKOMM1 := LEFT ( ZC , 6 ) ;
     COBSTMT := COMP_COBOL ( SUBSTR ( ZC , 8 ) ) ;
     case DIV_FLAG of

     //*****************************************
     // generate content in procedure division  
     //*****************************************

       'P' : WORK_PARAGRAPH_SECT ( ZKOMM1 , COBSTMT ) ;

     //********************************************************
     // generate content in data division                      
     // most interesting: lines which start with level number  
     //********************************************************

       'D' : begin
               if GS . PUNKT_GEF and ( COBSTMT [ 1 ] in [ '0' .. '9' ]
               ) then
                 begin
                   WORK_LEVEL ( ZKOMM1 , COBSTMT , XLEVEL , XPOS0 ,
                                XPOS1 , XPOS2 , GS . PUNKT_GEF ) ;
                   MODIFY_LEVEL ( 'A' , COBSTMT , XLEVEL , XPOS0 ,
                                  XPOS1 , XPOS2 ) ;
                 end (* then *)
               else
                 if LEFT ( COBSTMT , 3 ) = 'FD ' then
                   begin
                     WORK_FD ( ZKOMM1 , COBSTMT ) ;
                     COBSTMT := SUBSTR ( COBSTMT , 4 ) ;
                     COBSTMT := TRIM ( COBSTMT ) ;
                     COBSTMT := 'FD  ' || COBSTMT ;
                     GS . PUNKT_GEF := TRUE ;
                   end (* then *)
                 else
                   if INDEX ( COBSTMT , 'SECTION' ) <> 0 then
                     begin
                       WORK_DATA_SECTION ( ZKOMM1 , COBSTMT ) ;
                       BLANK_LINES := 1 ;
                       GS . PUNKT_GEF := TRUE ;
                     end (* then *)
                   else
                     if ( INDEX ( COBSTMT , 'DIVISION' ) <> 0 ) and (
                     LEFT ( COBSTMT , 4 ) = 'PROC' ) then
                       begin
                         ACTLEVEL := 0 ;
                         COBSTMT := 'PROCEDURE DIVISION.' ;
                         DIV_FLAG := 'P' ;
                         SECT_FLAG := ' ' ;
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
                   DIV_FLAG := 'I' ;
                 end (* then *)
               else
                 if LEFT ( COBSTMT , 3 ) = 'ENV' then
                   begin
                     COBSTMT := 'ENVIRONMENT DIVISION.' ;
                     DIV_FLAG := 'I' ;
                   end (* then *)
                 else
                   if LEFT ( COBSTMT , 4 ) = 'DATA' then
                     begin
                       COBSTMT := 'DATA DIVISION.' ;
                       DIV_FLAG := 'D' ;
                       GS . PUNKT_GEF := TRUE ;
                     end (* then *)
                   else
                     if LEFT ( COBSTMT , 4 ) = 'PROC' then
                       begin
                         COBSTMT := 'PROCEDURE DIVISION.' ;
                         DIV_FLAG := 'P' ;
                         SECT_FLAG := ' '
                       end (* then *)
             end (* then *)
           else
             if INDEX ( COBSTMT , 'SECTION' ) <> 0 then
               begin
                 WORK_OTHER_SECTIONS ( ZKOMM1 , COBSTMT ) ;
                 BLANK_LINES := 1
               end (* then *)
             else
               if INDEX ( COBSTMT , 'SPECIAL-NAMES' ) <> 0 then
                 WORK_SPECIAL ( ZKOMM1 , COBSTMT )
               else
                 if INDEX ( COBSTMT , 'FILE-CONTROL' ) <> 0 then
                   WORK_FILE_CONTROL ( ZKOMM1 , COBSTMT )
                 else
                   if INDEX ( COBSTMT , 'PROGRAM-ID' ) <> 0 then
                     WORK_PROGRAM_ID ( ZKOMM1 , COBSTMT )
                   else
                     WORK_UNDEF ( ZKOMM1 , COBSTMT )
         end (* otherw *)
     end (* case *) ;

     //****************************************************
     // check if output fits into current line             
     // or if additional line is needed                    
     //****************************************************

     ZCNEU2 := ' ' ;
     CHECK_OUTPUTLINE ( 'A' , ZKOMM1 , COBSTMT , GS . INDENT , ZCNEU ,
                        ZCNEU2 ) ;
   end (* WORK_ZONEA *) ;



procedure WORK_ZONEB ( var OUTF : TEXT ;              // Ausgabedatei
                     var ZCNEU : CHAR ( 72 ) ;        // ergebn.zeile 1
                     var ZCNEU2 : CHAR ( 72 ) ;       // ergebn.zeile 2
                     ZC : CHAR ( 72 ) ;               // eingabezeile
                     var BLANK_LINES : INTEGER ) ;    // leerzeilen

//******************************************
// Bearbeite Zeile mit Zone B Content       
//******************************************


   var ZKOMM1 : CHAR ( 6 ) ;
       COBSTMT : STRING ( 70 ) ;
       IS_SELECT : BOOLEAN ;
       XPOS0 : INTEGER ;
       XPOS1 : INTEGER ;
       XPOS2 : INTEGER ;
       XLEVEL : INTEGER ;

   begin (* WORK_ZONEB *)
     BLANK_LINES := 0 ;
     ZKOMM1 := LEFT ( ZC , 6 ) ;
     COBSTMT := COMP_COBOL ( SUBSTR ( ZC , 12 ) ) ;
     case DIV_FLAG of

     //*****************************************
     // generate content in procedure division  
     //*****************************************

       'P' : begin
               WORK_NORMAL_STMT ( OUTF , ZKOMM1 , COBSTMT ) ;
             end (* tag/ca *) ;

     //********************************************************
     // generate content in data division                      
     // most interesting: lines which start with level number  
     //********************************************************

       'D' : begin
               if GS . PUNKT_GEF and ( COBSTMT [ 1 ] in [ '0' .. '9' ]
               ) then
                 begin
                   WORK_LEVEL ( ZKOMM1 , COBSTMT , XLEVEL , XPOS0 ,
                                XPOS1 , XPOS2 , GS . PUNKT_GEF ) ;
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
           WORK_FILES ( ZKOMM1 , COBSTMT , IS_SELECT ) ;
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

     CHECK_OUTPUT_B ( ZKOMM1 , COBSTMT , ZCNEU , ZCNEU2 ) ;
   end (* WORK_ZONEB *) ;



procedure WORK ( OUTF : TEXT ; const ZEILE : STRING ) ;

   var ZC : CHAR ( 72 ) ;
       ZCNEU : CHAR ( 72 ) ;
       ZCNEU2 : CHAR ( 72 ) ;
       DONE : BOOLEAN ;
       I : INTEGER ;
       BL : INTEGER ;

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
           ZCNEU := ZC ;
           OUT_SRCLINE ( OUTF , ZCNEU ) ;
           DONE := TRUE
         end (* then *) ;

     //********************************************
     // handle cobol card with zone b content      
     //********************************************

     if not DONE then
       begin
         GS . INDENT_EJECT := FALSE ;
         WORK_ZONEB ( OUTF , ZCNEU , ZCNEU2 , ZC , BL ) ;
         DO_OUTPUT ( OUTF , ZCNEU , ZCNEU2 , BL , GS . INDENT_EJECT ) ;
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
  DIV_FLAG := ' ' ;
  SECT_FLAG := ' ' ;
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
  GS . INDENT := 0 ;
  GS . INDENT_INCR := 0 ;
  GS . INDENT_ZERO := FALSE ;
  GS . INDENT_EJECT := FALSE ;
  GS . STMT_TYPE := ' ' ;
  GS . STMT_SPECIAL := ' ' ;
  GS . STMT_LEVEL := 0 ;
  GS . STMT_STATUS := 0 ;
  GS . STMT_TEMP := '' ;
  GS . NEW_STATEMENT := ' ' ;
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
