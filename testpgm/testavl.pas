program TESTAVL ( OUTPUT ) ;

//**********************************************************************
// test program to test avltree library module                          
//**********************************************************************
//$X+                                                                   
//**********************************************************************



type CACHEPTR = -> CACHEREC ;
     CACHEREC = record
                  MAGIC : CHAR ( 8 ) ;     // always 'AVLCACHE'
                  CNAME : CHAR ( 8 ) ;     // Name of Cache
                  COUNT : INTEGER ;        // Nbr of entries
                  PTREE : VOIDPTR ;        // ptr to tree
                end ;


var I : INTEGER ;
    RC : INTEGER ;
    PBAUM : VOIDPTR ;
    PELEMENT : VOIDPTR ;
    PLAUF : VOIDPTR ;
    HCHANGED : BOOLEAN ;
    GEFUNDEN : BOOLEAN ;
    VVEKTOR : array [ 1 .. 20 ] of INTEGER ;
    KEYLEN : INTEGER ;
    POBJ : VOIDPTR ;
    OBJLEN : INTEGER ;
    POBJLEN : -> INTEGER ;
    PINT : -> INTEGER ;
    S9 : STRING ( 9 ) ;
    PS9 : -> STRING ( 9 ) ;
    SEQKEY : VOIDPTR ;
    PKEY : VOIDPTR ;
    LKEY : INTEGER ;
    PDAT : VOIDPTR ;
    LDAT : INTEGER ;
    CACHENAME : CHAR ( 8 ) ;
    KEY : CHAR ( 8 ) ;
    DAT : CHAR ( 12 ) ;
    PCACHE : CACHEPTR ;
    PC12 : -> CHAR ( 12 ) ;
    DUMMYP : VOIDPTR ;
    DUMMYI : INTEGER ;



function AVLSRCH ( SKEY : VOIDPTR ;           // ptr to key
                 SKEYLEN : INTEGER ;          // keylen (if deep copy)
                 var POBJ : VOIDPTR ;         // ptr to obj ptr
                 var POBJLEN : VOIDPTR ;      // ptr to objlen field
                 var GEFUNDEN : BOOLEAN ;     // true if found
                 var PP : VOIDPTR ;           // tree pointer
                 var HCHANGED : BOOLEAN ;     // height changed
                 EINFUEGEN : BOOLEAN ;        // if insert then true
                 function AVLCOMP ( X1 : VOIDPTR ; L1 : INTEGER ; X2 :
                 VOIDPTR ; L2 : INTEGER ) : INTEGER ) : VOIDPTR ;

   EXTERNAL ;



function AVLGET ( MODUS : CHAR ;            // mode = F(irst), N(ext)
                START : VOIDPTR ;           // starting position
                var RESULT : VOIDPTR ;      // new position
                var PKEY : VOIDPTR ;        // pointer to key
                var KEYLEN : INTEGER ;      // keylen
                var POBJ : VOIDPTR ;        // pointer to obj
                var OBJLEN : INTEGER )      // objlen
                : INTEGER ;                 // zero, if OK

   EXTERNAL ;



procedure AVLPRINT ( P : VOIDPTR ;            // tree to print
                   var AUSGFILE : TEXT ;      // output file
                   EINRUECK : INTEGER ;       // indentation count
                   PV : VOIDPTR ;             // nil on top level call
                   RICHTUNG : CHAR ;          // blank on top level call
                   procedure AVLPKEY ( var F : TEXT ; P : VOIDPTR ) ) ;

   EXTERNAL ;



procedure AVLFREE ( P : VOIDPTR ) ;

   EXTERNAL ;



function AVLCACHE ( FUNKCODE : CHAR ( 8 ) ;   // Funktionscode
                  PHANDLE : VOIDPTR ;         // Cachehandle
                  var SEQKEY : VOIDPTR ;      // seq. Keyposition,
                  var PKEY : VOIDPTR ;        // Zeiger auf Key
                  var LKEY : INTEGER ;        // Laenge Key
                  var PDAT : VOIDPTR ;        // Zeiger auf Daten
                  var LDAT : INTEGER )        // Laenge Daten
                  : INTEGER ;

   EXTERNAL ;



function COMPARE ( X1 : VOIDPTR ; L1 : INTEGER ; X2 : VOIDPTR ; L2 :
                 INTEGER ) : INTEGER ;

//**********************************************************************
// compare function for AVL tree key values (here: integer keys)        
// this function is passed as a parameter to avlsrch                    
//**********************************************************************


   var I1 : INTEGER ;
       IP1 : -> INTEGER ;
       I2 : INTEGER ;
       IP2 : -> INTEGER ;

   begin (* COMPARE *)
     IP1 := X1 ;
     IP2 := X2 ;
     I1 := IP1 -> ;
     I2 := IP2 -> ;
     if I1 > I2 then
       COMPARE := 1
     else
       if I1 < I2 then
         COMPARE := - 1
       else
         COMPARE := 0 ;
   end (* COMPARE *) ;



procedure PRINTELEM ( var F : TEXT ; X : VOIDPTR ) ;

//**********************************************************************
// print key value                                                      
// this function is passed as a parameter to avlprint                   
//**********************************************************************


   var I1 : INTEGER ;
       IP1 : -> INTEGER ;

   begin (* PRINTELEM *)
     IP1 := X ;
     I1 := IP1 -> ;
     WRITE ( F , I1 : 8 ) ;
   end (* PRINTELEM *) ;



function COMPARE_S9 ( X1 : VOIDPTR ; L1 : INTEGER ; X2 : VOIDPTR ; L2 :
                    INTEGER ) : INTEGER ;

//**********************************************************************
// compare function for AVL tree key values (here: string(9) keys)      
// this function is passed as a parameter to avlsrch                    
//**********************************************************************


   var S1 : STRING ( 9 ) ;
       SP1 : -> STRING ( 9 ) ;
       S2 : STRING ( 9 ) ;
       SP2 : -> STRING ( 9 ) ;

   begin (* COMPARE_S9 *)
     SP1 := X1 ;
     SP2 := X2 ;
     S1 := SP1 -> ;
     S2 := SP2 -> ;
     if S1 > S2 then
       COMPARE_S9 := 1
     else
       if S1 < S2 then
         COMPARE_S9 := - 1
       else
         COMPARE_S9 := 0 ;
   end (* COMPARE_S9 *) ;



procedure PRINT_S9 ( var F : TEXT ; X : VOIDPTR ) ;

//**********************************************************************
// print key value                                                      
// this function is passed as a parameter to avlprint                   
//**********************************************************************


   var S1 : STRING ( 9 ) ;
       SP1 : -> STRING ( 9 ) ;

   begin (* PRINT_S9 *)
     SP1 := X ;
     S1 := SP1 -> ;
     WRITE ( F , S1 : 10 ) ;
   end (* PRINT_S9 *) ;



procedure INSERT_WORD ;

   type TPINT = -> INTEGER ;
        TPPINT = -> TPINT ;

   var PPINT : TPPINT ;
       PINT : TPINT ;

   begin (* INSERT_WORD *)
     HCHANGED := FALSE ;
     PELEMENT := AVLSRCH ( ADDR ( S9 ) , SIZEOF ( S9 ) , POBJ , POBJLEN
                 , GEFUNDEN , PBAUM , HCHANGED , TRUE , COMPARE_S9 ) ;
     if GEFUNDEN then
       begin
         PPINT := POBJ ;
         PINT := PPINT -> ;
         PINT -> := PINT -> + 1 ;
       end (* then *)
     else
       begin
         PINT := ALLOC ( SIZEOF ( INTEGER ) ) ;
         PINT -> := 1 ;
         PPINT := POBJ ;
         PPINT -> := PINT ;
         PINT := POBJLEN ;
         PINT -> := SIZEOF ( INTEGER ) ;
       end (* else *)
   end (* INSERT_WORD *) ;



procedure TEST1 ;

   begin (* TEST1 *)
     VVEKTOR [ 1 ] := 3 ;
     VVEKTOR [ 2 ] := 5 ;
     VVEKTOR [ 3 ] := 7 ;
     VVEKTOR [ 4 ] := 12 ;
     VVEKTOR [ 5 ] := 34 ;
     VVEKTOR [ 6 ] := 1 ;
     VVEKTOR [ 7 ] := 6 ;
     VVEKTOR [ 8 ] := 23 ;
     VVEKTOR [ 9 ] := 78 ;
     VVEKTOR [ 10 ] := 45 ;
     VVEKTOR [ 11 ] := 32 ;
     VVEKTOR [ 12 ] := 89 ;
     VVEKTOR [ 13 ] := 11 ;
     VVEKTOR [ 14 ] := 102 ;
     VVEKTOR [ 15 ] := 2 ;
     VVEKTOR [ 16 ] := 77 ;
     VVEKTOR [ 17 ] := 4 ;
     VVEKTOR [ 18 ] := 66 ;
     VVEKTOR [ 19 ] := 44 ;
     VVEKTOR [ 20 ] := 99 ;

     //*****************************************************************
     //*                                                                
     // insert values into AVL tree                                     
     // only pointers are recorded in AVL tree, no deep copy            
     //*****************************************************************
     //*                                                                

     WRITELN ;
     WRITELN ( 'values are inserted into AVL tree' ) ;
     WRITELN ( 'using general procedure AVLSRCH' ) ;
     WRITELN ( '===================================================' )
               ;
     PBAUM := NIL ;
     for I := 1 to 20 do
       begin
         HCHANGED := FALSE ;
         PELEMENT := AVLSRCH ( ADDR ( VVEKTOR [ I ] ) , 0 , POBJ ,
                     POBJLEN , GEFUNDEN , PBAUM , HCHANGED , TRUE ,
                     COMPARE ) ;
       end (* for *) ;

     //*****************************************************************
     //*                                                                
     // print AVL tree using AVLPRINT                                   
     //*****************************************************************
     //*                                                                

     WRITELN ;
     WRITELN ( 'print AVL tree using general procedure AVLPRINT' ) ;
     WRITELN ( '===================================================' )
               ;
     AVLPRINT ( PBAUM , OUTPUT , 8 , NIL , ' ' , PRINTELEM ) ;

     //*****************************************************************
     //*                                                                
     // walk thru AVL tree and print values                             
     //*****************************************************************
     //*                                                                

     WRITELN ;
     WRITELN ( 'walk thru AVL tree using general procedure AVLGET' ) ;
     WRITELN ( '===================================================' )
               ;
     RC := AVLGET ( 'F' , PBAUM , PLAUF , PINT , KEYLEN , POBJ , OBJLEN
           ) ;
     while RC = 0 do
       begin
         WRITELN ( 'aus AVL-Baum: ' , PINT -> , '   Keylen: ' , KEYLEN
                   ) ;
         RC := AVLGET ( 'N' , PBAUM , PLAUF , PINT , KEYLEN , POBJ ,
               OBJLEN ) ;
       end (* while *) ;
     AVLFREE ( PBAUM ) ;

     //*****************************************************************
     //*                                                                
     // insert values into AVL tree                                     
     // now with deep copy                                              
     //*****************************************************************
     //*                                                                

     WRITELN ;
     WRITELN ( 'values are inserted into AVL tree' ) ;
     WRITELN ( 'using general procedure AVLSRCH' ) ;
     WRITELN ( '===================================================' )
               ;
     PBAUM := NIL ;
     for I := 1 to 20 do
       begin
         HCHANGED := FALSE ;
         PELEMENT := AVLSRCH ( ADDR ( VVEKTOR [ I ] ) , SIZEOF (
                     INTEGER ) , POBJ , POBJLEN , GEFUNDEN , PBAUM ,
                     HCHANGED , TRUE , COMPARE ) ;
       end (* for *) ;

     //*****************************************************************
     //*                                                                
     // print AVL tree using AVLPRINT                                   
     //*****************************************************************
     //*                                                                

     WRITELN ;
     WRITELN ( 'print AVL tree using general procedure AVLPRINT' ) ;
     WRITELN ( '===================================================' )
               ;
     AVLPRINT ( PBAUM , OUTPUT , 8 , NIL , ' ' , PRINTELEM ) ;

     //*****************************************************************
     //*                                                                
     // walk thru AVL tree and print values                             
     //*****************************************************************
     //*                                                                

     WRITELN ;
     WRITELN ( 'walk thru AVL tree using general procedure AVLGET' ) ;
     WRITELN ( '===================================================' )
               ;
     RC := AVLGET ( 'F' , PBAUM , PLAUF , PINT , KEYLEN , POBJ , OBJLEN
           ) ;
     while RC = 0 do
       begin
         WRITELN ( 'aus AVL-Baum: ' , PINT -> , '   Keylen: ' , KEYLEN
                   ) ;
         RC := AVLGET ( 'N' , PBAUM , PLAUF , PINT , KEYLEN , POBJ ,
               OBJLEN ) ;
       end (* while *) ;
     AVLFREE ( PBAUM ) ;
   end (* TEST1 *) ;



procedure TEST2 ;

   begin (* TEST2 *)

     //************************************************************
     // insert values into AVL tree                                
     // string (9) variables instead of integers                   
     //************************************************************
     //******                                                      
     // the same variable is used again and again to insert values 
     // into the AVL tree; this is only valid if deep copy is speci
     //fied                                                        
     // (the second parameter of AVLSRCH needs to be equal to the  
     // key size in this case)                                     
     //************************************************************
     //******                                                      
     // most interesting: the AVL functions work for different     
     // key types; the compare functions passed as parameter handle
     // the differences                                            
     //************************************************************
     //******                                                      
     //************************************************************

     WRITELN ;
     WRITELN ( 'values are inserted into AVL tree' ) ;
     WRITELN ( 'using general procedure AVLSRCH' ) ;
     WRITELN ( '===================================================' )
               ;
     PBAUM := NIL ;
     S9 := 'wovon' ;
     INSERT_WORD ;
     S9 := 'man' ;
     INSERT_WORD ;
     S9 := 'nicht' ;
     INSERT_WORD ;
     S9 := 'reden' ;
     INSERT_WORD ;
     S9 := 'kann' ;
     INSERT_WORD ;
     S9 := 'davon' ;
     INSERT_WORD ;
     S9 := 'muss' ;
     INSERT_WORD ;
     S9 := 'man' ;
     INSERT_WORD ;
     S9 := 'schweigen' ;
     INSERT_WORD ;

     //*****************************************************************
     //*                                                                
     // print AVL tree using AVLPRINT                                   
     //*****************************************************************
     //*                                                                

     WRITELN ;
     WRITELN ( 'print AVL tree using general procedure AVLPRINT' ) ;
     WRITELN ( '===================================================' )
               ;
     AVLPRINT ( PBAUM , OUTPUT , 10 , NIL , ' ' , PRINT_S9 ) ;

     //*****************************************************************
     //*                                                                
     // walk thru AVL tree and print values                             
     //*****************************************************************
     //*                                                                

     WRITELN ;
     WRITELN ( 'walk thru AVL tree using general procedure AVLGET' ) ;
     WRITELN ( '===================================================' )
               ;
     PLAUF := NIL ;
     RC := AVLGET ( 'N' , PBAUM , PLAUF , PS9 , KEYLEN , POBJ , OBJLEN
           ) ;
     while RC = 0 do
       begin
         PINT := POBJ ;
         WRITELN ( 'aus AVL-Baum: ' , PS9 -> : 9 , '   Keylen: ' ,
                   KEYLEN , '   Obj: ' , PINT -> ) ;
         RC := AVLGET ( 'N' , PBAUM , PLAUF , PS9 , KEYLEN , POBJ ,
               OBJLEN ) ;
       end (* while *) ;
     AVLFREE ( PBAUM ) ;
   end (* TEST2 *) ;



procedure TEST3 ;

   begin (* TEST3 *)

     //************************************************************
     // test cache create calls                                    
     //************************************************************
     //******                                                      
     //************************************************************

     WRITELN ;
     WRITELN ( 'Test AVLCACHE CREATE' ) ;
     WRITELN ( '===================================================' )
               ;
     PKEY := ADDR ( CACHENAME ) ;
     CACHENAME := 'CACHE1' ;
     RC := AVLCACHE ( 'CREATE' , NIL , SEQKEY , PKEY , LKEY , PDAT ,
           LDAT ) ;
     WRITELN ( 'RC          = ' , RC ) ;
     WRITELN ( 'PDAT        = ' , PDAT ) ;
     WRITELN ( 'LDAT        = ' , LDAT ) ;
     PCACHE := PDAT ;
     WRITELN ( 'CACHE.MAGIC = ' , PCACHE -> . MAGIC ) ;
     WRITELN ( 'CACHE.CNAME = ' , PCACHE -> . CNAME ) ;
     WRITELN ( 'CACHE.COUNT = ' , PCACHE -> . COUNT ) ;
     WRITELN ( 'CACHE.PTREE = ' , PCACHE -> . PTREE ) ;
     WRITELN ;
     WRITELN ( 'Test AVLCACHE CREATE' ) ;
     WRITELN ( '===================================================' )
               ;
     PKEY := ADDR ( CACHENAME ) ;
     CACHENAME := 'CACHE2' ;
     RC := AVLCACHE ( 'CREATE' , NIL , SEQKEY , PKEY , LKEY , PDAT ,
           LDAT ) ;
     WRITELN ( 'RC          = ' , RC ) ;
     WRITELN ( 'PDAT        = ' , PDAT ) ;
     WRITELN ( 'LDAT        = ' , LDAT ) ;
     PCACHE := PDAT ;
     WRITELN ( 'CACHE.MAGIC = ' , PCACHE -> . MAGIC ) ;
     WRITELN ( 'CACHE.CNAME = ' , PCACHE -> . CNAME ) ;
     WRITELN ( 'CACHE.COUNT = ' , PCACHE -> . COUNT ) ;
     WRITELN ( 'CACHE.PTREE = ' , PCACHE -> . PTREE ) ;
     WRITELN ;
     WRITELN ( 'Test AVLCACHE CREATE' ) ;
     WRITELN ( '===================================================' )
               ;
     PKEY := ADDR ( CACHENAME ) ;
     CACHENAME := 'CACHE1' ;
     RC := AVLCACHE ( 'CREATE' , NIL , SEQKEY , PKEY , LKEY , PDAT ,
           LDAT ) ;
     WRITELN ( 'RC          = ' , RC ) ;
     WRITELN ( 'PDAT        = ' , PDAT ) ;
     WRITELN ( 'LDAT        = ' , LDAT ) ;
     PCACHE := PDAT ;
     WRITELN ( 'CACHE.MAGIC = ' , PCACHE -> . MAGIC ) ;
     WRITELN ( 'CACHE.CNAME = ' , PCACHE -> . CNAME ) ;
     WRITELN ( 'CACHE.COUNT = ' , PCACHE -> . COUNT ) ;
     WRITELN ( 'CACHE.PTREE = ' , PCACHE -> . PTREE ) ;

     //*****************************************************************
     //*                                                                
     // test put calls                                                  
     //*****************************************************************
     //*                                                                

     WRITELN ;
     WRITELN ( 'Test AVLCACHE PUT' ) ;
     WRITELN ( '===================================================' )
               ;
     KEY := 'BERND' ;
     DAT := 'OPPOLZER' ;
     PKEY := ADDR ( KEY ) ;
     LKEY := 5 ;
     PDAT := ADDR ( DAT ) ;
     LDAT := 12 ;
     RC := AVLCACHE ( 'PUT' , PCACHE , SEQKEY , PKEY , LKEY , PDAT ,
           LDAT ) ;
     WRITELN ( 'RC          = ' , RC ) ;
     WRITELN ;
     WRITELN ( 'Test AVLCACHE PUT' ) ;
     WRITELN ( '===================================================' )
               ;
     KEY := 'BERND' ;
     DAT := 'OPPOLZER' ;
     PKEY := ADDR ( KEY ) ;
     LKEY := 5 ;
     PDAT := ADDR ( DAT ) ;
     LDAT := 12 ;
     RC := AVLCACHE ( 'PUT' , PCACHE , SEQKEY , PKEY , LKEY , PDAT ,
           LDAT ) ;
     WRITELN ( 'RC          = ' , RC ) ;
     WRITELN ;
     WRITELN ( 'Test AVLCACHE PUT' ) ;
     WRITELN ( '===================================================' )
               ;
     KEY := 'BERND2' ;
     DAT := 'OPPOLZER' ;
     PKEY := ADDR ( KEY ) ;
     LKEY := 6 ;
     PDAT := ADDR ( DAT ) ;
     LDAT := 12 ;
     RC := AVLCACHE ( 'PUT' , PCACHE , SEQKEY , PKEY , LKEY , PDAT ,
           LDAT ) ;
     WRITELN ( 'RC          = ' , RC ) ;
     WRITELN ;
     WRITELN ( 'Test AVLCACHE PUT' ) ;
     WRITELN ( '===================================================' )
               ;
     KEY := 'BERND1' ;
     DAT := 'Oppolzer' ;
     PKEY := ADDR ( KEY ) ;
     LKEY := 6 ;
     PDAT := ADDR ( DAT ) ;
     LDAT := 12 ;
     RC := AVLCACHE ( 'PUT' , PCACHE , SEQKEY , PKEY , LKEY , PDAT ,
           LDAT ) ;
     WRITELN ( 'RC          = ' , RC ) ;

     //*****************************************************************
     //*                                                                
     // nochmal test create                                             
     //*****************************************************************
     //*                                                                

     WRITELN ;
     WRITELN ( 'Test AVLCACHE CREATE' ) ;
     WRITELN ( '===================================================' )
               ;
     PKEY := ADDR ( CACHENAME ) ;
     CACHENAME := 'CACHE1' ;
     RC := AVLCACHE ( 'CREATE' , NIL , SEQKEY , PKEY , LKEY , PDAT ,
           LDAT ) ;
     WRITELN ( 'RC          = ' , RC ) ;
     WRITELN ( 'PDAT        = ' , PDAT ) ;
     WRITELN ( 'LDAT        = ' , LDAT ) ;
     PCACHE := PDAT ;
     WRITELN ( 'CACHE.MAGIC = ' , PCACHE -> . MAGIC ) ;
     WRITELN ( 'CACHE.CNAME = ' , PCACHE -> . CNAME ) ;
     WRITELN ( 'CACHE.COUNT = ' , PCACHE -> . COUNT ) ;
     WRITELN ( 'CACHE.PTREE = ' , PCACHE -> . PTREE ) ;

     //*****************************************************************
     //*                                                                
     // test get                                                        
     //*****************************************************************
     //*                                                                

     WRITELN ;
     WRITELN ( 'Test AVLCACHE GET' ) ;
     WRITELN ( '===================================================' )
               ;
     KEY := 'BERND2' ;
     PKEY := ADDR ( KEY ) ;
     LKEY := 6 ;
     PDAT := NIL ;
     LDAT := 0 ;
     RC := AVLCACHE ( 'GET' , PCACHE , SEQKEY , PKEY , LKEY , PDAT ,
           LDAT ) ;
     WRITELN ( 'RC          = ' , RC ) ;
     WRITELN ( 'PDAT        = ' , PDAT ) ;
     WRITELN ( 'LDAT        = ' , LDAT ) ;
     if RC = 0 then
       begin
         PC12 := PDAT ;
         WRITELN ( 'PC12        = ' , PC12 ) ;
         WRITELN ( 'PC12 ->     = ' , PC12 -> ) ;
       end (* then *) ;

     //*****************************************************************
     //*                                                                
     // test gfirst                                                     
     //*****************************************************************
     //*                                                                

     WRITELN ;
     WRITELN ( 'Test AVLCACHE GFIRST und GNEXT' ) ;
     WRITELN ( '===================================================' )
               ;
     PKEY := NIL ;
     LKEY := 0 ;
     PDAT := NIL ;
     LDAT := 0 ;
     SEQKEY := NIL ;
     RC := AVLCACHE ( 'GFIRST' , PCACHE , SEQKEY , PKEY , LKEY , PDAT ,
           LDAT ) ;
     WRITELN ( 'RC          = ' , RC ) ;
     WRITELN ( 'PKEY        = ' , PKEY ) ;
     WRITELN ( 'LKEY        = ' , LKEY ) ;
     WRITELN ( 'PDAT        = ' , PDAT ) ;
     WRITELN ( 'LDAT        = ' , LDAT ) ;
     WRITELN ( 'SEQKEY      = ' , SEQKEY ) ;
     if RC = 0 then
       begin
         PC12 := PKEY ;
         WRITELN ( 'PKEY ->     = ' , SUBSTR ( PC12 -> , 1 , LKEY ) ) ;
         PC12 := PDAT ;
         WRITELN ( 'PDAT ->     = ' , SUBSTR ( PC12 -> , 1 , LDAT ) ) ;
       end (* then *) ;
     while TRUE do
       begin
         PKEY := NIL ;
         LKEY := 0 ;
         PDAT := NIL ;
         LDAT := 0 ;
         RC := AVLCACHE ( 'GNEXT' , PCACHE , SEQKEY , PKEY , LKEY ,
               PDAT , LDAT ) ;
         WRITELN ( 'RC          = ' , RC ) ;
         WRITELN ( 'PKEY        = ' , PKEY ) ;
         WRITELN ( 'LKEY        = ' , LKEY ) ;
         WRITELN ( 'PDAT        = ' , PDAT ) ;
         WRITELN ( 'LDAT        = ' , LDAT ) ;
         WRITELN ( 'SEQKEY      = ' , SEQKEY ) ;
         if RC = 0 then
           begin
             PC12 := PKEY ;
             WRITELN ( 'PKEY ->     = ' , SUBSTR ( PC12 -> , 1 , LKEY )
                       ) ;
             PC12 := PDAT ;
             WRITELN ( 'PDAT ->     = ' , SUBSTR ( PC12 -> , 1 , LDAT )
                       ) ;
           end (* then *)
         else
           break
       end (* while *) ;
     WRITELN ;
     WRITELN ( 'Test AVLCACHE GNEXT only' ) ;
     WRITELN ( '===================================================' )
               ;
     SEQKEY := NIL ;
     while TRUE do
       begin
         PKEY := NIL ;
         LKEY := 0 ;
         PDAT := NIL ;
         LDAT := 0 ;
         RC := AVLCACHE ( 'GNEXT' , PCACHE , SEQKEY , PKEY , LKEY ,
               PDAT , LDAT ) ;
         WRITELN ( 'RC          = ' , RC ) ;
         WRITELN ( 'PKEY        = ' , PKEY ) ;
         WRITELN ( 'LKEY        = ' , LKEY ) ;
         WRITELN ( 'PDAT        = ' , PDAT ) ;
         WRITELN ( 'LDAT        = ' , LDAT ) ;
         WRITELN ( 'SEQKEY      = ' , SEQKEY ) ;
         if RC = 0 then
           begin
             PC12 := PKEY ;
             WRITELN ( 'PKEY ->     = ' , SUBSTR ( PC12 -> , 1 , LKEY )
                       ) ;
             PC12 := PDAT ;
             WRITELN ( 'PDAT ->     = ' , SUBSTR ( PC12 -> , 1 , LDAT )
                       ) ;
           end (* then *)
         else
           break
       end (* while *) ;
     WRITELN ;
     WRITELN ( 'Test AVLCACHE TRACE' ) ;
     WRITELN ( '===================================================' )
               ;
     PKEY := ADDR ( CACHENAME ) ;
     RC := AVLCACHE ( 'TRACE' , PCACHE , DUMMYP , PKEY , DUMMYI ,
           DUMMYP , DUMMYI ) ;

     //*****************************************************************
     //*                                                                
     // nochmal test create                                             
     //*****************************************************************
     //*                                                                

     WRITELN ;
     WRITELN ( 'Test AVLCACHE CREATE' ) ;
     WRITELN ( '===================================================' )
               ;
     PKEY := ADDR ( CACHENAME ) ;
     CACHENAME := 'CACHE2' ;
     RC := AVLCACHE ( 'CREATE' , NIL , SEQKEY , PKEY , LKEY , PDAT ,
           LDAT ) ;
     WRITELN ( 'RC          = ' , RC ) ;
     WRITELN ( 'PDAT        = ' , PDAT ) ;
     WRITELN ( 'LDAT        = ' , LDAT ) ;
     PCACHE := PDAT ;
     WRITELN ( 'CACHE.MAGIC = ' , PCACHE -> . MAGIC ) ;
     WRITELN ( 'CACHE.CNAME = ' , PCACHE -> . CNAME ) ;
     WRITELN ( 'CACHE.COUNT = ' , PCACHE -> . COUNT ) ;
     WRITELN ( 'CACHE.PTREE = ' , PCACHE -> . PTREE ) ;

     //*****************************************************************
     //*                                                                
     // test gfirst                                                     
     //*****************************************************************
     //*                                                                

     WRITELN ;
     WRITELN ( 'Test AVLCACHE GNEXT on empty cache' ) ;
     WRITELN ( '===================================================' )
               ;
     SEQKEY := NIL ;
     while TRUE do
       begin
         PKEY := NIL ;
         LKEY := 0 ;
         PDAT := NIL ;
         LDAT := 0 ;
         RC := AVLCACHE ( 'GNEXT' , PCACHE , SEQKEY , PKEY , LKEY ,
               PDAT , LDAT ) ;
         WRITELN ( 'RC          = ' , RC ) ;
         WRITELN ( 'PKEY        = ' , PKEY ) ;
         WRITELN ( 'LKEY        = ' , LKEY ) ;
         WRITELN ( 'PDAT        = ' , PDAT ) ;
         WRITELN ( 'LDAT        = ' , LDAT ) ;
         WRITELN ( 'SEQKEY      = ' , SEQKEY ) ;
         if RC = 0 then
           begin
             PC12 := PKEY ;
             WRITELN ( 'PKEY ->     = ' , SUBSTR ( PC12 -> , 1 , LKEY )
                       ) ;
             PC12 := PDAT ;
             WRITELN ( 'PDAT ->     = ' , SUBSTR ( PC12 -> , 1 , LDAT )
                       ) ;
           end (* then *)
         else
           break
       end (* while *) ;
   end (* TEST3 *) ;



begin (* HAUPTPROGRAMM *)
  TEST1 ;
  TEST2 ;
  TEST3
end (* HAUPTPROGRAMM *) .
