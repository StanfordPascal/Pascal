program TESTAVL ( OUTPUT ) ;

//**********************************************************************
// test program to test avltree library module                          
//**********************************************************************
//$X+                                                                   
//**********************************************************************



var I : INTEGER ;
    RC : INTEGER ;
    PBAUM : VOIDPTR ;
    PELEMENT : VOIDPTR ;
    PLAUF : VOIDPTR ;
    HCHANGED : BOOLEAN ;
    VVEKTOR : array [ 1 .. 20 ] of INTEGER ;
    POBJ : VOIDPTR ;
    PINT : -> INTEGER ;
    S9 : STRING ( 9 ) ;
    PS9 : -> STRING ( 9 ) ;



function AVLSRCH ( SKEY : VOIDPTR ; SKEYLEN : INTEGER ; function
                 AVLCOMP ( X1 , X2 : VOIDPTR ) : INTEGER ; var PP :
                 VOIDPTR ; var HCHANGED : BOOLEAN ; EINFUEGEN : BOOLEAN
                 ) : VOIDPTR ;

   EXTERNAL ;



function AVLGET ( MODUS : CHAR ; START : VOIDPTR ; var RESULT : VOIDPTR
                ; var PKEY : VOIDPTR ; var POBJ : VOIDPTR ) : INTEGER ;

   EXTERNAL ;



procedure AVLPRINT ( P : VOIDPTR ; var AUSGFILE : TEXT ; EINRUECK :
                   INTEGER ; procedure AVLPKEY ( var F : TEXT ; P :
                   VOIDPTR ) ; PV : VOIDPTR ; RICHTUNG : CHAR ) ;

   EXTERNAL ;



procedure AVLFREE ( P : VOIDPTR ) ;

   EXTERNAL ;



function COMPARE ( X1 , X2 : VOIDPTR ) : INTEGER ;

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



function COMPARE_S9 ( X1 , X2 : VOIDPTR ) : INTEGER ;

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



begin (* HAUPTPROGRAMM *)
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

  //******************************************************************
  // insert values into AVL tree                                      
  // only pointers are recorded in AVL tree, no deep copy             
  //******************************************************************

  WRITELN ;
  WRITELN ( 'values are inserted into AVL tree' ) ;
  WRITELN ( 'using general procedure AVLSRCH' ) ;
  WRITELN ( '===================================================' ) ;
  PBAUM := NIL ;
  for I := 1 to 20 do
    begin
      HCHANGED := FALSE ;
      PELEMENT := AVLSRCH ( ADDR ( VVEKTOR [ I ] ) , 0 , COMPARE ,
                  PBAUM , HCHANGED , TRUE ) ;
    end (* for *) ;

  //******************************************************************
  // print AVL tree using AVLPRINT                                    
  //******************************************************************

  WRITELN ;
  WRITELN ( 'print AVL tree using general procedure AVLPRINT' ) ;
  WRITELN ( '===================================================' ) ;
  AVLPRINT ( PBAUM , OUTPUT , 8 , PRINTELEM , NIL , ' ' ) ;

  //******************************************************************
  // walk thru AVL tree and print values                              
  //******************************************************************

  WRITELN ;
  WRITELN ( 'walk thru AVL tree using general procedure AVLGET' ) ;
  WRITELN ( '===================================================' ) ;
  PLAUF := PBAUM ;
  RC := AVLGET ( 'F' , PLAUF , PLAUF , PINT , POBJ ) ;
  while RC = 0 do
    begin
      WRITELN ( 'aus AVL-Baum: ' , PINT -> ) ;
      RC := AVLGET ( 'N' , PLAUF , PLAUF , PINT , POBJ ) ;
    end (* while *) ;
  AVLFREE ( PBAUM ) ;

  //******************************************************************
  // insert values into AVL tree                                      
  // now with deep copy                                               
  //******************************************************************

  WRITELN ;
  WRITELN ( 'values are inserted into AVL tree' ) ;
  WRITELN ( 'using general procedure AVLSRCH' ) ;
  WRITELN ( '===================================================' ) ;
  PBAUM := NIL ;
  for I := 1 to 20 do
    begin
      HCHANGED := FALSE ;
      PELEMENT := AVLSRCH ( ADDR ( VVEKTOR [ I ] ) , SIZEOF ( INTEGER )
                  , COMPARE , PBAUM , HCHANGED , TRUE ) ;
    end (* for *) ;

  //******************************************************************
  // print AVL tree using AVLPRINT                                    
  //******************************************************************

  WRITELN ;
  WRITELN ( 'print AVL tree using general procedure AVLPRINT' ) ;
  WRITELN ( '===================================================' ) ;
  AVLPRINT ( PBAUM , OUTPUT , 8 , PRINTELEM , NIL , ' ' ) ;

  //******************************************************************
  // walk thru AVL tree and print values                              
  //******************************************************************

  WRITELN ;
  WRITELN ( 'walk thru AVL tree using general procedure AVLGET' ) ;
  WRITELN ( '===================================================' ) ;
  PLAUF := PBAUM ;
  RC := AVLGET ( 'F' , PLAUF , PLAUF , PINT , POBJ ) ;
  while RC = 0 do
    begin
      WRITELN ( 'aus AVL-Baum: ' , PINT -> ) ;
      RC := AVLGET ( 'N' , PLAUF , PLAUF , PINT , POBJ ) ;
    end (* while *) ;
  AVLFREE ( PBAUM ) ;

  //******************************************************************
  // insert values into AVL tree                                      
  // string (9) variables instead of integers                         
  //******************************************************************
  // the same variable is used again and again to insert values       
  // into the AVL tree; this is only valid if deep copy is specified  
  // (the second parameter of AVLSRCH needs to be equal to the        
  // key size in this case)                                           
  //******************************************************************
  // most interesting: the AVL functions work for different           
  // key types; the compare functions passed as parameter handle      
  // the differences                                                  
  //******************************************************************

  WRITELN ;
  WRITELN ( 'values are inserted into AVL tree' ) ;
  WRITELN ( 'using general procedure AVLSRCH' ) ;
  WRITELN ( '===================================================' ) ;
  PBAUM := NIL ;
  HCHANGED := FALSE ;
  S9 := 'wovon' ;
  PELEMENT := AVLSRCH ( ADDR ( S9 ) , SIZEOF ( S9 ) , COMPARE_S9 ,
              PBAUM , HCHANGED , TRUE ) ;
  HCHANGED := FALSE ;
  S9 := 'man' ;
  PELEMENT := AVLSRCH ( ADDR ( S9 ) , SIZEOF ( S9 ) , COMPARE_S9 ,
              PBAUM , HCHANGED , TRUE ) ;
  HCHANGED := FALSE ;
  S9 := 'nicht' ;
  PELEMENT := AVLSRCH ( ADDR ( S9 ) , SIZEOF ( S9 ) , COMPARE_S9 ,
              PBAUM , HCHANGED , TRUE ) ;
  HCHANGED := FALSE ;
  S9 := 'reden' ;
  PELEMENT := AVLSRCH ( ADDR ( S9 ) , SIZEOF ( S9 ) , COMPARE_S9 ,
              PBAUM , HCHANGED , TRUE ) ;
  HCHANGED := FALSE ;
  S9 := 'kann' ;
  PELEMENT := AVLSRCH ( ADDR ( S9 ) , SIZEOF ( S9 ) , COMPARE_S9 ,
              PBAUM , HCHANGED , TRUE ) ;
  HCHANGED := FALSE ;
  S9 := 'davon' ;
  PELEMENT := AVLSRCH ( ADDR ( S9 ) , SIZEOF ( S9 ) , COMPARE_S9 ,
              PBAUM , HCHANGED , TRUE ) ;
  HCHANGED := FALSE ;
  S9 := 'muss' ;
  PELEMENT := AVLSRCH ( ADDR ( S9 ) , SIZEOF ( S9 ) , COMPARE_S9 ,
              PBAUM , HCHANGED , TRUE ) ;
  HCHANGED := FALSE ;
  S9 := 'man' ;
  PELEMENT := AVLSRCH ( ADDR ( S9 ) , SIZEOF ( S9 ) , COMPARE_S9 ,
              PBAUM , HCHANGED , TRUE ) ;
  HCHANGED := FALSE ;
  S9 := 'schweigen' ;
  PELEMENT := AVLSRCH ( ADDR ( S9 ) , SIZEOF ( S9 ) , COMPARE_S9 ,
              PBAUM , HCHANGED , TRUE ) ;

  //******************************************************************
  // print AVL tree using AVLPRINT                                    
  //******************************************************************

  WRITELN ;
  WRITELN ( 'print AVL tree using general procedure AVLPRINT' ) ;
  WRITELN ( '===================================================' ) ;
  AVLPRINT ( PBAUM , OUTPUT , 10 , PRINT_S9 , NIL , ' ' ) ;

  //******************************************************************
  // walk thru AVL tree and print values                              
  //******************************************************************

  WRITELN ;
  WRITELN ( 'walk thru AVL tree using general procedure AVLGET' ) ;
  WRITELN ( '===================================================' ) ;
  PLAUF := PBAUM ;
  RC := AVLGET ( 'F' , PLAUF , PLAUF , PS9 , POBJ ) ;
  while RC = 0 do
    begin
      WRITELN ( 'aus AVL-Baum: ' , PS9 -> ) ;
      RC := AVLGET ( 'N' , PLAUF , PLAUF , PS9 , POBJ ) ;
    end (* while *) ;
  AVLFREE ( PBAUM ) ;
end (* HAUPTPROGRAMM *) .
