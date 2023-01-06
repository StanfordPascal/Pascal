program TQSORT ( OUTPUT ) ;

/**********************************************************/
/*                                                        */
/*   Module QSORT                                         */
/*                                                        */
/*   Quicksort lt. C.A.R. Hoare                           */
/*                                                        */
/*   w und x k”nnen static sein, da sie trotz             */
/*   Rekursion nicht von verschiedenen Ebenen             */
/*   gleichzeitig gebraucht werden.                       */
/*                                                        */
/*   zusatz1 und zusatz2 sind Informationen, die          */
/*   an die Vergleichsfunktion durchgereicht werden,      */
/*   sie steuern den Vergleich                            */
/*                                                        */
/*   Die Maximalgr”áe eines Elements ist 2048,            */
/*   das Einhalten dieser Gr”áe wird NICHT berprft      */
/*                                                        */
/**********************************************************/



var TAB : array [ 1 .. 120 ] of INTEGER :=
          ( 1 , 4 , 7 , 12 , 67 , 32 , 89 , 13 , 22 , 54 , 2 , 11 , 1 ,
            4 , 7 , 12 , 67 , 32 , 89 , 13 , 22 , 54 , 2 , 11 , 1 , 4 ,
            7 , 12 , 67 , 32 , 89 , 13 , 22 , 54 , 2 , 11 , 1 , 4 , 7 ,
            12 , 67 , 32 , 89 , 13 , 22 , 54 , 2 , 11 , 1 , 4 , 7 , 12
            , 67 , 32 , 89 , 13 , 22 , 54 , 2 , 11 , 1 , 4 , 7 , 12 ,
            67 , 32 , 89 , 13 , 22 , 54 , 2 , 11 , 1 , 4 , 7 , 12 , 67
            , 32 , 89 , 13 , 22 , 54 , 2 , 11 , 1 , 4 , 7 , 12 , 67 ,
            32 , 89 , 13 , 22 , 54 , 2 , 11 , 1 , 4 , 7 , 12 , 67 , 32
            , 89 , 13 , 22 , 54 , 2 , 11 , 1 , 4 , 7 , 12 , 67 , 32 ,
            89 , 13 , 22 , 54 , 2 , 11 ) ;
    I : INTEGER ;
    TRACE : BOOLEAN := FALSE ;



function COMPX ( X , Y : ANYPTR ; L : INTEGER ; Z1 , Z2 : INTEGER ) :
               INTEGER ;

   var IX , IY : -> INTEGER ;

   begin (* COMPX *)
     IX := X ;
     IY := Y ;
     if IX -> < IY -> then
       COMPX := - 1
     else
       if IX -> > IY -> then
         COMPX := 1
       else
         COMPX := 0
   end (* COMPX *) ;



procedure QSORT ( TAB : ANYPTR ;         // address of table
                SIZE : INTEGER ;         // size of element
                LEFT : INTEGER ;         // left index minus 1
                RIGHT : INTEGER ;        // right index minus 1
                ZUSATZ1 : INTEGER ;      // add. argument for comp
                ZUSATZ2 : INTEGER ;      // add. argument for comp
                function COMP            //
                ( X1 : ANYPTR ;          // function comp is a
                X2 : ANYPTR ;            // parameter for QSORT
                L : INTEGER ;            // and is used to compare
                Z1 : INTEGER ;           // two elements; results
                Z2 : INTEGER )           // similar to MEMCMP
                : INTEGER ) ;            //

//**********************************************************
//   QSORT - Quicksort
//   C.A.R. Hoare method
//   with recursive call
//**********************************************************
//   QSORT sorts arbitrary tables
//   the length of an element should not exceed 2048 bytes
//**********************************************************
//   A compare function is passed as an additional parameter;
//   this function compares two elements (via pointers)
//   it gets the length of an element and two additional
//   arguments (could be key position and length) and then
//   it should return a comparison result similar to
//   the MEMCMP function
//**********************************************************


   static W : CHAR ( 2048 ) ;
          X : CHAR ( 2048 ) ;

   var I : INTEGER ;
       J : INTEGER ;
       M : INTEGER ;
       ATAB : ANYPTR ;
       AI : ANYPTR ;
       AJ : ANYPTR ;
       AM : ANYPTR ;

   begin (* QSORT *)

     //********************************************
     //   trace message at beginning of iteration
     //********************************************

     if TRACE then
       begin
         WRITELN ( 'Start QSORT iteration' ) ;
         WRITELN ( 'left,right = ' , LEFT : 1 , ',' , RIGHT : 1 ) ;
       end (* then *) ;

     //********************************************
     //   start QSORT
     //********************************************

     ATAB := TAB ;
     I := LEFT ;
     J := RIGHT ;
     M := ( LEFT + RIGHT ) DIV 2 ;
     AM := PTRADD ( ATAB , M * SIZE ) ;
     MEMCPY ( ADDR ( X ) , AM , SIZE ) ;
     AI := PTRADD ( ATAB , I * SIZE ) ;
     AJ := PTRADD ( ATAB , J * SIZE ) ;
     repeat
       while COMP ( AI , ADDR ( X ) , SIZE , ZUSATZ1 , ZUSATZ2 ) < 0 do
         AI := PTRADD ( AI , SIZE ) ;
       while COMP ( ADDR ( X ) , AJ , SIZE , ZUSATZ1 , ZUSATZ2 ) < 0 do
         AJ := PTRADD ( AJ , - SIZE ) ;
       if PTRDIFF ( AI , AJ ) <= 0 then
         begin
           if PTRDIFF ( AI , AJ ) < 0 then
             begin
               MEMCPY ( ADDR ( W ) , AI , SIZE ) ;
               MEMCPY ( AI , AJ , SIZE ) ;
               MEMCPY ( AJ , ADDR ( W ) , SIZE ) ;
             end (* then *) ;
           AI := PTRADD ( AI , SIZE ) ;
           AJ := PTRADD ( AJ , - SIZE ) ;
         end (* then *)
     until PTRDIFF ( AI , AJ ) > 0 ;
     I := PTRDIFF ( AI , ATAB ) DIV SIZE ;
     J := PTRDIFF ( AJ , ATAB ) DIV SIZE ;

     //********************************************
     //   check and iterate left and right part
     //********************************************

     if LEFT < J then
       QSORT ( ATAB , SIZE , LEFT , J , ZUSATZ1 , ZUSATZ2 , COMP ) ;
     if I < RIGHT then
       QSORT ( ATAB , SIZE , I , RIGHT , ZUSATZ1 , ZUSATZ2 , COMP ) ;

     //********************************************
     //   trace message at end of iteration
     //********************************************

     if TRACE then
       begin
         WRITELN ( 'left,right = ' , LEFT : 1 , ',' , RIGHT : 1 ) ;
         WRITELN ( 'End QSORT iteration' ) ;
       end (* then *) ;
   end (* QSORT *) ;



begin (* HAUPTPROGRAMM *)
  for I := 1 to 120 do
    WRITE ( TAB [ I ] : 5 ) ;
  WRITELN ;
  WRITELN ;
  QSORT ( ADDR ( TAB ) , 4 , 0 , 119 , 0 , 0 , COMPX ) ;
  for I := 1 to 120 do
    WRITE ( TAB [ I ] : 5 ) ;
  WRITELN ;
end (* HAUPTPROGRAMM *) .
