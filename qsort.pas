module QSORT ;

/**********************************************************/
/*                                                        */
/*   Module QSORT                                         */
/*                                                        */
/*   Quicksort lt. C.A.R. Hoare                           */
/*                                                        */
/*   w und x kînnen static sein, da sie trotz             */
/*   Rekursion nicht von verschiedenen Ebenen             */
/*   gleichzeitig gebraucht werden.                       */
/*                                                        */
/*   zusatz1 und zusatz2 sind Informationen, die          */
/*   an die Vergleichsfunktion durchgereicht werden,      */
/*   sie steuern den Vergleich                            */
/*                                                        */
/*   Die Maximalgrî·e eines Elements ist 2048,            */
/*   das Einhalten dieser Grî·e wird NICHT ÅberprÅft      */
/*                                                        */
/**********************************************************/




procedure QSORT ( TAB : ANYPTR ;                    //
                SIZE : INTEGER ;                    //
                LEFT : INTEGER ;                    //
                RIGHT : INTEGER ;                   //
                ZUSATZ1 : INTEGER ;                 //
                ZUSATZ2 : INTEGER ;                 //
                function COMP                       //
                ( X1 : VOIDPTR ;                    //
                X2 : VOIDPTR ;                      //
                L : INTEGER ;                       //
                Z1 : INTEGER ;                      //
                Z2 : INTEGER )                      //
                : INTEGER ) ;                       //

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
     ATAB := TAB ;

     /*                                            */
     /*   TRACE_STRT ("adipx_qsort");              */
     /*   TRACE_MESS ("atab    = %p\n", atab);     */
     /*   TRACE_MESS ("size    = %d\n", size);     */
     /*   TRACE_MESS ("left.a  = %d\n", left);     */
     /*   TRACE_MESS ("right.a = %d\n", right);    */
     /*   TRACE_MESS ("zusatz1 = <%s>\n", zusatz1);*/
     /*   TRACE_MESS ("zusatz2 = <%s>\n", zusatz2);*/
     /*                                            */

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
     if LEFT < J then
       QSORT ( ATAB , SIZE , LEFT , J , ZUSATZ1 , ZUSATZ2 , COMP ) ;
     if I < RIGHT then
       QSORT ( ATAB , SIZE , I , RIGHT , ZUSATZ1 , ZUSATZ2 , COMP ) ;

     /*                                        */
     /*   TRACE_MESS ("left.e  = %d\n", left); */
     /*   TRACE_MESS ("right.e = %d\n", right);*/
     /*   TRACE_STOP ("adipx_qsort");          */
     /*                                        */

   end (* QSORT *) ;



begin (* HAUPTPROGRAMM *)
  
end (* HAUPTPROGRAMM *) .
