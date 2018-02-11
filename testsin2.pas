program TESTSIN2 ( OUTPUT ) ;

(**********************************************************************)
(*$A+                                                                 *)
(**********************************************************************)



var X : REAL ;



function F1 ( X : REAL ) : REAL ;

   begin (* F1 *)
     F1 := X * X
   end (* F1 *) ;



function F2 ( X : REAL ) : REAL ;

   begin (* F2 *)
     F2 := SIN ( X )
   end (* F2 *) ;



procedure F3 ( X1 : REAL ; X2 : REAL ; function F ( Y : REAL ) : REAL )
             ;

   var X : REAL ;

   begin (* F3 *)
     X := X1 ;
     while X <= X2 do
       begin
         WRITELN ( 'funktion f: ' , X : 10 : 2 , F ( X ) : 10 : 2 ) ;
         X := X + 0.1 ;
       end (* while *)
   end (* F3 *) ;



begin (* HAUPTPROGRAMM *)
  X := 1.0 ;
  while X <= 3.0 do
    begin
      WRITELN ( 'funktion f: ' , X : 10 : 2 , F2 ( X ) : 10 : 2 ) ;
      X := X + 0.1 ;
    end (* while *)
end (* HAUPTPROGRAMM *) .
