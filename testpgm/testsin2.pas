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



function F3 ( X : REAL ) : REAL ;

   begin (* F3 *)
     F3 := SQRT ( X )
   end (* F3 *) ;



procedure F4 ( X1 : REAL ; X2 : REAL ; function F ( Y : REAL ) : REAL )
             ;

   var X : REAL ;

   begin (* F4 *)
     X := X1 ;
     while X <= X2 do
       begin
         WRITELN ( 'funktion f: ' , X : 10 : 2 , F ( X ) : 10 : 2 ) ;
         X := ROUNDX ( X + 0.1 , - 1 ) ;
       end (* while *)
   end (* F4 *) ;



begin (* HAUPTPROGRAMM *)
  WRITELN ( 'schleife mit f1-aufrufen' ) ;
  X := 1.0 ;
  while X <= 4.0 do
    begin
      WRITELN ( 'funktion f: ' , X : 10 : 2 , F1 ( X ) : 10 : 5 ) ;
      X := ROUNDX ( X + 0.1 , - 1 ) ;
    end (* while *) ;
  WRITELN ( 'schleife mit sinus-aufrufen' ) ;
  X := 1.0 ;
  while X <= 4.0 do
    begin
      WRITELN ( 'funktion f: ' , X : 10 : 2 , F2 ( X ) : 10 : 5 ) ;
      X := ROUNDX ( X + 0.1 , - 1 ) ;
    end (* while *) ;
  WRITELN ( 'schleife mit sqrt-aufrufen' ) ;
  X := 1.0 ;
  while X <= 4.0 do
    begin
      WRITELN ( 'funktion f: ' , X : 10 : 2 , F3 ( X ) : 10 : 5 ) ;
      X := ROUNDX ( X + 0.1 , - 1 ) ;
    end (* while *) ;
  WRITELN ( 'variabler aufruf (F4)' ) ;
  F4 ( 1.0 , 4.0 , F1 ) ;
  F4 ( 1.0 , 4.0 , F2 ) ;
  F4 ( 1.0 , 4.0 , F3 ) ;
end (* HAUPTPROGRAMM *) .
