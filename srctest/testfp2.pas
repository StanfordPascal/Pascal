program TESTFP2 ( OUTPUT ) ;

(**********************************************************************)
(*                                                                    *)
(*   Testprogram fuer die Uebergabe von Funktionsparametern           *)
(*                                                                    *)
(*   Opp - 2018.02                                                    *)
(*                                                                    *)
(**********************************************************************)
(* Assembler-Output:                                                  *)
(*$A+                                                                 *)
(**********************************************************************)




function F1 ( X : REAL ) : REAL ;

   begin (* F1 *)
     F1 := X * X
   end (* F1 *) ;



function F2 ( X : REAL ) : REAL ;

   begin (* F2 *)
     F2 := SIN ( X )
   end (* F2 *) ;



procedure F3 ( X1 : REAL ; X2 : REAL ; function F ( X : REAL ) : REAL )
             ;

   var X : REAL ;

   begin (* F3 *)
     X := X1 ;
     while X <= X2 do
       begin
         WRITELN ( 'funktion f: ' , X : 10 : 2 , F ( X ) : 15 : 7 ) ;
         X := ROUNDX ( X + 0.1 , - 1 ) ;
       end (* while *)
   end (* F3 *) ;



begin (* HAUPTPROGRAMM *)
  WRITELN ( 'calling F3 with F1 = square x' ) ;
  F3 ( 1.0 , 3.0 , F1 ) ;
  WRITELN ( 'calling F3 with F2 = sin x' ) ;
  F3 ( 1.0 , 3.0 , F2 ) ;
end (* HAUPTPROGRAMM *) .
