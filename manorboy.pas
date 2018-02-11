program MANORBOY ( OUTPUT ) ;

(*************************************************************)
(* program to do knuth's man or boy test                     *)
(* from https://rosettacode.org/wiki/Man_or_boy_test#Pascal  *)
(*************************************************************)
(* option to enable assembler listing:                       *)
(*$A+                                                        *)
(*************************************************************)




function ZERO : INTEGER ;

   begin (* ZERO *)
     ZERO := 0
   end (* ZERO *) ;



function ONE : INTEGER ;

   begin (* ONE *)
     ONE := 1
   end (* ONE *) ;



function NEGONE : INTEGER ;

   begin (* NEGONE *)
     NEGONE := - 1
   end (* NEGONE *) ;



function A ( K : INTEGER ; function X1 : INTEGER ; function X2 :
           INTEGER ; function X3 : INTEGER ; function X4 : INTEGER ;
           function X5 : INTEGER ) : INTEGER ;


   function B : INTEGER ;

      begin (* B *)
        K := K - 1 ;
        B := A ( K , B , X1 , X2 , X3 , X4 )
      end (* B *) ;


   begin (* A *)
     if K <= 0 then
       A := X4 + X5
     else
       A := B
   end (* A *) ;



begin (* HAUPTPROGRAMM *)
  WRITELN ( A ( 10 , ONE , NEGONE , NEGONE , ONE , ZERO ) )
end (* HAUPTPROGRAMM *) .
