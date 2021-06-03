program MANORBOY ( OUTPUT ) ;

(*************************************************************)
(* program to do knuth's man or boy test                     *)
(* from https://rosettacode.org/wiki/Man_or_boy_test#Pascal  *)
(*************************************************************)
(* option to enable assembler listing:                       *)
(*$A+                                                        *)
(*************************************************************)



var I : INTEGER ;



function ZERO : INTEGER ;

   begin (* ZERO *)
     ZERO := 0 ;
   end (* ZERO *) ;



function ONE : INTEGER ;

   begin (* ONE *)
     ONE := 1 ;
   end (* ONE *) ;



function NEGONE : INTEGER ;

   begin (* NEGONE *)
     NEGONE := - 1 ;
   end (* NEGONE *) ;



function A ( K : INTEGER ; function X1 : INTEGER ; function X2 :
           INTEGER ; function X3 : INTEGER ; function X4 : INTEGER ;
           function X5 : INTEGER ) : INTEGER ;


   function B : INTEGER ;

      begin (* B *)
        K := K - 1 ;
        B := A ( K , B , X1 , X2 , X3 , X4 ) ;
      end (* B *) ;


   begin (* A *)
     if K <= 0 then
       begin
         A := X4 + X5 ;
       end (* then *)
     else
       begin
         A := B ;
       end (* else *) ;
   end (* A *) ;



begin (* HAUPTPROGRAMM *)
  for I := 1 to 13 do
    WRITELN ( 'manorboy result ' , I : 2 , ' = ' , A ( I , ONE , NEGONE
              , NEGONE , ONE , ZERO ) )
end (* HAUPTPROGRAMM *) .
