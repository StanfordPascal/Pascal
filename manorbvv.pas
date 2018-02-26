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
     WRITELN ( 'start zero' ) ;
     ZERO := 0 ;
     WRITELN ( 'ende zero' ) ;
   end (* ZERO *) ;



function ONE : INTEGER ;

   begin (* ONE *)
     WRITELN ( 'start one' ) ;
     ONE := 1 ;
     WRITELN ( 'ende one' ) ;
   end (* ONE *) ;



function NEGONE : INTEGER ;

   begin (* NEGONE *)
     WRITELN ( 'start negone' ) ;
     NEGONE := - 1 ;
     WRITELN ( 'ende negone' ) ;
   end (* NEGONE *) ;



function A ( K : INTEGER ; function X1 : INTEGER ; function X2 :
           INTEGER ; function X3 : INTEGER ; function X4 : INTEGER ;
           function X5 : INTEGER ) : INTEGER ;

   static LEVELA : INTEGER ;


   function B : INTEGER ;

      static LEVELB : INTEGER ;

      begin (* B *)
        LEVELB := LEVELB + 1 ;
        WRITELN ( 'start B level = ' , LEVELB ) ;
        K := K - 1 ;
        B := A ( K , B , X1 , X2 , X3 , X4 ) ;
        WRITELN ( 'ende B level = ' , LEVELB ) ;
        LEVELB := LEVELB - 1 ;
      end (* B *) ;


   begin (* A *)
     LEVELA := LEVELA + 1 ;
     WRITELN ( 'start A level = ' , LEVELA ) ;
     WRITELN ( 'k = ' , K , ' addr(k) = ' , ADDR ( K ) ) ;
     if K <= 0 then
       begin
         WRITELN ( 'zweig 1' ) ;
         A := X4 + X5 ;
         WRITELN ( 'ende zweig 1' ) ;
       end (* then *)
     else
       begin
         WRITELN ( 'zweig 2' ) ;
         A := B ;
         WRITELN ( 'ende zweig 2' ) ;
       end (* else *) ;
     WRITELN ( 'ende A level = ' , LEVELA ) ;
     LEVELA := LEVELA - 1 ;
   end (* A *) ;



begin (* HAUPTPROGRAMM *)
  WRITELN ( A ( 4 , ONE , NEGONE , NEGONE , ONE , ZERO ) )
end (* HAUPTPROGRAMM *) .
