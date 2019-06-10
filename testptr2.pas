program TESTPTR ( OUTPUT ) ;

//**********************************************************************
//$A+                                                                   
//**********************************************************************



type CFORW = record
               SOMEV : INTEGER ;
               C : -> CHAIN ;
             end ;
     CHAIN = record
               N : INTEGER ;
               NEXT : -> CHAIN ;
             end ;


var K : -> CHAIN ;
    X : CFORW ;



procedure PRINT ( X : -> CHAIN ) ;

   begin (* PRINT *)
     while X <> NIL do
       begin
         WRITELN ( X -> . N ) ;
         X := X -> . NEXT
       end (* while *)
   end (* PRINT *) ;



begin (* HAUPTPROGRAMM *)
  NEW ( K ) ;
  X . C := K ;
  K -> . N := 1 ;
  NEW ( K -> . NEXT ) ;
  K := K -> . NEXT ;
  K -> . N := 2 ;
  NEW ( K -> . NEXT ) ;
  K := K -> . NEXT ;
  K -> . N := 3 ;
  K -> . NEXT := NIL ;
  PRINT ( X . C ) ;
end (* HAUPTPROGRAMM *) .
