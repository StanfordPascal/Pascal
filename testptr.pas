program TESTPTR ( OUTPUT ) ;

//**********************************************************************
//$A+                                                                   
//**********************************************************************



type CHAINPTR = -> CHAIN ;
     CHAIN = record
               N : INTEGER ;
               NEXT : CHAINPTR ;
             end ;


var K : CHAINPTR ;
    KSTART : CHAINPTR ;



procedure PRINT ( X : CHAINPTR ) ;

   begin (* PRINT *)
     while X <> NIL do
       begin
         WRITELN ( X -> . N ) ;
         X := X -> . NEXT
       end (* while *)
   end (* PRINT *) ;



begin (* HAUPTPROGRAMM *)
  NEW ( K ) ;
  KSTART := K ;
  K -> . N := 1 ;
  NEW ( K -> . NEXT ) ;
  K := K -> . NEXT ;
  K -> . N := 2 ;
  NEW ( K -> . NEXT ) ;
  K := K -> . NEXT ;
  K -> . N := 3 ;
  K -> . NEXT := NIL ;
  PRINT ( KSTART ) ;
end (* HAUPTPROGRAMM *) .
