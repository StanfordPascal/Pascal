program TESTSET2 ( OUTPUT ) ;

(********)
(*$A+   *)
(********)



type ERRTYPE = 1 .. 999 ;
     ERRSET = set of ERRTYPE ;


var X : ERRTYPE ;
    S : ERRSET ;



procedure WRITESET ( S : ERRSET ) ;

   var I : ERRTYPE ;

   begin (* WRITESET *)
     WRITE ( 'writeset:' ) ;
     for I := 1 to 999 do
       if I in S then
         WRITE ( ' ' , I : 1 ) ;
     WRITELN
   end (* WRITESET *) ;



begin (* HAUPTPROGRAMM *)
  S := [ ] ;
  S := [ 1 , 4 , 9 , 16 , 25 ] ;
  WRITESET ( S ) ;
  X := 400 ;
  S := S + [ X ] ;
  WRITESET ( S ) ;
end (* HAUPTPROGRAMM *) .
