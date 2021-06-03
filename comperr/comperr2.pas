program TESTERR4 ( OUTPUT ) ;



function F1 : INTEGER ;

   begin (* F1 *)
     F1 := 5 ;
   end (* F1 *) ;



function F2 : INTEGER ;

   begin (* F2 *)
     F1 := 12 ;
   end (* F2 *) ;



begin (* HAUPTPROGRAMM *)
  WRITELN ( F1 ) ;
  WRITELN ( F2 ) ;
end (* HAUPTPROGRAMM *) .
