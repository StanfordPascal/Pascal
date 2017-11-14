module TESTM1 ;



procedure PROC1 ;

   var A1 : INTEGER ;


   procedure PROC3 ;

      FORWARD ;


   procedure PROC2 ;

      var A2 : INTEGER ;

      begin (* PROC2 *)
        PROC3 ;
      end (* PROC2 *) ;


   procedure PROC3 ;

      var A3 : INTEGER ;

      begin (* PROC3 *)
        PROC2 ;
      end (* PROC3 *) ;


   begin (* PROC1 *)
     PROC2 ;
     PROC3 ;
   end (* PROC1 *) ;



begin (* HAUPTPROGRAMM *)
  
end (* HAUPTPROGRAMM *) .
