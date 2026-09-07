program TESTWS ( INPUT , OUTPUT ) ;


var S : STRING ( 50 ) ;
    CP : -> CHAR ;
    F : ( ROT , GELB , GRUEN , BLAU ) ;
    CH : CHAR ;



procedure TESTAUSGABE ( R : REAL ) ;


   procedure TESTAUS1 ;

      begin (* TESTAUS1 *)
      end (* TESTAUS1 *) ;


   procedure TESTAUS2 ;

      begin (* TESTAUS2 *)
      end (* TESTAUS2 *) ;


   begin (* TESTAUSGABE *)
     TESTAUS1 ;
     TESTAUS2 ;
   end (* TESTAUSGABE *) ;



procedure TEST2 ;

   begin (* TEST2 *)

     //************************
     // test: writestr mit enum
     //************************

     F := GELB ;
   end (* TEST2 *) ;



procedure TEST3 ;

   begin (* TEST3 *)
     S := 'GELB' ;

     //************************
     // test: writestr mit ptr
     //************************

     CP := ADDR ( S [ 1 ] ) ;
     WRITELN ( 'c.ptr ohne = >' , CP , '<' ) ;
     WRITELN ( 'c.ptr 12   = >' , CP : 12 , '<' ) ;
     WRITELN ( 'c.ptr - 12 = >' , CP : - 12 , '<' ) ;
     WRITELN ( 'c.ptr 1    = >' , CP : 1 , '<' ) ;

     //************************
     // test mit strings
     //************************

     if TRUE then
       begin
         TESTAUSGABE ( 0.0 ) ;
         TESTAUSGABE ( 123.456 ) ;
         TESTAUSGABE ( 123456.789 ) ;
         TESTAUSGABE ( - 123.456 ) ;
         TESTAUSGABE ( - 123456.789 ) ;
       end (* then *)
   end (* TEST3 *) ;



procedure TEST1 ;

   begin (* TEST1 *)
   end (* TEST1 *) ;



begin (* HAUPTPROGRAMM *)
  WRITELN ( 'test 1' ) ;
  reset (input);
  READLN ( CH ) ;
  TEST1 ;
  WRITELN ( 'test 3' ) ;
  READLN ( CH ) ;
  TEST3
end (* HAUPTPROGRAMM *) .
