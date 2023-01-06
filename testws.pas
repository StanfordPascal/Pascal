program TESTWS ( INPUT , OUTPUT ) ;


var S : STRING ( 50 ) ;
    CP : -> CHAR ;
    F : ( ROT , GELB , GRUEN , BLAU ) ;



procedure TESTAUSGABE ( R : REAL ) ;

   begin (* TESTAUSGABE *)
     WRITESTR ( S , R : 4 : 2 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'write mit real : >' , R : 4 : 2 , '<' ) ;
     WRITESTR ( S , R : - 4 : 2 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'write mit real : >' , R : - 4 : 2 , '<' ) ;
     WRITESTR ( S , R : 10 : 2 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'write mit real : >' , R : 10 : 2 , '<' ) ;
     WRITESTR ( S , R : - 10 : 2 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'write mit real : >' , R : - 10 : 2 , '<' ) ;
     WRITESTR ( S , R : 11 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'write mit real : >' , R : 11 , '<' ) ;
     WRITESTR ( S , R : - 11 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'write mit real : >' , R : - 11 , '<' ) ;
     WRITESTR ( S , R ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'write mit real : >' , R , '<' ) ;
     WRITESTR ( S , R : 25 : 2 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'write mit real : >' , R : 25 : 2 , '<' ) ;
     WRITESTR ( S , R : - 25 : 2 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'write mit real : >' , R : - 25 : 2 , '<' ) ;
     WRITESTR ( S , R : 25 : 15 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'write mit real : >' , R : 25 : 15 , '<' ) ;
     WRITESTR ( S , R : - 25 : 15 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'write mit real : >' , R : - 25 : 15 , '<' ) ;
     WRITESTR ( S , R : 25 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'write mit real : >' , R : 25 , '<' ) ;
     WRITESTR ( S , R : - 25 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'write mit real : >' , R : - 25 , '<' ) ;
   end (* TESTAUSGABE *) ;



procedure TEST2 ;

   begin (* TEST2 *)

     //************************
     // test: writestr mit enum
     //************************

     F := GELB ;
     WRITESTR ( S , F ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'skalar direkt  : >' , F , '<' ) ;
     WRITESTR ( S , F : 10 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'skalar direkt  : >' , F : 10 , '<' ) ;
     WRITESTR ( S , F : - 10 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'skalar direkt  : >' , F : - 10 , '<' ) ;
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
     WRITESTR ( S , 'c.ptr = ' , CP ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;

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
     WRITESTR ( S , 45 : 10 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'write integer  : >' , 45 : 10 , '<' ) ;
     WRITESTR ( S , 45 : - 10 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'write integer  : >' , 45 : - 10 , '<' ) ;
     WRITESTR ( S , - 45 : 10 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'write integer  : >' , - 45 : 10 , '<' ) ;
     WRITESTR ( S , - 45 : - 10 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'write integer  : >' , - 45 : - 10 , '<' ) ;
     WRITESTR ( S , 45 : 1 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'write integer  : >' , 45 : 1 , '<' ) ;
     WRITESTR ( S , - 45 : 1 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'write integer  : >' , - 45 : 1 , '<' ) ;
     WRITESTR ( S , 123456789 : 9 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'write integer  : >' , 123456789 : 9 , '<' ) ;
     WRITESTR ( S , 123456789 : - 9 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'write integer  : >' , 123456789 : - 9 , '<' ) ;
     WRITESTR ( S , - 123456789 : 9 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'write integer  : >' , - 123456789 : 9 , '<' ) ;
     WRITESTR ( S , - 123456789 : - 9 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'write integer  : >' , - 123456789 : - 9 , '<' ) ;
   end (* TEST1 *) ;



begin (* HAUPTPROGRAMM *)
  TEST1 ;
  TEST3
end (* HAUPTPROGRAMM *) .
