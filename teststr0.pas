program TESTSTR0 ( OUTPUT ) ;

//**********************************************************************
//$A+                                                                   
//**********************************************************************



var C : CHAR ;
    CX : CHAR ( 5 ) ;



procedure TESTCA ( S : CHAR ( 10 ) ) ;

   begin (* TESTCA *)
     WRITELN ( 's = ' , LENGTH ( S ) : 3 , ' <' , S , '>' )
   end (* TESTCA *) ;



procedure TESTS2 ( S : STRING ( 10 ) ) ;

   begin (* TESTS2 *)
     WRITELN ( 's = ' , LENGTH ( S ) : 3 , ' <' , S , '>' )
   end (* TESTS2 *) ;



procedure TESTS ( const S : STRING ) ;

   begin (* TESTS *)
     WRITELN ( 's = ' , LENGTH ( S ) : 3 , ' <' , S , '>' )
   end (* TESTS *) ;



begin (* HAUPTPROGRAMM *)

  //******************************************************************
  // C := '' ;                                                        
  //******************************************************************

  C := '*' ;
  CX := '' ;
  CX := '*' ;
  CX := '*****' ;
  CX := '***' '' ;
  WRITELN ( 'should be ***: <' , CX , '>' ) ;
  CX := '' '' ;
  WRITELN ( 'should be empty string: <' , CX , '>' ) ;
  TESTCA ( ' ' ) ;
  TESTCA ( '' ) ;
  TESTCA ( 'bernd' ) ;
  TESTS2 ( ' ' ) ;
  TESTS2 ( '' ) ;
  TESTS2 ( 'bernd' ) ;
  TESTS ( ' ' ) ;
  TESTS ( '' ) ;
  TESTS ( 'bernd' )
end (* HAUPTPROGRAMM *) .
