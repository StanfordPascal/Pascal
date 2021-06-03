program TESTMEMS ( OUTPUT ) ;

//*****
//$A+  
//*****



var A : CHAR ( 500 ) ;



procedure INIT ( var X : CHAR ( 500 ) ) ;

   begin (* INIT *)
     MEMSET ( ADDR ( X ) , CHR ( 64 ) , 500 ) ;
   end (* INIT *) ;



begin (* HAUPTPROGRAMM *)
  MEMSET ( ADDR ( A ) , ' ' , 500 ) ;
  WRITELN ( 'test: <' , SUBSTR ( STR ( A ) , 1 , 20 ) , '>' ) ;
  MEMSET ( ADDR ( A ) , CHR ( ORD ( ' ' ) ) , 500 ) ;
  WRITELN ( 'test: <' , SUBSTR ( STR ( A ) , 1 , 20 ) , '>' ) ;
  MEMSET ( ADDR ( A ) , CHR ( 64 ) , 500 ) ;
  WRITELN ( 'test: <' , SUBSTR ( STR ( A ) , 1 , 20 ) , '>' ) ;
  INIT ( A ) ;
  WRITELN ( 'test: <' , SUBSTR ( STR ( A ) , 1 , 20 ) , '>' ) ;
end (* HAUPTPROGRAMM *) .
