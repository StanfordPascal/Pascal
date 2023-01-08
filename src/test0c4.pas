program TEST0C4 ( OUTPUT ) ;

//******
//$A+,D+
//******



var CP : -> CHAR ;
    TAG : CHAR ( 20 ) := 'Tag Main Program' ;



procedure PROC2 ;

   var TAG : CHAR ( 20 ) := 'Tag Procedure 2' ;

   begin (* PROC2 *)
     CP := NIL ;
     CP -> := 'A' ;
   end (* PROC2 *) ;



procedure PROC1 ;

   var TAG : CHAR ( 20 ) := 'Tag Procedure 1' ;

   begin (* PROC1 *)
     PROC2
   end (* PROC1 *) ;



begin (* HAUPTPROGRAMM *)
  PROC1
end (* HAUPTPROGRAMM *) .
