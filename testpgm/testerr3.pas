program TESTERR3 ( OUTPUT ) ;


var S : CHAR ( 80 ) ;
    S2 : CHAR ( 10 ) ;



procedure WRITE_VORSCHRIFT ( var F : TEXT ; VORSCHRIFT : CHAR ( 10 ) )
                           ;

   begin (* WRITE_VORSCHRIFT *)
     WRITELN ( F , VORSCHRIFT ) ;
   end (* WRITE_VORSCHRIFT *) ;



function CHARFUNC : STRING ;

   begin (* CHARFUNC *)
     CHARFUNC := 'constant' ;
   end (* CHARFUNC *) ;



procedure FEHLER_ZUWEISUNG ;

   begin (* FEHLER_ZUWEISUNG *)
     S := 'langer string mit vielen zeichen' ;
     S2 := SUBSTR ( S , 8 , 8 ) ;

     //****************************************
     // runtime error stringsize is diagnosed  
     // which is correct                       
     // but wrong error line                   
     // error line refers to line in $PASSTR1  
     // should refer to loc here !             
     //****************************************

     if FALSE then
       S2 := 'zu langer ' || 'String' ;
     if FALSE then
       S2 := SUBSTR ( S , 8 , 20 ) ;
   end (* FEHLER_ZUWEISUNG *) ;



begin (* HAUPTPROGRAMM *)
  FEHLER_ZUWEISUNG ;

  //*********************************************
  // compiler refuses to compile procedure call  
  // where function result of string type        
  // is passed by value to fixed char argument   
  // but should work                             
  //*********************************************

  if TRUE then
    begin
      WRITE_VORSCHRIFT ( OUTPUT , S2 ) ;
      WRITE_VORSCHRIFT ( OUTPUT , CHARFUNC ) ;
      WRITE_VORSCHRIFT ( OUTPUT , SUBSTR ( S , 8 , 7 ) ) ;
    end (* then *)
end (* HAUPTPROGRAMM *) .
