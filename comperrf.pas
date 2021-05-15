program COMPERRF ( PCODE , OUTPUT ) ;

//*****
//$A+  
//*****



var PCODE : TEXT ;
    ZEILE : STRING ( 100 ) ;



procedure READ ( var Z : STRING ) ;

   begin (* READ *)
     READLN ( PCODE , Z ) ;
   end (* READ *) ;



begin (* HAUPTPROGRAMM *)
  repeat
    READLN ( PCODE , ZEILE ) ;
    WRITELN ( ZEILE ) ;
    READ ( ZEILE ) ;
    WRITELN ( ZEILE ) ;
  until EOF ( PCODE )
end (* HAUPTPROGRAMM *) .
