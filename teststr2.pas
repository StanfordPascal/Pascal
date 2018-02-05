program TESTSTR2 ( OUTPUT ) ;


var S : STRING ( 20 ) ;



procedure TESTVALUE ( S : STRING ( 30 ) ) ;

   begin (* TESTVALUE *)
     WRITELN ( 'testvalue: maxlength = ' , MAXLENGTH ( S ) ) ;
     WRITELN ( 'testvalue: length    = ' , LENGTH ( S ) ) ;
     WRITELN ( 'testvalue: content   = ' , S ) ;
     S := 'Hugo' ;
   end (* TESTVALUE *) ;



procedure TESTCONST ( const S : STRING ) ;

   begin (* TESTCONST *)
     WRITELN ( 'testconst: maxlength = ' , MAXLENGTH ( S ) ) ;
     WRITELN ( 'testconst: length    = ' , LENGTH ( S ) ) ;
     WRITELN ( 'testconst: content   = ' , S ) ;
   end (* TESTCONST *) ;



procedure TESTVAR ( var S : STRING ) ;

   begin (* TESTVAR *)
     WRITELN ( 'testvar: maxlength = ' , MAXLENGTH ( S ) ) ;
     WRITELN ( 'testvar: length    = ' , LENGTH ( S ) ) ;
     WRITELN ( 'testvar: content   = ' , S ) ;
     S := 'Hugo' ;
   end (* TESTVAR *) ;



begin (* HAUPTPROGRAMM *)
  S := 'Bernd' ;
  TESTVALUE ( S ) ;
  TESTVALUE ( 'Bernd ' || 'Oppolzer' ) ;
  TESTCONST ( S ) ;
  TESTCONST ( 'Bernd ' || 'Oppolzer' ) ;
  TESTVAR ( S ) ;
  WRITELN ( 'main: S after testvar = ' , S ) ;
  WRITELN ( 'main: maxlength (expr) = ' , MAXLENGTH ( 'Bernd ' ||
            'Oppolzer' ) ) ;
end (* HAUPTPROGRAMM *) .
