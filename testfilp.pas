program TESTFILP ( OUTPUT ) ;


var FILEP : -> TEXT ;



procedure OUTTEST ( var FP : TEXT ) ;

   begin (* OUTTEST *)
     WRITELN ( FP , 'outtest: works' ) ;
   end (* OUTTEST *) ;



begin (* HAUPTPROGRAMM *)
  FILEP := ADDR ( OUTPUT ) ;
  OUTTEST ( FILEP -> ) ;
  WRITELN ( 'eof = ' , EOF ( OUTPUT ) ) ;
  WRITELN ( 'eof = ' , EOF ( FILEP -> ) ) ;
  WRITELN ( OUTPUT , 'writeln: works' ) ;
  WRITELN ( FILEP -> , 'writeln: works' ) ;
end (* HAUPTPROGRAMM *) .
