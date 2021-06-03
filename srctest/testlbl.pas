program TESTLBL ( OUTPUT ) ;


label 100000 , 123456 ;


begin (* HAUPTPROGRAMM *)
  123456 :
  WRITELN ( 'Test Labels' ) ;
  goto 100000 ;
  WRITELN ( 'should not be written' ) ;
  100000 :
  
end (* HAUPTPROGRAMM *) .
