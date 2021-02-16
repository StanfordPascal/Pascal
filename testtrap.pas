program TESTTRAP ( OUTPUT ) ;


type XRECORD = record
                 RC : INTEGER ;
                 MESSAGE : CHAR ( 80 ) ;
               end ;


var X : XRECORD ;


begin (* HAUPTPROGRAMM *)
  WRITELN ( 'test trap 5 with record parameter' ) ;
  TRAP ( 5 , X ) ;
  WRITELN ( 'after trap' ) ;
end (* HAUPTPROGRAMM *) .
