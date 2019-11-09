program TESTC1 ( INPUT , OUTPUT ) ;


type TMYRECORD = record
                   A : INTEGER ;
                   B : CHAR ( 5 ) ;
                 end ;


const DEFAULT : TMYRECORD =
      ( 100 , 'foo' ) ;


var R : TMYRECORD ;


begin (* HAUPTPROGRAMM *)
  R := DEFAULT ;
  WRITELN ( 'test const with records' ) ;
  WRITELN ( 'r.a = <' , R . A , '>' ) ;
  WRITELN ( 'r.b = <' , R . B , '>' ) ;
end (* HAUPTPROGRAMM *) .


