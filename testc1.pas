program TESTC1 ( INPUT , OUTPUT ) ;

//**********************************************************************
//$X+
//**********************************************************************



type TMYRECORD = record
                   A : INTEGER ;
                   B : STRING ( 5 ) ;
                 end ;
     LAENGENFELD = 0 .. 1000 ;
     TESTRECORD = record
                    MAXLEN : LAENGENFELD ;
                    ACTLEN : LAENGENFELD ;
                    B : CHAR ( 5 ) ;
                  end ;


const X : STRING ( 10 ) = 'Oppolzer' ;
      X2 : CHAR ( 10 ) = 'Oppolzer' ;
      DEFAULT : TMYRECORD =
      ( 100 , 'foo' ) ;
      DEFAULT2 : TESTRECORD =
      ( 5 , 3 , 'foo' ) ;


var R : TMYRECORD ;
    S : STRING ( 20 ) ;


begin (* HAUPTPROGRAMM *)
  R := DEFAULT ;

  //******************************************************************
  // R . A := 100 ;
  // R . B := 'opp' ;
  //******************************************************************

  S := 'Bernd' ;

  // S := X ;

  WRITELN ( 'test const string' ) ;

  //******************************************************************
  // x ausgeben geht nicht
  // S := X wird offenbar ignoriert (nichts gescheites generiert)
  // fuer S wird 'Bernd' ausgegeben
  // R zugewiesen DEFAULT geht schief,
  // weil DEFAULT unvollstaendig angelegt wird
  //******************************************************************

  // WRITELN ( 'X  = <' , X , '>' ) ;

  WRITELN ( 'S  = <' , S , '>' ) ;
  WRITELN ( 'x2 = <' , X2 , '>' ) ;
  WRITELN ( 'test const with records' ) ;
  WRITELN ( 'r.a = <' , R . A , '>' ) ;
  WRITELN ( 'r.b = <' , R . B , '>' ) ;
end (* HAUPTPROGRAMM *) .
