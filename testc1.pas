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
      X1 : STRING = 'Oppolzer' ;
      X2 : CHAR ( 10 ) = 'Oppolzer' ;
      X3 : array [ 1 .. 10 ] of CHAR = 'Oppolzer' ;
      X4 = 'Oppolzer' ;
      F : STRING = 5 ;
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

  WRITELN ( 'test const strings' ) ;
  S := X ;
  WRITELN ( 'S  = <' , S , '>' ) ;
  WRITELN ( 'X  = <' , X , '>' ) ;
  S := X1 ;
  WRITELN ( 'S  = <' , S , '>' ) ;
  WRITELN ( 'X1 = <' , X1 , '>' ) ;
  S := X2 ;
  WRITELN ( 'S  = <' , S , '>' ) ;
  WRITELN ( 'X2 = <' , X2 , '>' ) ;
  S := X3 ;
  WRITELN ( 'S  = <' , S , '>' ) ;
  WRITELN ( 'X3 = <' , X3 , '>' ) ;
  S := X4 ;
  WRITELN ( 'S  = <' , S , '>' ) ;
  WRITELN ( 'X4 = <' , X4 , '>' ) ;

  //******************************************************************
  // x ausgeben geht nicht                                            
  // S := X wird offenbar ignoriert (nichts gescheites generiert)     
  // fuer S wird 'Bernd' ausgegeben                                   
  // R zugewiesen DEFAULT geht schief,                                
  // weil DEFAULT unvollstaendig angelegt wird                        
  //******************************************************************

  WRITELN ( 'test const with records' ) ;
  WRITELN ( 'r.a = <' , R . A , '>' ) ;
  WRITELN ( 'r.b = <' , R . B , '>' ) ;
end (* HAUPTPROGRAMM *) .
