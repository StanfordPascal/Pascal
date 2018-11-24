program TESTOPERATORPRECEDENce ;


var A , B , C , D : REAL ;
    ANS : REAL ;


begin (* HAUPTPROGRAMM *)
  A := 1.0 ;
  B := 51009.9 ;
  C := 51009.9 ;
  D := 51009.9 ;
  WRITELN ( B ) ;
  WRITELN ( C ) ;
  WRITELN ( D ) ;
  WRITELN ( '---' ) ;
  ANS := C - B * C / D ;
  WRITELN ( ANS ) ;
  ANS := C - ( B * C ) / D ;
  WRITELN ( ANS ) ;
  ANS := C - ( B * C / D ) ;
  WRITELN ( ANS ) ;
  ANS := C - B * ( C / D ) ;
  WRITELN ( ANS ) ;
  ANS := C - ( B * ( C / D ) ) ;
  WRITELN ( ANS ) ;
end (* HAUPTPROGRAMM *) .
