program EXPONENTIATION ( INPUT , OUTPUT ) ;

//***************************************
// exponentiation with natural exponent  
//***************************************



var E , Y : INTEGER ;
    U , X , Z : REAL ;


begin (* HAUPTPROGRAMM *)
  READ ( X , Y ) ;
  WRITE ( X : 20 , Y ) ;
  Z := 1 ;
  U := X ;
  E := Y ;

  //*****************************
  // z * u ** e = x ** y, e > 0  
  //*****************************

  while E > 0 do
    begin
      while not ODD ( E ) do
        begin
          E := E DIV 2 ;
          U := SQR ( U ) ;
        end (* while *) ;
      E := E - 1 ;
      Z := U * Z ;
    end (* while *) ;

  //*************
  // z = x ** y  
  //*************

  WRITELN ( Z : 20 ) ;
end (* HAUPTPROGRAMM *) .
