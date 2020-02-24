program TESTWRB ( OUTPUT ) ;

//*****
//$A+  
//*****



type COLOR = ( YELLOW , RED , GREEN , BLUE ) ;


var B : BOOLEAN ;
    I : INTEGER ;
    CH : CHAR ( 10 ) ;
    V : STRING ( 10 ) ;
    C : COLOR ;
    X : INTEGER ;
    R : REAL ;



procedure TERMIN ( var X : TEXT ) ;

   EXTERNAL ;



procedure TERMOUT ( var X : TEXT ) ;

   EXTERNAL ;



procedure $PASWRI ( var F : TEXT ; WIDTH : INTEGER ; V : INTEGER ) ;

   EXTERNAL ;



procedure $PASWRR ( var F : TEXT ; WIDTH : INTEGER ; SCALE : INTEGER ;
                  V : REAL ) ;

   EXTERNAL ;



begin (* HAUPTPROGRAMM *)
  TERMIN ( INPUT ) ;
  TERMOUT ( OUTPUT ) ;
  WRITELN ( 'give width for various outputs:' ) ;
  READLN ( I ) ;
  X := 27 ;
  WRITELN ( X : I , ' <===' ) ;
  X := - 27 ;
  WRITELN ( X : I , ' <===' ) ;
  X := 27 ;
  $PASWRI ( OUTPUT , I , X ) ;
  WRITELN ( ' <===' ) ;
  X := - 27 ;
  $PASWRI ( OUTPUT , I , X ) ;
  WRITELN ( ' <===' ) ;
  X := 0 ;
  $PASWRI ( OUTPUT , I , X ) ;
  WRITELN ( ' <===' ) ;
  R := 12.37 ;
  WRITELN ( R : I : 1 , ' <===' ) ;
  R := - 12.37 ;
  WRITELN ( R : I : 1 , ' <===' ) ;
  R := 0.0 ;
  WRITELN ( R : I : 1 , ' <===' ) ;
  R := 12.37 ;
  WRITELN ( R : I : - 1 , ' <===' ) ;
  R := - 12.37 ;
  WRITELN ( R : I : - 1 , ' <===' ) ;
  R := 0.0 ;
  WRITELN ( R : I : - 1 , ' <===' ) ;
  $PASWRR ( OUTPUT , I , 2 , R ) ;
  WRITELN ( ' <===' ) ;
  $PASWRR ( OUTPUT , I , 1 , R ) ;
  WRITELN ( ' <===' ) ;
  $PASWRR ( OUTPUT , I , - 1 , R ) ;
  WRITELN ( ' <===' ) ;
  R := 123456.78 ;
  $PASWRR ( OUTPUT , I , 2 , R ) ;
  WRITELN ( ' <===' ) ;
  $PASWRR ( OUTPUT , I , 1 , R ) ;
  WRITELN ( ' <===' ) ;
  $PASWRR ( OUTPUT , I , - 1 , R ) ;
  WRITELN ( ' <===' ) ;
  B := TRUE ;
  WRITELN ( B : I , ' <===' ) ;
  B := FALSE ;
  WRITELN ( B : I , ' <===' ) ;
  CH := 'Bernd' ;
  WRITELN ( CH : I , ' <===' ) ;
  V := 'Oppolzer' ;
  WRITELN ( V : I , ' <===' ) ;
  C := GREEN ;
  WRITELN ( C : I , ' <===' ) ;
end (* HAUPTPROGRAMM *) .
