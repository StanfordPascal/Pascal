program TESTDUMP ( OUTPUT ) ;


var F : TEXT ;
    FNAME : array [ 1 .. 80 ] of CHAR ;
    CP : -> CHAR ;
    CPENDE : -> CHAR ;



procedure DUMPSTOR ( PVON : VOIDPTR ; PBIS : VOIDPTR ) ;

   EXTERNAL ;



begin (* HAUPTPROGRAMM *)
  FNAME := 'c:\\work\\pascal\\work\\testausgabe.dat' ;
  CP := ADDR ( FNAME ) ;
  CPENDE := CP ;
  while PTRDIFF ( CPENDE , CP ) < 64 do
    begin
      WRITELN ( 'adressen: ' , CP , ' ' , CPENDE ) ;
      DUMPSTOR ( CP , CPENDE ) ;
      CPENDE := PTRADD ( CPENDE , 1 ) ;
    end (* while *)
end (* HAUPTPROGRAMM *) .
