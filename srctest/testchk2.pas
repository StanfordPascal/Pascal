program TESTCHK2 ( OUTPUT ) ;

(********)
(*$A+   *)
(********)



type SUBRG = 10 .. 30 ;
     SUBCHAR = 'A' .. 'Z' ;


var X : SUBRG ;
    C : SUBCHAR ;



procedure TERMIN ( var X : TEXT ) ;

   EXTERNAL ;



procedure CHKTEST ( X : SUBCHAR ) ;

   begin (* CHKTEST *)
     WRITELN ( 'in chktest: c = ' , X ) ;
   end (* CHKTEST *) ;



begin (* HAUPTPROGRAMM *)
  TERMIN ( INPUT ) ;
  WRITELN ( 'bitte zahl eingeben (10 .. 30)' ) ;
  READLN ( X ) ;
  WRITELN ( 'x = ' , X ) ;
  WRITELN ( 'bitte buchstabe eingeben (A .. Z)' ) ;
  READLN ( C : 5 ) ;
  WRITELN ( 'c = ' , C ) ;
  WRITELN ( 'c = ' , ORD ( C ) ) ;
  CHKTEST ( 'x' ) ;
  C := 'p' ;
  WRITELN ( 'c = ' , C ) ;
end (* HAUPTPROGRAMM *) .
