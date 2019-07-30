program TESTW2 ( OUTPUT ) ;

//********************************
//$X+                             
//********************************



type S1 = record
            X : CHAR ( 27 ) ;
            V : INTEGER ;
            W : REAL ;
          end ;
     S2 = record
            A : INTEGER ;
            with B : S1 ;
            C : INTEGER ;
          end ;


var S : S2 ;


const C1 : S2 =
      ( 1 , ( 'Bernd' , 12 , 3.4 ) , 5 ) ;


begin (* HAUPTPROGRAMM *)
  S := C1 ;
  WRITELN ( S . X , S . A , S . C ) ;
end (* HAUPTPROGRAMM *) .
