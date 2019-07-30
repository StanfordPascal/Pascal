program TESTW2 ( OUTPUT ) ;

//********************************
//$X+
//********************************



type S1 = record
            X : CHAR ( 27 ) ;
            case INTEGER of
              1 :
                ( V : INTEGER ) ;
              2 :
                ( W : REAL ) ;
              3 :
                ( Z : CHAR ) ;
              4 :
                ( case BOOLEAN of
                    TRUE :
                      ( A1 : CHAR ( 5 ) ) ;
                    FALSE :
                      ( A2 : CHAR ( 7 ) ) )
          end ;
     S2 = record
            A : INTEGER ;
            with B : -> S1 ;
            C : INTEGER ;
          end ;


var S : S2 ;



procedure WORK ( var PARM : S2 ) ;

   begin (* WORK *)
     WRITELN ( 'work1: ' , PARM . X ) ;
     PARM . A := 3 ;
     PARM . X := 'Hugo' ;
     PARM . Z := '*';
     WRITELN ( 'work2: ' , PARM . X ) ;
     WRITELN ( 'work3: ' , PARM . Z ) ;
     PARM . X := 'Bernd' ;
     with PARM do
       begin
         WRITELN ( 'work1: ' , X ) ;
         A := 3 ;
         X := 'Hugo' ;
         Z := '*';
         WRITELN ( 'work2: ' , X ) ;
         WRITELN ( 'work3: ' , Z ) ;
       end (* with *)
   end (* WORK *) ;



begin (* HAUPTPROGRAMM *)
  NEW ( S . B ) ;
  S . A := 1 ;
  S . X := 'Bernd' ;
  S . C := 3 ;
  S . V := 5 ;
  S . Z := '*' ;
  S . B -> . V := 5 ;
  WRITELN ( S . X , S . A ) ;
  WORK ( S ) ;
  WRITELN ( S . X , S . A ) ;
end (* HAUPTPROGRAMM *) .
