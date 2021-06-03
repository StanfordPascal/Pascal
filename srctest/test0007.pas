program CASEEX ( OUTPUT ) ;

//*****
//$A+  
//*****



label 3 , 4 , 5 , 6 ;


var I : INTEGER ;
    X : REAL := 3.14 ;



procedure P ;

   begin (* P *)
     6 :
     X := 27 ;
     WRITELN ( 'inside P, x = ' , X : 5 : 1 ) ;
     goto 5 ;
     WRITELN ( 'inside P, after goto ??' ) ;
   end (* P *) ;



begin (* HAUPTPROGRAMM *)
  I := 0 ;
  case I of
    0 : X := 0 ;
    1 : X := SIN ( X ) ;
    2 : X := COS ( X ) ;
    3 .. 12 :
      X := EXP ( X ) ;
    401 : X := LN ( X ) ;
    otherwise
      X := SQR ( X )
  end (* case *) ;
  for I := 1 to 10 do
    begin
      X := 2 ;
      3 :
      X := X + 1
    end (* for *) ;
  if I < 10 then
    goto 3 ;
  if I > 5 then
    goto 4 ;
  if I < 5 then
    begin
      4 :
      I := 7 ;
      WRITELN ( 'after label 4, I = ' , I ) ;
    end (* then *) ;

  //***********
  // goto 6 ;
  //***********

  P ;
  5 :
  
end (* HAUPTPROGRAMM *) .
