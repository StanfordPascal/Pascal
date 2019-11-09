program TESTSET6 ( OUTPUT ) ;

//*****
//$A+
//*****



type ORD1 = ( X0 , X1 , X2 , X3 , X4 , X5 , X6 , X7 , X8 , X9 , X10 ,
            X11 , X12 , X13 , X14 , X15 , X16 , X17 , X18 , X19 , X20 ,
            X21 , X22 , X23 , X24 , X25 ) ;
     ORD2 = ( Y0 , Y1 , Y2 , Y3 , Y4 , Y5 , Y6 , Y7 , Y8 , Y9 , Y10 ,
            Y11 , Y12 , Y13 , Y14 , Y15 , Y16 , Y17 , Y18 , Y19 , Y20 ,
            Y21 , Y22 , Y23 , Y24 , Y25 , Y26 , Y27 , Y28 , Y29 , Y30 ,
            Y31 , Y32 , Y33 , Y34 , Y35 , Y36 , Y37 , Y38 , Y39 , Y40 ,
            Y41 , Y42 , Y43 , Y44 , Y45 , Y46 , Y47 , Y48 , Y49 ) ;
     SET1 = set of ORD1 ;
     SET2 = set of ORD2 ;
     SETTY = set of 0 .. 255 ;


var S : SETTY ;
    X , Y : INTEGER ;
    VX1 , VX2 : ORD1 ;
    VY1 , VY2 : ORD2 ;
    S1 : SET1 ;
    S2 : SET2 ;
    PX : -> INTEGER ;



procedure PRINTSET ( const X : STRING ; const S : SETTY ) ;

   var I : INTEGER ;

   begin (* PRINTSET *)
     WRITE ( X , ': ' ) ;
     for I := 0 to 63 do
       if I in S then
         WRITE ( I : 3 ) ;
     WRITELN
   end (* PRINTSET *) ;



procedure PRINTSET1 ( const X : STRING ; const S : SET1 ) ;

   var I : ORD1 ;

   begin (* PRINTSET1 *)
     WRITE ( X , ': ' ) ;
     for I := X0 to X25 do
       if I in S then
         WRITE ( I : 4 ) ;
     WRITELN
   end (* PRINTSET1 *) ;



procedure PRINTSET2 ( const X : STRING ; const S : SET2 ) ;

   var I : ORD2 ;

   begin (* PRINTSET2 *)
     WRITE ( X , ': ' ) ;
     for I := Y0 to Y49 do
       if I in S then
         WRITE ( I : 4 ) ;
     WRITELN
   end (* PRINTSET2 *) ;



begin (* HAUPTPROGRAMM *)
  S := [ 0 .. 12 ] ;
  PRINTSET ( '0 bis 12' , S ) ;
  S := S + [ 15 ] ;
  PRINTSET ( '15 dazu ' , S ) ;
  S := S + [ 17 , 20 .. 22 , 25 ] ;
  PRINTSET ( '+ mehr  ' , S ) ;
  X := 20 ;
  Y := 23 ;
  S := [ Y ] ;
  PRINTSET ( 'nur 23  ' , S ) ;
  S := [ X ] + [ Y ] ;
  PRINTSET ( '20 u. 23' , S ) ;
  S := [ ] ;
  S := S + [ X ] ;
  PRINTSET ( '20      ' , S ) ;
  S := S + [ Y ] ;
  PRINTSET ( '23 dazu ' , S ) ;
  S := [ 12 .. Y ] ;
  PRINTSET ( '12 .. 23' , S ) ;
  S := [ X .. Y ] ;
  PRINTSET ( '20 .. 23' , S ) ;
  S := S + [ 12 .. Y ] ;
  PRINTSET ( '12 .. 23' , S ) ;
  S := [ ] ;
  Y := 5 ;
  S := S + [ Y .. 12 ] ;
  PRINTSET ( '5 bis 12' , S ) ;
  S := [ ] ;
  S := S + [ X .. Y ] ;
  PRINTSET ( 'leer ?  ' , S ) ;
  VX1 := X5 ;
  VX2 := X12 ;
  S1 := [ X3 .. X15 ] ;
  PRINTSET1 ( '3 bis 15' , S1 ) ;
  S1 := [ VX1 ] ;
  PRINTSET1 ( 'nur 5   ' , S1 ) ;
  S1 := [ VX1 .. VX2 ] ;
  PRINTSET1 ( '5 bis 12' , S1 ) ;
  VY1 := Y5 ;
  VY2 := Y34 ;
  S2 := [ VY1 ] + [ VY2 ] ;
  PRINTSET2 ( '5 + 34  ' , S2 ) ;
  S2 := [ VY1 .. VY2 ] ;
  PRINTSET2 ( '5 .. 34 ' , S2 ) ;

  //***************
  // test dispose
  //***************

  NEW ( PX ) ;
  PX -> := 12 ;
  WRITELN ( 'px -> = ' , PX -> ) ;
  dispose ( PX ) ;
  WRITELN ( 'px -> = ' , PX -> ) ;
end (* HAUPTPROGRAMM *) .
