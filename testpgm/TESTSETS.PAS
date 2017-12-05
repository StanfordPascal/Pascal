program TESTSETS ( OUTPUT ) ;


const CH_CONST = X'4f' ;
      CH_BLANK = X'20' ;
      I_NEGATIV1 = - 13 ;
      I_NEGATIV2 = - 15 ;


type OPTYPE = ( PCTS , PCTI , PLOD , PSTR , PLDA , PLOC , PSTO , PLDC ,
              PLAB , PIND , PINC , PPOP , PCUP , PENT , PRET , PCSP ,
              PIXA , PEQU , PNEQ , PGEQ , PGRT , PLEQ , PLES , PUJP ,
              PFJP , PXJP , PCHK , PNEW , PADI , PADR , PSBI , PSBR ,
              PSCL , PFLT , PFLO , PTRC , PNGI , PNGR , PSQI , PSQR ,
              PABI , PABR , PNOT , PAND , PIOR , PDIF , PINT , PUNI ,
              PINN , PMOD , PODD , PMPI , PMPR , PDVI , PDVR , PMOV ,
              PLCA , PDEC , PSTP , PSAV , PRST , PCHR , PORD , PDEF ,
              PRND , PCRD , PXPO , PBGN , PEND , PASE , PSLD , PSMV ,
              PMST , PUXJ , PXLB , PCST , PDFC , PPAK , PADA , PSBA ,
              UNDEF_OP ) ;
     SET1 = set of CHAR ;
     SET2 = set of 'A' .. 'I' ;
     SET3 = set of '0' .. '9' ;
     FARBE = ( ROT , GELB , GRUEN , BLAU ) ;
     SET4 = set of FARBE ;
     SET5 = set of 10 .. 50 ;
     SET6 = set of 0 .. 255 ;
     SET7 = set of 100 .. 200 ;
     SET8 = set of 10000 .. 11000 ;
     SET9 = set of 300 .. 400 ;
     SET10 = set of 0 .. 300 ;
     SET11 = set of - 20 .. - 10 ;
     SET12 = set of OPTYPE ;
     TARRAY = array [ - 20 .. - 10 ] of INTEGER ;


const X1 : SET1 =
      [ 'A' .. 'J' , 'S' .. 'Z' ] ;
      X2 : SET1 =
      [ '3' .. '7' ] ;
      X3 : SET8 =
      [ 10500 .. 10600 ] ;
      X4 : array [ 1 .. 3 ] of SET8 =
      ( [ 10100 .. 10200 ] , [ 10220 .. 10300 ] , [ 10500 .. 10600 ] )
        ;
      X5 : array [ 1 .. 5 ] of SET1 =
      ( [ '0' .. '9' ] , [ 'A' .. 'F' ] , [ 'J' .. 'M' ] , [ '0' .. '5'
        ] , [ 'S' .. 'V' ] ) ;


var S1 : SET1 ;
    S2 : SET2 ;
    S3 : SET3 ;
    S4 : SET4 ;
    S5 : SET5 ;
    S6 : SET6 ;
    S : SET6 ;
    S7 : SET7 ;
    S8 : SET8 ;
    S9 : SET9 ;
    S10 : SET10 ;
    S11 : SET11 ;
    S12 : SET12 ;
    I : INTEGER ;
    CH : CHAR ;
    R : REAL ;
    S21 : SET1 ;
    S24 : SET4 ;



procedure PRINT_SET ( S : SET1 ) ;

   var C : CHAR ;
       CP : -> CHAR ;
       I : INTEGER ;

   begin (* PRINT_SET *)
     WRITE ( 'set: ' ) ;
     for C := CHR ( 0 ) to CHR ( 255 ) do
       if C in S then
         WRITE ( C ) ;
     WRITELN ;
     WRITE ( 'set in hex: ' ) ;
     CP := ADDR ( S ) ;
     for I := 1 to 32 do
       begin
         WRITE ( ORD ( CP -> ) : 1 , ' ' ) ;
         CP := PTRADD ( CP , 1 ) ;
       end (* for *) ;
     WRITELN ;
   end (* PRINT_SET *) ;



procedure PRINT_SET2 ( S : SET6 ) ;

   var C : INTEGER ;
       CP : -> CHAR ;
       I : INTEGER ;

   begin (* PRINT_SET2 *)
     WRITE ( 'set: ' ) ;
     for C := 0 to 255 do
       if C in S then
         WRITE ( C ) ;
     WRITELN ;
     WRITE ( 'set in hex: ' ) ;
     CP := ADDR ( S ) ;
     for I := 1 to 32 do
       begin
         WRITE ( ORD ( CP -> ) : 1 , ' ' ) ;
         CP := PTRADD ( CP , 1 ) ;
       end (* for *) ;
     WRITELN ;
   end (* PRINT_SET2 *) ;



begin (* HAUPTPROGRAMM *)
  WRITELN ( '-13 div 8 = ' , - 13 DIV 8 ) ;
  WRITELN ( '-13 mod 8 = ' , - 13 MOD 8 ) ;
  I := - 17 + 13 ;
  WRITELN ( 'i sollte -4 sein: ' , I ) ;
  R := - 3.2 + 0.4 ;
  WRITELN ( 'r sollte -2.8 sein: ' , R : 6 : 1 ) ;
  CH := 'A' ;
  S1 := [ 'J' .. 'R' , CH ] ;
  CH := 'A' ;
  S1 := [ 'B' , CH , 'A' .. 'R' , CH ] ;
  CH := 'A' ;
  S1 := X1 ;
  S1 := X2 ;

  /*********************************/
  /* S1 := [ 'B', CH .. 'R', CH ]; */
  /*********************************/

  CH := 'A' ;

  /*******************************/
  /* S1 := [ 'B' , 'A' .. CH ] ; */
  /*******************************/

  S1 := [ 'B' , 'A' .. CH_CONST ] ;
  S1 := [ 'B' , CH_BLANK .. 'A' ] ;
  S1 := [ 'J' .. 'R' ] ;

  (**********************************)
  (* PRINT_SET ( [ 'A' .. 'D' ] ) ; *)
  (**********************************)

  PRINT_SET ( S1 ) ;
  CH := 'X' ;
  S1 := [ CH ] ;
  PRINT_SET ( S1 ) ;
  S2 := [ 'C' .. 'E' ] ;
  PRINT_SET ( S2 ) ;
  S3 := [ '1' .. '6' ] ;
  S3 := S3 + [ '7' ] ;
  S3 := S3 - [ '5' ] ;
  PRINT_SET ( S3 ) ;
  S4 := [ GELB , BLAU ] ;
  S5 := [ 20 .. 30 ] ;
  S := S5 ;
  PRINT_SET2 ( S ) ;
  S6 := S5 ;
  S5 := S6 ;
  S := S6 ;
  PRINT_SET2 ( S ) ;
  S7 := [ 120 .. 140 ] ;
  S := S7 ;
  PRINT_SET2 ( S ) ;
  S11 := [ I_NEGATIV1 , I_NEGATIV2 ] ;
  S11 := [ - 13 , - 15 , - 17 .. - 12 ] ;
  S12 := [ PADI , PADA ] ;
  for I := - 20 to - 10 do
    if I in S11 then
      WRITE ( I : 1 ) ;
  WRITELN ;
  S8 := [ 10100 .. 10200 , 10300 .. 10400 ] ;

  /**************************/
  /* weitere aktionen: term */
  /**************************/

  S1 := X1 ;
  S1 := [ 'B' .. 'F' , 'T' , 'V' ] ;
  S1 := [ x'42' .. 'F' , 'T' , 'V' ] ;
  S1 := X1 * [ 'B' .. 'F' , 'T' , 'V' ] ;
  S1 := S21 ;
  S4 := [ GELB , BLAU ] ;
  S24 := [ ROT ] ;
  S4 := [ GELB , BLAU ] * S24 ;
end (* HAUPTPROGRAMM *) .
