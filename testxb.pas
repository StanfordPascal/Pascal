program TESTXB ( OUTPUT ) ;

//**********************************************************
//$A+
//
// Testprogramm fuer hexadezimale und binaere Codierung
// von Integers und Strings
//
//**********************************************************


const CC = 'A' ;
      CHEX = X'67' ;
      CSET =
      [ 'a' .. 'e' ] ;
      CSETHEX =
      [ 'a' .. X'66' ] ;    // hier Problem bei IBM


//****************************************************
//* verschiedene tests, z.B. wird diese konstante
//* csethex auf IBM nicht funktionieren, da 'a' schon
//* groesser ist als x'66'
//****************************************************

type SET_CHAR = set of CHAR ;
     ALPHA = array [ 1 .. 20 ] of CHAR ;


var C : CHAR ;
    S1 : SET_CHAR ;
    S2 : SET_CHAR ;
    S3 : SET_CHAR ;
    S4 : SET_CHAR ;
    S5 : SET_CHAR ;
    S6 : SET_CHAR ;
    I : INTEGER ;
    testf : text ;


const RW : array [ 1 .. 50 ] of ALPHA =
      ( 'IF          ' , 'DO          ' , 'OF          ' ,
        'TO          ' , 'IN          ' , 'OR          ' ,
        'END         ' , 'FOR         ' , 'VAR         ' ,
        'DIV         ' , 'MOD         ' , 'SET         ' ,
        'AND         ' , 'NOT         ' , 'XOR         ' ,
        'THEN        ' , 'ELSE        ' , 'WITH        ' ,
        'GOTO        ' , 'CASE        ' , 'TYPE        ' ,
        'FILE        ' , 'BEGIN       ' , 'UNTIL       ' ,
        'WHILE       ' , 'ARRAY       ' , 'CONST       ' ,
        'LABEL       ' , 'LOCAL       ' , 'BREAK       ' ,
        'REPEAT      ' , 'RECORD      ' , 'DOWNTO      ' ,
        'PACKED      ' , 'RETURN      ' , 'MODULE      ' ,
        'STATIC      ' , 'FORWARD     ' , 'PROGRAM     ' ,
        'FORTRAN     ' , 'EXTERNAL    ' , 'FUNCTION    ' ,
        'CONTINUE    ' , 'PROCEDURE   ' , 'OTHERWISE   ' ,
        X'30313a3b3d3e' , B'101101011011' , B'0100011001001100' ,
        '            ' , '            ' ) ;
   testt : array [ 1..5 ] of char  =
        ( 'A', 'B', 'C', 'D', X'8a' );



procedure PRINT_SET ( S : SET_CHAR ) ;

   var C : CHAR ;
       CP : -> CHAR ;

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



begin (* HAUPTPROGRAMM *)

  // if eof (scaninp) then
  //    writeln ('dateiende scaninp');
  // if eof (testf) then
  //    writeln ('dateiende testf');
  // writeln (scanout, 'testausgabe');

  for i := 1 to 5 do
     writeln ( 'testt [', i:1, '] = ', testt [i], ord (testt [i]));
  C := 'A' ;
  WRITELN ( 'c = <' , C , '>' , ORD ( C ) ) ;
  C := X'31' ;
  WRITELN ( 'c = <' , C , '>' , ORD ( C ) ) ;
  C := B'110011' ;
  WRITELN ( 'c = <' , C , '>' , ORD ( C ) ) ;
  C := CHEX ;
  WRITELN ( 'c = <' , C , '>' , ORD ( C ) ) ;
  I := 0b0001_1001_0001_0001 ;
  writeln ( 'I = ', i );
  I := 0b1111_1111_1111_1111 ;
  writeln ( 'I = ', i );
  I := 0b0000_0000_0000_0110 ;
  writeln ( 'I = ', i );
  I := 0x7fffffff;
  writeln ( 'I = ', i );

  // I := 0x80000000;
  // writeln ( 'I = ', i );

  I := 0xffffff;
  writeln ( 'I = ', i );

//****************************************************
//* Kollissionen bei case - doppelte case-tags -
//* auf beiden Plattformen
//****************************************************

  case C of
    '1' : WRITELN ( 'eins' ) ;
    '5' : WRITELN ( 'fuenf' ) ;
    X'31' : WRITELN ( 'nochmal eins' ) ;
    X'33' : WRITELN ( 'drei als hex' ) ;
    X'34' : WRITELN ( 'vier als hex' ) ;
    B'1000110' :
      WRITELN ( 'f binaer' ) ;
    X'41' .. 'E' :
      WRITELN ( 'a als hex' ) ;
    CHEX : WRITELN ( 'konstante chex' ) ;
  end (* case *) ;
  WRITELN ( 'text normal' ) ;
  WRITELN ( X'4142434445_414b4c44e' ) ;
  WRITELN ( X'4142434445_414b4c4d4e' ) ;
  WRITELN ( B'01001111_01000011' ) ;
  WRITELN ( 'chex = ' , CHEX ) ;
  S1 := [ 'A' .. 'E' ] ;
  S2 := [ X'41' .. 'E' ] ;

  /**********************************/
  /* will not work on ASCII machine */
  /* s := [ x'C1' .. 'E' ];         */
  /**********************************/

  S3 := [ X'41' .. X'45' ] ;

  /***********************************/
  /* will not work on EBCDIC machine */
  /***********************************/

  S4 := [ 'A' .. X'45' ] ;
  S5 := [ 'a' .. CHEX ] ;
  S6 := CSETHEX ;
  PRINT_SET ( S1 ) ;
  PRINT_SET ( S2 ) ;
  PRINT_SET ( S3 ) ;
  PRINT_SET ( S4 ) ;
  PRINT_SET ( S5 ) ;
  PRINT_SET ( S6 ) ;
  PRINT_SET ( CSETHEX ) ;
  for I := 44 to 48 do
    WRITELN ( RW [ I ] ) ;
end (* HAUPTPROGRAMM *) .
