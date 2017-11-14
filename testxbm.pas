program TESTXB ( OUTPUT ) ;

(********)
(*$A+   *)
(********)



const CC = 'A' ;
      CHEX = X'87' ;
      CSET =
      [ 'a' .. 'e' ] ;
      CSETHEX =
      [ 'a' .. X'86' ] ;


type SET_CHAR = set of CHAR ;
     ALPHA = array [ 1 .. 20 ] of CHAR ;


var C : CHAR ;
    S1 : SET_CHAR ;
    S2 : SET_CHAR ;
    S3 : SET_CHAR ;
    S4 : SET_CHAR ;
    S5 : SET_CHAR ;
    I : INTEGER ;


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
        X'f0f1fafbfdfe' , B'101101011011' , B'0100011001001100' ,
        '            ' , '            ' ) ;



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
  C := 'A' ;
  WRITELN ( 'c = <' , C , '>' , ORD ( C ) ) ;
  C := X'f1' ;
  WRITELN ( 'c = <' , C , '>' , ORD ( C ) ) ;
  C := B'11110011' ;
  WRITELN ( 'c = <' , C , '>' , ORD ( C ) ) ;
  C := CHEX ;
  WRITELN ( 'c = <' , C , '>' , ORD ( C ) ) ;
  case C of
    '1' : WRITELN ( 'eins' ) ;
    '5' : WRITELN ( 'fuenf' ) ;
    X'f2' : WRITELN ( 'nochmal zwei' ) ;
    X'f3' : WRITELN ( 'drei als hex' ) ;
    B'10000110' :
      WRITELN ( 'f binaer' ) ;
    X'C1' .. 'E' :
      WRITELN ( 'a als hex' ) ;
    CHEX : WRITELN ( 'konstante chex' ) ;
  end (* case *) ;
  WRITELN ( 'text normal' ) ;
  WRITELN ( X'c1c2c3c4c5_c1cbccc4e' ) ;
  WRITELN ( X'c1c2c3c4c5_c1cbcccdce' ) ;
  WRITELN ( B'11001111_11000011' ) ;
  WRITELN ( 'chex = ' , CHEX ) ;
  S1 := [ 'A' .. 'E' ] ;
  S2 := [ X'c1' .. 'E' ] ;

  /**********************************/
  /* will not work on ASCII machine */
  /* s := [ x'C1' .. 'E' ];         */
  /**********************************/

  S3 := [ X'c1' .. X'c5' ] ;

  /***********************************/
  /* will not work on EBCDIC machine */
  /***********************************/

  S4 := [ 'A' .. X'c5' ] ;
  S5 := [ 'a' .. CHEX ] ;
  PRINT_SET ( S1 ) ;
  PRINT_SET ( S2 ) ;
  PRINT_SET ( S3 ) ;
  PRINT_SET ( S4 ) ;
  PRINT_SET ( S5 ) ;
  PRINT_SET ( CSETHEX ) ;
  for I := 44 to 48 do
    WRITELN ( RW [ I ] ) ;
end (* HAUPTPROGRAMM *) .
