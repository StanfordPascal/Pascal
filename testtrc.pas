program TESTTRC ( OUTPUT ) ;

(********)
(*$a+   *)
(********)



var R : REAL ;
    X : REAL ;
    I : INTEGER ;
    C : array [ 1 .. 100 ] of CHAR ;



local procedure DUMP ( PVON : VOIDPTR ; PBIS : VOIDPTR ) ;

/*********************************************************/
/*  Speicherbereich von PVON bis PBIS hexadezimal        */
/*  ausgeben                                             */
/*********************************************************/


   var P1 : VOIDPTR ;
       P2 : VOIDPTR ;
       MOD1 : INTEGER ;
       MOD2 : INTEGER ;


   procedure DUMPCHAR ( CH : CHAR ) ;

      begin (* DUMPCHAR *)
        if CH in [ 'a' .. 'i' , 'j' .. 'r' , 's' .. 'z' , 'A' .. 'I' ,
        'J' .. 'R' , 'S' .. 'Z' , '0' .. '9' , ' ' , ',' , '.' , '-' ,
        ';' , ':' , '_' , '!' , '"' , 'õ' , '$' , '%' , '&' , '/' , '('
        , ')' , '=' , '?' , '+' , '*' , '#' , '*' ] then
          WRITE ( CH )
        else
          WRITE ( '.' )
      end (* DUMPCHAR *) ;


   procedure DUMPZEILE ( ADR : VOIDPTR ; P1 : VOIDPTR ; P2 : VOIDPTR )
                       ;

      var CH : -> CHAR ;
          I : INTEGER ;

      const HEXTAB : array [ 0 .. 15 ] of CHAR = '0123456789abcdef' ;

      begin (* DUMPZEILE *)
        WRITE ( ADR , ': ' ) ;
        CH := ADR ;
        if ( PTRDIFF ( ADR , P1 ) < 0 ) or ( PTRDIFF ( ADR , P2 ) > 0 )
        then
          WRITE ( '........ ' )
        else
          begin
            for I := 1 to 4 do
              begin
                WRITE ( HEXTAB [ ORD ( CH -> ) DIV 16 ] , HEXTAB [ ORD
                        ( CH -> ) MOD 16 ] ) ;
                CH := PTRADD ( CH , 1 ) ;
              end (* for *) ;
            WRITE ( ' ' ) ;
          end (* else *) ;
        ADR := PTRADD ( ADR , 4 ) ;
        CH := ADR ;
        if ( PTRDIFF ( ADR , P1 ) < 0 ) or ( PTRDIFF ( ADR , P2 ) > 0 )
        then
          WRITE ( '........ ' )
        else
          begin
            for I := 1 to 4 do
              begin
                WRITE ( HEXTAB [ ORD ( CH -> ) DIV 16 ] , HEXTAB [ ORD
                        ( CH -> ) MOD 16 ] ) ;
                CH := PTRADD ( CH , 1 ) ;
              end (* for *) ;
            WRITE ( ' ' ) ;
          end (* else *) ;
        ADR := PTRADD ( ADR , 4 ) ;
        CH := ADR ;
        if ( PTRDIFF ( ADR , P1 ) < 0 ) or ( PTRDIFF ( ADR , P2 ) > 0 )
        then
          WRITE ( '........ ' )
        else
          begin
            for I := 1 to 4 do
              begin
                WRITE ( HEXTAB [ ORD ( CH -> ) DIV 16 ] , HEXTAB [ ORD
                        ( CH -> ) MOD 16 ] ) ;
                CH := PTRADD ( CH , 1 ) ;
              end (* for *) ;
            WRITE ( ' ' ) ;
          end (* else *) ;
        ADR := PTRADD ( ADR , 4 ) ;
        CH := ADR ;
        if ( PTRDIFF ( ADR , P1 ) < 0 ) or ( PTRDIFF ( ADR , P2 ) > 0 )
        then
          WRITE ( '........ ' )
        else
          begin
            for I := 1 to 4 do
              begin
                WRITE ( HEXTAB [ ORD ( CH -> ) DIV 16 ] , HEXTAB [ ORD
                        ( CH -> ) MOD 16 ] ) ;
                CH := PTRADD ( CH , 1 ) ;
              end (* for *) ;
            WRITE ( ' ' ) ;
          end (* else *) ;
        ADR := PTRADD ( ADR , - 12 ) ;
        CH := ADR ;
        WRITE ( ' *' ) ;
        for I := 1 to 16 do
          begin
            DUMPCHAR ( CH -> ) ;
            CH := PTRADD ( CH , 1 )
          end (* for *) ;
        WRITELN ( '*' ) ;
      end (* DUMPZEILE *) ;


   begin (* DUMP *)
     WRITELN ( 'Dump Speicherbereich von ' , PVON , ' bis ' , PBIS ) ;
     P1 := PTRADD ( PVON , - 16 ) ;
     MOD1 := PTR2INT ( P1 ) MOD 16 ;
     P1 := PTRADD ( P1 , 16 - MOD1 ) ;
     P2 := PTRADD ( PBIS , 15 ) ;
     MOD2 := PTR2INT ( P2 ) MOD 16 ;
     P2 := PTRADD ( P2 , - MOD2 ) ;
     while PTRDIFF ( P1 , P2 ) < 0 do
       begin
         DUMPZEILE ( P1 , PVON , PBIS ) ;
         P1 := PTRADD ( P1 , 16 ) ;
       end (* while *) ;
   end (* DUMP *) ;



function TRUNC1 ( R : REAL ) : INTEGER ;

   begin (* TRUNC1 *)
     TRUNC1 := TRUNC ( R ) ;
   end (* TRUNC1 *) ;



begin (* HAUPTPROGRAMM *)
  R := 0.012345678912 ;
  WRITELN ( 'many fraction digits: ' , R : 20 ) ;
  MEMSET ( ADDR ( C ) , 'A' , 20 ) ;
  MEMSET ( PTRADD ( ADDR ( C ) , 20 ) , CHR ( 10 ) , 20 ) ;
  MEMCPY ( PTRADD ( ADDR ( C ) , 40 ) , ADDR ( R ) , SIZEOF ( REAL ) )
           ;
  MEMSET ( PTRADD ( ADDR ( C ) , 50 ) , CHR ( 0XCD ) , 20 ) ;
  DUMP ( ADDR ( C ) , PTRADD ( ADDR ( C ) , 99 ) ) ;
  R := 0.01 ;
  WRITELN ( 'should be 0.01: ' , R ) ;
  R := 0.0 ;
  R := TRUNC ( R ) + 1 ;
  WRITELN ( 'should be 1: ' , R : 7 : 4 ) ;
  R := 0.0 ;
  R := TRUNC1 ( R ) + 1 ;
  WRITELN ( 'should be 1: ' , R : 7 : 4 ) ;
  R := R + 0.5 ;
  R := ROUND ( R ) + 1 ;
  WRITELN ( 'should be 3: ' , R : 7 : 4 ) ;
  R := 0.0 ;
  R := 1 + TRUNC ( R ) ;
  WRITELN ( 'should be 1: ' , R : 7 : 4 ) ;
  R := R + 0.5 ;
  R := 1 + ROUND ( R ) ;
  WRITELN ( 'should be 3: ' , R : 7 : 4 ) ;
  WRITELN ( 'should write 3: ' , ROUND ( R ) : 7 ) ;
  R := 0.0067 ;
  WRITELN ( 'should be 0.0067: ' , R : 7 : 4 , R : 7 : 3 ) ;
  R := 12.30 ;
  while R <= 12.50 do
    begin
      WRITELN ( 'R unkorrigiert ..: ' , R : 7 : 2 , R : 7 : 1 , ROUNDX
                ( R , - 1 ) : 7 : 1 ) ;
      R := ROUNDX ( R , - 2 ) ;
      WRITELN ( 'R gerundet ......: ' , R : 7 : 2 , R : 7 : 1 , ROUNDX
                ( R , - 1 ) : 7 : 1 ) ;
      R := R + 0.01 ;
    end (* while *) ;
  for I := 2 DOWNTO - 14 do
    WRITELN ( 'test roundx (' , I : 3 , ') = ' , ROUNDX ( 12.543 *
              5.678 , I ) : 20 : 12 ) ;
  R := - 10.0 ;
  while R < 10.0 do
    begin
      WRITELN ( 'r = ' , R : 5 : 1 , ' trc = ' , TRUNC ( R ) : 3 ,
                ' RND = ' , ROUND ( R ) : 3 , ' FLR = ' , FLOOR ( R ) :
                5 : 1 ) ;
      R := R + 0.1 ;
      R := ROUND ( R * 10 ) / 10 ;
      R := ROUNDX ( R , - 1 ) ;
    end (* while *) ;
  if FALSE then
    begin
      R := - 10.0 ;
      while R < 10.0 do
        begin
          X := FLOOR ( R ) ;
          WRITELN ( 'Test Trunc:' ) ;
          DUMP ( ADDR ( R ) , PTRADD ( ADDR ( R ) , 7 ) ) ;
          DUMP ( ADDR ( X ) , PTRADD ( ADDR ( X ) , 7 ) ) ;
          R := R + 0.1 ;
          R := ROUNDX ( R , - 1 ) ;
        end (* while *)
    end (* then *)
end (* HAUPTPROGRAMM *) .
