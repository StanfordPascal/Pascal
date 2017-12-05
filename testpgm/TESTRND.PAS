program TESTRND ( OUTPUT ) ;

(********)
(*$A+   *)
(********)



var R : REAL ;
    I : INTEGER ;



local procedure DUMP ( PVON : VOIDPTR ; PBIS : VOIDPTR ) ;

(*********************************************************)
(*  Speicherbereich von PVON bis PBIS hexadezimal        *)
(*  ausgeben                                             *)
(*********************************************************)


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



begin (* HAUPTPROGRAMM *)

  /******************/
  /* positiver wert */
  /******************/

  R := 12.5 ;
  DUMP ( ADDR ( R ) , PTRADD ( ADDR ( R ) , 7 ) ) ;
  R := R * 4724731.123456 ;
  R := R / 4724731.123456 ;
  DUMP ( ADDR ( R ) , PTRADD ( ADDR ( R ) , 7 ) ) ;
  WRITELN ( 'r vor round   = ' , R : 15 : 7 ) ;
  I := ROUND ( R ) ;
  WRITELN ( 'i nach round  = ' , I : 13 ) ;
  I := TRUNC ( R ) ;
  WRITELN ( 'i nach trunc  = ' , I : 13 ) ;
  R := FLOOR ( R ) ;
  WRITELN ( 'r nach floor  = ' , R : 15 : 7 ) ;
  R := 12.357 ;
  WRITELN ( 'r vor roundx  = ' , R : 15 : 7 ) ;
  R := ROUNDX ( R , - 2 ) ;
  WRITELN ( 'r nach roundx = ' , R : 15 : 7 ) ;

  /******************/
  /* negativer wert */
  /******************/

  R := - 12.5 ;
  DUMP ( ADDR ( R ) , PTRADD ( ADDR ( R ) , 7 ) ) ;
  R := R * 4724731.123456 ;
  R := R / 4724731.123456 ;
  DUMP ( ADDR ( R ) , PTRADD ( ADDR ( R ) , 7 ) ) ;
  WRITELN ( 'r vor round   = ' , R : 15 : 7 ) ;
  I := ROUND ( R ) ;
  WRITELN ( 'i nach round  = ' , I : 13 ) ;
  I := TRUNC ( R ) ;
  WRITELN ( 'i nach trunc  = ' , I : 13 ) ;
  R := FLOOR ( R ) ;
  WRITELN ( 'r nach floor  = ' , R : 15 : 7 ) ;
  R := - 12.357 ;
  WRITELN ( 'r vor roundx  = ' , R : 15 : 7 ) ;
  R := ROUNDX ( R , - 2 ) ;
  WRITELN ( 'r nach roundx = ' , R : 15 : 7 ) ;
end (* HAUPTPROGRAMM *) .
