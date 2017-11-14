program TESTFILE ( INPUT ) ;

(********)
(*$A+   *)
(********)



const UMGEBUNG = 'WIN' ;


type VEKTOR = array [ 1 .. 10 ] of INTEGER ;
     TREC = record
              KEY : INTEGER ;
              FELD : VEKTOR ;
            end ;


var I : INTEGER ;
    ANZ : INTEGER ;
    F : VEKTOR ;
    FANZ : INTEGER ;
    OUTFILE_INTERN : FILE of TREC ;
    TXTFILE_INTERN : TEXT ;
    X : VOIDPTR ;
    RBUF : TREC ;
    P : VOIDPTR ;
    PP : -> VOIDPTR ;
    AP : ANYPTR ;
    IP : -> INTEGER ;
    AF : ANYFILE ;
    R : REAL ;
    C : array [ 1 .. 4 ] of CHAR ;



procedure DUMP ( PVON : VOIDPTR ; PBIS : VOIDPTR ) ;

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



procedure COMPUTE ( X : INTEGER ) ;

   begin (* COMPUTE *)
     if X = 1 then
       F [ 1 ] := 1
     else
       if X = 2 then
         F [ 2 ] := 1
       else
         F [ X ] := F [ X - 1 ] + F [ X - 2 ]
   end (* COMPUTE *) ;



procedure SHIFT ;

   var I : INTEGER ;

   begin (* SHIFT *)
     for I := 1 to 9 do
       F [ I ] := F [ I + 1 ] ;
   end (* SHIFT *) ;



procedure WRITE_VEKTOR ;

   var X : TREC ;
       I : INTEGER ;

   begin (* WRITE_VEKTOR *)
     ANZ := ANZ + 1 ;
     X . KEY := ANZ ;
     X . FELD := F ;
     WRITE ( OUTFILE_INTERN , X ) ;
     WRITELN ( OUTPUT , 'Testausgabe WRITE_VEKTOR, Key = ' , ANZ ) ;
     for I := 1 to 5 do
       WRITE ( OUTPUT , F [ I ] ) ;
     WRITELN ;
     for I := 6 to 10 do
       WRITE ( OUTPUT , F [ I ] ) ;
     WRITELN ;
     WRITELN ( '----------------------------------' ,
               '----------------------------------' ) ;
   end (* WRITE_VEKTOR *) ;



procedure TESTCLOSE ( var I : INTEGER ; var X : ANYFILE ) ;

   begin (* TESTCLOSE *)

     /******************************************************/
     /*   if FALSE then                                    */
     /*     WRITELN ( 'file component: ' , X -> ) ;        */
     /******************************************************/

     CLOSE ( X ) ;
   end (* TESTCLOSE *) ;



begin (* HAUPTPROGRAMM *)
  REWRITE ( TXTFILE_INTERN ) ;
  WRITELN ( TXTFILE_INTERN , 'TEST TEST ' ) ;
  X := ADDR ( TXTFILE_INTERN ) ;
  WRITELN ( 'pasfcb txtfile = ' , X ) ;
  DUMP ( PTRCAST ( X ) , PTRADD ( X , 12 ) ) ;
  WRITELN ( 'dump pasfcb fertig' ) ;
  if UMGEBUNG = 'WIN' then
    begin
      PP := PTRCAST ( X ) ;
      P := PP -> ;
      DUMP ( P , PTRADD ( P , 300 ) ) ;
    end (* then *)
  else
    begin
      X := FILEFCB ( TXTFILE_INTERN ) ;
      WRITELN ( 'filefcb txtfile = ' , X ) ;
      DUMP ( PTRCAST ( X ) , PTRADD ( X , 120 ) ) ;
      WRITELN ( 'dump filefcb fertig' ) ;
    end (* else *) ;
  REWRITE ( OUTFILE_INTERN ) ;
  X := ADDR ( OUTFILE_INTERN ) ;
  WRITELN ( 'pasfcb outfile = ' , X ) ;
  DUMP ( PTRCAST ( X ) , PTRADD ( X , 54 ) ) ;
  WRITELN ( 'dump pasfcb fertig' ) ;
  if UMGEBUNG = 'WIN' then
    begin
      PP := PTRCAST ( X ) ;
      P := PP -> ;
      DUMP ( P , PTRADD ( P , 300 ) ) ;
    end (* then *)
  else
    begin
      X := FILEFCB ( OUTFILE_INTERN ) ;
      WRITELN ( 'filefcb outfile = ' , X ) ;
      DUMP ( PTRCAST ( X ) , PTRADD ( X , 120 ) ) ;
      WRITELN ( 'dump filefcb fertig' ) ;
    end (* else *) ;
  for I := 1 to 10 do
    F [ I ] := 0 ;
  ANZ := 0 ;
  FANZ := 0 ;
  while ANZ < 100 do
    begin
      if FANZ < 10 then
        begin
          FANZ := FANZ + 1 ;
          COMPUTE ( FANZ ) ;
          WRITE_VEKTOR ;
        end (* then *)
      else
        begin
          SHIFT ;
          COMPUTE ( FANZ ) ;
          WRITE_VEKTOR ;
        end (* else *)
    end (* while *) ;
  RESET ( OUTFILE_INTERN ) ;
  X := ADDR ( OUTFILE_INTERN ) ;
  WRITELN ( 'pasfcb outfile = ' , X ) ;
  DUMP ( PTRCAST ( X ) , PTRADD ( X , 54 ) ) ;
  WRITELN ( 'dump pasfcb fertig' ) ;
  if UMGEBUNG = 'WIN' then
    begin
      PP := PTRCAST ( X ) ;
      P := PP -> ;
      DUMP ( P , PTRADD ( P , 300 ) ) ;
    end (* then *)
  else
    begin
      X := FILEFCB ( OUTFILE_INTERN ) ;
      WRITELN ( 'filefcb outfile = ' , X ) ;
      DUMP ( PTRCAST ( X ) , PTRADD ( X , 120 ) ) ;
      WRITELN ( 'dump filefcb fertig' ) ;
    end (* else *) ;
  while not EOF ( OUTFILE_INTERN ) do
    begin
      WRITELN ( 'key = ' , OUTFILE_INTERN -> . KEY ) ;
      for I := 1 to 5 do
        WRITE ( OUTPUT , OUTFILE_INTERN -> . FELD [ I ] ) ;
      WRITELN ;
      for I := 6 to 10 do
        WRITE ( OUTPUT , OUTFILE_INTERN -> . FELD [ I ] ) ;
      WRITELN ;
      WRITELN ( '----------------------------------' ,
                '----------------------------------' ) ;
      GET ( OUTFILE_INTERN ) ;
    end (* while *) ;
  WRITELN ( 'neue Tests Anyptr usw.' ) ;
  WRITELN ( '----------------------' ) ;
  P := ADDR ( ANZ ) ;
  WRITELN ( 'p  = ' , P ) ;
  AP := P ;
  IP := P ;
  WRITELN ( 'ap = ' , AP ) ;
  WRITELN ( 'ip = ' , IP ) ;
  WRITELN ( 'ip -> = ' , IP -> ) ;

  /************************************/
  /* WRITELN ( 'ap -> = ' , AP -> ) ; */
  /************************************/

  TESTCLOSE ( I , TXTFILE_INTERN ) ;
  TESTCLOSE ( I , OUTFILE_INTERN ) ;
  TESTCLOSE ( I , OUTFILE_INTERN ) ;
end (* HAUPTPROGRAMM *) .
