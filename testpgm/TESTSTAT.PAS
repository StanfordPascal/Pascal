program TESTSTAT ( OUTPUT ) ;

(********)
(*$A+   *)
(********)



var X : REAL ;
    I : INTEGER ;


type VOIDPTR = -> INTEGER ;
     ALFA = array [ 1 .. 40 ] of CHAR ;
     TESTREC = record
                 X1 : VOIDPTR ;
                 X2 : VOIDPTR ;
                 C1 : CHAR ;
                 C2 : CHAR ;
                 I1 : INTEGER ;
               end ;
     TESTSET = set of CHAR ;


const TCONST = 25 ;
      B : array [ 1 .. 4 ] of ALFA =
      ( 'A' , 'B' , 'C' , 'D' ) ;
      C : array [ 1 .. 4 ] of INTEGER =
      ( 1 , 2 , 3 , 4 ) ;


static X2 : INTEGER ;
       Y2 : INTEGER ;
       ABC : ALFA ;
       LAUF : INTEGER ;
       XR : TESTREC ;
       XS : TESTSET ;



procedure TSTAT ;

   static X : INTEGER ;
          Y : INTEGER ;

   const A : array [ 1 .. 4 ] of ALFA =
         ( 'A' , 'B' , 'C' , 'D' ) ;
         C : array [ 1 .. 4 ] of INTEGER =
         ( 1 , 2 , 3 , 4 ) ;


   procedure INTERN ;

      var I : INTEGER ;

      static X3 : INTEGER ;
             Y3 : INTEGER ;
             XF : INTEGER ;
             XX : REAL ;

      begin (* INTERN *)
        XX := SIN ( 0.7 ) ;
        X3 := X3 + 1 ;
        Y3 := Y3 + 1 ;
        XF := 5 ;
        WRITELN ( 'statisch mit Format aus stat: ' , X3 : XF ) ;
        WRITELN ( 'real mit Format aus stat: ' , XX : 13 : XF ) ;
        WRITELN ( 'Start Prozedur intern' ) ;
        WRITELN ( 'statisch x3 = ' , X3 ) ;
        WRITELN ( 'statisch y3 = ' , Y3 ) ;
        WRITELN ( 'statisch x = ' , X ) ;
        WRITELN ( 'statisch y = ' , Y ) ;
        WRITELN ( 'statisch x2 = ' , X2 ) ;
        X2 := X2 + 1 ;
        WRITELN ( 'statisch y2 = ' , Y2 ) ;
        Y2 := Y2 + 1 ;
        WRITELN ( 'abc = ' , ABC ) ;
        ABC := 'XYZ' ;
        WRITELN ( 'abc = ' , ABC ) ;
        WRITELN ( 'testrec:' ) ;
        WRITELN ( 'testrec.x1 = ' , XR . X1 ) ;
        WRITELN ( 'testrec.x2 = ' , XR . X2 ) ;
        WRITELN ( 'testrec.c1 = ' , ORD ( XR . C1 ) ) ;
        WRITELN ( 'testrec.c2 = ' , ORD ( XR . C2 ) ) ;
        WRITELN ( 'testrec.i1 = ' , XR . I1 ) ;
        WRITELN ( 'testset:' ) ;
        for I := 0 to 255 do
          if CHR ( I ) in XS then
            WRITELN ( 'testset enthaelt char ' , I ) ;
        WRITELN ( 'Ende Prozedur intern' ) ;
      end (* INTERN *) ;


   begin (* TSTAT *)
     WRITELN ( 'a1 = ' , A [ 1 ] ) ;
     WRITELN ( 'c1 = ' , C [ 1 ] ) ;
     WRITELN ( 'statisch x = ' , X ) ;
     X := X + 1 ;
     WRITELN ( 'statisch y = ' , Y ) ;
     Y := Y + 1 ;
     INTERN ;
     /* SNAPSHOT ( 3 , 10 ) ; */
   end (* TSTAT *) ;



begin (* HAUPTPROGRAMM *)
  X := SIN ( 3.141592654 / 2 ) ;
  X := 1.5 ;
  X := SIN ( X ) ;
  WRITELN ( 'SIN (1.5) = ' , X : 15 : 7 ) ;
  WRITELN ( 'SIN (1.0) = ' , SIN ( 1.0 ) : 15 : 7 ) ;
  WRITELN ( 'SIN (pi/2) = ' , SIN ( 3.141592654 / 2 ) : 15 : 7 ) ;
  MEMSET ( ADDR ( XR ) , '*' , SIZEOF ( TESTREC ) ) ;
  for I := 1 to 10 do
    TSTAT ;
  WRITELN ( 'b1 = ' , B [ 1 ] ) ;
  WRITELN ( 'c1 = ' , C [ 1 ] ) ;
  WRITELN ( 'statisch x2 = ' , X2 ) ;
  X2 := X2 + 1 ;
  WRITELN ( 'statisch y2 = ' , Y2 ) ;
  Y2 := Y2 + 1 ;
  WRITELN ( 'abc = ' , ABC ) ;
  ABC := 'XYZ' ;
  WRITELN ( 'abc = ' , ABC ) ;
  WRITELN ( 'testrec:' ) ;
  WRITELN ( 'testrec.x1 = ' , XR . X1 ) ;
  WRITELN ( 'testrec.x2 = ' , XR . X2 ) ;
  WRITELN ( 'testrec.c1 = ' , ORD ( XR . C1 ) ) ;
  WRITELN ( 'testrec.c2 = ' , ORD ( XR . C2 ) ) ;
  WRITELN ( 'testrec.i1 = ' , XR . I1 ) ;
  WRITELN ( 'testset:' ) ;
  for I := 0 to 255 do
    if CHR ( I ) in XS then
      WRITELN ( 'testset enthaelt char ' , I ) ;
end (* HAUPTPROGRAMM *) .
