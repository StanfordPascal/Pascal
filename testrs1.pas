program TESTRS1 ( INPUT , OUTPUT ) ;

//**********************************************************
//$X+                                                       
//**********************************************************



var S : STRING ( 200 ) ;
    CH : CHAR ;
    I : INTEGER ;
    F : CHAR ( 5 ) ;
    V : STRING ( 10 ) ;
    RPOS : INTEGER ;
    R : REAL ;



function $PASRSC ( const S : STRING ;         // source string
                 var RPOS : INTEGER ;         // reading position
                 WIDTH : INTEGER ) : CHAR ;   // optional width

   EXTERNAL ;



function $PASRSI ( const S : STRING ;         // source string
                 var RPOS : INTEGER ;         // reading position
                 WIDTH : INTEGER )            // optional width
                 : INTEGER ;

   EXTERNAL ;



function $PASRSR ( const S : STRING ;         // source string
                 var RPOS : INTEGER ;         // reading position
                 WIDTH : INTEGER )            // optional width
                 : REAL ;

   EXTERNAL ;



procedure $PASRSS ( const S : STRING ;     // source string
                  var RPOS : INTEGER ;     // reading position
                  WIDTH : INTEGER ;        // optional width
                  RES : VOIDPTR ;          // pointer to target
                  LEN : INTEGER ) ;        // len of target

   EXTERNAL ;



procedure $PASRSV ( const S : STRING ;     // source string
                  var RPOS : INTEGER ;     // reading position
                  WIDTH : INTEGER ;        // optional width
                  var RES : STRING ) ;     // target string

   EXTERNAL ;



begin (* HAUPTPROGRAMM *)
  S := '36 24  123.45678ABCDEFGHIJKLMNOPQRSTUVWXYZ' ;
  WRITELN ( 'source   s  = ' , S ) ;
  RPOS := 0 ;
  CH := $PASRSC ( S , RPOS , 2 ) ;
  I := $PASRSI ( S , RPOS , 4 ) ;
  R := $PASRSR ( S , RPOS , 8 ) ;
  $PASRSS ( S , RPOS , - 1 , ADDR ( F ) , 5 ) ;
  $PASRSV ( S , RPOS , 7 , V ) ;
  WRITELN ( 'ergebnis ch = ' , CH ) ;
  WRITELN ( 'ergebnis i  = ' , I : 1 ) ;
  WRITELN ( 'ergebnis r  = ' , R : 1 : 7 ) ;
  WRITELN ( 'ergebnis f  = ' , F ) ;
  WRITELN ( 'ergebnis v  = ' , V ) ;
end (* HAUPTPROGRAMM *) .
