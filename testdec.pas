program TESTDEC ( OUTPUT ) ;


const CONST1 = 1234.56 ;


type DEC72 = DECIMAL ( 7 , 2 ) ;
     DEC152 = DECIMAL ( 15 , 2 ) ;
     PERSON = record
                VORNAME : CHAR ( 20 ) ;
                NACHNAME : CHAR ( 20 ) ;
                ALTER : INTEGER ;
                GROESSE : DECIMAL ( 3 , 2 ) ;
                GEWICHT : DECIMAL ( 6 , 3 ) ;
              end ;


var D1 : DECIMAL ( 7 ) ;
    D2 : DECIMAL ( 15 , 2 ) ;
    D3 : DEC72 ;
    D4 : DECIMAL ( 25 , 4 ) ;
    P : PERSON ;



function BRUTTO ( X : DECIMAL ( 15 , 2 ) ) : DEC152 ;

   begin (* BRUTTO *)
     BRUTTO := ROUNDX ( X * 1.19 , - 2 ) ;
   end (* BRUTTO *) ;



function BRUTTO2 ( X : DECIMAL ( 15 , 2 ) ) : DECIMAL ( 15 , 2 ) ;

   begin (* BRUTTO2 *)
     BRUTTO2 := ROUNDX ( X * 1.19 , - 2 ) ;
   end (* BRUTTO2 *) ;



function NEXTCHAR ( C : CHAR ) : CHAR ( 1 ) ;

   begin (* NEXTCHAR *)
     NEXTCHAR := SUCC ( C ) ;
   end (* NEXTCHAR *) ;



procedure PRINT_PERSON ( var X : PERSON ) ;

   begin (* PRINT_PERSON *)
     WRITELN ( 'print_person: vorname  = ' , X . VORNAME ) ;
     WRITELN ( 'print_person: nachname = ' , X . NACHNAME ) ;
     WRITELN ( 'print_person: alter    = ' , X . ALTER ) ;
     WRITELN ( 'print_person: groesse  = ' , X . GROESSE ) ;
     WRITELN ( 'print_person: gewicht  = ' , X . GEWICHT ) ;
   end (* PRINT_PERSON *) ;



begin (* HAUPTPROGRAMM *)
  WRITELN ( '=====================================================' ) ;
  WRITELN ( 'Show different results of digitsof and precisionof' ) ;
  WRITELN ( '=====================================================' ) ;
  WRITELN ( 'digitsof (const)    = ' , DIGITSOF ( 1234 ) ) ;
  WRITELN ( 'precisionof (const) = ' , PRECISIONOF ( 1234 ) ) ;
  WRITELN ( 'digitsof (const)    = ' , DIGITSOF ( 1234.56 ) ) ;
  WRITELN ( 'precisionof (const) = ' , PRECISIONOF ( 1234.56 ) ) ;
  WRITELN ( 'sizeof (type)       = ' , SIZEOF ( DEC72 ) ) ;
  WRITELN ( 'digitsof (type)     = ' , DIGITSOF ( DEC72 ) ) ;
  WRITELN ( 'precisionof (type)  = ' , PRECISIONOF ( DEC72 ) ) ;
  WRITELN ( 'sizeof (d3)         = ' , SIZEOF ( D3 ) ) ;
  WRITELN ( 'digitsof (d3)       = ' , DIGITSOF ( D3 ) ) ;
  WRITELN ( 'precisionof (d3)    = ' , PRECISIONOF ( D3 ) ) ;
  WRITELN ( 'sizeof (d4)         = ' , SIZEOF ( D4 ) ) ;
  WRITELN ( 'digitsof (d4)       = ' , DIGITSOF ( D4 ) ) ;
  WRITELN ( 'precisionof (d4)    = ' , PRECISIONOF ( D4 ) ) ;
  WRITELN ( '=====================================================' ) ;
  WRITELN ( 'Test output of decimal variables using implicit width' ) ;
  WRITELN ( '=====================================================' ) ;
  D3 := 1234.56 ;
  D3 := D3 + 1234 ;
  WRITELN ( 'd3 = ' , D3 ) ;
  D2 := D3 ;
  WRITELN ( 'd2 = ' , D2 ) ;
  D4 := D3 ;
  WRITELN ( 'd4 = ' , D4 ) ;
  WRITELN ( '=====================================================' ) ;
  WRITELN ( 'Do some computations using decimal variables' ) ;
  WRITELN ( '=====================================================' ) ;
  WRITELN ( 'compute d4 * 1.19' ) ;
  D4 := D3 * 1.19 ;
  WRITELN ( 'd4 = ' , D4 ) ;
  WRITELN ( 'compute d4 * 1.19 and round to 2nd digit' ) ;
  D4 := ROUNDX ( D3 * 1.19 , - 2 ) ;
  WRITELN ( 'd4 = ' , D4 ) ;
  WRITELN ( 'compute d4 * 1.19 using function brutto' ) ;
  D4 := D3 ;
  WRITELN ( 'brutto     = ' , BRUTTO ( D4 ) ) ;
  WRITELN ( 'compute d4 * 1.19 using function brutto2' ) ;
  D4 := D3 ;
  WRITELN ( 'brutto2    = ' , BRUTTO2 ( D4 ) ) ;
  WRITELN ( '=====================================================' ) ;
  WRITELN ( 'output decimal constants' ) ;
  WRITELN ( '=====================================================' ) ;
  WRITELN ( 'const      = ' , 1234.56 ) ;
  WRITELN ( 'const1     = ' , CONST1 ) ;
  WRITELN ( '=====================================================' ) ;
  WRITELN ( 'use a record variable including the new types' ) ;
  WRITELN ( '=====================================================' ) ;
  WRITELN ( 'sizeof (p) = ' , SIZEOF ( P ) ) ;
  P . VORNAME := 'Bernd' ;
  P . NACHNAME := 'Oppolzer' ;
  P . ALTER := 58 ;
  P . GROESSE := 1.85 ;
  P . GEWICHT := 87.125 ;
  PRINT_PERSON ( P ) ;
end (* HAUPTPROGRAMM *) .
