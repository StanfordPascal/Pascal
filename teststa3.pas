program TESTSTA3 ( OUTPUT ) ;

//*************************************************************************************
//$A+,X+
//*************************************************************************************
// assignment to const compenent inside with should throw error
//-------------------------------------------------------------------------------------
// in withstatement: insert information about constant in display entry
// in selector: return information about component being constant to caller
// then assignment (for example) can throw error, when assignment to constant is done
// and same for passing to var parameters etc.
//*************************************************************************************


type testrec = record
                  a : integer ;
                  b : string (10);
                  c : integer ;
               end ;


const C = 17 ;
      c1 : integer = 27 ;
      c2 : decimal (7,2) = 12.5 ;
      c3 : boolean = true ;
      c4 : char (10 ) = 'HUGO';
      c5 : string (20) = 'TESTSTRING';
      f : array [1 .. 5] of char = ('a', 'b', 'c');
      XC : record A : INTEGER ;
                  B : INTEGER end = ( 12 , 15 ) ;
      cneu = c;
      fneu = f;
      ct : testrec = ( 2, 'BERND', 12 );
      cm1, cm2 : testrec = ( 2, 'BERND', 12 );
      cm3, cm4 = 47 ;
      cm5 = cm3 ;
      cm6 : testrec = cm1 ;
      CM7 : testrec = ( c , 'HUGO', c) ;
      cm8 = cm1 ;
      xx : array [1 .. 5] of testrec = ( cm1, cm7, cm2, cm7, cm1 );
      yy : record
              part1 : testrec ;
              part2 : testrec
           end
           = ( cm1, cm7 );


var X : array [ 1 .. 14 ] of INTEGER ;
    I : INTEGER ;
    CV : INTEGER := 5
         ;
    v1 : integer = 27 ;
    v2 : decimal (7,2) = 12.5 ;
    v3 : boolean = true ;
    v4 : char (10 ) = 'HUGO';
    v5 : string (20) = 'TESTSTRING';
    vX : array [ 1 .. 14 ] of INTEGER
    = ( 2, 3, 5, 7 , 11, 13 ) ;
    vrec1, vrec2 : testrec
    := ( 1, 'BERND', 7 );
    vt : testrec = ct ;
    vVS, vvs2 : record
                  A : INTEGER ;
                  B : INTEGER
                end
                := (12, 15)
                ;


static s1 : integer = 27 ;
       s2 : decimal (7,2) = 12.5 ;
       s3 : boolean = true ;
       s4 : char (10 ) = 'HUGO';
       s5 : string (20) = 'TESTSTRING';
       sX : array [ 1 .. 14 ] of INTEGER
       = ( 2, 3, 5, 7 , 11, 13 ) ;
       srec1, srec2 : testrec
       := ( 1, 'BERND', 7 );
       st : testrec = cm7 ;
       svs, svs2 : record
                     A : INTEGER ;
                     B : INTEGER
                   end
                   := (12, 15)
                   ;


begin (* HAUPTPROGRAMM *)
  WRITELN ( 'cm1.a = ', cm1.a , ' (should be 2)');
  WRITELN ( 'cm2.a = ', cm2.a , ' (should be 2)');
  WRITELN ( 'cm3   = ', cm3   , ' (should be 47)');
  WRITELN ( 'cm4   = ', cm4   , ' (should be 47)');
  WRITELN ( 'cm5   = ', cm5   , ' (should be 47)');
  WRITELN ( 'cm6.a = ', cm6.a , ' (should be 2)');
  WRITELN ( 'cm7.a = ', cm7.a , ' (should be 17)');
  WRITELN ( 'cm7.c = ', cm7.c , ' (should be 17)');
  WRITELN ( 'cm8.a = ', cm8.a , ' (should be 2)');
  for i := 1 to 5 do
    write ( xx [i].a );
  writeln ;
  WRITELN ( 'yy.1  = ', yy.part1.a , ' (should be 2)');
  WRITELN ( 'yy.2  = ', yy.part2.a , ' (should be 17)');
  WRITELN ( c1, c2, ' ', c3, ' ', c4, c5 );
  WRITELN ( s1, s2, ' ', s3, ' ', s4, s5 );
  for I := 1 to 14 do
    write ( sx [ I ] : 3 );
  writeln ;
  write ('array f = ');
  for I := 1 to 5 do
    write ( f [ I ] : 3 );
  writeln ;
  write ('array fneu = ');
  for I := 1 to 5 do
    write ( fneu [ I ] : 3 );
  writeln ;
  WRITELN ( 'srec1 = ' , srec1.a, ' ', srec1.b : -10, srec1.c);
  WRITELN ( 'srec2 = ' , srec2.a, ' ', srec2.b : -10, srec2.c);
  WRITELN ( 'svs   = ' , svs.a, svs.b );
  WRITELN ( 'svs2  = ' , svs2.a, svs2.b);
  for I := 1 to 14 do
    X [ I ] := 0 ;
  v5 := '';
  WRITELN ( v1, v2, ' ', v3, ' ', v4, v5 );
  WRITELN ( 'vvs   = ' , vvs.a, vvs.b );
  WRITELN ( 'vvs2  = ' , vvs2.a, vvs2.b);
  writeln ( 'cv    = ', cv , ' (should be 5)');
  with vvs do
    A := C ;
  WRITELN ( 'vvs   = ' , vvs.a, vvs.b );
  WRITELN ( 'vvs2  = ' , vvs2.a, vvs2.b);
  WRITELN ( 'vt    = ' , vt.a, ' ', vt.b : -10, vt.c);
  WRITELN ( 'st    = ' , st.a, ' ', st.b : -10, st.c);
  // C := 15 ;
  WRITELN ( 'c     = ' , C ) ;
  WRITELN ( 'cneu  = ' , cneu ) ;
  CV := 7 ;
  writeln ( 'cv    = ', cv );

  //********************************
  // svs . A := 27 ;
  //********************************

  with XC do
    A := svs . A ;

  //********************************
  // compiler should complain here
  // and not modify the constant
  //********************************

  WRITELN ( 'xc.a  = ' , XC . A ) ;
  WRITELN ( 'xc.b  = ' , XC . B ) ;
end (* HAUPTPROGRAMM *) .
