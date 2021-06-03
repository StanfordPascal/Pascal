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



type TESTREC = record
                 A : INTEGER ;
                 B : STRING ( 10 ) ;
                 C : INTEGER ;
               end ;


const C = 17 ;
      C1 : INTEGER = 27 ;
      C2 : DECIMAL ( 7 , 2 ) = 12.5 ;
      C3 : BOOLEAN = TRUE ;
      C4 : CHAR ( 10 ) = 'HUGO' ;
      C5 : STRING ( 20 ) = 'TESTSTRING' ;
      F : array [ 1 .. 5 ] of CHAR =
      ( 'a' , 'b' , 'c' ) ;
      XC : record
             A : INTEGER ;
             B : INTEGER
           end =
           ( 12 , 15 ) ;
      CNEU = C ;
      FNEU = F ;
      CT : TESTREC =
      ( 2 , 'BERND' , 12 ) ;
      CM1 , CM2 : TESTREC =
      ( 2 , 'BERND' , 12 ) ;
      CM3 , CM4 = 47 ;
      CM5 = CM3 ;
      CM6 : TESTREC = CM1 ;
      CM7 : TESTREC =
      ( C , 'HUGO' , C ) ;
      CM8 = CM1 ;
      XX : array [ 1 .. 5 ] of TESTREC =
      ( CM1 , CM7 , CM2 , CM7 , CM1 ) ;
      YY : record
             PART1 : TESTREC ;
             PART2 : TESTREC
           end =
           ( CM1 , CM7 ) ;


var X : array [ 1 .. 14 ] of INTEGER ;
    I : INTEGER ;
    CV : INTEGER := 5 ;
    V1 : INTEGER = 27 ;
    V2 : DECIMAL ( 7 , 2 ) = 12.5 ;
    V3 : BOOLEAN = TRUE ;
    V4 : CHAR ( 10 ) = 'HUGO' ;
    V5 : STRING ( 20 ) = 'TESTSTRING' ;
    VX : array [ 1 .. 14 ] of INTEGER =
         ( 2 , 3 , 5 , 7 , 11 , 13 ) ;
    VREC1 , VREC2 : TESTREC :=
                    ( 1 , 'BERND' , 7 ) ;
    VT : TESTREC = CT ;
    VVS , VVS2 : record
                   A : INTEGER ;
                   B : INTEGER
                 end :=
                 ( 12 , 15 ) ;


static S1 : INTEGER = 27 ;
       S2 : DECIMAL ( 7 , 2 ) = 12.5 ;
       S3 : BOOLEAN = TRUE ;
       S4 : CHAR ( 10 ) = 'HUGO' ;
       S5 : STRING ( 20 ) = 'TESTSTRING' ;
       SX : array [ 1 .. 14 ] of INTEGER =
            ( 2 , 3 , 5 , 7 , 11 , 13 ) ;
       SREC1 , SREC2 : TESTREC :=
                       ( 1 , 'BERND' , 7 ) ;
       ST : TESTREC = CM7 ;
       SVS , SVS2 : record
                      A : INTEGER ;
                      B : INTEGER
                    end :=
                    ( 12 , 15 ) ;


begin (* HAUPTPROGRAMM *)
  WRITELN ( 'cm1.a = ' , CM1 . A , ' (should be 2)' ) ;
  WRITELN ( 'cm2.a = ' , CM2 . A , ' (should be 2)' ) ;
  WRITELN ( 'cm3   = ' , CM3 , ' (should be 47)' ) ;
  WRITELN ( 'cm4   = ' , CM4 , ' (should be 47)' ) ;
  WRITELN ( 'cm5   = ' , CM5 , ' (should be 47)' ) ;
  WRITELN ( 'cm6.a = ' , CM6 . A , ' (should be 2)' ) ;
  WRITELN ( 'cm7.a = ' , CM7 . A , ' (should be 17)' ) ;
  WRITELN ( 'cm7.c = ' , CM7 . C , ' (should be 17)' ) ;
  WRITELN ( 'cm8.a = ' , CM8 . A , ' (should be 2)' ) ;
  for I := 1 to 5 do
    WRITE ( XX [ I ] . A ) ;
  WRITELN ;
  WRITELN ( 'yy.1  = ' , YY . PART1 . A , ' (should be 2)' ) ;
  WRITELN ( 'yy.2  = ' , YY . PART2 . A , ' (should be 17)' ) ;
  WRITELN ( C1 , C2 , ' ' , C3 , ' ' , C4 , C5 ) ;
  WRITELN ( S1 , S2 , ' ' , S3 , ' ' , S4 , S5 ) ;
  for I := 1 to 14 do
    WRITE ( SX [ I ] : 3 ) ;
  WRITELN ;
  for I := 1 to 14 do
    WRITE ( VX [ I ] : 3 ) ;
  WRITELN ;
  WRITE ( 'array f = ' ) ;
  for I := 1 to 5 do
    WRITE ( F [ I ] : 3 ) ;
  WRITELN ;
  WRITE ( 'array fneu = ' ) ;
  for I := 1 to 5 do
    WRITE ( FNEU [ I ] : 3 ) ;
  WRITELN ;
  WRITELN ( 'srec1 = ' , SREC1 . A , ' ' , SREC1 . B : - 10 , SREC1 . C
            ) ;
  WRITELN ( 'srec2 = ' , SREC2 . A , ' ' , SREC2 . B : - 10 , SREC2 . C
            ) ;
  WRITELN ( 'vrec1 = ' , VREC1 . A , ' ' , VREC1 . B : - 10 , VREC1 . C
            ) ;
  WRITELN ( 'vrec2 = ' , VREC2 . A , ' ' , VREC2 . B : - 10 , VREC2 . C
            ) ;
  WRITELN ( 'svs   = ' , SVS . A , SVS . B ) ;
  WRITELN ( 'svs2  = ' , SVS2 . A , SVS2 . B ) ;
  for I := 1 to 14 do
    X [ I ] := 0 ;
  V5 := '' ;
  WRITELN ( V1 , V2 , ' ' , V3 , ' ' , V4 , V5 ) ;
  WRITELN ( 'vvs   = ' , VVS . A , VVS . B ) ;
  WRITELN ( 'vvs2  = ' , VVS2 . A , VVS2 . B ) ;
  WRITELN ( 'cv    = ' , CV , ' (should be 5)' ) ;
  with VVS do
    A := C ;
  WRITELN ( 'vvs   = ' , VVS . A , VVS . B ) ;
  WRITELN ( 'vvs2  = ' , VVS2 . A , VVS2 . B ) ;
  WRITELN ( 'vt    = ' , VT . A , ' ' , VT . B : - 10 , VT . C ) ;
  WRITELN ( 'st    = ' , ST . A , ' ' , ST . B : - 10 , ST . C ) ;

  //********************************
  // compiler should complain here  
  // and not modify the constant    
  //********************************

  C := 15 ;
  WRITELN ( 'c     = ' , C , ' (should be 17)' ) ;
  WRITELN ( 'cneu  = ' , CNEU ) ;
  CV := 7 ;
  WRITELN ( 'cv    = ' , CV ) ;
  SVS . A := 27 ;

  //********************************
  // compiler should complain here  
  // and not modify the constant    
  //********************************

  XC . A := SVS . A ;
  WRITELN ( 'xc.a  = ' , XC . A , ' (should be 12)' ) ;

  //********************************
  // compiler should complain here  
  // and not modify the constant    
  //********************************

  with XC do
    A := SVS . A ;
  WRITELN ( 'xc.a  = ' , XC . A , ' (should be 12)' ) ;
  WRITELN ( 'xc.b  = ' , XC . B ) ;
end (* HAUPTPROGRAMM *) .
