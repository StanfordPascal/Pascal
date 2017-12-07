program TESTASM ( OUTPUT ) ;

//**********************************************************************
//                                                                      
// test program to test calls of external subroutines                   
// from Stanford Pascal                                                 
// written in ASSEMBLER and FORTRAN (and Pascal)                        
//                                                                      
// Bernd Oppolzer - 12.2017                                             
//                                                                      
//$A+                                                                   
//**********************************************************************



type CHAR20 = array [ 1 .. 20 ] of CHAR ;


var I : INTEGER ;
    N : INTEGER ;
    C1 : CHAR20 ;
    C2 : CHAR20 ;
    XRES : INTEGER ;
    WINDOWS : BOOLEAN ;



procedure PASCAL_TO_PASCAL ( X1 : INTEGER ; var X2 : INTEGER ; T1 :
                           CHAR20 ; var T2 : CHAR20 ) ;

   EXTERNAL 'PAS2PAS' ;



procedure PASCAL_TO_ASSEMBLER ( X1 : INTEGER ; var X2 : INTEGER ; T1 :
                              CHAR20 ; var T2 : CHAR20 ) ;

   EXTERNAL ASSEMBLER 'PAS2ASM' ;



procedure PASCAL_TO_FORTRAN ( X1 : INTEGER ; var X2 : INTEGER ; T1 :
                            CHAR20 ; var T2 : CHAR20 ) ;

   EXTERNAL FORTRAN 'PAS2FTN' ;



function PAS_TO_PAS_FUNC ( X1 : INTEGER ; X2 : INTEGER ) : INTEGER ;

   EXTERNAL 'PAS2PF' ;



function PAS_TO_ASM_FUNC ( X1 : INTEGER ; X2 : INTEGER ) : INTEGER ;

   EXTERNAL ASSEMBLER 'PAS2AF' ;



function PAS_TO_FTN_FUNC ( X1 : INTEGER ; X2 : INTEGER ) : INTEGER ;

   EXTERNAL FORTRAN 'PAS2FF' ;



begin (* HAUPTPROGRAMM *)
  WINDOWS := FALSE ;
  if ORD ( '0' ) <> 0xf0 then
    WINDOWS := TRUE ;
  WRITELN ( 'Test external calls from Pascal' ) ;

  //******************************************************************
  // set variables and test call of PASCAL proc                       
  //******************************************************************

  I := 42 ;
  N := 42 ;
  C1 := 'Test string 1' ;
  C2 := 'Test string 2' ;
  WRITELN ( 'Values before PAS2PAS call:' ) ;
  WRITELN ( 'i   = ' , I ) ;
  WRITELN ( 'n   = ' , N ) ;
  WRITELN ( 'c1  = <' , C1 , '>' ) ;
  WRITELN ( 'c2  = <' , C2 , '>' ) ;
  PASCAL_TO_PASCAL ( I , N , C1 , C2 ) ;
  WRITELN ( 'Values after PAS2PAS call:' ) ;
  WRITELN ( 'i   = ' , I ) ;
  WRITELN ( 'n   = ' , N ) ;
  WRITELN ( 'c1  = <' , C1 , '>' ) ;
  WRITELN ( 'c2  = <' , C2 , '>' ) ;

  //******************************************************************
  // set variables and test call of ASSEMBLER proc                    
  //******************************************************************

  if TRUE then
    begin
      I := 42 ;
      N := 42 ;
      C1 := 'Test string 1' ;
      C2 := 'Test string 2' ;
      WRITELN ( 'Values before PAS2ASM call:' ) ;
      WRITELN ( 'i   = ' , I ) ;
      WRITELN ( 'n   = ' , N ) ;
      WRITELN ( 'c1  = <' , C1 , '>' ) ;
      WRITELN ( 'c2  = <' , C2 , '>' ) ;
      PASCAL_TO_ASSEMBLER ( I , N , C1 , C2 ) ;
      WRITELN ( 'Values after PAS2ASM call:' ) ;
      WRITELN ( 'i   = ' , I ) ;
      WRITELN ( 'n   = ' , N ) ;
      WRITELN ( 'c1  = <' , C1 , '>' ) ;
      WRITELN ( 'c2  = <' , C2 , '>' ) ;
    end (* then *) ;

  //******************************************************************
  // set variables and test call of FORTRAN subroutine                
  //******************************************************************

  if not WINDOWS then
    begin
      I := 42 ;
      N := 42 ;
      C1 := 'Test string 1' ;
      C2 := 'Test string 2' ;
      WRITELN ( 'Values before PAS2FTN call:' ) ;
      WRITELN ( 'i   = ' , I ) ;
      WRITELN ( 'n   = ' , N ) ;
      WRITELN ( 'c1  = <' , C1 , '>' ) ;
      WRITELN ( 'c2  = <' , C2 , '>' ) ;
      PASCAL_TO_FORTRAN ( I , N , C1 , C2 ) ;
      WRITELN ( 'Values after PAS2FTN call:' ) ;
      WRITELN ( 'i   = ' , I ) ;
      WRITELN ( 'n   = ' , N ) ;
      WRITELN ( 'c1  = <' , C1 , '>' ) ;
      WRITELN ( 'c2  = <' , C2 , '>' ) ;
    end (* then *) ;

  //******************************************************************
  // test call of external functions                                  
  //******************************************************************

  XRES := PAS_TO_PAS_FUNC ( 20 , 30 ) ;
  WRITELN ( 'PAS_TO_PAS_FUNC (20, 30) RETURNS ' , XRES ) ;
  if WINDOWS then
    return ;
  XRES := PAS_TO_ASM_FUNC ( 20 , 30 ) ;
  WRITELN ( 'PAS_TO_ASM_FUNC (20, 30) RETURNS ' , XRES ) ;
  XRES := PAS_TO_FTN_FUNC ( 20 , 30 ) ;
  WRITELN ( 'PAS_TO_FTN_FUNC (20, 30) RETURNS ' , XRES ) ;
end (* HAUPTPROGRAMM *) .
