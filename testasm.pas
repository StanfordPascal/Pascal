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



procedure PASCAL_TO_ASSEMBLER ( X1 : INTEGER ; var X2 : INTEGER ; T1 :
                              CHAR20 ; var T2 : CHAR20 ) ;

   EXTERNAL ASSEMBLER 'PAS2ASM' ;



procedure PASCAL_TO_FORTRAN ( X1 : INTEGER ; var X2 : INTEGER ; T1 :
                            CHAR20 ; var T2 : CHAR20 ) ;

   EXTERNAL FORTRAN 'PAS2FTN' ;



procedure PASCAL_TO_PASCAL ( X1 : INTEGER ; var X2 : INTEGER ; T1 :
                           CHAR20 ; var T2 : CHAR20 ) ;

   EXTERNAL 'PAS2PAS' ;



begin (* HAUPTPROGRAMM *)
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

  //******************************************************************
  // set variables and test call of FORTRAN subroutine
  //******************************************************************

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
end (* HAUPTPROGRAMM *) .
