PAS2FTN  TITLE 'ASSEMBLER SUBROUTINE CALLABLE FROM PASCAL'
*
**************************************************************
*        Pascal prototype =
*
*        procedure PASCAL_TO_FORTRAN ( X1 : INTEGER ;
*                                      var X2 : INTEGER ;
*                                      T1 : CHAR20 ;
*                                      var T2 : CHAR20 ) ;
*
*           EXTERNAL FORTRAN 'PAS2FTN' ;
*
*        because the subroutine is specified as
*        EXTERNAL FORTRAN, all parameters are passed
*        by reference, and the leftmost bit is set
*        on the last parameter address
**************************************************************
*
PAS2FTN  CSECT
         STM   R14,R12,12(R13)
         LR    R11,R15             LOAD BASE REGISTER
         USING PAS2FTN,R11
         LA    R15,SAVEAREA
         ST    R15,8(R13)
         ST    R13,4(R15)
         LR    R13,R15
*
**************************************************************
*        fetch parameter addresses
**************************************************************
*
         LM    R2,R5,0(R1)
*
**************************************************************
*        work on parameters
**************************************************************
*
         LA    R6,8
         ST    R6,0(R2)            will change dummy arg only
         LA    R6,8
         ST    R6,0(R3)            set X2 to 7
*
         LTR   R5,R5
         BP    R5POS
         MVC   0(20,R5),=CL20'Last Addr. negative'
         B     OK
R5POS    DS    0H
         MVC   0(20,R5),=CL20'Last Addr. positive'
OK       DS    0H
*
**************************************************************
*        exit (return to caller)
**************************************************************
*
EXIT     DS    0H
         L     R13,4(R13)
         LM    R14,R12,12(R13)
         XR    R15,R15
         BR    R14
         EJECT
*
**************************************************************
*        definitions
**************************************************************
*
         DS    0D
SAVEAREA DS    18F
*
**************************************************************
*        REGISTER ASSIGNMENTS
**************************************************************
*
         REGEQU
*
         END
