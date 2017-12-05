PAS2ASM  TITLE 'ASSEMBLER SUBROUTINE CALLABLE FROM PASCAL'
*
**************************************************************
*        Pascal prototype =
*
*        procedure PAS2ASM ( X1 : INTEGER ;
*                            var X2 : INTEGER ;
*                            T1 : CHAR20 ;
*                            var T2 : CHAR20 ) ;
*
*           EXTERNAL ASSEMBLER 'PAS2FTN' ;
**************************************************************
*
PAS2ASM  CSECT
         STM   R14,R12,12(R13)
         LR    R11,R15             LOAD BASE REGISTER
         USING PAS2ASM,R11
         LA    R15,SAVEAREA
         ST    R15,8(R13)
         ST    R13,4(R15)
         LR    R13,R15
*
**************************************************************
*        fetch parameters
**************************************************************
*
         L     R2,0(R1)            = X1 (by value)
         L     R3,4(R1)            = ADDR of X2 (by addr)
         LA    R4,8(R1)            = ADDR of T1 (by value)
         L     R5,28(R1)           = ADDR of T2 (by addr)
*
**************************************************************
*        work on parameters
**************************************************************
*
         LA    R6,7
         ST    R6,0(R3)            set X2 to 7
*
         MVC   0(20,R5),=CL20'String from PAS2ASM'   set T2
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
