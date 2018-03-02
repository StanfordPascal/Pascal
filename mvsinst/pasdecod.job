//PASCALN2 JOB (PASCAL),'PASCAL ASMD',CLASS=A,MSGCLASS=X,
//             TIME=1440,REGION=0M,MSGLEVEL=(1,1),
//             USER=PASCALN,PASSWORD=PAS
//*
//********************************************************************
//*
//* assemble pascal object files decoding program
//*
//********************************************************************
//*
//PASDECOD EXEC ASMFCL,PARM.ASM=(OBJ,DECK),
//         MAC1='SYS2.MACLIB',
//         MAC2='SYS1.AMACLIB',
//         MAC3='SYS1.AMODGEN'
//ASM.SYSIN DD *
*
PASDECOD CSECT
*
************************************************************
*
*        PROGRAM FOR DECODING OBJECT FILES
*        FOR THE PASCAL INSTALLATION UTILITY SPLITMVS
*        (ACCEPTS THE SAME FORMAT AS SPLITMVS)
*
*        BERND OPPOLZER - 02.03.2018
*
************************************************************
*
         STM   R14,R12,12(R13)
         LR    R11,R15
         USING PASDECOD,R11
         LA    R15,SAVEAREA
         ST    R15,8(R13)
         ST    R13,4(R15)
         LR    R13,R15
         MVI   OPENFLAG,0
*
**************************************************
*        OPEN INPUT FILE
**************************************************
*
         OPEN  (INPUT,INPUT)
         OPEN  (SYSPRINT,OUTPUT)
*
**************************************************
*        LOOP TO READ INPUT RECORDS
**************************************************
*
LOOP     DS    0H
         GET   INPUT,INBUF
         CLC   INBUF(6),=CL6'++FILE'
         BNE   DATA
*
**************************************************
*        IF FILE ALREADY OPEN, CLOSE OLD FILE
*        ADD 1 TO DDNAME OF OUTPUT FILE (OUTxxxxx)
*        OPEN NEXT OUTPUT FILE
**************************************************
*
FILE     DS    0H
         CLI   OPENFLAG,1
         BNE   NFILE
         UNPK  L02COUNT,NLINES
         OI    L02COUNT+7,X'F0'
         MVC   L02NAME,OUTFNAME
         MVC   PRINTL,PRINTL02
         PUT   SYSPRINT,PRINTL
         CLOSE (OUTPUT)
NFILE    DS    0H
         AP    OUTFNR,=P'1'
         UNPK  OUTFNRX,OUTFNR
         OI    OUTFNRX+4,X'F0'
         MVC   OUTPUT+40(8),OUTFNAME
         OPEN  (OUTPUT,OUTPUT)
         MVC   L01NAME,OUTFNAME
         MVC   PRINTL,PRINTL01
         PUT   SYSPRINT,PRINTL
         ZAP   NLINES,=P'0'
         MVI   OPENFLAG,1
         B     LOOP
*
**************************************************
*        CHECK FOR DATA TAG
**************************************************
*
DATA     DS    0H
         CLC   INBUF(8),=CL8'++DATA1 '
         BNE   DATAN1
*
**************************************************
*        IF DATA1, CHECK FOR OPEN FLAG
*        AND PROCESS FIRST PART OF DATA RECORD
**************************************************
*
DATA1    DS    0H
         CLI   OPENFLAG,1
         BNE   NOTOPEN
         LA    R3,INBUF+18
         LA    R4,OUTBUF
         LA    R5,30
LOOP1    DS    0H
         BAL   R6,CONV
         LA    R3,2(R3)
         LA    R4,1(R4)
         BCT   R5,LOOP1
         B     LOOP
*
DATAN1   DS    0H
         CLC   INBUF(8),=CL8'++DATA2 '
         BNE   DATAN2
*
**************************************************
*        IF DATA2, CHECK FOR OPEN FLAG
*        AND PROCESS SECOND PART OF DATA RECORD
**************************************************
*
DATA2    DS    0H
         CLI   OPENFLAG,1
         BNE   NOTOPEN
         LA    R3,INBUF+18
         LA    R4,OUTBUF+30
         LA    R5,30
LOOP2    DS    0H
         BAL   R6,CONV
         LA    R3,2(R3)
         LA    R4,1(R4)
         BCT   R5,LOOP2
         B     LOOP
*
DATAN2   DS    0H
         CLC   INBUF(8),=CL8'++DATA3 '
         BNE   UNKTAG
*
**************************************************
*        IF DATA3, CHECK FOR OPEN FLAG
*        AND PROCESS THIRD PART OF DATA RECORD
*        THEN WRITE OUTPUT RECORD
**************************************************
*
DATA3    DS    0H
         CLI   OPENFLAG,1
         BNE   NOTOPEN
         LA    R3,INBUF+18
         LA    R4,OUTBUF+60
         LA    R5,20
LOOP3    DS    0H
         BAL   R6,CONV
         LA    R3,2(R3)
         LA    R4,1(R4)
         BCT   R5,LOOP3
         PUT   OUTPUT,OUTBUF
         AP    NLINES,=P'1'
         B     LOOP
*
UNKTAG   DS    0H
         ABEND 3001
*
NOTOPEN  DS    0H
         ABEND 3002
*
EOF      DS    0H
         CLI   OPENFLAG,1
         BNE   RETURN
         UNPK  L02COUNT,NLINES
         OI    L02COUNT+7,X'F0'
         MVC   L02NAME,OUTFNAME
         MVC   PRINTL,PRINTL02
         PUT   SYSPRINT,PRINTL
         CLOSE (OUTPUT)
*
RETURN   DS    0H
         CLOSE (INPUT)
         CLOSE (SYSPRINT)
         L     R13,4(R13)
         LM    R14,R12,12(R13)
         XR    R15,R15
         BR    R14
*
*
************************************************************
*        CONVERSION ROUTINE
************************************************************
*
CONV     DS    0H
         MVC   UNPK3(2),0(R3)
         TR    UNPK3(2),HEXTAB
         PACK  PACK2,UNPK3
         MVC   0(1,R4),PACK2
         BR    R6
*
*
*
************************************************************
*        D A T A   D E F I N I T I O N S
************************************************************
*
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
*
SAVEAREA DS    18F
*
INPUT    DCB   DDNAME=INPUT,DSORG=PS,MACRF=GM,RECFM=FB,LRECL=80,       *
               EODAD=EOF
OUTPUT   DCB   DDNAME=OUT00000,DSORG=PS,MACRF=PM,RECFM=FB,LRECL=80
SYSPRINT DCB   DDNAME=SYSPRINT,DSORG=PS,MACRF=PM,RECFM=FBA,LRECL=133
*
INBUF    DS    CL80
OUTBUF   DS    CL80
PRINTL   DS    CL133
*
UNPK3    DS    CL3
PACK2    DS    PL2
OUTFNR   DC    PL3'0'
OUTFNAME DS    0CL8
         DC    C'OUT'
OUTFNRX  DS    CL5
OPENFLAG DS    C
NLINES   DS    PL5
*
PRINTL01 DC    CL133' '
         ORG   PRINTL01+1
         DC    C'FILE '
L01NAME  DS    CL8
         DC    C' OPENED FOR OUTPUT'
         ORG
*
PRINTL02 DC    CL133' '
         ORG   PRINTL02+1
L02COUNT DS    CL8
         DC    C' LINES WRITTEN TO FILE '
L02NAME  DS    CL8
         ORG
*
HEXTAB   DC    256X'00'
         ORG   HEXTAB+C'A'
         DC    X'0A0B0C0D0E0F'
         ORG   HEXTAB+C'0'
         DC    X'0001020304'
         ORG   HEXTAB+C'5'
         DC    X'0506070809'
         ORG
         END   PASDECOD
*
//*
//LKED.SYSLMOD DD DISP=SHR,DSN=PASCALN.COMPILER.LOAD(PASDECOD)
//*
