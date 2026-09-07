XRUNPARM TITLE 'RUN PROGRAM WITH PARM FIELD'
*
**************************************************************
*        Source from COBOL disk
*        original RUNPARM - may be changed
**************************************************************
*
XRUNPARM START 0
         SAVE  (14,12),,XRUNPARM    SAVE REGISTERS
         LR    R12,R15              LOAD BASE REGISTER
         USING XRUNPARM,R12
         USING NUCON,0
         ST    R13,REGSAVE+4        CHAIN SAVE AREAS
         LA    R15,REGSAVE
         ST    R15,8(,R13)
         LR    R13,R15              POINT TO NEW SAVE AREA
*
**************************************************************
*        check parameters
**************************************************************
*
         LR    R4,R1                POINT TO COMMAND PLIST
         USING PLIST,R4
         CLI   PLSTPROG,X'FF'       IS THERE A PROGRAM NAME ?
         BE    NOPROGNM             NO
         CLC   PLSTOPTN,CCOMPOFF    WAS COMPOFF OPTION PRESENT ?
         BE    BUMPLIST             YES - IGNORE DEFAULT
         CLC   PLSTOPTN,CCOMPON     WAS COMPON OPTION PRESENT ?
         BNE   MOVEPROG             NO - FORGET OPTION
         OI    FLAGS,COMPON
*
**************************************************************
*        program name etc.
**************************************************************
*
BUMPLIST DS    0H
         LA    R4,PLSTOPTN          LOOK PAST THE OPTION
MOVEPROG DS    0H
         MVC   PROGNAME,PLSTPROG    OBTAIN PROGRAM NAME
         MVC   MODNAME,PROGNAME     INSERT PROGRAM NAME IN FSCB
         FSSTATE FSCB=MODFSCB,ERROR=NOMODULE ENSURE PROGRAM EXISTS
         USING FSTD,R1
         MVC   MODMODE,FSTFMODE     SET FILE MODE
         DROP  R1
         FSREAD FSCB=MODFSCB,ERROR=BADMOD    READ MODULE DESCRIPTOR
         FSCLOSE FSCB=MODFSCB       CLOSE MODULE FILE
         LA    R5,MODESCR           POINT TO MODULE DESCRIPTOR
         USING STRTADDR,R5
         LA    R0,ENDRUN            POINT TO END OF INTERFACE
         C     R0,FRSTLOC           DOES MODULE OVERLAY INTERFACE ?
         BH    BADORG               YES
         L     R2,LASTLOC           OBTAIN LAST LOCATION ADDRESS
         S     R2,FREELOWE          WILL MODULE FIT IN STORAGE ?
         BP    BADSIZE              YES
*
**************************************************************
*        check parameters
**************************************************************
*
         LA    R2,PARMFLD           POINT TO PARM FIELD
         LA    R3,PLSTPARM          POINT TO FIRST PARM TOKEN
         SR    R4,R4                INITIALIZE PARENTHESIS COUNT
         CLI   PLSTPARM,C'('        IS THERE A FORMAL DELIMITER ?
         BNE   NEXTOKEN             NO
         LA    R4,1                 ADJUST PARENTHESIS COUNT
SKPTOKEN DS    0H
         LA    R3,8(,R3)            POINT TO NEXT TOKEN
NEXTOKEN DS    0H
         CLI   0(R3),X'FF'          IS THERE ANOTHER PARAMETER ?
         BE    ENDTOKEN             NO
         CLI   0(R3),C'('           IS THIS A LEFT PARENTHESIS ?
         BE    LEFTPARN             YES
         CLI   0(R3),C')'           IS THIS A RIGHT PARENTHESIS ?
         BE    RGHTPARN             YES
MOVTOKEN DS    0H
         MVC   0(8,R2),0(R3)        INSERT TOKEN IN PARM FIELD
         LA    R2,8(,R2)            POINT PAST TOKEN
SCNTOKEN DS    0H
         BCTR  R2,0                 POINT BACK ONE TOKEN CHARACTER
         CLI   0(R2),C' '           IS THIS A TRAILING BLANK ?
         BE    SCNTOKEN             YES
         LA    R2,1(,R2)            POINT PAST END OF TOKEN
         MVI   0(R2),C' '           OPP: insert blank after token
         LA    R2,1(,R2)            OPP: to seperate tokens / 10.2016
         B     SKPTOKEN
LEFTPARN DS    0H
         LA    R4,1(,R4)            ADJUST LEFT PARENTHESIS COUNT
         B     MOVTOKEN
RGHTPARN DS    0H
         LTR   R4,R4                IS THERE A PARENTHESIS ?
         BZ    MOVTOKEN             NO
         BCT   R4,MOVTOKEN          BRANCH IF UNPAIRED PARENTHESIS
         CLI   PLSTPARM,C'('        WAS FIRST TOKEN A PARENTHESIS ?
         BNE   MOVTOKEN             NO
*
**************************************************************
*        end of parameter list
*        get storage for program and load it
**************************************************************
*
ENDTOKEN DS    0H
         LA    R1,PARMFLD           COMPUTE PARM FIELD LENGTH
         SR    R2,R1
         STH   R2,PARMLGTH          SAVE PARM FIELD LENGTH
         L     R0,LASTLOC           COMPUTE STORAGE TO RESERVE
         S     R0,=A(ENDRUN)
         GETMAIN R,LV=(0)           OBTAIN STORAGE FOR MODULE
         STM   R0,R1,MODSIZE        SAVE MODULE AREA SIZE AND ADDRESS
         COMPSWT ON                 TURN ON COMPILER SWITCH
         LOAD  EPLOC=PROGNAME       LOAD PROGRAM
         TM    FLAGS,COMPON         WAS COMPON SPECIFIED ?
         BO    RUNPROG              YES - LEAVE COMPSWT ON
         COMPSWT OFF                NO - TURN IT OFF
*
**************************************************************
*        call program
**************************************************************
*
RUNPROG  DS    0H
         LR    R15,R0
         LA    R1,PARMLIST          POINT TO PARM LIST
         BALR  R14,R15
         LR    R2,R15               SAVE RETURN CODE
         TM    OSSFLAGS,COMPSWT     WAS COMPSWT SET OFF ?
         BO    FREESIZE             YES - DON'T BOTHER TWICE
         COMPSWT OFF                NO - TURN IT OFF
*
**************************************************************
*        free storage
**************************************************************
*
FREESIZE DS    0H
         LM    R0,R1,MODSIZE        OBTAIN MODULE SIZE AND ADDRESS
         FREEMAIN R,LV=(0),A=(1)    RELEASE MODULE AREA
         LR    R15,R2               RESTORE RETURN CODE
         B     EXIT
*
**************************************************************
*        error messages
**************************************************************
*
NOPROGNM DS    0H
         WRTERM 'NO PROGRAM NAME SPECIFIED'
         LA    R15,24               SET RETURN CODE
         B     EXIT
NOMODULE DS    0H
         LINEDIT TEXT='FILE "...................." NOT FOUND',         *
               DOT=NO,RENT=NO,SUB=(CHAR8A,MODNAME)
         LA    R15,28               SET RETURN CODE
         B     EXIT
BADMOD   DS    0H
         LINEDIT TEXT='FILE "...................." HAS INVALID FORMAT',*
               DOT=NO,RENT=NO,SUB=(CHAR8A,MODNAME)
         LA    R15,32               SET RETURN CODE
         B     EXIT
BADORG   DS    0H
         LINEDIT TEXT='"...................." HAS ILLEGAL ORIGIN',     *
               DOT=NO,RENT=NO,SUB=(CHAR8A,MODNAME)
         LA    R15,32               SET RETURN CODE
         B     EXIT
BADSIZE  DS    0H
         LA    R2,1024-1(,R2) CONVERT REQUIRED SIZE TO 'K'
         SRL   R2,10
         LINEDIT TEXT='"...................." REQUIRES ....K MORE STORA*
               GE',DOT=NO,RENT=NO,SUB=(CHAR8A,MODNAME,DEC,(R2))
         LA    R15,104              SET RETURN CODE
*
**************************************************************
*        exit (return to op sys)
**************************************************************
*
EXIT     DS    0H
         L     R13,4(,R13)    POINT TO CALLER'S SAVE AREA
         RETURN (14,12),RC=(15)     RETURN TO CMS
         EJECT
*
**************************************************************
*        definitions
**************************************************************
*
REGSAVE  DC    18F'0'               REGISTER SAVE AREA
         SPACE
MODSIZE  DS    2F                   MODULE AREA SIZE AND ADDRESS
         SPACE
PROGNAME DS    CL8                  PROGRAM NAME AREA
         SPACE
PARMLIST DC    AL1(128),AL3(PARMLGTH)
         SPACE
         DS    H                    ALIGNMENT
PARMLGTH DS    H                    PARM FIELD LENGTH
PARMFLD  DC    CL256' '             PARM FIELD DATA
         SPACE
CCOMPON  DC    CL8'COMPON'          COMPON OPTION
CCOMPOFF DC    CL8'COMPOFF'         COMPOFF OPTION
         SPACE
FLAGS    DC    X'00'
COMPON   EQU   X'80'
         SPACE
MODFSCB  FSCB  'XXXXXXXX MODULE *',RECFM=V,BSIZE=80,BUFFER=MODESCR
         ORG   MODFSCB+8
MODNAME  DS    CL8                  MODULE NAME AREA
         DS    CL8
MODMODE  DS    CL2                  MODULE FILE TYPE AREA
         ORG
         SPACE
MODESCR  DS    CL80                 MODULE DESCRIPTOR RECORD
         SPACE
         LTORG
         SPACE
ENDRUN   DS    0D
         EJECT
PLIST    DSECT                      COMMAND PLIST
         SPACE
PLSTCMND DS    CL8                  COMMAND NAME
PLSTPROG DS    CL8                  PROGRAM NAME
PLSTOPTN EQU   PLSTPROG             COMPON/OFF OPTION
PLSTPARM DS    0CL8                 PROGRAM PARAMETERS
         EJECT
         PRINT GEN
         NUCON
         FSTD
*
**************************************************************
*        REGISTER ASSIGNMENTS
**************************************************************
*
         REGEQU
*
         END
