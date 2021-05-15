         TITLE 'PASCSP, PASCAL RUNTIME SUPPORT AND STANDARD PROCS'
***********************************************************************
*
*     PASCAL ENVIRONMENT AND ENTRY SETUP
*     ------------------------------------
*
*     see below: updated Program History / Bernd Oppolzer
*
*     This runtime targets MVS and CMS (tested with the free versions
*     distributed with Hercules) - will probably run on modern versions
*     of z/OS and z/VM ... not sure for z/VM ...
*
*     Only AMODE 24 at the moment ... sorry !
*
*     CMS Version must be compiled using SYSPARM(CMS)
*
***********************************************************************
*
*     Original comment from 1976:
*
***********************************************************************
*
*     COPYRIGHT 1976, STANFORD LINEAR ACCELERATOR CENTER.
*
*     THE FOLLOWING PROGRAMS CREATE  THE  RUN-TIME  ENVIRONMENT  AND
*     PROVIDE THE I/O INTERFACE FOR THE SLAC 'PASCAL' COMPILER.
*
*     EXCEPT FOR THE FEW POINTS EXPLAINED IN THIS BOX, THE INTERNALS
*     OF THESE ROUTINES SHOULD BE INVISIBLE (AND INCONSEQUENTIAL) TO
*     THE 'PASCAL' USER.
*
*     1) THE USER MAY SPECIFY THE SIZE OF THE RUN  TIME  STACK/HEAP,
*     THE  SIZE OF THE AREA RETURNED TO THE OPERATING SYSTEM FOR I/O
*     BUFFERS, THE MAXIMUM COUNT OF RUN  TIME  ERRORS,  THE  RUNNING
*     TIME  OF  THE  PROGRAM,  REQUEST  AN  OPTIONAL MEMORY DUMP AND
*     SPECIFY OTHER SPECIAL CONTROL OPTIONS AS FOLLOWS:
*
*       // EXEC USERPROG,
*       //      PARM='USER PARMS /STACK=XXXK,IOBUF=YYYK,
*                            TIME=TTTS,NOSPIE,NOSNAP,NOCC,DUMP'
*
*       'USER PARMS': THE PARAMETER LIST TO BE PASSED TO THE USER
*                     PROGRAM (IF ANY).
*       'XXX' : STORAGE AREA (IN K BYTES) FOR STACK+HEAP.
*       'YYY' : STORAGE AREA (IN K BYTES) TO BE RETURNED TO SYSTEM.
*       'TTT' : PROGRAM RUNNING TIME (IN SECONDS).
*       'DUMP': TO GENERATE AN OS STYLE MEMORY DUMP IN CASE OF AN
*               ABNORMAL PROGRAM TERMINATION.
*       'NOSPIE': TO PREVENT INTERCEPTION OF ERROR INTERRUPTS
*       'NOSNAP': TO STOP USE OF SNAPSHOT RT. AFTER AN ERROR
*       'NOCC': TO STOP FIRST CHARACTER ON EACH LINE FROM BEING
*               TAKEN AS A CONTROL CHARACTER
*       DEFAULT VALUE FOR 'XXXK' IS THE JOB 'REGION' SIZE.
*       DEFAULT VALUE FOR 'YYYK' IS 36K.
*
*     2) THE VALUE OF THE RETURN CODE 'RC', IF OTHER  THAN GENERATED
*     BY  THE  USER  PROGRAM,  MAY  BE  INTERPRETED ACCORDING TO THE
*     FOLLOWING TABLE.  FOR MORE DETAILED EXPANATION  OF  THE  ERROR
*     CONDITION,  SEE  THE  CONTENTS OF THE 'OUTPUT' FILE WHICH HAVE
*     THE APPROPRIATE MESSAGES.  NOTE THAT THIS FILE (OUTPUT) SHOULD
*     BE INCLUDED IN THE USER PROGRAM IN ORDER TO GET THE  RUN  TIME
*     DIAGNOSTICS AND RELATED MESSAGES.
*
*       RETURN CODE:  IMPLIES:
*
*       1001          INDEX VALUE OUT OF RANGE
*       1002          SUBRANGE VALUE OUT OF RANGE
*       1003          ACTUAL PARAMETER OUT OF RANGE
*       1004          SET MEMBER OUT OF RANGE
*       1005          POINTER VALUE INVALID
*       1006          STACK/HEAP COLLISION
*       1007          ILLEGAL INPUT/RESET OPERATION
*       1008          ILLEGAL OUTPUT/REWRITE OPERATION
*       1009          SYNCHRONOUS I/O ERROR
*       1010          PROGRAM EXCEEDED SPECIFIED RUNNING TIME
*       1011          ILLEGAL FILE DEFINITION (I.E., TOO MANY FILES)
*       1012          NOT ENOUGH STACK SPACE
*       1013          UNDEFINED OR OBSOLETE CSP CALL
*       1014          LINELIMIT EXCEEDED FOR A FILE
*       1015          BAD FILE CONTROL BLOCK
*       1016          INPUT RECORD TOO LARGE
*       1020          READ PAST END OF FILE
*       1021          BAD BOOLEAN INPUT
*       1022          BAD INTEGER INPUT
*       1023          BAD REAL INPUT
*       1024          OVER-LARGE INTEGER INPUT
*
*       200X          PROGRAM INTERRUPTION CODE 'X'
*
*       3001          MISC. EXTERNAL ERROR CONDITIONS.
*
*       X1XX          UNABLE TO RUN SNAPSHOT, OTHER DIGITS AS ABOVE
*
*
*     3) THE CONDITIONAL ASSEMBLY FLAG &SYSTEM DETERMINES WHETHER
*     CERTAIN SECTIONS OF CODE ARE INCLUDED IN THE PROGRAM.
*     WITH &SYSTEM=1, SOME CHECKING CODE, REAL NUMBER INPUT AND THE
*     FORTRAN INTERFACE IS OMITTED.  THIS RESULTS IN A SMALLER
*     FASTER PROGRAM BUT WHICH CAN ONLY BE USED WITH "SAFE"
*     PROGRAMS THAT DO NOT USE MATHEMATICAL ROUTINES - SUCH AS THE
*     COMPILER AND THE P-ASSEMBLER.
*     WITH &SYSTEM=0, THE FULL PROGRAM IS PRODUCED AND THIS IS THE
*     VERSION THAT SHOULD NORMALLY BE COMBINED WITH USER PROGRAMS.
*
*     4) THIS PROGRAM MAY BE ASSEMBLED WITH MOST STANDARD IBM
*     ASSEMBLERS.
*
*     5) IF THE RUN PROFILE SWITCH IS ENABLED IN THE PASCAL  PROGRAM
*     (I.E.   'K+'),  THE  RUN  TIME SYSTEM WILL 'REWRITE' THE 'RAW'
*     EXECUTION COUNTS ON THE PREDEFINED 'QRR'  FILE  AFTER  RUNNING
*     THE  USER  PROGRAM.  IN SUCH CASES THE USER PROGRAM SHOULD NOT
*     USE THE 'QRR' FILE BUT THE 'DD' STATEMENT FOR THIS FILE SHOULD
*     BE INCLUDED IN THE 'GO' STEP.  THE SUBMONITOR WILL SUBSEQUENTLY
*     INVOKE THE "PASPROF" LOAD-MODULE TO PRINT THE PROFILE.
*
*
*     THESE PROGRAMS INCLUDE SOME CONTRIBUTIONS BY KEITH RICH,  JOHN
*     BANNING AND NIGEL HORSPOOL.
*
*                                SASSAN HAZEGHI,
*
*                                COMPUTATION RESEARCH GROUP
*                                STANFORD LINEAR ACCELERATOR CENTER
*                                P. O. BOX 4349
*                                STANFORD, CALIFORNIA  94305.
*
*
*                                LAST UPDATE:
*                                             MAR.  15, 76.
*                                             SEPT.  8, 76.
*                                             JAN.  20, 77.
*                                             JULY  28, 77.
*                                             MAY   21, 77.
*                                             JULY   6, 78.
*                                             SEPT. 15, 78.
*                                             NOV.  11, 78.
*                                             AUG.  09, 79.
*
**********************************************************************
*
*
*
*
*
*
**********************************************************************
*
*    Program History - newest first
*
***********************************************************************
*
*    Some more modifications in 2020 by Bernd Oppolzer
*    (berndoppolzer@yahoo.com)
*
*    - resolved error in $PASSCMP
*      added support for string on stack representation
*      (comparisons following string function calls were not handled
*      correctly)
*
***********************************************************************
*
*    Some more modifications in 2020 by Bernd Oppolzer
*    (berndoppolzer@yahoo.com)
*
*    - new CSPs (Pascal Standard Procedures):
*      RFC, RFS and RFV - read char, char array and varchar from file
*      these new CSPs replace the old CSPs RDC, RDS and RDV;
*      they have an additional parameter - the optional field width
*      if the field width is negative, they behave as the old CSPs
*      (READ with field width is a feature of IBMs Pascal/VS)
*
*    - CSP GET sets terminal flag to N temporarily
*      GET this way forces reading a new buffer, even on terminal file
*      this is needed for the new READ functions implemented in Pascal
*      for example $PASRDI, which reads integers
*
***********************************************************************
*
*    Some more modifications in 2019 by Bernd Oppolzer
*    (berndoppolzer@yahoo.com)
*
*    - new CSPs (Pascal Standard Procedures) RDV (read varchar)
*      and WRV (write varchar) to support input and output of
*      varying strings.
*
***********************************************************************
*
*    Some more modifications in 2018 by Bernd Oppolzer
*    (berndoppolzer@yahoo.com)
*
*    - added $PASSCMP to support comparison of VARCHAR strings
*      (it was just too complicated to do it inline). The
*      function is called without full save area linking
*      directly from compiler generated code. Special interface.
*
*    - added $PASSVCC to support concatenation of VARCHAR strings
*      (in cases where inline code generation does not work
*      due to number of available register constraints)
*
***********************************************************************
*
*    Some more modifications in 2017 by Bernd Oppolzer
*    (berndoppolzer@yahoo.com)
*
*    - Eliminate &IBM370 switch
*      (MVCL and STCM are required from now on)
*
*    - some functions which deal with Pascal files renamed from
*      FILxxx to PAFxxx (FILxxx only used for offsets of Pascal FCB)
*
*    - some more functions moved out of the critical area and made
*      adressable by themselves (own base), e.g. PAFGOP, PAFPOP,
*      PAFOPN, CHKPRM
*
*    - Compile switch (SYSPARM(CMS)) to control environment; some
*      features are not needed on CMS; see such coding:
*      AIF   ('&SYSPARM' EQ 'CMS').CMS101
*
*    - Member name included in Pascal FCB (see how it will work)
*      Offset FILMEM
*
*    - Modify JFCB and OPEN ... TYPE=J to open specific members of
*      a PDS, controlled by member name in Pascal FCB;
*      member name can be set by new Pascal function ASSIGNMEM,
*      which is part of PASUTILS runtime library. Works on input
*      and output; the function ASSIGNMEM is limited to TEXT files
*      at the moment (no such restriction imposed by the runtime)
*
*    - in absence of a member name on both DD assignments and
*      Pascal FCB, the directory of the PO dataset should be read;
*      but to do that it is first necessary to recognize PO files
*      and not to do all those things on normal PS files - later ...
*
*    - thanks to Gerhard Pospischil for help with those topics
*
*    - open of members is a prerequisite for PASSNAP on MVS !!
*
***********************************************************************
*
*    Some modifications in 2016 / 2017 by Bernd Oppolzer
*    (berndoppolzer@yahoo.com)
*
*    - Table of functions in $PASCSP2 different
*
*    - several CSP functions added and changed, as needed
*
*    - compatibility issues resolved (the compiler has been ported
*      to some other platforms, too)
*
*    - $PASCSP2 can use the Pascal runtime stack for its save areas
*      and so maybe can be called recursively
*      (Pascal calls a $PASCSP function calls
*      a Pascal function calls another $PASCSP function
*      and so on ...)
*
*    - $PASCSP2 covered by two base regs (10 and 11)
*      but some subroutines are moved outside and reuse reg 11
*      for their own base reg. That means that subroutines inside
*      $PASCSP2 that are called from outside subroutines have to
*      be placed at the beginning of $PASCSP2 (later hopefully
*      may be reduced to reg 10 only - if enough subroutines
*      have been moved outside).
*
*    - Subroutine that handles JCL parameters is moved from mainline
*      to the end of the program (CHKPRM) and has its own base reg.
*
*    - PTRACE routine is available everywhere, called by BALR 14,15
*      and only needs register 3 on entry (pointing to the output
*      buffer, 80 characters)
*
*    - a temp save area is established on entry, before the "real"
*      save area is constructed. So PTRACE calls are possible
*      already in early stages (the "real" save area depends on
*      STACK size etc., which needs JCL parms to be analyzed first)
*
*
***********************************************************************
*
*    Some modifications since 2011 by Bernd Oppolzer
*    (berndoppolzer@yahoo.com)
*
*    - in 2011: year-2000 problems fixed
*
*    - in 2016: to allow true console dialog (FILEDEF INPUT TERM,
*      FILEDEF OUTPUT TERM and sequences of WRITELN and READ),
*      some changes are applied to the I/O subroutines:
*
*      a) WLN (writeln) outputs the buffer late, due to locate mode
*         used on output. I changed that in the following way:
*         when using files for OUTPUT only, move mode is used -
*         actually the standard file OUTPUT is the only file that
*         uses output only, so if you want to do TERMINAL output,
*         use file OUTPUT and assign it to the TERMINAL.
*         You will get immediate output to the TERMINAL on
*         WLN (writeln).
*
*         (some time later I changed PASCSP to use move mode on
*         EVERY output file; in fact this is controlled by the
*         setting in macro FILDEF only; the writing routines get
*         the setting from the DCB field DCBMACRF and act accordingly,
*         so it can be switched back to locate mode at any time,
*         if desired)
*
*      b) the compiler generates an implicit RESET call for INPUT
*         on the beginning of the main procedure. This leads to
*         an immediate READ action from the console (before any
*         output appears). I changed that in the following way:
*
*         when reading textfiles, the first RESET is not mandatory,
*         that is, it is done implicitly, when the first READ is
*         encountered and no RESET has been issued until then.
*
*         if there has been a RESET, this is recorded in one of the
*         new flags and no additional RESET is done. if a READ is
*         encountered, this is recorded, too, and then, if another
*         RESET is issued, it is executed.
*
*         that means: all READ calls have to be changed to check for
*         a RESET which has to be done prior to the READ, and to set
*         the "read executed" flag.
*
*         maybe REWRITE could be "automated" in the same way.
*
*         After having changed the runtime in this way, I removed the
*         implicit RESET(INPUT) generated at the beginning of the main
*         procedure by the compiler.
*
*         Now it is possible to build a simple terminal dialog with
*         the CMS user using WRITELN and READ.
*
*      c) to automate REWRITE too, the file status (new field) is
*         used in the following way:
*
*         0 = the file is closed
*         1 = reset has been issued, but no read operation
*         2 = rewrite has been issued, but no write operation
*         3 = read operations have been issued
*         4 = write operations have been issued
*
*         now we have the following transition matrix:
*
*         status  !  reset    rewrite   read     write
*         --------!------------------------------------
*         0       !  1        2         3 a)     4 b)
*         1       !  1 c)     2         3        inv
*         2       !  1        2 c)      inv      4
*         3       !  1        2         3        inv
*         4       !  1        2         inv      4
*
*         inv = invalid, run time error
*         a) = implicit reset
*         b) = implicit rewrite
*         c) = no action
*
**********************************************************************
*
*     Minor mods made by Dave Edwards (DE), Jan/2007 - see below.
*
*     See also:  $psc:pascal.mon.notes
*                $psc:pascal.lib.notes
*
*     *** This module, assembled with &SYSTEM set to 0, also forms
*         part of $psc:pascal.lib (run-time library object).
*
*     28jan2007 - JCL added an top of file, and module reassembled.
*     No change to the source. See $psc:pascal.mon.notes . (DE)
*
*     28jan2007 - Fix year-2000 problem when setting PASDATE (for the
*     Pascal predefined variable DATE e.g. '01-28-2007'): set correct
*     century if actual year is 20yy and this seems to be a MUSIC/SP
*     system. Previously, year 20nn would be reported as 19nn.
*     (But coding is still incorrect for years like 2100, because
*     that year is not a leap year.) (DE)
*
***********************************************************************
*
*     FURTHER MODIFICATIONS MADE AT MCGILL UNIVERSITY,
*
*                                R. NIGEL HORSPOOL
*                                APRIL 7, 1982
*
**********************************************************************
*
*
*
*
*
*
**************************************************************
*
*        I/O (FILE) HANDLING MACROS
*
**************************************************************
*
*
**************************************************************
*        filadr = adressen fuer file-handling-routinen
*        in bestimmte register ablegen
**************************************************************
*
         MACRO ,
&L       FILADR ,
.*       TO COMPUTE FILE BUFFER ADDRESS ETC.
         GBLB  &SYSTEM
&L       L     AE,PFILPTR(AD)      LOAD FILE BLOCK ADDR
         AIF   (&SYSTEM).NOCHK
         C     AD,FILPAS(AE)       CHECK THAT FILE BLOCK POINTS
         BNE   BADFILE             TO PASCAL FILE VARIABLE
.NOCHK   ANOP
         L     AF,FILBUF(AE)       SET I/O BUFFER POINTER
         MEND  ,
*
*
*
*
**************************************************************
*        filadrn = adressen fuer file-handling-routinen
*        in bestimmte register ablegen
*        bei fehler (kein gueltiger file in AD)
*        nil nach AE und AF
**************************************************************
*
         MACRO ,
&L       FILADRN ,
.*
.*       TO COMPUTE FILE BUFFER ADDRESS ETC.
.*
         LCLC  &LBL1
         LCLC  &LBL2
&LBL1    SETC  'FA01&SYSNDX'
&LBL2    SETC  'FA02&SYSNDX'
         GBLB  &SYSTEM
&L       L     AE,PFILPTR(AD)      LOAD FILE BLOCK ADDR
         LA    AE,0(AE)
         AIF   (&SYSTEM).NOCHK
         C     AD,FILPAS(AE)       CHECK THAT FILE BLOCK POINTS
         BNE   &LBL1               TO PASCAL FILE VARIABLE
.NOCHK   ANOP
         L     AF,FILBUF(AE)       SET I/O BUFFER POINTER
         LA    AF,0(AF)
         B     &LBL2
&LBL1    DS    0H
         XR    AE,AE               return nil on err
         BCTR  AE,0
         LR    AF,AE               in both regs
&LBL2    DS    0H
         MEND  ,
*
*
*
*
**************************************************************
*        fildef = definitionen fuer standard-files
*        aenderungen opp 2016:
*        - move mode fuer output
*        - dcb verschoben nach offset 36 / spaeter 40
*        - dadurch platz fuer neue flags
*        File Status:
*        0 = file closed
*        1 = file open, no reset/rewrite so far
*        2 = reset/rewrite issued, no get/read so far
*        3 = read/write action performed
*        the idea: when there is a read/write action without
*        prior reset/rewrite, reset and rewrite will be performed
*        automatically
*        weitere aenderungen opp 2016:
*        - eof und eoln flag hierher, weg von pascal-fcb
*        - compiler entsprechend geaendert
*        - flag wg. input/output/inout
*        - eof initial auf false fuer input- und inout-files
**************************************************************
*
         MACRO ,
         FILDEF &NAME,&DIRECT,&KIND,&LINK
.*       DEFINE A FILE
         LCLC  &NAM,&OPT1,&OPT2
         LCLC  &DIR
         DS    0D
&NAM     SETC  '&NAME'(1,3)
FIL&NAM  DC    CL8'&NAME'          PASCAL FILE IDENTIFIER
         DC    A(&LINK)            PTR TO NEXT FILE BLOCK
         DC    A(0)                PTR TO PASCAL FILE VRBL.
         DC    A(0)                I/O BUFFER ADDRESS
         DC    F'0'                LINE-LIMIT FOR FILE (ON OUTPUT)
         DC    H'0'                CURRENT RECORD LENGTH (TEXTFILE)
.*
         AIF   ('&DIRECT' EQ 'OUTPUT').DIRO
         AIF   ('&DIRECT' EQ 'INPUT').DIRI
&DIR     SETC  'U'                 DIRECT = INOUT
         AGO   .DIRX
.DIRI    ANOP
&DIR     SETC  'I'                 DIRECT = INPUT
         AGO   .DIRX
.DIRO    ANOP
&DIR     SETC  'O'                 DIRECT = OUTPUT
.DIRX    ANOP
         AIF   ('&KIND' NE 'TEXT').FD2
&OPT1    SETC  'PM'                LOCATE-MODE OUTPUT NEEDED
         AIF   ('&DIR' EQ 'O').FD1
&OPT1    SETC  'GL'                LOCATE-MODE INPUT NEEDED
         AIF   ('&DIR' EQ 'I').FD1
&OPT2    SETC  'PM'                BOTH LOCATE MODE INPUT & OUTPUT
.FD1     DC    AL1(TEXTFLAG,0)     OPEN/TEXT FLAGS, EOF FLAG
         DC    H'0',H'0'           CHAR PTR, CHAR START POS
         AGO   .FD3
.FD2     DC    AL1(0,0)            OPEN/TEXT FLAGS, EOF FLAG
         DC    H'0',H'0'           MAX REC SIZE, FILE COMP. SIZE
&OPT1    SETC  'GM'                MOVE-MODE INPUT AND
&OPT2    SETC  'PM'                MOVE-MODE OUTPUT NEEDED
.FD3     ANOP  ,
         DC    C'0'                File Status (Textfiles only)
         DC    X'00'               EOF-Flag
         DC    X'00'               EOLN-Flag (Textfiles only)
         DC    C'&DIR'             I=INPUT,O=OUTPUT,U=INOUT
*                                  DCB bei offset 40
         DC    C' '                Terminal Flag
         DC    X'00'               READLN scheduled
         DC    X'00'               unused2
         DC    X'00'               unused3
         DC    CL8' '              Member Name for PDS I/O
         DCB   DSORG=PS,DDNAME=&NAME,EODAD=EOD,SYNAD=SYNADRT,          X
               EXLST=XL&DIRECT,BFTEK=A,MACRF=(&OPT1,&OPT2)
         DS    CL(LENDCB)
*
         MEND  ,
*
*
*
*
         GBLB  &SYSTEM
*
&SYSTEM  SETB  0                   TRUE INDICATES A COMPACT 'CSP'
*
         AIF   (&SYSTEM).SYS1
*
***************************************************************
*        GENERAL SETUP FOR USER PROGRAM(S).
***************************************************************
*
         AGO   .USE1
.SYS1    ANOP
*
***************************************************************
*        COMPACT SETUP, OMITS FORTRAN INTERFACE & TRACING
***************************************************************
*
.USE1    ANOP
*
*
*
*
***************************************************************
*
*        STACK (AND SAVE AREA) LAYOUT
*
***************************************************************
*
*        PRINT NOGEN               ich will den dcb sehen
         DCBD  DSORG=PS
ENDDCBD  DS    0F
LENDCB   EQU   ENDDCBD-IHADCB
         PRINT GEN
*
*
*
*
DYNSTORE DSECT
*
         DS    20F                 PASCAL ENVIRONMENT SAVE AREA
STACK    DS    18F          0000   BOTTOM OF RUNTIME STACK
CLOCK    EQU   STACK               CLOCK LOCATION
NEWPTR   DS    A            0072   PASCAL 'NEW' POINTER
HEAPLIM  DS    A            0076   UPPER LIMIT OF HEAP ( +1 )
*                                  ALSO POINTS TO DYN2STOR
DISPREGS DS    10F          0080   RUN TIME DISPLAY REGISTERS
DISPLAY  EQU   DISPREGS,*-DISPREGS
*
FL1OLD   DS    D            0120   R/W  FIX/FLOAT CONVERSION HELPS
FL2OLD   DS    D            0128   R ONLY
FL3OLD   DS    D            0136   R/W
FL4OLD   DS    D            0144   R ONLY
*
CHKSUBS  DS    0F                  ENTRY TO RUN TIME CHECK ROUTINES
INXCHK   DS    3F           0152   INDEX CHECK
RNGCHK   DS    3F           0164   SUBRANGE CHECK
PRMCHK   DS    3F           0176   PARAMETER VALUE CHECK
PTRCHK   DS    3F           0188   POINTER CHECK
PTACHK   DS    3F           0200   SET MEMBER CHECK
SETCHK   DS    3F           0212
STKCHK   DS    3F           0224
TRACER   DS    3F           0236
INPUT    DS    3F           0248
OUTPUT   DS    3F           0260
PRD      DS    3F           0272
PRR      DS    3F           0284
QRD      DS    3F           0296
QRR      DS    3F           0308
CLEARBUF DS    XL8          0320   BUFFER TO CLEAR ACTIVATION RECORDS
PASDATE  DS    CL10         0328   PREDEFINED VARIABLE DATE
PASTIME  DS    CL10         0338   PREDEFINED VARIABLE TIME
OSPRMPTR DS    A            0348   POINTER TO O.S. PARM STRING
*
FL1      DS    D            0352   R/W  FIX/FLOAT CONVERSION HELPS
FL2      DS    D            0360   R ONLY
FL3      DS    D            0368   R/W
FL4      DS    D            0376   R ONLY
*
STRFIRST DS    A            0384   PTR TO BEGIN OF STR WORKAREA
STRCURR  DS    A            0388   ACTUAL PTR TO STR WORKAREA
STRSIZE  DS    F            0392   STR WORKAREA SIZE
*
FRSTGVAR DS    0D           0400   FOR ALIGNMENT PURPOSES
*
*
*
*
***************************************************************
*       DYNAMIC STORAGE AREA POINTED TO BY HEAPLIM
***************************************************************
*
DYN2STOR DSECT
DYNRUNC  DS    F                   # OF RUN TIME FREQUENCY COUNTERS
         DS    0D
DYNCOUNT DS    0F
         AIF   (&SYSTEM).SYS3
DYN2LEN  EQU   128                 EXTRA MARGIN FOR
*                                  PATHOLOGICAL CALL PARMS
         AGO   .USE3
.SYS3    ANOP
DYN2LEN  EQU   *-DYN2STOR
.USE3    ANOP
*
*
*
*
**************************************************************
*
*        PASCAL ENTRY POINT AND PROGRAM PROLOGUE
*
**************************************************************
*
$PASENT  CSECT ,
*
         ENTRY $PASENT,$PASINT,$PASTRC
         ENTRY $PASSYS,$PASCSP,$PASCSP2
         ENTRY $PASSCMP,$PASSVCC
*
         USING $PASENT,R15
         STM   R14,R12,12(R13)
         LR    R10,R15
         DROP  R15
         USING $PASENT,R10
         LA    R15,SAVETEMP
         ST    R15,8(R13)
         ST    R13,4(R15)
         LR    R13,R15
*
         ST    R1,OSPARMS          SAVE ADDRESS OF O.S. PARMS
*
********************************************************************
*        OPEN TRACE-DD FOR TRACES OF PASCAL MONITOR
********************************************************************
*
         XR    R3,R3
         L     R15,=A(PTRACE)
         BALR  R14,R15
*
********************************************************************
*        call subroutine to check runtime parameters
********************************************************************
*
         L     R15,=A(CHKPRM)
         BALR  R14,R15
*
**********************************************************************
*        GET SPACE FOR THE RUN TIME STACK
**********************************************************************
*
         L     R0,BUFSTORE
         A     R0,REQSTORE         COMPUTE THE SIZE OF THE SMALLEST
         ST    R0,REQSTORE         AREA THAT WILL MEET THE DEMAND
         C     R0,REQSTORE+4
         BL    *+8                 UPPER BOUND OK ?
         ST    R0,REQSTORE+4       ADJUST IT IF NEEDED.
*
**********************************************************************
*        GET ENOUGH SPACE FOR STACK+IOBUF NOW
**********************************************************************
*
         GETMAIN VU,LA=REQSTORE,A=ALOSTORE
*
**********************************************************************
*        GET SPACE FOR STRING WORKAREA
*        AND INITIALIZE GLOBAL STRING WORKAREA POINTERS
**********************************************************************
*
         L     R3,STRWSTOR
         GETMAIN R,LV=(R3)
         ST    R1,STRWTEMP
*
         B     P0100
*
TRGETM1  DC    CL80'TRACE: STACK1 = XXXXXXXXX'
TRGETM2  DC    CL80'TRACE: STACK2 = XXXXXXXXX'
TRGETM3  DC    CL80'TRACE: STRWA  = XXXXXXXXX'
*
P0100    DS    0H
*
**********************************************************************
*        print amount of acquired storage
**********************************************************************
*
*        L     R0,REQSTORE
*        CVD   R0,DOWO1
*        UNPK  TRGETM1+16(9),DOWO1+3(5)
*        OI    TRGETM1+24,X'F0'
*        LA    R3,TRGETM1
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
*        L     R0,REQSTORE+4
*        CVD   R0,DOWO1
*        UNPK  TRGETM2+16(9),DOWO1+3(5)
*        OI    TRGETM2+24,X'F0'
*        LA    R3,TRGETM2
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
*        L     R0,STRWSTOR
*        CVD   R0,DOWO1
*        UNPK  TRGETM3+16(9),DOWO1+3(5)
*        OI    TRGETM3+24,X'F0'
*        LA    R3,TRGETM3
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
**********************************************************************
*        new base
**********************************************************************
*
         BALR  R10,0
         USING *,R10
*
         L     R1,ALOSTORE         GET ADDRESS OF ALLOCATED AREA
         LR    R12,R1
         A     R1,ALOSTORE+4       ADD SIZE OF THE AREA
         S     R1,BUFSTORE         BEGINNING (ENDING !) OF THE HEAP
         S     R1,=A(8)            NAME FIELD OF THE HEAP
         USING DYNSTORE,R12
*
         L     R2,STRWSTOR
         ST    R2,STRSIZE
         L     R2,STRWTEMP
         ST    R2,STRFIRST
         ST    R2,STRCURR
*
         AIF   (&SYSTEM).SYS32
*
         LR    R2,R1
         SR    R2,R12              R2 <-- SIZE OF THE USABLE AREA
         L     R3,=A(FRSTGVAR-STACK)
         CLR   R2,R3
         BNH   NOCLR               SKIP IF NOT LARGE ENOUGH
*
**********************************************************************
*        init area with pattern x'81' using mvcl
*        (ibm370 dependency: MVCL)
**********************************************************************
*
         LR    R2,R12              ADDRESS OF STACK
         LA    R15,X'81'
         SLL   R15,24              SET PADDING CHAR FOR MVCL
         MVCL  R2,R14              CLEAR THE AREA
*
.SYS32   ANOP
*
**********************************************************************
*        replace temp savearea by "real" savearea
**********************************************************************
*
NOCLR    DS    0H
         L     R13,4(R13)          back chain from temp save area
         ST    R13,4(R12)          BACK LINK OF NEW SAVE AREA
         ST    R12,8(R13)          FRWRD LINK OF OLD SAVE AREA
         LR    R13,R12             RESET SAVE AREA POINTER
*
         MVC   STACK-8(8),=CL8'   STACK'
         MVC   0(8,R1),=CL8'HEAP    '
         LA    R12,STACK           GLOBAL (STACK BOTTOM) POINTER
         USING STACK,R12
         ST    R1,NEWPTR           SET PASCAL 'NEW' POINTER
*
**********************************************************************
*        CLEAR DISPLAY PSEUDO REGISTERS
**********************************************************************
*
         MVI   DISPLAY,X'FF'       SET DISP REGS TO '-1'
         MVC   DISPLAY+1(L'DISPLAY-1),DISPLAY
*
************************************************************
*        LINK PASCAL FILE VARIABLES TO FILE CONTROL
*        BLOCKS IN THIS SUBMONITOR PROGRAM
************************************************************
*
         L     AE,=A(FILLIST)
         L     AE,0(,AE)           POINT TO FIRST FILE CONTROL BLOCK
         LA    AD,INPUT            POINT TO FIRST FILE VARIABLE
*
FILLP    DS    0H
         ST    AD,FILPAS(AE)       SET LINK FROM HERE TO THERE
         ST    AE,PFILPTR(AD)      AND FROM THERE TO HERE
*
         CLI   FILDIR(AE),C'O'
         BE    FILLP1
         MVI   FILEOFP(AE),FALSE   EOF initial false bei INPUT/INOUT
         B     FILLP2
FILLP1   DS    0H
         MVI   FILEOFP(AE),TRUE    EOF initial true
FILLP2   DS    0H
         MVI   FILEOLN(AE),FALSE   INITIALIZE EOL FLAG IN PASCAL
*
         LA    AD,PFILTSIZ(AD)     ADVANCE TO NEXT BUILT IN VRBL.
         L     AE,FILLNK(AE)       ADVANCE TO NEXT FILE CONTROL BLOCK
         LTR   AE,AE               TEST FOR END OF LIST
         BNZ   FILLP               REPEAT
*
************************************************************
*        ende schleife zur initialisierung der fcbs
************************************************************
*
         L     R0,BUFSTORE         SIZE OF THE AREA TO BE RETURNED
         LA    R1,8(R1)            ADDRESS OF THE AREA TO BE RETURNED
         LR    R2,R1
         SR    R2,R12              R2 <-- SPACE LEFT FOR THE STACK
         C     R2,USESTORE
         LA    R2,SPCERR           ERROR CODE FOR LACK OF SPACE
         BL    QUIT1
*
***************************************************************
*        FREE SOME SPACE FOR O/S FILE BUFFERS (4K/FILE !)
***************************************************************
*
*        CVD   R0,DOWO1
*        ST    R1,WORKR1
*        UNPK  TRGETM4+16(9),DOWO1+3(5)
*        OI    TRGETM4+24,X'F0'
*        LA    R3,TRGETM4
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*        CVB   R0,DOWO1
*        L     R1,WORKR1
*
         FREEMAIN R,LV=(R0),A=(R1)
         L     R1,ALOSTORE+4       KEEP TRACK OF HOW MUCH CORE
         S     R1,BUFSTORE         TO RETURN TO THE O.S.
         ST    R1,ALOSTORE+4       AT END OF EXECUTION
*
***************************************************************
*        INITIALIZE FORTRAN ENVIRONMENT (IF THERE ARE FORTRAN
*        ROUTINES IN THE LOAD MODULE)
***************************************************************
*
         AIF   (&SYSTEM).SYS325
         L     R15,=V(IBCOM#)      SEE IF FORTRAN ENVIRONMENT INCLUDED
         LTR   R15,R15
         BZ    NOFORT
         BAL   R14,IBCOMINI(R15)   IF SO CALL IBCOM# INIT ENTRY POINT
*
***************************************************************
*        NOTE: THIS CALL SAVES R13 FOR IBCOMXIT, BE SURE TO HAVE
*        THE SAVE AREA CONSISTENT PRIOR TO CALLING IBCOMXIT
***************************************************************
*
.SYS325  ANOP
*
***************************************************************
*        SET THE 'SPIE' TO TRAP PROGRAM  INTERRUPTS
***************************************************************
*
NOFORT   CLI   SPIEFLAG,X'00'      TEST IF SPIE TO BE ISSUED
         BNE   NOSPIE
         SPIE  MF=(E,PASSPIE)      OTHERWISE TRAP TO $PASINT
         ST    R1,OLDPICA          SAVE PREVIOUS PICA ADDRESS
NOSPIE   EQU   *
*
***************************************************************
*        SETUP DYN2STOR AREA
***************************************************************
*
         L     R1,NEWPTR           TOP OF HEAP
         S     R1,=A(DYN2LEN)      LESS SIZE OF DYN2
         ST    R1,HEAPLIM          AND LIMIT
         USING DYN2STOR,R1
         SR    R0,R0
         ST    R0,DYNRUNC          CLEAR '# OF COUNTERS' FIELD
         LH    R2,OSPARML
         LTR   R2,R2
         BZ    OSPARM1             JUMP IF NO PARM STRING
         SLR   R1,R2
         SL    R1,=F'4'            ALLOCATE PARM STRING RECORD
         SRL   R1,3                FORCE TO DOUBLE-WORD BOUNDARY
         SLL   R1,3
         ST    R2,0(,R1)           PUT STRING LENGTH IN RECORD
         L     R3,OSPARMAD
         BCTR  R2,0
         EX    R2,OSPRMMVC         MOVE STRING INTO RECORD
         ST    R1,OSPRMPTR         SET POINTER TO RECORD
         B     OSPARM2
OSPRMMVC MVC   4(0,R1),0(R3)
OSPARM1  BCTR  R2,0                SET POINTER TO NIL
         ST    R2,OSPRMPTR
OSPARM2  ST    R1,NEWPTR
         DROP  R1
*
*********************************************************************
*        DISABLE INTEGER OVERFLOW, EXPONENT UNDERFLOW AND
*        SIGNIFICANCE INTERRUPTS.
*********************************************************************
*
         SR    R6,R6
         SPM   R6                  DISABLE ALL MASKABLE INTERRUPTS
*
         MVC   FL1,=X'4E00000000000000'  INITIALIZE FIX-FLOAT-FIX
         MVC   FL2,=X'4E00000080000000'  CONVERSION VALUES
         MVC   FL3,=X'0000000000000000'
         MVC   FL4,=X'4F08000000000000'
*
         MVC   FL1OLD,=X'4E00000000000000'
         MVC   FL2OLD,=X'4E00000080000000'
         MVC   FL3OLD,=X'0000000000000000'
         MVC   FL4OLD,=X'4F08000000000000'
*
         MVC   CHKSUBS(L'CALLSUBS),CALLSUBS  INIT. RUN TIME CHECK AREA
*
         MVC   CLOCK,EXECTIME                SET THE ALARM CLOCK
         STIMER  TASK,$TIMEOUT,TUINTVL=CLOCK
*
*        UNPK  TRTIME+16(9),EXECTIME(5)
*        TR    TRTIME+16(8),TABHEXB-C'0'
*        MVI   TRTIME+24,C' '
*        LA    R3,TRTIME
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
**********************************************************************
*        INITIALIZE DATE/TIME PREDEFINED VARIABLES
**********************************************************************
*
         TIME  DEC                 GET TOD IN TU
*
*        FIX TIME OF DAY STRING
*
         L     R15,=A(FIXTIME)
         BALR  R5,R15              CONVERT TIME
*
         ST    R1,OLDDATE          STORE ORIGINAL DATE
         L     R15,=A(FIXDATE)
         BALR  R5,R15              CONVERT DATE
*
********************************************************************
*        FINALLY CALL THE USER PROGRAM
********************************************************************
*
         LA    1,STACK
         L     LINK,=A($PASMAIN)
         BALR  RET,LINK
*
********************************************************************
*        CLOSE THE OPEN FILES AND RETURN TO OS
********************************************************************
*
         SR    R2,R2               RETURN CODE = ZERO  !
*
QUIT1    LA    R1,PXIT             CLOSE OPEN FILES / RETURN TO OS
         L     LINK,=A($PASCSP2)
         BR    LINK                EXIT PASCAL PROGRAM
*
*
*
STRWTEMP DS    A
SAVETEMP DS    18F
SAVEPRM  DS    2A
TRGETM4  DC    CL80'TRACE:   free = XXXXXXXXX'
TRTIME   DC    CL80'TRACE:   time = XXXXXXXX'
DOWO1    DS    D
WORKR1   DS    F
TABHEXB  DC    C'0123456789ABCDEF'
*
*
*
*
****************************************************************
*
*        TABLE OF CALLS FOR RUN TIME CHECK ROUTINES.
*        TO BE COPIED, EXACTLY AS IS, ONTO THE RUN TIME STACK.
*
****************************************************************
*
CALSUB   DS    0F
CALLINX  L     R15,INXCHK+8
         BR    R15
         DC    A($INXCHK)
*
CALLRNG  L     R15,RNGCHK+8
         BR    R15
         DC    A($RNGCHK)
*
CALLPRM  L     R15,PRMCHK+8
         BR    R15
         DC    A($PRMCHK)
*
CALLPTR  L     R15,PTRCHK+8
         BR    R15
         DC    A($PTRCHK)
*
CALLPTA  L     R15,PTACHK+8
         BR    R15
         DC    A($PTACHK)
*
CALLSET  L     R15,SETCHK+8
         BR    R15
         DC    A($SETCHK)
*
CALLSTK  L     R15,STKCHK+8
         BR    R15
         DC    A($STKCHK)
*
CALLTRC  L     R15,TRACER+8
         BR    R15
         DC    A($PASTRC)
*
CALLSUBS EQU   CALSUB,*-CALSUB
*
         DROP  R10
*
*
*
*
****************************************************************
*
*        ROUTINE TO CONVERT THE TIME TO THE
*        CORRECT EXTERNAL FORMAT
*
****************************************************************
*
FIXTIME  DS    0H
         USING FIXTIME,R15
         ST    R10,FIXTIMES
         LR    R10,R15
         USING FIXTIME,R10
         DROP  R15
*
         ST    R0,DATESAV
         UNPK  DATESAV(7),DATESAV(4)   CONVERT TO EBCDIC
         MVC   PASTIME(10),=X'00010A02030A04054040'
         TR    PASTIME(8),DATESAV      RE-ORDER THE CHARACTERS
*
         L     R10,FIXTIMES
         BR    R5
*
         DROP  R10
*
*
*
*
****************************************************************
*
*        ROUTINE TO CONVERT THE DATE TO THE
*        CORRECT EXTERNAL FORMAT
*
****************************************************************
*
FIXDATE  DS    0H
         USING FIXDATE,R15
         ST    R10,FIXDATES
         LR    R10,R15
         USING FIXDATE,R10
         DROP  R15
*
         ST    R1,DATESAV              PUT DATE IN WORK AREA
         CP    DATESAV+2(2),=PL2'59'
         BNH   FIXDLY
         TM    DATESAV+1,1             LEAP YEAR?
         BNZ   FIXDNLY                 NO
         TM    DATESAV+1,X'12'         LEAP YEAR?
         BNM   FIXDLY                  YES
FIXDNLY  AP    DATESAV+2(2),=P'1'
FIXDLY   LA    R4,JAN
         LA    R3,12
         ZAP   MONTH(3),=P'0'
FIXDML   AP    MONTH(3),=P'1000'       BUMP MONTH
         CP    DATESAV+2(2),0(2,R4)    THIS MONTH?
         BNH   FIXDME                  BR IF SO
         SP    DATESAV+2(2),0(2,R4)    TRY NEXT
         LA    R4,2(R4)
         BCT   R3,FIXDML
FIXDME   L     R3,DATESAV
         N     R3,=X'00FF0000'         GET YEAR
         O     R3,MONTH-2              INSERT MONTH
         L     R4,DATESAV              GET DAY
         SRL   R4,4
         N     R4,=X'000000FF'
         OR    R3,R4
         ST    R3,DATESAV              PREPARE TO REFORMAT DATE
         UNPK  DATESAV(9),DATESAV(5)
         MVC   PASDATE(10),=X'04050B06070B00010203'
         TR    PASDATE(10),DATESAV     RE-ORDER THE CHARACTERS
*
********************************************************************
*        FIX YEAR-2000 PROBLEM / BERND OPPOLZER / 10.2011
*        IF YEAR LESS THAN 70, THEN CENTURY = 2000
********************************************************************
*
         CLC   PASDATE+8(2),=CL2'70'
         BNL   FIXDNOK
         MVC   PASDATE+6(2),=CL2'20'
FIXDNOK  DS    0H
*
         L     R10,FIXDATES
         BR    R5
*
         DROP  R10
*
*
*
*
*
BUFSTORE DC    A(IOBUFSZE)
STRWSTOR DC    A(STRWSIZE)
REQSTORE DC    A(MINSTORE,MAXSTORE)
ALOSTORE DS    2A
OSPARMS  DS    A                   ADDRESS OF O.S. PARAMETERS
OSPARMAD DC    A(0)                POINTER TO O.S. STRING
USESTORE DC    A(8000)
OLDPICA  DC    A(1)
EXECTIME DC    XL4'7FFFFFFF'       DEFAULT TIME LIMIT
PASSPIE  SPIE  $PASINT,((1,7),9,11,12,15),MF=L
OSPARML  DC    H'0'                LENGTH OF PARM STRING
DUMPFLAG DC    X'00'               X'FF' IF DUMP REQUESTED
SPIEFLAG DC    X'00'               X'FF' IF SPIE NOT TO BE ISSUED
SNAPFLAG DC    X'00'               X'FF' IF SNAPSHOT NOT TO BE CALLED#
*
DATESAV  DS    2F        # THESE LOCATIONS TO SUCCEED W.O.GAPS
         DC    C'  :-'   # (UNPACKING BUFFERS ETC.)
         DC    X'1900'   #
MONTH    DS    3X        #
JAN      DC    P'31,29,31,30,31,30,31,31,30,31,30,31'
FIXDATES DS    A
FIXTIMES DS    A
OLDDATE  DS    F
*
         LTORG ,
*
*
*
*
***********************************************************************
*
*        INTERRRUPT PROCCESSING FOR PASCAL PROGRAMS
*
*        ONLY FIXED/FLOAT DIVISION BY ZERO AND EXPONENT OVERFLOW
*        INTERRUPTS ARE EXPECTED TO BE CAUGHT HERE, OTHER INTERRUPTS
*        IN GENERAL ARE CAUSED BY STACK/HEAP OVER FLOW OR A BAD I/O
*        FILE SPECIFICATION AND OR MISSING APPROPRIATE DD STATEMENTS.
*
***********************************************************************
*
         USING $PASINT,R15
$PASINT  B     *+12
         DC    X'7',C'$PASINT'
*
***************************************************************
*        INTDATA = AREA WHERE PASINT STORES THE ERROR INFORMATION
*        0 = unknown at the moment
*        4 = first half of psw, error code at position 7
*        8 = second half of psw aka instruction address
*        12 = register 0
*        16 = register 1
*        20 = register 2
*        24 = register 3 and so on ...
***************************************************************
*
         MVC   INTDATA(12),0(R1)   SAVE ALL INTERRUPT DATA
         MVC   INTDATA+12(20),20(R1)
         STM   R3,R13,INTDATA+24
         MVC   INTDATA+68(8),12(R1)
         LA    R0,PASINTA          GO TO PASINTA VIA THE CONTROL
         ST    R0,8(R1)            PROGRAM TO CANCEL SPIE EXIT
         BR    R14
         DROP  R15
*
PASINTA  BALR  R11,0               RE-ESTABLISH ADDRESSABILITY
         USING *,R11
         L     R1,=A(OLDPICA)      CANCEL THE SPIE TRAP
         L     1,0(R1)             THAT IS IN EFFECT
         SPIE  MF=(E,(1))
*
***************************************************************
*        GET INTERRUPT CODE AND POINT TO THE APPROPRIATE ERROR MESSAGE
***************************************************************
*
         SR    R4,R4
         IC    R4,INTDATA+7        GET THE INTERRUPT CODE
         LA    R8,2000(R4)         SET THE RETURN CODE
         IC    R4,MSGTBL(R4)
         LA    R3,MSGTXT+1(R4)     R3 --> ERROR MESSAGE
         IC    R4,MSGTXT(R4)       R4 --> MESSAGE LENGTH
*
***************************************************************
*        check if error is in $PASCSP
*        if so, build fake save area
***************************************************************
*
         L     R15,=A($PASINTF)
         BALR  R14,R15
         LTR   R1,R1
         BZ    NOTINSP
*
         LR    R13,R1
         B     KNOWNPRC
*
***************************************************************
*        SEE IF R10 POINTS TO THE BEGINING OF A PROC.
***************************************************************
*
NOTINSP  L     R12,=A(ALOSTORE)        GET THE STACK ADDRESS
         L     R12,0(R12)
         LA    R12,STACK-DYNSTORE(R12) POINT TO BASE OF THE STACK
         LA    R10,0(R10)
         LH    R5,0(R10)               R10 IS WITHIN BOUND, SEE IF
         CH    R5,=XL2'47F0'           IT POINTS TO A PROC ENTRY POINT
         BNE   FIXENTRY
         CR    R13,R12                 SEE IF SAVE AREA PTR IS
         BL    FIXENTRY                WITHIN BOUNDS
         C     R13,NEWPTR-STACK(R12)
         BH    FIXENTRY
         C     R10,16(R13)             CONSISTENCY CHECK
         BE    KNOWNPRC                THIS IS A USER PROCEDURE ?
*
***************************************************************
*        R10 POINTS TO NOWHERE, FAKE A PROCEDURE HEADING
***************************************************************
*
FIXENTRY ST    R12,4+FAKESA        CHAIN THE FAKE SAVE AREA
         L     R5,16(R12)          POINT TO $PASMAIN ENTRY POINT
         ST    R5,12+FAKESA        SET RET. ADR. FROM FAKE PROC
         LA    R10,FAKEPROC        POINT TO THE ENTRY OF FAKEPROC
         AR    R14,R10             ALSO SET THE ERROR LOCATION ADR
         LA    R13,FAKESA
*
***************************************************************
*        THIS IS THE ENTRY TO A FAKE PROC TO BE USED IF
*        NO MEANINGFULL PROC IS FOUND AFTER AN INTRRUPT
***************************************************************
*
KNOWNPRC L     R15,=A(SNAPOUT)
         BR    R15                 GO TO PRINT ERROR MESSAGE
*
         USING *,R15
FAKEPROC B     *+12
         DC    AL1(7),C'UNKNOWN'
         DC    CL8'INTDATA'
INTDATA  DC    19F'0'              INTERRUPT DATA
*
MSGTBL   DC    AL1(0,IMSG1,IMSG1,IMSG1,IMSG4,IMSG1,IMSG1,IMSG1,IMSG1)
         DC    AL1(IMSG2,IMSG1,IMSG2,IMSG3,IMSG1,IMSG1,IMSG2)
*
MSGTXT   DS    0C
IM1      DC    AL1(L'IMSG1),C'PROGRAM INTERRUPT, SEE RETURN CODE.'
IMSG1    EQU   IM1-MSGTXT,*-IM1-1
IM2      DC    AL1(L'IMSG2),C'DIVISION BY ZERO'
IMSG2    EQU   IM2-MSGTXT,*-IM2-1
IM3      DC    AL1(L'IMSG3),C'EXPONENT OVERFLOW'
IMSG3    EQU   IM3-MSGTXT,*-IM3-1
IM4      DC    AL1(L'IMSG4),C'ADDRESSING EXCEPTION'
IMSG4    EQU   IM4-MSGTXT,*-IM4-1
         DC    C'    '
*
***************************************************************
*
*        END OF INTERRUPT HANDLING ROUTINE
*
***************************************************************
*
*
*
*
***************************************************************
*
*        to build fake save area for errors in $PASCSP
*
***************************************************************
*
$PASINTF DS    0H
         USING $PASINTF,R15
         STM   R2,R14,SAVEPASI
         LR    R11,R15
         USING $PASINTF,R11
         DROP  R15
*
         L     R5,=A(SCHCSP)
         CLI   0(R5),X'00'         SEE IF INTERR. IN SP MODULE
         XR    R1,R1
         BE    PASINTZ
*
***************************************************************
*        IF INTERRUPTION OCCURED WITHIN THE '$PASCSP' ROUTINE PATCH UP
*        A SAVE AREA TO POINT TO CALLERS SAVE AREA FOR A MEANINGFULL
*        ERROR MESSAGE.
***************************************************************
*
         CLI   0(R5),X'01'
         BE    PASINT1
*
PASINT2  DS    0H
         L     R5,=A(SPUSERSA)
         L     R5,0(R5)
         LM    R14,R15,12(R5)
         L     R12,68(R5)
         L     R13,4(R5)
         LR    R10,R15             SET PROC ENTRY POINT ADR
         ST    R13,FAKESA+4        SET SAVE AREA CHAIN
         STM   R14,R15,FAKESA+12   SET RETURN ADR FIELD
         LA    R1,FAKESA
         B     PASINTZ
*
PASINT1  DS    0H
         L     R5,=A(SPUSERSA)         GET USER REGS
         LM    R12,R15,(R12-R1)*4(R5)  GET IMPORTANT VALUES
         LR    R10,R15                 SET PROC ENTRY POINT ADR
         ST    R13,FAKESA+4            SET SAVE AREA CHAIN
         STM   R14,R15,FAKESA+12       SET RETURN ADR FIELD
         LA    R1,FAKESA
         B     PASINTZ
*
PASINTZ  DS    0H
         LM    R2,R14,SAVEPASI
         BR    R14
*
         DROP  R11
*
SAVEPASI DS    18F
*
FAKESA   DC    6F'0'
*
*
*
*
*
         AIF   (&SYSTEM).SYS900
*
***************************************************************
*
*        $PASTRC IS CALLED FROM THE PASCAL CODE IN ORDER TO
*        ENTER A CONTROL TRANSFER INTO THE TRANSFER TABLE
*        AND (IF DESIRED) PRINT THIS TRANSFER.
*
*        CALLING CODE:
*                         BAL 14,TRACER
*                         DC  AL2( PARAMETER )
*
*        THE ROUTINE 'TRACER' IS ONE OF THE CHECK ROUTINES
*        INCLUDED ON THE PASCAL RUN STACK. ITS STRUCTURE IS
*        SIMPLY:
*
*                 TRACER  L    15,=V($PASTRC)
*                         BR   15
*
*        THE HALFWORD PARAMETER TO TRACER HAS THE FOLLOWING
*        INTERPRETATIONS:
*
*        1.  POSITIVE VALUE IS TAKEN TO MEAN A BRANCH TO THIS
*            ADDRESS RELATIVE TO THE START OF THE CURRENT
*            PROCEDURE (WHOSE BASE ADDRESS IS IN REG 10).
*            THIS IS THE USAGE FOR ALL BRANCHES INTERNAL TO A
*            PROCEDURE.
*
*        2.  ZERO VALUE IMPLIES A PROCEDURE RETURN TO THE
*            ADDRESS IN REG. 0.  NOTE: THIS IS ALSO THE USAGE
*            FOR A GOTO THAT EXITS THE CURRENT PROCEDURE.
*
*        3.  NEGATIVE VALUE IMPLIES A PROCEDURE CALL TO THE
*            ABSOLUTE ADDRESS WHICH IS HELD IN A LOCAL V-TYPE
*            ADDRESS CONSTANT WITHIN THE CURRENT PROCEDURE.
*            THE ADDRESS CONSTANT'S OFFSET WITHIN THE CURRENT
*            PROCEDURE IS THE NEGATIVE OF THE PARAMETER VALUE.
*
***************************************************************
*
         DROP  ,
         USING $PASTRC,R15
$PASTRC  STM   R0,R2,TRACESA+4
         L     R2,TRPTR           LOAD AND ADVANCE
         LA    R2,8(,R2)          THE CYCLIC POINTER
         N     R2,=F'127'
         ST    R2,TRPTR
         LH    R1,0(,R14)         LOAD AND TEST PARAMETER
         LTR   R1,R1
         BNP   TRACE4             JUMP IF PROC. CALL/RETURN
         LA    R1,0(R1,R10)       R1 = DESTINATION ADDRESS
TRACE2   ST    R1,TRTABL(R2)      PUT IN TABLE
         ST    R14,TRTABL+4(R2)   PUT ORIGIN ADDRESS+4 IN TABLE
         L     R0,TRLINES
         BCT   R0,TRACE6          JUMP TO PRINT TRANSFER
TRRET    BC    0,TRACE3
         LR    R14,R1
         LM    R0,R2,TRACESA+4
         BR    R14
TRACE3   NI    TRRET+1,X'0F'      CLEAR BRANCH CONDITION
         LA    R14,2(,R14)
         ST    R1,TRACESA
         LM    R15,R2,TRACESA
         BR    R15                JUMP TO CALLED PROCEDURE
TRACE4   BZ    TRACE5             JUMP IF PROC. RETURN
         OI    TRRET+1,X'F0'      FORCE LATER JUMP TO TRACE3
         LPR   R1,R1              MAKE OFFSET POSITIVE
         L     R1,0(R1,R10)       LOAD THE ADDRESS CONSTANT
         O     R1,TRACEFL1        SET FLAG BYTE (FOR CALL)
         B     TRACE2
TRACE5   LR    R1,R0
         LA    R1,0(,R1)          CLEAR HIGH BYTE
         O     R1,TRACEFL2        SET FLAG BYTE (FOR RETURN)
         B     TRACE2
TRACE6   ST    R0,TRLINES         STORE UPDATED LINE COUNT
         L     R15,=A(TRPR1)
         BALR  R14,R15            CALL PRINT ROUTINE
         USING *,R14
         L     R15,=A($PASTRC)    RESTORE BASE REG.
         DROP  R14
         L     R14,TRTABL+4(R2)   RESTORE RETURN REG
         B     TRRET
*
*      TRDUMP IS CALLED IN CASE OF ABNORMAL PROGRAM TERMINATION
*      TO PRINT OUT THE CONTENTS OF THE TRACE TABLE.
*
TRDUMP   LR    R10,R15
         DROP  ,
         USING TRDUMP,R10
         USING STACK,R12
         TM    TRPTR,X'80'
         BOR   R14                RETURN IF TABLE IS EMPTY
         ST    R14,TRDUMPSV       SAVE RETURN ADDRESS
         L     R15,=A($PASCSP2)
         LA    AD,OUTPUT
         L     AE,=A(FILOUT)
         TM    FILOPN(AE),WRITEOPN
         BNZ   TRDUMP1                 JUMP IF OUTPUT FILE OPEN
         LA    R1,PREW                 FORCE THE FILE TO BE OPEN
         B     TRDUMP2
TRDUMP1  LA    R1,PSKP            DOUBLE-SPACE
         LA    R2,2
TRDUMP2  BALR  R14,R15
         LA    R2,TRDUMPMS
         LA    R3,L'TRDUMPMS
         LR    R4,R3
         LA    R1,PWRS            PUT OUT HEADING
         BALR  R14,R15
         LA    R6,16              LOAD NO. OF TABLE ENTRIES
TRDUMP3  L     R2,TRPTR
         LA    R2,8(,R2)
         N     R2,=F'127'         CYCLICALLY ADVANCE POINTER
         ST    R2,TRPTR
         LA    R15,TRPR1
         L     R1,TRTABL(R2)      LOAD TABLE ENTRY
         LTR   R1,R1              TEST IF EMPTY
         BZ    *+6
         BALR  R14,R15            PRINT NON-EMPTY ENTRY
         XC    TRPRFST(4),TRPRFST FORCE NEXT ITEM ON NEW LINE
         BCT   R6,TRDUMP3
         L     R14,TRDUMPSV
         BR    R14                RETURN
*
*      TRPR1 OUTPUTS ONE ENTRY IN THE TRACE TABLE.  THE INDEX
*      OF THIS ENTRY IS GIVEN BY REG 2.
*
         DROP  ,
         USING TRPR1,R15
         USING STACK,R12
TRPR1    STM   R0,R15,TRPRSAV
         LR    R10,R15
         USING TRPR1,R10
         DROP  R15
         LA    R3,TRTABL(R2)
         MVC   TRPRDLIM(3),=C' ->'
         MVC   TRPRTAG(1),0(R3)
         LR    R6,R2
         SR    R0,R0
         TM    0(R3),X'FF'
         BZ    TRPR3              JUMP FOR NORMAL BRANCH
         BM    TRPR2              JUMP IF PROC. RETURN
         MVC   TRPRMSG(10),=C' CALL FROM'
         LA    R0,10
         B     TRPR3
TRPR2    MVC   TRPRMSG(12),=C' RETURN FROM'
         LA    R0,12
TRPR3    STH   R0,TRPRLEN         SAVE STRING LENGTH SO FAR
         L     R1,TRTABL+4(R6)    LOAD ORIGIN ADDRESS
         SH    R1,=H'4'
         LA    R15,TRLN
         BALR  R5,R15              CONVERT TO TEXT
         LTR   R0,R0              TEST RETURN CODE
         BNZ   TRPR6              JUMP IF INCOMPLETE INFO.
         TM    TRPRTAG,X'FF'      TEST TRANSFER TYPE
         BZ    TRPR4              JUMP IF NORMAL BRANCH
         MVC   TRPRDLIM(3),=C' TO'
TRPR4    LH    R1,TRPRLEN
         LA    R4,TRPRMSG(R1)
         ALR   R1,R3              COMPUTE NEW TEXT LENGTH
         EX    R3,TRPRMVC1        CATENATE STRINGS
         LA    R1,3(,R1)
         ALR   R4,R3
TRPR4A   MVC   0(3,R4),TRPRDLIM   CATENATE 'TO' SYMBOL
         STH   R1,TRPRLEN
         L     R1,TRTABL(R6)
         LA    R1,0(,R1)
         TM    TRPRTAG,X'FF'      TEST IF PROC. CALL
         BNO   *+8                SKIP NEXT ACTION IF NOT A CALL
         ST    R1,TRLNLAST        TELL TRLN ABOUT THE NEW PROC.
         LA    R15,TRLN
         BALR  R5,R15              CONVERT IT TO TEXT
         LTR   R0,R0              TEST RETURN CODE
         BNZ   TRPR9
         TM    TRPRTAG,X'FF'      TEST TRANSFER TYPE
         BZ    TRPR5              JUMP IF NORMAL BRANCH
         LR    R3,R4              FORCE FULL MESSAGE TEXT
TRPR5    LH    R1,TRPRLEN
         LA    R4,TRPRMSG(R1)
         ALR   R1,R3
         EX    R3,TRPRMVC1        CATENATE STRINGS
         LA    R1,2(,R1)
         ALR   R4,R3
         MVC   0(2,R4),=C'; '
         STH   R1,TRPRLEN
         B     TRPR10             GO AND PRINT MESSAGE
TRPR6    TM    TRPRTAG,X'FF'
         BZ    TRPR4              (SHOULD NOT OCCUR)
         CH    R0,=H'4'
         BH    TRPR7              JUMP IF NO INFO AT ALL
         LA    R2,6(,R2)          OTHERWISE, JUST USE
         SH    R4,=H'6'           THE PROCEDURE NAME
         LR    R3,R4
         B     TRPR4-6
TRPR7    LH    R1,TRPRLEN         IF NO INFO, WE WONT INCLUDE
         SH    R1,=H'3'           THE ORIGIN IN THE MESSAGE
         LA    R4,TRPRMSG-2(R1)
         B     TRPR4A
TRPR9    TM    TRPRTAG,X'FF'
         BZ    TRPR5              (SHOULD NOT OCCUR)
         LA    R2,5(,R2)          JUST USE THE PROCEDURE
         SH    R4,=H'5'           NAME PART
         LR    R3,R4
         B     TRPR5
TRPR10   L     R15,=A($PASCSP2)
         L     AE,=A(FILOUT)
         LA    AD,OUTPUT
         TM    FILOPN(AE),WRITEOPN
         BNZ   TRPR11
         LA    R1,PREW
         BALR  R14,R15            FORCE FILE TO BE OPEN
         B     TRPR12A
TRPR11   CLC   FILBUF(16,AE),TRPRFST   TEST IF FILE HAS BEEN USED
         BNE   TRPR12             SINCE LAST CALL TO TRPR
         LH    R1,TRPRFEND
         STH   R1,FILEND(AE)      RESET FILE ATTRIBUTES
         SH    R1,FILPTR(AE)
         CH    R1,TRPRLEN         SEE IF ENOUGH ROOM ON LINE
         BH    TRPR13             JUMP IF NOT
TRPR12   LA    R1,PWLN
         BALR  R14,R15            FORCE NEW LINE
TRPR12A  LA    R1,PWRS            PUT OUT A SPECIAL
         LA    R2,=C' *TRACE:'    MARGIN MESSAGE TO
         LA    R3,8               FLAG TRACE OUTPUT
         LR    R4,R3
         BALR  R14,R15
TRPR13   LH    R3,TRPRLEN
         LR    R4,R3
         LA    R2,TRPRMSG
         LA    R1,PWRS
         BALR  R14,R15            OUTPUT TRACE MESSAGE
         LH    R1,FILEND(AE)
         STH   R1,TRPRFEND        SAVE FILE STATUS
         LH    R1,FILPTR(AE)
         BCTR  R1,0               FLAG LINE AS 'OVER-FULL'
         STH   R1,FILEND(AE)
         MVC   TRPRFST(16),FILBUF(AE)   SAVE COPY OF FILE STATE
         LM    R0,R15,TRPRSAV
         BR    R14
*
*
*
*     TRLN CONVERTS AN ABSOLUTE ADDRESS IN THE CODE AREA INTO A
*     TEXT STRING THAT SPECIFIES THE SOURCE LINE NUMBER AND THE
*     PROCEDURE/FUNCTION NAME.  FOR EXAMPLE, THE FOLLOWING STRING
*     IS A POSSIBLE RESULT:
*            ' 276 IN TREE_SEARCH '
*     IF THE LINE NO. IS UNAVAILABLE, A QUESTION MARK APPEARS IN
*     THE MESSAGE.  IF THE PROCEDURE NAME IS UNAVAILABLE (BECAUSE
*     THE PROCEDURE WAS NOT GENERATED BY PASCAL), THE TEXT STRING
*     WILL BE ' ? IN UNKNOWN PROC. '.
*
*     INPUT PARAMETER:  REG 1 MUST HOLD THE CODE ADDRESS.
*
*     OUTPUT PARAMETERS:
*          REG. 2 = ADDRESS OF TEXT STRING,
*          REG. 4 = LENGTH OF TEXT STRING,
*          REG. 3 = LENGTH OF TEXT STRING IF PROC. NAME IS OMITTED,
*          REG. 0 = RETURN CODE (0 MEANS O.K., 4 MEANS LINE NUMBER
*                   APPEARS AS '?', 8 MEANS UNKNOWN PROCEDURE).
*
         DROP  ,
         USING TRLN,R15
TRLN     LA    R1,0(,R1)          CLEAR HIGH BYTE
         L     R2,TRLNLAST        BASE ADDRESS OF LAST PROC.
         LR    R0,R1
         SR    R0,R2              TEST IF CURRENT ADDRESS IS WITHIN
         BM    TRLN0              THIS LAST PROC.
         BZ    TRLN4
         CH    R0,=H'8192'
         BNL   TRLN0
         LH    R0,10(,R2)         THIS SHOULD BE THE CODE SIZE
         CL    R0,=F'8192'        OF THE PROCEDURE
         BNL   TRLN0
         AR    R0,R2
         CR    R1,R0
         BL    TRLN4              PASSED ALL CHECKS
*
TRLN0    LR    R2,R1
         N     R2,TRLNMSK         FORCE DOUBLE-WORD ALIGNMENT
         LH    R0,TRLNPROL        LOAD PROC. START PATTERN
         LA    R3,1024            SET 8K SEARCH LIMIT
TRLN1    CH    R0,0(,R2)
         BNE   TRLN2              JUMP IF MISMATCH
         CLC   2(3,R2),=X'F00C07'
         BE    TRLN6              JUMP IF MATCH FOR PROC. START
         CLC   2(3,R2),=X'F02C27'
         BE    TRLN6              JUMP IF MATCH FOR $PASMAIN
TRLN2    SH    R2,=H'8'
         BCT   R3,TRLN1
TRLN3    LA    R2,=C' ? IN UNKNOWN PROC. '
         LA    R4,20
         LA    R3,3
         LA    R0,8
         BR    R5
TRLN4    CLC   0(5,R2),=X'47F0F00C07'
         BE    TRLN5              DOUBLE-CHECK FOR STANDARD ENTRY CODE
         CLC   0(5,R2),=X'47F0F02C27'
         BNE   TRLN3
TRLN5    ST    R2,TRLNLAST        SAVE PROC. ADDR. FOR NEXT TIME
TRLN6    LH    R3,10(R2)
         CL    R3,=F'8192'
         BNL   TRLN10             JUMP IF NO LINE NUM TABLE PROVIDED
         ALR   R3,R2
         MVC   TRLNMSG+10(12),4(R3)
         MVC   TRLNMSG(10),TRLNPAT SET UP EDIT PATTERN
         SLR   R1,R2
         SRA   R1,1               CONVERT TO HALFWORD OFFSET
         LH    R2,2(,R3)
         SR    R0,R0
TRLN7    IC    R0,16(,R3)         LOAD ENTRY IN LINE NUM TABLE
         CH    R0,=H'250'
         BL    TRLN8              JUMP IF NOT ESCAPE MODE ENTRY
         CH    R0,=H'255'
         BE    TRLN9              EXIT IF END OF TABLE FLAG
         IC    R0,17(,R3)
         SLA   R0,8
         SR    R1,R0              PROCESS 1ST BYTE OF LARGE ENTRY
         SR    R0,R0
         IC    R0,18(,R3)         BUT TREAT LOW BYTE AS NORMAL ENTRY
         LA    R3,2(,R3)
TRLN8    LA    R3,1(,R3)
         LA    R2,1(,R2)
         SR    R1,R0
         BP    TRLN7
         BCTR  R2,0               CORRECT OVERSHOOT OF LINE NUM
TRLN9    CVD   R2,TRLNPINT
         LA    R1,TRLNMSG+5
         EDMK  TRLNMSG(6),TRLNPINT+5
         BCTR  R1,0
         LR    R2,R1
         LA    R4,TRLNMSG+22
         BCTR  R4,0               REMOVE
         CLI   0(R4),C' '         TRAILING
         BE    *-6                BLANKS FROM
         LA    R4,1(,R4)          PROCEDURE NAME
         SLR   R4,R1
         LA    R3,TRLNMSG+6
         SLR   R3,R1
         SR    R0,R0
         BR    R5
TRLN10   MVC   TRLNMSG(6),=C' ? IN '
         MVC   TRLNMSG+6(7),5(R2)
         LA    R2,TRLNMSG
         LA    R3,3
         LA    R4,13
         LA    R0,4
         BR    R5
*
TRTABL   DC    32F'0'              SPACE FOR 16 CONTROL TRANSFERS
TRPTR    DC    F'-8'
TRACESA  DS    4F
TRACEFL1 DC    XL4'FF000000'
TRACEFL2 DC    XL4'FE000000'
TRLINES  DC    F'1'               => NO TRACE OUTPUT
TRDUMPSV DS    F
TRDUMPMS DC    C'0*  TRACE OF LAST 16 CONTROL TRANSFERS'
TRPRMVC1 MVC   0(*-*,R4),0(R2)
TRPRSAV  DS    16F
TRPRLEN  DS    1H
TRPRFEND DS    1H
TRPRFST  DS    16X
TRPRTAG  DS    1X
TRPRDLIM DS    3C
TRPRMSG  DS    64C
TRLNPINT DS    D
TRLNMSK  DC    X'00FFFFF8'
TRLNPROL DC    X'47F0'
TRLNMSG  DC    23C' '
TRLNLAST DC    A($PASMAIN)
TRLNPAT  DC    X'402020202120',C' IN '
         LTORG
         AGO   .USE900
*
.SYS900  ANOP
*
*
*
*
**********************************************************************
*        - DUMMY VERSION FOR COMPACT SP
**********************************************************************
*
         USING $PASTRC,R15
$PASTRC  STM   R0,R1,TRACESA+4
         LH    R1,0(,R14)          LOAD PARAMETER
         LTR   R1,R1               TEST FOR TRANSFER TYPE
         BNP   TRACE4
         LA    R14,0(R1,R10)       OFFSET WITHIN CUR. PROC.
         L     R1,TRACESA+8
         BR    R14
TRACE4   BZ    TRACE5
         LPR   R1,R1
         LA    R14,2(,R14)         COMPUTE RETURN ADDRESS
         L     R0,0(R1,R10)        LOAD PROC. ADDRESS
         ST    R0,TRACESA
         LM    R15,R1,TRACESA      LOAD AFFECTED REG.S
         BR    R15                 GOTO PROCEDURE
TRACE5   LR    R14,R0              LOAD RETURN ADDRESS
         BR    R14                 AND GOTO IT
TRACESA  DS    3F
*
.USE900  ANOP
*
         DROP ,
*
*
*
*
**********************************************************************
*        RANGE CHECK, INDEX CHECK, PARAMETER CHECK
**********************************************************************
*
         USING $RNGCHK,R15
$RNGCHK  MVI   CHKRANGE,1
         BAL   R3,BADRNG
         DC    AL2(30,SUBERR),CL30'SUBRANGE VALUE OUT OF RANGE'
*
         USING $INXCHK,R15
$INXCHK  MVI   CHKRANGE,1
         BAL   R3,BADRNG
         DC    AL2(30,INXERR),CL30'INDEX VALUE OUT OF RANGE'
*
         USING $PRMCHK,R15
$PRMCHK  MVI   CHKRANGE,1
         BAL   R3,BADRNG
         DC    AL2(30,PARERR),CL30'ACTUAL PARAMETER OUT OF RANGE'
*
*
*
**********************************************************************
*        POINTER CHECK ROUTINE
**********************************************************************
*
         USING $PTACHK,R15
$PTACHK  SR    R0,R0
         BCTR  R0,0                R0 <-- '-1'
         CR    R0,R2
         BER   R14                 NIL VALUE BEING ASSIGNED
*
         BALR  R15,0               GO TO NEXT ROUTINE
*                                  TO CONTINUE CHECKING
*
         USING $PTRCHK,R15
         USING STACK,R12
$PTRCHK  LM    R0,R1,NEWPTR        LOAD POINTER RANGE
         CR    R2,R0               CHECK LOWER BOUND
         BL    BADPTR
         CR    R2,R1               CHECK UPPER BOUND
         BNHR  R14                 IF WITHIN BOUNDS, RETURN
BADPTR   MVI   CHKRANGE,1
         BAL   R3,RUNMSG
         DC    AL2(30,PTRERR),CL30'POINTER VALUE OUT OF RANGE'
*
*
*
**********************************************************************
*        SET MEMBER CHECK ROUTINE
**********************************************************************
*
         USING $SETCHK,R15
$SETCHK  MVI   CHKRANGE,1
         BAL   R3,RUNMSG
         DC    AL2(30,SETERR),CL30'SET MEMBER(S) OUT OF RANGE'
*
*
*
**********************************************************************
*        STACK/HEAP INTERFERENCE ERROR
**********************************************************************
*
         USING $STKCHK,R15
$STKCHK  BAL   R3,RUNMSG
         DC    AL2(30,STKERR),CL30'STACK/HEAP COLLISION'
*
*
*
**********************************************************************
*        PASCSP RECURSIVE CALL NOT POSSIBLE (THIS WAY)
**********************************************************************
*
         USING $CSPERR,R15
$CSPERR  BAL   R3,RUNMSG
         DC    AL2(30,CSPERR),CL30'$PASCSP RECURSION NOK'
*
*
*
*
**********************************************************************
*
*        'TIME OUT' ENTRY POINT
*
*        ASSUMES THAT R10 POINTS TO THE CURR PROC ENTRY POINT
*        THIS MAY NOT BE TRUE UNDER MFT OR SOME OTHER VARIANTS
*        OF THE OPERATING SYSTEM. ('STIMER' PECULIARITY)
*
**********************************************************************
*
$TIMEOUT DS    0H
         BALR  R15,0
         USING *,R15
*
         L     R12,=A(ALOSTORE)    RESTORE STACK POINTER IN CASE !
         L     R12,0(R12)
         LA    R12,STACK-DYNSTORE(R12)
         LR    R13,R12
*
*        ST    R10,TIMOENT
*        UNPK  TRTIMO+16(9),TIMOENT(5)
*        TR    TRTIMO+16(8),TABHEXA-C'0'
*        MVI   TRTIMO+24,C' '
*        LA    R3,TRTIMO
*        LR    R2,R15
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*        LR    R15,R2
*
         L     R14,=A(SCHCSP)
         CLI   0(R14),X'00'        SEE IF TIMER WENT OFF IN '$PASCSP'
         BE    TIMEOUT2
*
         CLI   0(R14),X'01'        CHECK FOR OLD STACK
         BNE   TIMEOUT1
*
         L     R14,=A(SPUSERSA)
         LM    R1,R14,0(R14)       IF SO, RESTORE REGS ACCORDINGLY
         B     TIMEOUT4
*
TIMEOUT1 DS    0H                  NEW STACK
*
*        LA    R3,TRTIMO1
*        LR    R2,R15
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*        LR    R15,R2
*
         L     R14,=A(SPUSERSA)
         L     R1,0(R14)
         L     R14,12(R1)
         L     R13,4(R1)
         LM    R1,R12,24(R1)
         B     TIMEOUT4
*
TIMEOUT2 DS    0H
*
*        LA    R3,TRTIMO2
*        LR    R2,R15
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*        LR    R15,R2
*
         L     R1,16               LOAD CVT ADDR
         L     R1,0(,R1)           LOAD TCB WORDS ADDR
         L     R1,4(,R1)           ADDR OF CURRENT TCB
         L     R1,120(,R1)         ADDR OF TQE
         L     R1,28(,R1)          ADDR OF RB
         L     R1,20(,R1)          WORD2 OF OLD PSW (AT LAST)
         LA    R14,0(,R1)          SET ERROR LOC TO THIS ADDRESS
*
TIMEOUT3 DS    0H
         C     R10,16(R13)         GO DOWN THE CALL CHAIN UP TO
         BE    TIMEOUT4            CURRENT PROC.
         L     R13,8(R13)          PROCEED TO NEXT SAVE AREA
         B     TIMEOUT3
*
TIMEOUT4 DS    0H
         BAL   R3,RUNMSG
         DC    AL2(30,TIMERR),CL30'TIME LIMIT EXCEEDED'
*
TIMOENT  DS    A
TRTIMO   DC    CL80'TRACE:   timo  (xxxxxxxx = reg 10)'
TRTIMO1  DC    CL80'TRACE:   timo1'
TRTIMO2  DC    CL80'TRACE:   timo2'
*
*
**********************************************************************
*        BADRNG ROUTINE
**********************************************************************
*
BADRNG   BALR  R15,0
         USING *,R15
         LH    R0,0(,R14)          GET S-FORMAT ADDRESS PARM.
         STH   R0,*+6              PLANT IN NEXT INSTR.
         LM    R4,R5,*-*           *** SEE LINE ABOVE ***
         AR    R2,R5
         AR    R4,R5
         LR    R0,R5
         LR    R1,R4
         STM   R0,R2,CHKLOWR
         LA    R14,2(,R14)         TRUE RETURN ADDRESS
*
**********************************************************************
*        RUNMSG ROUTINE
**********************************************************************
*
RUNMSG   BALR  R15,0
         USING *,R15
         LH    R4,0(,R3)           LOAD MESSAGE LENGTH
         LH    R8,2(,R3)           LOAD ERROR CODE
         LA    R3,4(,R3)           LOAD MESSAGE ADDRESS
         LA    R15,SNAPOUT
         BR    R15                 continue at label SNAPOUT
*
*
*
*
**********************************************************************
*
*        THIS IS THE ENTRY POINT FOR RUN-TIME ERROR INDICATION
*        AND THE RELATED MESSAGES
*
*        AND: interface to passnap, if passnap is loaded and should
*        be activated due to runtime options (SNAP / NOSNAP)
*
**********************************************************************
*        reworked bernd oppolzer / from 03.2017 to 05.2017
*        inline snap dump (without passnap) was wrong in some aspects
*        large parts moved to separate procedures at the end of
*        this module, with separate base registers, see:
*        a) showregs - to print psw and register contents
*        b) showintr - to print interrupt address and other info
*        c) showstak - to print call stack etc.
*        d) showaddr - to print some interesting addresses of pasmonn
**********************************************************************
*
SNAPOUT  LR    R11,R15
         USING SNAPOUT,R11
         USING STACK,R12
         DROP  R15
         LA    R14,0(R14)
         LA    R15,0(R15)          GET RID OF FLAG BITS
         STM   R0,R15,CHKREGS      SAVE RELEVANT REGISTERS
         ST    R3,CHKMSG
         ST    R4,CHKMSGL
         ST    R8,CHKERRC          SAVE ERROR CODE
         L     R7,=A($PASENT)
         ST    R7,CHKPASE          SAVE $PASENT entry
         L     R7,NEWPTR
         ST    R7,CHKHEAPP         SAVE HEAP POINTER
         L     R7,HEAPLIM
         ST    R7,CHKHEAPT         SAVE TOP OF HEAP
*
**********************************************************************
*        check IF 0cx interrupt or pascal runtime check
**********************************************************************
*
         L     R7,=A(INTDATA+4)    POINT AT INTERRUPT PSW
         TM    3(R7),X'FF'
         BZ    SNAPOUT1            NO 0CX error
*
         MVC   CHKPSW1(72),0(R7)   PSW + REGS from INTDATA + 4
         L     R15,CHKPSW2
         LA    R15,0(R15)
         ST    R15,CHKINTRP
         MVI   CHKITYPE,C'S'       SYSTEM CODE (0CX)
         B     SNAPOUT2
*
SNAPOUT1 DS    0H
         XR    R15,R15
         ST    R15,CHKPSW1
         L     R15,CHKREGS+4*R14   MOVE R14 TO INTERRUPT ADDRESS
         LA    R15,0(R15)          GET RID OF FLAG BITS
         ST    R15,CHKINTRP
         ST    R15,CHKPSW2
         MVI   CHKITYPE,C'P'       PASCAL RUNTIME CHECK
*
SNAPOUT2 DS    0H
         L     R15,=A($PASCSP2)
         L     R1,=A(FILOUT)           REFER TO OUTPUT FCB
         LA    R9,OUTPUT
         TM    FILOPN(R1),WRITEOPN     CHECK IF "OUTPUT" IS OPEN
         BNZ   SNAPOUT4                JUMP IF OPEN
*
         LA    R1,PREW
         BALR  R14,R15             CALL PASCSP TO OPEN THE FILE
*
**********************************************************************
*        check is passnap is loaded and should be called
*        if not, go to nosnap and produce a short "inline" dump
**********************************************************************
*
SNAPOUT4 DS    0H
         L     R2,SNAPADR
         LTR   R2,R2               IS SNAPSHOT LOADED ?
         BZ    NOSNAP
*
         L     R2,=A(SNAPFLAG)
         CLI   0(R2),X'00'         SNAPSHOT TO BE USED?
         BNE   NOSNAP              NO
*
         MVI   0(R2),X'FF'         PREVENT SNAPSHOT BEING REENTERED
*
**********************************************************************
*        IF AN INTERRUPT, DUMP PSW AND REGISTERS
**********************************************************************
*
         L     R2,=A(FILOUT)       FORCE "OUTPUT" FILE TO
         NI    FILBEG+1(R2),X'FE'  ACCEPT CONTROL CHARS
*
         GETMAIN EC,LV=SNAPAREA,A=SNAPLOCS
         LTR   R15,R15             SUCCESSFUL ?
         BNZ   NOSNAP              NOT ENOUGH SPACE FOR SNAPSHOT
*
**********************************************************************
*        R1 POINTS TO THE BASE OF GOTTEN AREA
*        adjust newptr to prevent stack collision errors
*        when passnap calls other routines - opp 04.2017
**********************************************************************
*
         L     R1,SNAPLOCS
*
         MVC   SAVENEWP,NEWPTR
         LR    R15,R1
         AH    R15,=AL2(SNAPAREA)
         ST    R15,NEWPTR
*
         SR    R2,R2
         BCTR  R2,0                R2 <-- '-1'
         LA    R3,CHKERRC          SECOND PARM OF 'SNAPSHOT'
         STM   R2,R3,LCAFTMST+FPRSAREA(R1)
*
**********************************************************************
*        call passnap
**********************************************************************
*
         L     R15,SNAPADR
         BALR  R14,R15
*
**********************************************************************
*        restore newptr and
*        free passnap workarea
**********************************************************************
*
         MVC   NEWPTR,SAVENEWP
         FREEMAIN R,LV=SNAPAREA,A=SNAPLOCS  RETURN THE AREA TO O.S.
*
         AIF   (&SYSTEM).SYS910
*
         L     R15,=A(TRDUMP)      CALL THE TRACE TABLE DUMP
         BALR  R14,R15             ROUTINE TO PRINT LAST TRANSFERS
*
.SYS910  ANOP
*
         L     R2,=A(SNAPFLAG)
         MVI   0(R2),X'00'         REALLOW SNAPSHOT CALLS
         L     R2,=A(CCFLAG)
         TM    0(R2),X'FF'         TEST IF "NOCC" OPTION ON
         BZ    SNAPDONE            JUMP IF NOT
         L     R2,=A(FILOUT)
         OI    FILBEG+1(R2),X'01'  READJUST FILE BLOCK
         B     SNAPDONE
*
**********************************************************************
*        produce a short inline snap dump
**********************************************************************
*
NOSNAP   DS    0H
*
**********************************************************************
*        IF AN INTERRUPT, DUMP PSW AND REGISTERS
**********************************************************************
*
         LA    R7,CHKPSW1
         L     R15,=A(SHOWREGS)    dump psw and registers
         BALR  R14,R15
         L     R15,=A(SHOWINTR)    dump interrupt information
         BALR  R14,R15
         L     R15,=A(SHOWSTAK)    dump call stack
         BALR  R14,R15
         L     R15,=A(SHOWADDR)    dump some interesting addresses
         BALR  R14,R15
         L     R8,CHKERRC          LOAD ERROR CODE
         LA    R8,SNPERR(R8)       error code += 100
         ST    R8,CHKERRC          LOAD ERROR CODE
*
**********************************************************************
*        snap has been produced, either by calling passnap
*        or inline
**********************************************************************
*
SNAPDONE DS    0H
         L     R15,=A($PASCSP2)
         LA    R9,OUTPUT
         LA    R1,PWLN             ANOTHER LINE FOR LUCK
         BALR  R14,R15
*
         L     R8,CHKERRC          LOAD ERROR CODE
         L     R1,=A(DUMPFLAG)
         CLI   0(R1),X'FF'         TEST IF O.S. DUMP REQUESTED
         BNE   SNAPDON1
*
         ABEND (R8),DUMP
*
SNAPDON1 DS    0H
         LR    R2,R8
         LA    R1,PXIT
         BR    R15
*
*
**********************************************************************
*        this area is input area to error diagnosis
*        and to passnap
**********************************************************************
*
CHKAREA  DS    0D
*
CHKERRC  DS    F                   ERROR CODE
CHKPSW1  DS    A                   psw left part
CHKPSW2  DS    A                   psw right part
CHKREGS  DS    16F                 REGISTERS 0 TO 15
CHKINTRP DS    A                   INTERRUPT ADDRESS
CHKITYPE DS    C                   INTERRUPT TYPE (S OR P)
CHKRANGE DC    AL1(0)              range error flag
CHKFILE  DC    AL1(0)              file error flag
CHKDUMMY DS    C                   not used
CHKLOWR  DS    F                   range error lower bound
CHKUPPR  DS    F                   range error upper bound
CHKCVAL  DS    F                   offending value
CHKMSGL  DS    F                   message length
CHKMSG   DS    A                   pointer to message
CHKPASE  DS    A                   entry $PASENT
CHKHEAPP DS    A                   heap pointer
CHKHEAPT DS    A                   top of heap
*
**********************************************************************
*        end of input area
**********************************************************************
*
SAVENEWP DS    A
SNAPADR  DC    V($PASSNAP)
SNAPAREA EQU   20480               OVERESTIMATE OF SNAPSHOT DATA SIZE#
SNAPLOCS DS    A                   ADDRESS OF TEMP SNAPAREA
*
HEXBUF   DS    CL9                 UNPACKING BUFFER
HEXCHARS EQU   *-240               TRANSLATE TABLE FOR CODES
         DC    C'0123456789ABCDEF'     X'F0' TO X'FF' ONLY
*
         DROP  R11
         LTORG
*
*
*
*
******************************************************************
*
*
*        PASCAL I/O AND STANDARD PROCEDURE (CSP) INTERFACE
*
*        FOLLOWING CODE INTERCEPTS ERRORS DETECTED BY FORTRAN
*        MATH. ROUTINES.  SUCH ROUTINES ARE PRESENT IF REFERRED
*        TO BY THE PASCAL PROGRAM.
*
******************************************************************
*
         AIF   (&SYSTEM).SYS38
*
*        IHCERRM - CALLED BY FORTRAN MATH ROUTINES IN CASE OF ERROR
*
ERRMON   DS    0H
IHCERRM  DS    0H
IHOERRM  DS    0H
IHNERRM  DS    0H
*
         ENTRY IHCERRM,IHOERRM,IHOERRE,IHNERRM,IHNERRE,IHCERRE
         ENTRY  ERRMON
         USING IHCERRM,R15
         L     R1,0(R1)            GET 1ST ARG - PTR TO A(LEN),C'MSG'
         SR    R4,R4
         IC    R4,3(R1)            GET THE MESSAGE LENGTH
         LA    R3,4(R1)            POINT TO MESSAGE TEXT
         NOPR  0                   WARNING - THE LOCATION ERRMON+19
         BC    0,0                 CAN BE OVERWRITTEN BY FORTRAN
*
*        SET RELEVANT REGS ACCORDING TO SNAPOUT CONVENTIONS
*
         L     R13,4(R13)          POINT TO THE ORIGINAL SAVE AREA
         L     R10,12+4*1(R13)     ENTRY POINT OF THE CURRENT ROUTINE
         LR    R14,R10             SET ERROR LOC TO ENTRY POINT TOO
         L     R12,=A(ALOSTORE)
         L     R12,0(R12)
         LA    R12,STACK-DYNSTORE(R12)    AND THE GLOBAL DATA POINTER
*
         L     R15,=A(SNAPOUT)
         LA    R8,3001             SET ERROR RETURN CODE
         BR    R15                 AND JOIN THE ERROR HANDLER
         DROP  R15
*
*        THE FOLLOWING IS FOR THE FORTRAN ERROR MESSAGES
*
IHCERRE  DS    0H
IHNERRE  DS    0H
IHOERRE  BR    R14                 THIS SHOULD NOT BE CALLED
*
         LTORG
*
.SYS38   ANOP
*
*
*
*
*****************************************************************
*
*        $PASSVCC (String aka VARCHAR concat)
*
*        Unterfunktion zum Verketten von Strings
*        (wenn Inline nicht geht)
*        Parameter bei R1 = STRCURR:
*        0(R1) - Adresse erster String
*        4(R1) - Adresse zweiter String
*        8(R1) - Adresse des Ergebnisses
*
*****************************************************************
*
$PASSVCC DS    0H
         STM   R14,R12,32(R1)    Registersicherung in String Workarea
         USING $PASSVCC,15
         LR    R2,R1
         LA    R1,100(R1)
*
         L     R4,0(R2)          r4 = addr of first string desc
         L     R5,4(R2)          r5 = addr of second string desc
         LR    R6,R1             r6 = addr of target string desc
         ST    R6,8(R2)          store into result addr
         LH    R7,2(R4)          length of first string
         AH    R7,2(R5)          add length of second string
         STH   R7,0(R6)          maxlength of target
         STH   R7,2(R6)          length of target
*
         LA    R6,4(R6)          addr of target string content
         LH    R7,2(R4)          length of first string
         LH    R5,0(R4)          load maxlength
         LA    R4,4(R4)          set r4 to content addr
         LTR   R5,R5             if maxlength < 0
         BNM   P01
         L     R4,0(R4)          load content addr indirect
P01      LR    R5,R7             length in r5 := r7
         MVCL  R6,R4             copy first string
*
*        R6 has been advanced by MVCL :-)
*
         L     R4,4(R2)          r4 = now addr of second string desc
         LH    R7,2(R4)          length of second string
         LH    R5,0(R4)          load maxlength
         LA    R4,4(R4)          set r4 to content addr
         LTR   R5,R5             if maxlength < 0
         BNM   P02
         L     R4,0(R4)          load content addr indirect
P02      LR    R5,R7             length in r5 := r7
         MVCL  R6,R4             copy second string
*
*        R6 has been advanced by MVCL :-)
*
         ST    R6,12(R2)         new value for STRCURR
*
         LR    R1,R2
         LM    R14,R12,32(R1)
         DROP  R15
         BR    14
*
*
*
*
*****************************************************************
*
*        $PASSCMP (String aka VARCHAR compare)
*
*        Unterfunktion zum String-Vergleich in PASMONN
*        die Adressen der beiden String-Deskriptoren
*        stehen bei STRCURR
*
*****************************************************************
*
$PASSCMP DS    0H
         STM   R14,R12,32(R1)    Registersicherung in String Workarea
         USING $PASSCMP,15
         LR    R2,R1
         LA    R1,100(R1)
*
         L     R7,0(R2)      r7 = carr / varc indicator first str
         L     R6,4(R2)      r6 = addr of first string or desc
         L     R5,8(R2)      r5 = carr / varc indicator second str
         L     R4,12(R2)     r4 = addr of second string or desc
*
SCMP1    DS    0H
         LTR   R7,R7         if length positive (that is: CARR)
         BNM   SCMP2         use length in R7 and addr in R6
         LH    R7,0(R6)
         LTR   R7,R7         if maxlength negative then
         BM    SCMP1A        string on stack representation
         LH    R7,2(R6)      length of first string
         LA    R6,4(R6)      address of first string content
         B     SCMP2
SCMP1A   DS    0H
         LH    R7,2(R6)      length of first string
         L     R6,4(R6)      address of first string content
         B     SCMP2
*
SCMP2    DS    0H
         LTR   R5,R5         if length positive (that is: CARR)
         BNM   SCMPOK        use length in R5 and addr in R4
         LH    R5,0(R4)
         LTR   R5,R5         if maxlength negative then
         BM    SCMP2A        string on stack representation
         LH    R5,2(R4)      length of second string
         LA    R4,4(R4)      address of second string content
         B     SCMPOK
SCMP2A   DS    0H
         LH    R5,2(R4)      length of second string
         L     R4,4(R4)      address of second string content
         B     SCMPOK
*
SCMPOK   DS    0H
*
*        CVD   R4,DOWOCMP
*        UNPK  TRCMP1+24(9),DOWOCMP+3(5)
*        OI    TRCMP1+32,X'F0'
*        LA    R3,TRCMP1
*        LR    R10,R15
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
*        LR    R15,R10
*        MVC   TRCMPINH+24(8),0(R4)
*        LA    R3,TRCMPINH
*        LR    R10,R15
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*        LR    R15,R10
*
*        CVD   R5,DOWOCMP
*        UNPK  TRCMP2+24(9),DOWOCMP+3(5)
*        OI    TRCMP2+32,X'F0'
*        LA    R3,TRCMP2
*        LR    R10,R15
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*        LR    R15,R10
*
*        CVD   R6,DOWOCMP
*        UNPK  TRCMP3+24(9),DOWOCMP+3(5)
*        OI    TRCMP3+32,X'F0'
*        LA    R3,TRCMP3
*        LR    R10,R15
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*        LR    R15,R10
*
*        LR    R15,R10
*        MVC   TRCMPINH+24(8),0(R6)
*        LA    R3,TRCMPINH
*        LR    R10,R15
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*        LR    R15,R10
*
*        CVD   R7,DOWOCMP
*        UNPK  TRCMP4+24(9),DOWOCMP+3(5)
*        OI    TRCMP4+32,X'F0'
*        LA    R3,TRCMP4
*        LR    R10,R15
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*        LR    R15,R10
*
         STM   R4,R7,16(R2)
         CR    R7,R5
         LA    R8,0          indicate equal length
         BE    P1
         LA    R8,1          indicate first string shorter
         BL    P1
         LA    R8,2          indicate second string shorter
         LR    R7,R5         use shorter length of second operand
P1       DS    0H
         LR    R5,R7         copy length to r5
         CLCL  R6,R4         compare strings
         BNE   READY         if neq, result found
*
         LTR   R8,R8         if shorter = 0, both strings
         BZ    READY         have equal length
*
         CH    R8,=H'2'
         BE    SHORT2
*
*******************************************************
*        short1 coding is for first string shorter case
*******************************************************
*
SHORT1   DS    0H
         LM    R4,R7,16(R2)
         LR    R3,R7         save shorter string length in R3
         LR    R8,R1         pointer to free area to R8
         LR    R9,R5
         SR    R9,R7         remaining length in R9
         LR    R5,R9         save in R5
         XR    R6,R6
         LA    R7,C' '
         SLL   R7,24
         MVCL  R8,R6         create blank buffer
         LR    R8,R1         restore blank buffer addr in r8
         LR    R9,R5         restore length
*                            r4 points to longer string content
         AR    R4,R3         add shorter string length = rest
         CLCL  R8,R4         compare rest with blanks
         B     READY
*
*******************************************************
*        short2 coding is for second string shorter case
*        the registers at the beginning are loaded from
*        different places, and the CLCL is coded in the
*        reverse direction ... the rest of the code
*        is the same !!
*******************************************************
*
SHORT2   DS    0H
         LM    R6,R7,16(R2)
         LM    R4,R5,24(R2)
         LR    R3,R7         save shorter string length in R3
         LR    R8,R1         pointer to free area to R8
         LR    R9,R5
         SR    R9,R7         remaining length in R9
         LR    R5,R9         save in R5
         XR    R6,R6
         LA    R7,C' '
         SLL   R7,24
         MVCL  R8,R6         create blank buffer
         LR    R8,R1         restore blank buffer addr in r8
         LR    R9,R5         restore length
*                            r4 points to longer string content
         AR    R4,R3         add shorter string length = rest
         CLCL  R4,R8         compare rest with blanks
         B     READY
*
READY    DS    0H
         LR    R1,R2
         LM    R14,R12,32(R1)
         DROP  R15
         BR    14
*
DOWOCMP  DS    D
TRCMP1   DC    CL80'TRACE: $PASSCMP REG 4 = XXXXXXXXX'
TRCMP2   DC    CL80'TRACE: $PASSCMP REG 5 = XXXXXXXXX'
TRCMP3   DC    CL80'TRACE: $PASSCMP REG 6 = XXXXXXXXX'
TRCMP4   DC    CL80'TRACE: $PASSCMP REG 7 = XXXXXXXXX'
TRCMPINH DC    CL80'TRACE: $PASSCMP INH.  = XXXXXXXXX'
*
*
*
*****************************************************************
*
*        $PASSYS (System Basisfunktionen)
*
*****************************************************************
*
         DS    0D
         USING *,15
$PASSYS  B     *+12
         DC    AL1(7),CL7'$PASSYS'
*
* ------ uebergangsweise
*
$PASSTOR EQU   $PASSYS
         ENTRY $PASSTOR
*
* ------ uebergangsweise
*
         STM   R1,R15,SPUSERSA
         MVI   SCHCSP,X'01'        SET CSP FLAG
         L     R13,=A(CSPSTCK1)    SET UP SYSTEM SAVE AREA
         LR    R10,R15
         LA    R11,4092(,R10)
         DROP  R15
         USING $PASSYS,R10
         USING STACK,R12
*
************************************************************
*        Pascal-Parameter adressieren
************************************************************
*
         LA    R2,LCAFTMST+FPRSAREA(R1)
         L     R3,0(R2)            R3 = Funccode
         L     R4,4(R2)            R4 = 2. Parameter
*
************************************************************
*        Abfrage Funktionscode
************************************************************
*
         CH    R3,=H'1'            = ALLOC ?
         BE    STOR01
         CH    R3,=H'2'            = FREE ?
         BE    STOR02
         CH    R3,=H'3'            = GIVEHPCB ?
         BE    STOR03
         CH    R3,=H'10'           = FILEFCB ?
         BE    FILEFCB
         CH    R3,=H'11'           = CMS-Call ?
         BE    CMSCALL
         CH    R3,=H'13'           = WAITT-Call
         BE    WAITT
         XR    R5,R5               sonst Rueckgabe Null
         B     STOR99
*
************************************************************
*        Alloc = Getmain
************************************************************
*
STOR01   DS    0H
         AH    R4,=H'8'            8 Bytes mehr
         GETMAIN R,LV=(R4)
         LR    R5,R1               Adresse nach R5
         MVC   0(4,R5),=C'PSTG'    Kennung vorne rein
         ST    R4,4(R5)            Laenge rein
         LA    R5,8(R5)            8 Bytes spaeter zurueckgeben
         B     STOR99
*
************************************************************
*        Freemain
************************************************************
*
STOR02   DS    0H
         LR    R5,R4               Adresse nach R5
         SH    R5,=H'8'            8 Bytes vorher
         CLC   0(4,R5),=C'PSTG'    steht hier die Kennung?
         BE    STOR02A             ja ?
         XR    R5,R5               nein, dann raus
         B     STOR99
STOR02A  DS    0H
         L     R4,4(R5)            Laenge holen und Bereich
         FREEMAIN R,LV=(R4),A=(R5)             freigeben
         XR    R5,R5               Null zurueckgeben
         B     STOR99
*
************************************************************
*        Give HPCB / Adresse statischer HPCB
************************************************************
*
STOR03   DS    0H
         LA    R5,HPCBGLOB
         B     STOR99
*
************************************************************
*        FILEFCB
************************************************************
*
FILEFCB  DS    0H
         LR    AD,R4               AD = R9 = Pascal File Block
         FILADRN ,                 Register setzen
         LR    R5,AE               R8 = FCB nach R5
         B     STOR99
*
************************************************************
*        CMSCALL
************************************************************
*
CMSCALL  DS    0H
         LR    R1,R4               CMS-Parameter nach R1
         SVC   X'CA'               CMS-SVC = 'CAMBRIDGE'
         DC    AL4(*+4)            Fehler-Adresse = naechste
         LR    R5,R15              Returncode nach R5
         B     STOR99
*
************************************************************
*        WAITT - Wait for completion of Terminal I/O
************************************************************
*
WAITT    DS    0H
*
         AIF   ('&SYSPARM' NE 'CMS').CMS001
*
         WAITT                     not supported on MVS
*
.CMS001  ANOP
*
         XR    R5,R5
         B     STOR99
*
************************************************************
*        Rueckkehr usw.
************************************************************
*
STOR99   DS    0H
         L     R1,SPUSERSA         Restore R1
         ST    R5,72(R1)           Funktionsergebnis
         MVI   SCHCSP,X'00'        CLEAR CSP FLAG
         LM    R2,R15,SPUSERSA+4
         BR    14
*
         DS    0D
HPCBGLOB DC    C'HPCB'             Eye Catcher Heap Control Block
         DC    F'-1'               First Pointer = NIL
         DC    F'-1'               Last Pointer = NIL
         DC    F'0'                Dummy = 0
*
TRSTOR   DC    CL80'TRACE: AUFRUF $PASSYS'
*
         LTORG
*
*
*
*
**********************************************************************
*        EXIT ROUTINE CALLED WHEN DCB IS OPENED
*
*        IN THIS ROUTINE, WE PROVIDE SOME REASONABLE DEFAULTS
*        FOR THE RECFM, LRECL, BLKSIZE AND BUFNO ATTRIBUTES
*        THIS ROUTINE ALSO SETS THE REREAD FLAG WHEN NEEDED
*        AT THE CHANGE-OVER BETWEEN TWO CONCATENATED
*        INPUT FILES.
**********************************************************************
*        opp 02.2017:
*        don't change dcb values on open for input
*        because values come from the file system
**********************************************************************
*
         DROP  ,
*
         USING XLSTRTOT,R15
         USING IHADCB,R1
         USING STACK,R12
*
XLSTRTOT DS    0H
         LA    R5,DEFAULTS         ENTRY FOR OUTPUT DCB'S
         LA    R0,OUTPUT
         CR    AD,R0               TEST IF FILE IS "OUTPUT"
         BNE   *+8                 IT USES DIFFERENT DEFAULTS
         LA    R5,OUTDFLTS
         BAL   R15,XLSTRT2         RESET BASE REGISTER & TRANSFER
         USING XLSTRTIN,R15
*
XLSTRTIN DS    0H
         LA    R5,DEFAULTS         ENTRY FOR INPUT AND INOUT FILES
         TM    DCBOFLGS,X'08'      TEST IF CONCAT. FLAG SET
         BZ    XLSTRT1
         L     R4,=A(RRFLAG)
         MVI   0(R4),X'FF'         REQUEST A REREAD
         B     XLSTRT2
XLSTRT1  OI    DCBOFLGS,X'08'      REQUEST PROC. OF CONCAT FILES
*
******************************************************************
*        BUFNO ggf. setzen
******************************************************************
*
XLSTRT2  DS    0H
         SR    R4,R4
         CLI   DCBBUFNO,0
         BNE   XLSTRT3             JUMP IF BUFNO PROVIDED
         CLI   FILTERM(AE),C'Y'    BUFNO = 1 on Terminal files
         MVI   DCBBUFNO,1
         BE    XLSTRT3
         MVC   DCBBUFNO(1),4(R5)
*
******************************************************************
*        RECFM ggf. setzen
******************************************************************
*        hier Modifikation 02.2017:
*        wenn OpenMode = I, keine Werte aus dem DCB aendern;
*        sie kommen alle aus dem FileSystem
******************************************************************
*
XLSTRT3  DS    0H
*
         CLI   OPENMODE,C'I'
         BE    XLSTRTE
*
         TM    DCBRECFM,X'FE'
         BNZ   XLSTRT4             JUMP IF RECFM SPECIFIED
         OC    DCBRECFM(1),5(R5)
*
******************************************************************
*        LRECL ggf. setzen - Nicht Textfile
******************************************************************
*
XLSTRT4  DS    0H
         TM    FILOPN(AE),TEXTFLAG
         BNZ   XLSTRT47            JUMP IF A TEXT FILE
         LH    R3,FILCSZ(AE)       GET FILE COMPONENT SIZE
         LA    AF,PFILCOMP(AD)
         CLI   DCBRECFM,X'80'
         BNL   XLSTRT45            JUMP IF RECFM=F OR U
         LA    R3,4(,R3)           ADD IN 4 BYTES FOR RDW
         S     AF,=F'4'            AND CORRECT BUFFER ADDRESS
XLSTRT45 ST    AF,FILBUF(AE)
         STH   R3,FILRSZ(AE)       SAVE CORRECT RECORD SIZE
         CH    R4,DCBLRECL         R4 is zero at this point !
         BNZ   XLSTRT5             JUMP IF LRECL SPECIFIED
         B     XLSTRT49            GO AND SET THE LRECL
*
******************************************************************
*        LRECL ggf. setzen - Textfile
******************************************************************
*
XLSTRT47 DS    0H
         CH    R4,DCBLRECL         R4 is zero at this point !
         BNZ   XLSTRT5             JUMP IF LRECL SPECIFIED
         LH    R3,0(R5)            load default lrecl
         TM    DCBRECFM,X'C0'
         BO    XLSTRT5             JUMP IF U-FORMAT (LEAVE LRECL=0)
         CLI   DCBRECFM,X'80'
         BNL   *+8                 JUMP IF F-FORMAT
         LA    R3,4(R3)            ALLOW FOR RDW/SDW IN REC. LENGTH
         TM    DCBRECFM,X'06'
         BZ    *+8                 JUMP IF NO CONTROL CHAR.
         LA    R3,1(R3)            ADD IN 1 BYTE FOR CONTROL CHAR
XLSTRT49 STH   R3,DCBLRECL
*
******************************************************************
*        BLOCKSIZE ermitteln bzw. korrigieren
******************************************************************
*
XLSTRT5  DS    0H
         LH    R3,DCBLRECL
         LH    R5,DCBBLKSI         R5 = specified blocksize
         CLI   DCBRECFM,X'80'
         BNL   XLSTRT6             JUMP IF NOT V-FORMAT
*
         LA    R3,4(R3)            LRECL + 4 IS MINIMUM BLKSIZE
         LR    R4,R3               R4 = minimum blocksize
         B     XLSTRT7
*
XLSTRT6  TM    DCBRECFM,X'C0'
         BNO   XLSTRT61
         STH   R3,DCBBLKSI         RECFM = U, blck = rec size
         BR    R14                 fertig
*
XLSTRT61 XR    R4,R4               RECFM = F
         DR    R4,R3               Blocksize in R5
         MR    R4,R3               auf Multiple of RecLen
         LR    R4,R3               R4 = RecLen
*
XLSTRT7  CR    R5,R4
         BNL   XLSTRT8
         LR    R5,R4
*
XLSTRT8  STH   R5,DCBBLKSI
*
XLSTRTE  DS    0H
         BR    R14
*
*
*
*
**********************************************************************
*        SYNAD ROUTINE
**********************************************************************
*
         USING SYNADRT,R15
         DROP  R1
*
SYNADRT  DS    0H
         LA    R3,OUTPUT           GENERATES MESSAGES FOR FILE ERRORS
         CR    AD,R3               CHECK IF PROBLEM WITH OUTPUT FILE
         BNE   *+8                 JUMP IF NOT OUTPUT FILE
         ST    R14,QUITADR         SAVE ERROR EXIT ADDRESS
         SYNADAF ACSMETH=QSAM      GENERATE THE ERROR DESCRIPTION
         MVC   SYNMSG+4(78),50(R1)   AND MOVE IT INTO LOCAL BUFFER
         SYNADRLS ,                NOW RELEASE THE SYSTEM'S BUFFER
         LA    R14,SYNMSG
         L     R10,=A(CSPBASE)     RESET SOME BASE REGS.
         LA    R11,4092(,R10)      (JUST IN CASE)
         USING CSPBASE,R10
         USING CSPBASE+4092,R11
         DROP  R15
         B     ERRMSGRT
*
         DROP  ,
*
*
*
*
**********************************************************************
*        OPEN ABEND EXIT
**********************************************************************
*
         USING ABNDEXIT,R15
*
ABNDEXIT DS    0H
         ST    R11,SAVEABND+4
         ST    R14,SAVEABND
         LR    R11,R15
         DROP  R15
         USING ABNDEXIT,R11
         ST    R1,ABNDPARM
         MVI   ABNDCODE,C' '
*
******************************************************************
*        First two Bytes: ABEND Code in first 12 bits
******************************************************************
*
         LH    R15,0(R1)
         SRL   R15,4
         STH   R15,HALFA
         UNPK  TRABND+24(5),HALFA(3)
         TR    TRABND+24(4),TABHEXA-C'0'
         MVI   TRABND+28,C' '
*
******************************************************************
*        Byte 2: Reason Code (e.g.: 013-xx)
******************************************************************
*
         XR    R15,R15
         IC    R15,2(R1)
         STH   R15,HALFA
         UNPK  TRABND+29(3),HALFA+1(2)
         TR    TRABND+29(2),TABHEXA-C'0'
         MVI   TRABND+31,C' '
*
******************************************************************
*        Byte 3: Option Byte, see below
******************************************************************
*
         XR    R15,R15
         IC    R15,3(R1)
         STH   R15,HALFA
         UNPK  TRABND+32(3),HALFA+1(2)
         TR    TRABND+32(2),TABHEXA-C'0'
         MVI   TRABND+34,C' '
*
*        LA    R3,TRABND
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
******************************************************************
*        Input to exit:
*
*        3(3)          1                Option mask.
*                      xxxx ...x        Reserved.
*                      .... 1...        Okay to recover.
*                      .... .1..        Okay to ignore.
*                      .... ..1.        Okay to delay.
*
*        Test if ignore bit is on
*        if not, set error code to 'X'
*        set 0 into option byte
*        (write message and abend immediately)
*        return from abndexit
******************************************************************
*
         L     R1,ABNDPARM
         TM    3(R1),X'04'
         BO    ABND1
         MVI   ABNDCODE,C'X'
         MVI   3(R1),X'00'
         B     ABNDZ
*
******************************************************************
*        if code = 013-14: member was issued for non-pds
*        set code accordingly (code = 'P')
*        set 20 into option byte
*        return from abndexit
******************************************************************
*
ABND1    DS    0H
         LH    R15,0(R1)
         SRL   R15,4
         CH    R15,=X'0013'
         BNE   ABND3
         CLI   2(R1),X'14'
         BNE   ABND2
         MVI   ABNDCODE,C'P'
         MVI   3(R1),X'14'
         B     ABNDZ
*
******************************************************************
*        if code = 013-18: member was not found
*        set code accordingly (code = 'N')
*        set 20 into option byte
*        return from abndexit
******************************************************************
*
ABND2    DS    0H
         CLI   2(R1),X'18'
         BNE   ABND3
         MVI   ABNDCODE,C'N'
         MVI   3(R1),X'14'
         B     ABNDZ
*
******************************************************************
*        otherwise: other errors which can be ignored
*        set code accordingly (code = 'E')
*        set 20 into option byte
*        return from abndexit
******************************************************************
*
ABND3    DS    0H
         MVI   ABNDCODE,C'E'
         MVI   3(R1),X'14'
         B     ABNDZ
*
ABNDZ    DS    0H
         XR    R15,R15
         IC    R15,3(R1)
         STH   R15,HALFA
         UNPK  TRABND+32(3),HALFA+1(2)
         MVI   TRABND+34,C' '
*
*        LA    R3,TRABND
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
         L     R14,SAVEABND
         L     R11,SAVEABND+4
         BR    R14
*
         DROP  ,
*
ABNDPARM DS    A
SAVEABND DS    2A
HALFA    DS    H
         DS    C
*
TRABND   DC    CL80'TRACE: CALL TO ABNDEXIT XXXX XX XX'
*                   012345....1....5....2....5....3....5.
ABNDCODE EQU   TRABND+35,1,C'C'
*
         LTORG
*
TABHEXA  DC    C'0123456789ABCDEF'
*
*
*
************************************************************
*        DCB EXIT LIST PARAMETERS -
*        see suggestions from gerhard postpischil
************************************************************
*
         AIF   ('&SYSPARM' EQ 'CMS').CMS101
*
         DS    0F
XLINPUT  DC    X'05',AL3(XLSTRTIN)
         DC    X'07',AL3(JFCBDSNM)
         DC    X'91',AL3(ABNDEXIT)
*
XLINOUT  EQU   XLINPUT
*
         DS    0F
XLOUTPUT DC    X'05',AL3(XLSTRTOT)
         DC    X'07',AL3(JFCBDSNM)
         DC    X'91',AL3(ABNDEXIT)
*
         AGO   .CMS102
*
.CMS101  ANOP
*
         DS    0F
XLINPUT  DC    X'85',AL3(XLSTRTIN)
*
XLINOUT  EQU   XLINPUT
*
         DS    0F
XLOUTPUT DC    X'85',AL3(XLSTRTOT)
*
.CMS102  ANOP
*
*
*
************************************************************
*        DEFAULT AND TARGET VALUES FOR FILES OTHER THAN "OUTPUT"
*        opp 2017: blocksize was 1600, changed to 0 for cms
*        opp 2017: make it equal to lrecl in exit routine
************************************************************
*
DEFAULTS DC    H'80'               TARGET DATA BYTES PER RECORD
         DC    H'0'                TARGET BYTES PER BLOCK
         DC    AL1(3)              DEFAULT BUFNO VALUE
         DC    X'50'               DEFAULT RECFM = VB
*
************************************************************
*        DEFAULT AND TARGET VALUES FOR "OUTPUT" FILE
*        opp 2017: blocksize was 1600, changed to 0 for cms
*        opp 2017: make it equal to lrecl in exit routine
************************************************************
*
OUTDFLTS DC    H'132'              TARGET DATA BYTES PER RECORD
         DC    H'0'                TARGET BYTES PER BLOCK
         DC    AL1(5)              DEFAULT BUFNO VALUE
         DC    X'54'               DEFAULT RECFM = VBA
*
SYNMSG   DC    H'-79',AL2(SYNERR),CL79' '
*
QUITADR  DC    A(XIT)
*
OPENMODE DS    C
*
*
*
*
*
*
*
*****************************************************************
*
*        $PASCSP (PASCAL I/0) ENTRY POINT - new technique
*
*        R8 points to Pascal runtime stack
*        $PASCSP always uses the Pascal stack
*        R1 contains the desired CSP number
*
*        The compiler now always generates calls to $PASCSP
*
*****************************************************************
*
         USING $PASCSP,R15
$PASCSP  B     P002
*
         DC    AL1(8),CL8'$PASCSP'
*
P002     DS    0H
*
         MVI   SCHCSP,X'02'            SET CSP FLAG - 02
         STM   R14,R12,12(R8)          store regs into Pascal stacks
         ST    R8,SPUSERSA             only for interrupt case
         ST    R8,8(R13)               chaining
         ST    R13,4(R8)               chaining
*
         LA    R13,72(R8)              SET UP SYSTEM SAVE AREA
         ST    R8,4(R13)               chaining
         ST    R13,8(R8)               chaining
*
****************************************************************
*        ab hier aufmachen, wenn trace gewuenscht
*        problem: R15 wird veraendert in PTRACE
****************************************************************
*
*        LR    R10,R15
*        USING $PASCSP,R10
*        DROP  R15
*
*        CVD   R1,DOWO
*        UNPK  TRSTART+35(9),DOWO+3(5)
*        OI    TRSTART+43,X'F0'
*        LA    R3,TRSTART
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
*        CVB   R1,DOWO
*        LM    R2,R5,28(R1)
*
*        LR    R15,R10
*        USING $PASCSP,R15
*        DROP  R10
*
****************************************************************
*        bis hier aufmachen, wenn trace gewuenscht
****************************************************************
*
         LA    R10,CSPBASE
         LA    R11,4092(,R10)
*
         L     R1,SUBTBL2(R1)
         BR    R1
*
*
*****************************************************************
*
*        $PASCSP2 (PASCAL I/0) ENTRY POINT - old technique
*
*        R1 contains the desired CSP number
*        or points to the Pascal runtime stack
*        $PASCSP decides and acts accordingly
*
*****************************************************************
*
         USING $PASCSP2,R15
$PASCSP2 B     P001
*
         DC    AL1(9),CL9'$PASCSP2'
*
P001     DS    0H
         C     R1,=A(SUBTBLZ2-SUBTBL2)
         BH    CSPNEWST
*
         MVI   SCHCSP,X'01'            SET CSP FLAG - 01
         STM   R1,R15,SPUSERSA         store regs in SPUSER area
         L     R13,=A(CSPSTCK1)        SET UP SYSTEM SAVE AREA
*
****************************************************************
*        ab hier aufmachen, wenn trace gewuenscht
*        problem: R15 wird veraendert in PTRACE
****************************************************************
*
*        LR    R10,R15
*        USING $PASCSP2,R10
*        DROP  R15
*
*        CVD   R1,DOWO
*        UNPK  TRSTART+35(9),DOWO+3(5)
*        OI    TRSTART+43,X'F0'
*        LA    R3,TRSTART
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
*        CVB   R1,DOWO
*        LM    R2,R5,SPUSERSA+4
*
*        LR    R15,R10
*        USING $PASCSP2,R15
*        DROP  R10
*
****************************************************************
*        bis hier aufmachen, wenn trace gewuenscht
****************************************************************
*
         LA    R10,CSPBASE
         LA    R11,4092(,R10)
*
         L     R1,SUBTBL2(R1)
         BR    R1
*
*
*****************************************************************
*        $PASCSP nutzt den Pascal Runtime Stack ...
*        die erste Savearea (R1) dient zur Ablage der User Regs
*        wir muessen gleich eine zweite machen, auf die R13
*        zeigen kann.
*****************************************************************
*
CSPNEWST DS    0H
*
         MVI   SCHCSP,X'02'            SET CSP FLAG - 02
         STM   R14,R12,12(R1)          store regs into Pascal stacks
         ST    R1,SPUSERSA             only for interrupt case
         ST    R1,8(R13)               chaining
         ST    R13,4(R1)               chaining
*
         LA    R13,72(R1)              SET UP SYSTEM SAVE AREA
         ST    R1,4(R13)               chaining
         ST    R13,8(R1)               chaining
*
         LA    R10,CSPBASE
         LA    R11,4092(,R10)
*
*        L     R1,0(R1)
*        CVD   R1,DOWO
*        UNPK  TRSTART+35(9),DOWO+3(5)
*        OI    TRSTART+43,X'F0'
*        LA    R3,TRSTART
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
*        L     R1,SPUSERSA
*        LM    R2,R5,28(R1)
*
         L     R1,0(R1)                Function Code from here
         L     R1,SUBTBL2(R1)
         BR    R1
*
         LTORG
*
*
*****************************************************************
*        Sprungtabelle fuer CSP Funktionen
*****************************************************************
*
SUBTBL2  DS    0A
         DC    A(CTR)
         DC    A(0,0,0,0,0,0,0,0,0)
         DC    A(PAG,GET,PUT,RES,REW,RDC,WRI,WRE)
         DC    A(WRR,WRC,WRS,WRX,RDB,WRB,RDR,RDH)
         DC    A(RDY,EOL,EOT,RDD,WRD,CLK,WLN,RLN)
         DC    A(RDI,EOF,ELN,RDS,TRP,XIT,FDF,SIO)
         DC    A(EIO,MSG,SKP,LIM,TRA,WRP,CLS,DAT)
         DC    A(TIM,FLR,TRC,RND,WRV,RDV,RFC,RFS)
         DC    A(RFV)
         DC    A(0,0,0,0,0,0,0,0)
SUBTBLZ2 DS    0A
*
TRSTART  DC    CL80'TRACE: FUNKTIONSAUFRUF XXX BEI ADR XXXXXXXX'
*
CSPSTCK1 DS    18F
*
*        PRINT NOGEN               TURN OFF DCB EXPANSIONS
*
*
**************************************************************
*        FILE CONTROL BLOCKS
**************************************************************
*        verkettete Liste von Dateidefinitionen;
*        die Namen werden aus den DD-Namen gebildet (FIL plus
*        erste drei Zeichen); der letzte Parameter ist die
*        Verkettung, d.h. Zeiger auf den naechsten File.
*        Das hier sind die bekannten Standard-Files, weitere
*        waeren moeglich; Move oder Locate-Mode wird im
*        Macro FILDEF festgelegt.
**************************************************************
*
         FILDEF INPUT,INPUT,TEXT,FILOUT
         FILDEF OUTPUT,OUTPUT,TEXT,FILPRD
         FILDEF PRD,INOUT,TEXT,FILPRR
*
#FILREC  EQU   FILPRD,*-FILPRD     TEXT FILE TEMPLATE
*
         FILDEF PRR,INOUT,TEXT,FILQRD
         FILDEF QRD,INOUT,TEXT,FILQRR
         FILDEF QRR,INOUT,TEXT,0
         FILDEF UNKNOWN,INOUT,NONTEXT,0
*
#NONTXT  EQU   FILUNK,*-FILUNK     NON-TEXT FILE TEMPLATE
*
*
*
*
*****************************************************************
*        hier position fuer basisregister
*        alles vorher wird nur fuer csp-startup gebraucht
*****************************************************************
*
CSPBASE  DS    0D
*
         DROP  R15
*
*****************************************************************
*
*        erst jetzt die Basisregister umsetzen,
*        bis hierher gilt R15
*
*****************************************************************
*
         USING CSPBASE,R10
         USING CSPBASE+4092,R11
         USING STACK,R12
         USING IHADCB-FILDCB,AE
*
SCHCSP   DC    X'00'
*
SPUSERSA DS    18F
*
FLHILF   DS    D
*
DOWO     DS    D
*
SAVERETC DS    F
SAVECHK  DS    5F
SAVETRC  DS    5F
SAVEGET  DS    5F
SAVEEOD  DS    5F
SAVERDS  DS    5F
SAVERDI  DS    5F
SAVERDI2 DS    F
*
SAVECLS  DS    2A
SAVEWRR  DS    2A
SAVEPOP  DS    2A
SAVEGOP  DS    2A
SAVEOPN  DS    2A
RETAGNB  DS    A
RETAPST  DS    A
*
CCFLAG   DC    X'00'               SET BY NOCC O.S. PARM STRING
*
FILLIST  DC    A(FILINP)           HEAD POINTER FOR CHAIN OF FILES
*
*****************************************************************
*        messages moved here, so that they can accessed by
*        first base register (10)
*****************************************************************
*
READMSG  DC    H'-30',AL2(INPERR),CL30'INVALID INPUT OPERATION'
WRITEMSG DC    H'-30',AL2(OUTERR),CL30'INVALID OUTPUT OPERATION'
FDFMSG   DC    H'-30',AL2(FDFERR),CL30'INVALID FILE DEFINITION'
UNDFMSG  DC    H'30',AL2(NDFERR),CL30'CALL TO UNDEF. STANDARD PROC.'
LIMMSG   DC    H'-30',AL2(LIMERR),CL30'LINELIMIT VALUE EXCEEDED'
EOFMSG   DC    H'-30',AL2(EOFERR),CL30'ILLEGAL READ AFTER EOF'
BOOLMSG  DC    H'-30',AL2(BOLERR),CL30'BAD BOOLEAN ON INPUT'
INTMSG   DC    H'-30',AL2(INTERR),CL30'BAD INTEGER ON INPUT'
BIGMSG   DC    H'-30',AL2(BIGERR),CL30'OVER-LARGE INTEGER ON INPUT'
REALMSG  DC    H'-30',AL2(RELERR),CL30'BAD REAL ON INPUT'
FILERMSG DC    H'-30',AL2(FILERR),CL30'INVALID FILE CONTROL BLOCK'
RECRDMSG DC    H'-30',AL2(RCDERR),CL30'INPUT RECORD TOO LARGE'
RSTAMSG  DC    H'-30',AL2(RSTERR),CL30'BAD FILE STATUS FOR READ'
WSTAMSG  DC    H'-30',AL2(WSTERR),CL30'BAD FILE STATUS FOR WRITE'
         DS    0H
MSGBUF   DC    CL128' '            BUFFER FOR WTO MESSAGES
*
MSGMVC   MVC   MSGBUF+4(*-*),0(R2)
*
*****************************************************************
*        VARIOUS TABLE AND DCB DEFINITIONS
*        moved here, so that all that can accessed by
*        first base register (10)
*****************************************************************
*
TABHEX   DC    C'0123456789ABCDEF'
DECTBL   DC    D'0,1,2,3,4,5,6,7,8,9'
PINT     DS    D                   PACKED INTEGER BUFFER
CINT     DC    30AL1(FILBLA)
ZINT     DS    CL12                ZONED INTEGER BUFFER
*
MAXSKIP  DC    F'60'               MAXIMUM NUMBER OF LINES TO SKIP
SKPASCII EQU   *+1                 TABLE OF ASCII CONTROL CHARS
         DC    C'1+ 0-'
SKPMACH  EQU   *+1                 TABLE OF MACHINE CONTROL CHARS
         DC    X'8901091119'
RRFLAG   DC    X'00'               REREAD FLAG
PROFFLAG DC    X'00'               CALL PROFILE MODULE FLAG
*
         LTORG ,
*
*
*
*
*****************************************************************
*        error exits - moved here so that base reg 11 is not needed
*****************************************************************
*
BADFDF   LA    R14,FDFMSG
         L     AE,=A(FILUNK)       PROVIDE A FILE
         MVC   FILNAM(8,AE),0(R2)  WITH RIGHT NAME
         B     ERRMSGRT
*
UNDFERR  LA    R14,UNDFMSG
         B     ERRMSGRT
*
LIMITERR LA    R14,LIMMSG
         B     ERRMSGRT
*
BADREAD  LA    R14,READMSG
         B     ERRMSGRT
*
EOFREAD  LA    R14,EOFMSG
         B     ERRMSGRT
*
BADBOOL  LA    R14,BOOLMSG
         B     ERRMSGRT
*
BIGINT   LA    R14,BIGMSG
         B     ERRMSGRT
*
BADINT   LA    R14,INTMSG
         B     ERRMSGRT
*
BADREAL  LA    R14,REALMSG
         B     ERRMSGRT
*
BADRECRD LA    R14,RECRDMSG
         B     ERRMSGRT
*
BADRSTA  LA    R14,RSTAMSG
         B     ERRMSGRT
*
BADWSTA  LA    R14,WSTAMSG
         B     ERRMSGRT
*
BADFILE  LA    R14,FILERMSG
         L     AE,FILLIST          SEARCH FOR CORRECT FILE BLOCK
BF1      C     AD,FILPAS(AE)
         BE    ERRMSGRT            EXIT IF FOUND
         L     AE,FILLNK(AE)       OTHERWISE TRY NEXT FILE
         LTR   AE,AE
         BNZ   BF1                 REPEAT LOOP
         L     AE,=A(FILUNK)       NOT FOUND - SO PROVIDE
         B     ERRMSGRT            A DUMMY FILE BLOCK INSTEAD
*
BADWRITE LA    R14,WRITEMSG
         B     ERRMSGRT
*
ERRMSGRT DS    0H
         LH    R3,0(,R14)          GET MESSAGE LENGTH
         LA    R2,4(,R14)          ADDR OF TEXT FOR MESSAGE
         LPR   R4,R3
         EX    R4,MSGMVC           MOVE MESSAGE INTO BIG BUFFER
         LTR   R3,R3               R3 < 0 => PROBLEM WITH A FILE
         BNM   ERR0                JUMP IF OTHER THAN A FILE ERROR
*
         L     R5,=A(CHKFILE)
         MVI   0(R5),1             SET FILE MSG INDICATOR
*
         LR    R5,R14              save R14
         L     R15,=A($PASINTF)    build fake save area for errors
         BALR  R14,R15             inside $pascsp
*
         LTR   R1,R1
         BZ    NOFAKESA
*
         CLI   0(R5),X'01'
         BE    FAKE1
*
FAKE2    DS    0H
         L     R14,SPUSERSA
         ST    R1,4(R14)               Reg 13 = FakeSA
         MVC   12(4,R14),12(R1)        Reg 14
         MVC   16(4,R14),=A($PASCSP)   Reg 15
         B     NOFAKESA
*
FAKE1    DS    0H
         ST    R1,SPUSERSA+48               Reg 13 = FakeSA
         MVC   SPUSERSA+52(4),12(R1)        Reg 14
         MVC   SPUSERSA+56(4),=A($PASCSP)   Reg 15
*
NOFAKESA DS    0H
*
         LR    R14,R5              restore R14
*
         LA    R5,MSGBUF+4(R4)
         MVC   0(26,R5),=CL26' (PASCAL FILE =         )'
         MVC   16(8,R5),FILNAM(AE)     MOVE FILE NAME INTO MSG
         LA    R4,26(,R4)          NEW MESSAGE LENGTH
*
         MVC   MSGBUF+2(2),=X'8000'
         LA    R4,4(,R4)           CONSTRUCT VARIABLE LENGTH RECORD
         STH   R4,MSGBUF
         LA    R3,MSGBUF(R4)
         MVC   0(4,R3),=X'00000020'    SET ROUTING CODE
         LA    R1,MSGBUF
         SVC   35                  ISSUE WTO REQUEST
*
         SH    R4,=H'4'
         LA    R0,OUTPUT
         CR    AD,R0               TEST IF FILE IS "OUTPUT"
         BNE   ERR0                IF NOT, CARRY ON AND PRINT MESSAGE
*
         L     R14,=A(QUITADR)     GET ERROR EXIT ADDRESS
         L     R14,0(R14)          GET ERROR EXIT ADDRESS
         BR    R14                 AND GO TO IT
*
ERR0     DS    0H
         LA    R3,MSGBUF+4         NEW MESSAGE ADDRESS
         LH    R8,2(,R14)          ERROR CODE
         L     R15,=A(SNAPOUT)     WHERE TO GO NEXT
*
         CLI   0(R5),X'01'
         BE    RESTORE1
*
RESTORE2 DS    0H
         L     R14,SPUSERSA
         LM    R1,R2,24(R14)       RESTORE MOST OF THE
         LM    R5,R7,40(R14)       USER'S REGISTERS
         LM    R9,R12,56(R14)
         L     R13,4(R14)
         L     R14,12(R14)
         BR    R15
*
RESTORE1 DS    0H
         LM    R1,R2,SPUSERSA      RESTORE MOST OF THE
         LM    R5,R7,SPUSERSA+16   USER'S REGISTERS
         LM    R9,R14,SPUSERSA+32
         BR    R15
*
*
*
*
*****************************************************************
*        Unterprogramme fuer Mathe usw.
*****************************************************************
*
*
*
*****************************************************************
*        Floor
*****************************************************************
*
         DC    CL4'FLR '
FLR      DS    0H
*
         LTDR  2,2                 Bei Null: nichts machen
         BZ    RTN
         BP    FLRP                positiv?
*
FLRN     DS    0H                  negativ
         STD   2,FL3               Originalwert speichern
         AD    2,FL4               auf Ganzzahl bringen durch
         STD   2,FLHILF            Addition von 4f08...
         LD    2,FLHILF
         SD    2,FL4
         CD    2,FL3               identisch mit Original?
         BZ    RTN                 dann raus
         SD    2,=D'1.0'           sonst eins abziehen
         B     RTN
*
FLRP     DS    0H                  positiv
         AD    2,FL4               auf Ganzzahl bringen durch
         STD   2,FLHILF            Addition von 4f08...
         LD    2,FLHILF
         SD    2,FL4
         B     RTN
*
*
*
*****************************************************************
*        Trunc
*****************************************************************
*
         DC    CL4'TRC '
TRC      DS    0H
*
         XR    2,2                 Ergebnis Init auf Null
         LTDR  2,2                 Bei Null: nichts machen
         BZ    RTNR2
*
         AD    2,FL4               auf Ganzzahl bringen durch
         STD   2,FLHILF            Addition von 4f08...
         L     2,FLHILF+4
         B     RTNR2
*
*
*
*****************************************************************
*        Round
*****************************************************************
*
         DC    CL4'RND '
RND      DS    0H
*
         C     R13,=A(CSPSTCK1)    R13 in Pascal stack ?
         BNE   RNDOK
*
         L     R15,=A($CSPERR)
         BALR  R14,R15
*
RNDOK    DS    0H
         LA    R1,72(R13)          save area position
         LA    R2,1                function number, roundx = 1
         ST    R2,112(R1)
         LA    R2,0                roundx 2nd argument, zero here
         ST    R2,116(R1)
         STD   2,120(R1)           value to be rounded
         L     R15,=V($PASMAT)     call $pasmat
         BALR  R14,R15
         LD    2,72(R1)            retrieve rounded value
         AD    2,FL4               auf Ganzzahl bringen durch
         STD   2,FLHILF            Addition von 4f08...
         L     R2,FLHILF+4
         B     RTNR2
*
         LTORG
*
*
*
*****************************************************************
*        Unterprogramme fuer file-handling-funktionen
*****************************************************************
*
*
*
*****************************************************************
*        (GET FETCH) GET A CHAR TO (AD)
*****************************************************************
*
PAFGFE   DS    0H
         LH    R1,FILPTR(AE)
         LA    R1,1(,R1)
         CH    R1,FILEND(AE)       TEST FOR END-OF-LINE
         BNL   PAFGFE2             IF SO, GO AND PROVIDE A BLANK
         IC    R0,0(R1,AF)         PICK UP INPUT CHARACTER
*
PAFGFE1  DS    0H
         STC   R0,PFILCOMP(AD)     AND PUT INTO CALLER'S BUFFER
         STH   R1,FILPTR(AE)
         BR    R5
*
PAFGFE2  DS    0H
         LA    R0,FILBLA           SET EOL CHARACTER
         MVI   FILEOLN(AE),TRUE    SET EOL FLAG
         BE    PAFGFE1             GO BACK TO RETURN EOL CHAR
         CLI   FILTERM(AE),C'Y'    Terminal File?
         BNE   PAFGET              No, goto PAFGET
*
*------- MVI   FILRDBS(AE),1       No READBUF on Terminal files
*
         BR    R5
*
*
*
*****************************************************************
*        GET A RECORD TO (AF)
*****************************************************************
*
PAFGET   DS    0H
         TM    FILEOF(AE),1
         BOR   R5                  RETURN IF END-OF-FILE
*
         AIF   (&SYSTEM).SYS42
         TM    FILOPN(AE),READOPN
         BZ    BADREAD             IF FILE N.O. FOR READ, COMPLAIN
.SYS42   ANOP
*
PAFGET1  DS    0H
*
*----------------------------------------------------------
*        nur test wg. MVS Bug
*        Anzahl Pruefen und bei > 2000 EOF Melden
*----------------------------------------------------------
*
*        STM   R1,R5,SAVEGET
*        LH    R3,XXANZ
*        LA    R3,1(R3)
*        STH   R3,XXANZ
*        CH    R3,=H'2000'
*        BL    XX001
*        MVI   FILEOF(AE),1        EOF SETZEN
*        BR    R5                  UND RUECKSPRUNG
*
*X001    DS    0H
*        LA    R3,TRGET
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*        LM    R1,R5,SAVEGET
*
*----------------------------------------------------------
*
         STM   R1,R5,SAVEGET
         BAL   R5,GETL
         LM    R1,R5,SAVEGET
*
*----------------------------------------------------------
*        Ausgabe Laenge und Adresse des Buffers
*----------------------------------------------------------
*
*        STM   R1,R5,SAVEGET
*        ST    AF,DOWO
*        UNPK  TRGET2+49(9),DOWO(5)
*        TR    TRGET2+49(8),TABHEX-C'0'
*        MVI   TRGET2+57,C' '
*        LH    R2,FILEND(AE)
*        CVD   R2,DOWO
*        UNPK  TRGET2+33(5),DOWO+5(3)
*        OI    TRGET2+37,X'F0'
*        LA    R3,TRGET2
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*        L     R2,0(AF)
*        ST    R2,DOWO
*        UNPK  TRGET3+16(9),DOWO(5)
*        TR    TRGET3+16(8),TABHEX-C'0'
*        MVI   TRGET3+24,C' '
*        MVC   TRGET3+25(40),0(AF)
*        LA    R3,TRGET3
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*        LM    R1,R5,SAVEGET
*
*----------------------------------------------------------
*
         BR    R5
*
TRGET    DC    CL80'TRACE: BEFORE GET-CALL INSIDE PAFGET'
TRGET2   DC    CL80'TRACE: SUCCESSFUL RETURN LRECL = XXXXX ADDRBUF = XX*
               XXXXXX'
TRGET3   DC    CL80'TRACE: BUFFER = XXXXXXXX XXX'
XXANZ    DC    H'0'
*
*
*
*
*****************************************************************
*        (GET NON-BLANK) GET NEXT NON-BLANK TO (AD)
*****************************************************************
*
PAFGNB   DS    0H
         ST    R5,RETAGNB
PAFGNBA  LH    R1,FILPTR(AE)       LOAD ADRESS OF CHAR
PAFGNBN  CH    R1,FILEND(AE)       AT THE END OF BUFFER ?
         BL    PAFGNB0             NO, TEST CHAR
*
         BAL   R5,PAFGET           GET NEXT BUFFER
*
         TM    FILEOF(AE),1        HAVE WE HIT THE EOF?
         BNO   PAFGNBA             NO, GET CHAR AT BEGIN OF BUFFER
         L     R5,RETAGNB
         BR    R5                  MAKE ERROR RETURN
*
PAFGNB0  LA    R15,0(R1,AF)        TEST CHAR AT CURRENT POSITION
         CLI   0(R15),FILBLA       IS IT BLANK ?
         BNE   PAFGNBF             JUMP IF WE HAVE A NON-BLANK
         LA    R1,1(,R1)           OTHERWISE, STEP TO NEXT POSITION
         B     PAFGNBN
*
PAFGNBF  IC    R0,0(R15)           TRANSFER THE CHAR TO
         STC   R0,PFILCOMP(AD)     THE FILE BUFFER
         MVI   FILEOLN(AE),FALSE   CLEAR EOL FLAG
         STH   R1,FILPTR(AE)       RESTORE THE FILE POINTER
         L     R5,RETAGNB
         B     4(R5)               AND MAKE NORMAL RETURN
*
*
*
*
*****************************************************************
*        (PUT-STORE) PUT A CHAR FROM (AD)
*****************************************************************
*
PAFPST   DS    0H
         ST    R5,RETAPST
*
PAFPST1  LH    R1,FILPTR(AE)
         CH    R1,FILEND(AE)       TEST IF BUFFER FULL
         BL    PAFPST2             CLEAR IT FIRST
         BAL   R5,PAFPUT           EMPTY THE BUFFER
         B     PAFPST1
*
PAFPST2  IC    R0,PFILCOMP(AD)
         STC   R0,0(R1,AF)
         LA    R1,1(R1)            INCREMENT CHAR. PTR.
         STH   R1,FILPTR(AE)
         L     R5,RETAPST
         BR    R5
*
*
*
*****************************************************************
*        PUT A RECORD (FROM AF)
*****************************************************************
*
PAFPUT   DS    0H
         AIF   (&SYSTEM).SYS44
         TM    FILOPN(AE),WRITEOPN
         BZ    BADWRITE            IF N.O. FOR WRITE, COMPLAIN
.SYS44   ANOP
*
*        PREPARE THE OUTPUT BUFFER FOR TRANSMITTING TO O.S.
*
         CLI   DCBRECFM,X'80'
         BNL   PAFPUT4             JUMP IF F OR U-FORMAT
         LA    R0,6                MIN LENGTH FOR VBA RECORDS
         TM    DCBRECFM,X'06'
         BNZ   *+6                 JUMP IF CONTROL CHARS
         BCTR  R0,0                ADJUST MIN LENGTH
         LH    R1,FILPTR(AE)
         CR    R1,R0
         BNL   PAFPUT2             JUMP IF RECORD SIZE OK
         LTR   AF,AF               RETURN IF THE I/O BUFFER IS NOT
         BZ    PAFPUT4             ALLOCATED  (FILE NEWLY OPENED)
         LA    R0,FILBLA
         STC   R0,0(R1,AF)         PROVIDE 1 CHAR AT LEAST
         LA    R1,1(R1)
*
**********************************************************************
*        (ibm370 dependency: STCM)
**********************************************************************
*
PAFPUT2  STCM  R1,3,0(AF)          SET RDW FOR V-FORMAT
*
PAFPUT4  DS    0H
         TM    FILOPN(AE),TEXTMOVE
         BZ    PAFPUT5
*
         PUT   FILDCB(AE),0(AF)
         MVC   FILEND(2,AE),DCBLRECL
         B     PAFPUT5A
*
PAFPUT5  DS    0H
         PUT   FILDCB(AE)          WRITE THE RECORD
         ST    R1,FILBUF(AE)       SAVE ADDRESS OF NEXT OUTPUT BUFFER#
         MVC   FILEND(2,AE),DCBLRECL  RESET BUFFER LENGTH
         LR    AF,R1
*
PAFPUT5A DS    0H
         AIF   (&SYSTEM).SYS415
         L     R0,FILLIM(AE)       LOAD LINES LIMIT
         BCT   R0,PAFPUT6          DECREMENT
         ST    R0,FILLIM(AE)       OVERFLOW OF COUNTER
         B     LIMITERR
PAFPUT6  ST    R0,FILLIM(AE)       PUT DECREMENTED COUNT BACK
.SYS415  ANOP
*
*        B     PAFCLR              I.E., NEXT LINE
*
*
*
*****************************************************************
*        CLEAR THE OUTPUT BUFFER TO BLANKS
*****************************************************************
*
PAFCLR   DS    0H
         LH    R1,FILBEG(AE)
         STH   R1,FILPTR(AE)       RESET CHAR. POINTER
         CLI   DCBRECFM,X'80'      JUMP IF F OR U-FORMAT
         BNL   PAFCLR1
         TM    DCBRECFM,X'06'
         BZR   R5                  RETURN IF NO CONTROL CHAR NEEDED
         MVI   4(AF),FILBLA        SET DEFAULT CONTROL CHAR
         TM    DCBRECFM,X'04'
         BOR   R5                  RETURN IF ASCII CONTROL CHARS
         MVI   4(AF),X'09'         REPLACE WITH MACHINE CHAR
         BR    R5
PAFCLR1  DS    0H
         LH    R15,FILEND(AE)      LOAD BUFFER LENGTH
*
**********************************************************************
*        (ibm370 dependency: MVCL)
**********************************************************************
*
         LR    R14,AF
         LA    R1,FILBLA
         SLL   R1,24               SET FILL CHARACTER
         MVCL  R14,R0              CLEAR THE I/O BUFFER
*
         TM    DCBRECFM,X'02'
         BZR   R5                  RETURN IF NOT MACH CONTROL CHAR
         MVI   0(AF),X'09'
         BR    R5
*
CLRBUF   MVC   1(0,R1),0(R1)       PROPAGATE BLANKS
*
*
*
*****************************************************************
*        check file status
*****************************************************************
*
PAFCHK   DS    0H
         TM    FILOPN(AE),TEXTFLAG
         BZ    PAFCHKE             JUMP IF NON-TEXT FILE
*
         CLI   FILSTA(AE),C'3'
         BE    PAFCHK3             JUMP IF FILE IN R/W STATE
         CLI   FILSTA(AE),C'0'
         BE    PAFCHK2             JUMP IF FILE NEEDS RES/REW
         CLI   FILSTA(AE),C'1'
         BNE   BADRSTA             error on other filsta values
         MVI   FILSTA(AE),C'3'
         B     PAFCHK3             JUMP READY
*
PAFCHK2  DS    0H                  DO IMPLICIT RESET ON FILE
         STM   R1,R5,SAVECHK
*
         L     R15,=A(PAFCLS)
         BALR  R14,R15             GO AND CLOSE THE FILE
*
         L     R15,=A(PAFGOP)
         BALR  R14,R15             open file for read
*
*                                  only in case of textfile there is
*                                  a return from filgop, otherwise
*                                  there is a branch to label get1
*
         L     R14,=A(ABNDCODE)
         CLI   0(R14),C' '
         BNE   PAFCHK4
*
         BAL   R5,PAFGET
*
         LM    R1,R5,SAVECHK
         MVI   FILSTA(AE),C'3'
         B     PAFCHKE
*
PAFCHK3  DS    0H
         CLI   FILRDBS(AE),1       READBUF SCHEDULED ?
         BNE   PAFCHKE             JUMP IF NOT
*
         MVI   FILRDBS(AE),0       switch readbuf schedule off
         STM   R1,R5,SAVECHK       save regs
*
*        MVC   TREOF+18(1),FILEOF(AE)
*        OI    TREOF+18,X'F0'
*        LA    R3,TREOF
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
*        MVC   TREOF2+18(1),FILEOFP(AE)
*        OI    TREOF2+18,X'F0'
*        LA    R3,TREOF2
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
         BAL   R5,PAFGET           NOW PAFGET = READ BUFFER
*
         LM    R1,R5,SAVECHK       restore regs
         B     PAFCHKE
*
PAFCHK4  DS    0H
         L     R15,=A(PAFCLS)
         BALR  R14,R15             CLOSE FILE FOLLOWING OPEN ERROR
         LM    R1,R5,SAVECHK       restore regs
         MVI   FILSTA(AE),C'0'     set file status to 0
*
PAFCHKE  DS    0H
         BR    R5
*
TREOF    DC    CL80'TRACE: EOF-FLAG = X (FILEOF)  - nach READLN'
TREOF2   DC    CL80'TRACE: EOF-FLAG = X (FILEOFP) - nach READLN'
*
*
*
*****************************************************************
*        check file status (write)
*****************************************************************
*
PAFCHKW  DS    0H
         TM    FILOPN(AE),TEXTFLAG
         BZ    PAFCHKWE            JUMP IF NON-TEXT FILE
*
         CLI   FILSTA(AE),C'4'
         BE    PAFCHKWE            JUMP IF FILE IN R/W STATE
         CLI   FILSTA(AE),C'0'
         BE    PAFCHKW2            JUMP IF FILE NEEDS RES/REW
         CLI   FILSTA(AE),C'2'
         BNE   BADWSTA             error on other filsta values
         MVI   FILSTA(AE),C'4'
         B     PAFCHKWE            JUMP READY
*
PAFCHKW2 DS    0H                  DO IMPLICIT REWRITE ON FILE
         STM   R1,R5,SAVECHK
*
         L     R15,=A(PAFCLS)
         BALR  R14,R15             GO AND CLOSE THE FILE
*
         L     R15,=A(PAFPOP)
         BALR  R14,R15             open file for write
*
         L     R14,=A(ABNDCODE)
         CLI   0(R14),C' '
         BNE   PAFCHKW4
*
         LM    R1,R5,SAVECHK
         MVI   FILSTA(AE),C'4'
PAFCHKWE DS    0H
         BR    R5
*
PAFCHKW4 DS    0H
         L     R15,=A(PAFCLS)
         BALR  R14,R15             CLOSE FILE FOLLOWING OPEN ERROR
         LM    R1,R5,SAVECHK       restore regs
         MVI   FILSTA(AE),C'0'     set file status to 0
         BR    R5
*
         LTORG
*
*
*
*****************************************************************
*        RESET AN INPUT FILE
*****************************************************************
*
         DS    0H
         DC    CL4'RES '
RES      DS    0H
         FILADR ,
         CLI   FILSTA(AE),C'1'     if filsta is 1, last call was reset
         BE    RTN                 then no action
*
         L     R15,=A(PAFCLS)
         BALR  R14,R15             GO AND CLOSE THE FILE
*
         L     R15,=A(PAFGOP)
         BALR  R14,R15             open file for read
*
         L     R14,=A(ABNDCODE)
         CLI   0(R14),C' '
         BNE   RES1
*
*                                  only in case of textfile there is
*                                  a return from PAFGOP, otherwise
*                                  there is a branch to label GET1
*
*------- CLI   FILTERM(AE),C'Y'    don't call PAFGET on terminal files
*------- BE    RES3
         BAL   R5,PAFGET
         B     RES2
*
RES3     DS    0H
         MVI   FILRDBS(AE),1       READBUF SCHEDULED := 1
*
RES2     DS    0H
         MVI   FILSTA(AE),C'1'     set file status to 1
         B     RTN                 RETURN TO PASCAL PROGRAM
*
RES1     DS    0H
         L     R15,=A(PAFCLS)
         BALR  R14,R15             CLOSE FILE FOLLOWING OPEN ERROR
         MVI   FILSTA(AE),C'0'     set file status to 0
         B     RTN
*
*
*
*****************************************************************
*        GET AN INPUT RECORD
*****************************************************************
*        changed opp 15.08.2020
*        don't skip first byte after implied reset
*****************************************************************
*
         DS    0H
         DC    CL4'GET'
GET      DS    0H
         FILADR ,
         TM    FILEOF(AE),1
         BO    RTN
         TM    FILOPN(AE),TEXTFLAG
         BZ    GET1                jump if non-text file
*
*****************************************************************
*        GET processing for textfiles
*        reworked 08.2020 - opp
*****************************************************************
*
         STM   R1,R5,SAVEGET       save regs 1 to 5
*
*****************************************************************
*        if FILE status is zero, call PAFCHK to do open etc.
*        no subsequent PAFGFE
*        same goes if Read Buffer has been scheduled due to
*        preceeding EOLN processing
*        otherwise call PAFGFE to advance the buffer pointer
*        after that check for EOLN and schedule Read Buffer
*        for next time
*****************************************************************
*        GET reads next buffer, no matter if Terminal file or not
*****************************************************************
*
         CLI   FILSTA(AE),C'0'     if file not open or
         BE    GET7
         CLI   FILRDBS(AE),1       if Readbuf scheduled
         BE    GET7                go to GET7 ... PAFCHK
*
         BAL   R5,PAFGFE           otherwise: advance one character
         B     GET6
*
GET7     DS    0H
         BAL   R5,PAFCHK           file open and buffer read
*
GET6     DS    0H
         CLI   FILEOLN(AE),TRUE    check if at end of line
         BNE   GET4
         MVI   FILRDBS(AE),1       if so: schedule Readbuf
GET4     DS    0H
         LM    R1,R5,SAVEGET       restore regs 1 to 5
         B     RTN
*
*****************************************************************
*        GET processing for binary files
*        simple
*****************************************************************
*
GET1     DS    0H
         STM   R1,R5,SAVEGET
         BAL   R5,GETM
         LM    R1,R5,SAVEGET
*
         CLI   DCBRECFM,X'80'
         BL    GET2                JUMP IF V-FORMAT
         LH    R1,DCBLRECL
         B     GET3
GET2     LH    R1,0(AF)            LOAD RDW
         SH    R1,=H'4'            SUBTRACT RDW LENGTH
GET3     ST    R1,PFILRSZ(AD)      PUT SIZE BACK FOR PASCAL PROG ?
         CH    R1,FILCSZ(AE)       CHECK THAT INPUT RECORD ISNT
         BH    BADRECRD            TOO LARGE FOR BUFFER
         B     RTN
*
*
*
*****************************************************************
*        EOT (SKIP BLANKS TO END OF TEXT)
*****************************************************************
*
EOT      DS    0H
         FILADR ,
         BAL   R5,PAFCHK           CHECK FILE STATUS
         BAL   R5,PAFGNB
         B     RTN
         B     RTN
*
*
*
*****************************************************************
*        READ-LINE (FOR TEXTFILE ONLY)
*****************************************************************
*
         DS    0H
         DC    CL4'RLN'
RLN      DS    0H
         FILADR ,
*
*****************************************************************
*        SET readbuf_sched to 0, because readln is done anyway
*        otherwise PAFCHK will do an additional pafget,
*        which is plain wrong
*****************************************************************
*
         MVI   FILRDBS(AE),0       READBUF SCHEDULED := 0
         BAL   R5,PAFCHK           CHECK FILE STATUS
*
         CLI   FILTERM(AE),C'Y'    don't filget on terminal files
         BE    RLN2
*
         BAL   R5,PAFGET
         B     RTN
*
RLN2     DS    0H
         LH    R1,FILEND(AE)       set buffer etc. to end of line
         STH   R1,FILPTR(AE)
         MVI   FILEOLN(AE),TRUE
         MVI   PFILCOMP(AD),FILBLA
         MVI   FILRDBS(AE),1       READBUF SCHEDULED := 1
         B     RTN
*
*
*
*
*****************************************************************
*        EOL (SKIP BLANKS TO END OF LINE)
*****************************************************************
*
EOL      DS    0H
         FILADR ,
         BAL   R5,PAFCHK           CHECK FILE STATUS
         TM    FILEOF(AE),1
         BO    RTN                 RETURN IF END OF FILE
         LH    R1,FILPTR(AE)
EOL1     CH    R1,FILEND(AE)
         BNL   EOL3                JUMP IF AT END OF LINE
         LA    R15,0(R1,AF)
         CLI   0(R15),FILBLA
         BNE   EOL2                EXIT LOOP IF A NON-BLANK
         LA    R1,1(,R1)           ADVANCE TO NEXT CHAR.
         B     EOL1
EOL2     IC    R0,0(R15)           LOAD NON-BLANK CHAR.
         STC   R0,PFILCOMP(AD)     PASS TO PASCAL PROG.
         MVI   FILEOLN(AE),FALSE   REDUNDANT?
         STH   R1,FILPTR(AE)
         B     RTN
EOL3     DS    0H
         MVI   FILEOLN(AE),TRUE
         MVI   PFILCOMP(AD),FILBLA
         STH   R1,FILPTR(AE)
         B     RTN
*
*
*
*****************************************************************
*        RDB, (READ BOOLEAN)   ADR(FILE),ADR(BOOLEAN)
*****************************************************************
*
         DS    0H
         DC    CL4'RDB'
RDB      DS    0H
         FILADR ,
         BAL   R5,PAFCHK           CHECK FILE STATUS
         BAL   R5,PAFGNB           ADVANCE TO A NON-BLANK
         B     EOFREAD             ERROR RETURN FOR EOF
         LA    RSLT,TRUE
         OI    PFILCOMP(AD),X'40'  CONVERT TO UPPER CASE (IN CASE !)
         CLI   PFILCOMP(AD),C'T'   T = TRUE ?
         BE    RDB6
         CLI   PFILCOMP(AD),C'F'   F = FALSE ?
         BNE   BADBOOL             NEITHER = AN ERROR
RDB4     SR    RSLT,RSLT
RDB6     STC   RSLT,0(R2)          STORE THE RESULT
         BAL   R5,PAFGFE           STEP TO FOLLOWING CHAR
         B     RTN
*
*
*
*****************************************************************
*        RDC  (READ CHAR),  R2 = ADDR. OF CHAR
*****************************************************************
*
         DS    0H
         DC    CL4'RDC'
RDCOLD   DS    0H
         FILADR ,
*
         BAL   R5,PAFCHK           CHECK FILE STATUS
         IC    R0,PFILCOMP(AD)
         STC   R0,0(R2)
         BAL   R5,PAFGFE
         B     RTN
*
*
*
*
*****************************************************************
*        RDI/RDH/RDY (READ INTEGER)    AD: ADR(FILE),  R2: ADR(INT)
*****************************************************************
*
         DS    0H
         DC    CL4'RDY'
RDY      OI    RDY1+1,X'F0'        OVERWRITE NO-OP
         B     RDI
*
         DS    0H
         DC    CL4'RDH'
RDH      OI    RDH1+1,X'F0'        OVERWRITE NO-OP
         B     RDI
*
*
*
         DS    0H
         DC    CL4'RDI'
RDI      DS    0H
         FILADR ,
*
         BAL   R5,PAFCHK           CHECK FILE STATUS
         LR    R4,R2               CLEAR A REGISTER PAIR  (R2,R3)
         SR    R3,R3
         MVI   RDISIGN,1           ASSUME POSITIVE
         BAL   R5,PAFGNB           ADVANCE TO A NON-BLANK
         B     EOFREAD             ERROR RETURN FOR EOF
         CLI   PFILCOMP(AD),FILPLU
         BE    RDI23
         CLI   PFILCOMP(AD),FILMIN
         BNE   RDI25
         MVI   RDISIGN,0           CHANGE TO NEGATIVE
RDI23    BAL   R5,PAFGFE
RDI25    LA    R15,BADINT          PREPARE ERROR RT ADDRESS
RDI4     DS    0H
*
*        STM   R1,R5,SAVERDI
*        ST    R15,SAVERDI2
*        SR    R0,R0
*        IC    R0,PFILCOMP(AD)     GET DIGIT
*        CVD   R0,DOWO
*        UNPK  TRRDI1+21(9),DOWO+3(5)
*        OI    TRRDI1+29,X'F0'
*        LA    R3,TRRDI1
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*        L     R15,SAVERDI2
*        LM    R1,R5,SAVERDI
*
         SR    R0,R0
         IC    R0,PFILCOMP(AD)     GET DIGIT
         SH    R0,=AL2(FILZER)
         BMR   R15                 QUIT LOOP IF NON-DIGIT
         CH    R0,=AL2(9)
         BHR   R15
         M     R2,=F'10'
         AR    R3,R0
         BAL   R5,PAFGFE
         BAL   R15,RDI4            LOOP BACK & SET EXIT ADDRESS
*
         TM    RDISIGN,1
         BO    *+6
         LCR   R3,R3
RDH1     BC    0,RDH2              JUMP FOR RDH/RDY ROUTINES
         ST    R3,0(R4)
         B     RTN
RDH2     NI    RDH1+1,X'0F'        RESET JUMP BACK TO NO-OP
RDY1     BC    0,RDY2              JUMP FOR RDY ROUTINE
         CH    R3,=H'32767'
         BH    BIGINT
         CH    R3,=H'-32768'
         BL    BIGINT
         STH   R3,0(R4)
         B     RTN
RDY2     NI    RDY1+1,X'0F'        RESET JUMP BACK TO NO-OP
         CL    R3,=F'255'
         BH    BIGINT
         STC   R3,0(R4)
         B     RTN
*
TRRDI1   DC    CL80'TRACE: ZEICHEN RDI = XXXXXXXXX'
RDISIGN  DC    C' '
*
*
*
*****************************************************************
*  RDS, (READ STRING), R2 = STRING ADDRESS, R3 = STRING LENGTH !
*****************************************************************
*
         DS    0H
         DC    CL4'RDS'
RDS      DS    0H
         FILADR ,
*
         BAL   R5,PAFCHK           check file status
*
         CLI   FILEOF(AE),TRUE     if eof set then
         BNE   RDSNOEOF
*
         XR    R14,R14             source address irrelevant
         L     R15,=X'40000000'    source length zero, fill w. blank
         MVCL  R2,R14              set string to blank
         LA    R0,FILBLA           end of line character
         STC   R0,PFILCOMP(AD)     set current char
         B     RDSZ                go to end of function
*
RDSNOEOF DS    0H
         LH    R15,FILEND(AE)
         LH    R14,FILPTR(AE)
         SR    R15,R14             compute number of chars left
         CR    R3,R15              more required ?
         BL    RDSL
         MVC   FILPTR(2,AE),FILEND(AE)  filptr to filend
         MVI   FILEOLN(AE),TRUE         incidate eoln
         B     RDS01
*
RDSL     DS    0H
         LR    R0,R14              R14 = filptr (see above)
         AR    R0,R3               add length (R3) to filptr
         STH   R0,FILPTR(AE)       new position to filptr
*
RDS01    DS    0H
         IC    R0,PFILCOMP(AD)     start with current character
*
**********************************************************************
*        (ibm370 dependency: MVCL)
**********************************************************************
*
         STC   R0,0(R2)            copy chars one by one
         LA    R14,1(R14)          increment source address
         BCTR  R15,0
         LA    R2,1(R2)            increment target address
         BCTR  R3,0
         LTR   R3,R3
         BNP   RDSF3
         LTR   R15,R15
         BNM   RDSF2
         SR    R15,R15
*
RDSF2    DS    0H
         AR    R14,AF              addr of first char
         ICM   R15,8,=AL1(FILBLA)  set fill char
         MVCL  R2,R14              now copy string
*
RDSF3    DS    0H
         LH    R14,FILPTR(AE)
         CLI   FILEOLN(AE),TRUE    eoln ?
         BE    RDSF4
         IC    R0,0(R14,AF)        load next input char
         STC   R0,PFILCOMP(AD)     set current char
         B     RDSZ
*
RDSF4    DS    0H
         LA    R0,FILBLA           end of line character
         STC   R0,PFILCOMP(AD)     set current char
*
*****************************************************************
*        if eoln, do readbuf on next iteration
*****************************************************************
*
         CLI   FILTERM(AE),C'Y'    if not a terminal file
         BE    RDSZ
         MVI   FILRDBS(AE),1       schedule Readbuf for next RDS
*
RDSZ     DS    0H
*
         B     RTN
*
         LTORG
*
*
*
*****************************************************************
*  RDV, (READ STRING), R2 = STRING ADDRESS, R3 = STRING LENGTH !
*****************************************************************
*
         DS    0H
         DC    CL4'RDV'
RDV      DS    0H
         FILADR ,
*
*        STM   R1,R5,SAVERDS
*
*        ST    AF,HEX5
*        UNPK  HEX9,HEX5
*        TR    HEX9(8),TABHEX-C'0'
*        MVC   TRRDV0+36(8),HEX9
*        LA    R3,TRRDV0
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
*        MVC   TRRDV1+20(30),0(R2)
*        LA    R3,TRRDV1
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
*        LM    R1,R5,SAVERDS
*
         BAL   R5,PAFCHK           check file status
*
         CLI   FILEOF(AE),TRUE     if eof set then
         BNE   RDVNOEOF
*
         STH   R3,0(R2)            store R3 to maxlength
         XR    R14,R14
         STH   R14,2(R2)           store zero to actual length
         LA    R0,FILBLA           end of line character
         STC   R0,PFILCOMP(AD)     set current char
         B     RDVZ                go to end of function
*
RDVNOEOF DS    0H
         LH    R15,FILEND(AE)
         LH    R14,FILPTR(AE)
         SR    R15,R14             compute number of chars left
         CR    R3,R15              string has more space ?
         BL    RDVL
         MVC   FILPTR(2,AE),FILEND(AE)  filptr to filend
         MVI   FILEOLN(AE),TRUE         incidate eoln
         B     RDV01
*
RDVL     DS    0H
         LR    R0,R14              R14 = filptr (see above)
         AR    R0,R3               add length (R3) to filptr
         STH   R0,FILPTR(AE)       new position to filptr
         LR    R15,R3              only transfer max. R3 bytes
*
RDV01    DS    0H
         STH   R3,0(R2)            store maxlength to String maxl
         STH   R15,2(R2)           store actlength to String actl
         LA    R2,4(R2)            point R2 to String content
         IC    R0,PFILCOMP(AD)     start with current character
         LTR   R15,R15             if actlen = zero
         BNP   RDV02A              don't copy characters
*
RDV02    DS    0H
         STC   R0,0(R2)            copy chars one by one
         LA    R14,1(R14)          increment source address
         LA    R2,1(R2)            increment target address
         IC    R0,0(R14,AF)        load next input char
         BCT   R15,RDV02
*
RDV02A   DS    0H
         CLI   FILEOLN(AE),TRUE    eoln ?
         BE    RDV03
         STC   R0,PFILCOMP(AD)     set current char
         B     RDVZ
*
RDV03    DS    0H
         LA    R0,FILBLA           end of line character
         STC   R0,PFILCOMP(AD)     set current char
*
*****************************************************************
*        if eoln, do readbuf on next iteration
*****************************************************************
*
         CLI   FILTERM(AE),C'Y'    if not a terminal file
         BE    RDVZ
         MVI   FILRDBS(AE),1       schedule Readbuf for next RDS
*
RDVZ     DS    0H
*
*        LM    R1,R5,SAVERDS
*        MVC   TRRDV2+20(30),0(R2)
*        LA    R3,TRRDV2
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
         B     RTN
*
TRRDV0   DC    CL80'TRACE: START  RDV FILEBUFFER (AF) = XXXXXXXX'
*                   0....5....1....5....2....5....3....5
TRRDV1   DC    CL80'TRACE: START  RDV = XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
TRRDV2   DC    CL80'TRACE: RETURN RDV = XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
         DS    0F
HEX5     DS    CL5
HEX9     DS    CL9
*
         LTORG
*
*
*
*****************************************************************
*        RFC (READ CHAR FROM FILE)
*        R2 = ADDR. OF CHAR
*        R3 = field width (-1 = default)
*****************************************************************
*        change 27.08.2020:
*        ignore target addr in R2
*        return result (char) in R3, because the compiler
*        expects it there. The compiler generates CHK instruction
*        and then STO C to store the result into target
*****************************************************************
*
         DS    0H
         DC    CL4'RFC'
RFC      DS    0H
         FILADR ,
*
         LTR   R3,R3               check field width
         BZ    RFC01               if zero read nothing
         BP    RFC02               if positive use it as is
         LA    R3,1                otherwise set to 1 (default)
*
RFC02    DS    0H
         BAL   R5,PAFCHK           check file status
*
         XR    R4,R4               set R4 to zero (for temp result)
         IC    R4,PFILCOMP(AD)     insert char from file buffer
*
RFC03    DS    0H
         BAL   R5,PAFGFE           get next character
         CLI   FILEOLN(AE),TRUE    if eoln leave loop
         BE    RFCXX
         BCT   R3,RFC03            repeat get until r3 = zero
         B     RFCXX               and quit
*
RFC01    DS    0H
         LA    R3,FILBLA           store blank into target reg
         B     RTNR3               and quit
*
RFCXX    DS    0H
         LR    R3,R4               result into register R3
         B     RTNR3               and quit
*
*
*
*****************************************************************
*        RDC (READ CHAR FROM FILE)
*        same as RFC to support RFC change ... 25.08.2020
*        R2 = ADDR. OF CHAR
*        R3 = field width (-1 = default)
*****************************************************************
*
         DS    0H
         DC    CL4'RDC'
RDC      DS    0H
         FILADR ,
*
         LTR   R3,R3               check field width
         BZ    RDC01               if zero read nothing
         BP    RDC02               if positive use it as is
         LA    R3,1                otherwise set to 1 (default)
*
RDC02    DS    0H
         BAL   R5,PAFCHK           check file status
*
         IC    R0,PFILCOMP(AD)     insert char from file buffer
         STC   R0,0(R2)            store into target
*
RDC03    DS    0H
         BAL   R5,PAFGFE           get next character
         CLI   FILEOLN(AE),TRUE    if eoln leave loop
         BE    RTN
         BCT   R3,RDC03            repeat get until r3 = zero
*
         B     RTN                 and quit
*
RDC01    DS    0H
         LA    R0,FILBLA           store blank
         STC   R0,0(R2)            into target
         B     RTN                 and quit
*
*
*
*****************************************************************
*        RFS (READ char array - fixed size)
*        R2 = address of char array
*        R3 = length of char array
*        R4 = field width (-1 = default)
*****************************************************************
*
         DS    0H
         DC    CL4'RFS'
RFS      DS    0H
         FILADR ,
*
         LTR   R4,R4               check field width
         BZ    RFS01               if zero read nothing
         BP    RFS02               if positive use it as is
         LR    R4,R3               otherwise set to field length (R3)
*
RFS02    DS    0H
         BAL   R5,PAFCHK           check file status
*
         CLI   FILEOF(AE),TRUE     if eof set then
         BNE   RFSNOEOF
*
         LA    R0,FILBLA           end of line character
         STC   R0,PFILCOMP(AD)     set current char
*
RFS01    DS    0H
         XR    R14,R14             source address irrelevant
         L     R15,=X'40000000'    source length zero, fill w. blank
         MVCL  R2,R14              set string to blank
         B     RFSZ                go to end of function
*
RFSNOEOF DS    0H
         LH    R15,FILEND(AE)
         LH    R14,FILPTR(AE)
         SR    R15,R14             compute number of chars left
         CR    R15,R4              enough chars left (w.r. field width)
         BH    RFSH
         MVC   FILPTR(2,AE),FILEND(AE)  filptr to filend
         MVI   FILEOLN(AE),TRUE         incidate eoln
         B     RFSL1
*
RFSH     DS    0H
         LR    R0,R14              R14 = filptr (see above)
         AR    R0,R4               add field width (R4) to filptr
         STH   R0,FILPTR(AE)       new position to filptr
*
RFSL1    DS    0H
         CR    R15,R4              if length of sender > field width
         BNH   RFSL2               then
         LR    R15,R4              set length of sender to field width
*
RFSL2    DS    0H
         IC    R0,PFILCOMP(AD)     start with current character
*
**********************************************************************
*        (ibm370 dependency: MVCL)
**********************************************************************
*
         STC   R0,0(R2)            copy first char from file buffer
         LA    R14,1(R14)          increment source address
         BCTR  R15,0               reduce length of sender
         LA    R2,1(R2)            increment target address
         BCTR  R3,0                reduce length of target
         LTR   R3,R3               if no more bytes to transfer
         BNP   RFSF3               we're done
         LTR   R15,R15             check length of sender
         BNM   RFSF2               if negative
         SR    R15,R15             set to zero
*
RFSF2    DS    0H
         AR    R14,AF              addr of first char
         ICM   R15,8,=AL1(FILBLA)  set fill char
         MVCL  R2,R14              now copy (rest of string)
*
RFSF3    DS    0H
         LH    R14,FILPTR(AE)
         CLI   FILEOLN(AE),TRUE    eoln ?
         BE    RFSF4
         IC    R0,0(R14,AF)        load next input char
         STC   R0,PFILCOMP(AD)     set current char
         B     RFSZ
*
RFSF4    DS    0H
         LA    R0,FILBLA           end of line character
         STC   R0,PFILCOMP(AD)     set current char
*
*****************************************************************
*        if eoln, do readbuf on next iteration
*****************************************************************
*
         CLI   FILTERM(AE),C'Y'    if not a terminal file
         BE    RFSZ
         MVI   FILRDBS(AE),1       schedule Readbuf for next RFS
*
RFSZ     DS    0H
         B     RTN
*
         LTORG
*
*
*
*****************************************************************
*        RFV (READ varchar a.k.a. string)
*        R2 = address of string
*        R3 = maxlength of string
*        R4 = field width (-1 = default)
*****************************************************************
*
         DS    0H
         DC    CL4'RFV'
RFV      DS    0H
         FILADR ,
*
         LTR   R4,R4               check field width
         BZ    RFV01               if zero read nothing
         BP    RFV02               if positive use it as is
         LR    R4,R3               otherwise set to field length (R3)
*
RFV02    DS    0H
*
         BAL   R5,PAFCHK           check file status
*
         CLI   FILEOF(AE),TRUE     if eof set then
         BNE   RFVNOEOF
*
         LA    R0,FILBLA           end of line character
         STC   R0,PFILCOMP(AD)     set current char
*
RFV01    DS    0H
         STH   R3,0(R2)            store R3 to maxlength
         XR    R14,R14
         STH   R14,2(R2)           store zero to actual length
         B     RFVZ                go to end of function
*
RFVNOEOF DS    0H
         LH    R15,FILEND(AE)
         LH    R14,FILPTR(AE)
         SR    R15,R14             compute number of chars left
         CR    R15,R4              enough chars left (w.r. field width)
         BH    RFVH
         MVC   FILPTR(2,AE),FILEND(AE)  filptr to filend
         MVI   FILEOLN(AE),TRUE         incidate eoln
         B     RFVL1
*
RFVH     DS    0H
         LR    R0,R14              R14 = filptr (see above)
         AR    R0,R4               add field width (R4) to filptr
         STH   R0,FILPTR(AE)       new position to filptr
         LR    R15,R4              only transfer max. R4 bytes
         CR    R15,R3
         BNH   RFVL1
         LR    R15,R3              but not more than maxlength
*
RFVL1    DS    0H
         STH   R3,0(R2)            store maxlength to String maxl
         STH   R15,2(R2)           store actlength to String actl
         LA    R2,4(R2)            point R2 to String content
         IC    R0,PFILCOMP(AD)     start with current character
         LTR   R15,R15             if actlen = zero
         BNP   RFVL2A              don't copy characters
*
RFVL2    DS    0H
         STC   R0,0(R2)            copy chars one by one
         LA    R14,1(R14)          increment source address
         LA    R2,1(R2)            increment target address
         IC    R0,0(R14,AF)        load next input char
         BCT   R15,RFVL2
*
RFVL2A   DS    0H
         CLI   FILEOLN(AE),TRUE    eoln ?
         BE    RFVL3
         STC   R0,PFILCOMP(AD)     set current char
         B     RFVZ
*
RFVL3    DS    0H
         LA    R0,FILBLA           end of line character
         STC   R0,PFILCOMP(AD)     set current char
*
*****************************************************************
*        if eoln, do readbuf on next iteration
*****************************************************************
*
         CLI   FILTERM(AE),C'Y'    if not a terminal file
         BE    RFVZ
         MVI   FILRDBS(AE),1       schedule Readbuf for next RFS
*
RFVZ     DS    0H
         B     RTN
*
         LTORG
*
*
*
*****************************************************************
*  REW (REWRITE = REWIND FOR OUTPUT)
*****************************************************************
*
         DS    0H
         DC    CL4'REW'
REW      DS    0H
         FILADR ,
*
         CLI   FILSTA(AE),C'2'     if filsta is 2, last call w. rewrite
         BE    RTN                 then no action
*
         L     R15,=A(PAFCLS)
         BALR  R14,R15             go and close the file
*
         L     R15,=A(PAFPOP)
         BALR  R14,R15             open file for write
*
         L     R14,=A(ABNDCODE)
         CLI   0(R14),C' '
         BE    REW1
         L     R15,=A(PAFCLS)
         BALR  R14,R15             CLOSE FILE FOLLOWING OPEN ERROR
         MVI   FILSTA(AE),C'0'     set file status to 0
         B     RTN
*
REW1     DS    0H
         TM    FILOPN(AE),TEXTFLAG
         BZ    RTN                 return if not a text file
         MVI   FILSTA(AE),C'2'     set file status to 2
         B     RTN
*
*
*
*
*****************************************************************
*        PUT (PASCAL 'PUT' OPERATION)
*****************************************************************
*
         DS    0H
         DC    CL4'PUT '
PUT      DS    0H
         FILADR ,
*
*************************************************************
*        03.10.2017 - versuchsweise test auf eof auskomm.
*************************************************************
*
*----    TM    FILEOF(AE),1
*----    BZ    RTN
*
         TM    FILOPN(AE),TEXTFLAG
         BZ    PUT1                JUMP IF NOT A TEXT-FILE
         BAL   R5,PAFCHKW          CHECK FILE STATUS (WRITE)
         BAL   R5,PAFPST
         B     RTN
PUT1     LH    R1,FILRSZ(AE)       GET CORRECT RECORD SIZE
         SLL   R1,16               CONSTRUCT A RDW
         ST    R1,PFILRSZ(AD)      - IN CASE FILE HAS V-FORMAT
         PUT   FILDCB(AE),(AF)     WRITE THE RECORD
         AIF   (&SYSTEM).SYS425
         L     R0,FILLIM(AE)
         BCT   R0,PUT2             UPDATE OUTPUT LIMIT
         ST    R0,FILLIM(AE)       - LIMIT EXCEEDED
         B     LIMITERR
PUT2     ST    R0,FILLIM(AE)       PUT UPDATED COUNT BACK
.SYS425  ANOP
         B     RTN
*
*
*
*****************************************************************
*        WLN (WRITE LINE), TERMINATE CURRENT OUTPUT LINE
*        18.08.2016: VERSUCH, SOFORT ZU SCHREIBEN
*****************************************************************
*
         DS    0H
         DC    CL4'WLN '
WLN      DS    0H
         FILADR ,
         BAL   R5,PAFCHKW          CHECK FILE STATUS (WRITE)
*
         TM    FILOPN(AE),TEXTMOVE
         BZ    WLN1
*
         BAL   R5,PAFPUT
         B     RTN
*
WLN1     DS    0H
         LH    R1,FILPTR(AE)
         CH    R1,FILEND(AE)       IF BUFFER IS MARKED "OVER-FULL",
         BNH   WLN2                WE WRITE IT OUT
         BAL   R5,PAFPUT
         LH    R1,FILPTR(AE)
WLN2     DS    0H
         BCTR  R1,0
         STH   R1,FILEND(AE)       FLAG BUFFER AS "OVER-FULL"
*
         B     RTN
*
*
*
*
*****************************************************************
*        WRB (WRITE BOOLEAN)
*        R1 = ADR(FILE)
*        R2 = BOOLEAN
*        R3 = FIELD_WIDTH (see comment on WRS)
*****************************************************************
*
         DC    CL4'WRB'
WRB      LTR   R2,R2
         LA    R2,=C'TRUE'
         LA    R4,4
         BNZ   WRS                 JUMP IF "TRUE"
WRB1     LA    R2,=C'FALSE'
         LA    R4,5
         B     WRS
*
*
*
*
*****************************************************************
*        WRV (WRITE VARCHAR)
*        R1 = ADR(FILE)
*        R2 = VARCHAR
*        R3 = FIELD_WIDTH (see comment on WRS)
*****************************************************************
*
         DC    CL4'WRV'
WRV      DS    0H
         LH    R4,0(R2)
         LTR   R4,R4               load direct / indirect indicator
         BM    WRV2
*
         LH    R4,2(R2)            load varchar length
         LA    R2,4(R2)            set ptr to varchar content
         LTR   R3,R3               if field width nonzero
         BNZ   WRS                 continue with WRS
         LR    R3,R4               else set field width to length
         B     WRS                 continue with WRS
*
WRV2     DS    0H
         LH    R4,2(R2)            load varchar length
         L     R2,4(R2)            set ptr to varchar content
         LTR   R3,R3               if field width nonzero
         BNZ   WRS                 continue with WRS
         LR    R3,R4               else set field width to length
         B     WRS                 continue with WRS
*
*
*
*****************************************************************
*        WRP (WRITE pointer)
*        R1 = ADR(FILE)
*        R2 = Pointer
*        R3 = FIELD_WIDTH
*****************************************************************
*
         DC    CL4'WRP'
WRP      DS    0H
         ST    R2,PINT             Print Pointer as Hex String
         UNPK  ZINT(9),PINT(5)         Convert
         TR    ZINT(8),TABHEX-C'0'     to Hex
         LA    R2,ZINT
         LA    R4,8                string length = 8
         LTR   R3,R3               if given length = 0
         BNZ   WRP1                then
         LA    R3,8                set default length to 8
WRP1     DS    0H
         CR    R3,R4               if length > 8, all is ok
         BNL   WRS
         SR    R4,R3               otherwise inc address in R2
         AR    R2,R4               so that rightmost part is printed
         LR    R4,R3               adjust R4
         B     WRS
*
*
*
*****************************************************************
*        WRX (WRITE scalar)
*        R1 = ADR(FILE)
*        R2 = Scalar value
*        R3 = Length value
*        R4 = Pointer to Scalar String Vektor
*****************************************************************
*
         DC    CL4'WRX'
WRX      DS    0H
         ST    R2,PINT             Store R2
         LTR   R2,R2               if Scalar value negative
         BM    WRXERR              branch to WRXERR
         ST    R3,PINT+4           Store R3
         LH    R2,0(R4)            Load offset of highest Scalar
         LTR   R2,R2               if zero, new layout of meta info
         BNZ   WRX1
         LH    R2,2(R4)            this is max scalar value plus 1
         BCTR  R2,0                max Scalar value
         C     R2,PINT             compare max value with parm
         BL    WRXERR              branch to WRXERR
*
         S     R2,PINT             value is ok, subtract parm
         SLA   R2,2                multiply by 4, = offset of entry
         LH    R3,10(R2,R4)        length of string
         LH    R2,8(R2,R4)         offset of string
         AR    R2,R4               address of string
         LR    R4,R3               copy to R4
         L     R3,PINT+4           restore R3
         B     WRXC
*
WRX1     DS    0H
         SRA   R2,2                divide by 4, = max scalar plus 1
         BCTR  R2,0                max Scalar value
         C     R2,PINT             compare max value with parm
         BL    WRXERR              branch to WRXERR
*
         S     R2,PINT             value is ok, subtract parm
         SLA   R2,2                multiply by 4, = offset of entry
         LH    R3,2(R2,R4)         length of string
         LH    R2,0(R2,R4)         offset of string
         AR    R2,R4               address of string
         LR    R4,R3               copy to R4
         L     R3,PINT+4           restore R3
*
WRXC     DS    0H
         B     WRS
*
WRXERR   DS    0H
         L     R2,PINT             Print Pointer as Hex String
         UNPK  ZINT(9),PINT(5)         Convert
         TR    ZINT(8),TABHEX-C'0'     to Hex
         LA    R2,ZINT
         LA    R4,8                string length = 8
         MVC   ZINT(4),=C'WRX:'
         B     WRXC
*
*
*
*****************************************************************
*        WRC
*        R1 = ADR(FILE)
*        R2 = Char
*        R3 = FIELD_WIDTH
*****************************************************************
*
         DS    0H
         DC    CL4'WRC '
WRC      DS    0H
         FILADR ,
         BAL   R5,PAFCHKW          CHECK FILE STATUS (WRITE)
         BCTR  R3,0
         LTR   R3,R3
         BZ    WRC3
         BM    RTN
WRC2     MVI   PFILCOMP(AD),FILBLA
         BAL   R5,PAFPST
         BCT   R3,WRC2
WRC3     STC   R2,PFILCOMP(AD)
         BAL   R5,PAFPST
         B     RTN
*
*
*
*****************************************************************
*        WRS
*        R1 = ADR(FILE)
*        R2 = Char
*        R3 = FIELD_WIDTH (if negative, adjust left)
*        R4 = String length (may be longer than R3, or shorter)
*****************************************************************
*
         DS    0H
         DC    CL4'WRS '
WRS      DS    0H
         FILADR ,
*
         BAL   R5,PAFCHKW          CHECK FILE STATUS (WRITE)
         LTR   R3,R3
         BZ    RTN                 ZERO FIELD WIDTH, RETURN
*
*****************************************************************
*        IF R3 > R4, WRITE BLANKS BEFORE STRING CONTENT
*        THAT IS: WRITE STRING CONTENT RIGHT JUSTIFIED
*        OTHERWISE WRITE STRING LEFT JUSTIFIED
*****************************************************************
*
         BP    WRSC1
         B     WRSC2
*
WRSC1    DS    0H
         CR    R3,R4
         BH    WRSC11
         LR    R4,R3
*
WRSC11   DS    0H
         SR    R3,R4
         B     WRSC3
*
WRSC2    DS    0H
         LCR   R3,R3
         CR    R3,R4
         BH    WRSC21
         LR    R4,R3
*
WRSC21   DS    0H
         SR    R3,R4
         LCR   R3,R3
*
WRSC3    DS    0H
         LH    R1,FILPTR(AE)
         LA    R0,FILBLA           FILL CHARACTER
*
*****************************************************************
*        write blanks before string content, if needed
*****************************************************************
*
         LTR   R3,R3               Blanks needed before string content?
         BNP   WRSB2               NO NEED FOR FILL CHARS
*
WRSB1    DS    0H
         CH    R1,FILEND(AE)
         BL    WRSF1
         STH   R1,FILPTR(AE)       UPDATE LENGTH FIRST
         BAL   R5,PAFPUT           THEN EMPTY THE FULL BUFFER
         LH    R1,FILPTR(AE)
         LA    R0,FILBLA
WRSF1    STC   R0,0(R1,AF)         PLANT NEXT BLANK IN BUFFER
         LA    R1,1(R1)            AND ADVANCE TO NEXT POSITION
         BCT   R3,WRSB1
*
*****************************************************************
*        Now write string content
*****************************************************************
*
WRSB2    DS    0H
         CH    R1,FILEND(AE)
         BL    WRSF3
         STH   R1,FILPTR(AE)       UPDATE LENGTH FIELD
         BAL   R5,PAFPUT           THEN EMPTY THE FULL BUFFER
         LH    R1,FILPTR(AE)
WRSF3    IC    R0,0(R2)            PICK UP NEXT CHAR IN STRING
         STC   R0,0(R1,AF)         AND PLANT IT IN BUFFER
         LA    R2,1(R2)            ADVANCE IN THE STRING
         LA    R1,1(R1)            ADVANCE IN THE BUFFER
         BCT   R4,WRSB2
*
*****************************************************************
*        write blanks after string content, if needed
*****************************************************************
*
         LTR   R3,R3               Blanks needed after string content?
         BZ    WRSF5               NO NEED FOR FILL CHARS
         LCR   R3,R3               negate R3 content
         LA    R0,FILBLA
*
WRSB3    DS    0H
         CH    R1,FILEND(AE)
         BL    WRSF4
         STH   R1,FILPTR(AE)       UPDATE LENGTH FIRST
         BAL   R5,PAFPUT           THEN EMPTY THE FULL BUFFER
         LH    R1,FILPTR(AE)
         LA    R0,FILBLA
WRSF4    STC   R0,0(R1,AF)         PLANT NEXT BLANK IN BUFFER
         LA    R1,1(R1)            AND ADVANCE TO NEXT POSITION
         BCT   R3,WRSB3
*
WRSF5    DS    0H
         STH   R1,FILPTR(AE)
         STC   R0,PFILCOMP(AD)
         B     RTN
*
*
*
*****************************************************************
*        WRI
*
*        09.2017: allow negative width to print leading zeroes
*
*        R1 = ADR(FILE)
*        R2 = Integer
*        R3 = FIELD_WIDTH (if negative, fill with leading zeros)
*****************************************************************
*
         DC    CL4'WRI '
WRI      DS    0H
         LTR   R3,R3
         BNM   WRI2
*
********************************************************
*        if width negative, change to positive
*        do normal ed and put sign in first position
*        works only for lengths up to 12
********************************************************
*        this is only part of the solution,
*        for widths lower than -12 there has to be done
*        some additional work
********************************************************
*
         LPR   R3,R3
         LR    R4,R3
         CH    R4,=H'12'
         BNH   *+8
         LA    R4,12
         CVD   R2,PINT
         MVC   ZINT,=X'402120202020202020202020'
         ED    ZINT,PINT+2
         MVC   ZINT(2),=C'00'
         LA    R1,ZINT+12
         SR    R1,R4
         LTR   R2,R2
         BNM   *+8
         MVI   0(R1),C'-'
         LR    R2,R1
         B     WRI3
*
********************************************************
*        if width positive, do edmk and put sign
*        before first nonzero position. because
*        integers are limited to 10 digits, this is ok
*        with a 12 byte buffer (ZINT).
*        WRS writes the string right justified
*        with blanks added to the left
********************************************************
*
WRI2     DS    0H
         CVD   R2,PINT
         LA    R1,ZINT+11
         MVC   ZINT,=X'402020202020202020202120'
         EDMK  ZINT,PINT+2
         LTR   R2,R2
         BNM   *+10
         BCTR  R1,0
         MVI   0(R1),C'-'
         LR    R2,R1
*
WRI3     DS    0H
         LA    R4,ZINT+L'ZINT
         SR    R4,R1
         CR    R4,R3
         BNH   WRS                 PRINT INTEGER AS A STRING
         LR    R3,R4               - BUT INCREASE FIELD WIDTH IF
         B     WRS                 NECESSARY TO PRINT ENTIRE NUMBER
*
*
*
*****************************************************************
*        WRR
*
*        R1 = ADR(FILE)
*        FP2 = Real
*        R3 = FIELD_WIDTH (if negative, fill with leading zeros)
*        R4 = Scale (if negative, E-Notation)
*
*        Implementation in WRRSUB
*****************************************************************
*
         DS    0H
         DC    CL4'WRR'
WRR      DS    0H
         FILADR ,
         L     R15,=A(WRRSUB)
         BALR  R14,R15
         B     RTN
*
*
*
*****************************************************************
*        RDR, READ THE NEXT (REAL) NUMBER INTO (GRG2)
*****************************************************************
*
         DS    0H
         DC    CL4'RDR'
RDR      DS    0H
         FILADR ,
         BAL   R5,PAFCHK           CHECK FILE STATUS
         SDR   0,0
         LA    R3,1
         BAL   R5,PAFGNB           ADVANCE TO A NON-BLANK
         CLI   PFILCOMP(AD),FILPLU
         BE    RDR3
         CLI   PFILCOMP(AD),FILMIN
         BNE   RDR4
         LCR   R3,R3
RDR3     BAL   R5,PAFGFE
RDR4     LA    R15,BADREAL         SET ERROR EXIT ADDRESS
RDR45    SR    R1,R1
         IC    R1,PFILCOMP(AD)            GET CHARACTER
         SH    R1,=AL2(FILZER)
         BMR   R15                 QUIT LOOP IF NON-DIGIT
         CH    R1,=AL2(9)
         BHR   R15
         MD    0,=D'10.0'
         SLA   R1,3
         AD    0,DECTBL(R1)
         BAL   R5,PAFGFE
         BAL   R15,RDR45           LOOP BACK & SET EXIT ADDRESS
RDR5     SR    R4,R4
         CLI   PFILCOMP(AD),FILDOT
         BNE   RDR7
         BAL   R5,PAFGFE
RDR6     CLI   PFILCOMP(AD),FILZER
         BL    RDR7
         CLI   PFILCOMP(AD),FILNIN
         BH    RDR7
         MD    0,=D'10.0'
         SR    R1,R1
         IC    R1,PFILCOMP(AD)
         SH    R1,=AL2(FILZER)
         SLA   R1,3
         AD    0,DECTBL(R1)
         BAL   R5,PAFGFE
         BCTR  R4,0
         B     RDR6
RDR7     LTR   R3,R3
         BNM   *+6
         LCDR  0,0
         CLI   PFILCOMP(AD),C'E'
         BE    RDR75
         CLI   PFILCOMP(AD),C'e'
         BNE   RDR13
RDR75    LA    R3,1
RDR8     BAL   R5,PAFGFE
         CLI   PFILCOMP(AD),FILBLA
         BE    RDR8
         ST    R4,RDREXPO
         SR    R4,R4
RDR9     CLI   PFILCOMP(AD),FILPLU
         BE    RDR10
         CLI   PFILCOMP(AD),FILMIN
         BNE   RDR11
         LCR   R3,R3
RDR10    BAL   R5,PAFGFE
RDR11    LA    R15,BADREAL         SET ERROR EXIT ADDRESS
RDR115   SR    R1,R1
         IC    R1,PFILCOMP(AD)     GET CHARACTER
         SH    R1,=AL2(FILZER)
         BMR   R15                 QUIT LOOP IF NON-DIGIT
         CH    R1,=AL2(9)
         BHR   R15
         MH    R4,=H'10'
         AR    R4,R1
         BAL   R5,PAFGFE
         BAL   R15,RDR115          LOOP BACK & SET EXIT ADDRESS
RDR12    LTR   R3,R3
         BNM   *+6
         LCR   R4,R4
         A     R4,RDREXPO
RDR13    LTR   R4,R4
         BZ    RDR16
         BP    RDR14
         LPR   R4,R4
         B     RDR15
RDR14    MD    0,=D'10.0'
         BCT   R4,RDR14
         B     RDR16
RDR15    DD    0,=D'10.0'
         BCT   R4,RDR15
RDR16    STD   0,0(R2)             PUT THE RESULT IN PASCAL VARIABLE
         B     RTN
*
RDREXPO  DC    F'0'
*
*
*
*****************************************************************
*        PAG   SKIP TO NEW PAGE
*****************************************************************
*
         DC    CL4'PAG'
PAG      SR    R2,R2
         BCT   R2,SKP1             SET R2 = -1, THEN USE SKP ROUTINE
*
*
*
*****************************************************************
*        SKP - SKIP R2 INPUT OR OUTPUT LINES
*****************************************************************
*
         DS    0H
         DC    CL4'SKP'
SKP      LTR   R2,R2
         BM    RTN
SKP1     DS    0H
         FILADR ,
         BAL   R5,PAFCHKW          CHECK FILE STATUS (WRITE)
         TM    FILOPN(AE),TEXTFLAG
         BZ    SKP9                JUMP IF NOT A TEXT FILE
         TM    FILOPN(AE),READOPN
         BO    SKP7                JUMP IF AN INPUT FILE
         LA    R0,MAXSKIP
         CR    R2,R0
         BL    *+6                 LIMIT NUMBER OF LINES TO "MAXSKIP"#
         LR    R2,R0
         TM    DCBRECFM,X'06'      JUMP IF FILE DOES NOT HAVE
         BZ    SKP6                CARRIAGE CONTROL CHARS
         LA    R3,3                MAX NO. OF SKIPS PER RECORD
         TM    DCBRECFM,X'04'
         BNZ   SKP2                JUMP IF ANS CONTROL CHARS
         LTR   AF,AF               FORCE AN INITIAL PUT OPERATION IF
         BNZ   SKP3                  NO I/O BUFFER YET ALLOCATED
SKP2     BAL   R5,PAFPUT           CLEAR LINE BUFFER
SKP3     CR    R2,R3
         LR    R4,R2
         BNH   *+6
         LR    R4,R3               NOW, R4 = MIN( R2, 3 )
         SR    R1,R1
         CLI   DCBRECFM,X'80'
         BNL   *+8
         LA    R1,4
*
         TM    DCBRECFM,X'04'
         BZ    SKP5                JUMP IF MACHINE CHARS
         IC    R0,SKPASCII(R4)
         STC   R0,0(R1,AF)         SET ASCII CONTROL CHAR
         SR    R2,R4
         BP    SKP2                REPEAT IF MORE LINES TO SKIP
         LA    R1,1(R1)
         STH   R1,FILPTR(AE)       POINT AT FIRST DATA BYTE
         B     RTN
SKP5     IC    R0,SKPMACH(R4)
         STC   R0,0(R1,AF)         SET MACHINE CONTROL CHAR
         BAL   R5,PAFPUT           AND SEND TO OUTPUT DEVICE
         SR    R2,R4
         BP    SKP3                REPEAT IF MORE LINES TO SKIP
         B     RTN
SKP6     BAL   R5,PAFPUT           NO CONTROL CHARS
         BCTR  R2,0                BUT GIVE AN APPROPRIATE NUMBER
         LTR   R2,R2               OF EMPTY OUTPUT RECORDS
         BP    SKP6
         B     RTN
SKP7     LTR   R2,R2               REJECT PAGE() ON AN INPUT FILE
         BM    BADWRITE
         BNZ   SKP8                JUMP IF > 0 SKIPS
         LH    R1,FILBEG(AE)
         STH   R1,FILPTR(AE)       RESET CHAR PTR. TO REREAD LINE
         IC    R0,0(R1,AF)
         STC   R0,PFILCOMP(AD)     RESET CURRENT FILE ELEMENT
         B     RTN
SKP8     BAL   R5,PAFGET
         BCT   R2,SKP8             STEP THROUGH THE LINES
         B     RTN
*
SKP9     LTR   R2,R2
         BNP   RTN                 RETURN IF SKIP CNT NON-POS.
         TM    FILOPN(AE),READOPN
         BNZ   SKP11               JUMP IF AN INPUT FILE
         LH    R1,FILRSZ(AE)
         SLL   R1,16               CONSTRUCT A RDW
         ST    R1,PFILRSZ(AD)      - IN CASE OF A V-FORMAT FILE
SKP10    PUT   FILDCB(AE),(AF)     WRITE A RECORD
         BCT   R2,SKP10            REPEAT
         B     RTN
*
SKP11    DS    0H
         STM   R1,R5,SAVEGET
         BAL   R5,GETM
         LM    R1,R5,SAVEGET
*
         LH    R1,DCBLRECL         GET RECORD LENGTH
         CLI   DCBRECFM,X'80'
         BNL   SKP12               JUMP IF F OR U-FORMAT
         LH    R1,0(AF)            GET RECORD LENGTH FROM RDW
         SH    R1,=H'4'
SKP12    CH    R1,FILCSZ(AE)       CHECK FOR OVERFLOW
         BH    BADRECRD
         BCT   R2,SKP11
         ST    R1,PFILRSZ(AD)      SET CURRENT LENGTH
         B     RTN
*
*
*
*****************************************************************
*        GET MOVE MODE
*****************************************************************
*
GETM     DS    0H
         GET   FILDCB(AE),(AF)
         BR    R5
*
*
*
*****************************************************************
*        GET LOCATE MODE
*****************************************************************
*
GETL     DS    0H
*
         MVI   RRFLAG,0            CLEAR REREAD FLAG
*
         GET   FILDCB(AE)
*
         CLI   RRFLAG,0            TEST IF REREAD ASKED FOR
         BNE   GETL                IF SO, GO BACK AND TRY AGAIN
*
         ST    R1,FILBUF(AE)       STORE ADDRESS OF INPUT RECORD
         LR    AF,R1
*
         LH    R1,FILBEG(AE)
         STH   R1,FILPTR(AE)       RESET CHAR. PTR
         IC    R0,0(R1,AF)
         STC   R0,PFILCOMP(AD)     UPDATE CURRENT FILE ELEMENT
         MVI   FILEOLN(AE),FALSE   CLEAR EOL FLAG
         MVC   FILEND(2,AE),DCBLRECL    RESET BUFFER LENGTH
*
         BR    R5
*
EODTERM  DS    0H
         LA    R1,BUF0
         ST    R1,FILBUF(AE)       STORE ADDRESS OF DUMMY RECORD
         LR    AF,R1
*
         LH    R1,FILBEG(AE)
         STH   R1,FILPTR(AE)       RESET CHAR. PTR
         IC    R0,0(R1,AF)
         STC   R0,PFILCOMP(AD)     UPDATE CURRENT FILE ELEMENT
         MVI   FILEOLN(AE),TRUE    SET EOL FLAG
         MVC   FILEND(2,AE),=H'4'
         BR    R5
*
BUF0     DC    F'0'
         DC    CL4' '
*
*
*
*****************************************************************
*        'END OF DATA' EXIT, THIS ROUTINE IS ENTERED WHENEVER
*        A 'GET' IS ISSUED FOR A FILE WHICH HAS REACHED THE
*        'END OF FILE' MARK. (I.E. NO MORE INPUT)
*****************************************************************
*
EOD      DS    0H
*
         STM   R1,R5,SAVEEOD
         LA    R3,TREOD
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
         MVC   TREOD2+23(1),FILTERM(AE)
         LA    R3,TREOD2
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
         LM    R1,R5,SAVEEOD
*
         CLI   FILTERM(AE),C'Y'    simulate success for
         BE    EODTERM             terminal file
*
         LA    AG,FILDCB(AE)
         CLOSE ((AG))
         TM    DCBBUFCB+3,1
         BO    EOD1                SKIP IF NO BUFFER POOL
*
         FREEPOOL (AG)
*
*
EOD1     OI    FILEOF(AE),1
         MVI   FILEOFP(AE),TRUE        SET EOF FLAG IN PASCAL
         NI    FILOPN(AE),TEXTFLAG     CLEAR THE OPENFLAGS
         BZ    EOD2                    JUMP IF NOT A TEXTFILE
         MVI   FILEOLN(AE),TRUE        SET EOL FLAG
         MVI   PFILCOMP(AD),FILBLA
         SR    AF,AF               INDICATE NO I/O BUFFER
         STH   AF,FILBEG(AE)       RESET VARIOUS POINTERS
         STH   AF,FILEND(AE)
         STH   AF,FILPTR(AE)
         ST    AF,FILBUF(AE)
*
         LM    R1,R5,SAVEGET       return to caller of
         BR    R5                  GETM/GETL Caller !!!
*
EOD2     SR    R0,R0
         ST    R0,PFILRSZ(AD)      INDICATE ZERO LENGTH RECORD
         B     RTN                 GO BACK TO CALLER
*
TREOD    DC    CL80'TRACE: BRANCH TO EOD'
TREOD2   DC    CL80'TRACE: TERM-Schalter = X'
*
*
*
*****************************************************************
*        CLOCK FUNCTION
*****************************************************************
*
         DC    CL4'CLK'
CLK      LR    R3,R0               SAVE PARAMETER
         TTIMER ,
         S     R0,CLOCK            SUBTRACT START-UP TIME
         LPR   R0,R0               MAKE POSITIVE
         LTR   R3,R3               TEST CLOCK PARAMETER
         BP    RTN                 >0 MEANS RAW TIMER UNITS
         LPR   R1,R0
         M     R0,=FS32'0.02604166'
         B     RTN                 RETURNS THE RESULT IN R0
*
*
*
*****************************************************************
*        MESSAGE PROCEDURE   R2=STRING ADDR,  R3=LENGTH
*****************************************************************
*
         DC    CL4'MSG'
MSG      LTR   R3,R3
         BNP   RTN
         LA    R0,L'MSGBUF-8       SET MAX. MESSAGE LENGTH
         CR    R3,R0
         BNH   *+6
         LR    R3,R0
         EX    R3,MSGMVC
         LA    R3,4(R3)            CONSTRUCT VARIABLE-LENGTH RECORD
         STH   R3,MSGBUF
         LA    R3,MSGBUF(R3)
         MVC   0(4,R3),=X'00000020'    SET THE ROUTING CODE
         MVC   MSGBUF+2(2),=X'8000'
         LA    R1,MSGBUF
         SVC   35                  ISSUE WTO REQUEST
         B     RTN
*
*
*
*****************************************************************
*        CTR, TO ALLOCATE AND CLEAR 'R2' RUN TIME 'COUNTERS'
*****************************************************************
*
         DC    CL4'CTR'
CTR      L     R3,HEAPLIM
         USING DYN2STOR,R3
         ST    R2,DYNRUNC          SET THE # OF COUNTERS
         DROP  R3
         SLA   R2,2                CONVERT COUNT TO BYTES
         LR    R4,R3               POINT TO HEAP END
         SR    R4,R2               LOWER IT TO ITS NEW POSITION
         ST    R4,HEAPLIM          AND SAVE THE NEW HEAP LIMIT PTR
*
         SR    R5,R5               ASSUME NO OS PARAMETER AREA
         L     R0,OSPRMPTR
         LTR   R0,R0               SEE IF THAT IS THE CASE
         BM    CTR10
         LR    R5,R3               IF NOT, FIND THE LENGTH OF
         SR    R5,R0               THE OS PARM AREA
         SR    R3,R5               AND ADJUST 'TO' AND 'FROM' PTRS
         SR    R4,R5
         ST    R4,OSPRMPTR
CTR10    LA    R5,DYNCOUNT-DYN2STOR(R5)   ADD THE SIZE OF FIXED AREA
*
*        AN EXTRA BYTE IS MOVED BUT IT IS OK.
*
         EX    R5,CTRMVE           AND COPY FROM THE OLD AREA.
         ST    R4,NEWPTR           UPDATE HEAP PTR
         AR    R4,R5               POINT BACK TO THE HEAP END
         SRA   R2,2                CONVERT BACK BYTES TO WORDS
         SR    R0,R0
         ST    R0,0(R4)            AND CLEAR THE COUNTER AREA
         LA    R4,4(R4)            ADJUST THE POINTER
         BCT   R2,*-8              REPEAT AS NEEDED
         B     RTN
*
CTRMVE   MVC   0(0,R4),0(R3)
*
*
*
*****************************************************************
*        LINELIMIT PROCEDURE   AD=FILE,  R2=LIMIT
*****************************************************************
*
         DS    0H
         DC    CL4'LIM'
LIM      DS    0H
         FILADR ,
         ST    R2,FILLIM(AE)       SET THE NEW LIMIT
         B     RTN
*
*
*
*****************************************************************
*        DEFINE A FILE,  R2: NAME,  R3: FILE COMP. SIZE
*****************************************************************
*
         DC    CL4'FDF'
FDF      DS    0H
*
         GETMAIN EC,LV=L'#FILREC,A=(AD)
*
         LTR   R15,R15             TEST IF STORAGE AVAILABLE
         BNZ   BADFDF
         L     AE,0(,AD)
         L     R1,=A(#FILREC)      TEMPLATE FOR TEXT FILES
         LTR   R3,R3               R3=0 => A TEXT FILE
         BZ    *+8                 JUMP IF A TEXT FILE
         L     R1,=A(#NONTXT)      TEMPLATE FOR NON-TEXT FILES
         MVC   0(L'#FILREC,AE),0(R1)  COPY TO GETMAINED AREA
         STH   R3,FILCSZ(AE)       SAVE FILE COMP. SIZE
         ST    AD,FILPAS(AE)       SET BACK LINK
         L     R1,FILLIST
         ST    R1,FILLNK(AE)       LINK NEW FILE BLOCK
         ST    AE,FILLIST          INTO THE CHAIN
         OI    FILLIST,X'80'       FLAG AS NOT BUILT-IN
         MVC   FILNAM(8,AE),0(R2)
         MVC   DCBDDNAM,0(R2)      MOVE THE FILE NAME TO ITS DCB FIELD
         B     RTN
*
*
*
*****************************************************************
*        REQUEST FOR CONTROL TRACE OUTPUT
*****************************************************************
*
TRA      EQU   *
         AIF   (&SYSTEM).SYS920
         LTR   R2,R2
         BP    *+6
         SR    R2,R2               KILL OUTPUT REQUEST
         LA    R2,1(,R2)
         L     R15,=A(TRLINES)
         ST    R2,0(,R15)
.SYS920  ANOP
         B     RTN
*
*
*
*
*****************************************************************
*        CLOSE: CLOSE A FILE
*****************************************************************
*
         DS    0H
         DC    CL4'CLS'
CLS      DS    0H
         FILADR ,
*
         L     R15,=A(PAFCLS)
         BALR  R14,R15             GO AND CLOSE THE FILE
*
         B     RTN
*
*
*
*
*****************************************************************
*        DAT: GET DATE FROM SYSTEM
*****************************************************************
*
         DC    CL4'DAT'
DAT      DS    0H
*
         TIME  DEC                 GET TOD IN TU
*
         L     R2,=A(OLDDATE)
         C     R1,0(R2)            COMP. ORIGINAL DATE
         BE    RTN                 ONLY CONVERT,
         ST    R1,0(R2)            IF DATE CHANGED
*
         L     R15,=A(FIXDATE)
         BALR  R5,R15              CONVERT DATE
*
         B     RTN
*
*
*
*
*****************************************************************
*        TIM: GET TIME FROM SYSTEM
*****************************************************************
*
         DC    CL4'TIM'
TIM      DS    0H
*
         TIME  DEC                 GET TOD IN TU
*
*        FIX TIME OF DAY STRING
*
         L     R15,=A(FIXTIME)
         BALR  R5,R15              CONVERT TIME
*
         B     RTN
*
*
*
*
*****************************************************************
*        EXIT : TO EXIT USER PROGRAM WITH A RETURN CODE
*****************************************************************
*
         DC    CL4'XIT'
XIT      DS    0H
         ST    R2,SAVERETC         SAVE RETURN CODE
*
         C     R2,=A(1000)         IF ERROR GO TO CLOSE/EXIT ROUTINE
         BNL   CLSALL
*
         L     R4,HEAPLIM
         USING DYN2STOR,R4
         L     R5,DYNRUNC
         LTR   R5,R5               SEE IF RUN TIME COUNTERS ARE PRESENT
         BNP   CLSALL              IF NOT, QUIT.
*
         LA    AD,QRR              PICK THE COUNTER FILE NAME
         LA    R1,PREW
         L     R15,=A($PASCSP2)
         BALR  R14,R15             CALL THE $PASCSP ROUTINES
*
*        THIS CAN BE DONE ONLY IF WE KNOW THAT WE DO NOT NEED
*        TO GO BACK TO THE PROGRAM WHICH CALLED THE EXIT
*        ROUTINE.
*
         LA    R1,PWRI             SET THE FUNCTION CODE
         LA    R3,10               SET THE FIELD_WIDTH
NXTCNT   L     R2,DYNCOUNT         PICK THE NEXT COUNT VALUE
         BALR  R14,R15             OUTPUT IT
         LA    R4,4(R4)
         BCT   R5,NXTCNT           REPEAT UNTIL DONE
         DROP  R4
*
         LA    R1,PWLN
         BALR  R14,R15             OUTPUT THE LAST LINE
         MVI   PROFFLAG,X'FF'      SET FLAG TO INDICATE PASPROF CALL
         B     CLSALL              GO TO COMPLETE EXIT EPILOGUE.
*
*****************************************************************
*        CLOSE ALL THE FILES
*****************************************************************
*
CLSALL   DS    0H
*        LA    R3,TRCLSALL
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
         L     AE,FILLIST
*
CLSALL1  DS    0H
         L     AF,FILBUF(AE)
*
         L     R15,=A(PAFCLS)
         BALR  R14,R15             GO AND CLOSE THE FILE
*
         L     R3,FILLNK(AE)       GET POINTER TO NEXT BLOCK
         LTR   AE,AE               TEST IF BUILT-IN FILE
         BNM   CLSALL2             IF SO, JUMP
*
         FREEMAIN R,LV=L'#FILREC,A=(AE)  RELEASE FILE BLOCK STORAGE
*
CLSALL2  DS    0H
         LTR   AE,R3               FOLLOW CHAIN OF FILE BLOCKS
         BNZ   CLSALL1             REPEAT
*
*        LA    R3,TRCLSEND
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
*****************************************************************
*        TIME TO EXIT
*****************************************************************
*
         L     R2,SAVERETC         RESTORE RETURN CODE
         L     R1,=A(OLDPICA)
         TM    3(R1),1             TEST IF SPIE WAS ISSUED
         BO    SKIPSPIE            JUMP IF NOT
         L     1,0(R1)             RESET OLD PICA ADDRESS
         SPIE  MF=(E,(1))          TO CANCEL ANY PASCAL SPIE
*
SKIPSPIE DS    0H
*
*****************************************************************
*        CHECK FOR ABEND
*****************************************************************
*
         C     R2,=A(1000)         IF RETURN CODE < 1000
         BL    XIT2                  THEN DON'T ABEND
*
         L     R1,=A(DUMPFLAG)
         CLI   0(R1),X'FF'         TEST IF O.S. DUMP REQUESTED
         BNE   XIT2
*
         LA    R1,X'FFF'
         NR    R1,R2
         O     R1,=A(X'80000000')
         SVC   13
*
XIT2     DS    0H
         C     R2,=A(TIMERR)       SEE IF THIS IS A TIME OUT EXIT
         BE    XIT3
*
         C     R2,=A(TIMERR+SNPERR)
         BNE   XIT4
*
*****************************************************************
*        THIS ABEND IS BECAUSE THERE IS NO CLEAN AND EASY WAY
*        TO TERMINATE THE PROGRAM IN CASE OF A TIMER INTERRUPT !
*****************************************************************
*
XIT3     ABEND (R2)
*
XIT4     DS    0H
*        LA    R3,TRXIT4
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
         XR    R3,R3
         BCTR  R3,0
         L     R15,=A(PTRACE)
         BALR  R14,R15
*
*****************************************************************
*        after this point the save area should not be used
*        any more (no ptrace output, for example)
*****************************************************************
*
         LR    R13,R12             POINT TO STACK BASE
         S     R13,=A(STACK-DYNSTORE)
         L     R15,=V(IBCOM#)      IS FORTRAN PRESENT ?
         LTR   R15,R15
         BZ    XIT6
*
*****************************************************************
*        NOTE: THE IBCOME 'EXIT' ROUTINE USES THE SAVE AREA PASSED
*        TO THE IBCOM INIT ROUTINE. THE FOLLOWING PATCH SHOULD
*        ENSURE RETURNING TO 'XIT6' AFTER CLOSING FORTRAN I/O AND
*        CANCELING IBCOM 'SPIE' AND 'STAE'.
*****************************************************************
*
         LA    R14,XIT6            IF SO, SET THE RETURN ADDRESS
         STM   R14,R12,12(R13)     SET 'RESTORE' AREA FOR THE CALL
         BAL   R14,IBCOMXIT(R15)   TO IBCOM# XIT ROUTINE.
         DC    AL2(0)              RETURN CODE FOR ABOVE CALL
*
XIT6     DS    0H
         L     R13,4(R13)          POINT TO THE SYSTEM SAVE AREA
         FREEMAIN V,A=ALOSTORE     RELEASE STACK STORAGE
         TM    PROFFLAG,X'FF'
         BNZ   CALLPROF            JUMP IF PASPROF TO BE INVOKED
*
         LR    R15,R2              SET THE RETURN CODE
         L     R14,12(R13)         PICK THE RETURN ADDRESS
         LM    R0,R12,20(R13)      RESTORE OTHER REGS
         BR    14
*
CALLPROF DS    0H
         L     R1,=A(OSPARMS)      GIVE PASPROF SAME PARAMETERS
         L     R1,0(,R1)           AS WE WERE GIVEN
         L     R14,12(,R13)        SET JUST R14 (XCTL SETS REST)
         XCTL  (2,12),EP=PASPROF   INVOKE THE PROFILER MODULE
*
*
TRCLSALL DC    CL80'TRACE: START CLSALL SCHLEIFE'
TRCLSEND DC    CL80'TRACE: ENDE CLSALL SCHLEIFE'
TRXIT4   DC    CL80'TRACE: AT LABEL XIT4'
*
*
*
*
*****************************************************************
*        UNDEFINED CSP ENTRIES
*****************************************************************
*
TRP      DS    0H
SIO      DS    0H
EIO      DS    0H
EOF      DS    0H
ELN      DS    0H
RDD      DS    0H
WRD      DS    0H
WRE      DS    0H
XXX      DS    0H
         B     UNDFERR             REPORT AN ERROR
*
*
*
*
*****************************************************************
*         RETURN TO PASCAL PROGRAM
*****************************************************************
*
RTNR2    DS    0H
         C     R13,=A(CSPSTCK1)    R13 in Pascal stack ?
         BNE   RTNEWR2             yes, new return
         ST    R2,SPUSERSA+4
*
RTN      DS    0H
         C     R13,=A(CSPSTCK1)    R13 in Pascal stack ?
         BNE   RTNEW               yes, new return
         MVI   SCHCSP,X'00'        CLEAR CSP FLAG
         LM    R1,R15,SPUSERSA     rslt reg SHOULD NOT BE RESTORED !
         BR    14
*
RTNEWR2  DS    0H
         L     R1,4(R13)           nur einmal zurueck
         ST    R2,28(R1)           R2 abspeichern, siehe unten LM
*
RTNEW    DS    0H
         L     R13,4(R13)          zunaechst nur einmal zurueck
         MVI   SCHCSP,X'00'        CLEAR CSP FLAG
         LM    R14,R15,12(R13)
         LM    R1,R12,24(R13)
         L     R13,4(R13)          und jetzt nochmal zurueck
         BR    14
*
RTNR3    DS    0H
         L     R13,4(R13)          zunaechst nur einmal zurueck
         MVI   SCHCSP,X'00'        CLEAR CSP FLAG
         LM    R14,R15,12(R13)
         LM    R1,R2,24(R13)
         LM    R4,R12,36(R13)
         L     R13,4(R13)          und jetzt nochmal zurueck
         BR    14
*
         LTORG ,
*
*
*****************************************************************
*        drop all base regs
*****************************************************************
*
         DROP  ,
*
*
*
*
**************************************************************
*
*        END OF "NORMAL" $PASCSP MODULE
*
**************************************************************
*
*
*
*
*
**************************************************************
*
*        $PASCSP EXTENSION -
*
*        Procedures moved "outside"
*        that have their own base registers
*
**************************************************************
*
         USING CSPBASE,R10
         USING IHADCB-FILDCB,AE
*
*****************************************************************
*        GENERAL ROUTINE TO CLOSE A FILE
*****************************************************************
*
PAFCLS   DS    0H
*
         USING PAFCLS,R15
*
         ST    R14,SAVECLS
         ST    R11,SAVECLS+4
         LR    R11,R15
*
         DROP  R15
         USING PAFCLS,R11
*
         TM    FILOPN(AE),READOPN+WRITEOPN
         BZ    PAFCLSZ             RETURN IF FILE NOT OPEN
*
         MVI   FILSTA(AE),C'0'     File-Status auf Null zurueck
*
         TM    FILOPN(AE),TEXTFLAG+WRITEOPN
         BNO   PAFCLS4             JUMP IF NONTEXT OR INPUT FILE
*
         TM    FILOPN(AE),TEXTMOVE
         BNO   PAFCLSL
*
*****************************************************************
*        letzte zeile ausgeben bei move mode - falls noetig
*****************************************************************
*
         LH    R1,FILPTR(AE)       nur ausgeben, wenn was da ist
         CH    R1,FILBEG(AE)
         BNH   PAFCLSM
*
         BAL   R5,PAFPUT           letzte Zeile ausgeben
*
*****************************************************************
*        speicher fuer buffer freigeben bei move mode
*****************************************************************
*
PAFCLSM  DS    0H
         LH    R3,DCBLRECL
         FREEMAIN R,LV=(R3),A=FILBUF(AE)
         B     PAFCLS4
*
*****************************************************************
*        PREPARE THE OUTPUT BUFFER FOR TRANSMITTING TO O.S.
*        bei locate mode
*****************************************************************
*
PAFCLSL  DS    0H
         LH    R1,FILPTR(AE)       IF BUFFER IS IN "OVER-FULL" STATE
         CH    R1,FILEND(AE)       WE MUST WRITE A RECORD NOW -
         BNH   PAFCLSL1            wir schreiben es jetzt im
         BAL   R5,PAFPUT           zusammenhang mit close
*
PAFCLSL1 LH    R1,FILPTR(AE)
         CLI   DCBRECFM,X'80'
         BNL   PAFCLS4             RETURN IF F OR U-FORMAT
         LA    R0,6                MIN LENGTH FOR VBA RECORDS
         TM    DCBRECFM,X'06'
         BNZ   *+6                 JUMP IF CONTROL CHARS
         BCTR  R0,0                ADJUST MIN LENGTH
         CR    R1,R0
         BNL   PAFCLSL2            JUMP IF RECORD SIZE OK
         LTR   AF,AF               RETURN IF THE I/O BUFFER IS NOT
         BZ    PAFCLS4             ALLOCATED  (FILE NEWLY OPENED)
         LA    R0,FILBLA
         STC   R0,0(R1,AF)         PROVIDE 1 CHAR AT LEAST
         LA    R1,1(R1)
*
**********************************************************************
*        (ibm370 dependendy: STCM)
**********************************************************************
*
PAFCLSL2 STCM  R1,3,0(AF)          SET RDW FOR V-FORMAT
*
PAFCLS4  DS    0H
         LA    AG,FILDCB(AE)
         CLOSE ((AG))
*
         NI    FILOPN(AE),X'FF'-READOPN-WRITEOPN   CLEAR OPEN FLAGS
*
         MVC   FILMEM(8,AE),=CL8' '    CLEAR MEMBER NAME
*
         TM    DCBBUFCB+3,1        TEST IF BUFFER POOL THERE
         BO    PAFCLSZ             RETURN IF NOT
*
         FREEPOOL (AG)
*
PAFCLSZ  DS    0H
         L     R11,SAVECLS+4
         L     R14,SAVECLS
         BR    R14
*
*
*
*
*****************************************************************
*        OUTPUT OF FLOATING POINT VALUES
*
*        R1 = ADR(FILE)
*        R2 = Real
*        R3 = FIELD_WIDTH (if negative, fill with leading zeros)
*        R4 = Scale (if negative, E-Notation)
*****************************************************************
*
WRRSUB   DS    0H
*
         USING WRRSUB,R15
*
         ST    R14,SAVEWRR
         ST    R11,SAVEWRR+4
         LR    R11,R15
*
         DROP  R15
         USING WRRSUB,R11
*
         SR    R2,R2
         STC   R2,SIGN             SIGN := 0
         STC   R2,EFORM            EFORM := 0
         MVI   LEADZ,C' '          LEADZ := ' '
*
         LTR   R3,R3
         BZ    WRRSUBZ
         BP    WRR10               IF R3 < 0
         MVI   LEADZ,C'0'          LEADZ := '0'
         LCR   R3,R3               R3 := - R3
WRR10    DS    0H
*
         BAL   R5,PAFCHKW          CHECK FILE STATUS (WRITE)
*
         SR    R2,R2               Exponent := 0
         SDR   FPR0,FPR0           set FP0 to zero
         ADR   FPR2,FPR0           FORCE NORMALIZATION
         LDR   FPR0,FPR2           FP0 := FP2 (ORIGINAL VALUE)
         BNZ   WRR20
*
************************************************************
*        PRINT 'ZERO' IN THE PROPER FORMAT
************************************************************
*
         LTR   R4,R4               write zero in 'F' FORMAT ?
         BNM   WRR40
*
**************************************************
*        E-FORMAT ZERO
**************************************************
*
         MVC   PFILCOMP(1,AD),LEADZ    write 2 blanks (or zeros)
         BAL   R5,PAFPST
         BAL   R5,PAFPST
         LR    R4,R3
         SH    R4,=H'8'                at least 9 positions needed
         MVI   PFILCOMP(AD),FILZER     write 0
         BAL   R5,PAFPST
         MVI   PFILCOMP(AD),FILDOT     write .
         BAL   R5,PAFPST
         MVI   PFILCOMP(AD),FILZER     write 0
         BAL   R5,PAFPST
*
**************************************************
*        PRINT additional zeros after decimal point
**************************************************
*
         LTR   R4,R4
         BNP   WRR12
         MVI   PFILCOMP(AD),FILZER
         BALR  R5,0                    SET RETURN ADDRESS FOR PAFPST
         BCT   R4,PAFPST               CALL REPEATEDLY
*
WRR12    DS    0H
         MVI   PFILCOMP(AD),FILEXP     write E
         BAL   R5,PAFPST
         MVI   PFILCOMP(AD),FILPLU     write +
         BAL   R5,PAFPST
         MVI   PFILCOMP(AD),FILZER     write 00
         BAL   R5,PAFPST
         BAL   R5,PAFPST
         B     WRRSUBZ
*
************************************************************
*        PRINT A NON_ZERO REAL NUMBER
************************************************************
*
WRR20    DS    0H
         BNM   WRR24
         MVI   SIGN,X'01'          NEGATIVE VALUE, REMEMBER SIGN
         BCTR  R3,0                AND ADJUST FIELD WIDTH
         LPDR  FPR2,FPR2
         LDR   FPR0,FPR2           SAVE THE ORIGINAL VALUE
*
WRR24    DS    0H
         CD    FPR2,=D'1.0'        PUT FPR2 INTO RANGE 1.0 to 10
         BNL   WRR26               AND COUNT EXPONENT
         MD    FPR2,=D'10.0'
         BCT   R2,WRR24            expo := expo - 1
*
WRR26    DS    0H
         CD    FPR2,=D'10.0'
         BL    WRR30
         DD    FPR2,=D'10.0'
         AH    R2,=H'1'            expo := expo + 1
         B     WRR26
*
**************************************************
*        NOW WE HAVE  "1.0 <= FPR2 < 10"
*        AND "FPR0 = FPR2 * (10 ** R2)"
**************************************************
*
WRR30    DS    0H
         LTR   R4,R4               SEE IF F_FORMAT REQUESTED
         BNM   WRR40
*
************************************************************
*        PRINT THE VALUE IN E_FORM
************************************************************
*
         LD    FPR0,=D'1.0E-13'    wert ein bisschen hoch ...
         MDR   FPR0,FPR2
         ADR   FPR2,FPR0
*
         CD    FPR2,=D'10.0'       nochmal korrigieren ...
         BL    WRR31
         DD    FPR2,=D'10.0'
         AH    R2,=H'1'
*
WRR31    DS    0H
*
         MVI   EFORM,X'01'         SET E_FORMAT FLAG
         ST    R2,EXPONENT         SAVE EXPONENT VALUE
         SR    R2,R2               ONLY ONE DIGIT BEFORE DEC. P.
         LR    R4,R3
         SH    R4,=H'7'            subtract used positions
         BP    WRR50
         LA    R4,1
         B     WRR50
*
*
************************************************************
*        F_FORMAT OUTPUT
************************************************************
*
WRR40    DS    0H
*
         LTR   R2,R2
         BNM   WRR44               SEE IF NEGATIVE EXPONENT
*
         LDR   FPR2,FPR0           IF SO, USE THE ORIGINAL VALUE
         SR    R2,R2               ONLY ONE DIGIT BEFORE DEC. P.
*
WRR44    DS    0H
*
********************************************************
*        2017 - bernd oppolzer
*        apply roundx function at the desired position
********************************************************
*
         C     R13,=A(CSPSTCK1)    R13 in Pascal stack ?
         BNE   WRR43
*
         L     R15,=A($CSPERR)
         BALR  R14,R15
*
WRR43    DS    0H
         LA    R1,72(R13)          save area position for roundx
         LA    R5,1                function number, roundx = 1
         ST    R5,112(R1)
         LNR   R5,R4               roundx 2nd argument, negative scale
         SR    R5,R2               adjust according to exponent
         ST    R5,116(R1)
         STD   2,120(R1)           value to be rounded
         L     R15,=V($PASMAT)     call $pasmat
         BALR  R14,R15
         LD    2,72(R1)            retrieve rounded value
*
         LD    FPR0,=D'1.0E-13'    wert ein bisschen hoch
         MDR   FPR0,FPR2
         ADR   FPR2,FPR0
*
*        ST    R3,SAVEWRR3
*
*        CVD   R2,DOWOWRR
*        UNPK  TRWRRS2+22(9),DOWOWRR+3(5)
*        OI    TRWRRS2+30,X'F0'
*        LA    R3,TRWRRS2
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
*        L     R3,SAVEWRR3
*        CVD   R3,DOWOWRR
*        UNPK  TRWRRS3+22(9),DOWOWRR+3(5)
*        OI    TRWRRS3+30,X'F0'
*        LA    R3,TRWRRS3
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
*        CVD   R4,DOWOWRR
*        UNPK  TRWRRS4+22(9),DOWOWRR+3(5)
*        OI    TRWRRS4+30,X'F0'
*        LA    R3,TRWRRS4
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
*        L     R3,SAVEWRR3
*
         BCTR  R3,0                eine stelle weg bei width
*
         LTR   R4,R4               IF SCALE = 0, NO DEC. POINT
         BZ    WRR45
         SR    R3,R4               COMPUTE THE # OF LEADING BLANKS
         BCTR  R3,0
*
WRR45    DS    0H
         SR    R3,R2
         BNP   WRR50
*
WRR46    DS    0H
         MVC   PFILCOMP(1,AD),LEADZ
         BALR  R5,0                    SET RETURN ADDRESS FOR PAFPST
         BCT   R3,PAFPST               CALL REPEATEDLY
*
**************************************************
*        R2 # OF DIGITS BEFORE DECIMAL POINT
*        R4 # OF DIGITS FOLLOWING DECIMAL POINT
*        IF R4 = ZERO, NO DECIMAL POINT
**************************************************
*
WRR50    DS    0H
         TM    SIGN,X'01'
         BZ    WRR51
         CLI   LEADZ,C'0'
         BNE   WRR50A
         MVI   PFILCOMP(AD),FILMIN
         BAL   R5,PAFPST
         MVC   PFILCOMP(1,AD),LEADZ
         BAL   R5,PAFPST
         B     WRR52
*
WRR50A   DS    0H
         MVC   PFILCOMP(1,AD),LEADZ
         BAL   R5,PAFPST
         MVI   PFILCOMP(AD),FILMIN
         BAL   R5,PAFPST
         B     WRR52
*
WRR51    DS    0H
         MVC   PFILCOMP(1,AD),LEADZ
         BAL   R5,PAFPST
*
WRR52    DS    0H
         LA    R2,2(R2)
         BALR  R3,0                SET RETURN ADDRESS FOR WRRDGTS
         BCT   R2,WRRDGTS          CALL REPEATEDLY
         TM    EFORM,X'01'
         BNZ   WRR53
         LTR   R4,R4
         BZ    WRRSUBZ
*
WRR53    DS    0H
         MVI   PFILCOMP(AD),FILDOT
         BAL   R5,PAFPST
         BAL   R3,WRRDGTS          SET RETURN ADDRESS FOR WRRDGTS
         BCT   R4,WRRDGTS          CALL REPEATEDLY
         TM    EFORM,X'01'
         BZ    WRRSUBZ
*
**************************************************
*        PRINT THE EXPONENT FOR E_FORMAT OUTPUT
**************************************************
*
         L     R4,EXPONENT
         MVI   PFILCOMP(AD),FILEXP
         BAL   R5,PAFPST
         MVI   PFILCOMP(AD),FILPLU
         LTR   R4,R4
         BNM   *+8
         MVI   PFILCOMP(AD),FILMIN
         BAL   R5,PAFPST
         LPR   R3,R4
         SR    R2,R2
         D     R2,=F'10'
         LA    R3,FILZER(R3)
         STC   R3,PFILCOMP(AD)
         BAL   R5,PAFPST
         LA    R2,FILZER(R2)
         STC   R2,PFILCOMP(AD)
         BAL   R5,PAFPST
*
WRRSUBZ  DS    0H
         L     R11,SAVEWRR+4
         L     R14,SAVEWRR
         BR    R14
*
**************************************************
*        local variables of wrrsub
**************************************************
*
LEADZ    DC    C' '
SIGN     DC    C' '
EFORM    DC    C' '
EXPONENT DC    F'0'
*
SAVEWRR3 DS    F
DOWOWRR  DS    D
TRWRRS2  DC    CL80'TRACE: WRRSUB REG 2 = XXXXXXXXX'
TRWRRS3  DC    CL80'TRACE: WRRSUB REG 3 = XXXXXXXXX'
TRWRRS4  DC    CL80'TRACE: WRRSUB REG 4 = XXXXXXXXX'
*
         LTORG
*
*
*
*
*******************************************************************
*        THIS ROUTINE PRINTS THE NEXT DIGIT OF THE VALUE
*        IN FPR2, IT USES R3 FOR ITS RETURN ADDRESS
*        IT IS ASSUMED THAT "0 <= FPR2 < 10" AND FPR2
*        IS UPDATED FOR THE NEXT DIGIT
*******************************************************************
*
WRRDGTS  DS    0H
         CD    FPR2,=D'8.0'
         BNL   WRRD8
         CD    FPR2,=D'4.0'
         BNL   WRRD4
*
         CD    FPR2,=D'2.0'
         BNL   WRRD2
         CD    FPR2,=D'1.0'
         LD    FPR0,=D'10.0'
         LA    R1,1
         BNL   WRROK
         LD    FPR0,=D'0.0'
         LA    R1,0
         B     WRROK
*
WRRD2    DS    0H
         CD    FPR2,=D'3.0'
         LD    FPR0,=D'30.0'
         LA    R1,3
         BNL   WRROK
         LD    FPR0,=D'20.0'
         LA    R1,2
         B     WRROK
*
WRRD4    DS    0H
         CD    FPR2,=D'6.0'
         BNL   WRRD6
         CD    FPR2,=D'5.0'
         LD    FPR0,=D'50.0'
         LA    R1,5
         BNL   WRROK
         LD    FPR0,=D'40.0'
         LA    R1,4
         B     WRROK
*
WRRD6    DS    0H
         CD    FPR2,=D'7.0'
         LD    FPR0,=D'70.0'
         LA    R1,7
         BNL   WRROK
         LD    FPR0,=D'60.0'
         LA    R1,6
         B     WRROK
*
WRRD8    DS    0H
         CD    FPR2,=D'9.0'
         LD    FPR0,=D'90.0'
         LA    R1,9
         BNL   WRROK
         LD    FPR0,=D'80.0'
         LA    R1,8
         B     WRROK
*
WRROK    DS    0H
         LA    R1,FILZER(R1)
         STC   R1,PFILCOMP(AD)
         BAL   R5,PAFPST           WRITE NEXT DIGIT
         MD    FPR2,=D'10.0'
         SDR   FPR2,FPR0
         BR    R3                  RETURN TO CALLER
*
         LTORG
*
*
*
*
*****************************************************************
*        Subroutine to open a file (for input)
*****************************************************************
*
PAFGOP   DS    0H
*
         USING PAFGOP,R15
*
         ST    R14,SAVEGOP
         ST    R11,SAVEGOP+4
         LR    R11,R15
*
         DROP  R15
         USING PAFGOP,R11
*
         NI    FILOPN(AE),TEXTFLAG     CLEAR OPEN FLAGS
*
         L     R14,=A(OPENMODE)
         MVI   0(R14),C'I'
*
         L     R15,=A(PAFOPN)
         BALR  R14,R15             call open for read
*
*        MVI   TROPENR+12,C'I'
*        MVC   TROPENR+14(8),FILNAM(AE)
*        MVC   TROPENR+32(8),FILMEM(AE)
*        L     R14,=A(ABNDCODE)
*        MVC   TROPENR+41(1),0(R14)
*        LA    R3,TROPENR
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
         L     R14,=A(ABNDCODE)
         CLI   0(R14),C' '
         BNE   PAFGOPZ
*
         TM    DCBOFLGS,X'10'
         BNO   BADREAD             exit, if wrong file type
*
         L     AF,FILBUF(AE)
         MVI   FILEOF(AE),0        CLEAR BOTH EOF FLAGS
         MVI   FILEOFP(AE),FALSE
         OI    FILOPN(AE),READOPN
         TM    FILOPN(AE),TEXTFLAG
         BNZ   PAFGOP2
*
         L     R14,=A(GET1)        no return to caller
         ST    R14,SAVEGOP         branch to GET1 instead
         B     PAFGOPZ
*
PAFGOP2  DS    0H
         MVI   FILEOLN(AE),TRUE
         SR    R0,R0
         STH   R0,FILBEG(AE)
         LH    R1,DCBLRECL
         TM    DCBRECFM,X'C0'
         BNO   *+8                 JUMP IF NOT U-FORMAT
         LH    R1,DCBBLKSI
         STH   R1,FILEND(AE)       SET LRECL FOR THE LOCAL BUF
         CLI   DCBRECFM,X'80'
         BNL   PAFGOP1             JUMP IF NOT V-FORMAT
         MVI   FILBEG+1(AE),4      ALLOW 4-BYTE RDW/SDW
*
PAFGOP1  DS    0H
         TM    DCBRECFM,X'06'
         BZ    PAFGOPZ             JUMP IF NO CONTROL CHARS
         CLI   CCFLAG,0            OR IF CONTROL CHARS ARE NOT
         BE    PAFGOPZ             TO BE INSERTED
         OI    FILBEG+1(AE),1      ADD IN 1-BYTE CONTROL CHAR
*
PAFGOPZ  DS    0H
         L     R11,SAVEGOP+4
         L     R14,SAVEGOP
         BR    R14
*
*
*
*
*****************************************************************
*        Subroutine to open a file (for output)
*****************************************************************
*
PAFPOP   DS    0H
*
         USING PAFPOP,R15
*
         ST    R14,SAVEPOP
         ST    R11,SAVEPOP+4
         LR    R11,R15
*
         DROP  R15
         USING PAFPOP,R11
*
         NI    FILOPN(AE),TEXTFLAG     PUT-OPEN THE FILE
*
         L     R14,=A(OPENMODE)
         MVI   0(R14),C'O'
*
         L     R15,=A(PAFOPN)
         BALR  R14,R15             call open for read
*
*        MVI   TROPENR+12,C'O'
*        MVC   TROPENR+14(8),FILNAM(AE)
*        MVC   TROPENR+32(8),FILMEM(AE)
*        L     R14,=A(ABNDCODE)
*        MVC   TROPENR+41(1),0(R14)
*        LA    R3,TROPENR
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
         L     R14,=A(ABNDCODE)
         CLI   0(R14),C' '
         BNE   PAFPOPZ
*
         TM    DCBOFLGS,X'10'
         BNO   BADWRITE            REPORT ANY PROBLEM
*
************************************************************
*   ausgeben der beiden MACRF Bytes im DCB zum Test
*   2. fuer PUT
*   Bits wie folgt belegt:
*   X'01000000' - keine Ahnung, immer an im Test
*   X'00010000' - move mode
*   X'00001000' - locate mode
************************************************************
*
*        XR    R3,R3
*        IC    R3,DCBMACRF+1
*        CVD   R3,DOWO
*        UNPK  TRMACRF+18(9),DOWO+3(5)
*        OI    TRMACRF+26,X'F0'
*        LA    R3,TRMACRF
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
         TM    DCBMACRF+1,X'10'
         BNO   PAFPOP2
         OI    FILOPN(AE),TEXTMOVE     MOVE-MODE ON FILE OUTPUT
*
PAFPOP2  DS    0H
         OI    FILOPN(AE),WRITEOPN     SET FLAGS, BUF PTRS ETC.
         OI    FILEOF(AE),1
         MVI   FILEOFP(AE),TRUE    OUTPUT FILES HAVE EOF TRUE
         SR    R0,R0
         ST    R0,FILLIM(AE)       CLEAR OUTPUT LIMIT
         TM    FILOPN(AE),TEXTFLAG
         BZ    PAFPOPZ             RETURN IF NOT A TEXT FILE
         MVI   FILEOLN(AE),TRUE    SET EOL - IN CASE !
         STH   R0,FILPTR(AE)       CLEAR LINE POSN PTR
         ST    R0,FILBUF(AE)       NULLIFY BUFFER POINTER
         LH    R1,DCBLRECL         GET LRECL FROM THE DCB
*
************************************************************
*   das fuehrt dazu, dass auch bei RECFM F die BLKSIZE in die
*   Satzlaenge kopiert wird - offenbar nur bei RECFM = U
************************************************************
*
         TM    DCBRECFM,X'C0'
         BNO   *+12                JUMP IF NOT U-FORMAT
         LH    R1,DCBBLKSI
         STH   R1,DCBLRECL
*
         STH   R0,FILBEG(AE)       RESET CHAR. START POSITION
         BCTR  R0,0
         STH   R0,FILEND(AE)       INIT. BUFFER TO "OVER-FULL"
         CLI   DCBRECFM,X'80'
         BNL   PAFPOP1             JUMP IF NOT V-FORMAT
         MVI   FILBEG+1(AE),4      ALLOW 4-BYTE RDW/SDW
PAFPOP1  TM    DCBRECFM,X'06'      TEST IF CONTROL CHARS
         BZ    PAFPOPE
         CLI   CCFLAG,0            RETURN IF CONTROL CHARS ARE NOT
         BE    PAFPOPE             TO BE INSERTED
         OI    FILBEG+1(AE),1      ADD 1-BYTE CONTROL CHAR.
*
PAFPOPE  DS    0H
         TM    FILOPN(AE),TEXTMOVE
         BZ    PAFPOPZ
*
*        XR    R3,R3
*        LH    R3,DCBLRECL
*        CVD   R3,DOWO
*        UNPK  TRLRECL2+18(9),DOWO+3(5)
*        OI    TRLRECL2+26,X'F0'
*        LA    R3,TRLRECL2
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
         LH    R5,DCBLRECL
         GETMAIN R,LV=(R5)
         ST    R1,FILBUF(AE)           FILE BUFFER FOR MOVE MODE
         MVC   FILEND(2,AE),DCBLRECL   RESET BUFFER LENGTH
         LR    AF,R1
*
         BAL   R5,PAFCLR
*
PAFPOPZ  DS    0H
         L     R11,SAVEPOP+4
         L     R14,SAVEPOP
         BR    R14
*
*
*
*
*****************************************************************
*        Subroutine to prepare open of a file
*****************************************************************
*
PAFOPN   DS    0H
*
         USING PAFOPN,R15
*
         ST    R14,SAVEOPN
         ST    R11,SAVEOPN+4
         LR    R11,R15
*
         DROP  R15
         USING PAFOPN,R11
*
*        L     R14,=A(OPENMODE)
*        MVC   TRMEMBER+12(1),0(R14)
*        MVC   TRMEMBER+14(8),FILNAM(AE)
*        MVC   TRMEMBER+32(8),FILMEM(AE)
*        LA    R3,TRMEMBER
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
*****************************************************************
*        save dcb for possible retry
*        and set abndcode flag to space
*****************************************************************
*
         MVC   FILDCBS(LENDCB,AE),FILDCB(AE)
*
PAFOPN1  DS    0H
         L     R14,=A(ABNDCODE)
         MVI   0(R14),C' '
*
*****************************************************************
*        no check for member name etc. on CMS
*****************************************************************
*
         AIF   ('&SYSPARM' EQ 'CMS').CMS103
*
*****************************************************************
*        if a member name has been assigned using ASSIGNMEM,
*        this member name overrides the member name in the JFCB,
*        which means: this member will be opened
*        see: OPEN ... TYPE=J
*        thanks to Gerhard Pospischil for support on this
*****************************************************************
*
         CLC   FILMEM(8,AE),=CL8' '
         BE    PAFOPN2
*
*****************************************************************
*        call DEVTYPE to get the device type
*        X'20' means: direct access device
*        X'01' means: instream file
*****************************************************************
*
         LA    AG,FILDCB(AE)
         LA    R3,DCBDDNAM-IHADCB(AG)
         DEVTYPE (R3),RDEVTYPE
*
         LTR   R15,R15
         BNZ   PAFOPN2
*
*        XR    R3,R3
*        IC    R3,RDEVTYPE+2
*        CVD   R3,DOWO
*        UNPK  TRDEVTYP+18(9),DOWO+3(5)
*        OI    TRDEVTYP+26,X'F0'
*        LA    R3,TRDEVTYP
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
         CLI   RDEVTYPE+2,X'20'
         BNE   PAFOPN2
*
*****************************************************************
*        call RDJFCB to get DSName and Member name (from DD)
*        JFCPDS: true if PDS - Member name has been specified
*        JFCGDG: true if GDG version has been specified
*        JFCPDS does not tell if DSN is PDS or not ...
*****************************************************************
*
         LA    AG,FILDCB(AE)
         LR    R3,AG
         RDJFCB ((R3))
*
         MVC   TRJFCB+18(44),JFCBDSNM
         TM    JFCBIND1,JFCPDS         Member specified on DD ?
         MVI   TRJFCB+60,C' '
         BNO   *+8
         MVI   TRJFCB+60,C'P'
         MVC   TRJFCB+62(8),JFCBELNM
*
*****************************************************************
*        JFCDSRG1 does not tell, if PDS or not before OPEN ...
*****************************************************************
*
*        XR    R3,R3
*        IC    R3,JFCDSRG1
*        CVD   R3,DOWO
*        UNPK  TRJFCB+71(9),DOWO+3(5)
*        OI    TRJFCB+79,X'F0'
*
*        LA    R3,TRJFCB
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
         MVC   JFCBELNM,FILMEM(AE)     Membername in JFCB
         OI    JFCBIND1,JFCPDS         Datei ist PDS
         OI    JFCBTSDM,JFCVSL         redrive allocation (?)
*
         L     R14,=A(OPENMODE)
         CLI   0(R14),C'O'
         BE    PAFOPNO1
*
         LA    AG,FILDCB(AE)
         OPEN  ((AG),(INPUT)),TYPE=J
         B     PAFOPN3
*
PAFOPNO1 DS    0H
         LA    AG,FILDCB(AE)
         OPEN  ((AG),(OUTPUT)),TYPE=J
         B     PAFOPN3
*
.CMS103  ANOP
*
PAFOPN2  DS    0H
         L     R14,=A(OPENMODE)
         CLI   0(R14),C'O'
         BE    PAFOPNO2
*
         LA    AG,FILDCB(AE)
         OPEN  ((AG),(INPUT))
         B     PAFOPN3
*
PAFOPNO2 DS    0H
         LA    AG,FILDCB(AE)
         OPEN  ((AG),(OUTPUT))
         B     PAFOPN3
*
************************************************************
*   if Abend Code not Blank, the DCB is destroyed
*   and must be restored from the saved DCB
*   if the Abend Code is P (S013-14), there was a try
*   to open a member on a sequential file; try again without
*   a member name
************************************************************
*
PAFOPN3  DS    0H
         L     R14,=A(ABNDCODE)
         CLI   0(R14),C' '
         BE    PAFOPN4
*
         LA    AG,FILDCB(AE)
         CLOSE ((AG))
         TM    DCBBUFCB+3,1
         BO    PAFOPN3A
         FREEPOOL (AG)
*
PAFOPN3A DS    0H
         MVC   FILDCB(LENDCB,AE),FILDCBS(AE)
*
************************************************************
*   retry bei 013-14 funktioniert nicht, desh. auskommentiert
************************************************************
*
*        L     R14,=A(ABNDCODE)
*        CLI   0(R14),C'P'
*        BNE   PAFOPN4
*        CLC   FILMEM(8,AE),=CL8' '
*        BE    PAFOPN4
*        MVC   FILMEM(8,AE),=CL8' '
*        B     PAFOPN1
*
************************************************************
*   ausgeben LRECL und BLKSI
************************************************************
*
PAFOPN4  DS    0H
*        XR    R3,R3
*        LH    R3,DCBLRECL
*        CVD   R3,DOWO
*        UNPK  TRLRECL+18(9),DOWO+3(5)
*        OI    TRLRECL+26,X'F0'
*        LA    R3,TRLRECL
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
*        XR    R3,R3
*        LH    R3,DCBBLKSI
*        CVD   R3,DOWO
*        UNPK  TRBLKSI+18(9),DOWO+3(5)
*        OI    TRBLKSI+26,X'F0'
*        LA    R3,TRBLKSI
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
*        XR    R3,R3
*        IC    R3,DCBRECFM
*        CVD   R3,DOWO
*        UNPK  TRRECFM+18(9),DOWO+3(5)
*        OI    TRRECFM+26,X'F0'
*        LA    R3,TRRECFM
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
PAFOPNZ  DS    0H
         L     R11,SAVEOPN+4
         L     R14,SAVEOPN
         BR    R14
*
*
TRDDNAM  DC    CL80'TRACE: FILENAME = XXXXXXXX'
TRLRECL  DC    CL80'TRACE: LRECL    = XXXXXXXXX'
TRBLKSI  DC    CL80'TRACE: BLKSIZE  = XXXXXXXXX'
TRDEVTYP DC    CL80'TRACE: DEVTYPE  = XXXXXXXXX'
TRJFCB   DC    CL80'TRACE: JFCB     = '
TRRECFM  DC    CL80'TRACE: RECFM    = XXXXXXXXX'
TRMACRF  DC    CL80'TRACE: MACRF    = XXXXXXXXX'
TRLRECL2 DC    CL80'TRACE: LRECL(2) = XXXXXXXXX'
TRMEMBER DC    CL80'TRACE: OPEN X DDXXXXXX MEMBER = XXXXXXXX'
TROPENR  DC    CL80'TRACE:  RET X DDXXXXXX MEMBER = XXXXXXXX'
*                   0....5....1....5....2....5....3....5....4
*
RDEVTYPE DS    0D
         DS    CL(LENDCB)
*
         AIF   ('&SYSPARM' EQ 'CMS').CMS104
         IEFJFCBN
.CMS104  ANOP
*
*
*
*
*
**************************************************************
*        drop all base regs
**************************************************************
*
         DROP  ,
         USING $PASENT,R10
*
*****************************************************************
*        Subroutine to handle JCL parms
*****************************************************************
*
CHKPRM   DS    0H
*
         USING CHKPRM,R15
*
         ST    R14,SAVEPRM
         ST    R11,SAVEPRM+4
         LR    R11,R15
*
         DROP  R15
         USING CHKPRM,R11
*
         L     R1,OSPARMS
         L     R1,0(R1)
*
***************************************************************
*        R1 POINTS TO THE PARAMETER LIST THE FIRST HALF WORD OF
*        WHICH GIVES THE LENGTH OF THE LIST
***************************************************************
*
         LH    R2,0(R1)
         LTR   R2,R2
         BNH   NOPARM              NO PARAMETER LIST SPECIFIED
         LA    R0,256              SET MAX STRING LENGTH
         CR    R2,R0
         BNH   *+6                 JUMP IF LENGTH OK
         LR    R2,R0               ENFORCE THE LIMIT
         LA    R8,1                INCREMENT FOR BXLE & BXH
         LA    R9,1(R1,R2)         LIMIT FOR BXLE & BXH
         LA    R1,2(,R1)           POINT AT FIRST CHAR
         ST    R1,OSPARMAD         SAVE ADDRESS FOR LATER
*
*        LA    R3,TRPARM2
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
PARMRTRY DS    0H
         CLI   0(R1),C'/'
         BE    PARMSLSH            SEPARATOR FOUND ?
         BXLE  R1,R8,PARMRTRY
*
PARMSLSH DS    0H
         LR    R3,R1
         SL    R3,OSPARMAD         COMPUTE STRING LENGTH
         STH   R3,OSPARML          SAVE IT FOR LATER
         BXH   R1,R8,NOPARM        JUMP IF STRING END
*
GOTPARM  DS    0H
         SR    R0,R0               CLEAR NEGATE FLAG
         CLI   0(R1),C','
         BNE   *+8
         LA    R1,1(,R1)
         CLC   0(2,R1),=C'NO'      TEST FOR NEGATION OF KEYWORD
         BNE   *+12
         LA    R0,X'FF'            SET NEGATION FLAG
*
         LA    R1,2(,R1)
         LA    R5,KWRDTAB
         SR    R3,R3
KWRDSRCH DS    0H
         IC    R3,2(,R5)           LOAD KEYWORD LENGTH-1
         LTR   R3,R3               ZERO FLAGS TABLE END
         BZ    NXTPARM             SO EXIT
         EX    R3,KWRDCLC          COMPARE NEXT KEYWORD
         BE    KWRDFND             JUMP IF MATCHED
         LA    R5,4(R3,R5)         STEP TO NEXT ENTRY
         B     KWRDSRCH            AND REPEAT
*
KWRDCLC  CLC   0(*-*,R1),3(R5)
*
****************************************************************
*        GET THE NEXT INTEGER IN THE PARAMETER LIST
****************************************************************
*
         BXH   R1,R8,NOPARM        QUIT IF NO MORE CHARS
GETNUM   DS    0H
         CLI   0(R1),C'='
         BNE   GETNUM-4            SKIP UNTIL THE FIRST '='
*
         SR    R3,R3
         SR    R4,R4               CLEAR ACCUMULATOR
*
NXTDIG   DS    0H
         BXH   R1,R8,0(R7)         RETURN IF NO MORE CHARS
         CLI   0(R1),C'9'
         BHR   R7                  OR IF A NON DIGIT
         IC    R3,0(R1)
         SH    R3,=Y(C'0')
         BLR   R7                  IS ENCOUNTERED
         MH    R4,=H'10'
         AR    R4,R3               OTHERWISE KEEP ACCUMULATING
         B     NXTDIG
*
KWRDFND  DS    0H
         LA    R1,1(R3,R1)         ADVANCE IN PARM STRING
         CLI   1(R5),0             TEST NUMERIC INPUT FLAG
         BE    KWRDNON             JUMP IF NOT WANTED
         LTR   R0,R0               TEST IF "NO" SPECIFIED
         BNZ   KWRDNON             IF SO, NO INTEGER FOLLOWS
         BAL   R7,GETNUM           GET AN INTEGER
         LTR   R4,R4               TEST FOR VALIDITY
         BNP   NXTPARM             AND IGNORE IF NO GOOD
         SR    R3,R3               RE-CLEAR R3 (USED BY GETNUM)
KWRDNON  DS    0H
         IC    R3,0(,R5)           GET RELATIVE ADDRESS
         B     KWRDBASE(R3)        AND GO TO THIS ROUTINE
*
KWRDBASE DS    0H
KWRDSTAK DS    0H
         LTR   R0,R0               TEST "NO" OPTION
         BNZ   NXTPARM             IF SO, IGNORE
         SLA   R4,10               CONVERT TO K
         ST    R4,REQSTORE         RESET REGION SIZE
         ST    R4,REQSTORE+4       AND SET MAXIMUM STORAGE
         B     NXTPARM
*
KWRDIOB  DS    0H
         LTR   R0,R0               TEST "NO" OPTION
         BNZ   NXTPARM             IF SO, IGNORE
         SLA   R4,10               CONVERT TO K
         ST    R4,BUFSTORE         SET I/O BUFFER AMOUNT
         B     NXTPARM
*
KWRDSTRW DS    0H
         LTR   R0,R0               TEST "NO" OPTION
         BNZ   NXTPARM             IF SO, IGNORE
         SLA   R4,10               CONVERT TO K
         ST    R4,STRWSTOR         SET STRING WORKAREA SIZE
         B     NXTPARM
*
KWRDDUMP DS    0H
         STC   R0,DUMPFLAG         SET THE DUMP FLAG
         XI    DUMPFLAG,X'FF'      BUT R0 WAS REVERSED
         B     NXTPARM
*
KWRDTIME DS    0H
*
*        MVC   TRPARM1+17(8),=CL8'TIME'
*        LA    R3,TRPARM1
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
*
         LTR   R0,R0               TEST "NO" OPTION
         LH    R5,=H'-1'           SET FOR UNLIMITED EXECUTION
         BNZ   KWRDTIM2
         LR    R5,R4
         M     R4,=F'38400'        CONVERT TO TIMER UNITS
         CLI   0(R1),C'M'          TEST FOR TIME IN
         BNE   KWRDTIM2            THOUSANDTHS OF A SECOND
         D     R4,=F'1000'         IF SO, CONVERT
*
KWRDTIM2 DS    0H
         ST    R5,EXECTIME         AND SAVE FOR STIMER
         B     NXTPARM
*
KWRDCC   DS    0H
         L     R15,=A(CCFLAG)      FLAG NOT DIRECTLY ADDRESSABLE
         STC   R0,0(,R15)          SET THE FLAG
         B     NXTPARM
*
KWRDSPIE DS    0H
         STC   R0,SPIEFLAG         SET THE FLAG
         B     NXTPARM
*
KWRDSNAP DS    0H
         LTR   R0,R0
         BNZ   KWRDNOSN
*        MVC   TRPARM1+17(8),=CL8'SNAP'
*        LA    R3,TRPARM1
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
         B     KWRDSNP
KWRDNOSN DS    0H
*        MVC   TRPARM1+17(8),=CL8'NOSNAP'
*        LA    R3,TRPARM1
*        L     R15,=A(PTRACE)
*        BALR  R14,R15
KWRDSNP  DS    0H
         STC   R0,SNAPFLAG         SET THE FLAG
         B     NXTPARM
*
NXTPARM  DS    0H
         BXLE  R1,R8,GOTPARM       STEP TO NEXT CHAR
*
*
***************************************************************
*        DDNAME-LIST PARAMETER PROCESSING
***************************************************************
*
NOPARM   EQU   *
         L     R1,OSPARMS
         TM    0(R1),X'80'         TEST IF DDNAME LIST PROVIDED
         BO    NODDPARM
         L     R1,4(,R1)           ADDRESS OF DDNAME LIST PARM
         LH    R2,0(,R1)           LENGTH OF LIST IN BYTES
         L     AE,=A(FILLIST)      POINT AT FIRST FILE IN
         L     AE,0(,AE)              THE CHAIN OF FILE BLOCKS
DDLOOP   SH    R2,=H'8'            CHECK FOR END OF DDNAME LIST
         BM    NODDPARM
         TM    2(R1),X'FF'         CHECK FOR BINARY ZEROS
         BZ    DDDFLT              IF SO, DONT CHANGE DDNAME
         USING IHADCB-FILDCB,AE
         MVC   DCBDDNAM(8),2(R1)   MOVE NEW DDNAME INTO DCB
DDDFLT   LA    R1,8(,R1)           ADVANCE THROUGH LIST
         L     AE,FILLNK(AE)       ADVANCE TO NEXT FILE IN CHAIN
         LTR   AE,AE               TEST FOR END OF CHAIN
         BNZ   DDLOOP              IF NOT END, REPEAT
NODDPARM EQU   *
*
CHKPRMZ  DS    0H
         L     R11,SAVEPRM+4
         L     R14,SAVEPRM
         BR    R14
*
*
KWRDTAB  DC    AL1(KWRDSTAK-KWRDBASE,1,4),C'STACK'
         DC    AL1(KWRDIOB-KWRDBASE,1,4),C'IOBUF'
         DC    AL1(KWRDSTRW-KWRDBASE,1,4),C'STRWA'
         DC    AL1(KWRDDUMP-KWRDBASE,0,3),C'DUMP'
         DC    AL1(KWRDTIME-KWRDBASE,1,3),C'TIME'
         DC    AL1(KWRDCC-KWRDBASE,0,1),C'CC'
         DC    AL1(KWRDSPIE-KWRDBASE,0,3),C'SPIE'
         DC    AL1(KWRDSNAP-KWRDBASE,0,3),C'SNAP'
*        END-OF-TABLE MARKER
         DC    AL1(0,0,0)
*
TRPARM1  DC    CL80'TRACE: PARAMETER XXXXXXXX GEFUNDEN'
TRPARM2  DC    CL80'TRACE: CHKPARM SUBROUTINE Aktiv'
*
*
*
*
*
**************************************************************
*
*        GLOBAL TRACE ROUTINE
*
**************************************************************
*
**************************************************************
*        drop all base regs
**************************************************************
*
         DROP  ,
*
*****************************************************************
*        PASTRACE - TRACE-AUSGABEN FUER MONITOR
*****************************************************************
*
PTRACE   DS    0H
         USING PTRACE,R15
         STM   R14,R4,SAVETRAC
         LR    R4,R15
         DROP  R15
         USING PTRACE,R4
         LTR   R3,R3
         BZ    PTRACEO
         BNP   PTRACEC
*
         CLI   TRACOPEN,1
         BNE   PTRACEZ
         PUT   PASTRACE,0(R3)
         B     PTRACEZ
*
PTRACEO  DS    0H
         OPEN  (PASTRACE,(OUTPUT))
*        PUT   PASTRACE,OPENMSG
         MVI   TRACOPEN,1
         B     PTRACEZ
*
PTRACEC  DS    0H
*        PUT   PASTRACE,CLOSMSG
         CLOSE (PASTRACE)
         MVI   TRACOPEN,0
         B     PTRACEZ
*
PTRACEZ  DS    0H
         LM    R14,R4,SAVETRAC
         BR    R14
*
PASTRACE DCB   DDNAME=PASTRACE,DSORG=PS,MACRF=PM,RECFM=F,LRECL=80
TRACOPEN DC    X'0'
OPENMSG  DC    CL80'PTRACE: NACH OPEN PASTRACE'
CLOSMSG  DC    CL80'PTRACE: VOR CLOSE PASTRACE'
SAVETRAC DS    7F
         LTORG
*
*
*
*
*
**************************************************************
*
*        Show Registers and PSW
*        after certain runtime errors
*
**************************************************************
*
**************************************************************
*        drop all base regs
**************************************************************
*
         DROP  ,
*
SHOWREGS DS    0H
         USING SHOWREGS,R15
         STM   R14,R12,SAVESHOW
         LR    R11,R15
         DROP  R15
         USING SHOWREGS,R11
*
         L     R15,=A($PASCSP2)
         BAL   R6,SKIP2            Skip 2 lines
*
**********************************************************************
*        IF AN INTERRUPT, DUMP PSW AND REGISTERS
**********************************************************************
*
         L     R6,=A(CHKITYPE)     INTERRUPT TYPE
         CLI   0(R6),C'S'
         BNE   SHOWREGZ            JUMP IF NOT AN OCX TYPE ERROR
*
         L     R15,=A($PASCSP2)
         BAL   R6,SKIP2            Skip 2 lines
*
         LA    R2,MSG8             Dump PSW
         LA    R4,L'MSG8
         BAL   R6,MSGLINE          PRINT A DESCRIPTION
*
         LA    R3,9
         L     R2,0(R7)
         BAL   R6,HEXWORD          PRINT 1ST PSW WORD
         LA    R7,4(,R7)
         L     R2,0(R7)
         BAL   R6,HEXWORD          PRINT 2ND PSW WORD
*
         BAL   R6,SKIP2
*
         LA    R2,MSG9A
         LA    R4,L'MSG9A
         BAL   R6,MSGLINE
*
         LA    R5,4                # OF REGS PER LINE
         LA    R3,9
SHOWREG1 DS    0H
         LA    R7,4(R7)
         L     R2,0(R7)
         BAL   R6,HEXWORD          PRINT REGS 0-3
         BCT   R5,SHOWREG1
         BAL   R6,SKIP1
*
         LA    R2,MSG9B
         LA    R4,L'MSG9B
         BAL   R6,MSGLINE
*
         LA    R5,4                # OF REGS PER LINE
         LA    R3,9
SHOWREG2 DS    0H
         LA    R7,4(R7)
         L     R2,0(R7)
         BAL   R6,HEXWORD          PRINT REGS 4-7
         BCT   R5,SHOWREG2
         BAL   R6,SKIP1
*
         LA    R2,MSG9C
         LA    R4,L'MSG9C
         BAL   R6,MSGLINE
*
         LA    R5,4                # OF REGS PER LINE
         LA    R3,9
SHOWREG3 DS    0H
         LA    R7,4(R7)
         L     R2,0(R7)
         BAL   R6,HEXWORD          PRINT REGS 8-11
         BCT   R5,SHOWREG3
         BAL   R6,SKIP1
*
         LA    R2,MSG9D
         LA    R4,L'MSG9D
         BAL   R6,MSGLINE
*
         LA    R5,4                # OF REGS PER LINE
         LA    R3,9
SHOWREG4 DS    0H
         LA    R7,4(R7)
         L     R2,0(R7)
         BAL   R6,HEXWORD          PRINT REGS 12-15
         BCT   R5,SHOWREG4
         BAL   R6,SKIP1
*
SHOWREGZ DS    0H
         LM    R14,R12,SAVESHOW
         BR    R14
*
*
*
*
**************************************************************
*
*        Show Interrupt address etc.
*        after certain runtime errors
*
**************************************************************
*
**************************************************************
*        drop all base regs
**************************************************************
*
         DROP  ,
*
SHOWINTR DS    0H
         USING SHOWINTR,R15
         STM   R14,R12,SAVESHOW
         LR    R11,R15
         DROP  R15
         USING SHOWINTR,R11
*
         L     R7,=A(CHKAREA)
         USING CHKAREA,R7
*
         L     R15,=A($PASCSP2)
         BAL   R6,SKIP1            skip one line
*
         LA    R1,PWRS             runtime function = WRITE STRING
         LA    R2,MSG15            which string
         LA    R4,L'MSG15
         LR    R3,R4               string length and field width
         BALR  R14,R15
*
         L     R2,CHKINTRP         pointer to write
         LA    R3,8                length of output
         BAL   R6,HEXWORD
*
         BAL   R6,SKIP1
*
         LA    R1,PWRS             runtime function = WRITE STRING
         LA    R2,MSG1             which string
         LA    R4,L'MSG1
         LR    R3,R4               string length and field width
         BALR  R14,R15
*
**********************************************************************
*        von R14 (chksave) den entry point (R10) abziehen
*        ergibt offset innerhalb der prozedur
*        neu: WRP wg. hex ausgabe
**********************************************************************
*
         LA    R1,PWRP             runtime function = WRITE pointer
         LA    R3,8                length of output
         L     R2,CHKINTRP         interrupt address
         S     R2,CHKREGS+4*R10    minus register 10 = last entry
         BM    SHOWINT5            TEST FOR OUT-OF-RANGE
         CH    R2,=H'8192'
         BNH   SHOWINT6            ADDRESS OK
SHOWINT5 DS    0H
         LA    R1,PWRC             NOT OK, SO PRINT
         LA    R2,C'?'             A QUESTION MARK INSTEAD
         LA    R3,1
SHOWINT6 DS    0H
         BALR  R14,R15
*
**********************************************************************
*        hier weiter wg. Prozedur-Name
**********************************************************************
*
         L     R2,CHKREGS+4*R10    POINTer TO PROC. ENTRY POINT
         MVC   MSG2PROC,14(R2)     MOVE THE PROC. NAME to message
         LA    R1,PWRS             runtime function = WRITE STRING
         LA    R2,MSG2             which string
         LA    R3,L'MSG2+20
         LR    R4,R3               string length and field width
         BALR  R14,R15
*
**********************************************************************
*        INDICATE THE TYPE OF RUN TIME ERROR
**********************************************************************
*
         BAL   R6,SKIP1
*
         L     R2,CHKERRC             check for 2001 error code
         CH    R2,=H'2001'
         BNE   NORTERR
*
         L     R4,CHKINTRP            check 4 bytes before error psw
         SH    R4,=H'4'
         CLC   0(4,R4),=X'18120053'   this pattern indicates $ERROR
         BNE   NORTERR
*
         LA    R1,PWRS             output $ERROR call
         LA    R2,MSG12A
         LA    R4,L'MSG12A
         LR    R3,R4
         BALR  R14,R15
*
         LA    R1,PWRS             output colon
         LA    R2,MSG13
         LA    R4,L'MSG13
         LR    R3,R4
         BALR  R14,R15
*
         LA    R1,PWRS             output 'ERROR CODE IS'
         LA    R2,MSG12C
         LA    R4,L'MSG12C
         LR    R3,R4
         BALR  R14,R15
*
         LA    R1,PWRI             output error number
         L     R2,CHKREGS+4        from register 1
         LA    R4,1
         LR    R3,R4
         BALR  R14,R15
         B     CMNEXT
*
NORTERR  DS    0H
         LA    R1,PWRS             output ' **** '
         LA    R2,MSG12B
         LA    R4,L'MSG12B
         LR    R3,R4
         BALR  R14,R15
*
         LA    R1,PWRS             output 'ERROR CODE IS'
         LA    R2,MSG12C
         LA    R4,L'MSG12C
         LR    R3,R4
         BALR  R14,R15
*
         LA    R1,PWRI             output error number
         L     R2,CHKERRC
         LA    R4,7
         LR    R3,R4
         BALR  R14,R15
*
         LA    R1,PWRS             output colon
         LA    R2,MSG13
         LA    R4,L'MSG13
         LR    R3,R4
         BALR  R14,R15
*
         CLI   CHKFILE,1           error message related to file i/O
         BNE   NOFILE              no, goto nofile
*
         LA    R1,PWRS
*
         L     R2,CHKERRC
         CH    R2,=H'1009'
         BE    CM1009
*
         LA    R2,MSG14
         LA    R3,L'MSG14
         LR    R4,R3
         BALR  R14,R15
         B     CMSKIP
*
CM1009   DS    0H
         LA    R2,MSG1009
         LA    R3,L'MSG1009
         LR    R4,R3
         BALR  R14,R15
         B     CMSKIP
*
CMSKIP   DS    0H
         BAL   R6,SKIP1
*
         LA    R1,PWRS
         LA    R2,MSPREF
         LA    R3,L'MSPREF
         LR    R4,R3
         BALR  R14,R15
*
NOFILE   DS    0H
         L     R2,CHKMSG
         L     R3,CHKMSGL
         LR    R4,R3
         BALR  R14,R15
*
**********************************************************************
*        INDICATE THE STATMNT # OF THE ERROR LOCATION, IF ASKED FOR
*        (we don't have that without passnap)
**********************************************************************
*
*        BAL   R6,PRNTLOC
*
CMNEXT   DS    0H
         BAL   R6,SKIP1
*
         CLI   CHKITYPE,C'S'       system error (0Cx)
         BNE   NOSYSERR            no, goto nosyserr
*
         LA    R1,PWRS
         LA    R2,MSG16
         LA    R3,L'MSG16
         LR    R4,R3
         BALR  R14,R15
*
         L     R4,CHKINTRP         write 6 bytes before and
         SH    R4,=H'6'            after error psw
         L     R2,0(R4)
         LA    R3,8                lead to error is there)
         BAL   R6,HEXWORD
         L     R2,4(R4)
         LA    R3,9
         BAL   R6,HEXWORD
         L     R2,8(R4)
         BAL   R6,HEXWORD
         BAL   R6,SKIP1
*
NOSYSERR DS    0H
*
         AIF   (&SYSTEM).SYS34
*
**********************************************************************
*        PRINT THE ERRONEOUS VALUE AND THE RANGE
**********************************************************************
*
PRNTRNG  DS    0H
         CLI   CHKRANGE,1
         BNE   SHOWINTZ            NO RANGE TO BE PRINTED
*
         LA    R1,PWRS
         LA    R2,MSG4
         LA    R3,L'MSG4
         LR    R4,R3
         BALR  R14,R15
         LA    R1,PWRI
         L     R2,CHKREGS+4*R2
         LA    R3,8
         BALR  R14,R15             PRINT THE OUT OF RANGE VALUE
         LA    R1,PWRS
         LA    R2,MSG5
         LA    R4,L'MSG5
         LR    R3,R4
         BALR  R14,R15
         LA    R1,PWRI
         L     R2,CHKREGS+4*R0
         LA    R3,8
         BALR  R14,R15             PRINT LOWER BOUND (OF THE RANGE)
         LA    R3,8
         L     R2,CHKREGS+4*R1
         BALR  R14,R15             AND THE UPPER BOUND
         BAL   R6,SKIP1
*
.SYS34   ANOP
*
SHOWINTZ DS    0H
         LM    R14,R12,SAVESHOW
         BR    R14
*
*
*
*
**************************************************************
*
*        Show Call Stack
*        after certain runtime errors
*
**************************************************************
*
**************************************************************
*        drop all base regs
**************************************************************
*
         DROP  ,
*
SHOWSTAK DS    0H
         USING SHOWSTAK,R15
         STM   R14,R12,SAVESHOW
         LR    R11,R15
         DROP  R15
         USING SHOWSTAK,R11
*
         L     R15,=A($PASCSP2)
         BAL   R6,SKIP1
*
**************************************************************
*        ueberschriftszeilen
**************************************************************
*
         LA    R1,PWRS
         LA    R2,MSG6
         LA    R3,L'MSG6
         LR    R4,R3
         BALR  R14,R15
*
         BAL   R6,SKIP1
*
         LA    R1,PWRS
         LA    R2,MSG7A
         LA    R3,MSG7AL
         LR    R4,R3
         BALR  R14,R15
*
         BAL   R6,SKIP1
*
**************************************************************
*        maximal 100 stack zeilen
*        schalter fuer letzte zeile auf null
**************************************************************
*
         LA    R7,100              SET MAX # OF LINKS TO BE TRACED
         MVI   STAKLAST,0
*
**************************************************************
*        hier startet die schleife
*        wenn r13 = R12, sind wir oben angekommen,
*        dann noch eine Zeile fuer rufer = pasmonn ausgeben
**************************************************************
*
NXTLNK   DS    0H
         CR    R13,R12             SEE IF END OF STACK
         BNE   SHOWSTA1
*
         MVI   STAKLAST,1
*
SHOWSTA1 DS    0H
         MVC   CALLED(20),14(R10)        MOVE CALLED'S NAME
         LR    R5,R13                    addr of lower save area
         CLI   STAKLAST,1
         BNE   SHOWSTA2
*
         MVC   CALLER(22),=CL22'Pascal Monitor'
         B     SHOWSTA3
*
SHOWSTA2 DS    0H
         L     R13,4(R13)                go to higher save area
         L     R10,16(R13)               ENTRY POINT OF THE CALLER
         MVC   CALLER(20),14(R10)        MOVE CALLER'S 'NAME'
*
SHOWSTA3 DS    0H
         LA    R1,PWRS
         LA    R2,MSG7B
         LA    R3,MSG7BL
         LR    R4,R3
         BALR  R14,R15
*
         L     R2,16(R5)           get current entry from lower sa
         LA    R2,0(R2)            clean bits at left end
         LA    R3,8
         BAL   R6,HEXWORD          PRINT entry point of current proc
*
         LA    R1,PWRS
         LA    R2,MSG7C
         LA    R3,MSG7CL
         LR    R4,R3
         BALR  R14,R15
*
         CLI   STAKLAST,1
         BE    SHOWSTAZ
*
         L     R3,16(R13)          get higher entry from higher sa
         LA    R3,0(R3)            clean bits at left end
         L     R2,12(R5)           get return address from lower sa
         LA    R2,0(R2)            clean bits at left end
         SR    R2,R3               subtract entry point = call offset
         LA    R3,8
         BAL   R6,HEXWORD          PRINT call offset
*
         LA    R1,PWRC
         LA    R2,C')'
         LA    R3,1
         BALR  R14,R15
*
         BAL   R6,SKIP1
*
         BCT   R7,NXTLNK           NEXT LEVEL IF NOT EXHAUSTED
*
**************************************************************
*        hier ende der schleife
*        wenn wir wegen Anzahl aus der schleife rausfallen,
*        dann noch eine entsprechende Hinweiszeile ausgeben
**************************************************************
*
         LA    R1,PWRS
         LA    R2,MSG7D
         LA    R3,MSG7DL
         LR    R4,R3
         BALR  R14,R15
*
SHOWSTAZ DS    0H
         LM    R14,R12,SAVESHOW
         BR    R14
*
*
*
*
**************************************************************
*
*        Show Some Addresses
*        after certain runtime errors
*
**************************************************************
*
**************************************************************
*        drop all base regs
**************************************************************
*
         DROP  ,
*
SHOWADDR DS    0H
         USING SHOWADDR,R15
         STM   R14,R12,SAVESHOW
         LR    R11,R15
         DROP  R15
         USING SHOWADDR,R11
*
         L     R7,=A(CHKAREA)
         USING CHKAREA,R7
*
         USING STACK,R12
*
         L     R15,=A($PASCSP2)
*
         BAL   R6,SKIP2
*
         LA    R2,MSG21
         LA    R4,L'MSG21
         BAL   R6,MSGLINE
         LA    R1,PWRP             WRITE EPA of $PASENT
         L     R2,CHKPASE
         LA    R3,8
         BALR  R14,R15
         BAL   R6,SKIP1
*
         LA    R2,MSG22
         LA    R4,L'MSG22
         BAL   R6,MSGLINE
         LA    R1,PWRP             WRITE BOTTOM OF STACK
         L     R2,CHKREGS+4*R12
         LA    R3,8
         BALR  R14,R15
         BAL   R6,SKIP1
*
         LA    R2,MSG23
         LA    R4,L'MSG23
         BAL   R6,MSGLINE
         LA    R1,PWRP             WRITE CURRENT STACK FRAME
         L     R2,CHKREGS+4*R13
         LA    R3,8
         BALR  R14,R15
         BAL   R6,SKIP1
*
         LA    R2,MSG24
         LA    R4,L'MSG24
         BAL   R6,MSGLINE
         LA    R1,PWRP             WRITE CURRENT HEAP PTR
         L     R2,CHKHEAPP
         LA    R3,8
         BALR  R14,R15
         BAL   R6,SKIP1
*
         LA    R2,MSG25
         LA    R4,L'MSG25
         BAL   R6,MSGLINE
         LA    R1,PWRP             WRITE POINTER TO TOP OF HEAP
         L     R2,CHKHEAPT
         LA    R3,8
         BALR  R14,R15
         BAL   R6,SKIP2
*
SHOWADDZ DS    0H
         LM    R14,R12,SAVESHOW
         BR    R14
*
*
*
*
**********************************************************************
*        subroutine to print a msg line via CSP WRS
**********************************************************************
*
MSGLINE  DS    0H
         LA    R1,PWRS             WRITE-STRING
         LR    R3,R4               SET FIELD-WIDTH
         LR    R14,R6              SET RETURN ADDRESS
         BR    R15                 GOTO CSP
*
**********************************************************************
*        subroutine to print a hex word via CSP WRP
**********************************************************************
*
HEXWORD  DS    0H
         LA    R1,PWRP             runtime function = WRITE pointer
         LR    R14,R6
         BR    R15                 PRINT THE HEX NUMBER via CSP WRP
*
**********************************************************************
*        call CSP SKP
**********************************************************************
*
SKIP2    LA    R1,PSKP
         LA    R2,2
         LR    R14,R6              SET RETURN ADDRESS
         BR    R15                 GOTO CSP
*
SKIP1    LA    R1,PSKP
         LA    R2,1
         LR    R14,R6              SET RETURN ADDRESS
         BR    R15                 GOTO CSP
*
*
*
SAVESHOW DS    15F
*
STAKLAST DC    X'0'
MSPREF   DC    C' **** '
MSG1     DC    C' **** RUN ERROR AT LOCATION : '
MSG2     DC    C' OF PROCEDURE : '
MSG2PROC DS    CL20
MSG3     DC    C' BETWEEN P_STATEMENTS : '
MSG4     DC    C' **** THE OFFENDING VALUE :'
MSG5     DC    C' IS NOT IN THE RANGE :'
MSG6     DC    C' **** CALL STACK:'
MSG7A    DC    C' CALLED                ENTRY                '
         DC    C' CALLER                CALLOFFS'
MSG7AL   EQU   *-MSG7A
MSG7B    DC    C' '
CALLED   DS    CL20
         DC    C' ('
MSG7BL   EQU   *-MSG7B
MSG7C    DC    C')  CALLED BY  '
CALLER   DS    CL20
         DC    C' ('
MSG7CL   EQU   *-MSG7C
MSG7D    DC    C' ++++ THE CALL STACK LISTING IS'
         DC    C' INCOMPLETE ++++'
MSG7DL   EQU   *-MSG7D
MSG8     DC    C' **** INTERRUPT PSW : '
MSG9A    DC    C' **** REGS   0 -  3 : '
MSG9B    DC    C' **** REGS   4 -  7 : '
MSG9C    DC    C' **** REGS   8 - 11 : '
MSG9D    DC    C' **** REGS  12 - 15 : '
MSG12A   DC    C' **** CALL TO PROC $ERROR  '
MSG12B   DC    C' **** '
MSG12C   DC    C'ERROR CODE IS '
MSG13    DC    C' : '
MSG14    DC    C'OTHER FILE ERROR, SEE NEXT LINE'
MSG15    DC    C' **** INTERRUPT ADDRESS AT  : '
MSG16    DC    C' **** CODE AROUND ERROR PSW : '
MSG1009  DC    C'SYNCHRONOUS I/O ERROR'
MSG21    DC    C' **** ENTRY POINT $PASENT AT  : '
MSG22    DC    C' **** BOTTOM OF RUNTIME STACK : '
MSG23    DC    C' **** CURRENT STACK FRAME     : '
MSG24    DC    C' **** CURRENT HEAP POINTER    : '
MSG25    DC    C' **** POINTER TO TOP OF HEAP  : '
*
TRSHOW1  DC    CL80'TRACE: SHOWREG1 POSITION 1'
TRSHOW2  DC    CL80'TRACE: SHOWREG1 POSITION 2'
TRSHOW3  DC    CL80'TRACE: SHOWREG1 POSITION 3'
TRSHOW4  DC    CL80'TRACE: SHOWREG1 POSITION 4'
*
XHEXBUF  DS    CL9                 UNPACKING BUFFER
XHEXCHAR EQU   *-240               TRANSLATE TABLE FOR CODES
         DC    C'0123456789ABCDEF'     X'F0' TO X'FF' ONLY
*
         LTORG
*
*
*
*
*
*
**************************************************************
*
*        GLOBAL DEFINITIONS
*
*        REGISTER ASSIGNMENTS
*
**************************************************************
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
FPR0     EQU   0                   FLOATING REG 0
FPR2     EQU   2
FPR4     EQU   4
FPR6     EQU   6
K        EQU   1024
IOBUFSZE EQU   36*K                SIZE OF AREA RETURNED TO OS FOR I/O
STRWSIZE EQU   256*K               DEFAULT SIZE OF STRING WORKAREA
MINSTORE EQU   8*K                 MIN ACCEPTABLE DYNAMIC STORAGE SIZE
MAXSTORE EQU   4000*K              MAX REQUESTABLE DYNAMIC STORAGE
LCAFTMST EQU   80                  LOCAL VARIABLE AREA (PASCAL PROCS)
FPRSAREA EQU   32                  FP REG SAVE AREA SIZE
RSLT     EQU   0
RET      EQU   14                  RETURN ADDRESS/LOCATION COUNTER
LINK     EQU   15                  BRANCH REGISTER
PBASE2   EQU   11                  PROGRAM BASE REGISTER #2
PBASE1   EQU   10                  "     "      #1
*
INXERR   EQU   1001                INDEX ERROR RETURN CODE
SUBERR   EQU   1002                SUBRANGE ERROR RETURN CODE
PARERR   EQU   1003                PARAMETER ERROR RETURN CODE
SETERR   EQU   1004                SET ELEMENT ERROR RETURN CODE
PTRERR   EQU   1005                POINTER ERROR RETURN CODE
STKERR   EQU   1006                STACK/HEAP ERROR RETURN CODE
INPERR   EQU   1007                INPUT OPERATION ERROR RETURN CODE
OUTERR   EQU   1008                OUTPUT OPERATION ERROR RETURN CODE
SYNERR   EQU   1009                SYNCHRONOUS I/O ERROR RETURN CODE
TIMERR   EQU   1010                TIME OUT ERROR RETURN CODE
FDFERR   EQU   1011                FILE DEFINITION ERROR RETURN CODE
SPCERR   EQU   1012                'NO SPACE' ERROR RETURN CODE
NDFERR   EQU   1013                UNDEFINED CSP CALL ERROR
LIMERR   EQU   1014                'LINES LIMIT EXCEEDED' ERROR
FILERR   EQU   1015                BAD FILE CONTROL BLOCK
RCDERR   EQU   1016                INPUT RECORD TOO LARGE
EOFERR   EQU   1020                ILLEGAL READ PAST EOF
BOLERR   EQU   1021                BAD BOOLEAN ON INPUT
INTERR   EQU   1022                BAD INTEGER ON INPUT
RELERR   EQU   1023                BAD REAL ON INPUT
BIGERR   EQU   1024                TOO BIG INTEGER ON INPUT
RSTERR   EQU   1025                BAD FILE STATUS FOR READ OPERATION
WSTERR   EQU   1026                BAD FILE STATUS FOR WRITE OPERATION
CSPERR   EQU   1027                PASCSP RECURSION NOT OK
SNPERR   EQU   100                 SNAPSHOT ERROR RETURN CODE
*
*
**************************************************************
*        MISCELLANEOUS CONSTANTS
**************************************************************
*
*
**************************************************************
*        FILE RELATED SYMBOL DEFINITIONS
**************************************************************
*
         EXTRN $PASMAIN
         WXTRN $PASSNAP,IBCOM#
IBCOMINI EQU   64                  IBCOM INITIALIZATION ENTRY POINT
IBCOMXIT EQU   68                  IBCOM TERMINATION ENTRY POINT
*
*
**************************************************************
*        CSP ROUTINE NUMBERS
**************************************************************
*
PREW     EQU   4*4+40
PWRI     EQU   6*4+40
PWRC     EQU   9*4+40
PWRS     EQU   10*4+40
PWLN     EQU   22*4+40
PXIT     EQU   29*4+40
PSKP     EQU   34*4+40
PWRP     EQU   37*4+40
*
**************************************************************
*        FILE BLOCK FIELDS
**************************************************************
*
FILNAM   EQU   0                   PASCAL NAME FOR FILE
FILLNK   EQU   8                   LINK PTR TO NEXT FILE BLOCK
FILPAS   EQU   12                  PTR TO PASCAL FILE VARIABLE
FILBUF   EQU   16                  PTR TO I/O BUFFER
FILLIM   EQU   20                  OUTPUT LINES LIMIT FOR FILE
FILEND   EQU   24                  CURRENT BUFFER LENGTH (TEXTFILES)
FILOPN   EQU   26                  OPEN/TEXT FLAGS
FILEOF   EQU   27                  EOF FLAG
FILPTR   EQU   28                  CURRENT CHAR POS (TEXTFILE)
FILBEG   EQU   30                  CHAR START POS   (TEXTFILE)
FILRSZ   EQU   28                  MAX RECORD SIZE (NON TEXTFILE)
FILCSZ   EQU   30                  FILE COMP. SIZE (NON TEXTFILE)
FILSTA   EQU   32                  File Status (Textfile)
FILEOFP  EQU   33                  EOF FLAG IN PASCAL
*                                  (neu, vorher in PASCAL FCB)
FILEOLN  EQU   34                  EOLN FLAG IN PASCAL
*                                  (neu, vorher in PASCAL FCB)
FILDIR   EQU   35                  DIR (I(NPUT),O(UTPUT), U=INOUT)
FILTERM  EQU   36                  TERMINAL FLAG
FILRDBS  EQU   37                  READBUF SCHEDULED (Terminal file)
FILMEM   EQU   40                  Member Name for PDS I/O (MVS only)
FILDCB   EQU   48                  DCB POSITION IN BLOCK
*                                  (was 32, when work started in 2016)
FILDCBS  EQU   144                 SAVED DCB (FOR OPEN RETRY) 04.2017
*
**************************************************************
*        PASCAL FILE BLOCK FIELDS
**************************************************************
*
PFILPTR  EQU   0                   PTR TO FILE CONTROL BLOCK
PFILRSZ  EQU   4                   RECORD SIZE  (NON TEXTFILE)
PFILCOMP EQU   8                   FILE COMPONENT
PFILTSIZ EQU   12                  TOTAL SIZE FOR TEXT FILE
*
**************************************************************
*        INTERNAL CHARACTER CODE FOR USEFUL CHARACTERS
**************************************************************
*
FILEOL   EQU   0  ?
FILEXP   EQU   C'E'
FILZER   EQU   C'0'
FILNIN   EQU   C'9'
FILPLU   EQU   C'+'
FILMIN   EQU   C'-'
FILTIM   EQU   C'*'
FILBLA   EQU   C' '
FILDOT   EQU   C'.'
*
**************************************************************
*        READ/WRITE AND TEXT FLAG VALUES
**************************************************************
*
READOPN  EQU   1
WRITEOPN EQU   2
TEXTFLAG EQU   4
TEXTMOVE EQU   8
*
**************************************************************
*        REGISTERS ASSOCIATED WITH FILE USAGE
**************************************************************
*
AD       EQU   9                   PASCAL FILE CONTROL BLOCK
AE       EQU   8                   CSP FILE CONTROL BLOCK
AF       EQU   7                   O.S. RECORD BUFFER FOR FILE
AG       EQU   6                   DCB FOR FILE
*
**************************************************************
*        MISCELLANEOUS CONSTANTS
**************************************************************
*
FALSE    EQU   0
TRUE     EQU   1
*
         END   $PASENT
