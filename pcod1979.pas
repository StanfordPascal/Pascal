(*$D-,N+*)
PROGRAM PCODE_TRANSLATOR(INPUT, OUTPUT, PRD, PRR) ;



  (********************************************************************
   *                                                                  *
   *                                                                  *
   *                                                                  *
   *   P_CODE (POST) PROCESSOR                                        *
   *   -----------------------                                        *
   *                                                                  *
   *                                                                  *
   *   COPYRIGHT 1976, STANFORD LINEAR ACCELERATOR CENTER.            *
   *                                                                  *
   *                                                                  *
   *   THIS IS A TRANSLATOR FOR THE MODIFIED  P-CODE  GENERATED  BY   *
   *   THE  SLAC  PASCAL   COMPILER.  THE TRANSLATOR TRANSLATES THE   *
   *   P_CODE INTO IBM/370 ASSEMBLY  LANGUAGE  OR  STANDARD  OS/370   *
   *   OBJECT  MODULE  WHICH  COULD BE RUN ON THE 370 USING A SMALL   *
   *   I/O PACKAGE.  ALSO  THE  IMMEDIATE  TARGET  MACHINE  OF  THE   *
   *   TRANSLATOR  IS  THE 360/370 COMPUTERS, THE MACHINE DEPENDENT   *
   *   MODULES IN THE PROGRAM ARE  RELATIVELY  ISOLATED  SUCH  THAT   *
   *   CONVERSIONS  FOR  OTHER  REGISTER  ORIENTED  TARGET MACHINES   *
   *   SHOULD BE STRAIGHTFORWARD.                                     *
   *                                                                  *
   *   REFER TO THE 'THE PASCAL P COMPILER:  IMPLEMENTATION  NOTES,   *
   *   U.  AMMANN, K.  JENSEN, H.  NAGELI, AND K.  NORI, DEC.  74.'   *
   *   FOR  THE DEFINITION OF THE P_MACHINE AND THE P SUBSET OF THE   *
   *   PROGRAMMING LANGUAGE "PASCAL".                                 *
   *                                                                  *
   *   FOR A MORE DETAILED DESCRIPTION OF  THE  P_MACHINE  AND  ITS   *
   *   INSTRUCTION   SET   REFER   TO   THE   P_INTERPRETER  UNDER:   *
   *   'WYL.CG.PAS.LIB(PINTERP)' (ON THE TRIPLEX SYSTEM AT SLAC)      *
   *                                                                  *
   *   -GLOBAL, PROCEDURE LEVEL, INFORMATION SUCH AS SIZE AND  CALL   *
   *   PATTERNS  ARE  (OPTIONALLY) READ IN PARALLEL WITH THE P_CODE   *
   *   AND MAY BE USED  FOR  MORE  EFFICIENT  PROCEDURE  ENTRY/EXIT   *
   *   IMPLEMENTATION.                                                *
   *                                                                  *
   *   -P_INSTRUCTIONS WITH AN IMPLIED GLOBAL  (DATA)  LEVEL  (I.E.   *
   *   LAO, LDO, SRO) ARE ELIMINATED FROM THE P_MACHINE INSTRUCTION   *
   *   SET.   THESE INSTRUCTIONS ARE REPLACED BY THEIR MORE GENERAL   *
   *   COUNTER-PARTS (I.E.  LDA, LOD, STR) AND THE 'LOCAL'/'GLOBAL'   *
   *   ATTRIBUTE OF THE REFERENCE IS  EASILY  DETERMINED  FROM  THE   *
   *   CONTEXT.                                                       *
   *                                                                  *
   *   -THE NAME, AS WELL AS  COMPILATION  DATE  AND  TIME  OF  THE   *
   *   SOURCE  PROGRAM  ARE  RECORDED  AT  THE  ENTRY  POINT OF THE   *
   *   'MAINBLK' IN  THE  OBJECT  CODE.   THIS  PROVIDES  A  UNIQUE   *
   *   SIGNATURE  FOR  EACH OBJECT MODULE AND SIMPLIFIES MATCHING A   *
   *   SOURCE PROGRAM LISTING AND ITS CORRESPONDING OBJECT CODE.      *
   *                                                                  *
   *                                                                  *
   *   -THE ERROR MESSAGES ISSUED  BY  THE TRANSLATOR  ARE  USUALLY   *
   *   ACCOMPANIED  BY THE  APPROXIMATE LINE  NUMBER  OF THE SOURCE   *
   *   STATEMENT.    THESE NUMBERS APPEAR ON THE LEFT OF THE SOURCE   *
   *   PROGRAM LISTING AND THE ERROR SHOULD BE LOCATED BETWEEN  THE   *
   *   STATEMENT  WITH THE GIVEN NUMBER AND THAT NUMBER+1.    THESE   *
   *   ERROR CODES SHOULD BE INTERPRETED ACCORDING TO THE FOLLOWING   *
   *   TABLE:                                                         *
   *                                                                  *
   *                                                                  *
   *                                                                  *
   *   202- STRING TOO LONG (LOGER THAN 255 CHARACTERS)               *
   *        --> REDUCE THE VALUE FOR 'MAXSTRLEN' IN THE COMPILER.     *
   *                                                                  *
   *   253- PROCEDURE TOO LONG (LARGER THAN 8K BYTES).                *
   *        --> DIVIDE (THE PROCEDURE) AND CONQUER.                   *
   *                                                                  *
   *   254- TOO MANY LONG (STRING) CONSTANTS.                         *
   *        --> RECOMPILE THE POST_PROCESSOR WITH  A  LARGER  VALUE   *
   *        FOR STRCNT.                                               *
   *                                                                  *
   *   256- TOO MANY PROCEDURES/FUNCTIONS REFERENCED IN THIS PROC.    *
   *        --> RECOMPILE THE POST_PROCESSOR WITH  A  LARGER  VALUE   *
   *        FOR PRCCNT.                                               *
   *                                                                  *
   *   259- EXPRESSION TOO COMPLICATED.                               *
   *        -->  SIMPLIFY  THE  EXPRESSION  BY  REARRANGING  AND/OR   *
   *        BREAKING.                                                 *
   *                                                                  *
   *   263- TOO MANY (COMPILER GENERATED) LABELS IN THIS PROCEDURE.   *
   *        --> RECOMPILE THE POST_PROCESSOR WITH  A  LARGER  VALUE   *
   *        FOR LBLCNT.                                               *
   *                                                                  *
   *   281- TOO MANY INTEGER CONSTANTS IN THIS PROCEDURE.             *
   *        --> RECOMPILE THE POST_PROCESSOR WITH  A  LARGER  VALUE   *
   *        FOR INTCNT.                                               *
   *                                                                  *
   *   282- TOO MANY DOUBLE WORD (REAL,SET) CONSTANTS IN THIS PROC.   *
   *        --> RECOMPILE THE POST_PROCESSOR WITH  A  LARGER  VALUE   *
   *        FOR DBLCNT.                                                *
   *                                                                  *
   *   300- DIVIDE BY ZERO (RESULT OF CONSTANT PROPAGATION).          *
   *        --> FIX UP  THE  (CONSTANT)  EXPRESSION  EVALUATING  TO   *
   *        ZERO.                                                     *
   *                                                                  *
   *                                                                  *
   *   501- ILLEGAL/UNDEFINED P_INSTRUCTION.                          *
   *        --> FIX UP THE INPUT TO THE POST_PROCESSOR.               *
   *                                                                  *
   *   502- ILLEGAL/UNDEFINED STANDARD PROCEDURE CALL.                *
   *        --> FIXUP THE INPUT TO THE POST_PROESSOR.                 *
   *                                                                  *
   *   503- THE CONTENTS OF 'INPUT' AND 'PRD' FILES DON'T AGREE.      *
   *        -->  FIX  THE  JCL  AND/OR  THE  'QRR'  OUTPUT  OF  THE   *
   *        COMPILER.                                                 *
   *   504- SIZE OF THE ARRAY ELEMENT TOO LARGE.                      *
   *        --> REORDER THE DIMENSIONS OF THE ARRAY (SO THAT THE      *
   *        THE LARGER DIMENTIONS ARE FIRST) OR REDUCE THE RANGE      *
   *        OF THE LOW ORDER (LAST) INDICES.                          *
   *                                                                  *
   *                                                                  *
   *                                                                  *
   *                                                                  *
   *   THE FOLLOWING ERRORS NORMALLY INDICATE AN  INCNSISTANCY IN     *
   *   THE COMPILER AND OR THE POST_PROCESSOR.                        *
   *                                                                  *
   *                                                                  *
   *   601- TYPE CONFLICT OF OPERANDS IN THE P_PROGRAM.               *
   *                                                                  *
   *   602- OPERAND SHOULD BE OF TYPE 'ADR'.                          *
   *                                                                  *
   *   604- ILLEGAL TYPE FOR RUN TIME CHECKING.                       *
   *                                                                  *
   *   605- OPERAND SHOULD BE OF TYPE 'BOOL'.                         *
   *                                                                  *
   *   606- UNDEFINED P_INSTRUCTION CODE.                             *
   *                                                                  *
   *   607- UNDEFINED STANDARD PROCEDURE NAME.                        *
   *                                                                  *
   *   608- DISPLACEMENT FIELD OUT OF RANGE                           *
   *                                                                  *
   *   609- SMALL PROC IS LARGER THAN 4K, RESET SHRT_PROC = 350       *
   *                                                                  *
   *   611- BAD INTEGER ALIGNMENT.                                    *
   *                                                                  *
   *   612- BAD REAL ALIGNMENT.                                       *
   *                                                                  *
   *   613- BAD REAL CONSTANT.                                        *
   *                                                                  *
   *   614- THE PRE_PASS FILE (PRD) IS INCONSISTANT.                  *
   *                                                                  *
   *                                                                  *
   *   THIS PROGRAM SHOULD NOT BE COMPILED WITH THE 'D+' OPTION.      *
   *                                                                  *
   *                                                                  *
   *                                                                  *
   *                                                                  *
   *                                                                  *
   *                           S. HAZEGHI,                            *
   *                                                                  *
   *                           COMPUTATION RESEARCH GROUP             *
   *                           STANFORD LINEAR ACCELARATOR CENTER     *
   *                           STANFORD, CA. 94305.                   *
   *                                                                  *
   *                                                                  *
   *                                                                  *
   *         DATE OF LAST UPDATES:                                    *
   *                                                                  *
   *             JULY 18, 76.                                         *
   *             NOV. 10, 76.                                         *
   *             JAN. 26, 77.                                         *
   *             APR. 15, 77.                                         *
   *             JUNE 22, 77.                                         *
   *             SEP.  7, 77.                                         *
   *             AUG.  4, 78.                                         *
   *             JULY 11, 79.                                         *
   *                                                                  *
   *                                                                  *
   *                                                                  *
   ********************************************************************)




CONST
      VERSION  = 'Oct.-79.';
      MXADR    =  65535 ;
      SHRTINT  =   4095 ;
      HALFINT  =  32700 ;
      STKDPTH  =     15 ;
      MXLVL    =     16 ;
      IDLNGTH  =     12 ;  (*LENGTH OF IDENTIFIERS                            *)
      RGCNT    =      9 ;  (*REGISTER COUNT                                   *)
      FPCNT    =      6 ;  (*FLOATING POINT REG. COUNT                        *)
      OPCNT    =     70 ;  (* OPCODE COUNT                                    *)
"NH"  SPCNT    =     40 ;  (* STANDARD PROC. CNT.                             *)
      GBR      =     12 ;  (*GLOBAL BASE REGITER                              *)
      LBR      =     13 ;  (*LOCAL    "     "                                 *)
      JREG     =     15 ;  (*JUMP (BRANCH) REGISTER                           *)
      RTREG    =     14 ;  (*RETURN ADDR. REGISTER                            *)
      TRG0     =      0 ;  (*PARAMETER REGISTER                               *)
      FPR0     =      0 ;  (*FLOATING POINT REGISTER 0                        *)
      TRG1     =      1 ;  (*TEMPORARY VALUE/BASE REGISTERS                   *)
      TRG13    =     13 ;  (*SAVE AREA/LOCAL STACK FRAME                      *)
      TRG14    =     14 ;
      TRG15    =     15 ;
      TRG9     =      9 ;
      PBR1     =     10 ;
      PBR2     =     11 ;
      FILADR   =      9 ;  (*FILE ADDRESS REGISTER                            *)
      MXSLNGTH =     72 ;
      HDRLNGTH =     32 ;  (*LENGTH OF PROGRAM HEADING                        *)

      ADRSIZE  =      4 ;  CHARSIZE = 1 ;  BOOLSIZE = 1 ;
      INTSIZE  =      4 ;  REALSIZE = 8 ;  SETSIZE  = 8 ;


                          (* LAYOUT OF THE 'GLOBAL' STACK FRAME:              *)

      NEWPTR   =     72 ; (* NEWPTR , OFFSET FROM BOTTOM OF RUNTIME STACK     *)
      HEAPLMT  =     76 ; (* HEAP LIMIT PTR, OFFSET FROM BOTTOM OF STACK      *)
      DYNRUNC  =      0 ; (* # OF COUNTERS , FIELD OFFSET FROM HEAPLMT        *)
      DYN2LEN  =     16 ; (* LENGTH OF THE DYN. INFO. AREA AT THE END OF HEAP *)
      FNCRSLT  =     72 ; (* FUNCTION RESULT LOCATION, OFFSET FROM MP         *)

      DISPLAY  =     80 ; (* DISPLAY REGS, OFFSET FROM BOTTOM OF RUNTIME STK  *)
      LCAFTMST =     80 ; (* SIZE OF THE PROCEDURE LINKAGE AREA               *)
      FPRSAREA =     80 ; (* FLOATING PT. REGS SAVE AREA, (OPTIONAL SAVE)     *)
      FPSIZE   =     32 ; (* LENGTH OF FPR SAVE AREA                          *)

      FL1      =    120 ; (*GLOBAL LOCATIONS USED FOR FIX-FLOAT CONVERSIONS   *)
      FL2      =    128 ;    "                                       "
      FL3      =    136 ;    "                                       "
      FL4      =    144 ;    "                                       "

      INXCHK   =    152 ; (* ADDRESS OF RUNTIME CHECK ROUTINES                *)
      RNGCHK   =    164 ;
      PRMCHK   =    176 ;
      PTRCHK   =    188 ;
      PTACHK   =    200 ;
      SETCHK   =    212 ;
      STKCHK   =    224 ;
      XXXCHK   =    236 ;

      FILEBUFS =    248 ; (* INPUT, OUTPUT, PRD,.... BUFFERS                  *)
      CLEARBUF =    272 ; (* PRESET BUFER TO ERASE MEMORY WITH ITS CONTENTS   *)
      PASDATE  =    280 ; (* PREDEFINED DATE VARIABLE                         *)
      PASTIME  =    290 ; (* PREDEFINED TIME VARIABLE                         *)
      OSPRM    =    300 ;
      FRSTGVAR =    304 ; (* FIRST GLOBAL VAR, SHOULD BE A MULTIPLE OF 8      *)

                          (* VARIOUS TABLE SIZES AND MISC. CONSTATNTS         *)

      HTSIZE   =    139 ; (* HASH TABLE SIZE                                  *)

      INTCNT   =    200 ; (* # OF DISTINCT INT'S REFERENCED IN A PROC.        *)
      DBLCNT   =    100 ; (* # OF DISTINCT REALS/SETS REF. IN A PROC.         *)
      STRCNT   =    200 ; (* # OF CHAR STRINGS REF. IN A PROC.                *)
      PRCCNT   =     45 ; (* # OF PROC'S REFERENCED IN ONE CSECT              *)
      LBLCNT   =    220 ; (* # OF LABELS IN A CSECT                           *)

      MXCODE   =   4092 ; (* MAX OBJECT CODE SIZE (APPROX. 8K BYTES)          *)
      MXCODE1  =   4093 ;
      MXLNP    =    400 ; (* SIZE OF LINE NUMBER TABLE                        *)
      ENDCODE  =   4500 ; (* MXCODE+MXLN, LINE # TABLE IS NOT A PART OF CODE  *)

      LESCND   =      4 ;         LEQCND = 13 ;  (* CONDITION CODE SYMBOLS    *)
      GRTCND   =      2 ;         GEQCND = 11 ;
      EQUCND   =      8 ;         NEQCND =  7 ;
      ANYCND   =     15 ;         NOCND  =  0 ;
      TRUCND   =      1 ;         FLSCND =  8 ;

      SHRT_PROC=    550 ; (* LIMIT VALUE FOR A PROC. TO BE CONSIDERED SMALL   *)


     (* OPCODE TABLES  (P-OPCODES / P-STANDARD PROCS / 370-OPCODES )          *)

      PCTS =  0 ;           " PLDO =  1 ; "  PCTI =  1 ;
      PLOD =  2 ;           " PSRO =  3 ; "  PSTR =  3 ;
      PLDA =  4 ;           " PLAO =  5 ; "  PLOC =  5 ;
      PSTO =  6 ;             PLDC =  7 ;
      PLAB =  8 ;             PIND =  9 ;
      PINC = 10 ;             PMST = 11 ;
      PCUP = 12 ;             PENT = 13 ;
      PRET = 14 ;             PCSP = 15 ;
      PIXA = 16 ;             PEQU = 17 ;
      PNEQ = 18 ;             PGEQ = 19 ;
      PGRT = 20 ;             PLEQ = 21 ;
      PLES = 22 ;             PUJP = 23 ;
      PFJP = 24 ;             PXJP = 25 ;
      PCHK = 26 ;             PNEW = 27 ;
      PADI = 28 ;             PADR = 29 ;
      PSBI = 30 ;             PSBR = 31 ;
      PSGS = 32 ;             PFLT = 33 ;
      PFLO = 34 ;             PTRC = 35 ;
      PNGI = 36 ;             PNGR = 37 ;
      PSQI = 38 ;             PSQR = 39 ;
      PABI = 40 ;             PABR = 41 ;
      PNOT = 42 ;             PAND = 43 ;
      PIOR = 44 ;             PDIF = 45 ;
      PINT = 46 ;             PUNI = 47 ;
      PINN = 48 ;             PMOD = 49 ;
      PODD = 50 ;             PMPI = 51 ;
      PMPR = 52 ;             PDVI = 53 ;
      PDVR = 54 ;             PMOV = 55 ;
      PLCA = 56 ;             PDEC = 57 ;
      PSTP = 58 ;             PSAV = 59 ;
      PRST = 60 ;             PCHR = 61 ;

      PORD = 62 ;             PDEF = 63 ;
      PRND = 64 ;
"NH"  PCRD = 65 ;             PXPO = 66 ;
"NH"  PBGN = 67 ;             PEND = 68 ;

      PGET =  0 ;             PPUT =  1 ;
      PRES =  2 ;             PRLN =  3 ;
      PREW =  4 ;             PWLN =  5 ;
      PWRS =  6 ;             PELN =  7 ;
      PWRI =  8 ;             PWRR =  9 ;
      PWRC = 10 ;             PRDI = 11 ;
      PRDR = 12 ;             PRDC = 13 ;
      PSIN = 14 ;             PCOS = 15 ;
      PEXP = 16 ;             PLOG = 17 ;
      PSQT = 18 ;             PATN = 19 ;
      PEOF = 20 ;             PXIT = 21 ;
      PRDS = 22 ;             PTRP = 23 ;
      PSIO = 24 ;             PEIO = 25 ;
      PCLK = 26 ;             PFDF = 27 ;
      PPAG = 28 ;             PNUL = 29 ;
      PRDB = 30 ;             PWRB = 31 ;
"NH"  PSKP = 32 ;             PLIM = 33 ;
"NH"  PMSG = 34 ;
      PCTR = 35 ;            (*INTERNALLY GENERATED CSP  TO CLEAR RUN COUNTS*)

                                   XLTR = "0 " 18  ;
      XL   = "1 " 88  ;            XLH  = "2 " 72  ;
      XLD  = "3 " 104 ;            XLR  = "4 " 24  ;
      XLDR = "5 " 40  ;            XIC  = "6 " 67  ;
      XLM  = "7 " 152 ;            XLA  = "8 " 65  ;
      XLPR = "9 " 16  ;            XLCR = "10" 19  ;
      XLPDR= "19" 32  ;            XLCDR= " ?" 35  ;
      XLTDR=      34  ;

      XA   = "11" 90  ;            XAH  = "12" 74  ;
      XAD  = "13" 106 ;            XAR  = "14" 26  ;
      XADR = "15" 42  ;            XSDR = "16" 43  ;
      XMDR = "17" 44  ;            XDDR = "18" 45  ;
      XAW  = "20" 110 ;

      XST  = "21" 80  ;            XSTD = "22" 96  ;
      XSTH = "23" 64  ;            XSTC = "24" 66  ;
      XSTM = "25" 144 ;

      XMVC = "27" 210 ;            XMVCL= "28" 14  ;
      XMVI = "29" 146 ;

      XS   = "31" 91  ;            XSH  = "32" 75  ;
      XSD  = "33" 107 ;            XSR  = "34" 27  ;

      XN   = "35" 84  ;            XNR  = "36" 20  ;
      XO   = "37" 86  ;            XXOR  = "38" 22  ;
      XX   = "39" 87  ;            XXR  = "40" 23  ;

      XM   = "41" 92  ;            XMH  = "42" 76  ;
      XMD  = "43" 108 ;            XMR  = "44" 28  ;

      XD   = "47" 93  ;
      XDD  = "49" 109 ;            XDR  = "50" 29  ;

      XBCR = "51" 7   ;            XBC  = "52" 71  ;
      XBCTR= "53" 6   ;            XBCT = "54" 70  ;
      XBALR= "55" 5   ;            XBAL = "56" 69  ;

      XC   = "57" 89  ;            XCR  = "58" 25  ;
      XTM  = "59" 145 ;
      XCLC = "60" 213 ;            XCLCL= "  "  15 ;

      XSLA = "61" 139 ;            XSLDA= "62" 143 ;
      XSRA = "63" 138 ;            XSRDA= "64" 142 ;
      XSLL = "65" 137 ;            XSLDL= "66" 141 ;
      XCD  = "67" 105 ;            XCDR = "68" 41  ;



"CONST" SMX_HIGHT    = 400;                                               ####
                                                                          ####


TYPE  DATATYPE    = (BOOL,CHRC,ADR,INT,PSET,REEL,PROC,STRG,INX,
                     FORT,FINT,FBOOL,FREAL,NON);
      BETA        = ARRAY[1..3] OF CHAR ;
      STRNG       = PACKED ARRAY [1..MXSLNGTH] OF CHAR ;
      ALFA        = PACKED ARRAY [1..8] OF CHAR ;
      IDTYPE      = PACKED ARRAY [1..IDLNGTH] OF CHAR ;
      ADRRNG      = 0..MXADR ;
      OPRNG       = 0..OPCNT ;    (* OPCODE RANGE                             *)
      SPRNG       = 0..SPCNT ;    (* STANDARD PROCEDURE CODE RANGE            *)
      LVLRNG      = 0..MXLVL ;
      RGRNG       = LVLRNG ;      (* REGISTER NUMBER RANGE                    *)
      SETRNG      = SET OF 0..63 ;
      BYTE        = 0..255 ;
      LINE_NUM    = 0..10000 ;
      STKPTR      = 0..STKDPTH ;  (* POINTER TO THE COMPILE_TIME STACK        *)
      LVLDSP      = RECORD LVL: LVLRNG ; DSPLMT: INTEGER  END ;
      BANK        = (RGS,MEM,NEITHER) ;    (*WHERE ABOUT OF THE OPERAND       *)
      SPTR        = @STRNG;
      ICRNG       = 0..MXCODE1;   (* PROGRAM COUNTER RANGE                    *)
      LBLRNG      = -1..LBLCNT;   (* RANGE OF P_COMPILER GENERATED LABELS     *)
      STRLRNG     = 0..MXSLNGTH;
      POSINT      = 0..214748360 ;

      PLABEL      = RECORD
                    NAM : ALFA ;
                    LEN : 0.. IDLNGTH
                    END ;

      DATUM       = RECORD
                  " STKADR: ADRRNG ;"
                    FPA   : LVLDSP ;
                    PCNST : SETRNG ;
                    RCNST : REAL ;
                    SCNSTL : 0..MXSLNGTH ; SCNST : @STRNG ;
                    DTYPE: DATATYPE ;
                    VRBL, DRCT : BOOLEAN ;

                    CASE VPA: BANK OF

                       RGS: (RGADR: RGRNG) ;
                       MEM: (MEMADR: LVLDSP)

                    END ;


VAR   OPC, OLDOPC:     OPRNG ;     (* CURRENT/OLD INST. OPCODE                *)
      CSP, OLDCSP:     SPRNG ;     (* CURRENT STND. PROC. CODE                *)
      NMCDE, EMPTY:    BETA ;      (* CURRENT (SYMBOLIC) PCODE /CSP NAME      *)
      OP_SP :          BOOLEAN ;   (* P INSTR/SP SWITCH                       *)
      INIT:    BOOLEAN ;           (* INITIALIZATION PHASE FLAG               *)
      CH:      CHAR ;              (* CURRENT INPUT CHARACTER                 *)
      BVAL:    BOOLEAN ;
      CHVAL:   CHAR ;
      IVAL:    INTEGER ;
      RVAL:    REAL ;
      PSVAL:   SETRNG ;
      STRPTR:  SPTR ;
      SVAL :   STRNG ;
      SLNGTH :  0..MXSLNGTH ;
      CURLVL:  LVLRNG ;            (* CURRENT PROC. STATIC LEVEL              *)
      TOP :    STKPTR ;            (* TOP OF EXPRESSION STACK                 *)
      LASTLN,  NXTLNP,
      LASTPC,  LASTPCDIF : INTEGER ;
      LBL1, LBL2, LBL3,
      SEGSZE : PLABEL ;            (* LEFT AND RIGHT LABELS OF INSTRUCTIONS   *)
      OPNDTYPE: DATATYPE ;         (* TYPE OF OPERAND OF INSTRUCTION          *)
      P, Q: INTEGER ;              (* P_Q FIELDS OF INSTRUCTION               *)
      SP     : ADRRNG ;            (* MEMORY STACK POINTER, NOT USED          *)
      LCAFTSAREA : ADRRNG ;        (* FIRST LOC. AFTER PROC. SAVE AREA        *)
      CALSTK : ARRAY[0..4] OF
                         STKPTR ;  (* STACKING OF PARM_LIST WITHIN PARM_LIST  *)
      CSTOP  : 0..4 ;              (* TOP OF ABOVE STACK                      *)
      FILECNT : 0..2 ;             (* COUNT OF ACTIVE FILE ADDRESSES          *)
      NXTRG, TXRG: RGRNG ;         (* AQUIRED REGISTERS                       *)

      ROUNDFLG,                    (* KLUDGE, ROUND/TRUNC FLAG                *)
      FILREGACTIVE,                (* FILE ADDRESS REG. ACTIVE                *)
      CSPREGACTIVE,                (* I/O REGISTERS ACTIVE                    *)
      CLEAR_REG, NEG_CND,          (* CLEAR BEFORE LOADING THE REG.           *)
      SAVERGS, SAVEFPRS,
      DEBUG, OS_STYLE, CASE_FLAG,
      TRACE, NEWLINE, FLIPDEBUG,
      RUNPROFILE,                  (* VARIOUS OPTIONS                         *)
      ASM,                         (* ASSEMBLY/OBJECT CODE FLAG               *)
      ASMVERB,                     (* VERBOSE ASSEMBLY, OUTPUT TABLES         *)
      GET_STAT :  BOOLEAN ;        (* TO COLLECT STACK_DEPTH STATISTICS *)####
      CURLINE : LINE_NUM ;         (* CURRENT SOURCE LINE NUMBER              *)
     "NXTLOC  : ICRNG ;  "         (* CURRENT VAL OF P_INS_CNTR MOD 20        *)
      MDTAG :   OPRNG ;            (* MULTIPLY/DIVIDE TAG                     *)
      HEAPMARK : @INTEGER ;
      ZEROBL :   LVLDSP    ;       (* TO CLEAR BASE ,DISPLACEMENT FIELDS      *)
     "TOTALPCODE,"
      TOTALBYTES,
      ERRORCNT :  INTEGER ;        (* TOTAL ERROR COUNT, ALSO RETURN CODE     *)
      S370CNT :   INTEGER ;        (* COUNT OF 370-ONLY INSTRUCTIONS GENERATED*)

      I_S_R   : RECORD             (* SET <=> INTEGER <=> REAL, 370 IMPL.ONLY *)
                DUMMY : REAL    ;  (* DOUBL WORD ALIGNMENT                    *)
                CASE (*TAG:*) INTEGER OF
                     1:   (I1: INTEGER; I2: INTEGER) ;
                     2:   (S: SETRNG) ;
                     3:   (R: REAL )
                END ;

      TYPCDE: ARRAY ['A'..'Z'"CHAR"] OF DATATYPE ; (* ENCODING OF TYPE FIELD *)
      STK:   ARRAY [STKPTR] OF DATUM ;       (* EXPRESSION STACK              *)
      PTBL: ARRAY [OPRNG] OF BETA ;          (* P_INSTRUCTION TABLE           *)
      CSPTBL: ARRAY [SPRNG] OF BETA ;        (*STND. PROC. TABLE              *)
      AVAIL:   ARRAY [0..RGCNT] OF BOOLEAN ; (*AVAILABLE REGISTERS            *)
      AVAILFP: ARRAY [0..FPCNT] OF BOOLEAN ; (* AVAIL. F.P. REGS              *)

      INVBRM:  ARRAY [PEQU..PLES] OF PEQU..PLES ; (* INV. MAP OF REL. OPCODES *)
      BRMSK:   ARRAY [PEQU..PLES] OF 0..15 ;      (* 370 CONDITION CODES      *)
      BRCND : -1..15 ;                            (* ACTIVE BRANCH MASK       *)
      TIMER : POSINT ;

      HTBL: ARRAY [0..HTSIZE] OF                  (* HASH TABLE, INST./PROCS *)
                RECORD
                NAME: BETA ;

                CASE "KIND:" BOOLEAN OF

                  FALSE: (OPCDE: OPRNG) ;
                  TRUE : (SPCDE: SPRNG)

                END ;

  (* PRE-PASS (PRD FILE) INFORMATION                                          *)

  PROC_SIZE  : ICRNG ;
  DATA_SIZE  : ADRRNG ;
  CALL_CNT   : 0..50 ;
  CALL_HIGHER, LARGE_PROC, LARGE_DFRAME, PRE_PASS : BOOLEAN ;


  (* POINTERS TO LAST ELEMENTS OF 'OBJECT' CODE TABLES                        *)


  NXTINT  : 0..INTCNT;  NXTDBL : 0..DBLCNT;  NXTSTR : 0..STRCNT;
  NXTPRC  : 0..PRCCNT;  NXTSLOC : ICRNG    ;
  PC      : ICRNG ;     (* PROGRAM COUNTER DIV 2                              *)
  MINLBL  : LBLRNG ;    (* STARTING LABEL VALUE FOR CURRENT PROC              *)


  (* DECLARATIONS FOR LITERAL TABLES ...ETC. NEEDED TO GENERATE OBJECT MODULE *)

  CURPNAME : ARRAY[1..12] OF CHAR ;         (*NAME OF THE CURRENT PROC        *)
  CURPNO   : INTEGER ;                      (*CURRENT PROC #                  *)

  CODE     : ARRAY[0..ENDCODE] OF INTEGER ;

  INTTBL   : ARRAY[0..INTCNT] OF  RECORD
                                 VAL    : INTEGER ;
                                 LNK    : INTEGER
                                 END ;

  DBLTBL   : ARRAY[0..DBLCNT] OF  RECORD
                                 VAL : SETRNG ;
                                 LNK : INTEGER
                                 END ;

  STRTBL   : ARRAY[0..STRCNT] OF  RECORD
                                 LNK    : INTEGER ;
                                 LNGTH  : STRLRNG
                                 END ;

  LBLTBL   : ARRAY[0..LBLCNT] OF  RECORD
                                 DEFINED : BOOLEAN ;
                                 LNK     : INTEGER
                                 END ;

  PRCTBL   : ARRAY[0..PRCCNT] OF  RECORD
                                 NAME : ALFA ;
                                 LNK  : INTEGER
                                 END ;

  PROGHDR  : ARRAY[1..HDRLNGTH] OF CHAR;     (*PROGRAM HEADER/DATE/TIME     *)

  XTBL: ARRAY [XBALR..XCLC] OF
               ARRAY [0..3] OF CHAR ;        (*370 SYMBOLIC OPCODES          *)

"""EBCDIC   : ARRAY[CHAR] OF  0..255  ;      (*INTERNAL CODE TO EBCDIC CONV.*)"


(*+####                                                                   ####

"VAR "  N_E_CNT, INSTRCNT :  0..MXADR ;                                   ####
      S_AREA, S_LIFE, SMX_DEPTH, OLDTOP :  0..SMX_HIGHT ;                 ####
      T_SAV_DEPTH  : REAL ;                                               ####
      SD_HIST, SMXD_HIST, SAVD_HIST, SAREA_HIST, SLIFE_HIST               ####
                   : ARRAY[0..SMX_HIGHT] OF 0..MXADR ;                    ####
"""   STATFILE     : FILE OF CHAR ;   "                                   ####
      DOING_IO , STAT_DEBUG ,PSEUDO_OP : BOOLEAN ;  DOING_CALL : 0..10 ;  ####
(*_________________________________________________________________           *)


                                                                          ####
PROCEDURE RECORD_STAT ;                                                   ####
                                                                          ####
VAR  AV_DEPTH : REAL ;  LTOP : 0..SMX_HIGHT ;                             ####
                                                                          ####
BEGIN                                                                     ####
IF OPC = PCSP THEN  IF CSP = PSIO THEN  DOING_IO := TRUE                  ####
                   ELSE IF CSP = PEIO THEN DOING_IO := FALSE ;            ####
"IF OPC = PMST THEN  DOING_CALL := DOING_CALL+1 ;                         ####
IF OPC = PCUP THEN  DOING_CALL := DOING_CALL-1 ;  "                       ####
LTOP := TOP-1 ;  IF DOING_IO THEN LTOP := LTOP-1 ;                        ####
IF STAT_DEBUG THEN                                                        ####
  WRITELN(OUTPUT, ' TOP, LTOP, OLDTOP=' ,TOP , LTOP, OLDTOP:6) ;          ####
IF PSEUDO_OP THEN PSEUDO_OP := FALSE ELSE                                 ####
IF OPC < 64 THEN                                                          ####
IF DOING_CALL = 0 THEN                                                    ####
IF NOT(OPC IN [PLAB,PDEF,PLOC]) THEN   (*EXCLUDE PSEUDO OPS *)            ####
BEGIN                                                                     ####
IF LTOP > 0 THEN   (* NON EMPTY STACK *)                                  ####
  IF OLDTOP = 0 THEN                                                      ####
    BEGIN  N_E_CNT := N_E_CNT+1 ;                                         ####
    S_AREA := LTOP ;  S_LIFE := 1 ;  SMX_DEPTH := LTOP                    ####
    END                                                                   ####
  ELSE                                                                    ####
    BEGIN  S_AREA := S_AREA+(LTOP) ;  S_LIFE := S_LIFE+1 ;                ####
    IF (LTOP) > SMX_DEPTH THEN  SMX_DEPTH := LTOP                         ####
    END                                                                   ####
ELSE                                                                      ####
   IF OLDTOP > 0 THEN                                                     ####
      BEGIN  S_LIFE := S_LIFE+1 ;  S_AREA := S_AREA+ (OLDTOP) ;           ####
      SMXD_HIST[SMX_DEPTH] :=SMXD_HIST[SMX_DEPTH]+1 ;                     ####
      SAREA_HIST[S_AREA] := SAREA_HIST[S_AREA] + 1 ;                      ####
      SLIFE_HIST[S_LIFE] := SLIFE_HIST[S_LIFE]+1 ;                        ####
      AV_DEPTH := 10 * S_AREA / S_LIFE ;                                  ####
      SAVD_HIST [ TRUNC(AV_DEPTH) ] := SAVD_HIST[ TRUNC(AV_DEPTH) ]+1 ;   ####
      T_SAV_DEPTH := T_SAV_DEPTH+AV_DEPTH ;                               ####
      END ;                                                               ####
SD_HIST[LTOP] := SD_HIST[LTOP]+1 ;                                        ####
OLDTOP := LTOP ;  INSTRCNT := INSTRCNT+1 ;                                ####
END (* IF NOT OPC IN [LAB,DEF,LOC] *)                                     ####
END (* RECORD_STAT *) ;                                                   ####
                                                                          ####
                                                                          ####
PROCEDURE INIT_STAT  ;                                                    ####
                                                                          ####
VAR  I :  0..SMX_HIGHT  ;                                                 ####
                                                                          ####
BEGIN                                                                     ####
FOR I := 0 TO SMX_HIGHT DO                                                ####
   BEGIN                                                                  ####
   SAREA_HIST[I] := 0 ;                                                   ####
   SMXD_HIST[I] := 0 ;                                                    ####
   SD_HIST[I] := 0 ;                                                      ####
   SLIFE_HIST[I] := 0 ;                                                   ####
   SAVD_HIST[I] := 0 ;                                                    ####
   END ;                                                                  ####
   DOING_CALL := 0 ;   STAT_DEBUG := FALSE ;   PSEUDO_OP := FALSE ;       ####
N_E_CNT := 0 ;   T_SAV_DEPTH := 0.0 ;  OLDTOP := 0 ;  INSTRCNT := 0 ;     ####
END (* INIT_STAT *) ;                                                     ####
                                                                          ####
PROCEDURE PRINT_STAT  ;                                                   ####
                                                                          ####
VAR I : 0..SMX_HIGHT ;                                                    ####
                                                                          ####
BEGIN"""REWRITE(STATFILE) ;                                               ####
WRITELN(STATFILE, '    HISTOGRAMS' ) ;                                    ####
WRITELN(STATFILE, '    ++++++++++') ;                                     ####
WRITELN(STATFILE) ;  WRITELN(STATFILE) ;                                  ####
WRITELN(STATFILE,                                                         ####
    '     STACK_DEPTH, MAX_DEPTH, AVE_DEPTH, STACK_AREA, STACK_LIFE ') ;  ####
WRITELN(STATFILE) ;                                                       ####
FOR I := 0 TO SMX_HIGHT DO                                                ####
WRITELN(STATFILE, I:6, ')', SD_HIST[I]:5, SMXD_HIST[I]:5, SAVD_HIST[I]:5, ####
                  SAREA_HIST[I]:5, SLIFE_HIST[I]:5 ) ;                    ####
WRITELN(STATFILE) ;                                                       ####
WRITELN(STATFILE,'   NON EMPTY STACK CASES, AVER_DEPTH, INSTR COUNT:' ,   ####
                 N_E_CNT:6, T_SAV_DEPTH/(10*N_E_CNT):11, INSTRCNT:6) ;""" ####
END (* PRINT_STAT *)  ;                                                  ####+*)
(*_________________________________________________________________*)

PROCEDURE ERROR(ERRCDE: INTEGER) ;

  BEGIN
  IF ASM THEN
    BEGIN  WRITELN(PRR,'*') ;  WRITELN(PRR,' PERROR', ERRCDE:5) ;  END
  ELSE
    BEGIN
    WRITELN(OUTPUT,'     ****  PERROR',ERRCDE:8, '  ( NEAR LINE',
            LASTLN:6, '  OF PROCEDURE:', CURPNAME:14, ' )') ;
    IF ERRCDE = 253 THEN  WRITELN(' ': 12 , 'PROCEDURE TOO LARGE.') ;
    END ;
  ERRORCNT := ERRORCNT+1 ;
  END ;


PROCEDURE ENTERLOOKUP (*ITEM: BETA; OP_SP: BOOLEAN*) ;

  VAR H: 0..HTSIZE ;

  BEGIN
  H := (ORD(NMCDE[1])*64+ORD(NMCDE[2])*4096+ORD(NMCDE[3])) MOD HTSIZE ;

  IF HTBL[H].NAME <> NMCDE THEN
    WHILE (HTBL[H].NAME <> EMPTY) AND (HTBL[H].NAME <> NMCDE)  DO
""""    BEGIN"WRITELN(' ENTL >',NMCDE, OPC, H) ;" H := (H+2) MOD HTSIZE ; END ;
  (*NO CHECK FOR CYCLES IS MADE HERE*)

  WITH HTBL[H] DO

    IF NAME = NMCDE THEN (*LOOKUP*)

      IF OP_SP THEN OPC := OPCDE  (*OPCODE FOUND*)
      ELSE CSP := SPCDE   (*STANDARD PROC. FOUND *)

    ELSE

      IF INIT THEN
        BEGIN (*ENTER THE ITEM*)
        NAME := NMCDE ;
        IF OP_SP THEN OPCDE := OPC
        ELSE SPCDE := CSP ;
        END
      ELSE IF OP_SP THEN  OPC := OPCNT  ELSE CSP := SPCNT ;
  END (*ENTERLOOKUP*) ;


FUNCTION FLDW(NUM : INTEGER) : INTEGER ;

  VAR FW : INTEGER ;

  BEGIN
    FW := 0 ;
    IF NUM < 0 THEN  BEGIN  FW := 1 ;  NUM := ABS(NUM) ;  END ;
    REPEAT
      NUM := NUM DIV 10 ;  FW := FW+1 ;
    UNTIL NUM = 0 ;
    FLDW := FW
  END (*FLDW*);


"PROCEDURE SET_TO_INT(PSVAL : SETRNG; VAR PS1, PS0 : INTEGER) ;
  (*RETURNS  TWO INTEGERS HAVING THE SAME INTERNAL REPRESENTAION OF PSVAL*)

  VAR I : 0..63 ;

  FUNCTION TWO_TO_I(I : INTEGER): INTEGER ;
    (*COMPUTES 2**I FOR NON NEGATIVE I *)
    BEGIN
    IF I <= 0 THEN TWO_TO_I := 1
    ELSE IF I = 1 THEN  TWO_TO_I := 2
         ELSE TWO_TO_I := TWO_TO_I(I DIV 2)*TWO_TO_I(I-(I DIV 2)) ;
    END (*TWO_TO_I*) ;


  BEGIN  PS0 := 0 ;  PS1 := 0 ;
  FOR I := 0 TO 63 DO
    BEGIN
    IF I IN PSVAL THEN
      IF I < 32 THEN  PS0 := PS0+TWO_TO_I(I)
      ELSE PS1 := PS1+TWO_TO_I(I-32) ;
    END ;
  END (*SET_TO_INT*) ; (*REPLACED BY THE 'SET_TO..' IN THE 370 IMPLEMENTAION*) "


PROCEDURE DUMPSTK(STP1, STP2: STKPTR) ;

  VAR I : STKPTR ;

  BEGIN
  FOR I := STP1 TO STP2 DO
    WITH STK[I] DO
      BEGIN WRITE(PRR,' +++ DEPTH =',I:3,'  FPA =',FPA.LVL:3, FPA.DSPLMT:6) ;
      IF VRBL THEN
      BEGIN
        IF VPA = RGS THEN WRITE(PRR,'  VPA-REG =',RGADR:3)
        ELSE  WRITE(PRR,'  VPA-MEM =',MEMADR.LVL:3,MEMADR.DSPLMT:6) ;
        IF DRCT THEN WRITE(PRR,'  DIRECT ACC.')
          ELSE WRITE(PRR,'  INDIRECT ACC.') ;
        END ;
      IF DTYPE = ADR THEN WRITELN(PRR,'   (ADR)')
      ELSE IF DTYPE = INT THEN WRITELN(PRR,'   (INT)')
           ELSE IF DTYPE = CHRC THEN WRITELN(PRR,'   (CHR)')
                ELSE IF DTYPE = BOOL THEN WRITELN(PRR,'   (BOL)')
                     ELSE WRITELN(PRR,'   (ETC)') ;
      END (*WITH*) ;
  END (*DUMPSTK*) ;
(*_________________________________________________________________*)


PROCEDURE READNXTINST ;

(* TO READ AND DECODE NEXT P_INSTRUCTION *)
(* ------------------------------------- *)

  CONST
         SL16 = 65536 ;

  VAR    I, J, K: INTEGER ;   DUMMYCH, CH1: CHAR ;
         TEMPLBL: ARRAY [1..12] OF CHAR ;

  PROCEDURE READLBL(VAR LBL: PLABEL) ;

    (* SKIPS LEADING BLANKS AND READS THE NEXT CHARACTER SEQUENCE AS A LABEL *)
    (* --------------------------------------------------------------------- *)

    BEGIN
    WITH LBL DO
      BEGIN
      NAM := '        ' ;  LEN := 0 ;

      WHILE INPUT@ = ' ' DO  GET(INPUT) ;

        REPEAT
        READ(INPUT, CH) ;  LEN := LEN+1 ;  NAM[LEN] := CH ;
        UNTIL (INPUT@ = ' ') OR (LEN = 8)  ;

      END (* WITH *) ;
    END (*READLBL*) ;


  PROCEDURE PRINTSRC ;

    (* TO PRINT THE PSOURCE P_INSTRUCTIONS *)
    (* ----------------------------------- *)

    BEGIN
    WRITE(PRR,'*') ;
    IF OPC IN [PDEF,PENT,PLAB] THEN  WRITE(PRR,LBL1.NAM) ;
    WRITE(PRR, NMCDE:5) ;
    IF OPC IN [PCUP,PINC,PDEC,PEQU,PNEQ,PGEQ,PGRT,PLEQ,PLES,PIND,PLDC,"PLDO,"
          PCHK,PLOD,PRET,"PSRO,"PSTO,PSTR,PENT] THEN   (*OPERAND TYPE FIELD*)
      WRITE(PRR,CH1:3) ;

    IF OPC IN [PCUP,PENT,PLDA,PLOD,PMST,PSTR,PCHK] THEN
      WRITE(PRR,P:5) ;

    IF (OPC IN [PCHK,PINC,PDEC,PIND,PIXA,"PLAO,"PLDA,"LDO,"PLOD,PLOC,
                "PSRO,"PSTR,PMOV,PNEW,PDEF,PCTI])
      OR ((OPC IN [PEQU,PLES,PGRT,PNEQ,PLEQ,PGEQ]) AND (OPNDTYPE = STRG)) THEN
      WRITE(PRR,Q:8) ;

    IF OPC IN [PCUP,PFJP,PUJP,PXJP,PCTS] THEN
      WRITE(PRR, LBL2.NAM:10)  ;

    IF OPC = PENT THEN  WRITE(PRR, CURPNAME:14) ;

    IF OPC = PLDC THEN

      CASE OPNDTYPE OF

      BOOL: WRITE(PRR,IVAL:3) ;
      CHRC: WRITE(PRR,' ',CHR(IVAL):2) ;
      INT:  WRITE(PRR,IVAL:10) ;
      ADR:  WRITE(PRR, 'N':3);
      REEL: WRITE(PRR,STRPTR@:SLNGTH) ;
      PSET:
        BEGIN  I_S_R.S := PSVAL ;
        WRITE(PRR,I_S_R.I1:11,' ,',I_S_R.I2:11)
        END (*PSET*)

      END (*CASE*) ;

    IF OPC = PLCA THEN  WRITE(PRR,' ''',SVAL:SLNGTH,'''') ;
    WRITELN(PRR) ;
    "WRITELN(' OPC=',OPC:3,' TYPE,P,Q,IVAL ',CH1:3,P:4,Q:8,IVAL:8,LBL1,LBL2);"
    END (*PRINTSRC*) ;


    BEGIN (*READNXTINST*)

    IF INPUT@ <> ' ' THEN  READLBL(LBL1) ;
    REPEAT  GET(INPUT)  UNTIL INPUT@ <> ' ' ;
    READ(INPUT, NMCDE (* READS NMCDE[1]..NMCDE[3] *) )  ;
(*+#IF STAT_DEBUG THEN  WRITE(OUTPUT,NXTLOC,NMCDE:6) ;                    ####*)
    ENTERLOOKUP(*NMCDE,TRUE*) ; (*LOOK UP THE OPCDE*)
""" WRITELN(' OPCED LOOKED UP >',NMCDE, OPC, PTBL[OPC]:5) ;"
(*+#IF OPC = PMST THEN DOING_CALL := DOING_CALL+1                         ####*)
(*+#ELSE IF OPC = PCUP THEN DOING_CALL := DOING_CALL-1 ;                  ####*)

    IF OPC >= 64 THEN    (*FILTER OUT THOSE THAT CANNOT BE TESTED BY "IN" OP.*)

    BEGIN
    IF OPC = PBGN  THEN
      BEGIN    (*READ AND SET VARIOUS ASSEMBLY / RUNTIME OPTIONS*)
      READLN(INPUT, ASM, CH, GET_STAT, CH, ASMVERB, CH, PROGHDR);
      END
    ELSE
      IF OPC = OPCNT THEN
        BEGIN  WRITE(OUTPUT, ' -->', NMCDE) ; ERROR(606);  READLN(INPUT) END
      ELSE
        BEGIN
        IF ASM THEN  WRITELN(PRR, '*', NMCDE:4);
        READLN(INPUT);
        END
      END (* IF OPC >= 64 *)

    ELSE (* I.E. OPC < 64 *)

      BEGIN
      OPNDTYPE := INT ;  P := CURLVL ;  Q := 0 ;  (*DEFAULT VALUES*)

      IF OPC IN ([PADI,PADR,PSBI,PSBR,PSGS,PFLT,PFLO,PTRC,PNGI,PNGR,PSQI,PSQR]+
                 [PABI,PABR,PNOT,PAND,PIOR,PDIF,PINT,PUNI,PINN,PMOD,PODD,PMPI,
                  PMPR,PDVI,PDVR,PSTP])  (*ZERO ADDRESS OPCODES*)
        THEN  READLN(INPUT)

      ELSE  (*OTHER INSTRUCTIONS*)

        BEGIN

        IF OPC IN [PCUP,PINC,PDEC,PEQU,PNEQ,PGEQ,PGRT,PLEQ,PLES,PIND,PLDC,
                   PLOD,PRET,PSTO,PSTR,PENT,PCHK] THEN    (*OPERAND TYPE FIELD*)
          BEGIN    REPEAT  READ(INPUT, CH1) ;  UNTIL CH1 <> ' ' ;
"NH"      OPNDTYPE := TYPCDE[CH1];  IF INPUT@=',' THEN GET(INPUT);
          END (* OPC IN [PCUP,PINC,PDEC... *) ;

"NH"    IF OPC IN [PCUP,PENT,PLDA,PLOD,PMST,PSTR,PCHK] THEN
"NH"      BEGIN READ(INPUT, P);  IF INPUT@=',' THEN GET(INPUT);  END;

        IF (OPC IN [PCHK,PINC,PDEC,PIND,PIXA,PLDA,PLOD,PLOC,
                    PSTR,PMOV,PNEW,PDEF,PCHK,PCTI]) THEN  READ(INPUT, Q)
        ELSE

          IF OPC IN [PCUP,PFJP,PUJP,PXJP,PCTS] THEN  READLBL(LBL2)
          ELSE

            BEGIN  (* PENT, PCSP, PLCA, PLDC, PEQU, PNEQU .... *)
            IF OPNDTYPE = STRG THEN
              IF OPC IN [PEQU,PLES,PGRT,PNEQ,PLEQ,PGEQ] THEN  READ(INPUT, Q) ;


            IF OPC = PENT THEN
              BEGIN  READLBL(SEGSZE) ;
              REPEAT  GET(INPUT)  UNTIL INPUT@ <> ' ' ;
              READ(INPUT, CURPNAME, CH, SAVERGS, CH, SAVEFPRS, CH, DEBUG, CH,
                          CURPNO) ;
              SAVEFPRS := SAVEFPRS AND (OPNDTYPE = REEL) ;
              CALL_HIGHER := TRUE;  PROC_SIZE := MXCODE1;  DATA_SIZE := MXADR;

              IF PRE_PASS THEN
                BEGIN
                   REPEAT
                   READLN(PRD);  READ(PRD, LBL2.NAM);
                   UNTIL LBL2.NAM = '#PROC   ';
                (* POSITION TO NEXT PROC. INFO. *)
                READ(PRD, TEMPLBL) ;
                IF TEMPLBL <> CURPNAME THEN ERROR(614)
                ELSE
                  BEGIN
                  READLN(PRD, CALL_HIGHER, PROC_SIZE, DATA_SIZE, FLIPDEBUG);
                  END ;
                LARGE_PROC := PROC_SIZE > SHRT_PROC ;
               "WRITELN(' ***',LBL2.NAM:10,TEMPLBL:10,CALL_HIGHER,PROC_SIZE,
                        DATA_SIZE);"
                END (* IF PRE_PASS*) ;

              END (* OPC = ENT *) ;

            IF OPC = PLDC THEN

              CASE OPNDTYPE OF

              BOOL: READ(INPUT, IVAL) ;
              CHRC: BEGIN  READ(INPUT, CH,CH);  IVAL :=" EBCDIC["ORD(CH)"]" END;
              INT:  READ(INPUT, IVAL) ;
              REEL:
"NH"            BEGIN NEW(STRPTR);  SLNGTH := 0;
"NH"              REPEAT READ(INPUT, CH);  SLNGTH := SLNGTH + 1;
"NH"                STRPTR@[SLNGTH] := CH;
"NH"              UNTIL INPUT@ = ' ';
                END ;
              PSET:
                BEGIN
                READ(INPUT, CH, Q, CH, I, CH) ;  I_S_R.I1 := Q*SL16 + I ;
                READ(INPUT, Q, CH, I) ;  I_S_R.I2 := Q*SL16 + I ;
                PSVAL := I_S_R.S ;
                END
              END (*CASE*)


            ELSE  IF OPC IN [PCSP, PLCA] THEN

              BEGIN   REPEAT  GET(INPUT)  UNTIL INPUT@ <> ' ' ;

              IF OPC = PCSP THEN
                BEGIN
                READ(INPUT, NMCDE) ;
                OP_SP := FALSE ;  (* INDICATES A CSP LOOKUP *)
                ENTERLOOKUP(*NMCDE,FALSE "CSP" LOOKUP*) ;
                OP_SP := TRUE ;
                END
              ELSE  (* I.E. IF OPC = PLCA THEN *)
"NH"            BEGIN  SLNGTH := 0;

                  REPEAT
"NH"              READ(INPUT, CH);

                    REPEAT  (*READ INPUT UP TO AND INCLUDING THE FIRST QUOTE*)
                    READ(INPUT, CH);  SLNGTH := SLNGTH+1;  SVAL[SLNGTH] := CH;
                    UNTIL  CH = '''' ;

"NH"              IF INPUT@ = '''' THEN
                    BEGIN
                    IF ASM THEN SLNGTH := SLNGTH+1; SVAL[SLNGTH] := ''''
                    END
                  ELSE  SLNGTH := SLNGTH-1 ;
"NH"              UNTIL INPUT@ <> '''';

                END (*LCA*) ;
              END (* OPC IN [PCSP, PLCA] *);

            END  (* PENT, PCSP, PLCA, PLDC, PEQU, PNEQU .... *) ;
"NH"
"NH"    READLN(INPUT);
        END (*ELSE OTHER INSTRUCTIONS*) ;

      END (* ELSE I.E. OPC < 64 *);

    IF ASM THEN PRINTSRC ;

    END (*READNXTINST*) ;


  PROCEDURE ASMNXTINST ;

  (* TO TRANSLATE THE NEXT P_INSTRUCTION INTO 370 ASSEMBLY/OBJECT CODE *)
  (* ----------------------------------------------------------------- *)


  CONST  SL8     = 256        ;  (* SHIFT LEFT 8 BITS  *)
         SL12    = 4096       ;  (*            12      *)
         SL16    = 65536      ;  (*           16       *)
         SL24    = 16777216   ;  (*           24       *)


  VAR    OP : BYTE ;   P1, P2, B1, B2 : LVLRNG ;   Q1, Q2 : ADRRNG ;
         I,J: INTEGER;   LEFTDEC, NEGATE: BOOLEAN;   POWER10: REAL;
         OPPTR: STKPTR;


    (* THE FOLLOWING PROCEDURES ARE FOR OBJECT CODE GENERATION ONLY  *)
    (* ------------------------------------------------------------- *)


    FUNCTION NEXTPC(PCINCR: ICRNG): ICRNG ;

      BEGIN
      IF PC >= NXTSLOC THEN
        BEGIN  ERROR(253) ;  EXIT(253)  END ;
      NEXTPC := PC+PCINCR ;
      END (*NEXTPC*) ;


    FUNCTION BASE_DSPLMT(PC : ICRNG) : INTEGER ;

      (* CONVERTS PROGRAM COUNTER VALUES TO 370 BASE/DISPLACEMENT HALF WORDS *)
      (* ------------------------------------------------------------------- *)

      BEGIN PC := 2*PC ;
      IF PC < 4096 THEN BASE_DSPLMT := PBR1*SL12+PC
      ELSE IF PC <= 8188 THEN  BASE_DSPLMT := PBR2*SL12+PC-4092
        ELSE  ERROR(253)
      END (*BASE_DSPLMT*) ;


    PROCEDURE UPD_INTTBL(PC: ICRNG; D: INTEGER; TAG :INTEGER) ;

    (* TO ADD HALF/FULL INTEGERS TO THE LITERAL POOL *)
    (* --------------------------------------------- *)

    VAR I : 0..INTCNT;

      BEGIN
      INTTBL[NXTINT].VAL := D ;  I := 0 ;
      WHILE INTTBL[I].VAL <> D DO I := I+1 ;
      CODE[PC] := INTTBL[I].LNK ;  INTTBL[I].LNK := PC ;
      IF I = NXTINT THEN
        IF NXTINT = INTCNT THEN ERROR(261)
        ELSE  BEGIN  NXTINT := NXTINT+1 ;  INTTBL[NXTINT].LNK := 0  END ;
      IF TAG < 0 THEN CODE[PC-1] := -CODE[PC-1] ;
      END (* UPD_INTTBL *) ;


    PROCEDURE UPD_DBLTBL(PC : ICRNG ; S: SETRNG) ;

    (* TO ADD DOUBLE (REALS/SETS) VALUSE TO 'DOUBLE' POOL *)
    (* -------------------------------------------------- *)

    VAR I : 0..DBLCNT;

      BEGIN
      DBLTBL[NXTDBL].VAL := S ;  I := 0 ;
      WHILE DBLTBL[I].VAL <> S DO I := I+1 ;
      CODE[PC] := DBLTBL[I].LNK ;  DBLTBL[I].LNK := PC ;
      IF I = NXTDBL THEN
        IF NXTDBL >= DBLCNT THEN  ERROR(282)
        ELSE  BEGIN  NXTDBL := NXTDBL+1 ;  DBLTBL[NXTDBL].LNK := 0  END
    END (* UPD_DBLTBL *) ;


    PROCEDURE UPD_PRCTBL(PC : ICRNG ; PRC_NAME : ALFA) ;

      (* TO UPDATE EXTERNAL REFERENCE TABLE *)
      (* ---------------------------------- *)

      VAR I : 0..PRCCNT;

      BEGIN
      PRCTBL[NXTPRC].NAME := PRC_NAME ;  I := 0 ;
      WHILE PRCTBL[I].NAME <> PRC_NAME DO I := I+1 ;
      CODE[PC] := PRCTBL[I].LNK ;  PRCTBL[I].LNK := PC ;
      IF I = NXTPRC THEN
        IF NXTPRC >= PRCCNT THEN ERROR(256)
        ELSE
          BEGIN  NXTPRC := NXTPRC+1 ;  PRCTBL[NXTPRC].LNK := 0  END
    END (* UPD_PRCTBL *) ;


    (* UPD_STRTBL IS DONE DIRECTLY BY THE P_INSTRUCTION 'PLCA' *)


    FUNCTION LBLMAP(ALFLBL: ALFA) : LBLRNG ;

    VAR  I : 2..8 ;  J : LBLRNG ;

      BEGIN  (* TRANSLATE GENERATED LABELS TO CONSECUTIVE INTEGERS *)
      I := 2 ;  J := 0 ;
        REPEAT J := J*10+ORD(ALFLBL[I])-ORD('0') ;  I := I+1
        UNTIL ALFLBL[I] = ' ' ;
      LBLMAP := J ;
      END (* LBLMAP *) ;


    PROCEDURE UPD_LBLTBL( PC: ICRNG; INTLBL: LBLRNG; NEWLBL: BOOLEAN) ;

    (* TO 'DEFINE' LABELS AND/OR RESOLVE FORWARD REFERENCES *)
    (* ---------------------------------------------------- *)

    VAR I : LBLRNG ;  TPC, QPC : INTEGER ;

      BEGIN  (* NO PROTECTION AGAINST REDEFINITIONS HERE *)
      IF INTLBL > LBLCNT THEN
        BEGIN WRITELN(' **** INTLBL ',INTLBL) ;  ERROR(263) ;  EXIT(263)   END
      ELSE
        WITH LBLTBL[INTLBL] DO
          IF DEFINED THEN     (* BACKWARD REFERENCE *)

            IF CASE_FLAG THEN  CODE[PC] := LNK*2 (* THIS IS  A HALF WORD ADR. *)
            ELSE  CODE[PC] := BASE_DSPLMT(LNK)   (* BASE/DSPLMT HALF WORD     *)

          ELSE

            IF NEWLBL THEN    (* LABEL DEFINITION *)
              BEGIN
              DEFINED := TRUE ;  TPC := LNK ;
              LNK := PC ;  (* SET LABEL VALUE *)
              WHILE TPC <> 0 DO
                BEGIN
                QPC := TPC ; TPC := CODE[QPC] ;
                IF TPC < 0 THEN
                  BEGIN  CODE[QPC] := PC*2 ;  TPC := ABS(TPC)  END
                ELSE  CODE[QPC] := BASE_DSPLMT(PC) ;
                END
              END
            ELSE   (* NOT NEWLBL I.E. FORWARD REFERENCE, TO BE RESOLVED LATER *)
              BEGIN
              IF CASE_FLAG THEN  CODE[PC] := -LNK  ELSE  CODE[PC] := LNK ;
              LNK := PC
              END ;

      END (* UPD_LBLTBL *) ;


    (* 370 FORMAT CODE GENERATOR (ASSEMBLY/OBJECT CODE) *)
    (* ------------------------------------------------ *)


    PROCEDURE GENRR(OP: BYTE; R1,R2: OPRNG) ;

      BEGIN
      IF ASM THEN
        WRITELN(PRR,XTBL[OP]:5, R1:3,',',R2:FLDW(R2) )
      ELSE  BEGIN  CODE[PC] := OP*SL8  +R1*16+R2 ;  PC := NEXTPC(1)  END
      END (*GENRR*) ;


    PROCEDURE GENRXLIT(OP: BYTE; R: RGRNG; D: INTEGER ; TAG: INTEGER) ;FORWARD ;


    PROCEDURE GENRX(OP:BYTE; R: RGRNG ; D: ADRRNG; X,B: RGRNG) ;

      BEGIN

      IF (D < 0) OR (D > SHRTINT) THEN
        BEGIN  ERROR(608 (*THIS SHOULD NOT BE THE CASE NOW*)) ;

        IF B = TXRG THEN GENRXLIT(XA,TXRG,D,0)
        ELSE
          BEGIN  GENRXLIT(XL,TXRG,D,0) ;
          IF B = 0 THEN B := TXRG
          ELSE
            IF X = 0 THEN X := TXRG
            ELSE  BEGIN GENRR(XAR,TXRG,B) ;  B := TXRG  END ;
          END ;

        D := 0
        END ;

      IF ASM THEN
        BEGIN  WRITE(PRR,XTBL[OP]:5,R:3,',',D: FLDW(D),'(', X: FLDW(X) ) ;
        IF B > 0 THEN WRITE(PRR,',',B: FLDW(B) ) ;
        WRITELN(PRR,')') ;
        END
      ELSE
        BEGIN
        CODE[PC] := OP*SL8  +R*16+X ;  CODE[PC+1] := SL12*B+D ;  PC := NEXTPC(2)
        END
      END (*GENRX*) ;

    PROCEDURE GENRXLIT ;
      VAR SMALL: BOOLEAN ;

      BEGIN
      SMALL := (D >= 0) AND (D <= SHRTINT) ;

      IF SMALL AND (OP = XL) THEN GENRX(XLA,R,D,0,0)
      ELSE

        IF (TAG < 0) AND ((D > 32767) OR (D < -32767)) THEN  ERROR(504)
        ELSE

          IF ASM THEN
            BEGIN   WRITE(PRR,XTBL[OP]:5,R:3) ;
            IF TAG < 0 THEN  WRITELN(PRR,',=H''',D:FLDW(D),'''')
            ELSE  WRITELN(PRR,',=A(',D:FLDW(D),')')
            END
          ELSE   (* ÖASM *)
            BEGIN  CODE[PC] := OP*SL8  +R*16 ;
            UPD_INTTBL(PC+1,D,TAG) ;  PC := NEXTPC(2) ;
            END

    END (*GENRXLIT*) ;


    PROCEDURE GENRXDLIT(OP: BYTE; R: RGRNG; STR: SPTR; STRL: STRLRNG; VAL:REAL);

    VAR I: INTEGER;
    BEGIN
      IF ASM THEN
      BEGIN
       WRITELN(PRR, XTBL[OP]:5, R:3,',=D''', STR@:STRL, '''');
      "FOR I := 1 TO STRL DO
          WRITE(PRR,STR@[I]:1);
       WRITELN(PRR,'''') "
      END
    ELSE
      BEGIN  CODE[PC] := OP*SL8+ R*16+ 00 ;  PC := NEXTPC(2)  ;
      I_S_R.R := VAL ;  UPD_DBLTBL(PC-1,I_S_R.S) ;
      END ;
    END (* GENRXDLIT *);


    PROCEDURE GENRS(OP: BYTE; R1,R2: RGRNG; D: ADRRNG; B: RGRNG ) ;

      BEGIN

      IF (D < 0) OR (D > SHRTINT) THEN
        BEGIN  IF B <> TXRG THEN   GENRR(XLR,TXRG,B); (*GENRX(XLR,TXRG,B,0,0);*)
        GENRXLIT(XA,TXRG,D,0) ;   D := 0 ;   B := TXRG
        END ;

      IF ASM THEN
      WRITELN(PRR,XTBL[OP]:5,R1:3,',',R2:FLDW(R2),',',
                  D:FLDW(D),'(',B:FLDW(B),')')
      ELSE
        BEGIN
        CODE[PC] := OP*SL8 +R1*16 +R2 ; CODE[PC+1] := B*SL12 +D; PC := NEXTPC(2)
        END
      END (*GENRS*) ;


    PROCEDURE GENRSLIT( OP: BYTE; R1,R2: RGRNG; VAL: SETRNG ) ;

      BEGIN
      IF ASM THEN
        BEGIN  I_S_R.S := VAL ;
        WRITELN(PRR, XTBL[OP]:5, R1:3,',', R2:FLDW(R2), ',=F''',
                     I_S_R.I1: FLDW(I_S_R.I1),',',I_S_R.I2:FLDW(I_S_R.I2),'''')
        END
      ELSE
        BEGIN
        CODE[PC] := OP*SL8  +R1*16+R2 ;  UPD_DBLTBL(PC+1,VAL) ;  PC := NEXTPC(2)
        END
      END (*GENRSLIT*) ;


    PROCEDURE GENSS(OP,LNGTH: BYTE; D1: ADRRNG; B1: RGRNG; D2:ADRRNG;B2:RGRNG) ;

      BEGIN
      IF ASM THEN
        WRITELN(PRR,XTBL[OP]:5,D1:6,'(',LNGTH:FLDW(LNGTH),',',B1:FLDW(B1), '),',
                     D2:FLDW(D2),'(',B2:FLDW(B2),')' )
      ELSE
        BEGIN  CODE[PC] := OP*SL8  +(LNGTH-1) ;
        CODE[PC+1] := B1*SL12+D1 ;  CODE[PC+2] := B2*SL12+D2 ;  PC := NEXTPC(3)
        END
      END (*GENSS*) ;


    PROCEDURE GENSI(OP: BYTE; D: ADRRNG; B: RGRNG; I: BYTE) ;

      BEGIN
      IF ASM THEN
        WRITELN(PRR,XTBL[OP]:5,D:8,'(',B:FLDW(B),'),',I:FLDW(I))
      ELSE
        BEGIN
        CODE[PC] := OP*SL8  +I ; CODE[PC+1] := B*SL12+D ; PC := NEXTPC(2)
        END
      END (*GENSI*) ;


    PROCEDURE GENRXLAB(OP: BYTE; R: RGRNG; LAB: PLABEL; TAG:INTEGER) ;

      BEGIN
      IF ASM THEN
        BEGIN
        WRITE(PRR,XTBL[OP]:5, R:3,',') ;
        IF TAG >= 0 THEN  WRITE(PRR,LAB.NAM:LAB.LEN,'(',TAG:FLDW(TAG))
        ELSE  BEGIN  IF TAG = -3 THEN  WRITE(PRR,'=V(')
                     ELSE  (* TAG = -1 *)  WRITE(PRR,'=A(') ;
              WRITE(PRR, LAB.NAM: LAB.LEN) ;
              END ;
        WRITELN(PRR,')') ;
        END
      ELSE

        IF CASE_FLAG THEN
          BEGIN
          UPD_LBLTBL(PC,LBLMAP(LAB.NAM),FALSE(*LAB REF*) ) ;  PC := NEXTPC(1) ;
          END
        ELSE
          BEGIN
          IF TAG >= -1 THEN  (*GENERATED LABEL*)
            UPD_LBLTBL(PC+1,LBLMAP(LAB.NAM),FALSE (*LABEL REFERENCE*))
          ELSE  (*PROC. ID.*)  UPD_PRCTBL(PC+1,LAB.NAM) ;
          IF TAG < 0 THEN TAG := 0 ;
          CODE[PC] := OP*SL8  +R*16+TAG ;  PC := NEXTPC(2)
          END
        END (*GENRXLAB*) ;


    PROCEDURE  FINDRG ;  (*TO FIND A GP REGISTER*)

      VAR I :RGRNG ;

      BEGIN   I := 1 ;

      REPEAT  I := I+1  UNTIL  (AVAIL[I] OR (I = RGCNT)) ;

      IF NOT AVAIL[I] THEN ERROR(259) ;
      AVAIL[I] := FALSE ;  NXTRG := I ;
      END (*FINDRG*) ;


    PROCEDURE  FINDRP ;  (*FIND REGISTER PAIR*)

      VAR  I: RGRNG ;

      BEGIN   I := RGCNT+1  ;

      REPEAT  I := I-2  UNTIL (I < 4) OR (AVAIL[I] AND AVAIL[I+1]) ;

      IF  NOT (AVAIL[I] AND AVAIL[I+1])  THEN ERROR(259) ;
      AVAIL[I] := FALSE ;  AVAIL[I+1] := FALSE ;  NXTRG := I
      END (*FINDRP*) ;


    PROCEDURE FINDFP ; (*FIND A FLOATING POINT REGISTER*)

      VAR I : INTEGER ;

      BEGIN  I := 0 ;

      REPEAT  I := I+2  UNTIL AVAILFP[I] OR (I = FPCNT) ;

      IF NOT AVAILFP[I] THEN ERROR(259)  ;
      AVAILFP[I] := FALSE ;  NXTRG := I
      END (*FINDFP*) ;


    PROCEDURE FREEREG(VAR STE : DATUM) ;

      BEGIN
      WITH STE DO
        IF VRBL AND (VPA = RGS) THEN
        BEGIN
          IF DTYPE=REEL THEN
            AVAILFP[RGADR] := TRUE
          ELSE  (* DTYPE <> REEL *)
            AVAIL[RGADR] := TRUE ;
          IF DTYPE = PSET THEN
            AVAIL[RGADR + 1] := TRUE
        END  (* IF VRBL *)
      END (*FREEREG*) ;


    FUNCTION ALIGN( Q, P: INTEGER): INTEGER ;

      VAR I : INTEGER ;

      BEGIN
      ALIGN := Q ;  I := Q MOD P ;
      IF I <> 0 THEN ALIGN := Q+(P-I) ;
      END (*ALIGN*) ;


   FUNCTION  POWER2(I : INTEGER) : INTEGER ;

   (* IF I > 0 IS A POWER OF TWO, RETURN 'THAT' POWER, ELSE RETURN NEGATIVE *)
   (* --------------------------------------------------------------------- *)

     BEGIN   (* TEMPORARRY     POWER2 := -999 ; *)
     IF I = 1 THEN  POWER2 := 0
     ELSE IF ODD(I) OR (I <= 0) THEN  POWER2 := -999
       ELSE IF I = 2 THEN  POWER2 := 1
         ELSE IF I = 4 THEN  POWER2 := 2
           ELSE IF I = 8 THEN  POWER2 := 3
             ELSE IF I =16 THEN  POWER2 := 4
               ELSE IF I = 32 THEN  POWER2 := 5
                 ELSE  POWER2 := 1+POWER2(I DIV 2) ;
     END (*POWER2*) ;


    PROCEDURE BASE(VAR Q: ADRRNG;VAR P,B: LVLRNG ) ;
    (* TO TRANSLATE A 'LEVEL/OFFSET' P/Q ADDRESS TO 'BASE/INDEX/DISPLACEMENT' *)
    (* ---------------------------------------------------------------------- *)

      BEGIN  B := 0  ;

      IF P > 0 THEN
        BEGIN
        IF P = CURLVL THEN  BEGIN  B := LBR ;  P := 0  END
        ELSE IF P = 1 THEN  BEGIN  B := GBR ;  P := 0  END
             ELSE
               BEGIN
               GENRX(XL,TXRG,DISPLAY+4*P,GBR,0) ;  P := TXRG ;
               END
        END ;

      IF (Q < 0) OR (Q > SHRTINT) THEN
        BEGIN
        IF P > 0 THEN GENRXLIT(XA,P,Q,0)
        ELSE  BEGIN  GENRXLIT(XL,TXRG,Q,0) ;  P := TXRG  END ;
        Q := 0
        END

      END (*BASE*) ;


  PROCEDURE GETADR( STE: DATUM; VAR Q: ADRRNG; VAR P, B: RGRNG) ;  FORWARD ;


  PROCEDURE LOAD(VAR STE: DATUM) ;

    (* LOADS AN STACK ELEMENT INTO A REGISTER, IF NOT ALREADY THERE *)
    (* ------------------------------------------------------------ *)


    VAR P: LVLRNG; Q: ADRRNG; B, R: RGRNG ; OP: BYTE ;


    PROCEDURE FINDMDRG ;

      (*TO FIND A MULTIPLY/DEVIDE REGISTER*)

      BEGIN
      IF MDTAG = PDVI THEN
        BEGIN  FINDRP ;  AVAIL[NXTRG+1] := TRUE  END
      ELSE IF MDTAG = PMPI THEN
             BEGIN  FINDRP ;  AVAIL[NXTRG] := TRUE ;  NXTRG := NXTRG+1 END
           ELSE FINDRG ;
      END (*FINDMDRG*) ;


    BEGIN  (*LOAD*)

    WITH STE DO
      BEGIN
      IF VRBL THEN (* LOAD THE VARIABLE POINTED TO BY STP*)

        IF DRCT THEN (*DIRECTLY ACCESIBLE VARIABLE*)

          CASE DTYPE OF

          ADR,INT,BOOL,CHRC:
            BEGIN

              IF VPA = MEM THEN
                BEGIN  FINDMDRG ;
                P := MEMADR.LVL ;   Q := MEMADR.DSPLMT ;
                BASE(Q,P,B) ;

                IF (DTYPE = CHRC) OR (DTYPE = BOOL) THEN
                  BEGIN
                  IF CLEAR_REG " AND (FPA.DSPLMT = 0) " THEN
                    GENRR(XSR,NXTRG,NXTRG) ;
                  GENRX(XIC,NXTRG,Q,B,P) ;
                  END
                ELSE  GENRX(XL,NXTRG,Q,B,P) ;

                VPA := RGS ;  RGADR := NXTRG ;
                END ;

              P := FPA.LVL ;  Q := FPA.DSPLMT ;
              FPA := ZEROBL ;

              IF Q <> 0 THEN
                IF P > 0 THEN
                  BEGIN  BASE(Q,P,B) ;
                  IF P <= 0 THEN P := B
                  ELSE IF B > 0 THEN GENRR(XAR,P,B) ;
                  IF Q = 0 THEN GENRR(XAR,RGADR,P)
                  ELSE GENRX(XLA,RGADR,Q,P,RGADR) ;
                  END
                ELSE IF Q = -1 THEN GENRR(XBCTR,RGADR,0)
                     ELSE GENRXLIT(XA,RGADR,Q,0) ;

            END (*ADR,INT,BOOL,CHRC*) ;

          REEL:
             IF VPA = MEM THEN
             BEGIN
                FINDFP;
                P := MEMADR.LVL;
                Q := MEMADR.DSPLMT;
                BASE(Q,P,B);
                GENRX(XLD,NXTRG,Q,B,P);
                VPA := RGS;
                RGADR := NXTRG;
             END (* REEL *);

          PSET:
            IF VPA = MEM THEN  (* LOAD POWERSET *)
              BEGIN   P := MEMADR.LVL ;   Q := MEMADR.DSPLMT ;
              BASE( Q, P, B) ;   FINDRP ;

              IF B > 0 THEN IF P > 0 THEN  GENRR(XAR,P,B)  ELSE  P := B ;
              GENRS(XLM,NXTRG,NXTRG+1,Q,P) ;
              VPA := RGS ;  RGADR := NXTRG ;
              END (*PSET*)

          END (* CASE DTYPE_ END OF DIRECT VARIABLE LOAD *)

        ELSE (* IF NOT DRCT *)

          BEGIN
          GETADR(STE, Q, P, B) ;   FPA := ZEROBL ;

            CASE DTYPE OF

            ADR, INT :
              BEGIN   IF VPA = RGS THEN  AVAIL[RGADR] := TRUE ;
              FINDMDRG ;  GENRX(XL, NXTRG, Q, B, P) ;
              END ;

            BOOL, CHRC:
              BEGIN  FINDMDRG ;
              IF CLEAR_REG THEN  GENRR(XSR,NXTRG,NXTRG) ;
              GENRX(XIC, NXTRG, Q, B, P) ;
              IF VPA = RGS THEN  AVAIL[RGADR] := TRUE ;
              END ;

            REEL:
              BEGIN
              FINDFP;
              GENRX(XLD,NXTRG,Q,B,P) ;
              IF VPA = RGS THEN  AVAIL[RGADR] := TRUE ;
              END (* REEL *);

            PSET:
              BEGIN
              IF VPA = RGS THEN  AVAIL[RGADR] := TRUE ;
              IF P > 0 THEN  BEGIN  IF B > 0 THEN  GENRR(XAR,P,B)  END
              ELSE  P := B ;
              FINDRP ;  GENRS(XLM, NXTRG, NXTRG+1, Q, P) ;
              END

            END (* CASE DTYPE *) ;

          VPA := RGS ;   RGADR := NXTRG ;  DRCT := TRUE ;
          END (* LOAD INDRCT *)


      ELSE (* IF NOT VRBL, I.E. LOAD CONSTANT *)


          BEGIN

            CASE DTYPE OF

            ADR:
              BEGIN  P := FPA.LVL ;  Q := FPA.DSPLMT ;
              FINDRG ;
              IF P > 0 THEN  BEGIN  BASE(Q,P,B) ;  GENRX(XLA,NXTRG,Q,B,P) END
              ELSE IF P < 0 THEN
                      BEGIN   GENRX(XLA, NXTRG, 0, 0, 0);
                      CODE[PC-1] := STRTBL[Q].LNK;  STRTBL[Q].LNK := PC-1;
                      END
              ELSE  GENRXLIT(XL,NXTRG,FPA.DSPLMT,0) ; (*NIL VALUE*)
              FPA := ZEROBL ;
              END ;

            INT,BOOL,CHRC:
              BEGIN  FINDMDRG ;
              IF FPA.DSPLMT = 0 THEN GENRR(XSR,NXTRG,NXTRG)
              ELSE GENRXLIT(XL,NXTRG,FPA.DSPLMT,0) ;
              FPA := ZEROBL ;
              END ;

            REEL:
               BEGIN
                  FINDFP;
                  GENRXDLIT(XLD,NXTRG,SCNST,SCNSTL,RCNST)
               END (* REEL *);

            PSET:
              BEGIN  FINDRP ;
              GENRSLIT(XLM,NXTRG,NXTRG+1,PCNST) ;
              END

            END (* CASE DTYPE *) ;

          VRBL := TRUE ;  VPA := RGS ;  RGADR := NXTRG ;  DRCT := TRUE ;
          END (* LOAD CONSTANT *) ;

      IF MDTAG = PMPI THEN
        BEGIN
        IF NOT ODD(RGADR) OR NOT AVAIL[RGADR-1] THEN
          BEGIN  AVAIL[RGADR] := TRUE ;
          FINDRP ;  GENRR(XLR,NXTRG+1,RGADR) ;
          RGADR := NXTRG+1 ;
          END ;
        RGADR := RGADR-1 ;
        AVAIL[RGADR] := FALSE
        END
      ELSE
       IF MDTAG = PDVI THEN
         BEGIN
         IF ODD(RGADR) OR NOT AVAIL[RGADR+1] THEN
           BEGIN  AVAIL[RGADR] := TRUE ;
           FINDRP ;  GENRR(XLR,NXTRG,RGADR) ;
           RGADR := NXTRG ;
           END ;
         AVAIL[RGADR+1] := FALSE ;
         IF ASM THEN  GENRR(XSRDA,RGADR,32)  ELSE  GENRS(XSRDA,RGADR,0,32,0) ;
         END

       END (*WITH STE DO*) ;

    END (* LOAD *) ;


  PROCEDURE GETADR ;


     (* IF PASSED THE ADR. OF AN ITEM , THIS ROUTINE RETURNS A <Q,B,P> ADR. *)
     (* INDIRECTIONS ARE NOT DEREFERENCED HERE.                             *)
     (* ------------------------------------------------------------------- *)

     VAR R: RGRNG ;

     BEGIN  R := 0 ;
     WITH STE DO
       BEGIN  IF DRCT AND (DTYPE <> ADR) THEN  ERROR(602) ;
       IF VRBL THEN

           IF VPA = RGS THEN  R := RGADR

           ELSE  (*IF VPA = MEM THEN*)
                 BEGIN  P := MEMADR.LVL ;  Q := MEMADR.DSPLMT ;
                 BASE(Q, P, B) ;
                 GENRX(XL, TXRG, Q, B, P) ;   R := TXRG
                 END ;

       (* NOW THE VARIABLE PORTION OF THE ADR., IF ANY, IS IN TXRG *)

       Q := FPA.DSPLMT ;  P := FPA.LVL ;
       IF R > 0 THEN
         BEGIN
         IF (Q < 0) OR (Q > SHRTINT) THEN
           BEGIN  GENRXLIT(XA,R,Q,0) ;  Q := 0  END ;
         B := 0 ;
         IF P = CURLVL THEN  B := LBR
         ELSE IF P = 1 THEN  B := GBR
         ELSE IF P > 0 THEN  GENRX(XA,R,DISPLAY+4*P,GBR,0) ;
         P := R ;
         END
       ELSE (* NO INDEX OR VPA *)
         BASE(Q,P,B) ;

       END (*WITH STE*)
     END (*GETADR*) ;


  PROCEDURE GETOPERAND(VAR STE: DATUM; VAR Q1 : ADRRNG; VAR P1, B1: RGRNG) ;

   (* IF PASSED AN ITEM, THIS ROUTINE RETURNS ITS <Q,B,P> ADDRESS *)
   (* ----------------------------------------------------------- *)

    BEGIN
    WITH STE DO
      IF VRBL THEN

        IF DRCT THEN
          IF FPA.DSPLMT <> 0 THEN LOAD(STE)
          ELSE
            BEGIN  IF VPA = MEM THEN
                     BEGIN  Q1 := MEMADR.DSPLMT ;  P1 := MEMADR.LVL ;
                     BASE(Q1,P1,B1) ;
                     END
            (* THE VPA=REG NOT HANDLED HERE *)
            END
        ELSE (*NOT DIRCT*)
          BEGIN
          GETADR(STE,Q1,P1,B1) ;
          IF VPA = RGS THEN  AVAIL[RGADR] := TRUE ;
          END

      ELSE (*^ VRBL*)  (*MAY NOT HAVE ANY FUNCTION ANY MORE*)
        BEGIN  IF DTYPE <> ADR THEN ERROR(602) ;
        Q1 := FPA.DSPLMT ;  P1 := FPA.LVL ;
        BASE(Q1,P1,B1) ;
        END ;
    END (*GETOPERAND*) ;


  PROCEDURE STORE(STP: STKPTR; INDRCT: BOOLEAN) ;

    (* STORE THE STACK ELEMENT IN THE LOCATION DENOTED BY : *)
    (* IF INDRCT  THEN  2_ND TOP STACK ELEMENT              *)
    (* ELSE P_Q FIELDS OF THE CURRENT INSTRUCTION           *)
    (* ---------------------------------------------------- *)

    VAR B : RGRNG ;

    BEGIN
    (* LOADS THE ELEMENT INTO A REGISTER *)
    CLEAR_REG := STK[STP].DTYPE <> OPNDTYPE ;
    LOAD(STK[STP]) ;  CLEAR_REG := TRUE ;

    IF INDRCT THEN
      BEGIN  IF NOT STK[STP-1].DRCT THEN LOAD(STK[STP-1]) ;
      GETADR(STK[STP-1],Q,P,B) ;
      FREEREG(STK[STP-1]) ;
      END
    ELSE  BASE(Q,P,B) ;

    WITH STK[STP] DO
      BEGIN
      IF VRBL THEN
        IF (NOT DRCT) OR (VPA = MEM) THEN
          IF DTYPE <> OPNDTYPE THEN ERROR(601) ;

        CASE OPNDTYPE OF

        ADR,INT:   BEGIN  GENRX(XST,RGADR,Q,B,P) ;  AVAIL[RGADR] := TRUE END ;

        BOOL,CHRC: BEGIN  GENRX(XSTC,RGADR,Q,B,P) ;  AVAIL[RGADR] := TRUE END ;

        REEL:
          BEGIN
            GENRX(XSTD,RGADR,Q,B,P) ;
            AVAILFP[RGADR] := TRUE
          END  (* REEL *) ;

        PSET:      BEGIN
                   IF B > 0 THEN  IF P > 0 THEN GENRR(XAR,P,B) ELSE P := B ;
                   GENRS(XSTM,RGADR,RGADR+1,Q,P) ;
                   AVAIL[RGADR] := TRUE ;  AVAIL[RGADR+1] := TRUE ;
                   END

        END (* CASE OPNDTYPE *)

      END (* WITH STK... *)

    END (* STORE *) ;


" PROCEDURE PUTLNM(Q : LINE_NUM) ;

    BEGIN  (* TO OUTPUT THE SOURCE PROGRAM LINE NUMBER *)
    IF RUNPROFILE AND NEWLINE THEN
      BEGIN  WRITELN(PRR,'LNM':4,Q:6) ;
      CURLINE := Q ;  NEWLINE := FALSE ;
      END ;
    END (*PUTLNM*) ;  "


  PROCEDURE CALLSUB(FPARM, LPARM : STKPTR) ;

  (* TO 'UNLOAD' REGISTER STACK ONTO ALLOCATION STACK PRIOR TO A CALL *)
  (* ---------------------------------------------------------------- *)

    VAR  I : STKPTR ;  PRMOFST : ADRRNG ;
"NH"   FPR : 0 .. FPCNT ;

    BEGIN  (* WE CAN LOAD EVERYTING STACKED SO FAR TO AVOID POSSIBLE SIDE EFFECT
              DUE TO CURRENT CALL MODIFYING THE PRECEDING PARAMETERS          *)
    (* PUT THE SUBROUTINE PARAMETERS ON THE STACK *) ;
    IF DATA_SIZE < 4096 THEN  GENRX(XLA,TRG1,DATA_SIZE,LBR,0)
    ELSE
      BEGIN  GENRR(XLR,TRG1,LBR); GENRXLAB(XA,TRG1,SEGSZE,-1) END;

    PRMOFST := LCAFTMST ;  CLEAR_REG := FALSE ;
    IF ODD(P) THEN
      IF NOT(OPNDTYPE IN [FORT,FBOOL,FINT,FREAL]) THEN
        PRMOFST := LCAFTMST+FPSIZE ;  (* REAL FUNC SAVE FPRS *)
    FOR I := FPARM TO LPARM DO
      BEGIN  LOAD(STK[I]) ;
        WITH STK[I] DO
        CASE DTYPE OF

          BOOL, CHRC: BEGIN  GENRX(XSTC,RGADR,PRMOFST,TRG1,0) ;
                      PRMOFST := PRMOFST+"INTSIZE" BOOLSIZE ;
                      END ;

          INT, ADR:   BEGIN  PRMOFST := ALIGN(PRMOFST,INTSIZE) ;
                      GENRX(XST,RGADR,PRMOFST,TRG1,0) ;
                      PRMOFST := PRMOFST+INTSIZE ;
                      END ;

          REEL :      BEGIN   PRMOFST := ALIGN(PRMOFST, REALSIZE) ;
                      GENRX(XSTD,RGADR,PRMOFST,TRG1,0) ;
                      PRMOFST := PRMOFST+REALSIZE ;
                      END ;

          PSET :      BEGIN   PRMOFST := ALIGN(PRMOFST, INTSIZE) ;
                      GENRS(XSTM,RGADR,RGADR+1,PRMOFST,TRG1) ;
                      PRMOFST := PRMOFST+SETSIZE ;
                      END

        END (* CASE DTAYPE OF ...,  WITH STK[I]...  *) ;
      FREEREG(STK[I]) ;
      END (* FOR I := FPARM.. *) ;

    IF OPNDTYPE IN [FORT,FBOOL,FINT,FREAL] THEN
      BEGIN
      GENSI(XMVI,PRMOFST-ADRSIZE,TRG1,128) ;
      (* THIS FLAGS THE END OF PARAMETER ADDRESS LIST TO FORTRAN *)
      GENRX(XST, TRG13, 4, TRG1, 0) ;        (* SAVE R13 *)
"NH"  GENRX(XST,TRG1,8,TRG13,0);  GENRR(XLR,TRG13,TRG1);
"NH"  FPR := 0;  PRMOFST := ALIGN(PRMOFST,REALSIZE);
"NH"  REPEAT  FPR := FPR + 2;
"NH"     IF NOT AVAILFP[FPR] THEN BEGIN
"NH"        GENRX(XSTD,FPR,PRMOFST,TRG13,0);  PRMOFST := PRMOFST+REALSIZE;
"NH"     END;
"NH"  UNTIL FPR = FPCNT;
      GENRX(XLA, TRG1, LCAFTMST, TRG1, 0) ;  (* R1 POINTS TO PARM LIST NOW *)
      END ;

    GENRXLAB(XL,JREG,LBL2,-3) ;   (* BRANCH TO THE CALLED ROUTINE *)
    GENRR(XBALR,RTREG,JREG) ;

    IF OPNDTYPE IN [FORT,FBOOL,FINT,FREAL] THEN
"NH"  BEGIN
"NH"     REPEAT
"NH"        IF NOT AVAILFP[FPR] THEN BEGIN
"NH"           PRMOFST := PRMOFST-REALSIZE; GENRX(XLD,FPR,PRMOFST,TRG13,0);
"NH"        END;
"NH"        FPR := FPR - 2;
"NH"     UNTIL FPR = 0;
"NH"     GENRX(XL,TRG13,4,TRG13,0);
"NH"  END ;

    CLEAR_REG := TRUE ;
    END ;


    PROCEDURE GOTOCSP ;
      BEGIN
      IF NOT CSPREGACTIVE THEN        (* (RE)LOAD PROCADR AND FUNC NUMBER *)
        BEGIN
        LBL3.NAM := '$PASCSP ' ;  LBL3.LEN := 7 ;
        GENRXLAB(XL,TRG15,LBL3,-3) ;
        END ;
      IF CSP <> OLDCSP THEN  GENRX(XLA,TRG1,CSP*4,0,0) ;

      IF NOT FILREGACTIVE THEN
        IF CSP IN [PRES,PREW,PGET,PPUT,PELN,PEOF,PRLN,PWLN,PPAG,
"NH"               PSKP,PLIM,PRDB,PWRB,
                   PRDC,PWRC,PRDI,PWRI,PRDS,PWRS,PRDR,PWRR,PFDF]  THEN
          WITH STK[TOP-1] DO
            BEGIN
            "IF DTYPE <> ADR THEN ERROR(602) ;  (* TEST PHASE ONLY *) "
            IF VRBL THEN  BEGIN  Q1 := MEMADR.DSPLMT ;  P1 := MEMADR.LVL  END
            ELSE          BEGIN  Q1 := FPA.DSPLMT ;  P1 := FPA.LVL  END ;
            BASE(Q1,P1,B1) ;
            IF VRBL THEN  GENRX(XL,FILADR,Q1,B1,P1)
            ELSE          GENRX(XLA,FILADR,Q1,B1,P1) ;
            (* ALTERNATE:  IF NOT VRBL: GENRX(XLA,FILADR,FPA.DSPLMT,0,0) *)
            FILREGACTIVE := TRUE ;
            END ;
      GENRR(XBALR,TRG14,TRG15) ;
      CSPREGACTIVE := TRUE ;  OLDCSP := CSP ;
      END (*GOTOCSP*) ;


  PROCEDURE CALLSTNDRD ;

  (* TO CALL A STANDARD PROCEDURE *)
  (* ---------------------------- *)

    VAR  Q1,P1,B1 : ADRRNG ;



    PROCEDURE FILESETUP(PRMCNT: RGRNG) ;

    (* TO SET UP PARAMETERS FOR THE FILE I/O AND CALL THE I/O ROUTINE *)
    (* -------------------------------------------------------------- *)


      VAR  I : RGRNG ;
           STP: STKPTR;

      BEGIN
      STP := TOP-PRMCNT+1 ;
      TOP := STP ;           (* POINTING TO NEXT AVAILABLE STACK ELEMENT *)
      IF PRMCNT >= 2 THEN  (*POTENTIAL REGISTER CONFLICT*)
        WITH STK[STP+1] DO
          IF VRBL AND (VPA = RGS) AND (RGADR = 2) THEN
            BEGIN  FINDRG ;  GENRR(XLR,NXTRG,2) ;
            AVAIL[NXTRG] := FALSE ;  AVAIL[2] := TRUE ;  RGADR := NXTRG ;
            END ;

      FOR I := 2 TO PRMCNT+1 DO
        BEGIN
        LOAD( STK[STP]) ;
        WITH STK[STP] DO
          IF DTYPE <> REEL THEN

            BEGIN (*THE COMMON CASE*)
            IF RGADR <> I THEN
                IF AVAIL[I]  THEN
                  BEGIN
                  GENRR(XLR,I,RGADR) ;
                  AVAIL[RGADR] := TRUE ;  AVAIL[I] := FALSE ;  RGADR := I ;
                  END
                ELSE  ERROR(259)
            END

          ELSE (*DTYPE = REEL, I.E. WRR*)

            BEGIN
            IF RGADR <> I THEN
              IF AVAILFP[I] THEN  GENRR(XLDR,I,RGADR)
              ELSE  ERROR(259) ;
            AVAILFP[RGADR] := TRUE ;  AVAIL[I] := FALSE ;
            RGADR := I ;  (*KLUDGE TO RELEASE THE FIX. REG. LATER*)
            END ;
        STP := STP+1 ;
        END (* FOR I...*) ;

      GOTOCSP ;
      FOR I := 2 TO PRMCNT+1 DO
        BEGIN  STP := STP-1 ;  AVAIL[STK[STP].RGADR] := TRUE  END ;
      END (*FILESETUP*) ;


    BEGIN (*CALLSTNDRD*)   TOP := TOP-1 ;
    (* -------------- *)

    IF CSP = SPCNT THEN  BEGIN WRITE(' --> ',NMCDE) ;  ERROR(607)  END ;

     CASE CSP OF

      PATN,PEXP,PSIN,PCOS,PLOG,PSQT :
        BEGIN  WRITE(' --> ',NMCDE);  ERROR(607)  END;
¯"""
"NH"  PATN,PEXP,PSIN,PCOS,PLOG,PSQT :
"NH"    WITH STK[TOP] DO
"NH"    BEGIN
"NH"       IF VRBL THEN GETOPERAND( STK[TOP], Q1, P1, B1 )
"NH"       ELSE LOAD( STK[TOP] );
"NH"       IF DATA_SIZE < 4096 THEN
"NH"          GENRX(XLA,TRG1,DATA_SIZE,LBR,0)
"NH"       ELSE BEGIN
"NH"          GENRR(XLR,TRG1,LBR);
"NH"          GENRXLAB(XA,TRG1,SEGSZE,-1);
"NH"       END;
"NH"       GENRX(XST,TRG13,4,TRG1,0);
"NH"       GENRX(XST,TRG1,8,TRG13,0);
"NH"       IF (VPA = RGS) AND DRCT THEN
"NH"          BEGIN
"NH"             GENRX(XSTD,RGADR,LCAFTMST,TRG1,0);
"NH"             GENRX(XLA,TRG0,LCAFTMST,TRG1,0);
"NH"             Q1 := LCAFTMST + REALSIZE;
"NH"          END
"NH"       ELSE BEGIN
"NH"             FINDFP;
"NH"             RGADR := NXTRG;
"NH"             VPA := RGS;  DRCT := TRUE;  DTYPE := REEL;
"NH"             GENRX(XLA,0,Q1,P1,B1);
"NH"             Q1 := LCAFTMST;
"NH"          END;
"NH"       GENRR(XLR,TRG13,TRG1);
"NH"       GENRX(XST,TRG0,Q1,TRG13,0);
"NH"       GENSI(XMVI,Q1,TRG13,128);
"NH"       GENRX(XLA,TRG1,Q1,TRG13,0);
"NH"       B1 := 2;
"NH"       REPEAT   (*  SAVE ALL ACTIVE F.P. REGS  *)
"NH"          IF B1 <> RGADR THEN IF NOT AVAILFP[B1] THEN
"NH"          BEGIN
"NH"             Q1 := Q1 + REALSIZE;  GENRX(XSTD,B1,Q1,TRG13,0);
"NH"          END;
"NH"          B1 := B1 + 2;
"NH"       UNTIL B1 > FPCNT;
"NH"       WITH LBL2 DO
"NH"          IF CSP = PATN THEN
"NH"             BEGIN  NAM := 'DATAN   ';  LEN := 5;  END
"NH"          ELSE IF CSP = PSQT THEN
"NH"             BEGIN  NAM := 'DSQRT   ';  LEN := 5;  END
"NH"          ELSE BEGIN
"NH"             NAM := 'D       ';  LEN := 4;
"NH"             FOR B1 := 1 TO 3 DO
"NH"                NAM[B1+1] := CSPTBL[CSP,B1];
"NH"          END;
"NH"       GENRXLAB(XL,JREG,LBL2,-3);
"NH"       GENRR(XBALR,RTREG,JREG);
"NH"       B1 := FPCNT;
"NH"       REPEAT   (* RELOAD ACTIVE FP REGS  *)
"NH"          IF B1 <> RGADR THEN IF NOT AVAILFP[B1] THEN
"NH"          BEGIN
"NH"             GENRX(XLD,B1,Q1,TRG13,0);  Q1 := Q1 - REALSIZE;
"NH"          END;
"NH"          B1 := B1 - 2;
"NH"       UNTIL B1 = 0;
"NH"       GENRX(XL,TRG13,4,TRG13,0);
"NH"       GENRR(XLDR,RGADR,0);
           FPA := ZEROBL;  VPA := RGS;  DRCT := TRUE;
"NH"       TOP := TOP + 1;  OLDCSP := PSIO;
"NH"       CSPREGACTIVE := FALSE;
"NH"    END (* PATN,PEXP,... *) ;
"""ò
"NH"  PCLK :
"NH"    WITH STK[TOP] DO
"NH"    BEGIN
"NH"       LOAD(STK[TOP]);
"NH"       GENRR(XLR,0,RGADR);
"NH"       GOTOCSP;
"NH"       GENRR(XLR,RGADR,0);
"NH"       TOP := TOP + 1;  OLDCSP := PSIO;
"NH"    END (* PCLK *) ;
"NH"
"NH"  PMSG :
"NH"    BEGIN
"NH"       LOAD( STK[TOP-1] );
"NH"       WITH STK[TOP-1] DO
"NH"       BEGIN
"NH"          IF RGADR <> 2 THEN
                 IF AVAIL[2] THEN
                    BEGIN  GENRR(XLR,2,RGADR);
                    AVAIL[RGADR] := TRUE;  AVAIL[2] := FALSE;
                    RGADR := 2;
                    END
                 ELSE  ERROR(259);   (* ASSUMING THE CURRENT SIMPLE FORMAT *)
"NH"       END;
"NH"       LOAD( STK[TOP] );
"NH"       WITH STK[TOP] DO
"NH"       BEGIN
"NH"          IF RGADR <> 3 THEN
                 IF AVAIL[3] THEN
                    BEGIN  GENRR(XLR,3,RGADR);
                    AVAIL[RGADR] := TRUE;  AVAIL[3] := FALSE;
                    RGADR := 3;
                    END
                 ELSE  ERROR(259);
"NH"       END;
"NH"       GOTOCSP;
"NH"       AVAIL[2] := TRUE;  AVAIL[3] := TRUE;
           TOP := TOP-1;
"NH"    END (* PMSG *) ;

      PXIT :
        WITH STK[TOP] DO
          BEGIN  LOAD(STK[TOP]) ;  AVAIL[RGADR] := TRUE ;
          IF RGADR <> 2 THEN GENRR(XLR,2,RGADR) ;
          GOTOCSP ;
          END (*PXIT*) ;

      PTRP :
        BEGIN
        WITH STK[TOP] DO
          IF (NOT DRCT) OR (DTYPE <> ADR) THEN ERROR(602)
          ELSE
            BEGIN  GETOPERAND(STK[TOP],Q1,P1,B1)  ;
            IF VRBL THEN
              IF VPA = MEM THEN  GENRX(XL,1,Q1,B1,P1)
              ELSE  BEGIN  GENRR(XLR,1,RGADR) ; AVAIL[RGADR] := TRUE END
            ELSE  GENRX(XLA,1,Q1,B1,P1) ;
            END ;
        TOP := TOP-1 ;
        WITH STK[TOP] DO
          IF NOT DRCT THEN ERROR(602)
          ELSE
            IF NOT VRBL THEN GENRXLIT(XL,0,FPA.DSPLMT,0)
            ELSE
              BEGIN  GETOPERAND(STK[TOP],Q1,P1,B1) ;
              IF VPA = MEM THEN  GENRX(XL,0,Q1,B1,P1)
              ELSE  BEGIN GENRR(XLR,0,RGADR) ;  AVAIL[RGADR] := TRUE ; END ;
              END ;
        LBL3.NAM := '$PASTRAP' ;  LBL3.LEN := 8 ;  GENRXLAB(XL,JREG,LBL3,-3) ;
        GENRR(XBALR,RTREG,JREG) ;
        END (*PTRP*) ;

      PSIO :
          BEGIN
          IF NOT AVAIL[FILADR] THEN  IF FILECNT = 0 THEN  ERROR(259) ;
          AVAIL[FILADR] := FALSE ;
          FILREGACTIVE := FALSE ;  FILECNT := FILECNT+1 ;
          OLDCSP := PSIO ;
          TOP := TOP+1 ;    (* TO CANCEL OUT PREVIOUS SUBTRACT OPERATION *)
          END (*PSIO*) ;

      PEIO :
        BEGIN    (* RELEASE FILE ADR REG ETC. *)
        FILECNT := FILECNT-1 ;
        IF FILECNT = 0 THEN AVAIL[FILADR] := TRUE ;
        FILREGACTIVE := FALSE ;  OLDCSP := PEIO ;
        (* TOP := TOP-1 IS DONE AT ENTRY TO CALLSTNDRD  *)
        END (*PEIO*) ;

      PELN,PEOF :
        BEGIN  FILESETUP(0) ;
        FINDRG ;  GENRR(XLR,NXTRG,TRG0) ; (*LOAD THE RETURNED VALUE*)
        WITH STK[TOP-1] DO   (* RETURNED VALUE REPACES THE FILA ADR ENTRY *)
          BEGIN   DTYPE := BOOL ;  VRBL := TRUE ;  DRCT := TRUE ;
          FPA := ZEROBL ;  VPA := RGS ;  RGADR := NXTRG ;
          TOP := TOP+1 ;     (* THIS WILL BE CORRECTED BY THE PENDING 'EIO' *)
          END
        END (*PELN.PEOF*) ;

      PGET,PPUT,PRLN,PWLN,PRES,PREW,PPAG :
        BEGIN
        FILESETUP(0) ;
        END ;

      PRDC,PRDI,PRDR,PFDF,PSKP,PLIM,PRDB :
        BEGIN
        FILESETUP(1) ;
        END ;

      PRDS,PWRC,PWRI,PWRB :
        BEGIN
        FILESETUP(2) ;
        END ;

      PWRS,PWRR :
        BEGIN
        FILESETUP(3) ;
        END

      END (*CASE CSP...*) ;

    END (*CALLSTNDRD*) ;


    PROCEDURE COPERATION ;

    (* CONTROL AND BRANCH INSTRUCTIONS *)
    (* ------------------------------- *)


    PROCEDURE MKLBL(VAR LBL: PLABEL; Q: ADRRNG) ;

      (* ASSUMES     0 <= Q <= 9999999 *)
      VAR  I :  1..8 ;

      BEGIN
      I  := FLDW(Q)+1 ;
      LBL.NAM := 'L       ' ;  LBL.LEN := I ;
        REPEAT
        LBL.NAM[I] := CHR( (Q MOD 10) + ORD('0') ) ;  Q := Q DIV 10 ;
        I := I-1 ;
        UNTIL Q = 0 ;
      END (* MKLBL *) ;

    PROCEDURE ADDLNP(PCDIF : INTEGER) ;

    (* TO ADD A (SOURCE) LINE POINTER TO THE POINTER TABLE *)
    (* --------------------------------------------------- *)

    BEGIN
    IF NXTLNP < MXLNP THEN
      IF LASTPCDIF >= 0 THEN
        BEGIN  CODE[MXCODE+NXTLNP] := LASTPCDIF*SL8 + PCDIF ;
        NXTLNP := NXTLNP+1 ;
        LASTPCDIF := -1 ;
        END
      ELSE  LASTPCDIF := PCDIF ;
    END (*ADDLNP*) ;


    PROCEDURE UPDLNTBL(PCDIF : ICRNG) ;

    (* TO UPDATE LINE POINTER TABLE FOR THE RUN TIME DEBUG OPTION *)
    (* ---------------------------------------------------------- *)

      BEGIN
      IF PCDIF >= 250 THEN (* ENTER ESCAPE MODE *)
        BEGIN
        ADDLNP(254 (*ESCAPE CHAR*) ) ;
        ADDLNP(PCDIF DIV 256) ;   ADDLNP(PCDIF MOD 256) ;
        END
      ELSE
        ADDLNP(PCDIF) ;
      END ;



    PROCEDURE INIT_CSECT ;

    (* TO INITIALIZE OBJECT CODE TABLES AND POINTERS *)
    (* --------------------------------------------- *)

    VAR  I : LBLRNG ;   J : INTEGER ;

      BEGIN

      FOR I := 0 TO LBLCNT DO
        WITH LBLTBL[I] DO
          BEGIN  DEFINED := FALSE ;  LNK := 0  END ;

      INTTBL[0].LNK := 0 ;  NXTINT := 0 ;
      DBLTBL[0].LNK := 0 ;  NXTDBL := 0 ;
      PRCTBL[0].NAME := LBL1.NAM ;  PRCTBL[0].LNK := 0 ; PRCTBL[1].LNK := 0 ;
      NXTPRC := 1 ;  NXTSTR := 0 ;
      PC := 0 ;  NXTSLOC := MXCODE ;  MINLBL := LBLMAP(SEGSZE.NAM);
      LASTPC := 0 ;  LASTPCDIF := -1 ;
      J := 0 ;
      IF CURLVL = 1 THEN (*MAINBLK*)  J := HDRLNGTH ;
      GENRX(XBC,ANYCND,12+J,0,JREG) ;          (* SKIP AROUND CSECT NAME *)
      PC := 6+(J DIV 2) ;  (* PRECEEDING 'GENRX' USES THE VALUE OF 'PC' *)
      CODE[2] := (7+J)*SL8 + "EBCDIC["ORD(CURPNAME[1])"]" ;  (*ID LENGTH FIELD*)

      FOR I := 1 TO 3 DO
        CODE[I+2] := "EBCDIC[" ORD(CURPNAME[I*2]) "]" * SL8 +
                     "EBCDIC[" ORD(CURPNAME[2*I+1]) "]" ;
      (*CODE[5] := 0 ;     PROC SIZE FIELD SET BY THE EPILOG IF NEEDED *)

      FOR I := 1 TO J DIV 2 DO
        CODE[5+I] := ORD(PROGHDR[I*2-1]) * SL8 + ORD(PROGHDR[I*2]) ;

      CODE[MXCODE] := CURPNO;  (* UNIQUE PROC NO *)
      IF DEBUG THEN
        BEGIN
        CODE[MXCODE+1] := LASTLN ;
        FOR I := 2 TO 7 DO
          CODE[MXCODE+I] := "EBCDIC[" ORD(CURPNAME[I*2-3]) "]"*SL8 +
                            "EBCDIC[" ORD(CURPNAME[I*2-2]) "]" ;
        NXTLNP := 8 ;
        END
      ELSE NXTLNP := 0 ;

      END (*INIT_CSECT*) ;


    PROCEDURE GEN_CSECT ;

    (* TO MERGE LITERAL POOLS AND GENERATE ONE OBJECT MODULE FOR THIS PROC *)
    (* ------------------------------------------------------------------- *)

      CONST  XESD    = 46523076   ;  (*EBCDIC FOR  2×'ESD' *)
             XTXT    = 48490467   ;  (*               TXT  *)
             XRLD    = 47829956   ;  (*               RLD  *)
             XEND    = 46519748   ;  (*               END  *)

             BLNK1   = 64         ;  (* EBCDIC FOR ' '     *)
             BLNK2   = 16448      ;  (*            '  '    *)
             BLNK3   = 4210752    ;  (*            '   '   *)
             BLNK4   = 1077952576 ;  (*            '    '  *)

      VAR  I, J      : -1000..1000 ;
           TPC, QPC, OBJEND : INTEGER ;
           LNGTH     : STRLRNG  ;
           VSL16     : INTEGER ;

           BLNK80    : ARRAY[1..80] OF CHAR ;

           CARD      : RECORD  CASE DUMMY : INTEGER OF
                       1: (C : ARRAY[1..80] OF CHAR) ;   (*CHAR CARD IMAGE*)
                       2: (I : ARRAY[1..20] OF INTEGER)  (*INT. CARD IMAGE*)
                       END ;


      BEGIN  (*GEN_CSECT*)

      (* PROCESS REAL/SET POOL *)



      TPC := PC ;  IF ODD(TPC) THEN  TPC := TPC+1 ;  (* WORD ALIGNMENT *)
      IF NXTDBL > 0 THEN
        IF (TPC MOD 4) <> 0 THEN  TPC := TPC+2 ;     (* DOUBLE WORD ALIGNMENT *)
      FOR QPC := PC TO TPC-1 DO  CODE[QPC] := 0 ;
      PC := TPC ;

      FOR I := 0 TO NXTDBL-1 DO
        WITH DBLTBL[I] DO
          BEGIN   TPC := LNK ;  LNK := BASE_DSPLMT(PC) ;
          REPEAT  QPC := CODE[TPC] ;  CODE[TPC] := LNK ;  TPC := QPC
          UNTIL TPC = 0 ;
          I_S_R.S := VAL ;
          CODE[PC] := I_S_R.I1 DIV SL16 ;  CODE[PC+1] := I_S_R.I1 MOD SL16 ;
          CODE[PC+2] := I_S_R.I2 DIV SL16 ;  CODE[PC+3] := I_S_R.I2 MOD SL16 ;
          PC := NEXTPC(4) ;
          END (* FOR..., WITH... *) ;

      (* PROCESS EXTERNAL REFERENCES  *)


      FOR I := 0 TO NXTPRC-1 DO
        WITH PRCTBL[I] DO
          IF LNK > 0 THEN
            BEGIN   TPC := LNK ;  LNK := BASE_DSPLMT(PC) ;
              REPEAT
              QPC := CODE[TPC] ;  CODE[TPC] := LNK ;  TPC := QPC ;
              UNTIL TPC = 0 ;
            LNK := PC ;  " WRITELN(' *** ADCON#, ADDR.',I:3,PC*2) ; "
            CODE[PC] := 0 ;  CODE[PC+1] := 0 ;  PC := NEXTPC(2) ;
            END (* FOR..., IF...,  WITH... *) ;

      (* PROCESS INTEGER (FULL/HALF) POOL *)


      IF LBLTBL[MINLBL].LNK <> 0 THEN
        WITH INTTBL[NXTINT] DO
          BEGIN    (* 'Q' IS THE SIZE OF THE CURRENT ROUTINE'S DATA AREA *)
          VAL := Q ;  LNK := LBLTBL[MINLBL].LNK ;  NXTINT := NXTINT+1
          END ;

      FOR I := 0 TO NXTINT-1 DO
        WITH INTTBL[I] DO
          BEGIN  TPC := LNK ;  LNK := BASE_DSPLMT(PC) ;
         "  WRITELN(' *** I, PC*2, INTTBL[I].VAL, .LNK',I,PC*2,VAL,TPC);"
            REPEAT  " WRITELN(' *** TPC,QPC',TPC,QPC) ;   "
            QPC := CODE[TPC] ; CODE[TPC] := LNK ;
            IF CODE[TPC-1] < 0 THEN
              BEGIN  CODE[TPC] := LNK+2 ;  CODE[TPC-1] := -CODE[TPC-1]  END ;
            TPC := QPC
            UNTIL TPC = 0 ;

          CODE[PC] := VAL DIV SL16 ;  CODE[PC+1] := VAL MOD SL16 ;
          PC := NEXTPC(2) ;
          END (* FOR..., WITH... *) ;

      (* PROCESS STRING POOL *) ;


      TPC := PC ;
      IF DEBUG THEN    (* WORD ALIGN THE CSECT *)
        BEGIN

        IF ODD(NXTSLOC) THEN
          BEGIN  NXTSLOC := NXTSLOC-1 ;
          CODE[NXTSLOC] := 0 ;  TPC := TPC+1
          END ;

        CODE[5] := (PC+MXCODE-NXTSLOC)*2 ;  (* PROC SIZE FIELD *)
        ADDLNP(255) ; ADDLNP(255) ;         (* COMPLETE LN TABLE *)
        CODE[MXCODE+NXTLNP] := 0 ;
        END ;

      IF  NOT LARGE_PROC  THEN
        IF PC+MXCODE-NXTSLOC > (MXCODE DIV 2) THEN
          ERROR(609 (*SHORT PROC TOO LONG*) ) ;

      IF PC > NXTSLOC THEN  BEGIN  ERROR(253) ;  EXIT(253)  END ;
        FOR I := NXTSTR-1 DOWNTO 0 DO
          WITH STRTBL[I] DO
            BEGIN
            CODE[LNK] := BASE_DSPLMT(TPC) ;  TPC := TPC+(LNGTH+1) DIV 2
            END ;
      (* STRING POOL IS NOW CODE[NXTSLOC]..CODE[MXCODE-1] *)

   (*  WRITELN(' ***',PC:5,NXTSLOC:6,NXTINT:4,NXTDBL:4,NXTSTR:4,
                      NXTPRC:4,MINLBL:8) ;  *)

      (* OUTPUT THE OBJECT CODE   *)


      FOR I := 1 TO 80 DO  BLNK80[I] := CHR(BLNK1) ;

      (* OUTPUT THE 'ESD' ENTRIES *)

      CARD.C := BLNK80 ;
      CARD.I[1] := XESD ;  CARD.I[4] := BLNK2*SL16+ 01 ;  (*FIRST ESDID*)
      I := 0 ;  LNGTH := 0 ;
"NH"  IF CURLVL = 1 THEN  (* IF $MAINBLK, ADD $PASENT TO ESD ITEMS *)
"NH"     BEGIN  PRCTBL[NXTPRC].NAME := '$PASENT ';
"NH"            NXTPRC := NXTPRC + 1;                 END;

        REPEAT  (* SCAN OVER ALL ENTRIES (OF PROCEDURES) *)

        WITH PRCTBL[I] DO
          FOR J := 1 TO 8  DO CARD.C[16+LNGTH+J] := "CHR(EBCDIC["NAME[J]"])" ;

        I := I+1 ;                       (*THIS IS NOW THE ESDID*)
        CARD.I[LNGTH DIV 4 +7] := 0 ;     (*TYPE-ADDRESS (SD) FIELD*)
        IF I = 1 THEN  CARD.I[8] := BLNK1*SL24+ 00   (*CSECT LNGTH ON END CARD*)
        ELSE
          BEGIN
          CARD.C[25+LNGTH] := CHR(2);  CARD.I[8+(LNGTH DIV 4)] := BLNK4 (* ER *)
          END ;
        LNGTH := LNGTH+16 ;

        IF (LNGTH >= 48) OR (I >= NXTPRC) THEN     (*TIME TO OUPUT THE BUFFER*)
          BEGIN
          CARD.I[3] := BLNK2*SL16 +LNGTH ;
          FOR J := (LNGTH DIV 4)+5 TO 16 DO  CARD.I[J] := BLNK4 ;
          WRITE(PRR,CARD.C) ;
          CARD.I[4] := BLNK2*SL16+ I+1  (*NEXT ESDID*) ;  LNGTH := 0 ;
          END ;

        UNTIL I >= NXTPRC ;

"NH"  IF CURLVL = 1 THEN NXTPRC := NXTPRC - 1;  (* UNDO EARLIER ADDITION *)

      (* OUTPUT THE 'TXT' CARDS   *)

      CARD.I[1] := XTXT ;  I := 0 ;  TPC := 0 ;  QPC := 0 ;
      OBJEND := MXCODE+NXTLNP ;

      CARD.I[4] := BLNK2*SL16 +01  ;        (*ESDID OF CURRENT CSECT *)
      LNGTH := 0 ;

        REPEAT
        CARD.I[LNGTH+5] := CODE[TPC]*SL16 + CODE[TPC+1] MOD SL16  ;
        LNGTH := LNGTH+1 ;  TPC := TPC+2 ;
        IF TPC = PC THEN TPC := NXTSLOC ;   (*STRING POOL FOLLOWS*)

        IF (LNGTH >= 14) OR (TPC >= OBJEND) THEN
          BEGIN
          CARD.I[2] := BLNK1*SL24 +QPC ;      (*FIRST BYTE ADDRESS*)
          CARD.I[3] := BLNK2*SL16 +LNGTH*4 ;    (* # OF TXT DATA BYTES*)
          FOR J := LNGTH+5 TO 18 DO  CARD.I[J] := BLNK4 ;
          WRITE(PRR,CARD.C) ;
          QPC := QPC+LNGTH*4 ;  LNGTH := 0 ;
          END (* IF... *) ;

        UNTIL TPC >= OBJEND ;

      (* OUTPUT THE 'RLD' ENTRIES *)


      CARD.C := BLNK80 ;  CARD.I[1] := XRLD ;  I := 0 ;  LNGTH := 0 ;

        REPEAT         (* SCAN OVER ALL EXTERNAL REFERENCES *)

        WITH PRCTBL[I] DO
          BEGIN   I := I+1 ;  (* I NOW BECOMES ESDID FOR THE CURRENT ENTRY *)
          IF LNK > 0 THEN     (* IMPLIES RECURSIVE CALL *)
            BEGIN  "WRITELN(' *** ADCON#, ADDRESS:',I:3,LNK*2) ; "
            CARD.I[LNGTH+5] := I*SL16+01 ;      (* 'P#', 'R#' FIELDS  *)
            CARD.I[LNGTH+6] := 28*SL24+LNK*2 ;  (* ADCON DISPLACEMENT *)
            LNGTH := LNGTH+2 ;
            IF (LNGTH >= 14) OR (I >= NXTPRC) THEN      (* OUTPUT THE BUFFER *)
              BEGIN  CARD.I[3] := BLNK2*SL16 +LNGTH*4 ; (* # OF RLD DATA BYTES*)
              FOR J := LNGTH TO 14 DO  CARD.I[LNGTH+5] := BLNK4 ;
              WRITE(PRR,CARD.C) ;  LNGTH := 0 ;
              END (* IF(LNGTH >... *) ;
            END (* IF LNK > 0 *) ;

          END (* WITH PRCTBL... *)

        UNTIL  I >= NXTPRC ;

      (* OUTPUT 'END' CARD WITH CSECT LENGTH *)


      CARD.C := BLNK80 ;
      CARD.I[1] := XEND ;  CARD.I[8] := QPC ;

"NH"  IF CURLVL = 1 THEN BEGIN  (* PUT ENTRY POINT ON END CARD *)
"NH"     CARD.I[2] := BLNK1*SL24;
"NH"     CARD.I[4] := BLNK2*SL16 + NXTPRC + 1;  END;
"NH"  WRITE(PRR,CARD.C);
      IF ASMVERB THEN
"NH"  WRITELN('0','****  PROC: ':16,PRCTBL[0].NAME,'  LEN:':6,PROC_SIZE:4,QPC:5,
              ' (P_STMTS, BYTES), EXT. REFS:',NXTPRC-1:3,', DOUBLES:',NXTDBL:3,
              ', INTS:',NXTINT:4, ', STRINGS:',NXTSTR:3,
              ', CHARS:',(MXCODE-NXTSLOC)*2:4 ) ;
      TOTALBYTES := TOTALBYTES+QPC ;
      END (*GEN_CSECT*) ;
      (*_________________________________________________________________*)


      BEGIN   (*COPERATION*)

        CASE OPC OF

        (* P_MACHINE PSEUDO OPS *)

        PLAB :
          BEGIN  "NEWLINE := TRUE ;"
          CASE_FLAG := FALSE ;   (* END OF BRANCH TABLE *)
          IF ASM THEN  WRITELN(PRR,LBL1.NAM,' DS 0H')
          ELSE  UPD_LBLTBL(PC, LBLMAP(LBL1.NAM), TRUE (*LABEL DEFINITION*)) ;

          CASE_FLAG := OLDOPC = PDEF ;   (* START BRANCH TABLE *)
          CSPREGACTIVE := FALSE ;
          END (*PLAB*) ;

        PLOC :
          BEGIN
          IF CURPNO >= 0 THEN
            BEGIN
            IF ASM THEN
              BEGIN  (* DO NOT PUT LINE # IN DANGAROUS PLACES *)
              IF NOT CASE_FLAG THEN
                 IF OLDOPC <> PDEF THEN WRITELN(PRR,' LOC', Q:7)
              END
            ELSE
              IF DEBUG THEN  (*FILL THE ENTRIES OF LINE PTR TABLE*)
              " CODE[NXTLOC] := PC*2 ; "
                FOR I := LASTLN TO Q-1 DO
                  BEGIN
                  UPDLNTBL(PC-LASTPC) ;
                  LASTPC := PC ;
                  END ;

         "  NXTLOC := NXTLOC+1 ; "
            END ;
          LASTLN := Q ;
          OPC := OLDOPC ;   (* TO TREAT THIS AS A NOOP *)
(*+####   PSEUDO_OP := TRUE ;                                            ####+*)
          END (*PLOC*) ;

        PDEF :
          BEGIN
          IF ASM THEN
            BEGIN  WRITELN(PRR, LBL1.NAM, ' DEF', Q:7) ;
            IF OLDOPC = PRET THEN  WRITELN(PRR,' PEND') ;
            END
          ELSE
            IF OLDOPC = PRET THEN
              BEGIN  GEN_CSECT ;  CURPNO := -1 ; END
            ELSE (* CTR/CASE EXPRESSION RANGE, PUT BOUNDS IN 'CONSTANT' TABLE *)
              UPD_INTTBL(LBLTBL[LBLMAP(LBL1.NAM)].LNK,Q,0) ;
          END (*DEF*) ;

        PCHK :
            WITH STK[TOP-1] DO
            IF VRBL THEN   (* GENERATE CODE FOR RUN TIME CHECK *)
              BEGIN
              IF NOT AVAIL[2] THEN
                IF NOT ((VPA = RGS) AND (RGADR = 2)) THEN
                  BEGIN   J := 0 ;  (* CLEAR GPR 2 *)

                  FOR I := TOP-2 DOWNTO 1 DO
                    WITH STK[I] DO
                      IF VRBL THEN
                        IF (NOT DRCT) OR (DTYPE <> REEL) THEN
                          IF (VPA = RGS) AND (RGADR = 2) THEN  J := I ;

                  IF J = 0 THEN  ERROR(259)
                  ELSE
                    WITH STK[J]  DO
                      BEGIN   FINDRG ;  (* TRADE GPR2 FOR ANOTHER ONE *)
                      IF DTYPE = PSET THEN  IF DRCT THEN  ERROR(259) ;
                      GENRR(XLR,NXTRG,2) ;  (* THIS FREES REG 2 *)
                      RGADR := NXTRG ;  AVAIL[2] := TRUE ;
                      END ;
                  END ;

              LOAD(STK[TOP-1]) ;
              IF NOT (DTYPE IN [INT,ADR,CHRC,BOOL]) THEN  ERROR(604) ;
              IF RGADR <> 2 THEN  (* LOAD RVALUE IN GPR 2 *)
                BEGIN
                IF NOT AVAIL[2] THEN ERROR(259) ;
                GENRR(XLR,2,RGADR) ;  AVAIL[RGADR] := TRUE ;
                RGADR := 2 ;  AVAIL[2] := FALSE ;
                END ;

              I_S_R.I1 := P ;  I_S_R.I2 := Q ;

                CASE OPNDTYPE OF

                INT :
                       BEGIN  GENRSLIT(XLM,TRG0,TRG1,I_S_R.S) ;  (*LOAD BOUNDS*)
                       GENRX(XBAL,RTREG,RNGCHK,GBR,0)
                       END ;
                INX :
                       BEGIN  GENRSLIT(XLM,TRG0,TRG1,I_S_R.S) ;  (*LOAD BOUNDS*)
                       GENRX(XBAL,RTREG,INXCHK,GBR,0) ;
                       END ;

                ADR :  IF P < 0 THEN GENRX(XBAL,14,PTACHK,GBR,0)
                       ELSE  GENRX(XBAL,14,PTRCHK,GBR,0) ;

                PSET : GENRX(XBAL,RTREG,SETCHK,GBR,0) ;

                PROC : BEGIN  GENRSLIT(XLM,TRG0,TRG1,I_S_R.S) ;
                       GENRX(XBAL,RTREG,PRMCHK,GBR,0) ;
                       END

                END (* CASE OPNDTYPE OF .. *)  ;
              CSPREGACTIVE := FALSE ; OLDCSP := PSIO ;
              (* INDICATE LOSS OF R1,R15 *)
              END (* IF VRBL *)

            ELSE (* ^ VAR,  I.E. CHECK A CONSTANT EXPRESSION *)

              BEGIN
              IF (FPA.DSPLMT < P) OR (FPA.DSPLMT > Q) THEN
                BEGIN
                ERROR(302) ;
                WRITELN(OUTPUT, '****':9, FPA.DSPLMT:9,
                                ' IS NOT IN THE RANGE:', P:9, Q:10 ) ;
                END ;
              END (*PCHK*) ;


        (* BRANCH/CONTROL INSTRUCTIONS *)

        PUJP :
          GENRXLAB(XBC,15,LBL2,0) ;

        PFJP :
          BEGIN  TOP := TOP-1 ;
          IF (BRCND >= 0) AND (NOT NEG_CND) THEN   (* COND. CODE IS ALIVE *)
            BRCND := 15-BRCND   "BRMSK[OLDOPC]"
          ELSE
          WITH STK[TOP] DO
            BEGIN  "IF DTYPE <> BOOL THEN  ERROR(605) ; "
            IF VRBL THEN
              BEGIN

              IF DRCT AND (VPA = MEM) THEN
                BEGIN  GETOPERAND(STK[TOP],Q1,P1,B1) ;
                IF B1 > 0 THEN  IF P1 > 0 THEN  GENRR(XAR,P1,B1)
                                ELSE  P1 := B1 ;
                GENSI(XTM,Q1,P1,1) ;
                BRCND := 8 (* BZ *) ;  IF NEG_CND THEN BRCND := 1 (* BO *) ;
                END
              ELSE
                BEGIN  LOAD(STK[TOP]) ;
                GENRR(XLTR,RGADR,RGADR) ;
                BRCND := EQUCND ;  IF NEG_CND THEN BRCND := NEQCND ;
                END ;

              FREEREG(STK[TOP]) ;
              END
            ELSE (*NOT VRBL*)
              IF FPA.DSPLMT = 0 THEN BRCND := ANYCND (*TAKE THE BRANCH*)
              ELSE  BRCND := NOCND ; (*DO NOT BRANCH*) ;
            IF VRBL AND (VPA = RGS) THEN AVAIL[RGADR] := TRUE ;
            END (*WITH STK...*) ;

          GENRXLAB(XBC,BRCND,LBL2,0) ; "NEWLINE := TRUE ;"
          BRCND := -1 ;  NEG_CND := FALSE ;  (* CLEAR C.C./ NEGATE FLAGS *)
          END (*PFJP*) ;

        PXJP :
          (* LBL2   = LOWER BOUND, CASE EXPRESSION       *)
          (* LBL2+1 = UPPER BOUND,                       *)
          (* LBL2+2 = BRANCH TABLE LABEL                 *)
          (* LBL2+3 = CASE EXIT LABEL                    *)

          BEGIN  TOP := TOP-1 ;  LOAD(STK[TOP]) ;
          WITH STK[TOP] DO
            BEGIN
            Q := LBLMAP(LBL2.NAM) ;
            MKLBL(LBL1,Q+1) ;  MKLBL(LBL3,Q+3) ;
            GENRXLAB(XC,RGADR,LBL1,-1) ;   (* CHECK AGAINST UPPER BOUND *)
            GENRXLAB(XBC,GRTCND,LBL3,0) ;  (* GO TO CASE_EXIT IF OUT OF RANGE *)
            GENRXLAB(XS,RGADR,LBL2,-1) ;   (* OTHERWISE SUBTRACT LOWER BOUND *)
            GENRXLAB(XBC,LESCND,LBL3,0) ;  (* CASE_EXIT IF OUT OF RANGE *)
            MKLBL(LBL3,Q+2) ;

            IF ASM THEN
              BEGIN
              GENRR(XSLA,RGADR,2) ;  GENRXLAB(XBC,ANYCND,LBL3,RGADR) ;
              END
            ELSE
              BEGIN
              GENRR(XAR,RGADR,RGADR) ;  (* CONV. INDEX TO TABLE OFFSET *)
              GENRXLAB(XLH,JREG,LBL3,RGADR) ;  GENRX(XBC,ANYCND,0,JREG,PBR1) ;
              END ;

            AVAIL[RGADR] := TRUE ;
            END (* WITH STK[TOP] DO *) ;
          END (*PXJP*) ;

        PMST :
          BEGIN   CSTOP := CSTOP+1 ;  CALSTK[CSTOP] :=  TOP ;
          END ;

        PCUP :
          BEGIN
          CALLSUB(CALSTK[CSTOP], TOP-1) ;  (* SET UP CALL PARAMETERS *)
          TOP := CALSTK[CSTOP] ;

          IF OPNDTYPE <> PROC THEN (*ADJUST STACK TOP*)
            IF OPNDTYPE <> FORT THEN

            WITH STK[TOP] DO
              BEGIN   VRBL := TRUE  ; DRCT := TRUE ;
              FPA := ZEROBL ; VPA := RGS ;

              CASE OPNDTYPE OF

              ADR,INT :
                BEGIN  FINDRG ;
                GENRX(XL,NXTRG,FNCRSLT,TRG1,0)
                END ;

              BOOL,CHRC :
                BEGIN  FINDRG ;
                GENRR(XSR,NXTRG,NXTRG) ;
                GENRX(XIC,NXTRG,FNCRSLT,TRG1,0) ;
                END ;

              PSET :
                BEGIN  FINDRP ;
                GENRS(XLM,NXTRG,NXTRG+1,FNCRSLT,TRG1) ;
                END ;

              REEL :
                BEGIN  FINDFP ;  GENRX(XLD,NXTRG,FNCRSLT,TRG1,0)  END;

              FBOOL:
                BEGIN  FINDRG ;  OPNDTYPE := BOOL ;
                GENRR(XLR,NXTRG,0)    (*COPY RESULT FROM REGISTER ZERO*)
                END;

              FINT :
                BEGIN  FINDRG ;  OPNDTYPE := INT ;
                GENRR(XLR,NXTRG,0)    (*COPY RESULT FROM REGISTER ZERO*)
                END ;

              FREAL:
                BEGIN  FINDFP ;  OPNDTYPE := REEL ;
                GENRR(XLDR,NXTRG,0)    (*COPY RESULT FROM REGISTER ZERO*)
                END ;

              END (*CASE OPNDTYPE*) ;

            RGADR := NXTRG ;  DTYPE := OPNDTYPE ;  TOP := TOP+1 ;
            END (*WITH STK...*) ;

          CSTOP := CSTOP-1 ;  CSPREGACTIVE := FALSE ;  OLDCSP := PSIO ;
          END (* CUP*) ;

        PENT :
          BEGIN  (* ON ENTRY TRG1 POINTS TO DATA AREA FOR THE CALLED ROUTINE *)
          CURLVL := P ;
          IF ASM  THEN
            BEGIN  WRITELN(PRR,'  PBGN ', CURPNAME, PROGHDR:HDRLNGTH+2) ;
            WRITELN(PRR,LBL1.NAM,' CSECT') ;  WRITELN(PRR,' USING *,',JREG:2) ;
            WRITELN(PRR,' B *+12') ;   (* IN FACT GENRX(XB,15,... *)
            WRITELN(PRR, ' DC AL1(7),CL7''' ,CURPNAME:7, '''') ;
            END (*IF ASM*)
          ELSE   INIT_CSECT ;     (*INITIALIZE NEW CSECT PARAMETERS*)

          IF CALL_HIGHER THEN
          GENRX(XL,TRG0,DISPLAY+4*CURLVL,GBR,0) ; (* TO SAVE DISPLAY[CURLVL] *)

          IF SAVERGS OR (OPNDTYPE <> PROC) THEN
            BEGIN

            IF OS_STYLE THEN
              BEGIN
              GENRS(XSTM,14,12,12,TRG1) ;(*SAVE OLD DISPLAY[CURLVL] & REGS*)
              GENRX(XST,TRG1,8,LBR,0) ; (*FORWARD CHAIN OF SAVE AREAS*)
              GENRX(XST,LBR,4,TRG1,0) ; (*DYNAMIC LINK, ALSO SAVE AREA CHAIN*)
              END
            ELSE  GENRS(XSTM,13,12,8,TRG1) ;  (* SAVE DYNAMIC LINK + REGS *);

            END
          ELSE (*JUST SAVE RETURN ADR. & PROGRAM BASE REGS*)
            BEGIN
            GENRX(XST,LBR,4,TRG1,0) ;   (* SET DYNAMIC LINK *)
            IF CALL_HIGHER THEN  GENRX(XST,TRG0,20,TRG1,0)  ;
            GENRX(XST,RTREG,12,TRG1,0) ; GENRX(XST,PBR1,60,TRG1,0) ;
            IF LARGE_PROC THEN  GENRX(XST,PBR2,64,TRG1,0) ;
            END ;

          IF SAVEFPRS OR (OPNDTYPE = REEL) THEN
            FOR I := 1 TO 3 DO
              GENRX(XSTD,2*I,FPRSAREA+I*8,TRG1,0) ;

          GENRR(XLR,LBR,TRG1) ;     (*UPDATE THE 'MP'*)
          IF CALL_HIGHER THEN
          GENRX(XST,LBR,DISPLAY+4*CURLVL,GBR,0) ; (*UPDATE DISPLAY[CURLVL]*)
          GENRR(XLR,PBR1,JREG) ;  (* SET UP PROGRAM BASE REGISTERS *)
          IF LARGE_PROC THEN  GENRX(XLA,PBR2,4092,PBR1,0) ;

          IF ASM THEN
            BEGIN
            WRITELN(PRR, ' USING ', LBL1.NAM:LBL1.LEN, ',', PBR1:2) ;
            WRITELN(PRR, ' USING 4092+', LBL1.NAM:LBL1.LEN, ',', PBR2:2) ;
            WRITELN(PRR,' DROP',JREG:3) ;
            "NEWLINE := TRUE ;"
            END ;

          IF DEBUG THEN
            BEGIN
            GENRR(XLR,RTREG,JREG) ;   (* SAVE CURR. LOC. FOR ERROR ROUTINE *)
            IF DATA_SIZE < 4096 THEN  GENRX(XLA,TRG1,DATA_SIZE,TRG1,0)
            ELSE  GENRXLAB(XA,TRG1,SEGSZE,-1) ;
            GENRX(XC,TRG1,NEWPTR,GBR,0) ;    (* COMPARE 'SP' AND 'NP' *)
            GENRX(XBC,GEQCND,STKCHK,GBR,0) ; (* BRANCH TO ERROR ? *)
            IF CURLVL = 1 THEN   (*ENTERING MAINBLK, CLEAR STACK/HEAP AREA*)
               BEGIN
               GENRX(XLD, FPR0, CLEARBUF, GBR, 0);   (*GET THE "CLEAR" PATTERN*)
               GENRX(XL, TRG14, NEWPTR, GBR, 0);     (*END OF HEAP*)
               GENRX(XLA, TRG1, FRSTGVAR, GBR, 0);
               GENRR(XSR, TRG14, TRG1);              (*TRG14 <-- BYTE COUNT*)
               IF ASM THEN GENRR(XSRA, TRG14, 3)     (*TRG14 <-- DOUBLE WD CNT*)
               ELSE GENRS(XSRA, TRG14, 0, 3, 0);
               GENRX(XSTD, FPR0, 0, TRG1, 0);
               GENRX(XLA, TRG1, 8, TRG1, 0);
               IF ASM THEN  WRITELN(PRR, ' BCT ', TRG14:2, ',*-8')
               ELSE GENRX(XBCT, TRG14, PC*2-8, PBR1, 0);
               END;
            END ;

          CSPREGACTIVE := FALSE ;

          END (*PENT*) ;

        PRET :  (* FOR FUNCTIONS, WE COULD LOAD THE RESULT IN A REG *)
          BEGIN (*RESTORES DISPLAY[CURLVL] AND MP, THEN RETURNS*)
          IF SAVEFPRS OR (OPNDTYPE = REEL) THEN
            FOR I := 1 TO 3 DO
              GENRX(XLD,2*I,FPRSAREA+I*8,LBR,0) ;

          IF DEBUG AND (CURLVL > 1) AND (DATA_SIZE >80) THEN
            (* CLEAR THE STACK FRAME  *)
            BEGIN
            GENRX(XLD, TRG0, CLEARBUF, GBR, 0) ;  (* THE PATTERN TO CLEAR MEM *)

            IF DATA_SIZE < (4096*8) THEN
              GENRX(XLA, TRG1, (DATA_SIZE-LCAFTMST) DIV 8, 0, 0)
            ELSE
              BEGIN
              GENRXLAB(XL, TRG1, SEGSZE, -1) ;
              GENRXLIT(XS, TRG1, LCAFTMST-REALSIZE, 0) ;
              IF ASM THEN  GENRR(XSRA, TRG1, 3)   (* DIVIDE BY 8 *)
              ELSE  GENRS(XSRA, TRG1, 0, 3, 0)
              END ;

            (* TRG1 HOLDS THE # OF DOUBLE WORDS TO BE CLEARED *)

            GENRR(XSR, TRG15, TRG15) ;          (* ADDRESS/INCREMENT POINTER *)
            GENRR(XBALR, TRG14, 0) ;            (* BEGINING OF CLEAR LOOP *)
            GENRX(XSTD, FPR0, LCAFTMST, LBR, TRG15) ;
            GENRX(XLA, TRG15, REALSIZE, TRG15, 0) ;   (* POINT TO NEXT D_WORD *)
            GENRR(XBCTR, TRG1, TRG14) ;         (* REPEAT UNTIL DONE *)
            END (* IF DEBUG *) ;

          IF SAVERGS OR (OPNDTYPE <> PROC) THEN
            IF OS_STYLE THEN
              BEGIN   GENRS(XLM,14,12,12,LBR) ;   GENRX(XL,LBR,4,LBR,0)  END
            ELSE GENRS(XLM,13,12,8,LBR)    (* UPDATE ALL INCL. LOCAL BASE REG *)
          ELSE  (*RESTORE BASE REGS AND RETURN ADR. ONLY*)
            BEGIN
            IF OPNDTYPE <> PROC THEN GENRR(XLR,TRG1,LBR) ; (* FOR FUNC. RSLT. *)
            IF CALL_HIGHER THEN  GENRX(XL,TRG0,20,LBR,0)  ;
            GENRX(XL,RTREG,12,LBR,0) ;
            GENRX(XL,PBR1,60,LBR,0) ;
            IF LARGE_PROC THEN  GENRX(XL,PBR2,64,LBR,0) ;
            GENRX(XL,LBR,4,LBR,0) ;    (* RESET LOCAL PTR TO PREV ACTIV. REC. *)
            END ;

          IF CALL_HIGHER THEN  GENRX(XST,TRG0,DISPLAY+4*CURLVL,GBR,0) ;
          IF DEBUG AND (CURLVL > 1) THEN        (* CLEAR THE SAVE AREA *)
            BEGIN  I := 80 ;   IF OPNDTYPE <> PROC THEN I := 72 ;
            GENSS(XMVC, I, 0, TRG1, 80, TRG1) ;
            END ;
          GENRR(XBCR,ANYCND,RTREG) ;  RELEASE(HEAPMARK) ;
          END (*PRET*) ;

        PCSP : CALLSTNDRD ;

        PSTP :
"NH"      IF ASM THEN  BEGIN  (* GENERATE ASSEMBLER END CARD *)
"NH"         WRITELN(PRR,' EXTRN $PASENT');
"NH"         WRITELN(PRR,' END   $PASENT');     END

        END (*CASE OPC OF*) ;
      END (*COPERATION*) ;


    PROCEDURE UOPERATION ;

    (* UNARY OPERATIONS *)

      BEGIN

        CASE OPC OF

        PSGS :
          WITH STK[TOP-1] DO
            IF VRBL THEN
            BEGIN  LOAD(STK[TOP-1]) ;
              FINDRP ;
              GENRX(XLA,NXTRG+1,1,0,0) ;  GENRR(XSR,NXTRG,NXTRG) ;
              IF ASM THEN GENRX(XSLDL,NXTRG,0,RGADR,0)
              ELSE  GENRS(XSLDL,NXTRG,0,0,RGADR) ;  AVAIL[RGADR] := TRUE ;
              DTYPE := PSET ;  RGADR := NXTRG ;
              END
            ELSE  (*GENERATE A SET OUT OF THE CONSTANT*)
              BEGIN
              PCNST := [FPA.DSPLMT] ;
              DTYPE := PSET ;
              END (*PSGS*) ;

        PFLT,PFLO :
          BEGIN
            IF OPC = PFLT THEN OPPTR := TOP - 1
              ELSE OPPTR := TOP - 2 ;
            WITH STK[OPPTR] DO
               IF VRBL THEN
               BEGIN
                  LOAD(STK[OPPTR]) ;
                  FINDFP ;
                  GENRX(XX,RGADR,FL2+4,GBR,0) ;  GENRX(XST,RGADR,FL1+4,GBR,0) ;
                  GENRX(XLD,NXTRG,FL1,GBR,0) ;  GENRX(XSD,NXTRG,FL2,GBR,0) ;
                  AVAIL[RGADR] := TRUE ;  DTYPE := REEL ;
                  RGADR := NXTRG ;
               END  (* VRBL *)
               ELSE  (* CONSTANT*)
               BEGIN
                  DTYPE := REEL ;
                  RCNST := FPA.DSPLMT ;
                  J := MXSLNGTH ;
                  SCNSTL := 1 ;
                  NEW(STRPTR) ;

                  IF FPA.DSPLMT < 0 THEN
                     BEGIN  FPA.DSPLMT := -FPA.DSPLMT ;
                     STRPTR@[1] := '-' ;
                     END
                  ELSE  STRPTR@[1] := '+' ;

                  REPEAT
                     STRPTR@[J] := CHR((FPA.DSPLMT MOD 10) + ORD('0')) ;
                     J := J - 1 ;
                     SCNSTL := SCNSTL + 1 ;
                     FPA.DSPLMT := FPA.DSPLMT DIV 10 ;
                  UNTIL ( J < 2 ) OR ( FPA.DSPLMT = 0 ) ;
                  FOR I := 2 TO SCNSTL DO
                     STRPTR@[I] := STRPTR@[I+J-1] ;
                  SCNST := STRPTR
               END  (* CONSTANT -- WITH *)
          END  (* PFLT,PFLO *) ;

"NH"    PTRC, PRND :
          BEGIN
            LOAD(STK[ TOP-1 ]) ;
            FINDRG ;
            WITH STK[ TOP-1 ] DO
            BEGIN
            (*GENRX(XAW,RGADR,FL2,GBR,0) ;  GENRX(XSTD,RGADR,FL3,GBR,0) ;
              GENRX(XL,NXTRG,FL3+4,GBR,0) ;  GENRX(XX,NXTRG,FL2+4,GBR,0) ; *)

"NH"          IF OPC = PRND THEN
                BEGIN   NEW(STRPTR) ;
                (* TO ROUND ADD +/-0.5 AND THEN TRUNCATE *)
                STRPTR@[1] := '0' ;   STRPTR@[2] := '.' ;   STRPTR@[3] := '5' ;
                GENRXDLIT(XLD, TRG0, STRPTR, 3(*STRL*), 0.5) ;  (*TRG0 <-- 0.5*)
                GENRR(XLTDR, RGADR, RGADR) ;         (*SEE IF NEGATIVE OPERAND*)
                IF ASM THEN  WRITELN(PRR, ' BNL *+6')
                ELSE
                  BEGIN   Q := BASE_DSPLMT(PC+3) ;
                  GENRX(XBC, GEQCND, Q MOD SL12, Q DIV SL12, 0) ;
                  END ;
                GENRR(XLCDR, TRG0, TRG0) ;         (* TRG0 <-- -0.5 *)
                GENRR(XADR, RGADR, TRG0) ;
"NH"            END (* OPC = PRND *) ;

              GENRX(XAD,RGADR,FL4,GBR,0) ;  GENRX(XSTD,RGADR,FL3,GBR,0) ;
              GENRX(XL,NXTRG,FL3+4,GBR,0) ;
              AVAILFP[ RGADR ] := TRUE ;
              DTYPE := INT ;
              RGADR := NXTRG
            END (* WITH *)
          END  (* PTRC *) ;

        PNGR :
          WITH STK[ TOP-1 ] DO

            IF VRBL THEN
              BEGIN
              LOAD(STK[TOP-1]) ;  GENRR(XLCDR,RGADR,RGADR)
              "  GETOPERAND(STK[TOP-1],Q1,P1,B1) ;
                 IF (VPA = RGS) AND DRCT THEN
                 BEGIN
                    GENRR(XLDR,0,RGADR) ;
                    GENRR(XSDR,RGADR,RGADR) ;
                    GENRR(XSDR,RGADR,0);
                 END  (* VPA = RGS AND DRCT *)
                 ELSE  (* VPA = MEM OR NOT DRCT *)
                 BEGIN
                    FINDFP ;
                    GENRR(XSDR,NXTRG,NXTRG) ;
                    GENRX(XSD,NXTRG,Q1,B1,P1) ;
                    VPA := RGS ;  RGADR := NXTRG ;
                 END  (* VPA = MEM OR NOT DRCT *)   "
              END  (* VRBL *)
            ELSE  (* CONSTANT *)
              BEGIN
              IF SCNST@[1] = '-' THEN
                BEGIN
                FOR I := 1 TO SCNSTL DO
                  SCNST@[I] := SCNST@[I+1] ;
                SCNSTL := SCNSTL - 1
                END
              ELSE  (* POSITIVE NUMBER *)
                BEGIN
                FOR I := SCNSTL DOWNTO 1 DO
                  SCNST@[I+1] := SCNST@[I] ;
                SCNST@[1] := '-' ;
                SCNSTL := SCNSTL + 1
                END ;
              RCNST := -RCNST ;
              END  (* CONSTANT *)
          (* END  PNGR *) ;

        PNGI :
          WITH STK[TOP-1] DO
            IF VRBL THEN
              BEGIN  LOAD(STK[TOP-1]) ;
              GENRR(XLCR,RGADR,RGADR) ;
              END
            ELSE  FPA.DSPLMT := -FPA.DSPLMT ;

        PABI :
          WITH STK[TOP-1] DO
            IF VRBL THEN
              BEGIN  LOAD(STK[TOP-1]) ;
              GENRR(XLPR,RGADR,RGADR) ;
              END
            ELSE  FPA.DSPLMT := ABS(FPA.DSPLMT) ;

        PABR:
           WITH STK[TOP-1] DO
           BEGIN
              LOAD(STK[TOP-1]) ;
              GENRR(XLPDR,RGADR,RGADR)
           END  (* PABR *) ;

        PSQI :
          WITH STK[TOP-1] DO
          BEGIN  MDTAG := PMPI ;  LOAD(STK[TOP-1]) ;  MDTAG := PBGN ;
          GENRR(XMR,RGADR,RGADR+1) ;  AVAIL[RGADR] := TRUE ;
          RGADR := RGADR+1 ;
          END (*PSQI*) ;

        PSQR :
           WITH STK[TOP-1] DO
           BEGIN
              LOAD(STK[TOP-1]) ;
              GENRR(XMDR,RGADR,RGADR)
           END  (* PSQR *) ;
"NH"
"NH"    PCRD :
"NH"      WITH STK[TOP-1] DO
"NH"      BEGIN
"NH"        LOAD( STK[TOP-1] );
"NH"        GENRR(XSR,1,1);  GENRR(XLR,0,RGADR);  GENRR(XNR,RGADR,0);
"NH"        IF ASM THEN WRITELN(PRR,' BZ *+12')
"NH"        ELSE BEGIN
"NH"           Q := BASE_DSPLMT(PC+6);
"NH"           GENRX(XBC,EQUCND,Q MOD SL12, Q DIV SL12, 0);
"NH"        END;
"NH"        GENRR(XLR,0,RGADR);  GENRR(XBCTR,0,0);
"NH"        IF ASM THEN WRITELN(PRR,' BCT 1,*-10')
"NH"       ELSE BEGIN
"NH"           Q := BASE_DSPLMT(PC-5);
"NH"           GENRX(XBCT,1,Q MOD SL12, Q DIV SL12, 0);
"NH"        END;
"NH"        GENRR(XLTR,RGADR,RGADR+1);
"NH"        GENRX(XLA,RGADR+1,0,0,0);
"NH"        IF ASM THEN WRITELN(PRR,' BNZ *-22')
"NH"        ELSE BEGIN
"NH"           Q := BASE_DSPLMT(PC-11);
"NH"           GENRX(XBC,NEQCND,Q MOD SL12, Q DIV SL12, 0);
"NH"        END;
"NH"        GENRR(XLPR,RGADR,1);
"NH"        AVAIL[RGADR+1] := TRUE;  DTYPE := INT;
"NH"      END (* PCRD *) ;
"NH"
"NH"    PXPO :
"NH"      WITH STK[TOP-1] DO
"NH"      BEGIN
"NH"        FINDRG;
"NH"        IF VRBL THEN GETOPERAND(STK[TOP-1],Q1,P1,B1)
"NH"        ELSE LOAD(STK[TOP-1]);
"NH"        IF (VPA = RGS) AND DRCT THEN
"NH"          BEGIN
"NH"             GENRX(XSTD,RGADR,FL3,GBR,0);
"NH"             GENRX(XIC,NXTRG,FL3,GBR,0);
"NH"             AVAILFP[RGADR] := TRUE;
"NH"          END
"NH"        ELSE
"NH"          BEGIN
"NH"            GENRX(XIC,NXTRG,Q1,P1,B1);
"NH"            VPA := RGS;  DRCT := TRUE;
"NH"          END;
"NH"        GENRX(XLA,0,127,0,0);  GENRR(XNR,NXTRG,0);
"NH"        GENRX(XLA,0,64,0,0);   GENRR(XSR,NXTRG,0);
"NH"        RGADR := NXTRG;  DTYPE := INT;
"NH"      END (* PXPO *) ;
"NH"

        PNOT :
          WITH STK[TOP-1] DO
            IF BRCND >= 0 THEN
              IF NEG_CND THEN   (* CLEAR NEGATE FLAG *)
                BEGIN  NEG_CND := FALSE ;  BRCND := -1 ;  END
              ELSE  BRCND := 15-BRCND
            ELSE (* NEGATING A BOOLEAN VLUE *)
              IF VRBL THEN  BEGIN  NEG_CND := TRUE ;  BRCND := 0  END
              ELSE
                IF  FPA.DSPLMT = 0 THEN FPA.DSPLMT := 1 ELSE FPA.DSPLMT := 0 ;

         "WITH STK[TOP-1] DO
            IF VRBL THEN  (*NEEDS FURTHER REFINEMENT*)
              IF BRCND >= 0 THEN  BRCND := 15-BRCND
              ELSE
                BEGIN  LOAD(STK[TOP-1]) ;  GENRXLIT(XX,RGADR,1,0)  END
            ELSE
              IF  FPA.DSPLMT = 0 THEN FPA.DSPLMT := 1 ELSE FPA.DSPLMT := 0 ;"
        PODD :
          WITH STK[TOP-1] DO
          BEGIN
            IF VRBL THEN

              IF DRCT AND (VPA = MEM) THEN
                BEGIN  GETOPERAND(STK[TOP-1],Q1,P1,B1) ;
                IF B1 > 0 THEN  IF P1 > 0 THEN  GENRR(XAR,P1,B1)
                                ELSE  P1 := B1 ;
                GENSI(XTM,Q1+3,P1,1) ;  (* RIGHT MOST BYTE IS BEING TESTED *)
                BRCND := 1 (* B0 *) ;
                END

              ELSE
              BEGIN  LOAD(STK[TOP-1]) ;  GENRXLIT(XN,RGADR,1,0)  END
            ELSE
              IF ODD(FPA.DSPLMT) THEN FPA.DSPLMT := 1 ELSE FPA.DSPLMT := 0  ;
            DTYPE := BOOL
            END (*WITH, PODD*) ;

        PINC,PDEC :
          WITH STK[TOP-1] DO
            BEGIN  IF OPC = PDEC THEN Q := -Q ;
            IF NOT DRCT THEN LOAD(STK[TOP-1]) ;
            FPA.DSPLMT := FPA.DSPLMT+Q ;
            END ;

        PCHR :
          WITH STK[TOP-1] DO
            IF DTYPE > CHRC THEN
              BEGIN  IF VRBL THEN  LOAD(STK[TOP-1]) ;  DTYPE := CHRC  END ;

        PORD :
          WITH STK[TOP-1] DO
            IF DTYPE <= CHRC THEN
              BEGIN  IF VRBL THEN  LOAD(STK[TOP-1]) ;  DTYPE := INT  END ;

        PNEW :
          BEGIN  TOP := TOP-1 ;
          GENRX(XL,TRG0,NEWPTR,GBR,0) ;  GENRXLIT(XS,TRG0,Q,0) ;
          GENRX(XST,TRG0,NEWPTR,GBR,0) ;
          IF NOT STK[TOP].DRCT THEN  LOAD(STK[TOP]) ;
          GETADR(STK[TOP],Q1,P1,B1) ;  GENRX(XST,TRG0,Q1,B1,P1) ;
          FREEREG(STK[TOP]) ;

          IF DEBUG THEN   (* CHECK FOR STACK-HEAP INTERFERENCE *)
            BEGIN
            GENRXLAB(XS,TRG0,SEGSZE,-1) ;
            GENRR(XCR,TRG0,LBR) ;
            GENRR(XBALR,RTREG,0) ;  GENRX(XBC,LEQCND,STKCHK,GBR,0) ;
            END ;

          END (*PNEW*) ;

        PSAV :
          BEGIN  TOP := TOP-1 ;
          GENRX(XL,TRG0,NEWPTR,GBR,0) ;
          IF NOT STK[TOP].DRCT THEN  LOAD(STK[TOP]) ;
          GETADR(STK[TOP],Q1,P1,B1) ;  GENRX(XST,TRG0,Q1,B1,P1) ;
          FREEREG(STK[TOP]) ;
          END (*PSAV*) ;

        PRST :
          BEGIN  TOP := TOP-1 ;
          WITH STK[TOP] DO
            BEGIN  LOAD(STK[TOP]) ;

            IF DEBUG THEN   (* SEE IF NEW HEAP POINTER VALID *)
              BEGIN
              IF RGADR <> 2 THEN
                BEGIN  IF NOT AVAIL[2] THEN ERROR(259) ;
                GENRR(XLR,2,RGADR) ;
                END ;
              GENRX(XBAL,RTREG,PTRCHK,GBR,0) ;
              END ;

            (* CODE FOR CLEARING THE RELEASE HEAP AREA SHOULD GO HERE *)
            (* SEE RETURN SEQUENCE 'PRET' AS AN EXAMPLE.              *)
            GENRX(XST,RGADR,NEWPTR,GBR,0) ;  AVAIL[RGADR] := TRUE ;
            END ;
          END (*PRST*) ;

        PCTS :
          BEGIN   (* SET/INITIALIZE RUN TIME COUNTERS *)
(*
          GENRXLAB(XL, TRG15, LBL2, -1) ;
          GENRX(XL, TRG1, HEAPLMT, GBR, 0) ;
          GENRX(XST, TRG15, DYNRUNC, TRG1, 0) ;   (* SET THE # OF COUNTERS *)
          IF ASM THEN  GENRR(XSLA, TRG15, 2)      (* CONVERT COUNT TO      *)
          ELSE  GENRS(XSLA, TRG15, 0, 2, 0) ;     (* TO # OF WORDS NEEDED  *)
          GENRR(XLR, TRG14,TRG1) ;       (* TRG1 POINTS TO CURRENT HEAP LIMIT *)
          GENRR(XSR, TRG14, TRG15) ;     (* TRG14 POINTS TO NEW HEAP LMT *)
          GENRX(XST, TRG14, NEWPTR, GBR, 0) ;       (* UPDATE THE 'HEAP_PTR' *)
          GENRX(XST, TRG14, HEAPLMT, GBR, 0) ;      (* AND HEAP_LIMIT_PTR    *)
          GENSS(XMVC, DYN2LEN, 0, TRG14, 0, TRG1) ; (* MOVE HEAP LIMIT AREA *)
          IF ASM THEN  GENRR(XSRA, TRG15, 2)        (* CONVERT BYTE COUNT TO *)
          ELSE  GENRS(XSRA, TRG15, 0, 2, 0) ;       (* TO WORD COUNT         *)
          GENRR(XSR, TRG0, TRG0) ;                  (* TRG0 <-- '0' *)
          GENRX(XST, TRG0, DYN2LEN, TRG14, 0) ;     (* CLEAR COUNTERS *)
          GENRX(XLA, TRG14, 4, TRG14, 0) ;          (* POINT TO NEXT COUNTER *)

          IF ASM THEN  WRITELN(PRR, ' BCT ', TRG15:2, ',*-8')
          ELSE GENRX(XBCT, TRG15, PC*2-8, PBR1,0) ; (* REPEAT AS NEEDED *)

          CSPREGACTIVE := FALSE ;
*)
          GENRXLAB(XL, 2, LBL2, -1) ;
          CSP := PCTR;
          GOTOCSP;
         END (*PCTS*) ;

        PCTI :
          BEGIN   (* INCREMENT THE COUNT OF COUNTER # 'Q' *)
          GENRX(XL, TRG1, HEAPLMT, GBR, 0) ;
          GENRX(XLA, TRG14, 1, 0, 0) ;
          Q := 4*Q+DYN2LEN ;

          IF Q > SHRTINT THEN
            BEGIN
            GENRXLIT(XA, TRG1, Q, 0) ;   Q := 0 ;
            END ;

          GENRX(XA, TRG14, Q, TRG1, 0) ;
          GENRX(XST, TRG14, Q, TRG1, 0) ;
          END (*PCTI*) ;

        END (*CASE OPC OF*) ;


      END (*UOPERATION*) ;


    PROCEDURE SOPERATION(VAR L, R: DATUM);
      (* SET UP FOR STRING MOVE/COMPARE OPERATIONS *)
      VAR  P1, B1, P2, B2: LVLRNG;  Q1, Q2: ADRRNG;  XOPC: BYTE;

      BEGIN
      GETADR(L,Q1,P1,B1) ;
      IF NOT L.DRCT THEN
        BEGIN  GENRX(XL,TXRG,Q1,B1,P1) ;
        Q1 := 0 ;  B1 := 0 ;  P1 := TXRG ;
        END ;
      TXRG := TRG1 ;  (*TO AVOID REASSINMENT OF THE SAME BASE REG*)
      OLDCSP := PSIO; (*INDICATES LOSS OF TRG1*)
      GETADR(R,Q2,P2,B2) ;
      IF NOT R.DRCT THEN
        BEGIN  GENRX(XL,TXRG,Q2,B2,P2) ;
        Q2 := 0 ;  B2 := 0 ;  P2 := TXRG ;
        END ;
      TXRG := TRG14  ; (*RESTORE THE OLD MIDLEVEL BASE REG*)

      IF P1 < 0 THEN  BEGIN  B1 := P1;  P1 := 0;  END;
      IF P2 < 0 THEN  BEGIN  B2 := P2;  P2 := 0;  END;

      IF Q <= 256 THEN
        BEGIN (*SHORT MOVE*)
         IF B1 > 0 THEN IF P1 > 0 THEN GENRR(XAR,P1,B1)
                        ELSE P1 := B1 ;
         IF B2 > 0 THEN IF P2 > 0 THEN GENRR(XAR,P2,B2)
                        ELSE P2 := B2 ;
         XOPC := XMVC;  IF OPC <> PMOV THEN  XOPC := XCLC;
         GENSS(XOPC,Q,Q1,P1,Q2,P2) ;
         IF B1 < 0 THEN
            BEGIN  CODE[PC-2] := STRTBL[Q1].LNK;  STRTBL[Q1].LNK := PC-2;  END;
         IF B2 < 0 THEN
            BEGIN  CODE[PC-1] := STRTBL[Q2].LNK;  STRTBL[Q2].LNK := PC-1;  END;
         END (*SHORT STRING*)
      ELSE
        BEGIN

        (* THIS IS ONLY VALID FOR THE 370,    FOR THE 360 THE 'CLCL'    *)
        (* INSTR. SHOULD BE REPLACED BY AN APPROPRIATE NUMBER OF 'CLC'S *)

        FINDRP ;  GENRX(XLA,NXTRG,Q1,B1,P1) ;  P1 := NXTRG ; B1 := NXTRG+1 ;
        FINDRP ;  GENRX(XLA,NXTRG,Q2,B2,P2) ;  P2 := NXTRG ; B2 := NXTRG+1 ;
        IF (B1 < 0) OR (B2 < 0) THEN  ERROR(202);
        GENRXLIT(XL,B1,Q,0) ;  GENRR(XLR,B2,B1) ;
        XOPC := XMVCL;  IF OPC <> PMOV THEN XOPC := XCLCL;
        GENRR(XOPC,P1,P2) ;
        AVAIL[P1] := TRUE ;  AVAIL[B1] := TRUE ;
        AVAIL[P2] := TRUE ;  AVAIL[B2] := TRUE ;
        S370CNT := S370CNT+1;
        END ;

      FREEREG(L) ;  FREEREG(R) ;
      END (*SOPERATION*);


  PROCEDURE BOPERATION ;
    (* BINARY OPERATIONS *)

    VAR  L, R :     DATUM ;  (*LEFT AND RIGHT OPERANDS*)
         LOP, ROP : STKPTR ; (*STACK INDEX OF LEFT AND RIGHT OPERANDS*)
         LRG :      RGRNG ;  (*REGISTER HOLDING LEFT OPERAND*)
         OP1, OP2 : BYTE ;
         LR :       BOOLEAN ; (*LEFT/RIGHT INTERCHANGE FLAG*)
        Q1, Q2 : ADRRNG ;  P1, P2, B1: LVLRNG ;

    BEGIN  (*ADI,SUBI,MPI, COULD BE FURTHER OPTIMIZED*)
    (*DETERMINE WHICH OPERAND SHOULD BE USED AS LEFT HAND OPERAND ...*)
    LR := (OPC IN [PSBR,PDVR,PDVI,PMOD,PDIF,PINN])
       OR (STK[TOP-1].VRBL AND STK[TOP].DRCT)
       OR (NOT STK[TOP-1].DRCT) OR (NOT STK[TOP].VRBL) ;

    IF LR THEN BEGIN  LOP := TOP-1 ;  ROP := TOP  END
    ELSE BEGIN LOP := TOP ;  ROP := TOP-1  END ;
    L := STK[LOP] ;  R := STK[ROP] ;

    CASE OPC OF

    PADI,PSBI :
      BEGIN
      IF NOT L.DRCT THEN LOAD(L) ;
      IF R.DRCT THEN
        IF OPC = PADI THEN
          BEGIN L.FPA.DSPLMT := L.FPA.DSPLMT+R.FPA.DSPLMT; R.FPA.DSPLMT := 0 END
        ELSE
          BEGIN L.FPA.DSPLMT := L.FPA.DSPLMT-R.FPA.DSPLMT; R.FPA.DSPLMT :=0 END;
      (*CONST<OPR>CONST AND VRBL<OPR>CONST CASES ARE COMPLETED NOW *)

      OP1 := XAR ;  OP2 := XA ;
      IF OPC = PSBI THEN  BEGIN OP1 := XSR ;  OP2 := XS  END ;
      IF R.VRBL THEN
        BEGIN  Q := L.FPA.DSPLMT ; L.FPA.DSPLMT := 0 ;  (*SAVE FPA*)
        LOAD(L) ;  IF R.DTYPE <> INT THEN  LOAD(R) ;
        IF R.DRCT THEN
          IF R.VPA = RGS THEN
            BEGIN  GENRR(OP1,L.RGADR,R.RGADR) ;  AVAIL[R.RGADR] := TRUE  END
          ELSE (*VPA=MEM*)
            BEGIN  Q1 := R.MEMADR.DSPLMT ; P1 := R.MEMADR.LVL ;
            BASE(Q1,P1,B1) ;  GENRX(OP2,L.RGADR,Q1,B1,P1) ;
            END
        ELSE (*NOT R.DRCT*)
          BEGIN  GETOPERAND(R,Q1,P1,B1) ;
          GENRX(OP2, L.RGADR,Q1,B1,P1) ;
          END ;
        L.FPA.DSPLMT := Q ;  (*RESTORE FPA*)
        END (*IF R.VRBL*) ;

        IF NOT LR AND (OPC = PSBI) THEN  (*THIS DOES NOT SEEM TO BE COMPLETE*)
          BEGIN  Q := -L.FPA.DSPLMT ;  L.FPA.DSPLMT := 0 ;
          IF L.VRBL THEN
            BEGIN  LOAD(L) ;
            GENRR(XLCR,L.RGADR,L.RGADR) ;
            END ;
          L.FPA.DSPLMT := Q ;
          END ;
      END (*ADI,SBI*) ;

   PMPI :
      BEGIN
      IF R.VRBL THEN
        BEGIN
        MDTAG := PMPI ;  LOAD(L) ;  MDTAG := PBGN ;
        IF R.DTYPE <> INT THEN LOAD(R) ELSE GETOPERAND(R,Q1,P1,B1) ;
        IF (NOT R.DRCT) OR (R.VPA = MEM) THEN  GENRX(XM,L.RGADR,Q1,P1,B1)
        ELSE  BEGIN GENRR(XMR,L.RGADR,R.RGADR) ;  AVAIL[R.RGADR] := TRUE ; END
        END
      ELSE (*NOT R.VRBL*)

        BEGIN  Q := 0 ;
        IF (L.DRCT) THEN
          BEGIN  Q := L.FPA.DSPLMT*R.FPA.DSPLMT ;  L.FPA.DSPLMT := 0  END
        ELSE  LOAD(L) ;

        IF L.VRBL THEN
          BEGIN
          MDTAG := PMPI ;  LOAD(L) ;  MDTAG := PBGN ;
          P := POWER2(R.FPA.DSPLMT) ;
          IF P <> 0 THEN   (* MULTIPLY BY UNITY CAN BE SKIPPED *)
             IF P >= 0 THEN
                IF ASM THEN  GENRR(XSLL,L.RGADR+1,P)
                ELSE   GENRS(XSLL,L.RGADR+1,0,P,0)
                          (* NOTE LOGICAL SHIFT *)
             ELSE  GENRXLIT(XM,L.RGADR,R.FPA.DSPLMT,0) ;
          END (*IF L.VRBL*);

        L.FPA.DSPLMT := Q ;
        END (*ELSE ...*);

      IF L.VRBL THEN
        AVAIL[L.RGADR] := TRUE ;  L.RGADR := L.RGADR+1 ;
      END (*PMPI*) ;

    PDVI,PMOD :
      IF NOT L.VRBL AND NOT R.VRBL THEN  (* BOTH CONSTANTS*)
        IF R.FPA.DSPLMT = 0 THEN  ERROR(300)  (* DIVISION BY ZERO*)
        ELSE
          IF OPC = PDVI THEN
            L.FPA.DSPLMT := L.FPA.DSPLMT DIV R.FPA.DSPLMT
          ELSE  L.FPA.DSPLMT := L.FPA.DSPLMT MOD R.FPA.DSPLMT
      ELSE (* MORE COMMON CASES *)
        BEGIN  MDTAG := PDVI ;  LOAD(L) ;  MDTAG := PBGN ;
        IF R.VRBL THEN
          BEGIN
          IF R.DTYPE <> INT THEN LOAD(R) ELSE GETOPERAND(R,Q1,P1,B1) ;
          IF NOT R.DRCT OR (R.VPA = MEM) THEN GENRX(XD,L.RGADR,Q1,B1,P1)
          ELSE  BEGIN GENRR(XDR,L.RGADR,R.RGADR) ; AVAIL[R.RGADR] := TRUE END
          END (*R.VRBL*)
        ELSE  (*^R.VRBL*)

        (*BEGIN  P := POWER2(R.FPA.DSPLMT) ;                                  *)
        (*IF (P < 0) OR (OPC = PMOD) THEN  GENRXLIT(XD,L.RGADR,R.FPA.DSPLMT,0)*)
        (*ELSE  IF ASM THEN  GENRR(XSRA,L.RGADR+1,P)                          *)
        (*      ELSE  GENRS(XSRA,L.RGADR+1,0,P,0)  ;                          *)
        (*END ;                                                               *)
        (*                                                                    *)
        (*--DIVISION BY A POWER OF 2 IS NOT THE SAME AS ARITHMETIC            *)
        (*--RIGHT SHIFT ACCORDING TO THE 370, SO INSTEAD OF THE ABOVE :       *)

          GENRXLIT(XD,L.RGADR,R.FPA.DSPLMT,0) ;

        IF OPC = PDVI THEN
          BEGIN AVAIL[L.RGADR] := TRUE ;  L.RGADR := L.RGADR+1  END
        ELSE AVAIL[L.RGADR+1] := TRUE ;
        END (* ELSE , PDVI *) ;

    PEQU,PNEQ,PGRT,PLEQ,PLES,PGEQ :
      BEGIN   IF NOT LR THEN OPC := INVBRM[OPC] ;

        CASE OPNDTYPE OF

        ADR,INT,BOOL,CHRC:
          WITH R DO
            BEGIN
            LOAD(L) ;

            IF VRBL THEN
              BEGIN
              IF NOT(DTYPE IN [ADR,INT]) THEN  LOAD(R)
              ELSE  GETOPERAND(R,Q1,P1,B1) ;
              IF (NOT DRCT) OR (VPA = MEM) THEN  GENRX(XC,L.RGADR,Q1,B1,P1)
              ELSE
                BEGIN  GENRR(XCR,L.RGADR,RGADR) ;  AVAIL[RGADR] := TRUE  END
              END


            ELSE (*IF NOT VRBL (I.E.CONST)*)

              IF FPA.DSPLMT = 0 THEN GENRR(XLTR,L.RGADR,L.RGADR)
              ELSE
                IF (OPNDTYPE = ADR) AND (NOT FLIPDEBUG) THEN
                  BEGIN   (* CONSTANT OF TYPE ADR = NIL ! *)
                  (* FOLLOWING VALID ONLY IF $D- IS USED *)
                  GENRR(XLTR, L.RGADR, L.RGADR) ;
                  IF OPC = PEQU THEN  OPC := PLES
                  ELSE  (* OPC = PNEQ *) OPC := PGEQ ;
                  END
                ELSE  GENRXLIT(XC,L.RGADR,FPA.DSPLMT,0) ;

            AVAIL[L.RGADR] := TRUE ;
            END (*WITH R---ADR,INT,...*) ;

        REEL :
          WITH R DO
          BEGIN
            LOAD(L) ;
            IF VRBL THEN
            BEGIN
              GETOPERAND(R,Q1,P1,B1) ;
              IF (VPA = RGS) AND DRCT THEN
              BEGIN
                GENRR(XCDR,L.RGADR,R.RGADR) ;
                AVAILFP [RGADR] := TRUE
              END  (* IF VPA = RGS *)
              ELSE  (* VPA = MEM OR NOT DRCT *)
                GENRX(XCD,L.RGADR,Q1,B1,P1)
            END  (* VRBL *)
            ELSE  (* CONSTANT *)
              GENRXDLIT(XCD,L.RGADR,SCNST,SCNSTL,RCNST) ;
            AVAILFP[L.RGADR] := TRUE ;
          END  (* WITH -- REEL *) ;

        PSET :
          BEGIN
          LOAD(L) ;  LOAD(R) ;
          IF OPC IN [PLES,PLEQ] THEN
            BEGIN
            GENRR(XNR,R.RGADR,L.RGADR) ;  GENRR(XNR,R.RGADR+1,L.RGADR+1) ;
            END
          ELSE IF OPC IN [PGRT,PGEQ] THEN
            BEGIN
            GENRR(XNR,L.RGADR,R.RGADR) ;  GENRR(XNR,L.RGADR+1,R.RGADR+1) ;
            END ;
          GENRR(XXR,L.RGADR,R.RGADR) ;  GENRR(XXR,L.RGADR+1,R.RGADR+1) ;
          GENRR(XXOR,L.RGADR,L.RGADR+1) ;
          FREEREG(L) ;  FREEREG(R) ;
          IF OPC <> PNEQ THEN  OPC := PEQU ;
          END (*PSET*) ;


        STRG :
          BEGIN
"""       GETADR(L,Q1,P1,B1) ;
          IF NOT L.DRCT THEN
            BEGIN  GENRX(XL,TXRG,Q1,B1,P1) ;
            Q1 := 0 ;  B1 := 0 ;  P1 := TXRG ;
            END ;
          TXRG := TRG1 ;  (*TO AVOID REASSINMENT OF THE SAME BASE REG*)
          GETADR(R,Q2,P2,B2) ;
          IF NOT R.DRCT THEN
            BEGIN  GENRX(XL,TXRG,Q2,B2,P2) ;
            Q2 := 0 ;  B2 := 0 ;  P2 := TXRG ;
            END ;
          TXRG := TRG14  ; (*RESTORE THE OLD MIDLEVEL BASE REG*)

          IF Q <= 256 THEN
            BEGIN (*SHORT MOVE*)
             IF B1 > 0 THEN IF P1 > 0 THEN GENRR(XAR,P1,B1)
                            ELSE P1 := B1 ;
             IF B2 > 0 THEN IF P2 > 0 THEN GENRR(XAR,P2,B2)
                            ELSE P2 := B2 ;
             GENSS(XCLC,Q,Q1,P1,Q2,P2) ;
             END (*SHORT COMPARE*)
          ELSE
            BEGIN

            (* THIS IS ONLY VALID FOR THE 370,    FOR THE 360 THE 'CLCL'    *)
            (* INSTR. SHOULD BE REPLACED BY AN APPROPRIATE NUMBER OF 'CLC'S *)

            FINDRP ;  GENRX(XLA,NXTRG,Q1,B1,P1) ;  P1 := NXTRG ; B1 := NXTRG+1 ;
            FINDRP ;  GENRX(XLA,NXTRG,Q2,B2,P2) ;  P2 := NXTRG ; B2 := NXTRG+1 ;
            GENRXLIT(XL,B1,Q,0) ;  GENRR(XLR,B2,B1) ;
            GENRR(XCLCL,P1,P2) ;
            AVAIL[P1] := TRUE ;  AVAIL[B1] := TRUE ;
            AVAIL[P2] := TRUE ;  AVAIL[B2] := TRUE ;
            END ;

          FREEREG(L) ;  FREEREG(R) ;   """
          SOPERATION(L, R);
          OLDCSP := PSIO ;
          END (*STRG*)

        END (*CASE OPNDTYE OF*) ;

      BRCND := BRMSK[OPC] ;
      END (*PEQU,PNEQ,...*) ;

    PAND, PIOR :
      WITH R DO
        BEGIN  OP1 := XNR ;
        IF OPC = PIOR THEN OP1 := XXOR ;
        LOAD(L) ;  LOAD(R)  ;
        (*THIS CAN BE IMPROVED BY USING THE CONDITION CODE AS THE TOP ELEMENT *)
        GENRR(OP1,L.RGADR,RGADR) ;
        AVAIL[RGADR] := TRUE ;
        END (*WITH...,PAND,PIOR*) ;

    PINN :
      WITH R DO
        BEGIN   LOAD(R) ;

        IF L.VRBL THEN
          BEGIN   LOAD(L) ;
          GENRR(XLCR,L.RGADR,L.RGADR) ;
          IF ASM THEN GENRX(XSLDL,RGADR,63,L.RGADR,0)
          ELSE  GENRS(XSLDL,RGADR,0,63,L.RGADR) ;
          AVAIL[L.RGADR] := TRUE ;
          END
        ELSE  (* ^ L.VRBL *)
          IF ASM THEN GENRR(XSLDL,RGADR,63-L.FPA.DSPLMT)
          ELSE  GENRS(XSLDL,RGADR,0,63-L.FPA.DSPLMT,0) ;

        GENRR(XLTR,RGADR,RGADR) ;
        AVAIL[RGADR] := TRUE ;
        AVAIL[RGADR+1] := TRUE ;
        "OPC := PLES ;"  BRCND := LESCND ;
        END (*WITH R...,PINN*) ;

    PUNI :
      WITH R DO
        IF L.VRBL THEN
          BEGIN  LOAD(L) ;  IF NOT DRCT THEN  LOAD(R) ;
          IF R.VRBL THEN
            IF VPA = MEM THEN
              BEGIN
              P1 := MEMADR.LVL ;  Q1 := MEMADR.DSPLMT ;
              BASE(Q1,P1,B1) ;
              GENRX(XO,L.RGADR,Q1,B1,P1) ; GENRX(XO,L.RGADR+1,Q1+4,B1,P1) ;
              END
            ELSE (*I.E. VPA = RGS*)
              BEGIN
              GENRR(XXOR,L.RGADR,RGADR) ;
              GENRR(XXOR,L.RGADR+1,RGADR+1) ;
              FREEREG(R) ;
              END  (*IF VPA = MEM*)
          ELSE (*I.E. ^R.VRBL*)
            BEGIN  I_S_R.S := PCNST  ;
            GENRXLIT(XO,L.RGADR,I_S_R.I1,0) ;
            GENRXLIT(XO,L.RGADR+1,I_S_R.I2,0) ;
            END ;
          END (*IF L.VRBL*)
        ELSE (*BOTH OPERANDS CONSTANT*)
          L.PCNST := L.PCNST+PCNST
      (*END WITH R...,PUNI*) ;

    PINT :
      WITH R DO
        IF L.VRBL THEN
          BEGIN  LOAD(L) ;  IF NOT DRCT THEN  LOAD(R) ;
          IF R.VRBL THEN
            IF VPA = MEM THEN
              BEGIN
              P1 := MEMADR.LVL ;  Q1 := MEMADR.DSPLMT ;
              BASE(Q1,P1,B1) ;
              GENRX(XN,L.RGADR,Q1,B1,P1) ;  GENRX(XN,L.RGADR+1,Q1+4,B1,P1) ;
              END
            ELSE (*I.E. VPA = RGS*)
              BEGIN
              GENRR(XNR,L.RGADR,RGADR) ;  GENRR(XNR,L.RGADR+1,RGADR+1) ;
              FREEREG(R) ;
              END  (*IF VPA = MEM*)
          ELSE (*I.E. ^R.VRBL*)
            BEGIN  I_S_R.S := PCNST ;
            GENRXLIT(XN,L.RGADR,I_S_R.I1,0) ;
            GENRXLIT(XN,L.RGADR+1,I_S_R.I2,0) ;
            END ;
          END (*IF L.VRBL*)
        ELSE (*BOTH OPERANDS CONSTANT*)
          L.PCNST := L.PCNST*PCNST
        (*END WITH R...,PINT*) ;

    PDIF :
      WITH R DO
        IF L.VRBL OR R.VRBL THEN
          BEGIN  LOAD(L) ;   IF NOT DRCT THEN   LOAD(R) ;
          (* NOW BOTH OPERANDS ARE IN REGS, ONLY THAT CASE TO BE CONSIDERED *)
          IF R.VRBL THEN
            IF VPA = MEM THEN
              BEGIN
              P1 := MEMADR.LVL ;  Q1 := MEMADR.DSPLMT ;
              BASE(Q1,P1,B1) ;
              GENRX(XX,L.RGADR,Q1,B1,P1) ;  GENRX(XX,L.RGADR+1,Q1+4,B1,P1) ;
              END
            ELSE (*I.E. VPA = RGS*)
              BEGIN
              GENRR(XXOR,L.RGADR,RGADR) ;
              GENRR(XXOR,L.RGADR+1,RGADR+1) ;
              GENRR(XXR,L.RGADR,RGADR) ;  GENRR(XXR,L.RGADR+1,RGADR+1) ;
              FREEREG(R) ;
              END  (*IF VPA = MEM*)
          ELSE (*I.E.^R.VRBL*)
            BEGIN  I_S_R.S := PCNST ;
            GENRXLIT(XX,L.RGADR,I_S_R.I1,0) ;
            GENRXLIT(XX,L.RGADR+1,I_S_R.I2,0) ;
            END ;
          END (*IF L.VRBL*)
        ELSE (*BOTH OPERANDS CONSTANT*)
          L.PCNST := L.PCNST-PCNST
        (*END WITH R...,PDIF*) ;


    PADR,PSBR :
       BEGIN
          OP1 := XADR;   OP2 := XAD;
          IF OPC = PSBR THEN
          BEGIN OP1 := XSDR;   OP2 := XSD   END;
          LOAD(L);
          IF R.VRBL THEN
          BEGIN
             GETOPERAND(R,Q1,P1,B1) ;
             IF (R.VPA = RGS) AND R.DRCT THEN
             BEGIN
                GENRR(OP1,L.RGADR,R.RGADR);
                AVAILFP[R.RGADR] := TRUE
             END
             ELSE  (* VPA = MEM OR NOT DRCT *)
                GENRX(OP2,L.RGADR,Q1,B1,P1)
          END  (* IF R.VRBL *)
          ELSE  (* CONSTANT *)
          GENRXDLIT(OP2,L.RGADR,R.SCNST,R.SCNSTL,R.RCNST)
       END  (* PADR,PSBR *) ;

     PDVR,PMPR:
       BEGIN
         LOAD(L) ;
         OP1 := XDDR ;   OP2 := XDD ;
         IF OPC=PMPR THEN
         BEGIN OP1 := XMDR ;   OP2 := XMD    END ;
         IF R.VRBL THEN
         BEGIN
           GETOPERAND(R,Q1,P1,B1) ;
           IF (R.VPA = RGS) AND R.DRCT THEN
           BEGIN
             GENRR(OP1,L.RGADR,R.RGADR) ;
             AVAILFP [R.RGADR] := TRUE
           END  (* R.VPA = RGS *)
           ELSE  (* R.VPA = MEM OR NOT DRCT *)
             GENRX(OP2,L.RGADR,Q1,B1,P1)
         END  (*  R.VRBL  *)
         ELSE  (*  CONSTANT  *)
           GENRXDLIT(OP2,L.RGADR,R.SCNST,R.SCNSTL,R.RCNST)
       END  (* PDVR,PMDR *)   ;

    END (*CASE OPC OF*) ;

  STK[TOP-1] := L ;
  END (*BOPERATION*) ;



    (* LOAD_STOR_MOVE IOPERATIONS *)
    (* -------------------------- *)


    BEGIN (*ASMNXTINST*)

    IF BRCND >= 0 THEN
      IF NOT (OPC IN [PFJP,PNOT,PLOC]) THEN   (* XLATE COND CODE TO BOOL. VAL *)
        WITH STK[TOP-1] DO
          BEGIN

          IF NEG_CND THEN   (* JUST NEGATE TOP OF STACK *)
            BEGIN  LOAD(STK[TOP-1]) ;  GENRXLIT(XX,RGADR,1,0)  END
          ELSE  (* OTHERWISE TRANSLATE CC TO BOOLEAN *)

            BEGIN  FINDRG ;
            GENRX(XLA,NXTRG,1,0,0) ;  (*ASSUME TRUE*)
            IF ASM THEN
              WRITELN(PRR,' BC',BRCND:4,',*+6')
            ELSE
              BEGIN
              GENRX(XBC,BRCND,0,0,0) ;
              CODE[PC-1] := BASE_DSPLMT(PC+1) ;
              END ;
            GENRR(XSR,NXTRG,NXTRG) ;  (* THEN CHANGE TO FALSE IF NEEDED*)

              BEGIN  DTYPE := BOOL ;
              VRBL := TRUE ; DRCT := TRUE ;
              VPA := RGS ;  RGADR := NXTRG ;
              FPA := ZEROBL ;
              END (*WITH STK...*) ;

            END (* TRANSLATE CC *) ;

          BRCND := -1 ;  NEG_CND := FALSE ; (* RESET C.C. FLAG TO INACTIVE *)
          END (*WITH STK[..., IF NOT (OPC IN [PFJP,... *) ;


    CASE OPC OF


    PLOD ",PLDO" :
      WITH STK[TOP] DO
        BEGIN
        IF OPNDTYPE IN [ADR,INT,PSET] THEN
          IF (Q MOD INTSIZE) <> 0 THEN  ERROR(611) ;
        IF OPNDTYPE = REEL THEN
          IF (Q MOD REALSIZE) <> 0 THEN  ERROR(612) ;
        DTYPE := OPNDTYPE ;  VRBL := TRUE ;  DRCT := TRUE ;
        FPA := ZEROBL ;  VPA := MEM ;
        MEMADR.LVL := "CURLVL-" P ;  MEMADR.DSPLMT := Q ;
        TOP := TOP+1 ;
        END (*LOD,LDO*) ;

    PSTR ",PSRO" :
      BEGIN " P := CURLVL-P ;"  TOP := TOP-1 ;
      IF OPNDTYPE IN [ADR,INT,PSET] THEN
        IF (Q MOD INTSIZE) <> 0 THEN  ERROR(611) ;
      IF OPNDTYPE = REEL THEN
        IF (Q MOD REALSIZE) <> 0 THEN  ERROR(612) ;
      STORE(TOP,FALSE(*DIRECT*) ) ;
      END ;

    PSTO:
      BEGIN  STORE(TOP-1,TRUE(*INDIRECT*)) ;  TOP := TOP-2  END ;

    PLDA ",PLAO" :
      WITH STK[TOP] DO
      BEGIN  DTYPE := ADR ;  VRBL := FALSE ;  DRCT := TRUE ;
      FPA.LVL := "CURLVL-"P ;  FPA.DSPLMT := Q ;
      TOP := TOP+1 ;
      END ;

    PLDC :
      WITH STK[TOP] DO
      BEGIN  DTYPE := OPNDTYPE ;  VRBL := FALSE ;  FPA := ZEROBL ;
      DRCT := TRUE ;

        CASE OPNDTYPE OF

        ADR:
          FPA.DSPLMT := -1 ; (*LDC NIL*)

        BOOL,CHRC,INT :
          FPA.DSPLMT :=  IVAL ;

        REEL :
           BEGIN
              SCNST := STRPTR;
              SCNSTL := SLNGTH;
              RCNST := 0 ;  I := 1 ;  NEGATE := FALSE ;
              IF STRPTR@[I] = '-' THEN  BEGIN  NEGATE := TRUE ;  I := I+1 END
              ELSE  IF STRPTR@[I] = '+' THEN I := I+1 ;

              REPEAT
                RCNST := RCNST*10 + (ORD(STRPTR@[I])-ORD('0')) ;
                I := I+1 ;
              UNTIL (STRPTR@[I] < '0') OR (STRPTR@[I] > '9') ;


              IF STRPTR@[I]= '.' THEN
                BEGIN
                I := I+1 ;   POWER10 := 10 ;
                  REPEAT
                  RCNST := RCNST+(ORD(STRPTR@[I])-ORD('0')) / POWER10 ;
                  I := I+1 ;   POWER10 := POWER10 * 10
                  UNTIL (I > SLNGTH) OR (STRPTR@[I] < '0')OR(STRPTR@[I] > '9');
                END ;

              IF NEGATE THEN RCNST := -RCNST  ;

              IF I < SLNGTH THEN   (* SCIENTIFIC NOTATION FOR REAL *)
                IF STRPTR@[I] <> 'E' THEN ERROR(613)
                  ELSE
                    BEGIN   I := I+1 ;   NEGATE := FALSE ;

                    IF STRPTR@[I] = '-' THEN
                      BEGIN  NEGATE := TRUE ;  I := I+1   END
                    ELSE  IF STRPTR@[I] = '+' THEN I := I+1 ;

                    J := 0 ;
                    FOR I := I TO SLNGTH DO
                      J := J*10 + (ORD(STRPTR@[I])-ORD('0')) ;

                    IF  J > 100 THEN ERROR(613)
                    ELSE
                      FOR I := 1 TO J DO
                        IF NEGATE THEN RCNST := RCNST / 10
                        ELSE  RCNST := RCNST * 10 ;
                    END  ;

           END  (* REEL *);

        PSET :
          PCNST := PSVAL

        END (*CASE OPNDTYPE*) ;

      TOP := TOP+1 ;
      END (*LDC*) ;

    PIND :
      WITH STK[TOP-1] DO
      BEGIN  IF DTYPE <> ADR THEN  ERROR(602) ;
      IF VRBL THEN
        BEGIN  IF NOT DRCT THEN  LOAD(STK[TOP-1]) ;
        FPA.DSPLMT := FPA.DSPLMT+Q ;  DRCT := FALSE ;
        END
      ELSE
        BEGIN
        MEMADR := FPA ;  MEMADR.DSPLMT := MEMADR.DSPLMT+Q ;  FPA := ZEROBL ;
        VRBL := TRUE ;  VPA := MEM ;  DRCT := TRUE ;
        END ;

      DTYPE  := OPNDTYPE ;
      END (*PIND*) ;

    PLCA :
      WITH STK[TOP] DO
        BEGIN  DTYPE := ADR;
        IF ASM THEN
           BEGIN  FINDRG;
           VRBL := TRUE ;  DRCT := TRUE ;  FPA := ZEROBL ;
           VPA := RGS ;  RGADR := NXTRG ;
           WRITELN(PRR,' LSA ',NXTRG:FLDW(NXTRG),',''',SVAL:SLNGTH,'''')
           END
        ELSE

          IF NXTSLOC < (PC+SLNGTH) THEN  ERROR(253)  (* CSECT OVERFLOW *)
          ELSE
            BEGIN   (* STRINGS ARE CURRENTLY ALINGED ON 2-BYTE BOUNDARY *)
            FOR I := (SLNGTH+1) DIV 2 DOWNTO 1 DO
              BEGIN
              NXTSLOC := NXTSLOC-1 ;
              CODE[NXTSLOC] := "EBCDIC["ORD(SVAL[I*2-1])"]"*SL8  +
                               "EBCDIC["ORD(SVAL[I*2])"]" ;
              END ;

          (*GENRX(XLA,NXTRG,0,0,0) ;  CODE[PC-1] := NXTSLOC ;               *)
          (*STRTBL[NXTSTR].LNK := PC-1 ;  STRTBL[NXTSTR].LNGTH := SLNGTH ;  *)
          (*   TO DELAY ISSUING THE EXTRA 'LA' INSTRUCTION:                 *)
            STRTBL[NXTSTR].LNK := NXTSLOC;  STRTBL[NXTSTR].LNGTH := SLNGTH;
          (*VRBL := TRUE ;  DRCT := TRUE ;                 FPA := ZEROBL ;  *)
          (*VPA := RGS ;  RGADR := NXTRG ;                                  *)
            VRBL := FALSE;  DRCT := TRUE;
            FPA.DSPLMT := NXTSTR;  FPA.LVL := -1;
            IF NXTSTR < STRCNT THEN  NXTSTR := NXTSTR+1
            ELSE  ERROR(254) ;
            END  (* STRING_POOL FIXUP *) ;

        TOP := TOP+1;
        END (*PLCA*) ;

    PIXA :
      BEGIN  TOP := TOP-1  ;
      WITH STK[TOP] DO
        BEGIN  IF NOT DRCT THEN  LOAD(STK[TOP]) ;
        IF NOT (DTYPE IN [ADR,INT,BOOL,CHRC]) THEN  ERROR(601) ;
        FPA.DSPLMT := FPA.DSPLMT*Q ;

        IF VRBL THEN

          BEGIN
          IF VPA = MEM THEN
            BEGIN  FINDRG ;
            P1 := MEMADR.LVL ;  Q1 := MEMADR.DSPLMT ;
            BASE(Q1,P1,B1) ;
            IF DTYPE IN [CHRC,BOOL] THEN
              BEGIN  GENRR(XSR,NXTRG,NXTRG) ;
              GENRX(XIC,NXTRG,Q1,B1,P1) ;
              END
            ELSE (*INT,ADR*)  GENRX(XL,NXTRG,Q1,B1,P1) ;
            VPA := RGS ;  RGADR := NXTRG ;
            END ;
          (* VPA IS IN A REG. NOW*)
          IF Q > HALFINT THEN  ERROR(504) ;    (* TOO LARGE FOR A HALF WORD *)
          Q2 := POWER2(Q) ;
          IF Q2 > 0 THEN

            IF ASM THEN  GENRR(XSLA,RGADR,Q2)
            ELSE  GENRS(XSLA,RGADR,0,Q2,0)

          ELSE  IF Q2 < 0 THEN  GENRXLIT(XMH,RGADR,Q,-2 (*=H'Q'*) ) ;
          END ;

        (* NOW ADD THE TOP TO THE SECOND TOP *)

        WITH STK[TOP-1] DO
           BEGIN
           IF NOT VRBL THEN
              IF FPA.LVL < 0 THEN  (*I.E. INDEXING THROUGH A CONSTANT STRING*)
                 LOAD(STK[TOP-1]);
           IF NOT DRCT THEN LOAD(STK[TOP-1]) ;
           END (*with stk[top-1*);

        STK[TOP-1].FPA.DSPLMT := STK[TOP-1].FPA.DSPLMT+FPA.DSPLMT ;
        FPA.DSPLMT := 0 ;

        IF VRBL AND STK[TOP-1].VRBL THEN
          IF VPA = RGS THEN
            IF STK[TOP-1].VPA = RGS THEN  (*BOTH OPERANDWS IN REGS*)
              BEGIN
              GENRR(XAR,STK[TOP-1].RGADR,RGADR) ;  AVAIL[RGADR] := TRUE
              END
            ELSE   (*TOP IN REG., 2_ND TOP IN MEMORY.*)
              BEGIN
              Q1 := STK[TOP-1].MEMADR.DSPLMT ;  P1 := STK[TOP-1].MEMADR.LVL ;
              BASE(Q1,P1,B1) ;
              GENRX(XA,RGADR,Q1,B1,P1) ;
              STK[TOP-1].VPA := RGS ;
              STK[TOP-1].RGADR := RGADR ;
              END
          ELSE (*VPA = MEM*)
            BEGIN  IF STK[TOP-1].VPA <> RGS THEN  LOAD(STK[TOP-1]) ;
            Q1 := MEMADR.DSPLMT ;  P1 := MEMADR.LVL ;
            BASE(Q1,P1,B1) ;
            GENRX(XA,STK[TOP-1].RGADR,Q1,B1,P1) ;
            END

        ELSE (*NOT (VRBL AND STK[TOP-1].VRBL) *)

          IF VRBL THEN
            BEGIN  FPA.LVL := STK[TOP-1].FPA.LVL ;
            FPA.DSPLMT := STK[TOP-1].FPA.DSPLMT ;
            DTYPE := ADR ;
            STK[TOP-1] := STK[TOP] ;
            END
        END (*WITH STK...*) ;

      END (*PIXA*) ;

    PMOV :
      BEGIN  TOP := TOP-2 ;
      IF Q > 0 THEN (*FORWARD MOVE*)  SOPERATION(STK[TOP], STK[TOP+1])
      ELSE
         BEGIN  (*BACKWARD MOVE*)
         Q := ABS(Q);   SOPERATION(STK[TOP+1], STK[TOP]);
         END;
      END (*PMOV*) ;


    (* CONTROL/BRANCH OPERATIONS *)

    PUJP,PFJP,PXJP,PMST,PCUP,PENT,PLOC,
    PRET,PCSP,PSTP,PLAB,PDEF,PCHK :  COPERATION ;


    (* UNARY OPERATIONS *)

    PABI,PABR,PNGI,PNGR,PINC,PDEC,
    PNOT,PODD,PCHR,PORD,PFLO,PFLT,
    PTRC,PSGS,PNEW,PSAV,PRST,PSQI,
"NH" PSQR,PCTS,PCTI,PCRD,PXPO,PRND:  UOPERATION ;


    (* BINARY OPERATIONS *)

    PEQU,PNEQ,PLES,PLEQ,PGRT,PGEQ,
    PADI,PSBI,PMPI,PDVI,PMOD,PAND,
    PIOR,PADR,PSBR,PMPR,PDVR,PINN,
    PINT,PUNI,PDIF :
      BEGIN  TOP := TOP-1 ;  BOPERATION  END


    END (*CASE OPC OF*) ;

  OLDOPC := OPC ;
  END (*ASMNXTINST*) ;


PROCEDURE SETUP ;
(* INITIALIZE GLOBAL VARIABLE/SET FLAGS ETC. *)
(* ----------------------------------------- *)

  VAR  I: INTEGER ;

  BEGIN
"NH" EMPTY := '   ';
"NH" FOR I := 0 TO OPCNT DO PTBL[I] := EMPTY;
"NH" FOR I := 0 TO SPCNT DO CSPTBL[I] := EMPTY;

  PTBL[PLOD]  :='LOD'    ;  " PTBL[PLDO]  :='LDO'    ;" PTBL[PCTS]  :='CTS'  ;
  PTBL[PSTR]  :='STR'    ;  " PTBL[PSRO]  :='SRO'    ;" PTBL[PLOC]   := 'LOC'  ;
  PTBL[PLDA]  :='LDA'    ;  " PTBL[PLAO]  :='LAO'    ;" PTBL[PCTI]  :='CTI'   ;
  PTBL[PSTO]  :='STO'    ;    PTBL[PLDC]  :='LDC'    ;
  PTBL[PLAB]  :='LAB'    ;    PTBL[PIND]  :='IND'    ;
  PTBL[PINC]  :='INC'    ;    PTBL[PMST]  :='MST'    ;
  PTBL[PCUP]  :='CUP'    ;    PTBL[PENT]  :='ENT'    ;
  PTBL[PRET]  :='RET'    ;    PTBL[PCSP]  :='CSP'    ;
  PTBL[PIXA]  :='IXA'    ;    PTBL[PEQU]  :='EQU'    ;
  PTBL[PNEQ]  :='NEQ'    ;    PTBL[PGEQ]  :='GEQ'    ;
  PTBL[PGRT]  :='GRT'    ;    PTBL[PLEQ]  :='LEQ'    ;
  PTBL[PLES]  :='LES'    ;    PTBL[PUJP]  :='UJP'    ;
  PTBL[PFJP]  :='FJP'    ;    PTBL[PXJP]  :='XJP'    ;
  PTBL[PCHK]  :='CHK'    ;    PTBL[PNEW]  :='NEW'    ;
  PTBL[PADI]  :='ADI'    ;    PTBL[PADR]  :='ADR'    ;
  PTBL[PSBI]  :='SBI'    ;    PTBL[PSBR]  :='SBR'    ;
  PTBL[PSGS]  :='SGS'    ;    PTBL[PFLT]  :='FLT'    ;
  PTBL[PFLO]  :='FLO'    ;    PTBL[PTRC]  :='TRC'    ;
  PTBL[PNGI]  :='NGI'    ;    PTBL[PNGR]  :='NGR'    ;
  PTBL[PSQI]  :='SQI'    ;    PTBL[PSQR]  :='SQR'    ;
  PTBL[PABI]  :='ABI'    ;    PTBL[PABR]  :='ABR'    ;
  PTBL[PNOT]  :='NOT'    ;    PTBL[PAND]  :='AND'    ;
  PTBL[PIOR]  :='IOR'    ;    PTBL[PDIF]  :='DIF'    ;
  PTBL[PINT]  :='INT'    ;    PTBL[PUNI]  :='UNI'    ;
  PTBL[PINN]  :='INN'    ;    PTBL[PMOD]  :='MOD'    ;
  PTBL[PODD]  :='ODD'    ;    PTBL[PMPI]  :='MPI'    ;
  PTBL[PMPR]  :='MPR'    ;    PTBL[PDVI]  :='DVI'    ;
  PTBL[PDVR]  :='DVR'    ;    PTBL[PMOV]  :='MOV'    ;
  PTBL[PLCA]  :='LCA'    ;    PTBL[PDEC]  :='DEC'    ;
  PTBL[PSTP]  :='STP'    ;    PTBL[PSAV]  :='SAV'    ;
  PTBL[PRST]  :='RST'    ;    PTBL[PORD]  :='ORD'    ;
  PTBL[PCHR]  :='CHR'    ;    PTBL[PEND]  :='END'    ;
  PTBL[PBGN]  :='BGN'    ;    PTBL[PDEF]  :='DEF'    ;
  PTBL[PRND]  :='RND'    ;
"NH" PTBL[PCRD]:='CRD'    ;    PTBL[PXPO]:='XPO'    ;


  CSPTBL[PGET]  :='GET'  ;    CSPTBL[PPUT]  :='PUT'  ;
  CSPTBL[PRES]  :='RES'  ;    CSPTBL[PRLN]  :='RLN'  ;
  CSPTBL[PREW]  :='REW'  ;    CSPTBL[PWLN]  :='WLN'  ;
  CSPTBL[PWRS]  :='WRS'  ;    CSPTBL[PELN]  :='ELN'  ;
  CSPTBL[PWRI]  :='WRI'  ;    CSPTBL[PWRR]  :='WRR'  ;
  CSPTBL[PWRC]  :='WRC'  ;    CSPTBL[PRDI]  :='RDI'  ;
  CSPTBL[PRDR]  :='RDR'  ;    CSPTBL[PRDC]  :='RDC'  ;
  CSPTBL[PSIN]  :='SIN'  ;    CSPTBL[PCOS]  :='COS'  ;
  CSPTBL[PEXP]  :='EXP'  ;    CSPTBL[PLOG]  :='LOG'  ;
  CSPTBL[PSQT]  :='SQT'  ;    CSPTBL[PATN]  :='ATN'  ;
  CSPTBL[PEOF]  :='EOF'  ;    CSPTBL[PXIT]  :='XIT'  ;
  CSPTBL[PRDS]  :='RDS'  ;    CSPTBL[PTRP]  :='TRP'  ;
  CSPTBL[PSIO]  :='SIO'  ;    CSPTBL[PEIO]  :='EIO'  ;
  CSPTBL[PCLK]  :='CLK'  ;    CSPTBL[PFDF]  :='FDF'  ;
  CSPTBL[PPAG]  :='PAG'  ;    CSPTBL[PNUL]  := EMPTY ;
  CSPTBL[PRDB]  :='RDB'  ;    CSPTBL[PWRB]  :='WRB'  ;
"NH" CSPTBL[PSKP]:='SKP'  ;    CSPTBL[PLIM]:='LIM'  ;
"NH" CSPTBL[PMSG]:='MSG'  ;


                              XTBL[XLTR ]:='LTR '    ;
  XTBL[XL   ]:='L   '    ;    XTBL[XLH  ]:='LH  '    ;
  XTBL[XLD  ]:='LD  '    ;    XTBL[XLR  ]:='LR  '    ;
  XTBL[XLDR ]:='LDR '    ;    XTBL[XIC  ]:='IC  '    ;
  XTBL[XLM  ]:='LM  '    ;    XTBL[XLA  ]:='LA  '    ;
  XTBL[XLPR ]:='LPR '    ;    XTBL[XLCR ]:='LCR '    ;
  XTBL[XLPDR]:='LPDR'    ;    XTBL[XLCDR]:='LCDR'    ;
  XTBL[XLTDR]:='LTDR'    ;

  XTBL[XA   ]:='A   '    ;    XTBL[XAH  ]:='AH  '    ;
  XTBL[XAD  ]:='AD  '    ;    XTBL[XAR  ]:='AR  '    ;
  XTBL[XADR ]:='ADR '    ;    XTBL[XSDR ]:='SDR '    ;
  XTBL[XMDR ]:='MDR '    ;    XTBL[XDDR ]:='DDR '    ;
  XTBL[XAW  ]:='AW  '    ;

  XTBL[XST  ]:='ST  '    ;    XTBL[XSTD ]:='STD '    ;
  XTBL[XSTH ]:='STH '    ;    XTBL[XSTC ]:='STC '    ;
  XTBL[XSTM ]:='STM '    ;

  XTBL[XMVC ]:='MVC '    ;    XTBL[XMVCL]:='MVCL'    ;
  XTBL[XMVI ]:='MVI '    ;

  XTBL[XS   ]:='S   '    ;    XTBL[XSH  ]:='SH  '    ;
  XTBL[XSD  ]:='SD  '    ;    XTBL[XSR  ]:='SR  '    ;

  XTBL[XN   ]:='N   '    ;    XTBL[XNR  ]:='NR  '    ;
  XTBL[XO   ]:='O   '    ;    XTBL[XXOR  ]:='OR  '    ;
  XTBL[XX   ]:='X   '    ;    XTBL[XXR  ]:='XR  '    ;

  XTBL[XM   ]:='M   '    ;    XTBL[XMH  ]:='MH  '    ;
  XTBL[XMD  ]:='MD  '    ;    XTBL[XMR  ]:='MR  '    ;

  XTBL[XD   ]:='D   '    ;
  XTBL[XDD  ]:='DD  '    ;    XTBL[XDR  ]:='DR  '    ;

  XTBL[XBCR ]:='BCR '    ;    XTBL[XBC  ]:='BC  '    ;
  XTBL[XBCTR]:='BCTR'    ;    XTBL[XBCT ]:='BCT '    ;
  XTBL[XBALR]:='BALR'    ;    XTBL[XBAL ]:='BAL '    ;

  XTBL[XC   ]:='C   '    ;    XTBL[XCR  ]:='CR  '    ;
  XTBL[XTM  ]:='TM  '    ;
  XTBL[XCLC ]:='CLC '    ;    XTBL[XCLCL]:='CLCL'    ;

  XTBL[XSLA ]:='SLA '    ;    XTBL[XSLDA]:='SLDA'    ;
  XTBL[XSRA ]:='SRA '    ;    XTBL[XSRDA]:='SRDA'    ;
  XTBL[XSLL ]:='SLL '    ;    XTBL[XSLDL]:='SLDL'    ;
  XTBL[XCD ] :='CD  '    ;    XTBL[XCDR ]:='CDR '    ;

  BRMSK[PEQU] :=  8      ;    BRMSK[PNEQ] :=  7      ;
  BRMSK[PGEQ] := 11      ;    BRMSK[PGRT] :=  2      ;
  BRMSK[PLEQ] := 13      ;    BRMSK[PLES] :=  4      ;

  INVBRM[PEQU]:= PEQU    ;    INVBRM[PNEQ]:= PNEQ    ;
  INVBRM[PGEQ]:= PLEQ    ;    INVBRM[PGRT]:= PLES    ;
  INVBRM[PLEQ]:= PGEQ    ;    INVBRM[PLES]:= PGRT    ;


  FOR I := 0 TO HTSIZE DO  HTBL[I].NAME := EMPTY ;

  OP_SP := TRUE ;
"NH"  FOR OPC := 0 TO OPCNT DO
"NH"  BEGIN NMCDE := PTBL[OPC]; IF NMCDE <> EMPTY THEN ENTERLOOKUP  END;

  OP_SP := FALSE ;
"NH"  FOR CSP := 0 TO SPCNT DO
"NH"  BEGIN NMCDE := CSPTBL[CSP]; IF NMCDE <> EMPTY THEN
"NH"                                    ENTERLOOKUP  END;

  OP_SP := TRUE ;               (*TO PREPARE FOR OPCODE LOOKUP*)

  FOR NXTRG := 0 TO RGCNT DO AVAIL[NXTRG] := TRUE ;

  FOR NXTRG := 0 TO FPCNT DO AVAILFP[NXTRG] := TRUE ;

  FOR CH := 'A' TO 'Z' DO TYPCDE[CH] := NON ;

  TYPCDE['A'] := ADR ;  TYPCDE['B'] := BOOL ;
  TYPCDE['C'] := CHRC;  TYPCDE['I'] := INT ;
  TYPCDE['M'] := STRG;  TYPCDE['S'] := PSET;
  TYPCDE['P'] := PROC;  TYPCDE['R'] := REEL;
  TYPCDE['N'] := ADR ;  TYPCDE['J'] := INX ;
  TYPCDE['F'] := FORT;  TYPCDE['X'] := FBOOL;
  TYPCDE['Y'] := FINT;  TYPCDE['Z'] := FREAL;

  TOP := 1 ;  CURLVL := 1 ;  BRCND := -1 ;  NEG_CND := FALSE ;  TRACE := FALSE ;
  OLDOPC := PBGN ;  OLDCSP := PSIO ;  MDTAG := PBGN ;
  TXRG := TRG14 ; CSTOP := 0 ; " NXTLOC := 0 ; "
  ZEROBL.LVL := 0 ;  ZEROBL.DSPLMT := 0 ;  ERRORCNT := 0 ;  S370CNT := 0 ;
  LCAFTSAREA := LCAFTMST ;  SAVERGS := TRUE ; SAVEFPRS := FALSE ;
  CLEAR_REG := TRUE ;  PRE_PASS := TRUE ; OS_STYLE := TRUE ;
 "TOTALPCODE := 0 ;"   TOTALBYTES := 0 ;   CASE_FLAG := FALSE ;
 "CSPREGACTIVE := FALSE ;  FILREGACTIVE := FALSE ; "
  FILECNT      := 0 ;      ROUNDFLG     := FALSE ;
  ASM := FALSE ;      (*ASSEMBLY/OBJECT CODE SWITCH*)
  ASMVERB := FALSE ;  (*OUTPUT PROC. INFO. SWITCH*)
  CURPNO  := -1 ;  (* TO FLAG INITIALIZATION OF LINE # PTR TABLE *)
(*#DOING_IO := FALSE ;                                                    ####*)
(*#GET_STAT := FALSE ;                                                    ####*)
  MARK(HEAPMARK) ;

(*FOR CH := 'A' TO 'I' DO  EBCDIC[CH] := 192+ORD(CH) ;
  FOR CH := 'J' TO 'R' DO  EBCDIC[CH] := 199+ORD(CH) ;
  FOR CH := 'S' TO 'Z' DO  EBCDIC[CH] := 207+ORD(CH) ;
  FOR CH := '0' TO '9' DO  EBCDIC[CH] := 213+ORD(CH) ;
  EBCDIC[' ']  :=  64 ;
  EBCDIC['!']  :=  90 ;
  EBCDIC['"']  := 127 ;
  EBCDIC['#']  := 123 ;
  EBCDIC['$']  :=  91 ;
  EBCDIC['%']  := 108 ;
  EBCDIC['&']  :=  80 ;
  EBCDIC[''''] := 125 ;
  EBCDIC['(']  :=  77 ;
  EBCDIC[')']  :=  93 ;
  EBCDIC['*']  :=  92 ;
  EBCDIC['+']  :=  78 ;
  EBCDIC[',']  :=  107;
  EBCDIC['-']  :=  96 ;
  EBCDIC['.']  :=  75 ;
  EBCDIC['/']  :=  97 ;
  EBCDIC[':']  := 122 ;
  EBCDIC[';']  :=  94 ;
  EBCDIC['<']  :=  76 ;
  EBCDIC['=']  := 126 ;
  EBCDIC['>']  := 110 ;
  EBCDIC['?']  := 111 ;
  EBCDIC['@']  := 124 ;
  EBCDIC['[']  := 173 ;
  EBCDIC['×']  := 106 ;
  EBCDIC[']']  := 189 ;
  EBCDIC['^']  :=  95 ;
  EBCDIC['_']  := 109 ;  *)
  "CODE[MXCODE1] := 0 ;  CODE[MXCODE] := 0 ; "(* TO AVOID OVERFLOW *)

  END (*SETUP*) ;



BEGIN (*PCODE_TRANSLATOR*)

INIT := TRUE ;  SETUP ;  INIT := FALSE ;  (*INITIALIZE*)
TIMER := CLOCK(1) ;
(*+#IF GET_STAT THEN  INIT_STAT ;                                        ####+*)

WRITE(OUTPUT,'****':9, '      STANFORD PASCAL POST-PROCESSOR, VERSION OF ',
                       VERSION);
WRITELN(OUTPUT);

REPEAT
  READNXTINST ;
  ASMNXTINST ;
  IF TRACE THEN  DUMPSTK(1,TOP-1) ;
(*+#IF GET_STAT THEN  RECORD_STAT ;                                      ####+*)
 "IF NOT(OPC IN [PLOC, PLAB, PBGN, PEND]) THEN TOTALPCODE := TOTALPCODE+1 ;"
UNTIL OPC = PSTP ;

"NH" TIMER := CLOCK(1) - TIMER ;
WRITELN(OUTPUT) ;  WRITE(OUTPUT,'****':9) ;
IF ERRORCNT > 0 THEN WRITE(OUTPUT,ERRORCNT:8)
ELSE WRITE(OUTPUT, 'NO':8) ;
WRITELN(OUTPUT, ' ASSEMBLY ERROR(S) DETECTED.') ;
WRITELN(OUTPUT);
WRITELN(OUTPUT, '****':9,"TOTALPCODE:8, ' P_INSTRUCTIONS READ,',"TOTALBYTES:8,
                ' BYTES OF CODE GENERATED,', TIMER*0.001:6:2,
                ' SECONDS IN POST_PROCESSING.');

IF S370CNT > 0 THEN
  WRITELN(OUTPUT, '****':9, S370CNT:8,
                  ' "370"-ONLY INSTRUCTION(S) ISSUED.');

(*+#IF GET_STAT THEN   PRINT_STAT ;                                      ####+*)

EXIT(ERRORCNT) ;


END.
