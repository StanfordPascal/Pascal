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
   *   253- PROCEDURE TOO LONG (LARGER THAN 8K BYTES).                *
   *        --> SUBDIVIDE THE PROCEDURE.                              *
   *   256- TOO MANY PROCEDURES/FUNCTIONS REFERENCED IN THIS PROC.    *
   *        --> RECOMPILE THE POST_PROCESSOR WITH  A  LARGER  VALUE   *
   *        FOR PRCCNT.                                               *
   *   259- EXPRESSION TOO COMPLICATED.                               *
   *        -->  SIMPLIFY  THE  EXPRESSION  BY  REARRANGING  AND/OR   *
   *        BREAKING.                                                 *
   *   263- TOO MANY (COMPILER GENERATED) LABELS IN THIS PROCEDURE.   *
   *        --> RECOMPILE THE POST_PROCESSOR WITH  A  LARGER  VALUE   *
   *        FOR LBLCNT.                                               *
   *   300- DIVIDE BY ZERO (RESULT OF CONSTANT PROPAGATION).          *
   *        --> FIX UP THE (CONSTANT) EXPRESSION EVALUATING TO ZERO.  *
   *   301- RANGE ERROR IN STRUCTURED CONSTANT.                       *
   *        --> CORRECT INITIAL VALUE FOR FIELD/ELEMENT OF CONSTANT.  *
   *   302- SUBSCRIPTRANGE ERROR (RESULT OF CONSTANT PROPAGATION).    *
   *        --> FIX UP THE CONSTANT SUBSCRIPT EXPRESSION.             *
   *   303- CONSTANT SET TOO LARGE FOR TARGET VARIABLE IN AN ASSMT.   *
   *        --> CORRECT DECLARATION FOR VARIABLE.                     *
   *                                                                  *
   *   504- SIZE OF ARRAY ELEMENT TOO LARGE.                          *
   *        --> REORDER THE DIMENSIONS OF THE ARRAY (SO THAT THE      *
   *        THE LARGER DIMENSIONS ARE FIRST) OR REDUCE THE RANGE      *
   *        OF THE LOW ORDER (LAST) INDICES.                          *
   *                                                                  *
   *   THE FOLLOWING ERRORS NORMALLY INDICATE AN INCONSISTENCY IN     *
   *   THE COMPILER AND OR THE POST_PROCESSOR.                        *
   *                                                                  *
   *   601- TYPE CONFLICT OF OPERANDS IN THE P_PROGRAM.               *
   *   602- OPERAND SHOULD BE OF TYPE 'ADR'.                          *
   *   604- ILLEGAL TYPE FOR RUN TIME CHECKING.                       *
   *   605- OPERAND SHOULD BE OF TYPE 'BOOL'.                         *
   *   606- UNDEFINED P_INSTRUCTION CODE.                             *
   *   607- UNDEFINED STANDARD PROCEDURE NAME.                        *
   *   608- DISPLACEMENT FIELD OUT OF RANGE                           *
   *   609- SMALL PROC IS LARGER THAN 4K, RESET SHRT_PROC = 350       *
   *   610- BAD HALFWORD INTEGER ALIGNMENT                            *
   *   611- BAD INTEGER ALIGNMENT.                                    *
   *   612- BAD REAL ALIGNMENT.                                       *
   *   614- THE PRE_PASS FILE (PRD) IS INCONSISTENT.                  *
   *   615- OPERAND SHOULD BE OF TYPE 'SET'.                          *
   *   616- CONSISTENCY CHECK ON 'SET' OPS FAILED.                    *
   *   617- BAD DISPLACEMENT FOR STRUCTURED CONSTANT.                 *
   *   618- UNEXPECTED END-OF-FILE WHEN READING P-CODE.               *
   *   619- BAD OPERANDS FOR PACK/UNPACK PROCEDURE.                   *
   *                                                                  *
   *   THIS PROGRAM SHOULD NOT BE COMPILED WITH THE 'D+' OPTION.      *
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
   *   EXTENSIVE MODIFICATIONS MADE BY:                               *
   *                                                                  *
   *                           R. NIGEL HORSPOOL                      *
   *                                                                  *
   *                           SCHOOL OF COMPUTER SCIENCE             *
   *                           MCGILL UNIVERSITY                      *
   *                           805 SHERBROOKE STREET WEST             *
   *                           MONTREAL                               *
   *                           QUEBEC  H3A 2K6   CANADA               *
   *                                                                  *
   *                                                                  *
   ********************************************************************)




CONST
      VERSION  = 'MAY -82';
      MXADR    =  65535 ;
      SHRTINT  =   4095 ;
      HALFINT  =  32700 ;
      STKDPTH  =     15 ;
      MXLVL    =     16 ;
      IDLNGTH  =     12 ;  (*LENGTH OF IDENTIFIERS                            *)
      RGCNT    =      9 ;  (*REGISTER COUNT                                   *)
      FPCNT    =      6 ;  (*FLOATING POINT REG. COUNT                        *)
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
"**"  MXPLNGTH =     32 ;  (*MAX NO. OF BYTES IN A POWER SET                  *)
"**"  MXSETINX =      4 ;  (* = MXPLNGTH DIV 8                                *)
"**"  MXSETIXI =      8 ;  (* = MXPLNGTH DIV 4                                *)
      HDRLNGTH =     32 ;  (*LENGTH OF PROGRAM HEADING                        *)
"@@"  EOFDPLMT =      0 ;  (*POSN OF EOF FLAG WITHIN FILE HEADER             *)
"@@"  EOLDPLMT =      4 ;  (*POSN OF EOL FLAG WITHIN HEADER FOR TEXT FILES   *)
"&&"  FILHDRSZ =      8 ;  (*OFFSET OF FILE COMPONENT WITHIN FILE VAR.       *)

      ADRSIZE  =      4 ;  CHARSIZE = 1 ;  BOOLSIZE = 1 ;
"&&"  INTSIZE  =      4 ;  HINTSIZE = 2 ;  REALSIZE = 8 ;


                          (* LAYOUT OF THE 'GLOBAL' STACK FRAME:              *)

      NEWPTR   =     72 ; (* NEWPTR , OFFSET FROM BOTTOM OF RUNTIME STACK     *)
      HEAPLMT  =     76 ; (* HEAP LIMIT PTR, OFFSET FROM BOTTOM OF STACK      *)
      DYNRUNC  =      0 ; (* # OF COUNTERS , FIELD OFFSET FROM HEAPLMT        *)
"&&"  DYN2LEN  =      8 ; (* LENGTH OF THE DYN. INFO. AREA AT THE END OF HEAP *)
      FNCRSLT  =     72 ; (* FUNCTION RESULT LOCATION, OFFSET FROM MP         *)

      DISPLAY  =     80 ; (* DISPLAY REGS, OFFSET FROM BOTTOM OF RUNTIME STK  *)
"&&"  DISPAREA =     40 ; (* SIZE OF DISPLAY TABLE                            *)
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
"&&"  TRACER   =    236 ; (* CONTROL FLOW TRACE ROUTINE                       *)

      FILEBUFS =    248 ; (* INPUT, OUTPUT, PRD,.... BUFFERS                  *)
"@@"  CLEARBUF =    320 ; (* PRESET BUFER TO ERASE MEMORY WITH ITS CONTENTS   *)
"@@"  PASDATE  =    328 ; (* PREDEFINED DATE VARIABLE                         *)
"@@"  PASTIME  =    338 ; (* PREDEFINED TIME VARIABLE                         *)
"@@"  OSPRM    =    348 ; (* POINTER TO O.S. PARMS RECORD                     *)
"@@"  FRSTGVAR =    352 ; (* FIRST GLOBAL VAR, SHOULD BE A MULTIPLE OF 8      *)

                          (* VARIOUS TABLE SIZES AND MISC. CONSTATNTS         *)

"&&"  HTSIZE   =    200 ; (* HASH TABLE SIZE (MUST EXCEED # OPS + # CSP OPS)  *)

      DBLCNT   =    200 ; (* SIZE OF LITERAL POOL - IN DOUBLE-WORDS           *)
"&&"  DBLDANGER =   190 ; (* SAFE LIMIT FOR NXTDBL                            *)
"**"  INTCNT   =    400 ; (*   = DBLCNT*2                                     *)
"**"  HWCNT    =    800 ; (*   = DBLCNT*4                                     *)
"&&"  CHCNT    =   1600 ; (*   = DBLCNT*8                                     *)
"&&"  LITCNT   =    400 ; (* # OF NUMERIC LITERALS IN A PROC.                 *)
"&&"  LITDANGER =   395 ; (* SAFE LIMIT FOR NXTLIT                            *)
"&&"  PRCCNT   =     50 ; (* # OF PROC'S OR ENTRY PT.S IN ONE CSECT           *)
      LBLCNT   =    220 ; (* # OF LABELS IN A CSECT                           *)
"&&"  MAXCALDPTH =    4 ; (* MAX NESTING OF FUNCTION CALLS IN A STMT.         *)

      MXCODE   =   4092 ; (* MAX OBJECT CODE SIZE (APPROX. 8K BYTES)          *)
      MXCODE1  =   4093 ;
"&&"  MXLNP    =    800 ; (* SIZE OF LINE NUMBER TABLE IN BYTES               *)
      ENDCODE  =   4500 ; (* MXCODE+MXLN DIV 2, LINE NO.S NOT A PART OF CODE  *)
"&&"  TXTCHUNK =     56 ; (* MAX. BYTES PER TXT CARD IN 360 OBJECT DECK       *)

      LESCND   =      4 ;         LEQCND = 13 ;  (* CONDITION CODE SYMBOLS    *)
      GRTCND   =      2 ;         GEQCND = 11 ;
      EQUCND   =      8 ;         NEQCND =  7 ;
      ANYCND   =     15 ;         NOCND  =  0 ;
      TRUCND   =      1 ;         FLSCND =  8 ;

      SHRT_PROC=    550 ; (* LIMIT VALUE FOR A PROC. TO BE CONSIDERED SMALL   *)


     (* OPCODE TABLES  (P-OPCODES / P-STANDARD PROCS / 370-OPCODES )          *)

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
"**"  XNC  =      212 ;
      XO   = "37" 86  ;            XXOR  = "38" 22  ;
"**"  XOC  =      214 ;
      XX   = "39" 87  ;            XXR  = "40" 23  ;
"**"  XXC  =      215 ;

      XM   = "41" 92  ;            XMH  = "42" 76  ;
      XMD  = "43" 108 ;            XMR  = "44" 28  ;

      XD   = "47" 93  ;
      XDD  = "49" 109 ;            XDR  = "50" 29  ;

      XBCR = "51" 7   ;            XBC  = "52" 71  ;
      XBCTR= "53" 6   ;            XBCT = "54" 70  ;
      XBALR= "55" 5   ;            XBAL = "56" 69  ;

      XC   = "57" 89  ;            XCR  = "58" 25  ;
"&&"  XCH  =      73  ;
"**"  XCL  =     85  ;             XCLR  =      21 ;
"**"  XTM  = "59" 145 ;            XCLI =      149 ;
      XCLC = "60" 213 ;            XCLCL= "  "  15 ;

      XSLA = "61" 139 ;            XSLDA= "62" 143 ;
      XSRA = "63" 138 ;            XSRDA= "64" 142 ;
      XSLL = "65" 137 ;            XSLDL= "66" 141 ;
"**"  XSRL =     136 ;             XSRDL=      140 ;
      XCD  = "67" 105 ;            XCDR = "68" 41  ;

"**"  XEX  =      68 ;



TYPE

"&&"  OPTYPE      = (PCTS, PCTI, PLOD, PSTR, PLDA, PLOC, PSTO, PLDC,
"&&"                 PLAB, PIND, PINC, PPOP, PCUP, PENT, PRET, PCSP,
"&&"                 PIXA, PEQU, PNEQ, PGEQ, PGRT, PLEQ, PLES, PUJP,
"&&"                 PFJP, PXJP, PCHK, PNEW, PADI, PADR, PSBI, PSBR,
"&&"                 PSCL, PFLT, PFLO, PTRC, PNGI, PNGR, PSQI, PSQR,
"&&"                 PABI, PABR, PNOT, PAND, PIOR, PDIF, PINT, PUNI,
"&&"                 PINN, PMOD, PODD, PMPI, PMPR, PDVI, PDVR, PMOV,
"&&"                 PLCA, PDEC, PSTP, PSAV, PRST, PCHR, PORD, PDEF,
"&&"                 PRND, PCRD, PXPO, PBGN, PEND, PASE, PSLD, PSMV,
"&&"                 PMST, PUXJ, PXLB, PCST, PDFC, PPAK, UNDEF_OP );
"&&"  CSPTYPE     = (PGET, PPUT, PRES, PRLN, PREW, PWLN, PWRS, PELN,
"&&"                 PWRI, PWRR, PWRC, PRDI, PRDR, PRDC, PRDH, PRDY,
"&&"                 PRDD, PWRD, PWRE, PEOL, PEOF, PXIT, PRDS, PTRP,
"&&"                 PSIO, PEIO, PCLK, PFDF, PPAG, PNUL, PRDB, PWRB,
"&&"                 PSKP, PLIM, PMSG, PEOT, PCTR, PTRA, UNDEF_CSP );
"&&"  DATATYPE    = (BOOL,CHRC,ADR,HINT,INT,PSET,REEL,PROC,STRG,INX,
                     FORT,FINT,FBOOL,FREAL,NON);
      BETA        = ARRAY[1..3] OF CHAR ;
"&&"  DUMMYRNG    = 0 .. 1;
"&&"  HINTEGER    = -32768 .. 32767 ;
      STRNG       = PACKED ARRAY [1..MXSLNGTH] OF CHAR ;
      ALFA        = PACKED ARRAY [1..8] OF CHAR ;
      IDTYPE      = PACKED ARRAY [1..IDLNGTH] OF CHAR ;
      ADRRNG      = 0..MXADR ;
"&&"  LVLRNG      = -2..MXLVL ;
      RGRNG       = LVLRNG ;      (* REGISTER NUMBER RANGE                    *)
"**"  SSETRNG     = SET OF 0..63 ;
"**"  SETINX      = 1 .. MXSETINX ;
"**"  SETRNG      = ARRAY[SETINX] OF SSETRNG ;
      BYTE        = 0..255 ;
      LINE_NUM    = 0..10000 ;
      STKPTR      = 0..STKDPTH ;  (* POINTER TO THE COMPILE_TIME STACK        *)
      LVLDSP      = RECORD DSPLMT: INTEGER; LVL: LVLRNG  END ;
      BANK        = (RGS,MEM,ONSTK,NEITHER) ;    (*WHERE ABOUT OF THE OPERAND *)
      SPTR        = @STRNG;
      ICRNG       = 0..MXCODE1;   (* PROGRAM COUNTER RANGE                    *)
      LBLRNG      = -1..LBLCNT;   (* RANGE OF P_COMPILER GENERATED LABELS     *)
      STRLRNG     = 0..MXSLNGTH;
"&&"  PLNRNG      = 0..MXPLNGTH;
      POSINT      = 0..214748360 ;
"&&"  HEX4        = ARRAY[1..4] OF CHAR;

"**"  SET_S_I   = RECORD
"**"              CASE INTEGER OF
"**"              1: (S: SETRNG);
"**"              2: (I: ARRAY[1..MXSETIXI] OF INTEGER);
"**"              3: (C: ARRAY[1..MXPLNGTH] OF CHAR);
"**"              4: (R: ARRAY[1..MXSETINX] OF REAL)
"**"              END ;

"**"  MNEM_TABLE = ARRAY[0..255] OF ARRAY[1..4] OF CHAR;


      PLABEL      = RECORD
                    NAM : ALFA ;
                    LEN : 0.. IDLNGTH
                    END ;

      DATUM       = RECORD
                    RCNST : REAL ;
                    PCNST : @SETRNG ;
                    STKADR: ADRRNG ;
                    FPA   : LVLDSP ;
"&&"                PLEN: PLNRNG;
"&&"                SCNSTNO: 0..LITCNT;
                    DTYPE: DATATYPE ;
                    VRBL, DRCT : BOOLEAN ;

                    CASE VPA: BANK OF
                       RGS: (RGADR: RGRNG) ;
                       MEM: (MEMADR: LVLDSP)
                    END ;


VAR   OPC, OLDOPC:     OPTYPE;     (* CURRENT/OLD INST. OPCODE                *)
      CSP, OLDCSP:     CSPTYPE;    (* CURRENT STND. PROC. CODE                *)
      NMCDE, EMPTY:    BETA ;      (* CURRENT (SYMBOLIC) PCODE /CSP NAME      *)
      OP_SP :          BOOLEAN ;   (* P INSTR/SP SWITCH                       *)
      INIT:    BOOLEAN ;           (* INITIALIZATION PHASE FLAG               *)
      CH:      CHAR ;              (* CURRENT INPUT CHARACTER                 *)
      BVAL:    BOOLEAN ;
      CHVAL:   CHAR ;
      IVAL:    INTEGER ;
      RVAL:    REAL ;
"**"  PSVAL:   SET_S_I ;
      STRPTR:  SPTR ;
"**"  PSLNGTH: 0 .. MXPLNGTH ;
      SVAL :   STRNG ;
      SLNGTH :  0..MXSLNGTH ;
      CURLVL:  LVLRNG ;            (* CURRENT PROC. STATIC LEVEL              *)
      TOP :    STKPTR ;            (* TOP OF EXPRESSION STACK                 *)
"&&"  CALDPTH : 0 .. MAXCALDPTH ;  (* PROC. CALL NESTING                      *)
      LASTLN,  NXTLNP,
      LASTPC,  LASTPCDIF : INTEGER ;
      LBL1, LBL2, LBL3,
      SEGSZE : PLABEL ;            (* LEFT AND RIGHT LABELS OF INSTRUCTIONS   *)
      OPNDTYPE: DATATYPE ;         (* TYPE OF OPERAND OF INSTRUCTION          *)
"&&"  P, Q, R: INTEGER ;           (* P_Q FIELDS OF INSTRUCTION               *)
"&&"  CADDR,                       (* LOC. OF STRUCT. CONSTANT ITEM           *)
      SP     : ADRRNG ;            (* MEMORY STACK POINTER, NOT USED          *)
      LCAFTSAREA : ADRRNG ;        (* FIRST LOC. AFTER PROC. SAVE AREA        *)
      FILECNT : 0..2 ;             (* COUNT OF ACTIVE FILE ADDRESSES          *)
"&&"  DEBUG_LEV : 0..9 ;           (* DEBUG CHECK LEVEL                       *)
      NXTRG, TXRG: RGRNG ;         (* AQUIRED REGISTERS                       *)

"&&"  DBLALN, OPT_FLG,             (* DWRD ALIGNMENT NEEDED, OPT. IN EFFECT   *)
      FILREGACTIVE,                (* FILE ADDRESS REG. ACTIVE                *)
      CSPREGACTIVE,                (* I/O REGISTERS ACTIVE                    *)
"&&"  CSTBLK, MUSIC,               (* STRUCT. CONST. BLOCK?, MUSIC O.S.?      *)
      CLEAR_REG, NEG_CND,          (* CLEAR BEFORE LOADING THE REG.           *)
      SAVERGS, SAVEFPRS,
      DEBUG, OS_STYLE, CASE_FLAG,
      TRACE, NEWLINE, FLIPDEBUG,
"**"  RUNPROFILE, CKMODE,          (* VARIOUS OPTIONS                         *)
      ASM, FLOW_TRACE,             (* OBJ LISTING, FLOW-TRACING FLAGS         *)
      ASMVERB,                     (* VERBOSE ASSEMBLY                        *)
      GET_STAT :  BOOLEAN ;        (* CURRENTLY UNUSED                        *)
      CURLINE : LINE_NUM ;         (* CURRENT SOURCE LINE NUMBER              *)
"&&"  POOL_SIZE : ICRNG ;          (* LITERAL POOL SIZE FOR STATISTICS ONLY   *)
"&&"  NUMLITS : INTEGER ;          (* NUMBER OF LITERALS, FOR STATISTICS      *)
"&&"  PCAFTLIT: ICRNG ;            (* PC AFTER LITERAL DUMP                   *)
      MDTAG :   OPTYPE;            (* MULTIPLY/DIVIDE TAG                     *)
      HEAPMARK : @INTEGER ;
"**"  TESTCNT  : INTEGER ;
      ZEROBL :   LVLDSP    ;       (* TO CLEAR BASE ,DISPLACEMENT FIELDS      *)
      TOTALBYTES,
      ERRORCNT :  INTEGER ;        (* TOTAL ERROR COUNT, ALSO RETURN CODE     *)
      S370CNT :   INTEGER ;        (* COUNT OF 370-ONLY INSTRUCTIONS GENERATED*)

      I_S_R   : RECORD             (* SET <=> INTEGER <=> REAL, 370 IMPL.ONLY *)
                CASE (*TAG:*) INTEGER OF
                     1:   (I1: INTEGER; I2: INTEGER) ;
"**"                 2:   (S: SSETRNG) ;
                     3:   (R: REAL ) ;
"**"                 4:   (C1, C2, C3, C4: CHAR ) ;
                END ;

      TYPCDE: ARRAY ['A'..'Z'"CHAR"] OF DATATYPE ; (* ENCODING OF TYPE FIELD *)
      STK:   ARRAY [STKPTR] OF DATUM ;       (* EXPRESSION STACK              *)
      AVAIL:   ARRAY [0..RGCNT] OF BOOLEAN ; (*AVAILABLE REGISTERS            *)
      AVAILFP: ARRAY [0..FPCNT] OF BOOLEAN ; (* AVAIL. F.P. REGS              *)

      INVBRM:  ARRAY [PEQU..PLES] OF PEQU..PLES ; (* INV. MAP OF REL. OPCODES *)
      BRMSK:   ARRAY [PEQU..PLES] OF 0..15 ;      (* 370 CONDITION CODES      *)
      BRCND : -1..15 ;                            (* ACTIVE BRANCH MASK       *)
      TIMER : POSINT ;
"**"  XTBL  : @ MNEM_TABLE ;
"&&"  HEXCHARS: ARRAY[0..15] OF CHAR;

      HTBL: ARRAY [0..HTSIZE] OF                  (* HASH TABLE, INST./PROCS *)
                RECORD
                NAME: BETA ;
                CASE "KIND:" BOOLEAN OF
                  FALSE: (OPCDE: OPTYPE) ;
                  TRUE : (SPCDE: CSPTYPE)
                END ;

"&&"  LAST_CC: RECORD   (* REMEMBERS USEFUL COND-CODE MEANINGS *)
"&&"              LPC: ICRNG;  LOP: BYTE;  LR: RGRNG
"&&"           END;

"&&"  TXR_CONTENTS: RECORD   (* REMEMBERS CONTENTS OF REG 14 *)
"&&"                   VALID: BOOLEAN;  LEVEL: LVLRNG;
"&&"                   OFFSET, DISP: ADRRNG;  BASE: RGRNG
"&&"                END;

"&&"  LAST_STR:     RECORD   (* REMEMBERS OPNDS OF LAST STR INSTR. *)
"&&"                   STOPND: LVLDSP;  STRG: RGRNG;
"&&"                   LPC: ICRNG;  STDT: DATATYPE;
"&&"                END;
"&&"
"&&"  LAST_FILE:    RECORD   (* REMEMBERS LAST FILE USED *)
"&&"                   LPC: ICRNG;  LFOPND: LVLDSP;  LFV: BOOLEAN
"&&"                END;
"&&"
"&&"  LAST_MVC:     RECORD   (* REMEMBERS LAST MVC INSTRUCTION *)
"&&"                   LPC: ICRNG;  LLEN: BYTE;
"&&"                END;

  (* PRE-PASS (PRD FILE) INFORMATION                                          *)

      PROC_SIZE  : ICRNG ;
      DATA_SIZE  : ADRRNG ;
      CALL_CNT   : 0..50 ;
      CALL_HIGHER, LARGE_PROC, LARGE_DFRAME, PRE_PASS : BOOLEAN ;


  (* POINTERS TO LAST ELEMENTS OF 'OBJECT' CODE TABLES                        *)


"&&" NXTINT  : 0..INTCNT;  NXTDBL : 0..DBLCNT;  NXTCH : 0..CHCNT;
"&&" NXTPRC, NXTEP  : 0..PRCCNT;
     PC      : ICRNG ;     (* PROGRAM COUNTER DIV 2                          *)
"&&" CPC,                  (* PC FOR CONSTANT BLOCK                           *)
"&&" CSEGSTRT,             (* START FOR CPC IN CURRENT SEGMENT                *)
"&&" CSEGLIMIT : HINTEGER; (* END FOR CPC IN CURRENT SEGMENT                  *)
     MINLBL  : LBLRNG ;    (* STARTING LABEL VALUE FOR CURRENT PROC          *)


  (* DECLARATIONS FOR LITERAL TABLES ...ETC. NEEDED TO GENERATE OBJECT MODULE *)

     CURPNAME : ARRAY[1..12] OF CHAR ;     (*NAME OF THE CURRENT PROC        *)
     CURPNO   : INTEGER ;                  (*CURRENT PROC #                  *)
"&&" NXTLIT: -1 .. LITCNT ;
"**" HW_GAP: -1..HWCNT ;                    (*SPARE HALFWORD SLOT IN TABLE   *)
"**" INT_GAP, IHCONF: -1..INTCNT ;            (*SPARE INTEGER SLOT, CONFLICT  *)
"**" RICONF, RHCONF: -1..DBLCNT ;           (*CONFLICTS WITH REAL TABLE      **)

"&&" CODE     : RECORD CASE INTEGER OF
"&&"            1: (H: ARRAY(/0..ENDCODE/) OF HINTEGER);
"&&"            2: (I: ARRAY(/DUMMYRNG/)   OF  INTEGER);
"&&"            3: (R: ARRAY(/DUMMYRNG/)   OF     REAL);
"&&"            4: (C: ARRAY(/DUMMYRNG/)   OF     CHAR);
"&&"            5: (TXTCARD: ARRAY(/DUMMYRNG/) OF
"&&"                         ARRAY(/1..TXTCHUNK/) OF CHAR);
"&&"            END;

"**" IDP_POOL : RECORD CASE INTEGER OF
"**"              1: ( R: ARRAY[0..DBLCNT] OF REAL );
"**"              2: ( I: ARRAY[0..INTCNT] OF INTEGER );
"&&"              3: ( H: ARRAY[0..HWCNT ] OF HINTEGER );
"**"              4: (S: ARRAY[0..DBLCNT] OF SSETRNG );
"&&"              5: (C: ARRAY[0..CHCNT] OF CHAR );
"**"            END;

"&&" LITTBL : ARRAY[1..LITCNT] OF RECORD LNK: ICRNG END ;

     LBLTBL : ARRAY[0..LBLCNT] OF  RECORD
                                   DEFINED : BOOLEAN ;
"&&"                               LNK     : ICRNG
                                   END ;

     PRCTBL : ARRAY[0..PRCCNT] OF  RECORD
                                   NAME : ALFA ;
"&&"                               LNK  : ICRNG
                                   END ;

"&&" CALSTK : ARRAY[1..MAXCALDPTH] OF RECORD
"&&"                               PFLEV: INTEGER;  DISPSAV: ADRRNG
"&&"                               END;

     PROGHDR: ARRAY[1..HDRLNGTH] OF CHAR;     (*PROGRAM HEADER/DATE/TIME    *)

(*_________________________________________________________________*)

PROCEDURE ERROR(ERRCDE: INTEGER) ;
  BEGIN
  ERRORCNT := ERRORCNT+1 ;
  WRITELN('****  PERROR':17, ERRCDE:8, '( NEAR LINE':13,
       LASTLN:6, 'OF PROCEDURE:':15, CURPNAME:14, ')':2 );
  IF ERRCDE = 253 THEN WRITELN('- PROCEDURE TOO LARGE.':33);
  IF ERRCDE = 256 THEN WRITELN('- TOO MANY PROC/FUNC CALLS IN THIS PROC.':51);
  IF ERRCDE = 259 THEN WRITELN('- EXPRESSION TOO COMPLICATED.':40);
  IF ERRCDE = 263 THEN WRITELN('- TOO MANY CONTROL JUMPS IN THIS PROC.':49);
  IF ERRCDE = 300 THEN WRITELN('- IMPLIED DIVISION BY ZERO.':38);
  IF ERRCDE = 301 THEN WRITELN('- RANGE ERROR IN STRUCTURED CONST.':45);
  IF ERRCDE = 302 THEN WRITELN('- IMPLIED SUBSCRIPTRANGE ERROR.':42);
  IF ERRCDE = 303 THEN WRITELN('- ILLEGAL CONSTANT SET ASSMT.':40);
  IF ERRCDE = 504 THEN WRITELN('- ARRAY COMPONENT TOO LARGE (>32K).':47);
  IF ERRCDE = 618 THEN WRITELN('- UNEXPECTED EOF IN P-CODE INPUT':45);
  END ;

"**" PROCEDURE CHECKFREEREGS;
"**"    (* TO BE INVOKED WHEN COMPILATION STACK IS EMPTY,
"**"       CHECKS THAT ALL REGS HAVE BEEN MARKED AS AVAILABLE *)
"**"  VAR LIST: ARRAY[1..12] OF
"**"              RECORD  RGNO: RGRNG;  KIND: CHAR  END;
"**"      LP:   0..12;   I: RGRNG;
"**"  BEGIN
"&&"    IF TOP <> 1 THEN
"&&"      BEGIN  WRITELN('0    ****  WARNING: STACK HEIGHT =', TOP:3 );
"&&"             TOP := 1
"&&"      END;
"**"    I := 1;  LP := 0;
"**"    REPEAT  I := I + 1;
"**"      IF NOT AVAIL[I] THEN
"**"        BEGIN  LP := LP + 1;
"**"          LIST[LP].RGNO := I;  LIST[LP].KIND := 'G';
"**"          AVAIL[I] := TRUE;
"**"        END;
"**"    UNTIL I >= RGCNT;
"**"    I := 0;
"**"    REPEAT  I := I + 2;
"**"      IF NOT AVAILFP[I] THEN
"**"        BEGIN  LP := LP + 1;
"**"          LIST[LP].RGNO := I;  LIST[LP].KIND := 'F';
"**"          AVAILFP[I] := TRUE;
"**"        END;
"**"    UNTIL I >= FPCNT;
"**"    IF LP > 0 THEN
"**"      BEGIN  WRITELN('0    ****  WARNING: REGISTERS NOT FREED ');
"**"        FOR I := 1 TO LP DO
"&&"          WRITE( LIST[I].KIND:8, 'PR', LIST[I].RGNO:3 );
"**"        WRITELN;  WRITELN( '( NEAR LINE':34, LASTLN:6,
"**"                   'OF PROCEDURE:':15, CURPNAME:14, ')':2 );
"**"      END;
"**"  END  (* CHECKFREEREGS *) ;


PROCEDURE ENTERLOOKUP;
"&&" LABEL 10;
"&&" CONST STEP = 17;  (* MUST BE COPRIME TO HTSIZE *)
     VAR H: 0..HTSIZE ;
  BEGIN
     H := (ORD(NMCDE[1])*64+ORD(NMCDE[2])*4096+ORD(NMCDE[3])) MOD HTSIZE ;
10:  WITH HTBL(/H/) DO
       IF NAME <> NMCDE THEN
         IF NAME <> EMPTY THEN
"&&"        BEGIN  H := H + STEP;  IF H >= HTSIZE THEN H := H - HTSIZE;
"&&"              GOTO 10;  (* NO CHECK FOR CYCLES! *)  END
"&&"      ELSE IF INIT THEN
"&&"             BEGIN  (* ENTER THE ITEM *)
"&&"               NAME := NMCDE;
"&&"               IF OP_SP THEN OPCDE := OPC ELSE SPCDE := CSP
"&&"             END
"&&"           ELSE
"&&"             IF OP_SP THEN OPC := UNDEF_OP ELSE CSP := UNDEF_CSP
"&&"    ELSE
"&&"      IF OP_SP THEN OPC := OPCDE ELSE CSP := SPCDE;
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


PROCEDURE DUMPSTK(STP1, STP2: STKPTR) ;
"&&" CONST TYPNAME: ARRAY(/BOOL..STRG/) OF ARRAY(/1..4/) OF CHAR
"&&"       = ('BOOL', 'CHR ', 'ADR ', 'HINT', 'INT ', 'SET ',
"&&"          'REAL', 'PROC', 'STRG');
  VAR I : STKPTR ;
  BEGIN
  FOR I := STP1 TO STP2 DO
    WITH STK[I] DO
      BEGIN WRITE(OUTPUT,' +++ DEPTH =',I:3,'  FPA =',FPA.LVL:3, FPA.DSPLMT:6) ;
      IF VRBL THEN
      BEGIN
        IF VPA = RGS THEN WRITE(OUTPUT,'  VPA-REG =',RGADR:3)
        ELSE  WRITE(OUTPUT,'  VPA-MEM =',MEMADR.LVL:3,MEMADR.DSPLMT:6) ;
        IF DRCT THEN WRITE(OUTPUT,'  DIRECT ACC.')
          ELSE WRITE(OUTPUT,'  INDIRECT ACC.') ;
        END ;
"&&"  IF DTYPE <= STRG THEN
"&&"    WRITELN(OUTPUT,'(':3,TYPNAME(/DTYPE/),')')
"&&"  ELSE WRITELN(OUTPUT,'(ETC.)':9);
      END (*WITH*) ;
  END (*DUMPSTK*) ;


"&&" PROCEDURE HEXHW( HW: HINTEGER; VAR HEX: HEX4 );
"&&"   (* CONVERTS HALFWORD TO 4 HEXADECIMAL CHARACTERS *)
"&&"   VAR  C: INTEGER;  N: 1..4;
"&&"   BEGIN
"&&"     C := 65536 + HW;  (* ELIMINATES HW<0 CASE *)
"&&"     FOR N := 4 DOWNTO 1 DO
"&&"       BEGIN  HEX[N] := HEXCHARS[ C MOD 16 ];
"&&"              C := C DIV 16
"&&"       END;
"&&"   END (*HEXHW*) ;


PROCEDURE READNXTINST ;
(* TO READ AND DECODE NEXT P_INSTRUCTION *)
(* ------------------------------------- *)
"**" LABEL 10 ;
"**" CONST SL16 = 65536;
"**"  VAR  I, J, K: INTEGER ;   DUMMYCH, CH1: CHAR ;  PREV_ASM: BOOLEAN;
"&&"     TEMPLBL: ARRAY [1..12] OF CHAR ;  HLOC: HEX4;

  PROCEDURE READLBL(VAR LBL: PLABEL) ;
    (* SKIPS LEADING BLANKS AND READS THE NEXT CHARACTER SEQUENCE AS A LABEL *)
    (* --------------------------------------------------------------------- *)
"&&" VAR  I: INTEGER;  CH: CHAR;
    BEGIN
    WITH LBL DO
      BEGIN
        NAM := '        ' ;  LEN := 0 ;
"&&"    IF EOL(INPUT) THEN ERROR(618);
        REPEAT
          READ(INPUT, CH) ;  LEN := LEN+1 ;  NAM[LEN] := CH ;
        UNTIL (INPUT@ = ' ') OR (LEN = 8)  ;
"&&"    IF NAM[1] IN ['0'..'9'] THEN
"&&"      BEGIN  CADDR := 0;  I := 1;  CH := NAM[1];
"&&"        REPEAT  CADDR := CADDR*10 + ORD(CH) - ORD('0');
"&&"                I := I + 1;  CH := NAM[I];
"&&"        UNTIL NOT (CH IN ['0'..'9'] );
"&&"      END;
        END (* WITH *) ;
    END (*READLBL*) ;

"**" PROCEDURE SKIPBLANKS;
"**"   BEGIN
"&&"     GET(INPUT);
"&&"     IF EOL(INPUT) THEN ERROR(618);
"&&"   END;


    BEGIN (*READNXTINST*)

"**" P := 0;  Q := 0;  LBL1.LEN := 0;
"**" IF INPUT@ <> ' ' THEN  READLBL(LBL1) ;
"**" GET(INPUT);  IF INPUT@ = ' ' THEN SKIPBLANKS;
"**" READ(INPUT,NMCDE);
"**" IF ASM THEN
"&&"   BEGIN  HEXHW( 2*PC, HLOC );
"&&"     WRITE(OUTPUT, HLOC:9, ':  ', LBL1.NAM:LBL1.LEN,
"&&"             ' ':6-LBL1.LEN, NMCDE:4);
"&&"   END;
"**" ENTERLOOKUP;

"**" CASE OPC OF

"**" PADI,PADR,PSBI,PSBR,PFLT,PFLO,PTRC,PRND,PNGI,PNGR,PSQI,
"**" PSQR,PABI,PABR,PNOT,PAND,PIOR,PMOD,PODD,PMPI,PMPR,PDVI,
"**" PDVR,PSTP,PUNI,PINT,PDIF,PINN,PCRD,PLAB,PSAV,PRST,PCHR,
"&&" PORD,PXPO,PPOP,PXLB,PEND :
"**"    BEGIN   (* NO OPERANDS *)
"**"       READLN(INPUT);
"**"       IF ASM THEN WRITELN(OUTPUT);
"**"    END;

"&&" PDEF,PCTI,PLOC,PIXA,PASE,PMOV :
"**"    BEGIN   (* INTEGER OPERAND *)
"**"       READLN(INPUT,Q);
"**"       IF ASM THEN WRITELN(OUTPUT,Q:5);
"**"    END;

"**" PINC,PDEC,PIND :
"**"    BEGIN   (* TYPE-CODE AND INTEGER OPERANDS *)
"&&"       SKIPBLANKS;  OPNDTYPE := TYPCDE[INPUT@];
"**"       READLN(INPUT,CH1,CH,Q);
"**"       IF ASM THEN WRITELN(OUTPUT,CH1:3,Q:5);
"**"    END;

"&&" PNEW,PLDA,PSMV,PSLD,PSCL,PMST :
"**"    BEGIN   (* TWO INTEGER OPERANDS *)
"**"       READLN(INPUT,P,CH,Q);
"**"       IF ASM THEN WRITELN(OUTPUT,' ':2,P:1,' ',Q:1);
"**"    END;

"**" PLOD,PSTR :
"**"   BEGIN   (* TYPE-CODE AND TWO INTEGER OPERANDS *)
"&&"      SKIPBLANKS;  OPNDTYPE := TYPCDE[INPUT@];
"**"      READLN(INPUT,CH1,CH,P,CH,Q);
"**"      IF ASM THEN WRITELN(OUTPUT,CH1:3,P:4,Q:7);
"**"   END;

"&&" PPAK :
"&&"   BEGIN   (* THREE INTEGER OPERANDS *)
"&&"      READLN(INPUT, IVAL, CH, P, CH, Q );
"&&"      IF ASM THEN WRITELN(OUTPUT, ' ':2, IVAL:1, ',', P:1, ',', Q:1 );
"&&"   END;

"&&" PCHK :
"&&"   BEGIN   (* TYPE-CODE AND TWO INTEGER OPERANDS *)
"&&"      SKIPBLANKS;  OPNDTYPE := TYPCDE[INPUT@];
"&&"      READLN(INPUT,CH1,CH,P,CH,Q);
"&&"      IF ASM THEN WRITELN(OUTPUT,CH1:3,' ',P:1,' ',Q:1);
"&&"   END;

"**" PEQU,PNEQ,PLES,PGRT,PLEQ,PGEQ,PSTO,PRET :
"**"    BEGIN   (* TYPE-CODE AND POSSIBLY AN INTEGER OPERAND *)
"**"       SKIPBLANKS;  OPNDTYPE := TYPCDE[INPUT@];
"**"       IF OPNDTYPE = STRG THEN
"**"         BEGIN  READLN(INPUT,CH1,CH,Q);
"**"                IF ASM THEN WRITELN(OUTPUT,CH1:3,Q:5);
"**"         END
"**"       ELSE BEGIN  READLN(INPUT,CH1);
"**"                   IF ASM THEN WRITELN(OUTPUT,CH1:3);
"**"            END;
"**"    END;

"&&" PFJP,PUJP,PXJP,PCTS,PUXJ :
"**"    BEGIN   (* LABEL-NAME OPERAND *)
"**"       READLBL(LBL2);  READLN(INPUT);
"**"       IF ASM THEN WRITELN(OUTPUT,' ',LBL2.NAM:LBL2.LEN);
"**"    END;
"&&"
"&&" PCST :
"&&"    BEGIN   (* PROCEDURE NAME & NUMBER OPERANDS *)
"&&"      PREV_ASM := ASM;
"&&"      READLN(INPUT,CH1,CURPNAME,CURPNO,CH,ASM,CH,GET_STAT,CH,ASMVERB);
"&&"      IF ASM OR PREV_ASM THEN
"&&"        BEGIN  IF NOT PREV_ASM THEN WRITELN(OUTPUT, '0000:   ':12,
"&&"                 LBL1.NAM:LBL1.LEN, ' ':6-LBL1.LEN, NMCDE:4 );
"&&"               WRITELN(OUTPUT,CURPNAME:14,CURPNO:4,',',ASM:1,',',
"&&"                 GET_STAT:1,',',ASMVERB:1);
"&&"        END;
"&&"    END;

"**" PCUP :
"**"    BEGIN   (* TYPE-CODE,LEXIC-LEVEL,LABEL-NAME,INTEGER OPERANDS *)
"**"       SKIPBLANKS;  OPNDTYPE := TYPCDE[INPUT@];
"**"       READ(INPUT,CH1,CH,P,CH);  READLBL(LBL2);
"**"       IF INPUT@=' ' THEN SKIPBLANKS;
"**"       READLN(INPUT,CH,Q);
"**"       IF ASM THEN WRITELN(OUTPUT,CH1:3,P:4,' ',LBL2.NAM:LBL2.LEN,Q:5);
"**"    END;

"**" PBGN :
"&&"    BEGIN   (* STRING OPERAND *)
"&&"       READLN(INPUT,CH,PROGHDR);
"&&"       IF ASM THEN WRITELN(OUTPUT, ' ', PROGHDR);
"**"    END;

"**" PENT :
"**"    BEGIN   (* TYPE-CODE,LEXIC-LEVEL,LABEL,THREE FLAGS,INTEGER OPERANDS*)
"&&"       SKIPBLANKS;  OPNDTYPE := TYPCDE[INPUT@];  PREV_ASM := ASM;
"**"       READ(INPUT,CH1,CH,P,CH);  READLBL(SEGSZE);
"**"       IF INPUT@ = ' ' THEN SKIPBLANKS;
"&&"       READLN(INPUT,CURPNAME,CH,SAVERGS,CH,ASM,CH,GET_STAT,CH,
"&&"              ASMVERB,CH,DEBUG_LEV,CH,CURPNO);
"&&"       DEBUG := DEBUG_LEV >= 2;  FLOW_TRACE := DEBUG_LEV >= 3;
"&&"       IF ASM OR PREV_ASM THEN
"&&"         BEGIN  IF NOT PREV_ASM THEN
"&&"                BEGIN  WRITELN(OUTPUT);  HEXHW(2*PC,HLOC);
"&&"                  WRITE(OUTPUT,HLOC:9,':  ',LBL1.NAM:LBL1.LEN,
"&&"                   ' ':6-LBL1.LEN,NMCDE:4);  END;
"**"           WRITELN(OUTPUT,CH1:3,P:4,SEGSZE.NAM:10,CURPNAME:14,
"&&"             ',',SAVERGS:1,',',ASM:1,',',GET_STAT:1,',',
"&&"             ASMVERB:1,',',DEBUG_LEV:1,',',CURPNO:5);
"&&"           END;
           REPEAT
             READLN(PRD);  READ(PRD, LBL2.NAM);
"&&"         IF EOF(PRD) THEN BEGIN  ERROR(614);  EXIT(614)  END;
           UNTIL LBL2.NAM = '#PROC   ';
             (* POSITION TO NEXT PROC. INFO. *)
           READ(PRD, TEMPLBL) ;
"&&"       IF TEMPLBL <> CURPNAME THEN IVAL := -1
           ELSE  READLN(PRD,IVAL,CALL_HIGHER,PROC_SIZE,DATA_SIZE,FLIPDEBUG);
"&&"       IF IVAL <> CURPNO THEN ERROR(614);
"&&"       LARGE_PROC := (PROC_SIZE > SHRT_PROC) OR DEBUG ;
"**"    END;

"**" PLDC,PLCA,PDFC :
"**"    BEGIN   (* TYPE-CODE,CONSTANT OPERANDS *)
"**"       SKIPBLANKS;  OPNDTYPE := TYPCDE[INPUT@];
"**"       READ(INPUT,CH1);
"**"       CASE OPNDTYPE OF
"&&"       HINT,
"**"       BOOL,INT: BEGIN READLN(INPUT,CH,IVAL);
"**"                       IF ASM THEN WRITELN(OUTPUT,CH1:3,IVAL:10);
"**"                 END;
"**"       CHRC:     BEGIN  READLN(INPUT,CH,CH,CH);
"**"                        IVAL := ORD(CH);
"**"                        IF ASM THEN WRITELN(OUTPUT,'C,''':5,CH,'''');
"**"                 END;
"**"       REEL:     BEGIN  READLN(INPUT,CH,RVAL);
"**"                        IF ASM THEN WRITELN(OUTPUT,'R,':4,RVAL);
"**"                 END;
"&&"       ADR:      BEGIN  READLN(INPUT);  IVAL := -1;
"**"                        IF ASM THEN WRITELN(OUTPUT,'NIL':4);
"**"                 END;
"**"       PSET:     BEGIN  I := 0;
"&&"                 READ(INPUT,CH,CH);
"**"                 IF INPUT@ <> ')' THEN
"**"                   REPEAT
"**"                     I := I + 1;
"**"                     READ(INPUT,P,CH,Q,CH);
"**"                     PSVAL.I[I] := P*SL16 + Q;
"**"                   UNTIL CH = ')' ;
"**"                 PSLNGTH := I * 4;
"**"                 READLN(INPUT);
"**"                 IF ASM THEN BEGIN
"**"                    WRITE(OUTPUT,'S,(':5);
"**"                    FOR Q := 1 TO I DO WRITE(OUTPUT,' ',PSVAL.I[Q]:1);
"**"                    WRITELN(OUTPUT,' )');  END
"**"                 END;
"&&"       PROC:     BEGIN
"&&"                 READ(INPUT,CH);  READLBL(LBL2);  READLN(INPUT);
"&&"                 IF ASM THEN WRITELN(OUTPUT,'P,':4,LBL2.NAM:LBL2.LEN);
"&&"                 END;
"**"       STRG:     BEGIN
"**"                 READLN(INPUT,CH,CH,SVAL);
"**"                 IF ASM THEN
"**"                 BEGIN
"**"                    SLNGTH := MXSLNGTH - 1;
"**"                    WHILE SVAL[SLNGTH+1] <> '''' DO
"**"                       SLNGTH := SLNGTH - 1;
"**"                    WRITELN(OUTPUT,'M,''':5,SVAL:SLNGTH,'''');
"**"                 END;
"**"                 SLNGTH := 0;  I := 0;
"**"                 REPEAT  I := I + 1;
"**"                    IF SVAL[I] = '''' THEN
"**"                      IF SVAL[I+1] <> '''' THEN
"**"                        GOTO 10
"**"                      ELSE I := I + 1;
"**"                    SLNGTH := SLNGTH + 1;
"**"                    IF SLNGTH <> I THEN
"**"                      SVAL[SLNGTH] := SVAL[I];
"**"                 UNTIL FALSE;
"**"    10:        END (*STRG*) ;
"**"       END  (* CASE OPNDTYPE.. *)
"**"    END;

"**" PCSP :
"**"    BEGIN   (* SUBMONITOR OPERATION NAME OPERAND *)
"**"       SKIPBLANKS;  READLN(INPUT,NMCDE);
"**"       OP_SP := FALSE;  ENTERLOOKUP;  OP_SP := TRUE;
"**"       IF ASM THEN WRITELN(OUTPUT,NMCDE:5);
"**"    END;

"&&" OTHERWISE
"**"    BEGIN    (* OPCODE NOT FOUND IN TABLE *)
"&&"       IF NOT ASM THEN WRITE(OUTPUT,LBL1.NAM:LBL1.LEN,' ':6-LBL1.LEN,
"&&"           ' "',NMCDE,'" ');
"&&"       WHILE NOT EOLN DO
"&&"         BEGIN  WRITE(OUTPUT,INPUT@);  GET(INPUT)  END;
"&&"       WRITELN(OUTPUT);  READLN(INPUT);  ERROR(606);
"**"    END;
"**"
"**" END  (* CASE OPC OF .. *)
"**" END  (*READNXTINST*) ;


  PROCEDURE ASMNXTINST ;

  (* TO TRANSLATE THE NEXT P_INSTRUCTION INTO 370 ASSEMBLY/OBJECT CODE *)
  (* ----------------------------------------------------------------- *)

"&&" LABEL 10,20;
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
"&&"  IF PC >= MXCODE THEN
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


"**" PROCEDURE UPD_DBLTBL( PC: ICRNG; R: REAL );
"**" VAR I:INTEGER;
"**"     S_I: RECORD CASE INTEGER OF
"**"          1: ( R: REAL );
"**"          2: ( S: SSETRNG );
"**"          END;
"**" BEGIN
"**"   DBLALN := TRUE; (* INDICATE ALIGNMENT FOR LITERAL POOL *)
"**"   IDP_POOL.R[NXTDBL] := R;  I := 0;
"**"   S_I.R := R;
"**"   WHILE IDP_POOL.S[I] <> S_I.S DO I := I + 1;
"**"   IF I = RICONF THEN   (* AN AMAZING COINCIDENCE HAS OCCURRED *)
"**"     BEGIN  RICONF := -1;  INT_GAP := -1  END;
"**"   IF I = RHCONF THEN   (* DITTO *)
"**"     BEGIN  RHCONF := -1;  HW_GAP := -1  END;
"&&"  NXTLIT := NXTLIT + 1;
"&&"  LITTBL[NXTLIT].LNK := PC;
"**"  IF I = NXTDBL THEN
"**"    NXTDBL := NXTDBL + 1;
"&&"    I := I*8;
"&&"    IF I >= NXTCH THEN NXTCH := I + 8;
"&&"    CODE.H(/PC/) := I;
"**" END  (*UPD_DBLTBL*) ;
"**"
"**" PROCEDURE UPD_HWTBL( PC: ICRNG; H: HINTEGER );
"**" VAR  I, NXTHW: 0..HWCNT;
"**" BEGIN
"**"   IF HW_GAP >= 0 THEN   (* PREVENT MATCH WITH EMPTY SLOT *)
"&&"     IF H = 0 THEN IDP_POOL.H(/HW_GAP/) := -1
"&&"     ELSE IDP_POOL.H[HW_GAP] := 0;
"**"   IF INT_GAP >= 0 THEN  (* PREVENT MATCH WITH EMPTY SLOT *)
"**"     IF H = 0 THEN
"**"       IDP_POOL.I[INT_GAP] := -1
"**"     ELSE IDP_POOL.I[INT_GAP] := 0;
"&&"   NXTHW := NXTDBL*4;  IDP_POOL.H[NXTHW] := H;  I := 0;
"**"   WHILE IDP_POOL.H[I] <> H DO I := I + 1;
"**"   IF I = NXTHW THEN
"**"     IF HW_GAP >= 0 THEN  (* NOW USE EMPTY SLOT *)
"**"       BEGIN  I := HW_GAP;  IDP_POOL.H[I] := H;
"**"              HW_GAP := -1;  IHCONF := -1;  RHCONF := -1
"**"       END
"**"     ELSE
"**"       IF INT_GAP >= 0 THEN (* SPLIT EMPTY INTEGER SLOT *)
"**"         BEGIN  HW_GAP := 2*INT_GAP + 1;  I := HW_GAP - 1;
"**"                IDP_POOL.H[I] := H;
"&&"                IHCONF := INT_GAP;  RHCONF := IHCONF DIV 2;
"&&"                RICONF := -1;  IDP_POOL.H[HW_GAP] := 0;
"**"                INT_GAP := -1
"**"         END
"**"       ELSE
"**"         BEGIN  HW_GAP := NXTHW + 1;  INT_GAP := NXTDBL*2 + 1;
"&&"                RICONF := NXTDBL;  RHCONF := NXTDBL;
"&&"                IHCONF := INT_GAP - 1;
"**"                NXTDBL := NXTDBL + 1;
"**"                IDP_POOL.I[INT_GAP] := 0;
"**"                IDP_POOL.H[HW_GAP]  := 0;
"**"         END;
"&&"  I := I*2;  CODE.H[PC] := I;
"&&"  IF I >= NXTCH THEN NXTCH := I + 2;
"&&"  NXTLIT := NXTLIT + 1;
"&&"  LITTBL[NXTLIT].LNK := PC;
"**" END  (*UPD_HWTBL*) ;
"**"
"**" PROCEDURE UPD_INTTBL( PC: ICRNG; D: INTEGER );
"**" VAR  I, NXTINT: 0..INTCNT;
"**" BEGIN
"**"   IF INT_GAP >= 0 THEN  (* PREVENT MATCH WITH EMPTY SLOT *)
"**"     IF D = 0 THEN
"**"       IDP_POOL.I[INT_GAP] := -1
"**"     ELSE IDP_POOL.I[INT_GAP] := 0;
"**"   NXTINT := NXTDBL*2;  IDP_POOL.I[NXTINT] := D;  I := 0;
"**"     WHILE IDP_POOL.I[I] <> D DO I := I + 1;
"**"     IF I = IHCONF THEN  (* CHECK FOR A COINCIDENCE *)
"**"       BEGIN  HW_GAP := -1;  IHCONF := -1;  RHCONF := -1  END;
"**"     IF I = NXTINT THEN
"**"       IF INT_GAP >= 0 THEN  (* USE EMPTY SLOT INSTEAD *)
"**"         BEGIN  I := INT_GAP;  INT_GAP := -1;  RICONF := -1;
"**"                IDP_POOL.I[I] := D;
"**"         END
"**"       ELSE
"**"         BEGIN  INT_GAP := NXTINT + 1;  RICONF := INT_GAP DIV 2;
"**"                NXTDBL := NXTDBL + 1;
"**"                IDP_POOL.I[INT_GAP] := 0;
"**"         END;
"&&"  I := I*4;  CODE.H[PC] := I;
"&&"  IF I >= NXTCH THEN NXTCH := I + 4;
"&&"  NXTLIT := NXTLIT + 1;
"&&"  LITTBL[NXTLIT].LNK := PC;
"**" END  (*UPD_INTTBL*) ;
"**"
"**" PROCEDURE UPD_SETTBL( PC: ICRNG; PS: SETRNG; L: INTEGER );
"**" VAR   S_I: SET_S_I;  I, J, LD4: INTEGER;
"**" BEGIN  S_I.S := PS;
"**"   IF L = 0 THEN ERROR(616)
"**"   ELSE IF L <= 4 THEN UPD_INTTBL( PC, S_I.I[1] )
"**"   ELSE IF L <= 8 THEN UPD_DBLTBL( PC, S_I.R[1] )
"**"   ELSE BEGIN
"**"     WHILE (L MOD INTSIZE) <> 0 DO  L := L + 1;
"**"     LD4 := L DIV 4;
"**"     I := 2 * NXTDBL;
"**"     IF INT_GAP >= 0 THEN
"**"       IF INT_GAP = I - 1 THEN
"**"         BEGIN  I := I - 1;  INT_GAP := -1;  RICONF := -1  END;
"&&"     CODE.H[PC] := I*4;
"&&"     NXTLIT := NXTLIT + 1;
"&&"     LITTBL[NXTLIT].LNK := PC;
"**"     FOR J := 1 TO LD4 DO
"**"       BEGIN  IDP_POOL.I[I] := S_I.I[J];
"**"              I := I + 1;
"**"       END;
"&&"     IF I*4 > NXTCH THEN NXTCH := I*4;
"**"     IF I > NXTDBL*2 THEN
"**"       BEGIN  NXTDBL := I DIV 2;
"**"         IF ODD(I) THEN
"**"           BEGIN  RICONF := NXTDBL;  NXTDBL := NXTDBL + 1;
"**"                  INT_GAP := I;  IDP_POOL.I[I] := 0;
"**"           END;
"**"       END
"**"   END
"**" END  (*UPD_SETTBL*) ;


    PROCEDURE UPD_PRCTBL(PC : ICRNG ; PRC_NAME : ALFA) ;
      (* TO UPDATE EXTERNAL REFERENCE TABLE *)
      (* ---------------------------------- *)
      VAR I : 0..PRCCNT;
      BEGIN
      PRCTBL[NXTPRC].NAME := PRC_NAME ;  I := 0 ;
      WHILE PRCTBL[I].NAME <> PRC_NAME DO I := I+1 ;
      CODE.H[PC] := PRCTBL[I].LNK ;  PRCTBL[I].LNK := PC ;
      IF I = NXTPRC THEN
"&&"    IF NXTPRC >= NXTEP THEN ERROR(256)
        ELSE
          BEGIN  NXTPRC := NXTPRC+1 ;  PRCTBL[NXTPRC].LNK := 0  END
    END (* UPD_PRCTBL *) ;


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
        BEGIN  WRITELN(' **** INTLBL ':17,INTLBL);  ERROR(263);  EXIT(263)   END
      ELSE
        WITH LBLTBL[INTLBL] DO
          IF DEFINED THEN     (* BACKWARD REFERENCE *)
            IF CASE_FLAG THEN  CODE.H[PC] := LNK*2 (*HALFWORD ADDR.*)
            ELSE  CODE.H[PC] := BASE_DSPLMT(LNK)   (* BASE/DSPLMT HALF WORD   *)
          ELSE
            IF NEWLBL THEN    (* LABEL DEFINITION *)
              BEGIN
              DEFINED := TRUE ;  TPC := LNK ;
              LNK := PC ;  (* SET LABEL VALUE *)
"&&"          WHILE TPC > 1 DO
                BEGIN
                QPC := TPC ; TPC := CODE.H[QPC] ;
                IF TPC < 0 THEN
                  BEGIN  CODE.H[QPC] := PC*2 ;  TPC := ABS(TPC)  END
                ELSE  CODE.H[QPC] := BASE_DSPLMT(PC) ;
                END
              END
            ELSE   (* NOT NEWLBL I.E. FORWARD REFERENCE, TO BE RESOLVED LATER *)
              BEGIN
              IF CASE_FLAG THEN  CODE.H[PC] := -LNK  ELSE  CODE.H[PC] := LNK ;
              LNK := PC
              END ;
      END (* UPD_LBLTBL *) ;


    (* 370 FORMAT CODE GENERATOR (ASSEMBLY/OBJECT CODE) *)
    (* ------------------------------------------------ *)


    PROCEDURE GENRR(OP: BYTE; R1,R2: RGRNG) ;
"&&"  LABEL 10;
      BEGIN
"&&"  IF R1 = TRG14 THEN TXR_CONTENTS.VALID := FALSE;
"&&"  IF OPT_FLG THEN
"&&"   IF (OP=XLTR) OR (OP=XLTDR) THEN
"&&"     WITH LAST_CC DO
"&&"       IF PC = LPC THEN  (* NO INTERVENING INSTRUCTIONS *)
"&&"         IF R1 = R2 THEN
"&&"           IF LR = R1 THEN
"&&"             IF OP = XLTDR THEN
"&&"               IF LOP IN [XAD,XSD,XLCDR,XLPDR,XADR,XSDR,XAD,XSD] THEN
"&&"                 GOTO 10
"&&"               ELSE
"&&"             ELSE (* OP = XLTR *)
"&&"               IF LOP IN [XLPR,XLCR,XNR,XXOR,
                              XXR,XAR,XSR,XAH,XSH,XO,XX,
"&&"                           XN,XSLA,XSRA,XA,XS] THEN GOTO 10;
        (* IF ASM THEN
          WRITELN(OUTPUT,XTBL@[OP]:5, R1:3,',',R2:1 )
        ELSE *)  CODE.H[PC] := OP*SL8 + R1*16 + R2;
"&&"    PC := NEXTPC(1);
"&&"    WITH LAST_CC DO
"&&"      BEGIN  LPC := PC;  LR := R1;  LOP := OP  END;
"&&" 10:
      END (*GENRR*) ;


    PROCEDURE GENRXLIT(OP: BYTE; R: RGRNG; D: INTEGER ; TAG: INTEGER) ;FORWARD ;


    PROCEDURE GENRX(OP:BYTE; R: RGRNG ; D: ADRRNG; X,B: RGRNG) ;
"&&"  LABEL 10;
      BEGIN
"&&"    IF R = TRG14 THEN TXR_CONTENTS.VALID := FALSE;
      IF (D < 0) OR (D > SHRTINT) THEN
        BEGIN  ERROR(608 (*THIS SHOULD NOT BE THE CASE NOW*)) ;
"&&"    TXR_CONTENTS.VALID := FALSE;
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
      (* IF ASM THEN
        BEGIN  WRITE(OUTPUT,XTBL@[OP]:5,R:3,',',D: 1,'(', X: 1 ) ;
        IF B > 0 THEN WRITE(OUTPUT,',',B: 1 ) ;
        WRITELN(OUTPUT,')') ;
        END
      ELSE *)
        BEGIN
"&&"      CODE.H[PC] := OP*SL8  +R*16+X ;  CODE.H[PC+1] := SL12*B+D ;
        END;
"&&"  PC := NEXTPC(2);
"&&"  WITH LAST_CC DO
"&&"    BEGIN  LPC := PC;  LR := R;  LOP := OP  END;
"&&" 10:
      END (*GENRX*) ;

    PROCEDURE GENRXLIT ;
"**"  LABEL 10;
      BEGIN
"&&"  IF R = TRG14 THEN TXR_CONTENTS.VALID := FALSE;
"**"  IF TAG >= 0 THEN
"**"    IF (OP >= XL) AND (OP <= XS) THEN
"**"      IF (D >= -32768) AND (D <= 32767) THEN
"**"        BEGIN  OP := OP - 16;  (* USE HALFWORD INSTR. *)
"**"               TAG := -1;
"**"        END;
"**"    IF OP = XLH THEN
"**"      IF (D >= 0) AND (D <= SHRTINT) THEN
"**"        BEGIN  GENRX( XLA, R, D, 0, 0 );  GOTO 10  END;
"**"    IF OP = XAH THEN
"**"      IF D = -1 THEN
"**"        BEGIN  GENRR( XBCTR, R, 0 );  GOTO 10  END;
"**"    IF OP = XSH THEN
"**"      IF D = 1 THEN
"**"        BEGIN  GENRR( XBCTR, R, 0 );  GOTO 10  END;

      (* IF ASM THEN
        BEGIN   WRITE(OUTPUT,XTBL@[OP]:5,R:3) ;
        IF TAG < 0 THEN  WRITELN(OUTPUT,',=H''',D:1,'''')
        ELSE  WRITELN(OUTPUT,',=A(',D:1,')')
        END
      ELSE *)   (* ^ASM *)
        BEGIN  CODE.H[PC] := OP*SL8  +R*16 ;
"**"    IF TAG<0 THEN UPD_HWTBL( PC+1, D )
"**"             ELSE UPD_INTTBL( PC+1, D );
        END;
"&&" PC := NEXTPC(2);
"&&" WITH LAST_CC DO
"&&"   BEGIN  LPC := PC;  LR := R;  LOP := OP  END;
10:  END (*GENRXLIT*) ;


"&&" PROCEDURE GENRXDLIT(OP: BYTE; R: RGRNG; VAL: REAL);
"&&" LABEL 10;
     VAR I: INTEGER;
     BEGIN
"&&"   IF OP = XLD THEN
"&&"     IF VAL = 0.0 THEN
"&&"       BEGIN  GENRR( XSDR, R, R );  GOTO 10  END;
    (* IF ASM THEN
       WRITELN(OUTPUT, XTBL@[OP]:5, R:3,',=D''', VAL, '''')
      ELSE *)
      BEGIN  CODE.H[PC] := OP*SL8+ R*16+ 00 ;
"**"  UPD_DBLTBL( PC+1, VAL );
      END ;
"&&"  PC := NEXTPC(2);
"&&"  WITH LAST_CC DO
"&&"    BEGIN  LPC := PC;  LR := R;  LOP := OP  END;
"&&" 10:
    END (* GENRXDLIT *);


    PROCEDURE GENRS(OP: BYTE; R1,R2: RGRNG; D: ADRRNG; B: RGRNG ) ;
      BEGIN
"&&"  IF R1 = TRG14 THEN TXR_CONTENTS.VALID := FALSE;
      IF (D < 0) OR (D > SHRTINT) THEN
        BEGIN  IF B <> TXRG THEN   GENRR(XLR,TXRG,B);
          GENRXLIT(XA,TXRG,D,0) ;   D := 0 ;   B := TXRG;
        END ;
      (* IF ASM THEN
"**"    IF (OP <= XSLDA) AND (OP >= XSRL) THEN
"**"      WRITELN(OUTPUT,XTBL@[OP]:5,R1:3,',',D:1,'(',B:1,')')
"**"    ELSE
"**"     WRITELN(OUTPUT,XTBL@[OP]:5,R1:3,',',R2:1,',',D:1,'(',B:1,')')
      ELSE *)
        BEGIN
"&&"    CODE.H[PC] := OP*SL8 +R1*16 +R2 ; CODE.H[PC+1] := B*SL12 +D;
        END;
"&&"  PC := NEXTPC(2);
      END (*GENRS*) ;


"**" PROCEDURE GENRSLIT(OP: BYTE; R1,R2: RGRNG; S: SSETRNG);
"**" BEGIN  I_S_R.S := S;
"&&"   IF R1 = TRG14 THEN TXR_CONTENTS.VALID := FALSE;
"**" (* IF ASM THEN
"**"     WRITELN(OUTPUT,XTBL@[OP]:5,R1:3,',',R2:1,',=F''',
"**"          I_S_R.I1:1,',',I_S_R.I2:1,'''')
"**"   ELSE *)  BEGIN
"**"     CODE.H[PC] := OP*SL8 + R1*16 + R2;  PC := NEXTPC(2);
"**"     UPD_DBLTBL( PC-1, I_S_R.R );
"**"   END;
"**" END  (*GENRSLIT*) ;


    PROCEDURE GENSS(OP,LNGTH: BYTE; D1: ADRRNG; B1: RGRNG; D2:ADRRNG;B2:RGRNG) ;
      BEGIN
      (* IF ASM THEN
        WRITELN(OUTPUT,XTBL@[OP]:5,D1:6,'(',LNGTH:1,',',B1:1, '),',
                     D2:1,'(',B2:1,')' )
      ELSE *)
        BEGIN  CODE.H[PC] := OP*SL8  +(LNGTH-1) ;
"&&"    CODE.H[PC+1] := B1*SL12+D1 ;  CODE.H[PC+2] := B2*SL12+D2 ;
        END;
"&&"  PC := NEXTPC(3);
      END (*GENSS*) ;


    PROCEDURE GENSI(OP: BYTE; D: ADRRNG; B: RGRNG; I: BYTE) ;
      BEGIN
      (* IF ASM THEN
        WRITELN(OUTPUT,XTBL@[OP]:5,D:8,'(',B:1,'),',I:1)
      ELSE *)
        BEGIN
"&&"      CODE.H[PC] := OP*SL8  +I ; CODE.H[PC+1] := B*SL12+D ;
        END;
"&&"  PC := NEXTPC(2);
      END (*GENSI*) ;


"**" (* PROCEDURE PRINT_SET( S: SETRNG; LNGTH: BYTE; COL: INTEGER );
"**"   VAR  I, INDNT: INTEGER;  DELIM: CHAR;
"**" BEGIN
"**"       PSVAL.S := S;  DELIM := '''';  WRITE(OUTPUT,'=F');
"**"       COL := COL + 2;
"**"       FOR I := 1 TO (LNGTH+3) DIV 4 DO
"**"         BEGIN  INDNT := FLDW(PSVAL.I[I]) + 1;
"**"           IF (COL+INDNT) < 72 THEN
"**"             WRITE(OUTPUT,DELIM,PSVAL.I[I]:1)
"**"           ELSE BEGIN
"**"             WRITELN(OUTPUT,DELIM,'X':73-COL);
"**"             WRITE(OUTPUT,' ':15,PSVAL.I[I]:1);
"**"             COL := 15;
"**"           END;
"**"           DELIM := ',';  COL := COL + INDNT;
"**"         END;
"**"       WRITELN(OUTPUT,'''');
"**" END (*PRINT_SET*) ;  *)

"**" PROCEDURE GENSSLIT( OP,LNGTH: BYTE; D1: ADRRNG; B1: RGRNG; S: SETRNG );
"**"   VAR COL: INTEGER;
"**" BEGIN
"**"   IF LNGTH = 1 THEN  (* SUBSTITUTE AN IMMEDIATE INST. *)
"**"     BEGIN  I_S_R.S := S[1];
"**"       GENSI( OP-XMVC+XMVI, D1, B1, ORD(I_S_R.C1) );
"**"     END
"**"   ELSE IF LNGTH > 1 THEN
"&&"   BEGIN
"**"     (* IF ASM THEN
"**"       BEGIN
"**"         COL := FLDW(LNGTH) + FLDW(B1) + 16;
"**"         WRITE(OUTPUT,XTBL@[OP]:5,D1:6,'(',LNGTH:1,',',B1:1,'),');
"**"         PRINT_SET(S,LNGTH,COL);
"**"       END
"**"     ELSE *)   BEGIN
"**"       CODE.H[PC] := OP*SL8 + (LNGTH-1);
"**"       CODE.H[PC+1] := B1*SL12 + D1;
"**"       UPD_SETTBL( PC+2, S, LNGTH );
"**"     END;
"&&"     PC := NEXTPC(3);
"&&"     END;
"**" END (*GENSSLIT*) ;


    PROCEDURE GENRXLAB(OP: BYTE; R: RGRNG; LAB: PLABEL; TAG:INTEGER) ;
      BEGIN
"&&"  IF R = TRG14 THEN TXR_CONTENTS.VALID := FALSE;
      (* IF ASM THEN
"&&"       IF CASE_FLAG THEN WRITELN(OUTPUT,' DC AL2(',LAB.NAM:LAB.LEN,
"&&"           '-',PRCTBL[0].NAME,')')
"&&"       ELSE
        BEGIN
        WRITE(OUTPUT,XTBL@[OP]:5, R:3,',') ;
        IF TAG >= 0 THEN  WRITE(OUTPUT,LAB.NAM:LAB.LEN,'(',TAG:1)
        ELSE  BEGIN  IF TAG = -3 THEN  WRITE(OUTPUT,'=V(')
                     ELSE  (* TAG = -1 *)  WRITE(OUTPUT,'=A(') ;
              WRITE(OUTPUT, LAB.NAM: LAB.LEN) ;
              END ;
        WRITELN(OUTPUT,')') ;  PC := NEXTPC(2);
        END
      ELSE *)
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
          CODE.H[PC] := OP*SL8  +R*16+TAG ;  PC := NEXTPC(2)
          END
        END (*GENRXLAB*) ;


"&&" PROCEDURE GENRELRX( OP: BYTE; R: RGRNG; OFFSET: HINTEGER );
"&&"   (* OPERAND OF RX INST. IS "*+2*OFFSET"  *)
"&&"   BEGIN
"&&"  (* IF ASM THEN WRITELN(OUTPUT, XTBL@[OP]:5, R:3,
"&&"                   ',*+',2*OFFSET:1) ELSE *)
"&&"        BEGIN  GENRX( OP, R, 0, 0, 0 );
"&&"               CODE.H[PC-1] := BASE_DSPLMT( PC+OFFSET-2 );
"&&"        END;
"&&"   END (*GENRELRX*) ;


"&&" PROCEDURE BRANCH_CHAIN( LPC: ICRNG );
"&&"   LABEL 10;
"&&"   CONST X47F0 = 18416;  X4700 = 18176;  X4000 = 16384;
"&&"         XC000 = 19152;  MAXCNT = 5;
"&&"   VAR   BC15, TI, DI:  RECORD CASE INTEGER OF
"&&"                        1: ( I: INTEGER );
"&&"                        2: ( S: SET OF 0..31 );
"&&"                        END;
"&&"         TPC, DPC: ICRNG;  CNT: 0..MAXCNT;  TIOP: INTEGER;
"&&"   BEGIN
"&&"     BC15.I := X47F0;  TPC := PCAFTLIT;
"&&"     REPEAT
"&&"       TI.I := CODE.H[TPC];
"&&"       IF TI.I > X4700 THEN
"&&"         IF TI.S <= BC15.S THEN  (* MUST BE UNINDEXED BC INSTR. *)
"&&"           BEGIN  CNT := 0;
"&&"             REPEAT
"&&"               TIOP := CODE.H[TPC+1];
"&&"               IF TIOP < 0 THEN TIOP := TIOP + 65536;
"&&"               DPC := (TIOP MOD SL12) DIV 2;
"&&"               TIOP := TIOP DIV SL12 - PBR1;
"&&"               IF TIOP < 0 THEN GOTO 10;
"&&"               IF TIOP > 0 THEN
"&&"                 IF TIOP > 1 THEN GOTO 10
"&&"                 ELSE DPC := DPC + 2046;
"&&"               IF DPC >= LPC THEN GOTO 10;
"&&"               DI.I := CODE.H[DPC];
"&&"               IF DI.I <= X4700 THEN GOTO 10;
"&&"               IF DI.I > X47F0 THEN GOTO 10;
"&&"               IF NOT ( TI.S <= DI.S ) THEN GOTO 10;
"&&"               TIOP := CODE.H[DPC+1];
"&&"               CODE.H[TPC+1] := TIOP;
"&&"               CNT := CNT + 1;
"&&"             UNTIL CNT > MAXCNT;
"&&"        10:END;
"&&"       IF TI.I < 0 THEN TI.I := TI.I + 65536;
"&&"       IF TI.I < X4000 THEN TPC := TPC + 1  (* RR *)
"&&"       ELSE IF TI.I < XC000 THEN TPC := TPC + 2  (* RX *)
"&&"            ELSE TPC := TPC + 3;                 (* SS *)
"&&"     UNTIL TPC >= LPC;
"&&"   END  (* BRANCH_CHAIN *) ;


"&&" PROCEDURE DUMP_LITERALS;
"&&"    (* PROCEDURE TO EMPTY LITERAL POOL INTO CODE ARRAY *)
"&&" VAR  I: INTEGER;  QPC, TPC: ICRNG;
"&&" BEGIN
"&&"   IF OPT_FLG THEN IF NOT DEBUG THEN BRANCH_CHAIN( PC );
"&&"   IF ODD(PC) THEN GENRR( XBCR, 0, 0 );
"&&"   IF DBLALN THEN
"&&"     IF (PC MOD 4) <> 0 THEN GENRX( XBC, 0, 0, 0, 0 );
"&&"   IF NXTLIT > 0 THEN
"&&"     IF (NXTDBL*4+PC) <= 4095 THEN
"&&"       BEGIN
"&&"         FOR I := 1 TO NXTLIT DO
"&&"           BEGIN  TPC := LITTBL[I].LNK;
"&&"             IF TPC > 0 THEN  (* USUAL CASE *)
"&&"               BEGIN  QPC := CODE.H[TPC];
"&&"                 CODE.H[TPC] := BASE_DSPLMT( QPC DIV 2 + PC );
"&&"                 IF ODD(QPC) THEN CODE.H[TPC] := CODE.H[TPC] + 1;
"&&"               END
"&&"             ELSE IF TPC < 0 THEN  (* STRING CONST. NOT YET USED *)
"&&"               WITH STK[-TPC-1] DO
"&&"                 BEGIN  QPC := FPA.DSPLMT;
"&&"                   FPA.LVL := -2;  (* FLAG TO CODE GENERATORS *)
"&&"                   FPA.DSPLMT := BASE_DSPLMT( QPC DIV 2 + PC );
"&&"                   IF ODD(QPC) THEN FPA.DSPLMT := FPA.DSPLMT + 1;
"&&"                 END;
"&&"           END;
"&&"         TPC := NXTDBL*2 - 1;
"&&"         IF INT_GAP = TPC THEN TPC := TPC - 1;
"&&"         POOL_SIZE := POOL_SIZE + TPC*2;
"&&"         NUMLITS   := NUMLITS + NXTLIT;  QPC := PC DIV 2;
"&&"         FOR I := 0 TO TPC DO
"&&"           BEGIN
"&&"             CODE.I(/QPC/) := IDP_POOL.I(/I/);
"&&"             QPC := QPC + 1;
"&&"           END;
"&&"           PC := QPC*2;
"&&"       END
"&&"     ELSE ERROR(253);
"&&"   NXTLIT := 0;  NXTDBL := 0;  IHCONF := -1;  RICONF := -1;
"&&"   RHCONF := -1; INT_GAP := -1; HW_GAP := -1; DBLALN := FALSE;
"&&"   PCAFTLIT := PC;  NXTCH := 0;
"&&" END (* DUMP_LITERALS *) ;


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
"&&"        IF DRCT AND (DTYPE = PSET) THEN
"&&"          IF PLEN > 4 THEN  (* REG. PAIR IN USE *)
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
"**" VAR  K: INTEGER;
"**" BEGIN
"**"    POWER2 := -999;
"**"    IF I > 0 THEN BEGIN
"**"      K := 0;
"**"      WHILE NOT ODD(I) DO
"**"         BEGIN  I := I DIV 2;  K := K + 1;  END;
"**"      IF I = 1 THEN POWER2 := K;   END;
     END (*POWER2*) ;


    PROCEDURE BASE(VAR Q: ADRRNG;VAR P,B: LVLRNG ) ;
    (* TO TRANSLATE A 'LEVEL/OFFSET' P/Q ADDRESS TO 'BASE/INDEX/DISPLACEMENT' *)
    (* ---------------------------------------------------------------------- *)
"&&"  LABEL 10;
"@@"  CONST  MAXDISP  = 4088;
"@@"         SHRTINT2 = 8183;  (* SHRTINT + MAXDISP *)
"&&"  VAR    T, TQ: ADRRNG;  TP: LVLRNG;
      BEGIN  B := 0  ;
"&&"    IF P < 0 THEN GOTO 10;   (* STRING CONSTANT *)
"&&"    TQ := Q;  TP := P;
"&&"    IF OPT_FLG THEN WITH TXR_CONTENTS DO
"&&"      IF TP = LEVEL THEN
"&&"        IF VALID THEN
"&&"          IF TXRG = TRG14 THEN
"&&"            BEGIN  T := TQ - OFFSET + DISP;
"&&"              IF (T>=0) AND (T<=MAXDISP) THEN
"&&"                BEGIN  Q := T;  P := TRG14;
"&&"                       B := BASE;  GOTO 10  END;
"&&"            END;
        IF P > 0 THEN
        IF P = CURLVL THEN  BEGIN  B := LBR ;  P := 0  END
        ELSE IF P = 1 THEN  BEGIN  B := GBR ;  P := 0  END
             ELSE
               BEGIN
               GENRX(XL,TXRG,DISPLAY+4*P,GBR,0) ;  P := TXRG ;
               END;
"@@"    IF (Q < 0) OR (Q > SHRTINT2) THEN
"&&"      BEGIN  Q := Q - 2048;
          IF P > 0 THEN GENRXLIT(XA,P,Q,0)
          ELSE  BEGIN  GENRXLIT(XL,TXRG,Q,0) ;  P := TXRG  END ;
"&&"      Q := 2048
          END
"&&"    ELSE IF Q > SHRTINT THEN
"@@"      BEGIN  GENRX(XLA,TXRG,MAXDISP,B,P);
"@@"             Q := Q - MAXDISP;  P := TXRG;  B := 0;
"@@"      END;
"&&"    IF P = TRG14 THEN
"&&"      WITH TXR_CONTENTS DO
"&&"       BEGIN  VALID := TRUE;  LEVEL := TP;  OFFSET := TQ;
"&&"              DISP := Q;  BASE := B;
"&&"       END;
"&&" 10:
      END (*BASE*) ;


"@@" PROCEDURE CHECKDISP( VAR Q: ADRRNG; VAR P,B: LVLRNG );
"@@"(*  TO ELIMINATE THE RESULT Q=4092 THAT MAY BE GENERATED BY BASE
"@@"    AND WHICH CAUSES TROUBLE FOR OPERATIONS ON SETS             *)
"@@" BEGIN
"@@"   IF Q > (SHRTINT-4) THEN
"@@"     BEGIN
"@@"       GENRX(XLA,TXRG,SHRTINT-4,B,P);
"@@"       Q := Q - (SHRTINT-4);  P := TXRG;  B := 0
"@@"     END
"@@" END;


  PROCEDURE GETADR( STE: DATUM; VAR Q: ADRRNG; VAR P, B: RGRNG) ;  FORWARD ;


  PROCEDURE LOAD(VAR STE: DATUM) ;
    (* LOADS AN STACK ELEMENT INTO A REGISTER, IF NOT ALREADY THERE *)
    (* ------------------------------------------------------------ *)
    VAR P: LVLRNG; Q: ADRRNG; B, R: RGRNG ; OP: BYTE ;

    PROCEDURE FINDMDRG ;
      (*TO FIND A MULTIPLY/DIVIDE REGISTER*)
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
        IF DRCT THEN (*DIRECTLY ACCESSIBLE VARIABLE*)
          CASE DTYPE OF

"&&"      ADR,HINT,INT,BOOL,CHRC:
            BEGIN
              IF VPA = MEM THEN
                BEGIN  FINDMDRG ;
                P := MEMADR.LVL ;   Q := MEMADR.DSPLMT ;
                BASE(Q,P,B) ;
"&&"            CASE DTYPE OF
"&&"  CHRC,BOOL:  BEGIN
                  IF CLEAR_REG THEN
                    GENRR(XSR,NXTRG,NXTRG) ;
                  GENRX(XIC,NXTRG,Q,B,P) ;
                  END;
"&&"  INT,ADR:    GENRX(XL,NXTRG,Q,B,P) ;
"&&"  HINT:       BEGIN  GENRX(XLH,NXTRG,Q,B,P);  DTYPE := INT  END;
"&&"            END;
                VPA := RGS ;  RGADR := NXTRG ;
                END ;
              P := FPA.LVL ;  Q := FPA.DSPLMT ;
              FPA := ZEROBL ;
              IF Q <> 0 THEN
                IF P > 0 THEN
                  BEGIN  BASE(Q,P,B) ;
                  IF P <= 0 THEN P := B ELSE
                    IF B > 0 THEN GENRR(XAR,P,B) ;
                  IF Q = 0 THEN GENRR(XAR,RGADR,P) ELSE
                    GENRX(XLA,RGADR,Q,P,RGADR) ;
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
"**"        IF VPA <> RGS THEN
"**"        BEGIN
"**"          IF VPA = MEM THEN
"**"            BEGIN  P := MEMADR.LVL;  Q := MEMADR.DSPLMT  END
"**"          ELSE BEGIN  P := CURLVL;  Q := STKADR  END;
"**"          BASE( Q, P, B );
"**"          IF PLEN <= 8 THEN
"&&"            IF PLEN > 4 THEN
"**"              BEGIN  FINDRP;
"**"                IF B > 0 THEN IF P > 0 THEN GENRR(XAR,P,B) ELSE P:=B;
"**"                GENRS(XLM,NXTRG,NXTRG+1,Q,P);
"**"              END
"**"            ELSE IF PLEN > 0 THEN
"**"              BEGIN  FINDRG;
"**"                GENRX(XL,NXTRG,Q,P,B);
"**"              END;
"**"          VPA := RGS;  RGADR := NXTRG;
"**"        END  (*PSET*) ;

          END (* CASE DTYPE_ END OF DIRECT VARIABLE LOAD *)

        ELSE (* IF NOT DRCT *)

          BEGIN
          GETADR(STE, Q, P, B) ;   FPA := ZEROBL ;

            CASE DTYPE OF

            ADR, HINT, INT :
              BEGIN   IF VPA = RGS THEN  AVAIL[RGADR] := TRUE ;
              FINDMDRG ;
"&&"          IF DTYPE <> HINT THEN
"&&"            GENRX(XL, NXTRG, Q, B, P)
"&&"          ELSE
"&&"            BEGIN  GENRX(XLH, NXTRG, Q, B, P);  DTYPE := INT  END;
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
"**"          IF PLEN <= 8 THEN
"&&"            IF PLEN > 4 THEN
"**"              BEGIN  FINDRP;
"**"                IF B>0 THEN IF P>0 THEN GENRR(XAR,P,B) ELSE P:=B;
"**"                GENRS(XLM,NXTRG,NXTRG+1,Q,P);
"**"              END
"**"            ELSE IF PLEN > 0 THEN
"**"              BEGIN FINDRG;
"**"                GENRX(XL,NXTRG,Q,P,B);
"**"              END;
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
"&&"            BEGIN  GENRX( XLA, NXTRG, 0, 0, 0 );
"&&"              IF P = -1 THEN LITTBL[SCNSTNO].LNK := PC-1            ;
"&&"              CODE.H[PC-1] := Q;
"&&"            END
              ELSE  GENRXLIT(XL,NXTRG,FPA.DSPLMT,0) ; (*NIL VALUE*)
              FPA := ZEROBL ;
              END ;

            HINT,INT,BOOL,CHRC:
              BEGIN  FINDMDRG ;
              IF FPA.DSPLMT = 0 THEN GENRR(XSR,NXTRG,NXTRG)
              ELSE GENRXLIT(XL,NXTRG,FPA.DSPLMT,0) ;
              FPA := ZEROBL ;
              END ;

            REEL:
               BEGIN
                  FINDFP;
"&&"              GENRXDLIT(XLD,NXTRG,RCNST)
               END (* REEL *);

            PSET:
"**"          IF PLEN <= 8 THEN
"&&"            IF PLEN > 4 THEN
"**"              BEGIN  FINDRP;
"**"                GENRSLIT(XLM,NXTRG,NXTRG+1,PCNST@[1]);
"**"              END
"**"            ELSE IF PLEN > 0 THEN
"**"              BEGIN  FINDRG;  I_S_R.S := PCNST@[1];
"**"                GENRXLIT(XL,NXTRG,I_S_R.I1,0);
"**"              END
"**"            ELSE  (* PLEN = 0 *)
"**"              BEGIN  FINDRG;  PLEN := 4;
"**"                GENRR( XSR, NXTRG, NXTRG );
"**"              END;

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
         GENRS(XSRDA,RGADR,0,32,0);
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
           ELSE  (*VPA = MEM OR VPA = ONSTK *)
"**"             BEGIN
"**"             IF VPA = MEM THEN
"**"               BEGIN  P := MEMADR.LVL;  Q := MEMADR.DSPLMT  END
"**"             ELSE ERROR( 616 );
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
   (* WARNING ON USAGE OF THIS PROCEDURE!!!
      IT IS UNSAFE TO CALL FINDRG (AND THEREFORE ALSO FINDRP, LOAD, ...)
      AFTER GETOPERAND AND BEFORE THE P1 REGISTER HAS BEEN USED       *)
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
"**"               ELSE IF VPA = ONSTK THEN
"**"                 BEGIN  P1 := CURLVL;  Q1 := STKADR;
"**"                   BASE(Q1,P1,B1);
"**"                 END
            (* THE VPA=REG CASE NOT HANDLED HERE *)
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


"**" PROCEDURE GETQB( VAR STE: DATUM;  VAR Q: ADRRNG;
"**"                        VAR P: RGRNG;  L:INTEGER );
"**"   (* GETS BASE-DISPLACEMENT ADDRESS SUCH THAT THE
"**"      DISPLACEMENT < 4096-L                        *)
"**"    VAR  B: RGRNG;
"**" BEGIN
"**"   IF L < 0 THEN L := 0;
"**"   GETOPERAND( STE, Q, P, B );
"**"   IF B > 0 THEN
"**"     IF P > 0 THEN
"**"       IF Q >= (4096-L) THEN
"**"         BEGIN  GENRX( XLA, TXRG, Q, P, B );
"**"                Q := 0;  P := TXRG;  B := 0;
"**"         END
"**"       ELSE GENRR( XAR, P, B )
"**"     ELSE P := B;
"**"   IF Q >= (4096-L) THEN
"**"     BEGIN  GENRX( XLA, TXRG, Q, P, 0 );
"**"            P := TXRG;  Q := 0;
"**"     END;
"**" END  (*GETQB*) ;


  PROCEDURE STORE(STP: STKPTR; INDRCT: BOOLEAN) ;
    (* STORE THE STACK ELEMENT IN THE LOCATION DENOTED BY : *)
    (* IF INDRCT  THEN  2_ND TOP STACK ELEMENT              *)
    (* ELSE P_Q FIELDS OF THE CURRENT INSTRUCTION           *)
    (* ---------------------------------------------------- *)
"&&"  VAR B: RGRNG;  P1: RGRNG;
    BEGIN
    (* LOADS THE ELEMENT INTO A REGISTER *)
     CLEAR_REG := STK[STP].DTYPE <> OPNDTYPE ;
"&&" IF (OPNDTYPE > CHRC) OR STK[STP].VRBL THEN LOAD( STK[STP] );
"&&" CLEAR_REG := TRUE;
"&&" P1 := P;
    IF INDRCT THEN
      BEGIN  IF NOT STK[STP-1].DRCT THEN LOAD(STK[STP-1]) ;
"&&"    GETADR(STK[STP-1],Q,P1,B) ;
        FREEREG(STK[STP-1]) ;
      END
"&&"ELSE  BASE(Q,P1,B) ;

    WITH STK[STP] DO
      BEGIN
      IF VRBL THEN
        IF (NOT DRCT) OR (VPA = MEM) THEN
          IF DTYPE <> OPNDTYPE THEN
"&&"        IF (DTYPE <> INT) OR (OPNDTYPE <> HINT) THEN ERROR(601);

        CASE OPNDTYPE OF

        ADR,INT:   BEGIN  GENRX(XST,RGADR,Q,B,P1) ;  AVAIL[RGADR] := TRUE END ;

"&&"    HINT:      BEGIN  GENRX(XSTH,RGADR,Q,B,P1);
"&&"                      AVAIL[RGADR] := TRUE
"&&"               END;

"&&"    BOOL,CHRC: IF VRBL THEN
"&&"                 BEGIN  AVAIL[RGADR] := TRUE;
"&&"                        GENRX(XSTC,RGADR,Q,B,P1);
"&&"                 END
"&&"               ELSE  (* STORING A CONSTANT *)
"&&"                 BEGIN
"&&"                   IF (FPA.DSPLMT<0) OR (FPA.DSPLMT>255) THEN
"&&"                     BEGIN  ERROR(302);  FPA.DSPLMT := 0  END;
"&&"                   IF B > 0 THEN
"&&"                     IF P1 > 0 THEN GENRR(XAR,P1,B)
"&&"                     ELSE P1 := B;
"&&"                   GENSI(XMVI,Q,P1,FPA.DSPLMT);
"&&"                 END;

        REEL:
          BEGIN
            GENRX(XSTD,RGADR,Q,B,P1) ;
            AVAILFP[RGADR] := TRUE
          END  (* REEL *) ;

        PSET:      ERROR( 616 );

        END (* CASE OPNDTYPE *)

      END (* WITH STK... *)

    END (* STORE *) ;


"**" PROCEDURE CALLSUB;
"**"   VAR  K: INTEGER;
"&&"        DSREG,P1,B1,FPR: RGRNG;  Q1: ADRRNG;  PPCALL: BOOLEAN;
"**" BEGIN   (* Q = STACK DISPLACEMENT OF NEW ACTIV. RECORD *)
"**"   IF ODD(P) THEN  (* SAVEFPRS FOR THIS CALL *)
"**"     BEGIN
"**"       P := P - 1;  SAVEFPRS := TRUE;
"**"       FPR := 0;  K := FPRSAREA;
"**"       REPEAT  FPR := FPR+2;  K := K + REALSIZE;
"**"               IF NOT AVAILFP[FPR] THEN
"**"                 GENRX( XSTD, FPR, K, LBR, 0 );
"**"       UNTIL FPR >= FPCNT;
"**"     END
"**"     ELSE  SAVEFPRS := FALSE;
"&&"     WITH CALSTK[CALDPTH] DO
"&&"       IF DISPSAV > 0 THEN  (* CALL ON PARAMETRIC PROCEDURE *)
"&&"         BEGIN  FINDRG;
"&&"           GENRR( XLR, NXTRG, LBR );  DSREG := NXTRG;
"&&"           IF DISPSAV > 4095 THEN
"&&"             BEGIN  GENRXLIT( XA, DSREG, DISPSAV, 0 );
"&&"                    DISPSAV := 0
"&&"             END;
"&&"           GENSS( XMVC, DISPAREA, DISPSAV, DSREG, DISPLAY, GBR );
"&&"           PPCALL := TRUE;
"&&"           Q1 := PFLEV DIV 10;  P1 := PFLEV MOD 10;
"&&"           BASE( Q1, P1, B1 );
"&&"           IF P1 <= 0 THEN P1 := B1 ELSE
"&&"             IF B1 > 0 THEN GENRR( XAR, P1, B1 );
"&&"           GENSS( XMVC, DISPAREA-4, DISPLAY+4, GBR, Q1+4, P1 );
"&&"           GENRX( XL, TRG15, Q1, P1, 0 );  (* LOAD PROC. ADDR. *)
"&&"         END
"&&"       ELSE PPCALL := FALSE;
"**"   IF Q <= 4095 THEN GENRX( XLA, TRG1, Q, LBR, 0 )
"**"   ELSE BEGIN
"**"     GENRR( XLR, TRG1, LBR );
"**"     GENRXLIT( XA, TRG1, Q, 0 );
"**"   END;
"**"   IF OPNDTYPE IN [FORT,FBOOL,FINT,FREAL] THEN
"**"     BEGIN
"**"       K := P * 2;  (* K = LENGTH OF PARM LIST *)
"**"       IF K > 0 THEN GENSI( XMVI, K-4, TRG1, 128 );
"**"       K := ALIGN( K, REALSIZE );
"**"       GENRX( XST, TRG13, K+4, TRG1, 0 );  (* S/A CHAINING *)
"**"       GENRR( XLR, TRG14, TRG13 );
"**"       GENRX( XLA, TRG13, K, TRG1, 0 );
"**"       GENRX( XST, TRG13, 8, TRG14, 0 );
"&&"     END;
"&&"   IF NOT FLOW_TRACE THEN
"&&"     BEGIN
"&&"       IF NOT PPCALL THEN GENRXLAB( XL, TRG15, LBL2, -3 );
"**"       GENRR( XBALR, TRG14, TRG15 );
"&&"     END
"&&"   ELSE  (* GENERATE SPECIAL CALL CODE *)
"&&"     BEGIN
"&&"       IF PPCALL THEN
"&&"         BEGIN  IF ODD(PC) THEN GENRR(XBCR,NOCND,0);  (* ALIGN TO WORD *)
"&&"           GENRELRX( XST, TRG15, 4 );     (* ST 15,*+8  *)
"&&"           GENRELRX( XBC, ANYCND, 4 );    (* B  *+8     *)
"&&"           CODE.I(/PC DIV 2/) := 0;  PC := NEXTPC(2);
"&&"           GENRX( XBAL, TRG14, TRACER, GBR, 0 );
"&&"           CODE.H[PC] := 2*PC - 8;        (* DC AL2( *-8 )  *)
"&&"         END
"&&"       ELSE
"&&"         BEGIN
"&&"           GENRX( XBAL, TRG14, TRACER, GBR, 0 );
"&&"           UPD_PRCTBL( PC, LBL2.NAM );
"&&"         END;
"&&"       PC := NEXTPC(1);
"&&"     END;
"&&"   IF PPCALL THEN  (* RESTORE DISPLAY *)
"&&"     BEGIN
"&&"       GENSS( XMVC, DISPAREA, DISPLAY, GBR,
"&&"              CALSTK[CALDPTH].DISPSAV, DSREG );
"&&"       AVAIL[DSREG] := TRUE;
"&&"     END;
"&&"   CALDPTH := CALDPTH - 1;
"&&"   IF OPNDTYPE IN [FORT,FBOOL,FINT,FREAL] THEN
"**"       GENRX( XL, TRG13, 4, TRG13, 0 );
"**"   IF SAVEFPRS THEN
"**"     BEGIN  FPR := 0;  K := FPRSAREA;
"**"            REPEAT  FPR := FPR + 2;  K := K + REALSIZE;
"**"                    IF NOT AVAILFP[FPR] THEN
"**"                      GENRX( XLD, FPR, K, LBR, 0 );
"**"            UNTIL  FPR >= FPCNT;
"**"     END;
"**"   CLEAR_REG := TRUE;
"**"   CSPREGACTIVE := FALSE;  OLDCSP := PSIO;  (* R1,R15 USED *)
"**" END (*CALLSUB*) ;


    PROCEDURE GOTOCSP ;
"&&"  VAR  OPC: BYTE;
      BEGIN
      IF NOT CSPREGACTIVE THEN        (* (RE)LOAD PROCADR AND FUNC NUMBER *)
        BEGIN
        LBL3.NAM := '$PASCSP ' ;  LBL3.LEN := 7 ;
        GENRXLAB(XL,TRG15,LBL3,-3) ;
        END ;
      IF CSP <> OLDCSP THEN  GENRX(XLA,TRG1,ORD(CSP)*4,0,0) ;

      IF NOT FILREGACTIVE THEN
        IF CSP IN [PRES,PREW,PGET,PPUT,PRLN,PWLN,PPAG,
"&&"               PSKP,PLIM,PRDB,PWRB,PRDH,PRDY,PEOL,PEOT,
                   PRDC,PWRC,PRDI,PWRI,PRDS,PWRS,PRDR,PWRR,PFDF]  THEN
          WITH STK[TOP-1] DO
            BEGIN
            IF VRBL THEN
"&&"          BEGIN  OPC := XL;  Q1 := MEMADR.DSPLMT;  P1 := MEMADR.LVL  END
"&&"        ELSE
"&&"          BEGIN  OPC := XLA;  Q1 := FPA.DSPLMT;  P1 := FPA.LVL  END;
"&&"        WITH LAST_FILE DO
"&&"          BEGIN  LFOPND.DSPLMT := Q1;  LFOPND.LVL := P1;
"&&"                 LFV := VRBL;
"&&"          END;
            BASE(Q1,P1,B1) ;
"&&"      GENRX( OPC, FILADR, Q1, B1, P1 );
            FILREGACTIVE := TRUE ;
            END ;
      GENRR(XBALR,TRG14,TRG15) ;
"&&"  CSPREGACTIVE := TRUE;  OLDCSP := CSP;  LAST_FILE.LPC := PC;
      END (*GOTOCSP*) ;


  PROCEDURE CALLSTNDRD ;
  (* TO CALL A STANDARD PROCEDURE *)
  (* ---------------------------- *)
"&&"  VAR  Q1,LEN: ADRRNG;  P1,B1: RGRNG;  OPC: BYTE;


    PROCEDURE FILESETUP(PRMCNT: RGRNG) ;
    (* TO SET UP PARAMETERS FOR THE FILE I/O AND CALL THE I/O ROUTINE *)
    (* -------------------------------------------------------------- *)
"&&"  LABEL 10;
      VAR  I : RGRNG ;
"&&"       STP: STKPTR;  CPARM3: INTEGER;
      BEGIN
      STP := TOP-PRMCNT+1 ;
      TOP := STP ;           (* POINTING TO NEXT AVAILABLE STACK ELEMENT *)
      IF PRMCNT >= 2 THEN  (*POTENTIAL REGISTER CONFLICT*)
        WITH STK[STP+1] DO
          IF VRBL AND (VPA = RGS) AND (RGADR = 2) THEN
            BEGIN  FINDRG ;  GENRR(XLR,NXTRG,2) ;
            AVAIL[NXTRG] := FALSE ;  AVAIL[2] := TRUE ;  RGADR := NXTRG ;
            END ;
"&&"  CPARM3 := -1;
      FOR I := 2 TO PRMCNT+1 DO
"&&"    WITH STK[STP] DO
"&&"      BEGIN
"&&"        IF NOT VRBL THEN
"&&"          IF I = 3 THEN CPARM3 := FPA.DSPLMT
"&&"          ELSE IF I = 4 THEN
"&&"            IF CPARM3 = FPA.DSPLMT THEN
"&&"              BEGIN  RGADR := 3;  GOTO 10  END;
"&&"        LOAD( STK[STP] );
"&&"   10:  IF DTYPE <> REEL THEN
              BEGIN (*THE COMMON CASE*)
              IF RGADR <> I THEN
                  IF AVAIL[I]  THEN
                    BEGIN
                    GENRR(XLR,I,RGADR) ;
"&&"                IF RGADR > I THEN AVAIL[RGADR] := TRUE;
                    AVAIL[I] := FALSE ;  RGADR := I ;
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


    BEGIN (*CALLSTNDRD*)
     TOP := TOP - 1;
     CASE CSP OF

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
"NH"       GOTOCSP;  OLDCSP := PSIO;
"NH"       AVAIL[2] := TRUE;  AVAIL[3] := TRUE;
           TOP := TOP-1;
"NH"    END (* PMSG *) ;

"&&"  PXIT, PTRA :
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
        LBL3.NAM := '$PASTRAP' ;  LBL3.LEN := 8 ;
"&&"    IF NOT FLOW_TRACE THEN
"&&"      BEGIN  GENRXLAB( XL, JREG, LBL3, -3 );
"&&"             GENRR( XBALR, RTREG, JREG );
"&&"      END
"&&"    ELSE  (* SPECIAL CALL CODE *)
"&&"      BEGIN  GENRX( XBAL, TRG14, TRACER, GBR, 0 );
"&&"         (*  IF ASM THEN WRITELN(OUTPUT,' DC AL2(',
"&&"                PRCTBL[0].NAME,'-=V($PASTRAP))') ELSE *)
"&&"             UPD_PRCTBL( PC, LBL3.NAM );
"&&"             PC := NEXTPC(1);
"&&"      END;
        END (*PTRP*) ;

      PSIO :
          BEGIN
          IF NOT AVAIL[FILADR] THEN  IF FILECNT = 0 THEN  ERROR(259) ;
          AVAIL[FILADR] := FALSE ;
          FILREGACTIVE := FALSE ;  FILECNT := FILECNT+1 ;
"&&"      WITH LAST_FILE DO
"&&"        IF LPC = PC THEN
"&&"          WITH STK[TOP] DO
"&&"            IF VRBL THEN
"&&"              FILREGACTIVE := LFV AND (LFOPND = MEMADR)
"&&"            ELSE
"&&"              FILREGACTIVE := (NOT LFV) AND (LFOPND = FPA);
          OLDCSP := PSIO ;
          TOP := TOP+1 ;    (* TO CANCEL OUT PREVIOUS SUBTRACT OPERATION *)
          END (*PSIO*) ;

      PEIO :
        BEGIN    (* RELEASE FILE ADR REG ETC. *)
        FILECNT := FILECNT-1 ;
        IF FILECNT = 0 THEN AVAIL[FILADR] := TRUE ;
        FILREGACTIVE := FALSE ;  OLDCSP := PEIO ;
"&&"    LAST_FILE.LPC := PC;
        (* TOP := TOP-1 IS DONE AT ENTRY TO CALLSTNDRD  *)
        END (*PEIO*) ;

      PELN,PEOF :
"@@"    BEGIN
"@@"    WITH STK[TOP] DO
"@@"      BEGIN
"@@"        IF VRBL THEN
"@@"        BEGIN
"@@"          LOAD( STK[TOP] );
"@@"          DRCT := FALSE;  FPA.LVL := -1;
"@@"          IF CSP = PEOF THEN FPA.DSPLMT := EOFDPLMT
"@@"                        ELSE FPA.DSPLMT := EOLDPLMT;
"@@"        END
"@@"        ELSE BEGIN
"@@"          VPA := MEM;  VRBL := TRUE;
"@@"          MEMADR.LVL := FPA.LVL;
"@@"          IF CSP = PEOF THEN MEMADR.DSPLMT := FPA.DSPLMT+EOFDPLMT
"@@"                        ELSE MEMADR.DSPLMT := FPA.DSPLMT+EOLDPLMT;
"@@"          FPA := ZEROBL;
"@@"        END;
"@@"        DTYPE := BOOL
"@@"      END;
"@@"      TOP := TOP + 2;  (*TO BE CORRECTED BY PENDING EIO*)
        END (*PELN.PEOF*) ;

"&&"   PEOL,PEOT :
"&&"    BEGIN
"&&"    FILESETUP(0);
"&&"    WITH STK[TOP-1] DO
"&&"      BEGIN  FINDRG;
"&&"        GENRR( XLR, NXTRG, FILADR );
"&&"        VRBL := TRUE;  DRCT := FALSE;  VPA := RGS;
"&&"        RGADR := NXTRG;  FPA.LVL := -1;
"&&"        IF CSP = PEOL THEN FPA.DSPLMT := EOLDPLMT
"&&"                      ELSE FPA.DSPLMT := EOFDPLMT;
"&&"        DTYPE := BOOL;
"&&"      END;
"&&"    TOP := TOP + 1;
"&&"    END (*PEOL,PEOT*) ;

"&&"   PRDD :
"&&"    BEGIN
"&&"    TOP := TOP - 2;
"&&"    CSP := PGET;
"&&"    FILESETUP(0);
"&&"    GETADR( STK[TOP], Q1, P1, B1 );
"&&"    IF NOT STK[TOP].DRCT THEN
"&&"      BEGIN  GENRX( XL, TXRG, Q1, B1, P1 );
"&&"             Q1 := 0;  B1 := 0;  P1 := TXRG
"&&"      END;
"&&"    FREEREG( STK(/TOP/) );
"&&"    LEN := STK[TOP+1].FPA.DSPLMT;
"&&"    IF LEN <= 256 THEN
"&&"      BEGIN
"&&"        IF B1 > 0 THEN IF P1 > 0 THEN GENRR( XAR, P1, B1 )
"&&"                       ELSE P1 := B1;
"&&"        GENSS( XMVC, LEN, Q1, P1, FILHDRSZ, FILADR );
"&&"      END
"&&"    ELSE
"&&"      BEGIN  FINDRP;
"&&"        GENRX( XLA, NXTRG, Q1, B1, P1 );
"&&"        GENRXLIT( XL, NXTRG+1, LEN, 0 );
"&&"        P1 := NXTRG;  FINDRP;
"&&"        GENRX( XLA, NXTRG, FILHDRSZ, FILADR, 0 );
"&&"        GENRR( XLR, NXTRG+1, P1+1 );
"&&"        GENRR( XMVCL, P1, NXTRG );
"&&"        S370CNT := S370CNT + 1;
"&&"        AVAIL[P1]   := TRUE;  AVAIL[NXTRG]   := TRUE;
"&&"        AVAIL[P1+1] := TRUE;  AVAIL[NXTRG+1] := TRUE;
"&&"      END
"&&"    END (*RDD*);
"&&"
"&&"   PWRD, PWRE :
"&&"    BEGIN
"&L"    LEN := STK[TOP].FPA.DSPLMT;
"&&"    IF NOT FILREGACTIVE THEN
"&&"      WITH STK[TOP-2] DO
"&&"        BEGIN
"&&"          IF VRBL THEN
"&&"            BEGIN  OPC := XL;  P1 := MEMADR.LVL;  Q1 := MEMADR.DSPLMT  END
"&&"          ELSE
"&&"            BEGIN  OPC := XLA;  P1 := FPA.LVL;  Q1 := FPA.DSPLMT  END;
"&&"          BASE( Q1, P1, B1 );
"&&"          GENRX( OPC, FILADR, Q1, B1, P1 );
"&&"          FILREGACTIVE := TRUE;
"&&"        END;
"&&"      WITH STK[TOP-1] DO
"&&"        IF CSP = PWRE THEN
"&&"          BEGIN  LOAD( STK[TOP-1] );
"&&"            IF DTYPE = REEL THEN
"&&"              BEGIN  OPC := XSTD;  AVAILFP[RGADR] := TRUE  END
"&&"            ELSE
"&&"              BEGIN  AVAIL[RGADR] := TRUE;
"&&"                IF LEN = 2 THEN OPC := XSTH ELSE
"&&"                  IF LEN = 1 THEN OPC := XSTC ELSE
"&&"                    OPC := XST;
"&&"              END;
"&&"            GENRX( OPC, RGADR, FILHDRSZ, FILADR, 0 );
"&&"          END
"&&"        ELSE
"&&"          BEGIN  (* CSP = PWRD *)
"&&"            GETADR( STK[TOP-1], Q1, P1, B1 );
"&&"            IF NOT DRCT THEN
"&&"              BEGIN  GENRX( XL, TXRG, Q1, B1, P1 );
"&&"                     Q1 := 0;  B1 := 0;  P1 := TXRG
"&&"              END;
"&&"            FREEREG( STK(/TOP-1/) );
"&&"            IF LEN <= 256 THEN
"&&"              BEGIN
"&&"                IF B1 > 0 THEN IF P1 > 0 THEN GENRR( XAR, P1, B1 )
"&&"                               ELSE P1 := B1;
"&&"                GENSS( XMVC, LEN, FILHDRSZ, FILADR, Q1, P1 );
"&&"              END
"&&"            ELSE
"&&"              BEGIN  FINDRP;
"&&"                GENRX( XLA, NXTRG, Q1, B1, P1 );
"&&"                GENRXLIT( XL, NXTRG+1, LEN, 0 );
"&&"                P1 := NXTRG;  FINDRP;
"&&"                GENRX( XLA, NXTRG, FILHDRSZ, FILADR, 0 );
"&&"                GENRR( XLR, NXTRG+1, P1+1 );
"&&"                GENRR( XMVCL, NXTRG, P1 );
"&&"                S370CNT := S370CNT + 1;
"&&"                AVAIL[P1]    := TRUE;  AVAIL[P1+1] := TRUE;
"&&"                AVAIL[NXTRG] := TRUE;  AVAIL[NXTRG+1] := TRUE;
"&&"              END;
"&&"          END;
"&&"      TOP := TOP - 2;
"&&"      CSP := PPUT;
"&&"      FILESETUP( 0 );
"&&"      END (*PWRD,PWRE*);

      PGET,PPUT,PRLN,PWLN,PRES,PREW,PPAG :
        FILESETUP(0) ;

"&&"  PRDC,PRDI,PRDR,PSKP,PLIM,PRDB,PRDH,PRDY:
        FILESETUP(1) ;

"@@"  PRDS,PWRC,PWRI,PWRB,PFDF :
        FILESETUP(2) ;

      PWRS,PWRR :
        FILESETUP(3) ;
"&&"
"&&" OTHERWISE  BEGIN  WRITE(' -->', NMCDE);  ERROR(607)  END;

      END (*CASE CSP...*) ;

    END (*CALLSTNDRD*) ;

"@@" PROCEDURE CHKOPERATION ;
"&&" VAR  RTA: ADRRNG;  BPC: ICRNG;
"@@" BEGIN
       WITH STK[TOP-1] DO
         IF VRBL THEN   (* GENERATE CODE FOR RUN TIME CHECK *)
"&&"       IF OPNDTYPE = ADR THEN  (* POINTER CHECK *)
             BEGIN
             IF NOT AVAIL[2] THEN
               IF NOT ((VPA = RGS) AND (RGADR = 2)) THEN
                 BEGIN   J := 0 ;  (* CLEAR GPR 2 *)
                 FOR I := TOP-2 DOWNTO 1 DO
                   WITH STK[I] DO
                     IF VRBL THEN
                       IF (NOT DRCT) OR (DTYPE <> REEL) THEN
                         IF (VPA = RGS) AND (RGADR = 2) THEN  J := I;
                 IF J = 0 THEN  ERROR(259)
                 ELSE
                   WITH STK[J]  DO
                   BEGIN   FINDRG ;  (* TRADE GPR2 FOR ANOTHER ONE *)
                     GENRR(XLR,NXTRG,2) ;  (* THIS FREES REG 2 *)
                     RGADR := NXTRG ;
                   END;
                 END ;
"&&"        AVAIL[2] := TRUE;  (* IN CASE *)
"&&"        LOAD( STK[TOP-1] );
"&&"        AVAIL(/2/) := FALSE;
"&&"        IF RGADR <> 2 THEN  (* VALUE IS IN WRONG REG. *)
"&&"          BEGIN  AVAIL(/RGADR/) := TRUE;
"&&"                 GENRR( XLR, 2, RGADR );
"&&"                 RGADR := 2
"&&"          END;
"&&"        RTA := PTRCHK;
"&&"        IF P < 0 THEN RTA := PTACHK;
"&&"        GENRX( XBAL, RTREG, RTA, GBR, 0 );
"&&"        CSPREGACTIVE := FALSE;  OLDCSP := PSIO;  (* R1,R15 DESTROYED *)
"&&"      END
"&&"    ELSE  (* OPNDTYPE <> ADR *)
"&&"      BEGIN
"&&"        IF NOT DRCT THEN LOAD( STK(/TOP-1/) );
"&&"        FPA.DSPLMT := FPA.DSPLMT - P;
"&&"        LOAD( STK(/TOP-1/) );
"&&"        I_S_R.I1 := Q-P;  I_S_R.I2 := P;
"&&"        GENRXDLIT( XCL, RGADR, I_S_R.R );  (*CHEATING WITH DATATYPES*)
"&&"        GENRX( XBC, LEQCND, 0, 0, 0 );
"&&"        BPC := PC;  (* REMEMBER WHERE BRANCH FIX-UP NEEDED *)
"&&"        IF RGADR <> 2 THEN GENRR( XLR, 2, RGADR );
"&&"        IF OPNDTYPE = PROC THEN    RTA := PRMCHK
"&&"        ELSE IF OPNDTYPE = INX THEN RTA := INXCHK
"&&"        ELSE (* HINT OR INT *)      RTA := RNGCHK;
"&&"        GENRX( XBAL, RTREG, RTA, GBR, 0 );
"&&"        UPD_DBLTBL( PC, I_S_R.R );
"&&"        PC := NEXTPC( 1 );  FPA.DSPLMT := P;
"&&"        CODE.H(/BPC-1/) := BASE_DSPLMT( PC );
"&&"      END
         ELSE (* ^ VAR,  I.E. CHECK A CONSTANT EXPRESSION *)
           IF (FPA.DSPLMT < P) OR (FPA.DSPLMT > Q) THEN
             BEGIN
             ERROR(302) ;
             WRITELN(OUTPUT, '****':9, FPA.DSPLMT:9,
"&&"                     ' IS NOT IN THE RANGE:', P:9, Q:10 );
             END ;
"@@"   END (*CHKOPERATION*) ;

"**" PROCEDURE FORCESTK( VAR STE: DATUM );
"**"   (* FORCES A SET INTO RUN-STACK MEMORY *)
"**" VAR  Q1, Q2: ADRRNG;  P1, P2, B1, R: RGRNG;
"**" BEGIN
"**"   WITH STE DO
"**"     IF NOT( DRCT AND VRBL AND (VPA=ONSTK)) THEN
"**"       IF DRCT AND VRBL AND (VPA=RGS) THEN
"**"         BEGIN
"**"           R := RGADR;  VPA := ONSTK;
"**"           IF PLEN = 8 THEN
"**"             BEGIN
"**"               GETQB( STE, Q1, P1, 0 );
"**"               GENRS( XSTM, R, R+1, Q1, P1 );
"**"               AVAIL[R+1] := TRUE;
"**"             END
"**"           ELSE  (* PLEN = 4 *)
"**"             BEGIN
"**"               GETOPERAND( STE, Q1, P1, B1 );
"**"               GENRX( XST, R, Q1, P1, B1 );
"**"             END;
"**"           AVAIL[R] := TRUE;
"**"         END
"**"       ELSE
"**"         IF DRCT AND NOT VRBL THEN
"**"           BEGIN  (* TRANSFER A CONSTANT ONTO RUNSTACK *)
"**"             VPA := ONSTK;  VRBL := TRUE;
"**"             GETQB( STE, Q1, P1, 0 );
"**"             GENSSLIT( XMVC, PLEN, Q1, P1, PCNST@ );
"**"           END
"**"         ELSE  (* SET IS SOMEWHERE IN MEMORY *)
"**"           BEGIN
"**"             GETQB( STE, Q2, P2, 0 );
"**"             TXRG := TRG1;  DRCT := TRUE;  VRBL := TRUE;
"**"             VPA := ONSTK;
"**"             GETQB( STE, Q1, P1, 0 );
"**"             TXRG := TRG14;
"**"             GENSS( XMVC, PLEN, Q1, P1, Q2, P2 );
"**"           END;
"**"   OLDCSP := PSIO;  (* INDICATE LOSS OF REG 1 *)
"**" END  (* FORCESTK *) ;
"**"
"**" PROCEDURE BSETOPS;
"**"   (* BINARY SET OPERATIONS *)
"**" VAR  L, R: DATUM;
"**"      Q1, Q2: ADRRNG;
"**"      P1, P2, B1, B2: RGRNG;
"&&"      I, J, K, MIN, STKADR: INTEGER;  LEN: PLNRNG;
"&&"      LR: BOOLEAN;  OP: BYTE;
"&&"      RNG: ARRAY[1..6] OF ADRRNG;
"**"
"**"   PROCEDURE MINCONSTSET;
"**"     LABEL 10;  VAR I, J: INTEGER;
"**"   BEGIN
"**"     J := MXSETINX * 8;
"**"     IF L.PLEN > 0 THEN
"**"       FOR I := MXSETINX DOWNTO 1 DO
"**"         BEGIN
"**"           I_S_R.S := L.PCNST@[I];
"**"           IF I_S_R.I2 = 0 THEN J := J - 4
"**"                           ELSE GOTO 10;
"**"           IF I_S_R.I1 = 0 THEN J := J - 4
"**"                           ELSE GOTO 10;
"**"         END
"**"     ELSE J := 0;
"**"   10: L.PLEN := J;
"**"     IF J = 0 THEN L.PCNST := NIL;
"**"   END (* MINCONSTSET *) ;
"**"
"&&"   PROCEDURE COMPACT( VAR S: SETRNG;  VAR LEN: PLNRNG;
"&&"                        VAR OFFSET: INTEGER; TAG: CHAR );
"**"   VAR  S_C: RECORD CASE INTEGER OF
"**"               1: ( S: SETRNG );
"**"               2: ( C: ARRAY[1..MXPLNGTH] OF CHAR );
"**"             END;
"&&"        I:   PLNRNG;
"**"   BEGIN
"**"     S_C.S := S;
"**"     WHILE (LEN>0) AND (S_C.C[LEN] = TAG) DO
"**"       LEN := LEN - 1;
"**"     OFFSET := 0;
"**"     WHILE (S_C.C[OFFSET+1] = TAG) AND (OFFSET < LEN) DO
"**"       OFFSET := OFFSET + 1;
"**"     IF OFFSET > 0 THEN
"&&"       BEGIN  LEN := LEN - OFFSET;
"&&"         FOR I := 1 TO LEN DO
"**"           S_C.C[I] := S_C.C[I+OFFSET];
"&&"         FOR I := LEN+1 TO MXPLNGTH DO
"**"           S_C.C[I] := TAG;
"**"         S := S_C.S;
"**"       END;
"**"   END  (*COMPACT*) ;
"**"
"&&"   PROCEDURE INN_OP;
"&&"   LABEL 10;
"&&"   VAR  LN, TOO_MANY: BOOLEAN;  I, J, K: INTEGER;
"&&"   BEGIN
"&&"     IF L.DTYPE <> INT THEN IF L.DTYPE <> HINT THEN ERROR( 601 );
"**"     IF R.DTYPE <> PSET THEN ERROR(615);
"**"     IF NOT L.VRBL THEN
"&&"       IF (R.PLEN*8<=L.FPA.DSPLMT) OR (L.FPA.DSPLMT < 0) THEN
"**"         BEGIN
"**"                L.FPA.LVL := 0;  L.FPA.DSPLMT := 0;
"**"         END
"**"       ELSE
"**"         IF NOT R.VRBL THEN
"**"           BEGIN  (* BOTH OPERANDS ARE CONSTANTS *)
"**"             I := L.FPA.DSPLMT MOD 64;  J := L.FPA.DSPLMT DIV 64;
"**"             L.FPA.DSPLMT := ORD(I IN R.PCNST@[J+1]);
"**"           END
"**"         ELSE
"**"           IF NOT ( R.DRCT AND (R.VPA = RGS )) THEN
"**"             BEGIN  (* LEFT OPND IS CONST, RIGHT OPND IN MEMORY *)
"**"               P1 := L.FPA.DSPLMT MOD 8;  Q1 := L.FPA.DSPLMT DIV 8;
"**"               GETQB( R, Q2, P2, Q1 );
"**"               J := 1;
"**"               FOR I := 6 DOWNTO P1 DO  J := J * 2;
"**"               GENSI( XTM, Q2+Q1, P2, J );
"&&"               BRCND := TRUCND;  L.VRBL := TRUE;
"**"             END
"**"           ELSE
"**"             BEGIN  (* LEFT OPND IS CONST, RIGHT OPND IN REGS *)
"&&"               IF R.PLEN > 4 THEN
"**"                 GENRS( XSLDL, R.RGADR, 0, L.FPA.DSPLMT, 0 )
"**"               ELSE GENRS( XSLL, R.RGADR, 0, L.FPA.DSPLMT, 0 );
"**"               GENRR( XLTR, R.RGADR, R.RGADR );
"**"               BRCND := LESCND;  L.VRBL := TRUE;
"**"             END
"**"     ELSE  (* L.VRBL *)
"**"       IF R.PLEN <= 0 THEN
"**"         BEGIN  FREEREG( L );  L.VRBL := FALSE;
"**"                L.FPA.LVL := 0;  L.FPA.DSPLMT := 0;
"**"         END
"**"       ELSE  (* R.PLEN > 0 *)
"&&"           IF NOT R.VRBL THEN
"&&"             BEGIN   (* TRY FOR BETTER CODE SEQUENCE *)
"&&"               IF NOT L.DRCT THEN LOAD(L);
"&&"               K := R.PLEN*8;  J := 0;  LN := TRUE;  TOO_MANY := FALSE;
"&&"               FOR I := 0 TO K DO
"&&"                 IF ((I MOD 64) IN R.PCNST@[I DIV 64 + 1]) AND (I < K) THEN
"&&"                   IF LN THEN
"&&"                     BEGIN  J := J + 1;
"&&"                       IF J>6 THEN BEGIN  J:=5; TOO_MANY := TRUE  END;
"&&"                            RNG[J] := I;  LN := FALSE;
"&&"                     END
"&&"                   ELSE
"&&"                 ELSE
"&&"                   IF NOT LN THEN
"&&"                     BEGIN  J := J + 1;
"&&"                            RNG[J] := I-1;  LN := TRUE;
"&&"                     END;
"&&"               IF J > 2 THEN
"&&"                 IF ((RNG[J]-RNG[1])<=50) OR TOO_MANY THEN
"&&"                   BEGIN
"&&"                     COMPACT( R.PCNST@, R.PLEN, I, CHR(0) );
"&&"                     L.FPA.DSPLMT := L.FPA.DSPLMT - I*8;
"&&"                     GOTO 10
"&&"                   END;
"&&"               K := RNG[1];
"&&"               L.FPA.DSPLMT := L.FPA.DSPLMT - K;
"&&"               LOAD(L);
"&&"               I := 1;
"&&"               WHILE I < J DO
"&&"                 BEGIN
"&&"                   IF RNG[I] > K THEN
"&&"                     GENRXLIT( XSH, L.RGADR, RNG[I]-K, -1 );
"&&"                   GENRXLIT( XCL, L.RGADR, RNG[I+1]-RNG[I], 0 );
"&&"                   K := RNG[I];  RNG[I] := PC+1;
"&&"                   I := I + 2;
"&&"                   IF I < J THEN
"&&"                 (*  IF ASM THEN WRITELN(OUTPUT,' BNH T',TESTCNT+1:1)
"&&"                     ELSE *)  GENRX( XBC, LEQCND, 0, 0, 0 );
"&&"                 END;
"&&"          (* IF ASM THEN BEGIN  TESTCNT := TESTCNT + 1;
"&&"                 WRITELN(OUTPUT,'T',TESTCNT:1,' DS 0H'); END ELSE *)
"&&"             BEGIN
"&&"               K := BASE_DSPLMT( PC );
"&&"               WHILE I > 2 DO
"&&"                 BEGIN  I := I - 2;
"&&"                        CODE.H[ RNG[I] ] := K;
"&&"                 END;
"&&"             END;
"&&"             BRCND := LEQCND;
"&&"           END  (* NOT R.VRBL *)
"&&"         ELSE
"&&"  10:      BEGIN  (* R.VRBL OR UNOPTIMIZED CASE OF ABOVE *)
"&&"             LOAD( L );
"**"         IF R.PLEN <= 8 THEN
"**"           BEGIN  (* OPERATE ON RIGHT OPND IN REGS *)
"&&"             LOAD( R );
"**"             GENRX( XLA, 0, R.PLEN*8, 0, 0 );
"**"             GENRR( XCLR, L.RGADR, 0 );
"**"             GENRELRX( XBC, GEQCND, 5 );  (* BNL *+10 *)
"&&"             IF R.PLEN > 4 THEN
"**"               BEGIN  AVAIL[R.RGADR+1] := TRUE;
"**"                 GENRS( XSLDL, R.RGADR, 0, 0, L.RGADR );
"**"               END
"**"             ELSE GENRS( XSLL, R.RGADR, 0, 0, L.RGADR );
"**"             GENRR( XLTR, R.RGADR, R.RGADR );
"**"             BRCND := LESCND;
"**"           END
"**"         ELSE
"**"           BEGIN  (* RIGHT OPERAND IN MEMORY *)
"&&"             IF R.VRBL THEN GETQB( R, Q2, P2, 0 )
"&&"             ELSE           BEGIN  P2 := 0;  Q2 := 0  END;
"**"             GENRXLIT( XCL, L.RGADR, R.PLEN*8-1, 0 );
"**"          (* IF ASM THEN
"**"               BEGIN  TESTCNT := TESTCNT + 1;
"**"                      WRITELN(OUTPUT, ' BH T',TESTCNT:1 );
"**"                   END
"**"             ELSE *)
"**"               GENRELRX( XBC, GRTCND, 12 );  (* BH *+24 *)
"**"             GENRX( XLA, TRG1, 7, 0, 0 );
"**"             GENRR( XNR, TRG1, L.RGADR );
"**"             GENRS( XSRA, L.RGADR, 0, 3, 0 );
"**"          (* IF ASM AND (NOT R.VRBL) THEN BEGIN
"**"               WRITE(OUTPUT,' LA 15,');  PRINT_SET(R.PCNST@,R.PLEN,7);
"**"               GENRX( XIC, L.RGADR, 0, L.RGADR, 15 );
"**"               CSPREGACTIVE := FALSE;
"**"             END
"**"             ELSE *)   BEGIN
"**"               GENRX( XIC, L.RGADR, Q2, L.RGADR, P2 );
"**"               IF NOT R.VRBL THEN
"**"                 UPD_SETTBL( PC-1, R.PCNST@, R.PLEN );
"**"             END;
"**"             GENRS( XSLL, L.RGADR, 0, 24, TRG1 );
"**"             GENRR( XLTR, L.RGADR, L.RGADR );
"**"          (* IF ASM THEN WRITELN(PRR,'T',TESTCNT:1,' DS 0H');  *)
"**"             BRCND := LESCND;
"**"           END;
"&&"       END (* L.VRBL *) ;
"**"     FREEREG( L );  FREEREG( R );
"**"     L.DTYPE := BOOL;
"**"   END  (* INN_OP *) ;

"**"   PROCEDURE ASE_OP ;
"**"   BEGIN
"**"     IF Q < 0 THEN  (* OPERANDS ARE IN REVERSE ORDER *)
"**"       BEGIN  L := STK[TOP];  R := STK[TOP-1];  Q := -Q  END;
"**"     IF L.DTYPE <> PSET THEN ERROR(615);
"&&"     IF R.DTYPE <> INT THEN IF R.DTYPE <> HINT THEN ERROR( 602 );
"**"     LOAD(R);
"**"     IF DEBUG THEN
"**"       BEGIN  (* CHECK THAT ELEMENT IS IN RANGE *)
"**"         GENRR( XBALR, TRG14, 0 );
"**"         GENRX( XLA, TRG1, L.PLEN*8-1, 0, 0 );
"**"         GENRR( XCLR, R.RGADR, TRG1 );
"**"         GENRX( XBC, GRTCND, SETCHK, GBR, 0 );
"**"       END;
"**"     IF L.PLEN <= 8 THEN
"**"       BEGIN  (* PRODUCE THE RESULT IN REGS *)
"**"         LOAD(L);
"**"         GENRX( XLA, TRG1, 1, 0, 0 );
"**"         GENRR( XLCR, R.RGADR, R.RGADR );
"&&"         IF L.PLEN > 4 THEN
"**"           BEGIN  GENRR( XSR, TRG0, TRG0 );
"**"             GENRS( XSLDL, TRG0, 0, 63, R.RGADR );
"**"             GENRR( XXOR, L.RGADR, TRG0 );
"**"             GENRR( XXOR, L.RGADR+1, TRG1 );
"**"           END
"**"         ELSE
"**"           BEGIN
"**"             GENRS( XSLL, TRG1, 0, 31, R.RGADR );
"**"             GENRR( XXOR, L.RGADR, TRG1 );
"**"           END;
"**"       END
"**"     ELSE
"**"       BEGIN  (* OPERATE ON SET IN MEMORY *)
"**"         FORCESTK(L);
"**"         GETQB( L, Q1, P1, 0 );
"**"         GENRX( XLA, TRG15, 7, 0, 0 );
"**"         GENRR( XNR, TRG15, R.RGADR );
"**"         GENRS( XSRL, R.RGADR, 0, 3, 0 );
"**"         GENRX( XLA, TRG1, Q1, P1, R.RGADR );
"**"         GENRX( XLA, R.RGADR, 128, 0, 0 );
"**"         GENRS( XSRL, R.RGADR, 0, 0, TRG15 );
"**"         GENRXLIT( XEX, R.RGADR,
"**"            -1778380800 (* OI 0(1),0 *), 0 );
"**"         CSPREGACTIVE := FALSE;  (* INDICATE LOSS OF REG 15 *)
"**"       END;
"**"     AVAIL[R.RGADR] := TRUE;
"**"   END  (* ASE_OP *) ;
"**"
"**" BEGIN
"**"   L := STK[TOP-1];  R := STK[TOP];
"**"   CASE OPC OF
"**"
"**"   PUNI: BEGIN
"**"     IF L.DTYPE <> PSET THEN ERROR(615);
"**"     IF R.DTYPE <> PSET THEN ERROR(615);
"**"     STKADR := L.STKADR;  LEN := L.PLEN;
"**"     IF LEN < R.PLEN THEN LEN := R.PLEN;
"**"     IF R.PLEN > 0 THEN
"**"       IF L.PLEN <= 0 THEN
"**"         IF (R.STKADR<>STKADR) AND R.VRBL AND R.DRCT
"**"             AND (R.VPA=ONSTK) THEN
"**"           BEGIN  L.VRBL := TRUE;  L.DRCT := TRUE;  L.VPA := ONSTK;
"**"             GETQB( L, Q1, P1, 0 );  TXRG := TRG1;
"**"             GETQB( R, Q2, P2, 0 );  TXRG := TRG14;
"**"             GENSS( XMVC, LEN, Q1, P1, Q2, P2 );
"**"           END
"**"         ELSE
"**"           L := R
"**"       ELSE  (* BOTH OPERANDS ARE NON-NULL *)
"**"         IF NOT L.VRBL AND NOT R.VRBL THEN  (* FOLD CONSTANTS *)
"**"           BEGIN
"**"             FOR I := 1 TO MXSETINX DO
"**"               L.PCNST@[I] := L.PCNST@[I] + R.PCNST@[I];
"**"             MINCONSTSET;
"**"           END
"**"         ELSE
"**"           IF LEN <= 8 THEN  (* PRODUCE RESULT IN REGISTERS *)
"**"             BEGIN  LR := TRUE;
"**"               IF L.PLEN < R.PLEN THEN
"**"                 BEGIN  LOAD(R);  LR := FALSE  END
"**"               ELSE IF L.PLEN > R.PLEN THEN
"**"                 LOAD(L)
"**"               ELSE  (* EQUAL LENGTH *)
"**"                 IF NOT(L.VRBL AND L.DRCT AND (L.VPA=RGS)) THEN
"**"                   IF R.VRBL AND R.DRCT AND (R.VPA=RGS) THEN
"**"                     LR := FALSE
"**"                   ELSE LOAD(L);
"**"               IF NOT LR THEN  (* INTERCHANGE OPERANDS *)
"**"                 BEGIN  L := R;  R := STK[TOP-1]  END;
"**"               IF R.VRBL THEN
"**"                 IF R.DRCT AND (R.VPA=RGS) THEN
"**"                   BEGIN  (* BOTH OPERANDS IN REGISTERS *)
"**"                     GENRR( XXOR, L.RGADR, R.RGADR );
"**"                     AVAIL[R.RGADR] := TRUE;
"&&"                     IF R.PLEN > 4 THEN
"**"                       BEGIN  GENRR( XXOR, L.RGADR+1, R.RGADR+1 );
"**"                              AVAIL[R.RGADR+1] := TRUE
"**"                       END
"**"                   END
"**"                 ELSE  (* SECOND OPERAND IN MEMORY *)
"**"                   BEGIN  GETOPERAND( R, Q2, P2, B2 );
"**"                     GENRX( XO, L.RGADR, Q2, P2, B2 );
"&&"                     IF R.PLEN > 4 THEN
"**"                       BEGIN  CHECKDISP( Q2, P2, B2 );
"**"                              GENRX( XO, L.RGADR+1, Q2+4, P2, B2 );
"**"                       END
"**"                   END
"**"               ELSE  (* SECOND OPERAND IS A CONSTANT SET *)
"**"                 BEGIN  I_S_R.S := R.PCNST@[1];
"**"                   IF I_S_R.I1 <> 0 THEN
"**"                     GENRXLIT( XO, L.RGADR, I_S_R.I1, 0 );
"&&"                   IF R.PLEN > 4 THEN
"**"                     IF I_S_R.I2 <> 0 THEN
"**"                       GENRXLIT( XO, L.RGADR+1, I_S_R.I2, 0 )
"**"                 END
"**"             END
"**"           ELSE  (* LENGTH > 8 *)
"**"             BEGIN  FORCESTK(L);
"**"               IF R.VRBL THEN
"**"                 IF R.DRCT AND (R.VPA=RGS) THEN
"**"                   BEGIN  GETQB( L, Q1, P1, 4 );
"**"                     GENRX( XO, R.RGADR, Q1, P1, 0 );
"**"                     AVAIL[R.RGADR] := TRUE;
"&&"                     IF R.PLEN > 4 THEN
"**"                       BEGIN  GENRX( XO, R.RGADR+1, Q1+4, P1, 0 );
"**"                              GENRS( XSTM, R.RGADR, R.RGADR+1, Q1, P1 );
"**"                              AVAIL[R.RGADR+1] := TRUE
"**"                       END
"**"                     ELSE GENRX( XST, R.RGADR, Q1, P1, 0 )
"**"                   END
"**"                 ELSE  (* BOTH OPERANDS IN MEMORY *)
"**"                   BEGIN  MIN := L.PLEN;
"**"                     IF MIN > R.PLEN THEN MIN := R.PLEN;
"**"                     GETQB( L, Q1, P1, MIN );
"**"                     TXRG := TRG1;  GETQB( R, Q2, P2, MIN );
"**"                     TXRG := TRG14;
"**"                     GENSS( XOC, MIN, Q1, P1, Q2, P2 );
"**"                     IF R.PLEN > L.PLEN THEN
"**"                       GENSS( XMVC, R.PLEN-L.PLEN, Q1+MIN, P1,
"**"                                     Q2+MIN, P2 )
"**"                   END
"**"               ELSE  (* SECOND OPERAND IS A CONSTANT SET *)
"**"                 BEGIN  PSVAL.S := R.PCNST@;
"&&"                   MIN := L.PLEN;  IF MIN>R.PLEN THEN MIN := R.PLEN;
"**"                   COMPACT( R.PCNST@, R.PLEN, J, CHR(0) );
"**"                   GETQB( L, Q1, P1, MIN );
"**"                   IF MIN >= J THEN
"**"                     GENSSLIT( XOC, MIN-J, Q1+J, P1, R.PCNST@ );
"**"                   IF LEN > L.PLEN THEN
"**"                     BEGIN
"**"                       FOR I := 1 TO LEN-L.PLEN DO
"**"                         PSVAL.C[I] := PSVAL.C[I+L.PLEN];
"**"                       GENSSLIT( XMVC, LEN-L.PLEN,
"**"                                  Q1+MIN, P1, PSVAL.S );
"**"                     END
"**"                 END ;
"**"             END ;
"**"     L.STKADR := STKADR;  L.PLEN := LEN;  L.DTYPE := PSET;
"**"   END  (* PUNI *) ;
"**"
"**"   PINT: BEGIN
"**"     IF L.DTYPE <> PSET THEN ERROR(615);
"**"     IF R.DTYPE <> PSET THEN ERROR(615);
"**"     STKADR := L.STKADR;  LEN := L.PLEN;
"**"     IF LEN > R.PLEN THEN LEN := R.PLEN;
"**"     IF LEN <= 0 THEN  (* ONE OR BOTH OPERANDS NULL *)
"**"       IF R.PLEN <= 0 THEN
"**"         BEGIN  FREEREG(L);  L := R  END
"**"       ELSE
"**"         FREEREG(R)
"**"     ELSE  (* BOTH OPERANDS ARE NON-NULL *)
"**"       IF NOT L.VRBL AND NOT R.VRBL THEN  (* FOLD CONSTANTS *)
"**"         BEGIN
"**"          FOR I := 1 TO MXSETINX DO
"**"            L.PCNST@[I] := L.PCNST@[I] * R.PCNST@[I];
"**"          MINCONSTSET;
"**"         END
"**"       ELSE
"**"         IF LEN <= 8 THEN  (* GENERATE RESULT IN REGISTERS *)
"**"           BEGIN  LR := TRUE;
"**"             IF L.PLEN > R.PLEN THEN
"**"               BEGIN  LOAD(R);  LR := FALSE  END
"**"             ELSE IF L.PLEN < R.PLEN THEN
"**"               LOAD(L)
"**"             ELSE  (* EQUAL LENGTH OPERANDS *)
"**"               IF NOT ( L.VRBL AND L.DRCT AND (L.VPA=RGS) ) THEN
"**"                 IF R.VRBL AND R.DRCT AND (R.VPA=RGS) THEN
"**"                   LR := FALSE
"**"                 ELSE
"**"                   LOAD(L);
"**"             IF NOT LR THEN  (* INTERCHANGE OPERANDS *)
"**"               BEGIN  L := R;  R := STK[TOP-1]  END;
"**"             IF R.VRBL THEN
"**"               IF R.DRCT AND (R.VPA=RGS) THEN
"**"                 BEGIN  (* BOTH OPERANDS ARE IN REGS *)
"**"                   GENRR( XNR, L.RGADR, R.RGADR );
"**"                   AVAIL[R.RGADR] := TRUE;
"&&"                   IF L.PLEN > 4 THEN GENRR( XNR, L.RGADR+1, R.RGADR+1 );
"&&"                   IF R.PLEN > 4 THEN AVAIL[R.RGADR+1] := TRUE;
"**"                 END
"**"               ELSE
"**"                 BEGIN  (* LEFT OPND IN REGS, RIGHT OPND IN MEMORY *)
"**"                   GETOPERAND( R, Q2, P2, B2 );
"**"                   GENRX( XN, L.RGADR, Q2, P2, B2 );
"&&"                   IF L.PLEN > 4 THEN
"**"                     BEGIN  CHECKDISP( Q2, P2, B2 );
"**"                            GENRX( XN, L.RGADR+1, Q2+4, P2, B2 )
"**"                     END
"**"                 END
"**"             ELSE
"**"               BEGIN  (* LEFT OPND IN REGS, RIGHT OPND IS CONST *)
"**"                 I_S_R.S := R.PCNST@[1];
"**"                 IF I_S_R.I1 <> -1 THEN
"**"                   IF I_S_R.I1 <> 0 THEN
"**"                      GENRXLIT( XN, L.RGADR, I_S_R.I1, 0 )
"**"                   ELSE GENRR( XSR, L.RGADR, L.RGADR );
"&&"                 IF LEN > 4 THEN GENRXLIT( XN, L.RGADR+1, I_S_R.I2, 0 )
"&&"                 ELSE IF L.PLEN > 4 THEN AVAIL[L.RGADR+1] := TRUE;
"**"               END
"**"           END
"**"       ELSE
"**"         BEGIN  (* LEN > 8 *)
"**"           FORCESTK( L );
"**"           IF R.VRBL THEN
"**"             BEGIN  (* BOTH OPERANDS IN MEMORY *)
"**"               GETQB( L, Q1, P1, 0 );
"**"               TXRG := TRG1;
"**"               GETQB( R, Q2, P2, 0 );
"**"               TXRG := TRG14;
"**"               GENSS( XNC, LEN, Q1, P1, Q2, P2 );
"**"             END
"**"           ELSE
"**"             BEGIN  (* LEFT OPND IN MEM, RIGHT OPND IS CONST *)
"**"               COMPACT( R.PCNST@, LEN, J, CHR(255) );
"**"               GETQB( L, Q1, P1, J );
"**"               LEN := ALIGN( LEN, INTSIZE );
"**"               IF LEN >= J THEN
"**"                 GENSSLIT( XNC, LEN-J, Q1+J, P1, R.PCNST@ );
"**"             END;
"**"         END;
"**"     L.STKADR := STKADR;  L.PLEN := LEN;
"**"   END  (* PINT *) ;
"**"
"**"   PDIF: BEGIN
"**"     IF L.DTYPE <> PSET THEN ERROR(615);
"**"     IF R.DTYPE <> PSET THEN ERROR(615);
"**"     IF R.PLEN > 0 THEN
"**"       IF L.PLEN <= 0 THEN
"**"         FREEREG( R )
"**"       ELSE
"**"         IF NOT L.VRBL AND NOT R.VRBL THEN  (* FOLD CONSTANTS *)
"**"           BEGIN
"**"            FOR I := 1 TO MXSETINX DO
"**"              L.PCNST@[I] := L.PCNST@[I] - R.PCNST@[I];
"**"            MINCONSTSET;
"**"          END
"**"         ELSE
"**"           IF L.PLEN <= 8 THEN
"**"             BEGIN  (* GENERATE RESULT IN REGS *)
"**"               LOAD( L );
"**"               IF R.VRBL THEN
"**"                 BEGIN
"**"                   IF NOT ( R.DRCT AND (R.VPA = RGS) ) THEN
"**"                     BEGIN  (* FORCE R INTO REGISTERS *)
"**"                       IF R.PLEN > L.PLEN THEN R.PLEN := L.PLEN;
"**"                       LOAD( R );
"**"                     END;
"**"                   GENRR( XXOR, L.RGADR, R.RGADR );
"**"                   GENRR( XXR, L.RGADR, R.RGADR );
"**"                   AVAIL[R.RGADR] := TRUE;
"&&"                   IF R.PLEN > 4 THEN
"**"                     BEGIN
"&&"                       IF L.PLEN > 4 THEN
"**"                         BEGIN
"**"                           GENRR( XXOR, L.RGADR+1, R.RGADR+1 );
"**"                           GENRR( XXR, L.RGADR+1, R.RGADR+1 );
"**"                         END;
"**"                       AVAIL[R.RGADR+1] := TRUE;
"**"                     END
"**"                 END
"**"               ELSE
"**"                 BEGIN  (* LEFT OPND IN REGS, RIGHT OPND IS CNST *)
"**"                   I_S_R.S := [0..63] - R.PCNST@[1];
"**"                   IF I_S_R.I1 <> -1 THEN
"**"                     IF I_S_R.I1 <> 0 THEN
"**"                     GENRXLIT( XN, L.RGADR, I_S_R.I1, 0 )
"**"                   ELSE GENRR( XSR, L.RGADR, L.RGADR );
"&&"                   IF (L.PLEN > 4) AND (R.PLEN > 4) THEN
"&&"                     IF I_S_R.I2 <> 0 THEN
"**"                       GENRXLIT( XN, L.RGADR+1, I_S_R.I2, 0 )
"&&"                     ELSE BEGIN  L.PLEN := 4;
"&&"                                 AVAIL(/L.RGADR+1/) := TRUE  END;
"**"                 END
"**"             END
"**"           ELSE
"**"             BEGIN  (* L.PLEN > 8 *)
"&&"               LEN := L.PLEN;  IF LEN>R.PLEN THEN LEN := R.PLEN;
"**"               IF R.VRBL THEN
"**"                 BEGIN
"**"                   FORCESTK( L );
"**"                   IF NOT( R.VRBL AND R.DRCT AND (R.VPA=MEM)) THEN
"**"                       FORCESTK( R );
"**"                   GETQB( L, Q1, P1, 0 );
"**"                   TXRG := TRG1;
"**"                   GETQB( R, Q2, P2, 0 );
"**"                   TXRG := TRG14;
"&&"                   GENSS( XOC, LEN, Q1, P1, Q2, P2 );
"&&"                   GENSS( XXC, LEN, Q1, P1, Q2, P2 );
"**"                 END
"**"               ELSE
"**"                 BEGIN  (* RIGHT OPND IS CONST *)
"**"                   FORCESTK( L );
"**"                   FOR I := 1 TO MXSETINX DO
"**"                     R.PCNST@[I] := [0..63] - R.PCNST@[I];
"&&"                   COMPACT( R.PCNST@, LEN, J, CHR(255) );
"**"                   GETQB( L, Q1, P1, J );
"&&"                   IF LEN > J THEN
"&&"                     GENSSLIT( XNC, LEN-J, Q1+J, P1, R.PCNST@ );
"**"                 END
"**"             END ;
"**"   END  (* PDIF *) ;
"**"
"&&"   PINN: INN_OP;
"**"
"**"   PASE: ASE_OP ;
"**"
"**"   END  (* CASE OPC .. *) ;
"**"   STK[TOP-1] := L;
"**"   OLDCSP := PSIO;  (* INDICATE POSSIBLE LOSS OF REG 1 *)
"**"
"**" END (* BSETOPS *) ;
"**"
"**" PROCEDURE CSETOPS;
"**"   (* CONTROL AND MISCELLANEOUS OPERATIONS ON SETS *)
"**" VAR  Q1, Q2: ADRRNG;
"**"      P1, P2, B1: RGRNG;
"**"      L, R:   DATUM;
"**"      K:      INTEGER;
"**"
"**"   PROCEDURE FORCESET( VAR STE: DATUM; LEN: INTEGER );
"**"     (* CONVERTS A SET ADDR INTO SET ON RUN STACK *)
"**"   BEGIN
"**"     WITH STE DO
"**"       IF DTYPE = ADR THEN
"**"         BEGIN
"**"           IF VRBL THEN
"**"             BEGIN
"**"               LOAD( STE );  DRCT := FALSE;
"**"             END
"**"           ELSE
"**"             BEGIN
"**"               MEMADR := FPA;  FPA := ZEROBL;
"**"               DRCT := TRUE;  VRBL := TRUE;  VPA := MEM;
"**"             END;
"**"           DTYPE := PSET;  PLEN := LEN;
"**"           STKADR := 0;  (* TO BE SET LATER *)
"**"         END
"**"       ELSE
"**"         IF DTYPE <> PSET THEN ERROR( 615 );
"**"   END  (* FORCESET *) ;
"**"
"**" BEGIN
"**"   CASE OPC OF
"**"
"**" PSLD: WITH STK[TOP-1] DO
"**"   BEGIN
"**"     FORCESET( STK[TOP-1], P );
"**"     STKADR := Q;
"**"   END (* PSLD *);
"**"
"**" PSCL: WITH STK[TOP] DO
"**"   BEGIN
"**"     DTYPE := PSET;  PLEN := P;  STKADR := Q;
"**"     VRBL := TRUE;   DRCT := TRUE;  FPA := ZEROBL;
"**"     IF PLEN = 0 THEN
"**"       BEGIN  (* THIS CASE NEVER OCCURS IN PRACTICE *)
"**"         VRBL := FALSE;  VPA := NEITHER;
"**"       END
"**"     ELSE
"**"       IF P <= 8 THEN
"**"         BEGIN  (* CLEAR REG(S) *)
"**"           VPA := RGS;
"**"           IF P = 4 THEN FINDRG ELSE FINDRP;
"**"           RGADR := NXTRG;
"**"           GENRR( XSR, RGADR, RGADR );
"&&"           IF P > 4 THEN GENRR( XSR, RGADR+1, RGADR+1 );
"**"         END
"**"       ELSE
"**"         BEGIN  (* CLEAR MEMORY ON RUN-STACK *)
"**"           VPA := ONSTK;
"**"           GETQB( STK[TOP], Q1, P1, 0 );
"**"           GENSS( XXC, PLEN, Q1, P1, Q1, P1 );
"**"         END;
"**"     TOP := TOP + 1;
"**"    END (* PSCL *) ;
"**"
"**" PCRD: WITH STK[TOP-1] DO
"**"   BEGIN
"**"     IF PLEN <= 4 THEN LOAD( STK[TOP-1] )
"**"     ELSE IF NOT VRBL OR (DRCT AND (VPA=RGS)) THEN
"**"       FORCESTK( STK[TOP-1] );
"**"     (* OPERAND = SINGLE REG. OR A MEMORY AREA *)
"**"     FINDRG;  (* REGISTER FOR RESULT *)
"**"     GENRR( XSR, NXTRG, NXTRG );
"**"     IF PLEN > 4 THEN
"**"       BEGIN  (* MEMORY OPERAND *)
"**"         GETOPERAND( STK[TOP-1], Q1, P1, B1 );
"**"         IF P1 <> TRG14 THEN  (* WE NEED AN INDEX REG. *)
"**"           BEGIN  GENRX( XLA, TRG14, Q1, P1, B1 );
"**"                  P1 := TRG14;  Q1 := 0;  B1 := 0;
"**"           END;
"**"         GENRX( XLA, TRG1, PLEN DIV 4, 0, 0 );
"**"         GENRX( XL, 0, Q1, P1, B1 );
"**"         P2 := 0;
"**"       END
"**"     ELSE
"**"       P2 := RGADR;
"**"     GENRR( XLTR, 15, P2 );
"**"     GENRELRX( XBC, EQUCND, 6 );  (* BZ *+12 *)
"**"     GENRR( XBCTR, P2, 0 );  GENRR( XNR, P2, 15 );
"**"     GENRELRX( XBCT, NXTRG, -5 );  (* BCT NXTRG,*-10 *)
"**"     IF PLEN > 4 THEN
"**"       BEGIN
"**"         GENRX( XLA, P1, 4, P1, 0 );
"**"         GENRELRX( XBCT, TRG1, -11 );  (* BCT R1,*-22 *)
"**"       END
"**"     ELSE
"**"       AVAIL[RGADR] := TRUE;
"**"     GENRR( XLPR, NXTRG, NXTRG );
"**"     DTYPE := BOOL;  DRCT := TRUE;  VRBL := TRUE;
"**"     VPA := RGS;     RGADR := NXTRG;
"**"     CSPREGACTIVE := FALSE;  (* INDICATE LOSS OF REG 15 *)
"**"   END  (* PCRD *) ;
"**"
"**" PSMV: BEGIN
"**"   TOP := TOP - 2;
"**"   IF P < 0 THEN  (* REVERSED OPERANDS *)
"**"     BEGIN  L := STK[TOP+1];  R := STK[TOP];  P := -P  END
"**"   ELSE
"**"     BEGIN  L := STK[TOP];  R := STK[TOP+1]  END;
"**"   FORCESET( R, Q );
"**"   FORCESET( L, P );
"**"   (* L = DESTINATION SET, R = SOURCE SET *)
"**"   IF R.VRBL THEN
"**"     BEGIN
"**"       IF (R.PLEN <= 8) AND (P=4) AND DEBUG THEN LOAD( R );
"**"       IF R.DRCT AND (R.VPA = RGS) THEN
"**"         IF P < R.PLEN THEN  (* R.PLEN=8, P=4 *)
"**"           BEGIN
"**"             IF DEBUG THEN
"**"               BEGIN
"**"                 GENRR( XBALR, TRG14, 0 );
"**"                 GENRR( XLTR, R.RGADR+1, R.RGADR+1 );
"**"                 GENRX( XBC, NEQCND, SETCHK, GBR, 0 );
"**"               END;
"**"             AVAIL[R.RGADR+1] := TRUE;
"**"             R.PLEN := P;
"**"           END
"**"         ELSE  (* NOTHING *)
"**"       ELSE  (* R IS IN MEMORY *)
"**"         BEGIN  TXRG := TRG1;  K := 0;
"**"           IF P < R.PLEN THEN IF DEBUG THEN K := P;
"**"           GETQB( R, Q2, P2, K );
"**"           TXRG := TRG14;
"**"           IF P < R.PLEN THEN
"**"              BEGIN
"**"                IF DEBUG THEN
"**"                  BEGIN
"**"                    GENRR( XBALR, TRG14, 0 );
"**"                    GENSS( XNC, R.PLEN-P, Q2+P, P2, Q2+P, P2 );
"**"                    GENRX( XBC, NEQCND, SETCHK, GBR, 0 );
"**"                  END;
"**"                R.PLEN := P;
"**"              END
"**"         END
"**"     END
"**"   ELSE  (* R IS A CONSTANT *)
"**"     IF R.PLEN > P THEN
"**"       BEGIN  ERROR(303);  R.PLEN := P;  END;
"**"   IF P > R.PLEN THEN
"**"     BEGIN  (* CLEAR EXCESS BYTES IN DESTINATION *)
"**"       GETQB( L, Q1, P1, R.PLEN );
"**"       GENSS( XXC, P-R.PLEN, Q1+R.PLEN, P1, Q1+R.PLEN, P1 );
"**"     END
"**"   ELSE  GETQB( L, Q1, P1, 0 );
"**"   IF R.VRBL THEN
"**"     IF R.DRCT AND (R.VPA=RGS) THEN
"&&"       IF R.PLEN > 4 THEN
"**"           GENRS( XSTM, R.RGADR, R.RGADR+1, Q1, P1 )
"**"       ELSE  (* R.PLEN = 4 *)
"**"         GENRX( XST, R.RGADR, Q1, P1, 0 )
"**"     ELSE  (* R IS IN MEMORY *)
"**"       GENSS( XMVC, R.PLEN, Q1, P1, Q2, P2 )
"**"   ELSE  (* R IS A CONSTANT SET *)
"**"     IF R.PLEN > 0 THEN
"**"       GENSSLIT( XMVC, R.PLEN, Q1, P1, R.PCNST@ );
"**"     FREEREG( L );  FREEREG( R );
"**"   END  (* PSMV *) ;
"**"
"**"   END  (* CASE OPC OF .. *) ;
"**"   OLDCSP := PSIO;  (* INDICATE LOSS OF REG 1 *)
"**" END (* CSETOPS *) ;
"**"
"**" PROCEDURE SETCOMPARE( VAR L,R: DATUM );
"**"   (* GENERATE CODE FOR SET COMPARISONS *)
"**"   VAR   Q1, Q2, FIXUPLOC: ADRRNG;
"**"         P1, P2: LVLRNG;
"**"         EQ, INTCHG, CONSTSET, TEST_PENDING: BOOLEAN;
"**"         I,MIN:  INTEGER;
"**"
"**"   PROCEDURE TESTNULL( VAR STE: DATUM;  VAR Q: ADRRNG;
"**"                       VAR P: LVLRNG;   LEN: ADRRNG   );
"**"   BEGIN
"**"     IF LEN < STE.PLEN THEN
"**"       BEGIN
"**"         GETQB( STE, Q, P, LEN );
"**"         GENSS( XNC, STE.PLEN-LEN, Q+LEN, P, Q+LEN, P );
"**"         TEST_PENDING := TRUE;
"**"         STE.PLEN := LEN;
"**"       END
"**"     ELSE
"**"       GETQB( STE, Q, P, 0 );
"**"   END  (* TESTNULL *) ;
"**"
"**"   PROCEDURE GENBRANCH;
"**"     (* GENERATES INTERMEDIATE TEST BRANCHES *)
"**"   BEGIN
"**"     IF TEST_PENDING THEN
"**"       BEGIN
"**"         TESTCNT := TESTCNT + 1;
"**"      (* IF ASM THEN
"**"           BEGIN  FIXUPLOC := 0;  WRITELN(PRR,' BNZ T',TESTCNT:1);  END
"**"         ELSE *)
"**"           BEGIN  GENRX( XBC, NEQCND, 0, 0, 0 );
"**"                  FIXUPLOC := PC - 1;
"**"           END;
"**"       END
"**"   END  (* GENBRANCH *) ;
"**"
"**"   PROCEDURE SETCONSTBOOL( B: BOOLEAN );
"**"   BEGIN  FREEREG( L );
"**"          L.FPA.LVL := 0;  L.FPA.DSPLMT := ORD(B);
"**"          L.VRBL := FALSE;  L.DRCT := TRUE;  L.VPA := NEITHER;
"**"          CONSTSET := TRUE;
"**"   END;
"**"
"**" BEGIN
"**"   CONSTSET := FALSE;  INTCHG := FALSE;  TEST_PENDING := FALSE;
"**"   FIXUPLOC := -1;
"**"   IF (OPC = PEQU) OR (OPC = PNEQ) THEN
"**"     BEGIN
"**"
"**"     REPEAT
"**"       IF INTCHG THEN
"**"         BEGIN  L := STK[TOP];  R := STK[TOP-1];  END
"**"       ELSE
"**"         BEGIN  L := STK[TOP-1];  R := STK[TOP];  END;
"**"       INTCHG := FALSE;
"**"       IF L.PLEN <= 0 THEN  (* NULL LEFT OPERAND *)
"**"         IF R.PLEN <= 0 THEN  (* NULL RIGHT OPERAND *)
"**"           SETCONSTBOOL( OPC = PEQU )
"**"         ELSE
"**"           IF R.VRBL THEN
"**"             IF R.DRCT AND (R.VPA=RGS) THEN
"**"               IF R.PLEN = 4 THEN
"**"                 GENRR( XLTR, R.RGADR, R.RGADR )
"**"               ELSE
"**"                 GENRR( XXOR, R.RGADR, R.RGADR+1 )
"**"             ELSE  (* R IS IN MEMORY *)
"**"               TESTNULL( R, Q2, P2, 0 )
"**"           ELSE  (* R IS CONSTANT *)
"**"             SETCONSTBOOL( OPC <> PEQU )
"**"       ELSE
"**"         IF L.VRBL THEN
"**"           IF L.DRCT AND (L.VPA=RGS) THEN
"**"             IF R.PLEN <= 0 THEN
"**"               INTCHG := TRUE
"**"             ELSE
"**"               IF R.VRBL THEN
"**"                 IF R.DRCT AND (R.VPA=RGS) THEN
"**"                   BEGIN
"**"                     GENRR( XXR, L.RGADR, R.RGADR );
"**"                     IF L.PLEN < R.PLEN THEN
"**"                       GENRR( XXOR, L.RGADR, R.RGADR+1 )
"**"                     ELSE
"&&"                       IF L.PLEN > 4 THEN
"**"                         BEGIN
"&&"                           IF R.PLEN > 4 THEN
"**"                             GENRR( XXR, L.RGADR+1, R.RGADR+1 );
"**"                           GENRR( XXOR, L.RGADR, L.RGADR+1 );
"**"                         END;
"**"                   END
"**"                 ELSE  (* R IS IN MEMORY *)
"**"                   BEGIN
"**"                     TESTNULL( R, Q2, P2, L.PLEN );
"**"                     GENBRANCH;
"**"                     GENRX( XX, L.RGADR, Q2, P2, 0 );
"&&"                     IF L.PLEN > 4 THEN
"**"                       BEGIN
"**"                         IF R.PLEN >= 8 THEN
"**"                           GENRX( XX, L.RGADR+1, Q2+4, P2, 0 );
"**"                         GENRR( XXOR, L.RGADR, L.RGADR+1 );
"**"                       END
"**"                   END
"**"               ELSE  (* R IS CONSTANT *)
"**"                 IF R.PLEN > L.PLEN THEN
"**"                   SETCONSTBOOL( OPC <> PEQU )
"**"                 ELSE
"**"                   BEGIN  I_S_R.S := R.PCNST@[1];
"**"                     IF I_S_R.I1 <> 0 THEN
"**"                       GENRXLIT( XX, L.RGADR, I_S_R.I1, 0 );
"&&"                     IF L.PLEN > 4 THEN
"**"                       BEGIN
"**"                         IF R.PLEN >= 8 THEN
"**"                           GENRXLIT( XX, L.RGADR+1, I_S_R.I2, 0 );
"**"                         GENRR( XXOR, L.RGADR, L.RGADR+1 );
"**"                       END
"**"                   END
"**"             ELSE  (* L IS IN MEMORY *)
"**"               IF (R.PLEN=0) OR (R.VRBL AND R.DRCT AND
"**"                                 (R.VPA=RGS)) THEN
"**"                 INTCHG := TRUE
"**"               ELSE
"**"                 IF R.VRBL THEN  (* R IS IN MEMORY *)
"**"                   BEGIN
"**"                     TESTNULL( L, Q1, P1, R.PLEN );
"**"                     TXRG := TRG1;
"**"                     TESTNULL( R, Q2, P2, L.PLEN );
"**"                     TXRG := TRG14;
"**"                     GENBRANCH;
"**"                     MIN := L.PLEN;  IF MIN>R.PLEN THEN MIN:=R.PLEN;
"**"                     GENSS( XCLC, MIN, Q1, P1, Q2, P2 );
"**"                   END
"**"                 ELSE  (* R IS CONSTANT *)
"**"                   IF L.PLEN < R.PLEN THEN
"**"                     SETCONSTBOOL( OPC <> PEQU )
"**"                   ELSE
"**"                     BEGIN
"**"                       TESTNULL( L, Q1, P1, R.PLEN );
"**"                       GENBRANCH;
"**"                       GENSSLIT( XCLC, R.PLEN, Q1, P1, R.PCNST@ );
"**"                     END
"**"          ELSE  (* L IS CONSTANT *)
"**"            IF (R.PLEN=0) OR R.VRBL THEN
"**"              INTCHG := TRUE
"**"            ELSE
"**"              BEGIN  EQ := TRUE;
"**"                FOR I := 1 TO MXSETINX DO
"**"                  IF L.PCNST@[I] <> R.PCNST@[I] THEN
"**"                    EQ := FALSE;
"**"                  SETCONSTBOOL( (OPC=PEQU) = EQ );
"**"              END;
"**"     UNTIL NOT INTCHG;
"**"   END  (* IF OPC IS PEQU OR PNEQ *)
"**"   ELSE
"**"     BEGIN  (* OPC IS PGEQ OR PLEQ *)
"**"       IF OPC = PGEQ THEN
"**"         BEGIN  L := STK[TOP];  R := STK[TOP-1]  END
"**"       ELSE
"**"         BEGIN  L := STK[TOP-1];  R := STK[TOP]  END;
"**"       OPC := PEQU;
"**"       IF L.PLEN <= 4 THEN
"**"         BEGIN  LOAD(L);
"**"           IF R.PLEN = 0 THEN
"**"             GENRR( XLTR, L.RGADR, L.RGADR )
"**"           ELSE
"**"             IF R.VRBL THEN
"**"               IF R.DRCT AND (R.VPA=RGS) THEN
"**"                 BEGIN  GENRR( XXOR, L.RGADR, R.RGADR );
"**"                        GENRR( XXR, L.RGADR, R.RGADR );
"**"                 END
"**"               ELSE
"**"                 BEGIN  GETOPERAND( R, Q1, P1, B1 );
"**"                        GENRX( XO, L.RGADR, Q1, P1, B1 );
"**"                        GENRX( XX, L.RGADR, Q1, P1, B1 );
"**"                 END
"**"             ELSE  (* R IS CONSTANT *)
"**"               BEGIN  I_S_R.S := R.PCNST@[1];
"**"                      GENRXLIT( XO, L.RGADR, I_S_R.I1, 0 );
"**"                      GENRXLIT( XX, L.RGADR, I_S_R.I1, 0 );
"**"               END
"**"         END
"**"       ELSE
"**"         BEGIN  FORCESTK( L );
"**"           TESTNULL( L, Q1, P1, R.PLEN );
"**"           IF R.PLEN > 0 THEN
"**"             BEGIN  GENBRANCH;
"**"               IF R.VRBL THEN
"**"                 IF R.DRCT AND (R.VPA=RGS) THEN
"**"                   BEGIN
"**"                     GENRX( XN, R.RGADR, Q1, P1, 0 );
"**"                     GENRX( XX, R.RGADR, Q1, P1, 0 );
"&&"                     IF R.PLEN > 4 THEN
"**"                       BEGIN
"**"                         GENRX( XN, R.RGADR+1, Q1+4, P1, 0 );
"**"                         GENRX( XX, R.RGADR+1, Q1+4, P1, 0 );
"**"                         GENRR( XXOR, R.RGADR, R.RGADR+1 );
"**"                       END
"**"                   END
"**"                 ELSE
"**"                   BEGIN  TXRG := TRG1;
"**"                     GETQB( R, Q2, P2, 0 );
"**"                     TXRG := TRG14;
"**"                     GENSS( XOC, L.PLEN, Q1, P1, Q2, P2 );
"**"                     GENSS( XXC, L.PLEN, Q1, P1, Q2, P2 );
"**"                   END
"**"             ELSE
"**"               BEGIN  (* R IS CONSTANT *)
"**"                 GENSSLIT( XOC, L.PLEN, Q1, P1, R.PCNST@ );
"**"              (* IF ASM THEN
"**"                   GENSSLIT( XXC, L.PLEN, Q1, P1, R.PCNST@ )
"**"                 ELSE *)   BEGIN  (*KLUDGE TO RE-USE SAME CONSTANT*)
"**"                    GENSS( XXC, L.PLEN, Q1, P1, 0, 0 );
"&&"                    CODE.H[PC-1] := CODE.H[PC-4];
"&&"                    NXTLIT := NXTLIT + 1;
"&&"                    LITTBL[NXTLIT].LNK := PC-1;
"**"                 END;
"**"               END
"**"         END
"**"     END
"**"   END  (* OF CODE FOR >= AND <= *);
"**"
"**"       FREEREG(L);  FREEREG(R);
"**"       L.DTYPE := BOOL;
"**"       IF NOT CONSTSET THEN
"**"         BRCND := BRMSK[OPC];
"**"       IF FIXUPLOC >= 0 THEN
"**"      (* IF ASM THEN WRITELN(OUTPUT,'T',TESTCNT:1,' DS 0H')
"**"         ELSE *)   CODE.H[FIXUPLOC] := BASE_DSPLMT( PC );
"**"   OLDCSP := PSIO;  (* INDICATE LOSS OF REG 1 *)
"&&"   TXR_CONTENTS.VALID := FALSE;
"**"
"**" END  (* SETCOMPARE *) ;


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

    PROCEDURE ADDLNP(PCDIF: BYTE);
    (* TO ADD A (SOURCE) LINE POINTER TO THE POINTER TABLE *)
    (* --------------------------------------------------- *)
    BEGIN
    IF NXTLNP < MXLNP THEN
"&&"  BEGIN  CODE.C(/MXCODE*2+NXTLNP/) := CHR(PCDIF);
             NXTLNP := NXTLNP+1 ;
      END
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
"&&"      BEGIN  DEFINED := FALSE ;  LNK := 1  END ;
"&&"  NXTLIT := 0;  NXTDBL := 0;  NXTCH := 0;
"**"  IHCONF := -1;  RICONF := -1;  RHCONF := -1;
"**"  INT_GAP := -1;  HW_GAP := -1;
"&&"  POOL_SIZE := 0;  NUMLITS := 0;
"**"  DBLALN := FALSE;
"&&"  LAST_CC.LPC := 0;  TXR_CONTENTS.VALID := FALSE;
"&&"  LAST_STR.LPC := 0;  LAST_MVC.LPC := 0;
"&&"  LAST_FILE.LPC := 0;
      PRCTBL[0].NAME := LBL1.NAM ;  PRCTBL[0].LNK := 0 ; PRCTBL[1].LNK := 0 ;
"&&"  NXTPRC := 1 ;  NXTEP := PRCCNT;  CALDPTH := 0;
      PC := 0 ;  MINLBL := LBLMAP(SEGSZE.NAM);
      LASTPC := 0 ;
      J := 0 ;
      IF CURLVL = 1 THEN (*MAINBLK*)  J := HDRLNGTH ;
      GENRX(XBC,ANYCND,12+J,0,JREG) ;          (* SKIP AROUND CSECT NAME *)
      PC := 6+(J DIV 2) ;  (* PRECEEDING 'GENRX' USES THE VALUE OF 'PC' *)
"&&"  PCAFTLIT := PC;
"&&"  CODE.C(/4/) := CHR(7+J);
"&&"  FOR I := 1 TO 7 DO
"&&"    CODE.C(/I+4/) := CURPNAME(/I/);
"&&"  FOR I := 1 TO J  DO
"&&"    CODE.C(/I+11/) := PROGHDR(/I/);

      CODE.H[MXCODE] := CURPNO;  (* UNIQUE PROC NO *)
"&&"  IF DEBUG_LEV > 0 THEN
        BEGIN
          CODE.H[MXCODE+1] := LASTLN ;
"&&"      FOR I := 1 TO 12 DO
"&&"        CODE.C(/MXCODE*2+3+I/) := CURPNAME(/I/);
"&&"      NXTLNP := 16 ;
        END
      ELSE NXTLNP := 0 ;

      END (*INIT_CSECT*) ;


    PROCEDURE GEN_CSECT ;
    (* TO MERGE LITERAL POOLS AND GENERATE ONE OBJECT MODULE FOR THIS PROC *)
    (* ------------------------------------------------------------------- *)
"**"  LABEL 10;
      CONST  XESD    = 46523076   ;  (*EBCDIC FOR  2|'ESD' *)
             XTXT    = 48490467   ;  (*               TXT  *)
             XRLD    = 47829956   ;  (*               RLD  *)
             XEND    = 46519748   ;  (*               END  *)

             BLNK1   = 64         ;  (* EBCDIC FOR ' '     *)
             BLNK2   = 16448      ;  (*            '  '    *)
             BLNK3   = 4210752    ;  (*            '   '   *)
             BLNK4   = 1077952576 ;  (*            '    '  *)

"&&"  VAR  I, J, K   : INTEGER ;
           TPC, QPC, OBJEND : INTEGER ;
           LNGTH     : STRLRNG  ;
           VSL16     : INTEGER ;
           BLNK80    : ARRAY[1..80] OF CHAR ;
"&&"       BLNK64    : ARRAY[1..64] OF CHAR ;
           CARD      : RECORD  CASE  INTEGER OF
                       1: (C : ARRAY[1..80] OF CHAR) ;   (*CHAR CARD IMAGE*)
                       2: (I : ARRAY[1..20] OF INTEGER); (*INT. CARD IMAGE*)
"&&"                   3: (H: ARRAY(/1..40/) OF HINTEGER)  (*HALFWORD IMAGE   *)
                       END ;

"&&"       ESD_CARD  : RECORD  CASE INTEGER OF
"&&"                   1:( C16: ARRAY[1..16] OF CHAR;
"&&"                       C64: ARRAY[1..64] OF CHAR );
"&&"                   2:( I4: ARRAY[1..4] OF INTEGER;
"&&"                       ITEM: ARRAY[1..3] OF RECORD
"&&"                             XNAME: ALFA;  F1,F2: INTEGER  END );
"&&"                   3:( C80: ARRAY[1..80] OF CHAR )
"&&"                   END;
"&&"
"&&"  PROCEDURE PRNT_CSECT( LPC1: ICRNG );
"&&"    LABEL 10;
"&&"    VAR   LPC, CON1, CON2: HEX4;
"&&"          APC, APC1: ICRNG;  I, K: 0 .. 9999;
"&&"    BEGIN  (*PRNT_CSECT*)
"&&"      WRITELN(OUTPUT);
"&&"      WRITELN(OUTPUT,' OBJECT CODE FOR CSECT', PRCTBL[0].NAME:9,
"&&"        '(PROCEDURE ':13, CURPNAME, ')');
"&&"      APC := 0;  APC1 := 0;
"&&"      REPEAT
"&&"        WRITELN(OUTPUT);
"&&"        HEXHW( 2*APC, LPC );  WRITE(OUTPUT, LPC:5, ': ');
"&&"        FOR I := 0 TO 7 DO  (* 32 BYTES PER LINE *)
"&&"          BEGIN
"&&"            IF I = 4 THEN WRITE(OUTPUT,'  ');
"&&"            HEXHW( CODE.H[APC1], CON1 );  HEXHW( CODE.H[APC1+1], CON2 );
"&&"            WRITE(OUTPUT,' ',CON1,CON2);
"&&"            APC := APC + 2;  APC1 := APC1 + 2;
"&&"            IF APC1 >= LPC1 THEN GOTO 10;
"&&"          END;
"&&"      UNTIL FALSE;
"&&" 10:  K := 0;
"&&"      IF (NXTPRC > 1) OR (NXTEP < PRCCNT) THEN
"&&"        BEGIN  WRITELN(OUTPUT);  WRITELN(OUTPUT);
"&&"        WRITELN(OUTPUT,' EXTERNAL REFERENCES AND LABEL DEFINITIONS:');
"&&"        FOR I := 0 TO PRCCNT DO
"&&"          IF (I < NXTPRC) OR (I > NXTEP) THEN
"&&"            WITH PRCTBL[I] DO
"&&"              IF LNK > 0 THEN
"&&"                BEGIN  IF (K MOD 3) = 0 THEN WRITELN(OUTPUT);
"&&"                  K := K + 1;
"&&"                  HEXHW( LNK*2, CON1 );
"&&"                  WRITE(OUTPUT,CON1:5,':',NAME:9);
"&&"                  IF I < NXTPRC THEN WRITE(OUTPUT,' (ER);    ')
"&&"                  ELSE               WRITE(OUTPUT,' (LD);    ');
"&&"                END;
"&&"          WRITELN(OUTPUT);
"&&"        END
"&&"      ELSE WRITELN(OUTPUT);
"&&"      WRITELN(OUTPUT);
"&&"    END (*PRNT_CSECT*) ;
"&&"


      BEGIN  (*GEN_CSECT*)

"**"  QPC := LBLTBL[MINLBL].LNK;
"&&"  WHILE QPC > 1 DO
"**"    BEGIN  TPC := CODE.H[QPC];
"**"           UPD_INTTBL( QPC, Q );
"**"           QPC := TPC;
"**"    END;
"&&"  DUMP_LITERALS;

      (* PROCESS EXTERNAL REFERENCES  *)

      FOR I := 0 TO NXTPRC-1 DO
        WITH PRCTBL[I] DO
          IF LNK > 0 THEN
"&&"        BEGIN   TPC := LNK;
"&&"          IF FLOW_TRACE AND (NAME(/1/) <> '$') THEN LNK := -PC*2
"&&"          ELSE LNK := BASE_DSPLMT(PC);
              REPEAT
              QPC := CODE.H[TPC] ;  CODE.H[TPC] := LNK ;  TPC := QPC ;
              UNTIL TPC = 0 ;
            LNK := PC ;
"&&"        CODE.I(/PC DIV 2/) := 0;  PC := NEXTPC(2);
            END (* FOR..., IF...,  WITH... *) ;

      TPC := PC;
"&&"  IF DEBUG_LEV > 0 THEN
        BEGIN
"&&"      CODE.H[5] := PC*2;  (* PROC SIZE FIELD *)
"&&"      REPEAT ADDLNP(255) UNTIL NXTLNP MOD 4 = 0;
        END ;

      IF  NOT LARGE_PROC  THEN
"&&"    IF PC > 4096 THEN
          ERROR(609 (*SHORT PROC TOO LONG*) ) ;


      (* OUTPUT THE OBJECT CODE   *)

"&&"  FOR I := 1 TO 20 DO  CARD.I[I] := BLNK4;
"&&"  BLNK80 := CARD.C;
"&&"  PACK( BLNK80, 1, BLNK64 );

      (* OUTPUT THE 'ESD' ENTRIES *)

"&&"  IF CURLVL = 1 THEN IF NOT MUSIC THEN
"&&"    BEGIN  PRCTBL[NXTPRC].NAME := '$PASENT ';
"&&"           NXTPRC := NXTPRC + 1
"&&"    END;
"&&"  WITH ESD_CARD DO
"&&"    BEGIN  I4[1] := XESD;  I4[2] := BLNK4;  C64 := BLNK64;
"&&"      I := 0;  J := 0;  K := BLNK2*SL16 + 1;
"&&"      REPEAT   J := J + 1;
"&&"        WITH ITEM[J], PRCTBL[I] DO
"&&"          BEGIN  XNAME := NAME;
"&&"            IF I < NXTPRC THEN
"&&"              IF I = 0 THEN  (* NAME OF THIS CSECT *)
"&&"                BEGIN  F1 := 0;
"&&"                       F2 := BLNK1*SL24 + PC*2 + NXTLNP;  (*CSECT SIZE*)
"&&"                END
"&&"              ELSE           (* EXTERNAL REFERENCE *)
"&&"                BEGIN  F1 := 2*SL24;
"&&"                       F2 := BLNK4;
"&&"                END
"&&"            ELSE             (* LABEL DEFINITION *)
"&&"                BEGIN  F1 := 1*SL24 + LNK*2;
"&&"                       F2 := BLNK1*SL24 + 1;
"&&"                END
"&&"          END;
"&&"        I := I + 1;
"&&"        IF I = NXTPRC THEN I := NXTEP + 1;
"&&"        IF (J = 3) OR (I > PRCCNT) THEN
"&&"          BEGIN  I4[3] := BLNK2*SL16 + J*16;
"&&"                 I4[4] := K;
"&&"                 WRITE(PRR,C80);
"&&"                 IF I < NXTPRC THEN K := K + 3 ELSE K := BLNK4;
"&&"                 C64 := BLNK64;  J := 0
"&&"          END;
"&&"      UNTIL I > PRCCNT;
"&&"    END (* WITH ESD_CARD *) ;
"&&"  IF CURLVL = 1  THEN IF NOT MUSIC THEN NXTPRC := NXTPRC - 1;

      (* OUTPUT THE 'TXT' CARDS   *)

      CARD.I[1] := XTXT ;
"&&"  CARD.I(/2/) := BLNK1*SL24 + 0;
"&&"  CARD.I(/3/) := BLNK2*SL16 + TXTCHUNK;
"&&"  CARD.I(/4/) := BLNK2*SL16 + 01;
"&&"  TPC := MXCODE;  QPC := TPC + NXTLNP DIV 2;
"&&"  WHILE TPC < QPC DO
"&&"    BEGIN  CODE.H(/PC/) := CODE.H(/TPC/);
"&&"           PC := PC + 1;  TPC := TPC + 1;
"&&"    END;
"&&"  TPC := 0;  I := 0;  QPC := PC*2;  LNGTH := TXTCHUNK;
"&&"  WHILE TPC < QPC DO
"&&"    BEGIN
"&&"      IF (QPC - TPC) < TXTCHUNK THEN
"&&"        BEGIN  LNGTH := QPC - TPC;  CARD.H(/6/) := LNGTH;  END;
"&&"      CARD.H(/4/) := TPC;
"&&"      WRITE(PRR, CARD.C:16, CODE.TXTCARD(/I/):LNGTH, ' ':64-LNGTH);
"&&"      I := I + 1;  TPC := TPC + TXTCHUNK;
"&&"    END;

      (* OUTPUT THE 'RLD' ENTRIES *)

      CARD.C := BLNK80 ;  CARD.I[1] := XRLD ;  I := 0 ;  LNGTH := 0 ;
      REPEAT         (* SCAN OVER ALL EXTERNAL REFERENCES *)
        WITH PRCTBL[I] DO
          BEGIN   I := I+1 ;  (* I NOW BECOMES ESDID FOR THE CURRENT ENTRY *)
          IF LNK > 0 THEN     (* IMPLIES RECURSIVE CALL *)
            BEGIN
            CARD.I[LNGTH+5] := I*SL16+01 ;      (* 'P#', 'R#' FIELDS  *)
            CARD.I[LNGTH+6] := 28*SL24+LNK*2 ;  (* ADCON DISPLACEMENT *)
            LNGTH := LNGTH+2 ;
            IF (LNGTH >= 14) OR (I >= NXTPRC) THEN      (* OUTPUT THE BUFFER *)
              BEGIN  CARD.H(/6/) := LNGTH*4;  (* # OF RLD DATA BYTES *)
              WHILE LNGTH < 14 DO
"&&"            BEGIN  CARD.I(/LNGTH+5/) := BLNK4;  LNGTH := LNGTH+1  END;
              WRITE(PRR,CARD.C) ;  LNGTH := 0 ;
              END (* IF(LNGTH >... *) ;
            END (* IF LNK > 0 *) ;
          END (* WITH PRCTBL... *)
      UNTIL  I >= NXTPRC ;

      (* OUTPUT 'END' CARD *)

      CARD.C := BLNK80 ;
      CARD.I[1] := XEND ;
"&&"  IF CURLVL = 1 THEN IF NOT MUSIC THEN
"&&"     BEGIN  CARD.I(/2/) := BLNK1*SL24;  CARD.H(/8/) := NXTPRC+1  END;
"&&"  WRITE(PRR,CARD.C:32,'PASCAL:':7,DATE:11,' ':30);
"&&"  IF ASM THEN PRNT_CSECT(PC);
"&&"  IF ASMVERB THEN BEGIN
"&&"  WRITELN('****  PROC: ':17,PRCTBL[0].NAME,'; ',PROC_SIZE:1,' P-STMTS, ',
"&&"    PC*2:1,' BYTES, ',NXTPRC-1:1,' EXT. REFS., ',NUMLITS:1,' CONSTANTS, ',
"&&"    POOL_SIZE:1,' BYTES OF CONSTANTS.');  WRITELN;  END;
      TOTALBYTES := TOTALBYTES+QPC ;
10:   END (*GEN_CSECT*) ;
"&&"
"&&"  PROCEDURE DUMPCONSTBLK( CLOSE: BOOLEAN );
"&&"  VAR  CPC1, LEN, I, J: HINTEGER;  TXTNUM: 0..150;
"&&"  BEGIN
"&&"    IF CSEGSTRT = 0 THEN  (* FIRST CALL *)
"&&"      BEGIN  (* PUT OUT ESD CARD TO BEGIN CSECT *)
"&&"        WRITE(PRR, CHR(02), 'ESD      ', CHR(0), CHR(16), '  ',
"&&"              CHR(0), CHR(1), PRCTBL(/0/).NAME, CHR(0), CHR(0),
"&&"              CHR(0), CHR(0), ' ', CHR(0), CHR(0), CHR(0), ' ':48);
"&&"      END;
"&&"    CPC1 := CSEGSTRT;  TXTNUM := 0;  LEN := TXTCHUNK;
"&&"    WHILE CPC1 < CPC DO
"&&"      BEGIN
"&&"        IF (CPC-CPC1) < TXTCHUNK THEN LEN := CPC-CPC1;
"&&"        IF (LEN = TXTCHUNK) OR CLOSE THEN
"&&"          WRITE(PRR, CHR(02), 'TXT ', CHR(0), CHR(CPC1 DIV 256),
"&&"              CHR(CPC1 MOD 256), '  ', CHR(0), CHR(LEN), '  ',
"&&"              CHR(0), CHR(1), CODE.TXTCARD(/TXTNUM/):LEN, ' ':64-LEN);
"&&"        TXTNUM := TXTNUM + 1;  CPC1 := CPC1 + LEN;
"&&"      END;
"&&"    IF CLOSE THEN  (* LAST CALL, PUT OUT END CARD *)
"&&"      BEGIN
"&&"        WRITE(PRR, CHR(02), 'END', ' ':24, CHR(0), CHR(0),
"&&"              CHR(CPC1 DIV 256), CHR(CPC1 MOD 256), ' ':48);
"&&"        IF ASMVERB THEN
"&&"          BEGIN  WRITELN('****  CONSTS: ':20, PRCTBL(/0/).NAME, '; ',
"&&"                         CPC1:1, ' BYTES.');  WRITELN;
"&&"          END;
"&&"      END
"&&"    ELSE
"&&"      BEGIN
"&&"        J := CPC1 - LEN - CSEGSTRT;  CSEGSTRT := CPC1 - LEN;  I := 0;
"&&"        WHILE I < LEN DO
"&&"          BEGIN  CODE.C(/I/) := CODE.C(/J+I/);  I := I+1  END;
"&&"        CSEGLIMIT := CSEGSTRT + TXTCHUNK*145;
"&&"      END;
"&&"  END  (* DUMPCONSTBLK *) ;

"&&" PROCEDURE ENT_RET;
"&&"   BEGIN
"&&"     IF OPC = PENT THEN
          BEGIN  (* ON ENTRY TRG1 POINTS TO DATA AREA FOR THE CALLED ROUTINE *)
          CURLVL := P ;
       (* IF ASM  THEN
            BEGIN  WRITELN(OUTPUT,'  PBGN ', CURPNAME, PROGHDR:HDRLNGTH+2) ;
            WRITELN(OUTPUT,LBL1.NAM,' CSECT') ;  WRITELN(OUTPUT,' USING *,',JREG
            WRITELN(OUTPUT,' B *+12') ;   (* IN FACT GENRX(XB,15,... *)
            WRITELN(OUTPUT, ' DC AL1(7),CL7''' ,CURPNAME:7, '''') ;
            END (*IF ASM*)
          ELSE *)   INIT_CSECT ;     (*INITIALIZE NEW CSECT PARAMETERS*)

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

          GENRR(XLR,LBR,TRG1) ;     (*UPDATE THE 'MP'*)
          IF CALL_HIGHER THEN
          GENRX(XST,LBR,DISPLAY+4*CURLVL,GBR,0) ; (*UPDATE DISPLAY[CURLVL]*)
          GENRR(XLR,PBR1,JREG) ;  (* SET UP PROGRAM BASE REGISTERS *)
          IF LARGE_PROC THEN  GENRX(XLA,PBR2,4092,PBR1,0) ;

       (* IF ASM THEN
            BEGIN
            WRITELN(OUTPUT, ' USING ', LBL1.NAM:LBL1.LEN, ',', PBR1:2) ;
            WRITELN(OUTPUT, ' USING 4092+', LBL1.NAM:LBL1.LEN, ',', PBR2:2) ;
            WRITELN(OUTPUT,' DROP',JREG:3) ;
            END ; *)

"&&"      IF DEBUG OR MUSIC THEN
            BEGIN
            GENRR(XLR,RTREG,JREG) ;   (* SAVE CURR. LOC. FOR ERROR ROUTINE *)
            IF DATA_SIZE < 4096 THEN  GENRX(XLA,TRG1,DATA_SIZE,TRG1,0)
            ELSE  GENRXLAB(XA,TRG1,SEGSZE,-1) ;
            GENRX(XC,TRG1,NEWPTR,GBR,0) ;    (* COMPARE 'SP' AND 'NP' *)
            GENRX(XBC,GEQCND,STKCHK,GBR,0) ; (* BRANCH TO ERROR ? *)
"&&"        IF DEBUG THEN
            IF CURLVL = 1 THEN   (*ENTERING MAINBLK, CLEAR STACK/HEAP AREA*)
               BEGIN
               GENRX(XLD, FPR0, CLEARBUF, GBR, 0);   (*GET THE "CLEAR" PATTERN*)
               GENRX(XL, TRG15, NEWPTR, GBR, 0);     (*END OF HEAP*)
               GENRX(XLA, TRG1, FRSTGVAR, GBR, 0);
               GENRR(XSR, TRG15, TRG1);              (*TRG14 <-- BYTE COUNT*)
"**"           GENRS(XSRA, TRG15, 0, 3, 0);
"&&"           GENRR( XBALR, TRG14, 0 );
               GENRX(XSTD, FPR0, 0, TRG1, 0);
               GENRX(XLA, TRG1, 8, TRG1, 0);
"&&"           GENRR( XBCTR, TRG15, TRG14 );
               END;
            END ;

          CSPREGACTIVE := FALSE ;

          END
"&&"     ELSE  (* OPC = PRET *)
          BEGIN (*RESTORES DISPLAY[CURLVL] AND MP, THEN RETURNS*)
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
"**"          GENRS(XSRA, TRG1, 0, 3, 0)  (* DIVIDE BY 8 *)
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
"&&"      IF FLOW_TRACE THEN
"&&"        BEGIN  GENRR( XLR, 0, RTREG );
"&&"          GENRX( XBAL, RTREG, TRACER, GBR, 0 );
"&&"       (* IF ASM THEN WRITELN(OUTPUT,' DC AL2(0)') ELSE *)
"&&"          CODE.H[PC] := 0;  PC := NEXTPC(1);
"&&"        END
"&&"      ELSE GENRR(XBCR,ANYCND,RTREG) ;  RELEASE(HEAPMARK) ;
"**"      IF CKMODE THEN CHECKFREEREGS;
          END
"&&"    END (* ENT_RET *) ;

      (*_________________________________________________________________*)


      BEGIN   (*COPERATION*)

        CASE OPC OF

        (* P_MACHINE PSEUDO OPS *)

"&&"    PXLB :
"&&"      BEGIN
"&&"      GENRELRX( XBC, ANYCND, 14 );  (* B *+28, SKIP OVER ENTRY CODE *)
"&&"      WITH PRCTBL[NXTEP] DO
"&&"        BEGIN  NAME := LBL1.NAM;  LNK := PC  END;
"&&"      IF NXTEP > NXTPRC THEN NXTEP := NXTEP - 1
"&&"      ELSE ERROR( 256 );  (* COLLISION OF TWO LISTS *)
"&&"      GENRR( XBALR, RTREG, 0 );  (* FORCE A BASE REG. FOR NEXT INST. *)
"&&"      GENRX( XBAL, PBR1, 6, RTREG, 0 );
"&&"      CODE.H[PC] := PC*2;  PC := NEXTPC(1);
"&&"      GENRX( XLA, PBR1, 4, RTREG, 0 );  (* CLEAR HIGH BYTE *)
"&&"      GENRX( XSH, PBR1, 4, RTREG, 0 );
"&&"      IF LARGE_PROC THEN GENRX( XLA, PBR2, 4092, PBR1, 0 )
"&&"                    ELSE GENRX( XBC, NOCND, 0, 0, 0 );
"&&"      GENRX( XL, LBR, DISPLAY+4*CURLVL, GBR, 0 );
"&&"      (* PLAB INSTR. IS NEXT ==> NO NEED TO RESET ANY FLAGS *)
"&&"      END;

        PLAB :
          BEGIN
          CASE_FLAG := FALSE ;   (* END OF BRANCH TABLE *)
       (* IF ASM THEN  WRITELN(OUTPUT,LBL1.NAM,' DS 0H')
          ELSE *)  UPD_LBLTBL(PC, LBLMAP(LBL1.NAM), TRUE (*LABEL DEFINITION*));

          CASE_FLAG := OLDOPC = PDEF ;   (* START BRANCH TABLE *)
          CSPREGACTIVE := FALSE ;
"&&"      TXR_CONTENTS.VALID := FALSE;  LAST_CC.LPC := 0;
"&&"      LAST_STR.LPC := 0;  LAST_FILE.LPC := 0;
"&&"      LAST_MVC.LPC := 0;
"**"      IF CKMODE THEN CHECKFREEREGS;
          END (*PLAB*) ;

        PLOC :
          BEGIN
          IF CURPNO >= 0 THEN
            BEGIN
         (* IF ASM THEN
              BEGIN  (* DO NOT PUT LINE # IN DANGAROUS PLACES *)
              IF NOT CASE_FLAG THEN
                 IF OLDOPC <> PDEF THEN WRITELN(OUTPUT,' LOC', Q:7)
              END
            ELSE *)
"&&"          IF DEBUG_LEV > 0 THEN  (*FILL THE ENTRIES OF LINE PTR TABLE*)
                FOR I := LASTLN TO Q-1 DO
                  BEGIN
                  UPDLNTBL(PC-LASTPC) ;
                  LASTPC := PC ;
                  END ;
            END ;
          LASTLN := Q ;
          OPC := OLDOPC ;   (* TO TREAT THIS AS A NOOP *)
          END (*PLOC*) ;

        PDEF :
          BEGIN
       (* IF ASM THEN
            BEGIN  WRITELN(OUTPUT, LBL1.NAM, ' DEF', Q:7) ;
            IF OLDOPC = PRET THEN  WRITELN(OUTPUT,' PEND') ;
            END
          ELSE *)
            IF OLDOPC = PRET THEN
"&&"          BEGIN  GEN_CSECT;  CURPNO := -1;  PC := 0  END
            ELSE (* CTR/CASE EXPRESSION RANGE, PUT BOUNDS IN 'CONSTANT' TABLE *)
"**"          UPD_INTTBL(LBLTBL[LBLMAP(LBL1.NAM)].LNK,Q) ;
          END (*DEF*) ;


        (* BRANCH/CONTROL INSTRUCTIONS *)

        PUJP :
"&&"      BEGIN
"&&"        IF FLOW_TRACE AND NOT CASE_FLAG THEN
"&&"          BEGIN  GENRX(XBAL,RTREG,TRACER,GBR,0);
"&&"            CASE_FLAG := TRUE;
"&&"         (* IF ASM THEN WRITELN(OUTPUT,' DC AL2(',LBL2.NAM:LBL2.LEN,
"&&"                         '-',PRCTBL[0].NAME,')')
"&&"            ELSE *)  UPD_LBLTBL(PC, LBLMAP(LBL2.NAM), FALSE);
"&&"            CASE_FLAG := FALSE;
"&&"            PC := NEXTPC(1);
"&&"          END
"&&"        ELSE  GENRXLAB( XBC, 15, LBL2, 0 );
"&&"      END;

"&&"    PUXJ :
"&&"      BEGIN
"&&"        IF CALL_HIGHER THEN
"&&"          BEGIN  GENRX( XL, TRG0, 20, LBR, 0 );
"&&"                 GENRX( XST, TRG0, DISPLAY+4*CURLVL, GBR, 0 );
"&&"          END;
"&&"        IF FLOW_TRACE THEN
"&&"          BEGIN  GENRXLAB( XL, TRG0, LBL2, -3 );
"&&"                 GENRX( XBAL, RTREG, TRACER, GBR, 0 );
"&&"                 CODE.H[PC] := 0;  PC := NEXTPC(1);
"&&"          END
"&&"        ELSE
"&&"          BEGIN  GENRXLAB( XL, RTREG, LBL2, -3 );
"&&"                 GENRR( XBCR, ANYCND, RTREG );
"&&"          END;
"&&"        OPC := PUJP;
"&&"      END;

        PFJP :
          BEGIN  TOP := TOP-1 ;
          IF (BRCND >= 0) AND (NOT NEG_CND) THEN   (* COND. CODE IS ALIVE *)
            BRCND := 15-BRCND
          ELSE
          WITH STK[TOP] DO
            BEGIN
            IF VRBL THEN
              BEGIN

              IF DRCT AND (VPA = MEM) THEN
                BEGIN  GETOPERAND(STK[TOP],Q1,P1,B1) ;
                IF B1 > 0 THEN  IF P1 > 0 THEN  GENRR(XAR,P1,B1)
                                ELSE  P1 := B1 ;
                GENSI(XTM,Q1,P1,1) ;
                BRCND := 8 (* BZ *) ;  IF NEG_CND THEN BRCND := 1 (* BO *) ;
                END
"@@"          ELSE IF NOT DRCT THEN
"@@"            BEGIN  GETADR(STK[TOP],Q1,P1,B1);
"@@"            IF B1>0 THEN IF P1>0 THEN GENRR(XAR,P1,B1)
"@@"                                 ELSE P1 := B1;
"@@"            GENSI(XTM,Q1,P1,1);
"@@"            BRCND := 8;  IF NEG_CND THEN BRCND := 1;
"@@"            END
              ELSE
                BEGIN  LOAD(STK[TOP]) ;
                GENRR(XLTR,RGADR,RGADR) ;
                BRCND := EQUCND ;  IF NEG_CND THEN BRCND := NEQCND ;
                END ;

              FREEREG(STK[TOP]) ;
              END
            ELSE (*NOT VRBL*)
"&&"          IF FPA.DSPLMT = 0 THEN
"&&"            BEGIN  BRCND := ANYCND;  OPC := PUJP  END
              ELSE  BRCND := NOCND ; (*DO NOT BRANCH*) ;
            IF VRBL THEN IF (VPA = RGS) THEN AVAIL[RGADR] := TRUE ;
            END (*WITH STK...*) ;

"&&"      IF BRCND <> NOCND THEN
"&&"      IF FLOW_TRACE THEN
"&&"        BEGIN  BRCND := 15 - BRCND;
"&&"          IF BRCND > 0 THEN
"&&"            GENRELRX( XBC, BRCND, 5 );  (* BC BRCND,*+10 *)
"&&"          GENRX(XBAL,RTREG,TRACER,GBR,0);
"&&"          CASE_FLAG := TRUE;
"&&"       (* IF ASM THEN
"&&"            WRITELN(OUTPUT,' DC AL2(',LBL2.NAM:LBL2.LEN,
"&&"                     '-',PRCTBL[0].NAME,')') ELSE *)
"&&"          UPD_LBLTBL( PC, LBLMAP(LBL2.NAM), FALSE );
"&&"          CASE_FLAG := FALSE;  PC := NEXTPC(1);
"&&"        END
"&&"      ELSE GENRXLAB(XBC,BRCND,LBL2,0) ;
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
"&&"        IF FLOW_TRACE THEN
"&&"          BEGIN
"&&"            GENRXLAB( XLA, JREG, LBL3, -1 );
"&&"            GENRR( XSR, JREG, PBR1 );
"&&"            GENRXLAB( XC,RGADR,LBL1,-1);
"&&"            GENRELRX( XBC, GRTCND, 9 );  (* BH *+18 *)
"&&"            GENRXLAB( XS, RGADR, LBL2, -1 );
"&&"            GENRELRX( XBC, LESCND, 5 );  (* BM *+10 *)
"&&"            GENRR( XAR, RGADR, RGADR );
"&&"            MKLBL(LBL3,Q+2);
"&&"            GENRXLAB( XLH, JREG, LBL3, RGADR );
"&&"            GENRELRX( XSTH, JREG, 4 );  (* STH JREG,*+8 *)
"&&"            GENRX( XBAL, RTREG, TRACER, GBR, 0 );
"&&"            CODE.H[PC] := 0;  PC := NEXTPC(1);
"&&"          END
"&&"        ELSE
"&&"          BEGIN
               GENRXLAB(XC,RGADR,LBL1,-1) ;   (* CHECK AGAINST UPPER BOUND *)
               GENRXLAB(XBC,GRTCND,LBL3,0) ;  (* GO TO EXIT IF OUT OF RANGE*)
               GENRXLAB(XS,RGADR,LBL2,-1) ;   (* ELSE SUBTRACT LOWER BOUND *)
               GENRXLAB(XBC,LESCND,LBL3,0) ;  (* CASE_EXIT IF OUT OF RANGE *)
               MKLBL(LBL3,Q+2) ;
               GENRR(XAR,RGADR,RGADR) ;  (* CONV. INDEX TO TABLE OFFSET *)
               GENRXLAB(XLH,JREG,LBL3,RGADR);  GENRX(XBC,ANYCND,0,JREG,PBR1);
"&&"           END;

            AVAIL[RGADR] := TRUE ;
            END (* WITH STK[TOP] DO *) ;
          END (*PXJP*) ;

        PPOP :
          BEGIN   TOP := TOP - 1;
"&&"        FREEREG( STK[TOP] );
          END ;

"&&"    PMST :
"&&"      IF CALDPTH < MAXCALDPTH THEN
"&&"        BEGIN  CALDPTH := CALDPTH + 1;
"&&"          WITH CALSTK[CALDPTH] DO
"&&"            BEGIN  PFLEV := P;  DISPSAV := Q  END;
"&&"        END
"&&"      ELSE ERROR( 259 );

        PCUP :
          BEGIN
          CALLSUB ;
"**"      IF (OPNDTYPE <> PROC) AND (OPNDTYPE <> FORT) THEN

            WITH STK[TOP] DO
              BEGIN   VRBL := TRUE  ; DRCT := TRUE ;
              FPA := ZEROBL ; VPA := RGS ;

              CASE OPNDTYPE OF

              ADR,INT :
                BEGIN  FINDRG ;
                GENRX(XL,NXTRG,FNCRSLT,TRG1,0)
                END ;

"&&"          HINT :
"&&"            BEGIN  FINDRG;
"&&"              GENRX(XLH,NXTRG,FNCRSLT,TRG1,0);
"&&"            END;

              BOOL,CHRC :
                BEGIN  FINDRG ;
                GENRR(XSR,NXTRG,NXTRG) ;
                GENRX(XIC,NXTRG,FNCRSLT,TRG1,0) ;
                END ;

              PSET :
                ERROR(616);

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
            END (*WITH STK...*)
"**"      ELSE
"**"        IF CKMODE THEN CHECKFREEREGS;

          CSPREGACTIVE := FALSE ;  OLDCSP := PSIO ;
          END (* CUP*) ;

"&&"    PENT, PRET:  ENT_RET;

        PCSP : CALLSTNDRD ;

"&&"    PCST :   (* BEGINNING OF A CSECT OF STRUCTURED CONSTANTS *)
"&&"      BEGIN
"&&"        PRCTBL[0].NAME := LBL1.NAM;  PRCTBL[0].LNK := 0;
"&&"        FOR CPC := 0 TO 7 DO
"&&"          CODE.C(/CPC/) := LBL1.NAM(/CPC+1/);
"&&"        CPC := 8;  PC := CPC;  CSTBLK := TRUE;  CSEGSTRT := 0;
"&&"        CSEGLIMIT := TXTCHUNK*145;
"&&"      END;
"&&"
"&&"    PDFC:   (* A SIMPLE CONSTANT IN THE CONSTANTS CSECT *)
"&&"      IF CSTBLK THEN
"&&"        IF CADDR <= 32767 THEN
"&&"          BEGIN
"&&"            IF CPC > CADDR THEN ERROR(617);
"&&"            WHILE CPC < CADDR DO
"&&"              BEGIN
"&&"                IF CPC = CSEGLIMIT THEN DUMPCONSTBLK( FALSE );
"&&"                CODE.C(/CPC-CSEGSTRT/) := CHR(0);
"&&"                CPC := CPC + 1;
"&&"              END;
"&&"            PC := CADDR;  Q := PC - CSEGSTRT;
"&&"            CASE OPNDTYPE OF
"&&"
"&&"   BOOL,CHRC: BEGIN
"&&"                IF NOT (IVAL IN [0..255]) THEN ERROR(301);
"&&"                CODE.C(/Q/) := CHR(IVAL);  CPC := CPC + 1;
"&&"              END;
"&&"   HINT:      BEGIN
"&&"                IF (IVAL < -32768) OR (IVAL > 32767) THEN ERROR(301);
"&&"                IF ODD(Q) THEN ERROR(610);
"&&"                CODE.H(/Q DIV 2/) := IVAL;  CPC := CPC + 2;
"&&"              END;
"&&"   INT,ADR:   BEGIN
"&&"                IF Q MOD 4 <> 0 THEN ERROR(611);
"&&"                CODE.I(/Q DIV 4/) := IVAL; CPC := CPC + 4;
"&&"              END;
"&&"   PSET:      BEGIN  IF Q MOD 4 <> 0 THEN ERROR(611);
"&&"                Q := Q DIV 4;
"&&"                FOR P := 1 TO PSLNGTH DIV 4 DO
"&&"                  BEGIN
"&&"                    CODE.I(/Q/) := PSVAL.I(/P/);  Q := Q + 1;
"&&"                  END;
"&&"                CPC := CADDR + PSLNGTH;
"&&"              END;
"&&"   STRG:      BEGIN
"&&"                FOR P := 1 TO SLNGTH DO
"&&"                  BEGIN
"&&"                    CODE.C(/Q/) := SVAL(/P/);  Q := Q + 1;
"&&"                  END;
"&&"                CPC := CADDR + SLNGTH;
"&&"              END;
"&&"   REEL:      BEGIN
"&&"                IF Q MOD 8 <> 0 THEN ERROR(612);
"&&"                CODE.R(/Q DIV 8/) := RVAL;  CPC := CPC + 8;
"&&"              END;
"&&"            END  (*CASE*) ;
"&&"          END
"&&"        ELSE ERROR(251);
"&&"
"&&"   PEND:
"&&"     IF CSTBLK THEN
"&&"       BEGIN  IF CPC > 8 THEN DUMPCONSTBLK( TRUE );
"&&"         TOTALBYTES := TOTALBYTES + CPC;
"&&"         CSTBLK := FALSE;
"&&"       END;

        PSTP :
"NH"   (* IF ASM THEN  BEGIN  (* GENERATE ASSEMBLER END CARD *)
"NH"         WRITELN(OUTPUT,' EXTRN $PASENT');
"NH"         WRITELN(OUTPUT,' END   $PASENT');     END*) ;

        END (*CASE OPC OF*) ;
      END (*COPERATION*) ;


    PROCEDURE UOPERATION ;

    (* UNARY OPERATIONS *)

      BEGIN

        CASE OPC OF

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
               END  (* CONSTANT -- WITH *)
          END  (* PFLT,PFLO *) ;

"NH"    PTRC, PRND :
          BEGIN
            LOAD(STK[ TOP-1 ]) ;
            FINDRG ;
            WITH STK[ TOP-1 ] DO
            BEGIN
"NH"          IF OPC = PRND THEN
                BEGIN
                (* TO ROUND ADD +/-0.5 AND THEN TRUNCATE *)
                GENRR( XLTDR, RGADR, RGADR );
                GENRXDLIT(XLD, TRG0, 0.5) ;  (*TRG0 <-- 0.5*)
"&&"            GENRELRX( XBC, GEQCND, 3 );  (* BNL *+6 *)
                GENRR(XLCDR, TRG0, TRG0) ;         (* TRG0 <-- -0.5 *)
                GENRR(XADR, RGADR, TRG0) ;
"NH"            END (* OPC = PRND *) ;

              GENRX(XAD,RGADR,FL4,GBR,0) ;  GENRX(XSTD,RGADR,FL3,GBR,0) ;
              GENRX(XL,NXTRG,FL3+4,GBR,0) ;
              AVAILFP[ RGADR ] := TRUE ;
"&&"          DTYPE := INT ;  FPA := ZEROBL;
              RGADR := NXTRG
            END (* WITH *)
          END  (* PTRC *) ;

        PNGR :
          WITH STK[ TOP-1 ] DO

            IF VRBL THEN
              BEGIN
              LOAD(STK[TOP-1]) ;  GENRR(XLCDR,RGADR,RGADR)
              END  (* VRBL *)
            ELSE  (* CONSTANT *)
              RCNST := -RCNST ;

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

        PODD :
          WITH STK[TOP-1] DO
          BEGIN
            IF VRBL THEN
              IF DRCT AND (VPA = MEM) THEN
"&&"            BEGIN
"&&"              IF ODD(FPA.DSPLMT) THEN Q := 14 ELSE Q := 1;
"&&"              FPA.DSPLMT := 0;
                  GETOPERAND(STK[TOP-1],Q1,P1,B1) ;
                  IF B1 > 0 THEN  IF P1 > 0 THEN  GENRR(XAR,P1,B1)
                                  ELSE  P1 := B1 ;
"&&"              IF DTYPE = HINT THEN Q1 := Q1 + 1 ELSE Q1 := Q1 + 3;
                  GENSI(XTM,Q1,P1,1) ;  (* RIGHT MOST BYTE IS BEING TESTED *)
"&&"              BRCND := Q (* BO OR BNO *) ;
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
"&&"      GENRX(XL,TRG0,NEWPTR,GBR,0) ;  GENRXLIT(XS,TRG0,P,0) ;
"&&"      IF Q <> 4 THEN  (* MUST ALIGN TO DOUBLEWORD *)
"&&"        GENRXLIT( XN, TRG0, -8, 0 );
          GENRX(XST,TRG0,NEWPTR,GBR,0) ;
          IF NOT STK[TOP].DRCT THEN  LOAD(STK[TOP]) ;
          GETADR(STK[TOP],Q1,P1,B1) ;  GENRX(XST,TRG0,Q1,B1,P1) ;
          FREEREG(STK[TOP]) ;
"&&"      IF DEBUG OR MUSIC THEN   (* CHECK FOR STACK-HEAP INTERFERENCE *)
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


"&&" PROCEDURE PACK_UNPACK(VAR L,R: DATUM);
"&&"   VAR  XOPC: BYTE;
"&&"   BEGIN
"&&"     LOAD( L );  (* LOAD SOURCE ADDRESS *)
"&&"     LOAD( R );  (* LOAD DESTINATION ADDRESS *)
"&&"     IF P = 1 THEN GENRR( XSR, TRG0, TRG0 );  (*FOR BYTE INSERTIONS*)
"&&"     IF IVAL <= 0 THEN BEGIN  ERROR(619);  IVAL := 1  END;
"&&"     FINDRG;  (* REGISTER FOR LOOP COUNT *)
"&&"     GENRXLIT( XL, NXTRG, IVAL, 0 );
"&&"     GENRR( XBALR, TRG1, 0 );  OLDCSP := PSIO;
"&&"     IF P = 1 THEN XOPC := XIC
"&&"     ELSE IF P = 2 THEN XOPC := XLH
"&&"     ELSE BEGIN  XOPC := XL;  IF P<>4 THEN ERROR(619)  END;
"&&"     GENRX( XOPC, TRG0, 0, 0, L.RGADR );
"&&"     IF Q = 1 THEN XOPC := XSTC
"&&"     ELSE IF Q = 2 THEN XOPC := XSTH
"&&"     ELSE BEGIN  XOPC := XST;  IF Q<>4 THEN ERROR(619)  END;
"&&"     GENRX( XOPC, TRG0, 0, 0, R.RGADR );
"&&"     GENRX( XLA, L.RGADR, P, 0, L.RGADR );
"&&"     GENRX( XLA, R.RGADR, Q, 0, R.RGADR );
"&&"     GENRR( XBCTR, NXTRG, TRG1 );
"&&"     AVAIL(/NXTRG/) := TRUE;  AVAIL(/L.RGADR/) := TRUE;
"&&"     AVAIL(/R.RGADR/) := TRUE;
"&&"   END;

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
"&&"        BEGIN
"&&"          IF B1 = -1 THEN LITTBL[L.SCNSTNO].LNK := PC-2;
"&&"          CODE.H[PC-2] := Q1;
"&&"        END;
         IF B2 < 0 THEN
"&&"        BEGIN
"&&"          IF B2 = -1 THEN LITTBL[R.SCNSTNO].LNK := PC-1;
"&&"          CODE.H[PC-1] := Q2;
"&&"        END;
"&&"      IF OPT_FLG THEN IF XOPC = XMVC THEN
"&&"        WITH LAST_MVC DO
"&&"          BEGIN
"&&"            IF PC = (LPC+3) THEN  (* CONSECUTIVE MVC INSTS *)
"&&"              IF (CODE.H[LPC-2]+LLEN) = CODE.H[PC-2] THEN
"&&"                IF (CODE.H[LPC-1]+LLEN) = CODE.H[PC-1] THEN
"&&"                  IF (LLEN+Q) <= 256 THEN
"&&"                    BEGIN
"&&"                      CODE.H[LPC-3] :=CODE.H[LPC-3] + Q;
"&&"                      Q := Q + LLEN;  PC := LPC;
"&&"                      IF B2 = -1 THEN
"&&"                        IF R.SCNSTNO = NXTLIT-1 THEN
"&&"                           NXTLIT := NXTLIT - 1
"&&"                        ELSE
"&&"                          LITTBL[R.SCNSTNO].LNK := 0;
"&&"                    END;
"&&"             LPC := PC;  LLEN := Q;
"&&"           END  (* WITH LAST_MVC *) ;
         END (*SHORT STRING*)
      ELSE
        BEGIN

        (* THIS IS ONLY VALID FOR THE 370,    FOR THE 360 THE 'CLCL'    *)
        (* INSTR. SHOULD BE REPLACED BY AN APPROPRIATE NUMBER OF 'CLC'S *)

        IF (B1 < 0) OR (B2 < 0) THEN  ERROR(202);
        FINDRP ;  GENRX(XLA,NXTRG,Q1,B1,P1) ;  P1 := NXTRG ; B1 := NXTRG+1 ;
        FINDRP ;  GENRX(XLA,NXTRG,Q2,B2,P2) ;  P2 := NXTRG ; B2 := NXTRG+1 ;
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
"&&" LABEL 10, 20, 30;
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

10: IF LR THEN BEGIN  LOP := TOP-1 ;  ROP := TOP  END
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
        LOAD(L);
"&&"    IF R.DTYPE <> INT THEN
"&&"      IF R.DTYPE = HINT THEN
"&&"        OP2 := OP2 - 16  (* SWITCH TO HALFWORD INSTRUCTION *)
"&&"      ELSE
"&&"        LOAD(R);
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
"&&"      IF R.DTYPE = HINT THEN
"&&"        BEGIN
"&&"          IF (NOT R.DRCT) OR (R.VPA = MEM) THEN
"&&"            BEGIN  LOAD(L);  GETOPERAND( R, Q1, P1, B1 );
"&&"              GENRX( XMH, L.RGADR, Q1, P1, B1 );
"&&"                   GOTO 30;
"&&"            END;
"&&"        END;
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
"&&"          IF (R.FPA.DSPLMT>=-32768) AND (R.FPA.DSPLMT<=32767) THEN
"&&"            R.DTYPE := HINT;
"&&"          P := POWER2(R.FPA.DSPLMT);
"&&"          IF (P<0) AND (R.DTYPE<>HINT) THEN MDTAG := PMPI;
"&&"          LOAD(L);  MDTAG := PBGN;  L.FPA.DSPLMT := Q;
"&&"          IF P < 0 THEN
"&&"            IF R.DTYPE <> HINT THEN
"&&"              GENRXLIT( XM, L.RGADR, R.FPA.DSPLMT, 0 )
"&&"            ELSE
"&&"              BEGIN
"&&"                GENRXLIT( XMH, L.RGADR, R.FPA.DSPLMT, -1 );
"&&"                GOTO 30;
"&&"              END
"&&"          ELSE
"&&"            BEGIN
"&&"              IF P > 1 THEN
"&&"                GENRS( XSLL, L.RGADR, 0, P, 0 )
"&&"              ELSE IF P > 0 THEN
"&&"                GENRR( XAR, L.RGADR, L.RGADR );
"&&"              GOTO 30;
"&&"            END;
          END (*IF L.VRBL*);
        L.FPA.DSPLMT := Q ;
        END (*ELSE ...*);

      IF L.VRBL THEN
        AVAIL[L.RGADR] := TRUE ;  L.RGADR := L.RGADR+1 ;
  30: END (*PMPI*) ;

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
          GENRXLIT(XD,L.RGADR,R.FPA.DSPLMT,0) ;
        IF OPC = PDVI THEN
          BEGIN AVAIL[L.RGADR] := TRUE ;  L.RGADR := L.RGADR+1  END
        ELSE AVAIL[L.RGADR+1] := TRUE ;
        END (* ELSE , PDVI *) ;

    PEQU,PNEQ,PGRT,PLEQ,PLES,PGEQ :
"**"  IF OPNDTYPE = PSET THEN
"**"    SETCOMPARE(L,R)
"**"  ELSE
      BEGIN   IF NOT LR THEN OPC := INVBRM[OPC] ;

        CASE OPNDTYPE OF

"&&"    ADR,INT,HINT:
          WITH R DO
            BEGIN
            LOAD(L) ;

            IF VRBL THEN
              BEGIN
"**"          GETOPERAND(R,Q1,P1,B1) ;
"&&"          IF (NOT DRCT) OR (VPA = MEM) THEN
"&&"            IF DTYPE = HINT THEN GENRX(XCH,L.RGADR,Q1,B1,P1)
"&&"            ELSE GENRX(XC,L.RGADR,Q1,B1,P1)
              ELSE
                BEGIN  GENRR(XCR,L.RGADR,RGADR) ;  AVAIL[RGADR] := TRUE  END
              END
            ELSE (*IF NOT VRBL (I.E.CONST)*)
"&&"         BEGIN
"&&"           IF FPA.DSPLMT = 1 THEN
"&&"             IF OPC = PLES THEN  (* COMPARISON AGAINST 0 IS BETTER *)
"&&"               BEGIN  FPA.DSPLMT := 0;  OPC := PLEQ  END
"&&"             ELSE IF OPC = PGEQ THEN
"&&"               BEGIN  FPA.DSPLMT := 0;  OPC := PGRT  END;
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
"&&"        END;

            AVAIL[L.RGADR] := TRUE ;
            END (*WITH R---ADR,INT,...*) ;

"**"    BOOL, CHRC :
"**"      WITH R DO
"**" 20:    IF L.VRBL THEN
"**"          IF (L.VPA=RGS) AND L.DRCT THEN
"**"            BEGIN
"**"              IF VRBL THEN
"**"                IF (VPA=RGS) AND DRCT THEN
"**"                  BEGIN  GENRR( XCR, L.RGADR, RGADR );
"**"                         AVAIL[RGADR] := TRUE;
"**"                  END
"**"                ELSE  BEGIN
"**"                         GETQB( R, Q1, B1, 0 );
"**"                         Q := XCLI*SL24 + B1*SL12 + Q1;
"**"                         GENRXLIT( XEX, L.RGADR, Q, 0 );
"**"                         OPC := INVBRM[OPC];
"**"                      END
"**"              ELSE
"**"                IF FPA.DSPLMT = 0 THEN
"**"                  GENRR( XLTR, L.RGADR, L.RGADR )
"**"                ELSE
"**"                  BEGIN  LOAD(R);  GOTO 20  END;
"**"            AVAIL[L.RGADR] := TRUE;
"**"          END
"**"        ELSE  (* L IS IN MEMORY *)
"**"          IF VRBL THEN
"**"            BEGIN  CLEAR_REG := FALSE;
"**"                   LOAD( STK[ROP] );
"**"                   CLEAR_REG := TRUE;
"**"                   LR := NOT LR;  GOTO 10;
"**"            END
"**"          ELSE BEGIN
"**"                 GETQB( L, Q1, B1, 0 );
"**"                 GENSI( XCLI, Q1, B1, FPA.DSPLMT );
"**"               END
"**"      ELSE  (* L IS A CONSTANT *)
"**"        IF VRBL THEN
"**"          BEGIN  LR := NOT LR;  GOTO 10  END
"**"        ELSE
"**"          BEGIN  LOAD( STK(/ROP/) );  GOTO 10  END;

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
"&&"          IF RCNST = 0.0 THEN GENRR( XLTDR, L.RGADR, L.RGADR )
"&&"          ELSE GENRXDLIT(XCD,L.RGADR,RCNST);
            AVAILFP[L.RGADR] := TRUE ;
          END  (* WITH -- REEL *) ;

        STRG :
          BEGIN
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
"&&"      GENRXDLIT(OP2,L.RGADR,R.RCNST)
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
"&&"       GENRXDLIT(OP2,L.RGADR,R.RCNST)
       END  (* PDVR,PMDR *)   ;

    END (*CASE OPC OF*) ;

  STK[TOP-1] := L ;
  END (*BOPERATION*) ;



    (* LOAD_STOR_MOVE IOPERATIONS *)
    (* -------------------------- *)


    BEGIN (*ASMNXTINST*)

"&&" IF OLDOPC = PUJP THEN
"&&"   IF NOT CASE_FLAG THEN   (* IGNORE INACCESSIBLE INSTRUCTIONS *)
"&&"     IF NOT (OPC IN [PXLB,PEND,PCST,PLAB,PLOC,PDEF,PRET,PSTP,PENT,PCTS])
"&&"       THEN GOTO 10;
    IF BRCND >= 0 THEN
      IF NOT (OPC IN [PFJP,PNOT,PLOC]) THEN   (* XLATE COND CODE TO BOOL. VAL *)
        WITH STK[TOP-1] DO
          BEGIN
            IF NEG_CND THEN   (* JUST NEGATE TOP OF STACK *)
              BEGIN  LOAD(STK[TOP-1]) ;
"&&"            IF OPC = PAND THEN GENRR( XBCTR, RGADR, 0 )
"&&"                          ELSE GENRXLIT( XX, RGADR, 1, 0 );
"&&"          END
            ELSE  (* OTHERWISE TRANSLATE CC TO BOOLEAN *)
              BEGIN  FINDRG ;
                GENRX(XLA,NXTRG,1,0,0) ;  (*ASSUME TRUE*)
"&&"            GENRELRX( XBC, BRCND, 3 );  (* BC BRCND,*+3 *)
                GENRR(XSR,NXTRG,NXTRG) ;  (* THEN CHANGE TO FALSE IF NEEDED*)
"&&"            LAST_CC.LPC := 0;  (* THIS C.C. HAS NO MEANING *)
                 DTYPE := BOOL ;
              VRBL := TRUE ; DRCT := TRUE ;
              VPA := RGS ;  RGADR := NXTRG ;
              FPA := ZEROBL ;
              END (* TRANSLATE CC *) ;
          BRCND := -1 ;  NEG_CND := FALSE ; (* RESET C.C. FLAG TO INACTIVE *)
          END (*WITH STK[..., IF NOT (OPC IN [PFJP,... *) ;

"&&" IF NOT CASE_FLAG THEN
"&&"   IF (NXTLIT >= LITDANGER) OR (NXTDBL >= DBLDANGER) THEN
"&&"     BEGIN  (* EMPTY THE LITERAL POOL NOW *)
"&&"       GENRX( XBC, ANYCND, 0, 0, 0 );
"&&"       I := PC - 1;
"&&"       DUMP_LITERALS;
"&&"       CODE.H[I] := BASE_DSPLMT( PC );
"&&"     END;

    CASE OPC OF

    PLOD  :
      WITH STK[TOP] DO
        BEGIN
        IF OPNDTYPE IN [ADR,INT,PSET] THEN
          BEGIN  IF (Q MOD INTSIZE) <> 0 THEN  ERROR(611) ;  END
        ELSE IF OPNDTYPE = REEL THEN
          BEGIN  IF (Q MOD REALSIZE) <> 0 THEN  ERROR(612) ;  END
"&&"      ELSE IF OPNDTYPE = HINT THEN
"&&"        BEGIN  IF ODD(Q) THEN ERROR(610)  END;
        DTYPE := OPNDTYPE ;  VRBL := TRUE ;  DRCT := TRUE ;
        FPA := ZEROBL ;  VPA := MEM ;
        MEMADR.LVL :=  P ;  MEMADR.DSPLMT := Q ;
"&&"    WITH LAST_STR DO
"&&"      IF LPC = PC THEN  (* TRY TO OPTIMIZE STR/LOD PAIR *)
"&&"        IF MEMADR = STOPND THEN
"&&"          IF OPNDTYPE = STDT THEN  (* IN CASE OF VARIANT RECORDS *)
"&&"            BEGIN  VPA := RGS;  RGADR := STRG;
"&&"              IF OPNDTYPE <> REEL THEN
"&&"                BEGIN
"&&"                  IF NOT AVAIL[RGADR] THEN
"&&"                    BEGIN  FINDRG;  GENRR( XLR, NXTRG, RGADR );
"&&"                           RGADR := NXTRG;
"&&"                    END;
"&&"                  AVAIL[RGADR] := FALSE;
"&&"                END
"&&"              ELSE
"&&"                BEGIN
"&&"                  IF NOT AVAILFP[RGADR] THEN
"&&"                    BEGIN  FINDFP;  GENRR( XLDR, NXTRG, RGADR );
"&&"                           RGADR := NXTRG;
"&&"                    END;
"&&"                  AVAILFP[RGADR] := FALSE;
"&&"                END;
"&&"            END;
        TOP := TOP+1 ;
        END (*LOD,LDO*) ;

    PSTR  :
      BEGIN  TOP := TOP - 1;
      IF OPNDTYPE IN [ADR,INT,PSET] THEN
        BEGIN IF (Q MOD INTSIZE) <> 0 THEN  ERROR(611) ;  END
      ELSE IF OPNDTYPE = REEL THEN
        BEGIN  IF (Q MOD REALSIZE) <> 0 THEN  ERROR(612) ;  END
"&&"      ELSE IF OPNDTYPE = HINT THEN
"&&"        BEGIN  IF ODD(Q) THEN ERROR(610);  END;
"&&"  WITH LAST_STR DO  (* SAVE INFO ABOUT STORED VARIABLE *)
"&&"    BEGIN  STOPND.LVL := P;  STOPND.DSPLMT := Q;
"&&"           STDT := OPNDTYPE;
"&&"           STORE(TOP,FALSE);
"&&"           IF OPNDTYPE <= CHRC THEN
"&&"             LPC := 0
"&&"           ELSE
"&&"             LPC := PC;
"&&"           STRG := STK[TOP].RGADR;
"&&"    END;
      END ;

    PSTO:
      BEGIN  STORE(TOP-1,TRUE(*INDIRECT*)) ;  TOP := TOP-2  END ;

    PLDA  :
      WITH STK[TOP] DO
      BEGIN  DTYPE := ADR ;  VRBL := FALSE ;  DRCT := TRUE ;
      FPA.LVL := P ;  FPA.DSPLMT := Q ;
      TOP := TOP+1 ;
      END ;

    PLDC :
      WITH STK[TOP] DO
      BEGIN  DTYPE := OPNDTYPE ;  VRBL := FALSE ;  FPA := ZEROBL ;
      DRCT := TRUE ;

        CASE OPNDTYPE OF

        ADR:
          FPA.DSPLMT := -1 ; (*LDC NIL*)

        BOOL,CHRC,HINT,INT :
          FPA.DSPLMT :=  IVAL ;

        REEL :
"&&"       RCNST := RVAL;

        PSET :
"**"      ERROR(616);

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
"**"      IF OPNDTYPE = PSET THEN
"**"        BEGIN  VRBL := FALSE;  DRCT := TRUE;  VPA := NEITHER;
"**"          FPA := ZEROBL;  STKADR := 0;  PLEN := PSLNGTH;
"**"          IF PLEN > 0 THEN
"**"            BEGIN  NEW(PCNST);  PCNST@ := PSVAL.S  END
"**"          ELSE PCNST := NIL;
"**"          DTYPE := PSET ;
"**"        END
"&&"      ELSE IF OPNDTYPE = PROC THEN
"&&"        BEGIN  FINDRG;  VRBL := TRUE;  DRCT := TRUE;  VPA := RGS;
"&&"          FPA := ZEROBL;  RGADR := NXTRG;
"&&"          GENRXLAB( XL, RGADR, LBL2, -3 );
"&&"        END
"&&"      ELSE  (* OPNDTYPE IS STRG *)
     (* IF ASM THEN
           BEGIN  FINDRG;
           VRBL := TRUE ;  DRCT := TRUE ;  FPA := ZEROBL ;
           VPA := RGS ;  RGADR := NXTRG ;
           WRITELN(OUTPUT,' LA ',NXTRG:1,',=C''',SVAL:SLNGTH,'''')
           END
        ELSE *)
"&&"      BEGIN
"&&"          IF NXTCH <= HW_GAP*2 THEN
"&&"            BEGIN  HW_GAP := -1;  RHCONF := -1;  IHCONF := -1  END;
"&&"          IF NXTCH <= INT_GAP*4 THEN
"&&"            BEGIN  INT_GAP := -1;  RICONF := -1  END;
"&&"  20:     NXTLIT := NXTLIT + 1;
"&&"          LITTBL[NXTLIT].LNK := -TOP-1;  (* REF. TO EXP. STACK *)
"&&"          SCNSTNO := NXTLIT;  FPA.LVL := -1;
"&&"          FPA.DSPLMT := NXTCH;  VRBL := FALSE;  DRCT := TRUE;
"&&"          FOR I := 1 TO SLNGTH DO
"&&"            BEGIN  IDP_POOL.C[NXTCH] := SVAL[I];
"&&"                   NXTCH := NXTCH + 1;
"&&"            END;
"&&"          I := NXTDBL*8 - NXTCH;
"&&"          WHILE I < 0 DO
"&&"            BEGIN  NXTDBL := NXTDBL + 1;  I := I + 8;  END;
"&&"          NXTINT := NXTDBL * 2;
"&&"          IF I >= 4 THEN
"&&"            BEGIN  I := I - 4;
"&&"              IF INT_GAP < 0 THEN
"&&"                BEGIN  INT_GAP := NXTINT - 1;  NXTINT := INT_GAP;
"&&"                       RICONF := NXTDBL - 1;
"&&"                END;
"&&"            END;
"&&"          IF I >= 2 THEN
"&&"            IF HW_GAP < 0 THEN
"&&"              BEGIN  HW_GAP := 2*NXTINT - 1;  RHCONF := NXTDBL - 1;
"&&"                     IHCONF := NXTINT - 1;
"&&"              END;
"&&"        END;
        TOP := TOP+1;
        END (*PLCA*) ;

    PIXA :
      BEGIN  TOP := TOP-1  ;
      WITH STK[TOP] DO
        BEGIN  IF NOT DRCT THEN  LOAD(STK[TOP]) ;
        IF NOT (DTYPE IN [ADR,HINT,INT,BOOL,CHRC]) THEN  ERROR(601) ;
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
"&&"        ELSE IF DTYPE = HINT THEN
"&&"          GENRX( XLH, NXTRG, Q1, B1, P1 )
            ELSE (*INT,ADR*)  GENRX(XL,NXTRG,Q1,B1,P1) ;
            VPA := RGS ;  RGADR := NXTRG ;
            END ;
          (* VPA IS IN A REG. NOW*)
          IF Q > HALFINT THEN  ERROR(504) ;    (* TOO LARGE FOR A HALF WORD *)
          Q2 := POWER2(Q) ;
"@@"      IF Q2 = 1 THEN GENRR(XAR,RGADR,RGADR)
"@@"      ELSE IF Q2 > 0 THEN
"**"        GENRS(XSLA,RGADR,0,Q2,0)
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

"&&" PPAK :
"&&"   BEGIN  TOP := TOP - 2;
"&&"          PACK_UNPACK( STK(/TOP/), STK(/TOP+1/) );
"&&"   END;

    PMOV :
      BEGIN  TOP := TOP-2 ;
      IF Q > 0 THEN (*FORWARD MOVE*)  SOPERATION(STK[TOP], STK[TOP+1])
      ELSE
         BEGIN  (*BACKWARD MOVE*)
         Q := ABS(Q);   SOPERATION(STK[TOP+1], STK[TOP]);
         END;
      END (*PMOV*) ;


    (* CONTROL/BRANCH OPERATIONS *)

"&&" PUJP,PFJP,PXJP,PPOP,PCUP,PENT,PLOC,PXLB,PUXJ,PMST,
"&&" PRET,PCSP,PSTP,PLAB,PDEF,PDFC,PCST,PEND :  COPERATION ;

"@@" PCHK :                                     CHKOPERATION ;


    (* UNARY OPERATIONS *)

    PABI,PABR,PNGI,PNGR,PINC,PDEC,
    PNOT,PODD,PCHR,PORD,PFLO,PFLT,
    PTRC,PNEW,PSAV,PRST,PSQI,
"NH" PSQR,PCTS,PCTI,PXPO,PRND:       UOPERATION ;


    (* BINARY OPERATIONS *)

    PEQU,PNEQ,PLES,PLEQ,PGRT,PGEQ,
    PADI,PSBI,PMPI,PDVI,PMOD,PAND,
    PIOR,PADR,PSBR,PMPR,PDVR:
      BEGIN  TOP := TOP - 1;   BOPERATION  END;


"**" (* SET OPERATIONS *)
"**"
"**" PINN,PINT,PUNI,PDIF,PASE :
"**"   BEGIN  TOP := TOP - 1;  BSETOPS  END;
"**"
"**" PSCL,PCRD,PSMV,PSLD :  CSETOPS;


    END (*CASE OPC OF*) ;

  OLDOPC := OPC ;
10:  END (*ASMNXTINST*) ;


PROCEDURE SETUP ;
(* INITIALIZE GLOBAL VARIABLE/SET FLAGS ETC. *)
(* ----------------------------------------- *)

"**" (* TYPE  BLOCK_MNEM = ARRAY[0..15] OF ARRAY[1..64] OF CHAR; *)
     VAR  I: INTEGER ;
"**"    (* M:  RECORD  CASE  BOOLEAN  OF
"**"          FALSE: ( X: @MNEM_TABLE );
"**"          TRUE:  ( B: @BLOCK_MNEM );
"**"        END; *)
"&&" CONST
"&&"    PTBL: ARRAY[OPTYPE] OF BETA =
"&&"          ('CTS', 'CTI', 'LOD', 'STR', 'LDA', 'LOC', 'STO', 'LDC',
"&&"           'LAB', 'IND', 'INC', 'POP', 'CUP', 'ENT', 'RET', 'CSP',
"&&"           'IXA', 'EQU', 'NEQ', 'GEQ', 'GRT', 'LEQ', 'LES', 'UJP',
"&&"           'FJP', 'XJP', 'CHK', 'NEW', 'ADI', 'ADR', 'SBI', 'SBR',
"&&"           'SCL', 'FLT', 'FLO', 'TRC', 'NGI', 'NGR', 'SQI', 'SQR',
"&&"           'ABI', 'ABR', 'NOT', 'AND', 'IOR', 'DIF', 'INT', 'UNI',
"&&"           'INN', 'MOD', 'ODD', 'MPI', 'MPR', 'DVI', 'DVR', 'MOV',
"&&"           'LCA', 'DEC', 'STP', 'SAV', 'RST', 'CHR', 'ORD', 'DEF',
"&&"           'RND', 'CRD', 'XPO', 'BGN', 'END', 'ASE', 'SLD', 'SMV',
"&&"           'MST', 'UXJ', 'XLB', 'CST', 'DFC', 'PAK', '???' );
"&&"    CSPTBL: ARRAY[CSPTYPE] OF BETA =
"&&"         ( 'GET', 'PUT', 'RES', 'RLN', 'REW', 'WLN', 'WRS', 'ELN',
"&&"           'WRI', 'WRR', 'WRC', 'RDI', 'RDR', 'RDC', 'RDH', 'RDY',
"&&"           'RDD', 'WRD', 'WRE', 'EOL', 'EOF', 'XIT', 'RDS', 'TRP',
"&&"           'SIO', 'EIO', 'CLK', 'FDF', 'PAG', 'NUL', 'RDB', 'WRB',
"&&"           'SKP', 'LIM', 'MSG', 'EOT', 'CTR', 'TRA', '???' );

  BEGIN
"NH" EMPTY := '   ';
(*
"**"  NEW( M.B );   XTBL := M.X;
M.B@[0] :='(00)(01)(02)(03)SPM BALRBCTRBCR SSK ISK SVC (0B)(0C)(0D)MVCLCLCL';
M.B@[1] :='LPR LNR LTR LCR NR  CLR OR  XR  LR  CR  AR  SR  MR  DR  ALR SLR ';
M.B@[2] :='LPDRLNDRLTDRLCDRHDR LRDRMXR MXDRLDR CDR ADR SDR MDR DDR AWR SWR ';
M.B@[3] :='LPERLNERLTERLCERHER LRERAXR SXR LER CER AER SER MER DER AUR SUR ';
M.B@[4] :='STH LA  STC IC  EX  BAL BCT BC  LH  CH  AH  SH  MH  (4D)CVD CVB ';
M.B@[5] :='ST  (51)(52)(53)N   CL  O   X   L   C   A   S   M   D   AL  SL  ';
M.B@[6] :='STD (61)(62)(63)(64)(65)(66)MXD LD  CD  AD  SD  MD  DD  AW  SW  ';
M.B@[7] :='STE (71)(72)(73)(74)(75)(76)(77)LE  CE  AE  SE  ME  DE  AU  SU  ';
M.B@[8] :='SSM (81)LPSWDIAGWRD RDD BXH BXLESRL SLL SRA SLA SRDLSLDLSRDASLDA';
M.B@[9] :='STM TM  MVI TS  NI  CLI OI  XI  LM  (99)(9A)(9B)SIO TIO HIO TCH ';
M.B@[11]:='(B0)LRA STCK(B3)(B4)(B5)STCTLCTL(B8)(B9)(BA)(BB)(BC)CLM STCMICM ';
M.B@[13]:='(D0)MVN MVC MVZ NC  CLC OC  XC  (D8)(D9)(DA)(DB)TR  TRT ED  EDMK';
M.B@[15]:='SRP MVO PACKUNPK(F4)(F5)(F6)(F7)ZAP CP  AP  SP  MP  DP  (FE)(FF)';
*)

  BRMSK[PEQU] :=  8      ;    BRMSK[PNEQ] :=  7      ;
  BRMSK[PGEQ] := 11      ;    BRMSK[PGRT] :=  2      ;
  BRMSK[PLEQ] := 13      ;    BRMSK[PLES] :=  4      ;

  INVBRM[PEQU]:= PEQU    ;    INVBRM[PNEQ]:= PNEQ    ;
  INVBRM[PGEQ]:= PLEQ    ;    INVBRM[PGRT]:= PLES    ;
  INVBRM[PLEQ]:= PGEQ    ;    INVBRM[PLES]:= PGRT    ;


  FOR I := 0 TO HTSIZE DO  HTBL[I].NAME := EMPTY ;

  OP_SP := TRUE ;
"&&"  FOR OPC := PCTS TO PRED(UNDEF_OP) DO
"&&"    BEGIN NMCDE := PTBL[OPC]; ENTERLOOKUP  END;
      OP_SP := FALSE ;
"&&"  FOR CSP := PGET TO PRED(UNDEF_CSP) DO
"&&"    BEGIN NMCDE := CSPTBL[CSP];  ENTERLOOKUP  END;
      OP_SP := TRUE ;               (*TO PREPARE FOR OPCODE LOOKUP*)

  FOR NXTRG := 0 TO RGCNT DO AVAIL[NXTRG] := TRUE ;

  FOR NXTRG := 0 TO FPCNT DO AVAILFP[NXTRG] := TRUE ;

  FOR CH := 'A' TO 'Z' DO TYPCDE[CH] := NON ;

  TYPCDE['A'] := ADR ;  TYPCDE['B'] := BOOL ;
  TYPCDE['C'] := CHRC;  TYPCDE['I'] := INT ;
"&&"                    TYPCDE['H'] := HINT ;
  TYPCDE['M'] := STRG;  TYPCDE['S'] := PSET;
  TYPCDE['P'] := PROC;  TYPCDE['R'] := REEL;
  TYPCDE['N'] := ADR ;  TYPCDE['J'] := INX ;
  TYPCDE['F'] := FORT;  TYPCDE['X'] := FBOOL;
  TYPCDE['Y'] := FINT;  TYPCDE['Z'] := FREAL;

  TOP := 1 ;  CURLVL := 1 ;  BRCND := -1 ;  NEG_CND := FALSE ;  TRACE := FALSE ;
  OLDOPC := PBGN ;  OLDCSP := PSIO ;  MDTAG := PBGN ;
"&&" TXRG := TRG14 ;  MUSIC := FALSE;
  ZEROBL.LVL := 0 ;  ZEROBL.DSPLMT := 0 ;  ERRORCNT := 0 ;  S370CNT := 0 ;
  LCAFTSAREA := LCAFTMST ;  SAVERGS := TRUE ; SAVEFPRS := TRUE ;
  CLEAR_REG := TRUE ;  PRE_PASS := TRUE ; OS_STYLE := TRUE ;
  TOTALBYTES := 0 ;   CASE_FLAG := FALSE ;
  FILECNT      := 0 ;     CKMODE := FALSE ;
  ASM := FALSE;  ASMVERB := FALSE;  DEBUG := TRUE;
  FLOW_TRACE := FALSE;  CURPNO := -1;
  NXTLIT := 0; NXTDBL := 0;
"&&" LAST_CC.LPC := 0;  TXR_CONTENTS.VALID := FALSE;
"&&" LAST_MVC.LPC := 0;  LAST_STR.LPC := 0;
"&&" LAST_STR.STOPND := ZEROBL;  OPT_FLG := TRUE;
"&&" HEXCHARS := '0123456789ABCDEF';
"**" TESTCNT := 0;
  MARK(HEAPMARK) ;
  END (*SETUP*) ;



BEGIN (*PCODE_TRANSLATOR*)

RESET (INPUT);  (*opp*)

INIT := TRUE ;  SETUP ;  INIT := FALSE ;  (*INITIALIZE*)
"**" IF OSPARM <> NIL THEN
"**"   WITH OSPARM@ DO
"**"     IF LENGTH >= 2 THEN
"**"       FOR Q := 1 TO LENGTH-1 DO
"**"         IF (STRING[Q]='T') AND (STRING[Q+1]='R') THEN
"**"           TRACE := TRUE
"**"         ELSE IF (STRING[Q]='C') AND (STRING[Q+1]='K') THEN
"**"           CKMODE := TRUE
"&&"         ELSE IF (STRING(/Q/)='M') AND (STRING(/Q+1/)='U') THEN
"&&"           MUSIC := TRUE;
TIMER := CLOCK(0) ;

WRITE(OUTPUT,'****':9, 'STANFORD PASCAL POST-PROCESSOR, MCGILL VERSION OF ':56,
                       VERSION);
"&&" IF NOT MUSIC THEN WRITELN(OUTPUT);

REPEAT
  READNXTINST ;
  ASMNXTINST ;
  IF TRACE THEN  DUMPSTK(1,TOP-1) ;
UNTIL OPC = PSTP ;

"NH" TIMER := CLOCK(0) - TIMER ;
"&&" IF NOT MUSIC THEN WRITELN(OUTPUT);
     WRITE(OUTPUT,'****':9);
IF ERRORCNT > 0 THEN WRITE(OUTPUT,ERRORCNT:8)
ELSE WRITE(OUTPUT, 'NO':8) ;
WRITELN(OUTPUT, ' ASSEMBLY ERROR(S) DETECTED.') ;
"&&" IF NOT MUSIC THEN WRITELN(OUTPUT);
WRITELN(OUTPUT, '****':9, TOTALBYTES:8,
                ' BYTES OF CODE GENERATED,', TIMER*0.001:6:2,
                ' SECONDS IN POST_PROCESSING.');

IF S370CNT > 0 THEN
  WRITELN(OUTPUT, '****':9, S370CNT:8,
                  ' "370"-ONLY INSTRUCTION(S) ISSUED.');


EXIT(ERRORCNT) ;


END.
