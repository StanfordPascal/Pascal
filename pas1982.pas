(*$D-,N+*)

PROGRAM PASCALCOMPILER(INPUT, OUTPUT, PRR, QRR);

 (******************************************************************
  *                                                                *
  *         S T A N F O R D   P A S C A L   C O M P I L E R        *
  *                                                                *
  *                  MCGILL UNIVERSITY VERSION                     *
  *                                                                *
  *                                                                *
  *         AUTHOR OF ORIGINAL PROGRAM:                            *
  *                                                                *
  *              URS AMMANN                                        *
  *              FACHGRUPPE COMPUTERWISSENSCHAFTEN                 *
  *              EIDG. TECHNISCHE HOCHSCHULE                       *
  *              CH-8006 ZUERICH                                   *
  *                                                                *
  *                                                                *
  *         AUTHOR OF SECOND VERSION:                              *
  *                                                                *
  *              KESAV NORI                                        *
  *              COMPUTER GROUP                                    *
  *              T.I.F.R.                                          *
  *              HOMI BHABHA ROAD                                  *
  *              BOMBAY - 400005                                   *
  *              INDIA                                             *
  *                                                                *
  *                                                                *
  *         AUTHOR OF THIRD VERSION (STANFORD PASCAL):             *
  *                                                                *
  *              S. HAZEGHI                                        *
  *              COMPUTATION RESEARCH GROUP                        *
  *              STANFORD LINEAR ACCELERATOR CENTER                *
  *              STANFORD, CA. 94305.                              *
  *                                                                *
  *                                                                *
  *         AUTHOR OF FOURTH VERSION (MCGILL PASCAL):              *
  *                                                                *
  *              R. NIGEL HORSPOOL                                 *
  *              SCHOOL OF COMPUTER SCIENCE                        *
  *              MCGILL UNIVERSITY                                 *
  *              MONTREAL  QUEBEC  H3A 2K6                         *
*
* 02FEB2007 - Changes by Dave Edwards to use hex codes C0,D0
*    (was 8B,9B) for EBCDIC brace characters (curly brackets).
*    New constants CHLBRACE and CHRBRACE defined.
*    VERSION date left unchanged as MAY -82.
*    Hex C0,D0 seem to be the codes used by most current software
*    such as tn3270 clients, Ascii-EBCDIC translate tables, etc.
*    and conform to the standard IBM-037 US code page.
*    See additional notes in file ccde:pascal_info.txt .
*  - Also, in INITTABLES procedure, set UPSHIFT to only upshift
*    lowercase chars (e.g. exclude tilde, which is in the range
*    a to z), and add comments re. definition of SSY array.
*    Note that curly brackets and backslash are within the
*    range A to Z in the EBCDIC character set.
*  - Also fix spacing of text for BGN output record: change TIME:9
*    to TIME:8. Was causing last char of year to be truncated, in
*    the info text at the start of $MAINBLK csect in the object file.
*  - Source changes are identified by flag DE near beginning of lines.
*    (Write date of previous source file: 14sep1983.)
*
  *                                                                *
  *                                                                *
  ******************************************************************)



CONST
#      VERSION     = 'MAY -82';
#      PAGESIZE    = 55;           (*MAX # OF LINES PER PAGE OF LISTING      *)
#      MAXINT      = 2147483647;
#      MAXADDR     = 16777215;
#      SETMAX      = 255;          (*LARGEST POSSIBLE SET ELEMENT            *)
"**"   SSETMAX     =  63;          (*LARGEST ELEMENT USED IN THIS CODE       *)
#      BUFLEN      = 122;          (*MAX LINE LENGTH + 2                     *)
"BM"   INTSIZE     = 4;
"&&"   HINTSIZE    = 2;
"BM"   REALSIZE    = 8;
"BM"   CHARSIZE    = 1;
"&&"   MXDATASZE   = 8;
"BM"   BOOLSIZE    = 1;
"**"   WORDSIZE    = 4;            (* NUMBER OF BYTES PER WORD              *)
"**"   SETPACK     = 32;           (* NUMBER OF SET ELEMENTS PER WORD       *)
"**"   MAXSETSIZE  = 32;           (*  = (SETMAX+1) DIV SETPACK * WORDSIZE  *)
"BM"   PTRSIZE     = 4;
"@@"   FILHDRSIZE  = 8;
#      REALLNGTH   = 20;           (*STRING REPRESENTATION OF REAL NUMBERS   *)
#      DIGMAX      = 19;           (*REALLNGTH-1                             *)
#      IDLNGTH     = 12;
#      ALFALNGTH   = 10;
#      STRGLNGTH   = 64;
#      DISPLIMIT   = 20;
"&&"   MAX_BKT     = 58;           (* HASH TABLE SIZE                        *)
"&&"   MAXLEVEL    =  9;
#      ORDCHMAX    = 255;          (*SIZE OF CHAR SET OF TARGET MACHINE      *)
"&&"   OPMAX       = 76;           (* OPCODE RANGE  *)
#      MAXERRNR    = 401;          (*MAX VAL OF ERROR CODE                   *)
#      MAXERRLOG   =   8;          (* > (MAXERRNR DIV SETMAX)                *)
"&&"   NRSW        = 38;
"&&"   NRSW1       = 39;           (*NRSW+1                                  *)
"NH"   NSPROC      = 36;           (* # OF STANDARD PROCS                    *)
       NPDW        = 57;           (* # OF PREDEFINED WORDS                  *)
"CT"   CTRMAX      = 16384;
"@@"   EXTNAMSZ    = 8;            (* EXTERNAL NAME LENGTH                  *)

"BM"   (*SAVE AREAS, FUNCTION RETURN VALUE SPACE, DISPLAY AREA, ETC.         *)
"BM"   LCAFTMST    = 80;    FPSAVEAREA  = 32;     RUNCHKAREA  = 96;
"BM"   DISPADR     = 80;    FNCRSLT     = 72;     DISPAREA    = 40;
"&&"   FIRSTCONSTLC=  8;
"BM"   FIRSTFILBUF = 248;          (* = LCAFTMST+RUNCHKAREA+DSPLYAREA        *)
"@@"   TIMEDATELOC = 328;          (* LOCATION OF TIME/DATE PREDEF. VARS     *)
"@@"   OSPARMLOC   = 348;          (* LOCATION FOR 'OSPARM' PTR.             *)
"@@"   FIRSTGVAR   = 352;          (* FIRST USER DEFINED GLOBAL VARIABLE     *)
"DE"  CHLBRACE = '{';   (* LEFT CURLY BRACKET: EBCDIC HEX CODE C0 *)
"DE"  CHRBRACE = '}';   (* RIGHT CURLY BRACKET: EBCDIC HEX CODE D0 *)

(*----------------------------------------------------------------------------*)


TYPE
                                                            (*BASIC SYMBOLS   *)
                                                            (**************   *)

     SYMBOL       = (IDENT,INTCONST,REALCONST,STRINGCONST,NOTSY,MULOP,ADDOP,
                     RELOP,LPARENT,RPARENT,LBRACK,RBRACK,COMMA,SEMICOLON,PERIOD,
                     ARROW,COLON,DOTDOT,BECOMES,LABELSY,CONSTSY,TYPESY,VARSY,
                     FUNCSY,PROGSY,PROCSY,SETSY,PACKEDSY,ARRAYSY,RECORDSY,
                     FILESY,FORWARDSY,BEGINSY,IFSY,CASESY,REPEATSY,WHILESY,
                     FORSY,WITHSY,GOTOSY,ENDSY,ELSESY,UNTILSY,OFSY,DOSY,TOSY,
"&&"                 DOWNTOSY,THENSY,FRTRNSY,EXTRNSY,OTHERWISESY,OTHERSY);

     OPERATOR     = (MUL,RDIV,ANDOP,IDIV,IMOD,PLUS,MINUS,OROP,LTOP,LEOP,GEOP,
                     GTOP,NEOP,EQOP,INOP,NOOP,
#                    ATOZCH, NUMCH, QUOTCH, DQUOTCH, COLONCH, DOTCH, LPARCH,
#                    LBRACE, UNDSCH, DOLLARCH, SKIPCH, SPECH, ILLEGCH ) ;

     SETOFSYS     = SET OF SYMBOL;
"**" SSETINX      = 1..4;          (* 4 = (SETMAX+1) DIV (SSETMAX+1)  *)
"**" SSETRANGE    = SET OF 0..SSETMAX;
"**" SETRANGE     = ARRAY[SSETINX] OF SSETRANGE;


                                                            (*CONSTANTS       *)
                                                            (**********       *)
     CSTCLASS     = (REEL,PSET,STRG);
     CSP          = @ CONSTANT;
     CONSTANT     = RECORD CASE CSTCLASS OF
                           REEL: (RVAL: PACKED ARRAY [1..REALLNGTH] OF CHAR);
"**"                       PSET: (PLNGTH: 0..MAXSETSIZE; PVAL: SETRANGE  );
                           STRG: (SLNGTH: 0..STRGLNGTH;
                                  SVAL: PACKED ARRAY [1..STRGLNGTH] OF CHAR)
                         END;

     VALU         = RECORD CASE BOOLEAN OF
                      TRUE:  (IVAL: INTEGER);
                      FALSE: (VALP: CSP)
                    END;

                                                           (*DATA STRUCTURES  *)
                                                           (****************  *)
     LEVRANGE     = 0..MAXLEVEL;       ADDRRANGE   = 0..MAXADDR;
     ALNRNG       = 1..8 ;             LABELRNG    = 0..1000 ;
"&&" BKT_RNG      = 0 .. MAX_BKT ;
     STRUCTFORM   = (SCALAR,SUBRANGE,POINTER,POWER,ARRAYS,RECORDS,FILES,
                     TAGFLD,VARIANT);
     DECLKIND     = (STANDARD,DECLARED);
     STP          = @ STRUCTURE;
     CTP          = @ IDENTIFIER;

     STRUCTURE    = RECORD
                    SIZE: ADDRRANGE;
"&&"                ALN:  ALNRNG;         (*ALIGNMENT FACTOR*)
                    CASE FORM: STRUCTFORM OF
                      SCALAR:   (CASE SCALKIND: DECLKIND OF
                                   DECLARED: (FCONST: CTP));
                      SUBRANGE: (RANGETYPE: STP; MIN,MAX: VALU);
                      POINTER:  (ELTYPE: STP);
                      POWER:    (ELSET: STP);
                      ARRAYS:   (AELTYPE,INXTYPE: STP);
"&&"                  RECORDS:  (FSTFLD: CTP; RECVAR: STP;
"SY"                             NO_FLDS: 0 .. 1000;
"&&"                             FLD_DISP_LEV: -1..DISPLIMIT );
                      FILES:    (FILTYPE: STP);
                      TAGFLD:   (TAGFIELDP: CTP; FSTVAR: STP);
"&&"                  VARIANT:  (NXTVAR,SUBVAR: STP;
"&&"                             FSTSUBFLD: CTP; VARVAL: VALU)
                    END;

                                                            (*NAMES           *)
                                                            (******           *)

"&&" IDCLASS      = (TYPES,KONST,STRUCTKONST,VARS,FIELD,PROC,FUNC);
     SETOFIDS     = SET OF IDCLASS;
     IDKIND       = (ACTUAL,FORMAL);
     ALPHA        = PACKED ARRAY [1..IDLNGTH] OF CHAR;

     IDENTIFIER   = RECORD
                    NAME: ALPHA;
"&&"                IDTYPE: STP; NEXT_IN_BKT, NEXT: CTP;
"&&"                DECL_LEV: LEVRANGE;
                    CASE KLASS: IDCLASS OF
                      KONST: (VALUES: VALU);
"&&"                STRUCTKONST: (SKOWNER: CTP; SKADDR: ADDRRANGE);
                      VARS:  (VKIND: IDKIND;
                              VLEV: LEVRANGE; VADDR: ADDRRANGE);
"&&"                  FIELD: (FLDADDR: ADDRRANGE;  OWNER: STP );
                      PROC, FUNC:
                        (CASE PFDECKIND: DECLKIND OF
                           STANDARD: (KEY: 0..NSPROC);
                           DECLARED:
"&&"                         (PFLEV: INTEGER; PFNAME: LABELRNG;
"&&" "SH"                     PRMPTR,NXTFWRD: CTP;  PFKIND: IDKIND;
                              FWDECL,EXTRN,FRTRN:BOOLEAN;
"@@"                          EXTNAME: ARRAY[1..EXTNAMSZ] OF CHAR))
                    END;


     DISPRANGE    = 0..DISPLIMIT;
"&&" HASH_TABLE   = ARRAY(/ BKT_RNG /) OF CTP;
     WHERE        = (BLCK,CREC,VREC,REC);

                                                            (*EXPRESSIONS     *)
                                                            (************     *)
     ATTRKIND     = (CST,VARBL,EXPR);
"**" VACCESS      = (DRCT,INDRCT,INXD,STKEXPR);

#    ATTR         = RECORD
"&&"                 TYPTR,      (* TYPE AS AN EXPR. ON RUN-STACK *)
"&&"                 BTYPE: STP; (* TYPE AS A VARIABLE IN MEMORY  *)
                     CASE KIND: ATTRKIND OF
                       CST:   (CVAL: VALU);
                       VARBL: (CASE ACCESS: VACCESS OF
                                 DRCT: (VLEVEL: LEVRANGE; DPLMT: ADDRRANGE);
                                 INDRCT: (IDPLMT: ADDRRANGE);
"**"                             STKEXPR: (STKDPLMT,STKLEN: ADDRRANGE))
                     END;

     TESTP        = @ TESTPOINTER;
     TESTPOINTER  = PACKED RECORD
                      ELT1,ELT2 : STP;
                      LASTTESTP : TESTP
                      END;

                                                                 (*LABELS     *)
                                                                 (*******     *)
     LBP          = @ LABL;
     LABL         = RECORD NEXTLAB: LBP;  LABVAL: INTEGER;
"&&"                  LABNAME, XNO: LABELRNG;  DEFINED: BOOLEAN
                    END;

     FRECPTR      = @FILEREC;
     FILEREC      = RECORD FILIDPTR: CTP; NEXTFILE: FRECPTR;  END;

"&&" PRNTTYLISTP = @PRNTTYLIST;
"&&" PRNTTYLIST  = RECORD  ELT: STP;  TNO: 0..999;  NXT: PRNTTYLISTP  END;

#    ERRCODE      = 0..MAXERRNR;

"CT" CTRRANGE   = 0..CTRMAX;
"CT" CTRTYPE    = (CTRPROC, CTRLBL, CTRGOTO, CTRIF, CTRWHILE, CTRREPEAT,
"CT"               CTRFOR, CTRCASE);

(*----------------------------------------------------------------------------*)


VAR


                                    (*RETURNED BY SOURCE PROGRAM SCANNER
                                     INSYMBOL:
                                     **********)

    SY: SYMBOL;                     (*LAST SYMBOL                             *)
    OP: OPERATOR;                   (*CLASSIFICATION OF LAST SYMBOL           *)
    VAL: VALU;                      (*VALUE OF LAST CONSTANT                  *)
    LNGTH: INTEGER;                 (*LENGTH OF LAST STRING CONSTANT          *)
    PROGNAME,
    ID:  ALPHA ;                    (*LAST IDENTIFIER (POSSIBLY TRUNCATED)    *)
    CH: CHAR;                       (*LAST CHARACTER READ                     *)
    EOL: BOOLEAN;                   (*END OF LINE FLAG                        *)


                                    (*COUNTERS:                               *)
                                    (**********                               *)

    CHCNT: 0..BUFLEN;               (*CHARACTER COUNTER                       *)
"&&" CONSTLC,                       (*DATA LOC. FOR STRUCTURED CONSTANTS      *)
    LC, IC, OLDIC, STIC: ADDRRANGE; (*DATA LOCATION AND INSTRUCTION COUNTER   *)
#   LINECNT, OLDLN, PLCNT, ERRLN,
"&&"  PAGECNT, LASTLINELISTED: INTEGER;


                                    (*SWITCHES:                               *)
                                    (**********                               *)

    HP,                             (*HEADER PART                             *)
    PRTERR,                         (*TO ALLOW FORWARD REFERENCES IN PTR TYPE *)
                                    (*DECLARATION BY SUPPRESSING ERROR MSG    *)
#   DOTFLG,                         (*ONE DOT ALREADY SEEN                    *)
#   ASSIGN,PACKDATA,                (*ASSIGNMENT GOING ON,PACKING IN EFFECT   *)
#   LIST, PRCODE,                   (*LIST SOURCE, OUTPUT P-CODE              *)
#   DEBUG, MWARN,                   (*DEBUG CODE WANTED, MARGINS WARNING      *)
"&&"FLIPDEBUG, NOPACKING,           (*DEBUG FLIPPED, PACKING SUPPRESSED       *)
"&&" NESTCOMM, MUSIC,               (*NESTED COMMENTS ALLOWED, MUSIC SYSTEM   *)
"&&" WARNING, EXTUSED,              (*WARNINGS WANTED, EXTENSIONS USED        *)
#   ASSEMBLE, ASMVERB,              (*POSTPROCESSOR TRANSLATION, VERBOSE      *)
    XLINK, GET_STAT,                (*EXTERNAL LINKAGE, NAME CHANGE FLAG      *)
#   SAVEREGS, SAVEFPRS: BOOLEAN;
"&&" LISTTAG:   CHAR;               (* LISTING TAG, 'D'/'C'/'N'/' '           *)
"&&"DEBUG_LEV:  0 .. 9;
#
                                    (*POINTERS:                               *)
                                    (**********                               *)

"@@" INPUTPTR, OUTPUTPTR: CTP;      (* PREDEFINED FILES INPUT + OUTPUT  *)
    INTPTR,REALPTR,CHARPTR,BOOLPTR,
    NILPTR,TEXTPTR,ALFAPTR: STP;    (*POINTERS TO ENTRIES OF STANDARD IDS     *)
    UTYPPTR,UCSTPTR,UVARPTR,
    UFLDPTR,UPRCPTR,UFCTPTR,        (*POINTERS TO ENTRIES FOR UNDECLARED IDS  *)
"&&" MAINPROG,                      (*POINTER TO $MAINBLK ENTRY               *)
"&&"FRTPARHD,                       (*POINTER TO LIST OF FORTRAN PROC PARMS   *)
    FWPTR: CTP;                     (*HEAD OF CHAIN OF FORW DECL TYPE IDS     *)
#   FILEHEAD: FRECPTR ;             (*HEAD OF CHAIN OF EXTERNAL FILES         *)
"&&" OPEN_RECORD: STP;              (*CURRENT RECORD OPENED BY "WITH"         *)
    GLOBTESTP: TESTP;               (*LAST TESTPOINTER                        *)
"&&" PRNTTYPHD: PRNTTYLISTP;        (*LIST OF HEAP STORAGE ITEMS FOR DEBUG    *)
"&&" PRNTTYNO:  0..999;


                                    (*BOOKKEEPING OF DECLARATION LEVELS:      *)
                                    (***********************************      *)

    LEVEL: LEVRANGE;                (*CURRENT STATIC LEVEL                    *)
"&&" STMTNEST: 0 .. 100;            (*CURRENT STATEMENT NESTING               *)
    DISX,                           (*LEVEL OF LAST ID SEARCHED BY SEARCHID   *)
"&&"TOP: -1..DISPLIMIT;             (*TOP OF DISPLAY                          *)

    DISPLAY:                        (*WHERE:   MEANS:                         *)
      ARRAY [DISPRANGE] OF
        PACKED RECORD               (*=BLCK:   ID IS VARIABLE ID              *)
          CASE OCCUR: WHERE OF      (*=CREC:   ID IS FIELD ID IN RECORD WITH  *)
            BLCK: (FLABEL: LBP);    (*         CONSTANT ADDRESS               *)
            CREC: (CLEV: LEVRANGE;  (*=VREC:   ID IS FIELD ID IN RECORD WITH  *)
                  CDSPL: ADDRRANGE);(*         VARIABLE ADDRESS               *)
            VREC: (VDSPL: ADDRRANGE)
          END;                      (* --> PROCEDURE WITHSTATEMENT            *)


                                    (*RUN-TIME PROFILER COUNTERS              *)
                                    (***************************              *)

"CT"  CTRCNT     : CTRRANGE;
"CT"  CTRCNTLBL  : LABELRNG;
"CT"  CTROPTION  : BOOLEAN;



                                    (*EXPRESSION COMPILATION:                 *)
                                    (************************                 *)

    GATTR        : ATTR;            (*DESCRIBES THE EXPR CURRENTLY COMPILED   *)

    MXINT10      : INTEGER;

                                    (*BUFFERS, READ ONLY TABLES ETC.          *)
                                    (******************************           *)

    LSTOP        : CHAR;                      (*MARKS THE BEGINNING OF LINEBUF*)
    LINEBUF      : ARRAY [1..BUFLEN] OF CHAR; (*CURRENT LINE BUFFER           *)
"NH"LMARGIN, RMARGIN, LINELEN, BUFEND,
"NH"LASTCOL      : 0..BUFLEN;                 (*LEFT, RIGHT MARGINS ant PTRS  *)

"&&"INTLABEL, PROCLAB, XLABNO: LABELRNG ;

#   CALL_LVL     : ARRAY[BOOLEAN] OF INTEGER ;

"&&" SOP          : PACKED ARRAY [CHAR] OF OPERATOR;
     UPSHIFT      : ARRAY [char (*SHOULD BE CHAR*) ] OF CHAR ;          (*opp*)
"&&" SSY          : PACKED ARRAY [char (*SHOULD BE CHAR*)] OF SYMBOL;   (*opp*)
"&&" BUCKET      : HASH_TABLE;


"SY"                                (* SYMBOL TABLE USAGE STATISTICS          *)
"SY"                                (* ****** ***** ***** **********          *)
"SY"
"SY" FENT_CNT, SF_CNT, SF_TOT,      (* # FIELD ENTRIES, SEARCHES, PRODUCT     *)
"SY" WE_CNT, RE_CNT,                (* # "WITH" LOOKUPS, # RECORDS            *)
"SY" WS_CNT:          INTEGER;      (* # WITH STATEMENTS                      *)
"SY" PROC_CNT, ENT_CNT: ARRAY[LEVRANGE] OF INTEGER;
"SY" LU_CNT: ARRAY[LEVRANGE,DISPRANGE] OF INTEGER;
"SY" WLU_CNT: ARRAY[1..10,1..10] OF INTEGER;

                                    (*ERROR MESSAGES:                         *)
                                    (****************                         *)

#   ERRLOG       : ARRAY [0..MAXERRLOG(* = 400 DIV SSETMAX+1*)] OF SSETRANGE;
"&&"ERRORCNT, WARNCNT,              (*ERRORS AND WARNINGS COUNTS              *)
#   CTIME:      INTEGER;            (*COMPILATION TIME                        *)
    ERRINX       : 0..10;           (*NR OF ERRORS IN CURRENT SOURCE LINE     *)
"&&"ERRKIND      : CHAR;            (*KIND OF ERROR, 'E' / 'W' (WARNING)      *)
    ERRLIST      : ARRAY[1..10] OF
"&&"                 PACKED RECORD NMR: 1..401;
"&&"                        KIND: CHAR;  POS: 1..81
                     END;

                                   (* STRUCTURE CONSTANTS, READ-ONLY TABLES   *)
                                   (* ********* *********  **** **** ****** *)
CONST
"&&"  BLANKID: ALPHA = '            ';
"&&"  CONSTBEGSYS: SETOFSYS =
"&&"      (/ADDOP,INTCONST,REALCONST,STRINGCONST,IDENT/);
"&&"  SIMPTYPEBEGSYS: SETOFSYS =
"&&"      (/ADDOP,INTCONST,REALCONST,STRINGCONST,IDENT,LPARENT/);
"&&"  TYPEBEGSYS: SETOFSYS =
"&&"      (/ARROW,PACKEDSY,ARRAYSY,RECORDSY,SETSY,FILESY,ADDOP,INTCONST,
"&&"        REALCONST,STRINGCONST,IDENT,LPARENT/);
"&&"  TYPEDELS: SETOFSYS =
"&&"      (/ARRAYSY,RECORDSY,SETSY,FILESY,PACKEDSY/);
"&&"  BLOCKBEGSYS: SETOFSYS =
"&&"      (/LABELSY,CONSTSY,TYPESY,VARSY,PROCSY,FUNCSY,BEGINSY/);
"&&"  SELECTSYS: SETOFSYS =
"&&"      (/ARROW,PERIOD,LBRACK,LPARENT/);
"&&"  FACBEGSYS: SETOFSYS =
"&&"      (/INTCONST,REALCONST,STRINGCONST,IDENT,LPARENT,LBRACK,NOTSY/);
"&&"  STATBEGSYS: SETOFSYS =
"&&"      (/BEGINSY,GOTOSY,IFSY,WHILESY,REPEATSY,FORSY,WITHSY,CASESY/);
"&&"  RW: ARRAY(/1..NRSW/) OF ALPHA =
"&&"       ('IF          ', 'DO          ', 'OF          ', 'TO          ',
"&&"        'IN          ', 'OR          ', 'END         ', 'FOR         ',
"&&"        'VAR         ', 'DIV         ', 'MOD         ', 'SET         ',
"&&"        'AND         ', 'NOT         ', 'THEN        ', 'ELSE        ',
"&&"        'WITH        ', 'GOTO        ', 'CASE        ', 'TYPE        ',
"&&"        'FILE        ', 'BEGIN       ', 'UNTIL       ', 'WHILE       ',
"&&"        'ARRAY       ', 'CONST       ', 'LABEL       ', 'REPEAT      ',
"&&"        'RECORD      ', 'DOWNTO      ', 'PACKED      ', 'FORWARD     ',
"&&"        'PROGRAM     ', 'FORTRAN     ', 'EXTERNAL    ', 'FUNCTION    ',
"&&"        'PROCEDURE   ', 'OTHERWISE   ' );
"&&"  FRW: ARRAY(/1..14/) OF 1..NRSW1 =
"&&"       (1, 1, 7, 15, 22, 28, 32, 35, 37, 39, 39, 39, 39, 39);
"&&"  RSY: ARRAY(/1..NRSW/) OF SYMBOL =
"&&"       (IFSY, DOSY, OFSY, TOSY, RELOP, ADDOP, ENDSY, FORSY,
"&&"        VARSY, MULOP, MULOP, SETSY, MULOP, NOTSY, THENSY, ELSESY,
"&&"        WITHSY, GOTOSY, CASESY, TYPESY, FILESY, BEGINSY, UNTILSY, WHILESY,
"&&"        ARRAYSY, CONSTSY, LABELSY, REPEATSY,
"&&"        RECORDSY, DOWNTOSY, PACKEDSY, FORWARDSY,
"&&"        PROGSY, FRTRNSY, EXTRNSY, FUNCSY, PROCSY, OTHERWISESY);
"&&"  ROP: ARRAY(/1..NRSW/) OF OPERATOR =
"&&"       (NOOP, NOOP, NOOP, NOOP, INOP, OROP, NOOP, NOOP,
"&&"        NOOP, IDIV, IMOD, NOOP, ANDOP, NOOP, NOOP, NOOP,
"&&"        NOOP, NOOP, NOOP, NOOP, NOOP, NOOP, NOOP, NOOP,
"&&"        NOOP, NOOP, NOOP, NOOP, NOOP, NOOP, NOOP, NOOP,
"&&"        NOOP, NOOP, NOOP, NOOP, NOOP, NOOP);
"&&"  MN: ARRAY(/0..OPMAX/) OF ARRAY(/1..4/) OF CHAR =
"&&"       (' ABI', ' ABR', ' ADI', ' ADR', ' AND', ' DIF', ' DVI', ' DVR',
"&&"        ' SBR', ' FLO', ' FLT', ' INN', ' INT', ' IOR', ' MOD', ' MPI',
"&&"        ' MPR', ' NGI', ' NGR', ' NOT', ' ODD', ' SBI', ' DEC', ' INC',
"&&"        ' SQI', ' SQR', ' STO', ' TRC', ' RND', ' SCL', ' CSP', ' UNI',
"&&"        ' ENT', ' FJP', ' POP', ' IND', ' IXA', ' LCA', ' CTS', ' CTI',
"&&"        ' MOV', ' MST', ' RET', ' STP', ' XJP', ' CHK', ' CUP', ' EQU',
"&&"        ' GEQ', ' GRT', ' LDA', ' LDC', ' LEQ', ' LES', ' LOD', ' NEQ',
"&&"        ' STR', ' UJP', ' NEW', ' SAV', ' RST', ' ORD', ' CHR', ' DEF',
"&&"        ' LAB', ' CRD', ' XPO', ' ASE', ' SLD', ' SMV', ' DFC', ' CST',
"&&"        ' BGN', ' UXJ', ' XLB', ' END', ' PAK' );
"&&"  SNA: ARRAY(/0..NSPROC/) OF ARRAY(/1..3/) OF CHAR =
"&&"       ('PAG', 'GET', 'PUT', 'RES', 'REW', 'RDC', 'WRI', 'WRE',
"&&"        'WRR', 'WRC', 'WRS', 'PAK', 'RDB', 'WRB', 'RDR', 'RDH',
"&&"        'RDY', 'EOL', 'EOT', 'RDD', 'WRD', 'CLK', 'WLN', 'RLN',
"&&"        'RDI', 'EOF', 'ELN', 'RDS', 'TRP', 'XIT', 'FDF', 'SIO',
"&&"        'EIO', 'MSG', 'SKP', 'LIM', 'TRA');

(*----------------------------------------------------------------------------*)



  PROCEDURE ERROR(FERRNR: ERRCODE);
  VAR  I : 0..10 (*MAXERRNR DIV SETMAX*) ;
  BEGIN
"&&" IF (ERRKIND <> 'W') OR WARNING THEN
"&&"   BEGIN
        IF ERRINX >= 9 THEN
"&&"      BEGIN  FERRNR := 255;  ERRINX := 10  END
"&&"    ELSE ERRINX := ERRINX + 1;
"&&"    WITH ERRLIST(/ERRINX/) DO
"&&"      BEGIN  KIND := ERRKIND;  NMR := FERRNR;  POS := CHCNT  END;
#       I := FERRNR DIV (SSETMAX+1) ;
#       ERRLOG[I] := ERRLOG[I]+ [FERRNR MOD (SSETMAX+1)] ;
"&&"    IF ERRKIND <> 'W' THEN ERRORCNT := ERRORCNT + 1
"&&"    ELSE                   WARNCNT  := WARNCNT  + 1;
"&&"  END;
"&&"  ERRKIND := 'E';
  END (*ERROR*) ;


PROCEDURE PRINTLINE ;
VAR  DCN: ADDRRANGE;
# BEGIN
"&&"   IF PLCNT >= PAGESIZE THEN
"&&"   BEGIN
#       PAGECNT := PAGECNT+1 ;  PLCNT := 0 ;
#       WRITELN(OUTPUT, '1  LINE #  D/NEST  LVL',
#                   '< STANFORD PASCAL,   MCGILL VERSION OF ':44,
#                   VERSION, ' >', TIME:14, DATE, 'PAGE':8, PAGECNT:4) ;
#       WRITELN(OUTPUT, '------  ------  ---':22,
#                   '---- ---':89) ;
#       WRITELN(OUTPUT) ;
"&&"  END;
"&&"  IF LINECNT > LASTLINELISTED THEN
"&&"    BEGIN  LASTLINELISTED := LINECNT;
"&&"      PLCNT := PLCNT + 1;
"&&"      WRITE(OUTPUT, LINECNT:9);
"&&"      IF LISTTAG = 'N' THEN DCN := STMTNEST ELSE
"&&"        IF LISTTAG = 'D' THEN DCN := LC       ELSE
"&&"          IF LISTTAG = 'C' THEN DCN := CONSTLC  ELSE DCN := 0;
"&&"      IF DCN > 0 THEN WRITE(OUTPUT, DCN:7, LISTTAG, LEVEL:3 )
"&&"      ELSE            WRITE(OUTPUT, ' ':11 );
"&&"      WRITELN(OUTPUT, ') ', LINEBUF:LINELEN );
"&&"    END;
# END (*PRINTLINE*) ;


PROCEDURE PRINTERROR ;
"&&" VAR LASTPOS,FREEPOS,CURRPOS,CURRNMR,F,K,DCN: INTEGER;
"&&"     CURRKIND,LASTKIND: CHAR;
# BEGIN
"&&" PRINTLINE;
#   PLCNT := PLCNT+2;  (* TWO LINES OF ERROR INFO *)
#   WRITE(OUTPUT,'****':9, ' ':13) ;
"&&" LASTPOS := 0;  FREEPOS := 1;  LASTKIND := '?';
#   FOR K := 1 TO ERRINX DO
#     BEGIN
#       WITH ERRLIST[K] DO
"&&"      BEGIN  CURRPOS := POS;  CURRNMR := NMR;  CURRKIND := KIND  END;
#       IF CURRPOS = LASTPOS THEN
"&&"      IF CURRKIND = LASTKIND THEN WRITE(OUTPUT,',')
"&&"      ELSE                        WRITE(OUTPUT,CURRKIND)
#       ELSE
"&&"      BEGIN  LASTKIND := CURRKIND;
#           WHILE FREEPOS < CURRPOS DO
#             BEGIN WRITE(OUTPUT,' '); FREEPOS := FREEPOS + 1 END;
"&&"        WRITE(OUTPUT,CURRKIND);
#           LASTPOS := CURRPOS
#         END;
#       IF CURRNMR < 10 THEN F := 1
#       ELSE IF CURRNMR < 100 THEN F := 2
#         ELSE F := 3;
#       WRITE(OUTPUT,CURRNMR:F);
#       FREEPOS := FREEPOS + F + 1
#     END;
"&&" WRITELN(OUTPUT);  ERRINX := 0 ;  IF ERRORCNT>0 THEN PRCODE := FALSE ;
#   IF ERRLN > 0 THEN
#     WRITELN(OUTPUT,'****':9,'  PREVIOUS ERROR/WARNING ON LINE -->', ERRLN:4);
#   ERRLN := LINECNT;
# END (*PRINTERROR*) ;


PROCEDURE ENDOFLINE ;
# LABEL 10;
"&&" VAR I: 1..9;  DCN: INTEGER;
  BEGIN   IF ERRINX > 0 THEN PRINTERROR ;
#   READLN(INPUT, LINEBUF);
    LINELEN := BUFEND;   (*THIS WILL SPEED THINGS UP IF NO MARGIN IS SET/RESET*)

    (*$D-  ... MUST BE IN EFFECT FOR THIS LOOP*)
      REPEAT  LINELEN := LINELEN - 1;
      UNTIL   LINEBUF[LINELEN] <> ' ';
    (* if needed, debug switch should be restored here ---> $D+*)
10:
    IF LINELEN > RMARGIN THEN  BEGIN  MWARN := TRUE;  LASTCOL := RMARGIN END
    ELSE LASTCOL := LINELEN;
#   LINECNT := LINECNT+1 ;
"&&" IF LIST THEN PRINTLINE;
"&&" IF HP THEN BEGIN  IC := 0;  LISTTAG := ' ';  HP := FALSE END;
#   LINEBUF[LASTCOL+1] := '#';    (*TO STOP 'SKIPBLNK' + PROVIDE VALID EOL CH.*)
#   CHCNT := LMARGIN;
  END  (*ENDOFLINE*) ;


# PROCEDURE LISTMSGS ;
#   VAR I, J : ERRCODE ;
#       MSG  : ARRAY[1..64] OF CHAR ;
#   BEGIN
#      WRITELN(OUTPUT);
#      IF ERRLN > 0 THEN
#        BEGIN  WRITELN(OUTPUT, '****':9,
"&&"              '  LAST ERROR/WARNING ON LINE -->', ERRLN:4 );
#               WRITELN(OUTPUT);
#        END;
#      WRITELN(OUTPUT);
"&&"   WRITELN(OUTPUT, '****':9, '  ERROR/WARNING CODES FOR THIS PROGRAM :') ;
#      WRITELN(OUTPUT) ;   RESET(PRD) ;   J := 0 ;
#      FOR I := 1 TO MAXERRNR DO
#        IF (I MOD (SSETMAX+1)) IN ERRLOG[I DIV (SSETMAX+1)] THEN
#           BEGIN
#           WHILE (NOT EOF(PRD)) AND (I > J) DO  READLN(PRD, J, MSG) ;
#           IF J = I THEN  WRITELN('****':9, J:6, MSG) ;
#           END ;
#   END (*LISTMSGS*);


# PROCEDURE GOODBYE;
#   BEGIN
#    CTIME := (CLOCK(0)-CTIME);
"&&" IF PAGECNT = 0 THEN  (* NO HEADING EVER PRINTED, DO ONE NOW *)
"&&"   WRITELN(OUTPUT, '****':9,
"&&"     'STANFORD PASCAL COMPILER, MCGILL VERSION OF ':50, VERSION);
"&&" IF NOT MUSIC THEN WRITELN(OUTPUT);
"&&" IF WARNING THEN
"&&"   BEGIN
"&&"     IF EXTUSED THEN
"&&"     WRITELN(OUTPUT, '0', '****':8, '  WARNING: PASCAL EXTENSIONS USED.');
"&&"     IF WARNCNT > 0 THEN WRITELN(OUTPUT, '0', '****':8,
"&&"          WARNCNT:8, '  WARNING MESSAGE(S) ISSUED.' );
#       IF MWARN THEN
#      WRITELN(OUTPUT, '0', '****':8, '  CONTENTS OF SOURCE LINES OUTSIDE  ',
#                      LMARGIN:1, '..', RMARGIN:1, '  MARGINS IGNORED.');
"&&"  END (* IF WARNING *) ;
"&&" IF NOT MUSIC THEN WRITELN(OUTPUT);
#    IF ERRORCNT = 0 THEN  WRITE(OUTPUT, '****      NO':17)
#    ELSE  WRITE(OUTPUT, '****':9, ERRORCNT:8) ;
#    WRITELN(OUTPUT, '  SYNTAX ERROR(S) DETECTED.');
"&&" IF NOT MUSIC THEN WRITELN(OUTPUT);
#    WRITELN(OUTPUT, '****':9, LINECNT:8, '  LINE(S) READ, ', PROCLAB:4,
#                    ' PROCEDURE(S) COMPILED,');
"&&" IF NOT MUSIC THEN WRITELN(OUTPUT);
#    WRITELN(OUTPUT, '****':9, OLDIC:8,'  P_INSTRUCTIONS GENERATED,',
#                    CTIME*0.001:7:2, ' SECONDS IN COMPILATION.') ;
#    IF ERRORCNT > 0 THEN  LISTMSGS ;
#    EXIT(ERRORCNT) ;
#   END (*GOODBYE*) ;


#   PROCEDURE FATALERROR( CODE: ERRCODE );
#     BEGIN
#     ERROR(CODE);
#     PRINTERROR;
#     IF CODE <> 390 THEN
#        WRITELN(OUTPUT, '0    ****   FATAL ERROR - ',
#                'COMPILATION TERMINATED PREMATURELY.' );
#     GOODBYE;
#     END (*EOFEXIT*) ;


  PROCEDURE INSYMBOL;
    (*READ NEXT BASIC SYMBOL OF SOURCE PROGRAM AND RETURN ITS
    DESCRIPTION IN THE GLOBAL VARIABLES SY, OP, ID, VAL AND LNGTH*)
    LABEL 1,2,3;
"&&"VAR I,K,STATE: INTEGER;
        DIGIT: PACKED ARRAY [1..REALLNGTH] OF CHAR;
        STRING: PACKED ARRAY [1..STRGLNGTH] OF CHAR;
"&&"    LVP: CSP;  TEST,DIGSEEN: BOOLEAN;


#   PROCEDURE SKIPBLNK;
#   (* SKIP BLANKS, ENDOFLINE, AND (OPTIONAL) MARGIN, SKIPS AT LEAST ONE CHAR *)
#
#     BEGIN
#       REPEAT
#
#       IF EOL THEN
#         BEGIN
#         IF EOF(INPUT)  THEN  FATALERROR( 390 ) ;
#         ENDOFLINE ;
#         END ;
#
#        REPEAT CHCNT := CHCNT+1;  UNTIL LINEBUF[CHCNT] <> ' ';
#        (* NOTE THAT LINEBUF[LINELEN+1] <> ' ' *)
#        EOL := CHCNT > LASTCOL;
#        UNTIL NOT EOL ;
#      CH := LINEBUF[CHCNT] ;
#    END (*SKIPBLNK*) ;


    PROCEDURE NEXTCH;
#     BEGIN
#     IF EOL THEN
#        BEGIN
#        IF EOF(INPUT)  THEN  FATALERROR( 390 ) ;
#        ENDOFLINE ;
#        END ;
#     CHCNT := CHCNT+1;  EOL := (CHCNT > LASTCOL);
#     CH := LINEBUF[CHCNT] ;
#     END;


#     PROCEDURE OPTIONS(CCH: CHAR (*COMMENT TERMINATOR CH*) );
"&&"     VAR SCH: CHAR;  OLDLIST: BOOLEAN;
#
#     FUNCTION DECNUM : INTEGER;
#       VAR NUM: INTEGER;
#       BEGIN
#          NUM := 0;  NEXTCH;
#          WHILE CH >= '0' DO
#             BEGIN  NUM := NUM*10+ORD(CH)-ORD('0');  NEXTCH  END;
#          DECNUM := NUM
#       END;
#
#     BEGIN
#       REPEAT  NEXTCH;
"&&"       IF CH IN ['a'..'z', 'A'..'Z'] THEN
"&&"         BEGIN
"&&"           SCH := UPSHIFT[CH];  NEXTCH;
"&&"           CASE SCH OF
"&&"           'L': BEGIN
"&&"                  OLDLIST := LIST;  LIST := CH <> '-';
"&&"                  IF NOT OLDLIST THEN IF LIST THEN PRINTLINE;
"&&"                END;
"&&"           'C': PRCODE := CH <> '-';
"&&"           'E': IF LIST THEN PLCNT := PAGESIZE;
"&&"           'A': ASSEMBLE := CH = '+';
"&&"           'M': BEGIN
                      IF CH = '+' THEN
                         BEGIN  LMARGIN := 0;  RMARGIN := 72;  END
                      ELSE IF CH = '-' THEN
                         BEGIN  LMARGIN := 0;  BUFEND := BUFLEN;
                                RMARGIN := BUFLEN;  END
                      ELSE IF CH = '(' THEN
                         BEGIN  LMARGIN := DECNUM - 1;
                           IF LMARGIN < 0 THEN LMARGIN := 0;
                           IF CH = ',' THEN  RMARGIN := DECNUM
                                       ELSE  RMARGIN := BUFLEN;
                           IF (RMARGIN <= LMARGIN) OR
                              (RMARGIN >= BUFLEN) THEN
                                 RMARGIN := BUFLEN-1;
                           BUFEND := BUFLEN;
                         END
                   END;
"&&"           'S': SAVEREGS := CH <> '-';
"&&"           'F': SAVEFPRS := CH <> '-';
"&&"           'D': IF CH >= '0' THEN
"&&"                  BEGIN  DEBUG_LEV := ORD(CH) - ORD('0');
"&&"                         DEBUG := DEBUG_LEV >= 2;
"&&"                  END
"&&"                ELSE
"&&"                  BEGIN  DEBUG := CH <> '-';
"&&"                         DEBUG_LEV := ORD(DEBUG)*2;
"&&"                  END;
"&&"           'V': ASMVERB := CH = '+';
"&&"           'W': WARNING := CH <> '-';
"&&"           'U': GET_STAT := CH = '+';
"&&"           'P': NOPACKING := CH = '-';
"&&"           'X': BEGIN
#                     XLINK := CH = '+';
#                     (* XLINK --> ALLOW '$' AS FIRST CH.*)
#                     IF XLINK THEN  SOP['$'] := ATOZCH;
#                   END;
"&&"           'K': CTROPTION := CH = '+';
"&&"           'N': NESTCOMM := CH = '+';
"&&"           'Z': MUSIC := CH = '+';
"&&"          END;
#             IF CH <> CCH THEN
"&&"             IF CH <> ',' THEN  NEXTCH;
#           END
#       UNTIL CH <> ','
#     END (*OPTIONS*) ;


#   PROCEDURE COMMENT(CCH: CHAR (*COMMENT DELIMITER CHARACTER*) );
#
#     BEGIN (*COMMENT*)
#
#       REPEAT
#
#       IF CH <> CCH THEN
#         REPEAT
#         IF NESTCOMM THEN
#           BEGIN
"DE" #      IF CH = CHLBRACE THEN
"DE" #         IF CCH = CHRBRACE THEN  BEGIN  EOL := FALSE;
"DE" #         COMMENT(CHRBRACE)  END;
#           IF CH = '(' THEN
#              IF LINEBUF[CHCNT+1] = '*' THEN
#                 IF CCH = '*' THEN  (*TO PREVENT
                        LEFTPAREN RIGHTBRACE FROM GOING THROUGH*)
#                    BEGIN  EOL := FALSE;  NEXTCH;  NEXTCH;  COMMENT('*')  END;
#           END;
#
#         IF CHCNT > LASTCOL THEN
#           BEGIN  IF EOF(INPUT) THEN FATALERROR( 390 ) ;
#           ENDOFLINE ;
#           END ;
#
#         CHCNT := CHCNT+1 ;
#         CH := LINEBUF[CHCNT] ;
#         UNTIL CH = CCH ;
#
#       IF CCH = '*' THEN
#         BEGIN
#         CHCNT := CHCNT+1 ;
#         CH := LINEBUF[CHCNT] ;
#         END
#       ELSE CH := ')' (*CHEATING A BIT*);
#
#       UNTIL CH = ')' ;
#
#     EOL := FALSE;    (*PREPARE FOR NEXT CALL TO 'NEXTCH'*)
#     END (*COMMENT*) ;
#
#

  BEGIN (*INSYMBOL*)
  1:
#   IF CH = ' ' THEN SKIPBLNK ;
#   CASE SOP[CH] OF
#     ATOZCH :
#       BEGIN   K := 0 ;   ID := BLANKID ;
#         REPEAT
#         IF K < IDLNGTH THEN
#           BEGIN K := K + 1; ID[K] := UPSHIFT[CH] (*CH*) END ;        (*UPL*)
#         NEXTCH
#         UNTIL NOT(SOP[CH] IN [ATOZCH, NUMCH, UNDSCH, DOLLARCH]) ;
#
          FOR I := FRW[K] TO FRW[K+1] - 1 DO
            IF RW[I] = ID THEN
              BEGIN SY := RSY[I]; OP := ROP[I]; GOTO 2 END;
            SY := IDENT; OP := NOOP;
  2:    END;

#     NUMCH :
"&&"    BEGIN OP := NOOP; K := 0;  SY := REALCONST;  STATE := 0;
"&&"      REPEAT
"&&"        DIGSEEN := FALSE;
"&&" 3:     K := K + 1;  IF K <= DIGMAX THEN DIGIT[K] := CH;
"&&"        NEXTCH;
"&&"        IF SOP[CH] = NUMCH THEN
"&&"          BEGIN  DIGSEEN := TRUE;  GOTO 3  END;
"&&"        IF DIGSEEN THEN STATE := STATE + 1;
"&&"        CASE STATE OF
"&&"0,1:      (* LEADING DIGIT SEQUENCE SCANNED *)
"&&"          IF CH = '.' THEN            STATE := 2
"&&"          ELSE IF CH = 'E' THEN       STATE := 4
"&&"            ELSE IF CH = 'e' THEN     STATE := 4
"&&"              ELSE BEGIN  SY := INTCONST;  STATE := 0  END;
"&&"2:        (* DECIMAL POINT JUST SCANNED *)
"&&"          IF CH = '.' THEN
"&&"            BEGIN  SY := INTCONST;  K := K - 1;
"&&"                   DOTFLG := TRUE;  STATE := 0   END
"&&"          ELSE IF UPSHIFT[CH] = 'E' THEN
"&&"              BEGIN  K := K - 1;  STATE := 4  END
"&&"            ELSE  STATE := -1;
"SY"3:        (* DIGIT SEQUENCE AFTER POINT JUST SEEN *)
"&&"          IF CH = 'E' THEN      STATE := 4
"&&"          ELSE IF CH = 'e' THEN STATE := 4
"&&"            ELSE STATE := 0;
"&&"4:        (* EXPONENT SYMBOL JUST SEEN *)
"&&"          IF CH = '-' THEN       STATE := 6
"&&"          ELSE IF CH = '+' THEN  STATE := 6
"&&"            ELSE  STATE := -1;
"&&"5,7:      (* DIGIT STRING IN EXPONENT JUST SEEN *)
"&&"          STATE := 0;
"&&"6:        (* BAD CHARACTER AFTER 'E+' OR 'E-' FOUND *)
"&&"          STATE := -1;
"&&"        END;
"&&"      UNTIL STATE <= 0;
"&&"      IF STATE < 0 THEN  (* LEXICAL ERROR IN REAL CONST *)
"&&"        ERROR(201);
"&&"      IF SY = REALCONST THEN
"&&"        BEGIN
"&&"          NEW( VAL.VALP, REEL );
"&&"          WITH VAL.VALP@ DO
"&&"            BEGIN FOR I := 1 TO REALLNGTH DO RVAL[I] := ' ';
"&&"              IF K <= DIGMAX THEN
"&&"                FOR I := 2 TO K + 1 DO RVAL[I] := DIGIT[I-1]
"&&"              ELSE BEGIN ERROR(203);
"&&"                     UNPACK('0.0',RVAL,2)
"&&"                   END
"&&"            END;
"&&"        END
"&&"      ELSE
"&&"        BEGIN
"&&"          VAL.IVAL := 0;
"&&"          IF K> DIGMAX THEN
"&&"            ERROR(203)
"&&"          ELSE WITH VAL DO
"&&"              FOR I := 1 TO K DO
"&&"                IF IVAL <= MXINT10 THEN
"&&"                  IVAL := IVAL*10 + (ORD(DIGIT[I])-ORD('0'))
"&&"                ELSE BEGIN ERROR(203); IVAL := 0 END
"&&"        END
        END;

#     QUOTCH   "''''  " :
        BEGIN LNGTH := 0; SY := STRINGCONST;  OP := NOOP;
          REPEAT
"&&"        REPEAT NEXTCH;
"&&"               IF EOL THEN BEGIN ERROR(202); CH := '''' END;
"&&"               LNGTH := LNGTH + 1;
                   IF LNGTH <= STRGLNGTH THEN STRING[LNGTH] := CH
"&&"        UNTIL CH = '''';
"&&"      NEXTCH
          UNTIL CH <> '''';
          LNGTH := LNGTH - 1;   (*NOW LNGTH = NR OF CHARS IN STRING*)
          IF LNGTH = 1 THEN VAL.IVAL := ORD(STRING[1])
          ELSE
            BEGIN   NEW(LVP,STRG);
              IF LNGTH > STRGLNGTH THEN
                BEGIN ERROR(398); LNGTH := STRGLNGTH END;
#             IF LNGTH <= 0 THEN ERROR(205) ;
              WITH  LVP@  DO
                BEGIN SLNGTH := LNGTH;
"&&"              SVAL := STRING
                END;
              VAL.VALP := LVP
            END
        END;

#     COLONCH   "':'  ":
        BEGIN OP := NOOP; NEXTCH;
          IF CH = '=' THEN
            BEGIN SY := BECOMES; NEXTCH END
          ELSE SY := COLON
        END;

#     DOTCH   "'.'  " :
        BEGIN  OP := NOOP;  IF NOT DOTFLG THEN NEXTCH;
          IF CH = '.' THEN
            BEGIN  SY := DOTDOT;  DOTFLG := FALSE ;  NEXTCH  END
          ELSE SY := PERIOD
        END;

#     LTOP   "'<'  " :
        BEGIN NEXTCH; SY := RELOP;
          IF CH = '=' THEN
            BEGIN OP := LEOP; NEXTCH END
          ELSE
            IF CH = '>' THEN
              BEGIN OP := NEOP; NEXTCH END
            ELSE OP := LTOP
        END;

#     GTOP   "'>'  ":
        BEGIN NEXTCH; SY := RELOP;
          IF CH = '=' THEN
            BEGIN OP := GEOP; NEXTCH END
          ELSE OP := GTOP
        END;

#     LPARCH   "'('  ":
       BEGIN NEXTCH;
#        IF CH = '*' THEN
            BEGIN  NEXTCH;
            IF CH = '$' THEN OPTIONS('*');
            COMMENT('*');  NEXTCH;  GOTO 1;
            END ;
#        IF CH = '/' THEN
#          BEGIN   SY := LBRACK ;  OP := NOOP ;
#          NEXTCH
#          END
         ELSE  BEGIN  SY := LPARENT; OP := NOOP  END
       END;

#     PLUS, MINUS, MUL, RDIV, EQOP, OROP, ANDOP, SPECH :
        BEGIN SY := SSY[CH]; OP := SOP[CH];
#         IF CH = '/' THEN
#           BEGIN  NEXTCH ;
#             IF CH =')' THEN
#               BEGIN  SY := RBRACK ;  OP := NOOP ;
#                 NEXTCH ;
#               END
#           END
#         ELSE  NEXTCH
        END;

#     DQUOTCH (* '"' *) :
#       BEGIN   REPEAT NEXTCH UNTIL CH = '"' ;
#         NEXTCH ;   GOTO 1 ;
#       END ;

#     LBRACE :
#       BEGIN  NEXTCH;
"DE"    IF CH = '$' THEN OPTIONS(CHRBRACE);
"DE"    COMMENT(CHRBRACE);  NEXTCH;  GOTO 1;
        END;

#     SKIPCH   "'#'  " :
#       BEGIN  NEXTCH ;  GOTO 1  END ;
#
#     ILLEGCH, DOLLARCH, UNDSCH   "'¢','_','$'  ":
#       BEGIN SY := OTHERSY; OP := NOOP; ERROR(6) ; NEXTCH END

    END (*CASE*)

  END (*INSYMBOL*) ;


"&&"  FUNCTION HASH( ID: ALPHA ): BKT_RNG;
"&&"    VAR  OL: RECORD  CASE INTEGER OF
"&&"               1:  ( IDK: ALPHA );
"&&"               2:  ( INT1, INT2, INT3: INTEGER )
"&&"             END;
"&&"    BEGIN
"&&"      WITH OL DO
"&&"        BEGIN  IDK := ID;  (* NO OVERFLOW CHECK FOR NEXT STMT *)
"&&"          HASH := ABS( (INT1*2 + INT2)*2 + INT3 ) MOD (MAX_BKT+1);
"&&"        END
"&&"    END  (* HASH *) ;


"&&"  PROCEDURE ENTERID( FCP: CTP );
"&&"  LABEL 1;
"&&"  VAR  K: BKT_RNG;  NAM: ALPHA;  LCP: CTP;
"&&"  BEGIN
"&&"    NAM := FCP@.NAME;  K := HASH( NAM );
"&&"    LCP := BUCKET[K];
"&&"    FCP@.DECL_LEV := LEVEL;
"&&"    FCP@.NEXT_IN_BKT := LCP;  BUCKET[K] := FCP;
"&&"    (* NOW CHECK FOR DUPLICATE DECLARATION *)
"&&"    WHILE LCP <> NIL DO
"&&"      WITH LCP@ DO
"&&"        BEGIN
"&&"          IF NAME = NAM THEN
"&&"            IF KLASS <> FIELD THEN
"&&"              BEGIN
"&&"                IF TOP = DECL_LEV THEN
"&&"                  BEGIN  ERROR(101);  GOTO 1  END
"&&"              END
"&&"            ELSE  (* SPECIAL LOOKUP FOR FIELDS *)
"&&"              IF TOP = OWNER@.FLD_DISP_LEV THEN
"&&"                BEGIN  ERROR(101);  GOTO 1  END;
"&&"          LCP := NEXT_IN_BKT;
"&&"        END;
"&&" 1:
"SY" IF GET_STAT THEN
"SY"   IF TOP = LEVEL THEN ENT_CNT[LEVEL] := ENT_CNT[LEVEL] + 1
"SY"   ELSE                FENT_CNT       := FENT_CNT       + 1;
"&&"  END  (* ENTERID *) ;


"&&"  PROCEDURE SEARCHSECTION( FSP: STP; VAR FCP: CTP );
"&&"  (* FINDS FIELD IN RECORD STRUCTURE INDICATED BY FSP *)
"&&"  LABEL 1;
"&&"  VAR  LCP: CTP;
"&&"  BEGIN
"&&"    LCP := BUCKET[ HASH(ID) ];
"&&"    WHILE LCP <> NIL DO
"&&"      WITH LCP@ DO
"&&"        BEGIN
"&&"          IF NAME = ID THEN
"&&"            IF KLASS = FIELD THEN
"&&"              IF OWNER = FSP THEN
"SY"              BEGIN
"SY"                IF GET_STAT THEN
"SY"                  BEGIN  SF_CNT := SF_CNT + 1;
"SY"                         SF_TOT := SF_TOT + FSP@.NO_FLDS;
"SY"                  END;
"&&"                GOTO 1;
"SY"              END;
"&&"          LCP := NEXT_IN_BKT;
"&&"        END;
"&&" 1:  FCP := LCP;
"&&"  END  (* SEARCHSECTION *) ;


"&&"  PROCEDURE SEARCHID( FIDCLS: SETOFIDS; VAR FCP: CTP );
"&&"  LABEL 1;
"&&"  VAR  LCP: CTP;  DL,EL: -1..DISPLIMIT;  K: BKT_RNG;
"&&"  BEGIN
"&&"    K := HASH(ID);  LCP := BUCKET[ K ];
"&&"    FCP := NIL;  EL := -1;  DISX := EL;
"&&"    WHILE LCP <> NIL DO
"&&"      WITH LCP@ DO
"&&"        BEGIN
"&&"          IF NAME = ID THEN
"&&"            BEGIN
"&&"              IF KLASS <> FIELD THEN
"&&"                DL := DECL_LEV
"&&"              ELSE
"&&"                DL := OWNER@.FLD_DISP_LEV;
"&&"              IF DL > DISX THEN
"&&"                IF KLASS IN FIDCLS THEN
"&&"                  BEGIN
"&&"                    FCP := LCP;  DISX := DL;
"&&"                    IF TOP = LEVEL THEN  (* NO POINT IN FURTHER SEARCH *)
"&&"                      GOTO 1
"&&"                  END
"&&"                ELSE
"&&"                  EL := DL;
"&&"            END;
"&&"          LCP := NEXT_IN_BKT;
"&&"        END;
"&&"  1:IF EL > DISX THEN  (* BAD IDENTIFIER ENCOUNTERED *)
"&&"      IF PRTERR THEN ERROR(103);
"&&"    IF DISX < 0 THEN
"&&"      BEGIN  (* THE SEARCH WAS UNSUCCESSFUL *)
"&&"        IF PRTERR THEN
"&&"          BEGIN  IF EL < 0 THEN ERROR(104);
"&&"            NEW( LCP );
"&&"            IF (FIELD IN FIDCLS) AND (TOP > LEVEL) THEN
"&&"              LCP@ := UFLDPTR@
"&&"            ELSE IF VARS IN FIDCLS THEN LCP@ := UVARPTR@
"&&"              ELSE IF TYPES IN FIDCLS THEN LCP@ := UTYPPTR@
"&&"                ELSE IF KONST IN FIDCLS THEN LCP@ := UCSTPTR@
"&&"                  ELSE IF PROC IN FIDCLS THEN LCP@ := UPRCPTR@
"&&"                    ELSE  (* FUNC *)          LCP@ := UFCTPTR@;
"&&"            WITH LCP@ DO
"&&"              BEGIN
"&&"                NAME := ID;  (* PREVENT RE-OCCURRENCE OF ERROR *)
"&&"                DECL_LEV := LEVEL;
"&&"                NEXT_IN_BKT := BUCKET(/K/);
"&&"                BUCKET(/K/) := LCP;  FCP := LCP;
"&&"                IF KLASS = FIELD THEN OWNER := OPEN_RECORD;
"&&"              END;
"&&"           DISX := LEVEL;
"&&"          END
"&&"        ELSE DISX := 0;
"&&"      END;
"SY" IF GET_STAT THEN
"SY"   BEGIN
"SY"     IF DISX <= LEVEL THEN
"SY"       LU_CNT[DISX,TOP] := LU_CNT[DISX,TOP] + 1
"SY"     ELSE
"SY"       WLU_CNT[DISX-LEVEL,TOP-LEVEL] := WLU_CNT[DISX-LEVEL,TOP-LEVEL]+1;
"SY"     IF TOP <> LEVEL THEN WE_CNT := WE_CNT + 1
"SY"   END;
"&&"  END  (* SEARCHID *) ;


  PROCEDURE GETBOUNDS(FSP: STP; VAR FMIN,FMAX: INTEGER);
    (*GET INTERNAL BOUNDS OF SUBRANGE OR SCALAR TYPE*)
    (*ASSUME (FSP <> NIL) AND (FSP@.FORM <= SUBRANGE) AND (FSP <> INTPTR)
     AND NOT COMPTYPES(REALPTR,FSP)*)
  BEGIN
    WITH FSP@ DO
      IF FORM = SUBRANGE THEN
        BEGIN FMIN := MIN.IVAL; FMAX := MAX.IVAL END
      ELSE
        BEGIN FMIN := 0;
#         IF FSP = CHARPTR THEN FMAX := ORDCHMAX
          ELSE
            IF (FORM = SCALAR) AND (FSP@.FCONST <> NIL) THEN
              FMAX := FSP@.FCONST@.VALUES.IVAL
            ELSE FMAX := 0
        END
  END (*GETBOUNDS*) ;

#     FUNCTION GETTYPE(OPERAND: STP): INTEGER;
#       BEGIN   GETTYPE := ORD('I') ;  (* ASSUME INTEGER TYPE *)
#         IF OPERAND = NIL THEN  BEGIN IF ERRORCNT = 0 THEN ERROR(401) END
#         ELSE
#           IF OPERAND@.FORM > POWER THEN GETTYPE := ORD('A')
#           ELSE
#             IF OPERAND@.FORM = POWER THEN GETTYPE := ORD('S')
#             ELSE
#               IF OPERAND@.FORM = POINTER THEN GETTYPE := ORD('A')
#               ELSE
#                   IF OPERAND = REALPTR THEN GETTYPE := ORD('R')
#                   ELSE
#                     IF OPERAND = BOOLPTR THEN GETTYPE := ORD('B')
#                     ELSE
"&&"                    IF OPERAND@.SIZE = HINTSIZE THEN
"&&"                      GETTYPE := ORD('H')
"&&"                    ELSE IF OPERAND@.SIZE = CHARSIZE THEN
"&&"                        GETTYPE := ORD('C')
"&&"                      ELSE ;
#       END (*GETTYPE*) ;


"**" FUNCTION OPNDSETSIZE(FATTR: ATTR): INTEGER;
"**"    (* COMPUTES THE SIZE OF A SET USED AS AN OPERAND *)
"**"    BEGIN  OPNDSETSIZE := 0;
"**"      WITH FATTR DO
"**"        IF TYPTR <> NIL THEN
"**"          CASE KIND OF
"**"          CST:   OPNDSETSIZE := CVAL.VALP@.PLNGTH;
"**"          VARBL: CASE ACCESS OF
"**"                 DRCT, INDRCT: OPNDSETSIZE := TYPTR@.SIZE;
"**"                 STKEXPR:      OPNDSETSIZE := STKLEN;
"**"                 INXD:         ERROR( 400 );
"**"                 END;
"**"          EXPR:  ERROR( 400 );
"**"          END;
"**"   END (* OPNDSETSIZE *) ;

  PROCEDURE GENLABEL(VAR NXTLAB: LABELRNG);
  BEGIN INTLABEL := INTLABEL + 1;
    NXTLAB := INTLABEL
  END (*GENLABEL*);


"E"(*THE FOLLOWING OUTPUTS A SYMBOL TABLE FILE FOR USE BY 'SNAPSHOT' PROGRAM*)
"E"
"E"PROCEDURE PRNTSYMBL(LCP:CTP);
"E"   VAR  LINELN:INTEGER;  (* CURRENT SYMBOL TABLE FILE LINE LENGTH *)
"&&"       TPT1: PRNTTYLISTP;
"E"
"&&"  PROCEDURE CHECKLN( LEN: INTEGER );
"&&"  BEGIN  IF (LINELN+LEN) >= 80 THEN
"&&"           BEGIN  WRITELN(QRR); WRITE(QRR,' '); LINELN := LEN END
"&&"         ELSE LINELN := LINELN + LEN
"&&"  END;
"E"
"&&"  PROCEDURE PRNTVAR(VRP:CTP); FORWARD;
"E"
"&&"  PROCEDURE PRNTTYPE(TYPP:STP);
"&&"    LABEL 1;
"&&"     VAR  VP, LVP: CTP;   RMIN, RMAX: INTEGER;
"&&"          TPT,LPT: PRNTTYLISTP;  TNO: 0..999;
"E"
"E"      BEGIN
"&&"     CHECKLN(4);
"&&"     IF TYPP=INTPTR THEN WRITE(QRR,'I4; ')
"E"         ELSE IF TYPP=REALPTR THEN WRITE(QRR,'R; ')
"E"         ELSE IF TYPP=BOOLPTR THEN WRITE(QRR,'B; ')
"E"         ELSE IF TYPP=CHARPTR THEN WRITE(QRR,'C; ')
"E"         ELSE IF TYPP <> NIL THEN
"&&"          WITH TYPP@ DO
"E"           CASE FORM OF
"E"
"E"           SUBRANGE:IF RANGETYPE = CHARPTR THEN
"&&"                     WRITE(QRR,'C; ')
"&&"                   ELSE IF RANGETYPE = INTPTR THEN
"&&"                     WRITE(QRR,'I',SIZE:1,'; ')
"&&"                   ELSE WRITE(QRR,'L',SIZE:1,'; ');

"&&"          SCALAR:  WRITE(QRR,'L', SIZE:1, '; ');
"E"
"&&"          POINTER: BEGIN
"&&"                   IF ELTYPE <> NIL THEN
"&&"                     BEGIN  TPT := PRNTTYPHD;  LPT := TPT;
"&&"                     WHILE TPT <> NIL DO
"&&"                       IF TPT@.ELT = ELTYPE THEN
"&&"                         BEGIN  TNO := TPT@.TNO;  GOTO 1  END
"&&"                       ELSE
"&&"                         BEGIN  LPT := TPT;  TPT := TPT@.NXT;  END;
"&&"                     NEW(TPT);
"&&"                     IF PRNTTYPHD = NIL THEN
"&&"                       PRNTTYPHD := TPT
"&&"                     ELSE
"&&"                       LPT@.NXT := TPT;
"&&"                     WITH TPT@ DO
"&&"                       BEGIN  NXT := NIL;  ELT := ELTYPE;
"&&"                          PRNTTYNO := PRNTTYNO+1;  TNO := PRNTTYNO  END;
"&&"                     TNO := PRNTTYNO;
"&&"                     END
"&&"                   ELSE  TNO := 0;
"&&"                1: CHECKLN(3);
"&&"                   WRITE(QRR,'P ',TNO:1,'; ');
"&&"                   END;
"E"
"E"           POWER:   IF ELSET <> NIL THEN
"E"                      BEGIN     WRITE(QRR,'S ');
"&&"                     CHECKLN(10);
"E"                      GETBOUNDS(ELSET, RMIN, RMAX) ;
"E"                      WRITE(QRR, RMIN:1,' ', RMAX:1, ' ; ');
"E"                      END;
"E"
"E"           FILES:   BEGIN  WRITE(QRR,'F ');
"&&"                     PRNTTYPE(FILTYPE);
"E"                    END;
"E"
"&&"          RECORDS: BEGIN   WRITE(QRR,'D',ALN:1,'(');
"&&"                   VP := FSTFLD;  LVP := VP;
"&&"                   WHILE VP <> NIL DO BEGIN PRNTVAR(VP);
"&&"                      LVP := VP;  VP := VP@.NEXT;  END;
"&&"                   IF RECVAR <> NIL THEN
"&&"                     BEGIN
"&&"                       IF RECVAR@.TAGFIELDP <> NIL THEN
"&&"                         IF RECVAR@.TAGFIELDP@.NAME <> BLANKID THEN
"&&"                           BEGIN  LVP := RECVAR@.TAGFIELDP;
"&&"                                  PRNTVAR( LVP );
"&&"                           END;
"&&"                       IF LVP <> NIL THEN
"&&"                         BEGIN  CHECKLN( 12 );
"&&"                           RMAX := SIZE - LVP@.FLDADDR;
"&&"                           IF LVP@.IDTYPE <> NIL THEN
"&&"                             RMAX := RMAX - LVP@.IDTYPE@.SIZE;
"&&"                           IF RMAX > 0 THEN
"&&"                             WRITE(QRR,'ETC=X',RMAX:1,'; ');
"&&"                         END;
"&&"                     END;
"&&"                   CHECKLN(3);
"&&"                   WRITE(QRR,'); ');
"E"                    END;
"E"
"E"           ARRAYS:  IF INXTYPE <> NIL THEN
"E"                      BEGIN    WRITE(QRR,'A ');
"&&"                     CHECKLN(26);
"E"                      GETBOUNDS(INXTYPE, RMIN, RMAX) ;
"E"                      WRITE(QRR, RMIN:1,' ', RMAX:1,' ');
"&&"                     PRNTTYPE(AELTYPE);
"E"                      END;
"E"           END (*CASE FORM OF...*)
"&&"        ELSE WRITE(QRR,';');
"E"
"E"      END;    (* PRNTTYPE *)
"E"
"E"   PROCEDURE PRNTVAR;
"&&"     VAR  I: 0 .. IDLNGTH;
"E"      BEGIN
"&&"       WITH VRP@ DO
"&&"         BEGIN  I := IDLNGTH;
"&&"           WHILE NAME(/I/) = ' ' DO  I := I - 1;
"&&"           CHECKLN( I+1 );
"&&"           WRITE( QRR, NAME:I, '=' );
"&&"           PRNTTYPE( IDTYPE );
"&&"         END
"E"      END;
"E"
"E"   BEGIN   (* PRNTSYMBL *)
"E"   IF PRCODE THEN
"&&"    IF LCP <> NIL THEN
"&&"      WITH LCP@ DO
"&&"        BEGIN
"&&"          IF KLASS = VARS THEN
"E"             BEGIN
"&&"              LINELN := 5;
"&&"              IF VKIND = FORMAL THEN
"&&"                BEGIN  WRITE(QRR,'@ ');  LINELN := 7  END;
"&&"              WRITE(QRR, VADDR:1, ' ');
"&&"              PRNTVAR( LCP );
"E"             END
"&&"          ELSE IF KLASS IN (/PROC,FUNC/) THEN
"&&"            BEGIN
"E"                 WRITELN(QRR,'% ',NAME,' ',PFNAME);
"SH"                LCP := PRMPTR;
"SH"                WHILE LCP <> NIL DO  (* SKIP PROC/FUNC PARAMETERS *)
"SH"                  BEGIN  IF LCP@.KLASS = VARS THEN PRNTSYMBL(LCP);
"SH"                         LCP := LCP@.NEXT  END;
"SH"            END;
"E"          WRITELN(QRR);
"E"        END (*IF LCP <> NIL, WITH LCP@... *)
"&&"     ELSE  (* DUMP HEAP STORAGE TYPE DEFINITIONS *)
"&&"       BEGIN
"&&"         TPT1 := PRNTTYPHD;
"&&"         WHILE TPT1 <> NIL DO
"&&"           BEGIN  WRITE(QRR,'>',TPT1@.TNO:1,' ');
"&&"             LINELN := 5;  PRNTTYPE(TPT1@.ELT);
"&&"             WRITELN(QRR);
"&&"             TPT1 := TPT1@.NXT;
"&&"           END;
"&&"         PRNTTYPHD := NIL;  PRNTTYNO := 0;
"&&"       END;
"E"   END;  (* PRNTSYMBL *)


  PROCEDURE BLOCK(FSYS: SETOFSYS; FSY: SYMBOL; FPROCP: CTP);
#   VAR LSY: SYMBOL; TEST: BOOLEAN; SEGSIZE: LABELRNG;
"&&"    LCP, FWRDPRCL: CTP;  DEC_ORDER: 0..4;


    PROCEDURE SKIP(FSYS: SETOFSYS);
      (*SKIP INPUT STRING UNTIL RELEVANT SYMBOL FOUND*)
    BEGIN
      WHILE NOT(SY IN FSYS) DO  INSYMBOL;
    END (*SKIP*) ;


#    PROCEDURE ALIGN(VAR Q:ADDRRANGE;  P: ADDRRANGE);
#      BEGIN
"&&"     CASE P OF
"&&"     REALSIZE:           Q := ((Q+7) DIV 8)*8;
"&&"     INTSIZE ",PTRSIZE": Q := ((Q+3) DIV 4)*4;
"&&"     HINTSIZE:           IF ODD(Q) THEN Q := Q + 1;
"&&"     CHARSIZE:           ;
"&&"     OTHERWISE:          IF ERRORCNT = 0 THEN ERROR(401);
"&&"     END;
#      END (*ALIGN*) ;


    PROCEDURE CONSTANT(FSYS: SETOFSYS; VAR FSP: STP; VAR FVALU: VALU);
      VAR LSP: STP; LCP: CTP; SIGN: (NONE,POS,NEG);
          LVP: CSP; I: 2..REALLNGTH;
    BEGIN LSP := NIL; FVALU.IVAL := 0;
      IF NOT(SY IN CONSTBEGSYS) THEN
        BEGIN ERROR(50); SKIP(FSYS+CONSTBEGSYS) END;
      IF SY IN CONSTBEGSYS THEN
        BEGIN
          IF SY = STRINGCONST THEN
            BEGIN
              IF LNGTH = 1 THEN LSP := CHARPTR
              ELSE
                BEGIN
                  NEW(LSP,ARRAYS);
                  WITH LSP@ DO
                    BEGIN AELTYPE := CHARPTR; INXTYPE := NIL;
                       SIZE := LNGTH*CHARSIZE; FORM := ARRAYS;
"&&"                   ALN := CHARSIZE;
                    END
                END;
              FVALU := VAL; INSYMBOL
            END
          ELSE
            BEGIN
              SIGN := NONE;
              IF (SY = ADDOP) AND (OP IN [PLUS,MINUS]) THEN
                BEGIN IF OP = PLUS THEN SIGN := POS ELSE SIGN := NEG;
                  INSYMBOL
                END;
              IF SY = IDENT THEN
                BEGIN SEARCHID([KONST],LCP);
                  WITH LCP@ DO
                    BEGIN LSP := IDTYPE; FVALU := VALUES END;
                  IF SIGN <> NONE THEN
                    IF LSP = INTPTR THEN
                      BEGIN IF SIGN = NEG THEN FVALU.IVAL := -FVALU.IVAL END
                    ELSE
                      IF LSP = REALPTR THEN
                        BEGIN
                          IF SIGN = NEG THEN
                            BEGIN NEW(LVP,REEL);
"&&"                          LVP@.RVAL := FVALU.VALP@.RVAL;
"&&"                          IF LVP@.RVAL(/1/) = '-' THEN
"&&"                            LVP@.RVAL(/1/) := '+'
"&&"                          ELSE LVP@.RVAL(/1/) := '-';
                              FVALU.VALP := LVP;
                            END
                          END
                        ELSE ERROR(105);
                  INSYMBOL;
                END
              ELSE
                IF SY = INTCONST THEN
                  BEGIN IF SIGN = NEG THEN VAL.IVAL := -VAL.IVAL;
                    LSP := INTPTR; FVALU := VAL; INSYMBOL
                  END
                ELSE
                  IF SY = REALCONST THEN
                    BEGIN IF SIGN = NEG THEN VAL.VALP@.RVAL[1] := '-';
                      LSP := REALPTR; FVALU := VAL; INSYMBOL
                    END
                  ELSE
                    BEGIN ERROR(106); SKIP(FSYS) END
            END;
          IF NOT (SY IN FSYS) THEN
            BEGIN ERROR(6); SKIP(FSYS) END
          END;
      FSP := LSP
    END (*CONSTANT*) ;


    FUNCTION COMPTYPES(FSP1,FSP2: STP) : BOOLEAN;
      (*DECIDE WHETHER STRUCTURES POINTED AT BY FSP1 AND FSP2 ARE COMPATIBLE*)
      VAR NXT1,NXT2: CTP; COMP: BOOLEAN;
        LTESTP1,LTESTP2 : TESTP;
    BEGIN
      IF FSP1 = FSP2 THEN COMPTYPES := TRUE
      ELSE
        IF (FSP1 <> NIL) AND (FSP2 <> NIL) THEN
          IF FSP1@.FORM = FSP2@.FORM THEN
            CASE FSP1@.FORM OF
              SCALAR:
                COMPTYPES := FALSE;
                (* IDENTICAL SCALARS DECLARED ON DIFFERENT LEVELS ARE
                 NOT RECOGNIZED TO BE COMPATIBLE*)
              SUBRANGE:
                COMPTYPES := COMPTYPES(FSP1@.RANGETYPE,FSP2@.RANGETYPE);
              POINTER:
                  BEGIN
                    COMP := FALSE; LTESTP1 := GLOBTESTP;
                    LTESTP2 := GLOBTESTP;
                    WHILE LTESTP1 <> NIL DO
                      WITH LTESTP1@ DO
                        BEGIN
                          IF (ELT1 = FSP1@.ELTYPE) AND
                            (ELT2 = FSP2@.ELTYPE) THEN COMP := TRUE;
                          LTESTP1 := LASTTESTP
                        END;
                    IF NOT COMP THEN
                      BEGIN NEW(LTESTP1);
                        WITH LTESTP1@ DO
                          BEGIN ELT1 := FSP1@.ELTYPE;
                            ELT2 := FSP2@.ELTYPE;
                            LASTTESTP := GLOBTESTP
                          END;
                        GLOBTESTP := LTESTP1;
                        COMP := COMPTYPES(FSP1@.ELTYPE,FSP2@.ELTYPE)
                      END;
                    COMPTYPES := COMP; GLOBTESTP := LTESTP2
                  END;
              POWER:
                COMPTYPES := COMPTYPES(FSP1@.ELSET,FSP2@.ELSET);
              ARRAYS:
                COMPTYPES := COMPTYPES(FSP1@.AELTYPE,FSP2@.AELTYPE)
                             AND (FSP1@.SIZE = FSP2@.SIZE);
                (*ALTERNATIVES: -- ADD A THIRD BOOLEAN TERM: INDEXTYPE MUST
                                  BE COMPATIBLE.
                               -- ADD A FOURTH BOOLEAN TERM: LOWBOUNDS MUST
                                  BE THE SAME*)
              RECORDS:
                BEGIN NXT1 := FSP1@.FSTFLD; NXT2 := FSP2@.FSTFLD;
"&&"              COMP := (FSP1@.RECVAR = FSP2@.RECVAR);
"&&"              WHILE COMP AND (NXT1 <> NIL) AND (NXT2 <> NIL) DO
                    BEGIN
                      IF NOT COMPTYPES(NXT1@.IDTYPE,NXT2@.IDTYPE) THEN
                        COMP := FALSE;
"&&"                  IF NXT1@.IDTYPE@.SIZE <> NXT2@.IDTYPE@.SIZE THEN
"&&"                    COMP := FALSE;
                      NXT1 := NXT1@.NEXT; NXT2 := NXT2@.NEXT
                    END;
                  COMPTYPES := COMP AND (NXT1 = NIL) AND (NXT2 = NIL)
                END;
                (*IDENTICAL RECORDS ARE RECOGNIZED TO BE COMPATIBLE
                 IFF NO VARIANTS OCCUR*)
              FILES:
                COMPTYPES := COMPTYPES(FSP1@.FILTYPE,FSP2@.FILTYPE)
            END (*CASE*)
          ELSE (*FSP1@.FORM <> FSP2@.FORM*)
            IF FSP1@.FORM = SUBRANGE THEN
              COMPTYPES := COMPTYPES(FSP1@.RANGETYPE,FSP2)
            ELSE
              IF FSP2@.FORM = SUBRANGE THEN
                COMPTYPES := COMPTYPES(FSP1,FSP2@.RANGETYPE)
              ELSE COMPTYPES := FALSE
        ELSE COMPTYPES := TRUE
    END (*COMPTYPES*) ;


    FUNCTION STRING(FSP: STP) : BOOLEAN;
    BEGIN STRING := FALSE;
      IF FSP <> NIL THEN
        IF FSP@.FORM = ARRAYS THEN
#         STRING := COMPTYPES(FSP@.AELTYPE,CHARPTR)
    END (*STRING*) ;

"**" FUNCTION CALC_SETSIZE(ELT: STP): INTEGER;
"**"    (* COMPUTES SIZE OF SET WHOSE ELEMENT TYPE IS ELT *)
"**"   VAR  MIN, MAX: INTEGER;
"**"   BEGIN  MAX := -1;
"**"      IF ELT <> NIL THEN GETBOUNDS( ELT, MIN, MAX );
"**"      CALC_SETSIZE := ((MAX+SETPACK) DIV SETPACK) * WORDSIZE;
"**"   END;

    PROCEDURE TYP(FSYS: SETOFSYS; VAR FSP: STP; VAR FSIZE: ADDRRANGE);
"&&"  VAR LSP,LSP1,LSP2: STP; OLDTOP: DISPRANGE; LCP,LCP2: CTP;
          LSIZE,DISPL: ADDRRANGE; LMIN,LMAX: INTEGER;  ALNFCT: ALNRNG ;
"&&"      OLDPACKST, PACKST2: BOOLEAN;

      PROCEDURE SIMPLETYPE(FSYS:SETOFSYS; VAR FSP:STP);
        VAR LSP,LSP1: STP; LCP,LCP1: CTP; TTOP: DISPRANGE;
"&&"        LCNT: INTEGER;  LVALU: VALU;  FLAG: BOOLEAN;
        BEGIN
        IF NOT (SY IN SIMPTYPEBEGSYS) THEN
          BEGIN ERROR(1); SKIP(FSYS + SIMPTYPEBEGSYS) END;
        IF SY IN SIMPTYPEBEGSYS THEN
          BEGIN
            IF SY = LPARENT THEN
              BEGIN TTOP := TOP;   (*DECL. CONSTS LOCAL TO INNERMOST BLOCK*)
"&&"            TOP := LEVEL;
                NEW(LSP,SCALAR,DECLARED);
                WITH LSP@ DO
                  BEGIN SIZE := INTSIZE; FORM := SCALAR;
                    SCALKIND := DECLARED
                  END;
                LCP1 := NIL; LCNT := 0;
                REPEAT INSYMBOL;
                  IF SY = IDENT THEN
                    BEGIN NEW(LCP,KONST);
                      WITH LCP@ DO
                        BEGIN NAME := ID; IDTYPE := LSP; NEXT := LCP1;
                          VALUES.IVAL := LCNT; KLASS := KONST
                        END;
                      ENTERID(LCP);
                      LCNT := LCNT + 1;
                      LCP1 := LCP; INSYMBOL
                    END
                  ELSE ERROR(2);
                  IF NOT (SY IN FSYS + [COMMA,RPARENT]) THEN
                    BEGIN ERROR(6); SKIP(FSYS + [COMMA,RPARENT]) END
                UNTIL SY <> COMMA;
"&&"            IF NOT NOPACKING THEN
"&&"                BEGIN  LSP@.SIZE := HINTSIZE;
"&&"                  IF PACKDATA THEN IF LCNT <= ORDCHMAX THEN
"&&"                    LSP@.SIZE := CHARSIZE
"&&"                END;
#               LSP@.ALN := LSP@.SIZE ;
                LSP@.FCONST := LCP1; TOP := TTOP;
                IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4)
              END
            ELSE
              BEGIN
                IF SY = IDENT THEN
                  BEGIN SEARCHID([TYPES,KONST],LCP);
                    INSYMBOL;
                    IF LCP@.KLASS = KONST THEN
                      BEGIN NEW(LSP,SUBRANGE);
                        WITH LSP@, LCP@ DO
                          BEGIN RANGETYPE := IDTYPE; FORM := SUBRANGE;
                            IF STRING(RANGETYPE) THEN
                              BEGIN ERROR(148); RANGETYPE := NIL END;
#                           MIN := VALUES; SIZE := IDTYPE@.SIZE
                          END;
"&&"                  END  (*IF LCP@.KLASS ...*)
"&&"                ELSE
"&&"                  LSP := NIL;  (* SIGNALS TYPE ID SEEN *)
"&&"              END
                ELSE
                  BEGIN NEW(LSP,SUBRANGE); LSP@.FORM := SUBRANGE;
                    CONSTANT(FSYS + [DOTDOT],LSP1,LVALU);
                    IF STRING(LSP1) THEN
                      BEGIN ERROR(148); LSP1 := NIL END;
                    WITH LSP@ DO
#                     BEGIN RANGETYPE:=LSP1; MIN:=LVALU; SIZE:=INTSIZE;
#                     IF LSP1 <> NIL THEN SIZE := LSP1@.SIZE ;
#                     END;
"&&"              END;
"&&"            IF LSP <> NIL THEN  (* CONSTANT JUST SEEN *)
"&&"              BEGIN
#                   IF SY = DOTDOT THEN INSYMBOL ELSE ERROR(5);
                    CONSTANT(FSYS,LSP1,LVALU);
                    LSP@.MAX := LVALU;
                    IF LSP1@.SIZE > CHARSIZE THEN  (* SCOPE FOR PACKING *)
"&&"                 IF NOT NOPACKING THEN
"&&"                  IF LVALU.IVAL <= 32767 THEN
"&&"                    IF LSP@.MIN.IVAL >= -32768 THEN
"&&"                      BEGIN  LSP@.SIZE := HINTSIZE;
"&&"                        IF PACKDATA THEN
"&&"                          IF LVALU.IVAL<=ORDCHMAX THEN
"&&"                            IF LSP@.MIN.IVAL>=0 THEN
"&&"                              LSP@.SIZE := CHARSIZE;
"&&"                      END;
#                   LSP@.ALN := LSP@.SIZE ;
                    IF LSP@.RANGETYPE <> LSP1 THEN ERROR(107)
                  END
"&&"            ELSE
"&&"              BEGIN
"&&"                LSP := LCP@.IDTYPE;  FLAG := FALSE;
"&&"                IF PACKDATA THEN IF NOT NOPACKING THEN
"&&"                  IF LSP <> NIL THEN
"&&"                    WITH LSP@ DO
"&&"                      IF SIZE > CHARSIZE THEN
"&&"                        IF FORM = SCALAR THEN
"&&"                          IF SCALKIND = DECLARED THEN
"&&"                            BEGIN  LCNT := -1;  LCP1 := FCONST;
"&&"                              WHILE LCP1 <> NIL DO
"&&"                                BEGIN  LCNT := LCNT + 1;
"&&"                                  LCP1 := LCP1@.NEXT;
"&&"                                END;
"&&"                              IF LCNT <= ORDCHMAX THEN FLAG := TRUE
"&&"                            END
"&&"                          ELSE
"&&"                        ELSE IF FORM = SUBRANGE THEN
"&&"                          IF MIN.IVAL >= 0 THEN
"&&"                            IF MAX.IVAL <= ORDCHMAX THEN FLAG := TRUE;
"&&"                IF FLAG THEN
"&&"                  BEGIN  (*CREATE PACKED VERSION OF THIS TYPE*)
"&&"                    NEW(LSP1);
"&&"                    LSP1@ := LSP@;
"&&"                    WITH LSP1@ DO
"&&"                      BEGIN  SIZE := CHARSIZE;  ALN := CHARSIZE;
"&&"                        IF FORM = SCALAR THEN  (* CONVERT TO SUBRANGE *)
"&&"                          BEGIN  FORM := SUBRANGE;
"&&"                            RANGETYPE := LSP;
"&&"                            MIN.IVAL := 0;  MAX.IVAL := LCNT;
"&&"                          END;
"&&"                      END;
"&&"                    LSP := LSP1;
"&&"                  END
"&&"              END;
                IF LSP <> NIL THEN
                  WITH LSP@ DO
                    IF FORM = SUBRANGE THEN
                      IF RANGETYPE <> NIL THEN
                        IF RANGETYPE = REALPTR THEN ERROR(398)
                        ELSE
                          IF MIN.IVAL > MAX.IVAL THEN ERROR(102)
              END;
            FSP := LSP;
            IF NOT (SY IN FSYS) THEN
              BEGIN ERROR(6); SKIP(FSYS) END
          END
            ELSE FSP := NIL
      END (*SIMPLETYPE*) ;


#     PROCEDURE FIELDLIST(FSYS: SETOFSYS; VAR FRECVAR: STP;
"&&"              VAR RECALN: ALNRNG; FLDOWNER: STP; VAR FIRSTFLD: CTP);
"&&"    LABEL 10;
        VAR LCP,LCP1,NXT,NXT1: CTP; LSP,LSP1,LSP2,LSP3,LSP4: STP;
#           MINSIZE,MAXSIZE,LSIZE: ADDRRANGE; LVALU: VALU; LALNFCT : ALNRNG ;
"&&"  BEGIN  NXT := NIL;  FIRSTFLD := NIL;  LSP := NIL;  RECALN := 1;
        IF NOT (SY IN FSYS+[IDENT,CASESY]) THEN
          BEGIN ERROR(19); SKIP(FSYS + [IDENT,CASESY]) END;

        WHILE SY = IDENT DO
#         BEGIN  NXT1 := NIL;
            REPEAT
              IF SY = IDENT THEN
                BEGIN NEW(LCP,FIELD);
                  WITH LCP@ DO
#                   BEGIN NAME := ID; IDTYPE := NIL; NEXT := NIL;
"&&"                  OWNER := FLDOWNER;  KLASS := FIELD;
"SY"                  IF GET_STAT THEN FLDOWNER@.NO_FLDS:=FLDOWNER@.NO_FLDS+1;
                    END;
#                 IF NXT1 = NIL THEN  NXT1 := LCP;
"&&"              IF NXT <> NIL THEN
"&&"                NXT@.NEXT := LCP;
                  NXT := LCP;
                  ENTERID(LCP);
                  INSYMBOL
                END
              ELSE ERROR(2);
              IF NOT (SY IN [COMMA,COLON]) THEN
                BEGIN ERROR(6); SKIP(FSYS + [COMMA,COLON,SEMICOLON,CASESY])
                END;
              TEST := SY <> COMMA;
              IF NOT TEST  THEN INSYMBOL
            UNTIL TEST;
            IF SY = COLON THEN INSYMBOL ELSE ERROR(5);
"&&"        IF FIRSTFLD = NIL THEN FIRSTFLD := NXT1;
            TYP(FSYS + [CASESY,SEMICOLON],LSP,LSIZE);
#           LALNFCT := 1 ;  IF LSP <> NIL THEN LALNFCT := LSP@.ALN ;
#
#           WHILE NXT1 <> NIL DO  (* ANY "FIELDS" DEFINED IN THIS ROUND ? *)
#             BEGIN
#             WITH NXT1@ DO
#               BEGIN  IDTYPE := LSP; ALIGN(DISPL,LALNFCT);  FLDADDR := DISPL;
#               DISPL := DISPL + LSIZE;  NXT1 := NEXT;
#               END;
#             END (*WHILE NXT1 <> NIL*);
#
#           IF LALNFCT > RECALN THEN RECALN := LSP@.ALN ;
#           IF SY = SEMICOLON THEN
#             BEGIN INSYMBOL;
#               IF NOT (SY IN [IDENT,CASESY,ENDSY]) THEN     (* IGNOR EXTRA ; *)
#                 BEGIN ERROR(19); SKIP(FSYS + [IDENT,CASESY]) END
#             END
#         END (*WHILE*);
#
        IF SY = CASESY THEN
          BEGIN NEW(LSP,TAGFLD);
            WITH LSP@ DO
              BEGIN TAGFIELDP := NIL; FSTVAR := NIL; FORM:=TAGFLD END;
            FRECVAR := LSP;
            INSYMBOL;
            IF SY = IDENT THEN
              BEGIN NEW(LCP,FIELD);
                WITH LCP@ DO
                  BEGIN NAME := ID; IDTYPE := NIL; KLASS:=FIELD;
#                   NEXT := NIL ; (*FLDADDR WILL BE SET WHEN TYPE IS KNOWN*)
"&&"                OWNER := FLDOWNER;
"SY"                IF GET_STAT THEN FLDOWNER@.NO_FLDS:=FLDOWNER@.NO_FLDS+1;
                  END;
"TF"           INSYMBOL;
"TF"           IF SY = COLON THEN  (* EXPLICIT TAG FIELD *)
"TF"             BEGIN
#                    ENTERID(LCP);  INSYMBOL ;
#                    IF SY <> IDENT THEN
"&&"                   GOTO 10
"&&"             END
"&&"           ELSE BEGIN
"&&"                  ID := LCP@.NAME;  LCP@.NAME := BLANKID;
"&&"                END;
"&&"           SEARCHID( [TYPES], LCP1 );
               LSP1 := LCP1@.IDTYPE;
               IF LSP1 <> NIL THEN
                 WITH LSP1@ DO
#                  BEGIN
"TF"                 IF LCP@.NAME <> BLANKID THEN
                       BEGIN
#                        ALIGN(DISPL,ALN) ;
#                        IF ALN > RECALN THEN RECALN := ALN ;
#                        LCP@.FLDADDR := DISPL ;  DISPL := DISPL + SIZE;
"TF"                   END (* LCP@.NAME <> BLANKID *) ;
                     IF (FORM <= SUBRANGE) OR STRING(LSP1) THEN
                       BEGIN IF COMPTYPES(REALPTR,LSP1) THEN ERROR(109)
                         ELSE IF STRING(LSP1) THEN ERROR(398);
                         LCP@.IDTYPE := LSP1; LSP@.TAGFIELDP := LCP;
                       END
                     ELSE ERROR(110);
                   END (* WITH LSP1@ DO *) ;
"&&"           IF LCP@.NAME <> BLANKID THEN INSYMBOL;
              END
            ELSE
10:             BEGIN ERROR(2); SKIP(FSYS + [OFSY,LPARENT]) END;
#           LSP@.SIZE := DISPL;
            IF SY = OFSY THEN INSYMBOL ELSE ERROR(8);
            LSP1 := NIL; MINSIZE := DISPL; MAXSIZE := DISPL;
            REPEAT LSP2 := NIL;
              REPEAT CONSTANT(FSYS + [COMMA,COLON,LPARENT],LSP3,LVALU);
                IF LSP@.TAGFIELDP <> NIL THEN
                 IF NOT COMPTYPES(LSP@.TAGFIELDP@.IDTYPE,LSP3)THEN ERROR(111);
                NEW(LSP3,VARIANT);
                WITH LSP3@ DO
                  BEGIN NXTVAR := LSP1; SUBVAR := LSP2; VARVAL := LVALU;
"&&"                FSTSUBFLD := NIL;  FORM := VARIANT
                  END;
                LSP1 := LSP3; LSP2 := LSP3;
                TEST := SY <> COMMA;
                IF NOT TEST THEN INSYMBOL
              UNTIL TEST;
              IF SY = COLON THEN INSYMBOL ELSE ERROR(5);
              IF SY = LPARENT THEN INSYMBOL ELSE ERROR(9);
"&&"          FIELDLIST(FSYS + [RPARENT,SEMICOLON],LSP2,LALNFCT,FLDOWNER,LCP1);
#             IF LALNFCT > RECALN THEN  RECALN := LALNFCT ;
              IF DISPL > MAXSIZE THEN MAXSIZE := DISPL;
              WHILE LSP3 <> NIL DO
"&&"            WITH LSP3@ DO
"&&"              BEGIN LSP4 := SUBVAR; SUBVAR := LSP2;
"&&"                SIZE := DISPL;  FSTSUBFLD := LCP1;  LSP3 := LSP4
                  END;
              IF SY = RPARENT THEN
                BEGIN INSYMBOL;
                  IF NOT (SY IN FSYS + [SEMICOLON]) THEN
                    BEGIN ERROR(6); SKIP(FSYS + [SEMICOLON]) END
                END
              ELSE ERROR(4);
              TEST := SY <> SEMICOLON;
              IF NOT TEST THEN
                BEGIN DISPL := MINSIZE;
#                  INSYMBOL ;  TEST := SY = ENDSY ;          (* IGNORE EXTRA ;*)
                END
            UNTIL TEST;
            DISPL := MAXSIZE;
            LSP@.FSTVAR := LSP1;
          END
        ELSE FRECVAR := NIL
      END (*FIELDLIST*) ;


    BEGIN (*TYP*)
"&&"  OLDPACKST := PACKDATA;
      IF NOT (SY IN TYPEBEGSYS) THEN
         BEGIN ERROR(10); SKIP(FSYS + TYPEBEGSYS) END;
      IF SY IN TYPEBEGSYS THEN
        BEGIN
          IF SY IN SIMPTYPEBEGSYS THEN SIMPLETYPE(FSYS,FSP)
          ELSE
    (*@*)     IF SY = ARROW THEN
              BEGIN NEW(LSP,POINTER); FSP := LSP;
                WITH LSP@ DO
                  BEGIN ELTYPE := NIL;
#                 SIZE := PTRSIZE; ALN := PTRSIZE ; FORM:=POINTER
                  END;
                INSYMBOL;
                IF SY = IDENT THEN
                  BEGIN PRTERR := FALSE; (*NO ERROR IF SEARCH NOT SUCCESSFUL*)
                    SEARCHID([TYPES],LCP); PRTERR := TRUE;
                    IF LCP = NIL THEN   (*FORWARD REFERENCED TYPE ID*)
                      BEGIN NEW(LCP,TYPES);
                        WITH LCP@ DO
                          BEGIN NAME := ID; IDTYPE := LSP;
                            NEXT := FWPTR; KLASS := TYPES
                          END;
                        FWPTR := LCP
                      END
                    ELSE
                      BEGIN
                        IF LCP@.IDTYPE <> NIL THEN
                          IF LCP@.IDTYPE@.FORM = FILES THEN ERROR(108)
                          ELSE LSP@.ELTYPE := LCP@.IDTYPE
                      END;
                    INSYMBOL;
                  END
                ELSE ERROR(2);
              END
            ELSE
              BEGIN
#"01/07"        LSP := NIL;
                IF SY = PACKEDSY THEN
                  BEGIN INSYMBOL;
"&&"                PACKDATA := TRUE;
                    IF NOT (SY IN TYPEDELS) THEN
                      BEGIN
                        ERROR(10); SKIP(FSYS + TYPEDELS)
                      END
                  END
"&&"            ELSE PACKDATA := FALSE;
    (*ARRAY*)   IF SY = ARRAYSY THEN
                  BEGIN INSYMBOL;
                    IF SY = LBRACK THEN INSYMBOL ELSE
"&&"                  BEGIN IF SY=LPARENT THEN
"&&"                        BEGIN  ERRKIND := 'W';  INSYMBOL  END;
"&&"                      ERROR(11)  END;
"&&"                LSP1 := NIL;  PACKST2 := PACKDATA;  PACKDATA := FALSE;
                    REPEAT NEW(LSP,ARRAYS);
                      WITH LSP@ DO
                        BEGIN AELTYPE := LSP1; INXTYPE := NIL; FORM:=ARRAYS END;
                      LSP1 := LSP;
"&&"                  SIMPLETYPE(FSYS + [COMMA,RBRACK,OFSY,RPARENT],LSP2);
                      IF LSP2 <> NIL THEN
                        IF LSP2@.FORM <= SUBRANGE THEN
                          BEGIN
                            IF LSP2 = REALPTR THEN
                              BEGIN ERROR(109); LSP2 := NIL END
                            ELSE
                              IF LSP2 = INTPTR THEN
                                BEGIN ERROR(149); LSP2 := NIL END;
                            LSP@.INXTYPE := LSP2
                          END
                        ELSE BEGIN ERROR(113); LSP2 := NIL END;
                      TEST := SY <> COMMA;
                      IF NOT TEST THEN INSYMBOL
                    UNTIL TEST;
                    IF SY = RBRACK THEN INSYMBOL ELSE
"&&"                  BEGIN IF SY=RPARENT THEN
"&&"                        BEGIN  ERRKIND := 'W';  INSYMBOL  END;
"&&"                      ERROR(12)  END;
                    IF SY = OFSY THEN INSYMBOL ELSE ERROR(8);
"&&"                PACKDATA := PACKST2;
#                   TYP(FSYS,LSP,LSIZE);
#                   IF LSP <> NIL THEN ALIGN(LSIZE,LSP@.ALN) ;
                    REPEAT
                      WITH LSP1@ DO
                        BEGIN LSP2 := AELTYPE; AELTYPE := LSP;
                          IF INXTYPE <> NIL THEN
                            BEGIN GETBOUNDS(INXTYPE,LMIN,LMAX);
                              LSIZE := LSIZE*(LMAX - LMIN + 1);
                              SIZE := LSIZE ;
#                             IF LSP <> NIL THEN
#                               ALN := LSP@.ALN (*PROPAGATE ALN*) ;
                            END
#"01/07"                  ELSE  (*INXTYPE = NIL*)  SIZE := 0
                        END;
                      LSP := LSP1; LSP1 := LSP2
                    UNTIL LSP1 = NIL
                  END
                ELSE
    (*RECORD*)    IF SY = RECORDSY THEN
                    BEGIN INSYMBOL;
"SY"                  IF GET_STAT THEN RE_CNT := RE_CNT + 1;
                      IF TOP < DISPLIMIT THEN
                        BEGIN TOP := TOP + 1;
                          WITH DISPLAY[TOP] DO
                            OCCUR := REC
                        END
"&&"                  ELSE FATALERROR(250);
                      DISPL := 0;
"&&"                  NEW(LSP,RECORDS);
                      WITH LSP@ DO
                        BEGIN
"&&"                      FLD_DISP_LEV := TOP;  FSTFLD := NIL;
"SY"                      NO_FLDS := 0;
"&&"                      FIELDLIST(FSYS-[SEMICOLON]+[ENDSY],LSP1,
"&&"                                ALNFCT,LSP,FSTFLD);
                          RECVAR := LSP1; SIZE := DISPL;
                          FORM := RECORDS ;  ALN := ALNFCT ;
"&&"                      FLD_DISP_LEV := -1;
                        END;
"&&"                  TOP := TOP - 1;
                      IF SY = ENDSY THEN INSYMBOL ELSE ERROR(13)
                    END
                  ELSE
    (*SET*)         IF SY = SETSY THEN
                      BEGIN INSYMBOL;
                        IF SY = OFSY THEN INSYMBOL ELSE ERROR(8);
"&&"                    PACKDATA := FALSE;
                        SIMPLETYPE(FSYS,LSP1);
                        IF LSP1 <> NIL THEN
                          IF LSP1 = INTPTR THEN ERROR(304)
                          ELSE IF (LSP1@.FORM > SUBRANGE) THEN
                            BEGIN ERROR(115); LSP1 := NIL END
                          ELSE
                            IF LSP1 = REALPTR THEN ERROR(114)
#                           ELSE IF LSP1@.FORM = SUBRANGE THEN
#                                  IF LSP1@.MAX.IVAL > SETMAX   THEN ERROR(304);
                        NEW(LSP,POWER);
                        WITH LSP@ DO
                          BEGIN ELSET:=LSP1;
"**"                      SIZE := CALC_SETSIZE(LSP1);
"**"                      ALN := WORDSIZE;  FORM := POWER
                          END;
                      END
                    ELSE
    (*FILE*)            IF SY = FILESY THEN
#                       BEGIN  INSYMBOL ;
#                       IF SY = OFSY THEN INSYMBOL  ELSE  ERROR(8) ;
"@@"                    TYP(FSYS,LSP1,LSIZE) ;
"@@"                    LSP := TEXTPTR;  (* ASSUME THE COMMON CASE *)
"@@"                    IF LSP1 <> NIL THEN
"@@"                      IF LSP1 <> CHARPTR THEN  (* NOT A TEXTFILE *)
"@@"                      IF LSP1@.FORM <> FILES THEN
"@@"                        BEGIN
"@@"                          NEW(LSP,FILES);
"@@"                          WITH LSP@ DO
"@@"                            BEGIN
"@@"                              FILTYPE := LSP1;  ALN := LSP1@.ALN;
"@@"                              SIZE := LSIZE + FILHDRSIZE;
"@@"                              IF ALN < PTRSIZE THEN ALN := PTRSIZE;
"@@"                              FORM := FILES;
"@@"                            END
"@@"                        END
"@@"                      ELSE BEGIN  LSP:=NIL;  ERROR(108)  END;
#                       END ;
                FSP := LSP
              END;
          IF NOT (SY IN FSYS) THEN
            BEGIN ERROR(6); SKIP(FSYS) END
        END
      ELSE FSP := NIL;
      IF FSP = NIL THEN FSIZE := 1 ELSE FSIZE := FSP@.SIZE;
"&&"  PACKDATA := OLDPACKST;
    END (*TYP*) ;


    PROCEDURE LABELDECLARATION;
      VAR LLP: LBP; REDEF: BOOLEAN; LBNAME: LABELRNG ;
    BEGIN
      REPEAT
        IF SY = INTCONST THEN
          WITH DISPLAY[TOP] DO
            BEGIN LLP := FLABEL; REDEF := FALSE;
              WHILE (LLP <> NIL) AND NOT REDEF DO
                IF LLP@.LABVAL <> VAL.IVAL THEN
                  LLP := LLP@.NEXTLAB
                ELSE BEGIN REDEF := TRUE; ERROR(166) END;
              IF NOT REDEF THEN
                BEGIN NEW(LLP);
                  WITH LLP@ DO
                    BEGIN LABVAL := VAL.IVAL; GENLABEL(LBNAME);
"&&"                  XNO := 0;  (* 0 => NOT AN EXTERNAL ENTRY PT. *)
                      DEFINED := FALSE; NEXTLAB := FLABEL; LABNAME := LBNAME
                    END;
                  FLABEL := LLP
                END;
              INSYMBOL
            END
        ELSE ERROR(15);
        IF NOT ( SY IN FSYS + [COMMA, SEMICOLON] ) THEN
          BEGIN ERROR(6); SKIP(FSYS+[COMMA,SEMICOLON]) END;
        TEST := SY <> COMMA;
        IF NOT TEST THEN INSYMBOL
      UNTIL TEST;
      IF SY = SEMICOLON THEN INSYMBOL ELSE ERROR(14)
    END (* LABELDECLARATION *) ;

"&&" PROCEDURE WRITESET( SP: CSP );
"&&"   VAR I,J,K,L,E,COL,LEN: INTEGER;  S: SETRANGE;  DELIM: CHAR;
"&&"   BEGIN  LEN := SP@.PLNGTH;  S := SP@.PVAL;
"&&"     I := 1;  DELIM := '(';  J := 0;  K := 0;  L := 0;  COL := 8;
"&&"     FOR E := 1 TO LEN*(SETPACK DIV WORDSIZE) DO
"&&"       BEGIN  J := J * 2;
"&&"         IF K IN S(/I/) THEN J := J + 1;
"&&"         L := L + 1;
"&&"         IF L = 16 THEN
"&&"           BEGIN  L := 0;
"&&"             IF COL >= 65 THEN
"&&"               BEGIN WRITELN(PRR,DELIM);  DELIM := ' ';  COL := 2  END;
"&&"             WRITE(PRR,DELIM,J:1);  DELIM := ',';
"&&"             IF J >= 1000 THEN COL := COL + 6 ELSE
"&&"             IF J >=   10 THEN COL := COL + 4 ELSE COL := COL + 2;
"&&"             J := 0;
"&&"           END;
"&&"         K := K + 1;
"&&"         IF K > SSETMAX THEN
"&&"           BEGIN  K := 0;  I := I + 1  END;
"&&"       END;
"&&"     IF LEN = 0 THEN WRITELN(PRR,'()') ELSE WRITELN(PRR,')');
"&&"   END (*WRITESET*) ;

      PROCEDURE PUTIC;
#     BEGIN
      IF LINECNT > OLDLN THEN
        BEGIN  WRITELN(PRR, ' LOC ',LINECNT:1);  OLDLN := LINECNT  END ;
      END;

"&&" PROCEDURE STRUCTCONSTANT( FSYS: SETOFSYS;  VAR FSP: STP;
"&&"                           VAR FVALU: VALU;  VAR SLC: INTEGER );
"&&"   LABEL 10;
"&&"   VAR  LSET: SETRANGE;  LVALU: VALU;  I,J,K,L,MAXELEM: INTEGER;
"&&"        LSP,LSP1,ELT,LRECVAR: STP;  FLDPR: CTP;  TEST: BOOLEAN;
"&&"        CSTEXTNAME: ARRAY(/1..EXTNAMSZ/) OF CHAR;
"&&"
"&&"   PROCEDURE STOWCONST( ELSP: STP );
"&&"     VAR  I,ELSIZE: INTEGER;  ELSP1: STP;  CH: CHAR;
"&&"     BEGIN  ELSP1 := ELSP;
"&&"       IF ELSP <> NIL THEN
"&&"         BEGIN  ALIGN(CONSTLC,ELSP@.ALN);  ELSIZE := ELSP@.SIZE  END
"&&"       ELSE ELSIZE := 1;
"&&"       STRUCTCONSTANT( FSYS+(/COMMA,RPARENT/),ELSP1,LVALU,I);
"&&"       IF NOT COMPTYPES(ELSP,ELSP1) THEN
"&&"         BEGIN  ERROR(145);  ELSP1 := NIL  END;
"&&"       IF ELSP1 <> NIL THEN
"&&"        IF PRCODE THEN IF I < 0 THEN
"&&"         BEGIN  PUTIC;  WRITE(PRR, CONSTLC:1, MN(/70/) (*DFC*) );
"&&"           IF ELSP1 = REALPTR THEN
"&&"             WRITELN(PRR,'R,':3,LVALU.VALP@.RVAL)
"&&"           ELSE IF ELSP1@.FORM <= SUBRANGE THEN
"&&"             BEGIN  CH := 'I';
"&&"               IF ELSIZE=2 THEN CH := 'H' ELSE IF ELSIZE=1 THEN CH := 'B';
"&&"               WRITELN(PRR, CH:2, ',', LVALU.IVAL:1 )
"&&"             END
"&&"           ELSE IF ELSP1@.FORM = POINTER THEN
"&&"             WRITELN(PRR,'N':2)
"&&"           ELSE IF ELSP1@.FORM = POWER THEN
"&&"             BEGIN  WRITE(PRR,'S,':3);
"&&"               LVALU.VALP@.PLNGTH := ELSP1@.SIZE;
"&&"               WRITESET( LVALU.VALP );
"&&"             END
"&&"           ELSE IF STRING(ELSP1) THEN
"&&"             BEGIN  WRITE(PRR,'M,''':4);  I := 1;
"&&"               WITH LVALU.VALP@ DO
"&&"                 WHILE I <= SLNGTH DO
"&&"                   BEGIN  WRITE(PRR,SVAL(/I/):1);
"&&"                     IF SVAL(/I/) = '''' THEN WRITE(PRR,'''');
"&&"                     I := I + 1
"&&"                   END;
"&&"               WRITELN(PRR,'''');
"&&"             END;
"&&"           CONSTLC := CONSTLC + ELSIZE;
"&&"         END;
"&&"     END (*STOWCONST*) ;

"&&"   BEGIN  (*STRUCTCONSTANT*)
"&&"     LSP := FSP;  FVALU.IVAL := 0;  SLC := -1;
"&&"     IF SY IN CONSTBEGSYS THEN  (*SIMPLE CONSTANT*)
"&&"       BEGIN
"&&"         CONSTANT( FSYS, FSP, FVALU );
"&&"         IF NOT COMPTYPES(LSP,FSP) THEN BEGIN ERROR(145); FSP := NIL END
"&&"         ELSE IF LSP <> NIL THEN FSP := LSP
"&&"       END
"&&"     ELSE IF SY = LBRACK THEN   (*SET CONSTANT*)
"&&"       BEGIN  INSYMBOL;
"&&"         ELT := NIL;  MAXELEM := -1;
"&&"         FOR I := 1 TO (SETMAX+1) DIV (SSETMAX+1) DO
"&&"           LSET(/I/) := (/ /);
"&&"         IF LSP <> NIL THEN
"&&"           IF LSP@.FORM = POWER THEN ELT := LSP@.ELSET ELSE ERROR(145);
"&&"           TEST := FALSE;
"&&"         IF SY <> RBRACK THEN
"&&"           REPEAT
"&&"             CONSTANT( FSYS+(/RBRACK,COMMA,DOTDOT/),LSP1,LVALU );
"&&"             IF NOT COMPTYPES(LSP1,ELT) THEN ERROR(145);
"&&"             ELT := LSP1;  I := LVALU.IVAL;
"&&"             IF SY = DOTDOT THEN
"&&"               BEGIN  INSYMBOL;
"&&"                 CONSTANT( FSYS+(/RBRACK,COMMA/),LSP1,LVALU );
"&&"                 IF NOT COMPTYPES(LSP1,ELT) THEN
"&&"                   BEGIN  LVALU.IVAL := I;  ERROR(137)  END
"&&"               END;
"&&"             IF (I < 0) OR (LVALU.IVAL > SETMAX) OR (I > LVALU.IVAL) THEN
"&&"               ERROR(137)
"&&"             ELSE
"&&"               BEGIN
"&&"                 IF LVALU.IVAL > MAXELEM THEN MAXELEM := LVALU.IVAL;
"&&"                 REPEAT  J := I DIV (SSETMAX+1);
"&&"                   LSET(/J+1/) := LSET(/J+1/) + (/I-J*(SSETMAX+1)/);
"&&"                   I := I + 1
"&&"                 UNTIL I > LVALU.IVAL
"&&"               END;
"&&"             IF SY = COMMA THEN INSYMBOL ELSE TEST := TRUE;
"&&"           UNTIL TEST;
"&&"         IF SY = RBRACK THEN INSYMBOL ELSE ERROR(12);
"&&"         NEW( FVALU.VALP, PSET );
"&&"         WITH FVALU.VALP@ DO
"&&"           BEGIN  PVAL := LSET;
"&&"             PLNGTH := ((MAXELEM+SETPACK) DIV SETPACK)*WORDSIZE;
"&&"           END;
"&&"         IF LSP = NIL THEN
"&&"           BEGIN  NEW(LSP,POWER);
"&&"             WITH LSP@ DO
"&&"               BEGIN  ELSET := ELT;  FORM := POWER;
"&&"                 SIZE := FVALU.VALP@.PLNGTH;  ALN := WORDSIZE
"&&"               END;
"&&"             FSP := LSP
"&&"           END
"&&"       END  (* IF SY=LBRACK... *)
"&&"     ELSE IF SY = LPARENT THEN     (*ARRAY OR RECORD CONSTANT*)
"&&"       BEGIN  INSYMBOL;  K := 0;
"&&"         IF CONSTLC < 0 THEN  (* NO CONSTANTS WRITTEN YET *)
"&&"           WITH FPROCP@ DO
"&&"             BEGIN  CSTEXTNAME := EXTNAME;  I := EXTNAMSZ;
"&&"               IF FPROCP <> MAINPROG THEN
"&&"                 IF NOT EXTRN THEN I := 5;
"&&"               REPEAT  CSTEXTNAME(/I/) := '#';  I := I - 1
"&&"               UNTIL  CSTEXTNAME(/I/) <> ' ';
"&&"               WRITELN(PRR,CSTEXTNAME,MN(/71/) (*CST*), ' ',
"&&"                 NAME,PFNAME:5,',',ASSEMBLE:1,',',GET_STAT:1,
"&&"                 ',',ASMVERB:1);
"&&"               CONSTLC := FIRSTCONSTLC;
"&&"             END;
"&&"         IF LSP <> NIL THEN
"&&"           WITH LSP@ DO
"&&"             IF FORM = ARRAYS THEN
"&&"               BEGIN  ALIGN(CONSTLC,ALN);  SLC := CONSTLC;  J := SLC;
"&&"                 IF AELTYPE <> NIL THEN L := AELTYPE@.SIZE
"&&"                 ELSE                   L := 1;
"&&"                 ALIGN(L,ALN);  TEST := FALSE;
"&&"                 REPEAT  K := K + 1;
"&&"                   STOWCONST( AELTYPE );
"&&"                   IF SY = COMMA THEN
"&&"                     BEGIN  INSYMBOL;  J := J+L;  CONSTLC := J  END
"&&"                   ELSE  TEST := TRUE
"&&"                 UNTIL TEST;
"&&"                 IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4);
"&&"                 IF INXTYPE <> NIL THEN
"&&"                   BEGIN  GETBOUNDS(INXTYPE,I,J);  J := J-I+1  END
"&&"                 ELSE  J := SIZE DIV L;
"&&"                 IF K <> J THEN
"&&"                   IF K > J THEN ERROR(207)
"&&"                   ELSE BEGIN
"&&"                     ERRKIND := 'W';  ERROR(306);
"&&"                     IF PRCODE THEN WRITELN(PRR, SLC+SIZE-1:1,
"&&"                         MN(/70/) (*DFC*), ' B,0');
"&&"                   END;
"&&"                 CONSTLC := SLC + SIZE;
"&&"               END
"&&"             ELSE IF FORM = RECORDS THEN
"&&"               BEGIN  ALIGN(CONSTLC,ALN);  SLC := CONSTLC;  L := SIZE;
"&&"                 LRECVAR := RECVAR;  TEST := TRUE;  FLDPR := FSTFLD;
"&&"   10:           WHILE TEST AND (FLDPR <> NIL) DO
"&&"                   WITH FLDPR@ DO
"&&"                     BEGIN  CONSTLC := SLC + FLDADDR;
"&&"                       STOWCONST( IDTYPE );
"&&"                       FLDPR := NEXT;
"&&"                       IF SY = COMMA THEN INSYMBOL ELSE TEST := FALSE
"&&"                     END;
"&&"                 IF TEST THEN
"&&"                   IF LRECVAR <> NIL THEN  (*TAG FIELD VALUE IS NEXT*)
"&&"                     WITH LRECVAR@ DO
"&&"                       IF TAGFIELDP <> NIL THEN
"&&"                         WITH TAGFIELDP@ DO
"&&"                           BEGIN
"&&"                             IF NAME <> BLANKID THEN
"&&"                               BEGIN  CONSTLC := SLC + FLDADDR;
"&&"                                      STOWCONST( IDTYPE )  END
"&&"                             ELSE BEGIN
"&&"                                 CONSTANT(FSYS+(/COMMA,RPARENT/),LSP1,LVALU)
"&&"                              ;  IF NOT COMPTYPES(IDTYPE,LSP1) THEN
"&&"                                   ERROR(145);
"&&"                               END;
"&&"                             IF SY=COMMA THEN INSYMBOL ELSE TEST:=FALSE;
"&&"                             LSP1 := FSTVAR;  L := SIZE;
"&&"                             WHILE LSP1 <> NIL DO
"&&"                               WITH LSP1@ DO
"&&"                                 IF VARVAL = LVALU THEN
"&&"                                   BEGIN  LRECVAR := SUBVAR;  L := SIZE;
"&&"                                     FLDPR := FSTSUBFLD;  GOTO 10
"&&"                                   END
"&&"                                 ELSE LSP1 := NXTVAR;
"&&"                           END;
"&&"                 CONSTLC := SLC + L;
"&&"                 IF SY <> RPARENT THEN ERROR(4) ELSE INSYMBOL;
"&&"               END (* IF FORM=RECORDS... *)
"&&"             ELSE ERROR(208);  (*WRONG FORM FOR CONSTANT*)
"&&"       END (*IF SY=LPARENT*)
"&&"     ELSE ERROR(50);
"&&"   END (*STRUCTCONSTANT*);

    PROCEDURE CONSTDECLARATION;
      VAR LCP: CTP; LSP: STP; LVALU: VALU;
"&&"      SKID: ALPHA;  SKLC: ADDRRANGE;
"&&" BEGIN  LISTTAG := 'C';
      IF SY <> IDENT THEN
        BEGIN ERROR(2); SKIP(FSYS + [IDENT]) END;
      WHILE SY = IDENT DO
"&&"    BEGIN SKID := ID;  INSYMBOL;
"&&"      IF SY = COLON THEN
"&&"        BEGIN  INSYMBOL;  EXTUSED := TRUE;
"&&"               TYP( FSYS+(/RELOP/), LSP, SKLC );
"&&"        END
"&&"      ELSE LSP := NIL;
"&&"      IF (SY=RELOP) AND (OP=EQOP) THEN
"&&"        BEGIN  INSYMBOL;
"&&"          STRUCTCONSTANT( FSYS+(/SEMICOLON/), LSP, LVALU, SKLC );
"&&"          IF SKLC >= 0 THEN
"&&"            NEW(LCP,STRUCTKONST)
"&&"          ELSE NEW(LCP,KONST);
"&&"          WITH LCP@ DO
"&&"            BEGIN  NAME := SKID;  IDTYPE := LSP;  NEXT := NIL;
"&&"              IF SKLC >= 0 THEN
"&&"                BEGIN  KLASS := STRUCTKONST;  SKOWNER := FPROCP;
"&&"                       SKADDR := SKLC
"&&"                END
"&&"              ELSE BEGIN  KLASS := KONST;  VALUES := LVALU  END
"&&"            END;
"&&"          ENTERID( LCP );
"&&"        END
"&&"      ELSE ERROR(16);
          IF SY = SEMICOLON THEN
            BEGIN INSYMBOL;
              IF NOT (SY IN FSYS + [IDENT]) THEN
                BEGIN ERROR(6); SKIP(FSYS + [IDENT]) END
            END
          ELSE ERROR(14)
        END;
"&&" LISTTAG := ' ';
    END (*CONSTDECLARATION*) ;

    PROCEDURE TYPEDECLARATION;
      VAR LCP,LCP1,LCP2: CTP; LSP: STP; LSIZE: ADDRRANGE;
    BEGIN
      IF SY <> IDENT THEN
        BEGIN ERROR(2); SKIP(FSYS + [IDENT]) END;
      WHILE SY = IDENT DO
        BEGIN NEW(LCP,TYPES);
          WITH LCP@ DO
            BEGIN NAME := ID; IDTYPE := NIL; KLASS := TYPES END;
          INSYMBOL;
          IF (SY = RELOP) AND (OP = EQOP) THEN INSYMBOL ELSE ERROR(16);
          TYP(FSYS + [SEMICOLON],LSP,LSIZE);
          ENTERID(LCP);
          LCP@.IDTYPE := LSP;
          (*HAS ANY FORWARD REFERENCE BEEN SATISFIED:*)
          LCP1 := FWPTR;
          WHILE LCP1 <> NIL DO
            BEGIN
              IF LCP1@.NAME = LCP@.NAME THEN
                BEGIN LCP1@.IDTYPE@.ELTYPE := LCP@.IDTYPE;
                  IF LCP1 <> FWPTR THEN
                    LCP2@.NEXT := LCP1@.NEXT
                  ELSE FWPTR := LCP1@.NEXT;
                END;
              LCP2 := LCP1; LCP1 := LCP1@.NEXT
            END;
          IF SY = SEMICOLON THEN
            BEGIN INSYMBOL;
              IF NOT (SY IN FSYS + [IDENT]) THEN
                BEGIN ERROR(6); SKIP(FSYS + [IDENT]) END
            END
          ELSE ERROR(14)
        END;
      IF FWPTR <> NIL THEN
"&&"    BEGIN ERROR(117); WRITELN(OUTPUT);  PLCNT := PLCNT + 1;
"&&"      REPEAT WRITELN(OUTPUT,' UNDEFINED TYPE: ',FWPTR@.NAME);
"&&"        PLCNT := PLCNT + 1;  FWPTR := FWPTR@.NEXT
          UNTIL FWPTR = NIL;
        END
    END (*TYPEDECLARATION*) ;

    PROCEDURE VARDECLARATION;
#     VAR  LCP, NXT, NXT1: CTP;  LSP: STP;  LSIZE: ADDRRANGE;
           LFPTR : FRECPTR ;
"&&" BEGIN  LISTTAG := 'D';  NXT := NIL;
#     REPEAT  NXT1 := NIL;
#       REPEAT
#         IF SY = IDENT THEN
#           BEGIN  NEW(LCP,VARS);
#             WITH LCP@ DO
#               BEGIN  NAME := ID;  NEXT := NIL;  KLASS := VARS;
#                 IDTYPE := NIL;  VKIND := ACTUAL;  VLEV := LEVEL
#               END;
#             ENTERID(LCP);
#             IF NXT1 = NIL THEN NXT1 := LCP;       (*BEGINNING OF THIS ROUND*)
#             IF NXT <> NIL THEN NXT@.NEXT := LCP;  (*LINK TO PREVIOUS CHAIN*)
#             NXT := LCP;
#             INSYMBOL;
#           END
#         ELSE ERROR(2);
#         IF NOT (SY IN FSYS + [COMMA,COLON] + TYPEDELS) THEN
#           BEGIN ERROR(6); SKIP(FSYS+[COMMA,COLON,SEMICOLON]+TYPEDELS) END;
#         TEST := SY <> COMMA;
#         IF NOT TEST THEN INSYMBOL
#       UNTIL TEST;
#       IF SY = COLON THEN INSYMBOL ELSE ERROR(5);
#       TYP(FSYS + [SEMICOLON] + TYPEDELS,LSP,LSIZE);
#
#       WHILE NXT1 <> NIL DO
#         WITH  NXT1@ DO
#           BEGIN   IDTYPE := LSP;
"@@"          IF LSP <> NIL THEN
"@@"            BEGIN
"@@"              ALIGN( LC, LSP@.ALN );
"@@"              VADDR := LC;  LC := LC + LSIZE;
"@@"              IF LSP@.FORM = FILES THEN
"@@"                IF LEVEL > 1 THEN
"@@"                  ERROR( 398 )   (* ONLY GLOBAL FILES SUPPORTED *)
"@@"                ELSE BEGIN
"@@"                  NEW(LFPTR);  LFPTR@.FILIDPTR := NXT1;
"@@"                  LFPTR@.NEXTFILE := FILEHEAD;  FILEHEAD := LFPTR
"@@"                END
"@@"            END
"@@"          ELSE VADDR := LC;
"E"           IF DEBUG_LEV > 0 THEN  PRNTSYMBL(NXT1);
#             NXT1 := NEXT;
#           END;

        IF SY = SEMICOLON THEN
          BEGIN INSYMBOL;
            IF NOT (SY IN FSYS + [IDENT]) THEN
              BEGIN ERROR(6); SKIP(FSYS + [IDENT]) END
          END
        ELSE ERROR(14)
      UNTIL (SY <> IDENT) AND NOT (SY IN TYPEDELS);
"&&"  LISTTAG := ' ';
      IF FWPTR <> NIL THEN
        BEGIN ERROR(117); WRITELN(OUTPUT); PLCNT := PLCNT + 1;
          REPEAT WRITELN(OUTPUT,' UNDEFINED TYPE: ',FWPTR@.NAME);
            PLCNT := PLCNT + 1;  FWPTR := FWPTR@.NEXT
          UNTIL FWPTR = NIL;
        END ;
    END (*VARDECLARATION*) ;

"&&" PROCEDURE MKNAME(VAR ALB: ALPHA; NLB: INTEGER; NCFLAG: BOOLEAN);
       VAR I,J: INTEGER;
     BEGIN  I := 1;  J := 8;
#      IF NOT NCFLAG THEN J := 5;
#      REPEAT
#        IF ALB(/I/) = '_' THEN ALB(/I/) := '$';  I := I + 1
#      UNTIL (I > J) OR (ALB(/I/) = ' ');
#      IF NOT NCFLAG THEN
#        FOR J := 8 DOWNTO I DO
#          BEGIN  ALB(/J/) := CHR( ORD('0') + NLB MOD 10 );
#            NLB := NLB DIV 10
#          END;
#    END (*MKNAME*) ;

    PROCEDURE PROCDECLARATION(FSY: SYMBOL);
"&&" LABEL 10;
"&&"  VAR LSY: SYMBOL; LCP,LCP1,LCP2: CTP; LSP: STP;
"&&"      FORW: BOOLEAN; K, PARCNT: INTEGER;  OLDLABEL: LABELRNG;
"@@"      LLC,LCM: ADDRRANGE; I, NAME: INTEGER; MARKP: @INTEGER;
"&&"      OLD_HASH: HASH_TABLE;

"SH"  PROCEDURE PARAMETERLIST(FSY: SETOFSYS; FPAR: CTP; FW: BOOLEAN);
"**"    VAR LCP,LCP1,LCP2,LCP3,LCP4: CTP; LSP: STP; LKIND: IDKIND;
"**"      LLC,LEN,LALN: ADDRRANGE;
"SH"      LSY: SYMBOL;
"**"  BEGIN   LCP1 := NIL;  LCP := NIL;
        IF NOT (SY IN FSY + [LPARENT]) THEN
          BEGIN ERROR(7); SKIP(FSYS + FSY + [LPARENT]) END;
        IF SY = LPARENT THEN
"SH"      BEGIN IF FORW THEN ERROR(119) ELSE LC := LCAFTMST + FPSAVEAREA;
            INSYMBOL;
            IF NOT (SY IN [IDENT,VARSY,PROCSY,FUNCSY]) THEN
              BEGIN ERROR(7); SKIP(FSYS + [IDENT,RPARENT]) END;
            WHILE SY IN [IDENT,VARSY,PROCSY,FUNCSY] DO
"**"          BEGIN  LCP3 := NIL;
"SH"            IF SY IN [PROCSY,FUNCSY] THEN
"SH"              BEGIN  LSY := SY;  (*REMEMBER IF PROC OR FUNC *)
"SH"                INSYMBOL;
                      IF SY = IDENT THEN
"SH"                  BEGIN
"SH"                    IF LSY = PROCSY THEN NEW(LCP,PROC,DECLARED)
"SH"                    ELSE                 NEW(LCP,FUNC,DECLARED);
"**"                    IF LCP3<>NIL THEN LCP4@.NEXT:=LCP ELSE LCP3:=LCP;
"**"                    LCP4 := LCP;
"SH"                    ALIGN(LC, PTRSIZE);
                        WITH LCP@ DO
"**"                      BEGIN NAME := ID; IDTYPE := NIL; NEXT := NIL;
"SH"                        PFDECKIND := DECLARED;  PFKIND := FORMAL;
"SH"                        FRTRN := FALSE;  EXTRN := FALSE;
"&&"                        EXTNAME := '*PFPARM*';
"SH"                        PFLEV := LC*10 + LEVEL;
"SH"                        (*I.E. PFLEV > LCAFTMST => PROC PARM *)
"SH"                        PROCLAB := PROCLAB+1;  PFNAME := PROCLAB;
"SH"                        IF LSY = PROCSY THEN KLASS := PROC
"SH"                        ELSE                 KLASS := FUNC;
                            END;
                        ENTERID(LCP);
"&&"                    LC := LC + DISPAREA;
"SH"                    INSYMBOL;
"SH"                    LLC := LC;
"SH"                    IF LSY = PROCSY THEN
"SH"                      PARAMETERLIST( [SEMICOLON,RPARENT], LCP, FALSE )
"SH"                    ELSE
"SH"                      PARAMETERLIST( [SEMICOLON,COLON], LCP, FALSE );
"SH"                    LC := LLC;
                      END
                    ELSE ERROR(2);
"SH"                IF NOT (SY IN FSYS + [SEMICOLON,RPARENT]) THEN
"SH"                  BEGIN ERROR(7);SKIP(FSYS+[SEMICOLON,RPARENT])END
                  END
                ELSE
                  BEGIN
                    IF SY = VARSY THEN
                      BEGIN LKIND := FORMAL; INSYMBOL END
                    ELSE LKIND := ACTUAL;
                    REPEAT
                      IF SY = IDENT THEN
                        BEGIN NEW(LCP,VARS);
"**"                      IF LCP3<>NIL THEN LCP4@.NEXT:=LCP ELSE LCP3:=LCP;
"**"                      LCP4 := LCP;
                          WITH LCP@ DO
                            BEGIN NAME:=ID; IDTYPE:=NIL; KLASS:=VARS;
                              VKIND := LKIND; NEXT := NIL; VLEV := LEVEL;
                            END;
                          ENTERID(LCP);
                          INSYMBOL;
                        END;
                      IF NOT (SY IN [COMMA,COLON] + FSYS) THEN
                        BEGIN ERROR(7);SKIP(FSYS+[COMMA,SEMICOLON,RPARENT])
                        END;
                      TEST := SY <> COMMA;
                      IF NOT TEST THEN INSYMBOL
                    UNTIL TEST;
                    IF SY = COLON THEN
                      BEGIN INSYMBOL;
                        IF SY = IDENT THEN
"**"                      BEGIN  SEARCHID([TYPES],LCP4); LEN := PTRSIZE;
"**"                        LSP := LCP4@.IDTYPE;  LALN := PTRSIZE;
                            IF LSP <> NIL THEN
#                             IF (LKIND=ACTUAL) THEN
"@@"                            IF LSP@.FORM = FILES THEN
"**"                              BEGIN ERROR(121); LKIND:=FORMAL END
"**"                            ELSE
"**"                              BEGIN LEN:=LSP@.SIZE; LALN:=LSP@.ALN END;
"**"                        LCP4 := LCP3;
"**"                        WHILE LCP4 <> NIL DO
"**"                        BEGIN WITH LCP4@ DO
"**"                          BEGIN IDTYPE := LSP; ALIGN(LC,LALN);
"**"                                VADDR := LC;   LC := LC+LEN;
"**"                          END;
"**"                          LCP4 := LCP4@.NEXT
"**"                        END;
                            INSYMBOL
                          END
                        ELSE ERROR(2);
                        IF NOT (SY IN FSYS + [SEMICOLON,RPARENT]) THEN
                          BEGIN ERROR(7);SKIP(FSYS+[SEMICOLON,RPARENT])END
                      END
                    ELSE ERROR(5);
                  END;
                IF SY = SEMICOLON THEN
                  BEGIN INSYMBOL;
                    IF NOT (SY IN FSYS + [IDENT,VARSY,PROCSY,FUNCSY]) THEN
                      BEGIN ERROR(7); SKIP(FSYS + [IDENT,RPARENT]) END
                  END;
"**"            IF LCP1 <> NIL THEN LCP2@.NEXT := LCP3
"**"            ELSE LCP1 := LCP3;
"**"            LCP2 := LCP;
              END (*WHILE*) ;
            IF SY = RPARENT THEN
              BEGIN INSYMBOL;
                IF NOT (SY IN FSY + FSYS) THEN
                  BEGIN ERROR(6); SKIP(FSY + FSYS) END
              END
            ELSE ERROR(4);
"SH"      END
"SH"    ELSE  (* IF SY <> LPARENT *)
"SH"      ;
"SH"
"SH"    IF NOT FW THEN FPAR@.PRMPTR := LCP1;
"SH"
"SH"    IF FPAR@.KLASS = FUNC THEN
"SH"      IF SY = COLON THEN
"SH"        BEGIN  INSYMBOL;
"SH"          IF SY = IDENT THEN
"SH"            BEGIN  IF FW THEN ERROR(122);
"SH"              SEARCHID( [TYPES], LCP1 );
"SH"              LSP := LCP1@.IDTYPE;
"SH"              FPAR@.IDTYPE := LSP;
"SH"              IF LSP <> NIL THEN
"SH"                IF LSP@.FORM >= POWER THEN
"SH"                  BEGIN  ERROR(120);  FPAR@.IDTYPE := NIL  END;
"SH"              INSYMBOL
"SH"            END
"SH"          ELSE BEGIN  ERROR(2);  SKIP( FSYS+[SEMICOLON] )  END
"SH"        END
"SH"      ELSE
"SH"        IF NOT FW THEN ERROR(123)

    END (*PARAMETERLIST*) ;

    BEGIN (*PROCDECLARATION*)
      LLC := LC; LC := LCAFTMST + FPSAVEAREA;  (* ADR. OF 1ST VAR OR PARM *)
"@@"  LCM := LCAFTMST;
#     LCP := UPRCPTR ;            (* TO INITIALIZE LCP IN CASE ! *)
      IF SY = IDENT THEN
"&&"    BEGIN  (* SEE IF PROC. ON FORWARD DECL. LIST *)
"&&"      FORW := FALSE;
"&&"      LCP := FWRDPRCL;  LCP2 := NIL;
"&&"      WHILE LCP <> NIL DO
"&&"        IF LCP@.NAME = ID THEN
"&&"          BEGIN  FORW := TRUE;
"&&"            IF LCP2 <> NIL THEN
"&&"              LCP2@.NXTFWRD := LCP@.NXTFWRD
"&&"            ELSE
"&&"              FWRDPRCL := LCP@.NXTFWRD;
"&&"            GOTO 10;
"&&"          END
"&&"        ELSE
"&&"          BEGIN  LCP2 := LCP;
"&&"            LCP := LCP@.NXTFWRD
"&&"          END;
   10:    IF NOT FORW THEN
            BEGIN
"SH"          IF FSY = PROCSY THEN NEW(LCP,PROC,DECLARED)
"SH"          ELSE NEW(LCP,FUNC,DECLARED);
              WITH LCP@ DO
"**"            BEGIN NAME := ID; IDTYPE := NIL;
#                 PFLEV := LEVEL; PROCLAB := PROCLAB+1 ;
#                 PFDECKIND := DECLARED; PFKIND := ACTUAL; PFNAME := PROCLAB;
"&&"              MKNAME(ID,PFNAME,XLINK);
"&&"              EXTRN := XLINK;   FRTRN := FALSE;  FWDECL := FALSE;
"&&"              PACK(ID,1,EXTNAME);
                  IF FSY = PROCSY THEN KLASS := PROC
                  ELSE KLASS := FUNC
                END;
              ENTERID(LCP);
"&&"          OLD_HASH := BUCKET;  (* NEW SCOPE BEGINS NEXT *)
            END
          ELSE
"SH"        BEGIN LCP1 := LCP@.PRMPTR;  LCP@.FWDECL := FALSE;
"&&"          OLD_HASH := BUCKET;  (* NEW SCOPE BEGINS NOW *)
"&&"          LEVEL := LEVEL+1;  TOP := LEVEL;
              WHILE LCP1 <> NIL DO
                BEGIN
"&&"              ENTERID(LCP1);  (* NAME NEEDS TO BE RE-ENTERED *)
                  WITH LCP1@ DO
                    IF KLASS = VARS THEN
                      IF IDTYPE <> NIL THEN
                        BEGIN
#                       IF VKIND = FORMAL THEN LCM := VADDR+PTRSIZE
                        ELSE LCM := VADDR + IDTYPE@.SIZE;
                        IF LCM > LC THEN LC := LCM
                        END;
                  LCP1 := LCP1@.NEXT
                END;
"&&"          LEVEL := LEVEL - 1;  TOP := LEVEL
            END;
          INSYMBOL
        END
      ELSE ERROR(2);
      OLDLABEL := INTLABEL ;  INTLABEL := 0 ;
      IF LEVEL<MAXLEVEL THEN LEVEL := LEVEL+1 ELSE FATALERROR(251);
"&&"  TOP := LEVEL;
      WITH DISPLAY[TOP] DO
"&&"    BEGIN OCCUR := BLCK; FLABEL := NIL END;
"SY"  IF GET_STAT THEN
"SY"    PROC_CNT[LEVEL] := PROC_CNT[LEVEL] + 1;
"SH"  IF FSY = PROCSY THEN  PARAMETERLIST( [SEMICOLON], LCP, FORW )
"SH"  ELSE PARAMETERLIST( [SEMICOLON,COLON], LCP, FORW );

"&&"  LCP@.FWDECL := FALSE;
      IF SY = SEMICOLON THEN INSYMBOL ELSE ERROR(14);
#     IF SY IN  [FORWARDSY,FRTRNSY,EXTRNSY]  THEN
#       BEGIN
#       IF SY = FORWARDSY THEN
"@@"      BEGIN IF FORW THEN ERROR(161);
"@@"            LCP@.FWDECL := TRUE;
"&&"            LCP@.NXTFWRD := FWRDPRCL;   (* LINK PROC. INTO *)
"&&"            FWRDPRCL := LCP;            (* FORWARD PROC. LIST *)
"@@"            INSYMBOL
"@@"      END
"@@"    ELSE BEGIN  (* SY MUST BE FRTRNSY OR EXTRNSY *)
"@@"        IF SY = FRTRNSY THEN LCP@.FRTRN := TRUE
"@@"                        ELSE LCP@.EXTRN := TRUE;
"@@"        INSYMBOL;
"&&"        WITH LCP@ DO
"@@"          IF SY = STRINGCONST THEN
"&&"            WITH VAL.VALP@ DO
"@@"              BEGIN
"&&"                WHILE LNGTH < EXTNAMSZ DO
"&&"                  BEGIN  LNGTH := LNGTH+1;  SVAL(/I/) := ' '  END;
"&&"                PACK( SVAL, 1, EXTNAME );
"@@"               INSYMBOL
"@@"             END
"@@"          ELSE
"&&"            PACK( NAME, 1, EXTNAME );
"@@"      END;
          IF SY = SEMICOLON THEN INSYMBOL ELSE ERROR(14);
          IF NOT (SY IN FSYS) THEN  BEGIN ERROR(6); SKIP(FSYS) END
        END
      ELSE
        BEGIN
"E"       IF DEBUG_LEV > 0 THEN  PRNTSYMBL(LCP);
          MARK(MARKP); (* MARK HEAP FOR BLOCK ENTRY *)
          REPEAT BLOCK(FSYS,SEMICOLON,LCP);
            IF SY = SEMICOLON THEN
              BEGIN INSYMBOL;
                IF NOT (SY IN [BEGINSY,PROCSY,FUNCSY]) THEN
                  BEGIN ERROR(6); SKIP(FSYS) END
              END
            ELSE ERROR(14)
          UNTIL SY IN [BEGINSY,PROCSY,FUNCSY];
          RELEASE(MARKP); (* RETURN LOCAL ENTRIES ON RUNTIME HEAP *)
        END;
"&&"  LEVEL := LEVEL-1; TOP := LEVEL; LC := LLC; INTLABEL := OLDLABEL ;
"&&"  BUCKET := OLD_HASH;  (*RESTORE SYMBOL TABLE*)
    END (*PROCDECLARATION*) ;

#
#     FUNCTION PROCTYPE(FPROCP: CTP): INTEGER ;
#
#     BEGIN   PROCTYPE := ORD('P') ;
#       IF FPROCP <> NIL THEN
#         WITH FPROCP@ DO
#           BEGIN  IF FRTRN THEN  PROCTYPE := ORD('F') ;
#           IF FPROCP@.IDTYPE <> NIL THEN
#           WITH FPROCP@ DO
#             BEGIN
#             IF IDTYPE = REALPTR THEN
#                     IF FRTRN THEN PROCTYPE := ORD('Z')
#                     ELSE PROCTYPE := ORD('R')
#               ELSE IF IDTYPE = BOOLPTR THEN
#                      IF FRTRN THEN PROCTYPE := ORD('X')
#                      ELSE PROCTYPE := ORD('B')
#                 ELSE IF IDTYPE@.FORM = POINTER THEN
#                   PROCTYPE := ORD('A')
#                   ELSE
#                       IF IDTYPE@.SIZE = 1 THEN PROCTYPE := ORD('C')
#                      ELSE  IF FRTRN THEN PROCTYPE := ORD('Y')
"&&"                         ELSE IF IDTYPE@.SIZE = HINTSIZE THEN
"&&"                            PROCTYPE := ORD('H')
"&&"                         ELSE  PROCTYPE := ORD('I');
#            END
#          END (*WITH FPROCP@*) ;
#     END (*PROCTYPE*) ;
#
    PROCEDURE BODY(FSYS: SETOFSYS);
#     CONST   CIXMAX = 400;
      TYPE OPRANGE = 0..OPMAX;
#
#         CALLED_PROC = RECORD
#                         NAME : ALPHA ;
#                         LVL  : LEVRANGE ;
#                         CNT  : 1..100 ;
#                         NXT  : @ CALLED_PROC
#                       END ;
#
#   VAR
#         CALL_HEAD, T2_CLIST, T_CLIST  : @ CALLED_PROC ;
#         LOCAL_CALL,                   (* THIS PROC CALLS A LOCAL PROC *)
#         MODIFYING : BOOLEAN ;         (* A PROGRAM VAR BEING MODIFIED*)
#         VAR_REF, VAR_MOD : INTEGER ;  (* # OF VARIABLES ACCESSED/REFERENCED*)
#         CNSTPTR:  CSP;
          I: INTEGER;
          LCMAX,LLC1: ADDRRANGE;  LCP,LLCP: CTP;
#         LLP: LBP;
"CT"      FIRSTLN : INTEGER;  CTRNO : CTRRANGE;



      PROCEDURE GEN0(FOP: OPRANGE);
      BEGIN
        IF PRCODE THEN BEGIN PUTIC; WRITELN(PRR,MN[FOP]:4) END;
        IC := IC + 1
      END (*GEN0*) ;

      PROCEDURE GEN1(FOP: OPRANGE; FP2: INTEGER);
        VAR K: INTEGER;
      BEGIN
        IF PRCODE THEN
          BEGIN PUTIC; WRITE(PRR,MN[FOP]:4);
            IF FOP = 30 THEN  (*CSP*)  WRITELN(PRR,SNA[FP2]:4)
            ELSE IF FOP = 37 THEN  (*LCA*)
"SH"          IF FP2 = ORD('P') THEN  (* LOAD PROCEDURE ADDRESS *)
"SH"            WRITELN(PRR, ' P,', ID:EXTNAMSZ)
"SH"          ELSE
"**"          IF FP2 <> ORD('S') THEN  (* CHAR STRING OPND *)
"**"               BEGIN WRITE(PRR,' M,''');
#                    WITH CNSTPTR@  DO
#                      BEGIN
#                      FOR K := 1 TO SLNGTH DO
#                        BEGIN  WRITE(PRR,SVAL[K]:1);
#                        IF SVAL[K] = '''' THEN WRITE(PRR,'''')
#                        END ;
#                      STIC := STIC+SLNGTH ;
#                      END ;
#                    WRITELN(PRR,'''')
                   END
"**"             ELSE  (* SET OPERAND *)
"**"               BEGIN  WRITE(PRR,' S,');
"&&"                 WRITESET( CNSTPTR );
"**"               END
                 ELSE IF (FOP = 26) OR (FOP = 42)
                         THEN  (*STO,RET*)
                        WRITELN(PRR,CHR(FP2):2)
                      ELSE WRITELN(PRR, ' ', FP2:1)
          END;
        IC := IC + 1
      END (*GEN1*) ;

      PROCEDURE GEN2(FOP: OPRANGE; FP1,FP2: INTEGER);
#       VAR I, J, K : INTEGER;
      BEGIN
        IF PRCODE THEN
          BEGIN PUTIC; WRITE(PRR,MN[FOP]:4,' ');
            CASE FOP OF
              22,23,35,39,43: (*DEC,INC,IND,LDO,SRO*)
                WRITELN(PRR,CHR(FP1),',',FP2:1 );
"&&"          29,41,50,58,68,69:  (*MST,LDA,SCL,NEW,SLD,SMV*)
                WRITELN(PRR,FP1:1,',',FP2:1);
              47,48,49,52,53,55: (*EQU..NEQ*)
                BEGIN WRITE(PRR,CHR(FP1));
                  IF FP1 = ORD('M') THEN WRITE(PRR,',',FP2:1);
                  WRITELN(PRR)
                END;
              51: (*LDC*)
                CASE FP1 OF
#                 0: WRITELN(PRR,'C,''',CHR(FP2):1,'''') ;
                  1: WRITELN(PRR,'I,',FP2:1);
                  2: BEGIN WRITE(PRR,'R,');
#                      WITH CNSTPTR@  DO
                         FOR K := 1 TO REALLNGTH DO
                           IF RVAL[K] <> ' ' THEN WRITE(PRR,RVAL[K]);
                       WRITELN(PRR)
                     END;
                  3: WRITELN(PRR,'B,',FP2:1);
                  4: WRITELN(PRR,'N');
"**"              OTHERWISE ERROR( 400 );
                END
            END;
          END;
          IC := IC + 1
      END (*GEN2*) ;

#     PROCEDURE GEN3(FOP: OPRANGE; FP0,FP1,FP2: INTEGER);
#     BEGIN
#       IF PRCODE THEN
#         BEGIN PUTIC;
"&&"        IF FOP = 76 (*PAK*) THEN
"&&"          WRITELN(PRR,MN(/FOP/):4,' ',FP0:1,' ',FP1:1,' ',FP2:1)
"&&"        ELSE
"&&"          WRITELN(PRR,MN(/FOP/):4,CHR(FP0):2,',',FP1:1,',',FP2:1)
"&&"      END;
#         IC := IC + 1
#     END (*GEN3*) ;

      PROCEDURE LOAD;
      BEGIN
        WITH GATTR DO
          IF TYPTR <> NIL THEN
            BEGIN
              CASE KIND OF
                CST:   IF (TYPTR@.FORM = SCALAR) AND (TYPTR <> REALPTR) THEN
                         IF TYPTR = BOOLPTR THEN GEN2(51(*LDC*),3,CVAL.IVAL)
                         ELSE
                          IF TYPTR = CHARPTR THEN GEN2(51(*LDC*),0,CVAL.IVAL)
                           ELSE GEN2(51(*LDC*),1,CVAL.IVAL)  (*INTEGER*)
                       ELSE
                         IF TYPTR = NILPTR THEN GEN2(51(*LDC*),4,0)
                         ELSE
#                            BEGIN
#                              CNSTPTR  := CVAL.VALP;
                               IF TYPTR = REALPTR THEN
#                                GEN2(51(*LDC*),2,0)
                               ELSE
#                                 GEN2(51(*LDC*),5,0)
                             END;
                VARBL: CASE ACCESS OF
#                        DRCT:   GEN3(54(*LOD*),GETTYPE(BTYPE),
                                         VLEVEL,DPLMT);
                         INDRCT: GEN2(35(*IND*),GETTYPE(BTYPE),IDPLMT);
"**"                     INXD,STKEXPR:   ERROR(400)
                       END;
                EXPR:
              END;
#             IF KIND = VARBL THEN VAR_REF := VAR_REF+1 ;
              KIND := EXPR
            END
      END (*LOAD*) ;

      PROCEDURE STORE(VAR FATTR: ATTR);
      BEGIN
        WITH FATTR DO
          IF TYPTR <> NIL THEN
            CASE ACCESS OF
#             DRCT:   GEN3(56(*STR*),GETTYPE(BTYPE),VLEVEL,DPLMT);
              INDRCT: IF IDPLMT <> 0 THEN ERROR(400)
                      ELSE GEN1(26(*STO*),GETTYPE(BTYPE));
"**"          INXD, STKEXPR:   ERROR(400)
            END
      END (*STORE*) ;

      PROCEDURE LOADADDRESS;
      BEGIN
        WITH GATTR DO
          IF TYPTR <> NIL THEN
            BEGIN
              CASE KIND OF
                CST:   IF STRING(TYPTR) THEN
#                        BEGIN
#                        CNSTPTR := CVAL.VALP ;  GEN1(37(*LCA*),ORD('M')) ;
#                        END
"**"                     ELSE IF TYPTR@.FORM = POWER THEN
"**"                       BEGIN  CNSTPTR := CVAL.VALP;
"**"                              GEN1(37(*LCA*),ORD('S'))
"**"                       END
                       ELSE ERROR(400);
                VARBL: CASE ACCESS OF
#                        DRCT:   GEN2(50(*LDA*),VLEVEL,DPLMT);
                         INDRCT: IF IDPLMT <> 0 THEN
                                    GEN2(23(*INC*),ORD('A'),IDPLMT);
                         INXD:   ERROR(400);
"**"                     STKEXPR: ;   (*SET ALREADY REPRESENTED BY AN ADDRESS*)
                       END;
                EXPR:  ERROR(400)
              END;
              KIND := VARBL; ACCESS := INDRCT; IDPLMT := 0
            END
      END (*LOADADDRESS*) ;


      PROCEDURE GENFJP(FADDR: INTEGER);
      BEGIN LOAD;
        IF GATTR.TYPTR <> NIL THEN
          IF GATTR.TYPTR <> BOOLPTR THEN ERROR(144);
        IF PRCODE THEN BEGIN PUTIC;
#         WRITELN(PRR,MN[33]:4,' L',FADDR:1) END;
        IC := IC + 1
      END (*GENFJP*) ;

      PROCEDURE GENUJPFJP(FOP: OPRANGE; FP2: INTEGER);
      BEGIN
        IF PRCODE THEN
#         BEGIN PUTIC; WRITELN(PRR, MN[FOP]:4, ' L',FP2:1) END ;
        IC := IC + 1
      END (*GENUJPFJP*);


      PROCEDURE GENDEF(L1, L2: ADDRRANGE ) ;
        BEGIN
        IF PRCODE THEN  WRITELN(PRR,'L', L1:1, MN[63(*DEF*)], L2:10);
        END (*GENDEF*) ;


#     PROCEDURE CHKBNDS(FSP: STP);
#       VAR LMIN,LMAX: INTEGER;
#     BEGIN
#       IF FSP <> NIL THEN
#         IF FSP <> BOOLPTR THEN
#           IF FSP <> INTPTR THEN
#             IF FSP <> REALPTR THEN
#               IF FSP@.FORM <= POINTER THEN
#                   IF FSP@.FORM = POINTER (*LMAX <= LMIN*) THEN
#                     BEGIN  FLIPDEBUG := TRUE;
#                     IF ASSIGN THEN  GEN3(45(*CHK*),ORD('A'),-1,0)
#                     ELSE (* ACCESS *)  GEN3(45(*CHK*),ORD('A'),0,0)
#                     END
#                   ELSE
#                     BEGIN
#                     GETBOUNDS(FSP,LMIN,LMAX);
#                     GEN3(45(*CHK*),ORD('I'),LMIN,LMAX) ;
#                     END ;
#     END (*CHKBNDS*);

      PROCEDURE PUTLABEL(LABNAME: INTEGER);
      BEGIN IF PRCODE THEN WRITELN(PRR, 'L', LABNAME:1, MN(/64/) (*LAB*) )
      END (*PUTLABEL*);
"CT"
"CT"
"CT"   FUNCTION CTRGEN : CTRRANGE;
"CT"
"CT"   BEGIN   (* CREATE A UNIQUE STMT COUNTER AND EMIT P-CODE TO INCREMENT IT*)
"CT"     (* R. L. SITES  3 AUG 77 *)
"CT"     CTRGEN := CTRCNT;
"CT"     IF CTROPTION THEN
"CT"             BEGIN
"CT"             GEN1(39(*CTI*), CTRCNT);
"CT"             CTRCNT := CTRCNT+1;
"CT"             END;
"CT"   END; (* CTRGEN *)
"CT"
"CT"   PROCEDURE CTREMIT(CTRT:CTRTYPE; CTRNO:CTRRANGE; FLN, MLN, LLN:INTEGER) ;
"CT"
"CT"     BEGIN   (* WRITE AN ENTRY DESCRIBING A STATEMENT COUNTER. *)
"CT"     (* R. L. SITES  3 AUG 77 *)
"CT"     IF CTROPTION THEN
"CT"       WRITELN(QRR, '#CTR    ', ORD(CTRT):4, CTRNO:6, FLN:7, MLN:7, LLN:7 );
"CT"   END; (* CTREMIT *)
"CT"
      PROCEDURE STATEMENT(FSYS: SETOFSYS);
        LABEL 1;
        VAR LCP: CTP; LLP: LBP; TTOP: DISPRANGE ;
"&&"        XLABEL: ALPHA;
"CT"        CTRNO : CTRRANGE;

        PROCEDURE EXPRESSION(FSYS: SETOFSYS); FORWARD;

"**"    PROCEDURE FORCETEMPSET;
"**"      (* "LOADS" CURRENT SET ONTO RUN-STACK *)
"**"      LABEL 10;  VAR LSIZE: ADDRRANGE;
"**"    BEGIN
"**"      WITH GATTR DO
"**"        IF TYPTR <> NIL THEN
"**"          IF TYPTR@.FORM = POWER (*REDUNDANT TEST?*) THEN
"**"            BEGIN
"**"              IF KIND = VARBL THEN
"**"                IF ACCESS = STKEXPR THEN GOTO 10;
"**"              LSIZE := OPNDSETSIZE( GATTR );
"**"              ALIGN( LC, WORDSIZE );
"**"              LOADADDRESS;
"**"              GEN2( 68 (*SLD*), LSIZE, LC );
"**"              KIND := VARBL;  ACCESS := STKEXPR;
"**"              STKLEN := LSIZE;  STKDPLMT := LC;
"**"              LC := LC + LSIZE;
"**"              IF LC > LCMAX THEN LCMAX := LC;
"**"            END;
"**"    10: END (* FORCETEMPSET *) ;

        PROCEDURE SELECTOR(FSYS: SETOFSYS; FCP: CTP);
          VAR LATTR: ATTR; LCP: CTP; LMIN,LMAX: INTEGER;
        BEGIN
          WITH FCP@, GATTR DO
"&&"        BEGIN TYPTR := IDTYPE; BTYPE := TYPTR; KIND := VARBL;
              CASE KLASS OF
                VARS:
                  IF VKIND = ACTUAL THEN
                    BEGIN ACCESS := DRCT; VLEVEL := VLEV;
                      DPLMT := VADDR
                    END
                  ELSE
                    BEGIN
#                     GEN3(54(*LOD*),ORD('A'),VLEV,VADDR);
                      ACCESS := INDRCT; IDPLMT := 0
                    END;
                FIELD:
                  WITH DISPLAY[DISX] DO
                    IF OCCUR = CREC THEN
                      BEGIN ACCESS := DRCT; VLEVEL := CLEV;
                        DPLMT := CDSPL + FLDADDR
                      END
                    ELSE
                      BEGIN
                        GEN3(54(*LOD*),ORD('A'), LEVEL,VDSPL)  ;
                        ACCESS := INDRCT; IDPLMT := FLDADDR
                      END;
"&&"            STRUCTKONST:
"&&"              BEGIN
"&&"                ID := '########    ';  LMAX := EXTNAMSZ;
"&&"                UNPACK( SKOWNER@.EXTNAME, ID, 1 );
"&&"                IF (SKOWNER <> MAINPROG) THEN
"&&"                  IF NOT SKOWNER@.EXTRN THEN LMAX := 5;
"&&"                REPEAT  ID(/LMAX/) := '#';  LMAX := LMAX - 1
"&&"                UNTIL  ID(/LMAX/) <> ' ';
"&&"                GEN1( 37(*LCA*), ORD('P') );
"&&"                ACCESS := INDRCT;  IDPLMT := SKADDR
"&&"              END;
                FUNC:
"&&"              IF FCP <> UFCTPTR THEN
                    IF PFDECKIND = STANDARD THEN ERROR(150)
                    ELSE
                    IF PFLEV = 0 THEN ERROR(150)   (*EXTERNAL FCT*)
                    ELSE
                      IF PFKIND = FORMAL THEN ERROR(151)
                      ELSE
#                       IF (FPROCP <> FCP) THEN  ERROR(177)
#                       ELSE
                          BEGIN ACCESS := DRCT; VLEVEL := PFLEV + 1;
                            DPLMT := FNCRSLT ;  (*RELAT. ADDR. OF FCT. RESULT*)
                          END
              END (*CASE*) ;
"&&"          IF TYPTR <> NIL THEN
"&&"            IF TYPTR@.FORM = SUBRANGE THEN
"&&"              TYPTR := TYPTR@.RANGETYPE;
            END (*WITH*);
          IF NOT (SY IN SELECTSYS + FSYS) THEN
            BEGIN ERROR(59); SKIP(SELECTSYS + FSYS) END;
          WHILE SY IN SELECTSYS DO
            BEGIN
"&&"          IF SY = LPARENT THEN  (* THIS IS AN ERROR, BUT .. *)
"&&"            BEGIN  SY := LBRACK;
"&&"              IF GATTR.TYPTR <> NIL THEN
"&&"                IF GATTR.TYPTR@.FORM = ARRAYS THEN
"&&"                  BEGIN  ERRKIND := 'W';  ERROR(11)  END
"&&"            END;
        (*[*)   IF SY = LBRACK THEN
                BEGIN
                  REPEAT LATTR := GATTR;
                    WITH LATTR DO
                      IF TYPTR <> NIL THEN
                        IF TYPTR@.FORM <> ARRAYS THEN
                          BEGIN ERROR(138); TYPTR := NIL END;
                    LOADADDRESS;
"&&"                INSYMBOL; EXPRESSION(FSYS + [COMMA,RBRACK,RPARENT]);
                    LOAD;
                    IF GATTR.TYPTR <> NIL THEN
                      IF GATTR.TYPTR@.FORM <> SCALAR THEN ERROR(113);
                    IF LATTR.TYPTR <> NIL THEN
                      WITH LATTR.TYPTR@ DO
                        BEGIN
                          IF COMPTYPES(INXTYPE,GATTR.TYPTR) THEN
                            BEGIN
                              IF INXTYPE <> NIL THEN
                                BEGIN GETBOUNDS(INXTYPE,LMIN,LMAX);
#                                 IF DEBUG THEN
#                                   GEN3(45(*CHK*),ORD('J'),LMIN,LMAX) ;
#                                 IF LMIN > 0 THEN
#                                   GEN2(22(*DEC*),GETTYPE(GATTR.TYPTR),LMIN)
#                                 ELSE IF LMIN < 0 THEN
                                     GEN2(23(*INC*),GETTYPE(GATTR.TYPTR),-LMIN)
#                                 (*OR SIMPLY GEN1(31,LMIN)*)
                                END
                            END
                          ELSE ERROR(139);
                          WITH GATTR DO
                            BEGIN TYPTR := AELTYPE; KIND := VARBL;
                              ACCESS := INDRCT; IDPLMT := 0 ;
                              IF GATTR.TYPTR <> NIL THEN
#                               BEGIN  LMIN := TYPTR@.SIZE ;
#                               ALIGN(LMIN,TYPTR@.ALN) ;
#                               GEN1(36(*IXA*),LMIN)
#                               END (*TYPTR <> NIL*) ;
                            END (*WITH GATTR DO*) ;
                        END
                  UNTIL SY <> COMMA;
                  IF SY = RBRACK THEN INSYMBOL ELSE
"&&"                BEGIN  IF SY = RPARENT THEN
"&&"                  BEGIN  ERRKIND := 'W';  INSYMBOL  END;
"&&"                  ERROR(12)
"&&"                END;
                END (*IF SY = LBRACK*)
              ELSE
        (*.*)     IF SY = PERIOD THEN
                  BEGIN
                    WITH GATTR DO
                      BEGIN
                        IF TYPTR <> NIL THEN
                          IF TYPTR@.FORM <> RECORDS THEN
                            BEGIN ERROR(140); TYPTR := NIL END;
                        INSYMBOL;
                        IF SY = IDENT THEN
                          BEGIN
                            IF TYPTR <> NIL THEN
"&&"                          BEGIN SEARCHSECTION(TYPTR,LCP);
                                IF LCP = NIL THEN
                                  BEGIN ERROR(152); TYPTR := NIL END
                                ELSE
                                  WITH LCP@ DO
                                    BEGIN TYPTR := IDTYPE;
                                      CASE ACCESS OF
                                        DRCT:   DPLMT := DPLMT + FLDADDR;
                                        INDRCT: IDPLMT := IDPLMT + FLDADDR;
"**"                                    INXD, STKEXPR:   ERROR(400)
                                      END
                                    END
                              END;
                            INSYMBOL
                          END (*SY = IDENT*)
                        ELSE ERROR(2)
                      END (*WITH GATTR*)
                  END (*IF SY = PERIOD*)
                ELSE
        (*@*)       BEGIN
                    IF GATTR.TYPTR <> NIL THEN
                      WITH GATTR,TYPTR@ DO
                        IF FORM = POINTER THEN
#                         BEGIN
#                         LOAD ;
#                         IF DEBUG THEN  CHKBNDS(GATTR.TYPTR) ;
#                         TYPTR := ELTYPE ;
                            WITH GATTR DO
                              BEGIN KIND := VARBL; ACCESS := INDRCT;
                                IDPLMT := 0
                              END
                          END
                        ELSE
"@@"                      IF FORM = FILES THEN
"@@"                        BEGIN
"@@"                          TYPTR := FILTYPE;
"@@"                          CASE ACCESS OF
"@@"                            DRCT:   DPLMT := DPLMT + FILHDRSIZE;
"@@"                            INDRCT: IDPLMT := IDPLMT + FILHDRSIZE;
"**"                            INXD, STKEXPR:   ERROR(400)
"@@"                          END
"@@"                        END
                          ELSE ERROR(141);
                    INSYMBOL
                  END;
              IF NOT (SY IN FSYS + SELECTSYS) THEN
                BEGIN ERROR(6); SKIP(FSYS + SELECTSYS) END ;
#             GATTR.BTYPE := GATTR.TYPTR ;
"&&"          IF GATTR.TYPTR <> NIL THEN
"&&"            IF GATTR.TYPTR@.FORM = SUBRANGE THEN
"&&"              GATTR.TYPTR := GATTR.TYPTR@.RANGETYPE;
            END (*WHILE*) ;
        END (*SELECTOR*) ;

        PROCEDURE CALL(FSYS: SETOFSYS; FCP: CTP);
"&&"      VAR LKEY: 0..NSPROC;  MATCHPAR: BOOLEAN;  RWFILE: STP;

          PROCEDURE VARIABLE(FSYS: SETOFSYS);
            VAR LCP: CTP;
          BEGIN
            IF SY = IDENT THEN
              BEGIN SEARCHID([VARS,FIELD],LCP); INSYMBOL END
            ELSE BEGIN ERROR(2); LCP := UVARPTR END;
            SELECTOR(FSYS,LCP)
          END (*VARIABLE*) ;


"@@"    PROCEDURE  RWSETUP(DFILE: CTP) ;
#       (* TO SET UP FILE ADDRESS PARAMETER FOR READ/WRITE *)
#
#         VAR  LCP : CTP ;  SAVED : BOOLEAN ; TEMPID : ALPHA ; TEMPSY : SYMBOL ;
#
"&&"      BEGIN  SAVED := TRUE;  RWFILE := NIL;
#
"&&"      IF MATCHPAR THEN  (* OTHERWISE THERE ARE NO PARAMETERS *)
#           IF SY = IDENT THEN
"&&"          BEGIN  SEARCHID([VARS,FIELD,FUNC,KONST,STRUCTKONST],LCP) ;
#             IF LCP@.IDTYPE <> NIL THEN
#               WITH LCP@.IDTYPE@ DO
#                 IF FORM = FILES THEN
"@@"                SAVED := FALSE;
#             END (* SY = IDENT *) ;
#
#         IF SAVED THEN (* USE IMPLIED FILE NAME *)
"@@"        BEGIN   TEMPSY := SY ;  TEMPID := ID ;  SY := COMMA ;
"@@"                LCP := DFILE;
#           END (* IF SAVED *)
#         ELSE  INSYMBOL ;
#
#         SELECTOR(FSYS+[COMMA,RPARENT],LCP) ;
"@@"      WITH GATTR DO
"@@"        IF NOT COMPTYPES( TYPTR, TEXTPTR ) THEN
"@@"          IF TYPTR <> NIL THEN
"@@"            IF TYPTR@.FORM <> FILES THEN ERROR(116)
"&&"            ELSE  BEGIN
"&&"              RWFILE := TYPTR@.FILTYPE;
"&&"              IF NOT (LKEY IN [1..6,25,36,37]) THEN ERROR(116);
"@@"              (* NON-TEXT FILES PERMITTED ONLY FOR:  GET, PUT, RESET,
"&&"                 READ, WRITE, REWRITE, EOF, SKIP, LINELIMIT               *)
"&&"               END;
#         LOADADDRESS ; (* GET FILE ADR *)
#         GEN1(30(*CSP*),31(*SIO*)) ;
#         IF SAVED THEN  BEGIN  ID := TEMPID ;  SY := TEMPSY  END ;
#         END (*RWSETUP*) ;
#

          PROCEDURE GETPUTRESETREWRITE;
          BEGIN
"@@"      IF ODD(LKEY) THEN RWSETUP( INPUTPTR )    (* GET, RESET *)
"@@"      ELSE              RWSETUP( OUTPUTPTR );  (* PUT, REWRITE, PAGE *)
#         GEN1(30(*CSP*),LKEY(*GET,PUT,RES,REW,PAG*)) ;
#         GEN1(30(*CSP*),32(*EIO*)) ;
          END (*GETPUTRESETREWRITE*) ;

          PROCEDURE READ1;
"&&"      VAR  CSPNO: 0..NSPROC;  TEST: BOOLEAN;
#         BEGIN (*ASSUME 'INPUT' FILE*)
"@@"        RWSETUP( INPUTPTR );
"&&"        IF RWFILE <> NIL THEN IF LKEY = 11 THEN ERROR(116);
"&&"        IF MATCHPAR THEN  (* OTHERWISE THERE ARE NO PARAMETERS *)
"&&"        BEGIN
              IF SY = COMMA THEN  INSYMBOL ;
#           IF LKEY = 5 (*READ*) THEN  IF SY <> IDENT THEN ERROR(2) ;
"&&"        TEST := FALSE;
            IF SY = IDENT THEN
              REPEAT
                VARIABLE(FSYS + [COMMA,RPARENT]) ;
                LOADADDRESS ;
                IF GATTR.TYPTR <> NIL THEN
"&&"              IF RWFILE = NIL THEN
#                   IF STRING(GATTR.TYPTR) THEN
#                     BEGIN
#                     GEN2(51(*LDC*),1,GATTR.TYPTR@.SIZE DIV CHARSIZE) ;
#                     CSPNO := 27 (*RDS*)
#                     END
#                   ELSE
#                     BEGIN
"&&"                  IF GATTR.TYPTR = INTPTR THEN
"&&"                    IF GATTR.BTYPE@.SIZE = INTSIZE THEN CSPNO := 24(*RDI*)
"&&"                    ELSE IF GATTR.BTYPE@.SIZE=HINTSIZE THEN CSPNO:=15(*RDH*)
"&&"                    ELSE CSPNO := 16 (*RDY - ONE BYTE INTEGER READ*)
                      ELSE
"&&"                    IF GATTR.TYPTR = REALPTR THEN CSPNO := 14 (*RDR*)
                        ELSE IF GATTR.TYPTR=CHARPTR THEN CSPNO := 5(*RDC*)
#                       ELSE IF GATTR.TYPTR=BOOLPTR THEN CSPNO:=12(*RDB*)
#                       ELSE BEGIN  ERROR(116); CSPNO := 24  END;
#                     END
"&&"              ELSE  (* NON-TEXT FILE INPUT *)
"&&"                BEGIN
"&&"                  IF NOT COMPTYPES( GATTR.TYPTR, RWFILE) THEN ERROR(153);
"&&"                  GEN2(51(*LDC*),1,GATTR.BTYPE@.SIZE);
"&&"                  CSPNO := 19 (*RDD*) ;
"&&"                  EXTUSED := TRUE;
"&&"                END;
"&&"            GEN1( 30 (*CSP*), CSPNO );
                IF SY = COMMA THEN INSYMBOL ELSE TEST := TRUE;
              UNTIL TEST ;
"&&"        END (*IF MATCHPAR*);
            IF LKEY = 11 THEN
                GEN1(30(*CSP*),23(*RLN*));
            GEN1(30(*CSP*),32(*EIO*)) ;
          END (*READ*) ;

          PROCEDURE WRITE1;
            VAR LSP: STP; DEFAULT, DEFAULT1, TEST: BOOLEAN;
"&&"            CSPNO, LLKEY: 0..NSPROC;  LEN: ADDRRANGE;
#         BEGIN LLKEY := LKEY;  TEST := FALSE ;
"@@"        RWSETUP( OUTPUTPTR );
"&&"        IF RWFILE <> NIL THEN IF LLKEY = 12 THEN ERROR(116);
"&&"        IF MATCHPAR THEN  (* OTHERWISE NO PARAMETERS *)
"&&"        BEGIN
#             IF SY = RPARENT THEN
#               IF LLKEY = 6 THEN ERROR(116) ;
#             IF SY = COMMA THEN
#             BEGIN INSYMBOL; IF NOT (SY IN SIMPTYPEBEGSYS) THEN ERROR(6)  END ;
#             IF SY IN SIMPTYPEBEGSYS THEN
#             REPEAT  EXPRESSION(FSYS+[COMMA,COLON,RPARENT]) ;
                LSP := GATTR.TYPTR;
                IF LSP <> NIL THEN
                  IF LSP@.FORM <= SUBRANGE THEN LOAD ELSE LOADADDRESS;
"&&"              IF RWFILE = NIL THEN
"&&"              BEGIN
                  DEFAULT := TRUE ;  DEFAULT1 := TRUE ;
                  IF SY = COLON THEN
                    BEGIN INSYMBOL; EXPRESSION(FSYS + [COMMA,COLON,RPARENT]);
"&&"                  LOAD;
                      IF GATTR.TYPTR <> NIL THEN
                        IF GATTR.TYPTR <> INTPTR THEN ERROR(116);
                      DEFAULT := FALSE ;
                      IF SY = COLON THEN
                        BEGIN  INSYMBOL;  EXPRESSION(FSYS + [COMMA,RPARENT]);
"&&"                      LOAD;
                          IF GATTR.TYPTR <> NIL THEN
                            IF GATTR.TYPTR <> INTPTR THEN ERROR(116);
                          IF LSP <> REALPTR THEN ERROR(124);
"&&"                      DEFAULT1 := FALSE ;
                        END ;
                    END ;
                  IF LSP = INTPTR THEN
                    BEGIN IF DEFAULT THEN GEN2(51(*LDC*),1,12);
"&&"                  CSPNO := 6 (*WRI*)
                    END
                  ELSE
                    IF LSP = REALPTR THEN
                      BEGIN IF DEFAULT THEN GEN2(51(*LDC*),1,14);
                        IF DEFAULT1 THEN GEN2(51(*LDC*),1,0);
"&&"                    CSPNO := 8 (*WRR*)
                      END
                    ELSE
                      IF LSP = CHARPTR THEN
                        BEGIN IF DEFAULT THEN GEN2(51(*LDC*),1,1);
"&&"                      CSPNO := 9 (*WRC*)
                        END
                      ELSE
#                     IF LSP = BOOLPTR THEN
#                       BEGIN IF DEFAULT THEN GEN2(51(*LDC*),1,5);
"&&"                      CSPNO := 13 (*WRB*)
#                       END
#                     ELSE
                        IF LSP <> NIL THEN
                          BEGIN
                            IF LSP@.FORM = SCALAR THEN ERROR(398)
                            ELSE
                              IF STRING(LSP) THEN
                                BEGIN LEN := LSP@.SIZE DIV CHARSIZE;
                                  IF DEFAULT THEN
                                        GEN2(51(*LDC*),1,LEN);
                                  GEN2(51(*LDC*),1,LEN);
"&&"                              CSPNO := 10 (*WRS*)
                                END
"&&"                        ELSE BEGIN  ERROR(116); CSPNO := 6  END
                          END
"&&"            END
"&&"            ELSE  (* NON-TEXT FILE *)
"&&"            BEGIN
"&&"              IF NOT COMPTYPES( LSP, RWFILE) THEN ERROR(145);
"&&"              GEN2(51(*LDC*),1,RWFILE@.SIZE);
"&&"              EXTUSED := TRUE;
"&&"              IF LSP <> NIL THEN
"&&"                IF LSP@.FORM <= SUBRANGE THEN  CSPNO := 7 (*WRE*)
"&&"                ELSE                           CSPNO := 20 (*WRD*)
"&&"            END;
"&&"            GEN1( 30 (*CSP*), CSPNO );
                IF SY = COMMA THEN INSYMBOL ELSE TEST := TRUE;
#             UNTIL TEST;
"&&"        END (*IF MATCHPAR*) ;
#
            IF LLKEY = 12 THEN (*WRITELN*)
                GEN1(30(*CSP*),22(*WLN*));
#           GEN1(30(*CSP*),32(*EIO*)) ;
          END (*WRITE*) ;
"NH"
"NH"      PROCEDURE SKIPLIM;
"NH"      BEGIN
"@@"        RWSETUP( OUTPUTPTR );
"NH"        IF SY = COMMA THEN
"NH"          BEGIN INSYMBOL;
"NH"            IF NOT (SY IN SIMPTYPEBEGSYS) THEN ERROR(6)
"NH"          END;
"NH"        IF SY IN SIMPTYPEBEGSYS THEN
"NH"          BEGIN
"NH"            EXPRESSION( FSYS + [RPARENT] );  LOAD;
"NH"            IF GATTR.TYPTR <> NIL THEN
"NH"               IF GATTR.TYPTR <> INTPTR THEN ERROR(125);
"NH"            GEN1( 30 (*CSP*), LKEY-2 (*SKP/LIM *) );
"@@"            GEN1( 30 (*CSP*), 32 (*EIO*) );
"NH"          END
"NH"      END;
"NH"
"NH"      PROCEDURE MESSAGE1;
"NH"      VAR LEN : INTEGER ;
"NH"      BEGIN
"NH"        EXPRESSION( FSYS + [RPARENT] );
"NH"        IF GATTR.TYPTR <> NIL THEN
"NH"           IF STRING(GATTR.TYPTR) THEN
"NH"              LEN := GATTR.TYPTR@.SIZE DIV CHARSIZE
"NH"           ELSE ERROR(125);
"NH"        LOADADDRESS;
"NH"        GEN2( 51 (*LDC*), 1, LEN );
"NH"        GEN1( 30 (*CSP*), 33 (*MSG*) );
"NH"      END;

          PROCEDURE PACK1;
            VAR  LSP,LSP1: STP;   LSIZE, IMIN, IMAX: INTEGER;
"&&"             LCNT, RCNT, LELEMSIZE, RELEMSIZE: INTEGER;
          BEGIN
#           EXPRESSION(FSYS + [COMMA, RPARENT]);
            LSP := NIL; LSP1 := NIL;
            IF GATTR.TYPTR <> NIL THEN
              WITH GATTR, GATTR.TYPTR@ DO
                IF FORM = ARRAYS THEN
                  BEGIN   LSP := INXTYPE;  LSP1 := AELTYPE;
"&&"              IMIN := 1;  LSIZE := SIZE;  IMAX := LSIZE;
#                 IF LSP <> NIL THEN  GETBOUNDS(LSP, IMIN, IMAX);
"&&"              LCNT := IMAX - IMIN + 1;
"&&"              LELEMSIZE := LSIZE DIV LCNT;
#                 LOADADDRESS;
#                 END
                ELSE ERROR(116);
            IF SY = COMMA THEN INSYMBOL ELSE ERROR(20);
            EXPRESSION(FSYS + [COMMA,RPARENT]);
            IF GATTR.TYPTR <> NIL THEN
              IF GATTR.TYPTR@.FORM <> SCALAR THEN ERROR(116)
              ELSE
                IF NOT COMPTYPES(LSP,GATTR.TYPTR) THEN ERROR(116)
#               ELSE
#                 BEGIN
#                 LOAD;
#                 IF DEBUG THEN GEN3(45(*CHK*),ORD('J'),IMIN,IMAX) ;
#                 IF IMIN > 0 THEN
#                    GEN2(22(*DEC*),GETTYPE(GATTR.TYPTR),IMIN)
#                 ELSE IF IMIN < 0 THEN
#                    GEN2(23(*INC*),GETTYPE(GATTR.TYPTR),-IMIN);
"&&"              GEN1( 36 (*IXA*), LELEMSIZE );
#                 END;
            IF SY = COMMA THEN INSYMBOL ELSE ERROR(20);
            VARIABLE(FSYS + [RPARENT]);
            IF GATTR.TYPTR <> NIL THEN
              WITH GATTR.TYPTR@ DO
                IF FORM = ARRAYS THEN
                  BEGIN
                    IF NOT COMPTYPES(AELTYPE,LSP1)
                      OR NOT COMPTYPES(INXTYPE,LSP) THEN  ERROR(116)
#                   ELSE
#                     BEGIN
#                     LOADADDRESS;
"&&"                  LSP := INXTYPE;  LSP1 := AELTYPE;
"&&"                  IF LSP <> NIL THEN GETBOUNDS( LSP, IMIN, IMAX )
"&&"                  ELSE BEGIN  IMIN := 1;  IMAX := SIZE  END;
"&&"                  RCNT := IMAX - IMIN + 1;
"&&"                  RELEMSIZE := SIZE DIV RCNT;
"&&"                  IF RCNT > LCNT THEN ERROR(303);
"&&"                  IF LELEMSIZE = RELEMSIZE THEN  (* A MOVE WORKS OK *)
"&&"                    GEN1( 40(*MOV*), -SIZE )
"&&"                  ELSE
"&&"                    GEN3( 76(*PAK*), RCNT, LELEMSIZE, RELEMSIZE );
#                     END;
                  END
                ELSE ERROR(116)
          END (*PACK*) ;

          PROCEDURE UNPACK1;
#           VAR  LSP,LSP1: STP;   IMIN, IMAX, LSIZE: INTEGER;
"&&"              LCNT, RCNT, LELEMSIZE, RELEMSIZE: INTEGER;
          BEGIN
#           EXPRESSION(FSYS + [COMMA, RPARENT]);
            LSP := NIL; LSP1 := NIL;
            IF GATTR.TYPTR <> NIL THEN
              WITH GATTR.TYPTR@ DO
                IF FORM = ARRAYS THEN
                  BEGIN LSP := INXTYPE; LSP1 := AELTYPE;
#                 IMIN := 1;  LSIZE := SIZE;  IMAX := LSIZE;
"&&"              IF LSP <> NIL THEN GETBOUNDS( LSP, IMIN, IMAX );
"&&"              LCNT := IMAX - IMIN + 1;
"&&"              LELEMSIZE := LSIZE DIV LCNT;
#                 LOADADDRESS;
                  END
                ELSE ERROR(116);
            IF SY = COMMA THEN INSYMBOL ELSE ERROR(20);
            VARIABLE(FSYS + [COMMA,RPARENT]);
            IF GATTR.TYPTR <> NIL THEN
              WITH GATTR, GATTR.TYPTR@ DO
                IF FORM = ARRAYS THEN
                  BEGIN
                    IF NOT COMPTYPES(AELTYPE,LSP1)
                      OR NOT COMPTYPES(INXTYPE,LSP) THEN  ERROR(116)
                    ELSE
#                     BEGIN
#                     IF INXTYPE <> NIL THEN GETBOUNDS(INXTYPE, IMIN,IMAX)
"&&"                  ELSE BEGIN  IMIN := 1;  IMAX := SIZE  END;
"&&"                  RCNT := IMAX - IMIN + 1;
"&&"                  RELEMSIZE := SIZE DIV RCNT;
"&&"                  IF LCNT > RCNT THEN ERROR(303);
#                     LOADADDRESS;
#                     END;
                  END
                ELSE ERROR(116);
            IF SY = COMMA THEN INSYMBOL ELSE ERROR(20);
            EXPRESSION(FSYS + [RPARENT]);
            IF GATTR.TYPTR <> NIL THEN
              IF GATTR.TYPTR@.FORM <> SCALAR THEN ERROR(116)
              ELSE
                IF NOT COMPTYPES(LSP,GATTR.TYPTR) THEN ERROR(116)
#               ELSE
#                 BEGIN
#                 LOAD;
#                 IF DEBUG THEN GEN3(45(*CHK*),ORD('J'),IMIN,IMAX) ;
#                 IF IMIN > 0 THEN
#                    GEN2(22(*DEC*),GETTYPE(GATTR.TYPTR),IMIN)
#                  ELSE IF IMIN < 0 THEN
#                    GEN2(23(*INC*),GETTYPE(GATTR.TYPTR),-IMIN);
"&&"               GEN1( 36(*IXA*), RELEMSIZE );
"&&"               IF LELEMSIZE = RELEMSIZE THEN  (* A MOVE IS OK *)
"&&"                 GEN1( 40(*MOV*), -LSIZE )
"&&"               ELSE
"&&"                 GEN3( 76(*PAK*), LCNT, LELEMSIZE, RELEMSIZE );
#                 END;
          END (*UNPACK*) ;

          PROCEDURE NEW1;
            LABEL 1;
            VAR LSP,LSP1: STP; VARTS,LMIN,LMAX: INTEGER;
                LSIZE,LSZ: ADDRRANGE; LVAL: VALU;
"&&"            LALN: ALNRNG;
          BEGIN VARIABLE(FSYS + [COMMA,RPARENT]); LOADADDRESS;
"&&"        LSP := NIL; VARTS := 0; LSIZE := 0;  LALN := INTSIZE;
            IF GATTR.TYPTR <> NIL THEN
              WITH GATTR.TYPTR@ DO
                IF FORM = POINTER THEN
                  BEGIN
                    IF ELTYPE <> NIL THEN
                      BEGIN LSIZE := ELTYPE@.SIZE;
"&&"                    IF ELTYPE@.ALN > INTSIZE THEN
"&&"                      LALN := REALSIZE;
                        IF ELTYPE@.FORM = RECORDS THEN LSP := ELTYPE@.RECVAR
                      END
                  END
                ELSE ERROR(116);
            WHILE SY = COMMA DO
              BEGIN INSYMBOL;CONSTANT(FSYS + [COMMA,RPARENT],LSP1,LVAL);
                VARTS := VARTS + 1;
                (*CHECK TO INSERT HERE: IS CONSTANT IN TAGFIELDTYPE RANGE*)
                IF LSP = NIL THEN ERROR(158)
                ELSE
                  IF LSP@.FORM <> TAGFLD THEN ERROR(162)
                  ELSE
                    IF LSP@.TAGFIELDP <> NIL THEN
                      IF STRING(LSP1) OR (LSP1 = REALPTR) THEN ERROR(159)
                      ELSE
                        IF COMPTYPES(LSP@.TAGFIELDP@.IDTYPE,LSP1) THEN
                          BEGIN
                            LSP1 := LSP@.FSTVAR;
                            WHILE LSP1 <> NIL DO
                              WITH LSP1@ DO
                                IF VARVAL.IVAL = LVAL.IVAL THEN
                                  BEGIN LSIZE := SIZE; LSP := SUBVAR;
                                    GOTO 1
                                  END
                                ELSE LSP1 := NXTVAR;
                            LSIZE := LSP@.SIZE; LSP := NIL;
                          END
                        ELSE ERROR(116);
          1:  END (*WHILE*) ;
"&&"        ALIGN(LSIZE,INTSIZE);
"&&"        GEN2(58(*NEW*),LSIZE,LALN);
          END (*NEW*) ;

"&&"      PROCEDURE MARKRELEASE;
          BEGIN VARIABLE(FSYS+[RPARENT]);
             IF GATTR.TYPTR <> NIL THEN
               IF GATTR.TYPTR@.FORM = POINTER THEN
"&&"             IF LKEY = 13 (*MARK*) THEN
                   BEGIN LOADADDRESS; GEN0(59(*SAV*)) END
"&&"             ELSE (* LKEY = 10, RELEASE *)
"&&"               BEGIN LOAD; GEN0(60(*RST*)) END
               ELSE ERROR(125)
          END(*MARKRELEASE*);

#         PROCEDURE TRAPEXIT ;
#
#         (*THIS PROCEDURE IS TO SIMPLIFY COMMUNICATION WITH THE OUTSIDE WORLD*)
#         (* AND PROVIDE BREAK POINTS IN THE PASCAL PROGRAM.                  *)
#         (* 'TRAP(I, R)'  RETURNS THE INTEGER CONSTANT I AS WELL AS A POINTER*)
#         (* TO THE SECOND PARAMETER 'R' (I.E. ADDRESS OF R) TO THE OPERATING *)
#         (* SYSTEM. THE FIRST PARAMETER IS INTENDED TO BE USED AS A          *)
#         (* 'FUNCTION NUMBER' AND THE SECOND ONE AS THE 'VAR' TYPE ARGUMENT  *)
#         (* WHICH MAY BE INSPECTED AND MODIFIED, TO THAT FUNCTION            *)
#
"**"         VAR  LLC: ADDRRANGE;
"**"        BEGIN  LLC := LC;  (* IN CASE OF SET TYPE ARGUMENT *)
#           IF GATTR.TYPTR <> INTPTR THEN  ERROR(116) ;
#           IF LKEY = 14 THEN (*TRAP*)
#             BEGIN
#             IF SY <> COMMA THEN  ERROR(6)
#             ELSE
#               BEGIN  INSYMBOL ;
#               EXPRESSION(FSYS+[RPARENT]) ;
#               WITH GATTR DO
#                 IF TYPTR <> NIL THEN
#                   BEGIN
#                   IF KIND <> VARBL THEN
"**"                  IF TYPTR@.FORM < POWER THEN
#                       BEGIN  LOAD ;
#                       KIND := VARBL ;  ACCESS := DRCT ;  VLEVEL := LEVEL ;
#                       ALIGN(LC,MXDATASZE) ;  DPLMT := LC ;  BTYPE := TYPTR ;
#                       STORE(GATTR) ;
#                       END ;
#                   LOADADDRESS ;
#                   END ;
#               END (*WITH*) ;
#             END ;
#           GEN1(30(*CSP*),LKEY+14 (*TRP*) (*XIT*)) ;
"**"        LC := LLC;
#           END (* TRAPEXIT *) ;

#         PROCEDURE SQRABS;
#         VAR OP : OPRANGE ;
#         BEGIN   OP := 0 (*ABI*) ;
#           IF LKEY = 17 THEN  OP := 24 (*SQI*) ;
#           IF GATTR.TYPTR <> NIL THEN
#             IF GATTR.TYPTR = INTPTR THEN GEN0(OP(*ABI*)(*SQI*))
#             ELSE
#               IF GATTR.TYPTR = REALPTR THEN GEN0(OP+1(*ABR*)(*ABR*))
#               ELSE BEGIN ERROR(125); GATTR.TYPTR := INTPTR END
#         END (*SQRABS*) ;

          PROCEDURE TRUNCROUND;
          BEGIN
            IF GATTR.TYPTR <> NIL THEN
              IF GATTR.TYPTR <> REALPTR THEN ERROR(125);
            GEN0(LKEY-18+27(*TRC*)(*RND*));
            GATTR.TYPTR := INTPTR
          END (*TRUNCROUND*) ;
"NH"
"NH"      PROCEDURE EXPO1;
"NH"      BEGIN
"NH"        IF GATTR.TYPTR <> NIL THEN
"NH"           IF GATTR.TYPTR = INTPTR THEN
"NH"              GEN0( 10 (*FLT*) )
"NH"           ELSE IF GATTR.TYPTR <> REALPTR THEN ERROR(125);
"NH"        GEN0( 66 (*XPO*) );  GATTR.TYPTR := INTPTR;
"NH"      END (*EXPO*);
"NH"
"NH"      PROCEDURE CARD1;
"**"      VAR  LLC, LEN: ADDRRANGE;
"NH"      BEGIN
"**"        LLC := LC;  EXPRESSION( FSYS + [RPARENT] );
"**"        IF GATTR.TYPTR <> NIL THEN
"**"          IF GATTR.TYPTR@.FORM = POWER THEN
"**"            BEGIN
"**"              FORCETEMPSET;  GEN0( 65 (*CRD*) );
"**"            END
"**"          ELSE ERROR( 125 );
"**"        LC := LLC;
"**"        GATTR.TYPTR := INTPTR;
"@@"      END  (*CARD*) ;

          PROCEDURE ODD1;
          BEGIN
            IF GATTR.TYPTR <> NIL THEN
              IF GATTR.TYPTR <> INTPTR THEN ERROR(125);
            GEN0(20(*ODD*));
            GATTR.TYPTR := BOOLPTR
          END (*ODD*) ;

          PROCEDURE ORD1;
          BEGIN
            IF GATTR.TYPTR <> NIL THEN
              IF GATTR.TYPTR@.FORM >= POWER THEN ERROR(125);
#           GEN0(61(*ORD*)) ;
            GATTR.TYPTR := INTPTR
          END (*ORD1*) ;

          PROCEDURE CHR1;
          BEGIN
            IF GATTR.TYPTR <> NIL THEN
              IF GATTR.TYPTR <> INTPTR THEN ERROR(125);
#           GEN0(62(*CHR*)) ;
            GATTR.TYPTR := CHARPTR
          END (*CHR*) ;

          PROCEDURE PREDSUCCTIM;
#         BEGIN  (*TRANSLATES INTO 'DEC' AND 'INC'*)
#           IF GATTR.TYPTR <> NIL THEN
"&&"           IF (LKEY = 24) OR (LKEY = 30) THEN
#                 BEGIN  IF GATTR.TYPTR <> INTPTR THEN  ERROR(116) ;
"&&"              IF LKEY=24 THEN GEN1(30(*CSP*),21(*CLK*))
"&&"              ELSE (*30*)     GEN1(30(*CSP*),36(*TRA*));
#                 END
#              ELSE
#               IF (GATTR.TYPTR = REALPTR) OR (GATTR.TYPTR@.FORM <> SCALAR) THEN
#                  ERROR(125)
#               ELSE  GEN2(LKEY(*DEC,INC*),GETTYPE(GATTR.TYPTR),1) ;
                (* LKEY HAPPENS TO BE THE OPCODE AS WELL *)
#         END (*PREDSUCCTIM*) ;

          PROCEDURE EOFEOLN;
          BEGIN
"@@"        RWSETUP( INPUTPTR );
            IF GATTR.TYPTR <> NIL THEN
              IF GATTR.TYPTR@.FORM <> FILES THEN ERROR(125);
"&&"        IF LKEY >= 28 THEN
"&&"          BEGIN  EXTUSED := TRUE;
"&&"            GEN1(30(*CSP*), LKEY-11(*EOL/EOT*) );
"&&"          END
"&&"        ELSE GEN1(30(*CSP*), LKEY(*EOF/ELN*)) ;
#           GEN1(30(*CSP*),32(*EIO*)) ;
            GATTR.TYPTR := BOOLPTR
          END (*EOF*) ;

          PROCEDURE CALLNONSTANDARD;
"&&"        LABEL 10;
"&&"        VAR NXT,LCP,LCP2: CTP;  LSP: STP;  LB: BOOLEAN;
"&&"            LOCPAR, LLC, LSIZE, LLC1, LLC2, LLC3, LLC4, LLC5: ADDRRANGE;
"@@"            I: INTEGER;  PROCNAME: ALPHA;

"SH"        FUNCTION COMPTLIST( CP1, CP2: CTP): BOOLEAN;
"SH"        (* MATCH PARAMETER LISTS CP1 AND CP2 *)
"SH"        VAR X: BOOLEAN;
"SH"        BEGIN
"SH"          WHILE (CP1 <> NIL) AND (CP2 <> NIL) DO
"SH"            BEGIN
"SH"            IF CP1@.KLASS IN [PROC,FUNC] THEN
"SH"              (* I.E. PROC PARAMETER *)
"SH"              IF (CP1@.KLASS = CP2@.KLASS) AND
"SH"                 (CP1@.IDTYPE = CP2@.IDTYPE) AND
"SH"                 (CP1@.PFDECKIND = CP2@.PFDECKIND) THEN
"SH"                X := COMPTLIST( CP1@.PRMPTR, CP2@.PRMPTR) AND
"SH"                     (CP1@.VKIND = CP2@.VKIND)
"SH"              ELSE
"SH"                X := FALSE
"SH"            ELSE
"SH"              X := COMPTYPES( CP1@.IDTYPE, CP2@.IDTYPE );
"SH"            IF X THEN
"SH"              BEGIN  CP1 := CP1@.NEXT;  CP2 := CP2@.NEXT  END
"SH"            ELSE
"SH"              CP1 := NIL;
"SH"          END  (* WHILE *);
"SH"        COMPTLIST := CP1 <> CP2;
"SH"      END  (*COMPTLIST*);

          BEGIN (*CALLNONSTANDARD*)  LOCPAR := 0;
"**"        LLC1 := LC;  ALIGN(LLC1,MXDATASZE);
            WITH FCP@ DO
"SH"          BEGIN NXT := PRMPTR;
"&&"            IF PFLEV > LCAFTMST THEN (* PROC. PARAMETER *)
"&&"              BEGIN  LLC5 := LLC1;
"&&"                     LLC1 := LLC1 + DISPAREA
"&&"              END
"&&"            ELSE LLC5 := 0;
"&&"            GEN2( 41(*MST*), PFLEV, LLC5 );
#               IF PFLEV = LEVEL THEN   LOCAL_CALL := TRUE ;
              END;
            IF SY = LPARENT THEN
"&&"          BEGIN  LSIZE := 0;  LLC := LLC1;  LLC2 := LLC1;
"**"            IF FCP@.FRTRN THEN
"**"              BEGIN  (* RESERVE STORAGE FOR COPIES OF PARMS *)
"**"                LCP := NXT;
"**"                WHILE LCP <> NIL DO
"**"                  BEGIN
"**"                    LSP := LCP@.IDTYPE;
"**"                    IF LSP<> NIL THEN
"**"                       IF LSP@.FORM <= POWER THEN
"**"                         BEGIN  ALIGN(LLC1,LSP@.ALN);
"**"                              LLC1 := LLC1 + LSP@.SIZE
"**"                         END;
"**"                     LCP := LCP@.NEXT
"**"                  END;
"&&"                ALIGN(LLC1,MXDATASZE);
"**"              END;
"**"            LLC3 := LLC1;
                REPEAT LB := FALSE; (*DECIDE WHETHER PROC/FUNC MUST BE PASSED*)
                  IF NXT = NIL THEN ERROR(126)
                  ELSE LB := NXT@.KLASS IN [PROC,FUNC];
                  (*FOR FORMAL PROC/FUNC LB IS FALSE AND EXPRESSION
                   WILL BE CALLED, WHICH WILL ALLWAYS INTERPRET A PROC/FUNC ID
                  AT ITS BEGINNING AS A CALL RATHER THAN A PARAMETER PASSING.
                  IN THIS IMPLEMENTATION, PARAMETER PROCEDURES/FUNCTIONS
                  ARE THEREFORE NOT ALLOWED TO HAVE PROCEDURE/FUNCTION
                  PARAMETERS*)
                  INSYMBOL;
                  IF LB THEN   (*PASS FUNCTION OR PROCEDURE*)
"SH"                BEGIN
                      IF SY <> IDENT THEN
                        BEGIN ERROR(2); SKIP(FSYS + [COMMA,RPARENT]) END
                      ELSE
                        BEGIN
"&&"                      SEARCHID( [NXT@.KLASS], LCP );
"SH"                      IF COMPTLIST( LCP, NXT ) THEN
"SH"                        BEGIN
"SH"                          LOCAL_CALL := TRUE;  (* => UPDATES DISP REGS *)
"SH"                          LLC4 := LLC1 + NXT@.PFLEV DIV 10;
"SH"                          (* PFLEV = ADDR OF PROC IN NEW ACTIV RECORD *)
"SH"                          LSIZE := DISPAREA;
"SH"                          IF LCP@.PFKIND = ACTUAL THEN
"SH"                            WITH LCP@ DO
"&&"                            BEGIN
"&&"                              IF FRTRN THEN
"&&"                                BEGIN  (* REMEMBER THIS PROC FOR LATER *)
"&&"                                  LCP2 := FRTPARHD;
"&&"                                  WHILE LCP2 <> NIL DO
"&&"                                    IF LCP2@.EXTNAME = EXTNAME THEN
"&&"                                      GOTO 10  (* ALREADY ON LIST *)
"&&"                                    ELSE LCP2 := LCP2@.NXTFWRD;
"&&"                                  NEW(LCP2,PROC,DECLARED);  LCP2@ := LCP@;
"&&"                                  WITH LCP2@ DO
"&&"                                    BEGIN  NAME := '            ';
"&&"                                      UNPACK(EXTNAME,NAME,1);
"&&"                                      PROCLAB := PROCLAB + 1;
"&&"                                      PFNAME := PROCLAB;
"&&"                                      NXTFWRD := FRTPARHD;
"&&"                                      FRTPARHD := LCP2;
"&&"                                    END;
"&&"                             10:  LCP := LCP2;
"&&"                                END;
"&&"                            UNPACK( EXTNAME, ID, 1 );
"SH"                            GEN1(37(*LCA*), ORD('P'));  (* PASSING PROC *)
"SH"                            GEN3(56(*STR*), ORD('A'), LEVEL, LLC4 );
"SH"                            GEN2(50(*LDA*), LEVEL, LLC4+PTRSIZE );
"SH"                            GEN2(50(*LDA*), 1, DISPADR+PTRSIZE );
"SH"                            GEN1(40(*MOV*), DISPAREA-PTRSIZE );
"SH"                            END
"SH"                          ELSE (* PROC PARM IS ITSELF A PASSED PROC *)
"SH"                            BEGIN
"SH"                            GEN2(50(*LDA*), LEVEL, LLC4 );
"SH"                            (* COPY ENTIRE PROC RECORD INTO PARM LIST *)
"SH"                            GEN2(50(*LDA*), LEVEL, LCP@.PFLEV DIV 10 );
"SH"                            GEN1(40(*MOV*), DISPAREA);
"SH"                            END;
"SH"                        END
"SH"                      ELSE ERROR(128);
                          INSYMBOL;
                          IF NOT (SY IN FSYS + [COMMA,RPARENT]) THEN
                            BEGIN ERROR(6); SKIP(FSYS + [COMMA,RPARENT]) END
                        END
                    END (*IF LB*)
                  ELSE
                    BEGIN
"**"                  IF NXT <> NIL THEN
"**"                    LC := LLC1 + NXT@.VADDR;
"**"                  LLC4 := LC;
                      EXPRESSION(FSYS + [COMMA,RPARENT]);
                      IF GATTR.TYPTR <> NIL THEN
                        BEGIN
                          IF NXT <> NIL THEN
                            BEGIN LSP := NXT@.IDTYPE;
                              IF LSP <> NIL THEN
                                BEGIN
                                  IF (NXT@.VKIND = ACTUAL) THEN
#                                   IF LSP@.FORM < POWER THEN
#                                     BEGIN LOAD;
#                                     IF DEBUG THEN
#                                       BEGIN  ASSIGN := TRUE ;
#                                       CHKBNDS(LSP) ;  ASSIGN := FALSE ;
#                                       END ;
                                      IF COMPTYPES(REALPTR,LSP) THEN
                                        IF (GATTR.TYPTR = INTPTR) THEN
                                        BEGIN GEN0(10(*FLT*));
                                          GATTR.TYPTR := REALPTR ;
                                          GATTR.BTYPE := REALPTR ;
                                        END;
                                      LOCPAR := LOCPAR+ 1 (*LSP@.SIZE*) ;
#                                     IF FCP@.FRTRN THEN
"**"                                  BEGIN
"**"                                    ALIGN(LLC2,LSP@.ALN);
"**"                                    WITH GATTR DO
"**"                                    BEGIN  VLEVEL:=LEVEL; DPLMT:=LLC2;
"&&"                                           BTYPE := LSP;
"**"                                           KIND := VARBL; ACCESS:=DRCT
"**"                                    END;
"**"                                    STORE(GATTR);  LOADADDRESS;
"**"                                    GEN3(56(*STR*),ORD('A'),LEVEL,LLC3);
"**"                                    LLC3 := LLC3 + PTRSIZE;
"**"                                    LLC2 := LLC2 + LSP@.SIZE;
"**"                                  END
"**"                                  ELSE
"**"                                    GEN3(56,GETTYPE(LSP),LEVEL,LLC4);
                                      END
"**"                                ELSE  (* LSP@.FORM >= POWER *)
"**"                                  BEGIN  LOCPAR := LOCPAR + 1;
"**"                                    IF FCP@.FRTRN THEN
"**"                                    BEGIN
"**"                                    IF (LSP@.FORM=POWER) AND
"**"                                       (GATTR.ACCESS=STKEXPR) THEN
"**"                                    BEGIN ALIGN(LLC2,LSP@.ALN);
"**"                                     FORCETEMPSET;
"**"                                     LSIZE:=OPNDSETSIZE(GATTR);
"**"                                     GEN2(50(*LDA*),LEVEL,LLC2);
"**"                                     GEN2(69(*SMV*),-LSP@.SIZE,LSIZE);
"**"                                     GEN2(50(*LDA*),LEVEL,LLC2);
"**"                                     LLC2 := LLC2 + LSP@.SIZE;
"**"                                    END
"**"                                    ELSE LOADADDRESS;
"**"                                      GEN3(56(*STR*),ORD('A'),LEVEL,LLC3);
"**"                                      LLC3 := LLC3 + PTRSIZE;
"**"                                    END
"**"                                    ELSE IF LSP@.FORM = POWER THEN
"**"                                    BEGIN
"**"                                      LSIZE := OPNDSETSIZE(GATTR);
"**"                                      LOADADDRESS;
"**"                                      GEN2(50(*LDA*),LEVEL,LLC4);
"**"                                      GEN2(69(*SMV*),-LSP@.SIZE,LSIZE);
"**"                                    END
"**"                                    ELSE BEGIN
"**"                                      LOADADDRESS;
"**"                                      GEN2(50(*LDA*),LEVEL,LLC4);
"**"                                      GEN1(40(*MOV*),-LSP@.SIZE);
"**"                                    END
                                      END
                                  ELSE  (* VKIND = FORMAL I.E. VAR PARM *)
                                    IF GATTR.KIND = VARBL THEN
                                      BEGIN  LOADADDRESS;
"**"                                  IF NOT FCP@.FRTRN THEN
"**"                                    GEN3(56,ORD('A'),LEVEL,LLC4)
"**"                                  ELSE BEGIN
"**"                                    GEN3(56,ORD('A'),LEVEL,LLC3);
"**"                                    LLC3:=LLC3+PTRSIZE;
"**"                                  END;
"**"                                  IF GATTR.ACCESS = STKEXPR THEN
"**"                                    ERROR(154);
                                      LOCPAR := LOCPAR + 1 (*PTRSIZE*);
                                      IF GATTR.BTYPE@.SIZE <> LSP@.SIZE THEN
                                        ERROR(142) ;
                                      END
                                    ELSE ERROR(154);
"**"                              IF LSP <> NIL THEN LSIZE := LSP@.SIZE;
                                  IF NOT COMPTYPES(LSP,GATTR.TYPTR) THEN
                                    ERROR(142)
                                END
                            END
                        END
                    END;
"SH"              IF (NXT <> NIL) THEN NXT := NXT@.NEXT
                UNTIL SY <> COMMA;
"**"            LC := LLC4 + LSIZE;
"**"            IF LC > LCMAX THEN LCMAX := LC;
                LC := LLC;
              IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4)
            END (*IF LPARENT*);
#           LOCPAR := LOCPAR*2 ;
            IF NXT <> NIL THEN ERROR(126);
            WITH FCP@ DO
#             BEGIN
"**"            IF SAVEFPRS THEN LOCPAR := LOCPAR+1;  (*ENCODE SAVEFPR FLG*)
"&&"            IF PRCODE THEN
"&&"              BEGIN  PUTIC;  WRITELN(PRR, MN(/46/) (*CUP*),
"&&"                CHR(PROCTYPE(FCP)):2, ',', LOCPAR:1, ',', EXTNAME,
"&&"                ',', LLC1:1 );
"&&"              END;
              END;
"&&"        WITH GATTR DO
"&&"          BEGIN  TYPTR := FCP@.IDTYPE;  BTYPE := TYPTR;
"&&"            IF TYPTR <> NIL THEN
"&&"              IF TYPTR@.FORM = SUBRANGE THEN
"&&"                TYPTR := TYPTR@.RANGETYPE
"&&"          END;
          END (*CALLNONSTANDARD*) ;

        BEGIN (*CALL*)
          IF FCP@.PFDECKIND = STANDARD THEN
            BEGIN
            LKEY := FCP@.KEY;
#           IF SY = LPARENT THEN
#             BEGIN  INSYMBOL ;   MATCHPAR := TRUE ;
#               IF SY = RPARENT THEN
#                 IF NOT (LKEY IN [0,1,2,3,4,11,12,25,26,28,29]) THEN ERROR(7) ;
"&&"               (*GET,PUT,RESET,REWRITE,RDLN,WRITELN,EOL,EOT,EOF,ELN*)
#             END
#           ELSE
"&&"          BEGIN  IF NOT (LKEY IN [0,1,2,3,4,11,12,25,26,28,29]) THEN
"&&"                    ERROR(7);
"&&"                    (*GET,PUT,RESET,REWRITE,RDLN,WRITELN,EOL,EOT,EOF,ELN*)
#               MATCHPAR := FALSE ;
              END;

"&&"        IF LKEY IN [14..24,30,33,39] THEN
"&&"           (*TRAP,EXIT,ABS...,TRACE,ODD,EXPO*)
#             BEGIN
                IF LKEY = 14 (*TRAP*) THEN  EXPRESSION(FSYS+[COMMA])
                ELSE  EXPRESSION(FSYS+[RPARENT]) ;
                LOAD ;
              END  ;
#
#             CASE LKEY OF
#           0,1,2,
#             3,4:    GETPUTRESETREWRITE;
#            5,11:    READ1;
#            6,12:    WRITE1;
#               7:    PACK1;
#               8:    UNPACK1;
#               9:    NEW1;
"&&"        10,13:    MARKRELEASE;
#           14,15:    TRAPEXIT;
#           16,17:    SQRABS;
#           18,19:    TRUNCROUND;
#              20:    ORD1;
#              21:    CHR1;
"&&"  22,23,24,30:    PREDSUCCTIM;
"&&"  25,26,28,29:    EOFEOLN ;
#              33:    ODD1;
"NH"           35:    MESSAGE1;
"NH"        36,37:    SKIPLIM;
"NH"           38:    CARD1;
"NH"           39:    EXPO1;
              END (*CASE LKEY OF*) ;

"&&"        IF LKEY IN [16..26,28,29,33,38,39] THEN
"NH"           GATTR.BTYPE := GATTR.TYPTR;
#           IF MATCHPAR THEN
#             IF SY = RPARENT THEN INSYMBOL ELSE  ERROR(4) ;

            END (*IF FCP@.PFDECKIND = STANDARD*)

          ELSE CALLNONSTANDARD

        END (*CALL*) ;

"**"    PROCEDURE GENSETOP( LATTR: ATTR;  OP: OPRANGE);
"**"      BEGIN
"**"        WITH GATTR DO
"**"          IF (TYPTR<>NIL) AND (LATTR.TYPTR<>NIL) THEN
"**"            IF (TYPTR@.FORM = POWER) AND
"**"               COMPTYPES( TYPTR, LATTR.TYPTR ) THEN
"**"              BEGIN
"**"                FORCETEMPSET;
"**"                GEN0( OP );
"**"                IF OP = 12 THEN IF LATTR.STKLEN < STKLEN THEN
"**"                   STKLEN := LATTR.STKLEN;
"**"                IF OP = 31 THEN IF LATTR.STKLEN > STKLEN THEN
"**"                   STKLEN := LATTR.STKLEN;
"**"                IF OP = 5 THEN STKLEN := LATTR.STKLEN;
"**"                STKDPLMT := LATTR.STKDPLMT;
"**"                LC := STKDPLMT + STKLEN;
"**"                IF LC > LCMAX THEN LCMAX := LC;
"**"              END
"**"            ELSE BEGIN
"&&"                ERROR( 134 );  GATTR.TYPTR := NIL; GATTR.BTYPE := NIL
"**"              END
"**"      END  (* GENSETOP *) ;

        PROCEDURE EXPRESSION;
"&&"      CONST COMPARE_OP: ARRAY(/LTOP..EQOP/) OF 0..OPMAX
"&&"                        = (53, 52, 48, 49, 55, 47);
          VAR LATTR: ATTR; LOP: OPERATOR; TYPIND: CHAR;
"**"          LLC,LSIZE: ADDRRANGE;

          PROCEDURE SIMPLEEXPRESSION(FSYS: SETOFSYS);
            VAR LATTR: ATTR; LOP: OPERATOR; SIGNED: BOOLEAN;

            PROCEDURE TERM(FSYS: SETOFSYS);
              VAR LATTR: ATTR; LOP: OPERATOR;

              PROCEDURE FACTOR(FSYS: SETOFSYS);
                VAR LCP: CTP; LVP: CSP; VARPART: BOOLEAN;
#                   LATTR: ATTR;
"**"                CSTPART: SETRANGE  ; LSP: STP;   I,J: INTEGER;
"**"                TS_LC, TS_SIZE: ADDRRANGE;  MAXELEM: INTEGER;
              BEGIN
                IF NOT (SY IN FACBEGSYS) THEN
                  BEGIN ERROR(58); SKIP(FSYS + FACBEGSYS);
                    GATTR.TYPTR := NIL
                  END;
                WHILE SY IN FACBEGSYS DO
                  BEGIN
                    CASE SY OF
              (*ID*)    IDENT:
"&&"                    BEGIN SEARCHID([STRUCTKONST,KONST,VARS,FIELD,FUNC],LCP);
                          INSYMBOL;
                          IF LCP@.KLASS = FUNC THEN
#                           BEGIN CALL(FSYS,LCP);
"&&"                          GATTR.KIND := EXPR
#                           END
                          ELSE
                            IF LCP@.KLASS = KONST THEN
                              WITH GATTR, LCP@ DO
                                BEGIN TYPTR := IDTYPE; KIND := CST;
"&&"                              CVAL := VALUES;
"&&"                              IF SY IN (/LBRACK,LPARENT/) THEN
"&&"                                IF STRING(TYPTR) THEN
"&&"                                  BEGIN
"&&"                                    IF SY=LPARENT THEN BEGIN
"&&"                                      ERRKIND:='W'; ERROR(11) END;
"&&"                                    LATTR := GATTR;  LOADADDRESS;
"&&"                                    INSYMBOL;
"&&"                                    EXPRESSION(FSYS+(/RBRACK,RPARENT/));
"&&"                                    LOAD;
"&&"                                    LSP := LATTR.TYPTR@.INXTYPE;
"&&"                                    IF COMPTYPES(TYPTR,LSP) THEN
"&&"                                      BEGIN
"&&"                                       IF LSP <> NIL THEN
"&&"                                         GETBOUNDS(LSP,I,J)
"&&"                                       ELSE BEGIN I:=1; J:=LATTR.TYPTR@
"&&"                                                           .SIZE END;
"&&"                                        IF DEBUG THEN GEN3(45(*CHK*),
"&&"                                           ORD('J'),I,J DIV CHARSIZE);
"&&"                                        IF I <> 0 THEN
"&&"                                          GEN2(22(*DEC*),ORD('I'),I);
"&&"                                        GEN1(36(*IXA*),CHARSIZE);
"&&"                                      END
"&&"                                    ELSE ERROR(139);
"&&"                                    TYPTR := CHARPTR;  KIND := VARBL;
"&&"                                    BTYPE := CHARPTR;
"&&"                                    ACCESS := INDRCT;  IDPLMT := 0;
"&&"                                    IF SY = RBRACK THEN INSYMBOL ELSE
"&&"                                      IF SY = RPARENT THEN BEGIN
"&&"                                        ERRKIND:='W'; ERROR(12);
"&&"                                        INSYMBOL;  END
"&&"                                      ELSE ERROR(12);
"&&"                                  END
                                END
                            ELSE
                              SELECTOR(FSYS,LCP)
                        END;
              (*CST*)   INTCONST:
                        BEGIN
                          WITH GATTR DO
                            BEGIN TYPTR := INTPTR; KIND := CST;
"&&"                          CVAL := VAL
                            END;
                          INSYMBOL
                        END;
                      REALCONST:
                        BEGIN
                          WITH GATTR DO
                            BEGIN TYPTR := REALPTR; KIND := CST;
"&&"                          CVAL := VAL
                            END;
                          INSYMBOL
                        END;
                      STRINGCONST:
                        BEGIN
                          WITH GATTR DO
                            BEGIN
                              IF LNGTH = 1 THEN TYPTR := CHARPTR
                              ELSE
                                BEGIN NEW(LSP,ARRAYS);
                                  WITH LSP@ DO
                                    BEGIN AELTYPE := CHARPTR; FORM:=ARRAYS;
                                      INXTYPE := NIL; SIZE := LNGTH*CHARSIZE;
"&&"                                  ALN := CHARSIZE;
                                    END;
                                  TYPTR := LSP
                                END;
"&&"                          KIND := CST; CVAL := VAL;
                            END;
                          INSYMBOL
                        END;
              (* ( *)   LPARENT:
                        BEGIN INSYMBOL; EXPRESSION(FSYS + [RPARENT]);
                          IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4)
                        END;
              (*NOT*)   NOTSY:
                        BEGIN INSYMBOL; FACTOR(FSYS);
                          LOAD; GEN0(19(*NOT*));
                          IF GATTR.TYPTR <> NIL THEN
                            IF GATTR.TYPTR <> BOOLPTR THEN
                              BEGIN ERROR(135); GATTR.TYPTR := NIL END;
                        END;
              (*[*)     LBRACK:
"**"                    BEGIN INSYMBOL; VARPART := FALSE; MAXELEM := -1;
"**"                      FOR I:=1 TO (SETMAX+1) DIV (SSETMAX+1) DO
"**"                        CSTPART[I] := [];
                          NEW(LSP,POWER);
                          WITH LSP@ DO
"**"                        BEGIN ELSET:=NIL;SIZE:=0;FORM:=POWER END;
                          IF SY = RBRACK THEN
                            BEGIN
                              WITH GATTR DO
                                BEGIN TYPTR := LSP; KIND := CST END;
                              INSYMBOL
                            END
                          ELSE
                            BEGIN
#                             REPEAT EXPRESSION(FSYS + [COMMA,DOTDOT,RBRACK]);
                                IF GATTR.TYPTR <> NIL THEN
                                  IF GATTR.TYPTR@.FORM <> SCALAR THEN
                                    BEGIN ERROR(136); GATTR.TYPTR := NIL END
                                  ELSE
                                    IF COMPTYPES(LSP@.ELSET,GATTR.TYPTR) THEN
                                      BEGIN
"**"                                    LSP@.ELSET := GATTR.TYPTR;
                                        IF GATTR.KIND = CST THEN
#                                         BEGIN
#                                         LATTR := GATTR ;
#                                         IF SY = DOTDOT THEN   (*RANGE GIVEN*)
#                                           BEGIN  INSYMBOL ;
#                                           EXPRESSION(FSYS+[COMMA,RBRACK]) ;
"@@"                                        IF GATTR.KIND<>CST THEN
"@@"                                        BEGIN GATTR:=LATTR; ERROR(305) END;
#                                           END ;
#                                         IF GATTR.TYPTR <> LATTR.TYPTR THEN
#                                             ERROR(137)
#                                         ELSE
#
#                                           IF (LATTR.CVAL.IVAL < 0)         OR
#                                              (GATTR.CVAL.IVAL > SETMAX  )  OR
#                                              (LATTR.CVAL.IVAL>GATTR.CVAL.IVAL)
#                                             THEN  ERROR(304)
"**"                                        ELSE BEGIN
"**"                                         IF GATTR.CVAL.IVAL>MAXELEM THEN
"**"                                         BEGIN
"**"                                         IF GATTR.CVAL.IVAL>SETMAX THEN
"**"                                         BEGIN  ERROR( 304 );
"**"                                           GATTR.CVAL.IVAL := SETMAX;
"**"                                         END;
"**"                                         MAXELEM := GATTR.CVAL.IVAL
"**"                                         END;
"**"                                         FOR I := LATTR.CVAL.IVAL TO
"**"                                                  GATTR.CVAL.IVAL DO
"**"                                           BEGIN
"**"                                           J := I DIV (SSETMAX+1);
"**"                                           CSTPART[J+1] := CSTPART[J+1]
"**"                                                   + [I - J*(SSETMAX+1)];
"**"                                           END;
"**"                                         END
"**"                                       END  (*GATTR.KIND = CST *)
"**"                                    ELSE
"**"                                    BEGIN  LOAD;
"&&"                                     IF GATTR.TYPTR <> INTPTR THEN
"**"                                       GEN0(61(*ORD*));
"**"                                     IF NOT VARPART THEN
"**"                                       BEGIN  (* ALLOCATE STORAGE *)
"**"                                       TS_SIZE := MAXSETSIZE;
"**"                                       IF GATTR.TYPTR <> NIL THEN
"&&"                                        IF GATTR.TYPTR <> INTPTR THEN
"**"                                          TS_SIZE :=
"**"                                               CALC_SETSIZE(GATTR.TYPTR);
"**"                                        IF TS_SIZE > MAXSETSIZE THEN
"**"                                          TS_SIZE := MAXSETSIZE;
"**"                                        ALIGN(LC,WORDSIZE);  TS_LC := LC;
"**"                                        GEN2(29(*SCL*),TS_SIZE,LC);
"**"                                        LC := LC + TS_SIZE;
"**"                                        IF LC>LCMAX THEN LCMAX := LC;
"**"                                        VARPART := TRUE;
"**"                                        GEN1(67(*ASE*),-TS_SIZE);
"**"                                       END
"**"                                     ELSE GEN1(67(*ASE*),TS_SIZE);
"**"                                    END
"**"                                  END
"**"                                ELSE ERROR( 137 );
                                TEST := SY <> COMMA;
                                IF NOT TEST THEN INSYMBOL
                              UNTIL TEST;
                              IF SY = RBRACK THEN INSYMBOL ELSE ERROR(12)
                            END;
                          IF VARPART THEN
                            BEGIN
"**"                          IF MAXELEM >= 0 THEN
"**"                            BEGIN  NEW(LVP,PSET);
"**"                              LVP@.PVAL := CSTPART;
"**"                              LVP@.PLNGTH := ((MAXELEM+SETPACK) DIV
"**"                                              SETPACK) * WORDSIZE;
"**"                              CNSTPTR := LVP;
"**"                              ALIGN( LC, WORDSIZE );
"**"                              GEN1(37(*LCA*),ORD('S'));
"**"                              GEN2(68(*SLD*),LVP@.PLNGTH,LC);
"**"                              GEN0( 31 (*UNI*) );
"**"                              IF LVP@.PLNGTH > TS_SIZE THEN
"**"                                TS_SIZE := LVP@.PLNGTH;
"**"                              IF (TS_SIZE+LC) > LCMAX THEN
"**"                                LCMAX := TS_SIZE + LC;
"**"                            END;
"&&"                          GATTR.KIND := VARBL;
"**"                          GATTR.ACCESS := STKEXPR;
"**"                          GATTR.STKDPLMT := TS_LC;
"**"                          GATTR.STKLEN := TS_SIZE;
"**"                          LSP@.SIZE := TS_SIZE;
"**"                        END
                          ELSE
                            BEGIN NEW(LVP,PSET); LVP@.PVAL := CSTPART;
"**"                         LVP@.PLNGTH := ((MAXELEM+SETPACK) DIV
"**"                                          SETPACK) * WORDSIZE;
"**"                         LSP@.SIZE := LVP@.PLNGTH;
"**"                         GATTR.KIND := CST;
"**"                         GATTR.CVAL.VALP := LVP;
"**"                       END;
"**"                     GATTR.TYPTR := LSP;
                        END
                    END (*CASE*) ;
                    IF NOT (SY IN FSYS) THEN
                      BEGIN ERROR(6); SKIP(FSYS + FACBEGSYS) END;
"&&"              IF GATTR.KIND <> VARBL THEN
"&&"                GATTR.BTYPE := GATTR.TYPTR
"&&"              ELSE IF GATTR.TYPTR = NIL THEN
"&&"                  GATTR.BTYPE := NIL;
                  END (*WHILE*)
              END (*FACTOR*) ;

            BEGIN (*TERM*)
              FACTOR(FSYS + [MULOP]);
              WHILE SY = MULOP DO
"**"            BEGIN
"**"              IF GATTR.TYPTR <> NIL THEN
"**"                IF GATTR.TYPTR@.FORM < POWER THEN
"**"                  LOAD
"**"                ELSE FORCETEMPSET;
"**"              LATTR := GATTR;  LOP := OP;
"**"              INSYMBOL; FACTOR(FSYS + [MULOP] );
"**"              IF GATTR.TYPTR <> NIL THEN
"**"                IF GATTR.TYPTR@.FORM < POWER THEN
"**"                  LOAD;
                  IF (LATTR.TYPTR <> NIL) AND (GATTR.TYPTR <> NIL) THEN
                    CASE LOP OF
            (***)       MUL:  IF (LATTR.TYPTR=INTPTR)AND(GATTR.TYPTR=INTPTR)
                              THEN GEN0(15(*MPI*))
                            ELSE
                              BEGIN
#                               IF GATTR.TYPTR = INTPTR THEN
#                                 BEGIN GEN0(10(*FLT*));
#                                   GATTR.TYPTR := REALPTR
#                                 END
#                               ELSE
#                                 IF LATTR.TYPTR = INTPTR THEN
#                                   BEGIN GEN0(9(*FLO*));
#                                     LATTR.TYPTR := REALPTR
                                    END;
                                IF (LATTR.TYPTR = REALPTR)
                                  AND(GATTR.TYPTR=REALPTR)THEN GEN0(16(*MPR*))
"**"                            ELSE GENSETOP(LATTR,12(*INT*));
                              END;
            (*/*)       RDIV: BEGIN
#                             IF GATTR.TYPTR = INTPTR THEN
#                               BEGIN GEN0(10(*FLT*));
#                                 GATTR.TYPTR := REALPTR
#                               END;
#                             IF LATTR.TYPTR = INTPTR THEN
#                               BEGIN GEN0(9(*FLO*));
#                                 LATTR.TYPTR := REALPTR
#                               END;
                              IF (LATTR.TYPTR = REALPTR)
                                AND (GATTR.TYPTR=REALPTR)THEN GEN0(7(*DVR*))
                              ELSE BEGIN ERROR(134); GATTR.TYPTR := NIL END
                            END;
            (*DIV*)     IDIV: IF (LATTR.TYPTR = INTPTR)
                              AND (GATTR.TYPTR = INTPTR) THEN GEN0(6(*DVI*))
                            ELSE BEGIN ERROR(134); GATTR.TYPTR := NIL END;
            (*MOD*)     IMOD: IF (LATTR.TYPTR = INTPTR)
#                             AND (GATTR.TYPTR = INTPTR) THEN GEN0(14       )
                            ELSE BEGIN ERROR(134); GATTR.TYPTR := NIL END;
            (*AND*)     ANDOP:IF (LATTR.TYPTR = BOOLPTR)
                              AND (GATTR.TYPTR = BOOLPTR) THEN GEN0(4(*AND*))
                            ELSE BEGIN ERROR(134); GATTR.TYPTR := NIL END
                    END (*CASE*)
                  ELSE GATTR.TYPTR := NIL;
#                 GATTR.BTYPE := GATTR.TYPTR;
                END (*WHILE*)
            END (*TERM*) ;

          BEGIN (*SIMPLEEXPRESSION*)
            SIGNED := FALSE;
            IF (SY = ADDOP) AND (OP IN [PLUS,MINUS]) THEN
              BEGIN SIGNED := OP = MINUS; INSYMBOL END;
            TERM(FSYS + [ADDOP]);
            IF SIGNED THEN
              BEGIN LOAD;
                IF GATTR.TYPTR = INTPTR THEN GEN0(17(*NGI*))
                ELSE
                  IF GATTR.TYPTR = REALPTR THEN GEN0(18(*NGR*))
                  ELSE BEGIN ERROR(134); GATTR.TYPTR := NIL END
              END;
            WHILE SY = ADDOP DO
"**"          BEGIN
"**"            IF GATTR.TYPTR <> NIL THEN
"**"              IF GATTR.TYPTR@.FORM < POWER THEN
"**"                LOAD
"**"              ELSE FORCETEMPSET;
"**"            LATTR := GATTR;  LOP := OP;
"**"            INSYMBOL; TERM(FSYS + [ADDOP]);
"**"            IF GATTR.TYPTR <> NIL THEN
"**"              IF GATTR.TYPTR@.FORM < POWER THEN
"**"                LOAD;
                IF (LATTR.TYPTR <> NIL) AND (GATTR.TYPTR <> NIL) THEN
                  CASE LOP OF
          (*+*)       PLUS:
                      IF (LATTR.TYPTR = INTPTR)AND(GATTR.TYPTR = INTPTR) THEN
                        GEN0(2(*ADI*))
                      ELSE
                        BEGIN
#                         IF GATTR.TYPTR = INTPTR THEN
#                           BEGIN GEN0(10(*FLT*));
#                             GATTR.TYPTR := REALPTR
#                           END
#                         ELSE
#                           IF LATTR.TYPTR = INTPTR THEN
#                             BEGIN GEN0(9(*FLO*));
#                               LATTR.TYPTR := REALPTR
#                             END;
                          IF (LATTR.TYPTR = REALPTR)AND(GATTR.TYPTR = REALPTR)
                            THEN GEN0(3(*ADR*))
"**"                      ELSE GENSETOP(LATTR,31(*UNI*));
                        END;
          (*-*)       MINUS:
                      IF (LATTR.TYPTR = INTPTR)AND(GATTR.TYPTR = INTPTR) THEN
                        GEN0(21(*SBI*))
                      ELSE
                        BEGIN
#                         IF GATTR.TYPTR = INTPTR THEN
#                           BEGIN GEN0(10(*FLT*));
#                             GATTR.TYPTR := REALPTR
#                           END
#                         ELSE
#                           IF LATTR.TYPTR = INTPTR THEN
#                             BEGIN GEN0(9(*FLO*));
#                               LATTR.TYPTR := REALPTR
                              END;
                          IF (LATTR.TYPTR = REALPTR)AND(GATTR.TYPTR = REALPTR)
                            THEN GEN0(8(*SBR*))
"**"                      ELSE GENSETOP(LATTR,5(*DIF*));
                        END;
          (*OR*)      OROP:
                      IF(LATTR.TYPTR=BOOLPTR)AND(GATTR.TYPTR=BOOLPTR)THEN
                        GEN0(13(*IOR*))
                      ELSE BEGIN ERROR(134); GATTR.TYPTR := NIL END
                  END (*CASE*)
                ELSE GATTR.TYPTR := NIL;
#               GATTR.BTYPE := GATTR.TYPTR;
              END (*WHILE*)
          END (*SIMPLEEXPRESSION*) ;

        BEGIN (*EXPRESSION*)
"**"      LLC := LC;  SIMPLEEXPRESSION(FSYS + [RELOP]);
          IF SY = RELOP THEN
            BEGIN
              IF GATTR.TYPTR <> NIL THEN
"**"            IF GATTR.TYPTR@.FORM < POWER THEN LOAD
"**"            ELSE IF GATTR.TYPTR@.FORM = POWER THEN FORCETEMPSET
                ELSE LOADADDRESS;
              LATTR := GATTR; LOP := OP;
"**"          IF LOP = INOP THEN
"&&"            IF GATTR.TYPTR <> INTPTR THEN
"**"              GEN0( 61 (*ORD*) );
              INSYMBOL; SIMPLEEXPRESSION(FSYS);
              IF GATTR.TYPTR <> NIL THEN
"**"            IF GATTR.TYPTR@.FORM < POWER THEN LOAD
"**"            ELSE IF GATTR.TYPTR@.FORM = POWER THEN FORCETEMPSET
                ELSE LOADADDRESS;
              IF (LATTR.TYPTR <> NIL) AND (GATTR.TYPTR <> NIL) THEN
     (*IN*)     IF LOP = INOP THEN
                  IF GATTR.TYPTR@.FORM = POWER THEN
                    IF COMPTYPES(LATTR.TYPTR,GATTR.TYPTR@.ELSET) THEN
"**"                  GEN0( 11 (*INN*) )
                    ELSE BEGIN ERROR(129); GATTR.TYPTR := NIL END
                  ELSE BEGIN ERROR(130); GATTR.TYPTR := NIL END
                ELSE
                  BEGIN
                    IF LATTR.TYPTR <> GATTR.TYPTR THEN
#                     IF GATTR.TYPTR = INTPTR THEN
#                       BEGIN GEN0(10(*FLT*));
#                         GATTR.TYPTR := REALPTR
#                       END
#                     ELSE
#                       IF LATTR.TYPTR = INTPTR THEN
#                         BEGIN GEN0(9(*FLO*));
#                           LATTR.TYPTR := REALPTR
                          END;
                    IF COMPTYPES(LATTR.TYPTR,GATTR.TYPTR) THEN
                      BEGIN LSIZE := LATTR.TYPTR@.SIZE;
                        CASE LATTR.TYPTR@.FORM OF
                          SCALAR:
                            IF LATTR.TYPTR = REALPTR THEN TYPIND := 'R'
                            ELSE
                              IF LATTR.TYPTR = BOOLPTR THEN TYPIND := 'B'
#                             ELSE
#                               IF LATTR.TYPTR = CHARPTR THEN TYPIND := 'C'
#                               ELSE TYPIND := 'I' ;
                          POINTER:
                            BEGIN
                              IF LOP IN [LTOP,LEOP,GTOP,GEOP] THEN ERROR(131);
                              TYPIND := 'A'
                            END;
                          POWER:
                            BEGIN IF LOP IN [LTOP,GTOP] THEN ERROR(132);
                              TYPIND := 'S';
                          END;
                          ARRAYS:
                            BEGIN
                              IF NOT STRING(LATTR.TYPTR) THEN
                                IF LOP IN[LTOP,LEOP,GTOP,GEOP] THEN ERROR(131);
                              TYPIND := 'M'
                            END;
                          RECORDS:
                            BEGIN
                              IF LOP IN [LTOP,LEOP,GTOP,GEOP] THEN ERROR(131);
                              TYPIND := 'M'
                            END;
                          FILES:
                            BEGIN ERROR(133); TYPIND := 'F' END
                        END;
"&&"                    GEN2( COMPARE_OP(/LOP/), ORD(TYPIND), LSIZE );
                      END
                    ELSE ERROR(129)
                  END;
"&&"          GATTR.TYPTR := BOOLPTR; GATTR.BTYPE := BOOLPTR;
              GATTR.KIND := EXPR;
"**"          LC := LLC;
            END (*SY = RELOP*)
        END (*EXPRESSION*) ;

        PROCEDURE ASSIGNMENT(FCP: CTP);
"**"      VAR LATTR: ATTR;  RSIZE, LLC: ADDRRANGE;
        BEGIN  LLC := LC;
          SELECTOR(FSYS + [BECOMES],FCP);
#         VAR_MOD := VAR_MOD+1 ;
          IF SY = BECOMES THEN
            BEGIN
              IF GATTR.TYPTR <> NIL THEN
                IF (GATTR.ACCESS<>DRCT) OR (GATTR.TYPTR@.FORM>=POWER) THEN
                  LOADADDRESS;
              LATTR := GATTR;
              INSYMBOL; EXPRESSION(FSYS);
              IF GATTR.TYPTR <> NIL THEN
"**"            IF GATTR.TYPTR@.FORM < POWER THEN LOAD
"**"            ELSE IF GATTR.TYPTR@.FORM = POWER THEN FORCETEMPSET
                ELSE LOADADDRESS;
              IF (LATTR.TYPTR <> NIL) AND (GATTR.TYPTR <> NIL) THEN
                BEGIN
                  IF COMPTYPES(REALPTR,LATTR.TYPTR)AND(GATTR.TYPTR=INTPTR)THEN
                    BEGIN GEN0(10(*FLT*));
                      GATTR.TYPTR := REALPTR
                    END;
                  IF COMPTYPES(LATTR.TYPTR,GATTR.TYPTR) THEN
                    BEGIN

#                     IF DEBUG THEN
#                       BEGIN
"&&"                    ASSIGN := TRUE ;  CHKBNDS(LATTR.BTYPE);  ASSIGN := FALSE
#                       END ;

                      CASE LATTR.TYPTR@.FORM OF
                        SCALAR,
                        SUBRANGE,
"**"                    POINTER: STORE(LATTR);
"**"                    POWER:   GEN2(69(*SMV*),LATTR.TYPTR@.SIZE,
"**"                                            GATTR.TYPTR@.SIZE);
                        ARRAYS,
                        RECORDS: GEN1(40(*MOV*),LATTR.TYPTR@.SIZE);
                        FILES: ERROR(146)
                      END  (*CASE LATTR...*)
#                   END
                  ELSE ERROR(129)
                END
            END (*SY = BECOMES*)
          ELSE ERROR(51);
"**"  LC := LLC;
        END (*ASSIGNMENT*) ;

        PROCEDURE GOTOSTATEMENT;
"&&"      LABEL 10;
"&&"      VAR LLP: LBP;  TTOP: DISPRANGE;  XLABEL: ALPHA;
        BEGIN
          IF SY = INTCONST THEN
            BEGIN
"&&"          TTOP := LEVEL;
#             REPEAT
                LLP := DISPLAY[TTOP].FLABEL;
"&&"            WHILE LLP <> NIL DO
                  WITH LLP@ DO
                    IF LABVAL = VAL.IVAL THEN
"&&"                  BEGIN
"&&"                    IF TTOP = LEVEL THEN
"&&"                      GENUJPFJP( 57(*UJP*), LABNAME )
"&&"                    ELSE BEGIN
"&&"                      IF XNO = 0 THEN  (* FIRST DEEP GOTO TO THIS LABEL *)
"&&"                        BEGIN  XLABNO := XLABNO+1;  XNO := XLABNO  END;
"&&"                      XLABEL := '############';  MKNAME(XLABEL,XNO,FALSE);
"&&"                      IF PRCODE THEN
"&&"                        WRITELN(PRR, MN(/73/), ' ', XLABEL:EXTNAMSZ );
"&&"                    END;
"CT"                    CTREMIT( CTRGOTO, 0, LINECNT, 0, LINECNT );
"&&"                    GOTO 10
                      END
                    ELSE LLP := NEXTLAB;
                TTOP := TTOP - 1
"&&"          UNTIL TTOP = 0;
              ERROR(167);
"&&" 10:      INSYMBOL
            END
          ELSE ERROR(15)
        END (*GOTOSTATEMENT*) ;

        PROCEDURE COMPOUNDSTATEMENT;
        BEGIN
          REPEAT
            REPEAT STATEMENT(FSYS + [SEMICOLON,ENDSY])
            UNTIL NOT (SY IN STATBEGSYS);
            TEST := SY <> SEMICOLON;
            IF NOT TEST THEN INSYMBOL
          UNTIL TEST;
          IF SY = ENDSY THEN INSYMBOL ELSE ERROR(13)
        END (*COMPOUNDSTATEMENET*) ;

        PROCEDURE IFSTATEMENT;
          VAR LCIX1,LCIX2: LABELRNG;
"CT"          FIRSTLN, MIDLN : INTEGER;   CTRNO : CTRRANGE;
        BEGIN EXPRESSION(FSYS + [THENSY]);
          GENLABEL(LCIX1); GENFJP(LCIX1);
          IF SY = THENSY THEN INSYMBOL ELSE ERROR(52);
"CT"      FIRSTLN := LINECNT; CTRNO := CTRGEN;
"CT"      (*** COUNTER HERE ***)

          STATEMENT(FSYS + [ELSESY]);
          IF SY = ELSESY THEN
            BEGIN GENLABEL(LCIX2); GENUJPFJP(57(*UJP*),LCIX2);
              PUTLABEL(LCIX1);
              INSYMBOL;
"CT"          MIDLN := LINECNT ;
              STATEMENT(FSYS);
              PUTLABEL(LCIX2)
            END
          ELSE
            BEGIN
            PUTLABEL(LCIX1) ;
"CT"        MIDLN := 0;
            END ;
"CT"      CTREMIT(CTRIF, CTRNO, FIRSTLN, MIDLN, LINECNT)
        END (*IFSTATEMENT*) ;

        PROCEDURE CASESTATEMENT;
"&&"      LABEL 1;
          TYPE CIP = @CASEINFO;
"&&"           CASEINFO = RECORD NEXT: CIP;
                            CSSTART: LABELRNG;
"&&"                        CSLAB1,CSLAB2: INTEGER
                          END;
#         VAR LSP,LSP1: STP; FSTPTR,LPT1,LPT2,LPT3: CIP; LVAL,LVAL1: VALU;
"&&"          LADDR, LCIX, LCIX1, UBND, LBND, XADDR: LABELRNG ;
"&&"          LMIN, LMAX : INTEGER ;  OTHWC: BOOLEAN;
"CT"          FIRSTLN : INTEGER; TEMPLN  : INTEGER;
"CT"          CTRCASES : INTEGER; CTRNO : CTRRANGE;
        BEGIN EXPRESSION(FSYS + [OFSY,COMMA,COLON]);
#         LOAD ;
#         LSP := GATTR.TYPTR;
#         IF LSP <> NIL THEN
#           IF (LSP@.FORM <> SCALAR) OR (LSP = REALPTR) THEN
#             BEGIN  ERROR(144); LSP := NIL END
#           ELSE  IF NOT COMPTYPES(LSP,INTPTR) THEN  GEN0(61(*ORD*)) ;
#         IF DEBUG THEN  CHKBNDS(GATTR.TYPTR) ;
          IF SY = OFSY THEN INSYMBOL ELSE ERROR(8);
#         FSTPTR := NIL ;  GENLABEL(LBND) ;  GENLABEL(UBND) ;
#         GENLABEL(LCIX) ;  GENLABEL(LADDR);
"&&"      GENLABEL(XADDR);
#         (* WE SHOULD HAVE:  LADDR = LCIX+1 = UBND+2 = LBND+3   HERE *)
#         GENUJPFJP(44 (*XJP*), LBND) ;
"&&"      OTHWC := FALSE;
"CT"      FIRSTLN := LINECNT;  CTRCASES := 0;
          REPEAT
            LPT3 := NIL; GENLABEL(LCIX1);
#           IF NOT(SY IN [SEMICOLON,ENDSY]) THEN
#           BEGIN
"&&"          IF SY <> OTHERWISESY THEN
"&&"            BEGIN
                  REPEAT CONSTANT(FSYS + [COMMA,COLON,DOTDOT],LSP1,LVAL);
                    IF LSP <> NIL THEN
                      IF COMPTYPES(LSP,LSP1) THEN
#                       BEGIN
#                       LVAL1.IVAL := LVAL.IVAL ;
#                       IF SY = DOTDOT THEN
#                         BEGIN  INSYMBOL ;
#                         CONSTANT(FSYS+[COMMA,COLON],LSP1,LVAL1)
#                         END ;
#                       IF COMPTYPES(LSP,LSP1) THEN
"&&"                        IF LVAL.IVAL <= LVAL1.IVAL THEN
                              BEGIN LPT1 := FSTPTR; LPT2 := NIL;
                              WHILE LPT1 <> NIL DO
                                WITH LPT1@ DO
                                  BEGIN
"&&"                                IF LVAL1.IVAL >= CSLAB2 THEN
                                      BEGIN
"&&"                                    IF LVAL.IVAL <= CSLAB2 THEN ERROR(156);
                                        GOTO 1
                                      END;
                                    LPT2 := LPT1; LPT1 := NEXT
                                  END;
                1:            NEW(LPT3);
                              WITH LPT3@ DO
                                BEGIN NEXT := LPT1;
"&&"                              CSLAB1 := LVAL.IVAL; CSLAB2 := LVAL1.IVAL;
                                  CSSTART := LCIX1
                                END;
                              IF LPT2 = NIL THEN FSTPTR := LPT3
                              ELSE LPT2@.NEXT := LPT3
                             END
"&&"                   ELSE ERROR(102)
                        ELSE ERROR(147);
                        END  ELSE ERROR(147);
                    TEST := SY <> COMMA;
                    IF NOT TEST THEN INSYMBOL
                  UNTIL TEST;
"&&"              IF SY = COLON THEN INSYMBOL ELSE ERROR(5)
"&&"            END
"&&"          ELSE  (* SY = OTHERWISESY *)
"&&"            BEGIN
"&&"              IF OTHWC THEN ERROR(156) ELSE LCIX1 := LADDR;
"&&"              OTHWC := TRUE;  INSYMBOL;
"&&"              IF SY = COLON THEN INSYMBOL  (* IGNORE : FOR NOW *)
"&&"            END;
              PUTLABEL(LCIX1);
"CT"          TEMPLN := LINECNT; (*** COUNTER HERE ***)
"CT"          CTRNO := CTRGEN;  CTRCASES := CTRCASES+1 ;
              REPEAT STATEMENT(FSYS + [SEMICOLON])
              UNTIL NOT (SY IN STATBEGSYS);
"&&"          GENUJPFJP(57(*UJP*),XADDR);
"CT"          CTREMIT(CTRCASE, CTRNO, TEMPLN, 0, LINECNT);
#           END ;
            TEST := SY <> SEMICOLON;
            IF NOT TEST THEN INSYMBOL ;
          UNTIL TEST;
          IF FSTPTR <> NIL THEN
"&&"        BEGIN LMAX := FSTPTR@.CSLAB2;
              (*REVERSE POINTERS*)
              LPT1 := FSTPTR; FSTPTR := NIL;
              REPEAT LPT2 := LPT1@.NEXT; LPT1@.NEXT := FSTPTR;
                FSTPTR := LPT1; LPT1 := LPT2
              UNTIL LPT1 = NIL;
"&&"          LMIN := FSTPTR@.CSLAB1;
#           END
#         ELSE  BEGIN  LMIN := 1 ;  LMAX := 0  END ;
#         GENDEF(LBND,LMIN) ;  GENDEF(UBND,LMAX) ;  PUTLABEL(LCIX) ;
          IF LMAX - LMIN < CIXMAX THEN
#           BEGIN
#           IF FSTPTR <> NIL THEN
              REPEAT
                WITH FSTPTR@ DO
                  BEGIN
"&&"                WHILE CSLAB1 > LMIN DO
                      BEGIN GENUJPFJP(57(*UJP*),LADDR); LMIN:=LMIN+1 END;
"&&"                REPEAT
                      GENUJPFJP(57(*UJP*),CSSTART);
"&&"                  LMIN := LMIN + 1;
"&&"                UNTIL LMIN > CSLAB2;
                    FSTPTR := NEXT;
                  END
              UNTIL FSTPTR = NIL;
"&&"          IF NOT OTHWC THEN PUTLABEL(LADDR) ;
"&&"          PUTLABEL(XADDR);
"CT"          CTREMIT(CTRCASE, 0, FIRSTLN, CTRCASES, LINECNT);
            END
          ELSE ERROR(157) ;
          IF SY = ENDSY THEN INSYMBOL ELSE ERROR(13)
        END (*CASESTATEMENT*) ;

        PROCEDURE REPEATSTATEMENT;
          VAR LADDR: LABELRNG;
"CT"            FIRSTLN : INTEGER; CTRNO : CTRRANGE;
        BEGIN GENLABEL(LADDR); PUTLABEL(LADDR);
"CT"      FIRSTLN := LINECNT; CTRNO := CTRGEN;
"CT"      (*** COUNTER HERE ***)
          REPEAT
            REPEAT STATEMENT(FSYS + [SEMICOLON,UNTILSY])
            UNTIL NOT (SY IN STATBEGSYS);
            TEST := SY <> SEMICOLON;
            IF NOT TEST THEN INSYMBOL
          UNTIL TEST;
          IF SY = UNTILSY THEN
            BEGIN INSYMBOL; EXPRESSION(FSYS); GENFJP(LADDR) ;
"CT"            CTREMIT(CTRREPEAT, CTRNO, FIRSTLN, 0, LINECNT)
            END
          ELSE ERROR(53)
        END (*REPEATSTATEMENT*) ;

        PROCEDURE WHILESTATEMENT;
          VAR LADDR, LCIX: LABELRNG;
"CT"          FIRSTLN : INTEGER; CTRNO : CTRRANGE;
        BEGIN GENLABEL(LADDR); PUTLABEL(LADDR);
          EXPRESSION(FSYS + [DOSY]); GENLABEL(LCIX); GENFJP(LCIX);
          IF SY = DOSY THEN INSYMBOL ELSE ERROR(54);
"CT"      FIRSTLN := LINECNT; CTRNO := CTRGEN;
          (*** COUNTER HERE ***)
          STATEMENT(FSYS); GENUJPFJP(57(*UJP*),LADDR); PUTLABEL(LCIX) ;
"CT"      CTREMIT(CTRWHILE, CTRNO, FIRSTLN, 0, LINECNT);
        END (*WHILESTATEMENT*) ;

        PROCEDURE FORSTATEMENT;
"&&"      VAR LATTR: ATTR; LSP: STP;  LSY: SYMBOL;  LOP: OPRANGE;
"&&"          XT, CV1, CV2: INTEGER;  CB1, CB2: BOOLEAN;
#             LCIX, LADDR: LABELRNG;  LLC : ADDRRANGE;
"CT"          FIRSTLN : INTEGER; CTRNO : CTRRANGE;
        BEGIN
          IF SY = IDENT THEN
            BEGIN SEARCHID([VARS],LCP);
              WITH LCP@, LATTR DO
#               BEGIN TYPTR := IDTYPE; KIND := VARBL; BTYPE := TYPTR ;
"&&"              IF TYPTR <> NIL THEN
"&&"                IF TYPTR@.FORM = SUBRANGE THEN
"&&"                  TYPTR := TYPTR@.RANGETYPE;
                  IF VKIND = ACTUAL THEN
                    BEGIN ACCESS := DRCT; VLEVEL := VLEV;
                      DPLMT := VADDR ;
                    END
                  ELSE BEGIN ERROR(155); TYPTR := NIL END
                END;
              IF LATTR.TYPTR <> NIL THEN
                IF (LATTR.TYPTR@.FORM > SUBRANGE)
"&&"               OR (LATTR.TYPTR = REALPTR) THEN
                  BEGIN ERROR(143); LATTR.TYPTR := NIL END;
              INSYMBOL
            END
          ELSE
            BEGIN ERROR(2);  LATTR.TYPTR := NIL;
                  SKIP(FSYS + [BECOMES,TOSY,DOWNTOSY,DOSY]) END;
          IF SY = BECOMES THEN
            BEGIN INSYMBOL; EXPRESSION(FSYS + [TOSY,DOWNTOSY,DOSY]);
              IF GATTR.TYPTR <> NIL THEN
                  IF GATTR.TYPTR@.FORM <> SCALAR THEN ERROR(144)
                  ELSE
                    IF COMPTYPES(LATTR.TYPTR,GATTR.TYPTR) THEN
"&&"                  BEGIN
"&&"                    IF GATTR.KIND = CST THEN
"&&"                      BEGIN  CB1 := TRUE;
"&&"                        CV1 := GATTR.CVAL.IVAL
"&&"                      END
"&&"                    ELSE CB1 := FALSE;
"&&"                    LOAD;
#                       STORE(LATTR);
                      END
                    ELSE ERROR(145)
            END
          ELSE
            BEGIN ERROR(51); SKIP(FSYS + [TOSY,DOWNTOSY,DOSY]) END;
"&&"      IF (SY = TOSY) OR (SY = DOWNTOSY) THEN
            BEGIN LSY := SY; INSYMBOL; EXPRESSION(FSYS + [DOSY]);
              IF GATTR.TYPTR <> NIL THEN
              IF GATTR.TYPTR@.FORM <> SCALAR THEN ERROR(144)
                ELSE
                  IF COMPTYPES(LATTR.TYPTR,GATTR.TYPTR) THEN
"&&"                BEGIN
"&&"                  IF GATTR.KIND = CST THEN
"&&"                    BEGIN  CB2 := TRUE;  LLC := LC;
"&&"                      CV2 := GATTR.CVAL.IVAL
"&&"                    END
"&&"                  ELSE
"&&"                    BEGIN  CB2 := FALSE;
"&&"                      LOAD;
"&&"                      ALIGN(LC,INTSIZE);  LLC := LC;
"&&"                      IF GATTR.TYPTR <> INTPTR THEN GEN0(61(*ORD*));
"&&"                      GEN3(56(*STR*),ORD('I'),LEVEL,LLC);
"&&"                      LC := LC + INTSIZE;
"&&"                      IF LC > LCMAX THEN LCMAX := LC;
"&&"                    END;
"&&"                  IF CB1 AND CB2 THEN
"&&"                    BEGIN  XT := 1;
"&&"                      IF LSY = TOSY THEN
"&&"                        IF CV1 > CV2 THEN XT := 0 ELSE
"&&"                      ELSE
"&&"                        IF CV1 < CV2 THEN XT := 0;
"&&"                      GEN2(51(*LDC*),3,XT);
"&&"                    END
"&&"                  ELSE
"&&"                    BEGIN
"&&"                      IF CB1 THEN
"&&"                        GEN2(51(*LDC*),1,CV1)
"&&"                      ELSE
"&&"                        BEGIN  GATTR := LATTR;  LOAD;
"&&"                          IF GATTR.TYPTR <> INTPTR THEN GEN0(61(*ORD*));
"&&"                        END;
"&&"                      IF CB2 THEN
"&&"                        GEN2(51(*LDC*),1,CV2)
"&&"                      ELSE
"&&"                        GEN3(54(*LOD*),ORD('I'),LEVEL,LLC);
"&&"                      IF LSY = TOSY THEN LOP := 52 ELSE LOP := 48;
"&&"                      GEN2(LOP,ORD('I'),1);
"&&"                    END;
"&&"                END
                  ELSE ERROR(145)
            END
          ELSE BEGIN ERROR(55); SKIP(FSYS + [DOSY]) END;
#         GENLABEL(LADDR) ;  GENLABEL(LCIX);  GENUJPFJP(33(*FJP*),LCIX);
#         PUTLABEL(LADDR) ;  (*BEGINNING OF THE FOR 'LOOP'*)
          IF SY = DOSY THEN INSYMBOL ELSE ERROR(54);
"CT"      FIRSTLN := LINECNT; CTRNO := CTRGEN;
          (*** COUNTER HERE ***)
          STATEMENT(FSYS);
#         GATTR := LATTR ;  LOAD ;
"&&"      IF GATTR.TYPTR <> INTPTR THEN  GEN0(61(*ORD*));
"&&"      IF CB2 THEN
"&&"        GEN2(51(*LDC*),1,CV2)
"&&"      ELSE
#           GEN3(54(*LOD*),ORD('I'),LEVEL,LLC);
#         GEN2(55(*NEQ*),ORD('I'),1);
#         GENUJPFJP(33(*FJP*),LCIX) ;
#         GATTR := LATTR; LOAD;
"&&"      LOP := 23 (*INC*) ;
"&&"      IF LSY <> TOSY THEN LOP := 22 (*DEC*) ;
"&&"      GEN2(LOP,GETTYPE(GATTR.TYPTR),1);
#         IF DEBUG THEN CHKBNDS(LATTR.TYPTR) ;
          STORE(LATTR); GENUJPFJP(57(*UJP*),LADDR); PUTLABEL(LCIX);
          LC := LLC ;
"CT"      CTREMIT(CTRFOR, CTRNO, FIRSTLN, 0, LINECNT);
        END (*FORSTATEMENT*) ;

        PROCEDURE WITHSTATEMENT;
#         VAR LCP: CTP; LCNT: DISPRANGE; LLC: ADDRRANGE;
"&&"          OLD_LEV: -1 .. DISPLIMIT;  REC_STR: STP;
"&&"    BEGIN LLC := LC;
"SY"    IF GET_STAT THEN WS_CNT := WS_CNT + 1;
          IF SY = IDENT THEN
            BEGIN SEARCHID([VARS,FIELD],LCP); INSYMBOL END
          ELSE BEGIN ERROR(2); LCP := UVARPTR END;
          SELECTOR(FSYS + [COMMA,DOSY],LCP);
"&&"      REC_STR := GATTR.TYPTR;
          IF GATTR.TYPTR <> NIL THEN
            IF GATTR.TYPTR@.FORM = RECORDS THEN
              IF TOP < DISPLIMIT THEN
#               BEGIN  TOP := TOP + 1;
                  WITH DISPLAY[TOP] DO
                    BEGIN
"&&"                  OLD_LEV := REC_STR@.FLD_DISP_LEV;
"&&"                  REC_STR@.FLD_DISP_LEV := TOP;
                      IF GATTR.ACCESS = DRCT THEN
                        BEGIN OCCUR := CREC; CLEV := GATTR.VLEVEL;
                          CDSPL := GATTR.DPLMT
                        END
                      ELSE
#                       BEGIN  LOADADDRESS;  ALIGN(LC,PTRSIZE) ;
#                         GEN3(56(*STR*),ORD('A'),LEVEL,LC);
                          OCCUR := VREC; VDSPL := LC;
#                         LC := LC + PTRSIZE;
                          IF LC > LCMAX THEN LCMAX := LC
                        END
"&&"                END
                END
              ELSE FATALERROR(250)
"&&"        ELSE BEGIN ERROR(140); REC_STR := NIL  END;
"&&"      OPEN_RECORD := REC_STR;
"&&"      IF SY = COMMA THEN
"&&"        BEGIN  INSYMBOL;  WITHSTATEMENT  END
"&&"      ELSE
"&&"        BEGIN
              IF SY = DOSY THEN INSYMBOL ELSE ERROR(54);
              STATEMENT(FSYS);
"&&"        END;
"&&"      IF REC_STR <> NIL THEN
"&&"        REC_STR@.FLD_DISP_LEV := OLD_LEV;
"&&"      TOP := TOP-1;  LC := LLC;  OPEN_RECORD := NIL;
        END (*WITHSTATEMENT*) ;

      BEGIN (*STATEMENT*)
        IF SY = INTCONST THEN (*LABEL*)
"&&"      BEGIN
"&&"      LLP := DISPLAY[LEVEL].FLABEL;
            WHILE LLP <> NIL DO
              WITH LLP@ DO
                IF LABVAL = VAL.IVAL THEN
                  BEGIN IF DEFINED THEN ERROR(165);
"&&"                  IF XNO > 0 THEN  (* LABEL IS AN EXTERNAL ENTRY PT. *)
"&&"                    BEGIN  XLABEL := '############';
"&&"                      MKNAME( XLABEL, XNO, FALSE );
"&&"                      IF PRCODE THEN
"&&"                        WRITELN(PRR, XLABEL:EXTNAMSZ, MN(/74/) );
"&&"                      XNO := 0;  (* IN CASE OF REDEFINITION *)
"&&"                    END;
                    PUTLABEL(LABNAME); DEFINED := TRUE;
"CT"                CTRNO := CTRGEN;
"CT"                CTREMIT(CTRLBL, CTRNO, LINECNT, 0, LINECNT);
"CT"                (*** COUNTER HERE ***)
                    GOTO 1
                  END
                ELSE LLP := NEXTLAB;
            ERROR(167);
      1:    INSYMBOL;
            IF SY = COLON THEN INSYMBOL ELSE ERROR(5)
          END;
        IF NOT (SY IN FSYS + [IDENT]) THEN
          BEGIN ERROR(6); SKIP(FSYS) END;
        IF SY IN STATBEGSYS + [IDENT] THEN
          BEGIN
            CASE SY OF
              IDENT:    BEGIN SEARCHID([VARS,FIELD,FUNC,PROC],LCP); INSYMBOL;
                          IF LCP@.KLASS = PROC THEN CALL(FSYS,LCP)
                          ELSE ASSIGNMENT(LCP)
                        END;
"&&"          BEGINSY:  BEGIN  STMTNEST := STMTNEST + 1;  INSYMBOL;
"&&"                      COMPOUNDSTATEMENT;  STMTNEST := STMTNEST - 1;  END;
              GOTOSY:   BEGIN INSYMBOL; GOTOSTATEMENT END;
              IFSY:     BEGIN INSYMBOL; IFSTATEMENT END;
"&&"          CASESY:   BEGIN   STMTNEST := STMTNEST + 1;  INSYMBOL;
"&&"                      CASESTATEMENT;  STMTNEST := STMTNEST - 1  END;
              WHILESY:  BEGIN INSYMBOL; WHILESTATEMENT END;
"&&"          REPEATSY: BEGIN   STMTNEST := STMTNEST + 1;  INSYMBOL;
"&&"                      REPEATSTATEMENT;  STMTNEST := STMTNEST - 1  END;
              FORSY:    BEGIN INSYMBOL; FORSTATEMENT END;
              WITHSY:   BEGIN INSYMBOL; WITHSTATEMENT END
            END;
            IF NOT (SY IN [SEMICOLON,ENDSY,ELSESY,UNTILSY]) THEN
              BEGIN ERROR(6); SKIP(FSYS) END
          END
      END (*STATEMENT*) ;

"&&" PROCEDURE FRTPARMS;
"&&"   (* THIS BORING PROCEDURE GENERATES DUMMY ROUTINES
"&&"      TO REPLACE FORTRAN ROUTINES PASSED AS PROC. PARAMETERS  *)
"&&"   VAR  PT, LOCPAR: INTEGER;  LLC, LCM: ADDRRANGE;  LCP1: CTP;
"&&"        FNAME: ALPHA;
"&&"   BEGIN
"&&"     LEVEL := LEVEL + 1;  OLDIC := IC;
"&&"     WHILE FRTPARHD <> NIL DO
"&&"       WITH FRTPARHD@ DO
"&&"         BEGIN  IC := 0;
"&&"           FRTRN := FALSE;  PT := PROCTYPE(FRTPARHD);
"&&"           FNAME := NAME;  MKNAME( FNAME, PFNAME, FALSE );
"&&"           WRITELN(PRR,FNAME:8, MN(/32/) (*ENT*), CHR(PT):2, ',',
"&&"              LEVEL:1, ',L', SEGSIZE:1, NAME:14, ',', SAVEREGS:1, ',',
"&&"              ASSEMBLE:1, ',', GET_STAT:1, ',', ASMVERB:1, ',',
"&&"              DEBUG_LEV:1, ',', PFNAME:1 );
"&&"           WRITELN(QRR,'#BGN    ', NAME, LEVEL:4 );
"&&"           LCP1 := PRMPTR;  LC := LCAFTMST + FPSAVEAREA;
"&&"           WHILE LCP1 <> NIL DO
"&&"             WITH LCP1@ DO
"&&"               BEGIN
"&&"                 IF KLASS = VARS THEN
"&&"                   IF IDTYPE <> NIL THEN
"&&"                     BEGIN
"&&"                       IF VKIND = FORMAL THEN LCM := VADDR + PTRSIZE
"&&"                       ELSE LCM := VADDR + IDTYPE@.SIZE;
"&&"                       IF LCM > LC THEN LC := LCM;
"&&"                     END;
"&&"                 LCP1 := NEXT
"&&"               END;
"&&"           ALIGN( LC, PTRSIZE );  LLC := LC;  LCP1 := PRMPTR;
"&&"           WHILE LCP1 <> NIL DO
"&&"             WITH LCP1@ DO
"&&"               BEGIN
"&&"                 IF KLASS = VARS THEN
"&&"                   IF IDTYPE <> NIL THEN
"&&"                     BEGIN
"&&"                       IF VKIND = FORMAL THEN
"&&"                         GEN3(54(*LOD*),ORD('A'),LEVEL,VADDR)
"&&"                       ELSE
"&&"                         GEN2(50(*LDA*),LEVEL,VADDR);
"&&"                       GEN3(56(*STR*),ORD('A'),LEVEL,LLC);
"&&"                       LLC := LLC + PTRSIZE;
"&&"                     END;
"&&"                 LCP1 := NEXT;
"&&"               END;
"&&"           FRTRN := TRUE;
"&&"           LOCPAR := (LLC - LC) DIV 2 + 1;
"&&"           PUTIC;  WRITELN(PRR, MN(/46/) (*CUP*), CHR(PROCTYPE(
"&&"                     FRTPARHD)):2, ',', LOCPAR:1, ',', EXTNAME, ',', LC:1 );
"&&"           IF KLASS = FUNC THEN
"&&"             GEN3( 56(*STR*), PT, LEVEL, FNCRSLT );
"&&"           GEN1( 42(*RET*), PT );
"&&"           GENDEF( SEGSIZE, LLC );
"&&"           WRITELN(QRR,'#PROC   ', NAME:IDLNGTH, ' ', PFNAME:1, ' ',
"&&"              FALSE:1, IC:6,LLC:8,' ',FALSE:1,' REF/MOD RATIO:', 0:4,
"&&"              0:6, 0.0:10);
"&&"           WRITELN(QRR,'#END');
"&&"           OLDIC := OLDIC + IC;
"&&"           FRTPARHD := NXTFWRD;
"&&"         END (* WHILE FRTPARHD <> NIL DO WITH ... *) ;
"&&"     LEVEL := LEVEL - 1;  IC := OLDIC;
"&&"   END  (* FRTPARMS *)  ;


    BEGIN (*BODY*)
"&&"  STMTNEST := 1;  LISTTAG := 'N';  PUTIC;
"&&"  IF FPROCP = MAINPROG THEN
"&&"    WRITELN(PRR, MN(/72/) (*BGN*), ' ', PROGNAME, ' ',
"DE"                TIME:8, ' ', DATE );
"&&"  WRITELN(PRR, FPROCP@.EXTNAME, MN(/32/) (*ENT*), CHR(PROCTYPE(FPROCP)):2,
"&&"    ',', LEVEL:1, ',L', SEGSIZE:1, FPROCP@.NAME:14, ',', SAVEREGS:1, ',',
"&&"    ASSEMBLE:1, ',', GET_STAT:1, ',', ASMVERB:1, ',', DEBUG_LEV:1, ',':1,
"&&"    FPROCP@.PFNAME:1 );
#     IC := IC + 1;  STIC := 0 ;     (* LENGTH OF STRING CONSTANTS *)
#
#     LOCAL_CALL := FALSE ;
#     VAR_REF := 0 ;  VAR_MOD := 0 ;
#     WRITELN(QRR, '#BGN    ', FPROCP@.NAME, LEVEL:4) ;
#
#
#     IF FPROCP = MAINPROG THEN  (* ENTERING MAIN BLOCK *)
        BEGIN
        WHILE FILEHEAD <> NIL DO
          BEGIN
            WITH FILEHEAD@ DO
               BEGIN
#                WITH FILIDPTR@ DO
#                  BEGIN
#                  GEN2(50(*LDA*),1,VADDR) ;
#                  GEN1(30(*CSP*),31(*SIO*)) ;
#
"@@"               IF VADDR >= FIRSTGVAR THEN  (* USER DEFINED FILES *)
"CM"                 BEGIN
"CM"                 NEW(CNSTPTR, STRG) ;
"CM"                 CNSTPTR@.SLNGTH := 8  (*OS NAME LENGTH*);
"CM"                 FOR I := 1 TO 8 DO CNSTPTR@.SVAL[I] := NAME[I] ;
"CM"                 GEN1(37(*LCA*), 0) ;
"@@"                 LLC1 := 0;   (* LENGTH CODE FOR A TEXT FILE *)
"@@"                 IF NOT COMPTYPES( IDTYPE, TEXTPTR ) THEN
"@@"                   IF IDTYPE <> NIL THEN
"@@"                     IF IDTYPE@.FILTYPE <> NIL THEN
"@@"                       LLC1 := IDTYPE@.FILTYPE@.SIZE;
"@@"                 GEN2( 51 (*LDC*), 1, LLC1 );  (* FILE COMP. SIZE *)
"CM"                 GEN1(30(*CSP*), 30(*FDF*)) ;
"CM"                 END
"CM"               ELSE  (* I.E. IF VADDR < FIRSTUSERF *)
#
"@@"                 BEGIN  I := 3;  (* CODE FOR RESET *)
"@@"                   IF FILIDPTR = OUTPUTPTR THEN I := 4  (* REWRITE *)
"@@"                   ELSE IF NAME[3] = 'R' THEN I := 4  (* REWRITE *);
"@@"                   GEN1( 30 (*CSP*), I  (* RES/REW *) )
"@@"                 END;
#                  GEN1(30(*CSP*),32(*EIO*)) ;
#                  END ;
               END;
          FILEHEAD := FILEHEAD@.NEXTFILE
          END;
"CT"    IF CTROPTION THEN
"CT"      BEGIN
"CT"      GENLABEL(CTRCNTLBL) ;   GENUJPFJP(38(*CTS*), CTRCNTLBL) ;
"CT"      END ;
        END; (* PROCESSING MAIN BLOCK *)
"CT"  FIRSTLN := LINECNT; CTRNO := CTRGEN;
"CT"  (*** COUNTER HERE ***)
      LCMAX := LC;

      (* COMPILE THE STATEMENTS WITHIN THIS BLOCK (BODY) *)

      REPEAT
        REPEAT STATEMENT(FSYS + [SEMICOLON,ENDSY])
        UNTIL NOT (SY IN STATBEGSYS);
        TEST := SY <> SEMICOLON;
        IF NOT TEST THEN INSYMBOL
      UNTIL TEST;

      IF SY = ENDSY THEN INSYMBOL ELSE ERROR(13);
"&&"  STMTNEST := 0;  LISTTAG := ' ';
      LLP := DISPLAY[TOP].FLABEL; (*TEST FOR UNDEFINED LABELS*)

      WHILE LLP <> NIL DO
        WITH LLP@ DO
          BEGIN
            IF NOT DEFINED THEN
              BEGIN
"&&"            PLCNT := PLCNT + 1;
#               WRITELN(OUTPUT,'**** UNDEF. LABEL:':23,LABVAL); ERROR(168) ;
              END;
            LLP := NEXTLAB
          END;

"CT"  CTREMIT(CTRPROC, CTRNO, FIRSTLN, 0, LINECNT);
"CT"  IF FPROCP = MAINPROG THEN            (* RESET COUNTERS *)
"CT"    BEGIN
"CT"    CTREMIT(CTRPROC, 0, 0, 0, 0); (* EOF FOR COUNTER TABLE *)
"CT"    IF ODD(CTRCNT) THEN  CTRCNT := CTRCNT+1 ;
"CT"    IF CTROPTION THEN  GENDEF(CTRCNTLBL, CTRCNT) ;
"CT"    END ;

#     GEN1(42(*RET*),PROCTYPE(FPROCP)); ALIGN(LCMAX,MXDATASZE) ;
#     IF PRCODE THEN GENDEF(SEGSIZE,LCMAX);
#
#   CALL_LVL[LOCAL_CALL] := CALL_LVL[LOCAL_CALL]+1 ;
#   WRITE(QRR, '#PROC   ',  FPROCP@.NAME:IDLNGTH, ' ', FPROCP@.PFNAME:1, ' ',
                  LOCAL_CALL:1,
#              ' ', IC+(STIC DIV 4):1, ' ', LCMAX:1, ' ', FLIPDEBUG:1,
#              ' REF/MOD RATIO:', VAR_MOD:4, VAR_MOD+VAR_REF:6) ;
#   IF (VAR_MOD+VAR_REF) = 0 THEN  WRITELN(QRR,0.0:10)
#   ELSE  WRITELN(QRR, VAR_MOD/(VAR_MOD+VAR_REF):10) ;
#   WRITELN(QRR, '#END') ;
#
#   OLDIC := OLDIC+ IC ;  IC := OLDIC ;  (* DISPLAY CUMULATIVE IC  *)
#   HP := TRUE;
"&&" IF FRTPARHD <> NIL THEN FRTPARMS;
"&&" IF FPROCP = MAINPROG THEN GEN0( 43(*STP*) );
    END (*BODY*) ;

  BEGIN (*BLOCK*)
"&&" IC := 0;  GENLABEL(SEGSIZE);  CONSTLC := -1;  FWRDPRCL := NIL;
"&&" DEC_ORDER := 0;
    REPEAT
"&&"  WHILE SY IN (/LABELSY,CONSTSY,TYPESY,VARSY/) DO
"&&"    BEGIN  LSY := SY;  INSYMBOL;
"&&"      CASE LSY OF
"&&"  LABELSY: BEGIN LABELDECLARATION;
"&&"                 IF DEC_ORDER >= 1 THEN EXTUSED := TRUE;  DEC_ORDER := 1;
"&&"           END;
"&&"  CONSTSY: BEGIN CONSTDECLARATION;
"&&"                 IF DEC_ORDER >= 2 THEN EXTUSED := TRUE;  DEC_ORDER := 2;
"&&"           END;
"&&"  TYPESY: BEGIN TYPEDECLARATION;
"&&"                IF DEC_ORDER >= 3 THEN EXTUSED := TRUE;  DEC_ORDER := 3;
"&&"          END;
"&&"  VARSY: BEGIN VARDECLARATION;
"&&"               IF DEC_ORDER >= 4 THEN EXTUSED := TRUE;  DEC_ORDER := 4;
"&&"         END;
"&&"      END;
"&&"    END;
"&&"  IF CONSTLC >= 0 THEN  (* A CONSTANTS BLOCK WAS GENERATED *)
"&&"    BEGIN  CONSTLC := -1;
"&&"      WRITELN(PRR, MN(/75/) );
"&&"    END;
"&&"  IF DEBUG_LEV > 0 THEN PRNTSYMBL( NIL );  (* PRINT HEAP TYPE DEFNS. *)
"&&"  WHILE SY IN (/PROCSY,FUNCSY/) DO
"&&"    BEGIN  LSY := SY;  INSYMBOL;  PROCDECLARATION(LSY)  END;
      IF SY <> BEGINSY THEN
        BEGIN ERROR(18); SKIP(FSYS) END
    UNTIL SY IN STATBEGSYS;
    IF SY = BEGINSY THEN INSYMBOL ELSE ERROR(17);
"&&" WHILE FWRDPRCL <> NIL DO
"&&"   BEGIN  WRITELN( '**** MISSING FORWARD DECLARED PROCEDURE:':50,
"&&"                   FWRDPRCL@.NAME:14 );
"&&"          PLCNT := PLCNT + 1;
"&&"          FWRDPRCL := FWRDPRCL@.NXTFWRD
"&&"   END;
    REPEAT BODY(FSYS + [CASESY]);
      IF SY <> FSY THEN
        BEGIN ERROR(6); SKIP(FSYS + [FSY]) END
    UNTIL (SY = FSY) OR (SY IN BLOCKBEGSYS);
  END (*BLOCK*) ;


  PROCEDURE PROGRAMME(FSYS:SETOFSYS);
#   VAR   LFPTR: FRECPTR ;   LCP : CTP ;
"&&"      I, J: INTEGER;
  BEGIN
#   CALL_LVL[FALSE] := 0 ;  CALL_LVL[TRUE] := 0 ;
    IF SY = PROGSY THEN
#     BEGIN INSYMBOL; IF SY <> IDENT THEN ERROR(2); PROGNAME := ID ; INSYMBOL;
        IF NOT (SY IN [LPARENT,SEMICOLON]) THEN ERROR(14);
        IF SY = LPARENT  THEN
"@@"      BEGIN  PRTERR := FALSE;  (* IGNORE BAD PROG. PARMS *)
            REPEAT INSYMBOL;
              IF SY = IDENT THEN
#               BEGIN  SEARCHID([VARS], LCP) ;
"@@"            IF LCP <> NIL THEN
"@@"              IF LCP@.IDTYPE = TEXTPTR THEN
#                 BEGIN  NEW(LFPTR);
#                 WITH LFPTR@ DO
#                   BEGIN  FILIDPTR := LCP ;  NEXTFILE := FILEHEAD ;
#                   END;
#                 FILEHEAD := LFPTR;
#                 END ;
                INSYMBOL;
                IF NOT ( SY IN [COMMA,RPARENT] ) THEN ERROR(20)
                END
              ELSE ERROR(2)
            UNTIL SY <> COMMA;
            IF SY <> RPARENT THEN ERROR(4);
"@@"        PRTERR := TRUE;
            INSYMBOL
          END;
        IF SY <> SEMICOLON THEN ERROR(14)
        ELSE INSYMBOL;
      END;

"&&" NEW( MAINPROG, PROC, DECLARED );
"&&" WITH MAINPROG@ DO
"&&"   BEGIN  NAME := '$MAINBLK    ';  EXTNAME := '$MAINBLK';  PFNAME := 0;
"&&"          IDTYPE := NIL;  FWDECL := FALSE;  PFLEV := 0;  KLASS := PROC;
"&&"          PFDECKIND := DECLARED;  NEXT := NIL;  NEXT_IN_BKT := NIL;
"&&"          DECL_LEV := 0;  FRTRN := FALSE;  EXTRN := FALSE;
"&&"          IF XLINK THEN
"&&"            BEGIN  EXTRN := TRUE;  EXTNAME(/1/) := '#' END;
"&&"   END;
"E" IF DEBUG_LEV > 0 THEN
"E"     WRITELN(QRR,'% $MAINBLK  0');

"&&" REPEAT BLOCK(FSYS,PERIOD,MAINPROG);
       IF SY <> PERIOD THEN ERROR(21)
     UNTIL SY = PERIOD ;
#   WRITELN(QRR,'#HLT  CALL_RATIO', CALL_LVL[TRUE]:4, CALL_LVL[FALSE]:4,
#                  CALL_LVL[TRUE]+CALL_LVL[FALSE]:4) ;
#   IF ERRINX > 0 THEN  PRINTERROR ;
"SY" IF GET_STAT THEN
"SY"   BEGIN   (* PRINT SYMBOL TABLE STATISTICS *)
"SY"     WRITELN(QRR,'&SYT1 ',FENT_CNT:1,' ',SF_CNT:1,' ',SF_TOT:1,
"SY"             ' ',WE_CNT:1,' ',RE_CNT:1,' ',WS_CNT:1);
"SY"     WRITE(QRR,'&SYT2');
"SY"     FOR I:=0 TO MAXLEVEL DO WRITE(QRR,' ',PROC_CNT[I]:1);
"SY"     WRITELN(QRR);  WRITE(QRR,'&SYT3');
"SY"     FOR I:=0 TO MAXLEVEL DO WRITE(QRR,' ',ENT_CNT[I]:1);
"SY"     FOR I := 0 TO MAXLEVEL DO
"SY"       BEGIN  WRITELN(QRR);  WRITE(QRR,'&SYT4');
"SY"         FOR J:= 0 TO DISPLIMIT DO WRITE(QRR,' ',LU_CNT[I,J]:1);
"SY"       END;
"SY"     FOR I := 1 TO 10 DO
"SY"       BEGIN  WRITELN(QRR);  WRITE(QRR,'&SYT5');
"SY"         FOR J:=1 TO 10 DO WRITE(QRR,' ',WLU_CNT[I,J]:1);
"SY"       END;
"SY"   END;
  END (*PROGRAMME*) ;


  PROCEDURE ENTERSTDTYPES;
"&&" CONST
"&&"   INTTYP: STRUCTURE = (INTSIZE, INTSIZE, SCALAR, STANDARD);
"&&"   REALTYPE: STRUCTURE = (REALSIZE, REALSIZE, SCALAR, STANDARD);
"&&"   CHARTYPE: STRUCTURE = (CHARSIZE, CHARSIZE, SCALAR, STANDARD);
"&&"   BOOLTYPE: STRUCTURE = (BOOLSIZE, BOOLSIZE, SCALAR, DECLARED, NIL);
"&&"   NILTYPE:  STRUCTURE = (PTRSIZE, PTRSIZE, POINTER, NIL);
"&&"   TEXTTYPE: STRUCTURE = (0, PTRSIZE, FILES, NIL);
"&&"   ALFATYPE: STRUCTURE = (ALFALNGTH, CHARSIZE, ARRAYS, NIL, NIL);
"&&"   ALFAINX:  STRUCTURE = (INTSIZE, INTSIZE, SUBRANGE, NIL,
"&&"                          (TRUE,1), (TRUE,ALFALNGTH));
"&&"   UTYP:  IDENTIFIER = (BLANKID, NIL, NIL, NIL, 0, TYPES);
"&&"   UCST:  IDENTIFIER = (BLANKID, NIL, NIL, NIL, 0, KONST, (TRUE, 1));
"&&"   UVAR:  IDENTIFIER = (BLANKID, NIL, NIL, NIL, 0, VARS, ACTUAL, 0, 0);
"&&"   UFLD:  IDENTIFIER = (BLANKID, NIL, NIL, NIL, 0, FIELD, 0, NIL);
"&&"   UPF :  IDENTIFIER = (BLANKID, NIL, NIL, NIL, 0, PROC, DECLARED,
"&&"                        0, 0, NIL, NIL, ACTUAL, FALSE, FALSE, FALSE,
"&&"                        '$UNK_PF ');
"&&"   UREC:  STRUCTURE =  (1, 1, RECORDS, NIL, NIL, 0, 0);
    VAR SP: STP;
    BEGIN                                                (*TYPE UNDERLYING:  *)
                                                         (*****************  *)

"&&"  NEW(INTPTR);  INTPTR@ := INTTYP;                  (*INTEGER*)
"&&"  NEW(REALPTR);  REALPTR@ := REALTYPE;              (*REAL*)
"&&"  NEW(CHARPTR);  CHARPTR@ := CHARTYPE;              (*CHAR*)
"&&"  NEW(BOOLPTR);  BOOLPTR@ := BOOLTYPE;              (*BOOLEAN*)
"&&"  NEW(NILPTR);  NILPTR@ := NILTYPE;                 (*NIL*)
"&&"  NEW(TEXTPTR);  TEXTPTR@ := TEXTTYPE;              (*TEXT*)
      WITH TEXTPTR@ DO
        BEGIN FILTYPE := CHARPTR; SIZE := CHARSIZE+FILHDRSIZE END;
"&&"  NEW(ALFAPTR);  ALFAPTR@ := ALFATYPE;              (*ALFA*)
      WITH ALFAPTR@ DO
"&&"    BEGIN  AELTYPE := CHARPTR;
"&&"          NEW(INXTYPE);  INXTYPE@ := ALFAINX;
        END ;
"&&"  NEW(UTYPPTR);  UTYPPTR@ := UTYP;
"&&"  NEW(UVARPTR);  UVARPTR@ := UVAR;
"&&"  NEW(UFLDPTR);  UFLDPTR@ := UFLD;
"&&"  NEW(UPRCPTR);  UPRCPTR@ := UPF;  GENLABEL(UPRCPTR@.PFNAME);
"&&"  NEW(UFCTPTR);  UFCTPTR@ := UPF;  GENLABEL(UFCTPTR@.PFNAME);
"&&"  NEW(SP);       SP@      := UREC; UFLDPTR@.OWNER := SP;
  END (*ENTERSTDTYPES*) ;

  PROCEDURE ENTSTDNAMES;
"@@" VAR CP,CP1: CTP; I,J: INTEGER;
"&&" CONST
"&&"     NA: ARRAY[1..NPDW] OF ALPHA =
"&&"         ('FALSE       ', 'TRUE        ', '            ', 'PAGE        ',
"&&"          'GET         ', 'PUT         ', 'RESET       ', 'REWRITE     ',
"&&"          'READ        ', 'WRITE       ', 'PACK        ', 'UNPACK      ',
"&&"          'NEW         ', 'RELEASE     ', 'READLN      ', 'WRITELN     ',
"&&"          'MARK        ', 'TRAP        ', 'EXIT        ', 'ABS         ',
"&&"          'SQR         ', 'TRUNC       ', 'ROUND       ', 'ORD         ',
"&&"          'CHR         ', 'PRED        ', 'SUCC        ', 'CLOCK       ',
"&&"          'EOF         ', 'EOLN        ', 'ODD         ', 'EOL         ',
"&&"          'EOT         ', 'TRACE       ', '            ', '            ',
"&&"          '            ', '            ', 'INPUT       ', 'OUTPUT      ',
"&&"          'PRD         ', 'PRR         ', 'QRD         ', 'QRR         ',
"&&"          'DATE        ', 'TIME        ', 'MESSAGE     ', 'SKIP        ',
"&&"          'LINELIMIT   ', 'CARD        ', 'EXPO        ', 'SIN         ',
"&&"          'COS         ', 'EXP         ', 'SQRT        ', 'LN          ',
"&&"          'ARCTAN      '  );
"&&"  XNA: ARRAY(/52..57/) OF ARRAY(/1..EXTNAMSZ/) OF CHAR =
"&&"         ('DSIN    ', 'DCOS    ', 'DEXP    ', 'DSQRT   ',
"&&"          'DLOG    ', 'DATAN   ' );

  BEGIN                                                       (*NAME:         *)
                                                              (******         *)
    NEW(CP,TYPES);                                            (*INTEGER       *)
    WITH CP@ DO
      BEGIN NAME := 'INTEGER     '; IDTYPE := INTPTR; KLASS := TYPES END;
    ENTERID(CP);
    NEW(CP,TYPES);                                            (*REAL          *)
    WITH CP@ DO
      BEGIN NAME := 'REAL        '; IDTYPE := REALPTR; KLASS := TYPES END;
    ENTERID(CP);
    NEW(CP,TYPES);                                            (*CHAR          *)
    WITH CP@ DO
      BEGIN NAME := 'CHAR        '; IDTYPE := CHARPTR; KLASS := TYPES END;
    ENTERID(CP);
    NEW(CP,TYPES);                                            (*BOOLEAN       *)
    WITH CP@ DO
      BEGIN NAME := 'BOOLEAN     '; IDTYPE := BOOLPTR; KLASS := TYPES END;
    ENTERID(CP);
    NEW(CP,TYPES);                                            (*CHAR          *)
    WITH CP@ DO
      BEGIN NAME := 'TEXT        '; IDTYPE := TEXTPTR; KLASS := TYPES END;
    ENTERID(CP);
    NEW(CP,TYPES);                                            (*ALFA          *)
    WITH CP@ DO
      BEGIN NAME := 'ALFA        '; IDTYPE := ALFAPTR;  KLASS := TYPES END ;

    ENTERID(CP);
    NEW(CP,KONST);                                            (*MAXINT        *)
    WITH CP@ DO
      BEGIN NAME := 'MAXINT      '; IDTYPE := INTPTR;  KLASS := KONST;
      VALUES.IVAL := MAXINT;
      END;

    ENTERID(CP);
    CP1 := NIL;
    FOR I := 1 TO 2 DO
      BEGIN NEW(CP,KONST);                                    (*FALSE,TRUE    *)
        WITH CP@ DO
          BEGIN NAME := NA[I]; IDTYPE := BOOLPTR;
            NEXT := CP1; VALUES.IVAL := I - 1; KLASS := KONST
          END;
        ENTERID(CP); CP1 := CP
      END;
    BOOLPTR@.FCONST := CP;

    NEW(CP,KONST);                                             (*NIL          *)
    WITH CP@ DO
      BEGIN NAME := 'NIL         '; IDTYPE := NILPTR;
        NEXT := NIL; VALUES.IVAL := 0; KLASS := KONST
      END;
    ENTERID(CP);

#   FOR I := 39 TO 44 DO
      BEGIN NEW(CP,VARS);                                     (*INPUT,OUTPUT  *)
        WITH CP@ DO                                           (*PRD,PRR       *)
          BEGIN NAME := NA[I]; IDTYPE := TEXTPTR;             (*QRD,QRR       *)
            KLASS := VARS; VKIND := ACTUAL; NEXT := NIL; VLEV := 1;
"@@"        VADDR := FIRSTFILBUF+(I-39)*(FILHDRSIZE+PTRSIZE);
"@@"        IF I <= 40 THEN
"@@"          IF I = 39 THEN INPUTPTR := CP
"@@"            ELSE        OUTPUTPTR := CP;
          END;
        ENTERID(CP)
      END;
#
#   FOR I := 45 TO 46 DO                                  (*DATE, TIME        *)
#     BEGIN NEW(CP,VARS);
#       WITH CP@ DO
#         BEGIN NAME := NA[I]; IDTYPE := ALFAPTR;
#           KLASS := VARS; VKIND := ACTUAL; NEXT := NIL; VLEV := 1;
#           VADDR := TIMEDATELOC+(I-45)*ALFALNGTH  ;
#         END;
#       ENTERID(CP)
#     END;

#   NEW(CP, VARS);      (*OSPARM PTR                                          *)
#                       (*THE REST OF THIS CODE IS TO DEFINE:                 *)
#                       (* VAR:  OSPARM: @ RECORD                             *)
#                       (*                 LENGTH: INTEGER;                   *)
#                       (*                 STRING: ARRAY[1..64] OF CHAR       *)
#                       (*                 END;                               *)
#                       (*                                                    *)
#   WITH CP@ DO
#       BEGIN   NAME := 'OSPARM      ';
#       KLASS := VARS;  VKIND := ACTUAL;  NEXT := NIL;  VLEV := 1;
#       VADDR := OSPARMLOC;
#       ENTERID(CP);
#
#       NEW(IDTYPE, POINTER);
#       WITH IDTYPE@ DO
#         BEGIN   SIZE := PTRSIZE;  ALN := PTRSIZE;  FORM := POINTER;
#
#         NEW(ELTYPE, RECORDS);                      (*TYPE OF THE PARM RECORD*)
#         WITH ELTYPE@ DO
#           BEGIN   SIZE := INTSIZE+ STRGLNGTH*CHARSIZE;  ALN := PTRSIZE;
"&&"        FORM := RECORDS;  RECVAR := NIL;  FLD_DISP_LEV := -1;
"SY"        NO_FLDS := 2;
#
#           NEW(FSTFLD, FIELD);
#             WITH FSTFLD@ DO
#             BEGIN   NAME := 'LENGTH      ';  IDTYPE := INTPTR;   FLDADDR := 0;
#             KLASS := FIELD;
"&&"          TOP := TOP + 1;  (* FIELDS ENTERED AT HIGHER SCOPE *)
"&&"          ENTERID(FSTFLD);
"&&"          OWNER := CP@.IDTYPE@.ELTYPE;
#
#             NEW(NEXT, FIELD);
#             WITH NEXT@ DO
#               BEGIN   NAME := 'STRING      ';
#               FLDADDR := PTRSIZE;  NEXT := NIL;  KLASS := FIELD;
#
#               NEW(IDTYPE, ARRAYS);
#               WITH IDTYPE@ DO
#                 BEGIN  SIZE := STRGLNGTH*CHARSIZE;  ALN := CHARSIZE;
#                 FORM := ARRAYS;  AELTYPE := CHARPTR;
#
#                 NEW(INXTYPE, SUBRANGE);
#                 WITH INXTYPE@ DO
#                   BEGIN  FORM := SUBRANGE;  RANGETYPE := INTPTR;
#                   MIN.IVAL := 1;  MAX.IVAL := STRGLNGTH;
#                   END (*WITH INXTYPE...*);
#                 END (*WITH IDTYPE...*);
#
#               END (*WITH NEXT@...*);
"&&"          ENTERID(NEXT);
"&&"          NEXT@.OWNER := CP@.IDTYPE@.ELTYPE;
"&&"            TOP := TOP - 1;
#             END (*WITH FSTFLD ...*);
#
#           END (*WITH ELTYPE ...*);
#
#         END (*WITH IDTYPE ...*);
#
#       END (*WITH CP ...*);
#
    NEW(CP1,VARS);                     (*PARAMETER OF PREDECLARED FUNCTIONS   *)
    WITH CP1@ DO
      BEGIN NAME := BLANKID; IDTYPE := REALPTR; KLASS := VARS;
        VKIND := ACTUAL; NEXT := NIL; VLEV := 1;
"&&"    VADDR := LCAFTMST + FPSAVEAREA;
      END;

"&&" FOR I := 4 TO 34 DO                                  (*PAGE,GET...TRC*)
#     BEGIN NEW(CP,PROC,STANDARD);                         (*GET,PUT,RESET    *)
#       WITH CP@ DO                                        (*REWRITE,READ     *)
#         BEGIN NAME := NA[I]; IDTYPE := NIL;              (*WRITE,PACK       *)
#           NEXT := NIL; KEY := I - 4;                     (*UNPACK,PACK      *)
"@@"        IF I = 31 (*ODD*) THEN KEY := 33;
#           IF I <= 19 THEN KLASS := PROC ELSE KLASS := FUNC  ;
"&&"        IF I = 34 THEN KLASS := PROC;
#           PFDECKIND := STANDARD;                         (*READLN,WRITELN   *)
#                                                          (*MARK,RELEASE,TRAP*)
#         END;
#       ENTERID(CP)
#     END;

"&&"FOR I := 52 TO 57 DO    (* SIN,COS,EXP,SQRT,LN,ARCTAN *)
"@@"  BEGIN NEW(CP,FUNC,DECLARED);
"@@"    WITH CP@ DO
"@@"      BEGIN
"@@"        NAME := NA[I];  IDTYPE := REALPTR;  NEXT := NIL;
"SH"        PRMPTR := CP1;  FWDECL := FALSE;  EXTRN := FALSE;  FRTRN := TRUE;
"@@"        KLASS := FUNC;    PFDECKIND := DECLARED; PFKIND := ACTUAL;
"@@"        PFLEV := 0;       PFNAME := 0;
"&&"        EXTNAME := XNA[I];
"@@"      END;
"@@"    ENTERID(CP);
"@@"  END;
"@@"
"NH"FOR I := 47 TO 51 DO
"NH"  BEGIN NEW(CP,PROC,STANDARD);
"NH"    WITH CP@ DO
"NH"       BEGIN  NAME := NA[I];  IDTYPE := NIL; NEXT := NIL;
"NH"         IF I <= 49 THEN KLASS := PROC ELSE KLASS := FUNC;
"NH"         KEY := I-12;  PFDECKIND := STANDARD;
"NH"       END;
"NH"    ENTERID(CP);
"NH"  END;

#   NEW(CP,PROC,DECLARED);                          (*SNAPSHOT         *)
#     WITH CP@ DO
#       BEGIN   NAME := 'SNAPSHOT    ';  IDTYPE := NIL;  FRTRN := FALSE ;
#         FWDECL := FALSE ;  EXTRN := TRUE ; PFLEV := 0 ; PFNAME := 0 ;
#         KLASS := PROC; PFDECKIND := DECLARED ;   PFKIND := ACTUAL ;
"@@"      EXTNAME := 'SNAPSHOT' ;  NEXT := NIL;
#       END;
#     ENTERID(CP) ;
#
#   NEW(CP@.PRMPTR,VARS);               (* FIRST PARAMETER OF SNAPSHOT *)
"&&" NEW(CP1,VARS);                     (* SECOND PARAMETER OF SNAPSHOT *)
"&&" WITH CP1@ DO
"&&"   BEGIN  IDTYPE := INTPTR;  KLASS := VARS;  VKIND := ACTUAL;
"&&"     NEXT := NIL;  VLEV := 1;  VADDR := LCAFTMST+FPSAVEAREA+INTSIZE
"&&"   END;
#   WITH CP@.PRMPTR@ DO
#     BEGIN  IDTYPE := INTPTR; KLASS := VARS;  VKIND := ACTUAL;
#       NEXT := CP1;  VLEV := 1;  VADDR := LCAFTMST+FPSAVEAREA
#     END;
  END (*ENTSTDNAMES*) ;


  PROCEDURE INITSCALARS;
  BEGIN   FWPTR := NIL;       STMTNEST := 0;    LISTTAG := ' ';
#      LIST   := TRUE;        PRCODE    := TRUE;
#      PRTERR := TRUE;     ERRINX    := 0;   CONSTLC := -1;
#      HP        := FALSE;    IC     := 0;
#      INTLABEL  := 0;
#      FILEHEAD  := NIL;
#      LC        := FIRSTGVAR;             (*ADR. OF THE FIRST GLOBAL VARIABLE*)
#      (* NOTE IN THE ABOVE RESERVATION OF BUFFER STORE FOR TEXT FILES *)
#      OLDIC     := 0;      IC       := 0 ;       EOL       := TRUE;
#      LINECNT   := 0;      CH       := ' ';      CHCNT     := 0;
#      PAGECNT   := 0;      PLCNT    := PAGESIZE; (* GENERATES FIRST HEADLINE *)
#      LMARGIN   := 0;      RMARGIN  := 80;       BUFEND    := 81;
#      OLDLN     := 0;      MWARN    := FALSE;    LSTOP     := '#';
"&&"   GLOBTESTP := NIL;    OPEN_RECORD := NIL;   LASTLINELISTED := 0;
#      PROGNAME  := '$MAINBLK    ' ;
#      MXINT10   := MAXINT DIV 10;
"&&"   PROCLAB   := 0;      ERRORCNT := 0;         WARNCNT := 0;
"&&"   ASSEMBLE  := FALSE;  NESTCOMM  := FALSE;    ERRKIND := 'E';
#      SAVEREGS  := TRUE ;  SAVEFPRS := TRUE;
"&&"   DEBUG     := TRUE ;  DEBUG_LEV := 2 ;       ASSIGN    := FALSE  ;
"&&"   FLIPDEBUG := FALSE ; EXTUSED   := FALSE;    WARNING := TRUE;
"&&"   DOTFLG    := FALSE ; NOPACKING := FALSE;
#      PACKDATA  := FALSE ; XLINK    := FALSE ;   (*GENERATES UNIQUE NAMES *)
"&&"   PRNTTYPHD := NIL;    PRNTTYNO  := 0 ;
"&&"   FRTPARHD  := NIL;    XLABNO    := 0;
#      GET_STAT  := TRUE ;  ASMVERB  := FALSE ;
"CT"   CTRCNT    := 0 ;     CTROPTION:= FALSE ;
"SY"   FENT_CNT  := 0;      SF_CNT   := 0;       SF_TOT   := 0;
"SY"   WE_CNT    := 0;      RE_CNT   := 0;       WS_CNT := 0;
  END (*INITSCALARS*) ;


  PROCEDURE INITTABLES;
"&&" VAR K: BKT_RNG;  I,J: INTEGER;

    PROCEDURE RATORS;
      VAR I: INTEGER; CH: CHAR;
    BEGIN
      (* THE LIMIT OF THESE LOOP IS CHAR SET DEPENDENT *)
#     FOR CH := chr(0) TO chr(255) DO    UPSHIFT[CH] := CH ;            (*opp*)
"DE"  (* NOP following stmt, to avoid upshifting tilde char, which
"DE"     is in the range 'a'..'z': *)
"DE" (* FOR CH := 'a' TO 'z' DO UPSHIFT[CH] := CHR(ORD(CH) + 64) ; *)    (*UPL*)
      FOR I := 0 TO ORDCHMAX DO  SOP[CHR(I)] := ILLEGCH ;
      FOR CH := 'A' TO 'I' DO    SOP[CH] := ATOZCH ;
      FOR CH := 'J' TO 'R' DO    SOP[CH] := ATOZCH ;
      FOR CH := 'S' TO 'Z' DO    SOP[CH] := ATOZCH ;
      FOR CH := 'a' TO 'i' DO
         BEGIN  SOP[CH] := ATOZCH;
         UPSHIFT[CH] := CHR(ORD(CH) + ORD('A') - ORD('a')) END;         (*opp*)
      FOR CH := 'j' TO 'r' DO
         BEGIN  SOP[CH] := ATOZCH;
         UPSHIFT[CH] := CHR(ORD(CH) + ORD('A') - ORD('a')) END;         (*opp*)
      FOR CH := 's' TO 'z' DO
         BEGIN  SOP[CH] := ATOZCH;
         UPSHIFT[CH] := CHR(ORD(CH) + ORD('A') - ORD('a')) END;         (*opp*)
      FOR CH := '0' TO '9' DO    SOP[CH] := NUMCH ;
      SOP['"']  := DQUOTCH ;
"DE"  SOP[CHLBRACE]  := LBRACE ;
      SOP['#']  := SKIPCH ;
      SOP['$']  := DOLLARCH ;
      SOP[''''] := QUOTCH ;
      SOP['(']  := LPARCH ;
      SOP[')']  := SPECH ;
      SOP[',']  := SPECH ;
      SOP['.']  := DOTCH ;
      SOP[':']  := COLONCH ;
      SOP[';']  := SPECH ;
      SOP['@']  := SPECH ;
      SOP['[']  := SPECH ;
      SOP['|']  := SPECH ;
      SOP[']']  := SPECH ;
      SOP['^']  := SPECH ;
      SOP['_']  := UNDSCH ;
      SOP['+'] := PLUS; SOP['-'] := MINUS;
      SOP['*'] := MUL;  SOP['/'] := RDIV;
      SOP['='] := EQOP;
      SOP['<'] := LTOP; SOP['>'] := GTOP;
#     SOP['|'] := OROP ;  SOP['&'] := ANDOP ;
"DE" (* Note: SSY array should be defined as full range of
        characters, but it's ok as is for now because it is
        currently indexed only for specific characters.
        It it were indexed by higher chars (e.g. backslash or
        curly brackets) in the future, its definition would
        have to change. Currently, SPECH does not include any
        of these higher chars. - D.E. 02feb2007 *)
"&&" SSY(/'+'/) := ADDOP;        SSY(/'-'/) := ADDOP;
"&&" SSY(/'*'/) := MULOP;        SSY(/'/'/) := MULOP;
"&&" SSY(/'('/) := LPARENT;      SSY(/')'/) := RPARENT;
"&&" SSY(/'['/) := LBRACK;       SSY(/']'/) := RBRACK;                  (*opp*)
"&&" SSY(/','/) := COMMA;        SSY(/':'/) := COLON;
"&&" SSY(/'|'/) := ADDOP;        SSY(/'&'/) := MULOP;
"&&" SSY(/'<'/) := RELOP;        SSY(/'>'/) := RELOP;
"&&" SSY(/'='/) := RELOP;        SSY(/'@'/) := ARROW;
"&&" (*SSY(/CHR(74)/) := ARROW;*)                                       (*opp*)
     SSY(/'^'/) := NOTSY;
"&&" SSY(/';'/) := SEMICOLON;    SSY(/'.'/) := PERIOD;
    END (*RATORS*) ;

  BEGIN (*INITTABLES*)
    RATORS;
"&&" FOR I := 0 TO MAXERRLOG DO  ERRLOG(/I/) := (//);  (*CLEAR ERROR LOG*)
"&&" FOR K := 0 TO MAX_BKT DO
"&&"   BUCKET[K] := NIL;
"SY"   FOR I := 0 TO MAXLEVEL DO
"SY"     BEGIN  PROC_CNT[I] := 0;  ENT_CNT[I] := 0;
"SY"      FOR J := 0 TO DISPLIMIT DO  LU_CNT[I,J] := 0;  END;
"SY"   FOR I := 1 TO 10 DO
"SY"     FOR J := 1 TO 10 DO  WLU_CNT[I,J] := 0;
"SY"   PROC_CNT[1] := 1;
  END (*INITTABLES*) ;


BEGIN  (*PASCALCOMPILER*)
  (*INITIALIZE*)
  (************)
  INITSCALARS;  INITTABLES;


  (*ENTER STANDARD NAMES AND STANDARD TYPES:*)
  (******************************************)

  LEVEL := 0; TOP := 0;
  WITH DISPLAY[0] DO
"&&" BEGIN  OCCUR := BLCK; FLABEL := NIL;  END;
  ENTERSTDTYPES;   ENTSTDNAMES;
  TOP := 1; LEVEL := 1;
  WITH DISPLAY[1] DO
"&&" BEGIN  OCCUR := BLCK; FLABEL := NIL;  END;
"SY" GET_STAT := FALSE;

  (*set options passed as parameter to the compiler*)
  (*************************************************)

  IF OSPARM <> NIL THEN
    WITH  OSPARM@  DO
      BEGIN
"DE"  CH "LINEBUF[1]" := CHLBRACE;  LINEBUF[2] := '$';
      IF LENGTH > 64 THEN LENGTH := 64;
      FOR CHCNT := 1 TO LENGTH DO  LINEBUF[CHCNT+2] := STRING[CHCNT];
      (*THE REST OF THE LINE DOES NOT HAVE TO BE CLEARED BUT...*)
      FOR CHCNT := LENGTH TO 77 DO   LINEBUF[CHCNT+3] := ' ';
"DE"  LINEBUF[LENGTH+3] := CHRBRACE;  LINEBUF[LENGTH+4] := '#';
      EOL := FALSE;  CHCNT := 1;  LASTCOL := LENGTH+3;
      END (*WITH OSPARM .., IF OSPARM ...*);

  (*COMPILE:*)
  (**********)

# CTIME := CLOCK(0) ;
# (* FIRST HEADLINE PRINTED BY 'ENDOFLINE' *)
#
# INSYMBOL;
#
# PROGRAMME(BLOCKBEGSYS+STATBEGSYS-[CASESY]);
#
# (* PRINT POST COMPILATION MESSAGES *)
#
# GOODBYE;
#
# END. (*PASCALCOMPILER*)
