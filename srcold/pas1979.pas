(*$D-,N+,L+*)

PROGRAM PASCALCOMPILER(INPUT, OUTPUT, PRR, QRR);

 (******************************************************************
  *                                                                *
  *                                                                *
  *    STEP-WISE DEVELOPMENT OF A PASCAL COMPILER                  *
  *    ******************************************                  *
  *                                                                *
  *                                                                *
  *    STEP 5:   SYNTAX ANALYSIS INCLUDING ERROR                   *
  *              HANDLING; CHECKS BASED ON DECLARA-                *
  *    10/7/73   TIONS; ADDRESS AND CODE GENERATION                *
  *              FOR A HYPOTHETICAL STACK COMPUTER                 *
  *                                                                *
  *                                                                *
  *    AUTHOR:   URS AMMANN                                        *
  *              FACHGRUPPE COMPUTERWISSENSCHAFTEN                 *
  *              EIDG. TECHNISCHE HOCHSCHULE                       *
  *              CH-8006 ZUERICH                                   *
  *                                                                *
  *                                                                *
  *    MODIFICATION OF STEP 5 OF PASCAL COMPILER                   *
  *    *****************************************                   *
  *                                                                *
  *    THE COMPILER IS NOW WRITTEN IN A SUBSET OF                  *
  *    STANDARD PASCAL  -  AS DEFINED IN THE NEW                   *
  *    MANUAL BY K. JENSEN AND N. WIRTH  - AND IT                  *
  *    PROCESSES EXACTLY THIS SUBSET.                              *
  *                                                                *
  *    AUTHOR OF CHANGES:   KESAV NORI                             *
  *                         COMPUTER GROUP                         *
  *                         T.I.F.R.                               *
  *                         HOMI BHABHA ROAD                       *
  *                         BOMBAY - 400005                        *
  *                         INDIA                                  *
  *                                                                *
  *    THESE CHANGES WERE COMPLETED AT ETH, ZURICH                 *
  *    ON 20/5/74.                                                 *
  *                                                                *
  *                                                                *
  *    +++++++++++++++++++++++++++++++++++++++++++                 *
  *                                                                *
  *                                                                *
  *                                                                *
  *    THE COMPILER IS NOW CHANGED TO:                             *
  *    *******************************                             *
  *                                                                *
  *                                                                *
  *      -PRODUCE  THE  INTERMEDIATE  CODE  IN  AN   ASSEMBLER     *
  *      READABLE  FORM  WITH  NO EXTRA SPACES BETWEEN VARIOUS     *
  *      FIELDS, 15-NOV-75                                         *
  *                                                                *
  *      -PRESERVE PROCEDURE NAMES AND THEIR STATIC LEVELS  AT     *
  *      THE  OBJECT  LEVEL,  THUS ALLOWING A SET OF 'DISPLAY'     *
  *      REGISTERS  TO  BE  USED   IN   ACCESSING   NON_LOCAL,     *
  *      NON_GLOBAL  VARIABLES  (INSTEAD  OF  GOING  THROUGH A     *
  *      CHAIN OF POINTERS), 10-DEC-75.                            *
  *                                                                *
  *      -INCLUDE  THE   TYPE   OF   THE   OPERANDS   IN   THE     *
  *      P_INSTRUCTIONS AS FOLLOWS:                                *
  *                                                                *
  *           A : ADDRESS (POINTER) OPERAND                        *
  *           B : BOOLEAN              "                           *
  *           C : CHARACTER            "                           *
  *           I : INTEGER              "                           *
  *           R : REAL                 "                           *
  *           S : SET                  "                           *
  *                                                                *
  *      THE  P_INSTRUCTION  NOW  LOOKS  LIKE:   (LAB)  OPCODE     *
  *      (TYPE),(OPERANDS)   A   NEW    PROCEDURE    'EXIT(RC:     *
  *      INTEGER)'  IS ADDED TO THE SET OF STANDARD PROCEDURES     *
  *      TO FACILITATE TERMINATING A PROGRAM AT ANY POINT  AND     *
  *      RETURNING  A  'RETURN  CODE' TO THE OPERATING SYSTEM,     *
  *      26-JAN-76.                                                *
  *                                                                *
  *      -TREAT THE INPUT AS A TEXT FILE WITH LINES  (RECORDS)     *
  *      OF LINELGTH CHAR.  EACH, THIS ALLOWS A MORE EFFICIENT     *
  *      STRING ORIENTED INPUT, 20-MAR-76.                         *
  *                                                                *
  *      -'READ' OF 'STRING' VARIABLES (I.E.  ARRAY  OF  CHAR)     *
  *      IS  NOW  IMPLEMENTED  AND  IT  IS  TO  COMPLEMENT THE     *
  *      SIMILAR  'WRITE'   FUNCTION.    ALSO   THE   STANDARD     *
  *      PROCEDURE:   TRAP(I:   INTEGER; VAR V:  [ANY TYPE] );     *
  *      IS  ADDED  TO  THE  SET  OF  STANDARD  PROCEDURES  TO     *
  *      FACILITATE  COMMUNICATION  WITH  THE  OUTSIDE  WORLD,     *
  *      10-SEP-76.                                                *
  *                                                                *
  *      -RELEVENT INFORMATION  ON/ABOUT  PROCEDURES  ARE  NOW     *
  *      SENT  TO  'QRD' FILE.  THIS INCLUDES SUCH INFORMATION     *
  *      AS THE SIZE OF THE PROCE- DURE AS WELL  AS  ITS  DATA     *
  *      AREA,  LIST  OF  THE  PROCEDURES  CALLED AND THE # OF     *
  *      CALLS,  THE  LEVEL  OF  THE  HIGHEST_LEVEL  PROCEDURE     *
  *      CALLED ETC.  THIS INFORMATION IS MAINLY INTENDED  FOR     *
  *      INTER_PROCEDURAL  ANALYSIS, BUT IT IS ALSO USEFUL FOR     *
  *      MORE EFFICIENT PROCEDURE ENTRY/EXIT CODE, 22-MAR-77.      *
  *                                                                *
  *      -TO BE COMPATIBLE WITH THE PASCAL-6000 THE FILE  NAME     *
  *      AS  WELL  AS  THE  ARGUMENT  LIST  MAY BE OMITTED FOR     *
  *      CERTAIN I/O RELATED OPERATIONS,  IN  WHICH  CASE  THE     *
  *      APPROPRIATE  DEFAULT  FILE  WILL  BE  USED  FOR  THAT     *
  *      OPERATION.  FOR EXAMPLE STATEMENTS SUCH AS "WRITELN",     *
  *      "WRITELN()"  OR  "WRITELN(OUTPUT)"  WILL HAVE SIMILAR     *
  *      EFFECT.  LIKEWISE "EOF(INPUT)", "EOF()" OR "EOF" WILL     *
  *      TRANSLATE INTO IDENTICAL EXPRESSIONS, 23-MAY-78.          *
  *                                                                *
  *      -TYPES "TEXT" (FILE OF CHAR) AND "ALFA" (PACKED ARRAY     *
  *      [1..10]  OF  CHAR)  ARE  NOW  ADDED  TO  THE  SET  OF     *
  *      PREDEFINED TYPES.  THE (INTEGER) CONSTANT "MAXINT" IS     *
  *      IS ALSO DEFINED AND SET TO  2147483647  =  (2**31)-1,     *
  *      20-MAY-78.                                                *
  *                                                                *
  *      -REAL VALUES MAY BE PRINTED IN SCIENTIFIC NOTATION OR     *
  *      THE  SO  CALLED  F_FORMAT  DEPENDING  ON  THE   FIELD     *
  *      SPECIFICATION  IN  THE 'WRITE' / 'WRITELN' STATEMENT.     *
  *      A SIMPLE FIELD SPECIFIER OF  THE  FORM  "R  :   FLDW"     *
  *      RESULTS  IN E_FORMAT WHILE "R :  FLDW:DFLD" GENERATES     *
  *      AN F_FORMAT OUTPUT, 20-MAY-78.                            *
  *                                                                *
  *      -'EXTERNAL' AND 'FORTRAN' PROCEDURES / FUNCTIONS  ARE     *
  *      NOW SUPPORTED.  IN ORDER TO MAKE THE EXTERNAL (CSECT)     *
  *      AND  INTERNAL  NAMES  IDENTICAL,  THE  NEW 'X' OPTION     *
  *      SWITCH IS INTRODUCED.  IF PROCEDURE / FUNCTION  NAMES     *
  *      IN  A  PROGRAM  ARE  NOT  DISTINCT WITHIN THE FIRST 8     *
  *      CHARACTERS,  (A  PROBLEM  WHICH  WILL   CONFUSE   THE     *
  *      "LOADER")   THE  'X-'  OPTION  WILL  GENERATE  UNIQUE     *
  *      EXTERNAL NAMES FOR ALL PROCEDURES IN THE PROGRAM, AND     *
  *      THESE NAMES SHOULD  BE  USED  FOR  THE  CORRESPONDING     *
  *      EXTERNAL/FORTRAN  ROUTINES,  OTHERWISE ONE SHOULD USE     *
  *      THE 'X+' OPTION TO BE ABLE TO USE THE EXTERNAL  NAMES     *
  *      WITH NO CHANGE, 2-JUNE-78.                                *
  *                                                                *
  *      -THE NEW  OPTION  'N+'  (DEFAULTED  TO  'N-')  PERMITS    *
  *      NESTING  OF 'COMMENTS'. IF THE NESTING IS NOT PROPERLY    *
  *      TERMINATED, PARTS OF THE SOURCE PROGRAM MAY BE TREATED    *
  *      AS COMMENT, OR VICE-VERSA.   FURTHERMORE,  THE  SOURCE    *
  *      PROGRAM  LISTING  GENERATED  BY  THE  COMPILER, IS NOW    *
  *      PAGINATED  AND  THE   COMPILER   CONSTANT   'PAGESIZE'    *
  *      DETERMINES  THE  NUMBER  OF  LINES PER EACH PAGE.  THE    *
  *      GENERATION OF PAGE HEADINGS CAN BE SUPPRESSD, IF  THEY    *
  *      ARE FOUND UNDESIRABLE (E.G.  FOR INTERACTIVE DEVICES),    *
  *      BY  SETTING  THE  'PAGESIZE'  TO  A  LARGE VALUE (E.G.    *
  *      PAGESIZE = 30000;).  YOU MAY ALSO CAUSE A  PAGE  EJECT    *
  *      IN THE SOURCE LISTING BY THE 'OPTION' SWITCH 'E' WHICH    *
  *      TAKES  EFFECT  AS  SOON  AS  IT  APPEARS  IN THE INPUT    *
  *      STREAM, 12-SEPT-78.                                       *
  *                                                                *
  *      -THE STANDARD PROCEDURES 'ROUND'  AND  'PAGE' ARE NOW     *
  *      IMPLEMENTED, 12-SEPT-78.                                  *
  *                                                                *
  *      -The  source  program,  including  the  reserved  and     *
  *      predefined  words  may  now be in upper/lower case to     *
  *      improve the program's readability, 2-Oct-78.              *
  *                                                                *
  *      -Variables DATE, TIME:  ALFA;  are  now  defined  and     *
  *      contain  the  date  and  time of the execution of the     *
  *      program upon entry to the user program, 5-May-79.         *
  *                                                                *
  *      -Predefined       procedures       LINELIMIT(f:TEXT),     *
  *      SKIP(f:TEXT),  LINELIMIT(f:TEXT),  MESSAGE(s:STRING);     *
  *      functions EXPO(r:REAL), CARD(s:SET) and the  variable     *
  *      OSPARM: @   ARRAY[1..64] OF CHAR are now added to the     *
  *      set of predefined names, 11-July-79 "RNH".                *
  *                                                                *
  *      -Standard  procedures  PACK  and   UNPACK   are   now     *
  *      implemented  and they operated on 'PACKED' ad well as     *
  *      unpacked arrays, 10-Oct.-79.                              *
  *                                                                *
  *                                                                *
  *    THE ABOVE CHANGES (INCLUDING ADDITIONS AND/OR  DELETIONS)   *
  *    HAVE BEEN TAGGED BY A '#' TAG AT THE BEGINNING OR THE END   *
  *    OF THE AFFECTED LINES.                                      *
  *                                                                *
  *                                                                *
  *                                                                *
  *                                                                *
  *                         S. HAZEGHI                             *
  *                                                                *
  *                         COMPUTATION RESEARCH GROUP             *
  *                         STANFORD LINEAR ACCELERATOR CENTER     *
  *                         STANFORD, CA. 94305.                   *
  *                                                                *
  *                                                                *
  *                                                                *
  ******************************************************************)

 (******************************************************************
  *                                                                *
  *                                                                *
  *   "BM"+"CM"  --->  IBM/360/370 TRANSLATOR (ASMPCODE)           *
  *   "S1"       --->  STANFORD-1 TRANSLATOR (SOPA-3)              *
  *   "PM"+"CM"  --->  P_CODE INTERPRETER (PINTERP9)               *
  *                                                                *
  *                                                                *
  ******************************************************************)



CONST
#      VERSION     = 'Oct.-79';
#      PAGESIZE    = 55;           (*MAX # OF LINES PER PAGE OF LISTING      *)
#      MAXINT      = 2147483647;
#      MAXADDR     = 16777215;
#      SETMAX      = 63;           (*LARGEST POSSIBLE SET ELEMENT            *)
#      BUFLEN      = 122;          (*MAX LINE LENGTH + 2                     *)
"BM"   INTSIZE     = 4;
"BM"   REALSIZE    = 8;
"BM"   CHARSIZE    = 1;
"BM"   BOOLSIZE    = 1;
"BM"   SETSIZE     = 8;
"BM"   PTRSIZE     = 4;
"S1" " INTSIZE     = 4;                                                        "
"S1" " REALSIZE    = 8;                                                        "
"S1" " CHARSIZE    = 1;                                                        "
"S1" " BOOLSIZE    = 1;                                                        "
"S1" " SETSIZE     = 8;                                                        "
"S1" " PTRSIZE     = 4;                                                        "
"PM" " INTSIZE     = 1;                                                        "
"PM" " REALSIZE    = 1;                                                        "
"PM" " CHARSIZE    = 1;                                                        "
"PM" " BOOLSIZE    = 1;                                                        "
"PM" " SETSIZE     = 1;                                                        "
"PM" " PTRSIZE     = 1;                                                        "
#      REALLNGTH   = 20;           (*STRING REPRESENTATION OF REAL NUMBERS   *)
#      DIGMAX      = 19;           (*REALLNGHT-1                             *)
#      IDLNGTH     = 12;
#      ALFALNGTH   = 10;
#      STRGLNGTH   = 64;
#      DISPLIMIT   = 20;
#      MAXLEVEL    = 10;
#      ORDCHMAX    = 255;          (*SIZE OF CHAR SET OF TARGET MACHINE      *)
"NH"   OPMAX       = 66;           (* OPCODE RANGE  *)
#      MAXERRNR    = 401;          (*MAX VAL OF ERROR CODE                   *)
#      MAXERRLOG   =   8;          (* > (MAXERRNR DIV SETMAX)                *)
#      NRSW        = 37;
#      NRSW1       = 38;           (*NRSW+1                                  *)
"NH"   NSPROC      = 39;           (* # OF STANDARD PROCS                    *)
       NPDW        = 58;           (* # OF PREDEFINED WORDS                  *)
"CT"   CTRMAX      = 16384;

"BM"   (*SAVE AREAS, FUNCTION RETURN VALUE SPACE, DISPLAY AREA, ETC.         *)
"BM"   LCAFTMST    = 80;    FPSAVEAREA  = 32;     RUNCHKAREA  = 96;
"BM"   DSPLYAREA   = 72;    FNCRSLT     = 72;
"BM"   FIRSTFILBUF = 248;          (* = LCAFTMST+RUNCHKAREA+DSPLYAREA        *)
"BM"   FIRSTUSERF  = 254;          (* = FIRSTFILEBUF+6                       *)
"BM"   LASTFILBUF  = 272;          (* LAST USER DEFINED FILE BUFFER+1        *)
"BM"   TIMEDATELOC = 280;          (* LOCATION OF TIME/DATE PREDEF. VARS     *)
"BM"   OSPARMLOC   = 300;          (* LOCATION FOR 'OSPARM' PTR.             *)
"BM"   FIRSTGVAR   = 304;          (* FIRST USER DEFINED GLOBAL VARIABLE     *)

"S1" "                             (* 'S1' CONSTANT DEFINITION               *)"
"S1" " REGPRMAREA  = 40;           (* SHOULD BE A MULTIPLE OF '4' BYTES      *)"
"S1" " LCAFTMST    = 8;   FPSAVEAREA  = 0;   RUNCHKAREA = 0;    DSPLYAREA = 0; "
"S1" " FNCRSLT     = 0;   FIRSTFILBUF = 12;  LASTFILBUF = 44;                  "
"S1" " TIMEDATELOC = 12;           (* TIME/DATE LOCATION                     *)"
"S1" " FIRSTGVAR   = 12;           (* FIRST GLOBAL VARIABLE                  *)"


"PM" " (*SAVE AREAS, FUNCTION RETURN VALUE SPACE, DISPLAY AREA, ETC. FOR P.M.*)"
"PM" " LCAFTMST    = 16;    FPSAVEAREA  =  0;     RUNCHKAREA  =  0;            "
"PM" " DSPLYAREA   =  0;    FNCRSLT     =  0;                                  "
"PM" " FIRSTFILBUF =  6;           (* = LCAFTMST+RUNCHKAREA+DSPLYAREA        *)"
"PM" " FIRSTUSERF  = 12;           (* = FIRSTFILEBUF+6                       *)"
"PM" " LASTFILBUF  = 15;           (* LAST USER DEFINED FILE BUFFER+1        *)"
"PM" " OSPARMLOC   = 16;           (* LOCATION FOR 'OSPARM' PTR.             *)"
"PM" " TIMEDATELOC = 20;           (* LOCATION OF TIME/DATE VARS             *)"
"PM" " FIRSTGVAR   = 40;           (* FIRST GLOBAL VAR ADDRESS               *)"
(*----------------------------------------------------------------------------*)


TYPE                                                        (*DESCRIBING:     *)
                                                            (************     *)


                                                            (*BASIC SYMBOLS   *)
                                                            (**************   *)

     SYMBOL       = (IDENT,INTCONST,REALCONST,STRINGCONST,NOTSY,MULOP,ADDOP,
                     RELOP,LPARENT,RPARENT,LBRACK,RBRACK,COMMA,SEMICOLON,PERIOD,
                     ARROW,COLON,DOTDOT,BECOMES,LABELSY,CONSTSY,TYPESY,VARSY,
                     FUNCSY,PROGSY,PROCSY,SETSY,PACKEDSY,ARRAYSY,RECORDSY,
                     FILESY,FORWARDSY,BEGINSY,IFSY,CASESY,REPEATSY,WHILESY,
                     FORSY,WITHSY,GOTOSY,ENDSY,ELSESY,UNTILSY,OFSY,DOSY,TOSY,
#                    DOWNTOSY,THENSY,FRTRNSY,EXTRNSY,OTHERSY);

     OPERATOR     = (MUL,RDIV,ANDOP,IDIV,IMOD,PLUS,MINUS,OROP,LTOP,LEOP,GEOP,
                     GTOP,NEOP,EQOP,INOP,NOOP,
#                    ATOZCH, NUMCH, QUOTCH, DQUOTCH, COLONCH, DOTCH, LPARCH,
#                    LBRACE, UNDSCH, DOLLARCH, SKIPCH, SPECH, ILLEGCH ) ;

     SETOFSYS     = SET OF SYMBOL;
#    SETRANGE     = SET OF 0..SETMAX;


                                                            (*CONSTANTS       *)
                                                            (**********       *)

     CSTCLASS     = (REEL,PSET,STRG);
     CSP          = @ CONSTANT;
     CONSTANT     = RECORD CASE "CCLASS:" CSTCLASS OF
                           REEL: (RVAL: PACKED ARRAY [1..REALLNGTH] OF CHAR);
                           PSET: (PVAL: SETRANGE  );
                           STRG: (SLNGTH: 0..STRGLNGTH;
                                  SVAL: PACKED ARRAY [1..STRGLNGTH] OF CHAR)
                         END;

     VALU         = RECORD CASE "INTVAL:" BOOLEAN OF  (*INTVAL NEVER USED     *)
                      TRUE:  (IVAL: INTEGER);
                      FALSE: (VALP: CSP)
                    END;

                                                           (*DATA STRUCTURES  *)
                                                           (****************  *)
     LEVRANGE     = 0..MAXLEVEL;       ADDRRANGE   = 0..MAXADDR;
     ALNRNG       = 1..8 ;             LABELRNG    = 0..1000 ;
     STRUCTFORM   = (SCALAR,SUBRANGE,POINTER,POWER,ARRAYS,RECORDS,FILES,
                     TAGFLD,VARIANT);
     DECLKIND     = (STANDARD,DECLARED);
     STP          = @ STRUCTURE;
     CTP          = @ IDENTIFIER;

     STRUCTURE    = PACKED RECORD
                    (* MARKED: BOOLEAN;  TO BE USED WITH 'T+', FOR TEST PHASE *)
                    ALN :   ALNRNG ;      (*ALIGNMENT FACTOR                  *)
                    SIZE: ADDRRANGE;
                    CASE FORM: STRUCTFORM OF
                      SCALAR:   (CASE SCALKIND: DECLKIND OF
                                   DECLARED: (FCONST: CTP));
                      SUBRANGE: (RANGETYPE: STP; MIN,MAX: VALU);
                      POINTER:  (ELTYPE: STP);
                      POWER:    (ELSET: STP);
                      ARRAYS:   (AELTYPE,INXTYPE: STP);
                      RECORDS:  (FSTFLD: CTP; RECVAR: STP);
                      FILES:    (FILTYPE: STP);
                      TAGFLD:   (TAGFIELDP: CTP; FSTVAR: STP);
                      VARIANT:  (NXTVAR,SUBVAR: STP; VARVAL: VALU)
                    END;

                                                            (*NAMES           *)
                                                            (******           *)

     IDCLASS      = (TYPES,KONST,VARS,FIELD,PROC,FUNC);
     SETOFIDS     = SET OF IDCLASS;
     IDKIND       = (ACTUAL,FORMAL);
     ALPHA        = PACKED ARRAY [1..IDLNGTH] OF CHAR;

     IDENTIFIER   = PACKED RECORD
                    NAME: ALPHA; LLINK, RLINK: CTP;
                    IDTYPE: STP; NEXT: CTP;
                    CASE KLASS: IDCLASS OF
                      KONST: (VALUES: VALU);
                      VARS:  (VKIND: IDKIND; "EBCD: BOOLEAN;"
                              VLEV: LEVRANGE; VADDR: ADDRRANGE);
                      FIELD: (FLDADDR: ADDRRANGE);
                      PROC,
                      FUNC:  (CASE PFDECKIND: DECLKIND OF
                               STANDARD: (KEY: 0..NSPROC);
                               DECLARED: (PFLEV: LEVRANGE; PFNAME: LABELRNG;
"S1" "                                     FPRMSZE,RPRMSZE,SPRMSZE: ADDRRANGE; "
                                           CASE PFKIND: IDKIND OF
                                            ACTUAL: (FWDECL, EXTRN,FRTRN,SAVEFP:
                                                     BOOLEAN)))
                    END;


     DISPRANGE    = 0..DISPLIMIT;
     WHERE        = (BLCK,CREC,VREC,REC);

                                                            (*EXPRESSIONS     *)
                                                            (************     *)
     ATTRKIND     = (CST,VARBL,EXPR);
     VACCESS      = (DRCT,INDRCT,INXD);

#    ATTR         = RECORD TYPTR, BTYPE: STP;
                     CASE KIND: ATTRKIND OF
                       CST:   (CVAL: VALU);
                       VARBL: (CASE ACCESS: VACCESS OF
                                 DRCT: (VLEVEL: LEVRANGE; DPLMT: ADDRRANGE);
                                 INDRCT: (IDPLMT: ADDRRANGE))
                     END;

     TESTP        = @ TESTPOINTER;
     TESTPOINTER  = PACKED RECORD
                      ELT1,ELT2 : STP;
                      LASTTESTP : TESTP
                      END;

                                                                 (*LABELS     *)
                                                                 (*******     *)
     LBP          = @ LABL;
     LABL         = RECORD NEXTLAB: LBP; DEFINED: BOOLEAN;
                      LABVAL, LABNAME: INTEGER
                    END;

     FRECPTR      = @FILEREC;
     FILEREC      = RECORD FILIDPTR: CTP; NEXTFILE: FRECPTR; "GEBCDF: BOOL" END;

#    ERRCODE      = 0..MAXERRNR;

"CT"   CTRRANGE   = 0..CTRMAX;
"CT"   CTRTYPE    = (CTRPROC, CTRLBL, CTRGOTO, CTRIF, CTRWHILE, CTRREPEAT,
"CT"                 CTRFOR, CTRCASE);

(*----------------------------------------------------------------------------*)


VAR


                                    (*RETURNED BY SOURCE PROGRAM SCANNER
                                     INSYMBOL:
                                     **********)

    SY: SYMBOL;                     (*LAST SYMBOL                             *)
    OP: OPERATOR;                   (*CLASSIFICATION OF LAST SYMBOL           *)
    VAL: VALU;                      (*VALUE OF LAST CONSTANT                  *)
    LNGTH: INTEGER;                 (*LENGTH OF LAST STRING CONSTANT          *)
    PROGNAME, BLANKID,
    ID:  ALPHA ;                    (*LAST IDENTIFIER (POSSIBLY TRUNCATED)    *)
    CH: CHAR;                       (*LAST CHARACTER READ                     *)
    EOL: BOOLEAN;                   (*END OF LINE FLAG                        *)


                                    (*COUNTERS:                               *)
                                    (**********                               *)

    CHCNT: 0..BUFLEN;               (*CHARACTER COUNTER                       *)
    LC, IC, OLDIC, STIC,            (*DATA LOCATION AND INSTRUCTION COUNTER   *)
    MXDATASZE :         ADDRRANGE;
#   LINECNT, OLDLN, NXTLN, PAGECNT : INTEGER;


                                    (*SWITCHES:                               *)
                                    (**********                               *)

    HP,                             (*HEADER PART                             *)
    DP,                             (*DECLARATION PART                        *)
    PRTERR,                         (*TO ALLOW FORWARD REFERENCES IN PTR TYPE *)
                                    (*DECLARATION BY SUPPRESSING ERROR MSG    *)
#   DOTFLG,                         (*ONE DOT ALREADY SEEN                    *)
#   ASSIGN,PACKDATA,                (*ASSIGNMENT GOING ON, WORD ALIGN FLAG    *)
#   LIST,PRCODE,"PRTABLES,PRTIC,"
#  "MARGIN," DEBUG, MWARN,
#   FLIPDEBUG,
#                                   (*OUTPUT OPTIONS FOR                      *)
#                                   (*  --> SOURCE PROGRAM LISTING            *)
#                                   (*  --> PRINTING SYMBOLIC CODE            *)
#                                   (*  --> DISPLAY IDENT AND STRUCT TABLES   *)
#                                   (*  --> SET INPUT MARGIN AT 72 COLS.      *)
#                                   (*  --> PRINT INST_CNTR, PROCEDURE OPTION *)
#
#   NESTCOMM,
#   ASSEMBLE,ASMVERB,"EBCDFLG,"XLINK, NAMFLAG,
#   SAVEREGS,SAVEFPRS,GET_STAT:     BOOLEAN;
#                                   (*POST PROCESSOR OPTIONS                  *)
#
                                    (*POINTERS:                               *)
                                    (**********                               *)
    INTPTR,REALPTR,CHARPTR,BOOLPTR,
    NILPTR,TEXTPTR,ALFAPTR: STP;    (*POINTERS TO ENTRIES OF STANDARD IDS     *)
    UTYPPTR,UCSTPTR,UVARPTR,
    UFLDPTR,UPRCPTR,UFCTPTR,        (*POINTERS TO ENTRIES FOR UNDECLARED IDS  *)
    FWPTR: CTP;                     (*HEAD OF CHAIN OF FORW DECL TYPE IDS     *)
#   FILEHEAD: FRECPTR ;             (*HEAD OF CHAIN OF EXTERNAL FILES         *)
    GLOBTESTP: TESTP;               (*LAST TESTPOINTER                        *)


                                    (*BOOKKEEPING OF DECLARATION LEVELS:      *)
                                    (***********************************      *)

    LEVEL: LEVRANGE;                (*CURRENT STATIC LEVEL                    *)
    DISX,                           (*LEVEL OF LAST ID SEARCHED BY SEARCHID   *)
    TOP: DISPRANGE;                 (*TOP OF DISPLAY                          *)

    DISPLAY:                        (*WHERE:   MEANS:                         *)
      ARRAY [DISPRANGE] OF
        PACKED RECORD               (*=BLCK:   ID IS VARIABLE ID              *)
          FNAME: CTP; FLABEL: LBP;  (*=CREC:   ID IS FIELD ID IN RECORD WITH  *)
          CASE OCCUR: WHERE OF      (*         CONSTANT ADDRESS               *)
            CREC: (CLEV: LEVRANGE;  (*=VREC:   ID IS FIELD ID IN RECORD WITH  *)
                  CDSPL: ADDRRANGE);(*         VARIABLE ADDRESS               *)
            VREC: (VDSPL: ADDRRANGE)
          END;                      (* --> PROCEDURE WITHSTATEMENT            *)


                                    (*RUN-TIME PROFILER COUNTERS              *)
                                    (***************************              *)

"CT"  CTRCNT     : CTRRANGE;
"CT"  CTRCNTLBL  : LABELRNG;
"CT"  CTROPTION  : BOOLEAN;
"CT"  "FIRSTCTR  : BOOLEAN;"



                                    (*EXPRESSION COMPILATION:                 *)
                                    (************************                 *)

    GATTR        : ATTR;            (*DESCRIBES THE EXPR CURRENTLY COMPILED   *)

    MXINT10      : INTEGER;

                                    (*STRUCTURED CONSTANTS:                   *)
                                    (**********************                   *)

    CONSTBEGSYS,SIMPTYPEBEGSYS,TYPEBEGSYS,BLOCKBEGSYS,SELECTSYS,FACBEGSYS,
    STATBEGSYS,TYPEDELS: SETOFSYS;

# " ATOZ, NUMERIC,
#   ALPHANUMERIC : SET OF CHAR;"    (*VALID ALPHA-NUMERICS                    *)


                                    (*BUFFERS, READ ONLY TABLES ETC.          *)
                                    (******************************           *)

    LSTOP        : CHAR;                      (*MARKS THE BEGINNING OF LINEBUF*)
    LINEBUF      : ARRAY [1..BUFLEN] OF CHAR; (*CURRENT LINE BUFFER           *)
"NH"LMARGIN, RMARGIN, LINELEN, BUFEND,
"NH"LASTCOL      : 0..BUFLEN;                 (*LEFT, RIGHT MARGINS ant PTRS  *)
#  "SEQFLD       : ARRAY [1..8] OF CHAR; (*SEQ. NUM. FIELD USED WITH $M+ ONLY*)"

#   INTLABEL,PROCLAB: LABELRNG ;

#   NXTFILBUF    : ADDRRANGE ;
#   CALL_LVL     : ARRAY[BOOLEAN] OF INTEGER ;

    RW           : ARRAY [1..NRSW(*NR. OF RES. WORDS*)] OF ALPHA;
    FRW          : ARRAY [1..14] OF 1..NRSW1;
    RSY          : ARRAY [1..NRSW(*NR. OF RES. WORDS*)] OF SYMBOL;
    ROP          : ARRAY [1..NRSW(*NR. OF RES. WORDS*)] OF OPERATOR;
    NA           : ARRAY [1..NPDW] OF ALPHA;
    MN           : ARRAY [0..OPMAX] OF PACKED ARRAY [1..4] OF CHAR;
"NH"SNA          : ARRAY [0..NSPROC] OF PACKED ARRAY[1..3] OF CHAR;
    SOP          : ARRAY [CHAR] OF OPERATOR;
#   UPSHIFT      : ARRAY [char (*SHOULD BE CHAR*) ] OF CHAR ;        (*opp*)
    SSY          : ARRAY [char (*SHOULD BE CHAR*)] OF SYMBOL;        (*opp*)
#   ERRLOG       : ARRAY [0..MAXERRLOG(* = 400 DIV SETMAX+1*)] OF SETRANGE;
""" EBCDIC       : ARRAY [CHAR] OF 0..255 ;" (*CHAR CODE CONVERSION           *)
# (*INPUT   : TEXT ;         FILE CONTAINING SOURCE PROGRAM                   *)
# (*OUTPUT  : TEXT ;         FILE USED FOR SOURCE LISTING                     *)
# (*PRR     : TEXT ;         FILE TO SEND P_CODE TO                           *)
# (*PRD     : TEXT ;         FILE TO READ TEXT OF ERROR MESSAGES FROM         *)
# (*QRR     : TEXT ;         FILE FOR :   PROCEDURE TABLES                    *)
# (*QRR     : TEXT ;                      SYMBOL TABLE                        *)
# (*QRR     : TEXT ;         AND          COUNTER TABLE.                      *)


                                    (*ERROR MESSAGES:                         *)
                                    (****************                         *)

#   ERRORCNT, CTIME: INTEGER ;      (*ERROR_COUNT, COMPILATION_TIME           *)
    ERRINX       : 0..10;           (*NR OF ERRORS IN CURRENT SOURCE LINE     *)
    ERRLIST      : ARRAY[1..10] OF
                     PACKED RECORD POS: 1..81;
                       NMR: 1..400
                     END;

"S1" "  FPRM1, SPRM1, RPRM1 : ADDRRANGE ;   REGS_FULL : BOOLEAN;               "

(*----------------------------------------------------------------------------*)



  PROCEDURE ERROR(FERRNR: ERRCODE);
  VAR  I : 0..10 (*MAXERRNR DIV SETMAX*) ;
  BEGIN
    IF ERRINX >= 9 THEN
      BEGIN ERRLIST[10].NMR := 255; ERRINX := 10 END
    ELSE
      BEGIN ERRINX := ERRINX + 1;
        ERRLIST[ERRINX].NMR := FERRNR
      END;
    ERRLIST[ERRINX].POS := CHCNT ;
#   I := FERRNR DIV (SETMAX+1) ;
#   ERRLOG[I] := ERRLOG[I]+ [FERRNR MOD (SETMAX+1)] ;
#   ERRORCNT := ERRORCNT+1 ;
  END (*ERROR*) ;


PROCEDURE PRINTERROR ;
    VAR LASTPOS,FREEPOS,CURRPOS,CURRNMR,F,K: INTEGER;
# BEGIN
#   IF NOT LIST THEN
#     BEGIN   NXTLN := NXTLN+1 ;
#    "IF MARGIN THEN  WRITE(OUTPUT, SEQFLD:9)  ELSE  WRITE(OUTPUT,LINECNT:9);"
#     WRITELN(OUTPUT, LINECNT:9, ') ':13, LINEBUF:LINELEN);
#     END ;
#   (*OUTPUT ERROR CODES*)
#   NXTLN := NXTLN+1 ;
#   WRITE(OUTPUT,'****':9, ' ':13) ;
#   LASTPOS := 0;  FREEPOS := 1;
#   FOR K := 1 TO ERRINX DO
#     BEGIN
#       WITH ERRLIST[K] DO
#         BEGIN CURRPOS := POS; CURRNMR := NMR END;
#       IF CURRPOS = LASTPOS THEN WRITE(OUTPUT,',')
#       ELSE
#         BEGIN
#           WHILE FREEPOS < CURRPOS DO
#             BEGIN WRITE(OUTPUT,' '); FREEPOS := FREEPOS + 1 END;
#           WRITE(OUTPUT,'@');
#           LASTPOS := CURRPOS
#         END;
#       IF CURRNMR < 10 THEN F := 1
#       ELSE IF CURRNMR < 100 THEN F := 2
#         ELSE F := 3;
#       WRITE(OUTPUT,CURRNMR:F);
#       FREEPOS := FREEPOS + F + 1
#     END;
#   WRITELN(OUTPUT);  ERRINX := 0 ;  PRCODE := FALSE ;
# END (*PRINTERROR*) ;


PROCEDURE HEADLINE ;
# BEGIN
#   PAGECNT := PAGECNT+1 ;  NXTLN := 0 ;
#   WRITELN(OUTPUT, '1  LINE #  P/D LC  LVL',
#                   '< Stanford PASCAL Compiler, Version of ':44,
#                   VERSION, ' >', TIME:14, DATE, 'Page':8, PAGECNT:4) ;
#   WRITELN(OUTPUT, '------  ------  ---':22,
#                   '---- ---':89) ;
#   WRITELN(OUTPUT) ;
# END (*HEADLINE*) ;


PROCEDURE ENDOFLINE ;
# LABEL 10;
# VAR I: 1..9 ;
  BEGIN   IF ERRINX > 0 THEN PRINTERROR ;
#   READLN(INPUT, LINEBUF);
#  "IF MARGIN THEN
#     FOR I := 1 TO 8 DO
#       BEGIN  SEQFLD[I] := LINEBUF[72+I] ; LINEBUF[72+I] := ' '  END ;"
    LINELEN := BUFEND;   (*THIS WILL SPEED THINGS UP IF NO MARGIN IS SET/RESET*)

    (*RUN TIME CHECK I.E. '$D+' SHOULD NOT BE ENABLED FOR THIS LOOP!*)

    REPEAT  LINELEN := LINELEN - 1; "IF LINELEN = 0 THEN GOTO 10;"
    UNTIL   LINEBUF(/LINELEN/) <> ' ';
10:"LINELEN := LINELEN + 1;" (*SO THAT LAST INPUT CHAR = BLANK, SERVES AS EOL*)

    IF LINELEN > RMARGIN THEN  BEGIN  MWARN := TRUE;  LASTCOL := RMARGIN END
    ELSE LASTCOL := LINELEN;
#   LINECNT := LINECNT+1 ;
#   IF LIST THEN
#     BEGIN
#       IF NXTLN >= PAGESIZE THEN  HEADLINE ;
#       NXTLN := NXTLN+1 ;
#      "IF MARGIN THEN  WRITE(OUTPUT, SEQFLD:9)
#       ELSE" WRITE(OUTPUT,LINECNT: 9) ;
#
#       IF IC > 0 THEN
#         BEGIN  WRITE(OUTPUT, IC:8, LEVEL:3);
#         "IF HP THEN BEGIN  IC := 0;  HP := FALSE END;"
#         END
#       ELSE
#         BEGIN
#         IF DP THEN  WRITE(OUTPUT, LC:8, LEVEL:3)
#         ELSE  (*WRITE(OUTPUT, 0:8, 0:3)*)  WRITE(OUTPUT, ' ':11)
#         END;
#
#       WRITELN(OUTPUT, ') ', LINEBUF:LINELEN) ;
#     END;
#   IF HP THEN BEGIN  IC := 0;  HP := FALSE END;
#   LINEBUF[LASTCOL+1] := '#';    (*TO STOP 'SKIPBLNK' + PROVIDE VALID EOL CH.*)
#   CHCNT := LMARGIN;
  END  (*ENDOFLINE*) ;


# PROCEDURE LISTMSGS ;
#
#   VAR I, J : ERRCODE ;
#       MSG  : ARRAY[1..64] OF CHAR ;
#
#   BEGIN
#      WRITELN(OUTPUT) ;   WRITELN(OUTPUT) ;
#      WRITELN(OUTPUT, '****':9, '   ERROR CODES FOR THIS PROGRAM :') ;
#      WRITELN(OUTPUT) ;   RESET(PRD) ;   J := 0 ;
#      FOR I := 1 TO MAXERRNR DO
#        IF (I MOD (SETMAX+1)) IN ERRLOG[I DIV (SETMAX+1)] THEN
#           BEGIN
#           WHILE (NOT EOF(PRD)) AND (I > J) DO  READLN(PRD, J, MSG) ;
#           IF J = I THEN  WRITELN('****':9, J:6, MSG) ;
#           END ;
#   END (*LISTMSGS*);


# PROCEDURE GOODBYE;
#
#   BEGIN
#   CTIME := (CLOCK(1)-CTIME)" DIV 10" ;
#   WRITELN(OUTPUT);
#   IF MWARN THEN
#      WRITELN(OUTPUT, '0', '    ****      CONTENTS OF SOURCE LINES OUTSIDE  ',
#                      LMARGIN:1, '..', RMARGIN:1, '  MARGINS IGNORED.');
#
#   WRITELN(OUTPUT);
#   IF ERRORCNT = 0 THEN  WRITE(OUTPUT, '****      NO':17)
#   ELSE  WRITE(OUTPUT, '****':9, ERRORCNT:8) ;
#   WRITELN(OUTPUT, '  SYNTAX ERROR(S) DETECTED.');
#   WRITELN(OUTPUT);
#   WRITELN(OUTPUT, '****':9, LINECNT:8, '  LINE(S) READ, ', PROCLAB:4,
#                   ' PROCEDURE(S) COMPILED,');
#   WRITELN(OUTPUT);
#   WRITELN(OUTPUT, '****':9, OLDIC:8,'  P_INSTRUCTIONS GENERATED,',
#                   "CTIME DIV 100 :4, '.', CTIME MOD 100:2,"
#                   ctime*0.001:7:2, ' SECONDS IN COMPILATION.') ;
#
#   IF ERRORCNT > 0 THEN  LISTMSGS ;
#   EXIT(ERRORCNT) ;
#   END (*GOODBYE*) ;


  PROCEDURE INSYMBOL;
    (*READ NEXT BASIC SYMBOL OF SOURCE PROGRAM AND RETURN ITS
    DESCRIPTION IN THE GLOBAL VARIABLES SY, OP, ID, VAL AND LNGTH*)
    LABEL 1,2,3;
    VAR I,K: INTEGER;
        DIGIT: PACKED ARRAY [1..REALLNGTH] OF CHAR;
        STRING: PACKED ARRAY [1..STRGLNGTH] OF CHAR;
        LVP: CSP;  TEST: BOOLEAN;


#   PROCEDURE EOFEXIT;
#
#     BEGIN
#     ERROR(390);
#     PRINTERROR;
#     GOODBYE;
#     END (*EOFEXIT*) ;


#   PROCEDURE SKIPBLNK;
#   (* SKIP BLANKS, ENDOFLINE, AND (OPTIONAL) MARGIN, SKIPS AT LEAST ONE CHAR *)
#
#     BEGIN
#       REPEAT
#
#       IF EOL THEN
#         BEGIN
#         IF EOF(INPUT)  THEN  EOFEXIT ;
#         ENDOFLINE ;
#         END ;
#
#     "IF CHCNT < LASTCOL THEN"
#         REPEAT CHCNT := CHCNT+1;  UNTIL LINEBUF[CHCNT] <> ' ';
#      (* NOTE THAT LINEBUF[LINELEN+1] <> ' ' *)
#      EOL := CHCNT > LASTCOL;
#      UNTIL NOT EOL ;
#    CH := LINEBUF[CHCNT] ;
#    END (*SKIPBLNK*) ;


    PROCEDURE NEXTCH;
#     BEGIN
#     IF EOL THEN
#        BEGIN
#        IF EOF(INPUT)  THEN  EOFEXIT ;
#        ENDOFLINE ;
#        END ;
#     CHCNT := CHCNT+1;  EOL := (CHCNT > LASTCOL);
#     CH := LINEBUF[CHCNT] ;
#     END;


#     PROCEDURE OPTIONS(CCH: CHAR (*COMMENT TERMINATOR CH*) );
#
#     FUNCTION DECNUM : INTEGER;
#       VAR NUM, D: INTEGER;
#       BEGIN
#          NUM := 0;  NEXTCH;
#          WHILE CH >= '0' DO
#             BEGIN  NUM := NUM*10+ORD(CH)-ORD('0');  NEXTCH  END;
#          DECNUM := NUM
#       END;
#
#     BEGIN
#       REPEAT  NEXTCH;  CH := UPSHIFT[CH] ;                             (*UPL*)
#         IF CH <> CCH THEN
#           BEGIN
#             IF CH = 'T' THEN
#               BEGIN NEXTCH; "PRTABLES := CH = '+'"  END
#             ELSE
#               IF CH = 'L' THEN
#                 BEGIN NEXTCH; LIST := CH = '+';
#                "  IF NOT LIST THEN WRITELN(OUTPUT) "
#                 END
#               ELSE
#                 IF CH = 'C' THEN
#                   BEGIN NEXTCH; PRCODE := CH <> '-' END
#                 ELSE
#                   IF CH = 'E' THEN
#                     BEGIN  IF LIST THEN HEADLINE ;  "NEXTCH ;
#                     EBCDFLG := CH = '+' ;"
#                     END
#                   ELSE
#                     IF CH = 'A' THEN
#                       BEGIN  NEXTCH ;  ASSEMBLE := CH ='+'  END
#                     ELSE
#                       IF CH='M' THEN
                          BEGIN  NEXTCH;
                             IF CH = '+' THEN BEGIN
                                LMARGIN := 0;  RMARGIN := 72;  END
                             ELSE IF CH = '-' THEN BEGIN
                                LMARGIN := 0;  BUFEND := BUFLEN;
                                RMARGIN := BUFLEN;
                                END
                             ELSE IF CH = '(' THEN BEGIN
                                LMARGIN := DECNUM - 1;
                                IF LMARGIN < 0 THEN LMARGIN := 0;
                                IF CH = ',' THEN  RMARGIN := DECNUM
                                            ELSE  RMARGIN := BUFLEN;
                                IF (RMARGIN <= LMARGIN) OR
                                   (RMARGIN >= BUFLEN) THEN
                                      RMARGIN := BUFLEN-1;
                                BUFEND := BUFLEN;
                                END
                          END
#                       ELSE
#                         IF CH = 'S' THEN
#                           BEGIN  NEXTCH ;  SAVEREGS := CH <> '-'  END
#                         ELSE
#                           IF CH = 'F' THEN
#                             BEGIN NEXTCH ;  SAVEFPRS := CH <> '-' ;
#                             END
#                           ELSE
#                             IF CH = 'D' THEN
#                               BEGIN  NEXTCH ;  DEBUG := CH <> '-' END
#                             ELSE
#                               IF CH = 'P' THEN
#                                 BEGIN  NEXTCH ;  PACKDATA := CH = '+' ;
#                                   IF PACKDATA THEN  MXDATASZE := INTSIZE
#                                   ELSE  MXDATASZE := REALSIZE ;
#                                 END
#                               ELSE
#                               " IF CH = 'B' THEN
#                                   BEGIN  NEXTCH ;  BYTEON := CH = '+' ;
#                                   DEBUG := BYTEON ;
#                                   END
#                                 ELSE "
#                                   IF CH = 'V' THEN
#                                     BEGIN  NEXTCH ;  ASMVERB := CH ='+' END
#                                   ELSE
#                                     IF CH = 'U' THEN
#                                        BEGIN  NEXTCH; GET_STAT := CH = '+' END
#                                     ELSE
#                                       IF CH = 'X' THEN
#                                          BEGIN  NEXTCH;  XLINK := CH = '+';
#                                          (* XLINK --> ALLOW '$' AS FIRST CH.*)
#                                          IF XLINK THEN  SOP['$'] := ATOZCH;
#                                          END
#                                       ELSE
#                                         IF CH = 'K' THEN
#                                           BEGIN   NEXTCH;
"CT"                                         CTROPTION := CH = '+' ;
"CT"                                        "IF CTROPTION THEN  REWRITE(QRR) ;"
#                                           END
#                                         ELSE IF CH = 'N' THEN
#                                           BEGIN
#                                           NEXTCH ;   NESTCOMM := CH = '+' ;
#                                           END ;
#             IF CH <> CCH THEN NEXTCH;
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
#           IF CH = '¯' THEN
#              IF CCH = 'ò' THEN  BEGIN  EOL := FALSE; COMMENT('ò')  END;
#           IF CH = '(' THEN
#              IF LINEBUF[CHCNT+1] = '*' THEN
#                 IF CCH = '*' THEN  (*TO PREVENT '(ò' FROM GOING THROUGH*)
#                    BEGIN  EOL := FALSE;  NEXTCH;  NEXTCH;  COMMENT('*')  END;
#           END;
#
#         IF CHCNT > LASTCOL THEN
#           BEGIN  IF EOF(INPUT) THEN  EOFEXIT ;
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
  "   'A','B','C','D','E','F','G','H','I',
      'J','K','L','M','N','O','P','Q','R',
      'S','T','U','V','W','X','Y','Z'  "
#     ATOZCH :
#       BEGIN   K := 0 ;   ID := BLANKID ;
#
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

  "   '0','1','2','3','4','5','6','7','8','9'  "
#     NUMCH :
        BEGIN OP := NOOP; I := 0;
          REPEAT I := I+1; IF I<= DIGMAX THEN DIGIT[I] := CH; NEXTCH
          UNTIL SOP[CH] <> NUMCH ;
          IF (CH = '.') OR (UPSHIFT[CH] (*CH*) = 'E') THEN
            BEGIN
                  K := I;
                  IF CH = '.' THEN
                    BEGIN K := K+1; IF K <= DIGMAX THEN DIGIT[K] := CH;
                      NEXTCH;
#                     IF CH = '.' THEN BEGIN  DOTFLG := TRUE;  GOTO 3 END;
                      IF SOP[CH] <> NUMCH THEN
                        ERROR(201)
                      ELSE
                        REPEAT K := K + 1;
                          IF K <= DIGMAX THEN DIGIT[K] := CH; NEXTCH
                        UNTIL SOP[CH] <> NUMCH
                    END;
#                 IF UPSHIFT[CH]  (*CH*) = 'E' THEN                      (*UPL*)
                    BEGIN K := K+1; IF K <= DIGMAX THEN DIGIT[K] := CH;
                      NEXTCH;
                      IF (CH = '+') OR (CH ='-') THEN
                        BEGIN K := K+1; IF K <= DIGMAX THEN DIGIT[K] := CH;
                          NEXTCH
                        END;
                      IF SOP[CH] <> NUMCH THEN
                        ERROR(201)
                      ELSE
                        REPEAT K := K+1;
                          IF K <= DIGMAX THEN DIGIT[K] := CH; NEXTCH
                        UNTIL SOP[CH] <> NUMCH
                     END;
                   NEW(LVP,REEL); SY:= REALCONST; "LVP@.CCLASS := REEL;"
                   WITH LVP@ DO
                     BEGIN FOR I := 1 TO REALLNGTH DO RVAL[I] := ' ';
                       IF K <= DIGMAX THEN
                         FOR I := 2 TO K + 1 DO RVAL[I] := DIGIT[I-1]
                       ELSE BEGIN ERROR(203); RVAL[2] := '0';
                              RVAL[3] := '.'; RVAL[4] := '0'
                            END
                     END;
                   VAL.VALP := LVP
            END
          ELSE
  3:        BEGIN
              IF I > DIGMAX THEN BEGIN ERROR(203); VAL.IVAL := 0 END
              ELSE
                WITH VAL DO
                  BEGIN IVAL := 0;
                    FOR K := 1 TO I DO
                      BEGIN
                        IF IVAL <= MXINT10 THEN
                          IVAL := IVAL*10 + (ORD(DIGIT[K])-ORD('0'))
                        ELSE BEGIN ERROR(203); IVAL := 0 END
                      END;
                    SY := INTCONST
                 END
            END
        END;

#     QUOTCH   "''''  " :
        BEGIN LNGTH := 0; SY := STRINGCONST;  OP := NOOP;
          REPEAT
            REPEAT NEXTCH; LNGTH := LNGTH + 1;
                   IF LNGTH <= STRGLNGTH THEN STRING[LNGTH] := CH
            UNTIL (EOL) OR (CH = '''');
            IF EOL THEN ERROR(202) ELSE NEXTCH
          UNTIL CH <> '''';
          LNGTH := LNGTH - 1;   (*NOW LNGTH = NR OF CHARS IN STRING*)
          IF LNGTH = 1 THEN VAL.IVAL := ORD(STRING[1])
          ELSE
            BEGIN   NEW(LVP,STRG);  "LVP@.CLASS := STRG ;"
              IF LNGTH > STRGLNGTH THEN
                BEGIN ERROR(398); LNGTH := STRGLNGTH END;
#             IF LNGTH <= 0 THEN ERROR(205) ;
              WITH  LVP@  DO
                BEGIN SLNGTH := LNGTH;
                  FOR I := 1 TO LNGTH DO SVAL[I] := STRING[I]
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
          " BEGIN NEXTCH;
              IF CH = '$' THEN OPTIONS;
              REPEAT
                WHILE CH <> '*'  DO NEXTCH;
                NEXTCH
              UNTIL CH = ')';
              NEXTCH; GOTO 1
            END ; "

#        IF CH = '/' THEN
#          BEGIN   SY := LBRACK ;  OP := NOOP ;
#          NEXTCH
#          END
         ELSE  BEGIN  SY := LPARENT; OP := NOOP  END
       END;

  "   '*','+','-',
      '=','/',')','&','×','^',
#     '[',']',',',';','@'   "
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

#     LBRACE (* '¯' *) :
#       BEGIN  NEXTCH;
        IF CH = '$' THEN OPTIONS('ò');
        COMMENT('ò');  NEXTCH;  GOTO 1;
        END;

#     SKIPCH   "'#'  " :
#       BEGIN  NEXTCH ;  GOTO 1  END ;
#
#     ILLEGCH, DOLLARCH, UNDSCH   "'Ö','_','$'  ":
#       BEGIN SY := OTHERSY; OP := NOOP; ERROR(6) ; NEXTCH END

    END (*CASE*)

  END (*INSYMBOL*) ;


  PROCEDURE ENTERID(FCP: CTP);
    (*ENTER ID POINTED AT BY FCP INTO THE NAME-TABLE,
     WHICH ON EACH DECLARATION LEVEL IS ORGANISED AS
     AN UNBALANCED BINARY TREE*)
    VAR NAM: ALPHA; LCP, LCP1: CTP; LLEFT: BOOLEAN;
  BEGIN NAM := FCP@.NAME;
    LCP := DISPLAY[TOP].FNAME;
    IF LCP = NIL THEN
      DISPLAY[TOP].FNAME := FCP
    ELSE
      BEGIN
        REPEAT LCP1 := LCP;
          IF LCP@.NAME = NAM THEN   (*NAME CONFLICT, FOLLOW RIGHT LINK*)
            BEGIN ERROR(101); LCP := LCP@.RLINK; LLEFT := FALSE END
          ELSE
            IF LCP@.NAME < NAM THEN
              BEGIN LCP := LCP@.RLINK; LLEFT := FALSE END
            ELSE BEGIN LCP := LCP@.LLINK; LLEFT := TRUE END
        UNTIL LCP = NIL;
        IF LLEFT THEN LCP1@.LLINK := FCP ELSE LCP1@.RLINK := FCP
      END;
    FCP@.LLINK := NIL; FCP@.RLINK := NIL
  END (*ENTERID*) ;


  PROCEDURE SEARCHSECTION(FCP: CTP; VAR FCP1: CTP);
    (*TO FIND RECORD FIELDS AND FORWARD DECLARED PROCEDURE ID'S
     --> PROCEDURE PROCEDUREDECLARATION
     --> PROCEDURE SELECTOR*)
     LABEL 1;
  BEGIN
    WHILE FCP <> NIL DO
      IF FCP@.NAME = ID THEN GOTO 1
      ELSE IF FCP@.NAME < ID THEN FCP := FCP@.RLINK
        ELSE FCP := FCP@.LLINK;
1:  FCP1 := FCP
  END (*SEARCHSECTION*) ;


  PROCEDURE SEARCHID(FIDCLS: SETOFIDS; VAR FCP: CTP);
    LABEL 1;
    VAR LCP: CTP;
  BEGIN
    FOR DISX := TOP DOWNTO 0 DO
      BEGIN LCP := DISPLAY[DISX].FNAME;
        WHILE LCP <> NIL DO
          IF LCP@.NAME = ID THEN
            IF LCP@.KLASS IN FIDCLS THEN GOTO 1
            ELSE
              BEGIN IF PRTERR THEN ERROR(103);
                LCP := LCP@.RLINK
              END
          ELSE
            IF LCP@.NAME < ID THEN
              LCP := LCP@.RLINK
            ELSE LCP := LCP@.LLINK
      END;
    (*SEARCH NOT SUCCSESSFUL; SUPPRESS ERROR MESSAGE IN CASE
     OF FORWARD REFERENCED TYPE ID IN POINTER TYPE DEFINITION
     --> PROCEDURE SIMPLETYPE*)
    IF PRTERR THEN
      BEGIN ERROR(104);
        (*TO AVOID RETURNING NIL, REFERENCE AN ENTRY
         FOR AN UNDECLARED ID OF APPROPRIATE CLASS
         --> PROCEDURE ENTERUNDECL*)
        IF TYPES IN FIDCLS THEN LCP := UTYPPTR
        ELSE
          IF VARS IN FIDCLS THEN LCP := UVARPTR
          ELSE
            IF FIELD IN FIDCLS THEN LCP := UFLDPTR
            ELSE
              IF KONST IN FIDCLS THEN LCP := UCSTPTR
              ELSE
                IF PROC IN FIDCLS THEN LCP := UPRCPTR
                ELSE LCP := UFCTPTR;
      END;
1:  FCP := LCP
  END (*SEARCHID*) ;


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
#         IF FSP = CHARPTR THEN "IF BYTEON THEN" FMAX := ORDCHMAX "ELSEFMAX:=63"
          ELSE
            IF (FORM = SCALAR) AND (FSP@.FCONST <> NIL) THEN
              FMAX := FSP@.FCONST@.VALUES.IVAL
            ELSE FMAX := 0
        END
  END (*GETBOUNDS*) ;


" PROCEDURE PRINTTABLES(FB: BOOLEAN);
    (*PRINT DATA STRUCTURE AND NAME TABLE*)
    VAR I, LIM: DISPRANGE;

    PROCEDURE MARKER;
      (*MARK DATA STRUCTURE ENTRIES TO AVOID MULTIPLE PRINTOUT*)
      VAR I: INTEGER;

      PROCEDURE MARKCTP(FP: CTP); FORWARD;

      PROCEDURE MARKSTP(FP: STP);
        (*MARK DATA STRUCTURES, PREVENT CYCLES*)
      BEGIN
        IF FP <> NIL THEN
          WITH FP@ DO
            BEGIN MARKED := TRUE;
              CASE FORM OF
              SCALAR:   ;
              SUBRANGE: MARKSTP(RANGETYPE);
              POINTER:  (*DON'T MARK ELTYPE: CYCLE POSSIBLE; WILL BE MARKED
                        ANYWAY, IF FP = TRUE*) ;
              POWER:    MARKSTP(ELSET) ;
              ARRAYS:   BEGIN MARKSTP(AELTYPE); MARKSTP(INXTYPE) END;
              RECORDS:  BEGIN MARKCTP(FSTFLD); MARKSTP(RECVAR) END;
              FILES:    MARKSTP(FILTYPE);
              TAGFLD:   MARKSTP(FSTVAR);
              VARIANT:  BEGIN MARKSTP(NXTVAR); MARKSTP(SUBVAR) END
              END (*CASE*)
            END (*WITH*)
      END (*MARKSTP*);

      PROCEDURE MARKCTP;
      BEGIN
        IF FP <> NIL THEN
          WITH FP@ DO
            BEGIN MARKCTP(LLINK); MARKCTP(RLINK);
              MARKSTP(IDTYPE)
            END
      END (*MARKCTP*);

    BEGIN (*MARK*)
      FOR I := TOP DOWNTO LIM DO
        MARKCTP(DISPLAY[I].FNAME)
    END (*MARK*);

    PROCEDURE FOLLOWCTP(FP: CTP); FORWARD;

    PROCEDURE FOLLOWSTP(FP: STP);
    BEGIN
      IF FP <> NIL THEN
        WITH FP@ DO
          IF MARKED THEN
            BEGIN MARKED := FALSE; WRITE(OUTPUT,' ':4,ORD(FP):6,SIZE:10);
              CASE FORM OF
              SCALAR:   BEGIN WRITE(OUTPUT,'SCALAR':10);
                          IF SCALKIND = STANDARD THEN
                           WRITE(OUTPUT,'STANDARD    ':10)
                          ELSE WRITE(OUTPUT,'DECLARED    ':10, ORD(FCONST):8);
                          WRITELN(OUTPUT)
                        END;
              SUBRANGE:BEGIN
                        WRITE(OUTPUT,'SUBRANGE    ':10,' ':4,ORD(RANGETYPE):6);
                            IF RANGETYPE <> REALPTR THEN
                              WRITE(OUTPUT,MIN.IVAL,MAX.IVAL)
                            ELSE
                              IF (MIN.VALP <> NIL) AND (MAX.VALP <> NIL) THEN
                                WRITE(OUTPUT,' ',MIN.VALP@.RVAL:9,
                                      ' ',MAX.VALP@.RVAL:9);
                            WRITELN(OUTPUT); FOLLOWSTP(RANGETYPE);
                          END;
              POINTER:  WRITELN(OUTPUT,'POINTER':10,' ':4,ORD(ELTYPE):6);
              POWER:    BEGIN WRITELN(OUTPUT,'SET':10,' ':4,ORD(ELSET):6);
                            FOLLOWSTP(ELSET)
                          END;
              ARRAYS:   BEGIN
                         WRITELN(OUTPUT,'ARRAY':10,' ':4,ORD(AELTYPE):6,' ':4,
                            ORD(INXTYPE):6);
                            FOLLOWSTP(AELTYPE); FOLLOWSTP(INXTYPE)
                          END;
              RECORDS:  BEGIN
                        WRITELN(OUTPUT,'RECORD':10,' ':4,ORD(FSTFLD):6,' ':4,
                            ORD(RECVAR):6); FOLLOWCTP(FSTFLD);
                            FOLLOWSTP(RECVAR)
                          END;
              FILES:    BEGIN WRITE(OUTPUT,'FILE':10,' ':4,ORD(FILTYPE):6);
                            FOLLOWSTP(FILTYPE)
                          END;
              TAGFLD:   BEGIN WRITELN(OUTPUT,'TAGFLD':10,' ':4,ORD(TAGFIELDP):6,
                            ' ':4,ORD(FSTVAR):6);
                            FOLLOWSTP(FSTVAR)
                          END;
              VARIANT:  BEGIN WRITELN(OUTPUT,'VARIANT':10,' ':4,ORD(NXTVAR):6,
                            ' ':4,ORD(SUBVAR):6,VARVAL.IVAL);
                            FOLLOWSTP(NXTVAR); FOLLOWSTP(SUBVAR)
                          END
              END (*CASE*)
            END (*IF MARKED*)
    END (*FOLLOWSTP*);

    PROCEDURE FOLLOWCTP;
      VAR I: INTEGER;
    BEGIN
      IF FP <> NIL THEN
        WITH FP@ DO
          BEGIN WRITE(OUTPUT,' ':4,ORD(FP):6,' ',NAME:9,' ':4,ORD(LLINK):6,
            ' ':4,ORD(RLINK):6,' ':4,ORD(IDTYPE):6);
            CASE KLASS OF
              TYPES: WRITE(OUTPUT,'TYPE':10);
              KONST: BEGIN WRITE(OUTPUT,'CONSTANT    ':10,' ':4,ORD(NEXT):6);
                     IF IDTYPE <> NIL THEN
                         IF IDTYPE = REALPTR THEN
                           BEGIN
                             IF VALUES.VALP <> NIL THEN
                               WRITE(OUTPUT,' ',VALUES.VALP@.RVAL:9)
                           END
                         ELSE
                           IF IDTYPE@.FORM = ARRAYS THEN  (*STRINGCONST*)
                             BEGIN
                               IF VALUES.VALP <> NIL THEN
                                 BEGIN WRITE(OUTPUT,' ');
                                   WITH VALUES.VALP@ DO
                                     FOR I := 1 TO SLNGTH DO
                                      WRITE(OUTPUT,SVAL[I])
                                 END
                             END
                           ELSE WRITE(OUTPUT,VALUES.IVAL)
                       END;
              VARS:  BEGIN WRITE(OUTPUT,'VARIABLE    ':10);
                        IF VKIND = ACTUAL THEN WRITE(OUTPUT,'ACTUAL':10)
                        ELSE WRITE(OUTPUT,'FORMAL':10);
                        WRITE(OUTPUT,' ':4,ORD(NEXT):6,VLEV,' ':4,VADDR:6 );
                      END;
              FIELD: WRITE(OUTPUT,'FIELD':10,' ':4,ORD(NEXT):6,' ':4,FLDADDR:6);
              PROC,
              FUNC:  BEGIN
                        IF KLASS = PROC THEN WRITE(OUTPUT,'PROCEDURE':10)
                        ELSE WRITE(OUTPUT,'FUNCTION    ':10);
                        IF PFDECKIND = STANDARD THEN
                         WRITE(OUTPUT,'STANDARD    ':10,
                          KEY:10)
                        ELSE
                          BEGIN WRITE(OUTPUT,'DECLARED    ':10, ORD(NEXT):8);
                            WRITE(OUTPUT,PFLEV,' ':4,PFNAME:6);
                            IF PFKIND = ACTUAL THEN
                              BEGIN WRITE(OUTPUT,'ACTUAL':10);
                                IF FWDECL THEN WRITE(OUTPUT,'FORWARD':10)
                                ELSE WRITE(OUTPUT,'NOTFORWARD':10);
                                IF EXTRN THEN WRITE(OUTPUT,'EXTRN':10)
                                ELSE WRITE(OUTPUT,'NOT EXTRN':10);
                              END
                            ELSE WRITE(OUTPUT,'FORMAL':10)
                          END
                     END
            END (*CASE*);
            WRITELN(OUTPUT); FOLLOWCTP(LLINK); FOLLOWCTP(RLINK);
            FOLLOWSTP(IDTYPE)
          END (*WITH*)
    END (*FOLLOWCTP*);

  BEGIN (*PRINTTABLES*)
    WRITELN(OUTPUT); WRITELN(OUTPUT); WRITELN(OUTPUT);
    IF FB THEN LIM := 0
    ELSE BEGIN LIM := TOP; WRITE(OUTPUT,' LOCAL') END;
    WRITELN(OUTPUT,' TABLES     '); WRITELN(OUTPUT);
    MARKER;
    FOR I := TOP DOWNTO LIM DO
      FOLLOWCTP(DISPLAY[I].FNAME);
      WRITELN(OUTPUT);
      IF NOT EOL THEN WRITE(OUTPUT,' ':CHCNT+16)
  END (*PRINTTABLES*); "


  PROCEDURE GENLABEL(VAR NXTLAB: INTEGER);
  BEGIN INTLABEL := INTLABEL + 1;
    NXTLAB := INTLABEL
  END (*GENLABEL*);


"E"(*THE FOLLOWING OUTPUTS ASYMBOL TABLE FILE FOR USE BY 'SNAPSHOT' PROGRAM*)
"E"
"E"PROCEDURE PRNTSYMBL(LCP:CTP);
"E"
"E"   VAR  LINELN:INTEGER;  (* CURRENT SYMBOL TABLE FILE LINE LENGTH *)
"E"
"E"   PROCEDURE PRNTVAR(VRP:CTP; VAR LINELN:INTEGER); FORWARD;
"E"
"E"   PROCEDURE PRNTTYPE(TYPP:STP; VAR LINELN:INTEGER);
"E"
"E"      VAR  VP: CTP;   MIN, MAX: INTEGER;
"E"
"E"      BEGIN
"E"      IF (LINELN+3) >= 80 THEN BEGIN WRITELN(QRR);
"E"            WRITE(QRR,' ');  LINELN := 0; END
"E"         ELSE  LINELN := LINELN+3;
"E"      IF TYPP=INTPTR THEN WRITE(QRR,'I; ')
"E"         ELSE IF TYPP=REALPTR THEN WRITE(QRR,'R; ')
"E"         ELSE IF TYPP=BOOLPTR THEN WRITE(QRR,'B; ')
"E"         ELSE IF TYPP=CHARPTR THEN WRITE(QRR,'C; ')
"E"         ELSE IF TYPP <> NIL THEN
"E"           CASE TYPP@.FORM OF
"E"
"E"           SUBRANGE,
"E"           SCALAR:  WRITE(QRR,'L; ');
"E"
"E"           POINTER: WRITE(QRR,'P; ');
"E"
"E"           POWER:   IF TYPP@.ELSET <> NIL THEN
"E"                      BEGIN     WRITE(QRR,'S ');
"E"                      IF (LINELN + 10) >= 80
"E"                         THEN BEGIN LINELN := 0;  WRITELN(QRR); END
"E"                         ELSE  LINELN := LINELN + 10;
"E"                      GETBOUNDS(TYPP@.ELSET, MIN, MAX) ;
"E"                      WRITE(QRR, MIN:3,' ', MAX:3, ' ; ');
"E"                      END;
"E"
"E"           FILES:   WRITE(QRR,'F; ');
"E"
"E"           RECORDS: BEGIN   WRITE(QRR,'D ');
"E"                    VP := TYPP@.FSTFLD;
"E"                    WHILE VP <> NIL DO BEGIN PRNTVAR(VP, LINELN);
"E"                       VP := VP@.NEXT;  END;
"E"                    IF (LINELN+2) >= 80 THEN BEGIN WRITELN(QRR);
"E"                         WRITE(QRR,' '); LINELN := 0; END
"E"                      ELSE LINELN := LINELN+2;
"E"                    WRITE(QRR,'; ');
"E"                    END;
"E"
"E"           ARRAYS:  IF TYPP@.INXTYPE <> NIL THEN
"E"                      BEGIN    WRITE(QRR,'A ');
"E"                      IF (LINELN+26) >= 80 THEN BEGIN  WRITELN(QRR);
"E"                           WRITE(QRR,' ');  LINELN := 0 END
"E"                        ELSE LINELN := LINELN+26;
"E"                      GETBOUNDS(TYPP@.INXTYPE, MIN, MAX) ;
"E"                      WRITE(QRR, MIN ,' ', MAX,' ');
"E"                      PRNTTYPE(TYPP@.AELTYPE, LINELN);
"E"                      END;
"E"           END (*CASE FORM OF...*) ;
"E"
"E"      END;    (* PRNTTYPE *)
"E"
"E"
"E"   PROCEDURE PRNTVAR;
"E"
"E"      BEGIN
"E"      IF (LINELN+IDLNGTH+1) >= 80 THEN BEGIN  WRITELN(QRR);
"E"            WRITE(QRR,' ');  LINELN := 0; END
"E"         ELSE LINELN := LINELN+IDLNGTH+1;
"E"
"E"      WRITE(QRR,VRP@.NAME,' ');
"E"      PRNTTYPE(VRP@.IDTYPE, LINELN);
"E"      END;
"E"
"E"   BEGIN   (* PRNTSYMBL *)
"E"   IF PRCODE THEN
"E"      BEGIN
"E"      CASE LCP@.KLASS OF
"E"        VARS:  BEGIN
"E"               IF LCP@.VKIND = FORMAL THEN WRITE(QRR,'@');
"E"               WRITE(QRR,LCP@.VADDR,' ',LCP@.NAME,' ');
"E"               LINELN := IDLNGTH+12;
"E"               PRNTTYPE(LCP@.IDTYPE, LINELN);
"E"               END;
"E"
"E"        PROC,FUNC:  BEGIN
"E"               WRITELN(QRR,'% ',LCP@.NAME,' ',LCP@.PFNAME);
"E"               WHILE LCP@.NEXT <> NIL DO  BEGIN
"E"                 PRNTSYMBL(LCP@.NEXT);  LCP := LCP@.NEXT;  END;
"E"               END;
"E"        TYPES,KONST,FIELD: ;
"E"        END;
"E"      WRITELN(QRR);
"E"      END (*IF PRCODE*) ;
"E"   END;  (* PRNTSYMBL *)


  PROCEDURE BLOCK(FSYS: SETOFSYS; FSY: SYMBOL; FPROCP: CTP);
#   VAR LSY: SYMBOL; TEST: BOOLEAN; SEGSIZE: INTEGER ;


    PROCEDURE SKIP(FSYS: SETOFSYS);
      (*SKIP INPUT STRING UNTIL RELEVANT SYMBOL FOUND*)
    BEGIN
    WHILE NOT(SY IN FSYS) DO
      BEGIN
      INSYMBOL
      END ;
    END (*SKIP*) ;


#    PROCEDURE ALIGN(VAR Q:ADDRRANGE;  P: ADDRRANGE) ;
#      VAR I : INTEGER ;
#
#      BEGIN
#      IF P >= REALSIZE THEN  P := REALSIZE
#      ELSE  IF P >= INTSIZE THEN  P := INTSIZE
#            ELSE IF P <= 0 THEN  IF ERRORCNT = 0 THEN  ERROR(401) ;
#      IF P >= INTSIZE THEN
#      BEGIN  I:= Q MOD P ; IF I > 0 THEN Q := Q+(P-I) END ;
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
                       SIZE := LNGTH*CHARSIZE; FORM := ARRAYS
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
                              IF FVALU.VALP@.RVAL[1] = '-' THEN
                                LVP@.RVAL[1] := '+'
                              ELSE LVP@.RVAL[1] := '-';
                              FOR I := 2 TO REALLNGTH DO
                                LVP@.RVAL[I] := FVALU.VALP@.RVAL[I];
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
                BEGIN NXT1 := FSP1@.FSTFLD; NXT2 := FSP2@.FSTFLD; COMP:=TRUE;
                  WHILE (NXT1 <> NIL) AND (NXT2 <> NIL) DO
                    BEGIN COMP:=COMP AND COMPTYPES(NXT1@.IDTYPE,NXT2@.IDTYPE);
                      NXT1 := NXT1@.NEXT; NXT2 := NXT2@.NEXT
                    END;
                  COMPTYPES := COMP AND (NXT1 = NIL) AND (NXT2 = NIL)
                              AND(FSP1@.RECVAR = NIL)AND(FSP2@.RECVAR = NIL)
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


    PROCEDURE TYP(FSYS: SETOFSYS; VAR FSP: STP; VAR FSIZE: ADDRRANGE);
      VAR LSP,LSP1,LSP2: STP; OLDTOP: DISPRANGE; LCP: CTP;
          LSIZE,DISPL: ADDRRANGE; LMIN,LMAX: INTEGER;  ALNFCT : 1..8 ;

      PROCEDURE SIMPLETYPE(FSYS:SETOFSYS; VAR FSP:STP"; VAR FSIZE:ADDRRANGE");
        VAR LSP,LSP1: STP; LCP,LCP1: CTP; TTOP: DISPRANGE;
            LCNT: INTEGER; LVALU: VALU;
      BEGIN FSIZE := 1;
        IF NOT (SY IN SIMPTYPEBEGSYS) THEN
          BEGIN ERROR(1); SKIP(FSYS + SIMPTYPEBEGSYS) END;
        IF SY IN SIMPTYPEBEGSYS THEN
          BEGIN
            IF SY = LPARENT THEN
              BEGIN TTOP := TOP;   (*DECL. CONSTS LOCAL TO INNERMOST BLOCK*)
                WHILE DISPLAY[TOP].OCCUR <> BLCK DO TOP := TOP - 1;
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
#               IF PACKDATA THEN
#                 IF LCNT < 256 THEN  LSP@.SIZE := CHARSIZE ;
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
#                       IF SY = DOTDOT THEN INSYMBOL ELSE ERROR(5);
                        CONSTANT(FSYS,LSP1,LVALU);
                        LSP@.MAX := LVALU;
#                       IF PACKDATA THEN
#                         IF LVALU.IVAL < 256 THEN
#                           IF LSP@.MIN.IVAL >= 0 THEN  LSP@.SIZE := CHARSIZE ;
#                       LSP@.ALN := LSP@.SIZE ;
                        IF LSP@.RANGETYPE <> LSP1 THEN ERROR(107)
                      END
                    ELSE
                      BEGIN LSP := LCP@.IDTYPE;
#                     " IF LSP <> NIL THEN FSIZE := LSP@.SIZE  "
                      END
                  END (*SY = IDENT*)
                ELSE
                  BEGIN NEW(LSP,SUBRANGE); LSP@.FORM := SUBRANGE;
                    CONSTANT(FSYS + [DOTDOT],LSP1,LVALU);
                    IF STRING(LSP1) THEN
                      BEGIN ERROR(148); LSP1 := NIL END;
                    WITH LSP@ DO
#                     BEGIN RANGETYPE:=LSP1; MIN:=LVALU; SIZE:=INTSIZE;
#                     IF LSP1 <> NIL THEN SIZE := LSP1@.SIZE ;
#                     END;
#                   IF SY = DOTDOT THEN INSYMBOL ELSE ERROR(5);
                    CONSTANT(FSYS,LSP1,LVALU);
                    LSP@.MAX := LVALU;
#                   IF PACKDATA THEN
#                     IF LVALU.IVAL < 256 THEN
#                       IF LSP@.MIN.IVAL >= 0 THEN  LSP@.SIZE := CHARSIZE ;
#                   LSP@.ALN := LSP@.SIZE ;
                    IF LSP@.RANGETYPE <> LSP1 THEN ERROR(107)
                  END;
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


#     PROCEDURE FIELDLIST(FSYS: SETOFSYS; VAR FRECVAR: STP;VAR RECALN: ALNRNG);
        VAR LCP,LCP1,NXT,NXT1: CTP; LSP,LSP1,LSP2,LSP3,LSP4: STP;
#           MINSIZE,MAXSIZE,LSIZE: ADDRRANGE; LVALU: VALU; LALNFCT : ALNRNG ;
#     BEGIN  NXT := NIL;  LSP := NIL;  RECALN := 1;
        IF NOT (SY IN FSYS+[IDENT,CASESY]) THEN
          BEGIN ERROR(19); SKIP(FSYS + [IDENT,CASESY]) END;

        WHILE SY = IDENT DO
#         BEGIN  NXT1 := NIL;
            REPEAT
              IF SY = IDENT THEN
                BEGIN NEW(LCP,FIELD);
                  WITH LCP@ DO
#                   BEGIN NAME := ID; IDTYPE := NIL; NEXT := NIL;
                      KLASS := FIELD;
                    END;
#                 IF NXT1 = NIL THEN  NXT1 := LCP;
#                 IF NXT <> NIL THEN  NXT@.NEXT := LCP;
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
#      "NXT := NIL;
#       WHILE NXT1 <> NIL DO
#         WITH NXT1@ DO
#           BEGIN LCP := NEXT; NEXT := NXT; NXT := NXT1; NXT1 := LCP END;"
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
                  END;
"TF"           PRTERR := FALSE ;  SEARCHID([TYPES],LCP1) ;  PRTERR := TRUE ;
"TF"           IF LCP1 = NIL THEN  BEGIN  (*EXPLICIT TAG FIELD *)
#               ENTERID(LCP);  INSYMBOL ;
#               IF SY = COLON THEN INSYMBOL ELSE ERROR(5);
#               IF SY <> IDENT THEN
#                 BEGIN ERROR(2); SKIP(FSYS + [OFSY,LPARENT]) END
"TF"        ;  END (* IF LCP1 = NIL *)
#               ELSE (* NO EXPLICT TAG FIELD  *)
"TF"             LCP@.NAME := BLANKID ;
                  BEGIN SEARCHID([TYPES],LCP1);
                    LSP1 := LCP1@.IDTYPE;
                    IF LSP1 <> NIL THEN
                      WITH LSP1@ DO
#                       BEGIN
"TF"                   IF LCP@.NAME <> BLANKID THEN  BEGIN
#                       ALIGN(DISPL,ALN) ;
#                       IF ALN > RECALN THEN RECALN := ALN ;
#                       LCP@.FLDADDR := DISPL ;  DISPL := DISPL + SIZE;
"TF"                   END (* LCP@.NAME <> BLANKID *) ;
                        IF (FORM <= SUBRANGE) OR STRING(LSP1) THEN
                          BEGIN IF COMPTYPES(REALPTR,LSP1) THEN ERROR(109)
                            ELSE IF STRING(LSP1) THEN ERROR(398);
                            LCP@.IDTYPE := LSP1; LSP@.TAGFIELDP := LCP;
                          END
                        ELSE ERROR(110);
                        END (* WITH LSP1@ DO *) ;
                    INSYMBOL;
                  END
              END
            ELSE BEGIN ERROR(2); SKIP(FSYS + [OFSY,LPARENT]) END;
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
                    FORM := VARIANT
                  END;
                LSP1 := LSP3; LSP2 := LSP3;
                TEST := SY <> COMMA;
                IF NOT TEST THEN INSYMBOL
              UNTIL TEST;
              IF SY = COLON THEN INSYMBOL ELSE ERROR(5);
              IF SY = LPARENT THEN INSYMBOL ELSE ERROR(9);
              FIELDLIST(FSYS + [RPARENT,SEMICOLON],LSP2,LALNFCT);
#             IF LALNFCT > RECALN THEN  RECALN := LALNFCT ;
              IF DISPL > MAXSIZE THEN MAXSIZE := DISPL;
              WHILE LSP3 <> NIL DO
                BEGIN LSP4 := LSP3@.SUBVAR; LSP3@.SUBVAR := LSP2;
                  LSP3@.SIZE := DISPL;
                  LSP3 := LSP4
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
      IF NOT (SY IN TYPEBEGSYS) THEN
         BEGIN ERROR(10); SKIP(FSYS + TYPEBEGSYS) END;
      IF SY IN TYPEBEGSYS THEN
        BEGIN
          IF SY IN SIMPTYPEBEGSYS THEN SIMPLETYPE(FSYS,FSP",FSIZE")
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
                IF SY = PACKEDSY THEN
                  BEGIN INSYMBOL;
                    IF NOT (SY IN TYPEDELS) THEN
                      BEGIN
                        ERROR(10); SKIP(FSYS + TYPEDELS)
                      END
                  END;
    (*ARRAY*)     IF SY = ARRAYSY THEN
                  BEGIN INSYMBOL;
                    IF SY = LBRACK THEN INSYMBOL ELSE ERROR(11);
                    LSP1 := NIL;
                    REPEAT NEW(LSP,ARRAYS);
                      WITH LSP@ DO
                        BEGIN AELTYPE := LSP1; INXTYPE := NIL; FORM:=ARRAYS END;
                      LSP1 := LSP;
                      SIMPLETYPE(FSYS + [COMMA,RBRACK,OFSY],LSP2",LSIZE");
#                   " LSP1@.SIZE := LSIZE ;  NOT USED "
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
                    IF SY = RBRACK THEN INSYMBOL ELSE ERROR(12);
                    IF SY = OFSY THEN INSYMBOL ELSE ERROR(8);
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
                        END;
                      LSP := LSP1; LSP1 := LSP2
                    UNTIL LSP1 = NIL
                  END
                ELSE
    (*RECORD*)      IF SY = RECORDSY THEN
                    BEGIN INSYMBOL;
                      OLDTOP := TOP;
                      IF TOP < DISPLIMIT THEN
                        BEGIN TOP := TOP + 1;
                          WITH DISPLAY[TOP] DO
                            BEGIN FNAME := NIL;
                              FLABEL := NIL;
                                  OCCUR := REC
                            END
                        END
                      ELSE ERROR(250);
                      DISPL := 0;
                      FIELDLIST(FSYS-[SEMICOLON]+[ENDSY],LSP1,ALNFCT);
                      NEW(LSP,RECORDS);
                      WITH LSP@ DO
                        BEGIN FSTFLD := DISPLAY[TOP].FNAME;
                          RECVAR := LSP1; SIZE := DISPL;
                          FORM := RECORDS ;  ALN := ALNFCT ;
                        END;
                      TOP := OLDTOP;
                      IF SY = ENDSY THEN INSYMBOL ELSE ERROR(13)
                    END
                  ELSE
    (*SET*)           IF SY = SETSY THEN
                      BEGIN INSYMBOL;
                        IF SY = OFSY THEN INSYMBOL ELSE ERROR(8);
                        SIMPLETYPE(FSYS,LSP1",LSIZE");
                        IF LSP1 <> NIL THEN
                          IF (LSP1 = CHARPTR) OR (LSP1 = INTPTR) THEN ERROR(304)
                          ELSE IF (LSP1@.FORM > SUBRANGE) THEN
                            BEGIN ERROR(115); LSP1 := NIL END
                          ELSE
                            IF LSP1 = REALPTR THEN ERROR(114)
#                           ELSE IF LSP1@.FORM = SUBRANGE THEN
#                                  IF LSP1@.MAX.IVAL > SETMAX   THEN ERROR(304);
                        NEW(LSP,POWER);
                        WITH LSP@ DO
                          BEGIN ELSET:=LSP1;
                          SIZE:=SETSIZE; ALN := INTSIZE ; FORM:=POWER
                          END;
                      END
                    ELSE
    (*FILE*)            IF SY = FILESY THEN
#                      "BEGIN ERROR(398); INSYMBOL; SKIP(FSYS); LSP:= NIL END;"
#                       BEGIN  INSYMBOL ;
#                       IF SY = OFSY THEN INSYMBOL  ELSE  ERROR(8) ;
#                       SIMPLETYPE(FSYS,LSP1",LSIZE") ;
#                       IF LSP1 = NIL THEN  ERROR(398)
#                       ELSE  IF LSP1 <> CHARPTR THEN ERROR(398) ;
#                       LSP := TEXTPTR ;
#                       END ;
                FSP := LSP
              END;
          IF NOT (SY IN FSYS) THEN
            BEGIN ERROR(6); SKIP(FSYS) END
        END
      ELSE FSP := NIL;
      IF FSP = NIL THEN FSIZE := 1 ELSE FSIZE := FSP@.SIZE
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

    PROCEDURE CONSTDECLARATION;
      VAR LCP: CTP; LSP: STP; LVALU: VALU;
    BEGIN
      IF SY <> IDENT THEN
        BEGIN ERROR(2); SKIP(FSYS + [IDENT]) END;
      WHILE SY = IDENT DO
        BEGIN NEW(LCP,KONST);
          WITH LCP@ DO
            BEGIN NAME := ID; IDTYPE := NIL; NEXT := NIL; KLASS:=KONST END;
          INSYMBOL;
          IF (SY = RELOP) AND (OP = EQOP) THEN INSYMBOL ELSE ERROR(16);
          CONSTANT(FSYS + [SEMICOLON],LSP,LVALU);
          ENTERID(LCP);
          LCP@.IDTYPE := LSP; LCP@.VALUES := LVALU;
          IF SY = SEMICOLON THEN
            BEGIN INSYMBOL;
              IF NOT (SY IN FSYS + [IDENT]) THEN
                BEGIN ERROR(6); SKIP(FSYS + [IDENT]) END
            END
          ELSE ERROR(14)
        END
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
        BEGIN ERROR(117); WRITELN(OUTPUT);
          REPEAT WRITELN(OUTPUT,' TYPE-ID ',FWPTR@.NAME);
            FWPTR := FWPTR@.NEXT
          UNTIL FWPTR = NIL;
       """IF NOT EOL THEN WRITE(OUTPUT,' ': CHCNT+16) """
        END
    END (*TYPEDECLARATION*) ;

    PROCEDURE VARDECLARATION;
#     VAR  LCP, NXT, NXT1: CTP;  LSP: STP;  LSIZE: ADDRRANGE;
           LFPTR : FRECPTR ;
    BEGIN NXT := NIL;
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
#             IF  LSP = TEXTPTR THEN  (* TEXT FILE DECLARATION *)
#               BEGIN  "EBCD := EBCDFLG ;   EBCDFLG := FALSE ; "
#               IF LEVEL > 1 THEN  ERROR(398)
#               ELSE BEGIN
#                    NEW(LFPTR);  LFPTR@.FILIDPTR := NXT1;
#                    LFPTR@.NEXTFILE := FILEHEAD;  FILEHEAD := LFPTR;
#                    END ;
#               IF NXTFILBUF >= LASTFILBUF THEN  ERROR(258 (*TOO MANY FILES*)) ;
#               VADDR := NXTFILBUF; "VLEV := 1;"
#               NXTFILBUF := NXTFILBUF+1;
#               END
#             ELSE  (* OTHER VARIABLE DECLARATION *)
#               BEGIN
#               IF LSP <> NIL THEN  ALIGN(LC,LSP@.ALN) ;
#               VADDR := LC ;  LC := LC+LSIZE
#               END ;
"E"           IF DEBUG THEN  PRNTSYMBL(NXT1);
#             NXT1 := NEXT;
#           END;

        IF SY = SEMICOLON THEN
          BEGIN INSYMBOL;
            IF NOT (SY IN FSYS + [IDENT]) THEN
              BEGIN ERROR(6); SKIP(FSYS + [IDENT]) END
          END
        ELSE ERROR(14)
      UNTIL (SY <> IDENT) AND NOT (SY IN TYPEDELS);
      IF FWPTR <> NIL THEN
        BEGIN ERROR(117); WRITELN(OUTPUT);
          REPEAT WRITELN(OUTPUT,' TYPE-ID ',FWPTR@.NAME);
            FWPTR := FWPTR@.NEXT
          UNTIL FWPTR = NIL;
      """ IF NOT EOL THEN WRITE(OUTPUT,' ': CHCNT+16) """
        END ;
    END (*VARDECLARATION*) ;

    PROCEDURE PROCDECLARATION(FSY: SYMBOL);
      VAR OLDLEV: 0..MAXLEVEL; LSY: SYMBOL; LCP,LCP1: CTP; LSP: STP;
#         FORW: BOOLEAN; OLDTOP: DISPRANGE; PARCNT: INTEGER;
          LLC,LCM: ADDRRANGE; LBNAME, OLDLABEL: INTEGER; MARKP: @INTEGER;

      PROCEDURE PARAMETERLIST(FSY: SETOFSYS; VAR FPAR: CTP);
        VAR LCP,LCP1,LCP2,LCP3: CTP; LSP: STP; LKIND: IDKIND;
          LLC,LEN : ADDRRANGE; COUNT : INTEGER;
      BEGIN   LCP1 := NIL ;
"S1" "FPRM1 := LC ; RPRM1 := 0 ;  REGS_FULL := FALSE ;                         "
        IF NOT (SY IN FSY + [LPARENT]) THEN
          BEGIN ERROR(7); SKIP(FSYS + FSY + [LPARENT]) END;
        IF SY = LPARENT THEN
          BEGIN IF FORW THEN ERROR(119);
            INSYMBOL;
            IF NOT (SY IN [IDENT,VARSY,PROCSY,FUNCSY]) THEN
              BEGIN ERROR(7); SKIP(FSYS + [IDENT,RPARENT]) END;
            WHILE SY IN [IDENT,VARSY,PROCSY,FUNCSY] DO
              BEGIN
                IF SY = PROCSY THEN
                  BEGIN ERROR(398);
                    REPEAT INSYMBOL;
                      IF SY = IDENT THEN
                      BEGIN NEW(LCP,PROC,DECLARED,FORMAL);
                          WITH LCP@ DO
                            BEGIN NAME := ID; IDTYPE := NIL; NEXT := LCP1;
                              PFLEV := LEVEL (*BEWARE OF PARAMETER PROCEDURES*);
                              KLASS:=PROC;PFDECKIND:=DECLARED;PFKIND:=FORMAL
                            END;
                          ENTERID(LCP);
                          LCP1 := LCP; LC := LC + PTRSIZE;
                          INSYMBOL
                        END
                      ELSE ERROR(2);
                      IF NOT (SY IN FSYS + [COMMA,SEMICOLON,RPARENT]) THEN
                        BEGIN ERROR(7);SKIP(FSYS+[COMMA,SEMICOLON,RPARENT])END
                    UNTIL SY <> COMMA
                  END
                ELSE
                  BEGIN
                    IF SY = FUNCSY THEN
                      BEGIN ERROR(398); LCP2 := NIL;
                        REPEAT INSYMBOL;
                          IF SY = IDENT THEN
                            BEGIN NEW(LCP,FUNC,DECLARED,FORMAL);
                              WITH LCP@ DO
                                BEGIN NAME := ID; IDTYPE := NIL; NEXT := LCP2;
                                  PFLEV := LEVEL (*BEWARE PARAM FUNCS*);
                                  KLASS:=FUNC;PFDECKIND:=DECLARED;
                                  PFKIND:=FORMAL
                                END;
                              ENTERID(LCP);
                              LCP2 := LCP; LC := LC + PTRSIZE;
                              INSYMBOL;
                            END;
                          IF NOT (SY IN [COMMA,COLON] + FSYS) THEN
                           BEGIN ERROR(7);SKIP(FSYS+[COMMA,SEMICOLON,RPARENT])
                            END
                        UNTIL SY <> COMMA;
                        IF SY = COLON THEN
                          BEGIN INSYMBOL;
                            IF SY = IDENT THEN
                              BEGIN SEARCHID([TYPES],LCP);
                                LSP := LCP@.IDTYPE;
                                IF LSP <> NIL THEN
                                 IF NOT(LSP@.FORM IN[SCALAR,SUBRANGE,POINTER])
                                    THEN BEGIN ERROR(120); LSP := NIL END;
                                LCP3 := LCP2;
                                WHILE LCP2 <> NIL DO
                                  BEGIN LCP2@.IDTYPE := LSP; LCP := LCP2;
                                    LCP2 := LCP2@.NEXT
                                  END;
                                LCP@.NEXT := LCP1; LCP1 := LCP3;
                                INSYMBOL
                              END
                            ELSE ERROR(2);
                            IF NOT (SY IN FSYS + [SEMICOLON,RPARENT]) THEN
                              BEGIN ERROR(7);SKIP(FSYS+[SEMICOLON,RPARENT])END
                          END
                        ELSE ERROR(5)
                      END
                    ELSE
                      BEGIN
                        IF SY = VARSY THEN
                          BEGIN LKIND := FORMAL; INSYMBOL END
                        ELSE LKIND := ACTUAL;
                        LCP2 := NIL;
                        COUNT := 0;
                        REPEAT
                          IF SY = IDENT THEN
                            BEGIN NEW(LCP,VARS);
                              WITH LCP@ DO
                                BEGIN NAME:=ID; IDTYPE:=NIL; KLASS:=VARS;
                                  VKIND := LKIND; NEXT := LCP2; VLEV := LEVEL;
                                END;
                              ENTERID(LCP);
                              LCP2 := LCP; COUNT := COUNT+1;
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
                              BEGIN  SEARCHID([TYPES],LCP); LEN := PTRSIZE ;
#                               LSP := LCP@.IDTYPE;
                                IF LSP <> NIL THEN
#                                 BEGIN
#                                 IF (LKIND=ACTUAL) THEN
#                                   IF LSP@.FORM <= POWER THEN LEN := LSP@.SIZE
#                                   ELSE IF LSP@.FORM = FILES THEN ERROR(121)  ;
"CM"                              IF LSP@.FORM = POWER THEN  ALIGN(LC,INTSIZE)
"CM"                              ELSE ALIGN(LC, LEN) ;
#                                 END;
"S1" "                          ALIGN(LEN,MXDATASZE) ;  ALIGN(LC,MXDATASZE) ;  "
                                LC := LC+COUNT*LEN ; LCP3 := LCP2 ;  LLC := LC ;
                                WHILE LCP2 <> NIL DO
                                  BEGIN LCP := LCP2;
                                    WITH LCP2@ DO
                                      BEGIN IDTYPE := LSP; LLC := LLC-LEN;
                                        VADDR := LLC;
"S1" "                                  IF NOT REGS_FULL THEN                  "
"S1" "                                  IF RPRM1+LEN <= REGPRMAREA THEN        "
"S1" "                                    RPRM1 := RPRM1+LEN                   "
"S1" "                                  ELSE  REGS_FULL := TRUE ;              "
                                      END;
                                    LCP2 := LCP2@.NEXT
                                  END;
                                LCP@.NEXT := LCP1; LCP1 := LCP3;
                                INSYMBOL
                              END
                            ELSE ERROR(2);
                            IF NOT (SY IN FSYS + [SEMICOLON,RPARENT]) THEN
                              BEGIN ERROR(7);SKIP(FSYS+[SEMICOLON,RPARENT])END
                          END
                        ELSE ERROR(5);
                      END;
                  END;
                IF SY = SEMICOLON THEN
                  BEGIN INSYMBOL;
                    IF NOT (SY IN FSYS + [IDENT,VARSY,PROCSY,FUNCSY]) THEN
                      BEGIN ERROR(7); SKIP(FSYS + [IDENT,RPARENT]) END
                  END
              END (*WHILE*) ;
            IF SY = RPARENT THEN
              BEGIN INSYMBOL;
                IF NOT (SY IN FSY + FSYS) THEN
                  BEGIN ERROR(6); SKIP(FSY + FSYS) END
              END
            ELSE ERROR(4);
            LCP3 := NIL;
            (*REVERSE POINTERS AND RESERVE LOCAL CELLS FOR COPIES OF MULTIPLE
             VALUES*)
          " ALIGN(LC,MXDATASZE) ; " (*NORMALIZE STACK BEFORE ENTRING BLOCK*)
"S1" "      FPRM1 := LC-FPRM1 ;   SPRM1 := LC ;                                "
            WHILE LCP1 <> NIL DO
              WITH LCP1@ DO
                BEGIN LCP2 := NEXT; NEXT := LCP3;
                 "IF KLASS = VARS THEN           ???"
                    IF IDTYPE <> NIL THEN
#                     IF VKIND = ACTUAL THEN
#                         IF (IDTYPE@.FORM > POWER) THEN
                            BEGIN  ALIGN(LC,IDTYPE@.ALN (*OR IDTYPE@.SIZE*) ) ;
                            VADDR := LC; LC := LC + IDTYPE@.SIZE ;
                            END ;
                  LCP3 := LCP1; LCP1 := LCP2
                END;
"S1" "      ALIGN(LC, PTRSIZE) ;  SPRM1 := LC-SPRM1 ;                          "
            FPAR := LCP3
          END
            ELSE
"S1" "        BEGIN                                                            "
              FPAR := NIL ;
"S1" "        FPRM1 := 0 ;   SPRM1 := 0 ;   RPRM1 := 0 ;                       "
"S1" "        END ;                                                            "
    END (*PARAMETERLIST*) ;

    BEGIN (*PROCDECLARATION*)
      LLC := LC; LC := LCAFTMST;  (* ADR. OF THE FIRST VAR. IN THIS PROC. *)
#     LCP := UPRCPTR ;            (* TO INITIALIZE LCP IN CASE ! *)
      IF SY = IDENT THEN
        BEGIN SEARCHSECTION(DISPLAY[TOP].FNAME,LCP); (*DECIDE WHETHER FORW.*)
          IF LCP <> NIL THEN
          BEGIN
            IF LCP@.KLASS = PROC THEN
              FORW := LCP@.FWDECL AND(FSY = PROCSY)AND(LCP@.PFKIND = ACTUAL)
            ELSE
              IF LCP@.KLASS = FUNC THEN
                FORW:=LCP@.FWDECL AND(FSY=FUNCSY)AND(LCP@.PFKIND=ACTUAL)
              ELSE FORW := FALSE;
            IF NOT FORW THEN ERROR(160)
          END
          ELSE FORW := FALSE;
          IF NOT FORW THEN
            BEGIN
              IF FSY = PROCSY THEN NEW(LCP,PROC,DECLARED,ACTUAL)
              ELSE NEW(LCP,FUNC,DECLARED,ACTUAL);
              WITH LCP@ DO
#               BEGIN NAME := ID; IDTYPE := NIL;  SAVEFP := FALSE ;
#               " EXTRN := FALSE;" PFLEV := LEVEL; PROCLAB := PROCLAB+1 ;
#                 PFDECKIND := DECLARED; PFKIND := ACTUAL; PFNAME := PROCLAB ;
                  IF FSY = PROCSY THEN KLASS := PROC
                  ELSE KLASS := FUNC
                END;
              ENTERID(LCP)
            END
          ELSE
            BEGIN LCP1 := LCP@.NEXT;
              WHILE LCP1 <> NIL DO
                BEGIN
                  WITH LCP1@ DO
                    IF KLASS = VARS THEN
                      IF IDTYPE <> NIL THEN
                        BEGIN
#                       IF VKIND = FORMAL THEN LCM := LCM+PTRSIZE
                        ELSE LCM := VADDR + IDTYPE@.SIZE;
                        IF LCM > LC THEN LC := LCM
                        END;
                  LCP1 := LCP1@.NEXT
                END
              END;
          INSYMBOL
        END
      ELSE ERROR(2);
#     OLDLEV := LEVEL; OLDTOP := TOP;  OLDLABEL := INTLABEL ;  INTLABEL := 0 ;
      IF LEVEL < MAXLEVEL THEN LEVEL := LEVEL + 1 ELSE ERROR(251);
      IF TOP < DISPLIMIT THEN
        BEGIN TOP := TOP + 1;
          WITH DISPLAY[TOP] DO
            BEGIN
              IF FORW THEN FNAME := LCP@.NEXT
              ELSE FNAME := NIL;
              FLABEL := NIL;
              OCCUR := BLCK
            END
        END
      ELSE ERROR(250);
      IF FSY = PROCSY THEN
        BEGIN PARAMETERLIST([SEMICOLON],LCP1);
          IF NOT FORW THEN LCP@.NEXT := LCP1
        END
      ELSE
        BEGIN PARAMETERLIST([SEMICOLON,COLON],LCP1);
          IF NOT FORW THEN LCP@.NEXT := LCP1;
          IF SY = COLON THEN
            BEGIN INSYMBOL;
              IF SY = IDENT THEN
                BEGIN IF FORW THEN ERROR(122);
                  SEARCHID([TYPES],LCP1);
                  LSP := LCP1@.IDTYPE;
                  LCP@.IDTYPE := LSP;
                  IF LSP <> NIL THEN
#                   BEGIN
#                   IF NOT (LSP@.FORM IN [SCALAR,SUBRANGE,POINTER,POWER]) THEN
#                     BEGIN  ERROR(120);  LCP@.IDTYPE := NIL END;
#                   IF LSP = REALPTR THEN
#                     IF SAVEFPRS THEN
#                       BEGIN  LCP1 := LCP@.NEXT ;
#                       WHILE LCP1 <> NIL DO
#                         BEGIN
#                         LCP1@.VADDR := LCP1@.VADDR+FPSAVEAREA ;
#                         LCP1 := LCP1@.NEXT ;
#                         END ;
#                       LCP@.SAVEFP := TRUE ;    (* SET SAVE FPRS FLAG *)
#                       LC := LC+FPSAVEAREA ;    (* ADJUST LOC. CNTR *)
#                       END ;
#                   END (* WITH LSP@ DO *) ;
                  INSYMBOL
                END
              ELSE BEGIN ERROR(2); SKIP(FSYS + [SEMICOLON]) END
            END
          ELSE
            IF NOT FORW THEN ERROR(123)
        END;

#     WITH LCP@ DO
#       BEGIN FWDECL := FALSE ;  FRTRN := FALSE ; EXTRN := FALSE  END ;
"S1" "                                                                         "
"S1" "IF NOT FORW THEN                                                         "
"S1" "   WITH LCP@ DO                                                          "
"S1" "     BEGIN  FPRMSZE := FPRM1 ; RPRMSZE := RPRM1 ; SPRMSZE := SPRM1  END; "
"S1" "                                                                         "
      IF SY = SEMICOLON THEN INSYMBOL ELSE ERROR(14);
#     IF SY IN  [FORWARDSY,FRTRNSY,EXTRNSY]  THEN
#       BEGIN
#       IF SY = FORWARDSY THEN
#           IF FORW THEN ERROR(161)
#           ELSE LCP@.FWDECL := TRUE;
#         IF SY = FRTRNSY THEN  LCP@.FRTRN := TRUE ;
#         " BEGIN  LCP@.FRTRN := TRUE ;
#           LCP1 := LCP@.NEXT ;
#           WHILE LCP1 <> NIL DO
#             BEGIN  IF LCP1@.VKIND <> FORMAL THEN  ERROR(7) ;
#             LCP1 := LCP1@.NEXT ;
#             END ;
#           END (* SY = FRTRNSY *) ;  "
#         IF SY = EXTRNSY THEN LCP@.EXTRN := TRUE ;
          INSYMBOL;
          IF SY = SEMICOLON THEN INSYMBOL ELSE ERROR(14);
          IF NOT (SY IN FSYS) THEN  BEGIN ERROR(6); SKIP(FSYS) END
        END
      ELSE
        BEGIN " LCP@.FWDECL := FALSE; "
"E"       IF DEBUG THEN  PRNTSYMBL(LCP);
          MARK(MARKP); (* MARK HEAP FOR BLOCK ENTRY *)
          REPEAT BLOCK(FSYS,SEMICOLON,LCP);
            IF SY = SEMICOLON THEN
              BEGIN "IF PRTABLES THEN PRINTTABLES(FALSE);" INSYMBOL;
                IF NOT (SY IN [BEGINSY,PROCSY,FUNCSY]) THEN
                  BEGIN ERROR(6); SKIP(FSYS) END
              END
            ELSE ERROR(14)
          UNTIL SY IN [BEGINSY,PROCSY,FUNCSY];
          RELEASE(MARKP); (* RETURN LOCAL ENTRIES ON RUNTIME HEAP *)
        END;
      LEVEL := OLDLEV; TOP := OLDTOP; LC := LLC; INTLABEL := OLDLABEL ;
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
#             IF IDTYPE@.FORM = POWER THEN  PROCTYPE := ORD('S')
#             ELSE  IF IDTYPE = REALPTR THEN
#                     IF FRTRN THEN PROCTYPE := ORD('Z')
#                     ELSE PROCTYPE := ORD('R')
#               ELSE IF IDTYPE = BOOLPTR THEN
#                      IF FRTRN THEN PROCTYPE := ORD('X')
#                      ELSE PROCTYPE := ORD('B')
#                 ELSE IF IDTYPE@.FORM = POINTER THEN
#                   PROCTYPE := ORD('A')
#                   ELSE IF "(IDTYPE = CHARPTR) OR ((IDTYPE@.FORM = SUBRANGE)
#                           AND (IDTYPE@.RANGETYPE = CHARPTR)) "
#                           IDTYPE@.SIZE = 1 THEN PROCTYPE := ORD('C')
#                      ELSE  IF FRTRN THEN PROCTYPE := ORD('Y')
#                            ELSE PROCTYPE := ORD('I') ;
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
#        "MOD_TRACE,"                   (* TRACE VARS BEING MODIFIED *)
#         MODIFYING : BOOLEAN ;         (* A PROGRAM VAR BEING MODIFIED*)
#         VAR_REF, VAR_MOD : INTEGER ;  (* # OF VARIABLES ACCESSED/REFERENCED*)
#

          LLCP:CTP;
#         CNSTPTR:  CSP;
          (*ALLOWS REFERENCING OF NONINTEGER CONSTANTS BY AN INDEX
           (INSTEAD OF A POINTER), WHICH CAN BE STORED IN THE P2-FIELD
           OF THE INSTRUCTION RECORD UNTIL WRITEOUT.
#          --> PROCEDURE LOAD, PROCEDURE WRITEOUT*)  (*NOT NEEDED IN P_COMP.*)
          I, ENTNAME : INTEGER;
          LCMAX,LLC1: ADDRRANGE; LCP: CTP;
#         LLP: LBP;  PROCNAME : ALPHA ;
"CT"      FIRSTLN : INTEGER;  CTRNO : CTRRANGE;

      PROCEDURE PUTIC;
#     BEGIN
      (*IF (IC MOD 10 = 0) THEN WRITELN(PRR,' LOC ',IC:1);*)
      IF LINECNT > OLDLN THEN
        BEGIN  WRITELN(PRR, ' LOC ',LINECNT:1);  OLDLN := LINECNT  END ;
      END;


#    "FUNCTION FLDW(NUM : INTEGER) : INTEGER ;
#       VAR FW: 0..20 ;
#     BEGIN
#       FW := 0 ;  IF NUM < 0 THEN FW := 1 ;
#       NUM := ABS(NUM) ;
#       REPEAT
#         NUM := NUM DIV 10 ;  FW := FW+1 ;
#       UNTIL NUM = 0 ;
#       FLDW := FW
#     END (*FLDW*);"
#
#     FUNCTION GETTYPE(OPERAND: STP): INTEGER ;
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
#                       BEGIN
#                       IF OPERAND@.SIZE = CHARSIZE THEN GETTYPE := ORD('C')
#                       END
#       END (*GETTYPE*) ;
#
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
#                  BEGIN WRITE(PRR,' ''');
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
                 ELSE IF (FOP = 26) OR (FOP = 42)
"S1" "                   OR (FOP = 64)   (*PRM*)                               "
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
                WRITELN(PRR,CHR(FP1),',',FP2:1"FLDW(FP2)") ;
              45,50: (*CHK,LDA*)
                WRITELN(PRR,FP1:1"FLDW(FP1)",',',FP2:1"FLDW(FP2)");
              47,48,49,52,53,55: (*EQU..NEQ*)
                BEGIN WRITE(PRR,CHR(FP1));
                  IF FP1 = ORD('M') THEN WRITE(PRR,',',FP2:1"FLDW(FP2)");
                  WRITELN(PRR)
                END;
              51: (*LDC*)
                CASE FP1 OF
#                 0: WRITELN(PRR,'C,''',CHR(FP2):1,'''') ;
                  1: WRITELN(PRR,'I,',FP2:1"FLDW(FP2)");
                  2: BEGIN WRITE(PRR,'R,');
#                      WITH CNSTPTR@  DO
                         FOR K := 1 TO REALLNGTH DO
                           IF RVAL[K] <> ' ' THEN WRITE(PRR,RVAL[K]);
                       WRITELN(PRR)
                     END;
                  3: WRITELN(PRR,'B,',FP2:1);
                  4: WRITELN(PRR,'N');
                  5: BEGIN WRITE(PRR,'S,(');

#                      WITH CNSTPTR@ DO
#                        FOR I := 0 TO 3 DO
#                          BEGIN  J := 0 ;  K := SETMAX  -I*16 ;
#                          FOR K := K DOWNTO K-15 DO
#                            BEGIN  J := J*2 ;
#                            IF K IN PVAL THEN J := J+1 ;
#                            END ;
#                          IF I > 0 THEN  WRITE(PRR,',') ;
#                          WRITE(PRR, J: 1"FLDW(J)") ;
#                          END (* FOR I := 0 TO 3 *) ;
#                      WRITELN(PRR,')') ;
                     END
                END
            END;
          END;
          IC := IC + 1
      END (*GEN2*) ;

#     PROCEDURE GEN3(FOP: OPRANGE; FP0,FP1,FP2: INTEGER);
#     BEGIN
#       IF PRCODE THEN
#         BEGIN PUTIC; WRITE(PRR,MN[FOP]:4);
"S1" "          IF FOP = 41 THEN  (*MST*)                                      "
"S1" "             WRITE(PRR, FP0:2)                                           "
"S1" "          ELSE                                                           "
#                  WRITE(PRR, CHR(FP0):2) ;
#               WRITELN(PRR, ',', FP1:1"FLDW(FP1)", ',', FP2:1"FLDW(FP2)") ;
#         END;
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
                         DRCT: " IF VLEVEL <= 1 THEN
                                    GEN2(39(*LDO*),GETTYPE(BTYPE),DPLMT)
#                                ELSE " GEN3(54(*LOD*),GETTYPE(BTYPE),
                                         " LEVEL-" VLEVEL,DPLMT);
                         INDRCT: GEN2(35(*IND*),GETTYPE(BTYPE),IDPLMT);
                         INXD:   ERROR(400)
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
              INXD:   ERROR(400)
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
#                        CNSTPTR := CVAL.VALP ;  GEN1(37(*LCA*),0) ;
#                        END
                       ELSE ERROR(400);
                VARBL: CASE ACCESS OF
#                        DRCT:   GEN2(50(*LDA*),VLEVEL,DPLMT);
                         INDRCT: IF IDPLMT <> 0 THEN
                                    GEN2(23(*INC*),ORD('A'),IDPLMT);
                         INXD:   ERROR(400)
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
#         WRITELN(PRR,MN[33]:4,' L',FADDR:1"FLDW(FADDR)") END;
        IC := IC + 1
      END (*GENFJP*) ;

      PROCEDURE GENUJPFJP(FOP: OPRANGE; FP2: INTEGER);
      BEGIN
        IF PRCODE THEN
#         BEGIN PUTIC; WRITELN(PRR, MN[FOP]:4, ' L',FP2:1"FLDW(FP2)") END ;
        IC := IC + 1
      END (*GENUJPFJP*);


#     PROCEDURE GENCUPENT(FOP: OPRANGE;FP0,FP1,FP2: INTEGER;PROCNAME: ALPHA);
#       VAR TEMPNAME : ALPHA ;
#
#       PROCEDURE MKNAME(VAR ALB: ALPHA; NLB: INTEGER ) ;
#         VAR I, J: INTEGER ;
#
#       BEGIN
#         I := 1 ;   J := 8 ;
#         IF NOT NAMFLAG THEN J := 5 ;
#           REPEAT
#           IF ALB[I] = '_' THEN  ALB[I] := '$' ;  I := I+1  ;
#           UNTIL (I > J) OR (ALB[I] = ' ') ;
#         IF NOT NAMFLAG THEN
#           FOR J := 8 DOWNTO I DO
#             BEGIN
#             ALB[J] := CHR( ORD('0')+ NLB MOD 10 ) ;
#             NLB := NLB DIV 10 ;
#             END ;
#       END (*MKNAME*) ;
#
#     BEGIN (*GENCUPENT*)
#       IF PRCODE THEN
#         BEGIN  PUTIC ;  TEMPNAME := PROCNAME ;  (*TO PRESERVE FULL NAME*)
#           IF FOP = 46 THEN (*CUP*)
#             BEGIN    MKNAME(TEMPNAME,FP2) ;
#               WRITELN(PRR,MN[46],CHR(FP0):2,',',FP1:1"FLDW(FP1)",',',
#                           TEMPNAME:8);
#             END
#           ELSE  (*ENT*)
#             BEGIN
#             IF OLDIC = 0 THEN  WRITELN(PRR,' BGN ', ASSEMBLE:1, ',',
#                                         GET_STAT:1, ',', ASMVERB:1, ',',
#                                         PROGNAME, ' ', TIME:9, DATE) ;
#             IF FPROCP <> NIL THEN  MKNAME(TEMPNAME,FP2) ;
#             WRITELN(PRR, TEMPNAME:8, MN[32], CHR(FP0):2, ',',
#                          LEVEL:1"FLDW(LEVEL)",',L', FP1:1"FLDW(FP1)",
#                          PROCNAME:14, ',', SAVEREGS:1, ',', SAVEFPRS:1, ',',
                           DEBUG:1, ',', FP2:1) ;
#             END ;
#         END ;
#       IC := IC + 1
#     END (*GENCUPENT*);

      PROCEDURE GENDEF(L1, L2: ADDRRANGE ) ;
        BEGIN
        IF PRCODE THEN  WRITELN(PRR,'L', L1:1"FLDW(L1)", MN[63(*DEF*)], L2:10);
        END (*GENDEF*) ;


#     PROCEDURE CHKBNDS(FSP: STP);
#       VAR LMIN,LMAX: INTEGER;
#     BEGIN
#       IF FSP <> NIL THEN
#         IF FSP <> BOOLPTR THEN
#           IF FSP <> INTPTR THEN
#             IF FSP <> REALPTR THEN
#               IF FSP@.FORM <= POINTER THEN
#                 BEGIN
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
#                 END
#     END (*CHKBNDS*);

      PROCEDURE PUTLABEL(LABNAME: INTEGER);
      BEGIN IF PRCODE THEN WRITELN(PRR, 'L', LABNAME:1"FLDW(LABNAME)", ' LAB')
      END (*PUTLABEL*);
"CT"
"CT"
"CT"   FUNCTION CTRGEN : CTRRANGE;
"CT"
"CT"   BEGIN   (* CREATE A UNIQUE STATEMENT COUNTER AND EMIT P-CODE TO INCREME
"CT"            IT *)
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
"CT"
"CT"     BEGIN   (* WRITE AN ENTRY DESCRIBING A STATEMENT COUNTER. *)
"CT"     (* R. L. SITES  3 AUG 77 *)
"CT"     IF CTROPTION THEN
"CT"       BEGIN   (* IF FIRSTCTR THEN
"CT"                      BEGIN   WRITELN(CTRTBL , COMPDATE); WRITELN(
"CT"                      COMPTIME);
"CT"                      FIRSTCTR := FALSE END;
"CT"                  WRITELN(CTRLBL, (((ORD(CTRT)*CTRMAX+CTRNO)*MAXLN+FLN)
"CT"                           *MAXLN+MLN)*MAXLN+LLN:20);  *)
"CT"       WRITELN(QRR, '#CTR    ', ORD(CTRT):4, CTRNO:6, FLN:7, MLN:7, LLN:7 );
"CT"       END
"CT"   END; (* CTREMIT *)
"CT"
      PROCEDURE STATEMENT(FSYS: SETOFSYS);
        LABEL 1;
        VAR LCP: CTP; LLP: LBP; TTOP : DISPRANGE ;
"CT"        CTRNO : CTRRANGE;

        PROCEDURE EXPRESSION(FSYS: SETOFSYS); FORWARD;

        PROCEDURE SELECTOR(FSYS: SETOFSYS; FCP: CTP);
          VAR LATTR: ATTR; LCP: CTP; LMIN,LMAX: INTEGER;
(*+RM     INDEXING : BOOLEAN ;                                               +*)
        BEGIN
(*+RM     INDEXING := FALSE ;                                                +*)
          WITH FCP@, GATTR DO
            BEGIN TYPTR := IDTYPE; KIND := VARBL;
(*+RM                                                                        +*)
(*+RM       IF GET_STAT THEN                                                 +*)
(*+RM         BEGIN                                                          +*)
(*+RM         IF MODIFYING THEN  WRITE(QRR,' #MOD')                          +*)
(*+RM         ELSE WRITE(QRR,' #REF') ;                                      +*)
(*+RM         WRITE(QRR, CHR(GETTYPE("BTYPE" TYPTR)), ' ':2 );               +*)
(*+RM         END (*GET_STAT*) ;                                             +*)
(*+RM                                                                        +*)
              CASE KLASS OF
                VARS:
                  IF VKIND = ACTUAL THEN
                    BEGIN ACCESS := DRCT; VLEVEL := VLEV;
                      DPLMT := VADDR
                    END
                  ELSE
                    BEGIN
(*+RM               IF GET_STAT THEN  WRITE(QRR,' #IND',VLEV:3,VADDR:8);     +*)
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
(*+RM                   IF GET_STAT THEN  WRITE(QRR,' #IND',LEVEL:3,VDSPL:8);+*)
                        ACCESS := INDRCT; IDPLMT := FLDADDR
                      END;
                FUNC:
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
(*+RM           (*          IF MODIFYING THEN                                +*)
(*+RM                         WRITE(QRR,'  DIR',VLEVEL:3, DPLMT:7) ; *)      +*)
                          END
              END (*CASE*) ;
#             GATTR.BTYPE := GATTR.TYPTR ;
            END (*WITH*);
          IF NOT (SY IN SELECTSYS + FSYS) THEN
            BEGIN ERROR(59); SKIP(SELECTSYS + FSYS) END;
          WHILE SY IN SELECTSYS DO
            BEGIN
        (*[*)   IF SY = LBRACK THEN
                BEGIN
(*+RM                                                                        +*)
(*+RM            IF GET_STAT THEN                                            +*)
(*+RM              WITH GATTR DO                                             +*)
(*+RM                BEGIN                                                   +*)
(*+RM                IF ACCESS = DRCT THEN                                   +*)
(*+RM                  WRITE(QRR, ' #DIR',VLEVEL:3,DPLMT:8)                  +*)
(*+RM                ELSE IF (ACCESS = INDRCT) AND (IDPLMT <> 0) THEN        +*)
(*+RM                       WRITE(QRR,' #DPM   ', IDPLMT:8) ;                +*)
(*+RM                WRITE(QRR,' #INX   ') ;                                 +*)
(*+RM                IF MODIFYING THEN                                       +*)
(*+RM                   BEGIN  INDEXING := TRUE ;  MODIFYING := FALSE END ;  +*)
(*+RM                END ;                                                   +*)
(*+RM                                                                        +*)
                  REPEAT LATTR := GATTR;
                    WITH LATTR DO
                      IF TYPTR <> NIL THEN
                        IF TYPTR@.FORM <> ARRAYS THEN
                          BEGIN ERROR(138); TYPTR := NIL END;
                    LOADADDRESS;
                    INSYMBOL; EXPRESSION(FSYS + [COMMA,RBRACK]);
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
#                                   GEN2(22(*DEC*),GETTYPE(GATTR.BTYPE),LMIN)
#                                 ELSE IF LMIN < 0 THEN
                                     GEN2(23(*INC*),GETTYPE(GATTR.BTYPE),-LMIN)
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
                  IF SY = RBRACK THEN INSYMBOL ELSE ERROR(12) ;
(*+RM             IF INDEXING THEN                                           +*)
(*+RM                BEGIN  MODIFYING := TRUE ;  INDEXING := FALSE END ;     +*)
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
                              BEGIN SEARCHSECTION(TYPTR@.FSTFLD,LCP);
                                IF LCP = NIL THEN
                                  BEGIN ERROR(152); TYPTR := NIL END
                                ELSE
                                  WITH LCP@ DO
                                    BEGIN TYPTR := IDTYPE;
                                      CASE ACCESS OF
                                        DRCT:   DPLMT := DPLMT + FLDADDR;
                                        INDRCT: IDPLMT := IDPLMT + FLDADDR;
                                        INXD:   ERROR(400)
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
(*+RM                     IF GET_STAT THEN                                   +*)
(*+RM                       IF ACCESS = DRCT THEN                            +*)
(*+RM                         WRITE(QRR,' #PTR',VLEVEL:3,DPLMT:8)            +*)
(*+RM                       ELSE  (*ACCESS = INDRCT *)                       +*)
(*+RM                         WRITE(QRR,' #DPM   ',"LEVEL:3,"IDPLMT:8) ;     +*)
#                         LOAD ;
#                         IF DEBUG THEN  CHKBNDS(GATTR.TYPTR) ;
#                         TYPTR := ELTYPE ;
                            WITH GATTR DO
                              BEGIN KIND := VARBL; ACCESS := INDRCT;
                                IDPLMT := 0
                              END
                          END
                        ELSE
                          IF FORM = FILES THEN TYPTR := FILTYPE
                          ELSE ERROR(141);
                    INSYMBOL
                  END;
              IF NOT (SY IN FSYS + SELECTSYS) THEN
                BEGIN ERROR(6); SKIP(FSYS + SELECTSYS) END ;
#             GATTR.BTYPE := GATTR.TYPTR ;
            END (*WHILE*) ;
(*+RM                                                                        +*)
(*+RM     IF GET_STAT THEN                                                   +*)
(*+RM       WITH GATTR DO                                                    +*)
(*+RM         BEGIN                                                          +*)
(*+RM         IF ACCESS = DRCT THEN                                          +*)
(*+RM           WRITE(QRR,' #DIR', VLEVEL:3,DPLMT:8)                         +*)
(*+RM         ELSE IF (ACCESS = INDRCT) AND (IDPLMT <> 0) THEN               +*)
(*+RM           WRITE(QRR, ' #DPM   ',IDPLMT:8) ;                            +*)
(*+RM         IF MODIFYING THEN  WRITE(QRR, ' #MND   ')                      +*)
(*+RM         ELSE  WRITE(QRR,' #RND   ') ;                                  +*)
(*+RM         END ;                                                          +*)
(*+RM                                                                        +*)
        END (*SELECTOR*) ;

        PROCEDURE CALL(FSYS: SETOFSYS; FCP: CTP);
#         VAR LKEY: 0..NSPROC;  MATCHPAR: BOOLEAN ;

          PROCEDURE VARIABLE(FSYS: SETOFSYS);
            VAR LCP: CTP;
          BEGIN
            IF SY = IDENT THEN
              BEGIN SEARCHID([VARS,FIELD],LCP); INSYMBOL END
            ELSE BEGIN ERROR(2); LCP := UVARPTR END;
            SELECTOR(FSYS,LCP)
          END (*VARIABLE*) ;


#       PROCEDURE  RWSETUP(DFILE: ALPHA) ;
#       (* TO SET UP FILE ADDRESS PARAMETER FOR READ/WRITE *)
#
#         VAR  LCP : CTP ;  SAVED : BOOLEAN ; TEMPID : ALPHA ; TEMPSY : SYMBOL ;
#
#         BEGIN  SAVED := TRUE ;
#
#         IF SY = IDENT THEN
#           BEGIN  SEARCHID([VARS,FIELD,FUNC,KONST],LCP) ;
#           IF LCP@.IDTYPE <> NIL THEN
#             WITH LCP@.IDTYPE@ DO
#               IF FORM = FILES THEN
#                 IF FILTYPE = CHARPTR THEN SAVED := FALSE
#                 ELSE  ERROR(398) ;
#           END (* SY = IDENT *) ;
#
#         IF SAVED THEN (* USE IMPLIED FILE NAME *)
#           BEGIN   TEMPSY := SY ;  TEMPID := ID ;  SY := COMMA ;  ID := DFILE ;
#           SEARCHID([VARS],LCP) ;
#           END (* IF SAVED *)
#         ELSE  INSYMBOL ;
#
#         SELECTOR(FSYS+[COMMA,RPARENT],LCP) ;
#         IF GATTR.TYPTR <> TEXTPTR THEN  ERROR(116) ;
#         LOADADDRESS ; (* GET FILE ADR *)
#         GEN1(30(*CSP*),31(*SIO*)) ;
#         IF SAVED THEN  BEGIN  ID := TEMPID ;  SY := TEMPSY  END ;
#         END (*RWSETUP*) ;
#

          PROCEDURE GETPUTRESETREWRITE;

          BEGIN "VARIABLE(FSYS + [RPARENT]); LOADADDRESS;"
#         IF ODD(LKEY) (*GET, RESET*)  THEN  RWSETUP(NA[39] (*INPUT*))
#         ELSE (*PUT, REWRITE, PAGE*)  RWSETUP(NA[40] (*OUTPUT*) ) ;
#          "IF EBCDFLG THEN
#             IF LKEY > 2 THEN  (*RESET , REWRITE*)
#               BEGIN  GEN2(23(*INC*),ORD('A'),1000) ; EBCDFLG := FALSE  END ; "
"           IF GATTR.TYPTR <> NIL THEN
              IF GATTR.TYPTR@.FORM <> FILES THEN ERROR(116);
#             GEN1(30(*CSP*),31(*SIO*)) ;  "
#             GEN1(30(*CSP*),LKEY(*GET,PUT,RES,REW,PAG*)) ;
#             GEN1(30(*CSP*),32(*EIO*)) ;
          END (*GETPUTRESETREWRITE*) ;

          PROCEDURE READ1;

#         BEGIN (*ASSUME 'INPUT' FILE*)
#           RWSETUP(NA[39] (*'INPUT     '*));
            IF SY = COMMA THEN  INSYMBOL ;
#           IF LKEY = 5 (*READ*) THEN  IF SY <> IDENT THEN ERROR(2) ;
            IF SY = IDENT THEN
              REPEAT
(*+RM         MODIFYING := TRUE ;                                            +*)
              VARIABLE(FSYS + [COMMA,RPARENT]) ;
(*+RM         MODIFYING := FALSE ;                                           +*)
              LOADADDRESS ;
                IF GATTR.TYPTR <> NIL THEN
#                 IF STRING(GATTR.TYPTR) THEN
#                   BEGIN
#                   GEN2(51(*LDC*),1,GATTR.TYPTR@.SIZE DIV CHARSIZE) ;
#                   GEN1(30(*CSP*),27(*RDS*))
#                   END
#                 ELSE
#                   BEGIN
                    IF COMPTYPES(INTPTR,GATTR.TYPTR) THEN
#                     BEGIN  IF GATTR.BTYPE@.SIZE <> INTSIZE THEN ERROR(116);
                      GEN1(30(*CSP*),24(*RDI*))
                      END
                    ELSE
                      IF COMPTYPES(REALPTR,GATTR.TYPTR) THEN
                        GEN1(30(*CSP*),14(*RDR*))
                      ELSE
                        IF COMPTYPES(CHARPTR,GATTR.TYPTR) THEN
                          GEN1(30(*CSP*),5(*RDC*))
#                       ELSE
#                         IF COMPTYPES(BOOLPTR,GATTR.TYPTR) THEN
#                           GEN1(30(*CSP*),12(*RDB*))
#                         ELSE  ERROR(116) ;
#                   END ;
                TEST := SY <> COMMA;
                IF NOT TEST THEN INSYMBOL
              UNTIL TEST ;
            IF LKEY = 11 THEN
              BEGIN
                GEN1(30(*CSP*),23(*RLN*))
              END ;
            GEN1(30(*CSP*),32(*EIO*)) ;
          END (*READ*) ;

          PROCEDURE WRITE1;
            VAR LSP: STP; DEFAULT, DEFAULT1 : BOOLEAN; LLKEY: 0..NSPROC;
              LEN:ADDRRANGE;
#         BEGIN LLKEY := LKEY;  TEST := FALSE ;
#           RWSETUP(NA[40] (*'OUTPUT      '*) ) ;
#           IF SY = RPARENT THEN
#             BEGIN  "TEST := TRUE ;" IF LLKEY = 6 THEN ERROR(116) ; END ;
#           IF SY = COMMA THEN
#             BEGIN INSYMBOL; IF NOT (SY IN SIMPTYPEBEGSYS) THEN ERROR(6)  END ;
#           IF SY IN SIMPTYPEBEGSYS THEN
#             REPEAT  EXPRESSION(FSYS+[COMMA,COLON,RPARENT]) ;
                LSP := GATTR.TYPTR;
                IF LSP <> NIL THEN
                  IF LSP@.FORM <= SUBRANGE THEN LOAD ELSE LOADADDRESS;
                DEFAULT := TRUE ;  DEFAULT1 := TRUE ;
                IF SY = COLON THEN
                  BEGIN INSYMBOL; EXPRESSION(FSYS + [COMMA,COLON,RPARENT]);
                    IF GATTR.TYPTR <> NIL THEN
                      IF GATTR.TYPTR <> INTPTR THEN ERROR(116);
                    LOAD; DEFAULT := FALSE ;
                    IF SY = COLON THEN
                      BEGIN  INSYMBOL;  EXPRESSION(FSYS + [COMMA,RPARENT]);
                        IF GATTR.TYPTR <> NIL THEN
                          IF GATTR.TYPTR <> INTPTR THEN ERROR(116);
                        IF LSP <> REALPTR THEN ERROR(124);
#                       LOAD; DEFAULT1 := FALSE ; " ERROR(398);  "
                      END ;
                  END ;
                IF LSP = INTPTR THEN
                  BEGIN IF DEFAULT THEN GEN2(51(*LDC*),1,12);
                    GEN1(30(*CSP*),6(*WRI*))
                  END
                ELSE
                  IF LSP = REALPTR THEN
                    BEGIN IF DEFAULT THEN GEN2(51(*LDC*),1,14);
                      IF DEFAULT1 THEN GEN2(51(*LDC*),1,0);
                      GEN1(30(*CSP*),8(*WRR*))
                    END
                  ELSE
                    IF LSP = CHARPTR THEN
                      BEGIN IF DEFAULT THEN GEN2(51(*LDC*),1,1);
                        GEN1(30(*CSP*),9(*WRC*))
                      END
                    ELSE
#                   IF LSP = BOOLPTR THEN
#                     BEGIN IF DEFAULT THEN GEN2(51(*LDC*),1,5);
#                       GEN1(30(*CSP*),13(*WRB*))
#                     END
#                   ELSE
                      IF LSP <> NIL THEN
                        BEGIN
                          IF LSP@.FORM = SCALAR THEN ERROR(398)
                          ELSE
                            IF STRING(LSP) THEN
                              BEGIN LEN := LSP@.SIZE DIV CHARSIZE;
                                IF DEFAULT THEN
                                      GEN2(51(*LDC*),1,LEN);
                                GEN2(51(*LDC*),1,LEN);
                                GEN1(30(*CSP*),10(*WRS*))
                              END
                            ELSE ERROR(116)
                        END;
                TEST := SY <> COMMA;
#               IF NOT TEST THEN   INSYMBOL ;
#             UNTIL TEST;
#
            IF LLKEY = 12 THEN (*WRITELN*)
              BEGIN
                GEN1(30(*CSP*),22(*WLN*))
              END ;
#           GEN1(30(*CSP*),32(*EIO*)) ;
          END (*WRITE*) ;
"NH"
"NH"      PROCEDURE SKIPLIM;
"NH"      BEGIN
"NH"        RWSETUP( NA[40] (*OUTPUT*) );
"NH"        IF SY = COMMA THEN
"NH"          BEGIN INSYMBOL;
"NH"            IF NOT (SY IN SIMPTYPEBEGSYS) THEN ERROR(6)
"NH"          END;
"NH"        IF SY IN SIMPTYPEBEGSYS THEN
"NH"          BEGIN
"NH"            EXPRESSION( FSYS + [RPARENT] );
"NH"            IF GATTR.TYPTR <> NIL THEN
"NH"               IF GATTR.TYPTR <> INTPTR THEN ERROR(125);
"NH"            LOAD;  GEN1( 30 (*CSP*), LKEY-2 (*SKP/LIM *) );
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
          BEGIN  """ERROR(398);  VARIABLE(FSYS + [COMMA,RPARENT]);"""
#           EXPRESSION(FSYS + [COMMA, RPARENT]);
            LSP := NIL; LSP1 := NIL;
            IF GATTR.TYPTR <> NIL THEN
              WITH GATTR, GATTR.TYPTR@ DO
                IF FORM = ARRAYS THEN
                  BEGIN   LSP := INXTYPE;  LSP1 := AELTYPE;
#                 IF KIND = CST THEN  BEGIN  IMIN := 1;  IMAX := SIZE  END
#                 ELSE  IF LSP <> NIL THEN  GETBOUNDS(LSP, IMIN, IMAX);
#                 LSIZE := SIZE;
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
#                  ELSE IF IMIN < 0 THEN
#                    GEN2(23(*INC*),GETTYPE(GATTR.TYPTR),-IMIN);
#                  IF LSP1 <> NIL THEN
#                    BEGIN  IMIN := LSP1@.SIZE;  ALIGN(IMIN, LSP1@.ALN);
#                    GEN1(36 (*IXA*), IMIN);
#                    END;
#                  END;

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
#                     IF SIZE > LSIZE THEN ERROR(303 (*CANNOT PACK*));
#                     GEN1(40 (*MOV*), -SIZE);  (*MOVE BACKWARD*)
#                     END;
                  END
                ELSE ERROR(116)
          END (*PACK*) ;

          PROCEDURE UNPACK1;
#           VAR  LSP,LSP1: STP;   IMIN, IMAX, LSIZE: INTEGER;
          BEGIN """ERROR(398); VARIABLE(FSYS + [COMMA,RPARENT]);"""
#           EXPRESSION(FSYS + [COMMA, RPARENT]);
            LSP := NIL; LSP1 := NIL;
            IF GATTR.TYPTR <> NIL THEN
              WITH GATTR.TYPTR@ DO
                IF FORM = ARRAYS THEN
                  BEGIN LSP := INXTYPE; LSP1 := AELTYPE;
#                 LSIZE := SIZE;
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
#                     IF LSIZE > SIZE THEN  ERROR(303 (*SOURCE OPERAND LARGE*));
#                     IF KIND = CST THEN
#                       BEGIN  IMIN := 1;  IMAX := SIZE END
#                     ELSE IF INXTYPE <> NIL THEN GETBOUNDS(INXTYPE, IMIN,IMAX);
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
#                  IF LSP1 <> NIL THEN
#                    BEGIN  IMIN := LSP1@.SIZE;  ALIGN(IMIN, LSP1@.ALN);
#                    GEN1(36 (*IXA*), IMIN);
#                    GEN1(40 (*MOV*), -LSIZE);   (*MOVE DOWN !*)
#                    END;
#                 END;
          END (*UNPACK*) ;

          PROCEDURE NEW1;
            LABEL 1;
            VAR LSP,LSP1: STP; VARTS,LMIN,LMAX: INTEGER;
                LSIZE,LSZ: ADDRRANGE; LVAL: VALU;
          BEGIN VARIABLE(FSYS + [COMMA,RPARENT]); LOADADDRESS;
            LSP := NIL; VARTS := 0; LSIZE := 0;
            IF GATTR.TYPTR <> NIL THEN
              WITH GATTR.TYPTR@ DO
                IF FORM = POINTER THEN
                  BEGIN
                    IF ELTYPE <> NIL THEN
                      BEGIN LSIZE := ELTYPE@.SIZE;
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
#           ALIGN(LSIZE,MXDATASZE) ;
#           GEN1(58(*NEW*),LSIZE);
          END (*NEW*) ;

          PROCEDURE MARK1;
          BEGIN VARIABLE(FSYS+[RPARENT]);
             IF GATTR.TYPTR <> NIL THEN
               IF GATTR.TYPTR@.FORM = POINTER THEN
                 BEGIN LOADADDRESS; GEN0(59(*SAV*)) END
               ELSE ERROR(125)
          END(*MARK*);

          PROCEDURE RELEASE1;
          BEGIN  VARIABLE(FSYS+[RPARENT]);
                IF GATTR.TYPTR <> NIL THEN
                   IF GATTR.TYPTR@.FORM = POINTER THEN
                      BEGIN   LOAD;  GEN0(60(*RST*))  END
                   ELSE ERROR(125)
          END (*RELEASE*);

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
#           BEGIN  " EXPRESSION(FSYS+[RPARENT,COMMA]) ;  "
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
#                     IF TYPTR@.FORM <= POWER THEN
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

         "PROCEDURE SQR1;
          BEGIN
            IF GATTR.TYPTR <> NIL THEN
              IF GATTR.TYPTR = INTPTR THEN GEN0(24(*SQI*))
              ELSE
                IF GATTR.TYPTR = REALPTR THEN GEN0(25(*SQR*))
                ELSE BEGIN ERROR(125); GATTR.TYPTR := INTPTR END
          END (*SQR*) ; "

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
"NH"      BEGIN
"NH"        IF GATTR.TYPTR <> NIL THEN
"NH"           IF GATTR.TYPTR@.FORM <> POWER THEN ERROR(125);
"NH"        GEN0( 65 (*CRD*) );  GATTR.TYPTR := INTPTR;
"NH"      END (*CARD*);

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
#         BEGIN (*ERROR(398);*) (*TRANSLATES INTO 'DEC' AND 'INC'*)
#           IF GATTR.TYPTR <> NIL THEN
#              IF LKEY = 24  THEN
#                 BEGIN  IF GATTR.TYPTR <> INTPTR THEN  ERROR(116) ;
#                 GEN1(30(*CSP*),21(*CLK*)) ;
#                 END
#              ELSE
#               IF (GATTR.TYPTR = REALPTR) OR (GATTR.TYPTR@.FORM <> SCALAR) THEN
#                  ERROR(125)
#               ELSE  GEN2(LKEY(*DEC,INC*),GETTYPE(GATTR.BTYPE),1) ;
                (* LKEY HAPPENS TO BE THE OPCODE AS WELL *)
#         END (*PREDSUCCTIM*) ;

          PROCEDURE EOFEOLN;
          BEGIN
#           RWSETUP(NA[39] (*'INPUT       '*) ) ;
            IF GATTR.TYPTR <> NIL THEN
              IF GATTR.TYPTR@.FORM <> FILES THEN ERROR(125);  "
            ELSE GEN1(30(*CSP*),26(*ELN*));  "
            (* LKEY HAPPENS TO BE THE CSP NUMBER AS WELL ! *)
#           GEN1(30(*CSP*), LKEY(*EOF*)(*ELN*)) ;
#           GEN1(30(*CSP*),32(*EIO*)) ;
            GATTR.TYPTR := BOOLPTR
          END (*EOF*) ;

       """PROCEDURE MATH;
          BEGIN
            IF GATTR.TYPTR <> NIL THEN
              IF GATTR.TYPTR = INTPTR  THEN
                BEGIN  GEN0(10(*FLT*)) ;
                GATTR.TYPTR := REALPTR ;
              END ;
              IF GATTR.TYPTR <> REALPTR THEN ERROR(116)
              ELSE   GEN1(30(*CSP*), LKEY-12(*SIN..ATAN*)) ;
          END (*MATH*) ;
       """
          PROCEDURE CALLNONSTANDARD;
            VAR NXT,LCP: CTP; LSP: STP; LKIND: IDKIND; LB: BOOLEAN;
#               LOCPAR, LLC, LSIZE: ADDRRANGE;
          BEGIN LOCPAR := 0;
            WITH FCP@ DO
              BEGIN NXT := NEXT; LKIND := PFKIND;
"CM"              GEN1(41(*MST*),PFLEV) ;
"S1" "            GEN3(41(*MST*), PFLEV+1, FPRMSZE, RPRMSZE) ;                 "
(*+CG                                                                         *)
(*+CG             T_CLIST := CALL_HEAD ;                                      *)
(*+CG             WHILE NAME < T_CLIST@.NAME DO  T_CLIST := T_CLIST@.NXT ;    *)
(*+CG             IF T_CLIST@.NAME <> NAME THEN                               *)
(*+CG               BEGIN   NEW(T2_CLIST) ;   T2_CLIST@ := T_CLIST@ ;         *)
(*+CG               T_CLIST@.NAME := NAME ;   T_CLIST@.NXT := T2_CLIST ;      *)
(*+CG               T_CLIST@.CNT := 1 ;  T_CLIST@.LVL := PFLEV ;              *)
#                   IF PFLEV = LEVEL THEN   LOCAL_CALL := TRUE ;
(*+CG               END                                                       *)
(*+CG             ELSE  T_CLIST@.CNT := T_CLIST@.CNT+1 ;                      *)
(*+CG                                                                         *)
              END;
            IF SY = LPARENT THEN
              BEGIN LLC := LC;
                REPEAT LB := FALSE; (*DECIDE WHETHER PROC/FUNC MUST BE PASSED*)
                  IF LKIND = ACTUAL THEN
                    BEGIN
                      IF NXT = NIL THEN ERROR(126)
                      ELSE LB := NXT@.KLASS IN [PROC,FUNC]
                    END ELSE ERROR(398);
                  (*FOR FORMAL PROC/FUNC LB IS FALSE AND EXPRESSION
                   WILL BE CALLED, WHICH WILL ALLWAYS INTERPRET A PROC/FUNC ID
                  AT ITS BEGINNING AS A CALL RATHER THAN A PARAMETER PASSING.
                  IN THIS IMPLEMENTATION, PARAMETER PROCEDURES/FUNCTIONS
                  ARE THEREFORE NOT ALLOWED TO HAVE PROCEDURE/FUNCTION
                  PARAMETERS*)
                  INSYMBOL;
                  IF LB THEN   (*PASS FUNCTION OR PROCEDURE*)
                    BEGIN ERROR(398);
                      IF SY <> IDENT THEN
                        BEGIN ERROR(2); SKIP(FSYS + [COMMA,RPARENT]) END
                      ELSE
                        BEGIN
                          IF NXT@.KLASS = PROC THEN SEARCHID([PROC],LCP)
                          ELSE
                            BEGIN SEARCHID([FUNC],LCP);
                              IF NOT COMPTYPES(LCP@.IDTYPE,NXT@.IDTYPE) THEN
                                ERROR(128)
                            END;
                          INSYMBOL;
                          IF NOT (SY IN FSYS + [COMMA,RPARENT]) THEN
                            BEGIN ERROR(6); SKIP(FSYS + [COMMA,RPARENT]) END
                        END
                    END (*IF LB*)
                  ELSE
                    BEGIN
(*+RM               IF NXT <> NIL THEN                                       +*)
(*+RM                  IF NXT@.VKIND = FORMAL THEN  MODIFYING := TRUE ;      +*)
                    EXPRESSION(FSYS + [COMMA,RPARENT]);
(*+RM               MODIFYING := FALSE ;                                     +*)
                      IF GATTR.TYPTR <> NIL THEN
                        IF LKIND = ACTUAL THEN
                          BEGIN
                            IF NXT <> NIL THEN
                              BEGIN LSP := NXT@.IDTYPE;
                                IF LSP <> NIL THEN
                                  BEGIN
                                    IF (NXT@.VKIND = ACTUAL) THEN
#                                     IF LSP@.FORM <= POWER THEN
#                                       BEGIN LOAD;
#                                       IF DEBUG THEN
#                                         BEGIN  ASSIGN := TRUE ;
#                                         CHKBNDS(LSP) ;  ASSIGN := FALSE ;
#                                         END ;
                                        IF COMPTYPES(REALPTR,LSP)
                                           AND (GATTR.TYPTR = INTPTR) THEN
                                          BEGIN GEN0(10(*FLT*));
                                            GATTR.TYPTR := REALPTR ;
                                            GATTR.BTYPE := REALPTR ;
                                          END;
                                        LOCPAR := LOCPAR+ 1 (*LSP@.SIZE*) ;
#                                       IF PACKDATA THEN
#                                         BEGIN
#                                         IF LSP@.SIZE = 4 THEN GEN0(61(*ORD*));
#                                         IF LSP@.SIZE = 1 THEN GEN0(62(*CHR*));
                                           END (*PACKDATA*) ;
#
#                                       IF FCP@.FRTRN THEN
#                                         BEGIN (*PASS ADDRESS OF PARAMETER*)
#                                         LSIZE := LSP@.SIZE ;
#                                         ALIGN(LC,LSIZE) ;
#                                         WITH GATTR DO
#                                           BEGIN
#                                           VLEVEL := LEVEL ;   DPLMT := LC ;
#                                           KIND := VARBL ;  ACCESS := DRCT ;
#                                           END (*WITH*) ;
#                                         STORE(GATTR) ;  LOADADDRESS ;
#                                         LC := LC+LSIZE ;
#                                         IF LC > LCMAX THEN LCMAX := LC ;
#                                         END (*IF FCP@.FRTRN*) ;
#
"S1" "                                  IF NOT FCP@.EXTRN THEN                 "
"S1" "                                    GEN1(64(*PRM*), GETTYPE(LSP));       "
                                        END
                                      ELSE  (* LSP@.FORM > POWER *)
                                        BEGIN
                                        LOADADDRESS;
                                        LOCPAR := LOCPAR+ 1 (*PTRSIZE*);
"S1" "                                  IF NOT FCP@.EXTRN THEN                 "
"S1" "                                    GEN1(64(*PRM*), ORD('A')) ;          "
                                        END
                                    ELSE  (* VKIND = FORMAL I.E. VAR PARM *)
                                      IF GATTR.KIND = VARBL THEN
                                        BEGIN  LOADADDRESS;
                                        LOCPAR := LOCPAR + 1 (*PTRSIZE*);
"S1" "                                  IF NOT FCP@.EXTRN THEN                 "
"S1" "                                    GEN1(64(*PRM*), ORD('A')) ;          "
                                        IF GATTR.BTYPE@.SIZE <> LSP@.SIZE THEN
                                          ERROR(142) ;
                                        END
                                      ELSE ERROR(154);
                                    IF NOT COMPTYPES(LSP,GATTR.TYPTR) THEN
                                      ERROR(142)
                                  END
                              END
                          END
                      ELSE (*LKIND = FORMAL*)
                        BEGIN (*PASS FORMAL PROC/FUNC PARAM*)
                        END
                    END;
                  IF (LKIND = ACTUAL) AND (NXT <> NIL) THEN NXT := NXT@.NEXT
                UNTIL SY <> COMMA;
                LC := LLC;
              IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4)
            END (*IF LPARENT*);
#           LOCPAR := LOCPAR*2 ;
            IF LKIND = ACTUAL THEN
              BEGIN IF NXT <> NIL THEN ERROR(126);
                WITH FCP@ DO
            "     IF EXTRN THEN GEN1(30(*CSP*),PFNAME)
#                 ELSE    "
#                   BEGIN
#                   IF SAVEFP THEN LOCPAR := LOCPAR+1 ;  (*ENCODE SAVE FPR FLG*)
                    NAMFLAG := XLINK OR FRTRN OR EXTRN ;
                    GENCUPENT(46(*CUP*),PROCTYPE(FCP),LOCPAR,PFNAME,NAME);
                    END ;
              END;
#           GATTR.TYPTR := FCP@.IDTYPE ;  GATTR.BTYPE := GATTR.TYPTR ;
          END (*CALLNONSTANDARD*) ;

        BEGIN (*CALL*)
          IF FCP@.PFDECKIND = STANDARD THEN
            BEGIN
            LKEY := FCP@.KEY;

#           IF LKEY IN [27..32 (*SIN..ATAN*)] THEN
#             BEGIN  FCP := FCP@.NEXT;  (*POINT TO FORTRAN VERSION*)
#             CALLNONSTANDARD;
#             END
#           ELSE

#             BEGIN  (*OTHER STANDARD PROCS/FUNCTIONS*)
#             IF SY = LPARENT THEN
#               BEGIN  INSYMBOL ;   MATCHPAR := TRUE ;
#               IF SY = RPARENT THEN
#                 IF NOT (LKEY IN [0,1,2,3,4,11,12,25,26]) THEN ERROR(7) ;
                         (*GET,PUT,RESET,REWRITE,RDLN,WRITELN,EOF,ELN*)
#               END
#             ELSE
#               BEGIN  IF NOT (LKEY IN [0,1,2,3,4,11,12,25,26]) THEN ERROR(6) ;
                              (*GET,PUT,RESET,REWRITE,RDLN,WRITELN,EOF,ELN*)
#             " IF SY =RPARENT THEN ERROR(6) ; "
#               MATCHPAR := FALSE ;
#               END ;

"NH"          IF LKEY IN [14..24,27..33,38,39] THEN
"NH"             (*TRAP,EXIT,ABS...MATH,ODD,CARD,EXPO*)
#               BEGIN
                IF LKEY = 14 (*TRAP*) THEN  EXPRESSION(FSYS+[COMMA])
                ELSE  EXPRESSION(FSYS+[RPARENT]) ;
                LOAD ;
                END  ;
#
#               CASE LKEY OF
#             0,1,2,
#               3,4:    GETPUTRESETREWRITE;
#              5,11:    READ1;
#              6,12:    WRITE1;
#                 7:    PACK1;
#                 8:    UNPACK1;
#                 9:    NEW1;
#                10:    RELEASE1;
#                13:    MARK1;
#             14,15:    TRAPEXIT;
#             16,17:    SQRABS;
#               "17:    SQR1;"
#             18,19:    TRUNCROUND;
#                20:    ORD1;
#                21:    CHR1;
#          22,23,24:    PREDSUCCTIM;
#             25,26:    EOFEOLN ;
#       """27,28,29,
#          30,31,32:    MATH ;  """
#                33:    ODD1;
"NH"             35:    MESSAGE1;
"NH"          36,37:    SKIPLIM;
"NH"             38:    CARD1;
"NH"             39:    EXPO1;
                END (*CASE LKEY OF*) ;

"NH"          IF LKEY IN [16..24,27..33,38,39] THEN
"NH"             GATTR.BTYPE := GATTR.TYPTR;
#             IF MATCHPAR THEN
#               IF SY = RPARENT THEN INSYMBOL ELSE  ERROR(4) ;
              END (*OTHER STANDARD PROCEDURES AND FUNCTIONS*)

            END (*IF FCP@.PFDECKIND = STANDARD*)

          ELSE CALLNONSTANDARD

        END (*CALL*) ;

        PROCEDURE EXPRESSION;
          VAR LATTR: ATTR; LOP: OPERATOR; TYPIND: CHAR; LSIZE: ADDRRANGE;

          PROCEDURE SIMPLEEXPRESSION(FSYS: SETOFSYS);
            VAR LATTR: ATTR; LOP: OPERATOR; SIGNED: BOOLEAN;

            PROCEDURE TERM(FSYS: SETOFSYS);
              VAR LATTR: ATTR; LOP: OPERATOR;

              PROCEDURE FACTOR(FSYS: SETOFSYS);
                VAR LCP: CTP; LVP: CSP; VARPART: BOOLEAN;
#                   LATTR: ATTR;
#                   CSTPART: SETRANGE  ; LSP: STP;   I: 0..64 ;
              BEGIN
                IF NOT (SY IN FACBEGSYS) THEN
                  BEGIN ERROR(58); SKIP(FSYS + FACBEGSYS);
                    GATTR.TYPTR := NIL
                  END;
                WHILE SY IN FACBEGSYS DO
                  BEGIN
                    CASE SY OF
              (*ID*)    IDENT:
                        BEGIN SEARCHID([KONST,VARS,FIELD,FUNC],LCP);
                          INSYMBOL;
                          IF LCP@.KLASS = FUNC THEN
#                           BEGIN CALL(FSYS,LCP);
#                             WITH GATTR DO
#                               BEGIN KIND := EXPR;
#                                 IF TYPTR <> NIL THEN
#                                   BEGIN  BTYPE := TYPTR ;
#                                   IF TYPTR@.FORM=SUBRANGE THEN
#                                     TYPTR := TYPTR@.RANGETYPE
#                                   END
#                               END
#                           END
                          ELSE
                            IF LCP@.KLASS = KONST THEN
                              WITH GATTR, LCP@ DO
                                BEGIN TYPTR := IDTYPE; KIND := CST;
#                                 CVAL := VALUES; GATTR.BTYPE := GATTR.TYPTR
                                END
                            ELSE
                              BEGIN SELECTOR(FSYS,LCP);
                                IF GATTR.TYPTR<>NIL THEN(*ELIM.SUBR.TYPES TO*)
                                  WITH GATTR,TYPTR@ DO(*SIMPLIFY LATER TESTS*)
                                    IF FORM = SUBRANGE THEN
                                      TYPTR := RANGETYPE
                              END
                        END;
              (*CST*)   INTCONST:
                        BEGIN
                          WITH GATTR DO
                            BEGIN TYPTR := INTPTR; KIND := CST;
#                             CVAL := VAL; BTYPE := TYPTR
                            END;
                          INSYMBOL
                        END;
                      REALCONST:
                        BEGIN
                          WITH GATTR DO
                            BEGIN TYPTR := REALPTR; KIND := CST;
#                             BTYPE := TYPTR ;   CVAL := VAL
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
                                      INXTYPE := NIL; SIZE := LNGTH*CHARSIZE
                                    END;
                                  TYPTR := LSP
                                END;
#                             KIND := CST; CVAL := VAL;BTYPE := TYPTR;
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
                        BEGIN INSYMBOL; CSTPART := [ ]; VARPART := FALSE;
                          NEW(LSP,POWER);
                          WITH LSP@ DO
                            BEGIN ELSET:=NIL;SIZE:=SETSIZE;FORM:=POWER END;
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
                                        IF GATTR.KIND = CST THEN
#                                         BEGIN
#                                         LATTR := GATTR ;
#                                         IF SY = DOTDOT THEN   (*RANGE GIVEN*)
#                                           BEGIN  INSYMBOL ;"LATTR := GATTR;"
#                                           EXPRESSION(FSYS+[COMMA,RBRACK]) ;
#                                           END ;
#                                         IF GATTR.TYPTR <> LATTR.TYPTR THEN
#                                             ERROR(137)
#                                         ELSE
#
#                                           IF (LATTR.CVAL.IVAL < 0)         OR
#                                              (GATTR.CVAL.IVAL > SETMAX  )  OR
#                                              (LATTR.CVAL.IVAL>GATTR.CVAL.IVAL)
#                                             THEN  ERROR(304)
#                                           ELSE
#                                             FOR I := LATTR.CVAL.IVAL TO
#                                                      GATTR.CVAL.IVAL DO
#                                               CSTPART := CSTPART+[I] ;
#                                         END  (* GATTR.KIND = CST *)
#                                       ELSE
#                                         BEGIN LOAD;
#                                         IF NOT COMPTYPES(GATTR.TYPTR,INTPTR)
#                                           THEN GEN0(61(*ORD*));
#                                         IF DEBUG THEN
#                                           GEN3(45(*CHK*),ORD('S'),0,SETMAX  );
#                                         GEN0(29(*SGS*));
                                          IF VARPART THEN GEN0(31(*UNI*))
                                          ELSE VARPART := TRUE
                                          END;
                                        LSP@.ELSET := GATTR.TYPTR;
                                        GATTR.TYPTR := LSP
                                      END
                                    ELSE ERROR(137);
                                TEST := SY <> COMMA;
                                IF NOT TEST THEN INSYMBOL
                              UNTIL TEST;
                              IF SY = RBRACK THEN INSYMBOL ELSE ERROR(12)
                            END;
                          IF VARPART THEN
                            BEGIN
                              IF CSTPART <> [ ] THEN
                                BEGIN NEW(LVP,PSET); LVP@.PVAL := CSTPART;
                                  "LVP@.CCLASS := PSET;"
#                                 CNSTPTR := LVP;
#                                 GEN2(51(*LDC*),5,0);
                                  GEN0(31(*UNI*)); GATTR.KIND := EXPR
                                END
                            END
                          ELSE
                            BEGIN NEW(LVP,PSET); LVP@.PVAL := CSTPART;
                             "LVP@.CCLASS := PSET;"
                              GATTR.CVAL.VALP := LVP
                            END
                        END
                    END (*CASE*) ;
                    IF NOT (SY IN FSYS) THEN
                      BEGIN ERROR(6); SKIP(FSYS + FACBEGSYS) END
                  END (*WHILE*)
              END (*FACTOR*) ;

            BEGIN (*TERM*)
              FACTOR(FSYS + [MULOP]);
              WHILE SY = MULOP DO
                      BEGIN LOAD; LATTR := GATTR; LOP := OP;
                  INSYMBOL; FACTOR(FSYS + [MULOP]); LOAD;
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
                                ELSE
                                  IF(LATTR.TYPTR@.FORM=POWER)
                                    AND COMPTYPES(LATTR.TYPTR,GATTR.TYPTR)THEN
                                    GEN0(12(*INT*))
                                  ELSE BEGIN ERROR(134);GATTR.TYPTR:=NIL END
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
                  ELSE GATTR.TYPTR := NIL
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
              BEGIN LOAD; LATTR := GATTR; LOP := OP;
                INSYMBOL; TERM(FSYS + [ADDOP]); LOAD;
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
                          ELSE IF(LATTR.TYPTR@.FORM=POWER)
                                 AND COMPTYPES(LATTR.TYPTR,GATTR.TYPTR) THEN
                                 GEN0(31(*UNI*))
                               ELSE BEGIN ERROR(134);GATTR.TYPTR:=NIL END
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
                          ELSE
                            IF (LATTR.TYPTR@.FORM = POWER)
                              AND COMPTYPES(LATTR.TYPTR,GATTR.TYPTR) THEN
                              GEN0(5(*DIF*))
                            ELSE BEGIN ERROR(134); GATTR.TYPTR := NIL END
                        END;
          (*OR*)      OROP:
                      IF(LATTR.TYPTR=BOOLPTR)AND(GATTR.TYPTR=BOOLPTR)THEN
                        GEN0(13(*IOR*))
                      ELSE BEGIN ERROR(134); GATTR.TYPTR := NIL END
                  END (*CASE*)
                ELSE GATTR.TYPTR := NIL
              END (*WHILE*)
          END (*SIMPLEEXPRESSION*) ;

        BEGIN (*EXPRESSION*)
          SIMPLEEXPRESSION(FSYS + [RELOP]);
          IF SY = RELOP THEN
            BEGIN
              IF GATTR.TYPTR <> NIL THEN
                IF GATTR.TYPTR@.FORM <= POWER THEN LOAD
                ELSE LOADADDRESS;
              LATTR := GATTR; LOP := OP;
#   (*IN*)    IF LOP = INOP THEN
#               BEGIN
#               IF NOT COMPTYPES(GATTR.TYPTR,INTPTR) THEN  GEN0(61(*ORD*)) ;
#               IF DEBUG THEN GEN3(45(*CHK*),ORD('S'),0,SETMAX  ) ;
#               END ;
              INSYMBOL; SIMPLEEXPRESSION(FSYS);
              IF GATTR.TYPTR <> NIL THEN
                IF GATTR.TYPTR@.FORM <= POWER THEN LOAD
                ELSE LOADADDRESS;
              IF (LATTR.TYPTR <> NIL) AND (GATTR.TYPTR <> NIL) THEN
                IF LOP = INOP THEN
                  IF GATTR.TYPTR@.FORM = POWER THEN
                    IF COMPTYPES(LATTR.TYPTR,GATTR.TYPTR@.ELSET) THEN
                      GEN0(11(*INN*))
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
                              TYPIND := 'S'
                          END;
                          ARRAYS:
                            BEGIN
                              IF NOT STRING(LATTR.TYPTR)
                              AND(LOP IN[LTOP,LEOP,GTOP,GEOP])THEN ERROR(131);
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
                        CASE LOP OF
                          LTOP: GEN2(53(*LES*),ORD(TYPIND),LSIZE);
                          LEOP: GEN2(52(*LEQ*),ORD(TYPIND),LSIZE);
                          GTOP: GEN2(49(*GRT*),ORD(TYPIND),LSIZE);
                          GEOP: GEN2(48(*GEQ*),ORD(TYPIND),LSIZE);
                          NEOP: GEN2(55(*NEQ*),ORD(TYPIND),LSIZE);
                          EQOP: GEN2(47(*EQU*),ORD(TYPIND),LSIZE)
                        END
                      END
                    ELSE ERROR(129)
                  END;
              GATTR.TYPTR := BOOLPTR; GATTR.KIND := EXPR
            END (*SY = RELOP*)
        END (*EXPRESSION*) ;

        PROCEDURE ASSIGNMENT(FCP: CTP);
          VAR LATTR: ATTR;
        BEGIN
(*+RM     MODIFYING := TRUE ;                                                +*)
          SELECTOR(FSYS + [BECOMES],FCP);
(*+RM     MODIFYING := FALSE ;                                               +*)
#         VAR_MOD := VAR_MOD+1 ;
          IF SY = BECOMES THEN
            BEGIN
              IF GATTR.TYPTR <> NIL THEN
                IF (GATTR.ACCESS<>DRCT) OR (GATTR.TYPTR@.FORM>POWER) THEN
                  LOADADDRESS;
              LATTR := GATTR;
              INSYMBOL; EXPRESSION(FSYS);
              IF GATTR.TYPTR <> NIL THEN
                IF GATTR.TYPTR@.FORM <= POWER THEN LOAD
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
#                       ASSIGN := TRUE ;  CHKBNDS(LATTR.TYPTR);  ASSIGN := FALSE
#                       END ;

                      CASE LATTR.TYPTR@.FORM OF
                        SCALAR,
                        SUBRANGE,
                        POINTER,
                        POWER:   STORE(LATTR);
                        ARRAYS,
                        RECORDS: GEN1(40(*MOV*),LATTR.TYPTR@.SIZE);
                        FILES: ERROR(146)
                      END  (*CASE LATTR...*)
#                   END
                  ELSE ERROR(129)
                END
            END (*SY = BECOMES*)
          ELSE ERROR(51)
        END (*ASSIGNMENT*) ;

        PROCEDURE GOTOSTATEMENT;
          VAR LLP: LBP; FOUND: BOOLEAN; TTOP,TTOP1: DISPRANGE;
        BEGIN
          IF SY = INTCONST THEN
            BEGIN
              FOUND := FALSE;  TTOP := TOP;
#             WHILE DISPLAY[TTOP].OCCUR <> BLCK DO TTOP := TTOP - 1;
#             TTOP1 := TTOP;
#             REPEAT
                LLP := DISPLAY[TTOP].FLABEL;
                WHILE (LLP <> NIL) AND NOT FOUND DO
                  WITH LLP@ DO
                    IF LABVAL = VAL.IVAL THEN
                      BEGIN FOUND := TRUE;
                        IF TTOP = TTOP1 THEN
                          BEGIN
                          GENUJPFJP(57(*UJP*),LABNAME) ;
"CT"                      CTREMIT(CTRGOTO, 0, LINECNT, 0, LINECNT)
                          END
                        ELSE (*GOTO LEADS OUT OF PROCEDURE*) ERROR(398)
                      END
                    ELSE LLP := NEXTLAB;
                TTOP := TTOP - 1
              UNTIL FOUND OR (TTOP = 0);
              IF NOT FOUND THEN ERROR(167);
              INSYMBOL
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
          VAR LCIX1,LCIX2: INTEGER;
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
          LABEL 1, 2;
          TYPE CIP = @CASEINFO;
               CASEINFO = PACKED
                          RECORD NEXT: CIP;
                            CSSTART: INTEGER;
                            CSLAB: INTEGER
                          END;
#         VAR LSP,LSP1: STP; FSTPTR,LPT1,LPT2,LPT3: CIP; LVAL,LVAL1: VALU;
#             LADDR, LCIX, LCIX1, UBND, LBND: ADDRRANGE ;
#             LMIN, LMAX : INTEGER ;
"CT"          FIRSTLN : INTEGER; TEMPLN  : INTEGER;
"CT"          CTRCASES : INTEGER; CTRNO : CTRRANGE;
        BEGIN EXPRESSION(FSYS + [OFSY,COMMA,COLON]);
#         LOAD ; " ALIGN(LC,INTSIZE) ;  LLC := LC ; "
#         LSP := GATTR.TYPTR;
#         IF LSP <> NIL THEN
#           IF (LSP@.FORM <> SCALAR) OR (LSP = REALPTR) THEN
#             BEGIN  ERROR(144); LSP := NIL END
#           ELSE  IF NOT COMPTYPES(LSP,INTPTR) THEN  GEN0(61(*ORD*)) ;
#         IF DEBUG THEN  CHKBNDS(GATTR.TYPTR) ;
          IF SY = OFSY THEN INSYMBOL ELSE ERROR(8);
#         FSTPTR := NIL ;  GENLABEL(LBND) ;  GENLABEL(UBND) ;
#         GENLABEL(LCIX) ;  GENLABEL(LADDR);
#         (* WE SHOULD HAVE:  LADDR = LCIX+1 = UBND+2 = LBND+3   HERE *)
#         GENUJPFJP(44 (*XJP*), LBND) ;  "GENCASE(LBND,UBND,LCIX) ; "
"CT"      FIRSTLN := LINECNT;  CTRCASES := 0;
#         LMAX := CIXMAX;
          REPEAT
            LPT3 := NIL; GENLABEL(LCIX1);
#           IF NOT(SY IN [SEMICOLON,ENDSY]) THEN
#           BEGIN
              REPEAT CONSTANT(FSYS + [COMMA,COLON,DOTDOT],LSP1,LVAL);
                IF LSP <> NIL THEN
                  IF COMPTYPES(LSP,LSP1) THEN
#                   BEGIN
#          """      IF LSP = CHARPTR THEN  LVAL.IVAL := EBCDIC[CHR(LVAL.IVAL)];"
#                   LVAL1.IVAL := LVAL.IVAL ;
#                   IF SY = DOTDOT THEN
#                     BEGIN  INSYMBOL ;
#                     CONSTANT(FSYS+[COMMA,COLON],LSP1,LVAL1)
#                     END ;
#                   IF COMPTYPES(LSP,LSP1) THEN
#                       FOR LMIN := LVAL.IVAL TO LVAL1.IVAL DO
                          BEGIN LPT1 := FSTPTR; LPT2 := NIL;
#                         LMAX := LMAX-1;
#                         IF LMAX <= 0 THEN
#                           BEGIN  ERROR(157);  GOTO 2  END;
                          WHILE LPT1 <> NIL DO
                            WITH LPT1@ DO
                              BEGIN
                              IF CSLAB <= LMIN THEN
                                BEGIN
                                IF CSLAB = LMIN THEN ERROR(156);
                                GOTO 1
                                END;
                              LPT2 := LPT1; LPT1 := NEXT
                              END;
            1:            NEW(LPT3);
                          WITH LPT3@ DO
                            BEGIN NEXT := LPT1; CSLAB := LMIN ;
                            CSSTART := LCIX1
                            END;
                          IF LPT2 = NIL THEN FSTPTR := LPT3
                          ELSE LPT2@.NEXT := LPT3
                         END
                    ELSE ERROR(147);
                    END ;
                TEST := SY <> COMMA;
                IF NOT TEST THEN INSYMBOL
              UNTIL TEST;
#         2:
              IF SY = COLON THEN INSYMBOL ELSE ERROR(5);
              PUTLABEL(LCIX1);
"CT"          TEMPLN := LINECNT; (*** COUNTER HERE ***)
"CT"          CTRNO := CTRGEN;  CTRCASES := CTRCASES+1 ;
              REPEAT STATEMENT(FSYS + [SEMICOLON])
              UNTIL NOT (SY IN STATBEGSYS);
              IF LPT3 <> NIL THEN
                GENUJPFJP(57(*UJP*),LADDR);
"CT"          CTREMIT(CTRCASE, CTRNO, TEMPLN, 0, LINECNT);
#           END ;
            TEST := SY <> SEMICOLON;
            IF NOT TEST THEN INSYMBOL ;
          UNTIL TEST;
          IF FSTPTR <> NIL THEN
            BEGIN LMAX := FSTPTR@.CSLAB;
              (*REVERSE POINTERS*)
              LPT1 := FSTPTR; FSTPTR := NIL;
              REPEAT LPT2 := LPT1@.NEXT; LPT1@.NEXT := FSTPTR;
                FSTPTR := LPT1; LPT1 := LPT2
              UNTIL LPT1 = NIL;
              LMIN := FSTPTR@.CSLAB;

#           END
#         ELSE  BEGIN  LMIN := 1 ;  LMAX := 0  END ;
#         GENDEF(LBND,LMIN) ;  GENDEF(UBND,LMAX) ;  PUTLABEL(LCIX) ;
          IF LMAX - LMIN < CIXMAX THEN
#           BEGIN
#           IF FSTPTR <> NIL THEN
              REPEAT
                WITH FSTPTR@ DO
                  BEGIN
                    WHILE CSLAB > LMIN DO
                      BEGIN GENUJPFJP(57(*UJP*),LADDR); LMIN:=LMIN+1 END;
                    GENUJPFJP(57(*UJP*),CSSTART);
                    FSTPTR := NEXT; LMIN := LMIN + 1
                  END
              UNTIL FSTPTR = NIL;
              PUTLABEL(LADDR) ;
"CT"          CTREMIT(CTRCASE, 0, FIRSTLN, CTRCASES, LINECNT);
            END
          ELSE ERROR(157) ;
          IF SY = ENDSY THEN INSYMBOL ELSE ERROR(13)
        END (*CASESTATEMENT*) ;

        PROCEDURE REPEATSTATEMENT;
          VAR LADDR: INTEGER;
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
          VAR LADDR, LCIX: INTEGER;
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
          VAR LATTR: ATTR; LSP: STP;  LSY: SYMBOL;
#             LCIX, LADDR: LABELRNG ;  LLC : ADDRRANGE ;
"CT"          FIRSTLN : INTEGER; CTRNO : CTRRANGE;
        BEGIN
          IF SY = IDENT THEN
            BEGIN SEARCHID([VARS],LCP);
              WITH LCP@, LATTR DO
#               BEGIN TYPTR := IDTYPE; KIND := VARBL; BTYPE := TYPTR ;
                  IF VKIND = ACTUAL THEN
                    BEGIN ACCESS := DRCT; VLEVEL := VLEV;
                      DPLMT := VADDR ;
(*+RM                 IF GET_STAT THEN                                       +*)
(*+RM                   WRITE(QRR, ' #MOD', CHR( GETTYPE(BTYPE) ), ' ':2,    +*)
(*+RM                              ' #DIR', VLEVEL:3, DPLMT:8, ' #MND   ' ) ;+*)
                    END
                  ELSE BEGIN ERROR(155); TYPTR := NIL END
                END;
              IF LATTR.TYPTR <> NIL THEN
                IF (LATTR.TYPTR@.FORM > SUBRANGE)
                   OR COMPTYPES(REALPTR,LATTR.TYPTR) THEN
                  BEGIN ERROR(143); LATTR.TYPTR := NIL END;
              INSYMBOL
            END
          ELSE
            BEGIN ERROR(2); SKIP(FSYS + [BECOMES,TOSY,DOWNTOSY,DOSY]) END;
          IF SY = BECOMES THEN
            BEGIN INSYMBOL; EXPRESSION(FSYS + [TOSY,DOWNTOSY,DOSY]);
              IF GATTR.TYPTR <> NIL THEN
                  IF GATTR.TYPTR@.FORM <> SCALAR THEN ERROR(144)
                  ELSE
                    IF COMPTYPES(LATTR.TYPTR,GATTR.TYPTR) THEN
                      BEGIN LOAD;
#                     "IF DEBUG THEN CHKBNDS(LATTR.TYPTR) ;"  STORE(LATTR) ;
                      END
                    ELSE ERROR(145)
            END
          ELSE
            BEGIN ERROR(51); SKIP(FSYS + [TOSY,DOWNTOSY,DOSY]) END;
          IF SY IN [TOSY,DOWNTOSY] THEN
            BEGIN LSY := SY; INSYMBOL; EXPRESSION(FSYS + [DOSY]);
              IF GATTR.TYPTR <> NIL THEN
              IF GATTR.TYPTR@.FORM <> SCALAR THEN ERROR(144)
                ELSE
                  IF COMPTYPES(LATTR.TYPTR,GATTR.TYPTR) THEN
#                   BEGIN  LOAD;  "IF DEBUG THEN  CHKBNDS(LATTR.TYPTR) ;"
#                     ALIGN(LC,INTSIZE) ;  LLC := LC ;
#                     IF GATTR.BTYPE <> INTPTR THEN  GEN0(61(*ORD*));
#                     GEN3(56(*STR*),ORD('I'),LEVEL,LLC);
#                     GATTR := LATTR; LOAD;
#                     IF GATTR.BTYPE <> INTPTR THEN  GEN0(61(*ORD*));
#                     GEN3(54(*LOD*),ORD('I'),LEVEL,LLC);
#                     IF LSY = TOSY THEN GEN2(52(*LEQ*),ORD('I'),1)
#                     ELSE GEN2(48(*GEQ*),ORD('I'),1);
#                     LC := LC + INTSIZE;
#                     IF LC > LCMAX THEN LCMAX := LC;
                    END
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
#         IF GATTR.BTYPE <> INTPTR THEN  GEN0(61(*ORD*));
#         GEN3(54(*LOD*),ORD('I'),LEVEL,LLC) ;
#         GEN2(55(*NEQ*),ORD('I'),1);
#         GENUJPFJP(33(*FJP*),LCIX) ;
#         GATTR := LATTR; LOAD;
#         IF LSY = TOSY THEN  GEN2(23(*INC*),GETTYPE(GATTR.BTYPE),1)
#         ELSE  GEN2(22(*DEC*),GETTYPE(GATTR.BTYPE),1);
#         IF DEBUG THEN CHKBNDS(LATTR.TYPTR) ;
          STORE(LATTR); GENUJPFJP(57(*UJP*),LADDR); PUTLABEL(LCIX);
          LC := LLC ;
"CT"      CTREMIT(CTRFOR, CTRNO, FIRSTLN, 0, LINECNT);
        END (*FORSTATEMENT*) ;

        PROCEDURE WITHSTATEMENT;
#         VAR LCP: CTP; LCNT: DISPRANGE; LLC: ADDRRANGE;
        BEGIN LCNT := TOP ; LLC := LC ;
          REPEAT
            IF SY = IDENT THEN
              BEGIN SEARCHID([VARS,FIELD],LCP); INSYMBOL END
            ELSE BEGIN ERROR(2); LCP := UVARPTR END;
            SELECTOR(FSYS + [COMMA,DOSY],LCP);
            IF GATTR.TYPTR <> NIL THEN
              IF GATTR.TYPTR@.FORM = RECORDS THEN
                IF TOP < DISPLIMIT THEN
#                 BEGIN  TOP := TOP + 1;
                    WITH DISPLAY[TOP] DO
                      BEGIN FNAME := GATTR.TYPTR@.FSTFLD;
                        FLABEL := NIL
                      END;
                    IF GATTR.ACCESS = DRCT THEN
                      WITH DISPLAY[TOP] DO
                        BEGIN OCCUR := CREC; CLEV := GATTR.VLEVEL;
                          CDSPL := GATTR.DPLMT
                        END
                    ELSE
#                     BEGIN  LOADADDRESS;  ALIGN(LC,PTRSIZE) ;
#                     GEN3(56(*STR*),ORD('A'),LEVEL,LC);(*=GETTYPE(GAT.TYP)*)
                        WITH DISPLAY[TOP] DO
                          BEGIN OCCUR := VREC; VDSPL := LC END;
#                       LC := LC + PTRSIZE;
                        IF LC > LCMAX THEN LCMAX := LC
                      END
                  END
                ELSE ERROR(250)
              ELSE ERROR(140);
            TEST := SY <> COMMA;
            IF NOT TEST THEN INSYMBOL
          UNTIL TEST;
          IF SY = DOSY THEN INSYMBOL ELSE ERROR(54);
          STATEMENT(FSYS);
#         TOP :=  LCNT ; LC := LLC ;
        END (*WITHSTATEMENT*) ;

      BEGIN (*STATEMENT*)
        IF SY = INTCONST THEN (*LABEL*)
#         BEGIN  TTOP := TOP ;
#         WHILE DISPLAY[TTOP].OCCUR <> BLCK DO  TTOP := TTOP-1 ;
#         LLP := DISPLAY[TTOP].FLABEL;
            WHILE LLP <> NIL DO
              WITH LLP@ DO
                IF LABVAL = VAL.IVAL THEN
                  BEGIN IF DEFINED THEN ERROR(165);
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
              BEGINSY:  BEGIN INSYMBOL; COMPOUNDSTATEMENT END;
              GOTOSY:   BEGIN INSYMBOL; GOTOSTATEMENT END;
              IFSY:     BEGIN INSYMBOL; IFSTATEMENT END;
              CASESY:   BEGIN INSYMBOL; CASESTATEMENT END;
              WHILESY:  BEGIN INSYMBOL; WHILESTATEMENT END;
              REPEATSY: BEGIN INSYMBOL; REPEATSTATEMENT END;
              FORSY:    BEGIN INSYMBOL; FORSTATEMENT END;
              WITHSY:   BEGIN INSYMBOL; WITHSTATEMENT END
            END;
            IF NOT (SY IN [SEMICOLON,ENDSY,ELSESY,UNTILSY]) THEN
              BEGIN ERROR(6); SKIP(FSYS) END
          END
      END (*STATEMENT*) ;


    BEGIN (*BODY*)
#     IF FPROCP <> NIL THEN
#       BEGIN  ENTNAME := FPROCP@.PFNAME ;  PROCNAME := FPROCP@.NAME ; END
#     ELSE  BEGIN  ENTNAME := 0 ;  PROCNAME := '$MAINBLK    ' ;
            IF XLINK THEN PROCNAME[1] := '#' ;
            END ;
#     NAMFLAG := XLINK ;
#     GENCUPENT(32(*ENT*),PROCTYPE(FPROCP),SEGSIZE,ENTNAME,PROCNAME) ;
#     STIC := 0 ;     (* LENGTH OF STRING CONSTANTS *)
#
(*+CG NEW(CALL_HEAD) ;                                                        *)
(*+CG CALL_HEAD@.NAME := BLANKID ; CALL_HEAD@.NXT := NIL ;                    *)
(*+CG MODIFYING := FALSE ;                                                    *)
#     LOCAL_CALL := FALSE ;
#     VAR_REF := 0 ;  VAR_MOD := 0 ;
#     WRITELN(QRR, '#BGN    ', PROCNAME, LEVEL:4) ;
#
#
#     IF FPROCP = NIL THEN  (* ENTERING MAIN BLOCK *)
        BEGIN
        WHILE FILEHEAD <> NIL DO
          BEGIN
            WITH FILEHEAD@ DO
               BEGIN """ID := FILENAME;
               PRTERR := FALSE ;  SEARCHID([VARS],LLCP); PRTERR := TRUE ;
               IF LLCP <> NIL THEN
                 IF LLCP@.IDTYPE@.FORM <> FILES THEN
                   LLCP := NIL;
#              IF LLCP = NIL THEN
#                BEGIN
#                  WRITELN('**** UNDECLARED EXTERNAL FILE:':40, ID:10);
#                  ERROR(398) ;
#                END
#              ELSE (* OPEN THE FILES REQUESTED ABOVE *)   """

#                WITH FILIDPTR@ DO
#                  BEGIN
#                " IF GEBCDF THEN GEN2(50(*LDA*),1,VADDR+1000)
#                  ELSE"
#                  GEN2(50(*LDA*),1,VADDR) ;
#                  GEN1(30(*CSP*),31(*SIO*)) ;
#
"CM"               IF VADDR >= FIRSTUSERF THEN  (* USER DEFINED FILES *)
"CM"                 BEGIN
"CM"                 NEW(CNSTPTR, STRG) ;
"CM"                 CNSTPTR@.SLNGTH := 8 "IDLNGTH" ;
"CM"                 FOR I := 1 TO 8 "IDLNGTH" DO CNSTPTR@.SVAL[I] := NAME[I] ;
"CM"                 GEN1(37(*LCA*), 0) ;
"CM"                 GEN1(30(*CSP*), 30(*FDF*)) ;
"CM"                 END
"CM"               ELSE  (* I.E. IF VADDR < FIRSTUSERF *)
#                    IF ODD(VADDR) THEN  GEN1(30(*CSP*),4(*REW*))
#                    ELSE  GEN1(30(*CSP*),3(*RES*)) ;
#
#                  GEN1(30(*CSP*),32(*EIO*)) ;
#                  END ;
               END;
          FILEHEAD := FILEHEAD@.NEXTFILE
          END;
"CT"    IF CTROPTION THEN
"CT"      BEGIN
"CT"      GENLABEL(CTRCNTLBL) ;   GENUJPFJP(38(*CTS*), CTRCNTLBL) ;
"CT"      END ;
        END (* PROCESSING MAIN BLOCK *)
      ELSE (* FPROCP <> NIL ==> COPY MULTIPLE VALUES INTO LOACAL CELLS*)
#       BEGIN  LLC1 := LCAFTMST ;
#       IF FPROCP@.SAVEFP THEN LLC1 := LCAFTMST+FPSAVEAREA ;
          LCP := FPROCP@.NEXT;
          WHILE LCP <> NIL DO
            WITH LCP@ DO
              BEGIN
                IF KLASS = VARS THEN
                  IF IDTYPE <> NIL THEN
#                   IF VKIND = FORMAL THEN  (* VAR PARAMETER *)
#                     BEGIN  ALIGN(LLC1,PTRSIZE) ;
#                     LLC1 := LLC1+PTRSIZE ;
#                     END
#                   ELSE  (* VKIND = ACTUAL *)
#                     IF IDTYPE@.FORM > POWER THEN
#                       BEGIN
#                       ALIGN(LLC1,PTRSIZE) ;
#                       GEN2(50(*LDA*),LEVEL,VADDR);
#                       GEN3(54(*LOD*),ORD('A'),LEVEL,LLC1);
#                       GEN1(40(*MOV*),IDTYPE@.SIZE);
#                       LLC1 := LLC1 + PTRSIZE
#                       END
#                     ELSE  (* FORM <= POWER *)
#                       BEGIN
#                       ALIGN(LLC1,IDTYPE@.ALN) ;  LLC1 := LLC1 + IDTYPE@.SIZE ;
#                       END ;
                LCP := LCP@.NEXT;
              END;
        END;
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
      LLP := DISPLAY[TOP].FLABEL; (*TEST FOR UNDEFINED LABELS*)

      WHILE LLP <> NIL DO
        WITH LLP@ DO
          BEGIN
            IF NOT DEFINED THEN
              BEGIN
#               WRITELN(OUTPUT,'**** UNDEF. LABEL:':23,LABVAL); ERROR(168) ;
              END;
            LLP := NEXTLAB
          END;

"CT"  CTREMIT(CTRPROC, CTRNO, FIRSTLN, 0, LINECNT);
"CT"  IF FPROCP = NIL THEN            (* RESET COUNTERS *)
"CT"    BEGIN
"CT"    CTREMIT(CTRPROC, 0, 0, 0, 0); (* EOF FOR COUNTER TABLE *)
"CT"    IF ODD(CTRCNT) THEN  CTRCNT := CTRCNT+1 ;
"CT"    IF CTROPTION THEN  GENDEF(CTRCNTLBL, CTRCNT) ;
"CT"    END ;

#     GEN1(42(*RET*),PROCTYPE(FPROCP)); ALIGN(LCMAX,MXDATASZE) ;
#     IF PRCODE THEN
        BEGIN  GENDEF(SEGSIZE,LCMAX) ;
        IF FPROCP = NIL THEN  GEN0(43(*STP*) ) ;
        END ;
       "IF (FPROCP = NIL) AND  PRTABLES THEN PRINTTABLES(TRUE) "

#
#   CALL_LVL[LOCAL_CALL] := CALL_LVL[LOCAL_CALL]+1 ;
#   WRITELN(QRR) ;
#   WRITE(QRR, '#PROC   ':8,  PROCNAME:IDLNGTH, '   ', LOCAL_CALL:1,
#              IC+(STIC DIV 4):6, LCMAX:8, ' ', FLIPDEBUG:1,
#              '  REF./MOD. RATIO:', VAR_MOD:4, VAR_MOD+VAR_REF:6) ;
#   IF (VAR_MOD+VAR_REF) = 0 THEN  WRITELN(QRR,0.0:10)
#   ELSE  WRITELN(QRR, VAR_MOD/(VAR_MOD+VAR_REF):10) ;
(*+CG WHILE CALL_HEAD@.NXT <> NIL DO                                          *)
(*+CG   BEGIN                                                                 *)
(*+CG   WRITE(QRR, ' ', CALL_HEAD@.NAME, CALL_HEAD@.LVL:3, CALL_HEAD@.CNT: 4);*)
(*+CG   CALL_HEAD :=  CALL_HEAD@.NXT ;                                        *)
(*+CG   END ;                                                                 *)
#   WRITELN(QRR) ;  WRITELN(QRR, '#END') ;
#
#   OLDIC := OLDIC+ IC ;  IC := OLDIC ;  (* DISPLAY CUMULATIVE IC  *)
#   HP := TRUE;
    END (*BODY*) ;

"S1" "                                                                         "
"S1" "  PROCEDURE MKNAME(VAR ALB: ALPHA; NLB: INTEGER ) ;                      "
"S1" "    VAR I, J: INTEGER ;                                                  "
"S1" "                                                                         "
"S1" "  BEGIN                                                                  "
"S1" "    I := 1 ;                                                             "
"S1" "    WHILE (I < 6) AND (ALB[I] <> ' ') DO                                 "
"S1" "      BEGIN  IF ALB[I] = '_' THEN  ALB[I] := '$' ;  I := I+1  END ;      "
"S1" "    FOR J := 8 DOWNTO I DO                                               "
"S1" "      BEGIN                                                              "
"S1" "      ALB[J] := CHR( ORD('0')+ NLB MOD 10 ) ;                            "
"S1" "      NLB := NLB DIV 10 ;                                                "
"S1" "      END ;                                                              "
"S1" "  END (*MKNAME*) ;                                                       "


  BEGIN (*BLOCK*)
    "DP := TRUE;"  IC := 0;  GENLABEL(SEGSIZE) ;
    REPEAT
      IF SY = LABELSY THEN
        BEGIN INSYMBOL; LABELDECLARATION END;
      IF SY = CONSTSY THEN
        BEGIN INSYMBOL; CONSTDECLARATION END;
      IF SY = TYPESY THEN
        BEGIN   INSYMBOL; TYPEDECLARATION;  END;
      IF SY = VARSY THEN
        BEGIN  DP := TRUE;  INSYMBOL;  VARDECLARATION;  DP := FALSE;   END;
"S1" "                                                                         "
"S1" "  WRITE(PRR, ' SST ', CHR( PROCTYPE(FPROCP) ):1, '  ') ;                 "
"S1" "  IF FPROCP = NIL THEN                                                   "
"S1" "    WRITELN(PRR, '$MAINBLK', 1:3, 0:4, 0:4, LC-LASTFILBUF:8, 0:4)        "
"S1" "  ELSE                                                                   "
"S1" "    WITH FPROCP@ DO                                                      "
"S1" "      BEGIN  ID := NAME ;  MKNAME(ID, PFNAME) ;  ALIGN(LC,MXDATASZE) ;   "
"S1" "      WRITELN(PRR, ID:8, PFLEV+1:3, FPRMSZE:4, SPRMSZE:4,                "
"S1" "                   LC-LCAFTMST-FPRMSZE-SPRMSZE:8, RPRMSZE:4) ;           "
"S1" "      END ;                                                              "
"S1" "                                                                         "
      WHILE SY IN [PROCSY,FUNCSY] DO
        BEGIN LSY := SY; INSYMBOL; PROCDECLARATION(LSY) END;
      IF SY <> BEGINSY THEN
        BEGIN ERROR(18); SKIP(FSYS) END
    UNTIL SY IN STATBEGSYS;
    " DP := FALSE;  IC := 0; "(*RESET PROGRAM COUNTER*)
    IF SY = BEGINSY THEN INSYMBOL ELSE ERROR(17);
    REPEAT BODY(FSYS + [CASESY]);
      IF SY <> FSY THEN
        BEGIN ERROR(6); SKIP(FSYS + [FSY]) END
    UNTIL (SY = FSY) OR (SY IN BLOCKBEGSYS);
#  "DP := TRUE;"
  END (*BLOCK*) ;


  PROCEDURE PROGRAMME(FSYS:SETOFSYS);
#   VAR   LFPTR: FRECPTR ;   LCP : CTP ;
  BEGIN
#   (*REWRITE(QRR) ;         USED FOR P_TBL, SYM_TBL, CTR_TBL *)
#   (*REWRITE(PRR) ;         USED FOR P_CODE OUTPUT *)
#   CALL_LVL[FALSE] := 0 ;  CALL_LVL[TRUE] := 0 ;
    IF SY = PROGSY THEN
#     BEGIN INSYMBOL; IF SY <> IDENT THEN ERROR(2); PROGNAME := ID ; INSYMBOL;
        IF NOT (SY IN [LPARENT,SEMICOLON]) THEN ERROR(14);
        IF SY = LPARENT  THEN
          BEGIN
            REPEAT INSYMBOL;
              IF SY = IDENT THEN
#               BEGIN  SEARCHID([VARS], LCP) ;
#               IF LCP@.IDTYPE <> TEXTPTR THEN  ERROR(103)
#               ELSE
#                 BEGIN  NEW(LFPTR);
#                 WITH LFPTR@ DO
#                   BEGIN  FILIDPTR := LCP ;  NEXTFILE := FILEHEAD ;
#                 " GEBCDF := EBCDFLG ; EBCDFLG := FALSE "
#                   END;
#                 FILEHEAD := LFPTR;
#                 END ;
                INSYMBOL;
                IF NOT ( SY IN [COMMA,RPARENT] ) THEN ERROR(20)
                END
              ELSE ERROR(2)
            UNTIL SY <> COMMA;
            IF SY <> RPARENT THEN ERROR(4);
            INSYMBOL
          END;
        IF SY <> SEMICOLON THEN ERROR(14)
        ELSE INSYMBOL;
      END;

"E" IF DEBUG THEN
"E"     BEGIN   "REWRITE(QRR) ;"
"E"     WRITELN(QRR,'% $MAINBLK  0');
"E"     END ;

    REPEAT BLOCK(FSYS,PERIOD,NIL);
      IF SY <> PERIOD THEN ERROR(21)
    UNTIL SY = PERIOD ;
#   WRITELN(QRR,'#HLT  CALL_RATIO', CALL_LVL[TRUE]:4, CALL_LVL[FALSE]:4,
#                  CALL_LVL[TRUE]+CALL_LVL[FALSE]:4) ;
#   IF ERRINX > 0 THEN  PRINTERROR ;
  END (*PROGRAMME*) ;


  PROCEDURE STDNAMES;
  BEGIN
    NA[ 1] := 'FALSE       '; NA[ 2] := 'TRUE        ';
    NA[ 4] := 'PAGE        '; NA[ 5] := 'GET         '; NA[ 6]:= 'PUT         ';
    NA[ 7] := 'RESET       '; NA[ 8] := 'REWRITE     '; NA[ 9]:= 'READ        ';
    NA[10] := 'WRITE       '; NA[11] := 'PACK        '; NA[12]:= 'UNPACK      ';
    NA[13] := 'NEW         '; NA[14] := 'RELEASE     '; NA[15]:= 'READLN      ';
    NA[16] := 'WRITELN     '; NA[17] := 'MARK        '; NA[18]:= 'TRAP        ';
    NA[19] := 'EXIT        ';
    NA[20] := 'ABS         '; NA[21] := 'SQR         '; NA[22]:= 'TRUNC       ';
    NA[23] := 'ROUND       '; NA[24] := 'ORD         '; NA[25]:= 'CHR         ';
    NA[26] := 'PRED        '; NA[27] := 'SUCC        '; NA[28]:= 'CLOCK       ';
    NA[29] := 'EOF         '; NA[30] := 'EOLN        ';
    NA[31] := 'SIN         '; NA[32] := 'COS         '; NA[33]:= 'EXP         ';
    NA[34] := 'SQRT        '; NA[35] := 'LN          '; NA[36]:= 'ARCTAN      ';
    NA[37] := 'ODD         ';
#   NA[39] := 'INPUT       '; NA[40] := 'OUTPUT      '; NA[41]:= 'PRD         ';
#   NA[42] := 'PRR         '; NA[43] := 'QRD         '; NA[44]:= 'QRR         ';
#   NA[45] := 'DATE        '; NA[46] := 'TIME        ';
"NH"NA[47] := 'MESSAGE     '; NA[48] := 'SKIP        ';
"NH"NA[49] := 'LINELIMIT   '; NA[50] := 'CARD        ';
"NH"NA[51] := 'EXPO        ';
#   NA[52] := 'DSIN        '; NA[53] := 'DCOS        '; NA[54]:= 'DEXP        ';
#   NA[55] := 'DSQRT       '; NA[56] := 'DLOG        '; NA[57]:= 'DATAN       ';
  END (*STDNAMES*) ;


  PROCEDURE ENTERSTDTYPES;
    VAR SP: STP;
  BEGIN                                                  (*TYPE UNDERLIEING:  *)
                                                         (******************  *)

    NEW(INTPTR,SCALAR,STANDARD);                              (*INTEGER       *)
    WITH INTPTR@ DO
      BEGIN SIZE := INTSIZE; ALN := INTSIZE ;
            FORM := SCALAR; SCALKIND := STANDARD END;
    NEW(REALPTR,SCALAR,STANDARD);                             (*REAL          *)
    WITH REALPTR@ DO
      BEGIN SIZE := REALSIZE; ALN := MXDATASZE ;
            FORM := SCALAR; SCALKIND := STANDARD END;
    NEW(CHARPTR,SCALAR,STANDARD);                             (*CHAR          *)
    WITH CHARPTR@ DO
      BEGIN SIZE := CHARSIZE; ALN := CHARSIZE ;
            FORM := SCALAR; SCALKIND := STANDARD END;
    NEW(BOOLPTR,SCALAR,DECLARED);                             (*BOOLEAN       *)
    WITH BOOLPTR@ DO
      BEGIN SIZE := BOOLSIZE; ALN := BOOLSIZE ;
            FORM := SCALAR; SCALKIND := DECLARED END;
    NEW(NILPTR,POINTER);                                      (*NIL           *)
    WITH NILPTR@ DO
      BEGIN ELTYPE := NIL; SIZE := PTRSIZE; ALN := PTRSIZE ;
            FORM := POINTER END;
    NEW(TEXTPTR,FILES);                                       (*TEXT          *)
    WITH TEXTPTR@ DO
      BEGIN FILTYPE := CHARPTR; SIZE := CHARSIZE; ALN := CHARSIZE ;
            FORM := FILES  END ;
    NEW(ALFAPTR,ARRAYS);                                      (*ALFA          *)
    WITH ALFAPTR@ DO
      BEGIN  AELTYPE := CHARPTR; SIZE := ALFALNGTH ; ALN := CHARSIZE ;
            FORM := ARRAYS ;
            NEW(INXTYPE,SUBRANGE) ;
            INXTYPE@.FORM := SUBRANGE ;  INXTYPE@.RANGETYPE := INTPTR;
            INXTYPE@.MIN.IVAL := 1; INXTYPE@.MAX.IVAL := 10;
            (* OTHER FIELDS ARE IRRELEVENT !!! *)
      END ;
  END (*ENTERSTDTYPES*) ;

  PROCEDURE ENTSTDNAMES;
    VAR CP,CP1: CTP; I: INTEGER;
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
            VADDR := FIRSTFILBUF+(I-39)*CHARSIZE  ;  "EBCD := FALSE ;"
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
#           FORM := RECORDS;  RECVAR := NIL;
#
#           NEW(FSTFLD, FIELD);
#             WITH FSTFLD@ DO
#             BEGIN   NAME := 'LENGTH      ';  IDTYPE := INTPTR;   FLDADDR := 0;
#             KLASS := FIELD;
#
#             NEW(RLINK, FIELD);    (*'LENGTH' < 'STRING' --> GOING DOWN RLINK*)
#             WITH RLINK@ DO
#               BEGIN   NAME := 'STRING      ';  LLINK := NIL;  RLINK := NIL;
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
#               END (*WITH LLINK...*);
#
#             LLINK := NIL;  NEXT := RLINK;
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
        VKIND := ACTUAL; NEXT := NIL; VLEV := 1; VADDR := 0
      END;

#   FOR I := 4 TO 37 DO  (*PAGE,GET...ARCTAN,ODD*)
#     BEGIN NEW(CP,PROC,STANDARD);                         (*GET,PUT,RESET    *)
#       WITH CP@ DO                                        (*REWRITE,READ     *)
#         BEGIN NAME := NA[I]; IDTYPE := NIL;              (*WRITE,PACK       *)
#           NEXT := NIL; KEY := I - 4;                     (*UNPACK,PACK      *)
#           IF I <= 19 THEN KLASS := PROC ELSE KLASS := FUNC  ;
#           PFDECKIND := STANDARD;                         (*READLN,WRITELN   *)
#                                                          (*MARK,RELEASE,TRAP*)

            IF I IN [31..36 (*SIN..ARCTAN*)] THEN  (*ADD THE FORTAN SHADOW TOO*)
              BEGIN NEW(NEXT,FUNC,DECLARED,ACTUAL);     (*SIN,COS,EXP,SQRT    *)
                WITH NEXT@ DO                           (*LN,ARCTAN,EXIT      *)
                  BEGIN  NAME := NA[I-31+52]; IDTYPE := REALPTR; NEXT := CP1;
                  FWDECL := FALSE; EXTRN := FALSE; FRTRN := TRUE;
                  KLASS := FUNC; PFDECKIND := DECLARED; PFKIND := ACTUAL;
                  PFLEV := 0; PFNAME := 0;
                  END;
                ENTERID(NEXT)
              END (*IF I IN [31..36]...*);

#         END;
#       ENTERID(CP)
#     END;

"NH"FOR I := 47 TO 51 DO
"NH"  BEGIN NEW(CP,PROC,STANDARD);
"NH"    WITH CP@ DO
"NH"       BEGIN  NAME := NA[I];  IDTYPE := NIL; NEXT := NIL;
"NH"         IF I <= 49 THEN KLASS := PROC ELSE KLASS := FUNC;
"NH"         KEY := I-12;  PFDECKIND := STANDARD;
"NH"       END;
"NH"    ENTERID(CP);
"NH"  END;

#   NEW(CP,PROC,DECLARED,ACTUAL);                          (*SNAPSHOT         *)
#     WITH CP@ DO
#       BEGIN   NAME := 'SNAPSHOT    ';  IDTYPE := NIL;  FRTRN := FALSE ;
#         FWDECL := FALSE ;  EXTRN := TRUE ; PFLEV := 0 ; PFNAME := 0 ;
#         KLASS := PROC; PFDECKIND := DECLARED ;   PFKIND := ACTUAL ;
#         SAVEFP := FALSE ;
#       END;
#     ENTERID(CP) ;
#
#   NEW(CP@.NEXT,VARS);               (*PARAMETER OF PREDECLARED EXTERNAL PROC*)
#   WITH CP@.NEXT@ DO
#     BEGIN  IDTYPE := INTPTR; KLASS := VARS;
#       VKIND := ACTUAL; NEXT := NIL; VLEV := 1; VADDR := 0 ;
#     END;
#   NEW(CP1,VARS) ;
#   CP1@ := CP@.NEXT@ ;                 (*SECOND PARAMETER FOR SNAPSHOT       *)
#   CP@.NEXT@.NEXT := CP1 ;


 """FOR I := 20 TO 30 DO
      BEGIN NEW(CP,FUNC,STANDARD);                         (*ABS,SQR,TRUNC    *)
        WITH CP@ DO                                        (*ODD,ORD,CHR      *)
          BEGIN NAME := NA[I]; IDTYPE := NIL;              (*PRED,SUCC        *)
            NEXT := NIL; KEY := I - 19;                    (*CLOCK,EOF,EOLN   *)
            KLASS := FUNC; PFDECKIND := STANDARD
          END;
        ENTERID(CP)
      END;
    NEW(CP,VARS);                      (*PARAMETER OF PREDECLARED FUNCTIONS   *)
    WITH CP@ DO
      BEGIN NAME := BLANKID; IDTYPE := REALPTR; KLASS := VARS;
        VKIND := ACTUAL; NEXT := NIL; VLEV := 1; VADDR := 0
      END;
    FOR I := 31 TO 37 DO
      BEGIN NEW(CP1,FUNC,DECLARED,ACTUAL);              (*SIN,COS,EXP,SQRT    *)
        WITH CP1@ DO                                    (*LN,ARCTAN,EXIT      *)
          BEGIN NAME := NA[I]; IDTYPE := REALPTR; NEXT := CP;
            FWDECL := FALSE; EXTRN := TRUE; PFLEV := 0; PFNAME := I - 16;
            KLASS := FUNC; PFDECKIND := DECLARED; PFKIND := ACTUAL
          END;
        ENTERID(CP1)
      END;
    WITH CP1@ DO                               (*FIXUPS FOR EXIT PROCEDURE    *)
      BEGIN  IDTYPE := NIL;  NEXT := CP;  KLASS := PROC   END;
    NEW(CP,VARS);                              (*PARAMETER OF EXIT ROUTINE    *)
    WITH CP@ DO
      BEGIN NAME := BLANKID; IDTYPE := INTPTR; KLASS := VARS;
        VKIND := ACTUAL; NEXT := NIL; VLEV := 1; VADDR := 0
      END;"""

  END (*ENTSTDNAMES*) ;


  PROCEDURE ENTERUNDECL;
  BEGIN
    NEW(UTYPPTR,TYPES);
    WITH UTYPPTR@ DO
      BEGIN NAME := BLANKID; IDTYPE := NIL; KLASS := TYPES END;
    NEW(UCSTPTR,KONST);
    WITH UCSTPTR@ DO
      BEGIN NAME := BLANKID; IDTYPE := NIL; NEXT := NIL;
        VALUES.IVAL := 0; KLASS := KONST
      END;
    NEW(UVARPTR,VARS);
    WITH UVARPTR@ DO
      BEGIN NAME := BLANKID; IDTYPE := NIL; VKIND := ACTUAL;
        NEXT := NIL; VLEV := 0; VADDR := 0; KLASS := VARS
      END;
    NEW(UFLDPTR,FIELD);
    WITH UFLDPTR@ DO
      BEGIN NAME := BLANKID; IDTYPE := NIL; NEXT := NIL; FLDADDR := 0;
        KLASS := FIELD
      END;
    NEW(UPRCPTR,PROC,DECLARED,ACTUAL);
    WITH UPRCPTR@ DO
      BEGIN NAME := BLANKID; IDTYPE := NIL; FWDECL := FALSE;
        NEXT := NIL; EXTRN := FALSE; PFLEV := 0; GENLABEL(PFNAME);
        KLASS := PROC; PFDECKIND := DECLARED; PFKIND := ACTUAL
      END;
    NEW(UFCTPTR,FUNC,DECLARED,ACTUAL);
    WITH UFCTPTR@ DO
      BEGIN NAME := BLANKID; IDTYPE := NIL; NEXT := NIL;
        FWDECL := FALSE; EXTRN := FALSE; PFLEV := 0; GENLABEL(PFNAME);
        KLASS := FUNC; PFDECKIND := DECLARED; PFKIND := ACTUAL
      END
  END (*ENTERUNDECL*) ;


  PROCEDURE INITSCALARS;
  BEGIN   FWPTR := NIL;
#     "PRTABLES  := FALSE;"   LIST   := TRUE;     PRCODE    := TRUE;
#      DP        := FALSE;    PRTERR := TRUE;     ERRINX    := 0;
#      HP        := FALSE;    IC     := 0;
#      INTLABEL  := 0;
#      FILEHEAD  := NIL;
#      LC        := FIRSTGVAR;             (*ADR. OF THE FIRST GLOBAL VARIABLE*)
#      (* NOTE IN THE ABOVE RESERVATION OF BUFFER STORE FOR TEXT FILES *)
#      OLDIC     := 0;      IC       := 0 ;       EOL       := TRUE;
#      LINECNT   := 0;      CH       := ' ';      CHCNT     := 0;
#      PAGECNT   := 0;      NXTLN    := PAGESIZE; (* GENERATES FIRST HEADLINE *)
#      LMARGIN   := 0;      RMARGIN  := 80;       BUFEND    := 81;
#      OLDLN     := 0;      MWARN    := FALSE;    LSTOP     := '#';
#      GLOBTESTP := NIL;
#      PROGNAME  := '$MAINBLK    ' ;
#      BLANKID   := '            ' ;
#      MXINT10   := MAXINT DIV 10;               "DIGMAX    := REALLNGTH - 1;"
#      PROCLAB   := 0;      ERRORCNT := 0;
#      ASSEMBLE  := FALSE; "MARGIN   := FALSE;"   NESTCOMM  := FALSE ;
#      SAVEREGS  := TRUE ;  SAVEFPRS := TRUE;    "EBCDFLG   := FALSE ;"
#      DEBUG     := TRUE ; "BYTEON   :=  FALSE ; "ASSIGN    := FALSE  ;
#      FLIPDEBUG := FALSE ;
#      DOTFLG    := FALSE ; NXTFILBUF:= FIRSTUSERF ;
#      PACKDATA  := FALSE ; XLINK    := FALSE ;   (*GENERATES UNIQUE NAMES *)
"CM"   MXDATASZE := REALSIZE ;
"S1" " MXDATASZE := PTRSIZE ;                                                  "
#      GET_STAT  := FALSE ; ASMVERB  := FALSE ;
"CT"   CTRCNT    := 0 ;     CTROPTION:= FALSE ;
  END (*INITSCALARS*) ;


  PROCEDURE INITSETS;
  VAR  I : 0..10 (*MAXERRNR DIV SETMAX*) ;
  BEGIN
    CONSTBEGSYS    := [ADDOP,INTCONST,REALCONST,STRINGCONST,IDENT];
    SIMPTYPEBEGSYS := [LPARENT] + CONSTBEGSYS;
    TYPEBEGSYS     := [ARROW,PACKEDSY,ARRAYSY,RECORDSY,SETSY,FILESY]+
                      SIMPTYPEBEGSYS;
    TYPEDELS       := [ARRAYSY,RECORDSY,SETSY,FILESY];
    BLOCKBEGSYS    := [LABELSY,CONSTSY,TYPESY,VARSY,PROCSY,FUNCSY,
                       BEGINSY];
    SELECTSYS      := [ARROW,PERIOD,LBRACK];
    FACBEGSYS      := [INTCONST,REALCONST,STRINGCONST,IDENT,LPARENT,LBRACK,
                       NOTSY];
    STATBEGSYS     := [BEGINSY,GOTOSY,IFSY,WHILESY,REPEATSY,FORSY,WITHSY,
                       CASESY];
#
#   FOR I := 0 TO MAXERRLOG DO  ERRLOG[I] := [] ;   (*CLEAR ERROR LOG*)
#
  END (*INITSETS*) ;


  PROCEDURE INITTABLES;
    PROCEDURE RESWORDS;
    BEGIN
      RW[ 1]:= 'IF          '; RW[ 2]:= 'DO          '; RW[ 3]:= 'OF          ';
      RW[ 4]:= 'TO          '; RW[ 5]:= 'IN          '; RW[ 6]:= 'OR          ';
      RW[ 7]:= 'END         '; RW[ 8]:= 'FOR         '; RW[ 9]:= 'VAR         ';
      RW[10]:= 'DIV         '; RW[11]:= 'MOD         '; RW[12]:= 'SET         ';
      RW[13]:= 'AND         '; RW[14]:= 'NOT         '; RW[15]:= 'THEN        ';
      RW[16]:= 'ELSE        '; RW[17]:= 'WITH        '; RW[18]:= 'GOTO        ';
      RW[19]:= 'CASE        '; RW[20]:= 'TYPE        ';
      RW[21]:= 'FILE        '; RW[22]:= 'BEGIN       ';
      RW[23]:= 'UNTIL       '; RW[24]:= 'WHILE       '; RW[25]:= 'ARRAY       ';
      RW[26]:= 'CONST       '; RW[27]:= 'LABEL       ';
      RW[28]:= 'REPEAT      '; RW[29]:= 'RECORD      '; RW[30]:= 'DOWNTO      ';
      RW[31]:= 'PACKED      '; RW[32]:= 'FORWARD     '; RW[33]:= 'PROGRAM     ';
      RW[34]:= 'FORTRAN     '; RW[35]:= 'EXTERNAL    ';
      RW[36]:= 'FUNCTION    '; RW[37]:= 'PROCEDURE   ';

      FRW[1] :=  1; FRW[2] :=  1; FRW[3] :=  7; FRW[4] := 15; FRW[5] := 22;
      FRW[6] := 28; FRW[7] := 32; FRW[8] := 35; FRW[9] := 37;
#     FRW[10] := 38 ; FRW[11] := 38;  FRW[12] := 38; FRW[13] := 38 ;

#    "SEQFLD[9] := ' ';  SEQFLD[10] := ' '; "  (*CLEAR EXTRA CHARS IN SEQ. FLD*)
    END (*RESWORDS*) ;

    PROCEDURE SYMBOLS;
    BEGIN
      RSY[1] := IFSY; RSY[2] := DOSY; RSY[3] := OFSY; RSY[4] := TOSY;
      RSY[5] := RELOP; RSY[6] := ADDOP; RSY[7] := ENDSY; RSY[8] := FORSY;
      RSY[9] := VARSY; RSY[10] := MULOP; RSY[11] := MULOP; RSY[12] := SETSY;
      RSY[13] := MULOP; RSY[14] := NOTSY; RSY[15] := THENSY;
      RSY[16] := ELSESY; RSY[17] := WITHSY; RSY[18] := GOTOSY;
      RSY[19] := CASESY; RSY[20] := TYPESY; RSY[21] := FILESY;
      RSY[22] := BEGINSY; RSY[23] := UNTILSY; RSY[24] := WHILESY;
      RSY[25] := ARRAYSY; RSY[26] := CONSTSY; RSY[27] := LABELSY;
      RSY[28] := REPEATSY; RSY[29] := RECORDSY; RSY[30] := DOWNTOSY;
      RSY[31] := PACKEDSY; RSY[32] := FORWARDSY; RSY[33] := PROGSY;
      RSY[34] := FRTRNSY ; RSY[35] := EXTRNSY ;  RSY[36] := FUNCSY;
      RSY[37] := PROCSY;

      SSY['+'] := ADDOP;    SSY['-'] := ADDOP;
      SSY['*'] := MULOP;    SSY['/'] := MULOP;
      SSY['('] := LPARENT;  SSY[')'] := RPARENT;
      SSY['['] := LBRACK;   SSY[']'] := RBRACK;
      SSY[','] := COMMA;    SSY[':'] := COLON;
      SSY['×'] := ADDOP ;   SSY['&'] := MULOP ;
      SSY['<'] := RELOP;    SSY['>'] := RELOP;
      SSY['='] := RELOP;    SSY['@'] := ARROW;
      SSY['Ö'] := ARROW;
      SSY['^'] := NOTSY ;   SSY[';'] := SEMICOLON;
      SSY['.'] := PERIOD;
    END (*SYMBOLS*) ;


    PROCEDURE RATORS;
      VAR I: INTEGER; CH: CHAR;
    BEGIN
      FOR I := 1 TO NRSW (*NR OF RES WORDS*) DO ROP[I] := NOOP;
      ROP[5] := INOP; ROP[10] := IDIV; ROP[11] := IMOD;
      ROP[6] := OROP; ROP[13] := ANDOP;

      (* THE LIMIT OF THESE LOOP IS CHAR SET DEPENDENT *)
#     FOR CH := chr(0) TO chr(255) DO    UPSHIFT[CH] := CH ;         (*opp*)
#     FOR CH := 'a' TO 'z' DO    UPSHIFT[CH] := CHR(ORD(CH) + 64) ;      (*UPL*)
      FOR I := 0 TO ORDCHMAX DO  SOP[CHR(I)] := ILLEGCH ;
      FOR CH := 'A' TO 'I' DO    SOP[CH] := ATOZCH ;
      FOR CH := 'J' TO 'R' DO    SOP[CH] := ATOZCH ;
      FOR CH := 'S' TO 'Z' DO    SOP[CH] := ATOZCH ;
      FOR CH := 'a' TO 'i' DO
         BEGIN  SOP[CH] := ATOZCH; UPSHIFT[CH] := CHR(ORD(CH) + 64)  END;(*UPL*)
      FOR CH := 'j' TO 'r' DO
         BEGIN  SOP[CH] := ATOZCH; UPSHIFT[CH] := CHR(ORD(CH) + 64)  END;(*UPL*)
      FOR CH := 's' TO 'z' DO
         BEGIN  SOP[CH] := ATOZCH; UPSHIFT[CH] := CHR(ORD(CH) + 64)  END;(*UPL*)
      FOR CH := '0' TO '9' DO    SOP[CH] := NUMCH ;
      SOP['"']  := DQUOTCH ;
      SOP['¯']  := LBRACE ;
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
      SOP['×']  := SPECH ;
      SOP[']']  := SPECH ;
      SOP['^']  := SPECH ;
      SOP['_']  := UNDSCH ;
      SOP['+'] := PLUS; SOP['-'] := MINUS;
      SOP['*'] := MUL;  SOP['/'] := RDIV;
      SOP['='] := EQOP;
      SOP['<'] := LTOP; SOP['>'] := GTOP;
#     SOP['×'] := OROP ;  SOP['&'] := ANDOP ;
    END (*RATORS*) ;

    PROCEDURE PROCMNEMONICS;
    BEGIN
      SNA[ 0] :='PAG';
      SNA[ 1] :='GET'; SNA[ 2] :='PUT'; SNA[ 3] :='RES'; SNA[ 4] :='REW';
      SNA[ 5] :='RDC'; SNA[ 6] :='WRI'; SNA[ 7] :='WRO'; SNA[ 8] :='WRR';
      SNA[ 9] :='WRC'; SNA[10] :='WRS'; SNA[11] :='PAK'; SNA[12] :='RDB';
      SNA[13] :='WRB'; SNA[14] :='RDR'; SNA[15] :='SIN'; SNA[16] :='COS';
      SNA[17] :='EXP'; SNA[18] :='SQT'; SNA[19] :='LOG'; SNA[20] :='ATN';
      SNA[21] :='CLK'; SNA[22] :='WLN'; SNA[23] :='RLN'; SNA[24] :='RDI';
      SNA[25] :='EOF'; SNA[26] :='ELN'; SNA[27] :='RDS'; SNA[28] :='TRP';
      SNA[29] :='XIT'; SNA[30] :='FDF'; SNA[31] :='SIO'; SNA[32] :='EIO';
"NH"  SNA[33] := 'MSG'; SNA[34] := 'SKP'; SNA[35] := 'LIM';
    END (*PROCMNEMONICS*) ;

    PROCEDURE INSTRMNEMONICS;
    BEGIN
      MN[ 0] :=' ABI'; MN[ 1] :=' ABR'; MN[ 2] :=' ADI'; MN[ 3] :=' ADR';
      MN[ 4] :=' AND'; MN[ 5] :=' DIF'; MN[ 6] :=' DVI'; MN[ 7] :=' DVR';
      MN[ 8] :=' SBR'; MN[ 9] :=' FLO'; MN[10] :=' FLT'; MN[11] :=' INN';
      MN[12] :=' INT'; MN[13] :=' IOR'; MN[14] :=' MOD'; MN[15] :=' MPI';
      MN[16] :=' MPR'; MN[17] :=' NGI'; MN[18] :=' NGR'; MN[19] :=' NOT';
      MN[20] :=' ODD'; MN[21] :=' SBI'; MN[22] :=' DEC'; MN[23] :=' INC';
      MN[24] :=' SQI'; MN[25] :=' SQR'; MN[26] :=' STO'; MN[27] :=' TRC';
      MN[28] :=' RND'; MN[29] :=' SGS'; MN[30] :=' CSP'; MN[31] :=' UNI';
      MN[32] :=' ENT'; MN[33] :=' FJP'; MN[34] :='    '; MN[35] :=' IND';
      MN[36] :=' IXA'; MN[37] :=' LCA'; MN[38] :=' CTS'; MN[39] :=' CTI';
      MN[40] :=' MOV'; MN[41] :=' MST'; MN[42] :=' RET'; MN[43] :=' STP';
      MN[44] :=' XJP'; MN[45] :=' CHK'; MN[46] :=' CUP'; MN[47] :=' EQU';
      MN[48] :=' GEQ'; MN[49] :=' GRT'; MN[50] :=' LDA'; MN[51] :=' LDC';
      MN[52] :=' LEQ'; MN[53] :=' LES'; MN[54] :=' LOD'; MN[55] :=' NEQ';
#     MN[56] :=' STR'; MN[57] :=' UJP'; MN[58] :=' NEW'; MN[59] :=' SAV';
#     MN[60] :=' RST'; MN[61] :=' ORD'; MN[62] :=' CHR'; MN[63] :=' DEF';
"S1" "MN[64] :=' PAR';                                                         "
"NH"  MN[65] := ' CRD';  MN[66] := ' XPO';
    END (*INSTRMNEMONICS*) ;

  BEGIN (*INITTABLES*)
    RESWORDS; SYMBOLS; RATORS;
    INSTRMNEMONICS; PROCMNEMONICS;
(*  FOR CH := 'A' TO 'I' DO  EBCDIC[CH] := 192+ORD(CH) ;
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
    EBCDIC['_']  := 109 ;     CH := ' ' ;   *)
  END (*INITTABLES*) ;


BEGIN  (*PASCALCOMPILER*)
  (*INITIALIZE*)
  (************)
  INITSCALARS; INITSETS; INITTABLES;


  (*ENTER STANDARD NAMES AND STANDARD TYPES:*)
  (******************************************)

  LEVEL := 0; TOP := 0;
  WITH DISPLAY[0] DO
    BEGIN FNAME := NIL; FLABEL := NIL; OCCUR := BLCK END;
  ENTERSTDTYPES;   STDNAMES; ENTSTDNAMES;   ENTERUNDECL;
  TOP := 1; LEVEL := 1;
  WITH DISPLAY[1] DO
    BEGIN FNAME := NIL; FLABEL := NIL; OCCUR := BLCK END;

  (*set options passed as parameter to the compiler*)
  (*************************************************)

  IF OSPARM <> NIL THEN
    WITH  OSPARM@  DO
      BEGIN
      CH "LINEBUF[1]" := '¯';  LINEBUF[2] := '$';
      IF LENGTH > 64 THEN LENGTH := 64;
      FOR CHCNT := 1 TO LENGTH DO  LINEBUF[CHCNT+2] := STRING[CHCNT];
      (*THE REST OF THE LINE DOES NOT HAVE TO BE CLEARED BUT...*)
      FOR CHCNT := LENGTH TO 77 DO   LINEBUF[CHCNT+3] := ' ';
      LINEBUF[LENGTH+3] := 'ò';  LINEBUF[LENGTH+4] := '#';
      EOL := FALSE;  CHCNT := 1;  LASTCOL := LENGTH+3;
      END (*WITH OSPARM .., IF OSPARM ...*);

  (*COMPILE:*)
  (**********)

# CTIME := CLOCK(1) ;
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
