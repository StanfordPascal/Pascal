(************************************************************************
 *                                                                      *
 *                                                                      *
 *                                                                      *
 *                                                                      *
 *                                                                      *
 *                  PASCAL 8000 - IBM 360/370 VERSION                   *
 *                  ---------------------------------                   *
 *                                                                      *
 *                         LINKAGE EDITOR VERSION                       *
 *                         ----------------------                       *
 *                                                                      *
 *                                                                      *
 *        VERSION:                     2.0                              *
 *        -------                                                       *
 *                                                                      *
 *        DATE:                        JULY 27, 1980                    *
 *        ----                                                          *
 *                                                                      *
 *        ORIGINAL AUTHORS:            TERUO HIKITA                     *
 *        (HITAC VERSION)              KIYOSHI ISHIHATA                 *
 *        ----------------             (UNIVERSITY OF TOKYO)            *
 *                                                                      *
 *        CURRENT AUTHORS:             JEFFREY TOBIAS                   *
 *        (IBM VERSION)                GORDON COX                       *
 *        ---------------              (AUSTRALIAN ATOMIC ENERGY COMM] *
 *                                                                      *
 *        CMS MODIFICATIONS:           HAL PERKINS                      *
 *        -----------------            (CORNELL UNIVERSITY)             *
 *                                                                      *
 *        IMPLEMENTATION LANGUAGE:     PASCAL 8000/370                  *
 *        -----------------------                                       *
 *                                                                      *
 *        COPYRIGHT (C) 1980, AUSTRALIAN ATOMIC ENERGY COMMISSION       *
 *                                                                      *
 *                                                                      *
 *        DESCRIPTION:                                                  *
 *        -----------                                                   *
 *           THIS IS A PASCAL 8000 COMPILER DESIGNED TO EXECUTE ON      *
 *           AN IBM SERIES 360 OR 370 COMPUTER (OR EQUIVALENT)          *
 *           UNDER THE OPERATING SYSTEMS OS/MFT,OS/MVT,VS1,SYS,MVS,     *
 *           AND CMS.                                                   *
 *                                                                      *
 *           THE CODE PRODUCED BY THIS VERSION OF THE COMPILER CAN      *
 *           BE PASSED THROUGH THE STANDARD IBM LINKAGE EDITOR.         *
 *                                                                      *
 *                                                                      *
 *        DATASETS:             "INPUT" - PROGRAM TO COMPILE            *
 *        --------             "OUTPUT" - PROGRAM LISTING               *
 *                           "$PASMSGS" - ERROR MESSAGE DATASET         *
 *                              "SYSGO" - CODE FILE PRODUCED            *
 *                            "SYSTERM" - TERMINAL ERROR MESSAGE OUTPUT *
 *                                                                      *
 ************************************************************************)
PROGRAM COMPILER(INPUT,OUTPUT,$PASMSGS,SYSGO,SYSTERM);

LABEL 9999;
CONST
  DISPLIMIT = 20;            (* MAX NUMBER OF NESTED SCOPES OF IDENTIFIERS   *)
  MAXLEVEL  = 6;             (* MAX NUMBER OF NESTED PROC/FUNCT              *)
  MAXCHCNT  = 121;           (* MAX NO OF CHARS ON AN INPUT LINE + 1         *)
  MAXCP     = MAXCHCNT;
  DARKNESS  = 2;             (* NUMBER OF TIMES TO OVERPRINT KEYWORDS IF $B+ *)
  MAXPARMLEN= 131;           (* MAX NO. CHARACTERS IN EXEC CARD PARAMETERS   *)
  NOSPCH    = 18;
  NULLID    = '              ';
  EOLCODE   = 0;
  IGNORECHARACTER = '_';
  MAGIC     = 258;
  RESWORDS  = 40;            (* NUMBER OF RESERVED WORDS                *)
  NRTEXTP1  = 13;            (* NO. WXTRNS IN RUNTIME SYS + 1           *)
  NRSTDPROC = 25;            (* NO. STANDARD PROCEDURES                 *)
  NRSTDFUNC = 28;            (* NO. STANDARD FUNCTIONS                  *)
  NRSTDNAMES= 53;            (* NRSTDPROC + NRSTDFUNC                   *)
  ALFALENG  = 8;             (* NO OF SIGNIFICANT CHAR IN AN IDENTIFIER *)
  UNDEFBYTE = 254;           (* VALUE OF "UNDEFINED" BYTE               *)
  ORDCHARMAX= 255;           (* ORDINAL NUMBER OF THE LAST CHARACTER    *)
  SETMIN    = 0;   SETMAX   = 63; (*SMALLEST AND LARGEST ELEMENT OF A SET *)
  NILVAL    = 0;             (* = ORD(NIL)              *)
  MXINT     = 2147483647;    (* LARGEST INTEGER VALUE   *)
  TWO24     = 16777215;      (* LARGEST 24-BIT ADDRESS. *)
  LCSTART   = 1664;          (* INITIAL DATA COUNTER IN MAIN PGM. X'680'    *)
                             (* MUST AGREE WITH VALUE IN R.T. SYS. OPTIONS. *)
  MAXPROCFUNC  = 256;
  MAXPR1       = 257;        (* = MAXPROCFUNC + 1            *)
  CIXMAX       = 256;        (* MAXIMUM NUMBER OF CASE LABEL *)
  TEXTSIZE     = 20;         (* NO. BYTES IN TEXT FILE BLOCK *)
  LINESPERPAGE = 60;
  INDENT       = 20;         (* # COLS IN OUTPUT LISTING TO LEFT OF SOURCE PGM *)
  INDENTTY     = 12;         (* # COLS TO LEFT OF SOURCE PGM IN SYSTERM OUTPUT *)
  MAXMSGSDIV64 = 7;
  VERSION      = '2.0      AAEC  (27JUL80) ';  (* SHOULD BE 25 COLS. LEFT JUST.*)
  ENDCARDID    = ' PASCAL 8000/2.0 (27JUL80)                      ';
                             (* IDENTIFICATION FOR OBJECT DECKS.  48 COLS.*)
                             (* FIRST COLUMN MUST BE BLANK FOR LOADER     *)
  CODEBLCK     = 63;         (* 1024 DIV 16 - 1         *)
  NCODESEGS    = 96;         (* NUMBER OF CODE SEGMENTS *)
  CODEPERSEG   = 256;        (* BYTES PER SEGMENT       *)
  OBJLENGTH    = 14;
  OBJLENPL1    = 15;
  ALLSPACES    = 1077952576; (* = INTEGER('    ')       *)
  RELOC2       = 65536;
  RELOC1       = 16777216;   (* 256 * 65536             *)
  BYTE1SPACE   = 1073741824; (* 256 * 65536 * 56(X'40)  *)
  Z7FE         = 134086656;  (* X'07FE                  *)
  BYTE2SPACE   = 4194304;    (* 65536 * 56(X'40)        *)
  SD = 0;
  ER = 2;

TYPE

  ALFA = PACKED ARRAY(.1..ALFALENG] OF CHAR;
  ALFA14 = PACKED ARRAY(.1..14] OF CHAR;
  LEVRANGE = 0..8; ADDRRANGE = INTEGER;

                             (*BASIC SYMBOLS*)
                             (***************)


  SYMBOL = (IDENT,INTCONST,REALCONST,CHARCONST,STRINGCONST,NOTSY,MULOP,
       ADDOP,RELOP,LPARENT,RPARENT,LBRACK,RBRACK,LCBRACK,RCBRACK,COMMA,SEMICOLON,
       PERIOD,ARROW,COLON,BECOMES,LABELSY,CONSTSY,TYPESY,VARSY,VALUESY,
       FUNCTSY,PROCSY,SETSY,PACKEDSY,ARRAYSY,RECORDSY,FILESY,
       BEGINSY, IFSY,CASESY,REPEATSY,WHILESY,LOOPSY,FORSY,FORALLSY,WITHSY,
       GOTOSY,ENDSY,ELSESY,POSTSY,UNTILSY,OFSY,DOSY,TOSY,DOWNTOSY,
       OTHERWSY,NILSY,LBRACESY,RBRACESY,
       THENSY,PROGRAMSY,EXPONOP,OTHERSY);
  OPERATOR = (MUL,RDIV,ANDOP,IDIV,IMOD,PLUS,MINUS,OROP,LTOP,LEOP,GEOP,
        GTOP,NEOP,EQOP,INOP,NOOP);
  CHRTYP = (DDIGIT,MUSTBEID,LLETTER,SINGLE,DOUBLE,QUOTE,
                    EOL,INVALID,IGNORE);
  IRW = 0..RESWORDS;
  ISPCH = 1..NOSPCH;
  BUFX = 0..MAXCP;
  HASH = -MAGIC .. 0;
  SETOFSYS = SET OF SYMBOL;

                             (*CONSTANTS*)
                             (***********)

  LOCOFREF = @LOCREC;
  LOCREC=RECORD NXTREF:LOCOFREF;
             LOC: ADDRRANGE
           END;

  CSTCLASS = (INT,REEL,PSET,STRG);
  CTAILP = @ CSTTAILREC;
  STRGFRAG=PACKED ARRAY(.1..4] OF 0..255;
  CSTTAILREC = RECORD NXTCSP: CTAILP; STFR : INTEGER END;

  BASICSET=SET OF SETMIN..SETMAX;
  CELLUNIT=1..8;
  VALU=RECORD CASE CKIND:CSTCLASS OF
          INT:  (IVAL: INTEGER);
          REEL: (RVAL: REAL);
          PSET: (PVAL: BASICSET);
          STRG: (VALP: CTAILP)
         END;

                              (*DATA STRUCTURES*)
                              (*****************)

  STRUCTFORM = (SCALAR,PACKDTYPE,SUBRANGE,POINTER,POWER,ARRAYS,RECORDS,FILES,
         TAGFIELD,VARIANT);
  DECLKIND = (STANDARD,DECLARED);
  WBSIZE=RECORD WBLENGTH:INTEGER; BOUNDARY:CELLUNIT END;
  STP = @ STRUCTREC; CTP = @ IDENTREC;

  STRUCTREC=RECORD
         FTYPE: BOOLEAN; (* TRUE IF THE STRUCTURE CONTAINS OR IS A FILE *)
         SIZE: WBSIZE;
         CASE FORM: STRUCTFORM OF
          SCALAR:   (CASE SCALKIND: DECLKIND OF
                     DECLARED: (FCONST: CTP));
          PACKDTYPE:(BASETYPE:STP);
          SUBRANGE: (RANGETYPE: STP; MIN,MAX: INTEGER);
          POINTER:  (ELTYPE: STP);
          POWER:    (PCKDSET: BOOLEAN; ELSET: STP);
          ARRAYS:   (AELTYPE,INXTYPE: STP; AELLENG:INTEGER);
          RECORDS:  (FIELDS,FSTFLD: CTP;
                     RECVAR: STP);
          FILES:    (TEXTFILE:BOOLEAN; FILTYPE:STP);
          TAGFIELD: (TGFLDP: CTP; FSTVAR: STP);
          VARIANT:  (FSTVARFLD: CTP; NXTVAR,SUBVAR: STP;
                     VARVAL: INTEGER)
         END;

(**********************************************************************)
(*                                                                    *)
(* STRUCTRECS ARE THE INTERNAL REPRESENTATION OF PASCAL TYPES.  THE   *)
(* TYPE OF EVERY OBJECT IN THE PROGRAM (CONSTANTS, VARIABLES, AND     *)
(* EXPRESSIONS) IS REPRESENTED BY A POINTER TO A STRUCTREC (TYPE STP).*)
(* IF THE TYPE OF AN OBJECT CANNOT BE DETERMINED (UNDECLARED          *)
(* IDENTIFIER OR IMPROPERLY FORMED EXPRESSION, FOR EXAMPLE), THE TYPE *)
(* IS REPRESENTED BY A NIL POINTER.  THE PROCEDURE COMPTYPES TREATS   *)
(* NIL AS A UNIVERSAL TYPE, WHICH MATCHES ANY OTHER TYPE.  THIS       *)
(* PREVENTS SPURIOUS "ILLEGAL TYPE" MESSAGES GENERATED WHEN AN ILL-   *)
(* FORMED OBJECT IS USED LATER IN THE PROGRAM, SINCE A TYPE POINTER   *)
(* (STP) CAN HAVE THE VALUE NIL. THE FOLLOWING RULE MUST BE OBSERVED  *)
(* AT ALL TIMES:                                                      *)
(*                                                                    *)
(*     NEVER DEREFERENCE A POINTER OF TYPE STP UNLESS YOU ARE SURE    *)
(*     IT DOES NOT HAVE THE VALUE NIL.  THIS APPLIES PARTICULARLY     *)
(*     TO THE IDTYPE FIELD OF A IDENTIFIER RECORD (IDENTREC) AND TO   *)
(*     THE TYPTR FIELD OF AN ATTRIBUTE RECORD (ATTR).                 *)
(*                                                                    *)
(**********************************************************************)


                             (*NAMES*)
                             (*******)

  IDCLASS = (TYPES,KONST,VARS,FIELD,EVENT,PROC,FUNC);
  SETOFIDS = SET OF IDCLASS;
  IDKIND = (ACTUAL,FORMAL);
  DRCTINDRCT = (DRCT,INDRCT);        (*INDRCT: VARIABLE PARAMETER, WITH STATEMENT*)

  IDENTREC=RECORD
         NAME: ALFA; LLINK,RLINK: CTP;
         IDTYPE: STP; NEXT: CTP;
         CASE KLASS: IDCLASS OF
          KONST: (VALUES: VALU);
          VARS:  (VKIND: DRCTINDRCT; VLEV: LEVRANGE;
                  CNTRLVAR:BOOLEAN;  (* FOR-LOOP CONTROL VARIABLE *)
                  VADDR,PARADDR: ADDRRANGE);
          FIELD: (FLDADDR: ADDRRANGE);
          EVENT: (EVENTJUMP: LOCOFREF; EVENTDEF:BOOLEAN);
          PROC,
          FUNC:  (CASE PFDECKIND: DECLKIND OF
                  STANDARD: (KEY: 1..NRSTDNAMES);
                  DECLARED: (PFLEV: LEVRANGE; PARAMS:CTP;
                             ASSIGNEDTO : BOOLEAN;
                             WITHINSCOPE : BOOLEAN;
                             CASE PFKIND: IDKIND OF
                              ACTUAL: (PFCNT:INTEGER; LCSAVE:ADDRRANGE);
                              FORMAL: (PFADDR:ADDRRANGE)))
         END;


  CEP=@CSTEXPREC;
  CSTEXPREC=RECORD ELEMTYPE:STP; ELEMVALUE:VALU;
                   NEXTELEM:CEP
            END;
  FILEP = @ FILEREC;
  FILEREC = RECORD
              FILENAME: ALFA; ADDR:ADDRRANGE;
              NXTP: FILEP;
              DECLARED, GETONRESET : BOOLEAN
            END;

  SCOPEP = @SCOPEREC;
  SCOPEREC = RECORD
               NAME : ALFA;
               NXTP : SCOPEP
             END;

  DISPRANGE = 0..DISPLIMIT;
  WHERE = (BLCK,REC);


                             (*LABELS*)
                             (********)
  LBP = @LABREC;
  LABREC=RECORD
           LABVAL: INTEGER; NEXTLAB: LBP;
           LCNT: 0..MAXPROCFUNC;
           CASE DEFINED: BOOLEAN OF
             TRUE:  (LABADDR: ADDRRANGE);
             FALSE: (FSTOCC: LOCOFREF)
         END;

                      (* CODE GENERATION STRUCTURES *)
                      (******************************)

  TEXTTYPE = ARRAY (.1..OBJLENGTH] OF INTEGER;

  TXTBUF = RECORD
             PRELUDE : PACKED ARRAY (.1..4] OF CHAR;
             ADDRESS : INTEGER;
             LENGTH  : INTEGER;
                 ID  : INTEGER;
             TEXTDATA: TEXTTYPE;
             SEQNOS  : ALFA
           END;

  ENDBUF = RECORD
             PRELUDE : PACKED ARRAY (.1..4] OF CHAR;
             ADDRESS : INTEGER;
               FILL1 : PACKED ARRAY (.1..4] OF CHAR;
                  ID : INTEGER;
               FILL2 : PACKED ARRAY (.1..12] OF CHAR;
              LENGTH : INTEGER;
             PSTLUDE : PACKED ARRAY (.1..48] OF CHAR
           END;

  ESDDATA = RECORD
              NAME : ALFA;
              ADDRESS:INTEGER;
              LENGTH : INTEGER
            END;

  ESDBUF = RECORD
             PRELUDE : ALFA;
             BYTES   : INTEGER;
             ID      : INTEGER;
             DATAITEMS:ARRAY (.1..3] OF ESDDATA;
             FILLER  : ALFA;
             SEQNOS  : ALFA
           END;

  RLDDATA = RECORD
              RELPOS : INTEGER;
              FLAGADDRESS : INTEGER
            END;

  RLDBUF = RECORD
             PRELUDE : ALFA;
             BYTES   : INTEGER;
             DUMMY   : PACKED ARRAY (. 1..4 ] OF CHAR;
             RLDITEMS: ARRAY (. 1..7 ] OF RLDDATA;
             SEQNOS  : ALFA
           END;

  CARD = PACKED ARRAY (. 1..80 ] OF CHAR;

                              (*MISCELLANEOUS*)
                              (***************)

  MARKP = @BOOLEAN;        (*MARK AND RELEASE*)
  REGNO = (R10,R11,R12,R13,F0,F2,F4,F6);


  (*------------------------------------------------------------------------------*)
VAR
                  (*RETURNED BY SOURCE PROGRAM SCANNER
                   INSYMBOL:
                   **********)

  SY: SYMBOL;                     (*LAST SYMBOL*)
  OP: OPERATOR;                   (*CLASSIFICATION OF LAST SYMBOL*)
  IVAL: INTEGER;                  (*VALUE OF LAST INTEGER CONSTANT*)
  RVAL: REAL;                     (*VALUE OF LAST REAL CONSTANT*)
  CONSTP: CTAILP;                 (*POINTER TO LAST STRING*)
  LGTH: INTEGER;                  (*LENGTH OF LAST STRING CONSTANT*)
  ID: ALFA;                       (*LAST IDENT (POSSIBLY TRUNCATED)*)
  CH: CHAR;                       (*LAST CHARACTER*)
  DOTDOT : BOOLEAN;
  DOTFLG : BOOLEAN;


                  (*COUNTERS:*)
                  (***********)

  LC,IC: ADDRRANGE;               (*DATA LOCATION AND INSTR COUNTER*)
  PCNT: INTEGER;                  (*NUMBER OF PROCSY/FUNCTIONS*)
  PROGCOUNT:INTEGER;              (*GLOBAL INSTRUCTION COUNTER*)


                  (*SWITCHES:*)
                  (***********)

  PRTERR: BOOLEAN;                (*TO ALLOW FORWARD REFERENCES
                                  BY SUPPRESSING ERROR MESSAGE*)
  POSSFWDREF: BOOLEAN;            (*SUPPRESS SEARCHID WHERE POSSIBLE
                                  FORWARD REFERENCE*)
  ENTERSCOPE: BOOLEAN;            (*ID ENTERED INTO SCOPE CHECKING
                                  LIST WHEN ENTERSCOPE TRUE*)
  MAINCSECT: BOOLEAN;             (* HEADER OF P@MAIN CSECT GENERATED *)
  DEBUG,LISTON,PMD,PRINTCODE,EXTWARN : BOOLEAN; (* $ SWITCHES *)
  PROCNAMES     :BOOLEAN; (* GENERATE TRACEBACK INFO EVEN WITHOUT PMD *)
  EXTRNL        :BOOLEAN; (* COMPILING EXTERNAL PROCEDURE             *)
  BOLDFACE      :BOOLEAN; (* OVERPRINT KEYWORDS IN LOWER CASE         *)
  WARNING       :BOOLEAN; (* GENERATE WARNING MESSAGES                *)
  VARCHECK      :BOOLEAN; (* CHECK FOR UNDEFINED VARIABLES IN EXPR.   *)
                          (* AND PERFORM MORE COMPLETE POINTER CHECKS.*)


                  (*POINTERS:*)
                  (***********)
  INTPTR,REALPTR,CHARPTR,ALFAPTR,
  BOOLPTR,NILPTR,TEXTPTR: STP;    (*POINTERS TO ENTRIES OF STD IDS*)
  PACKDINTPTR,PACKDCHARPTR:STP;
  UTYPPTR,UCSTPTR,UVARPTR,
  UFLDPTR,UPRCPTR,UFCTPTR,        (*POINTERS TO ENTRIES FOR UNDECL IDS*)
  UEVENTPTR,
  INPUTPTR,OUTPUTPTR,             (*ENTRIES FOR INPUT AND OUTPUT*)
  FWPTR: CTP;                     (*HEAD OF CHAIN OF FORW TYPE IDS*)
  FSTLABP : LBP;                  (*HEAD OF LABEL CHAIN*)
  FEXFILP,LOCFILP: FILEP;         (*HEAD OF LIST OF EXTERNAL/LOCAL FILES*)
  SCOPEHEAD : SCOPEP;             (*POINTER TO HEAD OF SCOPE CHECK LIST*)


                  (*BOOKKEEPING OF DECLARATION LEVELS:*)
                  (************************************)

  LEVEL: LEVRANGE;                (*CURRENT STATIC LEVEL*)
  DISX,                           (*LEVEL OF LAST ID SRCHD BY SEARCHID*)
  TOP: DISPRANGE;                 (*TOP OF DISPLAY*)

  DISPLAY:                        (*WHERE:   MEANS:*)
   ARRAY (.DISPRANGE] OF
           RECORD               (*=BLCK:   ID IS VARIABLE ID*)
             FNAME: CTP;        (*=REC:   ID IS FIELD ID IN RECORD*)
             CASE OCCUR: WHERE OF
               REC: (DADRS:ADDRRANGE;
                     CASE DISPKIND:DRCTINDRCT OF
                          DRCT  : (DLEVEL:LEVRANGE);
                          INDRCT: (DBASEL:LEVRANGE; DBASEA:ADDRRANGE))
           END;

                   (* LISTING CONTROLS AND SCANNER DATA *)
                   (*************************************)

  LEFT,RIGHT,PROCLEV : CHAR;             (*NESTING LEVEL INDICATORS *)
  LOCATION           : INTEGER;          (*OFFSET AT EOL            *)
  DDATE              : PACKED ARRAY(.1..9]
                         OF CHAR;        (* DATE IN FORMAT 'DD MON YY' *)
  TTIME              : ALFA;             (* TIME OF COMPILATION        *)
  PAGEE              : INTEGER;          (*PAGE COUNTER             *)
  SOURCELNO          : INTEGER;          (*SOURCE PGM LINE NUMBER   *)
  ZLEV               : INTEGER;          (*NESTING LEVEL COUNTER    *)
  TTL                : PACKED ARRAY(.1..40]
                          OF CHAR;       (*TITLE BUFFER             *)
  LINEE              : INTEGER;          (*NO OF LINES PRINTED      *)
  DP                 : BOOLEAN;
  LINE    :PACKED ARRAY(.1..MAXCHCNT] OF CHAR;  (* LINE(.1..LL-1] IS CURRENT  *)
  LL      : BUFX;                        (* INPUT LINE UP TO COL. SET BY $U OPT *)
  SAVELL  : BUFX;                        (* LINE(.1..SAVELL] IS ENTIRE LINE    *)
                                         (* READ FROM INPUT, UP TO MAXCHCNT CHRS*)
  CP      : BUFX;                        (* LINE(.CP] IS NEXT CHAR IN PGM      *)
  EOLCH,SAVECH: CHAR;                    (* LINE(.LL] HOLDS EOLCH DURING SCAN. *)
                                         (* SAVECH IS CONTENTS OF LINE(.LL]    *)
                                         (* THAT WERE READ FROM INPUT.          *)
  MMAXLINE : BUFX;                       (* MAX COL TO BE EXAMINED IN INPUT     *)
                                         (* PLUS ONE.  SET BY $U OPTION         *)
  OVERPRINT : BOOLEAN;                   (* CURRENT LINE SHOULD BE OVERPRINTED  *)
                                         (* WITH CONTENTS OF OVERLINE           *)
  OVERLINE : PACKED ARRAY(.1..MAXCHCNT] OF CHAR;  (* COPY OF LINE CONTAINING   *)
                                         (* ONLY CHARACTERS TO BE OVERPRINTED.  *)
  PRINTED : BOOLEAN;                     (* LINE HAS BEEN WRITTEN TO LISTING    *)



                  (*STRUCTURED CONSTANTS:*)
                  (***********************)

  CONSTBEGSYS,SIMPTYPEBEGSYS,TYPEBEGSYS,BLOCKBEGSYS,SELECTSYS,FACBEGSYS,
  STATBEGSYS,TYPEDELS: SETOFSYS;
  CHSY : PACKED ARRAY (. '.' .. 'J' ] OF SYMBOL;
  CHOP : PACKED ARRAY (. '.' .. 'J' ] OF OPERATOR;
  CCHTYPE : PACKED ARRAY (.CHAR] OF CHRTYP;
  HASHTABLE : PACKED ARRAY (.HASH] OF IRW;
  BMASK   : ARRAY(.LTOP..EQOP] OF INTEGER;
  DUALOP  : ARRAY(.LTOP..EQOP] OF LTOP..EQOP;



                    (* OBJECT FILE GENERATION *)
                    (**************************)


  ESDID : 1..256;     (* ESD IDENTIFIER *)
  ESDCNT: 0..256;
  RLDPOS: INTEGER;
  CURRADDRESS : INTEGER;     (* OFFSET FROM START OF TXT BLOCK FOR SD *)
  OBJECTCODE : TEXTTYPE;
  EXTPROCS : 0..MAXPROCFUNC;
  EXTARRAY : PACKED ARRAY (.0..MAXPROCFUNC] OF
                RECORD
                  ENAME:ALFA;
                  ECNT : 0..MAXPROCFUNC
                END;
  PROCREF : ALFA;    (* STORES LAST PROC COMPILED *)
  INITFLAG : BOOLEAN;
  STARTED : BOOLEAN;                  (* TRUE IF P@START CSECT WRITTEN TO SYSGO *)
  RTEXTRN  : ARRAY (.1..NRTEXTP1] OF ALFA;  (* RTEXTRN(.1..NREXTRN] ARE NAMES *)
  NRTEXTRN : INTEGER;                        (*  OF WXTRNS NEEDED BY OBJECT PGM *)


                  (*ERROR MESSAGES:*)
                  (*****************)

  ERRINX: 0..10;                  (*NR OF ERRORS IN CURR SOURCE LINE*)
  ERRORS: BOOLEAN;                (*TRUE IF THE PROGRAM CONTAINS AN ERROR*)
  ERRLIST:
           ARRAY (.1..10] OF
             RECORD POS: 1..MAXCHCNT;
                    NMR: 1..400
             END;

  ERRORTOT : INTEGER;     (* TOTAL NUMBER OF LINES IN ERROR   *)
  $PASMSGS : TEXT;        (* PASCAL ERROR MESSAGE FILE        *)
  SYSTERM  : TEXT;        (* ERROR MESSAGE OUTPUT TO TERMINAL *)
  ERRMSGS  : ARRAY (. 0 .. MAXMSGSDIV64 ]
                     OF SET OF SETMIN..SETMAX;


                    (*OUTPUT BUFFER:*)
                    (****************)

  SYSGO : FILE OF CARD;     (* OUTPUT FILE *)
  INITNUMBER, OBPOINTER : INTEGER;
  MMAXLN : BOOLEAN;
  AUTOGETINPUT, AUTOGETOUTPUT : BOOLEAN;  (*TRUE IF GET ON RESET OF STD FILE*)
  PROCADDRESS : ARRAY (.1..MAXPR1] OF INTEGER ;

  // variables with init values - new in stanford pascal 2020.09

  RLD : RLDBUF        (* MAIN RLD BUFFER *)
     = ( ' RLD    ',0,'    ',((0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0)),
         '        ' );
  ESD : ESDBUF        (* MAIN ESD BUFFER *)
     = ( ' ESD    ',16,1,(('P@MAIN@V',0,0),
                          ('        ',0,0),
                          ('        ',0,0)),
         '        ', '        '  );
  TXT : TXTBUF        (* MAIN TEXT BUFFER *)
     = ( ' TXT',0,0,0,  (0,0,0,0,
                         0,0,0,0,
                         0,0,0,0,
                         0,0),
         '        '  );
  ENDC: ENDBUF        (* MAIN END BUFFER *)
      = ( ' END', 0, '    ', 0, '            ', 0,
          ENDCARDID  );


const
  MNEMONIC: PACKED ARRAY(.0..255,1..4] OF CHAR =

    ( '    ', '    ', '    ', 'TRSK', 'SPM ', 'BALR', 'BCTR', 'BCR ',
      'SSK ', 'ISK ', 'SVC ', 'SKC ', '    ', 'BASR', 'SCFR', 'ICFR',
      'LPR ', 'LNR ', 'LTR ', 'LCR ', 'NR  ', 'CLR ', 'OR  ', 'XR  ',
      'LR  ', 'CR  ', 'AR  ', 'SR  ', 'MR  ', 'DR  ', 'ALR ', 'SLR ',
      'LPDR', 'LNDR', 'LTDR', 'LCDR', 'HDR ', 'LRDR', 'MXR ', 'MXDR',
      'LDR ', 'CDR ', 'ADR ', 'SDR ', 'MDR ', 'DDR ', 'AWR ', 'SWR ',
      'LPER', 'LNER', 'LTER', 'LCER', 'HER ', 'LRER', 'AXR ', 'SXR ',
      'LER ', 'CER ', 'AER ', 'SER ', 'MER ', 'DER ', 'AUR ', 'SUR ',
      'STH ', 'LA  ', 'STC ', 'IC  ', 'EX  ', 'BAL ', 'BCT ', 'BC  ',
      'LH  ', 'CH  ', 'AH  ', 'SH  ', 'MH  ', 'BAS ', 'CVD ', 'CVB ',
      'ST  ', 'LAE ', 'LS  ', 'ICE ', 'N   ', 'CL  ', 'O   ', 'X   ',
      'L   ', 'C   ', 'A   ', 'S   ', 'M   ', 'D   ', 'AL  ', 'SL  ',
      'STD ', '    ', '    ', '    ', '    ', '    ', '    ', 'MXD ',
      'LD  ', 'CD  ', 'AD  ', 'SD  ', 'MD  ', 'DD  ', 'AW  ', 'SW  ',
      'STE ', '    ', '    ', '    ', '    ', '    ', '    ', '    ',
      'LE  ', 'CE  ', 'AE  ', 'SE  ', 'ME  ', 'DE  ', 'AU  ', 'SU  ',
      'IDL ', 'FGP ', 'PC  ', 'DIG ', 'WRD ', 'RDD ', 'BXH ', 'BXLE',
      'SRL ', 'SLL ', 'SRA ', 'SLA ', 'SRDL', 'SLDL', 'SRDA', 'SLDA',
      'STM ', 'TM  ', 'MVI ', 'TS  ', 'NI  ', 'CLI ', 'OI  ', 'XI  ',
      'LM  ', '    ', '    ', '    ', 'SDV ', 'TDV ', 'HDV ', 'CKC ',
      'STMA', 'SKB ', 'PCAS', 'GSK ', '    ', '    ', '    ', '    ',
      'LMA ', 'RTN ', 'TRC ', '    ', '    ', '    ', '    ', '    ',
      'STMC', 'LRA ', '    ', '    ', '    ', '    ', '    ', '    ',
      'LMC ', 'FSK ', '    ', '    ', '    ', '    ', '    ', '    ',
      '    ', '    ', '    ', '    ', '    ', '    ', '    ', '    ',
      '    ', '    ', '    ', '    ', '    ', '    ', '    ', '    ',
      '    ', 'MVN ', 'MVC ', 'MVZ ', 'NC  ', 'CLC ', 'OC  ', 'XC  ',
      '    ', '    ', '    ', '    ', 'TR  ', 'TRT ', 'ED  ', 'EDMK',
      '    ', '    ', '    ', '    ', '    ', '    ', '    ', '    ',
      '    ', '    ', '    ', '    ', '    ', '    ', '    ', '    ',
      '    ', 'MVO ', 'PACK', 'UNPK', '    ', '    ', '    ', '    ',
      'ZAP ', 'CP  ', 'AP  ', 'SP  ', 'MP  ', 'DP  ', '    ', '    ' );

  REALREG : ARRAY(.REGNO] OF INTEGER =
           (  10, 11, 12, 13, 0, 2, 4, 6  );

  NA      : ARRAY(.1..NRSTDNAMES] OF ALFA =
      ( 'GET     ', 'PUT     ', 'RESET   ', 'REWRITE ',
        'PAGE    ', 'READ    ', 'READLN  ', 'WRITE   ',
        'WRITELN ', 'TIME    ', 'DATE    ', 'NEW     ',
        'MARK    ', 'RELEASE ', 'PACK    ', 'UNPACK  ',
        'MESSAGE ', 'HALT    ', 'DISPOSE ', 'SETCC   ',
        'CLOSE   ', 'EXTEND  ', 'TRAPATTN', 'GETPARM ',
        'SYSTEM  ',
        'EOF     ', 'EOLN    ',
        'ODD     ', 'ROUND   ', 'TRUNC   ', 'ABS     ',
        'SQR     ', 'ORD     ', 'CHR     ', 'PRED    ',
        'SUCC    ', 'SIN     ', 'COS     ', 'EXP     ',
        'SQRT    ', 'LN      ', 'ARCTAN  ', 'CLOCK   ',
        'CARD    ', 'TERMINAL', 'LINELENG', 'ATTENTIO',
        'CLOCKLEF', 'UNDEFINE', 'SIZEHEAP', 'SIZESTAC',
        'SIZEFREE', 'SIZEOF  '  );

  IINITCH : PACKED ARRAY (.ISPCH] OF RECORD  C:CHAR; S:SYMBOL;
                                        O:OPERATOR; T:CHRTYP
                                      END
         =
           ( ('+', ADDOP,    PLUS,     SINGLE,),
             ('-', ADDOP,   MINUS,     SINGLE,),
             ('*', MULOP,     MUL,     DOUBLE,),
             ('/', MULOP,    RDIV,     SINGLE,),
             ('=', RELOP,    EQOP,     SINGLE,),
             ('<', RELOP,    LTOP,     DOUBLE,),
             ('>', RELOP,    GTOP,     DOUBLE,),
             ('^', NOTSY,    NOOP,     DOUBLE,),
             ('?', ADDOP,    OROP,     SINGLE,),
             ('&', MULOP,   ANDOP,     SINGLE,),
             ('.', PERIOD,   NOOP,     DOUBLE,),
             (',', COMMA,    NOOP,     SINGLE,),
             (':', COLON,    NOOP,     DOUBLE,),
             (';', SEMICOLON,NOOP,     SINGLE,),
             ('@', ARROW,    NOOP,     SINGLE,),
             ('(', LPARENT,  NOOP,     DOUBLE,),
             (')', RPARENT,  NOOP,     SINGLE,),
             ('#', RELOP,    NOOP,     DOUBLE ));

  RWW : PACKED ARRAY (.IRW] OF
          RECORD
            R : ALFA14; S : SYMBOL; O : OPERATOR;
          END =
       (
        ('              ',      OTHERSY,  NOOP,  ),
        ('AND           ',      MULOP,    ANDOP, ),
        ('ARRAY         ',      ARRAYSY,  NOOP,  ),
        ('BEGIN         ',      BEGINSY,  NOOP,  ),
        ('CASE          ',      CASESY,   NOOP,  ),
        ('CONST         ',      CONSTSY,  NOOP,  ),
        ('DO            ',      DOSY,     NOOP,  ),
        ('DIV           ',      MULOP,    IDIV,  ),
        ('DOWNTO        ',      DOWNTOSY, NOOP,  ),
        ('END           ',      ENDSY,    NOOP,  ),
        ('ELSE          ',      ELSESY,   NOOP,  ),
        ('FOR           ',      FORSY,    NOOP,  ),
        ('FUNCTION      ',      FUNCTSY,  NOOP,  ),
        ('FORALL        ',      FORALLSY, NOOP,  ),
        ('FILE          ',      FILESY,   NOOP,  ),
        ('GOTO          ',      GOTOSY,   NOOP,  ),
        ('IF            ',      IFSY,     NOOP,  ),
        ('IN            ',      RELOP,    INOP,  ),
        ('LOOP          ',      LOOPSY,   NOOP,  ),
        ('LABEL         ',      LABELSY,  NOOP,  ),
        ('MOD           ',      MULOP,    IMOD,  ),
        ('NIL           ',      NILSY,    NOOP,  ),
        ('NOT           ',      NOTSY,    NOOP,  ),
        ('OF            ',      OFSY,     NOOP,  ),
        ('OR            ',      ADDOP,    OROP,  ),
        ('OTHERWISE     ',      OTHERWSY, NOOP,  ),
        ('PROCEDURE     ',      PROCSY,   NOOP,  ),
        ('PACKED        ',      PACKEDSY, NOOP,  ),
        ('POSTLUDE      ',      POSTSY,   NOOP,  ),
        ('PROGRAM       ',      PROGRAMSY,NOOP,  ),
        ('REPEAT        ',      REPEATSY, NOOP,  ),
        ('RECORD        ',      RECORDSY, NOOP,  ),
        ('SET           ',      SETSY,    NOOP,  ),
        ('THEN          ',      THENSY,   NOOP,  ),
        ('TO            ',      TOSY,     NOOP,  ),
        ('TYPE          ',      TYPESY,   NOOP,  ),
        ('UNTIL         ',      UNTILSY,  NOOP,  ),
        ('VAR           ',      VARSY,    NOOP,  ),
        ('INITIALVALUES ',      VALUESY,  NOOP,  ),
        ('WHILE         ',      WHILESY,  NOOP,  ),
        ('WITH          ',      WITHSY,   NOOP   ));


  MONTHS             : PACKED ARRAY(.1..36]
                         OF CHAR (* ABBREVIATIONS FOR MONTHS   *)
        = 'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC';

(*--------------------------------------------------------------------*)

procedure init_procaddress;
var i : integer ;
begin
   procaddress [1] := 0;
   for i := 2 to maxpr1 do procaddress [i] := 1;
end ;

PROCEDURE NEWPAGE;
VAR I:INTEGER;
BEGIN
  PAGE(OUTPUT);
  WRITELN(' PASCAL 8000/', VERSION: 25,
    ' ':3, TTL:40, ' ':11, DDATE, ' AT ',
          TTIME,'     PAGE ',PAGEE:4);
  WRITELN;
  PAGEE := PAGEE+1;
  LINEE := 2;
END; (* NEWPAGE *)


PROCEDURE NEWLINE;
  (* ADVANCES TO NEW PAGE AND WRITES TITLE IF NO ROOM LEFT ON CURRENT PAGE. *)
  (* SHOULD BE CALLED BEFORE WRITING EACH LINE TO OUTPUT *)
BEGIN
  IF LINEE = LINESPERPAGE THEN NEWPAGE;
  LINEE := LINEE + 1;
END;  (* NEWLINE *)


PROCEDURE RIGHTCHECK;
BEGIN
  RIGHT := CHR(ORD('0') + ZLEV MOD 10);
  ZLEV := ZLEV - 1
END;


PROCEDURE LEFTCHECK;
BEGIN
  ZLEV:=ZLEV+1;
  IF LEFT = '-' THEN LEFT:=CHR(ORD('0')+ZLEV MOD 10)
END;


PROCEDURE WRITEHEX(X:INTEGER);
 VAR I,N,C : INTEGER;
  L:INTEGER;
BEGIN
   IF X < 65536 THEN BEGIN L:=4; N:=4096 END
     ELSE IF X < 1048576 THEN BEGIN L:=5; N:=65536 END
       ELSE BEGIN L:=6; N:=1048576 END;
   IF L < 6 THEN WRITE(' ':6-L);
   FOR I := 1 TO L DO
    BEGIN C:=X DIV N;
      IF C>= 10 THEN WRITE(CHR(C-10+ORD('A')))
                ELSE WRITE(CHR(C+ORD('0')));
      X:=X MOD N; N:=N DIV 16
    END
END; (* WRITEHEX *)

PROCEDURE WRITERRORS;
  (* WRITE ERROR NUMBERS AND COLUMN POINTERS ON LISTING AND SYSTERM FILES.
     ASSUMES SOURCE PROGRAM LINE ALREADY WRITTEN. *)
   VAR I: INTEGER;
       LASTPOS,FREEPOS,CURRPOS,CURRNMR,F,K: INTEGER;
         FLAG:BOOLEAN;
BEGIN
  FLAG:=FALSE;
  FOR K:=1 TO ERRINX DO
  IF (ERRLIST(.K].NMR < 310) OR (ERRLIST(.K].NMR > 350)
    THEN FLAG:=TRUE;
  NEWLINE;
  IF FLAG THEN
    BEGIN WRITE(' ***ERROR***':INDENT);
      WRITE(SYSTERM,' ***ERROR***':INDENTTY);
    END
  ELSE
    BEGIN WRITE(' **WARNING**':INDENT);
      WRITE(SYSTERM,' **WARNING**':INDENTTY);
    END;
  LASTPOS := 0; FREEPOS := 1;
  FOR K := 1 TO ERRINX DO
    BEGIN
      WITH ERRLIST(.K] DO
        BEGIN CURRPOS := POS; CURRNMR := NMR END;
      IF CURRPOS = LASTPOS THEN
        BEGIN WRITE(','); WRITE(SYSTERM,',') END
      ELSE
        BEGIN
          IF FREEPOS > CURRPOS THEN
            BEGIN
              WRITELN; WRITELN(SYSTERM);
              NEWLINE; WRITE(' ':INDENT);
              WRITE(SYSTERM,' ':INDENTTY);
              FREEPOS:=1
            END;
          WHILE FREEPOS < CURRPOS DO
            BEGIN WRITE(' '); WRITE(SYSTERM,' ');
              FREEPOS:=FREEPOS+1;
            END;
          WRITE('!'); WRITE(SYSTERM,'!');
          LASTPOS:=CURRPOS;
        END;
      IF CURRNMR < 10 THEN F := 1
        ELSE IF CURRNMR < 100 THEN F := 2
          ELSE F:= 3;
      WRITE(CURRNMR:F);
      WRITE(SYSTERM,CURRNMR:F);
      FREEPOS := FREEPOS + F + 1
    END;
  WRITELN(SYSTERM); WRITELN;
  ERRINX := 0;
END; (* WRITERRORS *)

PROCEDURE ENDOFLINE;
 VAR I,J : INTEGER;
BEGIN
  IF NOT PRINTED THEN
   BEGIN
     IF LISTON OR (ERRINX>0) THEN
     BEGIN
       NEWLINE;
       WRITE(' ');
       WRITE(SOURCELNO:5,' ');
       WRITEHEX(LOCATION);
       WRITE(' ',LEFT,RIGHT,' ',PROCLEV,' ':2);
       IF SAVELL < MAXCHCNT THEN I := SAVELL ELSE I := MAXCHCNT;
       LINE(.LL] := SAVECH;
       WRITELN(LINE:I);
       IF OVERPRINT THEN
         FOR J := 1 TO DARKNESS DO
           WRITELN('+', ' ':INDENT-1, OVERLINE:I);
       IF ERRINX > 0 THEN
         WRITELN(SYSTERM,' ':4,SOURCELNO:5,' ':3,LINE:I);
       LINE(.LL] := EOLCH;
    END;
    LEFT := '-'; RIGHT := '-';  PROCLEV := ' ';  PRINTED := TRUE;
   END;  (* IF NOT PRINTED *)
   IF ERRINX >0 THEN WRITERRORS;
   IF DP THEN LOCATION := LC ELSE LOCATION := IC;
END;(*ENDOFLINE*)


PROCEDURE WRITEMESSAGES; FORWARD;

PROCEDURE ERROR(FERRNR: INTEGER);
    (* MESSAGES 310-350 ARE WARNING MESSAGES *)
  VAR
      ERRCNT : INTEGER;
BEGIN
  IF (FERRNR < 310) OR (FERRNR > 350) THEN ERRORS:=TRUE;
  IF ERRINX = 0 THEN ERRORTOT := ERRORTOT + 1;
  IF ERRINX >= 9 THEN
     BEGIN ERRLIST(.10].NMR := 255; ERRINX := 10 END
  ELSE
    BEGIN ERRINX := ERRINX + 1;
      ERRLIST(.ERRINX].NMR := FERRNR
    END;
  ERRCNT := ERRLIST(. ERRINX ].NMR;
  ERRMSGS(. ERRCNT DIV 64 ] :=
              ERRMSGS(. ERRCNT DIV 64 ] + (. ERRCNT MOD 64 ];
  ERRLIST(.ERRINX].POS := CP;
  IF FERRNR=400 THEN
    BEGIN WRITEMESSAGES; HALT END;
END; (* ERROR *)

PROCEDURE EXTENSION(ENO: INTEGER);
  (* ISSUE WARNING ABOUT EXTENSION NUMBER 'ENO' IF REQUESTED *)
BEGIN
  IF EXTWARN THEN ERROR(ENO)
END;  (* EXTENSION *)

PROCEDURE SSYMBOLS;
  VAR I:IRW; K:CHAR; L:ISPCH; M:HASH;
      T:RECORD CASE INTEGER OF 0:(R:ALFA14); 1:(I:INTEGER) END;
BEGIN
  FOR K := CHR(0) TO CHR(255) DO CCHTYPE(.K] := INVALID;
  FOR K := '0' TO '9'         DO CCHTYPE(.K] := DDIGIT;
  FOR K := 'A' TO 'I'         DO CCHTYPE(.K] := LLETTER;
  FOR K := 'J' TO 'R'         DO CCHTYPE(.K] := LLETTER;
  FOR K := 'S' TO 'Z'         DO CCHTYPE(.K] := LLETTER;
  FOR K := CHR(129) TO CHR(137) DO CCHTYPE(.K] := LLETTER;
  FOR K := CHR(145) TO CHR(153) DO CCHTYPE(.K] := LLETTER;
  FOR K := CHR(162) TO CHR(169) DO CCHTYPE(.K] := LLETTER;
  CCHTYPE(.'H'] := MUSTBEID; CCHTYPE(.'J'] := MUSTBEID;
  CCHTYPE(.'K'] := MUSTBEID; CCHTYPE(.'Q'] := MUSTBEID;
  CCHTYPE(.'X'] := MUSTBEID; CCHTYPE(.'Y'] := MUSTBEID;
  CCHTYPE(.CHR(136)] := MUSTBEID;
  CCHTYPE(.CHR(145)] := MUSTBEID;
  CCHTYPE(.CHR(146)] := MUSTBEID;
  CCHTYPE(.CHR(152)] := MUSTBEID;
  CCHTYPE(.CHR(167)] := MUSTBEID;
  CCHTYPE(.CHR(168)] := MUSTBEID;
  CCHTYPE(.'$'] := MUSTBEID; CCHTYPE(.''''] := QUOTE;
  CCHTYPE(.IGNORECHARACTER] := IGNORE;
  FOR L:=1 TO NOSPCH DO
   WITH IINITCH(.L] DO
     BEGIN CCHTYPE(.C]:=T; CHSY(.C]:=S; CHOP(.C]:=O END;
  (*LBRACK:*) CCHTYPE(.CHR(173)] := SINGLE;
              CHSY(.CHR(173)] := LBRACK;
              CHOP(.CHR(173)] := NOOP;
  (*RBRACK:*) CCHTYPE(.CHR(189)] := SINGLE;
              CHSY(.CHR(189)] := RBRACK;
              CHOP(.CHR(189)] := NOOP;
  (*LBRACE:*) CCHTYPE(.CHR(139)] := SINGLE;
              CHSY(.CHR(139)] := LBRACESY;
              CHOP(.CHR(139)] := NOOP;
  (*LBRACE:*) CCHTYPE(.CHR(192)] := SINGLE;
              CHSY(.CHR(192)] := LBRACESY;
              CHOP(.CHR(192)] := NOOP;
  (*RBRACE:*) CCHTYPE(.CHR(155)] := SINGLE;
              CHSY(.CHR(155)] := RBRACESY;
              CHOP(.CHR(155)] := NOOP;
  (*RBRACE:*) CCHTYPE(.CHR(208)] := SINGLE;
              CHSY(.CHR(208)] := RBRACESY;
              CHOP(.CHR(208)] := NOOP;
  FOR M:=-MAGIC TO 0 DO HASHTABLE(.M]:=0; (*INDEX OF DUMMY ENTRY*)
  FOR I := 1 TO RESWORDS DO
    BEGIN
      T.R:=RWW(.I].R; M:=T.I MOD MAGIC;
      IF HASHTABLE(.M] = 0 THEN HASHTABLE(.M] := I ELSE ERROR(400);
    END;
  EOLCH := CHR(EOLCODE); CCHTYPE(.EOLCH] := EOL; MMAXLN := FALSE;
  MMAXLINE := MAXCP;
END; (* SSYMBOLS *)

PROCEDURE OPTCARD;
 VAR I:INTEGER;
     M:INTEGER;
     C:CHAR;
BEGIN (*OPTCARD*)
  IF LL > 2 THEN
  BEGIN
    CH := LINE(.2];
    IF (CH='E') OR (CH=CHR(133)) THEN
      LINEE := LINESPERPAGE
    ELSE IF (CH='U') OR (CH=CHR(164)) THEN
      LINEE := LINESPERPAGE+1    (* NO FURTHER TITLES *)
    ELSE IF (CH='T') OR (CH=CHR(163)) THEN
    BEGIN
      I := 3;
      WHILE (I<LL) AND (LINE(.I] <> ' ') DO I:=I+1;
      I := I+1;
      FOR M:=1 TO 40 DO
      BEGIN
        IF I < LL THEN TTL(.M] := LINE(.I]
        ELSE TTL(.M] := ' ';
        I := I+1;
      END; LINEE := LINESPERPAGE;
    END
    ELSE IF (CH='S') OR (CH=CHR(162)) THEN
    BEGIN
      I := 3;
      WHILE (I<LL) AND (LINE(.I]<>' ') DO I:=I+1;
      WHILE (I<LL) AND (LINE(.I]= ' ') DO I:=I+1;
      M:=0; C:=LINE(.I];
      WHILE ('0'<=C) AND (C<='9') DO
        BEGIN M:=10*M + ORD(C)-ORD('0'); I:=I+1; C:=LINE(.I] END;
      IF LISTON THEN
       FOR I := 1 TO M DO
        BEGIN NEWLINE; WRITELN END;
    END
 END
END; (*OPTCARD*)

PROCEDURE GETLN;
  (* INVARIANT ON ENTRY AND EXIT:
         NOT EOF(INPUT) => INPUT@ = FIRST CHARACTER OF NEXT LINE. *)
VAR I : INTEGER;
BEGIN (* GETLN *)
  ENDOFLINE;
  while true do begin
    CP := 1;
    IF EOF(INPUT) THEN
      BEGIN  ERROR(398); GOTO 9999 END;
    I := LINELENGTH(INPUT);         (* LENGTH OF NEXT LINE *)
    READLN(INPUT,LINE);             (* NON-STANDARD READ   *)
    SOURCELNO := SOURCELNO + 1 ;
    IF I >= MAXCHCNT THEN
      BEGIN ERROR(180); SAVELL := MAXCHCNT END
    ELSE SAVELL := I;
    IF SAVELL < 1 THEN
      BEGIN (* PAD NULL LINE TO PREVENT INVALID FIELDWIDTH WHEN WRITTEN *)
        LINE(.1] := ' '; SAVELL := 1;
      END;
    IF SAVELL < MMAXLINE THEN LL := SAVELL + 1 ELSE LL := MMAXLINE;
    SAVECH := LINE(.LL];  LINE(.LL] := EOLCH;
    PRINTED := FALSE;
    IF OVERPRINT THEN
      BEGIN (* CLEAR OVERPRINT LINE *)
        FOR I := 1 TO MAXCHCNT DO OVERLINE(.I]:=' ';
        OVERPRINT := FALSE;
      END;
    IF LINE(.1] = '$' THEN OPTCARD ELSE break;
  END; (* LOOP *)
END;

PROCEDURE OOPTIONS;
  LABEL 99;
  VAR CH:CHAR;

  PROCEDURE SETOPTION(VAR F:BOOLEAN);
  BEGIN
    IF (CH='+') OR (CH='-') THEN F:=(CH='+') ELSE GOTO 99;
  END;

BEGIN   (* OOPTIONS *)
  (* NOTICE:  FOLLOWING OPTION LETTERS ARE RESERVED FOR THE FUTURE
     USES INDICATED AND SHOULD NOT BE USED FOR OTHER PURPOSES:
     (THESE LETTERS ARE USED SIMILARLY IN SOME OTHER IMPLEMENTATIONS)
     (THIS DOES NOT MEAN WE WILL EVER GET AROUND TO IMPLEMENTING ALL OF
      THESE, BUT IF WE DO, WE SHOULD USE THESE OPTION LETTERS]
        A - ASCII.  USE ASCII COLLATING SEQUENCE
        D - DEBUG.  INSERT BREAKPOINT CODE AND EXTERN FOR DEBUGGER
        G - GOTOS.  PROHIBIT GOTO STATEMENTS (FOR TEACHING)
        I - INCLUDE. INCLUDE SOURCE FROM PDS OR SEQUENTIAL FILE.
        K - KRUNCH.  USED WHEN DISPLAY IS NOT AVAILABLE FOR EXTENDED ADDRESSES
                     TO INDICATE PROC BODY IS <= 4K, AND DIRECT CODE CAN BE
                     GENERATED FOR ADDRESSING (NO EXTRA BASE REGS NEEDED).
        O - OFFSETS. PRINT PROC BODY CODE OFFSETS IN LISTING.
        Q - QUICK.  USED IF WE EVER HAVE AN OPTIMIZER TO SPECIFY PREFERENCE
                    FOR SPEED OVER SPACE.
        R - REGISTER.  USED WHEN STATIC BACK CHAIN REPLACES DISPLAY TO INDICATE
                    DEDICATED BASE REGISTER SHOULD BE ASSIGNED FOR VARS OF
                    NEXT PROCEDURE.  (2 OR 3 PROCEDURES MAX AT ANY ONE TIME)
        X - XREF.   GENERATE CROSS REFERENCE.
        8 - 8 LPI.  PRINT 80 LINES/PAGE TO FILL 11" PAGE AT 8 LINES PER INCH.
     (K AND R ARE INTENDED FOR PROGRAMMER-DIRECTED OPTIMIZATION WHEN MORE
      THAN 5 NESTED PROCS ARE ALLOWED, AND DISPLAY HAS TO BE ABANDONED.  THIS
      SHOULD ALLOW US TO RETAIN SAME GENERAL EFFICIENCY WE HAVE NOW FOR
      THE COMPILER, AT LEAST]
   *)

  (* SECOND CONSTATN IN EACH CASE LABEL IS A LOWER CASE LETTER *)
  REPEAT
    CP := CP+1;
    IF CP>LL-2 THEN GOTO 99;
    CH := LINE(.CP+1];

// opp: hier waren zusaetzliche blanks hinter den case-tags ???

    CASE LINE(.CP] OF
      'B': SETOPTION(BOLDFACE);
      'C': SETOPTION(PRINTCODE);
      'E': IF EXTRNL THEN ERROR(382) ELSE
             BEGIN SETOPTION(EXTRNL);
               IF EXTRNL THEN
                 BEGIN IF INITNUMBER <> 0 THEN ERROR(300);
                       IF MAINCSECT THEN ERROR(385);
                 END;
             END;
      'L': SETOPTION(LISTON);
      'N': SETOPTION(PROCNAMES);
      'P': SETOPTION(PMD);
      'S': SETOPTION(EXTWARN);
      'T': BEGIN
             SETOPTION(DEBUG);
             IF NOT DEBUG THEN VARCHECK := FALSE;
           END;
      'U': BEGIN
             SETOPTION(MMAXLN); LINE(.LL]:=SAVECH;
             IF MMAXLN THEN MMAXLINE:=73 ELSE MMAXLINE:=MAXCP;
             IF SAVELL<MMAXLINE THEN LL:=SAVELL+1 ELSE LL:=MMAXLINE;
             IF CP > LL THEN
               BEGIN ERROR(293); GETLN; GOTO 99 END;
             SAVECH := LINE(.LL]; LINE(.LL] := EOLCH;
           END;
      'V': BEGIN
             SETOPTION(VARCHECK);
             IF VARCHECK THEN DEBUG := TRUE;
           END;
      'W': SETOPTION(WARNING);
      OTHERWISE GOTO 99
    END;
    CP := CP+2;
  UNTIL LINE(.CP] <> ',';
99:END; (* OOPTIONS *)

PROCEDURE READPARMS;
  (* READ PARAMETERS FROM EXEC CARD OR COMMAND LINE & SET OPTIONS *)
  (* MUST BE CALLED BEFORE FIRST CALL TO GETLN.                   *)
  (* IF ANY OPTIONS ARE SUPPLIED AS PARAMETERS, THOSE THAT ARE    *)
  (* USED WILL BE PRINTED IN THE LISTING FILE.                    *)
LABEL 1,99;
VAR OPTS: PACKED ARRAY(.1..MAXPARMLEN] OF CHAR;
    OPTLEN,I: 0..MAXPARMLEN;
BEGIN
  GETPARM(OPTS,OPTLEN);
  IF OPTLEN < 1 THEN GOTO 99;

  (* COPY OPTIONS TO LINE WITH EXACTLY ONE COMMA BETWEEN EACH PAIR *)
  IF OPTLEN = MAXPARMLEN THEN OPTLEN := MAXPARMLEN - 1;
  OPTS(.OPTLEN+1] := ' ';
  LL := 0;  I := 1;  CP := 0;
  WHILE LL < MAXCHCNT DO
    BEGIN  (* SKIP TO BEGINNING OF NEXT OPTION *)
      while true do begin IF I > OPTLEN THEN GOTO 1;
        IF (OPTS(.I]<>' ') AND (OPTS(.I]<>',') THEN break;
        I := I + 1;
      END;

      (* COPY NEXT OPTION TO STRING *)
      LL := LL + 1;  (* INCLUDE PREVIOUS COMMA *)
      WHILE (OPTS(.I]<>' ') AND (OPTS(.I]<>',') AND (LL<MAXCHCNT) DO
        BEGIN  LINE(.LL]:=OPTS(.I];
          LL := LL + 1;  I := I + 1;
        END;
      LINE(.LL] := ',';
    END;
1:  ;
  IF LL > 1 THEN
    BEGIN LINE(.LL] := EOLCH;
      SAVECH := ' ';
      SAVELL := LL - 1;
      OOPTIONS;
    END;
  NEWLINE; WRITE('INITIAL OPTIONS: ': INDENT);
  IF CP > 1 THEN WRITE(LINE:CP-1);  (* LINE(.CP] = CHAR PAST TRAILING COMMA *)
  WRITELN;
99: ;
END;  (* READPARMS *)

PROCEDURE INSYMBOL;
LABEL 1,50;
 VAR L:BUFX; K:IRW; CH,C:CHAR; EXP,SCALE:INTEGER; SIGN:BOOLEAN;
   I: BUFX;
   TP: CTAILP; R:REAL;
   IDBUF: RECORD CASE INTEGER OF
            0: (B:PACKED ARRAY (.1..MAXCP] OF CHAR);
            1: (I:ALFA);
            2: (N:INTEGER);
            3: (S:ARRAY(.1..30] OF INTEGER);
            4: (R:ALFA14)
           END;


  PROCEDURE INTGR(VAR IV:INTEGER);
  VAR D,I:INTEGER;
  BEGIN (* INTGR *)
    I:=ORD(LINE(.CP])-ORD('0'); CP:=CP+1;
    WHILE CCHTYPE(.LINE(.CP]]=DDIGIT DO
      BEGIN
        D := ORD(LINE(.CP])-ORD('0'); CP:=CP+1;
        IF I<=(MAXINT-D) DIV 10 THEN I:=10*I+D
          ELSE BEGIN ERROR(203); I:=0 END;
      END;
    IV := I
  END; (* INTGR *)

  PROCEDURE SKIPCOMMENT;
    (* SKIP THROUGH INPUT UNTIL LINE(.CP] IS CHAR PAST COMMENT *)
    (* ON ENTRY, LINE(.CP] = FIRST CHARACTER OF COMMENT        *)
    (* ALSO PROCESSES $ OPTIONS AT BEGINNING.                   *)
  VAR CH:CHAR;
  BEGIN
    IF LINE(.CP] ='$' THEN OOPTIONS;
    (* SKIP UNTIL END OF COMMENT *)
    while true do begin
      (* SKIP UNTIL '*' OR RIGHT BRACE *)
      while true do begin
        LINE(.LL] := CHR(155);  (* RIGHT BRACE *)
        CH := LINE(.CP];
        IF WARNING THEN
          WHILE (CH <> '*') AND (CH <> CHR(155)) AND (CH <> CHR(208)) DO
            BEGIN
              IF (CH=';') OR (CH=CHR(139)) OR (CH=CHR(192)) (* LEFT BRACE *)
                THEN ERROR(310);
              CP:=CP+1;  CH:=LINE(.CP];
            END
        ELSE (* SEPARATE LOOP FOR SPEED IF NOT CHECKING COMMENTS *)
          WHILE (CH <> '*') AND (CH <> CHR(155)) AND (CH <> CHR(208)) DO
            BEGIN CP:=CP+1; CH:=LINE(.CP] END;
        (* (CH='*') OR (CH=RIGHT BRACE) OR (END OF LINE)  *)
        IF CP < LL THEN break;
        GETLN;
      END;
      (* CH = '*' OR CH = RIGHT BRACE *)
      IF CH = CHR(155) THEN break;
      IF CH = CHR(208) THEN break;
      (* CH = '*' *)
      CP := CP+1;  CH := LINE(.CP];
      IF CH = ')' THEN break; (* END OF COMMENT *)
      IF WARNING THEN
        IF CP > 2 THEN
          IF LINE(.CP-2] = '(' THEN ERROR(310);  (* OPEN COMMENT IN COMMENT *)
    END;  (* LOOP *)
    (* LINE(.CP] IS RIGHT BRACE OR ')' *)
    LINE(.LL] := EOLCH;  CP := CP + 1;
  END;  (* SKIPCOMMENT *)


  PROCEDURE CHECKDOLLAR;  (* ISSUE WARNING IF $ IN CURRENT ID *)
  VAR I: INTEGER;
  BEGIN
    I := 1;
    while true do begin
      IF I = L THEN break;
      IF IDBUF.B(.I] = '$' THEN
        BEGIN EXTENSION(339); break END;
      I := I + 1;
   END;
 END;  (* CHECKDOLLAR *)


BEGIN (* INSYMBOL *)
 1: WHILE LINE(.CP]=' ' DO CP:=CP+1; CH:=LINE(.CP];
  CASE CCHTYPE(.CH] OF
 EOL:   IF CP >= LL THEN BEGIN GETLN; GOTO 1 END ELSE
        BEGIN SY := OTHERSY; CP:=CP+1 END;
INVALID:BEGIN SY := OTHERSY; CP:=CP+1 END;
IGNORE : BEGIN CP:=CP+1; GOTO 1 END;
LLETTER: BEGIN
          IDBUF.R := NULLID; L:=1;
          REPEAT
            IF (CH >= CHR(129)) AND (CH <= CHR(169)) THEN
              IDBUF.B(.L] := CHR(ORD(CH)+64)
            ELSE IDBUF.B(.L] := CH;
            L := L+1; REPEAT CP := CP+1; CH := LINE(.CP]
                      UNTIL (CH <> IGNORECHARACTER);
          UNTIL CCHTYPE(.CH] > LLETTER;
          K:=HASHTABLE(.IDBUF.N MOD MAGIC];
          IF K=0 THEN BEGIN SY:=IDENT; ID:=IDBUF.I;
                            IF EXTWARN THEN CHECKDOLLAR END
          ELSE
           WITH RWW(.K] DO
             IF R = IDBUF.R THEN
               BEGIN (* RESERVED WORD *)
                 SY := S; OP := O;
                 IF BOLDFACE THEN
                   BEGIN (* TRANSLATE TO LOWER CASE AND OVERPRINT *)
                     FOR I := CP-1 DOWNTO CP-L+1 DO
                       BEGIN C := LINE(.I];
                         IF ('A'<=C) AND (C<='Z') THEN C:=CHR(ORD(C)-64);
                         LINE(.I]:=C; OVERLINE(.I]:=C;
                       END;
                     OVERPRINT:=TRUE;
                   END
               END  (* RESERVED WORD *)
             ELSE BEGIN SY:=IDENT; ID:=IDBUF.I;
                        IF EXTWARN THEN CHECKDOLLAR END;
        END;
MUSTBEID:BEGIN
           IDBUF.R:=NULLID; L:=1;
           REPEAT
             IDBUF.B(.L]:=CH; L:=L+1; REPEAT CP:=CP+1; CH:=LINE(.CP];
                                       UNTIL (CH <> IGNORECHARACTER);
           UNTIL CCHTYPE(.CH] > LLETTER;
           SY:=IDENT; ID:=IDBUF.I;
           IF EXTWARN THEN CHECKDOLLAR;
           (* CONVERT CASE TO UPPERCASE *)
           FOR L := 1 TO ALFALENG DO
             IF (ID(.L] >= CHR(129)) AND (ID(.L] <= CHR(169)) THEN
               ID(.L] := CHR(ORD(ID(.L])+64)
         END;
DDIGIT: BEGIN
          SY:=INTCONST; INTGR(IVAL); SCALE := 0;
          IF LINE(.CP]='.' THEN BEGIN
           CP:=CP+1; CH:=LINE(.CP];
           IF (CH=')') OR (CH='.') THEN CP:=CP-1
           ELSE IF CCHTYPE(.CH] <> DDIGIT THEN ERROR(201)
           ELSE
           BEGIN
             SY:=REALCONST; RVAL:=IVAL;
             REPEAT
               RVAL:=10.0*RVAL+(ORD(LINE(.CP])-ORD('0'));
               SCALE:=SCALE-1; CP:=CP+1;
             UNTIL CCHTYPE(.LINE(.CP]] <> DDIGIT;
           END
          END;
         IF (LINE(.CP] = 'E') OR (LINE(.CP] = CHR(133)) THEN
         BEGIN
          IF SY=INTCONST THEN BEGIN SY:=REALCONST; RVAL:=IVAL END;
          CP:=CP+1; CH:=LINE(.CP]; SIGN := TRUE;
          IF (CH='+') OR (CH='-') THEN BEGIN SIGN:=(CH='+');
                                       CP:=CP+1 END;
          IF CCHTYPE(.LINE(.CP]]<>DDIGIT THEN ERROR(201)
          ELSE
          BEGIN
            INTGR(EXP);
            IF SIGN THEN SCALE:=SCALE+EXP ELSE SCALE:=SCALE-EXP;
          END;
         END;
         IF SCALE <> 0 THEN BEGIN
          IF SCALE >= 0 THEN SIGN:=TRUE ELSE BEGIN SIGN:=FALSE;
                                        SCALE:=-SCALE END;
         R:=1.0; FOR EXP:=1 TO SCALE DO R:=R*10.0;
         IF SIGN THEN RVAL:=RVAL*R ELSE RVAL:=RVAL/R;
         END;
        END;
SINGLE:BEGIN SY := CHSY(.CH]; OP := CHOP(.CH];
         CP := CP+1;
         IF SY = LBRACESY THEN
           BEGIN
             SKIPCOMMENT;
             GOTO 1
           END;
        END;
DOUBLE:BEGIN SY:=CHSY(.CH]; OP:=CHOP(.CH]; CP:=CP+1;
         C:=LINE(.CP];
         CASE CH OF
           '¬': IF C='=' THEN BEGIN CP:=CP+1; SY:=RELOP; OP:=NEOP END;
           '<': IF C='>' THEN BEGIN CP:=CP+1;            OP:=NEOP END
                ELSE IF C='=' THEN BEGIN CP:=CP+1;       OP:=LEOP END;
           '>': IF C='=' THEN BEGIN CP:=CP+1;            OP:=GEOP END;
           ':': IF C='=' THEN BEGIN CP:=CP+1; SY:=BECOMES END
                ELSE DOTDOT:=FALSE;
           '.': IF C=')' THEN BEGIN CP:=CP+1; SY:=RBRACK END
                ELSE  IF C='.' THEN BEGIN CP:=CP+1; SY:=COLON;
                        DOTDOT:=TRUE END;
           '#': IF C=')' THEN BEGIN CP:=CP+1; SY:=RCBRACK END
                ELSE OP := NEOP;
           '*': IF C='*' THEN BEGIN CP:=CP+1; SY:=EXPONOP END;
           '(': IF C='.' THEN BEGIN CP:=CP+1; SY:=LBRACK END
                ELSE
                  IF C = '*' THEN
                  BEGIN
                    CP := CP + 1;
                    SKIPCOMMENT;
                    GOTO 1;
                  END
                  ELSE IF C='#' THEN BEGIN CP:=CP+1; SY:=LCBRACK END
         END; (* CASE CH *)
       END; (* DOUBLE *)
QUOTE: BEGIN CP:=CP+1; LGTH := 0;
         while true do begin
           WHILE LINE(.CP] <> '''' DO
           BEGIN
             IF CP >= LL THEN BEGIN ERROR(202); break END;
50:          LGTH:=LGTH+1; IDBUF.B(.LGTH] := LINE(.CP];
             CP:=CP+1;
           END;
           CP:=CP+1;
           IF LINE(.CP]='''' THEN GOTO 50 ELSE break;
         END;
         IF LGTH=1 THEN BEGIN SY:=CHARCONST;
           IVAL:=ORD(IDBUF.B(.1])
         END
         ELSE IF LGTH = 0 THEN
           BEGIN
             ERROR(204);
             SY := CHARCONST;
             IVAL := 0
           END
         ELSE BEGIN
                SY:=STRINGCONST; CONSTP:=NIL;
                FOR L:=LGTH+1 TO LGTH+3 DO IF L <= MAXCP THEN
                  IDBUF.B(.L]:=' ';
                FOR L:=(LGTH+3) DIV 4 DOWNTO 1 DO
                BEGIN
                  TP:=CONSTP; NEW(CONSTP);
                  WITH CONSTP@ DO
                    BEGIN NXTCSP:=TP; STFR:=IDBUF.S(.L] END
                END
              END
       END
  END
END; (* INSYMBOL *)

PROCEDURE ENTERID(FCP: CTP);
  (*ENTER ID POINTED AT BY FCP INTO THE NAME-TABLE,
   WHICH ON EACH DECLARATION LEVEL IS ORGANISED AS
   AN UNBALANCED BINARY TREE*)
VAR
    NAM: ALFA; LCP, LCP1: CTP; LLEFT: BOOLEAN;
BEGIN NAM := FCP@.NAME;
  LCP := DISPLAY(.TOP].FNAME;
  IF LCP = NIL THEN
    DISPLAY(.TOP].FNAME := FCP
  ELSE
    BEGIN
      REPEAT LCP1 := LCP;
        IF LCP@.NAME = NAM THEN   (*NAME CONFLICT, FOLLOW RIGHT LINK*)
          BEGIN ERROR(101); LCP := LCP@.RLINK; LLEFT := FALSE END
        ELSE
          IF LCP@.NAME < NAM
            THEN BEGIN LCP := LCP@.RLINK; LLEFT := FALSE END
            ELSE BEGIN LCP := LCP@.LLINK; LLEFT := TRUE END
      UNTIL LCP = NIL;
      IF LLEFT THEN LCP1@.LLINK := FCP ELSE LCP1@.RLINK := FCP
    END;
  FCP@.LLINK := NIL; FCP@.RLINK := NIL
END;

PROCEDURE SEARCHSECTION(FCP: CTP; VAR FCP1: CTP);
  LABEL 1;
BEGIN
  WHILE FCP <> NIL DO
    IF FCP@.NAME = ID THEN GOTO 1
      ELSE IF FCP@.NAME < ID THEN FCP := FCP@.RLINK
        ELSE FCP := FCP@.LLINK;
1: FCP1 := FCP
END; (* SEARCHSECTION *)

PROCEDURE SEARCHID(FIDCLS: SETOFIDS; VAR FCP: CTP);
  LABEL 1;
  VAR LCP: CTP;
      DIST : DISPRANGE;
      SCOPETMP : SCOPEP;
      FOUND : BOOLEAN;
BEGIN
  FOUND := FALSE;
  DIST := TOP;
  while true do begin  (* FOR DIST := TOP DOWNTO 0 DO ... -- BUT LEAVES DIST DEFINED AFTER *)
    LCP:=DISPLAY(.DIST].FNAME;
    WHILE LCP <> NIL DO
      WITH LCP@ DO
        IF NAME = ID THEN
          IF KLASS IN FIDCLS THEN
            BEGIN
              FOUND := TRUE;
              GOTO 1
            END
          ELSE
            BEGIN IF PRTERR THEN ERROR(103);
              LCP := RLINK
            END
        ELSE
          IF NAME<ID THEN LCP:=RLINK
                     ELSE LCP:=LLINK;
   IF DIST = 0 THEN break
     ELSE DIST := DIST - 1;
 END;  (* LOOP *)
 (*SEARCH NOT SUCCESSFUL; SUPPRESS ERROR MESSAGE IN CASE
                        !310
  OF FORWARD REFERENCED TYPE ID IN POINTER TYPE DEFINITION
  OR VARIANTS WITHOUT TAGFIELDS
  --> PROCEDURE FIELDLIST
  --> PROCEDURE TYP*)
 IF PRTERR THEN
   BEGIN ERROR(104);
     (*TO AVOID RETURNING NIL, REFERENCE AN ENTRY
      FOR AN UNDECLARED ID OF APPROPRIATE CLASS
      --> PROCEDURE ENTERUNDECL*)
     IF VARS IN FIDCLS THEN LCP:=UVARPTR
       ELSE IF FIELD IN FIDCLS THEN LCP:=UFLDPTR
         ELSE IF KONST IN FIDCLS THEN LCP:=UCSTPTR
           ELSE IF PROC IN FIDCLS THEN LCP:=UPRCPTR
             ELSE IF FUNC IN FIDCLS THEN LCP:=UFCTPTR
              ELSE IF TYPES IN FIDCLS THEN LCP:=UTYPPTR
               ELSE LCP:=UEVENTPTR;
   END;
1: ;
 IF FOUND AND ENTERSCOPE THEN
   IF DIST < LEVEL THEN
     BEGIN
       NEW(SCOPETMP);  SCOPETMP@.NAME := ID;
       SCOPETMP@.NXTP := NIL;
       IF SCOPEHEAD <> NIL THEN
         SCOPETMP@.NXTP := SCOPEHEAD;
       SCOPEHEAD := SCOPETMP
     END;
 DISX := DIST; FCP := LCP
END (*SEARCHID*) ;

PROCEDURE GETBOUNDS(FSP: STP; VAR FMIN,FMAX: INTEGER);
  (*GET INTERNAL BOUNDS OF SUBRANGE OR SCALAR TYPE*)
  (*ASSUME (FSP <> INTPTR) AND (FSP <> REALPTR)*)
  BEGIN
    IF FSP <> NIL THEN
     BEGIN
       IF FSP@.FORM = PACKDTYPE THEN FSP:=FSP@.BASETYPE;
      WITH FSP@ DO
        BEGIN
          IF FORM = SUBRANGE THEN
            BEGIN FMIN := MIN; FMAX := MAX END
          ELSE
            BEGIN FMIN := 0; FMAX := 0;
              IF FORM = SCALAR THEN
                BEGIN
                  IF SCALKIND = STANDARD THEN
                    BEGIN IF FSP = CHARPTR THEN FMAX := ORDCHARMAX
                      ELSE IF FSP=BOOLPTR THEN FMAX:=1;
                    END
                  ELSE
                    IF FSP@.FCONST <> NIL THEN
                     FMAX := FSP@.FCONST@.VALUES.IVAL
                END
            END
        END;
       END
  END;


PROCEDURE SKIP(FSYS: SETOFSYS);
  (*SKIP INPUT STRING UNTIL RELEVANT SYMBOL FOUND*)
BEGIN WHILE NOT (SY IN FSYS) DO INSYMBOL; END;


PROCEDURE ACCEPT(X:SYMBOL; Y:INTEGER);
  (*  REPLACES 'IF <COND> THEN INSYMBOL ELSE ERROR(<NUM>)' *)
BEGIN (*ACCEPT*)
  IF SY = X THEN INSYMBOL ELSE ERROR(Y)
END;   (*ACCEPT*)


PROCEDURE TESTFOR(X:SETOFSYS; Y:INTEGER; Z:SETOFSYS);
BEGIN
  IF NOT (SY IN X) THEN
  BEGIN
    ERROR(Y);
    SKIP(X+Z)
  END
END; (*TESTFOR*)

PROCEDURE PUTESD(NAM:ALFA; ETYPE:0..2; CLEAR:BOOLEAN);
BEGIN
  WITH ESD.DATAITEMS(.ESDCNT+1] DO
    BEGIN
      NAME:=NAM;
      ADDRESS := RELOC1*ETYPE;
      IF ETYPE = SD THEN LENGTH := BYTE1SPACE
        ELSE LENGTH := ALLSPACES;
      ESDCNT:=ESDCNT+1;
      ESD.BYTES:=BYTE1SPACE+BYTE2SPACE+ESDCNT*16;
    END;
  IF (ESDCNT = 3) OR CLEAR THEN
  WITH ESD DO
    BEGIN
      ID:=BYTE1SPACE+BYTE2SPACE+ESDID;
      ESDID:=ESDID+ESDCNT;
      ESDCNT:=0; SYSGO@:=CARD(ESD);
      PUT(SYSGO);
    END;
END; (*PUTESD*)


PROCEDURE PUTRLD(R,P,ADDRESS:INTEGER; CLEAR:BOOLEAN);
BEGIN
  WITH RLD.RLDITEMS(.RLDPOS] DO
    BEGIN
      RELPOS:=RELOC2*R+P;
      FLAGADDRESS := RELOC1 * 28 + ADDRESS;
    END;
  RLDPOS:=RLDPOS+1;
  IF (RLDPOS=8) OR CLEAR THEN
    BEGIN
      RLD.BYTES:=BYTE1SPACE+BYTE2SPACE+(RLDPOS-1)*8;
      SYSGO@:=CARD(RLD);
      PUT(SYSGO); RLDPOS:=1;
    END;
END; (* PUTRLD *)


PROCEDURE OBCLEAR;
BEGIN
  WITH TXT DO
    BEGIN
      ADDRESS := BYTE1SPACE + CURRADDRESS;
      LENGTH := BYTE1SPACE+BYTE2SPACE + 4*(OBPOINTER-1);
      ID := 1+BYTE1SPACE+BYTE2SPACE;
      TEXTDATA := OBJECTCODE;
      SYSGO@:=CARD(TXT);
      PUT(SYSGO);
      CURRADDRESS:=CURRADDRESS+ 4*(OBPOINTER-1);
      OBPOINTER := 1;
    END;
END; (* OBCLEAR *)

PROCEDURE DATA1(X:INTEGER);
BEGIN
  OBJECTCODE (.OBPOINTER] := X;
  OBPOINTER := OBPOINTER + 1;
  IF OBPOINTER = OBJLENPL1 THEN OBCLEAR;
END;


PROCEDURE PUTENDC(STARTID,STARTADDR: INTEGER);
  (* WRITE END CARD TO SYSGO AND INITIALIZE FOR NEXT CSECT *)
  (* STARTID=ESDID OF ENTRY POINT OR 0 IF NO ENTRY TO BE GENERATED *)
BEGIN
  WITH ENDC DO
    BEGIN  LENGTH := CURRADDRESS;
      IF STARTID > 0 THEN
        BEGIN (* PUT ENTRY ADDRESS IN END CARD *)
          ADDRESS := STARTADDR + BYTE1SPACE;
          ID := STARTID + BYTE1SPACE + BYTE2SPACE
        END
      ELSE
        BEGIN (* NO ENTRY POINT *)
          ADDRESS := ALLSPACES;  ID := ALLSPACES;
        END
    END;  (* WITH *)
  SYSGO@ := CARD(ENDC);  PUT(SYSGO);
  CURRADDRESS := 0;  ESDCNT := 0;  ESDID := 1
END;  (* PUTENDC *)


PROCEDURE PUTSTARTER;
  (* GENERATES P@START CSECT IN FRONT OF MAIN PROGRAM *)
BEGIN
  IF STARTED THEN ERROR(400);   (* SHOULDN'T GET CALLED TWICE *)
  PUTESD('P@START ',SD,FALSE);
  PUTESD('P@CNTRL ',ER,TRUE);
  DATA1(1492185096);            (*  L  15,8(,15)     X'58F0F008'  *)
  DATA1( 134152192);            (*  BR 15; DC H'0'   X'07FF0000'  *)
  DATA1(         0);            (*  =V(P@CNTRL)                   *)
  OBCLEAR;
  PUTRLD(2,1,8,TRUE);
  PUTENDC(1,0);
  STARTED := TRUE;
END;  (* PUTSTARTER *)

PROCEDURE BLOCK(FSYS: SETOFSYS; FSY: SYMBOL; FPROCP: CTP);
  VAR LSY : SYMBOL; FLABP : LBP; FWPROCS,TLCP : CTP;
      LC1STLOCVAR : INTEGER;   (* LC OF 1ST LOCAL VAR IN BLOCK (FOR UNDEFINE) *)

  PROCEDURE INITSIZE(VAR FSIZE : WBSIZE);
    BEGIN FSIZE.WBLENGTH:=4; FSIZE.BOUNDARY:=4;
    END;

  PROCEDURE ALIGNMENT(VAR COUNTER:INTEGER; UNIT:CELLUNIT);
    BEGIN IF COUNTER MOD UNIT >0 THEN
      COUNTER:=(COUNTER+UNIT) DIV UNIT*UNIT;
    END;

  FUNCTION STRING(FSP:STP) : BOOLEAN;
  BEGIN
    STRING := FALSE;
    IF FSP <> NIL THEN
    WITH FSP@ DO
       IF FORM = ARRAYS THEN
        IF INXTYPE <> NIL THEN
         IF INXTYPE@.RANGETYPE = INTPTR THEN
          IF INXTYPE@.MIN = 1 THEN
           IF AELTYPE <> NIL THEN
            IF AELTYPE@.FORM = PACKDTYPE THEN
             IF AELTYPE@.BASETYPE = CHARPTR THEN
               STRING := TRUE
  END;


  FUNCTION SUBRNGTYPE(FSP: STP): BOOLEAN;
    (* RETURNS TRUE IF FSP IS A SUBRANGE OR PACKED SUBRANGE TYPE *)
  BEGIN
    SUBRNGTYPE := FALSE;
    IF FSP <> NIL THEN
      BEGIN
        IF FSP@.FORM = PACKDTYPE THEN FSP := FSP@.BASETYPE;
        SUBRNGTYPE := FSP@.FORM = SUBRANGE;
      END;
  END;  (* SUBRNGTYPE *)

  FUNCTION COMPTYPES(FSP1,FSP2: STP) : BOOLEAN;
   (*DECIDE WHERHER STRUCT POINTED AT BY FSP1 AND FSP2 ARE COMPATIBLE*)
   VAR NXT1,NXT2: CTP; COMP: BOOLEAN; LP: MARKP;

    FUNCTION EQUALBOUNDS(FSP1,FSP2: STP) : BOOLEAN;
      VAR LMIN1,LMIN2,LMAX1,LMAX2: INTEGER;
      BEGIN GETBOUNDS(FSP1,LMIN1,LMAX1);
            GETBOUNDS(FSP2,LMIN2,LMAX2);
            EQUALBOUNDS := (LMIN1 = LMIN2) AND (LMAX1 = LMAX2)
      END;

  BEGIN (*COMPTYPES*)
    IF FSP1 = FSP2 THEN COMPTYPES := TRUE
    ELSE IF (FSP1=NIL) OR (FSP2=NIL) THEN COMPTYPES:=TRUE
    ELSE
     BEGIN
       IF FSP1@.FORM=PACKDTYPE THEN FSP1:=FSP1@.BASETYPE;
       IF FSP2@.FORM=PACKDTYPE THEN FSP2:=FSP2@.BASETYPE;
       IF FSP1@.FORM=SUBRANGE THEN FSP1:=FSP1@.RANGETYPE;
       IF FSP2@.FORM=SUBRANGE THEN FSP2:=FSP2@.RANGETYPE;
       IF FSP1=FSP2 THEN COMPTYPES:=TRUE ELSE
       IF (FSP1 = NIL) OR (FSP2 = NIL) THEN
         COMPTYPES := FALSE
       ELSE
         IF FSP1@.FORM<>FSP2@.FORM THEN COMPTYPES := FALSE ELSE
           CASE FSP1@.FORM OF
             SCALAR,RECORDS,FILES: COMPTYPES:=FALSE;
             PACKDTYPE,SUBRANGE,TAGFIELD,VARIANT: ERROR(400);
             ARRAYS:
                IF STRING(FSP1) AND STRING(FSP2) THEN
                  COMPTYPES := EQUALBOUNDS(FSP1@.INXTYPE,FSP2@.INXTYPE)
                ELSE COMPTYPES := FALSE;
             POWER: COMPTYPES := (FSP1@.PCKDSET = FSP2@.PCKDSET) AND
                             COMPTYPES(FSP1@.ELSET,FSP2@.ELSET);
             POINTER: COMPTYPES := (FSP1 = NILPTR) OR (FSP2=NILPTR);
           END;
    END
  END (*COMPTYPES*);

  PROCEDURE STRINGTYPE(VAR FSP: STP);
    (*ENTER TYPE OF STRINGCONST (PACKED ARRAY (.1..LGTH] OF CHAR) INTO
     STRUCTURE TABLE*)
    VAR LSP,LSP1: STP;
    BEGIN NEW(LSP,SUBRANGE);
      WITH LSP@ DO
        BEGIN RANGETYPE:=INTPTR;
          MIN := 1; MAX := LGTH; FTYPE := FALSE;
          SIZE.WBLENGTH:=4; SIZE.BOUNDARY:=4;
        END;
      NEW(LSP1,ARRAYS);
      WITH LSP1@ DO
        BEGIN
          AELTYPE := PACKDCHARPTR; INXTYPE := LSP;
          FTYPE:=FALSE; AELLENG:=1;
          SIZE.WBLENGTH:=LGTH; SIZE.BOUNDARY:=1;
        END;
      FSP := LSP1
    END;

    PROCEDURE REVERSE(A:CTP; VAR B:CTP);
      VAR WORK,ANSWER:CTP;
      BEGIN ANSWER:=NIL;
        WHILE A<>NIL DO
          WITH A@ DO
            BEGIN WORK:=NEXT; NEXT:=ANSWER;
                  ANSWER:=A; A:=WORK;
            END;
        B:=ANSWER;
      END;

  (* SET OF CHAR PROCESSING *)
  PROCEDURE FOLDCNCHAR(VAR IVAL:INTEGER);
  TYPE INTSET = SET OF 0..63;
  VAR AUX : INTEGER;
    S : INTSET;
  BEGIN
    S := INTSET( (.CHR(IVAL)] );
    AUX := -1;
    REPEAT AUX := AUX + 1 UNTIL AUX IN S;
    IVAL := AUX
  END; (* FOLDCNCHAR *)

  FUNCTION BYTEPACK(X:STRGFRAG):INTEGER;
    BEGIN BYTEPACK:=INTEGER(X);
    END;

  PROCEDURE BYTEUNPACK(VAR X:STRGFRAG; V:INTEGER);
    VAR W: RECORD CASE BOOLEAN OF
             TRUE: (STR: STRGFRAG);
             FALSE:(INT: INTEGER)
           END;
    BEGIN  W.INT:=V;  X:=W.STR;
    END;

  PROCEDURE SETVALUE(X:BASICSET; VAR I1,I2:INTEGER);
    VAR W: RECORD DUMMY:INTEGER;
             CASE FLAG:BOOLEAN OF
               FALSE: (S: BASICSET);
               TRUE:  (A1,A2: INTEGER)
           END;
    BEGIN W.S:=X; I1:=W.A1; I2:=W.A2; END;

  PROCEDURE HALFWORD(X:INTEGER; VAR X1,X2:INTEGER);
    VAR WORD,HALF: RECORD
                     CASE BOOLEAN OF
                       TRUE:  (I: INTEGER);
                       FALSE: (B: STRGFRAG);
                     END;
    BEGIN
      WORD.I := X;
      HALF.I := WORD.I;
      HALF.B(.1] := 0; HALF.B(.2] := 0;
      X2 := HALF.I;
      HALF.B(.3] := WORD.B(.1]; HALF.B(.4] := WORD.B(.2];
      X1 := HALF.I;
    END;

  PROCEDURE CONSTANT(FSYS: SETOFSYS; VAR FSP: STP; VAR FVALU: VALU);
   VAR LSP: STP; LCP: CTP; SIGN: (NONE,POS,NEG);
       SETTYPE1,SETTYPE2:STP; SETVAL1,SETVAL2:VALU;
       N:INTEGER; NOERROR:BOOLEAN;

    PROCEDURE SETELEMENT(SETTYPE:STP; SETVALUE:VALU);
      VAR X:BOOLEAN;
    BEGIN  X:=FALSE;
      IF COMPTYPES(SETTYPE,CHARPTR) THEN FOLDCNCHAR(SETVALUE.IVAL);
        IF SETTYPE=REALPTR THEN ERROR(109)
          ELSE IF SETTYPE@.FORM > SUBRANGE THEN ERROR(136)
            ELSE IF NOT COMPTYPES(LSP@.ELSET,SETTYPE) THEN ERROR(137)
              ELSE IF (SETVALUE.IVAL < SETMIN) OR (SETVALUE.IVAL > SETMAX) THEN ERROR(304)
                ELSE X:= TRUE;
      NOERROR:=NOERROR AND X;
    END;

  BEGIN LSP := NIL; FVALU.IVAL := 0; FVALU.CKIND:=INT;
   TESTFOR(CONSTBEGSYS,50,FSYS);
   IF SY IN CONSTBEGSYS THEN
    BEGIN
     IF SY = CHARCONST THEN
       BEGIN LSP:=CHARPTR; FVALU.CKIND:=INT; FVALU.IVAL:=IVAL; INSYMBOL END
     ELSE IF SY=STRINGCONST THEN
       BEGIN STRINGTYPE(LSP);
         FVALU.CKIND:=STRG; FVALU.VALP:=CONSTP;
         INSYMBOL
       END
     ELSE IF SY = NILSY THEN
       BEGIN  LSP:=NILPTR;
         FVALU.CKIND:=INT; FVALU.IVAL:=NILVAL;
         INSYMBOL;
       END
     ELSE IF SY=LBRACK THEN
       BEGIN NEW(LSP,POWER);
         WITH LSP@ DO
           BEGIN ELSET:=NIL; PCKDSET:=FALSE; FTYPE:=FALSE;
                 SIZE.WBLENGTH:=8; SIZE.BOUNDARY:=8;
           END;
         FVALU.CKIND:=PSET; FVALU.PVAL:=(.];
         INSYMBOL;
         IF SY=RBRACK THEN INSYMBOL
         ELSE
           BEGIN
             while true do begin NOERROR:=TRUE;
               CONSTANT(FSYS+(.COMMA,COLON,RBRACK],SETTYPE1,SETVAL1);
               SETELEMENT(SETTYPE1,SETVAL1);
               IF SY=COLON THEN
                 BEGIN INSYMBOL; CONSTANT(FSYS+(.COMMA,RBRACK],SETTYPE2,SETVAL2);
                   SETELEMENT(SETTYPE2,SETVAL2);
                   IF NOERROR THEN
                     BEGIN
                       FOR N:=SETVAL1.IVAL TO SETVAL2.IVAL DO
                         FVALU.PVAL:=FVALU.PVAL+(.N];
                       LSP@.ELSET:=SETTYPE1;
                     END;
                 END
               ELSE IF NOERROR THEN
                 BEGIN FVALU.PVAL:=FVALU.PVAL+(.SETVAL1.IVAL];
                       LSP@.ELSET:=SETTYPE1;
                 END;
               IF SY <> COMMA THEN break; INSYMBOL;
             END;
             ACCEPT(RBRACK,12);
           END;
       END
     ELSE
       BEGIN
         SIGN := NONE;
         IF OP IN (.PLUS,MINUS] THEN
           BEGIN IF OP = PLUS THEN SIGN := POS ELSE SIGN := NEG;
             INSYMBOL
           END;
         IF SY = IDENT THEN
           BEGIN SEARCHID((.KONST],LCP);
             WITH LCP@ DO
               BEGIN LSP := IDTYPE; FVALU := VALUES END;
             IF SIGN <> NONE THEN
               IF LSP = INTPTR THEN
                 BEGIN IF SIGN = NEG THEN FVALU.IVAL := -FVALU.IVAL END
               ELSE
                 IF LSP = REALPTR THEN
                   BEGIN
                     IF SIGN = NEG THEN FVALU.RVAL := -FVALU.RVAL
                   END
               ELSE ERROR(105);
             INSYMBOL;
           END
         ELSE
           IF SY = INTCONST THEN
             BEGIN IF SIGN = NEG THEN IVAL := -IVAL;
               LSP:=INTPTR; FVALU.CKIND:=INT; FVALU.IVAL:=IVAL; INSYMBOL
             END
           ELSE
             IF SY = REALCONST THEN
               BEGIN IF SIGN = NEG THEN RVAL := -RVAL;
                 LSP:=REALPTR; FVALU.CKIND:=REEL; FVALU.RVAL:=RVAL; INSYMBOL
               END
             ELSE
               BEGIN ERROR(106); SKIP(FSYS) END
       END;
       TESTFOR(FSYS,6,(. ]);
     END;
   FSP := LSP
  END (*CONSTANT*) ;

  PROCEDURE CONSTEXPRESSION(FSYS:SETOFSYS; VAR P:CEP);
    VAR X1,X2,T,W:CEP; LSP:STP; LVALU:VALU;
    BEGIN X1:=NIL;
      REPEAT INSYMBOL;
        CONSTANT(FSYS+(.COMMA,RCBRACK],LSP,LVALU);
        IF LSP = NIL THEN ERROR(204) ELSE
        IF (LSP@.FORM >= ARRAYS) AND (NOT STRING(LSP)) THEN ERROR(224)
        ELSE
          BEGIN NEW(X2);
            WITH X2@ DO
              BEGIN ELEMTYPE:=LSP; ELEMVALUE:=LVALU;
                   NEXTELEM:=X1;
              END;
            X1:=X2;
          END;
      UNTIL SY <> COMMA;
      T:=NIL;
      WHILE X1 <> NIL DO WITH X1@ DO
        BEGIN W:=NEXTELEM; NEXTELEM:=T;
              T:=X1; X1:=W;
        END;
      P:=T;
      ACCEPT(RCBRACK,255);
      TESTFOR(FSYS,6,(.]);
    END;

  PROCEDURE CONSTIMAGE(FSP:STP; FEP:CEP; VAR FVALU:VALU);
    VAR ERRFLAG:BOOLEAN; ANSWER,WORK,XX:CTAILP; CURRENT:ADDRRANGE;
        BYTEFLAG:BOOLEAN; BYTEPART,BUFFER:STRGFRAG;

    PROCEDURE ERROR1(N:INTEGER);
      BEGIN IF ERRFLAG THEN ERROR(N);
        ERRFLAG:=FALSE;
      END;

    PROCEDURE WORDCONST(V:INTEGER);
      BEGIN NEW(WORK);
        WORK@.NXTCSP:=ANSWER; WORK@.STFR:=V;
        ANSWER:=WORK; CURRENT:=CURRENT+4;
      END;

    PROCEDURE BUFFEROUT;
      BEGIN NEW(WORK);
        WORK@.NXTCSP:=ANSWER; WORK@.STFR:=BYTEPACK(BYTEPART);
        ANSWER:=WORK; BYTEFLAG:=FALSE;
          CURRENT := (CURRENT+3) DIV 4*4;
      END;

    PROCEDURE BYTECONST(V:INTEGER);
      BEGIN BYTEPART(.CURRENT MOD 4+1]:=V;
        BYTEFLAG:=TRUE; CURRENT:=CURRENT+1;
        IF CURRENT MOD 4 = 0 THEN BUFFEROUT;
      END;

    PROCEDURE UNITCONST(DISPL:ADDRRANGE; FSP:STP);
      VAR X:CTAILP; I,A1,A2:INTEGER;
      BEGIN IF FEP=NIL THEN ERROR1(222)
        ELSE IF NOT COMPTYPES(FEP@.ELEMTYPE,FSP) THEN
          BEGIN
            IF NOT COMPTYPES(FSP,NILPTR) THEN ERROR1(223)
          END
        ELSE
          BEGIN
            IF DISPL > CURRENT THEN
              BEGIN IF BYTEFLAG THEN
                   BUFFEROUT;
                IF DISPL > CURRENT THEN WORDCONST(0);
              END;
            IF FSP@.FORM = ARRAYS THEN
              BEGIN X:=FEP@.ELEMVALUE.VALP;
                FOR I:=0 TO FSP@.SIZE.WBLENGTH-1 DO
                  BEGIN IF (I MOD 4) = 0 THEN BYTEUNPACK(BUFFER,X@.STFR);
                        BYTECONST(BUFFER(.I MOD 4+1]);
                        IF (I MOD 4) = 3 THEN X:=X@.NXTCSP;
                  END;
              END
            ELSE IF FSP@.SIZE.WBLENGTH = 1 THEN BYTECONST(FEP@.ELEMVALUE.IVAL)
            ELSE IF (FEP@.ELEMVALUE.CKIND=REEL) OR (FEP@.ELEMVALUE.CKIND=PSET) THEN
              BEGIN SETVALUE(FEP@.ELEMVALUE.PVAL,A1,A2);
                    WORDCONST(A1); WORDCONST(A2);
              END
            ELSE WORDCONST(FEP@.ELEMVALUE.IVAL);
          FEP:=FEP@.NEXTELEM;
        END;
      END;

    PROCEDURE STCONST(DISPL:ADDRRANGE; FSP:STP);
      VAR LMIN,LMAX,I:INTEGER; LCP:CTP;
      BEGIN
        IF FSP <> NIL THEN
          CASE FSP@.FORM OF
            SCALAR,PACKDTYPE,SUBRANGE,POWER:
              UNITCONST(DISPL,FSP);
      POINTER: IF NOT COMPTYPES(NILPTR,FSP) THEN ERROR1(226)
                  ELSE UNITCONST(DISPL,FSP);
      FILES,TAGFIELD,VARIANT:
              ERROR1(226);
            ARRAYS:
              IF STRING(FSP) THEN UNITCONST(DISPL,FSP)
                ELSE
                  BEGIN GETBOUNDS(FSP@.INXTYPE,LMIN,LMAX);
                    FOR I:=LMIN TO LMAX DO
                      BEGIN STCONST(DISPL,FSP@.AELTYPE);
                            DISPL:=DISPL+FSP@.AELLENG;
                      END;
                  END;
            RECORDS:
              IF FSP@.RECVAR <> NIL THEN ERROR1(227)
              ELSE
                BEGIN LCP:=FSP@.FSTFLD;
                  WHILE LCP <> NIL DO
                    BEGIN STCONST(DISPL+LCP@.FLDADDR,LCP@.IDTYPE);
                          LCP:=LCP@.NEXT;
                    END;
                END
          END;
      END;

    BEGIN (*CONSTIMAGE*)
      ERRFLAG:=TRUE; CURRENT:=0;
      ANSWER:=NIL; BYTEFLAG:=FALSE;
      STCONST(0,FSP);
      IF BYTEFLAG THEN BUFFEROUT;
      IF FSP <> NIL THEN
        IF FSP@.SIZE.WBLENGTH > CURRENT THEN WORDCONST(0);
      IF FEP <> NIL THEN ERROR1(222);
      WORK:=NIL;
      WHILE ANSWER <> NIL DO WITH ANSWER@ DO
        BEGIN XX:=NXTCSP; NXTCSP:=WORK; WORK:=ANSWER; ANSWER:=XX; END;
      FVALU.CKIND:=STRG; FVALU.VALP:=WORK;
    END;

  PROCEDURE TYP(FSYS: SETOFSYS; VAR FSP: STP; PACKFLAG:BOOLEAN);
   VAR LSP,LSP1,LSP2: STP; OLDTOP: DISPRANGE; LCP: CTP;
     LMIN,LMAX: INTEGER;
     LFILTYP,TEMPENTERSCOPE: BOOLEAN;
     DISPL: ADDRRANGE;       (*LOCATION COUNTER WITHIN A RECORD*)
     LSIZE: CELLUNIT;       (*BOUNDARY OF THE RECORD*)
     TOOBIG: BOOLEAN; MAXELTS: INTEGER;

    PROCEDURE CHECKPACK(VAR ORG:STP);
      VAR W:STP; XMIN,XMAX:INTEGER;
      BEGIN
        IF ORG<>NIL THEN
          IF (ORG@.FORM=SCALAR) OR (ORG@.FORM=SUBRANGE) THEN
            IF ORG<>INTPTR THEN
              IF ORG<>REALPTR THEN
                BEGIN GETBOUNDS(ORG,XMIN,XMAX);
                  IF (XMIN>=0) AND (XMAX<=255) THEN
                    BEGIN NEW(W,PACKDTYPE);
                      WITH W@ DO
                        BEGIN SIZE.WBLENGTH:=1; SIZE.BOUNDARY:=1;
                              BASETYPE:=ORG; FTYPE:=FALSE;
                        END;
                      ORG:=W;
                    END;
                END;
      END;

    PROCEDURE SIMPLETYPE(FSYS: SETOFSYS; VAR FSP: STP; PACKFLAG:BOOLEAN);
      VAR LSP,LSP1: STP; LCP,LCP1: CTP; TTOP: DISPRANGE;
          LVAL:INTEGER; LVALU:VALU;

      PROCEDURE SUBRNGS(FSP: STP; FVALU: VALU);
      BEGIN NEW(LSP,SUBRANGE);
        WITH LSP@ DO
          BEGIN RANGETYPE:=FSP;
            MIN := FVALU.IVAL; FTYPE := FALSE
          END;
        ACCEPT(COLON,5);
        CONSTANT(FSYS,LSP1,LVALU);
        WITH LSP@ DO
          BEGIN MAX := LVALU.IVAL;
            INITSIZE(SIZE);
            IF FSP <> NIL THEN
              IF NOT COMPTYPES(FSP,LSP1) THEN ERROR(107)
              ELSE IF (FSP=REALPTR) OR (FSP@.FORM>=POWER) THEN
                     BEGIN ERROR(148); RANGETYPE:=NIL; END
                   ELSE IF MIN > MAX THEN ERROR(102);
          END
      END;

    BEGIN (*SIMPLETYPE*)
      TESTFOR(SIMPTYPEBEGSYS,1,FSYS);
      IF SY IN SIMPTYPEBEGSYS THEN
       BEGIN
        IF SY = LPARENT THEN
         BEGIN TTOP := TOP;   (*DECL. CONSTS LOCAL TO INNERMOST BLOCK*)
          WHILE DISPLAY(.TOP].OCCUR <> BLCK DO TOP := TOP -1;
          NEW(LSP,SCALAR,DECLARED);
          WITH LSP@ DO
            BEGIN FTYPE := FALSE;
              FCONST := NIL; INITSIZE(SIZE)
            END;
          LCP1 := NIL; LVAL := -1;
          REPEAT INSYMBOL;
            IF SY = IDENT THEN
              BEGIN NEW(LCP,KONST); LVAL := LVAL + 1;
                WITH LCP@ DO
                  BEGIN NAME := ID; IDTYPE := LSP; NEXT := LCP1;
                        VALUES.CKIND := INT; VALUES.IVAL := LVAL;
                  END;
                ENTERID(LCP);
                LCP1 := LCP; INSYMBOL
              END
            ELSE ERROR(2);
          TESTFOR(FSYS+(.COMMA,RPARENT],6,(.]);
          UNTIL SY <> COMMA;
          LSP@.FCONST:=LCP1;
          TOP := TTOP;
          ACCEPT(RPARENT,4);
         END
        ELSE
          BEGIN
            IF SY = IDENT THEN
              BEGIN SEARCHID((.TYPES,KONST],LCP);
                INSYMBOL;
                WITH LCP@ DO
                  IF (KLASS = KONST) AND (LCP <> UCSTPTR) THEN
                    SUBRNGS(IDTYPE,VALUES)
                  ELSE BEGIN LSP := IDTYPE;
                         IF LSP = ALFAPTR THEN EXTENSION(332);
                       END;
              END (*SY = IDENT*)
            ELSE
              BEGIN CONSTANT(FSYS+(.COLON],LSP1,LVALU);
                SUBRNGS(LSP1,LVALU)
              END;
          END;
        IF PACKFLAG THEN CHECKPACK(LSP);
        FSP:=LSP;
        TESTFOR(FSYS,6,(.]);
       END
        ELSE FSP := NIL
    END (*SIMPLETYPE*);

  PROCEDURE FIELDLIST(FSYS: SETOFSYS; VAR FRECVAR: STP;
             VAR FFSTFLD: CTP; VAR FTYP: BOOLEAN);
             (* FTYP IS TRUE IF  A FIELD OF THE LIST IS OR CONTAINS A FILE *)
    VAR A,LCP,LCP1,NXT,NXT1: CTP; LSP,LSP1,LSP2,LSP3,LSP4: STP;
      SAVEDISPL,MAXDISPL : ADDRRANGE; SAVESIZE,MAXSIZE: CELLUNIT;
      LVALU : VALU; IDSTORE : ALFA;
      LFILTYP: BOOLEAN;

    PROCEDURE FIELDADDRESS(FCP: CTP; FSP: STP);
      BEGIN
        WITH FCP@ DO
          IF FSP = NIL THEN FLDADDR := DISPL
          ELSE WITH FSP@ DO
                 BEGIN
                   ALIGNMENT(DISPL,SIZE.BOUNDARY); FLDADDR := DISPL;
                     DISPL:=DISPL+SIZE.WBLENGTH;
                     IF LSIZE<SIZE.BOUNDARY THEN LSIZE:=SIZE.BOUNDARY;
                 END;
      END;

  BEGIN (*FIELDLIST*)
    NXT1 := NIL; LSP := NIL;
    LSP1 := NIL;
    FTYP := FALSE;
    TESTFOR(FSYS+(.IDENT,CASESY],19,(.]);
    WHILE SY = IDENT DO
     BEGIN
     NXT := NXT1;
      while true do begin
        IF SY = IDENT THEN
          BEGIN
          NEW(LCP,FIELD);
            WITH LCP@ DO
              BEGIN
              NAME:=ID; IDTYPE:=NIL; NEXT:=NXT; END;
            NXT:=LCP; ENTERID(LCP); INSYMBOL;
          END
        ELSE ERROR(2);
        TESTFOR((.COMMA,COLON],6,FSYS+(.SEMICOLON,CASESY]);
        IF SY <> COMMA THEN break; INSYMBOL;
      END; (* LOOP *)
      ACCEPT(COLON,5);
      TYP(FSYS+(.CASESY,SEMICOLON],LSP,PACKFLAG);
      IF LSP <> NIL THEN FTYP := FTYP OR LSP@.FTYPE;
      WHILE NXT <> NXT1 DO
        WITH NXT@ DO
          BEGIN
          IDTYPE := LSP;
            NXT := NEXT;
          END;
      NXT1:=LCP;
      IF SY = SEMICOLON THEN
        BEGIN
        INSYMBOL;
          TESTFOR(FSYS+(.IDENT,CASESY],19,(.]);
        END
     END (*WHILE*) ;
   REVERSE(NXT1,FFSTFLD);
   NXT:=FFSTFLD;
   WHILE NXT <> NIL DO
     BEGIN
     FIELDADDRESS(NXT,NXT@.IDTYPE); NXT:=NXT@.NEXT;
     END;
   IF SY = CASESY THEN
   BEGIN
     NEW(LSP,TAGFIELD);
      WITH LSP@ DO
        BEGIN
        TGFLDP:=NIL; FSTVAR:=NIL;
              FTYPE:=FALSE;
        END;
      FRECVAR := LSP;
      INSYMBOL;
      IF SY = IDENT THEN
       BEGIN
       PRTERR := FALSE; SEARCHID((.TYPES],LCP1); PRTERR := TRUE;
        IDSTORE := ID; INSYMBOL;
        IF SY = COLON THEN LCP1 := NIL;
        NEW(LCP,FIELD);
        WITH LCP@ DO
         BEGIN
         IDTYPE:=NIL; NEXT:=NIL END;
        IF LCP1 = NIL THEN   (*EXPLICIT TAGFIELD*)
         BEGIN
         LCP@.NAME := IDSTORE; ENTERID(LCP);
          ACCEPT(COLON,5);
          IF SY = IDENT THEN SEARCHID((.TYPES],LCP1)
          ELSE
           BEGIN
           ERROR(2); SKIP(FSYS+(.OFSY,LPARENT]);
            LCP1 := NIL
           END; INSYMBOL
         END
        ELSE LCP@.NAME := '        ';
        IF LCP1<>NIL THEN LSP1:=LCP1@.IDTYPE;
        IF PACKFLAG THEN CHECKPACK(LSP1);
        IF LSP1 <> NIL THEN
          BEGIN
            IF LSP1@.FORM > SUBRANGE THEN ERROR(110)
              ELSE IF LSP1 = REALPTR THEN ERROR(109)
              ELSE
                BEGIN
                LSP@.TGFLDP := LCP;
                  WITH LCP@ DO
                    BEGIN
                    IDTYPE := LSP1;
                      IF NAME <> '        ' THEN
                        FIELDADDRESS(LCP,LSP1)
                    END
                END
          END;
       END
      ELSE
       BEGIN
       ERROR(2); SKIP(FSYS+(.OFSY,LPARENT]) END;
      LSP@.SIZE.WBLENGTH := DISPL;
      LSP@.SIZE.BOUNDARY := LSIZE;
      ACCEPT(OFSY,8);
      LSP1 := NIL; SAVEDISPL := DISPL; MAXDISPL := DISPL;
      SAVESIZE := LSIZE; MAXSIZE := LSIZE;
      (*LOOP UNTIL SY <> SEMICOLON:*)
      while true do begin
       IF NOT (SY IN FSYS+(.SEMICOLON]) THEN
         BEGIN LSP2 := NIL;
           while true do begin
           CONSTANT(FSYS+(.COMMA,COLON,LPARENT],LSP3,LVALU);
             IF LSP@.TGFLDP <> NIL THEN
               IF NOT COMPTYPES(LSP@.TGFLDP@.IDTYPE,LSP3) THEN
                 ERROR(111);
             NEW(LSP3,VARIANT);
             WITH LSP3@ DO
               BEGIN NXTVAR := LSP1; SUBVAR := LSP2;
                     VARVAL := LVALU.IVAL;
                     FTYPE:=FALSE;
               END;
             LSP1 := LSP3; LSP2 := LSP3;
             IF SY <> COMMA THEN break; INSYMBOL;
           END;
           ACCEPT(COLON,5);
           ACCEPT(LPARENT,9);
           FIELDLIST(FSYS+(.RPARENT,SEMICOLON],LSP2,LCP,LFILTYP);
           IF LFILTYP THEN BEGIN ERROR(108); FTYP:=TRUE; END;
           IF DISPL > MAXDISPL THEN MAXDISPL:=DISPL;
           IF LSIZE > MAXSIZE THEN MAXSIZE:=LSIZE;
           WHILE LSP3 <> NIL DO
             WITH LSP3@ DO
               BEGIN LSP4 := SUBVAR; SUBVAR := LSP2;
                 SIZE.WBLENGTH:=DISPL; SIZE.BOUNDARY:=LSIZE;
                 FSTVARFLD := LCP;
                 LSP3 := LSP4
               END;
           IF  SY = RPARENT THEN
            BEGIN INSYMBOL;
              TESTFOR(FSYS+(.SEMICOLON],6,(. ]);
            END
           ELSE ERROR(4);
         END (*NOT (SY IN ...*) ;
       IF SY <> SEMICOLON THEN break;
       DISPL:=SAVEDISPL; LSIZE:=SAVESIZE; INSYMBOL;
     END;
      DISPL := MAXDISPL; LSIZE := MAXSIZE;
      LSP@.FSTVAR := LSP1;
    END
    ELSE
     FRECVAR := NIL
  END (*FIELDLIST*) ;

    PROCEDURE FILETYPE;
      VAR COMPONENT,S:STP; TMP:INTEGER;
      BEGIN INSYMBOL;
        ACCEPT(OFSY,8);
        NEW(LSP,FILES);
        WITH LSP@ DO
          BEGIN FILTYPE:=NIL; FTYPE:=TRUE;
                TEXTFILE:=FALSE; SIZE.WBLENGTH:=16; SIZE.BOUNDARY:=4;
          END;
        TYP(FSYS,COMPONENT,PACKFLAG);
        IF COMPONENT <> NIL THEN
          IF COMPONENT@.FTYPE THEN
            BEGIN ERROR(108); COMPONENT:=NIL END;
          IF COMPONENT <> NIL THEN
            WITH LSP@ DO
              BEGIN
                FILTYPE := COMPONENT;
                TEXTFILE := COMPTYPES(COMPONENT,CHARPTR);
                IF TEXTFILE
                  THEN BEGIN SIZE.WBLENGTH:=TEXTSIZE; SIZE.BOUNDARY:=4;
                         IF COMPONENT@.FORM=PACKDTYPE
                           THEN S:=COMPONENT
                           ELSE BEGIN NEW(S,PACKDTYPE);
                                  WITH S@ DO
                                    BEGIN SIZE.WBLENGTH:=1; SIZE.BOUNDARY:=1;
                                      FTYPE:=FALSE; BASETYPE:=COMPONENT;
                                    END;
                                END;
                              FILTYPE:=S;
                       END
                  ELSE BEGIN SIZE.WBLENGTH:=COMPONENT@.SIZE.WBLENGTH+8;
                             SIZE.BOUNDARY:=COMPONENT@.SIZE.BOUNDARY;
                             TMP := SIZE.BOUNDARY;
                             ALIGNMENT(SIZE.WBLENGTH,4); ALIGNMENT(TMP,4);
                       END;
              END;
      END;

  BEGIN (*TYP*)
   LSP := NIL; LCP := NIL;
   TESTFOR(TYPEBEGSYS,10,FSYS);
   IF SY IN TYPEBEGSYS THEN
    BEGIN
     IF SY IN SIMPTYPEBEGSYS THEN SIMPLETYPE(FSYS,LSP,PACKFLAG)
     ELSE
  (*@*)
      IF SY = ARROW THEN
       BEGIN NEW(LSP,POINTER);
        WITH LSP@ DO
         BEGIN ELTYPE := NIL; FTYPE := FALSE;
          INITSIZE(SIZE)
         END;
        INSYMBOL;
        IF SY = IDENT THEN
         BEGIN
          IF NOT POSSFWDREF THEN
           BEGIN
            TEMPENTERSCOPE := ENTERSCOPE;
            ENTERSCOPE := FALSE;
            PRTERR := FALSE;
            SEARCHID((.TYPES],LCP); PRTERR := TRUE;
            ENTERSCOPE := TEMPENTERSCOPE;
           END;
          IF LCP = NIL THEN   (*FORWARD REFERENCED TYPE ID*)
           BEGIN NEW(LCP,TYPES);
            WITH LCP@ DO
             BEGIN NAME := ID; IDTYPE := LSP;
              NEXT := FWPTR
             END;
            FWPTR := LCP
           END
          ELSE
           BEGIN
            IF LCP@.IDTYPE <> NIL THEN
             IF LCP@.IDTYPE@.FTYPE THEN ERROR(108)
               ELSE LSP@.ELTYPE:=LCP@.IDTYPE;
           END;
          INSYMBOL;
         END
        ELSE ERROR(2);
       END
      ELSE
       BEGIN
        IF SY = PACKEDSY THEN
         BEGIN PACKFLAG := TRUE; INSYMBOL END;
         TESTFOR(TYPEDELS,10,FSYS);
  (*ARRAY*)
        IF SY = ARRAYSY THEN
         BEGIN INSYMBOL;
          ACCEPT(LBRACK,11);
          LSP1 := NIL;
          while true do begin NEW(LSP,ARRAYS);
           WITH LSP@ DO
            BEGIN AELTYPE := LSP1; INXTYPE := NIL;
             FTYPE := FALSE; INITSIZE(SIZE)
            END;
           LSP1 := LSP;
           SIMPLETYPE(FSYS+(.COMMA,RBRACK,OFSY],LSP2,FALSE);
           IF LSP2 <> NIL THEN
            IF LSP2@.FORM <= SUBRANGE THEN
             IF LSP2 = INTPTR THEN ERROR(149)
             ELSE
              IF LSP2 = REALPTR THEN ERROR(112)
             ELSE LSP@.INXTYPE := LSP2
            ELSE ERROR(113);
           IF SY <> COMMA THEN break; INSYMBOL;
         END;
          ACCEPT(RBRACK,12);
          ACCEPT(OFSY,8);
          TYP(FSYS,LSP,PACKFLAG);
          (* REVERSE POINTERS, COMPUTE SIZE *)
          (* THE SIZE COMPUTATION INCLUDES A CHECK THAT THE ARRAY   *)
          (* ISN'T TOO LARGE. I.E., AELLENG*(LMAX-LMIN+1) < 16M.    *)
          (* THE COMPUTATION IS SOMEWHAT TRICKY TO PREVENT OVERFLOW *)
          (* FOR BOUNDS OF -MAXINT..MAXINT, 0..MAXINT, -MAXINT..0   *)
          (* AND OTHER PERVERSE THIGS.                              *)
          IF LSP <> NIL THEN
           BEGIN
            REPEAT
             WITH LSP1@ DO
              BEGIN LSP2 := AELTYPE; AELTYPE := LSP;
               FTYPE := LSP@.FTYPE;
               IF INXTYPE <> NIL THEN
                 BEGIN AELLENG := AELTYPE@.SIZE.WBLENGTH;
                   ALIGNMENT(AELLENG,AELTYPE@.SIZE.BOUNDARY); GETBOUNDS(INXTYPE,LMIN,LMAX);
                   MAXELTS := TWO24 DIV AELLENG;
                   IF LMAX > 0
                     THEN TOOBIG := ((LMAX - MAXELTS) + 1) > LMIN
                     ELSE TOOBIG := (LMAX + 1) > (MAXELTS + LMIN);
                   IF TOOBIG THEN
                     BEGIN ERROR(254); SIZE.WBLENGTH := AELLENG END
                   ELSE SIZE.WBLENGTH := AELLENG * (LMAX - LMIN + 1);
                   SIZE.BOUNDARY := AELTYPE@.SIZE.BOUNDARY;
                 END;
              END (* WITH LSP1@ *) ;
             LSP := LSP1; LSP1 := LSP2
            UNTIL LSP1 = NIL
           END (* LSP <> NIL *)
         END
        ELSE
  (*RECORD*)
         IF SY = RECORDSY THEN
          BEGIN INSYMBOL;
           OLDTOP := TOP;
           IF TOP < DISPLIMIT THEN
            BEGIN TOP := TOP + 1;
             WITH DISPLAY(.TOP] DO
              BEGIN FNAME := NIL; OCCUR := REC END
            END
           ELSE ERROR(250);
           DISPL := 0; LSIZE := 1;
           FIELDLIST(FSYS+(.ENDSY],LSP1,LCP,LFILTYP);
           NEW(LSP,RECORDS);
           WITH LSP@ DO
            BEGIN FIELDS := DISPLAY(.TOP].FNAME; FTYPE := LFILTYP;
             FSTFLD := LCP; RECVAR := LSP1;
             SIZE.WBLENGTH := DISPL; SIZE.BOUNDARY := LSIZE;
            END;
           TOP := OLDTOP;
           IF SY = SEMICOLON THEN INSYMBOL;
           ACCEPT(ENDSY,13);
          END
         ELSE
  (*SET*)
         IF SY = SETSY THEN
           BEGIN INSYMBOL;
            ACCEPT(OFSY,8);
            NEW(LSP,POWER);
            WITH LSP@ DO
              BEGIN ELSET:=NIL; PCKDSET:=FALSE; FTYPE:=FALSE;
                    SIZE.WBLENGTH:=8; SIZE.BOUNDARY:=8;
              END;
            SIMPLETYPE(FSYS,LSP1,FALSE);
            IF LSP1 <> NIL THEN
              IF LSP1@.FORM > SUBRANGE THEN ERROR(115)
                ELSE IF LSP1 = REALPTR THEN ERROR(114)
                  ELSE IF LSP1 = INTPTR THEN ERROR(169)
                    ELSE
                      BEGIN GETBOUNDS(LSP1,LMIN,LMAX);
                        IF (LMIN < SETMIN) OR (LMAX > SETMAX) THEN
                          IF NOT COMPTYPES(LSP1,CHARPTR) THEN
                            ERROR(205);
                          LSP@.ELSET := LSP1;
                      END
           END
         ELSE
  (*FILE*) IF SY = FILESY THEN FILETYPE;
       END;
       TESTFOR(FSYS,6,(. ]);
    END;
   FSP := LSP
  END (*TYP*) ;

  PROCEDURE LABELDECLARATION;
    LABEL 1;
    VAR LLP: LBP;
    BEGIN
      REPEAT INSYMBOL;
        IF SY = INTCONST THEN
          BEGIN LLP := FSTLABP;
            WHILE LLP <> FLABP DO
              IF LLP@.LABVAL = IVAL THEN
                BEGIN ERROR(166); GOTO 1 END
              ELSE LLP := LLP@.NEXTLAB;
            NEW(LLP);
            WITH LLP@ DO
              BEGIN LABVAL := IVAL; DEFINED := FALSE; NEXTLAB := FSTLABP;
                    LCNT := 0; FSTOCC := NIL;
              END;
            FSTLABP := LLP;
            IF IVAL > 9999 THEN EXTENSION(338);
        1:  INSYMBOL
          END
        ELSE ERROR(15);
        TESTFOR(FSYS+(.COMMA,SEMICOLON],6,(. ]);
      UNTIL SY <> COMMA;
      IF SY = SEMICOLON THEN INSYMBOL ELSE ERROR(14)
    END (*LABELDECLARATION*) ;

  PROCEDURE CONSTDECLARATION;
    VAR LCP:CTP; LSP:STP; LVALU:VALU; EXPR:CEP;
    BEGIN
      IF SY <> IDENT THEN
        BEGIN ERROR(2); SKIP(FSYS+(.IDENT]) END;
      WHILE SY = IDENT DO
        BEGIN NEW(LCP,KONST);
          WITH LCP@ DO
            BEGIN NAME := ID; IDTYPE := NIL; NEXT := NIL;
            END;
          INSYMBOL;
          IF OP = EQOP THEN INSYMBOL ELSE ERROR(16);
          IF SY = LCBRACK THEN
            BEGIN
              EXTENSION(320);
              CONSTEXPRESSION(FSYS+(.COLON,SEMICOLON],EXPR);
              IF SY = COLON THEN INSYMBOL ELSE ERROR(5);
              TYP(FSYS+(.SEMICOLON]+TYPEDELS,LSP,FALSE);
              CONSTIMAGE(LSP,EXPR,LVALU);
            END
          ELSE CONSTANT(FSYS+(.SEMICOLON],LSP,LVALU);
          ENTERID(LCP);
          LCP@.IDTYPE := LSP; LCP@.VALUES := LVALU;
          IF SY = SEMICOLON THEN
            BEGIN INSYMBOL;
              TESTFOR(FSYS+(.IDENT],6,(. ]);
            END
          ELSE ERROR(14)
        END
    END (*CONSTDECLARATION*) ;

  PROCEDURE UNDEFINED(VAR F: CTP; STRING: PACKED ARRAY(.1..9] OF CHAR);
    BEGIN
      IF F <> NIL THEN
        BEGIN ERROR(117); ENDOFLINE;
          REPEAT NEWLINE;
                 WRITELN(SYSTERM,'>>>':INDENTTY,'UNDEFINED ',
                          STRING,'  ',F@.NAME);
                 WRITELN('>>>':INDENT,'UNDEFINED ',STRING,'  ',F@.NAME);
                 F := F@.NEXT;
          UNTIL F = NIL;
        END;
    END;


  PROCEDURE TYPEDECLARATION;
    VAR LCP,LCP1,LCP2 : CTP;
        TEMPID : IDCLASS; LSP : STP; RESETID : BOOLEAN;
    BEGIN
      IF SY <> IDENT THEN
        BEGIN ERROR(2); SKIP(FSYS+(.IDENT]) END;
      WHILE SY = IDENT DO
        BEGIN NEW(LCP,TYPES);
          WITH LCP@ DO
            BEGIN NAME := ID; IDTYPE := NIL END;
          RESETID := FALSE;
          PRTERR := FALSE;
          ENTERSCOPE := FALSE;
          SEARCHID((.TYPES,KONST],LCP1);
          ENTERSCOPE := TRUE;
          PRTERR := TRUE;
          IF LCP1 <> NIL THEN
            BEGIN
              TEMPID := LCP1@.KLASS;
              LCP1@.KLASS := VARS;
              RESETID := TRUE
            END;
          POSSFWDREF := TRUE;
          INSYMBOL;
          IF OP = EQOP THEN INSYMBOL ELSE ERROR(16);
          TYP(FSYS+(.SEMICOLON],LSP,FALSE);
          POSSFWDREF := FALSE;
          IF RESETID THEN LCP1@.KLASS := TEMPID;
          ENTERID(LCP);
          LCP@.IDTYPE := LSP;
          IF SY = SEMICOLON THEN
            BEGIN INSYMBOL;
              TESTFOR(FSYS+(.IDENT],6,(. ]);
            END
          ELSE ERROR(14)
        END;
      (* HAS ANY FORWARD REFERENCE BEEN SATISFIED: *)
      LCP1 := FWPTR;
      WHILE LCP1 <> NIL DO
        BEGIN
          PRTERR := FALSE; ID := LCP1@.NAME;
          SEARCHID((.TYPES],LCP); PRTERR := TRUE;
          IF LCP <> NIL THEN
            BEGIN
              WITH LCP@ DO
                BEGIN
                  LCP1@.IDTYPE@.ELTYPE := IDTYPE;
                  IF IDTYPE <> NIL THEN
                    IF IDTYPE@.FTYPE THEN
                      BEGIN ERROR(306); ENDOFLINE;
                        WRITELN(SYSTERM,'>>>':INDENTTY,'DOMAIN TYPE: ',NAME);
                        NEWLINE; WRITELN('>>>':INDENT,'DOMAIN TYPE: ',NAME);
                      END;
                END;
              IF LCP1 <> FWPTR THEN
                LCP2@.NEXT := LCP1@.NEXT
              ELSE FWPTR := LCP1@.NEXT
            END
          ELSE LCP2 := LCP1;
          LCP1 := LCP1@.NEXT
        END;
      UNDEFINED(FWPTR,'TYPE-ID  ');
    END (*TYPEDECLARATION*) ;

  PROCEDURE ADDRESS(FCP:CTP);
    BEGIN ALIGNMENT(LC,4);
      WITH FCP@ DO
        IF KLASS = VARS THEN
          IF VKIND = DRCT THEN
            BEGIN IF IDTYPE <> NIL
              THEN BEGIN ALIGNMENT(LC,IDTYPE@.SIZE.BOUNDARY); VADDR:=LC;
                         LC:=VADDR+IDTYPE@.SIZE.WBLENGTH;
                   END
              ELSE BEGIN VADDR:=LC; LC:=LC+4; END
            END
          ELSE BEGIN PARADDR:=LC; LC:=LC+4 END
        ELSE IF (KLASS=PROC) OR (KLASS=FUNC)
         THEN BEGIN PFADDR:=LC; LC:=LC+8 END
          ELSE ERROR(400);
      IF LC > TWO24 THEN
        BEGIN ERROR(254); LC:=0 END;
    END;

  PROCEDURE VARDECLARATION;
    VAR LCP,NXT: CTP; LSP: STP;
    BEGIN NXT := NIL;
      REPEAT
        while true do begin
          IF SY = IDENT THEN
            BEGIN NEW(LCP,VARS);
              WITH LCP@ DO
                BEGIN NAME := ID; NEXT := NXT;
                      CNTRLVAR := FALSE;
                      IDTYPE := NIL; VKIND := DRCT; VLEV := LEVEL
                END;
              ENTERID(LCP); NXT := LCP; INSYMBOL;
            END
          ELSE ERROR(2);
          TESTFOR(FSYS+(.COMMA,COLON]+TYPEDELS,6,(.SEMICOLON]);
          IF SY <> COMMA THEN break; INSYMBOL;
        END;
        ACCEPT(COLON,5);
        TYP(FSYS+(.SEMICOLON]+TYPEDELS,LSP,FALSE);
        WHILE NXT <> NIL DO
          WITH NXT@ DO
            BEGIN IDTYPE := LSP; ADDRESS(NXT);
                 NXT := NEXT
            END;
        IF SY = SEMICOLON THEN
          BEGIN INSYMBOL;
            TESTFOR(FSYS+(.IDENT],6,(. ]);
          END
        ELSE ERROR(14)
      UNTIL (SY <> IDENT) AND NOT (SY IN TYPEDELS);
      UNDEFINED(FWPTR,'TYPE-ID  ');
    END (*VARDECLARATION*) ;

  PROCEDURE VARINITIALIZATION;
   VAR LCP:CTP; LSP:STP; LVALU:VALU; EXPR:CEP;

    PROCEDURE INITDATA(FCP:CTP; FVALU:VALU);
      VAR A1,A2,X:INTEGER; P:CTAILP;
      BEGIN
        CASE FVALU.CKIND OF
         INT : BEGIN
                IF FCP@.IDTYPE <> NIL THEN
                 IF FCP@.IDTYPE@.FORM=SUBRANGE THEN
                   IF (FVALU.IVAL>FCP@.IDTYPE@.MAX) OR
                      (FVALU.IVAL<FCP@.IDTYPE@.MIN) THEN ERROR(303);
                 DATA1(4); DATA1(FCP@.VADDR);
                 DATA1(FVALU.IVAL);
               END;
         REEL,PSET:
               BEGIN SETVALUE(FVALU.PVAL,A1,A2); DATA1(8);
                     DATA1(FCP@.VADDR); DATA1(A1); DATA1(A2);
               END;
         STRG: IF FCP@.IDTYPE <> NIL THEN
                 BEGIN P:=FVALU.VALP; X:=FCP@.IDTYPE@.SIZE.WBLENGTH;
                   ALIGNMENT(X,4); DATA1(X); DATA1(FCP@.VADDR);
                   WHILE P<>NIL DO
                     BEGIN DATA1(P@.STFR); P:=P@.NXTCSP; END;
                 END
        END;
      END;

   BEGIN (*VARINITIALIZATION*)
     IF LEVEL <> 1 THEN
       BEGIN ERROR(220); SKIP(FSYS); END
     ELSE
       BEGIN
         IF SY <> IDENT THEN
           BEGIN ERROR(2); SKIP(FSYS+(.IDENT]); END;
         PUTSTARTER;
         PUTESD('P@MAIN@V',SD,TRUE);
         ESDID:=1;
         DATA1(Z7FE);
         WHILE SY=IDENT DO
           BEGIN SEARCHID((.VARS],LCP); INSYMBOL;
             INITNUMBER:=INITNUMBER+1;
             ACCEPT(BECOMES,51);
             IF SY IN CONSTBEGSYS THEN
               BEGIN CONSTANT(FSYS+(.SEMICOLON],LSP,LVALU);
                 IF COMPTYPES(LSP,LCP@.IDTYPE)
                   THEN INITDATA(LCP,LVALU)
                   ELSE ERROR(221);
               END
             ELSE IF SY=LCBRACK THEN
               BEGIN CONSTEXPRESSION(FSYS+(.SEMICOLON],EXPR);
                 CONSTIMAGE(LCP@.IDTYPE,EXPR,LVALU);
                 INITDATA(LCP,LVALU);
               END
             ELSE BEGIN ERROR(50); SKIP(FSYS+(.SEMICOLON]); END;
             IF SY <> SEMICOLON THEN ERROR(14)
             ELSE BEGIN INSYMBOL;
                    TESTFOR(FSYS+(.IDENT],6,(. ]);
                  END;
           END;
         OBCLEAR; PUTENDC(0,0);
       END;
   END;

  PROCEDURE PROCDECLARATION(FSY: SYMBOL);
   (* 'FSY' WILL BE EITHER 'PROCSY' OR 'FUNCTSY' *)
   VAR OLDLEV: 0..MAXLEVEL; LSY: SYMBOL; LCP,LCP1,COMPARE,SAVE: CTP; LSP: STP;
     FORW: BOOLEAN; OLDTOP: DISPRANGE;
     LLC: ADDRRANGE; LP: MARKP;
     TP: INTEGER;

   PROCEDURE PARAMETERLIST(FSY:SETOFSYS; VAR FPAR:CTP; IDENTER:BOOLEAN);
     VAR LCP,LCP1,LCP2,LCP3: CTP; LSP: STP; LKIND: DRCTINDRCT;

     PROCEDURE DEFINESKELETON(FID:IDCLASS);
       VAR LCP,WORK1: CTP;
           TMPLEVEL: INTEGER;
           LCSAVE: ADDRRANGE;
       BEGIN INSYMBOL;
         IF SY <> IDENT THEN ERROR(2)
         ELSE
           BEGIN LCSAVE:=LC;
             IF FID = PROC THEN BEGIN NEW(LCP,PROC,DECLARED,FORMAL); LC:=64; END
                           ELSE BEGIN NEW(LCP,FUNC,DECLARED,FORMAL); LC:=72; END;
             WITH LCP@ DO
               BEGIN NAME:=ID; IDTYPE:=NIL; NEXT:=LCP1;
                     PFLEV:=LEVEL; PARAMS:=NIL;
                     ASSIGNEDTO:=FALSE; WITHINSCOPE:=FALSE;
               END;
             INSYMBOL;
             TMPLEVEL := LEVEL;
             LEVEL := LEVEL + 1;
             PARAMETERLIST(FSY+(.COMMA,COLON,RPARENT],WORK1,FALSE);
             LCP@.PARAMS := WORK1;
             LEVEL := TMPLEVEL;
             ENTERID(LCP); LCP1:=LCP; LC:=LCSAVE;
           END;
       END;

     BEGIN (*PARAMETERLIST*)
       LCP1:=NIL;
       TESTFOR(FSY+(.LPARENT],7,FSYS);
       IF SY = LPARENT THEN
        BEGIN IF FORW THEN ERROR(119);
         INSYMBOL;
         IF NOT (SY IN (.IDENT,VARSY,PROCSY,FUNCTSY]) THEN
           BEGIN ERROR(7); SKIP(FSYS+(.IDENT,RPARENT]) END;
         WHILE SY IN (.IDENT,VARSY,PROCSY,FUNCTSY] DO
          BEGIN
           IF SY = PROCSY THEN
            BEGIN
             REPEAT
               DEFINESKELETON(PROC);
               TESTFOR(FSYS+(.COMMA,SEMICOLON,RPARENT],7,(. ]);
               IF SY = COMMA THEN ERROR(7);
             UNTIL SY <> COMMA
            END
           ELSE
            BEGIN LCP2 := LCP1; LSP := NIL;
             IF SY = FUNCTSY THEN
              BEGIN
               REPEAT
                 DEFINESKELETON(FUNC);
                 IF NOT (SY IN (.COMMA,COLON]+FSYS) THEN
                   BEGIN ERROR(7); SKIP(FSYS+(.COMMA,SEMICOLON,RPARENT]) END
                 ELSE IF SY = COMMA THEN ERROR(7);
               UNTIL SY <> COMMA;
               IF SY = COLON THEN
                 BEGIN INSYMBOL;
                   IF SY <> IDENT THEN EXTENSION(326);
                   TYP(FSYS+(.SEMICOLON,RPARENT],LSP,FALSE);
                   IF LSP <> NIL THEN
                     IF NOT (LSP@.FORM IN (.SCALAR,SUBRANGE,POINTER]) THEN
                       BEGIN ERROR(120); LCP@.IDTYPE := NIL END;
                 END
               ELSE ERROR(5)
              END
             ELSE
              BEGIN
               IF SY = VARSY THEN BEGIN LKIND := INDRCT; INSYMBOL; END
                 ELSE LKIND := DRCT;
               while true do begin
                IF SY = IDENT THEN
                  BEGIN NEW(LCP,VARS);
                    WITH LCP@ DO
                      BEGIN NAME := ID; IDTYPE := NIL;
                        CNTRLVAR := FALSE;
                        VKIND := LKIND; NEXT := LCP1; VLEV := LEVEL;
                      END;
                    IF IDENTER THEN ENTERID(LCP);
                    LCP1 := LCP; INSYMBOL;
                  END
                ELSE ERROR(2);
                IF NOT (SY IN (.COMMA,COLON]+FSYS) THEN
                  BEGIN ERROR(7); SKIP(FSYS+(.COMMA,SEMICOLON,RPARENT]) END;
                IF SY <> COMMA THEN break; INSYMBOL;
               END; (* LOOP *)
               IF SY = COLON THEN
                 BEGIN INSYMBOL;
                   IF SY <> IDENT THEN EXTENSION(326);
                   TYP(FSYS+(.RPARENT,SEMICOLON],LSP,FALSE);
                   IF LSP <> NIL THEN
                     IF (LKIND = DRCT) AND LSP@.FTYPE THEN ERROR(121);
                 END
               ELSE ERROR(5);
              END;
             LCP3 := LCP1;
             WHILE LCP3 <> LCP2 DO
               BEGIN LCP3@.IDTYPE:=LSP; LCP3:=LCP3@.NEXT; END;
            END;
           IF SY = SEMICOLON THEN
            BEGIN INSYMBOL;
             IF NOT (SY IN FSYS+(.IDENT,VARSY,PROCSY,FUNCTSY]) THEN
               BEGIN ERROR(7); SKIP(FSYS+(.IDENT,RPARENT]) END
            END
          END (*WHILE*) ;
         IF SY = RPARENT THEN
          BEGIN INSYMBOL;
           TESTFOR(FSY+FSYS,6,(. ]);
          END
         ELSE ERROR(4);
         REVERSE(LCP1,LCP3);
         LCP1 := LCP3;
         WHILE LCP1 <> NIL DO
           BEGIN ADDRESS(LCP1); LCP1:=LCP1@.NEXT; END;
         FPAR := LCP3
        END
      ELSE FPAR := NIL
     END (*PARAMETERLIST*) ;

   BEGIN (*PROCDECLARATION*)
     LLC := LC; FORW := FALSE;
     DP := TRUE;
     IF FSY = PROCSY THEN LC := 64 ELSE LC := 72;
     IF SY <> IDENT THEN BEGIN ERROR(2); LCP := UFCTPTR; END
     ELSE
       BEGIN COMPARE := FWPROCS; LCP := NIL;
         WHILE COMPARE <> NIL DO
           BEGIN
             IF ID = COMPARE@.NAME THEN
               BEGIN LCP := COMPARE;
                 IF COMPARE = FWPROCS THEN FWPROCS := COMPARE@.NEXT
                                      ELSE SAVE@.NEXT := COMPARE@.NEXT;
               END;
             SAVE := COMPARE; COMPARE := COMPARE@.NEXT;
           END;
         IF LCP = NIL
           THEN FORW := FALSE
           ELSE
             BEGIN IF LCP@.KLASS = PROC THEN FORW := (FSY = PROCSY)
                                        ELSE FORW := (FSY = FUNCTSY);
                   IF NOT FORW THEN ERROR(160);
             END;
         IF FORW THEN LC := LCP@.LCSAVE
         ELSE
           BEGIN
             IF FSY = PROCSY THEN NEW(LCP,PROC,DECLARED,ACTUAL)
                             ELSE NEW(LCP,FUNC,DECLARED,ACTUAL);
             WITH LCP@ DO
               BEGIN NAME := ID; IDTYPE := NIL; NEXT := NIL; PFLEV := LEVEL; PARAMS := NIL;
                 IF FSY = FUNCTSY THEN
                   BEGIN
                     ASSIGNEDTO := FALSE;
                     WITHINSCOPE := TRUE;
                   END;
                 IF PCNT < MAXPROCFUNC THEN
                   BEGIN
                     PCNT := PCNT + 1;
                     IF (LEVEL = 1) AND EXTRNL THEN
                       BEGIN
                         PUTESD(ID,SD,FALSE);
                         PROCREF := ID; TP := 8;
                         WHILE PROCREF(.TP] = ' ' DO TP := TP - 1;
                         IF TP = 8 THEN PROCREF(.8] := '@'
                           ELSE PROCREF(.TP+1] := '@';
                         PUTESD(PROCREF,ER,TRUE);
                       END;
                   END
                 ELSE BEGIN ERROR(261); PCNT := 1 END;
                 PFCNT := PCNT;
               END;
             ENTERID(LCP);
           END;
         INSYMBOL;
       END;
     OLDLEV := LEVEL; OLDTOP := TOP;
     IF LEVEL < MAXLEVEL THEN LEVEL := LEVEL + 1 ELSE ERROR(251);
     IF TOP >= DISPLIMIT
       THEN ERROR(250)
       ELSE BEGIN TOP := TOP + 1;
              WITH DISPLAY(.TOP] DO
                BEGIN OCCUR := BLCK;
                  IF FORW THEN FNAME := LCP@.PARAMS ELSE FNAME := NIL;
                END;
            END;
     IF FSY = PROCSY THEN
       BEGIN PARAMETERLIST((.SEMICOLON],LCP1,TRUE);
         IF NOT FORW THEN LCP@.PARAMS := LCP1
       END
     ELSE
       BEGIN PARAMETERLIST((.SEMICOLON,COLON],LCP1,TRUE);
         IF NOT FORW THEN LCP@.PARAMS := LCP1;
         IF SY = COLON THEN
           BEGIN INSYMBOL; IF FORW THEN ERROR(122);
             IF SY <> IDENT THEN EXTENSION(326);
             TYP(FSYS+(.SEMICOLON],LSP,FALSE);
             LCP@.IDTYPE := LSP;
             IF LSP <> NIL THEN
               IF NOT (LSP@.FORM IN (.SCALAR,SUBRANGE,POINTER]) THEN
                 BEGIN ERROR(120); LCP@.IDTYPE := NIL END;
           END
         ELSE IF NOT FORW THEN ERROR(123);
       END;
     ACCEPT(SEMICOLON,14);

     IF (SY = IDENT) AND (ID = 'FORWARD ') THEN
       BEGIN IF FORW THEN ERROR(161);
         LCP@.LCSAVE:=LC; LCP@.NEXT:=FWPROCS; FWPROCS:=LCP;
         PROCADDRESS(.PCNT]:=-1;  (* TO AVOID CONFUSING CALLNONSTANDARD *)
         INSYMBOL;
         IF SY = SEMICOLON THEN INSYMBOL ELSE ERROR(14);
         TESTFOR(FSYS,6,(. ]);
       END
     ELSE IF (SY = IDENT) AND ((ID = 'FORTRAN ') OR (ID = 'PASCAL  ') OR
                               (ID = 'EXTERN  ')) THEN
       BEGIN
         EXTENSION(336);
         IF FORW THEN ERROR(162);
         IF EXTRNL AND (LEVEL = 2) THEN ERROR(383);
         IF PCNT < MAXPROCFUNC THEN
           BEGIN
             WITH EXTARRAY(.EXTPROCS] DO
               BEGIN
                  ENAME := LCP@.NAME;
                  ECNT := PCNT;
               END;
             EXTPROCS := EXTPROCS + 1;
             PROCADDRESS(.PCNT] := 1; (* DEFAULT IS EXTERNAL PASCAL *)
             PCNT := PCNT + 1; PROCADDRESS(.PCNT] := 0;
             LCP1 := LCP@.PARAMS;
             WHILE LCP1 <> NIL DO
               BEGIN
                 IF LCP1@.KLASS IN (.PROC,FUNC] THEN ERROR(380);
                 LCP1 := LCP1@.NEXT;
               END;
             IF ID = 'FORTRAN ' THEN
               BEGIN
                 WITH LCP@ DO
                   BEGIN
                     IF KLASS = PROC THEN TP := 2
                       ELSE IF IDTYPE = REALPTR THEN TP := 4
                         ELSE IF COMPTYPES(IDTYPE,INTPTR) OR
                                 COMPTYPES(IDTYPE,BOOLPTR) THEN TP := 3
                           ELSE ERROR(381);
                   END;
                 PROCADDRESS(.PCNT-1] := TP;
               END;
           END
         ELSE BEGIN ERROR(261); PCNT := 1 END;
         INSYMBOL; ACCEPT(SEMICOLON,14); TESTFOR(FSYS,6,(.]);
       END
     ELSE (* PROCEDURE BLOCK *)
       BEGIN MARK(LP);
         REPEAT BLOCK(FSYS,SEMICOLON,LCP);
           IF FSY = FUNCTSY THEN IF NOT LCP@.ASSIGNEDTO THEN ERROR(179);
           LCP@.WITHINSCOPE := FALSE;
           IF SY = SEMICOLON THEN
             BEGIN
               IF (NOT EXTRNL) OR (EXTRNL AND (LEVEL > 2)) THEN
                 BEGIN INSYMBOL;
                   IF NOT (SY IN (.BEGINSY,PROCSY,FUNCTSY]) THEN
                     BEGIN ERROR(6); SKIP(FSYS) END
                 END
             END
           ELSE ERROR(14)
         UNTIL (SY IN (.BEGINSY,PROCSY,FUNCTSY]) OR
                      (EXTRNL AND (LEVEL = 2));
         RELEASE(LP);
       END;
     LEVEL := OLDLEV; TOP := OLDTOP; LC := LLC;
   END (*PROCDECLARATION*) ;

  PROCEDURE BODY(FSYS: SETOFSYS);
    CONST
      ZA=90;     ZAD=106;   ZADR=42;   ZAR=26;
      ZAW=110;   ZBAL=69;   ZBALR=5;   ZBC=71;
      ZBCR=7;    ZBCTR=6;   ZC=89;     ZCD=105;
      ZCDR=41;   ZCL=85;    ZCLC=213;  ZCLR=21;
      ZCR=25;    ZD=93;     ZDD=109;   ZIC=67;
      ZL=88;     ZLA=65;               ZLCDR=35;
      ZLCR=19;   ZLD=104;   ZLM=152;   ZLNDR=33;
      ZLNR=17;   ZLPDR=32;  ZLPR=16;   ZLR=24;
      ZLTDR=34;  ZLTR=18;   ZM=92;     ZMD=108;
      ZMDR=44;   ZMR=28;    ZMVC=210;  ZN=84;
      ZNR=20;    ZO=86;     ZS=91;     ZSD=107;
      ZSDR=43;   ZSLA=139;  ZSLDA=143; ZSLDL=141;
      ZSLL=137;  ZSR=27;    ZSRDA=142; ZSRDL=140;
      ZSRL=136;  ZST=80;    ZSTC=66;   ZSTD=96;
      ZSTM=144;  ZTM=145;   ZX=87;     ZXR=23;
      ZBCT=70;   ZEX=68;
      ZMVI=146;
      ZNC=212;   ZOC=214;   ZXC=215;
      ZLH=72;

      PBASE1=14; R0=0; BASEWORK=9;
      NEWPOINTER=7; STACKPOINTER=8;
      SAVEAREA=64;             (*LENGTH OF SAVEAREA*)
      BTTEXT=1; BTNOGET=64; BTLOCAL=128;  (* FILE BLOCK TYPE BITS *)
      BLKPC=8; BLKPE=12; BLKPS=16;  (* POINTER OFFSETS IN TEXT FILE BLOCK *)
      BLKSTAT=7;            (* FILE STATUS BITS: *)
      BSREAD=4; BSWRITE=8;  (* FILE OPEN FOR READ OR WRITE *)
      BSEOF=1;              (* EOR(F)=TRUE, OR FILE NOT OPEN *)
      PROCBASE=0;

      (* OFFSETS FROM BASE OF THE STACK TO VARIOUS SUBROUTINES AND
         WORK AREAS.  MUST AGREE WITH OFFSETS IN RUNTIME SYSTEM
         STACK MACRO.                                                 *)

      ENTVARPROC = 8;     (* CALL PASSED PROC                         *)
      ENTRETURN  = 88;    (* RETURN FROM PROCEDURE AT LEVELS 2-6      *)
      ENTEXTPASC = 144;   (* CALL EXTERNAL PASCAL PROCEDURE           *)
      ENTEXTFORT = 176;   (* CALL EXTERNAL FORTRAN PROCEDURE          *)
      ENTNOP     = 228;   (* NOP, ST, AND STD OPCODES USED IN FORTRAN *)
      ENTST      = 232;   (* CALLING SEQUENCE                         *)
      ENTSTD     = 236;
      ENTSYS     = 240;   (* CALL RUNTIME SYSTEM                      *)
      ENTUNDPROC = 272;   (* UNDEFINED VARIABLES ON PROCEDURE ENTRY   *)
      ENTUNDER   = 300;   (* UNDEFINED VARIABLE USED IN EXPRESSION    *)
      ENTCKPTR   = 312;   (* CHECK POINTER VALUE                      *)
      ENTCKRANGE = 336;   (* CHECK VALUE IN RANGE                     *)
      ENTOUTRANGE= 352;   (* VALUE OUT OF RANGE                       *)
      ENTCKSUB   = 376;   (* CHECK SUBSCRIPT VALUE                    *)
      ENTOUTSUB  = 392;   (* SUBSCRIPT OUT OF RANGE                   *)
      ENTCKUNWORD= 416;   (* CHECK FOR UNDEFINED WORD                 *)
      ENTCKUNBYTE= 426;   (* CHECK FOR UNDEFINED BYTE                 *)
      ENTCHSET   = 436;   (* V(P@CHSET) IF IT IS LOADED               *)
      ENTIRCONV  = 440;   (* X'4E00000000000000' FOR INT/FLOAT        *)
      ENTUNNORM0 = 448;   (* UNNORMALIZED ZERO FOR TRUNC/ROUND        *)
      ENTUNDEFWD = 456;   (* "UNDEFINED" DOUBLE WORD                  *)
      ENTUNBYTE  = 474;   (* FULLWORD WITH "UNDEFINED" LOW ORDER BYTE *)
      ENTTAIL    = 468;   (* INITIAL NP - ADDR OF TAIL END OF REGION  *)
      ENTWO31    = 472;   (* X'4E00000080000000' =2**31 FOR INT/FLOAT *)

      (* CODES FOR RUNTIME SYSTEM SERVICES.  THESE MUST AGREE WITH
         OFFSETS IN ADDRESS CONSTANT VECTOR P@ADCON IN THE RUNTIME
         SYSTEM ROUTINE P@RUN.                                        *)

      (* STANDARD MATH FUNCTIONS *)
      SYSSIN  = 0;   SYSCOS  = 4;   SYSEXP  = 8;
      SYSSQRT = 12;  SYSLN    = 16;  SYSATAN = 20;

      (* BASIC FILE PROCESSING PROCEDURES *)
      SYSFDEF   = 24;     (* DEFINE FILE ON BLOCK ENTRY               *)
      SYSCLOSCR = 28;     (* CLOSE ON BLOCK EXIT & SCRATCH IF LOCAL   *)
      SYSRESET  = 32;  SYSREWRITE = 36;  SYSEXTEND = 40;  SYSCLOSE = 44;
      SYSGET    = 48;  SYSPUT     = 52;  (* DATA FILE I/O             *)
      SYSTERMINAL=212;    (* TRUE IF FILE IS CONNECTED TO TERMINAL    *)
      SYSUNFIL  = 64;     (* EOF/EOLN UNDEFINED OR FILE CLOSED        *)

      (* TEXT FILE INPUT *)
      SYSRDGETEOL = 56;   (* GET AT END OF BUFFER                     *)
      SYSRDLN     = 68; SYSRDSTRING = 76; SYSRDREAL = 72;
      SYSRDINT    = 60;

      (* TEXT FILE OUTPUT *)
      SYSWRPUTEOL = 96;   (* PUT AT END OF BUFFER                     *)
      SYSWRCHAR   = 84;   (* WRITE CHAR WHEN INLINE CODE ISN'T USED   *)
      SYSWRITELN  = 92; SYSWRPAGE = 100;  SYSWRSTRING = 112;
      SYSWRBOOL   = 80; SYSWRINT  = 88;
      SYSWRE = 104;  SYSWRF = 108;  (* WRITE REAL FLOAT/FIXED FORMAT  *)

      SYSNEW     = 224;   (* NEW, DISPOSE, AND RELEASE WITHOUT        *)
      SYSDISP    = 228;   (* EXECUTION TIME TESTS.                    *)
      SYSRLSE    = 232;
      SYSTNEW    = 236;   (* HEAP STUFF WITH POINTER CHECKS AND       *)
      SYSTDISP   = 240;   (* UNDEFINE OF NEW HEAP BLOCKS              *)
      SYSTRLSE   = 244;

      (* ASSORTED BUILT-IN PROCEDURES *)
      SYSCLOCK   = 148;   SYSTIME   = 152;  SYSDATE = 156;
      SYSCLKLEFT = 196;   (* GET CPU TIME LEFT FOR PROGRAM            *)
      SYSLMOVE   = 140;   SYSLCOMP  = 144; (* COMPARE/MOVE >256 BYTES *)
      SYSMESSAGE = 164;   SYSIPOWER = 160; (* R**I OR I**I            *)
      SYSSETCC   = 180;   (* SET CONDITION CODE                       *)
      SYSGETPARM = 192;   (* GET EXECUTION PARAMETERS                 *)
      SYSSYSTEM  = 208;   (* SUBMIT COMMAND TO SYSTEM                 *)
      SYSTRAPATN = 200;   (* INDICATE PROGRAM WANTS TO PROCESS ATTN   *)
      SYSNATTNS  = 204;   (* NO. ATTENTIONS SINCE LAST CALL           *)

      (* TERMINATION - NORMAL AND OTHERWISE.  MISCL. FUNCTIONS.       *)
      SYSHALT    = 132;   SYSRETURN = 136; (* EXIT FROM MAIN PROGRAM  *)
      SYSCASER   = 116;   (* UNDEFINED CASE LABEL                     *)
      SYSFORER   = 184;   (*FOR CONTROL VARIABLE MODIFIED BY LOOP BODY*)
      SYSFUNCER  = 188;   (* NO VALUE ASSIGNED TO FUNCTION            *)
      SYSLJUMP   = 172;   (* OUT-OF-BLOCK GOTO                        *)

      CONDZ=8; CONDP=2; CONDM=4; CONDNZ=7; CONDNP=13; CONDNM=11;

   TYPE

               (*TO DESCRIBE EXPRESSION CURRENTLY COMPILED*)
               (*******************************************)

      ATTRP = @ ATTR;
      ATTRKIND = (CST,VARBL,EXPR);
      CMP=@TEMPREC;
      TEMPREC=RECORD TEMPADRS:ADDRRANGE; TEMPLNGTH:INTEGER; (* 4 OR 8 *)
                     NEXTTEMP:CMP; TEMPCONT:ATTRP END;
      REGKIND=(SINGLE,DOUBLE,FLOAT);
      EXPRKIND=(REGIST,TEMPORARY);
      ACCESSKIND=(DIRECT,INDIRECT);  (* INDIRECT: INDEXED OR POINTED VARIABLE *)
      REGORTEMP=RECORD CASE REGTEMP:EXPRKIND OF
                             REGIST:(RNO:REGNO);
                             TEMPORARY:(ATEMP:CMP)
                END;
      REGRECORD=RECORD USED:BOOLEAN; REGCONT:ATTRP END;

      ATTR = RECORD TYPTR: STP;
               FOLLOW: ATTRP;
               ICUNCHECK: ADDRRANGE; (* IC AFTER UNDEFINED TEST       *)
               CASE KIND: ATTRKIND OF
                CST:   (CVAL: VALU);
                VARBL: (VADRS:ADDRRANGE;
                        ACCESS:ACCESSKIND; INDEXREG:REGORTEMP;
                        CASE VARKIND:DRCTINDRCT OF
                              DRCT: (VLEVEL:LEVRANGE);
                              INDRCT: (BASELEV:LEVRANGE; BASEADD:ADDRRANGE));
                EXPR:(REXPR:REGORTEMP)
             END;

      CONSTCHAIN=@CONSTCREC;
      CONSTCREC=RECORD SAVECONST:VALU; CCHAIN:LOCOFREF;
                       NEXTCONST:CONSTCHAIN
                END;


          (*  CODE  BUFFERS  *)
          (*******************)


      CODESPTR = @CODESEG;           (* POINTER TO CODE SEGMENT       *)
      CODESEG  = RECORD              (* CODE SEGMENT DESCRIPTOR       *)
                   CASE BOOLEAN OF
                     TRUE : (FULLWORDS:ARRAY(.0..CODEBLCK] OF INTEGER);
                     FALSE: (BYTES    :PACKED ARRAY
                                          (. 0 .. 255 ] OF CHAR )
                 END;                (* OF CODE SEGMENT               *)

   VAR

      REGISTER:ARRAY(.REGNO] OF REGRECORD;
      DISPLEVEL: LEVRANGE; (*NUMBER OF USED DISPLAY REGISTERS, C.F. WITH STATEMENT*)
      GATTRP,ATTRHEAD: ATTRP;
      RINDEX,RBASE:INTEGER;          (* INDEX AND BASE REGISTER NUMBER*)
      EFFADRS:INTEGER;               (* EFFECTIVE ADDRESS             *)
      RMAIN:INTEGER;                 (* WORKING REGISTER NUMBER       *)
      RWORK:REGNO;
      FREETEMP:CMP;
      STACKTOP:INTEGER;
      BOOLFLAG: BOOLEAN;

      CONSTTOP:CONSTCHAIN;
      STACKSIZE:LOCOFREF;
      CODEPTR:ARRAY (.0..95] OF CODESPTR;
      EXTENDEDADDRESS:BOOLEAN;       (* FLAG FOR EXTENDED ADDRESSING  *)
      REG6USED,REG5USED:BOOLEAN;
      PROCPASS:BOOLEAN;       (* SET WHEN FUNC/PROC CALL COMPILED     *)
      PMDPTREXISTS:BOOLEAN;   (* TRUE IF NOP FOR PMD OFFSET GENERATED
                                 AT START OF CODE FOR THIS PROCEDURE  *)

    PROCEDURE ATTRNEW(VAR FATTRP: ATTRP);
      BEGIN
        IF ATTRHEAD = NIL THEN NEW(FATTRP)
        ELSE BEGIN FATTRP:=ATTRHEAD; ATTRHEAD:=ATTRHEAD@.FOLLOW
             END;
        WITH FATTRP@ DO
          BEGIN ICUNCHECK:=0;
            TYPTR:=NIL; KIND:=CST;
          END;
      END;

    PROCEDURE ATTRDISP(FATTRP:ATTRP);

      PROCEDURE TEMPDISP(ATP:CMP);
        BEGIN
          IF ATP@.TEMPCONT = FATTRP THEN
           BEGIN ATP@.NEXTTEMP := FREETEMP; FREETEMP := ATP END;
        END;

    BEGIN
      WITH FATTRP@ DO
        BEGIN
          FOLLOW := ATTRHEAD; ATTRHEAD := FATTRP;
          IF KIND = EXPR THEN
           WITH REXPR DO
            BEGIN
             IF REGTEMP = REGIST THEN
               BEGIN
                 IF REGISTER(.RNO].REGCONT = FATTRP THEN
                  BEGIN REGISTER(.RNO].USED := FALSE;
                   IF FATTRP@.TYPTR <> NIL THEN
                    IF FATTRP@.TYPTR@.FORM = POWER THEN
                     REGISTER(.SUCC(RNO)].USED := FALSE;
                  END
               END
             ELSE TEMPDISP(ATEMP);
            END;
        END;
    END;

    PROCEDURE COPYATTR(SOURCEATTRP,DESTATTRP : ATTRP);

      PROCEDURE COPYREG(R: REGORTEMP);
        BEGIN IF R.REGTEMP = REGIST THEN REGISTER(.R.RNO].REGCONT := DESTATTRP
              ELSE R.ATEMP@.TEMPCONT := DESTATTRP;
        END;

    BEGIN DESTATTRP@ := SOURCEATTRP@;
      WITH SOURCEATTRP@ DO
        BEGIN ICUNCHECK := 0;
          IF KIND = VARBL THEN
            BEGIN IF ACCESS = INDIRECT THEN COPYREG(INDEXREG) END
          ELSE IF KIND = EXPR THEN
            BEGIN COPYREG(REXPR);
              IF TYPTR <> NIL THEN
                IF (TYPTR@.FORM = POWER) AND (REXPR.REGTEMP = REGIST) THEN
                  REGISTER(.SUCC(REXPR.RNO)].REGCONT := DESTATTRP;
            END;
        END;  (* WITH SOURCEATTRP@ *)
    END;


    PROCEDURE EXCATTR(F1ATTRP,F2ATTRP:ATTRP);
      VAR ATTRWORK:ATTRP;
      BEGIN ATTRNEW(ATTRWORK); COPYATTR(F1ATTRP,ATTRWORK);
        COPYATTR(F2ATTRP,F1ATTRP); COPYATTR(ATTRWORK,F2ATTRP); ATTRDISP(ATTRWORK)
      END;

    PROCEDURE RESETG;
      BEGIN ATTRDISP(GATTRP); ATTRNEW(GATTRP);
      END;

    PROCEDURE ERRORRESET(N:INTEGER);
      BEGIN ERROR(N);
            GATTRP@.TYPTR:=NIL;
            GATTRP@.ICUNCHECK:=0;
      END;

    PROCEDURE MAKECODE(LOC,HALF:INTEGER);
      VAR
        LOCSEG : CODESPTR;
        N      : 0..CODEPERSEG;
        DUMMY  : RECORD
                   CASE BOOLEAN OF
                     TRUE:(A: PACKED ARRAY (.1..4] OF CHAR);
                     FALSE:(X:INTEGER)
                 END;

    BEGIN (* MAKECODE *)
      LOCSEG := CODEPTR(. LOC DIV CODEPERSEG ];  (* PICK UP SEGMENT *)
      IF LOCSEG = NIL THEN   (* PERHAPS NOT CREATED YET *)
        BEGIN
          NEW(LOCSEG,TRUE);
          CODEPTR(. LOC DIV CODEPERSEG ] := LOCSEG;
        END;   (* NEW SEGMENT NOW CREATED *)
      N := LOC MOD CODEPERSEG;
      DUMMY.X:=HALF;      (* NOW PICK UP HALF WORD *)
      LOCSEG@.BYTES(. N ] := DUMMY.A(. 3 ];
      LOCSEG@.BYTES(. N + 1 ] := DUMMY.A(. 4 ];
    END; (* MAKECODE *)


    FUNCTION GETCODE(LOC : INTEGER):INTEGER;
      VAR
        DUMMY : RECORD
                  CASE BOOLEAN OF
                    TRUE : (INT:INTEGER);
                    FALSE: (CH : PACKED ARRAY(.1..4] OF CHAR)
                END;
        LOCPTR : CODESPTR;
        X      : INTEGER;
    BEGIN (*GETCODE*)
      LOCPTR := CODEPTR(. LOC DIV CODEPERSEG ];
      DUMMY.INT := 0;
      X := LOC MOD CODEPERSEG;
      DUMMY.CH(. 3 ] := LOCPTR@.BYTES(.X];
      DUMMY.CH(. 4 ] := LOCPTR@.BYTES(.X+1];
      GETCODE := DUMMY.INT;
    END; (* GETCODE *)

    PROCEDURE GENRX(OP,REG,INDEX,BASE,ADDR:INTEGER);
      BEGIN
        IF IC >= 4096*(7-LEVEL)-2 THEN
        BEGIN ERROR(253); IC:=0 END;
        IF (BASE=14) AND (ADDR >= 4096) THEN
        BEGIN EXTENDEDADDRESS:=TRUE; BASE:=LEVEL END;
        MAKECODE(IC,256*OP+16*REG+INDEX);
        MAKECODE(IC+2,4096*BASE+ADDR);
        IC:=IC+4; BOOLFLAG:=FALSE;
      END;

    PROCEDURE GENRXP(OP:INTEGER; R:REGNO; INDEX,BASE,ADDR:INTEGER);
      BEGIN GENRX(OP,REALREG(.R],INDEX,BASE,ADDR);
      END;

    PROCEDURE GENRR(OP,R1,R2:INTEGER);
        BEGIN
          IF IC >= 4096*(7-LEVEL) THEN
          BEGIN
            ERROR(253); IC:=0
          END;
          MAKECODE(IC,256*OP+16*R1+R2);
          IC:=IC+2; BOOLFLAG:=FALSE;
        END;

    PROCEDURE GENRRP1(OP:INTEGER; R:REGNO);
      BEGIN GENRR(OP,REALREG(.R],REALREG(.R]);
      END;

    PROCEDURE GENRRP(OP:INTEGER; R1,R2:REGNO);
      BEGIN GENRR(OP,REALREG(.R1],REALREG(.R2]);
      END;

    PROCEDURE GENSS(OP,L,R1,D1,R2,D2:INTEGER);
      BEGIN
        IF IC >= 4096*(7-LEVEL)-4 THEN
        BEGIN ERROR(253); IC:=0 END;
        IF (R2=14) AND (D2 >= 4096) THEN
        BEGIN EXTENDEDADDRESS:=TRUE; R2:=LEVEL END;
        MAKECODE(IC,256*OP+L);
        MAKECODE(IC+2,4096*R1+D1);
        MAKECODE(IC+4,4096*R2+D2);
        IC:=IC+6; BOOLFLAG:=FALSE;
      END;

    PROCEDURE CALLRTSYS(SCODE:INTEGER);
      (* CALL RUNTIME SYSTEM. SCODE=SERVICE SCODE. *)
      (* SOME CALLERS OF THIS PROCEDURE DEPEND ON IT GENERATING
         EXACTLY SIX BYTES OF CODE.                *)
      BEGIN
        GENRX(ZBAL,BASEWORK,0,1,ENTSYS);
        MAKECODE(IC,SCODE); IC:=IC+2
      END;

    PROCEDURE INSERTIC(FCIX:ADDRRANGE);
      VAR BASE : INTEGER;
      BEGIN
        IF IC >= 4096 THEN
          BEGIN BASE:=LEVEL; EXTENDEDADDRESS:=TRUE
          END
        ELSE BASE:=PBASE1;
        MAKECODE(FCIX+2,4096*BASE+IC);
      END;

    PROCEDURE INSERTCHAIN(CHAIN:LOCOFREF);
      BEGIN
        WHILE CHAIN <> NIL DO
          WITH CHAIN@ DO
            BEGIN INSERTIC(LOC); CHAIN:=NXTREF; END;
      END;

    PROCEDURE LINKOCC(VAR FPTR: LOCOFREF; FCIX: ADDRRANGE);
      VAR LOCP: LOCOFREF;
      BEGIN NEW(LOCP);
        WITH LOCP@ DO
          BEGIN NXTREF:=FPTR; LOC:=FCIX; END;
        FPTR:=LOCP;
      END;

    PROCEDURE MAKECONSTANT(X:VALU);
      LABEL 1;
      VAR EQUAL:BOOLEAN; P,Q:CTAILP; C:CONSTCHAIN;
      BEGIN C:=CONSTTOP;
        WHILE C <> NIL DO
          WITH C@ DO
            BEGIN
              IF SAVECONST.CKIND = X.CKIND THEN
                BEGIN CASE X.CKIND OF
                  INT: EQUAL:=(X.IVAL=SAVECONST.IVAL);
                  REEL:EQUAL:=(X.RVAL=SAVECONST.RVAL);
                  PSET:EQUAL:=(X.PVAL=SAVECONST.PVAL);
                  STRG:BEGIN EQUAL:=TRUE;
                         P:=X.VALP; Q:=SAVECONST.VALP;
                         WHILE EQUAL AND (P<>NIL) AND (Q<>NIL) DO
                           BEGIN EQUAL:=(P@.STFR=Q@.STFR);
                             P:=P@.NXTCSP; Q:=Q@.NXTCSP;
                           END;
                         EQUAL:=EQUAL AND (P=Q);
                       END
                  END;
                  IF EQUAL THEN
                    BEGIN LINKOCC(CCHAIN,IC); GOTO 1; END;
                END;
              C:=C@.NEXTCONST;
            END;
         NEW(C);
         WITH C@ DO
           BEGIN SAVECONST:=X; CCHAIN:=NIL;
                 NEXTCONST:=CONSTTOP; LINKOCC(CCHAIN,IC);
           END;
         CONSTTOP:=C;
   1: END;

    PROCEDURE MAKEINTCONST(N:INTEGER);
      VAR X:VALU;
      BEGIN X.CKIND:=INT; X.IVAL:=N;
            MAKECONSTANT(X);
      END;

    PROCEDURE GETTEMP(LENGTH:INTEGER; VAR X:CMP);
      LABEL 1,2;
      VAR P,Q:CMP;
      BEGIN Q:=NIL; P:=FREETEMP;
        WHILE P <> NIL DO
          IF P@.TEMPLNGTH=LENGTH THEN GOTO 1
          ELSE BEGIN Q:=P; P:=P@.NEXTTEMP END;
        NEW(P);
        ALIGNMENT(LC,LENGTH);
        P@.TEMPADRS:=LC; LC:=LC+LENGTH;
        P@.TEMPLNGTH:=LENGTH; GOTO 2;
     1: IF Q=NIL THEN FREETEMP:=P@.NEXTTEMP
                 ELSE Q@.NEXTTEMP:=P@.NEXTTEMP;
     2: X:=P;
      END;

    PROCEDURE DELETETEMP(X:CMP);
      BEGIN X@.NEXTTEMP:=FREETEMP; FREETEMP:=X;
      END;

    FUNCTION USING(R:REGNO; FATTRP:ATTRP):BOOLEAN; (*CHECK IF R IS OCCUPIED BY FATTRP *)
      BEGIN
        IF (FATTRP=NIL) OR (NOT REGISTER(.R].USED) THEN USING:=FALSE
        ELSE
          BEGIN WITH FATTRP@ DO CASE KIND OF
            CST:   USING:=FALSE;
            VARBL: IF ACCESS=INDIRECT THEN IF INDEXREG.REGTEMP=REGIST
                       THEN USING:=(R=INDEXREG.RNO)
                      ELSE USING:=FALSE
                     ELSE USING:=FALSE;
            EXPR:  IF REXPR.REGTEMP=REGIST
                      THEN USING:=(REGISTER(.R].REGCONT=FATTRP)
                      ELSE USING:=FALSE
            END;
          END;
      END;

    PROCEDURE DISPLACEMENT(ADRS:INTEGER; VAR REM:INTEGER);
      VAR I:INTEGER;
      BEGIN
        IF ADRS >= 0 THEN I:=ADRS DIV 4096*4096
                     ELSE I:=((ADRS+1) DIV 4096-1)*4096;
        MAKEINTCONST(I); REM:=ADRS-I;
      END;

    PROCEDURE BASEREGISTER(LEVEL:LEVRANGE; ADRS:ADDRRANGE);
      BEGIN
        IF (ADRS>=4096) OR (ADRS<0) THEN
          BEGIN DISPLACEMENT(ADRS,EFFADRS); GENRX(ZL,BASEWORK,0,0,0);
                IF LEVEL<>0 THEN GENRR(ZAR,BASEWORK,LEVEL);
                RBASE:=BASEWORK;
          END
        ELSE BEGIN EFFADRS:=ADRS; RBASE:=LEVEL; END;
      END;

    PROCEDURE SAVE(R:REGNO);
      LABEL 1;
      VAR TEMP:CMP;
      BEGIN IF REGISTER(.R].USED THEN
        BEGIN
          IF R>=F0 THEN
            BEGIN GETTEMP(8,TEMP);
                  BASEREGISTER(LEVEL,TEMP@.TEMPADRS);
                  GENRXP(ZSTD,R,0,RBASE,EFFADRS)
            END
          ELSE
           BEGIN  WITH REGISTER(.R].REGCONT@ DO
            IF TYPTR <> NIL THEN
              IF (TYPTR@.FORM=POWER) AND (KIND=EXPR) THEN
                BEGIN (* SAVE A SET *)
                  GETTEMP(8,TEMP);
                  BASEREGISTER(LEVEL,TEMP@.TEMPADRS);
                  GENRXP(ZSTM,R,REALREG(.SUCC(R)],RBASE,EFFADRS);
                  REGISTER(.SUCC(R)].USED:=FALSE;
                  GOTO 1;
                END;
               (* ELSE SAVE SINGLE REGISTER--NOT POWERSET *)
                     GETTEMP(4,TEMP);
                     BASEREGISTER(LEVEL,TEMP@.TEMPADRS);
                     GENRXP(ZST,R,0,RBASE,EFFADRS)
           END;
       1: ;
          REGISTER(.R].USED:=FALSE;
          TEMP@.TEMPCONT:=REGISTER(.R].REGCONT;
          WITH REGISTER(.R].REGCONT@ DO
            IF KIND=EXPR THEN
              BEGIN REXPR.REGTEMP:=TEMPORARY;
                    REXPR.ATEMP:=TEMP
              END
            ELSE BEGIN INDEXREG.REGTEMP:=TEMPORARY;
                       INDEXREG.ATEMP:=TEMP
                 END
        END;
      END;

    PROCEDURE REGSEARCH(FATTRP:ATTRP; T:REGKIND);
      LABEL 1;
      VAR TWORK : REGNO;
      BEGIN CASE T OF
        SINGLE: BEGIN FOR TWORK:=R10 TO R13 DO
                  IF NOT REGISTER(.TWORK].USED THEN BEGIN
                     RWORK:=TWORK; GOTO 1 END;
                  FOR TWORK := R10 TO R13 DO
                    IF NOT USING(TWORK,FATTRP) THEN BEGIN
                      RWORK:=TWORK; SAVE(RWORK); GOTO 1 END;
                      ERROR(400);
                END;
              FLOAT: BEGIN FOR TWORK := F0 TO F6 DO
                       IF NOT REGISTER(.TWORK].USED THEN
                         BEGIN RWORK:=TWORK; GOTO 1 END;
                       FOR TWORK:=F0 TO F6 DO
                         IF NOT USING(TWORK,FATTRP) THEN
                           BEGIN RWORK:=TWORK; SAVE(RWORK); GOTO 1 END;
                       ERROR(400);
                     END;
        DOUBLE: IF NOT(REGISTER(.R10].USED OR REGISTER(.R11].USED) THEN RWORK:=R10
                  ELSE IF NOT(REGISTER(.R12].USED OR REGISTER(.R13].USED) THEN RWORK:=R12
                  ELSE IF NOT(USING(R10,FATTRP) OR USING(R11,FATTRP)) THEN
                           BEGIN SAVE(R10); SAVE(R11); RWORK:=R10 END
                  ELSE BEGIN SAVE(R12); SAVE(R13); RWORK:=R12 END
            END;
     1: RMAIN:=REALREG(.RWORK];
      END;

    PROCEDURE LOADINDEX(F1ATTRP,F2ATTRP:ATTRP);
      BEGIN
        WITH F1ATTRP@ DO
          BEGIN
            IF ACCESS=DIRECT THEN RINDEX:=0
              ELSE IF INDEXREG.REGTEMP=REGIST THEN
                BEGIN REGISTER(.INDEXREG.RNO].USED:=FALSE;
                      RINDEX:=REALREG(.INDEXREG.RNO]; RWORK:=INDEXREG.RNO;
                END
              ELSE WITH INDEXREG.ATEMP@ DO
                BEGIN REGSEARCH(F2ATTRP,SINGLE); BASEREGISTER(LEVEL,TEMPADRS);
                      GENRX(ZL,RMAIN,0,RBASE,EFFADRS);
                      RINDEX:=RMAIN; DELETETEMP(INDEXREG.ATEMP);
                END;
          END;
      END;

    PROCEDURE LOADBASE(FATTRP:ATTRP);
      BEGIN
        WITH FATTRP@ DO
          BEGIN
            IF VARKIND=DRCT
              THEN BASEREGISTER(VLEVEL,VADRS)
              ELSE
                BEGIN
                  BASEREGISTER(BASELEV,BASEADD);
                  GENRX(ZL,BASEWORK,0,RBASE,EFFADRS);
                  RBASE:=BASEWORK;
                  IF (VADRS>=4096) OR (VADRS<0) THEN
                    BEGIN DISPLACEMENT(VADRS,EFFADRS); GENRX(ZA,BASEWORK,0,0,0); END
                  ELSE EFFADRS:=VADRS;
                END;
          END;
      END;


    PROCEDURE LOADINTCONST(REG:0..15; VAL:INTEGER);
      BEGIN
        IF VAL=0
          THEN GENRR(ZXR,REG,REG)
          ELSE IF (VAL>0) AND (VAL<4096)
            THEN GENRX(ZLA,REG,0,0,VAL)
            ELSE BEGIN MAKEINTCONST(VAL);
                       GENRX(ZL,REG,0,0,0);
                 END;
      END;

    PROCEDURE LOAD(F1ATTRP,F2ATTRP:ATTRP);
      VAR RKIND:REGKIND; LOADOP:INTEGER;
      BEGIN WITH F1ATTRP@ DO
        BEGIN
          IF (KIND<>EXPR) OR (REXPR.REGTEMP<>REGIST) THEN
            BEGIN
              IF TYPTR@.FORM=POWER THEN RKIND:=DOUBLE
                ELSE IF COMPTYPES(TYPTR,REALPTR)
                  THEN BEGIN RKIND:=FLOAT; LOADOP:=ZLD; END
                  ELSE BEGIN RKIND:=SINGLE; LOADOP:=ZL; END;
              CASE KIND OF
          CST:   BEGIN REGSEARCH(F2ATTRP,RKIND);
                   IF RKIND=SINGLE
                     THEN LOADINTCONST(RMAIN,CVAL.IVAL)
                     ELSE BEGIN MAKECONSTANT(CVAL);
                            IF RKIND=DOUBLE THEN GENRX(ZLM,RMAIN,RMAIN+1,0,0)
                                            ELSE GENRX(LOADOP,RMAIN,0,0,0);
                          END;
                 END;
          VARBL: BEGIN LOADINDEX(F1ATTRP,F2ATTRP); REGSEARCH(F2ATTRP,RKIND);
                   LOADBASE(F1ATTRP);
                   IF RKIND=DOUBLE THEN
                     BEGIN
                       IF RINDEX=0 THEN
                          GENRX(ZLM,RMAIN,RMAIN+1,RBASE,EFFADRS)
                       ELSE IF RINDEX=RMAIN
                              THEN BEGIN GENRX(ZL,RMAIN+1,RINDEX,RBASE,EFFADRS+4);
                                         GENRX(ZL,RMAIN,RINDEX,RBASE,EFFADRS);
                                   END
                              ELSE BEGIN GENRX(ZL,RMAIN,RINDEX,RBASE,EFFADRS);
                                         GENRX(ZL,RMAIN+1,RINDEX,RBASE,EFFADRS+4);
                                   END
                     END
                   ELSE IF TYPTR@.SIZE.WBLENGTH=1
                     THEN BEGIN GENRX(ZIC,RMAIN,RINDEX,RBASE,EFFADRS);
                                MAKEINTCONST(255); GENRX(ZN,RMAIN,0,0,0);
                          END
                     ELSE GENRX(LOADOP,RMAIN,RINDEX,RBASE,EFFADRS);
                 END;
          EXPR:  BEGIN REGSEARCH(F2ATTRP,RKIND);
                   BASEREGISTER(LEVEL,REXPR.ATEMP@.TEMPADRS);
                   IF RKIND=DOUBLE
                     THEN GENRX(ZLM,RMAIN,RMAIN+1,RBASE,EFFADRS)
                     ELSE GENRX(LOADOP,RMAIN,0,RBASE,EFFADRS);
                   DELETETEMP(REXPR.ATEMP);
                 END
              END; (*CASE*)
              KIND:=EXPR; REXPR.REGTEMP:=REGIST; REXPR.RNO:=RWORK;
              REGISTER(.RWORK].USED:=TRUE; REGISTER(.RWORK].REGCONT:=F1ATTRP;
              IF RKIND=DOUBLE THEN
                BEGIN REGISTER(.SUCC(RWORK)].USED:=TRUE;
                      REGISTER(.SUCC(RWORK)].REGCONT:=F1ATTRP;
                END;
            END;
        END;
      END;

    PROCEDURE LOADEVENODD(F1ATTRP,F2ATTRP:ATTRP; SWITCH:INTEGER); (*SWITCH 0: EVEN, 1: ODD*)
      BEGIN WITH F1ATTRP@ DO
        BEGIN CASE KIND OF
          CST:  BEGIN REGSEARCH(F2ATTRP,DOUBLE);
                      LOADINTCONST(RMAIN+SWITCH,CVAL.IVAL);
                END;
          VARBL:BEGIN LOADINDEX(F1ATTRP,F2ATTRP); REGSEARCH(F2ATTRP,DOUBLE);
                  LOADBASE(F1ATTRP);
                  IF TYPTR@.SIZE.WBLENGTH=1
                    THEN BEGIN GENRX(ZIC,RMAIN+SWITCH,RINDEX,RBASE,EFFADRS);
                               MAKEINTCONST(255); GENRX(ZN,RMAIN+SWITCH,0,0,0);
                         END
                    ELSE GENRX(ZL,RMAIN+SWITCH,RINDEX,RBASE,EFFADRS);
                END;
          EXPR: IF REXPR.REGTEMP=REGIST
                  THEN BEGIN REGISTER(.REXPR.RNO].USED:=FALSE;
                             REGSEARCH(F2ATTRP,DOUBLE);
                             IF RMAIN+SWITCH<>REALREG(.REXPR.RNO] THEN
                               GENRR(ZLR,RMAIN+SWITCH,REALREG(.REXPR.RNO]);
                       END
                  ELSE BEGIN REGSEARCH(F2ATTRP,DOUBLE);
                             BASEREGISTER(LEVEL,REXPR.ATEMP@.TEMPADRS);
                             GENRX(ZL,RMAIN+SWITCH,0,RBASE,EFFADRS);
                             DELETETEMP(REXPR.ATEMP);
                       END
              END; (*CASE*)
              IF SWITCH=1 THEN RWORK:=SUCC(RWORK);
              KIND:=EXPR; REXPR.REGTEMP:=REGIST; REXPR.RNO:=RWORK;
              REGISTER(.RWORK].USED:=TRUE; REGISTER(.RWORK].REGCONT:=F1ATTRP;
        END;
      END;

    PROCEDURE LOADADDRESS(F1ATTRP,F2ATTRP:ATTRP);
     VAR SWITCH:BOOLEAN;
     BEGIN
       SWITCH:=FALSE;
       WITH F1ATTRP@ DO
        CASE KIND OF
          EXPR: ERROR(400);
          CST: BEGIN REGSEARCH(F2ATTRP,SINGLE); MAKECONSTANT(CVAL);
                     GENRX(ZLA,RMAIN,0,0,0);
               END;
          VARBL:BEGIN LOADINDEX(F1ATTRP,F2ATTRP);
                  IF RINDEX=0
                    THEN
                      BEGIN REGSEARCH(F2ATTRP,SINGLE);
                            IF VARKIND=DRCT
                              THEN BEGIN IF VLEVEL=0 THEN ERROR(400)
                                     ELSE
                                       IF (VADRS<4096) AND (VADRS>0) THEN
                                       BEGIN SWITCH:=TRUE;
                                         GENRX(ZLA,RMAIN,0,VLEVEL,VADRS)
                                       END ELSE GENRR(ZLR,RMAIN,VLEVEL);
                                   END
                              ELSE
                                BEGIN BASEREGISTER(BASELEV,BASEADD);
                                      GENRX(ZL,RMAIN,0,RBASE,EFFADRS);
                                END;
                      END
                    ELSE
                      BEGIN RMAIN:=RINDEX;
                        IF VARKIND=DRCT THEN
                          BEGIN IF VLEVEL<>0 THEN GENRR(ZAR,RMAIN,VLEVEL); END
                        ELSE BEGIN BASEREGISTER(BASELEV,BASEADD);
                                   GENRX(ZA,RMAIN,0,RBASE,EFFADRS);
                             END;
                      END;
                 IF (VADRS<>0) AND (NOT SWITCH) THEN
                     BEGIN MAKEINTCONST(VADRS); GENRX(ZA,RMAIN,0,0,0); END;
                END
        END; (*CASE*)
       WITH F1ATTRP@ DO
         BEGIN TYPTR:=INTPTR; KIND:=EXPR;
               REXPR.REGTEMP:=REGIST; REXPR.RNO:=RWORK;
         END;
       REGISTER(.RWORK].USED:=TRUE; REGISTER(.RWORK].REGCONT:=F1ATTRP;
     END;

    PROCEDURE NEEDRTPROC(NAME : ALFA);
      (* ADD NAME TO TABLE OF RUNTIME SYSTEM ENTRY POINTS REQUIRED FOR THE
         PROGRAM.  USED TO FORCE LOADING OF ITEMS REFERENCED BY WXTRNS IN
         THE RUNTIME SYSTEM. *)

    VAR  I : INTEGER;
    BEGIN
      RTEXTRN(.NRTEXTRN+1] := NAME;  I := 1;
      WHILE RTEXTRN(.I] <> NAME DO I := I + 1;
      IF I > NRTEXTRN THEN
        BEGIN (* ADD NEW NAME TO TABLE *)
          NRTEXTRN := NRTEXTRN + 1;
          IF NRTEXTRN >= NRTEXTP1 THEN ERROR(400)  (* TABLE OVERFLOW *)
        END
    END; (* NEEDRTPROC *)


    PROCEDURE FOLDANYCHAR(CHATTRP: ATTRP);
    VAR I,REGNO : INTEGER;
    BEGIN
     WITH CHATTRP@ DO
       IF (TYPTR<>NIL) AND COMPTYPES(TYPTR,CHARPTR) THEN
         IF KIND = CST THEN FOLDCNCHAR(CVAL.IVAL)
         ELSE
         BEGIN (* EXPR OR VAR *)
           IF KIND = VARBL THEN LOAD(CHATTRP,NIL);
           (* CONVERT TO AN EXPRESSION *)
           REGNO := REALREG(.REXPR.RNO];
           NEEDRTPROC('P@CHSET ');          (* TRANSLATE TBL *)
           GENRX(ZL,BASEWORK,0,1,ENTCHSET);
           GENRX(ZIC,REGNO,BASEWORK,REGNO,0);
         END
    END (* FOLDANY CHAR *);

    PROCEDURE SETOPERATION(F1ATTRP,F2ATTRP:ATTRP; OPRX1,OPRR1,OPRX2,OPRR2:INTEGER);

      PROCEDURE SETOP1(OPRX,OPRR:INTEGER);
        VAR A1,A2:INTEGER;
        BEGIN
          WITH F2ATTRP@ DO CASE KIND OF
            CST:   BEGIN SETVALUE(CVAL.PVAL,A1,A2); MAKEINTCONST(A1);
                     GENRXP(OPRX,F1ATTRP@.REXPR.RNO,0,0,0);
                     IF OPRX=ZCL THEN GENRX(ZBC,CONDNZ,0,PBASE1,IC+8);
                     MAKEINTCONST(A2); GENRXP(OPRX,SUCC(F1ATTRP@.REXPR.RNO),0,0,0);
                   END;
            VARBL: BEGIN GENRXP(OPRX,F1ATTRP@.REXPR.RNO,RINDEX,RBASE,EFFADRS);
                     IF OPRX=ZCL THEN GENRX(ZBC,CONDNZ,0,PBASE1,IC+8);
                     GENRXP(OPRX,SUCC(F1ATTRP@.REXPR.RNO),RINDEX,RBASE,EFFADRS+4);
                   END;
            EXPR:  IF REXPR.REGTEMP=REGIST
                     THEN BEGIN GENRRP(OPRR,F1ATTRP@.REXPR.RNO,REXPR.RNO);
                            IF OPRX=ZCL THEN GENRX(ZBC,CONDNZ,0,PBASE1,IC+6);
                            GENRRP(OPRR,SUCC(F1ATTRP@.REXPR.RNO),SUCC(REXPR.RNO));
                          END
                     ELSE BEGIN GENRXP(OPRX,F1ATTRP@.REXPR.RNO,0,RBASE,EFFADRS);
                            IF OPRX=ZCL THEN GENRX(ZBC,CONDNZ,0,PBASE1,IC+8);
                            GENRXP(OPRX,SUCC(F1ATTRP@.REXPR.RNO),0,RBASE,EFFADRS+4);
                          END
          END;
        END; (* SETOP1 *)

      BEGIN (* SETOPERATION *)
        WITH F2ATTRP@ DO
          IF KIND=VARBL THEN
            BEGIN LOADINDEX(F2ATTRP,F1ATTRP); LOADBASE(F2ATTRP); END
          ELSE IF KIND=EXPR THEN
            IF REXPR.REGTEMP<>REGIST THEN
              BASEREGISTER(LEVEL,REXPR.ATEMP@.TEMPADRS);
        SETOP1(OPRX1,OPRR1);
        IF OPRX2<>0 THEN SETOP1(OPRX2,OPRR2);
      END; (* SETOPERATION *)

    PROCEDURE OPERATION(F1ATTRP,F2ATTRP:ATTRP; OPRX,OPRR:INTEGER);
      BEGIN
        WITH F2ATTRP@ DO
          BEGIN
            IF KIND=VARBL THEN IF TYPTR@.SIZE.WBLENGTH=1
              THEN LOAD(F2ATTRP,F1ATTRP);
            CASE KIND OF
             CST:   BEGIN MAKECONSTANT(CVAL);
                      GENRXP(OPRX,F1ATTRP@.REXPR.RNO,0,0,0)
                    END;
             VARBL: BEGIN LOADINDEX(F2ATTRP,F1ATTRP); LOADBASE(F2ATTRP);
                      GENRXP(OPRX,F1ATTRP@.REXPR.RNO,RINDEX,RBASE,EFFADRS);
                    END;
             EXPR: IF REXPR.REGTEMP=REGIST
                     THEN GENRRP(OPRR,F1ATTRP@.REXPR.RNO,REXPR.RNO)
                     ELSE BEGIN BASEREGISTER(LEVEL,REXPR.ATEMP@.TEMPADRS);
                                GENRXP(OPRX,F1ATTRP@.REXPR.RNO,0,RBASE,EFFADRS)
                          END
            END; (*CASE*)
          END;
        END; (* OPERATION *)


    PROCEDURE INTTOREAL(FATTRP : ATTRP);
       (* CONVERT FATTRP FROM INTEGER TO REAL *)
    VAR R: INTEGER;
      BEGIN
        LOAD(FATTRP,NIL);
        R:=REALREG(.FATTRP@.REXPR.RNO];
        GENRX(ZX,R,0,1,ENTWO31+4);       (* ADD 2**31 (FLIP SIGN BIT) *)
        GENRX(ZST,R,0,1,ENTIRCONV+4);    (* CONVERT TO FLOATING       *)
        REGSEARCH(NIL,FLOAT);
        GENRX(ZLD,RMAIN,0,1,ENTIRCONV);
        GENRX(ZSD,RMAIN,0,1,ENTWO31);    (* SUBTRACT 2**31 & NORMALIZE*)
        WITH FATTRP@ DO
          BEGIN REGISTER(.REXPR.RNO].USED:=FALSE;
                TYPTR:=REALPTR; REXPR.RNO:=RWORK;
          END;
        REGISTER(.RWORK].USED:=TRUE; REGISTER(.RWORK].REGCONT:=FATTRP;
      END;

     (* IN THE FOLLOWING ROUTINES CONSIDER THE RULES:
        - IF A ROUTINE HAS TWO ARGUMENTS 'F1ATTRP' AND 'F2ATTRP', THE ARGUMENTS
          HAVE TO BE TAKEN IN THIS ORDER. THE DESCRIPTION OF THE RESULT IS
          ALWAYS TO BE PUT IN 'F2ATTRP@'.
        - IF A ROUTINE HAS ONE ARGUMENT 'FATTRP' THE DESCRIPTION OF THE RESULT
          HAS TO REPLACE THE DESCRIPTION OF THE ARGUMENT IN 'FATTRP@' *)

    PROCEDURE INTARITH(F1ATTRP,F2ATTRP:ATTRP; FOP:OPERATOR);
      VAR X:INTEGER;
      BEGIN
        IF FOP IN (.PLUS,MUL] THEN
          IF F2ATTRP@.KIND=EXPR THEN
            IF F2ATTRP@.REXPR.REGTEMP=REGIST THEN EXCATTR(F1ATTRP,F2ATTRP);
          CASE FOP OF
            PLUS:  BEGIN X:=0; LOAD(F1ATTRP,F2ATTRP); END;
            MINUS: BEGIN X:=ZS-ZA; LOAD(F1ATTRP,F2ATTRP); END;
            MUL:   BEGIN X:=ZM-ZA; LOADEVENODD(F1ATTRP,F2ATTRP,1); END;
            IDIV,IMOD: BEGIN X:=ZD-ZA; LOADEVENODD(F1ATTRP,F2ATTRP,0);
                             GENRXP(ZSRDA,F1ATTRP@.REXPR.RNO,0,0,32);
                       END
          END;
          OPERATION(F1ATTRP,F2ATTRP,ZA+X,ZAR+X);
          IF FOP=MUL THEN
            BEGIN
              IF (F2ATTRP@.KIND=EXPR) AND (F2ATTRP@.REXPR.REGTEMP=REGIST)
                THEN MAKECODE(IC-2,GETCODE(IC-2)-16)
                ELSE MAKECODE(IC-4,GETCODE(IC-4)-16);
              IF DEBUG THEN
                WITH F1ATTRP@.REXPR DO  (* CHECK OVERFLOW *)
                  BEGIN GENRXP(ZSLDA,PRED(RNO),0,0,32);
                    REGISTER(.PRED(RNO)] := REGISTER(.RNO];
                    REGISTER(.RNO].USED := FALSE;
                    RNO := PRED(RNO);
                  END;
            END;
          IF FOP=IDIV THEN
            WITH F1ATTRP@.REXPR DO
              BEGIN REGISTER(.SUCC(RNO)]:=REGISTER(.RNO];
                    REGISTER(.RNO].USED:=FALSE; RNO:=SUCC(RNO);
              END;
          EXCATTR(F1ATTRP,F2ATTRP);
        END;

    PROCEDURE REALARITH(F1ATTRP,F2ATTRP: ATTRP; FOP: OPERATOR);
      VAR X:INTEGER;
      BEGIN
        IF COMPTYPES(F1ATTRP@.TYPTR,INTPTR) THEN INTTOREAL(F1ATTRP);
        IF COMPTYPES(F2ATTRP@.TYPTR,INTPTR) THEN INTTOREAL(F2ATTRP);
        IF FOP IN (.PLUS,MUL] THEN
          IF F2ATTRP@.KIND=EXPR THEN
            IF F2ATTRP@.REXPR.REGTEMP=REGIST THEN EXCATTR(F1ATTRP,F2ATTRP);
        LOAD(F1ATTRP,F2ATTRP);
        CASE FOP OF
          PLUS:  X:=0;
          MINUS: X:=ZSD-ZAD;
          MUL:   X:=ZMD-ZAD;
          RDIV:  X:=ZDD-ZAD
        END;
        OPERATION(F1ATTRP,F2ATTRP,ZAD+X,ZADR+X);
        EXCATTR(F1ATTRP,F2ATTRP);
      END;

    PROCEDURE SETARITH(F1ATTRP,F2ATTRP: ATTRP; FOP: OPERATOR);
      VAR X:INTEGER;
      BEGIN
        IF FOP<>MINUS THEN
          BEGIN IF F2ATTRP@.KIND=EXPR THEN
              IF F2ATTRP@.REXPR.REGTEMP=REGIST THEN EXCATTR(F1ATTRP,F2ATTRP);
            IF FOP=MUL THEN X:=0 ELSE X:=ZO-ZN;
            LOAD(F1ATTRP,F2ATTRP);
            SETOPERATION(F1ATTRP,F2ATTRP,ZN+X,ZNR+X,0,0);
            EXCATTR(F1ATTRP,F2ATTRP);
          END
        ELSE (*FOP=MINUS*)
          BEGIN LOAD(F2ATTRP,F1ATTRP);
            SETOPERATION(F2ATTRP,F1ATTRP,ZN,ZNR,ZX,ZXR);
          END;
      END;

    PROCEDURE NEGATE(FATTRP: ATTRP);
      BEGIN
        WITH FATTRP@ DO
          IF KIND=CST
            THEN IF COMPTYPES(TYPTR,INTPTR)
              THEN CVAL.IVAL:=-CVAL.IVAL
              ELSE CVAL.RVAL:=-CVAL.RVAL
            ELSE
              BEGIN LOAD(FATTRP,NIL);
                IF COMPTYPES(TYPTR,INTPTR)
                  THEN GENRRP1(ZLCR,REXPR.RNO)
                  ELSE GENRRP1(ZLCDR,REXPR.RNO);
              END;
      END;

    PROCEDURE NOTFACTOR(FATTRP: ATTRP);
      BEGIN
        LOAD(FATTRP,NIL);
        IF BOOLFLAG THEN
          MAKECODE(IC-6,256*ZBC+240 -(GETCODE(IC-6) MOD 256))
        ELSE BEGIN MAKEINTCONST(1); GENRXP(ZX,FATTRP@.REXPR.RNO,0,0,0); END;
      END;


    PROCEDURE BOOLARITH(F1ATTRP,F2ATTRP: ATTRP; FOP: OPERATOR);
      VAR X:INTEGER;
      BEGIN
        IF F2ATTRP@.KIND=EXPR THEN EXCATTR(F1ATTRP,F2ATTRP);
        LOAD(F1ATTRP,F2ATTRP);
        IF FOP=ANDOP THEN X:=0 ELSE X:=ZO-ZN;
        OPERATION(F1ATTRP,F2ATTRP,ZN+X,ZNR+X);
        EXCATTR(F1ATTRP,F2ATTRP);
      END;

    PROCEDURE BOOLVALUE(REG,TRUECOND: INTEGER);
      BEGIN GENRX(ZLA,REG,0,0,1);
            GENRX(ZBC,TRUECOND,0,PBASE1,IC+6);
            GENRR(ZXR,REG,REG); BOOLFLAG:=TRUE;
      END;

    PROCEDURE RELINT(F1ATTRP,F2ATTRP: ATTRP; FOP: OPERATOR);
      BEGIN
        IF F2ATTRP@.KIND=EXPR THEN
          BEGIN FOP:=DUALOP(.FOP]; EXCATTR(F1ATTRP,F2ATTRP);
          END;
        LOAD(F1ATTRP,F2ATTRP);
        OPERATION(F1ATTRP,F2ATTRP,ZC,ZCR);
        BOOLVALUE(REALREG(.F1ATTRP@.REXPR.RNO],BMASK(.FOP]);
        F1ATTRP@.TYPTR:=BOOLPTR;
        EXCATTR(F1ATTRP,F2ATTRP);
      END;

    PROCEDURE RELREAL(F1ATTRP,F2ATTRP: ATTRP; FOP: OPERATOR);
      BEGIN
        IF F2ATTRP@.KIND=EXPR THEN
          BEGIN FOP:=DUALOP(.FOP]; EXCATTR(F1ATTRP,F2ATTRP); END;
        LOAD(F1ATTRP,F2ATTRP);
        OPERATION(F1ATTRP,F2ATTRP,ZCD,ZCDR);
        REGISTER(.F1ATTRP@.REXPR.RNO].USED:=FALSE;
        REGSEARCH(NIL,SINGLE);
        BOOLVALUE(RMAIN,BMASK(.FOP]);
        WITH F1ATTRP@ DO
          BEGIN TYPTR:=BOOLPTR; REXPR.RNO:=RWORK; END;
        REGISTER(.RWORK].USED:=TRUE; REGISTER(.RWORK].REGCONT:=F1ATTRP;
        EXCATTR(F1ATTRP,F2ATTRP);
      END;

    PROCEDURE INPOWER(F1ATTRP,F2ATTRP: ATTRP);
      BEGIN
        LOAD(F2ATTRP,F1ATTRP); LOAD(F1ATTRP,F2ATTRP);
        GENRXP(ZSLDL,F2ATTRP@.REXPR.RNO,0,REALREG(.F1ATTRP@.REXPR.RNO],0);
        MAKEINTCONST(63);
        GENRXP(ZCL,F1ATTRP@.REXPR.RNO,0,0,0);
        GENRX(ZBC,CONDNP,0,PBASE1,IC+6);
        GENRRP1(ZXR,F2ATTRP@.REXPR.RNO);
        GENRRP1(ZLTR,F2ATTRP@.REXPR.RNO);
        EXCATTR(F1ATTRP,F2ATTRP);
        BOOLVALUE(REALREG(.F2ATTRP@.REXPR.RNO],CONDM);
        F2ATTRP@.TYPTR:=BOOLPTR;
      END;

    PROCEDURE RELPOWER(F1ATTRP,F2ATTRP: ATTRP; FOP: OPERATOR);
      BEGIN
        IF FOP=LEOP THEN BEGIN EXCATTR(F1ATTRP,F2ATTRP); FOP:=GEOP END
          ELSE IF FOP<>GEOP THEN
            IF F2ATTRP@.KIND=EXPR THEN
              IF F2ATTRP@.REXPR.REGTEMP=REGIST THEN EXCATTR(F1ATTRP,F2ATTRP);
        LOAD(F1ATTRP,F2ATTRP);
        IF FOP=GEOP THEN SETOPERATION(F1ATTRP,F2ATTRP,ZN,ZNR,ZCL,ZCLR)
                    ELSE SETOPERATION(F1ATTRP,F2ATTRP,ZCL,ZCLR,0,0);
        IF FOP=NEOP
          THEN BOOLVALUE(REALREG(.F1ATTRP@.REXPR.RNO],CONDNZ)
          ELSE BOOLVALUE(REALREG(.F1ATTRP@.REXPR.RNO],CONDZ);
        REGISTER(.SUCC(F1ATTRP@.REXPR.RNO)].USED:=FALSE;
        F1ATTRP@.TYPTR:=BOOLPTR; EXCATTR(F1ATTRP,F2ATTRP);
      END;

    PROCEDURE LONGOPERATION(F1ATTRP,F2ATTRP:ATTRP; OPSS,SCODE:INTEGER);
      VAR LENGTH,BR1,BR2,DISPL1,DISPL2: INTEGER;
          COMPARE:BOOLEAN;

      PROCEDURE SSOPERAND(F1ATTRP,F2ATTRP:ATTRP; VAR BR,DISPL:INTEGER);
        VAR VARFLAG:BOOLEAN;
        BEGIN WITH F1ATTRP@ DO
          BEGIN VARFLAG:=TRUE;
            LOADINDEX(F1ATTRP,F2ATTRP);
            IF RINDEX=0 THEN
              BEGIN IF VARKIND <> DRCT THEN
                BEGIN REGSEARCH(F2ATTRP,SINGLE); BASEREGISTER(BASELEV,BASEADD);
                      GENRX(ZL,RMAIN,0,RBASE,EFFADRS); VARFLAG:=FALSE;
                END;
              END
            ELSE BEGIN RMAIN:=RINDEX; VARFLAG:=FALSE;
                   IF VARKIND <> DRCT THEN
                     BEGIN BASEREGISTER(BASELEV,BASEADD); GENRX(ZA,RMAIN,0,RBASE,EFFADRS); END
                   ELSE IF VLEVEL <> 0 THEN GENRR(ZAR,RMAIN,VLEVEL);
                 END;
            IF (VADRS >= 4096) OR (VADRS < 0) THEN
              BEGIN IF VARFLAG THEN
                BEGIN REGSEARCH(F2ATTRP,SINGLE); VARFLAG:=FALSE; GENRR(ZLR,RMAIN,VLEVEL); END;
                  DISPLACEMENT(VADRS,VADRS); GENRX(ZA,RMAIN,0,0,0);
              END;
            DISPL:=VADRS;
            IF VARFLAG THEN BR:=VLEVEL
              ELSE BEGIN TYPTR:=INTPTR; KIND:=EXPR; REXPR.REGTEMP:=REGIST;
                         REXPR.RNO:=RWORK; REGISTER(.RWORK].USED:=TRUE;
                         REGISTER(.RWORK].REGCONT:=F1ATTRP; BR:=RMAIN;
                   END;
          END;
        END; (* SSOPERAND *)

      BEGIN  (* LONGOPERATION *)
       LENGTH:=F1ATTRP@.TYPTR@.SIZE.WBLENGTH;
       COMPARE:=FALSE;
       IF LENGTH > 0 THEN
        IF LENGTH <= 256 THEN
          BEGIN
            WITH F1ATTRP@ DO CASE KIND OF
              EXPR:  ERROR(400);
              CST:   BEGIN COMPARE:=TRUE; BR1:=0; DISPL1:=0 END;
              VARBL: SSOPERAND(F1ATTRP,F2ATTRP,BR1,DISPL1)
            END; (*CASE*)
            WITH F2ATTRP@ DO CASE KIND OF
              EXPR:  ERROR(400);
              CST:   BEGIN IC:=IC+2; MAKECONSTANT(CVAL); IC:=IC-2;
                       BR2:=0; DISPL2:=0;
                     END;
              VARBL: SSOPERAND(F2ATTRP,F1ATTRP,BR2,DISPL2);
            END; (*CASE*)
            IF COMPARE THEN MAKECONSTANT(F1ATTRP@.CVAL);
            GENSS(OPSS,LENGTH-1,BR1,DISPL1,BR2,DISPL2);
          END
        ELSE
          BEGIN LOADADDRESS(F1ATTRP,F2ATTRP); LOADADDRESS(F2ATTRP,F1ATTRP);
            LOADINTCONST(R0,256*LENGTH+16*REALREG(.F1ATTRP@.REXPR.RNO]+REALREG(.F2ATTRP@.REXPR.RNO]);
            CALLRTSYS(SCODE);
          END;
      END; (* LONGOPERATION *)


    PROCEDURE ASSIGNLONG(F1ATTRP,F2ATTRP:ATTRP);
      BEGIN
        LONGOPERATION(F1ATTRP,F2ATTRP,ZMVC,SYSLMOVE)
      END;

    PROCEDURE RELLONG(F1ATTRP,F2ATTRP:ATTRP; FOP:OPERATOR);
      BEGIN
        LONGOPERATION(F1ATTRP,F2ATTRP,ZCLC,SYSLCOMP);
        IF F2ATTRP@.KIND <> EXPR THEN
          IF F1ATTRP@.KIND = EXPR THEN EXCATTR(F1ATTRP,F2ATTRP)
            ELSE BEGIN REGSEARCH(NIL,SINGLE);
                   WITH F2ATTRP@ DO
                     BEGIN KIND:=EXPR; REXPR.REGTEMP:=REGIST;
                           REXPR.RNO:=RWORK;
                     END;
                   REGISTER(.RWORK].USED:=TRUE; REGISTER(.RWORK].REGCONT:=F2ATTRP;
                 END;
        BOOLVALUE(REALREG(.F2ATTRP@.REXPR.RNO],BMASK(.FOP]);
        F2ATTRP@.TYPTR:=BOOLPTR;
      END;

    (* GENERAL NOTES ON UNDEFINED AND RANGE CHECKS:
       SOME CARE IS TAKEN TO AVOID GENERATING REDUNDANT RANGE AND UNDEFINED
       CHECKS OF THE SAME VARIABLE.
       IF A RANGE TEST OR POINTER TEST IS NEEDED, AN "UNDEFINED" TEST IS
       REDUNDANT. TO PREVENT EXCESS TESTS, THE LOC. COUNTER AT THE END
       OF A UNDEFINED TEST (WHICH IS ALWAYS DONE WHEN A VARIABLE IS USED AS
       A FACTOR IN AN EXPRESSION) IS RECORDED IN THE ATTR RECORD.  IF A RANGE OR
       POINTER TEST IS DONE NEXT, AND THE CURRENT IC IS THE ONE GIVEN IN THE
       ATTR, THEN THE IC IS BACKED UP TO ELIMINATE THE USELESS UNDEFINED TEST.
       CHR, PRED, SUCC, AND THE TYPE CONVERSION FUNCTION CALL ALLOWUNDEFINED
       TO SET THIS ADDRESS IN THE ATTR RECORD TO ZERO, WHICH MAKES SURE THE
       RANGE TEST WON'T BE BYPASSED (SINCE THESE FUNCTIONS CAN OTHERWISE
       GENERATE A VALUE OUTSIDE A SCALAR TYPE).  *)


    PROCEDURE CHECKUNDEFINED(FATTRP: ATTRP);
      (* GENERATE CODE TO CHECK IF VARIABLE FATTRP IS UNDEFINED. *)
      (* NO CHECK IS DONE IF THE VARIABLE IS A COMPONENT OF A    *)
      (* PACKED ARRAY AND THE "UNDEFINED" BYTE VALUE IS A LEGAL  *)
      (* VALUE.  THIS PREVENTS MOST SPURIOUS ERROR STOPS.        *)
      LABEL 1;  (* EXIT IF NO CHECK SHOULD BE DONE *)
      VAR  LMIN,LMAX,ENTRY: INTEGER;
    BEGIN
      IF VARCHECK THEN
        WITH FATTRP@ DO
          IF (KIND = VARBL) AND (TYPTR <> NIL) THEN
            IF TYPTR@.FORM <= POINTER THEN
              BEGIN
                IF TYPTR@.SIZE.WBLENGTH = 1 THEN
                  BEGIN GETBOUNDS(TYPTR,LMIN,LMAX);
                    IF (UNDEFBYTE >= LMIN) AND (UNDEFBYTE <= LMAX) THEN GOTO 1;
                    ENTRY := ENTCKUNBYTE;
                  END
                ELSE ENTRY := ENTCKUNWORD;
                LOAD(FATTRP,NIL);
                IF TYPTR = REALPTR THEN
                  BEGIN
                    GENRR(ZBALR,BASEWORK,0);
                    GENRXP(ZCD,REXPR.RNO,0,1,ENTUNDEFWD);
                    GENRX(ZBC,CONDZ,0,1,ENTUNDER);
                  END
                ELSE
                  BEGIN
                    GENRR(ZLR,R0,REALREG(.REXPR.RNO]);
                    GENRX(ZBAL,BASEWORK,0,1,ENTRY);
                    ICUNCHECK := IC;
                  END;
              END;
 1: END;  (* CHECKUNDEFINED *)

    PROCEDURE ALLOWUNDEFINED(FATTRP: ATTRP);
      (* IF THE LAST CODE GENERATED WAS A UNDEFINED CHECK ON THE VARIABLE
         FATTRP, BACK IC UP TO ELIMINATE THE CHECK.  THIS DEPENDS ON THE
         CHECK BEING EXACTLY 6 BYTES LONG.  *)
    BEGIN
      WITH FATTRP@ DO
        BEGIN
          IF ICUNCHECK = IC THEN IC := IC - 6;
          ICUNCHECK := 0;  (* DO ALWAYS TO FORCE RANGE TEST *)
        END;
    END;  (* ALLOWUNDEFINED *)


    PROCEDURE CHECKREGISTER(R,FMIN,FMAX,FENTCHECK,FENTERROR:INTEGER);
    BEGIN
      IF R <> R0 THEN GENRR(ZLR,R0,R);  (* VALUE MUST BE IN R0 *)
      IF (FMIN >= -32768) AND (FMIN <= 32767) AND
         (FMAX >= -32768) AND (FMAX <= 32767) THEN
        BEGIN (* LIMITS FIT IN HALF WORD.  USE COMPACT RANGE TEST *)
          GENRX(ZBAL,BASEWORK,0,1,FENTCHECK);
          MAKECODE(IC,FMIN); MAKECODE(IC+2,FMAX); IC:=IC+4;
        END
      ELSE
        BEGIN  (* FULL WORD COMPARE FOR LARGER LIMITS  *)
          GENRR(ZBALR,BASEWORK,0);  (* ADDRESS OF TEST *)
          MAKEINTCONST(FMIN); GENRX(ZC,R0,0,0,0);
          GENRX(ZBC,CONDM,0,1,FENTERROR);
          MAKEINTCONST(FMAX); GENRX(ZC,R0,0,0,0);
          GENRX(ZBC,CONDP,0,1,FENTERROR);
        END;
    END;  (*  CHECKREGISTER  *)

    PROCEDURE CHECKRANGE(FATTRP: ATTRP;
                         FMIN,FMAX,ERRORNO,FENTCHECK,FENTERROR:INTEGER);
    LABEL 1;  (* EXIT IF NO TEST NEEDED *)
    BEGIN
      WITH FATTRP@ DO
        IF KIND = CST THEN
          BEGIN
            IF CVAL.IVAL < FMIN THEN
              BEGIN ERROR(ERRORNO); CVAL.IVAL:=FMIN END
            ELSE IF CVAL.IVAL > FMAX THEN
              BEGIN ERROR(ERRORNO); CVAL.IVAL:=FMAX END;
          END
        ELSE IF DEBUG THEN
          BEGIN
            ALLOWUNDEFINED(FATTRP);  (* BACK UP OVER UNDEF TEST *)
            LOAD(FATTRP,NIL);
            CHECKREGISTER(REALREG(.FATTRP@.REXPR.RNO],
                          FMIN,FMAX,FENTCHECK,FENTERROR);
          END;
1:  END;  (* CHECKRANGE *)


    PROCEDURE CHECKPOINTER(FATTRP: ATTRP; NILALLOWED: BOOLEAN);
      BEGIN
        IF FATTRP@.KIND = CST THEN
          BEGIN IF NOT NILALLOWED THEN ERROR(305); END
        ELSE
          IF DEBUG THEN
            BEGIN
              ALLOWUNDEFINED(FATTRP);  (* BACK UP OVER UNDEF TEST *)
              LOAD(FATTRP,NIL);
              GENRR(ZLTR,R0,REALREG(.FATTRP@.REXPR.RNO]);
              IF NILALLOWED THEN GENRX(ZBC,CONDZ,0,PBASE1,IC+8);
              GENRX(ZBAL,BASEWORK,0,1,ENTCKPTR);
            END;
      END;

    PROCEDURE STORE(F1ATTRP,F2ATTRP:ATTRP; ERRORNO:INTEGER);
      VAR LMIN,LMAX:INTEGER;
          R,OP: INTEGER;

      PROCEDURE ASSIGN(X:INTEGER);
        BEGIN LOAD(F2ATTRP,F1ATTRP); LOADINDEX(F1ATTRP,F2ATTRP);
          LOADBASE(F1ATTRP);
          GENRXP(X,F2ATTRP@.REXPR.RNO,RINDEX,RBASE,EFFADRS);
        END;

      BEGIN (* STORE *)
        IF F1ATTRP@.TYPTR = REALPTR THEN
          BEGIN
            IF COMPTYPES(F2ATTRP@.TYPTR,INTPTR) THEN
              INTTOREAL(F2ATTRP);
            IF F2ATTRP@.TYPTR = REALPTR THEN ASSIGN(ZSTD)
              ELSE ERROR(ERRORNO)
          END
        ELSE
          IF COMPTYPES(F2ATTRP@.TYPTR,F1ATTRP@.TYPTR) THEN
            CASE F1ATTRP@.TYPTR@.FORM OF
              SCALAR,
              SUBRANGE,
              PACKDTYPE:
                BEGIN
                  IF F1ATTRP@.TYPTR@.SIZE.WBLENGTH = 1
                    THEN OP:=ZSTC ELSE OP:=ZST;
                  IF SUBRNGTYPE(F1ATTRP@.TYPTR) OR
                     ((NOT VARCHECK) AND (F1ATTRP@.TYPTR <> INTPTR)) THEN
                    BEGIN
                      GETBOUNDS(F1ATTRP@.TYPTR,LMIN,LMAX);
                      CHECKRANGE(F2ATTRP,LMIN,LMAX,303,ENTCKRANGE,ENTOUTRANGE);
                    END;
                  ASSIGN(OP);
                END;
              POINTER:
                BEGIN CHECKPOINTER(F2ATTRP,TRUE); ASSIGN(ZST) END;
              POWER: BEGIN LOAD(F2ATTRP,F1ATTRP); LOADINDEX(F1ATTRP,F2ATTRP);
                       LOADBASE(F1ATTRP);
                       R := REALREG(.F2ATTRP@.REXPR.RNO];
                       IF RINDEX = 0 THEN GENRX(ZSTM,R,R+1,RBASE,EFFADRS)
                       ELSE
                         BEGIN
                           GENRX(ZST,R,RINDEX,RBASE,EFFADRS);
                           GENRX(ZST,R+1,RINDEX,RBASE,EFFADRS+4);
                         END;
                     END;
              ARRAYS,
              RECORDS: IF F1ATTRP@.TYPTR@.FTYPE THEN ERROR(146)
                         ELSE ASSIGNLONG(F1ATTRP,F2ATTRP);
              FILES: ERROR(146)
            END (*CASE*)
          ELSE ERROR(ERRORNO)
      END; (* STORE *)

    PROCEDURE FILEBUFFER(FATTRP: ATTRP);
      (* CONVERT FATTRP, WHICH DESCRIBES A FILE, INTO A REFERENCE
       * TO THE FILE'S BUFFER VARIABLE.  CALLED BY SELECTOR TO PROCESS
       * EXPLICIT BUFFER VARIABLE REFERENCES IN THE PROGRAM AND BY
       * RWITEM FOR IMPLICIT I/O ASSIGNMENTS. *)
      VAR
        R:REGNO;
      BEGIN
        WITH FATTRP@ DO
          BEGIN
            IF TYPTR@.FORM <> FILES THEN ERROR(400);
            IF TYPTR@.TEXTFILE
              THEN BEGIN LOADADDRESS(FATTRP,NIL);  R:=REXPR.RNO;
                     GENRXP(ZL,R,0,REALREG(.R],BLKPC);
                     ACCESS:=INDIRECT; INDEXREG.REGTEMP:=REGIST;
                     INDEXREG.RNO:=R; VARKIND:=DRCT;
                     VADRS:=0; VLEVEL:=0;
                     TYPTR:=PACKDCHARPTR;
                   END
              ELSE BEGIN VADRS:=VADRS+8; TYPTR:=TYPTR@.FILTYPE END;
            KIND:=VARBL;
          END;
      END;  (* FILEBUFFER *)

    PROCEDURE EXPRESSION(FSYS: SETOFSYS); FORWARD;

    PROCEDURE SELECTOR(FSYS: SETOFSYS; FCP: CTP);
     VAR LATTRP: ATTRP; LCP: CTP;

      PROCEDURE IDADDRESS;
       (* PUT IN 'LATTRP@' THE DESCRIPTION OF THE IDENTIFIER POINTED AT BY 'FCP'.
          USEFUL GLOBAL VARIABLES:
         LATTRP: ATTRP (POINTS TO THE ATTRIBUTE TO BE BUILT UP)
         FCP: CTP; (POINTS TO THE IDENTIFIER WE ARE WORKING ON)
                 !310
         DISX: DISPRANGE (IN THE CASE WHERE THE IDENTIFIER 'ID' IS A FIELD
                          IDENTIFIER: 'DISX' IS THE LEVEL ON WHICH 'ID'
                          WAS DEFINED)  *)
      BEGIN
       WITH FCP@, LATTRP@ DO
        BEGIN TYPTR := IDTYPE; KIND := VARBL;
          VADRS:=0; ACCESS:=DIRECT; VARKIND:=DRCT; VLEVEL:=0;
          IF TYPTR <> NIL THEN
         CASE KLASS OF
          VARS:
           BEGIN
             IF VKIND = DRCT THEN BEGIN VLEVEL:=VLEV; VADRS:=VADDR END
             ELSE BEGIN BASELEV:=VLEV; BASEADD:=PARADDR; VADRS:=0 END;
             ACCESS:=DIRECT; VARKIND:=VKIND;
           END;
          FIELD:
           WITH DISPLAY(.DISX] DO
            BEGIN
              VADRS:=DADRS+FLDADDR; ACCESS:=DIRECT; VARKIND:=DISPKIND;
              IF VARKIND = DRCT THEN VLEVEL:=DLEVEL
              ELSE BEGIN BASELEV:=DBASEL; BASEADD:=DBASEA END;
            END;
          FUNC:
           IF PFDECKIND = STANDARD THEN ERROR(150)
             ELSE IF PFKIND = FORMAL THEN ERROR(151)
               ELSE IF FPROCP@.KLASS <> FUNC THEN ERROR(182)
                 ELSE IF NOT WITHINSCOPE THEN ERROR(183)
                   ELSE
                     BEGIN VLEVEL:=PFLEV+1; VARKIND:=DRCT;
                           VADRS:=SAVEAREA; ACCESS:=DIRECT;
                           FCP@.ASSIGNEDTO := TRUE
                     END
         END (*CASE*)
        END (*WITH*)
      END (*IDADDRESS*) ;

      PROCEDURE INDEXCODE;
        LABEL 1;
        VAR ATTRWORK:ATTRP; LMIN,LMAX,LENGTH,SHIFT,N:INTEGER;
      BEGIN
        LENGTH:=LATTRP@.TYPTR@.AELLENG;
        GETBOUNDS(LATTRP@.TYPTR@.INXTYPE,LMIN,LMAX);
        IF SUBRNGTYPE(LATTRP@.TYPTR@.INXTYPE) OR (NOT VARCHECK) THEN
          CHECKRANGE(GATTRP,LMIN,LMAX,302,ENTCKSUB,ENTOUTSUB);
        IF GATTRP@.KIND=CST
          THEN LATTRP@.VADRS:=LATTRP@.VADRS+(GATTRP@.CVAL.IVAL-LMIN)*LENGTH
        ELSE
          BEGIN
            LATTRP@.VADRS:=LATTRP@.VADRS-LMIN*LENGTH;
            LOAD(GATTRP,NIL);
            IF LENGTH <> 1 THEN
              BEGIN N:=2;
                FOR SHIFT:=1 TO 12 DO
                  BEGIN
                    IF LENGTH = N THEN
                      BEGIN GENRXP(ZSLA,GATTRP@.REXPR.RNO,0,0,SHIFT);
                            GOTO 1;
                      END;
                    N:=N*2;
                  END;
                ATTRNEW(ATTRWORK);
                WITH ATTRWORK@ DO
                  BEGIN TYPTR:=INTPTR; KIND:=CST; CVAL.CKIND:=INT;
                        CVAL.IVAL:=LENGTH;
                  END;
                INTARITH(ATTRWORK,GATTRP,MUL);
                ATTRDISP(ATTRWORK);
              END;
        1:  IF LATTRP@.ACCESS = INDIRECT THEN
              BEGIN ATTRNEW(ATTRWORK);
                WITH ATTRWORK@ DO
                  BEGIN TYPTR:=INTPTR; KIND:=EXPR;
                        REXPR:=LATTRP@.INDEXREG;
                        IF REXPR.REGTEMP=REGIST
                        THEN REGISTER(.REXPR.RNO].REGCONT:=ATTRWORK
                        ELSE REXPR.ATEMP@.TEMPCONT:=ATTRWORK;
                  END;
                INTARITH(ATTRWORK,GATTRP,PLUS); ATTRDISP(ATTRWORK);
              END;
            WITH LATTRP@ DO
              BEGIN INDEXREG:=GATTRP@.REXPR;
                    ACCESS:=INDIRECT; KIND:=VARBL;
                    REGISTER(.INDEXREG.RNO].USED:=TRUE;
                    REGISTER(.INDEXREG.RNO].REGCONT:=LATTRP;
              END;
          END;
      END (*INDEXCODE*);

      PROCEDURE RECFIELD;
      BEGIN WITH LCP@, LATTRP@ DO
        BEGIN
          VADRS:=VADRS+FLDADDR;
          TYPTR:=IDTYPE;
          KIND := VARBL;
        END
      END;

      PROCEDURE POINTEDELEMENT;
        VAR WORK:REGORTEMP;
      BEGIN
        WITH LATTRP@ DO
          BEGIN
            CHECKPOINTER(LATTRP,FALSE);
            LOAD(LATTRP,NIL); WORK:=REXPR;
            INDEXREG:=WORK; ACCESS:=INDIRECT;
            VADRS:=0; VARKIND:=DRCT; VLEVEL:=0;
            TYPTR := TYPTR@.ELTYPE; KIND := VARBL;
          END
      END;


    BEGIN (* SELECTOR *)
     ATTRNEW(LATTRP);
     IDADDRESS;
     IF NOT (SY IN SELECTSYS+FSYS) THEN
      BEGIN ERROR(59); SKIP(SELECTSYS+FSYS) END;
     WHILE SY IN SELECTSYS DO
      BEGIN
(*(.*)   IF SY = LBRACK THEN
        BEGIN
         REPEAT
          WITH LATTRP@ DO
           IF TYPTR <> NIL THEN
            IF TYPTR@.FORM <> ARRAYS THEN
             BEGIN ERROR(138); TYPTR := NIL END;
          INSYMBOL; EXPRESSION(FSYS+(.COMMA,RBRACK]);
          IF GATTRP@.TYPTR <> NIL THEN
           IF GATTRP@.TYPTR@.FORM > SUBRANGE THEN ERROR(113);
          IF LATTRP@.TYPTR <> NIL THEN
           WITH LATTRP@.TYPTR@ DO
            BEGIN
             IF COMPTYPES(INXTYPE,GATTRP@.TYPTR) THEN
              BEGIN
               IF (INXTYPE <> NIL) AND (AELTYPE <> NIL) AND
                  (GATTRP@.TYPTR <> NIL) THEN INDEXCODE;
              END
             ELSE ERROR(139);
             LATTRP@.TYPTR := AELTYPE
            END
         UNTIL SY <> COMMA;
         ACCEPT(RBRACK,12);
        END (* IF SY = LBRACK *)
       ELSE
(*.*)    IF SY = PERIOD THEN
         BEGIN
          WITH LATTRP@ DO
           BEGIN
            IF TYPTR <> NIL THEN
             IF TYPTR@.FORM <> RECORDS THEN
              BEGIN ERROR(140); TYPTR := NIL END;
            INSYMBOL;
            IF SY = IDENT THEN
             BEGIN
              IF TYPTR <> NIL THEN
               BEGIN SEARCHSECTION(TYPTR@.FIELDS,LCP);
                IF LCP = NIL THEN
                 BEGIN ERROR(152); TYPTR := NIL END
                ELSE
                 RECFIELD;
               END;
              INSYMBOL
             END (* SY = IDENT *)
            ELSE ERROR(2)
           END (* WITH LATTRP@ *)
         END (* IF SY = PERIOD *)
        ELSE
(*@*)    BEGIN
          IF LATTRP@.TYPTR <> NIL THEN
           BEGIN
            WITH LATTRP@.TYPTR@ DO
             IF FORM = FILES THEN FILEBUFFER(LATTRP)
             ELSE
              IF FORM = POINTER THEN POINTEDELEMENT
              ELSE ERROR(141);
           END;
          INSYMBOL
         END;
         TESTFOR(FSYS+SELECTSYS,6,(. ]);
      END (* WHILE *) ;
     COPYATTR(LATTRP,GATTRP);
     ATTRDISP(LATTRP);
    END (* SELECTOR *) ;

    PROCEDURE CALL(FSYS: SETOFSYS; FCP: CTP);
     VAR LKEY: 1..NRSTDNAMES;

      PROCEDURE VARIABLE(FSYS: SETOFSYS; CVOK: BOOLEAN);
        (* CVOK => FOR CONTROL VAR OK IN THIS CONTEXT *)
        VAR LCP: CTP;
      BEGIN
        IF SY = IDENT
          THEN BEGIN SEARCHID((.VARS,FIELD],LCP); INSYMBOL END
          ELSE BEGIN ERROR(154); LCP := UVARPTR END;
        SELECTOR(FSYS,LCP);
        IF NOT CVOK THEN
          IF LCP@.KLASS=VARS THEN
            IF LCP@.CNTRLVAR THEN ERROR(191);
      END;


      PROCEDURE GETPUTTEXT(SCODE:INTEGER);
        (* GENERATE CODE FOR INLINE TEXTFILE GET/PUT.  R.T. PROCEDURE
         * 'SCODE' IS CALLED IF THE END OF THE BUFFER IS REACHED.
         * ASSUMES PREVIOUS CODE LOADED I/O BLOCK ADDRESS INTO R15. *)
      VAR BIT: INTEGER;
      BEGIN
        GENRX(ZLA,0,0,0,1);              (* ADD 1 TO BUFFER POINTER *)
        GENRX(ZA,0,0,15,BLKPC);
        GENRX(ZST,0,0,15,BLKPC);
        IF DEBUG THEN
          BEGIN (* CHECK PROPERLY OPEN, EVEN IF IN MIDDLE OF BUFFER *)
            IF SCODE = SYSRDGETEOL THEN BIT:=BSREAD ELSE BIT:=BSWRITE;
            GENRX(ZTM,0,BIT,15,BLKSTAT);
            GENRX(ZBC,CONDZ,0,PBASE1,IC+12);  (* CALL SYS IF NOT OPEN *)
          END;
        GENRX(ZC,0,0,15,BLKPE);          (* CHECK FOR ROOM IN BUFFER *)
        GENRX(ZBC,CONDM,0,PBASE1,IC+10); (* CONTINUE IF BLKPC<BLKPE  *)
        CALLRTSYS(SCODE);                (* ELSE GET NEXT RECORD     *)
      END; (* GETPUTTEXT *)


      PROCEDURE FILEPARM(MUSTBETEXT:BOOLEAN; ERRNO: INTEGER;
                         VAR ISFILE, ISTEXT: BOOLEAN);
        (* PROCESS "(FILE)" PARAMETER.  GENERATES AN ERROR IF PARAMETER IS
           NOT A FILE, OR NOT A TEXT FILE AND MUSTBETEXT IS TRUE.
           ISFILE IS RETURNED TRUE IF THE PARAMETER IS A FILE, AND ISTEXT IS
           TRUE IF IT IS A TEXT FILE.  *)
      BEGIN
        ISFILE := FALSE;  ISTEXT := FALSE;
        ACCEPT(LPARENT,9);
        VARIABLE(FSYS+(.RPARENT],FALSE);
        WITH GATTRP@ DO
          IF TYPTR <> NIL THEN
            WITH TYPTR@ DO
              IF FORM = FILES THEN
                BEGIN ISFILE := TRUE;
                  ISTEXT := TEXTFILE;
                  IF MUSTBETEXT AND (NOT ISTEXT) THEN ERROR(ERRNO);
                END
              ELSE ERROR(ERRNO);
        ACCEPT(RPARENT,4);
      END; (* FILEPARM *)


      PROCEDURE LOADARGADDR(FATTRP: ATTRP);
        (* LOAD ADDRESS OF FATTRP INTO R15 *)
      BEGIN
        LOADADDRESS(FATTRP,NIL);
        GENRR(ZLR,15,REALREG(.FATTRP@.REXPR.RNO]);
      END;

      PROCEDURE STDFLPROCS;
        (* GET, PUT, RESET, REWRITE, CLOSE, EXTEND *)
      VAR SCODE:INTEGER;
          TXT: BOOLEAN;
          ISFILE: BOOLEAN;
      BEGIN
        FILEPARM(FALSE,116,ISFILE,TXT);
        IF ISFILE THEN
          BEGIN
            LOADARGADDR(GATTRP);
            SCODE:=-1;
            CASE LKEY OF
              1: IF TXT THEN GETPUTTEXT(SYSRDGETEOL) ELSE SCODE:=SYSGET;
              2: IF TXT THEN GETPUTTEXT(SYSWRPUTEOL) ELSE SCODE:=SYSPUT;
              3: SCODE:=SYSRESET;
              4: SCODE:=SYSREWRITE;
             21: SCODE:=SYSCLOSE;
             22: SCODE:=SYSEXTEND;
            END; (* CASE *)
            IF SCODE <> -1 THEN CALLRTSYS(SCODE);
            RESETG;
          END;
      END;

      PROCEDURE SETSTFILATTR(FATTRP:ATTRP; FCP:CTP);
      BEGIN
        WITH FATTRP@ DO
          BEGIN TYPTR:=TEXTPTR; KIND:=VARBL; ACCESS:=DIRECT;
            VARKIND := DRCT; VLEVEL := 1;
            VADRS := 0;
            IF FCP <> NIL THEN VADRS := FCP@.VADDR
          END;
      END;

      PROCEDURE STDFILPARM(STDFILPTR: CTP; ERRNO: INTEGER; VAR OK: BOOLEAN);
        (* SETS GATTRP TO STANDARD FILE ATTR IF IT IS DEFINED.  OTHERWISE
           ISSUED ERROR ERRNO.  OK IS TRUE IF NO ERROR DETECTED.  *)
      BEGIN
        OK := STDFILPTR <> NIL;
        IF OK THEN SETSTFILATTR(GATTRP,STDFILPTR)
          ELSE ERROR(ERRNO);
      END;  (* STDFILPARM *)

      PROCEDURE INTRESULT;
        (* ALLOCATE A FREE REGISTER AND FIX GATTRP TO DESCRIBE IT
           AS AN INTEGER VALUE.  ON EXIT, RMAIN = REGISTER NUMBER. *)
      BEGIN
        REGSEARCH(NIL,SINGLE);
        WITH GATTRP@ DO
          BEGIN
            TYPTR := INTPTR; KIND := EXPR;
            REXPR.REGTEMP := REGIST; REXPR.RNO := RWORK;
          END;
        WITH REGISTER(.RWORK] DO
          BEGIN USED := TRUE; REGCONT := GATTRP END;
      END;  (* INTRESULT *)

      PROCEDURE STDWIDTH(VAR FORMP: ATTRP; WIDTH: INTEGER);
       VAR SW: BOOLEAN;
      BEGIN (* STDWIDTH *)
        IF FORMP = NIL THEN SW := TRUE
          ELSE IF FORMP@.TYPTR = NIL THEN SW := TRUE
            ELSE SW := FALSE;
        IF SW THEN
          BEGIN
            ATTRNEW(FORMP);
            WITH FORMP@ DO
            BEGIN
              TYPTR := INTPTR; KIND := CST;
              CVAL.CKIND := INT; CVAL.IVAL := WIDTH;
            END;
          END;
      END; (* STDWIDTH *)


      PROCEDURE RWITEM(FFILATTRP,FITEMATTRP: ATTRP;
                       TXT,READING: BOOLEAN; SCODE:INTEGER);
        (* GENERATE CODE FOR BASIC READ/WRITE PROCEDURES.
         * IF READING IS TRUE, GENERATE READ(F,X) == X:=F@; GET(F).
                                                          !310
         * OTHERWISE, GENERATE WRITE(F,X) == F@:=X; PUT(F).
                                                  !310
         * TXT SHOULD BE TRUE IF THE FILE IS A TEXTFILE.
         * SCODE IS THE CODE OF THE ASSOCIATED RUNTIME SYSTEM GET/PUT
         * ROUTINE TO BE CALLED TO GET THE NEXT RECORD FROM THE FILE.
         * ASSUMES PREVIOUS CODE LOADED I/O BLOCK ADDRESS INTO R15. *)
      VAR
        FILATTRP: ATTRP;
      BEGIN
        ATTRNEW(FILATTRP); COPYATTR(FFILATTRP,FILATTRP);
        FILEBUFFER(FILATTRP);
        IF READING THEN STORE(FITEMATTRP,FILATTRP,116)
                   ELSE STORE(FILATTRP,FITEMATTRP,116);
        ATTRDISP(FILATTRP);
        IF TXT THEN GETPUTTEXT(SCODE) ELSE CALLRTSYS(SCODE)
      END;  (* RWITEM *)


      PROCEDURE STRINGIO(VAR LATTRP,FORM1P:ATTRP; SCODE:INTEGER);
       VAR LENGTH: INTEGER;
      BEGIN
        LENGTH := LATTRP@.TYPTR@.SIZE.WBLENGTH;
        LOADADDRESS(LATTRP,FORM1P);
        STDWIDTH(FORM1P,LENGTH); LOAD(FORM1P,LATTRP);
        LOADINTCONST(R0, 256*LENGTH +
                          16*REALREG(.LATTRP@.REXPR.RNO]+
                             REALREG(.FORM1P@.REXPR.RNO]);
        CALLRTSYS(SCODE);
      END; (* STRINGIO *)

      PROCEDURE READWRITE;
       VAR GETIN,DEFAULT: BOOLEAN; FILATTRP: ATTRP;
           FORM1P,FORM2P,LATTRP,FIL1ATTRP: ATTRP;
           LSP: STP;
           FCP: CTP;
           INLINE: BOOLEAN;  (* TRUE IF WRITE CHARACTER S/B EXPANDED INLINE *)


        PROCEDURE WRITEINT(SCODE,STDLENG:INTEGER);
        BEGIN
          LOAD(LATTRP,FORM1P);
          STDWIDTH(FORM1P,STDLENG); LOAD(FORM1P,LATTRP);
          LOADINTCONST(R0,16*REALREG(.LATTRP@.REXPR.RNO]+
                             REALREG(.FORM1P@.REXPR.RNO]);
          CALLRTSYS(SCODE);
        END;


        PROCEDURE WRITEREAL;
          VAR SCODE:INTEGER;
        BEGIN (* WRITEREAL *)
          LOAD(LATTRP,NIL);
          STDWIDTH(FORM1P,24);
          IF FORM2P=NIL THEN SCODE:=SYSWRE ELSE SCODE:=SYSWRF;
          STDWIDTH(FORM2P,0);
          LOAD(FORM1P,NIL); LOAD(FORM2P,FORM1P);
          LOADINTCONST(R0,256*REALREG(.LATTRP@.REXPR.RNO]
                    +16*REALREG(.FORM1P@.REXPR.RNO]+
                    REALREG(.FORM2P@.REXPR.RNO]);
          CALLRTSYS(SCODE);
        END; (* WRITEREAL *)


        PROCEDURE FIRSTPART;
        BEGIN (* PROCESS FILE NAME *)
          ATTRNEW(FILATTRP); ATTRNEW(FIL1ATTRP);
          IF (LKEY <= 7) THEN FCP:=INPUTPTR
            ELSE FCP:=OUTPUTPTR;
          SETSTFILATTR(FILATTRP,FCP);
          GETIN:=FALSE; DEFAULT:=TRUE;
          IF SY=LPARENT THEN
            BEGIN
              GETIN:=TRUE; INSYMBOL;
              IF LKEY <= 7 THEN VARIABLE(FSYS+(.COMMA,RPARENT,COLON,IDENT],FALSE)
                ELSE EXPRESSION(FSYS+(.COMMA,COLON,RPARENT,IDENT]);
              IF GATTRP@.TYPTR <> NIL THEN
                IF GATTRP@.TYPTR@.FORM=FILES THEN
                  BEGIN
                    COPYATTR(GATTRP,FILATTRP); DEFAULT:=FALSE;
                    IF SY=RPARENT THEN
                      BEGIN INSYMBOL; GETIN:=FALSE; END
                    ELSE IF SY=COMMA THEN
                      BEGIN INSYMBOL;
                        IF LKEY <= 7 THEN VARIABLE(FSYS+(.COMMA,COLON,RPARENT,IDENT],FALSE)
                          ELSE EXPRESSION(FSYS+(.COMMA,COLON,RPARENT,IDENT])
                      END;
                  END;
            END;
          IF DEFAULT THEN
            IF FCP = NIL THEN
              IF LKEY <= 7 THEN ERROR(175) ELSE ERROR(176);
          COPYATTR(FILATTRP,FIL1ATTRP);
          PROCPASS:=FALSE;
          LOADARGADDR(FILATTRP);
          ATTRDISP(FILATTRP);
        END; (* FIRSTPART *)


      BEGIN (* READWRITE *)
        FIRSTPART;
        IF GETIN THEN
          BEGIN (* I/O LIST *)
            while true do begin
              LSP:=GATTRP@.TYPTR; ATTRNEW(LATTRP);
              IF LKEY <= 7 THEN
                IF STRING(LSP) THEN EXTENSION(330);
              COPYATTR(GATTRP,LATTRP);
              FORM1P:=NIL; FORM2P:=NIL;
              IF FIL1ATTRP@.TYPTR@.TEXTFILE THEN
                IF SY=COLON THEN
                  BEGIN (* FIELD WIDTH *)
                    IF (LKEY <= 7) AND NOT STRING(LSP) THEN ERROR(59);
                    INSYMBOL;
                    EXPRESSION(FSYS+(.COMMA,COLON,RPARENT,IDENT]);
                    IF COMPTYPES(GATTRP@.TYPTR,INTPTR) THEN
                      BEGIN
                        IF (GATTRP@.KIND = CST) THEN
                          IF GATTRP@.CVAL.IVAL < 1 THEN ERROR(189);
                        ATTRNEW(FORM1P); COPYATTR(GATTRP,FORM1P);
                      END
                    ELSE ERROR(116);
                    (* FOR FUTURE IMPLEMENTATION *)
                    (*****************************)
                    IF SY = COLON THEN
                      BEGIN (* FRACTIONAL DIGITS *)
                        INSYMBOL; EXPRESSION(FSYS+(.COMMA,RPARENT]);
                        IF COMPTYPES(GATTRP@.TYPTR,INTPTR) THEN
                          BEGIN
                            IF (GATTRP@.KIND = CST) THEN
                              IF GATTRP@.CVAL.IVAL < 1 THEN ERROR(189);
                            ATTRNEW(FORM2P); COPYATTR(GATTRP,FORM2P);
                          END
                        ELSE ERROR(116);
                        IF LSP <> REALPTR THEN ERROR(124);
                      END;
                  END;
              IF PROCPASS THEN
                BEGIN (* RELOAD R15 IF FUNCTION CALLED *)
                  ATTRNEW(FILATTRP); COPYATTR(FIL1ATTRP,FILATTRP);
                  LOADARGADDR(FILATTRP);
                  ATTRDISP(FILATTRP);
                END;
              IF LKEY <= 7 THEN
                BEGIN (* READ *)
                  IF LSP <> NIL THEN
                    BEGIN
                      IF NOT FIL1ATTRP@.TYPTR@.TEXTFILE THEN
                        RWITEM(FIL1ATTRP,LATTRP,FALSE,TRUE,SYSGET)
                      ELSE IF COMPTYPES(LSP,CHARPTR) THEN
                        RWITEM(FIL1ATTRP,LATTRP,TRUE,TRUE,SYSRDGETEOL)
                      ELSE IF COMPTYPES(LSP,INTPTR) THEN
                        BEGIN CALLRTSYS(SYSRDINT);
                          RESETG; INTRESULT;
                          GENRR(ZLR,RMAIN,R0);
                          STORE(LATTRP,GATTRP,116);
                        END
                      ELSE IF LSP = REALPTR THEN
                        BEGIN LOADADDRESS(LATTRP,NIL);
                          GENRR(ZLR,R0,REALREG(.LATTRP@.REXPR.RNO]);
                          CALLRTSYS(SYSRDREAL);
                        END
                      ELSE IF STRING(LSP) THEN
                        STRINGIO(LATTRP,FORM1P,SYSRDSTRING)
                      ELSE ERROR(153);
                    END;
                END  (* READ *)
              ELSE
                BEGIN (* WRITE *)
                  IF LSP <> NIL THEN
                    IF NOT FIL1ATTRP@.TYPTR@.TEXTFILE THEN
                      RWITEM(FIL1ATTRP,LATTRP,FALSE,FALSE,SYSPUT)
                    ELSE IF COMPTYPES(LSP,CHARPTR) THEN
                      BEGIN INLINE := FALSE;
                        IF FORM1P = NIL THEN INLINE := TRUE  (* DEFAULT WIDTH = 1 *)
                        ELSE WITH FORM1P@ DO
                          IF KIND = CST THEN
                            IF CVAL.IVAL = 1 THEN INLINE := TRUE;
                        IF INLINE THEN
                          RWITEM(FIL1ATTRP,LATTRP,TRUE,FALSE,SYSWRPUTEOL)
                        ELSE WRITEINT(SYSWRCHAR,1);
                      END (* WRITE CHAR *)
                    ELSE IF COMPTYPES(LSP,INTPTR) THEN WRITEINT(SYSWRINT,12)
                    ELSE IF LSP = REALPTR THEN WRITEREAL
                    ELSE IF COMPTYPES(LSP,BOOLPTR) THEN
                      WRITEINT(SYSWRBOOL,4)
                    ELSE IF STRING(LSP) THEN
                      STRINGIO(LATTRP,FORM1P,SYSWRSTRING)
                    ELSE ERROR(116)
                END; (* WRITE *)
              ATTRDISP(LATTRP);
              IF FORM1P <> NIL THEN ATTRDISP(FORM1P);
              IF FORM2P <> NIL THEN ATTRDISP(FORM2P);
              RESETG;
              IF SY <> COMMA THEN break;
              INSYMBOL;
              IF LKEY <= 7 THEN VARIABLE(FSYS+(.COMMA,COLON,RPARENT,IDENT],FALSE)
                ELSE EXPRESSION(FSYS+(.COMMA,COLON,RPARENT,IDENT]);
            END; (* LOOP *)
            ACCEPT(RPARENT,4);
          END
          ELSE IF (LKEY = 6) OR (LKEY = 8) THEN ERROR(116);
          IF (LKEY IN (.7,9]) AND
             (NOT FIL1ATTRP@.TYPTR@.TEXTFILE) THEN ERROR(116);
          IF LKEY = 7 THEN CALLRTSYS(SYSRDLN)
          ELSE IF LKEY = 9 THEN CALLRTSYS(SYSWRITELN);
        ATTRDISP(FIL1ATTRP);
      END; (* READWRITE *)



      PROCEDURE PAGE;
        VAR ISFILE, ISTEXT: BOOLEAN;
      BEGIN
        IF SY = LPARENT THEN FILEPARM(TRUE,116,ISFILE,ISTEXT)
        ELSE STDFILPARM(OUTPUTPTR,176,ISTEXT);
        IF ISTEXT THEN
          BEGIN
            LOADARGADDR(GATTRP);
            CALLRTSYS(SYSWRPAGE);
            RESETG;
          END;
      END (* PAGE *) ;

      PROCEDURE PACK;
        VAR
          LATTRP,CATTRP: ATTRP;
          LOW,HIGH,LMIN,LMAX: INTEGER;
          LSP,LSP1: STP;
          LADDR: ADDRRANGE;
          SOURCEREG,DESTREG: INTEGER;
          DMIN,DMAX: INTEGER;

      BEGIN (* PACK *)
        ACCEPT(LPARENT,9);
        VARIABLE(FSYS+(.COMMA,RPARENT],FALSE);
        ATTRNEW(LATTRP); COPYATTR(GATTRP,LATTRP);
        LOW:=0; HIGH:=0; LSP:=NIL; LSP1:=NIL;
        IF GATTRP@.TYPTR <> NIL THEN
          WITH GATTRP@.TYPTR@ DO
            IF FORM = ARRAYS THEN
              IF AELTYPE@.FORM <> PACKDTYPE THEN
                BEGIN
                  LSP:=INXTYPE; LSP1:=AELTYPE;
                  IF LSP <> NIL THEN GETBOUNDS(LSP,LOW,HIGH);
                END
              ELSE ERROR(116)
            ELSE ERROR(116);
        ACCEPT(COMMA,20);
        EXPRESSION(FSYS+(.COMMA,RPARENT]);
        IF NOT COMPTYPES(GATTRP@.TYPTR,LSP) THEN ERROR(116);
        ACCEPT(COMMA,20);
        ATTRNEW(CATTRP); COPYATTR(GATTRP,CATTRP);
        VARIABLE(FSYS+(.RPARENT],FALSE);
        IF GATTRP@.TYPTR <> NIL THEN
        WITH GATTRP@.TYPTR@ DO
        BEGIN
          IF FORM = ARRAYS THEN
            IF (AELTYPE@.FORM = PACKDTYPE) THEN
              IF COMPTYPES(AELTYPE,LSP1) AND
                 COMPTYPES(INXTYPE,LSP) THEN
               BEGIN
                 LMIN:=0; LMAX:=0;
                 SOURCEREG:=0; DESTREG:=0;
                 IF INXTYPE <> NIL THEN GETBOUNDS(INXTYPE,LMIN,LMAX);
                 IF LMAX-LMIN > HIGH - LOW THEN ERROR(116);
                 CHECKRANGE(CATTRP,LOW,LMIN-LMAX+HIGH,302,ENTCKSUB,ENTOUTSUB);
                 IF (LATTRP@.TYPTR <> NIL) AND (CATTRP@.TYPTR <> NIL) THEN
                   BEGIN
                     LATTRP@.VADRS:=LATTRP@.VADRS-4*LOW;
                     IF CATTRP@.KIND = CST THEN
                     BEGIN
                       LATTRP@.VADRS:=LATTRP@.VADRS+4*CATTRP@.CVAL.IVAL;
                       LOADADDRESS(LATTRP,NIL);
                     END ELSE
                     BEGIN
                       LOADADDRESS(LATTRP,NIL);
                       LOAD(CATTRP,LATTRP);
                       GENRX(ZSLL,REALREG(.CATTRP@.REXPR.RNO],0,0,2);
                       GENRR(ZAR,REALREG(.LATTRP@.REXPR.RNO],
                                 REALREG(.CATTRP@.REXPR.RNO]);
                     END;
                     SOURCEREG := REALREG(.LATTRP@.REXPR.RNO];
                   END;
                 ATTRDISP(CATTRP);
                 IF GATTRP@.TYPTR <> NIL THEN
                   BEGIN LOADADDRESS(GATTRP,LATTRP);
                     DESTREG := REALREG(.GATTRP@.REXPR.RNO];
                   END;
                 LOADINTCONST(15,ABS(LMAX-LMIN)+1);
                 LADDR := IC;
                 GENRX(ZL,R0,0,SOURCEREG,0);
                 IF DEBUG AND (SUBRNGTYPE(AELTYPE) OR (NOT VARCHECK)) THEN
                   BEGIN GETBOUNDS(AELTYPE,DMIN,DMAX);
                     CHECKREGISTER(R0,DMIN,DMAX,ENTCKRANGE,ENTOUTRANGE);
                   END
                 ELSE IF VARCHECK THEN
                   GENRX(ZBAL,BASEWORK,0,1,ENTCKUNWORD);
                   GENRX(ZSTC,R0,0,DESTREG,0);
                   GENRX(ZLA,SOURCEREG,0,SOURCEREG,4);
                   GENRX(ZLA,DESTREG,0,DESTREG,1);
                   GENRX(ZBCT,15,0,PBASE1,LADDR);
               END ELSE ERROR(116)
            ELSE ERROR(118)
          ELSE ERROR(116);

        END;
        ATTRDISP(LATTRP);
        RESETG;
        ACCEPT(RPARENT,4);
      END; (* PACK *)

      PROCEDURE UNPACK;
      VAR
        SOURCE,DEST: ATTRP;
        LOW,HIGH,LMIN,LMAX: INTEGER;
        LSP,LSP1: STP;
        LADDR: ADDRRANGE;
        SOURCEREG,DESTREG: INTEGER;
        DMIN,DMAX: INTEGER;
        CHECKED: BOOLEAN;

      BEGIN (* UNPACK *)
        ACCEPT(LPARENT,9);
        EXPRESSION(FSYS+(.COMMA,RPARENT]);
        LSP:=NIL; LSP1:=NIL; LMIN:=0; LMAX:=0;
        IF GATTRP@.TYPTR <> NIL THEN
          WITH GATTRP@.TYPTR@ DO
            IF FORM = ARRAYS THEN
              IF AELTYPE@.FORM = PACKDTYPE THEN
                BEGIN
                  LSP:=INXTYPE; LSP1:=AELTYPE;
                  IF LSP <> NIL THEN GETBOUNDS(LSP,LMIN,LMAX);
                END
              ELSE ERROR(118)
            ELSE ERROR(116);
        ATTRNEW(SOURCE); COPYATTR(GATTRP,SOURCE);
        ACCEPT(COMMA,20);
        VARIABLE(FSYS+(.COMMA,RPARENT],FALSE);
        ATTRNEW(DEST); COPYATTR(GATTRP,DEST);
        LOW:=0; HIGH:=0;
        SOURCEREG:=0; DESTREG:=0;
        IF DEST@.TYPTR <> NIL THEN
          WITH DEST@.TYPTR@ DO
            IF FORM = ARRAYS THEN
              IF (AELTYPE@.FORM <> PACKDTYPE) THEN
                IF COMPTYPES(INXTYPE,LSP) AND COMPTYPES(AELTYPE,LSP1) THEN
                BEGIN
                  IF INXTYPE <> NIL THEN GETBOUNDS(INXTYPE,LOW,HIGH);
                  IF LMAX-LMIN > HIGH - LOW THEN ERROR(116);
                END
                ELSE ERROR(116)
              ELSE ERROR(116)
            ELSE ERROR(116);
        ACCEPT(COMMA,20);
        EXPRESSION(FSYS+(.RPARENT]);
        CHECKRANGE(GATTRP,LOW,LMIN-LMAX+HIGH,302,ENTCKSUB,ENTOUTSUB);
        IF (DEST@.TYPTR <> NIL) AND (GATTRP@.TYPTR <> NIL) THEN
        BEGIN
          DEST@.VADRS := DEST@.VADRS - 4*LOW;
          IF GATTRP@.KIND = CST THEN
          BEGIN
            DEST@.VADRS := DEST@.VADRS + 4*GATTRP@.CVAL.IVAL;
            LOADADDRESS(DEST,NIL);
          END ELSE
          BEGIN
            LOADADDRESS(DEST,NIL);
            LOAD(GATTRP,DEST);
            GENRX(ZSLL,REALREG(.GATTRP@.REXPR.RNO],0,0,2);
            GENRR(ZAR,REALREG(.DEST@.REXPR.RNO],REALREG(.GATTRP@.REXPR.RNO]);
          END;
          DESTREG := REALREG(.DEST@.REXPR.RNO];
        END;
        RESETG;
        IF SOURCE@.TYPTR <> NIL THEN
          BEGIN LOADADDRESS(SOURCE,DEST);
            SOURCEREG := REALREG(.SOURCE@.REXPR.RNO];
          END;
        LOADINTCONST(15,ABS(LMAX-LMIN)+1);
        LOADINTCONST(R0,0);
        LADDR:=IC;
        GENRX(ZIC,R0,0,SOURCEREG,0);
        (* ISSUE BOTH UNDEFINED AND RANGE TESTS HERE IF REQUESTED.
           RANGE TEST CANNOT DETECT AN UNDEFINED BYTE VALUE AND ISSUE
           THE APPROPRIATE MESSAGE. *)
        CHECKED := FALSE;
        IF VARCHECK AND (LSP1 <> NIL) THEN
          BEGIN GETBOUNDS(LSP1,DMIN,DMAX);
            IF (UNDEFBYTE < DMIN) OR (UNDEFBYTE > DMAX) THEN
              BEGIN CHECKED := TRUE;
                GENRX(ZBAL,BASEWORK,0,1,ENTCKUNBYTE);
              END;
          END;
        (* CHECK DESTINATION RANGE *)
        (* BUT DON'T BOTHER UNLESS DESTINATION IS A SUBRANGE OF 0..255, *)
        (* WHICH IS THE MAXIMUM POSSIBLE RANGE OF THE SOURCE            *)
        IF (DEST@.TYPTR <> NIL) AND (DEST@.TYPTR <> INTPTR) THEN
          IF DEBUG AND ((NOT CHECKED) OR SUBRNGTYPE(DEST@.TYPTR)) THEN
            BEGIN GETBOUNDS(DEST@.TYPTR,DMIN,DMAX);
              IF (DMIN > 0) OR (DMAX < 255) THEN
                CHECKREGISTER(R0,DMIN,DMAX,ENTCKRANGE,ENTOUTRANGE);
            END;
        GENRX(ZST,R0,0,DESTREG,0);
        GENRX(ZLA,SOURCEREG,0,SOURCEREG,1);
        GENRX(ZLA,DESTREG,0,DESTREG,4);
        GENRX(ZBCT,15,0,PBASE1,LADDR);
        ATTRDISP(SOURCE); ATTRDISP(DEST);
        ACCEPT(RPARENT,4);
      END; (* UNPACK *)

      PROCEDURE TIMEDATE;
        VAR LMIN,LMAX: INTEGER;
      BEGIN
        ACCEPT(LPARENT,9);
        VARIABLE(FSYS+(.RPARENT],FALSE);
        WITH GATTRP@ DO
          IF TYPTR <> NIL THEN
            IF TYPTR@.FORM <> ARRAYS THEN ERRORRESET(116)
              ELSE IF TYPTR@.AELTYPE <> NIL THEN
                IF (TYPTR@.AELTYPE@.FORM <> PACKDTYPE) OR
                   (TYPTR@.AELTYPE@.BASETYPE <> CHARPTR)
                  THEN ERRORRESET(116)
                  ELSE BEGIN GETBOUNDS(TYPTR@.INXTYPE,LMIN,LMAX);
                         IF LMAX-LMIN <> 7 THEN ERRORRESET(116);
                       END;
        IF GATTRP@.TYPTR <> NIL THEN
          BEGIN LOADADDRESS(GATTRP,NIL);
            GENRR(ZLR,R0,REALREG(.GATTRP@.REXPR.RNO]);
            IF LKEY = 10 THEN CALLRTSYS(SYSTIME) ELSE CALLRTSYS(SYSDATE);
          END;
        RESETG;
        ACCEPT(RPARENT,4);
      END;

      PROCEDURE NEWDISPOSE;
        LABEL 1;
        TYPE TAGPTR = @TAGSTORE;
             TAGSTORE = RECORD OP,VAL,OFFST: INTEGER; NXT: TAGPTR END;
        VAR LSP,LSP1: STP; LVAL: VALU; LSIZE: WBSIZE; LMIN,LMAX: INTEGER;
            STOREOP: INTEGER;
            SAVEDISP,FIRSTDISP: TAGPTR;
      BEGIN IF SY = LPARENT THEN INSYMBOL ELSE ERROR(9);
        VARIABLE(FSYS+(.COMMA,RPARENT],FALSE);
        FIRSTDISP := NIL;
        LSP := NIL; INITSIZE(LSIZE);
        IF GATTRP@.TYPTR <> NIL THEN
          WITH GATTRP@.TYPTR@ DO
            IF FORM = POINTER THEN
              BEGIN
                IF ELTYPE <> NIL THEN
                  BEGIN LSIZE := ELTYPE@.SIZE;
                    IF ELTYPE@.FORM = RECORDS THEN LSP := ELTYPE@.RECVAR
                  END
              END
            ELSE ERROR(116);
        WHILE SY = COMMA DO
          BEGIN INSYMBOL; CONSTANT(FSYS+(.COMMA,RPARENT],LSP1,LVAL);
            IF LSP = NIL THEN ERROR(158)
            ELSE
              IF LSP@.TGFLDP <> NIL THEN
                IF (LSP1 = REALPTR) OR STRING(LSP1) THEN ERROR(159)
                ELSE
                  IF COMPTYPES(LSP@.TGFLDP@.IDTYPE,LSP1) THEN
                    BEGIN
                      GETBOUNDS(LSP@.TGFLDP@.IDTYPE,LMIN,LMAX);
                      IF (LVAL.IVAL > LMAX) OR (LVAL.IVAL < LMIN) THEN ERROR(181);
                      IF LSP@.TGFLDP@.NAME <> '        ' THEN
                        BEGIN
                          IF LSP@.TGFLDP@.IDTYPE <> NIL THEN
                            BEGIN IF LSP@.TGFLDP@.IDTYPE@.FORM = PACKDTYPE
                                    THEN STOREOP:=ZSTC
                                    ELSE STOREOP:=ZST;
                              NEW(SAVEDISP); SAVEDISP@.NXT:=NIL;
                              IF FIRSTDISP = NIL THEN
                                FIRSTDISP:=SAVEDISP
                              ELSE
                                BEGIN
                                  SAVEDISP@.NXT:=FIRSTDISP;
                                  FIRSTDISP:=SAVEDISP
                                END;
                              WITH SAVEDISP@ DO
                                BEGIN
                                  OP:=STOREOP;
                                  VAL:=LVAL.IVAL;
                                  OFFST:=LSP@.TGFLDP@.FLDADDR
                                END;
                            END;
                        END;
                      LSP1:= LSP@.FSTVAR;
                      WHILE LSP1 <> NIL DO
                        WITH LSP1@ DO
                          IF VARVAL = LVAL.IVAL THEN
                            BEGIN LSP:=SUBVAR;
                              LSIZE:=SIZE;
                              GOTO 1
                            END
                          ELSE LSP1:=NXTVAR;
                      LSIZE:=LSP@.SIZE;
                      LSP:=NIL;
                    END
                  ELSE ERROR(116);
      1:  END (* WHILE *) ;
        IF DEBUG THEN NEEDRTPROC('P@THEAP ') ELSE NEEDRTPROC('P@HEAP  ');
        ALIGNMENT(LSIZE.WBLENGTH,8);
        LOADINTCONST(R0,LSIZE.WBLENGTH);
        IF LKEY = 12 THEN
          BEGIN (* NEW *)
            IF DEBUG THEN CALLRTSYS(SYSTNEW) ELSE CALLRTSYS(SYSNEW);
            LOADINDEX(GATTRP,NIL); LOADBASE(GATTRP);
            GENRX(ZST,15,RINDEX,RBASE,EFFADRS);
            IF FIRSTDISP <> NIL THEN EXTENSION(331);
            WHILE FIRSTDISP <> NIL DO
            BEGIN (* INITIALIZE TAGFIELDS *)
              WITH FIRSTDISP@ DO
                BEGIN
                  LOADINTCONST(R0,VAL);
                  GENRX(OP,R0,0,15,OFFST);
                END;
              FIRSTDISP:=FIRSTDISP@.NXT
            END;
          END (* NEW *)
        ELSE
          BEGIN (* DISPOSE *)
            IF GATTRP@.TYPTR <> NIL THEN LOADARGADDRESS(GATTRP);
            IF DEBUG THEN CALLRTSYS(SYSTDISP) ELSE CALLRTSYS(SYSDISP);
          END;
        ACCEPT(RPARENT,4);
        RESETG;
      END (* NEWDISPOSE *) ;

      PROCEDURE MARKRELEASE;
      BEGIN
        ACCEPT(LPARENT,9);
        VARIABLE(FSYS+(.COMMA,RPARENT],FALSE);
        IF GATTRP@.TYPTR <> NIL THEN
          IF GATTRP@.TYPTR@.FORM = POINTER THEN
            BEGIN
              IF LKEY = 13 THEN
                BEGIN (* MARK *)
                  LOADINDEX(GATTRP,NIL); LOADBASE(GATTRP);
                  GENRX(ZST,NEWPOINTER,RINDEX,RBASE,EFFADRS);
                END
              ELSE
                BEGIN (* RELEASE *)
                  LOAD(GATTRP,NIL);
                  GENRR(ZLR,R0,REALREG(.GATTRP@.REXPR.RNO]);
                  IF DEBUG THEN
                    BEGIN NEEDRTPROC('P@THEAP '); CALLRTSYS(SYSTRLSE) END
                  ELSE
                    BEGIN NEEDRTPROC('P@HEAP  '); CALLRTSYS(SYSRLSE)  END;
                END
            END
          ELSE ERROR(116);
        RESETG;
        ACCEPT(RPARENT,4);
      END;

      PROCEDURE LEFTXPRS;
      BEGIN
        IF SY = LPARENT THEN INSYMBOL ELSE ERROR(9);
        EXPRESSION(FSYS+(.RPARENT]);
      END; (* LEFTXPRS *)


      PROCEDURE STRARG(NEEDVAR: BOOLEAN; ERRNO: INTEGER;
                       VAR OK: BOOLEAN);
        (* COMPILE "(<STRING>" OR "(<CHAR>" PARAMETER. NEEDVAR MEANS THE
           STRING MUST BE A VARIABLE. IF ERRORS, ISSUES ERRNO, ELSE SETS OK TRUE,
           LOADS THE STRING ADDRESS INTO R15, AND ITS LENGTH INTO R0. *)
      BEGIN
        ACCEPT(LPARENT,9);
        IF NEEDVAR THEN VARIABLE(FSYS+(.RPARENT,COMMA],FALSE)
          ELSE EXPRESSION(FSYS+(.RPARENT,COMMA]);
        OK := FALSE;
        WITH GATTRP@ DO
          IF TYPTR <> NIL THEN
            IF STRING(TYPTR) THEN
              BEGIN OK := TRUE;
                LOADINTCONST(R0,TYPTR@.SIZE.WBLENGTH);
                LOADARGADDR(GATTRP);
              END
            ELSE IF COMPTYPES(TYPTR,CHARPTR) THEN
              BEGIN OK := TRUE;
                LOADINTCONST(R0,1);
                IF KIND = EXPR THEN
                  BEGIN IF REXPR.REGTEMP = REGIST THEN SAVE(REXPR.RNO);
                    BASEREGISTER(LEVEL,REXPR.ATEMP@.TEMPADRS+3);
                    GENRX(ZLA,15,0,RBASE,EFFADRS);
                  END
                ELSE
                  BEGIN LOADADDRESS(GATTRP,NIL);
                    GENRX(ZLA,15,0,REALREG(.REXPR.RNO],3);  (* LAST BYTE OF WORD *)
                  END;
              END
            ELSE ERROR(ERRNO);
      END;  (* STRARG *)


      PROCEDURE INTARG(NEEDVAR: BOOLEAN; ERRNO: INTEGER;
                       VAR OK, ISPACKED: BOOLEAN);
        (*  PARSE "<INTEGER>)" PARAMETER FOR STANDARD PROCEDURES.
            ERRNO ISSUED IF PROBLEMS, OTHERWISE OK SET TRUE AND ISPACKED
            INDICATES IF THIS IS A PACKED (1 BYTE) INTEGER.  NEEDVAR =>
            MUST BE A FULL WORD VARIABLE. *)
      BEGIN
        IF NEEDVAR THEN VARIABLE(FSYS+(.RPARENT],FALSE)
          ELSE EXPRESSION(FSYS+(.RPARENT]);
        WITH GATTRP@ DO
          BEGIN
            ISPACKED := FALSE;
            OK := COMPTYPES(TYPTR,INTPTR) AND (TYPTR <> NIL);
            IF OK THEN ISPACKED := TYPTR@.SIZE.WBLENGTH = 1
              ELSE ERROR(ERRNO);
          END;
        ACCEPT(RPARENT,4);
      END;  (* INTARG *)


      PROCEDURE ROUNDTRUNCF;
        VAR TEMP: CMP; HALF,ONE: VALU;
      BEGIN
        LEFTXPRS;
        IF GATTRP@.TYPTR <> REALPTR THEN ERROR(125)
        ELSE BEGIN
        LOAD(GATTRP,NIL);
        IF LKEY = 4 THEN
          BEGIN HALF.CKIND:=REEL; HALF.RVAL:=0.5;
                ONE.CKIND:=REEL; ONE.RVAL:=1.0;
                MAKECONSTANT(HALF); GENRXP(ZAD, GATTRP@.REXPR.RNO,0,0,0);
                GENRX(ZBC,CONDP,0,PBASE1,IC+8);
                MAKECONSTANT(ONE); GENRXP(ZSD,GATTRP@.REXPR.RNO,0,0,0);
          END;
        GENRXP(ZAW,GATTRP@.REXPR.RNO,0,1,ENTUNNORM0);
        GETTEMP(8,TEMP); REGSEARCH(NIL,DOUBLE);
        BASEREGISTER(LEVEL,TEMP@.TEMPADRS);
        GENRXP(ZSTD,GATTRP@.REXPR.RNO,0,RBASE,EFFADRS);
        GENRX(ZLM,RMAIN,RMAIN+1,RBASE,EFFADRS);
        GENRX(ZLA,RMAIN,0,RMAIN,0); GENRX(ZSLDA,RMAIN,0,0,32);
        GENRX(ZTM,8,0,RBASE,EFFADRS); GENRX(ZBC,14,0,PBASE1,IC+6);
        GENRR(ZLNR,RMAIN,RMAIN);
        WITH GATTRP@ DO
          BEGIN REGISTER(.REXPR.RNO].USED:=FALSE;
                TYPTR:=INTPTR; REXPR.RNO:=RWORK;
          END;
        REGISTER(.RWORK].USED:=TRUE; REGISTER(.RWORK].REGCONT:=GATTRP;
        DELETETEMP(TEMP)
        END;
        ACCEPT(RPARENT,4);
      END;

      PROCEDURE ABSF;
      BEGIN
        LEFTXPRS;
        LOAD(GATTRP,NIL);
        WITH GATTRP@ DO
          IF TYPTR <> NIL THEN
            IF COMPTYPES(TYPTR,INTPTR)
              THEN GENRRP1(ZLPR,REXPR.RNO)
            ELSE IF TYPTR = REALPTR
              THEN GENRRP1(ZLPDR,REXPR.RNO)
            ELSE ERROR(125);
        ACCEPT(RPARENT,4);
      END;

      PROCEDURE SQRF;
      BEGIN
        LEFTXPRS;
        IF GATTRP@.TYPTR <> NIL THEN
        WITH GATTRP@ DO
          BEGIN
            IF TYPTR = REALPTR THEN
              BEGIN LOAD(GATTRP,NIL); GENRRP1(ZMDR,REXPR.RNO); END
            ELSE IF COMPTYPES(TYPTR,INTPTR) THEN
              BEGIN LOADEVENODD(GATTRP,NIL,1);
                GENRRP(ZMR,PRED(REXPR.RNO),REXPR.RNO);
              END
            ELSE ERROR(125);
          END;
        ACCEPT(RPARENT,4);
      END;

      PROCEDURE ODDP;
        VAR OK, P: BOOLEAN;
      BEGIN
        ACCEPT(LPARENT,9);
        INTARG(FALSE,125,OK,P);
        IF OK THEN
          BEGIN
            LOAD(GATTRP,NIL); MAKEINTCONST(1);
            GENRXP(ZN,GATTRP@.REXPR.RNO,0,0,0);
            GATTRP@.TYPTR := BOOLPTR;
          END
      END;

      PROCEDURE ORDF;
      BEGIN
        LEFTXPRS;
        WITH GATTRP@ DO
          IF TYPTR <> NIL THEN
            IF TYPTR@.FORM >= POWER
              THEN ERROR(125)
              ELSE IF TYPTR = REALPTR
                THEN ERROR(125)
                ELSE
                  BEGIN IF TYPTR@.FORM = POINTER THEN EXTENSION(335);
                    IF TYPTR@.SIZE.WBLENGTH <> 1
                      THEN TYPTR:=INTPTR
                      ELSE TYPTR:=PACKDINTPTR;
                  END;
        ACCEPT(RPARENT,4);
      END;

      PROCEDURE CHRF;
        VAR OK, PKD: BOOLEAN;
      BEGIN
        ACCEPT(LPARENT,9);
        INTARG(FALSE,125,OK,PKD);
        IF OK THEN
          WITH GATTRP@ DO
            IF PKD
              THEN TYPTR := PACKDCHARPTR
              ELSE
                BEGIN TYPTR := CHARPTR;
                  ALLOWUNDEFINED(GATTRP);  (* FORCE RANGE TEST *)
                  CHECKRANGE(GATTRP,0,ORDCHARMAX,303,ENTCKRANGE,ENTOUTRANGE);
                END;
      END;

      PROCEDURE PREDSUCCF;
        VAR R,LMIN,LMAX: INTEGER;
      BEGIN
        LEFTXPRS;
        WITH GATTRP@ DO
          IF TYPTR <> NIL THEN
            IF (TYPTR@.FORM > SUBRANGE) OR (TYPTR = REALPTR) THEN ERROR(125)
            ELSE
              BEGIN LOAD(GATTRP,NIL);
                R:=REALREG(.GATTRP@.REXPR.RNO];
                IF TYPTR = INTPTR THEN LMAX:=MAXINT
                ELSE GETBOUNDS(TYPTR,LMIN,LMAX);
                IF LKEY = 11 THEN
                  BEGIN (* SUCC *)
                    IF LMAX <= TWO24 THEN GENRX(ZLA,R,0,R,1)
                    ELSE
                      BEGIN MAKEINTCONST(1);
                        GENRX(ZA,R,0,0,0);
                      END;
                  END (* SUCC *)
                ELSE (* PRED *)
                  GENRR(ZBCTR,R,0);
                IF NOT COMPTYPES(TYPTR,INTPTR) THEN
                  BEGIN ALLOWUNDEFINED(GATTRP);  (* FORCE RANGE TEST *)
                    CHECKRANGE(GATTRP,LMIN,LMAX,304,ENTCKRANGE,ENTOUTRANGE);
                  END;
              END;
        ACCEPT(RPARENT,4);
      END;


      PROCEDURE HALTMSG;
        (* PROCESS HALT AND MESSAGE PROCEDURES.  ALLOWS STRING IN HALT PROC. *)
      VAR OK: BOOLEAN; SCODE: INTEGER;
      BEGIN
        IF (LKEY = 17) OR (SY = LPARENT) THEN
          BEGIN (* MESSAGE OR HALT WITH PARAMETER *)
            STRARG(FALSE,116,OK);
            ACCEPT(RPARENT,4);
            RESETG;
          END
        ELSE
          BEGIN (* HALT WITH NO ARGUMENTS *)
            GENRR(ZXR,R0,R0);  OK:=TRUE;
          END;
        IF OK THEN
          BEGIN
            IF LKEY = 17 THEN SCODE:=SYSMESSAGE ELSE SCODE:=SYSHALT;
            CALLRTSYS(SCODE);
          END;
      END;  (* HALTMSG *)

      PROCEDURE SYSPARMS;
        (* PROCESS SYSTEM AND GETPARMS PROCEDURES *)
      VAR OK,P: BOOLEAN;  SCODE: INTEGER;  LATTRP: ATTRP;
      BEGIN
        STRARG(LKEY=24,116,OK);
        IF OK THEN
          BEGIN
            IF LKEY = 24 THEN SCODE:=SYSGETPARM ELSE SCODE:=SYSSYSTEM;
            CALLRTSYS(SCODE);
          END;
        RESETG;
        INTRESULT;
        GENRR(ZLR,RMAIN,R0);
        ATTRNEW(LATTRP);  COPYATTR(GATTRP,LATTRP);
        ACCEPT(COMMA,20);
        INTARG(TRUE,116,OK,P);
        IF OK THEN STORE(GATTRP,LATTRP,116);
        ATTRDISP(LATTRP);  RESETG;
      END;  (* SYSPARMS *)


      PROCEDURE STDIPROCS;
        (* STANDARD PROCEDURES WITH ONE INTEGER PARAMETER:  SETCC, TRAPATTN *)
      VAR OK,P: BOOLEAN;  SCODE: INTEGER;
      BEGIN
        ACCEPT(LPARENT,9);
        INTARG(FALSE,116,OK,P);
        IF OK THEN
          BEGIN
            LOAD(GATTRP,NIL);
            GENRR(ZLR,R0,REALREG(.GATTRP@.REXPR.RNO]);
            IF LKEY = 20 THEN SCODE := SYSSETCC ELSE SCODE := SYSTRAPATN;
            CALLRTSYS(SCODE);
            RESETG;
          END;
      END;  (* STDIPROCS *)

      PROCEDURE STDIFUNCS;
        (* STANDARD FUNCTIONS WITH NO ARGUMENTS AND INTEGER RESULT:
             CLOCK, CLOCKLEFT, ATTENTIONS *)
      VAR SCODE: INTEGER;
      BEGIN
        CASE LKEY OF
          18: SCODE := SYSCLOCK;
          22: SCODE := SYSNATTNS;
          23: SCODE := SYSCLKLEFT;
        END;
        CALLRTSYS(SCODE);
        INTRESULT;
        GENRR(ZLR,RMAIN,R0);
      END;  (* STDIFUNCS *)


      PROCEDURE CARD;
      BEGIN (* CARD *)
        LEFTXPRS;
        LOAD(GATTRP,NIL);
        IF GATTRP@.TYPTR <> NIL THEN
          IF GATTRP@.TYPTR@.FORM = POWER THEN
          BEGIN
            REGSEARCH(NIL,SINGLE);
            GENRR(ZSR,RMAIN,RMAIN);
            GENRXP(ZSLDA,GATTRP@.REXPR.RNO,0,0,0);
            GENRX(ZBC,CONDZ,0,PBASE1,IC+20);
            GENRX(ZBC,CONDP,0,PBASE1,IC+8);
            GENRX(ZLA,RMAIN,0,RMAIN,1);
            GENRXP(ZSLDL,GATTRP@.REXPR.RNO,0,0,1);
            GENRX(ZBC,15,0,PBASE1,IC-20);
            WITH GATTRP@ DO
            BEGIN
              TYPTR := INTPTR;
              REGISTER(.REXPR.RNO].USED := FALSE;
              REGISTER(.SUCC(REXPR.RNO)].USED := FALSE;
              REXPR.RNO:=RWORK;
            END;
            REGISTER(.RWORK].USED := TRUE;
            REGISTER(.RWORK].REGCONT := GATTRP;
          END ELSE ERROR(116);
        ACCEPT(RPARENT,4);
      END; (* CARD *)

      PROCEDURE STDFLFUNCS;
        (* EOR, EOLN *)
        VAR ISFILE, ISTEXT: BOOLEAN;
            MASK, COND: INTEGER;
            LATTRP: ATTRP;
      BEGIN
        IF SY = LPARENT THEN FILEPARM(LKEY=2,125,ISFILE,ISTEXT)
        ELSE BEGIN STDFILPARM(INPUTPTR,175,ISTEXT); ISFILE:=ISTEXT END;
        IF ISFILE AND (ISTEXT OR (LKEY <> 2)) THEN
          WITH GATTRP@ DO
            BEGIN
              IF DEBUG THEN
                BEGIN ATTRNEW(LATTRP); COPYATTR(GATTRP,LATTRP);
                  LOADADDRESS(LATTRP,NIL);
                  GENRR(ZLR,15,REALREG(.LATTRP@.REXPR.RNO]);
                  ATTRDISP(LATTRP);
                END;
              VADRS:=VADRS+4;
              LOAD(GATTRP,NIL);
              IF DEBUG THEN
                BEGIN  (* CHECK OPEN AND, FOR EOLN, NOT EOF *)
                  IF LKEY = 2 THEN
                    BEGIN MASK:=BSEOF; COND:=CONDZ END
                  ELSE BEGIN MASK:=BSREAD+BSWRITE; COND:=CONDNZ END;
                  LOADINTCONST(R0,MASK);
                  GENRR(ZNR,R0,REALREG(.REXPR.RNO]);
                  GENRX(ZBC,COND,0,PBASE1,IC+10);
                  CALLRTSYS(SYSUNFIL);
                END;
              IF LKEY = 2 THEN GENRXP(ZSRL,REXPR.RNO,0,0,1);
              MAKEINTCONST(1); GENRXP(ZN,REXPR.RNO,0,0,0);
              TYPTR:=BOOLPTR; KIND:=EXPR;
            END;
      END;  (* STDFLFUNCS *)

      PROCEDURE TERMF;
        (* TERMINAL FUNCTION - TRUE IF FILE IS CONNECTED TO TTY *)
      VAR OK,P: BOOLEAN;
      BEGIN
        FILEPARM(FALSE,125,OK,P);
        IF OK THEN
          BEGIN
            LOADARGADDR(GATTRP);
            CALLRTSYS(SYSTERMINAL);
            RESETG;
            INTRESULT;
            GENRR(ZLR,RMAIN,R0);
            GATTRP@.TYPTR:=BOOLPTR;
          END;
      END;  (* TERMF *)

      PROCEDURE LINELENF;
        (* LINELENGTH FUNCTION = NO. CHARS IN CURRENT TEXTFILE LINE *)
      VAR  OK,P: BOOLEAN; R: INTEGER;
      BEGIN
        FILEPARM(TRUE,125,P,OK);
        IF OK THEN
          BEGIN
            LOADARGADDRESS(GATTRP);
            RESETG; INTRESULT;
            GENRX(ZL,RMAIN,0,15,BLKPE);
            GENRX(ZS,RMAIN,0,15,BLKPC);
          END;
      END;  (* LINELENF *)


      PROCEDURE UNDEFPROC;
        (* UNDEFINED(X) PROCEDURE.  RETURNS TRUE IF X HAS THE "UNDEFINED" VALUE. *)
        (* IF X IS A COMPONENT OF A PACKED ARRAY AND COULD LEGITIMATELY HAVE THE
           "UNDEFINED BYTE" VALUE (I.E., PACKED ARRAY OF CHAR), THEN UNDEFINED
           WILL RETURN FALSE BY COMPARING THE BYTE TO A UNDEFINED FULLWORD.
           THIS INSURES THET UNDEFINED DOES NOT RETURN TRUE FOR VARIABLES WITH
           VALUES THAT WOULD BE ALLOWED IN AN EXPRESSION. *)
      VAR R,LMIN,LMAX: INTEGER;
      BEGIN
        ACCEPT(LPARENT,9);
        VARIABLE(FSYS+(.RPARENT],TRUE);
        WITH GATTRP@ DO
          IF TYPTR <> NIL THEN
            IF TYPTR@.FORM > POINTER THEN ERROR(170)
            ELSE
              BEGIN
                LOAD(GATTRP,NIL); R:=REALREG(.GATTRP@.REXPR.RNO];
                IF TYPTR@.SIZE.WBLENGTH = 1 THEN
                  GETBOUNDS(TYPTR,LMIN,LMAX)
                ELSE BEGIN LMIN:=0; LMAX:=0 END;
                IF TYPTR = REALPTR THEN
                     GENRX(ZCD,R,0,1,ENTUNDEFWD)
                ELSE IF (TYPTR@.SIZE.WBLENGTH = 1) AND
                        ((UNDEFBYTE < LMIN) OR (UNDEFBYTE > LMAX)) THEN
                          GENRX(ZC,R,0,1,ENTUNBYTE)
                ELSE GENRX(ZC,R,0,1,ENTUNDEFWD);
              END;
        IF GATTRP@.TYPTR = REALPTR THEN
          BEGIN  (* FREE REAL REG AND GET INTEGER REG *)
            RESETG; INTRESULT;
            R:=REALREG(.GATTRP@.REXPR.RNO];
          END;
        BOOLVALUE(R,CONDZ);
        GATTRP@.TYPTR:=BOOLPTR;
        ACCEPT(RPARENT,4);
      END;  (* UNDEFPROC *)


      PROCEDURE STDHEAPFUNCS;
        (* STORAGE INQUIRY FUNCTIONS: SIZEHEAP, SIZESTACK, SIZEFREE *)
      BEGIN
        INTRESULT;
        CASE LKEY OF
          25: BEGIN  (* SIZEHEAP *)
                GENRX(ZL,RMAIN,0,1,ENTTAIL);
                GENRR(ZSR,RMAIN,NEWPOINTER);
              END;
          26: BEGIN  (* SIZESTACK *)
                GENRR(ZLR,RMAIN,STACKPOINTER);
                GENRR(ZSR,RMAIN,1);
              END;
          27: BEGIN  (* SIZEFREE *)
                GENRR(ZLR,RMAIN,NEWPOINTER);
                GENRR(ZSR,RMAIN,STACKPOINTER);
              END;
        END;  (* CASE *)
      END;  (* STDHEAPFUNCS *)

      PROCEDURE SIZEOFFUNC;
        (* SIZEOF(X) RETURNS THE NUMBER OF BYTES OCCUPIED BY X.
           X MAY BE EITHER A VARIABLE OR TYPE IDENTIFIER. *)
      VAR LCP: CTP;  LSP: STP;
      BEGIN
        ACCEPT(LPARENT,9);
        IF SY <> IDENT THEN ERROR(2)
        ELSE
          WITH GATTRP@ DO
            BEGIN
              TYPTR := INTPTR;
              KIND := CST;  CVAL.CKIND := INT;
              SEARCHID((.VARS,TYPES],LCP);
              IF LCP@.IDTYPE <> NIL
                THEN CVAL.IVAL := LCP@.IDTYPE@.SIZE.WBLENGTH
                ELSE CVAL.IVAL := 0;
              INSYMBOL;
            END;
        ACCEPT(RPARENT,4);
      END;  (* SIZEOF *)


      PROCEDURE STDARITHFUNCS;
        VAR  TP,SCODE: INTEGER; PNAME: ALFA;
           (* NOTE: STANDARD PROCS/FUNCTS USED LIKE THIS *)
           (*       MUST HAVE A LENGTH OF <= ALFALENG-2  *)
      BEGIN  (* CSECT NAME = P@XXXXXX, WHERE XXXXXX = FCN NAME *)
        PNAME(.1] := 'P';  PNAME(.2] := '@';
        WITH FCP@ DO
          BEGIN
            IF (NAME(.ALFALENG-1] <> ' ') OR (NAME(.ALFALENG] <> ' ')
              THEN ERROR(400);  (* BAD NAME *)
            FOR TP := 3 TO ALFALENG DO PNAME(.TP] := NAME(.TP-2]
          END;
        NEEDRTPROC(PNAME);
        LEFTXPRS;
        IF COMPTYPES(GATTRP@.TYPTR,INTPTR)
          THEN INTTOREAL(GATTRP);
        IF GATTRP@.TYPTR <> REALPTR THEN ERROR(125);
        LOAD(GATTRP,NIL);
        LOADINTCONST(R0,REALREG(.GATTRP@.REXPR.RNO]);
        CASE LKEY OF
          12: SCODE := SYSSIN;
          13: SCODE := SYSCOS;
          14: SCODE := SYSEXP;
          15: SCODE := SYSSQRT;
          16: SCODE := SYSLN;
          17: SCODE := SYSATAN
        END;
        CALLRTSYS(SCODE);
        ACCEPT(RPARENT,4);
      END;

      PROCEDURE CALLNONSTANDARD;
        VAR NXT,LCP,NXT1,NXT2: CTP; PASSPROC: BOOLEAN;
            OLDSTACK: INTEGER; FORMAL: ATTRP;
            FSP: STP; KIND: REGKIND; X: INTEGER;
            FLOATREG: REGNO;
            FORT,EXTPASC: BOOLEAN;  (* TRUE IF CALLING EXTERNAL PROC *)
            L,ENTRY: INTEGER;
            FORTSTACK: INTEGER;


        PROCEDURE PARTONE;
        BEGIN
          NXT:=FCP@.PARAMS;
          OLDSTACK:=STACKTOP;
          IF OLDSTACK <> 0 THEN
            BEGIN  (* MOVE SP ABOVE PARAMS FOR OTHER PROCS *)
              ALIGNMENT(OLDSTACK,8); MAKEINTCONST(OLDSTACK);
              GENRX(ZA,STACKPOINTER,0,0,0); STACKTOP:=0;
            END;
          L:=FCP@.PFCNT;
          FORT := (PROCADDRESS(.L+1] = 0) AND
                  (PROCADDRESS(.L]   > 1) AND
                  (PROCADDRESS(.L]  <= 4);
          EXTPASC := (PROCADDRESS(.L+1] = 0) AND (PROCADDRESS(.L] = 1);
          IF SY = LPARENT THEN
            BEGIN  (* PASS PARAMETERS *)
              REPEAT PASSPROC:=FALSE;
                IF NXT = NIL THEN ERROR(126)
                  ELSE IF NXT@.KLASS IN (.PROC,FUNC] THEN PASSPROC:=TRUE;
                INSYMBOL;
                IF PASSPROC THEN
                  BEGIN
                    IF SY <> IDENT
                      THEN BEGIN ERROR(2); SKIP(FSYS+(.COMMA,RPARENT]); END
                    ELSE
                      BEGIN
                        IF NXT@.KLASS = PROC THEN SEARCHID((.PROC],LCP)
                        ELSE
                          BEGIN SEARCHID((.FUNC],LCP);
                            IF NXT@.IDTYPE <> LCP@.IDTYPE THEN ERROR(128);
                          END;
                        IF LCP@.PFDECKIND = STANDARD THEN ERROR(164)
                        ELSE
                          BEGIN NXT1:=NXT@.PARAMS;
                            NXT2:=LCP@.PARAMS;
                            WHILE (NXT1 <> NIL) AND (NXT2 <> NIL) DO
                              BEGIN
                                IF NXT1@.IDTYPE <> NXT2@.IDTYPE THEN ERROR(186)
                                ELSE IF NXT2@.KLASS = VARS THEN
                                  IF NXT2@.VKIND <> NXT1@.VKIND THEN ERROR(186);
                                NXT1:=NXT1@.NEXT; NXT2:=NXT2@.NEXT;
                              END;
                            IF NXT1 <> NXT2 THEN ERROR(186);
                            WITH LCP@ DO
                              IF PFKIND = ACTUAL THEN
                                BEGIN
                                  IF (PROCADDRESS(.PFCNT+1]  = 0) AND
                                     (PROCADDRESS(. PFCNT ]  > 1) AND
                                     (PROCADDRESS(. PFCNT ] <= 4) THEN ERROR(384)
                                  ELSE
                                    BEGIN IF NOT EXTRNL THEN L:=4 ELSE L:=8;
                                      L := PROCBASE + 4 * PFCNT - L;
                                      IF (PROCADDRESS(.PFCNT+1] = 0) AND
                                         (PROCADDRESS(.PFCNT] = 1) THEN L:=L+4;
                                      GENRX(ZLA,R0,0,0,L);
                                      BASEREGISTER(STACKPOINTER,NXT@.PFADDR);
                                      GENRX(ZST,R0,0,RBASE,EFFADRS);
                                      BASEREGISTER(STACKPOINTER,NXT@.PFADDR+4);
                                      GENRX(ZST,STACKPOINTER,0,RBASE,EFFADRS);
                                    END
                                END
                              ELSE
                                BEGIN
                                  BASEREGISTER(PFLEV,PFADDR);
                                  GENSS(ZMVC,7,8,NXT@.PFADDR,RBASE,EFFADRS);
                                END;
                            STACKTOP:=NXT@.PFADDR+8;
                          END;
                        INSYMBOL;
                      END;
                  END  (* PROC/FUNC PARAMETER *)
                ELSE
                  BEGIN  (* VAR/VALUE OR EXCESS PARAMETER *)
                    IF NXT <> NIL THEN
                      IF NXT@.VKIND = DRCT THEN
                        BEGIN  (* VALUE PARAMETER *)
                          EXPRESSION(FSYS+(.COMMA,RPARENT]);
                          IF GATTRP@.TYPTR <> NIL THEN
                            BEGIN ATTRNEW(FORMAL);
                              WITH FORMAL@,NXT@ DO
                                BEGIN TYPTR:=IDTYPE; KIND:=VARBL; VADRS:=VADDR;
                                      ACCESS:=DIRECT; VARKIND:=DRCT; VLEVEL:=STACKPOINTER;
                                END;
                              IF NXT@.IDTYPE <> NIL THEN
                                BEGIN STORE(FORMAL,GATTRP,142);
                                  STACKTOP:=NXT@.VADDR+NXT@.IDTYPE@.SIZE.WBLENGTH;
                                END;
                              ATTRDISP(FORMAL);
                            END
                        END  (* VALUE PARAMETER *)
                      ELSE
                        BEGIN  (* VAR PARAMETER *)
                          VARIABLE(FSYS+(.COMMA,RPARENT],FALSE);
                          IF GATTRP@.TYPTR <> NIL THEN
                            IF NXT@.IDTYPE = GATTRP@.TYPTR THEN
                              IF (GATTRP@.TYPTR@.SIZE.WBLENGTH = 1)
                                  AND (GATTRP@.TYPTR@.FORM <> ARRAYS) THEN ERROR(187)
                              ELSE
                                BEGIN LOADADDRESS(GATTRP,NIL);
                                  BASEREGISTER(STACKPOINTER,NXT@.PARADDR);
                                  GENRXP(ZST,GATTRP@.REXPR.RNO,0,RBASE,EFFADRS);
                                  STACKTOP:=NXT@.PARADDR+4;
                                END
                            ELSE ERROR(142);
                        END  (* VAR PARAMETER *)
                    ELSE  (* SKIP EXTRA ACTUAL PARAMETER *)
                      EXPRESSION(FSYS+(.COMMA,RPARENT]);
                    RESETG;
                  END;  (* VAR/VALUE OR EXCESS PARAMETER *)
                IF NXT <> NIL THEN NXT:=NXT@.NEXT;
              UNTIL SY <> COMMA;
              ACCEPT(RPARENT,4);
            END;  (* PARAMETER LIST *)
          IF NXT <> NIL THEN ERROR(126);
        END;  (* PARTONE *)


      BEGIN  (* CALLNONSTANDARD *)
        PARTONE;
        IF FORT THEN
          BEGIN  (* SET UP FORTRAN PARAM ADDRESS LIST *)
            NXT := FCP@.PARAMS;
            ALIGNMENT(STACKTOP,4);
            FORTSTACK := STACKTOP;
            IF NXT <> NIL THEN
            REPEAT
              IF NXT@.VKIND = DRCT THEN
                BEGIN
                  BASEREGISTER(STACKPOINTER,NXT@.VADDR);
                  GENRX(ZLA,0,0,RBASE,EFFADRS)
                END
              ELSE
                BEGIN
                  BASEREGISTER(STACKPOINTER,NXT@.PARADDR);
                  GENRX(ZL,0,0,RBASE,EFFADRS);
                END;
              BASEREGISTER(STACKPOINTER,STACKTOP);
              GENRX(ZST,0,0,RBASE,EFFADRS);
              STACKTOP := STACKTOP+4;
              IF NXT <> NIL THEN NXT:=NXT@.NEXT;
            UNTIL NXT = NIL;
            IF FCP@.PARAMS <> NIL THEN
              BEGIN  (* ARGUMENT LIST ADDR TO R15 *)
                GENRX(ZMVI,8,0,RBASE,EFFADRS);
                BASEREGISTER(STACKPOINTER,FORTSTACK);
                GENRX(ZLA,15,0,RBASE,EFFADRS);
              END
            ELSE
              GENRR(ZSR,15,15);
            IF FCP@.KLASS = FUNC THEN
              BEGIN FSP:=FCP@.IDTYPE;
                IF FSP = REALPTR THEN L:=ENTSTD ELSE L:=ENTST
              END
            ELSE L:=ENTNOP;
            GENRX(ZO,15,0,1,L);            (* GET FUNCTION STORE OPCODE *)
            BASEREGISTER(STACKPOINTER,STACKTOP);
            GENRX(ZLA,0,0,RBASE,EFFADRS);  (* FOR STORE OVFL TEST *)
          END;  (* FORTRAN PARAMETER LIST *)

        FOR FLOATREG:=F0 TO F6 DO
           IF REGISTER(.FLOATREG].USED THEN SAVE(FLOATREG);
        WITH FCP@ DO
          IF PFKIND <> ACTUAL THEN
            BEGIN  (* CALL PROCEDURE PARAMETER *)
              BASEREGISTER(PFLEV,PFADDR);
              GENRX(ZL,15,0,RBASE,EFFADRS);
              GENRX(ZL,0,0,1,8);
              IF (PFADDR+4) >= 4096 THEN GENRX(ZLA,9,0,PBASE1,IC+24)
                ELSE GENRX(ZLA,9,0,PBASE1,IC+18);
              GENRX(ZSTM,8,6,STACKPOINTER,0);
              BASEREGISTER(PFLEV,PFADDR+4);
              GENRX(ZL,2,0,RBASE,EFFADRS);
              GENRX(ZLM,2,6,2,40);
              GENRX(ZBC,15,0,1,ENTVARPROC);
            END
          ELSE
            BEGIN  (* REGULAR PROCEDURE CALL *)
              IF NOT EXTRNL THEN L:=4 ELSE L:=8;
              L := PROCBASE+4*FCP@.PFCNT-L;
              IF L = 0 THEN GENRX(ZL,0,0,1,8);
              IF FORT THEN ENTRY:=ENTEXTFORT
                ELSE IF EXTPASC THEN ENTRY:=ENTEXTPASC
                  ELSE ENTRY:=0;
              IF ENTRY = 0 THEN GENRR(ZBALR,9,1)
                ELSE GENRX(ZBAL,9,0,1,ENTRY);
              MAKECODE(IC,L); IC:=IC+2;
            END;
        IF FCP@.KLASS = FUNC THEN
          BEGIN  (* LOAD FUNCTION RESULT INTO REGISTER *)
            FSP:=FCP@.IDTYPE;
            IF FSP = REALPTR
              THEN BEGIN KIND:=FLOAT; X:=ZLD END
              ELSE BEGIN KIND:=SINGLE; X:=ZL END;
            REGSEARCH(NIL,KIND);
            GENRX(X,RMAIN,0,STACKPOINTER,SAVEAREA);
            WITH GATTRP@ DO
              BEGIN TYPTR:=FSP; KIND:=EXPR;
                    REXPR.REGTEMP:=REGIST; REXPR.RNO:=RWORK;
              END;
            REGISTER(.RWORK].USED:=TRUE; REGISTER(.RWORK].REGCONT:=GATTRP;
          END;
        IF OLDSTACK <> 0 THEN
          BEGIN MAKEINTCONST(OLDSTACK); GENRX(ZS,STACKPOINTER,0,0,0); END;
        STACKTOP:=OLDSTACK;
      END;  (* CALLNONSTANDARD *)

    BEGIN (* CALL *)
      PROCPASS := TRUE;
      IF FCP@.PFDECKIND = DECLARED THEN CALLNONSTANDARD
      ELSE
        BEGIN
          LKEY := FCP@.KEY;
          IF FCP@.KLASS = PROC THEN
            BEGIN
              IF (LKEY IN (.13,14,17,18,20..25]) THEN EXTENSION(333);
              CASE LKEY OF
                21,22,
                1,2,
                3,4:   STDFLPROCS;      (* GET,PUT,RESET,REWRITE *)
                  5:   PAGE;
            6,7,8,9:   READWRITE;
                10,11: TIMEDATE;
                12,19: NEWDISPOSE;
                13,14: MARKRELEASE;
                15:    PACK;
                16:    UNPACK;
                17,18: HALTMSG;
                20,23: STDIPROCS;
                24,25: SYSPARMS;
              END (* CASE *)
            END
          ELSE
            BEGIN
              IF (LKEY IN (.18..28]) THEN EXTENSION(334);
              CASE LKEY OF
                1,2:   STDFLFUNCS;      (* EOF,EOLN *)
                3:     ODDP;
                4,5:   ROUNDTRUNCF;
                6:     ABSF;
                7:     SQRF;
                8:     ORDF;
                9:     CHRF;
                10,11: PREDSUCCF;
                12,13,
                14,15,
                16,17: STDARITHFUNCS;   (* SIN,COS,EXP,SQRT,LN,ARCTAN *)
                19:    CARD;
                20:    TERMF;
                21:    LINELENF;
                18,22,23: STDIFUNCS;
                24:    UNDEFPROC;
                25,26,27: STDHEAPFUNCS;
                28:    SIZEOFFUNC;
              END
            END
        END;
    END  (* CALL *) ;

    PROCEDURE EXPRESSION  (* FSYS: SETOFSYS *) ;
    VAR LATTRP: ATTRP; LOP: OPERATOR;

      PROCEDURE REGULAROPERATION(FATTRP:ATTRP; FOP:OPERATOR);
      BEGIN
        IF COMPTYPES(FATTRP@.TYPTR,INTPTR) THEN
          IF COMPTYPES(GATTRP@.TYPTR,INTPTR)
            THEN INTARITH(FATTRP,GATTRP,FOP)
            ELSE
              IF GATTRP@.TYPTR = REALPTR
                THEN REALARITH(FATTRP,GATTRP,FOP)
                ELSE ERRORRESET(134)
        ELSE
          IF (FATTRP@.TYPTR = REALPTR) THEN
            IF (GATTRP@.TYPTR = REALPTR) OR
                COMPTYPES(GATTRP@.TYPTR,INTPTR)
              THEN REALARITH(FATTRP,GATTRP,FOP)
              ELSE ERRORRESET(134)
          ELSE
            IF (FATTRP@.TYPTR@.FORM = POWER) AND
                COMPTYPES(FATTRP@.TYPTR,GATTRP@.TYPTR)
              THEN SETARITH(FATTRP,GATTRP,FOP)
              ELSE ERRORRESET(134);
      END;

      PROCEDURE SETTYPECHECK(FSP:STP);
        VAR LMIN,LMAX: INTEGER;
      BEGIN
        FOLDANYCHAR(GATTRP);
        WITH GATTRP@ DO
          BEGIN
            IF TYPTR = REALPTR THEN ERRORRESET(109);
            IF TYPTR <> NIL THEN
              IF TYPTR@.FORM > SUBRANGE THEN ERRORRESET(136)
              ELSE IF NOT COMPTYPES(FSP,TYPTR) THEN ERRORRESET(137);
            IF TYPTR <> NIL THEN
              BEGIN LMIN := -MAXINT; LMAX := MAXINT;
                IF TYPTR <> INTPTR THEN GETBOUNDS(TYPTR,LMIN,LMAX);
                IF (NOT VARCHECK) OR COMPTYPES(TYPTR,INTPTR) OR
                  ( (NOT COMPTYPES(TYPTR,CHARPTR)) AND
                   ((LMIN < SETMIN) OR (LMAX > SETMAX)) ) THEN
                  CHECKRANGE(GATTRP,SETMIN,SETMAX,304,
                             ENTCKRANGE,ENTOUTRANGE);
              END;
          END;
      END;

      PROCEDURE POWERSET;
        VAR LSP:STP; LCSTATTRP,LVARATTRP,LATTRP,ATTRWORK: ATTRP;
            VARPART: BOOLEAN; N: INTEGER;
      BEGIN INSYMBOL;
        NEW(LSP,POWER);
        WITH LSP@ DO
          BEGIN ELSET := NIL; PCKDSET := FALSE;
            FTYPE := FALSE;
            SIZE.WBLENGTH:=8; SIZE.BOUNDARY:=8;
          END;
        VARPART := FALSE;
        ATTRNEW(LCSTATTRP);
        WITH LCSTATTRP@ DO
          BEGIN TYPTR:=LSP; KIND:=CST; CVAL.CKIND:=PSET; CVAL.PVAL:=(. ];
          END;
        IF SY = RBRACK THEN INSYMBOL
        ELSE
          BEGIN
            (* LOOP UNTIL SY <> COMMA: *)
            while true do begin
              EXPRESSION(FSYS+(.COMMA,COLON,RBRACK]);
              SETTYPECHECK(LSP@.ELSET);
              IF GATTRP@.TYPTR <> NIL THEN LSP@.ELSET:=GATTRP@.TYPTR;
              IF SY = COLON THEN
                BEGIN ATTRNEW(LATTRP); COPYATTR(GATTRP,LATTRP);
                  INSYMBOL;
                  EXPRESSION(FSYS+(.COMMA,RBRACK]);
                  SETTYPECHECK(LATTRP@.TYPTR);
                  IF (LATTRP@.TYPTR <> NIL) AND (GATTRP@.TYPTR <> NIL) THEN
                    BEGIN
                      IF (LATTRP@.KIND = CST) AND (GATTRP@.KIND = CST) THEN
                        BEGIN
                          FOR N:=LATTRP@.CVAL.IVAL TO GATTRP@.CVAL.IVAL DO
                            IF (N >= SETMIN) AND (N <= SETMAX) THEN
                              LCSTATTRP@.CVAL.PVAL := LCSTATTRP@.CVAL.PVAL+(.N];
                          ATTRDISP(LATTRP);
                        END
                      ELSE
                        BEGIN
                          LOAD(GATTRP,LATTRP); OPERATION(GATTRP,LATTRP,ZS,ZSR);
                          ATTRNEW(ATTRWORK);
                          WITH ATTRWORK@ DO
                            BEGIN TYPTR:=LSP; KIND:=CST; CVAL.CKIND:=PSET;
                                  CVAL.PVAL:=(.0];
                            END;
                          LOAD(ATTRWORK,GATTRP);
                          GENRXP(ZSRDA,ATTRWORK@.REXPR.RNO,0,REALREG(.GATTRP@.REXPR.RNO],0);
                          EXCATTR(LATTRP,GATTRP); ATTRDISP(LATTRP);
                          LOAD(GATTRP,ATTRWORK);
                          GENRXP(ZSRDL,ATTRWORK@.REXPR.RNO,0,REALREG(.GATTRP@.REXPR.RNO],0);
                          EXCATTR(ATTRWORK,GATTRP); ATTRDISP(ATTRWORK);
                          IF VARPART THEN SETARITH(GATTRP,LVARATTRP,PLUS)
                          ELSE BEGIN VARPART:=TRUE; ATTRNEW(LVARATTRP); COPYATTR(GATTRP,LVARATTRP) END;
                        END;
                    END;
                END  (* COLON *)
              ELSE
                IF GATTRP@.TYPTR <> NIL THEN
                  BEGIN
                    IF GATTRP@.KIND = CST THEN
                      LCSTATTRP@.CVAL.PVAL := LCSTATTRP@.CVAL.PVAL
                                              +(.GATTRP@.CVAL.IVAL]
                    ELSE
                      BEGIN
                        ATTRNEW(ATTRWORK);
                        WITH ATTRWORK@ DO
                          BEGIN TYPTR:=LSP; KIND:=CST; CVAL.CKIND:=PSET;
                                CVAL.PVAL:=(.0];
                          END;
                        LOAD(ATTRWORK,GATTRP); LOAD(GATTRP,ATTRWORK);
                        GENRXP(ZSRDL,ATTRWORK@.REXPR.RNO,0,REALREG(.GATTRP@.REXPR.RNO],0);
                        EXCATTR(ATTRWORK,GATTRP); ATTRDISP(ATTRWORK);
                        IF VARPART THEN SETARITH(GATTRP,LVARATTRP,PLUS)
                          ELSE BEGIN VARPART:=TRUE; ATTRNEW(LVARATTRP); COPYATTR(GATTRP,LVARATTRP) END;
                      END;
                  END;
              IF SY <> COMMA THEN break; INSYMBOL;
            END;  (* LOOP *)
            ACCEPT(RBRACK,12);
          END;
        IF VARPART THEN
          BEGIN
            IF LCSTATTRP@.CVAL.PVAL <> (. ] THEN
              SETARITH(LCSTATTRP,LVARATTRP,PLUS);
            COPYATTR(LVARATTRP,GATTRP); ATTRDISP(LVARATTRP);
          END
        ELSE COPYATTR(LCSTATTRP,GATTRP);
        ATTRDISP(LCSTATTRP);
      END;

      PROCEDURE FACTOR(FSYS: SETOFSYS);
        VAR LCP: CTP;
            LATTRP: ATTRP;
            FLT: 0..1;
            LMIN,LMAX: INTEGER;
            CV: BOOLEAN;  (* TRUE IF VAR IS FOR-LOOP CONTROL VAR, AND *)
                          (* THUS NEED NOT BE TESTED FOR UNDEFINED.   *)
            VCHECKSAVE: BOOLEAN;
      BEGIN
        IF NOT (SY IN FACBEGSYS) THEN
          BEGIN ERROR(58); SKIP(FSYS+FACBEGSYS);
            GATTRP@.TYPTR := NIL
          END;
        REPEAT
          IF SY IN FACBEGSYS THEN
            BEGIN
              CASE SY OF
      (*ID*)    IDENT:
                  BEGIN
                    SEARCHID((.KONST,VARS,FIELD,FUNC,TYPES],LCP);
                    INSYMBOL;
                    CASE LCP@.KLASS OF
                      KONST: WITH LCP@,GATTRP@ DO
                               BEGIN TYPTR:=IDTYPE; KIND:=CST;
                                     CVAL:=VALUES;
                               END;
      (*TYPES*)       TYPES: BEGIN
                               EXTENSION(328);
                               ACCEPT(LPARENT,9);
                               VCHECKSAVE := VARCHECK;
                               VARCHECK := FALSE;
                               EXPRESSION(FSYS+(.RPARENT]);
                               VARCHECK := VCHECKSAVE;
                               WITH GATTRP@ DO
                                 IF TYPTR <> NIL THEN
                                   BEGIN
                                     IF KIND = CST THEN ERROR(292);
                                     TYPTR:=LCP@.IDTYPE;
                                   END;
                                 ACCEPT(RPARENT,4);
                             END;
                      VARS,
                      FIELD: BEGIN
                               IF LCP@.KLASS = VARS
                                 THEN CV:=LCP@.CNTRLVAR
                                 ELSE CV:=FALSE;
                                 SELECTOR(FSYS,LCP);
                                 IF NOT CV THEN CHECKUNDEFINED(GATTRP);
                             END;
                      FUNC:  CALL(FSYS,LCP)
                    END  (* CASE *)
                  END;
      (*NIL*)   NILSY:BEGIN
                        INSYMBOL;
                        WITH GATTRP@ DO
                          BEGIN
                            TYPTR := NILPTR;
                            KIND := CST;
                            CVAL.CKIND := INT;
                            CVAL.IVAL := NILVAL
                          END
                      END;
      (*CST*)   INTCONST:
                  BEGIN
                    WITH GATTRP@ DO
                      BEGIN TYPTR := INTPTR; KIND := CST;
                            CVAL.CKIND:=INT; CVAL.IVAL:=IVAL;
                      END;
                    INSYMBOL
                  END;
                REALCONST:
                  BEGIN
                    WITH GATTRP@ DO
                      BEGIN TYPTR := REALPTR; KIND := CST;
                            CVAL.CKIND:=REEL; CVAL.RVAL:=RVAL;
                      END;
                    INSYMBOL
                  END;
                CHARCONST:
                  BEGIN
                    WITH GATTRP@ DO
                      BEGIN TYPTR := CHARPTR; KIND := CST;
                            CVAL.CKIND:=INT; CVAL.IVAL:=IVAL;
                      END;
                    INSYMBOL
                  END;
                STRINGCONST:
                  BEGIN
                    WITH GATTRP@ DO
                      BEGIN STRINGTYPE(TYPTR); KIND := CST;
                            CVAL.CKIND:=STRG; CVAL.VALP:=CONSTP;
                      END;
                    INSYMBOL
                  END;
      (*(*)     LPARENT:
                  BEGIN INSYMBOL; EXPRESSION(FSYS+(.RPARENT]);
                    IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4)
                  END;
      (*NOT*)   NOTSY:
                  BEGIN INSYMBOL; FACTOR(FSYS);
                    IF GATTRP@.TYPTR <> NIL THEN
                      IF COMPTYPES(GATTRP@.TYPTR,BOOLPTR) THEN NOTFACTOR(GATTRP)
                        ELSE BEGIN ERROR(135); GATTRP@.TYPTR:=NIL; END;
                  END;
      (*(.*)    LBRACK:  POWERSET;
              END  (* CASE *) ;
              TESTFOR(FSYS,6,FACBEGSYS)
            END  (* IF *)
        UNTIL SY IN FSYS;


        (* EXPONENTIATION  *)
        (*******************)


        IF SY = EXPONOP THEN
          BEGIN
            EXTENSION(327);
            IF (NOT COMPTYPES(GATTRP@.TYPTR,INTPTR)) AND
               (GATTRP@.TYPTR <> REALPTR) THEN ERROR(399);
            INSYMBOL;
            ATTRNEW(LATTRP); COPYATTR(GATTRP,LATTRP);
            FACTOR(FSYS);
            IF COMPTYPES(GATTRP@.TYPTR,INTPTR) THEN
              BEGIN
                LOAD(LATTRP,GATTRP);
                LOAD(GATTRP,LATTRP);
                IF LATTRP@.TYPTR = INTPTR THEN FLT:=0 ELSE FLT:=1;
                LOADINTCONST(R0,FLT*256+16*REALREG(.LATTRP@.REXPR.RNO]
                             +REALREG(.GATTRP@.REXPR.RNO]);
                NEEDRTPROC('P@IPOWR ');
                CALLRTSYS(SYSIPOWER);
                EXCATTR(LATTRP,GATTRP);

              END
            ELSE
              IF GATTRP@.TYPTR = REALPTR THEN
                BEGIN
                  NEEDRTPROC('P@LN    '); NEEDRTPROC('P@EXP   ');
                  IF COMPTYPES(LATTRP@.TYPTR,INTPTR) THEN
                     INTTOREAL(LATTRP);
                  LOAD(LATTRP,GATTRP);
                  LOADINTCONST(R0,REALREG(.LATTRP@.REXPR.RNO]);
                  CALLRTSYS(SYSLN);
                  REGULAROPERATION(LATTRP,MUL);
                  LOAD(GATTRP,LATTRP);
                  LOADINTCONST(R0,REALREG(.GATTRP@.REXPR.RNO]);
                  CALLRTSYS(SYSEXP);
                END;
            ATTRDISP(LATTRP);
          END;  (* EXPONENTIATION *)
      END  (* FACTOR *) ;

      PROCEDURE TERM(FSYS: SETOFSYS);
        VAR LATTRP: ATTRP; LOP: OPERATOR;
      BEGIN
        FACTOR(FSYS+(.MULOP,EXPONOP]);
        WHILE SY = MULOP DO
          BEGIN
            ATTRNEW(LATTRP); COPYATTR(GATTRP,LATTRP);
            LOP:=OP;
              INSYMBOL; FACTOR(FSYS+(.MULOP,EXPONOP]);
            IF (LATTRP@.TYPTR <> NIL) AND (GATTRP@.TYPTR <> NIL) THEN
              CASE LOP OF
 (***)          MUL:  REGULAROPERATION(LATTRP,MUL);
 (*/*)          RDIV: IF COMPTYPES(LATTRP@.TYPTR,INTPTR) OR
                           (LATTRP@.TYPTR = REALPTR) THEN
                        IF COMPTYPES(GATTRP@.TYPTR,INTPTR) OR
                             (GATTRP@.TYPTR = REALPTR) THEN
                          REALARITH(LATTRP,GATTRP,RDIV)
                        ELSE ERRORRESET(134)
                      ELSE ERRORRESET(134);
 (*DIV,MOD*)    IDIV,IMOD: IF COMPTYPES(LATTRP@.TYPTR,INTPTR) AND
                                COMPTYPES(GATTRP@.TYPTR,INTPTR) THEN
                             INTARITH(LATTRP,GATTRP,LOP)
                           ELSE ERRORRESET(134);
 (*AND*)        ANDOP: IF COMPTYPES(LATTRP@.TYPTR,BOOLPTR) AND
                           COMPTYPES(GATTRP@.TYPTR,BOOLPTR) THEN
                         BOOLARITH(LATTRP,GATTRP,ANDOP)
                       ELSE ERRORRESET(134)
              END  (* CASE *)
            ELSE GATTRP@.TYPTR := NIL;
            ATTRDISP(LATTRP)
          END  (* WHILE *) ;
      END;

      PROCEDURE SIMPLEEXPRESSION(FSYS: SETOFSYS);
        VAR LATTRP: ATTRP; LOP: OPERATOR;
      BEGIN
        LOP:=NOOP;
        IF OP IN (.PLUS,MINUS] THEN
          BEGIN LOP:=OP; INSYMBOL; END;
        TERM(FSYS+(.ADDOP]);
        IF LOP <> NOOP THEN
          BEGIN
            IF NOT ((GATTRP@.TYPTR = REALPTR) OR COMPTYPES(GATTRP@.TYPTR,INTPTR))
              THEN ERRORRESET(134)
              ELSE IF LOP = MINUS THEN NEGATE(GATTRP);
          END;
        WHILE SY = ADDOP DO
          BEGIN ATTRNEW(LATTRP); COPYATTR(GATTRP,LATTRP); LOP:=OP;
            INSYMBOL; TERM(FSYS+(.ADDOP]);
            IF (LATTRP@.TYPTR <> NIL) AND (GATTRP@.TYPTR <> NIL) THEN
              CASE LOP OF
 (*+,-*)        PLUS,MINUS:
                  REGULAROPERATION(LATTRP,LOP);
 (*OR*)         OROP:
                  IF COMPTYPES(LATTRP@.TYPTR,BOOLPTR) AND COMPTYPES(GATTRP@.TYPTR,BOOLPTR)
                    THEN BOOLARITH(LATTRP,GATTRP,OROP)
                    ELSE ERRORRESET(134)
              END  (* CASE *)
            ELSE GATTRP@.TYPTR := NIL;
            ATTRDISP(LATTRP);
          END (* WHILE *) ;
      END;

    BEGIN  (* EXPRESSION *)
      SIMPLEEXPRESSION(FSYS+(.RELOP]);
      IF SY = RELOP THEN
        BEGIN
          LOP:=OP;
          ATTRNEW(LATTRP); COPYATTR(GATTRP,LATTRP);
          INSYMBOL; SIMPLEEXPRESSION(FSYS);
          IF (LATTRP@.TYPTR <> NIL) AND (GATTRP@.TYPTR <> NIL) THEN
            IF LOP = INOP THEN
            BEGIN
              FOLDANYCHAR(LATTRP);
              IF GATTRP@.TYPTR@.FORM = POWER THEN
                IF COMPTYPES(LATTRP@.TYPTR,GATTRP@.TYPTR@.ELSET) THEN
                  INPOWER(LATTRP,GATTRP)
                ELSE ERRORRESET(129)
              ELSE ERRORRESET(130)
            END
            ELSE
              BEGIN
                IF COMPTYPES(LATTRP@.TYPTR,INTPTR) AND
                   (GATTRP@.TYPTR = REALPTR) THEN INTTOREAL(LATTRP);
                IF COMPTYPES(GATTRP@.TYPTR,INTPTR) AND
                   (LATTRP@.TYPTR = REALPTR) THEN INTTOREAL(GATTRP);
                IF NOT COMPTYPES(LATTRP@.TYPTR,GATTRP@.TYPTR) THEN
                  ERRORRESET(129)
                ELSE
                  CASE LATTRP@.TYPTR@.FORM OF
                    SCALAR,SUBRANGE,PACKDTYPE:
                      IF (LATTRP@.TYPTR = REALPTR)
                        THEN RELREAL(LATTRP,GATTRP,LOP)
                        ELSE RELINT(LATTRP,GATTRP,LOP);
                    POINTER:
                      IF LOP IN (.EQOP,NEOP]
                        THEN RELINT(LATTRP,GATTRP,LOP)
                        ELSE ERRORRESET(131);
                    POWER:
                      IF LOP IN (.LTOP,GTOP]
                        THEN ERRORRESET(132)
                        ELSE RELPOWER(LATTRP,GATTRP,LOP);
                    ARRAYS,RECORDS:
                      IF STRING(LATTRP@.TYPTR) THEN RELLONG(LATTRP,GATTRP,LOP)
                        ELSE IF LOP IN (.LTOP,GTOP,LEOP,GEOP]
                          THEN ERROR(131)
                          ELSE ERROR(399);
                    FILES:
                      ERRORRESET(133)
                  END  (* CASE *) ;
                END  (* SY <> INOP *)
              ELSE GATTRP@.TYPTR := NIL;
          ATTRDISP(LATTRP);
        END  (* SY = RELOP *) ;
    END  (* EXPRESSION *) ;

    PROCEDURE COMPOUNDSTATEMENT(FSYS:SETOFSYS); FORWARD;

    PROCEDURE STATEMENT(FSYS: SETOFSYS);
      LABEL 1;
      VAR LCP: CTP; LLP: LBP; LCIX: ADDRRANGE;

      PROCEDURE GENFJMP(FADDR:ADDRRANGE);
        VAR X: INTEGER;
        BEGIN
          IF NOT COMPTYPES(GATTRP@.TYPTR,BOOLPTR) THEN ERROR(145)
          ELSE IF GATTRP@.TYPTR <> NIL THEN
            BEGIN
              LOAD(GATTRP,NIL);
              IF BOOLFLAG THEN
                BEGIN IC:=IC-10;
                      X:=15-GETCODE(IC+4) MOD 256 DIV 16;
                      GENRX(ZBC,X,0,PBASE1,FADDR);
                END
              ELSE
                BEGIN GENRRP1(ZLTR,GATTRP@.REXPR.RNO);
                      GENRX(ZBC,CONDZ,0,PBASE1,FADDR);
                END;
            END;
        END;

      PROCEDURE GENJMP(FADDR:ADDRRANGE);
        BEGIN
          GENRX(ZBC,15,0,PBASE1,FADDR);
        END;

      PROCEDURE PREPFJMP(VAR FIX: ADDRRANGE);
        BEGIN
          GENFJMP(-4096*PBASE1); FIX:=IC-4;
        END;

      PROCEDURE PREPJMP(VAR FIX: ADDRRANGE);
        BEGIN
          FIX:=IC; GENRX(ZBC,15,0,0,0);
        END;

      PROCEDURE ASSIGNMENT(FCP: CTP);
        VAR LATTRP: ATTRP;
      BEGIN
        SELECTOR(FSYS+(.BECOMES],FCP);
          IF FCP@.KLASS = VARS THEN
            IF FCP@.CNTRLVAR THEN ERROR(190);
        IF SY = BECOMES THEN
          BEGIN
            ATTRNEW(LATTRP); COPYATTR(GATTRP,LATTRP);
            INSYMBOL; EXPRESSION(FSYS);
            IF (LATTRP@.TYPTR <> NIL) AND (GATTRP@.TYPTR <> NIL) THEN
              STORE(LATTRP,GATTRP,129);
            ATTRDISP(LATTRP); RESETG;
          END
        ELSE ERROR(51);
      END;

      PROCEDURE GOTOSTATEMENT;
        LABEL 1;
        VAR LLP: LBP; LCIX: ADDRRANGE;
      BEGIN
        IF SY = INTCONST THEN
          BEGIN LLP := FSTLABP;
            WHILE LLP <> FLABP DO  (* DECIDE WHETHER LOCALLY DECLARED *)
              WITH LLP@ DO
                IF LABVAL = IVAL THEN
                  BEGIN
                    IF DEFINED THEN GENJMP(LABADDR)
                    ELSE
                      BEGIN PREPJMP(LCIX); LINKOCC(FSTOCC,LCIX); END;
                    GOTO 1
                  END
                ELSE LLP := NEXTLAB;
            WHILE LLP <> NIL DO
              WITH LLP@ DO
                IF LABVAL <> IVAL THEN LLP:=NEXTLAB
                ELSE
                  BEGIN
                    IF LCNT = 0 THEN
                      IF PCNT >= MAXPROCFUNC THEN ERROR(261)
                      ELSE BEGIN
                             PCNT:=PCNT+1; LCNT:=PCNT;
                           END;
                    IF EXTRNL THEN
                      GENRX(ZLA,15,0,0,PROCBASE+4*LCNT-8)
                    ELSE GENRX(ZLA,15,0,0,PROCBASE+4*LCNT-4);
                      GENRX(ZLA,9,0,0,240);
                      GENRX(ZEX,9,0,1,8);
                      GENRR(ZBCR,15,15);
                      GOTO 1;
                  END;
            ERROR(167);
      1:    INSYMBOL
          END
        ELSE ERROR(15);
      END  (* GOTOSTATEMENT *) ;

      PROCEDURE IFSTATEMENT;
        VAR LCIX1,LCIX2: ADDRRANGE;
      BEGIN EXPRESSION(FSYS+(.THENSY]);
        PREPFJMP(LCIX1); RESETG;
        ACCEPT(THENSY,52);
        STATEMENT(FSYS+(.ELSESY]);
        IF SY = ELSESY THEN
          BEGIN PREPJMP(LCIX2); INSERTIC(LCIX1); INSYMBOL;
                STATEMENT(FSYS); INSERTIC(LCIX2);
          END
        ELSE INSERTIC(LCIX1);
      END;

      PROCEDURE CASESTATEMENT;
        LABEL 1,2;
        TYPE CIP = @CASEREC;
          CASEREC =
            RECORD NEXT: CIP;
              CSLAB: INTEGER;
              CSADDR: ADDRRANGE;
            END;
        VAR LSP,LSP1: STP; FSTPTR,LPT1,LPT2,LPT3: CIP; LVAL: VALU;
            SWITCHIX,LCIX: ADDRRANGE;
            LMIN,LMAX: INTEGER;
            JUMPREG: REGNO; CHAIN: LOCOFREF;
            INDEXJUMP: BOOLEAN;
            COUNT: INTEGER;
            DEFAULT: INTEGER;  (* IC OF DEFAULT CASE OR UNDEFINED CASE JUMP *)
            CHECKRANGE: BOOLEAN;
            LSP2: STP;

        PROCEDURE GENSWITCH(FCIP: CIP);
          VAR LVAL,JUMPBASE: INTEGER;
        BEGIN
          IF ((LMAX-LMIN) < CIXMAX) AND INDEXJUMP THEN
            BEGIN
              IF CHECKRANGE THEN
                BEGIN
                  MAKEINTCONST(LMIN); GENRX(ZC,REALREG(.JUMPREG],0,0,0);
                  GENRX(ZBC,CONDM,0,PBASE1,DEFAULT);
                  MAKEINTCONST(LMAX); GENRX(ZC,REALREG(.JUMPREG],0,0,0);
                  GENRX(ZBC,CONDP,0,PBASE1,DEFAULT);
                END;
              GENRRP1(ZAR,JUMPREG);
              JUMPBASE := IC+8 - 2*LMIN;
              IF (JUMPBASE < 0) OR (JUMPBASE >= 4096) THEN
                BEGIN GENRR(ZLR,BASEWORK,PBASE1); MAKEINTCONST(JUMPBASE+6);
                      GENRX(ZA,BASEWORK,0,0,0);
                      GENRX(ZLH,BASEWORK,REALREG(.JUMPREG],BASEWORK,0);
                END
              ELSE GENRX(ZLH,BASEWORK,REALREG(.JUMPREG],PBASE1,JUMPBASE);
              GENRX(ZBC,15,BASEWORK,PBASE1,0);
              LVAL:=LMIN;
              REPEAT
                WITH FCIP@ DO
                  BEGIN
                    WHILE CSLAB > LVAL DO
                      BEGIN
                        MAKECODE(IC,DEFAULT); IC:=IC+2;
                            LVAL:=LVAL+1;
                      END;
                    MAKECODE(IC,CSADDR); IC:=IC+2;
                    LVAL := LVAL +1; FCIP := NEXT
                  END
              UNTIL FCIP = NIL
            END
          ELSE
            BEGIN
              REPEAT
                WITH FCIP@ DO
                  BEGIN MAKEINTCONST(CSLAB); GENRXP(ZC,JUMPREG,0,0,0);
                    GENRX(ZBC,CONDZ,0,PBASE1,CSADDR); FCIP:=NEXT;
                  END;
              UNTIL FCIP = NIL;
              GENJMP(DEFAULT);
            END;
        END;

      BEGIN
        EXPRESSION(FSYS+(.OFSY,COMMA,COLON]);
        LSP := GATTRP@.TYPTR;
        IF LSP <> NIL THEN
          IF (LSP@.FORM > SUBRANGE) OR (LSP = REALPTR) THEN
            BEGIN ERROR(144); LSP := NIL;
              JUMPREG := R10
            END
          ELSE
            BEGIN LOAD(GATTRP,NIL); JUMPREG := GATTRP@.REXPR.RNO END
        ELSE JUMPREG := R10;
        RESETG;
        PREPJMP(SWITCHIX);
        ACCEPT(OFSY,8);
        DEFAULT := 0;
        FSTPTR:=NIL; LPT3:=NIL; CHAIN:=NIL;

        (* LOOP UNTIL SY <> SEMICOLON *)
        while true do begin
          IF NOT (SY IN (.SEMICOLON,OTHERWSY,ENDSY]) THEN
            BEGIN
              (* LOOP UNTIL SY <> COMMA: *)
              IF SY = ELSESY THEN
                BEGIN  (* "ELSE:" DEFAULT LABEL *)
                  EXTENSION(324);
                  INSYMBOL; IF DEFAULT = 0 THEN DEFAULT:=IC ELSE ERROR(156)
                END
              ELSE
                while true do begin  (* CASE LABEL LIST *)
                  CONSTANT(FSYS+(.COMMA,COLON],LSP1,LVAL);
                  LMIN:=0;
                  IF LSP1 <> NIL THEN
                    IF COMPTYPES(LSP,LSP1) THEN LMIN:=LVAL.IVAL
                      ELSE BEGIN ERROR(147); LSP1:=NIL END;
                  IF (SY = COLON) AND DOTDOT THEN
                    BEGIN  (* SUBRANGE CASE LABEL *)
                      EXTENSION(325);
                      INSYMBOL; CONSTANT(FSYS+(.COMMA,COLON],LSP2,LVAL);
                      IF LSP2 <> NIL THEN
                        IF COMPTYPES(LSP,LSP2) THEN
                          IF LMIN <= LVAL.IVAL THEN LMAX:=LVAL.IVAL
                            ELSE BEGIN ERROR(102); LMAX:=LMIN; LMIN:=LVAL.IVAL; END
                        ELSE BEGIN ERROR(147); LSP2:=NIL END
                    END
                  ELSE BEGIN LSP2:=LSP1; LMAX:=LMIN END;
                  IF (LSP1 <> NIL) AND (LSP2 <> NIL) THEN
                    BEGIN LPT1:=FSTPTR; LPT2:=NIL;
                      WHILE LPT1 <> NIL DO
                        WITH LPT1@ DO
                          IF LMIN <= CSLAB THEN
                            IF (CSLAB = LMIN) OR (LMAX >= CSLAB) THEN
                              BEGIN ERROR(156); GOTO 2 END
                            ELSE GOTO 1
                          ELSE BEGIN LPT2:=LPT1; LPT1:=NEXT END;
                 1:    FOR COUNT:=LMIN TO LMAX DO
                         BEGIN
                           NEW(LPT3);
                           WITH LPT3@ DO
                             BEGIN
                               CSLAB:=COUNT;
                               CSADDR:=IC
                             END;
                           IF LPT2 = NIL THEN FSTPTR:=LPT3
                             ELSE LPT2@.NEXT:=LPT3;
                           LPT2:=LPT3;
                         END;
                       LPT2@.NEXT:=LPT1
                     END;
               2:
                   IF SY <> COMMA THEN break; INSYMBOL;
                 END;  (* CASE LABEL LOOP *)
                 ACCEPT(COLON,5);
                 REPEAT STATEMENT(FSYS+(.SEMICOLON,OTHERWSY]);
                   IF SY IN STATBEGSYS THEN ERROR(14);
                 UNTIL NOT (SY IN STATBEGSYS);
                 PREPJMP(LCIX); LINKOCC(CHAIN,LCIX);
            END  (* SY <> ENDSY *) ;
          IF SY <> SEMICOLON THEN break; INSYMBOL;
        END;  (* CASE STATEMENT LOOP *)

        IF SY = OTHERWSY THEN
          BEGIN
            EXTENSION(324);
            INSYMBOL; IF DEFAULT = 0 THEN DEFAULT := IC ELSE ERROR(156);
            REPEAT
              STATEMENT(FSYS+(.SEMICOLON,ENDSY]);
              IF SY IN STATBEGSYS THEN ERROR(14);
            UNTIL NOT (SY IN STATBEGSYS);
            WHILE SY = SEMICOLON DO
              BEGIN
                INSYMBOL;
                REPEAT STATEMENT(FSYS+(.SEMICOLON,ENDSY])
                UNTIL NOT (SY IN STATBEGSYS);
              END;
            PREPJMP(LCIX); LINKOCC(CHAIN,LCIX);
          END;  (* OTHERWISE *)

        IF FSTPTR <> NIL THEN
          BEGIN
            LPT1:=FSTPTR;
            WHILE LPT1 <> NIL DO
              BEGIN LPT2:=LPT1; LPT1:=LPT1@.NEXT; END;
            LMAX:=LPT2@.CSLAB; LMIN:=FSTPTR@.CSLAB;
            IF (LMAX > MXINT DIV 2-4098) OR (LMIN < -MXINT DIV 2) OR (IC > 32760)
              THEN INDEXJUMP:=FALSE
              ELSE INDEXJUMP:=TRUE;
            CHECKRANGE := DEBUG OR (DEFAULT <> 0);
            IF DEFAULT = 0 THEN
              BEGIN  (* NO ELSE CASE.  DEFAULT = UNDEFINED CASE LABEL ERROR *)
                DEFAULT := IC;
                CALLRTSYS(SYSCASER);
              END;
            INSERTIC(SWITCHIX);
            GENSWITCH(FSTPTR);
            INSERTCHAIN(CHAIN);
          END
        ELSE ERROR(6);

        IF SY = ENDSY THEN
          BEGIN
            RIGHTCHECK; INSYMBOL
          END
        ELSE ERROR(13);
      END  (* CASESTATEMENT *) ;

      PROCEDURE REPEATSTATEMENT;
        VAR LADDR: ADDRRANGE;
      BEGIN
        LADDR := IC;
        REPEAT
          STATEMENT(FSYS+(.SEMICOLON,UNTILSY]);
          IF SY IN STATBEGSYS THEN ERROR(14)
        UNTIL NOT (SY IN STATBEGSYS);
        WHILE SY = SEMICOLON DO
          BEGIN INSYMBOL;
            REPEAT STATEMENT(FSYS+(.SEMICOLON,UNTILSY])
            UNTIL NOT (SY IN STATBEGSYS);
          END;
        IF SY = UNTILSY THEN
          BEGIN
            RIGHTCHECK; INSYMBOL; EXPRESSION(FSYS);
            GENFJMP(LADDR); RESETG;
          END
        ELSE ERROR(53);
      END;

      PROCEDURE WHILESTATEMENT;
        VAR LADDR,LCIX: ADDRRANGE;
      BEGIN
        LADDR:=IC;
        EXPRESSION(FSYS+(.DOSY]);
        PREPFJMP(LCIX); RESETG;
        ACCEPT(DOSY,54);
        STATEMENT(FSYS);
        GENJMP(LADDR); INSERTIC(LCIX);
      END;

      PROCEDURE LOOPSTATEMENT;
        VAR OLDTOP: DISPRANGE; CHAIN: LOCOFREF; LCIX,LADDR: ADDRRANGE;
            LCP,LCP1: CTP;
      BEGIN
        EXTENSION(323); INSYMBOL;
        CHAIN:=NIL; OLDTOP:=TOP;
        IF TOP < DISPLIMIT THEN
          BEGIN TOP:=TOP+1; DISPLAY(.TOP].FNAME:=NIL;
                DISPLAY(.TOP].OCCUR:=REC;
          END
        ELSE ERROR(250);
        LCP1:=NIL; NEW(LCP,EVENT);
        WITH LCP@ DO
          BEGIN NAME:='LOOP    '; IDTYPE:=NIL; NEXT:=LCP1;
                EVENTJUMP:=NIL; EVENTDEF:=FALSE;
          END;
        ENTERID(LCP); LCP1:=LCP;
        IF SY = UNTILSY THEN
          BEGIN
            REPEAT INSYMBOL;
              IF SY <> IDENT THEN
                BEGIN ERROR(2); SKIP(FSYS+(.COMMA,COLON]); END
              ELSE
                BEGIN NEW(LCP,EVENT);
                  WITH LCP@ DO
                    BEGIN NAME:=ID; NEXT:=LCP1; IDTYPE:=NIL;
                          EVENTJUMP:=NIL; EVENTDEF:=FALSE;
                    END;
                  ENTERID(LCP); LCP1:=LCP; INSYMBOL;
                END;
            UNTIL SY <> COMMA;
            IF SY = COLON THEN INSYMBOL ELSE ERROR(5);
          END;
        LADDR:=IC;
        REPEAT STATEMENT(FSYS+(.SEMICOLON,ENDSY,POSTSY]);
          IF SY IN STATBEGSYS THEN ERROR(14);
        UNTIL NOT (SY IN STATBEGSYS);
        WHILE SY = SEMICOLON DO
          BEGIN INSYMBOL;
            REPEAT STATEMENT(FSYS+(.SEMICOLON,ENDSY,POSTSY]);
            UNTIL NOT (SY IN STATBEGSYS);
          END;
        GENJMP(LADDR);
        IF SY = POSTSY THEN
          BEGIN
            REPEAT INSYMBOL;
              IF SY <> IDENT THEN
                BEGIN ERROR(2); SKIP(FSYS+(.COLON]); END
              ELSE
                BEGIN SEARCHID((.EVENT],LCP);
                  WITH LCP@ DO
                    IF DISX <> TOP THEN ERROR(280)
                      ELSE IF NAME = 'LOOP    ' THEN ERROR(281)
                        ELSE IF EVENTDEF THEN ERROR(282)
                          ELSE
                            BEGIN INSERTCHAIN(EVENTJUMP);
                              EVENTJUMP:=NIL; EVENTDEF:=TRUE;
                            END;
                  INSYMBOL;
                  ACCEPT(COLON,5);
                END;
              REPEAT STATEMENT(FSYS+(.SEMICOLON]);
                IF SY IN STATBEGSYS THEN ERROR(14);
              UNTIL NOT (SY IN STATBEGSYS);
              PREPJMP(LCIX); LINKOCC(CHAIN,LCIX);
            UNTIL SY <> SEMICOLON;
          END;
        IF SY = ENDSY THEN
        BEGIN RIGHTCHECK; INSYMBOL END ELSE ERROR(13);
        WHILE LCP1 <> NIL DO
          BEGIN INSERTCHAIN(LCP1@.EVENTJUMP);
            LCP1:=LCP1@.NEXT;
          END;
        INSERTCHAIN(CHAIN);
        TOP:=OLDTOP;
      END;

      PROCEDURE CONTROLVARIABLE(VAR LCP:CTP; VAR CVAR1,CVAR2:INTEGER);
      BEGIN INSYMBOL;
        CVAR1 := 0; CVAR2 := 0;
        IF SY = IDENT THEN
          BEGIN SEARCHID((.VARS],LCP);
            IF LCP = UVARPTR THEN
              SKIP(FSYS+(.DOSY])
            ELSE
              BEGIN
                WITH LCP@ DO
                  IF IDTYPE <> NIL THEN
                    IF (IDTYPE@.FORM > SUBRANGE) OR (IDTYPE = REALPTR)
                        OR (IDTYPE@.FORM = PACKDTYPE) THEN ERROR(143)
                    ELSE IF VKIND = DRCT THEN
                      BEGIN CVAR1:=VLEV; CVAR2:=VADDR;
                        IF VLEV <> LEVEL THEN ERROR(177);
                        IF CNTRLVAR THEN ERROR(190);
                        CNTRLVAR:=TRUE;
                      END
                    ELSE ERROR(155);
                INSYMBOL;
              END
          END
        ELSE
          BEGIN ERROR(2); SKIP(FSYS+(.DOSY]);
                LCP:=UVARPTR;
          END;
      END;

      PROCEDURE FORSTATEMENT;
        VAR LIMITP,INITIALP: ATTRP; LSP: STP; LSY: SYMBOL;
            LCIX,LCIX1: ADDRRANGE; LCP: CTP;
            LMIN,LMAX: INTEGER; LADDR: ADDRRANGE;
            CVAR1,CVAR2: INTEGER;  (* ADDRESS OF CONTROL VARIABLE *)
            COND: INTEGER; TEMP: CMP;

        PROCEDURE COMPARETOLIMIT;
          (* COMPARE R0 TO FOR LOOP LIMIT *)
        BEGIN
          IF LIMITP@.TYPTR <> NIL THEN
            IF LIMITP@.KIND = CST THEN
              BEGIN MAKECONSTANT(LIMITP@.CVAL);
                    GENRX(ZC,R0,0,0,0);
              END
            ELSE
              BEGIN BASEREGISTER(LEVEL,LIMITP@.REXPR.ATEMP@.TEMPADRS);
                    GENRX(ZC,R0,0,RBASE,EFFADRS);
              END;
        END;  (* COMPARETOLIMIT *)

      BEGIN  (* FORSTATEMENT *)
        CONTROLVARIABLE(LCP,CVAR1,CVAR2);
        IF SY = DOSY THEN SY := SEMICOLON
        ELSE
          BEGIN
            ATTRNEW(INITIALP);
            INITIALP@.TYPTR:=NIL;
            IF SY = BECOMES THEN
              BEGIN  (* INITIAL VALUE *)
                INSYMBOL;
                EXPRESSION(FSYS+(.TOSY,DOWNTOSY,DOSY]);
                COPYATTR(GATTRP,INITIALP)
              END
            ELSE
              BEGIN ERROR(51); SKIP(FSYS+(.TOSY,DOWNTOSY,DOSY]) END;
            LSY := SY; ATTRNEW(LIMITP); LIMITP@.TYPTR := NIL;
            IF SY IN (.TOSY,DOWNTOSY] THEN
              BEGIN  (* LIMIT VALUE *)
                INSYMBOL; EXPRESSION(FSYS+(.DOSY]);
                IF GATTRP@.TYPTR <> NIL THEN
                  IF COMPTYPES(LCP@.IDTYPE,GATTRP@.TYPTR) THEN
                    BEGIN COPYATTR(GATTRP,LIMITP);
                      IF LIMITP@.KIND <> CST THEN
                        BEGIN LOAD(LIMITP,NIL); SAVE(LIMITP@.REXPR.RNO); END;
                    END
                  ELSE ERROR(145)
              END
            ELSE BEGIN ERROR(55); SKIP(FSYS+(.DOSY]) END;
            IF INITIALP@.TYPTR <> NIL THEN
              IF COMPTYPES(LCP@.IDTYPE,INITIALP@.TYPTR) THEN
                BEGIN LOAD(INITIALP,NIL);
                  GENRR(ZLR,R0,REALREG(.INITIALP@.REXPR.RNO]);
                END
              ELSE ERROR(145);
            ATTRDISP(INITIALP);
            IF LSY = TOSY THEN COND:=CONDNP ELSE COND:=CONDNM;
            COMPARETOLIMIT;
            LCIX1 := IC;
            GENRX(ZBC,COND,0,0,0);
            PREPJMP(LCIX);  (* JUMP IF LOOP NOT EXECUTED *)

            (* HEAD OF LOOP *)
            LADDR := IC;
            IF LSY = TOSY THEN
              BEGIN MAKEINTCONST(1); GENRX(ZA,R0,0,0,0) END
            ELSE GENRR(ZBCTR,R0,0);
            (* INITIAL PASS STARTS HERE *)
            INSERTIC(LCIX1);
            IF DEBUG THEN
              BEGIN
                IF LCP@.IDTYPE <> NIL THEN
                  IF SUBRNGTYPE(LCP@.IDTYPE) THEN
                    BEGIN GETBOUNDS(LCP@.IDTYPE,LMIN,LMAX);
                      CHECKREGISTER(R0,LMIN,LMAX,ENTCKRANGE,ENTOUTRANGE);
                    END;
                GETTEMP(4,TEMP);
                BASEREGISTER(LEVEL,TEMP@.TEMPADRS);
                GENRX(ZST,R0,0,RBASE,EFFADRS);
              END;
            BASEREGISTER(CVAR1,CVAR2);
            GENRX(ZST,R0,0,RBASE,EFFADRS);
            ACCEPT(DOSY,54);
            STATEMENT(FSYS);
            BASEREGISTER(CVAR1,CVAR2);
            GENRX(ZL,R0,0,RBASE,EFFADRS);
            IF DEBUG THEN
              BEGIN
                BASEREGISTER(LEVEL,TEMP@.TEMPADRS);
                GENRX(ZC,R0,0,RBASE,EFFADRS);
                GENRX(ZBC,CONDZ,0,PBASE1,IC+10);
                CALLRTSYS(SYSFORER);
              END;
            LCP@.CNTRLVAR := FALSE;
            COMPARETOLIMIT;
            GENRX(ZBC,CONDNZ,0,PBASE1,LADDR);
            INSERTIC(LCIX);

            (* UNDEFINE CONTROL VARIABLE AFTER LOOP *)
            IF DEBUG THEN
              BEGIN BASEREGISTER(CVAR1,CVAR2);
                GENRX(ZL,R0,0,1,ENTUNDEFWD);
                GENRX(ZST,R0,0,RBASE,EFFADRS);
              END;
            ATTRDISP(LIMITP);
            IF DEBUG THEN DELETETEMP(TEMP)
          END
      END  (* FORSTATEMENT *) ;

      PROCEDURE FORALLSTATEMENT;
        LABEL 9;
        VAR LCP: CTP; CVAR1,CVAR2: INTEGER; SETREG: INTEGER;
            TEMP: CMP; LADDR,LCIX: ADDRRANGE;
      BEGIN
        EXTENSION(322);
        CONTROLVARIABLE(LCP,CVAR1,CVAR2);
        IF SY = DOSY THEN SY := SEMICOLON
        ELSE
          BEGIN
            IF OP = INOP THEN
              BEGIN
                INSYMBOL;
                EXPRESSION(FSYS+(.DOSY]);
                IF GATTRP@.TYPTR <> NIL THEN
                  IF GATTRP@.TYPTR@.FORM <> POWER THEN ERROR(130)
                  ELSE IF COMPTYPES(LCP@.IDTYPE,GATTRP@.TYPTR@.ELSET)
                    THEN BEGIN LOAD(GATTRP,NIL); SETREG:=REALREG(.GATTRP@.REXPR.RNO]; END
                    ELSE BEGIN ERROR(129); SETREG:=10; END;
              END
            ELSE BEGIN ERROR(60); SKIP(FSYS+(.DOSY]); SETREG:=10; END;
            IF SY = DOSY THEN INSYMBOL ELSE ERROR(54);
            RESETG; GENRR(ZXR,R0,R0); LADDR:=IC; GENRR(ZLTR,SETREG,SETREG);
            LCIX:=IC; GENRX(ZBC,CONDNM,0,0,0); BASEREGISTER(CVAR1,CVAR2);
            GENRX(ZST,R0,0,RBASE,EFFADRS); GETTEMP(8,TEMP);
            BASEREGISTER(LEVEL,TEMP@.TEMPADRS);
            GENRX(ZSTM,SETREG,SETREG+1,RBASE,EFFADRS);
            STATEMENT(FSYS);
            BASEREGISTER(CVAR1,CVAR2); GENRX(ZL,R0,0,RBASE,EFFADRS);
            BASEREGISTER(LEVEL,TEMP@.TEMPADRS); GENRX(ZLM,SETREG,SETREG+1,RBASE,EFFADRS);
            INSERTIC(LCIX); MAKEINTCONST(1);
            GENRX(ZA,R0,0,0,0); MAKEINTCONST(64); GENRX(ZCL,R0,0,0,0);
            GENRX(ZBC,CONDNM,0,PBASE1,IC+12); GENRX(ZSLDL,SETREG,0,0,1);
            GENJMP(LADDR); DELETETEMP(TEMP);
   9:     END;
        LCP@.CNTRLVAR := FALSE;
      END;

      PROCEDURE WITHSTATEMENT;
        VAR LCP: CTP; OLDTOP: DISPRANGE; OLDLEVEL: LEVRANGE;
            TEMP: CMP;
      BEGIN OLDTOP:=TOP; OLDLEVEL:=DISPLEVEL;
        REPEAT INSYMBOL;
          IF SY = IDENT THEN
            BEGIN SEARCHID((.VARS,FIELD],LCP); INSYMBOL END
          ELSE BEGIN ERROR(2); LCP := UVARPTR END;
          SELECTOR(FSYS+(.COMMA,DOSY],LCP);
          IF GATTRP@.TYPTR <> NIL THEN
            IF GATTRP@.TYPTR@.FORM = RECORDS THEN
              IF TOP < DISPLIMIT THEN
                BEGIN TOP := TOP + 1;
                  WITH DISPLAY(.TOP], GATTRP@ DO
                    BEGIN FNAME:=TYPTR@.FIELDS; OCCUR:=REC;
                      IF ((ACCESS = INDIRECT) OR (VARKIND = INDRCT)) AND
                          (DISPLEVEL >= 5) AND
                          ((LEVEL < 5) OR ((LEVEL = 5) AND (DISPLEVEL = 6))) THEN
                        BEGIN
                          DADRS:=0; DISPKIND:=DRCT;
                          IF DISPLEVEL = 6 THEN REG6USED := TRUE
                            ELSE REG5USED := TRUE;
                          DLEVEL:=DISPLEVEL; LOADADDRESS(GATTRP,NIL);
                          GENRR(ZLR,DISPLEVEL,REALREG(.GATTRP@.REXPR.RNO]);
                          DISPLEVEL:=DISPLEVEL-1;
                        END
                      ELSE IF ACCESS = DIRECT THEN
                        BEGIN DADRS:=VADRS; DISPKIND:=VARKIND;
                          IF VARKIND = DRCT THEN DLEVEL:=VLEVEL
                            ELSE BEGIN DBASEL:=BASELEV; DBASEA:=BASEADD; END;
                        END
                      ELSE
                        BEGIN DADRS:=0; DISPKIND:=INDRCT;
                          LOADADDRESS(GATTRP,NIL);
                          GETTEMP(4,TEMP);
                          BASEREGISTER(LEVEL,TEMP@.TEMPADRS); GENRXP(ZST,REXPR.RNO,0,RBASE,EFFADRS);
                          DBASEL:=LEVEL; DBASEA:=TEMP@.TEMPADRS;
                        END;
                      RESETG;
                    END
                END
              ELSE ERROR(250)
            ELSE ERROR(140);
        UNTIL SY <> COMMA;
        ACCEPT(DOSY,54);
        STATEMENT(FSYS);
        TOP:=OLDTOP; DISPLEVEL:=OLDLEVEL;
      END  (* WITHSTATEMENT *);

    BEGIN  (* STATEMENT *)
      IF SY = INTCONST THEN  (* LABEL *)
        BEGIN
          LLP := FSTLABP;
          WHILE LLP <> FLABP DO
           WITH LLP@ DO
            IF LABVAL = IVAL THEN
             BEGIN
              IF DEFINED THEN ERROR(165)
              ELSE
               BEGIN INSERTCHAIN(FSTOCC);
                DEFINED := TRUE; LABADDR := IC;
                IF LCNT <> 0 THEN  (* LONG JUMP *)
                  BEGIN
                    GENRX(ZBC,15,0,PBASE1,IC+12);
                    PROCADDRESS(.LCNT]:=PROGCOUNT+IC;
                    GENRR(ZLR,0,LEVEL);
                    CALLRTSYS(SYSLJUMP);
                  END;
               END;
              GOTO 1
             END
            ELSE LLP := NEXTLAB;
          ERROR(167);
     1:   INSYMBOL;
          ACCEPT(COLON,5);
         END;
        IF NOT (SY IN FSYS+(.IDENT]) THEN
         BEGIN ERROR(6); SKIP(FSYS) END;
        IF SY IN STATBEGSYS+(.IDENT] THEN
         BEGIN
          CASE SY OF
           IDENT:    BEGIN SEARCHID((.VARS,FIELD,FUNC,PROC,EVENT],LCP); INSYMBOL;
                       CASE LCP@.KLASS OF
                         PROC:  CALL(FSYS,LCP);
                         VARS,FIELD,FUNC:  ASSIGNMENT(LCP);
                         EVENT: BEGIN PREPJMP(LCIX); LINKOCC(LCP@.EVENTJUMP,LCIX); END
                       END;
                     END;
           BEGINSY:  BEGIN
                      LEFTCHECK; INSYMBOL;
                      COMPOUNDSTATEMENT(FSYS+(.SEMICOLON,ENDSY]);
                      INSYMBOL;
                     END;
           GOTOSY:   BEGIN INSYMBOL; GOTOSTATEMENT END;
           IFSY:     BEGIN INSYMBOL; IFSTATEMENT END;
           CASESY:   BEGIN LEFTCHECK; INSYMBOL; CASESTATEMENT END;
           WHILESY:  BEGIN INSYMBOL; WHILESTATEMENT END;
           REPEATSY: BEGIN LEFTCHECK; INSYMBOL; REPEATSTATEMENT END;
           LOOPSY:   BEGIN LEFTCHECK; LOOPSTATEMENT END;
           FORSY:    FORSTATEMENT;
           FORALLSY: FORALLSTATEMENT;
           WITHSY:   WITHSTATEMENT
          END;
          TESTFOR(FSYS,6,(.]);
         END;
    END  (* STATEMENT *) ;

    PROCEDURE COMPOUNDSTATEMENT  (* FSYS: SETOFSYS *) ;
    BEGIN
      REPEAT
        STATEMENT(FSYS);
        IF SY IN STATBEGSYS THEN ERROR(14);
      UNTIL NOT (SY IN STATBEGSYS);
      WHILE SY = SEMICOLON DO
        BEGIN INSYMBOL;
          REPEAT
            STATEMENT(FSYS);
            IF SY IN STATBEGSYS THEN ERROR(14);
          UNTIL NOT (SY IN STATBEGSYS)
        END;
      IF SY = ENDSY THEN
      RIGHTCHECK ELSE ERROR(13);
    END;

    PROCEDURE BODYINITIALIZE;
      VAR R: REGNO;
          I: INTEGER;
    BEGIN
      DP := FALSE;
      REG5USED:=FALSE; REG6USED:=FALSE;
      DISPLEVEL:=6;
      FOR I:=0 TO NCODESEGS-1 DO CODEPTR(.I]:=NIL;
      FOR R:=R10 TO F6 DO
        REGISTER(.R].USED:=FALSE;
      ATTRHEAD:=NIL;
      ATTRNEW(GATTRP);
      WITH GATTRP@ DO
        BEGIN TYPTR:=NIL; KIND:=CST; END;
      FREETEMP:=NIL;
      IC:=4; STACKTOP:=0;
      IF INITFLAG THEN
        BEGIN
          DATA1(0); DATA1(INITNUMBER);
          INITFLAG:=FALSE;
        END;
      EXTENDEDADDRESS:=FALSE;
      CONSTTOP:=NIL; STACKSIZE:=NIL;
      GENRR(ZLR,LEVEL,0);
      IF PMD OR PROCNAMES THEN
        BEGIN GENRX(ZBC,0,0,0,0); PMDPTREXISTS:=TRUE END
      ELSE PMDPTREXISTS:=FALSE;
      IF DEBUG THEN
        BEGIN
          IF LEVEL > 1 THEN
            BEGIN  (* BACKGROUND LOCAL STORAGE TO UNDEFINED *)
                   (* BUT DON'T CLOBBER VALUE INITIALIZATION ON LEVEL 1 *)
              MAKEINTCONST(LC1STLOCVAR);
              GENRX(ZA,R0,0,0,0);
              GENRX(ZBAL,BASEWORK,0,1,ENTUNDPROC);
            END;
          IF FPROCP@.KLASS = FUNC THEN   (* "UNDEFINE" FUNCTION RESULT *)
            GENSS(ZMVC,7,LEVEL,SAVEAREA,1,ENTUNDEFWD);
        END;
    END;

    PROCEDURE CLOSECODEGEN;
      VAR I,A,B,X,CODEEND,OP,A1,A2,Y,Z: INTEGER; P: CTAILP;
          LOCODEPTR: CODESPTR;
          STCNVRT: RECORD
                     CASE X: BOOLEAN OF
                       TRUE:(NME:ALFA);
                       FALSE:(A1,A2:INTEGER);
                   END;


      PROCEDURE ALIGNCONST(X:INTEGER);
        VAR X1,X2: INTEGER;
      BEGIN HALFWORD(X,X1,X2);
            IF IC >= 4096*(7-LEVEL)-2 THEN
            BEGIN ERROR(253); IC:=0 END;
            MAKECODE(IC,X1); MAKECODE(IC+2,X2);
            IC:=IC+4;
      END;


      PROCEDURE PMDINFO(FCP: CTP);
        VAR I,DISPT: INTEGER;
      BEGIN
        IF FCP <> NIL THEN
          WITH FCP@ DO
            BEGIN
              PMDINFO(LLINK);
              IF KLASS = VARS THEN
                IF IDTYPE <> NIL THEN
                  IF ((IDTYPE@.FORM <= POINTER) AND
                     (IDTYPE@.FORM <> PACKDTYPE)) OR
                      COMPTYPES(IDTYPE,ALFAPTR) THEN
                    BEGIN
                      IF IDTYPE@.FORM = POINTER THEN I:=0
                      ELSE IF COMPTYPES(IDTYPE,INTPTR)  THEN I:=2
                      ELSE IF COMPTYPES(IDTYPE,REALPTR) THEN I:=4
                      ELSE IF COMPTYPES(IDTYPE,CHARPTR) THEN I:=6
                      ELSE IF COMPTYPES(IDTYPE,BOOLPTR) THEN I:=8
                      ELSE IF COMPTYPES(IDTYPE,ALFAPTR) THEN I:=10
                      ELSE I:=12;
                      STCNVRT.NME := NAME;
                      IF VKIND = INDRCT THEN
                        BEGIN I := I + 1; DISPT := PARADDR END
                      ELSE DISPT := VADDR;
                      DATA1(16777216*I+DISPT);
                      DATA1(STCNVRT.A1);
                      DATA1(STCNVRT.A2);
                      A:=A+12;
                    END;
              PMDINFO(RLINK);
            END
      END;  (* PMDINFO *)


      PROCEDURE WRITEOBJECT;  (* WRITE SYMBOLIC CODE LISTING *)
      BEGIN I:=0;
        ENDOFLINE;
        WHILE I < CODEEND DO
          BEGIN NEWLINE;
            WRITE(' '); WRITEHEX(I); WRITE(' ');
            X:=GETCODE(I); WRITEHEX(X);
            OP:=X DIV 256; X:=X MOD 256;
            IF (OP < 64) OR (OP >= 254) THEN
              BEGIN WRITE(' ':14,MNEMONIC(.OP],'  ',X DIV 16:1,
                          ',', X MOD 16:1); I:=I+2;
              END
            ELSE IF OP < 192 THEN
              BEGIN Y:=GETCODE(I+2); WRITEHEX(Y);
                WRITE(' ':8,MNEMONIC(.OP],'  ',X DIV 16:1,',',
                      Y MOD 4096:1, '(', X MOD 16:1,
                      ',', Y DIV 4096:1, ')'); I:=I+4;
              END
            ELSE
              BEGIN
                Y := GETCODE(I+2); WRITEHEX(Y);
                Z := GETCODE(I+4); WRITEHEX(Z);
                WRITE('  ', MNEMONIC(.OP], '  ',
                      Y MOD 4096:1, '(', X+1:1, ',', Y DIV 4096:1,
                      '),', Z MOD 4096:1, '(', Z DIV 4096:1, ')'); I:=I+6;
              END;
            WRITELN;
          END;
        IF I MOD 4 <> 0 THEN
          BEGIN NEWLINE;
                WRITE(' '); WRITEHEX(I);
                WRITE(' '); WRITEHEX(GETCODE(I));
                WRITELN; I:=I+2;
          END;
        WHILE I < IC DO
          BEGIN NEWLINE;
            WRITE(' '); WRITEHEX(I); WRITE(' ');
            WRITEHEX(GETCODE(I));
            WRITEHEX(GETCODE(I+2));
            WRITELN; I:=I+4;
          END;
      END;  (* WRITEOBJECTCODE *)

    BEGIN  (* CLOSECODEGEN *)
      IF VARCHECK AND (FPROCP@.KLASS = FUNC) THEN
        BEGIN  (* CHECK THAT VALUE HAS BEEN ASSIGNED TO FUNCTION *)
          GENSS(ZCLC,7,LEVEL,SAVEAREA,1,ENTUNDEFWD);
          GENRX(ZBC,CONDNZ,0,PBASE1,IC+10);
          CALLRTSYS(SYSFUNCER);
        END;
      ALIGNMENT(LC,8);
      IF EXTRNL AND (LEVEL = 2) THEN
        BEGIN
          GENRX(ZLM,8,6,2,0);
          GENRX(ZST,0,0,1,8);
          GENRX(ZBC,15,0,9,2);
        END
      ELSE IF LEVEL = 1 THEN CALLRTSYS(SYSRETURN)
      ELSE GENRX(ZBC,15,0,1,ENTRETURN + 8*(LEVEL-2));
      CODEEND:=IC;
      IF IC MOD 4 <> 0 THEN
        BEGIN MAKECODE(IC,0); IC:=IC+2 END;
      WHILE CONSTTOP <> NIL DO
        WITH CONSTTOP@.SAVECONST DO
          BEGIN CASE CKIND OF
            INT: BEGIN INSERTCHAIN(CONSTTOP@.CCHAIN); ALIGNCONST(IVAL); END;
            REEL,PSET:
                 BEGIN
                   IF IC MOD 8 <> 0 THEN ALIGNCONST(0);
                   INSERTCHAIN(CONSTTOP@.CCHAIN);
                   SETVALUE(PVAL,A1,A2);
                   ALIGNCONST(A1); ALIGNCONST(A2);
                 END;
            STRG:BEGIN P:=VALP; INSERTCHAIN(CONSTTOP@.CCHAIN);   (* BOUNDARY CHECK (8*N) IS     *)
                   WHILE P <> NIL DO WITH P@ DO                  (* NOT NECESSARY. THE ONLY USE *)
                     BEGIN ALIGNCONST(STFR); P:=NXTCSP; END;     (* OF STRUCTURED CONSTANT IS   *)
                 END                                             (* ASSIGNMENT AS A WHOLE       *)
            END;  (* CASE *)
            CONSTTOP:=CONSTTOP@.NEXTCONST;
          END;
      IF PMDPTREXISTS THEN MAKECODE(8,IC)
        ELSE IF IC MOD 8 <> 0 THEN ALIGNCONST(0);
      HALFWORD(LC,A1,A2);
      IF EXTENDEDADDRESS THEN A1:=A1+4*256*LEVEL;
      MAKECODE(0,A1); MAKECODE(2,A2);
      IF REG6USED THEN
        IF IC > 4096*(6-LEVEL) THEN ERROR(253);
      IF REG5USED THEN
        IF IC > 4096*(5-LEVEL) THEN ERROR(253);
      X:=0; Y:=0; LOCODEPTR:=CODEPTR(.0];
      FOR A := 0 TO (IC DIV 4) -1 DO
        BEGIN
          DATA1(LOCODEPTR@.FULLWORDS(.X]);
          X:=X+1;
          IF X = CODEBLCK+1 THEN
            BEGIN
              X:=0; Y:=Y+1;
              LOCODEPTR:=CODEPTR(.Y];
            END;
        END;
      IF PMDPTREXISTS THEN
        BEGIN
          A:=0;
          STCNVRT.NME:=FPROCP@.NAME;
          DATA1(STCNVRT.A1); DATA1(STCNVRT.A2);
          A:=A+8;
          IF PMD THEN PMDINFO(DISPLAY(.LEVEL].FNAME);
          DATA1(0);
          IF (A + IC + 4) MOD 8 <> 0 THEN
            BEGIN A:=A+4; DATA1(0) END;
          PROGCOUNT:=PROGCOUNT+IC+A+4
        END
      ELSE PROGCOUNT:=PROGCOUNT+IC;

      IF PRINTCODE THEN WRITEOBJECTCODE;
    END;  (* CLOSECODEGEN *)

    PROCEDURE LDFILADR(FADDR: INTEGER);
      (* LOAD ADDRESS (LEVEL,FADDR) INTO R15 *)
    BEGIN
      IF FADDR < 4096 THEN GENRX(ZLA,15,0,LEVEL,FADDR)
      ELSE
        BEGIN MAKEINTCONST(FADDR);
          GENRX(ZL,15,0,0,0);
          GENRR(ZAR,15,LEVEL);
        END;
    END;  (* LDFILADR *)


    PROCEDURE DEFINE1FILE(FRECL,FADDR: ADDRRANGE;
                          FEXTRNAL,FTEXT,FAUTOGET: BOOLEAN; FNAME: ALFA);
      (* GEN CODE TO CALL RUNTIME SYSTEM TO DEFINE 1 FILE ON BLOCK ENTRY *)
      (* INITIALIZE BLOCK TO: H'LRECL',X'TYPE FLAGS',X'0',CL8'FILENAME'  *)
      (* OR H'LRECL',X'TYPE FLAGS',X'0',8X'00' IF LOCAL FILE.            *)
    VAR
      WNAME: RECORD CASE BOOLEAN OF
               FALSE: (STR: ALFA);
                TRUE: (INT: ARRAY(.1..2] OF INTEGER)
             END;
      I: INTEGER;

    BEGIN
      LDFILADR(FADDR);
      IF FTEXT THEN
        BEGIN I := BTTEXT * 256;
          IF NOT FAUTOGET THEN I := I + BTNOGET*256
        END
      ELSE
        BEGIN I := FRECL*65536;
          IF NOT FAUTOGET THEN ERROR(178)
        END;
      IF NOT FEXTRNAL THEN I := I + BTLOCAL*256;
      LOADINTCONST(R0,I); GENRX(ZST,R0,0,15,0);
      IF FEXTRNAL THEN
        BEGIN WNAME.STR := FNAME;
          LOADINTCONST(R0,WNAME.INT(.1]); GENRX(ZST,R0,0,15,4);
          LOADINTCONST(R0,WNAME.INT(.2]); GENRX(ZST,R0,0,15,8);
        END
      ELSE
        BEGIN
          GENSS(ZXC,7,15,4,15,4);
          NEEDRTPROC('P@LOCAL ')
        END;
      CALLRTSYS(SYSFDEF);
    END;  (* DEFINE1FILE *)

    PROCEDURE OPENFILES(FCP:CTP);
      VAR  EXTFILE, AUTOGET: BOOLEAN;
           CLSP, EXFILP: FILEP;

      PROCEDURE OPEN1(FSP:STP; FADDR:ADDRRANGE);
        VAR I,LMIN,LMAX,S: INTEGER; LCP:CTP;
        BEGIN
          IF FSP <> NIL THEN
            WITH FSP@ DO
              IF FORM IN (.RECORDS,ARRAYS,FILES] THEN
                CASE FORM OF
         RECORDS: BEGIN LCP:=FSTFLD;
                    WHILE LCP <> NIL DO
                      WITH LCP@ DO
                        BEGIN OPEN1(IDTYPE,FADDR+FLDADDR);
                              LCP:=NEXT;
                        END;
                  END;
         ARRAYS:  IF INXTYPE <> NIL THEN
                    BEGIN GETBOUNDS(INXTYPE,LMIN,LMAX);
                      FOR I:=0 TO LMAX-LMIN DO
                        OPEN1(AELTYPE,FADDR+AELLENG*I);
                    END;
         FILES:   BEGIN
                    IF TEXTFILE
                      THEN S:=-1
                      ELSE S:=SIZE.WBLENGTH-8;
                    DEFINE1FILE(S,FADDR,EXTFILE,TEXTFILE,AUTOGET,FCP@.NAME);
                    NEW(CLSP);
                    WITH CLSP@ DO
                      BEGIN NXTP:=LOCFILP; ADDR:=FADDR; END;
                    LOCFILP:=CLSP;
                  END
                END;
        END;

      BEGIN
        IF FCP <> NIL THEN
          WITH FCP@ DO
            BEGIN OPENFILES(LLINK); OPENFILES(RLINK);
              IF KLASS = VARS THEN
               IF VKIND = DRCT THEN
                IF IDTYPE <> NIL THEN
                  IF IDTYPE@.FTYPE THEN
                    BEGIN EXTFILE:=FALSE; AUTOGET:=TRUE;
                      IF IDTYPE@.FORM = FILES THEN
                        BEGIN EXFILP:=FEXFILP;
                          while true do begin IF EXFILP = NIL THEN break;
                            WITH EXFILP@ DO
                              IF FILENAME = NAME THEN
                                BEGIN EXTFILE:=TRUE;
                                  IF LEVEL > 1 THEN EXTENSION(337);
                                  AUTOGET:=GETONRESET;
                                  DECLARED:=TRUE; ADDR:=VADDR; break
                                END;
                            EXFILP:=EXFILP@.NXTP
                          END;  (* LOOP *)
                        END;
                      IF (FCP <> INPUTPTR) AND (FCP <> OUTPUTPTR) THEN
                        OPEN1 (IDTYPE,VADDR);
                    END;
            END;
      END;  (* OPENFILES *)


    PROCEDURE FILECHECK;
      VAR FP: FILEP; FIRST: BOOLEAN;
      BEGIN FP:=FEXFILP; FIRST:=TRUE;
        WHILE FP <> NIL DO
          WITH FP@ DO
            BEGIN
              IF NOT DECLARED THEN
                BEGIN
                  IF FIRST THEN
                    BEGIN ERROR(172); ENDOFLINE; FIRST:=FALSE;
                    END;
                  WRITELN(SYSTERM,'>>>':INDENTTY,'FILE-ID ',FILENAME);
                  NEWLINE;  WRITELN('>>>':INDENT,'FILE-ID ',FILENAME);
                END;
              FP:=NXTP;
            END;
      END;

    PROCEDURE LABELCHECK;
      BEGIN
        WHILE FSTLABP <> FLABP DO
          WITH FSTLABP@ DO
            BEGIN
              IF NOT DEFINED THEN
                BEGIN
                  IF (FSTOCC <> NIL) OR WARNING THEN
                    BEGIN
                      IF FSTOCC <> NIL THEN ERROR(168) ELSE ERROR(311);
                      ENDOFLINE;
                      WRITELN(SYSTERM,'>>>':INDENTTY,'LABEL ',LABVAL:1);
                      NEWLINE;  WRITELN('>>>':INDENT,'LABEL ',LABVAL:1);
                    END;
                END;
              FSTLABP := NEXTLAB
            END;
      END;

    PROCEDURE CLOSEFILES;
      (* PURGE FILES ON BLOCK EXIT.  NOTE: FILES MUST BE CLOSED IN REVERSE *)
      (* ORDER THEY WERE DEFINED OR R.T. SYSTEM WILL COMPLAIN BITTERLY.    *)

      PROCEDURE CLOSEALLFILES(FP:FILEP);
      BEGIN
        LDFILADR(FP@.ADDR);
        CALLRTSYS(SYSCLOSCR);
      END;


    BEGIN
      WHILE LOCFILP <> NIL DO
        BEGIN CLOSEALLFILES(LOCFILP); LOCFILP:=LOCFILP@.NXTP END;
      IF LEVEL = 1 THEN
        BEGIN  (* INPUT, OUTPUT ARE LAST 2 CLOSED *)
          IF INPUTPTR <> NIL THEN
            BEGIN
              LDFILADR(INPUTPTR@.VADDR);
              CALLRTSYS(SYSCLOSCR)
            END;
          IF OUTPUTPTR <> NIL THEN
            BEGIN
              LDFILADR(OUTPUTPTR@.VADDR);
              CALLRTSYS(SYSCLOSCR)
            END
        END
    END;  (* CLOSEFILES *)

  BEGIN  (* BODY *)
    BODYINITIALIZE;
    IF LEVEL = 1 THEN
      BEGIN
        (* OUTPUT & INPUT ARE FIRST 2 FILES OPENED, AND IN THAT ORDER. *)
        (* RUNTIME SYS. MAY DEPEND ON THIS FACT IF NEEDED.             *)
        (* AUTO RESET/REWRITE CODE GENERATED HERE ALSO.                *)
        IF OUTPUTPTR <> NIL THEN
          BEGIN
            DEFINE1FILE(-1,OUTPUTPTR@.VADDR,TRUE,TRUE,
                        AUTOGETOUTPUT,'OUTPUT  ');
            LDFILADR(OUTPUTPTR@.VADDR);
            CALLRTSYS(SYSREWRITE)
          END;
        IF INPUTPTR <> NIL THEN
          BEGIN
            DEFINE1FILE(-1,INPUTPTR@.VADDR,TRUE,TRUE,
                        AUTOGETINPUT,'INPUT   ');
            LDFILADR(INPUTPTR@.VADDR);
            CALLRTSYS(SYSRESET)
          END;
      END;
    OPENFILES(DISPLAY(.TOP].FNAME);
    IF LEVEL = 1 THEN FILECHECK;
    IF SY = BEGINSY THEN INSYMBOL;
    COMPOUNDSTATEMENT(FSYS+(.SEMICOLON,ENDSY]);
    IF LEVEL > 1 THEN PROCLEV:=CHR(ORD('A')+LEVEL-2);
    INSYMBOL;
    LABELCHECK;
    CLOSEFILES;
    CLOSECODEGEN;
  END  (* BODY *) ;

BEGIN  (* BLOCK *)
  FLABP:=FSTLABP; FWPROCS:=NIL;
  LC1STLOCVAR := LC;
  REPEAT
    REPEAT
      SCOPEHEAD := NIL;  ENTERSCOPE := TRUE;
      IF SY = LABELSY THEN LABELDECLARATION;
      IF SY = CONSTSY THEN
        BEGIN INSYMBOL; CONSTDECLARATION END;
      IF SY = TYPESY THEN
        BEGIN INSYMBOL; TYPEDECLARATION END;
      IF SY = VARSY THEN
        BEGIN INSYMBOL; VARDECLARATION END;
      PRTERR := FALSE;  ENTERSCOPE := FALSE;
      WHILE SCOPEHEAD <> NIL DO
        BEGIN
          ID := SCOPEHEAD@.NAME;
          SEARCHID((.VARS,FIELD,KONST,TYPES],TLCP);
          IF (TLCP <> UVARPTR) AND (DISX = LEVEL) THEN
            BEGIN
              ERROR(188);
              ENDOFLINE;
              NEWLINE;  WRITELN('>>>':INDENT,'SCOPE ERROR - IDENTIFIER IS: ',ID);
              WRITELN(SYSTERM,'>>>':INDENTTY,'SCOPE ERROR - IDENTIFIER IS: ',ID);
            END;
          SCOPEHEAD := SCOPEHEAD@.NXTP
        END;
      PRTERR := TRUE;
      IF NOT (SY IN BLOCKBEGSYS) THEN
        BEGIN ERROR(18); SKIP(FSYS) END;
    UNTIL (SY IN STATBEGSYS+(.VALUESY,FUNCTSY,PROCSY]);
    IF SY = VALUESY THEN
      BEGIN
        EXTENSION(321);
        IF EXTRNL THEN ERROR(300);
        INSYMBOL;
        VARINITIALIZATION
      END;
    IF (LEVEL = 1) AND (NOT EXTRNL) THEN
      BEGIN
        IF NOT STARTED THEN PUTSTARTER;
        PUTESD('P@MAIN  ',SD,FALSE); PUTESD('P@MAIN@ ',ER,TRUE);
        MAINCSECT := TRUE;
      END;
    WHILE SY IN (.PROCSY,FUNCTSY] DO
      BEGIN
        PROCLEV:=CHR(ORD('A')+LEVEL-1);
        LSY := SY; INSYMBOL;
        PROCDECLARATION(LSY);
      END;
    IF SY <> BEGINSY THEN
      IF NOT EXTRNL THEN
        BEGIN ERROR(18); SKIP(FSYS) END
  UNTIL EXTRNL OR (SY IN STATBEGSYS);
  UNDEFINED(FWPROCS,'PROC/FUNC');
  IF (NOT EXTRNL) OR (EXTRNL AND (LEVEL <> 1)) THEN
    BEGIN
      IF SY = BEGINSY THEN
        BEGIN
          IF LEVEL > 1 THEN PROCLEV:=CHR(ORD('A')+LEVEL-2);
          LEFTCHECK;
          IC := 0; DP := FALSE;
          LOCATION := 0;
        END
      ELSE ERROR(17);
      PROCADDRESS(.FPROCP@.PFCNT]:=PROGCOUNT;
      BODY(FSYS+(.CASESY]);
      IF SY <> FSY THEN
        BEGIN ERROR(6); SKIP(FSYS) END;
    END;
END  (* BLOCK *) ;

PROCEDURE PROGRAMME(FSYS: SETOFSYS);
  VAR  EXFILP: FILEP;

  PROCEDURE DCLSTDFILE(VAR FCP:CTP; VAR AUTO: BOOLEAN);
    (* DECLARE STANDARD FILE & RETURN POINTER IN FCP *)
    (* SET AUTO TRUE UNLESS FILENAME FOLLOWED BY '/' *)
  BEGIN
    NEW(FCP,VARS);
    WITH FCP@ DO
      BEGIN
        NAME := ID; IDTYPE := TEXTPTR;
        VKIND := DRCT;  NEXT := NIL;
        VLEV := 1;  VADDR := LC;  LC := LC + TEXTSIZE;
        CNTRLVAR := FALSE;
      END;
    ENTERID(FCP);
    AUTO := TRUE;  INSYMBOL;
    IF SY = MULOP THEN
      IF OP = RDIV THEN
        BEGIN  AUTO := FALSE; INSYMBOL;
          EXTENSION(329);
        END
  END;  (* DCLSTDFILE *)

BEGIN
 WITH DISPLAY(.1] DO BEGIN FNAME:=NIL; OCCUR:=BLCK END;
 IF SY = PROGRAMSY THEN
  BEGIN INSYMBOL;
   IF SY = IDENT THEN
    BEGIN INSYMBOL;
     IF NOT (SY IN (.SEMICOLON,LPARENT]) THEN
      BEGIN ERROR(7); SKIP(FSYS+(.SEMICOLON,LPARENT]) END;
     IF SY = LPARENT THEN
      BEGIN
       REPEAT INSYMBOL;
        IF SY = IDENT THEN
         BEGIN
          IF ID = 'INPUT   ' THEN DCLSTDFILE(INPUTPTR,AUTOGETINPUT)
          ELSE IF ID = 'OUTPUT  ' THEN DCLSTDFILE(OUTPUTPTR,AUTOGETOUTPUT);
          EXFILP := FEXFILP;
          WHILE EXFILP <> NIL DO
            WITH EXFILP@ DO
              BEGIN
                IF FILENAME = ID THEN ERROR(101);
                EXFILP := NXTP
              END;
          IF (ID <> 'INPUT   ') AND (ID <> 'OUTPUT  ') THEN
            BEGIN NEW(EXFILP);
              WITH EXFILP@ DO
                BEGIN FILENAME := ID; NXTP := FEXFILP;
                 DECLARED := FALSE;
                 GETONRESET := TRUE;  INSYMBOL;
                 IF SY = MULOP THEN
                   IF OP = RDIV THEN
                     BEGIN GETONRESET := FALSE;  INSYMBOL;
                       EXTENSION(329);
                     END
                END;
              FEXFILP := EXFILP
            END;
         END
        ELSE ERROR(2);
        IF NOT (SY IN (.COMMA,RPARENT]) THEN
         BEGIN ERROR(6); SKIP(FSYS+(.COMMA,RPARENT]) END
       UNTIL SY <> COMMA;
       IF SY = RPARENT THEN INSYMBOL ELSE ERROR(4)
      END;
     IF SY = SEMICOLON THEN INSYMBOL ELSE ERROR(14)
    END
   ELSE BEGIN ERROR(2); SKIP(FSYS) END
  END
 ELSE BEGIN ERROR(3); SKIP(FSYS) END;
 BLOCK(FSYS,PERIOD,UPRCPTR);
END  (* PROGRAMME *) ;

PROCEDURE STDTYPENTRIES;
  VAR SP: STP;
BEGIN
  NEW(INTPTR,SCALAR,STANDARD);
  WITH INTPTR@ DO
    BEGIN FTYPE:=FALSE; SIZE.WBLENGTH:=4; SIZE.BOUNDARY:=4; END;
  NEW(REALPTR,SCALAR,STANDARD);
  WITH REALPTR@ DO
    BEGIN FTYPE:=FALSE; SIZE.WBLENGTH:=8; SIZE.BOUNDARY:=8; END;
  NEW(CHARPTR,SCALAR,STANDARD);
  WITH CHARPTR@ DO
    BEGIN FTYPE:=FALSE; SIZE.WBLENGTH:=4; SIZE.BOUNDARY:=4; END;
  NEW(BOOLPTR,SCALAR,DECLARED);
  WITH BOOLPTR@ DO
    BEGIN FTYPE:=FALSE; SIZE.WBLENGTH:=4; SIZE.BOUNDARY:=4; END;
  NEW(NILPTR,POINTER);
  WITH NILPTR@ DO
   BEGIN ELTYPE := NIL; FTYPE := FALSE;
         SIZE.WBLENGTH:=4; SIZE.BOUNDARY:=4;
   END;
  NEW(PACKDINTPTR,PACKDTYPE);
  WITH PACKDINTPTR@ DO
    BEGIN SIZE.WBLENGTH:=1; SIZE.BOUNDARY:=1;
          FTYPE:=FALSE; BASETYPE:=INTPTR;
    END;
  NEW(PACKDCHARPTR,PACKDTYPE);
  WITH PACKDCHARPTR@ DO
    BEGIN SIZE.WBLENGTH:=1; SIZE.BOUNDARY:=1;
          FTYPE:=FALSE; BASETYPE:=CHARPTR;
    END;
  NEW(TEXTPTR,FILES);
  WITH TEXTPTR@ DO
    BEGIN FILTYPE:=PACKDCHARPTR;
          TEXTFILE := TRUE;  FTYPE := TRUE;
          SIZE.WBLENGTH:=TEXTSIZE;  SIZE.BOUNDARY:=4;
    END;
  NEW(SP,SUBRANGE);
  WITH SP@ DO
    BEGIN
      RANGETYPE:=INTPTR;
      FTYPE:=FALSE;
      MIN:=1; MAX:=ALFALENG;
      SIZE.WBLENGTH:=4;
      SIZE.BOUNDARY:=4;
    END;
  NEW(ALFAPTR,ARRAYS);
  WITH ALFAPTR@ DO
    BEGIN
      AELTYPE:=PACKDCHARPTR;
      INXTYPE:=SP;
      FTYPE:=FALSE; AELLENG:=1;
      SIZE.WBLENGTH:=ALFALENG;
      SIZE.BOUNDARY:=1
    END
END  (* STDTYPENTRIES *) ;

PROCEDURE STDNAMENTRIES;
  VAR CP,CP1: CTP; I: INTEGER;

  PROCEDURE TYPENAME(S: ALFA; P: STP);
    BEGIN NEW(CP,TYPES);
      WITH CP@ DO
        BEGIN NAME:=S; IDTYPE:=P; END;
      ENTERID(CP);
    END;

  PROCEDURE CONSTNAME(S:ALFA; P:STP; V:INTEGER);
    BEGIN NEW(CP,KONST);
      WITH CP@ DO
        BEGIN NAME:=S; IDTYPE:=P; NEXT:=NIL;
              VALUES.CKIND:=INT; VALUES.IVAL:=V;
        END;
      ENTERID(CP);
    END;

  BEGIN
    TYPENAME('INTEGER ',INTPTR);  TYPENAME('REAL    ',REALPTR);
    TYPENAME('CHAR    ',CHARPTR); TYPENAME('BOOLEAN ',BOOLPTR);
    TYPENAME('TEXT    ',TEXTPTR);
    TYPENAME('ALFA    ',ALFAPTR);
    CONSTNAME('MAXINT  ',INTPTR,MXINT);
    CONSTNAME('FALSE   ',BOOLPTR,0); CP1:=CP;
    CONSTNAME('TRUE    ',BOOLPTR,1); CP@.NEXT:=CP1; BOOLPTR@.FCONST:=CP;
    FOR I := 1 TO NRSTDPROC DO
      BEGIN NEW(CP,PROC,STANDARD);                  (* STANDARD PROCEDURES *)
        WITH CP@ DO
          BEGIN NAME := NA(.I]; IDTYPE := NIL;
            NEXT := NIL; KEY := I;
          END;
        ENTERID(CP)
      END;
    FOR I := 1 TO NRSTDFUNC DO                      (* STANDARD FUNCTIONS *)
      BEGIN NEW(CP,FUNC,STANDARD);
        WITH CP@ DO
          BEGIN NAME := NA(.NRSTDPROC+I]; IDTYPE := NIL;
            NEXT := NIL; KEY := I;
          END;
        ENTERID(CP)
      END;
  END  (* STDNAMENTRIES *) ;

PROCEDURE ENTERUNDECL;
  BEGIN
    NEW(UTYPPTR,TYPES);
    WITH UTYPPTR@ DO
      BEGIN NAME:='        '; IDTYPE:=NIL; END;
    NEW(UCSTPTR,KONST);
    WITH UCSTPTR@ DO
      BEGIN NAME := '        '; IDTYPE := NIL; NEXT := NIL;
        VALUES.IVAL:=0; VALUES.CKIND := INT;
      END;
    NEW(UVARPTR,VARS);
    WITH UVARPTR@ DO
      BEGIN NAME := '        '; IDTYPE := NIL; VKIND := DRCT;
        CNTRLVAR := FALSE;
        NEXT := NIL; VLEV := 0; VADDR := 0
      END;
   NEW(UFLDPTR,FIELD);
   WITH UFLDPTR@ DO
     BEGIN NAME := '        '; IDTYPE := NIL; NEXT := NIL;
       FLDADDR := 0
     END;
   NEW(UPRCPTR,PROC,DECLARED,ACTUAL);
   WITH UPRCPTR@ DO
     BEGIN NAME := 'P@MAIN  '; IDTYPE := NIL;
       NEXT:=NIL; PFLEV:=0; PFCNT:=1; PARAMS:=NIL;
       ASSIGNEDTO := FALSE; WITHINSCOPE := FALSE;
     END;
   NEW(UFCTPTR,FUNC,DECLARED,ACTUAL);
   WITH UFCTPTR@ DO
     BEGIN NAME := '        '; IDTYPE := NIL; NEXT := NIL;
       PFLEV := 0; PFCNT:=1; PARAMS:=NIL;
       ASSIGNEDTO := FALSE; WITHINSCOPE := FALSE;
     END;
   NEW(UEVENTPTR,EVENT);
   WITH UEVENTPTR@ DO
     BEGIN NAME:='        '; IDTYPE:=NIL; NEXT:=NIL;
           EVENTJUMP:=NIL; EVENTDEF:=FALSE;
     END;
  END  (* ENTERUNDECL *) ;

PROCEDURE INITSCALARS;
  VAR ZL: INTEGER;

    PROCEDURE INITDATE;
      (* CONVERT DATE FROM 'DD/MM/YY' TO 'DD MON YY' FOR HEADING *)
    VAR  D: ALFA;  I,M: INTEGER;
    BEGIN
      DATE(D);
      M := 10*(ORD(D(.4])-ORD('0')) + ORD(D(.5])-ORD('0');
      M := 3 * (M-1);    (* OFFSET OF MONTH ABBREVIATION IN STRING *)
      DDATE(.1] := D(.1];  DDATE(.2] := D(.2];  DDATE(.3] := ' ';
      FOR I := 4 TO 6 DO DDATE(.I] := MONTHS(.(I-3)+M];
      DDATE(.7] := ' ';  DDATE(.8] := D(.7];  DDATE(.9] := D(.8]
    END;  (* INITDATE *)

  BEGIN
    CH := ' ';
    PROGCOUNT := 0;
    LC:=LCSTART;
    PCNT:=1;
    DOTFLG := FALSE;
    EXTWARN:=FALSE;
    PRTERR:=TRUE;
    POSSFWDREF := FALSE;
    AUTOGETINPUT:=TRUE; AUTOGETOUTPUT:=TRUE;
    DEBUG:=TRUE; LISTON:=TRUE; PMD:=TRUE; PRINTCODE:=FALSE;
    WARNING:=TRUE;
    VARCHECK:=TRUE;
    BOLDFACE:=FALSE; OVERPRINT:=FALSE;
    FOR ZL:=1 TO MAXCHCNT DO OVERLINE(.ZL]:=' ';
    INPUTPTR:=NIL; OUTPUTPTR:=NIL;
    FWPTR:=NIL; FSTLABP:=NIL; FEXFILP:=NIL; LOCFILP:=NIL;
    ERRINX:=0; ERRORS:=FALSE;
    INITNUMBER:=0; OBPOINTER:=0;
    LEFT := '-';  RIGHT := '-';  PROCLEV := ' ';
    SOURCELNO := 0;
    PAGEE := 1; FOR ZL := 1 TO 40 DO TTL(.ZL] := ' ';
    LINEE := LINESPERPAGE;  PRINTED := TRUE;
    TIME(TTIME);  INITDATE;
    ZLEV := -1;
    ERRORTOT := 0;
    DP:=TRUE;
    NRTEXTRN := 0;
    OBPOINTER := 1;
    CURRADDRESS := 0;
    PROCNAMES := FALSE;
    INITFLAG := TRUE;
    STARTED := FALSE;
    EXTRNL := FALSE;  PROCREF := 'NOPROC@@';
    MAINCSECT := FALSE;
    ESDID := 1;
    TXT.PRELUDE(.1]:=CHR(2);
    ESD.PRELUDE(.1]:=CHR(2);
    RLD.PRELUDE(.1]:=CHR(2);
    ENDC.PRELUDE(.1]:=CHR(2);
    ESDCNT := 0;
    RLDPOS := 1;
    EXTPROCS := 0;
  END;

PROCEDURE INITSETS;
  VAR I: 0..MAXMSGSDIV64;
BEGIN
  CONSTBEGSYS := (.ADDOP,INTCONST,REALCONST,CHARCONST,STRINGCONST,
                   IDENT,LBRACK,NILSY];
  SIMPTYPEBEGSYS := (.LPARENT]+CONSTBEGSYS-(.LBRACK,STRINGCONST];
  TYPEBEGSYS := (.ARROW,PACKEDSY,ARRAYSY,RECORDSY,SETSY,FILESY]+
                  SIMPTYPEBEGSYS;
  TYPEDELS := (.ARRAYSY,RECORDSY,SETSY,FILESY];
  BLOCKBEGSYS := (.LABELSY,CONSTSY,TYPESY,VARSY,VALUESY,PROCSY,FUNCTSY,
                   BEGINSY];
  SELECTSYS := (.ARROW,PERIOD,LBRACK];
  FACBEGSYS := (.INTCONST,REALCONST,CHARCONST,STRINGCONST,IDENT,LPARENT,
                 LBRACK,NOTSY,NILSY];
  STATBEGSYS := (.BEGINSY,GOTOSY,IFSY,WHILESY,REPEATSY,LOOPSY,FORSY,
                  FORALLSY,WITHSY,CASESY];

  BMASK(.EQOP]:=8; BMASK(.NEOP]:=7; BMASK(.GTOP]:=2;
  BMASK(.LTOP]:=4; BMASK(.GEOP]:=11; BMASK(.LEOP]:=13;

  DUALOP(.EQOP]:=EQOP; DUALOP(.NEOP]:=NEOP; DUALOP(.GTOP]:=LTOP;
  DUALOP(.GEOP]:=LEOP; DUALOP(.LTOP]:=GTOP; DUALOP(.LEOP]:=GEOP;

  FOR I := 0 TO MAXMSGSDIV64 DO
     ERRMSGS(.I] := (. ];
END  (* INITSETS *) ;

PROCEDURE FINAL;
  VAR  I:    INTEGER;
       TP,K: INTEGER;
       L:    INTEGER;
       CONV: RECORD CASE BOOLEAN OF
               TRUE:  (I1,I2:INTEGER);
               FALSE: (STR:ALFA)
             END;
BEGIN
  OBCLEAR;
  PUTRLD(2,1,0,TRUE);
  PUTENDC(0,0);
  IF NOT EXTRNL THEN PROCREF := 'P@MAIN@ ';
  PUTESD(PROCREF,SD,TRUE);
  TP:=EXTPROCS;
  IF TP <> 0 THEN
    BEGIN
      FOR L := 0 TO TP-2 DO
        PUTESD(EXTARRAY(.L].ENAME,ER,FALSE);
      PUTESD(EXTARRAY(.TP-1].ENAME,ER,TRUE);
    END;
  FOR L := 1 TO NRTEXTRN DO
    PUTESD(RTEXTRN(.L],ER,TRUE);
  IF NOT ERRORS THEN
    IF EXTRNL THEN DATA1((PCNT-1)+Z7FE) ELSE DATA1(PCNT+Z7FE)
  ELSE DATA1(Z7FE);
  IF EXTRNL THEN L:=2 ELSE L:=1;
  FOR I := L TO PCNT DO DATA1(PROCADDRESS(.I]);
  OBCLEAR;
  TP:=EXTPROCS; I:=2;
  IF EXTRNL THEN L:=0 ELSE L:=1;
  IF TP <> 0 THEN
    BEGIN
      FOR K := 0 TO TP-2 DO
        BEGIN
          PUTRLD(I,1,4*(EXTARRAY(.K].ECNT+L),FALSE);
          I := I + 1;
        END;
      PUTRLD(I,1,4*(EXTARRAY(.TP-1].ECNT+L),TRUE);
    END;
  PUTENDC(0,0)
END;  (* FINAL *)

PROCEDURE WRITEMESSAGES;
  (* WRITE ERROR MESSAGES ON OUTPUT AND TERMINAL FILES *)
VAR
    MSGNUM,ERRORNUM,GROUP,I: INTEGER;
    CH: CHAR;
BEGIN
  (* COPY REST OF PROGRAM TO LISTING IN CASE ABANDONING COMPILATION *)
  WHILE NOT EOF(INPUT) DO GETLN;
  ENDOFLINE;

  WRITELN(SYSTERM,'0','*AAEC PASCAL 2.0 COMPILATION CONCLUDED*');
  NEWLINE; WRITELN;
  NEWLINE; WRITELN(' ','*AAEC PASCAL 2.0 COMPILATION CONCLUDED*');
  NEWLINE; WRITELN;
  NEWLINE; WRITE(' *'); IF NOT ERRORS THEN WRITE('NO ');
           WRITELN('ERRORS DETECTED IN PASCAL PROGRAM*');

  IF ERRORTOT <> 0 THEN
    BEGIN
      IF ERRORS THEN SETCC(8) ELSE SETCC(4);
      WRITE(SYSTERM, '0* ', ERRORTOT:1, ' LINE');
      NEWLINE; WRITELN;
      NEWLINE; WRITE(' * ', ERRORTOT:1, ' LINE');
      IF ERRORTOT > 1 THEN BEGIN WRITE('S'); WRITE(SYSTERM,'S') END;
      WRITELN(SYSTERM,' FLAGGED IN PASCAL PROGRAM *');
      WRITELN(SYSTERM);
      WRITELN(' FLAGGED IN PASCAL PROGRAM *');
      NEWLINE;  WRITELN;
      NEWLINE;  WRITELN(' ERROR MESSAGES :');
      NEWLINE;  WRITELN(' ****************');
      NEWLINE;  WRITELN;

      (* COPY RELEVANT MESSAGES TO LISTING AND TERMINAL *)
      RESET($PASMSGS);
      IF NOT EOF($PASMSGS) THEN READ($PASMSGS,MSGNUM)
        ELSE MSGNUM := -1;
      FOR GROUP := 0 TO MAXMSGSDIV64 DO
        FOR I := 0 TO SETMAX DO
          IF I IN ERRMSGS(.GROUP] THEN
            BEGIN
              ERRORNUM := GROUP * 64 + I;

              (* ADVANCE UNTIL MSGNUM IS AT LEAST ERRORNUM         *)
              (* MSGNUM = NEXT UNCOPIED MESSAGE NUMBER IN $PASMSGS *)
              WHILE (MSGNUM < ERRORNUM) AND NOT EOF($PASMSGS) DO
                BEGIN READLN($PASMSGS);
                  IF NOT EOF($PASMSGS) THEN READ($PASMSGS,MSGNUM);
                END;

              (* (MSGNUM >= ERRORNUM) OR EOF($PASMSGS) *)
              NEWLINE; WRITE(' ',ERRORNUM:4);
              WRITE(SYSTERM, ' ',ERRORNUM:4);
              IF MSGNUM = ERRORNUM THEN
                BEGIN  (* COPY MESSAGE TO OUTPUT FILES *)
                  WHILE NOT EOLN($PASMSGS) DO
                    BEGIN READ($PASMSGS,CH);
                      WRITE(CH);  WRITE(SYSTERM,CH);
                    END;
                  WRITELN; WRITELN(SYSTERM);
                END  (* COPYING MESSAGE *)
              ELSE
                BEGIN
                  WRITELN(': MESSAGE NOT FOUND');
                  WRITELN(SYSTERM,': MESSAGE NOT FOUND');
                END;
            END;  (* ERRORNUM IN GROUP *)
    END;  (* ERRORTOT <> 0 *)
END;  (* WRITEMESSAGES *)

BEGIN
  INITSCALARS; INITSETS; SSYMBOLS;
  init_procaddress;

  LEVEL := 0;  TOP := 0;
  WITH DISPLAY(.0] DO
    BEGIN FNAME := NIL;  OCCUR := BLCK END;
  STDTYPENTRIES; STDNAMENTRIES; ENTERUNDECL;
  TOP := 1;  LEVEL := 1;

  REWRITE(SYSGO);
  REWRITE(SYSTERM);
  READPARMS;

  GETLN;
  INSYMBOL;
  PROGRAMME(BLOCKBEGSYS+STATBEGSYS-(.CASESY]);

9999:
  ENDOFLINE;
  FINAL;
  WRITEMESSAGES;

(*$L**)
END .
