program PASCALCOMPILER ( INPUT , OUTPUT , PRR , QRR ) ;

(********************************************************************)
(*$D-,N+                                                            *)
(*                                                                  *)
(*         S T A N F O R D   P A S C A L   C O M P I L E R          *)
(*                                                                  *)
(*                  MCGILL UNIVERSITY VERSION                       *)
(*                                                                  *)
(*                                                                  *)
(*         AUTHOR OF ORIGINAL PROGRAM:                              *)
(*                                                                  *)
(*              URS AMMANN                                          *)
(*              FACHGRUPPE COMPUTERWISSENSCHAFTEN                   *)
(*              EIDG. TECHNISCHE HOCHSCHULE                         *)
(*              CH-8006 ZUERICH                                     *)
(*                                                                  *)
(*                                                                  *)
(*         AUTHOR OF SECOND VERSION:                                *)
(*                                                                  *)
(*              KESAV NORI                                          *)
(*              COMPUTER GROUP                                      *)
(*              T.I.F.R.                                            *)
(*              HOMI BHABHA ROAD                                    *)
(*              BOMBAY - 400005                                     *)
(*              INDIA                                               *)
(*                                                                  *)
(*                                                                  *)
(*         AUTHOR OF THIRD VERSION (STANFORD PASCAL):               *)
(*                                                                  *)
(*              S. HAZEGHI                                          *)
(*              COMPUTATION RESEARCH GROUP                          *)
(*              STANFORD LINEAR ACCELERATOR CENTER                  *)
(*              STANFORD, CA. 94305.                                *)
(*                                                                  *)
(*                                                                  *)
(*         AUTHOR OF FOURTH VERSION (MCGILL PASCAL):                *)
(*                                                                  *)
(*              R. NIGEL HORSPOOL                                   *)
(*              SCHOOL OF COMPUTER SCIENCE                          *)
(*              MCGILL UNIVERSITY                                   *)
(*              MONTREAL  QUEBEC  H3A 2K6                           *)
(*                                                                  *)
(*==================================================================*)
(*                                                                  *)
(* 02FEB2007 - Changes by Dave Edwards to use hex codes C0,D0       *)
(*    (was 8B,9B) for EBCDIC brace characters (curly brackets).     *)
(*    New constants CHLBRACE and CHRBRACE defined.                  *)
(*    VERSION date left unchanged as MAY -82.                       *)
(*    Hex C0,D0 seem to be the codes used by most curr. software    *)
(*    such as tn3270 clients, Ascii-EBCDIC translate tables, etc.   *)
(*    and conform to the standard IBM-037 US code page.             *)
(*    See additional notes in file ccde:pascal_info.txt .           *)
(*  - Also, in INITTABLES procedure, set UPSHIFT to only upshift    *)
(*    lowercase chars (e.g. exclude tilde, which is in the range    *)
(*    a to z), and add comments re. definition of SSY array.        *)
(*    Note that curly brackets and backslash are within the         *)
(*    range A to Z in the EBCDIC character set.                     *)
(*  - Also fix spacing of text for BGN output record: change        *)
(*    TIME:9 to TIME:8. Was causing last char of year to be         *)
(*    truncated, in the info text at the start of $MAINBLK          *)
(*    csect in the object file.                                     *)
(*  - Source changes are identified by flag DE near beginning       *)
(*    of lines.                                                     *)
(*    (Write date of previous source file: 14sep1983.)              *)
(*                                                                  *)
(*==================================================================*)
(*                                                                  *)
(*  Oct.2011 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*    PASCAL 1982 ported to VM370 R6 on Hercules (from MUSIC/SP)    *)
(*                                                                  *)
(*    Summary of Changes:                                           *)
(*                                                                  *)
(*    - another substitute for brackets: (. .) - not only (/ /)     *)
(*                                                                  *)
(*    - substitute for pointer symbol: ->                           *)
(*                                                                  *)
(*    - comments like PL/1 and C: /* ... */                         *)
(*                                                                  *)
(*    - year 2000 problem fixed (still TIME DEC used in PASMONN)    *)
(*                                                                  *)
(*    - new loop control statements: BREAK and CONTINUE             *)
(*                                                                  *)
(*    - RETURN statement                                            *)
(*                                                                  *)
(*  to be done:                                                     *)
(*                                                                  *)
(*    - allow shorter String constants to be assigned               *)
(*      (padded with blanks)                                        *)
(*                                                                  *)
(*    - allow static definitions (local to procs and functions)     *)
(*                                                                  *)
(********************************************************************)



const VERSION = '10.2011' ;
      PAGESIZE = 55 ;

      (******************************************)
      (*MAX # OF LINES PER PAGE OF LISTING      *)
      (******************************************)

      MAXINT = 2147483647 ;
      MAXADDR = 16777215 ;
      SETMAX = 255 ;

      (******************************************)
      (*LARGEST POSSIBLE SET ELEMENT            *)
      (******************************************)

      SSETMAX = 63 ;

      (******************************************)
      (*LARGEST ELEMENT USED IN THIS CODE       *)
      (******************************************)

      BUFLEN = 122 ;

      (******************************************)
      (*MAX LINE LENGTH + 2                     *)
      (******************************************)

      INTSIZE = 4 ;
      HINTSIZE = 2 ;
      REALSIZE = 8 ;
      CHARSIZE = 1 ;
      MXDATASZE = 8 ;
      BOOLSIZE = 1 ;
      WORDSIZE = 4 ;

      (*****************************************)
      (* NUMBER OF BYTES PER WORD              *)
      (*****************************************)

      SETPACK = 32 ;

      (*****************************************)
      (* NUMBER OF SET ELEMENTS PER WORD       *)
      (*****************************************)

      MAXSETSIZE = 32 ;

      (*****************************************)
      (*  = (SETMAX+1) DIV SETPACK * WORDSIZE  *)
      (*****************************************)

      PTRSIZE = 4 ;
      FILHDRSIZE = 8 ;
      REALLNGTH = 20 ;

      (******************************************)
      (*STRING REPRESENTATION OF REAL NUMBERS   *)
      (******************************************)

      DIGMAX = 19 ;

      (******************************************)
      (*REALLNGTH-1                             *)
      (******************************************)

      IDLNGTH = 12 ;
      ALFALNGTH = 10 ;
      STRGLNGTH = 64 ;
      DISPLIMIT = 20 ;
      MAX_BKT = 58 ;

      (******************************************)
      (* HASH TABLE SIZE                        *)
      (******************************************)

      MAXLEVEL = 9 ;
      ORDCHMAX = 255 ;

      (******************************************)
      (*SIZE OF CHAR SET OF TARGET MACHINE      *)
      (******************************************)

      OPMAX = 76 ;

      (*****************)
      (* OPCODE RANGE  *)
      (*****************)

      MAXERRNR = 401 ;

      (******************************************)
      (*MAX VAL OF ERROR CODE                   *)
      (******************************************)

      MAXERRLOG = 8 ;

      (******************************************)
      (* > (MAXERRNR DIV SETMAX)                *)
      (******************************************)

      NRSW = 41 ;
      NRSW1 = 42 ;

      (************)
      (* NRSW + 1 *)
      (************)

      NSPROC = 36 ;

      (******************************************)
      (* # OF STANDARD PROCS                    *)
      (******************************************)

      NPDW = 57 ;

      (******************************************)
      (* # OF PREDEFINED WORDS                  *)
      (******************************************)

      CTRMAX = 16384 ;
      EXTNAMSZ = 8 ;

      (*****************************************)
      (* EXTERNAL NAME LENGTH                  *)
      (*SAVE AREAS, FUNCTION RETURN VALUE SPAC *)
      (*E, DISPLAY AREA, ETC.                  *)
      (*****************************************)

      LCAFTMST = 80 ;
      FPSAVEAREA = 32 ;
      RUNCHKAREA = 96 ;
      DISPADR = 80 ;
      FNCRSLT = 72 ;
      DISPAREA = 40 ;
      FIRSTCONSTLC = 8 ;
      FIRSTFILBUF = 248 ;

      (******************************************)
      (* = LCAFTMST+RUNCHKAREA+DSPLYAREA        *)
      (******************************************)

      TIMEDATELOC = 328 ;

      (******************************************)
      (* LOCATION OF TIME/DATE PREDEF. VARS     *)
      (******************************************)

      OSPARMLOC = 348 ;

      (******************************************)
      (* LOCATION FOR 'OSPARM' PTR.             *)
      (******************************************)

      FIRSTGVAR = 352 ;

      (******************************************)
      (* FIRST USER DEFINED GLOBAL VARIABLE     *)
      (******************************************)

      CHLBRACE = '{' ;

      (******************************************)
      (* LEFT CURLY BRACKET: EBCDIC HEX CODE C0 *)
      (******************************************)

      CHRBRACE = '}' ;

      (*******************************************)
      (* RIGHT CURLY BRACKET: EBCDIC HEX CODE D0 *)
      (*---------------------------------------- *)
      (*------------------------------------     *)
      (*******************************************)



type

     (******************)
     (*BASIC SYMBOLS   *)
     (**************   *)
     (******************)


     SYMBOL = ( IDENT , INTCONST , REALCONST , STRINGCONST , NOTSY ,
              MULOP , ADDOP , RELOP , LPARENT , RPARENT , LBRACK ,
              RBRACK , COMMA , SEMICOLON , PERIOD , ARROW , COLON ,
              DOTDOT , BECOMES , LABELSY , CONSTSY , TYPESY , VARSY ,
              FUNCSY , PROGSY , PROCSY , SETSY , PACKEDSY , ARRAYSY ,
              RECORDSY , FILESY , FORWARDSY , BEGINSY , IFSY , CASESY ,
              REPEATSY , WHILESY , FORSY , WITHSY , GOTOSY , ENDSY ,
              ELSESY , UNTILSY , OFSY , DOSY , TOSY , DOWNTOSY , THENSY
              , FRTRNSY , EXTRNSY , OTHERWISESY , OTHERSY , BREAKSY ,
              CONTINUESY , RETURNSY ) ;
     OPERATOR = ( MUL , RDIV , ANDOP , IDIV , IMOD , PLUS , MINUS ,
                OROP , LTOP , LEOP , GEOP , GTOP , NEOP , EQOP , INOP ,
                NOOP , ATOZCH , NUMCH , QUOTCH , DQUOTCH , COLONCH ,
                DOTCH , LPARCH , RPARCH , LBRACE , UNDSCH , DOLLARCH ,
                SKIPCH , SPECH , ILLEGCH ) ;
     SETOFSYS = set of SYMBOL ;
     SSETINX = 1 .. 4 ;

     (***********************************)
     (* 4 = (SETMAX+1) DIV (SSETMAX+1)  *)
     (***********************************)

     SSETRANGE = set of 0 .. SSETMAX ;
     SETRANGE = array [ SSETINX ] of SSETRANGE ;

     (******************)
     (*CONSTANTS       *)
     (**********       *)
     (******************)

     CSTCLASS = ( REEL , PSET , STRG ) ;
     CSP = -> CONSTANT ;
     CONSTANT = record
                  case CSTCLASS of
                    REEL :
                      ( RVAL : packed array [ 1 .. REALLNGTH ] of CHAR
                        ) ;
                    PSET :
                      ( PLNGTH : 0 .. MAXSETSIZE ;
                        PVAL : SETRANGE ) ;
                    STRG :
                      ( SLNGTH : 0 .. STRGLNGTH ;
                        SVAL : packed array [ 1 .. STRGLNGTH ] of CHAR
                        )
                end ;
     VALU = record
              case BOOLEAN of
                TRUE :
                  ( IVAL : INTEGER ) ;
                FALSE :
                  ( VALP : CSP )
            end ;

     (*******************)
     (*DATA STRUCTURES  *)
     (****************  *)
     (*******************)

     LEVRANGE = 0 .. MAXLEVEL ;
     ADDRRANGE = 0 .. MAXADDR ;
     ALNRNG = 1 .. 8 ;
     LABELRNG = 0 .. 1000 ;
     BKT_RNG = 0 .. MAX_BKT ;
     STRUCTFORM = ( SCALAR , SUBRANGE , POINTER , POWER , ARRAYS ,
                  RECORDS , FILES , TAGFLD , VARIANT ) ;
     DECLKIND = ( STANDARD , DECLARED ) ;
     STP = -> STRUCTURE ;
     CTP = -> IDENTIFIER ;
     STRUCTURE = record
                   SIZE : ADDRRANGE ;
                   ALN : ALNRNG ;

     (******************)
     (*ALIGNMENT FACTOR*)
     (******************)

                   case FORM : STRUCTFORM of
                     SCALAR :
                       ( case SCALKIND : DECLKIND of
                           DECLARED :
                             ( FCONST : CTP ) ) ;
                     SUBRANGE :
                       ( RANGETYPE : STP ;
                         MIN , MAX : VALU ) ;
                     POINTER :
                       ( ELTYPE : STP ) ;
                     POWER :
                       ( ELSET : STP ) ;
                     ARRAYS :
                       ( AELTYPE , INXTYPE : STP ) ;
                     RECORDS :
                       ( FSTFLD : CTP ;
                         RECVAR : STP ;
                         NO_FLDS : 0 .. 1000 ;
                         FLD_DISP_LEV : - 1 .. DISPLIMIT ) ;
                     FILES :
                       ( FILTYPE : STP ) ;
                     TAGFLD :
                       ( TAGFIELDP : CTP ;
                         FSTVAR : STP ) ;
                     VARIANT :
                       ( NXTVAR , SUBVAR : STP ;
                         FSTSUBFLD : CTP ;
                         VARVAL : VALU )
                 end ;

     (******************)
     (*NAMES           *)
     (******           *)
     (******************)

     IDCLASS = ( TYPES , KONST , STRUCTKONST , VARS , FIELD , PROC ,
               FUNC ) ;
     SETOFIDS = set of IDCLASS ;
     IDKIND = ( ACTUAL , FORMAL ) ;
     ALPHA = packed array [ 1 .. IDLNGTH ] of CHAR ;
     IDENTIFIER = record
                    NAME : ALPHA ;
                    IDTYPE : STP ;
                    NEXT_IN_BKT , NEXT : CTP ;
                    DECL_LEV : LEVRANGE ;
                    case KLASS : IDCLASS of
                      KONST :
                        ( VALUES : VALU ) ;
                      STRUCTKONST :
                        ( SKOWNER : CTP ;
                          SKADDR : ADDRRANGE ) ;
                      VARS :
                        ( VKIND : IDKIND ;
                          VLEV : LEVRANGE ;
                          VADDR : ADDRRANGE ) ;
                      FIELD :
                        ( FLDADDR : ADDRRANGE ;
                          OWNER : STP ) ;
                      PROC , FUNC :
                        ( case PFDECKIND : DECLKIND of
                            STANDARD :
                              ( KEY : 0 .. NSPROC ) ;
                            DECLARED :
                              ( PFLEV : INTEGER ;
                                PFNAME : LABELRNG ;
                                PRMPTR , NXTFWRD : CTP ;
                                PFKIND : IDKIND ;
                                FWDECL , EXTRN , FRTRN : BOOLEAN ;
                                EXTNAME : array [ 1 .. EXTNAMSZ ] of
                                          CHAR ) )
                  end ;
     DISPRANGE = 0 .. DISPLIMIT ;
     HASH_TABLE = array [ BKT_RNG ] of CTP ;
     WHERE = ( BLCK , CREC , VREC , REC ) ;

     (******************)
     (*EXPRESSIONS     *)
     (************     *)
     (******************)

     ATTRKIND = ( CST , VARBL , EXPR ) ;
     VACCESS = ( DRCT , INDRCT , INXD , STKEXPR ) ;
     ATTR = record
              TYPTR ,

     (*********************************)
     (* TYPE AS AN EXPR. ON RUN-STACK *)
     (*********************************)


              BTYPE : STP ;

     (*********************************)
     (* TYPE AS A VARIABLE IN MEMORY  *)
     (*********************************)

              case KIND : ATTRKIND of
                CST :
                  ( CVAL : VALU ) ;
                VARBL :
                  ( case ACCESS : VACCESS of
                      DRCT :
                        ( VLEVEL : LEVRANGE ;
                          DPLMT : ADDRRANGE ) ;
                      INDRCT :
                        ( IDPLMT : ADDRRANGE ) ;
                      STKEXPR :
                        ( STKDPLMT , STKLEN : ADDRRANGE ) )
            end ;
     TESTP = -> TESTPOINTER ;
     TESTPOINTER = packed record
                            ELT1 , ELT2 : STP ;
                            LASTTESTP : TESTP
                          end ;

     (*************)
     (*LABELS     *)
     (*******     *)
     (*************)

     LBP = -> LABL ;
     LABL = record
              NEXTLAB : LBP ;
              LABVAL : INTEGER ;
              LABNAME , XNO : LABELRNG ;
              DEFINED : BOOLEAN
            end ;
     FRECPTR = -> FILEREC ;
     FILEREC = record
                 FILIDPTR : CTP ;
                 NEXTFILE : FRECPTR ;
               end ;
     PRNTTYLISTP = -> PRNTTYLIST ;
     PRNTTYLIST = record
                    ELT : STP ;
                    TNO : 0 .. 999 ;
                    NXT : PRNTTYLISTP
                  end ;
     ERRCODE = 0 .. MAXERRNR ;
     CTRRANGE = 0 .. CTRMAX ;
     CTRTYPE = ( CTRPROC , CTRLBL , CTRGOTO , CTRIF , CTRWHILE ,
               CTRREPEAT , CTRFOR , CTRCASE ) ;
     LOOPCTL = record
                 LEVEL : INTEGER ;
                 BREAKLABEL : LABELRNG ;
                 BREAKUSED : BOOLEAN ;
                 CONTLABEL : LABELRNG ;
                 CONTUSED : BOOLEAN ;
               end ;
     SUBRCTL = record
                 RETURNLABEL : LABELRNG ;
                 RETURNUSED : BOOLEAN ;
               end ;

     (*************************************************************** *)
     (***                                                             *)
     (*-------------------------------------------------------------- *)
     (*--                                                             *)
     (*************************************************************** *)
     (***                                                             *)



var

    (**************************************************************** *)
    (******                                                           *)
    (*RETURNED BY SOURCE PROGRAM SCANNER                              *)
    (*                                                                *)
    (*                                                                *)
    (*                                                                *)
    (*                                      INSYMBOL:                 *)
    (*                                                                *)
    (*                                                                *)
    (*                                                                *)
    (*                                      *********                 *)
    (*                                                                *)
    (**************************************************************** *)
    (******                                                           *)


    SY : SYMBOL ;

    (******************************************)
    (*LAST SYMBOL                             *)
    (******************************************)

    OP : OPERATOR ;

    (******************************************)
    (*CLASSIFICATION OF LAST SYMBOL           *)
    (******************************************)

    VAL : VALU ;

    (******************************************)
    (*VALUE OF LAST CONSTANT                  *)
    (******************************************)

    LNGTH : INTEGER ;

    (******************************************)
    (*LENGTH OF LAST STRING CONSTANT          *)
    (******************************************)

    PROGNAME , ID : ALPHA ;

    (******************************************)
    (*LAST IDENTIFIER (POSSIBLY TRUNCATED)    *)
    (******************************************)

    CH : CHAR ;

    (******************************************)
    (*LAST CHARACTER READ                     *)
    (******************************************)

    EOL : BOOLEAN ;

    (******************************************)
    (*END OF LINE FLAG                        *)
    (*COUNTERS:                               *)
    (**********                               *)
    (******************************************)

    CHCNT : 0 .. BUFLEN ;

    (******************************************)
    (*CHARACTER COUNTER                       *)
    (******************************************)

    CONSTLC ,

    (******************************************)
    (*DATA LOC. FOR STRUCTURED CONSTANTS      *)
    (******************************************)


    LC , IC , OLDIC , STIC : ADDRRANGE ;

    (******************************************)
    (*DATA LOCATION AND INSTRUCTION COUNTER   *)
    (******************************************)

    LINECNT , OLDLN , PLCNT , ERRLN , PAGECNT , LASTLINELISTED :
                                                   INTEGER ;

    (******************************************)
    (*SWITCHES:                               *)
    (**********                               *)
    (******************************************)

    HP ,

    (******************************************)
    (*HEADER PART                             *)
    (******************************************)


    PRTERR ,

    (******************************************)
    (*TO ALLOW FORWARD REFERENCES IN PTR TYPE *)
    (*DECLARATION BY SUPPRESSING ERROR MSG    *)
    (******************************************)


    DOTFLG ,

    (******************************************)
    (*ONE DOT ALREADY SEEN                    *)
    (******************************************)


    ASSIGN , PACKDATA ,

    (******************************************)
    (*ASSIGNMENT GOING ON,PACKING IN EFFECT   *)
    (******************************************)


    LIST , PRCODE ,

    (******************************************)
    (*LIST SOURCE, OUTPUT P-CODE              *)
    (******************************************)


    DEBUG , MWARN ,

    (******************************************)
    (*DEBUG CODE WANTED, MARGINS WARNING      *)
    (******************************************)


    FLIPDEBUG , NOPACKING ,

    (******************************************)
    (*DEBUG FLIPPED, PACKING SUPPRESSED       *)
    (******************************************)


    NESTCOMM , MUSIC ,

    (******************************************)
    (*NESTED COMMENTS ALLOWED, MUSIC SYSTEM   *)
    (******************************************)


    WARNING , EXTUSED ,

    (******************************************)
    (*WARNINGS WANTED, EXTENSIONS USED        *)
    (******************************************)


    ASSEMBLE , ASMVERB ,

    (******************************************)
    (*POSTPROCESSOR TRANSLATION, VERBOSE      *)
    (******************************************)


    XLINK , GET_STAT ,

    (******************************************)
    (*EXTERNAL LINKAGE, NAME CHANGE FLAG      *)
    (******************************************)


    SAVEREGS , SAVEFPRS : BOOLEAN ;
    LISTTAG : CHAR ;

    (******************************************)
    (* LISTING TAG, 'D'/'C'/'N'/' '           *)
    (******************************************)

    DEBUG_LEV : 0 .. 9 ;

    (******************************************)
    (*POINTERS:                               *)
    (**********                               *)
    (******************************************)

    INPUTPTR , OUTPUTPTR : CTP ;

    (************************************)
    (* PREDEFINED FILES INPUT + OUTPUT  *)
    (************************************)

    INTPTR , REALPTR , CHARPTR , BOOLPTR , NILPTR , TEXTPTR , ALFAPTR :
                                                   STP ;

    (******************************************)
    (*POINTERS TO ENTRIES OF STANDARD IDS     *)
    (******************************************)

    UTYPPTR , UCSTPTR , UVARPTR , UFLDPTR , UPRCPTR , UFCTPTR ,

    (************************)
    (*POINTERS TO ENTRIES F *)
    (*    OR UNDECLARED IDS *)
    (*                      *)
    (************************)


    MAINPROG ,

    (******************************************)
    (*POINTER TO $MAINBLK ENTRY               *)
    (******************************************)


    FRTPARHD ,

    (******************************************)
    (*POINTER TO LIST OF FORTRAN PROC PARMS   *)
    (******************************************)


    FWPTR : CTP ;

    (******************************************)
    (*HEAD OF CHAIN OF FORW DECL TYPE IDS     *)
    (******************************************)

    FILEHEAD : FRECPTR ;

    (******************************************)
    (*HEAD OF CHAIN OF EXTERNAL FILES         *)
    (******************************************)

    OPEN_RECORD : STP ;

    (******************************************)
    (*CURRENT RECORD OPENED BY "WITH"         *)
    (******************************************)

    GLOBTESTP : TESTP ;

    (******************************************)
    (*LAST TESTPOINTER                        *)
    (******************************************)

    PRNTTYPHD : PRNTTYLISTP ;

    (******************************************)
    (*LIST OF HEAP STORAGE ITEMS FOR DEBUG    *)
    (******************************************)

    PRNTTYNO : 0 .. 999 ;

    (******************************************)
    (*BOOKKEEPING OF DECLARATION LEVELS:      *)
    (***********************************      *)
    (******************************************)

    LEVEL : LEVRANGE ;

    (******************************************)
    (*CURRENT STATIC LEVEL                    *)
    (******************************************)

    STMTNEST : 0 .. 100 ;

    (******************************************)
    (*CURRENT STATEMENT NESTING               *)
    (******************************************)

    DISX ,

    (******************************************)
    (*LEVEL OF LAST ID SEARCHED BY SEARCHID   *)
    (******************************************)


    TOP : - 1 .. DISPLIMIT ;

    (******************************************)
    (*TOP OF DISPLAY                          *)
    (******************************************)

    DISPLAY :

    (******************************************)
    (*WHERE:   MEANS:                         *)
    (******************************************)


    array [ DISPRANGE ] of packed record

    (*******************************************)
    (*=BLCK:   ID IS VARIABLE ID               *)
    (*                                         *)
    (*******************************************)

                                    case OCCUR : WHERE of

    (******************************************)
    (*=CREC:   ID IS FIELD ID IN RECORD WITH  *)
    (******************************************)

                                      BLCK :
                                        ( FLABEL : LBP ) ;

    (******************************************)
    (*         CONSTANT ADDRESS               *)
    (******************************************)

                                      CREC :
                                        ( CLEV : LEVRANGE ;

    (******************************************)
    (*=VREC:   ID IS FIELD ID IN RECORD WITH  *)
    (******************************************)

                                          CDSPL : ADDRRANGE ) ;

    (******************************************)
    (*         VARIABLE ADDRESS               *)
    (******************************************)

                                      VREC :
                                        ( VDSPL : ADDRRANGE )
                                  end ;

    (******************************************)
    (* --> PROCEDURE WITHSTATEMENT            *)
    (*RUN-TIME PROFILER COUNTERS              *)
    (***************************              *)
    (******************************************)

    CTRCNT : CTRRANGE ;
    CTRCNTLBL : LABELRNG ;
    CTROPTION : BOOLEAN ;

    (******************************************)
    (*EXPRESSION COMPILATION:                 *)
    (************************                 *)
    (******************************************)

    GATTR : ATTR ;

    (******************************************)
    (*DESCRIBES THE EXPR CURRENTLY COMPILED   *)
    (******************************************)

    MXINT10 : INTEGER ;

    (******************************************)
    (*BUFFERS, READ ONLY TABLES ETC.          *)
    (******************************           *)
    (******************************************)

    LSTOP : CHAR ;

    (********************************)
    (*MARKS THE BEGINNING OF LINEBUF*)
    (********************************)

    LINEBUF : array [ 1 .. BUFLEN ] of CHAR ;

    (********************************)
    (*CURRENT LINE BUFFER           *)
    (********************************)

    LMARGIN , RMARGIN , LINELEN , BUFEND , LASTCOL : 0 .. BUFLEN ;

    (********************************)
    (*LEFT, RIGHT MARGINS ant PTRS  *)
    (********************************)

    INTLABEL , PROCLAB , XLABNO : LABELRNG ;
    CALL_LVL : array [ BOOLEAN ] of INTEGER ;
    SOP : packed array [ CHAR ] of OPERATOR ;
    UPSHIFT : array [ ' ' .. '9'

    (****************)
    (*SHOULD BE CHAR*)
    (****************)


              ] of CHAR ;

    (********)
    (*OP    *)
    (********)

    SSY : packed array [ ' ' .. '9' ] of SYMBOL ;
    BUCKET : HASH_TABLE ;

    (******************************************)
    (* SYMBOL TABLE USAGE STATISTICS          *)
    (* ****** ***** ***** **********          *)
    (******************************************)

    FENT_CNT , SF_CNT , SF_TOT ,

    (******************************************)
    (* # FIELD ENTRIES, SEARCHES, PRODUCT     *)
    (******************************************)


    WE_CNT , RE_CNT ,

    (******************************************)
    (* # "WITH" LOOKUPS, # RECORDS            *)
    (******************************************)


    WS_CNT : INTEGER ;

    (******************************************)
    (* # WITH STATEMENTS                      *)
    (******************************************)

    PROC_CNT , ENT_CNT : array [ LEVRANGE ] of INTEGER ;
    LU_CNT : array [ LEVRANGE , DISPRANGE ] of INTEGER ;
    WLU_CNT : array [ 1 .. 10 , 1 .. 10 ] of INTEGER ;

    (******************************************)
    (*ERROR MESSAGES:                         *)
    (****************                         *)
    (******************************************)

    ERRLOG : array [ 0 .. MAXERRLOG

    (**********************)
    (* = 400 DIV SSETMAX+1*)
    (**********************)


             ] of SSETRANGE ;
    ERRORCNT , WARNCNT ,

    (******************************************)
    (*ERRORS AND WARNINGS COUNTS              *)
    (******************************************)


    CTIME : INTEGER ;

    (******************************************)
    (*COMPILATION TIME                        *)
    (******************************************)

    ERRINX : 0 .. 10 ;

    (******************************************)
    (*NR OF ERRORS IN CURRENT SOURCE LINE     *)
    (******************************************)

    ERRKIND : CHAR ;

    (******************************************)
    (*KIND OF ERROR, 'E' / 'W' (WARNING)      *)
    (******************************************)

    ERRLIST : array [ 1 .. 10 ] of packed record
                                            NMR : 1 .. 401 ;
                                            KIND : CHAR ;
                                            POS : 1 .. 81
                                          end ;

    (*******************************************)
    (* STRUCTURE CONSTANTS, READ-ONLY TABLES   *)
    (* ********* *********  **** **** ******   *)
    (*******************************************)



const BLANKID : ALPHA = '            ' ;
      CONSTBEGSYS : SETOFSYS =
      [ ADDOP , INTCONST , REALCONST , STRINGCONST , IDENT ] ;
      SIMPTYPEBEGSYS : SETOFSYS =
      [ ADDOP , INTCONST , REALCONST , STRINGCONST , IDENT , LPARENT ]
        ;
      TYPEBEGSYS : SETOFSYS =
      [ ARROW , PACKEDSY , ARRAYSY , RECORDSY , SETSY , FILESY , ADDOP
        , INTCONST , REALCONST , STRINGCONST , IDENT , LPARENT ] ;
      TYPEDELS : SETOFSYS =
      [ ARRAYSY , RECORDSY , SETSY , FILESY , PACKEDSY ] ;
      BLOCKBEGSYS : SETOFSYS =
      [ LABELSY , CONSTSY , TYPESY , VARSY , PROCSY , FUNCSY , BEGINSY
        ] ;
      SELECTSYS : SETOFSYS =
      [ ARROW , PERIOD , LBRACK , LPARENT ] ;
      FACBEGSYS : SETOFSYS =
      [ INTCONST , REALCONST , STRINGCONST , IDENT , LPARENT , LBRACK ,
        NOTSY ] ;
      STATBEGSYS : SETOFSYS =
      [ BEGINSY , GOTOSY , IFSY , WHILESY , REPEATSY , FORSY , WITHSY ,
        CASESY , BREAKSY , CONTINUESY , RETURNSY ] ;

      (*********************************************************)
      (*   new reserved symbols in the 2011 version:           *)
      (*   break, return, continue                             *)
      (*********************************************************)

      RW : array [ 1 .. NRSW ] of ALPHA =
      ( 'IF          ' , 'DO          ' , 'OF          ' ,
        'TO          ' , 'IN          ' , 'OR          ' ,
        'END         ' , 'FOR         ' , 'VAR         ' ,
        'DIV         ' , 'MOD         ' , 'SET         ' ,
        'AND         ' , 'NOT         ' , 'THEN        ' ,
        'ELSE        ' , 'WITH        ' , 'GOTO        ' ,
        'CASE        ' , 'TYPE        ' , 'FILE        ' ,
        'BEGIN       ' , 'UNTIL       ' , 'WHILE       ' ,
        'ARRAY       ' , 'CONST       ' , 'LABEL       ' ,
        'BREAK       ' , 'REPEAT      ' , 'RECORD      ' ,
        'DOWNTO      ' , 'PACKED      ' , 'RETURN      ' ,
        'FORWARD     ' , 'PROGRAM     ' , 'FORTRAN     ' ,
        'EXTERNAL    ' , 'FUNCTION    ' , 'CONTINUE    ' ,
        'PROCEDURE   ' , 'OTHERWISE   ' ) ;
      FRW : array [ 1 .. 14 ] of 1 .. NRSW1 =

      (**********************************************************)
      (*  1  2  3   4   5   6   7   8   9  10  11  12  13  14   *)
      (**********************************************************)

      ( 1 , 1 , 7 , 15 , 22 , 29 , 34 , 37 , 40 , 42 , 42 , 42 , 42 ,
        42 ) ;
      RSY : array [ 1 .. NRSW ] of SYMBOL =
      ( IFSY , DOSY , OFSY , TOSY , RELOP , ADDOP , ENDSY , FORSY ,
        VARSY , MULOP , MULOP , SETSY , MULOP , NOTSY , THENSY , ELSESY
        , WITHSY , GOTOSY , CASESY , TYPESY , FILESY , BEGINSY ,
        UNTILSY , WHILESY , ARRAYSY , CONSTSY , LABELSY , BREAKSY ,
        REPEATSY , RECORDSY , DOWNTOSY , PACKEDSY , RETURNSY ,
        FORWARDSY , PROGSY , FRTRNSY , EXTRNSY , FUNCSY , CONTINUESY ,
        PROCSY , OTHERWISESY ) ;
      ROP : array [ 1 .. NRSW ] of OPERATOR =
      ( NOOP , NOOP , NOOP , NOOP , INOP , OROP , NOOP , NOOP , NOOP ,
        IDIV , IMOD , NOOP , ANDOP , NOOP , NOOP , NOOP , NOOP , NOOP ,
        NOOP , NOOP , NOOP , NOOP , NOOP , NOOP , NOOP , NOOP , NOOP ,
        NOOP , NOOP , NOOP , NOOP , NOOP , NOOP , NOOP , NOOP , NOOP ,
        NOOP , NOOP , NOOP , NOOP , NOOP ) ;
      MN : array [ 0 .. OPMAX ] of array [ 1 .. 4 ] of CHAR =
      ( ' ABI' , ' ABR' , ' ADI' , ' ADR' , ' AND' , ' DIF' , ' DVI' ,
        ' DVR' , ' SBR' , ' FLO' , ' FLT' , ' INN' , ' INT' , ' IOR' ,
        ' MOD' , ' MPI' , ' MPR' , ' NGI' , ' NGR' , ' NOT' , ' ODD' ,
        ' SBI' , ' DEC' , ' INC' , ' SQI' , ' SQR' , ' STO' , ' TRC' ,
        ' RND' , ' SCL' , ' CSP' , ' UNI' , ' ENT' , ' FJP' , ' POP' ,
        ' IND' , ' IXA' , ' LCA' , ' CTS' , ' CTI' , ' MOV' , ' MST' ,
        ' RET' , ' STP' , ' XJP' , ' CHK' , ' CUP' , ' EQU' , ' GEQ' ,
        ' GRT' , ' LDA' , ' LDC' , ' LEQ' , ' LES' , ' LOD' , ' NEQ' ,
        ' STR' , ' UJP' , ' NEW' , ' SAV' , ' RST' , ' ORD' , ' CHR' ,
        ' DEF' , ' LAB' , ' CRD' , ' XPO' , ' ASE' , ' SLD' , ' SMV' ,
        ' DFC' , ' CST' , ' BGN' , ' UXJ' , ' XLB' , ' END' , ' PAK' )
        ;
      SNA : array [ 0 .. NSPROC ] of array [ 1 .. 3 ] of CHAR =
      ( 'PAG' , 'GET' , 'PUT' , 'RES' , 'REW' , 'RDC' , 'WRI' , 'WRE' ,
        'WRR' , 'WRC' , 'WRS' , 'PAK' , 'RDB' , 'WRB' , 'RDR' , 'RDH' ,
        'RDY' , 'EOL' , 'EOT' , 'RDD' , 'WRD' , 'CLK' , 'WLN' , 'RLN' ,
        'RDI' , 'EOF' , 'ELN' , 'RDS' , 'TRP' , 'XIT' , 'FDF' , 'SIO' ,
        'EIO' , 'MSG' , 'SKP' , 'LIM' , 'TRA' ) ;

      (*********************************************************)
      (*-------------------------------------------------------*)
      (*********************************************************)




procedure ERROR ( FERRNR : ERRCODE ) ;

(*********************)
(*MAXERRNR DIV SETMAX*)
(*********************)


   var I : 0 .. 10 ;

   begin (* ERROR *)
     if ( ERRKIND <> 'W' ) or WARNING then
       begin
         if ERRINX >= 9 then
           begin
             FERRNR := 255 ;
             ERRINX := 10
           end (* then *)
         else
           ERRINX := ERRINX + 1 ;
         with ERRLIST [ ERRINX ] do
           begin
             KIND := ERRKIND ;
             NMR := FERRNR ;
             POS := CHCNT
           end (* with *) ;
         I := FERRNR DIV ( SSETMAX + 1 ) ;
         ERRLOG [ I ] := ERRLOG [ I ] + [ FERRNR MOD ( SSETMAX + 1 ) ]
                         ;
         if ERRKIND <> 'W' then
           ERRORCNT := ERRORCNT + 1
         else
           WARNCNT := WARNCNT + 1 ;
       end (* then *) ;
     ERRKIND := 'E' ;
   end (* ERROR *) ;



procedure PRINTLINE ;

   var DCN : ADDRRANGE ;

   begin (* PRINTLINE *)
     if PLCNT >= PAGESIZE then
       begin
         PAGECNT := PAGECNT + 1 ;
         PLCNT := 0 ;
         WRITELN ( OUTPUT , '1  LINE #  D/NEST  LVL' ,
                   '< STANFORD PASCAL, OPPOLZER VERSION OF ' : 44 ,
                   VERSION , ' >' , TIME : 14 , DATE , 'PAGE' : 8 ,
                   PAGECNT : 4 ) ;
         WRITELN ( OUTPUT , '------  ------  ---' : 22 , '---- ---' :
                   89 ) ;
         WRITELN ( OUTPUT ) ;
       end (* then *) ;
     if LINECNT > LASTLINELISTED then
       begin
         LASTLINELISTED := LINECNT ;
         PLCNT := PLCNT + 1 ;
         WRITE ( OUTPUT , LINECNT : 9 ) ;
         if LISTTAG = 'N' then
           DCN := STMTNEST
         else
           if LISTTAG = 'D' then
             DCN := LC
           else
             if LISTTAG = 'C' then
               DCN := CONSTLC
             else
               DCN := 0 ;
         if DCN > 0 then
           WRITE ( OUTPUT , DCN : 7 , LISTTAG , LEVEL : 3 )
         else
           WRITE ( OUTPUT , ' ' : 11 ) ;
         WRITELN ( OUTPUT , ') ' , LINEBUF : LINELEN ) ;
       end (* then *) ;
   end (* PRINTLINE *) ;



procedure PRINTERROR ;

   var LASTPOS , FREEPOS , CURRPOS , CURRNMR , F , K , DCN : INTEGER ;
       CURRKIND , LASTKIND : CHAR ;

   begin (* PRINTERROR *)
     PRINTLINE ;
     PLCNT := PLCNT + 2 ;

     (***************************)
     (* TWO LINES OF ERROR INFO *)
     (***************************)

     WRITE ( OUTPUT , '****' : 9 , ' ' : 13 ) ;
     LASTPOS := 0 ;
     FREEPOS := 1 ;
     LASTKIND := '?' ;
     for K := 1 to ERRINX do
       begin
         with ERRLIST [ K ] do
           begin
             CURRPOS := POS ;
             CURRNMR := NMR ;
             CURRKIND := KIND
           end (* with *) ;
         if CURRPOS = LASTPOS then
           if CURRKIND = LASTKIND then
             WRITE ( OUTPUT , ',' )
           else
             WRITE ( OUTPUT , CURRKIND )
         else
           begin
             LASTKIND := CURRKIND ;
             while FREEPOS < CURRPOS do
               begin
                 WRITE ( OUTPUT , ' ' ) ;
                 FREEPOS := FREEPOS + 1
               end (* while *) ;
             WRITE ( OUTPUT , CURRKIND ) ;
             LASTPOS := CURRPOS
           end (* else *) ;
         if CURRNMR < 10 then
           F := 1
         else
           if CURRNMR < 100 then
             F := 2
           else
             F := 3 ;
         WRITE ( OUTPUT , CURRNMR : F ) ;
         FREEPOS := FREEPOS + F + 1
       end (* for *) ;
     WRITELN ( OUTPUT ) ;
     ERRINX := 0 ;
     if ERRORCNT > 0 then
       PRCODE := FALSE ;
     if ERRLN > 0 then
       WRITELN ( OUTPUT , '****' : 9 ,
                 '  PREVIOUS ERROR/WARNING ON LINE -->' , ERRLN : 4 ) ;
     ERRLN := LINECNT ;
   end (* PRINTERROR *) ;



procedure ENDOFLINE ;

   label 10 ;

   var I : 1 .. 9 ;
       DCN : INTEGER ;

   begin (* ENDOFLINE *)
     if ERRINX > 0 then
       PRINTERROR ;
     READLN ( INPUT , LINEBUF ) ;
     LINELEN := BUFEND ;

     (*******************************************************)
     (*THIS WILL SPEED THINGS UP IF NO MARGIN IS SET/RESET  *)
     (*$D-  ... MUST BE IN EFFECT FOR THIS LOOP             *)
     (*******************************************************)

     repeat
       LINELEN := LINELEN - 1 ;
     until LINEBUF [ LINELEN ] <> ' ' ;

     (***********************************************************)
     (* IF NEEDED, DEBUG SWITCH SHOULD BE RESTORED HERE ---> $D+*)
     (***********************************************************)

     10 :
     if LINELEN > RMARGIN then
       begin
         MWARN := TRUE ;
         LASTCOL := RMARGIN
       end (* then *)
     else
       LASTCOL := LINELEN ;
     LINECNT := LINECNT + 1 ;
     if LIST then
       PRINTLINE ;
     if HP then
       begin
         IC := 0 ;
         LISTTAG := ' ' ;
         HP := FALSE
       end (* then *) ;
     LINEBUF [ LASTCOL + 1 ] := '#' ;

     (********************************************)
     (*TO STOP 'SKIPBLNK' + PROVIDE VALID EOL CH.*)
     (********************************************)

     CHCNT := LMARGIN ;
   end (* ENDOFLINE *) ;



procedure LISTMSGS ;

   var I , J : ERRCODE ;
       MSG : array [ 1 .. 64 ] of CHAR ;

   begin (* LISTMSGS *)
     WRITELN ( OUTPUT ) ;
     if ERRLN > 0 then
       begin
         WRITELN ( OUTPUT , '****' : 9 ,
                   '  LAST ERROR/WARNING ON LINE -->' , ERRLN : 4 ) ;
         WRITELN ( OUTPUT ) ;
       end (* then *) ;
     WRITELN ( OUTPUT ) ;
     WRITELN ( OUTPUT , '****' : 9 ,
               '  ERROR/WARNING CODES FOR THIS PROGRAM :' ) ;
     WRITELN ( OUTPUT ) ;
     RESET ( PRD ) ;
     J := 0 ;
     for I := 1 to MAXERRNR do
       if ( I MOD ( SSETMAX + 1 ) ) in ERRLOG [ I DIV ( SSETMAX + 1 ) ]
       then
         begin
           while ( not EOF ( PRD ) ) and ( I > J ) do
             READLN ( PRD , J , MSG ) ;
           if J = I then
             WRITELN ( '****' : 9 , J : 6 , MSG ) ;
         end (* then *) ;
   end (* LISTMSGS *) ;



procedure GOODBYE ;

   begin (* GOODBYE *)
     CTIME := ( CLOCK ( 0 ) - CTIME ) ;
     if PAGECNT = 0 then

     (***************************************)
     (* NO HEADING EVER PRINTED, DO ONE NOW *)
     (***************************************)

       WRITELN ( OUTPUT , '****' : 9 ,
                 'STANFORD PASCAL COMPILER, OPPOLZER VERSION OF ' : 50
                 , VERSION ) ;
     if not MUSIC then
       WRITELN ( OUTPUT ) ;
     if WARNING then
       begin
         if EXTUSED then
           WRITELN ( OUTPUT , '0' , '****' : 8 ,
                     '  WARNING: PASCAL EXTENSIONS USED.' ) ;
         if WARNCNT > 0 then
           WRITELN ( OUTPUT , '0' , '****' : 8 , WARNCNT : 8 ,
                     '  WARNING MESSAGE(S) ISSUED.' ) ;
         if MWARN then
           WRITELN ( OUTPUT , '0' , '****' : 8 ,
                     '  CONTENTS OF SOURCE LINES OUTSIDE  ' , LMARGIN :
                     1 , '..' , RMARGIN : 1 , '  MARGINS IGNORED.' ) ;
       end (* then *) ;
     if not MUSIC then
       WRITELN ( OUTPUT ) ;
     if ERRORCNT = 0 then
       WRITE ( OUTPUT , '****      NO' : 17 )
     else
       WRITE ( OUTPUT , '****' : 9 , ERRORCNT : 8 ) ;
     WRITELN ( OUTPUT , '  SYNTAX ERROR(S) DETECTED.' ) ;
     if not MUSIC then
       WRITELN ( OUTPUT ) ;
     WRITELN ( OUTPUT , '****' : 9 , LINECNT : 8 , '  LINE(S) READ, ' ,
               PROCLAB : 4 , ' PROCEDURE(S) COMPILED,' ) ;
     if not MUSIC then
       WRITELN ( OUTPUT ) ;
     WRITELN ( OUTPUT , '****' : 9 , OLDIC : 8 ,
               '  P_INSTRUCTIONS GENERATED,' , CTIME * 0.001 : 7 : 2 ,
               ' SECONDS IN COMPILATION.' ) ;
     if ERRORCNT > 0 then
       LISTMSGS ;
     EXIT ( ERRORCNT ) ;
   end (* GOODBYE *) ;



procedure FATALERROR ( CODE : ERRCODE ) ;

   begin (* FATALERROR *)
     ERROR ( CODE ) ;
     PRINTERROR ;
     if CODE <> 390 then
       WRITELN ( OUTPUT , '0    ****   FATAL ERROR - ' ,
                 'COMPILATION TERMINATED PREMATURELY.' ) ;
     GOODBYE ;
   end (* FATALERROR *) ;



procedure INSYMBOL ;

(**************************************************************)
(*                                                            *)
(*   READ NEXT BASIS SYMBOL OF SOURCE PROGRAM AND RETURN      *)
(*   ITS DESCRIPTION IN THE GLOBAL VARIABLES                  *)
(*   SY, OP, ID, VAL AND LNGTH                                *)
(*                                                            *)
(*------------------------------------------------------------*)
(*                                                            *)
(*   REWORKED 24.10.2011 - BERND OPPOLZER                     *)
(*                                                            *)
(*   ADDED THE FOLLOWING SYMBOL SPELLINGS:                    *)
(*                                                            *)
(*   (. AND .) AS ANOTHER POSSIBILITY FOR [ ] AND (/ /)       *)
(*                                                            *)
(*   -> AS AN ALTERNATIVE FOR @                               *)
(*                                                            *)
(*   COMMENTS ALSO LIKE THIS: /* ... COMMENT ... */           *)
(*                                                            *)
(**************************************************************)


   label 1 , 2 , 3 ;

   var I , K , STATE : INTEGER ;
       DIGIT : packed array [ 1 .. REALLNGTH ] of CHAR ;
       STRING : packed array [ 1 .. STRGLNGTH ] of CHAR ;
       LVP : CSP ;
       TEST , DIGSEEN : BOOLEAN ;


   procedure SKIPBLNK ;

   (******************************************************)
   (*   SKIP BLANKS, ENDOFLINE, AND (OPTIONAL) MARGIN,   *)
   (*   SKIPS AT LEAST ONE LINE                          *)
   (******************************************************)


      begin (* SKIPBLNK *)
        repeat
          if EOL then
            begin
              if EOF ( INPUT ) then
                FATALERROR ( 390 ) ;
              ENDOFLINE ;
            end (* then *) ;
          repeat
            CHCNT := CHCNT + 1 ;
          until LINEBUF [ CHCNT ] <> ' ' ;

        (********************************************)
        (*   NOTE THAT LINEBUF[LINELEN+1] <> ' '    *)
        (********************************************)

          EOL := CHCNT > LASTCOL ;
        until not EOL ;
        CH := LINEBUF [ CHCNT ] ;
      end (* SKIPBLNK *) ;


   procedure NEXTCH ;

      begin (* NEXTCH *)
        if EOL then
          begin
            if EOF ( INPUT ) then
              FATALERROR ( 390 ) ;
            ENDOFLINE ;
          end (* then *) ;
        CHCNT := CHCNT + 1 ;
        EOL := ( CHCNT > LASTCOL ) ;
        CH := LINEBUF [ CHCNT ] ;
      end (* NEXTCH *) ;


   procedure OPTIONS ( CCH : CHAR ) ;

   (***********************************)
   (*   CCH = COMMENT TERMINATOR CH   *)
   (***********************************)


      var SCH : CHAR ;
          OLDLIST : BOOLEAN ;
          TERMCH : CHAR ;


      function DECNUM : INTEGER ;

         var NUM : INTEGER ;

         begin (* DECNUM *)
           NUM := 0 ;
           NEXTCH ;
           while CH >= '0' do
             begin
               NUM := NUM * 10 + ORD ( CH ) - ORD ( '0' ) ;
               NEXTCH
             end (* while *) ;
           DECNUM := NUM
         end (* DECNUM *) ;


      begin (* OPTIONS *)
        if ( CCH = '/' ) or ( CCH = ')' ) then
          TERMCH := '*'
        else
          TERMCH := CHRBRACE ;
        repeat
          NEXTCH ;
          if CH in [ 'a' .. 'z' , 'A' .. 'Z' ] then
            begin
              SCH := UPSHIFT [ CH ] ;
              NEXTCH ;
              case SCH of
                'L' : begin
                        OLDLIST := LIST ;
                        LIST := CH <> '-' ;
                        if not OLDLIST then
                          if LIST then
                            PRINTLINE ;
                      end (* tag/ca *) ;
                'C' : PRCODE := CH <> '-' ;
                'E' : if LIST then
                        PLCNT := PAGESIZE ;
                'A' : ASSEMBLE := CH = '+' ;
                'M' : begin
                        if CH = '+' then
                          begin
                            LMARGIN := 0 ;
                            RMARGIN := 72 ;
                          end (* then *)
                        else
                          if CH = '-' then
                            begin
                              LMARGIN := 0 ;
                              BUFEND := BUFLEN ;
                              RMARGIN := BUFLEN ;
                            end (* then *)
                          else
                            if CH = '(' then
                              begin
                                LMARGIN := DECNUM - 1 ;
                                if LMARGIN < 0 then
                                  LMARGIN := 0 ;
                                if CH = ',' then
                                  RMARGIN := DECNUM
                                else
                                  RMARGIN := BUFLEN ;
                                if ( RMARGIN <= LMARGIN ) or ( RMARGIN
                                >= BUFLEN ) then
                                  RMARGIN := BUFLEN - 1 ;
                                BUFEND := BUFLEN ;
                              end (* then *)
                      end (* tag/ca *) ;
                'S' : SAVEREGS := CH <> '-' ;
                'F' : SAVEFPRS := CH <> '-' ;
                'D' : if CH >= '0' then
                        begin
                          DEBUG_LEV := ORD ( CH ) - ORD ( '0' ) ;
                          DEBUG := DEBUG_LEV >= 2 ;
                        end (* then *)
                      else
                        begin
                          DEBUG := CH <> '-' ;
                          DEBUG_LEV := ORD ( DEBUG ) * 2 ;
                        end (* else *) ;
                'V' : ASMVERB := CH = '+' ;
                'W' : WARNING := CH <> '-' ;
                'U' : GET_STAT := CH = '+' ;
                'P' : NOPACKING := CH = '-' ;
                'X' : begin
                        XLINK := CH = '+' ;

        (****************************************)
        (*   XLINK --> ALLOW '$' AS FIRST CH.   *)
        (****************************************)

                        if XLINK then
                          SOP [ '$' ] := ATOZCH ;
                      end (* tag/ca *) ;
                'K' : CTROPTION := CH = '+' ;
                'N' : NESTCOMM := CH = '+' ;
                'Z' : MUSIC := CH = '+' ;
              end (* case *) ;
              if CH <> TERMCH then
                if CH <> ',' then
                  NEXTCH ;
            end (* then *)
        until CH <> ','
      end (* OPTIONS *) ;


   procedure COMMENT ( CCH : CHAR ) ;

   (***********************************)
   (*   CCH = COMMENT TERMINATOR CH   *)
   (***********************************)


      var TERMCH : CHAR ;

      begin (* COMMENT *)
        if ( CCH = '/' ) or ( CCH = ')' ) then
          TERMCH := '*'
        else
          TERMCH := CHRBRACE ;

        (*****************************************)
        (*   SET TERMCH - EXPECTED COMMENT       *)
        (*   TERMINATING CHARACTER               *)
        (*****************************************)

        repeat
          if CH <> TERMCH then
            repeat
              if NESTCOMM then
                begin

        (*****************************************)
        (*   ONLY ALLOW NESTING OF COMMENTS      *)
        (*   OF SAME TYPE                        *)
        (*****************************************)

                  if CH = CHLBRACE then
                    if CCH = CHRBRACE then
                      begin
                        EOL := FALSE ;
                        COMMENT ( CHRBRACE )
                      end (* then *) ;
                  if CH = '(' then
                    if LINEBUF [ CHCNT + 1 ] = '*' then
                      if CCH = ')' then
                        begin
                          EOL := FALSE ;
                          NEXTCH ;
                          NEXTCH ;
                          COMMENT ( ')' ) ;
                        end (* then *) ;
                  if CH = '/' then
                    if LINEBUF [ CHCNT + 1 ] = '*' then
                      if CCH = '/' then
                        begin
                          EOL := FALSE ;
                          NEXTCH ;
                          NEXTCH ;
                          COMMENT ( '/' ) ;
                        end (* then *) ;
                end (* then *) ;
              if CHCNT > LASTCOL then
                begin
                  if EOF ( INPUT ) then
                    FATALERROR ( 390 ) ;
                  ENDOFLINE ;
                end (* then *) ;
              CHCNT := CHCNT + 1 ;
              CH := LINEBUF [ CHCNT ] ;
            until CH = TERMCH ;
          if CCH <> CHRBRACE then
            begin
              CHCNT := CHCNT + 1 ;
              CH := LINEBUF [ CHCNT ] ;
            end (* then *)
        until CH = CCH ;

        (*****************************************)
        (*   PREPARE FOR NEXT CALL TO 'NEXTCH'   *)
        (*****************************************)

        EOL := FALSE ;
      end (* COMMENT *) ;


   begin (* INSYMBOL *)
     1 :
     if CH = ' ' then
       SKIPBLNK ;
     case SOP [ CH ] of

     (*********************)
     (*   LETTER A TO Z   *)
     (*********************)

       ATOZCH :
         begin
           K := 0 ;
           ID := BLANKID ;
           repeat
             if K < IDLNGTH then
               begin
                 K := K + 1 ;
                 ID [ K ] := UPSHIFT [ CH ]
               end (* then *) ;
             NEXTCH
           until not ( SOP [ CH ] in [ ATOZCH , NUMCH , UNDSCH ,
           DOLLARCH ] ) ;
           for I := FRW [ K ] to FRW [ K + 1 ] - 1 do
             if RW [ I ] = ID then
               begin
                 SY := RSY [ I ] ;
                 OP := ROP [ I ] ;
                 goto 2
               end (* then *) ;
           SY := IDENT ;
           OP := NOOP ;
           2 :

         end (* tag/ca *) ;

     (*********************)
     (*   NUMBER 0 TO 9   *)
     (*********************)

       NUMCH : begin
                 OP := NOOP ;
                 K := 0 ;
                 SY := REALCONST ;
                 STATE := 0 ;
                 repeat
                   DIGSEEN := FALSE ;
                   3 :
                   K := K + 1 ;
                   if K <= DIGMAX then
                     DIGIT [ K ] := CH ;
                   NEXTCH ;
                   if SOP [ CH ] = NUMCH then
                     begin
                       DIGSEEN := TRUE ;
                       goto 3
                     end (* then *) ;
                   if DIGSEEN then
                     STATE := STATE + 1 ;
                   case STATE of

     (**************************************)
     (*   LEADING DIGIT SEQUENCE SCANNED   *)
     (**************************************)

                     0 , 1 : if CH = '.' then
                               STATE := 2
                             else
                               if CH = 'E' then
                                 STATE := 4
                               else
                                 if CH = 'e' then
                                   STATE := 4
                                 else
                                   begin
                                     SY := INTCONST ;
                                     STATE := 0
                                   end (* else *) ;

     (**********************************)
     (*   DECIMAL POINT JUST SCANNED   *)
     (*   if period follows, we have   *)
     (*   no float constant, but       *)
     (*   two dots.                    *)
     (*   if rparent follows, we have  *)
     (*   substitute for rbracket      *)
     (**********************************)

                     2 : if CH = '.' then
                           begin
                             SY := INTCONST ;
                             K := K - 1 ;
                             DOTFLG := TRUE ;
                             STATE := 0
                           end (* then *)
                         else
                           if CH = ')' then
                             begin
                               SY := INTCONST ;
                               K := K - 1 ;
                               DOTFLG := TRUE ;
                               STATE := 0
                             end (* then *)
                           else
                             if UPSHIFT [ CH ] = 'E' then
                               begin
                                 K := K - 1 ;
                                 STATE := 4
                               end (* then *)
                             else
                               STATE := - 1 ;

     (********************************************)
     (*   DIGIT SEQUENCE AFTER POINT JUST SEEN   *)
     (********************************************)

                     3 : if CH = 'E' then
                           STATE := 4
                         else
                           if CH = 'e' then
                             STATE := 4
                           else
                             STATE := 0 ;

     (*********************************)
     (*   EXPONENT SYMBOL JUST SEEN   *)
     (*********************************)

                     4 : if CH = '-' then
                           STATE := 6
                         else
                           if CH = '+' then
                             STATE := 6
                           else
                             STATE := - 1 ;

     (******************************************)
     (*   DIGIT STRING IN EXPONENT JUST SEEN   *)
     (******************************************)

                     5 , 7 : STATE := 0 ;

     (**********************************************)
     (*   BAD CHARACTER AFTER 'E+' OR 'E-' FOUND   *)
     (**********************************************)

                     6 : STATE := - 1 ;
                   end (* case *) ;
                 until STATE <= 0 ;

     (***********************************)
     (*   LEXICAL ERROR IN REAL CONST   *)
     (***********************************)

                 if STATE < 0 then
                   ERROR ( 201 ) ;
                 if SY = REALCONST then
                   begin
                     NEW ( VAL . VALP , REEL ) ;
                     with VAL . VALP -> do
                       begin
                         for I := 1 to REALLNGTH do
                           RVAL [ I ] := ' ' ;
                         if K <= DIGMAX then
                           for I := 2 to K + 1 do
                             RVAL [ I ] := DIGIT [ I - 1 ]
                         else
                           begin
                             ERROR ( 203 ) ;
                             UNPACK ( '0.0' , RVAL , 2 )
                           end (* else *)
                       end (* with *) ;
                   end (* then *)
                 else
                   begin
                     VAL . IVAL := 0 ;
                     if K > DIGMAX then
                       ERROR ( 203 )
                     else
                       with VAL do
                         for I := 1 to K do
                           if IVAL <= MXINT10 then
                             IVAL := IVAL * 10 + ( ORD ( DIGIT [ I ] )
                                     - ORD ( '0' ) )
                           else
                             begin
                               ERROR ( 203 ) ;
                               IVAL := 0
                             end (* else *)
                   end (* else *)
               end (* tag/ca *) ;

     (****************************)
     (*   QUOTE = HOCHKOMMA      *)
     (****************************)

       QUOTCH :
         begin
           LNGTH := 0 ;
           SY := STRINGCONST ;
           OP := NOOP ;
           repeat
             repeat
               NEXTCH ;
               if EOL then
                 begin
                   ERROR ( 202 ) ;
                   CH := ''''
                 end (* then *) ;
               LNGTH := LNGTH + 1 ;
               if LNGTH <= STRGLNGTH then
                 STRING [ LNGTH ] := CH
             until CH = '''' ;
             NEXTCH
           until CH <> '''' ;
           LNGTH := LNGTH - 1 ;

     (*****************************************)
     (*   NOW LNGTH = NR OF CHARS IN STRING   *)
     (*****************************************)

           if LNGTH = 1 then
             VAL . IVAL := ORD ( STRING [ 1 ] )
           else
             begin
               NEW ( LVP , STRG ) ;
               if LNGTH > STRGLNGTH then
                 begin
                   ERROR ( 398 ) ;
                   LNGTH := STRGLNGTH
                 end (* then *) ;
               if LNGTH <= 0 then
                 ERROR ( 205 ) ;
               with LVP -> do
                 begin
                   SLNGTH := LNGTH ;
                   SVAL := STRING
                 end (* with *) ;
               VAL . VALP := LVP
             end (* else *)
         end (* tag/ca *) ;

     (****************************)
     (*   COLON = DOPPELPUNKT    *)
     (****************************)

       COLONCH :
         begin
           OP := NOOP ;
           NEXTCH ;
           if CH = '=' then
             begin
               SY := BECOMES ;
               NEXTCH
             end (* then *)
           else
             SY := COLON
         end (* tag/ca *) ;

     (****************************)
     (*   DOT = PUNKT            *)
     (****************************)

       DOTCH : begin
                 OP := NOOP ;
                 if not DOTFLG then
                   NEXTCH ;
                 if CH = '.' then
                   begin
                     SY := DOTDOT ;
                     DOTFLG := FALSE ;
                     NEXTCH
                   end (* then *)
                 else
                   if CH = ')' then
                     begin
                       SY := RBRACK ;
                       DOTFLG := FALSE ;
                       NEXTCH
                     end (* then *)
                   else
                     SY := PERIOD
               end (* tag/ca *) ;

     (****************************)
     (*   LT = KLEINERZEICHEN    *)
     (****************************)

       LTOP : begin
                NEXTCH ;
                SY := RELOP ;
                if CH = '=' then
                  begin
                    OP := LEOP ;
                    NEXTCH
                  end (* then *)
                else
                  if CH = '>' then
                    begin
                      OP := NEOP ;
                      NEXTCH
                    end (* then *)
                  else
                    OP := LTOP
              end (* tag/ca *) ;

     (****************************)
     (*   GT = GROESSERZEICHEN   *)
     (****************************)

       GTOP : begin
                NEXTCH ;
                SY := RELOP ;
                if CH = '=' then
                  begin
                    OP := GEOP ;
                    NEXTCH
                  end (* then *)
                else
                  OP := GTOP
              end (* tag/ca *) ;

     (*******************************)
     (*   LPARCH = LINKE KLAMMER    *)
     (*******************************)

       LPARCH :
         begin
           NEXTCH ;
           if CH = '*' then
             begin
               NEXTCH ;
               if CH = '$' then
                 OPTIONS ( ')' ) ;
               COMMENT ( ')' ) ;
               NEXTCH ;
               goto 1 ;
             end (* then *) ;
           if CH = '/' then
             begin
               SY := LBRACK ;
               OP := NOOP ;
               NEXTCH
             end (* then *)
           else
             if CH = '.' then
               begin
                 SY := LBRACK ;
                 OP := NOOP ;
                 NEXTCH
               end (* then *)
             else
               begin
                 SY := LPARENT ;
                 OP := NOOP
               end (* else *)
         end (* tag/ca *) ;

     (**********************************************************)
     (*   vielleicht wurde schon .) gelesen, beim              *)
     (*   einlesen einer zahl, die wie eine gleitkommazahl     *)
     (*   aussah: (.1..10.) - altes problem der pascal-syntax  *)
     (**********************************************************)

       RPARCH :
         begin
           if DOTFLG then
             begin
               SY := SSY [ ']' ] ;
               OP := SOP [ ']' ] ;
               DOTFLG := FALSE ;
             end (* then *)
           else
             begin
               SY := SSY [ ')' ] ;
               OP := SOP [ ')' ] ;
             end (* else *) ;
           NEXTCH
         end (* tag/ca *) ;

     (**********************************************************)
     (*   DIVERSE OPERATORZEICHEN                              *)
     (*   VEREINFACHT, WEIL LOGIK WG. RBRACK NUR BEI           *)
     (*   RDIV NOETIG IST / OPPOLZER                           *)
     (**********************************************************)

       PLUS , MUL , EQOP , OROP , ANDOP , SPECH :
         begin
           SY := SSY [ CH ] ;
           OP := SOP [ CH ] ;
           NEXTCH
         end (* tag/ca *) ;

     (**********************************************************)
     (*   RDIV = SCHRAEGSTRICH                                 *)
     (*   MAYBE A RIGHT BRACKET, IF A RIGHT PARANTH. FOLLOWS   *)
     (*   AND: MAY START /* ... */ STYLE COMMENTS              *)
     (**********************************************************)

       RDIV : begin
                NEXTCH ;
                if CH = '*' then
                  begin
                    NEXTCH ;
                    if CH = '$' then
                      OPTIONS ( '/' ) ;
                    COMMENT ( '/' ) ;
                    NEXTCH ;
                    goto 1 ;
                  end (* then *) ;
                if CH = ')' then
                  begin
                    SY := RBRACK ;
                    OP := NOOP ;
                    NEXTCH ;
                  end (* then *)
                else
                  begin
                    SY := SSY [ '/' ] ;
                    OP := SOP [ '/' ] ;
                  end (* else *)
              end (* tag/ca *) ;

     (**********************************************************)
     (*   MINUS                                                *)
     (*   MAYBE A POINTER SYM, IF A GTOP FOLLOWS               *)
     (**********************************************************)

       MINUS : begin
                 NEXTCH ;
                 if CH = '>' then
                   begin
                     SY := ARROW ;
                     OP := NOOP ;
                     NEXTCH ;
                   end (* then *)
                 else
                   begin
                     SY := SSY [ '-' ] ;
                     OP := SOP [ '-' ] ;
                   end (* else *)
               end (* tag/ca *) ;

     (*****************************************)
     (*   DOUBLE QUOTE = ANFUEHRUNGSZEICHEN   *)
     (*   ALLES ZWISCHEN ANFUEHRUNGSZEICHEN   *)
     (*   WIRD IGNORIERT !!!                  *)
     (*****************************************)

       DQUOTCH :
         begin
           repeat
             NEXTCH
           until CH = '"' ;
           NEXTCH ;
           goto 1 ;
         end (* tag/ca *) ;

     (********************************************)
     (*   LBRACE = LINKE GESCHWEIFTE KLAMMER -   *)
     (*   STANDARD KOMMENTAR IN PASCAL -         *)
     (*   HINZUGEFUEGT VON DAVE EDWARDS          *)
     (********************************************)

       LBRACE :
         begin
           NEXTCH ;
           if CH = '$' then
             OPTIONS ( CHRBRACE ) ;
           COMMENT ( CHRBRACE ) ;
           NEXTCH ;
           goto 1 ;
         end (* tag/ca *) ;

     (****************************)
     (*   SKIPCH                 *)
     (****************************)

       SKIPCH :
         begin
           NEXTCH ;
           goto 1
         end (* tag/ca *) ;

     (****************************)
     (*   ILLEGALE ZEICHEN       *)
     (****************************)

       ILLEGCH , DOLLARCH , UNDSCH :
         begin
           SY := OTHERSY ;
           OP := NOOP ;
           ERROR ( 6 ) ;
           NEXTCH
         end (* tag/ca *)
     end (* case *)
   end (* INSYMBOL *) ;



function HASH ( ID : ALPHA ) : BKT_RNG ;

   var OL : record
              case INTEGER of
                1 :
                  ( IDK : ALPHA ) ;
                2 :
                  ( INT1 , INT2 , INT3 : INTEGER )
            end ;

   begin (* HASH *)
     with OL do
       begin
         IDK := ID ;

     (***********************************)
     (* NO OVERFLOW CHECK FOR NEXT STMT *)
     (***********************************)

         HASH := ABS ( ( INT1 * 2 + INT2 ) * 2 + INT3 ) MOD ( MAX_BKT +
                 1 ) ;
       end (* with *)
   end (* HASH *) ;



procedure ENTERID ( FCP : CTP ) ;

   label 1 ;

   var K : BKT_RNG ;
       NAM : ALPHA ;
       LCP : CTP ;

   begin (* ENTERID *)
     NAM := FCP -> . NAME ;
     K := HASH ( NAM ) ;
     LCP := BUCKET [ K ] ;
     FCP -> . DECL_LEV := LEVEL ;
     FCP -> . NEXT_IN_BKT := LCP ;
     BUCKET [ K ] := FCP ;

     (***************************************)
     (* NOW CHECK FOR DUPLICATE DECLARATION *)
     (***************************************)

     while LCP <> NIL do
       with LCP -> do
         begin
           if NAME = NAM then
             if KLASS <> FIELD then
               begin
                 if TOP = DECL_LEV then
                   begin
                     ERROR ( 101 ) ;
                     goto 1
                   end (* then *)
               end (* then *)
             else

     (*****************************)
     (* SPECIAL LOOKUP FOR FIELDS *)
     (*****************************)

               if TOP = OWNER -> . FLD_DISP_LEV then
                 begin
                   ERROR ( 101 ) ;
                   goto 1
                 end (* then *) ;
           LCP := NEXT_IN_BKT ;
         end (* with *) ;
     1 :
     if GET_STAT then
       if TOP = LEVEL then
         ENT_CNT [ LEVEL ] := ENT_CNT [ LEVEL ] + 1
       else
         FENT_CNT := FENT_CNT + 1 ;
   end (* ENTERID *) ;



procedure SEARCHSECTION ( FSP : STP ; var FCP : CTP ) ;

(****************************************************)
(* FINDS FIELD IN RECORD STRUCTURE INDICATED BY FSP *)
(****************************************************)


   label 1 ;

   var LCP : CTP ;

   begin (* SEARCHSECTION *)
     LCP := BUCKET [ HASH ( ID ) ] ;
     while LCP <> NIL do
       with LCP -> do
         begin
           if NAME = ID then
             if KLASS = FIELD then
               if OWNER = FSP then
                 begin
                   if GET_STAT then
                     begin
                       SF_CNT := SF_CNT + 1 ;
                       SF_TOT := SF_TOT + FSP -> . NO_FLDS ;
                     end (* then *) ;
                   goto 1 ;
                 end (* then *) ;
           LCP := NEXT_IN_BKT ;
         end (* with *) ;
     1 :
     FCP := LCP ;
   end (* SEARCHSECTION *) ;



procedure SEARCHID ( FIDCLS : SETOFIDS ; var FCP : CTP ) ;

   label 1 ;

   var LCP : CTP ;
       DL , EL : - 1 .. DISPLIMIT ;
       K : BKT_RNG ;

   begin (* SEARCHID *)
     K := HASH ( ID ) ;
     LCP := BUCKET [ K ] ;
     FCP := NIL ;
     EL := - 1 ;
     DISX := EL ;
     while LCP <> NIL do
       with LCP -> do
         begin
           if NAME = ID then
             begin
               if KLASS <> FIELD then
                 DL := DECL_LEV
               else
                 DL := OWNER -> . FLD_DISP_LEV ;
               if DL > DISX then
                 if KLASS in FIDCLS then
                   begin
                     FCP := LCP ;
                     DISX := DL ;
                     if TOP = LEVEL then

     (*******************************)
     (* NO POINT IN FURTHER SEARCH  *)
     (*                             *)
     (*                             *)
     (*******************************)

                       goto 1
                   end (* then *)
                 else
                   EL := DL ;
             end (* then *) ;
           LCP := NEXT_IN_BKT ;
         end (* with *) ;
     1 :
     if EL > DISX then

     (******************************)
     (* BAD IDENTIFIER ENCOUNTERED *)
     (******************************)

       if PRTERR then
         ERROR ( 103 ) ;
     if DISX < 0 then
       begin

     (*******************************)
     (* THE SEARCH WAS UNSUCCESSFUL *)
     (*******************************)

         if PRTERR then
           begin
             if EL < 0 then
               ERROR ( 104 ) ;
             NEW ( LCP ) ;
             if ( FIELD in FIDCLS ) and ( TOP > LEVEL ) then
               LCP -> := UFLDPTR ->
             else
               if VARS in FIDCLS then
                 LCP -> := UVARPTR ->
               else
                 if TYPES in FIDCLS then
                   LCP -> := UTYPPTR ->
                 else
                   if KONST in FIDCLS then
                     LCP -> := UCSTPTR ->
                   else
                     if PROC in FIDCLS then
                       LCP -> := UPRCPTR ->
                     else

     (********)
     (* FUNC *)
     (********)

                       LCP -> := UFCTPTR -> ;
             with LCP -> do
               begin
                 NAME := ID ;

     (**********************************)
     (* PREVENT RE-OCCURRENCE OF ERROR *)
     (**********************************)

                 DECL_LEV := LEVEL ;
                 NEXT_IN_BKT := BUCKET [ K ] ;
                 BUCKET [ K ] := LCP ;
                 FCP := LCP ;
                 if KLASS = FIELD then
                   OWNER := OPEN_RECORD ;
               end (* with *) ;
             DISX := LEVEL ;
           end (* then *)
         else
           DISX := 0 ;
       end (* then *) ;
     if GET_STAT then
       begin
         if DISX <= LEVEL then
           LU_CNT [ DISX , TOP ] := LU_CNT [ DISX , TOP ] + 1
         else
           WLU_CNT [ DISX - LEVEL , TOP - LEVEL ] := WLU_CNT [ DISX -
                                                   LEVEL , TOP - LEVEL
                                                   ] + 1 ;
         if TOP <> LEVEL then
           WE_CNT := WE_CNT + 1
       end (* then *) ;
   end (* SEARCHID *) ;



procedure GETBOUNDS ( FSP : STP ; var FMIN , FMAX : INTEGER ) ;

(************************************************)
(*GET INTERNAL BOUNDS OF SUBRANGE OR SCALAR TYPE*)
(*ASSUME (FSP <> NIL) AND (FSP@.FORM <= SUBRANG *)
(*E) AND (FSP <> INTPTR)                        *)
(*      AND NOT COMPTYPES(REALPTR,FSP)          *)
(************************************************)


   begin (* GETBOUNDS *)
     with FSP -> do
       if FORM = SUBRANGE then
         begin
           FMIN := MIN . IVAL ;
           FMAX := MAX . IVAL
         end (* then *)
       else
         begin
           FMIN := 0 ;
           if FSP = CHARPTR then
             FMAX := ORDCHMAX
           else
             if ( FORM = SCALAR ) and ( FSP -> . FCONST <> NIL ) then
               FMAX := FSP -> . FCONST -> . VALUES . IVAL
             else
               FMAX := 0
         end (* else *)
   end (* GETBOUNDS *) ;



function GETTYPE ( OPERAND : STP ) : INTEGER ;

   begin (* GETTYPE *)
     GETTYPE := ORD ( 'I' ) ;

     (***********************)
     (* ASSUME INTEGER TYPE *)
     (***********************)

     if OPERAND = NIL then
       begin
         if ERRORCNT = 0 then
           ERROR ( 401 )
       end (* then *)
     else
       if OPERAND -> . FORM > POWER then
         GETTYPE := ORD ( 'A' )
       else
         if OPERAND -> . FORM = POWER then
           GETTYPE := ORD ( 'S' )
         else
           if OPERAND -> . FORM = POINTER then
             GETTYPE := ORD ( 'A' )
           else
             if OPERAND = REALPTR then
               GETTYPE := ORD ( 'R' )
             else
               if OPERAND = BOOLPTR then
                 GETTYPE := ORD ( 'B' )
               else
                 if OPERAND -> . SIZE = HINTSIZE then
                   GETTYPE := ORD ( 'H' )
                 else
                   if OPERAND -> . SIZE = CHARSIZE then
                     GETTYPE := ORD ( 'C' )
                   else
                     ;
   end (* GETTYPE *) ;



function OPNDSETSIZE ( FATTR : ATTR ) : INTEGER ;

(*************************************************)
(* COMPUTES THE SIZE OF A SET USED AS AN OPERAND *)
(*************************************************)


   begin (* OPNDSETSIZE *)
     OPNDSETSIZE := 0 ;
     with FATTR do
       if TYPTR <> NIL then
         case KIND of
           CST : OPNDSETSIZE := CVAL . VALP -> . PLNGTH ;
           VARBL : case ACCESS of
                     DRCT , INDRCT :
                       OPNDSETSIZE := TYPTR -> . SIZE ;
                     STKEXPR :
                       OPNDSETSIZE := STKLEN ;
                     INXD : ERROR ( 400 ) ;
                   end (* case *) ;
           EXPR : ERROR ( 400 ) ;
         end (* case *) ;
   end (* OPNDSETSIZE *) ;



procedure GENLABEL ( var NXTLAB : LABELRNG ) ;

   begin (* GENLABEL *)
     INTLABEL := INTLABEL + 1 ;
     NXTLAB := INTLABEL
   end (* GENLABEL *) ;





(**********************************************************************)
(*THE FOLLOWING OUTPUTS A SYMBOL TABLE FILE FOR USE BY 'SNAPSHOT' PRO *)
(*GRAM                                                                *)
(**********************************************************************)




procedure PRNTSYMBL ( LCP : CTP ) ;

   var LINELN : INTEGER ;

       (*****************************************)
       (* CURRENT SYMBOL TABLE FILE LINE LENGTH *)
       (*****************************************)

       TPT1 : PRNTTYLISTP ;


   procedure CHECKLN ( LEN : INTEGER ) ;

      begin (* CHECKLN *)
        if ( LINELN + LEN ) >= 80 then
          begin
            WRITELN ( QRR ) ;
            WRITE ( QRR , ' ' ) ;
            LINELN := LEN
          end (* then *)
        else
          LINELN := LINELN + LEN
      end (* CHECKLN *) ;


   procedure PRNTVAR ( VRP : CTP ) ;

      FORWARD ;


   procedure PRNTTYPE ( TYPP : STP ) ;

      label 1 ;

      var VP , LVP : CTP ;
          RMIN , RMAX : INTEGER ;
          TPT , LPT : PRNTTYLISTP ;
          TNO : 0 .. 999 ;

      begin (* PRNTTYPE *)
        CHECKLN ( 4 ) ;
        if TYPP = INTPTR then
          WRITE ( QRR , 'I4; ' )
        else
          if TYPP = REALPTR then
            WRITE ( QRR , 'R; ' )
          else
            if TYPP = BOOLPTR then
              WRITE ( QRR , 'B; ' )
            else
              if TYPP = CHARPTR then
                WRITE ( QRR , 'C; ' )
              else
                if TYPP <> NIL then
                  with TYPP -> do
                    case FORM of
                      SUBRANGE :
                        if RANGETYPE = CHARPTR then
                          WRITE ( QRR , 'C; ' )
                        else
                          if RANGETYPE = INTPTR then
                            WRITE ( QRR , 'I' , SIZE : 1 , '; ' )
                          else
                            WRITE ( QRR , 'L' , SIZE : 1 , '; ' ) ;
                      SCALAR :
                        WRITE ( QRR , 'L' , SIZE : 1 , '; ' ) ;
                      POINTER :
                        begin
                          if ELTYPE <> NIL then
                            begin
                              TPT := PRNTTYPHD ;
                              LPT := TPT ;
                              while TPT <> NIL do
                                if TPT -> . ELT = ELTYPE then
                                  begin
                                    TNO := TPT -> . TNO ;
                                    goto 1
                                  end (* then *)
                                else
                                  begin
                                    LPT := TPT ;
                                    TPT := TPT -> . NXT ;
                                  end (* else *) ;
                              NEW ( TPT ) ;
                              if PRNTTYPHD = NIL then
                                PRNTTYPHD := TPT
                              else
                                LPT -> . NXT := TPT ;
                              with TPT -> do
                                begin
                                  NXT := NIL ;
                                  ELT := ELTYPE ;
                                  PRNTTYNO := PRNTTYNO + 1 ;
                                  TNO := PRNTTYNO
                                end (* with *) ;
                              TNO := PRNTTYNO ;
                            end (* then *)
                          else
                            TNO := 0 ;
                          1 :
                          CHECKLN ( 3 ) ;
                          WRITE ( QRR , 'P ' , TNO : 1 , '; ' ) ;
                        end (* tag/ca *) ;
                      POWER : if ELSET <> NIL then
                                begin
                                  WRITE ( QRR , 'S ' ) ;
                                  CHECKLN ( 10 ) ;
                                  GETBOUNDS ( ELSET , RMIN , RMAX ) ;
                                  WRITE ( QRR , RMIN : 1 , ' ' , RMAX :
                                          1 , ' ; ' ) ;
                                end (* then *) ;
                      FILES : begin
                                WRITE ( QRR , 'F ' ) ;
                                PRNTTYPE ( FILTYPE ) ;
                              end (* tag/ca *) ;
                      RECORDS :
                        begin
                          WRITE ( QRR , 'D' , ALN : 1 , '(' ) ;
                          VP := FSTFLD ;
                          LVP := VP ;
                          while VP <> NIL do
                            begin
                              PRNTVAR ( VP ) ;
                              LVP := VP ;
                              VP := VP -> . NEXT ;
                            end (* while *) ;
                          if RECVAR <> NIL then
                            begin
                              if RECVAR -> . TAGFIELDP <> NIL then
                                if RECVAR -> . TAGFIELDP -> . NAME <>
                                BLANKID then
                                  begin
                                    LVP := RECVAR -> . TAGFIELDP ;
                                    PRNTVAR ( LVP ) ;
                                  end (* then *) ;
                              if LVP <> NIL then
                                begin
                                  CHECKLN ( 12 ) ;
                                  RMAX := SIZE - LVP -> . FLDADDR ;
                                  if LVP -> . IDTYPE <> NIL then
                                    RMAX := RMAX - LVP -> . IDTYPE -> .
                                            SIZE ;
                                  if RMAX > 0 then
                                    WRITE ( QRR , 'ETC=X' , RMAX : 1 ,
                                            '; ' ) ;
                                end (* then *) ;
                            end (* then *) ;
                          CHECKLN ( 3 ) ;
                          WRITE ( QRR , '); ' ) ;
                        end (* tag/ca *) ;
                      ARRAYS :
                        if INXTYPE <> NIL then
                          begin
                            WRITE ( QRR , 'A ' ) ;
                            CHECKLN ( 26 ) ;
                            GETBOUNDS ( INXTYPE , RMIN , RMAX ) ;
                            WRITE ( QRR , RMIN : 1 , ' ' , RMAX : 1 ,
                                    ' ' ) ;
                            PRNTTYPE ( AELTYPE ) ;
                          end (* then *) ;
                    end (* case *)
                else
                  WRITE ( QRR , ';' ) ;
      end (* PRNTTYPE *) ;

          (************)
          (* PRNTTYPE *)
          (************)



   procedure PRNTVAR ;

      var I : 0 .. IDLNGTH ;

      begin (* PRNTVAR *)
        with VRP -> do
          begin
            I := IDLNGTH ;
            while NAME [ I ] = ' ' do
              I := I - 1 ;
            CHECKLN ( I + 1 ) ;
            WRITE ( QRR , NAME : I , '=' ) ;
            PRNTTYPE ( IDTYPE ) ;
          end (* with *)
      end (* PRNTVAR *) ;


   begin (* PRNTSYMBL *)
     if PRCODE then
       if LCP <> NIL then
         with LCP -> do
           begin
             if KLASS = VARS then
               begin
                 LINELN := 5 ;
                 if VKIND = FORMAL then
                   begin
                     WRITE ( QRR , '@ ' ) ;
                     LINELN := 7
                   end (* then *) ;
                 WRITE ( QRR , VADDR : 1 , ' ' ) ;
                 PRNTVAR ( LCP ) ;
               end (* then *)
             else
               if KLASS in [ PROC , FUNC ] then
                 begin
                   WRITELN ( QRR , '% ' , NAME , ' ' , PFNAME ) ;
                   LCP := PRMPTR ;
                   while LCP <> NIL do

     (*****************************)
     (* SKIP PROC/FUNC PARAMETERS *)
     (*****************************)

                     begin
                       if LCP -> . KLASS = VARS then
                         PRNTSYMBL ( LCP ) ;
                       LCP := LCP -> . NEXT
                     end (* while *) ;
                 end (* then *) ;
             WRITELN ( QRR ) ;
           end (* with *)
       else

     (**************************************)
     (* DUMP HEAP STORAGE TYPE DEFINITIONS *)
     (**************************************)

         begin
           TPT1 := PRNTTYPHD ;
           while TPT1 <> NIL do
             begin
               WRITE ( QRR , '>' , TPT1 -> . TNO : 1 , ' ' ) ;
               LINELN := 5 ;
               PRNTTYPE ( TPT1 -> . ELT ) ;
               WRITELN ( QRR ) ;
               TPT1 := TPT1 -> . NXT ;
             end (* while *) ;
           PRNTTYPHD := NIL ;
           PRNTTYNO := 0 ;
         end (* else *) ;
   end (* PRNTSYMBL *) ;





(*************)
(* PRNTSYMBL *)
(*************)




procedure BLOCK ( FSYS : SETOFSYS ; FSY : SYMBOL ; FPROCP : CTP ) ;

   var LSY : SYMBOL ;
       TEST : BOOLEAN ;
       SEGSIZE : LABELRNG ;
       LCP , FWRDPRCL : CTP ;
       DEC_ORDER : 0 .. 4 ;


   procedure SKIP ( FSYS : SETOFSYS ) ;

   (***********************************************)
   (*SKIP INPUT STRING UNTIL RELEVANT SYMBOL FOUND*)
   (***********************************************)


      begin (* SKIP *)
        while not ( SY in FSYS ) do
          INSYMBOL ;
      end (* SKIP *) ;


   procedure ALIGN ( var Q : ADDRRANGE ; P : ADDRRANGE ) ;

      begin (* ALIGN *)
        case P of
          REALSIZE :
            Q := ( ( Q + 7 ) DIV 8 ) * 8 ;
          INTSIZE

        "********"
        ",PTRSIZE"
        "********"


          : Q := ( ( Q + 3 ) DIV 4 ) * 4 ;
          HINTSIZE :
            if ODD ( Q ) then
              Q := Q + 1 ;
          CHARSIZE :
            ;
          otherwise

          : if ERRORCNT = 0 then
              ERROR ( 401 ) ;
        end (* case *) ;
      end (* ALIGN *) ;


   procedure CONSTANT ( FSYS : SETOFSYS ; var FSP : STP ; var FVALU :
                      VALU ) ;

      var LSP : STP ;
          LCP : CTP ;
          SIGN : ( NONE , POS , NEG ) ;
          LVP : CSP ;
          I : 2 .. REALLNGTH ;

      begin (* CONSTANT *)
        LSP := NIL ;
        FVALU . IVAL := 0 ;
        if not ( SY in CONSTBEGSYS ) then
          begin
            ERROR ( 50 ) ;
            SKIP ( FSYS + CONSTBEGSYS )
          end (* then *) ;
        if SY in CONSTBEGSYS then
          begin
            if SY = STRINGCONST then
              begin
                if LNGTH = 1 then
                  LSP := CHARPTR
                else
                  begin
                    NEW ( LSP , ARRAYS ) ;
                    with LSP -> do
                      begin
                        AELTYPE := CHARPTR ;
                        INXTYPE := NIL ;
                        SIZE := LNGTH * CHARSIZE ;
                        FORM := ARRAYS ;
                        ALN := CHARSIZE ;
                      end (* with *)
                  end (* else *) ;
                FVALU := VAL ;
                INSYMBOL
              end (* then *)
            else
              begin
                SIGN := NONE ;
                if ( SY = ADDOP ) and ( OP in [ PLUS , MINUS ] ) then
                  begin
                    if OP = PLUS then
                      SIGN := POS
                    else
                      SIGN := NEG ;
                    INSYMBOL
                  end (* then *) ;
                if SY = IDENT then
                  begin
                    SEARCHID ( [ KONST ] , LCP ) ;
                    with LCP -> do
                      begin
                        LSP := IDTYPE ;
                        FVALU := VALUES
                      end (* with *) ;
                    if SIGN <> NONE then
                      if LSP = INTPTR then
                        begin
                          if SIGN = NEG then
                            FVALU . IVAL := - FVALU . IVAL
                        end (* then *)
                      else
                        if LSP = REALPTR then
                          begin
                            if SIGN = NEG then
                              begin
                                NEW ( LVP , REEL ) ;
                                LVP -> . RVAL := FVALU . VALP -> . RVAL
                                                 ;
                                if LVP -> . RVAL [ 1 ] = '-' then
                                  LVP -> . RVAL [ 1 ] := '+'
                                else
                                  LVP -> . RVAL [ 1 ] := '-' ;
                                FVALU . VALP := LVP ;
                              end (* then *)
                          end (* then *)
                        else
                          ERROR ( 105 ) ;
                    INSYMBOL ;
                  end (* then *)
                else
                  if SY = INTCONST then
                    begin
                      if SIGN = NEG then
                        VAL . IVAL := - VAL . IVAL ;
                      LSP := INTPTR ;
                      FVALU := VAL ;
                      INSYMBOL
                    end (* then *)
                  else
                    if SY = REALCONST then
                      begin
                        if SIGN = NEG then
                          VAL . VALP -> . RVAL [ 1 ] := '-' ;
                        LSP := REALPTR ;
                        FVALU := VAL ;
                        INSYMBOL
                      end (* then *)
                    else
                      begin
                        ERROR ( 106 ) ;
                        SKIP ( FSYS )
                      end (* else *)
              end (* else *) ;
            if not ( SY in FSYS ) then
              begin
                ERROR ( 6 ) ;
                SKIP ( FSYS )
              end (* then *)
          end (* then *) ;
        FSP := LSP
      end (* CONSTANT *) ;


   function COMPTYPES ( FSP1 , FSP2 : STP ) : BOOLEAN ;

   (***************************************************************** *)
   (*****                                                             *)
   (*DECIDE WHETHER STRUCTURES POINTED AT BY FSP1 AND FSP2 ARE COMPAT *)
   (*IBLE                                                             *)
   (***************************************************************** *)
   (*****                                                             *)


      var NXT1 , NXT2 : CTP ;
          COMP : BOOLEAN ;
          LTESTP1 , LTESTP2 : TESTP ;

      begin (* COMPTYPES *)
        if FSP1 = FSP2 then
          COMPTYPES := TRUE
        else
          if ( FSP1 <> NIL ) and ( FSP2 <> NIL ) then
            if FSP1 -> . FORM = FSP2 -> . FORM then
              case FSP1 -> . FORM of
                SCALAR :
                  COMPTYPES := FALSE ;

        (**************************************************************)
        (* IDENTICAL SCALARS DECLARED ON DIFFERENT LEVELS ARE         *)
        (*                                                            *)
        (*                  NOT RECOGNIZED TO BE COMPATIBLE           *)
        (**************************************************************)

                SUBRANGE :
                  COMPTYPES := COMPTYPES ( FSP1 -> . RANGETYPE , FSP2
                               -> . RANGETYPE ) ;
                POINTER :
                  begin
                    COMP := FALSE ;
                    LTESTP1 := GLOBTESTP ;
                    LTESTP2 := GLOBTESTP ;
                    while LTESTP1 <> NIL do
                      with LTESTP1 -> do
                        begin
                          if ( ELT1 = FSP1 -> . ELTYPE ) and ( ELT2 =
                          FSP2 -> . ELTYPE ) then
                            COMP := TRUE ;
                          LTESTP1 := LASTTESTP
                        end (* with *) ;
                    if not COMP then
                      begin
                        NEW ( LTESTP1 ) ;
                        with LTESTP1 -> do
                          begin
                            ELT1 := FSP1 -> . ELTYPE ;
                            ELT2 := FSP2 -> . ELTYPE ;
                            LASTTESTP := GLOBTESTP
                          end (* with *) ;
                        GLOBTESTP := LTESTP1 ;
                        COMP := COMPTYPES ( FSP1 -> . ELTYPE , FSP2 ->
                                . ELTYPE )
                      end (* then *) ;
                    COMPTYPES := COMP ;
                    GLOBTESTP := LTESTP2
                  end (* tag/ca *) ;
                POWER : COMPTYPES := COMPTYPES ( FSP1 -> . ELSET , FSP2
                                     -> . ELSET ) ;
                ARRAYS :
                  COMPTYPES := COMPTYPES ( FSP1 -> . AELTYPE , FSP2 ->
                               . AELTYPE ) and ( FSP1 -> . SIZE = FSP2
                               -> . SIZE ) ;

        (**************************************************************)
        (*ALTERNATIVES: -- ADD A THIRD BOOLEAN TERM: INDEXTYPE MUST   *)
        (*                                                            *)
        (*                                   BE COMPATIBLE.           *)
        (*                                                            *)
        (*                                -- ADD A FOURTH BOOLEAN TER *)
        (*M: LOWBOUNDS MUST                                           *)
        (*                                   BE THE SAME              *)
        (**************************************************************)

                RECORDS :
                  begin
                    NXT1 := FSP1 -> . FSTFLD ;
                    NXT2 := FSP2 -> . FSTFLD ;
                    COMP := ( FSP1 -> . RECVAR = FSP2 -> . RECVAR ) ;
                    while COMP and ( NXT1 <> NIL ) and ( NXT2 <> NIL )
                    do
                      begin
                        if not COMPTYPES ( NXT1 -> . IDTYPE , NXT2 -> .
                        IDTYPE ) then
                          COMP := FALSE ;
                        if NXT1 -> . IDTYPE -> . SIZE <> NXT2 -> .
                        IDTYPE -> . SIZE then
                          COMP := FALSE ;
                        NXT1 := NXT1 -> . NEXT ;
                        NXT2 := NXT2 -> . NEXT
                      end (* while *) ;
                    COMPTYPES := COMP and ( NXT1 = NIL ) and ( NXT2 =
                                 NIL )
                  end (* tag/ca *) ;

        (**************************************************************)
        (*IDENTICAL RECORDS ARE RECOGNIZED TO BE COMPATIBLE           *)
        (*                                                            *)
        (*                  IFF NO VARIANTS OCCUR                     *)
        (**************************************************************)

                FILES : COMPTYPES := COMPTYPES ( FSP1 -> . FILTYPE ,
                                     FSP2 -> . FILTYPE )
              end (* case *)
            else

        (**************************)
        (*FSP1@.FORM <> FSP2@.FORM*)
        (**************************)

              if FSP1 -> . FORM = SUBRANGE then
                COMPTYPES := COMPTYPES ( FSP1 -> . RANGETYPE , FSP2 )
              else
                if FSP2 -> . FORM = SUBRANGE then
                  COMPTYPES := COMPTYPES ( FSP1 , FSP2 -> . RANGETYPE )
                else
                  COMPTYPES := FALSE
          else
            COMPTYPES := TRUE
      end (* COMPTYPES *) ;


   function STRING ( FSP : STP ) : BOOLEAN ;

      begin (* STRING *)
        STRING := FALSE ;
        if FSP <> NIL then
          if FSP -> . FORM = ARRAYS then
            STRING := COMPTYPES ( FSP -> . AELTYPE , CHARPTR )
      end (* STRING *) ;


   function CALC_SETSIZE ( ELT : STP ) : INTEGER ;

   (**************************************************)
   (* COMPUTES SIZE OF SET WHOSE ELEMENT TYPE IS ELT *)
   (**************************************************)


      var MIN , MAX : INTEGER ;

      begin (* CALC_SETSIZE *)
        MAX := - 1 ;
        if ELT <> NIL then
          GETBOUNDS ( ELT , MIN , MAX ) ;
        CALC_SETSIZE := ( ( MAX + SETPACK ) DIV SETPACK ) * WORDSIZE ;
      end (* CALC_SETSIZE *) ;


   procedure TYP ( FSYS : SETOFSYS ; var FSP : STP ; var FSIZE :
                 ADDRRANGE ) ;

      var LSP , LSP1 , LSP2 : STP ;
          OLDTOP : DISPRANGE ;
          LCP , LCP2 : CTP ;
          LSIZE , DISPL : ADDRRANGE ;
          LMIN , LMAX : INTEGER ;
          ALNFCT : ALNRNG ;
          OLDPACKST , PACKST2 : BOOLEAN ;


      procedure SIMPLETYPE ( FSYS : SETOFSYS ; var FSP : STP ) ;

         var LSP , LSP1 : STP ;
             LCP , LCP1 : CTP ;
             TTOP : DISPRANGE ;
             LCNT : INTEGER ;
             LVALU : VALU ;
             FLAG : BOOLEAN ;

         begin (* SIMPLETYPE *)
           if not ( SY in SIMPTYPEBEGSYS ) then
             begin
               ERROR ( 1 ) ;
               SKIP ( FSYS + SIMPTYPEBEGSYS )
             end (* then *) ;
           if SY in SIMPTYPEBEGSYS then
             begin
               if SY = LPARENT then
                 begin
                   TTOP := TOP ;

           (***************************************)
           (*DECL. CONSTS LOCAL TO INNERMOST BLOCK*)
           (***************************************)

                   TOP := LEVEL ;
                   NEW ( LSP , SCALAR , DECLARED ) ;
                   with LSP -> do
                     begin
                       SIZE := INTSIZE ;
                       FORM := SCALAR ;
                       SCALKIND := DECLARED
                     end (* with *) ;
                   LCP1 := NIL ;
                   LCNT := 0 ;
                   repeat
                     INSYMBOL ;
                     if SY = IDENT then
                       begin
                         NEW ( LCP , KONST ) ;
                         with LCP -> do
                           begin
                             NAME := ID ;
                             IDTYPE := LSP ;
                             NEXT := LCP1 ;
                             VALUES . IVAL := LCNT ;
                             KLASS := KONST
                           end (* with *) ;
                         ENTERID ( LCP ) ;
                         LCNT := LCNT + 1 ;
                         LCP1 := LCP ;
                         INSYMBOL
                       end (* then *)
                     else
                       ERROR ( 2 ) ;
                     if not ( SY in FSYS + [ COMMA , RPARENT ] ) then
                       begin
                         ERROR ( 6 ) ;
                         SKIP ( FSYS + [ COMMA , RPARENT ] )
                       end (* then *)
                   until SY <> COMMA ;
                   if not NOPACKING then
                     begin
                       LSP -> . SIZE := HINTSIZE ;
                       if PACKDATA then
                         if LCNT <= ORDCHMAX then
                           LSP -> . SIZE := CHARSIZE
                     end (* then *) ;
                   LSP -> . ALN := LSP -> . SIZE ;
                   LSP -> . FCONST := LCP1 ;
                   TOP := TTOP ;
                   if SY = RPARENT then
                     INSYMBOL
                   else
                     ERROR ( 4 )
                 end (* then *)
               else
                 begin
                   if SY = IDENT then
                     begin
                       SEARCHID ( [ TYPES , KONST ] , LCP ) ;
                       INSYMBOL ;
                       if LCP -> . KLASS = KONST then
                         begin
                           NEW ( LSP , SUBRANGE ) ;
                           with LSP -> , LCP -> do
                             begin
                               RANGETYPE := IDTYPE ;
                               FORM := SUBRANGE ;
                               if STRING ( RANGETYPE ) then
                                 begin
                                   ERROR ( 148 ) ;
                                   RANGETYPE := NIL
                                 end (* then *) ;
                               MIN := VALUES ;
                               SIZE := IDTYPE -> . SIZE
                             end (* with *) ;
                         end (* then *)
                       else
                         LSP := NIL ;

           (************************)
           (* SIGNALS TYPE ID SEEN *)
           (************************)

                     end (* then *)
                   else
                     begin
                       NEW ( LSP , SUBRANGE ) ;
                       LSP -> . FORM := SUBRANGE ;
                       CONSTANT ( FSYS + [ DOTDOT ] , LSP1 , LVALU ) ;
                       if STRING ( LSP1 ) then
                         begin
                           ERROR ( 148 ) ;
                           LSP1 := NIL
                         end (* then *) ;
                       with LSP -> do
                         begin
                           RANGETYPE := LSP1 ;
                           MIN := LVALU ;
                           SIZE := INTSIZE ;
                           if LSP1 <> NIL then
                             SIZE := LSP1 -> . SIZE ;
                         end (* with *) ;
                     end (* else *) ;
                   if LSP <> NIL then

           (**********************)
           (* CONSTANT JUST SEEN *)
           (**********************)

                     begin
                       if SY = DOTDOT then
                         INSYMBOL
                       else
                         ERROR ( 5 ) ;
                       CONSTANT ( FSYS , LSP1 , LVALU ) ;
                       LSP -> . MAX := LVALU ;
                       if LSP1 -> . SIZE > CHARSIZE then

           (*********************)
           (* SCOPE FOR PACKING *)
           (*                   *)
           (*                   *)
           (*********************)

                         if not NOPACKING then
                           if LVALU . IVAL <= 32767 then
                             if LSP -> . MIN . IVAL >= - 32768 then
                               begin
                                 LSP -> . SIZE := HINTSIZE ;
                                 if PACKDATA then
                                   if LVALU . IVAL <= ORDCHMAX then
                                     if LSP -> . MIN . IVAL >= 0 then
                                       LSP -> . SIZE := CHARSIZE ;
                               end (* then *) ;
                       LSP -> . ALN := LSP -> . SIZE ;
                       if LSP -> . RANGETYPE <> LSP1 then
                         ERROR ( 107 )
                     end (* then *)
                   else
                     begin
                       LSP := LCP -> . IDTYPE ;
                       FLAG := FALSE ;
                       if PACKDATA then
                         if not NOPACKING then
                           if LSP <> NIL then
                             with LSP -> do
                               if SIZE > CHARSIZE then
                                 if FORM = SCALAR then
                                   if SCALKIND = DECLARED then
                                     begin
                                       LCNT := - 1 ;
                                       LCP1 := FCONST ;
                                       while LCP1 <> NIL do
                                         begin
                                           LCNT := LCNT + 1 ;
                                           LCP1 := LCP1 -> . NEXT ;
                                         end (* while *) ;
                                       if LCNT <= ORDCHMAX then
                                         FLAG := TRUE
                                     end (* then *)
                                   else

                                 else
                                   if FORM = SUBRANGE then
                                     if MIN . IVAL >= 0 then
                                       if MAX . IVAL <= ORDCHMAX then
                                         FLAG := TRUE ;
                       if FLAG then
                         begin

           (************************************)
           (*CREATE PACKED VERSION OF THIS TYPE*)
           (************************************)

                           NEW ( LSP1 ) ;
                           LSP1 -> := LSP -> ;
                           with LSP1 -> do
                             begin
                               SIZE := CHARSIZE ;
                               ALN := CHARSIZE ;
                               if FORM = SCALAR then

           (*****************)
           (* CONVERT TO SU *)
           (*               *)
           (*               *)
           (*               *)
           (* BRANGE        *)
           (*****************)

                                 begin
                                   FORM := SUBRANGE ;
                                   RANGETYPE := LSP ;
                                   MIN . IVAL := 0 ;
                                   MAX . IVAL := LCNT ;
                                 end (* then *) ;
                             end (* with *) ;
                           LSP := LSP1 ;
                         end (* then *)
                     end (* else *) ;
                   if LSP <> NIL then
                     with LSP -> do
                       if FORM = SUBRANGE then
                         if RANGETYPE <> NIL then
                           if RANGETYPE = REALPTR then
                             ERROR ( 398 )
                           else
                             if MIN . IVAL > MAX . IVAL then
                               ERROR ( 102 )
                 end (* else *) ;
               FSP := LSP ;
               if not ( SY in FSYS ) then
                 begin
                   ERROR ( 6 ) ;
                   SKIP ( FSYS )
                 end (* then *)
             end (* then *)
           else
             FSP := NIL
         end (* SIMPLETYPE *) ;


      procedure FIELDLIST ( FSYS : SETOFSYS ; var FRECVAR : STP ; var
                          RECALN : ALNRNG ; FLDOWNER : STP ; var
                          FIRSTFLD : CTP ) ;

         label 10 ;

         var LCP , LCP1 , NXT , NXT1 : CTP ;
             LSP , LSP1 , LSP2 , LSP3 , LSP4 : STP ;
             MINSIZE , MAXSIZE , LSIZE : ADDRRANGE ;
             LVALU : VALU ;
             LALNFCT : ALNRNG ;

         begin (* FIELDLIST *)
           NXT := NIL ;
           FIRSTFLD := NIL ;
           LSP := NIL ;
           RECALN := 1 ;
           if not ( SY in FSYS + [ IDENT , CASESY ] ) then
             begin
               ERROR ( 19 ) ;
               SKIP ( FSYS + [ IDENT , CASESY ] )
             end (* then *) ;
           while SY = IDENT do
             begin
               NXT1 := NIL ;
               repeat
                 if SY = IDENT then
                   begin
                     NEW ( LCP , FIELD ) ;
                     with LCP -> do
                       begin
                         NAME := ID ;
                         IDTYPE := NIL ;
                         NEXT := NIL ;
                         OWNER := FLDOWNER ;
                         KLASS := FIELD ;
                         if GET_STAT then
                           FLDOWNER -> . NO_FLDS := FLDOWNER -> .
                                                   NO_FLDS + 1 ;
                       end (* with *) ;
                     if NXT1 = NIL then
                       NXT1 := LCP ;
                     if NXT <> NIL then
                       NXT -> . NEXT := LCP ;
                     NXT := LCP ;
                     ENTERID ( LCP ) ;
                     INSYMBOL
                   end (* then *)
                 else
                   ERROR ( 2 ) ;
                 if not ( SY in [ COMMA , COLON ] ) then
                   begin
                     ERROR ( 6 ) ;
                     SKIP ( FSYS + [ COMMA , COLON , SEMICOLON , CASESY
                            ] )
                   end (* then *) ;
                 TEST := SY <> COMMA ;
                 if not TEST then
                   INSYMBOL
               until TEST ;
               if SY = COLON then
                 INSYMBOL
               else
                 ERROR ( 5 ) ;
               if FIRSTFLD = NIL then
                 FIRSTFLD := NXT1 ;
               TYP ( FSYS + [ CASESY , SEMICOLON ] , LSP , LSIZE ) ;
               LALNFCT := 1 ;
               if LSP <> NIL then
                 LALNFCT := LSP -> . ALN ;
               while NXT1 <> NIL do

           (******************************************)
           (* ANY "FIELDS" DEFINED IN THIS ROUND ?   *)
           (*                                        *)
           (******************************************)

                 begin
                   with NXT1 -> do
                     begin
                       IDTYPE := LSP ;
                       ALIGN ( DISPL , LALNFCT ) ;
                       FLDADDR := DISPL ;
                       DISPL := DISPL + LSIZE ;
                       NXT1 := NEXT ;
                     end (* with *) ;
                 end (* while *) ;
               if LALNFCT > RECALN then
                 RECALN := LSP -> . ALN ;
               if SY = SEMICOLON then
                 begin
                   INSYMBOL ;
                   if not ( SY in [ IDENT , CASESY , ENDSY ] ) then

           (**************)
           (* IGNOR EXTR *)
           (*            *)
           (*            *)
           (*   A ;      *)
           (**************)

                     begin
                       ERROR ( 19 ) ;
                       SKIP ( FSYS + [ IDENT , CASESY ] )
                     end (* then *)
                 end (* then *)
             end (* while *) ;
           if SY = CASESY then
             begin
               NEW ( LSP , TAGFLD ) ;
               with LSP -> do
                 begin
                   TAGFIELDP := NIL ;
                   FSTVAR := NIL ;
                   FORM := TAGFLD
                 end (* with *) ;
               FRECVAR := LSP ;
               INSYMBOL ;
               if SY = IDENT then
                 begin
                   NEW ( LCP , FIELD ) ;
                   with LCP -> do
                     begin
                       NAME := ID ;
                       IDTYPE := NIL ;
                       KLASS := FIELD ;
                       NEXT := NIL ;

           (****************************************)
           (*FLDADDR WILL BE SET WHEN TYPE IS KNOWN*)
           (****************************************)

                       OWNER := FLDOWNER ;
                       if GET_STAT then
                         FLDOWNER -> . NO_FLDS := FLDOWNER -> . NO_FLDS
                                                  + 1 ;
                     end (* with *) ;
                   INSYMBOL ;
                   if SY = COLON then

           (**********************)
           (* EXPLICIT TAG FIELD *)
           (**********************)

                     begin
                       ENTERID ( LCP ) ;
                       INSYMBOL ;
                       if SY <> IDENT then
                         goto 10
                     end (* then *)
                   else
                     begin
                       ID := LCP -> . NAME ;
                       LCP -> . NAME := BLANKID ;
                     end (* else *) ;
                   SEARCHID ( [ TYPES ] , LCP1 ) ;
                   LSP1 := LCP1 -> . IDTYPE ;
                   if LSP1 <> NIL then
                     with LSP1 -> do
                       begin
                         if LCP -> . NAME <> BLANKID then
                           begin
                             ALIGN ( DISPL , ALN ) ;
                             if ALN > RECALN then
                               RECALN := ALN ;
                             LCP -> . FLDADDR := DISPL ;
                             DISPL := DISPL + SIZE ;
                           end (* then *) ;
                         if ( FORM <= SUBRANGE ) or STRING ( LSP1 )
                         then
                           begin
                             if COMPTYPES ( REALPTR , LSP1 ) then
                               ERROR ( 109 )
                             else
                               if STRING ( LSP1 ) then
                                 ERROR ( 398 ) ;
                             LCP -> . IDTYPE := LSP1 ;
                             LSP -> . TAGFIELDP := LCP ;
                           end (* then *)
                         else
                           ERROR ( 110 ) ;
                       end (* with *) ;
                   if LCP -> . NAME <> BLANKID then
                     INSYMBOL ;
                 end (* then *)
               else
                 10 :
                 begin
                   ERROR ( 2 ) ;
                   SKIP ( FSYS + [ OFSY , LPARENT ] )
                 end ;
               LSP -> . SIZE := DISPL ;
               if SY = OFSY then
                 INSYMBOL
               else
                 ERROR ( 8 ) ;
               LSP1 := NIL ;
               MINSIZE := DISPL ;
               MAXSIZE := DISPL ;
               repeat
                 LSP2 := NIL ;
                 repeat
                   CONSTANT ( FSYS + [ COMMA , COLON , LPARENT ] , LSP3
                              , LVALU ) ;
                   if LSP -> . TAGFIELDP <> NIL then
                     if not COMPTYPES ( LSP -> . TAGFIELDP -> . IDTYPE
                     , LSP3 ) then
                       ERROR ( 111 ) ;
                   NEW ( LSP3 , VARIANT ) ;
                   with LSP3 -> do
                     begin
                       NXTVAR := LSP1 ;
                       SUBVAR := LSP2 ;
                       VARVAL := LVALU ;
                       FSTSUBFLD := NIL ;
                       FORM := VARIANT
                     end (* with *) ;
                   LSP1 := LSP3 ;
                   LSP2 := LSP3 ;
                   TEST := SY <> COMMA ;
                   if not TEST then
                     INSYMBOL
                 until TEST ;
                 if SY = COLON then
                   INSYMBOL
                 else
                   ERROR ( 5 ) ;
                 if SY = LPARENT then
                   INSYMBOL
                 else
                   ERROR ( 9 ) ;
                 FIELDLIST ( FSYS + [ RPARENT , SEMICOLON ] , LSP2 ,
                             LALNFCT , FLDOWNER , LCP1 ) ;
                 if LALNFCT > RECALN then
                   RECALN := LALNFCT ;
                 if DISPL > MAXSIZE then
                   MAXSIZE := DISPL ;
                 while LSP3 <> NIL do
                   with LSP3 -> do
                     begin
                       LSP4 := SUBVAR ;
                       SUBVAR := LSP2 ;
                       SIZE := DISPL ;
                       FSTSUBFLD := LCP1 ;
                       LSP3 := LSP4
                     end (* with *) ;
                 if SY = RPARENT then
                   begin
                     INSYMBOL ;
                     if not ( SY in FSYS + [ SEMICOLON ] ) then
                       begin
                         ERROR ( 6 ) ;
                         SKIP ( FSYS + [ SEMICOLON ] )
                       end (* then *)
                   end (* then *)
                 else
                   ERROR ( 4 ) ;
                 TEST := SY <> SEMICOLON ;
                 if not TEST then
                   begin
                     DISPL := MINSIZE ;
                     INSYMBOL ;
                     TEST := SY = ENDSY ;

           (*****************)
           (* IGNORE EXTRA ;*)
           (*****************)

                   end (* then *)
               until TEST ;
               DISPL := MAXSIZE ;
               LSP -> . FSTVAR := LSP1 ;
             end (* then *)
           else
             FRECVAR := NIL
         end (* FIELDLIST *) ;


      begin (* TYP *)
        OLDPACKST := PACKDATA ;
        if not ( SY in TYPEBEGSYS ) then
          begin
            ERROR ( 10 ) ;
            SKIP ( FSYS + TYPEBEGSYS )
          end (* then *) ;
        if SY in TYPEBEGSYS then
          begin
            if SY in SIMPTYPEBEGSYS then
              SIMPLETYPE ( FSYS , FSP )
            else

        (********)
        (*@     *)
        (********)

              if SY = ARROW then
                begin
                  NEW ( LSP , POINTER ) ;
                  FSP := LSP ;
                  with LSP -> do
                    begin
                      ELTYPE := NIL ;
                      SIZE := PTRSIZE ;
                      ALN := PTRSIZE ;
                      FORM := POINTER
                    end (* with *) ;
                  INSYMBOL ;
                  if SY = IDENT then
                    begin
                      PRTERR := FALSE ;

        (***********************************)
        (*NO ERROR IF SEARCH NOT SUCCESSFUL*)
        (***********************************)

                      SEARCHID ( [ TYPES ] , LCP ) ;
                      PRTERR := TRUE ;
                      if LCP = NIL then

        (****************************)
        (*FORWARD REFERENCED TYPE ID*)
        (****************************)

                        begin
                          NEW ( LCP , TYPES ) ;
                          with LCP -> do
                            begin
                              NAME := ID ;
                              IDTYPE := LSP ;
                              NEXT := FWPTR ;
                              KLASS := TYPES
                            end (* with *) ;
                          FWPTR := LCP
                        end (* then *)
                      else
                        begin
                          if LCP -> . IDTYPE <> NIL then
                            if LCP -> . IDTYPE -> . FORM = FILES then
                              ERROR ( 108 )
                            else
                              LSP -> . ELTYPE := LCP -> . IDTYPE
                        end (* else *) ;
                      INSYMBOL ;
                    end (* then *)
                  else
                    ERROR ( 2 ) ;
                end (* then *)
              else
                begin

        "********"
        "01/07   "
        "********"

                  LSP := NIL ;
                  if SY = PACKEDSY then
                    begin
                      INSYMBOL ;
                      PACKDATA := TRUE ;
                      if not ( SY in TYPEDELS ) then
                        begin
                          ERROR ( 10 ) ;
                          SKIP ( FSYS + TYPEDELS )
                        end (* then *)
                    end (* then *)
                  else
                    PACKDATA := FALSE ;

        (********)
        (*ARRAY *)
        (********)

                  if SY = ARRAYSY then
                    begin
                      INSYMBOL ;
                      if SY = LBRACK then
                        INSYMBOL
                      else
                        begin
                          if SY = LPARENT then
                            begin
                              ERRKIND := 'W' ;
                              INSYMBOL
                            end (* then *) ;
                          ERROR ( 11 )
                        end (* else *) ;
                      LSP1 := NIL ;
                      PACKST2 := PACKDATA ;
                      PACKDATA := FALSE ;
                      repeat
                        NEW ( LSP , ARRAYS ) ;
                        with LSP -> do
                          begin
                            AELTYPE := LSP1 ;
                            INXTYPE := NIL ;
                            FORM := ARRAYS
                          end (* with *) ;
                        LSP1 := LSP ;
                        SIMPLETYPE ( FSYS + [ COMMA , RBRACK , OFSY ,
                                     RPARENT ] , LSP2 ) ;
                        if LSP2 <> NIL then
                          if LSP2 -> . FORM <= SUBRANGE then
                            begin
                              if LSP2 = REALPTR then
                                begin
                                  ERROR ( 109 ) ;
                                  LSP2 := NIL
                                end (* then *)
                              else
                                if LSP2 = INTPTR then
                                  begin
                                    ERROR ( 149 ) ;
                                    LSP2 := NIL
                                  end (* then *) ;
                              LSP -> . INXTYPE := LSP2
                            end (* then *)
                          else
                            begin
                              ERROR ( 113 ) ;
                              LSP2 := NIL
                            end (* else *) ;
                        TEST := SY <> COMMA ;
                        if not TEST then
                          INSYMBOL
                      until TEST ;
                      if SY = RBRACK then
                        INSYMBOL
                      else
                        begin
                          if SY = RPARENT then
                            begin
                              ERRKIND := 'W' ;
                              INSYMBOL
                            end (* then *) ;
                          ERROR ( 12 )
                        end (* else *) ;
                      if SY = OFSY then
                        INSYMBOL
                      else
                        ERROR ( 8 ) ;
                      PACKDATA := PACKST2 ;
                      TYP ( FSYS , LSP , LSIZE ) ;
                      if LSP <> NIL then
                        ALIGN ( LSIZE , LSP -> . ALN ) ;
                      repeat
                        with LSP1 -> do
                          begin
                            LSP2 := AELTYPE ;
                            AELTYPE := LSP ;
                            if INXTYPE <> NIL then
                              begin
                                GETBOUNDS ( INXTYPE , LMIN , LMAX ) ;
                                LSIZE := LSIZE * ( LMAX - LMIN + 1 ) ;
                                SIZE := LSIZE ;
                                if LSP <> NIL then
                                  ALN := LSP -> . ALN

        (*****************)
        (*PROPAGATE ALN  *)
        (*               *)
        (*               *)
        (*               *)
        (*               *)
        (*****************)


                                         ;
                              end (* then *)

        "********"
        "01/07   "
        "********"

                            else

        (***************)
        (*INXTYPE = NIL*)
        (***************)

                              SIZE := 0
                          end (* with *) ;
                        LSP := LSP1 ;
                        LSP1 := LSP2
                      until LSP1 = NIL
                    end (* then *)
                  else

        (********)
        (*RECORD*)
        (********)

                    if SY = RECORDSY then
                      begin
                        INSYMBOL ;
                        if GET_STAT then
                          RE_CNT := RE_CNT + 1 ;
                        if TOP < DISPLIMIT then
                          begin
                            TOP := TOP + 1 ;
                            with DISPLAY [ TOP ] do
                              OCCUR := REC
                          end (* then *)
                        else
                          FATALERROR ( 250 ) ;
                        DISPL := 0 ;
                        NEW ( LSP , RECORDS ) ;
                        with LSP -> do
                          begin
                            FLD_DISP_LEV := TOP ;
                            FSTFLD := NIL ;
                            NO_FLDS := 0 ;
                            FIELDLIST ( FSYS - [ SEMICOLON ] + [ ENDSY
                                        ] , LSP1 , ALNFCT , LSP ,
                                        FSTFLD ) ;
                            RECVAR := LSP1 ;
                            SIZE := DISPL ;
                            FORM := RECORDS ;
                            ALN := ALNFCT ;
                            FLD_DISP_LEV := - 1 ;
                          end (* with *) ;
                        TOP := TOP - 1 ;
                        if SY = ENDSY then
                          INSYMBOL
                        else
                          ERROR ( 13 )
                      end (* then *)
                    else

        (********)
        (*SET   *)
        (********)

                      if SY = SETSY then
                        begin
                          INSYMBOL ;
                          if SY = OFSY then
                            INSYMBOL
                          else
                            ERROR ( 8 ) ;
                          PACKDATA := FALSE ;
                          SIMPLETYPE ( FSYS , LSP1 ) ;
                          if LSP1 <> NIL then
                            if LSP1 = INTPTR then
                              ERROR ( 304 )
                            else
                              if ( LSP1 -> . FORM > SUBRANGE ) then
                                begin
                                  ERROR ( 115 ) ;
                                  LSP1 := NIL
                                end (* then *)
                              else
                                if LSP1 = REALPTR then
                                  ERROR ( 114 )
                                else
                                  if LSP1 -> . FORM = SUBRANGE then
                                    if LSP1 -> . MAX . IVAL > SETMAX
                                    then
                                      ERROR ( 304 ) ;
                          NEW ( LSP , POWER ) ;
                          with LSP -> do
                            begin
                              ELSET := LSP1 ;
                              SIZE := CALC_SETSIZE ( LSP1 ) ;
                              ALN := WORDSIZE ;
                              FORM := POWER
                            end (* with *) ;
                        end (* then *)
                      else

        (********)
        (*FILE  *)
        (********)

                        if SY = FILESY then
                          begin
                            INSYMBOL ;
                            if SY = OFSY then
                              INSYMBOL
                            else
                              ERROR ( 8 ) ;
                            TYP ( FSYS , LSP1 , LSIZE ) ;
                            LSP := TEXTPTR ;

        (**************************)
        (* ASSUME THE COMMON CASE *)
        (**************************)

                            if LSP1 <> NIL then
                              if LSP1 <> CHARPTR then

        (***************)
        (* NOT A TEXTF *)
        (*             *)
        (*             *)
        (*             *)
        (*       ILE   *)
        (***************)

                                if LSP1 -> . FORM <> FILES then
                                  begin
                                    NEW ( LSP , FILES ) ;
                                    with LSP -> do
                                      begin
                                        FILTYPE := LSP1 ;
                                        ALN := LSP1 -> . ALN ;
                                        SIZE := LSIZE + FILHDRSIZE ;
                                        if ALN < PTRSIZE then
                                          ALN := PTRSIZE ;
                                        FORM := FILES ;
                                      end (* with *)
                                  end (* then *)
                                else
                                  begin
                                    LSP := NIL ;
                                    ERROR ( 108 )
                                  end (* else *) ;
                          end (* then *) ;
                  FSP := LSP
                end (* else *) ;
            if not ( SY in FSYS ) then
              begin
                ERROR ( 6 ) ;
                SKIP ( FSYS )
              end (* then *)
          end (* then *)
        else
          FSP := NIL ;
        if FSP = NIL then
          FSIZE := 1
        else
          FSIZE := FSP -> . SIZE ;
        PACKDATA := OLDPACKST ;
      end (* TYP *) ;


   procedure LABELDECLARATION ;

      var LLP : LBP ;
          REDEF : BOOLEAN ;
          LBNAME : LABELRNG ;

      begin (* LABELDECLARATION *)
        repeat
          if SY = INTCONST then
            with DISPLAY [ TOP ] do
              begin
                LLP := FLABEL ;
                REDEF := FALSE ;
                while ( LLP <> NIL ) and not REDEF do
                  if LLP -> . LABVAL <> VAL . IVAL then
                    LLP := LLP -> . NEXTLAB
                  else
                    begin
                      REDEF := TRUE ;
                      ERROR ( 166 )
                    end (* else *) ;
                if not REDEF then
                  begin
                    NEW ( LLP ) ;
                    with LLP -> do
                      begin
                        LABVAL := VAL . IVAL ;
                        GENLABEL ( LBNAME ) ;
                        XNO := 0 ;

        (**********************************)
        (* 0 => NOT AN EXTERNAL ENTRY PT. *)
        (**********************************)

                        DEFINED := FALSE ;
                        NEXTLAB := FLABEL ;
                        LABNAME := LBNAME
                      end (* with *) ;
                    FLABEL := LLP
                  end (* then *) ;
                INSYMBOL
              end (* with *)
          else
            ERROR ( 15 ) ;
          if not ( SY in FSYS + [ COMMA , SEMICOLON ] ) then
            begin
              ERROR ( 6 ) ;
              SKIP ( FSYS + [ COMMA , SEMICOLON ] )
            end (* then *) ;
          TEST := SY <> COMMA ;
          if not TEST then
            INSYMBOL
        until TEST ;
        if SY = SEMICOLON then
          INSYMBOL
        else
          ERROR ( 14 )
      end (* LABELDECLARATION *) ;


   procedure WRITESET ( SP : CSP ) ;

      var I , J , K , L , E , COL , LEN : INTEGER ;
          S : SETRANGE ;
          DELIM : CHAR ;

      begin (* WRITESET *)
        LEN := SP -> . PLNGTH ;
        S := SP -> . PVAL ;
        I := 1 ;
        DELIM := '(' ;
        J := 0 ;
        K := 0 ;
        L := 0 ;
        COL := 8 ;
        for E := 1 to LEN * ( SETPACK DIV WORDSIZE ) do
          begin
            J := J * 2 ;
            if K in S [ I ] then
              J := J + 1 ;
            L := L + 1 ;
            if L = 16 then
              begin
                L := 0 ;
                if COL >= 65 then
                  begin
                    WRITELN ( PRR , DELIM ) ;
                    DELIM := ' ' ;
                    COL := 2
                  end (* then *) ;
                WRITE ( PRR , DELIM , J : 1 ) ;
                DELIM := ',' ;
                if J >= 1000 then
                  COL := COL + 6
                else
                  if J >= 10 then
                    COL := COL + 4
                  else
                    COL := COL + 2 ;
                J := 0 ;
              end (* then *) ;
            K := K + 1 ;
            if K > SSETMAX then
              begin
                K := 0 ;
                I := I + 1
              end (* then *) ;
          end (* for *) ;
        if LEN = 0 then
          WRITELN ( PRR , '()' )
        else
          WRITELN ( PRR , ')' ) ;
      end (* WRITESET *) ;


   procedure PUTIC ;

      begin (* PUTIC *)
        if LINECNT > OLDLN then
          begin
            WRITELN ( PRR , ' LOC ' , LINECNT : 1 ) ;
            OLDLN := LINECNT
          end (* then *) ;
      end (* PUTIC *) ;


   procedure STRUCTCONSTANT ( FSYS : SETOFSYS ; var FSP : STP ; var
                            FVALU : VALU ; var SLC : INTEGER ) ;

      label 10 ;

      var LSET : SETRANGE ;
          LVALU : VALU ;
          I , J , K , L , MAXELEM : INTEGER ;
          LSP , LSP1 , ELT , LRECVAR : STP ;
          FLDPR : CTP ;
          TEST : BOOLEAN ;
          CSTEXTNAME : array [ 1 .. EXTNAMSZ ] of CHAR ;


      procedure STOWCONST ( ELSP : STP ) ;

         var I , ELSIZE : INTEGER ;
             ELSP1 : STP ;
             CH : CHAR ;

         begin (* STOWCONST *)
           ELSP1 := ELSP ;
           if ELSP <> NIL then
             begin
               ALIGN ( CONSTLC , ELSP -> . ALN ) ;
               ELSIZE := ELSP -> . SIZE
             end (* then *)
           else
             ELSIZE := 1 ;
           STRUCTCONSTANT ( FSYS + [ COMMA , RPARENT ] , ELSP1 , LVALU
                            , I ) ;
           if not COMPTYPES ( ELSP , ELSP1 ) then
             begin
               ERROR ( 145 ) ;
               ELSP1 := NIL
             end (* then *) ;
           if ELSP1 <> NIL then
             if PRCODE then
               if I < 0 then
                 begin
                   PUTIC ;
                   WRITE ( PRR , CONSTLC : 1 , MN [ 70 ]

           (********)
           (*DFC   *)
           (********)


                           ) ;
                   if ELSP1 = REALPTR then
                     WRITELN ( PRR , 'R,' : 3 , LVALU . VALP -> . RVAL
                               )
                   else
                     if ELSP1 -> . FORM <= SUBRANGE then
                       begin
                         CH := 'I' ;
                         if ELSIZE = 2 then
                           CH := 'H'
                         else
                           if ELSIZE = 1 then
                             CH := 'B' ;
                         WRITELN ( PRR , CH : 2 , ',' , LVALU . IVAL :
                                   1 )
                       end (* then *)
                     else
                       if ELSP1 -> . FORM = POINTER then
                         WRITELN ( PRR , 'N' : 2 )
                       else
                         if ELSP1 -> . FORM = POWER then
                           begin
                             WRITE ( PRR , 'S,' : 3 ) ;
                             LVALU . VALP -> . PLNGTH := ELSP1 -> .
                                                   SIZE ;
                             WRITESET ( LVALU . VALP ) ;
                           end (* then *)
                         else
                           if STRING ( ELSP1 ) then
                             begin
                               WRITE ( PRR , 'M,''' : 4 ) ;
                               I := 1 ;
                               with LVALU . VALP -> do
                                 while I <= SLNGTH do
                                   begin
                                     WRITE ( PRR , SVAL [ I ] : 1 ) ;
                                     if SVAL [ I ] = '''' then
                                       WRITE ( PRR , '''' ) ;
                                     I := I + 1
                                   end (* while *) ;
                               WRITELN ( PRR , '''' ) ;
                             end (* then *) ;
                   CONSTLC := CONSTLC + ELSIZE ;
                 end (* then *) ;
         end (* STOWCONST *) ;


      begin (* STRUCTCONSTANT *)
        LSP := FSP ;
        FVALU . IVAL := 0 ;
        SLC := - 1 ;
        if SY in CONSTBEGSYS then

        (*****************)
        (*SIMPLE CONSTANT*)
        (*****************)

          begin
            CONSTANT ( FSYS , FSP , FVALU ) ;
            if not COMPTYPES ( LSP , FSP ) then
              begin
                ERROR ( 145 ) ;
                FSP := NIL
              end (* then *)
            else
              if LSP <> NIL then
                FSP := LSP
          end (* then *)
        else
          if SY = LBRACK then

        (**************)
        (*SET CONSTANT*)
        (**************)

            begin
              INSYMBOL ;
              ELT := NIL ;
              MAXELEM := - 1 ;
              for I := 1 to ( SETMAX + 1 ) DIV ( SSETMAX + 1 ) do
                LSET [ I ] := [ ] ;
              if LSP <> NIL then
                if LSP -> . FORM = POWER then
                  ELT := LSP -> . ELSET
                else
                  ERROR ( 145 ) ;
              TEST := FALSE ;
              if SY <> RBRACK then
                repeat
                  CONSTANT ( FSYS + [ RBRACK , COMMA , DOTDOT ] , LSP1
                             , LVALU ) ;
                  if not COMPTYPES ( LSP1 , ELT ) then
                    ERROR ( 145 ) ;
                  ELT := LSP1 ;
                  I := LVALU . IVAL ;
                  if SY = DOTDOT then
                    begin
                      INSYMBOL ;
                      CONSTANT ( FSYS + [ RBRACK , COMMA ] , LSP1 ,
                                 LVALU ) ;
                      if not COMPTYPES ( LSP1 , ELT ) then
                        begin
                          LVALU . IVAL := I ;
                          ERROR ( 137 )
                        end (* then *)
                    end (* then *) ;
                  if ( I < 0 ) or ( LVALU . IVAL > SETMAX ) or ( I >
                  LVALU . IVAL ) then
                    ERROR ( 137 )
                  else
                    begin
                      if LVALU . IVAL > MAXELEM then
                        MAXELEM := LVALU . IVAL ;
                      repeat
                        J := I DIV ( SSETMAX + 1 ) ;
                        LSET [ J + 1 ] := LSET [ J + 1 ] + [ I - J * (
                                          SSETMAX + 1 ) ] ;
                        I := I + 1
                      until I > LVALU . IVAL
                    end (* else *) ;
                  if SY = COMMA then
                    INSYMBOL
                  else
                    TEST := TRUE ;
                until TEST ;
              if SY = RBRACK then
                INSYMBOL
              else
                ERROR ( 12 ) ;
              NEW ( FVALU . VALP , PSET ) ;
              with FVALU . VALP -> do
                begin
                  PVAL := LSET ;
                  PLNGTH := ( ( MAXELEM + SETPACK ) DIV SETPACK ) *
                            WORDSIZE ;
                end (* with *) ;
              if LSP = NIL then
                begin
                  NEW ( LSP , POWER ) ;
                  with LSP -> do
                    begin
                      ELSET := ELT ;
                      FORM := POWER ;
                      SIZE := FVALU . VALP -> . PLNGTH ;
                      ALN := WORDSIZE
                    end (* with *) ;
                  FSP := LSP
                end (* then *)
            end (* then *)
          else
            if SY = LPARENT then

        (**************************)
        (*ARRAY OR RECORD CONSTANT*)
        (**************************)

              begin
                INSYMBOL ;
                K := 0 ;
                if CONSTLC < 0 then

        (****************************)
        (* NO CONSTANTS WRITTEN YET *)
        (****************************)

                  with FPROCP -> do
                    begin
                      CSTEXTNAME := EXTNAME ;
                      I := EXTNAMSZ ;
                      if FPROCP <> MAINPROG then
                        if not EXTRN then
                          I := 5 ;
                      repeat
                        CSTEXTNAME [ I ] := '#' ;
                        I := I - 1
                      until CSTEXTNAME [ I ] <> ' ' ;
                      WRITELN ( PRR , CSTEXTNAME , MN [ 71 ]

        (********)
        (*CST   *)
        (********)


                                , ' ' , NAME , PFNAME : 5 , ',' ,
                                ASSEMBLE : 1 , ',' , GET_STAT : 1 , ','
                                , ASMVERB : 1 ) ;
                      CONSTLC := FIRSTCONSTLC ;
                    end (* with *) ;
                if LSP <> NIL then
                  with LSP -> do
                    if FORM = ARRAYS then
                      begin
                        ALIGN ( CONSTLC , ALN ) ;
                        SLC := CONSTLC ;
                        J := SLC ;
                        if AELTYPE <> NIL then
                          L := AELTYPE -> . SIZE
                        else
                          L := 1 ;
                        ALIGN ( L , ALN ) ;
                        TEST := FALSE ;
                        repeat
                          K := K + 1 ;
                          STOWCONST ( AELTYPE ) ;
                          if SY = COMMA then
                            begin
                              INSYMBOL ;
                              J := J + L ;
                              CONSTLC := J
                            end (* then *)
                          else
                            TEST := TRUE
                        until TEST ;
                        if SY = RPARENT then
                          INSYMBOL
                        else
                          ERROR ( 4 ) ;
                        if INXTYPE <> NIL then
                          begin
                            GETBOUNDS ( INXTYPE , I , J ) ;
                            J := J - I + 1
                          end (* then *)
                        else
                          J := SIZE DIV L ;
                        if K <> J then
                          if K > J then
                            ERROR ( 207 )
                          else
                            begin
                              ERRKIND := 'W' ;
                              ERROR ( 306 ) ;
                              if PRCODE then
                                WRITELN ( PRR , SLC + SIZE - 1 : 1 , MN
                                          [ 70 ]

        (********)
        (*DFC   *)
        (********)


                                          , ' B,0' ) ;
                            end (* else *) ;
                        CONSTLC := SLC + SIZE ;
                      end (* then *)
                    else
                      if FORM = RECORDS then
                        begin
                          ALIGN ( CONSTLC , ALN ) ;
                          SLC := CONSTLC ;
                          L := SIZE ;
                          LRECVAR := RECVAR ;
                          TEST := TRUE ;
                          FLDPR := FSTFLD ;
                          10 :
                          while TEST and ( FLDPR <> NIL ) do
                            with FLDPR -> do
                              begin
                                CONSTLC := SLC + FLDADDR ;
                                STOWCONST ( IDTYPE ) ;
                                FLDPR := NEXT ;
                                if SY = COMMA then
                                  INSYMBOL
                                else
                                  TEST := FALSE
                              end (* with *) ;
                          if TEST then
                            if LRECVAR <> NIL then

        (*******************)
        (*TAG FIELD VALUE  *)
        (*                 *)
        (*                 *)
        (*        IS NEXT  *)
        (*******************)

                              with LRECVAR -> do
                                if TAGFIELDP <> NIL then
                                  with TAGFIELDP -> do
                                    begin
                                      if NAME <> BLANKID then
                                        begin
                                          CONSTLC := SLC + FLDADDR ;
                                          STOWCONST ( IDTYPE )
                                        end (* then *)
                                      else
                                        begin
                                          CONSTANT ( FSYS + [ COMMA ,
                                                   RPARENT ] , LSP1 ,
                                                   LVALU ) ;
                                          if not COMPTYPES ( IDTYPE ,
                                          LSP1 ) then
                                            ERROR ( 145 ) ;
                                        end (* else *) ;
                                      if SY = COMMA then
                                        INSYMBOL
                                      else
                                        TEST := FALSE ;
                                      LSP1 := FSTVAR ;
                                      L := SIZE ;
                                      while LSP1 <> NIL do
                                        with LSP1 -> do
                                          if VARVAL = LVALU then
                                            begin
                                              LRECVAR := SUBVAR ;
                                              L := SIZE ;
                                              FLDPR := FSTSUBFLD ;
                                              goto 10
                                            end (* then *)
                                          else
                                            LSP1 := NXTVAR ;
                                    end (* with *) ;
                          CONSTLC := SLC + L ;
                          if SY <> RPARENT then
                            ERROR ( 4 )
                          else
                            INSYMBOL ;
                        end (* then *)
                      else
                        ERROR ( 208 ) ;

        (*************************)
        (*WRONG FORM FOR CONSTANT*)
        (*************************)

              end (* then *)
            else
              ERROR ( 50 ) ;
      end (* STRUCTCONSTANT *) ;


   procedure CONSTDECLARATION ;

      var LCP : CTP ;
          LSP : STP ;
          LVALU : VALU ;
          SKID : ALPHA ;
          SKLC : ADDRRANGE ;

      begin (* CONSTDECLARATION *)
        LISTTAG := 'C' ;
        if SY <> IDENT then
          begin
            ERROR ( 2 ) ;
            SKIP ( FSYS + [ IDENT ] )
          end (* then *) ;
        while SY = IDENT do
          begin
            SKID := ID ;
            INSYMBOL ;
            if SY = COLON then
              begin
                INSYMBOL ;
                EXTUSED := TRUE ;
                TYP ( FSYS + [ RELOP ] , LSP , SKLC ) ;
              end (* then *)
            else
              LSP := NIL ;
            if ( SY = RELOP ) and ( OP = EQOP ) then
              begin
                INSYMBOL ;
                STRUCTCONSTANT ( FSYS + [ SEMICOLON ] , LSP , LVALU ,
                                 SKLC ) ;
                if SKLC >= 0 then
                  NEW ( LCP , STRUCTKONST )
                else
                  NEW ( LCP , KONST ) ;
                with LCP -> do
                  begin
                    NAME := SKID ;
                    IDTYPE := LSP ;
                    NEXT := NIL ;
                    if SKLC >= 0 then
                      begin
                        KLASS := STRUCTKONST ;
                        SKOWNER := FPROCP ;
                        SKADDR := SKLC
                      end (* then *)
                    else
                      begin
                        KLASS := KONST ;
                        VALUES := LVALU
                      end (* else *)
                  end (* with *) ;
                ENTERID ( LCP ) ;
              end (* then *)
            else
              ERROR ( 16 ) ;
            if SY = SEMICOLON then
              begin
                INSYMBOL ;
                if not ( SY in FSYS + [ IDENT ] ) then
                  begin
                    ERROR ( 6 ) ;
                    SKIP ( FSYS + [ IDENT ] )
                  end (* then *)
              end (* then *)
            else
              ERROR ( 14 )
          end (* while *) ;
        LISTTAG := ' ' ;
      end (* CONSTDECLARATION *) ;


   procedure TYPEDECLARATION ;

      var LCP , LCP1 , LCP2 : CTP ;
          LSP : STP ;
          LSIZE : ADDRRANGE ;

      begin (* TYPEDECLARATION *)
        if SY <> IDENT then
          begin
            ERROR ( 2 ) ;
            SKIP ( FSYS + [ IDENT ] )
          end (* then *) ;
        while SY = IDENT do
          begin
            NEW ( LCP , TYPES ) ;
            with LCP -> do
              begin
                NAME := ID ;
                IDTYPE := NIL ;
                KLASS := TYPES
              end (* with *) ;
            INSYMBOL ;
            if ( SY = RELOP ) and ( OP = EQOP ) then
              INSYMBOL
            else
              ERROR ( 16 ) ;
            TYP ( FSYS + [ SEMICOLON ] , LSP , LSIZE ) ;
            ENTERID ( LCP ) ;
            LCP -> . IDTYPE := LSP ;

        (*******************************************)
        (*HAS ANY FORWARD REFERENCE BEEN SATISFIED:*)
        (*******************************************)

            LCP1 := FWPTR ;
            while LCP1 <> NIL do
              begin
                if LCP1 -> . NAME = LCP -> . NAME then
                  begin
                    LCP1 -> . IDTYPE -> . ELTYPE := LCP -> . IDTYPE ;
                    if LCP1 <> FWPTR then
                      LCP2 -> . NEXT := LCP1 -> . NEXT
                    else
                      FWPTR := LCP1 -> . NEXT ;
                  end (* then *) ;
                LCP2 := LCP1 ;
                LCP1 := LCP1 -> . NEXT
              end (* while *) ;
            if SY = SEMICOLON then
              begin
                INSYMBOL ;
                if not ( SY in FSYS + [ IDENT ] ) then
                  begin
                    ERROR ( 6 ) ;
                    SKIP ( FSYS + [ IDENT ] )
                  end (* then *)
              end (* then *)
            else
              ERROR ( 14 )
          end (* while *) ;
        if FWPTR <> NIL then
          begin
            ERROR ( 117 ) ;
            WRITELN ( OUTPUT ) ;
            PLCNT := PLCNT + 1 ;
            repeat
              WRITELN ( OUTPUT , ' UNDEFINED TYPE: ' , FWPTR -> . NAME
                        ) ;
              PLCNT := PLCNT + 1 ;
              FWPTR := FWPTR -> . NEXT
            until FWPTR = NIL ;
          end (* then *)
      end (* TYPEDECLARATION *) ;


   procedure VARDECLARATION ;

      var LCP , NXT , NXT1 : CTP ;
          LSP : STP ;
          LSIZE : ADDRRANGE ;
          LFPTR : FRECPTR ;

      begin (* VARDECLARATION *)
        LISTTAG := 'D' ;
        NXT := NIL ;
        repeat
          NXT1 := NIL ;
          repeat
            if SY = IDENT then
              begin
                NEW ( LCP , VARS ) ;
                with LCP -> do
                  begin
                    NAME := ID ;
                    NEXT := NIL ;
                    KLASS := VARS ;
                    IDTYPE := NIL ;
                    VKIND := ACTUAL ;
                    VLEV := LEVEL
                  end (* with *) ;
                ENTERID ( LCP ) ;
                if NXT1 = NIL then
                  NXT1 := LCP ;

        (*************************)
        (*BEGINNING OF THIS ROUND*)
        (*************************)

                if NXT <> NIL then
                  NXT -> . NEXT := LCP ;

        (************************)
        (*LINK TO PREVIOUS CHAIN*)
        (************************)

                NXT := LCP ;
                INSYMBOL ;
              end (* then *)
            else
              ERROR ( 2 ) ;
            if not ( SY in FSYS + [ COMMA , COLON ] + TYPEDELS ) then
              begin
                ERROR ( 6 ) ;
                SKIP ( FSYS + [ COMMA , COLON , SEMICOLON ] + TYPEDELS
                       )
              end (* then *) ;
            TEST := SY <> COMMA ;
            if not TEST then
              INSYMBOL
          until TEST ;
          if SY = COLON then
            INSYMBOL
          else
            ERROR ( 5 ) ;
          TYP ( FSYS + [ SEMICOLON ] + TYPEDELS , LSP , LSIZE ) ;
          while NXT1 <> NIL do
            with NXT1 -> do
              begin
                IDTYPE := LSP ;
                if LSP <> NIL then
                  begin
                    ALIGN ( LC , LSP -> . ALN ) ;
                    VADDR := LC ;
                    LC := LC + LSIZE ;
                    if LSP -> . FORM = FILES then
                      if LEVEL > 1 then
                        ERROR ( 398 )

        (*******************************)
        (* ONLY GLOBAL FILES SUPPORTED *)
        (*******************************)

                      else
                        begin
                          NEW ( LFPTR ) ;
                          LFPTR -> . FILIDPTR := NXT1 ;
                          LFPTR -> . NEXTFILE := FILEHEAD ;
                          FILEHEAD := LFPTR
                        end (* else *)
                  end (* then *)
                else
                  VADDR := LC ;
                if DEBUG_LEV > 0 then
                  PRNTSYMBL ( NXT1 ) ;
                NXT1 := NEXT ;
              end (* with *) ;
          if SY = SEMICOLON then
            begin
              INSYMBOL ;
              if not ( SY in FSYS + [ IDENT ] ) then
                begin
                  ERROR ( 6 ) ;
                  SKIP ( FSYS + [ IDENT ] )
                end (* then *)
            end (* then *)
          else
            ERROR ( 14 )
        until ( SY <> IDENT ) and not ( SY in TYPEDELS ) ;
        LISTTAG := ' ' ;
        if FWPTR <> NIL then
          begin
            ERROR ( 117 ) ;
            WRITELN ( OUTPUT ) ;
            PLCNT := PLCNT + 1 ;
            repeat
              WRITELN ( OUTPUT , ' UNDEFINED TYPE: ' , FWPTR -> . NAME
                        ) ;
              PLCNT := PLCNT + 1 ;
              FWPTR := FWPTR -> . NEXT
            until FWPTR = NIL ;
          end (* then *) ;
      end (* VARDECLARATION *) ;


   procedure MKNAME ( var ALB : ALPHA ; NLB : INTEGER ; NCFLAG :
                    BOOLEAN ) ;

      var I , J : INTEGER ;

      begin (* MKNAME *)
        I := 1 ;
        J := 8 ;
        if not NCFLAG then
          J := 5 ;
        repeat
          if ALB [ I ] = '_' then
            ALB [ I ] := '$' ;
          I := I + 1
        until ( I > J ) or ( ALB [ I ] = ' ' ) ;
        if not NCFLAG then
          for J := 8 DOWNTO I do
            begin
              ALB [ J ] := CHR ( ORD ( '0' ) + NLB MOD 10 ) ;
              NLB := NLB DIV 10
            end (* for *) ;
      end (* MKNAME *) ;


   procedure PROCDECLARATION ( FSY : SYMBOL ) ;

      label 10 ;

      var LSY : SYMBOL ;
          LCP , LCP1 , LCP2 : CTP ;
          LSP : STP ;
          FORW : BOOLEAN ;
          K , PARCNT : INTEGER ;
          OLDLABEL : LABELRNG ;
          LLC , LCM : ADDRRANGE ;
          I , NAME : INTEGER ;
          MARKP : -> INTEGER ;
          OLD_HASH : HASH_TABLE ;


      procedure PARAMETERLIST ( FSY : SETOFSYS ; FPAR : CTP ; FW :
                              BOOLEAN ) ;

         var LCP , LCP1 , LCP2 , LCP3 , LCP4 : CTP ;
             LSP : STP ;
             LKIND : IDKIND ;
             LLC , LEN , LALN : ADDRRANGE ;
             LSY : SYMBOL ;

         begin (* PARAMETERLIST *)
           LCP1 := NIL ;
           LCP := NIL ;
           if not ( SY in FSY + [ LPARENT ] ) then
             begin
               ERROR ( 7 ) ;
               SKIP ( FSYS + FSY + [ LPARENT ] )
             end (* then *) ;
           if SY = LPARENT then
             begin
               if FORW then
                 ERROR ( 119 )
               else
                 LC := LCAFTMST + FPSAVEAREA ;
               INSYMBOL ;
               if not ( SY in [ IDENT , VARSY , PROCSY , FUNCSY ] )
               then
                 begin
                   ERROR ( 7 ) ;
                   SKIP ( FSYS + [ IDENT , RPARENT ] )
                 end (* then *) ;
               while SY in [ IDENT , VARSY , PROCSY , FUNCSY ] do
                 begin
                   LCP3 := NIL ;
                   if SY in [ PROCSY , FUNCSY ] then
                     begin
                       LSY := SY ;

           (***************************)
           (*REMEMBER IF PROC OR FUNC *)
           (***************************)

                       INSYMBOL ;
                       if SY = IDENT then
                         begin
                           if LSY = PROCSY then
                             NEW ( LCP , PROC , DECLARED )
                           else
                             NEW ( LCP , FUNC , DECLARED ) ;
                           if LCP3 <> NIL then
                             LCP4 -> . NEXT := LCP
                           else
                             LCP3 := LCP ;
                           LCP4 := LCP ;
                           ALIGN ( LC , PTRSIZE ) ;
                           with LCP -> do
                             begin
                               NAME := ID ;
                               IDTYPE := NIL ;
                               NEXT := NIL ;
                               PFDECKIND := DECLARED ;
                               PFKIND := FORMAL ;
                               FRTRN := FALSE ;
                               EXTRN := FALSE ;
                               EXTNAME := '*PFPARM*' ;
                               PFLEV := LC * 10 + LEVEL ;

           (***************************************)
           (*I.E. PFLEV > LCAFTMST => PROC PARM   *)
           (*                                     *)
           (*                                     *)
           (***************************************)

                               PROCLAB := PROCLAB + 1 ;
                               PFNAME := PROCLAB ;
                               if LSY = PROCSY then
                                 KLASS := PROC
                               else
                                 KLASS := FUNC ;
                             end (* with *) ;
                           ENTERID ( LCP ) ;
                           LC := LC + DISPAREA ;
                           INSYMBOL ;
                           LLC := LC ;
                           if LSY = PROCSY then
                             PARAMETERLIST ( [ SEMICOLON , RPARENT ] ,
                                             LCP , FALSE )
                           else
                             PARAMETERLIST ( [ SEMICOLON , COLON ] ,
                                             LCP , FALSE ) ;
                           LC := LLC ;
                         end (* then *)
                       else
                         ERROR ( 2 ) ;
                       if not ( SY in FSYS + [ SEMICOLON , RPARENT ] )
                       then
                         begin
                           ERROR ( 7 ) ;
                           SKIP ( FSYS + [ SEMICOLON , RPARENT ] )
                         end (* then *)
                     end (* then *)
                   else
                     begin
                       if SY = VARSY then
                         begin
                           LKIND := FORMAL ;
                           INSYMBOL
                         end (* then *)
                       else
                         LKIND := ACTUAL ;
                       repeat
                         if SY = IDENT then
                           begin
                             NEW ( LCP , VARS ) ;
                             if LCP3 <> NIL then
                               LCP4 -> . NEXT := LCP
                             else
                               LCP3 := LCP ;
                             LCP4 := LCP ;
                             with LCP -> do
                               begin
                                 NAME := ID ;
                                 IDTYPE := NIL ;
                                 KLASS := VARS ;
                                 VKIND := LKIND ;
                                 NEXT := NIL ;
                                 VLEV := LEVEL ;
                               end (* with *) ;
                             ENTERID ( LCP ) ;
                             INSYMBOL ;
                           end (* then *) ;
                         if not ( SY in [ COMMA , COLON ] + FSYS ) then
                           begin
                             ERROR ( 7 ) ;
                             SKIP ( FSYS + [ COMMA , SEMICOLON ,
                                    RPARENT ] )
                           end (* then *) ;
                         TEST := SY <> COMMA ;
                         if not TEST then
                           INSYMBOL
                       until TEST ;
                       if SY = COLON then
                         begin
                           INSYMBOL ;
                           if SY = IDENT then
                             begin
                               SEARCHID ( [ TYPES ] , LCP4 ) ;
                               LEN := PTRSIZE ;
                               LSP := LCP4 -> . IDTYPE ;
                               LALN := PTRSIZE ;
                               if LSP <> NIL then
                                 if ( LKIND = ACTUAL ) then
                                   if LSP -> . FORM = FILES then
                                     begin
                                       ERROR ( 121 ) ;
                                       LKIND := FORMAL
                                     end (* then *)
                                   else
                                     begin
                                       LEN := LSP -> . SIZE ;
                                       LALN := LSP -> . ALN
                                     end (* else *) ;
                               LCP4 := LCP3 ;
                               while LCP4 <> NIL do
                                 begin
                                   with LCP4 -> do
                                     begin
                                       IDTYPE := LSP ;
                                       ALIGN ( LC , LALN ) ;
                                       VADDR := LC ;
                                       LC := LC + LEN ;
                                     end (* with *) ;
                                   LCP4 := LCP4 -> . NEXT
                                 end (* while *) ;
                               INSYMBOL
                             end (* then *)
                           else
                             ERROR ( 2 ) ;
                           if not ( SY in FSYS + [ SEMICOLON , RPARENT
                           ] ) then
                             begin
                               ERROR ( 7 ) ;
                               SKIP ( FSYS + [ SEMICOLON , RPARENT ] )
                             end (* then *)
                         end (* then *)
                       else
                         ERROR ( 5 ) ;
                     end (* else *) ;
                   if SY = SEMICOLON then
                     begin
                       INSYMBOL ;
                       if not ( SY in FSYS + [ IDENT , VARSY , PROCSY ,
                       FUNCSY ] ) then
                         begin
                           ERROR ( 7 ) ;
                           SKIP ( FSYS + [ IDENT , RPARENT ] )
                         end (* then *)
                     end (* then *) ;
                   if LCP1 <> NIL then
                     LCP2 -> . NEXT := LCP3
                   else
                     LCP1 := LCP3 ;
                   LCP2 := LCP ;
                 end (* while *) ;
               if SY = RPARENT then
                 begin
                   INSYMBOL ;
                   if not ( SY in FSY + FSYS ) then
                     begin
                       ERROR ( 6 ) ;
                       SKIP ( FSY + FSYS )
                     end (* then *)
                 end (* then *)
               else
                 ERROR ( 4 ) ;
             end (* then *)
           else

           (********************)
           (* IF SY <> LPARENT *)
           (********************)

             ;
           if not FW then
             FPAR -> . PRMPTR := LCP1 ;
           if FPAR -> . KLASS = FUNC then
             if SY = COLON then
               begin
                 INSYMBOL ;
                 if SY = IDENT then
                   begin
                     if FW then
                       ERROR ( 122 ) ;
                     SEARCHID ( [ TYPES ] , LCP1 ) ;
                     LSP := LCP1 -> . IDTYPE ;
                     FPAR -> . IDTYPE := LSP ;
                     if LSP <> NIL then
                       if LSP -> . FORM >= POWER then
                         begin
                           ERROR ( 120 ) ;
                           FPAR -> . IDTYPE := NIL
                         end (* then *) ;
                     INSYMBOL
                   end (* then *)
                 else
                   begin
                     ERROR ( 2 ) ;
                     SKIP ( FSYS + [ SEMICOLON ] )
                   end (* else *)
               end (* then *)
             else
               if not FW then
                 ERROR ( 123 )
         end (* PARAMETERLIST *) ;


      begin (* PROCDECLARATION *)
        LLC := LC ;
        LC := LCAFTMST + FPSAVEAREA ;

        (***************************)
        (* ADR. OF 1ST VAR OR PARM *)
        (***************************)

        LCM := LCAFTMST ;
        LCP := UPRCPTR ;

        (*******************************)
        (* TO INITIALIZE LCP IN CASE ! *)
        (*******************************)

        if SY = IDENT then
          begin

        (**************************************)
        (* SEE IF PROC. ON FORWARD DECL. LIST *)
        (**************************************)

            FORW := FALSE ;
            LCP := FWRDPRCL ;
            LCP2 := NIL ;
            while LCP <> NIL do
              if LCP -> . NAME = ID then
                begin
                  FORW := TRUE ;
                  if LCP2 <> NIL then
                    LCP2 -> . NXTFWRD := LCP -> . NXTFWRD
                  else
                    FWRDPRCL := LCP -> . NXTFWRD ;
                  goto 10 ;
                end (* then *)
              else
                begin
                  LCP2 := LCP ;
                  LCP := LCP -> . NXTFWRD
                end (* else *) ;
            10 :
            if not FORW then
              begin
                if FSY = PROCSY then
                  NEW ( LCP , PROC , DECLARED )
                else
                  NEW ( LCP , FUNC , DECLARED ) ;
                with LCP -> do
                  begin
                    NAME := ID ;
                    IDTYPE := NIL ;
                    PFLEV := LEVEL ;
                    PROCLAB := PROCLAB + 1 ;
                    PFDECKIND := DECLARED ;
                    PFKIND := ACTUAL ;
                    PFNAME := PROCLAB ;
                    MKNAME ( ID , PFNAME , XLINK ) ;
                    EXTRN := XLINK ;
                    FRTRN := FALSE ;
                    FWDECL := FALSE ;
                    PACK ( ID , 1 , EXTNAME ) ;
                    if FSY = PROCSY then
                      KLASS := PROC
                    else
                      KLASS := FUNC
                  end (* with *) ;
                ENTERID ( LCP ) ;
                OLD_HASH := BUCKET ;

        (*************************)
        (* NEW SCOPE BEGINS NEXT *)
        (*************************)

              end (* then *)
            else
              begin
                LCP1 := LCP -> . PRMPTR ;
                LCP -> . FWDECL := FALSE ;
                OLD_HASH := BUCKET ;

        (************************)
        (* NEW SCOPE BEGINS NOW *)
        (************************)

                LEVEL := LEVEL + 1 ;
                TOP := LEVEL ;
                while LCP1 <> NIL do
                  begin
                    ENTERID ( LCP1 ) ;

        (*******************************)
        (* NAME NEEDS TO BE RE-ENTERED *)
        (*******************************)

                    with LCP1 -> do
                      if KLASS = VARS then
                        if IDTYPE <> NIL then
                          begin
                            if VKIND = FORMAL then
                              LCM := VADDR + PTRSIZE
                            else
                              LCM := VADDR + IDTYPE -> . SIZE ;
                            if LCM > LC then
                              LC := LCM
                          end (* then *) ;
                    LCP1 := LCP1 -> . NEXT
                  end (* while *) ;
                LEVEL := LEVEL - 1 ;
                TOP := LEVEL
              end (* else *) ;
            INSYMBOL
          end (* then *)
        else
          ERROR ( 2 ) ;
        OLDLABEL := INTLABEL ;
        INTLABEL := 0 ;
        if LEVEL < MAXLEVEL then
          LEVEL := LEVEL + 1
        else
          FATALERROR ( 251 ) ;
        TOP := LEVEL ;
        with DISPLAY [ TOP ] do
          begin
            OCCUR := BLCK ;
            FLABEL := NIL
          end (* with *) ;
        if GET_STAT then
          PROC_CNT [ LEVEL ] := PROC_CNT [ LEVEL ] + 1 ;
        if FSY = PROCSY then
          PARAMETERLIST ( [ SEMICOLON ] , LCP , FORW )
        else
          PARAMETERLIST ( [ SEMICOLON , COLON ] , LCP , FORW ) ;
        LCP -> . FWDECL := FALSE ;
        if SY = SEMICOLON then
          INSYMBOL
        else
          ERROR ( 14 ) ;
        if SY in [ FORWARDSY , FRTRNSY , EXTRNSY ] then
          begin
            if SY = FORWARDSY then
              begin
                if FORW then
                  ERROR ( 161 ) ;
                LCP -> . FWDECL := TRUE ;
                LCP -> . NXTFWRD := FWRDPRCL ;

        (*******************)
        (* LINK PROC. INTO *)
        (*******************)

                FWRDPRCL := LCP ;

        (**********************)
        (* FORWARD PROC. LIST *)
        (**********************)

                INSYMBOL
              end (* then *)
            else
              begin

        (*********************************)
        (* SY MUST BE FRTRNSY OR EXTRNSY *)
        (*********************************)

                if SY = FRTRNSY then
                  LCP -> . FRTRN := TRUE
                else
                  LCP -> . EXTRN := TRUE ;
                INSYMBOL ;
                with LCP -> do
                  if SY = STRINGCONST then
                    with VAL . VALP -> do
                      begin
                        while LNGTH < EXTNAMSZ do
                          begin
                            LNGTH := LNGTH + 1 ;
                            SVAL [ I ] := ' '
                          end (* while *) ;
                        PACK ( SVAL , 1 , EXTNAME ) ;
                        INSYMBOL
                      end (* with *)
                  else
                    PACK ( NAME , 1 , EXTNAME ) ;
              end (* else *) ;
            if SY = SEMICOLON then
              INSYMBOL
            else
              ERROR ( 14 ) ;
            if not ( SY in FSYS ) then
              begin
                ERROR ( 6 ) ;
                SKIP ( FSYS )
              end (* then *)
          end (* then *)
        else
          begin
            if DEBUG_LEV > 0 then
              PRNTSYMBL ( LCP ) ;
            MARK ( MARKP ) ;

        (*****************************)
        (* MARK HEAP FOR BLOCK ENTRY *)
        (*****************************)

            repeat
              BLOCK ( FSYS , SEMICOLON , LCP ) ;
              if SY = SEMICOLON then
                begin
                  INSYMBOL ;
                  if not ( SY in [ BEGINSY , PROCSY , FUNCSY ] ) then
                    begin
                      ERROR ( 6 ) ;
                      SKIP ( FSYS )
                    end (* then *)
                end (* then *)
              else
                ERROR ( 14 )
            until SY in [ BEGINSY , PROCSY , FUNCSY ] ;
            RELEASE ( MARKP ) ;

        (****************************************)
        (* RETURN LOCAL ENTRIES ON RUNTIME HEAP *)
        (****************************************)

          end (* else *) ;
        LEVEL := LEVEL - 1 ;
        TOP := LEVEL ;
        LC := LLC ;
        INTLABEL := OLDLABEL ;
        BUCKET := OLD_HASH ;

        (**********************)
        (*RESTORE SYMBOL TABLE*)
        (**********************)

      end (* PROCDECLARATION *) ;


   function PROCTYPE ( FPROCP : CTP ) : INTEGER ;

      begin (* PROCTYPE *)
        PROCTYPE := ORD ( 'P' ) ;
        if FPROCP <> NIL then
          with FPROCP -> do
            begin
              if FRTRN then
                PROCTYPE := ORD ( 'F' ) ;
              if FPROCP -> . IDTYPE <> NIL then
                with FPROCP -> do
                  begin
                    if IDTYPE = REALPTR then
                      if FRTRN then
                        PROCTYPE := ORD ( 'Z' )
                      else
                        PROCTYPE := ORD ( 'R' )
                    else
                      if IDTYPE = BOOLPTR then
                        if FRTRN then
                          PROCTYPE := ORD ( 'X' )
                        else
                          PROCTYPE := ORD ( 'B' )
                      else
                        if IDTYPE -> . FORM = POINTER then
                          PROCTYPE := ORD ( 'A' )
                        else
                          if IDTYPE -> . SIZE = 1 then
                            PROCTYPE := ORD ( 'C' )
                          else
                            if FRTRN then
                              PROCTYPE := ORD ( 'Y' )
                            else
                              if IDTYPE -> . SIZE = HINTSIZE then
                                PROCTYPE := ORD ( 'H' )
                              else
                                PROCTYPE := ORD ( 'I' ) ;
                  end (* with *)
            end (* with *) ;
      end (* PROCTYPE *) ;


   procedure BODY ( FSYS : SETOFSYS ) ;

      const CIXMAX = 400 ;

      type OPRANGE = 0 .. OPMAX ;
           CALLED_PROC = record
                           NAME : ALPHA ;
                           LVL : LEVRANGE ;
                           CNT : 1 .. 100 ;
                           NXT : -> CALLED_PROC
                         end ;

      var CALL_HEAD , T2_CLIST , T_CLIST : -> CALLED_PROC ;
          LOCAL_CALL , MODIFYING : BOOLEAN ;

          (*********************************************)
          (* LOCAL_CALL = THIS PROC CALLS A LOCAL PROC *)
          (* MODIFYING = A PROGRAM VAR BEING MODIFIED  *)
          (*********************************************)

          VAR_REF , VAR_MOD : INTEGER ;

          (***************************************)
          (* # OF VARIABLES ACCESSED/REFERENCED  *)
          (***************************************)

          CNSTPTR : CSP ;
          I : INTEGER ;
          LCMAX , LLC1 : ADDRRANGE ;
          LCP , LLCP : CTP ;
          LLP : LBP ;
          FIRSTLN : INTEGER ;
          CTRNO : CTRRANGE ;
          LOOP0 : LOOPCTL ;
          SUBR : SUBRCTL ;
          LRETURN : LABELRNG ;


      procedure GEN0 ( FOP : OPRANGE ) ;

         begin (* GEN0 *)
           if PRCODE then
             begin
               PUTIC ;
               WRITELN ( PRR , MN [ FOP ] : 4 )
             end (* then *) ;
           IC := IC + 1
         end (* GEN0 *) ;


      procedure GEN1 ( FOP : OPRANGE ; FP2 : INTEGER ) ;

         var K : INTEGER ;

         begin (* GEN1 *)
           if PRCODE then
             begin
               PUTIC ;
               WRITE ( PRR , MN [ FOP ] : 4 ) ;
               if FOP = 30 then

           (********)
           (*CSP   *)
           (********)

                 WRITELN ( PRR , SNA [ FP2 ] : 4 )
               else
                 if FOP = 37 then

           (********)
           (*LCA   *)
           (********)

                   if FP2 = ORD ( 'P' ) then

           (**************************)
           (* LOAD PROCEDURE ADDRESS *)
           (**************************)

                     WRITELN ( PRR , ' P,' , ID : EXTNAMSZ )
                   else
                     if FP2 <> ORD ( 'S' ) then

           (********************)
           (* CHAR STRING OPND *)
           (********************)

                       begin
                         WRITE ( PRR , ' M,''' ) ;
                         with CNSTPTR -> do
                           begin
                             for K := 1 to SLNGTH do
                               begin
                                 WRITE ( PRR , SVAL [ K ] : 1 ) ;
                                 if SVAL [ K ] = '''' then
                                   WRITE ( PRR , '''' )
                               end (* for *) ;
                             STIC := STIC + SLNGTH ;
                           end (* with *) ;
                         WRITELN ( PRR , '''' )
                       end (* then *)
                     else

           (***************)
           (* SET OPERAND *)
           (***************)

                       begin
                         WRITE ( PRR , ' S,' ) ;
                         WRITESET ( CNSTPTR ) ;
                       end (* else *)
                 else
                   if ( FOP = 26 ) or ( FOP = 42 ) then

           (*********)
           (*STO,RET*)
           (*********)

                     WRITELN ( PRR , CHR ( FP2 ) : 2 )
                   else
                     WRITELN ( PRR , ' ' , FP2 : 1 )
             end (* then *) ;
           IC := IC + 1
         end (* GEN1 *) ;


      procedure GEN2 ( FOP : OPRANGE ; FP1 , FP2 : INTEGER ) ;

         var I , J , K : INTEGER ;

         begin (* GEN2 *)
           if PRCODE then
             begin
               PUTIC ;
               WRITE ( PRR , MN [ FOP ] : 4 , ' ' ) ;
               case FOP of
                 22 , 23 , 35 , 39 , 43 :


           (*********************)
           (*DEC,INC,IND,LDO,SRO*)
           (*********************)


                   WRITELN ( PRR , CHR ( FP1 ) , ',' , FP2 : 1 ) ;
                 29 , 41 , 50 , 58 , 68 , 69 :


           (*************************)
           (*MST,LDA,SCL,NEW,SLD,SMV*)
           (*************************)


                   WRITELN ( PRR , FP1 : 1 , ',' , FP2 : 1 ) ;
                 47 , 48 , 49 , 52 , 53 , 55 :


           (**********)
           (*EQU..NEQ*)
           (**********)


                   begin
                     WRITE ( PRR , CHR ( FP1 ) ) ;
                     if FP1 = ORD ( 'M' ) then
                       WRITE ( PRR , ',' , FP2 : 1 ) ;
                     WRITELN ( PRR )
                   end (* tag/ca *) ;
                 51 :

           (********)
           (*LDC   *)
           (********)


                      case FP1 of
                        0 : WRITELN ( PRR , 'C,''' , CHR ( FP2 ) : 1 ,
                                      '''' ) ;
                        1 : WRITELN ( PRR , 'I,' , FP2 : 1 ) ;
                        2 : begin
                              WRITE ( PRR , 'R,' ) ;
                              with CNSTPTR -> do
                                for K := 1 to REALLNGTH do
                                  if RVAL [ K ] <> ' ' then
                                    WRITE ( PRR , RVAL [ K ] ) ;
                              WRITELN ( PRR )
                            end (* tag/ca *) ;
                        3 : WRITELN ( PRR , 'B,' , FP2 : 1 ) ;
                        4 : WRITELN ( PRR , 'N' ) ;
                        otherwise
                          ERROR ( 400 ) ;
                      end (* case *)
               end (* case *) ;
             end (* then *) ;
           IC := IC + 1
         end (* GEN2 *) ;


      procedure GEN3 ( FOP : OPRANGE ; FP0 , FP1 , FP2 : INTEGER ) ;

         begin (* GEN3 *)
           if PRCODE then
             begin
               PUTIC ;
               if FOP = 76

           (********)
           (*PAK   *)
           (********)


               then
                 WRITELN ( PRR , MN [ FOP ] : 4 , ' ' , FP0 : 1 , ' ' ,
                           FP1 : 1 , ' ' , FP2 : 1 )
               else
                 WRITELN ( PRR , MN [ FOP ] : 4 , CHR ( FP0 ) : 2 , ','
                           , FP1 : 1 , ',' , FP2 : 1 )
             end (* then *) ;
           IC := IC + 1
         end (* GEN3 *) ;


      procedure LOAD ;

         begin (* LOAD *)
           with GATTR do
             if TYPTR <> NIL then
               begin
                 case KIND of
                   CST : if ( TYPTR -> . FORM = SCALAR ) and ( TYPTR <>
                         REALPTR ) then
                           if TYPTR = BOOLPTR then

           (********)
           (*LDC   *)
           (********)

                             GEN2 ( 51 , 3 , CVAL . IVAL )
                           else
                             if TYPTR = CHARPTR then

           (********)
           (*LDC   *)
           (********)

                               GEN2 ( 51 , 0 , CVAL . IVAL )
                             else

           (********)
           (*LDC   *)
           (********)

                               GEN2 ( 51 , 1 , CVAL . IVAL )

           (*********)
           (*INTEGER*)
           (*********)

                         else
                           if TYPTR = NILPTR then

           (********)
           (*LDC   *)
           (********)

                             GEN2 ( 51 , 4 , 0 )
                           else
                             begin
                               CNSTPTR := CVAL . VALP ;
                               if TYPTR = REALPTR then

           (********)
           (*LDC   *)
           (********)

                                 GEN2 ( 51 , 2 , 0 )
                               else

           (********)
           (*LDC   *)
           (********)

                                 GEN2 ( 51 , 5 , 0 )
                             end (* else *) ;
                   VARBL : case ACCESS of

           (********)
           (*LOD   *)
           (********)

                             DRCT : GEN3 ( 54 , GETTYPE ( BTYPE ) ,
                                           VLEVEL , DPLMT ) ;

           (********)
           (*IND   *)
           (********)

                             INDRCT :
                               GEN2 ( 35 , GETTYPE ( BTYPE ) , IDPLMT )
                                      ;
                             INXD , STKEXPR :
                               ERROR ( 400 )
                           end (* case *) ;
                   EXPR :
                 end (* case *) ;
                 if KIND = VARBL then
                   VAR_REF := VAR_REF + 1 ;
                 KIND := EXPR
               end (* then *)
         end (* LOAD *) ;


      procedure STORE ( var FATTR : ATTR ) ;

         begin (* STORE *)
           with FATTR do
             if TYPTR <> NIL then
               case ACCESS of

           (********)
           (*STR   *)
           (********)

                 DRCT : GEN3 ( 56 , GETTYPE ( BTYPE ) , VLEVEL , DPLMT
                               ) ;
                 INDRCT :
                   if IDPLMT <> 0 then
                     ERROR ( 400 )
                   else

           (********)
           (*STO   *)
           (********)

                     GEN1 ( 26 , GETTYPE ( BTYPE ) ) ;
                 INXD , STKEXPR :
                   ERROR ( 400 )
               end (* case *)
         end (* STORE *) ;


      procedure LOADADDRESS ;

         begin (* LOADADDRESS *)
           with GATTR do
             if TYPTR <> NIL then
               begin
                 case KIND of
                   CST : if STRING ( TYPTR ) then
                           begin
                             CNSTPTR := CVAL . VALP ;

           (********)
           (*LCA   *)
           (********)

                             GEN1 ( 37 , ORD ( 'M' ) ) ;
                           end (* then *)
                         else
                           if TYPTR -> . FORM = POWER then
                             begin
                               CNSTPTR := CVAL . VALP ;

           (********)
           (*LCA   *)
           (********)

                               GEN1 ( 37 , ORD ( 'S' ) )
                             end (* then *)
                           else
                             ERROR ( 400 ) ;
                   VARBL : case ACCESS of

           (********)
           (*LDA   *)
           (********)

                             DRCT : GEN2 ( 50 , VLEVEL , DPLMT ) ;
                             INDRCT :
                               if IDPLMT <> 0 then

           (********)
           (*INC   *)
           (********)

                                 GEN2 ( 23 , ORD ( 'A' ) , IDPLMT ) ;
                             INXD : ERROR ( 400 ) ;
                             STKEXPR :
                               ;

           (***************************************)
           (*SET ALREADY REPRESENTED BY AN ADDRESS*)
           (***************************************)

                           end (* case *) ;
                   EXPR : ERROR ( 400 )
                 end (* case *) ;
                 KIND := VARBL ;
                 ACCESS := INDRCT ;
                 IDPLMT := 0
               end (* then *)
         end (* LOADADDRESS *) ;


      procedure GENFJP ( FADDR : INTEGER ) ;

         begin (* GENFJP *)
           LOAD ;
           if GATTR . TYPTR <> NIL then
             if GATTR . TYPTR <> BOOLPTR then
               ERROR ( 144 ) ;
           if PRCODE then
             begin
               PUTIC ;
               WRITELN ( PRR , MN [ 33 ] : 4 , ' L' , FADDR : 1 )
             end (* then *) ;
           IC := IC + 1
         end (* GENFJP *) ;


      procedure GENUJPFJP ( FOP : OPRANGE ; FP2 : INTEGER ) ;

         begin (* GENUJPFJP *)
           if PRCODE then
             begin
               PUTIC ;
               WRITELN ( PRR , MN [ FOP ] : 4 , ' L' , FP2 : 1 )
             end (* then *) ;
           IC := IC + 1
         end (* GENUJPFJP *) ;


      procedure GENDEF ( L1 , L2 : ADDRRANGE ) ;

         begin (* GENDEF *)
           if PRCODE then

           (********)
           (*DEF   *)
           (********)

             WRITELN ( PRR , 'L' , L1 : 1 , MN [ 63 ] , L2 : 10 ) ;
         end (* GENDEF *) ;


      procedure CHKBNDS ( FSP : STP ) ;

         var LMIN , LMAX : INTEGER ;

         begin (* CHKBNDS *)
           if FSP <> NIL then
             if FSP <> BOOLPTR then
               if FSP <> INTPTR then
                 if FSP <> REALPTR then
                   if FSP -> . FORM <= POINTER then
                     if FSP -> . FORM = POINTER

           (**************)
           (*LMAX <= LMIN*)
           (**************)


                     then
                       begin
                         FLIPDEBUG := TRUE ;
                         if ASSIGN then

           (********)
           (*CHK   *)
           (********)

                           GEN3 ( 45 , ORD ( 'A' ) , - 1 , 0 )
                         else

           (**********)
           (* ACCESS *)
           (**********)

                           GEN3 ( 45 , ORD ( 'A' ) , 0 , 0 )

           (********)
           (*CHK   *)
           (********)

                       end (* then *)
                     else
                       begin
                         GETBOUNDS ( FSP , LMIN , LMAX ) ;

           (********)
           (*CHK   *)
           (********)

                         GEN3 ( 45 , ORD ( 'I' ) , LMIN , LMAX ) ;
                       end (* else *) ;
         end (* CHKBNDS *) ;


      procedure PUTLABEL ( LABNAME : INTEGER ) ;

         begin (* PUTLABEL *)
           if PRCODE then

           (********)
           (*LAB   *)
           (********)

             WRITELN ( PRR , 'L' , LABNAME : 1 , MN [ 64 ] )
         end (* PUTLABEL *) ;


      function CTRGEN : CTRRANGE ;

      (************************************)
      (* CREATE A UNIQUE STMT COUNTER     *)
      (* AND EMIT P-CODE TO INCREMENT IT  *)
      (* R. L. SITES  3 AUG 77            *)
      (************************************)


         begin (* CTRGEN *)
           CTRGEN := CTRCNT ;
           if CTROPTION then
             begin

           (********)
           (*CTI   *)
           (********)

               GEN1 ( 39 , CTRCNT ) ;
               CTRCNT := CTRCNT + 1 ;
             end (* then *) ;
         end (* CTRGEN *) ;

         (**********)
         (* CTRGEN *)
         (**********)



      procedure CTREMIT ( CTRT : CTRTYPE ; CTRNO : CTRRANGE ; FLN , MLN
                        , LLN : INTEGER ) ;

      (**************************************************)
      (* WRITE AN ENTRY DESCRIBING A STATEMENT COUNTER. *)
      (* R. L. SITES  3 AUG 77                          *)
      (**************************************************)


         begin (* CTREMIT *)
           if CTROPTION then
             WRITELN ( QRR , '#CTR    ' , ORD ( CTRT ) : 4 , CTRNO : 6
                       , FLN : 7 , MLN : 7 , LLN : 7 ) ;
         end (* CTREMIT *) ;


      procedure STATEMENT ( FSYS : SETOFSYS ; var LOOPC : LOOPCTL ; var
                          SUBR : SUBRCTL ) ;

         label 1 ;

         var LCP : CTP ;
             LLP : LBP ;
             TTOP : DISPRANGE ;
             XLABEL : ALPHA ;
             CTRNO : CTRRANGE ;


         procedure EXPRESSION ( FSYS : SETOFSYS ) ;

            FORWARD ;


         procedure FORCETEMPSET ;

         (**************************************)
         (* "LOADS" CURRENT SET ONTO RUN-STACK *)
         (**************************************)


            label 10 ;

            var LSIZE : ADDRRANGE ;

            begin (* FORCETEMPSET *)
              with GATTR do
                if TYPTR <> NIL then
                  if TYPTR -> . FORM = POWER

              (*****************)
              (*REDUNDANT TEST?*)
              (*****************)


                  then
                    begin
                      if KIND = VARBL then
                        if ACCESS = STKEXPR then
                          goto 10 ;
                      LSIZE := OPNDSETSIZE ( GATTR ) ;
                      ALIGN ( LC , WORDSIZE ) ;
                      LOADADDRESS ;

              (********)
              (*SLD   *)
              (********)

                      GEN2 ( 68 , LSIZE , LC ) ;
                      KIND := VARBL ;
                      ACCESS := STKEXPR ;
                      STKLEN := LSIZE ;
                      STKDPLMT := LC ;
                      LC := LC + LSIZE ;
                      if LC > LCMAX then
                        LCMAX := LC ;
                    end (* then *) ;
              10 :

            end (* FORCETEMPSET *) ;


         procedure SELECTOR ( FSYS : SETOFSYS ; FCP : CTP ) ;

            var LATTR : ATTR ;
                LCP : CTP ;
                LMIN , LMAX : INTEGER ;

            begin (* SELECTOR *)
              with FCP -> , GATTR do
                begin
                  TYPTR := IDTYPE ;
                  BTYPE := TYPTR ;
                  KIND := VARBL ;
                  case KLASS of
                    VARS : if VKIND = ACTUAL then
                             begin
                               ACCESS := DRCT ;
                               VLEVEL := VLEV ;
                               DPLMT := VADDR
                             end (* then *)
                           else
                             begin

              (********)
              (*LOD   *)
              (********)

                               GEN3 ( 54 , ORD ( 'A' ) , VLEV , VADDR )
                                      ;
                               ACCESS := INDRCT ;
                               IDPLMT := 0
                             end (* else *) ;
                    FIELD : with DISPLAY [ DISX ] do
                              if OCCUR = CREC then
                                begin
                                  ACCESS := DRCT ;
                                  VLEVEL := CLEV ;
                                  DPLMT := CDSPL + FLDADDR
                                end (* then *)
                              else
                                begin

              (********)
              (*LOD   *)
              (********)

                                  GEN3 ( 54 , ORD ( 'A' ) , LEVEL ,
                                         VDSPL ) ;
                                  ACCESS := INDRCT ;
                                  IDPLMT := FLDADDR
                                end (* else *) ;
                    STRUCTKONST :
                      begin
                        ID := '########    ' ;
                        LMAX := EXTNAMSZ ;
                        UNPACK ( SKOWNER -> . EXTNAME , ID , 1 ) ;
                        if ( SKOWNER <> MAINPROG ) then
                          if not SKOWNER -> . EXTRN then
                            LMAX := 5 ;
                        repeat
                          ID [ LMAX ] := '#' ;
                          LMAX := LMAX - 1
                        until ID [ LMAX ] <> ' ' ;

              (********)
              (*LCA   *)
              (********)

                        GEN1 ( 37 , ORD ( 'P' ) ) ;
                        ACCESS := INDRCT ;
                        IDPLMT := SKADDR
                      end (* tag/ca *) ;
                    FUNC : if FCP <> UFCTPTR then
                             if PFDECKIND = STANDARD then
                               ERROR ( 150 )
                             else
                               if PFLEV = 0 then
                                 ERROR ( 150 )

              (**************)
              (*EXTERNAL FCT*)
              (**************)

                               else
                                 if PFKIND = FORMAL then
                                   ERROR ( 151 )
                                 else
                                   if ( FPROCP <> FCP ) then
                                     ERROR ( 177 )
                                   else
                                     begin
                                       ACCESS := DRCT ;
                                       VLEVEL := PFLEV + 1 ;
                                       DPLMT := FNCRSLT ;

              (*****************************)
              (*RELAT. ADDR. OF FCT. RESULT*)
              (*****************************)

                                     end (* else *)
                  end (* case *) ;
                  if TYPTR <> NIL then
                    if TYPTR -> . FORM = SUBRANGE then
                      TYPTR := TYPTR -> . RANGETYPE ;
                end (* with *) ;
              if not ( SY in SELECTSYS + FSYS ) then
                begin
                  ERROR ( 59 ) ;
                  SKIP ( SELECTSYS + FSYS )
                end (* then *) ;
              while SY in SELECTSYS do
                begin
                  if SY = LPARENT then

              (****************************)
              (* THIS IS AN ERROR, BUT .. *)
              (****************************)

                    begin
                      SY := LBRACK ;
                      if GATTR . TYPTR <> NIL then
                        if GATTR . TYPTR -> . FORM = ARRAYS then
                          begin
                            ERRKIND := 'W' ;
                            ERROR ( 11 )
                          end (* then *)
                    end (* then *) ;

              (****************)
              (* LEFT BRACKET *)
              (****************)

                  if SY = LBRACK then
                    begin
                      repeat
                        LATTR := GATTR ;
                        with LATTR do
                          if TYPTR <> NIL then
                            if TYPTR -> . FORM <> ARRAYS then
                              begin
                                ERROR ( 138 ) ;
                                TYPTR := NIL
                              end (* then *) ;
                        LOADADDRESS ;
                        INSYMBOL ;
                        EXPRESSION ( FSYS + [ COMMA , RBRACK , RPARENT
                                     ] ) ;
                        LOAD ;
                        if GATTR . TYPTR <> NIL then
                          if GATTR . TYPTR -> . FORM <> SCALAR then
                            ERROR ( 113 ) ;
                        if LATTR . TYPTR <> NIL then
                          with LATTR . TYPTR -> do
                            begin
                              if COMPTYPES ( INXTYPE , GATTR . TYPTR )
                              then
                                begin
                                  if INXTYPE <> NIL then
                                    begin
                                      GETBOUNDS ( INXTYPE , LMIN , LMAX
                                                  ) ;
                                      if DEBUG then

              (********)
              (*CHK   *)
              (********)

                                        GEN3 ( 45 , ORD ( 'J' ) , LMIN
                                               , LMAX ) ;
                                      if LMIN > 0 then

              (********)
              (*DEC   *)
              (********)

                                        GEN2 ( 22 , GETTYPE ( GATTR .
                                               TYPTR ) , LMIN )
                                      else
                                        if LMIN < 0 then

              (********)
              (*INC   *)
              (********)

                                          GEN2 ( 23 , GETTYPE ( GATTR .
                                                 TYPTR ) , - LMIN )

              (**************************)
              (*OR SIMPLY GEN1(31, LMIN)*)
              (**************************)

                                    end (* then *)
                                end (* then *)
                              else
                                ERROR ( 139 ) ;
                              with GATTR do
                                begin
                                  TYPTR := AELTYPE ;
                                  KIND := VARBL ;
                                  ACCESS := INDRCT ;
                                  IDPLMT := 0 ;
                                  if GATTR . TYPTR <> NIL then
                                    begin
                                      LMIN := TYPTR -> . SIZE ;
                                      ALIGN ( LMIN , TYPTR -> . ALN ) ;

              (********)
              (*IXA   *)
              (********)

                                      GEN1 ( 36 , LMIN )
                                    end (* then *) ;
                                end (* with *) ;
                            end (* with *)
                      until SY <> COMMA ;
                      if SY = RBRACK then
                        INSYMBOL
                      else
                        begin
                          if SY = RPARENT then
                            begin
                              ERRKIND := 'W' ;
                              INSYMBOL
                            end (* then *) ;
                          ERROR ( 12 )
                        end (* else *) ;
                    end (* then *)
                  else

              (**********)
              (* PERIOD *)
              (**********)

                    if SY = PERIOD then
                      begin
                        with GATTR do
                          begin
                            if TYPTR <> NIL then
                              if TYPTR -> . FORM <> RECORDS then
                                begin
                                  ERROR ( 140 ) ;
                                  TYPTR := NIL
                                end (* then *) ;
                            INSYMBOL ;
                            if SY = IDENT then
                              begin
                                if TYPTR <> NIL then
                                  begin
                                    SEARCHSECTION ( TYPTR , LCP ) ;
                                    if LCP = NIL then
                                      begin
                                        ERROR ( 152 ) ;
                                        TYPTR := NIL
                                      end (* then *)
                                    else
                                      with LCP -> do
                                        begin
                                          TYPTR := IDTYPE ;
                                          case ACCESS of
                                            DRCT : DPLMT := DPLMT +
                                                   FLDADDR ;
                                            INDRCT :
                                              IDPLMT := IDPLMT +
                                                   FLDADDR ;
                                            INXD , STKEXPR :
                                              ERROR ( 400 )
                                          end (* case *)
                                        end (* with *)
                                  end (* then *) ;
                                INSYMBOL
                              end (* then *)
                            else
                              ERROR ( 2 )
                          end (* with *)
                      end (* then *)
                    else

              (******************)
              (* POINTER SYMBOL *)
              (******************)

                      begin
                        if GATTR . TYPTR <> NIL then
                          with GATTR , TYPTR -> do
                            if FORM = POINTER then
                              begin
                                LOAD ;
                                if DEBUG then
                                  CHKBNDS ( GATTR . TYPTR ) ;
                                TYPTR := ELTYPE ;
                                with GATTR do
                                  begin
                                    KIND := VARBL ;
                                    ACCESS := INDRCT ;
                                    IDPLMT := 0
                                  end (* with *)
                              end (* then *)
                            else
                              if FORM = FILES then
                                begin
                                  TYPTR := FILTYPE ;
                                  case ACCESS of
                                    DRCT : DPLMT := DPLMT + FILHDRSIZE
                                                   ;
                                    INDRCT :
                                      IDPLMT := IDPLMT + FILHDRSIZE ;
                                    INXD , STKEXPR :
                                      ERROR ( 400 )
                                  end (* case *)
                                end (* then *)
                              else
                                ERROR ( 141 ) ;
                        INSYMBOL
                      end (* else *) ;
                  if not ( SY in FSYS + SELECTSYS ) then
                    begin
                      ERROR ( 6 ) ;
                      SKIP ( FSYS + SELECTSYS )
                    end (* then *) ;
                  GATTR . BTYPE := GATTR . TYPTR ;
                  if GATTR . TYPTR <> NIL then
                    if GATTR . TYPTR -> . FORM = SUBRANGE then
                      GATTR . TYPTR := GATTR . TYPTR -> . RANGETYPE ;
                end (* while *) ;
            end (* SELECTOR *) ;


         procedure CALL ( FSYS : SETOFSYS ; FCP : CTP ) ;

            var LKEY : 0 .. NSPROC ;
                MATCHPAR : BOOLEAN ;
                RWFILE : STP ;


            procedure VARIABLE ( FSYS : SETOFSYS ) ;

               var LCP : CTP ;

               begin (* VARIABLE *)
                 if SY = IDENT then
                   begin
                     SEARCHID ( [ VARS , FIELD ] , LCP ) ;
                     INSYMBOL
                   end (* then *)
                 else
                   begin
                     ERROR ( 2 ) ;
                     LCP := UVARPTR
                   end (* else *) ;
                 SELECTOR ( FSYS , LCP )
               end (* VARIABLE *) ;


            procedure RWSETUP ( DFILE : CTP ) ;

            (***************************************************)
            (* TO SET UP FILE ADDRESS PARAMETER FOR READ/WRITE *)
            (***************************************************)


               var LCP : CTP ;
                   SAVED : BOOLEAN ;
                   TEMPID : ALPHA ;
                   TEMPSY : SYMBOL ;

               begin (* RWSETUP *)
                 SAVED := TRUE ;
                 RWFILE := NIL ;
                 if MATCHPAR then

                 (*************************************)
                 (* OTHERWISE THERE ARE NO PARAMETERS *)
                 (*************************************)

                   if SY = IDENT then
                     begin
                       SEARCHID ( [ VARS , FIELD , FUNC , KONST ,
                                  STRUCTKONST ] , LCP ) ;
                       if LCP -> . IDTYPE <> NIL then
                         with LCP -> . IDTYPE -> do
                           if FORM = FILES then
                             SAVED := FALSE ;
                     end (* then *) ;
                 if SAVED then

                 (*************************)
                 (* USE IMPLIED FILE NAME *)
                 (*************************)

                   begin
                     TEMPSY := SY ;
                     TEMPID := ID ;
                     SY := COMMA ;
                     LCP := DFILE ;
                   end (* then *)
                 else
                   INSYMBOL ;
                 SELECTOR ( FSYS + [ COMMA , RPARENT ] , LCP ) ;
                 with GATTR do
                   if not COMPTYPES ( TYPTR , TEXTPTR ) then
                     if TYPTR <> NIL then
                       if TYPTR -> . FORM <> FILES then
                         ERROR ( 116 )
                       else
                         begin
                           RWFILE := TYPTR -> . FILTYPE ;
                           if not ( LKEY in [ 1 .. 6 , 25 , 36 , 37 ] )
                           then
                             ERROR ( 116 ) ;

                 (**********************************************)
                 (*   NON-TEXT FILES PERMITTED ONLY FOR:       *)
                 (*   GET, PUT, RESET, READ, WRITE,            *)
                 (*   REWRITE, EOF, SKIP, LINELIMIT            *)
                 (**********************************************)

                         end (* else *) ;
                 LOADADDRESS ;

                 (****************)
                 (* GET FILE ADR *)
                 (****************)

                 GEN1 ( 30 , 31 ) ;

                 (************)
                 (* CSP, SIO *)
                 (************)

                 if SAVED then
                   begin
                     ID := TEMPID ;
                     SY := TEMPSY
                   end (* then *) ;
               end (* RWSETUP *) ;


            procedure GETPUTRESETREWRITE ;

               begin (* GETPUTRESETREWRITE *)
                 if ODD ( LKEY ) then
                   RWSETUP ( INPUTPTR )

                 (**************)
                 (* GET, RESET *)
                 (**************)

                 else
                   RWSETUP ( OUTPUTPTR ) ;

                 (**********************)
                 (* PUT, REWRITE, PAGE *)
                 (**********************)

                 GEN1 ( 30 , LKEY ) ;

                 (*****************************)
                 (* CSP - GET,PUT,RES,REW,PAG *)
                 (*****************************)

                 GEN1 ( 30 , 32 ) ;

                 (*************)
                 (* CSP - EIO *)
                 (*************)

               end (* GETPUTRESETREWRITE *) ;


            procedure READ1 ;

               var CSPNO : 0 .. NSPROC ;
                   TEST : BOOLEAN ;

               begin (* READ1 *)
                 RWSETUP ( INPUTPTR ) ;
                 if RWFILE <> NIL then
                   if LKEY = 11 then
                     ERROR ( 116 ) ;
                 if MATCHPAR then

                 (*************************************)
                 (* OTHERWISE THERE ARE NO PARAMETERS *)
                 (*************************************)

                   begin
                     if SY = COMMA then
                       INSYMBOL ;
                     if LKEY = 5 then

                 (********)
                 (*READ  *)
                 (********)

                       if SY <> IDENT then
                         ERROR ( 2 ) ;
                     TEST := FALSE ;
                     if SY = IDENT then
                       repeat
                         VARIABLE ( FSYS + [ COMMA , RPARENT ] ) ;
                         LOADADDRESS ;
                         if GATTR . TYPTR <> NIL then
                           if RWFILE = NIL then
                             if STRING ( GATTR . TYPTR ) then
                               begin

                 (********)
                 (*LDC   *)
                 (********)

                                 GEN2 ( 51 , 1 , GATTR . TYPTR -> .
                                        SIZE DIV CHARSIZE ) ;
                                 CSPNO := 27

                 (********)
                 (*RDS   *)
                 (********)

                               end (* then *)
                             else
                               begin
                                 if GATTR . TYPTR = INTPTR then
                                   if GATTR . BTYPE -> . SIZE = INTSIZE
                                   then
                                     CSPNO := 24

                 (********)
                 (*RDI   *)
                 (********)

                                   else
                                     if GATTR . BTYPE -> . SIZE =
                                     HINTSIZE then
                                       CSPNO := 15

                 (********)
                 (*RDH   *)
                 (********)

                                     else
                                       CSPNO := 16

                 (*****************************)
                 (*RDY - ONE BYTE INTEGER READ*)
                 (*****************************)

                                 else
                                   if GATTR . TYPTR = REALPTR then
                                     CSPNO := 14

                 (********)
                 (*RDR   *)
                 (********)

                                   else
                                     if GATTR . TYPTR = CHARPTR then
                                       CSPNO := 5

                 (********)
                 (*RDC   *)
                 (********)

                                     else
                                       if GATTR . TYPTR = BOOLPTR then
                                         CSPNO := 12

                 (********)
                 (*RDB   *)
                 (********)

                                       else
                                         begin
                                           ERROR ( 116 ) ;
                                           CSPNO := 24
                                         end (* else *) ;
                               end (* else *)
                           else

                 (***********************)
                 (* NON-TEXT FILE INPUT *)
                 (***********************)

                             begin
                               if not COMPTYPES ( GATTR . TYPTR ,
                               RWFILE ) then
                                 ERROR ( 153 ) ;

                 (********)
                 (*LDC   *)
                 (********)

                               GEN2 ( 51 , 1 , GATTR . BTYPE -> . SIZE
                                      ) ;
                               CSPNO := 19

                 (********)
                 (*RDD   *)
                 (********)


                                        ;
                               EXTUSED := TRUE ;
                             end (* else *) ;

                 (********)
                 (*CSP   *)
                 (********)

                         GEN1 ( 30 , CSPNO ) ;
                         if SY = COMMA then
                           INSYMBOL
                         else
                           TEST := TRUE ;
                       until TEST ;
                   end (* then *) ;
                 if LKEY = 11 then

                 (*************)
                 (* CSP - RLN *)
                 (*************)

                   GEN1 ( 30 , 23 ) ;

                 (*************)
                 (* CSP - EIO *)
                 (*************)

                 GEN1 ( 30 , 32 ) ;
               end (* READ1 *) ;


            procedure WRITE1 ;

               var LSP : STP ;
                   DEFAULT , DEFAULT1 , TEST : BOOLEAN ;
                   CSPNO , LLKEY : 0 .. NSPROC ;
                   LEN : ADDRRANGE ;

               begin (* WRITE1 *)
                 LLKEY := LKEY ;
                 TEST := FALSE ;
                 RWSETUP ( OUTPUTPTR ) ;
                 if RWFILE <> NIL then
                   if LLKEY = 12 then
                     ERROR ( 116 ) ;
                 if MATCHPAR then

                 (***************************)
                 (* OTHERWISE NO PARAMETERS *)
                 (***************************)

                   begin
                     if SY = RPARENT then
                       if LLKEY = 6 then
                         ERROR ( 116 ) ;
                     if SY = COMMA then
                       begin
                         INSYMBOL ;
                         if not ( SY in SIMPTYPEBEGSYS ) then
                           ERROR ( 6 )
                       end (* then *) ;
                     if SY in SIMPTYPEBEGSYS then
                       repeat
                         EXPRESSION ( FSYS + [ COMMA , COLON , RPARENT
                                      ] ) ;
                         LSP := GATTR . TYPTR ;
                         if LSP <> NIL then
                           if LSP -> . FORM <= SUBRANGE then
                             LOAD
                           else
                             LOADADDRESS ;
                         if RWFILE = NIL then
                           begin
                             DEFAULT := TRUE ;
                             DEFAULT1 := TRUE ;
                             if SY = COLON then
                               begin
                                 INSYMBOL ;
                                 EXPRESSION ( FSYS + [ COMMA , COLON ,
                                              RPARENT ] ) ;
                                 LOAD ;
                                 if GATTR . TYPTR <> NIL then
                                   if GATTR . TYPTR <> INTPTR then
                                     ERROR ( 116 ) ;
                                 DEFAULT := FALSE ;
                                 if SY = COLON then
                                   begin
                                     INSYMBOL ;
                                     EXPRESSION ( FSYS + [ COMMA ,
                                                  RPARENT ] ) ;
                                     LOAD ;
                                     if GATTR . TYPTR <> NIL then
                                       if GATTR . TYPTR <> INTPTR then
                                         ERROR ( 116 ) ;
                                     if LSP <> REALPTR then
                                       ERROR ( 124 ) ;
                                     DEFAULT1 := FALSE ;
                                   end (* then *) ;
                               end (* then *) ;
                             if LSP = INTPTR then
                               begin
                                 if DEFAULT then

                 (********)
                 (*LDC   *)
                 (********)

                                   GEN2 ( 51 , 1 , 12 ) ;
                                 CSPNO := 6

                 (********)
                 (*WRI   *)
                 (********)

                               end (* then *)
                             else
                               if LSP = REALPTR then
                                 begin
                                   if DEFAULT then

                 (********)
                 (*LDC   *)
                 (********)

                                     GEN2 ( 51 , 1 , 14 ) ;
                                   if DEFAULT1 then

                 (********)
                 (*LDC   *)
                 (********)

                                     GEN2 ( 51 , 1 , 0 ) ;
                                   CSPNO := 8

                 (********)
                 (*WRR   *)
                 (********)

                                 end (* then *)
                               else
                                 if LSP = CHARPTR then
                                   begin
                                     if DEFAULT then

                 (********)
                 (*LDC   *)
                 (********)

                                       GEN2 ( 51 , 1 , 1 ) ;
                                     CSPNO := 9

                 (********)
                 (*WRC   *)
                 (********)

                                   end (* then *)
                                 else
                                   if LSP = BOOLPTR then
                                     begin
                                       if DEFAULT then

                 (********)
                 (*LDC   *)
                 (********)

                                         GEN2 ( 51 , 1 , 5 ) ;
                                       CSPNO := 13

                 (********)
                 (*WRB   *)
                 (********)

                                     end (* then *)
                                   else
                                     if LSP <> NIL then
                                       begin
                                         if LSP -> . FORM = SCALAR then
                                           ERROR ( 398 )
                                         else
                                           if STRING ( LSP ) then
                                             begin
                                               LEN := LSP -> . SIZE DIV
                                                   CHARSIZE ;
                                               if DEFAULT then

                 (********)
                 (*LDC   *)
                 (********)

                                                 GEN2 ( 51 , 1 , LEN )
                                                   ;

                 (********)
                 (*LDC   *)
                 (********)

                                               GEN2 ( 51 , 1 , LEN ) ;
                                               CSPNO := 10

                 (********)
                 (*WRS   *)
                 (********)

                                             end (* then *)
                                           else
                                             begin
                                               ERROR ( 116 ) ;
                                               CSPNO := 6
                                             end (* else *)
                                       end (* then *)
                           end (* then *)
                         else

                 (*****************)
                 (* NON-TEXT FILE *)
                 (*****************)

                           begin
                             if not COMPTYPES ( LSP , RWFILE ) then
                               ERROR ( 145 ) ;

                 (********)
                 (*LDC   *)
                 (********)

                             GEN2 ( 51 , 1 , RWFILE -> . SIZE ) ;
                             EXTUSED := TRUE ;
                             if LSP <> NIL then
                               if LSP -> . FORM <= SUBRANGE then
                                 CSPNO := 7

                 (********)
                 (*WRE   *)
                 (********)

                               else
                                 CSPNO := 20

                 (********)
                 (*WRD   *)
                 (********)

                           end (* else *) ;

                 (********)
                 (*CSP   *)
                 (********)

                         GEN1 ( 30 , CSPNO ) ;
                         if SY = COMMA then
                           INSYMBOL
                         else
                           TEST := TRUE ;
                       until TEST ;
                   end (* then *) ;

                 (*********)
                 (*WRITELN*)
                 (*********)

                 if LLKEY = 12 then

                 (*************)
                 (* CSP - WLN *)
                 (*************)

                   GEN1 ( 30 , 22 ) ;

                 (*************)
                 (* CSP - EIO *)
                 (*************)

                 GEN1 ( 30 , 32 ) ;
               end (* WRITE1 *) ;


            procedure SKIPLIM ;

               begin (* SKIPLIM *)
                 RWSETUP ( OUTPUTPTR ) ;
                 if SY = COMMA then
                   begin
                     INSYMBOL ;
                     if not ( SY in SIMPTYPEBEGSYS ) then
                       ERROR ( 6 )
                   end (* then *) ;
                 if SY in SIMPTYPEBEGSYS then
                   begin
                     EXPRESSION ( FSYS + [ RPARENT ] ) ;
                     LOAD ;
                     if GATTR . TYPTR <> NIL then
                       if GATTR . TYPTR <> INTPTR then
                         ERROR ( 125 ) ;

                 (*****************)
                 (* CSP - SKP/LIM *)
                 (*****************)

                     GEN1 ( 30 , LKEY - 2 ) ;

                 (*************)
                 (* CSP - EIO *)
                 (*************)

                     GEN1 ( 30 , 32 ) ;
                   end (* then *)
               end (* SKIPLIM *) ;


            procedure MESSAGE1 ;

               var LEN : INTEGER ;

               begin (* MESSAGE1 *)
                 EXPRESSION ( FSYS + [ RPARENT ] ) ;
                 if GATTR . TYPTR <> NIL then
                   if STRING ( GATTR . TYPTR ) then
                     LEN := GATTR . TYPTR -> . SIZE DIV CHARSIZE
                   else
                     ERROR ( 125 ) ;
                 LOADADDRESS ;

                 (********)
                 (*LDC   *)
                 (********)

                 GEN2 ( 51 , 1 , LEN ) ;

                 (*************)
                 (* CSP - MSG *)
                 (*************)

                 GEN1 ( 30 , 33 ) ;
               end (* MESSAGE1 *) ;


            procedure PACK1 ;

               var LSP , LSP1 : STP ;
                   LSIZE , IMIN , IMAX : INTEGER ;
                   LCNT , RCNT , LELEMSIZE , RELEMSIZE : INTEGER ;

               begin (* PACK1 *)
                 EXPRESSION ( FSYS + [ COMMA , RPARENT ] ) ;
                 LSP := NIL ;
                 LSP1 := NIL ;
                 if GATTR . TYPTR <> NIL then
                   with GATTR , GATTR . TYPTR -> do
                     if FORM = ARRAYS then
                       begin
                         LSP := INXTYPE ;
                         LSP1 := AELTYPE ;
                         IMIN := 1 ;
                         LSIZE := SIZE ;
                         IMAX := LSIZE ;
                         if LSP <> NIL then
                           GETBOUNDS ( LSP , IMIN , IMAX ) ;
                         LCNT := IMAX - IMIN + 1 ;
                         LELEMSIZE := LSIZE DIV LCNT ;
                         LOADADDRESS ;
                       end (* then *)
                     else
                       ERROR ( 116 ) ;
                 if SY = COMMA then
                   INSYMBOL
                 else
                   ERROR ( 20 ) ;
                 EXPRESSION ( FSYS + [ COMMA , RPARENT ] ) ;
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM <> SCALAR then
                     ERROR ( 116 )
                   else
                     if not COMPTYPES ( LSP , GATTR . TYPTR ) then
                       ERROR ( 116 )
                     else
                       begin
                         LOAD ;
                         if DEBUG then

                 (********)
                 (*CHK   *)
                 (********)

                           GEN3 ( 45 , ORD ( 'J' ) , IMIN , IMAX ) ;
                         if IMIN > 0 then

                 (********)
                 (*DEC   *)
                 (********)

                           GEN2 ( 22 , GETTYPE ( GATTR . TYPTR ) , IMIN
                                  )
                         else
                           if IMIN < 0 then

                 (********)
                 (*INC   *)
                 (********)

                             GEN2 ( 23 , GETTYPE ( GATTR . TYPTR ) , -
                                    IMIN ) ;

                 (********)
                 (*IXA   *)
                 (********)

                         GEN1 ( 36 , LELEMSIZE ) ;
                       end (* else *) ;
                 if SY = COMMA then
                   INSYMBOL
                 else
                   ERROR ( 20 ) ;
                 VARIABLE ( FSYS + [ RPARENT ] ) ;
                 if GATTR . TYPTR <> NIL then
                   with GATTR . TYPTR -> do
                     if FORM = ARRAYS then
                       begin
                         if not COMPTYPES ( AELTYPE , LSP1 ) or not
                         COMPTYPES ( INXTYPE , LSP ) then
                           ERROR ( 116 )
                         else
                           begin
                             LOADADDRESS ;
                             LSP := INXTYPE ;
                             LSP1 := AELTYPE ;
                             if LSP <> NIL then
                               GETBOUNDS ( LSP , IMIN , IMAX )
                             else
                               begin
                                 IMIN := 1 ;
                                 IMAX := SIZE
                               end (* else *) ;
                             RCNT := IMAX - IMIN + 1 ;
                             RELEMSIZE := SIZE DIV RCNT ;
                             if RCNT > LCNT then
                               ERROR ( 303 ) ;
                             if LELEMSIZE = RELEMSIZE then

                 (*********************)
                 (* A MOVE WORK S OK  *)
                 (*********************)

                               GEN1 ( 40 , - SIZE )

                 (********)
                 (*MOV   *)
                 (********)

                             else

                 (********)
                 (*PAK   *)
                 (********)

                               GEN3 ( 76 , RCNT , LELEMSIZE , RELEMSIZE
                                      ) ;
                           end (* else *) ;
                       end (* then *)
                     else
                       ERROR ( 116 )
               end (* PACK1 *) ;


            procedure UNPACK1 ;

               var LSP , LSP1 : STP ;
                   IMIN , IMAX , LSIZE : INTEGER ;
                   LCNT , RCNT , LELEMSIZE , RELEMSIZE : INTEGER ;

               begin (* UNPACK1 *)
                 EXPRESSION ( FSYS + [ COMMA , RPARENT ] ) ;
                 LSP := NIL ;
                 LSP1 := NIL ;
                 if GATTR . TYPTR <> NIL then
                   with GATTR . TYPTR -> do
                     if FORM = ARRAYS then
                       begin
                         LSP := INXTYPE ;
                         LSP1 := AELTYPE ;
                         IMIN := 1 ;
                         LSIZE := SIZE ;
                         IMAX := LSIZE ;
                         if LSP <> NIL then
                           GETBOUNDS ( LSP , IMIN , IMAX ) ;
                         LCNT := IMAX - IMIN + 1 ;
                         LELEMSIZE := LSIZE DIV LCNT ;
                         LOADADDRESS ;
                       end (* then *)
                     else
                       ERROR ( 116 ) ;
                 if SY = COMMA then
                   INSYMBOL
                 else
                   ERROR ( 20 ) ;
                 VARIABLE ( FSYS + [ COMMA , RPARENT ] ) ;
                 if GATTR . TYPTR <> NIL then
                   with GATTR , GATTR . TYPTR -> do
                     if FORM = ARRAYS then
                       begin
                         if not COMPTYPES ( AELTYPE , LSP1 ) or not
                         COMPTYPES ( INXTYPE , LSP ) then
                           ERROR ( 116 )
                         else
                           begin
                             if INXTYPE <> NIL then
                               GETBOUNDS ( INXTYPE , IMIN , IMAX )
                             else
                               begin
                                 IMIN := 1 ;
                                 IMAX := SIZE
                               end (* else *) ;
                             RCNT := IMAX - IMIN + 1 ;
                             RELEMSIZE := SIZE DIV RCNT ;
                             if LCNT > RCNT then
                               ERROR ( 303 ) ;
                             LOADADDRESS ;
                           end (* else *) ;
                       end (* then *)
                     else
                       ERROR ( 116 ) ;
                 if SY = COMMA then
                   INSYMBOL
                 else
                   ERROR ( 20 ) ;
                 EXPRESSION ( FSYS + [ RPARENT ] ) ;
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM <> SCALAR then
                     ERROR ( 116 )
                   else
                     if not COMPTYPES ( LSP , GATTR . TYPTR ) then
                       ERROR ( 116 )
                     else
                       begin
                         LOAD ;
                         if DEBUG then

                 (********)
                 (*CHK   *)
                 (********)

                           GEN3 ( 45 , ORD ( 'J' ) , IMIN , IMAX ) ;
                         if IMIN > 0 then

                 (********)
                 (*DEC   *)
                 (********)

                           GEN2 ( 22 , GETTYPE ( GATTR . TYPTR ) , IMIN
                                  )
                         else
                           if IMIN < 0 then

                 (********)
                 (*INC   *)
                 (********)

                             GEN2 ( 23 , GETTYPE ( GATTR . TYPTR ) , -
                                    IMIN ) ;

                 (********)
                 (*IXA   *)
                 (********)

                         GEN1 ( 36 , RELEMSIZE ) ;
                         if LELEMSIZE = RELEMSIZE then

                 (*****************)
                 (* A MOVE IS OK  *)
                 (*****************)

                           GEN1 ( 40 , - LSIZE )

                 (********)
                 (*MOV   *)
                 (********)

                         else

                 (********)
                 (*PAK   *)
                 (********)

                           GEN3 ( 76 , LCNT , LELEMSIZE , RELEMSIZE ) ;
                       end (* else *) ;
               end (* UNPACK1 *) ;


            procedure NEW1 ;

               label 1 ;

               var LSP , LSP1 : STP ;
                   VARTS , LMIN , LMAX : INTEGER ;
                   LSIZE , LSZ : ADDRRANGE ;
                   LVAL : VALU ;
                   LALN : ALNRNG ;

               begin (* NEW1 *)
                 VARIABLE ( FSYS + [ COMMA , RPARENT ] ) ;
                 LOADADDRESS ;
                 LSP := NIL ;
                 VARTS := 0 ;
                 LSIZE := 0 ;
                 LALN := INTSIZE ;
                 if GATTR . TYPTR <> NIL then
                   with GATTR . TYPTR -> do
                     if FORM = POINTER then
                       begin
                         if ELTYPE <> NIL then
                           begin
                             LSIZE := ELTYPE -> . SIZE ;
                             if ELTYPE -> . ALN > INTSIZE then
                               LALN := REALSIZE ;
                             if ELTYPE -> . FORM = RECORDS then
                               LSP := ELTYPE -> . RECVAR
                           end (* then *)
                       end (* then *)
                     else
                       ERROR ( 116 ) ;
                 while SY = COMMA do
                   begin
                     INSYMBOL ;
                     CONSTANT ( FSYS + [ COMMA , RPARENT ] , LSP1 ,
                                LVAL ) ;
                     VARTS := VARTS + 1 ;

                 (*****************************************)
                 (*   CHECK TO INSERT HERE:               *)
                 (*   IS CONSTANT IN TAGFIELDTYPE RANGE   *)
                 (*****************************************)

                     if LSP = NIL then
                       ERROR ( 158 )
                     else
                       if LSP -> . FORM <> TAGFLD then
                         ERROR ( 162 )
                       else
                         if LSP -> . TAGFIELDP <> NIL then
                           if STRING ( LSP1 ) or ( LSP1 = REALPTR )
                           then
                             ERROR ( 159 )
                           else
                             if COMPTYPES ( LSP -> . TAGFIELDP -> .
                             IDTYPE , LSP1 ) then
                               begin
                                 LSP1 := LSP -> . FSTVAR ;
                                 while LSP1 <> NIL do
                                   with LSP1 -> do
                                     if VARVAL . IVAL = LVAL . IVAL
                                     then
                                       begin
                                         LSIZE := SIZE ;
                                         LSP := SUBVAR ;
                                         goto 1
                                       end (* then *)
                                     else
                                       LSP1 := NXTVAR ;
                                 LSIZE := LSP -> . SIZE ;
                                 LSP := NIL ;
                               end (* then *)
                             else
                               ERROR ( 116 ) ;
                     1 :

                   end (* while *) ;
                 ALIGN ( LSIZE , INTSIZE ) ;

                 (********)
                 (*NEW   *)
                 (********)

                 GEN2 ( 58 , LSIZE , LALN ) ;
               end (* NEW1 *) ;


            procedure MARKRELEASE ;

               begin (* MARKRELEASE *)
                 VARIABLE ( FSYS + [ RPARENT ] ) ;
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM = POINTER then
                     if LKEY = 13

                 (********)
                 (*MARK  *)
                 (********)


                     then
                       begin
                         LOADADDRESS ;

                 (********)
                 (*SAV   *)
                 (********)

                         GEN0 ( 59 )
                       end (* then *)
                     else

                 (**********************)
                 (* LKEY = 10, RELEASE *)
                 (**********************)

                       begin
                         LOAD ;

                 (********)
                 (*RST   *)
                 (********)

                         GEN0 ( 60 )
                       end (* else *)
                   else
                     ERROR ( 125 )
               end (* MARKRELEASE *) ;


            procedure TRAPEXIT ;

            (**********************************************************)
            (*   THIS PROCEDURE IS TO SIMPLIFY COMMUNICATION          *)
            (*   WITH THE OUTSIDE WORLD AND PROVIDE BREAK POINTS      *)
            (*   IN THE PASCAL PROGRAM.                               *)
            (*   'TRAP(I, R)'  RETURNS THE INTEGER CONSTANT I         *)
            (*   AS WELL AS A POINTER TO THE SECOND PARAMETER 'R'     *)
            (*   (I.E. ADDRESS OF R) TO THE OPERATING SYSTEM.         *)
            (*   THE FIRST PARAMETER IS INTENDED TO BE USED AS A      *)
            (*   'FUNCTION NUMBER' AND THE SECOND ONE AS THE 'VAR'    *)
            (*   TYPE ARGUMENT WHICH MAY BE INSPECTED AND MODIFIED,   *)
            (*   TO THAT FUNCTION                                     *)
            (**********************************************************)


               var LLC : ADDRRANGE ;

               begin (* TRAPEXIT *)
                 LLC := LC ;

                 (********************************)
                 (* IN CASE OF SET TYPE ARGUMENT *)
                 (********************************)

                 if GATTR . TYPTR <> INTPTR then
                   ERROR ( 116 ) ;
                 if LKEY = 14 then

                 (********)
                 (*TRAP  *)
                 (********)

                   begin
                     if SY <> COMMA then
                       ERROR ( 6 )
                     else
                       begin
                         INSYMBOL ;
                         EXPRESSION ( FSYS + [ RPARENT ] ) ;
                         with GATTR do
                           if TYPTR <> NIL then
                             begin
                               if KIND <> VARBL then
                                 if TYPTR -> . FORM < POWER then
                                   begin
                                     LOAD ;
                                     KIND := VARBL ;
                                     ACCESS := DRCT ;
                                     VLEVEL := LEVEL ;
                                     ALIGN ( LC , MXDATASZE ) ;
                                     DPLMT := LC ;
                                     BTYPE := TYPTR ;
                                     STORE ( GATTR ) ;
                                   end (* then *) ;
                               LOADADDRESS ;
                             end (* then *) ;
                       end (* else *) ;
                   end (* then *) ;

                 (*****************)
                 (* CSP - TRP/XIT *)
                 (*****************)

                 GEN1 ( 30 , LKEY + 14 ) ;
                 LC := LLC ;
               end (* TRAPEXIT *) ;


            procedure SQRABS ;

               var OP : OPRANGE ;

               begin (* SQRABS *)
                 OP := 0 ;

                 (********)
                 (*ABI   *)
                 (********)

                 if LKEY = 17 then
                   OP := 24 ;

                 (********)
                 (*SQI   *)
                 (********)

                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR = INTPTR then

                 (********)
                 (*ABI   *)
                 (*SQI   *)
                 (********)

                     GEN0 ( OP )
                   else
                     if GATTR . TYPTR = REALPTR then

                 (********)
                 (*ABR   *)
                 (*ABR   *)
                 (********)

                       GEN0 ( OP + 1 )
                     else
                       begin
                         ERROR ( 125 ) ;
                         GATTR . TYPTR := INTPTR
                       end (* else *)
               end (* SQRABS *) ;


            procedure TRUNCROUND ;

               begin (* TRUNCROUND *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> REALPTR then
                     ERROR ( 125 ) ;

                 (********)
                 (*TRC   *)
                 (*RND   *)
                 (********)

                 GEN0 ( LKEY - 18 + 27 ) ;
                 GATTR . TYPTR := INTPTR
               end (* TRUNCROUND *) ;


            procedure EXPO1 ;

               begin (* EXPO1 *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR = INTPTR then

                 (********)
                 (*FLT   *)
                 (********)

                     GEN0 ( 10 )
                   else
                     if GATTR . TYPTR <> REALPTR then
                       ERROR ( 125 ) ;

                 (********)
                 (*XPO   *)
                 (********)

                 GEN0 ( 66 ) ;
                 GATTR . TYPTR := INTPTR ;
               end (* EXPO1 *) ;


            procedure CARD1 ;

               var LLC , LEN : ADDRRANGE ;

               begin (* CARD1 *)
                 LLC := LC ;
                 EXPRESSION ( FSYS + [ RPARENT ] ) ;
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM = POWER then
                     begin
                       FORCETEMPSET ;

                 (********)
                 (*CRD   *)
                 (********)

                       GEN0 ( 65 ) ;
                     end (* then *)
                   else
                     ERROR ( 125 ) ;
                 LC := LLC ;
                 GATTR . TYPTR := INTPTR ;
               end (* CARD1 *) ;


            procedure ODD1 ;

               begin (* ODD1 *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> INTPTR then
                     ERROR ( 125 ) ;

                 (********)
                 (*ODD   *)
                 (********)

                 GEN0 ( 20 ) ;
                 GATTR . TYPTR := BOOLPTR
               end (* ODD1 *) ;


            procedure ORD1 ;

               begin (* ORD1 *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM >= POWER then
                     ERROR ( 125 ) ;

                 (********)
                 (*ORD   *)
                 (********)

                 GEN0 ( 61 ) ;
                 GATTR . TYPTR := INTPTR
               end (* ORD1 *) ;


            procedure CHR1 ;

               begin (* CHR1 *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> INTPTR then
                     ERROR ( 125 ) ;

                 (********)
                 (*CHR   *)
                 (********)

                 GEN0 ( 62 ) ;
                 GATTR . TYPTR := CHARPTR
               end (* CHR1 *) ;


            procedure PREDSUCCTIM ;

               begin (* PREDSUCCTIM *)
                 if GATTR . TYPTR <> NIL then
                   if ( LKEY = 24 ) or ( LKEY = 30 ) then
                     begin
                       if GATTR . TYPTR <> INTPTR then
                         ERROR ( 116 ) ;
                       if LKEY = 24 then

                 (*************)
                 (* CSP - CLK *)
                 (*************)

                         GEN1 ( 30 , 21 )
                       else

                 (*************)
                 (* CSP - TRA *)
                 (*************)

                         GEN1 ( 30 , 36 ) ;
                     end (* then *)
                   else
                     if ( GATTR . TYPTR = REALPTR ) or ( GATTR . TYPTR
                     -> . FORM <> SCALAR ) then
                       ERROR ( 125 )
                     else

                 (*********)
                 (*DEC,INC*)
                 (*********)

                       GEN2 ( LKEY , GETTYPE ( GATTR . TYPTR ) , 1 ) ;

                 (*********************************************)
                 (*   LKEY HAPPENS TO BE THE OPCODE AS WELL   *)
                 (*********************************************)

               end (* PREDSUCCTIM *) ;


            procedure EOFEOLN ;

               begin (* EOFEOLN *)
                 RWSETUP ( INPUTPTR ) ;
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM <> FILES then
                     ERROR ( 125 ) ;
                 if LKEY >= 28 then
                   begin
                     EXTUSED := TRUE ;

                 (*****************)
                 (* CSP - EOL/EOT *)
                 (*****************)

                     GEN1 ( 30 , LKEY - 11 ) ;
                   end (* then *)
                 else

                 (*****************)
                 (* CSP - EOF/ELN *)
                 (*****************)

                   GEN1 ( 30 , LKEY ) ;

                 (*************)
                 (* CSP - EIO *)
                 (*************)

                 GEN1 ( 30 , 32 ) ;
                 GATTR . TYPTR := BOOLPTR
               end (* EOFEOLN *) ;


            procedure CALLNONSTANDARD ;

               label 10 ;

               var NXT , LCP , LCP2 : CTP ;
                   LSP : STP ;
                   LB : BOOLEAN ;
                   LOCPAR , LLC , LSIZE , LLC1 , LLC2 , LLC3 , LLC4 ,
                   LLC5 : ADDRRANGE ;
                   I : INTEGER ;
                   PROCNAME : ALPHA ;


               function COMPTLIST ( CP1 , CP2 : CTP ) : BOOLEAN ;

               (*************************************)
               (* MATCH PARAMETER LISTS CP1 AND CP2 *)
               (*************************************)


                  var X : BOOLEAN ;

                  begin (* COMPTLIST *)
                    while ( CP1 <> NIL ) and ( CP2 <> NIL ) do
                      begin

                    (***********************)
                    (* I.E. PROC PARAMETER *)
                    (***********************)

                        if CP1 -> . KLASS in [ PROC , FUNC ] then
                          if ( CP1 -> . KLASS = CP2 -> . KLASS ) and (
                          CP1 -> . IDTYPE = CP2 -> . IDTYPE ) and ( CP1
                          -> . PFDECKIND = CP2 -> . PFDECKIND ) then
                            X := COMPTLIST ( CP1 -> . PRMPTR , CP2 -> .
                                 PRMPTR ) and ( CP1 -> . VKIND = CP2 ->
                                 . VKIND )
                          else
                            X := FALSE
                        else
                          X := COMPTYPES ( CP1 -> . IDTYPE , CP2 -> .
                               IDTYPE ) ;
                        if X then
                          begin
                            CP1 := CP1 -> . NEXT ;
                            CP2 := CP2 -> . NEXT
                          end (* then *)
                        else
                          CP1 := NIL ;
                      end (* while *) ;
                    COMPTLIST := CP1 <> CP2 ;
                  end (* COMPTLIST *) ;


               begin (* CALLNONSTANDARD *)
                 LOCPAR := 0 ;
                 LLC1 := LC ;
                 ALIGN ( LLC1 , MXDATASZE ) ;
                 with FCP -> do
                   begin
                     NXT := PRMPTR ;
                     if PFLEV > LCAFTMST then

                 (*******************)
                 (* PROC. PARAMETER *)
                 (*******************)

                       begin
                         LLC5 := LLC1 ;
                         LLC1 := LLC1 + DISPAREA
                       end (* then *)
                     else
                       LLC5 := 0 ;

                 (********)
                 (*MST   *)
                 (********)

                     GEN2 ( 41 , PFLEV , LLC5 ) ;
                     if PFLEV = LEVEL then
                       LOCAL_CALL := TRUE ;
                   end (* with *) ;
                 if SY = LPARENT then
                   begin
                     LSIZE := 0 ;
                     LLC := LLC1 ;
                     LLC2 := LLC1 ;
                     if FCP -> . FRTRN then
                       begin

                 (***************************************)
                 (* RESERVE STORAGE FOR COPIES OF PARMS *)
                 (***************************************)

                         LCP := NXT ;
                         while LCP <> NIL do
                           begin
                             LSP := LCP -> . IDTYPE ;
                             if LSP <> NIL then
                               if LSP -> . FORM <= POWER then
                                 begin
                                   ALIGN ( LLC1 , LSP -> . ALN ) ;
                                   LLC1 := LLC1 + LSP -> . SIZE
                                 end (* then *) ;
                             LCP := LCP -> . NEXT
                           end (* while *) ;
                         ALIGN ( LLC1 , MXDATASZE ) ;
                       end (* then *) ;
                     LLC3 := LLC1 ;
                     repeat
                       LB := FALSE ;

                 (*****************************************)
                 (*DECIDE WHETHER PROC/FUNC MUST BE PASSED*)
                 (*****************************************)

                       if NXT = NIL then
                         ERROR ( 126 )
                       else
                         LB := NXT -> . KLASS in [ PROC , FUNC ] ;

                 (*********************************************)
                 (*   FOR FORMAL PROC/FUNC LB IS FALSE AND    *)
                 (*   EXPRESSION WILL BE CALLED, WHICH WILL   *)
                 (*   ALWAYS INTERPRET A PROC/FUNC ID AT      *)
                 (*   ITS BEGINNING AS A CALL RATHER THAN     *)
                 (*   A PARAMETER PASSING. IN THIS            *)
                 (*   IMPLEMENTATION, PARAMETER PROCEDURES    *)
                 (*   /FUNCTIONS ARE THEREFORE NOT ALLOWED    *)
                 (*   TO HAVE PROCEDURE/FUNCTION PARAMETERS   *)
                 (*********************************************)

                       INSYMBOL ;
                       if LB then

                 (****************************)
                 (*PASS FUNCTION OR PROCEDURE*)
                 (****************************)

                         begin
                           if SY <> IDENT then
                             begin
                               ERROR ( 2 ) ;
                               SKIP ( FSYS + [ COMMA , RPARENT ] )
                             end (* then *)
                           else
                             begin
                               SEARCHID ( [ NXT -> . KLASS ] , LCP ) ;
                               if COMPTLIST ( LCP , NXT ) then
                                 begin
                                   LOCAL_CALL := TRUE ;

                 (************************)
                 (* => UPDATES DISP REGS *)
                 (************************)

                                   LLC4 := LLC1 + NXT -> . PFLEV DIV 10
                                           ;

                 (**********************************************)
                 (* PFLEV = ADDR OF PROC IN NEW ACTIV RECORD   *)
                 (**********************************************)

                                   LSIZE := DISPAREA ;
                                   if LCP -> . PFKIND = ACTUAL then
                                     with LCP -> do
                                       begin
                                         if FRTRN then
                                           begin

                 (********************************)
                 (* REMEMBER THIS PROC FOR LATER *)
                 (********************************)

                                             LCP2 := FRTPARHD ;
                                             while LCP2 <> NIL do
                                               if LCP2 -> . EXTNAME =
                                               EXTNAME then
                                                 goto 10

                 (********************)
                 (* ALREADY ON LIST  *)
                 (********************)

                                               else
                                                 LCP2 := LCP2 -> .
                                                   NXTFWRD ;
                                             NEW ( LCP2 , PROC ,
                                                   DECLARED ) ;
                                             LCP2 -> := LCP -> ;
                                             with LCP2 -> do
                                               begin
                                                 NAME := '            '
                                                   ;
                                                 UNPACK ( EXTNAME ,
                                                   NAME , 1 ) ;
                                                 PROCLAB := PROCLAB + 1
                                                   ;
                                                 PFNAME := PROCLAB ;
                                                 NXTFWRD := FRTPARHD ;
                                                 FRTPARHD := LCP2 ;
                                               end (* with *) ;
                                             10 :
                                             LCP := LCP2 ;
                                           end (* then *) ;

                 (*****************)
                 (* PASSING PROC  *)
                 (*****************)

                                         UNPACK ( EXTNAME , ID , 1 ) ;

                 (********)
                 (*LCA   *)
                 (********)

                                         GEN1 ( 37 , ORD ( 'P' ) ) ;

                 (********)
                 (*STR   *)
                 (********)

                                         GEN3 ( 56 , ORD ( 'A' ) ,
                                                LEVEL , LLC4 ) ;

                 (********)
                 (*LDA   *)
                 (********)

                                         GEN2 ( 50 , LEVEL , LLC4 +
                                                PTRSIZE ) ;

                 (********)
                 (*LDA   *)
                 (********)

                                         GEN2 ( 50 , 1 , DISPADR +
                                                PTRSIZE ) ;

                 (********)
                 (*MOV   *)
                 (********)

                                         GEN1 ( 40 , DISPAREA - PTRSIZE
                                                ) ;
                                       end (* with *)
                                   else

                 (***************************************)
                 (* PROC PARM IS ITSELF A PASSED PROC   *)
                 (***************************************)

                                     begin

                 (********)
                 (*LDA   *)
                 (********)

                                       GEN2 ( 50 , LEVEL , LLC4 ) ;

                 (********************************************)
                 (* COPY ENTIRE PROC RECORD IN TO PARM LIST  *)
                 (********************************************)

                                       GEN2 ( 50 , LEVEL , LCP -> .
                                              PFLEV DIV 10 ) ;

                 (********)
                 (*MOV   *)
                 (********)

                                       GEN1 ( 40 , DISPAREA ) ;
                                     end (* else *) ;
                                 end (* then *)
                               else
                                 ERROR ( 128 ) ;
                               INSYMBOL ;
                               if not ( SY in FSYS + [ COMMA , RPARENT
                               ] ) then
                                 begin
                                   ERROR ( 6 ) ;
                                   SKIP ( FSYS + [ COMMA , RPARENT ] )
                                 end (* then *)
                             end (* else *)
                         end (* then *)
                       else
                         begin
                           if NXT <> NIL then
                             LC := LLC1 + NXT -> . VADDR ;
                           LLC4 := LC ;
                           EXPRESSION ( FSYS + [ COMMA , RPARENT ] ) ;
                           if GATTR . TYPTR <> NIL then
                             begin
                               if NXT <> NIL then
                                 begin
                                   LSP := NXT -> . IDTYPE ;
                                   if LSP <> NIL then
                                     begin
                                       if ( NXT -> . VKIND = ACTUAL )
                                       then
                                         if LSP -> . FORM < POWER then
                                           begin
                                             LOAD ;
                                             if DEBUG then
                                               begin
                                                 ASSIGN := TRUE ;
                                                 CHKBNDS ( LSP ) ;
                                                 ASSIGN := FALSE ;
                                               end (* then *) ;
                                             if COMPTYPES ( REALPTR ,
                                             LSP ) then
                                               if ( GATTR . TYPTR =
                                               INTPTR ) then
                                                 begin

                 (********)
                 (*FLT   *)
                 (********)

                                                   GEN0 ( 10 ) ;
                                                   GATTR . TYPTR :=
                                                   REALPTR ;
                                                   GATTR . BTYPE :=
                                                   REALPTR ;
                                                 end (* then *) ;
                                             LOCPAR := LOCPAR + 1 ;

                 (************)
                 (*LSP->.SIZE*)
                 (************)

                                             if FCP -> . FRTRN then
                                               begin
                                                 ALIGN ( LLC2 , LSP ->
                                                   . ALN ) ;
                                                 with GATTR do
                                                   begin
                                                   VLEVEL := LEVEL ;
                                                   DPLMT := LLC2 ;
                                                   BTYPE := LSP ;
                                                   KIND := VARBL ;
                                                   ACCESS := DRCT
                                                   end (* with *) ;
                                                 STORE ( GATTR ) ;
                                                 LOADADDRESS ;

                 (********)
                 (*STR   *)
                 (********)

                                                 GEN3 ( 56 , ORD ( 'A'
                                                   ) , LEVEL , LLC3 ) ;
                                                 LLC3 := LLC3 + PTRSIZE
                                                   ;
                                                 LLC2 := LLC2 + LSP ->
                                                   . SIZE ;
                                               end (* then *)
                                             else
                                               GEN3 ( 56 , GETTYPE (
                                                   LSP ) , LEVEL , LLC4
                                                   ) ;
                                           end (* then *)
                                         else

                 (***********************)
                 (* LSP->.FORM >= POWER *)
                 (***********************)

                                           begin
                                             LOCPAR := LOCPAR + 1 ;
                                             if FCP -> . FRTRN then
                                               begin
                                                 if ( LSP -> . FORM =
                                                 POWER ) and ( GATTR .
                                                 ACCESS = STKEXPR )
                                                 then
                                                   begin
                                                   ALIGN ( LLC2 , LSP
                                                   -> . ALN ) ;
                                                   FORCETEMPSET ;
                                                   LSIZE := OPNDSETSIZE
                                                   ( GATTR ) ;

                 (********)
                 (*LDA   *)
                 (********)

                                                   GEN2 ( 50 , LEVEL ,
                                                   LLC2 ) ;

                 (********)
                 (*SMV   *)
                 (********)

                                                   GEN2 ( 69 , - LSP ->
                                                   . SIZE , LSIZE ) ;

                 (********)
                 (*LDA   *)
                 (********)

                                                   GEN2 ( 50 , LEVEL ,
                                                   LLC2 ) ;
                                                   LLC2 := LLC2 + LSP
                                                   -> . SIZE ;
                                                   end (* then *)
                                                 else
                                                   LOADADDRESS ;

                 (********)
                 (*STR   *)
                 (********)

                                                 GEN3 ( 56 , ORD ( 'A'
                                                   ) , LEVEL , LLC3 ) ;
                                                 LLC3 := LLC3 + PTRSIZE
                                                   ;
                                               end (* then *)
                                             else
                                               if LSP -> . FORM = POWER
                                               then
                                                 begin
                                                   LSIZE := OPNDSETSIZE
                                                   ( GATTR ) ;
                                                   LOADADDRESS ;

                 (********)
                 (*LDA   *)
                 (********)

                                                   GEN2 ( 50 , LEVEL ,
                                                   LLC4 ) ;

                 (********)
                 (*SMV   *)
                 (********)

                                                   GEN2 ( 69 , - LSP ->
                                                   . SIZE , LSIZE ) ;
                                                 end (* then *)
                                               else
                                                 begin
                                                   LOADADDRESS ;

                 (********)
                 (*LDA   *)
                 (********)

                                                   GEN2 ( 50 , LEVEL ,
                                                   LLC4 ) ;

                 (********)
                 (*MOV   *)
                 (********)

                                                   GEN1 ( 40 , - LSP ->
                                                   . SIZE ) ;
                                                 end (* else *)
                                           end (* else *)
                                       else

                 (**********************************)
                 (* VKIND = FORMAL I.E.  V AR PARM *)
                 (**********************************)

                                         if GATTR . KIND = VARBL then
                                           begin
                                             LOADADDRESS ;
                                             if not FCP -> . FRTRN then
                                               GEN3 ( 56 , ORD ( 'A' )
                                                   , LEVEL , LLC4 )
                                             else
                                               begin
                                                 GEN3 ( 56 , ORD ( 'A'
                                                   ) , LEVEL , LLC3 ) ;
                                                 LLC3 := LLC3 + PTRSIZE
                                                   ;
                                               end (* else *) ;
                                             if GATTR . ACCESS =
                                             STKEXPR then
                                               ERROR ( 154 ) ;
                                             LOCPAR := LOCPAR + 1 ;

                 (*********)
                 (*PTRSIZE*)
                 (*********)

                                             if GATTR . BTYPE -> . SIZE
                                             <> LSP -> . SIZE then
                                               ERROR ( 142 ) ;
                                           end (* then *)
                                         else
                                           ERROR ( 154 ) ;
                                       if LSP <> NIL then
                                         LSIZE := LSP -> . SIZE ;
                                       if not COMPTYPES ( LSP , GATTR .
                                       TYPTR ) then
                                         ERROR ( 142 )
                                     end (* then *)
                                 end (* then *)
                             end (* then *)
                         end (* else *) ;
                       if ( NXT <> NIL ) then
                         NXT := NXT -> . NEXT
                     until SY <> COMMA ;
                     LC := LLC4 + LSIZE ;
                     if LC > LCMAX then
                       LCMAX := LC ;
                     LC := LLC ;
                     if SY = RPARENT then
                       INSYMBOL
                     else
                       ERROR ( 4 )
                   end (* then *) ;
                 LOCPAR := LOCPAR * 2 ;
                 if NXT <> NIL then
                   ERROR ( 126 ) ;
                 with FCP -> do
                   begin
                     if SAVEFPRS then
                       LOCPAR := LOCPAR + 1 ;

                 (********************)
                 (*ENCODE SAVEFPR FLG*)
                 (********************)

                     if PRCODE then
                       begin
                         PUTIC ;

                 (********)
                 (*CUP   *)
                 (********)

                         WRITELN ( PRR , MN [ 46 ] , CHR ( PROCTYPE (
                                   FCP ) ) : 2 , ',' , LOCPAR : 1 , ','
                                   , EXTNAME , ',' , LLC1 : 1 ) ;
                       end (* then *) ;
                   end (* with *) ;
                 with GATTR do
                   begin
                     TYPTR := FCP -> . IDTYPE ;
                     BTYPE := TYPTR ;
                     if TYPTR <> NIL then
                       if TYPTR -> . FORM = SUBRANGE then
                         TYPTR := TYPTR -> . RANGETYPE
                   end (* with *) ;
               end (* CALLNONSTANDARD *) ;


            begin (* CALL *)
              if FCP -> . PFDECKIND = STANDARD then
                begin
                  LKEY := FCP -> . KEY ;
                  if SY = LPARENT then
                    begin
                      INSYMBOL ;
                      MATCHPAR := TRUE ;
                      if SY = RPARENT then
                        if not ( LKEY in [ 0 , 1 , 2 , 3 , 4 , 11 , 12
                        , 25 , 26 , 28 , 29 ] ) then
                          ERROR ( 7 ) ;

              (*****************************************************)
              (*GET,PUT,RESET,REWRITE,RDLN,WRITELN,EOL,EOT,EOF,ELN *)
              (*****************************************************)

                    end (* then *)
                  else
                    begin
                      if not ( LKEY in [ 0 , 1 , 2 , 3 , 4 , 11 , 12 ,
                      25 , 26 , 28 , 29 ] ) then
                        ERROR ( 7 ) ;

              (*****************************************************)
              (*GET,PUT,RESET,REWRITE,RDLN,WRITELN,EOL,EOT,EOF,ELN *)
              (*****************************************************)

                      MATCHPAR := FALSE ;
                    end (* else *) ;
                  if LKEY in [ 14 .. 24 , 30 , 33 , 39 ] then

              (*********************************)
              (*TRAP,EXIT,ABS...,TRACE,ODD,EXPO*)
              (*********************************)

                    begin

              (********)
              (*TRAP  *)
              (********)

                      if LKEY = 14 then
                        EXPRESSION ( FSYS + [ COMMA ] )
                      else
                        EXPRESSION ( FSYS + [ RPARENT ] ) ;
                      LOAD ;
                    end (* then *) ;
                  case LKEY of
                    0 , 1 , 2 , 3 , 4 :
                      GETPUTRESETREWRITE ;
                    5 , 11 :
                      READ1 ;
                    6 , 12 :
                      WRITE1 ;
                    7 : PACK1 ;
                    8 : UNPACK1 ;
                    9 : NEW1 ;
                    10 , 13 :
                      MARKRELEASE ;
                    14 , 15 :
                      TRAPEXIT ;
                    16 , 17 :
                      SQRABS ;
                    18 , 19 :
                      TRUNCROUND ;
                    20 : ORD1 ;
                    21 : CHR1 ;
                    22 , 23 , 24 , 30 :
                      PREDSUCCTIM ;
                    25 , 26 , 28 , 29 :
                      EOFEOLN ;
                    33 : ODD1 ;
                    35 : MESSAGE1 ;
                    36 , 37 :
                      SKIPLIM ;
                    38 : CARD1 ;
                    39 : EXPO1 ;
                  end (* case *) ;
                  if LKEY in [ 16 .. 26 , 28 , 29 , 33 , 38 , 39 ] then
                    GATTR . BTYPE := GATTR . TYPTR ;
                  if MATCHPAR then
                    if SY = RPARENT then
                      INSYMBOL
                    else
                      ERROR ( 4 ) ;
                end (* then *)
              else
                CALLNONSTANDARD
            end (* CALL *) ;


         procedure GENSETOP ( LATTR : ATTR ; OP : OPRANGE ) ;

            begin (* GENSETOP *)
              with GATTR do
                if ( TYPTR <> NIL ) and ( LATTR . TYPTR <> NIL ) then
                  if ( TYPTR -> . FORM = POWER ) and COMPTYPES ( TYPTR
                  , LATTR . TYPTR ) then
                    begin
                      FORCETEMPSET ;
                      GEN0 ( OP ) ;
                      if OP = 12 then
                        if LATTR . STKLEN < STKLEN then
                          STKLEN := LATTR . STKLEN ;
                      if OP = 31 then
                        if LATTR . STKLEN > STKLEN then
                          STKLEN := LATTR . STKLEN ;
                      if OP = 5 then
                        STKLEN := LATTR . STKLEN ;
                      STKDPLMT := LATTR . STKDPLMT ;
                      LC := STKDPLMT + STKLEN ;
                      if LC > LCMAX then
                        LCMAX := LC ;
                    end (* then *)
                  else
                    begin
                      ERROR ( 134 ) ;
                      GATTR . TYPTR := NIL ;
                      GATTR . BTYPE := NIL
                    end (* else *)
            end (* GENSETOP *) ;


         procedure EXPRESSION ;

            const COMPARE_OP : array [ LTOP .. EQOP ] of 0 .. OPMAX =
                  ( 53 , 52 , 48 , 49 , 55 , 47 ) ;

            var LATTR : ATTR ;
                LOP : OPERATOR ;
                TYPIND : CHAR ;
                LLC , LSIZE : ADDRRANGE ;


            procedure SIMPLEEXPRESSION ( FSYS : SETOFSYS ) ;

               var LATTR : ATTR ;
                   LOP : OPERATOR ;
                   SIGNED : BOOLEAN ;


               procedure TERM ( FSYS : SETOFSYS ) ;

                  var LATTR : ATTR ;
                      LOP : OPERATOR ;


                  procedure FACTOR ( FSYS : SETOFSYS ) ;

                     var LCP : CTP ;
                         LVP : CSP ;
                         VARPART : BOOLEAN ;
                         LATTR : ATTR ;
                         CSTPART : SETRANGE ;
                         LSP : STP ;
                         I , J : INTEGER ;
                         TS_LC , TS_SIZE : ADDRRANGE ;
                         MAXELEM : INTEGER ;

                     begin (* FACTOR *)
                       if not ( SY in FACBEGSYS ) then
                         begin
                           ERROR ( 58 ) ;
                           SKIP ( FSYS + FACBEGSYS ) ;
                           GATTR . TYPTR := NIL
                         end (* then *) ;
                       while SY in FACBEGSYS do
                         begin
                           case SY of
                             IDENT : begin
                                       SEARCHID ( [ STRUCTKONST , KONST
                                                  , VARS , FIELD , FUNC
                                                  ] , LCP ) ;
                                       INSYMBOL ;
                                       if LCP -> . KLASS = FUNC then
                                         begin
                                           CALL ( FSYS , LCP ) ;
                                           GATTR . KIND := EXPR
                                         end (* then *)
                                       else
                                         if LCP -> . KLASS = KONST then
                                           with GATTR , LCP -> do
                                             begin
                                               TYPTR := IDTYPE ;
                                               KIND := CST ;
                                               CVAL := VALUES ;
                                               if SY in [ LBRACK ,
                                               LPARENT ] then
                                                 if STRING ( TYPTR )
                                                 then
                                                   begin
                                                   if SY = LPARENT then
                                                   begin
                                                   ERRKIND := 'W' ;
                                                   ERROR ( 11 )
                                                   end (* then *) ;
                                                   LATTR := GATTR ;
                                                   LOADADDRESS ;
                                                   INSYMBOL ;
                                                   EXPRESSION ( FSYS +
                                                   [ RBRACK , RPARENT ]
                                                   ) ;
                                                   LOAD ;
                                                   LSP := LATTR . TYPTR
                                                   -> . INXTYPE ;
                                                   if COMPTYPES ( TYPTR
                                                   , LSP ) then
                                                   begin
                                                   if LSP <> NIL then
                                                   GETBOUNDS ( LSP , I
                                                   , J )
                                                   else
                                                   begin
                                                   I := 1 ;
                                                   J := LATTR . TYPTR
                                                   -> . SIZE
                                                   end (* else *) ;
                                                   if DEBUG then

                       (********)
                       (*CHK   *)
                       (********)

                                                   GEN3 ( 45 , ORD (
                                                   'J' ) , I , J DIV
                                                   CHARSIZE ) ;
                                                   if I <> 0 then

                       (********)
                       (*DEC   *)
                       (********)

                                                   GEN2 ( 22 , ORD (
                                                   'I' ) , I ) ;

                       (********)
                       (*IXA   *)
                       (********)

                                                   GEN1 ( 36 , CHARSIZE
                                                   ) ;
                                                   end (* then *)
                                                   else
                                                   ERROR ( 139 ) ;
                                                   TYPTR := CHARPTR ;
                                                   KIND := VARBL ;
                                                   BTYPE := CHARPTR ;
                                                   ACCESS := INDRCT ;
                                                   IDPLMT := 0 ;
                                                   if SY = RBRACK then
                                                   INSYMBOL
                                                   else
                                                   if SY = RPARENT then
                                                   begin
                                                   ERRKIND := 'W' ;
                                                   ERROR ( 12 ) ;
                                                   INSYMBOL ;
                                                   end (* then *)
                                                   else
                                                   ERROR ( 12 ) ;
                                                   end (* then *)
                                             end (* with *)
                                         else
                                           SELECTOR ( FSYS , LCP )
                                     end (* tag/ca *) ;
                             INTCONST :
                               begin
                                 with GATTR do
                                   begin
                                     TYPTR := INTPTR ;
                                     KIND := CST ;
                                     CVAL := VAL
                                   end (* with *) ;
                                 INSYMBOL
                               end (* tag/ca *) ;
                             REALCONST :
                               begin
                                 with GATTR do
                                   begin
                                     TYPTR := REALPTR ;
                                     KIND := CST ;
                                     CVAL := VAL
                                   end (* with *) ;
                                 INSYMBOL
                               end (* tag/ca *) ;
                             STRINGCONST :
                               begin
                                 with GATTR do
                                   begin
                                     if LNGTH = 1 then
                                       TYPTR := CHARPTR
                                     else
                                       begin
                                         NEW ( LSP , ARRAYS ) ;
                                         with LSP -> do
                                           begin
                                             AELTYPE := CHARPTR ;
                                             FORM := ARRAYS ;
                                             INXTYPE := NIL ;
                                             SIZE := LNGTH * CHARSIZE ;
                                             ALN := CHARSIZE ;
                                           end (* with *) ;
                                         TYPTR := LSP
                                       end (* else *) ;
                                     KIND := CST ;
                                     CVAL := VAL ;
                                   end (* with *) ;
                                 INSYMBOL
                               end (* tag/ca *) ;
                             LPARENT :
                               begin
                                 INSYMBOL ;
                                 EXPRESSION ( FSYS + [ RPARENT ] ) ;
                                 if SY = RPARENT then
                                   INSYMBOL
                                 else
                                   ERROR ( 4 )
                               end (* tag/ca *) ;
                             NOTSY : begin
                                       INSYMBOL ;
                                       FACTOR ( FSYS ) ;
                                       LOAD ;

                       (********)
                       (*NOT   *)
                       (********)

                                       GEN0 ( 19 ) ;
                                       if GATTR . TYPTR <> NIL then
                                         if GATTR . TYPTR <> BOOLPTR
                                         then
                                           begin
                                             ERROR ( 135 ) ;
                                             GATTR . TYPTR := NIL
                                           end (* then *) ;
                                     end (* tag/ca *) ;
                             LBRACK :
                               begin
                                 INSYMBOL ;
                                 VARPART := FALSE ;
                                 MAXELEM := - 1 ;
                                 for I := 1 to ( SETMAX + 1 ) DIV (
                                 SSETMAX + 1 ) do
                                   CSTPART [ I ] := [ ] ;
                                 NEW ( LSP , POWER ) ;
                                 with LSP -> do
                                   begin
                                     ELSET := NIL ;
                                     SIZE := 0 ;
                                     FORM := POWER
                                   end (* with *) ;
                                 if SY = RBRACK then
                                   begin
                                     with GATTR do
                                       begin
                                         TYPTR := LSP ;
                                         KIND := CST
                                       end (* with *) ;
                                     INSYMBOL
                                   end (* then *)
                                 else
                                   begin
                                     repeat
                                       EXPRESSION ( FSYS + [ COMMA ,
                                                   DOTDOT , RBRACK ] )
                                                   ;
                                       if GATTR . TYPTR <> NIL then
                                         if GATTR . TYPTR -> . FORM <>
                                         SCALAR then
                                           begin
                                             ERROR ( 136 ) ;
                                             GATTR . TYPTR := NIL
                                           end (* then *)
                                         else
                                           if COMPTYPES ( LSP -> .
                                           ELSET , GATTR . TYPTR ) then
                                             begin
                                               LSP -> . ELSET := GATTR
                                                   . TYPTR ;
                                               if GATTR . KIND = CST
                                               then
                                                 begin
                                                   LATTR := GATTR ;
                                                   if SY = DOTDOT then

                       (***************)
                       (* RANGE GIVEN *)
                       (***************)

                                                   begin
                                                   INSYMBOL ;
                                                   EXPRESSION ( FSYS +
                                                   [ COMMA , RBRACK ] )
                                                   ;
                                                   if GATTR . KIND <>
                                                   CST then
                                                   begin
                                                   GATTR := LATTR ;
                                                   ERROR ( 305 )
                                                   end (* then *) ;
                                                   end (* then *) ;
                                                   if GATTR . TYPTR <>
                                                   LATTR . TYPTR then
                                                   ERROR ( 137 )
                                                   else
                                                   if ( LATTR . CVAL .
                                                   IVAL < 0 ) or (
                                                   GATTR . CVAL . IVAL
                                                   > SETMAX ) or (
                                                   LATTR . CVAL . IVAL
                                                   > GATTR . CVAL .
                                                   IVAL ) then
                                                   ERROR ( 304 )
                                                   else
                                                   begin
                                                   if GATTR . CVAL .
                                                   IVAL > MAXELEM then
                                                   begin
                                                   if GATTR . CVAL .
                                                   IVAL > SETMAX then
                                                   begin
                                                   ERROR ( 304 ) ;
                                                   GATTR . CVAL . IVAL
                                                   := SETMAX ;
                                                   end (* then *) ;
                                                   MAXELEM := GATTR .
                                                   CVAL . IVAL
                                                   end (* then *) ;
                                                   for I := LATTR .
                                                   CVAL . IVAL to GATTR
                                                   . CVAL . IVAL do
                                                   begin
                                                   J := I DIV ( SSETMAX
                                                   + 1 ) ;
                                                   CSTPART [ J + 1 ] :=
                                                   CSTPART [ J + 1 ] +
                                                   [ I - J * ( SSETMAX
                                                   + 1 ) ] ;
                                                   end (* for *) ;
                                                   end (* else *)
                                                 end (* then *)
                                               else
                                                 begin
                                                   LOAD ;
                                                   if GATTR . TYPTR <>
                                                   INTPTR then

                       (********)
                       (*ORD   *)
                       (********)

                                                   GEN0 ( 61 ) ;
                                                   if not VARPART then
                                                   begin

                       (**********************)
                       (* ALLOCATE STORAGE   *)
                       (**********************)

                                                   TS_SIZE :=
                                                   MAXSETSIZE ;
                                                   if GATTR . TYPTR <>
                                                   NIL then
                                                   if GATTR . TYPTR <>
                                                   INTPTR then
                                                   TS_SIZE :=
                                                   CALC_SETSIZE ( GATTR
                                                   . TYPTR ) ;
                                                   if TS_SIZE >
                                                   MAXSETSIZE then
                                                   TS_SIZE :=
                                                   MAXSETSIZE ;
                                                   ALIGN ( LC ,
                                                   WORDSIZE ) ;
                                                   TS_LC := LC ;

                       (********)
                       (*SCL   *)
                       (********)

                                                   GEN2 ( 29 , TS_SIZE
                                                   , LC ) ;
                                                   LC := LC + TS_SIZE ;
                                                   if LC > LCMAX then
                                                   LCMAX := LC ;
                                                   VARPART := TRUE ;

                       (********)
                       (*ASE   *)
                       (********)

                                                   GEN1 ( 67 , -
                                                   TS_SIZE ) ;
                                                   end (* then *)
                                                   else

                       (********)
                       (*ASE   *)
                       (********)

                                                   GEN1 ( 67 , TS_SIZE
                                                   ) ;
                                                 end (* else *)
                                             end (* then *)
                                           else
                                             ERROR ( 137 ) ;
                                       TEST := SY <> COMMA ;
                                       if not TEST then
                                         INSYMBOL
                                     until TEST ;
                                     if SY = RBRACK then
                                       INSYMBOL
                                     else
                                       ERROR ( 12 )
                                   end (* else *) ;
                                 if VARPART then
                                   begin
                                     if MAXELEM >= 0 then
                                       begin
                                         NEW ( LVP , PSET ) ;
                                         LVP -> . PVAL := CSTPART ;
                                         LVP -> . PLNGTH := ( ( MAXELEM
                                                   + SETPACK ) DIV
                                                   SETPACK ) * WORDSIZE
                                                   ;
                                         CNSTPTR := LVP ;
                                         ALIGN ( LC , WORDSIZE ) ;

                       (********)
                       (*LCA   *)
                       (********)

                                         GEN1 ( 37 , ORD ( 'S' ) ) ;

                       (********)
                       (*SLD   *)
                       (********)

                                         GEN2 ( 68 , LVP -> . PLNGTH ,
                                                LC ) ;

                       (********)
                       (*UNI   *)
                       (********)

                                         GEN0 ( 31 ) ;
                                         if LVP -> . PLNGTH > TS_SIZE
                                         then
                                           TS_SIZE := LVP -> . PLNGTH ;
                                         if ( TS_SIZE + LC ) > LCMAX
                                         then
                                           LCMAX := TS_SIZE + LC ;
                                       end (* then *) ;
                                     GATTR . KIND := VARBL ;
                                     GATTR . ACCESS := STKEXPR ;
                                     GATTR . STKDPLMT := TS_LC ;
                                     GATTR . STKLEN := TS_SIZE ;
                                     LSP -> . SIZE := TS_SIZE ;
                                   end (* then *)
                                 else
                                   begin
                                     NEW ( LVP , PSET ) ;
                                     LVP -> . PVAL := CSTPART ;
                                     LVP -> . PLNGTH := ( ( MAXELEM +
                                                   SETPACK ) DIV
                                                   SETPACK ) * WORDSIZE
                                                   ;
                                     LSP -> . SIZE := LVP -> . PLNGTH ;
                                     GATTR . KIND := CST ;
                                     GATTR . CVAL . VALP := LVP ;
                                   end (* else *) ;
                                 GATTR . TYPTR := LSP ;
                               end (* tag/ca *)
                           end (* case *) ;
                           if not ( SY in FSYS ) then
                             begin
                               ERROR ( 6 ) ;
                               SKIP ( FSYS + FACBEGSYS )
                             end (* then *) ;
                           if GATTR . KIND <> VARBL then
                             GATTR . BTYPE := GATTR . TYPTR
                           else
                             if GATTR . TYPTR = NIL then
                               GATTR . BTYPE := NIL ;
                         end (* while *)
                     end (* FACTOR *) ;


                  begin (* TERM *)
                    FACTOR ( FSYS + [ MULOP ] ) ;
                    while SY = MULOP do
                      begin
                        if GATTR . TYPTR <> NIL then
                          if GATTR . TYPTR -> . FORM < POWER then
                            LOAD
                          else
                            FORCETEMPSET ;
                        LATTR := GATTR ;
                        LOP := OP ;
                        INSYMBOL ;
                        FACTOR ( FSYS + [ MULOP ] ) ;
                        if GATTR . TYPTR <> NIL then
                          if GATTR . TYPTR -> . FORM < POWER then
                            LOAD ;
                        if ( LATTR . TYPTR <> NIL ) and ( GATTR . TYPTR
                        <> NIL ) then
                          case LOP of
                            MUL : if ( LATTR . TYPTR = INTPTR ) and (
                                  GATTR . TYPTR = INTPTR ) then

                    (********)
                    (*MPI   *)
                    (********)

                                    GEN0 ( 15 )
                                  else
                                    begin
                                      if GATTR . TYPTR = INTPTR then
                                        begin

                    (********)
                    (*FLT   *)
                    (********)

                                          GEN0 ( 10 ) ;
                                          GATTR . TYPTR := REALPTR
                                        end (* then *)
                                      else
                                        if LATTR . TYPTR = INTPTR then
                                          begin

                    (********)
                    (*FLO   *)
                    (********)

                                            GEN0 ( 9 ) ;
                                            LATTR . TYPTR := REALPTR
                                          end (* then *) ;
                                      if ( LATTR . TYPTR = REALPTR )
                                      and ( GATTR . TYPTR = REALPTR )
                                      then

                    (********)
                    (*MPR   *)
                    (********)

                                        GEN0 ( 16 )
                                      else

                    (********)
                    (*INT   *)
                    (********)

                                        GENSETOP ( LATTR , 12 ) ;
                                    end (* else *) ;
                            RDIV : begin
                                     if GATTR . TYPTR = INTPTR then
                                       begin

                    (********)
                    (*FLT   *)
                    (********)

                                         GEN0 ( 10 ) ;
                                         GATTR . TYPTR := REALPTR
                                       end (* then *) ;
                                     if LATTR . TYPTR = INTPTR then
                                       begin

                    (********)
                    (*FLO   *)
                    (********)

                                         GEN0 ( 9 ) ;
                                         LATTR . TYPTR := REALPTR
                                       end (* then *) ;
                                     if ( LATTR . TYPTR = REALPTR ) and
                                     ( GATTR . TYPTR = REALPTR ) then

                    (********)
                    (*DVR   *)
                    (********)

                                       GEN0 ( 7 )
                                     else
                                       begin
                                         ERROR ( 134 ) ;
                                         GATTR . TYPTR := NIL
                                       end (* else *)
                                   end (* tag/ca *) ;
                            IDIV : if ( LATTR . TYPTR = INTPTR ) and (
                                   GATTR . TYPTR = INTPTR ) then

                    (********)
                    (*DVI   *)
                    (********)

                                     GEN0 ( 6 )
                                   else
                                     begin
                                       ERROR ( 134 ) ;
                                       GATTR . TYPTR := NIL
                                     end (* else *) ;
                            IMOD : if ( LATTR . TYPTR = INTPTR ) and (
                                   GATTR . TYPTR = INTPTR ) then
                                     GEN0 ( 14 )
                                   else
                                     begin
                                       ERROR ( 134 ) ;
                                       GATTR . TYPTR := NIL
                                     end (* else *) ;
                            ANDOP : if ( LATTR . TYPTR = BOOLPTR ) and
                                    ( GATTR . TYPTR = BOOLPTR ) then

                    (********)
                    (*AND   *)
                    (********)

                                      GEN0 ( 4 )
                                    else
                                      begin
                                        ERROR ( 134 ) ;
                                        GATTR . TYPTR := NIL
                                      end (* else *)
                          end (* case *)
                        else
                          GATTR . TYPTR := NIL ;
                        GATTR . BTYPE := GATTR . TYPTR ;
                      end (* while *)
                  end (* TERM *) ;


               begin (* SIMPLEEXPRESSION *)
                 SIGNED := FALSE ;
                 if ( SY = ADDOP ) and ( OP in [ PLUS , MINUS ] ) then
                   begin
                     SIGNED := OP = MINUS ;
                     INSYMBOL
                   end (* then *) ;
                 TERM ( FSYS + [ ADDOP ] ) ;
                 if SIGNED then
                   begin
                     LOAD ;
                     if GATTR . TYPTR = INTPTR then

                 (********)
                 (*NGI   *)
                 (********)

                       GEN0 ( 17 )
                     else
                       if GATTR . TYPTR = REALPTR then

                 (********)
                 (*NGR   *)
                 (********)

                         GEN0 ( 18 )
                       else
                         begin
                           ERROR ( 134 ) ;
                           GATTR . TYPTR := NIL
                         end (* else *)
                   end (* then *) ;
                 while SY = ADDOP do
                   begin
                     if GATTR . TYPTR <> NIL then
                       if GATTR . TYPTR -> . FORM < POWER then
                         LOAD
                       else
                         FORCETEMPSET ;
                     LATTR := GATTR ;
                     LOP := OP ;
                     INSYMBOL ;
                     TERM ( FSYS + [ ADDOP ] ) ;
                     if GATTR . TYPTR <> NIL then
                       if GATTR . TYPTR -> . FORM < POWER then
                         LOAD ;
                     if ( LATTR . TYPTR <> NIL ) and ( GATTR . TYPTR <>
                     NIL ) then
                       case LOP of
                         PLUS : if ( LATTR . TYPTR = INTPTR ) and (
                                GATTR . TYPTR = INTPTR ) then

                 (********)
                 (*ADI   *)
                 (********)

                                  GEN0 ( 2 )
                                else
                                  begin
                                    if GATTR . TYPTR = INTPTR then
                                      begin

                 (********)
                 (*FLT   *)
                 (********)

                                        GEN0 ( 10 ) ;
                                        GATTR . TYPTR := REALPTR
                                      end (* then *)
                                    else
                                      if LATTR . TYPTR = INTPTR then
                                        begin

                 (********)
                 (*FLO   *)
                 (********)

                                          GEN0 ( 9 ) ;
                                          LATTR . TYPTR := REALPTR
                                        end (* then *) ;
                                    if ( LATTR . TYPTR = REALPTR ) and
                                    ( GATTR . TYPTR = REALPTR ) then

                 (********)
                 (*ADR   *)
                 (********)

                                      GEN0 ( 3 )
                                    else

                 (********)
                 (*UNI   *)
                 (********)

                                      GENSETOP ( LATTR , 31 ) ;
                                  end (* else *) ;
                         MINUS : if ( LATTR . TYPTR = INTPTR ) and (
                                 GATTR . TYPTR = INTPTR ) then

                 (********)
                 (*SBI   *)
                 (********)

                                   GEN0 ( 21 )
                                 else
                                   begin
                                     if GATTR . TYPTR = INTPTR then
                                       begin

                 (********)
                 (*FLT   *)
                 (********)

                                         GEN0 ( 10 ) ;
                                         GATTR . TYPTR := REALPTR
                                       end (* then *)
                                     else
                                       if LATTR . TYPTR = INTPTR then
                                         begin

                 (********)
                 (*FLO   *)
                 (********)

                                           GEN0 ( 9 ) ;
                                           LATTR . TYPTR := REALPTR
                                         end (* then *) ;
                                     if ( LATTR . TYPTR = REALPTR ) and
                                     ( GATTR . TYPTR = REALPTR ) then

                 (********)
                 (*SBR   *)
                 (********)

                                       GEN0 ( 8 )
                                     else

                 (********)
                 (*DIF   *)
                 (********)

                                       GENSETOP ( LATTR , 5 ) ;
                                   end (* else *) ;
                         OROP : if ( LATTR . TYPTR = BOOLPTR ) and (
                                GATTR . TYPTR = BOOLPTR ) then

                 (********)
                 (*IOR   *)
                 (********)

                                  GEN0 ( 13 )
                                else
                                  begin
                                    ERROR ( 134 ) ;
                                    GATTR . TYPTR := NIL
                                  end (* else *)
                       end (* case *)
                     else
                       GATTR . TYPTR := NIL ;
                     GATTR . BTYPE := GATTR . TYPTR ;
                   end (* while *)
               end (* SIMPLEEXPRESSION *) ;


            begin (* EXPRESSION *)
              LLC := LC ;
              SIMPLEEXPRESSION ( FSYS + [ RELOP ] ) ;
              if SY = RELOP then
                begin
                  if GATTR . TYPTR <> NIL then
                    if GATTR . TYPTR -> . FORM < POWER then
                      LOAD
                    else
                      if GATTR . TYPTR -> . FORM = POWER then
                        FORCETEMPSET
                      else
                        LOADADDRESS ;
                  LATTR := GATTR ;
                  LOP := OP ;
                  if LOP = INOP then
                    if GATTR . TYPTR <> INTPTR then

              (********)
              (*ORD   *)
              (********)

                      GEN0 ( 61 ) ;
                  INSYMBOL ;
                  SIMPLEEXPRESSION ( FSYS ) ;
                  if GATTR . TYPTR <> NIL then
                    if GATTR . TYPTR -> . FORM < POWER then
                      LOAD
                    else
                      if GATTR . TYPTR -> . FORM = POWER then
                        FORCETEMPSET
                      else
                        LOADADDRESS ;
                  if ( LATTR . TYPTR <> NIL ) and ( GATTR . TYPTR <>
                  NIL ) then

              (********)
              (*IN    *)
              (********)

                    if LOP = INOP then
                      if GATTR . TYPTR -> . FORM = POWER then
                        if COMPTYPES ( LATTR . TYPTR , GATTR . TYPTR ->
                        . ELSET ) then

              (********)
              (*INN   *)
              (********)

                          GEN0 ( 11 )
                        else
                          begin
                            ERROR ( 129 ) ;
                            GATTR . TYPTR := NIL
                          end (* else *)
                      else
                        begin
                          ERROR ( 130 ) ;
                          GATTR . TYPTR := NIL
                        end (* else *)
                    else
                      begin
                        if LATTR . TYPTR <> GATTR . TYPTR then
                          if GATTR . TYPTR = INTPTR then
                            begin

              (********)
              (*FLT   *)
              (********)

                              GEN0 ( 10 ) ;
                              GATTR . TYPTR := REALPTR
                            end (* then *)
                          else
                            if LATTR . TYPTR = INTPTR then
                              begin

              (********)
              (*FLO   *)
              (********)

                                GEN0 ( 9 ) ;
                                LATTR . TYPTR := REALPTR
                              end (* then *) ;
                        if COMPTYPES ( LATTR . TYPTR , GATTR . TYPTR )
                        then
                          begin
                            LSIZE := LATTR . TYPTR -> . SIZE ;
                            case LATTR . TYPTR -> . FORM of
                              SCALAR :
                                if LATTR . TYPTR = REALPTR then
                                  TYPIND := 'R'
                                else
                                  if LATTR . TYPTR = BOOLPTR then
                                    TYPIND := 'B'
                                  else
                                    if LATTR . TYPTR = CHARPTR then
                                      TYPIND := 'C'
                                    else
                                      TYPIND := 'I' ;
                              POINTER :
                                begin
                                  if LOP in [ LTOP , LEOP , GTOP , GEOP
                                  ] then
                                    ERROR ( 131 ) ;
                                  TYPIND := 'A'
                                end (* tag/ca *) ;
                              POWER : begin
                                        if LOP in [ LTOP , GTOP ] then
                                          ERROR ( 132 ) ;
                                        TYPIND := 'S' ;
                                      end (* tag/ca *) ;
                              ARRAYS :
                                begin
                                  if not STRING ( LATTR . TYPTR ) then
                                    if LOP in [ LTOP , LEOP , GTOP ,
                                    GEOP ] then
                                      ERROR ( 131 ) ;
                                  TYPIND := 'M'
                                end (* tag/ca *) ;
                              RECORDS :
                                begin
                                  if LOP in [ LTOP , LEOP , GTOP , GEOP
                                  ] then
                                    ERROR ( 131 ) ;
                                  TYPIND := 'M'
                                end (* tag/ca *) ;
                              FILES : begin
                                        ERROR ( 133 ) ;
                                        TYPIND := 'F'
                                      end (* tag/ca *)
                            end (* case *) ;
                            GEN2 ( COMPARE_OP [ LOP ] , ORD ( TYPIND )
                                   , LSIZE ) ;
                          end (* then *)
                        else
                          ERROR ( 129 )
                      end (* else *) ;
                  GATTR . TYPTR := BOOLPTR ;
                  GATTR . BTYPE := BOOLPTR ;
                  GATTR . KIND := EXPR ;
                  LC := LLC ;
                end (* then *)
            end (* EXPRESSION *) ;


         procedure ASSIGNMENT ( FCP : CTP ) ;

            var LATTR : ATTR ;
                RSIZE , LLC : ADDRRANGE ;

            begin (* ASSIGNMENT *)
              LLC := LC ;
              SELECTOR ( FSYS + [ BECOMES ] , FCP ) ;
              VAR_MOD := VAR_MOD + 1 ;
              if SY = BECOMES then
                begin
                  if GATTR . TYPTR <> NIL then
                    if ( GATTR . ACCESS <> DRCT ) or ( GATTR . TYPTR ->
                    . FORM >= POWER ) then
                      LOADADDRESS ;
                  LATTR := GATTR ;
                  INSYMBOL ;
                  EXPRESSION ( FSYS ) ;
                  if GATTR . TYPTR <> NIL then
                    if GATTR . TYPTR -> . FORM < POWER then
                      LOAD
                    else
                      if GATTR . TYPTR -> . FORM = POWER then
                        FORCETEMPSET
                      else
                        LOADADDRESS ;
                  if ( LATTR . TYPTR <> NIL ) and ( GATTR . TYPTR <>
                  NIL ) then
                    begin
                      if COMPTYPES ( REALPTR , LATTR . TYPTR ) and (
                      GATTR . TYPTR = INTPTR ) then
                        begin

              (********)
              (*FLT   *)
              (********)

                          GEN0 ( 10 ) ;
                          GATTR . TYPTR := REALPTR
                        end (* then *) ;
                      if COMPTYPES ( LATTR . TYPTR , GATTR . TYPTR )
                      then
                        begin
                          if DEBUG then
                            begin
                              ASSIGN := TRUE ;
                              CHKBNDS ( LATTR . BTYPE ) ;
                              ASSIGN := FALSE
                            end (* then *) ;
                          case LATTR . TYPTR -> . FORM of
                            SCALAR , SUBRANGE , POINTER :
                              STORE ( LATTR ) ;

              (********)
              (*SMV   *)
              (********)

                            POWER : GEN2 ( 69 , LATTR . TYPTR -> . SIZE
                                           , GATTR . TYPTR -> . SIZE )
                                           ;

              (********)
              (*MOV   *)
              (********)

                            ARRAYS , RECORDS :
                              GEN1 ( 40 , LATTR . TYPTR -> . SIZE ) ;
                            FILES : ERROR ( 146 )
                          end (* case *)
                        end (* then *)
                      else
                        ERROR ( 129 )
                    end (* then *)
                end (* then *)
              else
                ERROR ( 51 ) ;
              LC := LLC ;
            end (* ASSIGNMENT *) ;


         procedure GOTOSTATEMENT ;

            label 10 ;

            var LLP : LBP ;
                TTOP : DISPRANGE ;
                XLABEL : ALPHA ;

            begin (* GOTOSTATEMENT *)
              if SY = INTCONST then
                begin
                  TTOP := LEVEL ;
                  repeat
                    LLP := DISPLAY [ TTOP ] . FLABEL ;
                    while LLP <> NIL do
                      with LLP -> do
                        if LABVAL = VAL . IVAL then
                          begin
                            if TTOP = LEVEL then

              (********)
              (*UJP   *)
              (********)

                              GENUJPFJP ( 57 , LABNAME )
                            else
                              begin
                                if XNO = 0 then

              (*****************************************)
              (*   FIRST DEEP GOTO TO THIS LABEL       *)
              (*****************************************)

                                  begin
                                    XLABNO := XLABNO + 1 ;
                                    XNO := XLABNO
                                  end (* then *) ;
                                XLABEL := '############' ;
                                MKNAME ( XLABEL , XNO , FALSE ) ;
                                if PRCODE then
                                  WRITELN ( PRR , MN [ 73 ] , ' ' ,
                                            XLABEL : EXTNAMSZ ) ;
                              end (* else *) ;
                            CTREMIT ( CTRGOTO , 0 , LINECNT , 0 ,
                                      LINECNT ) ;
                            goto 10
                          end (* then *)
                        else
                          LLP := NEXTLAB ;
                    TTOP := TTOP - 1
                  until TTOP = 0 ;
                  ERROR ( 167 ) ;
                  10 :
                  INSYMBOL
                end (* then *)
              else
                ERROR ( 15 )
            end (* GOTOSTATEMENT *) ;


         procedure COMPOUNDSTATEMENT ;

            begin (* COMPOUNDSTATEMENT *)
              repeat
                repeat
                  STATEMENT ( FSYS + [ SEMICOLON , ENDSY ] , LOOPC ,
                              SUBR ) ;
                until not ( SY in STATBEGSYS ) ;
                TEST := SY <> SEMICOLON ;
                if not TEST then
                  INSYMBOL
              until TEST ;
              if SY = ENDSY then
                INSYMBOL
              else
                ERROR ( 13 )
            end (* COMPOUNDSTATEMENT *) ;


         procedure IFSTATEMENT ;

            var LCIX1 , LCIX2 : LABELRNG ;
                FIRSTLN , MIDLN : INTEGER ;
                CTRNO : CTRRANGE ;

            begin (* IFSTATEMENT *)
              EXPRESSION ( FSYS + [ THENSY ] ) ;
              GENLABEL ( LCIX1 ) ;
              GENFJP ( LCIX1 ) ;
              if SY = THENSY then
                INSYMBOL
              else
                ERROR ( 52 ) ;
              FIRSTLN := LINECNT ;
              CTRNO := CTRGEN ;

              (********************)
              (*** COUNTER HERE ***)
              (********************)

              STATEMENT ( FSYS + [ ELSESY ] , LOOPC , SUBR ) ;
              if SY = ELSESY then
                begin
                  GENLABEL ( LCIX2 ) ;

              (********)
              (*UJP   *)
              (********)

                  GENUJPFJP ( 57 , LCIX2 ) ;
                  PUTLABEL ( LCIX1 ) ;
                  INSYMBOL ;
                  MIDLN := LINECNT ;
                  STATEMENT ( FSYS , LOOPC , SUBR ) ;
                  PUTLABEL ( LCIX2 )
                end (* then *)
              else
                begin
                  PUTLABEL ( LCIX1 ) ;
                  MIDLN := 0 ;
                end (* else *) ;
              CTREMIT ( CTRIF , CTRNO , FIRSTLN , MIDLN , LINECNT )
            end (* IFSTATEMENT *) ;


         procedure CASESTATEMENT ;

            label 1 ;

            type CIP = -> CASEINFO ;
                 CASEINFO = record
                              NEXT : CIP ;
                              CSSTART : LABELRNG ;
                              CSLAB1 , CSLAB2 : INTEGER
                            end ;

            var LSP , LSP1 : STP ;
                FSTPTR , LPT1 , LPT2 , LPT3 : CIP ;
                LVAL , LVAL1 : VALU ;
                LADDR , LCIX , LCIX1 , UBND , LBND , XADDR : LABELRNG ;
                LMIN , LMAX : INTEGER ;
                OTHWC : BOOLEAN ;
                FIRSTLN : INTEGER ;
                TEMPLN : INTEGER ;
                CTRCASES : INTEGER ;
                CTRNO : CTRRANGE ;

            begin (* CASESTATEMENT *)
              EXPRESSION ( FSYS + [ OFSY , COMMA , COLON ] ) ;
              LOAD ;
              LSP := GATTR . TYPTR ;
              if LSP <> NIL then
                if ( LSP -> . FORM <> SCALAR ) or ( LSP = REALPTR )
                then
                  begin
                    ERROR ( 144 ) ;
                    LSP := NIL
                  end (* then *)
                else
                  if not COMPTYPES ( LSP , INTPTR ) then

              (********)
              (*ORD   *)
              (********)

                    GEN0 ( 61 ) ;
              if DEBUG then
                CHKBNDS ( GATTR . TYPTR ) ;
              if SY = OFSY then
                INSYMBOL
              else
                ERROR ( 8 ) ;
              FSTPTR := NIL ;
              GENLABEL ( LBND ) ;
              GENLABEL ( UBND ) ;
              GENLABEL ( LCIX ) ;
              GENLABEL ( LADDR ) ;

              (******************************************)
              (* WE SHOULD HAVE:                        *)
              (* LADDR = LCIX+1 = UBND+2 = LBND+3  HERE *)
              (******************************************)

              GENLABEL ( XADDR ) ;

              (********)
              (*XJP   *)
              (********)

              GENUJPFJP ( 44 , LBND ) ;
              OTHWC := FALSE ;
              FIRSTLN := LINECNT ;
              CTRCASES := 0 ;
              repeat
                LPT3 := NIL ;
                GENLABEL ( LCIX1 ) ;
                if not ( SY in [ SEMICOLON , ENDSY ] ) then
                  begin
                    if SY <> OTHERWISESY then
                      begin
                        repeat
                          CONSTANT ( FSYS + [ COMMA , COLON , DOTDOT ]
                                     , LSP1 , LVAL ) ;
                          if LSP <> NIL then
                            if COMPTYPES ( LSP , LSP1 ) then
                              begin
                                LVAL1 . IVAL := LVAL . IVAL ;
                                if SY = DOTDOT then
                                  begin
                                    INSYMBOL ;
                                    CONSTANT ( FSYS + [ COMMA , COLON ]
                                               , LSP1 , LVAL1 )
                                  end (* then *) ;
                                if COMPTYPES ( LSP , LSP1 ) then
                                  if LVAL . IVAL <= LVAL1 . IVAL then
                                    begin
                                      LPT1 := FSTPTR ;
                                      LPT2 := NIL ;
                                      while LPT1 <> NIL do
                                        with LPT1 -> do
                                          begin
                                            if LVAL1 . IVAL >= CSLAB2
                                            then
                                              begin
                                                if LVAL . IVAL <=
                                                CSLAB2 then
                                                  ERROR ( 156 ) ;
                                                goto 1
                                              end (* then *) ;
                                            LPT2 := LPT1 ;
                                            LPT1 := NEXT
                                          end (* with *) ;
                                      1 :
                                      NEW ( LPT3 ) ;
                                      with LPT3 -> do
                                        begin
                                          NEXT := LPT1 ;
                                          CSLAB1 := LVAL . IVAL ;
                                          CSLAB2 := LVAL1 . IVAL ;
                                          CSSTART := LCIX1
                                        end (* with *) ;
                                      if LPT2 = NIL then
                                        FSTPTR := LPT3
                                      else
                                        LPT2 -> . NEXT := LPT3
                                    end (* then *)
                                  else
                                    ERROR ( 102 )
                                else
                                  ERROR ( 147 ) ;
                              end (* then *)
                            else
                              ERROR ( 147 ) ;
                          TEST := SY <> COMMA ;
                          if not TEST then
                            INSYMBOL
                        until TEST ;
                        if SY = COLON then
                          INSYMBOL
                        else
                          ERROR ( 5 )
                      end (* then *)
                    else

              (********************)
              (* SY = OTHERWISESY *)
              (********************)

                      begin
                        if OTHWC then
                          ERROR ( 156 )
                        else
                          LCIX1 := LADDR ;
                        OTHWC := TRUE ;
                        INSYMBOL ;
                        if SY = COLON then
                          INSYMBOL

              (********************)
              (* IGNORE : FOR NOW *)
              (********************)

                      end (* else *) ;
                    PUTLABEL ( LCIX1 ) ;
                    TEMPLN := LINECNT ;

              (********************)
              (*** COUNTER HERE ***)
              (********************)

                    CTRNO := CTRGEN ;
                    CTRCASES := CTRCASES + 1 ;
                    repeat
                      STATEMENT ( FSYS + [ SEMICOLON ] , LOOPC , SUBR )
                                  ;
                    until not ( SY in STATBEGSYS ) ;

              (********)
              (*UJP   *)
              (********)

                    GENUJPFJP ( 57 , XADDR ) ;
                    CTREMIT ( CTRCASE , CTRNO , TEMPLN , 0 , LINECNT )
                              ;
                  end (* then *) ;
                TEST := SY <> SEMICOLON ;
                if not TEST then
                  INSYMBOL ;
              until TEST ;
              if FSTPTR <> NIL then
                begin
                  LMAX := FSTPTR -> . CSLAB2 ;

              (******************)
              (*REVERSE POINTERS*)
              (******************)

                  LPT1 := FSTPTR ;
                  FSTPTR := NIL ;
                  repeat
                    LPT2 := LPT1 -> . NEXT ;
                    LPT1 -> . NEXT := FSTPTR ;
                    FSTPTR := LPT1 ;
                    LPT1 := LPT2
                  until LPT1 = NIL ;
                  LMIN := FSTPTR -> . CSLAB1 ;
                end (* then *)
              else
                begin
                  LMIN := 1 ;
                  LMAX := 0
                end (* else *) ;
              GENDEF ( LBND , LMIN ) ;
              GENDEF ( UBND , LMAX ) ;
              PUTLABEL ( LCIX ) ;
              if LMAX - LMIN < CIXMAX then
                begin
                  if FSTPTR <> NIL then
                    repeat
                      with FSTPTR -> do
                        begin
                          while CSLAB1 > LMIN do
                            begin

              (********)
              (*UJP   *)
              (********)

                              GENUJPFJP ( 57 , LADDR ) ;
                              LMIN := LMIN + 1
                            end (* while *) ;
                          repeat

              (********)
              (*UJP   *)
              (********)

                            GENUJPFJP ( 57 , CSSTART ) ;
                            LMIN := LMIN + 1 ;
                          until LMIN > CSLAB2 ;
                          FSTPTR := NEXT ;
                        end (* with *)
                    until FSTPTR = NIL ;
                  if not OTHWC then
                    PUTLABEL ( LADDR ) ;
                  PUTLABEL ( XADDR ) ;
                  CTREMIT ( CTRCASE , 0 , FIRSTLN , CTRCASES , LINECNT
                            ) ;
                end (* then *)
              else
                ERROR ( 157 ) ;
              if SY = ENDSY then
                INSYMBOL
              else
                ERROR ( 13 )
            end (* CASESTATEMENT *) ;


         procedure BREAKSTATEMENT ;

            begin (* BREAKSTATEMENT *)
              if LOOPC . LEVEL <= 0 then
                ERROR ( 70 )
              else
                begin
                  GENUJPFJP ( 57 , LOOPC . BREAKLABEL ) ;
                  LOOPC . BREAKUSED := TRUE ;
                end (* else *)
            end (* BREAKSTATEMENT *) ;


         procedure CONTSTATEMENT ;

            begin (* CONTSTATEMENT *)
              if LOOPC . LEVEL <= 0 then
                ERROR ( 71 )
              else
                begin
                  GENUJPFJP ( 57 , LOOPC . CONTLABEL ) ;
                  LOOPC . CONTUSED := TRUE ;
                end (* else *)
            end (* CONTSTATEMENT *) ;


         procedure RETURNSTATEMENT ;

            begin (* RETURNSTATEMENT *)
              GENUJPFJP ( 57 , SUBR . RETURNLABEL ) ;
              SUBR . RETURNUSED := TRUE ;
            end (* RETURNSTATEMENT *) ;


         procedure REPEATSTATEMENT ;

            var LADDR : LABELRNG ;
                LEXIT : LABELRNG ;
                LCONT : LABELRNG ;
                FIRSTLN : INTEGER ;
                CTRNO : CTRRANGE ;
                LOOPR : LOOPCTL ;

            begin (* REPEATSTATEMENT *)
              GENLABEL ( LADDR ) ;
              GENLABEL ( LCONT ) ;
              GENLABEL ( LEXIT ) ;
              PUTLABEL ( LADDR ) ;
              LOOPR . LEVEL := LOOPC . LEVEL + 1 ;
              LOOPR . BREAKLABEL := LEXIT ;
              LOOPR . BREAKUSED := FALSE ;
              LOOPR . CONTLABEL := LCONT ;
              LOOPR . CONTUSED := FALSE ;
              FIRSTLN := LINECNT ;
              CTRNO := CTRGEN ;

              (********************)
              (*** COUNTER HERE ***)
              (********************)

              repeat
                repeat
                  STATEMENT ( FSYS + [ SEMICOLON , UNTILSY ] , LOOPR ,
                              SUBR ) ;
                until not ( SY in STATBEGSYS ) ;
                TEST := SY <> SEMICOLON ;
                if not TEST then
                  INSYMBOL
              until TEST ;
              if SY = UNTILSY then
                begin
                  if LOOPR . CONTUSED then
                    PUTLABEL ( LCONT ) ;
                  INSYMBOL ;
                  EXPRESSION ( FSYS ) ;
                  GENFJP ( LADDR ) ;
                  if LOOPR . BREAKUSED then
                    PUTLABEL ( LEXIT ) ;
                  CTREMIT ( CTRREPEAT , CTRNO , FIRSTLN , 0 , LINECNT )
                end (* then *)
              else
                ERROR ( 53 ) ;
            end (* REPEATSTATEMENT *) ;


         procedure WHILESTATEMENT ;

            var LADDR , LCIX : LABELRNG ;
                FIRSTLN : INTEGER ;
                CTRNO : CTRRANGE ;
                LOOPW : LOOPCTL ;

            begin (* WHILESTATEMENT *)
              GENLABEL ( LADDR ) ;
              GENLABEL ( LCIX ) ;
              LOOPW . LEVEL := LOOPC . LEVEL + 1 ;
              LOOPW . BREAKLABEL := LCIX ;
              LOOPW . BREAKUSED := FALSE ;
              LOOPW . CONTLABEL := LADDR ;
              LOOPW . CONTUSED := FALSE ;
              PUTLABEL ( LADDR ) ;
              EXPRESSION ( FSYS + [ DOSY ] ) ;
              GENFJP ( LCIX ) ;
              if SY = DOSY then
                INSYMBOL
              else
                ERROR ( 54 ) ;
              FIRSTLN := LINECNT ;
              CTRNO := CTRGEN ;

              (********************)
              (*** COUNTER HERE ***)
              (********************)

              STATEMENT ( FSYS , LOOPW , SUBR ) ;

              (********)
              (*UJP   *)
              (********)

              GENUJPFJP ( 57 , LADDR ) ;
              PUTLABEL ( LCIX ) ;
              CTREMIT ( CTRWHILE , CTRNO , FIRSTLN , 0 , LINECNT ) ;
            end (* WHILESTATEMENT *) ;


         procedure FORSTATEMENT ;

            var LATTR : ATTR ;
                LSP : STP ;
                LSY : SYMBOL ;
                LOP : OPRANGE ;
                XT , CV1 , CV2 : INTEGER ;
                CB1 , CB2 : BOOLEAN ;
                LCIX , LADDR , LINCR : LABELRNG ;
                LLC : ADDRRANGE ;
                FIRSTLN : INTEGER ;
                CTRNO : CTRRANGE ;
                LOOPF : LOOPCTL ;

            begin (* FORSTATEMENT *)
              if SY = IDENT then
                begin
                  SEARCHID ( [ VARS ] , LCP ) ;
                  with LCP -> , LATTR do
                    begin
                      TYPTR := IDTYPE ;
                      KIND := VARBL ;
                      BTYPE := TYPTR ;
                      if TYPTR <> NIL then
                        if TYPTR -> . FORM = SUBRANGE then
                          TYPTR := TYPTR -> . RANGETYPE ;
                      if VKIND = ACTUAL then
                        begin
                          ACCESS := DRCT ;
                          VLEVEL := VLEV ;
                          DPLMT := VADDR ;
                        end (* then *)
                      else
                        begin
                          ERROR ( 155 ) ;
                          TYPTR := NIL
                        end (* else *)
                    end (* with *) ;
                  if LATTR . TYPTR <> NIL then
                    if ( LATTR . TYPTR -> . FORM > SUBRANGE ) or (
                    LATTR . TYPTR = REALPTR ) then
                      begin
                        ERROR ( 143 ) ;
                        LATTR . TYPTR := NIL
                      end (* then *) ;
                  INSYMBOL
                end (* then *)
              else
                begin
                  ERROR ( 2 ) ;
                  LATTR . TYPTR := NIL ;
                  SKIP ( FSYS + [ BECOMES , TOSY , DOWNTOSY , DOSY ] )
                end (* else *) ;
              if SY = BECOMES then
                begin
                  INSYMBOL ;
                  EXPRESSION ( FSYS + [ TOSY , DOWNTOSY , DOSY ] ) ;
                  if GATTR . TYPTR <> NIL then
                    if GATTR . TYPTR -> . FORM <> SCALAR then
                      ERROR ( 144 )
                    else
                      if COMPTYPES ( LATTR . TYPTR , GATTR . TYPTR )
                      then
                        begin
                          if GATTR . KIND = CST then
                            begin
                              CB1 := TRUE ;
                              CV1 := GATTR . CVAL . IVAL
                            end (* then *)
                          else
                            CB1 := FALSE ;
                          LOAD ;
                          STORE ( LATTR ) ;
                        end (* then *)
                      else
                        ERROR ( 145 )
                end (* then *)
              else
                begin
                  ERROR ( 51 ) ;
                  SKIP ( FSYS + [ TOSY , DOWNTOSY , DOSY ] )
                end (* else *) ;
              if ( SY = TOSY ) or ( SY = DOWNTOSY ) then
                begin
                  LSY := SY ;
                  INSYMBOL ;
                  EXPRESSION ( FSYS + [ DOSY ] ) ;
                  if GATTR . TYPTR <> NIL then
                    if GATTR . TYPTR -> . FORM <> SCALAR then
                      ERROR ( 144 )
                    else
                      if COMPTYPES ( LATTR . TYPTR , GATTR . TYPTR )
                      then
                        begin
                          if GATTR . KIND = CST then
                            begin
                              CB2 := TRUE ;
                              LLC := LC ;
                              CV2 := GATTR . CVAL . IVAL
                            end (* then *)
                          else
                            begin
                              CB2 := FALSE ;
                              LOAD ;
                              ALIGN ( LC , INTSIZE ) ;
                              LLC := LC ;
                              if GATTR . TYPTR <> INTPTR then

              (********)
              (*ORD   *)
              (********)

                                GEN0 ( 61 ) ;

              (********)
              (*STR   *)
              (********)

                              GEN3 ( 56 , ORD ( 'I' ) , LEVEL , LLC ) ;
                              LC := LC + INTSIZE ;
                              if LC > LCMAX then
                                LCMAX := LC ;
                            end (* else *) ;
                          if CB1 and CB2 then
                            begin
                              XT := 1 ;
                              if LSY = TOSY then
                                if CV1 > CV2 then
                                  XT := 0
                                else

                              else
                                if CV1 < CV2 then
                                  XT := 0 ;

              (********)
              (*LDC   *)
              (********)

                              GEN2 ( 51 , 3 , XT ) ;
                            end (* then *)
                          else
                            begin
                              if CB1 then

              (********)
              (*LDC   *)
              (********)

                                GEN2 ( 51 , 1 , CV1 )
                              else
                                begin
                                  GATTR := LATTR ;
                                  LOAD ;
                                  if GATTR . TYPTR <> INTPTR then

              (********)
              (*ORD   *)
              (********)

                                    GEN0 ( 61 ) ;
                                end (* else *) ;
                              if CB2 then

              (********)
              (*LDC   *)
              (********)

                                GEN2 ( 51 , 1 , CV2 )
                              else

              (********)
              (*LOD   *)
              (********)

                                GEN3 ( 54 , ORD ( 'I' ) , LEVEL , LLC )
                                       ;
                              if LSY = TOSY then
                                LOP := 52
                              else
                                LOP := 48 ;
                              GEN2 ( LOP , ORD ( 'I' ) , 1 ) ;
                            end (* else *) ;
                        end (* then *)
                      else
                        ERROR ( 145 )
                end (* then *)
              else
                begin
                  ERROR ( 55 ) ;
                  SKIP ( FSYS + [ DOSY ] )
                end (* else *) ;
              GENLABEL ( LADDR ) ;
              GENLABEL ( LINCR ) ;
              GENLABEL ( LCIX ) ;
              LOOPF . LEVEL := LOOPC . LEVEL + 1 ;
              LOOPF . BREAKLABEL := LCIX ;
              LOOPF . BREAKUSED := FALSE ;
              LOOPF . CONTLABEL := LINCR ;
              LOOPF . CONTUSED := FALSE ;

              (********)
              (*FJP   *)
              (********)

              GENUJPFJP ( 33 , LCIX ) ;
              PUTLABEL ( LADDR ) ;

              (***********************************)
              (*   BEGINNING OF THE FOR 'LOOP'   *)
              (***********************************)

              if SY = DOSY then
                INSYMBOL
              else
                ERROR ( 54 ) ;
              FIRSTLN := LINECNT ;
              CTRNO := CTRGEN ;

              (********************)
              (*** COUNTER HERE ***)
              (********************)

              STATEMENT ( FSYS , LOOPF , SUBR ) ;
              if LOOPF . CONTUSED then
                PUTLABEL ( LINCR ) ;
              GATTR := LATTR ;
              LOAD ;
              if GATTR . TYPTR <> INTPTR then

              (********)
              (*ORD   *)
              (********)

                GEN0 ( 61 ) ;
              if CB2 then

              (********)
              (*LDC   *)
              (********)

                GEN2 ( 51 , 1 , CV2 )
              else

              (********)
              (*LOD   *)
              (********)

                GEN3 ( 54 , ORD ( 'I' ) , LEVEL , LLC ) ;

              (********)
              (*NEQ   *)
              (********)

              GEN2 ( 55 , ORD ( 'I' ) , 1 ) ;

              (********)
              (*FJP   *)
              (********)

              GENUJPFJP ( 33 , LCIX ) ;
              GATTR := LATTR ;
              LOAD ;
              LOP := 23 ;

              (********)
              (*INC   *)
              (********)

              if LSY <> TOSY then
                LOP := 22 ;

              (********)
              (*DEC   *)
              (********)

              GEN2 ( LOP , GETTYPE ( GATTR . TYPTR ) , 1 ) ;
              if DEBUG then
                CHKBNDS ( LATTR . TYPTR ) ;
              STORE ( LATTR ) ;

              (********)
              (*UJP   *)
              (********)

              GENUJPFJP ( 57 , LADDR ) ;
              PUTLABEL ( LCIX ) ;
              LC := LLC ;
              CTREMIT ( CTRFOR , CTRNO , FIRSTLN , 0 , LINECNT ) ;
            end (* FORSTATEMENT *) ;


         procedure WITHSTATEMENT ;

            var LCP : CTP ;
                LCNT : DISPRANGE ;
                LLC : ADDRRANGE ;
                OLD_LEV : - 1 .. DISPLIMIT ;
                REC_STR : STP ;

            begin (* WITHSTATEMENT *)
              LLC := LC ;
              if GET_STAT then
                WS_CNT := WS_CNT + 1 ;
              if SY = IDENT then
                begin
                  SEARCHID ( [ VARS , FIELD ] , LCP ) ;
                  INSYMBOL
                end (* then *)
              else
                begin
                  ERROR ( 2 ) ;
                  LCP := UVARPTR
                end (* else *) ;
              SELECTOR ( FSYS + [ COMMA , DOSY ] , LCP ) ;
              REC_STR := GATTR . TYPTR ;
              if GATTR . TYPTR <> NIL then
                if GATTR . TYPTR -> . FORM = RECORDS then
                  if TOP < DISPLIMIT then
                    begin
                      TOP := TOP + 1 ;
                      with DISPLAY [ TOP ] do
                        begin
                          OLD_LEV := REC_STR -> . FLD_DISP_LEV ;
                          REC_STR -> . FLD_DISP_LEV := TOP ;
                          if GATTR . ACCESS = DRCT then
                            begin
                              OCCUR := CREC ;
                              CLEV := GATTR . VLEVEL ;
                              CDSPL := GATTR . DPLMT
                            end (* then *)
                          else
                            begin
                              LOADADDRESS ;
                              ALIGN ( LC , PTRSIZE ) ;

              (********)
              (*STR   *)
              (********)

                              GEN3 ( 56 , ORD ( 'A' ) , LEVEL , LC ) ;
                              OCCUR := VREC ;
                              VDSPL := LC ;
                              LC := LC + PTRSIZE ;
                              if LC > LCMAX then
                                LCMAX := LC
                            end (* else *)
                        end (* with *)
                    end (* then *)
                  else
                    FATALERROR ( 250 )
                else
                  begin
                    ERROR ( 140 ) ;
                    REC_STR := NIL
                  end (* else *) ;
              OPEN_RECORD := REC_STR ;
              if SY = COMMA then
                begin
                  INSYMBOL ;
                  WITHSTATEMENT
                end (* then *)
              else
                begin
                  if SY = DOSY then
                    INSYMBOL
                  else
                    ERROR ( 54 ) ;
                  STATEMENT ( FSYS , LOOPC , SUBR ) ;
                end (* else *) ;
              if REC_STR <> NIL then
                REC_STR -> . FLD_DISP_LEV := OLD_LEV ;
              TOP := TOP - 1 ;
              LC := LLC ;
              OPEN_RECORD := NIL ;
            end (* WITHSTATEMENT *) ;


         begin (* STATEMENT *)
           if SY = INTCONST then

           (********)
           (*LABEL *)
           (********)

             begin
               LLP := DISPLAY [ LEVEL ] . FLABEL ;
               while LLP <> NIL do
                 with LLP -> do
                   if LABVAL = VAL . IVAL then
                     begin
                       if DEFINED then
                         ERROR ( 165 ) ;
                       if XNO > 0 then

           (***********************************)
           (* LABEL IS AN EXTERNAL ENTRY PT.  *)
           (***********************************)

                         begin
                           XLABEL := '############' ;
                           MKNAME ( XLABEL , XNO , FALSE ) ;
                           if PRCODE then
                             WRITELN ( PRR , XLABEL : EXTNAMSZ , MN [
                                       74 ] ) ;
                           XNO := 0 ;

           (***************************)
           (* IN CASE OF REDEFINITION *)
           (***************************)

                         end (* then *) ;
                       PUTLABEL ( LABNAME ) ;
                       DEFINED := TRUE ;
                       CTRNO := CTRGEN ;
                       CTREMIT ( CTRLBL , CTRNO , LINECNT , 0 , LINECNT
                                 ) ;

           (********************)
           (*** COUNTER HERE ***)
           (********************)

                       goto 1
                     end (* then *)
                   else
                     LLP := NEXTLAB ;
               ERROR ( 167 ) ;
               1 :
               INSYMBOL ;
               if SY = COLON then
                 INSYMBOL
               else
                 ERROR ( 5 )
             end (* then *) ;
           if not ( SY in FSYS + [ IDENT ] ) then
             begin
               ERROR ( 6 ) ;
               SKIP ( FSYS )
             end (* then *) ;
           if SY in STATBEGSYS + [ IDENT ] then
             begin
               case SY of
                 IDENT : begin
                           SEARCHID ( [ VARS , FIELD , FUNC , PROC ] ,
                                      LCP ) ;
                           INSYMBOL ;
                           if LCP -> . KLASS = PROC then
                             CALL ( FSYS , LCP )
                           else
                             ASSIGNMENT ( LCP )
                         end (* tag/ca *) ;
                 BEGINSY :
                   begin
                     STMTNEST := STMTNEST + 1 ;
                     INSYMBOL ;
                     COMPOUNDSTATEMENT ;
                     STMTNEST := STMTNEST - 1 ;
                   end (* tag/ca *) ;
                 GOTOSY :
                   begin
                     INSYMBOL ;
                     GOTOSTATEMENT
                   end (* tag/ca *) ;
                 IFSY : begin
                          INSYMBOL ;
                          IFSTATEMENT
                        end (* tag/ca *) ;
                 CASESY :
                   begin
                     STMTNEST := STMTNEST + 1 ;
                     INSYMBOL ;
                     CASESTATEMENT ;
                     STMTNEST := STMTNEST - 1
                   end (* tag/ca *) ;
                 WHILESY :
                   begin
                     INSYMBOL ;
                     WHILESTATEMENT
                   end (* tag/ca *) ;
                 REPEATSY :
                   begin
                     STMTNEST := STMTNEST + 1 ;
                     INSYMBOL ;
                     REPEATSTATEMENT ;
                     STMTNEST := STMTNEST - 1
                   end (* tag/ca *) ;
                 FORSY : begin
                           INSYMBOL ;
                           FORSTATEMENT
                         end (* tag/ca *) ;
                 BREAKSY :
                   begin
                     INSYMBOL ;
                     BREAKSTATEMENT
                   end (* tag/ca *) ;
                 CONTINUESY :
                   begin
                     INSYMBOL ;
                     CONTSTATEMENT
                   end (* tag/ca *) ;
                 RETURNSY :
                   begin
                     INSYMBOL ;
                     RETURNSTATEMENT
                   end (* tag/ca *) ;
                 WITHSY :
                   begin
                     INSYMBOL ;
                     WITHSTATEMENT
                   end (* tag/ca *)
               end (* case *) ;
               if not ( SY in [ SEMICOLON , ENDSY , ELSESY , UNTILSY ]
               ) then
                 begin
                   ERROR ( 6 ) ;
                   SKIP ( FSYS )
                 end (* then *)
             end (* then *)
         end (* STATEMENT *) ;


      procedure FRTPARMS ;

      (*********************************)
      (* THIS BORING PROCEDURE         *)
      (* GENERATES DUMMY ROUTINES      *)
      (* TO REPLACE FORTRAN ROUTINES   *)
      (* PASSED AS PROC. PARAMETERS    *)
      (*********************************)


         var PT , LOCPAR : INTEGER ;
             LLC , LCM : ADDRRANGE ;
             LCP1 : CTP ;
             FNAME : ALPHA ;

         begin (* FRTPARMS *)
           LEVEL := LEVEL + 1 ;
           OLDIC := IC ;
           while FRTPARHD <> NIL do
             with FRTPARHD -> do
               begin
                 IC := 0 ;
                 FRTRN := FALSE ;
                 PT := PROCTYPE ( FRTPARHD ) ;
                 FNAME := NAME ;
                 MKNAME ( FNAME , PFNAME , FALSE ) ;

           (********)
           (*ENT   *)
           (********)

                 WRITELN ( PRR , FNAME : 8 , MN [ 32 ] , CHR ( PT ) : 2
                           , ',' , LEVEL : 1 , ',L' , SEGSIZE : 1 ,
                           NAME : 14 , ',' , SAVEREGS : 1 , ',' ,
                           ASSEMBLE : 1 , ',' , GET_STAT : 1 , ',' ,
                           ASMVERB : 1 , ',' , DEBUG_LEV : 1 , ',' ,
                           PFNAME : 1 ) ;
                 WRITELN ( QRR , '#BGN    ' , NAME , LEVEL : 4 ) ;
                 LCP1 := PRMPTR ;
                 LC := LCAFTMST + FPSAVEAREA ;
                 while LCP1 <> NIL do
                   with LCP1 -> do
                     begin
                       if KLASS = VARS then
                         if IDTYPE <> NIL then
                           begin
                             if VKIND = FORMAL then
                               LCM := VADDR + PTRSIZE
                             else
                               LCM := VADDR + IDTYPE -> . SIZE ;
                             if LCM > LC then
                               LC := LCM ;
                           end (* then *) ;
                       LCP1 := NEXT
                     end (* with *) ;
                 ALIGN ( LC , PTRSIZE ) ;
                 LLC := LC ;
                 LCP1 := PRMPTR ;
                 while LCP1 <> NIL do
                   with LCP1 -> do
                     begin
                       if KLASS = VARS then
                         if IDTYPE <> NIL then
                           begin
                             if VKIND = FORMAL then

           (********)
           (*LOD   *)
           (********)

                               GEN3 ( 54 , ORD ( 'A' ) , LEVEL , VADDR
                                      )
                             else

           (********)
           (*LDA   *)
           (********)

                               GEN2 ( 50 , LEVEL , VADDR ) ;

           (********)
           (*STR   *)
           (********)

                             GEN3 ( 56 , ORD ( 'A' ) , LEVEL , LLC ) ;
                             LLC := LLC + PTRSIZE ;
                           end (* then *) ;
                       LCP1 := NEXT ;
                     end (* with *) ;
                 FRTRN := TRUE ;
                 LOCPAR := ( LLC - LC ) DIV 2 + 1 ;
                 PUTIC ;

           (********)
           (*CUP   *)
           (********)

                 WRITELN ( PRR , MN [ 46 ] , CHR ( PROCTYPE ( FRTPARHD
                           ) ) : 2 , ',' , LOCPAR : 1 , ',' , EXTNAME ,
                           ',' , LC : 1 ) ;
                 if KLASS = FUNC then

           (********)
           (*STR   *)
           (********)

                   GEN3 ( 56 , PT , LEVEL , FNCRSLT ) ;

           (********)
           (*RET   *)
           (********)

                 GEN1 ( 42 , PT ) ;
                 GENDEF ( SEGSIZE , LLC ) ;
                 WRITELN ( QRR , '#PROC   ' , NAME : IDLNGTH , ' ' ,
                           PFNAME : 1 , ' ' , FALSE : 1 , IC : 6 , LLC
                           : 8 , ' ' , FALSE : 1 , ' REF/MOD RATIO:' ,
                           0 : 4 , 0 : 6 , 0.0 : 10 ) ;
                 WRITELN ( QRR , '#END' ) ;
                 OLDIC := OLDIC + IC ;
                 FRTPARHD := NXTFWRD ;
               end (* with *) ;
           LEVEL := LEVEL - 1 ;
           IC := OLDIC ;
         end (* FRTPARMS *) ;


      begin (* BODY *)
        STMTNEST := 1 ;
        LISTTAG := 'N' ;
        PUTIC ;
        if FPROCP = MAINPROG then

        (********)
        (*BGN   *)
        (********)

          WRITELN ( PRR , MN [ 72 ] , ' ' , PROGNAME , ' ' , TIME : 8 ,
                    ' ' , DATE ) ;

        (********)
        (*ENT   *)
        (********)

        WRITELN ( PRR , FPROCP -> . EXTNAME , MN [ 32 ] , CHR (
                  PROCTYPE ( FPROCP ) ) : 2 , ',' , LEVEL : 1 , ',L' ,
                  SEGSIZE : 1 , FPROCP -> . NAME : 14 , ',' , SAVEREGS
                  : 1 , ',' , ASSEMBLE : 1 , ',' , GET_STAT : 1 , ',' ,
                  ASMVERB : 1 , ',' , DEBUG_LEV : 1 , ',' : 1 , FPROCP
                  -> . PFNAME : 1 ) ;
        IC := IC + 1 ;
        STIC := 0 ;

        (******************************)
        (* LENGTH OF STRING CONSTANTS *)
        (******************************)

        LOCAL_CALL := FALSE ;
        VAR_REF := 0 ;
        VAR_MOD := 0 ;
        WRITELN ( QRR , '#BGN    ' , FPROCP -> . NAME , LEVEL : 4 ) ;
        if FPROCP = MAINPROG then

        (***********************)
        (* ENTERING MAIN BLOCK *)
        (***********************)

          begin
            while FILEHEAD <> NIL do
              begin
                with FILEHEAD -> do
                  begin
                    with FILIDPTR -> do
                      begin

        (********)
        (*LDA   *)
        (********)

                        GEN2 ( 50 , 1 , VADDR ) ;

        (*************)
        (* CSP - SIO *)
        (*************)

                        GEN1 ( 30 , 31 ) ;
                        if VADDR >= FIRSTGVAR then

        (**********************)
        (* USER DEFINED FILES *)
        (**********************)

                          begin
                            NEW ( CNSTPTR , STRG ) ;
                            CNSTPTR -> . SLNGTH := 8

        (****************)
        (*OS NAME LENGTH*)
        (****************)


                                                   ;
                            for I := 1 to 8 do
                              CNSTPTR -> . SVAL [ I ] := NAME [ I ] ;

        (********)
        (*LCA   *)
        (********)

                            GEN1 ( 37 , 0 ) ;
                            LLC1 := 0 ;

        (*******************************)
        (* LENGTH CODE FOR A TEXT FILE *)
        (*******************************)

                            if not COMPTYPES ( IDTYPE , TEXTPTR ) then
                              if IDTYPE <> NIL then
                                if IDTYPE -> . FILTYPE <> NIL then
                                  LLC1 := IDTYPE -> . FILTYPE -> . SIZE
                                          ;

        (********)
        (*LDC   *)
        (********)

                            GEN2 ( 51 , 1 , LLC1 ) ;

        (*******************)
        (* FILE COMP. SIZE *)
        (*******************)

                            GEN1 ( 30 , 30 ) ;

        (*************)
        (* CSP - FDF *)
        (*************)

                          end (* then *)
                        else

        (******************************)
        (* I.E. IF VADDR < FIRSTUSERF *)
        (******************************)

                          begin
                            I := 3 ;

        (******************)
        (* CODE FOR RESET *)
        (******************)

                            if FILIDPTR = OUTPUTPTR then
                              I := 4

        (***********)
        (* REWRITE *)
        (***********)

                            else
                              if NAME [ 3 ] = 'R' then
                                I := 4

        (***********)
        (* REWRITE *)
        (***********)


                                     ;

        (*****************)
        (* CSP - RES/REW *)
        (*****************)

                            GEN1 ( 30 , I )
                          end (* else *) ;

        (*************)
        (* CSP - EIO *)
        (*************)

                        GEN1 ( 30 , 32 ) ;
                      end (* with *) ;
                  end (* with *) ;
                FILEHEAD := FILEHEAD -> . NEXTFILE
              end (* while *) ;
            if CTROPTION then
              begin
                GENLABEL ( CTRCNTLBL ) ;

        (********)
        (*CTS   *)
        (********)

                GENUJPFJP ( 38 , CTRCNTLBL ) ;
              end (* then *) ;
          end (* then *) ;

        (*************************)
        (* PROCESSING MAIN BLOCK *)
        (*************************)

        FIRSTLN := LINECNT ;
        CTRNO := CTRGEN ;

        (********************)
        (*** COUNTER HERE ***)
        (********************)

        LCMAX := LC ;
        LOOP0 . LEVEL := 0 ;
        LOOP0 . BREAKLABEL := 0 ;
        LOOP0 . BREAKUSED := FALSE ;
        LOOP0 . CONTLABEL := 0 ;
        LOOP0 . CONTUSED := FALSE ;
        GENLABEL ( LRETURN ) ;
        SUBR . RETURNLABEL := LRETURN ;
        SUBR . RETURNUSED := FALSE ;

        (***************************************************)
        (* COMPILE THE STATEMENTS WITHIN THIS BLOCK (BODY) *)
        (***************************************************)

        repeat
          repeat
            STATEMENT ( FSYS + [ SEMICOLON , ENDSY ] , LOOP0 , SUBR ) ;
          until not ( SY in STATBEGSYS ) ;
          TEST := SY <> SEMICOLON ;
          if not TEST then
            INSYMBOL
        until TEST ;
        if SY = ENDSY then
          INSYMBOL
        else
          ERROR ( 13 ) ;
        if SUBR . RETURNUSED then
          PUTLABEL ( LRETURN ) ;
        STMTNEST := 0 ;
        LISTTAG := ' ' ;
        LLP := DISPLAY [ TOP ] . FLABEL ;

        (*****************************)
        (* TEST FOR UNDEFINED LABELS *)
        (*****************************)

        while LLP <> NIL do
          with LLP -> do
            begin
              if not DEFINED then
                begin
                  PLCNT := PLCNT + 1 ;
                  WRITELN ( OUTPUT , '**** UNDEF. LABEL:' : 23 , LABVAL
                            ) ;
                  ERROR ( 168 ) ;
                end (* then *) ;
              LLP := NEXTLAB
            end (* with *) ;
        CTREMIT ( CTRPROC , CTRNO , FIRSTLN , 0 , LINECNT ) ;
        if FPROCP = MAINPROG then

        (******************)
        (* RESET COUNTERS *)
        (******************)

          begin
            CTREMIT ( CTRPROC , 0 , 0 , 0 , 0 ) ;

        (*************************)
        (* EOF FOR COUNTER TABLE *)
        (*************************)

            if ODD ( CTRCNT ) then
              CTRCNT := CTRCNT + 1 ;
            if CTROPTION then
              GENDEF ( CTRCNTLBL , CTRCNT ) ;
          end (* then *) ;

        (********)
        (*RET   *)
        (********)

        GEN1 ( 42 , PROCTYPE ( FPROCP ) ) ;
        ALIGN ( LCMAX , MXDATASZE ) ;
        if PRCODE then
          GENDEF ( SEGSIZE , LCMAX ) ;
        CALL_LVL [ LOCAL_CALL ] := CALL_LVL [ LOCAL_CALL ] + 1 ;
        WRITE ( QRR , '#PROC   ' , FPROCP -> . NAME : IDLNGTH , ' ' ,
                FPROCP -> . PFNAME : 1 , ' ' , LOCAL_CALL : 1 , ' ' ,
                IC + ( STIC DIV 4 ) : 1 , ' ' , LCMAX : 1 , ' ' ,
                FLIPDEBUG : 1 , ' REF/MOD RATIO:' , VAR_MOD : 4 ,
                VAR_MOD + VAR_REF : 6 ) ;
        if ( VAR_MOD + VAR_REF ) = 0 then
          WRITELN ( QRR , 0.0 : 10 )
        else
          WRITELN ( QRR , VAR_MOD / ( VAR_MOD + VAR_REF ) : 10 ) ;
        WRITELN ( QRR , '#END' ) ;
        OLDIC := OLDIC + IC ;
        IC := OLDIC ;

        (**************************)
        (* DISPLAY CUMULATIVE IC  *)
        (**************************)

        HP := TRUE ;
        if FRTPARHD <> NIL then
          FRTPARMS ;
        if FPROCP = MAINPROG then

        (********)
        (*STP   *)
        (********)

          GEN0 ( 43 ) ;
      end (* BODY *) ;


   begin (* BLOCK *)
     IC := 0 ;
     GENLABEL ( SEGSIZE ) ;
     CONSTLC := - 1 ;
     FWRDPRCL := NIL ;
     DEC_ORDER := 0 ;
     repeat
       while SY in [ LABELSY , CONSTSY , TYPESY , VARSY ] do
         begin
           LSY := SY ;
           INSYMBOL ;
           case LSY of
             LABELSY :
               begin
                 LABELDECLARATION ;
                 if DEC_ORDER >= 1 then
                   EXTUSED := TRUE ;
                 DEC_ORDER := 1 ;
               end (* tag/ca *) ;
             CONSTSY :
               begin
                 CONSTDECLARATION ;
                 if DEC_ORDER >= 2 then
                   EXTUSED := TRUE ;
                 DEC_ORDER := 2 ;
               end (* tag/ca *) ;
             TYPESY :
               begin
                 TYPEDECLARATION ;
                 if DEC_ORDER >= 3 then
                   EXTUSED := TRUE ;
                 DEC_ORDER := 3 ;
               end (* tag/ca *) ;
             VARSY : begin
                       VARDECLARATION ;
                       if DEC_ORDER >= 4 then
                         EXTUSED := TRUE ;
                       DEC_ORDER := 4 ;
                     end (* tag/ca *) ;
           end (* case *) ;
         end (* while *) ;
       if CONSTLC >= 0 then

     (***********************************)
     (* A CONSTANTS BLOCK WAS GENERATED *)
     (***********************************)

         begin
           CONSTLC := - 1 ;
           WRITELN ( PRR , MN [ 75 ] ) ;
         end (* then *) ;
       if DEBUG_LEV > 0 then
         PRNTSYMBL ( NIL ) ;

     (**************************)
     (* PRINT HEAP TYPE DEFNS. *)
     (**************************)

       while SY in [ PROCSY , FUNCSY ] do
         begin
           LSY := SY ;
           INSYMBOL ;
           PROCDECLARATION ( LSY )
         end (* while *) ;
       if SY <> BEGINSY then
         begin
           ERROR ( 18 ) ;
           SKIP ( FSYS )
         end (* then *)
     until SY in STATBEGSYS ;
     if SY = BEGINSY then
       INSYMBOL
     else
       ERROR ( 17 ) ;
     while FWRDPRCL <> NIL do
       begin
         WRITELN ( '**** MISSING FORWARD DECLARED PROCEDURE:' : 50 ,
                   FWRDPRCL -> . NAME : 14 ) ;
         PLCNT := PLCNT + 1 ;
         FWRDPRCL := FWRDPRCL -> . NXTFWRD
       end (* while *) ;
     repeat
       BODY ( FSYS + [ CASESY ] ) ;
       if SY <> FSY then
         begin
           ERROR ( 6 ) ;
           SKIP ( FSYS + [ FSY ] )
         end (* then *)
     until ( SY = FSY ) or ( SY in BLOCKBEGSYS ) ;
   end (* BLOCK *) ;



procedure PROGRAMME ( FSYS : SETOFSYS ) ;

   var LFPTR : FRECPTR ;
       LCP : CTP ;
       I , J : INTEGER ;

   begin (* PROGRAMME *)
     CALL_LVL [ FALSE ] := 0 ;
     CALL_LVL [ TRUE ] := 0 ;
     if SY = PROGSY then
       begin
         INSYMBOL ;
         if SY <> IDENT then
           ERROR ( 2 ) ;
         PROGNAME := ID ;
         INSYMBOL ;
         if not ( SY in [ LPARENT , SEMICOLON ] ) then
           ERROR ( 14 ) ;
         if SY = LPARENT then
           begin
             PRTERR := FALSE ;

     (**************************)
     (* IGNORE BAD PROG. PARMS *)
     (**************************)

             repeat
               INSYMBOL ;
               if SY = IDENT then
                 begin
                   SEARCHID ( [ VARS ] , LCP ) ;
                   if LCP <> NIL then
                     if LCP -> . IDTYPE = TEXTPTR then
                       begin
                         NEW ( LFPTR ) ;
                         with LFPTR -> do
                           begin
                             FILIDPTR := LCP ;
                             NEXTFILE := FILEHEAD ;
                           end (* with *) ;
                         FILEHEAD := LFPTR ;
                       end (* then *) ;
                   INSYMBOL ;
                   if not ( SY in [ COMMA , RPARENT ] ) then
                     ERROR ( 20 )
                 end (* then *)
               else
                 ERROR ( 2 )
             until SY <> COMMA ;
             if SY <> RPARENT then
               ERROR ( 4 ) ;
             PRTERR := TRUE ;
             INSYMBOL
           end (* then *) ;
         if SY <> SEMICOLON then
           ERROR ( 14 )
         else
           INSYMBOL ;
       end (* then *) ;
     NEW ( MAINPROG , PROC , DECLARED ) ;
     with MAINPROG -> do
       begin
         NAME := '$MAINBLK    ' ;
         EXTNAME := '$MAINBLK' ;
         PFNAME := 0 ;
         IDTYPE := NIL ;
         FWDECL := FALSE ;
         PFLEV := 0 ;
         KLASS := PROC ;
         PFDECKIND := DECLARED ;
         NEXT := NIL ;
         NEXT_IN_BKT := NIL ;
         DECL_LEV := 0 ;
         FRTRN := FALSE ;
         EXTRN := FALSE ;
         if XLINK then
           begin
             EXTRN := TRUE ;
             EXTNAME [ 1 ] := '#'
           end (* then *) ;
       end (* with *) ;
     if DEBUG_LEV > 0 then
       WRITELN ( QRR , '% $MAINBLK  0' ) ;
     repeat
       BLOCK ( FSYS , PERIOD , MAINPROG ) ;
       if SY <> PERIOD then
         ERROR ( 21 )
     until SY = PERIOD ;
     WRITELN ( QRR , '#HLT  CALL_RATIO' , CALL_LVL [ TRUE ] : 4 ,
               CALL_LVL [ FALSE ] : 4 , CALL_LVL [ TRUE ] + CALL_LVL [
               FALSE ] : 4 ) ;
     if ERRINX > 0 then
       PRINTERROR ;
     if GET_STAT then
       begin

     (*********************************)
     (* PRINT SYMBOL TABLE STATISTICS *)
     (*********************************)

         WRITELN ( QRR , '&SYT1 ' , FENT_CNT : 1 , ' ' , SF_CNT : 1 ,
                   ' ' , SF_TOT : 1 , ' ' , WE_CNT : 1 , ' ' , RE_CNT :
                   1 , ' ' , WS_CNT : 1 ) ;
         WRITE ( QRR , '&SYT2' ) ;
         for I := 0 to MAXLEVEL do
           WRITE ( QRR , ' ' , PROC_CNT [ I ] : 1 ) ;
         WRITELN ( QRR ) ;
         WRITE ( QRR , '&SYT3' ) ;
         for I := 0 to MAXLEVEL do
           WRITE ( QRR , ' ' , ENT_CNT [ I ] : 1 ) ;
         for I := 0 to MAXLEVEL do
           begin
             WRITELN ( QRR ) ;
             WRITE ( QRR , '&SYT4' ) ;
             for J := 0 to DISPLIMIT do
               WRITE ( QRR , ' ' , LU_CNT [ I , J ] : 1 ) ;
           end (* for *) ;
         for I := 1 to 10 do
           begin
             WRITELN ( QRR ) ;
             WRITE ( QRR , '&SYT5' ) ;
             for J := 1 to 10 do
               WRITE ( QRR , ' ' , WLU_CNT [ I , J ] : 1 ) ;
           end (* for *) ;
       end (* then *) ;
   end (* PROGRAMME *) ;



procedure ENTERSTDTYPES ;

   const INTTYP : STRUCTURE =
         ( INTSIZE , INTSIZE , SCALAR , STANDARD ) ;
         REALTYPE : STRUCTURE =
         ( REALSIZE , REALSIZE , SCALAR , STANDARD ) ;
         CHARTYPE : STRUCTURE =
         ( CHARSIZE , CHARSIZE , SCALAR , STANDARD ) ;
         BOOLTYPE : STRUCTURE =
         ( BOOLSIZE , BOOLSIZE , SCALAR , DECLARED , NIL ) ;
         NILTYPE : STRUCTURE =
         ( PTRSIZE , PTRSIZE , POINTER , NIL ) ;
         TEXTTYPE : STRUCTURE =
         ( 0 , PTRSIZE , FILES , NIL ) ;
         ALFATYPE : STRUCTURE =
         ( ALFALNGTH , CHARSIZE , ARRAYS , NIL , NIL ) ;
         ALFAINX : STRUCTURE =
         ( INTSIZE , INTSIZE , SUBRANGE , NIL , ( TRUE , 1 ) , ( TRUE ,
           ALFALNGTH ) ) ;
         UTYP : IDENTIFIER =
         ( BLANKID , NIL , NIL , NIL , 0 , TYPES ) ;
         UCST : IDENTIFIER =
         ( BLANKID , NIL , NIL , NIL , 0 , KONST , ( TRUE , 1 ) ) ;
         UVAR : IDENTIFIER =
         ( BLANKID , NIL , NIL , NIL , 0 , VARS , ACTUAL , 0 , 0 ) ;
         UFLD : IDENTIFIER =
         ( BLANKID , NIL , NIL , NIL , 0 , FIELD , 0 , NIL ) ;
         UPF : IDENTIFIER =
         ( BLANKID , NIL , NIL , NIL , 0 , PROC , DECLARED , 0 , 0 ,
           NIL , NIL , ACTUAL , FALSE , FALSE , FALSE , '$UNK_PF ' ) ;
         UREC : STRUCTURE =
         ( 1 , 1 , RECORDS , NIL , NIL , 0 , 0 ) ;

   var SP : STP ;

   begin (* ENTERSTDTYPES *)

     (********************)
     (*****************  *)
     (********************)

     NEW ( INTPTR ) ;
     INTPTR -> := INTTYP ;

     (*********)
     (*INTEGER*)
     (*********)

     NEW ( REALPTR ) ;
     REALPTR -> := REALTYPE ;

     (********)
     (*REAL  *)
     (********)

     NEW ( CHARPTR ) ;
     CHARPTR -> := CHARTYPE ;

     (********)
     (*CHAR  *)
     (********)

     NEW ( BOOLPTR ) ;
     BOOLPTR -> := BOOLTYPE ;

     (*********)
     (*BOOLEAN*)
     (*********)

     NEW ( NILPTR ) ;
     NILPTR -> := NILTYPE ;

     (********)
     (*NIL   *)
     (********)

     NEW ( TEXTPTR ) ;
     TEXTPTR -> := TEXTTYPE ;

     (********)
     (*TEXT  *)
     (********)

     with TEXTPTR -> do
       begin
         FILTYPE := CHARPTR ;
         SIZE := CHARSIZE + FILHDRSIZE
       end (* with *) ;
     NEW ( ALFAPTR ) ;
     ALFAPTR -> := ALFATYPE ;

     (********)
     (*ALFA  *)
     (********)

     with ALFAPTR -> do
       begin
         AELTYPE := CHARPTR ;
         NEW ( INXTYPE ) ;
         INXTYPE -> := ALFAINX ;
       end (* with *) ;
     NEW ( UTYPPTR ) ;
     UTYPPTR -> := UTYP ;
     NEW ( UVARPTR ) ;
     UVARPTR -> := UVAR ;
     NEW ( UFLDPTR ) ;
     UFLDPTR -> := UFLD ;
     NEW ( UPRCPTR ) ;
     UPRCPTR -> := UPF ;
     GENLABEL ( UPRCPTR -> . PFNAME ) ;
     NEW ( UFCTPTR ) ;
     UFCTPTR -> := UPF ;
     GENLABEL ( UFCTPTR -> . PFNAME ) ;
     NEW ( SP ) ;
     SP -> := UREC ;
     UFLDPTR -> . OWNER := SP ;
   end (* ENTERSTDTYPES *) ;



procedure ENTSTDNAMES ;

   var CP , CP1 : CTP ;
       I , J : INTEGER ;

   const NA : array [ 1 .. NPDW ] of ALPHA =
         ( 'FALSE       ' , 'TRUE        ' , '            ' ,
           'PAGE        ' , 'GET         ' , 'PUT         ' ,
           'RESET       ' , 'REWRITE     ' , 'READ        ' ,
           'WRITE       ' , 'PACK        ' , 'UNPACK      ' ,
           'NEW         ' , 'RELEASE     ' , 'READLN      ' ,
           'WRITELN     ' , 'MARK        ' , 'TRAP        ' ,
           'EXIT        ' , 'ABS         ' , 'SQR         ' ,
           'TRUNC       ' , 'ROUND       ' , 'ORD         ' ,
           'CHR         ' , 'PRED        ' , 'SUCC        ' ,
           'CLOCK       ' , 'EOF         ' , 'EOLN        ' ,
           'ODD         ' , 'EOL         ' , 'EOT         ' ,
           'TRACE       ' , '            ' , '            ' ,
           '            ' , '            ' , 'INPUT       ' ,
           'OUTPUT      ' , 'PRD         ' , 'PRR         ' ,
           'QRD         ' , 'QRR         ' , 'DATE        ' ,
           'TIME        ' , 'MESSAGE     ' , 'SKIP        ' ,
           'LINELIMIT   ' , 'CARD        ' , 'EXPO        ' ,
           'SIN         ' , 'COS         ' , 'EXP         ' ,
           'SQRT        ' , 'LN          ' , 'ARCTAN      ' ) ;
         XNA : array [ 52 .. 57 ] of array [ 1 .. EXTNAMSZ ] of CHAR =
         ( 'DSIN    ' , 'DCOS    ' , 'DEXP    ' , 'DSQRT   ' ,
           'DLOG    ' , 'DATAN   ' ) ;

   begin (* ENTSTDNAMES *)

     (****************)
     (******         *)
     (****************)

     NEW ( CP , TYPES ) ;

     (****************)
     (*INTEGER       *)
     (****************)

     with CP -> do
       begin
         NAME := 'INTEGER     ' ;
         IDTYPE := INTPTR ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;
     NEW ( CP , TYPES ) ;

     (****************)
     (*REAL          *)
     (****************)

     with CP -> do
       begin
         NAME := 'REAL        ' ;
         IDTYPE := REALPTR ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;
     NEW ( CP , TYPES ) ;

     (****************)
     (*CHAR          *)
     (****************)

     with CP -> do
       begin
         NAME := 'CHAR        ' ;
         IDTYPE := CHARPTR ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;
     NEW ( CP , TYPES ) ;

     (****************)
     (*BOOLEAN       *)
     (****************)

     with CP -> do
       begin
         NAME := 'BOOLEAN     ' ;
         IDTYPE := BOOLPTR ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;
     NEW ( CP , TYPES ) ;

     (****************)
     (*CHAR          *)
     (****************)

     with CP -> do
       begin
         NAME := 'TEXT        ' ;
         IDTYPE := TEXTPTR ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;
     NEW ( CP , TYPES ) ;

     (****************)
     (*ALFA          *)
     (****************)

     with CP -> do
       begin
         NAME := 'ALFA        ' ;
         IDTYPE := ALFAPTR ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;
     NEW ( CP , KONST ) ;

     (****************)
     (*MAXINT        *)
     (****************)

     with CP -> do
       begin
         NAME := 'MAXINT      ' ;
         IDTYPE := INTPTR ;
         KLASS := KONST ;
         VALUES . IVAL := MAXINT ;
       end (* with *) ;
     ENTERID ( CP ) ;
     CP1 := NIL ;
     for I := 1 to 2 do
       begin
         NEW ( CP , KONST ) ;

     (****************)
     (*FALSE,TRUE    *)
     (****************)

         with CP -> do
           begin
             NAME := NA [ I ] ;
             IDTYPE := BOOLPTR ;
             NEXT := CP1 ;
             VALUES . IVAL := I - 1 ;
             KLASS := KONST
           end (* with *) ;
         ENTERID ( CP ) ;
         CP1 := CP
       end (* for *) ;
     BOOLPTR -> . FCONST := CP ;
     NEW ( CP , KONST ) ;

     (***************)
     (*NIL          *)
     (***************)

     with CP -> do
       begin
         NAME := 'NIL         ' ;
         IDTYPE := NILPTR ;
         NEXT := NIL ;
         VALUES . IVAL := 0 ;
         KLASS := KONST
       end (* with *) ;
     ENTERID ( CP ) ;
     for I := 39 to 44 do
       begin
         NEW ( CP , VARS ) ;

     (****************)
     (*INPUT,OUTPUT  *)
     (****************)

         with CP -> do

     (****************)
     (*PRD,PRR       *)
     (****************)

           begin
             NAME := NA [ I ] ;
             IDTYPE := TEXTPTR ;

     (****************)
     (*QRD,QRR       *)
     (****************)

             KLASS := VARS ;
             VKIND := ACTUAL ;
             NEXT := NIL ;
             VLEV := 1 ;
             VADDR := FIRSTFILBUF + ( I - 39 ) * ( FILHDRSIZE + PTRSIZE
                      ) ;
             if I <= 40 then
               if I = 39 then
                 INPUTPTR := CP
               else
                 OUTPUTPTR := CP ;
           end (* with *) ;
         ENTERID ( CP )
       end (* for *) ;
     for I := 45 to 46 do

     (********************)
     (*DATE, TIME        *)
     (********************)

       begin
         NEW ( CP , VARS ) ;
         with CP -> do
           begin
             NAME := NA [ I ] ;
             IDTYPE := ALFAPTR ;
             KLASS := VARS ;
             VKIND := ACTUAL ;
             NEXT := NIL ;
             VLEV := 1 ;
             VADDR := TIMEDATELOC + ( I - 45 ) * ALFALNGTH ;
           end (* with *) ;
         ENTERID ( CP )
       end (* for *) ;
     NEW ( CP , VARS ) ;

     (******************************************************)
     (*OSPARM PTR                                          *)
     (*THE REST OF THIS CODE IS TO DEFINE:                 *)
     (* VAR:  OSPARM: @ RECORD                             *)
     (*                 LENGTH: INTEGER;                   *)
     (*                 STRING: ARRAY[1..64] OF CHAR       *)
     (*                 END;                               *)
     (*                                                    *)
     (******************************************************)

     with CP -> do
       begin
         NAME := 'OSPARM      ' ;
         KLASS := VARS ;
         VKIND := ACTUAL ;
         NEXT := NIL ;
         VLEV := 1 ;
         VADDR := OSPARMLOC ;
         ENTERID ( CP ) ;
         NEW ( IDTYPE , POINTER ) ;
         with IDTYPE -> do
           begin
             SIZE := PTRSIZE ;
             ALN := PTRSIZE ;
             FORM := POINTER ;
             NEW ( ELTYPE , RECORDS ) ;

     (*************************)
     (*TYPE OF THE PARM RECORD*)
     (*************************)

             with ELTYPE -> do
               begin
                 SIZE := INTSIZE + STRGLNGTH * CHARSIZE ;
                 ALN := PTRSIZE ;
                 FORM := RECORDS ;
                 RECVAR := NIL ;
                 FLD_DISP_LEV := - 1 ;
                 NO_FLDS := 2 ;
                 NEW ( FSTFLD , FIELD ) ;
                 with FSTFLD -> do
                   begin
                     NAME := 'LENGTH      ' ;
                     IDTYPE := INTPTR ;
                     FLDADDR := 0 ;
                     KLASS := FIELD ;
                     TOP := TOP + 1 ;

     (**********************************)
     (* FIELDS ENTERED AT HIGHER SCOPE *)
     (**********************************)

                     ENTERID ( FSTFLD ) ;
                     OWNER := CP -> . IDTYPE -> . ELTYPE ;
                     NEW ( NEXT , FIELD ) ;
                     with NEXT -> do
                       begin
                         NAME := 'STRING      ' ;
                         FLDADDR := PTRSIZE ;
                         NEXT := NIL ;
                         KLASS := FIELD ;
                         NEW ( IDTYPE , ARRAYS ) ;
                         with IDTYPE -> do
                           begin
                             SIZE := STRGLNGTH * CHARSIZE ;
                             ALN := CHARSIZE ;
                             FORM := ARRAYS ;
                             AELTYPE := CHARPTR ;
                             NEW ( INXTYPE , SUBRANGE ) ;
                             with INXTYPE -> do
                               begin
                                 FORM := SUBRANGE ;
                                 RANGETYPE := INTPTR ;
                                 MIN . IVAL := 1 ;
                                 MAX . IVAL := STRGLNGTH ;
                               end (* with *) ;
                           end (* with *) ;
                       end (* with *) ;
                     ENTERID ( NEXT ) ;
                     NEXT -> . OWNER := CP -> . IDTYPE -> . ELTYPE ;
                     TOP := TOP - 1 ;
                   end (* with *) ;
               end (* with *) ;
           end (* with *) ;
       end (* with *) ;
     NEW ( CP1 , VARS ) ;

     (***************************************)
     (*PARAMETER OF PREDECLARED FUNCTIONS   *)
     (***************************************)

     with CP1 -> do
       begin
         NAME := BLANKID ;
         IDTYPE := REALPTR ;
         KLASS := VARS ;
         VKIND := ACTUAL ;
         NEXT := NIL ;
         VLEV := 1 ;
         VADDR := LCAFTMST + FPSAVEAREA ;
       end (* with *) ;
     for I := 4 to 34 do

     (****************)
     (*PAGE,GET...TRC*)
     (****************)

       begin
         NEW ( CP , PROC , STANDARD ) ;

     (*******************)
     (*GET,PUT,RESET    *)
     (*******************)

         with CP -> do

     (*******************)
     (*REWRITE,READ     *)
     (*******************)

           begin
             NAME := NA [ I ] ;
             IDTYPE := NIL ;

     (*******************)
     (*WRITE,PACK       *)
     (*******************)

             NEXT := NIL ;
             KEY := I - 4 ;

     (*******************)
     (*UNPACK,PACK      *)
     (*******************)

             if I = 31

     (********)
     (*ODD   *)
     (********)


             then
               KEY := 33 ;
             if I <= 19 then
               KLASS := PROC
             else
               KLASS := FUNC ;
             if I = 34 then
               KLASS := PROC ;
             PFDECKIND := STANDARD ;

     (*******************)
     (*READLN,WRITELN   *)
     (*MARK,RELEASE,TRAP*)
     (*******************)

           end (* with *) ;
         ENTERID ( CP )
       end (* for *) ;
     for I := 52 to 57 do

     (******************************)
     (* SIN,COS,EXP,SQRT,LN,ARCTAN *)
     (******************************)

       begin
         NEW ( CP , FUNC , DECLARED ) ;
         with CP -> do
           begin
             NAME := NA [ I ] ;
             IDTYPE := REALPTR ;
             NEXT := NIL ;
             PRMPTR := CP1 ;
             FWDECL := FALSE ;
             EXTRN := FALSE ;
             FRTRN := TRUE ;
             KLASS := FUNC ;
             PFDECKIND := DECLARED ;
             PFKIND := ACTUAL ;
             PFLEV := 0 ;
             PFNAME := 0 ;
             EXTNAME := XNA [ I ] ;
           end (* with *) ;
         ENTERID ( CP ) ;
       end (* for *) ;
     for I := 47 to 51 do
       begin
         NEW ( CP , PROC , STANDARD ) ;
         with CP -> do
           begin
             NAME := NA [ I ] ;
             IDTYPE := NIL ;
             NEXT := NIL ;
             if I <= 49 then
               KLASS := PROC
             else
               KLASS := FUNC ;
             KEY := I - 12 ;
             PFDECKIND := STANDARD ;
           end (* with *) ;
         ENTERID ( CP ) ;
       end (* for *) ;
     NEW ( CP , PROC , DECLARED ) ;

     (*******************)
     (*SNAPSHOT         *)
     (*******************)

     with CP -> do
       begin
         NAME := 'SNAPSHOT    ' ;
         IDTYPE := NIL ;
         FRTRN := FALSE ;
         FWDECL := FALSE ;
         EXTRN := TRUE ;
         PFLEV := 0 ;
         PFNAME := 0 ;
         KLASS := PROC ;
         PFDECKIND := DECLARED ;
         PFKIND := ACTUAL ;
         EXTNAME := 'SNAPSHOT' ;
         NEXT := NIL ;
       end (* with *) ;
     ENTERID ( CP ) ;
     NEW ( CP -> . PRMPTR , VARS ) ;

     (*******************************)
     (* FIRST PARAMETER OF SNAPSHOT *)
     (*******************************)

     NEW ( CP1 , VARS ) ;

     (********************************)
     (* SECOND PARAMETER OF SNAPSHOT *)
     (********************************)

     with CP1 -> do
       begin
         IDTYPE := INTPTR ;
         KLASS := VARS ;
         VKIND := ACTUAL ;
         NEXT := NIL ;
         VLEV := 1 ;
         VADDR := LCAFTMST + FPSAVEAREA + INTSIZE
       end (* with *) ;
     with CP -> . PRMPTR -> do
       begin
         IDTYPE := INTPTR ;
         KLASS := VARS ;
         VKIND := ACTUAL ;
         NEXT := CP1 ;
         VLEV := 1 ;
         VADDR := LCAFTMST + FPSAVEAREA
       end (* with *) ;
   end (* ENTSTDNAMES *) ;



procedure INITSCALARS ;

   begin (* INITSCALARS *)
     FWPTR := NIL ;
     STMTNEST := 0 ;
     LISTTAG := ' ' ;
     LIST := TRUE ;
     PRCODE := TRUE ;
     PRTERR := TRUE ;
     ERRINX := 0 ;
     CONSTLC := - 1 ;
     HP := FALSE ;
     IC := 0 ;
     INTLABEL := 0 ;
     FILEHEAD := NIL ;
     LC := FIRSTGVAR ;

     (***********************************)
     (*ADR. OF THE FIRST GLOBAL VARIABLE*)
     (* NOTE IN THE ABOVE RESERVATION O *)
     (*F BUFFER STORE FOR TEXT FILES    *)
     (***********************************)

     OLDIC := 0 ;
     IC := 0 ;
     EOL := TRUE ;
     LINECNT := 0 ;
     CH := ' ' ;
     CHCNT := 0 ;
     PAGECNT := 0 ;
     PLCNT := PAGESIZE ;

     (****************************)
     (* GENERATES FIRST HEADLINE *)
     (****************************)

     LMARGIN := 0 ;
     RMARGIN := 80 ;
     BUFEND := 81 ;
     OLDLN := 0 ;
     MWARN := FALSE ;
     LSTOP := '#' ;
     GLOBTESTP := NIL ;
     OPEN_RECORD := NIL ;
     LASTLINELISTED := 0 ;
     PROGNAME := '$MAINBLK    ' ;
     MXINT10 := MAXINT DIV 10 ;
     PROCLAB := 0 ;
     ERRORCNT := 0 ;
     WARNCNT := 0 ;
     ASSEMBLE := FALSE ;
     NESTCOMM := FALSE ;
     ERRKIND := 'E' ;
     SAVEREGS := TRUE ;
     SAVEFPRS := TRUE ;
     DEBUG := TRUE ;
     DEBUG_LEV := 2 ;
     ASSIGN := FALSE ;
     FLIPDEBUG := FALSE ;
     EXTUSED := FALSE ;
     WARNING := TRUE ;
     DOTFLG := FALSE ;
     NOPACKING := FALSE ;
     PACKDATA := FALSE ;
     XLINK := FALSE ;

     (*************************)
     (*GENERATES UNIQUE NAMES *)
     (*************************)

     PRNTTYPHD := NIL ;
     PRNTTYNO := 0 ;
     FRTPARHD := NIL ;
     XLABNO := 0 ;
     GET_STAT := TRUE ;
     ASMVERB := FALSE ;
     CTRCNT := 0 ;
     CTROPTION := FALSE ;
     FENT_CNT := 0 ;
     SF_CNT := 0 ;
     SF_TOT := 0 ;
     WE_CNT := 0 ;
     RE_CNT := 0 ;
     WS_CNT := 0 ;
   end (* INITSCALARS *) ;



procedure INITTABLES ;

   var K : BKT_RNG ;
       I , J : INTEGER ;


   procedure RATORS ;

      var I : INTEGER ;
          CH : CHAR ;

      begin (* RATORS *)
        for CH := ' ' to '9' do
          UPSHIFT [ CH ] := CH ;

        (********)
        (*UPL   *)
        (* NOP  *)
        (*follo *)
        (*wing  *)
        (*stmt, *)
        (* to a *)
        (*void  *)
        (*upshi *)
        (*fting *)
        (* tild *)
        (*e cha *)
        (*r, wh *)
        (*ich   *)
        (*      *)
        (*      *)
        (* "DE" *)
        (*      *)
        (*is in *)
        (* the  *)
        (*range *)
        (* 'a'. *)
        (*.'z': *)
        (* FOR  *)
        (*CH := *)
        (* 'a'  *)
        (*TO 'z *)
        (*' DO  *)
        (*UPSHI *)
        (*FT[CH *)
        (*] :=  *)
        (*CHR(O *)
        (*RD(CH *)
        (*) + 6 *)
        (*4) ;  *)
        (*UPL   *)
        (********)

        for I := 0 to ORDCHMAX do
          SOP [ CHR ( I ) ] := ILLEGCH ;
        for CH := 'A' to 'I' do
          SOP [ CH ] := ATOZCH ;
        for CH := 'J' to 'R' do
          SOP [ CH ] := ATOZCH ;
        for CH := 'S' to 'Z' do
          SOP [ CH ] := ATOZCH ;
        for CH := 'a' to 'i' do
          begin
            SOP [ CH ] := ATOZCH ;
            UPSHIFT [ CH ] := CHR ( ORD ( CH ) + 64 )
          end (* for *) ;

        (********)
        (*UPL   *)
        (********)

        for CH := 'j' to 'r' do
          begin
            SOP [ CH ] := ATOZCH ;
            UPSHIFT [ CH ] := CHR ( ORD ( CH ) + 64 )
          end (* for *) ;

        (********)
        (*UPL   *)
        (********)

        for CH := 's' to 'z' do
          begin
            SOP [ CH ] := ATOZCH ;
            UPSHIFT [ CH ] := CHR ( ORD ( CH ) + 64 )
          end (* for *) ;

        (********)
        (*UPL   *)
        (********)

        for CH := '0' to '9' do
          SOP [ CH ] := NUMCH ;
        SOP [ '"' ] := DQUOTCH ;
        SOP [ CHLBRACE ] := LBRACE ;
        SOP [ '#' ] := SKIPCH ;
        SOP [ '$' ] := DOLLARCH ;
        SOP [ '''' ] := QUOTCH ;
        SOP [ '(' ] := LPARCH ;
        SOP [ ')' ] := RPARCH ;
        SOP [ ',' ] := SPECH ;
        SOP [ '.' ] := DOTCH ;
        SOP [ ':' ] := COLONCH ;
        SOP [ ';' ] := SPECH ;
        SOP [ '@' ] := SPECH ;
        SOP [ '[' ] := SPECH ;
        SOP [ '|' ] := SPECH ;
        SOP [ ']' ] := SPECH ;
        SOP [ '^' ] := SPECH ;
        SOP [ '_' ] := UNDSCH ;
        SOP [ '+' ] := PLUS ;
        SOP [ '-' ] := MINUS ;
        SOP [ '*' ] := MUL ;
        SOP [ '/' ] := RDIV ;
        SOP [ '=' ] := EQOP ;
        SOP [ '<' ] := LTOP ;
        SOP [ '>' ] := GTOP ;
        SOP [ '|' ] := OROP ;
        SOP [ '&' ] := ANDOP ;

        (**************************************************************)
        (* Note: SSY array should be defined as full range of         *)
        (*                                                            *)
        (*         characters, but it's ok as is for now because it i *)
        (*s                                                           *)
        (*         currently indexed only for specific characters.    *)
        (*                                                            *)
        (*         It it were indexed by higher chars (e.g. backslash *)
        (* or                                                         *)
        (*         curly brackets) in the future, its definition woul *)
        (*d                                                           *)
        (*         have to change. Currently, SPECH does not include  *)
        (*any                                                         *)
        (*         of these higher chars. - D.E. 02feb2007            *)
        (**************************************************************)

        SSY [ '+' ] := ADDOP ;
        SSY [ '-' ] := ADDOP ;
        SSY [ '*' ] := MULOP ;
        SSY [ '/' ] := MULOP ;
        SSY [ '(' ] := LPARENT ;
        SSY [ ')' ] := RPARENT ;
        SSY [ CHR ( 173 ) ] := LBRACK ;
        SSY [ CHR ( 189 ) ] := RBRACK ;
        SSY [ ',' ] := COMMA ;
        SSY [ ':' ] := COLON ;
        SSY [ '|' ] := ADDOP ;
        SSY [ '&' ] := MULOP ;
        SSY [ '<' ] := RELOP ;
        SSY [ '>' ] := RELOP ;
        SSY [ '=' ] := RELOP ;
        SSY [ '@' ] := ARROW ;
        SSY [ CHR ( 74 ) ] := ARROW ;
        SSY [ '^' ] := NOTSY ;
        SSY [ ';' ] := SEMICOLON ;
        SSY [ '.' ] := PERIOD ;
      end (* RATORS *) ;


   begin (* INITTABLES *)
     RATORS ;
     for I := 0 to MAXERRLOG do
       ERRLOG [ I ] := [ ] ;

     (*****************)
     (*CLEAR ERROR LOG*)
     (*****************)

     for K := 0 to MAX_BKT do
       BUCKET [ K ] := NIL ;
     for I := 0 to MAXLEVEL do
       begin
         PROC_CNT [ I ] := 0 ;
         ENT_CNT [ I ] := 0 ;
         for J := 0 to DISPLIMIT do
           LU_CNT [ I , J ] := 0 ;
       end (* for *) ;
     for I := 1 to 10 do
       for J := 1 to 10 do
         WLU_CNT [ I , J ] := 0 ;
     PROC_CNT [ 1 ] := 1 ;
   end (* INITTABLES *) ;



begin (* HAUPTPROGRAMM *)

  (************)
  (*INITIALIZE*)
  (************)
  (************)

  INITSCALARS ;
  INITTABLES ;

  (******************************************)
  (*ENTER STANDARD NAMES AND STANDARD TYPES:*)
  (******************************************)
  (******************************************)

  LEVEL := 0 ;
  TOP := 0 ;
  with DISPLAY [ 0 ] do
    begin
      OCCUR := BLCK ;
      FLABEL := NIL ;
    end (* with *) ;
  ENTERSTDTYPES ;
  ENTSTDNAMES ;
  TOP := 1 ;
  LEVEL := 1 ;
  with DISPLAY [ 1 ] do
    begin
      OCCUR := BLCK ;
      FLABEL := NIL ;
    end (* with *) ;
  GET_STAT := FALSE ;

  (*************************************************)
  (*set options passed as parameter to the compiler*)
  (*************************************************)
  (*************************************************)

  if OSPARM <> NIL then
    with OSPARM -> do
      begin
        CH

  "**********"
  "LINEBUF[1]"
  "**********"


        := CHLBRACE ;
        LINEBUF [ 2 ] := '$' ;
        if LENGTH > 64 then
          LENGTH := 64 ;
        for CHCNT := 1 to LENGTH do
          LINEBUF [ CHCNT + 2 ] := STRING [ CHCNT ] ;

  (*********************************************************)
  (*THE REST OF THE LINE DOES NOT HAVE TO BE CLEARED BUT...*)
  (*********************************************************)

        for CHCNT := LENGTH to 77 do
          LINEBUF [ CHCNT + 3 ] := ' ' ;
        LINEBUF [ LENGTH + 3 ] := CHRBRACE ;
        LINEBUF [ LENGTH + 4 ] := '#' ;
        EOL := FALSE ;
        CHCNT := 1 ;
        LASTCOL := LENGTH + 3 ;
      end (* with *) ;

  (**********)
  (*COMPILE:*)
  (**********)
  (**********)

  CTIME := CLOCK ( 0 ) ;

  (*****************************************)
  (* FIRST HEADLINE PRINTED BY 'ENDOFLINE' *)
  (*****************************************)

  INSYMBOL ;
  PROGRAMME ( BLOCKBEGSYS + STATBEGSYS - [ CASESY ] ) ;

  (***********************************)
  (* PRINT POST COMPILATION MESSAGES *)
  (***********************************)

  GOODBYE ;
end (* HAUPTPROGRAMM *) .
