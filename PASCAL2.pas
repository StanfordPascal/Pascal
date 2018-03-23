program PCODE_TRANSLATOR ( INPUT , OUTPUT , OBJCODE , ASMOUT , TRACEF )
                           ;

(********************************************************************)
(*                                                                  *)
(*  aktuelle Themen:                                                *)
(*                                                                  *)
(*  - pruefung auf einhaltung der Laenge                            *)
(*  - LA ggf. durch LH ... ersetzen (wg. 4096)                      *)
(*  - Problem: Reg 14 wird ggf. benutzt (invalidieren)              *)
(*                                                                  *)
(********************************************************************)
(*$D-,N+                                                            *)
(********************************************************************)
(*                                                                  *)
(*  P_CODE (POST) PROCESSOR                                         *)
(*  -----------------------                                         *)
(*                                                                  *)
(*  COPYRIGHT 1976, STANFORD LINEAR ACCELERATOR CENTER.             *)
(*                                                                  *)
(*  THIS IS A TRANSLATOR FOR THE MODIFIED  P-CODE  GENERATED  BY    *)
(*  THE  SLAC  PASCAL   COMPILER.  THE TRANSLATOR TRANSLATES THE    *)
(*  P_CODE INTO IBM/370 ASSEMBLY  LANGUAGE  OR  STANDARD  OS/370    *)
(*  OBJECT  MODULE  WHICH  COULD BE RUN ON THE 370 USING A SMALL    *)
(*  I/O PACKAGE.  ALSO  THE  IMMEDIATE  TARGET  MACHINE  OF  THE    *)
(*  TRANSLATOR  IS  THE 360/370 COMPUTERS, THE MACHINE DEPENDENT    *)
(*  MODULES IN THE PROGRAM ARE  RELATIVELY  ISOLATED  SUCH  THAT    *)
(*  CONVERSIONS  FOR  OTHER  REGISTER  ORIENTED  TARGET MACHINES    *)
(*  SHOULD BE STRAIGHTFORWARD.                                      *)
(*                                                                  *)
(*  REFER TO THE 'THE PASCAL P COMPILER:  IMPLEMENTATION  NOTES,    *)
(*  U.  AMMANN, K.  JENSEN, H.  NAGELI, AND K.  NORI, DEC.  74.'    *)
(*  FOR  THE DEFINITION OF THE P_MACHINE AND THE P SUBSET OF THE    *)
(*  PROGRAMMING LANGUAGE "PASCAL".                                  *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  -THE ERROR MESSAGES ISSUED  BY  THE TRANSLATOR  ARE  USUALLY    *)
(*  ACCOMPANIED  BY THE  APPROXIMATE LINE  NUMBER  OF THE SOURCE    *)
(*  STATEMENT.    THESE NUMBERS APPEAR ON THE LEFT OF THE SOURCE    *)
(*  PROGRAM LISTING AND THE ERROR SHOULD BE LOCATED BETWEEN  THE    *)
(*  STATEMENT  WITH THE GIVEN NUMBER AND THAT NUMBER+1.    THESE    *)
(*  ERROR CODES SHOULD BE INTERPRETED ACCORDING TO THE FOLLOWING    *)
(*  TABLE:                                                          *)
(*                                                                  *)
(*  253- PROCEDURE TOO LONG (LARGER THAN 8K BYTES).                 *)
(*       --> SUBDIVIDE THE PROCEDURE.                               *)
(*  254- PROCEDURE TOO LONG (LARGER THAN 8K BYTES) - other place    *)
(*       --> SUBDIVIDE THE PROCEDURE.                               *)
(*  255- PROCEDURE TOO LONG (LARGER THAN 8K BYTES) - other place    *)
(*       --> SUBDIVIDE THE PROCEDURE.                               *)
(*  256- TOO MANY PROCEDURES/FUNCTIONS REFERENCED IN THIS PROC.     *)
(*       --> RECOMPILE THE POST_PROCESSOR WITH A LARGER VALUE       *)
(*       FOR PRCCNT.                                                *)
(*  259- EXPRESSION TOO COMPLICATED.                                *)
(*       -->  SIMPLIFY  THE  EXPRESSION  BY  REARRANGING  AND/OR    *)
(*       BREAKING.                                                  *)
(*  263- TOO MANY (COMPILER GENERATED) LABELS IN THIS PROCEDURE.    *)
(*       --> RECOMPILE THE POST_PROCESSOR WITH A LARGER VALUE       *)
(*       FOR LBLCNT.                                                *)
(*  300- DIVIDE BY ZERO (RESULT OF CONSTANT PROPAGATION).           *)
(*       --> FIX UP THE (CONSTANT) EXPRESSION EVALUATING TO ZERO.   *)
(*  301- RANGE ERROR IN STRUCTURED CONSTANT.                        *)
(*       --> CORRECT INITIAL VALUE FOR FIELD/ELEMENT OF CONSTANT.   *)
(*  302- SUBSCRIPTRANGE ERROR (RESULT OF CONSTANT PROPAGATION).     *)
(*       --> FIX UP THE CONSTANT SUBSCRIPT EXPRESSION.              *)
(*  303- CONSTANT SET TOO LARGE FOR TARGET VARIABLE IN AN ASSMT.    *)
(*       --> CORRECT DECLARATION FOR VARIABLE.                      *)
(*                                                                  *)
(*  504- SIZE OF ARRAY ELEMENT TOO LARGE.                           *)
(*       --> REORDER THE DIMENSIONS OF THE ARRAY (SO THAT THE       *)
(*       THE LARGER DIMENSIONS ARE FIRST) OR REDUCE THE RANGE       *)
(*       OF THE LOW ORDER (LAST) INDICES.                           *)
(*                                                                  *)
(*  THE FOLLOWING ERRORS NORMALLY INDICATE AN INCONSISTENCY IN      *)
(*  THE COMPILER AND OR THE POST_PROCESSOR.                         *)
(*                                                                  *)
(*  601- TYPE CONFLICT OF OPERANDS IN THE P_PROGRAM.                *)
(*  602- OPERAND SHOULD BE OF TYPE 'ADR'.                           *)
(*  604- ILLEGAL TYPE FOR RUN TIME CHECKING.                        *)
(*  605- OPERAND SHOULD BE OF TYPE 'BOOL'.                          *)
(*  606- UNDEFINED P_INSTRUCTION CODE.                              *)
(*  607- UNDEFINED STANDARD PROCEDURE NAME.                         *)
(*  608- DISPLACEMENT FIELD OUT OF RANGE                            *)
(*  609- SMALL PROC IS LARGER THAN 4K, RESET SHRT_PROC = 350        *)
(*  610- BAD HALFWORD INTEGER ALIGNMENT                             *)
(*  611- BAD INTEGER ALIGNMENT.                                     *)
(*  612- BAD REAL ALIGNMENT.                                        *)
(*  614- THE PRE_PASS FILE (PRD) IS INCONSISTENT.                   *)
(*  615- OPERAND SHOULD BE OF TYPE 'SET'.                           *)
(*  616- CONSISTENCY CHECK ON 'SET' OPS FAILED.                     *)
(*  617- BAD DISPLACEMENT FOR STRUCTURED CONSTANT.                  *)
(*  618- UNEXPECTED END-OF-LINE WHEN READING P-CODE.                *)
(*  619- BAD OPERANDS FOR PACK/UNPACK PROCEDURE.                    *)
(*                                                                  *)
(*  THIS PROGRAM SHOULD NOT BE COMPILED WITH THE 'D+' OPTION.       *)
(*                                                                  *)
(*                                                                  *)
(*                          S. HAZEGHI,                             *)
(*                                                                  *)
(*                          COMPUTATION RESEARCH GROUP              *)
(*                          STANFORD LINEAR ACCELARATOR CENTER      *)
(*                          STANFORD, CA. 94305.                    *)
(*                                                                  *)
(*                                                                  *)
(*  EXTENSIVE MODIFICATIONS MADE BY:                                *)
(*                                                                  *)
(*                          R. NIGEL HORSPOOL                       *)
(*                                                                  *)
(*                          SCHOOL OF COMPUTER SCIENCE              *)
(*                          MCGILL UNIVERSITY                       *)
(*                          805 SHERBROOKE STREET WEST              *)
(*                          MONTREAL                                *)
(*                          QUEBEC  H3A 2K6   CANADA                *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*         AUTHOR OF This VERSION (Oppolzer Version):               *)
(*                                                                  *)
(*              Bernd Oppolzer                                      *)
(*              Diplom-Informatiker                                 *)
(*              Baerenhofstr. 23                                    *)
(*              D-70771 Leinfelden-Echterdingen                     *)
(*              Germany                                             *)
(*                                                                  *)
(********************************************************************)
(*  History of changes since 2011:                                  *)
(********************************************************************)
(*                                                                  *)
(*  modification in 2011 by bernd oppolzer / stuttgart / germany    *)
(*                                                                  *)
(*                          berndoppolzer@yahoo.com                 *)
(*                                                                  *)
(*  - activate output of assembler mnemonics to asmout              *)
(*    to make analyzing the object code easier.                     *)
(*    the final goal is to create other pcode translators           *)
(*    for other platforms, e.g. windows and linux                   *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  modification in 2016 by bernd oppolzer / stuttgart / germany    *)
(*                                                                  *)
(*                          berndoppolzer@yahoo.com                 *)
(*                                                                  *)
(*  there was a need to do an explicit reset(input) at the          *)
(*  beginning of the main procedure, because the compiler           *)
(*  doesn't insert it any more automatically due to some            *)
(*  improvements (in my opinion); but pascal2.pas checks for        *)
(*  eof(input) before first read, and therefore the reset has       *)
(*  to be done before the first read (the implicit reset at the     *)
(*  time of the first read call is not sufficient).                 *)
(*                                                                  *)
(*  see some comments in pasmonn.ass for details.                   *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  modification in 2016 by bernd oppolzer / stuttgart / germany    *)
(*                                                                  *)
(*                          berndoppolzer@yahoo.com                 *)
(*                                                                  *)
(*  to allow longer variable names, some constants had to           *)
(*  be changed (and places, where numeric constants were used       *)
(*  instead of symbolic constants had to be identified and          *)
(*  changed, too)                                                   *)
(*                                                                  *)
(*  idlngth changed from 12 to 20                                   *)
(*  hdrlngth changed from 32 to 40                                  *)
(*                                                                  *)
(*  input routines affected for the following P-Code instructions:  *)
(*  ENT, CST, BGN - format specification IDLNGTH + 2 instead of 14  *)
(*                                                                  *)
(*  Header for Object Code output extended from 32 to 40 bytes;     *)
(*  Jump over Header adjusts automatically                          *)
(*                                                                  *)
(*  longer function names appear in Objekt Code, for example        *)
(*  PASCALCOMPILER (name of pass1) and function names like          *)
(*  ENTERSTDTYPES without truncation                                *)
(*                                                                  *)
(*  will SNAPSHOT etc. still work ?                                 *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  modification in 2016 by bernd oppolzer / stuttgart / germany    *)
(*                                                                  *)
(*                          berndoppolzer@yahoo.com                 *)
(*                                                                  *)
(*  the procedure names applied to the object file in case of       *)
(*  the active debug switch had to be extended to 20 chars,         *)
(*  so that SNAPSHOT could get the long names correctly.            *)
(*  the branches around those names had to be adjusted.             *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Oct.2016 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  modification to support static variables and to provide         *)
(*  informations for SNAPSHOT, so that static variables can         *)
(*  be found at run time. See pass 1 (PASCAL1.PAS) for details,     *)
(*  and some comments in INIT_CSECT and GEN_CSECT.                  *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Nov.2016 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  To enable the port to ASCII platforms, the following            *)
(*  changes have been made:                                         *)
(*                                                                  *)
(*  - set constants of set of char have a new representation        *)
(*    in the P-Code, envolving char representation of the           *)
(*    chars contained in the set                                    *)
(*                                                                  *)
(*  - not related to the port: set constants in P-Code are          *)
(*    represented by hexa byte strings instead of integer           *)
(*    strings, which makes them much better readable                *)
(*                                                                  *)
(*  See procedure READSET                                           *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Dec.2016 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  Another portability issue:                                      *)
(*                                                                  *)
(*  the branch table used with case statements implies the          *)
(*  EBCDIC char set, if the case control variable is of type        *)
(*  char. I changed the XJP logic to a portable representation      *)
(*  of the branch table and shifted the construction of the         *)
(*  "real" branch table to the second pass. This way, XJP           *)
(*  instructions and "portable branch tables" can be moved          *)
(*  to foreign platforms with foreign character sets.               *)
(*                                                                  *)
(*  see boolean constant 'PORTABLE_BRANCHTABLE' in pass 1           *)
(*                                                                  *)
(*  this is the second pass (the P-Code translator);                *)
(*  it recognizes and handles both variants of branch tables,       *)
(*  portable and non-portable                                       *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Dec.2016 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  The generation of the STATIC and CODE CSECTs was changed.       *)
(*                                                                  *)
(*  The STATIC CSECT contains its own size at offset 8;             *)
(*  the real data starts at offset 16 (10 to 15 are free).          *)
(*                                                                  *)
(*  The CODE CSECT contains the Pascal procedure name also          *)
(*  in the NODEBUG case in the CSECT identifier, and the            *)
(*  stacksize at a certain position (see GEN_CSECT and              *)
(*  INIT_CSECT for details).                                        *)
(*                                                                  *)
(*  This way it is possible for PASSNAP to show the areas           *)
(*  in their correct length also in the NODEBUG case in             *)
(*  hex dump format; and with the real Pascal proc names            *)
(*  (but no Pascal variable names; to do this, the DEBUG            *)
(*  switch and a DBGINFO file is needed).                           *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Jan.2017 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  From some discussions on the FPC mailing list, I got the        *)
(*  idea to support bit operations on integer operands, too.        *)
(*                                                                  *)
(*  The operations AND, OR, NOT have been extended to do            *)
(*  bit operations, when being used with integers (was error        *)
(*  134 before). Another operation XOR is provided (new             *)
(*  reserved symbol) for exclusive or operation; can be used        *)
(*  with integer or boolean operands.                               *)
(*                                                                  *)
(*  New P-Code instruction XOR; the P-Code instructions             *)
(*  AND, IOR, NOT and XOR have a type parameter (B or I).           *)
(*                                                                  *)
(*  PASCAL2 was extended to support the integer operands            *)
(*  with AND, IOR and NOT and the new P-Code instruction XOR;       *)
(*  the constant XOR had to be renamed to XORX, because             *)
(*  XOR now is a reserved word.                                     *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  May.2017 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  The compiler now runs on MVS (Hercules), too.                   *)
(*  Same source code (PASCAL1, PASCAL2) as with CMS,                *)
(*  same runtime (PASMONN) - although there are some                *)
(*  CMS dependencies, controlled by SYSPARM(CMS).                   *)
(*  Different PASSNAP ... see below.                                *)
(*                                                                  *)
(*  See more comments in PASCAL1.PAS                                *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Aug.2017 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  Some corrections on the implementation of Pascal sets,          *)
(*  sets can have up to 2000 elements now ... see constants         *)
(*  MXPLNGTH etc.                                                   *)
(*                                                                  *)
(*  More improvements on sets will follow                           *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Dec.2017 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  New P-Code instructions to better support block moves           *)
(*  like memcpy and memset:                                         *)
(*                                                                  *)
(*   - PCODE_MFI = 80 ;  // memory fill - constant length           *)
(*   - PCODE_MCP = 81 ;  // memcpy - three parameters               *)
(*   - PCODE_MSE = 82 ;  // memset - three parameters               *)
(*   - PCODE_MZE = 84 ;  // memory zero - constant length           *)
(*                                                                  *)
(*  and a new DBG instruction, which should be ignored:             *)
(*                                                                  *)
(*   - PCODE_DBG = 83 ;  // one parameter, ignored at the moment    *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Mar.2018 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  Implementing the new P-Code instructions to support             *)
(*  Strings aka VarChars ... the new P-Codes all start with         *)
(*  the letter V and are described elsewhere.                       *)
(*                                                                  *)
(*  The handling and administration of the literal pool             *)
(*  has been improved, see type literal_pool_control;               *)
(*  character string literals are stored only once, if              *)
(*  they appear more than once in a procedure or function;          *)
(*  this is also true if one string is the beginning or ending      *)
(*  part of another string (the longer string must have             *)
(*  appeared first in the source).                                  *)
(*                                                                  *)
(*  Many minor improvements to PASCAL2 to make the String           *)
(*  implementation possible :-)                                     *)
(*                                                                  *)
(*  The new P-Codes:                                                *)
(*                                                                  *)
(*  'VC1' , 'VC2' , 'VCC' , 'VLD'                                   *)
(*  'VST' , 'VMV' , 'VSM' , 'VLM'                                   *)
(*  'VPU' , 'VPO' , 'VIX' , 'VRP'                                   *)
(*                                                                  *)
(*  see procedure STRINGOPS (and others)                            *)
(*                                                                  *)
(********************************************************************)



const VERSION = '2018.03' ;        // Version for display message
      VERSION2 = 0x1803 ;          // Version for load module
      VERSION3 = 'XL2''1803''' ;   // Version for ASMOUT listing
      MXADR = 65535 ;
      SHRTINT = 4095 ;
      HALFINT = 32700 ;
      STKDPTH = 100 ;    // was 15 - reset when work finished
      MXLVL = 16 ;

      (*******************************************)
      (* cixmax: maximum difference of highest   *)
      (* and lowest case label                   *)
      (* must be coherent with pass 1            *)
      (* should be communicated via OBJCODE file *)
      (*                                         *)
      (*******************************************)

      CIXMAX = 400 ;
      IDLNGTH = 20 ;   // length of identifiers
      RGCNT = 9 ;      // register count
      FPCNT = 6 ;      // FLOATING POINT REG. COUNT
      GBR = 12 ;       // GLOBAL BASE REGITER
      LBR = 13 ;       // local base register
      JREG = 15 ;      // JUMP (BRANCH) REGISTER
      RTREG = 14 ;     // RETURN ADDR. REGISTER
      TRG0 = 0 ;       // PARAMETER REGISTER
      FPR0 = 0 ;       // FLOATING POINT REGISTER 0
      TRG1 = 1 ;       // TEMPORARY VALUE/BASE REGISTERS
      TRG13 = 13 ;     // SAVE AREA/LOCAL STACK FRAME
      TRG14 = 14 ;
      TRG15 = 15 ;
      TRG9 = 9 ;
      PBR1 = 10 ;
      PBR2 = 11 ;
      FILADR = 9 ;
      CALLSTACKADR = 8 ;

      (***************************************************)
      (* Reg 9 = Pascal FCB register                     *)
      (* normally available, but not during csp call     *)
      (* Reg 8 = Call Stack Adr                          *)
      (* adr of next save area on stack, should be       *)
      (* stable across calls (while in csp, reg 8 is     *)
      (* used to access file cb)                         *)
      (***************************************************)

      MAXSTRL = 254 ;

      (******************************************************)
      (* MAXSTRL  = maximal string length                   *)
      (* MXPLNGTH = maximal set length in chars (8 bits)    *)
      (* MXSETINX = maximal set length (64 bit packets)     *)
      (* MXSETIXI = maximal set length (32 bit packets)     *)
      (******************************************************)

      MXPLNGTH = 256 ;
      MXSETINX = 32 ;
      MXSETIXI = 64 ;
      MXSETLEN = 256 ;
      HDRLNGTH = 40 ;   // LENGTH OF PROGRAM HEADING
      EOFDPLMT = 33 ;

      (**************************************************)
      (*POSN OF EOF FLAG WITHIN FILE HEADER             *)
      (**************************************************)

      EOLDPLMT = 34 ;

      (**************************************************)
      (*POSN OF EOL FLAG WITHIN HEADER FOR TEXT FILES   *)
      (**************************************************)

      FILHDRSZ = 8 ;

      (**************************************************)
      (*OFFSET OF FILE COMPONENT WITHIN FILE VAR.       *)
      (**************************************************)

      ADRSIZE = 4 ;
      CHARSIZE = 1 ;
      BOOLSIZE = 1 ;
      INTSIZE = 4 ;
      HINTSIZE = 2 ;
      REALSIZE = 8 ;

      (****************************************************)
      (* LAYOUT OF THE 'GLOBAL' STACK FRAME:              *)
      (****************************************************)

      NEWPTR = 72 ;

      (****************************************************)
      (* NEWPTR , OFFSET FROM BOTTOM OF RUNTIME STACK     *)
      (****************************************************)

      HEAPLMT = 76 ;

      (****************************************************)
      (* HEAP LIMIT PTR, OFFSET FROM BOTTOM OF STACK      *)
      (****************************************************)

      DYNRUNC = 0 ;

      (****************************************************)
      (* # OF COUNTERS , FIELD OFFSET FROM HEAPLMT        *)
      (****************************************************)

      DYN2LEN = 8 ;

      (****************************************************)
      (* LENGTH OF THE DYN. INFO. AREA AT THE END OF HEAP *)
      (****************************************************)

      FNCRSLT = 72 ;

      (****************************************************)
      (* FUNCTION RESULT LOCATION, OFFSET FROM MP         *)
      (****************************************************)

      DISPLAY = 80 ;

      (****************************************************)
      (* DISPLAY REGS, OFFSET FROM BOTTOM OF RUNTIME STK  *)
      (****************************************************)

      DISPAREA = 40 ;

      (****************************************************)
      (* SIZE OF DISPLAY TABLE                            *)
      (****************************************************)

      LCAFTMST = 80 ;

      (****************************************************)
      (* SIZE OF THE PROCEDURE LINKAGE AREA               *)
      (****************************************************)

      FPRSAREA = 80 ;

      (****************************************************)
      (* FLOATING PT. REGS SAVE AREA, (OPTIONAL SAVE)     *)
      (****************************************************)

      FPSIZE = 32 ;

      (****************************************************)
      (* LENGTH OF FPR SAVE AREA                          *)
      (****************************************************)

      FL1 = 352 ;
      FL2 = 360 ;
      FL3 = 368 ;
      FL4 = 376 ;

      (****************************************************)
      (* GLOBAL LOCATIONS USED FOR FIX-FLOAT CONVERSIONS  *)
      (****************************************************)

      STRFIRST = 384 ;// PTR TO BEGIN OF STR WORKAREA
      STRCURR = 388 ; // ACTUAL PTR TO STR WORKAREA
      STRSIZE = 392 ; // STR WORKAREA SIZE

      (****************************************************)
      (* addresses for string workarea management         *)
      (****************************************************)

      INXCHK = 152 ;

      (****************************************************)
      (* ADDRESS OF RUNTIME CHECK ROUTINES                *)
      (****************************************************)

      RNGCHK = 164 ;
      PRMCHK = 176 ;
      PTRCHK = 188 ;
      PTACHK = 200 ;
      SETCHK = 212 ;
      STKCHK = 224 ;
      TRACER = 236 ;

      (****************************************************)
      (* CONTROL FLOW TRACE ROUTINE                       *)
      (****************************************************)

      FILEBUFS = 248 ;

      (****************************************************)
      (* INPUT, OUTPUT, PRD,.... BUFFERS                  *)
      (****************************************************)

      CLEARBUF = 320 ;

      (****************************************************)
      (* PRESET BUFER TO ERASE MEMORY WITH ITS CONTENTS   *)
      (****************************************************)

      PASDATE = 328 ;

      (****************************************************)
      (* PREDEFINED DATE VARIABLE                         *)
      (****************************************************)

      PASTIME = 338 ;

      (****************************************************)
      (* PREDEFINED TIME VARIABLE                         *)
      (****************************************************)

      OSPRM = 348 ;

      (****************************************************)
      (* POINTER TO O.S. PARMS RECORD                     *)
      (****************************************************)

      FRSTGVAR = 400 ;

      (****************************************************)
      (* FIRST GLOBAL VAR, SHOULD BE A MULTIPLE OF 8      *)
      (* VARIOUS TABLE SIZES AND MISC. CONSTATNTS         *)
      (****************************************************)

      HTSIZE = 200 ;

      (****************************************************)
      (* HASH TABLE SIZE (MUST EXCEED # OPS + # CSP OPS)  *)
      (****************************************************)

      DBLCNT = 200 ;

      (****************************************************)
      (* SIZE OF LITERAL POOL - IN DOUBLE-WORDS           *)
      (****************************************************)

      DBLDANGER = 190 ;   // SAFE LIMIT FOR nxtdbl
      INTCNT = 400 ;      // = DBLCNT*2
      HWCNT = 800 ;       // = DBLCNT*4
      CHCNT = 1600 ;      // = DBLCNT*8
      LITCNT = 400 ;      // # OF NUMERIC LITERALS IN A PROC.
      LITDANGER = 395 ;   // SAFE LIMIT FOR NXTLIT

      (****************************************************)
      (* PRCCNT = # OF PROC'S OR ENTRY PT.S IN ONE CSECT  *)
      (* opp 02.2018: was 50, set to 200                  *)
      (* LBLCNT = # OF LABELS IN A CSECT                  *)
      (****************************************************)

      PRCCNT = 200 ;
      LBLCNT = 500 ;
      MAXCALDPTH = 4 ;

      (****************************************************)
      (* MAX NESTING OF FUNCTION CALLS IN A STMT.         *)
      (****************************************************)

      MXCODE = 4092 ;

      (****************************************************)
      (* MAX OBJECT CODE SIZE (APPROX. 8K BYTES)          *)
      (****************************************************)

      MXCODE1 = 4093 ;
      MXLNP = 800 ;

      (****************************************************)
      (* SIZE OF LINE NUMBER TABLE IN BYTES               *)
      (****************************************************)

      CODE_BYTES = 11199 ;
      CODE_HINTS = 5599 ;
      CODE_INTS = 2799 ;
      CODE_REALS = 1399 ;
      CODE_CHUNKS = 199 ;
      SIZE_TXTCHUNK = 56 ;

      //**********************************************************
      // definitions for code vector                              
      // code_bytes = size of code vector in bytes (minus 1)      
      // code_hints = size of code vector in hints (minus 1)      
      // code_ints = size of code vector in ints (minus 1)        
      // code_reals = size of code vector in reals (minus 1)      
      // code_chunks = size of code vector in txt chunks (minus 1)
      // size_txtchunk = size of a txt chunk                      
      // MAX. BYTES PER TXT CARD IN 360 OBJECT DECK               
      //**********************************************************

      LESCND = 4 ;
      LEQCND = 13 ;

      (*****************************)
      (* CONDITION CODE SYMBOLS    *)
      (*****************************)

      GRTCND = 2 ;
      GEQCND = 11 ;
      EQUCND = 8 ;
      NEQCND = 7 ;
      ANYCND = 15 ;
      NOCND = 0 ;
      TRUCND = 1 ;
      FLSCND = 8 ;

      (****************************************************)
      (* LIMIT VALUE FOR A PROC. TO BE CONSIDERED SMALL   *)
      (****************************************************)

      SHRT_PROC = 550 ;

      /*******************************************************/
      /* asmtag = tag for assembler instructions in asmout   */
      /* colasmi = position for assembler instr. in asmout   */
      /* spaceasmi = spaces after asm instr. in asmout       */
      /*******************************************************/

      ASMTAG = '@@ ' ;
      COLASMI = 10 ;
      SPACEASMI = 2 ;
      SPACEASMX = 12 ;
      SPACEASML = 5 ;

      (****************************************************)
      (* OPCODE TABLES  (P-OPCODES / P-STANDARD PROCS /   *)
      (* 370-OPCODES )                                    *)
      (****************************************************)

      XLTR = 18 ;
      XL = 88 ;
      XLH = 72 ;
      XLD = 104 ;
      XLR = 24 ;
      XLDR = 40 ;
      XIC = 67 ;
      XLM = 152 ;
      XLA = 65 ;
      XLPR = 16 ;
      XLCR = 19 ;
      XLPDR = 32 ;
      XLCDR = 35 ;
      XLTDR = 34 ;
      XA = 90 ;
      XAH = 74 ;
      XAD = 106 ;
      XAR = 26 ;
      XADR = 42 ;
      XSDR = 43 ;
      XMDR = 44 ;
      XDDR = 45 ;
      XAW = 110 ;
      XST = 80 ;
      XSTD = 96 ;
      XSTH = 64 ;
      XSTC = 66 ;
      XSTM = 144 ;
      XMVC = 210 ;
      XMVCL = 14 ;
      XMVI = 146 ;
      XS = 91 ;
      XSH = 75 ;
      XSD = 107 ;
      XSR = 27 ;
      XN = 84 ;
      XNR = 20 ;
      XNC = 212 ;
      XO = 86 ;
      XORX = 22 ;
      XOC = 214 ;
      XX = 87 ;
      XXR = 23 ;
      XXC = 215 ;
      XM = 92 ;
      XMH = 76 ;
      XMD = 108 ;
      XMR = 28 ;
      XD = 93 ;
      XDD = 109 ;
      XDR = 29 ;
      XBCR = 7 ;
      XBC = 71 ;
      XBCTR = 6 ;
      XBCT = 70 ;
      XBALR = 5 ;
      XBAL = 69 ;
      XC = 89 ;
      XCR = 25 ;
      XCH = 73 ;
      XCL = 85 ;
      XCLR = 21 ;
      XTM = 145 ;
      XCLI = 149 ;
      XCLC = 213 ;
      XCLCL = 15 ;
      XSLA = 139 ;
      XSLDA = 143 ;
      XSRA = 138 ;
      XSRDA = 142 ;
      XSLL = 137 ;
      XSLDL = 141 ;
      XSRL = 136 ;
      XSRDL = 140 ;
      XCD = 105 ;
      XCDR = 41 ;
      XEX = 68 ;


type OPTYPE = ( PCTS , PCTI , PLOD , PSTR , PLDA , PLOC , PSTO , PLDC ,
              PLAB , PIND , PINC , PPOP , PCUP , PENT , PRET , PCSP ,
              PIXA , PEQU , PNEQ , PGEQ , PGRT , PLEQ , PLES , PUJP ,
              PFJP , PXJP , PCHK , PNEW , PADI , PADR , PSBI , PSBR ,
              PSCL , PFLT , PFLO , PNGI , PNGR , PSQI , PSQR , PABI ,
              PABR , PNOT , PAND , PIOR , PDIF , PINT , PUNI , PINN ,
              PMOD , PODD , PMPI , PMPR , PDVI , PDVR , PMOV , PLCA ,
              PDEC , PSTP , PSAV , PRST , PCHR , PORD , PDEF , PCRD ,
              PXPO , PBGN , PEND , PASE , PSLD , PSMV , PMST , PUXJ ,
              PXLB , PCST , PDFC , PPAK , PADA , PSBA , PXOR , PMFI ,
              PMCP , PMSE , PDBG , PMZE , PVC1 , PVC2 , PVCC , PVLD ,
              PVST , PVMV , PVSM , PVLM , PVPU , PVPO , PVIX , PVRP ,
              UNDEF_OP ) ;
     CSPTYPE = ( PCTR , PN01 , PN02 , PN03 , PN04 , PN05 , PN06 , PN07
               , PN08 , PN09 , PPAG , PGET , PPUT , PRES , PREW , PRDC
               , PWRI , PWRE , PWRR , PWRC , PWRS , PWRX , PRDB , PWRB
               , PRDR , PRDH , PRDY , PEOL , PEOT , PRDD , PWRD , PCLK
               , PWLN , PRLN , PRDI , PEOF , PELN , PRDS , PTRP , PXIT
               , PFDF , PSIO , PEIO , PMSG , PSKP , PLIM , PTRA , PWRP
               , PCLS , PDAT , PTIM , PFLR , PTRC , PRND , PWRV ,
               UNDEF_CSP ) ;
     BETA = array [ 1 .. 3 ] of CHAR ;
     HINTEGER = - 32768 .. 32767 ;
     STRNG = packed array [ 1 .. MAXSTRL ] of CHAR ;
     ALFA = packed array [ 1 .. 8 ] of CHAR ;
     IDTYPE = packed array [ 1 .. IDLNGTH ] of CHAR ;
     CHAR80 = packed array [ 1 .. 80 ] of CHAR ;
     ADRRNG = 0 .. MXADR ;
     LVLRNG = - 2 .. MXLVL ;

     (********************************************)
     (* REGISTER NUMBER RANGE                    *)
     (********************************************)

     RGRNG = LVLRNG ;

     (********************************************)
     (* set type definitions                     *)
     (********************************************)

     SHORT_SET = set of 0 .. 63 ;
     LARGE_SET = record
                   case INTEGER of
                     1 :
                       ( S : array [ 1 .. MXSETINX ] of SHORT_SET ) ;
                     2 :
                       ( C : array [ 1 .. MXSETLEN ] of CHAR ) ;
                 end ;

     (********************************************)
     (* some subranges                           *)
     (********************************************)

     BYTE = 0 .. 255 ;
     BYTE_PLUS_ONE = 1 .. 256 ;
     LINE_NUM = 0 .. 10000 ;
     STKPTR = 0 .. STKDPTH ;

     (********************************************)
     (* POINTER TO THE COMPILE_TIME STACK        *)
     (********************************************)

     LVLDSP = record
                DSPLMT : INTEGER ;
                LVL : LVLRNG
              end ;

     (******************************)
     (* WHERE ABOUT OF THE OPERAND *)
     (******************************)

     SPTR = -> STRNG ;
     ICRNG = 0 .. MXCODE1 ;
     ICRNG_EXT = - 100 .. MXCODE1 ;
     ADRRNG_EXT = - 100 .. MXADR ;

     (********************************************)
     (* PROGRAM COUNTER RANGE                    *)
     (* extended ranges, because negative        *)
     (* values are stored in linkage fields      *)
     (* (pointers to stack entries)              *)
     (********************************************)

     LBLRNG = - 1 .. LBLCNT ;

     (********************************************)
     (* RANGE OF P_COMPILER GENERATED LABELS     *)
     (********************************************)

     STRLRNG = 0 .. MAXSTRL ;
     PLNRNG = 0 .. MXPLNGTH ;
     POSINT = 0 .. 214748360 ;
     HEX4 = array [ 1 .. 4 ] of CHAR ;
     MNEM_TABLE = array [ 0 .. 255 ] of array [ 1 .. 4 ] of CHAR ;
     PLABEL = record
                NAM : ALFA ;
                LEN : 0 .. IDLNGTH ;
                CADDR : ADRRNG ;
              end ;

     //************************************************************
     // datatype and datum are types to define                     
     // the structure of the stack (stk)                           
     // used by pascal2 during optimization                        
     // bank is where the stack object resides                     
     //************************************************************

     BANK = ( RGS , MEM , ONSTK , NEITHER ) ;
     DATATYPE = ( BOOL , CHRC , ADR , HINT , INT , PSET , REEL , PROC ,
                CARR , VARC , INX , NON ) ;
     DATUM = record
               RCNST : REAL ;
               PCNST : -> LARGE_SET ;
               STKADR : ADRRNG ;
               PLEN : PLNRNG ;
               FPA : LVLDSP ;
               SCNSTNO : 0 .. LITCNT ;
               VRBL , DRCT : BOOLEAN ;
               PROCNAME : ALFA ;
               DTYPE : DATATYPE ;
               case VPA : BANK of
                 RGS :
                   ( RGADR : RGRNG ) ;
                 MEM :
                   ( MEMADR : LVLDSP )
             end ;

     //****************************************************************
     //* this new structure records the global state of processing     
     //****************************************************************

     GLOBAL_STATE = record
                      IN_PROCBODY : BOOLEAN ;
                      FILL_LINEPTR : BOOLEAN ;
                      MOD1DEFSTEP : INTEGER ;
                      MOD2DEFSTEP : INTEGER ;
                    end ;

     //****************************************************************
     // structure with literal pool control information                
     // nxtdbl, nxtch = indices of next free element                   
     // hw_gap = index of halfword gap to be filled (or -1, if no gap) 
     // int_gap = index of integer gap to be filled (or -1, if no gap) 
     // ihconf = index of integer containing halfword gap              
     // riconf = index of double containing integer gap                
     // (if double match, gap must not be used !)                      
     // rhconf = index of double containing halfword gap               
     // (if double match, gap must not be used !)                      
     //****************************************************************
     // if a double entry is used by a new halfword,                   
     // two gaps are inserted: a halfword gap and an integer gap       
     // later the halfword gap or the integer gap may be used;         
     // the integer gap may be used by a halfword, leaving another     
     // halfword gap, and so on. if an entry containing a gap          
     // is used otherwise, the gaps have to be invalidated etc.        
     // at a certain point in time, we have always at most one         
     // halfword gap and one integer gap                               
     //****************************************************************

     LITERAL_POOL_CONTROL = record
                              NXTDBL : 0 .. DBLCNT ;    // next double
                              NXTCH : 0 .. CHCNT ;      // next char
                              HW_GAP : - 1 .. HWCNT ;
                              INT_GAP : - 1 .. INTCNT ;
                              IHCONF : - 1 .. INTCNT ;
                              RICONF : - 1 .. DBLCNT ;
                              RHCONF : - 1 .. DBLCNT ;
                            end ;

     //****************************************************************
     // chain of procedure definitions                                 
     // built from pcode file in first pass                            
     // before actual pcode processing                                 
     //****************************************************************
     // PRE-PASS (PRD FILE) INFORMATION - now DBGINFO                  
     // opp - 2018.03:                                                 
     // pcode summary file contains informations for                   
     // procedures that are available at the end of the                
     // parsing of the procedure, but they are needed at               
     // the beginning of the code generation already. to               
     // keep the one pass paradigm of PASCAL1 and PASCAL2,             
     // this informtion is written to a separate file and              
     // read when a new procedure begins (tagged with the              
     // procedure name in the leading column)                          
     // - proc size = code size of the procedure                       
     // - data size = data size of the procedure                       
     // - call higher = other procedure calls, that is,                
     // the display position at the static level has to                
     // be saved and restored                                          
     //****************************************************************
     // later: this information is written into the normal             
     // pcode file, via def constants, after every procedure.          
     // because it is needed before code generation, but it            
     // is located after the procedure (the compiler does not          
     // know it before processing the procedure), the p-code           
     // translator (this program) reads the input twice.               
     // see the main program ... two processing loops                  
     //****************************************************************

     PPI = -> PROCEDURE_INFO ;
     PROCEDURE_INFO = record
                        CURPNAME : array [ 1 .. IDLNGTH ] of CHAR ;
                        CURPNO : INTEGER ;
                        OPNDTYPE : DATATYPE ;
                        SEGSZE : PLABEL ;
                        SAVERGS : BOOLEAN ;
                        ASM : BOOLEAN ;
                        ASMVERB : BOOLEAN ;
                        GET_STAT : BOOLEAN ;
                        DEBUG_LEV : 0 .. 9 ;
                        STATNAME : ALFA ;
                        SOURCENAME : ALFA ;
                        FLOW_TRACE : BOOLEAN ;
                        CALL_HIGHER : BOOLEAN ;
                        LARGE_PROC : BOOLEAN ;
                        CODE_SIZE : ADRRNG ;  // was icrng
                        DATA_SIZE : ADRRNG ;
                        NEXT : PPI
                      end ;


var GS : GLOBAL_STATE ;
    LINECNT : INTEGER ;

    //**************************************************************
    // anchor for procedure information chain                       
    // built during first p-code reading                            
    //**************************************************************

    PIANKER : PPI ;
    PIAKT : PPI ;

    //**************************************************************
    // CURRENT/OLD INST. OPCODE                                     
    // CURRENT STND. PROC. CODE                                     
    // CURRENT (SYMBOLIC) PCODE /CSP NAME                           
    //**************************************************************

    PCODE , OLDPCODE : OPTYPE ;
    CSP , OLDCSP : CSPTYPE ;
    NMCDE , EMPTY : BETA ;
    PROCOFFSET : INTEGER ;
    OP_SP : BOOLEAN ;

    (*******************************************)
    (* P INSTR/SP SWITCH                       *)
    (*******************************************)

    INIT : BOOLEAN ;

    (*******************************************)
    (* INITIALIZATION PHASE FLAG               *)
    (*******************************************)

    CH : CHAR ;

    (*******************************************)
    (* CURRENT INPUT CHARACTER                 *)
    (*******************************************)

    BVAL : BOOLEAN ;
    CHVAL : CHAR ;
    IVAL : INTEGER ;
    RVAL : REAL ;
    PSVAL : LARGE_SET ;
    STRPTR : SPTR ;
    PSLNGTH : 0 .. MXPLNGTH ;
    SVAL : STRNG ;
    SLNGTH : 0 .. MAXSTRL ;
    CURLVL : LVLRNG ;

    (*******************************************)
    (* CURRENT PROC. STATIC LEVEL              *)
    (*******************************************)

    TOP : STKPTR ;

    (*******************************************)
    (* TOP OF EXPRESSION STACK                 *)
    (*******************************************)

    CALDPTH : 0 .. MAXCALDPTH ;

    (*******************************************)
    (* PROC. CALL NESTING                      *)
    (*******************************************)

    LASTLN , NXTLNP , LASTPC , LASTPCDIF : INTEGER ;
    LBL1 , LBL2 : PLABEL ;
    STATCSECT : PLABEL ;

    (*******************************************)
    (* LEFT AND RIGHT LABELS OF INSTRUCTIONS   *)
    (*******************************************)

    XJPFLAG : CHAR ;
    OPNDTYPE : DATATYPE ;
    EXTLANG : CHAR ;

    (*******************************************)
    (* TYPE OF OPERAND OF INSTRUCTION          *)
    (*******************************************)

    P , Q , R : INTEGER ;

    (*******************************************)
    (* P_Q FIELDS OF INSTRUCTION               *)
    (*******************************************)
    (*******************************************)
    (* LOC. OF STRUCT. CONSTANT ITEM           *)
    (*******************************************)

    SP : ADRRNG ;

    (*******************************************)
    (* MEMORY STACK POINTER, NOT USED          *)
    (*******************************************)

    LCAFTSAREA : ADRRNG ;

    (*******************************************)
    (* FIRST LOC. AFTER PROC. SAVE AREA        *)
    (*******************************************)

    FILECNT : 0 .. 2 ;

    (*******************************************)
    (* COUNT OF ACTIVE FILE ADDRESSES          *)
    (*******************************************)
    (*******************************************)
    (* PDEF_CNT = PDEF before branch_table     *)
    (*******************************************)

    PDEF_CNT : INTEGER ;
    CASE_LOW : INTEGER ;
    CASE_HIGH : INTEGER ;
    CASE_LABEL : INTEGER ;
    CASE_DEFAULT : INTEGER ;
    CASE_OPNDTYPE : DATATYPE ;
    CASE_CHARTABLE : array [ CHAR ] of LBLRNG ;

    (*******************************************)
    (* variables for case implementation       *)
    (*******************************************)

    NXTRG , TXRG : RGRNG ;

    (*******************************************)
    (* AQUIRED REGISTERS                       *)
    (*******************************************)

    DBLALN , OPT_FLG : BOOLEAN ;

    (*******************************************)
    (* DWRD ALIGNMENT NEEDED, OPT. IN EFFECT   *)
    (*******************************************)

    FILREGACTIVE : BOOLEAN ;

    (*******************************************)
    (* FILE ADDRESS REG. ACTIVE                *)
    (*******************************************)

    CSPREGACTIVE : BOOLEAN ;

    (*******************************************)
    (* I/O REGISTERS ACTIVE                    *)
    (* if true, r15 points to $PASCSP entry    *)
    (* and needs not be loaded before csp      *)
    (* call. set to false on lab processing    *)
    (*******************************************)

    PROCOFFSET_OLD : INTEGER ;

    (*******************************************)
    (* old procoffset                          *)
    (* r1 is set to this value during          *)
    (* csp processing; if new procoffset       *)
    (* is equal, r1 needs not be reset         *)
    (* again. set to 0 on lab processing       *)
    (*******************************************)

    CSTBLK , MUSIC : BOOLEAN ;

    (*******************************************)
    (* STRUCT. CONST. BLOCK?, MUSIC O.S.?      *)
    (*******************************************)

    CLEAR_REG , NEG_CND : BOOLEAN ;

    (*******************************************)
    (* CLEAR BEFORE LOADING THE REG.           *)
    (*******************************************)

    SAVEFPRS , DEBUG : BOOLEAN ;

    (*******************************************)
    (* indicates, if we are inside of          *)
    (* case branch table / old or new style    *)
    (*******************************************)

    CASE_FLAG : BOOLEAN ;
    CASE_FLAG_NEW : BOOLEAN ;

    (*******************************************)
    (* if asm output is to be printed          *)
    (* first_asmout = first output to asmout   *)
    (*******************************************)

    ASM : BOOLEAN ;
    FIRST_ASMOUT : BOOLEAN ;

    (*******************************************)
    (* VARIOUS OPTIONS                         *)
    (*******************************************)

    TRACE , NEWLINE : BOOLEAN ;
    RUNPROFILE , CKMODE , FLOW_TRACE : BOOLEAN ;

    (*******************************************)
    (* OBJ LISTING, FLOW-TRACING FLAGS         *)
    (* (flow trace will probably not work      *)
    (* at the moment - 2016)                   *)
    (*******************************************)
    (*******************************************)
    (* CURRENTLY UNUSED                        *)
    (*******************************************)

    CURLINE : LINE_NUM ;

    (*******************************************)
    (* CURRENT SOURCE LINE NUMBER              *)
    (*******************************************)

    POOL_SIZE : ICRNG ;

    (*******************************************)
    (* LITERAL POOL SIZE FOR STATISTICS ONLY   *)
    (*******************************************)

    NUMLITS : INTEGER ;

    (*******************************************)
    (* NUMBER OF LITERALS, FOR STATISTICS      *)
    (*******************************************)

    PCAFTLIT : ICRNG ;

    (*******************************************)
    (* Pcounter AFTER LITERAL DUMP             *)
    (*******************************************)

    MDTAG : OPTYPE ;

    (*******************************************)
    (* MULTIPLY/DIVIDE TAG                     *)
    (*******************************************)

    HEAPMARK : -> INTEGER ;
    TESTCNT : INTEGER ;
    ZEROBL : LVLDSP ;

    (*******************************************)
    (* TO CLEAR BASE ,DISPLACEMENT FIELDS      *)
    (*******************************************)

    TOTALBYTES , ERRORCNT : INTEGER ;

    (*******************************************)
    (* TOTAL ERROR COUNT, ALSO RETURN CODE     *)
    (* COUNT OF 370-ONLY INSTRUCTIONS GENERATED*)
    (*******************************************)

    S370CNT : INTEGER ;

    (*******************************************)
    (* SET <=> INTEGER <=> REAL, 370 IMPL.ONLY *)
    (*******************************************)

    I_S_R : record
              case INTEGER of
                1 :
                  ( I1 : INTEGER ;
                    I2 : INTEGER ) ;
                2 :
                  ( R : REAL ) ;
                3 :
                  ( C1 , C2 , C3 , C4 : CHAR ) ;
                4 :
                  ( S : SHORT_SET ) ;
            end ;
    TYPCDE : array [ 'A' .. 'Z' ] of DATATYPE ;

    (**************************)
    (* ENCODING OF TYPE FIELD *)
    (**************************)

    STK : array [ STKPTR ] of DATUM ;

    (*********************************)
    (* EXPRESSION STACK              *)
    (*********************************)

    AVAIL : array [ 0 .. RGCNT ] of BOOLEAN ;

    (*********************************)
    (*AVAILABLE REGISTERS            *)
    (*********************************)

    AVAILFP : array [ 0 .. FPCNT ] of BOOLEAN ;

    (*********************************)
    (* AVAIL. F.P. REGS              *)
    (*********************************)

    INVBRM : array [ PEQU .. PLES ] of PEQU .. PLES ;

    (****************************)
    (* INV. MAP OF REL. OPCODES *)
    (****************************)

    BRMSK : array [ PEQU .. PLES ] of 0 .. 15 ;

    (****************************)
    (* 370 CONDITION CODES      *)
    (****************************)

    BRCND : - 1 .. 15 ;

    (****************************)
    (* ACTIVE BRANCH MASK       *)
    (****************************)

    TIMER : POSINT ;
    HEXCHARS : array [ 0 .. 15 ] of CHAR ;

    (***************************)
    (* HASH TABLE, INST./PROCS *)
    (***************************)

    HTBL : array [ 0 .. HTSIZE ] of record
                                      NAME : BETA ;
                                      case BOOLEAN of
                                        FALSE :
                                          ( OPCDE : OPTYPE ) ;
                                        TRUE :
                                          ( SPCDE : CSPTYPE )
                                    end ;

    (***************************************)
    (* REMEMBERS USEFUL COND-CODE MEANINGS *)
    (***************************************)

    LAST_CC : record
                LPC : ICRNG ;
                LOP : BYTE ;
                LR : RGRNG
              end ;

    (********************************)
    (* REMEMBERS CONTENTS OF REG 14 *)
    (********************************)

    TXR_CONTENTS : record
                     VALID : BOOLEAN ;
                     LEVEL : LVLRNG ;
                     OFFSET , DISP : ADRRNG ;
                     BASE : RGRNG
                   end ;

    (**************************************)
    (* REMEMBERS OPNDS OF LAST STR INSTR. *)
    (**************************************)

    LAST_STR : record
                 STOPND : LVLDSP ;
                 STRGX : RGRNG ;
                 LPC : ICRNG ;
                 STDT : DATATYPE ;
               end ;

    (****************************)
    (* REMEMBERS LAST FILE USED *)
    (****************************)

    LAST_FILE : record
                  LPC : ICRNG ;
                  LFOPND : LVLDSP ;
                  LFV : BOOLEAN
                end ;

    (**********************************)
    (* REMEMBERS LAST MVC INSTRUCTION *)
    (**********************************)

    LAST_MVC : record
                 LPC : ICRNG ;
                 LLEN : BYTE ;
               end ;

    (******************************************************)
    (* POINTERS TO LAST ELEMENTS OF 'OBJECT' CODE TABLES  *)
    (******************************************************)

    NXTPRC , NXTEP : 0 .. PRCCNT ;
    HEXPC : HEX4 ;

    (**************************************************)
    (* PROGRAM COUNTER DIV 2                          *)
    (**************************************************)

    PCOUNTER : ICRNG ;

    (***************************************************)
    (* Pcounter FOR CONSTANT BLOCK                     *)
    (***************************************************)

    CPCOUNTER : HINTEGER ;

    (***************************************************)
    (* START FOR CPCOUNTER IN CURRENT SEGMENT          *)
    (***************************************************)

    CSEGSTRT : HINTEGER ;

    (***************************************************)
    (* END FOR CPCOUNTER IN CURRENT SEGMENT            *)
    (***************************************************)

    CSEGLIMIT : HINTEGER ;
    MINLBL : LBLRNG ;

    (**************************************************)
    (* STARTING LABEL VALUE FOR CURRENT PROC          *)
    (* DECLARATIONS FOR LITERAL TABLES ...ETC. NEEDED *)
    (* TO GENERATE OBJECT MODULE                      *)
    (**************************************************)

    CST_CURPNAME : array [ 1 .. IDLNGTH ] of CHAR ;
    CST_CURPNO : INTEGER ;
    CST_ASMVERB : BOOLEAN ;
    CST_GET_STAT : BOOLEAN ;
    MATCH_CURPNO : INTEGER ;

    (**********************************)
    (*CURRENT PROC #                  *)
    (**********************************)

    NXTLIT : - 1 .. LITCNT ;

    //**************************************************************
    // this structure contains all the fields                       
    // which are used to control the literal pool                   
    // see type definition above                                    
    //**************************************************************

    LX : LITERAL_POOL_CONTROL ;

    //**************************************************************
    // code array to hold generated code for procedure              
    //**************************************************************

    CODE : record
             case INTEGER of
               1 :
                 ( C : array [ 0 .. CODE_BYTES ] of CHAR ) ;
               2 :
                 ( H : array [ 0 .. CODE_HINTS ] of HINTEGER ) ;
               3 :
                 ( I : array [ 0 .. CODE_INTS ] of INTEGER ) ;
               4 :
                 ( R : array [ 0 .. CODE_REALS ] of REAL ) ;
               5 :
                 ( TXTCARD : array [ 0 .. CODE_CHUNKS ] of array [ 1 ..
                             SIZE_TXTCHUNK ] of CHAR ) ;
           end ;

    //**************************************************************
    // literal pool for procedure                                   
    //**************************************************************

    IDP_POOL : record
                 case INTEGER of
                   1 :
                     ( C : array [ 0 .. CHCNT ] of CHAR ) ;
                   2 :
                     ( H : array [ 0 .. HWCNT ] of HINTEGER ) ;
                   3 :
                     ( I : array [ 0 .. INTCNT ] of INTEGER ) ;
                   4 :
                     ( R : array [ 0 .. DBLCNT ] of REAL ) ;
                   5 :
                     ( S : array [ 0 .. DBLCNT ] of SHORT_SET ) ;
               end ;

    //**************************************************************
    // literal vector for literals                                  
    // ltype = type of literal                                      
    // length = length                                              
    // xidp = index into literal pool                               
    // lnk = link into code array or zero, if notused               
    //**************************************************************

    LITTBL : array [ 1 .. LITCNT ] of record
                                        LTYPE : CHAR ;
                                        LENGTH : HINTEGER ;
                                        XIDP : INTEGER ;
                                        LNK : ICRNG_EXT ;
                                      end ;
    LBLTBL : array [ 0 .. LBLCNT ] of record
                                        DEFINED : BOOLEAN ;
                                        LNK : ICRNG_EXT
                                      end ;
    PRCTBL : array [ 0 .. PRCCNT ] of record
                                        NAME : ALFA ;
                                        LNK : ADRRNG_EXT ;
                                        VPOS : ICRNG
                                      end ;
    CALSTK : array [ 1 .. MAXCALDPTH ] of record
                                            PFLEV : INTEGER ;
                                            DISPSAV : ADRRNG
                                          end ;

    (*******************************************************)
    (* PROGRAM HEADER/DATE/TIME                            *)
    (*******************************************************)

    PROGHDR : array [ 1 .. HDRLNGTH ] of CHAR ;

    (*******************************************************)
    (* ASMOUT = Datei fuer ASSEMBLER-Ausgabe               *)
    (* STATNAME = Name der Static Csect (falls vorhanden)  *)
    (* posofproclen = Position des ProcLen-Feldes          *)
    (*******************************************************)

    OBJCODE : TEXT ;
    ASMOUT : TEXT ;
    TRACEF : TEXT ;
    POSOFPROCLEN : ICRNG ;

    (*******************************************************)
    (*____________________________________________________ *)
    (*******************************************************)



const HEXTAB : array [ 0 .. 15 ] of CHAR = '0123456789abcdef' ;
      DATNULL : DATUM =
      ( 0.0 , NIL , 0 , 0 , ( 0 , 0 ) , 0 , FALSE , FALSE , ' ' , NON ,
        NEITHER ) ;
      XTBLN : MNEM_TABLE =
      ( '(00)' , '(01)' , '(02)' , '(03)' , 'SPM ' , 'BALR' , 'BCTR' ,
        'BCR ' , 'SSK ' , 'ISK ' , 'SVC ' , '(0B)' , '(0C)' , '(0D)' ,
        'MVCL' , 'CLCL' , 'LPR ' , 'LNR ' , 'LTR ' , 'LCR ' , 'NR  ' ,
        'CLR ' , 'OR  ' , 'XR  ' , 'LR  ' , 'CR  ' , 'AR  ' , 'SR  ' ,
        'MR  ' , 'DR  ' , 'ALR ' , 'SLR ' , 'LPDR' , 'LNDR' , 'LTDR' ,
        'LCDR' , 'HDR ' , 'LRDR' , 'MXR ' , 'MXDR' , 'LDR ' , 'CDR ' ,
        'ADR ' , 'SDR ' , 'MDR ' , 'DDR ' , 'AWR ' , 'SWR ' , 'LPER' ,
        'LNER' , 'LTER' , 'LCER' , 'HER ' , 'LRER' , 'AXR ' , 'SXR ' ,
        'LER ' , 'CER ' , 'AER ' , 'SER ' , 'MER ' , 'DER ' , 'AUR ' ,
        'SUR ' , 'STH ' , 'LA  ' , 'STC ' , 'IC  ' , 'EX  ' , 'BAL ' ,
        'BCT ' , 'BC  ' , 'LH  ' , 'CH  ' , 'AH  ' , 'SH  ' , 'MH  ' ,
        '(4D)' , 'CVD ' , 'CVB ' , 'ST  ' , '(51)' , '(52)' , '(53)' ,
        'N   ' , 'CL  ' , 'O   ' , 'X   ' , 'L   ' , 'C   ' , 'A   ' ,
        'S   ' , 'M   ' , 'D   ' , 'AL  ' , 'SL  ' , 'STD ' , '(61)' ,
        '(62)' , '(63)' , '(64)' , '(65)' , '(66)' , 'MXD ' , 'LD  ' ,
        'CD  ' , 'AD  ' , 'SD  ' , 'MD  ' , 'DD  ' , 'AW  ' , 'SW  ' ,
        'STE ' , '(71)' , '(72)' , '(73)' , '(74)' , '(75)' , '(76)' ,
        '(77)' , 'LE  ' , 'CE  ' , 'AE  ' , 'SE  ' , 'ME  ' , 'DE  ' ,
        'AU  ' , 'SU  ' , 'SSM ' , '(81)' , 'LPSW' , 'DIAG' , 'WRD ' ,
        'RDD ' , 'BXH ' , 'BXLE' , 'SRL ' , 'SLL ' , 'SRA ' , 'SLA ' ,
        'SRDL' , 'SLDL' , 'SRDA' , 'SLDA' , 'STM ' , 'TM  ' , 'MVI ' ,
        'TS  ' , 'NI  ' , 'CLI ' , 'OI  ' , 'XI  ' , 'LM  ' , '(99)' ,
        '(9A)' , '(9B)' , 'SIO ' , 'TIO ' , 'HIO ' , 'TCH ' , '(A0)' ,
        '(A1)' , '(A2)' , '(A3)' , '(A4)' , '(A5)' , '(A6)' , '(A7)' ,
        '(A8)' , '(A9)' , '(AA)' , '(AB)' , '(AC)' , '(AD)' , '(AE)' ,
        '(AF)' , '(B0)' , 'LRA ' , 'STCK' , '(B3)' , '(B4)' , '(B5)' ,
        'STCT' , 'LCTL' , '(B8)' , '(B9)' , '(BA)' , '(BB)' , '(BC)' ,
        'CLM ' , 'STCM' , 'ICM ' , '(C0)' , '(C1)' , '(C2)' , '(C3)' ,
        '(C4)' , '(C5)' , '(C6)' , '(C7)' , '(C8)' , '(C9)' , '(CA)' ,
        '(CB)' , '(CC)' , '(CD)' , '(CE)' , '(CF)' , '(D0)' , 'MVN ' ,
        'MVC ' , 'MVZ ' , 'NC  ' , 'CLC ' , 'OC  ' , 'XC  ' , '(D8)' ,
        '(D9)' , '(DA)' , '(DB)' , 'TR  ' , 'TRT ' , 'ED  ' , 'EDMK' ,
        '(E0)' , '(E1)' , '(E2)' , '(E3)' , '(E4)' , '(E5)' , '(E6)' ,
        '(E7)' , '(E8)' , '(E9)' , '(EA)' , '(EB)' , '(EC)' , '(ED)' ,
        '(EE)' , '(EF)' , 'SRP ' , 'MVO ' , 'PACK' , 'UNPK' , '(F4)' ,
        '(F5)' , '(F6)' , '(F7)' , 'ZAP ' , 'CP  ' , 'AP  ' , 'SP  ' ,
        'MP  ' , 'DP  ' , '(FE)' , '(FF)' ) ;
      PTBL : array [ OPTYPE ] of BETA =
      ( 'CTS' , 'CTI' , 'LOD' , 'STR' , 'LDA' , 'LOC' , 'STO' , 'LDC' ,
        'LAB' , 'IND' , 'INC' , 'POP' , 'CUP' , 'ENT' , 'RET' , 'CSP' ,
        'IXA' , 'EQU' , 'NEQ' , 'GEQ' , 'GRT' , 'LEQ' , 'LES' , 'UJP' ,
        'FJP' , 'XJP' , 'CHK' , 'NEW' , 'ADI' , 'ADR' , 'SBI' , 'SBR' ,
        'SCL' , 'FLT' , 'FLO' , 'NGI' , 'NGR' , 'SQI' , 'SQR' , 'ABI' ,
        'ABR' , 'NOT' , 'AND' , 'IOR' , 'DIF' , 'INT' , 'UNI' , 'INN' ,
        'MOD' , 'ODD' , 'MPI' , 'MPR' , 'DVI' , 'DVR' , 'MOV' , 'LCA' ,
        'DEC' , 'STP' , 'SAV' , 'RST' , 'CHR' , 'ORD' , 'DEF' , 'CRD' ,
        'XPO' , 'BGN' , 'END' , 'ASE' , 'SLD' , 'SMV' , 'MST' , 'UXJ' ,
        'XLB' , 'CST' , 'DFC' , 'PAK' , 'ADA' , 'SBA' , 'XOR' , 'MFI' ,
        'MCP' , 'MSE' , 'DBG' , 'MZE' , 'VC1' , 'VC2' , 'VCC' , 'VLD' ,
        'VST' , 'VMV' , 'VSM' , 'VLM' , 'VPU' , 'VPO' , 'VIX' , 'VRP' ,
        '-?-' ) ;
      CSPTBL : array [ CSPTYPE ] of BETA =
      ( 'N00' , 'N01' , 'N02' , 'N03' , 'N04' , 'N05' , 'N06' , 'N07' ,
        'N08' , 'N09' , 'PAG' , 'GET' , 'PUT' , 'RES' , 'REW' , 'RDC' ,
        'WRI' , 'WRE' , 'WRR' , 'WRC' , 'WRS' , 'WRX' , 'RDB' , 'WRB' ,
        'RDR' , 'RDH' , 'RDY' , 'EOL' , 'EOT' , 'RDD' , 'WRD' , 'CLK' ,
        'WLN' , 'RLN' , 'RDI' , 'EOF' , 'ELN' , 'RDS' , 'TRP' , 'XIT' ,
        'FDF' , 'SIO' , 'EIO' , 'MSG' , 'SKP' , 'LIM' , 'TRA' , 'WRP' ,
        'CLS' , 'DAT' , 'TIM' , 'FLR' , 'TRC' , 'RND' , 'WRV' , '-?-' )
        ;



function MEMCMPX ( X : ANYPTR ; Y : ANYPTR ; L : INTEGER ) : INTEGER ;

   var PLINKS : -> CHAR ;
       PRECHTS : -> CHAR ;
       PLIMIT : -> CHAR ;
       RESULT : INTEGER ;

   begin (* MEMCMPX *)
     PLINKS := X ;
     PRECHTS := Y ;
     PLIMIT := PTRADD ( PLINKS , L ) ;
     RESULT := 0 ;
     while PTRDIFF ( PLIMIT , PLINKS ) > 0 do
       begin
         if PLINKS -> < PRECHTS -> then
           begin
             RESULT := - 1 ;
             break
           end (* then *)
         else
           if PLINKS -> > PRECHTS -> then
             begin
               RESULT := 1 ;
               break
             end (* then *)
           else
             begin
               PLINKS := PTRADD ( PLINKS , 1 ) ;
               PRECHTS := PTRADD ( PRECHTS , 1 ) ;
             end (* else *)
       end (* while *) ;
     MEMCMPX := RESULT
   end (* MEMCMPX *) ;



procedure WRITEHEXBYTE ( var F : TEXT ; I : INTEGER ) ;

   begin (* WRITEHEXBYTE *)
     WRITE ( F , HEXTAB [ I DIV 16 ] , HEXTAB [ I MOD 16 ] ) ;
   end (* WRITEHEXBYTE *) ;



procedure WRITEBINBYTE ( var F : TEXT ; I : INTEGER ) ;

   var X : INTEGER ;
       Y : INTEGER ;

   begin (* WRITEBINBYTE *)
     X := 128 ;
     for Y := 1 to 8 do
       begin
         if I >= X then
           begin
             WRITE ( F , '1' ) ;
             I := I - X ;
           end (* then *)
         else
           WRITE ( F , '0' ) ;
         X := X DIV 2 ;
       end (* for *)
   end (* WRITEBINBYTE *) ;



procedure ERROR ( ERRCDE : INTEGER ) ;

   begin (* ERROR *)
     ERRORCNT := ERRORCNT + 1 ;
     WRITELN ( OUTPUT , '   ++++ PERROR ' , ERRCDE : 5 , ' (NEAR LINE '
               , LASTLN : 6 , ' OF PROCEDURE ' , PIAKT -> . CURPNAME ,
               ')' ) ;
     if ERRCDE = 253 then
       WRITELN ( OUTPUT , ' ' : 8 , 'PROCEDURE TOO LARGE.' ) ;
     if ERRCDE = 254 then
       WRITELN ( OUTPUT , ' ' : 8 , 'PROCEDURE TOO LARGE.' ) ;
     if ERRCDE = 255 then
       WRITELN ( OUTPUT , ' ' : 8 , 'PROCEDURE TOO LARGE.' ) ;
     if ERRCDE = 256 then
       WRITELN ( OUTPUT , ' ' : 8 ,
                 'TOO MANY PROC/FUNC CALLS IN THIS PROC.' ) ;
     if ERRCDE = 259 then
       WRITELN ( OUTPUT , ' ' : 8 , 'EXPRESSION TOO COMPLICATED.' ) ;
     if ERRCDE = 263 then
       WRITELN ( OUTPUT , ' ' : 8 ,
                 'TOO MANY CONTROL JUMPS IN THIS PROC.' ) ;
     if ERRCDE = 300 then
       WRITELN ( OUTPUT , ' ' : 8 , 'IMPLIED DIVISION BY ZERO.' ) ;
     if ERRCDE = 301 then
       WRITELN ( OUTPUT , ' ' : 8 , 'RANGE ERROR IN STRUCTURED CONST.'
                 ) ;
     if ERRCDE = 302 then
       WRITELN ( OUTPUT , ' ' : 8 , 'IMPLIED SUBSCRIPTRANGE ERROR.' ) ;
     if ERRCDE = 303 then
       WRITELN ( OUTPUT , ' ' : 8 , 'ILLEGAL CONSTANT SET ASSMT.' ) ;
     if ERRCDE = 504 then
       WRITELN ( OUTPUT , ' ' : 8 , 'ARRAY COMPONENT TOO LARGE (>32K).'
                 ) ;
     if ERRCDE = 618 then
       WRITELN ( OUTPUT , ' ' : 8 , 'UNEXPECTED EOL IN P-CODE INPUT' )
                 ;
   end (* ERROR *) ;



procedure CHECKFREEREGS ;

(***********************************************************)
(* TO BE INVOKED WHEN COMPILATION STACK IS EMPTY,          *)
(* CHECKS THAT ALL REGS HAVE BEEN MARKED AS AVAILABLE      *)
(***********************************************************)


   var LIST : array [ 1 .. 12 ] of record
                                     RGNO : RGRNG ;
                                     KIND : CHAR
                                   end ;
       LP : 0 .. 12 ;
       I : RGRNG ;

   begin (* CHECKFREEREGS *)
     if TOP <> 1 then
       begin
         WRITELN ( OUTPUT , '****' : 7 , ' WARNING: STACK HEIGHT =' ,
                   TOP : 3 ) ;
         TOP := 1
       end (* then *) ;
     I := 1 ;
     LP := 0 ;
     repeat
       I := I + 1 ;
       if not AVAIL [ I ] then
         if I <> CALLSTACKADR then
           begin
             LP := LP + 1 ;
             LIST [ LP ] . RGNO := I ;
             LIST [ LP ] . KIND := 'G' ;
             AVAIL [ I ] := TRUE ;
           end (* then *) ;
     until I >= RGCNT ;
     I := 0 ;
     repeat
       I := I + 2 ;
       if not AVAILFP [ I ] then
         begin
           LP := LP + 1 ;
           LIST [ LP ] . RGNO := I ;
           LIST [ LP ] . KIND := 'F' ;
           AVAILFP [ I ] := TRUE ;
         end (* then *) ;
     until I >= FPCNT ;
     if LP > 0 then
       begin
         WRITELN ( OUTPUT , '****' : 7 ,
                   ' WARNING: REGISTERS NOT FREED ' ) ;
         for I := 1 to LP do
           WRITE ( OUTPUT , LIST [ I ] . KIND : 8 , 'PR' , LIST [ I ] .
                   RGNO : 3 ) ;
         WRITELN ( OUTPUT ) ;
         WRITELN ( OUTPUT , '(NEAR LINE' : 34 , LASTLN : 6 ,
                   'OF PROCEDURE:' : 15 , PIAKT -> . CURPNAME , ')' ) ;
       end (* then *) ;
   end (* CHECKFREEREGS *) ;



function TO_HINT ( X : INTEGER ) : INTEGER ;

   begin (* TO_HINT *)
     X := X & 0xffff ;
     if X > 0x8000 then
       X := X - 0x10000 ;
     TO_HINT := X ;
   end (* TO_HINT *) ;



procedure ENTERLOOKUP ;

   const STEP = 17 ;     // MUST BE COPRIME TO HTSIZE

   var H : INTEGER ;     // was 0 .. HTSIZE, changed due to rangeerr

   begin (* ENTERLOOKUP *)
     H := ( ORD ( NMCDE [ 1 ] ) * 64 +         // hashcode part 1
          ORD ( NMCDE [ 2 ] ) * 4096 +         // hashcode part 2
          ORD ( NMCDE [ 3 ] ) ) MOD HTSIZE ;   // hashcode part 3
     repeat
       with HTBL [ H ] do
         if NAME <> NMCDE then
           if NAME <> EMPTY then
             begin
               H := H + STEP ;
               if H >= HTSIZE then
                 H := H - HTSIZE ;
               continue ;

     (************************)
     (* NO CHECK FOR CYCLES! *)
     (************************)

             end (* then *)
           else
             if INIT then
               begin

     (******************)
     (* ENTER THE ITEM *)
     (******************)

                 NAME := NMCDE ;
                 if OP_SP then
                   OPCDE := PCODE
                 else
                   SPCDE := CSP
               end (* then *)
             else
               if OP_SP then
                 PCODE := UNDEF_OP
               else
                 CSP := UNDEF_CSP
         else
           if OP_SP then
             PCODE := OPCDE
           else
             CSP := SPCDE ;
       break ;
     until FALSE ;
   end (* ENTERLOOKUP *) ;



function FLDW ( NUM : INTEGER ) : INTEGER ;

   var FW : INTEGER ;

   begin (* FLDW *)
     FW := 0 ;
     if NUM < 0 then
       begin
         FW := 1 ;
         NUM := ABS ( NUM ) ;
       end (* then *) ;
     repeat
       NUM := NUM DIV 10 ;
       FW := FW + 1 ;
     until NUM = 0 ;
     FLDW := FW
   end (* FLDW *) ;



procedure DUMPSTKELEM ( STK : DATUM ) ;

   const TYPNAME : array [ BOOL .. VARC ] of array [ 1 .. 4 ] of CHAR =
         ( 'BOOL' , 'CHR ' , 'ADR ' , 'HINT' , 'INT ' , 'SET ' , 'REAL'
           , 'PROC' , 'CARR' , 'VARC' ) ;

   begin (* DUMPSTKELEM *)
     with STK do
       begin
         WRITE ( TRACEF , ' STKADR=' , STKADR : 5 , ' PLEN=' , PLEN : 3
                 , ' SCNSTNO=' , SCNSTNO : 3 , ' FPA=' , FPA . LVL : 3
                 , FPA . DSPLMT : 6 , ' VPA=' , VPA ) ;
         if VRBL then
           begin
             WRITELN ( TRACEF ) ;
             WRITE ( TRACEF , '    ' ) ;
             if VPA = RGS then
               WRITE ( TRACEF , ' VPA-REG =' , RGADR : 3 )
             else
               WRITE ( TRACEF , ' VPA-MEM =' , MEMADR . LVL : 3 ,
                       MEMADR . DSPLMT : 6 ) ;
             if DRCT then
               WRITE ( TRACEF , ' DIRECT' )
             else
               WRITE ( TRACEF , ' INDIR.' ) ;
           end (* then *) ;
         if DTYPE <= VARC then
           WRITE ( TRACEF , '(' : 2 , TYPNAME [ DTYPE ] , ')' )
         else
           WRITE ( TRACEF , '(ETC.)' : 8 ) ;
         WRITE ( TRACEF , ' ' , PROCNAME ) ;
         WRITELN ( TRACEF ) ;
       end (* with *) ;
   end (* DUMPSTKELEM *) ;



procedure DUMPSTK ( STP1 , STP2 : STKPTR ) ;

   var I : STKPTR ;

   begin (* DUMPSTK *)
     WRITE ( TRACEF , 'Available Regs: ' ) ;
     for I := 1 to 7 do
       if AVAIL [ I ] then
         WRITE ( TRACEF , I : 3 ) ;
     WRITELN ( TRACEF ) ;
     for I := STP1 to STP2 do
       begin
         WRITE ( TRACEF , ' +++ DEPTH=' , I : 2 ) ;
         DUMPSTKELEM ( STK [ I ] ) ;
       end (* for *)
   end (* DUMPSTK *) ;



procedure HEXHW ( HW : HINTEGER ; var HEX : HEX4 ) ;

(*************************************************)
(* CONVERTS HALFWORD TO 4 HEXADECIMAL CHARACTERS *)
(*************************************************)


   var C : INTEGER ;
       N : 1 .. 4 ;

   begin (* HEXHW *)
     C := 65536 + HW ;

     (************************)
     (* ELIMINATES HW<0 CASE *)
     (************************)

     for N := 4 DOWNTO 1 do
       begin
         HEX [ N ] := HEXCHARS [ C MOD 16 ] ;
         C := C DIV 16
       end (* for *) ;
   end (* HEXHW *) ;



procedure READNXTINST ( MODUS : INTEGER ) ;

(*****************************************)
(* TO READ AND DECODE NEXT P_INSTRUCTION *)
(* ------------------------------------- *)
(*****************************************)


   const SL16 = 65536 ;

   var I , J , K : INTEGER ;
       DUMMYCH , CH1 : CHAR ;
       TEMPLBL : array [ 1 .. IDLNGTH ] of CHAR ;
       HLOC : HEX4 ;
       LEN : INTEGER ;
       BUFFER : CHAR80 ;
       OUTPOS : INTEGER ;
       BUF20 : CHAR80 ;
       LSTART : INTEGER ;
       CP1 : -> CHAR ;
       CP2 : -> CHAR ;
       X1 : INTEGER ;
       DUMMYNAME : array [ 1 .. IDLNGTH ] of CHAR ;
       DUMMYINT : INTEGER ;
       DUMMYBOOL : BOOLEAN ;
       DUMMYLABEL : PLABEL ;


   procedure READLBL ( var LBL : PLABEL ) ;

   (*******************************************************)
   (* SKIPS LEADING BLANKS AND READS THE NEXT             *)
   (* CHARACTER SEQUENCE AS A LABEL                       *)
   (* --------------------------------------------------- *)
   (*******************************************************)


      var I : INTEGER ;
          CH : CHAR ;

      begin (* READLBL *)
        with LBL do
          begin
            CADDR := 0 ;
            NAM := '        ' ;
            LEN := 0 ;
            if EOL ( INPUT ) then
              ERROR ( 618 ) ;
            repeat
              READ ( CH ) ;
              LEN := LEN + 1 ;
              NAM [ LEN ] := CH ;
            until ( INPUT -> = ' ' ) or ( LEN = 8 ) ;
            if NAM [ 1 ] in [ '0' .. '9' ] then
              begin
                I := 1 ;
                CH := NAM [ 1 ] ;
                repeat
                  CADDR := CADDR * 10 + ORD ( CH ) - ORD ( '0' ) ;
                  I := I + 1 ;
                  CH := NAM [ I ] ;
                until not ( CH in [ '0' .. '9' ] ) ;
              end (* then *) ;
          end (* with *) ;
      end (* READLBL *) ;


   function HEXVALUE ( C : CHAR ) : INTEGER ;

      begin (* HEXVALUE *)
        if C in [ '0' .. '9' ] then
          HEXVALUE := ORD ( C ) - ORD ( '0' )
        else
          if C in [ 'A' .. 'F' ] then
            HEXVALUE := ORD ( C ) - ORD ( 'A' ) + 10
          else
            if C in [ 'a' .. 'f' ] then
              HEXVALUE := ORD ( C ) - ORD ( 'a' ) + 10
      end (* HEXVALUE *) ;


   procedure READSET ;

      var CH1 : CHAR ;
          CH2 : CHAR ;
          I : INTEGER ;
          X : set of CHAR ;
          Z : INTEGER ;

      begin (* READSET *)
        READ ( CH , CH ) ;

        (****************************)
        (* typ = e - d.h. empty set *)
        (****************************)

        if CH = 'E' then
          begin
            PSLNGTH := 0 ;
            READLN ( INPUT ) ;
            if ASM then
              WRITELN ( ASMOUT , '  E()' ) ;
            if FALSE then
              WRITELN ( TRACEF , '  E()' ) ;
            return
          end (* then *) ;

        (******************************************)
        (* typ = x - d.h. hexadezimaler bitstring *)
        (******************************************)

        if CH = 'X' then
          begin
            READ ( PSLNGTH ) ;
            Z := 30 ;
            READ ( CH ) ;
            if ASM then
              WRITE ( ASMOUT , '  S,X' , PSLNGTH : 1 , '''' ) ;
            if FALSE then
              WRITE ( TRACEF , '  S,X' , PSLNGTH : 1 , '''' ) ;
            I := 0 ;
            while TRUE do
              begin
                READ ( CH1 ) ;
                if CH1 = '''' then
                  begin
                    if INPUT -> <> ',' then
                      begin
                        if FALSE then
                          WRITELN ( TRACEF , '''' ) ;
                        break ;
                      end (* then *) ;
                    READLN ( INPUT ) ;
                    repeat
                      READ ( CH ) ;
                    until CH = '''' ;
                    continue ;
                  end (* then *) ;
                READ ( CH2 ) ;
                if FALSE then
                  WRITE ( TRACEF , CH1 , CH2 ) ;
                I := I + 1 ;
                PSVAL . C [ I ] := CHR ( HEXVALUE ( CH1 ) * 16 +
                                   HEXVALUE ( CH2 ) ) ;
                if Z >= 70 then
                  begin
                    if ASM then
                      begin
                        WRITELN ( ASMOUT , ''',' ) ;
                        WRITE ( ASMOUT , '''' : 27 ) ;
                      end (* then *) ;
                    Z := 27 ;
                  end (* then *) ;
                if ASM then
                  WRITE ( ASMOUT , CH1 , CH2 ) ;
                if FALSE then
                  WRITE ( TRACEF , CH1 , CH2 ) ;
                Z := Z + 2 ;
              end (* while *) ;
            PSLNGTH := I ;
            READLN ( INPUT ) ;
            if ASM then
              WRITELN ( ASMOUT , '''' ) ;
            return
          end (* then *) ;

        (******************************************)
        (* typ = c - d.h. char-string             *)
        (******************************************)

        if CH = 'C' then
          begin
            READ ( PSLNGTH ) ;
            Z := 30 ;
            READ ( CH ) ;
            if ASM then
              WRITE ( ASMOUT , '  S,C' , PSLNGTH : 1 , '''' ) ;
            if FALSE then
              WRITE ( TRACEF , '  S,C' , PSLNGTH : 1 , '''' ) ;
            X := [ ] ;
            while TRUE do
              begin
                READ ( CH ) ;
                if CH = '''' then
                  begin
                    CH := INPUT -> ;
                    if CH = '''' then
                      READ ( CH )
                    else
                      if CH = ',' then
                        begin
                          READLN ( INPUT ) ;
                          repeat
                            READ ( CH ) ;
                          until CH = '''' ;
                          continue ;
                        end (* then *)
                      else
                        break ;
                  end (* then *) ;
                X := X + [ CH ] ;
                if Z >= 70 then
                  begin
                    if ASM then
                      begin
                        WRITELN ( ASMOUT , ''',' ) ;
                        WRITE ( ASMOUT , '''' : 27 ) ;
                      end (* then *) ;
                    Z := 27 ;
                  end (* then *) ;
                if ASM then
                  begin
                    WRITE ( ASMOUT , CH ) ;
                    Z := Z + 1 ;
                    if CH = '''' then
                      begin
                        WRITE ( ASMOUT , CH ) ;
                        Z := Z + 1 ;
                      end (* then *)
                  end (* then *) ;
                if FALSE then
                  begin
                    WRITE ( TRACEF , CH ) ;
                    if CH = '''' then
                      WRITE ( TRACEF , CH ) ;
                  end (* then *) ;
              end (* while *) ;
            MEMCPY ( ADDR ( PSVAL ) , ADDR ( X ) , PSLNGTH ) ;
            READLN ( INPUT ) ;
            if ASM then
              WRITELN ( ASMOUT , '''' ) ;
            return
          end (* then *) ;
      end (* READSET *) ;


   procedure SKIPBLANKS ;

      begin (* SKIPBLANKS *)
        GET ( INPUT ) ;
        if EOL ( INPUT ) then
          ERROR ( 618 ) ;
      end (* SKIPBLANKS *) ;


   procedure READLOADINSTRUCTIONS ;

      var TYPETAG : CHAR ;
          INEU : INTEGER ;
          X2 : INTEGER ;
          LLIMIT : INTEGER ;

      begin (* READLOADINSTRUCTIONS *)
        SKIPBLANKS ;

        (*******************************************************)
        (* TYPE-CODE, CONSTANT OPERANDS                        *)
        (* with type-code = m:                                 *)
        (* length (optional) and  string constant              *)
        (* the string constant may be split over multiple      *)
        (* lines and may be prefixed by B or X for binary      *)
        (* or hex content                                      *)
        (*******************************************************)

        if ( PCODE = PDFC ) and ( INPUT -> = '0' ) then
          begin
            OPNDTYPE := NON ;
            READ ( CH1 ) ;
            READLN ( CH , IVAL ) ;
            SLNGTH := IVAL ;
            if ASM then
              WRITELN ( ASMOUT , CH1 : 3 , ',' , IVAL : 1 ) ;
          end (* then *)
        else
          begin
            OPNDTYPE := TYPCDE [ INPUT -> ] ;
            READ ( CH1 ) ;
            case OPNDTYPE of
              HINT , BOOL , INT :
                begin
                  READLN ( CH , IVAL ) ;
                  if ASM then
                    WRITELN ( ASMOUT , CH1 : 3 , ',' , IVAL : 1 ) ;
                end (* tag/ca *) ;
              CHRC : begin
                       READLN ( CH , CH , CH ) ;
                       IVAL := ORD ( CH ) ;
                       if ASM then
                         WRITELN ( ASMOUT , 'C,''' : 5 , CH , '''' ) ;
                     end (* tag/ca *) ;
              REEL : begin
                       READLN ( CH , RVAL ) ;
                       if ASM then
                         WRITELN ( ASMOUT , 'R,' : 4 , RVAL : 20 ) ;
                     end (* tag/ca *) ;
              ADR : begin
                      READLN ( INPUT ) ;
                      IVAL := - 1 ;
                      if ASM then
                        WRITELN ( ASMOUT , 'NIL' : 4 ) ;
                    end (* tag/ca *) ;
              PSET : begin
                       READSET
                     end (* tag/ca *) ;
              PROC : begin
                       READ ( CH ) ;
                       READLBL ( LBL2 ) ;
                       READLN ( INPUT ) ;
                       if ASM then
                         WRITELN ( ASMOUT , 'P,' : 4 , LBL2 . NAM :
                                   LBL2 . LEN ) ;
                     end (* tag/ca *) ;
              CARR : begin
                       LEN := - 1 ;
                       READ ( CH ) ;

        /************************************/
        /* read optional length information */
        /************************************/

                       if not ( INPUT -> in [ '''' , 'B' , 'X' ] ) then
                         begin
                           READ ( LEN ) ;
                           READ ( CH ) ;
                         end (* then *) ;

        /**************************/
        /* read optional type tag */
        /**************************/

                       READ ( CH ) ;
                       TYPETAG := ' ' ;
                       if CH in [ 'X' , 'B' ] then
                         begin
                           TYPETAG := CH ;
                           READ ( CH ) ;
                         end (* then *) ;
                       SVAL := ' ' ;
                       J := 0 ;
                       CH := '''' ;
                       repeat

        /*********************************/
        /* read rest of line into buffer */
        /*********************************/

                         READLN ( BUFFER ) ;
                         I := 80 ;
                         while BUFFER [ I ] = ' ' do
                           I := I - 1 ;
                         CH := BUFFER [ I ] ;

        /***********************************/
        /* if comma after string constant, */
        /* another part follows            */
        /***********************************/

                         if CH = ',' then
                           begin
                             I := I - 2 ;
                             SKIPBLANKS ;
                             READ ( CH ) ;
                             CH := ',' ;
                           end (* then *)
                         else
                           I := I - 1 ;

        /************************************/
        /* now move part of string constant */
        /* to result buffer (sval),         */
        /* depending on type tag            */
        /************************************/

                         K := 1 ;
                         while K <= I do
                           begin
                             J := J + 1 ;
                             case TYPETAG of
                               ' ' : begin
                                       SVAL [ J ] := BUFFER [ K ] ;
                                       if SVAL [ J ] = '''' then
                                         K := K + 2
                                       else
                                         K := K + 1 ;
                                     end (* tag/ca *) ;
                               'X' : begin
                                       INEU := 0 ;
                                       for X2 := K to K + 1 do
                                         begin
                                           INEU := INEU * 16 ;
                                           if BUFFER [ X2 ] in [ '1' ..
                                           '9' ] then
                                             INEU := INEU + ORD (
                                                   BUFFER [ X2 ] ) -
                                                   ORD ( '0' )
                                           else
                                             if BUFFER [ X2 ] in [ 'A'
                                             .. 'F' ] then
                                               INEU := INEU + ORD (
                                                   BUFFER [ X2 ] ) -
                                                   ORD ( 'A' ) + 10
                                             else
                                               if BUFFER [ X2 ] in [
                                               'a' .. 'f' ] then
                                                 INEU := INEU + ORD (
                                                   BUFFER [ X2 ] ) -
                                                   ORD ( 'a' ) + 10 ;
                                         end (* for *) ;
                                       SVAL [ J ] := CHR ( INEU ) ;
                                       K := K + 2 ;
                                     end (* tag/ca *) ;
                               'B' : begin
                                       INEU := 0 ;
                                       for X2 := K to K + 7 do
                                         begin
                                           INEU := INEU * 2 ;
                                           if BUFFER [ X2 ] = '1' then
                                             INEU := INEU + 1 ;
                                         end (* for *) ;
                                       SVAL [ J ] := CHR ( INEU ) ;
                                       K := K + 8 ;
                                     end (* tag/ca *) ;
                             end (* case *) ;
                           end (* while *) ;
                       until CH = '''' ;
                       if LEN < 0 then
                         SLNGTH := J
                       else
                         SLNGTH := LEN ;

        /************************************/
        /* show what has been read          */
        /* on asmout ...                    */
        /************************************/

                       if ASM then
                         begin
                           WRITE ( ASMOUT , '  M,' , SLNGTH : 1 , ',' )
                                   ;
                           case TYPETAG of
                             'X' : LLIMIT := SLNGTH * 2 ;
                             'B' : LLIMIT := SLNGTH * 8 ;
                             otherwise
                               LLIMIT := SLNGTH ;
                           end (* case *) ;
                           if LLIMIT < 40 then
                             begin
                               if TYPETAG <> ' ' then
                                 WRITE ( ASMOUT , TYPETAG ) ;
                               WRITE ( ASMOUT , '''' )
                             end (* then *)
                           else
                             begin
                               WRITELN ( ASMOUT ) ;
                               WRITE ( ASMOUT , '     ' ) ;
                               if TYPETAG <> ' ' then
                                 WRITE ( ASMOUT , TYPETAG ) ;
                               WRITE ( ASMOUT , '''' )
                             end (* else *) ;
                           OUTPOS := 0 ;
                           for I := 1 to SLNGTH do
                             begin
                               if OUTPOS > 60 then
                                 begin
                                   WRITELN ( ASMOUT , ''',' ) ;
                                   WRITE ( ASMOUT , '     ''' ) ;
                                   OUTPOS := 0 ;
                                 end (* then *) ;
                               case TYPETAG of
                                 'X' : begin
                                         CH := SVAL [ I ] ;
                                         WRITEHEXBYTE ( ASMOUT , ORD (
                                                   CH ) ) ;
                                         OUTPOS := OUTPOS + 2 ;
                                       end (* tag/ca *) ;
                                 'B' : begin
                                         CH := SVAL [ I ] ;
                                         WRITEBINBYTE ( ASMOUT , ORD (
                                                   CH ) ) ;
                                         OUTPOS := OUTPOS + 8 ;
                                       end (* tag/ca *) ;
                                 otherwise
                                   begin
                                     CH := SVAL [ I ] ;
                                     WRITE ( ASMOUT , CH ) ;
                                     OUTPOS := OUTPOS + 1 ;
                                     if CH = '''' then
                                       begin
                                         WRITE ( ASMOUT , CH ) ;
                                         OUTPOS := OUTPOS + 1
                                       end (* then *) ;
                                   end (* otherw *)
                               end (* case *) ;
                             end (* for *) ;
                           WRITELN ( ASMOUT , '''' ) ;
                         end (* then *) ;
                     end (* tag/ca *) ;
            end (* case *)
          end (* else *)
      end (* READLOADINSTRUCTIONS *) ;


   begin (* READNXTINST *)
     P := 0 ;
     Q := 0 ;
     LBL1 . LEN := 0 ;
     if INPUT -> <> ' ' then
       begin
         READLBL ( LBL1 ) ;
       end (* then *) ;
     GET ( INPUT ) ;
     if INPUT -> = ' ' then
       SKIPBLANKS ;
     READ ( NMCDE ) ;
     if MODUS = 1 then
       begin
         if ( NMCDE <> 'ENT' ) and ( NMCDE <> 'RET' ) and ( NMCDE <>
         'DEF' ) and ( NMCDE <> 'STP' ) then
           begin
             READLN ( INPUT ) ;
             return ;
           end (* then *) ;
       end (* then *)
     else
       if ASM and ( NMCDE <> 'LOC' ) and ( NMCDE <> 'ENT' ) then
         begin
           if NMCDE = 'DFC' then
             HEXHW ( LBL1 . CADDR , HLOC )
           else
             HEXHW ( 2 * PCOUNTER , HLOC ) ;
           WRITE ( ASMOUT , HLOC : 9 , ':  ' , LBL1 . NAM : LBL1 . LEN
                   , ' ' : 6 - LBL1 . LEN , NMCDE : 6 ) ;
         end (* then *) ;

     //************************************************************
     // achtung, nur uebergangsweise, bis V-Befehle                
     // korrekt implementiert sind                                 
     //************************************************************

     ENTERLOOKUP ;

     //************************************************************
     // achtung, nur uebergangsweise, bis V-Befehle                
     // korrekt implementiert sind                                 
     //************************************************************

     if PCODE = PENT then
       begin
         WRITELN ( TRACEF ) ;
         WRITELN ( TRACEF ) ;
         WRITELN ( TRACEF , 'Neuer Entry ' , LBL1 . NAM ) ;
         WRITELN ( TRACEF , 'TOP   = ' , TOP ) ;
         TOP := 1 ;
       end (* then *) ;

     //************************************************************
     // achtung, nur uebergangsweise, bis V-Befehle                
     // korrekt implementiert sind                                 
     // achtung, nur uebergangsweise, bis V-Befehle                
     // korrekt implementiert sind                                 
     //************************************************************

     case PCODE of
       PADI , PADR , PSBI , PSBR , PFLT , PFLO , PNGI , PNGR , PSQI ,
       PSQR , PABI , PABR , PMOD , PODD , PMPI , PMPR , PDVI , PDVR ,
       PUNI , PINT , PDIF , PINN , PCRD , PLAB , PSAV , PRST , PCHR ,
       PORD , PXPO , PPOP , PXLB , PEND , PADA , PSBA , PMCP :
         begin

     (***************)
     (* NO OPERANDS *)
     (***************)

           READLN ( INPUT ) ;
           if ASM then
             WRITELN ( ASMOUT ) ;
         end (* tag/ca *) ;
       PSTP : begin
                if MODUS = 1 then
                  begin
                    READLN ( INPUT ) ;
                    return
                  end (* then *) ;

     (***************)
     (* NO OPERANDS *)
     (***************)

                READLN ( INPUT ) ;
                if ASM then
                  WRITELN ( ASMOUT ) ;
              end (* tag/ca *) ;
       PDEF : begin
                if MODUS = 1 then
                  begin
                    SKIPBLANKS ;
                    case GS . MOD1DEFSTEP of
                      0 : begin
                            READLN ( CH , CH , Q ) ;
                            PIAKT -> . DATA_SIZE := Q ;
                            GS . MOD1DEFSTEP := 1 ;
                          end (* tag/ca *) ;
                      1 : begin
                            READLN ( CH , CH , Q ) ;
                            PIAKT -> . CODE_SIZE := Q ;
                            PIAKT -> . LARGE_PROC := ( PIAKT -> .
                                                   CODE_SIZE >
                                                   SHRT_PROC ) or DEBUG
                                                   ;
                            GS . MOD1DEFSTEP := 2 ;
                          end (* tag/ca *) ;
                      2 : begin
                            READLN ( CH , CH , Q ) ;
                            PIAKT -> . CALL_HIGHER := ( Q <> 0 ) ;
                            GS . MOD1DEFSTEP := - 1 ;
                          end (* tag/ca *) ;
                      otherwise
                        READLN
                    end (* case *) ;
                    return
                  end (* then *) ;

     (*****************************************)
     (* Type-Code and Integer or Char Operand *)
     (*****************************************)

                SKIPBLANKS ;
                if INPUT -> = 'C' then
                  begin
                    READLN ( CH , CH , CH , CH1 , CH ) ;
                    if ASM then
                      WRITELN ( ASMOUT , '  C,''' , CH1 : 1 , '''' ) ;
                    Q := ORD ( CH1 ) ;
                    OPNDTYPE := TYPCDE [ 'C' ] ;
                  end (* then *)
                else
                  if INPUT -> = 'I' then
                    begin
                      READLN ( CH , CH , Q ) ;
                      if ASM then
                        WRITELN ( ASMOUT , '  I,' , Q : 1 ) ;
                      OPNDTYPE := TYPCDE [ 'I' ] ;
                    end (* then *)
                  else
                    if INPUT -> = 'B' then
                      begin
                        READLN ( CH , CH , Q ) ;
                        if ASM then
                          WRITELN ( ASMOUT , '  I,' , Q : 1 ) ;
                        OPNDTYPE := TYPCDE [ 'I' ] ;
                      end (* then *)
                    else
                      begin
                        READLN ( Q ) ;
                        if ASM then
                          WRITELN ( ASMOUT , ' ' : 2 , Q : 1 ) ;
                        OPNDTYPE := TYPCDE [ 'I' ] ;
                      end (* else *)
              end (* tag/ca *) ;
       PCTI , PIXA , PASE , PMOV , PMFI , PMZE , PMSE , PDBG :
         begin

     (*******************)
     (* INTEGER OPERAND *)
     (*******************)

           READLN ( Q ) ;
           if ASM then
             WRITELN ( ASMOUT , ' ' : 2 , Q : 1 ) ;
         end (* tag/ca *) ;
       PLOC : begin

     (*******************)
     (* INTEGER OPERAND *)
     (*******************)

                READLN ( Q ) ;
                if ASM then
                  WRITELN ( ASMOUT , '-------------------- LOC  ' , Q :
                            1 , ' --------------------------------' ) ;
                LINECNT := Q
              end (* tag/ca *) ;
       PAND , PIOR , PXOR , PNOT :
         begin

     (******************************)
     (* TYPE-CODE; if blank then b *)
     (******************************)

           GET ( INPUT ) ;
           CH1 := INPUT -> ;
           if CH1 = ' ' then
             CH1 := 'B' ;
           OPNDTYPE := TYPCDE [ CH1 ] ;
           READLN ( INPUT ) ;
           if ASM then
             WRITELN ( ASMOUT , CH1 : 3 ) ;
         end (* tag/ca *) ;
       PINC , PDEC , PIND :
         begin

     (**********************************)
     (* TYPE-CODE AND INTEGER OPERANDS *)
     (**********************************)

           SKIPBLANKS ;
           OPNDTYPE := TYPCDE [ INPUT -> ] ;
           READLN ( CH1 , CH , Q ) ;
           if ASM then
             WRITELN ( ASMOUT , CH1 : 3 , ',' , Q : 1 ) ;
         end (* tag/ca *) ;
       PNEW , PLDA , PSMV , PSLD , PSCL , PMST :
         begin

     (************************)
     (* TWO INTEGER OPERANDS *)
     (************************)

           READLN ( P , CH , Q ) ;
           if ASM then
             WRITELN ( ASMOUT , ' ' : 2 , P : 1 , ',' , Q : 1 ) ;
         end (* tag/ca *) ;
       PLOD , PSTR :
         begin

     (**************************************)
     (* TYPE-CODE AND TWO INTEGER OPERANDS *)
     (**************************************)

           SKIPBLANKS ;
           OPNDTYPE := TYPCDE [ INPUT -> ] ;
           READLN ( CH1 , CH , P , CH , Q ) ;
           if ASM then
             WRITELN ( ASMOUT , CH1 : 3 , ',' , P : 1 , ',' , Q : 1 ) ;
         end (* tag/ca *) ;
       PPAK : begin

     (**************************)
     (* THREE INTEGER OPERANDS *)
     (**************************)

                READLN ( IVAL , CH , P , CH , Q ) ;
                if ASM then
                  WRITELN ( ASMOUT , ' ' : 2 , IVAL : 1 , ',' , P : 1 ,
                            ',' , Q : 1 ) ;
              end (* tag/ca *) ;
       PCHK : begin

     (**************************************)
     (* TYPE-CODE AND TWO INTEGER OPERANDS *)
     (**************************************)

                SKIPBLANKS ;
                OPNDTYPE := TYPCDE [ INPUT -> ] ;
                READLN ( CH1 , CH , P , CH , Q ) ;
                if ASM then
                  WRITELN ( ASMOUT , CH1 : 3 , ',' , P : 1 , ',' , Q :
                            1 ) ;
              end (* tag/ca *) ;
       PEQU , PNEQ , PLES , PGRT , PLEQ , PGEQ , PSTO :
         begin

     (*********************************************)
     (* TYPE-CODE AND POSSIBLY AN INTEGER OPERAND *)
     (*********************************************)

           SKIPBLANKS ;
           OPNDTYPE := TYPCDE [ INPUT -> ] ;
           if OPNDTYPE = CARR then
             begin
               READLN ( CH1 , CH , Q ) ;
               if ASM then
                 WRITELN ( ASMOUT , CH1 : 3 , ',' , Q : 1 ) ;
             end (* then *)
           else
             begin
               READLN ( CH1 ) ;
               if ASM then
                 WRITELN ( ASMOUT , CH1 : 3 ) ;
             end (* else *) ;
         end (* tag/ca *) ;
       PRET : begin
                if MODUS = 1 then
                  begin
                    GS . MOD1DEFSTEP := 0 ;
                    READLN ( INPUT ) ;
                    return
                  end (* then *) ;

     (*********************************************)
     (* TYPE-CODE AND POSSIBLY AN INTEGER OPERAND *)
     (*********************************************)

                SKIPBLANKS ;
                OPNDTYPE := TYPCDE [ INPUT -> ] ;
                if OPNDTYPE = CARR then
                  begin
                    READLN ( CH1 , CH , Q ) ;
                    if ASM then
                      WRITELN ( ASMOUT , CH1 : 3 , ',' , Q : 1 ) ;
                  end (* then *)
                else
                  begin
                    READLN ( CH1 ) ;
                    if ASM then
                      WRITELN ( ASMOUT , CH1 : 3 ) ;
                  end (* else *) ;
              end (* tag/ca *) ;
       PFJP , PUJP , PCTS , PUXJ :
         begin

     (**********************)
     (* LABEL-NAME OPERAND *)
     (**********************)

           READLBL ( LBL2 ) ;
           READLN ( INPUT ) ;
           if ASM then
             WRITELN ( ASMOUT , ' ' : 2 , LBL2 . NAM : LBL2 . LEN ) ;
         end (* tag/ca *) ;
       PXJP : begin

     (**********************)
     (* LABEL-NAME OPERAND *)
     (**********************)

                SKIPBLANKS ;
                READLN ( BUF20 ) ;
                if ( BUF20 [ 1 ] in [ 'N' , 'O' ] ) and ( BUF20 [ 2 ] =
                ',' ) then
                  begin
                    XJPFLAG := BUF20 [ 1 ] ;
                    LSTART := 2
                  end (* then *)
                else
                  begin
                    LSTART := 0 ;
                    XJPFLAG := ' ' ;
                  end (* else *) ;
                for X1 := 1 to 8 do
                  begin
                    LBL2 . NAM [ X1 ] := BUF20 [ X1 + LSTART ] ;
                  end (* for *) ;
                LBL2 . LEN := 8 ;
                while LBL2 . NAM [ LBL2 . LEN ] = ' ' do
                  begin
                    LBL2 . LEN := LBL2 . LEN - 1 ;
                    if LBL2 . LEN = 0 then
                      break ;
                  end (* while *) ;
                if ASM then
                  begin
                    if XJPFLAG = ' ' then
                      WRITELN ( ASMOUT , ' ' , LBL2 . NAM : LBL2 . LEN
                                )
                    else
                      WRITELN ( ASMOUT , ' ' , XJPFLAG , ',' , LBL2 .
                                NAM : LBL2 . LEN ) ;
                  end (* then *)
              end (* tag/ca *) ;
       PCST : begin

     (************************************)
     (* PROCEDURE NAME & NUMBER OPERANDS *)
     (************************************)

                READLN ( CH1 , CST_CURPNAME , CST_CURPNO , CH , ASM ,
                         CH , CST_GET_STAT , CH , CST_ASMVERB ) ;
                if ASM then
                  begin
                    if FIRST_ASMOUT then
                      begin
                        REWRITE ( ASMOUT ) ;
                        FIRST_ASMOUT := FALSE
                      end (* then *) ;
                    WRITE ( ASMOUT , '     0000:  ' , LBL1 . NAM : LBL1
                            . LEN , ' ' : 6 - LBL1 . LEN , NMCDE : 4 )
                            ;
                    WRITELN ( ASMOUT , CST_CURPNAME : IDLNGTH + 2 ,
                              CST_CURPNO : 4 , ',' , ASM : 1 , ',' ,
                              CST_GET_STAT : 1 , ',' , CST_ASMVERB : 1
                              ) ;
                  end (* then *) ;
              end (* tag/ca *) ;
       PCUP : begin

     (*****************************************************)
     (* TYPE-CODE,LEXIC-LEVEL,LABEL-NAME,INTEGER OPERANDS *)
     (*****************************************************)

                SKIPBLANKS ;
                OPNDTYPE := TYPCDE [ INPUT -> ] ;
                READ ( CH1 ) ;
                EXTLANG := ' ' ;
                if INPUT -> <> ',' then
                  begin
                    EXTLANG := INPUT -> ;
                    READ ( CH ) ;
                  end (* then *) ;
                READ ( CH , P , CH ) ;
                READLBL ( LBL2 ) ;
                if INPUT -> = ' ' then
                  SKIPBLANKS ;
                READLN ( CH , Q ) ;
                if ASM then
                  begin
                    WRITE ( ASMOUT , CH1 : 3 ) ;
                    if EXTLANG <> ' ' then
                      WRITE ( ASMOUT , EXTLANG ) ;
                    WRITE ( ASMOUT , ',' , P : 1 ) ;
                    WRITE ( ASMOUT , ',' , LBL2 . NAM : LBL2 . LEN ) ;
                    WRITE ( ASMOUT , ',' , Q : 1 ) ;
                    WRITELN ( ASMOUT ) ;
                  end (* then *)
              end (* tag/ca *) ;
       PBGN : begin

     (******************)
     (* STRING OPERAND *)
     (******************)

                READLN ( CH , PROGHDR ) ;
                if ASM then
                  WRITELN ( ASMOUT , ' ' , PROGHDR ) ;
              end (* tag/ca *) ;
       PENT : begin

     (*************************************************************)
     (* TYPE-CODE,LEXIC-LEVEL,LABEL,THREE FLAGS,INTEGER OPERANDS  *)
     (*************************************************************)

                if MODUS = 1 then
                  begin
                    if PIANKER = NIL then
                      begin
                        NEW ( PIANKER ) ;
                        PIAKT := PIANKER
                      end (* then *)
                    else
                      begin
                        NEW ( PIAKT -> . NEXT ) ;
                        PIAKT := PIAKT -> . NEXT
                      end (* else *) ;
                    PIAKT -> . NEXT := NIL ;
                    SKIPBLANKS ;
                    PIAKT -> . OPNDTYPE := TYPCDE [ INPUT -> ] ;
                    READ ( CH1 , CH , P , CH ) ;
                    READLBL ( PIAKT -> . SEGSZE ) ;
                    if INPUT -> = ' ' then
                      SKIPBLANKS ;
                    READ ( PIAKT -> . CURPNAME , CH ) ;
                    READ ( PIAKT -> . SAVERGS , CH ) ;
                    READ ( PIAKT -> . ASM , CH ) ;
                    READ ( PIAKT -> . GET_STAT , CH ) ;
                    READ ( PIAKT -> . ASMVERB , CH ) ;
                    READ ( PIAKT -> . DEBUG_LEV , CH ) ;
                    READ ( PIAKT -> . CURPNO , CH ) ;
                    PIAKT -> . STATNAME := ' ' ;
                    PIAKT -> . SOURCENAME := ' ' ;
                    if INPUT -> <> ',' then
                      READ ( PIAKT -> . STATNAME , CH )
                    else
                      READ ( CH ) ;
                    READ ( PIAKT -> . SOURCENAME ) ;
                    READLN ( INPUT ) ;
                    DEBUG := PIAKT -> . DEBUG_LEV >= 2 ;
                    PIAKT -> . FLOW_TRACE := PIAKT -> . DEBUG_LEV >= 3
                                             ;
                    return ;
                  end (* then *)
                else
                  begin
                    SKIPBLANKS ;
                    READ ( CH1 , CH , P , CH ) ;
                    READLBL ( DUMMYLABEL ) ;
                    if INPUT -> = ' ' then
                      SKIPBLANKS ;
                    READ ( DUMMYNAME , CH , DUMMYBOOL , CH , DUMMYBOOL
                           , CH , DUMMYBOOL , CH , DUMMYBOOL , CH ,
                           DUMMYINT , CH , MATCH_CURPNO , CH ) ;
                    READLN ( INPUT ) ;
                    PIAKT := PIANKER ;
                    while PIAKT <> NIL do
                      begin
                        if PIAKT -> . CURPNO <> MATCH_CURPNO then
                          PIAKT := PIAKT -> . NEXT
                        else
                          break ;
                      end (* while *) ;
                    OPNDTYPE := PIAKT -> . OPNDTYPE ;
                    ASM := PIAKT -> . ASM ;
                    FLOW_TRACE := PIAKT -> . FLOW_TRACE ;
                    if ASM then
                      begin
                        WRITELN ( ASMOUT ) ;
                        HEXHW ( 2 * PCOUNTER , HLOC ) ;
                        WRITE ( ASMOUT , HLOC : 9 , ':  ' ) ;
                        WRITE ( ASMOUT , LBL1 . NAM : LBL1 . LEN ) ;
                        WRITE ( ASMOUT , ' ' : 6 - LBL1 . LEN ) ;
                        WRITE ( ASMOUT , NMCDE : 4 ) ;
                        WRITE ( ASMOUT , CH1 : 3 , ',' ) ;
                        WRITE ( ASMOUT , P : 1 , ',' ) ;
                        WRITE ( ASMOUT , PIAKT -> . SEGSZE . NAM : 4 )
                                ;
                        WRITELN ( ASMOUT , PIAKT -> . CURPNAME :
                                  IDLNGTH + 2 , ',' ) ;
                        WRITE ( ASMOUT , HLOC : 9 , ':  ' ) ;
                        WRITE ( ASMOUT , ' ' : 14 ) ;
                        WRITE ( ASMOUT , PIAKT -> . SAVERGS : 1 , ',' )
                                ;
                        WRITE ( ASMOUT , PIAKT -> . ASM : 1 , ',' ) ;
                        WRITE ( ASMOUT , PIAKT -> . GET_STAT : 1 , ','
                                ) ;
                        WRITE ( ASMOUT , PIAKT -> . ASMVERB : 1 , ',' )
                                ;
                        WRITE ( ASMOUT , PIAKT -> . DEBUG_LEV : 1 , ','
                                ) ;
                        WRITE ( ASMOUT , PIAKT -> . CURPNO : 1 , ',' )
                                ;
                        if PIAKT -> . STATNAME <> '        ' then
                          WRITE ( ASMOUT , PIAKT -> . STATNAME ) ;
                        WRITE ( ASMOUT , ',' ) ;
                        if PIAKT -> . SOURCENAME <> '        ' then
                          WRITE ( ASMOUT , PIAKT -> . SOURCENAME ) ;
                        WRITELN ( ASMOUT ) ;
                      end (* then *) ;
                  end (* else *)
              end (* tag/ca *) ;
       PLDC , PLCA , PDFC :
         READLOADINSTRUCTIONS ;
       PCSP : begin

     (*************************************)
     (* SUBMONITOR OPERATION NAME OPERAND *)
     (*************************************)

                SKIPBLANKS ;
                READ ( NMCDE ) ;
                if FALSE then
                  WRITE ( TRACEF , 'read = ' , NMCDE ) ;
                if INPUT -> = ',' then
                  begin
                    READLN ( CH , PROCOFFSET ) ;
                  end (* then *)
                else
                  begin
                    READLN ( INPUT ) ;
                    PROCOFFSET := 0 ;
                  end (* else *) ;
                OP_SP := FALSE ;
                ENTERLOOKUP ;
                OP_SP := TRUE ;
                if ASM then
                  WRITELN ( ASMOUT , NMCDE : 5 , ',' , PROCOFFSET : 1 )
                            ;
                if FALSE then
                  WRITELN ( TRACEF , '  csp  = ' , ORD ( CSP ) ) ;
              end (* tag/ca *) ;

     //************************************************************
     // vstring instructions                                       
     //************************************************************

       PVPU , PVPO :
         begin

     //************************************************************
     // TWO INTEGER OPERANDS                                       
     //************************************************************

           READLN ( P , CH , Q ) ;
           if ASM then
             WRITELN ( ASMOUT , ' ' : 2 , P : 1 , ',' , Q : 1 ) ;
         end (* tag/ca *) ;
       PVLD , PVST :
         begin

     //************************************************************
     // TWO INTEGER OPERANDS                                       
     // the first one is a mode indicator                          
     //************************************************************

           READLN ( P , CH , Q ) ;
           if ASM then
             WRITELN ( ASMOUT , ' ' : 2 , P : 1 , ',' , Q : 1 ) ;
         end (* tag/ca *) ;
       PVC1 , PVCC , PVLM , PVIX , PVRP :
         begin

     //************************************************************
     // no operands                                                
     //************************************************************

           READLN ( INPUT ) ;
           if ASM then
             WRITELN ( ASMOUT ) ;
         end (* tag/ca *) ;
       PVC2 , PVMV , PVSM :
         begin

     //************************************************************
     // one integer operand                                        
     //************************************************************

           READLN ( Q ) ;
           if ASM then
             WRITELN ( ASMOUT , ' ' : 2 , Q : 1 ) ;
         end (* tag/ca *) ;
       otherwise
         begin

     (*****************************)
     (* OPCODE NOT FOUND IN TABLE *)
     (*****************************)

           if not ASM then
             WRITE ( OUTPUT , LBL1 . NAM : LBL1 . LEN , ' ' : 6 - LBL1
                     . LEN , ' "' , NMCDE , '" ' ) ;
           while not EOLN do
             begin
               WRITE ( OUTPUT , INPUT -> ) ;
               GET ( INPUT )
             end (* while *) ;
           WRITELN ( OUTPUT ) ;
           READLN ( INPUT ) ;
           ERROR ( 606 ) ;
         end (* otherw *) ;
     end (* case *)
   end (* READNXTINST *) ;



procedure TXT2LBL ( var LBL : PLABEL ; X : ALFA ) ;

   begin (* TXT2LBL *)
     LBL . NAM := X ;
     LBL . LEN := 8 ;
     while LBL . NAM [ LBL . LEN ] = ' ' do
       begin
         LBL . LEN := LBL . LEN - 1 ;
         if LBL . LEN = 0 then
           break ;
       end (* while *)
   end (* TXT2LBL *) ;



procedure ASMNXTINST ;

(*********************************************************************)
(* TO TRANSLATE THE NEXT P_INSTRUCTION INTO 370 ASSEMBLY/OBJECT CODE *)
(* ----------------------------------------------------------------- *)
(*********************************************************************)


   const SL8 = 256 ;          // SHIFT LEFT  8 BITS
         SL12 = 4096 ;        //            12
         SL16 = 65536 ;       //            16
         SL24 = 16777216 ;    //            24

   var OP : BYTE ;
       P1 , P2 , B1 , B2 : LVLRNG ;
       Q1 , Q2 : ADRRNG ;
       I , J : INTEGER ;
       LEFTDEC , NEGATE : BOOLEAN ;
       POWER10 : REAL ;
       OPPTR : STKPTR ;
       RGADR1 : RGRNG ;
       RGADR2 : RGRNG ;
       LBLX : PLABEL ;
       C : CHAR ;
       LITOK : INTEGER ;
       TAG : array [ 1 .. 3 ] of CHAR ;
       NXTINT : 0 .. INTCNT ;
       XOFFS : INTEGER ;

       (***************************************************)
       (* THE FOLLOWING PROCEDURES ARE FOR OBJECT CODE    *)
       (* GENERATION ONLY                                 *)
       (* ----------------------------------------------- *)
       (***************************************************)



   function NEXTPC ( PCINCR : ICRNG ) : ICRNG ;

      begin (* NEXTPC *)
        if FALSE then
          WRITELN ( TRACEF , 'nextpc: pcounter = ' , PCOUNTER ,
                    ' pcincr = ' , PCINCR ) ;
        if PCOUNTER >= MXCODE then
          begin
            ERROR ( 253 ) ;
            EXIT ( 253 )
          end (* then *) ;
        NEXTPC := PCOUNTER + PCINCR ;
      end (* NEXTPC *) ;


   function BASE_DSPLMT ( PCOUNTER : ICRNG ) : INTEGER ;

   (*****************************************************)
   (* CONVERTS PROGRAM COUNTER VALUES TO 370            *)
   (* BASE/DISPLACEMENT HALF WORDS                      *)
   (* ------------------------------------------------- *)
   (*****************************************************)


      var PC : INTEGER ;

      begin (* BASE_DSPLMT *)
        PC := 2 * PCOUNTER ;
        if PC < 4096 then
          begin
            BASE_DSPLMT := PBR1 * SL12 + PC ;
            return
          end (* then *) ;
        if PC <= 8188 then
          begin
            BASE_DSPLMT := PBR2 * SL12 + PC - 4092 ;
            return
          end (* then *) ;
        ERROR ( 254 )
      end (* BASE_DSPLMT *) ;


   procedure UPD_DBLTBL ( PCOUNTER : ICRNG ; R : REAL ) ;

      var I : INTEGER ;
          S_R : record
                  case INTEGER of
                    1 :
                      ( R : REAL ) ;
                    2 :
                      ( S : SHORT_SET ) ;
                end ;

      begin (* UPD_DBLTBL *)
        DBLALN := TRUE ;

        //******************************************************
        // INDICATE ALIGNMENT FOR LITERAL POOL                  
        //******************************************************

        IDP_POOL . R [ LX . NXTDBL ] := R ;
        I := 0 ;

        //******************************************************
        // look for matching entry in idp_pool                  
        //******************************************************

        S_R . R := R ;
        while IDP_POOL . S [ I ] <> S_R . S do
          I := I + 1 ;

        //******************************************************
        // if the matching entry contains the integer gaps      
        // don't use the integer gaps !                         
        //******************************************************

        if I = LX . RICONF then
          begin
            LX . RICONF := - 1 ;
            LX . INT_GAP := - 1
          end (* then *) ;

        //******************************************************
        // if the matching entry contains the halfword gaps     
        // don't use the halfword gaps !                        
        //******************************************************

        if I = LX . RHCONF then
          begin
            LX . RHCONF := - 1 ;
            LX . HW_GAP := - 1
          end (* then *) ;
        NXTLIT := NXTLIT + 1 ;
        LITTBL [ NXTLIT ] . LNK := PCOUNTER ;

        //******************************************************
        // if index found = nxtdbl, then increase nxtdbl        
        //******************************************************

        if I = LX . NXTDBL then
          LX . NXTDBL := LX . NXTDBL + 1 ;
        I := I * 8 ;

        //******************************************************
        // set nxtch to nxtdbl + 8                              
        //******************************************************

        if I >= LX . NXTCH then
          LX . NXTCH := I + 8 ;

        //******************************************************
        // insert pointer to literal pool into code             
        //******************************************************

        CODE . H [ PCOUNTER ] := TO_HINT ( I ) ;
        LITTBL [ NXTLIT ] . LTYPE := 'D' ;
        LITTBL [ NXTLIT ] . LENGTH := 8 ;
        LITTBL [ NXTLIT ] . XIDP := I ;
      end (* UPD_DBLTBL *) ;


   procedure UPD_HWTBL ( PCOUNTER : ICRNG ; H : HINTEGER ) ;

      var I , NXTHW : 0 .. HWCNT ;

      begin (* UPD_HWTBL *)
        if LX . HW_GAP >= 0 then

        //******************************************************
        // if there is a halfword gap                           
        // PREVENT MATCH WITH EMPTY SLOT                        
        //******************************************************

          if H = 0 then
            IDP_POOL . H [ LX . HW_GAP ] := - 1
          else
            IDP_POOL . H [ LX . HW_GAP ] := 0 ;
        if LX . INT_GAP >= 0 then

        //******************************************************
        // if there is an integer gap                           
        // PREVENT MATCH WITH EMPTY SLOT                        
        //******************************************************

          if H = 0 then
            IDP_POOL . I [ LX . INT_GAP ] := - 1
          else
            IDP_POOL . I [ LX . INT_GAP ] := 0 ;

        //******************************************************
        // look for matching entry in idp_pool                  
        //******************************************************

        NXTHW := LX . NXTDBL * 4 ;
        IDP_POOL . H [ NXTHW ] := H ;
        I := 0 ;
        while IDP_POOL . H [ I ] <> H do
          I := I + 1 ;
        if I = NXTHW then
          if LX . HW_GAP >= 0 then
            begin

        //******************************************************
        // NOW USE EMPTY SLOT                                   
        //******************************************************

              I := LX . HW_GAP ;
              IDP_POOL . H [ I ] := H ;
              LX . HW_GAP := - 1 ;
              LX . IHCONF := - 1 ;
              LX . RHCONF := - 1
            end (* then *)
          else
            if LX . INT_GAP >= 0 then
              begin

        //******************************************************
        // SPLIT EMPTY INTEGER SLOT                             
        //******************************************************

                LX . HW_GAP := 2 * LX . INT_GAP + 1 ;
                I := LX . HW_GAP - 1 ;
                IDP_POOL . H [ I ] := H ;
                LX . IHCONF := LX . INT_GAP ;
                LX . RHCONF := LX . IHCONF DIV 2 ;
                LX . RICONF := - 1 ;
                IDP_POOL . H [ LX . HW_GAP ] := 0 ;
                LX . INT_GAP := - 1
              end (* then *)
            else
              begin

        //******************************************************
        // use new double entry,                                
        // generate two gaps                                    
        //******************************************************

                LX . HW_GAP := NXTHW + 1 ;
                LX . INT_GAP := LX . NXTDBL * 2 + 1 ;
                LX . RICONF := LX . NXTDBL ;
                LX . RHCONF := LX . NXTDBL ;
                LX . IHCONF := LX . INT_GAP - 1 ;
                LX . NXTDBL := LX . NXTDBL + 1 ;
                IDP_POOL . I [ LX . INT_GAP ] := 0 ;
                IDP_POOL . H [ LX . HW_GAP ] := 0 ;
              end (* else *) ;
        I := I * 2 ;
        CODE . H [ PCOUNTER ] := TO_HINT ( I ) ;
        if I >= LX . NXTCH then
          LX . NXTCH := I + 2 ;
        NXTLIT := NXTLIT + 1 ;
        LITTBL [ NXTLIT ] . LNK := PCOUNTER ;
        if FALSE then
          begin
            WRITELN ( TRACEF , 'upd_hwtbl: nxtlit = ' , NXTLIT ) ;
            WRITELN ( TRACEF , 'upd_hwtbl: pcounter = ' , PCOUNTER ) ;
          end (* then *) ;
        LITTBL [ NXTLIT ] . LTYPE := 'H' ;
        LITTBL [ NXTLIT ] . LENGTH := 2 ;
        LITTBL [ NXTLIT ] . XIDP := I ;
      end (* UPD_HWTBL *) ;


   procedure UPD_INTTBL ( PCOUNTER : ICRNG ; D : INTEGER ) ;

      var I , NXTINT : 0 .. INTCNT ;

      begin (* UPD_INTTBL *)
        if LX . INT_GAP >= 0 then

        //******************************************************
        // if there is an integer gap                           
        // PREVENT MATCH WITH EMPTY SLOT                        
        //******************************************************

          if D = 0 then
            IDP_POOL . I [ LX . INT_GAP ] := - 1
          else
            IDP_POOL . I [ LX . INT_GAP ] := 0 ;
        NXTINT := LX . NXTDBL * 2 ;
        IDP_POOL . I [ NXTINT ] := D ;
        I := 0 ;
        while IDP_POOL . I [ I ] <> D do
          I := I + 1 ;

        //******************************************************
        // if the matching entry contains the halfword gaps     
        // don't use the halfword gaps !                        
        //******************************************************

        if I = LX . IHCONF then
          begin
            LX . HW_GAP := - 1 ;
            LX . IHCONF := - 1 ;
            LX . RHCONF := - 1
          end (* then *) ;
        if I = NXTINT then
          if LX . INT_GAP >= 0 then
            begin

        //******************************************************
        // NOW USE EMPTY SLOT                                   
        //******************************************************

              I := LX . INT_GAP ;
              LX . INT_GAP := - 1 ;
              LX . RICONF := - 1 ;
              IDP_POOL . I [ I ] := D ;
            end (* then *)
          else
            begin

        //******************************************************
        // use new double entry,                                
        // generate integer gap                                 
        //******************************************************

              LX . INT_GAP := NXTINT + 1 ;
              LX . RICONF := LX . INT_GAP DIV 2 ;
              LX . NXTDBL := LX . NXTDBL + 1 ;
              IDP_POOL . I [ LX . INT_GAP ] := 0 ;
            end (* else *) ;
        I := I * 4 ;
        CODE . H [ PCOUNTER ] := TO_HINT ( I ) ;
        if I >= LX . NXTCH then
          LX . NXTCH := I + 4 ;
        NXTLIT := NXTLIT + 1 ;
        LITTBL [ NXTLIT ] . LNK := PCOUNTER ;
        if FALSE then
          begin
            WRITELN ( TRACEF , 'upd_inttbl: nxtlit = ' , NXTLIT ) ;
            WRITELN ( TRACEF , 'upd_inttbl: pcounter = ' , PCOUNTER ) ;
            WRITELN ( TRACEF , 'upd_inttbl: d = ' , D ) ;
          end (* then *) ;
        LITTBL [ NXTLIT ] . LTYPE := 'I' ;
        LITTBL [ NXTLIT ] . LENGTH := 4 ;
        LITTBL [ NXTLIT ] . XIDP := I ;
      end (* UPD_INTTBL *) ;


   procedure UPD_SETTBL ( PCOUNTER : ICRNG ; PS : LARGE_SET ; L :
                        INTEGER ) ;

      type SET_S_I = record
                       case INTEGER of
                         1 :
                           ( S : LARGE_SET ) ;
                         2 :
                           ( I : array [ 1 .. MXSETIXI ] of INTEGER ) ;
                         3 :
                           ( C : array [ 1 .. MXSETLEN ] of CHAR ) ;
                         4 :
                           ( R : array [ 1 .. MXSETINX ] of REAL )
                     end ;

      var S_I : SET_S_I ;
          I , J , LD4 : INTEGER ;

      begin (* UPD_SETTBL *)
        S_I . S := PS ;

        /*************************/
        /* show error when l = 0 */
        /*************************/

        if L = 0 then
          begin
            ERROR ( 616 ) ;
            return
          end (* then *) ;

        /********************************************/
        /* set literal of length 4 - use upd_inttbl */
        /********************************************/

        if L <= 4 then
          begin
            UPD_INTTBL ( PCOUNTER , S_I . I [ 1 ] ) ;
            return
          end (* then *) ;

        /********************************************/
        /* set literal of length 8 - use upd_dbltbl */
        /********************************************/

        if L <= 8 then
          begin
            UPD_DBLTBL ( PCOUNTER , S_I . R [ 1 ] ) ;
            return ;
          end (* then *) ;

        /******************/
        /* longer literal */
        /******************/

        while ( L MOD INTSIZE ) <> 0 do
          L := L + 1 ;
        LD4 := L DIV 4 ;
        I := 2 * LX . NXTDBL ;

        //******************************************************
        // if int_gap preceeding free area, use int_gap, too    
        //******************************************************

        if LX . INT_GAP >= 0 then
          if LX . INT_GAP = I - 1 then
            begin
              I := I - 1 ;
              LX . INT_GAP := - 1 ;
              LX . RICONF := - 1
            end (* then *) ;

        //******************************************************
        // set literal starts at this position (I * 4)          
        // integer bound                                        
        //******************************************************

        CODE . H [ PCOUNTER ] := TO_HINT ( I * 4 ) ;
        NXTLIT := NXTLIT + 1 ;
        LITTBL [ NXTLIT ] . LNK := PCOUNTER ;
        LITTBL [ NXTLIT ] . LTYPE := 'S' ;
        LITTBL [ NXTLIT ] . LENGTH := L ;
        LITTBL [ NXTLIT ] . XIDP := I * 4 ;
        if FALSE then
          begin
            WRITELN ( TRACEF , 'upd_settbl: nxtlit = ' , NXTLIT ) ;
            WRITELN ( TRACEF , 'upd_settbl: pcounter = ' , PCOUNTER ) ;
          end (* then *) ;

        //******************************************************
        // copy set literal to literal pool                     
        //******************************************************

        for J := 1 to LD4 do
          begin
            IDP_POOL . I [ I ] := S_I . I [ J ] ;
            I := I + 1 ;
          end (* for *) ;

        //******************************************************
        // adjust nxtch and nxtdbl                              
        // and set new integer gap, if needed                   
        //******************************************************

        if I * 4 > LX . NXTCH then
          LX . NXTCH := I * 4 ;
        if I > LX . NXTDBL * 2 then
          begin
            LX . NXTDBL := I DIV 2 ;
            if ODD ( I ) then
              begin
                LX . RICONF := LX . NXTDBL ;
                LX . NXTDBL := LX . NXTDBL + 1 ;
                LX . INT_GAP := I ;
                IDP_POOL . I [ I ] := 0 ;
              end (* then *) ;
          end (* then *)
      end (* UPD_SETTBL *) ;


   procedure INS_PRCTBL ( PRC_NAME : ALFA ; VPOS : ICRNG ) ;

   (*******************************************************)
   (* Insert External Reference                           *)
   (* -------------------------                           *)
   (* an der vorgegebenen Position (VPos) befindet sich   *)
   (* eine externe Referenz mit dem angegeben Namen       *)
   (* (V-Adresse); diese ist bislang noch nicht vor-      *)
   (* handen und soll von nachfolgenden L-Befehlen        *)
   (* wie ein Literal verwendet werden.                   *)
   (*******************************************************)


      begin (* INS_PRCTBL *)
        PRCTBL [ NXTPRC ] . NAME := PRC_NAME ;
        PRCTBL [ NXTPRC ] . LNK := 0 ;
        PRCTBL [ NXTPRC ] . VPOS := VPOS ;
        if NXTPRC >= NXTEP then
          ERROR ( 256 )
        else
          begin
            NXTPRC := NXTPRC + 1 ;
            PRCTBL [ NXTPRC ] . LNK := 0 ;
            PRCTBL [ NXTPRC ] . VPOS := 0 ;
          end (* else *)
      end (* INS_PRCTBL *) ;


   procedure UPD_PRCTBL ( PCOUNTER : ICRNG ; PRC_NAME : ALFA ) ;

   (*******************************************************)
   (* TO UPDATE EXTERNAL REFERENCE TABLE                  *)
   (* ----------------------------------                  *)
   (* PRC-Name = Name der (neuen) externen Referenz;      *)
   (* dieser Name wird zunaechst an der Position          *)
   (* NXTPRC in die Tabelle PRCTBL eingetragen.           *)
   (* Dann Suche, ob es evtl. in der Tabelle schon        *)
   (* vorhanden ist. Falls ja, CODE.H an der Position     *)
   (* PCOUNTER verlinken mit dem entsprechenden           *)
   (* Eintrag (beide Richtungen). Wenn I = NXTPRC,        *)
   (* dann war es der neu eingefuegte hoechste Eintrag,   *)
   (* dann Pruefung auf Einhaltung der Grenzen,           *)
   (* ansonsten naechsten Eintrag vorbereiten.            *)
   (* --------------------------------------------------- *)
   (* Nachtrag: die Positionen im Code, wo dieselben      *)
   (* externen Namen benutzt werden, sind miteinander     *)
   (* verkettet ueber den LNK-Pointer; damit koennen      *)
   (* nach Festlegung der Adresse alle Offsets            *)
   (* angeglichen werden.                                 *)
   (*******************************************************)


      var I : 0 .. PRCCNT ;

      begin (* UPD_PRCTBL *)
        PRCTBL [ NXTPRC ] . NAME := PRC_NAME ;
        PRCTBL [ NXTPRC ] . VPOS := 0 ;
        I := 0 ;
        while PRCTBL [ I ] . NAME <> PRC_NAME do
          I := I + 1 ;
        CODE . H [ PCOUNTER ] := TO_HINT ( PRCTBL [ I ] . LNK ) ;
        PRCTBL [ I ] . LNK := PCOUNTER ;
        if I = NXTPRC then
          if NXTPRC >= NXTEP then
            ERROR ( 256 )
          else
            begin
              NXTPRC := NXTPRC + 1 ;
              PRCTBL [ NXTPRC ] . LNK := 0 ;
              PRCTBL [ NXTPRC ] . VPOS := 0 ;
            end (* else *)
      end (* UPD_PRCTBL *) ;


   function LBLMAP ( ALFLBL : ALFA ) : LBLRNG ;

      var I : 2 .. 8 ;
          J : LBLRNG ;

      begin (* LBLMAP *)
        I := 2 ;
        J := 0 ;
        repeat
          J := J * 10 + ORD ( ALFLBL [ I ] ) - ORD ( '0' ) ;
          I := I + 1
        until ALFLBL [ I ] = ' ' ;
        LBLMAP := J ;
      end (* LBLMAP *) ;


   procedure UPD_LBLTBL ( PCOUNTER : ICRNG ; INTLBL : LBLRNG ; NEWLBL :
                        BOOLEAN ; CASE_FLOW : BOOLEAN ) ;

   (********************************************************)
   (* TO 'DEFINE' LABELS AND/OR RESOLVE FORWARD REFERENCES *)
   (* ---------------------------------------------------- *)
   (********************************************************)


      var I : LBLRNG ;
          TPC , QPC : INTEGER ;

      begin (* UPD_LBLTBL *)
        if FALSE then
          begin
            WRITELN ( TRACEF , 'upd_lbltbl: pcounter  = ' , PCOUNTER )
                      ;
            WRITELN ( TRACEF , 'upd_lbltbl: intlbl    = ' , INTLBL ) ;
            WRITELN ( TRACEF , 'upd_lbltbl: newlbl    = ' , NEWLBL ) ;
            WRITELN ( TRACEF , 'upd_lbltbl: case_flow = ' , CASE_FLOW )
                      ;
          end (* then *) ;
        if INTLBL > LBLCNT then
          begin
            WRITELN ( ' **** INTLBL ' : 17 , INTLBL ) ;
            ERROR ( 263 ) ;
            EXIT ( 263 )
          end (* then *)
        else
          with LBLTBL [ INTLBL ] do
            begin
              if FALSE then
                begin
                  WRITELN ( TRACEF , 'upd_lbltbl: defined   = ' ,
                            DEFINED ) ;
                end (* then *) ;
              if DEFINED then

        (**********************)
        (* BACKWARD REFERENCE *)
        (**********************)

                if CASE_FLOW then
                  CODE . H [ PCOUNTER ] := TO_HINT ( LNK * 2 )

        (****************)
        (*HALFWORD ADDR.*)
        (****************)

                else
                  CODE . H [ PCOUNTER ] := TO_HINT ( BASE_DSPLMT ( LNK
                                           ) )

        (***************************)
        (* BASE/DSPLMT HALF WORD   *)
        (***************************)

              else
                if NEWLBL then

        (********************)
        (* LABEL DEFINITION *)
        (********************)

                  begin
                    DEFINED := TRUE ;
                    TPC := LNK ;
                    LNK := PCOUNTER ;
                    if FALSE then
                      begin
                        WRITELN ( TRACEF , 'upd_lbltbl: newlbl    = ' ,
                                  NEWLBL ) ;
                        WRITELN ( TRACEF , 'upd_lbltbl: lnk       = ' ,
                                  LNK ) ;
                      end (* then *) ;

        (*******************)
        (* SET LABEL VALUE *)
        (*******************)

                    while TPC > 1 do
                      begin
                        QPC := TPC ;
                        TPC := CODE . H [ QPC ] ;
                        if TPC < 0 then
                          begin
                            CODE . H [ QPC ] := TO_HINT ( PCOUNTER * 2
                                                ) ;
                            TPC := ABS ( TPC )
                          end (* then *)
                        else
                          CODE . H [ QPC ] := TO_HINT ( BASE_DSPLMT (
                                              PCOUNTER ) ) ;
                      end (* while *)
                  end (* then *)
                else

        (***********************************************************)
        (* NOT NEWLBL I.E. FORWARD REFERENCE, TO BE RESOLVED LATER *)
        (***********************************************************)

                  begin
                    if CASE_FLOW then
                      CODE . H [ PCOUNTER ] := TO_HINT ( - LNK )
                    else
                      CODE . H [ PCOUNTER ] := TO_HINT ( LNK ) ;
                    LNK := PCOUNTER
                  end (* else *)
            end (* with *)
      end (* UPD_LBLTBL *) ;


   procedure PRINT_SET ( S : LARGE_SET ; LNGTH : BYTE ) ;

      var I , INDNT : INTEGER ;
          DELIM : CHAR ;
          C , C1 , C2 : CHAR ;
          COL : INTEGER ;

      begin (* PRINT_SET *)
        PSVAL := S ;
        DELIM := '''' ;
        WRITE ( ASMOUT , '=XL' , LNGTH : 1 , '''' ) ;
        if FALSE then
          WRITE ( TRACEF , '=XL' , LNGTH : 1 , '''' ) ;
        COL := 0 ;
        for I := 1 to LNGTH do
          begin
            C := PSVAL . C [ I ] ;
            C1 := HEXTAB [ ORD ( C ) DIV 16 ] ;
            C2 := HEXTAB [ ORD ( C ) MOD 16 ] ;
            if COL + 1 > 32 then
              begin
                WRITELN ( ASMOUT , 'X' ) ;
                WRITE ( ASMOUT , ' ' : 21 ) ;
                COL := 1 ;
              end (* then *)
            else
              COL := COL + 1 ;
            WRITE ( ASMOUT , C1 ) ;
            if FALSE then
              WRITE ( TRACEF , C1 ) ;
            COL := COL + 1 ;
            WRITE ( ASMOUT , C2 ) ;
            if FALSE then
              WRITE ( TRACEF , C2 ) ;
          end (* for *) ;
        WRITELN ( ASMOUT , '''' ) ;
        if FALSE then
          WRITELN ( TRACEF , '''' ) ;
      end (* PRINT_SET *) ;


   procedure TRACE_SET ( S : LARGE_SET ; LNGTH : BYTE ) ;

      var I , INDNT : INTEGER ;
          DELIM : CHAR ;
          C , C1 , C2 : CHAR ;
          COL : INTEGER ;

      begin (* TRACE_SET *)
        PSVAL := S ;
        DELIM := '''' ;
        WRITE ( TRACEF , '=XL' , LNGTH : 1 , '''' ) ;
        COL := 0 ;
        for I := 1 to LNGTH do
          begin
            C := PSVAL . C [ I ] ;
            C1 := HEXTAB [ ORD ( C ) DIV 16 ] ;
            C2 := HEXTAB [ ORD ( C ) MOD 16 ] ;
            if COL + 1 > 32 then
              begin
                WRITELN ( TRACEF , 'X' ) ;
                WRITE ( TRACEF , ' ' : 21 ) ;
                COL := 1 ;
              end (* then *)
            else
              COL := COL + 1 ;
            WRITE ( TRACEF , C1 ) ;
            COL := COL + 1 ;
            WRITE ( TRACEF , C2 ) ;
          end (* for *) ;
        WRITELN ( TRACEF , '''' ) ;
      end (* TRACE_SET *) ;

          (****************************************************)
          (* 370 FORMAT CODE GENERATOR (ASSEMBLY/OBJECT CODE) *)
          (* ------------------------------------------------ *)
          (****************************************************)



   procedure GENRR ( OP : BYTE ; R1 , R2 : RGRNG ) ;

      begin (* GENRR *)
        if R1 = TRG14 then
          TXR_CONTENTS . VALID := FALSE ;
        if OPT_FLG then
          if ( OP = XLTR ) or ( OP = XLTDR ) then
            with LAST_CC do
              if PCOUNTER = LPC then

        (*******************************)
        (* NO INTERVENING INSTRUCTIONS *)
        (*******************************)

                if R1 = R2 then
                  if LR = R1 then
                    if OP = XLTDR then
                      if LOP in [ XAD , XSD , XLCDR , XLPDR , XADR ,
                      XSDR , XAD , XSD ] then
                        return
                      else
                        
                    else

        (*************)
        (* OP = XLTR *)
        (*************)

                      if LOP in [ XLPR , XLCR , XNR , XORX , XXR , XAR
                      , XSR , XAH , XSH , XO , XX , XN , XSLA , XSRA ,
                      XA , XS ] then
                        return ;

        (**********************************)
        (* write symbolic instr to asmout *)
        (**********************************)

        if ASM then
          begin
            HEXHW ( PCOUNTER * 2 , HEXPC ) ;
            WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
            WRITELN ( ASMOUT , XTBLN [ OP ] : COLASMI , ' ' : SPACEASMI
                      , R1 : 1 , ',' , R2 : 1 ) ;
          end (* then *) ;

        (**********************************)
        (* insert instr into code buffer  *)
        (**********************************)

        CODE . H [ PCOUNTER ] := TO_HINT ( OP * SL8 + R1 * 16 + R2 ) ;

        (**********************************)
        (* increment instr counter        *)
        (**********************************)

        PCOUNTER := NEXTPC ( 1 ) ;
        with LAST_CC do
          begin
            LPC := PCOUNTER ;
            LR := R1 ;
            LOP := OP
          end (* with *) ;
      end (* GENRR *) ;


   procedure GENRXLIT ( OP : BYTE ; R : RGRNG ; D : INTEGER ; TAG :
                      INTEGER ) ;

      FORWARD ;


   procedure GENRX_2 ( OP : BYTE ; R : RGRNG ; D : ADRRNG ; X , B :
                     RGRNG ; OPTION : INTEGER ) ;

      begin (* GENRX_2 *)
        if R = TRG14 then
          TXR_CONTENTS . VALID := FALSE ;
        if ( D < 0 ) or ( D > SHRTINT ) then
          begin

        (*********************************)
        (*THIS SHOULD NOT BE THE CASE NOW*)
        (*********************************)

            ERROR ( 608 ) ;
            TXR_CONTENTS . VALID := FALSE ;
            if B = TXRG then
              GENRXLIT ( XA , TXRG , D , 0 )
            else
              begin
                GENRXLIT ( XL , TXRG , D , 0 ) ;
                if B = 0 then
                  B := TXRG
                else
                  if X = 0 then
                    X := TXRG
                  else
                    begin
                      GENRR ( XAR , TXRG , B ) ;
                      B := TXRG
                    end (* else *) ;
              end (* else *) ;
            D := 0
          end (* then *) ;

        (**********************************)
        (* write symbolic instr to asmout *)
        (**********************************)

        if ASM then
          begin
            HEXHW ( PCOUNTER * 2 , HEXPC ) ;
            WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
            WRITE ( ASMOUT , XTBLN [ OP ] : COLASMI , ' ' : SPACEASMI ,
                    R : 1 , ',' ) ;
            case OPTION of
              99 : ;
              3 : begin
                    WRITE ( ASMOUT , '<constant>' ) ;
                    if ( X > 0 ) or ( B > 0 ) then
                      begin
                        WRITE ( ASMOUT , '(' , X : 1 ) ;
                        if B > 0 then
                          WRITE ( ASMOUT , ',' , B : 1 ) ;
                        WRITE ( ASMOUT , ')' ) ;
                      end (* then *) ;
                  end (* tag/ca *) ;
              2 : WRITE ( ASMOUT , '<constant>' ) ;
              1 : begin
                    WRITE ( ASMOUT , D : 1 ) ;
                    if ( X > 0 ) or ( B > 0 ) then
                      begin
                        WRITE ( ASMOUT , '(' , X : 1 ) ;
                        if B > 0 then
                          WRITE ( ASMOUT , ',' , B : 1 ) ;
                        WRITE ( ASMOUT , ')' ) ;
                      end (* then *) ;
                  end (* tag/ca *)
            end (* case *) ;
            if OPTION <> 99 then
              WRITELN ( ASMOUT ) ;
          end (* then *) ;

        (**********************************)
        (* insert instr into code buffer  *)
        (**********************************)

        CODE . H [ PCOUNTER ] := TO_HINT ( OP * SL8 + R * 16 + X ) ;
        CODE . H [ PCOUNTER + 1 ] := TO_HINT ( SL12 * B + D ) ;

        (**********************************)
        (* increment instr counter        *)
        (**********************************)

        PCOUNTER := NEXTPC ( 2 ) ;
        with LAST_CC do
          begin
            LPC := PCOUNTER ;
            LR := R ;
            LOP := OP
          end (* with *) ;
      end (* GENRX_2 *) ;


   procedure GENRX ( OP : BYTE ; R : RGRNG ; D : ADRRNG ; X , B : RGRNG
                   ) ;

      begin (* GENRX *)
        GENRX_2 ( OP , R , D , X , B , 1 )
      end (* GENRX *) ;


   procedure GENRXLIT ;

      begin (* GENRXLIT *)
        if R = TRG14 then
          TXR_CONTENTS . VALID := FALSE ;
        if TAG >= 0 then
          if ( OP >= XL ) and ( OP <= XS ) then
            if ( D >= - 32768 ) and ( D <= 32767 ) then
              begin
                OP := OP - 16 ;

        (***********************)
        (* USE HALFWORD INSTR. *)
        (***********************)

                TAG := - 1 ;
              end (* then *) ;
        if OP = XLH then
          if ( D >= 0 ) and ( D <= SHRTINT ) then
            begin
              GENRX ( XLA , R , D , 0 , 0 ) ;
              return
            end (* then *) ;
        if OP = XAH then
          if D = - 1 then
            begin
              GENRR ( XBCTR , R , 0 ) ;
              return
            end (* then *) ;
        if OP = XSH then
          if D = 1 then
            begin
              GENRR ( XBCTR , R , 0 ) ;
              return
            end (* then *) ;

        (**********************************)
        (* write symbolic instr to asmout *)
        (**********************************)

        if ASM then
          begin
            HEXHW ( PCOUNTER * 2 , HEXPC ) ;
            WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
            WRITE ( ASMOUT , XTBLN [ OP ] : COLASMI , ' ' : SPACEASMI ,
                    R : 1 ) ;
            if TAG < 0 then
              WRITELN ( ASMOUT , ',=H''' , D : 1 , '''' )
            else
              if TAG = 0 then
                WRITELN ( ASMOUT , ',=F''' , D : 1 , '''' )
              else
                WRITELN ( ASMOUT , ',=H''' , D DIV 65536 : 1 , ',' , D
                          MOD 65536 : 1 , '''' )
          end (* then *) ;

        (**********************************)
        (* insert instr into code buffer  *)
        (**********************************)

        CODE . H [ PCOUNTER ] := TO_HINT ( OP * SL8 + R * 16 ) ;
        if TAG < 0 then
          UPD_HWTBL ( PCOUNTER + 1 , D )
        else
          UPD_INTTBL ( PCOUNTER + 1 , D ) ;

        (**********************************)
        (* increment instr counter        *)
        (**********************************)

        PCOUNTER := NEXTPC ( 2 ) ;
        with LAST_CC do
          begin
            LPC := PCOUNTER ;
            LR := R ;
            LOP := OP
          end (* with *)
      end (* GENRXLIT *) ;


   procedure GENRXDLIT ( OP : BYTE ; R : RGRNG ; VAL : REAL ) ;

      var I : INTEGER ;

      begin (* GENRXDLIT *)
        if OP = XLD then
          if VAL = 0.0 then
            begin
              GENRR ( XSDR , R , R ) ;
              return
            end (* then *) ;

        (**********************************)
        (* write symbolic instr to asmout *)
        (**********************************)

        if ASM then
          begin
            HEXHW ( PCOUNTER * 2 , HEXPC ) ;
            WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
            WRITELN ( ASMOUT , XTBLN [ OP ] : COLASMI , ' ' : SPACEASMI
                      , R : 1 , ',=D''' , VAL : 20 , '''' ) ;
          end (* then *) ;

        (**********************************)
        (* insert instr into code buffer  *)
        (**********************************)

        CODE . H [ PCOUNTER ] := TO_HINT ( OP * SL8 + R * 16 + 00 ) ;
        UPD_DBLTBL ( PCOUNTER + 1 , VAL ) ;

        (**********************************)
        (* increment instr counter        *)
        (**********************************)

        PCOUNTER := NEXTPC ( 2 ) ;
        with LAST_CC do
          begin
            LPC := PCOUNTER ;
            LR := R ;
            LOP := OP
          end (* with *) ;
      end (* GENRXDLIT *) ;


   procedure GENRS ( OP : BYTE ; R1 , R2 : RGRNG ; D : ADRRNG ; B :
                   RGRNG ) ;

      begin (* GENRS *)
        if R1 = TRG14 then
          TXR_CONTENTS . VALID := FALSE ;
        if ( D < 0 ) or ( D > SHRTINT ) then
          begin
            if B <> TXRG then
              GENRR ( XLR , TXRG , B ) ;
            GENRXLIT ( XA , TXRG , D , 0 ) ;
            D := 0 ;
            B := TXRG ;
          end (* then *) ;

        (**********************************)
        (* write symbolic instr to asmout *)
        (**********************************)

        if ASM then
          begin
            HEXHW ( PCOUNTER * 2 , HEXPC ) ;
            WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
            if ( OP <= XSLDA ) and ( OP >= XSRL ) then
              WRITE ( ASMOUT , XTBLN [ OP ] : COLASMI , ' ' : SPACEASMI
                      , R1 : 1 , ',' , D : 1 )
            else
              WRITE ( ASMOUT , XTBLN [ OP ] : COLASMI , ' ' : SPACEASMI
                      , R1 : 1 , ',' , R2 : 1 , ',' , D : 1 ) ;
            if B <> 0 then
              WRITE ( ASMOUT , '(' , B : 1 , ')' ) ;
            WRITELN ( ASMOUT ) ;
          end (* then *) ;

        (**********************************)
        (* insert instr into code buffer  *)
        (**********************************)

        CODE . H [ PCOUNTER ] := TO_HINT ( OP * SL8 + R1 * 16 + R2 ) ;
        CODE . H [ PCOUNTER + 1 ] := TO_HINT ( B * SL12 + D ) ;

        (**********************************)
        (* increment instr counter        *)
        (**********************************)

        PCOUNTER := NEXTPC ( 2 ) ;
      end (* GENRS *) ;


   procedure GENRSLIT ( OP : BYTE ; R1 , R2 : RGRNG ; S : SHORT_SET ) ;

      var LS : LARGE_SET ;

      begin (* GENRSLIT *)
        I_S_R . S := S ;
        if R1 = TRG14 then
          TXR_CONTENTS . VALID := FALSE ;

        (**********************************)
        (* write symbolic instr to asmout *)
        (**********************************)

        if ASM then
          begin
            HEXHW ( PCOUNTER * 2 , HEXPC ) ;
            WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' , XTBLN [ OP ] :
                    COLASMI , ' ' : SPACEASMI , R1 : 1 , ',' , R2 : 1 ,
                    ',' ) ;

        (*************************************************)
        (* it is sufficient to assign the first part of  *)
        (* ls, because print_set will only use this part *)
        (*************************************************)

            LS . S [ 1 ] := S ;
            PRINT_SET ( LS , 8 ) ;
          end (* then *) ;

        (**********************************)
        (* insert instr into code buffer  *)
        (**********************************)

        CODE . H [ PCOUNTER ] := TO_HINT ( OP * SL8 + R1 * 16 + R2 ) ;

        (**********************************)
        (* increment instr counter        *)
        (**********************************)

        PCOUNTER := NEXTPC ( 2 ) ;
        UPD_DBLTBL ( PCOUNTER - 1 , I_S_R . R ) ;
      end (* GENRSLIT *) ;


   procedure GENSS ( OP : BYTE ; LNGTH : BYTE_PLUS_ONE ; D1 : ADRRNG ;
                   B1 : RGRNG ; D2 : ADRRNG ; B2 : RGRNG ) ;

      begin (* GENSS *)

        (**********************************)
        (* write symbolic instr to asmout *)
        (**********************************)

        if ASM then
          begin
            HEXHW ( PCOUNTER * 2 , HEXPC ) ;
            WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
            WRITELN ( ASMOUT , XTBLN [ OP ] : COLASMI , ' ' : SPACEASMI
                      , D1 : 1 , '(' , LNGTH : 1 , ',' , B1 : 1 , '),'
                      , D2 : 1 , '(' , B2 : 1 , ')' ) ;
          end (* then *) ;

        (**********************************)
        (* insert instr into code buffer  *)
        (**********************************)

        CODE . H [ PCOUNTER ] := TO_HINT ( OP * SL8 + ( LNGTH - 1 ) ) ;
        CODE . H [ PCOUNTER + 1 ] := TO_HINT ( B1 * SL12 + D1 ) ;
        CODE . H [ PCOUNTER + 2 ] := TO_HINT ( B2 * SL12 + D2 ) ;

        (**********************************)
        (* increment instr counter        *)
        (**********************************)

        PCOUNTER := NEXTPC ( 3 ) ;
      end (* GENSS *) ;


   procedure GENSI ( OP : BYTE ; D : ADRRNG ; B : RGRNG ; I : BYTE ) ;

      begin (* GENSI *)

        (**********************************)
        (* write symbolic instr to asmout *)
        (**********************************)

        if ASM then
          begin
            HEXHW ( PCOUNTER * 2 , HEXPC ) ;
            WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
            WRITELN ( ASMOUT , XTBLN [ OP ] : COLASMI , ' ' : SPACEASMI
                      , D : 1 , '(' , B : 1 , '),' , I : 1 ) ;
          end (* then *) ;

        (**********************************)
        (* insert instr into code buffer  *)
        (**********************************)

        CODE . H [ PCOUNTER ] := TO_HINT ( OP * SL8 + I ) ;
        CODE . H [ PCOUNTER + 1 ] := TO_HINT ( B * SL12 + D ) ;

        (**********************************)
        (* increment instr counter        *)
        (**********************************)

        PCOUNTER := NEXTPC ( 2 ) ;
      end (* GENSI *) ;


   procedure GENSSLIT ( OP , LNGTH : BYTE ; D1 : ADRRNG ; B1 : RGRNG ;
                      S : LARGE_SET ) ;

      begin (* GENSSLIT *)
        if LNGTH = 1 then

        (*********************************)
        (* SUBSTITUTE AN IMMEDIATE INST. *)
        (*********************************)

          begin
            I_S_R . S := S . S [ 1 ] ;
            GENSI ( OP - XMVC + XMVI , D1 , B1 , ORD ( I_S_R . C1 ) ) ;
          end (* then *)
        else
          if LNGTH > 1 then
            begin

        (**********************************)
        (* write symbolic instr to asmout *)
        (**********************************)

              if ASM then
                begin
                  HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                  WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
                  WRITE ( ASMOUT , XTBLN [ OP ] : COLASMI , ' ' :
                          SPACEASMI , D1 : 1 , '(' , LNGTH : 1 , ',' ,
                          B1 : 1 , '),' ) ;
                  PRINT_SET ( S , LNGTH ) ;
                end (* then *) ;

        (**********************************)
        (* insert instr into code buffer  *)
        (**********************************)

              CODE . H [ PCOUNTER ] := TO_HINT ( OP * SL8 + ( LNGTH - 1
                                       ) ) ;
              CODE . H [ PCOUNTER + 1 ] := TO_HINT ( B1 * SL12 + D1 ) ;
              UPD_SETTBL ( PCOUNTER + 2 , S , LNGTH ) ;

        (**********************************)
        (* increment instr counter        *)
        (**********************************)

              PCOUNTER := NEXTPC ( 3 ) ;
            end (* then *) ;
      end (* GENSSLIT *) ;


   procedure GENAL2 ( PC : ICRNG ; LAB : PLABEL ) ;

      var INTLBL : INTEGER ;

      begin (* GENAL2 *)
        if ASM then
          begin
            HEXHW ( PC * 2 , HEXPC ) ;
            WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
            WRITELN ( ASMOUT , ' DC AL2(' , LAB . NAM : LAB . LEN , '-'
                      , PRCTBL [ 0 ] . NAME , ')' ) ;
          end (* then *) ;
        INTLBL := LBLMAP ( LAB . NAM ) ;
        UPD_LBLTBL ( PC , INTLBL , FALSE , TRUE ) ;
        if FALSE then
          begin
            WRITELN ( TRACEF , 'genal2: pc      = ' , PC ) ;
            WRITELN ( TRACEF , 'genal2: intlbl  = ' , INTLBL ) ;
          end (* then *) ;
      end (* GENAL2 *) ;


   procedure GENRXLAB ( OP : BYTE ; R : RGRNG ; LAB : PLABEL ; TAG :
                      INTEGER ) ;

      begin (* GENRXLAB *)
        if R = TRG14 then
          TXR_CONTENTS . VALID := FALSE ;

        (**********************************)
        (* write symbolic instr to asmout *)
        (**********************************)

        if ASM then
          begin
            HEXHW ( PCOUNTER * 2 , HEXPC ) ;
            WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
            if CASE_FLAG then
              WRITELN ( ASMOUT , ' DC AL2(' , LAB . NAM : LAB . LEN ,
                        '-' , PRCTBL [ 0 ] . NAME , ')' )
            else
              begin
                WRITE ( ASMOUT , XTBLN [ OP ] : COLASMI , ' ' :
                        SPACEASMI , R : 1 , ',' ) ;
                if TAG = 0 then
                  WRITE ( ASMOUT , LAB . NAM : LAB . LEN )
                else
                  if TAG > 0 then
                    WRITE ( ASMOUT , LAB . NAM : LAB . LEN , '(' , TAG
                            : 1 , ')' )
                  else
                    begin
                      if TAG = - 3 then
                        WRITE ( ASMOUT , '=V(' )
                      else

        (************)
        (* TAG = -1 *)
        (************)

                        WRITE ( ASMOUT , '=A(' ) ;
                      WRITE ( ASMOUT , LAB . NAM : LAB . LEN ) ;
                      WRITE ( ASMOUT , ')' ) ;
                    end (* else *) ;
                WRITELN ( ASMOUT ) ;
              end (* else *) ;
          end (* then *) ;

        (**********************************)
        (* insert instr into code buffer  *)
        (**********************************)

        if CASE_FLAG then
          begin

        (*********)
        (*LAB REF*)
        (*********)

            UPD_LBLTBL ( PCOUNTER , LBLMAP ( LAB . NAM ) , FALSE , TRUE
                         ) ;
            PCOUNTER := NEXTPC ( 1 ) ;
          end (* then *)
        else
          begin
            if TAG >= - 1 then

        (*****************)
        (*GENERATED LABEL*)
        (*****************)

              UPD_LBLTBL ( PCOUNTER + 1 , LBLMAP ( LAB . NAM ) , FALSE
                           , FALSE )

        (*****************)
        (*LABEL REFERENCE*)
        (*****************)

            else

        (***********)
        (*PROC. ID.*)
        (***********)

              UPD_PRCTBL ( PCOUNTER + 1 , LAB . NAM ) ;
            if TAG < 0 then
              TAG := 0 ;
            CODE . H [ PCOUNTER ] := TO_HINT ( OP * SL8 + R * 16 + TAG
                                     ) ;
            PCOUNTER := NEXTPC ( 2 )
          end (* else *)
      end (* GENRXLAB *) ;


   procedure GENRELRX ( OP : BYTE ; R : RGRNG ; OFFSET : HINTEGER ) ;

   (****************************************)
   (* OPERAND OF RX INST. IS "*+2*OFFSET"  *)
   (****************************************)


      var SAVEASM : BOOLEAN ;

      begin (* GENRELRX *)

        (**********************************)
        (* write symbolic instr to asmout *)
        (**********************************)

        if ASM then
          begin
            HEXHW ( PCOUNTER * 2 , HEXPC ) ;
            WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
            WRITELN ( ASMOUT , XTBLN [ OP ] : COLASMI , ' ' : SPACEASMI
                      , R : 1 , ',*+' , 2 * OFFSET : 1 ) ;
          end (* then *) ;

        (**********************************)
        (* insert instr into code buffer  *)
        (**********************************)

        SAVEASM := ASM ;
        ASM := FALSE ;
        GENRX ( OP , R , 0 , 0 , 0 ) ;
        CODE . H [ PCOUNTER - 1 ] := TO_HINT ( BASE_DSPLMT ( PCOUNTER +
                                     OFFSET - 2 ) ) ;
        ASM := SAVEASM ;
      end (* GENRELRX *) ;


   procedure CONS_REGS ( var BASEREG : LVLRNG ; var INDEXREG : LVLRNG )
                       ;

      begin (* CONS_REGS *)
        if INDEXREG = 0 then
          return ;
        if BASEREG = 0 then
          begin
            BASEREG := INDEXREG ;
            INDEXREG := 0 ;
            return
          end (* then *) ;
        GENRR ( XAR , BASEREG , INDEXREG ) ;
        INDEXREG := 0 ;
      end (* CONS_REGS *) ;


   procedure BRANCH_CHAIN ( LPC : ICRNG ) ;

      label 10 ;

      const X47F0 = 18416 ;
            X4700 = 18176 ;
            X4000 = 16384 ;
            XC000 = 19152 ;
            MAXCNT = 5 ;

      var BC15 , TI , DI : record
                             case INTEGER of
                               1 :
                                 ( I : INTEGER ) ;
                               2 :
                                 ( S : set of 0 .. 31 ) ;
                           end ;
          TPC , DPC : ICRNG ;
          CNT : 0 .. MAXCNT ;
          TIOP : INTEGER ;

      begin (* BRANCH_CHAIN *)
        BC15 . I := X47F0 ;
        TPC := PCAFTLIT ;
        repeat
          TI . I := CODE . H [ TPC ] ;
          if TI . I > X4700 then
            if TI . S <= BC15 . S then

        (*******************************)
        (* MUST BE UNINDEXED BC INSTR. *)
        (*******************************)

              begin
                CNT := 0 ;
                repeat
                  TIOP := CODE . H [ TPC + 1 ] ;
                  if TIOP < 0 then
                    TIOP := TIOP + 65536 ;
                  DPC := ( TIOP MOD SL12 ) DIV 2 ;
                  TIOP := TIOP DIV SL12 - PBR1 ;
                  if TIOP < 0 then
                    goto 10 ;
                  if TIOP > 0 then
                    if TIOP > 1 then
                      goto 10
                    else
                      DPC := DPC + 2046 ;
                  if DPC >= LPC then
                    goto 10 ;
                  DI . I := CODE . H [ DPC ] ;
                  if DI . I <= X4700 then
                    goto 10 ;
                  if DI . I > X47F0 then
                    goto 10 ;
                  if not ( TI . S <= DI . S ) then
                    goto 10 ;
                  TIOP := CODE . H [ DPC + 1 ] ;
                  CODE . H [ TPC + 1 ] := TO_HINT ( TIOP ) ;
                  CNT := CNT + 1 ;
                until CNT > MAXCNT ;
                10 :
                
              end (* then *) ;
          if TI . I < 0 then
            TI . I := TI . I + 65536 ;
          if TI . I < X4000 then
            TPC := TPC + 1

        (********)
        (* RR   *)
        (********)

          else
            if TI . I < XC000 then
              TPC := TPC + 2

        (********)
        (* RX   *)
        (********)

            else
              TPC := TPC + 3 ;

        (********)
        (* SS   *)
        (********)

        until TPC >= LPC ;
      end (* BRANCH_CHAIN *) ;


   procedure DUMP_LITERALS ;

   (***************************************************)
   (* PROCEDURE TO EMPTY LITERAL POOL INTO CODE ARRAY *)
   (***************************************************)


      var I : INTEGER ;
          QPC : ICRNG ;
          TPC : ICRNG_EXT ;
          L : INTEGER ;

      begin (* DUMP_LITERALS *)
        if OPT_FLG then
          if not DEBUG then
            BRANCH_CHAIN ( PCOUNTER ) ;
        if ODD ( PCOUNTER ) then
          GENRR ( XBCR , 0 , 0 ) ;
        if DBLALN then
          if ( PCOUNTER MOD 4 ) <> 0 then
            GENRX ( XBC , 0 , 0 , 0 , 0 ) ;
        if NXTLIT > 0 then
          if ( LX . NXTDBL * 4 + PCOUNTER ) <= 8187 then
            begin
              for I := 1 to NXTLIT do
                begin
                  TPC := LITTBL [ I ] . LNK ;
                  if TPC > 0 then

        //******************************************************
        // USUAL CASE                                           
        //******************************************************

                    begin
                      QPC := CODE . H [ TPC ] ;
                      CODE . H [ TPC ] := TO_HINT ( BASE_DSPLMT ( QPC
                                          DIV 2 + PCOUNTER ) ) ;
                      if ODD ( QPC ) then
                        CODE . H [ TPC ] := TO_HINT ( CODE . H [ TPC ]
                                            + 1 ) ;
                    end (* then *)
                  else
                    ERROR ( 257 ) ;
                end (* for *) ;

        //******************************************************
        // copy literal pool into code array                    
        //******************************************************

              TPC := LX . NXTDBL * 2 - 1 ;
              if LX . INT_GAP = TPC then
                TPC := TPC - 1 ;
              POOL_SIZE := POOL_SIZE + TPC * 2 ;
              NUMLITS := NUMLITS + NXTLIT ;
              QPC := PCOUNTER DIV 2 ;
              L := TPC + 1 ;
              MEMCPY ( ADDR ( CODE . I [ QPC ] ) , ADDR ( IDP_POOL . I
                       [ 0 ] ) , L * 4 ) ;
              QPC := QPC + L ;
              PCOUNTER := QPC * 2 ;
            end (* then *)
          else
            ERROR ( 255 ) ;
        NXTLIT := 0 ;
        LX . NXTDBL := 0 ;
        LX . IHCONF := - 1 ;
        LX . RICONF := - 1 ;
        LX . RHCONF := - 1 ;
        LX . INT_GAP := - 1 ;
        LX . HW_GAP := - 1 ;
        DBLALN := FALSE ;
        PCAFTLIT := PCOUNTER ;
        LX . NXTCH := 0 ;
      end (* DUMP_LITERALS *) ;


   procedure FINDRG ;

   (***********************)
   (*TO FIND A GP REGISTER*)
   (***********************)


      var I : RGRNG ;

      begin (* FINDRG *)
        I := 1 ;
        repeat
          I := I + 1
        until ( AVAIL [ I ] or ( I = RGCNT ) ) ;
        if not AVAIL [ I ] then
          ERROR ( 259 ) ;
        AVAIL [ I ] := FALSE ;
        NXTRG := I ;
      end (* FINDRG *) ;


   procedure FINDRP ;

   (********************)
   (*FIND REGISTER PAIR*)
   (********************)


      var I : RGRNG ;

      begin (* FINDRP *)
        I := RGCNT + 1 ;
        repeat
          I := I - 2
        until ( I < 4 ) or ( AVAIL [ I ] and AVAIL [ I + 1 ] ) ;
        if not ( AVAIL [ I ] and AVAIL [ I + 1 ] ) then
          ERROR ( 259 ) ;
        AVAIL [ I ] := FALSE ;
        AVAIL [ I + 1 ] := FALSE ;
        NXTRG := I
      end (* FINDRP *) ;


   procedure FINDFP ;

   (********************************)
   (*FIND A FLOATING POINT REGISTER*)
   (********************************)


      var I : INTEGER ;

      begin (* FINDFP *)
        I := 0 ;
        repeat
          I := I + 2
        until AVAILFP [ I ] or ( I = FPCNT ) ;
        if not AVAILFP [ I ] then
          ERROR ( 259 ) ;
        AVAILFP [ I ] := FALSE ;
        NXTRG := I
      end (* FINDFP *) ;


   procedure FREEREG ( var STE : DATUM ) ;

      begin (* FREEREG *)
        with STE do
          if VRBL and ( VPA = RGS ) then
            begin
              if DTYPE = REEL then
                AVAILFP [ RGADR ] := TRUE
              else

        (*****************)
        (* DTYPE <> REEL *)
        (*****************)

                AVAIL [ RGADR ] := TRUE ;
              if DRCT and ( DTYPE = PSET ) then
                if PLEN > 4 then

        (********************)
        (* REG. PAIR IN USE *)
        (********************)

                  AVAIL [ RGADR + 1 ] := TRUE
            end (* then *)
      end (* FREEREG *) ;


   function ALIGN ( Q , P : INTEGER ) : INTEGER ;

      var I : INTEGER ;

      begin (* ALIGN *)
        ALIGN := Q ;
        I := Q MOD P ;
        if I <> 0 then
          ALIGN := Q + ( P - I ) ;
      end (* ALIGN *) ;


   function POWER2 ( I : INTEGER ) : INTEGER ;

   (********************************************************)
   (* IF I > 0 IS A POWER OF TWO, RETURN 'THAT' POWER,     *)
   (* ELSE RETURN NEGATIVE                                 *)
   (* ---------------------------------------------------- *)
   (********************************************************)


      var K : INTEGER ;

      begin (* POWER2 *)
        POWER2 := - 999 ;
        if I > 0 then
          begin
            K := 0 ;
            while not ODD ( I ) do
              begin
                I := I DIV 2 ;
                K := K + 1 ;
              end (* while *) ;
            if I = 1 then
              POWER2 := K ;
          end (* then *) ;
      end (* POWER2 *) ;


   procedure BASE ( var Q : ADRRNG ; var P , B : LVLRNG ) ;

   (*******************************************************)
   (* TO TRANSLATE A 'LEVEL/OFFSET' P/Q ADDRESS           *)
   (* TO 'BASE/INDEX/DISPLACEMENT'                        *)
   (* --------------------------------------------------- *)
   (*******************************************************)


      const MAXDISP = 4088 ;
            SHRTINT2 = 8183 ;

            (*********************)
            (* SHRTINT + MAXDISP *)
            (*********************)


      var T , TQ : ADRRNG ;
          TP : LVLRNG ;

      begin (* BASE *)
        B := 0 ;
        if P < 0 then
          return ;

        (*******************)
        (* STRING CONSTANT *)
        (*******************)

        TQ := Q ;
        TP := P ;
        if OPT_FLG then
          with TXR_CONTENTS do
            if TP = LEVEL then
              if VALID then
                if TXRG = TRG14 then
                  begin
                    T := TQ - OFFSET + DISP ;
                    if ( T >= 0 ) and ( T <= MAXDISP ) then
                      begin
                        Q := T ;
                        P := TRG14 ;
                        B := BASE ;
                        return
                      end (* then *) ;
                  end (* then *) ;
        if P > 0 then
          if P = CURLVL then
            begin
              B := LBR ;
              P := 0
            end (* then *)
          else
            if P = 1 then
              begin
                B := GBR ;
                P := 0
              end (* then *)
            else
              begin
                GENRX ( XL , TXRG , DISPLAY + 4 * P , GBR , 0 ) ;
                P := TXRG ;
              end (* else *) ;
        if ( Q < 0 ) or ( Q > SHRTINT2 ) then
          begin
            Q := Q - 2048 ;
            if P > 0 then
              GENRXLIT ( XA , P , Q , 0 )
            else
              begin
                GENRXLIT ( XL , TXRG , Q , 0 ) ;
                P := TXRG
              end (* else *) ;
            Q := 2048
          end (* then *)
        else
          if Q > SHRTINT then
            begin
              GENRX ( XLA , TXRG , MAXDISP , B , P ) ;
              Q := Q - MAXDISP ;
              P := TXRG ;
              B := 0 ;
            end (* then *) ;
        if P = TRG14 then
          with TXR_CONTENTS do
            begin
              VALID := TRUE ;
              LEVEL := TP ;
              OFFSET := TQ ;
              DISP := Q ;
              BASE := B ;
            end (* with *) ;
      end (* BASE *) ;


   procedure CHECKDISP ( var Q : ADRRNG ; var P , B : LVLRNG ) ;

   (*********************************************************)
   (*  TO ELIMINATE THE RESULT Q=4092                       *)
   (*  THAT MAY BE GENERATED BY BASE                        *)
   (*  AND WHICH CAUSES TROUBLE FOR OPERATIONS ON SETS      *)
   (*********************************************************)


      begin (* CHECKDISP *)
        if Q > ( SHRTINT - 4 ) then
          begin
            GENRX ( XLA , TXRG , SHRTINT - 4 , B , P ) ;
            Q := Q - ( SHRTINT - 4 ) ;
            P := TXRG ;
            B := 0
          end (* then *)
      end (* CHECKDISP *) ;


   procedure GENLA_LR ( R1 : RGRNG ; Q2 : ADRRNG ; R2 , X2 : RGRNG ) ;// genrr xlr

      begin (* GENLA_LR *)
        if Q2 = 0 then
          if R2 = 0 then
            GENRR ( XLR , R1 , X2 )
          else
            if X2 = 0 then
              GENRR ( XLR , R1 , R2 )
            else
              GENRX ( XLA , R1 , 0 , R2 , X2 )
        else
          GENRX ( XLA , R1 , Q2 , R2 , X2 )
      end (* GENLA_LR *) ;


   procedure GETADR ( STE : DATUM ; var Q : ADRRNG ; var P , B : RGRNG
                    ) ;

      FORWARD ;


   procedure LOAD ( var STE : DATUM ) ;

   (****************************************************************)
   (* LOADS AN STACK ELEMENT INTO A REGISTER, IF NOT ALREADY THERE *)
   (* ------------------------------------------------------------ *)
   (****************************************************************)


      var P : LVLRNG ;
          Q : ADRRNG_EXT ;
          B , R : RGRNG ;
          OP : BYTE ;
          LBL_WORK : PLABEL ;


      procedure FINDMDRG ;

      (************************************)
      (*TO FIND A MULTIPLY/DIVIDE REGISTER*)
      (************************************)


         begin (* FINDMDRG *)
           if MDTAG = PDVI then
             begin
               FINDRP ;
               AVAIL [ NXTRG + 1 ] := TRUE
             end (* then *)
           else
             if MDTAG = PMPI then
               begin
                 FINDRP ;
                 AVAIL [ NXTRG ] := TRUE ;
                 NXTRG := NXTRG + 1
               end (* then *)
             else
               FINDRG ;
         end (* FINDMDRG *) ;


      begin (* LOAD *)
        with STE do
          begin
            if VRBL then

        (**************************************)
        (* LOAD THE VARIABLE POINTED TO BY STP*)
        (**************************************)

              if DRCT then

        (******************************)
        (*DIRECTLY ACCESSIBLE VARIABLE*)
        (******************************)

                case DTYPE of
                  ADR , HINT , INT , BOOL , CHRC :
                    begin
                      if VPA = MEM then
                        begin
                          FINDMDRG ;
                          P := MEMADR . LVL ;
                          Q := MEMADR . DSPLMT ;
                          BASE ( Q , P , B ) ;
                          case DTYPE of
                            CHRC , BOOL :
                              begin
                                if CLEAR_REG then
                                  GENRR ( XSR , NXTRG , NXTRG ) ;
                                GENRX ( XIC , NXTRG , Q , B , P ) ;
                              end (* tag/ca *) ;
                            INT , ADR :
                              GENRX ( XL , NXTRG , Q , B , P ) ;
                            HINT : begin
                                     GENRX ( XLH , NXTRG , Q , B , P )
                                             ;
                                     DTYPE := INT
                                   end (* tag/ca *) ;
                          end (* case *) ;
                          VPA := RGS ;
                          RGADR := NXTRG ;
                        end (* then *) ;
                      if FALSE then
                        begin
                          if PROCNAME <> '        ' then
                            begin
                              TXT2LBL ( LBL_WORK , PROCNAME ) ;
                              WRITELN ( 'LA V-Konst ' , PROCNAME ) ;
                              GENRXLAB ( XL , RGADR , LBL_WORK , - 3 )
                            end (* then *)
                        end (* then *) ;
                      P := FPA . LVL ;
                      Q := FPA . DSPLMT ;
                      FPA := ZEROBL ;
                      if Q <> 0 then
                        if P > 0 then
                          begin
                            BASE ( Q , P , B ) ;
                            if P <= 0 then
                              P := B
                            else
                              if B > 0 then
                                GENRR ( XAR , P , B ) ;
                            if Q = 0 then
                              GENRR ( XAR , RGADR , P )
                            else
                              GENRX ( XLA , RGADR , Q , P , RGADR ) ;
                          end (* then *)
                        else
                          if Q = - 1 then
                            GENRR ( XBCTR , RGADR , 0 )
                          else
                            GENRXLIT ( XA , RGADR , Q , 0 ) ;
                    end (* tag/ca *) ;
                  REEL : if VPA = MEM then
                           begin
                             FINDFP ;
                             P := MEMADR . LVL ;
                             Q := MEMADR . DSPLMT ;
                             BASE ( Q , P , B ) ;
                             GENRX ( XLD , NXTRG , Q , B , P ) ;
                             VPA := RGS ;
                             RGADR := NXTRG ;
                           end (* then *) ;
                  PSET : if VPA <> RGS then
                           begin
                             if VPA = MEM then
                               begin
                                 P := MEMADR . LVL ;
                                 Q := MEMADR . DSPLMT
                               end (* then *)
                             else
                               begin
                                 P := CURLVL ;
                                 Q := STKADR
                               end (* else *) ;
                             BASE ( Q , P , B ) ;
                             if PLEN <= 8 then
                               if PLEN > 4 then
                                 begin
                                   FINDRP ;
                                   if B > 0 then
                                     if P > 0 then
                                       GENRR ( XAR , P , B )
                                     else
                                       P := B ;
                                   GENRS ( XLM , NXTRG , NXTRG + 1 , Q
                                           , P ) ;
                                 end (* then *)
                               else
                                 if PLEN > 0 then
                                   begin
                                     FINDRG ;
                                     GENRX ( XL , NXTRG , Q , P , B ) ;
                                   end (* then *) ;
                             VPA := RGS ;
                             RGADR := NXTRG ;
                           end (* then *) ;
                end (* case *)
              else

        //******************************************************
        // if not drct                                          
        //******************************************************

                begin
                  GETADR ( STE , Q , P , B ) ;
                  FPA := ZEROBL ;
                  case DTYPE of
                    ADR , HINT , INT :
                      begin
                        if VPA = RGS then
                          AVAIL [ RGADR ] := TRUE ;
                        FINDMDRG ;
                        if DTYPE <> HINT then
                          GENRX ( XL , NXTRG , Q , B , P )
                        else
                          begin
                            GENRX ( XLH , NXTRG , Q , B , P ) ;
                            DTYPE := INT
                          end (* else *) ;
                      end (* tag/ca *) ;
                    BOOL , CHRC :
                      begin
                        FINDMDRG ;
                        if CLEAR_REG then
                          GENRR ( XSR , NXTRG , NXTRG ) ;
                        GENRX ( XIC , NXTRG , Q , B , P ) ;
                        if VPA = RGS then
                          AVAIL [ RGADR ] := TRUE ;
                      end (* tag/ca *) ;
                    REEL : begin
                             FINDFP ;
                             GENRX ( XLD , NXTRG , Q , B , P ) ;
                             if VPA = RGS then
                               AVAIL [ RGADR ] := TRUE ;
                           end (* tag/ca *) ;
                    PSET : begin
                             if VPA = RGS then
                               AVAIL [ RGADR ] := TRUE ;
                             if PLEN <= 8 then
                               if PLEN > 4 then
                                 begin
                                   FINDRP ;
                                   if B > 0 then
                                     if P > 0 then
                                       GENRR ( XAR , P , B )
                                     else
                                       P := B ;
                                   GENRS ( XLM , NXTRG , NXTRG + 1 , Q
                                           , P ) ;
                                 end (* then *)
                               else
                                 if PLEN > 0 then
                                   begin
                                     FINDRG ;
                                     GENRX ( XL , NXTRG , Q , P , B ) ;
                                   end (* then *) ;
                           end (* tag/ca *)
                  end (* case *) ;
                  VPA := RGS ;
                  RGADR := NXTRG ;
                  DRCT := TRUE ;
                end (* else *)
            else

        //******************************************************
        // IF NOT VRBL, I.E. LOAD CONSTANT                      
        //******************************************************

              begin
                case DTYPE of
                  ADR : begin
                          P := FPA . LVL ;
                          Q := FPA . DSPLMT ;
                          FINDRG ;
                          if P > 0 then
                            begin
                              BASE ( Q , P , B ) ;
                              GENRX ( XLA , NXTRG , Q , B , P )
                            end (* then *)
                          else
                            if P < 0 then
                              begin
                                GENRX_2 ( XLA , NXTRG , 0 , 0 , 0 , 2 )
                                          ;
                                if P = - 1 then
                                  LITTBL [ SCNSTNO ] . LNK := PCOUNTER
                                                   - 1 ;
                                CODE . H [ PCOUNTER - 1 ] := TO_HINT (
                                                   Q ) ;
                              end (* then *)
                            else
                              GENRXLIT ( XL , NXTRG , FPA . DSPLMT , 0
                                         ) ;

        (***********)
        (*NIL VALUE*)
        (***********)

                          FPA := ZEROBL ;
                        end (* tag/ca *) ;
                  HINT , INT , BOOL , CHRC :
                    begin
                      FINDMDRG ;
                      if FPA . DSPLMT = 0 then
                        GENRR ( XSR , NXTRG , NXTRG )
                      else
                        GENRXLIT ( XL , NXTRG , FPA . DSPLMT , 0 ) ;
                      FPA := ZEROBL ;
                    end (* tag/ca *) ;
                  REEL : begin
                           FINDFP ;
                           GENRXDLIT ( XLD , NXTRG , RCNST )
                         end (* tag/ca *) ;
                  PSET : if PLEN <= 8 then
                           if PLEN > 4 then
                             begin
                               FINDRP ;
                               GENRSLIT ( XLM , NXTRG , NXTRG + 1 ,
                                          PCNST -> . S [ 1 ] ) ;
                             end (* then *)
                           else
                             if PLEN > 0 then
                               begin
                                 FINDRG ;
                                 I_S_R . S := PCNST -> . S [ 1 ] ;
                                 GENRXLIT ( XL , NXTRG , I_S_R . I1 , 0
                                            ) ;
                               end (* then *)
                             else

        //******************************************************
        // PLEN = 0                                             
        //******************************************************

                               begin
                                 FINDRG ;
                                 PLEN := 4 ;
                                 GENRR ( XSR , NXTRG , NXTRG ) ;
                               end (* else *) ;
                end (* case *) ;
                VRBL := TRUE ;
                VPA := RGS ;
                RGADR := NXTRG ;
                DRCT := TRUE ;
              end (* else *) ;
            if MDTAG = PMPI then
              begin
                if not ODD ( RGADR ) or not AVAIL [ RGADR - 1 ] then
                  begin
                    AVAIL [ RGADR ] := TRUE ;
                    FINDRP ;
                    GENRR ( XLR , NXTRG + 1 , RGADR ) ;
                    RGADR := NXTRG + 1 ;
                  end (* then *) ;
                RGADR := RGADR - 1 ;
                AVAIL [ RGADR ] := FALSE
              end (* then *)
            else
              if MDTAG = PDVI then
                begin
                  if ODD ( RGADR ) or not AVAIL [ RGADR + 1 ] then
                    begin
                      AVAIL [ RGADR ] := TRUE ;
                      FINDRP ;
                      GENRR ( XLR , NXTRG , RGADR ) ;
                      RGADR := NXTRG ;
                    end (* then *) ;
                  AVAIL [ RGADR + 1 ] := FALSE ;
                  GENRS ( XSRDA , RGADR , 0 , 32 , 0 ) ;
                end (* then *)
          end (* with *) ;
      end (* LOAD *) ;


   procedure GETADR ;

   (***************************************************)
   (* IF PASSED THE ADR. OF AN ITEM,                  *)
   (* THIS ROUTINE RETURNS A <Q,B,P> ADR.             *)
   (* INDIRECTIONS ARE NOT DEREFERENCED HERE.         *)
   (* ----------------------------------------------- *)
   (***************************************************)


      var R : RGRNG ;

      begin (* GETADR *)
        R := 0 ;
        with STE do
          begin
            if DRCT and not ( DTYPE in [ ADR ] ) then
              ERROR ( 602 ) ;
            if VRBL then
              if VPA = RGS then
                R := RGADR
              else

        (***************************)
        (*VPA = MEM OR VPA = ONSTK *)
        (***************************)

                begin
                  if VPA = MEM then
                    begin
                      P := MEMADR . LVL ;
                      Q := MEMADR . DSPLMT
                    end (* then *)
                  else
                    ERROR ( 616 ) ;
                  BASE ( Q , P , B ) ;
                  GENRX ( XL , TXRG , Q , B , P ) ;
                  R := TXRG
                end (* else *) ;

        (************************************************************)
        (* NOW THE VARIABLE PORTION OF THE ADR., IF ANY, IS IN TXRG *)
        (************************************************************)

            Q := FPA . DSPLMT ;
            P := FPA . LVL ;
            if R > 0 then
              begin
                if ( Q < 0 ) or ( Q > SHRTINT ) then
                  begin
                    GENRXLIT ( XA , R , Q , 0 ) ;
                    Q := 0
                  end (* then *) ;
                B := 0 ;
                if P = CURLVL then
                  B := LBR
                else
                  if P = 1 then
                    B := GBR
                  else
                    if P > 0 then
                      GENRX ( XA , R , DISPLAY + 4 * P , GBR , 0 ) ;
                P := R ;
              end (* then *)
            else

        (*******************)
        (* NO INDEX OR VPA *)
        (*******************)

              BASE ( Q , P , B ) ;
          end (* with *)
      end (* GETADR *) ;


   procedure GETADR2 ( STE : DATUM ; var Q : ADRRNG ; var P , B : RGRNG
                     ) ;

   (***************************************************)
   (* IF PASSED THE ADR. OF AN ITEM,                  *)
   (* THIS ROUTINE RETURNS A <Q,B,P> ADR.             *)
   (* INDIRECTIONS ARE NOT DEREFERENCED HERE.         *)
   (* ----------------------------------------------- *)
   (***************************************************)


      var R : RGRNG ;

      begin (* GETADR2 *)
        R := 0 ;
        with STE do
          begin
            if DRCT and not ( DTYPE in [ ADR , CHRC , CARR , VARC ] )
            then
              begin
                ERROR ( 602 ) ;
                if FALSE then
                  WRITELN ( TRACEF ,
                            'error 602 inside getadr2: dtype = ' ,
                            DTYPE ) ;
              end (* then *) ;
            if VRBL then
              if VPA = RGS then
                R := RGADR
              else

        (***************************)
        (*VPA = MEM OR VPA = ONSTK *)
        (***************************)

                begin
                  if VPA = MEM then
                    begin
                      P := MEMADR . LVL ;
                      Q := MEMADR . DSPLMT
                    end (* then *)
                  else
                    ERROR ( 616 ) ;
                  BASE ( Q , P , B ) ;
                  if FALSE then
                    begin
                      WRITELN ( TRACEF ,
                                'base/vrbl inside getadr2: q = ' , Q )
                                ;
                      WRITELN ( TRACEF ,
                                'base/vrbl inside getadr2: p = ' , P )
                                ;
                      WRITELN ( TRACEF ,
                                'base/vrbl inside getadr2: b = ' , B )
                    end (* then *) ;
                  GENRX ( XL , TXRG , Q , B , P ) ;
                  R := TXRG
                end (* else *) ;

        (************************************************************)
        (* NOW THE VARIABLE PORTION OF THE ADR., IF ANY, IS IN TXRG *)
        (************************************************************)

            Q := FPA . DSPLMT ;
            P := FPA . LVL ;
            if R > 0 then
              begin
                if ( Q < 0 ) or ( Q > SHRTINT ) then
                  begin
                    GENRXLIT ( XA , R , Q , 0 ) ;
                    Q := 0
                  end (* then *) ;
                B := 0 ;
                if P = CURLVL then
                  B := LBR
                else
                  if P = 1 then
                    B := GBR
                  else
                    if P > 0 then
                      GENRX ( XA , R , DISPLAY + 4 * P , GBR , 0 ) ;
                P := R ;
              end (* then *)
            else

        (*******************)
        (* NO INDEX OR VPA *)
        (*******************)

              BASE ( Q , P , B ) ;
          end (* with *) ;
        if FALSE then
          begin
            WRITELN ( TRACEF , 'base inside getadr2: q = ' , Q ) ;
            WRITELN ( TRACEF , 'base inside getadr2: p = ' , P ) ;
            WRITELN ( TRACEF , 'base inside getadr2: b = ' , B )
          end (* then *) ;
      end (* GETADR2 *) ;


   procedure GETOP_SIMPLE ( var STE : DATUM ; var Q1 : ADRRNG ; var P1
                          , B1 : RGRNG ; TXRG : RGRNG ; INIT_ON_CHAR :
                          BOOLEAN ) ;

   //****************************************************************
   // get operands when target register is already fixed             
   // target register is txrg                                        
   // for example for code generation of new p-codes mcp, mse etc.;  
   // see procedures mcpoperation, mseoperation                      
   // init_on_char: if false, then there is no need to clear the     
   // register before IC, because the garbage bits are shifted       
   // out later anyway.                                              
   //****************************************************************


      begin (* GETOP_SIMPLE *)
        with STE do
          begin

        //******************************************************
        // if reg, simple copy register                         
        // if mem, load register from storage                   
        // if mem and char type, use XR and IC                  
        // if neither, source was constant, use LA              
        //******************************************************

            case VPA of
              RGS : GENRR ( XLR , TXRG , RGADR ) ;
              MEM : begin
                      P1 := MEMADR . LVL ;
                      Q1 := MEMADR . DSPLMT ;
                      BASE ( Q1 , P1 , B1 ) ;
                      case DTYPE of
                        CHRC : begin
                                 if INIT_ON_CHAR then
                                   GENRR ( XXR , TXRG , TXRG ) ;
                                 GENRX ( XIC , TXRG , Q1 , B1 , P1 )
                               end (* tag/ca *) ;
                        HINT : GENRX ( XLH , TXRG , Q1 , B1 , P1 ) ;
                        otherwise
                          GENRX ( XL , TXRG , Q1 , B1 , P1 ) ;
                      end (* case *) ;
                    end (* tag/ca *) ;
              NEITHER :
                GENRX ( XLA , TXRG , FPA . DSPLMT , 0 , 0 ) ;
              otherwise
                
            end (* case *) ;
          end (* with *) ;
      end (* GETOP_SIMPLE *) ;


   procedure GEN_MOVECHAR ( var Q1 : ADRRNG ; var B1 : RGRNG ; var STE
                          : DATUM ) ;

      var P2 , B2 : LVLRNG ;
          Q2 : ADRRNG ;

      begin (* GEN_MOVECHAR *)
        with STE do
          begin
            case VPA of
              RGS : GENRX ( XSTC , RGADR , Q1 , 0 , B1 ) ;
              MEM : begin
                      P2 := MEMADR . LVL ;
                      Q2 := MEMADR . DSPLMT ;
                      BASE ( Q2 , P2 , B2 ) ;
                      CONS_REGS ( B2 , P2 ) ;
                      GENSS ( XMVC , 1 , Q1 , B1 , Q2 , B2 ) ;
                    end (* tag/ca *) ;
              NEITHER :
                GENSI ( XMVI , Q1 , B1 , FPA . DSPLMT ) ;
              otherwise
                
            end (* case *) ;
          end (* with *) ;
      end (* GEN_MOVECHAR *) ;


   procedure GETOPERAND ( var STE : DATUM ; var Q1 : ADRRNG ; var P1 ,
                        B1 : RGRNG ) ;

   (***************************************************************)
   (* IF PASSED AN ITEM, THIS ROUTINE RETURNS                     *)
   (* ITS <Q,B,P> ADDRESS                                         *)
   (* WARNING ON USAGE OF THIS PROCEDURE!!!                       *)
   (*                                                             *)
   (* IT IS UNSAFE TO CALL FINDRG (AND THEREFORE ALSO             *)
   (*   FINDRP, LOAD, ...) AFTER GETOPERAND AND BEFORE            *)
   (*   THE P1 REGISTER HAS BEEN  USED                            *)
   (***************************************************************)


      begin (* GETOPERAND *)
        with STE do
          if VRBL then
            if DRCT then
              if FPA . DSPLMT <> 0 then
                LOAD ( STE )
              else
                begin
                  if VPA = MEM then
                    begin
                      Q1 := MEMADR . DSPLMT ;
                      P1 := MEMADR . LVL ;
                      BASE ( Q1 , P1 , B1 ) ;
                    end (* then *)
                  else
                    if VPA = ONSTK then
                      begin
                        P1 := CURLVL ;
                        Q1 := STKADR ;
                        BASE ( Q1 , P1 , B1 ) ;
                      end (* then *)

        (*************************************)
        (* THE VPA=REG CASE NOT HANDLED HERE *)
        (*************************************)

                end (* else *)
            else

        (***********)
        (*NOT DIRCT*)
        (***********)

              begin
                GETADR ( STE , Q1 , P1 , B1 ) ;
                if VPA = RGS then
                  AVAIL [ RGADR ] := TRUE ;
              end (* else *)
          else

        (*************************************)
        (* VRBL MAY NOT HAVE ANY FUNCTION    *)
        (* ANY MORE                          *)
        (*************************************)

            begin
              if DTYPE <> ADR then
                ERROR ( 602 ) ;
              Q1 := FPA . DSPLMT ;
              P1 := FPA . LVL ;
              BASE ( Q1 , P1 , B1 ) ;
            end (* else *) ;
      end (* GETOPERAND *) ;


   procedure GETQB ( var STE : DATUM ; var Q : ADRRNG ; var P : RGRNG ;
                   L : INTEGER ) ;

   (*****************************************************)
   (* GETS BASE-DISPLACEMENT ADDRESS SUCH THAT THE      *)
   (* DISPLACEMENT < 4096-L                             *)
   (*****************************************************)


      var B : RGRNG ;

      begin (* GETQB *)
        if L < 0 then
          L := 0 ;
        GETOPERAND ( STE , Q , P , B ) ;
        if B > 0 then
          if P > 0 then
            if Q >= ( 4096 - L ) then
              begin
                GENRX ( XLA , TXRG , Q , P , B ) ;
                Q := 0 ;
                P := TXRG ;
                B := 0 ;
              end (* then *)
            else
              GENRR ( XAR , P , B )
          else
            P := B ;
        if Q >= ( 4096 - L ) then
          begin
            GENRX ( XLA , TXRG , Q , P , 0 ) ;
            P := TXRG ;
            Q := 0 ;
          end (* then *) ;
      end (* GETQB *) ;


   procedure STORE ( STP : STKPTR ; INDRCT : BOOLEAN ) ;

   (********************************************************)
   (* STORE THE STACK ELEMENT IN THE LOCATION DENOTED BY : *)
   (* IF INDRCT  THEN  2_ND TOP STACK ELEMENT              *)
   (* ELSE P_Q FIELDS OF THE CURRENT INSTRUCTION           *)
   (* ---------------------------------------------------- *)
   (********************************************************)


      var B : RGRNG ;
          P1 : RGRNG ;

      begin (* STORE *)

        (*************************************)
        (* LOADS THE ELEMENT INTO A REGISTER *)
        (*************************************)

        CLEAR_REG := STK [ STP ] . DTYPE <> OPNDTYPE ;
        if ( OPNDTYPE > CHRC ) or STK [ STP ] . VRBL then
          LOAD ( STK [ STP ] ) ;
        CLEAR_REG := TRUE ;
        P1 := P ;
        if INDRCT then
          begin
            if not STK [ STP - 1 ] . DRCT then
              LOAD ( STK [ STP - 1 ] ) ;
            GETADR ( STK [ STP - 1 ] , Q , P1 , B ) ;
            FREEREG ( STK [ STP - 1 ] ) ;
          end (* then *)
        else
          BASE ( Q , P1 , B ) ;
        with STK [ STP ] do
          begin
            if VRBL then
              if ( not DRCT ) or ( VPA = MEM ) then
                if DTYPE <> OPNDTYPE then
                  if ( DTYPE <> INT ) or ( OPNDTYPE <> HINT ) then
                    ERROR ( 601 ) ;
            case OPNDTYPE of
              ADR , INT :
                begin
                  GENRX ( XST , RGADR , Q , B , P1 ) ;
                  AVAIL [ RGADR ] := TRUE
                end (* tag/ca *) ;
              HINT : begin
                       GENRX ( XSTH , RGADR , Q , B , P1 ) ;
                       AVAIL [ RGADR ] := TRUE
                     end (* tag/ca *) ;
              BOOL , CHRC :
                if VRBL then
                  begin
                    AVAIL [ RGADR ] := TRUE ;
                    GENRX ( XSTC , RGADR , Q , B , P1 ) ;
                  end (* then *)
                else

        (**********************)
        (* STORING A CONSTANT *)
        (**********************)

                  begin
                    if ( FPA . DSPLMT < 0 ) or ( FPA . DSPLMT > 255 )
                    then
                      begin
                        ERROR ( 302 ) ;
                        FPA . DSPLMT := 0
                      end (* then *) ;
                    if B > 0 then
                      if P1 > 0 then
                        GENRR ( XAR , P1 , B )
                      else
                        P1 := B ;
                    GENSI ( XMVI , Q , P1 , FPA . DSPLMT ) ;
                  end (* else *) ;
              REEL : begin
                       GENRX ( XSTD , RGADR , Q , B , P1 ) ;
                       AVAILFP [ RGADR ] := TRUE
                     end (* tag/ca *) ;
              PSET : ERROR ( 616 ) ;
            end (* case *)
          end (* with *)
      end (* STORE *) ;


   procedure CALLSUB ;

      var K : INTEGER ;
          DSREG , P1 , B1 , FPR : RGRNG ;
          Q1 : ADRRNG ;
          PPCALL : BOOLEAN ;

      begin (* CALLSUB *)
        if ODD ( P ) then

        (**************************)
        (* SAVEFPRS FOR THIS CALL *)
        (**************************)

          begin
            P := P - 1 ;
            SAVEFPRS := TRUE ;
            FPR := 0 ;
            K := FPRSAREA ;
            repeat
              FPR := FPR + 2 ;
              K := K + REALSIZE ;
              if not AVAILFP [ FPR ] then
                GENRX ( XSTD , FPR , K , LBR , 0 ) ;
            until FPR >= FPCNT ;
          end (* then *)
        else
          SAVEFPRS := FALSE ;
        with CALSTK [ CALDPTH ] do
          if DISPSAV > 0 then

        (********************************)
        (* CALL ON PARAMETRIC PROCEDURE *)
        (********************************)

            begin
              FINDRG ;
              GENRR ( XLR , NXTRG , LBR ) ;
              DSREG := NXTRG ;
              if DISPSAV > 4095 then
                begin
                  GENRXLIT ( XA , DSREG , DISPSAV , 0 ) ;
                  DISPSAV := 0
                end (* then *) ;
              GENSS ( XMVC , DISPAREA , DISPSAV , DSREG , DISPLAY , GBR
                      ) ;
              PPCALL := TRUE ;
              Q1 := PFLEV DIV 10 ;
              P1 := PFLEV MOD 10 ;
              BASE ( Q1 , P1 , B1 ) ;
              if P1 <= 0 then
                P1 := B1
              else
                if B1 > 0 then
                  GENRR ( XAR , P1 , B1 ) ;
              GENSS ( XMVC , DISPAREA - 4 , DISPLAY + 4 , GBR , Q1 + 4
                      , P1 ) ;
              GENRX ( XL , TRG15 , Q1 , P1 , 0 ) ;

        (********************)
        (* LOAD PROC. ADDR. *)
        (********************)

            end (* then *)
          else
            PPCALL := FALSE ;
        if Q <= 4095 then
          GENRX ( XLA , TRG1 , Q , LBR , 0 )
        else
          begin
            GENRR ( XLR , TRG1 , LBR ) ;
            GENRXLIT ( XA , TRG1 , Q , 0 ) ;
          end (* else *) ;

        //******************************************************
        // generate different call sequences depending on       
        // external language; for fortran all parameters        
        // are call by reference, so dummy arguments are        
        // created for every by-value parameter ... this has    
        // been done elsewhere                                  
        //******************************************************

        case EXTLANG of
          'F' : begin
                  K := P * 2 ;

        //******************************************************
        // K = LENGTH OF PARM LIST                              
        //******************************************************

                  if K > 0 then
                    GENSI ( XMVI , K - 4 , TRG1 , 128 ) ;
                  K := ALIGN ( K , REALSIZE ) ;
                  GENRX ( XST , TRG13 , K + 4 , TRG1 , 0 ) ;

        //******************************************************
        // S/A CHAINING                                         
        // provide r13 as usable sa base addr                   
        //******************************************************

                  GENRR ( XLR , TRG14 , TRG13 ) ;
                  GENRX ( XLA , TRG13 , K , TRG1 , 0 ) ;
                  GENRX ( XST , TRG13 , 8 , TRG14 , 0 ) ;
                end (* tag/ca *) ;
          'A' : begin

        //******************************************************
        // S/A CHAINING                                         
        // provide r13 as usable sa base addr                   
        //******************************************************

                  GENRX ( XST , TRG1 , 8 , TRG13 , 0 ) ;
                  GENRX ( XST , TRG13 , 4 , TRG1 , 0 ) ;
                  GENRR ( XLR , TRG13 , TRG1 ) ;
                  GENRX ( XLA , TRG1 , 112 , TRG1 , 0 ) ;
                end (* tag/ca *) ;
          otherwise
            
        end (* case *) ;
        if not FLOW_TRACE then
          begin
            if not PPCALL then
              GENRXLAB ( XL , TRG15 , LBL2 , - 3 ) ;
            GENRR ( XBALR , TRG14 , TRG15 ) ;
          end (* then *)
        else

        (******************************)
        (* GENERATE SPECIAL CALL CODE *)
        (******************************)

          begin
            if PPCALL then
              begin
                if ODD ( PCOUNTER ) then
                  GENRR ( XBCR , NOCND , 0 ) ;

        (*****************)
        (* ALIGN TO WORD *)
        (*****************)

                GENRELRX ( XST , TRG15 , 4 ) ;

        (**************)
        (* ST 15,*+8  *)
        (**************)

                GENRELRX ( XBC , ANYCND , 4 ) ;

        (**************)
        (* B  *+8     *)
        (**************)

                CODE . I [ PCOUNTER DIV 2 ] := 0 ;
                PCOUNTER := NEXTPC ( 2 ) ;
                GENRX ( XBAL , TRG14 , TRACER , GBR , 0 ) ;
                CODE . H [ PCOUNTER ] := TO_HINT ( 2 * PCOUNTER - 8 ) ;

        (******************)
        (* DC AL2( *-8 )  *)
        (******************)

              end (* then *)
            else
              begin
                GENRX ( XBAL , TRG14 , TRACER , GBR , 0 ) ;
                UPD_PRCTBL ( PCOUNTER , LBL2 . NAM ) ;
              end (* else *) ;
            PCOUNTER := NEXTPC ( 1 ) ;
          end (* else *) ;
        if PPCALL then

        (*******************)
        (* RESTORE DISPLAY *)
        (*******************)

          begin
            GENSS ( XMVC , DISPAREA , DISPLAY , GBR , CALSTK [ CALDPTH
                    ] . DISPSAV , DSREG ) ;
            AVAIL [ DSREG ] := TRUE ;
          end (* then *) ;
        CALDPTH := CALDPTH - 1 ;

        //******************************************************
        // on return: different call seq. depending on extlang  
        //******************************************************

        case EXTLANG of
          'F' : begin
                  GENRX ( XL , TRG13 , 4 , TRG13 , 0 ) ;
                end (* tag/ca *) ;
          'A' : begin
                  GENRR ( XLR , TRG1 , TRG13 ) ;
                  GENRX ( XL , TRG13 , 4 , TRG13 , 0 ) ;
                end (* tag/ca *) ;
          otherwise
            
        end (* case *) ;
        if SAVEFPRS then
          begin
            FPR := 0 ;
            K := FPRSAREA ;
            repeat
              FPR := FPR + 2 ;
              K := K + REALSIZE ;
              if not AVAILFP [ FPR ] then
                GENRX ( XLD , FPR , K , LBR , 0 ) ;
            until FPR >= FPCNT ;
          end (* then *) ;
        CLEAR_REG := TRUE ;
        CSPREGACTIVE := FALSE ;
        OLDCSP := PSIO ;

        (***************)
        (* R1,R15 USED *)
        (***************)

      end (* CALLSUB *) ;


   procedure LOADFCBADDRESS ( STP : STKPTR ) ;

      var OPC : BYTE ;

      begin (* LOADFCBADDRESS *)
        if not FILREGACTIVE then
          if CSP in [ PRES , PREW , PGET , PPUT , PRLN , PWLN , PPAG ,
          PSKP , PLIM , PRDB , PWRB , PRDH , PRDY , PEOL , PEOT , PEOF
          , PELN , PRDC , PWRC , PRDI , PWRI , PRDS , PWRS , PRDR ,
          PWRR , PWRP , PWRX , PFDF , PWRD , PWRE , PCLS ] then
            with STK [ STP ] do
              begin
                if VRBL then
                  begin
                    OPC := XL ;
                    Q1 := MEMADR . DSPLMT ;
                    P1 := MEMADR . LVL
                  end (* then *)
                else
                  begin
                    OPC := XLA ;
                    Q1 := FPA . DSPLMT ;
                    P1 := FPA . LVL
                  end (* else *) ;
                with LAST_FILE do
                  begin
                    LFOPND . DSPLMT := Q1 ;
                    LFOPND . LVL := P1 ;
                    LFV := VRBL ;
                  end (* with *) ;
                BASE ( Q1 , P1 , B1 ) ;
                GENRX ( OPC , FILADR , Q1 , B1 , P1 ) ;
                FILREGACTIVE := TRUE ;
              end (* with *) ;
      end (* LOADFCBADDRESS *) ;


   procedure GOTOCSP ;

      var LBL_WORK : PLABEL ;

      begin (* GOTOCSP *)

        /**********************************/
        /* (RE)LOAD PROCADR, if necessary */
        /**********************************/

        if not CSPREGACTIVE then
          begin
            LBL_WORK . NAM := '$PASCSP' ;
            LBL_WORK . LEN := 7 ;
            GENRXLAB ( XL , TRG15 , LBL_WORK , - 3 ) ;
          end (* then *) ;

        /*************************************************/
        /* load new stackaddr, if necessary (if changed) */
        /*************************************************/

        if PROCOFFSET <> 0 then
          begin
            if PROCOFFSET_OLD <> PROCOFFSET then
              begin
                if PROCOFFSET <= 4095 then
                  begin
                    GENRX ( XLA , CALLSTACKADR , PROCOFFSET , 13 , 0 )
                            ;
                    AVAIL [ CALLSTACKADR ] := FALSE ;
                  end (* then *)
                else
                  begin
                    GENRR ( XLR , CALLSTACKADR , 13 ) ;
                    GENRXLIT ( XA , CALLSTACKADR , PROCOFFSET , 0 ) ;
                    AVAIL [ CALLSTACKADR ] := FALSE ;
                  end (* else *)
              end (* then *) ;
          end (* then *) ;

        /************************/
        /* proc number in reg 1 */
        /************************/

        if CSP <> OLDCSP then
          GENRX ( XLA , TRG1 , ORD ( CSP ) * 4 , 0 , 0 ) ;

        /**************************************/
        /* see if filaddress has to be loaded */
        /**************************************/

        LOADFCBADDRESS ( TOP - 1 ) ;

        /************************/
        /* branch to subroutine */
        /************************/

        GENRR ( XBALR , TRG14 , TRG15 ) ;

        /**********************************/
        /* save some values for next call */
        /**********************************/

        PROCOFFSET_OLD := PROCOFFSET ;
        CSPREGACTIVE := TRUE ;
        OLDCSP := CSP ;
        LAST_FILE . LPC := PCOUNTER ;
      end (* GOTOCSP *) ;


   procedure CALLSTNDRD ;

   (********************************)
   (* TO CALL A STANDARD PROCEDURE *)
   (* ---------------------------- *)
   (********************************)


      var Q1 , LEN : ADRRNG ;
          P1 , B1 : RGRNG ;
          OPC : BYTE ;
          ITEST : INTEGER ;
          LBL_WORK : PLABEL ;


      procedure FILESETUP ( PRMCNT : RGRNG ) ;

      (************************************************)
      (* TO SET UP PARAMETERS FOR THE FILE I/O        *)
      (* AND CALL THE I/O ROUTINE                     *)
      (* -------------------------------------------- *)
      (************************************************)


         label 10 ;

         var I : RGRNG ;
             STP : STKPTR ;
             STP1 : STKPTR ;
             STP2 : STKPTR ;
             STP3 : STKPTR ;
             CPARM3 : INTEGER ;
             TOPSTART : STKPTR ;

         begin (* FILESETUP *)
           TOPSTART := TOP ;
           if FALSE then
             begin
               WRITELN ( TRACEF , 'filesetup - csp: ' , CSPTBL [ CSP ]
                         ) ;
               DUMPSTK ( 1 , TOPSTART ) ;
             end (* then *) ;

           (********************************************)
           (* POINTING TO NEXT AVAILABLE STACK ELEMENT *)
           (********************************************)

           STP := TOP - PRMCNT + 1 ;
           STP1 := STP ;
           STP2 := STP + 1 ;
           STP3 := STP + 2 ;
           TOP := STP ;

           (*******************************)
           (* POTENTIAL REGISTER CONFLICT *)
           (*******************************)

           if PRMCNT >= 2 then
             begin
               with STK [ STP2 ] do
                 if VRBL and ( VPA = RGS ) and ( RGADR = 2 ) then
                   begin
                     FINDRG ;
                     GENRR ( XLR , NXTRG , 2 ) ;
                     AVAIL [ NXTRG ] := FALSE ;
                     AVAIL [ 2 ] := TRUE ;
                     RGADR := NXTRG ;
                   end (* then *)
             end (* then *) ;

           (*******************************)
           (* POTENTIAL REGISTER CONFLICT *)
           (*******************************)

           if PRMCNT = 3 then
             begin
               with STK [ STP3 ] do
                 if ( PROCNAME [ 1 ] <> ' ' ) and ( RGADR in [ 2 , 3 ]
                 ) then
                   begin
                     FINDRG ;
                     if NXTRG = 3 then
                       begin
                         FINDRG ;
                         AVAIL [ 3 ] := TRUE ;
                       end (* then *) ;
                     GENRR ( XLR , NXTRG , RGADR ) ;
                     AVAIL [ NXTRG ] := FALSE ;
                     AVAIL [ RGADR ] := TRUE ;
                     RGADR := NXTRG ;
                   end (* then *)
             end (* then *) ;
           if FALSE then
             begin
               WRITELN ( TRACEF , 'nach korrektur: ' , CSPTBL [ CSP ] )
                         ;
               DUMPSTK ( 1 , TOPSTART ) ;
             end (* then *) ;

           (*******************************)
           (* stack abarbeiten            *)
           (*******************************)

           CPARM3 := - 1 ;
           for I := 2 to PRMCNT + 1 do
             with STK [ STP ] do
               begin
                 if not VRBL then
                   if I = 3 then
                     CPARM3 := FPA . DSPLMT
                   else
                     if I = 4 then
                       if CPARM3 = FPA . DSPLMT then
                         begin
                           RGADR := 3 ;
                           goto 10
                         end (* then *) ;
                 LOAD ( STK [ STP ] ) ;
                 10 :
                 if DTYPE <> REEL then
                   begin

           (*******************)
           (* THE COMMON CASE *)
           (*******************)

                     if RGADR <> I then
                       if AVAIL [ I ] then
                         begin
                           GENRR ( XLR , I , RGADR ) ;
                           if RGADR > I then
                             AVAIL [ RGADR ] := TRUE ;
                           AVAIL [ I ] := FALSE ;
                           RGADR := I ;
                         end (* then *)
                       else
                         begin
                           ERROR ( 259 )
                         end (* else *)
                   end (* then *)
                 else

           (**************************)
           (* DTYPE = REEL, I.E. WRR *)
           (**************************)

                   begin
                     if RGADR <> I then
                       if AVAILFP [ I ] then
                         GENRR ( XLDR , I , RGADR )
                       else
                         begin
                           ERROR ( 259 )
                         end (* else *) ;
                     AVAILFP [ RGADR ] := TRUE ;
                     AVAIL [ I ] := FALSE ;
                     RGADR := I ;

           (*****************************************)
           (* KLUDGE TO RELEASE THE FIX. REG. LATER *)
           (*****************************************)

                   end (* else *) ;
                 STP := STP + 1 ;
               end (* with *) ;
           GOTOCSP ;
           for I := 2 to PRMCNT + 1 do
             begin
               STP := STP - 1 ;
               AVAIL [ STK [ STP ] . RGADR ] := TRUE
             end (* for *) ;
         end (* FILESETUP *) ;


      begin (* CALLSTNDRD *)
        if FALSE then
          begin
            WRITELN ( 'start callstandard csp: ' , ORD ( CSP ) : 3 ) ;
            for ITEST := 2 to 4 do
              if not AVAIL [ ITEST ] then
                begin
                  WRITELN ( 'register ' , ITEST : 3 ,
                            ' not available / line: ' , LASTLN : 6 ) ;
                end (* then *)
          end (* then *) ;
        TOP := TOP - 1 ;
        case CSP of
          PTRC : begin
                   with STK [ TOP ] do
                     begin
                       LOAD ( STK [ TOP ] ) ;
                       AVAILFP [ RGADR ] := TRUE ;
                       if RGADR <> 2 then
                         GENRR ( XLDR , 2 , RGADR )
                     end (* with *) ;
                   GOTOCSP ;
                   with STK [ TOP ] do
                     begin
                       VRBL := TRUE ;
                       DRCT := TRUE ;
                       VPA := RGS ;
                       RGADR := 2 ;
                       AVAIL [ RGADR ] := FALSE ;
                       FPA . LVL := - 1 ;
                       DTYPE := INT
                     end (* with *) ;
                   TOP := TOP + 1 ;

        /***********************************/
        /* r1 beim naechsten mal neu laden */
        /***********************************/

                   OLDCSP := PSIO ;
                 end (* tag/ca *) ;
          PRND : begin
                   with STK [ TOP ] do
                     begin
                       LOAD ( STK [ TOP ] ) ;
                       AVAILFP [ RGADR ] := TRUE ;
                       if RGADR <> 2 then
                         GENRR ( XLDR , 2 , RGADR )
                     end (* with *) ;
                   GOTOCSP ;
                   with STK [ TOP ] do
                     begin
                       VRBL := TRUE ;
                       DRCT := TRUE ;
                       VPA := RGS ;
                       RGADR := 2 ;
                       AVAIL [ RGADR ] := FALSE ;
                       FPA . LVL := - 1 ;
                       DTYPE := INT
                     end (* with *) ;
                   TOP := TOP + 1 ;

        /***********************************/
        /* r1 beim naechsten mal neu laden */
        /***********************************/

                   OLDCSP := PSIO ;
                 end (* tag/ca *) ;
          PFLR : begin
                   with STK [ TOP ] do
                     begin
                       LOAD ( STK [ TOP ] ) ;
                       AVAILFP [ RGADR ] := TRUE ;
                       if RGADR <> 2 then
                         GENRR ( XLDR , 2 , RGADR )
                     end (* with *) ;
                   GOTOCSP ;
                   with STK [ TOP ] do
                     begin
                       VRBL := TRUE ;
                       DRCT := TRUE ;
                       VPA := RGS ;
                       RGADR := 2 ;
                       AVAILFP [ RGADR ] := FALSE ;
                       FPA . LVL := - 1 ;
                       DTYPE := REEL
                     end (* with *) ;
                   TOP := TOP + 1 ;

        /***********************************/
        /* r1 beim naechsten mal neu laden */
        /***********************************/

                   OLDCSP := PSIO ;
                 end (* tag/ca *) ;
          PTIM : begin
                   GOTOCSP ;
                   TOP := TOP + 1 ;
                 end (* tag/ca *) ;
          PDAT : begin
                   GOTOCSP ;
                   TOP := TOP + 1 ;
                 end (* tag/ca *) ;
          PCLK : with STK [ TOP ] do
                   begin
                     LOAD ( STK [ TOP ] ) ;
                     GENRR ( XLR , 0 , RGADR ) ;
                     GOTOCSP ;
                     GENRR ( XLR , RGADR , 0 ) ;
                     TOP := TOP + 1 ;
                     OLDCSP := PSIO ;
                   end (* with *) ;
          PMSG : begin
                   LOAD ( STK [ TOP - 1 ] ) ;
                   with STK [ TOP - 1 ] do
                     begin
                       if RGADR <> 2 then
                         if AVAIL [ 2 ] then
                           begin
                             GENRR ( XLR , 2 , RGADR ) ;
                             AVAIL [ RGADR ] := TRUE ;
                             AVAIL [ 2 ] := FALSE ;
                             RGADR := 2 ;
                           end (* then *)
                         else
                           ERROR ( 259 ) ;

        (**************************************)
        (* ASSUMING THE CURRENT SIMPLE FORMAT *)
        (**************************************)

                     end (* with *) ;
                   LOAD ( STK [ TOP ] ) ;
                   with STK [ TOP ] do
                     begin
                       if RGADR <> 3 then
                         if AVAIL [ 3 ] then
                           begin
                             GENRR ( XLR , 3 , RGADR ) ;
                             AVAIL [ RGADR ] := TRUE ;
                             AVAIL [ 3 ] := FALSE ;
                             RGADR := 3 ;
                           end (* then *)
                         else
                           ERROR ( 259 ) ;
                     end (* with *) ;
                   GOTOCSP ;
                   OLDCSP := PSIO ;
                   AVAIL [ 2 ] := TRUE ;
                   AVAIL [ 3 ] := TRUE ;
                   TOP := TOP - 1 ;
                 end (* tag/ca *) ;
          PXIT , PTRA :
            with STK [ TOP ] do
              begin
                LOAD ( STK [ TOP ] ) ;
                AVAIL [ RGADR ] := TRUE ;
                if RGADR <> 2 then
                  GENRR ( XLR , 2 , RGADR ) ;
                GOTOCSP ;
              end (* with *) ;
          PTRP : begin
                   with STK [ TOP ] do
                     if ( not DRCT ) or ( DTYPE <> ADR ) then
                       ERROR ( 602 )
                     else
                       begin
                         GETOPERAND ( STK [ TOP ] , Q1 , P1 , B1 ) ;
                         if VRBL then
                           if VPA = MEM then
                             GENRX ( XL , 1 , Q1 , B1 , P1 )
                           else
                             begin
                               GENRR ( XLR , 1 , RGADR ) ;
                               AVAIL [ RGADR ] := TRUE
                             end (* else *)
                         else
                           GENRX ( XLA , 1 , Q1 , B1 , P1 ) ;
                       end (* else *) ;
                   TOP := TOP - 1 ;
                   with STK [ TOP ] do
                     if not DRCT then
                       ERROR ( 602 )
                     else
                       if not VRBL then
                         GENRXLIT ( XL , 0 , FPA . DSPLMT , 0 )
                       else
                         begin
                           GETOPERAND ( STK [ TOP ] , Q1 , P1 , B1 ) ;
                           if VPA = MEM then
                             GENRX ( XL , 0 , Q1 , B1 , P1 )
                           else
                             begin
                               GENRR ( XLR , 0 , RGADR ) ;
                               AVAIL [ RGADR ] := TRUE ;
                             end (* else *) ;
                         end (* else *) ;
                   LBL_WORK . NAM := '$PASTRAP' ;
                   LBL_WORK . LEN := 8 ;
                   if not FLOW_TRACE then
                     begin
                       GENRXLAB ( XL , JREG , LBL_WORK , - 3 ) ;
                       GENRR ( XBALR , RTREG , JREG ) ;
                     end (* then *)
                   else

        (*********************)
        (* SPECIAL CALL CODE *)
        (*********************)

                     begin
                       GENRX ( XBAL , TRG14 , TRACER , GBR , 0 ) ;

        (*****************************************************)
        (*  IF ASM THEN                                      *)
        (*     WRITELN(ASMOUT,' DC AL2(',                    *)
        (*             PRCTBL[0].NAME,'-=V($PASTRAP))')      *)
        (*  ELSE                                             *)
        (*****************************************************)

                       if ASM then
                         WRITELN ( ASMOUT , '## ' , ' ' : SPACEASMX ,
                                   'DC    AL2(' , PRCTBL [ 0 ] . NAME ,
                                   '-=V($PASTRAP))' ) ;
                       UPD_PRCTBL ( PCOUNTER , LBL_WORK . NAM ) ;
                       PCOUNTER := NEXTPC ( 1 ) ;
                     end (* else *) ;
                 end (* tag/ca *) ;
          PSIO : begin
                   if not AVAIL [ FILADR ] then
                     if FILECNT = 0 then
                       ERROR ( 259 ) ;
                   AVAIL [ FILADR ] := FALSE ;
                   FILREGACTIVE := FALSE ;
                   FILECNT := FILECNT + 1 ;
                   with LAST_FILE do
                     if LPC = PCOUNTER then
                       with STK [ TOP ] do
                         if VRBL then
                           FILREGACTIVE := LFV and ( LFOPND = MEMADR )
                         else
                           FILREGACTIVE := ( not LFV ) and ( LFOPND =
                                           FPA ) ;
                   OLDCSP := PSIO ;
                   TOP := TOP + 1 ;

        (*********************************************)
        (* TO CANCEL OUT PREVIOUS SUBTRACT OPERATION *)
        (*********************************************)

                 end (* tag/ca *) ;
          PEIO : begin

        (*****************************)
        (* RELEASE FILE ADR REG ETC. *)
        (*****************************)

                   FILECNT := FILECNT - 1 ;
                   if FILECNT = 0 then
                     AVAIL [ FILADR ] := TRUE ;
                   FILREGACTIVE := FALSE ;
                   OLDCSP := PEIO ;
                   LAST_FILE . LPC := PCOUNTER ;

        (************************************************)
        (* TOP := TOP-1 IS DONE AT ENTRY TO CALLSTNDRD  *)
        (************************************************)

                 end (* tag/ca *) ;
          PELN , PEOF :
            begin
              LOADFCBADDRESS ( TOP ) ;
              FINDRG ;
              GENRX ( XL , NXTRG , 0 , FILADR , 0 ) ;
              with STK [ TOP ] do
                begin
                  VRBL := TRUE ;
                  DRCT := FALSE ;
                  VPA := RGS ;
                  RGADR := NXTRG ;
                  FPA . LVL := - 1 ;
                  if CSP = PEOF then
                    FPA . DSPLMT := EOFDPLMT
                  else
                    FPA . DSPLMT := EOLDPLMT ;
                  DTYPE := BOOL
                end (* with *) ;
              TOP := TOP + 2 ;

        (********************************)
        (*TO BE CORRECTED BY PENDING EIO*)
        (********************************)

            end (* tag/ca *) ;
          PEOL , PEOT :
            begin
              FILESETUP ( 0 ) ;
              FINDRG ;
              GENRX ( XL , NXTRG , 0 , FILADR , 0 ) ;
              with STK [ TOP - 1 ] do
                begin
                  VRBL := TRUE ;
                  DRCT := FALSE ;
                  VPA := RGS ;
                  RGADR := NXTRG ;
                  FPA . LVL := - 1 ;
                  if CSP = PEOL then
                    FPA . DSPLMT := EOLDPLMT
                  else
                    FPA . DSPLMT := EOFDPLMT ;
                  DTYPE := BOOL ;
                end (* with *) ;
              TOP := TOP + 1 ;
            end (* tag/ca *) ;
          PRDD : begin
                   TOP := TOP - 2 ;
                   CSP := PGET ;
                   FILESETUP ( 0 ) ;
                   GETADR ( STK [ TOP ] , Q1 , P1 , B1 ) ;
                   if not STK [ TOP ] . DRCT then
                     begin
                       GENRX ( XL , TXRG , Q1 , B1 , P1 ) ;
                       Q1 := 0 ;
                       B1 := 0 ;
                       P1 := TXRG
                     end (* then *) ;
                   FREEREG ( STK [ TOP ] ) ;
                   LEN := STK [ TOP + 1 ] . FPA . DSPLMT ;
                   if LEN <= 256 then
                     begin
                       if B1 > 0 then
                         if P1 > 0 then
                           GENRR ( XAR , P1 , B1 )
                         else
                           P1 := B1 ;
                       GENSS ( XMVC , LEN , Q1 , P1 , FILHDRSZ , FILADR
                               ) ;
                     end (* then *)
                   else
                     begin
                       FINDRP ;
                       GENRX ( XLA , NXTRG , Q1 , B1 , P1 ) ;
                       GENRXLIT ( XL , NXTRG + 1 , LEN , 0 ) ;
                       P1 := NXTRG ;
                       FINDRP ;
                       GENRX ( XLA , NXTRG , FILHDRSZ , FILADR , 0 ) ;
                       GENRR ( XLR , NXTRG + 1 , P1 + 1 ) ;
                       GENRR ( XMVCL , P1 , NXTRG ) ;
                       S370CNT := S370CNT + 1 ;
                       AVAIL [ P1 ] := TRUE ;
                       AVAIL [ NXTRG ] := TRUE ;
                       AVAIL [ P1 + 1 ] := TRUE ;
                       AVAIL [ NXTRG + 1 ] := TRUE ;
                     end (* else *)
                 end (* tag/ca *) ;
          PWRD , PWRE :
            begin
              LEN := STK [ TOP ] . FPA . DSPLMT ;
              LOADFCBADDRESS ( TOP - 2 ) ;
              with STK [ TOP - 1 ] do
                if CSP = PWRE then
                  begin
                    LOAD ( STK [ TOP - 1 ] ) ;
                    if DTYPE = REEL then
                      begin
                        OPC := XSTD ;
                        AVAILFP [ RGADR ] := TRUE
                      end (* then *)
                    else
                      begin
                        AVAIL [ RGADR ] := TRUE ;
                        if LEN = 2 then
                          OPC := XSTH
                        else
                          if LEN = 1 then
                            OPC := XSTC
                          else
                            OPC := XST ;
                      end (* else *) ;
                    GENRX ( OPC , RGADR , FILHDRSZ , FILADR , 0 ) ;
                  end (* then *)
                else
                  begin

        (**************)
        (* CSP = PWRD *)
        (**************)

                    GETADR ( STK [ TOP - 1 ] , Q1 , P1 , B1 ) ;
                    if not DRCT then
                      begin
                        GENRX ( XL , TXRG , Q1 , B1 , P1 ) ;
                        Q1 := 0 ;
                        B1 := 0 ;
                        P1 := TXRG
                      end (* then *) ;
                    FREEREG ( STK [ TOP - 1 ] ) ;
                    if LEN <= 256 then
                      begin
                        if B1 > 0 then
                          if P1 > 0 then
                            GENRR ( XAR , P1 , B1 )
                          else
                            P1 := B1 ;
                        GENSS ( XMVC , LEN , FILHDRSZ , FILADR , Q1 ,
                                P1 ) ;
                      end (* then *)
                    else
                      begin
                        FINDRP ;
                        GENRX ( XLA , NXTRG , Q1 , B1 , P1 ) ;
                        GENRXLIT ( XL , NXTRG + 1 , LEN , 0 ) ;
                        P1 := NXTRG ;
                        FINDRP ;
                        GENRX ( XLA , NXTRG , FILHDRSZ , FILADR , 0 ) ;
                        GENRR ( XLR , NXTRG + 1 , P1 + 1 ) ;
                        GENRR ( XMVCL , NXTRG , P1 ) ;
                        S370CNT := S370CNT + 1 ;
                        AVAIL [ P1 ] := TRUE ;
                        AVAIL [ P1 + 1 ] := TRUE ;
                        AVAIL [ NXTRG ] := TRUE ;
                        AVAIL [ NXTRG + 1 ] := TRUE ;
                      end (* else *) ;
                  end (* else *) ;
              TOP := TOP - 2 ;
              CSP := PPUT ;
              FILESETUP ( 0 ) ;
            end (* tag/ca *) ;
          PGET , PPUT , PRLN , PWLN , PRES , PREW , PPAG , PCLS :
            FILESETUP ( 0 ) ;
          PRDC , PRDI , PRDR , PSKP , PLIM , PRDB , PRDH , PRDY :
            FILESETUP ( 1 ) ;
          PRDS , PWRC , PWRI , PWRB , PWRP , PFDF :
            FILESETUP ( 2 ) ;
          PWRS , PWRR , PWRX :
            begin
              FILESETUP ( 3 ) ;
            end (* tag/ca *) ;
          PWRV : begin
                   FILESETUP ( 2 ) ;
                 end (* tag/ca *) ;
          otherwise
            begin
              WRITE ( ' -->' , NMCDE ) ;
              ERROR ( 607 )
            end (* otherw *) ;
        end (* case *) ;
        if FALSE then
          begin
            WRITELN ( 'ende callstandard csp: ' , ORD ( CSP ) : 3 ) ;
            for ITEST := 2 to 4 do
              if not AVAIL [ ITEST ] then
                begin
                  WRITELN ( 'register ' , ITEST : 3 ,
                            ' not available / line: ' , LASTLN : 6 ) ;
                end (* then *)
          end (* then *) ;
      end (* CALLSTNDRD *) ;


   procedure CHKOPERATION ;

      var RTA : ADRRNG ;
          BPC : ICRNG ;

      begin (* CHKOPERATION *)
        with STK [ TOP - 1 ] do
          if VRBL then

        (************************************)
        (* GENERATE CODE FOR RUN TIME CHECK *)
        (************************************)

            if OPNDTYPE = ADR then

        (*****************)
        (* POINTER CHECK *)
        (*****************)

              begin
                if not AVAIL [ 2 ] then
                  if not ( ( VPA = RGS ) and ( RGADR = 2 ) ) then
                    begin
                      J := 0 ;

        (***************)
        (* CLEAR GPR 2 *)
        (***************)

                      for I := TOP - 2 DOWNTO 1 do
                        with STK [ I ] do
                          if VRBL then
                            if ( not DRCT ) or ( DTYPE <> REEL ) then
                              if ( VPA = RGS ) and ( RGADR = 2 ) then
                                J := I ;
                      if J = 0 then
                        ERROR ( 259 )
                      else
                        with STK [ J ] do
                          begin
                            FINDRG ;

        (******************************)
        (* TRADE GPR2 FOR ANOTHER ONE *)
        (******************************)

                            GENRR ( XLR , NXTRG , 2 ) ;

        (********************)
        (* THIS FREES REG 2 *)
        (********************)

                            RGADR := NXTRG ;
                          end (* with *) ;
                    end (* then *) ;
                AVAIL [ 2 ] := TRUE ;

        (***********)
        (* IN CASE *)
        (***********)

                LOAD ( STK [ TOP - 1 ] ) ;
                AVAIL [ 2 ] := FALSE ;
                if RGADR <> 2 then

        (**************************)
        (* VALUE IS IN WRONG REG. *)
        (**************************)

                  begin
                    AVAIL [ RGADR ] := TRUE ;
                    GENRR ( XLR , 2 , RGADR ) ;
                    RGADR := 2
                  end (* then *) ;
                RTA := PTRCHK ;
                if P < 0 then
                  RTA := PTACHK ;
                GENRX ( XBAL , RTREG , RTA , GBR , 0 ) ;
                CSPREGACTIVE := FALSE ;
                OLDCSP := PSIO ;

        (********************)
        (* R1,R15 DESTROYED *)
        (********************)

              end (* then *)
            else

        (*******************)
        (* OPNDTYPE <> ADR *)
        (*******************)

              begin
                if not DRCT then
                  LOAD ( STK [ TOP - 1 ] ) ;
                FPA . DSPLMT := FPA . DSPLMT - P ;
                LOAD ( STK [ TOP - 1 ] ) ;

        (*******************************)
        (* later literal will be built *)
        (* with two margin values      *)
        (*******************************)

                I_S_R . I1 := Q - P ;
                I_S_R . I2 := P ;

        (*********************************************)
        (* address of CL = first part of literal     *)
        (*********************************************)

                GENRX_2 ( XCL , RGADR , 0 , 0 , 0 , 99 ) ;
                if ASM then
                  begin
                    WRITELN ( ASMOUT , '=A(' , Q - P : 1 , ',' , P : 1
                              , ')' )
                  end (* then *) ;
                UPD_DBLTBL ( PCOUNTER - 1 , I_S_R . R ) ;

        (*********************************************)
        (* address of branch will be filled in later *)
        (*********************************************)

                GENRX ( XBC , LEQCND , 0 , 0 , 0 ) ;
                BPC := PCOUNTER ;
                if ASM then
                  WRITELN ( ASMOUT , '## ' , ' ' : SPACEASMX ,
                            'BNH   @OK' ) ;

        (***************************************)
        (* REMEMBER WHERE BRANCH FIX-UP NEEDED *)
        (***************************************)

                if RGADR <> 2 then
                  GENRR ( XLR , 2 , RGADR ) ;
                if OPNDTYPE = PROC then
                  RTA := PRMCHK
                else
                  if OPNDTYPE = INX then
                    RTA := INXCHK
                  else

        (***************)
        (* HINT OR INT *)
        (***************)

                    RTA := RNGCHK ;
                GENRX ( XBAL , RTREG , RTA , GBR , 0 ) ;

        (*********************************************)
        (* subprogram needs both parts of literal    *)
        (* for error message; put reg + displ        *)
        (* after call                                *)
        (*********************************************)

                UPD_DBLTBL ( PCOUNTER , I_S_R . R ) ;
                if ASM then
                  begin
                    HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                    WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
                    WRITE ( ASMOUT , ' ' : COLASMI , ' ' : SPACEASMI )
                            ;
                    WRITELN ( ASMOUT , '=A(' , Q - P : 1 , ',' , P : 1
                              , ')' )
                  end (* then *) ;
                PCOUNTER := NEXTPC ( 1 ) ;
                FPA . DSPLMT := P ;
                CODE . H [ BPC - 1 ] := TO_HINT ( BASE_DSPLMT (
                                        PCOUNTER ) ) ;
                if ASM then
                  WRITELN ( ASMOUT , '## ' , ' ' : SPACEASML ,
                            '@OK    DS    0H' ) ;
              end (* else *)
          else

        (********************************************)
        (* ^ VAR,  I.E. CHECK A CONSTANT EXPRESSION *)
        (********************************************)

            if ( FPA . DSPLMT < P ) or ( FPA . DSPLMT > Q ) then
              begin
                ERROR ( 302 ) ;
                WRITELN ( OUTPUT , '****' : 7 , FPA . DSPLMT : 9 ,
                          ' IS NOT IN THE RANGE:' , P : 9 , Q : 10 ) ;
              end (* then *) ;
      end (* CHKOPERATION *) ;


   procedure FORCESTK ( var STE : DATUM ) ;

   (**************************************)
   (* FORCES A SET INTO RUN-STACK MEMORY *)
   (**************************************)


      var Q1 , Q2 : ADRRNG ;
          P1 , P2 , B1 , R : RGRNG ;

      begin (* FORCESTK *)
        with STE do
          if not ( DRCT and VRBL and ( VPA = ONSTK ) ) then
            if DRCT and VRBL and ( VPA = RGS ) then
              begin
                R := RGADR ;
                VPA := ONSTK ;
                if PLEN = 8 then
                  begin
                    GETQB ( STE , Q1 , P1 , 0 ) ;
                    GENRS ( XSTM , R , R + 1 , Q1 , P1 ) ;
                    AVAIL [ R + 1 ] := TRUE ;
                  end (* then *)
                else

        (************)
        (* PLEN = 4 *)
        (************)

                  begin
                    GETOPERAND ( STE , Q1 , P1 , B1 ) ;
                    GENRX ( XST , R , Q1 , P1 , B1 ) ;
                  end (* else *) ;
                AVAIL [ R ] := TRUE ;
              end (* then *)
            else
              if DRCT and not VRBL then
                begin

        (*************************************)
        (* TRANSFER A CONSTANT ONTO RUNSTACK *)
        (*************************************)

                  VPA := ONSTK ;
                  VRBL := TRUE ;
                  GETQB ( STE , Q1 , P1 , 0 ) ;
                  GENSSLIT ( XMVC , PLEN , Q1 , P1 , PCNST -> ) ;
                end (* then *)
              else

        (******************************)
        (* SET IS SOMEWHERE IN MEMORY *)
        (******************************)

                begin
                  GETQB ( STE , Q2 , P2 , 0 ) ;
                  TXRG := TRG1 ;
                  DRCT := TRUE ;
                  VRBL := TRUE ;
                  VPA := ONSTK ;
                  GETQB ( STE , Q1 , P1 , 0 ) ;
                  TXRG := TRG14 ;
                  GENSS ( XMVC , PLEN , Q1 , P1 , Q2 , P2 ) ;
                end (* else *) ;
        OLDCSP := PSIO ;

        (**************************)
        (* INDICATE LOSS OF REG 1 *)
        (**************************)

      end (* FORCESTK *) ;


   procedure BSETOPS ;

   (*************************)
   (* BINARY SET OPERATIONS *)
   (*************************)


      var L , R : DATUM ;
          Q1 , Q2 : ADRRNG ;
          P1 , P2 , B1 , B2 : RGRNG ;
          I , J , K , STKADRX : INTEGER ;
          MIN , LEN : PLNRNG ;
          LR : BOOLEAN ;
          OP : BYTE ;
          RNG : array [ 1 .. 6 ] of ADRRNG ;


      procedure MINCONSTSET ;

         label 10 ;

         var I , J : INTEGER ;

         begin (* MINCONSTSET *)
           J := MXSETINX * 8 ;
           if L . PLEN > 0 then
             for I := MXSETINX DOWNTO 1 do
               begin
                 I_S_R . S := L . PCNST -> . S [ I ] ;
                 if I_S_R . I2 = 0 then
                   J := J - 4
                 else
                   goto 10 ;
                 if I_S_R . I1 = 0 then
                   J := J - 4
                 else
                   goto 10 ;
               end (* for *)
           else
             J := 0 ;
           10 :
           L . PLEN := J ;
           if J = 0 then
             L . PCNST := NIL ;
         end (* MINCONSTSET *) ;


      procedure COMPACT ( var S : LARGE_SET ; var LEN : PLNRNG ; var
                        OFFSET : INTEGER ; TAG : CHAR ) ;

         var S_C : record
                     case INTEGER of
                       1 :
                         ( S : LARGE_SET ) ;
                       2 :
                         ( C : array [ 1 .. MXPLNGTH ] of CHAR ) ;
                   end ;
             I : PLNRNG ;

         begin (* COMPACT *)
           S_C . S := S ;
           while ( LEN > 0 ) and ( S_C . C [ LEN ] = TAG ) do
             LEN := LEN - 1 ;
           OFFSET := 0 ;
           while ( S_C . C [ OFFSET + 1 ] = TAG ) and ( OFFSET < LEN )
           do
             OFFSET := OFFSET + 1 ;
           if OFFSET > 0 then
             begin
               LEN := LEN - OFFSET ;
               for I := 1 to LEN do
                 S_C . C [ I ] := S_C . C [ I + OFFSET ] ;
               for I := LEN + 1 to MXPLNGTH do
                 S_C . C [ I ] := TAG ;
               S := S_C . S ;
             end (* then *) ;
         end (* COMPACT *) ;


      procedure INN_OP ;

         label 10 ;

         var LN , TOO_MANY : BOOLEAN ;
             I , J , K : INTEGER ;

         begin (* INN_OP *)
           if L . DTYPE <> INT then
             if L . DTYPE <> HINT then
               ERROR ( 601 ) ;
           if R . DTYPE <> PSET then
             ERROR ( 615 ) ;
           if not L . VRBL then
             if ( R . PLEN * 8 <= L . FPA . DSPLMT ) or ( L . FPA .
             DSPLMT < 0 ) then
               begin
                 L . FPA . LVL := 0 ;
                 L . FPA . DSPLMT := 0 ;
               end (* then *)
             else
               if not R . VRBL then
                 begin

           (*******************************)
           (* BOTH OPERANDS ARE CONSTANTS *)
           (*******************************)

                   I := L . FPA . DSPLMT MOD 64 ;
                   J := L . FPA . DSPLMT DIV 64 ;
                   L . FPA . DSPLMT := ORD ( I in R . PCNST -> . S [ J
                                       + 1 ] ) ;
                 end (* then *)
               else
                 if not ( R . DRCT and ( R . VPA = RGS ) ) then
                   begin

           (********************************************)
           (* LEFT OPND IS CONST, RIGHT OPND IN MEMORY *)
           (********************************************)

                     P1 := L . FPA . DSPLMT MOD 8 ;
                     Q1 := L . FPA . DSPLMT DIV 8 ;
                     GETQB ( R , Q2 , P2 , Q1 ) ;
                     J := 1 ;
                     for I := 6 DOWNTO P1 do
                       J := J * 2 ;
                     GENSI ( XTM , Q2 + Q1 , P2 , J ) ;
                     BRCND := TRUCND ;
                     L . VRBL := TRUE ;
                   end (* then *)
                 else
                   begin

           (******************************************)
           (* LEFT OPND IS CONST, RIGHT OPND IN REGS *)
           (******************************************)

                     if R . PLEN > 4 then
                       GENRS ( XSLDL , R . RGADR , 0 , L . FPA . DSPLMT
                               , 0 )
                     else
                       GENRS ( XSLL , R . RGADR , 0 , L . FPA . DSPLMT
                               , 0 ) ;
                     GENRR ( XLTR , R . RGADR , R . RGADR ) ;
                     BRCND := LESCND ;
                     L . VRBL := TRUE ;
                   end (* else *)
           else

           (**********)
           (* L.VRBL *)
           (**********)

             if R . PLEN <= 0 then
               begin
                 FREEREG ( L ) ;
                 L . VRBL := FALSE ;
                 L . FPA . LVL := 0 ;
                 L . FPA . DSPLMT := 0 ;
               end (* then *)
             else

           (**************)
           (* R.PLEN > 0 *)
           (**************)

               if not R . VRBL then
                 begin

           (********************************)
           (* TRY FOR BETTER CODE SEQUENCE *)
           (********************************)

                   if not L . DRCT then
                     LOAD ( L ) ;
                   K := R . PLEN * 8 ;
                   J := 0 ;
                   LN := TRUE ;
                   TOO_MANY := FALSE ;
                   for I := 0 to K do
                     if ( ( I MOD 64 ) in R . PCNST -> . S [ I DIV 64 +
                     1 ] ) and ( I < K ) then
                       if LN then
                         begin
                           J := J + 1 ;
                           if J > 6 then
                             begin
                               J := 5 ;
                               TOO_MANY := TRUE
                             end (* then *) ;
                           RNG [ J ] := I ;
                           LN := FALSE ;
                         end (* then *)
                       else
                         
                     else
                       if not LN then
                         begin
                           J := J + 1 ;
                           RNG [ J ] := I - 1 ;
                           LN := TRUE ;
                         end (* then *) ;
                   if J > 2 then
                     if ( ( RNG [ J ] - RNG [ 1 ] ) <= 50 ) or TOO_MANY
                     then
                       begin
                         COMPACT ( R . PCNST -> , R . PLEN , I , CHR (
                                   0 ) ) ;
                         L . FPA . DSPLMT := L . FPA . DSPLMT - I * 8 ;
                         goto 10
                       end (* then *) ;
                   K := RNG [ 1 ] ;
                   L . FPA . DSPLMT := L . FPA . DSPLMT - K ;
                   LOAD ( L ) ;
                   I := 1 ;
                   while I < J do
                     begin
                       if RNG [ I ] > K then
                         GENRXLIT ( XSH , L . RGADR , RNG [ I ] - K , -
                                    1 ) ;
                       GENRXLIT ( XCL , L . RGADR , RNG [ I + 1 ] - RNG
                                  [ I ] , 0 ) ;
                       K := RNG [ I ] ;
                       RNG [ I ] := PCOUNTER + 1 ;
                       I := I + 2 ;
                       if I < J then
                         begin
                           if ASM then
                             WRITELN ( ASMOUT , '## ' , ' ' : SPACEASMX
                                       , 'BNH    T' , TESTCNT + 1 : 1 )
                                       ;
                           GENRX ( XBC , LEQCND , 0 , 0 , 0 )
                         end (* then *) ;
                     end (* while *) ;
                   if ASM then
                     begin
                       TESTCNT := TESTCNT + 1 ;
                       WRITELN ( ASMOUT , '## ' , ' ' : SPACEASML , 'T'
                                 , TESTCNT : 1 , '     DS    0H' ) ;
                     end (* then *) ;
                   begin
                     K := BASE_DSPLMT ( PCOUNTER ) ;
                     while I > 2 do
                       begin
                         I := I - 2 ;
                         CODE . H [ RNG [ I ] ] := TO_HINT ( K ) ;
                       end (* while *) ;
                   end ;
                   BRCND := LEQCND ;
                 end (* then *)
               else
                 10 :
                 begin

           (***************************************)
           (* R.VRBL OR UNOPTIMIZED CASE OF ABOVE *)
           (***************************************)

                   LOAD ( L ) ;
                   if R . PLEN <= 8 then
                     begin

           (*********************************)
           (* OPERATE ON RIGHT OPND IN REGS *)
           (*********************************)

                       LOAD ( R ) ;
                       GENRX ( XLA , 0 , R . PLEN * 8 , 0 , 0 ) ;
                       GENRR ( XCLR , L . RGADR , 0 ) ;
                       GENRELRX ( XBC , GEQCND , 5 ) ;

           (************)
           (* BNL *+10 *)
           (************)

                       if R . PLEN > 4 then
                         begin
                           AVAIL [ R . RGADR + 1 ] := TRUE ;
                           GENRS ( XSLDL , R . RGADR , 0 , 0 , L .
                                   RGADR ) ;
                         end (* then *)
                       else
                         GENRS ( XSLL , R . RGADR , 0 , 0 , L . RGADR )
                                 ;
                       GENRR ( XLTR , R . RGADR , R . RGADR ) ;
                       BRCND := LESCND ;
                     end (* then *)
                   else
                     begin

           (***************************)
           (* RIGHT OPERAND IN MEMORY *)
           (***************************)

                       if R . VRBL then
                         GETQB ( R , Q2 , P2 , 0 )
                       else
                         begin
                           P2 := 0 ;
                           Q2 := 0
                         end (* else *) ;
                       GENRXLIT ( XCL , L . RGADR , R . PLEN * 8 - 1 ,
                                  0 ) ;
                       if ASM then
                         begin
                           TESTCNT := TESTCNT + 1 ;
                           WRITELN ( ASMOUT , '## ' , ' ' : SPACEASMX ,
                                     'BH    T' , TESTCNT : 1 ) ;
                         end (* then *) ;
                       GENRELRX ( XBC , GRTCND , 12 ) ;

           (***********)
           (* BH *+24 *)
           (***********)

                       GENRX ( XLA , TRG1 , 7 , 0 , 0 ) ;
                       GENRR ( XNR , TRG1 , L . RGADR ) ;
                       GENRS ( XSRA , L . RGADR , 0 , 3 , 0 ) ;
                       if R . VRBL then
                         GENRX ( XIC , L . RGADR , Q2 , L . RGADR , P2
                                 )
                       else
                         begin
                           if ASM then
                             begin
                               WRITE ( ASMOUT , '## ' , ' ' : SPACEASMX
                                       , '<constant> ' ) ;
                               PRINT_SET ( R . PCNST -> , R . PLEN ) ;
                             end (* then *) ;
                           GENRX_2 ( XIC , L . RGADR , Q2 , L . RGADR ,
                                     P2 , 3 ) ;
                           UPD_SETTBL ( PCOUNTER - 1 , R . PCNST -> , R
                                        . PLEN ) ;
                         end (* else *) ;
                       GENRS ( XSLL , L . RGADR , 0 , 24 , TRG1 ) ;
                       GENRR ( XLTR , L . RGADR , L . RGADR ) ;
                       if ASM then
                         WRITELN ( ASMOUT , '## ' , ' ' : SPACEASML ,
                                   'T' , TESTCNT : 1 , '     DS    0H'
                                   ) ;
                       BRCND := LESCND ;
                     end (* else *) ;
                 end ;

           (**********)
           (* L.VRBL *)
           (**********)

           FREEREG ( L ) ;
           FREEREG ( R ) ;
           L . DTYPE := BOOL ;
         end (* INN_OP *) ;


      procedure ASE_OP ;

         begin (* ASE_OP *)
           if Q < 0 then

           (*********************************)
           (* OPERANDS ARE IN REVERSE ORDER *)
           (*********************************)

             begin
               L := STK [ TOP ] ;
               R := STK [ TOP - 1 ] ;
               Q := - Q
             end (* then *) ;
           if L . DTYPE <> PSET then
             ERROR ( 615 ) ;
           if R . DTYPE <> INT then
             if R . DTYPE <> HINT then
               ERROR ( 602 ) ;
           LOAD ( R ) ;
           if DEBUG then
             begin

           (**********************************)
           (* CHECK THAT ELEMENT IS IN RANGE *)
           (**********************************)

               GENRR ( XBALR , TRG14 , 0 ) ;
               GENRX ( XLA , TRG1 , L . PLEN * 8 - 1 , 0 , 0 ) ;
               GENRR ( XCLR , R . RGADR , TRG1 ) ;
               GENRX ( XBC , GRTCND , SETCHK , GBR , 0 ) ;
             end (* then *) ;
           if L . PLEN <= 8 then
             begin

           (******************************)
           (* PRODUCE THE RESULT IN REGS *)
           (******************************)

               LOAD ( L ) ;
               GENRX ( XLA , TRG1 , 1 , 0 , 0 ) ;
               GENRR ( XLCR , R . RGADR , R . RGADR ) ;
               if L . PLEN > 4 then
                 begin
                   GENRR ( XSR , TRG0 , TRG0 ) ;
                   GENRS ( XSLDL , TRG0 , 0 , 63 , R . RGADR ) ;
                   GENRR ( XORX , L . RGADR , TRG0 ) ;
                   GENRR ( XORX , L . RGADR + 1 , TRG1 ) ;
                 end (* then *)
               else
                 begin
                   GENRS ( XSLL , TRG1 , 0 , 31 , R . RGADR ) ;
                   GENRR ( XORX , L . RGADR , TRG1 ) ;
                 end (* else *) ;
             end (* then *)
           else
             begin

           (****************************)
           (* OPERATE ON SET IN MEMORY *)
           (****************************)

               FORCESTK ( L ) ;
               GETQB ( L , Q1 , P1 , 0 ) ;
               GENRX ( XLA , TRG15 , 7 , 0 , 0 ) ;
               GENRR ( XNR , TRG15 , R . RGADR ) ;
               GENRS ( XSRL , R . RGADR , 0 , 3 , 0 ) ;
               GENRX ( XLA , TRG1 , Q1 , P1 , R . RGADR ) ;
               GENRX ( XLA , R . RGADR , 128 , 0 , 0 ) ;
               GENRS ( XSRL , R . RGADR , 0 , 0 , TRG15 ) ;
               GENRXLIT ( XEX , R . RGADR , - 1778380800 , 0 ) ;

           (*************)
           (* OI 0(1),0 *)
           (*************)

               CSPREGACTIVE := FALSE ;

           (***************************)
           (* INDICATE LOSS OF REG 15 *)
           (***************************)

             end (* else *) ;
           AVAIL [ R . RGADR ] := TRUE ;
         end (* ASE_OP *) ;


      begin (* BSETOPS *)
        L := STK [ TOP - 1 ] ;
        R := STK [ TOP ] ;
        if FALSE then
          begin
            WRITELN ( TRACEF , 'pcode  = ' , PCODE ) ;
            WRITELN ( TRACEF , 'l.plen = ' , L . PLEN ) ;
            WRITELN ( TRACEF , 'r.plen = ' , R . PLEN )
          end (* then *) ;
        case PCODE of

        /************************************************************/
        /* UNI implementieren                                       */
        /************************************************************/

          PUNI : begin
                   if L . DTYPE <> PSET then
                     ERROR ( 615 ) ;
                   if R . DTYPE <> PSET then
                     ERROR ( 615 ) ;
                   STKADRX := L . STKADR ;

        (****************************************)
        (* len = maximum length of the operands *)
        (* ok on union                          *)
        (****************************************)

                   LEN := L . PLEN ;
                   if LEN < R . PLEN then
                     LEN := R . PLEN ;

        (********************************************)
        (* ONE time loop - using break to terminate *)
        (********************************************)

                   repeat

        (******************************************************)
        (* the right operand is null                          *)
        (* nothing to do                                      *)
        (******************************************************)

                     if R . PLEN <= 0 then
                       break ;

        (******************************************************)
        (* the left operand is null                           *)
        (* replace the left operand with the right operand    *)
        (******************************************************)

                     if L . PLEN <= 0 then
                       begin
                         if ( R . STKADR <> STKADRX ) and R . VRBL and
                         R . DRCT and ( R . VPA = ONSTK ) then
                           begin
                             L . VRBL := TRUE ;
                             L . DRCT := TRUE ;
                             L . VPA := ONSTK ;
                             GETQB ( L , Q1 , P1 , 0 ) ;
                             TXRG := TRG1 ;
                             GETQB ( R , Q2 , P2 , 0 ) ;
                             TXRG := TRG14 ;
                             GENSS ( XMVC , LEN , Q1 , P1 , Q2 , P2 ) ;
                           end (* then *)
                         else
                           L := R ;
                         break
                       end (* then *) ;

        (******************************)
        (* BOTH OPERANDS ARE NOT NULL *)
        (******************************)

                     if not L . VRBL and not R . VRBL then

        (**********************************************************)
        (* both operands are constants, operation at compile time *)
        (**********************************************************)

                       begin
                         for I := 1 to MXSETINX do
                           L . PCNST -> . S [ I ] := L . PCNST -> . S [
                                                   I ] + R . PCNST -> .
                                                   S [ I ] ;
                         MINCONSTSET ;
                         break ;
                       end (* then *) ;

        (*****************************************)
        (* one of the operands is not a constant *)
        (*****************************************)

                     if LEN <= 8 then

        (*******************************************)
        (* len <= 8 - generate result in registers *)
        (*******************************************)

                       begin
                         LR := TRUE ;
                         if L . PLEN < R . PLEN then
                           begin
                             LOAD ( R ) ;
                             LR := FALSE
                           end (* then *)
                         else
                           if L . PLEN > R . PLEN then
                             LOAD ( L )
                           else

        (*************************)
        (* EQUAL LENGTH OPERANDS *)
        (*************************)

                             if not ( L . VRBL and L . DRCT and ( L .
                             VPA = RGS ) ) then
                               if R . VRBL and R . DRCT and ( R . VPA =
                               RGS ) then
                                 LR := FALSE
                               else
                                 LOAD ( L ) ;
                         if not LR then

        (************************)
        (* INTERCHANGE OPERANDS *)
        (************************)

                           begin
                             L := R ;
                             R := STK [ TOP - 1 ]
                           end (* then *) ;
                         if R . VRBL then
                           if R . DRCT and ( R . VPA = RGS ) then
                             begin

        (******************************)
        (* BOTH OPERANDS IN REGISTERS *)
        (******************************)

                               GENRR ( XORX , L . RGADR , R . RGADR ) ;
                               AVAIL [ R . RGADR ] := TRUE ;
                               if R . PLEN > 4 then
                                 begin
                                   GENRR ( XORX , L . RGADR + 1 , R .
                                           RGADR + 1 ) ;
                                   AVAIL [ R . RGADR + 1 ] := TRUE
                                 end (* then *)
                             end (* then *)
                           else

        (*******************************************)
        (* LEFT OPND IN REGS, RIGHT OPND IN MEMORY *)
        (*******************************************)

                             begin
                               GETOPERAND ( R , Q2 , P2 , B2 ) ;
                               GENRX ( XO , L . RGADR , Q2 , P2 , B2 )
                                       ;
                               if R . PLEN > 4 then
                                 begin
                                   CHECKDISP ( Q2 , P2 , B2 ) ;
                                   GENRX ( XO , L . RGADR + 1 , Q2 + 4
                                           , P2 , B2 ) ;
                                 end (* then *)
                             end (* else *)
                         else

        (******************************************)
        (* LEFT OPND IN REGS, RIGHT OPND IS CONST *)
        (******************************************)

                           begin
                             I_S_R . S := R . PCNST -> . S [ 1 ] ;
                             if I_S_R . I1 <> 0 then
                               GENRXLIT ( XO , L . RGADR , I_S_R . I1 ,
                                          0 ) ;
                             if R . PLEN > 4 then
                               if I_S_R . I2 <> 0 then
                                 GENRXLIT ( XO , L . RGADR + 1 , I_S_R
                                            . I2 , 0 )
                           end (* else *) ;
                         break ;
                       end (* then *) ;

        (*****************************************)
        (* len > 8 - most complicated situation  *)
        (*****************************************)

                     FORCESTK ( L ) ;
                     if R . VRBL then
                       if R . DRCT and ( R . VPA = RGS ) then
                         begin
                           GETQB ( L , Q1 , P1 , 4 ) ;
                           GENRX ( XO , R . RGADR , Q1 , P1 , 0 ) ;
                           AVAIL [ R . RGADR ] := TRUE ;
                           if R . PLEN > 4 then
                             begin
                               GENRX ( XO , R . RGADR + 1 , Q1 + 4 , P1
                                       , 0 ) ;
                               GENRS ( XSTM , R . RGADR , R . RGADR + 1
                                       , Q1 , P1 ) ;
                               AVAIL [ R . RGADR + 1 ] := TRUE
                             end (* then *)
                           else
                             GENRX ( XST , R . RGADR , Q1 , P1 , 0 )
                         end (* then *)
                       else

        (***************************)
        (* BOTH OPERANDS IN MEMORY *)
        (***************************)

                         begin
                           MIN := L . PLEN ;
                           if MIN > R . PLEN then
                             MIN := R . PLEN ;
                           GETQB ( L , Q1 , P1 , MIN ) ;
                           TXRG := TRG1 ;
                           GETQB ( R , Q2 , P2 , MIN ) ;
                           TXRG := TRG14 ;
                           GENSS ( XOC , MIN , Q1 , P1 , Q2 , P2 ) ;
                           if R . PLEN > L . PLEN then
                             GENSS ( XMVC , R . PLEN - L . PLEN , Q1 +
                                     MIN , P1 , Q2 + MIN , P2 )
                         end (* else *)
                     else

        (*****************************************)
        (* LEFT OPND IN MEM, RIGHT OPND IS CONST *)
        (*****************************************)

                       begin
                         PSVAL := R . PCNST -> ;
                         MIN := L . PLEN ;
                         if MIN > R . PLEN then
                           MIN := R . PLEN ;
                         COMPACT ( R . PCNST -> , MIN , J , CHR ( 0 ) )
                                   ;
                         GETQB ( L , Q1 , P1 , MIN ) ;
                         if MIN >= 0 then
                           GENSSLIT ( XOC , MIN , Q1 + J , P1 , R .
                                      PCNST -> ) ;
                         if LEN > L . PLEN then
                           begin
                             for I := 1 to LEN - L . PLEN do
                               PSVAL . C [ I ] := PSVAL . C [ I + L .
                                                  PLEN ] ;
                             GENSSLIT ( XMVC , LEN - L . PLEN , Q1 + R
                                        . PLEN , P1 , PSVAL ) ;
                           end (* then *)
                       end (* else *) ;
                   until TRUE ;

        (*********************************************)
        (* this is done in any case before returning *)
        (*********************************************)

                   L . STKADR := STKADRX ;
                   L . PLEN := LEN ;
                 end (* tag/ca *) ;

        /************************************************************/
        /* INT implementieren                                       */
        /************************************************************/

          PINT : begin
                   if L . DTYPE <> PSET then
                     ERROR ( 615 ) ;
                   if R . DTYPE <> PSET then
                     ERROR ( 615 ) ;
                   STKADRX := L . STKADR ;

        (****************************************)
        (* len = minimum length of the operands *)
        (* ok on intersection                   *)
        (****************************************)

                   LEN := L . PLEN ;
                   if LEN > R . PLEN then
                     LEN := R . PLEN ;

        (********************************************)
        (* ONE time loop - using break to terminate *)
        (********************************************)

                   repeat

        (*****************************)
        (* ONE OR BOTH OPERANDS NULL *)
        (*****************************)

                     if LEN <= 0 then
                       begin
                         if R . PLEN <= 0 then
                           begin
                             FREEREG ( L ) ;
                             L := R
                           end (* then *)
                         else
                           FREEREG ( R ) ;
                         break ;
                       end (* then *) ;

        (******************************)
        (* BOTH OPERANDS ARE NOT NULL *)
        (******************************)

                     if not L . VRBL and not R . VRBL then

        (**********************************************************)
        (* both operands are constants, operation at compile time *)
        (**********************************************************)

                       begin
                         for I := 1 to MXSETINX do
                           L . PCNST -> . S [ I ] := L . PCNST -> . S [
                                                   I ] * R . PCNST -> .
                                                   S [ I ] ;
                         MINCONSTSET ;
                         break ;
                       end (* then *) ;

        (*****************************************)
        (* one of the operands is not a constant *)
        (*****************************************)

                     if LEN <= 8 then

        (*******************************************)
        (* len <= 8 - generate result in registers *)
        (*******************************************)

                       begin
                         LR := TRUE ;
                         if L . PLEN > R . PLEN then
                           begin
                             LOAD ( R ) ;
                             LR := FALSE
                           end (* then *)
                         else
                           if L . PLEN < R . PLEN then
                             LOAD ( L )
                           else

        (*************************)
        (* EQUAL LENGTH OPERANDS *)
        (*************************)

                             if not ( L . VRBL and L . DRCT and ( L .
                             VPA = RGS ) ) then
                               if R . VRBL and R . DRCT and ( R . VPA =
                               RGS ) then
                                 LR := FALSE
                               else
                                 LOAD ( L ) ;
                         if not LR then

        (************************)
        (* INTERCHANGE OPERANDS *)
        (************************)

                           begin
                             L := R ;
                             R := STK [ TOP - 1 ]
                           end (* then *) ;
                         if R . VRBL then
                           if R . DRCT and ( R . VPA = RGS ) then
                             begin

        (******************************)
        (* BOTH OPERANDS IN REGISTERS *)
        (******************************)

                               GENRR ( XNR , L . RGADR , R . RGADR ) ;
                               AVAIL [ R . RGADR ] := TRUE ;
                               if L . PLEN > 4 then
                                 GENRR ( XNR , L . RGADR + 1 , R .
                                         RGADR + 1 ) ;
                               if R . PLEN > 4 then
                                 AVAIL [ R . RGADR + 1 ] := TRUE ;
                             end (* then *)
                           else

        (*******************************************)
        (* LEFT OPND IN REGS, RIGHT OPND IN MEMORY *)
        (*******************************************)

                             begin
                               GETOPERAND ( R , Q2 , P2 , B2 ) ;
                               GENRX ( XN , L . RGADR , Q2 , P2 , B2 )
                                       ;
                               if L . PLEN > 4 then
                                 begin
                                   CHECKDISP ( Q2 , P2 , B2 ) ;
                                   GENRX ( XN , L . RGADR + 1 , Q2 + 4
                                           , P2 , B2 )
                                 end (* then *)
                             end (* else *)
                         else

        (******************************************)
        (* LEFT OPND IN REGS, RIGHT OPND IS CONST *)
        (******************************************)

                           begin
                             I_S_R . S := R . PCNST -> . S [ 1 ] ;
                             if I_S_R . I1 <> - 1 then
                               if I_S_R . I1 <> 0 then
                                 GENRXLIT ( XN , L . RGADR , I_S_R . I1
                                            , 0 )
                               else
                                 GENRR ( XSR , L . RGADR , L . RGADR )
                                         ;
                             if LEN > 4 then
                               GENRXLIT ( XN , L . RGADR + 1 , I_S_R .
                                          I2 , 0 )
                             else
                               if L . PLEN > 4 then
                                 AVAIL [ L . RGADR + 1 ] := TRUE ;
                           end (* else *) ;
                         break ;
                       end (* then *) ;

        (*****************************************)
        (* len > 8 - most complicated situation  *)
        (*****************************************)

                     FORCESTK ( L ) ;
                     if R . VRBL then
                       begin

        (***************************)
        (* BOTH OPERANDS IN MEMORY *)
        (***************************)

                         GETQB ( L , Q1 , P1 , 0 ) ;
                         TXRG := TRG1 ;
                         GETQB ( R , Q2 , P2 , 0 ) ;
                         TXRG := TRG14 ;
                         GENSS ( XNC , LEN , Q1 , P1 , Q2 , P2 ) ;
                       end (* then *)
                     else
                       begin

        (*****************************************)
        (* LEFT OPND IN MEM, RIGHT OPND IS CONST *)
        (*****************************************)

                         COMPACT ( R . PCNST -> , LEN , J , CHR ( 255 )
                                   ) ;
                         GETQB ( L , Q1 , P1 , J ) ;
                         LEN := ALIGN ( LEN , INTSIZE ) ;
                         if LEN >= J then
                           GENSSLIT ( XNC , LEN - J , Q1 + J , P1 , R .
                                      PCNST -> ) ;
                       end (* else *) ;
                   until TRUE ;

        (*********************************************)
        (* this is done in any case before returning *)
        (*********************************************)

                   L . STKADR := STKADRX ;
                   L . PLEN := LEN ;
                 end (* tag/ca *) ;

        /************************************************************/
        /* DIF implementieren                                       */
        /************************************************************/

          PDIF : begin
                   if L . DTYPE <> PSET then
                     ERROR ( 615 ) ;
                   if R . DTYPE <> PSET then
                     ERROR ( 615 ) ;

        (****************************************)
        (* len = maximum length of the operands *)
        (* ok on set difference                 *)
        (****************************************)

                   LEN := L . PLEN ;
                   if LEN < R . PLEN then
                     LEN := R . PLEN ;

        (********************************************)
        (* ONE time loop - using break to terminate *)
        (********************************************)

                   repeat

        (******************************************************)
        (* the right operand is null                          *)
        (* nothing to do                                      *)
        (******************************************************)

                     if R . PLEN <= 0 then
                       break ;

        (******************************************************)
        (* the left operand is null                           *)
        (******************************************************)

                     if L . PLEN <= 0 then
                       begin
                         FREEREG ( R ) ;
                         break ;
                       end (* then *) ;

        (******************************)
        (* BOTH OPERANDS ARE NOT NULL *)
        (******************************)

                     if not L . VRBL and not R . VRBL then

        (**********************************************************)
        (* both operands are constants, operation at compile time *)
        (**********************************************************)

                       begin
                         if LEN > R . PLEN then
                           for I := R . PLEN + 1 to LEN do
                             R . PCNST -> . C [ I ] := CHR ( 0 ) ;
                         for I := 1 to MXSETINX do
                           L . PCNST -> . S [ I ] := L . PCNST -> . S [
                                                   I ] - R . PCNST -> .
                                                   S [ I ] ;
                         MINCONSTSET ;
                         break ;
                       end (* then *) ;

        (*****************************************)
        (* one of the operands is not a constant *)
        (*****************************************)

                     if L . PLEN <= 8 then

        (*******************************************)
        (* len <= 8 - generate result in registers *)
        (*******************************************)

                       begin
                         LOAD ( L ) ;
                         if R . VRBL then
                           begin
                             if not ( R . DRCT and ( R . VPA = RGS ) )
                             then
                               begin

        (**************************)
        (* FORCE R INTO REGISTERS *)
        (**************************)

                                 if R . PLEN > L . PLEN then
                                   R . PLEN := L . PLEN ;
                                 LOAD ( R ) ;
                               end (* then *) ;
                             GENRR ( XORX , L . RGADR , R . RGADR ) ;
                             GENRR ( XXR , L . RGADR , R . RGADR ) ;
                             AVAIL [ R . RGADR ] := TRUE ;
                             if R . PLEN > 4 then
                               begin
                                 if L . PLEN > 4 then
                                   begin
                                     GENRR ( XORX , L . RGADR + 1 , R .
                                             RGADR + 1 ) ;
                                     GENRR ( XXR , L . RGADR + 1 , R .
                                             RGADR + 1 ) ;
                                   end (* then *) ;
                                 AVAIL [ R . RGADR + 1 ] := TRUE ;
                               end (* then *)
                           end (* then *)
                         else
                           begin

        (*****************************************)
        (* LEFT OPND IN REGS, RIGHT OPND IS CNST *)
        (*****************************************)

                             I_S_R . S := [ 0 .. 63 ] - R . PCNST -> .
                                          S [ 1 ] ;
                             if I_S_R . I1 <> - 1 then
                               if I_S_R . I1 <> 0 then
                                 GENRXLIT ( XN , L . RGADR , I_S_R . I1
                                            , 0 )
                               else
                                 GENRR ( XSR , L . RGADR , L . RGADR )
                                         ;
                             if ( L . PLEN > 4 ) and ( R . PLEN > 4 )
                             then
                               if I_S_R . I2 <> 0 then
                                 GENRXLIT ( XN , L . RGADR + 1 , I_S_R
                                            . I2 , 0 )
                               else
                                 begin
                                   L . PLEN := 4 ;
                                   AVAIL [ L . RGADR + 1 ] := TRUE
                                 end (* else *) ;
                           end (* else *) ;
                         break ;
                       end (* then *) ;

        (*****************************************)
        (* len > 8 - most complicated situation  *)
        (*****************************************)

                     FORCESTK ( L ) ;
                     if R . VRBL then
                       begin

        (*****************************************)
        (* fraglich, ob das hier richtig ist     *)
        (*****************************************)

                         if not ( R . VRBL and R . DRCT and ( R . VPA =
                         MEM ) ) then
                           FORCESTK ( R ) ;
                         GETQB ( L , Q1 , P1 , 0 ) ;
                         TXRG := TRG1 ;
                         GETQB ( R , Q2 , P2 , 0 ) ;
                         TXRG := TRG14 ;
                         GENSS ( XOC , LEN , Q1 , P1 , Q2 , P2 ) ;
                         GENSS ( XXC , LEN , Q1 , P1 , Q2 , P2 ) ;
                       end (* then *)
                     else
                       begin

        (***********************)
        (* RIGHT OPND IS CONST *)
        (***********************)

                         if LEN > R . PLEN then
                           for I := R . PLEN + 1 to LEN do
                             R . PCNST -> . C [ I ] := CHR ( 0 ) ;
                         for I := 1 to MXSETINX do
                           begin
                             R . PCNST -> . S [ I ] := [ 0 .. 63 ] - R
                                                   . PCNST -> . S [ I ]
                                                   ;
                           end (* for *) ;
                         COMPACT ( R . PCNST -> , LEN , J , CHR ( 255 )
                                   ) ;
                         GETQB ( L , Q1 , P1 , J ) ;
                         if LEN > 0 then
                           GENSSLIT ( XNC , LEN , Q1 + J , P1 , R .
                                      PCNST -> ) ;
                       end (* else *)
                   until TRUE
                 end (* tag/ca *) ;

        /************************************************************/
        /* INN implementieren                                       */
        /************************************************************/

          PINN : INN_OP ;

        /************************************************************/
        /* ASE implementieren                                       */
        /************************************************************/

          PASE : ASE_OP ;
        end (* case *) ;
        STK [ TOP - 1 ] := L ;
        OLDCSP := PSIO ;

        (***********************************)
        (* INDICATE POSSIBLE LOSS OF REG 1 *)
        (***********************************)

      end (* BSETOPS *) ;


   procedure CSETOPS ;

   (************************************************)
   (* CONTROL AND MISCELLANEOUS OPERATIONS ON SETS *)
   (************************************************)


      var Q1 , Q2 : ADRRNG ;
          P1 , P2 , B1 : RGRNG ;
          L , R : DATUM ;
          K : INTEGER ;


      procedure FORCESET ( var STE : DATUM ; LEN : INTEGER ) ;

      (*********************************************)
      (* CONVERTS A SET ADDR INTO SET ON RUN STACK *)
      (*********************************************)


         begin (* FORCESET *)
           with STE do
             if DTYPE = ADR then
               begin
                 if VRBL then
                   begin
                     LOAD ( STE ) ;
                     DRCT := FALSE ;
                   end (* then *)
                 else
                   begin
                     MEMADR := FPA ;
                     FPA := ZEROBL ;
                     DRCT := TRUE ;
                     VRBL := TRUE ;
                     VPA := MEM ;
                   end (* else *) ;
                 DTYPE := PSET ;
                 PLEN := LEN ;
                 STKADR := 0 ;

           (*******************)
           (* TO BE SET LATER *)
           (*******************)

               end (* then *)
             else
               if DTYPE <> PSET then
                 begin
                   WRITELN ( TRACEF , 'dtype falsch = ' , DTYPE ) ;
                   ERROR ( 615 ) ;
                 end (* then *)
         end (* FORCESET *) ;


      begin (* CSETOPS *)
        case PCODE of
          PSLD : with STK [ TOP - 1 ] do
                   begin
                     FORCESET ( STK [ TOP - 1 ] , P ) ;
                     STKADR := Q ;
                   end (* with *) ;
          PSCL : with STK [ TOP ] do
                   begin
                     DTYPE := PSET ;
                     PLEN := P ;
                     STKADR := Q ;
                     VRBL := TRUE ;
                     DRCT := TRUE ;
                     FPA := ZEROBL ;
                     if PLEN = 0 then
                       begin

        (**************************************)
        (* THIS CASE NEVER OCCURS IN PRACTICE *)
        (**************************************)

                         VRBL := FALSE ;
                         VPA := NEITHER ;
                       end (* then *)
                     else
                       if P <= 8 then
                         begin

        (****************)
        (* CLEAR REG(S) *)
        (****************)

                           VPA := RGS ;
                           if P = 4 then
                             FINDRG
                           else
                             FINDRP ;
                           RGADR := NXTRG ;
                           GENRR ( XSR , RGADR , RGADR ) ;
                           if P > 4 then
                             GENRR ( XSR , RGADR + 1 , RGADR + 1 ) ;
                         end (* then *)
                       else
                         begin

        (*****************************)
        (* CLEAR MEMORY ON RUN-STACK *)
        (*****************************)

                           VPA := ONSTK ;
                           GETQB ( STK [ TOP ] , Q1 , P1 , 0 ) ;
                           GENSS ( XXC , PLEN , Q1 , P1 , Q1 , P1 ) ;
                         end (* else *) ;
                     TOP := TOP + 1 ;
                   end (* with *) ;
          PCRD : with STK [ TOP - 1 ] do
                   begin
                     if PLEN <= 4 then
                       LOAD ( STK [ TOP - 1 ] )
                     else
                       if not VRBL or ( DRCT and ( VPA = RGS ) ) then
                         FORCESTK ( STK [ TOP - 1 ] ) ;

        (******************************************)
        (* OPERAND = SINGLE REG. OR A MEMORY AREA *)
        (******************************************)

                     FINDRG ;

        (***********************)
        (* REGISTER FOR RESULT *)
        (***********************)

                     GENRR ( XSR , NXTRG , NXTRG ) ;
                     if PLEN > 4 then
                       begin

        (******************)
        (* MEMORY OPERAND *)
        (******************)

                         GETOPERAND ( STK [ TOP - 1 ] , Q1 , P1 , B1 )
                                      ;
                         if P1 <> TRG14 then

        (*************************)
        (* WE NEED AN INDEX REG. *)
        (*************************)

                           begin
                             GENRX ( XLA , TRG14 , Q1 , P1 , B1 ) ;
                             P1 := TRG14 ;
                             Q1 := 0 ;
                             B1 := 0 ;
                           end (* then *) ;
                         GENRX ( XLA , TRG1 , PLEN DIV 4 , 0 , 0 ) ;
                         GENRX ( XL , 0 , Q1 , P1 , B1 ) ;
                         P2 := 0 ;
                       end (* then *)
                     else
                       P2 := RGADR ;
                     GENRR ( XLTR , 15 , P2 ) ;
                     GENRELRX ( XBC , EQUCND , 6 ) ;

        (***********)
        (* BZ *+12 *)
        (***********)

                     GENRR ( XBCTR , P2 , 0 ) ;
                     GENRR ( XNR , P2 , 15 ) ;
                     GENRELRX ( XBCT , NXTRG , - 5 ) ;

        (******************)
        (* BCT NXTRG,*-10 *)
        (******************)

                     if PLEN > 4 then
                       begin
                         GENRX ( XLA , P1 , 4 , P1 , 0 ) ;
                         GENRELRX ( XBCT , TRG1 , - 11 ) ;

        (***************)
        (* BCT R1,*-22 *)
        (***************)

                       end (* then *)
                     else
                       AVAIL [ RGADR ] := TRUE ;
                     GENRR ( XLPR , NXTRG , NXTRG ) ;
                     DTYPE := BOOL ;
                     DRCT := TRUE ;
                     VRBL := TRUE ;
                     VPA := RGS ;
                     RGADR := NXTRG ;
                     CSPREGACTIVE := FALSE ;

        (***************************)
        (* INDICATE LOSS OF REG 15 *)
        (***************************)

                   end (* with *) ;
          PSMV : begin
                   if FALSE then
                     begin
                       WRITELN ( TRACEF , 'DUMPSTK vor SMV' ) ;
                       DUMPSTK ( TOP - 2 , TOP - 1 )
                     end (* then *) ;
                   TOP := TOP - 2 ;
                   if P < 0 then

        (*********************)
        (* REVERSED OPERANDS *)
        (*********************)

                     begin
                       if FALSE then
                         WRITELN ( TRACEF , 'psmv, p negativ' ) ;
                       L := STK [ TOP + 1 ] ;
                       R := STK [ TOP ] ;
                       P := - P
                     end (* then *)
                   else
                     begin
                       L := STK [ TOP ] ;
                       R := STK [ TOP + 1 ]
                     end (* else *) ;
                   if FALSE then
                     begin
                       WRITELN ( TRACEF , 'psmv, forceset rechts' ) ;
                       WRITELN ( TRACEF , 'r.dtype = ' , R . DTYPE ) ;
                     end (* then *) ;
                   FORCESET ( R , Q ) ;
                   if FALSE then
                     begin
                       WRITELN ( TRACEF , 'psmv, forceset links' ) ;
                       WRITELN ( TRACEF , 'l.dtype = ' , L . DTYPE ) ;
                     end (* then *) ;
                   FORCESET ( L , P ) ;
                   if FALSE then
                     WRITELN ( TRACEF , 'psmv, ende forceset' ) ;

        (***************************************)
        (* L = DESTINATION SET, R = SOURCE SET *)
        (***************************************)

                   if R . VRBL then
                     begin
                       if ( R . PLEN <= 8 ) and ( P = 4 ) and DEBUG
                       then
                         LOAD ( R ) ;
                       if R . DRCT and ( R . VPA = RGS ) then
                         if P < R . PLEN then

        (*****************)
        (* R.PLEN=8, P=4 *)
        (*****************)

                           begin
                             if DEBUG then
                               begin
                                 GENRR ( XBALR , TRG14 , 0 ) ;
                                 GENRR ( XLTR , R . RGADR + 1 , R .
                                         RGADR + 1 ) ;
                                 GENRX ( XBC , NEQCND , SETCHK , GBR ,
                                         0 ) ;
                               end (* then *) ;
                             AVAIL [ R . RGADR + 1 ] := TRUE ;
                             R . PLEN := P ;
                           end (* then *)
                         else

        (***********)
        (* NOTHING *)
        (***********)

                           
                       else

        (******************)
        (* R IS IN MEMORY *)
        (******************)

                         begin
                           TXRG := TRG1 ;
                           K := 0 ;
                           if P < R . PLEN then
                             if DEBUG then
                               K := P ;
                           GETQB ( R , Q2 , P2 , K ) ;
                           TXRG := TRG14 ;
                           if P < R . PLEN then
                             begin
                               if DEBUG then
                                 begin
                                   GENRR ( XBALR , TRG14 , 0 ) ;
                                   GENSS ( XNC , R . PLEN - P , Q2 + P
                                           , P2 , Q2 + P , P2 ) ;
                                   GENRX ( XBC , NEQCND , SETCHK , GBR
                                           , 0 ) ;
                                 end (* then *) ;
                               R . PLEN := P ;
                             end (* then *)
                         end (* else *)
                     end (* then *)
                   else

        (*******************)
        (* R IS A CONSTANT *)
        (*******************)

                     if R . PLEN > P then
                       begin
                         ERROR ( 303 ) ;
                         R . PLEN := P ;
                       end (* then *) ;
                   if P > R . PLEN then
                     begin

        (*************************************)
        (* CLEAR EXCESS BYTES IN DESTINATION *)
        (*************************************)

                       GETQB ( L , Q1 , P1 , R . PLEN ) ;
                       GENSS ( XXC , P - R . PLEN , Q1 + R . PLEN , P1
                               , Q1 + R . PLEN , P1 ) ;
                     end (* then *)
                   else
                     GETQB ( L , Q1 , P1 , 0 ) ;
                   if R . VRBL then
                     if R . DRCT and ( R . VPA = RGS ) then
                       if R . PLEN > 4 then
                         GENRS ( XSTM , R . RGADR , R . RGADR + 1 , Q1
                                 , P1 )
                       else

        (**************)
        (* R.PLEN = 4 *)
        (**************)

                         GENRX ( XST , R . RGADR , Q1 , P1 , 0 )
                     else

        (******************)
        (* R IS IN MEMORY *)
        (******************)

                       GENSS ( XMVC , R . PLEN , Q1 , P1 , Q2 , P2 )
                   else

        (***********************)
        (* R IS A CONSTANT SET *)
        (***********************)

                     if R . PLEN > 0 then
                       begin
                         if FALSE then
                           begin
                             WRITELN ( TRACEF , '---------------------'
                                       '---------------------' ) ;
                             WRITELN ( 'tracef: SMV' ) ;
                             TRACE_SET ( R . PCNST -> , R . PLEN ) ;
                           end (* then *) ;
                         GENSSLIT ( XMVC , R . PLEN , Q1 , P1 , R .
                                    PCNST -> ) ;
                       end (* then *) ;
                   FREEREG ( L ) ;
                   FREEREG ( R ) ;
                 end (* tag/ca *) ;
        end (* case *) ;
        OLDCSP := PSIO ;

        (**************************)
        (* INDICATE LOSS OF REG 1 *)
        (**************************)

      end (* CSETOPS *) ;


   procedure STRINGCOMPARE ( var LEFT , RIGHT : DATUM ) ;

   //****************************************************************
   // implement string (varchar) comparisons                         
   // by calling the routine $PASSCMP                                
   // which is part of PASMONN (runtime)                             
   //****************************************************************


      var LBL : PLABEL ;
          RGWORK : RGRNG ;
          LITVALUE : INTEGER ;

      begin (* STRINGCOMPARE *)

        //******************************************************
        // show stack elements before compare                   
        //******************************************************

        if FALSE then
          begin
            WRITELN ( TRACEF , 'start stringcompare, linecnt = ' ,
                      LINECNT : 1 ) ;
            WRITE ( TRACEF , 'left  ' ) ;
            DUMPSTKELEM ( LEFT ) ;
            WRITE ( TRACEF , 'right ' ) ;
            DUMPSTKELEM ( RIGHT ) ;
          end (* then *) ;

        //******************************************************
        // load strcurr pointer -                               
        // stringcompare uses string workarea                   
        //******************************************************

        GENRX ( XL , TRG1 , STRCURR , 12 , 0 ) ;
        with LEFT do
          begin
            if DTYPE = CARR then
              LITVALUE := PLEN
            else
              LITVALUE := - 1 ;

        //******************************************************
        // store plen, if char array                            
        // or minus one, if varchar                             
        //******************************************************

            GENRXLIT ( XL , 14 , LITVALUE , 0 ) ;
            GENRX ( XST , 14 , 0 , TRG1 , 0 ) ;

        //******************************************************
        // store address of left operand                        
        // take care, if literal (carr constant)                
        //******************************************************

            if VPA = RGS then
              begin
                RGWORK := RGADR
              end (* then *)
            else
              begin
                P2 := FPA . LVL ;
                Q2 := FPA . DSPLMT ;
                BASE ( Q2 , P2 , B2 ) ;
                if P2 < 0 then
                  begin
                    LITTBL [ SCNSTNO ] . LNK := PCOUNTER + 1 ;
                    P2 := 0 ;
                  end (* then *) ;
                GENRX ( XLA , 14 , Q2 , B2 , P2 ) ;
                RGWORK := 14 ;
              end (* else *) ;
            GENRX ( XST , RGWORK , 4 , TRG1 , 0 ) ;
          end (* with *) ;

        //******************************************************
        // do the same for the right operand                    
        // at offsets 8 and 12 from R1                          
        //******************************************************

        with RIGHT do
          begin
            if DTYPE = CARR then
              LITVALUE := PLEN
            else
              LITVALUE := - 1 ;
            GENRXLIT ( XL , 14 , LITVALUE , 0 ) ;
            GENRX ( XST , 14 , 8 , TRG1 , 0 ) ;
            if VPA = RGS then
              begin
                RGWORK := RGADR
              end (* then *)
            else
              begin
                P2 := FPA . LVL ;
                Q2 := FPA . DSPLMT ;
                BASE ( Q2 , P2 , B2 ) ;
                if P2 < 0 then
                  begin
                    LITTBL [ SCNSTNO ] . LNK := PCOUNTER + 1 ;
                    P2 := 0 ;
                  end (* then *) ;
                GENRX ( XLA , 14 , Q2 , B2 , P2 ) ;
                RGWORK := 14 ;
              end (* else *) ;
            GENRX ( XST , RGWORK , 12 , TRG1 , 0 ) ;
          end (* with *) ;

        //******************************************************
        // free the registers possibly in use                   
        // in the left and right operands                       
        //******************************************************

        FREEREG ( LEFT ) ;
        FREEREG ( RIGHT ) ;

        //******************************************************
        // call the $PASSCMP routine (in PASMONN)               
        //******************************************************

        LBL . NAM := '$PASSCMP' ;
        LBL . LEN := 8 ;
        GENRXLAB ( XL , TRG15 , LBL , - 3 ) ;
        GENRR ( XBALR , 14 , 15 ) ;

        //******************************************************
        // indicate loss of reg 1 and reg 15                    
        //******************************************************

        CSPREGACTIVE := FALSE ;
        OLDCSP := PSIO ;

        //******************************************************
        // set the condition mask for the following branch      
        // depending on the PCODE which started the string      
        // comparison                                           
        //******************************************************

        BRCND := BRMSK [ PCODE ] ;
      end (* STRINGCOMPARE *) ;


   procedure SETCOMPARE ( var L , R : DATUM ) ;

   (*************************************)
   (* GENERATE CODE FOR SET COMPARISONS *)
   (*************************************)


      var Q1 , Q2 , FIXUPLOC : ADRRNG ;
          P1 , P2 : LVLRNG ;
          EQ , INTCHG , CONSTSET , TEST_PENDING : BOOLEAN ;
          I , MIN : INTEGER ;


      procedure TESTNULL ( var STE : DATUM ; var Q : ADRRNG ; var P :
                         LVLRNG ; LEN : ADRRNG ) ;

         begin (* TESTNULL *)
           if LEN < STE . PLEN then
             begin
               GETQB ( STE , Q , P , LEN ) ;
               GENSS ( XNC , STE . PLEN - LEN , Q + LEN , P , Q + LEN ,
                       P ) ;
               TEST_PENDING := TRUE ;
               STE . PLEN := LEN ;
             end (* then *)
           else
             GETQB ( STE , Q , P , 0 ) ;
         end (* TESTNULL *) ;


      procedure GENBRANCH ;

      (****************************************)
      (* GENERATES INTERMEDIATE TEST BRANCHES *)
      (****************************************)


         begin (* GENBRANCH *)
           if TEST_PENDING then
             begin
               TESTCNT := TESTCNT + 1 ;

           (************************************************)
           (* IF ASM THEN                                  *)
           (* BEGIN FIXUPLOC := 0;                         *)
           (*       WRITELN(PRR,' BNZ T',TESTCNT:1);       *)
           (* END                                          *)
           (* ELSE                                         *)
           (************************************************)

               if ASM then
                 begin
                   WRITELN ( ASMOUT , '## ' , ' ' : SPACEASMX ,
                             'BNZ   T' , TESTCNT : 1 ) ;
                 end (* then *) ;
               begin
                 GENRX ( XBC , NEQCND , 0 , 0 , 0 ) ;
                 FIXUPLOC := PCOUNTER - 1 ;
               end ;
             end (* then *)
         end (* GENBRANCH *) ;


      procedure SETCONSTBOOL ( B : BOOLEAN ) ;

         begin (* SETCONSTBOOL *)
           FREEREG ( L ) ;
           L . FPA . LVL := 0 ;
           L . FPA . DSPLMT := ORD ( B ) ;
           L . VRBL := FALSE ;
           L . DRCT := TRUE ;
           L . VPA := NEITHER ;
           CONSTSET := TRUE ;
         end (* SETCONSTBOOL *) ;


      begin (* SETCOMPARE *)
        CONSTSET := FALSE ;
        INTCHG := FALSE ;
        TEST_PENDING := FALSE ;
        FIXUPLOC := - 1 ;
        if ( PCODE = PEQU ) or ( PCODE = PNEQ ) then
          begin
            repeat
              if INTCHG then
                begin
                  L := STK [ TOP ] ;
                  R := STK [ TOP - 1 ] ;
                end (* then *)
              else
                begin
                  L := STK [ TOP - 1 ] ;
                  R := STK [ TOP ] ;
                end (* else *) ;
              INTCHG := FALSE ;
              if L . PLEN <= 0 then

        (*********************)
        (* NULL LEFT OPERAND *)
        (*********************)

                if R . PLEN <= 0 then

        (**********************)
        (* NULL RIGHT OPERAND *)
        (**********************)

                  SETCONSTBOOL ( PCODE = PEQU )
                else
                  if R . VRBL then
                    if R . DRCT and ( R . VPA = RGS ) then
                      if R . PLEN = 4 then
                        GENRR ( XLTR , R . RGADR , R . RGADR )
                      else
                        GENRR ( XORX , R . RGADR , R . RGADR + 1 )
                    else

        (******************)
        (* R IS IN MEMORY *)
        (******************)

                      TESTNULL ( R , Q2 , P2 , 0 )
                  else

        (*****************)
        (* R IS CONSTANT *)
        (*****************)

                    SETCONSTBOOL ( PCODE <> PEQU )
              else
                if L . VRBL then
                  if L . DRCT and ( L . VPA = RGS ) then
                    if R . PLEN <= 0 then
                      INTCHG := TRUE
                    else
                      if R . VRBL then
                        if R . DRCT and ( R . VPA = RGS ) then
                          begin
                            GENRR ( XXR , L . RGADR , R . RGADR ) ;
                            if L . PLEN < R . PLEN then
                              GENRR ( XORX , L . RGADR , R . RGADR + 1
                                      )
                            else
                              if L . PLEN > 4 then
                                begin
                                  if R . PLEN > 4 then
                                    GENRR ( XXR , L . RGADR + 1 , R .
                                            RGADR + 1 ) ;
                                  GENRR ( XORX , L . RGADR , L . RGADR
                                          + 1 ) ;
                                end (* then *) ;
                          end (* then *)
                        else

        (******************)
        (* R IS IN MEMORY *)
        (******************)

                          begin
                            TESTNULL ( R , Q2 , P2 , L . PLEN ) ;
                            GENBRANCH ;
                            GENRX ( XX , L . RGADR , Q2 , P2 , 0 ) ;
                            if L . PLEN > 4 then
                              begin
                                if R . PLEN >= 8 then
                                  GENRX ( XX , L . RGADR + 1 , Q2 + 4 ,
                                          P2 , 0 ) ;
                                GENRR ( XORX , L . RGADR , L . RGADR +
                                        1 ) ;
                              end (* then *)
                          end (* else *)
                      else

        (*****************)
        (* R IS CONSTANT *)
        (*****************)

                        if R . PLEN > L . PLEN then
                          SETCONSTBOOL ( PCODE <> PEQU )
                        else
                          begin
                            I_S_R . S := R . PCNST -> . S [ 1 ] ;
                            if I_S_R . I1 <> 0 then
                              GENRXLIT ( XX , L . RGADR , I_S_R . I1 ,
                                         0 ) ;
                            if L . PLEN > 4 then
                              begin
                                if R . PLEN >= 8 then
                                  GENRXLIT ( XX , L . RGADR + 1 , I_S_R
                                             . I2 , 0 ) ;
                                GENRR ( XORX , L . RGADR , L . RGADR +
                                        1 ) ;
                              end (* then *)
                          end (* else *)
                  else

        (******************)
        (* L IS IN MEMORY *)
        (******************)

                    if ( R . PLEN = 0 ) or ( R . VRBL and R . DRCT and
                    ( R . VPA = RGS ) ) then
                      INTCHG := TRUE
                    else
                      if R . VRBL then

        (******************)
        (* R IS IN MEMORY *)
        (******************)

                        begin
                          TESTNULL ( L , Q1 , P1 , R . PLEN ) ;
                          TXRG := TRG1 ;
                          TESTNULL ( R , Q2 , P2 , L . PLEN ) ;
                          TXRG := TRG14 ;
                          GENBRANCH ;
                          MIN := L . PLEN ;
                          if MIN > R . PLEN then
                            MIN := R . PLEN ;
                          GENSS ( XCLC , MIN , Q1 , P1 , Q2 , P2 ) ;
                        end (* then *)
                      else

        (*****************)
        (* R IS CONSTANT *)
        (*****************)

                        if L . PLEN < R . PLEN then
                          SETCONSTBOOL ( PCODE <> PEQU )
                        else
                          begin
                            TESTNULL ( L , Q1 , P1 , R . PLEN ) ;
                            GENBRANCH ;
                            GENSSLIT ( XCLC , R . PLEN , Q1 , P1 , R .
                                       PCNST -> ) ;
                          end (* else *)
                else

        (*****************)
        (* L IS CONSTANT *)
        (*****************)

                  if ( R . PLEN = 0 ) or R . VRBL then
                    INTCHG := TRUE
                  else
                    begin
                      EQ := TRUE ;
                      for I := 1 to MXSETINX do
                        if L . PCNST -> . S [ I ] <> R . PCNST -> . S [
                        I ] then
                          EQ := FALSE ;
                      SETCONSTBOOL ( ( PCODE = PEQU ) = EQ ) ;
                    end (* else *) ;
            until not INTCHG ;
          end (* then *)
        else
          begin

        (******************************************************)
        (* pcode IS PGEQ OR PLEQ                              *)
        (******************************************************)

            if PCODE = PGEQ then
              begin
                L := STK [ TOP ] ;
                R := STK [ TOP - 1 ]
              end (* then *)
            else
              begin
                L := STK [ TOP - 1 ] ;
                R := STK [ TOP ]
              end (* else *) ;
            PCODE := PEQU ;
            if L . PLEN <= 4 then
              begin
                LOAD ( L ) ;
                if R . PLEN = 0 then
                  GENRR ( XLTR , L . RGADR , L . RGADR )
                else
                  if R . VRBL then
                    if R . DRCT and ( R . VPA = RGS ) then
                      begin
                        GENRR ( XORX , L . RGADR , R . RGADR ) ;
                        GENRR ( XXR , L . RGADR , R . RGADR ) ;
                      end (* then *)
                    else
                      begin
                        GETOPERAND ( R , Q1 , P1 , B1 ) ;
                        GENRX ( XO , L . RGADR , Q1 , P1 , B1 ) ;
                        GENRX ( XX , L . RGADR , Q1 , P1 , B1 ) ;
                      end (* else *)
                  else

        (*****************)
        (* R IS CONSTANT *)
        (*****************)

                    begin
                      I_S_R . S := R . PCNST -> . S [ 1 ] ;
                      GENRXLIT ( XO , L . RGADR , I_S_R . I1 , 0 ) ;
                      GENRXLIT ( XX , L . RGADR , I_S_R . I1 , 0 ) ;
                    end (* else *)
              end (* then *)
            else
              begin
                FORCESTK ( L ) ;
                TESTNULL ( L , Q1 , P1 , R . PLEN ) ;
                if R . PLEN > 0 then
                  begin
                    GENBRANCH ;
                    if R . VRBL then
                      if R . DRCT and ( R . VPA = RGS ) then
                        begin
                          GENRX ( XN , R . RGADR , Q1 , P1 , 0 ) ;
                          GENRX ( XX , R . RGADR , Q1 , P1 , 0 ) ;
                          if R . PLEN > 4 then
                            begin
                              GENRX ( XN , R . RGADR + 1 , Q1 + 4 , P1
                                      , 0 ) ;
                              GENRX ( XX , R . RGADR + 1 , Q1 + 4 , P1
                                      , 0 ) ;
                              GENRR ( XORX , R . RGADR , R . RGADR + 1
                                      ) ;
                            end (* then *)
                        end (* then *)
                      else
                        begin
                          TXRG := TRG1 ;
                          GETQB ( R , Q2 , P2 , 0 ) ;
                          TXRG := TRG14 ;
                          GENSS ( XOC , L . PLEN , Q1 , P1 , Q2 , P2 )
                                  ;
                          GENSS ( XXC , L . PLEN , Q1 , P1 , Q2 , P2 )
                                  ;
                        end (* else *)
                    else
                      begin

        (*****************)
        (* R IS CONSTANT *)
        (*****************)

                        GENSSLIT ( XOC , L . PLEN , Q1 , P1 , R . PCNST
                                   -> ) ;

        (**************************************************************)
        (* IF ASM THEN                                                *)
        (*    GENSSLIT( XXC, L.PLEN, Q1, P1, R.PCNST@ )               *)
        (* ELSE                                                       *)
        (*                                                            *)
        (*                                                            *)
        (*  da machen wir nix, weil das, was unten kommt,             *)
        (*  ja seine befehle ausgeben sollte ...                      *)
        (*                                                            *)
        (**************************************************************)

                        begin

        (********************************)
        (*KLUDGE TO RE-USE SAME CONSTANT*)
        (********************************)

                          GENSS ( XXC , L . PLEN , Q1 , P1 , 0 , 0 ) ;
                          CODE . H [ PCOUNTER - 1 ] := TO_HINT ( CODE .
                                                   H [ PCOUNTER - 4 ] )
                                                   ;
                          NXTLIT := NXTLIT + 1 ;
                          LITTBL [ NXTLIT ] . LNK := PCOUNTER - 1 ;
                          LITTBL [ NXTLIT ] . LTYPE := 'X' ;
                          LITTBL [ NXTLIT ] . LENGTH := 0 ;
                          LITTBL [ NXTLIT ] . XIDP := 0 ;
                        end ;
                      end (* else *)
                  end (* then *)
              end (* else *)
          end (* else *) ;
        FREEREG ( L ) ;
        FREEREG ( R ) ;
        L . DTYPE := BOOL ;
        if not CONSTSET then
          BRCND := BRMSK [ PCODE ] ;
        if FIXUPLOC >= 0 then
          begin
            if ASM then
              WRITELN ( ASMOUT , '## ' , ' ' : SPACEASML , 'T' ,
                        TESTCNT : 1 , '     DS    0H' ) ;
            CODE . H [ FIXUPLOC ] := TO_HINT ( BASE_DSPLMT ( PCOUNTER )
                                     ) ;
          end (* then *) ;
        OLDCSP := PSIO ;

        (**************************)
        (* INDICATE LOSS OF REG 1 *)
        (**************************)

        TXR_CONTENTS . VALID := FALSE ;
      end (* SETCOMPARE *) ;


   procedure COPERATION ;

   (***********************************)
   (* CONTROL AND BRANCH INSTRUCTIONS *)
   (* ------------------------------- *)
   (***********************************)


      var PCNEU : ICRNG ;
          PC : ICRNG ;
          CASE_LAUF : INTEGER ;
          CL : LBLRNG ;
          X1 : INTEGER ;
          X2 : INTEGER ;
          LBL_WORK : PLABEL ;


      procedure MKLBL ( var LBL : PLABEL ; Q : LBLRNG ) ;

      (*********************************)
      (* ASSUMES     0 <= Q <= 9999999 *)
      (*********************************)


         var I : 1 .. 8 ;

         begin (* MKLBL *)
           I := FLDW ( Q ) + 1 ;
           LBL . NAM := 'L       ' ;
           LBL . LEN := I ;
           repeat
             LBL . NAM [ I ] := CHR ( ( Q MOD 10 ) + ORD ( '0' ) ) ;
             Q := Q DIV 10 ;
             I := I - 1 ;
           until Q = 0 ;
         end (* MKLBL *) ;


      procedure ADDLNP ( PCDIF : BYTE ) ;

      (*******************************************************)
      (* TO ADD A (SOURCE) LINE POINTER TO THE POINTER TABLE *)
      (* --------------------------------------------------- *)
      (*******************************************************)


         begin (* ADDLNP *)
           if NXTLNP < MXLNP then
             begin
               CODE . C [ MXCODE * 2 + NXTLNP ] := CHR ( PCDIF ) ;
               NXTLNP := NXTLNP + 1 ;
             end (* then *)
         end (* ADDLNP *) ;


      procedure UPDLNTBL ( PCDIF : ICRNG ) ;

      (**************************************************************)
      (* TO UPDATE LINE POINTER TABLE FOR THE RUN TIME DEBUG OPTION *)
      (* ---------------------------------------------------------- *)
      (**************************************************************)


         begin (* UPDLNTBL *)
           if PCDIF >= 250 then

           (*********************)
           (* ENTER ESCAPE MODE *)
           (*********************)

             begin
               ADDLNP ( 254 ) ;

           (*************)
           (*ESCAPE CHAR*)
           (*************)

               ADDLNP ( PCDIF DIV 256 ) ;
               ADDLNP ( PCDIF MOD 256 ) ;
             end (* then *)
           else
             ADDLNP ( PCDIF ) ;
         end (* UPDLNTBL *) ;


      procedure INIT_CSECT ;

      (*************************************************)
      (* TO INITIALIZE OBJECT CODE TABLES AND POINTERS *)
      (* --------------------------------------------- *)
      (*************************************************)


         var I : LBLRNG ;
             BTARGET : INTEGER ;
             CSECT_NAME : ALFA ;
             LEN_CSECTINFO : INTEGER ;
             CODEPOS : INTEGER ;
             IXCODE : INTEGER ;

         begin (* INIT_CSECT *)
           CSECT_NAME := LBL1 . NAM ;
           for I := 0 to LBLCNT do
             with LBLTBL [ I ] do
               begin
                 DEFINED := FALSE ;
                 LNK := 1
               end (* with *) ;
           NXTLIT := 0 ;
           LX . NXTDBL := 0 ;
           LX . NXTCH := 0 ;
           LX . IHCONF := - 1 ;
           LX . RICONF := - 1 ;
           LX . RHCONF := - 1 ;
           LX . INT_GAP := - 1 ;
           LX . HW_GAP := - 1 ;
           POOL_SIZE := 0 ;
           NUMLITS := 0 ;
           DBLALN := FALSE ;
           LAST_CC . LPC := 0 ;
           TXR_CONTENTS . VALID := FALSE ;
           LAST_STR . LPC := 0 ;
           LAST_MVC . LPC := 0 ;
           LAST_FILE . LPC := 0 ;
           PRCTBL [ 0 ] . NAME := LBL1 . NAM ;
           PRCTBL [ 0 ] . LNK := 0 ;
           PRCTBL [ 1 ] . LNK := 0 ;
           NXTPRC := 1 ;
           NXTEP := PRCCNT ;
           CALDPTH := 0 ;
           PCOUNTER := 0 ;
           MINLBL := LBLMAP ( PIAKT -> . SEGSZE . NAM ) ;
           LASTPC := 0 ;

           (*************************************************)
           (* header for pasmain contains compile timestamp *)
           (*************************************************)

           if CURLVL = 1 then
             LEN_CSECTINFO := 9 + IDLNGTH + 1 + HDRLNGTH
           else
             LEN_CSECTINFO := 9 + IDLNGTH ;
           if ASM then
             begin
               if CURLVL = 1 then
                 WRITELN ( ASMOUT , 'BGN  ' : 26 , CSECT_NAME , ',' ,
                           PIAKT -> . CURPNAME , ',' , PROGHDR : 1 )
               else
                 WRITELN ( ASMOUT , 'BGN  ' : 26 , CSECT_NAME , ',' ,
                           PIAKT -> . CURPNAME ) ;
               HEXHW ( PCOUNTER * 2 , HEXPC ) ;
               WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
               WRITELN ( ASMOUT , CSECT_NAME , ' CSECT' ) ;
             end (* then *) ;

           (************************************************)
           (* branch over constants                        *)
           (* including procedure names                    *)
           (************************************************)

           BTARGET := ( 24 + LEN_CSECTINFO ) DIV 2 ;
           BTARGET := ALIGN ( BTARGET , 2 ) ;
           GENRX ( XBC , ANYCND , BTARGET * 2 , 0 , JREG ) ;

           (************************************************)
           (* pcounter = position after ep constants       *)
           (************************************************)

           PCOUNTER := BTARGET ;
           PCAFTLIT := PCOUNTER ;

           (************************************************)
           (* format ep constants area and insert them     *)
           (************************************************)

           for I := 2 to PCOUNTER - 1 do
             CODE . H [ I ] := 0 ;
           IXCODE := 4 ;
           CODE . C [ IXCODE ] := CHR ( LEN_CSECTINFO ) ;
           for I := 1 to 8 do
             CODE . C [ IXCODE + I ] := CSECT_NAME [ I ] ;
           IXCODE := 13 ;
           CODE . C [ IXCODE ] := ' ' ;
           for I := 1 to IDLNGTH do
             CODE . C [ IXCODE + I ] := PIAKT -> . CURPNAME [ I ] ;
           if CURLVL = 1 then
             begin
               IXCODE := 14 + IDLNGTH ;
               CODE . C [ IXCODE ] := ' ' ;
               for I := 1 to HDRLNGTH do
                 CODE . C [ IXCODE + I ] := PROGHDR [ I ] ;
             end (* then *) ;

           (*************************************************)
           (* more ep constants:                            *)
           (*-----------------------------------------------*)
           (* pcounter - 9: Compiler signature (6 bytes)    *)
           (* pcounter - 6: Compiler version   (2 bytes)    *)
           (*-----------------------------------------------*)
           (* pcounter - 5: stacksize (set by gen_csect)    *)
           (*-----------------------------------------------*)
           (* pcounter - 4: DEBUG-Level                     *)
           (*-----------------------------------------------*)
           (* pcounter - 3: proclen (set by gen_csect)      *)
           (*-----------------------------------------------*)
           (* pcounter - 2: pointer to static csect         *)
           (* set by v-constant, see below                  *)
           (*************************************************)

           CODE . H [ PCOUNTER - 9 ] := TO_HINT ( ORD ( 'S' ) * 256 +
                                        ORD ( 'T' ) ) ;
           CODE . H [ PCOUNTER - 8 ] := TO_HINT ( ORD ( 'P' ) * 256 +
                                        ORD ( 'A' ) ) ;
           CODE . H [ PCOUNTER - 7 ] := TO_HINT ( ORD ( 'S' ) * 256 +
                                        ORD ( 'C' ) ) ;
           CODE . H [ PCOUNTER - 6 ] := VERSION2 ;
           CODE . H [ PCOUNTER - 5 ] := 0 ;
           CODE . H [ PCOUNTER - 4 ] := PIAKT -> . DEBUG_LEV ;
           CODE . H [ PCOUNTER - 3 ] := 0 ;
           CODE . H [ PCOUNTER - 2 ] := 0 ;
           CODE . H [ PCOUNTER - 1 ] := 0 ;

           (*********************************************)
           (* ins_prctbl: statname wird als v-adresse   *)
           (* registriert. wert = pcounter - 2. wirkt   *)
           (* genauso wie die ablage eines literals,    *)
           (* aber an dieser definierten stelle.        *)
           (* spaetere bezugnamen auf das literal       *)
           (* Statname (als v-adresse) via              *)
           (* UPD_PRCTBL holen dann ihre adresse        *)
           (* von hier.                                 *)
           (*********************************************)

           if PIAKT -> . STATNAME [ 1 ] <> ' ' then
             INS_PRCTBL ( PIAKT -> . STATNAME , PCOUNTER - 2 ) ;

           (*********************************************)
           (* pos of proc len                           *)
           (*********************************************)

           POSOFPROCLEN := ( PCOUNTER - 3 ) * 2 ;

           (*********************************************)
           (* UNIQUE PROC NO                            *)
           (*********************************************)

           CODE . H [ MXCODE ] := PIAKT -> . CURPNO ;

           (************************************************)
           (* the procedure name which is written          *)
           (* in case of debug is larger now (20 instead   *)
           (* of 12), so nxtnlp has to be started at       *)
           (* 24 instead of 16 - opp 2016                  *)
           (************************************************)

           if PIAKT -> . DEBUG_LEV > 0 then
             begin
               CODE . H [ MXCODE + 1 ] := LASTLN ;
               CODEPOS := MXCODE * 2 + 3 ;
               for I := 1 to 8 do
                 begin
                   CODEPOS := CODEPOS + 1 ;
                   CODE . C [ CODEPOS ] := PIAKT -> . SOURCENAME [ I ]
                                           ;
                 end (* for *) ;
               NXTLNP := 12 ;
             end (* then *)
           else
             NXTLNP := 0 ;
           if ASM then
             begin
               HEXHW ( 4 , HEXPC ) ;
               WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
               WRITE ( ASMOUT , 'DC  ' : COLASMI , ' ' : SPACEASMI ) ;
               WRITELN ( ASMOUT , 'AL1(' , LEN_CSECTINFO : 1 , ')' ) ;
               HEXHW ( 5 , HEXPC ) ;
               WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
               WRITE ( ASMOUT , 'DC  ' : COLASMI , ' ' : SPACEASMI ) ;
               WRITE ( ASMOUT , 'C''' ) ;
               for I := 1 to 8 do
                 WRITE ( ASMOUT , CSECT_NAME [ I ] ) ;
               WRITE ( ASMOUT , ' ' ) ;
               for I := 1 to IDLNGTH do
                 WRITE ( ASMOUT , PIAKT -> . CURPNAME [ I ] ) ;
               if CURLVL = 1 then
                 begin
                   WRITE ( ASMOUT , ' ' ) ;
                   for I := 1 to HDRLNGTH do
                     WRITE ( ASMOUT , PROGHDR [ I ] ) ;
                 end (* then *) ;
               WRITELN ( ASMOUT , '''' ) ;
               HEXHW ( POSOFPROCLEN - 12 , HEXPC ) ;
               WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
               WRITE ( ASMOUT , 'DC  ' : COLASMI , ' ' : SPACEASMI ) ;
               WRITELN ( ASMOUT , 'CL6''STPASC'''
                         '    -- Compiler signature' ) ;
               HEXHW ( POSOFPROCLEN - 6 , HEXPC ) ;
               WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
               WRITE ( ASMOUT , 'DC  ' : COLASMI , ' ' : SPACEASMI ) ;
               WRITELN ( ASMOUT , VERSION3 ,
                         '      -- Compiler version' ) ;
               HEXHW ( POSOFPROCLEN - 4 , HEXPC ) ;
               WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
               WRITE ( ASMOUT , 'DC  ' : COLASMI , ' ' : SPACEASMI ) ;
               WRITELN ( ASMOUT , 'AL2(0)' , '         -- Stacksize' )
                         ;
               HEXHW ( POSOFPROCLEN - 2 , HEXPC ) ;
               WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
               WRITE ( ASMOUT , 'DC  ' : COLASMI , ' ' : SPACEASMI ) ;
               WRITELN ( ASMOUT , 'AL2(' , PIAKT -> . DEBUG_LEV : 1 ,
                         ')' , '         -- Debug-Level' ) ;
               HEXHW ( POSOFPROCLEN , HEXPC ) ;
               WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
               WRITE ( ASMOUT , 'DC  ' : COLASMI , ' ' : SPACEASMI ) ;
               WRITELN ( ASMOUT , 'AL2(0)' ,
                         '         -- Length of Proc' ) ;
               HEXHW ( POSOFPROCLEN + 2 , HEXPC ) ;
               WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
               WRITE ( ASMOUT , 'DC  ' : COLASMI , ' ' : SPACEASMI ) ;
               if PIAKT -> . STATNAME [ 1 ] <> ' ' then
                 WRITELN ( ASMOUT , 'V(' , PIAKT -> . STATNAME , ')' ,
                           '    -- Static CSECT' )
               else
                 WRITELN ( ASMOUT , 'A(0)' ,
                           '           -- Static CSECT' ) ;
             end (* then *) ;
         end (* INIT_CSECT *) ;


      procedure GEN_CSECT ( STACKSIZE : INTEGER ) ;

      (************************************************)
      (* TO MERGE LITERAL POOLS AND GENERATE          *)
      (* ONE OBJECT MODULE FOR THIS PROC              *)
      (* -------------------------------------------- *)
      (************************************************)


         label 10 ;

         const XESD = 46523076 ;

               (**********************)
               (*EBCDIC FOR  2|'ESD' *)
               (**********************)

               XTXT = 48490467 ;

               (**********************)
               (*               TXT  *)
               (**********************)

               XRLD = 47829956 ;

               (**********************)
               (*               RLD  *)
               (**********************)

               XEND = 46519748 ;

               (**********************)
               (*               END  *)
               (**********************)

               BLNK1 = 64 ;

               (**********************)
               (* EBCDIC FOR ' '     *)
               (**********************)

               BLNK2 = 16448 ;
               BLNK3 = 4210752 ;
               BLNK4 = 1077952576 ;

         var I , J , K : INTEGER ;
             TPC , QPC , OBJEND : INTEGER ;
             LNGTH : STRLRNG ;
             VSL16 : INTEGER ;
             BLNK80 : array [ 1 .. 80 ] of CHAR ;
             BLNK64 : array [ 1 .. 64 ] of CHAR ;
             CODESIZE : INTEGER ;
             CARD : record
                      case INTEGER of
                        1 :
                          ( C : array [ 1 .. 80 ] of CHAR ) ;

             (*****************)
             (*CHAR CARD IMAGE*)
             (*****************)

                        2 :
                          ( I : array [ 1 .. 20 ] of INTEGER ) ;

             (*****************)
             (*INT. CARD IMAGE*)
             (*****************)

                        3 :
                          ( H : array [ 1 .. 40 ] of HINTEGER )

             (*******************)
             (*HALFWORD IMAGE   *)
             (*******************)

                    end ;
             ESD_CARD : record
                          case INTEGER of
                            1 :
                              ( C16 : array [ 1 .. 16 ] of CHAR ;
                                C64 : array [ 1 .. 64 ] of CHAR ) ;
                            2 :
                              ( I4 : array [ 1 .. 4 ] of INTEGER ;
                                ITEM : array [ 1 .. 3 ] of record
                                                   XNAME : ALFA ;
                                                   F1 , F2 : INTEGER
                                                   end ) ;
                            3 :
                              ( C80 : array [ 1 .. 80 ] of CHAR )
                        end ;


         procedure PRINT_CSECT ( LPC1 : ICRNG ) ;

            label 10 ;

            var LPC , CON1 , CON2 : HEX4 ;
                APC , APC1 : ICRNG ;
                I , K : 0 .. 9999 ;

            begin (* PRINT_CSECT *)
              WRITELN ( ASMOUT ) ;
              WRITELN ( ASMOUT , ' OBJECT CODE FOR CSECT' , PRCTBL [ 0
                        ] . NAME : 9 , '(PROCEDURE ' : 13 , PIAKT -> .
                        CURPNAME , ')' ) ;
              APC := 0 ;
              APC1 := 0 ;
              repeat
                WRITELN ( ASMOUT ) ;
                HEXHW ( 2 * APC , LPC ) ;
                WRITE ( ASMOUT , LPC : 5 , ':' ) ;
                for I := 0 to 7 do

              (*********************)
              (* 32 BYTES PER LINE *)
              (*********************)

                  begin
                    if I = 4 then
                      WRITE ( ASMOUT , ' ' ) ;
                    HEXHW ( CODE . H [ APC1 ] , CON1 ) ;
                    HEXHW ( CODE . H [ APC1 + 1 ] , CON2 ) ;
                    WRITE ( ASMOUT , ' ' , CON1 , CON2 ) ;
                    APC := APC + 2 ;
                    APC1 := APC1 + 2 ;
                    if APC1 >= LPC1 then
                      goto 10 ;
                  end (* for *) ;
              until FALSE ;
              10 :
              K := 0 ;
              if ( NXTPRC > 1 ) or ( NXTEP < PRCCNT ) then
                begin
                  WRITELN ( ASMOUT ) ;
                  WRITELN ( ASMOUT ) ;
                  WRITELN ( ASMOUT ,
                         ' EXTERNAL REFERENCES AND LABEL DEFINITIONS:'
                            ) ;
                  for I := 0 to PRCCNT do
                    if ( I < NXTPRC ) or ( I > NXTEP ) then
                      with PRCTBL [ I ] do
                        if LNK > 0 then
                          begin
                            if ( K MOD 3 ) = 0 then
                              WRITELN ( ASMOUT ) ;
                            K := K + 1 ;
                            HEXHW ( LNK * 2 , CON1 ) ;
                            WRITE ( ASMOUT , CON1 : 5 , ':' , NAME : 9
                                    ) ;
                            if I < NXTPRC then
                              WRITE ( ASMOUT , ' (ER);    ' )
                            else
                              WRITE ( ASMOUT , ' (LD);    ' ) ;
                          end (* then *) ;
                  WRITELN ( ASMOUT ) ;
                end (* then *)
              else
                WRITELN ( ASMOUT ) ;
              if PIAKT -> . DEBUG_LEV > 0 then
                begin
                  WRITELN ( ASMOUT ) ;
                  WRITELN ( ASMOUT , ' DEBUG INFORMATION:' ) ;
                  WRITELN ( ASMOUT ) ;
                  WRITELN ( ASMOUT , ' DEBUG LEVEL  = ' , PIAKT -> .
                            DEBUG_LEV : 1 ) ;
                  WRITELN ( ASMOUT , ' SOURCENAME   = ' , PIAKT -> .
                            SOURCENAME ) ;
                  WRITELN ( ASMOUT , ' PROCNAME     = ' , PIAKT -> .
                            CURPNAME ) ;
                  WRITELN ( ASMOUT , ' CODESIZE     = ' , CODESIZE : 1
                            ) ;
                  WRITELN ( ASMOUT , ' STATIC CSECT = ' , PIAKT -> .
                            STATNAME ) ;
                  WRITELN ( ASMOUT , ' STACKSIZE    = ' , STACKSIZE : 1
                            ) ;
                end (* then *) ;
              WRITELN ( ASMOUT ) ;
            end (* PRINT_CSECT *) ;


         begin (* GEN_CSECT *)
           QPC := LBLTBL [ MINLBL ] . LNK ;
           while QPC > 1 do
             begin
               TPC := CODE . H [ QPC ] ;
               UPD_INTTBL ( QPC , STACKSIZE ) ;
               QPC := TPC ;
             end (* while *) ;
           DUMP_LITERALS ;

           (********************************)
           (* PROCESS EXTERNAL REFERENCES  *)
           (********************************)

           for I := 0 to NXTPRC - 1 do
             with PRCTBL [ I ] do
               if LNK > 0 then
                 begin
                   TPC := LNK ;
                   if FLOW_TRACE and ( NAME [ 1 ] <> '$' ) then
                     LNK := - PCOUNTER * 2
                   else
                     if VPOS > 0 then
                       LNK := BASE_DSPLMT ( VPOS )
                     else
                       LNK := BASE_DSPLMT ( PCOUNTER ) ;
                   repeat
                     QPC := CODE . H [ TPC ] ;
                     CODE . H [ TPC ] := TO_HINT ( LNK ) ;
                     TPC := QPC ;
                   until TPC = 0 ;
                   if VPOS > 0 then
                     LNK := VPOS
                   else
                     begin
                       LNK := PCOUNTER ;
                       CODE . I [ PCOUNTER DIV 2 ] := 0 ;
                       PCOUNTER := NEXTPC ( 2 ) ;
                     end (* else *)
                 end (* then *) ;
           TPC := PCOUNTER ;

           (************************************************)
           (* SET Proc SIZE FIELD at position posofproclen *)
           (* for debug purposes                           *)
           (* ... and stacksize                            *)
           (************************************************)

           CODESIZE := PCOUNTER * 2 ;
           CODE . H [ POSOFPROCLEN DIV 2 ] := TO_HINT ( CODESIZE ) ;
           CODE . H [ POSOFPROCLEN DIV 2 - 2 ] := TO_HINT ( STACKSIZE )
                                                  ;
           if PIAKT -> . DEBUG_LEV > 0 then
             begin
               repeat
                 ADDLNP ( 255 )
               until NXTLNP MOD 4 = 0 ;
             end (* then *) ;

           (*********************)
           (*SHORT PROC TOO LONG*)
           (*********************)

           if not PIAKT -> . LARGE_PROC then
             if PCOUNTER > 4096 then
               ERROR ( 609 ) ;

           (****************************)
           (* OUTPUT THE OBJECT CODE   *)
           (****************************)

           for I := 1 to 20 do
             CARD . I [ I ] := BLNK4 ;
           BLNK80 := CARD . C ;
           PACK ( BLNK80 , 1 , BLNK64 ) ;

           (****************************)
           (* OUTPUT THE 'ESD' ENTRIES *)
           (****************************)

           if CURLVL = 1 then
             if not MUSIC then
               begin
                 PRCTBL [ NXTPRC ] . NAME := '$PASENT ' ;
                 NXTPRC := NXTPRC + 1
               end (* then *) ;
           with ESD_CARD do
             begin
               I4 [ 1 ] := XESD ;
               I4 [ 2 ] := BLNK4 ;
               C64 := BLNK64 ;
               I := 0 ;
               J := 0 ;
               K := BLNK2 * SL16 + 1 ;
               repeat
                 J := J + 1 ;
                 with ITEM [ J ] , PRCTBL [ I ] do
                   begin
                     XNAME := NAME ;
                     if I < NXTPRC then
                       if I = 0 then

           (**********************)
           (* NAME OF THIS CSECT *)
           (**********************)

                         begin
                           F1 := 0 ;
                           F2 := BLNK1 * SL24 + PCOUNTER * 2 + NXTLNP ;

           (************)
           (*CSECT SIZE*)
           (************)

                         end (* then *)
                       else

           (**********************)
           (* EXTERNAL REFERENCE *)
           (**********************)

                         begin
                           F1 := 2 * SL24 ;
                           F2 := BLNK4 ;
                         end (* else *)
                     else

           (********************)
           (* LABEL DEFINITION *)
           (********************)

                       begin
                         F1 := 1 * SL24 + LNK * 2 ;
                         F2 := BLNK1 * SL24 + 1 ;
                       end (* else *)
                   end (* with *) ;
                 I := I + 1 ;
                 if I = NXTPRC then
                   I := NXTEP + 1 ;
                 if ( J = 3 ) or ( I > PRCCNT ) then
                   begin
                     I4 [ 3 ] := BLNK2 * SL16 + J * 16 ;
                     I4 [ 4 ] := K ;
                     WRITE ( OBJCODE , C80 ) ;
                     if I < NXTPRC then
                       K := K + 3
                     else
                       K := BLNK4 ;
                     C64 := BLNK64 ;
                     J := 0
                   end (* then *) ;
               until I > PRCCNT ;
             end (* with *) ;
           if CURLVL = 1 then
             if not MUSIC then
               NXTPRC := NXTPRC - 1 ;

           (****************************)
           (* OUTPUT THE 'TXT' CARDS   *)
           (****************************)

           CARD . I [ 1 ] := XTXT ;
           CARD . I [ 2 ] := BLNK1 * SL24 + 0 ;
           CARD . I [ 3 ] := BLNK2 * SL16 + SIZE_TXTCHUNK ;
           CARD . I [ 4 ] := BLNK2 * SL16 + 01 ;
           TPC := MXCODE ;
           QPC := TPC + NXTLNP DIV 2 ;
           while TPC < QPC do
             begin
               CODE . H [ PCOUNTER ] := TO_HINT ( CODE . H [ TPC ] ) ;
               PCOUNTER := PCOUNTER + 1 ;
               TPC := TPC + 1 ;
             end (* while *) ;
           TPC := 0 ;
           I := 0 ;
           QPC := PCOUNTER * 2 ;
           LNGTH := SIZE_TXTCHUNK ;
           while TPC < QPC do
             begin
               if ( QPC - TPC ) < SIZE_TXTCHUNK then
                 begin
                   LNGTH := QPC - TPC ;
                   CARD . H [ 6 ] := LNGTH ;
                 end (* then *) ;
               CARD . H [ 4 ] := TPC ;
               WRITE ( OBJCODE , CARD . C : 16 , CODE . TXTCARD [ I ] :
                       LNGTH , ' ' : 64 - LNGTH ) ;
               I := I + 1 ;
               TPC := TPC + SIZE_TXTCHUNK ;
             end (* while *) ;

           (****************************)
           (* OUTPUT THE 'RLD' ENTRIES *)
           (****************************)

           CARD . C := BLNK80 ;
           CARD . I [ 1 ] := XRLD ;
           I := 0 ;
           LNGTH := 0 ;
           repeat

           (*************************************)
           (* SCAN OVER ALL EXTERNAL REFERENCES *)
           (*************************************)

             with PRCTBL [ I ] do
               begin
                 I := I + 1 ;

           (*********************************************)
           (* I NOW BECOMES ESDID FOR THE CURRENT ENTRY *)
           (*********************************************)

                 if LNK > 0 then

           (**************************)
           (* IMPLIES RECURSIVE CALL *)
           (**************************)

                   begin
                     CARD . I [ LNGTH + 5 ] := I * SL16 + 01 ;

           (**********************)
           (* 'P#', 'R#' FIELDS  *)
           (**********************)

                     CARD . I [ LNGTH + 6 ] := 28 * SL24 + LNK * 2 ;

           (**********************)
           (* ADCON DISPLACEMENT *)
           (**********************)

                     LNGTH := LNGTH + 2 ;
                     if ( LNGTH >= 14 ) or ( I >= NXTPRC ) then

           (*********************)
           (* OUTPUT THE BUFFER *)
           (*********************)

                       begin
                         CARD . H [ 6 ] := LNGTH * 4 ;

           (***********************)
           (* # OF RLD DATA BYTES *)
           (***********************)

                         while LNGTH < 14 do
                           begin
                             CARD . I [ LNGTH + 5 ] := BLNK4 ;
                             LNGTH := LNGTH + 1
                           end (* while *) ;
                         WRITE ( OBJCODE , CARD . C ) ;
                         LNGTH := 0 ;
                       end (* then *) ;
                   end (* then *) ;
               end (* with *)
           until I >= NXTPRC ;

           (*********************)
           (* OUTPUT 'END' CARD *)
           (*********************)

           CARD . C := BLNK80 ;
           CARD . I [ 1 ] := XEND ;
           if CURLVL = 1 then
             if not MUSIC then
               begin
                 CARD . I [ 2 ] := BLNK1 * SL24 ;
                 CARD . H [ 8 ] := NXTPRC + 1
               end (* then *) ;
           WRITE ( OBJCODE , CARD . C : 32 , 'PASCAL:' : 7 , DATE : 11
                   , ' ' : 30 ) ;
           if ASM then
             PRINT_CSECT ( PCOUNTER ) ;
           if PIAKT -> . ASMVERB then
             begin
               WRITELN ( OUTPUT , '****' : 7 , ' PROC: ' , PRCTBL [ 0 ]
                         . NAME , '; ' , PIAKT -> . CODE_SIZE : 1 ,
                         ' P-STMTS, ' , PCOUNTER * 2 : 1 , ' BYTES, ' ,
                         NXTPRC - 1 : 1 , ' EXT. REFS., ' , NUMLITS : 1
                         , ' CONSTANTS, ' , POOL_SIZE : 1 ,
                         ' BYTES OF CONSTANTS.' ) ;
               WRITELN ( OUTPUT ) ;
             end (* then *) ;
           TOTALBYTES := TOTALBYTES + QPC ;
           10 :
           
         end (* GEN_CSECT *) ;


      procedure DUMPCONSTBLK ( CLOSE : BOOLEAN ) ;

         var CPC1 , LEN , I , J : HINTEGER ;
             TXTNUM : 0 .. 150 ;

         begin (* DUMPCONSTBLK *)
           if CSEGSTRT = 0 then

           (**************)
           (* FIRST CALL *)
           (**************)

             begin

           (***********************************)
           (* PUT OUT ESD CARD TO BEGIN CSECT *)
           (***********************************)

               WRITE ( OBJCODE , CHR ( 02 ) , 'ESD      ' , CHR ( 0 ) ,
                       CHR ( 16 ) , '  ' , CHR ( 0 ) , CHR ( 1 ) ,
                       PRCTBL [ 0 ] . NAME , CHR ( 0 ) , CHR ( 0 ) ,
                       CHR ( 0 ) , CHR ( 0 ) , ' ' , CHR ( 0 ) , CHR (
                       0 ) , CHR ( 0 ) , ' ' : 48 ) ;
             end (* then *) ;
           CPC1 := CSEGSTRT ;
           TXTNUM := 0 ;
           LEN := SIZE_TXTCHUNK ;
           while CPC1 < CPCOUNTER do
             begin
               if ( CPCOUNTER - CPC1 ) < SIZE_TXTCHUNK then
                 LEN := CPCOUNTER - CPC1 ;
               if ( LEN = SIZE_TXTCHUNK ) or CLOSE then
                 WRITE ( OBJCODE , CHR ( 02 ) , 'TXT ' , CHR ( 0 ) ,
                         CHR ( CPC1 DIV 256 ) , CHR ( CPC1 MOD 256 ) ,
                         '  ' , CHR ( 0 ) , CHR ( LEN ) , '  ' , CHR (
                         0 ) , CHR ( 1 ) , CODE . TXTCARD [ TXTNUM ] :
                         LEN , ' ' : 64 - LEN ) ;
               TXTNUM := TXTNUM + 1 ;
               CPC1 := CPC1 + LEN ;
             end (* while *) ;
           if CLOSE then

           (*******************************)
           (* LAST CALL, PUT OUT END CARD *)
           (*******************************)

             begin
               WRITE ( OBJCODE , CHR ( 02 ) , 'END' , ' ' : 24 , CHR (
                       0 ) , CHR ( 0 ) , CHR ( CPC1 DIV 256 ) , CHR (
                       CPC1 MOD 256 ) , ' ' : 48 ) ;
               if CST_ASMVERB then
                 begin
                   WRITELN ( OUTPUT , '****' : 7 , ' CONSTS: ' , PRCTBL
                             [ 0 ] . NAME , '; ' , CPC1 : 1 , ' BYTES.'
                             ) ;
                   WRITELN ( OUTPUT ) ;
                 end (* then *) ;
             end (* then *)
           else
             begin
               J := CPC1 - LEN - CSEGSTRT ;
               CSEGSTRT := CPC1 - LEN ;
               I := 0 ;
               while I < LEN do
                 begin
                   CODE . C [ I ] := CODE . C [ J + I ] ;
                   I := I + 1
                 end (* while *) ;
               CSEGLIMIT := CSEGSTRT + SIZE_TXTCHUNK * 145 ;
             end (* else *) ;
         end (* DUMPCONSTBLK *) ;


      procedure ENT_RET ;

         var STATIC_ADDR : ADRRNG ;
             OFFS_WORK : ADRRNG ;
             SIZE_REST : ADRRNG ;

         begin (* ENT_RET *)
           PROCOFFSET_OLD := 0 ;
           if PCODE = PENT then
             begin

           (***********************************************************)
           (* ON ENTRY TRG1 POINTS TO DATA AREA                       *)
           (* for the called routine                                  *)
           (***********************************************************)

               CURLVL := P ;
               INIT_CSECT ;

           (*********************************)
           (*INITIALIZE NEW CSECT PARAMETERS*)
           (*********************************)

               STATIC_ADDR := PCOUNTER * 2 - 4 ;

           //***********************************************************
           // if there are local calls, the display value at the        
           // current static level has to saved and restored at         
           // the end ... load it to R0, it will be saved by the        
           // following stm 14,12,...                                   
           //***********************************************************

               if PIAKT -> . CALL_HIGHER then
                 begin
                   if ASM then
                     begin
                       HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                       WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
                       WRITE ( ASMOUT , '*' , ' ' : 26 ,
                               '-- save display level ' , CURLVL : 1 )
                               ;
                       WRITELN ( ASMOUT ) ;
                     end (* then *) ;
                   GENRX ( XL , TRG0 , DISPLAY + 4 * CURLVL , GBR , 0 )
                           ;
                 end (* then *) ;

           (***************************)
           (* TO SAVE DISPLAY[CURLVL] *)
           (***************************)

               if ASM then
                 begin
                   HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                   WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
                   WRITE ( ASMOUT , '*' , ' ' : 26 ,
                           '-- save registers and chain areas' ) ;
                   WRITELN ( ASMOUT ) ;
                 end (* then *) ;
               GENRS ( XSTM , 14 , 12 , 12 , TRG1 ) ;

           (*********************************)
           (*SAVE OLD DISPLAY[CURLVL] & REGS*)
           (*********************************)

               GENRX ( XST , TRG1 , 8 , LBR , 0 ) ;

           (*****************************)
           (*FORWARD CHAIN OF SAVE AREAS*)
           (*****************************)

               GENRX ( XST , LBR , 4 , TRG1 , 0 ) ;

           (************************************)
           (*DYNAMIC LINK, ALSO SAVE AREA CHAIN*)
           (************************************)
           (***************************        *)
           (* SAVE DYNAMIC LINK + REGS         *)
           (***************************        *)

               GENRR ( XLR , LBR , TRG1 ) ;

           (*****************)
           (*UPDATE THE 'MP'*)
           (*****************)

               if PIAKT -> . CALL_HIGHER then
                 begin
                   if ASM then
                     begin
                       HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                       WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
                       WRITE ( ASMOUT , '*' , ' ' : 26 ,
                               '-- update current display' ) ;
                       WRITELN ( ASMOUT ) ;
                     end (* then *) ;
                   GENRX ( XST , LBR , DISPLAY + 4 * CURLVL , GBR , 0 )
                           ;
                 end (* then *) ;

           (************************)
           (*UPDATE DISPLAY[CURLVL]*)
           (************************)

               if ASM then
                 begin
                   HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                   WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
                   WRITE ( ASMOUT , '*' , ' ' : 26 ,
                           '-- setup base registers' ) ;
                   WRITELN ( ASMOUT ) ;
                 end (* then *) ;
               GENRR ( XLR , PBR1 , JREG ) ;

           (*********************************)
           (* SET UP PROGRAM BASE REGISTERS *)
           (*********************************)

               if PIAKT -> . LARGE_PROC then
                 GENRX ( XLA , PBR2 , 4092 , PBR1 , 0 ) ;
               if DEBUG or MUSIC then
                 begin
                   if ASM then
                     begin
                       HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                       WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
                       WRITE ( ASMOUT , '*' , ' ' : 26 ,
                               '-- check for enough stack space' ) ;
                       WRITELN ( ASMOUT ) ;
                     end (* then *) ;
                   if PIAKT -> . DATA_SIZE < 4096 then
                     GENRX ( XLA , TRG1 , PIAKT -> . DATA_SIZE , TRG1 ,
                             0 )
                   else
                     GENRXLAB ( XA , TRG1 , PIAKT -> . SEGSZE , - 1 ) ;
                   GENRX ( XC , TRG1 , NEWPTR , GBR , 0 ) ;

           //************************************************
           // COMPARE 'SP' AND 'NP'                          
           //************************************************

                   GENRX ( XBC , GEQCND , STKCHK , GBR , 0 ) ;

           //************************************************
           // BRANCH TO ERROR ?                              
           //************************************************

                   if DEBUG then
                     if CURLVL = 1 then
                       begin

           //********************************************************
           // ENTERING PASMAIN, CLEAR STACK/HEAP AREA                
           //********************************************************

                         if ASM then
                           begin
                             HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                             WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
                             WRITE ( ASMOUT , '*' , ' ' : 26 ,
                                     '-- clear stack/heap area' ) ;
                             WRITELN ( ASMOUT ) ;
                           end (* then *) ;

           //********************************************************
           // L     15,72(12)    end of stack area                   
           // LA    14,400(12)   first global variable address       
           // SR    15,14        length to initialize in R15         
           // XR    0,0          source adr zero                     
           // LA    1,129        source len zero                     
           // SLL   1,24         pattern x'81' in leftmost 8 bytes   
           // MVCL  14,0         move init pattern                   
           //********************************************************

                         GENRX ( XL , TRG15 , NEWPTR , GBR , 0 ) ;
                         GENRX ( XLA , TRG14 , FRSTGVAR , GBR , 0 ) ;
                         GENRR ( XSR , TRG15 , TRG14 ) ;
                         GENRR ( XXR , TRG0 , TRG0 ) ;
                         GENRX ( XLA , TRG1 , 129 , 0 , 0 ) ;
                         GENRS ( XSLL , TRG1 , 0 , 24 , 0 ) ;
                         GENRR ( XMVCL , TRG14 , TRG0 ) ;
                       end (* then *) ;
                 end (* then *) ;
               CSPREGACTIVE := FALSE ;
               PROCOFFSET_OLD := 0 ;
             end (* then *)
           else

           (************************************************)
           (* pcode = PRET                                 *)
           (************************************************)

             begin

           (***********************************************)
           (*RESTORES DISPLAY[CURLVL] AND MP, THEN RETURNS*)
           (***********************************************)

               if DEBUG and ( CURLVL > 1 ) and ( PIAKT -> . DATA_SIZE >
               80 ) then
                 if PIAKT -> . DATA_SIZE < 1500 then
                   begin
                     if ASM then
                       begin
                         HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                         WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
                         WRITE ( ASMOUT , '*' , ' ' : 26 ,
                                 '-- clear stack frame using MVCs' ) ;
                         WRITELN ( ASMOUT ) ;
                       end (* then *) ;

           //************************************************
           // optimizing: generate MVC instead of MVCL       
           // MVI 80(13),X'81'                               
           // MVC 81(256,13),80(13) ...                      
           //************************************************

                     GENSI ( XMVI , LCAFTMST , LBR , 0x81 ) ;
                     OFFS_WORK := LCAFTMST + 1 ;
                     SIZE_REST := PIAKT -> . DATA_SIZE - LCAFTMST - 1 ;
                     while SIZE_REST > 256 do
                       begin
                         GENSS ( XMVC , 256 , OFFS_WORK , LBR ,
                                 OFFS_WORK - 1 , LBR ) ;
                         OFFS_WORK := OFFS_WORK + 256 ;
                         SIZE_REST := SIZE_REST - 256
                       end (* while *) ;
                     GENSS ( XMVC , SIZE_REST , OFFS_WORK , LBR ,
                             OFFS_WORK - 1 , LBR ) ;
                   end (* then *)
                 else
                   begin
                     if ASM then
                       begin
                         HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                         WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
                         WRITE ( ASMOUT , '*' , ' ' : 26 ,
                                 '-- clear stack frame using MVCL' ) ;
                         WRITELN ( ASMOUT ) ;
                       end (* then *) ;

           //******************************
           // clear the stack frame        
           // that is:                     
           // LA    0,80(13)               
           // length into R1               
           // XR    R14,R14                
           // LA    R15,X'81'              
           // SLL   R15,24                 
           // MVCL  R0,R14                 
           //******************************

                     GENRX ( XLA , TRG0 , LCAFTMST , LBR , 0 ) ;
                     if PIAKT -> . DATA_SIZE < 4096 then
                       GENRX ( XLA , TRG1 , PIAKT -> . DATA_SIZE -
                               LCAFTMST , 0 , 0 )
                     else
                       begin
                         GENRXLAB ( XL , TRG1 , PIAKT -> . SEGSZE , - 1
                                    ) ;
                         GENRXLIT ( XS , TRG1 , LCAFTMST - REALSIZE , 0
                                    ) ;
                       end (* else *) ;
                     GENRR ( XXR , TRG14 , TRG14 ) ;
                     GENRX ( XLA , TRG15 , 0x81 , 0 , 0 ) ;
                     GENRX ( XSLL , TRG15 , 24 , 0 , 0 ) ;
                     GENRR ( XMVCL , TRG0 , TRG14 ) ;
                   end (* else *) ;

           //************************************************
           // restore the general registers                  
           //************************************************

               if ASM then
                 begin
                   HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                   WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
                   WRITE ( ASMOUT , '*' , ' ' : 26 ,
                           '-- restore registers' ) ;
                   WRITELN ( ASMOUT ) ;
                 end (* then *) ;
               GENRS ( XLM , 14 , 12 , 12 , LBR ) ;
               GENRX ( XL , LBR , 4 , LBR , 0 ) ;

           //************************************************
           // restore the display value at the               
           // current static level; it has been reloaded from
           // the                                            
           // save area to R0 ...                            
           //************************************************

               if PIAKT -> . CALL_HIGHER then
                 begin
                   if ASM then
                     begin
                       HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                       WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
                       WRITE ( ASMOUT , '*' , ' ' : 26 ,
                               '-- restore display level ' , CURLVL : 1
                               ) ;
                       WRITELN ( ASMOUT ) ;
                     end (* then *) ;
                   GENRX ( XST , TRG0 , DISPLAY + 4 * CURLVL , GBR , 0
                           ) ;
                 end (* then *) ;
               if DEBUG and ( CURLVL > 1 ) then

           (***********************)
           (* CLEAR THE SAVE AREA *)
           (***********************)

                 begin
                   if ASM then
                     begin
                       HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                       WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
                       WRITE ( ASMOUT , '*' , ' ' : 26 ,
                               '-- clear the save area' ) ;
                       WRITELN ( ASMOUT ) ;
                     end (* then *) ;
                   I := 80 ;
                   if OPNDTYPE <> PROC then
                     I := 72 ;
                   GENSS ( XMVC , I , 0 , TRG1 , 80 , TRG1 ) ;
                 end (* then *) ;
               if ASM then
                 begin
                   HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                   WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
                   WRITE ( ASMOUT , '*' , ' ' : 26 ,
                           '-- branch to return address' ) ;
                   WRITELN ( ASMOUT ) ;
                 end (* then *) ;
               if FLOW_TRACE then
                 begin
                   GENRR ( XLR , 0 , RTREG ) ;
                   GENRX ( XBAL , RTREG , TRACER , GBR , 0 ) ;
                   if ASM then
                     begin
                       HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                       WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
                       WRITELN ( ASMOUT , ' DC AL2(0)' ) ;
                     end (* then *) ;
                   CODE . H [ PCOUNTER ] := 0 ;
                   PCOUNTER := NEXTPC ( 1 ) ;
                 end (* then *)
               else
                 GENRR ( XBCR , ANYCND , RTREG ) ;
               RELEASE ( HEAPMARK ) ;
               if CKMODE then
                 CHECKFREEREGS ;
             end (* else *)
         end (* ENT_RET *) ;


      procedure DEF_OPERATION ;

         begin (* DEF_OPERATION *)

           /***************************************************/
           /* pdef steht entweder am Ende einer Prozedur      */
           /* bzw. funktion und gibt deren laenge an          */
           /* oder am anfang einer branch table,              */
           /* dann zweimal fuer lower und upper limit         */
           /***************************************************/

           if GS . MOD2DEFSTEP >= 0 then
             begin

           /***************************************************/
           /* ende einer prozedur ... hier folgen             */
           /* ab 2018.03 noch mehrere defs, die weitere       */
           /* informationen fuer die prozedur enthalten       */
           /***************************************************/

               case GS . MOD2DEFSTEP of
                 0 : GS . MOD2DEFSTEP := 1 ;
                 1 : GS . MOD2DEFSTEP := 2 ;
                 2 : begin
                       GS . MOD2DEFSTEP := - 1 ;
                       PDEF_CNT := 0 ;
                       GEN_CSECT ( Q ) ;
                       GS . FILL_LINEPTR := FALSE ;
                       PIAKT := NIL ;
                       PCOUNTER := 0 ;
                     end (* tag/ca *)
               end (* case *) ;
               return
             end (* then *) ;

           /***************************************************/
           /* am anfang einer branch table,                   */
           /* dann zweimal fuer lower und upper limit         */
           /***************************************************/

           if LBL1 . LEN > 0 then
             begin
               PDEF_CNT := PDEF_CNT + 1 ;

           /*************************************/
           /* CTR/CASE EXPRESSION RANGE,        */
           /* PUT BOUNDS IN 'CONSTANT' TABLE    */
           /* but not for constants             */
           /* in new portable branch table      */
           /*************************************/

               UPD_INTTBL ( LBLTBL [ LBLMAP ( LBL1 . NAM ) ] . LNK , Q
                            ) ;
             end (* then *)
           else
             begin
               PDEF_CNT := PDEF_CNT + 1 ;
               if PDEF_CNT = 1 then
                 begin
                   CASE_LOW := Q ;
                   CASE_HIGH := Q ;
                 end (* then *) ;
               if not CASE_FLAG_NEW then

           /**********************************/
           /* portable branch table beginnt  */
           /**********************************/

                 begin
                   CASE_FLAG := TRUE ;
                   CASE_FLAG_NEW := TRUE ;
                   CASE_DEFAULT := LBLMAP ( LBL1 . NAM ) + 1 ;
                   CASE_OPNDTYPE := OPNDTYPE ;

           /**********************************/
           /* pre-format area of branch-     */
           /* table with zeroes              */
           /**********************************/

                   PCNEU := NEXTPC ( CIXMAX ) ;
                   for PC := PCOUNTER to PCNEU do
                     CODE . H [ PC ] := 0 ;
                   if CASE_OPNDTYPE = CHRC then
                     for C := CHR ( 0 ) to CHR ( 255 ) do
                       CASE_CHARTABLE [ C ] := - 1 ;
                   if FALSE then
                     begin
                       WRITELN ( TRACEF , '---------------------'
                                 '---------------------' ) ;
                       WRITELN ( TRACEF , 'pcounter      = ' , PCOUNTER
                                 ) ;
                       WRITELN ( TRACEF , 'case_low      = ' , CASE_LOW
                                 ) ;
                       WRITELN ( TRACEF , 'case_flag     = ' ,
                                 CASE_FLAG ) ;
                       WRITELN ( TRACEF , 'case_flag_new = ' ,
                                 CASE_FLAG_NEW ) ;
                       WRITELN ( TRACEF , 'case_default  = ' ,
                                 CASE_DEFAULT ) ;
                       WRITELN ( TRACEF , 'case_opndtype = ' ,
                                 CASE_OPNDTYPE ) ;
                       WRITELN ( TRACEF , '---------------------'
                                 '---------------------' ) ;
                     end (* then *) ;
                 end (* then *) ;
               CASE_LABEL := Q ;
               if FALSE then
                 begin
                   WRITELN ( TRACEF , 'case_label = ' , CASE_LABEL ) ;
                   WRITELN ( TRACEF , '---------------------'
                             '---------------------' ) ;
                 end (* then *)
             end (* else *)
         end (* DEF_OPERATION *) ;


      procedure LAB_OPERATION ;

         begin (* LAB_OPERATION *)
           if CASE_FLAG then
             begin
               if CASE_FLAG_NEW then

           /****************************************/
           /* portable branch table komplettieren  */
           /* und pcounter hochsetzen              */
           /****************************************/

                 begin
                   PCNEU := NEXTPC ( CASE_HIGH - CASE_LOW ) ;

           /****************************************/
           /* im fall char erst jetzt alle         */
           /* adresskonstanten anhand von          */
           /* case_chartable erzeugen - weil erst  */
           /* jetzt case_low und case_high         */
           /* festliegen                           */
           /****************************************/

                   if CASE_OPNDTYPE = CHRC then
                     begin
                       CASE_LAUF := CASE_LOW ;
                       for PC := PCOUNTER to PCNEU do
                         begin
                           CL := CASE_CHARTABLE [ CHR ( CASE_LAUF ) ] ;
                           if CL >= 0 then
                             begin
                               MKLBL ( LBL_WORK , CL ) ;
                               GENAL2 ( PC , LBL_WORK ) ;
                             end (* then *)
                           else
                             begin
                               MKLBL ( LBL_WORK , CASE_DEFAULT ) ;
                               GENAL2 ( PC , LBL_WORK ) ;
                             end (* else *) ;
                           CASE_LAUF := CASE_LAUF + 1 ;
                         end (* for *) ;
                     end (* then *)

           /****************************************/
           /* andernfalls war vorher schon alles   */
           /* klar (case_low lag schon fest,       */
           /* erste def_konstante) und jetzt sind  */
           /* nur noch die luecken zu fuellen      */
           /****************************************/

                   else
                     begin
                       for PC := PCOUNTER to PCNEU do
                         begin
                           if CODE . H [ PC ] = 0 then
                             begin
                               MKLBL ( LBL_WORK , CASE_DEFAULT ) ;
                               GENAL2 ( PC , LBL_WORK ) ;
                             end (* then *)
                         end (* for *)
                     end (* else *) ;
                   PCOUNTER := PCNEU ;
                   PCOUNTER := NEXTPC ( 1 ) ;

           (***********************************************)
           (* Konstanten bei neuer portabler Branch Table *)
           (* als literale ablegen                        *)
           (***********************************************)

                   UPD_INTTBL ( LBLTBL [ CASE_DEFAULT - 3 ] . LNK ,
                                CASE_LOW ) ;
                   if ASM then
                     begin
                       MKLBL ( LBLX , CASE_DEFAULT - 3 ) ;
                       HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                       WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
                       WRITELN ( ASMOUT , LBLX . NAM , ' EQU ' ,
                                 CASE_LOW : 1 ) ;
                     end (* then *) ;
                   UPD_INTTBL ( LBLTBL [ CASE_DEFAULT - 2 ] . LNK ,
                                CASE_HIGH ) ;
                   if ASM then
                     begin
                       MKLBL ( LBLX , CASE_DEFAULT - 2 ) ;
                       HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                       WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
                       WRITELN ( ASMOUT , LBLX . NAM , ' EQU ' ,
                                 CASE_HIGH : 1 ) ;
                     end (* then *) ;
                   CASE_FLAG_NEW := FALSE ;
                 end (* then *) ;
               PDEF_CNT := 0 ;
               CASE_FLAG := FALSE ;
             end (* then *) ;

           (***********************)
           (* END OF BRANCH TABLE *)
           (***********************)

           if ASM then
             begin
               HEXHW ( PCOUNTER * 2 , HEXPC ) ;
               WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
               X1 := 8 ;
               while LBL1 . NAM [ X1 ] = ' ' do
                 X1 := X1 - 1 ;
               if X1 < 5 then
                 X1 := 5 ;
               for X2 := 1 to X1 do
                 WRITE ( ASMOUT , LBL1 . NAM [ X2 ] ) ;
               WRITELN ( ASMOUT , ' DS    0H' ) ;
             end (* then *) ;

           (******************)
           (*LABEL DEFINITION*)
           (******************)

           UPD_LBLTBL ( PCOUNTER , LBLMAP ( LBL1 . NAM ) , TRUE , FALSE
                        ) ;

           (******************************************)
           (* if old opcode = PDEF and pdef_cnt = 2, *)
           (* start of branch table                  *)
           (******************************************)

           if FALSE then
             begin
               WRITELN ( TRACEF , 'oldpcode      = ' , OLDPCODE ) ;
               WRITELN ( TRACEF , 'pdef_cnt      = ' , PDEF_CNT ) ;
             end (* then *) ;
           CASE_FLAG := ( OLDPCODE = PDEF ) and ( PDEF_CNT = 2 ) ;

           (**********************)
           (* some inits         *)
           (**********************)

           CSPREGACTIVE := FALSE ;
           PROCOFFSET_OLD := 0 ;
           TXR_CONTENTS . VALID := FALSE ;
           LAST_CC . LPC := 0 ;
           LAST_STR . LPC := 0 ;
           LAST_FILE . LPC := 0 ;
           LAST_MVC . LPC := 0 ;
           if CKMODE then
             CHECKFREEREGS ;
         end (* LAB_OPERATION *) ;


      begin (* COPERATION *)
        case PCODE of

        (************************)
        (* P_MACHINE PSEUDO OPS *)
        (************************)

          PXLB : begin
                   GENRELRX ( XBC , ANYCND , 14 ) ;

        (********************************)
        (* B *+28, SKIP OVER ENTRY CODE *)
        (********************************)

                   with PRCTBL [ NXTEP ] do
                     begin
                       NAME := LBL1 . NAM ;
                       LNK := PCOUNTER
                     end (* with *) ;
                   if NXTEP > NXTPRC then
                     NXTEP := NXTEP - 1
                   else
                     ERROR ( 256 ) ;

        (**************************)
        (* COLLISION OF TWO LISTS *)
        (**************************)

                   GENRR ( XBALR , RTREG , 0 ) ;

        (************************************)
        (* FORCE A BASE REG. FOR NEXT INST. *)
        (************************************)

                   GENRX ( XBAL , PBR1 , 6 , RTREG , 0 ) ;
                   CODE . H [ PCOUNTER ] := TO_HINT ( PCOUNTER * 2 ) ;
                   PCOUNTER := NEXTPC ( 1 ) ;
                   GENRX ( XLA , PBR1 , 4 , RTREG , 0 ) ;

        (*******************)
        (* CLEAR HIGH BYTE *)
        (*******************)

                   GENRX ( XSH , PBR1 , 4 , RTREG , 0 ) ;
                   if PIAKT -> . LARGE_PROC then
                     GENRX ( XLA , PBR2 , 4092 , PBR1 , 0 )
                   else
                     GENRX ( XBC , NOCND , 0 , 0 , 0 ) ;
                   GENRX ( XL , LBR , DISPLAY + 4 * CURLVL , GBR , 0 )
                           ;

        (******************************************************)
        (* PLAB INSTR. IS NEXT ==> NO NEED TO RESET ANY FLAGS *)
        (******************************************************)

                 end (* tag/ca *) ;
          PLAB : LAB_OPERATION ;
          PLOC : begin
                   if GS . FILL_LINEPTR then
                     begin

        (***************************************)
        (* FILL THE ENTRIES OF LINE PTR TABLE  *)
        (***************************************)

                       if PIAKT -> . DEBUG_LEV > 0 then
                         for I := LASTLN to Q - 1 do
                           begin
                             UPDLNTBL ( PCOUNTER - LASTPC ) ;
                             LASTPC := PCOUNTER ;
                           end (* for *) ;
                     end (* then *) ;
                   LASTLN := Q ;
                   PCODE := OLDPCODE ;

        (***************************)
        (* TO TREAT THIS AS A NOOP *)
        (***************************)

                 end (* tag/ca *) ;
          PDEF : DEF_OPERATION ;

        (*******************************)
        (* BRANCH/CONTROL INSTRUCTIONS *)
        (*******************************)

          PUJP : begin
                   if FLOW_TRACE and not CASE_FLAG then
                     begin
                       GENRX ( XBAL , RTREG , TRACER , GBR , 0 ) ;
                       if ASM then
                         begin
                           HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                           WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
                           WRITELN ( ASMOUT , ' DC AL2(' , LBL2 . NAM :
                                     LBL2 . LEN , '-' , PRCTBL [ 0 ] .
                                     NAME , ')' ) ;
                         end (* then *) ;
                       UPD_LBLTBL ( PCOUNTER , LBLMAP ( LBL2 . NAM ) ,
                                    FALSE , TRUE ) ;
                       PCOUNTER := NEXTPC ( 1 ) ;
                     end (* then *)
                   else
                     if not CASE_FLAG_NEW then
                       begin
                         GENRXLAB ( XBC , 15 , LBL2 , 0 )
                       end (* then *)
                     else
                       begin
                         if CASE_LOW > CASE_LABEL then
                           CASE_LOW := CASE_LABEL ;
                         if CASE_HIGH < CASE_LABEL then
                           CASE_HIGH := CASE_LABEL ;
                         if CASE_OPNDTYPE = CHRC then
                           begin
                             CASE_CHARTABLE [ CHR ( CASE_LABEL ) ] :=
                                                   LBLMAP ( LBL2 . NAM
                                                   ) ;
                           end (* then *)
                         else
                           begin
                             PC := PCOUNTER + CASE_LABEL - CASE_LOW ;
                             GENAL2 ( PC , LBL2 )
                           end (* else *) ;
                       end (* else *) ;
                 end (* tag/ca *) ;
          PUXJ : begin
                   if PIAKT -> . CALL_HIGHER then
                     begin
                       GENRX ( XL , TRG0 , 20 , LBR , 0 ) ;
                       GENRX ( XST , TRG0 , DISPLAY + 4 * CURLVL , GBR
                               , 0 ) ;
                     end (* then *) ;
                   if FLOW_TRACE then
                     begin
                       GENRXLAB ( XL , TRG0 , LBL2 , - 3 ) ;
                       GENRX ( XBAL , RTREG , TRACER , GBR , 0 ) ;
                       CODE . H [ PCOUNTER ] := 0 ;
                       PCOUNTER := NEXTPC ( 1 ) ;
                     end (* then *)
                   else
                     begin
                       GENRXLAB ( XL , RTREG , LBL2 , - 3 ) ;
                       GENRR ( XBCR , ANYCND , RTREG ) ;
                     end (* else *) ;
                   PCODE := PUJP ;
                 end (* tag/ca *) ;
          PFJP : begin
                   TOP := TOP - 1 ;
                   if ( BRCND >= 0 ) and ( not NEG_CND ) then

        (***********************)
        (* COND. CODE IS ALIVE *)
        (***********************)

                     BRCND := 15 - BRCND
                   else
                     with STK [ TOP ] do
                       begin
                         if VRBL then
                           begin
                             if DRCT and ( VPA = MEM ) then
                               begin
                                 GETOPERAND ( STK [ TOP ] , Q1 , P1 ,
                                              B1 ) ;
                                 if B1 > 0 then
                                   if P1 > 0 then
                                     GENRR ( XAR , P1 , B1 )
                                   else
                                     P1 := B1 ;
                                 GENSI ( XTM , Q1 , P1 , 1 ) ;
                                 BRCND := 8 ;

        (********)
        (* BZ   *)
        (********)

                                 if NEG_CND then
                                   BRCND := 1 ;

        (********)
        (* BO   *)
        (********)

                               end (* then *)
                             else
                               if not DRCT then
                                 begin
                                   GETADR ( STK [ TOP ] , Q1 , P1 , B1
                                            ) ;
                                   if B1 > 0 then
                                     if P1 > 0 then
                                       GENRR ( XAR , P1 , B1 )
                                     else
                                       P1 := B1 ;
                                   GENSI ( XTM , Q1 , P1 , 1 ) ;
                                   BRCND := 8 ;
                                   if NEG_CND then
                                     BRCND := 1 ;
                                 end (* then *)
                               else
                                 begin
                                   LOAD ( STK [ TOP ] ) ;
                                   GENRR ( XLTR , RGADR , RGADR ) ;
                                   BRCND := EQUCND ;
                                   if NEG_CND then
                                     BRCND := NEQCND ;
                                 end (* else *) ;
                             FREEREG ( STK [ TOP ] ) ;
                           end (* then *)
                         else

        (**********)
        (*NOT VRBL*)
        (**********)

                           if FPA . DSPLMT = 0 then
                             begin
                               BRCND := ANYCND ;
                               PCODE := PUJP
                             end (* then *)
                           else
                             BRCND := NOCND ;

        (***************)
        (*DO NOT BRANCH*)
        (***************)

                         if VRBL then
                           if ( VPA = RGS ) then
                             AVAIL [ RGADR ] := TRUE ;
                       end (* with *) ;
                   if BRCND <> NOCND then
                     if FLOW_TRACE then
                       begin
                         BRCND := 15 - BRCND ;
                         if BRCND > 0 then
                           GENRELRX ( XBC , BRCND , 5 ) ;

        (*****************)
        (* BC BRCND,*+10 *)
        (*****************)

                         GENRX ( XBAL , RTREG , TRACER , GBR , 0 ) ;
                         if ASM then
                           begin
                             HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                             WRITE ( ASMOUT , ASMTAG , HEXPC , ': ' ) ;
                             WRITELN ( ASMOUT , ' DC AL2(' , LBL2 . NAM
                                       : LBL2 . LEN , '-' , PRCTBL [ 0
                                       ] . NAME , ')' ) ;
                           end (* then *) ;
                         UPD_LBLTBL ( PCOUNTER , LBLMAP ( LBL2 . NAM )
                                      , FALSE , TRUE ) ;
                         PCOUNTER := NEXTPC ( 1 ) ;
                       end (* then *)
                     else
                       GENRXLAB ( XBC , BRCND , LBL2 , 0 ) ;

        (****************************)
        (* CLEAR C.C./ NEGATE FLAGS *)
        (****************************)

                   BRCND := - 1 ;
                   NEG_CND := FALSE ;
                 end (* tag/ca *) ;
          PXJP :

        (***********************************************)
        (* LBL2   = LOWER BOUND, CASE EXPRESSION       *)
        (* LBL2+1 = UPPER BOUND,                       *)
        (* LBL2+2 = BRANCH TABLE LABEL                 *)
        (* LBL2+3 = CASE EXIT LABEL                    *)
        (***********************************************)


                 begin
                   TOP := TOP - 1 ;
                   LOAD ( STK [ TOP ] ) ;
                   with STK [ TOP ] do
                     begin
                       Q := LBLMAP ( LBL2 . NAM ) ;

        /****************************************/
        /* new xjp = xjp without def constants  */
        /* for high and low values              */
        /* ------------------------------------ */
        /* pascal1 has left 2 labels unused     */
        /* in this case to be used by code      */
        /* generators for their own fields      */
        /* to store the min and max values      */
        /* determined during branch table scan  */
        /****************************************/

                       if XJPFLAG = 'N' then
                         begin
                           Q := Q - 2 ;
                           MKLBL ( LBL2 , Q ) ;
                         end (* then *) ;
                       MKLBL ( LBL1 , Q + 1 ) ;
                       MKLBL ( LBL_WORK , Q + 3 ) ;
                       if FLOW_TRACE then
                         begin
                           GENRXLAB ( XLA , JREG , LBL_WORK , - 1 ) ;
                           GENRR ( XSR , JREG , PBR1 ) ;
                           GENRXLAB ( XC , RGADR , LBL1 , - 1 ) ;
                           GENRELRX ( XBC , GRTCND , 9 ) ;

        (***********)
        (* BH *+18 *)
        (***********)

                           GENRXLAB ( XS , RGADR , LBL2 , - 1 ) ;
                           GENRELRX ( XBC , LESCND , 5 ) ;

        (***********)
        (* BM *+10 *)
        (***********)

                           GENRR ( XAR , RGADR , RGADR ) ;
                           MKLBL ( LBL_WORK , Q + 2 ) ;
                           GENRXLAB ( XLH , JREG , LBL_WORK , RGADR ) ;
                           GENRELRX ( XSTH , JREG , 4 ) ;

        (****************)
        (* STH JREG,*+8 *)
        (****************)

                           GENRX ( XBAL , RTREG , TRACER , GBR , 0 ) ;
                           CODE . H [ PCOUNTER ] := 0 ;
                           PCOUNTER := NEXTPC ( 1 ) ;
                         end (* then *)
                       else
                         begin
                           GENRXLAB ( XC , RGADR , LBL1 , - 1 ) ;

        (*****************************)
        (* CHECK AGAINST UPPER BOUND *)
        (*****************************)

                           GENRXLAB ( XBC , GRTCND , LBL_WORK , 0 ) ;

        (*****************************)
        (* GO TO EXIT IF OUT OF RANGE*)
        (*****************************)

                           GENRXLAB ( XS , RGADR , LBL2 , - 1 ) ;

        (*****************************)
        (* ELSE SUBTRACT LOWER BOUND *)
        (*****************************)

                           GENRXLAB ( XBC , LESCND , LBL_WORK , 0 ) ;

        (*****************************)
        (* CASE_EXIT IF OUT OF RANGE *)
        (*****************************)

                           MKLBL ( LBL_WORK , Q + 2 ) ;
                           GENRR ( XAR , RGADR , RGADR ) ;

        (*******************************)
        (* CONV. INDEX TO TABLE OFFSET *)
        (*******************************)

                           GENRXLAB ( XLH , JREG , LBL_WORK , RGADR ) ;
                           GENRX ( XBC , ANYCND , 0 , JREG , PBR1 ) ;
                         end (* else *) ;
                       AVAIL [ RGADR ] := TRUE ;
                     end (* with *) ;
                 end (* tag/ca *) ;
          PPOP : begin
                   TOP := TOP - 1 ;
                   FREEREG ( STK [ TOP ] ) ;
                 end (* tag/ca *) ;
          PMST : if CALDPTH < MAXCALDPTH then
                   begin
                     CALDPTH := CALDPTH + 1 ;
                     with CALSTK [ CALDPTH ] do
                       begin
                         PFLEV := P ;
                         DISPSAV := Q
                       end (* with *) ;
                   end (* then *)
                 else
                   ERROR ( 259 ) ;
          PCUP : begin
                   CALLSUB ;
                   if OPNDTYPE <> PROC then
                     with STK [ TOP ] do
                       begin
                         STK [ TOP ] := DATNULL ;

        //******************************************************
        // extlang = fortran:                                   
        // COPY RESULT FROM REGISTER ZERO                       
        //******************************************************

                         case EXTLANG of
                           'F' : case OPNDTYPE of
                                   BOOL : begin
                                            FINDRG ;
                                            GENRR ( XLR , NXTRG , 0 )
                                          end (* tag/ca *) ;
                                   INT : begin
                                           FINDRG ;
                                           GENRR ( XLR , NXTRG , 0 )
                                         end (* tag/ca *) ;
                                   REEL : begin
                                            FINDFP ;
                                            GENRR ( XLDR , NXTRG , 0 )
                                          end (* tag/ca *) ;
                                 end (* case *) ;

        //******************************************************
        // extlang = assembler:                                 
        // COPY RESULT FROM REGISTER ZERO                       
        //******************************************************

                           'A' : case OPNDTYPE of
                                   ADR , INT :
                                     begin
                                       FINDRG ;
                                       GENRR ( XLR , NXTRG , 0 )
                                     end (* tag/ca *) ;
                                   HINT : begin
                                            FINDRG ;
                                            GENRR ( XLR , NXTRG , 0 )
                                          end (* tag/ca *) ;
                                   BOOL , CHRC :
                                     begin
                                       FINDRG ;
                                       GENRR ( XLR , NXTRG , 0 )
                                     end (* tag/ca *) ;
                                   PSET : ERROR ( 616 ) ;
                                   REEL : begin
                                            FINDFP ;
                                            GENRR ( XLDR , NXTRG , 0 )
                                          end (* tag/ca *) ;
                                 end (* case *) ;

        //******************************************************
        // extlang = pascal:                                    
        // COPY RESULT FROM 72 (R1)                             
        //******************************************************

                           otherwise
                             case OPNDTYPE of
                               ADR , INT :
                                 begin
                                   FINDRG ;
                                   GENRX ( XL , NXTRG , FNCRSLT , TRG1
                                           , 0 )
                                 end (* tag/ca *) ;
                               HINT : begin
                                        FINDRG ;
                                        GENRX ( XLH , NXTRG , FNCRSLT ,
                                                TRG1 , 0 ) ;
                                      end (* tag/ca *) ;
                               BOOL , CHRC :
                                 begin
                                   FINDRG ;
                                   GENRR ( XSR , NXTRG , NXTRG ) ;
                                   GENRX ( XIC , NXTRG , FNCRSLT , TRG1
                                           , 0 ) ;
                                 end (* tag/ca *) ;
                               PSET : ERROR ( 616 ) ;
                               REEL : begin
                                        FINDFP ;
                                        GENRX ( XLD , NXTRG , FNCRSLT ,
                                                TRG1 , 0 )
                                      end (* tag/ca *) ;
                               VARC : begin
                                        FINDRG ;
                                        GENRX ( XLA , NXTRG , FNCRSLT ,
                                                TRG1 , 0 ) ;
                                      end (* tag/ca *) ;
                             end (* case *)
                         end (* case *) ;
                         VRBL := TRUE ;
                         DRCT := TRUE ;
                         FPA := ZEROBL ;
                         VPA := RGS ;
                         RGADR := NXTRG ;
                         DTYPE := OPNDTYPE ;
                         TOP := TOP + 1 ;
                       end (* with *)
                   else
                     if CKMODE then
                       CHECKFREEREGS ;
                   CSPREGACTIVE := FALSE ;
                   OLDCSP := PSIO ;
                 end (* tag/ca *) ;
          PENT , PRET :
            begin
              if PCODE = PENT then
                begin
                  GS . IN_PROCBODY := TRUE ;
                  GS . FILL_LINEPTR := TRUE ;
                end (* then *)
              else
                begin
                  GS . IN_PROCBODY := FALSE ;
                  GS . MOD2DEFSTEP := 0
                end (* else *) ;
              ENT_RET ;
            end (* tag/ca *) ;
          PCSP : case CSP of
                   PDAT : CALLSTNDRD ;
                   PTIM : CALLSTNDRD ;
                   otherwise
                     CALLSTNDRD ;
                 end (* case *) ;
          PCST : begin

        (************************************************)
        (* BEGINNING OF A CSECT OF STRUCTURED CONSTANTS *)
        (************************************************)

                   PRCTBL [ 0 ] . NAME := LBL1 . NAM ;
                   PRCTBL [ 0 ] . LNK := 0 ;
                   for CPCOUNTER := 0 to 7 do
                     CODE . C [ CPCOUNTER ] := LBL1 . NAM [ CPCOUNTER +
                                               1 ] ;
                   for CPCOUNTER := 8 to 15 do
                     CODE . C [ CPCOUNTER ] := CHR ( 0 ) ;
                   CPCOUNTER := 16 ;
                   PCOUNTER := CPCOUNTER ;
                   CSTBLK := TRUE ;
                   CSEGSTRT := 0 ;
                   CSEGLIMIT := SIZE_TXTCHUNK * 145 ;
                 end (* tag/ca *) ;
          PDFC : begin

        (********************************************)
        (* A SIMPLE CONSTANT IN THE CONSTANTS CSECT *)
        (********************************************)

                   if CSTBLK then
                     if LBL1 . CADDR <= 32767 then
                       begin
                         if CPCOUNTER > LBL1 . CADDR then
                           ERROR ( 617 ) ;
                         while CPCOUNTER < LBL1 . CADDR do
                           begin
                             if CPCOUNTER = CSEGLIMIT then
                               DUMPCONSTBLK ( FALSE ) ;
                             CODE . C [ CPCOUNTER - CSEGSTRT ] := CHR (
                                                   0 ) ;
                             CPCOUNTER := CPCOUNTER + 1 ;
                           end (* while *) ;
                         PCOUNTER := LBL1 . CADDR ;
                         Q := PCOUNTER - CSEGSTRT ;
                         case OPNDTYPE of
                           NON : begin
                                   CPCOUNTER := LBL1 . CADDR + SLNGTH ;
                                 end (* tag/ca *) ;
                           BOOL , CHRC :
                             begin
                               if not ( IVAL in [ 0 .. 255 ] ) then
                                 ERROR ( 301 ) ;
                               CODE . C [ Q ] := CHR ( IVAL ) ;
                               CPCOUNTER := CPCOUNTER + 1 ;
                             end (* tag/ca *) ;
                           HINT : begin
                                    if ( IVAL < - 32768 ) or ( IVAL >
                                    32767 ) then
                                      ERROR ( 301 ) ;
                                    if ODD ( Q ) then
                                      ERROR ( 610 ) ;
                                    CODE . H [ Q DIV 2 ] := TO_HINT (
                                                   IVAL ) ;
                                    CPCOUNTER := CPCOUNTER + 2 ;
                                  end (* tag/ca *) ;
                           INT , ADR :
                             begin
                               if Q MOD 4 <> 0 then
                                 ERROR ( 611 ) ;
                               CODE . I [ Q DIV 4 ] := IVAL ;
                               CPCOUNTER := CPCOUNTER + 4 ;
                             end (* tag/ca *) ;
                           PSET : begin
                                    if Q MOD 4 <> 0 then
                                      ERROR ( 611 ) ;
                                    for P := 1 to PSLNGTH do
                                      begin
                                        CODE . C [ Q ] := PSVAL . C [ P
                                                   ] ;
                                        Q := Q + 1 ;
                                      end (* for *) ;
                                    CPCOUNTER := LBL1 . CADDR + PSLNGTH
                                                 ;
                                  end (* tag/ca *) ;
                           CARR : begin
                                    for P := 1 to SLNGTH do
                                      begin
                                        CODE . C [ Q ] := SVAL [ P ] ;
                                        Q := Q + 1 ;
                                      end (* for *) ;
                                    CPCOUNTER := LBL1 . CADDR + SLNGTH
                                                 ;
                                  end (* tag/ca *) ;
                           REEL : begin
                                    if Q MOD 8 <> 0 then
                                      ERROR ( 612 ) ;
                                    CODE . R [ Q DIV 8 ] := RVAL ;
                                    CPCOUNTER := CPCOUNTER + 8 ;
                                  end (* tag/ca *) ;
                         end (* case *) ;
                       end (* then *)
                     else
                       ERROR ( 251 ) ;
                 end (* tag/ca *) ;
          PEND : if CSTBLK then
                   begin

        (********************************)
        (* store length of static csect *)
        (* at addr of static csect + 8  *)
        (********************************)

                     CODE . H [ 4 ] := TO_HINT ( CPCOUNTER ) ;
                     if CPCOUNTER > 16 then
                       DUMPCONSTBLK ( TRUE ) ;
                     TOTALBYTES := TOTALBYTES + CPCOUNTER ;
                     CSTBLK := FALSE ;
                   end (* then *) ;
          PSTP : if ASM then
                   begin

        (*******************************)
        (* GENERATE ASSEMBLER END CARD *)
        (*******************************)

                     WRITELN ( ASMOUT , '## ' , ' ' : SPACEASMX ,
                               'EXTRN $PASENT' ) ;
                     WRITELN ( ASMOUT , '## ' , ' ' : SPACEASMX ,
                               'END   $PASENT' ) ;
                   end (* then *) ;
        end (* case *) ;
      end (* COPERATION *) ;


   procedure UOPERATION ;

   (********************)
   (* UNARY OPERATIONS *)
   (********************)


      begin (* UOPERATION *)
        case PCODE of
          PFLT , PFLO :
            begin
              if PCODE = PFLT then
                OPPTR := TOP - 1
              else
                OPPTR := TOP - 2 ;
              with STK [ OPPTR ] do
                if VRBL then
                  begin
                    LOAD ( STK [ OPPTR ] ) ;
                    FINDFP ;
                    GENRX ( XX , RGADR , FL2 + 4 , GBR , 0 ) ;
                    GENRX ( XST , RGADR , FL1 + 4 , GBR , 0 ) ;
                    GENRX ( XLD , NXTRG , FL1 , GBR , 0 ) ;
                    GENRX ( XSD , NXTRG , FL2 , GBR , 0 ) ;
                    AVAIL [ RGADR ] := TRUE ;
                    DTYPE := REEL ;
                    RGADR := NXTRG ;
                  end (* then *)
                else

        (***********)
        (* CONSTANT*)
        (***********)

                  begin
                    DTYPE := REEL ;
                    RCNST := FPA . DSPLMT ;
                  end (* else *)
            end (* tag/ca *) ;
          PNGR : with STK [ TOP - 1 ] do
                   if VRBL then
                     begin
                       LOAD ( STK [ TOP - 1 ] ) ;
                       GENRR ( XLCDR , RGADR , RGADR )
                     end (* then *)
                   else

        (************)
        (* CONSTANT *)
        (************)

                     RCNST := - RCNST ;
          PNGI : with STK [ TOP - 1 ] do
                   if VRBL then
                     begin
                       LOAD ( STK [ TOP - 1 ] ) ;
                       GENRR ( XLCR , RGADR , RGADR ) ;
                     end (* then *)
                   else
                     FPA . DSPLMT := - FPA . DSPLMT ;
          PABI : with STK [ TOP - 1 ] do
                   if VRBL then
                     begin
                       LOAD ( STK [ TOP - 1 ] ) ;
                       GENRR ( XLPR , RGADR , RGADR ) ;
                     end (* then *)
                   else
                     FPA . DSPLMT := ABS ( FPA . DSPLMT ) ;
          PABR : with STK [ TOP - 1 ] do
                   begin
                     LOAD ( STK [ TOP - 1 ] ) ;
                     GENRR ( XLPDR , RGADR , RGADR )
                   end (* with *) ;
          PSQI : with STK [ TOP - 1 ] do
                   begin
                     MDTAG := PMPI ;
                     LOAD ( STK [ TOP - 1 ] ) ;
                     MDTAG := PBGN ;
                     GENRR ( XMR , RGADR , RGADR + 1 ) ;
                     AVAIL [ RGADR ] := TRUE ;
                     RGADR := RGADR + 1 ;
                   end (* with *) ;
          PSQR : with STK [ TOP - 1 ] do
                   begin
                     LOAD ( STK [ TOP - 1 ] ) ;
                     GENRR ( XMDR , RGADR , RGADR )
                   end (* with *) ;
          PXPO : with STK [ TOP - 1 ] do
                   begin
                     FINDRG ;
                     if VRBL then
                       GETOPERAND ( STK [ TOP - 1 ] , Q1 , P1 , B1 )
                     else
                       LOAD ( STK [ TOP - 1 ] ) ;
                     if ( VPA = RGS ) and DRCT then
                       begin
                         GENRX ( XSTD , RGADR , FL3 , GBR , 0 ) ;
                         GENRX ( XIC , NXTRG , FL3 , GBR , 0 ) ;
                         AVAILFP [ RGADR ] := TRUE ;
                       end (* then *)
                     else
                       begin
                         GENRX ( XIC , NXTRG , Q1 , P1 , B1 ) ;
                         VPA := RGS ;
                         DRCT := TRUE ;
                       end (* else *) ;
                     GENRX ( XLA , 0 , 127 , 0 , 0 ) ;
                     GENRR ( XNR , NXTRG , 0 ) ;
                     GENRX ( XLA , 0 , 64 , 0 , 0 ) ;
                     GENRR ( XSR , NXTRG , 0 ) ;
                     RGADR := NXTRG ;
                     DTYPE := INT ;
                   end (* with *) ;
          PNOT : with STK [ TOP - 1 ] do
                   if OPNDTYPE = INT then
                     begin
                       LOAD ( STK [ TOP - 1 ] ) ;
                       GENRXLIT ( XX , RGADR , - 1 , 0 ) ;
                     end (* then *)
                   else
                     begin
                       if BRCND >= 0 then
                         if NEG_CND then

        (*********************)
        (* CLEAR NEGATE FLAG *)
        (*********************)

                           begin
                             NEG_CND := FALSE ;
                             BRCND := - 1 ;
                           end (* then *)
                         else
                           BRCND := 15 - BRCND
                       else

        (***************************)
        (* NEGATING A BOOLEAN VLUE *)
        (***************************)

                         if VRBL then
                           begin
                             NEG_CND := TRUE ;
                             BRCND := 0
                           end (* then *)
                         else
                           if FPA . DSPLMT = 0 then
                             FPA . DSPLMT := 1
                           else
                             FPA . DSPLMT := 0
                     end (* else *) ;
          PODD : with STK [ TOP - 1 ] do
                   begin
                     if VRBL then
                       if DRCT and ( VPA = MEM ) then
                         begin
                           if ODD ( FPA . DSPLMT ) then
                             Q := 14
                           else
                             Q := 1 ;
                           FPA . DSPLMT := 0 ;
                           GETOPERAND ( STK [ TOP - 1 ] , Q1 , P1 , B1
                                        ) ;
                           if B1 > 0 then
                             if P1 > 0 then
                               GENRR ( XAR , P1 , B1 )
                             else
                               P1 := B1 ;
                           if DTYPE = HINT then
                             Q1 := Q1 + 1
                           else
                             Q1 := Q1 + 3 ;
                           GENSI ( XTM , Q1 , P1 , 1 ) ;

        (***********************************)
        (* RIGHT MOST BYTE IS BEING TESTED *)
        (***********************************)

                           BRCND := Q ;

        (*************)
        (* BO OR BNO *)
        (*************)

                         end (* then *)
                       else
                         begin
                           LOAD ( STK [ TOP - 1 ] ) ;
                           GENRXLIT ( XN , RGADR , 1 , 0 )
                         end (* else *)
                     else
                       if ODD ( FPA . DSPLMT ) then
                         FPA . DSPLMT := 1
                       else
                         FPA . DSPLMT := 0 ;
                     DTYPE := BOOL
                   end (* with *) ;
          PINC , PDEC :
            with STK [ TOP - 1 ] do
              begin
                if PCODE = PDEC then
                  Q := - Q ;
                if not DRCT then
                  LOAD ( STK [ TOP - 1 ] ) ;
                FPA . DSPLMT := FPA . DSPLMT + Q ;
              end (* with *) ;
          PCHR : with STK [ TOP - 1 ] do
                   if DTYPE > CHRC then
                     begin
                       if VRBL then
                         LOAD ( STK [ TOP - 1 ] ) ;
                       DTYPE := CHRC
                     end (* then *) ;
          PORD : with STK [ TOP - 1 ] do
                   if DTYPE <= CHRC then
                     begin
                       if VRBL then
                         LOAD ( STK [ TOP - 1 ] ) ;
                       DTYPE := INT
                     end (* then *) ;
          PNEW : begin
                   TOP := TOP - 1 ;
                   GENRX ( XL , TRG0 , NEWPTR , GBR , 0 ) ;
                   GENRXLIT ( XS , TRG0 , P , 0 ) ;
                   if Q <> 4 then

        (****************************)
        (* MUST ALIGN TO DOUBLEWORD *)
        (****************************)

                     GENRXLIT ( XN , TRG0 , - 8 , 0 ) ;
                   GENRX ( XST , TRG0 , NEWPTR , GBR , 0 ) ;
                   if not STK [ TOP ] . DRCT then
                     LOAD ( STK [ TOP ] ) ;
                   GETADR ( STK [ TOP ] , Q1 , P1 , B1 ) ;
                   GENRX ( XST , TRG0 , Q1 , B1 , P1 ) ;
                   FREEREG ( STK [ TOP ] ) ;
                   if DEBUG or MUSIC then

        (*************************************)
        (* CHECK FOR STACK-HEAP INTERFERENCE *)
        (*************************************)

                     begin
                       GENRXLAB ( XS , TRG0 , PIAKT -> . SEGSZE , - 1 )
                                  ;
                       GENRR ( XCR , TRG0 , LBR ) ;
                       GENRR ( XBALR , RTREG , 0 ) ;
                       GENRX ( XBC , LEQCND , STKCHK , GBR , 0 ) ;
                     end (* then *) ;
                 end (* tag/ca *) ;
          PSAV : begin
                   TOP := TOP - 1 ;
                   GENRX ( XL , TRG0 , NEWPTR , GBR , 0 ) ;
                   if not STK [ TOP ] . DRCT then
                     LOAD ( STK [ TOP ] ) ;
                   GETADR ( STK [ TOP ] , Q1 , P1 , B1 ) ;
                   GENRX ( XST , TRG0 , Q1 , B1 , P1 ) ;
                   FREEREG ( STK [ TOP ] ) ;
                 end (* tag/ca *) ;
          PRST : begin
                   TOP := TOP - 1 ;
                   with STK [ TOP ] do
                     begin
                       LOAD ( STK [ TOP ] ) ;
                       if DEBUG then

        (*********************************)
        (* SEE IF NEW HEAP POINTER VALID *)
        (*********************************)

                         begin
                           if RGADR <> 2 then
                             begin
                               if not AVAIL [ 2 ] then
                                 ERROR ( 259 ) ;
                               GENRR ( XLR , 2 , RGADR ) ;
                             end (* then *) ;
                           GENRX ( XBAL , RTREG , PTRCHK , GBR , 0 ) ;
                         end (* then *) ;

        (**********************************************************)
        (* CODE FOR CLEARING THE RELEASE HEAP AREA SHOULD GO HERE *)
        (* SEE RETURN SEQUENCE 'PRET' AS AN EXAMPLE.              *)
        (**********************************************************)

                       GENRX ( XST , RGADR , NEWPTR , GBR , 0 ) ;
                       AVAIL [ RGADR ] := TRUE ;
                     end (* with *) ;
                 end (* tag/ca *) ;
          PCTS : begin

        (************************************)
        (* SET/INITIALIZE RUN TIME COUNTERS *)
        (************************************)

                   GENRXLAB ( XL , 2 , LBL2 , - 1 ) ;
                   CSP := PCTR ;
                   GOTOCSP ;
                 end (* tag/ca *) ;
          PCTI : begin

        (****************************************)
        (* INCREMENT THE COUNT OF COUNTER # 'Q' *)
        (****************************************)

                   GENRX ( XL , TRG1 , HEAPLMT , GBR , 0 ) ;
                   GENRX ( XLA , TRG14 , 1 , 0 , 0 ) ;
                   Q := 4 * Q + DYN2LEN ;
                   if Q > SHRTINT then
                     begin
                       GENRXLIT ( XA , TRG1 , Q , 0 ) ;
                       Q := 0 ;
                     end (* then *) ;
                   GENRX ( XA , TRG14 , Q , TRG1 , 0 ) ;
                   GENRX ( XST , TRG14 , Q , TRG1 , 0 ) ;
                 end (* tag/ca *) ;
        end (* case *) ;
      end (* UOPERATION *) ;


   procedure PACK_UNPACK ( var L , R : DATUM ) ;

      var XOPC : BYTE ;

      begin (* PACK_UNPACK *)
        LOAD ( L ) ;

        (***********************)
        (* LOAD SOURCE ADDRESS *)
        (***********************)

        LOAD ( R ) ;

        (****************************)
        (* LOAD DESTINATION ADDRESS *)
        (****************************)

        if P = 1 then
          GENRR ( XSR , TRG0 , TRG0 ) ;

        (*********************)
        (*FOR BYTE INSERTIONS*)
        (*********************)

        if IVAL <= 0 then
          begin
            ERROR ( 619 ) ;
            IVAL := 1
          end (* then *) ;
        FINDRG ;

        (***************************)
        (* REGISTER FOR LOOP COUNT *)
        (***************************)

        GENRXLIT ( XL , NXTRG , IVAL , 0 ) ;
        GENRR ( XBALR , TRG1 , 0 ) ;
        OLDCSP := PSIO ;
        if P = 1 then
          XOPC := XIC
        else
          if P = 2 then
            XOPC := XLH
          else
            begin
              XOPC := XL ;
              if P <> 4 then
                ERROR ( 619 )
            end (* else *) ;
        GENRX ( XOPC , TRG0 , 0 , 0 , L . RGADR ) ;
        if Q = 1 then
          XOPC := XSTC
        else
          if Q = 2 then
            XOPC := XSTH
          else
            begin
              XOPC := XST ;
              if Q <> 4 then
                ERROR ( 619 )
            end (* else *) ;
        GENRX ( XOPC , TRG0 , 0 , 0 , R . RGADR ) ;
        GENRX ( XLA , L . RGADR , P , 0 , L . RGADR ) ;
        GENRX ( XLA , R . RGADR , Q , 0 , R . RGADR ) ;
        GENRR ( XBCTR , NXTRG , TRG1 ) ;
        AVAIL [ NXTRG ] := TRUE ;
        AVAIL [ L . RGADR ] := TRUE ;
        AVAIL [ R . RGADR ] := TRUE ;
      end (* PACK_UNPACK *) ;


   procedure MFIOPERATION ( var LEFT , PAT : DATUM ; LEN : INTEGER ) ;

   //****************************************************************
   // generate overlapping MVC for short MFI                         
   // and MVCL for long MFI                                          
   //****************************************************************


      var P1 , B1 , P2 , B2 , PX , BX : LVLRNG ;
          Q1 , Q2 , QX : ADRRNG ;

      begin (* MFIOPERATION *)
        if LEN > 0 then

        //******************************************************
        // if length <= 256 generate overlapping MVC            
        //******************************************************

          if LEN <= 256 then
            begin

        //******************************************************
        // get address of left operand                          
        //******************************************************

              GETADR2 ( LEFT , Q1 , P1 , B1 ) ;
              if not LEFT . DRCT then
                begin
                  GENRX ( XL , TXRG , Q1 , B1 , P1 ) ;
                  Q1 := 0 ;
                  B1 := 0 ;
                  P1 := TXRG ;
                end (* then *) ;

        //******************************************************
        // consolidate index and base reg to base reg b1        
        //******************************************************

              CONS_REGS ( B1 , P1 ) ;

        //******************************************************
        // move pattern to byte 1                               
        //******************************************************

              GEN_MOVECHAR ( Q1 , B1 , PAT ) ;

        //******************************************************
        // operlapping MVC                                      
        //******************************************************

              if LEN > 1 then
                GENSS ( XMVC , LEN - 1 , Q1 + 1 , B1 , Q1 , B1 ) ;
            end (* then *)
          else
            begin

        //******************************************************
        // get address of left operand                          
        //******************************************************

              GETADR2 ( LEFT , Q1 , P1 , B1 ) ;
              if not LEFT . DRCT then
                begin
                  GENRX ( XL , TXRG , Q1 , B1 , P1 ) ;
                  Q1 := 0 ;
                  B1 := 0 ;
                  P1 := TXRG ;
                end (* then *) ;

        //******************************************************
        // get init pattern                                     
        //******************************************************

              if FALSE then
                begin
                  WRITELN ( TRACEF , 'pat.drct   = ' , PAT . DRCT ) ;
                  WRITELN ( TRACEF , 'pat.vrbl   = ' , PAT . VRBL ) ;
                  WRITELN ( TRACEF , 'pat.dtype  = ' , PAT . DTYPE ) ;
                  WRITELN ( TRACEF , 'pat.vpa    = ' , PAT . VPA ) ;
                  WRITELN ( TRACEF , 'pat.rgadr  = ' , PAT . RGADR ) ;
                end (* then *) ;

        //******************************************************
        // RESTORE THE OLD MIDLEVEL BASE REG                    
        //******************************************************

              if P1 < 0 then
                begin
                  B1 := P1 ;
                  P1 := 0 ;
                end (* then *) ;

        //**************************************************
        // THIS IS ONLY VALID FOR THE 370,                  
        // FOR THE 360 THE 'MVCL' INSTRUNCTION SHOULD BE    
        // REPLACED BY A subroutine or a loop or MVC;       
        // because the length of the operands is not        
        // known at compile time in this case, a fix list   
        // of MVCs is not sufficient                        
        //**************************************************

              if B1 < 0 then
                ERROR ( 202 ) ;
              FINDRP ;
              GENRX ( XLA , NXTRG , Q1 , B1 , P1 ) ;
              P1 := NXTRG ;
              B1 := NXTRG + 1 ;
              FINDRP ;
              P2 := NXTRG ;
              B2 := NXTRG + 1 ;

        //******************************************************
        // const length operand                                 
        // generate LA                                          
        //******************************************************

              GENRX ( XLA , B1 , LEN , 0 , 0 ) ;

        //******************************************************
        // source address is zero                               
        //******************************************************

              GENRR ( XXR , P2 , P2 ) ;

        //******************************************************
        // pattern operand                                      
        // generate L or LR                                     
        //******************************************************

              GETOP_SIMPLE ( PAT , QX , PX , BX , B2 , FALSE ) ;
              GENRX ( XSLL , B2 , 24 , 0 , 0 ) ;

        //******************************************************
        // generate MVCL instruction                            
        //******************************************************

              GENRR ( XMVCL , P1 , P2 ) ;
              AVAIL [ P1 ] := TRUE ;
              AVAIL [ B1 ] := TRUE ;
              AVAIL [ P2 ] := TRUE ;
              AVAIL [ B2 ] := TRUE ;
              S370CNT := S370CNT + 1 ;
            end (* else *) ;
        FREEREG ( LEFT ) ;
        FREEREG ( PAT ) ;
      end (* MFIOPERATION *) ;


   procedure MZEOPERATION ( var L : DATUM ; LEN : INTEGER ) ;

   //****************************************************************
   // generate XC for short MZE                                      
   // and MVCL for long MZE                                          
   //****************************************************************


      begin (* MZEOPERATION *)
        if LEN > 0 then

        //******************************************************
        // if length <= 256 generate XC                         
        //******************************************************

          if LEN <= 256 then
            begin

        //******************************************************
        // get address of left operand                          
        //******************************************************

              GETADR ( L , Q1 , P1 , B1 ) ;
              if not L . DRCT then
                begin
                  GENRX ( XL , TXRG , Q1 , B1 , P1 ) ;
                  Q1 := 0 ;
                  B1 := 0 ;
                  P1 := TXRG ;
                end (* then *) ;

        //******************************************************
        // consolidate index and base reg to base reg b1        
        //******************************************************

              CONS_REGS ( B1 , P1 ) ;

        //******************************************************
        // XC is used to zero the area                          
        //******************************************************

              GENSS ( XXC , LEN , Q1 , B1 , Q1 , B1 ) ;
            end (* then *)
          else
            begin

        //******************************************************
        // get address of left operand                          
        //******************************************************

              GETADR ( L , Q1 , P1 , B1 ) ;
              if not L . DRCT then
                begin
                  GENRX ( XL , TXRG , Q1 , B1 , P1 ) ;
                  Q1 := 0 ;
                  B1 := 0 ;
                  P1 := TXRG ;
                end (* then *) ;

        //******************************************************
        // RESTORE THE OLD MIDLEVEL BASE REG                    
        //******************************************************

              if P1 < 0 then
                begin
                  B1 := P1 ;
                  P1 := 0 ;
                end (* then *) ;

        //**************************************************
        // THIS IS ONLY VALID FOR THE 370,                  
        // FOR THE 360 THE 'MVCL' INSTRUNCTION SHOULD BE    
        // REPLACED BY A subroutine or a loop or MVC;       
        // because the length of the operands is not        
        // known at compile time in this case, a fix list   
        // of MVCs is not sufficient                        
        //**************************************************

              if B1 < 0 then
                ERROR ( 202 ) ;
              FINDRP ;
              GENRX ( XLA , NXTRG , Q1 , B1 , P1 ) ;
              P1 := NXTRG ;
              B1 := NXTRG + 1 ;
              FINDRP ;
              P2 := NXTRG ;
              B2 := NXTRG + 1 ;

        //******************************************************
        // const length operand                                 
        // generate LA                                          
        //******************************************************

              GENRX ( XLA , B1 , LEN , 0 , 0 ) ;

        //******************************************************
        // source address is zero                               
        //******************************************************

              GENRR ( XXR , P2 , P2 ) ;

        //******************************************************
        // pattern is zero                                      
        //******************************************************

              GENRR ( XXR , B2 , B2 ) ;

        //******************************************************
        // generate MVCL instruction                            
        //******************************************************

              GENRR ( XMVCL , P1 , P2 ) ;
              AVAIL [ P1 ] := TRUE ;
              AVAIL [ B1 ] := TRUE ;
              AVAIL [ P2 ] := TRUE ;
              AVAIL [ B2 ] := TRUE ;
              S370CNT := S370CNT + 1 ;
            end (* else *) ;
        FREEREG ( L ) ;
      end (* MZEOPERATION *) ;


   procedure MCPOPERATION ( var L , R , LEN : DATUM ) ;

   //****************************************************************
   // generate MVCL instruction for MEMCPY                           
   // no other possibility, since length it not known                
   // at compile time                                                
   //****************************************************************


      var P1 , B1 , P2 , B2 , PX , BX : LVLRNG ;
          Q1 , Q2 , QX : ADRRNG ;
          BPC : ICRNG ;

      begin (* MCPOPERATION *)

        //******************************************************
        // get address of left operand                          
        //******************************************************

        GETADR2 ( L , Q1 , P1 , B1 ) ;
        if not L . DRCT then
          begin
            GENRX ( XL , TXRG , Q1 , B1 , P1 ) ;
            Q1 := 0 ;
            B1 := 0 ;
            P1 := TXRG ;
          end (* then *) ;
        TXRG := TRG1 ;

        //******************************************************
        // TO AVOID REASSIGNM. OF THE SAME BASE REG             
        //******************************************************

        OLDCSP := PSIO ;  // INDICATES LOSS OF TRG1

        //******************************************************
        // get address of right operand                         
        //******************************************************

        GETADR2 ( R , Q2 , P2 , B2 ) ;
        if not R . DRCT then
          begin
            GENRX ( XL , TXRG , Q2 , B2 , P2 ) ;
            Q2 := 0 ;
            B2 := 0 ;
            P2 := TXRG ;
          end (* then *) ;
        TXRG := TRG14 ;

        //******************************************************
        // get length                                           
        //******************************************************

        if FALSE then
          begin
            WRITELN ( TRACEF , 'len.drct   = ' , LEN . DRCT ) ;
            WRITELN ( TRACEF , 'len.vrbl   = ' , LEN . VRBL ) ;
            WRITELN ( TRACEF , 'len.dtype  = ' , LEN . DTYPE ) ;
            WRITELN ( TRACEF , 'len.vpa    = ' , LEN . VPA ) ;
            WRITELN ( TRACEF , 'len.rgadr  = ' , LEN . RGADR ) ;
          end (* then *) ;

        //******************************************************
        // RESTORE THE OLD MIDLEVEL BASE REG                    
        //******************************************************

        if P1 < 0 then
          begin
            B1 := P1 ;
            P1 := 0 ;
          end (* then *) ;
        if P2 < 0 then
          begin
            B2 := P2 ;
            P2 := 0 ;
          end (* then *) ;

        //**************************************************
        // THIS IS ONLY VALID FOR THE 370,                  
        // FOR THE 360 THE 'MVCL' INSTRUNCTION SHOULD BE    
        // REPLACED BY A subroutine or a loop or MVC;       
        // because the length of the operands is not        
        // known at compile time in this case, a fix list   
        // of MVCs is not sufficient                        
        //**************************************************

        if ( B1 < 0 ) or ( B2 < 0 ) then
          ERROR ( 202 ) ;
        FINDRP ;
        GENRX ( XLA , NXTRG , Q1 , B1 , P1 ) ;
        P1 := NXTRG ;
        B1 := NXTRG + 1 ;
        FINDRP ;
        GENRX ( XLA , NXTRG , Q2 , B2 , P2 ) ;
        P2 := NXTRG ;
        B2 := NXTRG + 1 ;

        //******************************************************
        // length operand                                       
        // generate L or LR                                     
        //******************************************************

        GETOP_SIMPLE ( LEN , QX , PX , BX , B1 , TRUE ) ;

        //******************************************************
        // copy length in register b1 to register b2            
        // and test                                             
        //******************************************************

        GENRR ( XLTR , B2 , B1 ) ;

        //******************************************************
        // check length for positive                            
        // address of branch will be filled in later            
        //******************************************************

        GENRX ( XBC , LEQCND , 0 , 0 , 0 ) ;
        BPC := PCOUNTER ;
        if ASM then
          WRITELN ( ASMOUT , '## ' , ' ' : SPACEASMX , 'BNP   @NOMV' )
                    ;

        //******************************************************
        // generate MVCL instruction                            
        //******************************************************

        GENRR ( XMVCL , P1 , P2 ) ;

        //******************************************************
        // generate label after MVCL                            
        //******************************************************

        CODE . H [ BPC - 1 ] := TO_HINT ( BASE_DSPLMT ( PCOUNTER ) ) ;
        if ASM then
          WRITELN ( ASMOUT , '## ' , ' ' : SPACEASML ,
                    '@NOMV  DS    0H' ) ;
        AVAIL [ P1 ] := TRUE ;
        AVAIL [ B1 ] := TRUE ;
        AVAIL [ P2 ] := TRUE ;
        AVAIL [ B2 ] := TRUE ;
        S370CNT := S370CNT + 1 ;
        FREEREG ( L ) ;
        FREEREG ( R ) ;
        FREEREG ( LEN ) ;
      end (* MCPOPERATION *) ;


   procedure MSEOPERATION ( var L , PAT , LEN : DATUM ; REVERSE :
                          INTEGER ) ;

   //****************************************************************
   // generate MVCL instruction for MEMSET                           
   // no other possibility, since length it not known                
   // at compile time                                                
   //****************************************************************


      var P1 , B1 , P2 , B2 , PX , BX : LVLRNG ;
          Q1 , Q2 , QX : ADRRNG ;
          BPC : ICRNG ;
          XPAT , XLEN : DATUM ;

      begin (* MSEOPERATION *)

        //******************************************************
        // get address of left operand                          
        //******************************************************

        if REVERSE > 0 then
          begin
            XPAT := LEN ;
            XLEN := PAT ;
          end (* then *)
        else
          begin
            XPAT := PAT ;
            XLEN := LEN ;
          end (* else *) ;

        //******************************************************
        // get address of left operand                          
        //******************************************************

        GETADR ( L , Q1 , P1 , B1 ) ;
        if not L . DRCT then
          begin
            GENRX ( XL , TXRG , Q1 , B1 , P1 ) ;
            Q1 := 0 ;
            B1 := 0 ;
            P1 := TXRG ;
          end (* then *) ;

        //******************************************************
        // get init pattern                                     
        //******************************************************

        if FALSE then
          begin
            WRITELN ( TRACEF , 'pat.drct   = ' , XPAT . DRCT ) ;
            WRITELN ( TRACEF , 'pat.vrbl   = ' , XPAT . VRBL ) ;
            WRITELN ( TRACEF , 'pat.dtype  = ' , XPAT . DTYPE ) ;
            WRITELN ( TRACEF , 'pat.vpa    = ' , XPAT . VPA ) ;
            WRITELN ( TRACEF , 'pat.rgadr  = ' , XPAT . RGADR ) ;
          end (* then *) ;

        //******************************************************
        // get length                                           
        //******************************************************

        if FALSE then
          begin
            WRITELN ( TRACEF , 'len.drct   = ' , XLEN . DRCT ) ;
            WRITELN ( TRACEF , 'len.vrbl   = ' , XLEN . VRBL ) ;
            WRITELN ( TRACEF , 'len.dtype  = ' , XLEN . DTYPE ) ;
            WRITELN ( TRACEF , 'len.vpa    = ' , XLEN . VPA ) ;
            WRITELN ( TRACEF , 'len.rgadr  = ' , XLEN . RGADR ) ;
          end (* then *) ;

        //******************************************************
        // RESTORE THE OLD MIDLEVEL BASE REG                    
        //******************************************************

        if P1 < 0 then
          begin
            B1 := P1 ;
            P1 := 0 ;
          end (* then *) ;

        //**************************************************
        // THIS IS ONLY VALID FOR THE 370,                  
        // FOR THE 360 THE 'MVCL' INSTRUNCTION SHOULD BE    
        // REPLACED BY A subroutine or a loop or MVC;       
        // because the length of the operands is not        
        // known at compile time in this case, a fix list   
        // of MVCs is not sufficient                        
        //**************************************************

        if B1 < 0 then
          ERROR ( 202 ) ;
        FINDRP ;
        GENRX ( XLA , NXTRG , Q1 , B1 , P1 ) ;
        P1 := NXTRG ;
        B1 := NXTRG + 1 ;
        FINDRP ;
        P2 := NXTRG ;
        B2 := NXTRG + 1 ;

        //******************************************************
        // length operand                                       
        // generate L or LR                                     
        //******************************************************

        GETOP_SIMPLE ( XLEN , QX , PX , BX , B1 , TRUE ) ;

        //******************************************************
        // check length for positive                            
        // address of branch will be filled in later *)         
        //******************************************************

        GENRR ( XLTR , B1 , B1 ) ;
        GENRX ( XBC , LEQCND , 0 , 0 , 0 ) ;
        BPC := PCOUNTER ;
        if ASM then
          WRITELN ( ASMOUT , '## ' , ' ' : SPACEASMX , 'BNP   @NOMV' )
                    ;

        //******************************************************
        // source address is zero                               
        //******************************************************

        GENRR ( XXR , P2 , P2 ) ;

        //******************************************************
        // pattern operand                                      
        // generate L or LR                                     
        //******************************************************

        GETOP_SIMPLE ( XPAT , QX , PX , BX , B2 , FALSE ) ;
        GENRX ( XSLL , B2 , 24 , 0 , 0 ) ;

        //******************************************************
        // generate MVCL instruction                            
        //******************************************************

        GENRR ( XMVCL , P1 , P2 ) ;

        //******************************************************
        // generate label after MVCL                            
        //******************************************************

        CODE . H [ BPC - 1 ] := TO_HINT ( BASE_DSPLMT ( PCOUNTER ) ) ;
        if ASM then
          WRITELN ( ASMOUT , '## ' , ' ' : SPACEASML ,
                    '@NOMV  DS    0H' ) ;
        AVAIL [ P1 ] := TRUE ;
        AVAIL [ B1 ] := TRUE ;
        AVAIL [ P2 ] := TRUE ;
        AVAIL [ B2 ] := TRUE ;
        S370CNT := S370CNT + 1 ;
        FREEREG ( L ) ;
        FREEREG ( XPAT ) ;
        FREEREG ( XLEN ) ;
      end (* MSEOPERATION *) ;


   procedure STROPERATION_MVI ( var LEFT : DATUM ; CCONST : CHAR ) ;

   //****************************************************************
   // MVI to string target / implement VST after VC1                 
   //****************************************************************


      var P1 , B1 : LVLRNG ;
          Q1 : ADRRNG ;
          XOPC : BYTE ;

      begin (* STROPERATION_MVI *)

        //******************************************************
        // get address of left operand                          
        //******************************************************
        //******************************************************

        GETADR2 ( LEFT , Q1 , P1 , B1 ) ;
        if not LEFT . DRCT then
          begin
            GENRX ( XL , TXRG , Q1 , B1 , P1 ) ;
            Q1 := 0 ;
            B1 := 0 ;
            P1 := TXRG ;
          end (* then *) ;
        CONS_REGS ( B1 , P1 ) ;
        GENSI ( XMVI , Q1 , B1 , ORD ( CCONST ) ) ;
      end (* STROPERATION_MVI *) ;


   procedure STROPERATION_MVC1 ( var LEFT : DATUM ; var RIGHT : DATUM )
                               ;

   //****************************************************************
   // MVC to string target / implement VST after VC1                 
   //****************************************************************


      var P1 , B1 : LVLRNG ;
          Q1 : ADRRNG ;
          XOPC : BYTE ;

      begin (* STROPERATION_MVC1 *)

        //******************************************************
        // get address of left operand                          
        //******************************************************
        //******************************************************

        GETADR2 ( LEFT , Q1 , P1 , B1 ) ;
        if not LEFT . DRCT then
          begin
            GENRX ( XL , TXRG , Q1 , B1 , P1 ) ;
            Q1 := 0 ;
            B1 := 0 ;
            P1 := TXRG ;
          end (* then *) ;
        CONS_REGS ( B1 , P1 ) ;
        GEN_MOVECHAR ( Q1 , B1 , RIGHT ) ;
      end (* STROPERATION_MVC1 *) ;


   procedure SOPERATION ( var LEFT , RIGHT : DATUM ; PCODEPARM : OPTYPE
                        ; LENPARM : INTEGER ) ;

   //****************************************************************
   // SET UP FOR STRING MOVE/COMPARE OPERATIONS                      
   //****************************************************************


      var P1 , B1 , P2 , B2 : LVLRNG ;
          Q1 , Q2 : ADRRNG ;
          XOPC : BYTE ;

      begin (* SOPERATION *)

        //******************************************************
        // get address of left operand                          
        //******************************************************

        GETADR2 ( LEFT , Q1 , P1 , B1 ) ;
        if not LEFT . DRCT then
          begin
            GENRX ( XL , TXRG , Q1 , B1 , P1 ) ;
            Q1 := 0 ;
            B1 := 0 ;
            P1 := TXRG ;
          end (* then *) ;
        TXRG := TRG1 ;

        //******************************************************
        // TO AVOID REASSIGNM. OF THE SAME BASE REG             
        //******************************************************

        OLDCSP := PSIO ;  // INDICATES LOSS OF TRG1

        //******************************************************
        // get address of right operand                         
        //******************************************************

        GETADR2 ( RIGHT , Q2 , P2 , B2 ) ;
        if not RIGHT . DRCT then
          begin
            GENRX ( XL , TXRG , Q2 , B2 , P2 ) ;
            Q2 := 0 ;
            B2 := 0 ;
            P2 := TXRG ;
          end (* then *) ;
        TXRG := TRG14 ;

        //******************************************************
        // RESTORE THE OLD MIDLEVEL BASE REG                    
        //******************************************************

        if P1 < 0 then
          begin
            B1 := P1 ;
            P1 := 0 ;
          end (* then *) ;
        if P2 < 0 then
          begin
            B2 := P2 ;
            P2 := 0 ;
          end (* then *) ;

        //******************************************************
        // SHORT MOVE = length (q) <= 256                       
        //******************************************************

        if ( LENPARM > 0 ) and ( LENPARM <= 256 ) then
          begin
            if B1 > 0 then
              if P1 > 0 then
                GENRR ( XAR , P1 , B1 )
              else
                P1 := B1 ;
            if B2 > 0 then
              if P2 > 0 then
                GENRR ( XAR , P2 , B2 )
              else
                P2 := B2 ;
            XOPC := XMVC ;
            if PCODEPARM <> PMOV then
              XOPC := XCLC ;
            GENSS ( XOPC , LENPARM , Q1 , P1 , Q2 , P2 ) ;
            if B1 < 0 then
              begin
                if B1 = - 1 then
                  LITTBL [ LEFT . SCNSTNO ] . LNK := PCOUNTER - 2 ;
                CODE . H [ PCOUNTER - 2 ] := TO_HINT ( Q1 ) ;
              end (* then *) ;
            if B2 < 0 then
              begin
                if B2 = - 1 then
                  LITTBL [ RIGHT . SCNSTNO ] . LNK := PCOUNTER - 1 ;
                CODE . H [ PCOUNTER - 1 ] := TO_HINT ( Q2 ) ;
              end (* then *) ;
            if OPT_FLG then
              if XOPC = XMVC then
                with LAST_MVC do
                  begin
                    if PCOUNTER = ( LPC + 3 ) then

        //******************************************************
        // CONSECUTIVE MVC INSTS                                
        //******************************************************

                      if ( CODE . H [ LPC - 2 ] + LLEN ) = CODE . H [
                      PCOUNTER - 2 ] then
                        if ( CODE . H [ LPC - 1 ] + LLEN ) = CODE . H [
                        PCOUNTER - 1 ] then
                          if ( LLEN + LENPARM ) <= 256 then
                            begin
                              CODE . H [ LPC - 3 ] := TO_HINT ( CODE .
                                                   H [ LPC - 3 ] +
                                                   LENPARM ) ;
                              LENPARM := LENPARM + LLEN ;
                              PCOUNTER := LPC ;
                              if B2 = - 1 then
                                if RIGHT . SCNSTNO = NXTLIT - 1 then
                                  NXTLIT := NXTLIT - 1
                                else
                                  LITTBL [ RIGHT . SCNSTNO ] . LNK := 0
                                                   ;
                            end (* then *) ;
                    LPC := PCOUNTER ;
                    LLEN := LENPARM ;
                  end (* with *) ;
          end (* then *)

        //**************************************************
        // THIS IS ONLY VALID FOR THE 370,                  
        // FOR THE 360 THE 'CLCL' INSTRUCTION SHOULD BE     
        // REPLACED BY AN APPROPRIATE NUMBER OF 'CLC'S      
        //**************************************************

        else
          begin
            if ( B1 < 0 ) or ( B2 < 0 ) then
              ERROR ( 202 ) ;
            FINDRP ;
            GENRX ( XLA , NXTRG , Q1 , B1 , P1 ) ;
            P1 := NXTRG ;
            B1 := NXTRG + 1 ;
            FINDRP ;
            GENRX ( XLA , NXTRG , Q2 , B2 , P2 ) ;
            P2 := NXTRG ;
            B2 := NXTRG + 1 ;
            GENRXLIT ( XL , B1 , LENPARM , 0 ) ;
            GENRR ( XLR , B2 , B1 ) ;
            XOPC := XMVCL ;
            if PCODEPARM <> PMOV then
              XOPC := XCLCL ;
            GENRR ( XOPC , P1 , P2 ) ;
            AVAIL [ P1 ] := TRUE ;
            AVAIL [ B1 ] := TRUE ;
            AVAIL [ P2 ] := TRUE ;
            AVAIL [ B2 ] := TRUE ;
            S370CNT := S370CNT + 1 ;
          end (* else *) ;
        FREEREG ( LEFT ) ;
        FREEREG ( RIGHT ) ;
      end (* SOPERATION *) ;


   procedure STROPERATION_LEN ( var LEFT , RIGHT : DATUM ; PCODEPARM :
                              OPTYPE ; LEN_REG : RGRNG ; LEN_OFFS :
                              ADRRNG ) ;

   //****************************************************************
   // SET UP FOR STRING MOVE/COMPARE OPERATIONS                      
   //****************************************************************


      var P1 , B1 , P2 , B2 : LVLRNG ;
          Q1 , Q2 : ADRRNG ;
          XOPC : BYTE ;

      begin (* STROPERATION_LEN *)

        //******************************************************
        // get address of left operand                          
        //******************************************************
        //******************************************************

        GETADR2 ( LEFT , Q1 , P1 , B1 ) ;
        if not LEFT . DRCT then
          begin
            GENRX ( XL , TXRG , Q1 , B1 , P1 ) ;
            Q1 := 0 ;
            B1 := 0 ;
            P1 := TXRG ;
          end (* then *) ;
        TXRG := TRG1 ;

        //******************************************************
        // TO AVOID REASSIGNM. OF THE SAME BASE REG             
        //******************************************************

        OLDCSP := PSIO ;  // INDICATES LOSS OF TRG1

        //******************************************************
        // get address of right operand                         
        //******************************************************

        GETADR2 ( RIGHT , Q2 , P2 , B2 ) ;
        if not RIGHT . DRCT then
          begin
            GENRX ( XL , TXRG , Q2 , B2 , P2 ) ;
            Q2 := 0 ;
            B2 := 0 ;
            P2 := TXRG ;
          end (* then *) ;
        TXRG := TRG14 ;

        //******************************************************
        // RESTORE THE OLD MIDLEVEL BASE REG                    
        //******************************************************

        if P1 < 0 then
          begin
            B1 := P1 ;
            P1 := 0 ;
          end (* then *) ;
        if P2 < 0 then
          begin
            B2 := P2 ;
            P2 := 0 ;
          end (* then *) ;
        if ( B1 < 0 ) or ( B2 < 0 ) then
          ERROR ( 202 ) ;
        FINDRP ;
        GENRX ( XLA , NXTRG , Q1 , B1 , P1 ) ;
        P1 := NXTRG ;
        B1 := NXTRG + 1 ;
        FINDRP ;
        GENRX ( XLA , NXTRG , Q2 , B2 , P2 ) ;
        GENRX ( XLH , NXTRG + 1 , Q2 - 4 , B2 , P2 ) ;
        GENRR ( XLTR , NXTRG + 1 , NXTRG + 1 ) ;
        GENRELRX ( XBC , GEQCND , 4 ) ;
        GENRX ( XL , NXTRG , Q2 , B2 , P2 ) ;
        P2 := NXTRG ;
        B2 := NXTRG + 1 ;
        GENRX ( XLH , B1 , LEN_OFFS , LEN_REG , 0 ) ;
        GENRR ( XLR , B2 , B1 ) ;
        XOPC := XMVCL ;
        if PCODEPARM <> PMOV then
          XOPC := XCLCL ;
        GENRR ( XOPC , P1 , P2 ) ;
        AVAIL [ P1 ] := TRUE ;
        AVAIL [ B1 ] := TRUE ;
        AVAIL [ P2 ] := TRUE ;
        AVAIL [ B2 ] := TRUE ;
        S370CNT := S370CNT + 1 ;
        FREEREG ( LEFT ) ;
        FREEREG ( RIGHT ) ;
      end (* STROPERATION_LEN *) ;


   procedure BOPERATION ;

   //****************************************************************
   // BINARY OPERATIONS                                              
   //****************************************************************


      label 10 , 20 , 30 ;

      var L , R : DATUM ;
          X : DATUM ;

          (*************************)
          (*LEFT AND RIGHT OPERANDS*)
          (*************************)

          LOP , ROP : STKPTR ;

          (****************************************)
          (*STACK INDEX OF LEFT AND RIGHT OPERANDS*)
          (****************************************)

          LRG : RGRNG ;

          (*******************************)
          (*REGISTER HOLDING LEFT OPERAND*)
          (*******************************)

          OP1 , OP2 : BYTE ;
          LR : BOOLEAN ;

          (*****************************)
          (*LEFT/RIGHT INTERCHANGE FLAG*)
          (*****************************)

          Q1 , Q2 : ADRRNG ;
          P1 , P2 , B1 : LVLRNG ;

      begin (* BOPERATION *)

        (**************************************************)
        (* DETERMINE WHICH OPERAND SHOULD BE USED         *)
        (* AS LEFT HAND OPERAND ...                       *)
        (**************************************************)

        LR := ( PCODE in [ PSBA , PSBR , PDVR , PDVI , PMOD , PDIF ,
              PINN ] ) or ( STK [ TOP - 1 ] . VRBL and STK [ TOP ] .
              DRCT ) or ( not STK [ TOP - 1 ] . DRCT ) or ( not STK [
              TOP ] . VRBL ) ;
        10 :
        if LR then
          begin
            LOP := TOP - 1 ;
            ROP := TOP
          end (* then *)
        else
          begin
            LOP := TOP ;
            ROP := TOP - 1
          end (* else *) ;
        L := STK [ LOP ] ;
        R := STK [ ROP ] ;
        case PCODE of
          PADI , PSBI :
            begin
              if not L . DRCT then
                LOAD ( L ) ;
              if R . DRCT then
                if PCODE = PADI then
                  begin
                    L . FPA . DSPLMT := L . FPA . DSPLMT + R . FPA .
                                        DSPLMT ;
                    R . FPA . DSPLMT := 0
                  end (* then *)
                else
                  begin
                    L . FPA . DSPLMT := L . FPA . DSPLMT - R . FPA .
                                        DSPLMT ;
                    R . FPA . DSPLMT := 0
                  end (* else *) ;

        (*************************************************************)
        (*CONST<OPR>CONST AND VRBL<OPR>CONST CASES ARE COMPLETED NOW *)
        (*************************************************************)

              OP1 := XAR ;
              OP2 := XA ;
              if PCODE = PSBI then
                begin
                  OP1 := XSR ;
                  OP2 := XS
                end (* then *) ;
              if R . VRBL then
                begin
                  Q := L . FPA . DSPLMT ;
                  L . FPA . DSPLMT := 0 ;

        (**********)
        (*SAVE FPA*)
        (**********)

                  LOAD ( L ) ;
                  if R . DTYPE <> INT then
                    if R . DTYPE = HINT then
                      OP2 := OP2 - 16

        (**********************************)
        (* SWITCH TO HALFWORD INSTRUCTION *)
        (**********************************)

                    else
                      LOAD ( R ) ;
                  if R . DRCT then
                    if R . VPA = RGS then
                      begin
                        GENRR ( OP1 , L . RGADR , R . RGADR ) ;
                        AVAIL [ R . RGADR ] := TRUE
                      end (* then *)
                    else

        (*********)
        (*VPA=MEM*)
        (*********)

                      begin
                        Q1 := R . MEMADR . DSPLMT ;
                        P1 := R . MEMADR . LVL ;
                        BASE ( Q1 , P1 , B1 ) ;
                        GENRX ( OP2 , L . RGADR , Q1 , B1 , P1 ) ;
                      end (* else *)
                  else

        (************)
        (*NOT R.DRCT*)
        (************)

                    begin
                      GETOPERAND ( R , Q1 , P1 , B1 ) ;
                      GENRX ( OP2 , L . RGADR , Q1 , B1 , P1 ) ;
                    end (* else *) ;
                  L . FPA . DSPLMT := Q ;

        (*************)
        (*RESTORE FPA*)
        (*************)

                end (* then *) ;
              if not LR and ( PCODE = PSBI ) then

        (***********************************)
        (*THIS DOES NOT SEEM TO BE COMPLETE*)
        (***********************************)

                begin
                  Q := - L . FPA . DSPLMT ;
                  L . FPA . DSPLMT := 0 ;
                  if L . VRBL then
                    begin
                      LOAD ( L ) ;
                      GENRR ( XLCR , L . RGADR , L . RGADR ) ;
                    end (* then *) ;
                  L . FPA . DSPLMT := Q ;
                end (* then *) ;
            end (* tag/ca *) ;

        (****************************************************)
        (* neu 09.2016 : addiere int zu adresse / oppolzer  *)
        (* chg 11.2017 : error, when adr is second operand  *)
        (****************************************************)

          PADA : begin

        //******************************************************
        // if type of right operand = adr                       
        // exchange operands                                    
        //******************************************************

                   if R . DTYPE = ADR then
                     begin
                       X := R ;
                       R := L ;
                       L := X ;
                     end (* then *) ;
                   if FALSE then
                     begin
                       WRITELN ( TRACEF , 'pada0: l.dtype = ' , L .
                                 DTYPE ) ;
                       WRITELN ( TRACEF , 'pada0: r.dtype = ' , R .
                                 DTYPE ) ;
                       WRITELN ( TRACEF , 'pada0: l.drct  = ' , L .
                                 DRCT ) ;
                       WRITELN ( TRACEF , 'pada0: r.drct  = ' , R .
                                 DRCT ) ;
                       WRITELN ( TRACEF , 'pada0: l.displ = ' , L . FPA
                                 . DSPLMT ) ;
                       WRITELN ( TRACEF , 'pada0: r.displ = ' , R . FPA
                                 . DSPLMT ) ;
                     end (* then *) ;
                   if not L . DRCT then
                     LOAD ( L ) ;
                   if R . DRCT then
                     begin
                       L . FPA . DSPLMT := L . FPA . DSPLMT + R . FPA .
                                           DSPLMT ;
                       R . FPA . DSPLMT := 0
                     end (* then *) ;
                   OP1 := XAR ;
                   OP2 := XA ;
                   if R . VRBL then
                     begin

        //******************************************************
        // Q := L . FPA . DSPLMT ;                              
        // L . FPA . DSPLMT := 0 ;                              
        //******************************************************

                       LOAD ( L ) ;
                       if R . DTYPE <> INT then
                         if R . DTYPE = HINT then
                           OP2 := OP2 - 16
                         else
                           LOAD ( R ) ;
                       if R . DRCT then
                         if R . VPA = RGS then
                           begin
                             GENRR ( OP1 , L . RGADR , R . RGADR ) ;
                             AVAIL [ R . RGADR ] := TRUE
                           end (* then *)
                         else
                           begin
                             Q1 := R . MEMADR . DSPLMT ;
                             P1 := R . MEMADR . LVL ;
                             BASE ( Q1 , P1 , B1 ) ;
                             GENRX ( OP2 , L . RGADR , Q1 , B1 , P1 ) ;
                           end (* else *)
                       else
                         begin
                           GETOPERAND ( R , Q1 , P1 , B1 ) ;
                           GENRX ( OP2 , L . RGADR , Q1 , B1 , P1 ) ;
                         end (* else *) ;
                       L . FPA . DSPLMT := Q ;
                     end (* then *) ;
                 end (* tag/ca *) ;

        (****************************************************)
        (* neu 09.2019 : subtrahiere 2 adressen / oppolzer  *)
        (****************************************************)

          PSBA : begin
                   LOAD ( L ) ;
                   OP1 := XSR ;
                   OP2 := XS ;
                   Q := L . FPA . DSPLMT ;
                   L . FPA . DSPLMT := 0 ;
                   LOAD ( R ) ;
                   if R . DRCT then
                     if R . VPA = RGS then
                       begin
                         GENRR ( OP1 , L . RGADR , R . RGADR ) ;
                         AVAIL [ R . RGADR ] := TRUE
                       end (* then *)
                     else
                       begin
                         Q1 := R . MEMADR . DSPLMT ;
                         P1 := R . MEMADR . LVL ;
                         BASE ( Q1 , P1 , B1 ) ;
                         GENRX ( OP2 , L . RGADR , Q1 , B1 , P1 ) ;
                       end (* else *)
                   else
                     begin
                       GETOPERAND ( R , Q1 , P1 , B1 ) ;
                       GENRX ( OP2 , L . RGADR , Q1 , B1 , P1 ) ;
                     end (* else *) ;
                   L . FPA . DSPLMT := Q ;
                 end (* tag/ca *) ;

        (****************************************************)
        (* hier weiter alt - mpi                            *)
        (****************************************************)

          PMPI : begin
                   if R . VRBL then
                     begin
                       if R . DTYPE = HINT then
                         begin
                           if ( not R . DRCT ) or ( R . VPA = MEM )
                           then
                             begin
                               LOAD ( L ) ;
                               GETOPERAND ( R , Q1 , P1 , B1 ) ;
                               GENRX ( XMH , L . RGADR , Q1 , P1 , B1 )
                                       ;
                               goto 30 ;
                             end (* then *) ;
                         end (* then *) ;
                       MDTAG := PMPI ;
                       LOAD ( L ) ;
                       MDTAG := PBGN ;
                       if R . DTYPE <> INT then
                         LOAD ( R )
                       else
                         GETOPERAND ( R , Q1 , P1 , B1 ) ;
                       if ( not R . DRCT ) or ( R . VPA = MEM ) then
                         GENRX ( XM , L . RGADR , Q1 , P1 , B1 )
                       else
                         begin
                           GENRR ( XMR , L . RGADR , R . RGADR ) ;
                           AVAIL [ R . RGADR ] := TRUE ;
                         end (* else *)
                     end (* then *)
                   else

        (************)
        (*NOT R.VRBL*)
        (************)

                     begin
                       Q := 0 ;
                       if ( L . DRCT ) then
                         begin
                           Q := L . FPA . DSPLMT * R . FPA . DSPLMT ;
                           L . FPA . DSPLMT := 0
                         end (* then *)
                       else
                         LOAD ( L ) ;
                       if L . VRBL then
                         begin
                           if ( R . FPA . DSPLMT >= - 32768 ) and ( R .
                           FPA . DSPLMT <= 32767 ) then
                             R . DTYPE := HINT ;
                           P := POWER2 ( R . FPA . DSPLMT ) ;
                           if ( P < 0 ) and ( R . DTYPE <> HINT ) then
                             MDTAG := PMPI ;
                           LOAD ( L ) ;
                           MDTAG := PBGN ;
                           L . FPA . DSPLMT := Q ;
                           if P < 0 then
                             if R . DTYPE <> HINT then
                               GENRXLIT ( XM , L . RGADR , R . FPA .
                                          DSPLMT , 0 )
                             else
                               begin
                                 GENRXLIT ( XMH , L . RGADR , R . FPA .
                                            DSPLMT , - 1 ) ;
                                 goto 30 ;
                               end (* else *)
                           else
                             begin
                               if P > 1 then
                                 GENRS ( XSLL , L . RGADR , 0 , P , 0 )
                               else
                                 if P > 0 then
                                   GENRR ( XAR , L . RGADR , L . RGADR
                                           ) ;
                               goto 30 ;
                             end (* else *) ;
                         end (* then *) ;
                       L . FPA . DSPLMT := Q ;
                     end (* else *) ;
                   if L . VRBL then
                     AVAIL [ L . RGADR ] := TRUE ;
                   L . RGADR := L . RGADR + 1 ;
                   30 :
                   
                 end (* tag/ca *) ;
          PDVI , PMOD :
            if not L . VRBL and not R . VRBL then

        (*****************)
        (* BOTH CONSTANTS*)
        (*****************)

              if R . FPA . DSPLMT = 0 then
                ERROR ( 300 )

        (*******************)
        (* DIVISION BY ZERO*)
        (*******************)

              else
                if PCODE = PDVI then
                  L . FPA . DSPLMT := L . FPA . DSPLMT DIV R . FPA .
                                      DSPLMT
                else
                  L . FPA . DSPLMT := L . FPA . DSPLMT MOD R . FPA .
                                      DSPLMT
            else

        (*********************)
        (* MORE COMMON CASES *)
        (*********************)

              begin
                MDTAG := PDVI ;
                LOAD ( L ) ;
                MDTAG := PBGN ;
                if R . VRBL then
                  begin
                    if R . DTYPE <> INT then
                      LOAD ( R )
                    else
                      GETOPERAND ( R , Q1 , P1 , B1 ) ;
                    if not R . DRCT or ( R . VPA = MEM ) then
                      GENRX ( XD , L . RGADR , Q1 , B1 , P1 )
                    else
                      begin
                        GENRR ( XDR , L . RGADR , R . RGADR ) ;
                        AVAIL [ R . RGADR ] := TRUE
                      end (* else *)
                  end (* then *)
                else

        (*********)
        (*^R.VRBL*)
        (*********)

                  GENRXLIT ( XD , L . RGADR , R . FPA . DSPLMT , 0 ) ;
                if PCODE = PDVI then
                  begin
                    AVAIL [ L . RGADR ] := TRUE ;
                    L . RGADR := L . RGADR + 1
                  end (* then *)
                else
                  AVAIL [ L . RGADR + 1 ] := TRUE ;
              end (* else *) ;
          PEQU , PNEQ , PGRT , PLEQ , PLES , PGEQ :
            begin
              if OPNDTYPE = PSET then
                begin
                  SETCOMPARE ( L , R ) ;
                  STK [ TOP - 1 ] := L ;
                  return
                end (* then *) ;
              if OPNDTYPE = VARC then
                begin
                  STRINGCOMPARE ( L , R ) ;
                  STK [ TOP - 1 ] := L ;
                  return
                end (* then *) ;
              if not LR then
                PCODE := INVBRM [ PCODE ] ;
              case OPNDTYPE of
                ADR , INT , HINT :
                  with R do
                    begin
                      LOAD ( L ) ;
                      if VRBL then
                        begin
                          GETOPERAND ( R , Q1 , P1 , B1 ) ;
                          if ( not DRCT ) or ( VPA = MEM ) then
                            if DTYPE = HINT then
                              GENRX ( XCH , L . RGADR , Q1 , B1 , P1 )
                            else
                              GENRX ( XC , L . RGADR , Q1 , B1 , P1 )
                          else
                            begin
                              GENRR ( XCR , L . RGADR , RGADR ) ;
                              AVAIL [ RGADR ] := TRUE
                            end (* else *)
                        end (* then *)
                      else

        (*************************)
        (*IF NOT VRBL (I.E.CONST)*)
        (*************************)

                        begin
                          if FPA . DSPLMT = 1 then
                            if PCODE = PLES then

        (**********************************)
        (* COMPARISON AGAINST 0 IS BETTER *)
        (**********************************)

                              begin
                                FPA . DSPLMT := 0 ;
                                PCODE := PLEQ
                              end (* then *)
                            else
                              if PCODE = PGEQ then
                                begin
                                  FPA . DSPLMT := 0 ;
                                  PCODE := PGRT
                                end (* then *) ;
                          if FPA . DSPLMT = 0 then
                            GENRR ( XLTR , L . RGADR , L . RGADR )
                          else
                            if ( OPNDTYPE = ADR ) then
                              begin

        //******************************************************
        // CONSTANT OF TYPE ADR = NIL !                         
        // FOLLOWING VALID ONLY IF $D- IS USED                  
        //******************************************************

                                GENRR ( XLTR , L . RGADR , L . RGADR )
                                        ;

        //******************************************************
        // opc is pequ or pneq                                  
        // this logic applies imo                               
        // because nil = -1                                     
        //******************************************************

                                if PCODE = PEQU then
                                  PCODE := PLES
                                else
                                  PCODE := PGEQ ;
                              end (* then *)
                            else
                              GENRXLIT ( XC , L . RGADR , FPA . DSPLMT
                                         , 0 ) ;
                        end (* else *) ;
                      AVAIL [ L . RGADR ] := TRUE ;
                    end (* with *) ;
                BOOL , CHRC :
                  with R do
                    20 :
                    if L . VRBL then
                      if ( L . VPA = RGS ) and L . DRCT then
                        begin
                          if VRBL then
                            if ( VPA = RGS ) and DRCT then
                              begin
                                GENRR ( XCR , L . RGADR , RGADR ) ;
                                AVAIL [ RGADR ] := TRUE ;
                              end (* then *)
                            else
                              begin
                                GETQB ( R , Q1 , B1 , 0 ) ;
                                Q := XCLI * SL24 + B1 * SL12 + Q1 ;
                                GENRXLIT ( XEX , L . RGADR , Q , 0 ) ;
                                PCODE := INVBRM [ PCODE ] ;
                              end (* else *)
                          else
                            if FPA . DSPLMT = 0 then
                              GENRR ( XLTR , L . RGADR , L . RGADR )
                            else
                              begin
                                LOAD ( R ) ;
                                goto 20
                              end (* else *) ;
                          AVAIL [ L . RGADR ] := TRUE ;
                        end (* then *)
                      else

        (******************)
        (* L IS IN MEMORY *)
        (******************)

                        if VRBL then
                          begin
                            CLEAR_REG := FALSE ;
                            LOAD ( STK [ ROP ] ) ;
                            CLEAR_REG := TRUE ;
                            LR := not LR ;
                            goto 10 ;
                          end (* then *)
                        else
                          begin
                            GETQB ( L , Q1 , B1 , 0 ) ;
                            GENSI ( XCLI , Q1 , B1 , FPA . DSPLMT ) ;
                          end (* else *)
                    else

        (*******************)
        (* L IS A CONSTANT *)
        (*******************)

                      if VRBL then
                        begin
                          LR := not LR ;
                          goto 10
                        end (* then *)
                      else
                        begin
                          LOAD ( STK [ ROP ] ) ;
                          goto 10
                        end (* else *) ;
                REEL : with R do
                         begin
                           LOAD ( L ) ;
                           if VRBL then
                             begin
                               GETOPERAND ( R , Q1 , P1 , B1 ) ;
                               if ( VPA = RGS ) and DRCT then
                                 begin
                                   GENRR ( XCDR , L . RGADR , R . RGADR
                                           ) ;
                                   AVAILFP [ RGADR ] := TRUE
                                 end (* then *)
                               else

        (*************************)
        (* VPA = MEM OR NOT DRCT *)
        (*************************)

                                 GENRX ( XCD , L . RGADR , Q1 , B1 , P1
                                         )
                             end (* then *)
                           else

        (************)
        (* CONSTANT *)
        (************)

                             if RCNST = 0.0 then
                               GENRR ( XLTDR , L . RGADR , L . RGADR )
                             else
                               GENRXDLIT ( XCD , L . RGADR , RCNST ) ;
                           AVAILFP [ L . RGADR ] := TRUE ;
                         end (* with *) ;
                CARR : begin
                         SOPERATION ( L , R , PCODE , Q ) ;
                         OLDCSP := PSIO ;
                       end (* tag/ca *)
              end (* case *) ;
              BRCND := BRMSK [ PCODE ] ;
            end (* tag/ca *) ;
          PAND , PIOR , PXOR :
            with R do
              begin
                OP1 := XNR ;
                if PCODE = PIOR then
                  OP1 := XORX
                else
                  if PCODE = PXOR then
                    OP1 := XXR ;
                LOAD ( L ) ;
                LOAD ( R ) ;

        (*****************************************************)
        (* THIS CAN BE IMPROVED BY USING THE CONDITION CODE  *)
        (* AS THE TOP ELEMENT                                *)
        (*****************************************************)

                GENRR ( OP1 , L . RGADR , RGADR ) ;
                AVAIL [ RGADR ] := TRUE ;
              end (* with *) ;
          PADR , PSBR :
            begin
              OP1 := XADR ;
              OP2 := XAD ;
              if PCODE = PSBR then
                begin
                  OP1 := XSDR ;
                  OP2 := XSD
                end (* then *) ;
              LOAD ( L ) ;
              if R . VRBL then
                begin
                  GETOPERAND ( R , Q1 , P1 , B1 ) ;
                  if ( R . VPA = RGS ) and R . DRCT then
                    begin
                      GENRR ( OP1 , L . RGADR , R . RGADR ) ;
                      AVAILFP [ R . RGADR ] := TRUE
                    end (* then *)
                  else

        (*************************)
        (* VPA = MEM OR NOT DRCT *)
        (*************************)

                    GENRX ( OP2 , L . RGADR , Q1 , B1 , P1 )
                end (* then *)
              else

        (************)
        (* CONSTANT *)
        (************)

                GENRXDLIT ( OP2 , L . RGADR , R . RCNST )
            end (* tag/ca *) ;
          PDVR , PMPR :
            begin
              LOAD ( L ) ;
              OP1 := XDDR ;
              OP2 := XDD ;
              if PCODE = PMPR then
                begin
                  OP1 := XMDR ;
                  OP2 := XMD
                end (* then *) ;
              if R . VRBL then
                begin
                  GETOPERAND ( R , Q1 , P1 , B1 ) ;
                  if ( R . VPA = RGS ) and R . DRCT then
                    begin
                      GENRR ( OP1 , L . RGADR , R . RGADR ) ;
                      AVAILFP [ R . RGADR ] := TRUE
                    end (* then *)
                  else

        (***************************)
        (* R.VPA = MEM OR NOT DRCT *)
        (***************************)

                    GENRX ( OP2 , L . RGADR , Q1 , B1 , P1 )
                end (* then *)
              else

        (**************)
        (*  CONSTANT  *)
        (**************)

                GENRXDLIT ( OP2 , L . RGADR , R . RCNST )
            end (* tag/ca *) ;
        end (* case *) ;
        STK [ TOP - 1 ] := L ;
      end (* BOPERATION *) ;


   procedure ASSIGN_STRING ( TARGET , SOURCE : DATUM ; LEN_REG : RGRNG
                           ; LEN_OFFS : ADRRNG ) ;

      begin (* ASSIGN_STRING *)
        if FALSE then
          begin
            WRITELN ( TRACEF , 'start assign_string, linecnt = ' ,
                      LINECNT : 1 ) ;
            WRITE ( TRACEF , 'target: ' ) ;
            DUMPSTKELEM ( TARGET ) ;
            WRITE ( TRACEF , 'source: ' ) ;
            DUMPSTKELEM ( SOURCE ) ;
            WRITELN ( TRACEF , 'len_reg  = ' , LEN_REG ) ;
            WRITELN ( TRACEF , 'len_offs = ' , LEN_OFFS ) ;
          end (* then *) ;
        if TARGET . DTYPE = VARC then
          TARGET . FPA . DSPLMT := TARGET . FPA . DSPLMT + 4 ;
        if SOURCE . DTYPE = VARC then
          SOURCE . FPA . DSPLMT := SOURCE . FPA . DSPLMT + 4 ;
        if SOURCE . DTYPE = CHRC then
          begin
            if SOURCE . VRBL then
              STROPERATION_MVC1 ( TARGET , SOURCE )
            else
              STROPERATION_MVI ( TARGET , CHR ( SOURCE . FPA . DSPLMT )
                                 ) ;
            return
          end (* then *) ;
        if LEN_REG <> 0 then
          begin
            STROPERATION_LEN ( TARGET , SOURCE , PMOV , LEN_REG ,
                               LEN_OFFS ) ;
            return ;
          end (* then *) ;
        SOPERATION ( TARGET , SOURCE , PMOV , SOURCE . PLEN ) ;
      end (* ASSIGN_STRING *) ;


   procedure STRINGOPS ;

      var P1 , B1 , P2 , B2 : LVLRNG ;
          Q1 , Q2 : ADRRNG ;
          B : LVLRNG ;
          MAXL : INTEGER ;
          LEN : INTEGER ;
          COPYSTRING : BOOLEAN ;
          RGWORK : RGRNG ;
          PATBLANK : DATUM ;
          DATLEN : DATUM ;
          LITVALUE : INTEGER ;
          DATWORKAREA : DATUM ;
          LEN1 , LEN2 : INTEGER ;
          LEN_NEW : INTEGER ;
          LEN_REG : RGRNG ;
          LEN_OFFS : ADRRNG ;


      procedure WORK_VCC ;

         var LBL : PLABEL ;
             DO_STATICWORK : BOOLEAN ;

         begin (* WORK_VCC *)
           DO_STATICWORK := TRUE ;

           //*********************************************
           // get lengths of both strings on stack        
           //*********************************************

           DATWORKAREA := DATNULL ;
           with DATWORKAREA do
             begin
               DTYPE := VARC ;
               VRBL := TRUE ;
               DRCT := TRUE ;
               VPA := RGS ;
               RGADR := TXRG ;
             end (* with *) ;
           LEN1 := STK [ TOP - 1 ] . PLEN ;
           LEN2 := STK [ TOP - 2 ] . PLEN ;
           if ( LEN1 > 0 ) and ( LEN2 > 0 ) then
             begin

           //*********************************************
           // if both lengths are known at compile time   
           // build string descriptor element in          
           // workarea - load strcurr pointer first       
           //*********************************************

               LEN_NEW := LEN1 + LEN2 ;
               FINDRG ;
               RGWORK := NXTRG ;
               P1 := 1 ;
               Q1 := STRCURR ;
               BASE ( Q1 , P1 , B1 ) ;
               GENRX ( XL , RGWORK , Q1 , B1 , P1 ) ;
               LITVALUE := LEN_NEW * 65536 + LEN_NEW ;
               GENRXLIT ( XL , TXRG , LITVALUE , 1 ) ;
               GENRX ( XST , TXRG , 0 , RGWORK , 0 ) ;
               GENRR ( XLR , TXRG , RGWORK ) ;

           //*********************************************
           // concatenate strings in workarea and         
           // store new strcurr pointer                   
           //*********************************************

               ASSIGN_STRING ( DATWORKAREA , STK [ TOP - 2 ] , 0 , 0 )
                               ;
               if LEN2 < 4096 then
                 GENRX ( XLA , TXRG , LEN2 , TXRG , 0 )
               else
                 GENRXLIT ( XAH , TXRG , LEN2 , 1 ) ;
               ASSIGN_STRING ( DATWORKAREA , STK [ TOP - 1 ] , 0 , 0 )
                               ;
               if LEN1 + 4 < 4096 then
                 GENRX ( XLA , TXRG , LEN1 + 4 , TXRG , 0 )
               else
                 GENRXLIT ( XAH , TXRG , LEN1 + 4 , 1 ) ;
               GENRX ( XST , TXRG , Q1 , B1 , P1 ) ;
             end (* then *)
           else
             begin

           //*********************************************
           // if one of the lengths is not known          
           // at compile time                             
           // build string descriptor element in          
           // workarea from existent string descriptors   
           //*********************************************

               LEN_NEW := - 1 ;
               FINDRG ;
               RGWORK := NXTRG ;
               P1 := 1 ;
               Q1 := STRCURR ;
               BASE ( Q1 , P1 , B1 ) ;
               if LEN1 > 0 then
                 begin
                   GENRX ( XL , RGWORK , Q1 , B1 , P1 ) ;

           //*********************************************
           // length 1 ist known, that is                 
           // the length of the second operand            
           //*********************************************

                   with STK [ TOP - 2 ] do
                     if VPA = RGS then
                       begin
                         GENRX ( XLH , TXRG , 2 , RGADR , 0 )
                       end (* then *)
                     else
                       begin
                         P2 := FPA . LVL ;
                         Q2 := FPA . DSPLMT + 2 ;
                         BASE ( Q2 , P2 , B2 ) ;
                         GENRX ( XLH , TXRG , Q2 , B2 , P2 ) ;
                       end (* else *) ;
                   GENRS ( XSLL , TXRG , 0 , 16 , 0 ) ;
                   LITVALUE := LEN1 ;
                   GENRXLIT ( XA , TXRG , LITVALUE , 1 ) ;
                   GENRX ( XST , TXRG , 0 , RGWORK , 0 ) ;
                   GENRR ( XLR , TXRG , RGWORK ) ;

           //*********************************************
           // concatenate strings in workarea and         
           // store new strcurr pointer                   
           //*********************************************

                   ASSIGN_STRING ( DATWORKAREA , STK [ TOP - 2 ] ,
                                   RGWORK , 0 ) ;
                   GENRX ( XAH , TXRG , 0 , RGWORK , 0 ) ;
                   ASSIGN_STRING ( DATWORKAREA , STK [ TOP - 1 ] , 0 ,
                                   0 ) ;
                 end (* then *)
               else
                 if LEN2 > 0 then
                   begin
                     GENRX ( XL , RGWORK , Q1 , B1 , P1 ) ;

           //*********************************************
           // length 2 ist known, that is                 
           // the length of the first operand             
           //*********************************************

                     LITVALUE := LEN2 * 65536 ;
                     GENRXLIT ( XL , TXRG , LITVALUE , 1 ) ;
                     with STK [ TOP - 1 ] do
                       if VPA = RGS then
                         begin
                           GENRX ( XAH , TXRG , 2 , RGADR , 0 )
                         end (* then *)
                       else
                         begin
                           P2 := FPA . LVL ;
                           Q2 := FPA . DSPLMT + 2 ;
                           BASE ( Q2 , P2 , B2 ) ;
                           GENRX ( XAH , TXRG , Q2 , B2 , P2 ) ;
                         end (* else *) ;
                     GENRX ( XST , TXRG , 0 , RGWORK , 0 ) ;
                     GENRR ( XLR , TXRG , RGWORK ) ;

           //*********************************************
           // concatenate strings in workarea and         
           // store new strcurr pointer                   
           //*********************************************

                     ASSIGN_STRING ( DATWORKAREA , STK [ TOP - 2 ] , 0
                                     , 0 ) ;
                     GENRX ( XAH , TXRG , 0 , RGWORK , 0 ) ;
                     ASSIGN_STRING ( DATWORKAREA , STK [ TOP - 1 ] ,
                                     RGWORK , 2 ) ;
                   end (* then *)
                 else
                   begin

           //*********************************************
           // both lengths are unknown                    
           // if rgwork is not reg2 or reg3,              
           // there will be a register conflict           
           // (regs 4 to 7 are needed for MVCL)           
           // in this case: fall back to subroutine       
           //*********************************************

                     if not ( RGWORK in [ 2 , 3 ] ) then
                       begin
                         AVAIL [ RGWORK ] := TRUE ;
                         GENRX ( XL , TRG1 , STRCURR , 12 , 0 ) ;
                         with STK [ TOP - 2 ] do
                           if VPA = RGS then
                             begin
                               GENLA_LR ( TXRG , 0 , RGADR , 0 )
                             end (* then *)
                           else
                             begin
                               P2 := FPA . LVL ;
                               Q2 := FPA . DSPLMT + 2 ;
                               BASE ( Q2 , P2 , B2 ) ;
                               GENLA_LR ( TXRG , Q2 , B2 , P2 ) ;
                             end (* else *) ;

           //************************************************
           // store adr of first string to                   
           // first parm position                            
           //************************************************

                         GENRX ( XST , TXRG , 0 , TRG1 , 0 ) ;
                         with STK [ TOP - 1 ] do
                           if VPA = RGS then
                             begin
                               GENLA_LR ( TXRG , 0 , RGADR , 0 )
                             end (* then *)
                           else
                             begin
                               P2 := FPA . LVL ;
                               Q2 := FPA . DSPLMT + 2 ;
                               BASE ( Q2 , P2 , B2 ) ;
                               GENLA_LR ( TXRG , Q2 , B2 , P2 ) ;
                             end (* else *) ;

           //************************************************
           // store adr of second string to                  
           // second parm position                           
           //************************************************

                         GENRX ( XST , TXRG , 4 , TRG1 , 0 ) ;

           //******************************************************
           // free the registers possibly in use                   
           // in the left and right operands                       
           //******************************************************

                         FREEREG ( STK [ TOP - 2 ] ) ;
                         FREEREG ( STK [ TOP - 1 ] ) ;

           //******************************************************
           // call the $PASSCMP routine (in PASMONN)               
           //******************************************************

                         LBL . NAM := '$PASSVCC' ;
                         LBL . LEN := 8 ;
                         GENRXLAB ( XL , TRG15 , LBL , - 3 ) ;
                         GENRR ( XBALR , 14 , 15 ) ;

           //******************************************************
           // indicate loss of reg 1 and reg 15                    
           //******************************************************

                         CSPREGACTIVE := FALSE ;
                         OLDCSP := PSIO ;

           //******************************************************
           // load result string address in target reg             
           //******************************************************

                         FINDRG ;
                         RGWORK := NXTRG ;
                         GENRX ( XL , RGWORK , 12 , TRG1 , 0 ) ;
                         GENRX ( XST , RGWORK , STRCURR , 12 , 0 ) ;
                         GENRX ( XL , RGWORK , 8 , TRG1 , 0 ) ;
                         DO_STATICWORK := FALSE ;
                       end (* then *)
                     else
                       begin
                         GENRX ( XL , RGWORK , Q1 , B1 , P1 ) ;
                         with STK [ TOP - 2 ] do
                           if VPA = RGS then
                             begin
                               GENRX ( XLH , TXRG , 2 , RGADR , 0 )
                             end (* then *)
                           else
                             begin
                               P2 := FPA . LVL ;
                               Q2 := FPA . DSPLMT + 2 ;
                               BASE ( Q2 , P2 , B2 ) ;
                               GENRX ( XLH , TXRG , Q2 , B2 , P2 ) ;
                             end (* else *) ;
                         GENRS ( XSLL , TXRG , 0 , 16 , 0 ) ;
                         with STK [ TOP - 1 ] do
                           if VPA = RGS then
                             begin
                               GENRX ( XAH , TXRG , 2 , RGADR , 0 )
                             end (* then *)
                           else
                             begin
                               P2 := FPA . LVL ;
                               Q2 := FPA . DSPLMT + 2 ;
                               BASE ( Q2 , P2 , B2 ) ;
                               GENRX ( XAH , TXRG , Q2 , B2 , P2 ) ;
                             end (* else *) ;
                         GENRX ( XST , TXRG , 0 , RGWORK , 0 ) ;
                         GENRR ( XLR , TXRG , RGWORK ) ;

           //*********************************************
           // concatenate strings in workarea and         
           // store new strcurr pointer                   
           //*********************************************

                         ASSIGN_STRING ( DATWORKAREA , STK [ TOP - 2 ]
                                         , RGWORK , 0 ) ;
                         GENRX ( XAH , TXRG , 0 , RGWORK , 0 ) ;
                         ASSIGN_STRING ( DATWORKAREA , STK [ TOP - 1 ]
                                         , RGWORK , 2 ) ;
                       end (* else *)
                   end (* else *) ;
               if DO_STATICWORK then
                 begin
                   GENRX ( XAH , TXRG , 2 , RGWORK , 0 ) ;
                   GENRX ( XLA , TXRG , 4 , TXRG , 0 ) ;
                   GENRX ( XST , TXRG , Q1 , B1 , P1 ) ;
                   GENRX ( XLH , TXRG , 0 , RGWORK , 0 ) ;
                   GENRX ( XAH , TXRG , 2 , RGWORK , 0 ) ;
                   GENRX ( XSTH , TXRG , 0 , RGWORK , 0 ) ;
                   GENRX ( XSTH , TXRG , 2 , RGWORK , 0 ) ;
                 end (* then *)
             end (* else *) ;

           //*********************************************
           // new string is addressed by register         
           // in workarea - points to string descriptor   
           //*********************************************

           TOP := TOP - 1 ;
           with STK [ TOP - 1 ] do
             begin
               DTYPE := VARC ;
               PLEN := LEN_NEW ;
               VRBL := TRUE ;
               DRCT := TRUE ;
               VPA := RGS ;
               RGADR := RGWORK ;
               FPA . LVL := 0 ;
               FPA . DSPLMT := 0 ;
               MEMADR . LVL := 0 ;
               MEMADR . DSPLMT := 0 ;
             end (* with *)
         end (* WORK_VCC *) ;


      procedure WORK_VST ;

         begin (* WORK_VST *)

           //************************************************
           // p = mode (0 or 1)                              
           // q = maxLength of Target                        
           //************************************************

           if P = 0 then
             if Q > 0 then
               begin

           //*********************************************
           // VST 0,n (n > 0) is used to store strings    
           // TOP - 2 contains the target address         
           // Maxlength at target addr is set to n        
           // TOP - 1 contains the varchar                
           // two items popped                            
           //*********************************************
           //*********************************************
           // fetch length from stack                     
           // element and store length and maxlength      
           // in string                                   
           // target - literal consisting of two halfwords
           //*********************************************

                 MAXL := Q ;
                 LEN := STK [ TOP - 1 ] . PLEN ;
                 LEN_REG := 0 ;
                 LEN_OFFS := 0 ;
                 GETADR2 ( STK [ TOP - 2 ] , Q1 , P1 , B1 ) ;
                 CONS_REGS ( B1 , P1 ) ;
                 FINDRG ;
                 if LEN > 0 then
                   begin

           //******************************************************
           // in this case len_reg remains at zero,                
           // so assign_string uses the compile time               
           // len to control the transport                         
           //******************************************************

                     LITVALUE := MAXL * 65536 + LEN ;
                     GENRXLIT ( XL , NXTRG , LITVALUE , 1 ) ;
                   end (* then *)
                 else
                   begin
                     LITVALUE := MAXL * 65536 ;
                     GENRXLIT ( XL , NXTRG , LITVALUE , 1 ) ;
                     with STK [ TOP - 1 ] do
                       begin
                         if VPA = RGS then
                           begin

           //******************************************************
           // this is done to make assign_string                   
           // fetch the length from the source                     
           // string descriptor                                    
           //******************************************************

                             LEN_REG := RGADR ;
                             LEN_OFFS := 2 ;
                             GENRX ( XAH , NXTRG , 2 , RGADR , 0 ) ;
                           end (* then *)
                         else
                           begin
                             P2 := FPA . LVL ;
                             Q2 := FPA . DSPLMT + 2 ;
                             BASE ( Q2 , P2 , B2 ) ;
                             GENRX ( XAH , NXTRG , Q2 , B2 , P2 ) ;

           //******************************************************
           // this is done to make assign_string                   
           // fetch the length from the target                     
           // string descriptor                                    
           //******************************************************

                             LEN_REG := B1 ;
                             LEN_OFFS := Q1 + 2 ;
                           end (* else *) ;
                       end (* with *)
                   end (* else *) ;
                 GENRX ( XST , NXTRG , Q1 , B1 , P1 ) ;
                 AVAIL [ NXTRG ] := TRUE ;

           //*********************************************
           // assign string to stk [top - 2 ]             
           //*********************************************

                 STK [ TOP - 2 ] . DTYPE := VARC ;
                 ASSIGN_STRING ( STK [ TOP - 2 ] , STK [ TOP - 1 ] ,
                                 LEN_REG , LEN_OFFS ) ;
                 FREEREG ( STK [ TOP - 1 ] ) ;
                 FREEREG ( STK [ TOP - 2 ] ) ;
                 TOP := TOP - 2
               end (* then *)
             else
               if Q = 0 then
                 begin

           //*********************************************
           // VST 0,0 is used to store                    
           // string from stack to target addr            
           // actual length of string must be less        
           // or equal than maxlength of target (!)       
           // TOP - 2 = target addr of String variable    
           // TOP - 1 = source varchar (String on stack)  
           // two items popped                            
           //*********************************************

                   GETADR2 ( STK [ TOP - 2 ] , Q1 , P1 , B1 ) ;
                   FINDRG ;
                   GENLA_LR ( NXTRG , Q1 , B1 , P1 ) ;
                   with STK [ TOP - 2 ] do
                     begin
                       DTYPE := VARC ;
                       VRBL := TRUE ;
                       DRCT := TRUE ;
                       VPA := RGS ;
                       RGADR := NXTRG ;
                       FPA . LVL := 0 ;
                       FPA . DSPLMT := 0 ;
                       MEMADR . LVL := 0 ;
                       MEMADR . DSPLMT := 0 ;
                     end (* with *) ;
                   GENRX ( XLH , 14 , 0 , NXTRG , 0 ) ;
                   with STK [ TOP - 1 ] do
                     begin
                       if VPA = RGS then
                         begin
                           LEN_REG := RGADR ;
                           LEN_OFFS := 2 ;
                         end (* then *)
                       else
                         begin
                           P2 := FPA . LVL ;
                           Q2 := FPA . DSPLMT + 2 ;
                           BASE ( Q2 , P2 , B2 ) ;
                           CONS_REGS ( B2 , P2 ) ;
                           LEN_REG := B2 ;
                           LEN_OFFS := Q2 ;
                         end (* else *) ;
                     end (* with *) ;
                   GENRX ( XCH , 14 , LEN_OFFS , LEN_REG , 0 ) ;
                   GENRS ( XSLL , 14 , 0 , 16 , 0 ) ;
                   GENRX ( XAH , 14 , LEN_OFFS , LEN_REG , 0 ) ;
                   GENRX ( XST , 14 , 0 , NXTRG , 0 ) ;

           //*********************************************
           // assign string to stk [top - 2 ]             
           //*********************************************

                   ASSIGN_STRING ( STK [ TOP - 2 ] , STK [ TOP - 1 ] ,
                                   LEN_REG , LEN_OFFS ) ;
                   AVAIL [ NXTRG ] := TRUE ;
                   FREEREG ( STK [ TOP - 1 ] ) ;
                   FREEREG ( STK [ TOP - 2 ] ) ;
                   TOP := TOP - 2
                 end (* then *)
               else
                 begin

           //*********************************************
           // VST 0,-1 is used to move "String on stack"  
           // representation to memory (8 bytes)          
           // used for function results (conformant       
           // String type as function result type)        
           // pop 2 stack items                           
           // stack is empty after that                   
           // the function result is pushed to the stack  
           // by the RET instruction                      
           //*********************************************

                   GETADR2 ( STK [ TOP - 2 ] , Q1 , P1 , B1 ) ;
                   FINDRG ;
                   GENLA_LR ( NXTRG , Q1 , B1 , P1 ) ;
                   with STK [ TOP - 2 ] do
                     begin
                       DTYPE := VARC ;
                       VRBL := TRUE ;
                       DRCT := TRUE ;
                       VPA := RGS ;
                       RGADR := NXTRG ;
                       FPA . LVL := 0 ;
                       FPA . DSPLMT := 0 ;
                       MEMADR . LVL := 0 ;
                       MEMADR . DSPLMT := 0 ;
                     end (* with *) ;
                   LITVALUE := - 65536 ;
                   GENRXLIT ( XL , 14 , LITVALUE , 1 ) ;
                   with STK [ TOP - 1 ] do
                     begin
                       if PLEN = 0 then
                         begin
                           GENRX ( XST , 14 , 0 , NXTRG , 0 ) ;
                         end (* then *)
                       else
                         begin
                           if VPA = RGS then
                             begin
                               LEN_REG := RGADR ;
                               LEN_OFFS := 0 ;
                             end (* then *)
                           else
                             begin
                               P2 := FPA . LVL ;
                               Q2 := FPA . DSPLMT ;
                               BASE ( Q2 , P2 , B2 ) ;
                               CONS_REGS ( B2 , P2 ) ;
                               LEN_REG := B2 ;
                               LEN_OFFS := Q2 ;
                             end (* else *) ;
                           GENRX ( XAH , 14 , LEN_OFFS + 2 , LEN_REG ,
                                   0 ) ;
                           GENRX ( XST , 14 , 0 , NXTRG , 0 ) ;
                           if VPA <> RGS then
                             begin
                               RGWORK := NXTRG ;
                               FINDRP ;
                               P1 := NXTRG ;
                               B1 := NXTRG + 1 ;
                               GENRX ( XL , P1 , STRCURR , 12 , 0 ) ;
                               GENRX ( XST , P1 , 4 , RGWORK , 0 ) ;
                               GENRX ( XLH , B1 , LEN_OFFS + 2 ,
                                       LEN_REG , 0 ) ;
                               FINDRP ;
                               GENRX ( XLA , NXTRG , LEN_OFFS + 4 ,
                                       LEN_REG , 0 ) ;
                               GENRR ( XLR , NXTRG + 1 , B1 ) ;
                               P2 := NXTRG ;
                               B2 := NXTRG + 1 ;
                               GENRR ( XMVCL , P1 , P2 ) ;
                               GENRX ( XST , P1 , STRCURR , 12 , 0 ) ;
                               AVAIL [ P1 ] := TRUE ;
                               AVAIL [ B1 ] := TRUE ;
                               AVAIL [ P2 ] := TRUE ;
                               AVAIL [ B2 ] := TRUE ;
                               S370CNT := S370CNT + 1 ;
                             end (* then *)
                           else
                             begin
                               GENRX ( XLA , 14 , LEN_OFFS + 4 ,
                                       LEN_REG , 0 ) ;
                               GENRX ( XST , 14 , 4 , NXTRG , 0 ) ;
                             end (* else *)
                         end (* else *) ;
                     end (* with *) ;
                   AVAIL [ NXTRG ] := TRUE ;
                   FREEREG ( STK [ TOP - 1 ] ) ;
                   FREEREG ( STK [ TOP - 2 ] ) ;
                   TOP := TOP - 2
                 end (* else *)
           else
             if Q > 0 then
               begin

           //*********************************************
           // VST 1,n (n > 0) is used to move strings     
           // to a parameter list (value parameters)      
           // TOP - 1 contains the target address         
           // Maxlength at target addr is set to n        
           // TOP - 2 contains the varchar                
           // two items popped                            
           //*********************************************
           //*********************************************
           // fetch length from stack                     
           // element and store length and maxlength      
           // in string                                   
           // target - literal consisting of two halfwords
           //*********************************************

                 MAXL := Q ;
                 LEN := STK [ TOP - 2 ] . PLEN ;
                 LEN_REG := 0 ;
                 LEN_OFFS := 0 ;
                 GETADR2 ( STK [ TOP - 1 ] , Q1 , P1 , B1 ) ;
                 CONS_REGS ( B1 , P1 ) ;
                 FINDRG ;
                 if LEN > 0 then
                   begin

           //******************************************************
           // in this case len_reg remains at zero,                
           // so assign_string uses the compile time               
           // len to control the transport                         
           //******************************************************

                     LITVALUE := MAXL * 65536 + LEN ;
                     GENRXLIT ( XL , NXTRG , LITVALUE , 1 ) ;
                   end (* then *)
                 else
                   begin
                     LITVALUE := MAXL * 65536 ;
                     GENRXLIT ( XL , NXTRG , LITVALUE , 1 ) ;
                     with STK [ TOP - 2 ] do
                       begin
                         if VPA = RGS then
                           begin

           //******************************************************
           // this is done to make assign_string                   
           // fetch the length from the source                     
           // string descriptor                                    
           //******************************************************

                             LEN_REG := RGADR ;
                             LEN_OFFS := 2 ;
                             GENRX ( XAH , NXTRG , 2 , RGADR , 0 ) ;
                           end (* then *)
                         else
                           begin
                             P2 := FPA . LVL ;
                             Q2 := FPA . DSPLMT + 2 ;
                             BASE ( Q2 , P2 , B2 ) ;
                             GENRX ( XAH , NXTRG , Q2 , B2 , P2 ) ;

           //******************************************************
           // this is done to make assign_string                   
           // fetch the length from the target                     
           // string descriptor                                    
           //******************************************************

                             LEN_REG := B1 ;
                             LEN_OFFS := Q1 + 2 ;
                           end (* else *) ;
                       end (* with *)
                   end (* else *) ;
                 GENRX ( XST , NXTRG , Q1 , B1 , P1 ) ;
                 AVAIL [ NXTRG ] := TRUE ;

           //*********************************************
           // assign string to stk [top - 2 ]             
           //*********************************************

                 STK [ TOP - 1 ] . DTYPE := VARC ;
                 ASSIGN_STRING ( STK [ TOP - 1 ] , STK [ TOP - 2 ] ,
                                 LEN_REG , LEN_OFFS ) ;
                 FREEREG ( STK [ TOP - 1 ] ) ;
                 FREEREG ( STK [ TOP - 2 ] ) ;
                 TOP := TOP - 2
               end (* then *)
             else
               if Q = 0 then
                 begin
                   
                 end (* then *)
               else
                 begin

           //*********************************************
           // VST 1,-1 is used to store 8 bytes           
           // string on stack representation to           
           // a procedure parameter list, for example     
           // TOP - 1 contains the target address         
           // TOP - 2 contains the varchar                
           // two items popped, the target is pushed      
           //*********************************************

                   GETADR2 ( STK [ TOP - 1 ] , Q1 , P1 , B1 ) ;
                   LITVALUE := - 65536 ;
                   GENRXLIT ( XL , 14 , LITVALUE , 1 ) ;
                   with STK [ TOP - 2 ] do
                     begin
                       if VPA = RGS then
                         begin
                           LEN_REG := RGADR ;
                           LEN_OFFS := 2 ;
                           GENRX ( XAH , 14 , 2 , RGADR , 0 ) ;
                         end (* then *)
                       else
                         begin
                           P2 := FPA . LVL ;
                           Q2 := FPA . DSPLMT + 2 ;
                           BASE ( Q2 , P2 , B2 ) ;
                           CONS_REGS ( B2 , P2 ) ;
                           LEN_REG := B2 ;
                           LEN_OFFS := Q2 ;
                           GENRX ( XAH , 14 , Q2 , B2 , 0 ) ;
                         end (* else *) ;
                     end (* with *) ;
                   FINDRG ;
                   GENRX ( XLA , NXTRG , Q1 , B1 , P1 ) ;
                   GENRX ( XST , 14 , 0 , NXTRG , 0 ) ;
                   GENRX ( XLH , 14 , LEN_OFFS - 2 , LEN_REG , 0 ) ;
                   GENRR ( XLTR , 14 , 14 ) ;
                   GENRELRX ( XBC , GEQCND , 6 ) ;
                   GENRX ( XL , 14 , LEN_OFFS + 2 , LEN_REG , 0 ) ;
                   GENRELRX ( XBC , ANYCND , 4 ) ;
                   GENRX ( XLA , 14 , LEN_OFFS + 2 , LEN_REG , 0 ) ;
                   GENRX ( XST , 14 , 4 , NXTRG , 0 ) ;
                   AVAIL [ NXTRG ] := TRUE ;
                   FREEREG ( STK [ TOP - 2 ] ) ;
                   STK [ TOP - 2 ] := STK [ TOP - 1 ] ;
                   TOP := TOP - 1
                 end (* else *)
         end (* WORK_VST *) ;


      begin (* STRINGOPS *)
        if TRUE then
          begin
            WRITE ( TRACEF , 'start stringops - pcode = ' , PCODE ) ;
            WRITELN ( TRACEF , ' linecnt = ' , LINECNT : 1 ) ;
            WRITELN ( TRACEF , 'start stringops - p = ' , P ) ;
            WRITELN ( TRACEF , 'start stringops - q = ' , Q ) ;
            DUMPSTK ( 1 , TOP - 1 ) ;
          end (* then *) ;
        case PCODE of

        //*******************************************************
        // varchar push: save string workarea address            
        //*******************************************************

          PVPU : begin

        //*********************************************
        // save strcurr ptr into given location        
        //*********************************************

                   FINDRG ;
                   P2 := P ;
                   BASE ( Q , P2 , B ) ;
                   GENRX ( XL , NXTRG , STRCURR , 12 , 0 ) ;
                   GENRX ( XST , NXTRG , Q , B , P2 ) ;
                   AVAIL [ NXTRG ] := TRUE
                 end (* tag/ca *) ;

        //*******************************************************
        // varchar pop: restore string workarea address          
        //*******************************************************

          PVPO : begin

        //*********************************************
        // restore strcurr ptr from given location     
        //*********************************************

                   FINDRG ;
                   P2 := P ;
                   BASE ( Q , P2 , B ) ;
                   GENRX ( XL , NXTRG , Q , B , P2 ) ;
                   GENRX ( XST , NXTRG , STRCURR , 12 , 0 ) ;
                   AVAIL [ NXTRG ] := TRUE
                 end (* tag/ca *) ;

        //*******************************************************
        // varchar convert 1: convert single char to string      
        //*******************************************************

          PVC1 : with STK [ TOP - 1 ] do
                   begin

        //*********************************************
        // address of char array is on stack           
        // VC1 converts single char to string          
        // of length 1                                 
        // set plen of stack item to 1                 
        // datatype to varc                            
        //*********************************************

                     PLEN := 1 ;
                     DTYPE := CHRC ;
                   end (* with *) ;

        //*******************************************************
        // varchar convert 2: convert char array to string       
        //*******************************************************

          PVC2 : if Q > 0 then
                   with STK [ TOP - 1 ] do
                     begin

        //*********************************************
        // address of char array is on stack           
        // VC2 converts char array to string           
        // of length q                                 
        // q = instruction operand                     
        // set plen of stack item to q                 
        // datatype to varc                            
        //*********************************************

                       PLEN := Q ;
                       DTYPE := CARR ;
                     end (* with *)
                 else
                   begin
                     TOP := TOP + 1 ;
                     STK [ TOP - 1 ] := DATNULL ;
                     with STK [ TOP - 1 ] do
                       begin
                         PLEN := 0 ;
                         DTYPE := VARC ;
                       end (* with *) ;
                   end (* else *) ;

        //*******************************************************
        // varchar store: store string to memory                 
        //*******************************************************

          PVST : WORK_VST ;

        //*******************************************************
        // varchar load: load string from memory to stack        
        //*******************************************************

          PVLD : begin
                   COPYSTRING := ( P <> 0 ) ;
                   LEN := Q ;
                   if not COPYSTRING then
                     with STK [ TOP - 1 ] do
                       begin
                         PLEN := - 1 ;
                         DTYPE := VARC ;
                         if Q = 0 then
                           begin
                             FINDRG ;
                             P2 := MEMADR . LVL ;
                             Q2 := MEMADR . DSPLMT ;
                             BASE ( Q2 , P2 , B2 ) ;
                             GENRX ( XL , NXTRG , Q2 , B2 , P2 ) ;
                             FPA := ZEROBL ;
                             VPA := RGS ;
                             MEMADR := ZEROBL ;
                             RGADR := NXTRG ;
                           end (* then *)
                       end (* with *)
                 end (* tag/ca *) ;

        //*******************************************************
        // varchar move: move string to char array               
        //*******************************************************

          PVMV : begin

        //*********************************************
        // patblank = blank pattern for mfioperation   
        //*********************************************

                   with STK [ TOP - 1 ] do
                     begin

        //*********************************************
        // set target to blanks                        
        //*********************************************

                       PATBLANK := DATNULL ;
                       PATBLANK . FPA . DSPLMT := ORD ( ' ' ) ;
                       MFIOPERATION ( STK [ TOP - 2 ] , PATBLANK , Q )
                                      ;

        //*********************************************
        // fetch actual length (halfword)              
        // compare with maximum possible length        
        //*********************************************

                       FINDRG ;
                       RGWORK := NXTRG ;
                       FPA . DSPLMT := FPA . DSPLMT + 2 ;
                       GETADR2 ( STK [ TOP - 1 ] , Q1 , P1 , B1 ) ;
                       FINDRG ;
                       GENRX ( XLH , RGWORK , Q1 , B1 , P1 ) ;
                       if Q < 4096 then
                         GENRX ( XLA , NXTRG , Q , 0 , 0 )
                       else
                         GENRXLIT ( XLH , NXTRG , Q , - 1 ) ;
                       GENRR ( XCR , RGWORK , NXTRG ) ;
                       AVAIL [ NXTRG ] := TRUE ;
                       AVAIL [ RGWORK ] := TRUE ;

        //*********************************************
        // do variable length move - use mcpoperation  
        // length value comes from register rgwork     
        //*********************************************

                       DATLEN := DATNULL ;
                       DATLEN . VRBL := TRUE ;
                       DATLEN . VPA := RGS ;
                       DATLEN . RGADR := RGWORK ;
                       FPA . DSPLMT := FPA . DSPLMT + 2 ;
                       MCPOPERATION ( STK [ TOP - 2 ] , STK [ TOP - 1 ]
                                      , DATLEN ) ;
                     end (* with *) ;
                   TOP := TOP - 2
                 end (* tag/ca *) ;

        //*******************************************************
        // varchar index: retrieve single string char via index  
        //*******************************************************

          PVIX : begin
                   with STK [ TOP - 2 ] do
                     begin

        //*********************************************
        // load maxlength field                        
        // later: decide where string addr is          
        //*********************************************

                       GETADR2 ( STK [ TOP - 2 ] , Q1 , P1 , B1 ) ;
                       FINDRG ;
                       GENRX ( XLH , NXTRG , Q1 , B1 , P1 ) ;
                       GENRR ( XLTR , NXTRG , NXTRG ) ;
                       AVAIL [ NXTRG ] := TRUE ;

        //*********************************************
        // load length field                           
        // later: to check for index inside bounds     
        //*********************************************

                       FPA . DSPLMT := FPA . DSPLMT + 2 ;
                       GETADR2 ( STK [ TOP - 2 ] , Q1 , P1 , B1 ) ;
                       GENRX ( XLH , NXTRG , Q1 , B1 , P1 ) ;

        //*********************************************
        // load string address minus one               
        // (virtual origin)                            
        // load index value from Top - 1               
        // add index value to virtual origin           
        //*********************************************

                       FINDRG ;
                       FPA . DSPLMT := FPA . DSPLMT + 1 ;
                       GETADR2 ( STK [ TOP - 2 ] , Q1 , P1 , B1 ) ;
                       GENRX ( XLA , NXTRG , Q1 , B1 , P1 ) ;
                       RGWORK := NXTRG ;
                       LOAD ( STK [ TOP - 1 ] ) ;
                       GENRR ( XAR , NXTRG , RGWORK ) ;
                       AVAIL [ NXTRG ] := TRUE ;
                       AVAIL [ RGWORK ] := TRUE ;
                     end (* with *) ;

        //*********************************************
        // set top stack element (= string)            
        // to register address                         
        //*********************************************

                   TOP := TOP - 1 ;
                   with STK [ TOP - 1 ] do
                     begin
                       FPA := ZEROBL ;
                       DRCT := TRUE ;
                       VRBL := TRUE ;
                       VPA := RGS ;
                       RGADR := NXTRG ;
                     end (* with *) ;
                 end (* tag/ca *) ;

        //*******************************************************
        // varchar concat: concatenate varchars in workarea      
        //*******************************************************

          PVCC : WORK_VCC ;

        //*******************************************************
        // varchar set maxlength: sets maxlength on varchar      
        // used on potentially uninitialized varchars, when      
        // passed as var parameters (so that the procedure       
        // can determine their maximum length)                   
        //*******************************************************

          PVSM : begin
                   GENRXLIT ( XLH , 14 , Q , - 1 ) ;
                   FINDRG ;
                   GETADR2 ( STK [ TOP - 1 ] , Q1 , P1 , B1 ) ;
                   GENLA_LR ( NXTRG , Q1 , B1 , P1 ) ;
                   GENRX ( XSTH , 14 , 0 , NXTRG , 0 ) ;
                   AVAIL [ NXTRG ] := TRUE ;
                   with STK [ TOP - 1 ] do
                     begin
                       DTYPE := VARC ;
                       PLEN := Q ;
                       VRBL := TRUE ;
                       DRCT := TRUE ;
                       VPA := RGS ;
                       RGADR := NXTRG ;
                       FPA . LVL := 0 ;
                       FPA . DSPLMT := 0 ;
                       MEMADR . LVL := 0 ;
                       MEMADR . DSPLMT := 0 ;
                     end (* with *)
                 end (* tag/ca *) ;

        //*******************************************************
        // varchar load maxlength: loads maxlength in certain    
        // situations, for example when a string expression      
        // has been built and the maxlength of this expression   
        // is requested (which is equal to the length            
        // in this case)                                         
        //*******************************************************

          PVLM : begin
                   GETADR2 ( STK [ TOP - 1 ] , Q1 , P1 , B1 ) ;
                   GENLA_LR ( 14 , Q1 , B1 , P1 ) ;
                   FREEREG ( STK [ TOP - 1 ] ) ;
                   GENRX ( XLH , 14 , 0 , 14 , 0 ) ;
                   with STK [ TOP - 1 ] do
                     begin
                       DTYPE := INT ;
                       PLEN := 4 ;
                       VRBL := TRUE ;
                       DRCT := TRUE ;
                       VPA := RGS ;
                       RGADR := 14 ;
                       FPA . LVL := 0 ;
                       FPA . DSPLMT := 0 ;
                       MEMADR . LVL := 0 ;
                       MEMADR . DSPLMT := 0 ;
                     end (* with *)
                 end (* tag/ca *) ;

        //*******************************************************
        // varchar repeat: repeat string is implemented as       
        // P-Code, because this is needed to build new           
        // Strings on the stack (string of n blanks, for         
        // example) - at least with the P-Code interpreters      
        //*******************************************************

          PVRP : begin
                   
                 end (* tag/ca *) ;
        end (* case *)
      end (* STRINGOPS *) ;


   begin (* ASMNXTINST *)
     if OLDPCODE = PUJP then
       if not CASE_FLAG then

     (************************************)
     (* IGNORE INACCESSIBLE INSTRUCTIONS *)
     (************************************)

         if not ( PCODE in [ PXLB , PEND , PCST , PLAB , PLOC , PDEF ,
         PRET , PSTP , PENT , PCTS ] ) then
           return ;

     (********************************)
     (* XLATE COND CODE TO BOOL. VAL *)
     (********************************)

     if BRCND >= 0 then
       if not ( PCODE in [ PFJP , PNOT , PLOC ] ) then
         with STK [ TOP - 1 ] do
           begin

     (****************************)
     (* JUST NEGATE TOP OF STACK *)
     (****************************)

             if NEG_CND then
               begin
                 LOAD ( STK [ TOP - 1 ] ) ;
                 if PCODE = PAND then
                   GENRR ( XBCTR , RGADR , 0 )
                 else
                   GENRXLIT ( XX , RGADR , 1 , 0 ) ;
               end (* then *)

     (*************************************)
     (* OTHERWISE TRANSLATE CC TO BOOLEAN *)
     (*************************************)

             else
               begin
                 FINDRG ;
                 GENRX ( XLA , NXTRG , 1 , 0 , 0 ) ;

     (*************)
     (*ASSUME TRUE*)
     (*************)

                 GENRELRX ( XBC , BRCND , 3 ) ;

     (****************)
     (* BC BRCND,*+3 *)
     (****************)

                 GENRR ( XSR , NXTRG , NXTRG ) ;

     (*********************************)
     (* THEN CHANGE TO FALSE IF NEEDED*)
     (*********************************)

                 LAST_CC . LPC := 0 ;

     (****************************)
     (* THIS C.C. HAS NO MEANING *)
     (****************************)

                 DTYPE := BOOL ;
                 VRBL := TRUE ;
                 DRCT := TRUE ;
                 VPA := RGS ;
                 RGADR := NXTRG ;
                 FPA := ZEROBL ;
               end (* else *) ;
             BRCND := - 1 ;
             NEG_CND := FALSE ;

     (*******************************)
     (* RESET C.C. FLAG TO INACTIVE *)
     (*******************************)

           end (* with *) ;
     if not CASE_FLAG then
       if ( NXTLIT >= LITDANGER ) or ( LX . NXTDBL >= DBLDANGER ) then
         begin

     (******************************)
     (* EMPTY THE LITERAL POOL NOW *)
     (******************************)

           GENRX ( XBC , ANYCND , 0 , 0 , 0 ) ;
           I := PCOUNTER - 1 ;
           DUMP_LITERALS ;
           CODE . H [ I ] := TO_HINT ( BASE_DSPLMT ( PCOUNTER ) ) ;
         end (* then *) ;

     /********************************/
     /* verarbeitung abh. vom opcode */
     /********************************/

     if FALSE then
       begin
         WRITELN ( TRACEF , 'stack vor pcode = ' , PTBL [ PCODE ] ) ;
         DUMPSTK ( 1 , TOP - 1 )
       end (* then *) ;
     case PCODE of
       PLOD : with STK [ TOP ] do
                begin
                  if OPNDTYPE in [ ADR , INT , PSET ] then
                    begin
                      if ( Q MOD INTSIZE ) <> 0 then
                        ERROR ( 611 ) ;
                    end (* then *)
                  else
                    if OPNDTYPE = REEL then
                      begin
                        if ( Q MOD REALSIZE ) <> 0 then
                          ERROR ( 612 ) ;
                      end (* then *)
                    else
                      if OPNDTYPE = HINT then
                        begin
                          if ODD ( Q ) then
                            ERROR ( 610 )
                        end (* then *) ;
                  DTYPE := OPNDTYPE ;
                  VRBL := TRUE ;
                  DRCT := TRUE ;
                  FPA := ZEROBL ;
                  VPA := MEM ;
                  MEMADR . LVL := P ;
                  MEMADR . DSPLMT := Q ;
                  with LAST_STR do
                    if LPC = PCOUNTER then

     (********************************)
     (* TRY TO OPTIMIZE STR/LOD PAIR *)
     (********************************)

                      if MEMADR = STOPND then
                        if OPNDTYPE = STDT then

     (******************************)
     (* IN CASE OF VARIANT RECORDS *)
     (******************************)

                          begin
                            VPA := RGS ;
                            RGADR := STRGX ;
                            if OPNDTYPE <> REEL then
                              begin
                                if not AVAIL [ RGADR ] then
                                  begin
                                    FINDRG ;
                                    GENRR ( XLR , NXTRG , RGADR ) ;
                                    RGADR := NXTRG ;
                                  end (* then *) ;
                                AVAIL [ RGADR ] := FALSE ;
                              end (* then *)
                            else
                              begin
                                if not AVAILFP [ RGADR ] then
                                  begin
                                    FINDFP ;
                                    GENRR ( XLDR , NXTRG , RGADR ) ;
                                    RGADR := NXTRG ;
                                  end (* then *) ;
                                AVAILFP [ RGADR ] := FALSE ;
                              end (* else *) ;
                          end (* then *) ;
                  PROCNAME := ' ' ;
                  TOP := TOP + 1 ;
                end (* with *) ;
       PSTR : begin
                TOP := TOP - 1 ;
                if OPNDTYPE in [ ADR , INT , PSET ] then
                  begin
                    if ( Q MOD INTSIZE ) <> 0 then
                      ERROR ( 611 ) ;
                  end (* then *)
                else
                  if OPNDTYPE = REEL then
                    begin
                      if ( Q MOD REALSIZE ) <> 0 then
                        ERROR ( 612 ) ;
                    end (* then *)
                  else
                    if OPNDTYPE = HINT then
                      begin
                        if ODD ( Q ) then
                          ERROR ( 610 ) ;
                      end (* then *) ;
                with LAST_STR do

     (***********************************)
     (* SAVE INFO ABOUT STORED VARIABLE *)
     (***********************************)

                  begin
                    STOPND . LVL := P ;
                    STOPND . DSPLMT := Q ;
                    STDT := OPNDTYPE ;
                    STORE ( TOP , FALSE ) ;
                    if OPNDTYPE <= CHRC then
                      LPC := 0
                    else
                      LPC := PCOUNTER ;
                    STRGX := STK [ TOP ] . RGADR ;
                  end (* with *) ;
              end (* tag/ca *) ;
       PSTO : begin
                STORE ( TOP - 1 , TRUE ) ;

     (**********)
     (*INDIRECT*)
     (**********)

                TOP := TOP - 2
              end (* tag/ca *) ;
       PLDA : with STK [ TOP ] do
                begin
                  STK [ TOP ] := DATNULL ;
                  DTYPE := ADR ;
                  VRBL := FALSE ;
                  DRCT := TRUE ;
                  FPA . LVL := P ;
                  FPA . DSPLMT := Q ;
                  PROCNAME := ' ' ;
                  TOP := TOP + 1 ;
                  if FALSE then
                    begin
                      WRITELN ( TRACEF , 'DUMPSTK nach LDA' ) ;
                      DUMPSTK ( TOP - 1 , TOP - 1 )
                    end (* then *) ;
                end (* with *) ;
       PLDC : with STK [ TOP ] do
                begin
                  STK [ TOP ] := DATNULL ;
                  DTYPE := OPNDTYPE ;
                  VRBL := FALSE ;
                  FPA := ZEROBL ;
                  DRCT := TRUE ;
                  VPA := NEITHER ;
                  case OPNDTYPE of
                    ADR : FPA . DSPLMT := - 1 ;  //  LDC NIL
                    BOOL , CHRC , HINT , INT :
                      FPA . DSPLMT := IVAL ;
                    REEL : RCNST := RVAL ;
                    PSET : ERROR ( 616 ) ;
                  end (* case *) ;
                  PROCNAME := ' ' ;
                  TOP := TOP + 1 ;
                end (* with *) ;
       PIND : with STK [ TOP - 1 ] do
                begin
                  if DTYPE <> ADR then
                    ERROR ( 602 ) ;
                  if VRBL then
                    begin
                      if not DRCT then
                        LOAD ( STK [ TOP - 1 ] ) ;
                      FPA . DSPLMT := FPA . DSPLMT + Q ;
                      DRCT := FALSE ;
                    end (* then *)
                  else
                    begin
                      MEMADR := FPA ;
                      MEMADR . DSPLMT := MEMADR . DSPLMT + Q ;
                      FPA := ZEROBL ;
                      VRBL := TRUE ;
                      VPA := MEM ;
                      DRCT := TRUE ;
                    end (* else *) ;
                  DTYPE := OPNDTYPE ;
                end (* with *) ;
       PLCA : with STK [ TOP ] do
                begin
                  STK [ TOP ] := DATNULL ;
                  DTYPE := ADR ;
                  PROCNAME := ' ' ;
                  case OPNDTYPE of

     //************************************************************
     // load address of set constant                               
     //************************************************************

                    PSET : begin
                             VRBL := FALSE ;
                             DRCT := TRUE ;
                             VPA := NEITHER ;
                             PLEN := PSLNGTH ;
                             if PLEN > 0 then
                               begin
                                 NEW ( PCNST ) ;
                                 PCNST -> := PSVAL
                               end (* then *)
                             else
                               PCNST := NIL ;
                             DTYPE := PSET ;
                           end (* tag/ca *) ;

     //************************************************************
     // load address of procedure                                  
     //************************************************************

                    PROC : begin
                             VRBL := TRUE ;
                             DRCT := TRUE ;
                             VPA := RGS ;
                             FINDRG ;
                             RGADR := NXTRG ;
                             PROCNAME := LBL2 . NAM ;
                             if FALSE then
                               begin
                                 WRITELN ( 'PLCA mit Procname = ' ,
                                           PROCNAME ) ;
                                 WRITELN ( 'und gleich LA dazu' )
                               end (* then *) ;
                             GENRXLAB ( XL , RGADR , LBL2 , - 3 ) ;
                           end (* tag/ca *) ;

     //************************************************************
     // load address of constant string                            
     //************************************************************

                    CARR : begin

     //*****************************************************
     // look if literal is already in pool                  
     // a literal qualifies, if                             
     // 1) it has type C                                    
     // 2) it has a length >= the length of the new one     
     // 3) it starts or ends with the same characters       
     //    as the new one                                   
     //*****************************************************

                             LITOK := 0 ;
                             for I := 1 to NXTLIT do
                               with LITTBL [ I ] do
                                 if LTYPE = 'C' then
                                   if LENGTH >= SLNGTH then
                                     begin
                                       if MEMCMPX ( ADDR ( SVAL ) ,
                                       ADDR ( IDP_POOL . C [ XIDP ] ) ,
                                       SLNGTH ) = 0 then
                                         begin
                                           XOFFS := 0 ;
                                           LITOK := I ;
                                           break
                                         end (* then *) ;
                                       if LENGTH > SLNGTH then
                                         begin
                                           XOFFS := LENGTH - SLNGTH ;
                                           if MEMCMPX ( ADDR ( SVAL ) ,
                                           ADDR ( IDP_POOL . C [ XIDP +
                                           XOFFS ] ) , SLNGTH ) = 0
                                           then
                                             begin
                                               LITOK := I ;
                                               break
                                             end (* then *) ;
                                         end (* then *)
                                     end (* then *) ;

     //*****************************************************
     // if so, reuse; if not, add                           
     // reuse means: add entry in littbl, but don't add     
     // literal to literal pool (reuse literal there)       
     //*****************************************************

                             if LITOK > 0 then
                               begin
                                 TAG := 'use' ;
                                 NXTLIT := NXTLIT + 1 ;
                                 LITTBL [ NXTLIT ] . LNK := - TOP - 1 ;
                                 LITTBL [ NXTLIT ] . LTYPE := 'C' ;
                                 LITTBL [ NXTLIT ] . LENGTH := SLNGTH ;
                                 LITTBL [ NXTLIT ] . XIDP := LITTBL [
                                                   LITOK ] . XIDP +
                                                   XOFFS ;
                                 LITOK := NXTLIT ;
                               end (* then *)
                             else
                               begin

     //*****************************************************
     // if new literal is stored                            
     // and nxtch lower than gaps                           
     // invalidate gaps                                     
     //*****************************************************

                                 if LX . NXTCH <= LX . HW_GAP * 2 then
                                   begin
                                     LX . HW_GAP := - 1 ;
                                     LX . RHCONF := - 1 ;
                                     LX . IHCONF := - 1
                                   end (* then *) ;
                                 if LX . NXTCH <= LX . INT_GAP * 4 then
                                   begin
                                     LX . INT_GAP := - 1 ;
                                     LX . RICONF := - 1
                                   end (* then *) ;
                                 TAG := 'add' ;
                                 NXTLIT := NXTLIT + 1 ;
                                 LITOK := NXTLIT ;
                                 LITTBL [ LITOK ] . LNK := - TOP - 1 ;
                                 LITTBL [ LITOK ] . LTYPE := 'C' ;
                                 LITTBL [ LITOK ] . LENGTH := SLNGTH ;
                                 LITTBL [ LITOK ] . XIDP := LX . NXTCH
                                                   ;
                                 MEMCPY ( ADDR ( IDP_POOL . C [ LX .
                                          NXTCH ] ) , ADDR ( SVAL [ 1 ]
                                          ) , SLNGTH ) ;

     //*****************************************************
     // increment lx.nxtch                                  
     // and adjust lx.nxtdbl                                
     // and set new gaps, if possible                       
     //*****************************************************

                                 LX . NXTCH := LX . NXTCH + SLNGTH ;
                                 LX . NXTDBL := LX . NXTCH DIV 8 ;
                                 I := LX . NXTDBL * 8 - LX . NXTCH ;
                                 if I < 0 then
                                   begin
                                     LX . NXTDBL := LX . NXTDBL + 1 ;
                                     I := I + 8 ;
                                   end (* then *) ;
                                 NXTINT := LX . NXTDBL * 2 ;
                                 if I >= 4 then
                                   begin
                                     I := I - 4 ;
                                     if LX . INT_GAP < 0 then
                                       begin
                                         LX . INT_GAP := NXTINT - 1 ;
                                         NXTINT := LX . INT_GAP ;
                                         LX . RICONF := LX . NXTDBL - 1
                                                   ;
                                       end (* then *) ;
                                   end (* then *) ;
                                 if I >= 2 then
                                   if LX . HW_GAP < 0 then
                                     begin
                                       LX . HW_GAP := 2 * NXTINT - 1 ;
                                       LX . RHCONF := LX . NXTDBL - 1 ;
                                       LX . IHCONF := NXTINT - 1 ;
                                     end (* then *) ;
                               end (* else *) ;

     //*****************************************************
     // show entry info in literal pool                     
     //*****************************************************

                             if FALSE then
                               begin
                                 WRITELN ( TRACEF , TAG ,
                                           ' literal nr. ' , LITOK : 1
                                           , ' lnk = ' , LITTBL [ LITOK
                                           ] . LNK ) ;
                                 WRITELN ( TRACEF , TAG ,
                                           ' literal nr. ' , LITOK : 1
                                           , ' dsplmt = ' , LITTBL [
                                           LITOK ] . XIDP ) ;
                                 WRITELN ( TRACEF , TAG ,
                                           ' literal nr. ' , LITOK : 1
                                           , ' length = ' , LITTBL [
                                           LITOK ] . LENGTH ) ;
                                 WRITELN ( TRACEF , TAG ,
                                           ' literal nr. ' , LITOK : 1
                                           , ' sval   = ' , SVAL :
                                           SLNGTH ) ;
                               end (* then *) ;

     //*****************************************************
     // REF. TO EXP. STACK                                  
     //*****************************************************

                             SCNSTNO := LITOK ;
                             FPA . LVL := - 1 ;
                             FPA . DSPLMT := LITTBL [ LITOK ] . XIDP ;
                             VRBL := FALSE ;
                             DRCT := TRUE ;
                             if TRUE then
                               begin
                                 WRITELN ( TRACEF ,
                                           'DUMPSTK nach LCA x' ) ;
                                 DUMPSTK ( TOP , TOP )
                               end (* then *) ;
                           end (* tag/ca *) ;
                    otherwise
                      ERROR ( 601 )
                  end (* case *) ;
                  TOP := TOP + 1 ;
                end (* with *) ;
       PIXA : begin
                TOP := TOP - 1 ;
                with STK [ TOP ] do
                  begin
                    if not DRCT then
                      LOAD ( STK [ TOP ] ) ;
                    if not ( DTYPE in [ ADR , HINT , INT , BOOL , CHRC
                    ] ) then
                      ERROR ( 601 ) ;
                    FPA . DSPLMT := FPA . DSPLMT * Q ;
                    if VRBL then
                      begin
                        if VPA = MEM then
                          begin
                            FINDRG ;
                            P1 := MEMADR . LVL ;
                            Q1 := MEMADR . DSPLMT ;
                            BASE ( Q1 , P1 , B1 ) ;
                            if DTYPE in [ CHRC , BOOL ] then
                              begin
                                GENRR ( XSR , NXTRG , NXTRG ) ;
                                GENRX ( XIC , NXTRG , Q1 , B1 , P1 ) ;
                              end (* then *)
                            else
                              if DTYPE = HINT then
                                GENRX ( XLH , NXTRG , Q1 , B1 , P1 )
                              else

     (*********)
     (*INT,ADR*)
     (*********)

                                GENRX ( XL , NXTRG , Q1 , B1 , P1 ) ;
                            VPA := RGS ;
                            RGADR := NXTRG ;
                          end (* then *) ;

     (***********************)
     (* VPA IS IN A REG. NOW*)
     (***********************)

                        if Q > HALFINT then
                          ERROR ( 504 ) ;

     (*****************************)
     (* TOO LARGE FOR A HALF WORD *)
     (*****************************)

                        Q2 := POWER2 ( Q ) ;
                        if Q2 = 1 then
                          GENRR ( XAR , RGADR , RGADR )
                        else
                          if Q2 > 0 then
                            GENRS ( XSLA , RGADR , 0 , Q2 , 0 )
                          else
                            if Q2 < 0 then
                              GENRXLIT ( XMH , RGADR , Q , - 2 ) ;

     (********)
     (*=H'Q' *)
     (********)

                      end (* then *) ;

     (*************************************)
     (* NOW ADD THE TOP TO THE SECOND TOP *)
     (*************************************)

                    with STK [ TOP - 1 ] do
                      begin
                        if not VRBL then
                          if FPA . LVL < 0 then

     (*****************************************)
     (*I.E. INDEXING THROUGH A CONSTANT STRING*)
     (*****************************************)

                            LOAD ( STK [ TOP - 1 ] ) ;
                        if not DRCT then
                          LOAD ( STK [ TOP - 1 ] ) ;
                        if FALSE then
                          if PROCNAME <> '        ' then
                            begin
                              LOAD ( STK [ TOP - 1 ] ) ;
                            end (* then *)
                      end (* with *) ;
                    STK [ TOP - 1 ] . FPA . DSPLMT := STK [ TOP - 1 ] .
                                                   FPA . DSPLMT + FPA .
                                                   DSPLMT ;
                    FPA . DSPLMT := 0 ;
                    if VRBL and STK [ TOP - 1 ] . VRBL then
                      if VPA = RGS then
                        if STK [ TOP - 1 ] . VPA = RGS then

     (********************************************)
     (* BOTH OPERANDWS IN REGS                   *)
     (* free reg with higher number - opp / 2016 *)
     (********************************************)
     (* klappt nicht ...                         *)
     (********************************************)

                          begin
                            GENRR ( XAR , STK [ TOP - 1 ] . RGADR ,
                                    RGADR ) ;
                            AVAIL [ RGADR ] := TRUE ;
                            if FALSE then
                              begin
                                RGADR1 := STK [ TOP - 1 ] . RGADR ;
                                RGADR2 := RGADR ;
                                if RGADR1 < RGADR2 then
                                  begin
                                    GENRR ( XAR , RGADR1 , RGADR2 ) ;
                                    AVAIL [ RGADR2 ] := TRUE
                                  end (* then *)
                                else
                                  begin
                                    GENRR ( XAR , RGADR2 , RGADR1 ) ;
                                    AVAIL [ RGADR1 ] := TRUE
                                  end (* else *)
                              end (* then *)
                          end (* then *)
                        else

     (**********************************)
     (*TOP IN REG., 2_ND TOP IN MEMORY.*)
     (**********************************)

                          begin
                            Q1 := STK [ TOP - 1 ] . MEMADR . DSPLMT ;
                            P1 := STK [ TOP - 1 ] . MEMADR . LVL ;
                            BASE ( Q1 , P1 , B1 ) ;
                            GENRX ( XA , RGADR , Q1 , B1 , P1 ) ;
                            STK [ TOP - 1 ] . VPA := RGS ;
                            STK [ TOP - 1 ] . RGADR := RGADR ;
                          end (* else *)
                      else

     (***********)
     (*VPA = MEM*)
     (***********)

                        begin
                          if STK [ TOP - 1 ] . VPA <> RGS then
                            LOAD ( STK [ TOP - 1 ] ) ;
                          Q1 := MEMADR . DSPLMT ;
                          P1 := MEMADR . LVL ;
                          BASE ( Q1 , P1 , B1 ) ;
                          GENRX ( XA , STK [ TOP - 1 ] . RGADR , Q1 ,
                                  B1 , P1 ) ;
                        end (* else *)
                    else

     (*********************************)
     (*NOT (VRBL AND STK[TOP-1].VRBL) *)
     (*********************************)

                      if VRBL then
                        begin
                          FPA . LVL := STK [ TOP - 1 ] . FPA . LVL ;
                          FPA . DSPLMT := STK [ TOP - 1 ] . FPA .
                                          DSPLMT ;
                          DTYPE := ADR ;
                          STK [ TOP - 1 ] := STK [ TOP ] ;
                        end (* then *)
                  end (* with *) ;
              end (* tag/ca *) ;
       PPAK : begin
                TOP := TOP - 2 ;
                PACK_UNPACK ( STK [ TOP ] , STK [ TOP + 1 ] ) ;
              end (* tag/ca *) ;
       PMOV : begin
                TOP := TOP - 2 ;
                if Q > 0 then
                  begin  // FORWARD MOVE
                    SOPERATION ( STK [ TOP ] , STK [ TOP + 1 ] , PCODE
                                 , Q )
                  end (* then *)
                else
                  begin  // BACKWARD MOVE
                    Q := ABS ( Q ) ;
                    SOPERATION ( STK [ TOP + 1 ] , STK [ TOP ] , PCODE
                                 , Q ) ;
                  end (* else *) ;
              end (* tag/ca *) ;
       PDBG : ;
       PMFI : begin
                if Q > 0 then
                  begin
                    TOP := TOP - 2 ;
                    MFIOPERATION ( STK [ TOP ] , STK [ TOP + 1 ] , Q )
                  end (* then *)
                else
                  begin
                    Q := ABS ( Q ) ;
                    TOP := TOP - 1 ;
                    MFIOPERATION ( STK [ TOP - 2 ] , STK [ TOP ] , Q )
                  end (* else *) ;
              end (* tag/ca *) ;
       PMZE : begin
                TOP := TOP - 1 ;
                MZEOPERATION ( STK [ TOP ] , Q ) ;
              end (* tag/ca *) ;
       PMCP : begin
                TOP := TOP - 3 ;
                MCPOPERATION ( STK [ TOP ] , STK [ TOP + 1 ] , STK [
                               TOP + 2 ] ) ;
              end (* tag/ca *) ;
       PMSE : begin
                TOP := TOP - 3 ;
                MSEOPERATION ( STK [ TOP ] , STK [ TOP + 1 ] , STK [
                               TOP + 2 ] , Q ) ;
              end (* tag/ca *) ;

     (*****************************)
     (* CONTROL/BRANCH OPERATIONS *)
     (*****************************)

       PUJP , PFJP , PXJP , PPOP , PCUP , PENT , PLOC , PXLB , PUXJ ,
       PMST , PRET , PCSP , PSTP , PLAB , PDEF , PDFC , PCST , PEND :
         COPERATION ;
       PCHK : CHKOPERATION ;

     (********************)
     (* UNARY OPERATIONS *)
     (********************)

       PABI , PABR , PNGI , PNGR , PINC , PDEC , PNOT , PODD , PCHR ,
       PORD , PFLO , PFLT , PNEW , PSAV , PRST , PSQI , PSQR , PCTS ,
       PCTI , PXPO :
         UOPERATION ;

     (*********************)
     (* BINARY OPERATIONS *)
     (*********************)

       PEQU , PNEQ , PLES , PLEQ , PGRT , PGEQ , PADI , PSBI , PMPI ,
       PDVI , PMOD , PAND , PIOR , PADR , PSBR , PMPR , PDVR , PADA ,
       PSBA , PXOR :
         begin
           TOP := TOP - 1 ;
           BOPERATION
         end (* tag/ca *) ;

     (******************)
     (* SET OPERATIONS *)
     (******************)

       PINN , PINT , PUNI , PDIF , PASE :
         begin
           TOP := TOP - 1 ;
           BSETOPS
         end (* tag/ca *) ;
       PSCL , PCRD , PSMV , PSLD :
         CSETOPS ;

     //************************************************************
     // vstring instructions                                       
     //************************************************************

       PVPU , PVPO :
         STRINGOPS ;
       PVLD , PVST :
         STRINGOPS ;
       PVC1 , PVCC , PVLM , PVIX , PVRP :
         STRINGOPS ;
       PVC2 , PVMV , PVSM :
         STRINGOPS ;
     end (* case *) ;
     if FALSE then
       begin
         WRITELN ( TRACEF , 'stack nach pcode = ' , PTBL [ PCODE ] ) ;
         DUMPSTK ( 1 , TOP - 1 ) ;
         WRITELN ( TRACEF , '--------------------------------------' ,
                   '--------------------------------------' ) ;
       end (* then *) ;
     OLDPCODE := PCODE ;
   end (* ASMNXTINST *) ;



procedure SETUP ;

(*********************************************)
(* INITIALIZE GLOBAL VARIABLE/SET FLAGS ETC. *)
(*********************************************)


   var I : INTEGER ;

   begin (* SETUP *)
     GS . IN_PROCBODY := FALSE ;
     GS . FILL_LINEPTR := FALSE ;
     GS . MOD1DEFSTEP := - 1 ;
     GS . MOD2DEFSTEP := - 1 ;
     EMPTY := '   ' ;
     BRMSK [ PEQU ] := 8 ;
     BRMSK [ PNEQ ] := 7 ;
     BRMSK [ PGEQ ] := 11 ;
     BRMSK [ PGRT ] := 2 ;
     BRMSK [ PLEQ ] := 13 ;
     BRMSK [ PLES ] := 4 ;
     INVBRM [ PEQU ] := PEQU ;
     INVBRM [ PNEQ ] := PNEQ ;
     INVBRM [ PGEQ ] := PLEQ ;
     INVBRM [ PGRT ] := PLES ;
     INVBRM [ PLEQ ] := PGEQ ;
     INVBRM [ PLES ] := PGRT ;
     for I := 0 to HTSIZE do
       HTBL [ I ] . NAME := EMPTY ;
     OP_SP := TRUE ;
     for PCODE := PCTS to PRED ( UNDEF_OP ) do
       begin
         NMCDE := PTBL [ PCODE ] ;
         ENTERLOOKUP
       end (* for *) ;
     OP_SP := FALSE ;
     for CSP := PCTR to PRED ( UNDEF_CSP ) do
       begin
         NMCDE := CSPTBL [ CSP ] ;
         ENTERLOOKUP
       end (* for *) ;
     OP_SP := TRUE ;

     (******************************)
     (*TO PREPARE FOR OPCODE LOOKUP*)
     (******************************)

     for NXTRG := 0 to RGCNT do
       AVAIL [ NXTRG ] := TRUE ;
     for NXTRG := 0 to FPCNT do
       AVAILFP [ NXTRG ] := TRUE ;
     for CH := 'A' to 'Z' do
       TYPCDE [ CH ] := NON ;
     TYPCDE [ 'A' ] := ADR ;
     TYPCDE [ 'B' ] := BOOL ;
     TYPCDE [ 'C' ] := CHRC ;
     TYPCDE [ 'I' ] := INT ;
     TYPCDE [ 'H' ] := HINT ;
     TYPCDE [ 'M' ] := CARR ;
     TYPCDE [ 'S' ] := PSET ;
     TYPCDE [ 'P' ] := PROC ;
     TYPCDE [ 'R' ] := REEL ;
     TYPCDE [ 'N' ] := ADR ;
     TYPCDE [ 'J' ] := INX ;
     TYPCDE [ 'V' ] := VARC ;

     //************************************************************
     // TYPCDE [ 'F' ] := FORT ;                                   
     // TYPCDE [ 'X' ] := FBOOL ;                                  
     // TYPCDE [ 'Y' ] := FINT ;                                   
     // TYPCDE [ 'Z' ] := FREAL ;                                  
     //************************************************************

     TOP := 1 ;
     CURLVL := 1 ;
     BRCND := - 1 ;
     NEG_CND := FALSE ;
     TRACE := FALSE ;
     OLDPCODE := PBGN ;
     OLDCSP := PSIO ;
     MDTAG := PBGN ;
     TXRG := TRG14 ;
     MUSIC := FALSE ;
     ZEROBL . LVL := 0 ;
     ZEROBL . DSPLMT := 0 ;
     ERRORCNT := 0 ;
     S370CNT := 0 ;
     LCAFTSAREA := LCAFTMST ;
     SAVEFPRS := TRUE ;
     CLEAR_REG := TRUE ;
     TOTALBYTES := 0 ;
     CASE_FLAG := FALSE ;
     CASE_FLAG_NEW := FALSE ;
     FILECNT := 0 ;
     CKMODE := FALSE ;
     ASM := FALSE ;
     CST_ASMVERB := FALSE ;
     DEBUG := TRUE ;
     FLOW_TRACE := FALSE ;
     NXTLIT := 0 ;
     LX . NXTDBL := 0 ;
     LAST_CC . LPC := 0 ;
     TXR_CONTENTS . VALID := FALSE ;
     LAST_MVC . LPC := 0 ;
     LAST_STR . LPC := 0 ;
     LAST_STR . STOPND := ZEROBL ;
     OPT_FLG := TRUE ;
     HEXCHARS := '0123456789ABCDEF' ;
     TESTCNT := 0 ;
     PDEF_CNT := 0 ;
     CSPREGACTIVE := FALSE ;
     PROCOFFSET_OLD := 0 ;
     PCOUNTER := 0 ;
   end (* SETUP *) ;



begin (* HAUPTPROGRAMM *)
  RESET ( INPUT ) ;
  FIRST_ASMOUT := TRUE ;
  INIT := TRUE ;
  SETUP ;
  INIT := FALSE ;

  (************)
  (*INITIALIZE*)
  (************)

  if OSPARM <> NIL then
    with OSPARM -> do
      if LENGTH >= 2 then
        for Q := 1 to LENGTH - 1 do
          if ( STRING [ Q ] = 'T' ) and ( STRING [ Q + 1 ] = 'R' ) then
            TRACE := TRUE
          else
            if ( STRING [ Q ] = 'C' ) and ( STRING [ Q + 1 ] = 'K' )
            then
              CKMODE := TRUE
            else
              if ( STRING [ Q ] = 'M' ) and ( STRING [ Q + 1 ] = 'U' )
              then
                MUSIC := TRUE ;
  TIMER := CLOCK ( 0 ) ;
  WRITELN ( OUTPUT , '****' : 7 ,
            ' STANFORD PASCAL POST-PROCESSOR, OPPOLZER VERSION OF ' ,
            VERSION ) ;
  if not MUSIC then
    WRITELN ( OUTPUT ) ;

  //******************************************************************
  // read input first time to gather procedure information            
  //******************************************************************

  PIANKER := NIL ;
  repeat
    READNXTINST ( 1 ) ;
  until PCODE = PSTP ;
  if FALSE then
    begin
      PIAKT := PIANKER ;
      while PIAKT <> NIL do
        begin
          WRITELN ( TRACEF , 'information in procedure info chain' ) ;
          WRITELN ( TRACEF , '-----------------------------------' ) ;
          WRITELN ( TRACEF , 'CURPNAME...: ' , PIAKT -> . CURPNAME ) ;
          WRITELN ( TRACEF , 'CURPNO.....: ' , PIAKT -> . CURPNO ) ;
          WRITELN ( TRACEF , 'OPNDTYPE...: ' , PIAKT -> . OPNDTYPE ) ;
          WRITELN ( TRACEF , 'SEGSZE.....: ' , PIAKT -> . SEGSZE . NAM
                    ) ;
          WRITELN ( TRACEF , 'SAVERGS....: ' , PIAKT -> . SAVERGS ) ;
          WRITELN ( TRACEF , 'ASM........: ' , PIAKT -> . ASM ) ;
          WRITELN ( TRACEF , 'ASMVERB....: ' , PIAKT -> . ASMVERB ) ;
          WRITELN ( TRACEF , 'GET_STAT...: ' , PIAKT -> . GET_STAT ) ;
          WRITELN ( TRACEF , 'DEBUG_LEV..: ' , PIAKT -> . DEBUG_LEV ) ;
          WRITELN ( TRACEF , 'STATNAME...: ' , PIAKT -> . STATNAME ) ;
          WRITELN ( TRACEF , 'SOURCENAME.: ' , PIAKT -> . SOURCENAME )
                    ;
          WRITELN ( TRACEF , 'FLOW_TRACE.: ' , PIAKT -> . FLOW_TRACE )
                    ;
          WRITELN ( TRACEF , 'CALL_HIGHER: ' , PIAKT -> . CALL_HIGHER )
                    ;
          WRITELN ( TRACEF , 'LARGE_PROC.: ' , PIAKT -> . LARGE_PROC )
                    ;
          WRITELN ( TRACEF , 'code_size..: ' , PIAKT -> . CODE_SIZE ) ;
          WRITELN ( TRACEF , 'DATA_SIZE..: ' , PIAKT -> . DATA_SIZE ) ;
          PIAKT := PIAKT -> . NEXT
        end (* while *)
    end (* then *) ;
  PIAKT := NIL ;

  //******************************************************************
  // read input second time to process p-codes                        
  // curpno must be set to minus 1 again,                             
  // otherwise the first LOC instruction will go wild ...             
  // mark (heapmark) must be delayed after the first read loop :-)    
  //******************************************************************

  MARK ( HEAPMARK ) ;
  RESET ( INPUT ) ;
  repeat
    READNXTINST ( 2 ) ;
    ASMNXTINST ;
    if TRACE then
      DUMPSTK ( 1 , TOP - 1 ) ;
  until PCODE = PSTP ;

  //******************************************************************
  // check timer                                                      
  //******************************************************************

  TIMER := CLOCK ( 0 ) - TIMER ;
  WRITE ( OUTPUT , '****' : 7 ) ;
  if ERRORCNT > 0 then
    WRITE ( OUTPUT , ERRORCNT : 8 )
  else
    WRITE ( OUTPUT , 'NO' : 8 ) ;
  WRITELN ( OUTPUT , ' ASSEMBLY ERROR(S) DETECTED.' ) ;
  WRITELN ( OUTPUT , '****' : 7 , TOTALBYTES : 8 ,
            ' BYTES OF CODE GENERATED,' , TIMER * 0.001 : 6 : 2 ,
            ' SECONDS IN POST_PROCESSING.' ) ;
  if S370CNT > 0 then
    WRITELN ( OUTPUT , '****' : 7 , S370CNT : 8 ,
              ' "370"-ONLY INSTRUCTION(S) ISSUED.' ) ;
  EXIT ( ERRORCNT ) ;
end (* HAUPTPROGRAMM *) .
