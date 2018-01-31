program PASCALCOMPILER ( INPUT , OUTPUT , PRR , LISTING , DBGINFO ,
                         TRACEF ) ;

(********************************************************************)
(*$D+,N+,A-                                                         *)
(********************************************************************)
(*                                                                  *)
(*         S T A N F O R D   P A S C A L   C O M P I L E R          *)
(*                                                                  *)
(*              OPPOLZER VERSION                                    *)
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
(*                                                                  *)
(*         AUTHOR OF This VERSION (Oppolzer Version):               *)
(*                                                                  *)
(*              Bernd Oppolzer                                      *)
(*              Diplom-Informatiker                                 *)
(*              Baerenhofstr. 23                                    *)
(*              D-70771 Leinfelden-Echterdingen                     *)
(*              Germany                                             *)
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
(*    truncated, in the info text at the start of $PASMAIN          *)
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
(*==================================================================*)
(*                                                                  *)
(*  Aug.2016 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  to support the runtime extensions (PASCSP) regarding            *)
(*  textfile I/O, the implicit RESET on INPUT on the beginning      *)
(*  of the MAIN function has been removed; RESET is now done        *)
(*  when the first READ operation is encountered. That means        *)
(*  that the char variable INPUT -> is undefined until the          *)
(*  end of the first READ operation                                 *)
(*                                                                  *)
(*==================================================================*)
(*                                                                  *)
(*  Sep.2016 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  a) shorter strings allowed on const initializers                *)
(*     (on structured - typed - constants)                          *)
(*                                                                  *)
(*  b) shorter string constants on assignments                      *)
(*                                                                  *)
(*  c) even strings of length 1 (single chars) and zero ('')        *)
(*                                                                  *)
(*  d) prepared for new typeclass (charstring) -                    *)
(*     but not yet used or implemented; could make some             *)
(*     things simpler                                               *)
(*                                                                  *)
(*  e) using this: IDLNGTH changed from 12 to 20,                   *)
(*     so that the first 20 characters are significant on           *)
(*     identifiers (not only 12). The initializers needed           *)
(*     not be changed; although they are still 12 bytes long,       *)
(*     the fields are filled with blanks up to length 20            *)
(*                                                                  *)
(*  f) correct output to P-Code file, where necessary;              *)
(*     format changes observed on CST, ENT and BGN                  *)
(*                                                                  *)
(*  g) no changes so far to caching routines etc.                   *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Sep.2016 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  New functions added to support the construction of a new        *)
(*  storage management library using Pascal:                        *)
(*                                                                  *)
(*  - ADDR to get the address of any variable;                      *)
(*    the result of this function is a pointer without type         *)
(*    (aka void pointer) similar to the NIL pointer,                *)
(*    which is compatible with every other pointer type             *)
(*                                                                  *)
(*  - PTRADD to add an integer expression to a pointer              *)
(*    (of any type) - this adds addresses in contrast to C,         *)
(*    where element sizes are added; PTRADD without a               *)
(*    second argument (which is the same as PTRADD (X,0))           *)
(*    can be used to convert a typed pointer to a void pointer      *)
(*                                                                  *)
(*  - PTRDIFF, that subtracts two pointers (of any type),           *)
(*    giving an integer result                                      *)
(*                                                                  *)
(*  - SIZEOF, which works much the same as the C function           *)
(*    of the same name; as with C, you can specify a variable       *)
(*    as argument or a type identifier                              *)
(*                                                                  *)
(*  - PTR2INT, which converts a pointer to an integer value         *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Sep.2016 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  New keyword MODULE; a module is a collection of                 *)
(*  external procedures without a main program. The keyword         *)
(*  MODULE replaces the keyword PROGRAM on modules.                 *)
(*  It sets the compiler switch X implicitly and enforces that      *)
(*  the main block (which still has to be coded) is empty,          *)
(*  that is: "begin end.". No main block is generated, so           *)
(*  it is now possible to add multiply modules to a Pascal          *)
(*  program without name conflicts.                                 *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Sep.2016 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  Standard functions and procedures may not only be               *)
(*  implemented by inline code or CSP calls; another                *)
(*  possible (new) technique is to call an external function        *)
(*  in a library module.                                            *)
(*                                                                  *)
(*  To support this, several atributes have been added to the       *)
(*  internal Standard procedure description:                        *)
(*                                                                  *)
(*     STANDARD :                                                   *)
(*       ( KEY : INTEGER ;                                          *)
(*         LIBNAME : EXTNAMTP ;                                     *)
(*         FUNCCODE : INTEGER ;                                     *)
(*         PARMCNT : INTEGER ;                                      *)
(*         PROCTYP : CHAR ) ;                                       *)
(*                                                                  *)
(*   KEY is the only attribute that was present before and          *)
(*   is the number of the CSP call.                                 *)
(*                                                                  *)
(*   If LIBNAME is not blank, the Standard Proc is implemented      *)
(*   by a library function call. It gets the FUNCCODE as first      *)
(*   parameter; PARMCNT and PROCTYP are other attributes that       *)
(*   are needed to set up the CUP call for the library function     *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Sep.2016 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  Four new standard functions have been added using               *)
(*  the library function facility:                                  *)
(*                                                                  *)
(*  - ALLOC, which gets a length and returns a pointer              *)
(*    to a new area of that length                                  *)
(*                                                                  *)
(*  - ALLOCX, which does the same, but does not use the             *)
(*    (yet to come) sophisticated logic like LE, but does a pure    *)
(*    GETMAIN on every ALLOCX call, which is simple, but slow       *)
(*                                                                  *)
(*  - FREE, which frees the storage retrieved by ALLOC              *)
(*                                                                  *)
(*  - FREEX, which frees the storage retrieved by ALLOCX,           *)
(*    that is: FREEMAIN                                             *)
(*                                                                  *)
(*  The four new functions are implemented in the module            *)
(*  PASLIBX, seperate from the compiler (in Pascal)                 *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Oct.2016 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  New functions:                                                  *)
(*                                                                  *)
(*  - PTRCAST as a substitute for PTRADD with only one              *)
(*    argument; used to cast pointers of different types            *)
(*                                                                  *)
(*  - CHKHEAP to check the integrity of heap segments               *)
(*                                                                  *)
(*  - FILEFCB returns the pointer to the file control block         *)
(*    of the specified file (which contains the DCB at              *)
(*    position 36)                                                  *)
(*                                                                  *)
(*  - Pointers of any type can be written directly using            *)
(*    WRITE; the output format is 8 hex digits                      *)
(*                                                                  *)
(*  Other changes:                                                  *)
(*                                                                  *)
(*  The CSECTs of the internal procedures are now called            *)
(*  $PRVxxxx (xxxx is numbered starting from 0002).                 *)
(*                                                                  *)
(*  For external modules, the CSECT names of the internal           *)
(*  procedures are derived from the module name, for example:       *)
(*  module $PASLIBX --> CSECT names $LIBXxxx                        *)
(*                                                                  *)
(*  Only procs and funcs at level 1 are visible from outside        *)
(*  and keep their original names                                   *)
(*                                                                  *)
(*  If the new keyword LOCAL is specified in front of a             *)
(*  procedure or function declaration, the procedure is             *)
(*  local, even if it appears in an external module at level 1.     *)
(*                                                                  *)
(*  This all helps to reduce name conflicts and to allow            *)
(*  more than one external module (which was not possible           *)
(*  in original Stanford Pascal)                                    *)
(*                                                                  *)
(*  Changes to the environment:                                     *)
(*                                                                  *)
(*  in CMS the RUNPARM module was used to start Pascal programs     *)
(*  and to build an OS parm string out of the CMS tokenized         *)
(*  parameters, but: the CMS tokens were concatenated without       *)
(*  separating blanks. The new XRUNPARM module does the same,       *)
(*  but inserts blanks between the CMS tokens.                      *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Oct.2016 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  Static variables are supported; the new keyword static          *)
(*  starts a declaration section for static variables (like         *)
(*  the keyword var does it for "normal" auto variables)            *)
(*                                                                  *)
(*  This is done the same way in IBMs Pascal VS                     *)
(*                                                                  *)
(*  There is a separate STATIC CSECT for every Code CSECT           *)
(*  (every proc/func) which can hold up to 4k of static             *)
(*  variables or structured constants ... the same CSECT            *)
(*  is used for both. The CSECT is part of the load module,         *)
(*  that is: writing into this STATIC CSECT makes the program       *)
(*  non-reentrant.                                                  *)
(*                                                                  *)
(*  The SNAPSHOT routine (now called PASSNAP) has been              *)
(*  enhanced to be able to show static variables, too.              *)
(*  To support this, the address of the STATIC CSECT is             *)
(*  stored at a certain place in the function prolog, which         *)
(*  can easily be found at run time (displacement of the            *)
(*  branch instruction at the EPA minus 4). If the address          *)
(*  at this place is zero, there are no static variables.           *)
(*                                                                  *)
(*  PASSNAP was further enhanced to show the EP addresses           *)
(*  of every function, the call offset at every call level          *)
(*  and the storage class, offset and address of every              *)
(*  variable (in addition to the variable name and the              *)
(*  value at the time of error or SNAPSHOT - in Pascal              *)
(*  notation).                                                      *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Nov.2016 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  To enable the port to ASCII platforms, the following            *)
(*  changes have been made:                                         *)
(*                                                                  *)
(*  - the upper limit of for loops with loop control variables      *)
(*    of type char is encoded in char representation instead        *)
(*    of the internal numeric code in the P-Code source             *)
(*                                                                  *)
(*  - set constants of set of char have a new representation        *)
(*    in the P-Code, envolving char representation of the           *)
(*    chars contained in the set                                    *)
(*                                                                  *)
(*  - not related to the port: set constants in P-Code are          *)
(*    represented by hexa byte strings instead of integer           *)
(*    strings, which makes them much better readable                *)
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
(*  see boolean constant 'PORTABLE_BRANCHTABLE'                     *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Jan.2017 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  First version, which was extended / improved on Windows         *)
(*  first and later moved to Hercules/VM.                           *)
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
(********************************************************************)
(*                                                                  *)
(*  Jan.2017 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  Change SEARCHID, to prevent additional errors, when             *)
(*  encountering a procedure call for a procedure without           *)
(*  declaration; the proc name was entered into the id list         *)
(*  as a dummy var declaration, which leaded to more errors.        *)
(*  Now the insert into the id list is deferred until the next      *)
(*  symbol has been read. (SEARCHID has got much more               *)
(*  parameters to be able to do this).                              *)
(*                                                                  *)
(*  New error code for "undeclared proc" (changed from 104          *)
(*  "identifier not declared" to 184 "procedure not                 *)
(*  declared").                                                     *)
(*                                                                  *)
(*  Error 184 is changed to warning, this way it is possible        *)
(*  to generate code (which makes some sense) even for              *)
(*  undeclared procedures and functions - much the same way         *)
(*  as some other languages (including PL/1 and C and FORTRAN)      *)
(*  do it. The types of the parameters are                          *)
(*  taken from the types of the arguments, which will               *)
(*  hopefully fit to the external definition of the                 *)
(*  procedure. If the arguments are variables, the args             *)
(*  are passed by reference; if not, by value.                      *)
(*                                                                  *)
(*  New field CSTNAME for declared procs (records the name          *)
(*  of the STATIC CSECT assigned to the proc). This was needed,     *)
(*  because all STATIC CSECTs of the (empty) main programs of       *)
(*  external modules had the same name #PASMAI#, which led to       *)
(*  name conflicts. The CSTNAME now is determined when the          *)
(*  procedure is created, already.                                  *)
(*                                                                  *)
(*  MODULE PASCALVS implements some of the functions that           *)
(*  are known from the PASCAL/VS compiler, e.g. DATETIME,           *)
(*  DATTIM10 (with century), TERMIN and TERMOUT, HALT               *)
(*                                                                  *)
(*  CLOSE standard function added as new CSP P-Code operation;      *)
(*  it turned out that it was not sufficient to simply set          *)
(*  the file status to zero, but instead the runtime on             *)
(*  the mainframe has to flush the buffers etc., so a true CSP      *)
(*  is the better solution. Now it is possible to close a file      *)
(*  before program termination without reopening it again.          *)
(*  Up until now this was only possible by issuing RESET or         *)
(*  REWRITE, leaving the file open until program termination.       *)
(*                                                                  *)
(*  DATE and TIME had to be added as new CSP P-Code operations,     *)
(*  too, because DATE and TIME values up until now only were        *)
(*  retrieved from the system once at initialization time.          *)
(*  Now it is done at every reference to DATE or TIME, by calling   *)
(*  the new CSP operations.                                         *)
(*                                                                  *)
(*  Floating Point output operations have been fixed;               *)
(*  WRITE (X : N) prints exponential format (n.nnnE+xx)             *)
(*  WRITE (X : N : P) prints decimal format, but if P = 0,          *)
(*  only the integer part of X is printed on all platforms          *)
(*                                                                  *)
(*  New standard function FLOOR (gets real parm, yields             *)
(*  real result), which returns the next lower "integer"            *)
(*  (but real type). This is NOT the same as TRUNC.                 *)
(*  FLOOR exists in C, too.                                         *)
(*                                                                  *)
(*  New standard function ROUNDX (round extended), which            *)
(*  rounds real values at a given positon (for example,             *)
(*  at the second decimal position) - as accurate as possible       *)
(*                                                                  *)
(*  Undefined functions don't give errors 104 and follow-up         *)
(*  errors, but a warning 186 instead (undefined function);         *)
(*  the function is assumed to have result type integer, and        *)
(*  the parameters are handled in the same way as with              *)
(*  undefined procedures, see above.                                *)
(*                                                                  *)
(*  Some errors on floating point rounding and output have been     *)
(*  fixed; now in most cases the same results are presented on      *)
(*  all platforms.                                                  *)
(*                                                                  *)
(*  The Pascal monitor PASMONN has been reworked, so that           *)
(*  the 8k limit is no problem any more (subroutines have their     *)
(*  own base register); Pascal subroutines (from PASLIBX) can       *)
(*  be called from CSP implementations written in ASSEMBLER;        *)
(*  these Pascal subroutines can in turn call other CSP             *)
(*  subroutines (and so on ...)                                     *)
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
(*  Several changes and corrections to PASMONN (Pascal runtime)     *)
(*  have been made. The most important: PASMONN now supports        *)
(*  RESET and REWRITE of PO members with their name specified       *)
(*  at runtime using the new function ASSIGNMEM. After RESET,       *)
(*  success can be checked by looking at a flag in the Pascal       *)
(*  FCB, accessed by the (existing) function FILEFCB.               *)
(*                                                                  *)
(*  This has been necessary to provide a MVS variant of PASSNAP;    *)
(*  PASSNAP reads debug information at runtime, which depends       *)
(*  on the name of the source file. In CMS, this was accomplished   *)
(*  using CMS FILEDEFs, issued from the Pascal program.             *)
(*  In MVS, ASSIGNMEM is used. The version of PASSNAP for MVS       *)
(*  is located in the source file PASSNAPM. The technique to        *)
(*  open the debug information file is the only difference          *)
(*  between PASSNAP (CMS) and PASSNAPM (MVS).                       *)
(*                                                                  *)
(*  There is still room for some improvement in the area of         *)
(*  error handling etc.; some ideas:                                *)
(*                                                                  *)
(*  - register and PSW output in both PASSNAP and the simple        *)
(*    error output written by PASMONN                               *)
(*                                                                  *)
(*  - correct handling of 1006 - stack/heap collision               *)
(*                                                                  *)
(*  - control PASSNAP output by runtime option                      *)
(*                                                                  *)
(*  - control ABEND with SYSUDUMP after PASSNAP by runtime option   *)
(*                                                                  *)
(*  - use CAMLST to determine the type of the assigned dataset      *)
(*    (PS or PO) to prevent error 013-14 (which is unrecoverable)   *)
(*                                                                  *)
(*  - read directory or BLDL instead of reacting on 013-18          *)
(*    using the DCB ABEND EXIT (which is recoverable)               *)
(*                                                                  *)
(*  - if no member specified and dataset assigned is a PDS,         *)
(*    change RECFM to U on the fly and read directory               *)
(*                                                                  *)
(*  Thanks to Gerhard Postpischil and Juergen Winckelmann           *)
(*  for help and good advice and for encouraging me to do           *)
(*  the MVS port.                                                   *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  May.2017 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  - Minor error: allow CLOSE for non-Text-Files, too.             *)
(*                                                                  *)
(*  - New standard type ANYFILE, compatible to every other          *)
(*    file type; file variables can be arguments to var             *)
(*    parameters of type ANYFILE, and so functions and              *)
(*    procedures can be written that accept files of any            *)
(*    type as parameters                                            *)
(*                                                                  *)
(*  - standard type VOIDPTR renamed to ANYPTR; VOIDPTR is           *)
(*    allowed, too (for compatibility reasons)                      *)
(*                                                                  *)
(*  - X -> is invalid (flagged by the compiler) for variables       *)
(*    of type ANYPTR or ANYFILE; two new error messages 187         *)
(*    and 188                                                       *)
(*                                                                  *)
(*  - new error message 182 for var parameters with                 *)
(*    different lengths (was 142); this error is supressed          *)
(*    when file arguments are passed to ANYFILE parameters          *)
(*    (ANYFILEs variables have no file buffers, so they             *)
(*    are shorter than other file variables)                        *)
(*                                                                  *)
(*  - yet to be done: a function that tells if an ANYFILE           *)
(*    variable is a TEXT or a binary file; a function that          *)
(*    returns the size and the address of the file buffer           *)
(*    of an ANYFILE variable. What should already work with         *)
(*    ANYFILEs: functions like ASSIGN, REWRITE, RESET, CLOSE,       *)
(*    GET, PUT, and FILEFCB, which returns the address of the       *)
(*    Pascal FCB of the file. Maybe: a function that casts          *)
(*    an ANYFILE variable to a variable of type TEXT (so that       *)
(*    functions requiring TEXT files can be used on ANYFILES).      *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Jun.2017 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  - Character constants may be coded in hexadecimal and           *)
(*    binary (X'nn', B'bbbbbbbb'); this is a large topic and        *)
(*    not yet finished completely                                   *)
(*                                                                  *)
(*  - The implementation of sets will be reworked completely;       *)
(*    larger sets will be allowed (up to 2000 elements), and        *)
(*    some restrictions regarding set limits will be dropped        *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Aug.2017 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  - String constants now may be constructed of different          *)
(*    parts, seperated by blanks, some of them coded in hex         *)
(*    or binary, and others in normal char representation           *)
(*                                                                  *)
(*    Example: const S_VS_1 = X'1b' '&l12D' X'0d0a' ;               *)
(*                                                                  *)
(*    I used this to rewrite a very old BASIC program in            *)
(*    Pascal which outputs printer control sequences to             *)
(*    a HP Laserjet printer (preserving software and hardware       *)
(*    investments)                                                  *)
(*                                                                  *)
(*  - SIZEOF supports simple string constants (will be              *)
(*    problematic for expressions involving structured              *)
(*    constants, let's see later ... maybe expressions              *)
(*    should be allowed as argument to SIZEOF, not only             *)
(*    type identifiers, const identifiers - now new - and           *)
(*    variables with optional selectors).                           *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Oct.2017 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  - The old source program scanner (procedure INSYMBOL)           *)
(*    has been completely replaced by a new scanner called          *)
(*    PASSCAN; the new scanner is not hand-written any more,        *)
(*    but it is generated using a scanner-generating tool that      *)
(*    was written at the Computer Science department of the         *)
(*    Stuttgart University in 1980 by four students (including      *)
(*    myself). I extended this scanner generator in 1996, to        *)
(*    make a usable product out of it, and I used it in many        *)
(*    projects from 1996 until today. PASSCAN is an external        *)
(*    module, seperate from the compiler. It does all the           *)
(*    source handling and it writes the compile listing.            *)
(*                                                                  *)
(*  - The new scanner will make extensions to the compiler          *)
(*    symbol repertoire much easier, because it is generated        *)
(*    from a "grammar", which is in fact a large regular            *)
(*    expression (with attributes). The scanner generator           *)
(*    works similar to the well-known Unix tool "lex".              *)
(*                                                                  *)
(*  - With the help of the new scanner, some more extensions        *)
(*    have been made - more will follow - for example:              *)
(*                                                                  *)
(*  - C++ style comments: // the rest of the line is a comment      *)
(*                                                                  *)
(*  - binary integer constants: 0b00010010                          *)
(*                                                                  *)
(*  - write integer with negative width produces leading zeroes     *)
(*                                                                  *)
(*  - some restrictions on character strings have been removed      *)
(*                                                                  *)
(*  - some errors in the set implementation have been corrected     *)
(*                                                                  *)
(*  - the compiler listing has been slightly reworked; it is        *)
(*    now produced by the new generated scanner procedure           *)
(*                                                                  *)
(*  - this allows for lines with errors to be shown at the          *)
(*    terminal, followed immediately by the error message;          *)
(*    this way, the compile listing is needed no more for           *)
(*    error diagnosis in most cases                                 *)
(*                                                                  *)
(*  - the compiler now runs with the debug switch on (that is:      *)
(*    subrange checks etc. are now enabled on the compiler);        *)
(*    when I first tried this, I discovered that the compiler       *)
(*    up to now stored out-of-range values into scalar types        *)
(*    etc., so that the debug switch had to be turned off ...       *)
(*    this has been fixed.                                          *)
(*                                                                  *)
(*  - new operators (shift, more assignments like +=) and           *)
(*    more operator levels now seem possible without much           *)
(*    effort regarding the scanner; the extension of the            *)
(*    scanner could be done within minutes.                         *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Dec.2017 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  - Shorter strings (variables) can now be assigned to longer     *)
(*    strings; the longer target strings are filled with blanks     *)
(*    (no message P129: TYPE CONFLICT OF OPERANDS)                  *)
(*                                                                  *)
(*  - New P-Code instruction to support that (MFI to fill           *)
(*    a memory area with a pattern - fixed size)                    *)
(*                                                                  *)
(*  - Some error messages now have additional information,          *)
(*    for example P168: UNDEFINED LABEL - which is shown at the     *)
(*    end of a procedure block - now shows the number of the        *)
(*    missing label. Or: P117: MISSING FORWARE REFERENCE for        *)
(*    types now shows the name of the missing type. This was        *)
(*    shown before, but by doing an additional WRITELN call         *)
(*    into the source listing file, which corrupted the clean       *)
(*    layout of the listing a little bit. Now the information       *)
(*    is part of the error message, and the place, where it is      *)
(*    to be inserted, can be configured in the PASCAL.MESSAGES      *)
(*    file.                                                         *)
(*                                                                  *)
(*  - New P-Code instructions MCP to support MEMCPY and             *)
(*    MSE to support MEMSET ... no more function call               *)
(*    involving Pascal loop with bytewise copy, should              *)
(*    speed up things (maybe generating MVCL on 370 and             *)
(*    memset on PCINT-based platforms)                              *)
(*                                                                  *)
(*  - New P-Code instruction MZE to fill an area of fixed           *)
(*    length with zeroes (will generate XC instead of               *)
(*    overlapping MVC on the mainframe)                             *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Dec.2017 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  - Extensions to EXTERNAL procedure declarations: the            *)
(*    language of the external procedure may be specified,          *)
(*    and an additional external name (8 chars), which is           *)
(*    used instead of the Pascal name (which may be longer).        *)
(*    Supported languages are FORTRAN and ASSEMBLER on the          *)
(*    mainframe and Pascal, of course. On the PC only               *)
(*    Pascal, at the moment, because the P-Code interpreter         *)
(*    cannot call external C objects at the moment.                 *)
(*                                                                  *)
(*    Example:                                                      *)
(*                                                                  *)
(*    procedure PASCAL_TO_ASSEMBLER ( X1 : INTEGER ;                *)
(*                                    var X2 : INTEGER ;            *)
(*                                    T1 : CHAR20 ;                 *)
(*                                    var T2 : CHAR20 ) ;           *)
(*                                                                  *)
(*       EXTERNAL ASSEMBLER 'PAS2ASM' ;                             *)
(*                                                                  *)
(*  - For ASSEMBLER and FORTRAN, different call sequences           *)
(*    are created. ASSEMBLER and FORTRAN both use normal            *)
(*    OS linkage conventions, and FORTRAN expects all parms         *)
(*    passed by reference, so the Pascal compiler creates           *)
(*    dummy arguments for every Pascal by-value parameter.          *)
(*                                                                  *)
(*  - External ASSEMBLER functions must return their result         *)
(*    in general register 0 (or FP register 0, for double           *)
(*    float results). This is also what FORTRAN does.               *)
(*                                                                  *)
(*  - Example programs for both languages, showing external         *)
(*    procedures and functions implemented in ASSEMBLER and         *)
(*    FORTRAN, have been created.                                   *)
(*                                                                  *)
(*  - More advanced topics: using the Pascal stack in the           *)
(*    external procedures (allowing, maybe, recursive calls         *)
(*    of the ASSEMBLER subfunctions), and calling some              *)
(*    functions of the Pascal runtime library.                      *)
(*    This is possible, too, but has not been tested yet.           *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Jan.2018 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  - More extensions to make Stanford Pascal a competitor          *)
(*    to Pascal/VS (or VS Pascal) and PL/1                          *)
(*                                                                  *)
(*  - New data types CHAR (n), VARCHAR (n) - aka STRING (n)         *)
(*    and DECIMAL (n, m)                                            *)
(*                                                                  *)
(*  - Only CHAR (n) is fully implemented at the moment,             *)
(*    n can be in the range 1 to 254. Shorter CHARs may             *)
(*    be assigned to longer CHARs (and passed as byvalue            *)
(*    parameters to functions). CHAR (n) is an abbreviation         *)
(*    for "array [1..n] of char" ... no need to define it.          *)
(*                                                                  *)
(*  - DECIMAL (n, m) is implemented internally as REAL              *)
(*    (at the moment)                                               *)
(*                                                                  *)
(*  - New functions DIGITSOF and PRECISIONOF, to be used on         *)
(*    DECIMAL data; WRITE with DECIMALs will by default             *)
(*    use these options:                                            *)
(*    WRITE (D);                                                    *)
(*    with D of type DECIMAL will be the same as                    *)
(*    WRITE (D: DIGITSOF(D) + 3 : PRECISIONOF(D));                  *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Jan.2018 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  - Implementation of STRING (n)                                  *)
(*                                                                  *)
(*  - Many new P-Code instructions to support the STRING (n)        *)
(*    datatype:                                                     *)
(*                                                                  *)
(*    VC1 - varchar convert 1                                       *)
(*    VC2 - varchar convert 2                                       *)
(*    VCC - varchar concat                                          *)
(*    VLD - varchar load                                            *)
(*    VLM - varchar load maxlength                                  *)
(*    VMV - varchar move                                            *)
(*    VPO - varchar pop workarea addr                               *)
(*    VPU - varchar push workarea addr                              *)
(*    VSM - varchar set maxlength                                   *)
(*    VST - varchar store                                           *)
(*                                                                  *)
(*  - These P-Codes are not yet supported by the P-Code to          *)
(*    370 translator PASCAL2, so the STRING (n) datatype            *)
(*    only works on Windows etc. (at the moment)                    *)
(*                                                                  *)
(*  - Find more details in a separate document on the               *)
(*    New Stanford Pascal compiler website                          *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Feb.2018 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  - const Parameters (like in Pascal/VS) to allow the             *)
(*    implementation of most string functions using Pascal          *)
(*                                                                  *)
(*  - array like indexes for strings (new P-Code VIX similar        *)
(*    to IXA to index characters in strings)                        *)
(*                                                                  *)
(*  - many string functions from Pascal/VS (and more) are           *)
(*    supported; some implemented directly inline using             *)
(*    new P-Code instructions, but most (for the moment)            *)
(*    in Pascal (see PASLIBX.PAS)                                   *)
(*                                                                  *)
(*    STR          - convert to string                              *)
(*    MAXLENGTH    - maxlength of string                            *)
(*    LENGTH       - length of string                               *)
(*    STRRESULT    - result of str func                             *)
(*    STRRESULTP   - ptr to str result                              *)
(*    REPEATSTR    - repeat str n times                             *)
(*    RESULTP      - ptr to result                                  *)
(*    SUBSTR       - substring (like in PL/1)                       *)
(*    DELETE       - delete part of string (args like substr)       *)
(*    RTRIM        - trim blanks on the right                       *)
(*    LTRIM        - trim blanks on the left                        *)
(*    TRIM         - trim blanks on both sides                      *)
(*    COMPRESS     - reduce multiple blanks to one blank            *)
(*    INDEX        - search string position (like in PL/1)          *)
(*    VERIFY       - verify string (like in PL/1)                   *)
(*    TRANSLATE    - translate using tranlation table (PL/1)        *)
(*                                                                  *)
(********************************************************************)



const VERSION = '2018.02' ;
      MAXLSIZE = 120 ;
      MAXERRNO = 999 ;

      (******************************************)
      (* MAX # OF LINES PER PAGE OF LISTING     *)
      (******************************************)

      MAXINT = 2147483647 ;
      MAXADDR = 16777215 ;

      (******************************************)
      (* LARGEST ELEMENT USED IN THIS CODE      *)
      (******************************************)

      BUFLEN = 122 ;

      (******************************************)
      (* MAX LINE LENGTH + 2                    *)
      (******************************************)

      INTSIZE = 4 ;
      HINTSIZE = 2 ;
      REALSIZE = 8 ;
      STRSTACKSZ = 8 ;
      MXDATASZE = 8 ;
      CHARSIZE = 1 ;
      BOOLSIZE = 1 ;
      WORDSIZE = 4 ;

      (*****************************************)
      (*  some more size constants             *)
      (*****************************************)

      PTRSIZE = 4 ;
      FILHDRSIZE = 8 ;
      FILMINSIZE = 12 ;
      REALLNGTH = 20 ;

      (******************************************)
      (* STRING REPRESENTATION OF REAL NUMBERS  *)
      (******************************************)

      DIGMAX = 19 ;

      (******************************************)
      (* REALLNGTH - 1                          *)
      (******************************************)

      IDLNGTH = 20 ;
      ALFALNGTH = 10 ;
      MAXSTRL = 254 ;
      MAXVARCHARSIZE = 32767 ;
      DISPLIMIT = 20 ;
      MAX_BKT = 58 ;

      (******************************************)
      (* HASH TABLE SIZE                        *)
      (******************************************)

      MAXLEVEL = 9 ;
      ORDCHMAX = 255 ;

      (******************************************)
      (* SIZE OF CHAR SET OF TARGET MACHINE     *)
      (******************************************)

      OPMAX = 97 ;

      (*****************)
      (* OPCODE RANGE  *)
      (*****************)

      MAXERRNR = 999 ;

      (******************************************)
      (* MAX VAL OF ERROR CODE                  *)
      (******************************************)

      MAXRW = 50 ;

      (******************************************)
      (* room for 50 reserved words             *)
      (******************************************)

      MAXRWLEN = 9 ;

      (*****************************************)
      (* longest reserved word has length = 9  *)
      (* controls size of table frw            *)
      (*****************************************)

      CTRMAX = 16384 ;
      EXTNAMSZ = 8 ;

      (***********************************************)
      (* EXTERNAL NAME LENGTH                        *)
      (* SAVE AREAS, FUNCTION RETURN VALUE SPACE,    *)
      (* DISPLAY AREA, ETC.                          *)
      (***********************************************)

      LCAFTMST = 80 ;
      FPSAVEAREA = 32 ;
      RUNCHKAREA = 96 ;
      DISPADR = 80 ;
      FNCRSLT = 72 ;
      DISPAREA = 40 ;
      FIRSTCONSTLC = 16 ;
      FIRSTFILBUF = 248 ;

      (******************************************)
      (* = LCAFTMST + RUNCHKAREA + DSPLYAREA    *)
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
      (* portable_branchtable: new style of B.T. *)
      (* cixmax: maximum difference of highest   *)
      (* and lowest case label                   *)
      (*******************************************)

      PORTABLE_BRANCHTABLE = TRUE ;
      CIXMAX = 400 ;

      (***************************************************)
      (* new set related constants                       *)
      (* maxsetl = maximum set length in bytes (netto)   *)
      (* setmaxsize = maximum set length in bits         *)
      (* setupplimit = upper limit for setmax            *)
      (* setlowlimit = lower limit for setmin            *)
      (*    (setupplimit - setlowlimit) div 8 has to be  *)
      (*    stored in 3 bytes internally ...             *)
      (***************************************************)

      MAXSETL = 252 ;
      SETMAXSIZE = 2000 ;
      SETUPPLIMIT = 64000000 ;
      SETLOWLIMIT = - 64000000 ;

      (***************************************************)
      (* numeric constants for pcode positions           *)
      (***************************************************)

      PCODE_ADI = 2 ;
      PCODE_ADR = 3 ;
      PCODE_AND = 4 ;
      PCODE_DIF = 5 ;
      PCODE_DVI = 6 ;
      PCODE_DVR = 7 ;
      PCODE_SBR = 8 ;
      PCODE_FLO = 9 ;
      PCODE_FLT = 10 ;
      PCODE_INN = 11 ;
      PCODE_INT = 12 ;
      PCODE_IOR = 13 ;
      PCODE_MPI = 15 ;
      PCODE_MPR = 16 ;
      PCODE_NGI = 17 ;
      PCODE_NGR = 18 ;
      PCODE_NOT = 19 ;
      PCODE_ODD = 20 ;
      PCODE_SBI = 21 ;
      PCODE_DEC = 22 ;
      PCODE_INC = 23 ;
      PCODE_STO = 26 ;
      PCODE_SCL = 29 ;
      PCODE_CSP = 30 ;
      PCODE_UNI = 31 ;
      PCODE_ENT = 32 ;
      PCODE_IND = 35 ;
      PCODE_IXA = 36 ;
      PCODE_LCA = 37 ;
      PCODE_CTI = 39 ;
      PCODE_MOV = 40 ;
      PCODE_MST = 41 ;
      PCODE_RET = 42 ;
      PCODE_STP = 43 ;
      PCODE_CHK = 45 ;
      PCODE_CUP = 46 ;
      PCODE_EQU = 47 ;
      PCODE_GEQ = 48 ;
      PCODE_GRT = 49 ;
      PCODE_LDA = 50 ;
      PCODE_LDC = 51 ;
      PCODE_LEQ = 52 ;
      PCODE_LES = 53 ;
      PCODE_LOD = 54 ;
      PCODE_NEQ = 55 ;
      PCODE_STR = 56 ;
      PCODE_UJP = 57 ;
      PCODE_NEW = 58 ;
      PCODE_SAV = 59 ;
      PCODE_RST = 60 ;
      PCODE_ORD = 61 ;
      PCODE_CRD = 65 ;
      PCODE_XPO = 66 ;
      PCODE_ASE = 67 ;
      PCODE_SLD = 68 ;
      PCODE_SMV = 69 ;
      PCODE_BGN = 72 ;
      PCODE_PAK = 76 ;
      PCODE_XOR = 79 ;
      PCODE_MFI = 80 ;
      PCODE_MCP = 81 ;
      PCODE_MSE = 82 ;
      PCODE_DBG = 83 ;
      PCODE_MZE = 84 ;
      PCODE_VC1 = 85 ;
      PCODE_VC2 = 86 ;
      PCODE_VCC = 87 ;
      PCODE_VLD = 88 ;
      PCODE_VST = 89 ;
      PCODE_VMV = 90 ;
      PCODE_VSM = 91 ;
      PCODE_VLM = 92 ;
      PCODE_VPU = 93 ;
      PCODE_VPO = 94 ;
      PCODE_VIX = 95 ;
      PCODE_VRP = 96 ;


type ALPHA = array [ 1 .. IDLNGTH ] of CHAR ;
     EXTNAMTP = array [ 1 .. EXTNAMSZ ] of CHAR ;
     SET_CHAR = set of CHAR ;

     /***********************************/
     /* muss mit Def. beim Scanner      */
     /* uebereinstimmen                 */
     /***********************************/

     SYMB = ( SYMB_EOF , SYMB_UNKNOWN , EOLCHAR , SEPARATOR , COMMENT1
            , COMMENT2 , COMMENT3 , COMMENT4 , COMMENT5 , STRINGCONST ,
            HEXSTRINGCONST , BINSTRINGCONST , INTCONST , INTDOTDOT ,
            REALCONST , IDENT , SYLPARENT , SYRPARENT , SYLBRACK ,
            SYRBRACK , SYCOMMA , SYSEMICOLON , SYARROW , SYPERIOD ,
            SYDOTDOT , SYCOLON , SYPLUS , SYMINUS , SYMULT , SYSLASH ,
            SYEQOP , SYNEOP , SYGTOP , SYLTOP , SYGEOP , SYLEOP ,
            SYOROP , SYANDOP , SYASSIGN , SYCONCAT , SYAND , SYDIV ,
            SYMOD , SYOR , SYXOR , SYIN , SYNOT , SYLABEL , SYCONST ,
            SYTYPE , SYVAR , SYFUNC , SYPROG , SYPROC , SYSET ,
            SYPACKED , SYARRAY , SYRECORD , SYFILE , SYFORWARD ,
            SYBEGIN , SYIF , SYCASE , SYREPEAT , SYWHILE , SYFOR ,
            SYWITH , SYGOTO , SYEND , SYELSE , SYUNTIL , SYOF , SYDO ,
            SYTO , SYDOWNTO , SYTHEN , SYFRTRN , SYEXTRN , SYOTHERWISE
            , SYOTHER , SYBREAK , SYCONTINUE , SYRETURN , SYMODULE ,
            SYLOCAL , SYSTATIC , NOTUSED ) ;
     SYMSET = set of SYMB ;

     (***********************************)
     (* some set related definitions    *)
     (***********************************)

     SETSTRING = array [ 1 .. MAXSETL ] of CHAR ;

     (**********************************************)
     (* forward definitions of some pointer types  *)
     (**********************************************)

     SSP = -> XSTRCON ;
     CONSTP = -> XCONSTANT ;
     TTP = -> TYPEREC ;
     IDP = -> IDENTIFIER ;

     (******************)
     (* CONSTANTS      *)
     (******************)

     CSTCLASS = ( XINT , REEL , PSET , STRG ) ;

     /********************************************************/
     /* neue darstellung fuer konstanten                     */
     /* typ valu entfaellt                                   */
     /* basis ist kurze einfache struktur                    */
     /* lange bereiche fuer sets und strings werden erst     */
     /* bei bedarf dazugeholt                                */
     /********************************************************/

     XSTRCON = record
                 LENGTH : INTEGER ;
                 case TAG : CHAR of
                   'S' :
                     ( SSTR : array [ 1 .. MAXSTRL ] of CHAR ) ;
                   'P' :
                     ( PSTR : SETSTRING )
               end ;
     XCONSTANT = record
                   STRTYPE : CHAR ;
                   case CSTCLASS of
                     XINT :
                       ( IVAL : INTEGER ) ;
                     REEL :
                       ( RVAL : array [ 1 .. REALLNGTH ] of CHAR ) ;
                     PSET :
                       ( SETMIN : INTEGER ;
                         SETMAX : INTEGER ;
                         SETOFFS : INTEGER ;
                         SETTYPE : TTP ;
                         PVAL : SSP ) ;
                     STRG :
                       ( SVAL : SSP )
                 end ;

     (********************)
     (* DATA STRUCTURES  *)
     (********************)

     LEVRANGE = 0 .. MAXLEVEL ;
     ADDRRANGE = 0 .. MAXADDR ;
     ALNRNG = 1 .. 8 ;
     LABELRNG = 0 .. 1000 ;
     BKT_RNG = 0 .. MAX_BKT ;
     OPRANGE = 0 .. OPMAX ;

     (*************************************************)
     (* variant structure typerec for type definition *)
     (*************************************************)
     (*************************************************)
     (* basic type classes                            *)
     (*************************************************)

     TYPECLASS = ( SCALAR , SUBRANGE , POINTER , POWER , CSTRING ,
                 ARRAYS , RECORDS , FILES , TAGFLD , VARIANT ) ;
     DECLKIND = ( STANDARD , DECLARED ) ;
     TYPEREC = record
                 SIZE : ADDRRANGE ;

     //************************************************************
     // ALIGNMENT FACTOR                                           
     //************************************************************

                 ALN : ALNRNG ;
                 ERRORFLAG : BOOLEAN ;
                 case FORM : TYPECLASS of
                   SCALAR :
                     ( case SCALKIND : DECLKIND of
                         STANDARD :
                           ( WHATSTANDARD : CHAR ;
                             STDPARM1 : ADDRRANGE ;
                             STDPARM2 : ADDRRANGE ;
                             MINPARAMCOUNT : 0 .. 2 ;
                             MAXPARAMCOUNT : 0 .. 2 ;
                             MINPARAM : ADDRRANGE ;
                             MAXPARAM : ADDRRANGE ;
                             DEFAULTPARAM : ADDRRANGE ) ;
                         DECLARED :
                           ( FCONST : IDP ;
                             LITOFFS : ADDRRANGE ;
                             CSTNAME : EXTNAMTP ) ) ;
                   SUBRANGE :
                     ( RANGETYPE : TTP ;
                       MIN , MAX : XCONSTANT ) ;
                   POINTER :
                     ( ELTYPE : TTP ) ;

     //************************************************************
     // new fields for set definition - 06.2017                    
     // elset   = base type of set (subrange,                      
     //           scalar, maybe char)                              
     // setmin  = minimum value for set                            
     // setmax  = maximum value for set                            
     // setoffs = where bit string starts                          
     //           (minimum value div 8)                            
     //************************************************************

                   POWER :
                     ( ELSET : TTP ;
                       SETMIN : INTEGER ;
                       SETMAX : INTEGER ;
                       SETOFFS : INTEGER ) ;

     //************************************************************
     // conformant strings =                                       
     // strings without length                                     
     // used as var parameters (for example)                       
     //************************************************************

                   CSTRING :
                     ( CONFORMANT : BOOLEAN ) ;
                   ARRAYS :
                     ( AELTYPE , INXTYPE : TTP ) ;
                   RECORDS :
                     ( FSTFLD : IDP ;
                       RECVAR : TTP ;
                       NO_FLDS : 0 .. 1000 ;
                       FLD_DISP_LEV : - 1 .. DISPLIMIT ) ;
                   FILES :
                     ( FILTYPE : TTP ) ;
                   TAGFLD :
                     ( TAGFIELDP : IDP ;
                       FSTVAR : TTP ) ;
                   VARIANT :
                     ( NXTVAR , SUBVAR : TTP ;
                       FSTSUBFLD : IDP ;
                       VARVAL : XCONSTANT )
               end ;

     (**************************************************)
     (* type to store set constant infos temporarily   *)
     (**************************************************)

     PSETINFO = -> SETINFO_TEMP ;
     SETINFO_TEMP = record
                      ELEMCOUNT : INTEGER ;
                      SETMIN : INTEGER ;
                      SETMAX : INTEGER ;
                      RANGEERR : INTEGER ;
                      SETELEMS : array [ 1 .. SETMAXSIZE ] of BOOLEAN ;
                      CHARTYPE : BOOLEAN ;
                      HEXORBIN : CHAR ;
                      CONST_IN_SET : INTEGER ;
                      VARS_IN_SET : INTEGER ;
                      MODUS : CHAR ;
                    end ;

     (******************************)
     (* identifier classes         *)
     (******************************)

     IDCLASS = ( TYPES , KONST , STRUCTKONST , VARS , FIELD , PROC ,
               FUNC ) ;
     SETOFIDS = set of IDCLASS ;

     (******************************)
     (* standard procedures        *)
     (******************************)

     CSPTYPE = ( PPAG , PGET , PPUT , PRES , PREW , PRDC , PWRI , PWRE
               , PWRR , PWRC , PWRS , PWRX , PRDB , PWRB , PRDR , PRDH
               , PRDY , PEOL , PEOT , PRDD , PWRD , PCLK , PWLN , PRLN
               , PRDI , PEOF , PELN , PRDS , PTRP , PXIT , PFDF , PSIO
               , PEIO , PMSG , PSKP , PLIM , PTRA , PWRP , PCLS , PDAT
               , PTIM , PFLR , PTRC , PRND , PWRV , UNDEF_CSP ) ;

     (******************************)
     (* types of parameters        *)
     (******************************)

     IDKIND = ( NORMALVAR , VARPARM , CONSTPARM ) ;

     (******************************)
     (* storage classes            *)
     (******************************)

     STORAGE_CLASS = ( XAUTO , XSTATIC ) ;

     (******************************************************)
     (* identifier entries                                 *)
     (*                                                    *)
     (* some comments on new fields (since 2011):          *)
     (*                                                    *)
     (* konst - values: contains the value (xconstant)     *)
     (* structkonst: skownerproc is the external name of   *)
     (*       the proc which implements the                *)
     (*       structured constant ... the name             *)
     (*       of the static csect is derived from          *)
     (*       this; skaddr is the displ. there             *)
     (* vars: same for vownerproc and vaddr, if stklass    *)
     (*       is static. special ind., if the variable     *)
     (*       needs special treatment (TIME, DATE, for     *)
     (*       example; call a CSP at every reference)      *)
     (* proc/func: made extrn, extlang, extname global     *)
     (*       for both flavors of proc / func              *)
     (* proc/func standard: libname etc. for library       *)
     (*       functions which are not simply csps.         *)
     (*       the compiler generates calls to libname,     *)
     (*       where the first parameter is funccode.       *)
     (*       proctyp is omitted; the type of the          *)
     (*       standard function is known via proctype      *)
     (*       as in the declared case; idtype is set       *)
     (*       accordingly during enterstdnames.            *)
     (* proc/func declared: declmissing is used for        *)
     (*       procs and funcs which have no decl           *)
     (*       (works anyway, W184 and W186).               *)
     (*       frtrn attribute has been omitted.            *)
     (*       we now have extrn and extname and extlang;   *)
     (*       extname can be a different name from the     *)
     (*       (longer) internal name and extlang can be    *)
     (*       blank for Pascal or A / F for ASSEMBLER      *)
     (*       resp. FORTRAN. A and F generate different    *)
     (*       calling sequences.                           *)
     (******************************************************)

     IDENTIFIER = record
                    NAME : ALPHA ;
                    IDTYPE : TTP ;
                    NEXT_IN_BKT , NEXT : IDP ;
                    DECL_LEV : LEVRANGE ;
                    case KLASS : IDCLASS of
                      KONST :
                        ( VALUES : XCONSTANT ) ;
                      STRUCTKONST :
                        ( SKOWNERPROC : EXTNAMTP ;
                          SKADDR : ADDRRANGE ) ;
                      VARS :
                        ( VKIND : IDKIND ;
                          VLEV : LEVRANGE ;
                          STKLASS : STORAGE_CLASS ;
                          VOWNERPROC : EXTNAMTP ;
                          VADDR : ADDRRANGE ;
                          SPECIAL : INTEGER ;
                          DUMMYVAR : BOOLEAN ;
                          DUMMYLEV : LEVRANGE ;
                          DUMMYADDR : ADDRRANGE ) ;
                      FIELD :
                        ( FLDADDR : ADDRRANGE ;
                          OWNER : TTP ) ;
                      PROC , FUNC :
                        ( EXTRN : BOOLEAN ;
                          EXTLANG : CHAR ;
                          EXTNAME : EXTNAMTP ;
                          case PFDECKIND : DECLKIND of
                            STANDARD :
                              ( KEY : INTEGER ;
                                LIBNAME : EXTNAMTP ;
                                FUNCCODE : INTEGER ;
                                PARMCNT : INTEGER ;
                                WASIZE : INTEGER ) ;
                            DECLARED :
                              ( FWDECL : BOOLEAN ;
                                PFLEV : INTEGER ;
                                PFNAME : LABELRNG ;
                                PRMPTR , NXTFWRD : IDP ;
                                PFKIND : IDKIND ;
                                DECLMISSING : BOOLEAN ;
                                CSTNAME : EXTNAMTP ) )
                  end ;
     DISPRANGE = 0 .. DISPLIMIT ;
     HASH_TABLE = array [ BKT_RNG ] of IDP ;
     WHERE = ( BLCK , CREC , VREC , REC ) ;

     (******************)
     (* EXPRESSIONS    *)
     (******************)

     ATTRKIND = ( CST , VARBL , EXPR ) ;
     VACCESS = ( DRCT , INDRCT , INXD , STKEXPR ) ;
     ATTR = record

     (*********************************)
     (* TYPE AS AN EXPR. ON RUN-STACK *)
     (*********************************)

              TYPTR : TTP ;

     (*********************************)
     (* TYPE AS A VARIABLE IN MEMORY  *)
     (*********************************)

              BTYPE : TTP ;
              case KIND : ATTRKIND of
                CST :
                  ( CVAL : XCONSTANT ) ;
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
     TESTPOINTER = record
                     ELT1 , ELT2 : TTP ;
                     LASTTESTP : TESTP
                   end ;

     (*************)
     (* LABELS    *)
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
                 FILIDPTR : IDP ;
                 NEXTFILE : FRECPTR ;
               end ;
     PRNTTYLISTP = -> PRNTTYLIST ;
     PRNTTYLIST = record
                    ELT : TTP ;
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

     (********************************************)
     (* zum Anschauen von TYPECLASS Inhalten     *)
     (********************************************)

     FORMTEST = record
                  case BOOLEAN of
                    FALSE :
                      ( X1 : 0 .. 32000 ) ;
                    TRUE :
                      ( X2 : TYPECLASS ) ;
                end ;

     /***********************************/
     /* zentraler Scan-Block            */
     /***********************************/
     /* muss mit Def. beim Scanner      */
     /* uebereinstimmen                 */
     /***********************************/

     CHAR32 = array [ 1 .. 32 ] of CHAR ;
     SOURCELINE = array [ 1 .. MAXLSIZE ] of CHAR ;
     SCAN_ERRCLASS = 'A' .. 'Z' ;
     OPTIONS_PTR = -> COMP_OPTIONS ;
     SCAN_BLOCK = record
                    MODUS : INTEGER ;        // modus of scanner
                    DATEIENDE : INTEGER ;    // end of file indicator
                    ENDOFLINE : BOOLEAN ;    // end of line indicator
                    SLINE : SOURCELINE ;     // stored source line
                    LINENR : INTEGER ;       // line number of symbol
                    LINEPOS : INTEGER ;      // line position of symb
                    LINELEN : INTEGER ;      // line length
                    LOOKAHEAD : CHAR ;       // lookahead character
                    SYMBOLNR : SYMB ;        // symbol read
                    SYMBOL : SOURCELINE ;    // characters of symb
                    LSYMBOL : INTEGER ;      // no of chars in symb
                    MAXLSYMBOL : INTEGER ;   //
                    UFZAHL : INTEGER ;       // no of undef errors
                    SFZAHL : INTEGER ;       // no of severe errors
                    FEZAHL : INTEGER ;       // no of errors
                    WAZAHL : INTEGER ;       // no of warnings
                    INZAHL : INTEGER ;       // no of informations
                    FEANFANG : ANYPTR ;      // anchor to err list
                    FEAKT : ANYPTR ;         // actual err elem
                    FTTAB : ANYPTR ;         // error text table
                    FTTABA : ANYPTR ;        // same for applic.
                    OPTLINE : SOURCELINE ;   // options line
                    POPT : OPTIONS_PTR ;     // ptr to opt struct

     /******************************************/
     /* felder fuer sofortige Protokollausgabe */
     /******************************************/

                    PROTOUT : BOOLEAN ;        // switch for prot out
                    TERMOUT : BOOLEAN ;        // switch for term out
                    FEAKT_ALT : ANYPTR ;       // old feakt
                    LINEINFO : CHAR32 ;        // line information
                    LINEINFO_SIZE : INTEGER ;  // size of lineinfo

     /******************************************/
     /* felder fuer ueberschrift               */
     /******************************************/

                    LINECOUNT : INTEGER ;      // linecount f. heading
                    HEADLINE : SOURCELINE ;    // header line
                    HEADLINE_SIZE : INTEGER ;  // size of header line
                    PAGENR : INTEGER ;         // page number
                  end ;

     /***********************************/
     /* Optionen fuer Compiler          */
     /***********************************/
     /* muss mit Def. beim Scanner      */
     /* uebereinstimmen                 */
     /***********************************/

     COMP_OPTIONS = record
                      LMARGIN : INTEGER ;    // left margin
                      RMARGIN : INTEGER ;    // right margin
                      PAGESIZE : INTEGER ;   // pagesize of listing
                      LIST : BOOLEAN ;       // write listing
                      PRCODE : BOOLEAN ;     // print code
                      GET_STAT : BOOLEAN ;   // get statistics
                      SAVEREGS : BOOLEAN ;   // saveregs
                      SAVEFPRS : BOOLEAN ;   // save fp regs
                      DEBUG : BOOLEAN ;      // debug switch
                      MWARN : BOOLEAN ;      //
                      DEBUG_LEV : 0 .. 9 ;   // debug level
                      NOPACKING : BOOLEAN ;  // no packing
                      NESTCOMM : BOOLEAN ;   // nested comments
                      WARNING : BOOLEAN ;    // show warnings
                      ASSEMBLE : BOOLEAN ;   // show assembly
                      ASMVERB : BOOLEAN ;    // show verbose ass.
                      CTROPTION : BOOLEAN ;  // show counters
                    end ;

     /*****************************************************/
     /* to control the allocation                         */
     /* of items on the string workarea                   */
     /*****************************************************/
     /* watch1 stays on during compilation of a           */
     /* statement or condition and controls the           */
     /* generation of VPU/VPO pairs for this statements   */
     /*****************************************************/
     /* watch2 stays on during compilation of a           */
     /* function body involving a string function         */
     /* result and controls generation of one VPU at      */
     /* the beginning of the function and one or more     */
     /* VPOs every time when there is an assignment to    */
     /* the function result                               */
     /*****************************************************/

     CTL_STRINGAREA = record
                        WATCH1 : BOOLEAN ;
                        WATCH2 : BOOLEAN ;
                        VPU1_LEVEL : LEVRANGE ;
                        VPU1_OFFSET : ADDRRANGE ;
                        VPO1_NEEDED : BOOLEAN ;
                        VPU2_DONE : BOOLEAN ;
                        VPU2_LEVEL : LEVRANGE ;
                        VPU2_OFFSET : ADDRRANGE ;
                      end ;


var MXINT2 : INTEGER ;
    MXINT10 : INTEGER ;
    MXINT16 : INTEGER ;
    TRACEF : TEXT ;
    LISTING : TEXT ;
    DBGINFO : TEXT ;
    SOURCENAME : EXTNAMTP ;
    SX : INTEGER ;
    SID_RC : INTEGER ;
    PROGNAME : ALPHA ;
    NAME_PATTERN : ALPHA ;

    /*********************************************/
    /* compiler options in new opt structure     */
    /*********************************************/

    OPT : COMP_OPTIONS ;
    SCB : SCAN_BLOCK ;

    (*******************************************************)
    (* RETURNED BY SOURCE PROGRAM SCANNER    INSYMBOL:     *)
    (*                                       *********     *)
    (* SY       - symbol read                              *)
    (* SYLENGTH - length of symbol or constant             *)
    (* SYdigits - digits of symbol (if const number)       *)
    (* SYprec   - precision of symbol (if decimal const)   *)
    (* VAL      - constant (if symbol was constant)        *)
    (* ID       - identifier (if symbol was ident)         *)
    (*******************************************************)

    SY : SYMB ;
    SYLENGTH : INTEGER ;
    SYDIGITS : INTEGER ;
    SYPREC : INTEGER ;
    VAL : XCONSTANT ;
    ID : ALPHA ;

    (******************************************)
    (* COUNTERS:                              *)
    (* *********                              *)
    (******************************************)

    CHCNT : 0 .. BUFLEN ;

    (******************************************)
    (* CHARACTER COUNTER                      *)
    (******************************************)

    CONSTLCOUNTER , LCOUNTER : ADDRRANGE ;
    ICOUNTER , OLDICOUNTER : ADDRRANGE ;
    STRCOUNTER : ADDRRANGE ;

    (***********************************************************)
    (* CONSTLCOUNTER = DATA LOC. FOR STRUCTURED CONSTANTS      *)
    (* LCOUNTER      = DATA LOCATION                           *)
    (* ICOUNTER      = INSTRUCTION COUNTER                     *)
    (* OLDICOUNTER   = OLD INSTRUCTION COUNTER                 *)
    (* STRCOUNTER    = STRING INSTRUCTION COUNTER              *)
    (***********************************************************)

    LINECNT , OLDLN , PLCNT , ERRLN : INTEGER ;

    (******************************************)
    (* SWITCHES:                              *)
    (* *********                              *)
    (******************************************)

    HP : BOOLEAN ;

    (******************************************)
    (* HEADER PART                            *)
    (******************************************)

    ASSIGN , PACKDATA : BOOLEAN ;

    (******************************************)
    (* ASSIGNMENT GOING ON,PACKING IN EFFECT  *)
    (******************************************)

    FLIPDEBUG : BOOLEAN ;
    EXTUSED : BOOLEAN ;

    (******************************************)
    (* POSTPROCESSOR TRANSLATION, VERBOSE     *)
    (******************************************)

    IS_MODULE : BOOLEAN ;    // true, if source is a module

    (************************************)
    (* pointers to                      *)
    (* PREDEFINED FILES INPUT + OUTPUT  *)
    (************************************)

    INPUTPTR , OUTPUTPTR : IDP ;

    (************************************)
    (* pointers to                      *)
    (* PREDEFINED types                 *)
    (************************************)

    PTYPE_INT , PTYPE_REAL , PTYPE_CHAR , PTYPE_BOOL , PTYPE_ANY ,
    PTYPE_ANYFILE , PTYPE_TEXT , PTYPE_ALFA , PTYPE_VARCHAR ,
    PTYPE_DECIMAL : TTP ;

    (******************************************)
    (* pointers to                            *)
    (* ENTRIES OF STANDARD IDS                *)
    (******************************************)

    UTYPPTR , UCSTPTR , UVARPTR : IDP ;
    UFLDPTR , UPRCPTR , UFCTPTR : IDP ;

    (******************************************)
    (* POINTER TO $PASMAIN ENTRY              *)
    (******************************************)

    MAINPROG : IDP ;

    (******************************************)
    (* POINTER TO LIST OF FORTRAN PROC PARMS  *)
    (******************************************)

    FRTPARHD : IDP ;

    (******************************************)
    (* HEAD OF CHAIN OF FORW DECL TYPE IDS    *)
    (******************************************)

    FWPTR : IDP ;

    (******************************************)
    (* HEAD OF CHAIN OF EXTERNAL FILES        *)
    (******************************************)

    FILEHEAD : FRECPTR ;

    (******************************************)
    (* CURRENT RECORD OPENED BY "WITH"        *)
    (******************************************)

    OPEN_RECORD : TTP ;

    (******************************************)
    (* LAST TESTPOINTER                       *)
    (******************************************)

    GLOBTESTP : TESTP ;

    (******************************************)
    (* LIST OF HEAP STORAGE ITEMS FOR DEBUG   *)
    (******************************************)

    PRNTTYPHD : PRNTTYLISTP ;
    PRNTTYNO : 0 .. 999 ;

    (*************************************************)
    (* listtag = LISTING TAG, D / C / N / blank      *)
    (* level = bookkeeping OF DECLARATION LEVELS     *)
    (* dcn = level printed for compile listing       *)
    (* all three are used to print compiler info     *)
    (* on each source line in the compiler listing   *)
    (* old values to act on changes ...              *)
    (*************************************************)

    LISTTAG : CHAR ;
    LEVEL : LEVRANGE ;
    DCN : ADDRRANGE ;
    OLDLISTTAG : CHAR ;
    OLDLEVEL : LEVRANGE ;
    OLDDCN : ADDRRANGE ;
    OLDCONSTLCOUNTER , OLDLCOUNTER : ADDRRANGE ;
    OLDLINENR : INTEGER ;

    (******************************************)
    (* CURRENT STATIC LEVEL                   *)
    (******************************************)

    STMTNEST : 0 .. 100 ;

    (******************************************)
    (* CURRENT STATEMENT NESTING              *)
    (* LEVEL OF LAST ID SEARCHED BY SEARCHID  *)
    (******************************************)

    DISX , TOP : - 1 .. DISPLIMIT ;

    (*******************************************)
    (* TOP OF DISPLAY                          *)
    (* WHERE:   MEANS:                         *)
    (* =BLCK:   ID IS VARIABLE ID              *)
    (* =CREC:   ID IS FIELD ID IN RECORD WITH  *)
    (*          CONSTANT ADDRESS               *)
    (* =VREC:   ID IS FIELD ID IN RECORD WITH  *)
    (*          VARIABLE ADDRESS               *)
    (*******************************************)

    DISPLAY : array [ DISPRANGE ] of record
                                       case OCCUR : WHERE of
                                         BLCK :
                                           ( FLABEL : LBP ) ;
                                         CREC :
                                           ( CLEV : LEVRANGE ;
                                             CDSPL : ADDRRANGE ) ;
                                         VREC :
                                           ( VDSPL : ADDRRANGE )
                                     end ;

    (******************************************)
    (* --> PROCEDURE WITHSTATEMENT            *)
    (* RUN-TIME PROFILER COUNTERS             *)
    (* **************************             *)
    (******************************************)

    CTRCNT : CTRRANGE ;
    CTRCNTLBL : LABELRNG ;

    (******************************************)
    (* EXPRESSION COMPILATION:                *)
    (* ***********************                *)
    (******************************************)
    // gattr - very important - DESCRIBES THE EX
    //PR CURRENTLY COMPILED                     

    GATTR : ATTR ;

    //**************************************************************
    // ctls - watches the allocation of strings in the working area 
    //**************************************************************

    CTLS : CTL_STRINGAREA ;

    (******************************************)
    (* DESCRIBES THE EXPR CURRENTLY COMPILED  *)
    (******************************************)

    PSIGLOB : PSETINFO ;
    OPTLINE : SOURCELINE ;
    INTLABEL , PROCLAB , XLABNO : LABELRNG ;
    CALL_LVL : array [ BOOLEAN ] of INTEGER ;

    (**********************************************************)
    (*  UPSHIFT and SSY in an ideal world should cover        *)
    (*  the whole range of characters / B.Oppolzer - 2016     *)
    (**********************************************************)
    (*  must include all chars for port to Windows / Linux    *)
    (**********************************************************)
    (*  SSY completely removed when moving to new             *)
    (*  PASSCAN scanner, same goes for table SOP and          *)
    (*  type OPERSYMB ... all symbol handling is now done     *)
    (*  in PASSCAN by generated logic / B.Oppolzer - 2017     *)
    (**********************************************************)

    UPSHIFT : array [ CHAR ] of CHAR ;
    BUCKET : HASH_TABLE ;

    (******************************************)
    (* SYMBOL TABLE USAGE STATISTICS          *)
    (* ****** ***** ***** **********          *)
    (******************************************)

    FENT_CNT , SF_CNT , SF_TOT : INTEGER ;

    (******************************************)
    (* # FIELD ENTRIES, SEARCHES, PRODUCT     *)
    (******************************************)

    WE_CNT , RE_CNT : INTEGER ;

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
    (* ERROR MESSAGES:                        *)
    (* ***************                        *)
    (******************************************)

    ERRLOG : set of ERRCODE ;
    ERRORCNT , WARNCNT : INTEGER ;

    (******************************************)
    (* ERRORS AND WARNINGS COUNTS             *)
    (******************************************)

    CTIME : INTEGER ;

    (******************************************)
    (* COMPILATION TIME                       *)
    (* KIND OF ERROR, 'E' / 'W' (WARNING)     *)
    (******************************************)

    ERRKIND : CHAR ;

    (***********************************************************)
    (* STRUCTURED CONSTANTS, READ-ONLY TABLES                  *)
    (* ********** *********  **** **** ******                  *)
    (***********************************************************)



const BLANKID : ALPHA = '            ' ;
      HEXTAB : array [ 0 .. 15 ] of CHAR = '0123456789abcdef' ;
      LOW_LETTERS : SET_CHAR =
      [ 'a' .. 'i' , 'j' .. 'r' , 's' .. 'z' ] ;
      UP_LETTERS : SET_CHAR =
      [ 'A' .. 'I' , 'J' .. 'R' , 'S' .. 'Z' ] ;
      HEX_CHARS : SET_CHAR =
      [ 'a' .. 'f' , 'A' .. 'F' , '0' .. '9' ] ;
      BIN_CHARS : SET_CHAR =
      [ '0' .. '1' ] ;
      CONSTBEGSYS : SYMSET =
      [ SYPLUS , SYMINUS , INTCONST , REALCONST , STRINGCONST , IDENT ]
        ;
      SIMPTYPEBEGSYS : SYMSET =
      [ SYPLUS , SYMINUS , INTDOTDOT , INTCONST , REALCONST ,
        STRINGCONST , IDENT , SYLPARENT ] ;
      TYPEBEGSYS : SYMSET =
      [ SYARROW , SYPACKED , SYARRAY , SYRECORD , SYSET , SYFILE ,
        SYPLUS , SYMINUS , INTCONST , REALCONST , STRINGCONST , IDENT ,
        SYLPARENT ] ;
      TYPEDELS : SYMSET =
      [ SYARRAY , SYRECORD , SYSET , SYFILE , SYPACKED ] ;
      BLOCKBEGSYS : SYMSET =
      [ SYLABEL , SYCONST , SYTYPE , SYVAR , SYSTATIC , SYPROC , SYFUNC
        , SYLOCAL , SYBEGIN ] ;
      SELECTSYS : SYMSET =
      [ SYARROW , SYPERIOD , SYLBRACK , SYLPARENT ] ;
      FACBEGSYS : SYMSET =
      [ INTCONST , REALCONST , STRINGCONST , IDENT , SYLPARENT ,
        SYLBRACK , SYNOT ] ;
      STATBEGSYS : SYMSET =
      [ SYBEGIN , SYGOTO , SYIF , SYWHILE , SYREPEAT , SYFOR , SYWITH ,
        SYCASE , SYBREAK , SYCONTINUE , SYRETURN ] ;
      PROCCALLENDSYS : SYMSET =
      [ SYLPARENT , SYSEMICOLON , SYEND , SYELSE , SYUNTIL ] ;
      FACTOROPS : SYMSET =
      [ SYMULT , SYSLASH , SYDIV , SYMOD , SYAND , SYANDOP ] ;
      TERMOPS : SYMSET =
      [ SYPLUS , SYMINUS , SYOR , SYOROP , SYXOR , SYCONCAT ] ;
      EXPROPS : SYMSET =
      [ SYEQOP , SYNEOP , SYGTOP , SYLTOP , SYGEOP , SYLEOP , SYIN ] ;

      (*********************************************************)
      (*   new reserved symbols in the 2011 version:           *)
      (*   break, return, continue                             *)
      (*********************************************************)

      RW : array [ 1 .. MAXRW ] of ALPHA =
      ( 'IF          ' , 'DO          ' , 'OF          ' ,
        'TO          ' , 'IN          ' , 'OR          ' ,
        'END         ' , 'FOR         ' , 'VAR         ' ,
        'DIV         ' , 'MOD         ' , 'SET         ' ,
        'AND         ' , 'NOT         ' , 'XOR         ' ,
        'THEN        ' , 'ELSE        ' , 'WITH        ' ,
        'GOTO        ' , 'CASE        ' , 'TYPE        ' ,
        'FILE        ' , 'BEGIN       ' , 'UNTIL       ' ,
        'WHILE       ' , 'ARRAY       ' , 'CONST       ' ,
        'LABEL       ' , 'LOCAL       ' , 'BREAK       ' ,
        'REPEAT      ' , 'RECORD      ' , 'DOWNTO      ' ,
        'PACKED      ' , 'RETURN      ' , 'MODULE      ' ,
        'STATIC      ' , 'FORWARD     ' , 'PROGRAM     ' ,
        'FORTRAN     ' , 'EXTERNAL    ' , 'FUNCTION    ' ,
        'CONTINUE    ' , 'PROCEDURE   ' , 'OTHERWISE   ' ,
        '            ' , '            ' , '            ' ,
        '            ' , '            ' ) ;
      FRW : array [ 1 .. 12 ] of 1 .. MAXRW =

      (**********************************************************)
      (*  1  2  3    4    5    6    7    8    9   10   11   12  *)
      (**********************************************************)

      ( 1 , 1 , 7 , 16 , 23 , 31 , 38 , 41 , 44 , 46 , - 1 , - 1 ) ;
      RSY : array [ 1 .. MAXRW ] of SYMB =
      ( SYIF , SYDO , SYOF , SYTO , SYIN , SYOR , SYEND , SYFOR , SYVAR
        , SYDIV , SYMOD , SYSET , SYAND , SYNOT , SYXOR , SYTHEN ,
        SYELSE , SYWITH , SYGOTO , SYCASE , SYTYPE , SYFILE , SYBEGIN ,
        SYUNTIL , SYWHILE , SYARRAY , SYCONST , SYLABEL , SYLOCAL ,
        SYBREAK , SYREPEAT , SYRECORD , SYDOWNTO , SYPACKED , SYRETURN
        , SYMODULE , SYSTATIC , SYFORWARD , SYPROG , SYFRTRN , SYEXTRN
        , SYFUNC , SYCONTINUE , SYPROC , SYOTHERWISE , NOTUSED ,
        NOTUSED , NOTUSED , NOTUSED , NOTUSED ) ;
      MN : array [ 0 .. OPMAX ] of array [ 1 .. 4 ] of CHAR =
      ( ' ABI' , ' ABR' , ' ADI' , ' ADR' , ' AND' , ' DIF' , ' DVI' ,
        ' DVR' , ' SBR' , ' FLO' , ' FLT' , ' INN' , ' INT' , ' IOR' ,
        ' MOD' , ' MPI' , ' MPR' , ' NGI' , ' NGR' , ' NOT' , ' ODD' ,
        ' SBI' , ' DEC' , ' INC' , ' SQI' , ' SQR' , ' STO' , ' ---' ,
        ' ---' , ' SCL' , ' CSP' , ' UNI' , ' ENT' , ' FJP' , ' POP' ,
        ' IND' , ' IXA' , ' LCA' , ' CTS' , ' CTI' , ' MOV' , ' MST' ,
        ' RET' , ' STP' , ' XJP' , ' CHK' , ' CUP' , ' EQU' , ' GEQ' ,
        ' GRT' , ' LDA' , ' LDC' , ' LEQ' , ' LES' , ' LOD' , ' NEQ' ,
        ' STR' , ' UJP' , ' NEW' , ' SAV' , ' RST' , ' ORD' , ' CHR' ,
        ' DEF' , ' LAB' , ' CRD' , ' XPO' , ' ASE' , ' SLD' , ' SMV' ,
        ' DFC' , ' CST' , ' BGN' , ' UXJ' , ' XLB' , ' END' , ' PAK' ,
        ' ADA' , ' SBA' , ' XOR' , ' MFI' , ' MCP' , ' MSE' , ' DBG' ,
        ' MZE' , ' VC1' , ' VC2' , ' VCC' , ' VLD' , ' VST' , ' VMV' ,
        ' VSM' , ' VLM' , ' VPU' , ' VPO' , ' VIX' , ' VRP' , '    ' )
        ;

      (*********************************************************)
      (*   names of CSPs, index type should be CSPTYPE,        *)
      (*   but it is indexed by ORD (CSP) in GEN1 ...          *)
      (*********************************************************)

      CSPNAME : array [ 0 .. 47 ] of array [ 1 .. 3 ] of CHAR =
      ( 'PAG' , 'GET' , 'PUT' , 'RES' , 'REW' , 'RDC' , 'WRI' , 'WRE' ,
        'WRR' , 'WRC' , 'WRS' , 'WRX' , 'RDB' , 'WRB' , 'RDR' , 'RDH' ,
        'RDY' , 'EOL' , 'EOT' , 'RDD' , 'WRD' , 'CLK' , 'WLN' , 'RLN' ,
        'RDI' , 'EOF' , 'ELN' , 'RDS' , 'TRP' , 'XIT' , 'FDF' , 'SIO' ,
        'EIO' , 'MSG' , 'SKP' , 'LIM' , 'TRA' , 'WRP' , 'CLS' , 'DAT' ,
        'TIM' , 'FLR' , 'TRC' , 'RND' , 'WRV' , '   ' , '   ' , '   ' )
        ;

      (*********************************************************)
      (*-------------------------------------------------------*)
      (*********************************************************)




procedure PASSCANS ( var SCANOUT : TEXT ; var SCB : SCAN_BLOCK ) ;

   EXTERNAL ;



procedure PASSCANL ( var SCANINP : TEXT ; var SCANOUT : TEXT ; var SCB
                   : SCAN_BLOCK ; ALLES : BOOLEAN ) ;

   EXTERNAL ;



procedure PASSCANE ( var SCB : SCAN_BLOCK ; ERRLEVEL : CHAR ; ERRCLASS
                   : CHAR ; I : INTEGER ; INFO : CHAR32 ; ZEILNR :
                   INTEGER ; PLATZ : INTEGER ) ;

   EXTERNAL ;



procedure PASSCANF ( var SCB : SCAN_BLOCK ; WHICHTABLE : CHAR ;
                   ERRCLASS : SCAN_ERRCLASS ; ERRNUM : INTEGER ; ERRMSG
                   : SOURCELINE ; ERRMSGSIZE : INTEGER ) ;

   EXTERNAL ;



procedure PASSCAN ( var SCANINP : TEXT ; var SCANOUT : TEXT ; var SCB :
                  SCAN_BLOCK ; DO_COMMENT : BOOLEAN ) ;

   EXTERNAL ;



procedure DRUCKE_SYMSET ( T : CHAR32 ; S : SYMSET ) ;

   var X : SYMB ;

   begin (* DRUCKE_SYMSET *)
     WRITELN ( TRACEF , 'symset at ' , T ) ;
     for X := SYMB_EOF to NOTUSED do
       if X in S then
         WRITE ( TRACEF , X , ' ' ) ;
     WRITELN ( TRACEF ) ;
     WRITELN ( TRACEF ) ;
   end (* DRUCKE_SYMSET *) ;



procedure TOALFA ( var X : ALPHA ; Y : EXTNAMTP ) ;

   var I : INTEGER ;

   begin (* TOALFA *)
     X := BLANKID ;
     for I := 1 to EXTNAMSZ do
       X [ I ] := Y [ I ]
   end (* TOALFA *) ;



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



function MODP ( X : INTEGER ; Y : INTEGER ) : INTEGER ;

   var M : INTEGER ;

   begin (* MODP *)
     M := X MOD Y ;
     if M < 0 then
       M := M + Y ;
     MODP := M ;
   end (* MODP *) ;



procedure INTTOSTR ( CP : VOIDPTR ; LEN : INTEGER ; VAL : INTEGER ;
                   ZEROES : BOOLEAN ) ;

   var BUFFER : array [ 1 .. 20 ] of CHAR ;
       MINUS : BOOLEAN ;
       LETZT : INTEGER ;
       I : INTEGER ;
       LIMIT : INTEGER ;
       LENX : INTEGER ;
       POSX : INTEGER ;

   begin (* INTTOSTR *)
     if VAL < 0 then
       begin
         VAL := - VAL ;
         MINUS := TRUE
       end (* then *)
     else
       MINUS := FALSE ;
     I := 20 ;
     BUFFER := ' ' ;
     if VAL = 0 then
       begin
         BUFFER [ I ] := '0' ;
         I := I - 1 ;
       end (* then *)
     else
       while VAL > 0 do
         begin
           LETZT := VAL MOD 10 ;
           BUFFER [ I ] := CHR ( ORD ( '0' ) + LETZT ) ;
           I := I - 1 ;
           VAL := VAL DIV 10 ;
         end (* while *) ;
     if ZEROES then
       begin
         LIMIT := 20 - LEN ;
         while I > LIMIT do
           begin
             BUFFER [ I ] := '0' ;
             I := I - 1 ;
           end (* while *) ;
         if MINUS then
           BUFFER [ I + 1 ] := '-'
       end (* then *)
     else
       begin
         if MINUS then
           BUFFER [ I ] := '-' ;
         I := I - 1 ;
       end (* else *) ;
     LENX := 20 - I ;
     POSX := LEN - LENX ;
     MEMSET ( CP , ' ' , LEN ) ;
     MEMCPY ( PTRADD ( CP , POSX ) , ADDR ( BUFFER [ I + 1 ] ) , LENX )
              ;
   end (* INTTOSTR *) ;



procedure ERROR_POS ( ERRTYPE : CHAR ; FERRNR : ERRCODE ; ERRINFO :
                    CHAR32 ; LINENR : INTEGER ; LINEPOS : INTEGER ) ;

(*****************************************************)
(*  error-Prozedur                                   *)
(*  positionsangaben ueber parameter                 *)
(*****************************************************)


   var PASSCAN_ERRTYPE : CHAR ;

   begin (* ERROR_POS *)
     PASSCAN_ERRTYPE := ERRTYPE ;
     if PASSCAN_ERRTYPE = 'E' then
       PASSCAN_ERRTYPE := 'F' ;
     PASSCANE ( SCB , PASSCAN_ERRTYPE , 'P' , FERRNR , ERRINFO , LINENR
                , LINEPOS ) ;
     ERRLN := LINENR ;
     if ( ERRTYPE <> 'W' ) or OPT . WARNING then
       begin
         ERRLOG := ERRLOG + [ FERRNR ] ;
         if ERRTYPE <> 'W' then
           ERRORCNT := ERRORCNT + 1
         else
           WARNCNT := WARNCNT + 1 ;
       end (* then *) ;
   end (* ERROR_POS *) ;



procedure ERROR ( FERRNR : ERRCODE ) ;

(*****************************************************)
(*  error-Prozedur                                   *)
(*  position aus scb (standard)                      *)
(*****************************************************)


   var PASSCAN_ERRTYPE : CHAR ;

   begin (* ERROR *)

     (************************************************)
     (*  errkind wird ggf. vorher auf 'W' gesetzt    *)
     (*  nachher immer auf 'E'                       *)
     (************************************************)

     ERROR_POS ( ERRKIND , FERRNR , ' ' , SCB . LINENR , SCB . LINEPOS
                 ) ;
     ERRKIND := 'E' ;
   end (* ERROR *) ;



procedure LISTMSGS ;

   var I , J : ERRCODE ;
       INPLINE : SOURCELINE ;
       ERRMSG : array [ 1 .. 64 ] of CHAR ;

   begin (* LISTMSGS *)
     WRITELN ( OUTPUT ) ;
     if ERRLN > 0 then
       begin
         WRITELN ( OUTPUT , '****' : 7 , ' Last Error/Warning on Line '
                   , ERRLN : 4 ) ;
         WRITELN ( OUTPUT ) ;
       end (* then *) ;
     WRITELN ( OUTPUT , '****' : 7 ,
               ' Error/Warning Codes for this Program:' ) ;
     WRITELN ( LISTING ) ;
     if ERRLN > 0 then
       begin
         WRITELN ( LISTING , '****' : 7 ,
                   ' Last Error/Warning on Line ' , ERRLN : 4 ) ;
         WRITELN ( LISTING ) ;
       end (* then *) ;
     WRITELN ( LISTING , '****' : 7 ,
               ' Error/Warning Codes for this Program:' ) ;
     WRITELN ( LISTING ) ;
     RESET ( PRD ) ;
     J := 0 ;
     for I := 1 to MAXERRNR do
       begin
         if I in ERRLOG then
           begin
             while ( not EOF ( PRD ) ) and ( I > J ) do
               READLN ( PRD , J , INPLINE ) ;
             MEMCPY ( ADDR ( ERRMSG ) , ADDR ( INPLINE [ 4 ] ) , 64 ) ;
             if J = I then
               begin
                 WRITELN ( '****' : 7 , ' P' , J : - 3 , ': ' , ERRMSG
                           ) ;
                 WRITELN ( LISTING , '****' : 7 , ' P' , J : - 3 , ': '
                           , ERRMSG ) ;
               end (* then *)
           end (* then *)
       end (* for *)
   end (* LISTMSGS *) ;



procedure HELLO ;

   begin (* HELLO *)
     CTIME := CLOCK ( 0 ) ;
     WRITELN ( OUTPUT , '****' : 7 ,
               ' STANFORD PASCAL COMPILER, OPPOLZER VERSION OF ' ,
               VERSION , ' ****' ) ;
     WRITELN ( OUTPUT ) ;
   end (* HELLO *) ;



procedure GOODBYE ;

   begin (* GOODBYE *)
     CTIME := ( CLOCK ( 0 ) - CTIME ) ;
     WRITELN ( OUTPUT , '****' : 7 , ' Compiler Summary ****' ) ;
     if OPT . WARNING then
       begin
         if WARNCNT > 0 then
           WRITELN ( OUTPUT , '****' : 7 , WARNCNT : 8 ,
                     ' WARNING MESSAGE(S) ISSUED.' ) ;
         if OPT . MWARN then
           WRITELN ( OUTPUT , '****' : 7 ,
                     ' CONTENTS OF SOURCE LINES OUTSIDE  ' , OPT .
                     LMARGIN : 1 , '..' , OPT . RMARGIN : 1 ,
                     '  MARGINS IGNORED.' ) ;
       end (* then *) ;
     WRITE ( OUTPUT , '****' : 7 ) ;
     PASSCANS ( OUTPUT , SCB ) ;
     WRITELN ( OUTPUT , '****' : 7 , LINECNT : 8 , ' LINE(S) READ, ' ,
               PROCLAB : 4 , ' PROCEDURE(S) COMPILED,' ) ;
     WRITELN ( OUTPUT , '****' : 7 , OLDICOUNTER : 8 ,
               ' P_INSTRUCTIONS GENERATED,' , CTIME * 0.001 : 7 : 2 ,
               ' SECONDS IN COMPILATION.' ) ;
     WRITELN ( LISTING ) ;
     if OPT . WARNING then
       begin
         if EXTUSED then
           WRITELN ( LISTING , '****' : 7 ,
                     ' WARNING: PASCAL EXTENSIONS USED.' ) ;
         if OPT . MWARN then
           WRITELN ( LISTING , '****' : 7 ,
                     ' CONTENTS OF SOURCE LINES OUTSIDE  ' , OPT .
                     LMARGIN : 1 , '..' , OPT . RMARGIN : 1 ,
                     '  MARGINS IGNORED.' ) ;
       end (* then *) ;
     WRITE ( LISTING , '****' : 7 ) ;
     PASSCANS ( LISTING , SCB ) ;
     WRITELN ( LISTING , '****' : 7 , LINECNT : 8 , ' LINE(S) READ, ' ,
               PROCLAB : 4 , ' PROCEDURE(S) COMPILED,' ) ;
     WRITELN ( LISTING , '****' : 7 , OLDICOUNTER : 8 ,
               ' P_INSTRUCTIONS GENERATED,' , CTIME * 0.001 : 7 : 2 ,
               ' SECONDS IN COMPILATION.' ) ;
     if ( ERRORCNT > 0 ) or ( WARNCNT > 0 ) then
       LISTMSGS ;
     EXIT ( ERRORCNT ) ;
   end (* GOODBYE *) ;



procedure FATALERROR ( CODE : ERRCODE ) ;

   var ANZSYMBOLS : INTEGER ;

   begin (* FATALERROR *)
     ERROR ( CODE ) ;
     ANZSYMBOLS := 10 ;
     while ( SCB . SYMBOLNR <> SYMB_EOF ) and ( ANZSYMBOLS > 0 ) do
       begin
         PASSCAN ( INPUT , LISTING , SCB , TRUE ) ;
         if not ( SCB . SYMBOLNR in [ SYMB_EOF , SEPARATOR , COMMENT1 ,
         COMMENT2 , COMMENT3 , COMMENT4 , COMMENT5 ] ) then
           ERROR ( 402 ) ;
         ANZSYMBOLS := ANZSYMBOLS - 1 ;
       end (* while *) ;
     if CODE <> 390 then
       begin
         WRITELN ( LISTING ) ;
         WRITELN ( LISTING ) ;
         WRITELN ( LISTING , '****' : 7 , 'FATAL ERROR - ' ,
                   'COMPILATION TERMINATED PREMATURELY.' ) ;
         WRITELN ( LISTING ) ;
       end (* then *) ;
     GOODBYE ;
   end (* FATALERROR *) ;



procedure WORK_OPTIONS ( var OPTLINE : SOURCELINE ; var SCB :
                       SCAN_BLOCK ; var OPT : COMP_OPTIONS ) ;

   var OPTIX : INTEGER ;
       OPTCH : CHAR ;
       SCANCH : CHAR ;


   function NEXTCH : CHAR ;

      begin (* NEXTCH *)
        OPTIX := OPTIX + 1 ;
        if OPTIX > SIZEOF ( SOURCELINE ) then
          NEXTCH := ' '
        else
          NEXTCH := OPTLINE [ OPTIX ]
      end (* NEXTCH *) ;


   function DECNUM : INTEGER ;

      var NUM : INTEGER ;

      begin (* DECNUM *)
        NUM := 0 ;
        SCANCH := NEXTCH ;
        while SCANCH in [ '0' .. '9' ] do
          begin
            NUM := NUM * 10 + ORD ( SCANCH ) - ORD ( '0' ) ;
            SCANCH := NEXTCH ;
          end (* while *) ;
        DECNUM := NUM
      end (* DECNUM *) ;


   begin (* WORK_OPTIONS *)
     OPTIX := 0 ;
     with OPT do
       begin
         OPTCH := NEXTCH ;
         repeat
           SCANCH := NEXTCH ;
           case OPTCH of
             'A' : ASSEMBLE := SCANCH = '+' ;
             'C' : PRCODE := SCANCH <> '-' ;
             'D' : if SCANCH in [ '0' .. '9' ] then
                     begin
                       DEBUG_LEV := ORD ( SCANCH ) - ORD ( '0' ) ;
                       DEBUG := DEBUG_LEV >= 2 ;
                       if FALSE then
                         begin
                           WRITELN ( 'option d debug_lev = ' ,
                                     DEBUG_LEV ) ;
                           WRITELN ( 'option d debug = ' , DEBUG )
                         end (* then *) ;
                       SCANCH := NEXTCH ;
                     end (* then *)
                   else
                     begin
                       DEBUG := SCANCH <> '-' ;
                       DEBUG_LEV := ORD ( DEBUG ) * 2 ;
                       if FALSE then
                         begin
                           WRITELN ( 'option d debug_lev = ' ,
                                     DEBUG_LEV ) ;
                           WRITELN ( 'option d debug = ' , DEBUG )
                         end (* then *) ;
                     end (* else *) ;
             'E' : SCB . LINECOUNT := PAGESIZE + 1 ;
             'F' : SAVEFPRS := SCANCH <> '-' ;
             'K' : CTROPTION := SCANCH = '+' ;
             'L' : LIST := SCANCH <> '-' ;
             'M' : begin
                     if SCANCH = '+' then
                       begin
                         LMARGIN := 0 ;
                         RMARGIN := 72 ;
                       end (* then *)
                     else
                       if SCANCH = '-' then
                         begin
                           LMARGIN := 0 ;
                           RMARGIN := MAXLSIZE ;
                         end (* then *)
                       else
                         if SCANCH = '(' then
                           begin
                             LMARGIN := DECNUM - 1 ;
                             if LMARGIN < 0 then
                               LMARGIN := 0 ;
                             if SCANCH = ',' then
                               RMARGIN := DECNUM
                             else
                               RMARGIN := MAXLSIZE ;
                             if ( RMARGIN <= LMARGIN ) or ( RMARGIN >=
                             MAXLSIZE ) then
                               RMARGIN := MAXLSIZE - 1 ;
                             if SCANCH = ')' then
                               SCANCH := NEXTCH
                           end (* then *) ;
                     if FALSE then
                       begin
                         WRITELN ( 'option m lmargin = ' , LMARGIN ) ;
                         WRITELN ( 'option m rmargin = ' , RMARGIN )
                       end (* then *) ;
                   end (* tag/ca *) ;
             'N' : NESTCOMM := SCANCH = '+' ;
             'P' : NOPACKING := SCANCH = '-' ;
             'Q' : begin
                     PAGESIZE := DECNUM ;
                     if FALSE then
                       WRITELN ( 'option q pagesize = ' , PAGESIZE ) ;
                   end (* tag/ca *) ;
             'S' : SAVEREGS := SCANCH <> '-' ;
             'U' : GET_STAT := SCANCH = '+' ;
             'V' : ASMVERB := SCANCH = '+' ;
             'W' : WARNING := SCANCH <> '-' ;
           end (* case *) ;
           if SCANCH in [ '+' , '-' ] then
             SCANCH := NEXTCH ;
           if SCANCH = ',' then
             SCANCH := NEXTCH ;
           OPTCH := SCANCH ;
         until SCANCH = ' '
       end (* with *)
   end (* WORK_OPTIONS *) ;



procedure INSYMBOL ;

(**************************************************************)
(*                                                            *)
(*   READ NEXT BASIS SYMBOL OF SOURCE PROGRAM AND RETURN      *)
(*   ITS DESCRIPTION IN THE GLOBAL VARIABLES                  *)
(*   SY, OP, ID, VAL AND SYLENGTH                             *)
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


   var I , K : INTEGER ;
       DIGIT : array [ 1 .. 40 ] of CHAR ;
       SARRAY : array [ 1 .. MAXSTRL ] of CHAR ;
       SCH_DIG : BOOLEAN ;
       SCH_PREC : BOOLEAN ;


   procedure MODSTRING ( STRTYPE : CHAR ; var L : INTEGER ) ;

      var IX : INTEGER ;
          LNEU : INTEGER ;
          L1 : INTEGER ;
          X : INTEGER ;
          X2 : INTEGER ;
          INEU : INTEGER ;
          SX : INTEGER ;
          TX : INTEGER ;

      begin (* MODSTRING *)
        if FALSE then
          begin
            WRITE ( TRACEF , 'MODSTRING: ' , STRTYPE , ' ' , L : 1 ,
                    ' <' ) ;
            for I := 1 to L do
              WRITE ( TRACEF , SARRAY [ I ] ) ;
            WRITELN ( TRACEF , '>' ) ;
          end (* then *) ;
        case STRTYPE of
          'C' : begin
                  SX := 1 ;
                  TX := 0 ;
                  while SX <= L do
                    begin
                      if ( SARRAY [ SX ] <> '''' ) or ( SX = L ) then
                        begin
                          TX := TX + 1 ;
                          if TX < SX then
                            SARRAY [ TX ] := SARRAY [ SX ] ;
                          SX := SX + 1
                        end (* then *)
                      else
                        if SARRAY [ SX + 1 ] = '''' then
                          begin
                            TX := TX + 1 ;
                            if TX < SX then
                              SARRAY [ TX ] := SARRAY [ SX ] ;
                            SX := SX + 2
                          end (* then *)
                    end (* while *) ;
                  L := TX ;
                end (* tag/ca *) ;
          'B' : begin

        /**********************/
        /* remove underscores */
        /**********************/

                  LNEU := 0 ;
                  for X := 1 to L do
                    if SARRAY [ X ] <> '_' then
                      begin
                        LNEU := LNEU + 1 ;
                        if LNEU < X then
                          SARRAY [ LNEU ] := SARRAY [ X ] ;
                      end (* then *) ;
                  L := LNEU ;

        /**************************************/
        /* l = new length without underscores */
        /**************************************/

                  IX := 1 ;
                  LNEU := ( L + 7 ) DIV 8 ;

        /********************************************/
        /* lneu = length of converted target string */
        /********************************************/

                  L1 := L MOD 8 ;
                  if L1 = 0 then
                    L1 := 8 ;
                  for X := 1 to LNEU do
                    begin
                      INEU := 0 ;
                      for X2 := 1 to L1 do
                        begin
                          INEU := INEU * 2 ;
                          if SARRAY [ IX ] = '1' then
                            INEU := INEU + 1 ;
                          IX := IX + 1 ;
                        end (* for *) ;
                      L1 := 8 ;
                      SARRAY [ X ] := CHR ( INEU ) ;
                    end (* for *) ;
                  L := LNEU ;
                end (* tag/ca *) ;
          'X' : begin
                  LNEU := 0 ;
                  for X := 1 to L do
                    if SARRAY [ X ] <> '_' then
                      begin
                        LNEU := LNEU + 1 ;
                        if LNEU < X then
                          SARRAY [ LNEU ] := SARRAY [ X ] ;
                      end (* then *) ;
                  L := LNEU ;

        /**************************************/
        /* l = new length without underscores */
        /**************************************/

                  IX := 1 ;
                  LNEU := ( L + 1 ) DIV 2 ;

        /********************************************/
        /* lneu = length of converted target string */
        /********************************************/

                  L1 := L MOD 2 ;
                  if L1 = 0 then
                    L1 := 2 ;
                  for X := 1 to LNEU do
                    begin
                      INEU := 0 ;
                      for X2 := 1 to L1 do
                        begin
                          INEU := INEU * 16 ;
                          if SARRAY [ IX ] in [ '1' .. '9' ] then
                            INEU := INEU + ORD ( SARRAY [ IX ] ) - ORD
                                    ( '0' )
                          else
                            if SARRAY [ IX ] in [ 'A' .. 'F' ] then
                              INEU := INEU + ORD ( SARRAY [ IX ] ) -
                                      ORD ( 'A' ) + 10
                            else
                              if SARRAY [ IX ] in [ 'a' .. 'f' ] then
                                INEU := INEU + ORD ( SARRAY [ IX ] ) -
                                        ORD ( 'a' ) + 10 ;
                          IX := IX + 1 ;
                        end (* for *) ;
                      L1 := 2 ;
                      SARRAY [ X ] := CHR ( INEU ) ;
                    end (* for *) ;
                  L := LNEU ;
                end (* tag/ca *)
        end (* case *) ;
        if FALSE then
          begin
            WRITE ( TRACEF , 'ENDE MODS: ' , STRTYPE , ' ' , L : 1 ,
                    ' <' ) ;
            for I := 1 to L do
              if STRTYPE = 'C' then
                WRITE ( TRACEF , SARRAY [ I ] )
              else
                WRITEHEXBYTE ( TRACEF , ORD ( SARRAY [ I ] ) ) ;
            WRITELN ( TRACEF , '>' ) ;
          end (* then *)
      end (* MODSTRING *) ;


   begin (* INSYMBOL *)
     VAL . IVAL := 0 ;
     VAL . STRTYPE := ' ' ;

     (**********************************************************)
     (*   diese logik war frueher in der prozedur endofline    *)
     (**********************************************************)

     if HP then
       begin
         ICOUNTER := 0 ;
         LISTTAG := ' ' ;
         HP := FALSE
       end (* then *) ;

     (**********************************************************)
     (*   lineinfo setzen fuer compilerliste                   *)
     (**********************************************************)

     if LISTTAG = 'N' then
       DCN := STMTNEST
     else
       if LISTTAG = 'D' then
         DCN := OLDLCOUNTER
       else
         if LISTTAG = 'S' then
           DCN := OLDCONSTLCOUNTER
         else
           if LISTTAG = 'C' then
             DCN := OLDCONSTLCOUNTER
           else
             DCN := 0 ;
     if DCN < 0 then
       DCN := 0 ;

     (**********************************************************)
     (*   nur in liste einbauen, wenn aenderung                *)
     (*   gegenueber dem letzten mal ...                       *)
     (**********************************************************)

     if ( LISTTAG <> OLDLISTTAG ) or ( LEVEL <> OLDLEVEL ) or ( DCN <>
     OLDDCN ) then
       begin
         if ( LISTTAG in [ 'D' , 'S' , 'C' ] ) or ( DCN > 0 ) then
           begin
             SCB . LINEINFO := ' ' ;
             INTTOSTR ( ADDR ( SCB . LINEINFO ) , 7 , DCN , FALSE ) ;
             SCB . LINEINFO [ 8 ] := LISTTAG ;
             INTTOSTR ( ADDR ( SCB . LINEINFO [ 9 ] ) , 4 , LEVEL ,
                        FALSE ) ;
           end (* then *)
         else
           SCB . LINEINFO := ' ' ;
         SCB . LINEINFO [ 13 ] := ')' ;
         SCB . LINEINFO_SIZE := 14 ;
         OLDLISTTAG := LISTTAG ;
         OLDLEVEL := LEVEL ;
         OLDDCN := DCN ;
       end (* then *) ;

     (**********************************************************)
     (*   schleife, z.b. wg. blanks und kommentaren            *)
     (**********************************************************)

     while TRUE do
       begin
         SYDIGITS := 0 ;
         SYPREC := 0 ;

     (**********************************************************)
     (*   scanner aufrufen (externes modul)                    *)
     (**********************************************************)

         PASSCAN ( INPUT , LISTING , SCB , TRUE ) ;

     (**********************************************************)
     (*   variablen sy und sylength setzen (rueckg. scanner)   *)
     (**********************************************************)

         SY := SCB . SYMBOLNR ;
         SYLENGTH := SCB . LSYMBOL ;
         LINECNT := SCB . LINENR ;

     (**********************************************************)
     (*   look what has to be done depending on symbol         *)
     (*   (some symbols need additional work)                  *)
     (**********************************************************)

         case SY of

     (**********************************************************)
     (*   separator und kommentare ignorieren und nochmal      *)
     (**********************************************************)

           SEPARATOR :
             continue ;
           COMMENT1 .. COMMENT5 :
             begin
               if SCB . OPTLINE <> ' ' then
                 WORK_OPTIONS ( SCB . OPTLINE , SCB , OPT ) ;
               continue ;
             end (* tag/ca *) ;

     (**********************************************************)
     (*   ident in grossbuchstaben und gegen tabelle der       *)
     (*   reservierten worte abchecken                         *)
     (**********************************************************)

           IDENT : begin
                     ID := ' ' ;
                     K := SYLENGTH ;
                     if K > IDLNGTH then
                       K := IDLNGTH ;
                     MEMCPY ( ADDR ( ID ) , ADDR ( SCB . SYMBOL ) , K )
                              ;
                     for I := 1 to K do
                       ID [ I ] := UPSHIFT [ ID [ I ] ] ;

     (**********************************************************)
     (*   maxrwlen = laenge des laengsten reservierten wortes  *)
     (*   die tabelle frw ist nur so lang                      *)
     (**********************************************************)

                     if K <= MAXRWLEN then
                       for I := FRW [ K ] to FRW [ K + 1 ] - 1 do
                         if RW [ I ] = ID then
                           begin
                             SY := RSY [ I ] ;
                             break ;
                           end (* then *) ;
                   end (* tag/ca *) ;

     (**********************************************************)
     (*   stringconst nacharbeiten wg. doppelter               *)
     (*   hochkommas z.B.                                      *)
     (**********************************************************)

           STRINGCONST :
             begin
               K := SYLENGTH - 2 ;
               MEMCPY ( ADDR ( SARRAY ) , ADDR ( SCB . SYMBOL [ 2 ] ) ,
                        K ) ;
               MODSTRING ( 'C' , K ) ;
               VAL . STRTYPE := ' ' ;
               SYLENGTH := K ;
               if SYLENGTH = 0 then
                 VAL . IVAL := ORD ( ' ' )
               else
                 if SYLENGTH = 1 then
                   VAL . IVAL := ORD ( SARRAY [ 1 ] )
                 else
                   begin
                     if SYLENGTH > MAXSTRL then
                       begin
                         ERROR ( 398 ) ;
                         SYLENGTH := MAXSTRL
                       end (* then *) ;
                     NEW ( VAL . SVAL ) ;
                     VAL . SVAL -> . TAG := 'S' ;
                     VAL . SVAL -> . LENGTH := SYLENGTH ;
                     VAL . SVAL -> . SSTR := SARRAY ;
                   end (* else *)
             end (* tag/ca *) ;

     (**********************************************************)
     (*   hex stringconst umcodieren                           *)
     (**********************************************************)

           HEXSTRINGCONST :
             begin
               K := SYLENGTH - 3 ;
               MEMCPY ( ADDR ( SARRAY ) , ADDR ( SCB . SYMBOL [ 3 ] ) ,
                        K ) ;
               MODSTRING ( 'X' , K ) ;
               VAL . STRTYPE := 'X' ;
               SYLENGTH := K ;
               if SYLENGTH = 0 then
                 begin
                   VAL . IVAL := ORD ( ' ' ) ;
                   SYLENGTH := 1 ;
                 end (* then *)
               else
                 if SYLENGTH = 1 then
                   VAL . IVAL := ORD ( SARRAY [ 1 ] )
                 else
                   begin
                     if SYLENGTH > MAXSTRL then
                       begin
                         ERROR ( 398 ) ;
                         SYLENGTH := MAXSTRL
                       end (* then *) ;
                     NEW ( VAL . SVAL ) ;
                     VAL . SVAL -> . TAG := 'S' ;
                     VAL . SVAL -> . LENGTH := SYLENGTH ;
                     VAL . SVAL -> . SSTR := SARRAY ;
                   end (* else *) ;
               SY := STRINGCONST ;
             end (* tag/ca *) ;

     (**********************************************************)
     (*   bin stringconst umcodieren                           *)
     (**********************************************************)

           BINSTRINGCONST :
             begin
               K := SYLENGTH - 3 ;
               MEMCPY ( ADDR ( SARRAY ) , ADDR ( SCB . SYMBOL [ 3 ] ) ,
                        K ) ;
               MODSTRING ( 'B' , K ) ;
               VAL . STRTYPE := 'X' ;
               SYLENGTH := K ;
               if SYLENGTH = 0 then
                 begin
                   VAL . IVAL := ORD ( ' ' ) ;
                   SYLENGTH := 1 ;
                 end (* then *)
               else
                 if SYLENGTH = 1 then
                   VAL . IVAL := ORD ( SARRAY [ 1 ] )
                 else
                   begin
                     if SYLENGTH > MAXSTRL then
                       begin
                         ERROR ( 398 ) ;
                         SYLENGTH := MAXSTRL
                       end (* then *) ;
                     NEW ( VAL . SVAL ) ;
                     VAL . SVAL -> . TAG := 'S' ;
                     VAL . SVAL -> . LENGTH := SYLENGTH ;
                     VAL . SVAL -> . SSTR := SARRAY ;
                   end (* else *) ;
               SY := STRINGCONST ;
             end (* tag/ca *) ;

     (**********************************************************)
     (*   intconst kann hex oder binaer sein ...               *)
     (**********************************************************)

           INTCONST , INTDOTDOT :
             begin
               DIGIT := ' ' ;
               K := SYLENGTH ;
               if SY = INTDOTDOT then
                 K := K - 2 ;
               if K > SIZEOF ( DIGIT ) then
                 K := SIZEOF ( DIGIT ) ;
               MEMCPY ( ADDR ( DIGIT ) , ADDR ( SCB . SYMBOL ) , K ) ;
               VAL . IVAL := 0 ;

     (***********************************************)
     (*   if hex const, translate to integer / ival *)
     (***********************************************)

               if ( DIGIT [ 2 ] = 'X' ) or ( DIGIT [ 2 ] = 'x' ) then
                 begin
                   with VAL do
                     for I := 3 to K do
                       if IVAL <= MXINT16 then
                         case DIGIT [ I ] of
                           '0' .. '9' :
                             IVAL := IVAL * 16 + ORD ( DIGIT [ I ] ) -
                                     ORD ( '0' ) ;
                           'A' .. 'F' :
                             IVAL := IVAL * 16 + ORD ( DIGIT [ I ] ) -
                                     ORD ( 'A' ) + 10 ;
                           'a' .. 'f' :
                             IVAL := IVAL * 16 + ORD ( DIGIT [ I ] ) -
                                     ORD ( 'a' ) + 10 ;
                           '_' : ;
                         end (* case *)
                       else
                         begin
                           ERROR ( 203 ) ;
                           IVAL := 0 ;
                           break
                         end (* else *)
                 end (* then *)
               else

     (***********************************************)
     (*   if bin const, translate to integer / ival *)
     (***********************************************)

                 if ( DIGIT [ 2 ] = 'B' ) or ( DIGIT [ 2 ] = 'b' ) then
                   begin
                     with VAL do
                       for I := 3 to K do
                         if IVAL <= MXINT2 then
                           case DIGIT [ I ] of
                             '0' : IVAL := IVAL * 2 ;
                             '1' : IVAL := IVAL * 2 + 1 ;
                             '_' : ;
                           end (* case *)
                         else
                           begin
                             ERROR ( 203 ) ;
                             IVAL := 0 ;
                             break
                           end (* else *)
                   end (* then *)
                 else
                   begin

     (*************************)
     (*   normal int constant *)
     (*************************)

                     with VAL do
                       for I := 1 to K do
                         if DIGIT [ I ] <> '_' then
                           begin
                             SYDIGITS := SYDIGITS + 1 ;
                             if IVAL <= MXINT10 then
                               IVAL := IVAL * 10 + ( ORD ( DIGIT [ I ]
                                       ) - ORD ( '0' ) )
                             else
                               begin
                                 ERROR ( 203 ) ;
                                 IVAL := 0 ;
                                 break
                               end (* else *)
                           end (* then *)
                   end (* else *)
             end (* tag/ca *) ;

     (**********************************************************)
     (*   realconst ...                                        *)
     (**********************************************************)

           REALCONST :
             begin
               DIGIT := ' ' ;
               K := SYLENGTH ;
               if K > SIZEOF ( DIGIT ) then
                 K := SIZEOF ( DIGIT ) ;
               MEMCPY ( ADDR ( DIGIT ) , ADDR ( SCB . SYMBOL ) , K ) ;
               VAL . RVAL := ' ' ;
               if K <= DIGMAX then
                 begin
                   SCH_DIG := TRUE ;
                   SCH_PREC := FALSE ;
                   for I := 2 to K + 1 do
                     begin
                       VAL . RVAL [ I ] := DIGIT [ I - 1 ] ;
                       if VAL . RVAL [ I ] = '.' then
                         SCH_PREC := TRUE ;
                       if ( VAL . RVAL [ I ] = 'e' ) or ( VAL . RVAL [
                       I ] = 'E' ) then
                         begin
                           SCH_DIG := FALSE ;
                           SCH_PREC := FALSE ;
                         end (* then *) ;
                       if VAL . RVAL [ I ] in [ '0' .. '9' ] then
                         begin
                           if SCH_DIG then
                             SYDIGITS := SYDIGITS + 1 ;
                           if SCH_PREC then
                             SYPREC := SYPREC + 1 ;
                         end (* then *)
                     end (* for *)
                 end (* then *)
               else
                 begin
                   ERROR ( 203 ) ;
                   UNPACK ( '0.0' , VAL . RVAL , 2 )
                 end (* else *)
             end (* tag/ca *) ;
           otherwise
             
         end (* case *) ;

     (**********************************************************)
     (*   endlosschleife jetzt beenden - wenn nicht vorher     *)
     (*   schon mit continue eine wiederholung angefordert     *)
     (*   wurde                                                *)
     (**********************************************************)

         break ;
       end (* while *) ;

     (**********************************************************)
     (*   store counter values at start of new line            *)
     (**********************************************************)

     if SCB . LINENR <> OLDLINENR then
       begin
         OLDLCOUNTER := LCOUNTER ;
         OLDCONSTLCOUNTER := CONSTLCOUNTER ;
         OLDLINENR := SCB . LINENR ;
       end (* then *) ;

     (**********************************************************)
     (*   debug trace                                          *)
     (**********************************************************)

     if FALSE then
       begin
         WRITELN ( TRACEF ) ;
         WRITELN ( TRACEF , 'rueckgabe vom scanner:' ) ;
         WRITELN ( TRACEF , 'sy       = ' , SY ) ;
         WRITELN ( TRACEF , 'sylength = ' , SYLENGTH : 1 ) ;
       end (* then *) ;

     (**********************************************************)
     (*   unexpected eof is a fatal error                      *)
     (**********************************************************)

     if SY = SYMB_EOF then
       FATALERROR ( 390 ) ;
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



procedure ENTERID ( FCP : IDP ) ;

   var K : BKT_RNG ;
       NAM : ALPHA ;
       LCP : IDP ;

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
                     break ;
                   end (* then *)
               end (* then *)
             else

     (*****************************)
     (* SPECIAL LOOKUP FOR FIELDS *)
     (*****************************)

               if TOP = OWNER -> . FLD_DISP_LEV then
                 begin
                   ERROR ( 101 ) ;
                   break ;
                 end (* then *) ;
           LCP := NEXT_IN_BKT ;
         end (* with *) ;
     if OPT . GET_STAT then
       if TOP = LEVEL then
         ENT_CNT [ LEVEL ] := ENT_CNT [ LEVEL ] + 1
       else
         FENT_CNT := FENT_CNT + 1 ;
   end (* ENTERID *) ;



procedure SEARCHSECTION ( FSP : TTP ; var FCP : IDP ) ;

(****************************************************)
(* FINDS FIELD IN RECORD STRUCTURE INDICATED BY FSP *)
(****************************************************)


   var LCP : IDP ;

   begin (* SEARCHSECTION *)
     LCP := BUCKET [ HASH ( ID ) ] ;
     while LCP <> NIL do
       with LCP -> do
         begin
           if NAME = ID then
             if KLASS = FIELD then
               if OWNER = FSP then
                 begin
                   if OPT . GET_STAT then
                     begin
                       SF_CNT := SF_CNT + 1 ;
                       SF_TOT := SF_TOT + FSP -> . NO_FLDS ;
                     end (* then *) ;
                   break ;
                 end (* then *) ;
           LCP := NEXT_IN_BKT ;
         end (* with *) ;
     FCP := LCP ;
   end (* SEARCHSECTION *) ;



function SEARCHID ( ID : ALPHA ; PRTERR : BOOLEAN ; INSERT_ON_ERR :
                  BOOLEAN ; FIDCLS : SETOFIDS ; var FCP : IDP ) :
                  INTEGER ;

(********************************************************)
(*   prueft, ob bestimmte Bezeichner bekannt sind       *)
(*                                                      *)
(*   id            - um diesen Bezeichner geht es       *)
(*   prterr        - Fehler anzeigen oder nicht         *)
(*   insert_on_err - Fehler anzeigen oder nicht         *)
(*   fidcls        - Bezeichnerklasse(n)                *)
(*   fcp           - Zeiger auf Datentyp usw.           *)
(*------------------------------------------------------*)
(*   globale variablen:                                 *)
(*                                                      *)
(*   disx          = display-index, gibt an, in         *)
(*                   welchem level der bezeichner       *)
(*                   gefunden wurde (wird extern        *)
(*                   nur bei selektor/field benoetigt)  *)
(********************************************************)


   var LCP : IDP ;
       DL , EL : - 1 .. DISPLIMIT ;
       K : BKT_RNG ;

   begin (* SEARCHID *)
     SEARCHID := 0 ;
     K := HASH ( ID ) ;
     LCP := BUCKET [ K ] ;
     FCP := NIL ;
     EL := - 1 ;
     DISX := EL ;

     /************************************************************/
     /* der bezeichner wird in den entsprechenden listen gesucht */
     /************************************************************/

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
     (*******************************)

                       break
                   end (* then *)
                 else
                   EL := DL ;
             end (* then *) ;
           LCP := NEXT_IN_BKT ;
         end (* with *) ;

     /************************************************/
     /* bezeichner gefunden, hat aber falsche klasse */
     /************************************************/

     if EL > DISX then
       begin
         SEARCHID := 103 ;
         if PRTERR then
           ERROR ( 103 ) ;
       end (* then *) ;

     /*****************************/
     /* bezeichner nicht gefunden */
     /*****************************/

     if DISX < 0 then
       begin
         SEARCHID := 104 ;
         if PRTERR then
           begin

     /****************************************************/
     /* fehler nur ausgeben, wenn nicht schon fehler 103 */
     /****************************************************/

             if EL < 0 then
               ERROR ( 104 ) ;
             DISX := LEVEL ;
           end (* then *)
         else
           DISX := 0 ;
         if INSERT_ON_ERR then
           begin

     /***************************************************/
     /* im fehlerfall bezeichner mit der entsprechenden */
     /* id-klasse eintragen, um folgefehler zu          */
     /* vermeiden                                       */
     /***************************************************/

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
                       LCP -> := UFCTPTR -> ;
             with LCP -> do
               begin
                 NAME := ID ;
                 DECL_LEV := LEVEL ;
                 NEXT_IN_BKT := BUCKET [ K ] ;
                 BUCKET [ K ] := LCP ;
                 FCP := LCP ;
                 if KLASS = FIELD then
                   OWNER := OPEN_RECORD ;
               end (* with *) ;
           end (* then *)
       end (* then *) ;

     /*****************************/
     /* statistikwerte ermitteln  */
     /*****************************/

     if OPT . GET_STAT then
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



function IS_STDTYPE ( TYP : TTP ; WHAT : CHAR ) : BOOLEAN ;

   begin (* IS_STDTYPE *)
     IS_STDTYPE := FALSE ;
     if TYP <> NIL then
       if TYP -> . FORM = SCALAR then
         if TYP -> . SCALKIND = STANDARD then
           IS_STDTYPE := ( TYP -> . WHATSTANDARD = WHAT ) ;
   end (* IS_STDTYPE *) ;



function GETIDLEN ( X : ALPHA ) : INTEGER ;

   var I : INTEGER ;

   begin (* GETIDLEN *)
     I := IDLNGTH ;
     while X [ I ] = ' ' do
       begin
         I := I - 1 ;
         if I <= 0 then
           break
       end (* while *) ;
     GETIDLEN := I ;
   end (* GETIDLEN *) ;



procedure GETBOUNDS ( FSP : TTP ; var FMIN , FMAX : INTEGER ) ;

(******************************************************)
(* GET INTERNAL BOUNDS OF SUBRANGE OR SCALAR TYPE     *)
(* ASSUME (FSP <> NIL) AND (FSP@.FORM <= SUBRANGE)    *)
(* AND (FSP <> ptype_int)                             *)
(* AND NOT COMPTYPES (ptype_real, FSP)                *)
(******************************************************)


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
           if FSP = PTYPE_CHAR then
             FMAX := ORDCHMAX
           else
             if ( FORM = SCALAR ) and ( FSP -> . FCONST <> NIL ) then
               FMAX := FSP -> . FCONST -> . VALUES . IVAL
             else
               FMAX := 0
         end (* else *)
   end (* GETBOUNDS *) ;



function GETTYPE ( OPERAND : TTP ) : INTEGER ;

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
             if IS_STDTYPE ( OPERAND , 'R' ) then// ptype_real
               GETTYPE := ORD ( 'R' )
             else
               if OPERAND = PTYPE_BOOL then
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



procedure CALC_SETSIZE ( SETMIN : INTEGER ; SETMAX : INTEGER ; var
                       SETMIN_NEW : INTEGER ; var SETSIZE : INTEGER ) ;

   var BITS_NEEDED : INTEGER ;
       X : INTEGER ;

   begin (* CALC_SETSIZE *)
     SETMIN := SETMIN - MODP ( SETMIN , 8 ) ;
     SETMIN_NEW := SETMIN ;
     BITS_NEEDED := SETMAX - SETMIN + 1 ;
     SETSIZE := ( BITS_NEEDED + 7 ) DIV 8 ;

     /*************************************************/
     /* uebergangsweise auf durch 4 teilbar aufrunden */
     /*************************************************/

     X := SETSIZE MOD 4 ;
     if X <> 0 then
       SETSIZE := SETSIZE + 4 - X ;
   end (* CALC_SETSIZE *) ;



procedure CALC_SETTYPSIZE ( ELT : TTP ; var SETSIZE : INTEGER ; var
                          SETMIN : INTEGER ; var SETMAX : INTEGER ; var
                          SETOFFS : INTEGER ) ;

(**************************************************)
(* COMPUTES SIZE OF SET WHOSE ELEMENT TYPE IS ELT *)
(*------------------------------------------------*)
(* 12.2016 - opp - size for char-based sets       *)
(* always 32 due to codepage issues               *)
(*------------------------------------------------*)
(* 06.2017 - opp - changed again for new          *)
(* representation of sets                         *)
(**************************************************)


   var MIN , MAX : INTEGER ;
       MIN_NEW : INTEGER ;

   begin (* CALC_SETTYPSIZE *)
     MIN := 0 ;
     MAX := - 1 ;
     if ELT <> NIL then
       begin
         if ELT = PTYPE_CHAR then
           MAX := ORDCHMAX
         else
           if ELT -> . FORM = SUBRANGE then
             if ELT -> . RANGETYPE = PTYPE_CHAR then
               MAX := ORDCHMAX ;
         if MAX = - 1 then
           GETBOUNDS ( ELT , MIN , MAX ) ;
       end (* then *) ;

     /****************************************************/
     /* CALC_SETSIZE ( MIN , MAX , MIN_NEW , SETSIZE ) ; */
     /* --- zunaechst min immer 0 ---------------------  */
     /* --- zunaechst setmin immer 0 ------------------  */
     /* --- zunaechst setsize netto, ohne prefix ------  */
     /****************************************************/

     CALC_SETSIZE ( 0 , MAX , MIN_NEW , SETSIZE ) ;
     SETMIN := 0 ;
     SETMAX := MAX ;
     SETOFFS := SETMIN - MODP ( SETMIN , 8 ) ;
     SETOFFS := SETOFFS DIV 8 ;
   end (* CALC_SETTYPSIZE *) ;



function OPNDSETSIZE ( FATTR : ATTR ) : INTEGER ;

(*************************************************)
(* COMPUTES THE SIZE OF A SET USED AS AN OPERAND *)
(* opp 12.2016: sets with base type char are     *)
(* always 32 bytes long - code page dependency   *)
(*************************************************)


   var SET_OF_CHAR : BOOLEAN ;

   begin (* OPNDSETSIZE *)
     OPNDSETSIZE := 0 ;
     with FATTR do
       if TYPTR <> NIL then
         if TYPTR -> . FORM = POWER then
           begin
             SET_OF_CHAR := FALSE ;
             if TYPTR -> . ELSET <> NIL then
               if TYPTR -> . ELSET = PTYPE_CHAR then
                 SET_OF_CHAR := TRUE
               else
                 if TYPTR -> . ELSET -> . FORM = SUBRANGE then
                   if TYPTR -> . ELSET -> . RANGETYPE = PTYPE_CHAR then
                     SET_OF_CHAR := TRUE ;
             if SET_OF_CHAR then
               OPNDSETSIZE := 32
             else
               case KIND of
                 CST : OPNDSETSIZE := CVAL . PVAL -> . LENGTH ;
                 VARBL : case ACCESS of
                           DRCT , INDRCT :
                             OPNDSETSIZE := TYPTR -> . SIZE ;
                           STKEXPR :
                             OPNDSETSIZE := STKLEN ;
                           INXD : ERROR ( 410 ) ;
                         end (* case *) ;
                 EXPR : ERROR ( 411 ) ;
               end (* case *)
           end (* then *)
   end (* OPNDSETSIZE *) ;



procedure GENLABEL ( var NXTLAB : LABELRNG ) ;

   begin (* GENLABEL *)
     INTLABEL := INTLABEL + 1 ;
     NXTLAB := INTLABEL
   end (* GENLABEL *) ;





(***********************************************************)
(*  THE FOLLOWING OUTPUTS A SYMBOL TABLE FILE              *)
(*  FOR USE BY 'SNAPSHOT' PROGRAM                          *)
(***********************************************************)




procedure PRNTSYMBL ( LCP : IDP ) ;

   var LINELN : INTEGER ;

       (*****************************************)
       (* CURRENT SYMBOL TABLE FILE LINE LENGTH *)
       (*****************************************)

       TPT1 : PRNTTYLISTP ;


   procedure CHECKLN ( LEN : INTEGER ) ;

      begin (* CHECKLN *)
        if ( LINELN + LEN ) >= 80 then
          begin
            WRITELN ( DBGINFO ) ;
            WRITE ( DBGINFO , ' ' ) ;
            LINELN := LEN
          end (* then *)
        else
          LINELN := LINELN + LEN
      end (* CHECKLN *) ;


   procedure PRNTVAR ( VRP : IDP ) ;

      FORWARD ;


   procedure PRNTTYPE ( TYPP : TTP ) ;

      label 1 ;

      var VP , LVP : IDP ;
          RMIN , RMAX : INTEGER ;
          TPT , LPT : PRNTTYLISTP ;
          TNO : 0 .. 999 ;

      begin (* PRNTTYPE *)
        CHECKLN ( 4 ) ;
        if TYPP = PTYPE_INT then
          WRITE ( DBGINFO , 'I4; ' )
        else
          if IS_STDTYPE ( TYPP , 'R' ) then
            WRITE ( DBGINFO , 'R; ' )
          else
            if TYPP = PTYPE_BOOL then
              WRITE ( DBGINFO , 'B; ' )
            else
              if TYPP = PTYPE_CHAR then
                WRITE ( DBGINFO , 'C; ' )
              else
                if TYPP <> NIL then
                  with TYPP -> do
                    case FORM of
                      SUBRANGE :
                        if RANGETYPE = PTYPE_CHAR then
                          WRITE ( DBGINFO , 'C; ' )
                        else
                          if RANGETYPE = PTYPE_INT then
                            WRITE ( DBGINFO , 'I' , SIZE : 1 , '; ' )
                          else
                            WRITE ( DBGINFO , 'L' , SIZE : 1 , '; ' ) ;
                      SCALAR :
                        WRITE ( DBGINFO , 'L' , SIZE : 1 , '; ' ) ;
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
                          WRITE ( DBGINFO , 'P ' , TNO : 1 , '; ' ) ;
                        end (* tag/ca *) ;
                      POWER : if ELSET <> NIL then
                                begin
                                  WRITE ( DBGINFO , 'S ' ) ;
                                  CHECKLN ( 10 ) ;
                                  GETBOUNDS ( ELSET , RMIN , RMAX ) ;
                                  WRITE ( DBGINFO , RMIN : 1 , ' ' ,
                                          RMAX : 1 , ' ; ' ) ;
                                end (* then *) ;
                      FILES : begin
                                WRITE ( DBGINFO , 'F ' ) ;
                                PRNTTYPE ( FILTYPE ) ;
                              end (* tag/ca *) ;
                      RECORDS :
                        begin
                          WRITE ( DBGINFO , 'D' , ALN : 1 , '(' ) ;
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
                                    WRITE ( DBGINFO , 'ETC=X' , RMAX :
                                            1 , '; ' ) ;
                                end (* then *) ;
                            end (* then *) ;
                          CHECKLN ( 3 ) ;
                          WRITE ( DBGINFO , '); ' ) ;
                        end (* tag/ca *) ;
                      ARRAYS :
                        if INXTYPE <> NIL then
                          begin
                            WRITE ( DBGINFO , 'A ' ) ;
                            CHECKLN ( 26 ) ;
                            GETBOUNDS ( INXTYPE , RMIN , RMAX ) ;
                            WRITE ( DBGINFO , RMIN : 1 , ' ' , RMAX : 1
                                    , ' ' ) ;
                            PRNTTYPE ( AELTYPE ) ;
                          end (* then *) ;
                    end (* case *)
                else
                  WRITE ( DBGINFO , ';' ) ;
      end (* PRNTTYPE *) ;


   procedure PRNTVAR ;

      var I : 0 .. IDLNGTH ;

      begin (* PRNTVAR *)
        with VRP -> do
          begin
            I := GETIDLEN ( NAME ) ;
            CHECKLN ( I + 1 ) ;
            WRITE ( DBGINFO , NAME : I , '=' ) ;
            PRNTTYPE ( IDTYPE ) ;
          end (* with *)
      end (* PRNTVAR *) ;


   begin (* PRNTSYMBL *)
     if OPT . PRCODE then
       if LCP <> NIL then
         with LCP -> do
           begin
             if KLASS = VARS then
               begin
                 LINELN := 5 ;
                 if VKIND = VARPARM then
                   begin
                     WRITE ( DBGINFO , '@ ' ) ;
                     LINELN := 7
                   end (* then *) ;
                 WRITE ( DBGINFO , VADDR : 1 , ' ' ) ;
                 if STKLASS = XAUTO then
                   WRITE ( DBGINFO , 'A' )
                 else
                   WRITE ( DBGINFO , 'S' ) ;
                 WRITE ( DBGINFO , ' ' ) ;
                 PRNTVAR ( LCP ) ;
               end (* then *)
             else
               if KLASS in [ PROC , FUNC ] then
                 begin
                   WRITELN ( DBGINFO , '% ' , NAME , ' ' , PFNAME ) ;
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
             WRITELN ( DBGINFO ) ;
           end (* with *)
       else

     (**************************************)
     (* DUMP HEAP STORAGE TYPE DEFINITIONS *)
     (**************************************)

         begin
           TPT1 := PRNTTYPHD ;
           while TPT1 <> NIL do
             begin
               WRITE ( DBGINFO , '>' , TPT1 -> . TNO : 1 , ' ' ) ;
               LINELN := 5 ;
               PRNTTYPE ( TPT1 -> . ELT ) ;
               WRITELN ( DBGINFO ) ;
               TPT1 := TPT1 -> . NXT ;
             end (* while *) ;
           PRNTTYPHD := NIL ;
           PRNTTYNO := 0 ;
         end (* else *) ;
   end (* PRNTSYMBL *) ;



procedure PROC_TO_STATNAME ( PROCNAME : EXTNAMTP ; EXTRN : BOOLEAN ;
                           var STATNAME : EXTNAMTP ) ;

(******************************************************)
(* builds static csect name from procedure csect name *)
(* by inserting some # chars somewhere                *)
(******************************************************)


   var LMAX : INTEGER ;

   begin (* PROC_TO_STATNAME *)
     STATNAME := PROCNAME ;
     LMAX := EXTNAMSZ ;
     if not EXTRN then
       LMAX := 4 ;
     repeat
       STATNAME [ LMAX ] := '#' ;
       LMAX := LMAX - 1
     until STATNAME [ LMAX ] <> ' ' ;
   end (* PROC_TO_STATNAME *) ;



procedure BLOCK ( FSYS : SYMSET ; FSY : SYMB ; FPROCP : IDP ) ;

   var LSY : SYMB ;
       TEST : BOOLEAN ;
       SEGSIZE : LABELRNG ;
       LCP , FWRDPRCL : IDP ;
       DEC_ORDER : 0 .. 5 ;
       PLOCAL : BOOLEAN ;
       STATIC_VORHANDEN : BOOLEAN ;
       FUNCTYPE : TTP ;
       STRING_RESULT : BOOLEAN ;


   procedure SKIP ( FSYS : SYMSET ) ;

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
          INTSIZE :
            Q := ( ( Q + 3 ) DIV 4 ) * 4 ;
          HINTSIZE :
            if ODD ( Q ) then
              Q := Q + 1 ;
          CHARSIZE :
            ;
          otherwise
            if ERRORCNT = 0 then
              ERROR ( 401 ) ;
        end (* case *) ;
      end (* ALIGN *) ;


   procedure GEN_STRCONST ( VAL : XCONSTANT ) ;

   (*****************************)
   (* generate string constant  *)
   (* into file prr             *)
   (*****************************)


      var I : INTEGER ;
          OUTPOS : INTEGER ;

      begin (* GEN_STRCONST *)
        with VAL , VAL . SVAL -> do
          begin
            WRITE ( PRR , LENGTH : 1 , ',' ) ;
            if STRTYPE in [ 'B' , 'X' ] then
              WRITE ( PRR , STRTYPE ) ;
            WRITE ( PRR , '''' ) ;
            OUTPOS := 4 ;
            I := 1 ;
            while I <= LENGTH do
              begin
                if OUTPOS >= 56 then
                  begin
                    WRITELN ( PRR , ''',' ) ;
                    WRITE ( PRR , '       ''' ) ;
                    OUTPOS := 0 ;
                  end (* then *) ;
                case STRTYPE of
                  'X' : begin
                          WRITEHEXBYTE ( PRR , ORD ( SSTR [ I ] ) ) ;
                          OUTPOS := OUTPOS + 2 ;
                        end (* tag/ca *) ;
                  'B' : begin
                          WRITEBINBYTE ( PRR , ORD ( SSTR [ I ] ) ) ;
                          OUTPOS := OUTPOS + 8 ;
                        end (* tag/ca *) ;
                  otherwise
                    begin
                      WRITE ( PRR , SSTR [ I ] : 1 ) ;
                      OUTPOS := OUTPOS + 1 ;
                      if SSTR [ I ] = '''' then
                        begin
                          WRITE ( PRR , '''' ) ;
                          OUTPOS := OUTPOS + 1
                        end (* then *) ;
                    end (* otherw *)
                end (* case *) ;
                I := I + 1
              end (* while *) ;
            WRITE ( PRR , '''' ) ;
          end (* with *)
      end (* GEN_STRCONST *) ;


   procedure MOD_STRCONST ( CT_RESULT : INTEGER ; var V : XCONSTANT ;
                          var TYP : TTP ; SIZE_NEU : INTEGER ) ;

   (************************************************************)
   (* modify string constant: add blanks up to size            *)
   (* change size to new size                                  *)
   (************************************************************)
   (* if no string (valp), but char of length 1,               *)
   (* change constant to string and adjust length              *)
   (* as in the other (simpler) case                           *)
   (************************************************************)
   (* if typ is given, modify it to char array type            *)
   (* (later charstring)                                       *)
   (************************************************************)


      var I : INTEGER ;
          CH : CHAR ;
          CP : CONSTP ;

      begin (* MOD_STRCONST *)
        case CT_RESULT of
          2 : begin
                with V . SVAL -> do
                  begin
                    for I := LENGTH + 1 to SIZE_NEU do
                      SSTR [ I ] := ' ' ;
                    LENGTH := SIZE_NEU ;
                  end (* with *)
              end (* tag/ca *) ;
          3 : begin
                if TYP = PTYPE_CHAR then
                  begin
                    NEW ( TYP , ARRAYS ) ;
                    with TYP -> do
                      begin
                        ERRORFLAG := FALSE ;
                        AELTYPE := PTYPE_CHAR ;
                        INXTYPE := NIL ;
                        SIZE := SIZE_NEU * CHARSIZE ;
                        FORM := ARRAYS ;
                        ALN := CHARSIZE ;
                      end (* with *)
                  end (* then *) ;
                CH := CHR ( V . IVAL ) ;
                NEW ( V . SVAL ) ;
                with V . SVAL -> do
                  begin
                    TAG := 'S' ;
                    SSTR := ' ' ;
                    SSTR [ 1 ] := CH ;
                    LENGTH := SIZE_NEU ;
                  end (* with *)
              end (* tag/ca *) ;
        end (* case *) ;
      end (* MOD_STRCONST *) ;


   procedure STRCONCAT ( var TVAL : XCONSTANT ; SVAL : XCONSTANT ; var
                       LT : INTEGER ; L : INTEGER ) ;

      var STRTYPE_NEW : CHAR ;
          CH : CHAR ;

          /*************************************************/
          /* schwierig, weil strings mit laenge = 0 oder 1 */
          /* anders abgelegt sind ... als einzelne chars   */
          /*************************************************/


      begin (* STRCONCAT *)

        /****************************/
        /* neuen strtype ausrechnen */
        /****************************/

        STRTYPE_NEW := TVAL . STRTYPE ;
        if SVAL . STRTYPE <> ' ' then
          STRTYPE_NEW := SVAL . STRTYPE ;

        /********************************************/
        /* wenn target nullstring, einfach zuweisen */
        /********************************************/

        if LT = 0 then
          begin
            TVAL := SVAL ;
            TVAL . STRTYPE := STRTYPE_NEW ;
            LT := L ;
            return ;
          end (* then *) ;

        /************************/
        /* neuen strtype setzen */
        /************************/

        TVAL . STRTYPE := STRTYPE_NEW ;

        /*****************************************/
        /* wenn lt = 1, evtl. auf string umbauen */
        /* nur, wenn etwas dazukommt             */
        /*****************************************/

        if LT = 1 then
          begin
            if L = 0 then
              return ;
            CH := CHR ( TVAL . IVAL ) ;
            NEW ( TVAL . SVAL ) ;
            TVAL . SVAL -> . TAG := 'S' ;
            TVAL . SVAL -> . SSTR [ 1 ] := CH ;
          end (* then *) ;

        /*****************************************/
        /* wenn ein char dazukommt, diesen aus   */
        /* ival holen, ansonsten memcpy          */
        /* und neue laengen setzen               */
        /*****************************************/

        if L = 1 then
          TVAL . SVAL -> . SSTR [ LT + 1 ] := CHR ( SVAL . IVAL )
        else
          MEMCPY ( ADDR ( TVAL . SVAL -> . SSTR [ LT + 1 ] ) , ADDR (
                   SVAL . SVAL -> . SSTR [ 1 ] ) , L ) ;
        TVAL . SVAL -> . LENGTH := LT + L ;
        LT := TVAL . SVAL -> . LENGTH ;
      end (* STRCONCAT *) ;


   procedure MODIFY_TYPE_PARMS ( var FSP : TTP ; ID : ALPHA ; PARAM1 :
                               INTEGER ; PARAM2 : INTEGER ) ;

   //******************************************************
   // build new type from existing type and params         
   //******************************************************


      var TYPENEW : ALPHA ;
          I : INTEGER ;
          CP : -> CHAR ;
          NEWIDP : IDP ;

      begin (* MODIFY_TYPE_PARMS *)
        with FSP -> do
          begin

        //************************************************
        // no action needed, if default is ok             
        // and minparamcount = 1 (e.g. CHAR (1)).         
        //************************************************

            if FORM = SCALAR then
              if ( MINPARAMCOUNT = 0 ) and ( DEFAULTPARAM = PARAM1 )
              then
                return ;
            if FALSE then
              begin
                WRITELN ( TRACEF , 'modify_type_parms' ) ;
                WRITELN ( TRACEF , 'fsp.form          = ' , FORM ) ;
                WRITELN ( TRACEF , 'fsp.scalkind      = ' , SCALKIND )
                          ;
                WRITELN ( TRACEF , 'fsp.minparamcount = ' ,
                          MINPARAMCOUNT ) ;
                WRITELN ( TRACEF , 'fsp.defaultparam  = ' ,
                          DEFAULTPARAM ) ;
                WRITELN ( TRACEF , 'id                = ' , ID ) ;
                WRITELN ( TRACEF , 'param1            = ' , PARAM1 ) ;
                WRITELN ( TRACEF , 'param2            = ' , PARAM2 ) ;
              end (* then *) ;

        //************************************************
        // construct name of new type in variable typenew 
        //************************************************

            TYPENEW := '*' ;
            CP := ADDR ( TYPENEW ) ;
            CP := PTRADD ( CP , 1 ) ;
            MEMCPY ( CP , ADDR ( ID ) , SIZEOF ( ALPHA ) - 1 ) ;
            for I := 1 to SIZEOF ( ALPHA ) do
              if TYPENEW [ I ] = ' ' then
                break ;
            CP := ADDR ( TYPENEW [ I ] ) ;
            CP -> := '_' ;
            CP := PTRADD ( CP , 1 ) ;
            INTTOSTR ( CP , 5 , PARAM1 , TRUE ) ;
            CP := PTRADD ( CP , 5 ) ;
            CP -> := '_' ;
            CP := PTRADD ( CP , 1 ) ;
            INTTOSTR ( CP , 3 , PARAM2 , TRUE ) ;

        //************************************************
        // look, if type is already defined;              
        // if not (sid_rc = 104), insert it into type list
        //************************************************

            SID_RC := SEARCHID ( TYPENEW , FALSE , TRUE , [ TYPES ] ,
                      NEWIDP ) ;
            with NEWIDP -> do
              begin
                if FALSE then
                  begin
                    WRITELN ( TRACEF , 'typenew           = ' , TYPENEW
                              ) ;
                    WRITELN ( TRACEF , 'sid_rc            = ' , SID_RC
                              ) ;
                    WRITELN ( TRACEF , 'name              = ' , NAME )
                              ;
                    WRITELN ( TRACEF , 'decl_lev          = ' ,
                              DECL_LEV ) ;
                    WRITELN ( TRACEF , 'idtype            = ' , IDTYPE
                              ) ;
                  end (* then *) ;
                if SID_RC = 104 then
                  begin
                    DECL_LEV := 1 ;
                    if FSP = PTYPE_CHAR then
                      begin

        //************************************************
        // type similar to alfa, but different length     
        //************************************************

                        NEW ( IDTYPE ) ;
                        IDTYPE -> := PTYPE_ALFA -> ;
                        with IDTYPE -> do
                          begin
                            SIZE := PARAM1 ;
                            NEW ( INXTYPE ) ;
                            INXTYPE -> := PTYPE_ALFA -> . INXTYPE -> ;
                            INXTYPE -> . MAX . IVAL := PARAM1
                          end (* with *) ;
                      end (* then *)
                    else
                      if FSP = PTYPE_VARCHAR then
                        begin

        //************************************************
        // type similar to string                         
        // length must be set ... defined length plus 4   
        // 2 for maxlength field, 2 for actual length     
        //************************************************

                          NEW ( IDTYPE ) ;
                          IDTYPE -> := PTYPE_VARCHAR -> ;
                          with IDTYPE -> do
                            SIZE := PARAM1 + 4 ;
                        end (* then *)
                      else
                        if FSP = PTYPE_DECIMAL then
                          begin
                            NEW ( IDTYPE ) ;

        //************************************************
        // type similar to real at the moment             
        // but: store parameters to control output width  
        //************************************************

                            IDTYPE -> := PTYPE_REAL -> ;
                            IDTYPE -> . STDPARM1 := PARAM1 ;
                            IDTYPE -> . STDPARM2 := PARAM2 ;
                          end (* then *) ;
                  end (* then *) ;
                FSP := IDTYPE
              end (* with *)
          end (* with *) ;
        with FSP -> do
          if FALSE then
            begin
              WRITELN ( TRACEF , 'modify_type_parms' ) ;
              WRITELN ( TRACEF , 'loc               = ' , LINECNT ) ;
              WRITELN ( TRACEF , 'typenew           = ' , TYPENEW ) ;
              WRITELN ( TRACEF , 'fsp.form          = ' , FORM ) ;
            end (* then *) ;
      end (* MODIFY_TYPE_PARMS *) ;


   procedure CONSTANT ( FSYS : SYMSET ; var FSP : TTP ; var FVALU :
                      XCONSTANT ) ;

      var LSP : TTP ;
          LCP : IDP ;
          SIGN : ( NONE , POS , NEG ) ;
          LVP : CONSTP ;
          I : 2 .. REALLNGTH ;
          CONST_ERR : INTEGER ;
          LSTRING : INTEGER ;

      begin (* CONSTANT *)
        LSP := NIL ;
        FVALU . IVAL := 0 ;
        FVALU . STRTYPE := ' ' ;
        LSTRING := 0 ;
        if not ( SY in CONSTBEGSYS ) then
          begin
            ERROR ( 50 ) ;
            SKIP ( FSYS + CONSTBEGSYS )
          end (* then *) ;
        if SY in CONSTBEGSYS then
          begin
            SIGN := NONE ;
            CONST_ERR := 0 ;

        (***********************************************)
        (* loop, because                               *)
        (* - a sign may be followed by an intconst     *)
        (*   or realconst (or ident)                   *)
        (* - more than on stringconsts may be          *)
        (*   concatenated                              *)
        (***********************************************)

            while TRUE do
              begin
                case SY of

        (***********************************************)
        (* another stringconst may follow,             *)
        (* if not, break                               *)
        (***********************************************)

                  STRINGCONST :
                    begin
                      if LSTRING + SYLENGTH > MAXSTRL then
                        ERROR ( 395 )
                      else
                        begin
                          STRCONCAT ( FVALU , VAL , LSTRING , SYLENGTH
                                      ) ;
                          if FALSE then
                            begin
                              WRITE ( TRACEF , 'nach strconcat: ' ,
                                      FVALU . STRTYPE , ' ' , LSTRING :
                                      1 , ' <' ) ;
                              if LSTRING > 1 then
                                for I := 1 to LSTRING do
                                  if FVALU . STRTYPE = 'C' then
                                    WRITE ( TRACEF , FVALU . SVAL -> .
                                            SSTR [ I ] )
                                  else
                                    WRITEHEXBYTE ( TRACEF , ORD ( FVALU
                                                   . SVAL -> . SSTR [ I
                                                   ] ) ) ;
                              WRITELN ( TRACEF , '>' ) ;
                            end (* then *)
                        end (* else *) ;
                      INSYMBOL ;
                      if SY = STRINGCONST then
                        continue ;
                      if LSTRING > 1 then
                        begin
                          NEW ( LSP , ARRAYS ) ;
                          with LSP -> do
                            begin
                              ERRORFLAG := FALSE ;
                              AELTYPE := PTYPE_CHAR ;
                              INXTYPE := NIL ;
                              SIZE := LSTRING ;
                              FORM := ARRAYS ;
                              ALN := CHARSIZE ;
                            end (* with *)
                        end (* then *)
                      else
                        LSP := PTYPE_CHAR ;
                      break ;
                    end (* tag/ca *) ;

        (***********************************************)
        (* if there was a sign, the id must have       *)
        (* an appropriate type                         *)
        (***********************************************)

                  IDENT : begin
                            SID_RC := SEARCHID ( ID , FALSE , FALSE , [
                                      KONST ] , LCP ) ;
                            case SID_RC of
                              0 : ;
                              104 : begin
                                      ERROR ( SID_RC ) ;
                                      SID_RC := SEARCHID ( ID , FALSE ,
                                                TRUE , [ KONST ] , LCP
                                                )
                                    end (* tag/ca *) ;
                              otherwise
                                begin
                                  ERROR ( SID_RC ) ;
                                end (* otherw *)
                            end (* case *) ;
                            with LCP -> do
                              begin
                                LSP := IDTYPE ;
                                FVALU := VALUES
                              end (* with *) ;
                            if SIGN <> NONE then
                              if LSP = PTYPE_INT then
                                begin
                                  if SIGN = NEG then
                                    FVALU . IVAL := - FVALU . IVAL ;
                                end (* then *)
                              else
                                if IS_STDTYPE ( LSP , 'R' ) then
                                  begin
                                    if SIGN = NEG then
                                      begin
                                        if FVALU . RVAL [ 1 ] = '-'
                                        then
                                          FVALU . RVAL [ 1 ] := '+'
                                        else
                                          FVALU . RVAL [ 1 ] := '-' ;
                                      end (* then *)
                                  end (* then *)
                                else
                                  CONST_ERR := 105 ;
                            INSYMBOL ;
                            break ;
                          end (* tag/ca *) ;

        (***********************************************)
        (* signs may occur only once                   *)
        (***********************************************)

                  SYPLUS , SYMINUS :
                    begin
                      if SIGN <> NONE then
                        CONST_ERR := 106
                      else
                        begin
                          if SY = SYPLUS then
                            SIGN := POS
                          else
                            SIGN := NEG ;
                          INSYMBOL ;
                          continue ;
                        end (* else *)
                    end (* tag/ca *) ;

        (***********************************************)
        (* work on intconst, then break                *)
        (***********************************************)

                  INTCONST :
                    begin
                      if SIGN = NEG then
                        VAL . IVAL := - VAL . IVAL ;
                      LSP := PTYPE_INT ;
                      FVALU := VAL ;
                      INSYMBOL ;
                      break ;
                    end (* tag/ca *) ;

        (***********************************************)
        (* work on realconst, then break               *)
        (***********************************************)

                  REALCONST :
                    begin
                      if SIGN = NEG then
                        VAL . RVAL [ 1 ] := '-' ;
                      if SYDIGITS > 0 then
                        begin
                          LSP := PTYPE_DECIMAL ;
                          MODIFY_TYPE_PARMS ( LSP , 'DECIMAL' ,
                                              SYDIGITS , SYPREC ) ;
                        end (* then *)
                      else
                        LSP := PTYPE_REAL ;
                      FVALU := VAL ;
                      INSYMBOL ;
                      break ;
                    end (* tag/ca *) ;
                  otherwise
                    CONST_ERR := 50
                end (* case *) ;
                if CONST_ERR <> 0 then
                  break ;
              end (* while *) ;
            if CONST_ERR <> 0 then
              begin
                ERROR ( CONST_ERR ) ;
                SKIP ( FSYS )
              end (* then *) ;
          end (* then *) ;

        (***********************************************)
        (* return fsp                                  *)
        (***********************************************)

        FSP := LSP
      end (* CONSTANT *) ;


   function COMPTYPES ( FSP1 , FSP2 : TTP ) : INTEGER ;

   (***********************************************************)
   (* DECIDE WHETHER STRUCTURES POINTED AT BY FSP1 AND FSP2   *)
   (* ARE COMPATIBLE                                          *)
   (*---------------------------------------------------------*)
   (* 09.2016 - bernd oppolzer                                *)
   (* function result type changed from boolean to integer    *)
   (* 0: not compatible                                       *)
   (* 1: compatible                                           *)
   (* 2: char arrays with different lengths, size1 > size2    *)
   (* 3: fsp1 = char array, fsp2 = single char                *)
   (*---------------------------------------------------------*)
   (* 12.2017 - bernd oppolzer                                *)
   (* more results (4,5,6) when form = cstring is involved    *)
   (***********************************************************)


      var NXT1 , NXT2 : IDP ;
          COMP : INTEGER ;
          LTESTP1 , LTESTP2 : TESTP ;

      begin (* COMPTYPES *)
        COMPTYPES := 0 ;

        //******************************************************
        // comptypes ok, wenn typen gleich sind (gleiches       
        // typ-objekt)                                          
        //******************************************************

        if FSP1 = FSP2 then
          begin
            if FALSE then
              WRITELN ( TRACEF , 'comptypes: fsp equal' ) ;
            COMPTYPES := 1 ;
            return
          end (* then *) ;

        //******************************************************
        // comptypes ok, wenn einer der beiden typen nil ist    
        // zur vermeidung von folgefehlern                      
        //******************************************************

        if ( FSP1 = NIL ) or ( FSP2 = NIL ) then
          begin
            COMPTYPES := 1 ;
            if FALSE then
              WRITELN ( TRACEF , 'comptypes: fsp both nil' ) ;
            return
          end (* then *) ;

        //******************************************************
        // jetzt kommen die normalfaelle, wenn die typklasse    
        // dieselbe ist                                         
        //******************************************************

        if FSP1 -> . FORM = FSP2 -> . FORM then
          begin
            if FALSE then
              begin
                WRITELN ( TRACEF , 'comptypes: fsp forms equal: ' ,
                          FSP1 -> . FORM ) ;
              end (* then *) ;
            case FSP1 -> . FORM of

        //******************************************************
        // standard types with same 'whatstandard' flag are     
        // compatible - 01.2018                                 
        // *****************************************************
        // limit to real for the moment - to support decimal ...
        // *****************************************************
        // other different scalar types are not                 
        //******************************************************

              SCALAR :
                if ( FSP1 -> . SCALKIND = STANDARD ) and ( FSP2 -> .
                SCALKIND = STANDARD ) and ( FSP1 -> . WHATSTANDARD =
                FSP2 -> . WHATSTANDARD ) and ( FSP1 -> . WHATSTANDARD =
                'R' ) then
                  COMPTYPES := 1
                else
                  COMPTYPES := 0 ;
              SUBRANGE :
                COMPTYPES := COMPTYPES ( FSP1 -> . RANGETYPE , FSP2 ->
                             . RANGETYPE ) ;
              POINTER :
                begin
                  COMP := 0 ;
                  LTESTP1 := GLOBTESTP ;
                  LTESTP2 := GLOBTESTP ;
                  while LTESTP1 <> NIL do
                    with LTESTP1 -> do
                      begin
                        if ( ELT1 = FSP1 -> . ELTYPE ) and ( ELT2 =
                        FSP2 -> . ELTYPE ) then
                          COMP := 1 ;
                        LTESTP1 := LASTTESTP
                      end (* with *) ;
                  if COMP = 0 then
                    begin
                      NEW ( LTESTP1 ) ;
                      with LTESTP1 -> do
                        begin
                          ELT1 := FSP1 -> . ELTYPE ;
                          ELT2 := FSP2 -> . ELTYPE ;
                          LASTTESTP := GLOBTESTP
                        end (* with *) ;
                      GLOBTESTP := LTESTP1 ;
                      COMP := COMPTYPES ( FSP1 -> . ELTYPE , FSP2 -> .
                              ELTYPE )
                    end (* then *) ;
                  COMPTYPES := COMP ;
                  GLOBTESTP := LTESTP2
                end (* tag/ca *) ;
              POWER : COMPTYPES := COMPTYPES ( FSP1 -> . ELSET , FSP2
                                   -> . ELSET ) ;

        //******************************************************
        // cstrings are compatible,                             
        // no matter what the length is                         
        //******************************************************

              CSTRING :
                COMPTYPES := 1 ;

        //******************************************************
        // hier besonderheit und modifikation:                  
        // char arrays mit unterschiedlicher laenge             
        // ergeben comptypes = 2 (wenn zweiter laenger als 1.)  
        //******************************************************

              ARRAYS :
                begin
                  if FALSE then
                    WRITELN ( TRACEF , 'comptypes: fsp arrays' ) ;
                  COMP := 0 ;
                  if ( COMPTYPES ( FSP1 -> . AELTYPE , FSP2 -> .
                  AELTYPE ) > 0 ) and ( FSP1 -> . SIZE = FSP2 -> . SIZE
                  ) then
                    COMP := 1 ;
                  if FALSE then
                    WRITELN ( TRACEF , 'comptypes: fsp comp = ' , COMP
                              ) ;
                  if COMP = 0 then
                    if ( FSP1 -> . AELTYPE = PTYPE_CHAR ) and ( FSP2 ->
                    . AELTYPE = PTYPE_CHAR ) and ( FSP1 -> . SIZE >
                    FSP2 -> . SIZE ) and ( FSP1 -> . SIZE <= MAXSTRL )
                    then
                      COMP := 2 ;
                  if FALSE then
                    WRITELN ( TRACEF , 'comptypes: fsp comp 2 = ' ,
                              COMP ) ;
                  COMPTYPES := COMP ;
                end (* tag/ca *) ;

        //******************************************************
        // ALTERNATIVES:                                        
        // -- ADD A THIRD BOOLEAN TERM:                         
        //    INDEXTYPE MUST BE COMPATIBLE.                     
        // -- ADD A FOURTH BOOLEAN TERM:                        
        //    LOWBOUNDS MUST BE THE SAME                        
        //******************************************************

              RECORDS :
                begin
                  COMP := 0 ;
                  NXT1 := FSP1 -> . FSTFLD ;
                  NXT2 := FSP2 -> . FSTFLD ;
                  if ( FSP1 -> . RECVAR = FSP2 -> . RECVAR ) then
                    COMP := 1 ;
                  while ( COMP = 1 ) and ( NXT1 <> NIL ) and ( NXT2 <>
                  NIL ) do
                    begin
                      if COMPTYPES ( NXT1 -> . IDTYPE , NXT2 -> .
                      IDTYPE ) = 0 then
                        COMP := 0 ;
                      if NXT1 -> . IDTYPE -> . SIZE <> NXT2 -> . IDTYPE
                      -> . SIZE then
                        COMP := 0 ;
                      NXT1 := NXT1 -> . NEXT ;
                      NXT2 := NXT2 -> . NEXT
                    end (* while *) ;
                  if ( COMP = 1 ) and ( NXT1 = NIL ) and ( NXT2 = NIL )
                  then
                    COMPTYPES := 1
                  else
                    COMPTYPES := 0
                end (* tag/ca *) ;

        //******************************************************
        // IDENTICAL RECORDS ARE RECOGNIZED TO BE COMPATIBLE    
        // IFF NO VARIANTS OCCUR                                
        //******************************************************

              FILES : COMPTYPES := COMPTYPES ( FSP1 -> . FILTYPE , FSP2
                                   -> . FILTYPE )
            end (* case *) ;
            return
          end (* then *) ;

        //******************************************************
        // jetzt kommen die faelle, wo die typklasse nicht      
        // uebereinstimmt                                       
        // FSP1 -> . FORM <> FSP2 -> . FORM                     
        //******************************************************

        if FSP1 -> . FORM = SUBRANGE then
          begin
            COMPTYPES := COMPTYPES ( FSP1 -> . RANGETYPE , FSP2 ) ;
            return
          end (* then *) ;
        if FSP2 -> . FORM = SUBRANGE then
          begin
            COMPTYPES := COMPTYPES ( FSP1 , FSP2 -> . RANGETYPE ) ;
            return
          end (* then *) ;

        //******************************************************
        // neu: korrekt ist auch, wenn links ein char-array     
        // steht und rechts ein einzelner char                  
        // dann wird comptypes = 3 zurueckgegeben               
        //******************************************************

        if ( FSP1 -> . FORM = ARRAYS ) and ( FSP1 -> . AELTYPE =
        PTYPE_CHAR ) and ( FSP2 = PTYPE_CHAR ) and ( FSP1 -> . SIZE <=
        MAXSTRL ) then
          begin
            COMPTYPES := 3 ;
            return
          end (* then *) ;

        //******************************************************
        // new in 12.2017:                                      
        // comptypes = 4, when left operand is cstring          
        // and right operand is single char                     
        //******************************************************
        // is only ok for constants, not for variables          
        // if variables, show error 341 (use STR function)      
        //******************************************************

        if ( FSP1 -> . FORM = CSTRING ) and ( FSP2 = PTYPE_CHAR ) then
          begin
            COMPTYPES := 4 ;
            return
          end (* then *) ;

        //******************************************************
        // new in 12.2017:                                      
        // comptypes = 5, when left operand is cstring          
        // and right operand is array of chars                  
        //******************************************************
        // is only ok for constants, not for variables          
        // if variables, show error 341 (use STR function)      
        //******************************************************

        if ( FSP1 -> . FORM = CSTRING ) and ( FSP2 -> . FORM = ARRAYS )
        and ( FSP2 -> . AELTYPE = PTYPE_CHAR ) and ( FSP2 -> . SIZE <=
        MAXSTRL ) then
          begin
            if FSP2 -> . SIZE <= FSP1 -> . SIZE then
              begin
                COMPTYPES := 5 ;
                return
              end (* then *) ;
            if FSP1 -> . CONFORMANT then
              begin
                COMPTYPES := 7 ;
                return
              end (* then *) ;
          end (* then *) ;

        //******************************************************
        // new in 12.2017:                                      
        // comptypes = 6, when left operand is array of chars   
        // and right operand is cstring                         
        //******************************************************
        // is ok even if defined maxlength of string is longer  
        // than length of char array; run time error is thrown  
        // depending on actual length                           
        //******************************************************

        if ( FSP2 -> . FORM = CSTRING ) and ( FSP1 -> . FORM = ARRAYS )
        and ( FSP1 -> . AELTYPE = PTYPE_CHAR ) and ( FSP1 -> . SIZE <=
        MAXSTRL ) then
          begin
            COMPTYPES := 6 ;
            return
          end (* then *) ;

        //******************************************************
        // no success, comptypes = 0                            
        //******************************************************

        COMPTYPES := 0
      end (* COMPTYPES *) ;


   function IS_CARRAY ( FSP : TTP ) : BOOLEAN ;

      begin (* IS_CARRAY *)
        IS_CARRAY := FALSE ;
        if FSP <> NIL then
          begin
            if FSP -> . FORM = ARRAYS then
              IS_CARRAY := ( COMPTYPES ( FSP -> . AELTYPE , PTYPE_CHAR
                           ) = 1 )
          end (* then *)
      end (* IS_CARRAY *) ;


   procedure PUTIC ;

      begin (* PUTIC *)
        if LINECNT > OLDLN then
          begin
            WRITELN ( PRR , ' LOC ' , LINECNT : 1 ) ;
            OLDLN := LINECNT
          end (* then *) ;
      end (* PUTIC *) ;


   procedure CHECKSTARTCST ;

      var I : INTEGER ;
          CSTEXTNAME : EXTNAMTP ;

      begin (* CHECKSTARTCST *)
        if CONSTLCOUNTER <= 0 then

        (****************************)
        (* NO CONSTANTS WRITTEN YET *)
        (****************************)

          with FPROCP -> do
            begin
              CSTEXTNAME := FPROCP -> . CSTNAME ;

        (********)
        (*CST   *)
        (********)

              WRITELN ( PRR , CSTEXTNAME , MN [ 71 ] , ' ' , NAME ,
                        PFNAME : 5 , ',' , OPT . ASSEMBLE : 1 , ',' ,
                        OPT . GET_STAT : 1 , ',' , OPT . ASMVERB : 1 )
                        ;
              CONSTLCOUNTER := FIRSTCONSTLC ;
            end (* with *) ;
      end (* CHECKSTARTCST *) ;


   procedure BUILD_SETCONST ( var SETVAL : XCONSTANT ; PSI : PSETINFO ;
                            ELTYP : TTP ) ;

      var LVP : SSP ;
          SETMIN_NEW : INTEGER ;
          SETMAX_NEW : INTEGER ;
          SETMIN_DUMMY : INTEGER ;
          SETMAX_DUMMY : INTEGER ;
          SETOFFS_DUMMY : INTEGER ;
          SETSIZE : INTEGER ;
          I : INTEGER ;
          TX : INTEGER ;
          W : INTEGER ;

      begin (* BUILD_SETCONST *)
        NEW ( SETVAL . PVAL ) ;
        LVP := SETVAL . PVAL ;
        for I := 1 to MAXSETL do
          LVP -> . PSTR [ I ] := CHR ( 0 ) ;
        SETVAL . STRTYPE := PSI -> . HEXORBIN ;

        /*****************************************/
        /*   CALC_SETSIZE ( PSI -> . SETMIN ,    */
        /*                  PSI -> . SETMAX ,    */
        /*                  SETMIN_NEW ,         */
        /*                  SETSIZE ) ;          */
        /*   --- zunaechst setmin = 0 --------   */
        /*****************************************/

        if PSI -> . ELEMCOUNT = 0 then
          begin
            SETVAL . SETMIN := 0 ;
            SETVAL . SETMAX := - 1 ;
            SETVAL . SETOFFS := 0 ;
            LVP -> . TAG := 'P' ;
            LVP -> . LENGTH := 0 ;
          end (* then *)
        else
          begin
            if ELTYP <> NIL then
              begin
                CALC_SETTYPSIZE ( ELTYP , SETSIZE , SETMIN_DUMMY ,
                                  SETMAX_DUMMY , SETOFFS_DUMMY ) ;
              end (* then *)
            else
              begin
                CALC_SETSIZE ( 0 , PSI -> . SETMAX , SETMIN_DUMMY ,
                               SETSIZE )
              end (* else *) ;
            SETMIN_NEW := 0 ;
            SETMAX_NEW := PSI -> . SETMAX - MODP ( PSI -> . SETMAX , 8
                          ) + 7 ;
            if ELTYP = PTYPE_CHAR then
              SETMAX_NEW := ORDCHMAX ;
            if FALSE then
              begin
                WRITELN ( TRACEF ) ;
                WRITELN ( TRACEF , 'start build_set' ) ;
                WRITELN ( TRACEF , 'psi.setmin    = ' , PSI -> . SETMIN
                          ) ;
                WRITELN ( TRACEF , 'psi.setmax    = ' , PSI -> . SETMAX
                          ) ;
                WRITELN ( TRACEF , 'setmin_new    = ' , SETMIN_NEW ) ;
                WRITELN ( TRACEF , 'setmax_new    = ' , SETMAX_NEW ) ;
                WRITELN ( TRACEF , 'setsize       = ' , SETSIZE ) ;
              end (* then *) ;
            TX := 0 ;
            W := 0 ;
            for I := SETMIN_NEW to SETMAX_NEW do
              begin
                if I MOD 8 = 0 then
                  begin
                    if TX > 0 then
                      begin
                        if FALSE then
                          WRITELN ( TRACEF , 'pval ' , TX : 1 ,
                                    ' = chr ' , W : 1 ) ;
                        LVP -> . PSTR [ TX ] := CHR ( W ) ;
                      end (* then *) ;
                    TX := TX + 1 ;
                    W := 0 ;
                  end (* then *) ;
                W := W * 2 ;
                if ( I >= PSI -> . SETMIN ) and ( I <= PSI -> . SETMAX
                ) then
                  if ( PSI -> . SETELEMS [ I - PSI -> . SETMIN + 1 ] )
                  then
                    W := W + 1 ;
              end (* for *) ;
            if TX > 0 then
              begin
                if FALSE then
                  WRITELN ( TRACEF , 'pstr ' , TX : 1 , ' = chr ' , W :
                            1 ) ;
                LVP -> . PSTR [ TX ] := CHR ( W ) ;
              end (* then *) ;
            if FALSE then
              WRITELN ( TRACEF ) ;
            SETVAL . SETMIN := 0 ;
            SETVAL . SETMAX := PSI -> . SETMAX ;

        /******************************************/
        /* SETVAL . SETOFFS := SETMIN_NEW DIV 8 ; */
        /* --- zunaechst immer 0                  */
        /******************************************/

            SETVAL . SETOFFS := 0 ;
            LVP -> . TAG := 'P' ;
            LVP -> . LENGTH := SETSIZE ;
          end (* else *)
      end (* BUILD_SETCONST *) ;


   procedure WRITESET ( VAL : XCONSTANT ; ELTYPE : TTP ) ;

      var I , W , X , COL , LEN : INTEGER ;
          S : SETSTRING ;
          CW : INTEGER ;
          CH : CHAR ;
          HEXORBIN : BOOLEAN ;

      begin (* WRITESET *)
        LEN := VAL . PVAL -> . LENGTH ;
        S := VAL . PVAL -> . PSTR ;
        HEXORBIN := not ( VAL . STRTYPE in [ ' ' , 'N' ] ) ;

        (***********************)
        (* empty = leere menge *)
        (***********************)

        if LEN = 0 then
          WRITELN ( PRR , 'E()' )
        else
          if ( ELTYPE = PTYPE_CHAR ) and not HEXORBIN then

        (***************************************************)
        (* menge mit basistyp = char oder subrange davon   *)
        (* dann zeichen ausgeben - portable darstellung    *)
        (***************************************************)
        (* immer laenge 32 - anderen orts wird dafuer      *)
        (* gesorgt, dass sets of char immer 32 bytes lang  *)
        (* sind - siehe function opndsetsize               *)
        (***************************************************)
        (* 06.2017 - modifikation                          *)
        (* die sets werden intern anders abgelegt ...      *)
        (* setoffs gibt an, wo die interne darstellung     *)
        (* beginnt (mit wieviel bytes versatz)             *)
        (* setsize gibt die anzahl belegte bytes an        *)
        (* - plus 4 fuer die metadaten                     *)
        (***************************************************)

            begin
              WRITE ( PRR , 'C32''' ) ;
              CW := 0 ;
              COL := 11 ;
              for I := 1 to LEN do
                begin
                  W := ORD ( S [ I ] ) ;
                  X := 128 ;
                  while X > 0 do
                    begin
                      if ( W and X ) <> 0 then
                        begin
                          CH := CHR ( CW ) ;
                          if COL >= 65 then
                            begin
                              WRITELN ( PRR , ''',' ) ;
                              COL := 7 ;
                              WRITE ( PRR , '     ''' ) ;
                            end (* then *) ;
                          WRITE ( PRR , CH ) ;
                          COL := COL + 1 ;

        /*********************/
        /* hochkomma doppelt */
        /*********************/

                          if CH = '''' then
                            begin
                              WRITE ( PRR , CH ) ;
                              COL := COL + 1 ;
                            end (* then *) ;
                        end (* then *) ;
                      X := X DIV 2 ;
                      CW := CW + 1 ;
                    end (* while *) ;
                end (* for *) ;
              WRITELN ( PRR , '''' )
            end (* then *)
          else

        (***************************************************)
        (* menge mit anderen Basistypen,                   *)
        (* dann menge als bitstring in hex-darstellung     *)
        (* achtung: immer gerade anzahl pro zeile !!       *)
        (***************************************************)

            begin
              WRITE ( PRR , 'X' , LEN : 1 , '''' ) ;
              COL := 19 ;
              for I := 1 to LEN do
                begin
                  if COL >= 65 then
                    begin
                      WRITELN ( PRR , ''',' ) ;
                      COL := 7 ;
                      WRITE ( PRR , '     ''' ) ;
                    end (* then *) ;
                  WRITEHEXBYTE ( PRR , ORD ( S [ I ] ) ) ;
                  COL := COL + 2 ;
                end (* for *) ;
              WRITELN ( PRR , '''' )
            end (* else *) ;
      end (* WRITESET *) ;


   procedure WRITEDFC ( ELSP1 : TTP ; ELSIZE : INTEGER ; LVALU :
                      XCONSTANT ; INIT : BOOLEAN ) ;

      var CH : CHAR ;

      begin (* WRITEDFC *)
        PUTIC ;
        WRITE ( PRR , CONSTLCOUNTER : 1 , MN [ 70 ] ) ;

        (********)
        (*DFC   *)
        (********)

        if INIT then
          begin
            if IS_STDTYPE ( ELSP1 , 'R' ) then
              WRITELN ( PRR , ' R,' , 0.0 )
            else
              if ELSP1 -> . FORM <= SUBRANGE then
                begin
                  CH := 'I' ;
                  if ELSIZE = 2 then
                    CH := 'H'
                  else
                    if ELSIZE = 1 then
                      if ELSP1 = PTYPE_CHAR then
                        CH := 'C'
                      else
                        CH := 'B' ;
                  if CH = 'C' then
                    WRITELN ( PRR , CH : 2 , ','' ''' )
                  else
                    WRITELN ( PRR , CH : 2 , ',' , 0 : 1 )
                end (* then *)
              else
                if ELSP1 -> . FORM = POINTER then
                  WRITELN ( PRR , ' N' )
                else
                  if IS_CARRAY ( ELSP1 ) then
                    begin
                      WRITE ( PRR , ' M,' ) ;
                      WRITE ( PRR , ELSIZE : 1 ) ;
                      WRITE ( PRR , ','' ''' ) ;
                      WRITELN ( PRR ) ;
                    end (* then *)
                  else
                    begin
                      WRITE ( PRR , ' 0,' , ELSIZE : 1 ) ;
                      WRITELN ( PRR ) ;
                    end (* else *)
          end (* then *)
        else
          begin
            if ELSP1 = NIL then
              return ;
            if IS_STDTYPE ( ELSP1 , 'R' ) then
              begin
                WRITELN ( PRR , ' R,' , LVALU . RVAL ) ;
                return
              end (* then *) ;
            if ELSP1 -> . FORM <= SUBRANGE then
              begin
                CH := 'I' ;
                if ELSIZE = 2 then
                  CH := 'H'
                else
                  if ELSIZE = 1 then
                    if ELSP1 = PTYPE_CHAR then
                      if LVALU . STRTYPE in [ 'X' , 'B' ] then
                        CH := 'B'
                      else
                        CH := 'C'
                    else
                      CH := 'B' ;
                if CH = 'C' then
                  WRITELN ( PRR , CH : 2 , ',' , '''' , CHR ( LVALU .
                            IVAL ) , '''' )
                else
                  WRITELN ( PRR , CH : 2 , ',' , LVALU . IVAL : 1 ) ;
                return ;
              end (* then *) ;
            if ELSP1 -> . FORM = POINTER then
              begin
                WRITELN ( PRR , ' N' ) ;
                return
              end (* then *) ;
            if ELSP1 -> . FORM = POWER then
              begin
                WRITE ( PRR , ' S,' ) ;
                LVALU . PVAL -> . LENGTH := ELSP1 -> . SIZE ;
                WRITESET ( LVALU , ELSP1 -> . ELSET ) ;
                return ;
              end (* then *) ;
            if IS_CARRAY ( ELSP1 ) then
              begin
                WRITE ( PRR , ' M,' ) ;
                GEN_STRCONST ( LVALU ) ;
                WRITELN ( PRR ) ;
                return ;
              end (* then *)
          end (* else *)
      end (* WRITEDFC *) ;


   procedure TYPE_WITH_PARMS ( FSYS : SYMSET ; ID : ALPHA ; var FSP :
                             TTP ; CONF : BOOLEAN ) ;

   //******************************************************
   // process possible parameters on types                 
   // like char (30), decimal (15, 2)                      
   //******************************************************
   // called from procedure typ and from                   
   // procedure parameterlist (used when handling          
   // procedure and function definitions)                  
   //******************************************************


      var ERRINFO : CHAR32 ;
          PARAMCOUNT : ADDRRANGE ;
          PARAM1 : ADDRRANGE ;
          PARAM2 : ADDRRANGE ;
          ERRORFOUND : BOOLEAN ;
          OK : BOOLEAN ;
          MAXP : ADDRRANGE ;
          PMIN : ADDRRANGE ;
          PMAX : ADDRRANGE ;
          PDEF : ADDRRANGE ;

      begin (* TYPE_WITH_PARMS *)

        //******************************************************
        // if no sylparent present, then check for              
        // type, which needs parameters                         
        // if type needs parameters, throw error 331            
        //******************************************************

        if SY <> SYLPARENT then
          begin
            if FSP <> NIL then
              with FSP -> do
                begin
                  if FORM = CSTRING then
                    begin
                      if CONF then
                        begin
                          PARAM1 := 0 ;
                          PARAM2 := 0 ;
                          MODIFY_TYPE_PARMS ( FSP , ID , PARAM1 ,
                                              PARAM2 ) ;
                          FSP -> . CONFORMANT := TRUE ;
                        end (* then *)
                      else
                        begin
                          ERRINFO := ID ;
                          ERROR_POS ( 'E' , 331 , ERRINFO , SCB .
                                      LINENR , SCB . LINEPOS ) ;
                          PARAM1 := 254 ;
                          PARAM2 := 0 ;
                          MODIFY_TYPE_PARMS ( FSP , ID , PARAM1 ,
                                              PARAM2 ) ;
                        end (* else *)
                    end (* then *)
                  else
                    if FORM = SCALAR then
                      if SCALKIND = STANDARD then
                        begin
                          if MINPARAMCOUNT > 0 then
                            begin
                              ERRINFO := ID ;
                              ERROR_POS ( 'E' , 331 , ERRINFO , SCB .
                                          LINENR , SCB . LINEPOS ) ;
                            end (* then *) ;

        //************************************************
        // if type has defaultparameters unequal to zero, 
        // a type record with parameters has to be built  
        //************************************************

                          if DEFAULTPARAM <> 0 then
                            begin
                              PARAM1 := DEFAULTPARAM ;
                              PARAM2 := 0 ;
                              MODIFY_TYPE_PARMS ( FSP , ID , PARAM1 ,
                                                  PARAM2 ) ;
                            end (* then *)
                        end (* then *)
                end (* with *) ;
            return
          end (* then *) ;

        //******************************************************
        // at this place, a sylparent has been found            
        // if no known type, we skip until right parenthesis    
        // or another symbol from fsys                          
        //******************************************************

        if FSP = NIL then
          begin
            SKIP ( FSYS ) ;
            if SY = SYRPARENT then
              INSYMBOL ;
            return
          end (* then *) ;

        //******************************************************
        // if known type, we first check if this type           
        // accepts parameters - and how many                    
        //******************************************************

        with FSP -> do
          begin
            if FORM = SCALAR then
              if FALSE then
                begin
                  WRITELN ( TRACEF , 'type_with_parms' ) ;
                  WRITELN ( TRACEF , 'fsp.form          = ' , FORM ) ;
                  WRITELN ( TRACEF , 'fsp.scalkind      = ' , SCALKIND
                            ) ;
                  WRITELN ( TRACEF , 'fsp.minparamcount = ' ,
                            MINPARAMCOUNT ) ;
                  WRITELN ( TRACEF , 'fsp.maxparamcount = ' ,
                            MAXPARAMCOUNT ) ;
                end (* then *) ;
            case FORM of
              SCALAR :
                begin
                  if SCALKIND = STANDARD then
                    begin
                      OK := MAXPARAMCOUNT > 0 ;
                      MAXP := MAXPARAMCOUNT ;
                      PMIN := MINPARAM ;
                      PMAX := MAXPARAM ;
                      PDEF := DEFAULTPARAM ;
                    end (* then *)
                  else
                    begin
                      OK := FALSE ;
                    end (* else *) ;
                end (* tag/ca *) ;
              CSTRING :
                begin
                  OK := TRUE ;
                  MAXP := 1 ;
                  PMIN := 1 ;
                  PMAX := MAXVARCHARSIZE ;
                  PDEF := 254 ;
                end (* tag/ca *) ;
              otherwise
                OK := FALSE
            end (* case *) ;
            if not OK then
              begin
                ERRINFO := ID ;
                ERROR_POS ( 'E' , 330 , ERRINFO , SCB . LINENR , SCB .
                            LINEPOS ) ;
                SKIP ( FSYS ) ;
                if SY = SYRPARENT then
                  INSYMBOL ;
                return
              end (* then *) ;

        //******************************************************
        // now we know that parameters are ok, and we now       
        // can read the parameters and build the new type,      
        // if necessary                                         
        //******************************************************

            PARAMCOUNT := 0 ;
            PARAM1 := 0 ;
            PARAM2 := 0 ;
            ERRORFOUND := FALSE ;
            repeat
              if SY <> INTCONST then
                INSYMBOL ;
              if SY <> INTCONST then
                begin
                  ERROR ( 332 ) ;
                  SKIP ( FSYS + [ SYCOMMA , SYRPARENT , INTCONST ] ) ;
                  ERRORFOUND := TRUE ;
                end (* then *) ;

        //***************************************************
        // read intconst and set param1 and param2           
        // show errors, if too much parms etc.               
        // or if params are not acceptable                   
        //***************************************************

              if SY = INTCONST then
                begin
                  PARAMCOUNT := PARAMCOUNT + 1 ;
                  if PARAMCOUNT > MAXP then
                    begin
                      ERRINFO := ID ;
                      ERROR_POS ( 'E' , 336 , ERRINFO , SCB . LINENR ,
                                  SCB . LINEPOS ) ;
                      ERRORFOUND := TRUE ;
                    end (* then *) ;
                  case PARAMCOUNT of
                    1 : begin
                          PARAM1 := VAL . IVAL ;
                          if ( PARAM1 < PMIN ) or ( PARAM1 > PMAX )
                          then
                            begin
                              ERRINFO := ID ;
                              ERROR_POS ( 'E' , 334 , ERRINFO , SCB .
                                          LINENR , SCB . LINEPOS ) ;
                              ERRORFOUND := TRUE ;
                            end (* then *)
                        end (* tag/ca *) ;
                    2 : begin
                          PARAM2 := VAL . IVAL ;
                          if ( PARAM2 < 0 ) or ( PARAM2 > PARAM1 ) then
                            begin
                              ERRINFO := ID ;
                              ERROR_POS ( 'E' , 335 , ERRINFO , SCB .
                                          LINENR , SCB . LINEPOS ) ;
                              ERRORFOUND := TRUE ;
                            end (* then *)
                        end (* tag/ca *) ;
                    otherwise
                      begin
                        ERRINFO := ID ;
                        ERROR_POS ( 'E' , 336 , ERRINFO , SCB . LINENR
                                    , SCB . LINEPOS ) ;
                        ERRORFOUND := TRUE ;
                      end (* otherw *)
                  end (* case *) ;
                  INSYMBOL ;
                  if not ( SY in [ SYCOMMA , SYRPARENT ] ) then
                    begin
                      ERROR ( 333 ) ;
                      SKIP ( FSYS + [ SYCOMMA , SYRPARENT , INTCONST ]
                             ) ;
                      ERRORFOUND := TRUE ;
                    end (* then *)
                end (* then *)
            until SY in FSYS + [ SYRPARENT ] ;
            if ERRORFOUND then
              begin
                PARAM1 := PDEF ;
                PARAM2 := 0 ;
              end (* then *) ;
            MODIFY_TYPE_PARMS ( FSP , ID , PARAM1 , PARAM2 ) ;
            if SY = SYRPARENT then
              INSYMBOL ;
          end (* with *) ;
      end (* TYPE_WITH_PARMS *) ;


   procedure TYP ( FSYS : SYMSET ; var FSP : TTP ; var FSIZE :
                 ADDRRANGE ) ;

      var LSP , LSP1 , LSP2 : TTP ;
          OLDTOP : DISPRANGE ;
          LCP , LCP2 : IDP ;
          LSIZE , DISPL : ADDRRANGE ;
          LMIN , LMAX : INTEGER ;
          ALNFCT : ALNRNG ;
          OLDPACKST , PACKST2 : BOOLEAN ;


      procedure SIMPLETYPE ( FSYS : SYMSET ; var FSP : TTP ) ;

         var LSP , LSP1 : TTP ;
             LCP , LCP1 : IDP ;
             TTOP : DISPRANGE ;
             LCNT : INTEGER ;
             LVALU : XCONSTANT ;
             FLAG : BOOLEAN ;
             VALX : XCONSTANT ;
             SCAL_OFFS : INTEGER ;
             IDL : INTEGER ;
             IX : INTEGER ;
             IDCLASS : CHAR ;
             ERRLINE_SAVE : INTEGER ;
             ERRPOS_SAVE : INTEGER ;
             SUBR_OK : BOOLEAN ;

         begin (* SIMPLETYPE *)
           if not ( SY in SIMPTYPEBEGSYS ) then
             begin
               ERROR ( 1 ) ;
               SKIP ( FSYS + SIMPTYPEBEGSYS )
             end (* then *) ;
           if not ( SY in SIMPTYPEBEGSYS ) then
             begin
               FSP := NIL ;
               return
             end (* then *) ;

           (***********************************************)
           (*  if left parent, the new type is a          *)
           (*  scalar type aka enumeration                *)
           (***********************************************)

           if SY = SYLPARENT then
             begin

           (***********************************************)
           (*  declare new scalar type                    *)
           (***********************************************)

               TTOP := TOP ;
               TOP := LEVEL ;
               NEW ( LSP , SCALAR , DECLARED ) ;
               with LSP -> do
                 begin
                   ERRORFLAG := FALSE ;
                   SIZE := INTSIZE ;
                   FORM := SCALAR ;
                   SCALKIND := DECLARED
                 end (* with *) ;

           (***********************************************)
           (*  declare constants which belong to the new  *)
           (*  scalar type                                *)
           (***********************************************)

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
                         VALUES . STRTYPE := ' ' ;
                         KLASS := KONST
                       end (* with *) ;
                     ENTERID ( LCP ) ;
                     LCNT := LCNT + 1 ;
                     LCP1 := LCP ;
                     INSYMBOL
                   end (* then *)
                 else
                   ERROR ( 2 ) ;
                 if not ( SY in FSYS + [ SYCOMMA , SYRPARENT ] ) then
                   begin
                     ERROR ( 6 ) ;
                     SKIP ( FSYS + [ SYCOMMA , SYRPARENT ] )
                   end (* then *)
               until SY <> SYCOMMA ;
               if not OPT . NOPACKING then
                 begin
                   LSP -> . SIZE := HINTSIZE ;
                   if PACKDATA then
                     if LCNT <= ORDCHMAX then
                       LSP -> . SIZE := CHARSIZE
                 end (* then *) ;
               LSP -> . ALN := LSP -> . SIZE ;
               LSP -> . FCONST := LCP1 ;

           /***************************************/
           /*  im statischen Bereich Vektor mit   */
           /*  den namen der skalaren konstanten  */
           /*  anlegen fuer die spaeteren         */
           /*  ein- und ausgabe-Funktionen        */
           /*-------------------------------------*/
           /*  erweiterung opp - 11.2016          */
           /***************************************/

               CHECKSTARTCST ;

           /*****************************************************/
           /*  zunaechst einen vektor aus Halbworten mit        */
           /*  Offset und Laenge der Bezeichnungen anlegen;     */
           /*  letzte zuerst; Anzahl = hoechster Wert plus      */
           /*  eins, Offset demnach (Anzahl + 1) * 4.           */
           /*  Die Offsets werden je nach Laenge des            */
           /*  Bezeichners weitergeschaltet. Anhand des         */
           /*  Offsets kann man auch die Groesse des            */
           /*  Vektors und damit die Groesse des skalaren       */
           /*  Typs ableiten                                    */
           /*****************************************************/

               ALIGN ( CONSTLCOUNTER , HINTSIZE ) ;
               LSP -> . CSTNAME := FPROCP -> . CSTNAME ;
               LSP -> . LITOFFS := CONSTLCOUNTER ;
               LCP1 := LSP -> . FCONST ;
               SCAL_OFFS := ( LCP1 -> . VALUES . IVAL ) + 1 ;
               SCAL_OFFS := SCAL_OFFS * 4 ;
               while LCP1 <> NIL do
                 begin
                   VALX . IVAL := SCAL_OFFS ;
                   VALX . STRTYPE := ' ' ;
                   WRITEDFC ( PTYPE_INT , 2 , VALX , FALSE ) ;
                   CONSTLCOUNTER := CONSTLCOUNTER + 2 ;
                   VALX . IVAL := GETIDLEN ( LCP1 -> . NAME ) ;
                   VALX . STRTYPE := ' ' ;
                   WRITEDFC ( PTYPE_INT , 2 , VALX , FALSE ) ;
                   CONSTLCOUNTER := CONSTLCOUNTER + 2 ;
                   SCAL_OFFS := SCAL_OFFS + VALX . IVAL + 1 ;
                   LCP1 := LCP1 -> . NEXT ;
                 end (* while *) ;

           /*****************************************************/
           /*  jetzt die Konstanten selbst ablegen              */
           /*****************************************************/

               LCP1 := LSP -> . FCONST ;
               while LCP1 <> NIL do
                 begin
                   IDL := GETIDLEN ( LCP1 -> . NAME ) + 1 ;
                   WRITEDFC ( NIL , IDL , VALX , FALSE ) ;
                   WRITE ( PRR , ' M,''' ) ;
                   for IX := 1 to IDL - 1 do
                     WRITE ( PRR , LCP1 -> . NAME [ IX ] ) ;
                   WRITELN ( PRR , ' ''' ) ;
                   CONSTLCOUNTER := CONSTLCOUNTER + IDL ;
                   LCP1 := LCP1 -> . NEXT ;
                 end (* while *) ;
               IDL := 1 ;
               WRITEDFC ( NIL , IDL , VALX , FALSE ) ;
               WRITELN ( PRR , ' M,''*''' ) ;
               CONSTLCOUNTER := CONSTLCOUNTER + IDL ;

           /***************************************/
           /*  ende erweiterung 11.2016           */
           /***************************************/

               TOP := TTOP ;
               if SY = SYRPARENT then
                 INSYMBOL
               else
                 ERROR ( 4 ) ;
               FSP := LSP ;
               if not ( SY in FSYS ) then
                 begin
                   ERROR ( 6 ) ;
                   SKIP ( FSYS )
                 end (* then *) ;
               return ;
             end (* then *) ;

           (***********************************************)
           (*  otherwise the type is a subrange           *)
           (*  that is range of constants or              *)
           (*  a type identifier which refers to          *)
           (*  another type - problem: a subrange         *)
           (*  may start with an identifier, too          *)
           (*  (const identifier)                         *)
           (***********************************************)
           (*  10.2017: take special care for             *)
           (*  unknown identifiers to minimize            *)
           (*  more errors to follow                      *)
           (***********************************************)

           IDCLASS := ' ' ;
           if SY = IDENT then
             begin
               SID_RC := SEARCHID ( ID , FALSE , FALSE , [ TYPES ,
                         KONST ] , LCP ) ;
               ERRLINE_SAVE := SCB . LINENR ;
               ERRPOS_SAVE := SCB . LINEPOS ;
               INSYMBOL ;
               case SID_RC of
                 0 : ;
                 104 : begin
                         ERROR_POS ( 'E' , SID_RC , ' ' , ERRLINE_SAVE
                                     , ERRPOS_SAVE ) ;
                         if SY = SYDOTDOT then
                           SID_RC := SEARCHID ( ID , FALSE , TRUE , [
                                     KONST ] , LCP )
                         else
                           SID_RC := SEARCHID ( ID , FALSE , TRUE , [
                                     TYPES ] , LCP )
                       end (* tag/ca *) ;
                 otherwise
                   begin
                     ERROR_POS ( 'E' , SID_RC , ' ' , ERRLINE_SAVE ,
                                 ERRPOS_SAVE ) ;
                   end (* otherw *)
               end (* case *) ;
               LSP := NIL ;
               if LCP <> NIL then
                 begin
                   case LCP -> . KLASS of
                     KONST : begin
                               IDCLASS := 'K' ;
                               NEW ( LSP , SUBRANGE ) ;
                               LSP -> . ERRORFLAG := FALSE ;
                               with LSP -> , LCP -> do
                                 if IDTYPE = NIL then
                                   begin
                                     if FALSE then
                                       WRITELN ( TRACEF , 'simpletype '
                                                 'unknown konst id 1' )
                                                 ;
                                     RANGETYPE := NIL ;
                                     FORM := SUBRANGE ;
                                     MIN . IVAL := 1 ;
                                     SIZE := 1 ;
                                   end (* then *)
                                 else
                                   begin
                                     RANGETYPE := IDTYPE ;
                                     FORM := SUBRANGE ;
                                     if IS_CARRAY ( RANGETYPE ) then
                                       begin
                                         ERROR ( 148 ) ;
                                         RANGETYPE := NIL
                                       end (* then *) ;
                                     MIN := VALUES ;
                                     SIZE := IDTYPE -> . SIZE
                                   end (* else *)
                             end (* tag/ca *) ;
                     otherwise
                       IDCLASS := 'T' ;
                   end (* case *) ;
                 end (* then *)
             end (* then *)
           else
             begin
               NEW ( LSP , SUBRANGE ) ;
               LSP -> . ERRORFLAG := FALSE ;
               LSP -> . FORM := SUBRANGE ;
               if SY = INTDOTDOT then
                 begin
                   with LSP -> do
                     begin
                       RANGETYPE := PTYPE_INT ;
                       MIN := VAL ;
                       SIZE := INTSIZE ;
                     end (* with *) ;
                   SY := SYDOTDOT
                 end (* then *)
               else
                 begin
                   CONSTANT ( FSYS + [ SYDOTDOT ] , LSP1 , LVALU ) ;
                   if IS_CARRAY ( LSP1 ) then
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
                     end (* with *)
                 end (* else *)
             end (* else *) ;

           (***********************************************)
           (*  10.2017: idclass = 'T' only set, when      *)
           (*  known type id seen; in this case           *)
           (*  a dotdot symbol is an error                *)
           (***********************************************)

           if IDCLASS = 'T' then
             begin
               LSP := LCP -> . IDTYPE ;
               FLAG := FALSE ;
               if PACKDATA then
                 if not OPT . NOPACKING then
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

           (******************************)
           (* CONVERT TO SUBRANGE        *)
           (******************************)

                         begin
                           FORM := SUBRANGE ;
                           RANGETYPE := LSP ;
                           MIN . IVAL := 0 ;
                           MAX . IVAL := LCNT ;
                         end (* then *) ;
                     end (* with *) ;
                   LSP := LSP1 ;
                 end (* then *) ;
               FSP := LSP ;
               if SY = SYDOTDOT then
                 begin
                   ERROR ( 61 ) ;
                   INSYMBOL ;
                 end (* then *) ;
               TYPE_WITH_PARMS ( FSYS + [ SYRPARENT ] , ID , FSP ,
                                 FALSE ) ;
               if not ( SY in FSYS ) then
                 begin
                   ERROR ( 6 ) ;
                   SKIP ( FSYS )
                 end (* then *) ;
               return ;
             end (* then *) ;

           (***********************************************)
           (*  10.2017: work on subrange                  *)
           (***********************************************)

           if SY = SYDOTDOT then
             INSYMBOL
           else
             ERROR ( 60 ) ;
           CONSTANT ( FSYS , LSP1 , LVALU ) ;

           (*************************************************)
           (* don't continue checks, when lsp for           *)
           (* subrange is not ok so far ...                 *)
           (*************************************************)

           SUBR_OK := ( LSP1 <> NIL ) and ( LSP <> NIL ) ;
           if SUBR_OK then
             SUBR_OK := ( LSP -> . FORM = SUBRANGE ) and ( LSP -> .
                        RANGETYPE <> NIL ) ;
           if SUBR_OK then
             begin
               LSP -> . MAX := LVALU ;
               if LSP1 -> . SIZE > CHARSIZE then

           (*********************)
           (* SCOPE FOR PACKING *)
           (*********************)

                 if not OPT . NOPACKING then
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
                 ERROR ( 107 ) ;
               with LSP -> do
                 if FORM = SUBRANGE then
                   if RANGETYPE <> NIL then
                     if IS_STDTYPE ( RANGETYPE , 'R' ) then
                       ERROR ( 398 )
                     else
                       if MIN . IVAL > MAX . IVAL then
                         ERROR ( 102 )
             end (* then *)

           (*************************************************)
           (* if subr is not ok, then assign default        *)
           (* subrange type (int with limits 1 to 2)        *)
           (*************************************************)

           else
             begin
               if FALSE then
                 WRITELN ( TRACEF , 'simpletype '
                           'assign default subr type' ) ;
               if LSP = NIL then
                 NEW ( LSP , SUBRANGE ) ;
               LSP -> . ERRORFLAG := TRUE ;
               if FALSE then
                 WRITELN ( TRACEF , 'simpletype '
                           'set errorflag to true' ) ;
               LSP -> . FORM := SUBRANGE ;
               with LSP -> do
                 begin
                   RANGETYPE := PTYPE_INT ;
                   MIN . IVAL := 1 ;
                   MAX . IVAL := 2 ;
                   SIZE := INTSIZE ;
                   ALN := SIZE ;
                 end (* with *) ;
             end (* else *) ;
           FSP := LSP ;
           if not ( SY in FSYS ) then
             begin
               ERROR ( 6 ) ;
               SKIP ( FSYS )
             end (* then *)
         end (* SIMPLETYPE *) ;


      procedure FIELDLIST ( FSYS : SYMSET ; var FRECVAR : TTP ; var
                          RECALN : ALNRNG ; FLDOWNER : TTP ; var
                          FIRSTFLD : IDP ) ;

         label 10 ;

         var LCP , LCP1 , NXT , NXT1 : IDP ;
             LSP , LSP1 , LSP2 , LSP3 , LSP4 : TTP ;
             MINSIZE , MAXSIZE , LSIZE : ADDRRANGE ;
             LVALU : XCONSTANT ;
             LALNFCT : ALNRNG ;

         begin (* FIELDLIST *)
           NXT := NIL ;
           FIRSTFLD := NIL ;
           LSP := NIL ;
           RECALN := 1 ;
           if not ( SY in FSYS + [ IDENT , SYCASE ] ) then
             begin
               ERROR ( 19 ) ;
               SKIP ( FSYS + [ IDENT , SYCASE ] )
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
                         if OPT . GET_STAT then
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
                 if not ( SY in [ SYCOMMA , SYCOLON ] ) then
                   begin
                     ERROR ( 6 ) ;
                     SKIP ( FSYS + [ SYCOMMA , SYCOLON , SYSEMICOLON ,
                            SYCASE ] )
                   end (* then *) ;
                 TEST := SY <> SYCOMMA ;
                 if not TEST then
                   INSYMBOL
               until TEST ;
               if SY = SYCOLON then
                 INSYMBOL
               else
                 ERROR ( 5 ) ;
               if FIRSTFLD = NIL then
                 FIRSTFLD := NXT1 ;
               TYP ( FSYS + [ SYCASE , SYSEMICOLON ] , LSP , LSIZE ) ;
               LALNFCT := 1 ;
               if LSP <> NIL then
                 LALNFCT := LSP -> . ALN ;
               while NXT1 <> NIL do

           (******************************************)
           (* ANY "FIELDS" DEFINED IN THIS ROUND ?   *)
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
               if SY = SYSEMICOLON then
                 begin
                   INSYMBOL ;
                   if not ( SY in [ IDENT , SYCASE , SYEND ] ) then

           (**********************)
           (* IGNOR EXTRA ;      *)
           (**********************)

                     begin
                       ERROR ( 19 ) ;
                       SKIP ( FSYS + [ IDENT , SYCASE ] )
                     end (* then *)
                 end (* then *)
             end (* while *) ;
           if SY = SYCASE then
             begin
               NEW ( LSP , TAGFLD ) ;
               with LSP -> do
                 begin
                   ERRORFLAG := FALSE ;
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
                       if OPT . GET_STAT then
                         FLDOWNER -> . NO_FLDS := FLDOWNER -> . NO_FLDS
                                                  + 1 ;
                     end (* with *) ;
                   INSYMBOL ;
                   if SY = SYCOLON then

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
                   SID_RC := SEARCHID ( ID , TRUE , TRUE , [ TYPES ] ,
                             LCP1 ) ;
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
                         if ( FORM <= SUBRANGE ) or IS_CARRAY ( LSP1 )
                         then
                           begin
                             if COMPTYPES ( PTYPE_REAL , LSP1 ) = 1
                             then
                               ERROR ( 109 )
                             else
                               if IS_CARRAY ( LSP1 ) then
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
                   SKIP ( FSYS + [ SYOF , SYLPARENT ] )
                 end ;
               LSP -> . SIZE := DISPL ;
               if SY = SYOF then
                 INSYMBOL
               else
                 ERROR ( 8 ) ;
               LSP1 := NIL ;
               MINSIZE := DISPL ;
               MAXSIZE := DISPL ;
               repeat
                 LSP2 := NIL ;
                 repeat
                   CONSTANT ( FSYS + [ SYCOMMA , SYCOLON , SYLPARENT ]
                              , LSP3 , LVALU ) ;
                   if LSP -> . TAGFIELDP <> NIL then
                     if COMPTYPES ( LSP -> . TAGFIELDP -> . IDTYPE ,
                     LSP3 ) <> 1 then
                       ERROR ( 111 ) ;
                   NEW ( LSP3 , VARIANT ) ;
                   with LSP3 -> do
                     begin
                       ERRORFLAG := FALSE ;
                       NXTVAR := LSP1 ;
                       SUBVAR := LSP2 ;
                       VARVAL := LVALU ;
                       FSTSUBFLD := NIL ;
                       FORM := VARIANT
                     end (* with *) ;
                   LSP1 := LSP3 ;
                   LSP2 := LSP3 ;
                   TEST := SY <> SYCOMMA ;
                   if not TEST then
                     INSYMBOL
                 until TEST ;
                 if SY = SYCOLON then
                   INSYMBOL
                 else
                   ERROR ( 5 ) ;
                 if SY = SYLPARENT then
                   INSYMBOL
                 else
                   ERROR ( 9 ) ;
                 FIELDLIST ( FSYS + [ SYRPARENT , SYSEMICOLON ] , LSP2
                             , LALNFCT , FLDOWNER , LCP1 ) ;
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
                 if SY = SYRPARENT then
                   begin
                     INSYMBOL ;
                     if not ( SY in FSYS + [ SYSEMICOLON ] ) then
                       begin
                         ERROR ( 6 ) ;
                         SKIP ( FSYS + [ SYSEMICOLON ] )
                       end (* then *)
                   end (* then *)
                 else
                   ERROR ( 4 ) ;
                 TEST := SY <> SYSEMICOLON ;
                 if not TEST then
                   begin
                     DISPL := MINSIZE ;
                     INSYMBOL ;
                     TEST := SY = SYEND ;

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
              if SY = SYARROW then
                begin
                  NEW ( LSP , POINTER ) ;
                  FSP := LSP ;
                  with LSP -> do
                    begin
                      ERRORFLAG := FALSE ;
                      ELTYPE := NIL ;
                      SIZE := PTRSIZE ;
                      ALN := PTRSIZE ;
                      FORM := POINTER
                    end (* with *) ;
                  INSYMBOL ;
                  if SY = IDENT then
                    begin

        (***********************************)
        (*NO ERROR IF SEARCH NOT SUCCESSFUL*)
        (***********************************)

                      SID_RC := SEARCHID ( ID , FALSE , FALSE , [ TYPES
                                ] , LCP ) ;
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
                  LSP := NIL ;

        (*******************************)
        (*  keyword recognized         *)
        (*******************************)

                  if SY = SYPACKED then
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

        (*********************************************************)
        (*  case statement for complex types:                    *)
        (*  arrays, records, sets, files                         *)
        (*********************************************************)

                  case SY of

        (*******************************)
        (*  keyword array recognized   *)
        (*******************************)

                    SYARRAY :
                      begin
                        INSYMBOL ;
                        if SY = SYLBRACK then
                          INSYMBOL
                        else
                          begin
                            if SY = SYLPARENT then
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
                              ERRORFLAG := FALSE ;
                              ALN := 1 ;
                              AELTYPE := LSP1 ;
                              INXTYPE := NIL ;
                              FORM := ARRAYS
                            end (* with *) ;
                          LSP1 := LSP ;
                          SIMPLETYPE ( FSYS + [ SYCOMMA , SYRBRACK ,
                                       SYOF , SYRPARENT ] , LSP2 ) ;
                          if LSP2 <> NIL then
                            if LSP2 -> . FORM <= SUBRANGE then
                              begin
                                if IS_STDTYPE ( LSP2 , 'R' ) then
                                  begin
                                    ERROR ( 109 ) ;
                                    LSP2 := NIL
                                  end (* then *)
                                else
                                  if LSP2 = PTYPE_INT then
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
                          TEST := SY <> SYCOMMA ;
                          if not TEST then
                            INSYMBOL
                        until TEST ;
                        if SY = SYRBRACK then
                          INSYMBOL
                        else
                          begin
                            if SY = SYRPARENT then
                              begin
                                ERRKIND := 'W' ;
                                INSYMBOL
                              end (* then *) ;
                            ERROR ( 12 )
                          end (* else *) ;
                        if SY = SYOF then
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
                                  LSIZE := LSIZE * ( LMAX - LMIN + 1 )
                                           ;
                                  SIZE := LSIZE ;
                                  if LSP <> NIL then
                                    ALN := LSP -> . ALN

        (*****************)
        (*PROPAGATE ALN  *)
        (*****************)


                                           ;
                                end (* then *)
                              else

        (***************)
        (*INXTYPE = NIL*)
        (***************)

                                SIZE := 0
                            end (* with *) ;
                          LSP := LSP1 ;
                          LSP1 := LSP2
                        until LSP1 = NIL
                      end (* tag/ca *) ;

        (*******************************)
        (*  keyword record recognized  *)
        (*******************************)

                    SYRECORD :
                      begin
                        INSYMBOL ;
                        if OPT . GET_STAT then
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
                            ERRORFLAG := FALSE ;
                            ALN := 1 ;
                            FLD_DISP_LEV := TOP ;
                            FSTFLD := NIL ;
                            NO_FLDS := 0 ;
                            FIELDLIST ( FSYS - [ SYSEMICOLON ] + [
                                        SYEND ] , LSP1 , ALNFCT , LSP ,
                                        FSTFLD ) ;
                            RECVAR := LSP1 ;
                            SIZE := DISPL ;
                            FORM := RECORDS ;
                            ALN := ALNFCT ;
                            FLD_DISP_LEV := - 1 ;
                          end (* with *) ;
                        if TOP >= 0 then
                          TOP := TOP - 1 ;
                        if SY = SYEND then
                          INSYMBOL
                        else
                          ERROR ( 13 )
                      end (* tag/ca *) ;

        (*******************************)
        (*  keyword set recognized     *)
        (*******************************)

                    SYSET : begin
                              INSYMBOL ;
                              if SY = SYOF then
                                INSYMBOL
                              else
                                ERROR ( 8 ) ;
                              PACKDATA := FALSE ;
                              SIMPLETYPE ( FSYS , LSP1 ) ;
                              if LSP1 <> NIL then
                                if LSP1 = PTYPE_INT then
                                  ERROR ( 304 )
                                else
                                  if ( LSP1 -> . FORM > SUBRANGE ) then
                                    begin
                                      ERROR ( 115 ) ;
                                      LSP1 := NIL
                                    end (* then *)
                                  else
                                    if IS_STDTYPE ( LSP1 , 'R' ) then
                                      ERROR ( 114 )
                                    else
                                      if LSP1 -> . FORM = SUBRANGE then
                                        begin
                                          if LSP1 -> . MAX . IVAL -
                                          LSP1 -> . MIN . IVAL + 1 >
                                          SETMAXSIZE then
                                            ERROR ( 307 ) ;
                                          if LSP1 -> . MAX . IVAL >
                                          SETUPPLIMIT then
                                            ERROR ( 308 ) ;
                                          if LSP1 -> . MIN . IVAL <
                                          SETLOWLIMIT then
                                            ERROR ( 309 )
                                        end (* then *) ;
                              NEW ( LSP , POWER ) ;
                              with LSP -> do
                                begin
                                  ERRORFLAG := FALSE ;
                                  ALN := 1 ;
                                  ELSET := LSP1 ;
                                  CALC_SETTYPSIZE ( LSP1 , SIZE ,
                                                   SETMIN , SETMAX ,
                                                   SETOFFS ) ;
                                  ALN := WORDSIZE ;
                                  FORM := POWER
                                end (* with *) ;
                            end (* tag/ca *) ;

        (*******************************)
        (*  keyword file recognized    *)
        (*******************************)

                    SYFILE :
                      begin
                        INSYMBOL ;
                        if SY = SYOF then
                          INSYMBOL
                        else
                          ERROR ( 8 ) ;
                        TYP ( FSYS , LSP1 , LSIZE ) ;
                        LSP := PTYPE_TEXT ;

        (**************************)
        (* ASSUME THE COMMON CASE *)
        (**************************)

                        if LSP1 <> NIL then
                          if LSP1 <> PTYPE_CHAR then

        (******************)
        (* NOT A TEXTFILE *)
        (******************)

                            if LSP1 -> . FORM <> FILES then
                              begin
                                NEW ( LSP , FILES ) ;
                                with LSP -> do
                                  begin
                                    ERRORFLAG := FALSE ;
                                    FILTYPE := LSP1 ;
                                    ALN := LSP1 -> . ALN ;
                                    SIZE := LSIZE + FILHDRSIZE ;
                                    if SIZE < FILMINSIZE then
                                      SIZE := FILMINSIZE ;
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
                      end (* tag/ca *)
                  end (* case *) ;

        (*********************************************************)
        (*  end case statement for complex types:                *)
        (*  arrays, records, sets, files                         *)
        (*********************************************************)

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
          if not ( SY in FSYS + [ SYCOMMA , SYSEMICOLON ] ) then
            begin
              ERROR ( 6 ) ;
              SKIP ( FSYS + [ SYCOMMA , SYSEMICOLON ] )
            end (* then *) ;
          TEST := SY <> SYCOMMA ;
          if not TEST then
            INSYMBOL
        until TEST ;
        if SY = SYSEMICOLON then
          INSYMBOL
        else
          ERROR ( 14 )
      end (* LABELDECLARATION *) ;


   function SET_CONST_PART ( TYP1 : TTP ; VAL1 : XCONSTANT ; PSI :
                           PSETINFO ) : BOOLEAN ;

   /***********************************/
   /* typ1 = type of first constant   */
   /* lval1 = lvalu of first constant */
   /***********************************/


      var LATTR : ATTR ;
          OK : BOOLEAN ;
          TYP2 : TTP ;
          VAL2 : XCONSTANT ;
          I : INTEGER ;
          OFFS : INTEGER ;

      begin (* SET_CONST_PART *)
        TYP2 := TYP1 ;
        VAL2 := VAL1 ;

        (****************************************)
        (* if char type, check for hex constant *)
        (****************************************)

        if TYP1 = PTYPE_CHAR then
          begin
            PSI -> . CHARTYPE := TRUE ;
            if VAL1 . STRTYPE in [ 'X' , 'B' ] then
              if PSI -> . HEXORBIN in [ ' ' , 'J' ] then
                PSI -> . HEXORBIN := 'J'
              else
                PSI -> . HEXORBIN := 'T'
            else
              if PSI -> . HEXORBIN in [ ' ' , 'N' ] then
                PSI -> . HEXORBIN := 'N'
              else
                PSI -> . HEXORBIN := 'T'
          end (* then *) ;

        (***************************)
        (* dotdot = constant range *)
        (***************************)

        if SY = SYDOTDOT then
          begin
            INSYMBOL ;
            CONSTANT ( FSYS + [ SYCOMMA , SYRBRACK ] , TYP2 , VAL2 ) ;
            if COMPTYPES ( TYP2 , TYP1 ) <> 1 then
              begin
                VAL2 . IVAL := VAL1 . IVAL ;
                ERROR ( 137 )
              end (* then *) ;

        (****************************************)
        (* if char type, check for hex constant *)
        (****************************************)

            if TYP2 = PTYPE_CHAR then
              begin
                if VAL2 . STRTYPE in [ 'X' , 'B' ] then
                  if PSI -> . HEXORBIN in [ ' ' , 'J' ] then
                    PSI -> . HEXORBIN := 'J'
                  else
                    PSI -> . HEXORBIN := 'T'
                else
                  if PSI -> . HEXORBIN in [ ' ' , 'N' ] then
                    PSI -> . HEXORBIN := 'N'
                  else
                    PSI -> . HEXORBIN := 'T'
              end (* then *) ;
          end (* then *) ;

        (***************************************)
        (* some global set checks              *)
        (* first: val2 >= val1                 *)
        (***************************************)

        OK := TRUE ;
        if TYP2 <> NIL then
          if VAL2 . IVAL < VAL1 . IVAL then
            begin
              ERROR ( 310 ) ;
              OK := FALSE
            end (* then *) ;

        (***************************************)
        (* second: val2 not > setupplimit      *)
        (***************************************)

        if OK then
          if VAL2 . IVAL > SETUPPLIMIT then
            begin
              ERROR ( 308 ) ;
              OK := FALSE
            end (* then *) ;

        (**************************************)
        (* third: val2 not < setlowlimit      *)
        (**************************************)

        if OK then
          if VAL1 . IVAL < SETLOWLIMIT then
            begin
              ERROR ( 309 ) ;
              OK := FALSE
            end (* then *) ;
        OFFS := 0 ;
        if OK then
          begin
            if PSI -> . ELEMCOUNT = 0 then
              begin
                for I := 1 to SETMAXSIZE do
                  PSI -> . SETELEMS [ I ] := FALSE ;
                PSI -> . SETMIN := VAL1 . IVAL ;
                PSI -> . SETMAX := VAL2 . IVAL ;
                PSI -> . ELEMCOUNT := 1 ;
              end (* then *)
            else
              begin
                if PSI -> . SETMIN > VAL1 . IVAL then
                  begin
                    OFFS := PSI -> . SETMIN - VAL1 . IVAL ;
                    PSI -> . SETMIN := VAL1 . IVAL ;
                  end (* then *) ;
                if PSI -> . SETMAX < VAL2 . IVAL then
                  PSI -> . SETMAX := VAL2 . IVAL ;
                PSI -> . ELEMCOUNT := PSI -> . ELEMCOUNT + 1 ;
              end (* else *) ;
            if PSI -> . SETMAX - PSI -> . SETMIN + 1 > SETMAXSIZE then
              begin
                ERROR ( 311 ) ;
                PSI -> . RANGEERR := 1 ;
                OK := FALSE
              end (* then *) ;
          end (* then *) ;

        (**************************************)
        (* shift values in temp vector        *)
        (* insert values into temp vector     *)
        (**************************************)

        if OK then
          begin
            if OFFS > 0 then
              begin
                for I := SETMAXSIZE DOWNTO OFFS + 1 do
                  PSI -> . SETELEMS [ I ] := PSI -> . SETELEMS [ I -
                                             OFFS ] ;
                for I := 1 to OFFS do
                  PSI -> . SETELEMS [ I ] := FALSE ;
              end (* then *) ;
            for I := VAL1 . IVAL to VAL2 . IVAL do
              PSI -> . SETELEMS [ I - PSI -> . SETMIN + 1 ] := TRUE ;
          end (* then *) ;

        /************************************/
        /*   bei comma:                     */
        /*   naechstes symbol und           */
        /*   nochmal ein set_const_part     */
        /*   signalisieren                  */
        /************************************/

        if SY = SYCOMMA then
          begin
            INSYMBOL ;
            SET_CONST_PART := TRUE ;
          end (* then *)
        else
          begin
            SET_CONST_PART := FALSE ;
          end (* else *)
      end (* SET_CONST_PART *) ;


   procedure STRUCTCONSTANT ( FSYS : SYMSET ; var FSP : TTP ; var FVALU
                            : XCONSTANT ; var SLC : INTEGER ) ;

      label 10 ;

      var I , J , K , L : INTEGER ;
          OK : BOOLEAN ;
          NOCHMAL : BOOLEAN ;
          PSI : PSETINFO ;
          LVALU : XCONSTANT ;
          LSP , LSP1 , ELT , LRECVAR : TTP ;
          DUMMY_TYP : TTP ;
          FLDPR : IDP ;
          LX : ADDRRANGE ;
          CT_RESULT : INTEGER ;
          SIZE1 : INTEGER ;
          SIZE2 : INTEGER ;
          TEST : BOOLEAN ;
          ERRFLAG : BOOLEAN ;


      procedure STOWCONST ( ELSP : TTP ) ;

         var ELSIZE : INTEGER ;
             ELSP1 : TTP ;
             CT_RESULT : INTEGER ;
             SIZE1 : ADDRRANGE ;
             SIZE2 : ADDRRANGE ;

         begin (* STOWCONST *)
           ELSP1 := ELSP ;
           if ELSP <> NIL then
             begin
               ALIGN ( CONSTLCOUNTER , ELSP -> . ALN ) ;
               ELSIZE := ELSP -> . SIZE
             end (* then *)
           else
             ELSIZE := 1 ;
           STRUCTCONSTANT ( FSYS + [ SYCOMMA , SYRPARENT ] , ELSP1 ,
                            LVALU , I ) ;

           (******************************************)
           (* accept comptypes = 2 here too          *)
           (* no problem, if const string is shorter *)
           (******************************************)

           CT_RESULT := COMPTYPES ( ELSP , ELSP1 ) ;
           if not ( CT_RESULT in [ 1 , 2 , 3 ] ) then
             begin
               ERROR ( 145 ) ;
               ELSP1 := NIL
             end (* then *) ;
           if ELSP1 <> NIL then
             begin

           (******************************************)
           (* if comptypes returns 2, adjust         *)
           (* string constant size to size of        *)
           (* constant definition                    *)
           (******************************************)

               if CT_RESULT in [ 2 , 3 ] then
                 MOD_STRCONST ( CT_RESULT , LVALU , ELSP1 , ELSP -> .
                                SIZE ) ;
               if OPT . PRCODE then
                 if I < 0 then
                   begin
                     WRITEDFC ( ELSP1 , ELSIZE , LVALU , FALSE ) ;
                     CONSTLCOUNTER := CONSTLCOUNTER + ELSIZE ;
                   end (* then *)
             end (* then *) ;
         end (* STOWCONST *) ;


      begin (* STRUCTCONSTANT *)
        LSP := FSP ;
        FVALU . IVAL := 0 ;
        FVALU . STRTYPE := ' ' ;
        SLC := - 1 ;
        if SY in CONSTBEGSYS then

        (************************************************)
        (* simple constant is used here to recognize    *)
        (* for example the strings that are used inside *)
        (* of complex structured constants, so the      *)
        (* return code 2 has to be accepted here;       *)
        (* it is checked again by the caller, which     *)
        (* takes then appropriate action                *)
        (************************************************)

          begin
            CONSTANT ( FSYS , FSP , FVALU ) ;
            CT_RESULT := COMPTYPES ( LSP , FSP ) ;
            if CT_RESULT in [ 1 , 2 , 3 ] then
              begin
                if LSP <> NIL then
                  begin
                    FSP := LSP ;
                    if CT_RESULT in [ 2 , 3 ] then
                      MOD_STRCONST ( CT_RESULT , FVALU , DUMMY_TYP ,
                                     LSP -> . SIZE ) ;
                  end (* then *) ;
              end (* then *)
            else
              begin
                ERROR ( 145 ) ;
                FSP := NIL
              end (* else *)
          end (* then *)
        else
          if SY = SYLBRACK then
            begin

        (**********************************************)
        (* read set constant, that is:                *)
        (* empty set or a sequence of set const parts *)
        (* which consist of single constants or       *)
        (* constant ranges, separated by commas       *)
        (* see set const part                         *)
        (**********************************************)

              INSYMBOL ;
              ELT := NIL ;
              if LSP <> NIL then
                if LSP -> . FORM = POWER then
                  ELT := LSP -> . ELSET
                else
                  ERROR ( 145 ) ;
              PSI := PSIGLOB ;
              PSI -> . ELEMCOUNT := 0 ;
              PSI -> . SETMIN := 0 ;
              PSI -> . SETMAX := 0 ;
              PSI -> . RANGEERR := 0 ;
              PSI -> . CHARTYPE := FALSE ;
              PSI -> . HEXORBIN := ' ' ;
              PSI -> . CONST_IN_SET := 0 ;
              PSI -> . VARS_IN_SET := 0 ;
              if SY <> SYRBRACK then
                repeat
                  CONSTANT ( FSYS + [ SYRBRACK , SYCOMMA , SYDOTDOT ] ,
                             LSP1 , LVALU ) ;
                  if COMPTYPES ( LSP1 , ELT ) <> 1 then
                    ERROR ( 145 ) ;
                  ELT := LSP1 ;
                  NOCHMAL := SET_CONST_PART ( ELT , LVALU , PSI ) ;
                until not NOCHMAL ;
              if SY = SYRBRACK then
                INSYMBOL
              else
                ERROR ( 12 ) ;
              if FALSE then
                begin
                  WRITELN ( TRACEF ) ;
                  WRITELN ( TRACEF , 'linecnt = ' , LINECNT : 1 ) ;
                  WRITELN ( TRACEF , 'psi.elemcount = ' , PSI -> .
                            ELEMCOUNT ) ;
                  WRITELN ( TRACEF , 'psi.setmin    = ' , PSI -> .
                            SETMIN ) ;
                  WRITELN ( TRACEF , 'psi.setmax    = ' , PSI -> .
                            SETMAX ) ;
                  WRITELN ( TRACEF , 'psi.rangeerr  = ' , PSI -> .
                            RANGEERR ) ;
                  for I := 1 to SETMAXSIZE do
                    if PSI -> . SETELEMS [ I ] then
                      WRITELN ( TRACEF , 'in set        = ' , PSI -> .
                                SETMIN + I - 1 ) ;
                end (* then *) ;
              if ELT = PTYPE_CHAR then
                BUILD_SETCONST ( FVALU , PSI , ELT )
              else
                BUILD_SETCONST ( FVALU , PSI , NIL ) ;
              if LSP = NIL then
                begin
                  NEW ( LSP , POWER ) ;
                  with LSP -> do
                    begin
                      ERRORFLAG := FALSE ;
                      ELSET := ELT ;
                      FORM := POWER ;
                      SIZE := FVALU . PVAL -> . LENGTH ;
                      ALN := WORDSIZE
                    end (* with *) ;
                  FSP := LSP
                end (* then *) ;
              FVALU . SETTYPE := LSP ;
            end (* then *)
          else
            if SY = SYLPARENT then

        (**************************)
        (*ARRAY OR RECORD CONSTANT*)
        (**************************)

              begin
                INSYMBOL ;
                K := 0 ;
                CHECKSTARTCST ;
                if LSP <> NIL then
                  with LSP -> do
                    if FORM = ARRAYS then
                      begin
                        ALIGN ( CONSTLCOUNTER , ALN ) ;
                        SLC := CONSTLCOUNTER ;
                        J := SLC ;
                        if AELTYPE <> NIL then
                          L := AELTYPE -> . SIZE
                        else
                          L := 1 ;
                        LX := L ;
                        ALIGN ( LX , ALN ) ;
                        L := LX ;
                        TEST := FALSE ;
                        repeat
                          K := K + 1 ;
                          STOWCONST ( AELTYPE ) ;
                          if SY = SYCOMMA then
                            begin
                              INSYMBOL ;
                              J := J + L ;
                              CONSTLCOUNTER := J
                            end (* then *)
                          else
                            TEST := TRUE
                        until TEST ;
                        if SY = SYRPARENT then
                          INSYMBOL
                        else
                          ERROR ( 4 ) ;
                        ERRFLAG := FALSE ;
                        if INXTYPE <> NIL then
                          begin
                            ERRFLAG := INXTYPE -> . ERRORFLAG ;
                            GETBOUNDS ( INXTYPE , I , J ) ;
                            J := J - I + 1
                          end (* then *)
                        else
                          J := SIZE DIV L ;
                        if K <> J then
                          if K > J then
                            begin
                              if not ERRFLAG then
                                ERROR ( 207 )
                            end (* then *)
                          else
                            begin
                              ERRKIND := 'W' ;
                              ERROR ( 306 ) ;
                              if OPT . PRCODE then
                                WRITELN ( PRR , SLC + SIZE - 1 : 1 , MN
                                          [ 70 ] , ' B,0' ) ;

        (********)
        (*DFC   *)
        (********)

                            end (* else *) ;
                        CONSTLCOUNTER := SLC + SIZE ;
                      end (* then *)
                    else
                      if FORM = RECORDS then
                        begin
                          ALIGN ( CONSTLCOUNTER , ALN ) ;
                          SLC := CONSTLCOUNTER ;
                          L := SIZE ;
                          LRECVAR := RECVAR ;
                          TEST := TRUE ;
                          FLDPR := FSTFLD ;
                          10 :
                          while TEST and ( FLDPR <> NIL ) do
                            with FLDPR -> do
                              begin
                                CONSTLCOUNTER := SLC + FLDADDR ;
                                STOWCONST ( IDTYPE ) ;
                                FLDPR := NEXT ;
                                if SY = SYCOMMA then
                                  INSYMBOL
                                else
                                  TEST := FALSE
                              end (* with *) ;
                          if TEST then
                            if LRECVAR <> NIL then

        (****************************)
        (* TAG FIELD VALUE IS NEXT  *)
        (****************************)

                              with LRECVAR -> do
                                if TAGFIELDP <> NIL then
                                  with TAGFIELDP -> do
                                    begin
                                      if NAME <> BLANKID then
                                        begin
                                          CONSTLCOUNTER := SLC +
                                                   FLDADDR ;
                                          STOWCONST ( IDTYPE )
                                        end (* then *)
                                      else
                                        begin
                                          CONSTANT ( FSYS + [ SYCOMMA ,
                                                   SYRPARENT ] , LSP1 ,
                                                   LVALU ) ;
                                          if COMPTYPES ( IDTYPE , LSP1
                                          ) <> 1 then
                                            ERROR ( 145 ) ;
                                        end (* else *) ;
                                      if SY = SYCOMMA then
                                        INSYMBOL
                                      else
                                        TEST := FALSE ;
                                      LSP1 := FSTVAR ;
                                      L := SIZE ;
                                      while LSP1 <> NIL do
                                        with LSP1 -> do
                                          if VARVAL . IVAL = LVALU .
                                          IVAL then
                                            begin
                                              LRECVAR := SUBVAR ;
                                              L := SIZE ;
                                              FLDPR := FSTSUBFLD ;
                                              goto 10
                                            end (* then *)
                                          else
                                            LSP1 := NXTVAR ;
                                    end (* with *) ;
                          CONSTLCOUNTER := SLC + L ;
                          if SY <> SYRPARENT then
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

      var LCP : IDP ;
          LSP : TTP ;
          LVALU : XCONSTANT ;
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
            if SY = SYCOLON then
              begin
                INSYMBOL ;
                EXTUSED := TRUE ;
                TYP ( FSYS + [ SYEQOP ] , LSP , SKLC ) ;
              end (* then *)
            else
              LSP := NIL ;
            if SY = SYEQOP then
              begin
                INSYMBOL ;
                STRUCTCONSTANT ( FSYS + [ SYSEMICOLON ] , LSP , LVALU ,
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
                        SKOWNERPROC := FPROCP -> . CSTNAME ;
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
            if SY = SYSEMICOLON then
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

      var LCP , LCP1 , LCP2 : IDP ;
          LSP : TTP ;
          LSIZE : ADDRRANGE ;
          ERRINFO : CHAR32 ;

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
            if SY = SYEQOP then
              INSYMBOL
            else
              ERROR ( 16 ) ;
            TYP ( FSYS + [ SYSEMICOLON ] , LSP , LSIZE ) ;
            ENTERID ( LCP ) ;
            LCP -> . IDTYPE := LSP ;

        (******************************************************)
        (* has ANY FORWARD REFERENCE BEEN SATISFIED ?         *)
        (******************************************************)

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
            if SY = SYSEMICOLON then
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

        (******************************************************)
        (* show error 117 for unsatisfied FORWARD REFERENCEs  *)
        (******************************************************)

        if FWPTR <> NIL then
          repeat

        (*********************************************)
        (* not needed, when assignment with          *)
        (* differing lengths is fully implemented    *)
        (*********************************************)

            ERRINFO := FWPTR -> . NAME ;
            ERROR_POS ( 'E' , 117 , ERRINFO , SCB . LINENR , SCB .
                        LINEPOS ) ;
            FWPTR := FWPTR -> . NEXT
          until FWPTR = NIL ;
      end (* TYPEDECLARATION *) ;


   procedure VARDECLARATION ;

      var LCP , NXT , NXT1 : IDP ;
          LSP : TTP ;
          LSIZE : ADDRRANGE ;
          LFPTR : FRECPTR ;
          ERRINFO : CHAR32 ;

      begin (* VARDECLARATION *)
        if IS_MODULE and ( LEVEL <= 1 ) then
          ERROR ( 194 ) ;
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
                    VKIND := NORMALVAR ;
                    VLEV := LEVEL ;
                    STKLASS := XAUTO ;
                    VOWNERPROC := ' ' ;
                    SPECIAL := 0 ;
                  end (* with *) ;
                ENTERID ( LCP ) ;
                if NXT1 = NIL then
                  NXT1 := LCP ;

        (***********************************)
        (* BEGINNING OF THIS ROUND         *)
        (***********************************)

                if NXT <> NIL then
                  NXT -> . NEXT := LCP ;

        (***********************************)
        (* LINK TO PREVIOUS CHAIN          *)
        (***********************************)

                NXT := LCP ;
                INSYMBOL ;
              end (* then *)
            else
              ERROR ( 2 ) ;
            if not ( SY in FSYS + [ SYCOMMA , SYCOLON ] + TYPEDELS )
            then
              begin
                ERROR ( 6 ) ;
                SKIP ( FSYS + [ SYCOMMA , SYCOLON , SYSEMICOLON ] +
                       TYPEDELS )
              end (* then *) ;
            TEST := SY <> SYCOMMA ;
            if not TEST then
              INSYMBOL
          until TEST ;
          if SY = SYCOLON then
            INSYMBOL
          else
            ERROR ( 5 ) ;
          TYP ( FSYS + [ SYSEMICOLON ] + TYPEDELS , LSP , LSIZE ) ;
          while NXT1 <> NIL do
            with NXT1 -> do
              begin
                IDTYPE := LSP ;
                if LSP <> NIL then
                  begin
                    ALIGN ( LCOUNTER , LSP -> . ALN ) ;
                    VADDR := LCOUNTER ;
                    LCOUNTER := LCOUNTER + LSIZE ;
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
                  VADDR := LCOUNTER ;
                if OPT . DEBUG_LEV > 0 then
                  PRNTSYMBL ( NXT1 ) ;
                NXT1 := NEXT ;
              end (* with *) ;
          if SY = SYSEMICOLON then
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

        (******************************************************)
        (* show error 117 for unsatisfied FORWARD REFERENCEs  *)
        (******************************************************)

        if FWPTR <> NIL then
          repeat
            ERRINFO := FWPTR -> . NAME ;
            ERROR_POS ( 'E' , 117 , ERRINFO , SCB . LINENR , SCB .
                        LINEPOS ) ;
            FWPTR := FWPTR -> . NEXT
          until FWPTR = NIL ;
      end (* VARDECLARATION *) ;


   procedure STATICDECLARATION ;

      var LCP , NXT , NXT1 : IDP ;
          LSP : TTP ;
          LSIZE : ADDRRANGE ;
          LFPTR : FRECPTR ;
          LVALU_DUMMY : XCONSTANT ;
          ELSIZE : INTEGER ;
          ERRINFO : CHAR32 ;

      begin (* STATICDECLARATION *)
        LVALU_DUMMY . IVAL := 0 ;
        LVALU_DUMMY . STRTYPE := ' ' ;
        LISTTAG := 'S' ;
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
                    VKIND := NORMALVAR ;
                    VLEV := LEVEL ;
                    STKLASS := XSTATIC ;
                    VOWNERPROC := FPROCP -> . CSTNAME ;
                    SPECIAL := 0 ;
                  end (* with *) ;
                ENTERID ( LCP ) ;
                if NXT1 = NIL then
                  NXT1 := LCP ;

        (**********************************)
        (* BEGINNING OF THIS ROUND        *)
        (**********************************)

                if NXT <> NIL then
                  NXT -> . NEXT := LCP ;

        (**********************************)
        (* LINK TO PREVIOUS CHAIN         *)
        (**********************************)

                NXT := LCP ;
                INSYMBOL ;
              end (* then *)
            else
              ERROR ( 2 ) ;
            if not ( SY in FSYS + [ SYCOMMA , SYCOLON ] + TYPEDELS )
            then
              begin
                ERROR ( 6 ) ;
                SKIP ( FSYS + [ SYCOMMA , SYCOLON , SYSEMICOLON ] +
                       TYPEDELS )
              end (* then *) ;
            TEST := SY <> SYCOMMA ;
            if not TEST then
              INSYMBOL
          until TEST ;
          if SY = SYCOLON then
            INSYMBOL
          else
            ERROR ( 5 ) ;
          TYP ( FSYS + [ SYSEMICOLON ] + TYPEDELS , LSP , LSIZE ) ;
          while NXT1 <> NIL do
            with NXT1 -> do
              begin
                IDTYPE := LSP ;
                CHECKSTARTCST ;
                if LSP <> NIL then
                  begin
                    if LSP -> . FORM = FILES then
                      ERROR ( 398 )
                    else
                      begin
                        ALIGN ( CONSTLCOUNTER , LSP -> . ALN ) ;
                        ELSIZE := LSP -> . SIZE ;
                        VADDR := CONSTLCOUNTER ;
                        WRITEDFC ( LSP , ELSIZE , LVALU_DUMMY , TRUE )
                                   ;
                        CONSTLCOUNTER := CONSTLCOUNTER + LSIZE ;
                      end (* else *)
                  end (* then *)
                else
                  VADDR := CONSTLCOUNTER ;
                if OPT . DEBUG_LEV > 0 then
                  PRNTSYMBL ( NXT1 ) ;
                NXT1 := NEXT ;
              end (* with *) ;
          if SY = SYSEMICOLON then
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

        (******************************************************)
        (* show error 117 for unsatisfied FORWARD REFERENCEs  *)
        (******************************************************)

        LISTTAG := ' ' ;
        if FWPTR <> NIL then
          repeat
            ERRINFO := FWPTR -> . NAME ;
            ERROR_POS ( 'E' , 117 , ERRINFO , SCB . LINENR , SCB .
                        LINEPOS ) ;
            FWPTR := FWPTR -> . NEXT
          until FWPTR = NIL ;
      end (* STATICDECLARATION *) ;


   procedure MKNAME ( var ALB : ALPHA ; NLB : INTEGER ; NCFLAG :
                    BOOLEAN ) ;

   /*************************************/
   /* generische Namen fuer Labels usw. */
   /*************************************/


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


   procedure MKPROCNAME ( var NAM : ALPHA ; NAMPAT : ALPHA ; NUM :
                        INTEGER ; INTERN : BOOLEAN ) ;

   //******************************************************
   // this procedure builds                                
   // generic names for internal and external procedures   
   //******************************************************


      var I , J : INTEGER ;

      begin (* MKPROCNAME *)
        J := 8 ;
        if INTERN then
          begin
            NAM := NAMPAT ;
            I := 8 ;
            while NAM [ I ] = ' ' do
              I := I - 1 ;
            I := I + 1 ;
          end (* then *)
        else
          begin
            I := 1 ;
            while I < 9 do
              begin
                if NAM [ I ] = ' ' then
                  break ;
                if NAM [ I ] = '_' then
                  NAM [ I ] := '$' ;
                I := I + 1
              end (* while *)
          end (* else *) ;
        if INTERN then
          for J := 8 DOWNTO I do
            begin
              NAM [ J ] := CHR ( ORD ( '0' ) + NUM MOD 10 ) ;
              NUM := NUM DIV 10
            end (* for *) ;
      end (* MKPROCNAME *) ;


   procedure PROCDECLARATION ( FSY : SYMB ; PLOCAL : BOOLEAN ) ;

   //****************************************************************
   // a procedure or function declaration                            
   // is recognized                                                  
   //****************************************************************


      var LSY : SYMB ;
          LCP , LCP1 , LCP2 : IDP ;
          LSP : TTP ;
          FORW : BOOLEAN ;
          K , PARCNT : INTEGER ;
          OLDLABEL : LABELRNG ;
          LLC , LCM : ADDRRANGE ;
          I , NAME : INTEGER ;
          MARKP : -> INTEGER ;
          OLD_HASH : HASH_TABLE ;
          INTERN : BOOLEAN ;
          CHECKID : ALPHA ;
          ERRINFO : CHAR32 ;


      procedure PARAMETERLIST ( FSY : SYMSET ; FPAR : IDP ; FW :
                              BOOLEAN ) ;

      //**********************************************************
      // a parameter list of a procedure                          
      // or function is handled                                   
      //**********************************************************


         var LCP , LCP1 , LCP2 , LCP3 , LCP4 : IDP ;
             LSP : TTP ;
             LKIND : IDKIND ;
             LLC , LEN , LALN : ADDRRANGE ;
             LSY : SYMB ;


         procedure PROC_PARAMETER ;

         //****************************************************
         // a proc or function parameter is handled            
         //****************************************************


            begin (* PROC_PARAMETER *)
              LSY := SY ;

              (***************************)
              (*REMEMBER IF PROC OR FUNC *)
              (***************************)

              INSYMBOL ;
              if SY = IDENT then
                begin
                  if LSY = SYPROC then
                    NEW ( LCP , PROC , DECLARED )
                  else
                    NEW ( LCP , FUNC , DECLARED ) ;
                  if LCP3 <> NIL then
                    LCP4 -> . NEXT := LCP
                  else
                    LCP3 := LCP ;
                  LCP4 := LCP ;
                  ALIGN ( LCOUNTER , PTRSIZE ) ;
                  with LCP -> do
                    begin
                      NAME := ID ;
                      IDTYPE := NIL ;
                      NEXT := NIL ;
                      PFDECKIND := DECLARED ;
                      PFKIND := VARPARM ;
                      DECLMISSING := FALSE ;
                      EXTRN := FALSE ;
                      EXTLANG := ' ' ;
                      EXTNAME := '*PFPARM*' ;
                      PFLEV := LCOUNTER * 10 + LEVEL ;

              (***************************************)
              (*I.E. PFLEV > LCAFTMST => PROC PARM   *)
              (***************************************)

                      PROCLAB := PROCLAB + 1 ;
                      PFNAME := PROCLAB ;
                      if LSY = SYPROC then
                        KLASS := PROC
                      else
                        KLASS := FUNC ;
                    end (* with *) ;
                  ENTERID ( LCP ) ;
                  LCOUNTER := LCOUNTER + DISPAREA ;
                  INSYMBOL ;
                  LLC := LCOUNTER ;
                  if LSY = SYPROC then
                    PARAMETERLIST ( [ SYSEMICOLON , SYRPARENT ] , LCP ,
                                    FALSE )
                  else
                    PARAMETERLIST ( [ SYSEMICOLON , SYCOLON ] , LCP ,
                                    FALSE ) ;
                  LCOUNTER := LLC ;
                end (* then *)
              else
                ERROR ( 2 ) ;
              if not ( SY in FSYS + [ SYSEMICOLON , SYRPARENT ] ) then
                begin
                  ERROR ( 352 ) ;
                  SKIP ( FSYS + [ SYSEMICOLON , SYRPARENT ] )
                end (* then *)
            end (* PROC_PARAMETER *) ;


         procedure OTHER_PARAMETER ;

         //****************************************************
         // a var, const or normal (value) parameter is handled
         //****************************************************


            begin (* OTHER_PARAMETER *)
              if SY = SYVAR then
                begin
                  LKIND := VARPARM ;
                  INSYMBOL
                end (* then *)
              else
                if SY = SYCONST then
                  begin
                    LKIND := CONSTPARM ;
                    INSYMBOL
                  end (* then *)
                else
                  LKIND := NORMALVAR ;
              repeat
                if SY = IDENT then
                  begin
                    NEW ( LCP , VARS ) ;
                    LCP -> . STKLASS := XAUTO ;
                    LCP -> . VOWNERPROC := ' ' ;
                    LCP -> . SPECIAL := 0 ;
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
                if not ( SY in [ SYCOMMA , SYCOLON ] + FSYS ) then
                  begin
                    ERROR ( 353 ) ;
                    SKIP ( FSYS + [ SYCOMMA , SYSEMICOLON , SYRPARENT ]
                           )
                  end (* then *) ;
                TEST := SY <> SYCOMMA ;
                if not TEST then
                  INSYMBOL
              until TEST ;
              if SY = SYCOLON then
                begin
                  INSYMBOL ;
                  if SY = IDENT then
                    begin
                      SID_RC := SEARCHID ( ID , TRUE , TRUE , [ TYPES ]
                                , LCP4 ) ;
                      LEN := PTRSIZE ;
                      LSP := LCP4 -> . IDTYPE ;
                      LALN := PTRSIZE ;
                      if LSP <> NIL then
                        if ( LKIND = NORMALVAR ) then
                          if LSP -> . FORM = FILES then
                            begin
                              ERROR ( 121 ) ;
                              LKIND := VARPARM
                            end (* then *) ;

              //************************************************
              // read next symbol                               
              // maybe type with parameters                     
              // if so, lsp will be modified                    
              // so loop to modify lcp4 chain                   
              // has to be moved after this                     
              // function call - 13.12.2017                     
              //************************************************
              // len and laln assignment                        
              // has to be moved after parm checking            
              // 17.12.2017                                     
              //************************************************
              // allow conformant strings for var parameters    
              // (see parameter on type_with_parms)             
              // 06.01.2018                                     
              //************************************************

                      INSYMBOL ;
                      TYPE_WITH_PARMS ( FSYS + [ SYSEMICOLON ,
                                        SYRPARENT ] , ID , LSP , (
                                        LKIND in [ VARPARM , CONSTPARM
                                        ] ) ) ;
                      if LSP <> NIL then
                        if ( LKIND = NORMALVAR ) then
                          begin
                            LEN := LSP -> . SIZE ;
                            LALN := LSP -> . ALN
                          end (* then *) ;
                      LCP4 := LCP3 ;
                      while LCP4 <> NIL do
                        begin
                          with LCP4 -> do
                            begin
                              IDTYPE := LSP ;
                              ALIGN ( LCOUNTER , LALN ) ;
                              VADDR := LCOUNTER ;
                              LCOUNTER := LCOUNTER + LEN ;
                            end (* with *) ;
                          LCP4 := LCP4 -> . NEXT
                        end (* while *) ;
                    end (* then *)
                  else
                    ERROR ( 2 ) ;
                  if not ( SY in FSYS + [ SYSEMICOLON , SYRPARENT ] )
                  then
                    begin
                      ERROR ( 352 ) ;
                      SKIP ( FSYS + [ SYSEMICOLON , SYRPARENT ] )
                    end (* then *)
                end (* then *)
              else
                ERROR ( 5 ) ;
            end (* OTHER_PARAMETER *) ;


         begin (* PARAMETERLIST *)
           LCP1 := NIL ;
           LCP := NIL ;
           if not ( SY in FSY + [ SYLPARENT ] ) then
             begin
               ERROR ( 351 ) ;
               SKIP ( FSYS + FSY + [ SYLPARENT ] )
             end (* then *) ;
           if SY = SYLPARENT then
             begin
               if FORW then
                 ERROR ( 119 )
               else
                 LCOUNTER := LCAFTMST + FPSAVEAREA ;
               INSYMBOL ;
               if not ( SY in [ IDENT , SYVAR , SYCONST , SYPROC ,
               SYFUNC ] ) then
                 begin
                   ERROR ( 350 ) ;
                   SKIP ( FSYS + [ IDENT , SYRPARENT ] )
                 end (* then *) ;

           //************************************************
           // a parameter may start with                     
           // - an identifier                                
           // - var or const                                 
           // - procedure or function                        
           //************************************************

               while SY in [ IDENT , SYVAR , SYCONST , SYPROC , SYFUNC
               ] do
                 begin
                   LCP3 := NIL ;
                   if SY in [ SYPROC , SYFUNC ] then
                     PROC_PARAMETER
                   else
                     OTHER_PARAMETER ;
                   if SY = SYSEMICOLON then
                     begin
                       INSYMBOL ;
                       if not ( SY in [ IDENT , SYVAR , SYCONST ,
                       SYPROC , SYFUNC ] ) then
                         begin
                           ERROR ( 350 ) ;
                           if SY = SYCONST then
                             INSYMBOL ;
                           SKIP ( FSYS + [ IDENT , SYRPARENT ] )
                         end (* then *)
                     end (* then *) ;
                   if LCP1 <> NIL then
                     LCP2 -> . NEXT := LCP3
                   else
                     LCP1 := LCP3 ;
                   LCP2 := LCP ;
                 end (* while *) ;
               if SY = SYRPARENT then
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

           (**************************)
           (* IF SY <> SYLPARENT     *)
           (**************************)

             ;
           if not FW then
             FPAR -> . PRMPTR := LCP1 ;
           if FPAR -> . KLASS = FUNC then
             if SY = SYCOLON then
               begin
                 INSYMBOL ;
                 if SY = IDENT then
                   begin
                     if FW then
                       ERROR ( 122 ) ;
                     SID_RC := SEARCHID ( ID , TRUE , TRUE , [ TYPES ]
                               , LCP1 ) ;
                     LSP := LCP1 -> . IDTYPE ;

           //************************************************
           // evtl. result type with parameters              
           // allow conformant strings                       
           //************************************************

                     INSYMBOL ;
                     TYPE_WITH_PARMS ( FSYS + [ SYSEMICOLON ] , ID ,
                                       LSP , TRUE ) ;

           //************************************************
           // check if type is ok                            
           // don't accept char with parameter               
           // and varchar/string with length                 
           // accept decimal type                            
           //************************************************

                     FPAR -> . IDTYPE := LSP ;
                     if LSP <> NIL then
                       if LSP -> . FORM >= POWER then
                         if LSP -> . FORM = CSTRING then
                           begin
                             if not LSP -> . CONFORMANT then
                               begin
                                 ERROR ( 345 ) ;
                                 FPAR -> . IDTYPE := NIL
                               end (* then *) ;
                           end (* then *)
                         else
                           begin
                             ERROR ( 120 ) ;
                             FPAR -> . IDTYPE := NIL
                           end (* else *) ;
                   end (* then *)
                 else
                   begin
                     ERROR ( 2 ) ;
                     SKIP ( FSYS + [ SYSEMICOLON ] )
                   end (* else *)
               end (* then *)
             else
               if not FW then
                 ERROR ( 123 )
         end (* PARAMETERLIST *) ;


      begin (* PROCDECLARATION *)
        LLC := LCOUNTER ;
        LCOUNTER := LCAFTMST + FPSAVEAREA ;

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
                  break
                end (* then *)
              else
                begin
                  LCP2 := LCP ;
                  LCP := LCP -> . NXTFWRD
                end (* else *) ;
            if not FORW then
              begin
                if FSY = SYPROC then
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
                    PFKIND := NORMALVAR ;
                    PFNAME := PROCLAB ;
                    INTERN := ( not IS_MODULE ) or ( LEVEL > 1 ) or (
                              PLOCAL ) ;
                    MKPROCNAME ( ID , NAME_PATTERN , PFNAME , INTERN )
                                 ;
                    EXTRN := not INTERN ;
                    EXTLANG := ' ' ;
                    PACK ( ID , 1 , EXTNAME ) ;
                    FWDECL := FALSE ;
                    DECLMISSING := FALSE ;
                    PROC_TO_STATNAME ( EXTNAME , EXTRN , CSTNAME ) ;
                    if FSY = SYPROC then
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
                            if VKIND in [ VARPARM , CONSTPARM ] then
                              LCM := VADDR + PTRSIZE
                            else
                              LCM := VADDR + IDTYPE -> . SIZE ;
                            if LCM > LCOUNTER then
                              LCOUNTER := LCM
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
        if OPT . GET_STAT then
          PROC_CNT [ LEVEL ] := PROC_CNT [ LEVEL ] + 1 ;
        if FSY = SYPROC then
          PARAMETERLIST ( [ SYSEMICOLON ] , LCP , FORW )
        else
          PARAMETERLIST ( [ SYSEMICOLON , SYCOLON ] , LCP , FORW ) ;
        LCP -> . FWDECL := FALSE ;
        if SY = SYSEMICOLON then
          INSYMBOL
        else
          ERROR ( 14 ) ;
        if SY in [ SYFORWARD , SYFRTRN , SYEXTRN ] then
          begin
            if SY = SYFORWARD then
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

        (**********************************)
        (* SY MUST BE SYFRTRN OR SYEXTRN  *)
        (**********************************)

                with LCP -> do
                  begin
                    if SY = SYFRTRN then
                      LCP -> . EXTLANG := 'F'
                    else
                      LCP -> . EXTRN := TRUE ;
                    INSYMBOL ;
                    if SY = SYFRTRN then
                      begin
                        if LCP -> . EXTRN then
                          begin
                            LCP -> . EXTRN := FALSE ;
                            LCP -> . EXTLANG := 'F'
                          end (* then *)
                        else
                          ERROR ( 14 ) ;
                        INSYMBOL ;
                      end (* then *)
                    else
                      if SY = IDENT then
                        begin
                          if LCP -> . EXTRN then
                            begin
                              CHECKID := ID ;
                              if CHECKID = 'PASCAL' then
                                LCP -> . EXTLANG := ' '
                              else
                                if CHECKID = 'ASSEMBLER' then
                                  LCP -> . EXTLANG := 'A'
                                else
                                  begin
                                    ERRINFO := CHECKID ;
                                    ERROR_POS ( 'E' , 321 , ERRINFO ,
                                                SCB . LINENR , SCB .
                                                LINEPOS ) ;
                                  end (* else *)
                            end (* then *)
                          else
                            ERROR ( 14 ) ;
                          INSYMBOL ;
                        end (* then *) ;
                    if SY = STRINGCONST then
                      with VAL . SVAL -> do
                        begin
                          while LENGTH < EXTNAMSZ do
                            begin
                              LENGTH := LENGTH + 1 ;
                              SSTR [ LENGTH ] := ' '
                            end (* while *) ;
                          PACK ( SSTR , 1 , EXTNAME ) ;
                          PROC_TO_STATNAME ( EXTNAME , EXTRN , CSTNAME
                                             ) ;
                          INSYMBOL
                        end (* with *)
                    else
                      begin
                        PACK ( NAME , 1 , EXTNAME ) ;
                        PROC_TO_STATNAME ( EXTNAME , EXTRN , CSTNAME )
                                           ;
                      end (* else *)
                  end (* with *)
              end (* else *) ;
            if SY = SYSEMICOLON then
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
            if OPT . DEBUG_LEV > 0 then
              PRNTSYMBL ( LCP ) ;
            MARK ( MARKP ) ;

        (*****************************)
        (* MARK HEAP FOR BLOCK ENTRY *)
        (*****************************)

            repeat
              BLOCK ( FSYS , SYSEMICOLON , LCP ) ;
              if SY = SYSEMICOLON then
                begin
                  INSYMBOL ;
                  if not ( SY in [ SYBEGIN , SYPROC , SYFUNC , SYLOCAL
                  ] ) then
                    begin
                      ERROR ( 6 ) ;
                      SKIP ( FSYS )
                    end (* then *)
                end (* then *)
              else
                ERROR ( 14 )
            until SY in [ SYBEGIN , SYPROC , SYFUNC , SYLOCAL ] ;
            RELEASE ( MARKP ) ;

        (****************************************)
        (* RETURN LOCAL ENTRIES ON RUNTIME HEAP *)
        (****************************************)

          end (* else *) ;
        LEVEL := LEVEL - 1 ;
        TOP := LEVEL ;
        LCOUNTER := LLC ;
        INTLABEL := OLDLABEL ;
        BUCKET := OLD_HASH ;

        (**********************)
        (*RESTORE SYMBOL TABLE*)
        (**********************)

      end (* PROCDECLARATION *) ;


   function PROCTYPE ( FPROCP : IDP ) : CHAR ;

      begin (* PROCTYPE *)
        PROCTYPE := 'P' ;
        if FPROCP = NIL then
          return ;
        if FPROCP -> . IDTYPE = NIL then
          return ;
        with FPROCP -> do
          begin
            if IS_STDTYPE ( IDTYPE , 'R' ) then
              PROCTYPE := 'R'
            else
              if IDTYPE = PTYPE_BOOL then
                PROCTYPE := 'B'
              else
                if IDTYPE -> . FORM = POINTER then
                  PROCTYPE := 'A'
                else
                  if IDTYPE -> . FORM = CSTRING then
                    PROCTYPE := 'V'
                  else
                    if IDTYPE -> . SIZE = 1 then
                      PROCTYPE := 'C'
                    else
                      if IDTYPE -> . SIZE = HINTSIZE then
                        PROCTYPE := 'H'
                      else
                        PROCTYPE := 'I' ;
          end (* with *)
      end (* PROCTYPE *) ;


   procedure BODY ( FSYS : SYMSET ; STRING_RESULT : BOOLEAN ) ;

      type CALLED_PROC = record
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

          DDNAME : XCONSTANT ;
          CNSTPTR : CONSTP ;
          I : INTEGER ;
          LCMAX , LLC1 : ADDRRANGE ;
          LCP , LLCP : IDP ;
          LLP : LBP ;
          FIRSTLN : INTEGER ;
          CTRNO : CTRRANGE ;
          LOOP0 : LOOPCTL ;
          SUBR : SUBRCTL ;
          LRETURN : LABELRNG ;
          CSTEXTNAME : EXTNAMTP ;
          XCSP : CSPTYPE ;
          ERRINFO : CHAR32 ;


      procedure GEN0 ( FOP : OPRANGE ) ;

         begin (* GEN0 *)
           if OPT . PRCODE then
             begin
               PUTIC ;
               WRITELN ( PRR , MN [ FOP ] : 4 )
             end (* then *) ;
           ICOUNTER := ICOUNTER + 1
         end (* GEN0 *) ;


      procedure GEN_LCA_S ( ELTYPE : TTP ; VAL : XCONSTANT ) ;

         begin (* GEN_LCA_S *)
           if OPT . PRCODE then
             begin
               PUTIC ;
               WRITE ( PRR , MN [ PCODE_LCA ] : 4 ) ;
               WRITE ( PRR , ' S,' ) ;
               WRITESET ( VAL , ELTYPE ) ;
             end (* then *) ;
           ICOUNTER := ICOUNTER + 1
         end (* GEN_LCA_S *) ;


      procedure GEN_LCA_M ( VAL : XCONSTANT ) ;

         begin (* GEN_LCA_M *)
           if OPT . PRCODE then
             begin
               PUTIC ;
               WRITE ( PRR , MN [ PCODE_LCA ] : 4 ) ;
               WRITE ( PRR , ' M,' ) ;
               GEN_STRCONST ( VAL ) ;
               STRCOUNTER := STRCOUNTER + VAL . SVAL -> . LENGTH ;
               WRITELN ( PRR ) ;
             end (* then *) ;
           ICOUNTER := ICOUNTER + 1
         end (* GEN_LCA_M *) ;


      procedure GEN1 ( FOP : OPRANGE ; FP2 : INTEGER ) ;

         var K : INTEGER ;
             LCCALLER : ADDRRANGE ;

         begin (* GEN1 *)
           if OPT . PRCODE then
             begin
               PUTIC ;
               WRITE ( PRR , MN [ FOP ] : 4 ) ;
               case FOP of
                 PCODE_CSP :
                   begin
                     WRITE ( PRR , CSPNAME [ FP2 ] : 4 ) ;
                     LCCALLER := LCOUNTER ;
                     ALIGN ( LCCALLER , MXDATASZE ) ;
                     WRITELN ( PRR , ',' , LCCALLER : 1 ) ;
                   end (* tag/ca *) ;

           (************************************************)
           (* LCA P = LOAD PROCEDURE ADDRESS               *)
           (************************************************)

                 PCODE_LCA :
                   begin
                     if FP2 = ORD ( 'P' ) then
                       WRITELN ( PRR , ' P,' , ID : EXTNAMSZ )
                   end (* tag/ca *) ;
                 PCODE_STO , PCODE_RET :
                   begin
                     WRITELN ( PRR , CHR ( FP2 ) : 2 )
                   end (* tag/ca *) ;
                 otherwise
                   begin
                     WRITELN ( PRR , ' ' , FP2 : 1 )
                   end (* otherw *)
               end (* case *)
             end (* then *) ;
           ICOUNTER := ICOUNTER + 1
         end (* GEN1 *) ;


      procedure GEN2 ( FOP : OPRANGE ; FP1 , FP2 : INTEGER ) ;

         var I , J , K : INTEGER ;

         begin (* GEN2 *)
           if OPT . PRCODE then
             begin
               PUTIC ;
               WRITE ( PRR , MN [ FOP ] : 4 , ' ' ) ;
               case FOP of

           //************************************************
           // type (char) and integer parameter              
           //************************************************

                 PCODE_DEC , PCODE_INC , PCODE_IND , PCODE_CTI ,
                 PCODE_STP :
                   WRITELN ( PRR , CHR ( FP1 ) , ',' , FP2 : 1 ) ;

           //************************************************
           // two integer parameters (level and address      
           // or two lengths or ...)                         
           //************************************************

                 PCODE_SCL , PCODE_MST , PCODE_LDA , PCODE_SLD ,
                 PCODE_SMV , PCODE_NEW , PCODE_VST , PCODE_VPU ,
                 PCODE_VPO , PCODE_VLD :
                   WRITELN ( PRR , FP1 : 1 , ',' , FP2 : 1 ) ;

           //************************************************
           // logical instructions                           
           //************************************************

                 PCODE_AND , PCODE_IOR , PCODE_NOT , PCODE_XOR :
                   WRITELN ( PRR , CHR ( FP1 ) ) ;

           //************************************************
           // compare instructions                           
           //************************************************

                 PCODE_EQU , PCODE_GEQ , PCODE_GRT , PCODE_LEQ ,
                 PCODE_LES , PCODE_NEQ :
                   begin
                     WRITE ( PRR , CHR ( FP1 ) ) ;
                     if FP1 = ORD ( 'M' ) then
                       WRITE ( PRR , ',' , FP2 : 1 ) ;
                     WRITELN ( PRR )
                   end (* tag/ca *) ;

           //************************************************
           // load constant, different formats depending on  
           // type (first parameter)                         
           //************************************************

                 PCODE_LDC :
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
                       ERROR ( 412 ) ;
                   end (* case *)
               end (* case *) ;
             end (* then *) ;
           ICOUNTER := ICOUNTER + 1
         end (* GEN2 *) ;


      procedure GEN3 ( FOP : OPRANGE ; FP0 , FP1 , FP2 : INTEGER ) ;

         begin (* GEN3 *)
           if OPT . PRCODE then
             begin
               PUTIC ;
               if FOP = PCODE_PAK

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
           ICOUNTER := ICOUNTER + 1
         end (* GEN3 *) ;


      procedure CHECK_CTLS ( MODE : INTEGER ) ;

         begin (* CHECK_CTLS *)
           case MODE of
             1 : begin
                   GEN2 ( PCODE_VPU , CTLS . VPU1_LEVEL , CTLS .
                          VPU1_OFFSET ) ;
                 end (* tag/ca *) ;
             2 : begin
                   GEN2 ( PCODE_VPU , CTLS . VPU2_LEVEL , CTLS .
                          VPU2_OFFSET ) ;
                   CTLS . VPU2_DONE := TRUE ;
                 end (* tag/ca *) ;
             otherwise
               begin
                 
               end (* otherw *)
           end (* case *)
         end (* CHECK_CTLS *) ;


      procedure RESOLVE_CTLS ( MODE : INTEGER ) ;

         begin (* RESOLVE_CTLS *)
           case MODE of
             1 : begin
                   if CTLS . VPO1_NEEDED then
                     begin
                       GEN2 ( PCODE_VPO , CTLS . VPU1_LEVEL , CTLS .
                              VPU1_OFFSET ) ;
                       CTLS . VPO1_NEEDED := FALSE ;
                     end (* then *) ;
                   CTLS . WATCH1 := FALSE ;
                 end (* tag/ca *) ;
             2 : begin
                   if CTLS . VPU2_DONE then
                     begin
                       GEN2 ( PCODE_VPO , CTLS . VPU2_LEVEL , CTLS .
                              VPU2_OFFSET ) ;
                     end (* then *) ;
                 end (* tag/ca *) ;
             otherwise
               begin
                 
               end (* otherw *)
           end (* case *)
         end (* RESOLVE_CTLS *) ;


      procedure LOAD ;

         begin (* LOAD *)
           with GATTR do
             if TYPTR <> NIL then
               begin
                 case KIND of
                   CST : if ( TYPTR -> . FORM = SCALAR ) and not
                         IS_STDTYPE ( TYPTR , 'R' ) then
                           if TYPTR = PTYPE_BOOL then
                             GEN2 ( PCODE_LDC , 3 , CVAL . IVAL )
                           else
                             if TYPTR = PTYPE_CHAR then

           (*******************************************************)
           (* LDC - if char constant has been coded in binary     *)
           (*       or hex, it will be presented in integer       *)
           (*       format to the LDC P-Code operation - 06.2017  *)
           (*******************************************************)

                               if CVAL . STRTYPE in [ 'X' , 'B' ] then
                                 GEN2 ( PCODE_LDC , 1 , CVAL . IVAL )
                               else
                                 GEN2 ( PCODE_LDC , 0 , CVAL . IVAL )
                             else

           (************************************************)
           (* INTEGER                                      *)
           (************************************************)

                               GEN2 ( PCODE_LDC , 1 , CVAL . IVAL )
                         else
                           if TYPTR = PTYPE_ANY then
                             GEN2 ( PCODE_LDC , 4 , 0 )
                           else
                             begin
                               CNSTPTR := ADDR ( CVAL ) ;
                               if IS_STDTYPE ( TYPTR , 'R' ) then
                                 GEN2 ( PCODE_LDC , 2 , 0 )
                               else
                                 GEN2 ( PCODE_LDC , 5 , 0 )
                             end (* else *) ;
                   VARBL : case ACCESS of
                             DRCT : GEN3 ( PCODE_LOD , GETTYPE ( BTYPE
                                           ) , VLEVEL , DPLMT ) ;
                             INDRCT :
                               GEN2 ( PCODE_IND , GETTYPE ( BTYPE ) ,
                                      IDPLMT ) ;
                             INXD , STKEXPR :
                               ERROR ( 413 )
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
                 DRCT : GEN3 ( PCODE_STR , GETTYPE ( BTYPE ) , VLEVEL ,
                               DPLMT ) ;
                 INDRCT :
                   if IDPLMT <> 0 then
                     ERROR ( 414 )
                   else
                     GEN1 ( PCODE_STO , GETTYPE ( BTYPE ) ) ;
                 INXD , STKEXPR :
                   ERROR ( 415 )
               end (* case *)
         end (* STORE *) ;


      procedure LOADADDRESS ;

         begin (* LOADADDRESS *)
           with GATTR do
             if TYPTR <> NIL then
               begin
                 case KIND of
                   CST : if IS_CARRAY ( TYPTR ) then
                           GEN_LCA_M ( CVAL )
                         else
                           if TYPTR -> . FORM = POWER then
                             GEN_LCA_S ( TYPTR -> . ELSET , CVAL )
                           else
                             ERROR ( 416 ) ;
                   VARBL : case ACCESS of
                             DRCT : GEN2 ( PCODE_LDA , VLEVEL , DPLMT )
                                           ;
                             INDRCT :
                               if IDPLMT <> 0 then
                                 GEN2 ( PCODE_INC , ORD ( 'A' ) ,
                                        IDPLMT ) ;
                             INXD : ERROR ( 417 ) ;
                             STKEXPR :
                               ;

           (***************************************)
           (*SET ALREADY REPRESENTED BY AN ADDRESS*)
           (***************************************)

                           end (* case *) ;
                   EXPR : ERROR ( 418 )
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
             if GATTR . TYPTR <> PTYPE_BOOL then
               ERROR ( 144 ) ;
           if OPT . PRCODE then
             begin
               PUTIC ;
               WRITELN ( PRR , MN [ 33 ] : 4 , ' L' , FADDR : 1 )
             end (* then *) ;
           ICOUNTER := ICOUNTER + 1
         end (* GENFJP *) ;


      procedure GENUJPFJP ( FOP : OPRANGE ; FP2 : INTEGER ) ;

         begin (* GENUJPFJP *)
           if OPT . PRCODE then
             begin
               PUTIC ;
               if FOP = 44 then
                 begin
                   if PORTABLE_BRANCHTABLE then
                     WRITELN ( PRR , MN [ FOP ] : 4 , ' N,L' , FP2 : 1
                               )
                   else
                     WRITELN ( PRR , MN [ FOP ] : 4 , ' L' , FP2 : 1 )
                 end (* then *)
               else
                 WRITELN ( PRR , MN [ FOP ] : 4 , ' L' , FP2 : 1 )
             end (* then *) ;
           ICOUNTER := ICOUNTER + 1
         end (* GENUJPFJP *) ;


      procedure GENDEF ( LABELNR : ADDRRANGE ; TYP : CHAR ; WERT :
                       ADDRRANGE ) ;

         begin (* GENDEF *)
           if OPT . PRCODE then
             begin
               PUTIC ;

           (********)
           (*DEF   *)
           (********)

               if LABELNR > 0 then
                 WRITE ( PRR , 'L' , LABELNR : 1 ) ;
               WRITE ( PRR , MN [ 63 ] , ' ' ) ;
               WRITE ( PRR , TYP , ',' ) ;
               case TYP of
                 'I' : WRITE ( PRR , WERT : 1 ) ;
                 'C' : WRITE ( PRR , '''' , CHR ( WERT ) , '''' ) ;
               end (* case *) ;
               WRITELN ( PRR ) ;
             end (* then *)
         end (* GENDEF *) ;


      procedure CHKBNDS ( FSP : TTP ) ;

         var LMIN , LMAX : INTEGER ;

         begin (* CHKBNDS *)
           if FSP <> NIL then
             if FSP <> PTYPE_BOOL then
               if FSP <> PTYPE_INT then
                 if not IS_STDTYPE ( FSP , 'R' ) then

           (*********************************************)
           (* typ = voidptr rausnehmen                  *)
           (* opp 09.2016                               *)
           (*********************************************)

                   if FSP <> PTYPE_ANY then
                     if FSP -> . FORM <= POINTER then

           (*********************************************)
           (* LMAX <= LMIN (?)                          *)
           (* diese checks nicht mehr, weil pointer     *)
           (* nicht nur heap-pointer sein koennen       *)
           (* siehe pasmonn / $ptrchk                   *)
           (* opp 09.2016                               *)
           (*********************************************)

                       if FSP -> . FORM = POINTER then
                         begin
                           if FALSE then
                             begin
                               FLIPDEBUG := TRUE ;
                               if ASSIGN then
                                 GEN3 ( PCODE_CHK , ORD ( 'A' ) , - 1 ,
                                        0 )
                               else
                                 GEN3 ( PCODE_CHK , ORD ( 'A' ) , 0 , 0
                                        )
                             end (* then *)
                         end (* then *)
                       else
                         begin
                           GETBOUNDS ( FSP , LMIN , LMAX ) ;
                           GEN3 ( PCODE_CHK , ORD ( 'I' ) , LMIN , LMAX
                                  ) ;
                         end (* else *) ;
         end (* CHKBNDS *) ;


      procedure PUTLABEL ( LABNAME : INTEGER ) ;

         begin (* PUTLABEL *)
           if OPT . PRCODE then

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
           if OPT . CTROPTION then
             begin
               GEN1 ( PCODE_CTI , CTRCNT ) ;
               CTRCNT := CTRCNT + 1 ;
             end (* then *) ;
         end (* CTRGEN *) ;


      procedure CTREMIT ( CTRT : CTRTYPE ; CTRNO : CTRRANGE ; FLN , MLN
                        , LLN : INTEGER ) ;

      (**************************************************)
      (* WRITE AN ENTRY DESCRIBING A STATEMENT COUNTER. *)
      (* R. L. SITES  3 AUG 77                          *)
      (**************************************************)


         begin (* CTREMIT *)
           if OPT . CTROPTION then
             WRITELN ( DBGINFO , '#CTR    ' , ORD ( CTRT ) : 4 , CTRNO
                       : 6 , FLN : 7 , MLN : 7 , LLN : 7 ) ;
         end (* CTREMIT *) ;


      procedure STATEMENT ( FSYS : SYMSET ; var LOOPC : LOOPCTL ; var
                          SUBR : SUBRCTL ) ;

         label 1 ;

         var LCP : IDP ;
             LLP : LBP ;
             TTOP : DISPRANGE ;
             XLABEL : ALPHA ;
             CTRNO : CTRRANGE ;
             SID_RC : INTEGER ;
             STARTID : ALPHA ;
             ERRLINE_SAVE : INTEGER ;
             ERRPOS_SAVE : INTEGER ;


         procedure EXPRESSION ( FSYS : SYMSET ) ;

            FORWARD ;


         procedure FORCETEMPSET ;

         (**************************************)
         (* "LOADS" CURRENT SET ONTO RUN-STACK *)
         (**************************************)


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
                          return ;
                      LSIZE := OPNDSETSIZE ( GATTR ) ;
                      ALIGN ( LCOUNTER , WORDSIZE ) ;
                      LOADADDRESS ;
                      GEN2 ( PCODE_SLD , LSIZE , LCOUNTER ) ;
                      KIND := VARBL ;
                      ACCESS := STKEXPR ;
                      STKLEN := LSIZE ;
                      STKDPLMT := LCOUNTER ;
                      LCOUNTER := LCOUNTER + LSIZE ;
                      if LCOUNTER > LCMAX then
                        LCMAX := LCOUNTER ;
                    end (* then *) ;
            end (* FORCETEMPSET *) ;


         procedure SELECTOR ( FSYS : SYMSET ; FCP : IDP ; GEN : BOOLEAN
                            ; var IS_FUNCRES : BOOLEAN ) ;

            var LATTR : ATTR ;
                LCP : IDP ;
                LMIN , LMAX : INTEGER ;
                FACT_ERR : BOOLEAN ;

            begin (* SELECTOR *)
              IS_FUNCRES := FALSE ;
              FACT_ERR := ( FCP -> . IDTYPE = NIL ) ;

              /*************************************/
              /* if there were previous errors     */
              /* (for example "identifier not      */
              /* found") fcp->.idtype is nil.      */
              /* to prevent more errors, the       */
              /* nil pointer in fcp->.idtype is    */
              /* promoted to gattr.typtr           */
              /*************************************/
              /* opp - 10.2017                     */
              /*************************************/

              with FCP -> , GATTR do
                begin
                  TYPTR := IDTYPE ;
                  BTYPE := TYPTR ;
                  KIND := VARBL ;
                  case KLASS of

              (****************************************************)
              (*   Erweiterung am 26.10.2016:                     *)
              (*   fuer den zugriff auf statische variablen       *)
              (*   - erkennbar an stklass = xstatic -             *)
              (*   wurde die logik aus dem zweig                  *)
              (*   fuer strukturierte Konstanten uebernommen;     *)
              (*   die ID des Owners kommt aus VOWNER;            *)
              (*   die VARS-Struktur wurde entsprechend           *)
              (*   erweitert                                      *)
              (****************************************************)

                    VARS : if STKLASS = XAUTO then
                             begin
                               if VKIND = NORMALVAR then
                                 begin
                                   ACCESS := DRCT ;
                                   VLEVEL := VLEV ;
                                   DPLMT := VADDR
                                 end (* then *)
                               else
                                 begin
                                   if GEN then
                                     GEN3 ( PCODE_LOD , ORD ( 'A' ) ,
                                            VLEV , VADDR ) ;
                                   ACCESS := INDRCT ;
                                   IDPLMT := 0
                                 end (* else *)
                             end (* then *)
                           else
                             begin
                               ID := ' ' ;
                               UNPACK ( VOWNERPROC , ID , 1 ) ;
                               if GEN then
                                 GEN1 ( PCODE_LCA , ORD ( 'P' ) ) ;
                               ACCESS := INDRCT ;
                               VLEVEL := VLEV ;
                               IDPLMT := VADDR
                             end (* else *) ;

              //******************************************
              // access to fields of structs              
              //******************************************

                    FIELD : with DISPLAY [ DISX ] do
                              if OCCUR = CREC then
                                begin
                                  ACCESS := DRCT ;
                                  VLEVEL := CLEV ;
                                  DPLMT := CDSPL + FLDADDR
                                end (* then *)
                              else
                                begin
                                  if GEN then
                                    GEN3 ( PCODE_LOD , ORD ( 'A' ) ,
                                           LEVEL , VDSPL ) ;
                                  ACCESS := INDRCT ;
                                  IDPLMT := FLDADDR
                                end (* else *) ;

              //******************************************
              // access to structured consts              
              // locate static csects using P-Code LCA P  
              // access is indirect                       
              //******************************************

                    STRUCTKONST :
                      begin
                        ID := ' ' ;
                        UNPACK ( SKOWNERPROC , ID , 1 ) ;
                        if GEN then
                          GEN1 ( PCODE_LCA , ORD ( 'P' ) ) ;
                        ACCESS := INDRCT ;
                        IDPLMT := SKADDR
                      end (* tag/ca *) ;

              //******************************************
              // may be assignment to function result     
              //******************************************

                    FUNC : if FCP <> UFCTPTR then
                             if PFDECKIND = STANDARD then
                               ERROR ( 150 )
                             else
                               if PFLEV = 0 then
                                 ERROR ( 150 )

              //******************************************
              // EXTERNAL Function                        
              //******************************************

                               else
                                 if PFKIND = VARPARM then
                                   ERROR ( 151 )
                                 else
                                   if ( FPROCP <> FCP ) then
                                     ERROR ( 177 )
                                   else
                                     begin
                                       ACCESS := DRCT ;
                                       VLEVEL := PFLEV + 1 ;
                                       DPLMT := FNCRSLT ;
                                       IS_FUNCRES := TRUE ;

              //******************************************
              // relative addr. of function result (= 72) 
              //******************************************

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
                  if SY = SYLPARENT then

              //******************************************
              // THIS IS AN ERROR, BUT ..                 
              //******************************************

                    begin
                      SY := SYLBRACK ;
                      if GATTR . TYPTR <> NIL then
                        if GATTR . TYPTR -> . FORM = ARRAYS then
                          begin
                            ERRKIND := 'W' ;
                            ERROR ( 11 )
                          end (* then *)
                    end (* then *) ;

              //******************************************
              // LEFT BRACKET                             
              //******************************************

                  if SY = SYLBRACK then
                    begin
                      repeat
                        LATTR := GATTR ;

              //******************************************
              // new in 2018.02 version:                  
              // form of variable to be indexed           
              // can be array or cstring                  
              //******************************************

                        with LATTR do
                          if TYPTR <> NIL then
                            if not ( TYPTR -> . FORM in [ ARRAYS ,
                            CSTRING ] ) then
                              begin
                                ERROR ( 138 ) ;
                                TYPTR := NIL
                              end (* then *) ;
                        LOADADDRESS ;
                        INSYMBOL ;

              //******************************************
              // expression for index                     
              //******************************************

                        EXPRESSION ( FSYS + [ SYCOMMA , SYRBRACK ,
                                     SYRPARENT ] ) ;
                        LOAD ;
                        if GATTR . TYPTR <> NIL then
                          if GATTR . TYPTR -> . FORM <> SCALAR then
                            ERROR ( 113 ) ;
                        if LATTR . TYPTR <> NIL then
                          with LATTR . TYPTR -> do
                            if FORM = ARRAYS then
                              begin

              //******************************************
              // handle index for arrays                  
              //******************************************

                                if COMPTYPES ( INXTYPE , GATTR . TYPTR
                                ) <> 1 then
                                  begin
                                    if FALSE then
                                      if INXTYPE <> NIL then
                                        if INXTYPE -> . FORM = SUBRANGE
                                        then
                                          begin
                                            WRITELN ( TRACEF ,
                                                   'error 139/1: '
                                                'inxtype.rangetype = '
                                                   , INXTYPE -> .
                                                   RANGETYPE ) ;
                                            WRITELN ( TRACEF ,
                                                   'error 139/1: '
                                                   'inxtype.min = ' ,
                                                   INXTYPE -> . MIN .
                                                   IVAL ) ;
                                            WRITELN ( TRACEF ,
                                                   'error 139/1: '
                                                   'inxtype.max = ' ,
                                                   INXTYPE -> . MAX .
                                                   IVAL ) ;
                                            WRITELN ( TRACEF ,
                                                   'error 139/1: '
                                                'inxtype.errorflag = '
                                                   , INXTYPE -> .
                                                   ERRORFLAG ) ;
                                          end (* then *) ;
                                    if not INXTYPE -> . ERRORFLAG then
                                      ERROR ( 139 )
                                  end (* then *)
                                else
                                  begin
                                    if INXTYPE <> NIL then
                                      begin
                                        GETBOUNDS ( INXTYPE , LMIN ,
                                                   LMAX ) ;
                                        if OPT . DEBUG then
                                          if GEN then
                                            GEN3 ( PCODE_CHK , ORD (
                                                   'J' ) , LMIN , LMAX
                                                   ) ;
                                        if LMIN > 0 then
                                          begin
                                            if GEN then
                                              GEN2 ( PCODE_DEC ,
                                                   GETTYPE ( GATTR .
                                                   TYPTR ) , LMIN )
                                          end (* then *)
                                        else
                                          if LMIN < 0 then
                                            begin
                                              if GEN then
                                                GEN2 ( PCODE_INC ,
                                                   GETTYPE ( GATTR .
                                                   TYPTR ) , - LMIN )
                                            end (* then *)
                                      end (* then *)
                                  end (* else *) ;
                                with GATTR do
                                  begin
                                    TYPTR := AELTYPE ;
                                    KIND := VARBL ;
                                    ACCESS := INDRCT ;
                                    IDPLMT := 0 ;
                                    if TYPTR <> NIL then
                                      begin
                                        LMIN := TYPTR -> . SIZE ;
                                        ALIGN ( LMIN , TYPTR -> . ALN )
                                                ;
                                        if GEN then
                                          GEN1 ( PCODE_IXA , LMIN )
                                      end (* then *) ;
                                  end (* with *) ;
                              end (* then *)
                            else

              //******************************************
              // handle index for cstring                 
              //******************************************

                              with GATTR do
                                begin
                                  if COMPTYPES ( PTYPE_INT , GATTR .
                                  TYPTR ) <> 1 then
                                    ERROR ( 139 ) ;
                                  GEN0 ( PCODE_VIX ) ;
                                  TYPTR := PTYPE_CHAR ;
                                  KIND := VARBL ;
                                  ACCESS := INDRCT ;
                                  IDPLMT := 0 ;
                                end (* with *)
                      until SY <> SYCOMMA ;
                      if SY = SYRBRACK then
                        INSYMBOL
                      else
                        begin
                          if SY = SYRPARENT then
                            begin
                              ERRKIND := 'W' ;
                              INSYMBOL
                            end (* then *) ;
                          ERROR ( 12 )
                        end (* else *) ;
                    end (* then *)
                  else

              //******************************************
              // SYPERIOD                                 
              //******************************************

                    if SY = SYPERIOD then
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
                                              ERROR ( 419 )
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

              //******************************************
              // POINTER SYMBOL aka arrow                 
              //******************************************

                      begin
                        if GATTR . TYPTR <> NIL then
                          with GATTR , TYPTR -> do
                            if FORM = POINTER then
                              begin

              //******************************************
              // error if eltype = NIL, that is:          
              // anyptr - no deref allowed                
              //******************************************

                                if ELTYPE = NIL then
                                  ERROR ( 187 ) ;
                                LOAD ;
                                if OPT . DEBUG then
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
                                  if FILTYPE = NIL then
                                    ERROR ( 188 ) ;
                                  TYPTR := FILTYPE ;
                                  case ACCESS of
                                    DRCT : DPLMT := DPLMT + FILHDRSIZE
                                                   ;
                                    INDRCT :
                                      IDPLMT := IDPLMT + FILHDRSIZE ;
                                    INXD , STKEXPR :
                                      ERROR ( 420 )
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
              if FACT_ERR then
                GATTR . TYPTR := NIL ;
            end (* SELECTOR *) ;


         procedure CALL ( FSYS : SYMSET ; FCP : IDP ) ;

            var LKEY : INTEGER ;
                MATCHPAR : BOOLEAN ;
                RWFILE : TTP ;
                LCCALLER : ADDRRANGE ;
                LCPARM : ADDRRANGE ;
                LCWORK : ADDRRANGE ;


            procedure VARIABLE ( FSYS : SYMSET ; GEN : BOOLEAN ) ;

               var LCP : IDP ;
                   DUMMYB : BOOLEAN ;

               begin (* VARIABLE *)
                 if SY = IDENT then
                   begin
                     SID_RC := SEARCHID ( ID , TRUE , TRUE , [ VARS ,
                               FIELD ] , LCP ) ;
                     INSYMBOL
                   end (* then *)
                 else
                   begin
                     ERROR ( 2 ) ;
                     LCP := UVARPTR
                   end (* else *) ;
                 SELECTOR ( FSYS , LCP , GEN , DUMMYB ) ;
               end (* VARIABLE *) ;


            procedure PREPLIBRARYFUNC ( WASIZE : INTEGER ; var LCCALLER
                                      : ADDRRANGE ; var LCPARM :
                                      ADDRRANGE ; var LCWORK :
                                      ADDRRANGE ) ;

               begin (* PREPLIBRARYFUNC *)
                 GEN2 ( PCODE_MST , 0 , 0 ) ;

                 //************************************
                 // wenn wasize <> 0                   
                 // arbeitsbereich fuer lib function re
                 //servieren                           
                 //************************************

                 if WASIZE > 0 then
                   begin
                     LCWORK := LCOUNTER ;
                     ALIGN ( LCWORK , MXDATASZE ) ;
                     LCCALLER := LCWORK + WASIZE * MXDATASZE ;
                     LCOUNTER := LCCALLER ;
                     if LCOUNTER > LCMAX then
                       LCMAX := LCOUNTER ;
                   end (* then *)
                 else
                   begin
                     LCWORK := 0 ;
                     LCCALLER := LCOUNTER ;
                     ALIGN ( LCCALLER , MXDATASZE ) ;
                   end (* else *) ;

                 //*****************************************************
                 // adresse fuer parameter ermitteln und ausrichten     
                 //*****************************************************

                 LCPARM := LCCALLER + LCAFTMST + FPSAVEAREA
               end (* PREPLIBRARYFUNC *) ;


            procedure CALLLIBRARYFUNC ( FCP : IDP ; LLCALLER :
                                      ADDRRANGE ) ;

               begin (* CALLLIBRARYFUNC *)
                 if OPT . PRCODE then
                   begin
                     PUTIC ;
                     WRITE ( PRR , MN [ PCODE_CUP ] ) ;
                     WRITE ( PRR , PROCTYPE ( FCP ) : 2 ) ;
                     WRITE ( PRR , ',' , FCP -> . PARMCNT * 2 + 3 : 1 )
                             ;
                     WRITE ( PRR , ',' , FCP -> . LIBNAME ) ;
                     WRITELN ( PRR , ',' , LLCALLER : 1 ) ;
                   end (* then *) ;
               end (* CALLLIBRARYFUNC *) ;


            procedure FILESETUP ( DFILE : IDP ; GENSIO : BOOLEAN ) ;

            (***************************************************)
            (* TO SET UP FILE ADDRESS PARAMETER                *)
            (* for other functions like eoln and eof           *)
            (* file parameter is optional, but if parameter    *)
            (* is present, it must be a file parameter         *)
            (***************************************************)


               var LCP : IDP ;
                   SAVED : BOOLEAN ;
                   DUMMYB : BOOLEAN ;

               begin (* FILESETUP *)
                 SAVED := TRUE ;
                 if MATCHPAR then

                 (*************************************)
                 (* OTHERWISE THERE ARE NO PARAMETERS *)
                 (*************************************)

                   if SY = IDENT then
                     begin
                       SID_RC := SEARCHID ( ID , TRUE , TRUE , [ VARS ,
                                 FIELD , FUNC , KONST , STRUCTKONST ] ,
                                 LCP ) ;
                       if LCP -> . IDTYPE <> NIL then
                         with LCP -> . IDTYPE -> do
                           if FORM = FILES then
                             SAVED := FALSE ;
                       INSYMBOL ;
                     end (* then *) ;
                 if SAVED then
                   begin
                     if DFILE = NIL then
                       ERROR ( 185 ) ;
                     LCP := DFILE ;
                   end (* then *) ;
                 SELECTOR ( FSYS + [ SYRPARENT ] , LCP , TRUE , DUMMYB
                            ) ;
                 with GATTR do
                   if COMPTYPES ( TYPTR , PTYPE_TEXT ) <> 1 then
                     if TYPTR <> NIL then
                       if TYPTR -> . FORM <> FILES then
                         ERROR ( 116 )
                       else
                         begin
                           RWFILE := TYPTR -> . FILTYPE ;
                           if not ( LKEY in [ 1 .. 6 , 25 , 36 , 37 ,
                           46 , 70 ] ) then
                             ERROR ( 116 ) ;

                 (**********************************************)
                 (*   NON-TEXT FILES PERMITTED ONLY FOR:       *)
                 (*   GET, PUT, RESET, READ, WRITE,            *)
                 (*   REWRITE, EOF, SKIP, LINELIMIT            *)
                 (**********************************************)

                         end (* else *) ;
                 LOADADDRESS ;
                 if GENSIO then
                   GEN1 ( PCODE_CSP , ORD ( PSIO ) ) ;
               end (* FILESETUP *) ;


            procedure RWSETUP ( DFILE : IDP ; GENSIO : BOOLEAN ) ;

            (***************************************************)
            (* TO SET UP FILE ADDRESS PARAMETER FOR READ/WRITE *)
            (***************************************************)


               var LCP : IDP ;
                   SAVED : BOOLEAN ;
                   TEMPID : ALPHA ;
                   TEMPSY : SYMB ;
                   DUMMYB : BOOLEAN ;

               begin (* RWSETUP *)
                 SAVED := TRUE ;
                 RWFILE := NIL ;
                 if MATCHPAR then

                 (*************************************)
                 (* OTHERWISE THERE ARE NO PARAMETERS *)
                 (*************************************)

                   if SY = IDENT then
                     begin
                       SID_RC := SEARCHID ( ID , TRUE , TRUE , [ VARS ,
                                 FIELD , FUNC , KONST , STRUCTKONST ] ,
                                 LCP ) ;
                       if LCP -> . IDTYPE <> NIL then
                         with LCP -> . IDTYPE -> do
                           if FORM = FILES then
                             SAVED := FALSE ;
                     end (* then *) ;
                 if SAVED then
                   begin

                 (*************************)
                 (* USE IMPLIED FILE NAME *)
                 (*************************)

                     TEMPSY := SY ;
                     TEMPID := ID ;
                     SY := SYCOMMA ;
                     if DFILE = NIL then
                       ERROR ( 185 ) ;
                     LCP := DFILE ;
                   end (* then *)
                 else
                   INSYMBOL ;
                 SELECTOR ( FSYS + [ SYCOMMA , SYRPARENT ] , LCP , TRUE
                            , DUMMYB ) ;
                 with GATTR do
                   if COMPTYPES ( TYPTR , PTYPE_TEXT ) <> 1 then
                     if TYPTR <> NIL then
                       if TYPTR -> . FORM <> FILES then
                         ERROR ( 116 )
                       else
                         begin
                           RWFILE := TYPTR -> . FILTYPE ;
                           if not ( LKEY in [ 1 .. 6 , 25 , 36 , 37 ,
                           46 , 70 ] ) then
                             ERROR ( 116 ) ;

                 (**********************************************)
                 (*   NON-TEXT FILES PERMITTED ONLY FOR:       *)
                 (*   GET, PUT, RESET, READ, WRITE,            *)
                 (*   REWRITE, EOF, SKIP, LINELIMIT            *)
                 (**********************************************)

                         end (* else *) ;
                 LOADADDRESS ;
                 if GENSIO then
                   GEN1 ( PCODE_CSP , ORD ( PSIO ) ) ;
                 if SAVED then
                   begin
                     ID := TEMPID ;
                     SY := TEMPSY
                   end (* then *) ;
               end (* RWSETUP *) ;


            procedure GETPUTRESETREWRITE ;

               begin (* GETPUTRESETREWRITE *)
                 if LKEY = 46 then
                   FILESETUP ( NIL , TRUE )
                 else
                   if ODD ( LKEY ) then
                     FILESETUP ( INPUTPTR , TRUE )
                   else
                     FILESETUP ( OUTPUTPTR , TRUE ) ;
                 case LKEY of
                   0 .. 4 :
                     GEN1 ( PCODE_CSP , LKEY ) ;
                   46 : GEN1 ( PCODE_CSP , 38 ) ;
                 end (* case *) ;

                 (*****************************)
                 (*CSP - GET,PUT,RES,REW,PAG  *)
                 (*CSP - CLS                  *)
                 (*****************************)

                 GEN1 ( PCODE_CSP , ORD ( PEIO ) ) ;
               end (* GETPUTRESETREWRITE *) ;


            procedure READ1 ;

               var XCSP : CSPTYPE ;
                   TEST : BOOLEAN ;

               begin (* READ1 *)
                 RWSETUP ( INPUTPTR , TRUE ) ;
                 if RWFILE <> NIL then
                   if LKEY = 11 then
                     ERROR ( 116 ) ;
                 if MATCHPAR then

                 (*************************************)
                 (* OTHERWISE THERE ARE NO PARAMETERS *)
                 (*************************************)

                   begin
                     if SY = SYCOMMA then
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
                         VARIABLE ( FSYS + [ SYCOMMA , SYRPARENT ] ,
                                    TRUE ) ;
                         LOADADDRESS ;
                         if GATTR . TYPTR <> NIL then
                           if RWFILE = NIL then
                             if IS_CARRAY ( GATTR . TYPTR ) then
                               begin
                                 GEN2 ( PCODE_LDC , 1 , GATTR . TYPTR
                                        -> . SIZE DIV CHARSIZE ) ;
                                 XCSP := PRDS ;
                               end (* then *)
                             else
                               begin
                                 if GATTR . TYPTR = PTYPE_INT then
                                   if GATTR . BTYPE -> . SIZE = INTSIZE
                                   then
                                     XCSP := PRDI
                                   else
                                     if GATTR . BTYPE -> . SIZE =
                                     HINTSIZE then
                                       XCSP := PRDH
                                     else
                                       XCSP := PRDY

                 (*****************************)
                 (*RDY - ONE BYTE INTEGER READ*)
                 (*****************************)

                                 else
                                   if IS_STDTYPE ( GATTR . TYPTR , 'R'
                                   ) then
                                     XCSP := PRDR
                                   else
                                     if GATTR . TYPTR = PTYPE_CHAR then
                                       XCSP := PRDC
                                     else
                                       if GATTR . TYPTR = PTYPE_BOOL
                                       then
                                         XCSP := PRDB
                                       else
                                         begin
                                           ERROR ( 116 ) ;
                                           XCSP := PRDI
                                         end (* else *) ;
                               end (* else *)
                           else

                 (***********************)
                 (* NON-TEXT FILE INPUT *)
                 (***********************)

                             begin
                               if COMPTYPES ( GATTR . TYPTR , RWFILE )
                               <> 1 then
                                 ERROR ( 153 ) ;
                               GEN2 ( PCODE_LDC , 1 , GATTR . BTYPE ->
                                      . SIZE ) ;
                               XCSP := PRDD ;
                               EXTUSED := TRUE ;
                             end (* else *) ;
                         GEN1 ( PCODE_CSP , ORD ( XCSP ) ) ;
                         if SY = SYCOMMA then
                           INSYMBOL
                         else
                           TEST := TRUE ;
                       until TEST ;
                   end (* then *) ;
                 if LKEY = 11 then
                   GEN1 ( PCODE_CSP , ORD ( PRLN ) ) ;
                 GEN1 ( PCODE_CSP , ORD ( PEIO ) ) ;
               end (* READ1 *) ;


            procedure WRITE1 ;

               var LSP : TTP ;
                   DEFAULT , DEFAULT1 , TEST : BOOLEAN ;
                   LLKEY : INTEGER ;
                   LEN : ADDRRANGE ;
                   XCSP : CSPTYPE ;
                   LLC : ADDRRANGE ;


               procedure WRITE2 ;

                  begin (* WRITE2 *)
                    if LSP = NIL then
                      return ;

                    (************************)
                    (* write integer values *)
                    (************************)

                    if LSP = PTYPE_INT then
                      begin
                        if DEFAULT then
                          GEN2 ( PCODE_LDC , 1 , 12 ) ;
                        XCSP := PWRI ;
                        return
                      end (* then *) ;

                    (*********************)
                    (* write real values *)
                    (*********************)

                    if IS_STDTYPE ( LSP , 'R' ) then
                      begin
                        if LSP -> . STDPARM1 > 0 then
                          begin

                    //**************************************************
                    // if decimal (stdparm1 > 0) then                   
                    // get width etc. from type parameters              
                    //**************************************************

                            if DEFAULT then
                              GEN2 ( PCODE_LDC , 1 , LSP -> . STDPARM1
                                     + 3 ) ;
                            if DEFAULT1 then
                              GEN2 ( PCODE_LDC , 1 , LSP -> . STDPARM2
                                     ) ;
                          end (* then *)
                        else
                          begin
                            if DEFAULT then
                              GEN2 ( PCODE_LDC , 1 , 14 ) ;
                            if DEFAULT1 then
                              GEN2 ( PCODE_LDC , 1 , - 1 ) ;
                          end (* else *) ;
                        XCSP := PWRR ;
                        return
                      end (* then *) ;

                    (*********************)
                    (* write char values *)
                    (*********************)

                    if LSP = PTYPE_CHAR then
                      begin
                        if DEFAULT then
                          GEN2 ( PCODE_LDC , 1 , 1 ) ;
                        XCSP := PWRC ;
                        return
                      end (* then *) ;

                    (************************)
                    (* write boolean values *)
                    (************************)

                    if LSP = PTYPE_BOOL then
                      begin
                        if DEFAULT then
                          GEN2 ( PCODE_LDC , 1 , 5 ) ;
                        XCSP := PWRB ;
                        return
                      end (* then *) ;

                    (************************)
                    (* write pointer values *)
                    (************************)

                    if LSP -> . FORM = POINTER then
                      begin
                        if DEFAULT then
                          GEN2 ( PCODE_LDC , 1 , 8 ) ;
                        XCSP := PWRP ;
                        return
                      end (* then *) ;

                    (***********************)
                    (* write scalar values *)
                    (***********************)

                    if LSP -> . FORM = SCALAR then
                      begin
                        if DEFAULT then
                          GEN2 ( PCODE_LDC , 1 , 1 ) ;

                    (************************************)
                    (* LCA P for static csect of consts *)
                    (************************************)

                        ID := ' ' ;
                        UNPACK ( LSP -> . CSTNAME , ID , 1 ) ;
                        GEN1 ( PCODE_LCA , ORD ( 'P' ) ) ;
                        GEN2 ( PCODE_INC , ORD ( 'A' ) , LSP -> .
                               LITOFFS ) ;
                        XCSP := PWRX ;
                        return
                      end (* then *) ;

                    (************************************)
                    (* write string vars and constants  *)
                    (************************************)

                    if IS_CARRAY ( LSP ) then
                      begin
                        LEN := LSP -> . SIZE DIV CHARSIZE ;
                        if DEFAULT then
                          GEN2 ( PCODE_LDC , 1 , LEN ) ;
                        GEN2 ( PCODE_LDC , 1 , LEN ) ;
                        XCSP := PWRS ;
                        return
                      end (* then *) ;

                    (************************************)
                    (* write cstring vars and constants *)
                    (* not complete ... use true length *)
                    (* not defined maxlength !!!        *)
                    (************************************)

                    if LSP -> . FORM = CSTRING then
                      begin
                        if DEFAULT then
                          GEN2 ( PCODE_LDC , 1 , 0 ) ;
                        XCSP := PWRV ;
                        return
                      end (* then *) ;

                    (***********************)
                    (* erroneous parameter *)
                    (***********************)

                    ERROR ( 116 ) ;
                    XCSP := PWRI ;
                  end (* WRITE2 *) ;


               begin (* WRITE1 *)
                 XCSP := UNDEF_CSP ;
                 LLKEY := LKEY ;
                 TEST := FALSE ;
                 RWSETUP ( OUTPUTPTR , TRUE ) ;
                 if RWFILE <> NIL then
                   if LLKEY = 12 then
                     ERROR ( 116 ) ;
                 if MATCHPAR then

                 (***************************)
                 (* OTHERWISE NO PARAMETERS *)
                 (***************************)

                   begin
                     if SY = SYRPARENT then
                       if LLKEY = 6 then
                         ERROR ( 116 ) ;
                     if SY = SYCOMMA then
                       begin
                         INSYMBOL ;
                         if not ( SY in SIMPTYPEBEGSYS ) then
                           ERROR ( 6 )
                       end (* then *) ;
                     if SY in SIMPTYPEBEGSYS then
                       repeat
                         EXPRESSION ( FSYS + [ SYCOMMA , SYCOLON ,
                                      SYRPARENT ] ) ;
                         LSP := GATTR . TYPTR ;
                         if LSP -> . FORM <= POINTER then
                           LOAD
                         else
                           if LSP -> . FORM = CSTRING then
                             begin
                               if GATTR . KIND = VARBL then
                                 LOADADDRESS
                               else
                                 begin

                 //*****************************************************
                 // copy "string on stack" to free storage              
                 // and replace it by its address on the stack          
                 //*****************************************************

                                   LLC := LCOUNTER ;
                                   ALIGN ( LLC , PTRSIZE ) ;
                                   LCOUNTER := LLC + STRSTACKSZ ;
                                   if LCOUNTER > LCMAX then
                                     LCMAX := LCOUNTER ;
                                   GEN2 ( PCODE_LDA , LEVEL , LLC ) ;
                                   GEN2 ( PCODE_VST , 1 , - 1 ) ;
                                 end (* else *)
                             end (* then *)
                           else
                             LOADADDRESS ;
                         if RWFILE = NIL then
                           begin
                             DEFAULT := TRUE ;
                             DEFAULT1 := TRUE ;
                             if SY = SYCOLON then
                               begin
                                 INSYMBOL ;
                                 EXPRESSION ( FSYS + [ SYCOMMA ,
                                              SYCOLON , SYRPARENT ] ) ;
                                 LOAD ;
                                 if GATTR . TYPTR <> NIL then
                                   if GATTR . TYPTR <> PTYPE_INT then
                                     ERROR ( 116 ) ;
                                 DEFAULT := FALSE ;
                                 if SY = SYCOLON then
                                   begin
                                     INSYMBOL ;
                                     EXPRESSION ( FSYS + [ SYCOMMA ,
                                                  SYRPARENT ] ) ;
                                     LOAD ;
                                     if GATTR . TYPTR <> NIL then
                                       if GATTR . TYPTR <> PTYPE_INT
                                       then
                                         ERROR ( 116 ) ;
                                     if not IS_STDTYPE ( LSP , 'R' )
                                     then
                                       ERROR ( 124 ) ;
                                     DEFAULT1 := FALSE ;
                                   end (* then *) ;
                               end (* then *) ;

                 (**************************************)
                 (* call write funcs depending on type *)
                 (**************************************)

                             WRITE2
                           end (* then *)
                         else

                 (*****************)
                 (* NON-TEXT FILE *)
                 (*****************)

                           begin
                             if COMPTYPES ( LSP , RWFILE ) <> 1 then
                               ERROR ( 145 ) ;
                             GEN2 ( PCODE_LDC , 1 , RWFILE -> . SIZE )
                                    ;
                             EXTUSED := TRUE ;
                             if LSP <> NIL then
                               if LSP -> . FORM <= SUBRANGE then
                                 XCSP := PWRE
                               else
                                 XCSP := PWRD
                           end (* else *) ;
                         if XCSP <> UNDEF_CSP then
                           GEN1 ( PCODE_CSP , ORD ( XCSP ) ) ;
                         if SY = SYCOMMA then
                           INSYMBOL
                         else
                           TEST := TRUE ;
                       until TEST ;
                   end (* then *) ;

                 (***********)
                 (* WRITELN *)
                 (***********)

                 if LLKEY = 12 then
                   GEN1 ( PCODE_CSP , ORD ( PWLN ) ) ;
                 GEN1 ( PCODE_CSP , ORD ( PEIO ) ) ;
               end (* WRITE1 *) ;


            procedure SKIPLIM ;

               begin (* SKIPLIM *)
                 RWSETUP ( OUTPUTPTR , TRUE ) ;
                 if SY = SYCOMMA then
                   begin
                     INSYMBOL ;
                     if not ( SY in SIMPTYPEBEGSYS ) then
                       ERROR ( 6 )
                   end (* then *) ;
                 if SY in SIMPTYPEBEGSYS then
                   begin
                     EXPRESSION ( FSYS + [ SYRPARENT ] ) ;
                     LOAD ;
                     if GATTR . TYPTR <> NIL then
                       if GATTR . TYPTR <> PTYPE_INT then
                         ERROR ( 125 ) ;

                 (*****************)
                 (* CSP - SKP/LIM *)
                 (*****************)

                     GEN1 ( PCODE_CSP , LKEY - 2 ) ;
                     GEN1 ( PCODE_CSP , ORD ( PEIO ) ) ;
                   end (* then *)
               end (* SKIPLIM *) ;


            procedure MESSAGE1 ;

               var LEN : INTEGER ;

               begin (* MESSAGE1 *)
                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;
                 if GATTR . TYPTR <> NIL then
                   if IS_CARRAY ( GATTR . TYPTR ) then
                     LEN := GATTR . TYPTR -> . SIZE DIV CHARSIZE
                   else
                     ERROR ( 125 ) ;
                 LOADADDRESS ;
                 GEN2 ( PCODE_LDC , 1 , LEN ) ;
                 GEN1 ( PCODE_CSP , ORD ( PMSG ) ) ;
               end (* MESSAGE1 *) ;


            procedure PACK1 ;

               var LSP , LSP1 : TTP ;
                   LSIZE , IMIN , IMAX : INTEGER ;
                   LCNT , RCNT , LELEMSIZE , RELEMSIZE : INTEGER ;

               begin (* PACK1 *)
                 EXPRESSION ( FSYS + [ SYCOMMA , SYRPARENT ] ) ;
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
                 if SY = SYCOMMA then
                   INSYMBOL
                 else
                   ERROR ( 20 ) ;
                 EXPRESSION ( FSYS + [ SYCOMMA , SYRPARENT ] ) ;
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM <> SCALAR then
                     ERROR ( 116 )
                   else
                     if COMPTYPES ( LSP , GATTR . TYPTR ) <> 1 then
                       ERROR ( 116 )
                     else
                       begin
                         LOAD ;
                         if OPT . DEBUG then
                           GEN3 ( PCODE_CHK , ORD ( 'J' ) , IMIN , IMAX
                                  ) ;
                         if IMIN > 0 then
                           GEN2 ( PCODE_DEC , GETTYPE ( GATTR . TYPTR )
                                  , IMIN )
                         else
                           if IMIN < 0 then
                             GEN2 ( PCODE_INC , GETTYPE ( GATTR . TYPTR
                                    ) , - IMIN ) ;
                         GEN1 ( PCODE_IXA , LELEMSIZE ) ;
                       end (* else *) ;
                 if SY = SYCOMMA then
                   INSYMBOL
                 else
                   ERROR ( 20 ) ;
                 VARIABLE ( FSYS + [ SYRPARENT ] , TRUE ) ;
                 if GATTR . TYPTR <> NIL then
                   with GATTR . TYPTR -> do
                     if FORM = ARRAYS then
                       begin
                         if ( COMPTYPES ( AELTYPE , LSP1 ) <> 1 ) or (
                         COMPTYPES ( INXTYPE , LSP ) <> 1 ) then
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

                               GEN1 ( PCODE_MOV , - SIZE )
                             else
                               GEN3 ( PCODE_PAK , RCNT , LELEMSIZE ,
                                      RELEMSIZE ) ;
                           end (* else *) ;
                       end (* then *)
                     else
                       ERROR ( 116 )
               end (* PACK1 *) ;


            procedure UNPACK1 ;

               var LSP , LSP1 : TTP ;
                   IMIN , IMAX , LSIZE : INTEGER ;
                   LCNT , RCNT , LELEMSIZE , RELEMSIZE : INTEGER ;

               begin (* UNPACK1 *)
                 EXPRESSION ( FSYS + [ SYCOMMA , SYRPARENT ] ) ;
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
                 if SY = SYCOMMA then
                   INSYMBOL
                 else
                   ERROR ( 20 ) ;
                 VARIABLE ( FSYS + [ SYCOMMA , SYRPARENT ] , TRUE ) ;
                 if GATTR . TYPTR <> NIL then
                   with GATTR , GATTR . TYPTR -> do
                     if FORM = ARRAYS then
                       begin
                         if ( COMPTYPES ( AELTYPE , LSP1 ) <> 1 ) or (
                         COMPTYPES ( INXTYPE , LSP ) <> 1 ) then
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
                 if SY = SYCOMMA then
                   INSYMBOL
                 else
                   ERROR ( 20 ) ;
                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM <> SCALAR then
                     ERROR ( 116 )
                   else
                     if COMPTYPES ( LSP , GATTR . TYPTR ) <> 1 then
                       ERROR ( 116 )
                     else
                       begin
                         LOAD ;
                         if OPT . DEBUG then
                           GEN3 ( PCODE_CHK , ORD ( 'J' ) , IMIN , IMAX
                                  ) ;
                         if IMIN > 0 then
                           GEN2 ( PCODE_DEC , GETTYPE ( GATTR . TYPTR )
                                  , IMIN )
                         else
                           if IMIN < 0 then
                             GEN2 ( PCODE_INC , GETTYPE ( GATTR . TYPTR
                                    ) , - IMIN ) ;
                         GEN1 ( PCODE_IXA , RELEMSIZE ) ;
                         if LELEMSIZE = RELEMSIZE then

                 (*****************)
                 (* A MOVE IS OK  *)
                 (*****************)

                           GEN1 ( PCODE_MOV , - LSIZE )
                         else
                           GEN3 ( PCODE_PAK , LCNT , LELEMSIZE ,
                                  RELEMSIZE ) ;
                       end (* else *) ;
               end (* UNPACK1 *) ;


            procedure NEW1 ;

               label 1 ;

               var LSP , LSP1 : TTP ;
                   VARTS , LMIN , LMAX : INTEGER ;
                   LSIZE , LSZ : ADDRRANGE ;
                   LVAL : XCONSTANT ;
                   LALN : ALNRNG ;

               begin (* NEW1 *)
                 VARIABLE ( FSYS + [ SYCOMMA , SYRPARENT ] , TRUE ) ;
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
                 while SY = SYCOMMA do
                   begin
                     INSYMBOL ;
                     CONSTANT ( FSYS + [ SYCOMMA , SYRPARENT ] , LSP1 ,
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
                           if IS_CARRAY ( LSP1 ) or IS_STDTYPE ( LSP1 ,
                           'R' ) then
                             ERROR ( 159 )
                           else
                             if COMPTYPES ( LSP -> . TAGFIELDP -> .
                             IDTYPE , LSP1 ) = 1 then
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
                 GEN2 ( PCODE_NEW , LSIZE , LALN ) ;
               end (* NEW1 *) ;


            procedure MARKRELEASE ;

               begin (* MARKRELEASE *)
                 VARIABLE ( FSYS + [ SYRPARENT ] , TRUE ) ;
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM = POINTER then
                     if LKEY = 13

                 (********)
                 (*MARK  *)
                 (********)


                     then
                       begin
                         LOADADDRESS ;
                         GEN0 ( PCODE_SAV )
                       end (* then *)
                     else

                 (**********************)
                 (* LKEY = 10, RELEASE *)
                 (**********************)

                       begin
                         LOAD ;
                         GEN0 ( PCODE_RST )
                       end (* else *)
                   else
                     ERROR ( 125 )
               end (* MARKRELEASE *) ;


            procedure ADDR1 ;

               begin (* ADDR1 *)
                 VARIABLE ( FSYS + [ SYRPARENT ] , TRUE ) ;

                 (*******************************)
                 (* load addr of variable and   *)
                 (* set result type to void ptr *)
                 (*******************************)

                 LOADADDRESS ;
                 GATTR . TYPTR := PTYPE_ANY ;
               end (* ADDR1 *) ;


            procedure PTRADD1 ;

               begin (* PTRADD1 *)
                 EXPRESSION ( FSYS + [ SYCOMMA , SYRPARENT ] ) ;

                 (******************************************)
                 (* if type of expr = pointer then load it *)
                 (******************************************)

                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM <> POINTER then
                     ERROR ( 190 )
                   else
                     LOAD ;
                 if SY = SYCOMMA then
                   INSYMBOL
                 else
                   begin

                 (******************************************)
                 (* if only one parameter, then simply     *)
                 (* cast pointer to void pointer type      *)
                 (* so assignments of any pointer types    *)
                 (* are possible using ptradd              *)
                 (******************************************)

                     GATTR . TYPTR := PTYPE_ANY ;
                     return ;
                   end (* else *) ;
                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;

                 (***********************************)
                 (* if type of expr = integer then  *)
                 (* load it and generate ada instr  *)
                 (* - add integer to address        *)
                 (***********************************)

                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> PTYPE_INT then
                     ERROR ( 191 )
                   else
                     begin
                       LOAD ;
                       GEN0 ( 77 ) ;
                     end (* else *) ;
                 GATTR . TYPTR := PTYPE_ANY ;
               end (* PTRADD1 *) ;


            procedure PTRCAST1 ;

               begin (* PTRCAST1 *)
                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;

                 (******************************************)
                 (* if type of expr = pointer then load it *)
                 (******************************************)

                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM <> POINTER then
                     ERROR ( 190 )
                   else
                     LOAD ;
                 GATTR . TYPTR := PTYPE_ANY ;
               end (* PTRCAST1 *) ;


            procedure PTR2INT1 ;

               begin (* PTR2INT1 *)
                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;

                 (******************************************)
                 (* if type of expr = pointer then load it *)
                 (******************************************)

                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM <> POINTER then
                     ERROR ( 190 )
                   else
                     LOAD ;
                 GATTR . TYPTR := PTYPE_INT ;
               end (* PTR2INT1 *) ;


            procedure PTRDIFF1 ;

               begin (* PTRDIFF1 *)
                 EXPRESSION ( FSYS + [ SYCOMMA , SYRPARENT ] ) ;

                 (******************************************)
                 (* if type of expr = pointer then load it *)
                 (******************************************)

                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM <> POINTER then
                     ERROR ( 190 )
                   else
                     LOAD ;
                 if SY = SYCOMMA then
                   INSYMBOL
                 else
                   begin
                     ERROR ( 192 ) ;
                     if SY = SYRPARENT then
                       begin
                         return
                       end (* then *)
                   end (* else *) ;
                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;

                 (************************************)
                 (* if type of expr = pointer then   *)
                 (* load it and generate sba instr   *)
                 (* - subtract adresses              *)
                 (************************************)

                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM <> POINTER then
                     ERROR ( 190 )
                   else
                     begin
                       LOAD ;
                       GEN0 ( 78 )
                     end (* else *) ;
                 GATTR . TYPTR := PTYPE_INT ;
               end (* PTRDIFF1 *) ;


            procedure SIZEOF1 ;

               var SIZE : INTEGER ;
                   LCP : IDP ;
                   XTYPE : TTP ;

               begin (* SIZEOF1 *)
                 SIZE := 0 ;
                 LCP := NIL ;
                 if SY = IDENT then
                   begin

                 (********************************)
                 (* schauen, ob typ-bezeichner   *)
                 (********************************)

                     SID_RC := SEARCHID ( ID , FALSE , FALSE , [ TYPES
                               ] , LCP ) ;
                     if LCP <> NIL then
                       begin
                         XTYPE := LCP -> . IDTYPE ;
                         SIZE := XTYPE -> . SIZE ;
                         INSYMBOL ;
                       end (* then *)
                     else
                       begin

                 (**********************************)
                 (* schauen, ob const-bezeichner   *)
                 (**********************************)

                         SID_RC := SEARCHID ( ID , FALSE , FALSE , [
                                   KONST ] , LCP ) ;
                         if LCP <> NIL then
                           begin
                             XTYPE := LCP -> . IDTYPE ;
                             SIZE := XTYPE -> . SIZE ;
                             INSYMBOL ;
                           end (* then *)
                       end (* else *)
                   end (* then *) ;
                 if LCP = NIL then
                   begin

                 (***********************************)
                 (* wenn kein Typ gefunden:         *)
                 (* variable suchen                 *)
                 (* if type of variable known       *)
                 (* then load size of that type     *)
                 (***********************************)

                     VARIABLE ( FSYS + [ SYRPARENT ] , FALSE ) ;
                     if GATTR . TYPTR <> NIL then
                       begin
                         SIZE := GATTR . TYPTR -> . SIZE ;
                       end (* then *)
                   end (* then *) ;
                 GATTR . TYPTR := PTYPE_INT ;

                 //*******************************************
                 // dont gen ldc, set constant instead ...    
                 // GEN2 ( PCODE_LDC , 1 , SIZE ) ;           
                 //*******************************************

                 GATTR . KIND := CST ;
                 GATTR . CVAL . STRTYPE := ' ' ;
                 GATTR . CVAL . IVAL := SIZE ;
               end (* SIZEOF1 *) ;


            procedure DIGITSOF1 ;

               var DIGITS : INTEGER ;
                   LCP : IDP ;
                   XTYPE : TTP ;

               begin (* DIGITSOF1 *)
                 DIGITS := 0 ;
                 LCP := NIL ;
                 XTYPE := NIL ;
                 case SY of
                   IDENT : begin

                 (********************************)
                 (* schauen, ob typ-bezeichner   *)
                 (********************************)

                             SID_RC := SEARCHID ( ID , FALSE , FALSE ,
                                       [ TYPES ] , LCP ) ;
                             if LCP <> NIL then
                               begin
                                 XTYPE := LCP -> . IDTYPE ;
                                 INSYMBOL ;
                               end (* then *)
                             else
                               begin

                 (**********************************)
                 (* schauen, ob const-bezeichner   *)
                 (**********************************)

                                 SID_RC := SEARCHID ( ID , FALSE ,
                                           FALSE , [ KONST ] , LCP ) ;
                                 if LCP <> NIL then
                                   begin
                                     XTYPE := LCP -> . IDTYPE ;
                                     INSYMBOL ;
                                   end (* then *)
                               end (* else *) ;
                             if LCP = NIL then
                               begin

                 (***********************************)
                 (* wenn kein Typ gefunden:         *)
                 (* variable suchen                 *)
                 (* if type of variable known       *)
                 (* then load size of that type     *)
                 (***********************************)

                                 VARIABLE ( FSYS + [ SYRPARENT ] ,
                                            FALSE ) ;
                                 if GATTR . TYPTR <> NIL then
                                   begin
                                     XTYPE := GATTR . TYPTR ;
                                   end (* then *)
                               end (* then *) ;
                           end (* tag/ca *) ;
                   INTCONST :
                     begin
                       DIGITS := SYDIGITS ;
                       INSYMBOL ;
                     end (* tag/ca *) ;
                   REALCONST :
                     begin
                       DIGITS := SYDIGITS ;
                       INSYMBOL ;
                     end (* tag/ca *) ;
                   otherwise
                     ;
                 end (* case *) ;
                 if XTYPE <> NIL then
                   if XTYPE -> . FORM = SCALAR then
                     if XTYPE -> . SCALKIND = STANDARD then
                       DIGITS := XTYPE -> . STDPARM1 ;
                 GATTR . TYPTR := PTYPE_INT ;

                 //*******************************************
                 // dont gen ldc, set constant instead ...    
                 // GEN2 ( PCODE_LDC , 1 , digits ) ;         
                 //*******************************************

                 GATTR . KIND := CST ;
                 GATTR . CVAL . STRTYPE := ' ' ;
                 GATTR . CVAL . IVAL := DIGITS ;
               end (* DIGITSOF1 *) ;


            procedure PRECISIONOF1 ;

               var PREC : INTEGER ;
                   LCP : IDP ;
                   XTYPE : TTP ;

               begin (* PRECISIONOF1 *)
                 PREC := 0 ;
                 LCP := NIL ;
                 XTYPE := NIL ;
                 case SY of
                   IDENT : begin

                 (********************************)
                 (* schauen, ob typ-bezeichner   *)
                 (********************************)

                             SID_RC := SEARCHID ( ID , FALSE , FALSE ,
                                       [ TYPES ] , LCP ) ;
                             if LCP <> NIL then
                               begin
                                 XTYPE := LCP -> . IDTYPE ;
                                 INSYMBOL ;
                               end (* then *)
                             else
                               begin

                 (**********************************)
                 (* schauen, ob const-bezeichner   *)
                 (**********************************)

                                 SID_RC := SEARCHID ( ID , FALSE ,
                                           FALSE , [ KONST ] , LCP ) ;
                                 if LCP <> NIL then
                                   begin
                                     XTYPE := LCP -> . IDTYPE ;
                                     INSYMBOL ;
                                   end (* then *)
                               end (* else *) ;
                             if LCP = NIL then
                               begin

                 (***********************************)
                 (* wenn kein Typ gefunden:         *)
                 (* variable suchen                 *)
                 (* if type of variable known       *)
                 (* then load size of that type     *)
                 (***********************************)

                                 VARIABLE ( FSYS + [ SYRPARENT ] ,
                                            FALSE ) ;
                                 if GATTR . TYPTR <> NIL then
                                   begin
                                     XTYPE := GATTR . TYPTR ;
                                   end (* then *)
                               end (* then *) ;
                           end (* tag/ca *) ;
                   INTCONST :
                     begin
                       PREC := SYPREC ;
                       INSYMBOL ;
                     end (* tag/ca *) ;
                   REALCONST :
                     begin
                       PREC := SYPREC ;
                       INSYMBOL ;
                     end (* tag/ca *) ;
                   otherwise
                     ;
                 end (* case *) ;
                 if XTYPE <> NIL then
                   if XTYPE -> . FORM = SCALAR then
                     if XTYPE -> . SCALKIND = STANDARD then
                       PREC := XTYPE -> . STDPARM2 ;
                 GATTR . TYPTR := PTYPE_INT ;

                 //*******************************************
                 // dont gen ldc, set constant instead ...    
                 // GEN2 ( PCODE_LDC , 1 , prec ) ;           
                 //*******************************************

                 GATTR . KIND := CST ;
                 GATTR . CVAL . STRTYPE := ' ' ;
                 GATTR . CVAL . IVAL := PREC ;
               end (* PRECISIONOF1 *) ;


            procedure STR1 ;

            //**********************************************
            // function str converts chars and              
            // char arrays to strings                       
            //**********************************************


               begin (* STR1 *)
                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR = PTYPE_CHAR then
                     begin
                       LOAD ;
                       CTLS . VPO1_NEEDED := TRUE ;
                       GEN0 ( PCODE_VC1 ) ;
                     end (* then *)
                   else
                     if IS_CARRAY ( GATTR . TYPTR ) then
                       begin
                         LOADADDRESS ;
                         CTLS . VPO1_NEEDED := TRUE ;
                         GEN1 ( PCODE_VC2 , GATTR . TYPTR -> . SIZE ) ;
                       end (* then *)
                     else
                       ERROR ( 340 ) ;
                 GATTR . TYPTR := PTYPE_VARCHAR ;
                 GATTR . KIND := VARBL ;
                 GATTR . ACCESS := STKEXPR ;
               end (* STR1 *) ;


            procedure MAXLENGTH1 ;

            //**********************************************
            // get maxlength for strings                    
            // allowed for char and char arrays, too        
            //**********************************************


               begin (* MAXLENGTH1 *)
                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;
                 if GATTR . TYPTR <> NIL then

                 //************************************
                 // always 1 for char                  
                 //************************************

                   if GATTR . TYPTR = PTYPE_CHAR then
                     begin
                       GEN2 ( PCODE_LDC , 1 , 1 ) ;
                     end (* then *)
                   else

                 //************************************
                 // defined size for char arrays       
                 //************************************

                     if IS_CARRAY ( GATTR . TYPTR ) then
                       begin
                         GEN2 ( PCODE_LDC , 1 , GATTR . TYPTR -> . SIZE
                                ) ;
                       end (* then *)
                     else
                       if GATTR . TYPTR -> . FORM = CSTRING then
                         if GATTR . KIND <> EXPR then
                           begin

                 //*******************************************
                 // if not expression and not conformant      
                 // string type, get maxlength from           
                 // string type definition                    
                 //*******************************************

                             if not GATTR . TYPTR -> . CONFORMANT then
                               GEN2 ( PCODE_LDC , 1 , GATTR . TYPTR ->
                                      . SIZE - 4 )
                             else

                 //*******************************************
                 // otherwise, if string variable, locate it  
                 // and get maxlength from there              
                 //*******************************************

                               begin
                                 LOADADDRESS ;
                                 GEN2 ( PCODE_IND , ORD ( 'H' ) , 0 )
                               end (* else *)
                           end (* then *)
                         else
                           begin

                 //*****************************************************
                 // if string expression (= string on stack),           
                 // use VLM P-Code                                      
                 // maxlength will be equal to length in this case      
                 //*****************************************************

                             GEN0 ( PCODE_VLM ) ;
                           end (* else *)
                       else
                         ERROR ( 340 ) ;
                 GATTR . TYPTR := PTYPE_INT ;
               end (* MAXLENGTH1 *) ;


            procedure LENGTH1 ;

            //**********************************************
            // get length for strings                       
            // allowed for char and char arrays, too        
            //**********************************************


               begin (* LENGTH1 *)
                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;
                 if GATTR . TYPTR <> NIL then

                 //************************************
                 // always 1 for char                  
                 //************************************

                   if GATTR . TYPTR = PTYPE_CHAR then
                     begin
                       GEN2 ( PCODE_LDC , 1 , 1 ) ;
                     end (* then *)
                   else

                 //************************************
                 // defined size for char arrays       
                 //************************************

                     if IS_CARRAY ( GATTR . TYPTR ) then
                       begin
                         GEN2 ( PCODE_LDC , 1 , GATTR . TYPTR -> . SIZE
                                ) ;
                       end (* then *)
                     else
                       if GATTR . TYPTR -> . FORM = CSTRING then
                         if GATTR . KIND <> EXPR then
                           begin

                 //*******************************************
                 // if string variable, locate it             
                 // and get length from there                 
                 //*******************************************

                             LOADADDRESS ;
                             GEN2 ( PCODE_IND , ORD ( 'H' ) , 2 )
                           end (* then *)
                         else
                           begin

                 //*****************************************************
                 // if string expression (= string on stack),           
                 // use VLM P-Code                                      
                 // length will be equal to maxlength in this case      
                 // (maxlength field = -1)                              
                 //*****************************************************

                             GEN0 ( PCODE_VLM ) ;
                           end (* else *)
                       else
                         ERROR ( 340 ) ;
                 GATTR . TYPTR := PTYPE_INT ;
               end (* LENGTH1 *) ;


            procedure STRRESULTP1 ;

               begin (* STRRESULTP1 *)
                 if not STRING_RESULT then
                   ERROR ( 354 ) ;

                 //*******************************************
                 // strresultp only valid inside functions    
                 // with string result                        
                 // simply load pointer from fncrslt + 4      
                 // function result must be "string on stack" 
                 //*******************************************

                 GEN3 ( PCODE_LOD , ORD ( 'A' ) , LEVEL , FNCRSLT + 4 )
                        ;
                 GATTR . TYPTR := PTYPE_ANY ;
               end (* STRRESULTP1 *) ;


            procedure STRRESULT1 ;

               begin (* STRRESULT1 *)
                 if not STRING_RESULT then
                   ERROR ( 354 ) ;

                 //*******************************************
                 // strresult only valid inside functions     
                 // with string result                        
                 // simply load string from fncrslt           
                 // function result must be "string on stack" 
                 //*******************************************

                 GEN2 ( PCODE_LDA , LEVEL , FNCRSLT ) ;
                 GEN2 ( PCODE_VLD , 0 , 0 ) ;
                 GATTR . TYPTR := PTYPE_VARCHAR ;
               end (* STRRESULT1 *) ;


            procedure RESULTP1 ;

               begin (* RESULTP1 *)
                 if PROCTYPE ( FPROCP ) = 'P' then
                   ERROR ( 356 ) ;

                 //*******************************************
                 // strresultp only valid inside functions    
                 // with string result                        
                 // simply load pointer from fncrslt + 4      
                 // function result must be "string on stack" 
                 //*******************************************

                 GEN2 ( PCODE_LDA , LEVEL , FNCRSLT ) ;
                 GATTR . TYPTR := PTYPE_ANY ;
               end (* RESULTP1 *) ;


            procedure REPEATSTR1 ;

               var CT_RESULT : INTEGER ;

               begin (* REPEATSTR1 *)

                 //************************************
                 // this is implemented using a new    
                 // p-code instruction vrp             
                 // first load string (first parameter)
                 //************************************

                 EXPRESSION ( FSYS + [ SYCOMMA , SYRPARENT ] ) ;

                 //************************************
                 // do conversions, if needed          
                 //************************************

                 if GATTR . TYPTR -> . FORM = CSTRING then
                   begin
                     if GATTR . KIND <> EXPR then
                       begin
                         LOADADDRESS ;
                         GEN2 ( PCODE_VLD , 1 , GATTR . TYPTR -> . SIZE
                                - 4 ) ;
                       end (* then *) ;
                     CTLS . VPO1_NEEDED := TRUE ;
                     GATTR . KIND := EXPR ;
                   end (* then *)
                 else
                   begin
                     CT_RESULT := COMPTYPES ( PTYPE_VARCHAR , GATTR .
                                  TYPTR ) ;
                     case CT_RESULT of
                       4 : begin
                             LOAD ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN0 ( PCODE_VC1 ) ;
                           end (* tag/ca *) ;
                       5 : begin
                             LOADADDRESS ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN1 ( PCODE_VC2 , GATTR . TYPTR -> . SIZE
                                    ) ;
                           end (* tag/ca *) ;
                       otherwise
                         begin
                           ERROR ( 342 ) ;
                           GATTR . TYPTR := NIL
                         end (* otherw *)
                     end (* case *) ;
                     GATTR . TYPTR := PTYPE_VARCHAR ;
                     GATTR . KIND := EXPR ;
                   end (* else *) ;

                 //************************************
                 // next symbol must be comma          
                 // for second parameter               
                 //************************************

                 if SY = SYCOMMA then
                   INSYMBOL
                 else
                   begin
                     ERROR ( 355 ) ;
                     if SY = SYRPARENT then
                       begin
                         GATTR . TYPTR := PTYPE_VARCHAR ;
                         return
                       end (* then *)
                   end (* else *) ;

                 //************************************
                 // integer expression must follow     
                 // load integer value                 
                 //************************************

                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> PTYPE_INT then
                     ERROR ( 191 )
                   else
                     LOAD ;

                 //************************************
                 // new vrp instruction                
                 //************************************

                 GEN0 ( PCODE_VRP ) ;
                 GATTR . TYPTR := PTYPE_VARCHAR ;
               end (* REPEATSTR1 *) ;


            procedure SUBSTR1 ;

               var CT_RESULT : INTEGER ;
                   LLC : ADDRRANGE ;
                   LCPARM1 : ADDRRANGE ;
                   LCPARM2 : ADDRRANGE ;
                   LCPARM3 : ADDRRANGE ;

               begin (* SUBSTR1 *)

                 //************************************
                 // substr is implemented using a      
                 // library function (in pascal)       
                 //************************************

                 LCPARM1 := LCPARM ;                // str addr
                 LCPARM2 := LCPARM1 + PTRSIZE ;     // position
                 LCPARM3 := LCPARM2 + INTSIZE ;     // length
                 LCPARM := LCPARM3 + INTSIZE ;

                 //************************************
                 // string expression                  
                 //************************************

                 EXPRESSION ( FSYS + [ SYCOMMA , SYRPARENT ] ) ;
                 if GATTR . TYPTR -> . FORM = CSTRING then
                   begin
                     if GATTR . KIND <> EXPR then
                       begin
                         LOADADDRESS ;
                         GEN2 ( PCODE_VLD , 1 , GATTR . TYPTR -> . SIZE
                                - 4 ) ;
                       end (* then *) ;
                     CTLS . VPO1_NEEDED := TRUE ;
                     GATTR . KIND := EXPR ;
                   end (* then *)
                 else
                   begin
                     CT_RESULT := COMPTYPES ( PTYPE_VARCHAR , GATTR .
                                  TYPTR ) ;
                     case CT_RESULT of
                       4 : begin
                             LOAD ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN0 ( PCODE_VC1 ) ;
                           end (* tag/ca *) ;
                       5 : begin
                             LOADADDRESS ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN1 ( PCODE_VC2 , GATTR . TYPTR -> . SIZE
                                    ) ;
                           end (* tag/ca *) ;
                       otherwise
                         begin
                           ERROR ( 342 ) ;
                           GATTR . TYPTR := NIL
                         end (* otherw *)
                     end (* case *) ;
                     GATTR . TYPTR := PTYPE_VARCHAR ;
                     GATTR . KIND := EXPR ;
                   end (* else *) ;

                 //************************************
                 // this lib function has a work area  
                 // store string as first (const) parm 
                 //************************************

                 LLC := LCWORK ;
                 LCWORK := LLC + STRSTACKSZ ;
                 GEN2 ( PCODE_LDA , LEVEL , LLC ) ;
                 GEN2 ( PCODE_VST , 1 , - 1 ) ;
                 GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL , LCPARM1 ) ;
                 if SY = SYCOMMA then
                   INSYMBOL
                 else
                   begin
                     ERROR ( 355 ) ;
                     if SY = SYRPARENT then
                       begin
                         GATTR . TYPTR := PTYPE_VARCHAR ;
                         return
                       end (* then *)
                   end (* else *) ;

                 //****************************************
                 // second expression = starting position  
                 //****************************************

                 EXPRESSION ( FSYS + [ SYCOMMA , SYRPARENT ] ) ;
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> PTYPE_INT then
                     ERROR ( 191 )
                   else
                     begin
                       LOAD ;
                       GEN3 ( PCODE_STR , ORD ( 'I' ) , LEVEL , LCPARM2
                              ) ;
                     end (* else *) ;

                 //****************************************
                 // third expression = optional length     
                 //****************************************

                 if SY = SYCOMMA then
                   begin
                     INSYMBOL ;
                     EXPRESSION ( FSYS + [ SYRPARENT ] ) ;
                     if GATTR . TYPTR <> NIL then
                       if GATTR . TYPTR <> PTYPE_INT then
                         ERROR ( 191 )
                       else
                         LOAD ;
                     GEN3 ( PCODE_STR , ORD ( 'I' ) , LEVEL , LCPARM3 )
                            ;
                   end (* then *)

                 //****************************************
                 // if no third expr, set minus one        
                 //****************************************

                 else
                   begin
                     GEN2 ( PCODE_LDC , 1 , - 1 ) ;
                     GEN3 ( PCODE_STR , ORD ( 'I' ) , LEVEL , LCPARM3 )
                            ;
                   end (* else *) ;
                 CALLLIBRARYFUNC ( FCP , LCCALLER ) ;
                 GATTR . TYPTR := PTYPE_VARCHAR ;
               end (* SUBSTR1 *) ;


            procedure DELETE1 ;

               var CT_RESULT : INTEGER ;
                   LLC : ADDRRANGE ;
                   LCPARM1 : ADDRRANGE ;
                   LCPARM2 : ADDRRANGE ;
                   LCPARM3 : ADDRRANGE ;

               begin (* DELETE1 *)

                 //************************************
                 // delete is implemented using a      
                 // library function (in pascal)       
                 //************************************

                 LCPARM1 := LCPARM ;                // str addr
                 LCPARM2 := LCPARM1 + PTRSIZE ;     // position
                 LCPARM3 := LCPARM2 + INTSIZE ;     // length
                 LCPARM := LCPARM3 + INTSIZE ;

                 //************************************
                 // string expression                  
                 //************************************

                 EXPRESSION ( FSYS + [ SYCOMMA , SYRPARENT ] ) ;
                 if GATTR . TYPTR -> . FORM = CSTRING then
                   begin
                     if GATTR . KIND <> EXPR then
                       begin
                         LOADADDRESS ;
                         GEN2 ( PCODE_VLD , 1 , GATTR . TYPTR -> . SIZE
                                - 4 ) ;
                       end (* then *) ;
                     CTLS . VPO1_NEEDED := TRUE ;
                     GATTR . KIND := EXPR ;
                   end (* then *)
                 else
                   begin
                     CT_RESULT := COMPTYPES ( PTYPE_VARCHAR , GATTR .
                                  TYPTR ) ;
                     case CT_RESULT of
                       4 : begin
                             LOAD ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN0 ( PCODE_VC1 ) ;
                           end (* tag/ca *) ;
                       5 : begin
                             LOADADDRESS ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN1 ( PCODE_VC2 , GATTR . TYPTR -> . SIZE
                                    ) ;
                           end (* tag/ca *) ;
                       otherwise
                         begin
                           ERROR ( 342 ) ;
                           GATTR . TYPTR := NIL
                         end (* otherw *)
                     end (* case *) ;
                     GATTR . TYPTR := PTYPE_VARCHAR ;
                     GATTR . KIND := EXPR ;
                   end (* else *) ;

                 //************************************
                 // this lib function has a work area  
                 // store string as first (const) parm 
                 //************************************

                 LLC := LCWORK ;
                 LCWORK := LLC + STRSTACKSZ ;
                 GEN2 ( PCODE_LDA , LEVEL , LLC ) ;
                 GEN2 ( PCODE_VST , 1 , - 1 ) ;
                 GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL , LCPARM1 ) ;
                 if SY = SYCOMMA then
                   INSYMBOL
                 else
                   begin
                     ERROR ( 355 ) ;
                     if SY = SYRPARENT then
                       begin
                         GATTR . TYPTR := PTYPE_VARCHAR ;
                         return
                       end (* then *)
                   end (* else *) ;

                 //****************************************
                 // second expression = starting position  
                 //****************************************

                 EXPRESSION ( FSYS + [ SYCOMMA , SYRPARENT ] ) ;
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> PTYPE_INT then
                     ERROR ( 191 )
                   else
                     begin
                       LOAD ;
                       GEN3 ( PCODE_STR , ORD ( 'I' ) , LEVEL , LCPARM2
                              ) ;
                     end (* else *) ;

                 //****************************************
                 // third expression = optional length     
                 //****************************************

                 if SY = SYCOMMA then
                   begin
                     INSYMBOL ;
                     EXPRESSION ( FSYS + [ SYRPARENT ] ) ;
                     if GATTR . TYPTR <> NIL then
                       if GATTR . TYPTR <> PTYPE_INT then
                         ERROR ( 191 )
                       else
                         LOAD ;
                     GEN3 ( PCODE_STR , ORD ( 'I' ) , LEVEL , LCPARM3 )
                            ;
                   end (* then *)

                 //****************************************
                 // if no third expr, set minus one        
                 //****************************************

                 else
                   begin
                     GEN2 ( PCODE_LDC , 1 , - 1 ) ;
                     GEN3 ( PCODE_STR , ORD ( 'I' ) , LEVEL , LCPARM3 )
                            ;
                   end (* else *) ;
                 CALLLIBRARYFUNC ( FCP , LCCALLER ) ;
                 GATTR . TYPTR := PTYPE_VARCHAR ;
               end (* DELETE1 *) ;


            procedure RTRIM1 ;

               var CT_RESULT : INTEGER ;
                   LLC : ADDRRANGE ;
                   LCPARM1 : ADDRRANGE ;

               begin (* RTRIM1 *)

                 //************************************
                 // trim is implemented using a        
                 // library function (in pascal)       
                 //************************************

                 LCPARM1 := LCPARM ;                // str addr
                 LCPARM := LCPARM1 + PTRSIZE ;

                 //************************************
                 // string expression                  
                 //************************************

                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;
                 if GATTR . TYPTR -> . FORM = CSTRING then
                   begin
                     if GATTR . KIND <> EXPR then
                       begin
                         LOADADDRESS ;
                         GEN2 ( PCODE_VLD , 1 , GATTR . TYPTR -> . SIZE
                                - 4 ) ;
                       end (* then *) ;
                     CTLS . VPO1_NEEDED := TRUE ;
                     GATTR . KIND := EXPR ;
                   end (* then *)
                 else
                   begin
                     CT_RESULT := COMPTYPES ( PTYPE_VARCHAR , GATTR .
                                  TYPTR ) ;
                     case CT_RESULT of
                       4 : begin
                             LOAD ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN0 ( PCODE_VC1 ) ;
                           end (* tag/ca *) ;
                       5 : begin
                             LOADADDRESS ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN1 ( PCODE_VC2 , GATTR . TYPTR -> . SIZE
                                    ) ;
                           end (* tag/ca *) ;
                       otherwise
                         begin
                           ERROR ( 342 ) ;
                           GATTR . TYPTR := NIL
                         end (* otherw *)
                     end (* case *) ;
                     GATTR . TYPTR := PTYPE_VARCHAR ;
                     GATTR . KIND := EXPR ;
                   end (* else *) ;

                 //************************************
                 // this lib function has a work area  
                 // store string as first (const) parm 
                 //************************************

                 LLC := LCWORK ;
                 LCWORK := LLC + STRSTACKSZ ;
                 GEN2 ( PCODE_LDA , LEVEL , LLC ) ;
                 GEN2 ( PCODE_VST , 1 , - 1 ) ;
                 GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL , LCPARM1 ) ;
                 CALLLIBRARYFUNC ( FCP , LCCALLER ) ;
                 GATTR . TYPTR := PTYPE_VARCHAR ;
               end (* RTRIM1 *) ;


            procedure LTRIM1 ;

               var CT_RESULT : INTEGER ;
                   LLC : ADDRRANGE ;
                   LCPARM1 : ADDRRANGE ;

               begin (* LTRIM1 *)

                 //************************************
                 // ltrim is implemented using a       
                 // library function (in pascal)       
                 //************************************

                 LCPARM1 := LCPARM ;                // str addr
                 LCPARM := LCPARM1 + PTRSIZE ;

                 //************************************
                 // string expression                  
                 //************************************

                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;
                 if GATTR . TYPTR -> . FORM = CSTRING then
                   begin
                     if GATTR . KIND <> EXPR then
                       begin
                         LOADADDRESS ;
                         GEN2 ( PCODE_VLD , 1 , GATTR . TYPTR -> . SIZE
                                - 4 ) ;
                       end (* then *) ;
                     CTLS . VPO1_NEEDED := TRUE ;
                     GATTR . KIND := EXPR ;
                   end (* then *)
                 else
                   begin
                     CT_RESULT := COMPTYPES ( PTYPE_VARCHAR , GATTR .
                                  TYPTR ) ;
                     case CT_RESULT of
                       4 : begin
                             LOAD ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN0 ( PCODE_VC1 ) ;
                           end (* tag/ca *) ;
                       5 : begin
                             LOADADDRESS ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN1 ( PCODE_VC2 , GATTR . TYPTR -> . SIZE
                                    ) ;
                           end (* tag/ca *) ;
                       otherwise
                         begin
                           ERROR ( 342 ) ;
                           GATTR . TYPTR := NIL
                         end (* otherw *)
                     end (* case *) ;
                     GATTR . TYPTR := PTYPE_VARCHAR ;
                     GATTR . KIND := EXPR ;
                   end (* else *) ;

                 //************************************
                 // this lib function has a work area  
                 // store string as first (const) parm 
                 //************************************

                 LLC := LCWORK ;
                 LCWORK := LLC + STRSTACKSZ ;
                 GEN2 ( PCODE_LDA , LEVEL , LLC ) ;
                 GEN2 ( PCODE_VST , 1 , - 1 ) ;
                 GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL , LCPARM1 ) ;
                 CALLLIBRARYFUNC ( FCP , LCCALLER ) ;
                 GATTR . TYPTR := PTYPE_VARCHAR ;
               end (* LTRIM1 *) ;


            procedure TRIM1 ;

               var CT_RESULT : INTEGER ;
                   LLC : ADDRRANGE ;
                   LCPARM1 : ADDRRANGE ;

               begin (* TRIM1 *)

                 //************************************
                 // ltrim is implemented using a       
                 // library function (in pascal)       
                 //************************************

                 LCPARM1 := LCPARM ;                // str addr
                 LCPARM := LCPARM1 + PTRSIZE ;

                 //************************************
                 // string expression                  
                 //************************************

                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;
                 if GATTR . TYPTR -> . FORM = CSTRING then
                   begin
                     if GATTR . KIND <> EXPR then
                       begin
                         LOADADDRESS ;
                         GEN2 ( PCODE_VLD , 1 , GATTR . TYPTR -> . SIZE
                                - 4 ) ;
                       end (* then *) ;
                     CTLS . VPO1_NEEDED := TRUE ;
                     GATTR . KIND := EXPR ;
                   end (* then *)
                 else
                   begin
                     CT_RESULT := COMPTYPES ( PTYPE_VARCHAR , GATTR .
                                  TYPTR ) ;
                     case CT_RESULT of
                       4 : begin
                             LOAD ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN0 ( PCODE_VC1 ) ;
                           end (* tag/ca *) ;
                       5 : begin
                             LOADADDRESS ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN1 ( PCODE_VC2 , GATTR . TYPTR -> . SIZE
                                    ) ;
                           end (* tag/ca *) ;
                       otherwise
                         begin
                           ERROR ( 342 ) ;
                           GATTR . TYPTR := NIL
                         end (* otherw *)
                     end (* case *) ;
                     GATTR . TYPTR := PTYPE_VARCHAR ;
                     GATTR . KIND := EXPR ;
                   end (* else *) ;

                 //************************************
                 // this lib function has a work area  
                 // store string as first (const) parm 
                 //************************************

                 LLC := LCWORK ;
                 LCWORK := LLC + STRSTACKSZ ;
                 GEN2 ( PCODE_LDA , LEVEL , LLC ) ;
                 GEN2 ( PCODE_VST , 1 , - 1 ) ;
                 GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL , LCPARM1 ) ;
                 CALLLIBRARYFUNC ( FCP , LCCALLER ) ;
                 GATTR . TYPTR := PTYPE_VARCHAR ;
               end (* TRIM1 *) ;


            procedure COMPRESS1 ;

               var CT_RESULT : INTEGER ;
                   LLC : ADDRRANGE ;
                   LCPARM1 : ADDRRANGE ;

               begin (* COMPRESS1 *)

                 //************************************
                 // compress is implemented using a    
                 // library function (in pascal)       
                 //************************************

                 LCPARM1 := LCPARM ;                // str addr
                 LCPARM := LCPARM1 + PTRSIZE ;

                 //************************************
                 // string expression                  
                 //************************************

                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;
                 if GATTR . TYPTR -> . FORM = CSTRING then
                   begin
                     if GATTR . KIND <> EXPR then
                       begin
                         LOADADDRESS ;
                         GEN2 ( PCODE_VLD , 1 , GATTR . TYPTR -> . SIZE
                                - 4 ) ;
                       end (* then *) ;
                     CTLS . VPO1_NEEDED := TRUE ;
                     GATTR . KIND := EXPR ;
                   end (* then *)
                 else
                   begin
                     CT_RESULT := COMPTYPES ( PTYPE_VARCHAR , GATTR .
                                  TYPTR ) ;
                     case CT_RESULT of
                       4 : begin
                             LOAD ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN0 ( PCODE_VC1 ) ;
                           end (* tag/ca *) ;
                       5 : begin
                             LOADADDRESS ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN1 ( PCODE_VC2 , GATTR . TYPTR -> . SIZE
                                    ) ;
                           end (* tag/ca *) ;
                       otherwise
                         begin
                           ERROR ( 342 ) ;
                           GATTR . TYPTR := NIL
                         end (* otherw *)
                     end (* case *) ;
                     GATTR . TYPTR := PTYPE_VARCHAR ;
                     GATTR . KIND := EXPR ;
                   end (* else *) ;

                 //************************************
                 // this lib function has a work area  
                 // store string as first (const) parm 
                 //************************************

                 LLC := LCWORK ;
                 LCWORK := LLC + STRSTACKSZ ;
                 GEN2 ( PCODE_LDA , LEVEL , LLC ) ;
                 GEN2 ( PCODE_VST , 1 , - 1 ) ;
                 GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL , LCPARM1 ) ;
                 CALLLIBRARYFUNC ( FCP , LCCALLER ) ;
                 GATTR . TYPTR := PTYPE_VARCHAR ;
               end (* COMPRESS1 *) ;


            procedure INDEX1 ;

               var CT_RESULT : INTEGER ;
                   LLC : ADDRRANGE ;
                   LCPARM1 : ADDRRANGE ;
                   LCPARM2 : ADDRRANGE ;

               begin (* INDEX1 *)

                 //************************************
                 // index is implemented using a       
                 // library function (in pascal)       
                 //************************************

                 LCPARM1 := LCPARM ;                // str addr
                 LCPARM2 := LCPARM1 + PTRSIZE ;     // 2nd str addr
                 LCPARM := LCPARM2 + PTRSIZE ;

                 //************************************
                 // string expression                  
                 //************************************

                 EXPRESSION ( FSYS + [ SYCOMMA , SYRPARENT ] ) ;
                 if GATTR . TYPTR -> . FORM = CSTRING then
                   begin
                     if GATTR . KIND <> EXPR then
                       begin
                         LOADADDRESS ;
                         GEN2 ( PCODE_VLD , 0 , GATTR . TYPTR -> . SIZE
                                - 4 ) ;
                       end (* then *) ;
                     CTLS . VPO1_NEEDED := TRUE ;
                     GATTR . KIND := EXPR ;
                   end (* then *)
                 else
                   begin
                     CT_RESULT := COMPTYPES ( PTYPE_VARCHAR , GATTR .
                                  TYPTR ) ;
                     case CT_RESULT of
                       4 : begin
                             LOAD ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN0 ( PCODE_VC1 ) ;
                           end (* tag/ca *) ;
                       5 : begin
                             LOADADDRESS ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN1 ( PCODE_VC2 , GATTR . TYPTR -> . SIZE
                                    ) ;
                           end (* tag/ca *) ;
                       otherwise
                         begin
                           ERROR ( 342 ) ;
                           GATTR . TYPTR := NIL
                         end (* otherw *)
                     end (* case *) ;
                     GATTR . TYPTR := PTYPE_VARCHAR ;
                     GATTR . KIND := EXPR ;
                   end (* else *) ;

                 //************************************
                 // this lib function has a work area  
                 // store string as first (const) parm 
                 //************************************

                 LLC := LCWORK ;
                 LCWORK := LLC + STRSTACKSZ ;
                 GEN2 ( PCODE_LDA , LEVEL , LLC ) ;
                 GEN2 ( PCODE_VST , 1 , - 1 ) ;
                 GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL , LCPARM1 ) ;
                 if SY = SYCOMMA then
                   INSYMBOL
                 else
                   begin
                     ERROR ( 355 ) ;
                     if SY = SYRPARENT then
                       begin
                         GATTR . TYPTR := PTYPE_INT ;
                         return
                       end (* then *)
                   end (* else *) ;

                 //************************************
                 // string expression                  
                 //************************************

                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;
                 if GATTR . TYPTR -> . FORM = CSTRING then
                   begin
                     if GATTR . KIND <> EXPR then
                       begin
                         LOADADDRESS ;
                         GEN2 ( PCODE_VLD , 0 , GATTR . TYPTR -> . SIZE
                                - 4 ) ;
                       end (* then *) ;
                     CTLS . VPO1_NEEDED := TRUE ;
                     GATTR . KIND := EXPR ;
                   end (* then *)
                 else
                   begin
                     CT_RESULT := COMPTYPES ( PTYPE_VARCHAR , GATTR .
                                  TYPTR ) ;
                     case CT_RESULT of
                       4 : begin
                             LOAD ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN0 ( PCODE_VC1 ) ;
                           end (* tag/ca *) ;
                       5 : begin
                             LOADADDRESS ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN1 ( PCODE_VC2 , GATTR . TYPTR -> . SIZE
                                    ) ;
                           end (* tag/ca *) ;
                       otherwise
                         begin
                           ERROR ( 342 ) ;
                           GATTR . TYPTR := NIL
                         end (* otherw *)
                     end (* case *) ;
                     GATTR . TYPTR := PTYPE_VARCHAR ;
                     GATTR . KIND := EXPR ;
                   end (* else *) ;

                 //************************************
                 // this lib function has a work area  
                 // store string as first (const) parm 
                 //************************************

                 LLC := LCWORK ;
                 LCWORK := LLC + STRSTACKSZ ;
                 GEN2 ( PCODE_LDA , LEVEL , LLC ) ;
                 GEN2 ( PCODE_VST , 1 , - 1 ) ;
                 GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL , LCPARM2 ) ;
                 CALLLIBRARYFUNC ( FCP , LCCALLER ) ;
                 GATTR . TYPTR := PTYPE_INT ;
               end (* INDEX1 *) ;


            procedure VERIFY1 ;

               var CT_RESULT : INTEGER ;
                   LLC : ADDRRANGE ;
                   LCPARM1 : ADDRRANGE ;
                   LCPARM2 : ADDRRANGE ;

               begin (* VERIFY1 *)

                 //************************************
                 // verify is implemented using a      
                 // library function (in pascal)       
                 //************************************

                 LCPARM1 := LCPARM ;                // str addr
                 LCPARM2 := LCPARM1 + PTRSIZE ;     // 2nd str addr
                 LCPARM := LCPARM2 + PTRSIZE ;

                 //************************************
                 // string expression                  
                 //************************************

                 EXPRESSION ( FSYS + [ SYCOMMA , SYRPARENT ] ) ;
                 if GATTR . TYPTR -> . FORM = CSTRING then
                   begin
                     if GATTR . KIND <> EXPR then
                       begin
                         LOADADDRESS ;
                         GEN2 ( PCODE_VLD , 0 , GATTR . TYPTR -> . SIZE
                                - 4 ) ;
                       end (* then *) ;
                     CTLS . VPO1_NEEDED := TRUE ;
                     GATTR . KIND := EXPR ;
                   end (* then *)
                 else
                   begin
                     CT_RESULT := COMPTYPES ( PTYPE_VARCHAR , GATTR .
                                  TYPTR ) ;
                     case CT_RESULT of
                       4 : begin
                             LOAD ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN0 ( PCODE_VC1 ) ;
                           end (* tag/ca *) ;
                       5 : begin
                             LOADADDRESS ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN1 ( PCODE_VC2 , GATTR . TYPTR -> . SIZE
                                    ) ;
                           end (* tag/ca *) ;
                       otherwise
                         begin
                           ERROR ( 342 ) ;
                           GATTR . TYPTR := NIL
                         end (* otherw *)
                     end (* case *) ;
                     GATTR . TYPTR := PTYPE_VARCHAR ;
                     GATTR . KIND := EXPR ;
                   end (* else *) ;

                 //************************************
                 // this lib function has a work area  
                 // store string as first (const) parm 
                 //************************************

                 LLC := LCWORK ;
                 LCWORK := LLC + STRSTACKSZ ;
                 GEN2 ( PCODE_LDA , LEVEL , LLC ) ;
                 GEN2 ( PCODE_VST , 1 , - 1 ) ;
                 GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL , LCPARM1 ) ;
                 if SY = SYCOMMA then
                   INSYMBOL
                 else
                   begin
                     ERROR ( 355 ) ;
                     if SY = SYRPARENT then
                       begin
                         GATTR . TYPTR := PTYPE_INT ;
                         return
                       end (* then *)
                   end (* else *) ;

                 //************************************
                 // string expression                  
                 //************************************

                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;
                 if GATTR . TYPTR -> . FORM = CSTRING then
                   begin
                     if GATTR . KIND <> EXPR then
                       begin
                         LOADADDRESS ;
                         GEN2 ( PCODE_VLD , 0 , GATTR . TYPTR -> . SIZE
                                - 4 ) ;
                       end (* then *) ;
                     CTLS . VPO1_NEEDED := TRUE ;
                     GATTR . KIND := EXPR ;
                   end (* then *)
                 else
                   begin
                     CT_RESULT := COMPTYPES ( PTYPE_VARCHAR , GATTR .
                                  TYPTR ) ;
                     case CT_RESULT of
                       4 : begin
                             LOAD ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN0 ( PCODE_VC1 ) ;
                           end (* tag/ca *) ;
                       5 : begin
                             LOADADDRESS ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN1 ( PCODE_VC2 , GATTR . TYPTR -> . SIZE
                                    ) ;
                           end (* tag/ca *) ;
                       otherwise
                         begin
                           ERROR ( 342 ) ;
                           GATTR . TYPTR := NIL
                         end (* otherw *)
                     end (* case *) ;
                     GATTR . TYPTR := PTYPE_VARCHAR ;
                     GATTR . KIND := EXPR ;
                   end (* else *) ;

                 //************************************
                 // this lib function has a work area  
                 // store string as first (const) parm 
                 //************************************

                 LLC := LCWORK ;
                 LCWORK := LLC + STRSTACKSZ ;
                 GEN2 ( PCODE_LDA , LEVEL , LLC ) ;
                 GEN2 ( PCODE_VST , 1 , - 1 ) ;
                 GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL , LCPARM2 ) ;
                 CALLLIBRARYFUNC ( FCP , LCCALLER ) ;
                 GATTR . TYPTR := PTYPE_INT ;
               end (* VERIFY1 *) ;


            procedure TRANSLATE1 ;

               var CT_RESULT : INTEGER ;
                   LLC : ADDRRANGE ;
                   LCPARM1 : ADDRRANGE ;
                   LCPARM2 : ADDRRANGE ;
                   LCPARM3 : ADDRRANGE ;

               begin (* TRANSLATE1 *)

                 //************************************
                 // translate is implemented using a   
                 // library function (in pascal)       
                 //************************************

                 LCPARM1 := LCPARM ;                // str addr
                 LCPARM2 := LCPARM1 + PTRSIZE ;     // 2nd str addr
                 LCPARM3 := LCPARM2 + PTRSIZE ;     // 3nd str addr
                 LCPARM := LCPARM3 + PTRSIZE ;

                 //************************************
                 // string expression                  
                 //************************************

                 EXPRESSION ( FSYS + [ SYCOMMA , SYRPARENT ] ) ;
                 if GATTR . TYPTR -> . FORM = CSTRING then
                   begin
                     if GATTR . KIND <> EXPR then
                       begin
                         LOADADDRESS ;
                         GEN2 ( PCODE_VLD , 1 , GATTR . TYPTR -> . SIZE
                                - 4 ) ;
                       end (* then *) ;
                     CTLS . VPO1_NEEDED := TRUE ;
                     GATTR . KIND := EXPR ;
                   end (* then *)
                 else
                   begin
                     CT_RESULT := COMPTYPES ( PTYPE_VARCHAR , GATTR .
                                  TYPTR ) ;
                     case CT_RESULT of
                       4 : begin
                             LOAD ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN0 ( PCODE_VC1 ) ;
                           end (* tag/ca *) ;
                       5 : begin
                             LOADADDRESS ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN1 ( PCODE_VC2 , GATTR . TYPTR -> . SIZE
                                    ) ;
                           end (* tag/ca *) ;
                       otherwise
                         begin
                           ERROR ( 342 ) ;
                           GATTR . TYPTR := NIL
                         end (* otherw *)
                     end (* case *) ;
                     GATTR . TYPTR := PTYPE_VARCHAR ;
                     GATTR . KIND := EXPR ;
                   end (* else *) ;

                 //************************************
                 // this lib function has a work area  
                 // store string as first (const) parm 
                 //************************************

                 LLC := LCWORK ;
                 LCWORK := LLC + STRSTACKSZ ;
                 GEN2 ( PCODE_LDA , LEVEL , LLC ) ;
                 GEN2 ( PCODE_VST , 1 , - 1 ) ;
                 GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL , LCPARM1 ) ;
                 if SY = SYCOMMA then
                   INSYMBOL
                 else
                   begin
                     ERROR ( 355 ) ;
                     if SY = SYRPARENT then
                       begin
                         GATTR . TYPTR := PTYPE_VARCHAR ;
                         return
                       end (* then *)
                   end (* else *) ;

                 //************************************
                 // 2nd string expression              
                 //************************************

                 EXPRESSION ( FSYS + [ SYCOMMA , SYRPARENT ] ) ;
                 if GATTR . TYPTR -> . FORM = CSTRING then
                   begin
                     if GATTR . KIND <> EXPR then
                       begin
                         LOADADDRESS ;
                         GEN2 ( PCODE_VLD , 0 , GATTR . TYPTR -> . SIZE
                                - 4 ) ;
                       end (* then *) ;
                     CTLS . VPO1_NEEDED := TRUE ;
                     GATTR . KIND := EXPR ;
                   end (* then *)
                 else
                   begin
                     CT_RESULT := COMPTYPES ( PTYPE_VARCHAR , GATTR .
                                  TYPTR ) ;
                     case CT_RESULT of
                       4 : begin
                             LOAD ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN0 ( PCODE_VC1 ) ;
                           end (* tag/ca *) ;
                       5 : begin
                             LOADADDRESS ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN1 ( PCODE_VC2 , GATTR . TYPTR -> . SIZE
                                    ) ;
                           end (* tag/ca *) ;
                       otherwise
                         begin
                           ERROR ( 342 ) ;
                           GATTR . TYPTR := NIL
                         end (* otherw *)
                     end (* case *) ;
                     GATTR . TYPTR := PTYPE_VARCHAR ;
                     GATTR . KIND := EXPR ;
                   end (* else *) ;

                 //************************************
                 // this lib function has a work area  
                 // store string as second (const) parm
                 //************************************

                 LLC := LCWORK ;
                 LCWORK := LLC + STRSTACKSZ ;
                 GEN2 ( PCODE_LDA , LEVEL , LLC ) ;
                 GEN2 ( PCODE_VST , 1 , - 1 ) ;
                 GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL , LCPARM2 ) ;
                 if SY = SYCOMMA then
                   INSYMBOL
                 else
                   begin
                     if SY = SYRPARENT then
                       begin

                 //************************************
                 // no 3rd parameter is OK             
                 // store null string as 3rd parameter 
                 //************************************

                         GEN1 ( PCODE_VC2 , 0 ) ;
                         LLC := LCWORK ;
                         LCWORK := LLC + STRSTACKSZ ;
                         GEN2 ( PCODE_LDA , LEVEL , LLC ) ;
                         GEN2 ( PCODE_VST , 1 , - 1 ) ;
                         GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL ,
                                LCPARM3 ) ;
                         CALLLIBRARYFUNC ( FCP , LCCALLER ) ;
                         GATTR . TYPTR := PTYPE_VARCHAR ;
                         return
                       end (* then *)
                   end (* else *) ;

                 //************************************
                 // 3rd string expression              
                 //************************************

                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;
                 if GATTR . TYPTR -> . FORM = CSTRING then
                   begin
                     if GATTR . KIND <> EXPR then
                       begin
                         LOADADDRESS ;
                         GEN2 ( PCODE_VLD , 0 , GATTR . TYPTR -> . SIZE
                                - 4 ) ;
                       end (* then *) ;
                     CTLS . VPO1_NEEDED := TRUE ;
                     GATTR . KIND := EXPR ;
                   end (* then *)
                 else
                   begin
                     CT_RESULT := COMPTYPES ( PTYPE_VARCHAR , GATTR .
                                  TYPTR ) ;
                     case CT_RESULT of
                       4 : begin
                             LOAD ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN0 ( PCODE_VC1 ) ;
                           end (* tag/ca *) ;
                       5 : begin
                             LOADADDRESS ;
                             CTLS . VPO1_NEEDED := TRUE ;
                             GEN1 ( PCODE_VC2 , GATTR . TYPTR -> . SIZE
                                    ) ;
                           end (* tag/ca *) ;
                       otherwise
                         begin
                           ERROR ( 342 ) ;
                           GATTR . TYPTR := NIL
                         end (* otherw *)
                     end (* case *) ;
                     GATTR . TYPTR := PTYPE_VARCHAR ;
                     GATTR . KIND := EXPR ;
                   end (* else *) ;

                 //************************************
                 // this lib function has a work area  
                 // store string as first (const) parm 
                 //************************************

                 LLC := LCWORK ;
                 LCWORK := LLC + STRSTACKSZ ;
                 GEN2 ( PCODE_LDA , LEVEL , LLC ) ;
                 GEN2 ( PCODE_VST , 1 , - 1 ) ;
                 GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL , LCPARM3 ) ;
                 CALLLIBRARYFUNC ( FCP , LCCALLER ) ;
                 GATTR . TYPTR := PTYPE_VARCHAR ;
               end (* TRANSLATE1 *) ;


            procedure ALLOC1 ;

               var LLC1 : ADDRRANGE ;

               begin (* ALLOC1 *)
                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;

                 (******************************************)
                 (* if type of expr = integer then load it *)
                 (******************************************)

                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> PTYPE_INT then
                     ERROR ( 191 )
                   else
                     LOAD ;
                 GEN3 ( PCODE_STR , ORD ( 'I' ) , LEVEL , LCPARM ) ;
                 LCPARM := LCPARM + INTSIZE ;
                 CALLLIBRARYFUNC ( FCP , LCCALLER ) ;
                 GATTR . TYPTR := PTYPE_ANY ;
               end (* ALLOC1 *) ;


            procedure ALLOCX1 ;

               var LLC1 : ADDRRANGE ;

               begin (* ALLOCX1 *)
                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;

                 (******************************************)
                 (* if type of expr = integer then load it *)
                 (******************************************)

                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> PTYPE_INT then
                     ERROR ( 191 )
                   else
                     LOAD ;
                 GEN3 ( PCODE_STR , ORD ( 'I' ) , LEVEL , LCPARM ) ;
                 LCPARM := LCPARM + INTSIZE ;
                 CALLLIBRARYFUNC ( FCP , LCCALLER ) ;
                 GATTR . TYPTR := PTYPE_ANY ;
               end (* ALLOCX1 *) ;


            procedure FREE1 ;

               begin (* FREE1 *)
                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;

                 (******************************************)
                 (* if type of expr = pointer then load it *)
                 (******************************************)

                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM <> POINTER then
                     ERROR ( 190 )
                   else
                     LOAD ;
                 GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL , LCPARM ) ;
                 LCPARM := LCPARM + PTRSIZE ;
                 CALLLIBRARYFUNC ( FCP , LCCALLER ) ;
               end (* FREE1 *) ;


            procedure FREEX1 ;

               begin (* FREEX1 *)
                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;

                 (******************************************)
                 (* if type of expr = pointer then load it *)
                 (******************************************)

                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM <> POINTER then
                     ERROR ( 190 )
                   else
                     LOAD ;
                 GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL , LCPARM ) ;
                 LCPARM := LCPARM + PTRSIZE ;
                 CALLLIBRARYFUNC ( FCP , LCCALLER ) ;
               end (* FREEX1 *) ;


            procedure CHKHEAP1 ;

               begin (* CHKHEAP1 *)
                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;

                 (******************************************)
                 (* if type of expr = integer then load it *)
                 (******************************************)

                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> PTYPE_INT then
                     ERROR ( 191 )
                   else
                     LOAD ;
                 GEN3 ( PCODE_STR , ORD ( 'I' ) , LEVEL , LCPARM ) ;
                 LCPARM := LCPARM + INTSIZE ;
                 CALLLIBRARYFUNC ( FCP , LCCALLER ) ;
               end (* CHKHEAP1 *) ;


            procedure FILEFCB1 ;

               begin (* FILEFCB1 *)
                 if SY <> IDENT then
                   ERROR ( 193 ) ;
                 FILESETUP ( INPUTPTR , FALSE ) ;
                 RWFILE := NIL ;
                 GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL , LCPARM ) ;
                 LCPARM := LCPARM + PTRSIZE ;
                 CALLLIBRARYFUNC ( FCP , LCCALLER ) ;
                 GATTR . TYPTR := PTYPE_ANY ;
               end (* FILEFCB1 *) ;


            procedure CHKALLOC1 ;

               var LLC1 : ADDRRANGE ;

               begin (* CHKALLOC1 *)
                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;

                 (******************************************)
                 (* if type of expr = pointer then load it *)
                 (******************************************)

                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM <> POINTER then
                     ERROR ( 190 )
                   else
                     LOAD ;
                 GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL , LCPARM ) ;
                 LCPARM := LCPARM + PTRSIZE ;
                 CALLLIBRARYFUNC ( FCP , LCCALLER ) ;
                 GATTR . TYPTR := PTYPE_ANY ;
               end (* CHKALLOC1 *) ;


            procedure MEMSET1 ;

            //**********************************************
            // somehow sophisticated variant of memset      
            // which generates different p-codes            
            // depending on situation                       
            //**********************************************


               var L : INTEGER ;
                   L_CONST : BOOLEAN ;
                   PATTERN : XCONSTANT ;
                   PATTERN_CONST : BOOLEAN ;
                   REVERSE : BOOLEAN ;


               procedure LOAD_PATTERN ;

                  begin (* LOAD_PATTERN *)
                    if PATTERN . STRTYPE in [ 'X' , 'B' ] then
                      GEN2 ( PCODE_LDC , 1 , PATTERN . IVAL )
                    else
                      GEN2 ( PCODE_LDC , 0 , PATTERN . IVAL )
                  end (* LOAD_PATTERN *) ;


               begin (* MEMSET1 *)
                 REVERSE := FALSE ;
                 L_CONST := FALSE ;
                 PATTERN_CONST := FALSE ;
                 EXPRESSION ( FSYS + [ SYCOMMA , SYRPARENT ] ) ;

                 (******************************************)
                 (* 1. parameter                           *)
                 (******************************************)

                 if SY = SYRPARENT then
                   begin
                     ERROR ( 197 ) ;
                     return
                   end (* then *) ;

                 (******************************************)
                 (* if type of expr = pointer then load it *)
                 (******************************************)

                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM <> POINTER then
                     ERROR ( 190 )
                   else
                     begin
                       LOAD ;
                     end (* else *) ;
                 if SY = SYCOMMA then
                   INSYMBOL ;

                 (******************************************)
                 (* 2. parameter                           *)
                 (******************************************)

                 EXPRESSION ( FSYS + [ SYCOMMA , SYRPARENT ] ) ;
                 if SY = SYRPARENT then
                   begin
                     ERROR ( 197 ) ;
                     return
                   end (* then *) ;

                 (******************************************)
                 (* if type of expr = char then load it    *)
                 (******************************************)

                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> PTYPE_CHAR then
                     ERROR ( 199 )
                   else
                     begin
                       if GATTR . KIND = CST then
                         begin
                           if FALSE then
                             begin
                               WRITELN ( 'memset 2. parameter' ) ;
                               WRITELN ( 'memset: kind = ' , GATTR .
                                         KIND ) ;
                               WRITELN ( 'memset: ival = ' , GATTR .
                                         CVAL . IVAL )
                             end (* then *) ;
                           PATTERN := GATTR . CVAL ;
                           PATTERN_CONST := TRUE ;
                         end (* then *)
                       else
                         begin
                           LOAD ;
                         end (* else *)
                     end (* else *) ;
                 if SY = SYCOMMA then
                   INSYMBOL ;

                 (******************************************)
                 (* 3. parameter                           *)
                 (******************************************)

                 EXPRESSION ( FSYS + [ SYCOMMA , SYRPARENT ] ) ;
                 if SY = SYCOMMA then
                   begin
                     ERROR ( 198 ) ;
                     SKIP ( FSYS + [ SYRPARENT ] ) ;
                     return
                   end (* then *) ;

                 (******************************************)
                 (* if type of expr = integer then load it *)
                 (******************************************)

                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> PTYPE_INT then
                     ERROR ( 191 )
                   else
                     begin
                       if GATTR . KIND = CST then
                         begin
                           if FALSE then
                             begin
                               WRITELN ( 'memset 3. parameter' ) ;
                               WRITELN ( 'memset: kind = ' , GATTR .
                                         KIND ) ;
                               WRITELN ( 'memset: ival = ' , GATTR .
                                         CVAL . IVAL )
                             end (* then *) ;
                           L_CONST := TRUE ;
                           L := GATTR . CVAL . IVAL ;
                           if L <= 0 then
                             ERROR ( 320 )
                         end (* then *)
                       else
                         begin

                 //**********************************************
                 // problem here:                                
                 // if length is not constant, but pattern       
                 // was const, then MSE has to be generated      
                 // but length is maybe already loaded by        
                 // expression call. so we need a reverse        
                 // variant of MSE (length and pattern           
                 // reversed) - opp 03.12.2017                   
                 //**********************************************

                           LOAD ;
                           if PATTERN_CONST then
                             begin
                               LOAD_PATTERN ;
                               PATTERN_CONST := FALSE ;
                               REVERSE := TRUE ;
                             end (* then *) ;
                         end (* else *) ;
                     end (* else *) ;

                 (******************************************)
                 (* generate MSE, if length is variable    *)
                 (* otherwise MFI                          *)
                 (* but: if pattern is zero, generate MZE  *)
                 (* (if pattern is constant, it has not    *)
                 (* yet been loaded, so in case of MFI,    *)
                 (* the deferred LOAD has to be done now)  *)
                 (******************************************)

                 if L_CONST then
                   begin
                     if PATTERN_CONST then
                       begin
                         if PATTERN . IVAL = 0 then
                           if L > 0 then
                             GEN1 ( PCODE_MZE , L )
                           else
                             GEN1 ( PCODE_MZE , 0 )
                         else
                           begin
                             LOAD_PATTERN ;
                             if L > 0 then
                               GEN1 ( PCODE_MFI , L )
                             else
                               GEN1 ( PCODE_MFI , 0 )
                           end (* else *)
                       end (* then *)
                     else
                       if L > 0 then
                         GEN1 ( PCODE_MFI , L )
                       else
                         GEN1 ( PCODE_MFI , 0 )
                   end (* then *)
                 else
                   if REVERSE then
                     GEN1 ( PCODE_MSE , 1 )
                   else
                     GEN1 ( PCODE_MSE , 0 ) ;
               end (* MEMSET1 *) ;


            procedure MEMCPY1 ;

            //**********************************************
            // memcpy generates inline code                 
            // P-Code mov if length is known at compile time
            // P-Code mcp otherwise                         
            //**********************************************


               var L : INTEGER ;
                   L_CONST : BOOLEAN ;

               begin (* MEMCPY1 *)
                 L_CONST := FALSE ;
                 EXPRESSION ( FSYS + [ SYCOMMA , SYRPARENT ] ) ;

                 (******************************************)
                 (* 1. parameter                           *)
                 (******************************************)

                 if SY = SYRPARENT then
                   begin
                     ERROR ( 197 ) ;
                     return
                   end (* then *) ;

                 (******************************************)
                 (* if type of expr = pointer then load it *)
                 (******************************************)

                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM <> POINTER then
                     ERROR ( 190 )
                   else
                     LOAD ;
                 if SY = SYCOMMA then
                   INSYMBOL ;

                 (******************************************)
                 (* 2. parameter                           *)
                 (******************************************)

                 EXPRESSION ( FSYS + [ SYCOMMA , SYRPARENT ] ) ;
                 if SY = SYRPARENT then
                   begin
                     ERROR ( 197 ) ;
                     return
                   end (* then *) ;

                 (******************************************)
                 (* if type of expr = pointer then load it *)
                 (******************************************)

                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM <> POINTER then
                     ERROR ( 190 )
                   else
                     LOAD ;
                 if SY = SYCOMMA then
                   INSYMBOL ;

                 (******************************************)
                 (* 3. parameter                           *)
                 (******************************************)

                 EXPRESSION ( FSYS + [ SYCOMMA , SYRPARENT ] ) ;
                 if SY = SYCOMMA then
                   begin
                     ERROR ( 198 ) ;
                     SKIP ( FSYS + [ SYRPARENT ] ) ;
                     return
                   end (* then *) ;

                 (******************************************)
                 (* if type of expr = integer then load it *)
                 (******************************************)

                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> PTYPE_INT then
                     ERROR ( 191 )
                   else
                     begin
                       if GATTR . KIND = CST then
                         begin
                           if FALSE then
                             begin
                               WRITELN ( 'memcpy 3. parameter' ) ;
                               WRITELN ( 'memcpy: kind = ' , GATTR .
                                         KIND ) ;
                               WRITELN ( 'memcpy: ival = ' , GATTR .
                                         CVAL . IVAL )
                             end (* then *) ;
                           L_CONST := TRUE ;
                           L := GATTR . CVAL . IVAL ;
                           if L <= 0 then
                             ERROR ( 320 )
                         end (* then *)
                       else
                         LOAD ;
                     end (* else *) ;

                 (******************************************)
                 (* generate MCP, if length is variable    *)
                 (* otherwise MOV                          *)
                 (******************************************)

                 if L_CONST then
                   begin
                     if L > 0 then
                       GEN1 ( PCODE_MOV , L )
                     else
                       GEN1 ( PCODE_MOV , 0 )
                   end (* then *)
                 else
                   GEN0 ( PCODE_MCP ) ;
               end (* MEMCPY1 *) ;


            procedure ROUNDX1 ;

               var LCPARM1 : ADDRRANGE ;
                   LCPARM2 : ADDRRANGE ;

               begin (* ROUNDX1 *)
                 LCPARM1 := LCPARM + INTSIZE ;
                 LCPARM2 := LCPARM ;
                 LCPARM := LCPARM + INTSIZE + REALSIZE ;
                 EXPRESSION ( FSYS + [ SYCOMMA , SYRPARENT ] ) ;

                 (******************************************)
                 (* 1. parameter                           *)
                 (******************************************)

                 if SY = SYRPARENT then
                   begin
                     ERROR ( 197 ) ;
                     return
                   end (* then *) ;

                 (******************************************)
                 (* if type of expr = pointer then load it *)
                 (******************************************)

                 if GATTR . TYPTR <> NIL then
                   if not IS_STDTYPE ( GATTR . TYPTR , 'R' ) then
                     ERROR ( 189 )
                   else
                     begin
                       LOAD ;
                       GEN3 ( PCODE_STR , ORD ( 'R' ) , LEVEL , LCPARM1
                              ) ;
                     end (* else *) ;
                 if SY = SYCOMMA then
                   INSYMBOL ;

                 (******************************************)
                 (* 2. parameter                           *)
                 (******************************************)

                 EXPRESSION ( FSYS + [ SYCOMMA , SYRPARENT ] ) ;
                 if SY = SYCOMMA then
                   begin
                     ERROR ( 198 ) ;
                     SKIP ( FSYS + [ SYRPARENT ] ) ;
                     return
                   end (* then *) ;

                 (******************************************)
                 (* if type of expr = integer then load it *)
                 (******************************************)

                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> PTYPE_INT then
                     ERROR ( 191 )
                   else
                     begin
                       LOAD ;
                       GEN3 ( PCODE_STR , ORD ( 'I' ) , LEVEL , LCPARM2
                              ) ;
                     end (* else *) ;

                 (******************************************)
                 (* call procedure                         *)
                 (******************************************)

                 CALLLIBRARYFUNC ( FCP , LCCALLER ) ;
                 GATTR . TYPTR := PTYPE_REAL ;
               end (* ROUNDX1 *) ;


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
                 LLC := LCOUNTER ;

                 (********************************)
                 (* IN CASE OF SET TYPE ARGUMENT *)
                 (********************************)

                 if GATTR . TYPTR <> PTYPE_INT then
                   ERROR ( 116 ) ;
                 if LKEY = 14 then

                 (********)
                 (*TRAP  *)
                 (********)

                   begin
                     if SY <> SYCOMMA then
                       ERROR ( 6 )
                     else
                       begin
                         INSYMBOL ;
                         EXPRESSION ( FSYS + [ SYRPARENT ] ) ;
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
                                     ALIGN ( LCOUNTER , MXDATASZE ) ;
                                     DPLMT := LCOUNTER ;
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

                 GEN1 ( PCODE_CSP , LKEY + 14 ) ;
                 LCOUNTER := LLC ;
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
                   if GATTR . TYPTR = PTYPE_INT then

                 (********)
                 (*ABI   *)
                 (*SQI   *)
                 (********)

                     GEN0 ( OP )
                   else
                     if IS_STDTYPE ( GATTR . TYPTR , 'R' ) then

                 (********)
                 (*ABR   *)
                 (*ABR   *)
                 (********)

                       GEN0 ( OP + 1 )
                     else
                       begin
                         ERROR ( 125 ) ;
                         GATTR . TYPTR := PTYPE_INT
                       end (* else *)
               end (* SQRABS *) ;


            procedure TRUNCROUND ;

               begin (* TRUNCROUND *)
                 if GATTR . TYPTR <> NIL then
                   if not IS_STDTYPE ( GATTR . TYPTR , 'R' ) then
                     ERROR ( 125 ) ;

                 (********)
                 (*TRC   *)
                 (*RND   *)
                 (********)

                 if LKEY = 18 then
                   GEN1 ( PCODE_CSP , ORD ( PTRC ) )
                 else
                   GEN1 ( PCODE_CSP , ORD ( PRND ) ) ;
                 GATTR . TYPTR := PTYPE_INT
               end (* TRUNCROUND *) ;


            procedure FLOOR1 ;

               begin (* FLOOR1 *)
                 if GATTR . TYPTR <> NIL then
                   if not IS_STDTYPE ( GATTR . TYPTR , 'R' ) then
                     ERROR ( 125 ) ;

                 (********)
                 (*FLR   *)
                 (********)

                 GEN1 ( PCODE_CSP , ORD ( PFLR ) ) ;
                 GATTR . TYPTR := PTYPE_REAL ;
               end (* FLOOR1 *) ;


            procedure EXPO1 ;

               begin (* EXPO1 *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR = PTYPE_INT then
                     GEN0 ( PCODE_FLT )
                   else
                     if not IS_STDTYPE ( GATTR . TYPTR , 'R' ) then
                       ERROR ( 125 ) ;
                 GEN0 ( PCODE_XPO ) ;
                 GATTR . TYPTR := PTYPE_INT ;
               end (* EXPO1 *) ;


            procedure CARD1 ;

               var LLC , LEN : ADDRRANGE ;

               begin (* CARD1 *)
                 LLC := LCOUNTER ;
                 EXPRESSION ( FSYS + [ SYRPARENT ] ) ;
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM = POWER then
                     begin
                       FORCETEMPSET ;
                       GEN0 ( PCODE_CRD ) ;
                     end (* then *)
                   else
                     ERROR ( 125 ) ;
                 LCOUNTER := LLC ;
                 GATTR . TYPTR := PTYPE_INT ;
               end (* CARD1 *) ;


            procedure ODD1 ;

               begin (* ODD1 *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> PTYPE_INT then
                     ERROR ( 125 ) ;
                 GEN0 ( PCODE_ODD ) ;
                 GATTR . TYPTR := PTYPE_BOOL
               end (* ODD1 *) ;


            procedure ORD1 ;

               var X : FORMTEST ;

               begin (* ORD1 *)
                 if GATTR . TYPTR <> NIL then
                   begin
                     if GATTR . TYPTR -> . FORM >= POWER then
                       ERROR ( 125 ) ;
                   end (* then *) ;
                 GEN0 ( PCODE_ORD ) ;
                 GATTR . TYPTR := PTYPE_INT
               end (* ORD1 *) ;


            procedure CHR1 ;

               begin (* CHR1 *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> PTYPE_INT then
                     ERROR ( 125 ) ;

                 (********)
                 (*CHR   *)
                 (********)

                 GEN0 ( 62 ) ;
                 GATTR . TYPTR := PTYPE_CHAR
               end (* CHR1 *) ;


            procedure PREDSUCCTIM ;

               begin (* PREDSUCCTIM *)
                 if GATTR . TYPTR <> NIL then
                   if ( LKEY = 24 ) or ( LKEY = 30 ) then
                     begin
                       if GATTR . TYPTR <> PTYPE_INT then
                         ERROR ( 116 ) ;
                       if LKEY = 24 then
                         GEN1 ( PCODE_CSP , ORD ( PCLK ) )
                       else
                         GEN1 ( PCODE_CSP , ORD ( PTRA ) )
                     end (* then *)
                   else
                     if IS_STDTYPE ( GATTR . TYPTR , 'R' ) or ( GATTR .
                     TYPTR -> . FORM <> SCALAR ) then
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
                 FILESETUP ( INPUTPTR , TRUE ) ;
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM <> FILES then
                     ERROR ( 125 ) ;
                 if LKEY >= 28 then
                   begin
                     EXTUSED := TRUE ;

                 (*****************)
                 (* CSP - EOL/EOT *)
                 (*****************)

                     GEN1 ( PCODE_CSP , LKEY - 11 ) ;
                   end (* then *)
                 else

                 (*****************)
                 (* CSP - EOF/ELN *)
                 (*****************)

                   GEN1 ( PCODE_CSP , LKEY ) ;
                 GEN1 ( PCODE_CSP , ORD ( PEIO ) ) ;
                 GATTR . TYPTR := PTYPE_BOOL
               end (* EOFEOLN *) ;


            procedure CALLNONSTANDARD ;

               var PARMID , LCP , LCP2 : IDP ;
                   LSP : TTP ;
                   LB : BOOLEAN ;
                   LOCPAR , LLC , LSIZE , LLC1 , LLC3 , LLC_PARM , LLC5
                   : ADDRRANGE ;
                   I : INTEGER ;
                   PROCNAME : ALPHA ;


               function COMPTLIST ( CP1 , CP2 : IDP ) : BOOLEAN ;

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
                          X := ( COMPTYPES ( CP1 -> . IDTYPE , CP2 -> .
                               IDTYPE ) = 1 ) ;
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


               procedure WORK_PARAMETER ;

                  var CT_RESULT : INTEGER ;
                      PARMTYPE : TTP ;
                      PARMKIND : IDKIND ;
                      PARMLEN : INTEGER ;
                      PARMALN : INTEGER ;

                      (************************************)
                      (* deutlich vereinfacht bzw. klarer *)
                      (* durch die return-Anweisungen     *)
                      (* am anfang; spart einrueckungen   *)
                      (************************************)



                  procedure WORK_PARAM_BYVALUE ;

                     var ADJUST_STRINGSIZE : BOOLEAN ;
                         STRINGSIZE_LEFT : 1 .. MAXSTRL ;
                         STRINGSIZE_RIGHT : 1 .. MAXSTRL ;
                         LLC2 : ADDRRANGE ;

                     begin (* WORK_PARAM_BYVALUE *)

                       //****************************************
                       // maybe source string has shorter size   
                       // if so, adjust size                     
                       //****************************************

                       ADJUST_STRINGSIZE := FALSE ;
                       if FALSE then
                         begin
                           WRITELN ( TRACEF ,
                                     'work_param_byvalue, loc = ' ,
                                     LINECNT ) ;
                           WRITELN ( TRACEF , 'CT_result = ' ,
                                     CT_RESULT ) ;
                           WRITELN ( TRACEF , 'PARMTYPE -> . FORM = ' ,
                                     PARMTYPE -> . FORM ) ;
                         end (* then *) ;
                       if CT_RESULT in [ 2 , 3 ] then
                         begin
                           if IS_CARRAY ( GATTR . TYPTR ) or ( GATTR .
                           TYPTR = PTYPE_CHAR ) then
                             if GATTR . KIND = CST then
                               begin
                                 MOD_STRCONST ( CT_RESULT , GATTR .
                                                CVAL , GATTR . TYPTR ,
                                                PARMTYPE -> . SIZE ) ;
                               end (* then *)
                             else
                               begin
                                 ADJUST_STRINGSIZE := TRUE ;
                                 STRINGSIZE_LEFT := PARMTYPE -> . SIZE
                                                   ;
                                 STRINGSIZE_RIGHT := GATTR . TYPTR -> .
                                                   SIZE ;
                               end (* else *)
                           else
                             begin
                               CT_RESULT := 0
                             end (* else *)
                         end (* then *) ;

                       //***************************************
                       // start dummy loop to limit indentation 
                       //***************************************

                       repeat
                         if PARMTYPE -> . FORM < POWER then
                           begin
                             LOAD ;
                             if OPT . DEBUG then
                               begin
                                 ASSIGN := TRUE ;
                                 CHKBNDS ( PARMTYPE ) ;
                                 ASSIGN := FALSE ;
                               end (* then *) ;
                             if COMPTYPES ( PTYPE_REAL , PARMTYPE ) = 1
                             then
                               if ( GATTR . TYPTR = PTYPE_INT ) then
                                 begin
                                   GEN0 ( PCODE_FLT ) ;
                                   GATTR . TYPTR := PTYPE_REAL ;
                                   GATTR . BTYPE := PTYPE_REAL ;
                                   CT_RESULT := 1 ;
                                 end (* then *) ;
                             LOCPAR := LOCPAR + 1 ;

                       //*********************************************
                       // if dummy argument needed (for const param   
                       // or fortran ...)                             
                       //*********************************************

                             if PARMID -> . DUMMYVAR then
                               begin
                                 with GATTR do
                                   begin
                                     VLEVEL := PARMID -> . DUMMYLEV ;
                                     DPLMT := PARMID -> . DUMMYADDR ;
                                     BTYPE := PARMTYPE ;
                                     KIND := VARBL ;
                                     ACCESS := DRCT
                                   end (* with *) ;
                                 STORE ( GATTR ) ;
                                 LOADADDRESS ;
                                 GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL
                                        , LLC_PARM ) ;
                               end (* then *)

                       //***************************************
                       // normal case - no dummy argument       
                       //***************************************

                             else
                               begin
                                 GEN3 ( PCODE_STR , GETTYPE ( PARMTYPE
                                        ) , LEVEL , LLC_PARM ) ;
                                 LSIZE := PARMTYPE -> . SIZE ;
                               end (* else *) ;
                             break ;
                           end (* then *) ;

                       //***************************************
                       //  PARMTYPE -> . FORM >= POWER          
                       //***************************************

                         LOCPAR := LOCPAR + 1 ;

                       //*********************************************
                       // for dummy arguments:                        
                       // all complex types are passed by reference ! 
                       //*********************************************

                         if FALSE then
                           begin
                             WRITELN ( TRACEF , 'parmid.dummyvar    = '
                                       , PARMID -> . DUMMYVAR ) ;
                             WRITELN ( TRACEF , 'gattr.access       = '
                                       , GATTR . ACCESS ) ;
                             WRITELN ( TRACEF , 'gattr.kind         = '
                                       , GATTR . KIND ) ;
                           end (* then *) ;
                         if PARMID -> . DUMMYVAR then
                           begin
                             case PARMTYPE -> . FORM of
                               POWER : begin
                                         LLC2 := PARMID -> . DUMMYADDR
                                                 ;
                                         FORCETEMPSET ;
                                         LSIZE := OPNDSETSIZE ( GATTR )
                                                  ;
                                         GEN2 ( PCODE_LDA , LEVEL ,
                                                LLC2 ) ;
                                         GEN2 ( PCODE_SMV , - PARMTYPE
                                                -> . SIZE , LSIZE ) ;
                                         GEN2 ( PCODE_LDA , LEVEL ,
                                                LLC2 ) ;
                                       end (* tag/ca *) ;
                               CSTRING :
                                 begin
                                   if GATTR . KIND = VARBL then
                                     begin
                                       LOADADDRESS ;
                                       GEN2 ( PCODE_VLD , 1 , GATTR .
                                              TYPTR -> . SIZE - 4 ) ;
                                       CTLS . VPO1_NEEDED := TRUE ;
                                       LLC2 := PARMID -> . DUMMYADDR ;
                                       GEN2 ( PCODE_LDA , PARMID -> .
                                              DUMMYLEV , LLC2 ) ;
                                       GEN2 ( PCODE_VST , 1 , - 1 ) ;
                                     end (* then *)
                                   else
                                     begin
                                       LLC2 := PARMID -> . DUMMYADDR ;
                                       if FALSE then
                                         begin
                                           WRITELN ( TRACEF ,
                                               'ct_result          = '
                                                   , CT_RESULT ) ;
                                           WRITELN ( TRACEF ,
                                               'llc2               = '
                                                   , LLC2 ) ;
                                         end (* then *) ;
                                       case CT_RESULT of
                                         4 : begin
                                               CTLS . VPO1_NEEDED :=
                                                   TRUE ;
                                               GEN0 ( PCODE_VC1 ) ;
                                             end (* tag/ca *) ;
                                         5 , 7 : begin
                                                   LOADADDRESS ;
                                                   CTLS . VPO1_NEEDED
                                                   := TRUE ;
                                                   GEN1 ( PCODE_VC2 ,
                                                   GATTR . TYPTR -> .
                                                   SIZE ) ;
                                                 end (* tag/ca *) ;
                                         otherwise
                                           
                                       end (* case *) ;
                                       GEN2 ( PCODE_LDA , PARMID -> .
                                              DUMMYLEV , LLC2 ) ;
                                       GEN2 ( PCODE_VST , 1 , - 1 ) ;
                                     end (* else *)
                                 end (* tag/ca *) ;
                               otherwise
                                 begin
                                   LOADADDRESS
                                 end (* otherw *)
                             end (* case *) ;
                             GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL ,
                                    LLC_PARM ) ;
                             LSIZE := 0 ;
                             break ;
                           end (* then *) ;

                       //***************************************
                       // no dummy argument - now for sets      
                       //***************************************

                         if PARMTYPE -> . FORM = POWER then
                           begin
                             LSIZE := OPNDSETSIZE ( GATTR ) ;
                             LOADADDRESS ;
                             GEN2 ( PCODE_LDA , LEVEL , LLC_PARM ) ;
                             GEN2 ( PCODE_SMV , - PARMTYPE -> . SIZE ,
                                    LSIZE ) ;
                             break ;
                           end (* then *) ;

                       //***************************************
                       // no dummy argument - now for cstring   
                       //***************************************

                         if PARMTYPE -> . FORM = CSTRING then
                           begin
                             case CT_RESULT of
                               4 : begin
                                     CTLS . VPO1_NEEDED := TRUE ;
                                     GEN0 ( PCODE_VC1 ) ;
                                   end (* tag/ca *) ;
                               5 : begin
                                     LOADADDRESS ;
                                     CTLS . VPO1_NEEDED := TRUE ;
                                     GEN1 ( PCODE_VC2 , GATTR . TYPTR
                                            -> . SIZE ) ;
                                   end (* tag/ca *) ;
                               otherwise
                                 begin
                                   LOADADDRESS ;
                                   GEN2 ( PCODE_VLD , 0 , GATTR . TYPTR
                                          -> . SIZE - 4 ) ;
                                 end (* otherw *)
                             end (* case *) ;
                             GEN2 ( PCODE_LDA , LEVEL , LLC_PARM ) ;
                             GEN2 ( PCODE_VST , 1 , PARMTYPE -> . SIZE
                                    - 4 ) ;
                             break ;
                           end (* then *) ;

                       //*********************************************
                       // no dummy argument - other structured types  
                       //*********************************************

                         LOADADDRESS ;
                         GEN2 ( PCODE_LDA , LEVEL , LLC_PARM ) ;
                         if ADJUST_STRINGSIZE then
                           begin
                             if FALSE then
                               begin
                                 WRITELN ( TRACEF ,
                                           'loc linecnt      = ' ,
                                           LINECNT ) ;
                                 WRITELN ( TRACEF ,
                                           'adjust_stringsz  = ' ,
                                           ADJUST_STRINGSIZE ) ;
                                 WRITELN ( TRACEF ,
                                           'stringsize_left  = ' ,
                                           STRINGSIZE_LEFT ) ;
                                 WRITELN ( TRACEF ,
                                           'stringsize_right = ' ,
                                           STRINGSIZE_RIGHT ) ;
                               end (* then *) ;

                       //*****************************************
                       // set fill pattern to blank               
                       // and generate MFI                        
                       // target address has to be reloaded       
                       // because MFI has popped it               
                       //*****************************************

                             GEN2 ( PCODE_LDC , 0 , ORD ( ' ' ) ) ;
                             GEN1 ( PCODE_MFI , STRINGSIZE_LEFT ) ;
                             GEN2 ( PCODE_LDA , LEVEL , LLC_PARM ) ;

                       //*****************************************
                       //  copy string, but only using            
                       //  length of right side.                  
                       //*****************************************

                             GEN1 ( PCODE_MOV , - STRINGSIZE_RIGHT ) ;
                           end (* then *)
                         else
                           GEN1 ( PCODE_MOV , - PARMTYPE -> . SIZE ) ;
                         LSIZE := PARMTYPE -> . SIZE ;
                       until TRUE ;

                       //************************
                       // end of dummy loop      
                       //************************

                       if not ( CT_RESULT in [ 1 , 2 , 3 , 4 , 5 , 7 ]
                       ) then
                         begin
                           ERROR ( 142 ) ;
                         end (* then *)
                     end (* WORK_PARAM_BYVALUE *) ;


                  procedure WORK_PARAM_BYADDR ;

                     begin (* WORK_PARAM_BYADDR *)
                       if GATTR . ACCESS = STKEXPR then
                         ERROR ( 154 ) ;
                       LOADADDRESS ;
                       if PARMTYPE -> . FORM = CSTRING then
                         begin

                       //*****************************************
                       // for strings:                            
                       // make sure that maxlength is set         
                       //*****************************************

                           if not GATTR . BTYPE -> . CONFORMANT then
                             GEN1 ( PCODE_VSM , GATTR . BTYPE -> . SIZE
                                    - 4 ) ;
                         end (* then *) ;
                       if FCP -> . EXTLANG <> 'F' then
                         begin
                           GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL ,
                                  LLC_PARM ) ;
                           LSIZE := PTRSIZE ;
                         end (* then *)
                       else
                         begin
                           GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL ,
                                  LLC3 ) ;
                           LLC3 := LLC3 + PTRSIZE ;
                         end (* else *) ;
                       LOCPAR := LOCPAR + 1 ;

                       //***********************************************
                       // in some situations no error 182               
                       // for example:                                  
                       // anyfile - all files match                     
                       // conformant strings - all sizes match          
                       //***********************************************

                       if GATTR . BTYPE -> . SIZE <> PARMTYPE -> . SIZE
                       then
                         begin
                           if PARMTYPE = PTYPE_ANYFILE then
                             return ;
                           if PARMTYPE -> . FORM = CSTRING then
                             if PARMTYPE -> . CONFORMANT then
                               return ;
                           ERROR ( 182 ) ;
                         end (* then *)
                     end (* WORK_PARAM_BYADDR *) ;


                  begin (* WORK_PARAMETER *)
                    if FALSE then
                      begin
                        WRITELN ( TRACEF , 'work_parameter, loc = ' ,
                                  LINECNT ) ;
                      end (* then *) ;

                    /********************************************/
                    /* expression                               */
                    /********************************************/

                    EXPRESSION ( FSYS + [ SYCOMMA , SYRPARENT ] ) ;
                    if GATTR . TYPTR = NIL then
                      return ;

                    /********************************************/
                    /* new 01.2017:                             */
                    /* if PARMID = nil, that is, there is no    */
                    /* description of parameters because of a   */
                    /* missing procedure declaration, then the  */
                    /* known attributes up until now will be    */
                    /* used for the expression.                 */
                    /********************************************/

                    PARMLEN := 0 ;
                    PARMALN := 0 ;
                    if PARMID <> NIL then
                      begin
                        PARMTYPE := PARMID -> . IDTYPE ;
                        PARMKIND := PARMID -> . VKIND ;
                      end (* then *)
                    else
                      begin
                        PARMTYPE := GATTR . TYPTR ;
                        if GATTR . KIND = VARBL then
                          PARMKIND := VARPARM
                        else
                          PARMKIND := NORMALVAR ;
                      end (* else *) ;
                    if PARMTYPE = NIL then
                      return ;

                    //******************************
                    // now we have a parmtype       
                    //******************************

                    if PARMKIND in [ VARPARM , CONSTPARM ] then
                      begin
                        PARMLEN := PTRSIZE ;
                        PARMALN := PTRSIZE ;
                      end (* then *)
                    else
                      if PARMTYPE <> NIL then
                        begin
                          PARMLEN := PARMTYPE -> . SIZE ;
                          PARMALN := PARMTYPE -> . ALN
                        end (* then *) ;

                    (*******************************************)
                    (* ausrichten parm-offset entsprechend typ *)
                    (*******************************************)

                    ALIGN ( LLC_PARM , PARMALN ) ;
                    LSIZE := 0 ;

                    (**********************)
                    (* typen vergleichen  *)
                    (**********************)

                    if FALSE then
                      begin
                        WRITELN ( TRACEF ,
                                  'work_parameter, before comptypes' )
                                  ;
                        WRITELN ( TRACEF , 'loc = ' , LINECNT ) ;
                        WRITELN ( TRACEF , 'parmtype.form       = ' ,
                                  PARMTYPE -> . FORM ) ;
                        if PARMTYPE -> . FORM = CSTRING then
                          WRITELN ( TRACEF , 'parmtype.conformant = ' ,
                                    PARMTYPE -> . CONFORMANT ) ;
                        WRITELN ( TRACEF , 'gattr.form          = ' ,
                                  GATTR . TYPTR -> . FORM ) ;
                      end (* then *) ;
                    CT_RESULT := COMPTYPES ( PARMTYPE , GATTR . TYPTR )
                                 ;
                    if FALSE then
                      WRITELN ( TRACEF , 'ct_result           = ' ,
                                CT_RESULT ) ;

                    //**************************************************
                    // if parmkind = normalvar or constparm:            
                    // call work_param_byvalue                          
                    // if the parameter is a const parm,                
                    // the boolean dummyvar is set, and the             
                    // procedure generates code for this case           
                    // (true with fortran for every byvalue parm)       
                    //**************************************************

                    if PARMKIND in [ NORMALVAR , CONSTPARM ] then
                      begin
                        WORK_PARAM_BYVALUE ;
                        return ;
                      end (* then *) ;

                    //**************************************************
                    // if parmkind = varparm, every parameter           
                    // must be a variable                               
                    //**************************************************

                    if GATTR . KIND = VARBL then
                      begin
                        WORK_PARAM_BYADDR ;
                      end (* then *)
                    else
                      ERROR ( 154 ) ;

                    (**********************************)
                    (* hier muss der Typ genau passen *)
                    (**********************************)

                    if CT_RESULT <> 1 then
                      ERROR ( 142 ) ;
                  end (* WORK_PARAMETER *) ;


               procedure WORK_PROCEDURE_PARM ;

                  begin (* WORK_PROCEDURE_PARM *)
                    if SY <> IDENT then
                      begin
                        ERROR ( 2 ) ;
                        SKIP ( FSYS + [ SYCOMMA , SYRPARENT ] )
                      end (* then *)
                    else
                      begin
                        SID_RC := SEARCHID ( ID , TRUE , TRUE , [
                                  PARMID -> . KLASS ] , LCP ) ;
                        if not COMPTLIST ( LCP , PARMID ) then
                          ERROR ( 128 )
                        else
                          begin
                            LOCAL_CALL := TRUE ;

                    //******************************
                    // => UPDATES DISP REGS         
                    //******************************

                            LLC_PARM := LLC1 + PARMID -> . PFLEV DIV 10
                                        ;
                            LCOUNTER := LLC_PARM ;

                    //*******************************************
                    // PFLEV = ADDR OF PROC IN NEW ACTIV RECORD  
                    //*******************************************

                            LSIZE := DISPAREA ;
                            if LCP -> . PFKIND = NORMALVAR then
                              with LCP -> do
                                begin
                                  if EXTLANG = 'F' then
                                    begin

                    //******************************
                    // REMEMBER THIS PROC FOR LATER 
                    //******************************

                                      LCP2 := FRTPARHD ;
                                      while TRUE do
                                        begin
                                          if LCP2 = NIL then
                                            begin
                                              NEW ( LCP2 , PROC ,
                                                   DECLARED ) ;
                                              LCP2 -> := LCP -> ;
                                              with LCP2 -> do
                                                begin
                                                  NAME :=
                                                   '            ' ;
                                                  EXTRN := FALSE ;
                                                  EXTLANG := ' ' ;
                                                  UNPACK ( EXTNAME ,
                                                   NAME , 1 ) ;
                                                  PROCLAB := PROCLAB +
                                                   1 ;
                                                  PFNAME := PROCLAB ;
                                                  NXTFWRD := FRTPARHD ;
                                                  FRTPARHD := LCP2 ;
                                                end (* with *) ;
                                              break
                                            end (* then *) ;

                    //******************************
                    // already in list              
                    //******************************

                                          if LCP2 -> . EXTNAME =
                                          EXTNAME then
                                            break ;
                                          LCP2 := LCP2 -> . NXTFWRD ;
                                        end (* while *) ;
                                      LCP := LCP2 ;
                                    end (* then *) ;

                    //******************************
                    // PASSING PROC                 
                    //******************************

                                  UNPACK ( EXTNAME , ID , 1 ) ;
                                  GEN1 ( PCODE_LCA , ORD ( 'P' ) ) ;
                                  GEN3 ( PCODE_STR , ORD ( 'A' ) ,
                                         LEVEL , LLC_PARM ) ;
                                  GEN2 ( PCODE_LDA , LEVEL , LLC_PARM +
                                         PTRSIZE ) ;
                                  GEN2 ( PCODE_LDA , 1 , DISPADR +
                                         PTRSIZE ) ;
                                  GEN1 ( PCODE_MOV , DISPAREA - PTRSIZE
                                         ) ;
                                end (* with *)
                            else

                    //*****************************************
                    // PROC PARM IS ITSELF A PASSED PROC       
                    //*****************************************

                              begin
                                GEN2 ( PCODE_LDA , LEVEL , LLC_PARM ) ;

                    //*****************************************
                    // COPY ENTIRE PROC RECORD INTO PARM LIST  
                    //*****************************************

                                GEN2 ( PCODE_LDA , LEVEL , LCP -> .
                                       PFLEV DIV 10 ) ;
                                GEN1 ( PCODE_MOV , DISPAREA ) ;
                              end (* else *) ;
                            LLC_PARM := LLC_PARM + LSIZE ;
                          end (* else *) ;
                        INSYMBOL ;
                        if not ( SY in FSYS + [ SYCOMMA , SYRPARENT ] )
                        then
                          begin
                            ERROR ( 6 ) ;
                            SKIP ( FSYS + [ SYCOMMA , SYRPARENT ] )
                          end (* then *)
                      end (* else *)
                  end (* WORK_PROCEDURE_PARM *) ;


               begin (* CALLNONSTANDARD *)
                 LOCPAR := 0 ;
                 LLC1 := LCOUNTER ;
                 ALIGN ( LLC1 , MXDATASZE ) ;
                 LLC_PARM := LLC1 + LCAFTMST + FPSAVEAREA ;
                 with FCP -> do
                   begin
                     PARMID := PRMPTR ;
                     if PFLEV > LCAFTMST then

                 //************************************
                 // PROCedure parameter                
                 //************************************

                       begin
                         LLC5 := LLC1 ;
                         LLC1 := LLC1 + DISPAREA
                       end (* then *)
                     else
                       LLC5 := 0 ;
                     GEN2 ( PCODE_MST , PFLEV , LLC5 ) ;
                     if PFLEV = LEVEL then
                       LOCAL_CALL := TRUE ;
                   end (* with *) ;

                 //************************************
                 // if sylparent:                      
                 // parameter list begins              
                 //************************************

                 if SY = SYLPARENT then
                   begin
                     LSIZE := 0 ;
                     LLC := LLC1 ;

                 //*************************************************
                 // now storage is reserved for const parameters    
                 // (dummy arguments). if extlang = 'F' (fortran),  
                 // all byvalue parameter are treated like          
                 // const parameters (dummy arguments created,      
                 // if necessary, passed by reference)              
                 //*************************************************

                     LCP := PARMID ;
                     while LCP <> NIL do
                       begin
                         LCP -> . DUMMYVAR := FALSE ;
                         LCP -> . DUMMYLEV := 0 ;
                         LCP -> . DUMMYADDR := 0 ;
                         if ( LCP -> . VKIND = CONSTPARM ) or ( ( LCP
                         -> . VKIND = NORMALVAR ) and ( FCP -> .
                         EXTLANG = 'F' ) ) then
                           begin
                             if FALSE then
                               begin
                                 WRITELN ( TRACEF , 'lcp.name      = '
                                           , LCP -> . NAME ) ;
                                 WRITELN ( TRACEF , 'lcp.vkind     = '
                                           , LCP -> . VKIND ) ;
                                 WRITELN ( TRACEF , 'lcp.vlev      = '
                                           , LCP -> . VLEV ) ;
                                 WRITELN ( TRACEF , 'lcp.stklass   = '
                                           , LCP -> . STKLASS ) ;
                                 WRITELN ( TRACEF , 'lcp.vaddr     = '
                                           , LCP -> . VADDR ) ;
                               end (* then *) ;
                             LSP := LCP -> . IDTYPE ;
                             if LSP <> NIL then
                               begin
                                 if LSP -> . FORM <= POWER then
                                   begin
                                     ALIGN ( LLC1 , LSP -> . ALN ) ;
                                     LCP -> . DUMMYVAR := TRUE ;
                                     LCP -> . DUMMYLEV := LEVEL ;
                                     LCP -> . DUMMYADDR := LLC1 ;
                                     LLC1 := LLC1 + LSP -> . SIZE
                                   end (* then *)
                                 else
                                   if LSP -> . FORM = CSTRING then
                                     begin
                                       ALIGN ( LLC1 , LSP -> . ALN ) ;
                                       LCP -> . DUMMYVAR := TRUE ;
                                       LCP -> . DUMMYLEV := LEVEL ;
                                       LCP -> . DUMMYADDR := LLC1 ;
                                       LLC1 := LLC1 + STRSTACKSZ ;
                                     end (* then *)
                               end (* then *) ;
                           end (* then *) ;
                         LCP := LCP -> . NEXT
                       end (* while *) ;
                     ALIGN ( LLC1 , MXDATASZE ) ;
                     LLC3 := LLC1 ;

                 //************************************
                 // parameter abarbeiten               
                 //************************************

                     repeat
                       LB := FALSE ;

                 //********************************************
                 // DECIDE WHETHER PROC/FUNC MUST BE PASSED    
                 //********************************************

                       if PARMID = NIL then
                         begin
                           if not FCP -> . DECLMISSING then
                             ERROR ( 126 )
                         end (* then *)
                       else
                         LB := PARMID -> . KLASS in [ PROC , FUNC ] ;

                 //********************************************
                 // FOR varparm PROC/FUNC LB IS FALSE AND      
                 // EXPRESSION WILL BE CALLED, WHICH WILL      
                 // ALWAYS INTERPRET A PROC/FUNC ID AT         
                 // ITS BEGINNING AS A CALL RATHER THAN        
                 // A PARAMETER PASSING. IN THIS               
                 // IMPLEMENTATION, PARAMETER PROCEDURES       
                 // /FUNCTIONS ARE THEREFORE NOT ALLOWED       
                 // TO HAVE PROCEDURE/FUNCTION PARAMETERS      
                 //********************************************

                       INSYMBOL ;
                       if LB then
                         WORK_PROCEDURE_PARM
                       else
                         begin

                 //**********************************************
                 // neue funktion work_parameter                 
                 // setzt u.a. LLC_PARM und LSIZE ...            
                 // hier muesste eigentlich LLC_PARM mit         
                 // dem Wert aus parmid -> uebereinstimmen,      
                 //                                              
                 // ist aber nicht immer so ...                  
                 //**********************************************

                           if PARMID <> NIL then
                             LLC_PARM := LLC1 + PARMID -> . VADDR ;
                           LCOUNTER := LLC_PARM ;
                           WORK_PARAMETER ;
                           LLC_PARM := LLC_PARM + LSIZE ;
                         end (* else *) ;
                       if ( PARMID <> NIL ) then
                         PARMID := PARMID -> . NEXT
                     until SY <> SYCOMMA ;
                     LCOUNTER := LLC_PARM ;
                     if LCOUNTER > LCMAX then
                       LCMAX := LCOUNTER ;
                     LCOUNTER := LLC ;
                     if SY = SYRPARENT then
                       INSYMBOL
                     else
                       ERROR ( 4 )
                   end (* then *) ;
                 LOCPAR := LOCPAR * 2 ;
                 if PARMID <> NIL then
                   if not FCP -> . DECLMISSING then
                     ERROR ( 126 ) ;
                 with FCP -> do
                   begin
                     if OPT . SAVEFPRS then
                       LOCPAR := LOCPAR + 1 ;

                 //************************************
                 // ENCODE SAVEFPR FLG                 
                 //************************************

                     if OPT . PRCODE then
                       begin
                         PUTIC ;
                         WRITE ( PRR , MN [ PCODE_CUP ] ) ;
                         WRITE ( PRR , PROCTYPE ( FCP ) : 2 ) ;
                         if FCP -> . EXTLANG <> ' ' then
                           WRITE ( PRR , FCP -> . EXTLANG ) ;
                         WRITE ( PRR , ',' , LOCPAR : 1 ) ;
                         WRITE ( PRR , ',' , EXTNAME ) ;
                         WRITELN ( PRR , ',' , LLC1 : 1 ) ;
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

              (************************************************)
              (*  if library routine, that is:                *)
              (*  implemented using external pascal module    *)
              (*  insert funccode as first parameter          *)
              (*  for external procedure                      *)
              (*  opp / 06.2016                               *)
              (************************************************)

                  if FCP -> . LIBNAME [ 1 ] <> ' ' then
                    begin
                      PREPLIBRARYFUNC ( FCP -> . WASIZE , LCCALLER ,
                                        LCPARM , LCWORK ) ;
                      GEN2 ( PCODE_LDC , 1 , FCP -> . FUNCCODE ) ;
                      GEN3 ( PCODE_STR , ORD ( 'I' ) , LEVEL , LCPARM )
                             ;
                      LCPARM := LCPARM + INTSIZE ;
                    end (* then *) ;

              (************************************************)
              (*  opp / 06.2016 - end insertion               *)
              (************************************************)

                  LKEY := FCP -> . KEY ;
                  if SY = SYLPARENT then
                    begin
                      INSYMBOL ;
                      MATCHPAR := TRUE ;

              (*****************************************************)
              (*  diese Prozeduren kommen auch ohne Parameter aus  *)
              (*****************************************************)

                      if SY = SYRPARENT then
                        if not ( LKEY in [ 0 , 1 , 2 , 3 , 4 , 11 , 12
                        , 25 , 26 , 28 , 29 , 83 , 84 , 92 ] ) then
                          ERROR ( 7 ) ;
                    end (* then *)
                  else
                    begin

              (*****************************************************)
              (*  diese Prozeduren kommen auch ohne Parameter aus  *)
              (*****************************************************)

                      if not ( LKEY in [ 0 , 1 , 2 , 3 , 4 , 11 , 12 ,
                      25 , 26 , 28 , 29 , 83 , 84 , 92 ] ) then
                        ERROR ( 7 ) ;
                      MATCHPAR := FALSE ;
                    end (* else *) ;
                  if LKEY in [ 14 .. 24 , 30 , 33 , 39 , 47 ] then

              (*********************************)
              (*TRAP,EXIT,ABS...,TRACE,ODD,EXPO*)
              (*********************************)

                    begin

              (********)
              (*TRAP  *)
              (********)

                      if LKEY = 14 then
                        EXPRESSION ( FSYS + [ SYCOMMA ] )
                      else
                        EXPRESSION ( FSYS + [ SYRPARENT ] ) ;
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
                    40 : ADDR1 ;
                    41 : PTRADD1 ;
                    42 : PTRDIFF1 ;
                    43 : SIZEOF1 ;
                    44 : PTR2INT1 ;
                    45 : PTRCAST1 ;
                    46 : GETPUTRESETREWRITE ;
                    47 : FLOOR1 ;
                    61 : ALLOC1 ;
                    62 : ALLOCX1 ;
                    63 : FREE1 ;
                    64 : FREEX1 ;
                    65 : CHKHEAP1 ;
                    66 : CHKALLOC1 ;
                    70 : FILEFCB1 ;
                    75 : MEMSET1 ;
                    76 : MEMCPY1 ;
                    77 : ROUNDX1 ;
                    78 : DIGITSOF1 ;
                    79 : PRECISIONOF1 ;
                    80 : STR1 ;
                    81 : MAXLENGTH1 ;
                    82 : LENGTH1 ;
                    83 : STRRESULT1 ;
                    84 : STRRESULTP1 ;
                    85 : REPEATSTR1 ;
                    86 : SUBSTR1 ;
                    87 : DELETE1 ;
                    88 : RTRIM1 ;
                    89 : LTRIM1 ;
                    90 : TRIM1 ;
                    91 : COMPRESS1 ;
                    92 : RESULTP1 ;
                    93 : INDEX1 ;
                    94 : VERIFY1 ;
                    95 : TRANSLATE1 ;
                  end (* case *) ;
                  if LKEY in [ 16 .. 26 , 28 , 29 , 33 , 38 , 39 , 40 ,
                  41 , 42 , 43 , 44 , 47 , 63 , 64 , 78 , 79 ] then
                    GATTR . BTYPE := GATTR . TYPTR ;
                  if MATCHPAR then
                    if SY = SYRPARENT then
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
                  if ( TYPTR -> . FORM = POWER ) and ( COMPTYPES (
                  TYPTR , LATTR . TYPTR ) = 1 ) then
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
                      LCOUNTER := STKDPLMT + STKLEN ;
                      if LCOUNTER > LCMAX then
                        LCMAX := LCOUNTER ;
                    end (* then *)
                  else
                    begin
                      ERROR ( 134 ) ;
                      GATTR . TYPTR := NIL ;
                      GATTR . BTYPE := NIL
                    end (* else *)
            end (* GENSETOP *) ;


         procedure EXPRESSION ;

         //******************************************************
         // expressions are used to generate boolean conditions  
         // (if they are not only simple expressions)            
         //                                                      
         // they usually generate one out of six comparison      
         // p-code instructions:                                 
         //                                                      
         // PCODE_EQU = 47                                       
         // PCODE_GEQ = 48                                       
         // PCODE_GRT = 49                                       
         // PCODE_LEQ = 52                                       
         // PCODE_LES = 53                                       
         // PCODE_NEQ = 55                                       
         //                                                      
         // these p-code instructions have type flags:           
         //                                                      
         // TYPIND := 'R' - real                                 
         // TYPIND := 'B' - boolean                              
         // TYPIND := 'C' - char                                 
         // TYPIND := 'I' - integer                              
         // TYPIND := 'A' - pointer (address)                    
         // TYPIND := 'S' - set                                  
         // TYPIND := 'V' - varchar (new 12.2017)                
         // TYPIND := 'M' - memory (char array)                  
         // TYPIND := 'F' - file (not supported)                 
         //                                                      
         // normally it is made sure that both sides of the      
         // comparison have the same type, by doing the right    
         // conversions before.                                  
         //******************************************************


            const COMPARE_OP : array [ SYEQOP .. SYLEOP ] of 0 .. OPMAX
                  =
                  ( PCODE_EQU , PCODE_NEQ , PCODE_GRT , PCODE_LES ,
                    PCODE_GEQ , PCODE_LEQ ) ;

            var LATTR : ATTR ;
                LOP : SYMB ;
                TYPIND : CHAR ;
                LLC , LSIZE : ADDRRANGE ;
                CT_RESULT : INTEGER ;
                STRING_ON_STACK : BOOLEAN ;


            procedure SIMPLEEXPRESSION ( FSYS : SYMSET ) ;

               var LATTR : ATTR ;
                   LOP : SYMB ;
                   SIGNED : BOOLEAN ;
                   CT_RESULT : INTEGER ;
                   TT : TTP ;


               procedure TERM ( FSYS : SYMSET ) ;

                  var LATTR : ATTR ;
                      LOP : SYMB ;


                  procedure FACTOR ( FSYS : SYMSET ) ;

                     var LCP : IDP ;
                         SETVAL : XCONSTANT ;
                         LVP : SSP ;
                         LATTR : ATTR ;
                         LSP : TTP ;
                         I , J : INTEGER ;
                         TS_LC : ADDRRANGE ;
                         HEXORBIN : CHAR ;
                         SETLOCAL_LC , SETVAR_SIZE : ADDRRANGE ;
                         SETOFFS_LOCAL : INTEGER ;
                         NOCHMAL : BOOLEAN ;
                         PSI : PSETINFO ;
                         SETMIN_DUMMY : INTEGER ;
                         SETMAX_DUMMY : INTEGER ;
                         NOCHMALS : BOOLEAN ;
                         LSTRING : INTEGER ;


                     procedure FACT_KONST ;

                        begin (* FACT_KONST *)
                          with GATTR , LCP -> do
                            begin
                              TYPTR := IDTYPE ;
                              KIND := CST ;
                              CVAL := VALUES ;
                              if SY in [ SYLBRACK , SYLPARENT ] then
                                if IS_CARRAY ( TYPTR ) then
                                  begin
                                    if SY = SYLPARENT then
                                      begin
                                        ERRKIND := 'W' ;
                                        ERROR ( 11 )
                                      end (* then *) ;
                                    LATTR := GATTR ;
                                    LOADADDRESS ;
                                    INSYMBOL ;
                                    EXPRESSION ( FSYS + [ SYRBRACK ,
                                                 SYRPARENT ] ) ;
                                    LOAD ;
                                    LSP := LATTR . TYPTR -> . INXTYPE ;
                                    if COMPTYPES ( TYPTR , LSP ) <> 1
                                    then
                                      begin
                                        WRITELN ( TRACEF ,
                                                  'error 139/2: ' ,
                                                  TYPTR , ' ' , LSP ) ;
                                        ERROR ( 139 )
                                      end (* then *)
                                    else
                                      begin
                                        if LSP <> NIL then
                                          GETBOUNDS ( LSP , I , J )
                                        else
                                          begin
                                            I := 1 ;
                                            J := LATTR . TYPTR -> .
                                                 SIZE
                                          end (* else *) ;
                                        if OPT . DEBUG then
                                          GEN3 ( PCODE_CHK , ORD ( 'J'
                                                 ) , I , J DIV CHARSIZE
                                                 ) ;
                                        if I <> 0 then
                                          GEN2 ( PCODE_DEC , ORD ( 'I'
                                                 ) , I ) ;
                                        GEN1 ( PCODE_IXA , CHARSIZE ) ;
                                      end (* else *) ;
                                    TYPTR := PTYPE_CHAR ;
                                    KIND := VARBL ;
                                    BTYPE := PTYPE_CHAR ;
                                    ACCESS := INDRCT ;
                                    IDPLMT := 0 ;
                                    if SY = SYRBRACK then
                                      INSYMBOL
                                    else
                                      if SY = SYRPARENT then
                                        begin
                                          ERRKIND := 'W' ;
                                          ERROR ( 12 ) ;
                                          INSYMBOL ;
                                        end (* then *)
                                      else
                                        ERROR ( 12 ) ;
                                  end (* then *)
                            end (* with *)
                        end (* FACT_KONST *) ;


                     function FACT_SET_UP : BOOLEAN ;

                        var OK : BOOLEAN ;
                            NOCHMAL : BOOLEAN ;
                            SETMIN_DUMMY : INTEGER ;
                            SETMAX_DUMMY : INTEGER ;

                        begin (* FACT_SET_UP *)

                          (************************************)
                          (* typ nur merken, wenn subrange,   *)
                          (* skalar, oder char                *)
                          (************************************)

                          if ( LSP -> . ELSET = NIL ) and ( GATTR .
                          TYPTR <> PTYPE_INT ) then
                            LSP -> . ELSET := GATTR . TYPTR ;
                          if GATTR . KIND = CST then
                            begin

                          (******************************)
                          (* set_const_part, that is:   *)
                          (* single constant or         *)
                          (* range of constants         *)
                          (******************************)

                              NOCHMAL := SET_CONST_PART ( GATTR . TYPTR
                                         , GATTR . CVAL , PSI ) ;
                              PSI -> . CONST_IN_SET := PSI -> .
                                                   CONST_IN_SET + 1 ;
                              FACT_SET_UP := NOCHMAL ;
                            end (* then *)
                          else
                            begin
                              LOAD ;
                              if GATTR . TYPTR <> PTYPE_INT then
                                GEN0 ( PCODE_ORD ) ;
                              if PSI -> . VARS_IN_SET = 0 then
                                begin
                                  FACT_SET_UP := FALSE ;

                          (*****************************************)
                          (* ALLOCATE STORAGE                      *)
                          (* ------------------------------------  *)
                          (* 21.09.2017: muss maxsetl sein;        *)
                          (* es gibt keine andere chance ...       *)
                          (*****************************************)

                                  SETVAR_SIZE := MAXSETL ;
                                  if GATTR . TYPTR <> NIL then
                                    if GATTR . TYPTR <> PTYPE_INT then
                                      CALC_SETTYPSIZE ( GATTR . TYPTR ,
                                                   SETVAR_SIZE ,
                                                   SETMIN_DUMMY ,
                                                   SETMAX_DUMMY ,
                                                   SETOFFS_LOCAL ) ;
                                  if SETVAR_SIZE > MAXSETL then
                                    SETVAR_SIZE := MAXSETL ;
                                  ALIGN ( LCOUNTER , WORDSIZE ) ;
                                  TS_LC := LCOUNTER ;
                                  GEN2 ( PCODE_SCL , SETVAR_SIZE ,
                                         LCOUNTER ) ;
                                  LCOUNTER := LCOUNTER + SETVAR_SIZE ;
                                  if LCOUNTER > LCMAX then
                                    LCMAX := LCOUNTER ;
                                  PSI -> . VARS_IN_SET := PSI -> .
                                                   VARS_IN_SET + 1 ;
                                  GEN1 ( PCODE_ASE , - SETVAR_SIZE ) ;
                                  FACT_SET_UP := FALSE ;
                                end (* then *)
                              else
                                begin
                                  GEN1 ( PCODE_ASE , SETVAR_SIZE ) ;
                                  FACT_SET_UP := FALSE ;
                                end (* else *)
                            end (* else *)
                        end (* FACT_SET_UP *) ;


                     procedure SPECIAL_WORK ( LCP : IDP ) ;

                        begin (* SPECIAL_WORK *)
                          if LCP -> . KLASS = VARS then
                            case LCP -> . SPECIAL of
                              1 : begin
                                    GEN1 ( PCODE_CSP , ORD ( PDAT ) )
                                  end (* tag/ca *) ;
                              2 : begin
                                    GEN1 ( PCODE_CSP , ORD ( PTIM ) ) ;
                                  end (* tag/ca *) ;
                            end (* case *) ;
                        end (* SPECIAL_WORK *) ;


                     procedure FACTOR_IDENT ;

                        var DUMMYB : BOOLEAN ;

                        begin (* FACTOR_IDENT *)
                          SID_RC := SEARCHID ( ID , FALSE , FALSE , [
                                    STRUCTKONST , KONST , VARS , FIELD
                                    , FUNC ] , LCP ) ;
                          STARTID := ID ;
                          ERRLINE_SAVE := SCB . LINENR ;
                          ERRPOS_SAVE := SCB . LINEPOS ;
                          INSYMBOL ;

                          /************************************/
                          /* add id of appropriate class      */
                          /* to prevent more errors           */
                          /*   in case of notfound            */
                          /* special error code,              */
                          /*   if undecl. function            */
                          /************************************/

                          if SID_RC = 104 then
                            if SY = SYLPARENT then
                              begin
                                ERROR_POS ( 'W' , 186 , ' ' ,
                                            ERRLINE_SAVE , ERRPOS_SAVE
                                            ) ;
                                SID_RC := SEARCHID ( STARTID , FALSE ,
                                          TRUE , [ FUNC ] , LCP ) ;
                                LCP -> . IDTYPE := PTYPE_INT ;
                                LCP -> . KLASS := FUNC ;
                                LCP -> . DECLMISSING := TRUE ;
                                LCP -> . EXTRN := TRUE ;
                                PACK ( STARTID , 1 , LCP -> . EXTNAME )
                                       ;
                              end (* then *)
                            else
                              begin
                                ERROR_POS ( 'E' , 104 , ' ' ,
                                            ERRLINE_SAVE , ERRPOS_SAVE
                                            ) ;
                                SID_RC := SEARCHID ( STARTID , FALSE ,
                                          TRUE , [ VARS ] , LCP ) ;
                              end (* else *) ;
                          if LCP <> NIL then
                            SPECIAL_WORK ( LCP ) ;
                          if LCP -> . KLASS = FUNC then
                            begin

                          //********************************************
                          // string workarea used, if function          
                          // called with string result type             
                          // vpo type 1 needed at end of stmt           
                          //********************************************

                              if LCP -> . IDTYPE <> NIL then
                                begin
                                  if LCP -> . IDTYPE -> . FORM =
                                  CSTRING then
                                    CTLS . VPO1_NEEDED := TRUE ;
                                end (* then *) ;
                              CALL ( FSYS , LCP ) ;
                              if ( STARTID <> 'SIZEOF' ) and ( STARTID
                              <> 'DIGITSOF' ) and ( STARTID <>
                              'PRECISIONOF' ) then
                                GATTR . KIND := EXPR
                            end (* then *)
                          else
                            if LCP -> . KLASS = KONST then
                              FACT_KONST
                            else
                              begin
                                SELECTOR ( FSYS , LCP , TRUE , DUMMYB )
                                           ;
                              end (* else *)
                        end (* FACTOR_IDENT *) ;


                     begin (* FACTOR *)
                       if not ( SY in FACBEGSYS ) then
                         begin
                           ERROR ( 58 ) ;
                           SKIP ( FSYS + FACBEGSYS ) ;
                           GATTR . TYPTR := NIL
                         end (* then *) ;
                       while SY in FACBEGSYS do
                         begin
                           GATTR . CVAL . IVAL := 0 ;
                           GATTR . CVAL . STRTYPE := ' ' ;
                           LSTRING := 0 ;
                           repeat

                       /**********************************/
                       /* loop using nochmals switch for */
                       /* concatenation of stringconst   */
                       /**********************************/

                             NOCHMALS := FALSE ;
                             case SY of
                               IDENT : FACTOR_IDENT ;
                               INTCONST :
                                 begin
                                   with GATTR do
                                     begin
                                       TYPTR := PTYPE_INT ;
                                       KIND := CST ;
                                       CVAL := VAL
                                     end (* with *) ;
                                   INSYMBOL
                                 end (* tag/ca *) ;
                               REALCONST :
                                 begin
                                   with GATTR do
                                     begin
                                       if SYDIGITS > 0 then
                                         begin
                                           TYPTR := PTYPE_DECIMAL ;
                                           MODIFY_TYPE_PARMS ( TYPTR ,
                                                   'DECIMAL' , SYDIGITS
                                                   , SYPREC ) ;
                                         end (* then *)
                                       else
                                         TYPTR := PTYPE_REAL ;
                                       KIND := CST ;
                                       CVAL := VAL
                                     end (* with *) ;
                                   INSYMBOL
                                 end (* tag/ca *) ;
                               STRINGCONST :
                                 with GATTR do
                                   begin
                                     KIND := CST ;
                                     if LSTRING + SYLENGTH > MAXSTRL
                                     then
                                       ERROR ( 395 )
                                     else
                                       STRCONCAT ( CVAL , VAL , LSTRING
                                                   , SYLENGTH ) ;
                                     INSYMBOL ;
                                     NOCHMALS := ( SY = STRINGCONST ) ;
                                     if not NOCHMALS then
                                       if LSTRING <= 1 then
                                         TYPTR := PTYPE_CHAR
                                       else
                                         begin
                                           NEW ( LSP , ARRAYS ) ;
                                           with LSP -> do
                                             begin
                                               ERRORFLAG := FALSE ;
                                               AELTYPE := PTYPE_CHAR ;
                                               FORM := ARRAYS ;
                                               INXTYPE := NIL ;
                                               SIZE := LSTRING ;
                                               ALN := CHARSIZE ;
                                             end (* with *) ;
                                           TYPTR := LSP
                                         end (* else *)
                                   end (* with *) ;
                               SYLPARENT :
                                 begin
                                   INSYMBOL ;
                                   EXPRESSION ( FSYS + [ SYRPARENT ] )
                                                ;
                                   if SY = SYRPARENT then
                                     INSYMBOL
                                   else
                                     ERROR ( 4 )
                                 end (* tag/ca *) ;
                               SYNOT : begin
                                         INSYMBOL ;
                                         FACTOR ( FSYS ) ;
                                         LOAD ;
                                         if GATTR . TYPTR <> NIL then
                                           if GATTR . TYPTR =
                                           PTYPE_BOOL then
                                             GEN2 ( PCODE_NOT , ORD (
                                                   'B' ) , 0 )
                                           else
                                             if GATTR . TYPTR =
                                             PTYPE_INT then
                                               GEN2 ( PCODE_NOT , ORD (
                                                   'I' ) , 0 )
                                             else
                                               begin
                                                 ERROR ( 135 ) ;
                                                 GATTR . TYPTR := NIL
                                               end (* else *) ;
                                       end (* tag/ca *) ;
                               SYLBRACK :
                                 begin
                                   PSI := PSIGLOB ;
                                   PSI -> . ELEMCOUNT := 0 ;
                                   PSI -> . SETMIN := 0 ;
                                   PSI -> . SETMAX := 0 ;
                                   PSI -> . RANGEERR := 0 ;
                                   PSI -> . CHARTYPE := FALSE ;
                                   PSI -> . HEXORBIN := ' ' ;
                                   PSI -> . VARS_IN_SET := 0 ;
                                   PSI -> . CONST_IN_SET := 0 ;
                                   INSYMBOL ;

                       /**********************************************/
                       /* store set values in array of size          */
                       /* setmaxsize                                 */
                       /* compute setmin and setmax, while reading   */
                       /* constant elements; offset at the end       */
                       /* build set string at the end                */
                       /**********************************************/

                                   NEW ( LSP , POWER ) ;
                                   with LSP -> do
                                     begin
                                       ERRORFLAG := FALSE ;
                                       ELSET := NIL ;
                                       SIZE := 0 ;
                                       SETMIN := 0 ;
                                       SETMAX := 0 ;
                                       SETOFFS := 0 ;
                                       FORM := POWER
                                     end (* with *) ;
                                   if SY = SYRBRACK then
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
                                         EXPRESSION ( FSYS + [ SYCOMMA
                                                   , SYDOTDOT ,
                                                   SYRBRACK ] ) ;
                                         NOCHMAL := FALSE ;
                                         if GATTR . TYPTR <> NIL then
                                           if GATTR . TYPTR -> . FORM
                                           <> SCALAR then
                                             begin
                                               ERROR ( 136 ) ;
                                               GATTR . TYPTR := NIL
                                             end (* then *)
                                           else
                                             if COMPTYPES ( LSP -> .
                                             ELSET , GATTR . TYPTR ) =
                                             1 then
                                               NOCHMAL := FACT_SET_UP
                                             else
                                               ERROR ( 137 ) ;

                       /*******************************/
                       /* nochmal may already be true */
                       /* and sy neq comma            */
                       /* - set by fact_set_up        */
                       /*******************************/

                                         if SY = SYCOMMA then
                                           begin
                                             INSYMBOL ;
                                             NOCHMAL := TRUE ;
                                           end (* then *)
                                       until not NOCHMAL ;
                                       if SY = SYRBRACK then
                                         INSYMBOL
                                       else
                                         ERROR ( 12 ) ;
                                       if FALSE then
                                         begin
                                           WRITELN ( TRACEF ) ;
                                           WRITELN ( TRACEF ,
                                                   'linecnt = ' ,
                                                   LINECNT : 1 ) ;
                                           WRITELN ( TRACEF ,
                                                   'psi.elemcount = ' ,
                                                   PSI -> . ELEMCOUNT )
                                                   ;
                                           WRITELN ( TRACEF ,
                                                   'psi.setmin    = ' ,
                                                   PSI -> . SETMIN ) ;
                                           WRITELN ( TRACEF ,
                                                   'psi.setmax    = ' ,
                                                   PSI -> . SETMAX ) ;
                                           WRITELN ( TRACEF ,
                                                   'psi.rangeerr  = ' ,
                                                   PSI -> . RANGEERR )
                                                   ;
                                           for I := 1 to SETMAXSIZE do
                                             if PSI -> . SETELEMS [ I ]
                                             then
                                               WRITELN ( TRACEF ,
                                                   'in set        = ' ,
                                                   PSI -> . SETMIN + I
                                                   - 1 ) ;
                                         end (* then *) ;
                                     end (* else *) ;
                                   if PSI -> . VARS_IN_SET > 0 then
                                     begin
                                       if PSI -> . CONST_IN_SET > 0
                                       then
                                         begin
                                           if LSP -> . ELSET =
                                           PTYPE_CHAR then
                                             BUILD_SETCONST ( SETVAL ,
                                                   PSI , LSP -> . ELSET
                                                   )
                                           else
                                             BUILD_SETCONST ( SETVAL ,
                                                   PSI , NIL ) ;
                                           LVP := SETVAL . PVAL ;
                                           ALIGN ( LCOUNTER , WORDSIZE
                                                   ) ;
                                           GEN_LCA_S ( LSP -> . ELSET ,
                                                   SETVAL ) ;
                                           GEN2 ( PCODE_SLD , LVP -> .
                                                  LENGTH , LCOUNTER ) ;
                                           GEN0 ( PCODE_UNI ) ;
                                           if LVP -> . LENGTH >
                                           SETVAR_SIZE then
                                             SETVAR_SIZE := LVP -> .
                                                   LENGTH ;
                                           if ( SETVAR_SIZE + LCOUNTER
                                           ) > LCMAX then
                                             LCMAX := SETVAR_SIZE +
                                                   LCOUNTER ;
                                         end (* then *) ;
                                       GATTR . KIND := VARBL ;
                                       GATTR . ACCESS := STKEXPR ;
                                       GATTR . STKDPLMT := TS_LC ;
                                       GATTR . STKLEN := SETVAR_SIZE ;
                                       LSP -> . SIZE := SETVAR_SIZE ;
                                     end (* then *)
                                   else
                                     begin
                                       if LSP -> . ELSET = PTYPE_CHAR
                                       then
                                         BUILD_SETCONST ( SETVAL , PSI
                                                   , LSP -> . ELSET )
                                       else
                                         BUILD_SETCONST ( SETVAL , PSI
                                                   , NIL ) ;
                                       LVP := SETVAL . PVAL ;
                                       LSP -> . SIZE := LVP -> . LENGTH
                                                   ;
                                       GATTR . KIND := CST ;
                                       GATTR . CVAL := SETVAL ;
                                     end (* else *) ;
                                   GATTR . TYPTR := LSP ;
                                 end (* tag/ca *)
                             end (* case *)
                           until not NOCHMALS ;
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
                    FACTOR ( FSYS + FACTOROPS ) ;
                    while SY in FACTOROPS do
                      begin
                        if GATTR . TYPTR <> NIL then
                          if GATTR . TYPTR -> . FORM < POWER then
                            LOAD
                          else
                            FORCETEMPSET ;
                        LATTR := GATTR ;
                        LOP := SY ;
                        INSYMBOL ;
                        FACTOR ( FSYS + FACTOROPS ) ;
                        if GATTR . TYPTR <> NIL then
                          if GATTR . TYPTR -> . FORM < POWER then
                            LOAD ;
                        if ( LATTR . TYPTR <> NIL ) and ( GATTR . TYPTR
                        <> NIL ) then
                          case LOP of
                            SYMULT :
                              if ( LATTR . TYPTR = PTYPE_INT ) and (
                              GATTR . TYPTR = PTYPE_INT ) then
                                GEN0 ( PCODE_MPI )
                              else
                                begin
                                  if GATTR . TYPTR = PTYPE_INT then
                                    begin
                                      GEN0 ( PCODE_FLT ) ;
                                      GATTR . TYPTR := PTYPE_REAL
                                    end (* then *)
                                  else
                                    if LATTR . TYPTR = PTYPE_INT then
                                      begin
                                        GEN0 ( PCODE_FLO ) ;
                                        LATTR . TYPTR := PTYPE_REAL
                                      end (* then *) ;
                                  if IS_STDTYPE ( LATTR . TYPTR , 'R' )
                                  and IS_STDTYPE ( GATTR . TYPTR , 'R'
                                  ) then
                                    GEN0 ( PCODE_MPR )
                                  else
                                    GENSETOP ( LATTR , PCODE_INT ) ;
                                end (* else *) ;
                            SYSLASH :
                              begin
                                if GATTR . TYPTR = PTYPE_INT then
                                  begin
                                    GEN0 ( PCODE_FLT ) ;
                                    GATTR . TYPTR := PTYPE_REAL
                                  end (* then *) ;
                                if LATTR . TYPTR = PTYPE_INT then
                                  begin
                                    GEN0 ( PCODE_FLO ) ;
                                    LATTR . TYPTR := PTYPE_REAL
                                  end (* then *) ;
                                if IS_STDTYPE ( LATTR . TYPTR , 'R' )
                                and IS_STDTYPE ( GATTR . TYPTR , 'R' )
                                then
                                  GEN0 ( PCODE_DVR )
                                else
                                  begin
                                    ERROR ( 134 ) ;
                                    GATTR . TYPTR := NIL
                                  end (* else *)
                              end (* tag/ca *) ;
                            SYDIV : if ( LATTR . TYPTR = PTYPE_INT )
                                    and ( GATTR . TYPTR = PTYPE_INT )
                                    then
                                      GEN0 ( PCODE_DVI )
                                    else
                                      begin
                                        ERROR ( 134 ) ;
                                        GATTR . TYPTR := NIL
                                      end (* else *) ;
                            SYMOD : if ( LATTR . TYPTR = PTYPE_INT )
                                    and ( GATTR . TYPTR = PTYPE_INT )
                                    then
                                      GEN0 ( 14 )
                                    else
                                      begin
                                        ERROR ( 134 ) ;
                                        GATTR . TYPTR := NIL
                                      end (* else *) ;
                            SYANDOP , SYAND :
                              if ( LATTR . TYPTR = PTYPE_BOOL ) and (
                              GATTR . TYPTR = PTYPE_BOOL ) then
                                GEN2 ( PCODE_AND , ORD ( 'B' ) , 0 )
                              else
                                if ( LATTR . TYPTR = PTYPE_INT ) and (
                                GATTR . TYPTR = PTYPE_INT ) then
                                  GEN2 ( PCODE_AND , ORD ( 'I' ) , 0 )
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
                 if SY in [ SYPLUS , SYMINUS ] then
                   begin
                     SIGNED := ( SY = SYMINUS ) ;
                     INSYMBOL
                   end (* then *) ;

                 //************************************
                 // handle first term                  
                 //************************************

                 TERM ( FSYS + TERMOPS ) ;

                 //************************************
                 // check, if there was sign           
                 // before first term and take         
                 // appropriate action                 
                 //************************************

                 if SIGNED then
                   begin
                     LOAD ;
                     if GATTR . TYPTR = PTYPE_INT then
                       GEN0 ( PCODE_NGI )
                     else
                       if IS_STDTYPE ( GATTR . TYPTR , 'R' ) then
                         GEN0 ( PCODE_NGR )
                       else
                         begin
                           ERROR ( 134 ) ;
                           GATTR . TYPTR := NIL
                         end (* else *)
                   end (* then *) ;

                 //************************************
                 // check for more terms               
                 //************************************

                 while SY in TERMOPS do
                   begin

                 //*****************************************
                 // load first term                         
                 // somehow complicated for strings         
                 // (in case of concatenation)              
                 // maybe some conversion has to be done    
                 //*****************************************

                     if GATTR . TYPTR <> NIL then
                       if GATTR . TYPTR -> . FORM < POWER then
                         LOAD
                       else
                         if GATTR . TYPTR -> . FORM = POWER then
                           FORCETEMPSET ;
                     if SY = SYCONCAT then
                       begin
                         if not STRING_ON_STACK then
                           begin
                             if GATTR . TYPTR -> . FORM = CSTRING then
                               begin
                                 if FALSE then
                                   begin
                                     WRITELN ( TRACEF ,
                                               'simpleex loc = ' ,
                                               LINECNT : 1 ) ;
                                     WRITELN ( TRACEF ,
                                             'gattr.kind           = '
                                               , GATTR . KIND ) ;
                                     TT := GATTR . TYPTR ;
                                     WRITELN ( TRACEF ,
                                             'tt -> . FORM         = '
                                               , TT -> . FORM ) ;
                                   end (* then *) ;
                                 if GATTR . KIND <> EXPR then
                                   begin
                                     LOADADDRESS ;
                                     GEN2 ( PCODE_VLD , 0 , GATTR .
                                            TYPTR -> . SIZE - 4 )
                                   end (* then *)
                               end (* then *)
                             else
                               begin
                                 CT_RESULT := COMPTYPES ( PTYPE_VARCHAR
                                              , GATTR . TYPTR ) ;
                                 case CT_RESULT of
                                   4 : begin
                                         CTLS . VPO1_NEEDED := TRUE ;
                                         GEN0 ( PCODE_VC1 ) ;
                                       end (* tag/ca *) ;
                                   5 : begin
                                         LOADADDRESS ;
                                         CTLS . VPO1_NEEDED := TRUE ;
                                         GEN1 ( PCODE_VC2 , GATTR .
                                                TYPTR -> . SIZE ) ;
                                       end (* tag/ca *) ;
                                   otherwise
                                     
                                 end (* case *) ;
                               end (* else *) ;
                             STRING_ON_STACK := TRUE ;
                           end (* then *) ;
                       end (* then *) ;

                 //*****************************************
                 // move attributes of first or compound    
                 // term to lattr, sy to lop                
                 // handle next term                        
                 // attributes of next term to go gattr     
                 //*****************************************

                     LATTR := GATTR ;
                     LOP := SY ;
                     INSYMBOL ;
                     TERM ( FSYS + TERMOPS ) ;
                     if GATTR . TYPTR <> NIL then
                       if GATTR . TYPTR -> . FORM < POWER then
                         LOAD ;
                     if ( LATTR . TYPTR <> NIL ) and ( GATTR . TYPTR <>
                     NIL ) then

                 //************************************
                 // work on term operations            
                 //************************************

                       case LOP of

                 //************************************
                 // plus operator                      
                 // valid on integers, reals and sets  
                 //************************************

                         SYPLUS :
                           if ( LATTR . TYPTR = PTYPE_INT ) and ( GATTR
                           . TYPTR = PTYPE_INT ) then
                             GEN0 ( PCODE_ADI )
                           else
                             begin
                               if GATTR . TYPTR = PTYPE_INT then
                                 begin
                                   GEN0 ( PCODE_FLT ) ;
                                   GATTR . TYPTR := PTYPE_REAL
                                 end (* then *)
                               else
                                 if LATTR . TYPTR = PTYPE_INT then
                                   begin
                                     GEN0 ( PCODE_FLO ) ;
                                     LATTR . TYPTR := PTYPE_REAL
                                   end (* then *) ;
                               if IS_STDTYPE ( LATTR . TYPTR , 'R' )
                               and IS_STDTYPE ( GATTR . TYPTR , 'R' )
                               then
                                 GEN0 ( PCODE_ADR )
                               else
                                 GENSETOP ( LATTR , PCODE_UNI ) ;
                             end (* else *) ;

                 //************************************
                 // minus operator                     
                 // valid on integers, reals and sets  
                 //************************************

                         SYMINUS :
                           if ( LATTR . TYPTR = PTYPE_INT ) and ( GATTR
                           . TYPTR = PTYPE_INT ) then
                             GEN0 ( PCODE_SBI )
                           else
                             begin
                               if GATTR . TYPTR = PTYPE_INT then
                                 begin
                                   GEN0 ( PCODE_FLT ) ;
                                   GATTR . TYPTR := PTYPE_REAL
                                 end (* then *)
                               else
                                 if LATTR . TYPTR = PTYPE_INT then
                                   begin
                                     GEN0 ( PCODE_FLO ) ;
                                     LATTR . TYPTR := PTYPE_REAL
                                   end (* then *) ;
                               if IS_STDTYPE ( LATTR . TYPTR , 'R' )
                               and IS_STDTYPE ( GATTR . TYPTR , 'R' )
                               then
                                 GEN0 ( PCODE_SBR )
                               else
                                 GENSETOP ( LATTR , PCODE_DIF ) ;
                             end (* else *) ;

                 //***************************************
                 // or operator                           
                 // valid on booleans and integers (new)  
                 //***************************************

                         SYOROP , SYOR :
                           if ( LATTR . TYPTR = PTYPE_BOOL ) and (
                           GATTR . TYPTR = PTYPE_BOOL ) then
                             GEN2 ( PCODE_IOR , ORD ( 'B' ) , 0 )
                           else
                             if ( LATTR . TYPTR = PTYPE_INT ) and (
                             GATTR . TYPTR = PTYPE_INT ) then
                               GEN2 ( PCODE_IOR , ORD ( 'I' ) , 0 )
                             else
                               begin
                                 ERROR ( 134 ) ;
                                 GATTR . TYPTR := NIL
                               end (* else *) ;

                 //************************************
                 // xor operator (new)                 
                 // valid on booleans and integers     
                 //************************************

                         SYXOR : if ( LATTR . TYPTR = PTYPE_BOOL ) and
                                 ( GATTR . TYPTR = PTYPE_BOOL ) then
                                   begin
                                     GEN2 ( PCODE_XOR , ORD ( 'B' ) , 0
                                            )
                                   end (* then *)
                                 else
                                   if ( LATTR . TYPTR = PTYPE_INT ) and
                                   ( GATTR . TYPTR = PTYPE_INT ) then
                                     begin
                                       GEN2 ( PCODE_XOR , ORD ( 'I' ) ,
                                              0 )
                                     end (* then *)
                                   else
                                     begin
                                       ERROR ( 134 ) ;
                                       GATTR . TYPTR := NIL
                                     end (* else *) ;

                 //*******************************************
                 // concatenation operator                    
                 // valid on strings only                     
                 // same conversions as above on first term   
                 //*******************************************

                         SYCONCAT :
                           if GATTR . TYPTR -> . FORM = CSTRING then
                             begin
                               if FALSE then
                                 begin
                                   WRITELN ( TRACEF , 'simpleex loc = '
                                             , LINECNT : 1 ) ;
                                   WRITELN ( TRACEF ,
                                             'gattr.kind           = '
                                             , GATTR . KIND ) ;
                                   TT := GATTR . TYPTR ;
                                   WRITELN ( TRACEF ,
                                             'tt -> . FORM         = '
                                             , TT -> . FORM ) ;
                                 end (* then *) ;
                               if GATTR . KIND <> EXPR then
                                 begin
                                   LOADADDRESS ;
                                   GEN2 ( PCODE_VLD , 0 , GATTR . TYPTR
                                          -> . SIZE - 4 ) ;
                                 end (* then *) ;
                               CTLS . VPO1_NEEDED := TRUE ;
                               GEN0 ( PCODE_VCC ) ;
                               GATTR . KIND := EXPR ;
                             end (* then *)
                           else
                             begin
                               CT_RESULT := COMPTYPES ( PTYPE_VARCHAR ,
                                            GATTR . TYPTR ) ;
                               case CT_RESULT of
                                 4 : begin
                                       CTLS . VPO1_NEEDED := TRUE ;
                                       GEN0 ( PCODE_VC1 ) ;
                                       GEN0 ( PCODE_VCC ) ;
                                     end (* tag/ca *) ;
                                 5 : begin
                                       LOADADDRESS ;
                                       CTLS . VPO1_NEEDED := TRUE ;
                                       GEN1 ( PCODE_VC2 , GATTR . TYPTR
                                              -> . SIZE ) ;
                                       GEN0 ( PCODE_VCC ) ;
                                     end (* tag/ca *) ;
                                 otherwise
                                   begin
                                     ERROR ( 342 ) ;
                                     GATTR . TYPTR := NIL
                                   end (* otherw *)
                               end (* case *) ;
                               GATTR . TYPTR := PTYPE_VARCHAR ;
                               GATTR . KIND := EXPR ;
                             end (* else *)
                       end (* case *)
                     else
                       GATTR . TYPTR := NIL ;
                     GATTR . BTYPE := GATTR . TYPTR ;
                   end (* while *)
               end (* SIMPLEEXPRESSION *) ;


            begin (* EXPRESSION *)
              LLC := LCOUNTER ;

              //******************************************
              // handle first simple expression           
              //******************************************

              STRING_ON_STACK := FALSE ;
              SIMPLEEXPRESSION ( FSYS + EXPROPS ) ;

              //***************************************************
              // if symbol in exprops (that is: compare operator)  
              // look for more simple expressions                  
              // first load the first one                          
              // and put gattr to lattr                            
              //***************************************************

              if SY in EXPROPS then
                begin
                  if GATTR . TYPTR <> NIL then
                    if GATTR . TYPTR -> . FORM < POWER then
                      LOAD
                    else
                      if GATTR . TYPTR -> . FORM = POWER then
                        FORCETEMPSET
                      else
                        if GATTR . TYPTR -> . FORM <> CSTRING then
                          LOADADDRESS
                        else
                          begin
                            if GATTR . KIND <> EXPR then
                              begin
                                LOADADDRESS ;
                                GEN2 ( PCODE_VLD , 0 , GATTR . TYPTR ->
                                       . SIZE - 4 )
                              end (* then *)
                          end (* else *) ;
                  LATTR := GATTR ;
                  LOP := SY ;
                  if LOP = SYIN then
                    if GATTR . TYPTR <> PTYPE_INT then
                      GEN0 ( PCODE_ORD ) ;

              //******************************************
              // handle second simple expression          
              //******************************************

                  INSYMBOL ;
                  STRING_ON_STACK := FALSE ;
                  SIMPLEEXPRESSION ( FSYS ) ;

              //******************************************
              // handle strings of different length       
              //******************************************

                  if ( LATTR . TYPTR <> NIL ) and ( GATTR . TYPTR <>
                  NIL ) then
                    begin
                      CT_RESULT := COMPTYPES ( LATTR . TYPTR , GATTR .
                                   TYPTR ) ;

              //******************************************
              // maybe source string has shorter size     
              // if so, adjust size                       
              //******************************************

                      if CT_RESULT in [ 2 , 3 ] then
                        begin
                          if ( GATTR . KIND = CST ) and ( IS_CARRAY (
                          GATTR . TYPTR ) or ( GATTR . TYPTR =
                          PTYPE_CHAR ) ) then
                            begin
                              MOD_STRCONST ( CT_RESULT , GATTR . CVAL ,
                                             GATTR . TYPTR , LATTR .
                                             TYPTR -> . SIZE ) ;
                            end (* then *)
                        end (* then *) ;
                    end (* then *) ;

              //******************************************
              // first load the second simple expression  
              //******************************************

                  if GATTR . TYPTR <> NIL then
                    if GATTR . TYPTR -> . FORM < POWER then
                      LOAD
                    else
                      if GATTR . TYPTR -> . FORM = POWER then
                        FORCETEMPSET
                      else
                        if GATTR . TYPTR -> . FORM <> CSTRING then
                          LOADADDRESS ;

              //******************************************
              // dummy one time loop                      
              //******************************************

                  repeat
                    if ( LATTR . TYPTR = NIL ) or ( GATTR . TYPTR = NIL
                    ) then
                      break ;

              //******************************************
              // handle in operator                       
              //******************************************

                    if LOP = SYIN then
                      begin
                        if GATTR . TYPTR -> . FORM = POWER then
                          if COMPTYPES ( LATTR . TYPTR , GATTR . TYPTR
                          -> . ELSET ) = 1 then
                            GEN0 ( PCODE_INN )
                          else
                            begin
                              ERROR ( 129 ) ;
                              GATTR . TYPTR := NIL
                            end (* else *)
                        else
                          begin
                            ERROR ( 130 ) ;
                            GATTR . TYPTR := NIL
                          end (* else *) ;
                        break ;
                      end (* then *) ;

              //******************************************
              // expand integer to float if necessary     
              //******************************************

                    if LATTR . TYPTR <> GATTR . TYPTR then
                      if GATTR . TYPTR = PTYPE_INT then
                        begin
                          GEN0 ( PCODE_FLT ) ;
                          GATTR . TYPTR := PTYPE_REAL
                        end (* then *)
                      else
                        if LATTR . TYPTR = PTYPE_INT then
                          begin
                            GEN0 ( PCODE_FLO ) ;
                            LATTR . TYPTR := PTYPE_REAL
                          end (* then *) ;

              //******************************************
              // check type compatibility left to right   
              // and generate compare instruction         
              // using the appropriate type flag          
              //******************************************

                    CT_RESULT := COMPTYPES ( LATTR . TYPTR , GATTR .
                                 TYPTR ) ;
                    if CT_RESULT in [ 1 , 2 , 4 , 5 , 6 ] then
                      begin
                        LSIZE := LATTR . TYPTR -> . SIZE ;
                        case LATTR . TYPTR -> . FORM of

              //******************************************
              // scalar: different flags depending on type
              //******************************************

                          SCALAR :
                            if IS_STDTYPE ( LATTR . TYPTR , 'R' ) then
                              TYPIND := 'R'
                            else
                              if LATTR . TYPTR = PTYPE_BOOL then
                                TYPIND := 'B'
                              else
                                if LATTR . TYPTR = PTYPE_CHAR then
                                  TYPIND := 'C'
                                else
                                  TYPIND := 'I' ;

              //******************************************
              // pointer: only equal - unequal allowed    
              //******************************************

                          POINTER :
                            begin
                              if LOP in [ SYLTOP , SYLEOP , SYGTOP ,
                              SYGEOP ] then
                                ERROR ( 131 ) ;
                              TYPIND := 'A'
                            end (* tag/ca *) ;

              //******************************************
              // sets: lt and gt not allowed              
              //******************************************

                          POWER : begin
                                    if LOP in [ SYLTOP , SYGTOP ] then
                                      ERROR ( 132 ) ;
                                    TYPIND := 'S' ;
                                  end (* tag/ca *) ;

              //******************************************
              // cstring: conversion needed,              
              // if other expr is not cstring             
              //******************************************

                          CSTRING :
                            begin
                              if GATTR . TYPTR -> . FORM <> CSTRING
                              then
                                begin
                                  CT_RESULT := COMPTYPES (
                                               PTYPE_VARCHAR , GATTR .
                                               TYPTR ) ;
                                  case CT_RESULT of
                                    4 : begin
                                          CTLS . VPO1_NEEDED := TRUE ;
                                          GEN0 ( PCODE_VC1 ) ;
                                        end (* tag/ca *) ;
                                    5 : begin
                                          CTLS . VPO1_NEEDED := TRUE ;
                                          GEN1 ( PCODE_VC2 , GATTR .
                                                 TYPTR -> . SIZE ) ;
                                        end (* tag/ca *) ;
                                    otherwise
                                      
                                  end (* case *) ;
                                end (* then *)
                              else
                                if GATTR . KIND <> EXPR then
                                  begin
                                    LOADADDRESS ;
                                    GEN2 ( PCODE_VLD , 0 , GATTR .
                                           TYPTR -> . SIZE - 4 )
                                  end (* then *) ;
                              TYPIND := 'V'
                            end (* tag/ca *) ;

              //******************************************
              // some restrictions for non-char           
              // arrays. and: conversion needed,          
              // if other expression is cstring           
              //******************************************

                          ARRAYS :
                            begin
                              if not IS_CARRAY ( LATTR . TYPTR ) then
                                begin
                                  if LOP in [ SYLTOP , SYLEOP , SYGTOP
                                  , SYGEOP ] then
                                    ERROR ( 131 )
                                end (* then *)
                              else
                                begin
                                  if GATTR . TYPTR -> . FORM = CSTRING
                                  then
                                    begin
                                      ERROR ( 344 ) ;
                                      TYPIND := 'M'
                                    end (* then *)
                                  else
                                    TYPIND := 'M'
                                end (* else *)
                            end (* tag/ca *) ;

              //******************************************
              // records: only equal - unequal allowed    
              //******************************************

                          RECORDS :
                            begin
                              if LOP in [ SYLTOP , SYLEOP , SYGTOP ,
                              SYGEOP ] then
                                ERROR ( 131 ) ;
                              TYPIND := 'M'
                            end (* tag/ca *) ;

              //******************************************
              // files: nothing allowed                   
              //******************************************

                          FILES : begin
                                    ERROR ( 133 ) ;
                                    TYPIND := 'F'
                                  end (* tag/ca *)
                        end (* case *) ;
                        GEN2 ( COMPARE_OP [ LOP ] , ORD ( TYPIND ) ,
                               LSIZE ) ;
                      end (* then *)
                    else
                      begin
                        if FALSE then
                          begin
                            WRITELN ( TRACEF , 'expression loc = ' ,
                                      LINECNT : 1 ) ;
                            WRITELN ( TRACEF , 'CT_result = ' ,
                                      CT_RESULT ) ;
                            WRITELN ( TRACEF , 'lattr.typtr.form = ' ,
                                      LATTR . TYPTR -> . FORM ) ;
                            WRITELN ( TRACEF , 'gattr.typtr.form = ' ,
                                      GATTR . TYPTR -> . FORM ) ;
                            WRITELN ( TRACEF , 'lattr.typtr.size = ' ,
                                      LATTR . TYPTR -> . SIZE ) ;
                            WRITELN ( TRACEF , 'gattr.typtr.size = ' ,
                                      GATTR . TYPTR -> . SIZE ) ;
                          end (* then *) ;
                        ERROR ( 129 )
                      end (* else *)
                  until TRUE ;

              //******************************************
              // result is always boolean                 
              //******************************************

                  GATTR . TYPTR := PTYPE_BOOL ;
                  GATTR . BTYPE := PTYPE_BOOL ;
                  GATTR . KIND := EXPR ;
                  LCOUNTER := LLC ;
                end (* then *)
            end (* EXPRESSION *) ;


         procedure ASSIGNMENT ( FCP : IDP ) ;

         (*****************************************************)
         (* new in 12.2017 release:                           *)
         (* shorter strings may be assigned to longer ones,   *)
         (* the longer target string is filled with blanks    *)
         (*****************************************************)


            var LATTR : ATTR ;
                RSIZE , LLC : ADDRRANGE ;
                CT_RESULT : INTEGER ;
                XSAVE : TYPECLASS ;
                ADJUST_STRINGSIZE : BOOLEAN ;
                STRINGSIZE_LEFT : 1 .. MAXSTRL ;
                STRINGSIZE_RIGHT : 1 .. MAXSTRL ;
                GATTR_SAVE : ATTR ;
                TYPE_ERROR : INTEGER ;
                IS_FUNCRES : BOOLEAN ;

            begin (* ASSIGNMENT *)
              if CTLS . WATCH1 then
                ERROR ( 403 ) ;
              CTLS . WATCH1 := TRUE ;
              LLC := LCOUNTER ;
              SELECTOR ( FSYS + [ SYASSIGN ] , FCP , TRUE , IS_FUNCRES
                         ) ;
              VAR_MOD := VAR_MOD + 1 ;

              //*************************************************
              // if IS_FUNCRES and CTLS . WATCH2 then ...        
              // that means:                                     
              // if this statement is an assignment to the       
              // functions result AND if the function result     
              // is of type conformant string                    
              // THEN the string in the workarea should          
              // remain there and should not be freed at the     
              // end of the statement                            
              //*************************************************

              if IS_FUNCRES and CTLS . WATCH2 then
                begin
                  CTLS . WATCH1 := FALSE ;
                  CTLS . VPO1_NEEDED := FALSE ;
                  RESOLVE_CTLS ( 2 ) ;
                end (* then *) ;
              if SY = SYASSIGN then
                begin
                  TYPE_ERROR := 129 ;
                  if GATTR . TYPTR <> NIL then
                    if ( GATTR . ACCESS <> DRCT ) or ( GATTR . TYPTR ->
                    . FORM >= POWER ) then
                      LOADADDRESS ;
                  LATTR := GATTR ;
                  INSYMBOL ;
                  EXPRESSION ( FSYS ) ;

              (********************************)
              (* new: check comptypes first,  *)
              (* maybe short string constant  *)
              (* comptypes = 2                *)
              (********************************)

                  CT_RESULT := 0 ;
                  ADJUST_STRINGSIZE := FALSE ;
                  if ( LATTR . TYPTR <> NIL ) and ( GATTR . TYPTR <>
                  NIL ) then
                    begin

              (******************************************)
              (* lattr = left side of assignment        *)
              (* gattr = right side of assignment       *)
              (******************************************)

                      CT_RESULT := COMPTYPES ( LATTR . TYPTR , GATTR .
                                   TYPTR ) ;
                      if FALSE then
                        begin
                          WRITELN ( TRACEF , 'loc = ' , LINECNT : 1 ,
                                    ' CT_result (1) = ' , CT_RESULT ) ;
                        end (* then *) ;

              (*************************************************)
              (* maybe source string has shorter size          *)
              (* if so, adjust size                            *)
              (*************************************************)

                      case CT_RESULT of
                        1 : ;
                        2 , 3 : begin
                                  if IS_CARRAY ( GATTR . TYPTR ) or (
                                  GATTR . TYPTR = PTYPE_CHAR ) then
                                    if GATTR . KIND = CST then
                                      begin
                                        MOD_STRCONST ( CT_RESULT ,
                                                   GATTR . CVAL , GATTR
                                                   . TYPTR , LATTR .
                                                   TYPTR -> . SIZE ) ;
                                      end (* then *)
                                    else
                                      begin
                                        ADJUST_STRINGSIZE := TRUE ;
                                        STRINGSIZE_LEFT := LATTR .
                                                   TYPTR -> . SIZE ;
                                        STRINGSIZE_RIGHT := GATTR .
                                                   TYPTR -> . SIZE ;
                                      end (* else *)
                                  else
                                    begin
                                      CT_RESULT := 0
                                    end (* else *)
                                end (* tag/ca *) ;
                        4 , 5 , 7 :
                          begin
                            if GATTR . KIND <> CST then
                              begin
                                if FALSE then
                                  begin
                                    WRITELN ( TRACEF ,
                                              'source is not constant'
                                              ) ;
                                  end (* then *) ;
                                TYPE_ERROR := 341 ;
                                CT_RESULT := 0
                              end (* then *)
                          end (* tag/ca *) ;
                        6 : ;
                        otherwise
                          CT_RESULT := 0
                      end (* case *) ;
                    end (* then *) ;

              (*************************************************)
              (* load source of assignment                     *)
              (*************************************************)

                  if GATTR . TYPTR <> NIL then
                    if GATTR . TYPTR -> . FORM < POWER then
                      LOAD
                    else
                      if GATTR . TYPTR -> . FORM = POWER then
                        FORCETEMPSET
                      else
                        if GATTR . TYPTR -> . FORM = CSTRING then
                          begin
                            if GATTR . KIND <> EXPR then
                              LOADADDRESS
                          end (* then *)
                        else
                          LOADADDRESS ;

              (********************************)
              (* if both sides are ok         *)
              (********************************)

                  if ( LATTR . TYPTR <> NIL ) and ( GATTR . TYPTR <>
                  NIL ) then
                    begin
                      if IS_STDTYPE ( LATTR . TYPTR , 'R' ) and ( GATTR
                      . TYPTR = PTYPE_INT ) then
                        begin

              (**************************************)
              (* assignment integer to real is ok;  *)
              (* modify right hand type to real     *)
              (* set ct_result to 1 to avoid $E129  *)
              (* generate FLT instruction           *)
              (**************************************)

                          GEN0 ( PCODE_FLT ) ;
                          GATTR . TYPTR := PTYPE_REAL ;
                          CT_RESULT := 1 ;
                        end (* then *) ;

              (*******************************************)
              (* store result depending on object class  *)
              (*******************************************)

                      if not ( CT_RESULT in [ 1 .. 7 ] ) then
                        begin
                          if FALSE then
                            begin
                              WRITELN ( TRACEF , 'CT_result (2) = ' ,
                                        CT_RESULT ) ;
                              WRITELN ( TRACEF , 'lattr.typtr.form = '
                                        , LATTR . TYPTR -> . FORM ) ;
                              WRITELN ( TRACEF , 'gattr.typtr.form = '
                                        , GATTR . TYPTR -> . FORM ) ;
                              WRITELN ( TRACEF , 'lattr.typtr.size = '
                                        , LATTR . TYPTR -> . SIZE ) ;
                              WRITELN ( TRACEF , 'gattr.typtr.size = '
                                        , GATTR . TYPTR -> . SIZE ) ;
                            end (* then *) ;
                          ERROR ( TYPE_ERROR )
                        end (* then *)
                      else
                        begin
                          if FALSE then
                            begin
                              WRITELN ( TRACEF , 'CT_result (2) = ' ,
                                        CT_RESULT ) ;
                              WRITELN ( TRACEF , 'lattr.typtr.form = '
                                        , LATTR . TYPTR -> . FORM ) ;
                              WRITELN ( TRACEF , 'gattr.typtr.form = '
                                        , GATTR . TYPTR -> . FORM ) ;
                              WRITELN ( TRACEF , 'lattr.typtr.size = '
                                        , LATTR . TYPTR -> . SIZE ) ;
                              WRITELN ( TRACEF , 'gattr.typtr.size = '
                                        , GATTR . TYPTR -> . SIZE ) ;
                            end (* then *) ;
                          if OPT . DEBUG then
                            begin
                              ASSIGN := TRUE ;
                              CHKBNDS ( LATTR . BTYPE ) ;
                              ASSIGN := FALSE
                            end (* then *) ;

              //******************************************
              // assignment depending on target type class
              //******************************************

                          case LATTR . TYPTR -> . FORM of

              //**********************************************
              // simple types like scalar, subrange, pointer  
              //**********************************************

                            SCALAR , SUBRANGE , POINTER :
                              STORE ( LATTR ) ;

              //******************************************
              // sets                                     
              //******************************************

                            POWER : begin
                                      RSIZE := OPNDSETSIZE ( GATTR ) ;
                                      GEN2 ( PCODE_SMV , LATTR . TYPTR
                                             -> . SIZE , RSIZE )
                                    end (* tag/ca *) ;

              //******************************************
              // arrays                                   
              // some difficulties because of strings of  
              // different lengths and varchars           
              //******************************************

                            ARRAYS :
                              if GATTR . TYPTR -> . FORM = CSTRING then
                                begin

              //******************************************
              // assign cstring to packed array of char   
              // VLD to stack only, if source is variable 
              //******************************************

                                  if GATTR . KIND <> EXPR then
                                    GEN2 ( PCODE_VLD , 0 , GATTR .
                                           TYPTR -> . SIZE - 4 ) ;
                                  GEN1 ( PCODE_VMV , LATTR . TYPTR -> .
                                         SIZE )
                                end (* then *)
                              else
                                if ADJUST_STRINGSIZE then
                                  begin
                                    if FALSE then
                                      begin
                                        WRITELN ( TRACEF ,
                                                 'loc linecnt      = '
                                                  , LINECNT ) ;
                                        WRITELN ( TRACEF ,
                                                 'adjust_stringsz  = '
                                                  , ADJUST_STRINGSIZE )
                                                  ;
                                        WRITELN ( TRACEF ,
                                                 'stringsize_left  = '
                                                  , STRINGSIZE_LEFT ) ;
                                        WRITELN ( TRACEF ,
                                                 'stringsize_right = '
                                                  , STRINGSIZE_RIGHT )
                                                  ;
                                      end (* then *) ;

              (******************************************)
              (* set fill pattern to blank              *)
              (* and generate MFI (negative length)     *)
              (* MFI uses target address at SP - 3      *)
              (* MFI only pops the fill pattern and     *)
              (* leaves the two addresses at SP - 1     *)
              (* and SP - 2 for the following MOV       *)
              (******************************************)

                                    GEN2 ( PCODE_LDC , 0 , ORD ( ' ' )
                                           ) ;
                                    GEN1 ( PCODE_MFI , -
                                           STRINGSIZE_LEFT ) ;

              (******************************************)
              (*  copy string, but only using           *)
              (*  length of right side.                 *)
              (******************************************)

                                    if STRINGSIZE_RIGHT = 1 then
                                      GEN1 ( PCODE_STO , ORD ( 'C' ) )
                                    else
                                      GEN1 ( PCODE_MOV ,
                                             STRINGSIZE_RIGHT ) ;
                                  end (* then *)
                                else
                                  GEN1 ( PCODE_MOV , LATTR . TYPTR -> .
                                         SIZE ) ;

              //******************************************
              // records are simple                       
              //******************************************

                            RECORDS :
                              GEN1 ( PCODE_MOV , LATTR . TYPTR -> .
                                     SIZE ) ;

              //******************************************
              // cstrings need conversion sometimes       
              //******************************************

                            CSTRING :
                              begin
                                case CT_RESULT of
                                  4 : begin
                                        CTLS . VPO1_NEEDED := TRUE ;
                                        GEN0 ( PCODE_VC1 ) ;
                                      end (* tag/ca *) ;
                                  5 , 7 : begin
                                            CTLS . VPO1_NEEDED := TRUE
                                                   ;
                                            GEN1 ( PCODE_VC2 , GATTR .
                                                   TYPTR -> . SIZE ) ;
                                          end (* tag/ca *) ;
                                  otherwise
                                    if GATTR . KIND <> EXPR then
                                      begin
                                        LOADADDRESS ;
                                        GEN2 ( PCODE_VLD , 0 , GATTR .
                                               TYPTR -> . SIZE - 4 )
                                      end (* then *)
                                end (* case *) ;
                                if IS_FUNCRES then
                                  GEN2 ( PCODE_VST , 0 , - 1 )
                                else
                                  GEN2 ( PCODE_VST , 0 , LATTR . TYPTR
                                         -> . SIZE - 4 )
                              end (* tag/ca *) ;

              //******************************************
              // no direct assignment for files           
              //******************************************

                            FILES : ERROR ( 146 )
                          end (* case *)
                        end (* else *)
                    end (* then *)
                end (* then *)
              else
                ERROR ( 51 ) ;
              LCOUNTER := LLC ;

              //******************************************
              // don't free space in string workarea for  
              // string results of functions              
              // instead of this: set VPU1 address to new 
              // value, so that this computed string will 
              // not be overwritten ...                   
              //******************************************

              if IS_FUNCRES and CTLS . WATCH2 then
                begin
                  CHECK_CTLS ( 1 ) ;
                  CTLS . WATCH1 := FALSE ;
                  CTLS . VPO1_NEEDED := FALSE ;
                end (* then *)
              else
                RESOLVE_CTLS ( 1 ) ;
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
                              GENUJPFJP ( PCODE_UJP , LABNAME )
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
                                if OPT . PRCODE then
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

            var ELSE_IN_FSYS : BOOLEAN ;

            begin (* COMPOUNDSTATEMENT *)
              ELSE_IN_FSYS := FALSE ;
              if SYELSE in FSYS then
                begin
                  FSYS := FSYS - [ SYELSE ] ;
                  ELSE_IN_FSYS := TRUE ;
                end (* then *) ;
              repeat
                repeat
                  STATEMENT ( FSYS + [ SYSEMICOLON , SYEND ] , LOOPC ,
                              SUBR ) ;
                until not ( SY in STATBEGSYS ) ;
                TEST := SY <> SYSEMICOLON ;
                if not TEST then
                  INSYMBOL
              until TEST ;
              if SY = SYEND then
                INSYMBOL
              else
                ERROR ( 13 ) ;
              if ELSE_IN_FSYS then
                FSYS := FSYS + [ SYELSE ] ;
            end (* COMPOUNDSTATEMENT *) ;


         procedure IFSTATEMENT ;

            var LCIX1 , LCIX2 : LABELRNG ;
                FIRSTLN , MIDLN : INTEGER ;
                CTRNO : CTRRANGE ;

            begin (* IFSTATEMENT *)
              if CTLS . WATCH1 then
                ERROR ( 404 ) ;
              CTLS . WATCH1 := TRUE ;
              EXPRESSION ( FSYS + [ SYTHEN ] ) ;
              RESOLVE_CTLS ( 1 ) ;
              GENLABEL ( LCIX1 ) ;
              GENFJP ( LCIX1 ) ;
              if SY = SYTHEN then
                INSYMBOL
              else
                ERROR ( 52 ) ;
              FIRSTLN := LINECNT ;
              CTRNO := CTRGEN ;

              (********************)
              (*** COUNTER HERE ***)
              (********************)

              STATEMENT ( FSYS + [ SYELSE ] , LOOPC , SUBR ) ;
              if SY = SYELSE then
                begin
                  GENLABEL ( LCIX2 ) ;
                  GENUJPFJP ( PCODE_UJP , LCIX2 ) ;
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

            type CIP = -> CASEINFO ;
                 CASEINFO = record
                              NEXT : CIP ;
                              CSSTART : LABELRNG ;
                              HEXORBIN : CHAR ;
                              CSLAB1 , CSLAB2 : XCONSTANT ;
                            end ;

            var LSP , LSP1 : TTP ;
                FSTPTR , LPT1 , LPT2 , LPT3 : CIP ;
                LVAL1 , LVAL2 : XCONSTANT ;
                LADDR , LCIX , LCIX1 , UBND , LBND , XADDR : LABELRNG ;
                LMIN , LMAX : INTEGER ;
                OTHWC : BOOLEAN ;
                FIRSTLN : INTEGER ;
                TEMPLN : INTEGER ;
                CTRCASES : INTEGER ;
                CTRNO : CTRRANGE ;
                STRT : CHAR ;

            begin (* CASESTATEMENT *)
              if CTLS . WATCH1 then
                ERROR ( 405 ) ;
              CTLS . WATCH1 := TRUE ;
              EXPRESSION ( FSYS + [ SYOF , SYCOMMA , SYCOLON ] ) ;
              RESOLVE_CTLS ( 1 ) ;
              LOAD ;
              LSP := GATTR . TYPTR ;
              if LSP <> NIL then
                if ( LSP -> . FORM <> SCALAR ) or IS_STDTYPE ( LSP ,
                'R' ) then
                  begin
                    ERROR ( 144 ) ;
                    LSP := NIL
                  end (* then *)
                else
                  if COMPTYPES ( LSP , PTYPE_INT ) <> 1 then
                    GEN0 ( PCODE_ORD ) ;
              if OPT . DEBUG then
                CHKBNDS ( GATTR . TYPTR ) ;
              if SY = SYOF then
                INSYMBOL
              else
                ERROR ( 8 ) ;
              FSTPTR := NIL ;

              (******************************************)
              (* four labels are generated in both      *)
              (* cases (new = portable and old):        *)
              (* lbnd = lower bound                     *)
              (* ubnd = upper bound                     *)
              (* lcix = begin of branch table           *)
              (* laddr = default addr                   *)
              (******************************************)

              GENLABEL ( LBND ) ;
              GENLABEL ( UBND ) ;
              GENLABEL ( LCIX ) ;
              GENLABEL ( LADDR ) ;
              GENLABEL ( XADDR ) ;

              (*****************************************)
              (*XJP - if new, to LCIX, if old, to LBND *)
              (*****************************************)

              if not PORTABLE_BRANCHTABLE then
                GENUJPFJP ( 44 , LBND )
              else
                GENUJPFJP ( 44 , LCIX ) ;
              OTHWC := FALSE ;
              FIRSTLN := LINECNT ;
              CTRCASES := 0 ;
              repeat
                LPT3 := NIL ;
                GENLABEL ( LCIX1 ) ;
                if not ( SY in [ SYSEMICOLON , SYEND ] ) then
                  begin
                    if SY <> SYOTHERWISE then
                      begin
                        repeat

              /*****************************/
              /* first case label constant */
              /*****************************/

                          CONSTANT ( FSYS + [ SYCOMMA , SYCOLON ,
                                     SYDOTDOT ] , LSP1 , LVAL1 ) ;
                          if LSP <> NIL then
                            if COMPTYPES ( LSP , LSP1 ) = 1 then
                              begin
                                LVAL2 := LVAL1 ;
                                if SY = SYDOTDOT then
                                  begin

              /********************************/
              /* second constant after dotdot */
              /********************************/

                                    INSYMBOL ;
                                    CONSTANT ( FSYS + [ SYCOMMA ,
                                               SYCOLON ] , LSP1 , LVAL2
                                               )
                                  end (* then *) ;
                                if COMPTYPES ( LSP , LSP1 ) = 1 then
                                  if LVAL2 . IVAL >= LVAL1 . IVAL then
                                    begin

              /**********************************/
              /* check if new constants overlap */
              /* with existing constants        */
              /* and look for the right place   */
              /* to insert new element          */
              /**********************************/
              /* if lsp1 <> nil: don't show     */
              /* error 156, if constant         */
              /* identified was undeclared      */
              /* (for example)                  */
              /**********************************/

                                      LPT1 := FSTPTR ;
                                      LPT2 := NIL ;
                                      while LPT1 <> NIL do
                                        with LPT1 -> do
                                          begin
                                            if LVAL2 . IVAL >= CSLAB2 .
                                            IVAL then
                                              begin
                                                if LSP1 <> NIL then
                                                  if LVAL1 . IVAL <=
                                                  CSLAB2 . IVAL then
                                                   ERROR ( 156 ) ;
                                                break ;
                                              end (* then *) ;
                                            LPT2 := LPT1 ;
                                            LPT1 := NEXT
                                          end (* with *) ;
                                      NEW ( LPT3 ) ;
                                      with LPT3 -> do
                                        begin
                                          NEXT := LPT1 ;
                                          CSLAB1 := LVAL1 ;
                                          CSLAB2 := LVAL2 ;
                                          HEXORBIN := 'N' ;
                                          if CSLAB1 . STRTYPE in [ 'B'
                                          , 'X' ] then
                                            HEXORBIN := 'T' ;
                                          if CSLAB2 . STRTYPE in [ 'B'
                                          , 'X' ] then
                                            if HEXORBIN = 'T' then
                                              HEXORBIN := 'J'
                                            else
                                              HEXORBIN := 'T' ;
                                          if FALSE then
                                            WRITELN ( TRACEF , 'case: '
                                                   , CSLAB1 . IVAL : 4
                                                   , CSLAB2 . IVAL : 4
                                                   , CSLAB1 . STRTYPE :
                                                   2 , CSLAB2 . STRTYPE
                                                   : 2 , HEXORBIN : 2 )
                                                   ;
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
                          TEST := SY <> SYCOMMA ;
                          if not TEST then
                            INSYMBOL
                        until TEST ;
                        if SY = SYCOLON then
                          INSYMBOL
                        else
                          ERROR ( 5 )
                      end (* then *)
                    else

              (********************)
              (* SY = SYOTHERWISE *)
              (********************)

                      begin
                        if OTHWC then
                          ERROR ( 72 )
                        else
                          LCIX1 := LADDR ;
                        OTHWC := TRUE ;
                        INSYMBOL ;
                        if SY = SYCOLON then
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
                      STATEMENT ( FSYS + [ SYSEMICOLON ] , LOOPC , SUBR
                                  ) ;
                    until not ( SY in STATBEGSYS ) ;
                    GENUJPFJP ( PCODE_UJP , XADDR ) ;
                    CTREMIT ( CTRCASE , CTRNO , TEMPLN , 0 , LINECNT )
                              ;
                  end (* then *) ;
                TEST := SY <> SYSEMICOLON ;
                if not TEST then
                  INSYMBOL ;
              until TEST ;
              if FSTPTR <> NIL then
                begin
                  LMAX := FSTPTR -> . CSLAB2 . IVAL ;

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
                  LMIN := FSTPTR -> . CSLAB1 . IVAL ;
                end (* then *)
              else
                begin
                  LMIN := 1 ;
                  LMAX := 0 ;
                end (* else *) ;

              /*************************************/
              /* portable branch table: no need    */
              /* for LBND / UBND constants ...     */
              /* LBND and UBND are derived from    */
              /* constants in the branch table,    */
              /* but in pass 2                     */
              /*************************************/

              if not PORTABLE_BRANCHTABLE then
                begin
                  if LSP = PTYPE_CHAR then
                    begin
                      GENDEF ( LBND , 'C' , LMIN ) ;
                      GENDEF ( UBND , 'C' , LMAX ) ;
                    end (* then *)
                  else
                    begin
                      GENDEF ( LBND , 'I' , LMIN ) ;
                      GENDEF ( UBND , 'I' , LMAX ) ;
                    end (* else *)
                end (* then *) ;
              PUTLABEL ( LCIX ) ;
              if LMAX - LMIN < CIXMAX then
                begin
                  if FSTPTR <> NIL then
                    repeat
                      with FSTPTR -> do
                        begin
                          while CSLAB1 . IVAL > LMIN do
                            begin

              /*************************************/
              /* portable branch table: dont build */
              /* ujp for undefined cases (= holes) */
              /* opp - 11.2016                     */
              /*************************************/

                              if not PORTABLE_BRANCHTABLE then
                                GENUJPFJP ( PCODE_UJP , LADDR ) ;
                              LMIN := LMIN + 1
                            end (* while *) ;
                          repeat

              /*************************************/
              /* portable branch table: build      */
              /* defines for the constants         */
              /* together with the ujps            */
              /* opp - 11.2016                     */
              /*************************************/

                            if PORTABLE_BRANCHTABLE then
                              begin
                                if ( LSP = PTYPE_CHAR ) and ( HEXORBIN
                                = 'N' ) then
                                  GENDEF ( 0 , 'C' , LMIN )
                                else
                                  GENDEF ( 0 , 'I' , LMIN )
                              end (* then *) ;

              /*************************************/
              /* like before: build ujps to        */
              /* fill the branch table             */
              /*************************************/

                            GENUJPFJP ( PCODE_UJP , CSSTART ) ;
                            LMIN := LMIN + 1 ;
                          until LMIN > CSLAB2 . IVAL ;
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
              if SY = SYEND then
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
                  GENUJPFJP ( PCODE_UJP , LOOPC . BREAKLABEL ) ;
                  LOOPC . BREAKUSED := TRUE ;
                end (* else *)
            end (* BREAKSTATEMENT *) ;


         procedure CONTSTATEMENT ;

            begin (* CONTSTATEMENT *)
              if LOOPC . LEVEL <= 0 then
                ERROR ( 71 )
              else
                begin
                  GENUJPFJP ( PCODE_UJP , LOOPC . CONTLABEL ) ;
                  LOOPC . CONTUSED := TRUE ;
                end (* else *)
            end (* CONTSTATEMENT *) ;


         procedure RETURNSTATEMENT ;

            begin (* RETURNSTATEMENT *)
              GENUJPFJP ( PCODE_UJP , SUBR . RETURNLABEL ) ;
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
                  STATEMENT ( FSYS + [ SYSEMICOLON , SYUNTIL ] , LOOPR
                              , SUBR ) ;
                until not ( SY in STATBEGSYS ) ;
                TEST := SY <> SYSEMICOLON ;
                if not TEST then
                  INSYMBOL
              until TEST ;
              if SY = SYUNTIL then
                begin
                  if LOOPR . CONTUSED then
                    PUTLABEL ( LCONT ) ;
                  INSYMBOL ;
                  if CTLS . WATCH1 then
                    ERROR ( 406 ) ;
                  CTLS . WATCH1 := TRUE ;
                  EXPRESSION ( FSYS ) ;
                  RESOLVE_CTLS ( 1 ) ;
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
              if CTLS . WATCH1 then
                ERROR ( 407 ) ;
              CTLS . WATCH1 := TRUE ;
              EXPRESSION ( FSYS + [ SYDO ] ) ;
              RESOLVE_CTLS ( 1 ) ;
              GENFJP ( LCIX ) ;
              if SY = SYDO then
                INSYMBOL
              else
                ERROR ( 54 ) ;
              FIRSTLN := LINECNT ;
              CTRNO := CTRGEN ;

              (********************)
              (*** COUNTER HERE ***)
              (********************)

              STATEMENT ( FSYS , LOOPW , SUBR ) ;
              GENUJPFJP ( PCODE_UJP , LADDR ) ;
              PUTLABEL ( LCIX ) ;
              CTREMIT ( CTRWHILE , CTRNO , FIRSTLN , 0 , LINECNT ) ;
            end (* WHILESTATEMENT *) ;


         procedure FORSTATEMENT ;

            var LATTR : ATTR ;
                LSP : TTP ;
                LSY : SYMB ;
                LOP : OPRANGE ;
                XT , CV1 , CV2 : INTEGER ;
                CB1 , CB2 : BOOLEAN ;
                LCIX , LADDR , LINCR : LABELRNG ;
                LLC : ADDRRANGE ;
                FIRSTLN : INTEGER ;
                CTRNO : CTRRANGE ;
                LOOPF : LOOPCTL ;

            begin (* FORSTATEMENT *)
              LLC := LCOUNTER ;

              /************************************/
              /* set her in case of future errors */
              /************************************/

              if SY = IDENT then
                begin
                  SID_RC := SEARCHID ( ID , TRUE , TRUE , [ VARS ] ,
                            LCP ) ;
                  with LCP -> , LATTR do
                    begin
                      TYPTR := IDTYPE ;
                      KIND := VARBL ;
                      BTYPE := TYPTR ;
                      if TYPTR <> NIL then
                        if TYPTR -> . FORM = SUBRANGE then
                          TYPTR := TYPTR -> . RANGETYPE ;
                      if VKIND = NORMALVAR then
                        begin
                          if STKLASS = XAUTO then
                            begin
                              ACCESS := DRCT ;
                              VLEVEL := VLEV ;
                              DPLMT := VADDR ;
                            end (* then *)
                          else
                            begin

              /**********************************/
              /* zunaechst compiler restriction */
              /**********************************/

                              ERROR ( 398 ) ;
                              TYPTR := NIL
                            end (* else *)
                        end (* then *)
                      else
                        begin
                          ERROR ( 155 ) ;
                          TYPTR := NIL
                        end (* else *)
                    end (* with *) ;
                  if LATTR . TYPTR <> NIL then
                    if ( LATTR . TYPTR -> . FORM > SUBRANGE ) or
                    IS_STDTYPE ( LATTR . TYPTR , 'R' ) then
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
                  SKIP ( FSYS + [ SYASSIGN , SYTO , SYDOWNTO , SYDO ] )
                end (* else *) ;
              if SY = SYASSIGN then
                begin
                  INSYMBOL ;
                  if CTLS . WATCH1 then
                    ERROR ( 408 ) ;
                  CTLS . WATCH1 := TRUE ;
                  EXPRESSION ( FSYS + [ SYTO , SYDOWNTO , SYDO ] ) ;
                  RESOLVE_CTLS ( 1 ) ;
                  if GATTR . TYPTR <> NIL then
                    if GATTR . TYPTR -> . FORM <> SCALAR then
                      ERROR ( 144 )
                    else
                      if COMPTYPES ( LATTR . TYPTR , GATTR . TYPTR ) =
                      1 then
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
                  SKIP ( FSYS + [ SYTO , SYDOWNTO , SYDO ] )
                end (* else *) ;
              if ( SY = SYTO ) or ( SY = SYDOWNTO ) then
                begin
                  LSY := SY ;
                  INSYMBOL ;
                  if CTLS . WATCH1 then
                    ERROR ( 409 ) ;
                  CTLS . WATCH1 := TRUE ;
                  EXPRESSION ( FSYS + [ SYDO ] ) ;
                  RESOLVE_CTLS ( 1 ) ;
                  if GATTR . TYPTR <> NIL then
                    if GATTR . TYPTR -> . FORM <> SCALAR then
                      ERROR ( 144 )
                    else
                      if COMPTYPES ( LATTR . TYPTR , GATTR . TYPTR ) =
                      1 then
                        begin
                          if GATTR . KIND = CST then
                            begin
                              CB2 := TRUE ;
                              LLC := LCOUNTER ;
                              CV2 := GATTR . CVAL . IVAL
                            end (* then *)
                          else
                            begin
                              CB2 := FALSE ;
                              LOAD ;
                              ALIGN ( LCOUNTER , INTSIZE ) ;
                              LLC := LCOUNTER ;
                              if GATTR . TYPTR <> PTYPE_INT then
                                GEN0 ( PCODE_ORD ) ;
                              GEN3 ( PCODE_STR , ORD ( 'I' ) , LEVEL ,
                                     LLC ) ;
                              LCOUNTER := LCOUNTER + INTSIZE ;
                              if LCOUNTER > LCMAX then
                                LCMAX := LCOUNTER ;
                            end (* else *) ;
                          if CB1 and CB2 then
                            begin
                              XT := 1 ;
                              if LSY = SYTO then
                                if CV1 > CV2 then
                                  XT := 0
                                else
                                  
                              else
                                if CV1 < CV2 then
                                  XT := 0 ;
                              GEN2 ( PCODE_LDC , 3 , XT ) ;
                            end (* then *)
                          else
                            begin
                              if CB1 then
                                GEN2 ( PCODE_LDC , 1 , CV1 )
                              else
                                begin
                                  GATTR := LATTR ;
                                  LOAD ;
                                  if GATTR . TYPTR <> PTYPE_INT then
                                    GEN0 ( PCODE_ORD ) ;
                                end (* else *) ;
                              if CB2 then
                                begin
                                  GATTR := LATTR ;
                                  if GATTR . TYPTR = PTYPE_CHAR then
                                    begin
                                      GEN2 ( PCODE_LDC , 0 , CV2 ) ;
                                      GEN0 ( PCODE_ORD )
                                    end (* then *)
                                  else
                                    GEN2 ( PCODE_LDC , 1 , CV2 )
                                end (* then *)
                              else
                                GEN3 ( PCODE_LOD , ORD ( 'I' ) , LEVEL
                                       , LLC ) ;
                              if LSY = SYTO then
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
                  SKIP ( FSYS + [ SYDO ] )
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

              if SY = SYDO then
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
              if GATTR . TYPTR <> PTYPE_INT then
                GEN0 ( PCODE_ORD ) ;
              if CB2 then
                begin
                  GATTR := LATTR ;
                  if GATTR . TYPTR = PTYPE_CHAR then
                    begin
                      GEN2 ( PCODE_LDC , 0 , CV2 ) ;
                      GEN0 ( PCODE_ORD )
                    end (* then *)
                  else
                    GEN2 ( PCODE_LDC , 1 , CV2 )
                end (* then *)
              else
                GEN3 ( PCODE_LOD , ORD ( 'I' ) , LEVEL , LLC ) ;

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

              if LSY <> SYTO then
                LOP := 22 ;

              (********)
              (*DEC   *)
              (********)

              GEN2 ( LOP , GETTYPE ( GATTR . TYPTR ) , 1 ) ;
              if OPT . DEBUG then
                CHKBNDS ( LATTR . TYPTR ) ;
              STORE ( LATTR ) ;
              GENUJPFJP ( PCODE_UJP , LADDR ) ;
              PUTLABEL ( LCIX ) ;
              LCOUNTER := LLC ;
              CTREMIT ( CTRFOR , CTRNO , FIRSTLN , 0 , LINECNT ) ;
            end (* FORSTATEMENT *) ;


         procedure WITHSTATEMENT ;

            var LCP : IDP ;
                LCNT : DISPRANGE ;
                LLC : ADDRRANGE ;
                OLD_LEV : - 1 .. DISPLIMIT ;
                REC_STR : TTP ;
                DUMMYB : BOOLEAN ;

            begin (* WITHSTATEMENT *)
              LLC := LCOUNTER ;
              if OPT . GET_STAT then
                WS_CNT := WS_CNT + 1 ;
              if SY = IDENT then
                begin
                  SID_RC := SEARCHID ( ID , TRUE , TRUE , [ VARS ,
                            FIELD ] , LCP ) ;
                  INSYMBOL
                end (* then *)
              else
                begin
                  ERROR ( 2 ) ;
                  LCP := UVARPTR
                end (* else *) ;
              if CTLS . WATCH1 then
                ERROR ( 430 ) ;
              CTLS . WATCH1 := TRUE ;
              SELECTOR ( FSYS + [ SYCOMMA , SYDO ] , LCP , TRUE ,
                         DUMMYB ) ;
              RESOLVE_CTLS ( 1 ) ;
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
                              ALIGN ( LCOUNTER , PTRSIZE ) ;
                              GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL ,
                                     LCOUNTER ) ;
                              OCCUR := VREC ;
                              VDSPL := LCOUNTER ;
                              LCOUNTER := LCOUNTER + PTRSIZE ;
                              if LCOUNTER > LCMAX then
                                LCMAX := LCOUNTER
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
              if SY = SYCOMMA then
                begin
                  INSYMBOL ;
                  WITHSTATEMENT
                end (* then *)
              else
                begin
                  if SY = SYDO then
                    INSYMBOL
                  else
                    ERROR ( 54 ) ;
                  STATEMENT ( FSYS , LOOPC , SUBR ) ;
                end (* else *) ;
              if REC_STR <> NIL then
                REC_STR -> . FLD_DISP_LEV := OLD_LEV ;
              if TOP >= 0 then
                TOP := TOP - 1 ;
              LCOUNTER := LLC ;
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
                           if OPT . PRCODE then
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
               if SY = SYCOLON then
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
                           SID_RC := SEARCHID ( ID , FALSE , FALSE , [
                                     VARS , FIELD , FUNC , PROC ] , LCP
                                     ) ;
                           STARTID := ID ;
                           ERRLINE_SAVE := SCB . LINENR ;
                           ERRPOS_SAVE := SCB . LINEPOS ;
                           INSYMBOL ;

           /*************************************************/
           /* add id of appropriate class                   */
           /* to prevent more errors in case of notfound    */
           /* special error code, if undecl. procedure      */
           /*************************************************/

                           if SID_RC = 104 then
                             if SY in PROCCALLENDSYS then
                               begin
                                 ERROR_POS ( 'W' , 184 , ' ' ,
                                             ERRLINE_SAVE , ERRPOS_SAVE
                                             ) ;
                                 SID_RC := SEARCHID ( STARTID , FALSE ,
                                           TRUE , [ PROC ] , LCP ) ;
                                 LCP -> . DECLMISSING := TRUE ;
                                 LCP -> . EXTRN := TRUE ;
                                 PACK ( STARTID , 1 , LCP -> . EXTNAME
                                        ) ;
                               end (* then *)
                             else
                               begin
                                 ERROR_POS ( 'E' , 104 , ' ' ,
                                             ERRLINE_SAVE , ERRPOS_SAVE
                                             ) ;
                                 SID_RC := SEARCHID ( STARTID , FALSE ,
                                           TRUE , [ VARS ] , LCP )
                               end (* else *) ;

           /*************************************************/
           /* end of special error handling                 */
           /* after first id of statement                   */
           /*************************************************/

                           if LCP -> . KLASS = PROC then
                             begin
                               if CTLS . WATCH1 then
                                 ERROR ( 403 ) ;
                               CTLS . WATCH1 := TRUE ;
                               CALL ( FSYS , LCP ) ;
                               RESOLVE_CTLS ( 1 ) ;
                             end (* then *)
                           else
                             begin
                               ASSIGNMENT ( LCP )
                             end (* else *)
                         end (* tag/ca *) ;
                 SYBEGIN :
                   begin
                     STMTNEST := STMTNEST + 1 ;
                     INSYMBOL ;
                     COMPOUNDSTATEMENT ;
                     STMTNEST := STMTNEST - 1 ;
                   end (* tag/ca *) ;
                 SYGOTO :
                   begin
                     INSYMBOL ;
                     GOTOSTATEMENT
                   end (* tag/ca *) ;
                 SYIF : begin
                          INSYMBOL ;
                          IFSTATEMENT
                        end (* tag/ca *) ;
                 SYCASE :
                   begin
                     STMTNEST := STMTNEST + 1 ;
                     INSYMBOL ;
                     CASESTATEMENT ;
                     STMTNEST := STMTNEST - 1
                   end (* tag/ca *) ;
                 SYWHILE :
                   begin
                     INSYMBOL ;
                     WHILESTATEMENT
                   end (* tag/ca *) ;
                 SYREPEAT :
                   begin
                     STMTNEST := STMTNEST + 1 ;
                     INSYMBOL ;
                     REPEATSTATEMENT ;
                     STMTNEST := STMTNEST - 1
                   end (* tag/ca *) ;
                 SYFOR : begin
                           INSYMBOL ;
                           FORSTATEMENT
                         end (* tag/ca *) ;
                 SYBREAK :
                   begin
                     INSYMBOL ;
                     BREAKSTATEMENT
                   end (* tag/ca *) ;
                 SYCONTINUE :
                   begin
                     INSYMBOL ;
                     CONTSTATEMENT
                   end (* tag/ca *) ;
                 SYRETURN :
                   begin
                     INSYMBOL ;
                     RETURNSTATEMENT
                   end (* tag/ca *) ;
                 SYWITH :
                   begin
                     INSYMBOL ;
                     WITHSTATEMENT
                   end (* tag/ca *)
               end (* case *) ;
               if not ( SY in [ SYSEMICOLON , SYEND , SYELSE , SYUNTIL
               ] ) then
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


         var LOCPAR : INTEGER ;
             LLC , LCM : ADDRRANGE ;
             LCP1 : IDP ;
             FNAME : ALPHA ;
             PT : CHAR ;

         begin (* FRTPARMS *)
           LEVEL := LEVEL + 1 ;
           OLDICOUNTER := ICOUNTER ;
           while FRTPARHD <> NIL do
             with FRTPARHD -> do
               begin
                 ICOUNTER := 0 ;
                 PT := PROCTYPE ( FRTPARHD ) ;
                 FNAME := NAME ;
                 MKPROCNAME ( FNAME , NAME_PATTERN , PFNAME , TRUE ) ;
                 WRITE ( PRR , FNAME : 8 , MN [ PCODE_ENT ] ) ;
                 WRITE ( PRR , PT : 2 ) ;
                 WRITE ( PRR , ',' , LEVEL : 1 ) ;
                 WRITE ( PRR , ',L' , SEGSIZE : 1 ) ;
                 WRITE ( PRR , NAME : IDLNGTH + 2 ) ;
                 WRITE ( PRR , ',' , OPT . SAVEREGS : 1 ) ;
                 WRITE ( PRR , ',' , OPT . ASSEMBLE : 1 ) ;
                 WRITE ( PRR , ',' , OPT . GET_STAT : 1 ) ;
                 WRITE ( PRR , ',' , OPT . ASMVERB : 1 ) ;
                 WRITE ( PRR , ',' , OPT . DEBUG_LEV : 1 ) ;
                 WRITE ( PRR , ',' , PFNAME : 1 , ',,' ) ;
                 if OPT . DEBUG_LEV > 0 then
                   begin
                     WRITE ( PRR , SOURCENAME ) ;
                   end (* then *) ;
                 WRITELN ( PRR ) ;
                 WRITELN ( PRR , ' LOC ' , LINECNT : 1 ) ;
                 WRITELN ( DBGINFO , '#BGN    ' , NAME , ' ' , LEVEL :
                           4 ) ;
                 LCP1 := PRMPTR ;
                 LCOUNTER := LCAFTMST + FPSAVEAREA ;
                 while LCP1 <> NIL do
                   with LCP1 -> do
                     begin
                       if KLASS = VARS then
                         if IDTYPE <> NIL then
                           begin
                             if VKIND = VARPARM then
                               LCM := VADDR + PTRSIZE
                             else
                               LCM := VADDR + IDTYPE -> . SIZE ;
                             if LCM > LCOUNTER then
                               LCOUNTER := LCM ;
                           end (* then *) ;
                       LCP1 := NEXT
                     end (* with *) ;
                 ALIGN ( LCOUNTER , PTRSIZE ) ;
                 LLC := LCOUNTER ;
                 LCP1 := PRMPTR ;
                 while LCP1 <> NIL do
                   with LCP1 -> do
                     begin
                       if KLASS = VARS then
                         if IDTYPE <> NIL then
                           begin
                             if VKIND = VARPARM then
                               GEN3 ( PCODE_LOD , ORD ( 'A' ) , LEVEL ,
                                      VADDR )
                             else
                               GEN2 ( PCODE_LDA , LEVEL , VADDR ) ;
                             GEN3 ( PCODE_STR , ORD ( 'A' ) , LEVEL ,
                                    LLC ) ;
                             LLC := LLC + PTRSIZE ;
                           end (* then *) ;
                       LCP1 := NEXT ;
                     end (* with *) ;
                 LOCPAR := ( LLC - LCOUNTER ) DIV 2 + 1 ;
                 PUTIC ;
                 WRITE ( PRR , MN [ PCODE_CUP ] ) ;
                 WRITE ( PRR , PROCTYPE ( FRTPARHD ) : 2 ) ;
                 WRITE ( PRR , 'F' ) ;
                 WRITE ( PRR , ',' , LOCPAR : 1 ) ;
                 WRITE ( PRR , ',' , EXTNAME ) ;
                 WRITELN ( PRR , ',' , LCOUNTER : 1 ) ;
                 if KLASS = FUNC then
                   GEN3 ( PCODE_STR , ORD ( PT ) , LEVEL , FNCRSLT ) ;
                 GEN1 ( PCODE_RET , ORD ( PT ) ) ;
                 GENDEF ( SEGSIZE , 'I' , LLC ) ;
                 WRITELN ( DBGINFO , '#PROC   ' , NAME : IDLNGTH , ' '
                           , PFNAME : 4 , ' ' , FALSE : 1 , ICOUNTER :
                           6 , LLC : 8 , ' ' , FALSE : 1 ) ;
                 WRITELN ( DBGINFO , '#VARS   ' , 'REF/MOD RATIO      '
                           , 0 : 6 , 0 : 6 , 0.0 : 10 : 6 ) ;
                 WRITELN ( DBGINFO , '#END' ) ;
                 OLDICOUNTER := OLDICOUNTER + ICOUNTER ;
                 FRTPARHD := NXTFWRD ;
               end (* with *) ;
           LEVEL := LEVEL - 1 ;
           ICOUNTER := OLDICOUNTER ;
         end (* FRTPARMS *) ;


      begin (* BODY *)
        STMTNEST := 1 ;
        LISTTAG := 'N' ;
        PUTIC ;
        if FPROCP = MAINPROG then
          WRITELN ( PRR , MN [ PCODE_BGN ] , ' ' , PROGNAME , ' ' ,
                    TIME : 8 , ' ' , DATE ) ;
        WRITE ( PRR , FPROCP -> . EXTNAME , MN [ PCODE_ENT ] ) ;
        WRITE ( PRR , PROCTYPE ( FPROCP ) : 2 ) ;
        WRITE ( PRR , ',' , LEVEL : 1 ) ;
        WRITE ( PRR , ',L' , SEGSIZE : 1 ) ;
        WRITE ( PRR , FPROCP -> . NAME : IDLNGTH + 2 ) ;
        WRITE ( PRR , ',' , OPT . SAVEREGS : 1 ) ;
        WRITE ( PRR , ',' , OPT . ASSEMBLE : 1 ) ;
        WRITE ( PRR , ',' , OPT . GET_STAT : 1 ) ;
        WRITE ( PRR , ',' , OPT . ASMVERB : 1 ) ;
        WRITE ( PRR , ',' , OPT . DEBUG_LEV : 1 ) ;
        WRITE ( PRR , ',' , FPROCP -> . PFNAME : 1 , ',' ) ;
        if STATIC_VORHANDEN then
          begin
            CSTEXTNAME := FPROCP -> . CSTNAME ;
            WRITE ( PRR , CSTEXTNAME ) ;
          end (* then *) ;
        WRITE ( PRR , ',' ) ;
        if OPT . DEBUG_LEV > 0 then
          begin
            WRITE ( PRR , SOURCENAME ) ;
          end (* then *) ;
        WRITELN ( PRR ) ;
        WRITELN ( DBGINFO , '#BGN    ' , FPROCP -> . NAME , ' ' , LEVEL
                  : 4 ) ;
        WRITELN ( PRR , ' LOC ' , LINECNT : 1 ) ;
        ICOUNTER := ICOUNTER + 1 ;
        STRCOUNTER := 0 ;

        (******************************************************)
        (* allocate room for VPU Stringarea control field     *)
        (******************************************************)

        if FALSE then
          WRITELN ( TRACEF , 'alloc VPU1 field for ' , FPROCP -> . NAME
                    ) ;
        ALIGN ( LCOUNTER , PTRSIZE ) ;
        CTLS . VPU1_OFFSET := LCOUNTER ;
        CTLS . VPU1_LEVEL := LEVEL ;
        CTLS . VPO1_NEEDED := FALSE ;
        LCOUNTER := LCOUNTER + PTRSIZE ;
        if LCOUNTER > LCMAX then
          LCMAX := LCOUNTER ;
        CHECK_CTLS ( 1 ) ;
        if STRING_RESULT then
          begin
            if FALSE then
              WRITELN ( TRACEF , 'alloc VPU2 field for ' , FPROCP -> .
                        NAME ) ;
            CTLS . WATCH2 := TRUE ;
            CTLS . VPU2_OFFSET := LCOUNTER ;
            CTLS . VPU2_LEVEL := LEVEL ;
            LCOUNTER := LCOUNTER + PTRSIZE ;
            LCMAX := LCOUNTER ;
            CHECK_CTLS ( 2 ) ;

        //******************************************************
        // set function result to empty string                  
        //******************************************************

            GEN2 ( PCODE_LDA , LEVEL , FNCRSLT ) ;
            GEN1 ( PCODE_VC2 , 0 ) ;
            GEN2 ( PCODE_VST , 0 , - 1 )
          end (* then *) ;

        (******************************)
        (* LENGTH OF STRING CONSTANTS *)
        (******************************)

        LOCAL_CALL := FALSE ;
        VAR_REF := 0 ;
        VAR_MOD := 0 ;
        if FPROCP = MAINPROG then

        (**********************************************************)
        (* ENTERING MAIN BLOCK                                    *)
        (* this is where the implicit resets and rewrites for     *)
        (* the default files are generated; I decided in 2016     *)
        (* to remove the default reset for input and instead      *)
        (* to do the reset on the first read call                 *)
        (* - bernd oppolzer                                       *)
        (* some days later I removed the implicit rewrites        *)
        (* for the other predefined files, too, because           *)
        (* rewrite is now implicitly done at the first write      *)
        (* action by the pascal runtime.                          *)
        (* there is one problem with removing the reset for       *)
        (* input, because eof(input) is true before the reset     *)
        (* occured, and some programs (including the compiler)    *)
        (* have a problem with this. no problem for output files. *)
        (**********************************************************)

          begin
            while FILEHEAD <> NIL do
              begin
                with FILEHEAD -> do
                  begin

        /***********************************************************/
        /* input ueberlesen mithilfe neuer continue-anweisung :-)  */
        /* das war: if FILIDPTR = INPUTPTR then                    */
        /* jetzt aber: alle standard-files ueberlesen.             */
        /***********************************************************/

                    if FILIDPTR -> . VADDR < FIRSTGVAR then
                      begin
                        FILEHEAD := FILEHEAD -> . NEXTFILE ;
                        continue
                      end (* then *) ;

        (**************************)
        (* 3 = code for reset     *)
        (* 4 = code for rewrite   *)
        (**************************)

                    with FILIDPTR -> do
                      begin
                        GEN2 ( PCODE_LDA , 1 , VADDR ) ;
                        GEN1 ( PCODE_CSP , ORD ( PSIO ) ) ;
                        if VADDR >= FIRSTGVAR then

        (**********************)
        (* USER DEFINED FILES *)
        (**********************)

                          begin
                            NEW ( DDNAME . SVAL ) ;
                            with DDNAME . SVAL -> do
                              begin
                                TAG := 'S' ;
                                LENGTH := 8 ;

        (****************)
        (*OS NAME LENGTH*)
        (****************)

                                for I := 1 to 8 do
                                  SSTR [ I ] := NAME [ I ] ;
                              end (* with *) ;
                            GEN_LCA_M ( DDNAME ) ;
                            LLC1 := 0 ;

        (*******************************)
        (* LENGTH CODE FOR A TEXT FILE *)
        (*******************************)

                            if COMPTYPES ( IDTYPE , PTYPE_TEXT ) <> 1
                            then
                              if IDTYPE <> NIL then
                                if IDTYPE -> . FILTYPE <> NIL then
                                  LLC1 := IDTYPE -> . FILTYPE -> . SIZE
                                          ;
                            GEN2 ( PCODE_LDC , 1 , LLC1 ) ;
                            GEN1 ( PCODE_CSP , ORD ( PFDF ) ) ;
                          end (* then *)
                        else

        (******************************)
        (* I.E. IF VADDR < FIRSTUSERF *)
        (******************************)

                          begin
                            XCSP := PRES ;
                            if FILIDPTR = OUTPUTPTR then
                              XCSP := PREW
                            else
                              if NAME [ 3 ] = 'R' then
                                XCSP := PREW ;

        (*****************)
        (* CSP - RES/REW *)
        (*****************)

                            GEN1 ( PCODE_CSP , ORD ( XCSP ) )
                          end (* else *) ;
                        GEN1 ( PCODE_CSP , ORD ( PEIO ) ) ;
                      end (* with *) ;
                  end (* with *) ;
                FILEHEAD := FILEHEAD -> . NEXTFILE
              end (* while *) ;
            if OPT . CTROPTION then
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

        LCMAX := LCOUNTER ;
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
            STATEMENT ( FSYS + [ SYSEMICOLON , SYEND ] , LOOP0 , SUBR )
                        ;
          until not ( SY in STATBEGSYS ) ;
          TEST := SY <> SYSEMICOLON ;
          if not TEST then
            INSYMBOL
        until TEST ;
        if SY = SYEND then
          INSYMBOL
        else
          ERROR ( 13 ) ;
        if SUBR . RETURNUSED then
          PUTLABEL ( LRETURN ) ;
        STMTNEST := 0 ;
        LISTTAG := ' ' ;
        if TOP >= 0 then
          LLP := DISPLAY [ TOP ] . FLABEL
        else
          LLP := NIL ;

        (*****************************)
        (* TEST FOR UNDEFINED LABELS *)
        (*****************************)

        while LLP <> NIL do
          with LLP -> do
            begin
              if not DEFINED then
                begin
                  PLCNT := PLCNT + 1 ;

        (************************************************)
        (*   new error_pos call with parameter          *)
        (*   labval is the undefined label which        *)
        (*   should become part of the error message    *)
        (************************************************)

                  INTTOSTR ( ADDR ( ERRINFO ) , 32 , LABVAL , FALSE ) ;
                  ERROR_POS ( 'E' , 168 , ERRINFO , SCB . LINENR , SCB
                              . LINEPOS ) ;
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
            if OPT . CTROPTION then
              GENDEF ( CTRCNTLBL , 'I' , CTRCNT ) ;
          end (* then *) ;
        GEN1 ( PCODE_RET , ORD ( PROCTYPE ( FPROCP ) ) ) ;
        ALIGN ( LCMAX , MXDATASZE ) ;
        if OPT . PRCODE then
          GENDEF ( SEGSIZE , 'I' , LCMAX ) ;
        CALL_LVL [ LOCAL_CALL ] := CALL_LVL [ LOCAL_CALL ] + 1 ;
        WRITELN ( DBGINFO , '#PROC   ' , FPROCP -> . NAME : IDLNGTH ,
                  ' ' , FPROCP -> . PFNAME : 4 , ' ' , LOCAL_CALL : 1 ,
                  ' ' , ICOUNTER + ( STRCOUNTER DIV 4 ) : 6 , ' ' ,
                  LCMAX : 8 , ' ' , FLIPDEBUG : 1 ) ;
        WRITE ( DBGINFO , '#VARS   ' , 'REF/MOD RATIO      ' , VAR_MOD
                : 6 , VAR_MOD + VAR_REF : 6 ) ;
        if ( VAR_MOD + VAR_REF ) = 0 then
          WRITELN ( DBGINFO , 0.0 : 10 : 6 )
        else
          WRITELN ( DBGINFO , VAR_MOD / ( VAR_MOD + VAR_REF ) : 10 : 6
                    ) ;
        WRITELN ( DBGINFO , '#END' ) ;
        OLDICOUNTER := OLDICOUNTER + ICOUNTER ;
        ICOUNTER := OLDICOUNTER ;

        (********************************************)
        (* DISPLAY CUMULATIVE ICOUNTER              *)
        (********************************************)

        HP := TRUE ;
        if FRTPARHD <> NIL then
          FRTPARMS ;
        if FPROCP = MAINPROG then
          GEN0 ( PCODE_STP ) ;
        CTLS . WATCH2 := FALSE ;
      end (* BODY *) ;


   begin (* BLOCK *)
     if FALSE then
       DRUCKE_SYMSET ( 'START BLOCK ' , FSYS ) ;
     ICOUNTER := 0 ;
     GENLABEL ( SEGSIZE ) ;
     CONSTLCOUNTER := 0 ;
     STATIC_VORHANDEN := FALSE ;
     FWRDPRCL := NIL ;
     DEC_ORDER := 0 ;
     STRING_RESULT := FALSE ;
     if FPROCP -> . IDTYPE <> NIL then
       begin
         FUNCTYPE := FPROCP -> . IDTYPE ;
         if FUNCTYPE -> . FORM = CSTRING then
           if FUNCTYPE -> . CONFORMANT then
             STRING_RESULT := TRUE ;
       end (* then *) ;
     repeat
       while SY in [ SYLABEL , SYCONST , SYTYPE , SYVAR , SYSTATIC ] do
         begin
           LSY := SY ;
           INSYMBOL ;
           case LSY of
             SYLABEL :
               begin
                 LABELDECLARATION ;
                 if DEC_ORDER >= 1 then
                   EXTUSED := TRUE ;
                 DEC_ORDER := 1 ;
               end (* tag/ca *) ;
             SYCONST :
               begin
                 CONSTDECLARATION ;
                 if DEC_ORDER >= 2 then
                   EXTUSED := TRUE ;
                 DEC_ORDER := 2 ;
               end (* tag/ca *) ;
             SYTYPE :
               begin
                 TYPEDECLARATION ;
                 if DEC_ORDER >= 3 then
                   EXTUSED := TRUE ;
                 DEC_ORDER := 3 ;
               end (* tag/ca *) ;
             SYVAR : begin
                       VARDECLARATION ;
                       if DEC_ORDER >= 4 then
                         EXTUSED := TRUE ;
                       DEC_ORDER := 4 ;
                     end (* tag/ca *) ;
             SYSTATIC :
               begin
                 STATICDECLARATION ;
                 EXTUSED := TRUE ;
                 DEC_ORDER := 5 ;
               end (* tag/ca *) ;
           end (* case *) ;
         end (* while *) ;
       if CONSTLCOUNTER > 0 then

     (***********************************)
     (* A CONSTANTS BLOCK WAS GENERATED *)
     (***********************************)

         begin
           CONSTLCOUNTER := 0 ;
           STATIC_VORHANDEN := TRUE ;
           WRITELN ( PRR , MN [ 75 ] ) ;
         end (* then *) ;
       if OPT . DEBUG_LEV > 0 then
         PRNTSYMBL ( NIL ) ;

     (**************************)
     (* PRINT HEAP TYPE DEFNS. *)
     (**************************)

       while SY in [ SYPROC , SYFUNC , SYLOCAL ] do
         begin
           if SY = SYLOCAL then
             begin
               INSYMBOL ;
               PLOCAL := TRUE ;
               if not ( SY in [ SYPROC , SYFUNC ] ) then
                 ERROR ( 6 ) ;
             end (* then *)
           else
             PLOCAL := FALSE ;
           LSY := SY ;
           INSYMBOL ;
           PROCDECLARATION ( LSY , PLOCAL )
         end (* while *) ;
       if SY <> SYBEGIN then
         begin
           ERROR ( 18 ) ;
           SKIP ( FSYS )
         end (* then *)
     until SY in STATBEGSYS ;
     if SY = SYBEGIN then
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

     (***************************************)
     (* force empty main program for module *)
     (***************************************)

     if ( FPROCP = MAINPROG ) and ( FPROCP -> . EXTNAME = '#PASMAIN' )
     then
       begin
         if SY <> SYEND then
           ERROR ( 195 ) ;
         INSYMBOL ;
         if SY <> SYPERIOD then
           begin
             ERROR ( 196 ) ;
             SY := SYPERIOD
           end (* then *) ;

     (********)
     (*STP   *)
     (********)

         if OPT . PRCODE then
           begin
             PUTIC ;
             WRITELN ( PRR , MN [ 43 ] )
           end (* then *) ;
         ICOUNTER := ICOUNTER + 1
       end (* then *)
     else
       repeat
         BODY ( FSYS + [ SYCASE ] , STRING_RESULT ) ;
         if SY <> FSY then
           begin
             ERROR ( 6 ) ;
             SKIP ( FSYS + [ FSY ] )
           end (* then *)
       until ( SY = FSY ) or ( SY in BLOCKBEGSYS ) ;
   end (* BLOCK *) ;



procedure PROGRAMME ( FSYS : SYMSET ) ;

   var LFPTR : FRECPTR ;
       LCP : IDP ;
       I , J : INTEGER ;

   begin (* PROGRAMME *)
     if FALSE then
       DRUCKE_SYMSET ( 'START PROGRAMME ' , FSYS ) ;
     CALL_LVL [ FALSE ] := 0 ;
     CALL_LVL [ TRUE ] := 0 ;

     (*************************************************)
     (* 09.2016: support modules without main program *)
     (* only external procedures. is_module has to be *)
     (* true in this case, and $ has to be allowed    *)
     (* as start char of identifiers                  *)
     (*************************************************)

     if SY in [ SYPROG , SYMODULE ] then
       begin
         if SY = SYMODULE then
           begin
             IS_MODULE := TRUE ;
           end (* then *) ;
         INSYMBOL ;
         if SY <> IDENT then
           ERROR ( 2 ) ;
         PROGNAME := ID ;

     (*************************************************)
     (* 10.2016: CSECT names for internal procs       *)
     (* are $PRVxxxx; for modules,                    *)
     (* the CSECT names of the internal               *)
     (* procs are derived from the PROGNAME, so       *)
     (* hopefully there will be no name conflicts     *)
     (* - later a compiler option will be provided    *)
     (* to control the naming of the internal procs   *)
     (*************************************************)

         if IS_MODULE then
           begin
             NAME_PATTERN := PROGNAME ;

     (*************************************************)
     (* for modules:                                  *)
     (* first 5 chars are used for CSECT pattern;     *)
     (* but: if module name starts with $PAS, then    *)
     (* the $ and the 4 following chars are used.     *)
     (* for example: $PASLIBX --> $LIBXxxx            *)
     (*************************************************)

             if ( NAME_PATTERN [ 1 ] = '$' ) and ( NAME_PATTERN [ 2 ] =
             'P' ) and ( NAME_PATTERN [ 3 ] = 'A' ) and ( NAME_PATTERN
             [ 4 ] = 'S' ) then
               begin
                 NAME_PATTERN [ 2 ] := NAME_PATTERN [ 5 ] ;
                 NAME_PATTERN [ 3 ] := NAME_PATTERN [ 6 ] ;
                 NAME_PATTERN [ 4 ] := NAME_PATTERN [ 7 ] ;
                 NAME_PATTERN [ 5 ] := NAME_PATTERN [ 8 ] ;
               end (* then *) ;
             for I := 6 to 20 do
               begin
                 NAME_PATTERN [ I ] := ' ' ;
               end (* for *)
           end (* then *)
         else
           NAME_PATTERN := '$PRV' ;
         INSYMBOL ;
         if not ( SY in [ SYLPARENT , SYSEMICOLON ] ) then
           ERROR ( 14 ) ;
         if SY = SYLPARENT then
           begin

     (**************************)
     (* IGNORE BAD PROG. PARMS *)
     (**************************)

             repeat
               INSYMBOL ;
               if SY = IDENT then
                 begin
                   SID_RC := SEARCHID ( ID , FALSE , FALSE , [ VARS ] ,
                             LCP ) ;
                   if LCP <> NIL then
                     if LCP -> . IDTYPE = PTYPE_TEXT then
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
                   if not ( SY in [ SYCOMMA , SYRPARENT ] ) then
                     ERROR ( 20 )
                 end (* then *)
               else
                 ERROR ( 2 )
             until SY <> SYCOMMA ;
             if SY <> SYRPARENT then
               ERROR ( 4 ) ;
             INSYMBOL
           end (* then *) ;
         if SY <> SYSEMICOLON then
           ERROR ( 14 )
         else
           INSYMBOL ;
       end (* then *) ;
     NEW ( MAINPROG , PROC , DECLARED ) ;
     with MAINPROG -> do
       begin
         NAME := '$PASMAIN' ;
         EXTRN := FALSE ;
         EXTLANG := ' ' ;
         EXTNAME := '$PASMAIN' ;
         CSTNAME := '$PASMAI#' ;
         PFNAME := 0 ;
         IDTYPE := NIL ;
         FWDECL := FALSE ;
         PFLEV := 0 ;
         KLASS := PROC ;
         PFDECKIND := DECLARED ;
         NEXT := NIL ;
         NEXT_IN_BKT := NIL ;
         DECL_LEV := 0 ;
         DECLMISSING := FALSE ;
         if IS_MODULE then
           begin
             EXTRN := TRUE ;
             EXTNAME [ 1 ] := '#' ;
             PACK ( PROGNAME , 1 , CSTNAME ) ;
             PROC_TO_STATNAME ( CSTNAME , TRUE , CSTNAME ) ;
           end (* then *) ;
       end (* with *) ;
     if OPT . DEBUG_LEV > 0 then
       WRITELN ( DBGINFO , '% $PASMAIN  0' ) ;
     repeat
       BLOCK ( FSYS , SYPERIOD , MAINPROG ) ;
       if SY <> SYPERIOD then
         ERROR ( 21 )
     until SY = SYPERIOD ;
     WRITELN ( DBGINFO , '#HLT    ' , 'CALL RATIO         ' , CALL_LVL
               [ TRUE ] : 6 , CALL_LVL [ FALSE ] : 6 , CALL_LVL [ TRUE
               ] + CALL_LVL [ FALSE ] : 6 ) ;

     /*********************************************/
     /* to output the last source line etc.       */
     /* this is done instead                      */
     /*********************************************/
     /* read on to symb_eof                       */
     /* to produce correct end of listing         */
     /*********************************************/

     repeat
       PASSCAN ( INPUT , LISTING , SCB , TRUE ) ;
       if not ( SCB . SYMBOLNR in [ SYMB_EOF , SEPARATOR , COMMENT1 ,
       COMMENT2 , COMMENT3 , COMMENT4 , COMMENT5 ] ) then
         ERROR ( 402 ) ;
     until SCB . SYMBOLNR = SYMB_EOF ;

     (*********************************)
     (* PRINT SYMBOL TABLE STATISTICS *)
     (*********************************)

     if OPT . GET_STAT then
       begin
         WRITELN ( DBGINFO , '&SYT1 ' , FENT_CNT : 1 , ' ' , SF_CNT : 1
                   , ' ' , SF_TOT : 1 , ' ' , WE_CNT : 1 , ' ' , RE_CNT
                   : 1 , ' ' , WS_CNT : 1 ) ;
         WRITE ( DBGINFO , '&SYT2' ) ;
         for I := 0 to MAXLEVEL do
           WRITE ( DBGINFO , ' ' , PROC_CNT [ I ] : 1 ) ;
         WRITELN ( DBGINFO ) ;
         WRITE ( DBGINFO , '&SYT3' ) ;
         for I := 0 to MAXLEVEL do
           WRITE ( DBGINFO , ' ' , ENT_CNT [ I ] : 1 ) ;
         for I := 0 to MAXLEVEL do
           begin
             WRITELN ( DBGINFO ) ;
             WRITE ( DBGINFO , '&SYT4' ) ;
             for J := 0 to DISPLIMIT do
               WRITE ( DBGINFO , ' ' , LU_CNT [ I , J ] : 1 ) ;
           end (* for *) ;
         for I := 1 to 10 do
           begin
             WRITELN ( DBGINFO ) ;
             WRITE ( DBGINFO , '&SYT5' ) ;
             for J := 1 to 10 do
               WRITE ( DBGINFO , ' ' , WLU_CNT [ I , J ] : 1 ) ;
           end (* for *) ;
       end (* then *) ;
   end (* PROGRAMME *) ;



procedure ENTERSTDTYPES ;

   const INTTYP : TYPEREC =
         ( INTSIZE , INTSIZE , FALSE , SCALAR , STANDARD , 'I' , 0 , 0
           , 0 , 0 , 0 , 0 , 0 ) ;
         REALTYPE : TYPEREC =
         ( REALSIZE , REALSIZE , FALSE , SCALAR , STANDARD , 'R' , 0 ,
           0 , 0 , 0 , 0 , 0 , 0 ) ;
         CHARTYPE : TYPEREC =
         ( CHARSIZE , CHARSIZE , FALSE , SCALAR , STANDARD , 'C' , 1 ,
           0 , 0 , 1 , 1 , 254 , 1 ) ;
         DECIMALTYPE : TYPEREC =
         ( 0 , CHARSIZE , FALSE , SCALAR , STANDARD , 'D' , 15 , 0 , 1
           , 2 , 1 , 31 , 15 ) ;
         VARCHARTYPE : TYPEREC =
         ( MAXVARCHARSIZE , HINTSIZE , FALSE , CSTRING , FALSE ) ;
         BOOLTYPE : TYPEREC =
         ( BOOLSIZE , BOOLSIZE , FALSE , SCALAR , DECLARED , NIL ) ;
         ANYPTYPE : TYPEREC =
         ( PTRSIZE , PTRSIZE , FALSE , POINTER , NIL ) ;
         ANYFILETYPE : TYPEREC =
         ( 0 , PTRSIZE , FALSE , FILES , NIL ) ;
         TEXTTYPE : TYPEREC =
         ( 0 , PTRSIZE , FALSE , FILES , NIL ) ;
         ALFATYPE : TYPEREC =
         ( ALFALNGTH , CHARSIZE , FALSE , ARRAYS , NIL , NIL ) ;
         ALFAINX : TYPEREC =
         ( INTSIZE , INTSIZE , FALSE , SUBRANGE , NIL , ( ' ' , XINT ,
           1 ) , ( ' ' , XINT , ALFALNGTH ) ) ;
         UTYP : IDENTIFIER =
         ( BLANKID , NIL , NIL , NIL , 0 , TYPES ) ;
         UCST : IDENTIFIER =
         ( BLANKID , NIL , NIL , NIL , 0 , KONST , ( ' ' , XINT , 1 ) )
           ;
         UVAR : IDENTIFIER =
         ( BLANKID , NIL , NIL , NIL , 0 , VARS , NORMALVAR , 0 , XAUTO
           , ' ' , 0 , 0 , FALSE , 0 , 0 ) ;
         UFLD : IDENTIFIER =
         ( BLANKID , NIL , NIL , NIL , 0 , FIELD , 0 , NIL ) ;
         UPF : IDENTIFIER =
         ( BLANKID , NIL , NIL , NIL , 0 , PROC , FALSE , ' ' ,
           '$UNK_PF ' , DECLARED , FALSE , 0 , 0 , NIL , NIL ,
           NORMALVAR , FALSE , '$UNK_PF ' ) ;
         UREC : TYPEREC =
         ( 1 , 1 , FALSE , RECORDS , NIL , NIL , 0 , 0 ) ;

   var SP : TTP ;

   begin (* ENTERSTDTYPES *)

     (**************************************)
     (* integer type                       *)
     (**************************************)

     NEW ( PTYPE_INT ) ;
     PTYPE_INT -> := INTTYP ;

     (**************************************)
     (* real type                          *)
     (**************************************)

     NEW ( PTYPE_REAL ) ;
     PTYPE_REAL -> := REALTYPE ;

     (**************************************)
     (* char type                          *)
     (**************************************)

     NEW ( PTYPE_CHAR ) ;
     PTYPE_CHAR -> := CHARTYPE ;

     (**************************************)
     (* varchar type - needs rework        *)
     (**************************************)

     NEW ( PTYPE_VARCHAR ) ;
     PTYPE_VARCHAR -> := VARCHARTYPE ;

     (**************************************)
     (* decimal type - needs rework        *)
     (**************************************)

     NEW ( PTYPE_DECIMAL ) ;
     PTYPE_DECIMAL -> := DECIMALTYPE ;

     (**************************************)
     (* boolean type                       *)
     (**************************************)

     NEW ( PTYPE_BOOL ) ;
     PTYPE_BOOL -> := BOOLTYPE ;

     (**************************************)
     (* anyptr type = type of nil constant *)
     (**************************************)

     NEW ( PTYPE_ANY ) ;
     PTYPE_ANY -> := ANYPTYPE ;

     (**************************************)
     (* anyfile type =                     *)
     (* type of undefined element type     *)
     (**************************************)

     NEW ( PTYPE_ANYFILE ) ;
     PTYPE_ANYFILE -> := ANYFILETYPE ;
     with PTYPE_ANYFILE -> do
       begin
         FILTYPE := NIL ;
         SIZE := FILMINSIZE ;
       end (* with *) ;

     (**************************************)
     (* text (file) type                   *)
     (**************************************)

     NEW ( PTYPE_TEXT ) ;
     PTYPE_TEXT -> := TEXTTYPE ;
     with PTYPE_TEXT -> do
       begin
         FILTYPE := PTYPE_CHAR ;
         SIZE := CHARSIZE + FILHDRSIZE ;
         if SIZE < FILMINSIZE then
           SIZE := FILMINSIZE ;
       end (* with *) ;

     (**************************************)
     (* alfa type (char array)             *)
     (**************************************)

     NEW ( PTYPE_ALFA ) ;
     PTYPE_ALFA -> := ALFATYPE ;
     with PTYPE_ALFA -> do
       begin
         AELTYPE := PTYPE_CHAR ;
         NEW ( INXTYPE ) ;
         INXTYPE -> := ALFAINX ;
       end (* with *) ;

     (**************************************)
     (* undefined constant                 *)
     (* undefined type                     *)
     (* undefined var                      *)
     (* etc.                               *)
     (**************************************)

     NEW ( UCSTPTR ) ;
     UCSTPTR -> := UCST ;
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

   type STDPROC = record
                    NAME : ALPHA ;
                    KEY : INTEGER ;
                    KLASS : IDCLASS
                  end ;
        ESTDPROC = record
                     NAME : ALPHA ;
                     EXTNAM : EXTNAMTP
                   end ;
        XSTDPROC = record
                     NAME : ALPHA ;          // name of stdproc
                     KEY : INTEGER ;         // key of stdproc
                     KLASS : IDCLASS ;       // func or proc
                     LIBNAME : EXTNAMTP ;    // library function
                     FUNCCODE : INTEGER ;    // function code
                     PARMCNT : INTEGER ;     // parameter count
                     PROCTYP : CHAR ;        // result type
                     WASIZE : INTEGER ;      // workarea size
                   end ;

   var CP , CP1 : IDP ;
       I , J : INTEGER ;
       SP : STDPROC ;
       ESP : ESTDPROC ;
       XSP : XSTDPROC ;

   const CNA : array [ 1 .. 2 ] of ALPHA =
         ( 'FALSE   ' , 'TRUE    ' ) ;
         DTFNA : array [ 1 .. 2 ] of ALPHA =
         ( 'DATE    ' , 'TIME    ' ) ;

         //****************************************************
         // standard files                                     
         // known to the compiler                              
         //****************************************************

         FILNA : array [ 1 .. 6 ] of ALPHA =
         ( 'INPUT   ' , 'OUTPUT  ' , 'PRD     ' , 'PRR    ' ,
           'QRD     ' , 'QRR     ' ) ;

         //****************************************************
         // standard procedures and functions                  
         // known to the compiler                              
         //****************************************************

         STDP : array [ 1 .. 60 ] of STDPROC =
         ( ( 'PAGE      ' , 0 , PROC ) , ( 'GET       ' , 1 , PROC ) ,
           ( 'PUT       ' , 2 , PROC ) , ( 'RESET     ' , 3 , PROC ) ,
           ( 'REWRITE   ' , 4 , PROC ) , ( 'READ      ' , 5 , PROC ) ,
           ( 'WRITE     ' , 6 , PROC ) , ( 'PACK      ' , 7 , PROC ) ,
           ( 'UNPACK    ' , 8 , PROC ) , ( 'NEW       ' , 9 , PROC ) ,
           ( 'RELEASE  ' , 10 , PROC ) , ( 'READLN   ' , 11 , PROC ) ,
           ( 'WRITELN  ' , 12 , PROC ) , ( 'MARK     ' , 13 , PROC ) ,
           ( 'TRAP     ' , 14 , PROC ) , ( 'EXIT     ' , 15 , PROC ) ,
           ( 'TRACE    ' , 30 , PROC ) , ( 'ABS      ' , 16 , FUNC ) ,
           ( 'SQR      ' , 17 , FUNC ) , ( 'TRUNC    ' , 18 , FUNC ) ,
           ( 'ROUND    ' , 19 , FUNC ) , ( 'ORD      ' , 20 , FUNC ) ,
           ( 'CHR      ' , 21 , FUNC ) , ( 'PRED     ' , 22 , FUNC ) ,
           ( 'SUCC     ' , 23 , FUNC ) , ( 'CLOCK    ' , 24 , FUNC ) ,
           ( 'EOF      ' , 25 , FUNC ) , ( 'EOLN     ' , 26 , FUNC ) ,
           ( 'ODD      ' , 33 , FUNC ) , ( 'EOL      ' , 28 , FUNC ) ,
           ( 'EOT      ' , 29 , FUNC ) , ( 'MESSAGE  ' , 35 , PROC ) ,
           ( 'SKIP     ' , 36 , PROC ) , ( 'LINELIMIT' , 37 , PROC ) ,
           ( 'CARD     ' , 38 , FUNC ) , ( 'EXPO     ' , 39 , FUNC ) ,
           ( 'ADDR     ' , 40 , FUNC ) , ( 'PTRADD   ' , 41 , FUNC ) ,
           ( 'PTRDIFF  ' , 42 , FUNC ) , ( 'SIZEOF   ' , 43 , FUNC ) ,
           ( 'PTR2INT  ' , 44 , FUNC ) , ( 'PTRCAST  ' , 45 , FUNC ) ,
           ( 'CLOSE    ' , 46 , PROC ) , ( 'FLOOR    ' , 47 , FUNC ) ,
           ( 'MEMSET   ' , 75 , PROC ) , ( 'MEMCPY   ' , 76 , PROC ) ,

         //****************************************************
         // new functions since compiler release 2018.01       
         //****************************************************


           ( 'DIGITSOF    ' , 78 , FUNC ) ,    // digits of decimal
           ( 'PRECISIONOF ' , 79 , FUNC ) ,    // precision of decimal
           ( 'STR         ' , 80 , FUNC ) ,    // convert to string
           ( 'MAXLENGTH   ' , 81 , FUNC ) ,    // maxlength of string
           ( 'LENGTH      ' , 82 , FUNC ) ,    // length of string
           ( 'STRRESULT   ' , 83 , FUNC ) ,    // result of str func
           ( 'STRRESULTP  ' , 84 , FUNC ) ,    // ptr to str result
           ( 'REPEATSTR   ' , 85 , FUNC ) ,    // repeat str n times
           ( 'RESULTP     ' , 92 , FUNC ) ,    // ptr to result
           ( '           ' , - 1 , PROC ) ,    //
           ( '           ' , - 1 , PROC ) ,    //
           ( '           ' , - 1 , PROC ) ,    //
           ( '           ' , - 1 , PROC ) ,    //
           ( '           ' , - 1 , PROC ) ) ;  //

         //****************************************************
         // standard (fortran) math functions                  
         // known to the compiler                              
         //****************************************************

         ESTDP : array [ 1 .. 10 ] of ESTDPROC =
         ( ( 'SIN     ' , 'DSIN    ' ) , ( 'COS     ' , 'DCOS    ' ) ,
           ( 'EXP     ' , 'DEXP    ' ) , ( 'SQRT    ' , 'DSQRT   ' ) ,
           ( 'LN      ' , 'DLOG    ' ) , ( 'ARCTAN  ' , 'DATAN   ' ) ,
           ( '        ' , '        ' ) , ( '        ' , '        ' ) ,
           ( '        ' , '        ' ) , ( '        ' , '        ' ) )
           ;

         //****************************************************
         // extended standard functions and procedures         
         // implemented in pascal                              
         // see sourcefile paslibx.pas                         
         //****************************************************

         XSTDP : array [ 1 .. 20 ] of XSTDPROC =
         ( ( 'ALLOC    ' , 61 , FUNC , '$PASMEM ' , 1 , 1 , 'A' , 0 ) ,
           ( 'ALLOCX   ' , 62 , FUNC , '$PASMEM ' , 2 , 1 , 'A' , 0 ) ,
           ( 'FREE     ' , 63 , PROC , '$PASMEM ' , 3 , 1 , 'P' , 0 ) ,
           ( 'FREEX    ' , 64 , PROC , '$PASMEM ' , 4 , 1 , 'P' , 0 ) ,
           ( 'CHKHEAP  ' , 65 , PROC , '$PASMEM ' , 5 , 1 , 'P' , 0 ) ,
           ( 'CHKALLOC ' , 66 , FUNC , '$PASMEM ' , 6 , 1 , 'A' , 0 ) ,
           ( 'FILEFCB  ' , 70 , FUNC , '$PASMEM ' , 7 , 1 , 'A' , 0 ) ,
           ( 'ROUNDX   ' , 77 , FUNC , '$PASMAT ' , 1 , 2 , 'R' , 0 ) ,
           ( 'SUBSTR   ' , 86 , FUNC , '$PASSTR1' , 1 , 3 , 'V' , 1 ) ,
           ( 'DELETE   ' , 87 , FUNC , '$PASSTR1' , 2 , 3 , 'V' , 1 ) ,
           ( 'RTRIM    ' , 88 , FUNC , '$PASSTR1' , 3 , 1 , 'V' , 1 ) ,
           ( 'LTRIM    ' , 89 , FUNC , '$PASSTR1' , 4 , 1 , 'V' , 1 ) ,
           ( 'TRIM     ' , 90 , FUNC , '$PASSTR1' , 5 , 1 , 'V' , 1 ) ,
           ( 'COMPRESS ' , 91 , FUNC , '$PASSTR1' , 6 , 1 , 'V' , 1 ) ,
           ( 'INDEX    ' , 93 , FUNC , '$PASSTR2' , 1 , 2 , 'I' , 2 ) ,
           ( 'VERIFY   ' , 94 , FUNC , '$PASSTR2' , 2 , 2 , 'I' , 2 ) ,
           ( 'TRANSLATE' , 95 , FUNC , '$PASSTR3' , 1 , 3 , 'V' , 3 ) ,
           ( '         ' , - 1 , PROC , '     ' , - 1 , 0 , ' ' , 0 ) ,
           ( '         ' , - 1 , PROC , '     ' , - 1 , 0 , ' ' , 0 ) ,
           ( '         ' , - 1 , PROC , '     ' , - 1 , 0 , ' ' , 0 ) )
           ;

   begin (* ENTSTDNAMES *)

     (****************)
     (* ANYPTR       *)
     (****************)

     NEW ( CP , TYPES ) ;
     with CP -> do
       begin
         NAME := 'ANYPTR' ;
         IDTYPE := PTYPE_ANY ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;

     (**********************************)
     (* VOIDPTR = SYNONYM FOR ANYPTR   *)
     (**********************************)

     NEW ( CP , TYPES ) ;
     with CP -> do
       begin
         NAME := 'VOIDPTR' ;
         IDTYPE := PTYPE_ANY ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;

     (****************)
     (* INTEGER      *)
     (****************)

     NEW ( CP , TYPES ) ;
     with CP -> do
       begin
         NAME := 'INTEGER' ;
         IDTYPE := PTYPE_INT ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;

     (****************)
     (* REAL         *)
     (****************)

     NEW ( CP , TYPES ) ;
     with CP -> do
       begin
         NAME := 'REAL' ;
         IDTYPE := PTYPE_REAL ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;

     (****************)
     (* CHAR         *)
     (****************)

     NEW ( CP , TYPES ) ;
     with CP -> do
       begin
         NAME := 'CHAR' ;
         IDTYPE := PTYPE_CHAR ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;

     (****************)
     (* BOOLEAN      *)
     (****************)

     NEW ( CP , TYPES ) ;
     with CP -> do
       begin
         NAME := 'BOOLEAN' ;
         IDTYPE := PTYPE_BOOL ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;

     (****************)
     (* ANYFILE      *)
     (****************)

     NEW ( CP , TYPES ) ;
     with CP -> do
       begin
         NAME := 'ANYFILE' ;
         IDTYPE := PTYPE_ANYFILE ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;

     (****************)
     (* TEXT         *)
     (****************)

     NEW ( CP , TYPES ) ;
     with CP -> do
       begin
         NAME := 'TEXT' ;
         IDTYPE := PTYPE_TEXT ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;

     (****************)
     (* ALFA         *)
     (****************)

     NEW ( CP , TYPES ) ;
     with CP -> do
       begin
         NAME := 'ALFA' ;
         IDTYPE := PTYPE_ALFA ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;

     (****************)
     (* decimal      *)
     (****************)

     NEW ( CP , TYPES ) ;
     with CP -> do
       begin
         NAME := 'DECIMAL' ;
         IDTYPE := PTYPE_DECIMAL ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;

     (****************)
     (* varchar      *)
     (****************)

     NEW ( CP , TYPES ) ;
     with CP -> do
       begin
         NAME := 'VARCHAR' ;
         IDTYPE := PTYPE_VARCHAR ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;

     (**********************)
     (* string = varchar   *)
     (**********************)

     NEW ( CP , TYPES ) ;
     with CP -> do
       begin
         NAME := 'STRING' ;
         IDTYPE := PTYPE_VARCHAR ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;

     (****************)
     (* MAXINT       *)
     (****************)

     NEW ( CP , KONST ) ;
     with CP -> do
       begin
         NAME := 'MAXINT' ;
         IDTYPE := PTYPE_INT ;
         KLASS := KONST ;
         VALUES . IVAL := MAXINT ;
         VALUES . STRTYPE := ' ' ;
       end (* with *) ;
     ENTERID ( CP ) ;

     (*****************************)
     (* constants of boolean type *)
     (*****************************)

     CP1 := NIL ;
     for I := 1 to 2 do
       begin

     (******************)
     (* FALSE, TRUE    *)
     (******************)

         NEW ( CP , KONST ) ;
         with CP -> do
           begin
             NAME := CNA [ I ] ;
             IDTYPE := PTYPE_BOOL ;
             NEXT := CP1 ;
             VALUES . IVAL := I - 1 ;
             VALUES . STRTYPE := ' ' ;
             KLASS := KONST
           end (* with *) ;
         ENTERID ( CP ) ;
         CP1 := CP
       end (* for *) ;
     PTYPE_BOOL -> . FCONST := CP ;

     (***************)
     (* NIL         *)
     (***************)

     NEW ( CP , KONST ) ;
     with CP -> do
       begin
         NAME := 'NIL' ;
         IDTYPE := PTYPE_ANY ;
         NEXT := NIL ;
         VALUES . IVAL := 0 ;
         VALUES . STRTYPE := ' ' ;
         KLASS := KONST
       end (* with *) ;
     ENTERID ( CP ) ;

     (********************)
     (* predefined files *)
     (********************)

     for I := 1 to 6 do
       begin
         NEW ( CP , VARS ) ;
         with CP -> do
           begin
             NAME := FILNA [ I ] ;
             IDTYPE := PTYPE_TEXT ;
             KLASS := VARS ;
             VKIND := NORMALVAR ;
             STKLASS := XAUTO ;
             VOWNERPROC := ' ' ;
             SPECIAL := 0 ;
             NEXT := NIL ;
             VLEV := 1 ;
             VADDR := FIRSTFILBUF + ( I - 1 ) * FILMINSIZE ;
             if I = 1 then
               INPUTPTR := CP
             else
               if I = 2 then
                 OUTPUTPTR := CP ;
           end (* with *) ;
         ENTERID ( CP )
       end (* for *) ;

     (***************************)
     (* date and time functions *)
     (***************************)

     for I := 1 to 2 do
       begin
         NEW ( CP , VARS ) ;
         with CP -> do
           begin
             NAME := DTFNA [ I ] ;
             IDTYPE := PTYPE_ALFA ;
             KLASS := VARS ;
             VKIND := NORMALVAR ;
             STKLASS := XAUTO ;
             VOWNERPROC := ' ' ;
             NEXT := NIL ;
             VLEV := 1 ;
             VADDR := TIMEDATELOC + ( I - 1 ) * ALFALNGTH ;
             SPECIAL := I ;
           end (* with *) ;
         ENTERID ( CP )
       end (* for *) ;

     //************************************************************
     // OSPARM pointer ...                                         
     // is defined here as                                         
     // var OSPARM : -> record                                     
     //                    LENGTH : integer ;                      
     //                    STRING : array [ 1 .. MAXSTRL ]         
     //                             of char ;                      
     //                 end ;                                      
     // +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     // the component names will be changed later                  
     // because they now interfere with the predefined names       
     // STRING (type) and LENGTH (standard function)               
     //************************************************************

     NEW ( CP , VARS ) ;
     with CP -> do
       begin
         NAME := 'OSPARM' ;
         KLASS := VARS ;
         VKIND := NORMALVAR ;
         STKLASS := XAUTO ;
         VOWNERPROC := ' ' ;
         NEXT := NIL ;
         VLEV := 1 ;
         VADDR := OSPARMLOC ;
         SPECIAL := 3 ;
         ENTERID ( CP ) ;
         NEW ( IDTYPE , POINTER ) ;
         with IDTYPE -> do
           begin
             ERRORFLAG := FALSE ;
             SIZE := PTRSIZE ;
             ALN := PTRSIZE ;
             FORM := POINTER ;
             NEW ( ELTYPE , RECORDS ) ;

     (***************************)
     (* TYPE OF THE PARM RECORD *)
     (***************************)

             with ELTYPE -> do
               begin
                 ERRORFLAG := FALSE ;
                 SIZE := INTSIZE + MAXSTRL * CHARSIZE ;
                 ALN := PTRSIZE ;
                 FORM := RECORDS ;
                 RECVAR := NIL ;
                 FLD_DISP_LEV := - 1 ;
                 NO_FLDS := 2 ;
                 NEW ( FSTFLD , FIELD ) ;
                 with FSTFLD -> do
                   begin
                     NAME := 'LENGTH' ;
                     IDTYPE := PTYPE_INT ;
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
                         NAME := 'STRING' ;
                         FLDADDR := PTRSIZE ;
                         NEXT := NIL ;
                         KLASS := FIELD ;
                         NEW ( IDTYPE , ARRAYS ) ;
                         with IDTYPE -> do
                           begin
                             ERRORFLAG := FALSE ;
                             SIZE := MAXSTRL * CHARSIZE ;
                             ALN := CHARSIZE ;
                             FORM := ARRAYS ;
                             AELTYPE := PTYPE_CHAR ;
                             NEW ( INXTYPE , SUBRANGE ) ;
                             with INXTYPE -> do
                               begin
                                 ERRORFLAG := FALSE ;
                                 FORM := SUBRANGE ;
                                 RANGETYPE := PTYPE_INT ;
                                 MIN . IVAL := 1 ;
                                 MIN . STRTYPE := ' ' ;
                                 MAX . IVAL := MAXSTRL ;
                                 MAX . STRTYPE := ' ' ;
                               end (* with *) ;
                           end (* with *) ;
                       end (* with *) ;
                     ENTERID ( NEXT ) ;
                     NEXT -> . OWNER := CP -> . IDTYPE -> . ELTYPE ;
                     if TOP >= 0 then
                       TOP := TOP - 1 ;
                   end (* with *) ;
               end (* with *) ;
           end (* with *) ;
       end (* with *) ;

     (***************************************)
     (* predefined procedures and functions *)
     (***************************************)

     for I := 1 to 60 do
       begin
         SP := STDP [ I ] ;
         if SP . NAME [ 1 ] = ' ' then
           break ;
         NEW ( CP , PROC , STANDARD ) ;
         with CP -> do
           begin
             NAME := SP . NAME ;
             IDTYPE := NIL ;
             NEXT := NIL ;
             KEY := SP . KEY ;
             LIBNAME := '' ;
             FUNCCODE := - 1 ;
             PARMCNT := 0 ;
             KLASS := SP . KLASS ;
             PFDECKIND := STANDARD ;
             EXTRN := FALSE ;
             EXTLANG := ' ' ;
             EXTNAME := '' ;
           end (* with *) ;
         ENTERID ( CP )
       end (* for *) ;

     (***********************************)
     (* PARAMETER OF math FUNCTIONS     *)
     (***********************************)

     NEW ( CP1 , VARS ) ;
     with CP1 -> do
       begin
         NAME := BLANKID ;
         IDTYPE := PTYPE_REAL ;
         KLASS := VARS ;
         VKIND := NORMALVAR ;
         STKLASS := XAUTO ;
         VOWNERPROC := ' ' ;
         SPECIAL := 0 ;
         NEXT := NIL ;
         VLEV := 1 ;
         VADDR := LCAFTMST + FPSAVEAREA ;
       end (* with *) ;

     (***********************************)
     (* external fortran functions      *)
     (* that is: math functions         *)
     (* SIN, COS, EXP, SQRT, LN, ARCTAN *)
     (***********************************)

     for I := 1 to 10 do
       begin
         ESP := ESTDP [ I ] ;
         if ESP . NAME [ 1 ] = ' ' then
           break ;
         NEW ( CP , FUNC , DECLARED ) ;
         with CP -> do
           begin
             NAME := ESP . NAME ;
             IDTYPE := PTYPE_REAL ;
             NEXT := NIL ;
             PRMPTR := CP1 ;
             FWDECL := FALSE ;
             EXTRN := TRUE ;
             EXTLANG := 'F' ;
             EXTNAME := ESP . EXTNAM ;
             KLASS := FUNC ;
             PFDECKIND := DECLARED ;
             PFKIND := NORMALVAR ;
             PFLEV := 0 ;
             PFNAME := 0 ;
             CSTNAME := '' ;
           end (* with *) ;
         ENTERID ( CP ) ;
       end (* for *) ;

     (***********************************)
     (* external library functions      *)
     (***********************************)

     for I := 1 to 20 do
       begin
         XSP := XSTDP [ I ] ;
         if XSP . NAME [ 1 ] = ' ' then
           break ;
         NEW ( CP , PROC , STANDARD ) ;
         with CP -> do
           begin
             NAME := XSP . NAME ;
             EXTRN := FALSE ;
             EXTLANG := ' ' ;
             EXTNAME := '' ;
             IDTYPE := NIL ;
             NEXT := NIL ;
             KEY := XSP . KEY ;
             KLASS := XSP . KLASS ;
             PFDECKIND := STANDARD ;
             LIBNAME := XSP . LIBNAME ;
             FUNCCODE := XSP . FUNCCODE ;
             PARMCNT := XSP . PARMCNT ;
             case XSP . PROCTYP of
               'P' : IDTYPE := NIL ;
               'R' : IDTYPE := PTYPE_REAL ;
               'B' : IDTYPE := PTYPE_BOOL ;
               'C' : IDTYPE := PTYPE_CHAR ;
               'I' : IDTYPE := PTYPE_INT ;
               'A' : IDTYPE := PTYPE_ANY ;
               'V' : IDTYPE := PTYPE_VARCHAR ;
             end (* case *) ;
             WASIZE := XSP . WASIZE ;
           end (* with *) ;
         ENTERID ( CP ) ;
       end (* for *) ;

     (**************************************)
     (* SNAPSHOT                           *)
     (**************************************)

     NEW ( CP , PROC , DECLARED ) ;
     with CP -> do
       begin
         NAME := 'SNAPSHOT' ;
         IDTYPE := NIL ;
         EXTRN := TRUE ;
         EXTLANG := ' ' ;
         EXTNAME := '$PASSNAP' ;
         FWDECL := FALSE ;
         PFLEV := 0 ;
         PFNAME := 0 ;
         KLASS := PROC ;
         PFDECKIND := DECLARED ;
         PFKIND := NORMALVAR ;
         CSTNAME := '' ;
         DECLMISSING := FALSE ;
         NEXT := NIL ;
       end (* with *) ;
     ENTERID ( CP ) ;

     (*******************************)
     (* FIRST PARAMETER OF SNAPSHOT *)
     (*******************************)

     NEW ( CP -> . PRMPTR , VARS ) ;

     (********************************)
     (* SECOND PARAMETER OF SNAPSHOT *)
     (********************************)

     NEW ( CP1 , VARS ) ;
     with CP1 -> do
       begin
         IDTYPE := PTYPE_INT ;
         KLASS := VARS ;
         VKIND := NORMALVAR ;
         STKLASS := XAUTO ;
         VOWNERPROC := ' ' ;
         SPECIAL := 0 ;
         NEXT := NIL ;
         VLEV := 1 ;
         VADDR := LCAFTMST + FPSAVEAREA + INTSIZE
       end (* with *) ;

     (*******************************)
     (* FIRST PARAMETER OF SNAPSHOT *)
     (*******************************)

     with CP -> . PRMPTR -> do
       begin
         IDTYPE := PTYPE_INT ;
         KLASS := VARS ;
         VKIND := NORMALVAR ;
         STKLASS := XAUTO ;
         VOWNERPROC := ' ' ;
         NEXT := CP1 ;
         VLEV := 1 ;
         VADDR := LCAFTMST + FPSAVEAREA
       end (* with *) ;
   end (* ENTSTDNAMES *) ;



procedure INITSCALARS ;

   var ERRNO : INTEGER ;
       INPLINE : SOURCELINE ;
       ERRMSG : SOURCELINE ;
       ERRMSGL : INTEGER ;
       CTEMP : array [ 1 .. 16 ] of CHAR ;

   begin (* INITSCALARS *)

     (****************************************)
     (* default values for compiler options  *)
     (* and: init control blocks for scanner *)
     (****************************************)

     MEMSET ( ADDR ( SCB ) , CHR ( 0 ) , SIZEOF ( SCB ) ) ;
     MEMSET ( ADDR ( OPT ) , CHR ( 0 ) , SIZEOF ( OPT ) ) ;
     OPT . LMARGIN := 0 ;
     OPT . RMARGIN := 80 ;
     OPT . PAGESIZE := 72 ;
     OPT . LIST := TRUE ;
     OPT . PRCODE := TRUE ;
     OPT . GET_STAT := TRUE ;
     OPT . SAVEREGS := TRUE ;
     OPT . SAVEFPRS := TRUE ;
     OPT . DEBUG := TRUE ;
     OPT . MWARN := FALSE ;
     OPT . DEBUG_LEV := 2 ;
     OPT . NOPACKING := FALSE ;
     OPT . NESTCOMM := FALSE ;
     OPT . WARNING := TRUE ;
     OPT . ASSEMBLE := FALSE ;
     OPT . ASMVERB := FALSE ;
     OPT . CTROPTION := FALSE ;
     SCB . MAXLSYMBOL := MAXLSIZE ;
     SCB . MODUS := 1 ;
     SCB . SYMBOLNR := SYMB_UNKNOWN ;
     SCB . SLINE := ' ' ;
     SCB . LINENR := 0 ;
     SCB . LINEPOS := 1 ;
     SCB . LINELEN := 0 ;
     SCB . FEANFANG := NIL ;
     SCB . FTTAB := NIL ;
     SCB . FTTABA := NIL ;
     SCB . POPT := ADDR ( OPT ) ;
     SCB . FEAKT := NIL ;
     SCB . FEAKT_ALT := NIL ;

     (********************************************)
     (* listing is printed by scanner, too       *)
     (* so all needed information has to be      *)
     (* provided in the scanner control block    *)
     (********************************************)
     (* and terminal output in case of error     *)
     (********************************************)

     SCB . PROTOUT := TRUE ;
     SCB . TERMOUT := TRUE ;
     SCB . LINEINFO := ' ' ;
     SCB . LINEINFO_SIZE := 0 ;
     SCB . HEADLINE := '1LINE #   D/NEST  LVL    '
                       '< STANFORD PASCAL, OPPOLZER VERSION OF '
                       'MM.YYYY' ' >' '    hh:mm:ss  DD/MM/YYYY   ' ;
     CTEMP := VERSION ;
     MEMCPY ( ADDR ( SCB . HEADLINE [ 65 ] ) , ADDR ( CTEMP ) , 7 ) ;
     MEMCPY ( ADDR ( SCB . HEADLINE [ 78 ] ) , ADDR ( TIME ) , 8 ) ;
     MEMCPY ( ADDR ( SCB . HEADLINE [ 88 ] ) , ADDR ( DATE ) , 10 ) ;
     SCB . HEADLINE_SIZE := 100 ;

     (****************************************)
     (* linecount high to force heading      *)
     (* on first insymbol call               *)
     (****************************************)

     SCB . LINECOUNT := 100 ;

     (****************************************)
     (* read error messages file and         *)
     (* setup scanner tables                 *)
     (****************************************)

     RESET ( PRD ) ;
     while not EOF ( PRD ) do
       begin
         READLN ( PRD , ERRNO , INPLINE ) ;
         ERRMSG := ' ' ;
         MEMCPY ( ADDR ( ERRMSG ) , ADDR ( INPLINE [ 4 ] ) , SIZEOF (
                  SOURCELINE ) - 3 ) ;
         ERRMSGL := SIZEOF ( SOURCELINE ) ;
         while ERRMSG [ ERRMSGL ] = ' ' do
           ERRMSGL := ERRMSGL - 1 ;
         PASSCANF ( SCB , 'A' , 'P' , ERRNO , ERRMSG , ERRMSGL ) ;
       end (* while *) ;
     FWPTR := NIL ;
     STMTNEST := 0 ;

     (****************************************)
     (* init of listcat and level            *)
     (* old values to force changes on       *)
     (* first call to insymbol               *)
     (****************************************)

     LISTTAG := ' ' ;
     LEVEL := 0 ;
     DCN := 0 ;
     OLDLISTTAG := CHR ( 0 ) ;
     OLDLEVEL := 0 ;
     OLDDCN := 0 ;
     OLDLINENR := - 1 ;
     OLDLCOUNTER := 0 ;
     OLDCONSTLCOUNTER := 0 ;

     (****************************************)
     (* other initializations                *)
     (****************************************)

     CONSTLCOUNTER := 0 ;
     HP := FALSE ;
     ICOUNTER := 0 ;
     INTLABEL := 0 ;
     FILEHEAD := NIL ;
     LCOUNTER := FIRSTGVAR ;

     (***************************************)
     (* ADR. OF THE FIRST GLOBAL VARIABLE   *)
     (* NOTE IN THE ABOVE RESERVATION Of    *)
     (* BUFFER STORE FOR TEXT FILES         *)
     (***************************************)

     OLDICOUNTER := 0 ;
     ICOUNTER := 0 ;
     LINECNT := 0 ;
     PLCNT := OPT . PAGESIZE ;

     (****************************)
     (* GENERATES FIRST HEADLINE *)
     (****************************)

     OLDLN := 0 ;
     GLOBTESTP := NIL ;
     OPEN_RECORD := NIL ;
     PROGNAME := '$PASMAIN' ;
     PROCLAB := 0 ;
     ERRORCNT := 0 ;
     WARNCNT := 0 ;
     ERRKIND := 'E' ;
     ASSIGN := FALSE ;
     FLIPDEBUG := FALSE ;
     EXTUSED := FALSE ;
     PACKDATA := FALSE ;
     IS_MODULE := FALSE ;

     (*************************)
     (*GENERATES UNIQUE NAMES *)
     (*************************)

     PRNTTYPHD := NIL ;
     PRNTTYNO := 0 ;
     FRTPARHD := NIL ;
     XLABNO := 0 ;
     CTRCNT := 0 ;
     FENT_CNT := 0 ;
     SF_CNT := 0 ;
     SF_TOT := 0 ;
     WE_CNT := 0 ;
     RE_CNT := 0 ;
     WS_CNT := 0 ;

     (***************************************************)
     (* variables derived from constants                *)
     (* to speed up things a bit                        *)
     (***************************************************)

     MXINT2 := MAXINT DIV 2 ;
     MXINT10 := MAXINT DIV 10 ;
     MXINT16 := MAXINT DIV 16 ;

     (***************************************************)
     (* alloc storage for - relatively large - set info *)
     (***************************************************)

     NEW ( PSIGLOB ) ;

     (***************************************************)
     (* working storage control for stringss            *)
     (***************************************************)

     CTLS . WATCH1 := FALSE ;
     CTLS . VPU1_OFFSET := 0 ;
     CTLS . VPU1_LEVEL := 0 ;
     CTLS . VPO1_NEEDED := FALSE ;
     CTLS . VPU2_DONE := FALSE ;
     CTLS . VPU2_OFFSET := 0 ;
     CTLS . VPU2_LEVEL := 0 ;
   end (* INITSCALARS *) ;



procedure INITTABLES ;

   var K : BKT_RNG ;
       I , J : INTEGER ;


   procedure RATORS ;

      var I : INTEGER ;
          CH : CHAR ;

      begin (* RATORS *)

        (***************************************)
        (*   useful initializations            *)
        (***************************************)

        for CH := CHR ( 0 ) to CHR ( ORDCHMAX ) do
          begin
            UPSHIFT [ CH ] := CH ;
          end (* for *) ;

        (***************************************)
        (*   an old comment told about some    *)
        (*   troubles with upshift, because    *)
        (*   some meaningful chars (to pascal) *)
        (*   are in the letter range 'a' to    *)
        (*   'z', but are no letters ...       *)
        (*   see the EBCDIC letter gaps.       *)
        (*   I avoided those problems by       *)
        (*   the three loops below             *)
        (*   - bernd oppolzer (2016)           *)
        (***************************************)

        for CH := 'a' to 'i' do
          begin
            UPSHIFT [ CH ] := CHR ( ORD ( CH ) - ORD ( 'a' ) + ORD (
                              'A' ) ) ;
          end (* for *) ;
        for CH := 'j' to 'r' do
          begin
            UPSHIFT [ CH ] := CHR ( ORD ( CH ) - ORD ( 'a' ) + ORD (
                              'A' ) ) ;
          end (* for *) ;
        for CH := 's' to 'z' do
          begin
            UPSHIFT [ CH ] := CHR ( ORD ( CH ) - ORD ( 'a' ) + ORD (
                              'A' ) ) ;
          end (* for *) ;

        (**************************************************************)
        (* Note: SSY array should be defined as full range of         *)
        (*       characters, but it's ok as is for now because it is  *)
        (*       currently indexed only for specific characters.      *)
        (*       If it were indexed by higher chars (e.g. backslash   *)
        (*       or curly brackets) in the future, its definition     *)
        (*       would have to change. Currently, SPECH does not      *)
        (*       include any of these higher chars. - D.E. 02feb2007  *)
        (**************************************************************)
        (* fixed in 2016 during Windows / Linux port - bernd oppolzer *)
        (**************************************************************)
        (* SSY completely removed when moving to new                  *)
        (* PASSCAN scanner, same goes for table SOP and               *)
        (* type OPERSYMB ... all symbol handling is now done          *)
        (* in PASSCAN by generated logic / B.Oppolzer - 2017          *)
        (**************************************************************)

      end (* RATORS *) ;


   begin (* INITTABLES *)
     RATORS ;
     ERRLOG := [ ] ;

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

  (**********************************************************)
  (* INITIALIZE                                             *)
  (**********************************************************)

  INITSCALARS ;

  (*******************************************)
  (* ENTER STANDARD NAMES AND STANDARD TYPES *)
  (*******************************************)

  INITTABLES ;
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
  OPT . GET_STAT := FALSE ;

  (***************************************************)
  (* set options passed as parameter to the compiler *)
  (* needs to be reworked later - will not work ...  *)
  (***************************************************)

  if OSPARM = NIL then
    SOURCENAME := ''
  else
    with OSPARM -> do
      begin
        SOURCENAME := '' ;
        for SX := 1 to 8 do
          if SX <= LENGTH then
            SOURCENAME [ SX ] := STRING [ SX ] ;
        if LENGTH > 8 then
          begin
            OPTLINE := ' ' ;
            for CHCNT := 9 to LENGTH do
              OPTLINE [ CHCNT - 8 ] := STRING [ CHCNT ] ;
            WORK_OPTIONS ( OPTLINE , SCB , OPT ) ;
          end (* then *)
      end (* with *) ;

  (*********************************************)
  (* hello = PRINT start COMPILATION MESSAGE   *)
  (* insymbol = scan first symbol              *)
  (*********************************************)

  HELLO ;
  INSYMBOL ;

  (***********)
  (* COMPILE *)
  (***********)

  PROGRAMME ( BLOCKBEGSYS + STATBEGSYS - [ SYCASE ] ) ;

  (*********************************************)
  (* goodbye = PRINT POST COMPILATION MESSAGES *)
  (*********************************************)

  GOODBYE ;
end (* HAUPTPROGRAMM *) .
