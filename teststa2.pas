program TESTSTA2 ( INPUT , OUTPUT , PCODE , LISTING , LISTDEF , DBGINFO
                   , TRACEF ) ;

(********************************************************************)
(*$D+,N+,A+                                                         *)
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
(********************************************************************)
(*                                                                  *)
(*  History records - newest first                                  *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Aug 2020 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  The CHK instruction was not portable so far, because the        *)
(*  limits were always coded as integers, even if CHK was used      *)
(*  to CHK for a subrange of chars, like in 'A' .. 'Z'              *)
(*                                                                  *)
(*  This had to be fixed, because otherwise P-Code files cannot     *)
(*  be transferred across platforms                                 *)
(*                                                                  *)
(*  Now, if the limits of a char subrange are coded as char         *)
(*  constants (and not, for example, as hex constants), the         *)
(*  limits on the CHK instruction are written as char constants,    *)
(*  too, which should make the P-Code files portable.               *)
(*                                                                  *)
(*  Changes to PASCAL2 to tolerate the modificated coding of        *)
(*  the check instruction.                                          *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Aug 2020 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  More rework of READ runtime functions:                          *)
(*                                                                  *)
(*  READ of reals is implemented as Pascal function $PASRDR         *)
(*                                                                  *)
(*  READ of booleans implemented as Pascal function $PASRDB         *)
(*                                                                  *)
(*  READSTR now works for types integer, real, single char          *)
(*  and varying and fixed size character strings.                   *)
(*                                                                  *)
(*  see PASLIBX.PAS:                                                *)
(*                                                                  *)
(*  function $PASRDI ( var F : TEXT ; ...                           *)
(*  function $PASRDR ( var F : TEXT ; ...                           *)
(*  function $PASRDB ( var F : TEXT ; ...                           *)
(*  function $PASRSI ( const S : STRING ; ...                       *)
(*  function $PASRSR ( const S : STRING ; ...                       *)
(*  function $PASRSC ( const S : STRING ; ...                       *)
(*  procedure $PASRSV ( const S : STRING ; ...                      *)
(*  procedure $PASRSS ( const S : STRING ; ...                      *)
(*                                                                  *)
(*  READSTR for booleans and READ for scalar types (enums)          *)
(*  will follow soon                                                *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Aug 2020 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  I fixed some errors in PASCAL2 related to code generation,      *)
(*  see there.                                                      *)
(*                                                                  *)
(*  I had to do some rework in the way the GET standard proc        *)
(*  works, because it is heavily used in the new runtime procs      *)
(*  $PASRDI and $PASRDR (read integer and read reals, written       *)
(*  in Pascal). GET now works better, too; no surprises any         *)
(*  more. See the document which covers all the rework done to      *)
(*  the READ and WRITE procedures including formatted read          *)
(*  (with length specified) and READSTR and WRITESTR, as            *)
(*  known from Pascal/VS                                            *)
(*                                                                  *)
(*  This involved not only changes to PCINT, but also changes       *)
(*  to the Pascal monitor on the mainframe (PASMONN ASSEMBLE).      *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Apr 2020 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  - New Version, although the rework on READ and WRITE is not     *)
(*    yet completed. READSTR already works for some types,          *)
(*    much the same way as defined by Pascal/VS. WRITESTR           *)
(*    is still missing.                                             *)
(*                                                                  *)
(*  - The new version is needed because of some errors which        *)
(*    have to be repaired fast (thanks to George Smith):            *)
(*                                                                  *)
(*    a) READ of single char no more reads next lines               *)
(*       without READLN ... but it should (repaired 12.04.)         *)
(*                                                                  *)
(*    b) set comparisons of sets with different (internal) size     *)
(*       are not handled at all by PCINT ... for example            *)
(*       comparisons of set variables and set constants,            *)
(*       when the base type of the set is an integer subrange       *)
(*       (will be repaired in mid April 2020)                       *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Jan 2020 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  - Implement new READ procedures (new CSPs) which allow          *)
(*    width parameters like in Pascal/VS. At first, the new         *)
(*    procedures are implemented for chars, char arrays and         *)
(*    strings (varchars). The new CSPs are called RFC, RFS and      *)
(*    RFV. The old CSPs RDC, RDS and RDV are obsolete now           *)
(*    and will be phased out in a later release.                    *)
(*                                                                  *)
(*  - Implement a new READ function for integers. This new          *)
(*    function is implemented in Pascal (called $PASRDI) and        *)
(*    is located in the module PASLIBX. It allows the width         *)
(*    parameter, too. The compiler generates calls to $PASRDI       *)
(*    instead of a CSP, when doing an integer READ.                 *)
(*                                                                  *)
(*  - Implement a new procedure $ERROR, which allows the            *)
(*    runtime to throw real exceptions. Up until now, the           *)
(*    runtime issued EXIT calls, which was not ok, because          *)
(*    the stack traces were not triggered in this case.             *)
(*    The new $ERROR procedure generates a P-Code instruction       *)
(*    CHK E, which throws an exception and triggers all the         *)
(*    corrective actions that a "normal" exception does.            *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Dec 2019 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  - Implement READ of Varchars (String of varying length)         *)
(*    new CSP RDV is needed, had to implemented in PCINT and        *)
(*    in the Pascal monitor PASMONN.ASSEMBLE. Read of               *)
(*    fixed strings fills with blanks on shorter lines;             *)
(*    read of varying strings returns true length of the            *)
(*    line.                                                         *)
(*                                                                  *)
(*  - New String functions LEFT, RIGHT and LASTINDEX                *)
(*    LEFT and RIGHT have somehow different semantics               *)
(*    from SUBSTR ...; LEFT (s, n) always returns a string          *)
(*    of length n, even if the original length of s was lower,      *)
(*    same for RIGHT. SUBSTR will show a runtime error in           *)
(*    this case.                                                    *)
(*                                                                  *)
(*  - Allow left justified output of strings using                  *)
(*    negative width specification (modification to                 *)
(*    CSP WRS; this implies changes to the interpreter              *)
(*    PCINT and to the Pascal monitor PASMONN.ASSEMBLE)             *)
(*                                                                  *)
(*    example:                                                      *)
(*                                                                  *)
(*    VAR S: CHAR (8);                                              *)
(*                                                                  *)
(*    WRITE (S : 20);   // output of S width 20 justified right     *)
(*    WRITE (S : -20)   // output of S width 20 justified left      *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Nov 2019 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  - Allow strings as byvalue arguments to CHAR (n) parameters     *)
(*    (was error 418 before). This involves generation of a         *)
(*    VMV P-Code to copy the string into the CHAR (n) byvalue       *)
(*    parameter field. This in turn requires the VMV P-Code         *)
(*    instruction to accept negative lengths; a negative length     *)
(*    tells the interpreter (or PASCAL2) that the order of the      *)
(*    operands on the stack is reversed.                            *)
(*                                                                  *)
(*  - and this triggers a change to PASCAL2 ...                     *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Sep 2019 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  - To allow the compiler to compile the P5 compiler, it          *)
(*    is necessary to allow set expressions [ a .. b ],             *)
(*    where a and b are variable expressions.                       *)
(*                                                                  *)
(*  - This implies the creation of a new P-Code instruction         *)
(*    ASR, which sets a range of elements in a set                  *)
(*    (add set range ... similar to ASE, add set element).          *)
(*    ASR fetches three arguments from the stack: the set           *)
(*    and two elements: the two elements define the range.          *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Aug 2019 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  - New procedure APPEND to open files to append data             *)
(*    (implemented by a new CSP APN)                                *)
(*                                                                  *)
(*  - procedure FIELDLIST reworked completely                       *)
(*    for better analysis and management of variant records         *)
(*    and for more consistent computation of field offsets          *)
(*                                                                  *)
(*  - this is done to prepare the keyword WITH for use              *)
(*    within record definitions ... to add fields of other          *)
(*    records into the current record's namespace; even for         *)
(*    records which are addressed by pointer                        *)
(*                                                                  *)
(*  - currently all fields of all records added via WITH            *)
(*    must have unique names; there is no mechanism to              *)
(*    change field names during insertion via WITH                  *)
(*                                                                  *)
(*  - see some YouTube videos on the WITH topic:                    *)
(*    https://www.youtube.com/watch?v=ZHqFrNyLlpA                   *)
(*    (on data orientation)                                         *)
(*                                                                  *)
(*  - this could also help to make the compiler more secure;        *)
(*    by getting rid of some dangerours variant records and         *)
(*    replace them by records with subrecords which are             *)
(*    addressed by pointers (where only the pointer for             *)
(*    the correct variant is different from nil)                    *)
(*                                                                  *)
(*  - the new WITH keyword allows for only minor source             *)
(*    changes in this case; the changes are limited to the          *)
(*    record (type) definition                                      *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Jun 2019 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  - Labels which are defined but not used in the code now         *)
(*    generate a warning message, not an error (P168)               *)
(*                                                                  *)
(*  - Labels which are defined and used but not referenced by       *)
(*    a goto statement generate a new warning message (P183)        *)
(*                                                                  *)
(*  - Variables and procedures/functions which are not              *)
(*    referenced generate new warning messages (P220 and P221)      *)
(*                                                                  *)
(*  - Types which are not                                           *)
(*    referenced generate new warning messages (P222)               *)
(*                                                                  *)
(*  - Parameters of procedures and functions which are not          *)
(*    referenced generate new warning messages (P223)               *)
(*                                                                  *)
(*  These extensions are published as release 2019.07               *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  May 2019 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  - Code generation errors with builtin functions LENGTH          *)
(*    and MAXLENGTH, when the lengths are known at compile time     *)
(*                                                                  *)
(*  When the LENGTH or MAXLENGTH function was related to an         *)
(*  array element, the compiler generated code for the              *)
(*  addressing of the element, although not needed. What made       *)
(*  things worse: this code left an unneeded item (the address      *)
(*  of the element) on the stack, which was not removed and led     *)
(*  to problems in the PASCAL2 code generation (the PCINT           *)
(*  interpreter doesn't complain, although the memory leak          *)
(*  - or stack leak in this case - is clearly visible in            *)
(*  debug mode).                                                    *)
(*                                                                  *)
(*  The solution found is:                                          *)
(*                                                                  *)
(*  to invalidate the generated code using two new P-Code           *)
(*  instructions XBG and XEN.                                       *)
(*                                                                  *)
(*  XBG <seqno> is generated, when a critical code sequence         *)
(*  starts.                                                         *)
(*                                                                  *)
(*  If later the compiler decides that the code starting from       *)
(*  the last scheduled XBG is not needed, it generates a            *)
(*  XEN <seqno>,0 ... otherwise XEN <seqno>,1                       *)
(*                                                                  *)
(*  It is important that the compiler knows the seqno of the        *)
(*  XBG to write it on the XEN ... and: it should write the         *)
(*  XEN unconditionally, because PASCAL2 and the P-Code             *)
(*  interpreter will look for it (if no XEN for a particular        *)
(*  XBG is found, the code is generated, that is, an                *)
(*  XEN <seqno>,1 is implied).                                      *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  May 2019 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  - Some errors were reported by Ed Lott (thanks)                 *)
(*                                                                  *)
(*    a) when the compiler generated calls to the CSP WRV           *)
(*       (write strings), it did not make sure that the             *)
(*       FCB address was loaded (see PASCAL2, LOADFCBADDRESS).      *)
(*       Corrected 13.05.2019                                       *)
(*                                                                  *)
(*    b) when accessing static strings, the compiler did not        *)
(*       add the offset of the string in the STATIC CSECT           *)
(*       during address computation (in some situations)            *)
(*       Corrected 14.05.2019                                       *)
(*                                                                  *)
(*    c) when accessing the length field of a string,               *)
(*       the compiler did not compute the address correctly         *)
(*       (especially when the string was an array element).         *)
(*       The function GETADR2 must be used in this case.            *)
(*       Corrected 15.05.2019                                       *)
(*                                                                  *)
(*    d) wrong code was generated, when a string array              *)
(*       component was passed to a procedure (again, using          *)
(*       GETADR2 solved the problem).                               *)
(*       Corrected 17.05.2019                                       *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Feb.2019 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  - Fields of builtin record OSPARM renamed to PSTRING and        *)
(*    PLENGTH (instead of STRING and LENGTH) due to name            *)
(*    collision with new builtin type STRING and builtin            *)
(*    function LENGTH (no real problem, but it is much nicer        *)
(*    this way)                                                     *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Jan.2019 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  - Error occured in PCINT.C when CASE Statement had no           *)
(*    case tags at all (empty case). Fixed.                         *)
(*                                                                  *)
(*  - Errors in structured constants involving strings. Fixed.      *)
(*                                                                  *)
(*    Example:                                                      *)
(*                                                                  *)
(*       type TMYRECORD = record                                    *)
(*                          A : INTEGER ;                           *)
(*                          B : STRING ( 5 ) ;                      *)
(*                        end ;                                     *)
(*                                                                  *)
(*       const DEFAULT : TMYRECORD =                                *)
(*             ( 100 , 'foo' ) ;                                    *)
(*                                                                  *)
(*  - No length required (or supported) for STRING constants        *)
(*                                                                  *)
(*  - Supporting typed STRING constants by converting them to       *)
(*    char arrays internally (they are constant and fixed size,     *)
(*    after all)                                                    *)
(*                                                                  *)
(*    Example:                                                      *)
(*                                                                  *)
(*       const X : STRING = 'Oppolzer' ;                            *)
(*                                                                  *)
(*    is much the same as                                           *)
(*                                                                  *)
(*       const X = 'Oppolzer' ;                                     *)
(*                                                                  *)
(*    but                                                           *)
(*                                                                  *)
(*       const X2 : CHAR ( 10 ) = 'Oppolzer' ;                      *)
(*                                                                  *)
(*    is different (8 Bytes Content plus 2 Blanks)                  *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Jul.2018 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  Error fixed: negative constants on CASE threw range errors;     *)
(*  this has been corrected ... see test program TESTCASE.PAS       *)
(*  (discovered while working on module AVLTREE.PAS)                *)
(*                                                                  *)
(*  Allow types with parameters being specified in pointer type     *)
(*  declarations, for example:                                      *)
(*                                                                  *)
(*    var S1 : STRING ( 9 ) ;                                       *)
(*        SP1 : -> STRING ( 9 ) ;                                   *)
(*                                                                  *)
(*  the second declaration (SP1) was not possible before,           *)
(*  because only type identifiers were allowed after the            *)
(*  arrow symbol (no type parameters).                              *)
(*                                                                  *)
(*  Same goes for                                                   *)
(*                                                                  *)
(*    type CP8 = -> CHAR ( 8 ) ;                                    *)
(*                                                                  *)
(*  which is a pointer type declaration; variables of this          *)
(*  type point to variables of type CHAR (8), which is an           *)
(*  abbreviation for ARRAY [ 1 .. 8 ] OF CHAR ; you can also        *)
(*  declare variables directly, like                                *)
(*                                                                  *)
(*    var PV8 : -> CHAR ( 8 ) ;                                     *)
(*                                                                  *)
(*  and then do something like this:                                *)
(*                                                                  *)
(*    PV8 := ALLOC ( 8 ) ;                                          *)
(*    PV8 -> := 'Oppolzer';                                         *)
(*    WRITELN ( PV8 -> ) ;                                          *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Jun.2018 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  MEMCMP added as standard function, similar to MEMCPY.           *)
(*  Two new PCODE instructions added to implement MEMCMP inline     *)
(*  (MCC and MCV)                                                   *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  May 2018 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  The P-Codes for Strings (starting with the letter V)            *)
(*  are now recognized and translated to 370 machine code           *)
(*  by PASCAL2 ... see there                                        *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Feb.2018 - Extensions to the Compiler by Bernd Oppolzer         *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  many tests and improvements with respect to procedure and       *)
(*  function parameters (procedures and functions passed as         *)
(*  parameters to other procedures). The compiler had some bugs     *)
(*  here. In this context I re-activated the 1982 version of the    *)
(*  compiler, to see, if the bugs were there already (they were).   *)
(*                                                                  *)
(*  I was inspired to do this all by some postings regarding        *)
(*  Knuth's Man-or-Boy test, which I didn't know before.            *)
(*                                                                  *)
(*  look here: https://en.wikipedia.org/wiki/Man_or_boy_test        *)
(*                                                                  *)
(*  and here: https://rosettacode.org/wiki/Man_or_boy_test#Pascal   *)
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
(*  e) using this: idlength changed from 12 to 20,                  *)
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
(********************************************************************)
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
(********************************************************************)



const VERSION = '2020.09' ;
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

      IDLENGTH = 20 ;
      ALFALNGTH = 10 ;
      MAXSTRL = 254 ;
      MAXVARCHARSIZE = 32767 ;
      DISPLIMIT = 20 ;
      MAX_BKT = 232 ;

      (******************************************)
      (* HASH TABLE SIZE                        *)
      (******************************************)

      MAXLEVEL = 9 ;
      ORDCHMAX = 255 ;

      (******************************************)
      (* SIZE OF CHAR SET OF TARGET MACHINE     *)
      (******************************************)

      OPMAX = 104 ;

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

      FIRSTGVAR = 400 ;

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
      CIXMAX = 405 ;

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
      PCODE_FJP = 33 ;
      PCODE_IND = 35 ;
      PCODE_IXA = 36 ;
      PCODE_LCA = 37 ;
      PCODE_CTS = 38 ;
      PCODE_CTI = 39 ;
      PCODE_MOV = 40 ;
      PCODE_MST = 41 ;
      PCODE_RET = 42 ;
      PCODE_STP = 43 ;
      PCODE_XJP = 44 ;
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
      PCODE_CHR = 62 ;
      PCODE_DEF = 63 ;
      PCODE_LAB = 64 ;
      PCODE_CRD = 65 ;
      PCODE_XPO = 66 ;
      PCODE_ASE = 67 ;
      PCODE_SLD = 68 ;
      PCODE_SMV = 69 ;
      PCODE_DFC = 70 ;
      PCODE_CST = 71 ;
      PCODE_BGN = 72 ;
      PCODE_UXJ = 73 ;
      PCODE_XLB = 74 ;
      PCODE_END = 75 ;
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
      PCODE_MCC = 97 ;
      PCODE_MCV = 98 ;
      PCODE_ASR = 99 ;
      PCODE_XBG = 100 ;
      PCODE_XEN = 101 ;


type ALPHA = array [ 1 .. IDLENGTH ] of CHAR ;
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
            , SYBREAK , SYCONTINUE , SYRETURN , SYMODULE , SYLOCAL ,
            SYSTATIC , NOTUSED ) ;
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

     CSTCLASS = ( XINT , REEL , PSET , NULLSTR , STRG ) ;

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
                   case CONSTCLASS : CSTCLASS of
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
                     NULLSTR , STRG :
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
     CONSTLIST = record
                   C : XCONSTANT ;
                   NEXT : -> CONSTLIST
                 end ;
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
                             METAOFFS : ADDRRANGE ;
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
                     ( CONFORMANT : BOOLEAN ;
                       DEF_COMPLETE : BOOLEAN ) ;
                   ARRAYS :
                     ( AELTYPE , INXTYPE : TTP ) ;
                   RECORDS :
                     ( FIRSTFIELD : IDP ;
                       RECTAGTYPE : TTP ;
                       NUMBER_OF_FIELDS : 0 .. 1000 ;
                       FLD_DISP_LEV : - 1 .. DISPLIMIT ) ;
                   FILES :
                     ( FILTYPE : TTP ) ;
                   TAGFLD :
                     ( TAGFIELDP : IDP ;
                       FIRSTVARIANT : TTP ;
                       VARIANT_OFFS : ADDRRANGE ) ;
                   VARIANT :
                     ( FIRSTSUBFIELD : IDP ;    // firstfield of rec
                       SUBTAGTYPE : TTP ;       // rectagtype of rec
                       VARVALS : -> CONSTLIST ; // ptr to const lst
                       NEXTVARIANT : TTP )      // ptr to nxt var
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
               , PTIM , PFLR , PTRC , PRND , PWRV , PAPN , PRDV , PRFC
               , PRFS , PRFV , UNDEF_CSP ) ;

     (******************************)
     (* types of parameters        *)
     (******************************)

     IDKIND = ( NORMALVAR , VALUEPARM , VARPARM , CONSTPARM ) ;

     (******************************)
     (* storage classes            *)
     (******************************)

     STORAGE_CLASS = ( XAUTO , XSTATIC ) ;
     WITH_COPY_TYPE = ( WCNONE , WCINCLUDE , WCPOINTER ) ;

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
     (*                                                    *)
     (* some comments on fields (extension 2019):          *)
     (*                                                    *)
     (* fieldaddr: offset of field within fieldlist        *)
     (* owner: points to record type where field is        *)
     (*       part of                                      *)
     (* tagpointer: new in 2019, points to typerec         *)
     (*       of tagfld type iff field in the actual       *)
     (*       fieldlist is part of a set of variants       *)
     (*       (in this case, the variants start at         *)
     (*       a certain offset which will later be         *)
     (*       recorded in the tagfld typerec)              *)
     (* with_copy: tells if this field has been            *)
     (*       entered into the fieldlist using a           *)
     (*       with clause; may be wcnone, wcinclude,       *)
     (*       wcpointer (scalar type)                      *)
     (* pointer_offset: if with_copy = wcpointer, then     *)
     (*       this is the offset where the field is        *)
     (*       located in the referenced structure          *)
     (*       (needs verification ...)                     *)
     (******************************************************)

     IDENTIFIER = record
                    NAME : ALPHA ;
                    IDTYPE : TTP ;
                    NEXT_IN_BKT : IDP ;
                    NEXT : IDP ;
                    PREV_VAR_IN_BLOCK : IDP ;
                    PREV_TYPE_IN_BLOCK : IDP ;
                    PREV_PROC_IN_BLOCK : IDP ;
                    DECL_LEV : LEVRANGE ;
                    case KLASS : IDCLASS of
                      TYPES :
                        ( REFERENCET : CHAR ) ;
                      KONST :
                        ( VALUES : XCONSTANT ) ;
                      STRUCTKONST :
                        ( SKOWNERPROC : EXTNAMTP ;
                          SKADDR : ADDRRANGE ) ;
                      VARS :
                        ( VKIND : IDKIND ;
                          VLEV : LEVRANGE ;
                          STKLASS : STORAGE_CLASS ;
                          REFERENCE : CHAR ;
                          VOWNERPROC : EXTNAMTP ;
                          VADDR : ADDRRANGE ;
                          SPECIAL : INTEGER ;
                          DUMMYVAR : BOOLEAN ;
                          DUMMYLEV : LEVRANGE ;
                          DUMMYADDR : ADDRRANGE ) ;
                      FIELD :
                        ( FIELDADDR : ADDRRANGE ;
                          OWNER : TTP ;
                          TAGPOINTER : TTP ;
                          WITH_COPY : WITH_COPY_TYPE ;
                          POINTER_OFFSET : ADDRRANGE ;
                          INSERTED_BY_WITH : BOOLEAN ) ;
                      PROC , FUNC :
                        ( EXTRN : BOOLEAN ;
                          EXTLANG : CHAR ;
                          EXTNAME : EXTNAMTP ;
                          REFERENCEP : CHAR ;
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
     HASH_COUNT = array [ BKT_RNG ] of INTEGER ;
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
              DEFINED : BOOLEAN ;
              REFERENCED : BOOLEAN
            end ;
     FRECPTR = -> FILEREC ;
     FILEREC = record
                 FILIDPTR : IDP ;
                 NEXTFILE : FRECPTR ;
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

     /******************************************/
     /* Subtypen fuer Scanner-Typen            */
     /******************************************/

     CHAR32 = array [ 1 .. 32 ] of CHAR ;
     CHAR64 = array [ 1 .. 64 ] of CHAR ;
     SOURCELINE = array [ 1 .. MAXLSIZE ] of CHAR ;
     SCAN_ERRCLASS = 'A' .. 'Z' ;
     OPTIONS_PTR = -> COMP_OPTIONS ;

     /******************************************/
     /* Liste der Fehler pro Source-Zeile usw. */
     /******************************************/
     /* muss mit Def. beim Compiler            */
     /* uebereinstimmen                        */
     /******************************************/

     SCANF_PTR = -> SCAN_FEHLER ;
     SCAN_FEHLER = record
                     ERRLEVEL : CHAR ;       // error level
                     ERRCLASS : CHAR ;       // error class
                     NUMMER : INTEGER ;      // error number
                     INFO : CHAR64 ;         // additional info
                     ZEILNR : INTEGER ;      // line number of err
                     POSITION : INTEGER ;    // position of err
                     NAECHST : SCANF_PTR ;   // ptr to next
                     ZEILNR_SKIP : INTEGER ; // line number skip
                     POS_SKIP : INTEGER ;    // position skip
                   end ;

     /***********************************/
     /* zentraler Scan-Block            */
     /***********************************/
     /* muss mit Def. beim Scanner      */
     /* uebereinstimmen                 */
     /***********************************/

     SCAN_BLOCK = record
                    MODUS : INTEGER ;        // modus of scanner
                    DATEIENDE : INTEGER ;    // end of file indicator
                    ENDOFLINE : BOOLEAN ;    // end of line indicator
                    SLINE : SOURCELINE ;     // stored source line
                    LINENR : INTEGER ;       // line number of symbol
                    LINEPOS : INTEGER ;      // line position of symb
                    LINELEN : INTEGER ;      // line length
                    SKIPPING : BOOLEAN ;     // parser is skipping
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
                    FEANFANG : SCANF_PTR ;   // anchor to err list
                    FEAKT : SCANF_PTR ;      // actual err elem
                    FTTAB : ANYPTR ;         // error text table
                    FTTABA : ANYPTR ;        // same for applic.
                    OPTLINE : SOURCELINE ;   // options line
                    POPT : OPTIONS_PTR ;     // ptr to opt struct

     /******************************************/
     /* felder fuer sofortige Protokollausgabe */
     /******************************************/

                    PROTOUT : BOOLEAN ;        // switch for prot out
                    TERMOUT : BOOLEAN ;        // switch for term out
                    FEAKT_ALT : SCANF_PTR ;    // old feakt
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
                      SHOW_LISTDEF : BOOLEAN ; // show listdef
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
    PCODE : TEXT ;
    TRACEF : TEXT ;
    LISTING : TEXT ;
    LISTDEF : TEXT ;
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
    (* SYDIGITS - digits of symbol (if const number)       *)
    (* SYPREC   - precision of symbol (if decimal const)   *)
    (* SYID     - identifier (if symbol was ident)         *)
    (* SYVAL    - constant (if symbol was constant)        *)
    (*******************************************************)

    SY : SYMB ;
    SYLENGTH : INTEGER ;
    SYDIGITS : INTEGER ;
    SYPREC : INTEGER ;
    SYID : ALPHA ;
    SYVAL : XCONSTANT ;

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

    PACKDATA : BOOLEAN ;

    (******************************************)
    (* ASSIGNMENT GOING ON,PACKING IN EFFECT  *)
    (******************************************)

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

    GATTR : ATTR ;

    //****************************************************************
    // gattr - very important - DESCRIBES THE EXPR CURRENTLY COMPILED
    // ctls - watches the allocation of strings in the working area
    //****************************************************************

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
    BUCKET_COUNT : HASH_COUNT ;

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
    (* errlog stores the error types that     *)
    (* have occured during compile            *)
    (* set of errcode = bit mask              *)
    (******************************************)

    ERRLOG : set of ERRCODE ;

    (******************************************)
    (* COMPILATION TIME                       *)
    (******************************************)

    CTIME : INTEGER ;

    (******************************************)
    (* KIND OF ERROR, 'E' / 'W' (WARNING)     *)
    (******************************************)

    ERRKIND : CHAR ;

    (******************************************)
    (* ordinal number of conditional          *)
    (* code sequences, startet by XBG and     *)
    (* ended by XEN Pcode                     *)
    (******************************************)

    XBG_NUMBER : INTEGER ;

    (***********************************************************)
    (* STRUCTURED CONSTANTS, READ-ONLY TABLES                  *)
    (* ********** *********  **** **** ******                  *)
    (***********************************************************)

    xtest : string (100);

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
      WRITEBEGSYS : SYMSET =
      [ SYPLUS , SYMINUS , INTCONST , REALCONST , STRINGCONST , IDENT ,
        SYLPARENT ] ;
      SIMPTYPEBEGSYS : SYMSET =
      [ SYPLUS , SYMINUS , INTDOTDOT , INTCONST , STRINGCONST , IDENT ,
        SYLPARENT ] ;
      TYPEBEGSYS : SYMSET =
      [ SYARROW , SYPACKED , SYARRAY , SYRECORD , SYSET , SYFILE ,
        SYPLUS , SYMINUS , INTDOTDOT , INTCONST , STRINGCONST , IDENT ,
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

      (*********************************************************)
      (*   Symbols as Text                                     *)
      (*********************************************************)

      SYMB_CHAR : array [ SYMB ] of STRING ( 15 ) = //
      ( 'SYMB_EOF' ,                                //
        'SYMB_UNKNOWN' ,                            //
        'EOLCHAR' ,                                 //
        'SEPARATOR' ,                               //
        'COMMENT1' ,                                //
        'COMMENT2' ,                                //
        'COMMENT3' ,                                //
        'COMMENT4' ,                                //
        'COMMENT5' ,                                //
        'STRINGCONST' ,                             //
        'HEXSTRINGCONST' ,                          //
        'BINSTRINGCONST' ,                          //
        'INTCONST' ,                                //
        'INTDOTDOT' ,                               //
        'REALCONST' ,                               //
        'IDENT' ,                                   //
        'LPARENT' ,                                 //
        'RPARENT' ,                                 //
        'LBRACK' ,                                  //
        'RBRACK' ,                                  //
        'COMMA' ,                                   //
        'SEMICOLON' ,                               //
        'ARROW' ,                                   //
        'PERIOD' ,                                  //
        'DOTDOT' ,                                  //
        'COLON' ,                                   //
        'PLUS' ,                                    //
        'MINUS' ,                                   //
        'MULT' ,                                    //
        'SLASH' ,                                   //
        'EQOP' ,                                    //
        'NEOP' ,                                    //
        'GTOP' ,                                    //
        'LTOP' ,                                    //
        'GEOP' ,                                    //
        'LEOP' ,                                    //
        'OROP' ,                                    //
        'ANDOP' ,                                   //
        'ASSIGN' ,                                  //
        'CONCAT' ,                                  //
        'AND' ,                                     //
        'DIV' ,                                     //
        'MOD' ,                                     //
        'OR' ,                                      //
        'XOR' ,                                     //
        'IN' ,                                      //
        'NOT' ,                                     //
        'LABEL' ,                                   //
        'CONST' ,                                   //
        'TYPE' ,                                    //
        'VAR' ,                                     //
        'FUNCTION' ,                                //
        'PROGRAM' ,                                 //
        'PROCEDURE' ,                               //
        'SET' ,                                     //
        'PACKED' ,                                  //
        'ARRAY' ,                                   //
        'RECORD' ,                                  //
        'FILE' ,                                    //
        'FORWARD' ,                                 //
        'BEGIN' ,                                   //
        'IF' ,                                      //
        'CASE' ,                                    //
        'REPEAT' ,                                  //
        'WHILE' ,                                   //
        'FOR' ,                                     //
        'WITH' ,                                    //
        'GOTO' ,                                    //
        'END' ,                                     //
        'ELSE' ,                                    //
        'UNTIL' ,                                   //
        'OF' ,                                      //
        'DO' ,                                      //
        'TO' ,                                      //
        'DOWNTO' ,                                  //
        'THEN' ,                                    //
        'FORTRAN' ,                                 //
        'EXTERNAL' ,                                //
        'OTHERWISE' ,                               //
        'BREAK' ,                                   //
        'CONTINUE' ,                                //
        'RETURN' ,                                  //
        'MODULE' ,                                  //
        'LOCAL' ,                                   //
        'STATIC' ,                                  //
        'NOTUSED' ) ;                               //

      (*********************************************************)
      (*   names of P-Code instructions                        *)
      (*********************************************************)

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
        ' VSM' , ' VLM' , ' VPU' , ' VPO' , ' VIX' , ' VRP' , ' MCC' ,
        ' MCV' , ' ASR' , ' XBG' , ' XEN' , '    ' , '    ' , '    ' )
        ;

      (*********************************************************)
      (*   names of CSPs, index type should be CSPTYPE,        *)
      (*   but it is indexed by ORD (CSP) in GEN1 ...          *)
      (*********************************************************)

      CSPNAME : array [ 0 .. 55 ] of array [ 1 .. 3 ] of CHAR =
      ( 'PAG' , 'GET' , 'PUT' , 'RES' , 'REW' , 'RDC' , 'WRI' , 'WRE' ,
        'WRR' , 'WRC' , 'WRS' , 'WRX' , 'RDB' , 'WRB' , 'RDR' , 'RDH' ,
        'RDY' , 'EOL' , 'EOT' , 'RDD' , 'WRD' , 'CLK' , 'WLN' , 'RLN' ,
        'RDI' , 'EOF' , 'ELN' , 'RDS' , 'TRP' , 'XIT' , 'FDF' , 'SIO' ,
        'EIO' , 'MSG' , 'SKP' , 'LIM' , 'TRA' , 'WRP' , 'CLS' , 'DAT' ,
        'TIM' , 'FLR' , 'TRC' , 'RND' , 'WRV' , 'APN' , 'RDV' , 'RFC' ,
        'RFS' , 'RFV' , '   ' , '   ' , '   ' , '   ' , '   ' , '   ' )
        ;

      (*********************************************************)
      (*-------------------------------------------------------*)
      (*********************************************************)




procedure SKIP_SYMBOL ( FSYS : SYMSET ) ;

(*****************************************************)
(*   SKIP INPUT STRING UNTIL RELEVANT SYMBOL FOUND   *)
(*****************************************************)


   var PFDUMMY : SCANF_PTR ;
       S : SYMB ;
       X : STRING ( 64 ) ;
       SKIPINFO : CHAR64 ;
       SSTR : STRING ( 20 ) ;

   begin (* SKIP_SYMBOL *)
     if not ( SY in FSYS ) then
       begin
         X := '' ;
         for S := SYMB_EOF to NOTUSED do
           if S in FSYS then
             begin
               SSTR := SYMB_CHAR [ S ] ;
               if LENGTH ( X ) + LENGTH ( SSTR ) + 1 >= 60 then
                 begin
                   X := X || ',...' ;
                   break
                 end (* then *)
               else
                 X := X || ',' || SSTR ;
             end (* then *) ;
       end (* then *) ;
   end (* SKIP_SYMBOL *) ;



procedure WRITEDFC ( ELSP1 : TTP ; ELSIZE : INTEGER ; LVALU : XCONSTANT
                   ; INIT : BOOLEAN ) ;

   var CH : CHAR ;

   begin (* WRITEDFC *)
     WRITE ( PCODE , CONSTLCOUNTER : 1 , MN [ PCODE_DFC ] ) ;
   end (* WRITEDFC *) ;



begin (* HAUPTPROGRAMM *)
   xtest :=  rw [12] || symb_char [ident ];
   xtest :=  rw [12] || symb_char [ident ] || mn  [57];
   writeln (xtest);
end (* HAUPTPROGRAMM *) .
