{$IFNDEF FPC}
(*$c+,t-,d-,l-,s+*)
{$ENDIF}
{*******************************************************************************
*                                                                              *
*                     Portable Pascal assembler/interpreter                    *
*                     *************************************                    *
*                                                                              *
*                                 Pascal P6                                    *
*                             Based on Pascal P5                               *
*                                                                              *
*                       Viridian Development Corporation                       *
*                       University Park, MD, August 19 2013                    *
*                                                                              *
*   Special Note: Because P6 expands the feature set, it cannot be compiled    *
*   with the P5 compiler, it requires itself to compile. This was fixed by     *
*   compiling with Free Pascal to create an actual compiler, then once that    *
*   was built, compiling it with itself.                                       *
*                                                                              * 
*                                                                              *
*   Impelementation History                                                    *
*                                                                              *
*   --------------------------------   P4   -------------------------------    *
*                                 ETH May 76                                   *
*                                                                              *
* Authors:                                                                     *
*    Urs Ammann                                                                *
*    Kesav Nori                                                                *
*    Christian Jacobi                                                          *
*    K. Jensen                                                                 *
*    N. Wirth                                                                  *
*                                                                              *
*    Address:                                                                  *
*       Institut Fuer Informatik                                               *
*       Eidg. Technische Hochschule                                            *
*       CH-8096 Zuerich                                                        *
*                                                                              *
*  This code is fully documented in the book                                   *
*        "Pascal Implementation"                                               *
*   by Steven Pemberton and Martin Daniels                                     *
* published by Ellis Horwood, Chichester, UK                                   *
*         ISBN: 0-13-653-0311                                                  *
*       (also available in Japanese)                                           *
*                                                                              *
* Steven Pemberton, CWI/AA,                                                    *
* Kruislaan 413, 1098 SJ Amsterdam, NL                                         *
* Steven.Pemberton@cwi.nl                                                      *
*                                                                              *
*   --------------------------------   P5   -------------------------------    *
* Adaption from P4 to P5 by:                                                   *
*                                                                              *
*    Scott A. Moore                                                            *
*    samiam@moorecad.com                                                       *
*                                                                              *
*    The comments marked with brackets are mine [sam]                          *
*                                                                              *
* P5 is an extended version of P4 with the following goals:                    *
*                                                                              *
* 1. The remaining unimplemented functions of Pascal are implemented, so that  *
*    P5 is no longer a "subset" of full Pascal. This was done because it is    *
*    no longer necessary to produce a minimum size implementation, and it      *
*    allows any standard program to be used with P5.                           *
*                                                                              *
* 2. The P5 compiler is brought up to ISO 7185 level 0 standards, both in the  *
*    language it compiles for, and the language it is implemented in.          *
*                                                                              *
* 3. The internal storage efficiency is increased. For example, character      *
*    strings no longer take as much space per character as integers and other  *
*    data. Sets are placed in their own space so that the minimum stack size   *
*    not determined by set size.                                               *
*                                                                              *
* 4. The remaining limitations and dependencies on the CDC 6000 version are    *
*    removed. For example, the instruction store no longer is packed 2         *
*    instructions to a 60 bit word.                                            *
*                                                                              *
* 5. General clean up. Longstanding bugs and issues are addressed. Constants   *
*    that were buried in the source (magic numbers) were made constants. The   *
*    type 'alpha' (specific to CDC 6000) was replaced with idstr, etc.         *
*                                                                              *
* The idea of P5 is to obtain a compiler that is ISO 7185 compliant, can       *
* compile itself, can compile any reasonable standard program, and is          *
* efficient enough to be used as a normal compiler for some certain uses.      *
* Finally, it can serve as a starting implementation for native compilers.     *
*                                                                              *
* P5 machine instructions added:                                               *
*                                                                              *
* rnd    round:   expects a float on stack, performs round() and places the    *
*                 result back on the stack as an integer.                      *
*                                                                              *
* pck ln pack:    Expects a packed array on stack top, followed by the         *
*                 starting subscript, then the unpacked array. The parameter   *
*                 contains the length of packed array in elements. Performs    *
*                 pack(upa, ss, pa) and removes all from stack. The starting   *
*                 subscript is zero based and scaled to the element size.      *
*                                                                              *
* upk ln pack:    Expects the starting subscript on stack top, followed by the *
*                 unpacked array, then the packed array. The parameter         *
*                 contains the length of packed array in elements. Performs    *
*                 unpack(pa, upa, ss) and removes all from stack. The starting *
*                 subscript is zero based and scaled to the element size.      *
*                                                                              *
* rgs    set rng: Expects a set range specification on stack, with the last    *
*                 value on the top, and the first value next. The two values   *
*                 are replaced with a set with all of the values between and   *
*                 including the first and last values.                         *
*                                                                              *
* fbv ad buf val: Validates a file buffer variable. Expects a file address on  *
*                 stack. The buffer is "validated" for lazy I/O, which means   *
*                 that if the associated file is in read mode, the delayed     *
*                 read to the buffer variable occurs. The file address remains *
*                 on the stack.                                                *
*                                                                              *
* ipj v l ip jmp: Interprocedure jump. Contains the level of the target        *
*                 procedure, and the label to jump to. The stack is adjusted   *
*                 to remove all nested procedures/functions, then the label is *
*                 unconditionally jumped to.                                   *        
*                                                                              *
* cip p           Call indirect procedure/function. The top of stack has the   *
*                 address of a mp/address pair pushed by lpa. The dl of the    *
*                 current mark is replaced by the mp, and the address replaces *
*                 the current pc. The mp/ad address is removed from stack.     *
*                                                                              *
* lpa p l q       Load procedure address. The current mark pointer is loaded   *
*                 onto the stack, followed by the target procedure or function *
*                 address. This puts enough information on the stack to call   *
*                 it with the callers environment.                             *
*                                                                              *
* lip p q         load procedure function address. Loads a mark/address pair   *
*                 for a procedure or function parameter onto the stack. Used   *
*                 to pass a procedure or function parameter to another         *
*                 procedure or function.                                       *
*                                                                              *
* efb    eof:     Find eof for binary file. The top of stack is a logical file *
*                 number. The eof boolean vale replaces it.                    *
*                                                                              *
* fvb ad buf val: Expects the length of the file buffer on stack, and the file *
*                 address under that. The buffer is "validated" for lazy I/O,  *
*                 which means that if the associated file is in read mode, the *
*                 delayed read to the buffer variable occurs. The buffer       *
*                 length is removed only.                                      *
*                                                                              *
* dmp q           Subtracts the value from the stack top. Used to dump the top *
*                 of the stack.                                                *
*                                                                              *
* swp q           Pulls the second on stack to the top, swapping the top to    *
*                 elements. The size of the second on stack is specified, but  *
*                 the top of the on stack is implied as a pointer.             *
*                                                                              *
* tjp q           Expects a boolean on stack. Jumps to the address if the      *
*                 value is true. Removes the value from the stack.             *
*                                                                              *
* P5 machine built in procedures/functions added:                              *
*                                                                              *
* pag    page:    Expects a logical file number on stack top. Performs page(). *
*                                                                              *
* rsf    reset:   Expects a logical file number on stack top. Performs         *
*                 reset() and sets the file to text mode.                      *
*                                                                              *
* rwf    rewrite: Expects a logical file number on stack top. Performs         *
*                 reset() and sets the file to text mode.                      *
*                                                                              *
* wrb    write:   Expects a field number on stack top, followed by a boolean   *
*                 to print, then the logical file number. The boolean is       *
*                 output as per ISO 7185.                                      *
*                                                                              *
* rgs    set rng: Expects a set range specification on stack, with the last    *
*                 value on the top, and the first value next. The two values   *
*                 are replaced with a set with all of the values between and   *
*                 including the first and last values.                         *
*                                                                              *
* wrf    write:   Expects a logical file number on stack top, followed by a    *
*                 field number, then a fraction, then a real to print. The     *
*                 real is output in r:f:f (fraction) format. All but the file  *
*                 are removed from stack.                                      *
*                                                                              *
* wbf    write:   Expects a file address on stack top, followed by the length  *
*                 of the type to write, then the variable address to write     *
*                 from. Writes binary store to the file.                       *
*                                                                              *
* wbi    write:   Expects a file address on stack top, followed by an integer. *
*                 Writes the integer to the file in binary format.             *
*                                                                              *
* wbr    write:   Expects a file address on stack top, followed by a real.     *
*                 Writes the real to the file in binary format.                *
*                                                                              *
* wbc    write:   Expects a file address on stack top, followed by a           *
*                 character. Writes the character to the file in binary        *
*                 format.                                                      *
*                                                                              *
* wbb    write:   Expects a file address on stack top, followed by a boolean.  *
*                 Writes the boolean to the file in binary format.             *
*                                                                              *
* rbf    read:    Expects a file address on stack top, followed by the length  *
*                 of the type to read, then the variable address to read       *
*                 from. Reads binary store from the file.                      *
*                                                                              *
* rsb    reset:   Expects a logical file number on stack top. Performs         *
*                 reset() and sets the file to binary mode.                    *
*                                                                              *
* rwb    rewrite: Expects a logical file number on stack top. Performs         *
*                 reset() and sets the file to binary mode.                    *
*                                                                              *
* gbf    get:     Get file binary. Expects the length of a file element on     *
*                 stack top, followed by a pointer to the file. The next file  *
*                 element is loaded to the file buffer.                        *
*                                                                              *
* pbf    put:     Put file binary. Expects the length of a file element on     *
*                 stack top, followed by a pointer to the file. Writes the     *
*                 file buffer to thr file.                                     *
*                                                                              *
* Note that the previous version of P4 added some type specified instructions  *
* that used to be unified, typeless instructions.                              *
*                                                                              *
* P5 errors added:                                                             *
*                                                                              *
* 182 identifier too long                                                      *
* 183 For index variable must be local to this block                           *
* 184 Interprocedure goto does not reference outter block of destination       *
* 185 Goto references deeper nested statement                                  *
* 186 Label referenced by goto at lesser statement level                       *
* 187 Goto references label in different nested statement                      *
* 188 Label referenced by goto in different nested statement                   *
* 189 Parameter lists of formal and actual parameters not congruous.           *
*                                                                              *
* P5 instructions modified:                                                    *
*                                                                              *
* lca'string'       '                                                          *
*                                                                              *
* was changed to                                                               *
*                                                                              *
* lca 'string'''                                                               *
*                                                                              *
* That is, lca has a space before the opening quote, no longer pads to the     *
* right, and represents single quotes with a quote image. pint converts quote  *
* images back to single quotes, and pads out strings to their full length.     *
*                                                                              *
* In addition, the way files work was extensively modified. Original P5 could  *
* not represent files as full1y expressed variables, such as within an array   *
* or record, and were effectively treated as constants. To treat them as true  *
* variable accesses, the stacking order of the file in all file subroutines    *
* was changed so that the file is on the bottom. This matches the source       *
* order of the file in write(f, ...) or read(f, ...). Also, the file           *
* operations now leave the file on the stack for the duration of a write or    *
* read, then dump them using a specific new instruction "dmp". This allows     *
* multiparameter writes and reads to be effectively a chain of single          *
* operations using one file reference. Finally, files were tied to the type    *
* ending 'a', because files are now full variable references.                  *
*                                                                              *
*   --------------------------------   P6   -------------------------------    *
*                                                                              *
*                                 And now, P6                                  *
*                  Adapted from P5 to P6,  August 19-20, 2013,                 *
*                 Paul Robinson, Viridian Development Corporation              *
*                                                                              *
* If I haven't made any mistakes, this can be compiled with a standard         *
* Pascal compiler with some minor additions.  Set option $R+ to enable         *
* P6 features including generation of 'real' machine code as opposed           *
* to the P5 codes.                                                             *
*                                                                              *
* Will now generate actual code for a real machine which can be assembled, as  *
* opposed to generating fictional machine code for a ficticious (the P5)       *
* machine. This should allow the compiler to (if the target machine code       *
* is changed) compile itself to run on a new machine. This will be indicated   *
* where done.                                                                  *
*                                                                              *
* I still want P6 to be portable, I just want it to be able to do some actual  *
* work. Now, how do we get that? Have a compiler variable, if set, by using a  *
* compiler option, it creates "true" assembly code for the target machine, if  *
* not set, it generates the code for the P5 machine. I think this is a good    *
* compromise, if your machine is not the one I'm targeting for, you can still  *
* have it build for the P5 interpreter and run it there, otherwise it can be   *
* used to create code for a specific machine (or yours if you change the       *
* generated code).                                                             *
*                                                                              *
* Some very bad, very sloppy practices were done here because, I guess, on     *
* account of need to keep the code smaller. A lot of the scan routines are     *
* intimately tied to the code generator for P5. Code generator should have     *
* been separated from the scanner. This would have made porting the compiler   *
* a lot easier since it's not 'locked' to the P5 fictional machine code and    *
* its format. It shouldn't have been done that way, but it was, and it's too   *
* bad it was.                                                                  *
*                                                                              *
* This also means the compiler has probably LOTS of side effects, as routines  *
* look at variables and data structures created by other procedures, and       *
* modify stuff constructed elsewhere. This makes it difficult to debug         *
* as well. Also, if I promote a structure, record or linked list to a higher   *
* level such as if I move it out of a procedure and into the main line, is it  *
* going to still work?                                                         *
*                                                                              *
* New features                                                                 *
* 1. *Lots* of code cleanup.                                                   *
*    Whenever possible, manifest constants have been replaced by real ones.    *
*    this makes the use of these constants documenting instead of being some   *
*    magic value. Error messages and P5 instructions especially. Some of the   *
*    code has been reformatted to make it easier to read. This is also         *
*    important for converting the code to EBCDIC if needed for the IBM S 370   *
*    or potentially to Unicode.                                                *
*    I want to simply bring this code up to where it provides some good        *
*    functionality, so, what I decided to do was move all the P5 code          *
*    generation together and separate it from the rest of the application's    *
*    code, so that at some point when it is no longer necessary it can be      *
*    removed. Right now, it provides significant information on what is        *
*    going on in the program and is useful. But, since we want the program     *
*    to be useful for other environments, we need to make it easier for it     *
*    to generate code for those environments, and to segregate out the P5      *
*    processor code, the way you'd separate the processor code if you were     *
*    converting an existing compiler to generate code for a different          *
*    machine or architecture.                                                  *
* 2. New compiler options (Subject to removal)                                 *
*    $R+ - Enable P6 features including real code generation                   *
*    $S+ - Run a spinner; sometimes this program is really slow, so this       *
*          causes it to display its line number as it's compiling              *
* 3. Some names changed                                                        *
*    Operator changed to Operators because of the potential eventually         *
*                  to redefine certain operators                               *
*    string changed to FixedString - P5 implemented its own string system      *
*    function string changed to isString                                       *
* 4. New coding that makes P6 incompatible with p5                             *
*    * Required adding 'paramstr' which is the equivalent of C's argc array    *
*      so that we can pick up the command line. It can be inserted here to     *
*      remove it where the compiler you compile this with doesn't support it.  *
*    * adding the new instructions for the target machine so that the          *
*      compiler does 'real work' and is not simply creating a target for a     *
*      p-machine wannabe or weak clone of the Java Virtual Machine. If I've    *
*      done this correctly, the compiler should be retargetable for more       *
*      than one machine and to do so consistently.                             *
* 5. New features (the compiler may not use them)                              *
*    * _ (underline) is valid in an identifier the same way a letter is        *
*    * Free Pascal requires a forward declaration to repeat all of the         *
*      arguments to the procedure or function; P5 strictly forbids it. We will *
*      probably allow restatement.                                             *
*      In other words                                                          *
*          Procedure X(A:Integer); forward;                                    *
*      must be secondary declared in P5 as                                     *
*          Procedure X;                                                        *
*      while FPC requires the header be repeated as used, P5 forbids it        *
*      For right now this is being done with a $DEFINE statement, eventually   *
*      it will come out altogether.                                            *
*    * Eliminate the PROGRAM statement requirement, if PROGRAM is not          *
*      used a default name will be inserted (we will probably insert a default *
*      name anyway for Run-time Library support. Not sure how the Pascal       *
*      run-time library - we will require one - will be invoked, either the    *
*      program calls it, or the module references it and the RTL does a        *
*      callback to the default program name. The Run-Time library will provide *
*      support for the calls that the Interpreter provided, writeln, reset,    *
*      new, and other routines.                                                *
*                                                                              *
*                                                                              *
*                                                                              *
*                                                                              *
*******************************************************************************}

program p6(input,output,prr);

label 99; { terminate immediately }

const 

   {

   Program object sizes and characteristics, sync with pint. These define
   the machine specific characteristics of the target. 
   
   This configuration is for a 32 bit machine as follows:

   integer               32  bits
   real                  64  bits
   char                  8   bits
   boolean               8   bits
   set                   256 bits
   pointers              32  bits
   marks                 32  bits
   File logical number   8   bits

   Both endian types are supported. There is no alignment needed, but you
   may wish to use alignment to tune the runtime speed.

   The machine characteristics dependent on byte accessable machines. This
   table is all you should need to adapt to any byte addressable machine.

   }

   intsize     =        4;  { size of integer }
   intal       =        4;  { memory alignment of integer }
   realsize    =        8;  { size of real }
   realal      =        4;  { memory alignment of real }
   charsize    =        1;  { size of char }
   charal      =        1;  { memory alignment of char }
   charmax     =        1;
   boolsize    =        1;  { size of boolean }
   boolal      =        1;  { alignment of boolean }
   ptrsize     =        4;  { size of pointer }
   adrsize     =        4;  { size of address }
   adral       =        4;  { alignment of address }
   setsize     =       32;  { size of set }
   setal       =        1;  { alignment of set }
   filesize    =        1;  { required runtime space for file (lfn) }
   fileidsize  =        1;  { size of the lfn only }
   stackal     =        4;  { alignment of stack }
   stackelsize =        4;  { stack element size }
   maxsize     =       32;  { this is the largest type that can be on the stack }
   heapal      =        4;  { alignment for each heap arena }
   sethigh     =      255;  { Sets are 256 values }
   setlow      =        0;
   ordmaxchar  =      255;  { Characters are 8 bit ISO/IEC 8859-1 }
   ordminchar  =        0;
   maxresult   = realsize;  { maximum size of function result }
   marksize    =       32;  { maxresult+6*ptrsize }
   { Value of nil is 1 because this allows checks for pointers that were
     initialized, which would be zero (since we clear all space to zero).
     In the new unified code/data space scheme, 0 and 1 are always invalid
     addresses, since the startup code is at least that long. }
   nilval      =        1;  { value of 'nil' }

   { end of pcom and pint common parameters }

   displimit   = 300;
   maxlevel    = 255;
   { strglgth used to define the size of all strings in pcom and pint. With the
     string quanta system, string lengths are effectively unlimited, but there
     it still sets the size of some buffers in pcom. }
   strglgth    = 250;
   { maximum number of digits in real, including sign and exponent }
   digmax      = 250;
   { lcaftermarkstack is a very pcom specific way of stating the size of a mark
     in pint. However, it is used frequently in Perberton's documentation, so I
     left it, but equated it to the more portable marksize. }
   lcaftermarkstack = marksize;
   fileal      = charal;
   (* stackelsize = minimum size for 1 stackelement
                  = k*stackal
      stackal     = scm(all other al-constants)
      charmax     = scm(charsize,charal)
                    scm = smallest common multiple
      lcaftermarkstack >= maxresult+3*ptrsize+max(x-size)
                        = k1*stackelsize          *)
   maxstack   =       1;
   parmal     = stackal;
   parmsize   = stackelsize;
   recal      = stackal;
   filebuffer =       4; { number of system defined files }
   maxaddr    =  maxint;
   maxsp      = 39;  { number of standard procedures/functions }
   maxins     = 74;  { maximum number of instructions }
   maxids     = 250; { maximum characters in id string (basically, a full line) }
   maxstd     = 39;  { number of standard identifiers }
   maxres     = 35;  { number of reserved words }
   reslen     = 9;   { maximum length of reserved words }
   varsqt     = 10;  { variable string quanta }
   prtlln     = 10;  { number of label characters to print in dumps }

   { default field sizes for write }
   intdeff    = 11; { default field length for integer }
   reldeff    = 22; { default field length for real }
   chrdeff    = 1;  { default field length for char (usually 1) }
   boldeff    = 5;  { default field length for boolean (usually 5 for 'false' }

   { debug flags }

   dodmplex   = false; { dump lexical }
   doprtryc   = false; { dump recycling tracker counts }

   { version numbers }

   majorver   = 1; { major version number }
   minorver   = 3; { minor version number - Bumped by 1, PR 2013-08-19}
   
   
   { Added PR 2013-08-19 to clean up code a bit by }
   { removing manifest constants}
   
   { Comment field }
   p5comment  = 'i'; {Comment at start of generated lines for p5 output }
   p6comment  = '*'; {Comment at start of real code generated for P6 }


   { Quoted single characters }
   SymSpace   = ' '; {Get rid of single blank as manifest const.}
   SymDollar  = '$'; {Same for use of '$'}
   SymEqual   = '=';
   SymQuote   = '''';
   SymPlus    = '+';
   SymMinus   = '-';
   SymStar    = '*';
   SymSlash   = '/';
   SymOpenPar = '(';
   SymClosePar= ')';
   SymComma   = ',';
   SymPeriod  = '.';
   SymOpenBracket ='[';
   SymCloseBracket=']';
   SymColon   = ':';
   SymSemiColon = ';';
   SymLessThan = '<';
   SymGreaterThan = '>';
   SymOpenComment = '{';
   SymCloseComment= '}';
   SymAt       = '@';
   SymUpArrow = '^';
   SymHashTag = '#';
   SymAmpersand = '&';
   SymPercent = '%';
   SymUnderscore = '_';


   {Standard Constants}
   STDFalse    =   1;   {FALSE}
   STDTrue     =   2;   {TRUE}
   STDInput    =   3;   {Input}
   STDOutput   =   4;   {Output}
   
   STDPrd      =  33;   {PRD}
   STDPrr      =  34;   {PRR}

   {Name the instructions so we stop using manifest constants. 
    Using MCs makes the coding 'magic' instead of science

    At this point I'm not sure whether to eliminate the standard P5
    instruction set. Until I know what the compiler does, I'm leaving them
    in as documentation.

    }

(*   ------------------- P5 Code identifiers ----------------------------- *)

   
   {These are taken from the Interpreter. We'll use a sort of 
    macro call if we don't have our own code to do something
    similar. As I figure out what each instruction does, I will
    document it here.}
   
   PINSTabi =  0 ; {ABS Int}
   PINSTabr =  1 ; {ABS real}
   PINSTadi =  2 ; {Add int}
   PINSTadr =  3 ; {Add real}
   PINSTand =  4 ; {AND}
   PINSTdif =  5 ; {dif}
   PINSTdvi =  6 ; {divide int}
   PINSTdvr =  7 ; {divide real}
   PINSTeof =  8 ; {test for EOF}
   PINSTflo =  9 ;
   PINSTflt = 10 ;
   PINSTinn = 11 ;
   PINSTint = 12 ;
   PINSTior = 13 ;
   PINSTmod = 14 ;
   PINSTmpi = 15 ;  { Multiply Integer }
   PINSTmpr = 16 ;
   PINSTngi = 17 ;
   PINSTngr = 18 ;
   PINSTnot = 19 ;
   PINSTodd = 20 ;
   PINSTsbi = 21 ;
   PINSTsbr = 22 ;
   PINSTsgs=23;
   PINSTsqi=24;
   PINSTsqr=25;
   PINSTsto=26;
   PINSTtrc=27;
   PINSTuni=28;
   PINSTstp=29;
   PINSTcsp=30;
   PINSTdec=31;
   PINSTent=32;
   PINSTfjp=33;
   PINSTinc=34;
   PINSTind=35;
   PINSTixa=36;
   PINSTlao=37;
   PINSTlca=38;
   PINSTldo=39;
   PINSTmov=40;
   PINSTmst=41;
   PINSTret = 42 ; { Return from proc/func }
   PINSTsro=43;
   PINSTxjp=44;
   PINSTchk=45;
   PINSTcup=46;
   PINSTequ=47;
   PINSTgeq=48;
   PINSTgrt=49;
   PINSTlda=50;
   PINSTldc = 51;  {Load Constant}
   PINSTleq=52;
   PINSTles=53;
   PINSTlod=54;
   PINSTneq=55;
   PINSTstr = 56;  {Store}
   PINSTujp=57;
   PINSTord=58;
   PINSTchr=59;
   PINSTujc    = 60;  (*CASE statement, error in case*)
   PINSTrnd    = 61;  (*round:   expects a float on stack, performs round()
                        and places the result back on the stack as 
				an integer. *)
   PINSTpck    = 62;  (*ln pack:    Expects a packed array on stack top, 
                        followed by the starting subscript, then the 
			unpacked array. The parameter contains the
			length of packed array in elements. Performs
                        pack(upa, ss, pa) and removes all from stack.
			The starting subscript is zero based and
			scaled to the element size.      *)
   PINSTupk    =  63;    (* in pack:    Expects the starting subscript on 
                        stack top, followed by the unpacked array, then 
			the packed array. The parameter contains the
			length of packed array in elements. Performs
			unpack(pa, upa, ss) and removes all from stack.
			The starting subscript is zero based and scaled
			to the element size.      *)
   PINSTrgs     = 64;     (* set rng: Expects a set range specification 
                         on stack, with the last value on the top, 
			 and the first value next. The two values are
			 replaced with a set with all of the values
			 between and including the first and last
			 values.                  *)
   PINSTfbv     = 65;  {Validates a file buffer variable. Expects a 
                       file address on stack. The buffer is "validated" 
			   for lazy I/O, which means that if the associated
			   file is in read mode, the delayed read to the
			   buffer variable occurs. The file address
			   remains on the stack.}
   PINSTipj     = 66;     (* v l ip jmp: Interprocedure jump. Contains 
                         the level of the target procedure, and the 
			 label to jump to. The stack is adjusted to
			 remove all nested procedures/functions,
			 then the label is unconditionally jumped to.*)
   PINSTcip     = 67;   (*  p  Call indirect procedure/function. The top 
                         of stack has the address of a mp/address pair 
				 pushed by lpa. The dl of the current mark is
				 replaced by the mp, and the address replaces
				 the current pc. The mp/ad address is removed
				 from stack.     *)
   PINSTlpa     = 68;    (* p l q   Load procedure address. The 
			 current mark pointer is loaded onto the stack,
			 followed by the target procedure or function
			 address. This puts enough information on the
			 stack to call it with the callers environment. *)
   PINSTefb     = 69;   (* eof:   Find eof for binary file. The top of 
                         stack is a logical file number. The eof boolean 
						 value replaces it.                    *)
   PINSTfvb     = 70;   (* Expects the length of the file buffer on 
                        stack, and the file address under that. The 
			buffer is "validated" for lazy I/O, which means
			that if the associated file is in read mode, the
                        delayed read to the buffer variable occurs. 
				The buffer length is removed only.*)
   PINSTdmp     = 71;   (* q   Subtracts the value from the stack top. 
                         Used to dump the top of the stack. *)
   PINSTswp     = 72;   (* q   Pulls the second on stack to the top, 
                         swapping the top two elements. The size of the 
			 second on stack is specified, but the top of the
			 on stack is implied as a pointer.             *)
   PINSTtjp     = 73;   (* q   Expects a boolean on stack. Jumps to the 
                         address if the value is true. Removes the value
                         from the stack.  *)
   PINSTlip     = 74;    (* p q   load procedure function address. Loads a 
                         mark/address pair for a procedure or function 
			 parameter onto the stack. Used to pass a procedure
			 or function parameter to another procedure
			 or function.                               *)
   
   
  
   trueins    = 1;   { Maximum number of 'real' instructions }
   
   {ERROR NUMBERS are expressed here instead of as manifest constants}
   ErrSympType  =   1;    {Error in Symple Type}
   ErrIDExpect  =   2;    {Identifier Expected}
   ErrClosePar  =   4;    { ) expected } 
   ErrColon     =   5;    { : expected }
   ErrIllSym    =   6;    {Illegal Symbol}
   ErrNoOf      =   8;    { of expected }
   ErrOpenPar   =   9;    { ( expected }
   ErrType      =  10;    {Error in Type}
   ErrOpnBrack  =  11;    { [ expected }	
   ErrClsBrack  =  12;    { ] expected }
   ErrNoEnd     =  13;    {END expected}
   ErrNoSemi    =  14;    { ; expected}
   ErrIntExpect =  15;    {Integer expected}
   ErrEqualExpect= 16;    { = expected}
   ErrNoBegin   =  17;    {Begin Expected}
   ErrDeclare   =  18;    {Error in Declaration}
   ErrFieldL    =  19;    {Error in Field List}
   ErrConst     =  50;    {Error in constant}
   ErrBecomes   =  51;    {:= Expected}
   ErrTwice     = 101;    {Declared twice}
   ErrLowAboveHi= 102;    {Low bound > hi bound}
   ErrClass     = 103;    {Wrong class}
   ErrNotThere  = 104;    {Not Declared}
   ErrSign      = 105;    {Sign Not Allowed}
   ErrNumExpect = 106;    {Number expected}
   ErrIncompSub = 107;    {Incompatible subrange}
   ErrRealForbid= 109;    {Real not allowed}
   ErrBadTagFld = 110;    {Tagfield must be scalar or subrange}
   ErrIncTagFld = 111;    {Incompatible with tagfield type}
   ErrIdxNoScal = 113;    {Index must be scalar or subrange}
   ErrBaseReal  = 114;    {Base type must not be real}
   ErrBaseNoScal= 115;    {Base type must be scalar or subrange}
   ErrStdProcParm=116;    {Bad std proc parameter}
   ErrUnsatFwd  = 117;    {Unsatisfied Forward}
   ErrIllOp     = 134;    {Illegal Operand}
   ErrSubScal   = 148;    {Subrange must be scalar}
   ErrIndex     = 149;    {Index type must not be Integer}
   ErrMultiDecl = 166;    {Label multiply declared}
   ErrBaseSet   = 169;    {Error in base set}
   ERRMdefRecVar= 178;    {Multidefined record variant}
   ErrIDTooLong = 182;    {Identifier Too Long}
   ErrNestFile  = 190;    {Files cannot be components of files}
   ErrRealDigEx = 201;    {Error in real digit expected}
   ErrStrTooLng = 202;    {String too long}
   ErrIntConExR = 203;    {Integer Constant Exceeds Range}
   ErrBadOctal  = 204;    {8 or 9 in Octal String - P5 did not use }
                          { this error }
   ErrNoZeroStr = 205;    {Zero String not allowed}
   ErrNotImp    = 399;    {Not Implemented}

   ErrInternal1 = 400;    {Internal Error}
   
   ErrInternal3 = 501;    {Internal Error}
   

   {END UPDATE}
   
   
   
   

type                                                        (*describing:*)
                                                            (*************)

     {marktype= ^integer;}
                                                            (*basic symbols*)
                                                            (***************)

     symbol = (ident,intconst,realconst,stringconst,notsy,mulop,addop,relop,
               lparent,rparent,lbrack,rbrack,comma,semicolon,period,arrow,
               colon,becomes,range,labelsy,constsy,typesy,varsy,funcsy,progsy,
               procsy,setsy,packedsy,arraysy,recordsy,filesy,beginsy,ifsy,
               casesy,repeatsy,whilesy,forsy,withsy,gotosy,endsy,elsesy,untilsy,
               ofsy,dosy,tosy,downtosy,thensy,nilsy,othersy);
     Operators = (mul,rdiv,andop,idiv,imod,plus,minus,orop,ltop,leop,geop,gtop,
                 neop,eqop,inop,noop);
     setofsys = set of symbol;
     chtp = (letter,number,special,illegal,
             chstrquo,chcolon,chperiod,chlt,chgt,chlparen,chspace,chlcmt);
     { Here is the variable length string containment to save on space.
       strings are only stored in their length rounded to the nearest 10th. }
     strvsp = ^strvs; { pointer to variable length id string }
     strvs = record { id string variable length }
                 str:   packed array [1..varsqt] of char; { data contained }
                 next:  strvsp { next }
               end;

                                                            (*constants*)
                                                            (***********)
     setty = set of setlow..sethigh;
     cstclass = (reel,pset,strg);
     csp = ^ constant;
     constant = record  { A constant }
                       next: csp; { next entry link }
                       case cclass: cstclass of {class of constant}
                         reel: (rval: strvsp);  { real   }
                         pset: (pval: setty);   { set    }
                         strg: (                { string }
                            slgth: 0..strglgth; { length of string }
                            sval: strvsp        { pointer to string }
                            )
                       end;

     valu = record case intval: boolean of  (*intval never set nor tested*)
                     true:  (ival: integer);
                     false: (valp: csp)         { Pointer to constant }
                   end;

                                                           (*data structures*)
                                                           (*****************)
     levrange = 0..maxlevel; addrrange = 0..maxaddr;
     structform = (scalar,subrange,pointer,power,arrays,records,files,
                   tagfld,variant);
     declkind = (standard,declared);
     stp = ^ structure; 
     ctp = ^ identifier;

     structure = record
                   next: stp; { next entry link }
                   marked: boolean;   (*for test phase only*)
                   size: addrrange;
                   packing: boolean; { packing status }
                   case form: structform of
                     scalar:   (case scalkind: declkind of
                                  declared: (fconst: ctp); standard: ());
                     subrange: (rangetype: stp; min,max: valu);
                     pointer:  (eltype: stp);
                     power:    (elset: stp; matchpack: boolean);
                     arrays:   (aeltype,inxtype: stp);
                     records:  (fstfld: ctp; recvar: stp; recyc: stp);
                     files:    (filtype: stp);
                     tagfld:   (tagfieldp: ctp; fstvar: stp);
                     variant:  (nxtvar,subvar: stp; varval: valu)
                   end;

                                                            (*names*)
                                                            (*******)

     idclass = (types,konst,vars,field,proc,func);
     setofids = set of idclass;
     idkind = (actual,formal);
     idstr = packed array [1..maxids] of char;
     restr = packed array [1..reslen] of char;
     nmstr = packed array [1..digmax] of char;
     csstr = packed array [1..strglgth] of char;
     identifier = record
                   name: strvsp; llink, rlink: ctp;
                   idtype: stp; next: ctp; keep: boolean;
                   case klass: idclass of
                     types: ();
                     konst: (values: valu);
                     vars:  (vkind: idkind; vlev: levrange; vaddr: addrrange);
                     field: (fldaddr: addrrange);
                     proc, func:  (pfaddr: addrrange; pflist: ctp; { param list }
                                   case pfdeckind: declkind of
                              standard: (key: 1..18);
                              declared: (pflev: levrange; pfname: integer;
                                          case pfkind: idkind of
                                           actual: (forwdecl, externl: boolean);
                                           formal: ()))
                   end;


     disprange = 0..displimit;
     where = (blck,crec,vrec,rec);

                                                            (*expressions*)
                                                            (*************)
     attrkind = (cst,varbl,expr);
     vaccess = (drct,indrct,inxd);

     attr = record typtr: stp;
              case kind: attrkind of
                cst:   (cval: valu);
                varbl: (case access: vaccess of
                          drct: (
                          vlevel: levrange;
                          dplmt: addrrange
                          );
                          indrct: (
                            idplmt: addrrange
                          );
           inxd: ());
      expr: ()
              end;

                                                                 (*labels*)
                                                                 (********)
     lbp = ^ labl;
     labl = record { 'goto' label }
                   nextlab: lbp;      { next list link }
                   defined: boolean;  { label defining point was seen }
                   labval,            { numeric value of label }
                   labname: integer;  { internal sequental name of label }
                   vlevel: levrange;  { procedure level of definition }
                   slevel:  integer;  { statement level of definition }
                   ipcref:  boolean;  { was referenced by another proc/func }
                   minlvl:  integer;  { minimum goto reference statement lvl }
                   bact:    boolean;  { containing block is active }
            end;

     { external file tracking entries }
     extfilep = ^filerec;
     filerec = record
                   filename:idstr;
                   nextfile:extfilep
               end;

     { case statement tracking entries }
     cip = ^caseinfo;
     caseinfo = record next: cip;
                  csstart: integer;
                  cslab: integer
                end;


     { Used by P5 code generator }
     oprange = 0..maxins;


(*-------------------------------------------------------------------------*)

var

    { !!! remove this statement for self compile }    
    {elide}prr: text;{noelide}       { output code file }

                                    (*returned by source program scanner
                                     insymbol:
                                     **********)

    sy: symbol;                     (*last symbol*)
    op: Operators;                  (*classification of last symbol*)
    val: valu;                      (*value of last constant*)
    lgth: integer;                  (*length of last string constant*)
    id: idstr;                      (*last identifier (possibly truncated)*)
    kk: 1..maxids;                  (*nr of chars in last identifier*)
	
	
	CommentPrefix,                  (*PR 8/2013: P5 or P6 comment *)
    ch: char;                       (*last character*)
    eol: boolean;                   (*end of line flag*)


                                    (*counters:*)
                                    (***********)

    chcnt: integer;                 (*character counter*)
    lc,ic: addrrange;               (*data location and instruction counter*)
    linecount: integer;             (*Number of lines read from source file*)

                                    (*switches:*)
                                    (***********)

    dp,                             (*declaration part*)
	
	(*UPDATE PR 2013-08-19 *)
	realcode, (* R+ generate real machine code *)
	spinner,  (* S+ Compiler is sometimes slow, run a spinner or *)
                  (*    some form of indicator to let it be known *)
		  (*    we're not dead *)
			  
	(*END UPDATE*)
	
    list,prcode,prtables: boolean;  (*output options for
                                        -- source program listing
                                        -- printing symbolic code
                                        -- displaying ident and struct tables
                                        --> procedure option*)
    debug: boolean;


                                    (*pointers:*)
                                    (***********)
    parmptr,
    intptr,realptr,charptr,
    boolptr,nilptr,textptr: stp;    (*pointers to entries of standard ids*)
    utypptr,ucstptr,uvarptr,
    ufldptr,uprcptr,ufctptr,        (*pointers to entries for undeclared ids*)
    fwptr: ctp;                     (*head of chain of forw decl type ids*)
    outputptr,inputptr: ctp;        { pointers to default files }
    fextfilep: extfilep;            (*head of chain of external files*)

                                    (*bookkeeping of declaration levels:*)
                                    (************************************)

    level: levrange;                (*current static level*)
    disx,                           (*level of last id searched by searchid*)
    top: disprange;                 (*top of display*)

    display:                        (*where:   means:*)
      array [disprange] of
        packed record               (*=blck:   id is variable id*)
          fname: ctp; flabel: lbp;  (*=crec:   id is field id in record with*)
          fconst: csp; fstruct: stp;
          case occur: where of      (*   constant address*)
            crec: (clev: levrange;  (*=vrec:   id is field id in record with*)
                  cdspl: addrrange);(*   variable address*)
            vrec: (vdspl: addrrange);
            blck: (bname: ctp);     { block id }
            rec: ()
        end;                        (* --> procedure withstatement*)


                                    (*error messages:*)
                                    (*****************)

    errinx: 0..10;                  (*nr of errors in current source line*)
    errlist:
      array [1..10] of
        packed record pos: integer;
                      nmr: 1..500
               end;



                                    (*expression compilation:*)
                                    (*************************)

    gattr: attr;                    (*describes the expr currently compiled*)


                                    (*structured constants:*)
                                    (***********************)

    constbegsys,simptypebegsys,typebegsys,blockbegsys,selectsys,facbegsys,
    statbegsys,typedels: setofsys;
    chartp : array[char] of chtp;
    rw:  array [1..maxres(*nr. of res. words*)] of restr;
    frw: array [1..10] of 1..36(*nr. of res. words + 1*);
    rsy: array [1..maxres(*nr. of res. words*)] of symbol;
    ssy: array [char] of symbol;
    rop: array [1..maxres(*nr. of res. words*)] of Operators;
    sop: array [char] of Operators;
    na:  array [1..maxstd] of restr;
	{ These are the P5 mnemonics, used when generating P5 code }
    mn:  array [0..maxins] of packed array [1..4] of char; 
	
	{ PR 2013-08-19 These are the True mnemonics, used when 
	  generating real code }
	truemc: array[0..trueins] of packed array [1..5] of char;
	{ End of update }
	
	sna: array [1..maxsp] of packed array [1..4] of char;
    cdx: array [0..maxins] of -4..+4;
    pdx: array [1..maxsp] of -7..+7;
    ordint: array [char] of integer;

    intlabel,mxint10: integer;
    inputhdf: boolean; { 'input' appears in header files }
    outputhdf: boolean; { 'output' appears in header files }
    errtbl: array [1..500] of boolean; { error occrence tracking }
    toterr: integer; { total errors in program }

    { Recycling tracking counters, used to check for new/dispose mismatches. }
    strcnt: integer; { strings }
    cspcnt: integer; { constants }
    stpcnt: integer; { structures }
    ctpcnt: integer; { identifiers }
    lbpcnt: integer; { label counts }
    filcnt: integer; { file tracking counts }
    cipcnt: integer; { case entry tracking counts }

    f: boolean; { flag for if error number list entries were printed }
    i: 1..500; { index for error number tracking array }


{ Used to buffer the source file }
    ProgLine:   csstr;         { Current source line }
    QQQQ, {used as a tripper flag}
    ProgLineCurrent:Integer;        { Byte being read}
    isEndofSourceFile: boolean; {replaces Eof(Input) }


{$IFNDEF FPC}
procedure halt; begin goto 99; end;
{$ENDIF}

(* ------------------ Standardized Machine Instructions ---------------------  *
*                                                                              *
* Now, these procedures are used by your compiler when it generates code.      *
* These are called from the compiler to handle various conditions that         *
* your machine and/or operating system might need; if you don't need to        *
* do anything, then they just can be empty.                                   *)

{ Called before the Program Statement,
  or if no program statment is present }
Procedure CompBeforeProgram;
begin
end;


{ Called when the Program Statement occurs, in case you need to do some
  initializations for the Program Statement. The name of the program has
  been collected in the variable SY.
}
Procedure CompProgram;
begin
end;





(* ---------------- End of Standardized Machine Instructions ------------ *)


 {2013-08-20 PR: eliminate all references to Input; use these procedures
      and do all input through them. This decouples the scanner from the
      input file and allows us to use various other methods, including
      in-memory compile (useful if we were a script compiler for doing
      run-time scripting), stream scanning, even reading from a TCP/IP
      port }


  Function isEolnSourceFile: Boolean; {replaces Eoln(Input) }
  begin
       isEolnSourceFile := (Progline[ProgLineCurrent]=chr(0)) or
            isEndofSourceFile;
  end;

  Function SourceLookaheadCh: char; {replaces input^ }
  begin
      if isEolnSourceFile then
         SourceLookAheadCh  := ' '
      else
         SourceLookAheadCh := Progline[ProgLineCurrent+1];
  end;

  Function ReadSourceCh: char; {replaces Read(Input,ch) }
  VAR
      I:Integer;
  begin
     if (proglinecurrent = 0) or (isEolnSourceFile) then
     begin
       write(' start new line ');
       proglinecurrent := 1;
       for I := 1 to 250 do
          progline[I] := chr(0);
       if not Eof(input) then
       begin
          readln(input,progline);
          qqqq := qqqq+1;
          if qqqq > 6 then halt;
          I := 250;
          while progline[I]=chr(0) do
             I := I - 1;
          ProgLine[I] := ' ';
       end
       else
          isEndofSourceFile := TRUE;
      end;
      if not isEndOfSourceFile then
      begin
         ReadSourceCh := progline[ ProgLineCurrent ];
         proglinecurrent := proglinecurrent +1;
      end
      else
         readsourcech := ' ';
  end;

{END UPDATE}

	
(*-------------------------------------------------------------------------*)

                           { recycling controls }

(*-------------------------------------------------------------------------*)

  { get string quanta }
  procedure getstr(var p: strvsp);
  begin
     new(p); { get new entry }
     strcnt := strcnt+1 { count }
  end;

  { recycle string quanta list }
  procedure putstrs(p: strvsp);
  var p1: strvsp;
  begin
    while p <> nil do begin
      p1 := p; p := p^.next; dispose(p1); strcnt := strcnt-1
    end
  end;

  { get label entry }
  procedure getlab(var p: lbp);
  begin
     new(p); { get new entry }
     lbpcnt := lbpcnt+1 { add to count }
  end;

  { recycle label entry }
  procedure putlab(p: lbp);
  begin
     dispose(p); { release entry }
     lbpcnt := lbpcnt-1 { remove from count }
  end;

  { push constant entry to list }
  procedure pshcst(p: csp);
  begin
     { push to constant list }
     p^.next := display[top].fconst;
     display[top].fconst := p;
     cspcnt := cspcnt+1 { count entries }
  end;

  { recycle constant entry }
  procedure putcst(p: csp);
  begin
     { recycle string if present }
     if p^.cclass = strg then putstrs(p^.sval)
     else if p^.cclass = reel then putstrs(p^.rval);
     dispose(p); { release entry }
     cspcnt := cspcnt-1 { remove from count }
  end;

  { push structure entry to list }
  procedure pshstc(p: stp);
  begin
     { push to structures list }
     p^.next := display[top].fstruct;
     display[top].fstruct := p;
     stpcnt := stpcnt+1 { count entries }
  end;

  { recycle structure entry }
  procedure putstc(p: stp);
  begin
     dispose(p); { release entry }
     stpcnt := stpcnt-1
  end;

  { initialize and register identifier entry }
  procedure ininam(p: ctp);
  begin
     ctpcnt := ctpcnt+1; { count entry }
     p^.keep := false { clear keepme flag }
  end;

  { recycle identifier entry }
  procedure putnam(p: ctp);
  var p1: ctp;
  begin
     if (p^.klass = proc) or (p^.klass = func) then
        while p^.pflist <> nil do begin
        { scavenge the parameter list }
        p1 := p^.pflist; p^.pflist := p1^.next;
        putnam(p1) { release }
     end;
     putstrs(p^.name); { release name string }    
     dispose(p); { release entry }
     ctpcnt := ctpcnt-1 { remove from count }
  end;

  { recycle identifier tree }
  procedure putnams(p: ctp);
  begin
    if p <> nil then begin
      putnams(p^.llink); { release left }
      putnams(p^.rlink); { release right }
      { "keep" means it is a parameter and stays with it's procedure or
        function entry. }
      if not p^.keep then putnam(p) { release the id entry }
    end
  end;
     
  { scrub display level }
  procedure putdsp(l: disprange);
     var llp: lbp; lvp: csp; lsp: stp;
     { release substructure }
     procedure putsub(p: stp);
        var p1: stp;
     begin
        { clear record recycle list if record }
        if p^.form = records then begin
           { clear structure list }
           while p^.recyc <> nil do begin
              { remove top of list }
              p1 := p^.recyc; p^.recyc := p1^.next;
              putsub(p1) { release that element }
           end;
           putnams(p^.fstfld) { clear id list }
        end else if p^.form = tagfld then begin
              if p^.tagfieldp <> nil then
                 { recycle anonymous tag fields }
                 if p^.tagfieldp^.name = nil then putnam(p^.tagfieldp)
        end;
        putstc(p) { release head entry }
     end;
  begin { putdsp }
    putnams(display[l].fname); { dispose of identifier tree }
    { dispose of label list }
    while display[l].flabel <> nil do begin
      llp := display[l].flabel; display[l].flabel := llp^.nextlab; putlab(llp)
    end;
    { dispose of constant list }
    while display[l].fconst <> nil do begin
      lvp := display[l].fconst; display[l].fconst := lvp^.next; putcst(lvp)
    end;
    { dispose of structure list }
    while display[l].fstruct <> nil do begin
      { remove top from list }
      lsp := display[l].fstruct; display[l].fstruct := lsp^.next; putsub(lsp)
    end
  end; { putdsp }

  { scrub all display levels until given }
  procedure putdsps(l: disprange);
  var t: disprange;
  begin
    if l > top then begin
      writeln('*** Error: Compiler internal error');
      {$IFNDEF FPC}
       goto 99;
      {$ELSE}
       (*  }{  halt(99); XXX   }{   *)
      {$ENDIF}
    end;
    t := top;
    while t > l do
    begin
      putdsp(t);
      t := t-1
    end
  end;

  { get external file entry }
  procedure getfil(var p: extfilep);
  begin
     new(p); { get new entry }
     filcnt := filcnt+1 { count entry }
  end;

  { recycle external file entry }
  procedure putfil(p: extfilep);
  begin
     dispose(p); { release entry }
     filcnt := filcnt-1 { count entry }
  end;

  { get case tracking entry }
  procedure getcas(var p: cip);
  begin
     new(p); { get new entry }
     cipcnt := cipcnt+1 { count entry }
  end;

  { recycle case tracking entry }
  procedure putcas(p: cip);
  begin
     dispose(p); { release entry }
     cipcnt := cipcnt-1 { count entry }
  end;


(*-------------------------------------------------------------------------*)

                { character and string quanta functions }

(*-------------------------------------------------------------------------*)

  { find lower case of character }

  { 2013-08-20 PR: We have to split this; in EBCDIC the
      character set for lower case is not A to Z, there
      are breaks in the sequence }

  function lcase(c: char): char;
  begin
    if c in ['A'..'Z'] then c := chr(ord(c)-ord('A')+ord('a'));
    lcase := c
  end { lcase };

  { convert string to lower case }
  procedure lcases(var s: idstr);
  var i: integer;
  begin
    for i := 1 to maxids do s[i] := lcase(s[i]);
  end;

  { find reserved word string equal to id string }
  function strequri(a: restr; var b: idstr): boolean;
  var m: boolean; i: integer;
  begin
    m := true;
    for i := 1 to reslen do
       if lcase(a[i]) <> lcase(b[i]) then
           m := false;
    for i := reslen+1 to maxids do
       if b[i] <> SymSpace then
           m := false;
    strequri := m
  end { equstr };

  { write variable length id string to output }
  procedure writev(var f: text; s: strvsp; fl: integer);
  var
      i: integer;
      c: char;

  begin i := 1;
    while fl > 0 do
    begin
      c := SymSpace;
      if s <> nil then
      begin
         c := s^.str[i];
         i := i+1
      end;
      write(f, c);
      fl := fl-1;
      if i > varsqt then
      begin
         s := s^.next;
         i := 1
      end
    end
  end;

  { find padded length of variable length id string }
  function lenpv(s: strvsp): integer;
  var
      i, l, lc: integer;

  begin
    l := 1;
    lc := 0;
    while s <> nil do
    begin
      for i := 1 to varsqt do
      begin
        if s^.str[i] <> SymSpace then
           lc := l;
        l := l+1; { count characters }
      end;
      s := s^.next
    end;
    lenpv := lc
  end;

  { assign identifier fixed to variable length string, including allocation }
  procedure strassvf(var a: strvsp; var b: idstr);
  var
      i, j, l: integer;
      p, lp: strvsp;

  begin
    l := maxids;
    p := nil;
    a := nil;
    j := 1;
    while (l > 1) and (b[l] = SymSpace) do
        l := l-1; { find length of fixed string }
    if b[l] = SymSpace then
       l := 0;
    for i := 1 to l do
    begin
      if j > varsqt then
         p := nil;
      if p = nil then
      begin
        getstr(p);
        p^.next := nil;
        j := 1;
        if a = nil then
          a := p
        else
          lp^.next := p;
        lp := p
      end;
      p^.str[j] := b[i];
      j := j+1
    end;
    if p <> nil then
      for j := j to varsqt do
         p^.str[j] := SymSpace
  end;

  { assign reserved word fixed to variable length string, including allocation }
  procedure strassvr(var a: strvsp; b: restr);
  var
      i, j, l: integer;
      p, lp: strvsp;

  begin
    l := reslen;
    p := nil;
    a := nil;
    lp := nil;
    j := 1;
    while (l > 1) and (b[l] = SymSpace) do
       l := l-1; { find length of fixed string }
    if b[l] = SymSpace then
       l := 0;
    for i := 1 to l do
    begin
      if j > varsqt then
         p := nil;
      if p = nil then
      begin
        getstr(p);
        p^.next := nil;
        j := 1;
        if a = nil then
           a := p
        else
           lp^.next := p;
        lp := p
      end;
      p^.str[j] := b[i];
      j := j+1
    end;
    if p <> nil then
      for j := j to varsqt do
         p^.str[j] := SymSpace
  end;

  { assign number string fixed to variable length string, including allocation }
  procedure strassvd(var a: strvsp; b: nmstr);
  var
      i, j, l: integer;
      p, lp: strvsp;

  begin
     l := digmax;
     p := nil;
     a := nil;
     lp := nil;
     j := 1;
     while (l > 1) and (b[l] = SymSpace) do
         l := l-1; { find length of fixed string }
     if b[l] = SymSpace then
       l := 0;
     for i := 1 to l do
     begin
        if j > varsqt then
          p := nil;
        if p = nil then
        begin
           getstr(p);
           p^.next := nil;
           j := 1;
           if a = nil then
              a := p
           else
              lp^.next := p;
           lp := p
        end;
        p^.str[j] := b[i];
        j := j+1
    end;
    if p <> nil then
       for j := j to varsqt do
          p^.str[j] := SymSpace
  end;

  { assign constant string fixed to variable length
    string, including allocation }
  procedure strassvc(var a: strvsp; b: csstr; l: integer);
  var
      i, j: integer;
      p, lp: strvsp;

  begin
     p := nil;
     a := nil;
     lp := nil;
     j := 1;
     for i := 1 to l do
     begin
        if j > varsqt then
          p := nil;
        if p = nil then
        begin
           getstr(p);
           p^.next := nil;
           j := 1;
           if a = nil then
             a := p
           else
             lp^.next := p;
           lp := p
        end;
        p^.str[j] := b[i];
        j := j+1
    end;
    if p <> nil then
      for j := j to varsqt do
         p^.str[j] := SymSpace
  end;

  { assign variable length string to fixed identifier }
  procedure strassfv(var a: idstr; b: strvsp);
  var i, j: integer;
  begin for i := 1 to maxids do a[i] := SymSpace; i := 1;
     while b <> nil do begin
        for j := 1 to varsqt do begin a[i] := b^.str[j]; i := i+1 end; 
        b := b^.next
     end
  end;

  { compare variable length id strings }
  function strequvv(a, b: strvsp): boolean;
  var m: boolean; i: integer;
  begin
    m := true;
    while (a <> nil) and (b <> nil) do begin
      for i := 1 to varsqt do if lcase(a^.str[i]) <> lcase(b^.str[i]) then m := false;
      a := a^.next; b := b^.next
    end;
    if a <> b then m := false;
    strequvv := m
  end;

  { compare variable length id strings, a < b }
  function strltnvv(a, b: strvsp): boolean;
  var i: integer; ca, cb: char;
  begin ca := SymSpace; cb := SymSpace;
    while (a <> nil) or (b <> nil) do begin
      i := 1;
      while (i <= varsqt) and ((a <> nil) or (b <> nil)) do begin
        if a <> nil then ca := lcase(a^.str[i]) else ca := SymSpace;
        if b <> nil then cb := lcase(b^.str[i]) else cb := SymSpace;
        if ca <> cb then begin a := nil; b := nil end;
        i := i+1
      end;
      if a <> nil then a := a^.next; if b <> nil then b := b^.next
    end;
    strltnvv := ca < cb
  end;

  { compare variable length id string to fixed }
  function strequvf(a: strvsp; var b: idstr): boolean;
  var m: boolean; i, j: integer; c: char;
  begin
    m := true; j := 1;
    for i := 1 to maxids do begin
      c := SymSpace; if a <> nil then begin c := a^.str[j]; j := j+1 end;
      if lcase(c) <> lcase(b[i]) then m := false;
      if j > varsqt then begin a := a^.next; j := 1 end
    end;
    strequvf := m
  end;

  { compare variable length id string to fixed, a < b }
  function strltnvf(a: strvsp; var b: idstr): boolean;
  var m: boolean; i, j, f: integer; c: char;
  begin
    m := true; i := 1; j := 1;
    while i < maxids do begin
      c := SymSpace; if a <> nil then begin c := a^.str[j]; j := j+1 end;
      if lcase(c) <> lcase(b[i]) then begin f := i; i := maxids end else i := i+1;
      if j > varsqt then begin a := a^.next; j := 1 end
    end;
    strltnvf := lcase(c) < lcase(b[f])
  end;

  { get character from variable length string }

  function strchr(a: strvsp; x: integer): char;
  var c: char; i: integer; q: integer;
  begin
     c := SymSpace; i := 1; q := 1;
     while i < x do begin 
        if q >= varsqt then begin q := 1; if a <> nil then a := a^.next end
        else q := q+1;
        i := i+1
     end;
     if a <> nil then c := a^.str[q];
     strchr := c
   end;
     
  { put character to variable length string }

  procedure strchrass(var a: strvsp; x: integer; c: char);
  var i: integer; q: integer; p, l: strvsp;
  procedure getsqt;
  var y: integer;
  begin
     if p = nil then begin getstr(p); for y := 1 to varsqt do p^.str[y] := SymSpace;
        p^.next := nil; if a = nil then a := p else l^.next := p
     end
  end;
  begin
     i := 1; q := 1; p := a; l := nil;
     getsqt;
     while i < x do begin 
        if q >= varsqt then begin q := 1; l := p; p := p^.next; getsqt end
        else q := q+1;    
        i := i+1
     end;
     p^.str[q] := c
   end;

(*-------------------------------------------------------------------------*)

  { dump the display }
  procedure prtdsp;
  var i: integer;
  procedure prtlnk(p: ctp; f: integer);
  var i: integer;
  begin
    if p <> nil then begin
      for i := 1 to f do write(SymSpace);
      writev(output, p^.name, 10); writeln;
      if p^.llink <> nil then prtlnk(p^.llink, f+3);
      if p^.rlink <> nil then prtlnk(p^.rlink, f+3)
    end
  end;
  begin
    writeln;
    writeln('Display:');
    writeln;
    for i := 0 to displimit do if display[i].fname <> nil then begin

       writeln('level ', i:1);
       writeln;
       prtlnk(display[i].fname, 0);
       writeln
 
    end;
    writeln;
  end;

  procedure endofline;
    var lastpos,freepos,currpos,currnmr,f,k: integer;
  begin
    if errinx > 0 then   (*output error messages*)
      begin write(output,linecount:6,' ****  ':9);
        lastpos := -1; freepos := 1;
        for k := 1 to errinx do
          begin
            with errlist[k] do
              begin currpos := pos; currnmr := nmr end;
            if currpos = lastpos then write(output,SymComma)
            else
              begin
                while freepos < currpos do
                  begin write(output,SymSpace); freepos := freepos + 1 end;
                write(output,SymUpArrow);
                lastpos := currpos
              end;
            if currnmr < 10 then f := 1
            else if currnmr < 100 then f := 2
              else f := 3;
            write(output,currnmr:f);
            freepos := freepos + f + 1
          end;
        writeln(output); errinx := 0
      end;
    linecount := linecount + 1;

	{2013-08-13: PR Insert spinner here }
	if spinner then
	begin
	   write(output,linecount:5,chr(8),chr(8),chr(8),chr(8),chr(8));
	end;
	{END UPDATE}
	
    if list and (not isEndOfSourceFile) then
      begin write(output,linecount:6,'  ':2);
        if dp then write(output,lc:7) else write(output,ic:7);
        write(output,SymSpace)
      end;
    { output line marker in intermediate file }
    if not isEndOfSourceFile then
	  if realcode then
	  begin
	    writeln(prr, ' LINEM ',linecount:1);  
	  end
	 else
      begin 
         writeln(prr, SymColon, linecount:1);
      end;
    chcnt := 0
  end  (*endofline*) ;

  procedure errmsg(ferrnr: integer);
  begin case ferrnr of
    ErrSympType:   write('Error in simple type'); {  1}
    ErrIDExpect:   write('Identifier expected');  {  2}
    3:   write('''program'' expected');
    ErrClosePar:   write(''')'' expected');       {  4}
    ErrColon:      write(''':'' expected');       {  5}
    ErrIllSym:     write('Illegal symbol');       {  6}
    7:   write('Error in parameter list');
    ErrNoOf:      write('''of'' expected');       {  8}
    ErrOpenPar:   write('''('' expected');        {  9}
    ErrType:      write('Error in type');             { 10}
    ErrOpnBrack:  write('''['' expected');        { 11}
    ErrClsBrack:  write(''']'' expected');        { 12}
    ErrNoEnd:     write('''end'' expected');      { 13}
    ErrNoSemi:    write(''';'' expected');        { 14}
    ErrIntExpect:  write('Integer expected');      { 15}
    ErrEqualExpect:  write('''='' expected'); {16}
    ErrNoBegin:  write('''begin'' expected'); {17}
    ErrDeclare:  write('Error in declaration part'); {18}
    ErrFieldL:    write('Error in field-list'); {19}
    20:  write(''','' expected');
    21:  write('''.'' expected');

    ErrConst:  write('Error in constant'); {50}
    ErrBecomes:  write(''':='' expected');
    52:  write('''then'' expected');
    53:  write('''until'' expected');
    54:  write('''do'' expected');
    55:  write('''to''/''downto'' expected');
    56:  write('''if'' expected');
    57:  write('''file'' expected');
    58:  write('Error in factor');
    59:  write('Error in variable');

    ErrTwice:      write('Identifier declared twice');                {101}
    ErrLowAboveHi: write('Low bound exceeds highbound');              {102}
    ErrClass:      write('Identifier is not of appropriate class');   {103}
    ErrNotThere:   write('Identifier not declared');                  {104}
    ErrSign:       write('Sign not allowed');                         {105}
    ErrNumExpect:  write('Number expected');                          {106}
    ErrIncompSub:  write('Incompatible subrange types');              {107}
    ErrRealForbid: write('Type must not be real');                    {109}
    ErrBadTagFld:  write('Tagfield type must be scalar or subrange'); {110}
    ErrIncTagFld:  write('Incompatible with tagfield type');          {111}
    112: write('Index type must not be real');                        {112}
    ErrIdxNoScal:  write('Index type must be scalar or subrange');    {113}
    ErrBaseReal:   write('Base type must not be real');               {114}
    ErrBaseNoScal: write('Base type must be scalar or subrange');     {115}
    ErrStdProcParm:write('Error in type of standard procedure parameter'); {116}
    ErrUnsatFwd: write('Unsatisfied forward reference');         {117}
    118: write('Forward reference type identifier in variable declaration');
    119: write('Forward declared; repetition of parameter list not allowed');
    120: write('Function result type must be scalar, subrange or point');
    121: write('File value parameter not allowed');
    122: write('Forward declared function; repetition of result type not allowed');
    123: write('Missing result type in function declaration');
    124: write('F-format for real only');
    125: write('Error in type of standard function parameter');
    126: write('Number of parameters does not agree with declaration');
    127: write('Illegal parameter substitution');
    128: write('Result type of parameter function does not agree with declaration');
    129: write('Type conflict of operands');
    130: write('Expression is not of set type');
    131: write('Tests on equality allowed only');
    132: write('Strict inclusion not allowed');
    133: write('File comparison not allowed');
    ErrIllOp: write('Illegal type of operand(s)'); {134}
    135: write('Type of operand must be Boolean');
    136: write('Set element type must be scalar nr subrange');
    137: write('Set element types not compatible');
    138: write('Type of variable is not array');
    139: write('Index type is not compatible with declaration');
    140: write('Type of variable is not record');
    141: write('Type of variable must be file or pointer');
    142: write('Illegal parameter substitution');
    143: write('Illegal type of loop control variable');
    144: write('Illegal type of expression');
    145: write('Type conflict');
    146: write('Assignment of files not allowed');
    147: write('Label type incompatible with selecting expression');
    ErrSubScal: write('Subrange bounds must be scalar');               {148}
    ErrIndex:   write('Index type must not be integer');               {149}
    150: write('Assignment to standard function is not allowed');
    151: write('Assignment to formal function is not allowed');
    152: write('No such field in this record');
    153: write('Type error in read');
    154: write('Actual parameter must be a variable');
    155: write('Control variable must ~ot be declared on intermediate');
    156: write('Multidefined case label');
    157: write('Too many cases in case statement');
    158: write('Missing corresponding variant declaration');
    159: write('Real or string tagfields not allowed');
    160: write('Previous declaration was not forward');
    161: write('Again forward declared');
    162: write('Parameter size must be constant');
    163: write('Missing variant in declaration');
    164: write('Substitution of standard proc/func not allowed');
    165: write('Multidefined label');          {165}
    ErrMultiDecl: write('Multideclared label');  {166}
    167: write('Undeclared label');
    168: write('Undefined label');
    ErrBaseSet: write('Error in base set');     {169}
    170: write('Value parameter expected');
    171: write('Standard file was redeclared');
    172: write('Undeclared external file');
    173: write('Fortran procedure or function expected');
    174: write('Pascal procedure or function expected');
    175: write('Missing file "input" in program heading');
    176: write('Missing file "output" in program heading');
    177: write('Assiqnment to function identifier not allowed here');
    ERRMdefRecVar: write('Multidefined record variant'); {178}
    179: write('X-opt of actual proc/funcdoes not match formal declaration');
    180: write('Control variable must not be formal');
    181: write('Constant part of address out of ranqe');
    ErrIDTooLong:  write('identifier too long');                      {182}
    183: write('For index variable must be local to this block');
    184: write('Interprocedure goto does not reference outter block of destination');
    185: write('Goto references deeper nested statement');
    186: write('Label referenced by goto at lesser statement level');
    187: write('Goto references label in different nested statement');
    188: write('Label referenced by goto in different nested statement');
    189: write('Parameter lists of formal and actual parameters not congruous');
    ErrNestFile: write('File component may not contain other files'); {190}
    191: write('Cannot assign from file or component containing files');
    192: write('Assignment to function that is not active');

    ErrRealDigEx: write('Error in real constant: digit expected');       {201}
    ErrStrTooLng: write('String constant must not exceed source line');  {202}
    ErrIntConExR: write('Integer constant exceeds range');               {203} 
    ErrBadOctal:  write('8 or 9 in octal number');                       {204 - Not Currently Used }
    ErrNoZeroStr: write('Zero strinq not allowed');                      {205}
    206: write('Integer part of real constant exceeds ranqe');

    250: write('Too many nestedscopes of identifiers');
    251: write('Too many nested procedures and/or functions');
    252: write('Too many forward references of procedure entries');
    253: write('Procedure too long');
    254: write('Too many long constants in this procedure');
    255: write('Too many errors on this source line');
    256: write('Too many external references');
    257: write('Too many externals');
    258: write('Too many local files');
    259: write('Expression too complicated');
    260: write('Too many exit labels');

    300: write('Division by zero');
    301: write('No case provided for this value');
    302: write('Index expression out of bounds');
    303: write('Value to be assigned is out of bounds');
    304: write('Element expression out of range');

    398: write('Implementation restriction');
    ErrNotImp: write('Feature not implemented');  {399}

    ErrInternal1: write('Compiler internal error 400');   {400}
    500: write('Compiler internal error 500');
	ErrInternal3:  write('Compiler internal error 501');  {There's at least one use of 501}
    end    
  end;

  procedure error(ferrnr: integer);
  begin

    { This diagnostic is here because error buffers error numbers til the end
      of line, and sometimes you need to know exactly where they occurred. }

    {

    writeln('error: ', ferrnr:1); 

    }

    errtbl[ferrnr] := true; { track this error }
    if errinx >= 9 then
      begin errlist[10].nmr := 255; errinx := 10 end
    else
      begin errinx := errinx + 1;
        errlist[errinx].nmr := ferrnr
      end;
    errlist[errinx].pos := chcnt;
    toterr := toterr+1
  end (*error*) ;

  procedure insymbol;
    (*read next basic symbol of source program and return its
    description in the global variables sy, op, id, val and lgth*)
    label 1;
    var i,k,j: integer;
        digit: nmstr; { temp holding for digit string }
        rvalb: nmstr; { temp holding for real string }
        FixedString: csstr;
        lvp: csp; test, ferr: boolean;
        iscmte: boolean;

    procedure nextch;
    begin
      if eol then
      begin
         if list then
            writeln(output);
         endofline
      end;
      if not isEndOfSourceFile then
      begin
         eol := isEolnSourceFile;
         ch := ReadSourceCh;
         if list then
            write(output,ch);
         chcnt := chcnt + 1
      end
      else
      begin
         writeln(output,'   *** eof ','encountered');
         test := false
      end;
    end;

{ Called when a comment command $ occurs to check for a
  compiler option}

    procedure options;
    begin
       repeat
        nextch;
        if ch <> SymStar then
        begin
           if lcase(ch) = 't' then
           begin
              nextch;
              prtables := ch = SymPlus
           end
           else if lcase(ch) = 'l' then
           begin
              nextch;
              list := ch = SymPlus;
              if not list then
                 writeln(output)
           end
           else if lcase(ch) = 'd' then
           begin
              nextch;
              debug := ch = SymPlus
           end
           else if lcase(ch) = 'c' then
           begin
              nextch;
              prcode := ch = SymPlus
           end

{ PR: 2013-08-19: Add 'r' to indicate, generate real machine code 
                      's' to run a spinner showing what it's doing }
           else if lcase(ch) = 'r' then
           begin
              nextch;
              realcode := ch = SymPlus
           end
	   else if lcase(ch) = 's' then
           begin
              nextch;
              spinner := ch = SymPlus
           end;
{ end of addition }				  
           nextch
         end
      until ch <> SymComma
    end (*options*) ;

  begin (*insymbol*)

  1:
    { Skip both spaces and controls. This allows arbitrary
      formatting characters in the source. }
    repeat
        while (ch <= SymSpace) and not eol do
            nextch;
        test := eol;
        if test then
           nextch;
    until not test;
    if chartp[ch] = illegal then
    begin
       sy := othersy;
       op := noop;
       error(ErrNotImp);
       nextch
    end
    else
      case chartp[ch] of
      letter:
        begin
          k := 0;
          ferr := true;
          repeat
             if k < maxids then
             begin
                k := k + 1;
                id[k] := ch
             end
             else if ferr then
             begin
                error(ErrIDTooLong);
                ferr := false
             end;
             nextch
          until chartp[ch] in [special,illegal,chstrquo,
                               chcolon,chperiod,chlt,chgt,
                               chlparen,chspace,chlcmt];
          if k >= kk then
             kk := k
          else
            repeat
              id[kk] := SymSpace;
              kk := kk - 1
            until kk = k;
          sy := ident;
          op := noop;
          if k <= reslen then
          for i := frw[k] to frw[k+1] - 1 do
             if strequri(rw[i], id) then
                begin
                   sy := rsy[i];
                   op := rop[i]
                end;
        end;

      number:
        begin
           op := noop;
           i := 0;
           repeat
              i := i+1;
              if i<= digmax then
                 digit[i] := ch;
              nextch
           until chartp[ch] <> number;
           if ((ch = SymPeriod) and (SourceLookaheadCh <> SymPeriod) and
               (SourceLookaheadCh <> SymClosePar)) or
               (lcase(ch) = 'e') then
           begin
              k := i;
              if ch = SymPeriod then
              begin
                 k := k+1;
                 if k <= digmax then
                   digit[k] := ch;
                  nextch;
                  (*if ch = SymPeriod then
                     begin ch := SymColon; goto 3 end;
                  *)
                  if chartp[ch] <> number then
                     error(ErrRealDigEx)
                  else
                  repeat
                      k := k + 1;
                      if k <= digmax then
                         digit[k] := ch;
                      nextch
                  until chartp[ch] <>  number
               end;
               if lcase(ch) = 'e' then
                 begin
                    k := k+1;
                    if k <= digmax then
                       digit[k] := ch;
                    nextch;
                    if (ch = SymPlus) or (ch =SymMinus) then
                    begin
                      k := k+1;
                      if k <= digmax then
                        digit[k] := ch;
                      nextch
                    end;
                    if chartp[ch] <> number then
                      error(ErrRealDigEx)
                    else
                    repeat
                      k := k+1;
                      if k <= digmax then
                         digit[k] := ch; nextch
                    until chartp[ch] <> number
                 end;

               new(lvp { ,reel  -- new() referencing tagfield cclass });
               lvp^.cclass := reel;
               pshcst(lvp); sy:= realconst;
               lvp^.cclass := reel;
               with lvp^ do
                 begin
                    for i := 1 to digmax do
                       rvalb[i] := SymSpace;
                    if k <= digmax then
                    for i := 2 to k + 1 do
                       rvalb[i] := digit[i-1]
                    else
                    begin
                        error(ErrIntConExR);   {Integer const exceeds range}
                        rvalb[2] := '0';
                        rvalb[3] := SymPeriod;
                        rvalb[4] := '0'
                    end;
                   { place buffered real string in constant }
                    strassvd(rval, rvalb)
                 end;
               val.valp := lvp
            end
          else
            begin
              if i > digmax then
              begin
                 error(ErrIntConExR);
                 val.ival := 0
              end
              else
                with val do
                  begin ival := 0;
                    for k := 1 to i do
                      begin
                        if ival <= mxint10 then
                          ival := ival*10+ordint[digit[k]]
                        else
                        begin {Int exceeds range}
                          error(ErrIntConExR);
                          ival := 0
                        end
                      end;
                    sy := intconst
                  end
            end
        end;
      chstrquo:
        begin
          lgth := 0;
          sy := stringconst;
          op := noop;
          for i := 1 to strglgth do
             FixedString[i] := SymSpace;
          repeat
            repeat
                nextch;
                lgth := lgth + 1;
                if lgth <= strglgth then
                   FixedString[lgth] := ch
            until (eol) or (ch = SymQuote);
            if eol then
               error(ErrStrTooLng)
            else
               nextch
          until ch <> SymQuote;
          FixedString[lgth] := SymSpace; { get rid of trailing quote }
          lgth := lgth - 1;   (*now lgth = nr of chars in string*)
          if lgth = 0 then
             error(ErrNoZeroStr)
          else
          if lgth = 1 then
             val.ival := ord(FixedString[1])
          else
          begin
              new(lvp { ,strg -- tagfield } );
              lvp^.cclass := strg;
              pshcst(lvp);
              lvp^.cclass:=strg;
              if lgth > strglgth then
              begin
                 error(ErrNotImp);
                 lgth := strglgth
              end;
              with lvp^ do
              begin
                 slgth := lgth;
                 strassvc(sval, FixedString, strglgth)
              end;
              val.valp := lvp
          end
        end;

      chcolon:
        begin
           op := noop;
           nextch;
           if ch = SymEqual then
           begin
              sy := becomes;
              nextch
           end
           else
              sy := colon
        end;

      chperiod:
        begin
          op := noop;
          nextch;
          if ch = SymPeriod then
          begin                      { .. }
             sy := range;
             nextch
          end
          else if ch = SymClosePar then
          begin                  { .) translates to ] }
             sy := rbrack;
             nextch
          end
          else
             sy := period
        end;

      chlt:
        begin
          nextch;
          sy := relop;
          if ch = SymEqual then
          begin
             op := leop;
             nextch
          end
          else
          if ch = SymGreaterThan then
          begin
             op := neop;
             nextch
          end
          else
             op := ltop
        end;

      chgt:
        begin
          nextch;
          sy := relop;
          if ch = SymEqual then
          begin          { >= }
             op := geop;
             nextch end
          else
             op := gtop
        end;

      chlparen:
        begin
          nextch;
          if ch = SymStar then
          begin  {Start of (* comment }
             nextch;
             if ch = SymDollar then options;
             repeat
                 while (ch <> SymCloseComment) and (ch <> SymStar) and not
                        isEndOfSourceFile do
                     nextch;
                 iscmte := ch = SymCloseComment;
                 nextch
             until iscmte or (ch = SymClosePar) or isEndOfSourceFile;
             if not iscmte then
                nextch;
             goto 1
          end
          else if ch = SymPeriod then
          begin
             sy := lbrack;
             nextch
          end
          else
             sy := lparent;
          op := noop
       end;

      chlcmt:
       begin nextch;
         if ch = SymDollar then options;
         repeat
            while (ch <> SymCloseComment) and (ch <> SymStar) and not isEndOfSourceFile do nextch;
            iscmte := ch = SymCloseComment; nextch
         until iscmte or (ch = SymClosePar) or isEndOfSourceFile;
         if not iscmte then nextch; goto 1
       end;
      special:
        begin sy := ssy[ch]; op := sop[ch];
          nextch
        end;
      chspace: sy := othersy
    end; (*case*)

    if dodmplex then begin {  lexical dump }

      writeln;
      write('symbol: ');
      case sy of
         ident:       write('ident: ', id:10);
         intconst:    write('int const: ', val.ival:1);
         realconst:   begin write('real const: '); 
                            writev(output, val.valp^.rval, 9) end;
         stringconst: begin write('string const: '''); 
                            writev(output, val.valp^.sval, val.valp^.slgth) end;
         notsy: write('not');
         mulop: write(SymStar);
         addop: write(SymPlus);
         relop: write(SymLessThan);
         lparent: write(SymOpenPar);
         rparent: write(SymClosePar);
         lbrack: write(SymOpenBracket);
         rbrack: write(SymCloseBracket);
         comma: write(SymComma);
         semicolon: write(SymSemiColon);
         period: write(SymPeriod);
         arrow: write(SymUpArrow);
         colon: write(SymColon);
         becomes: write(':=');
         range: write('..');
         labelsy: write('label');
         constsy: write('const');
         typesy: write('type');
         varsy: write('var');
         funcsy: write('function');
         progsy: write('program');
         procsy: write('procedure');
         setsy: write('set');
         packedsy: write('packed');
         arraysy: write('array');
         recordsy: write('record');
         filesy: write('file');
         beginsy: write('begin');
         ifsy: write('if');
         casesy: write('case');
         repeatsy: write('repeat');
         whilesy: write('while');
         forsy: write('for');
         withsy: write('with');
         gotosy: write('goto');
         endsy: write('end');
         elsesy: write('else');
         untilsy: write('until');
         ofsy: write('of');
         dosy: write('do');
         tosy: write('to');
         downtosy: write('downto');
         thensy: write('then');
         othersy: write('<other>');
      end;
      writeln

    end

  end (*insymbol*) ;

  procedure enterid(fcp: ctp);
    (*enter id pointed at by fcp into the name-table,
     which on each declaration level is organised as
     an unbalanced binary tree*)
    var lcp, lcp1: ctp; lleft: boolean;
  begin
    lcp := display[top].fname;
    if lcp = nil then
      display[top].fname := fcp
    else
      begin
        repeat lcp1 := lcp;
          if strequvv(lcp^.name, fcp^.name) then (*name conflict, follow right link*)
            begin error(ErrTwice); lcp := lcp^.rlink; lleft := false end
          else
            if strltnvv(lcp^.name, fcp^.name) then
              begin lcp := lcp^.rlink; lleft := false end
            else begin lcp := lcp^.llink; lleft := true end
        until lcp = nil;
        if lleft then lcp1^.llink := fcp else lcp1^.rlink := fcp
      end;
    fcp^.llink := nil; fcp^.rlink := nil
  end (*enterid*) ;

  procedure searchsection(fcp: ctp; var fcp1: ctp);
    (*to find record fields and forward declared procedure id's
     --> procedure proceduredeclaration
     --> procedure selector*)
     label 1;
  begin
    while fcp <> nil do
      if strequvf(fcp^.name, id) then goto 1
      else if strltnvf(fcp^.name, id) then fcp := fcp^.rlink
        else fcp := fcp^.llink;
1:  fcp1 := fcp
  end (*searchsection*) ;

  procedure searchidnenm(fidcls: setofids; var fcp: ctp; var mm: boolean);
    label 1;
    var lcp: ctp;
        disxl: disprange;
  begin
    mm := false;
    for disxl := top downto 0 do
      begin lcp := display[disxl].fname;
        while lcp <> nil do begin
          if strequvf(lcp^.name, id) then
            if lcp^.klass in fidcls then begin disx := disxl; goto 1 end
            else
              begin mm := true;
                lcp := lcp^.rlink
              end
          else
            if strltnvf(lcp^.name, id) then
              lcp := lcp^.rlink
            else lcp := lcp^.llink
        end
      end;
      disx := 0;
      lcp := nil; { make sure this is not found }
1:  fcp := lcp
  end (*searchidne*) ;

  procedure searchidne(fidcls: setofids; var fcp: ctp);
    var mm: boolean;
  begin
    searchidnenm(fidcls, fcp, mm);
    if mm then error(ErrClass)
  end (*searchidne*) ;

  procedure searchid(fidcls: setofids; var fcp: ctp);
    label 1;
    var lcp: ctp;
  begin
    searchidne(fidcls, lcp); { perform no error search }
    if lcp <> nil then goto 1; { found }
    (*search not successful
     --> procedure simpletype*)
      error(ErrNotThere);
      (*to avoid returning nil, reference an entry
       for an undeclared id of appropriate class
       --> procedure enterundecl*)
      if types in fidcls then lcp := utypptr
      else
        if vars in fidcls then lcp := uvarptr
        else
          if field in fidcls then lcp := ufldptr
          else
            if konst in fidcls then lcp := ucstptr
            else
              if proc in fidcls then lcp := uprcptr
              else lcp := ufctptr;
1:  fcp := lcp
  end (*searchid*) ;

  procedure getbounds(fsp: stp; var fmin,fmax: integer);
    (*get internal bounds of subrange or scalar type*)
    (*assume fsp<>intptr and fsp<>realptr*)
  begin
    fmin := 0; fmax := 0;
    if fsp <> nil then
    with fsp^ do
      if form = subrange then
        begin fmin := min.ival; fmax := max.ival end
      else
          if fsp = charptr then
            begin fmin := ordminchar; fmax := ordmaxchar
            end
          else
            if fconst <> nil then
              fmax := fconst^.values.ival
  end (*getbounds*) ;

  { alignment for general memory placement }
  function alignquot(fsp: stp): integer;
  begin
    alignquot := 1;
    if fsp <> nil then
      with fsp^ do
        case form of
          scalar:   if fsp=intptr then alignquot := intal
                    else if fsp=boolptr then alignquot := boolal
                    else if scalkind=declared then alignquot := intal
                    else if fsp=charptr then alignquot := charal
                    else if fsp=realptr then alignquot := realal
                    else (*parmptr*) alignquot := parmal;
          subrange: alignquot := alignquot(rangetype);
          pointer:  alignquot := adral;
          power:    alignquot := setal;
          files:    alignquot := fileal;
          arrays:   alignquot := alignquot(aeltype);
          records:  alignquot := recal;
          variant,tagfld: error(ErrInternal3)
        end
  end (*alignquot*);

  procedure align(fsp: stp; var flc: addrrange);
    var k,l: integer;
  begin
    k := alignquot(fsp);
    l := flc-1;
    flc := l + k  -  (k+l) mod k
  end (*align*);

  procedure printtables(fb: boolean);
    (*print data structure and name table*)
    (* Added these functions to convert pointers to integers.
      Works on any machine where pointers and integers are the same format.
      The original code was for a processor where "ord" would do this, a
      very nonstandard feature [sam] *)
    const intsize = 11; (* size of printed integer *)

    var i, lim: disprange;

    function stptoint(p: stp): integer;
    var
        r: record
               case boolean of
                 false: (p: stp);
                  true: (i: integer)
            end;
    begin
       r.p := p;
       stptoint := r.i
    end;

    function ctptoint(p: ctp): integer;
    var
        r: record
              case boolean of
              false: (p: ctp);
               true: (i: integer)
           end;
    begin
       r.p := p;
       ctptoint := r.i
    end;

    procedure marker;
      (*mark data structure entries to avoid multiple printout*)
      var i: integer;

      procedure markctp(fp: ctp); forward;

      procedure markstp(fp: stp);
        (*mark data structures, prevent cycles*)
      begin
        if fp <> nil then
          with fp^ do
            begin marked := true;
              case form of
              scalar:   ;
              subrange: markstp(rangetype);
              pointer:  (*don't mark eltype: cycle possible; will be marked
                        anyway, if fp = true*) ;
              power:    markstp(elset) ;
              arrays:   begin markstp(aeltype); markstp(inxtype) end;
              records:  begin markctp(fstfld); markstp(recvar) end;
              files:    markstp(filtype);
              tagfld:   markstp(fstvar);
              variant:  begin markstp(nxtvar); markstp(subvar) end
              end (*case*)
            end (*with*)
      end (*markstp*);

{$IFNDEF FPC} (*$l+*) {$ENDIF}
      {procedure markctp was defined as forward }
{$IFNDEF FPC}
      procedure markctp;
{$ELSE}
      procedure markctp(fp: ctp);
{$ENDIF}
      begin

{$IFNDEF FPC} (*$l-*) {$ENDIF}
        if fp <> nil then
          with fp^ do
            begin markctp(llink); markctp(rlink);
              markstp(idtype)
            end
      end (*markctp*);

    begin (*marker*)
      for i := top downto lim do
        markctp(display[i].fname)
    end (*marker*);

    procedure followctp(fp: ctp); forward;

    procedure followstp(fp: stp);
    begin
      if fp <> nil then
        with fp^ do
         if marked then
           begin
              marked := false;
              write(output,SymSpace:4,
                 stptoint(*ord*)(fp):intsize(*6*),
                 size:10);
              case form of
              scalar:
                  begin
                     write(output,'scalar':10);
                     if scalkind = standard then
                       write(output,'standard':10)
                     else
                       write(output,'declared':10,
                         SymSpace:4,ctptoint(*ord*)(fconst):intsize(*6*));
                     writeln(output)
                  end;
              subrange:
                  begin
                     write(output,'subrange':10,
                           SymSpace:4,stptoint(*ord*)(rangetype):6);
                     if rangetype <> realptr then
                        write(output,min.ival,max.ival)
                     else
                        if (min.valp <> nil) and
                           (max.valp <> nil) then
                        begin
                           write(SymSpace);
                           writev(output, min.valp^.rval, 9);
                           write(SymSpace);
                           writev(output, max.valp^.rval, 9)
                        end;
                     writeln(output);
                     followstp(rangetype);
                  end;
              pointer:
                  writeln(output,'pointer':10,
                      SymSpace:4,
                      stptoint(*ord*)(eltype):intsize(*6*));
              power:
                  begin
                     writeln(output,'set':10,
                        SymSpace:4,
                        stptoint(*ord*)(elset):intsize(*6*));
                     followstp(elset)
                  end;
              arrays:
                  begin
                     writeln(output,'array':10,
                        SymSpace:4,stptoint(*ord*)(aeltype):intsize(*6*),
                        SymSpace:4,
                        stptoint(*ord*)(inxtype):6);
                     followstp(aeltype);
                     followstp(inxtype)
                  end;
              records:
                  begin
                     writeln(output,'record':10,
                        SymSpace:4,ctptoint(*ord*)(fstfld):intsize(*6*),
                        SymSpace:4,
                        stptoint(*ord*)(recvar):intsize(*6*));
                     followctp(fstfld);
                     followstp(recvar)
                  end;
              files:
                  begin
                     write(output,'file':10,
                        SymSpace:4,stptoint(*ord*)(filtype):intsize(*6*));
                     followstp(filtype)
                  end;
              tagfld:
                  begin
                     writeln(output,'tagfld':10,
                        SymSpace:4,
                        ctptoint(*ord*)(tagfieldp):intsize(*6*),
                        SymSpace:4,
                        stptoint(*ord*)(fstvar):intsize(*6*));
                     followstp(fstvar)
                  end;
              variant:
                  begin
                     writeln(output,'variant':10,
                        SymSpace:4,stptoint(*ord*)(nxtvar):intsize(*6*),
                        SymSpace:4,stptoint(*ord*)(subvar):intsize(*6*),
                        varval.ival);
                     followstp(nxtvar);
                     followstp(subvar)
                  end
           end (*case*)
       end (*if marked*)
    end (*followstp*);

   { procedure followctp was defined as forward }
{$IFDEF FPC}
    procedure followctp(fp: ctp);
{$ELSE}
    (*$l+*)
    procedure followctp;
{$ENDIF}
    begin
      if fp <> nil then
      with fp^ do
      begin
         write(output,SymSpace:4,
            ctptoint(*ord*)(fp):intsize(*6*),
            SymSpace);
         writev(output, name, 9);
         write(SymSpace:4,ctptoint(*ord*)(llink):intsize(*6*),
            SymSpace:4,ctptoint(*ord*)(rlink):intsize(*6*),
            SymSpace:4,stptoint(*ord*)(idtype):intsize(*6*));
         case klass of
              types:
                  write(output,'type':10);
              konst:
                  begin
                     write(output,'constant':10,
                            SymSpace:4,
                            ctptoint(*ord*)(next):intsize(*6*));
                     if idtype <> nil then
                        if idtype = realptr then
                        begin
                           if values.valp <> nil then
                           begin
                              write(SymSpace);
                              writev(output, values.valp^.rval, 9)
                           end
                        end
                        else
                          if idtype^.form = arrays then  (*stringconst*)
                          begin
                             if values.valp <> nil then
                             begin
                                write(output,SymSpace);
                                with values.valp^ do
                                   writev(output, sval, slgth)
                             end
                          end
                          else
                             write(output,values.ival)
                   end;
              vars:
                   begin
                      write(output,'variable':10);
                      if vkind = actual then
                         write(output,'actual':10)
                      else
                         write(output,'formal':10);
                      write(output,SymSpace:4,
                             ctptoint(*ord*)(next):intsize(*6*),
                             vlev,SymSpace:4,
                             vaddr:6 );
                  end;
              field:
                  write(output,'field':10,
                         SymSpace:4,
                         ctptoint(*ord*)(next):intsize(*6*),
                         SymSpace:4,fldaddr:6);
              proc,
              func:
                  begin
                     if klass = proc then
                        write(output,'procedure':10)
                     else
                        write(output,'function':10);
                     if pfdeckind = standard then
                        write(output,'standard':10, key:10)
                     else
                     begin
                        write(output,'declared':10,
                               SymSpace:4,
                               ctptoint(*ord*)(next):intsize(*6*));
                        write(output,pflev,
                            SymSpace:4,
                            pfname:6);
                        if pfkind = actual then
                        begin
                            write(output,'actual':10);
                            if forwdecl then
                               write(output,'forward':10)
                            else
                               write(output,'notforward':10);
                            if externl then
                               write(output,'extern':10)
                            else
                               write(output,'not extern':10);
                        end
                        else
                            write(output,'formal':10)
                     end
                  end
            end (*case*);
            writeln(output);
            followctp(llink);
            followctp(rlink);
            followstp(idtype)
          end (*with*)
    end (*followctp*);

  begin (*printtables*)
    writeln(output);
    writeln(output);
    writeln(output);
    if fb then
       lim := 0
    else
    begin
       lim := top;
       write(output,' local')
    end;
    writeln(output,' tables ');
    writeln(output);
    marker;
    for i := top downto lim do
        followctp(display[i].fname);
    writeln(output);
    if not eol then
        write(output,SymSpace:chcnt+16)
  end (*printtables*);

  procedure genlabel(var nxtlab: integer);
  begin
     intlabel := intlabel + 1;
     nxtlab := intlabel
  end (*genlabel*);

  procedure searchlabel(var llp: lbp; level: disprange);
  var fllp: lbp; { found label entry }

  begin
     fllp := nil; { set no label found }
     llp := display[level].flabel; { index top of label list }
     while llp <> nil do
     begin { traverse }
        if llp^.labval = val.ival then
        begin { found }
           fllp := llp; { set entry found }
           llp := nil { stop }
        end
        else
           llp := llp^.nextlab { next in list }
     end;
     llp := fllp { return found entry or nil }
  end;

  procedure newlabel(var llp: lbp);
  var lbname: integer;
  begin
      with display[top] do
      begin
         getlab(llp);
         with llp^ do
         begin
            labval := val.ival;
            genlabel(lbname);
            defined := false;
            nextlab := flabel;
            labname := lbname;
            vlevel := level;
            slevel := 0;
            ipcref := false;
            minlvl := maxint;
            bact := false;
        end;
        flabel := llp
     end
  end;

  procedure prtlabels;
  var llp: lbp; { found label entry }
  begin
    writeln;
    writeln('Labels: ');
    writeln;
    llp := display[level].flabel; { index top of label list }
    while llp <> nil do
    with llp^ do
    begin { traverse }
        writeln('label: ', labval:1,
                ' defined: ', defined,
                ' internal: ', labname:1,
                ' vlevel: ', vlevel:1,
                ' slevel: ', slevel:1,
                ' ipcref: ', ipcref:1,
                ' minlvl: ', minlvl:1);
        writeln('   bact: ', bact);
        llp := llp^.nextlab { next in list }
    end
  end;

  procedure block(fsys: setofsys; fsy: symbol; fprocp: ctp);
  var
      lsy: symbol;
      stalvl: integer; { statement nesting level }

     procedure skip(fsys: setofsys);
      (*skip input string until relevant symbol found*)
     begin
        if not isEndOfSourceFile then
        begin
           while not(sy in fsys) and
               (not isEndOfSourceFile) do
              insymbol;
           if not (sy in fsys) then
              insymbol
        end
     end (*skip*) ;

     procedure constant(fsys: setofsys; var fsp: stp; var fvalu: valu);
     var
        lsp: stp;
        lcp: ctp;
        sign: (none,pos,neg);
        lvp: csp;
        i: 2..strglgth;

     begin
        lsp := nil;
        fvalu.ival := 0;
        if not(sy in constbegsys) then
        begin
           error(ErrConst);
           skip(fsys+constbegsys)
        end;
        if sy in constbegsys then
        begin
           if sy = stringconst then
           begin
              if lgth = 1 then
                 lsp := charptr
              else
              begin
                 new(lsp {,arrays  -- tagfield } );
                 lsp^.form := arrays;
                 pshstc(lsp);
                 with lsp^ do
                 begin
                    aeltype := charptr;
                    inxtype := nil;
                    size := lgth*charsize;
                    packing := true
                 end
              end;
              fvalu := val; insymbol
           end
           else
           begin
              sign := none;
              if (sy = addop) and (op in [plus,minus]) then
              begin
                 if op = plus then
                    sign := pos
                 else
                    sign := neg;
                 insymbol
              end;
              if sy = ident then
              begin
                 searchid([konst],lcp);
                 with lcp^ do
                 begin
                    lsp := idtype;
                    fvalu := values
                 end;
                 if sign <> none then
                   if lsp = intptr then
                   begin
                      if sign = neg then
                        fvalu.ival := -fvalu.ival
                   end
                 else
                   if lsp = realptr then
                   begin
                      if sign = neg then
                      begin
                         new(lvp { ,reel -- tagfield } );
                         lvp^.cclass := reel;
                         pshcst(lvp);
                         if strchr(fvalu.valp^.rval, 1) = SymMinus then
                            strchrass(lvp^.rval, 1, SymPlus)
                         else
                            strchrass(lvp^.rval, 1, SymMinus);
                         for i := 2 to digmax do
                            strchrass(lvp^.rval, i,
                         strchr(fvalu.valp^.rval, i));
                         fvalu.valp := lvp;
                     end
                  end
                  else
                     error(ErrSign);
                  insymbol;
             end
             else
               if sy = intconst then
               begin
                  if sign = neg then
                     val.ival := -val.ival;
                  lsp := intptr;
                  fvalu := val;
                  insymbol
               end
               else
               if sy = realconst then
               begin
                  if sign = neg then
                     strchrass(val.valp^.rval, 1, SymMinus);
                  lsp := realptr;
                  fvalu := val;
                  insymbol
               end
               else
               begin
                  error(ErrNumExpect);
                  skip(fsys)
               end
       end;
       if not (sy in fsys) then
       begin
          error(ErrIllSym);
          skip(fsys)
       end
      end;
      fsp := lsp
    end (*constant*) ;

    function isString(fsp: stp) : boolean; forward; { replaces 'string' }

    function comptypes(fsp1,fsp2: stp) : boolean;
      (*decide whether structures pointed at by fsp1 and fsp2 are compatible*)
    begin
      comptypes := false; { set default is false }
      { Check equal. Aliases of the same type will also be equal. }
      if fsp1 = fsp2 then comptypes := true
      else
        if (fsp1 <> nil) and (fsp2 <> nil) then
          if fsp1^.form = fsp2^.form then
            case fsp1^.form of
              scalar: ;
              { Subranges are compatible if either type is a subrange of the 
                other, or if the base type is the same. }
              subrange: comptypes := (fsp1^.rangetype = fsp2) or 
                                     (fsp2^.rangetype = fsp1) or
                                     (fsp1^.rangetype = fsp2^.rangetype);
              { Sets are compatible if they have the same base types and packed/
                unpacked status, or one of them is the empty set. The empty set
                is indicated by a nil base type, which is identical to a base
                type in error. Either way, we treat them as compatible.
                
                Set types created for set constants have a flag that disables
                packing matches. This is because set constants can be packed or
                unpacked by context. }
              power: comptypes := (comptypes(fsp1^.elset, fsp2^.elset) and 
                                    ((fsp1^.packing = fsp2^.packing) or 
                                     not fsp1^.matchpack or 
                                     not fsp2^.matchpack)) or
                                  (fsp1^.elset = nil) or (fsp2^.elset = nil);
              { Arrays are compatible if they are string types and equal in size }
              arrays: comptypes := isString(fsp1) and isString(fsp2) and
                                   (fsp1^.size = fsp2^.size );
              { Pointers, must either be the same type or aliases of the same
                type, or one must be nil. The nil pointer is indicated by a nil
                base type, which is identical to a base type in error. Either 
                way, we treat them as compatible. }
              pointer: comptypes := (fsp1^.eltype = nil) or (fsp2^.eltype = nil);
              { records and files must either be the same type or aliases of the
                same type }
              records: ;
              files:
            end (*case*)
          else (*fsp1^.form <> fsp2^.form*)
            { subranges of a base type match the base type }
            if fsp1^.form = subrange then
              comptypes := fsp1^.rangetype = fsp2
            else
              if fsp2^.form = subrange then
                comptypes := fsp1 = fsp2^.rangetype
              else comptypes := false
        else comptypes := true { one of the types is in error }
    end (*comptypes*) ;

    { check structure is, or contains, a file }
    function filecomponent(fsp: stp): boolean;
    var f: boolean;
      { tour identifier tree }
      function filecomponentre(lcp: ctp): boolean;
      var f: boolean;
      begin
        f := false; { set not file by default }
        if lcp <> nil then with lcp^ do begin
          if filecomponent(idtype) then f := true;
          if filecomponentre(llink) then f := true;
          if filecomponentre(rlink) then f := true
        end;
        filecomponentre := f
      end;
    begin
      f := false; { set not a file by default }
      if fsp <> nil then with fsp^ do case form of
        scalar:   ;
        subrange: ;
        pointer:  ;
        power:    ;
        arrays:   if filecomponent(aeltype) then f := true; 
        records:  if filecomponentre(fstfld) then f := true;
        files:    f := true;
        tagfld:   ;
        variant:  ;
      end;
      filecomponent := f
    end;


    function isString(fsp: stp) : boolean;  {was 'string'}
    var fmin, fmax: integer;
    begin isString := false;
      if fsp <> nil then
        if (fsp^.form = arrays) and fsp^.packing then begin
          { if the index is nil, either the array is a string constant or the
            index type was in error. Either way, we call it a string }
          if fsp^.inxtype = nil then fmin := 1
          else getbounds(fsp^.inxtype,fmin,fmax);
          if comptypes(fsp^.aeltype,charptr) and (fmin = 1) then isString := true
        end
    end (*isString*) ;

    { resolve all pointer references in the forward list }
    procedure resolvep;
    var ids: idstr; lcp1, lcp2: ctp; mm, fe: boolean;
    begin
      ids := id;
      fe := true;
      while fwptr <> nil do begin
        lcp1 := fwptr;
        fwptr := lcp1^.next; 
        strassfv(id, lcp1^.name);
        searchidnenm([types], lcp2, mm);
        if lcp2 <> nil then lcp1^.idtype^.eltype := lcp2^.idtype 
        else begin 
          if fe then begin error(ErrUnsatFwd); writeln(output) end;
          write('*** undefined type-id forward reference: '); 
          writev(output, lcp1^.name, prtlln); writeln;
          fe := false
        end;
        putnam(lcp1)
      end;
      id := ids
    end;

    procedure typ(fsys: setofsys; var fsp: stp; var fsize: addrrange);
    var
        lsp,lsp1,lsp2: stp;
        oldtop: disprange;
        lcp: ctp;
        lsize,displ: addrrange;
        lmin,lmax: integer;
        test, ispacked: boolean;

      procedure simpletype(fsys:setofsys; var fsp:stp; var fsize:addrrange);
      var
        lsp,lsp1: stp;
        lcp,lcp1: ctp;
        ttop: disprange;
        lcnt: integer;
        lvalu: valu;

      begin
         fsize := 1;
         if not (sy in simptypebegsys) then
         begin
            error(ErrSympType);
            skip(fsys + simptypebegsys)
         end;
         if sy in simptypebegsys then
         begin
            if sy = lparent then
            begin
               ttop := top;   (*decl. consts local to innermost block*)
               while display[top].occur <> blck do
                  top := top - 1;
               new(lsp { ,scalar,declared  -- tagfield });
               pshstc(lsp);
               with lsp^ do
               begin
                  size := intsize;
                  form := scalar;
                  scalkind := declared
               end;
               lcp1 := nil;
               lcnt := 0;
               repeat
                  insymbol;
                  if sy = ident then
                  begin
                     new(lcp { ,konst -- tagfield } );
                     ininam(lcp);
                     with lcp^ do
                     begin
                        strassvf(name, id);
                        idtype := lsp;
                        next := lcp1;
                        values.ival := lcnt;
                        klass := konst
                     end;
                     enterid(lcp);
                     lcnt := lcnt + 1;
                     lcp1 := lcp; insymbol
                  end
                  else
                     error(ErrIDExpect);
                  if not (sy in fsys + [comma,rparent]) then
                  begin
                     error(ErrIllSym);
                     skip(fsys + [comma,rparent])
                  end
               until sy <> comma;
               lsp^.fconst := lcp1;
               top := ttop;
               if sy = rparent then
                 insymbol
               else
                 error(ErrClosePar)
            end
            else
            begin
               if sy = ident then
               begin searchid([types,konst],lcp);
                    insymbol;
                    if lcp^.klass = konst then
                    begin new(lsp { ,subrange -- tagfield } );
                       pshstc(lsp);
                       with lsp^, lcp^ do
                       begin
                          rangetype := idtype;
                          form := subrange;
                          if isString(rangetype) then
                          begin
                             error(ErrSubScal);
                             rangetype := nil
                          end;
                          min := values; size := intsize
                       end;
                       if sy = range then
                          insymbol
                       else
                          error(ErrColon);
                       constant(fsys,lsp1,lvalu);
                       lsp^.max := lvalu;
                       if lsp^.rangetype <> lsp1 then
                          error(ErrIncompSub)
                    end
                    else
                    begin
                       lsp := lcp^.idtype;
                       if lsp <> nil then
                          fsize := lsp^.size
                    end
                end (*sy = ident*)
                else
                begin
                   new(lsp {,subrange -- tagfield } );
                   pshstc(lsp);
                   lsp^.form := subrange;
                   constant(fsys + [range],lsp1,lvalu);
                   if isString(lsp1) then
                   begin
                      error(ErrSubScal);
                      lsp1 := nil
                   end;
                   with lsp^ do
                   begin
                      rangetype:=lsp1;
                      min:=lvalu;
                      size:=intsize
                   end;
                   if sy = range then
                      insymbol
                   else
                      error(ErrColon);
                   constant(fsys,lsp1,lvalu);
                   lsp^.max := lvalu;
                   if lsp^.rangetype <> lsp1 then
                      error(ErrIncompSub)
                end;
                if lsp <> nil then
                with lsp^ do
                  if form = subrange then
                     if rangetype <> nil then
                       if rangetype = realptr then
                          error(ErrNotImp)
                       else
                       if min.ival > max.ival then
                          error(ErrLowAboveHi)
            end;
            fsp := lsp;
            if not (sy in fsys) then
            begin
               error(ErrIllSym);
               skip(fsys)
            end
         end
         else
            fsp := nil
      end (*simpletype*) ;

      procedure fieldlist(fsys: setofsys; var frecvar: stp);
      var
         lcp,lcp1,nxt,nxt1:
         ctp; lsp,lsp1,lsp2,lsp3,lsp4: stp;
         minsize,maxsize,lsize: addrrange;
         lvalu: valu;
         test: boolean;
         mm: boolean;

       begin
         nxt1 := nil;
         lsp := nil;
         if not (sy in (fsys+[ident,casesy])) then
         begin
            error(ErrFieldL);
            skip(fsys + [ident,casesy])
         end;
         while sy = ident do
         begin
            nxt := nxt1;
            repeat
               if sy = ident then
               begin
                  new(lcp {,field -- tagfield } );
                  ininam(lcp);
                  with lcp^ do
                  begin
                     strassvf(name, id);
                     idtype := nil;
                     next := nxt;
                     klass := field
                  end;
                  nxt := lcp;
                  enterid(lcp);
                  insymbol
               end
               else
                  error(ErrIDExpect);
               if not (sy in [comma,colon]) then
               begin
                  error(ErrIllSym);
                  skip(fsys + [comma,colon,semicolon,casesy])
               end;
               test := sy <> comma;
               if not test then
                  insymbol
            until test;
            if sy = colon then
               insymbol
            else
               error(ErrColon);
            typ(fsys + [casesy,semicolon],lsp,lsize);
            while nxt <> nxt1 do
               with nxt^ do
               begin
                  align(lsp,displ);
                  idtype := lsp;
                  fldaddr := displ;
                  nxt := next;
                  displ := displ + lsize
               end;
            nxt1 := lcp;
            while sy = semicolon do
            begin
               insymbol;
               if not (sy in fsys + [ident,casesy,semicolon]) then
               begin
                  error(ErrFieldL);
                  skip(fsys + [ident,casesy])
               end
            end
         end (*while*);
         nxt := nil;
         while nxt1 <> nil do
         with nxt1^ do
         begin
            lcp := next;
            next := nxt;
            nxt := nxt1;
            nxt1 := lcp
         end;
         if sy = casesy then
         begin
            new(lsp { ,tagfld -- tagfield } );
            pshstc(lsp);
            with lsp^ do
            begin
               tagfieldp := nil;
               fstvar := nil;
               form:=tagfld
            end;
            frecvar := lsp;
            insymbol;
            if sy = ident then 
            begin  { find possible type first }
               searchidnenm([types],lcp1,mm);
               { now set up as field id }
               new(lcp { ,field -- tagfield } );
               ininam(lcp);
               with lcp^ do
               begin
                  strassvf(name, id);
                  idtype := nil;
                  klass:=field;
                  next := nil;
                  fldaddr := displ
               end;
               insymbol;
               { If type only (undiscriminated variant), kill the id. }
               if sy = colon then
               begin
                  enterid(lcp);
                  insymbol;
                  if sy = ident then
                  begin
                     searchid([types],lcp1);
                     insymbol
                  end
                  else
                  begin
                     error(ErrIDExpect);
                     skip(fsys + [ofsy,lparent]);
                     lcp1 := nil
                  end
               end
               else
               begin
                  if mm then
                     error(ErrClass);
                  putstrs(lcp^.name); { release name string }
                  lcp^.name := nil { set no tagfield }
               end;
               if lcp1 <> nil then
               begin
                  lsp1 := lcp1^.idtype;
                  if lsp1 <> nil then
                  begin
                     align(lsp1,displ);
                     lcp^.fldaddr := displ;
                     { only allocate field if named }
                     if lcp^.name <> nil then
                        displ := displ+lsp1^.size;
                     if (lsp1^.form <= subrange) or
                         isString(lsp1) then
                     begin
                        if comptypes(realptr,lsp1) then
                           error(ErrRealForbid)
                        else if isString(lsp1) then
                           error(ErrNotImp);
                        lcp^.idtype := lsp1;
                        lsp^.tagfieldp := lcp;
                     end
                     else
                        error(ErrBadTagFld);
                 end
              end
           end
           else
           begin
              error(ErrIDExpect);
              skip(fsys + [ofsy,lparent])
           end;
           lsp^.size := displ;
           if sy = ofsy then
              insymbol
           else
              error(ErrNoOf);
           lsp1 := nil;
           minsize := displ;
           maxsize := displ;
           repeat
              lsp2 := nil;
              if not (sy in fsys + [semicolon]) then
              begin
                 repeat
                    constant(fsys + [comma,colon,lparent],lsp3,lvalu);
                    if lsp^.tagfieldp <> nil then
                       if not comptypes(lsp^.tagfieldp^.idtype,lsp3)then
                          error(ErrIncTagFld);
                    new(lsp3 { ,variant -- tagfield } );
                    pshstc(lsp3);
                    with lsp3^ do
                    begin
                       nxtvar := lsp1;
                       subvar := lsp2;
                       varval := lvalu;
                       form := variant
                    end;
                    lsp4 := lsp1;
                    while lsp4 <> nil do
                    with lsp4^ do
                    begin
                       if varval.ival = lvalu.ival then
                          error(ERRMdefRecVar);
                       lsp4 := nxtvar
                    end;
                    lsp1 := lsp3;
                    lsp2 := lsp3;
                    test := sy <> comma;
                    if not test then
                       insymbol
                 until test;
                 if sy = colon then
                    insymbol
                 else
                    error(ErrColon);
                 if sy = lparent then
                    insymbol
                 else
                    error(ErrOpenPar);
                 fieldlist(fsys + [rparent,semicolon],lsp2);
                 if displ > maxsize then
                    maxsize := displ;
                 while lsp3 <> nil do
                 begin
                    lsp4 := lsp3^.subvar;
                    lsp3^.subvar := lsp2;
                    lsp3^.size := displ;
                    lsp3 := lsp4
                 end;
                 if sy = rparent then
                 begin
                    insymbol;
                    if not (sy in fsys + [semicolon]) then
                    begin
                       error(ErrIllSym);
                       skip(fsys + [semicolon])
                    end
                 end
                 else
                    error(ErrClosePar);
              end;
              test := sy <> semicolon;
              if not test then
              begin
                 displ := minsize;
                 insymbol
              end
           until test;
           displ := maxsize;
           lsp^.fstvar := lsp1;
        end
        else
           frecvar := nil
    end (*fieldlist*) ;

    begin (*typ*)
      if not (sy in typebegsys) then
      begin
         error(ErrType);
         skip(fsys + typebegsys)
      end;
      if sy in typebegsys then
      begin
         if sy in simptypebegsys then
            simpletype(fsys,fsp,fsize)
         else
    (*^*)   if sy = arrow then
            begin
               new(lsp { ,pointer -- tagfield });
               pshstc(lsp);
               fsp := lsp;
               with lsp^ do
               begin
                  eltype := nil;
                  size := ptrsize;
                  form:=pointer
               end;
               insymbol;
               if sy = ident then
               begin { forward reference everything }
                  new(lcp { ,types -- tagfield } );
                  ininam(lcp);
                  with lcp^ do
                  begin
                     strassvf(name,id);
                     idtype := lsp;
                     next := fwptr;
                     klass := types
                  end;
                  fwptr := lcp;
                  insymbol;
               end
               else
                  error(ErrIDExpect);
            end
            else
               begin
                  ispacked := false; { set not packed by default }
                  if sy = packedsy then
                  begin
                     insymbol;
                     ispacked := true; { packed }
                     if not (sy in typedels) then
                     begin
                        error(ErrType);
                        skip(fsys + typedels)
                     end
                  end;
    (*array*)     if sy = arraysy then
                  begin
                     insymbol;
                     if sy = lbrack then
                        insymbol
                     else
                        error(ErrOpnBrack);
                     lsp1 := nil;
                     repeat
                        new(lsp { ,arrays -- tagfield } );
                        pshstc(lsp);
                        with lsp^ do
                        begin
                           aeltype := lsp1;
                           inxtype := nil;
                           form:=arrays;
                           packing := ispacked
                        end;
                        lsp1 := lsp;
                        simpletype(fsys + [comma,rbrack,ofsy],lsp2,lsize);
                        lsp1^.size := lsize;
                        if lsp2 <> nil then
                          if lsp2^.form <= subrange then
                          begin
                             if lsp2 = realptr then
                             begin
                                error(ErrRealForbid);
                                lsp2 := nil
                             end
                             else
                             if lsp2 = intptr then
                             begin
                                error(ErrIndex);
                                lsp2 := nil
                             end;
                             lsp^.inxtype := lsp2
                         end
                         else
                         begin
                             error(ErrIdxNoScal);
                             lsp2 := nil
                         end;
                         test := sy <> comma;
                         if not test then
                            insymbol
                     until test;
                     if sy = rbrack then
                        insymbol
                     else
                        error(ErrClsBrack);
                     if sy = ofsy then
                        insymbol
                     else
                        error(ErrNoOf);
                     typ(fsys,lsp,lsize);
                     repeat
                        with lsp1^ do
                        begin
                           lsp2 := aeltype;
                           aeltype := lsp;
                           if inxtype <> nil then
                           begin
                              getbounds(inxtype,lmin,lmax);
                              align(lsp,lsize);
                              lsize := lsize*(lmax - lmin + 1);
                              size := lsize
                           end
                        end;
                        lsp := lsp1; lsp1 := lsp2
                    until lsp1 = nil
                  end
                  else
    (*record*)      if sy = recordsy then
                    begin
                      insymbol;
                      oldtop := top;
                      if top < displimit then
                      begin
                         top := top + 1;
                         with display[top] do
                         begin
                            fname := nil;
                            flabel := nil;
                            fconst := nil;
                            fstruct := nil;
                            occur := rec
                         end
                      end
                      else
                         error(250);
                      displ := 0;
                      fieldlist(fsys-[semicolon]+[endsy],lsp1);
                      new(lsp { ,records -- tagfield } );
                      with lsp^ do
                      begin
                         fstfld := display[top].fname;
                         display[top].fname := nil;
                         recvar := lsp1;
                         size := displ;
                         form := records;
                         packing := ispacked;
                         recyc := display[top].fstruct;
                         display[top].fstruct := nil
                      end;
                      putdsps(oldtop); top := oldtop;
                      { register the record late because of the purge above }
                      pshstc(lsp);
                      if sy = endsy then
                         insymbol
                      else
                         error(ErrNoEnd)
                   end
                   else
    (*set*)        if sy = setsy then
                   begin
                      insymbol;
                      if sy = ofsy then
                         insymbol
                      else
                         error(ErrNoOf);
                      simpletype(fsys,lsp1,lsize);
                      if lsp1 <> nil then
                         if lsp1^.form > subrange then
                         begin
                            error(ErrBaseNoScal);
                            lsp1 := nil
                         end
                         else
                            if lsp1 = realptr then
                            begin
                               error(ErrBaseReal);
                               lsp1 := nil
                            end
                            else
                            if lsp1 = intptr then
                            begin
                               error(ErrBaseSet);
                               lsp1 := nil
                            end
                            else
                            begin
                               getbounds(lsp1,lmin,lmax);
                               if (lmin < setlow) or (lmax > sethigh) then
                                  error(ErrBaseSet);
                            end;
                            new(lsp { ,power -- tagfield } );
                            pshstc(lsp);
                            with lsp^ do
                            begin
                               elset:=lsp1;
                               size:=setsize;
                               form:=power;
                               packing := ispacked;
                               matchpack := true
                            end;
                    end
                    else
    (*file*)        if sy = filesy then
                    begin
                       insymbol;
                       if sy = ofsy then
                          insymbol
                       else
                          error(ErrNoOf);
                       typ(fsys,lsp1,lsize);
                       if filecomponent(lsp1) then
                          error(ErrNestFile);
                       new(lsp { ,files -- tagfield } );
                       pshstc(lsp);
                       with lsp^ do
                       begin
                          filtype := lsp1;
                          size := filesize+lsize;
                          form := files;
                          packing := ispacked
                       end
                    end;
                    fsp := lsp
              end;
          if not (sy in fsys) then
            begin error(ErrIllSym); skip(fsys) end
        end
      else fsp := nil;
      if fsp = nil then fsize := 1 else fsize := fsp^.size
    end (*typ*) ;

    procedure labeldeclaration;
      var llp: lbp;
          test: boolean;
    begin
       repeat
          if sy = intconst then
          begin
             searchlabel(llp, top); { search preexisting label }
             if llp <> nil then
                error(ErrMultiDecl) { multideclared label }
             else
                newlabel(llp);
             insymbol
          end
          else
             error(ErrIntExpect);
          if not ( sy in fsys + [comma, semicolon] ) then
          begin
             error(ErrIllSym);
             skip(fsys+[comma,semicolon])
          end;
          test := sy <> comma;
          if not test then
             insymbol
       until test;
       if sy = semicolon then
          insymbol
       else
          error(ErrNoSemi)
    end (* labeldeclaration *) ;

    procedure constdeclaration;
      var lcp: ctp; lsp: stp; lvalu: valu;
    begin
      if sy <> ident then
        begin error(ErrIDExpect); skip(fsys + [ident]) end;
      while sy = ident do
        begin new(lcp { ,konst -- tagfield } ); ininam(lcp);
          with lcp^ do
            begin strassvf(name, id); idtype := nil; next := nil; klass:=konst end;
          insymbol;
          if (sy = relop) and (op = eqop) then
             insymbol
          else
             error(ErrEqualExpect);
          constant(fsys + [semicolon],lsp,lvalu);
          enterid(lcp);
          lcp^.idtype := lsp; lcp^.values := lvalu;
          if sy = semicolon then
            begin insymbol;
              if not (sy in fsys + [ident]) then
                begin error(ErrIllSym); skip(fsys + [ident]) end
            end
          else error(ErrNoSemi)
        end
    end (*constdeclaration*) ;

    procedure typedeclaration;
      var lcp,lcp1,lcp2,lcp3: ctp; lsp: stp; lsize: addrrange;
    begin
      if sy <> ident then
        begin error(ErrIDExpect); skip(fsys + [ident]) end;
      while sy = ident do
        begin new(lcp { ,types -- tagfield } ); ininam(lcp);
          with lcp^ do
            begin strassvf(name, id); idtype := nil; klass := types end;
          insymbol;
          if (sy = relop) and (op = eqop) then insymbol else error(ErrEqualExpect);
          typ(fsys + [semicolon],lsp,lsize);
          enterid(lcp);
          lcp^.idtype := lsp;
          if sy = semicolon then
            begin insymbol;
              if not (sy in fsys + [ident]) then
                begin error(ErrIllSym); skip(fsys + [ident]) end
            end
          else error(ErrNoSemi)
        end;
      resolvep
    end (*typedeclaration*) ;

    procedure vardeclaration;
      var lcp,nxt: ctp; lsp: stp; lsize: addrrange;
          test: boolean;
    begin nxt := nil;
      repeat
        repeat
          if sy = ident then
            begin new(lcp { ,vars -- tagfield } ); ininam(lcp);
              with lcp^ do
               begin strassvf(name, id); next := nxt; klass := vars;
                  idtype := nil; vkind := actual; vlev := level
                end;
              enterid(lcp);
              nxt := lcp;
              insymbol;
            end
          else error(ErrIDExpect);
          if not (sy in fsys + [comma,colon] + typedels) then
            begin error(ErrIllSym); skip(fsys+[comma,colon,semicolon]+typedels) end;
          test := sy <> comma;
          if not test then insymbol
        until test;
        if sy = colon then insymbol else error(ErrColon);
        typ(fsys + [semicolon] + typedels,lsp,lsize);
        while nxt <> nil do
          with  nxt^ do
            begin align(lsp,lc);
              idtype := lsp; vaddr := lc;
              lc := lc + lsize; nxt := next
            end;
        if sy = semicolon then
          begin insymbol;
            if not (sy in fsys + [ident]) then
              begin error(ErrIllSym); skip(fsys + [ident]) end
          end
        else error(ErrNoSemi)
      until (sy <> ident) and not (sy in typedels);
      resolvep
    end (*vardeclaration*) ;

    procedure procdeclaration(fsy: symbol);
      var oldlev: 0..maxlevel; lcp,lcp1: ctp; lsp: stp;
          forw: boolean; oldtop: disprange;
          llc,lcm: addrrange; lbname: integer; {markp: marktype;}

      procedure pushlvl(forw: boolean; lcp: ctp);
      begin
        if level < maxlevel then level := level + 1 else error(251);
        if top < displimit then
          begin top := top + 1;
            with display[top] do
              begin
                if forw then fname := lcp^.pflist
                else fname := nil;
                flabel := nil; fconst := nil; fstruct := nil;
                occur := blck;
                bname := lcp
              end
          end
        else error(250);
      end;

      procedure parameterlist(fsy: setofsys; var fpar: ctp);
      var lcp,lcp1,lcp2,lcp3: ctp;
          lsp: stp;
          lkind: idkind;
          llc,lsize: addrrange;
          count: integer;
          oldlev: 0..maxlevel;
          oldtop: disprange;
          lcs: addrrange;
          test: boolean;

      begin
         lcp1 := nil;
         if not (sy in fsy + [lparent]) then
         begin
            error(7);
            skip(fsys + fsy + [lparent])
         end;
         if sy = lparent then
         begin
            if forw then
               error(119);
            insymbol;
            if not (sy in [ident,varsy,procsy,funcsy]) then
            begin
               error(7);
               skip(fsys + [ident,rparent])
            end;
            while sy in [ident,varsy,procsy,funcsy] do
            begin
               if sy = procsy then
               begin
                  insymbol; lcp := nil;
                  if sy = ident then
                  begin
                     new(lcp { ,proc,declared,formal -- tagfield } );
                     ininam(lcp);
                     with lcp^ do
                     begin
                        strassvf(name, id);
                        idtype := nil;
                        next := lcp1;
                        pflev := level (*beware of parameter procedures*);
                        klass:=proc;pfdeckind:=declared;
                        pfkind:=formal;
                        pfaddr := lc;
                        keep := true
                     end;
                     enterid(lcp);
                     lcp1 := lcp;
                     align(parmptr,lc);
                     lc := lc+ptrsize*2; { mp and addr }
                     insymbol
                 end
                 else
                    error(ErrIDExpect);
                 oldlev := level;
                 oldtop := top;
                 pushlvl(false, lcp);
                 lcs := lc;
                 parameterlist([semicolon,rparent],lcp2);
                 lc := lcs;
                 if lcp <> nil then
                    lcp^.pflist := lcp2;
                 if not (sy in fsys+[semicolon,rparent]) then
                 begin
                    error(7);
                    skip(fsys+[semicolon,rparent])
                 end;
                 level := oldlev;
                 putdsps(oldtop);
                 top := oldtop
              end
              else
              begin
                 if sy = funcsy then
                 begin
                    lcp2 := nil;
                    insymbol;
                    if sy = ident then
                    begin
                       new(lcp { ,func,declared,formal -- tagfield } );
                       ininam(lcp);
                       with lcp^ do
                       begin
                          strassvf(name, id);
                          idtype := nil;
                          next := lcp1;
                          pflev := level (*beware param funcs*);
                          klass:=func;pfdeckind:=declared;
                          pfkind:=formal;
                          pfaddr:=lc;
                          keep := true
                       end;
                       enterid(lcp);
                       lcp1 := lcp;
                       align(parmptr,lc);
                       lc := lc+ptrsize*2; { mp and addr }
                       insymbol;
                    end
                    else
                        error(ErrIDExpect);
                    oldlev := level;
                    oldtop := top;
                    pushlvl(false, lcp);
                    lcs := lc;
                    parameterlist([colon,semicolon,rparent],lcp2);
                    lc := lcs;
                    if lcp <> nil then
                      lcp^.pflist := lcp2;
                    if not (sy in fsys+[colon]) then
                    begin
                       error(7);
                       skip(fsys+[comma,semicolon,rparent])
                    end;
                    if sy = colon then
                    begin
                       insymbol;
                       if sy = ident then
                       begin
                          searchid([types],lcp2);
                          lsp := lcp2^.idtype;
                          lcp^.idtype := lsp;
                          if lsp <> nil then
                             if not(lsp^.form in[scalar,subrange,pointer]) then
                             begin
                                error(120);
                                lsp := nil
                             end;
                          insymbol
                       end
                       else
                          error(ErrIDExpect);
                       if not (sy in fsys + [semicolon,rparent]) then
                       begin
                          error(7);
                          skip(fsys+[semicolon,rparent])
                       end
                    end
                    else
                       error(ErrColon);
                    level := oldlev;
                    putdsps(oldtop);
                    top := oldtop
                    end
                    else
                      begin
                        if sy = varsy then
                          begin lkind := formal; insymbol end
                        else lkind := actual;
                        lcp2 := nil;
                        count := 0;
                        repeat
                          if sy = ident then
                            begin new(lcp { ,vars -- tagfield } ); ininam(lcp);
                              with lcp^ do
                                begin strassvf(name,id); idtype:=nil; klass:=vars;
                                  vkind := lkind; next := lcp2; vlev := level; 
                                  keep := true
                                end;
                              enterid(lcp);
                              lcp2 := lcp; count := count+1;
                              insymbol;
                            end;
                          if not (sy in [comma,colon] + fsys) then
                            begin error(7);skip(fsys+[comma,semicolon,rparent])
                            end;
                          test := sy <> comma;
                          if not test then insymbol
                        until test;
                        if sy = colon then
                          begin insymbol;
                            if sy = ident then
                              begin searchid([types],lcp);
                                lsp := lcp^.idtype;
                                lsize := ptrsize;
                                if lsp <> nil then
                                  if lkind=actual then
                                    if lsp^.form<=power then lsize := lsp^.size
                                    else if lsp^.form=files then error(121);
                                align(parmptr,lsize);
                                lcp3 := lcp2;
                                align(parmptr,lc);
                                lc := lc+count*lsize;
                                llc := lc;
                                while lcp2 <> nil do
                                  begin lcp := lcp2;
                                    with lcp2^ do
                                      begin idtype := lsp;
                                        llc := llc-lsize;
                                        vaddr := llc;
                                      end;
                                    lcp2 := lcp2^.next
                                  end;
                                lcp^.next := lcp1; lcp1 := lcp3;
                                insymbol
                              end
                            else error(ErrIDExpect);
                            if not (sy in fsys + [semicolon,rparent]) then
                              begin error(7);skip(fsys+[semicolon,rparent])end
                          end
                        else error(ErrColon);
                      end;
                 end;
                 if sy = semicolon then
                 begin
                    insymbol;
                    if not (sy in fsys + [ident,varsy,procsy,funcsy]) then
                    begin
                       error(7);
                       skip(fsys + [ident,rparent])
                    end
                 end
            end (*while*) ;
            if sy = rparent then
            begin insymbol;
               if not (sy in fsy + fsys) then
               begin
                  error(ErrIllSym);
                  skip(fsy + fsys)
               end
            end
            else
               error(ErrClosePar);
            lcp3 := nil;
            (*reverse pointers and reserve local
              cells for copies of multiple values*)
             while lcp1 <> nil do
             with lcp1^ do
             begin
                lcp2 := next;
                next := lcp3;
                if klass = vars then
                   if idtype <> nil then
                     if (vkind=actual)and(idtype^.form>power) then
                     begin
                        align(idtype,lc);
                        vaddr := lc;
                        lc := lc+idtype^.size;
                     end;
                lcp3 := lcp1; lcp1 := lcp2
             end;
             fpar := lcp3
         end
         else
            fpar := nil
      end (*parameterlist*) ;

    begin (*procdeclaration*)
      llc := lc; lc := lcaftermarkstack; forw := false;
      if sy = ident then
        begin searchsection(display[top].fname,lcp); (*decide whether forw.*)
          if lcp <> nil then
            begin
              if lcp^.klass = proc then
                forw := lcp^.forwdecl and(fsy=procsy)and(lcp^.pfkind=actual)
              else
                if lcp^.klass = func then
                  forw:=lcp^.forwdecl and(fsy=funcsy)and(lcp^.pfkind=actual)
                else forw := false;
              if not forw then error(160)
            end;
          if not forw then
            begin
              if fsy = procsy then new(lcp { ,proc,declared,actual -- tagfield } )
              else new(lcp { ,func,declared,actual -- tagfield } );
              ininam(lcp);
              with lcp^ do
                begin strassvf(name, id); idtype := nil;
                  externl := false; pflev := level; genlabel(lbname);
                  pfdeckind := declared; pfkind := actual; pfname := lbname;
                  if fsy = procsy then klass := proc
                  else klass := func
                end;
              enterid(lcp)
            end
          else
            begin lcp1 := lcp^.pflist;
              while lcp1 <> nil do
                begin
                  with lcp1^ do
                    if klass = vars then
                      if idtype <> nil then
                        begin lcm := vaddr + idtype^.size;
                          if lcm > lc then lc := lcm
                        end;
                  lcp1 := lcp1^.next
                end
            end;
          insymbol
        end
      else
        begin error(ErrIDExpect); lcp := ufctptr end;
      oldlev := level; oldtop := top;
      pushlvl(forw, lcp);
      if fsy = procsy then
        begin parameterlist([semicolon],lcp1);
          if not forw then lcp^.pflist := lcp1
        end
      else
        begin parameterlist([semicolon,colon],lcp1);
          if not forw then lcp^.pflist := lcp1;
          if sy = colon then
            begin insymbol;
              if sy = ident then
                begin if forw then error(122);
                  searchid([types],lcp1);
                  lsp := lcp1^.idtype;
                  lcp^.idtype := lsp;
                  if lsp <> nil then
                    if not (lsp^.form in [scalar,subrange,pointer]) then
                      begin error(120); lcp^.idtype := nil end;
                  insymbol
                end
              else begin error(ErrIDExpect); skip(fsys + [semicolon]) end
            end
          else
            if not forw then error(123)
        end;
      if sy = semicolon then insymbol else error(ErrNoSemi);
      if (sy = ident) and strequri('forward  ', id) then
        begin
          if forw then error(161)
          else lcp^.forwdecl := true;
          insymbol;
          if sy = semicolon then insymbol else error(ErrNoSemi);
          if not (sy in fsys) then
            begin error(ErrIllSym); skip(fsys) end
        end
      else
        begin lcp^.forwdecl := false;
          { mark(markp); }
          repeat block(fsys,semicolon,lcp);
            if sy = semicolon then
              begin if prtables then printtables(false); insymbol;
                if not (sy in [beginsy,procsy,funcsy]) then
                  begin error(ErrIllSym); skip(fsys) end
              end
            else error(ErrNoSemi)
          until (sy in [beginsy,procsy,funcsy]) or isEndOfSourceFile;
          { release(markp); } (* return local entries on runtime heap *)
        end;
      level := oldlev; putdsps(oldtop); top := oldtop; lc := llc;
    end (*procdeclaration*) ;

    procedure body(fsys: setofsys);
    const
       cstoccmax=4000;
       cixmax=10000;

    {type oprange, cstptr etc. was used by P5 instructions; moved above them }

    var
       llcp:ctp;
       saveid:idstr;
       cstptr: array [1..cstoccmax] of csp;
       cstptrix: 0..cstoccmax;
          (*allows referencing of noninteger constants by an index
           (instead of a pointer), which can be stored in the p2-field
           of the instruction record until writeout.
           --> procedure load, procedure writeout*)
          entname, segsize: integer;

          lcmax,llc1: addrrange; lcp: ctp;
          llp: lbp;
          fp: extfilep;
          test: boolean;

          stacktop, topnew, topmax: integer;

      { add statement level }
      procedure addlvl;
      begin
        stalvl := stalvl+1
      end;

      { remove statement level }
      procedure sublvl;
      var llp: lbp;
      begin
         stalvl := stalvl-1;
         { traverse label list for current block and remove any label from
           active status whose statement block has closed }
         llp := display[top].flabel;
         while llp <> nil do with llp^ do begin
           if slevel > stalvl then bact := false;
           llp := nextlab { link next }
         end
      end;

      (* ---------------- P5 Instructions ------------------------------------ *)


       procedure mes(i: integer);
       begin
            topnew := topnew + cdx[i]*maxstack;
            if topnew > topmax then
               topmax := topnew
       end;

       procedure putic;
       begin
          if ic mod 10 = 0 then
             writeln(prr,commentprefix,ic:5)
       end;

       { Generate P5 0 argument instruction }
       procedure gen0(fop: oprange);
       begin
         if prcode then
         	   begin
         	      putic;
                       writeln(prr,mn[fop]:4)
         	   end;
         ic := ic + 1;
         mes(fop)
       end (*gen0*) ;

       { Generate P5 1 argument instruction }
       procedure gen1(fop: oprange; fp2: integer);
       var
           k, j: integer;
           p: strvsp;

       begin
         if prcode then
           begin
              putic;
              write(prr,mn[fop]:4);
              if fop = 30 then
              begin
                 writeln(prr,sna[fp2]:12);
                 topnew := topnew + pdx[fp2]*maxstack;
                 if topnew > topmax then
                    topmax := topnew
              end
              else
              begin
                 if fop = 38 then
                 begin
                    with cstptr[fp2]^ do
                    begin
                       p := sval;
                       j := 1;
                       write(prr,SymSpace,slgth:4,' ''');
                       for k := 1 to lenpv(p) do
                       begin
                          if p^.str[j] = SymQuote then
                             write(prr, '''''')
                          else
                             write(prr,p^.str[j]:1);
                          j := j+1;
                          if j > varsqt then
                          begin
                             p := p^.next;
                             j := 1
                          end
                      end
                   end;
                   writeln(prr,SymQuote)
                end
                else if fop = PINSTret then
                   writeln(prr,chr(fp2))
                else if fop = PINSTcip then
                   writeln(prr,fp2:4)
                else
                   writeln(prr,fp2:12);
                mes(fop)
             end
          end;
          ic := ic + 1
      end (*gen1*) ;

      { Generate p5 2 argument instructions }
      procedure gen2(fop: oprange; fp1,fp2: integer);
      var
          k : integer;

      begin
         if prcode then
         begin
            putic;
            write(prr,mn[fop]:4);
            case fop of
               45,50,54,PINSTstr,PINSTlip,62,63:
                   writeln(prr,SymSpace,fp1:3,fp2:8);
               47,48,49,52,53,55:
                   begin
                      write(prr,chr(fp1));
                      if chr(fp1) = 'm' then
                         write(prr,SymSpace,fp2:11);
                      writeln(prr)
                   end;
               PINSTldc:
                   case fp1 of
                      1: writeln(prr,'i ',fp2);
                      2: begin
                           write(prr,'r ');
                           with cstptr[fp2]^ do
                              writev(prr,rval,lenpv(rval));
                           writeln(prr)
                         end;
                      3: writeln(prr,'b ',fp2);
                      4: writeln(prr,'n');
                      6: writeln(prr,'c ''':3,chr(fp2),SymQuote);
                      5: begin
                           write(prr,SymOpenPar);
                           with cstptr[fp2]^ do
                           for k := setlow to sethigh do
                            (* increased for testing [sam] *)
                              if k in pval then
                                 write(prr,k:7(*3*));
                           writeln(prr,SymClosePar)
                         end
                  end; { Case fp1 }
           end; {case fop }
        end; {if prcode}
        ic := ic + 1;
        mes(fop)
      end (*gen2*) ;

      procedure gentypindicator(fsp: stp);
      begin
        if fsp<>nil then
           with fsp^ do
           case form of
               scalar: if fsp=intptr then
                          write(prr,'i')
                       else
                       if fsp=boolptr then
                          write(prr,'b')
                       else
                       if fsp=charptr then
                          write(prr,'c')
                       else
                       if scalkind = declared then
                          write(prr,'i')
                       else
                          write(prr,'r');
              subrange: gentypindicator(rangetype);
              pointer:  write(prr,'a');
              power:    write(prr,'s');
              records,arrays: write(prr,'m');
              files:    write(prr,'a');
              tagfld,variant: error(500)
           end
       end (*typindicator*);

       procedure gen0t(fop: oprange; fsp: stp);
       begin
         if prcode then
         begin
            putic;
            write(prr,mn[fop]:4);
            gentypindicator(fsp);
            writeln(prr);
         end;
         ic := ic + 1;
         mes(fop)
       end (*gen0t*);

       procedure gen1t(fop: oprange; fp2: integer; fsp: stp);
       begin
          if prcode then
          begin
             putic;
             write(prr,mn[fop]:4);
             gentypindicator(fsp);
             writeln(prr,SymSpace,fp2:11)
          end;
          ic := ic + 1;
          mes(fop)
       end (*gen1t*);

       procedure gen2t(fop: oprange; fp1,fp2: integer; fsp: stp);
       begin
         if prcode then
         begin
            putic;
            write(prr,mn[fop]: 4);
            gentypindicator(fsp);
            writeln(prr,SymSpace, fp1:3+5*ord(abs(fp1)>99),fp2:11);
         end;
         ic := ic + 1;
         mes(fop)
       end (*gen2t*);

       { P5 Load instruction }
       procedure load;
       begin
         with gattr do
           if typtr <> nil then
           begin
              case kind of
                 cst:   if (typtr^.form = scalar) and
                           (typtr <> realptr) then
                          if typtr = boolptr then
                             gen2(PINSTldc(*ldc*),3,cval.ival)
                          else
                             if typtr=charptr then
                                gen2(PINSTldc(*ldc*),6,cval.ival)
                             else
                                gen2(PINSTldc(*ldc*),1,cval.ival)
                        else
                          if typtr = nilptr then
                             gen2(PINSTldc(*ldc*),4,0)
                          else
                            if cstptrix >= cstoccmax then
                               error(254)
                            else
                              begin
                                 cstptrix := cstptrix + 1;
                                 cstptr[cstptrix] := cval.valp;
                                 if typtr = realptr then
                                    gen2(PINSTldc(*ldc*),2,cstptrix)
                                 else
                                    gen2(PINSTldc(*ldc*),5,cstptrix)
                              end;
                 varbl: case access of
                          drct:  if vlevel<=1 then
                                    gen1t(39(*ldo*),dplmt,typtr)
                                 else
                                    gen2t(54(*lod*),level-vlevel,dplmt,typtr);
                          indrct: gen1t(35(*ind*),idplmt,typtr);
                          inxd:   error(400)
                        end;
                 expr:
               end;
               kind := expr
             end
       end (*load*) ;

       { P5 Store Instruction }
       procedure store(var fattr: attr);
       begin
          with fattr do
          if typtr <> nil then
             case access of
               drct:   if vlevel <= 1 then gen1t(43(*sro*),dplmt,typtr)
                       else gen2t(PINSTstr(*str*),level-vlevel,dplmt,typtr);
               indrct: if idplmt <> 0 then error(400)
                       else gen0t(26(*sto*),typtr);
               inxd:   error(400)
             end
       end (*store*) ;

       { P5 Load Address }
       procedure loadaddress;
       begin
         with gattr do
           if typtr <> nil then
             begin
               case kind of
                 cst:   if isString(typtr) then
                          if cstptrix >= cstoccmax then error(254)
                          else
                            begin cstptrix := cstptrix + 1;
                              cstptr[cstptrix] := cval.valp;
                              gen1(38(*lca*),cstptrix)
                            end
                        else error(400);
                 varbl: case access of
                          drct:   if vlevel <= 1 then gen1(37(*lao*),dplmt)
                                  else gen2(50(*lda*),level-vlevel,dplmt);
                          indrct: if idplmt <> 0 then
                                    gen1t(34(*inc*),idplmt,nilptr);
                          inxd:   error(400)
                        end;
                 expr:  error(400)
               end;
               kind := varbl; access := indrct; idplmt := 0
             end
       end (*loadaddress*) ;

       { P5 False Jump - test and branch if test fails }
       procedure genfjp(faddr: integer);
       begin
         load;
         if gattr.typtr <> nil then
           if gattr.typtr <> boolptr then
              error(144);
         if prcode then
         begin
           putic;
           writeln(prr,mn[33]:4,' l':8,faddr:4)
         end;
         ic := ic + 1;
         mes(33)
       end (*genfjp*) ;

       { P5 Instruction }
       procedure genujpxjp(fop: oprange; fp2: integer);
       begin
          if prcode then
          begin
             putic;
             writeln(prr, mn[fop]:4, ' l':8,fp2:4)
          end;
          ic := ic + 1;
          mes(fop)
       end (*genujpxjp*);

       { P5 Instruction }
       procedure genipj(fop: oprange; fp1, fp2: integer);
       begin
        if prcode then
           begin
              putic;
              writeln(prr, mn[fop]:4,fp1:4,' l':8,fp2:4)
           end;
           ic := ic + 1;
           mes(fop)
       end (*genujpxjp*);

       { P5 Instruction }
       procedure gencupent(fop: oprange; fp1,fp2: integer);
       begin
          if prcode then
          begin
             putic;
             if fop = 32 then
             begin { create ents or ente instructions }
                if fp1 = 1 then
                   writeln(prr,mn[fop]:4,'s','l':8,fp2:4)
                else
                   writeln(prr,mn[fop]:4,'e','l':8,fp2:4)
             end
             else
                writeln(prr,mn[fop]:4,fp1:4,'l':4,fp2:4)
         end;
         ic := ic + 1;
         mes(fop)
       end;

       { p5 Instruction }
       procedure genlpa(fp1,fp2: integer);
       begin
          if prcode then
          begin
             putic;
             writeln(prr,mn[PINSTlpa]:4,fp2:4,'l':4,fp1:4)
          end;
          ic := ic + 1;
          mes(PINSTlpa)
       end (*genlpa*);

       { P5 Instruction }
       procedure checkbnds(fsp: stp);
         var lmin,lmax: integer;
       begin
         if fsp <> nil then
           if fsp <> intptr then
             if fsp <> realptr then
               if fsp^.form <= subrange then
                 begin
                   getbounds(fsp,lmin,lmax);
                   gen2t(45(*chk*),lmin,lmax,fsp)
                 end
       end (*checkbnds*);

       { Write P5 Label }
       procedure putlabel(labname: integer);
       begin
          if prcode then
            writeln(prr, 'l', labname:4)
       end (*putlabel*);

      (* ---------------- END of P5 Instructions ----------------------------- *)


      procedure statement(fsys: setofsys);
        var lcp: ctp; llp: lbp;
        
        procedure expression(fsys: setofsys); forward;

        procedure selector(fsys: setofsys; fcp: ctp);
        var lattr: attr; lcp: ctp; lsize: addrrange; lmin,lmax: integer;
        function schblk(fcp: ctp): boolean;
        var i: disprange; f: boolean;
        begin
           f := false;
           for i := level downto 2 do if display[i].bname = fcp then f := true;
           schblk := f
        end;
        begin
          with fcp^, gattr do
            begin typtr := idtype; kind := varbl;
              case klass of
                vars:
                  if vkind = actual then
                    begin access := drct; vlevel := vlev;
                      dplmt := vaddr
                    end
                  else
                    begin gen2t(54(*lod*),level-vlev,vaddr,nilptr);
                      access := indrct; idplmt := 0
                    end;
                field:
                  with display[disx] do
                    if occur = crec then
                      begin access := drct; vlevel := clev;
                        dplmt := cdspl + fldaddr
                      end
                    else
                      begin
                        if level = 1 then gen1t(39(*ldo*),vdspl,nilptr)
                        else gen2t(54(*lod*),0,vdspl,nilptr);
                        access := indrct; idplmt := fldaddr
                      end;
                func:
                  if pfdeckind = standard then
                    begin error(150); typtr := nil end
                  else
                    begin
                      if pfkind = formal then error(151)
                      else
                        if not schblk(fcp) then error(192);
                        begin access := drct; vlevel := pflev + 1;
                          dplmt := 0   (*impl. relat. addr. of fct. result*)
                        end
                    end
              end (*case*)
            end (*with*);
          if not (sy in selectsys + fsys) then
            begin error(59); skip(selectsys + fsys) end;
          while sy in selectsys do
            begin
        (*[*) if sy = lbrack then
                begin
                  repeat lattr := gattr;
                    with lattr do
                      if typtr <> nil then
                        if typtr^.form <> arrays then
                          begin error(138); typtr := nil end;
                    loadaddress;
                    insymbol; expression(fsys + [comma,rbrack]);
                    load;
                    if gattr.typtr <> nil then
                      if gattr.typtr^.form<>scalar then error(ErrIdxNoScal)
                      else if not comptypes(gattr.typtr,intptr) then
                             gen0t(58(*ord*),gattr.typtr);
                    if lattr.typtr <> nil then
                      with lattr.typtr^ do
                        begin
                          if comptypes(inxtype,gattr.typtr) then
                            begin
                              if inxtype <> nil then
                                begin getbounds(inxtype,lmin,lmax);
                                  if debug then
                                    gen2t(45(*chk*),lmin,lmax,intptr);
                                  if lmin>0 then gen1t(31(*dec*),lmin,intptr)
                                  else if lmin<0 then
                                    gen1t(34(*inc*),-lmin,intptr);
                                  (*or simply gen1(31,lmin)*)
                                end
                            end
                          else error(139);
                          with gattr do
                            begin typtr := aeltype; kind := varbl;
                              access := indrct; idplmt := 0
                            end;
                          if gattr.typtr <> nil then
                            begin
                              lsize := gattr.typtr^.size;
                              align(gattr.typtr,lsize);
                              gen1(36(*ixa*),lsize)
                            end
                        end
                  until sy <> comma;
                  if sy = rbrack then insymbol else error(ErrClsBrack)
                end (*if sy = lbrack*)
              else
        (*.*)   if sy = period then
                  begin
                    with gattr do
                      begin
                        if typtr <> nil then
                          if typtr^.form <> records then
                            begin error(140); typtr := nil end;
                        insymbol;
                        if sy = ident then
                          begin
                            if typtr <> nil then
                              begin searchsection(typtr^.fstfld,lcp);
                                if lcp = nil then
                                  begin error(152); typtr := nil end
                                else
                                  with lcp^ do
                                    begin typtr := idtype;
                                      case access of
                                        drct:   dplmt := dplmt + fldaddr;
                                        indrct: idplmt := idplmt + fldaddr;
                                        inxd:   error(400)
                                      end
                                    end
                              end;
                            insymbol
                          end (*sy = ident*)
                        else error(ErrIDExpect)
                      end (*with gattr*)
                  end (*if sy = period*)
                else
        (*^*)     begin
                    if gattr.typtr <> nil then
                      with gattr,typtr^ do
                        if form = pointer then
                          begin load; typtr := eltype;
                            if debug then gen2t(45(*chk*),1,maxaddr,nilptr);
                            with gattr do
                              begin kind := varbl; access := indrct;
                                idplmt := 0
                              end
                          end
                        else
                          if form = files then begin loadaddress;
                             { generate buffer validate for file }
                             if typtr = textptr then gen0( PINSTfbv )
                             else begin
                               gen2(PINSTldc(*ldc*),1,filtype^.size);
                               gen0(PINSTfvb(*fvb*))
                             end;
                             { index buffer }
                             gen1t(34(*inc*),fileidsize,gattr.typtr);
                             typtr := filtype;
                          end else error(141);
                    insymbol
                  end;
              if not (sy in fsys + selectsys) then
                begin error(ErrIllSym); skip(fsys + selectsys) end
            end (*while*)
        end (*selector*) ;

        procedure call(fsys: setofsys; fcp: ctp);
          var lkey: 1..18;

          procedure variable(fsys: setofsys);
            var lcp: ctp;
          begin
            if sy = ident then
              begin searchid([vars,field],lcp); insymbol end
            else begin error(ErrIDExpect); lcp := uvarptr end;
            selector(fsys,lcp)
          end (*variable*) ;

          procedure getputresetrewriteprocedure;
          begin variable(fsys + [rparent]); loadaddress;
            if gattr.typtr <> nil then
              if gattr.typtr^.form <> files then error(ErrStdProcParm);
            if lkey <= 2 then begin
              if gattr.typtr = textptr then gen1(30(*csp*),lkey(*get,put*))
              else begin
                if gattr.typtr <> nil then 
                  gen2(PINSTldc(*ldc*),1,gattr.typtr^.filtype^.size);
                if lkey = 1 then gen1(30(*csp*),38(*gbf*))
                else gen1(30(*csp*),39(*pbf*))
              end
            end else
              if gattr.typtr = textptr then begin 
                if lkey = 3 then gen1(30(*csp*),25(*reset*))
                else gen1(30(*csp*),26(*rewrite*))
              end else begin
                if lkey = 3 then gen1(30(*csp*),36(*reset*))
                else gen1(30(*csp*),37(*rewrite*))
              end
          end (*getputresetrewrite*) ;

          procedure pageprocedure;
          var llev:levrange; 
          begin 
            llev := 1; 
            if sy = lparent then
            begin insymbol;
              variable(fsys + [rparent]); loadaddress;
              if gattr.typtr <> nil then
                if gattr.typtr <> textptr then error(ErrStdProcParm);
              if sy = rparent then insymbol else error(ErrClosePar)
            end else begin
              if not outputhdf then error(176);
              gen2(50(*lda*),level-outputptr^.vlev,outputptr^.vaddr);
            end;
            gen1(30(*csp*),24(*page*))
          end (*page*) ;

          procedure readprocedure;
            var lsp : stp;
                txt: boolean; { is a text file }
                deffil: boolean; { default file was loaded }
                test: boolean;
          begin
            txt := true; deffil := true;
            if sy = lparent then
              begin insymbol;
                variable(fsys + [comma,rparent]);
                lsp := gattr.typtr; test := false;
                if lsp <> nil then
                  if lsp^.form = files then
                    with gattr, lsp^ do
                      begin
                        txt := lsp = textptr;
                        if not txt and (lkey = 11) then error(ErrStdProcParm);
                        loadaddress; deffil := false;
                        if sy = rparent then
                          begin if lkey = 5 then error(ErrStdProcParm);
                            test := true
                          end
                        else
                          if sy <> comma then
                            begin error(ErrStdProcParm); skip(fsys + [comma,rparent]) end;
                        if sy = comma then
                          begin insymbol; variable(fsys + [comma,rparent])
                          end
                        else test := true
                      end
                  else if not inputhdf then error(175);
               if not test then
                repeat loadaddress;
                  if deffil then begin 
                    { file was not loaded, we load and swap so that it ends up
                      on the bottom.}
                    gen2(50(*lda*),level-inputptr^.vlev,inputptr^.vaddr);
                    gen1(PINSTswp(*swp*),ptrsize); { note 2nd is always pointer }
                    deffil := false
                  end;
                  if txt then begin
                    if gattr.typtr <> nil then
                      if gattr.typtr^.form <= subrange then
                        if comptypes(intptr,gattr.typtr) then
                          gen1(30(*csp*),3(*rdi*))
                        else
                          if comptypes(realptr,gattr.typtr) then
                            gen1(30(*csp*),4(*rdr*))
                          else
                            if comptypes(charptr,gattr.typtr) then
                              gen1(30(*csp*),5(*rdc*))
                            else error(ErrNotImp)
                      else error(ErrStdProcParm);
                  end else begin { binary file }
                    if not comptypes(gattr.typtr,lsp^.filtype) then error(129);
                    gen2(PINSTldc(*ldc*),1,lsp^.filtype^.size);
                    gen1(30(*csp*),35(*rbf*))
                  end;
                  test := sy <> comma;
                  if not test then
                    begin insymbol; variable(fsys + [comma,rparent])
                    end
                until test;
                if sy = rparent then insymbol else error(ErrClosePar)
              end
            else begin
              if not inputhdf then error(175);
              if lkey = 5 then error(ErrStdProcParm);
              gen2(50(*lda*),level-inputptr^.vlev,inputptr^.vaddr)
            end;
            if lkey = 11 then gen1(30(*csp*),21(*rln*));
            { remove the file pointer from stack }
            gen1(PINSTdmp(*dmp*),ptrsize);
          end (*read*) ;

          procedure writeprocedure;
            var lsp,lsp1: stp; default, default1: boolean; llkey: 1..15;
                len:addrrange;
                txt: boolean; { is a text file }
                deffil: boolean; { default file was loaded }
                test: boolean;
          begin llkey := lkey; txt := true; deffil := true;
            if sy = lparent then
            begin insymbol;
            expression(fsys + [comma,colon,rparent]);
            lsp := gattr.typtr; test := false;
            if lsp <> nil then
              if lsp^.form = files then
                with gattr, lsp^ do
                  begin lsp1 := lsp;
                    txt := lsp = textptr;
                    if not txt and (lkey = 12) then error(ErrStdProcParm);
                    loadaddress; deffil := false;
                    if sy = rparent then
                      begin if llkey = 6 then error(ErrStdProcParm);
                        test := true
                      end
                    else
                      if sy <> comma then
                        begin error(ErrStdProcParm); skip(fsys+[comma,rparent]) end;
                    if sy = comma then
                      begin insymbol; expression(fsys+[comma,colon,rparent])
                      end
                    else test := true
                  end
              else if not outputhdf then error(176);
            if not test then
            repeat
              lsp := gattr.typtr;
              if lsp <> nil then
                if lsp^.form <= subrange then load else loadaddress;
              if deffil then begin 
                { file was not loaded, we load and swap so that it ends up
                  on the bottom.}
                gen2(50(*lda*),level-outputptr^.vlev,outputptr^.vaddr);
                if lsp <> nil then
                  if lsp^.form <= subrange then begin
                  if lsp^.size < stackelsize then 
                    gen1(PINSTswp(*swp*),stackelsize) { size of 2nd is minimum stack }
                  else 
                    gen1(PINSTswp(*swp*),lsp^.size) { size of 2nd is operand }
                end else
                  gen1(PINSTswp(*swp*),ptrsize); { size of 2nd is pointer }
                deffil := false
              end;
              if txt then begin
                if sy = colon then
                  begin insymbol; expression(fsys + [comma,colon,rparent]);
                    if gattr.typtr <> nil then
                      if gattr.typtr <> intptr then error(ErrStdProcParm);
                    load; default := false
                  end
                else default := true;
                if sy = colon then
                  begin insymbol; expression(fsys + [comma,rparent]);
                    if gattr.typtr <> nil then
                      if gattr.typtr <> intptr then error(ErrStdProcParm);
                    if lsp <> realptr then error(124);
                    load; default1 := false
                  end else default1 := true;
                if lsp = intptr then
                  begin if default then gen2(PINSTldc(*ldc*),1,intdeff);
                    gen1(30(*csp*),6(*wri*))
                  end
                else
                  if lsp = realptr then
                    begin
                      if default1 then begin 
                        if default then gen2(PINSTldc(*ldc*),1,reldeff);
                        gen1(30(*csp*),8(*wrr*))
                      end else begin
                        if default then gen2(PINSTldc(*ldc*),1,reldeff);
                        gen1(30(*csp*),28(*wrf*))
                      end
                    end
                  else
                    if lsp = charptr then
                      begin if default then gen2(PINSTldc(*ldc*),1,chrdeff);
                        gen1(30(*csp*),9(*wrc*))
                      end
                    else
                      if lsp = boolptr then
                        begin if default then gen2(PINSTldc(*ldc*),1,boldeff);
                          gen1(30(*csp*),27(*wrb*))
                        end
                      else
                        if lsp <> nil then
                          begin
                            if lsp^.form = scalar then error(ErrNotImp)
                            else
                              if isString(lsp) then
                                begin len := lsp^.size div charmax;
                                  if default then
                                        gen2(PINSTldc(*ldc*),1,len);
                                  gen2(PINSTldc(*ldc*),1,len);
                                  gen1(30(*csp*),10(*wrs*))
                                end
                              else error(ErrStdProcParm)
                          end
              end else begin { binary file }
                if not comptypes(lsp1^.filtype,lsp) then error(129);
                if lsp = intptr then gen1(30(*csp*),31(*wbi*))
                else
                  if lsp = realptr then gen1(30(*csp*),32(*wbr*))
                  else
                    if lsp = charptr then gen1(30(*csp*),33(*wbc*))
                    else
                      if lsp = boolptr then gen1(30(*csp*),34(*wbb*))
                      else
                        if lsp^.form <= subrange then gen1(30(*csp*),31(*wbi*))
                        else
                           if lsp <> nil then
                              begin
                                gen2(PINSTldc(*ldc*),1,lsp1^.filtype^.size);
                                gen1(30(*csp*),30(*wbf*))
                              end
              end;
              test := sy <> comma;
              if not test then
                begin insymbol; expression(fsys + [comma,colon,rparent])
                end
            until test;
            if sy = rparent then insymbol else error(ErrClosePar)
            end else begin
              if not outputhdf then error(176);
              if lkey = 6 then error(ErrStdProcParm);
              gen2(50(*lda*),level-outputptr^.vlev,outputptr^.vaddr)
            end;
            if llkey = 12 then (*writeln*)
              gen1(30(*csp*),22(*wln*));
            { remove the file pointer from stack }
            gen1(PINSTdmp(*dmp*),ptrsize);
          end (*write*) ;

          procedure packprocedure;
            var lsp,lsp1: stp; lb, bs: integer; lattr: attr;
          begin variable(fsys + [comma,rparent]); loadaddress;
            lsp := nil; lsp1 := nil; lb := 1; bs := 1;
            lattr := gattr;
            if gattr.typtr <> nil then
              with gattr.typtr^ do
                if form = arrays then
                  begin lsp := inxtype; lsp1 := aeltype;
                    if (inxtype = charptr) or (inxtype = boolptr) then lb := 0
                    else if inxtype^.form = subrange then lb := inxtype^.min.ival;
                    bs := aeltype^.size
                  end
                else error(ErrStdProcParm);
            if sy = comma then insymbol else error(20);
            expression(fsys + [comma,rparent]); load;
            if gattr.typtr <> nil then
              if gattr.typtr^.form <> scalar then error(ErrStdProcParm)
              else
                if not comptypes(lsp,gattr.typtr) then error(ErrStdProcParm);
            gen2(PINSTldc(*ldc*),1,lb);
            gen0(21(*sbi*));
            gen2(PINSTldc(*ldc*),1,bs);
            gen0(PINSTmpi(*mpi*));
            if sy = comma then insymbol else error(20);
            variable(fsys + [rparent]); loadaddress;
            if gattr.typtr <> nil then
              with gattr.typtr^ do
                if form = arrays then
                  begin
                    if not comptypes(aeltype,lsp1) then error(ErrStdProcParm)
                  end
                else error(ErrStdProcParm);
            if (gattr.typtr <> nil) and (lattr.typtr <> nil) then
              gen2(62(*pck*),gattr.typtr^.size,lattr.typtr^.size)
          end (*pack*) ;

          procedure unpackprocedure;
            var lsp,lsp1: stp; lattr,lattr1: attr; lb, bs: integer;
          begin variable(fsys + [comma,rparent]); loadaddress;
            lattr := gattr;
            lsp := nil; lsp1 := nil; lb := 1; bs := 1;
            if gattr.typtr <> nil then
              with gattr.typtr^ do
                if form = arrays then lsp1 := aeltype
                else error(ErrStdProcParm);
            if sy = comma then insymbol else error(20);
            variable(fsys + [comma,rparent]); loadaddress;
            lattr1 := gattr;
            if gattr.typtr <> nil then
              with gattr.typtr^ do
                if form = arrays then
                  begin
                    if not comptypes(aeltype,lsp1) then error(ErrStdProcParm);
                    if (inxtype = charptr) or (inxtype = boolptr) then lb := 0
                    else if inxtype^.form = subrange then lb := inxtype^.min.ival;
                    bs := aeltype^.size;
                    lsp := inxtype; 
                  end
                else error(ErrStdProcParm);
            if sy = comma then insymbol else error(20);
            expression(fsys + [rparent]); load;
            if gattr.typtr <> nil then
              if gattr.typtr^.form <> scalar then error(ErrStdProcParm)
              else
                if not comptypes(lsp,gattr.typtr) then error(ErrStdProcParm);
            gen2(PINSTldc(*ldc*),1,lb);
            gen0(21(*sbi*));
            gen2(PINSTldc(*ldc*),1,bs);
            gen0(PINSTmpi(*mpi*));
            if (gattr.typtr <> nil) and (lattr.typtr <> nil) then
              gen2(63(*upk*),lattr.typtr^.size,lattr1.typtr^.size)
          end (*unpack*) ;

          procedure newdisposeprocedure;
            label 1;
            var lsp,lsp1: stp; varts: integer;
                lsize: addrrange; lval: valu;
          begin variable(fsys + [comma,rparent]); loadaddress;
            lsp := nil; varts := 0; lsize := 0;
            if gattr.typtr <> nil then
              with gattr.typtr^ do
                if form = pointer then
                  begin
                    if eltype <> nil then
                      begin lsize := eltype^.size;
                        if eltype^.form = records then lsp := eltype^.recvar
                      end
                  end
                else error(ErrStdProcParm);
            while sy = comma do
              begin insymbol;constant(fsys + [comma,rparent],lsp1,lval);
                varts := varts + 1;
                (*check to insert here: is constant in tagfieldtype range*)
                if lsp = nil then error(158)
                else
                  if lsp^.form <> tagfld then error(162)
                  else
                    if lsp^.tagfieldp <> nil then
                      if isString(lsp1) or (lsp1 = realptr) then error(159)
                      else
                        if comptypes(lsp^.tagfieldp^.idtype,lsp1) then
                          begin
                            lsp1 := lsp^.fstvar;
                            while lsp1 <> nil do
                              with lsp1^ do
                                if varval.ival = lval.ival then
                                  begin lsize := size; lsp := subvar;
                                    goto 1
                                  end
                                else lsp1 := nxtvar;
                            lsize := lsp^.size; lsp := nil;
                          end
                        else error(ErrStdProcParm);
          1:  end (*while*) ;
            gen2(PINSTldc(*ldc*),1,lsize);
            if lkey = 9 then gen1(30(*csp*),12(*new*))
            else gen1(30(*csp*),29(*dispose*))
          end (*new*) ;

          procedure absfunction;
          begin
            if gattr.typtr <> nil then
              if gattr.typtr = intptr then gen0(PINSTabi(*abi*))
              else
                if gattr.typtr = realptr then gen0(PINSTabr(*abr*))
                else begin error(125); gattr.typtr := intptr end
          end (*abs*) ;

          procedure sqrfunction;
          begin
            if gattr.typtr <> nil then
              if gattr.typtr = intptr then gen0(24(*sqi*))
              else
                if gattr.typtr = realptr then gen0(25(*sqr*))
                else begin error(125); gattr.typtr := intptr end
          end (*sqr*) ;

          procedure truncfunction;
          begin
            if gattr.typtr <> nil then
              if gattr.typtr <> realptr then error(125);
            gen0(27(*trc*));
            gattr.typtr := intptr
          end (*trunc*) ;

          procedure roundfunction;
          begin
            if gattr.typtr <> nil then
              if gattr.typtr <> realptr then error(125);
            gen0(PINSTrnd(*rnd*));
            gattr.typtr := intptr
          end (*round*) ;

          procedure oddfunction;
          begin
            if gattr.typtr <> nil then
              if gattr.typtr <> intptr then error(125);
            gen0(20(*odd*));
            gattr.typtr := boolptr
          end (*odd*) ;

          procedure ordfunction;
          begin
            if gattr.typtr <> nil then
              if gattr.typtr^.form >= power then error(125);
            gen0t(58(*ord*),gattr.typtr);
            gattr.typtr := intptr
          end (*ord*) ;

          procedure chrfunction;
          begin
            if gattr.typtr <> nil then
              if gattr.typtr <> intptr then error(125);
            gen0(59(*chr*));
            gattr.typtr := charptr
          end (*chr*) ;

          procedure predsuccfunction;
          begin
            if gattr.typtr <> nil then
              if gattr.typtr^.form <> scalar then error(125);
            if lkey = 7 then gen1t(31(*dec*),1,gattr.typtr)
            else gen1t(34(*inc*),1,gattr.typtr)
          end (*predsucc*) ;

          procedure eofeolnfunction;
          begin
            if sy = lparent then
              begin
                insymbol;
                variable(fsys + [rparent]);
                if sy = rparent then
                   insymbol
                else
                   error(ErrClosePar)
              end
            else
            begin
              if not inputhdf then
                 error(175);
              with gattr do
              begin
                 typtr := textptr;
                 kind := varbl;
                 access := drct;
                 vlevel := inputptr^.vlev;
                 dplmt := inputptr^.vaddr
              end
            end;
            loadaddress;
            if gattr.typtr <> nil then
              if gattr.typtr^.form <> files then
                 error(125)
              else if (lkey = ErrType) and
                  (gattr.typtr <> textptr) then
                      error(ErrStdProcParm);
            if lkey = 9 then
            begin
               if gattr.typtr = textptr then
                  gen0(8(*eof*))
               else
                  gen0(PINSTefb(*efb*))
            end
            else
               gen1(30(*csp*),14(*eln*));
            gattr.typtr := boolptr
          end (*eof*) ;

          procedure callnonstandard;
            var nxt,lcp: ctp; lsp: stp; lkind: idkind; lb: boolean;
                locpar, llc: addrrange;

          procedure compparam(pla, plb: ctp);
          begin
            while (pla <> nil) and (plb <> nil) do begin
              if not comptypes(pla^.idtype,plb^.idtype) then error(189);
              pla := pla^.next; plb := plb^.next
            end;
            if (pla <> nil) or (plb <> nil) then error(189)
          end;

          begin locpar := 0;
            with fcp^ do
              begin nxt := pflist; lkind := pfkind;
                if pfkind = actual then begin { it's a system call }
                  if not externl then gen1(41(*mst*),level-pflev)
                end else gen1(41(*mst*),level-pflev) { its an indirect }
              end;
            if sy = lparent then
              begin llc := lc;
                repeat lb := false; (*decide whether proc/func must be passed*)
                  if nxt = nil then error(126)
                  else lb := nxt^.klass in [proc,func];
                  insymbol;
                  if lb then   (*pass function or procedure*)
                    begin
                      if sy <> ident then
                        begin error(ErrIDExpect); skip(fsys + [comma,rparent]) end
                      else if nxt <> nil then
                        begin
                          if nxt^.klass = proc then searchid([proc],lcp)
                          else
                            begin searchid([func],lcp);
                              { compare result types }
                              if not comptypes(lcp^.idtype,nxt^.idtype) then
                                error(128)
                            end;
                          { compare parameter lists }
                          if (nxt^.klass in [proc,func]) and 
                             (lcp^.klass in [proc,func]) then
                            compparam(nxt^.pflist, lcp^.pflist);
                          if lcp^.pfkind = actual then genlpa(lcp^.pfname,level-lcp^.pflev)
                          else gen2(PINSTlip(*lip*),level-lcp^.pflev,lcp^.pfaddr);
                          locpar := locpar+ptrsize*2;
                          insymbol;
                          if not (sy in fsys + [comma,rparent]) then
                            begin error(ErrIllSym); skip(fsys + [comma,rparent]) end
                        end
                    end (*if lb*)
                  else
                    begin expression(fsys + [comma,rparent]);
                      if gattr.typtr <> nil then
                        begin
                          if nxt <> nil then
                            begin lsp := nxt^.idtype;
                              if lsp <> nil then
                                begin
                                  if (nxt^.vkind = actual) then
                                    if lsp^.form <= power then
                                      begin load;
                                        if debug then checkbnds(lsp);
                                        if comptypes(realptr,lsp)
                                           and (gattr.typtr = intptr) then
                                          begin gen0(10(*flt*));
                                            gattr.typtr := realptr
                                          end;
                                        locpar := locpar+lsp^.size;
                                        align(parmptr,locpar);
                                      end
                                    else
                                      begin
                                        loadaddress;
                                        locpar := locpar+ptrsize;
                                        align(parmptr,locpar)
                                      end
                                  else
                                    if gattr.kind = varbl then
                                      begin loadaddress;
                                        locpar := locpar+ptrsize;
                                        align(parmptr,locpar);
                                      end
                                    else error(154);
                                  if not comptypes(lsp,gattr.typtr) then
                                    error(142)
                                end
                            end
                        end
                    end;
                  if nxt <> nil then nxt := nxt^.next
                until sy <> comma;
                lc := llc;
                if sy = rparent then insymbol else error(ErrClosePar)
              end (*if lparent*);
            if lkind = actual then
              begin if nxt <> nil then error(126);
                with fcp^ do
                  begin
                    if externl then gen1(30(*csp*),pfname)
                    else gencupent(46(*cup*),locpar,pfname);
                  end
              end
            else begin { call procedure or function parameter }
              gen2(50(*lda*),level-fcp^.pflev,fcp^.pfaddr);
              gen1(PINSTcip(*cip*),locpar)
            end;
            gattr.typtr := fcp^.idtype
          end (*callnonstandard*) ;

        begin (*call*)
          if fcp^.pfdeckind = standard then
            begin lkey := fcp^.key;
              if fcp^.klass = proc then
                begin
                  if not(lkey in [5,6,11,12,17]) then
                    if sy = lparent then insymbol else error(ErrOpenPar);
                  case lkey of
                    1,2,
                    3,4:   getputresetrewriteprocedure;
                    17:    pageprocedure;
                    5,11:  readprocedure;
                    6,12:  writeprocedure;
                    7:     packprocedure;
                    8:     unpackprocedure;
                    9,18:  newdisposeprocedure;
                    10,13: error(ErrNotImp)
                  end;
                  if not(lkey in [5,6,11,12,17]) then
                    if sy = rparent then insymbol else error(ErrClosePar)
                end
              else
                begin
                  if (lkey <= 8) or (lkey = 16) then
                    begin
                      if sy = lparent then insymbol else error(ErrOpenPar);
                      expression(fsys+[rparent]); load
                    end;
                  case lkey of
                    1:    absfunction;
                    2:    sqrfunction;
                    3:    truncfunction;
                    16:   roundfunction;
                    4:    oddfunction;
                    5:    ordfunction;
                    6:    chrfunction;
                    7,8:  predsuccfunction;
                    9,10: eofeolnfunction
                  end;
                  if (lkey <= 8) or (lkey = 16) then
                    if sy = rparent then insymbol else error(ErrClosePar)
                end;
            end (*standard procedures and functions*)
          else callnonstandard
        end (*call*) ;

{        procedure expression; -- declared forward }
         procedure expression(fsys: setofsys);
          var lattr: attr; lop: Operators; typind: char; lsize: addrrange;

          procedure simpleexpression(fsys: setofsys);
            var lattr: attr; lop: Operators; signed: boolean;

            procedure term(fsys: setofsys);
              var lattr: attr; lop: Operators;

              procedure factor(fsys: setofsys);
                var lcp: ctp; lvp: csp; varpart: boolean;
                    cstpart: setty; lsp: stp;
                    tattr, rattr: attr;
                    test: boolean;
              begin
                if not (sy in facbegsys) then
                  begin error(58); skip(fsys + facbegsys);
                    gattr.typtr := nil
                  end;
                while sy in facbegsys do
                  begin
                    case sy of
              (*id*)    ident:
                        begin searchid([konst,vars,field,func],lcp);
                          insymbol;
                          if lcp^.klass = func then
                            begin call(fsys,lcp);
                              with gattr do
                                begin kind := expr;
                                  if typtr <> nil then
                                    if typtr^.form=subrange then
                                      typtr := typtr^.rangetype
                                end
                            end
                          else
                            if lcp^.klass = konst then
                              with gattr, lcp^ do
                                begin typtr := idtype; kind := cst;
                                  cval := values
                                end
                            else
                              begin selector(fsys,lcp);
                                if gattr.typtr<>nil then(*elim.subr.types to*)
                                  with gattr,typtr^ do(*simplify later tests*)
                                    if form = subrange then
                                      typtr := rangetype
                              end
                        end;
              (*cst*)   intconst:
                        begin
                          with gattr do
                            begin typtr := intptr; kind := cst;
                              cval := val
                            end;
                          insymbol
                        end;
                        realconst:
                        begin
                          with gattr do
                            begin typtr := realptr; kind := cst;
                              cval := val
                            end;
                          insymbol
                        end;
                        stringconst:
                        begin
                          with gattr do
                            begin
                              if lgth = 1 then typtr := charptr
                              else
                                begin new(lsp { ,arrays -- tagfield } ); pshstc(lsp);
                                  with lsp^ do
                                    begin aeltype := charptr; form:=arrays;
                                      packing := true;
                                      inxtype := nil; size := lgth*charsize
                                    end;
                                  typtr := lsp
                                end;
                              kind := cst; cval := val
                            end;
                          insymbol
                        end;
              (* ( *)   lparent:
                        begin insymbol; expression(fsys + [rparent]);
                          if sy = rparent then insymbol else error(ErrClosePar)
                        end;
              (*not*)   notsy:
                        begin insymbol; factor(fsys);
                          load; gen0(19(*not*));
                          if gattr.typtr <> nil then
                            if gattr.typtr <> boolptr then
                              begin error(135); gattr.typtr := nil end;
                        end;
              (*[*)     lbrack:
                        begin insymbol; cstpart := [ ]; varpart := false;
                          new(lsp { ,power -- tagfield } ); pshstc(lsp);
                          with lsp^ do
                            begin elset:=nil;size:=setsize;form:=power; 
                                  packing := false; matchpack := false end;
                          if sy = rbrack then
                            begin
                              with gattr do
                                begin typtr := lsp; kind := cst end;
                              insymbol
                            end
                          else
                            begin
                              repeat expression(fsys + [comma,range,rbrack]);
                                rattr.typtr := nil;
                                if sy = range then begin insymbol; 
                                  { if the left side is not constant, load it
                                    and coerce it to integer now }
                                  if gattr.kind <> cst then begin
                                    load;
                                    if not comptypes(gattr.typtr,intptr)
                                    then gen0t(58(*ord*),gattr.typtr);
                                  end;
                                  tattr := gattr; expression(fsys + [comma,rbrack]);
                                  rattr := gattr; gattr := tattr;
                                end;
                                if gattr.typtr <> nil then
                                  if gattr.typtr^.form <> scalar then
                                    begin error(136); gattr.typtr := nil end
                                  else
                                    if comptypes(lsp^.elset,gattr.typtr) then
                                      begin
                                        if rattr.typtr <> nil then begin { x..y form }
                                          if rattr.typtr^.form <> scalar then
                                            begin error(136); rattr.typtr := nil end
                                          else
                                            if comptypes(lsp^.elset,rattr.typtr) then
                                              begin
                                                if (gattr.kind = cst) and 
                                                   (rattr.kind = cst) then
                                                  if (lattr.cval.ival < setlow) or
                                                     (lattr.cval.ival > sethigh) or
                                                     (gattr.cval.ival < setlow) or
                                                     (gattr.cval.ival > sethigh) then
                                                    error(304)
                                                  else
                                                    cstpart := cstpart+
                                                      [gattr.cval.ival..rattr.cval.ival]
                                                else
                                                  begin 
                                                    tattr := gattr; gattr := rattr;
                                                    load;
                                                    gattr := tattr;
                                                    if not comptypes(rattr.typtr,intptr)
                                                    then gen0t(58(*ord*),rattr.typtr);
                                                    gen0(64(*rgs*));
                                                    if varpart then gen0(28(*uni*))
                                                    else varpart := true
                                                  end
                                              end
                                            else error(137)
                                        end else begin
                                          if gattr.kind = cst then
                                            if (gattr.cval.ival < setlow) or
                                              (gattr.cval.ival > sethigh) then
                                              error(304)
                                            else
                                              cstpart := cstpart+[gattr.cval.ival]
                                          else
                                            begin load;
                                              if not comptypes(gattr.typtr,intptr)
                                              then gen0t(58(*ord*),gattr.typtr);
                                              gen0(23(*sgs*));
                                              if varpart then gen0(28(*uni*))
                                              else varpart := true
                                            end
                                        end;
                                        lsp^.elset := gattr.typtr;
                                        gattr.typtr := lsp
                                      end
                                    else error(137);
                                test := sy <> comma;
                                if not test then insymbol
                              until test;
                              if sy = rbrack then insymbol else error(ErrClsBrack)
                            end;
                          if varpart then
                            begin
                              if cstpart <> [ ] then
                                begin new(lvp { ,pset -- tagfield } ); pshcst(lvp);
                                  lvp^.pval := cstpart;
                                  lvp^.cclass := pset;
                                  if cstptrix = cstoccmax then error(254)
                                  else
                                    begin cstptrix := cstptrix + 1;
                                      cstptr[cstptrix] := lvp;
                                      gen2(PINSTldc(*ldc*),5,cstptrix);
                                      gen0(28(*uni*)); gattr.kind := expr
                                    end
                                end
                            end
                          else
                            begin new(lvp { ,pset -- tagfield } ); pshcst(lvp);
                              lvp^.pval := cstpart;
                              lvp^.cclass := pset;
                              gattr.cval.valp := lvp
                            end
                        end;
              (*nil*)   nilsy: with gattr do
                                 begin typtr := nilptr; kind := cst;
                                       cval.ival := nilval;
                                       insymbol
                                 end
                    end (*case*) ;
                    if not (sy in fsys) then
                      begin error(ErrIllSym); skip(fsys + facbegsys) end
                  end (*while*)
              end (*factor*) ;

            begin (*term*)
              factor(fsys + [mulop]);
              while sy = mulop do
                begin load; lattr := gattr; lop := op;
                  insymbol; factor(fsys + [mulop]); load;
                  if (lattr.typtr <> nil) and (gattr.typtr <> nil) then
                    case lop of
            (***)     mul:  if (lattr.typtr=intptr)and(gattr.typtr=intptr)
                            then gen0(PINSTmpi(*mpi*))
                            else
                              begin
                                if lattr.typtr = intptr then
                                  begin gen0(PINSTflo(*flo*));
                                    lattr.typtr := realptr
                                  end
                                else
                                  if gattr.typtr = intptr then
                                    begin gen0(10(*flt*));
                                      gattr.typtr := realptr
                                    end;
                                if (lattr.typtr = realptr)
                                  and(gattr.typtr=realptr)then gen0(16(*mpr*))
                                else
                                  if(lattr.typtr^.form=power)
                                    and comptypes(lattr.typtr,gattr.typtr)then
                                    gen0(12(*int*))
                                  else begin error(ErrIllOp); gattr.typtr:=nil end
                              end;
            (* / *)   rdiv: begin
                              if gattr.typtr = intptr then
                                begin gen0(10(*flt*));
                                  gattr.typtr := realptr
                                end;
                              if lattr.typtr = intptr then
                                begin gen0(PINSTflo(*flo*));
                                  lattr.typtr := realptr
                                end;
                              if (lattr.typtr = realptr)
                                and (gattr.typtr=realptr)then gen0(7(*dvr*))
                              else begin error(ErrIllOp); gattr.typtr := nil end
                            end;
            (*div*)   idiv: if (lattr.typtr = intptr)
                              and (gattr.typtr = intptr) then gen0(6(*dvi*))
                            else begin error(ErrIllOp); gattr.typtr := nil end;
            (*mod*)   imod: if (lattr.typtr = intptr)
                              and (gattr.typtr = intptr) then gen0(14(*mod*))
                            else begin error(ErrIllOp); gattr.typtr := nil end;
            (*and*)   andop:if (lattr.typtr = boolptr)
                              and (gattr.typtr = boolptr) then gen0(4(*and*))
                            else begin error(ErrIllOp); gattr.typtr := nil end
                    end (*case*)
                  else gattr.typtr := nil
                end (*while*)
            end (*term*) ;

          begin (*simpleexpression*)
            signed := false;
            if (sy = addop) and (op in [plus,minus]) then
              begin signed := op = minus; insymbol end;
            term(fsys + [addop]);
            if signed then
              begin load;
                if gattr.typtr = intptr then gen0(17(*ngi*))
                else
                  if gattr.typtr = realptr then gen0(18(*ngr*))
                  else begin error(ErrIllOp); gattr.typtr := nil end
              end;
            while sy = addop do
              begin load; lattr := gattr; lop := op;
                insymbol; term(fsys + [addop]); load;
                if (lattr.typtr <> nil) and (gattr.typtr <> nil) then
                  case lop of
          (*+*)       plus:
                      if (lattr.typtr = intptr)and(gattr.typtr = intptr) then
                        gen0(PINSTadi)
                      else
                        begin
                          if lattr.typtr = intptr then
                            begin gen0(PINSTflo(*flo*));
                              lattr.typtr := realptr
                            end
                          else
                            if gattr.typtr = intptr then
                              begin gen0(10(*flt*));
                                gattr.typtr := realptr
                              end;
                          if (lattr.typtr = realptr)and(gattr.typtr = realptr)
                            then gen0(PINSTadr(*adr*))
                          else if(lattr.typtr^.form=power)
                                 and comptypes(lattr.typtr,gattr.typtr) then
                                 gen0(28(*uni*))
                               else begin error(ErrIllOp); gattr.typtr:=nil end
                        end;
          (*-*)       minus:
                      if (lattr.typtr = intptr)and(gattr.typtr = intptr) then
                        gen0(21(*sbi*))
                      else
                        begin
                          if lattr.typtr = intptr then
                            begin gen0(PINSTflo(*flo*));
                              lattr.typtr := realptr
                            end
                          else
                            if gattr.typtr = intptr then
                              begin gen0(10(*flt*));
                                gattr.typtr := realptr
                              end;
                          if (lattr.typtr = realptr)and(gattr.typtr = realptr)
                            then gen0(22(*sbr*))
                          else
                            if (lattr.typtr^.form = power)
                              and comptypes(lattr.typtr,gattr.typtr) then
                              gen0(5(*dif*))
                            else begin error(ErrIllOp); gattr.typtr := nil end
                        end;
          (*or*)      orop:
                      if(lattr.typtr=boolptr)and(gattr.typtr=boolptr)then
                        gen0(13(*ior*))
                      else begin error(ErrIllOp); gattr.typtr := nil end
                  end (*case*)
                else gattr.typtr := nil
              end (*while*)
          end (*simpleexpression*) ;

        begin (*expression*)
          simpleexpression(fsys + [relop]);
          if sy = relop then
            begin
              if gattr.typtr <> nil then
                if gattr.typtr^.form <= power then load
                else loadaddress;
              lattr := gattr; lop := op;
              if lop = inop then
                if not comptypes(gattr.typtr,intptr) then
                  gen0t(58(*ord*),gattr.typtr);
              insymbol; simpleexpression(fsys);
              if gattr.typtr <> nil then
                if gattr.typtr^.form <= power then load
                else loadaddress;
              if (lattr.typtr <> nil) and (gattr.typtr <> nil) then
                if lop = inop then
                  if gattr.typtr^.form = power then
                    if comptypes(lattr.typtr,gattr.typtr^.elset) then
                      gen0(11(*inn*))
                    else begin error(129); gattr.typtr := nil end
                  else begin error(130); gattr.typtr := nil end
                else
                  begin
                    if lattr.typtr <> gattr.typtr then
                      if lattr.typtr = intptr then
                        begin gen0(PINSTflo(*flo*));
                          lattr.typtr := realptr
                        end
                      else
                        if gattr.typtr = intptr then
                          begin gen0(10(*flt*));
                            gattr.typtr := realptr
                          end;
                    if comptypes(lattr.typtr,gattr.typtr) then
                      begin lsize := lattr.typtr^.size;
                        case lattr.typtr^.form of
                          scalar:
                            if lattr.typtr = realptr then typind := 'r'
                            else
                              if lattr.typtr = boolptr then typind := 'b'
                              else
                                if lattr.typtr = charptr then typind := 'c'
                                else typind := 'i';
                          pointer:
                            begin
                              if lop in [ltop,leop,gtop,geop] then error(131);
                              typind := 'a'
                            end;
                          power:
                            begin if lop in [ltop,gtop] then error(132);
                              typind := 's'
                            end;
                          arrays:
                            begin
                              if not isString(lattr.typtr)
                                then error(ErrIllOp);
                              typind := 'm'
                            end;
                          records:
                            begin
                              error(ErrIllOp);
                              typind := 'm'
                            end;
                          files:
                            begin error(133); typind := 'f' end
                        end;
                        case lop of
                          ltop: gen2(53(*les*),ord(typind),lsize);
                          leop: gen2(52(*leq*),ord(typind),lsize);
                          gtop: gen2(49(*grt*),ord(typind),lsize);
                          geop: gen2(48(*geq*),ord(typind),lsize);
                          neop: gen2(55(*neq*),ord(typind),lsize);
                          eqop: gen2(47(*equ*),ord(typind),lsize)
                        end
                      end
                    else error(129)
                  end;
              gattr.typtr := boolptr; gattr.kind := expr
            end (*sy = relop*)
        end (*expression*) ;

        procedure assignment(fcp: ctp);
          var lattr: attr;
        begin selector(fsys + [becomes],fcp);
          if sy = becomes then
            begin
              if gattr.typtr <> nil then
                if (gattr.access<>drct) or (gattr.typtr^.form>power) then
                  loadaddress;
              lattr := gattr;
              insymbol; expression(fsys);
              if gattr.typtr <> nil then
                if gattr.typtr^.form <= power then load
                else loadaddress;
              if (lattr.typtr <> nil) and (gattr.typtr <> nil) then
                begin
                  if comptypes(realptr,lattr.typtr)and(gattr.typtr=intptr)then
                    begin gen0(10(*flt*));
                      gattr.typtr := realptr
                    end;
                  if comptypes(lattr.typtr,gattr.typtr) then begin
                    if filecomponent(gattr.typtr) then error(191);
                    case lattr.typtr^.form of
                      scalar,
                      subrange: begin
                                  if debug then checkbnds(lattr.typtr);
                                  store(lattr)
                                end;
                      pointer: begin
                                 if debug then
                                   gen2t(45(*chk*),0,maxaddr,nilptr);
                                 store(lattr)
                               end;
                      power:   store(lattr);
                      arrays,
                      records: gen1(40(*mov*),lattr.typtr^.size);
                      files: error(146)
                    end
                  end else error(129)
                end
            end (*sy = becomes*)
          else error(51)
        end (*assignment*) ;

        procedure gotostatement;
          var llp: lbp; ttop,ttop1: disprange;

        begin
          if sy = intconst then
            begin
              ttop := top;
              while display[ttop].occur <> blck do ttop := ttop - 1;
              ttop1 := ttop;
              repeat
                searchlabel(llp, ttop); { find label }
                if llp <> nil then with llp^ do begin
                  if defined then
                    if slevel > stalvl then { defining point level greater than
                                              present statement level }
                      error(185) { goto references deeper nested statement }
                    else if (slevel > 1) and not bact then 
                      error(187); { Goto references label in different nested
                                    statement }
                  { establish the minimum statement level a goto appeared at }
                  if minlvl > stalvl then minlvl := stalvl;
                  if ttop = ttop1 then
                    genujpxjp(57(*ujp*),labname) 
                  else begin { interprocedural goto }
                    genipj(66(*ipj*),level-vlevel,labname);
                    ipcref := true
                  end
                end;
                ttop := ttop - 1
              until (llp <> nil) or (ttop = 0);
              if llp = nil then begin
                error(167); { undeclared label }
                newlabel(llp) { create dummy label in current context }
              end;
              insymbol
            end
          else error(ErrIntExpect)
        end (*gotostatement*) ;

        procedure compoundstatement;
        var test: boolean;
        begin
          addlvl;
          repeat
            repeat statement(fsys + [semicolon,endsy])
            until not (sy in statbegsys);
            test := sy <> semicolon;
            if not test then insymbol
          until test;
          if sy = endsy then insymbol else error(13);
          sublvl
        end (*compoundstatemenet*) ;

        procedure ifstatement;
          var lcix1,lcix2: integer;
        begin expression(fsys + [thensy]);
          genlabel(lcix1); genfjp(lcix1);
          if sy = thensy then insymbol else error(52);
          addlvl;
          statement(fsys + [elsesy]);
          sublvl;
          if sy = elsesy then
            begin genlabel(lcix2); genujpxjp(57(*ujp*),lcix2);
              putlabel(lcix1);
              insymbol; 
              addlvl;
              statement(fsys);
              sublvl;
              putlabel(lcix2)
            end
          else putlabel(lcix1)
        end (*ifstatement*) ;

        procedure casestatement;
          label 1;
          var lsp,lsp1: stp; fstptr,lpt1,lpt2,lpt3: cip; lval: valu;
              laddr, lcix, lcix1, lmin, lmax: integer;
              test: boolean;
        begin expression(fsys + [ofsy,comma,colon]);
          load; genlabel(lcix);
          lsp := gattr.typtr;
          if lsp <> nil then
            if (lsp^.form <> scalar) or (lsp = realptr) then
              begin error(144); lsp := nil end
            else if not comptypes(lsp,intptr) then gen0t(58(*ord*),lsp);
          genujpxjp(57(*ujp*),lcix);
          if sy = ofsy then insymbol else error(ErrNoOf);
          fstptr := nil; genlabel(laddr);
          repeat
            lpt3 := nil; genlabel(lcix1);
            if not(sy in [semicolon,endsy]) then
              begin
                repeat constant(fsys + [comma,colon],lsp1,lval);
                  if lsp <> nil then
                    if comptypes(lsp,lsp1) then
                      begin lpt1 := fstptr; lpt2 := nil;
                        while lpt1 <> nil do
                          with lpt1^ do
                            begin
                              if cslab <= lval.ival then
                                begin if cslab = lval.ival then error(156);
                                  goto 1
                                end;
                              lpt2 := lpt1; lpt1 := next
                            end;
            1:    getcas(lpt3);
                        with lpt3^ do
                          begin next := lpt1; cslab := lval.ival;
                            csstart := lcix1
                          end;
                        if lpt2 = nil then fstptr := lpt3
                        else lpt2^.next := lpt3
                      end
                    else error(147);
                  test := sy <> comma;
                  if not test then insymbol
                until test;
                if sy = colon then insymbol else error(ErrColon);
                putlabel(lcix1);
                repeat 
                  addlvl;
                  statement(fsys + [semicolon]);
                  sublvl
                until not (sy in statbegsys);
                if lpt3 <> nil then
                  genujpxjp(57(*ujp*),laddr);
              end;
            test := sy <> semicolon;
            if not test then insymbol
          until test;
          putlabel(lcix);
          if fstptr <> nil then
            begin lmax := fstptr^.cslab;
              (*reverse pointers*)
              lpt1 := fstptr; fstptr := nil;
              repeat lpt2 := lpt1^.next; lpt1^.next := fstptr;
                fstptr := lpt1; lpt1 := lpt2
              until lpt1 = nil;
              lmin := fstptr^.cslab;
              if lmax - lmin < cixmax then
                begin
                  gen2t(45(*chk*),lmin,lmax,intptr);
                  gen2(PINSTldc(*ldc*),1,lmin); gen0(21(*sbi*)); genlabel(lcix);
                  genujpxjp(44(*xjp*),lcix); putlabel(lcix);
                  repeat
                    with fstptr^ do
                      begin
                        while cslab > lmin do
                           begin gen0(PINSTujc(*ujc error*));
                             lmin := lmin+1
                           end;
                        genujpxjp(57(*ujp*),csstart);
                        lpt1 := fstptr; fstptr := next; lmin := lmin + 1;
                        putcas(lpt1);
                      end
                  until fstptr = nil;
                  putlabel(laddr)
                end
              else begin
                error(157);
                repeat
                  with fstptr^ do
                    begin
                      lpt1 := fstptr; fstptr := next;
                      putcas(lpt1);
                    end
                until fstptr = nil
              end
            end;
            if sy = endsy then insymbol else error(13)
        end (*casestatement*) ;

        procedure repeatstatement;
          var laddr: integer;
        begin genlabel(laddr); putlabel(laddr);
          repeat 
            addlvl;
            statement(fsys + [semicolon,untilsy]);
            sublvl;
            if sy in statbegsys then error(ErrNoSemi)
          until not(sy in statbegsys);
          while sy = semicolon do
            begin insymbol;
              repeat 
                addlvl;
                statement(fsys + [semicolon,untilsy]);
                if sy in statbegsys then error(ErrNoSemi);
                sublvl
              until not (sy in statbegsys);
            end;
          if sy = untilsy then
            begin insymbol; expression(fsys); genfjp(laddr)
            end
          else error(53);
        end (*repeatstatement*) ;

        procedure whilestatement;
          var laddr, lcix: integer;
        begin genlabel(laddr); putlabel(laddr);
          expression(fsys + [dosy]); genlabel(lcix); genfjp(lcix);
          if sy = dosy then insymbol else error(54);
          addlvl;
          statement(fsys); 
          sublvl;
          genujpxjp(57(*ujp*),laddr); putlabel(lcix)
        end (*whilestatement*) ;

        procedure forstatement;
          var lattr: attr;  lsy: symbol;
              lcix, laddr: integer;
                    llc, lcs: addrrange;
              typind: char; (* added for typing [sam] *)
        begin llc := lc;
          with lattr do
            begin typtr := nil; kind := varbl;
              access := drct; vlevel := level; dplmt := 0
            end;
          typind := 'i'; (* default to integer [sam] *)
          if sy = ident then
            begin searchid([vars],lcp);
              with lcp^, lattr do
                begin typtr := idtype; kind := varbl;
                  if vkind = actual then
                    begin access := drct; vlevel := vlev;
                      if vlev <> level then error(183);
                      dplmt := vaddr
                    end
                  else begin error(155); typtr := nil end
                end;
              (* determine type of control variable [sam] *)
              if lattr.typtr = boolptr then typind := 'b'
              else if lattr.typtr = charptr then typind := 'c';
              if lattr.typtr <> nil then
                if (lattr.typtr^.form > subrange)
                   or comptypes(realptr,lattr.typtr) then
                  begin error(143); lattr.typtr := nil end;
              insymbol
            end
          else
            begin error(ErrIDExpect); skip(fsys + [becomes,tosy,downtosy,dosy]) end;
          if sy = becomes then
            begin insymbol; expression(fsys + [tosy,downtosy,dosy]);
              if gattr.typtr <> nil then
                  if gattr.typtr^.form <> scalar then error(144)
                  else
                    if comptypes(lattr.typtr,gattr.typtr) then begin
                      load; align(intptr,lc);
                      { store start to temp }
                      gen2t(PINSTstr(*str*),0,lc,intptr);
                    end else error(145)
            end
          else
            begin error(51); skip(fsys + [tosy,downtosy,dosy]) end;
          if sy in [tosy,downtosy] then
            begin lsy := sy; insymbol; expression(fsys + [dosy]);
              if gattr.typtr <> nil then
              if gattr.typtr^.form <> scalar then error(144)
                else
                  if comptypes(lattr.typtr,gattr.typtr) then
                    begin
                      load; align(intptr,lc);
                      if not comptypes(lattr.typtr,intptr) then
                        gen0t(58(*ord*),gattr.typtr);
                      gen2t(PINSTstr(*str*),0,lc+intsize,intptr);
                      { set initial value of index }
                      gen2t(54(*lod*),0,lc,intptr);
                      store(lattr);
                      genlabel(laddr); putlabel(laddr);
                      gattr := lattr; load;
                      if not comptypes(gattr.typtr,intptr) then
                        gen0t(58(*ord*),gattr.typtr);
                      gen2t(54(*lod*),0,lc+intsize,intptr);
                      lcs := lc;
                      lc := lc + intsize + intsize;
                      if lc > lcmax then lcmax := lc;
                      if lsy = tosy then gen2(52(*leq*),ord(typind),1)
                      else gen2(48(*geq*),ord(typind),1);
                    end
                  else error(145)
            end
          else begin error(55); skip(fsys + [dosy]) end;
          genlabel(lcix); genujpxjp(33(*fjp*),lcix);
          if sy = dosy then insymbol else error(54);
          addlvl;
          statement(fsys);
          sublvl;
          gattr := lattr; load;
          if not comptypes(gattr.typtr,intptr) then
            gen0t(58(*ord*),gattr.typtr);
          gen2t(54(*lod*),0,lcs+intsize,intptr);
          gen2(47(*equ*),ord(typind),1);
          genujpxjp(PINSTtjp(*tjp*),lcix);
          gattr := lattr; load;
          if lsy=tosy then gen1t(34(*inc*),1,gattr.typtr)
          else  gen1t(31(*dec*),1,gattr.typtr);
          store(lattr); genujpxjp(57(*ujp*),laddr); putlabel(lcix);
          lc := llc;
        end (*forstatement*) ;

        procedure withstatement;
          var lcp: ctp; lcnt1: disprange; llc: addrrange;
              test: boolean;
        begin lcnt1 := 0; llc := lc;
          repeat
            if sy = ident then
              begin searchid([vars,field],lcp); insymbol end
            else begin error(ErrIDExpect); lcp := uvarptr end;
            selector(fsys + [comma,dosy],lcp);
            if gattr.typtr <> nil then
              if gattr.typtr^.form = records then
                if top < displimit then
                  begin top := top + 1; lcnt1 := lcnt1 + 1;
                    with display[top] do
                      begin fname := gattr.typtr^.fstfld;
                        flabel := nil;
                        flabel := nil;
                        fconst := nil;
                        fstruct := nil;
                      end;
                    if gattr.access = drct then
                      with display[top] do
                        begin occur := crec; clev := gattr.vlevel;
                          cdspl := gattr.dplmt
                        end
                    else
                      begin loadaddress;
                        align(nilptr,lc);
                        gen2t(PINSTstr(*str*),0,lc,nilptr);
                        with display[top] do
                          begin occur := vrec; vdspl := lc end;
                        lc := lc+ptrsize;
                        if lc > lcmax then lcmax := lc
                      end
                  end
                else error(250)
              else error(140);
            test := sy <> comma;
            if not test then insymbol
          until test;
          if sy = dosy then insymbol else error(54);
          addlvl;
          statement(fsys);
          sublvl;
          { purge display levels }
          while lcnt1 > 0 do begin
             { don't recycle the record context }
             display[top].fname := nil;
             putdsp(top); { purge }
             top := top-1; lcnt1 := lcnt1-1; { count off }
          end;
          lc := llc;
        end (*withstatement*) ;

      begin (*statement*)
        if sy = intconst then (*label*)
          begin 
            searchlabel(llp, level); { search label }
            if llp <> nil then with llp^ do begin { found }
              if defined then error(165); { multidefined label }
              bact := true; { set in active block now }
              slevel := stalvl; { establish statement level }
              defined := true; { set defined }
              if ipcref and (stalvl > 1) then 
                error(184) { intraprocedure goto does not reference outter block }
              else if minlvl < stalvl then 
                error(186); { label referenced by goto at lesser statement level }
              putlabel(labname); { output label to intermediate }
            end else begin { not found }
              error(167); { undeclared label }
              newlabel(llp) { create a dummy level }
            end;
            insymbol;
            if sy = colon then insymbol else error(ErrColon)
          end;
        if not (sy in fsys + [ident]) then
          begin error(ErrIllSym); skip(fsys) end;
        if sy in statbegsys + [ident] then
          begin
            case sy of
              ident:    begin searchid([vars,field,func,proc],lcp); insymbol;
                          if lcp^.klass = proc then call(fsys,lcp)
                          else assignment(lcp)
                        end;
              beginsy:  begin insymbol; compoundstatement end;
              gotosy:   begin insymbol; gotostatement end;
              ifsy:     begin insymbol; ifstatement end;
              casesy:   begin insymbol; casestatement end;
              whilesy:  begin insymbol; whilestatement end;
              repeatsy: begin insymbol; repeatstatement end;
              forsy:    begin insymbol; forstatement end;
              withsy:   begin insymbol; withstatement end
            end;
            if not (sy in [semicolon,endsy,elsesy,untilsy]) then
              begin error(ErrIllSym); skip(fsys) end
          end
      end (*statement*) ;

    begin (*body*)
      if fprocp <> nil then entname := fprocp^.pfname
      else genlabel(entname);
      cstptrix := 0; topnew := lcaftermarkstack; topmax := lcaftermarkstack;
      putlabel(entname); genlabel(segsize); genlabel(stacktop);
      gencupent(32(*ent1*),1,segsize); gencupent(32(*ent2*),2,stacktop);
      if fprocp <> nil then (*copy multiple values into local cells*)
        begin llc1 := lcaftermarkstack;
          lcp := fprocp^.pflist;
          while lcp <> nil do
            with lcp^ do
              begin
                align(parmptr,llc1);
                if klass = vars then
                  if idtype <> nil then
                    if idtype^.form > power then
                      begin
                        if vkind = actual then
                          begin
                            gen2(50(*lda*),0,vaddr);
                            gen2t(54(*lod*),0,llc1,nilptr);
                            gen1(40(*mov*),idtype^.size);
                          end;
                        llc1 := llc1 + ptrsize
                      end
                    else llc1 := llc1 + idtype^.size;
                lcp := lcp^.next;
              end;
        end;
      lcmax := lc;
      addlvl;
      repeat
        repeat statement(fsys + [semicolon,endsy])
        until not (sy in statbegsys);
        test := sy <> semicolon;
        if not test then insymbol
      until test;
      sublvl;
      if sy = endsy then insymbol else error(13);
      llp := display[top].flabel; (*test for undefined labels*)
      while llp <> nil do
        with llp^ do
          begin
            if not defined then
              begin error(168);
                writeln(output); writeln(output,' label ',labval);
                write(output,SymSpace:chcnt+16)
              end;
            llp := nextlab
          end;
      if fprocp <> nil then
        begin
          if fprocp^.idtype = nil then gen1(PINSTret(*ret*),ord('p'))
          else gen0t(PINSTret(*ret*),fprocp^.idtype);
          align(parmptr,lcmax);
          if prcode then
            begin writeln(prr,'l',segsize:4,SymEqual,lcmax);
              writeln(prr,'l',stacktop:4,SymEqual,topmax)
            end
        end
      else
        begin gen1(PINSTret(*ret*),ord('p'));
          align(parmptr,lcmax);
          if prcode then
            begin writeln(prr,'l',segsize:4,SymEqual,lcmax);
              writeln(prr,'l',stacktop:4,SymEqual,topmax);
              writeln(prr,'q')
            end;
          ic := 0;
          (*generate call of main program; note that this call must be loaded
            at absolute address zero*)
          gen1(41(*mst*),0); gencupent(46(*cup*),0,entname); gen0(29(*stp*));
          if prcode then
            writeln(prr,'q');
          saveid := id;
          while fextfilep <> nil do
            begin
              with fextfilep^ do
                if not (strequri('input    ', filename) or 
                        strequri('output   ', filename) or
                        strequri('prd      ', filename) or 
                        strequri('prr      ', filename))
                then begin id := filename;
                       { output general error for undefined external file }
                       writeln(output);
                       writeln(output,'**** Error: external file unknown ''',
                                      fextfilep^.filename:8, SymQuote);
                       toterr := toterr+1;
                       { hold the error in case not found, since this error
                         occurs far from the original symbol }
                       searchidne([vars],llcp);
                       if llcp = nil then begin
                         { a header file was never defined in a var statement }
                         writeln(output);
                         writeln(output,'**** Error: Undeclared external file ''',
                                        fextfilep^.filename:8, SymQuote);
                         toterr := toterr+1;
                         llcp := uvarptr
                       end;
                       if llcp^.idtype<>nil then
                         if llcp^.idtype^.form<>files then
                           begin writeln(output);
                             writeln(output,'**** Error: Undeclared external file ''',
                                            fextfilep^.filename:8, SymQuote);
                             toterr := toterr+1
                           end
                     end;
                fp := fextfilep; fextfilep := fextfilep^.nextfile; putfil(fp);
            end;
          id := saveid;
          if prtables then
            begin writeln(output); printtables(true)
            end
        end;
    end (*body*) ;

  begin (*block*)
    stalvl := 0; { clear statement nesting level }
    dp := true;
    repeat
      if sy = labelsy then
        begin insymbol; labeldeclaration end;
      if sy = constsy then
        begin insymbol; constdeclaration end;
      if sy = typesy then
        begin insymbol; typedeclaration end;
      if sy = varsy then
        begin insymbol; vardeclaration end;
      while sy in [procsy,funcsy] do
        begin lsy := sy; insymbol; procdeclaration(lsy) end;
      if sy <> beginsy then
        begin error(18); skip(fsys) end
    until (sy in statbegsys) or isEndOfSourceFile;
    dp := false;
    if sy = beginsy then insymbol else error(ErrNoBegin);
    repeat body(fsys + [casesy]);
      if sy <> fsy then
        begin error(ErrIllSym); skip(fsys) end
    until ((sy = fsy) or (sy in blockbegsys)) or isEndOfSourceFile;
  end (*block*) ;

  procedure programme(fsys:setofsys);
    var extfp:extfilep;
  begin
    if sy = progsy then  {'Program' statement}
    begin
         insymbol;
         if sy <> ident then
            error(ErrIDExpect)
         else
         begin
            CompProgram;
            insymbol;
         end;
         if not (sy in [lparent,semicolon]) then error(ErrNoSemi);
         if sy = lparent  then
         begin
            repeat
              insymbol;
              if sy = ident then
              begin
                 getfil(extfp);
                 with extfp^ do
                 begin
                    filename := id;
                    nextfile := fextfilep
                 end;
                 fextfilep := extfp;
                 { check 'input' or 'output' appears in header for defaults }
                 if strequri('input    ', id) then
                    inputhdf := true
                 else if strequri('output   ', id) then
                    outputhdf := true;
                 insymbol;
                 if not ( sy in [comma,rparent] ) then
                   error(20)
              end
              else
                 error(ErrIDExpect)
            until sy <> comma;
            if sy <> rparent then
              error(ErrClosePar);
            insymbol;
            if sy <> semicolon then
              error(ErrNoSemi)
          end;
        if sy = semicolon then
          insymbol
    end
    else
      error(3);
    repeat
      block(fsys,period,nil);
      if sy <> period then
        error(21)
    until (sy = period) or isEndOfSourceFile;
    if list then
       writeln(output);
    if errinx <> 0 then
    begin
       list := false;
       endofline
    end;
  end (*programme*) ;


  procedure stdnames;
  begin
    { 'mark' and 'release' were removed and replaced with placeholders }
	
    {Manifest constant numbers replaced by identifiers}
    na[ STDFalse] := 'false    '; {1}
    na[ STDTrue] := 'true     ';  {2}
    na[ STDInput] := 'input    '; {3}
    na[STDOutput] := 'output   ';  {4}
    na[ 5] := 'get      '; na[ 6] := 'put      ';
    na[ 7] := 'reset    '; na[ 8] := 'rewrite  '; na[ 9] := 'read     ';
    na[10] := 'write    '; na[11] := 'pack     '; na[12] := 'unpack   ';
    na[13] := 'new      '; na[14] := '---      '; na[15] := 'readln   ';
    na[16] := 'writeln  ';
    na[17] := 'abs      '; na[18] := 'sqr      '; na[19] := 'trunc    ';
    na[20] := 'odd      '; na[21] := 'ord      '; na[22] := 'chr      ';
    na[23] := 'pred     '; na[24] := 'succ     '; na[25] := 'eof      ';
    na[26] := 'eoln     ';
    na[27] := 'sin      '; na[28] := 'cos      '; na[29] := 'exp      ';
    na[30] := 'sqrt     '; na[31] := 'ln       '; na[32] := 'arctan   ';
    na[33] := 'prd      '; na[34] := 'prr      '; na[35] := '---      ';
    na[36] := 'maxint   '; na[37] := 'round    '; na[38] := 'page     ';
    na[39] := 'dispose  ';
  end (*stdnames*) ;

  procedure enterstdtypes;

  begin                                                 (*type underlying:*)
                                                        (******************)

    new(intptr { ,scalar,standard -- tagfield } );
    pshstc(intptr);               (*integer*)
    with intptr^ do
      begin size := intsize; form := scalar; scalkind := standard end;
    new(realptr { ,scalar,standard-- tagfield } );
    pshstc(realptr);             (*real*)
    with realptr^ do
      begin size := realsize; form := scalar; scalkind := standard end;
    new(charptr { ,scalar,standard -- tagfield } );
    pshstc(charptr);             (*char*)
    with charptr^ do
      begin size := charsize; form := scalar; scalkind := standard end;
    new(boolptr { ,scalar,declared -- tagfield } );
    pshstc(boolptr);             (*boolean*)
    with boolptr^ do
      begin size := boolsize; form := scalar; scalkind := declared end;
    new(nilptr { ,pointer -- tagfield } );
    pshstc(nilptr);                (*nil*)
    with nilptr^ do
      begin eltype := nil; size := ptrsize; form := pointer end;
    (*for alignment of parameters*)
    new(parmptr { ,scalar,standard -- tagfield } );
    pshstc(parmptr);
    with parmptr^ do
      begin size := parmsize; form := scalar; scalkind := standard end ;
    new(textptr { ,files -- tagfield } );
    pshstc(textptr);                (*text*)
    with textptr^ do
      begin filtype := charptr; size := filesize+charsize; form := files end
  end (*enterstdtypes*) ;

  procedure entstdnames;
    var cp,cp1: ctp; i: integer;

  begin                                                       (*name:*)
                                                              (*******)

    new(cp { ,types -- tagfield } );
    ininam(cp);                                (*integer*)
    with cp^ do
      begin strassvr(name, 'integer  '); idtype := intptr; klass := types end;
    enterid(cp);
    new(cp { ,types -- tagfield } );
    ininam(cp);                                (*real*)
    with cp^ do
      begin strassvr(name, 'real     '); idtype := realptr; klass := types end;
    enterid(cp);
    new(cp { ,types -- tagfield } );
    ininam(cp);                                (*char*)
    with cp^ do
      begin strassvr(name, 'char     '); idtype := charptr; klass := types end;
    enterid(cp);
    new(cp { ,types -- tagfield } );
    ininam(cp);                                (*boolean*)
    with cp^ do
      begin strassvr(name, 'boolean  '); idtype := boolptr; klass := types end;
    enterid(cp);
    new(cp { ,types -- tagfield } ); ininam(cp);         (*text*)
    with cp^ do
      begin strassvr(name, 'text     '); idtype := textptr; klass := types end;
    enterid(cp);
    cp1 := nil;
	
    for i := STDFalse to STDTrue do
      begin new(cp { ,konst -- tagfield } );
        ininam(cp);                        (*false,true*)
        with cp^ do
          begin strassvr(name, na[i]); idtype := boolptr;
            next := cp1; values.ival := i - 1; klass := konst
          end;
        enterid(cp); cp1 := cp
      end;
    boolptr^.fconst := cp;
    for i := STDInput to STDOutput do
      begin new(cp { ,vars -- tagfield } );
        ininam(cp);                         (*input,output*)
        with cp^ do
          begin strassvr(name, na[i]); idtype := textptr; klass := vars;
            vkind := actual; next := nil; vlev := 1;
            vaddr := lcaftermarkstack+(i-3)*(filesize+charsize);
          end;
        enterid(cp);
        if i = STDInput then inputptr := cp else outputptr := cp
      end;
    for i:=STDPrd to STDPrr do
      begin new(cp { ,vars -- tagfield } );
        ininam(cp);                         (*prd,prr files*)
         with cp^ do
           begin strassvr(name, na[i]); idtype := textptr; klass := vars;
              vkind := actual; next := nil; vlev := 1;
              vaddr := lcaftermarkstack+(i-31)*(filesize+charsize);
           end;
         enterid(cp)
      end;
    for i := 5 to 16 do if i <> 14 then { no longer doing release }
      begin new(cp { ,proc,standard -- tagfield } );
        ininam(cp);                 (*get,put,reset*)
        with cp^ do                                          (*rewrite,read*)
          begin strassvr(name, na[i]); idtype := nil;        (*write,pack*)
            pflist := nil; next := nil; key := i - 4;        (*unpack,new*)
            klass := proc; pfdeckind := standard             (*readln,writeln*)
          end;
        enterid(cp)
      end;
    for i := 17 to 26 do
      begin new(cp { ,func,standard -- tagfield } );
        ininam(cp);                 (*abs,sqr,trunc*)
        with cp^ do                                          (*odd,ord,chr*)
          begin strassvr(name, na[i]); idtype := nil;        (*pred,succ,eof*)
            pflist := nil; next := nil; key := i - 16;
            klass := func; pfdeckind := standard
          end;
        enterid(cp)
      end;
    for i := 27 to 32 do
      begin 
        new(cp { ,vars -- tagfield } );
        ininam(cp);                   (*parameter of predeclared functions*)
        with cp^ do
          begin strassvr(name, '         '); idtype := realptr; klass := vars;
            vkind := actual; next := nil; vlev := 1; vaddr := 0
          end;
        new(cp1 { ,func,declared,actual -- tagfield } );
        ininam(cp1);            (*sin,cos,exp*)
        with cp1^ do                                (*sqrt,ln,arctan*)
          begin strassvr(name, na[i]); idtype := realptr; pflist := cp;
            forwdecl := false; externl := true; pflev := 0; pfname := i - 12;
            klass := func; pfdeckind := declared; pfkind := actual
          end;
        enterid(cp1)
      end;
    new(cp { ,konst -- tagfield } );
    ininam(cp);                                 (*maxint*)
    with cp^ do
      begin strassvr(name, na[36]); idtype := intptr;
        next := nil; values.ival := maxint; klass := konst
      end; enterid(cp);
    new(cp { ,func,standard -- tagfield } );
    ininam(cp);                         (*round*)
    with cp^ do
      begin strassvr(name, na[37]); idtype := nil;
        pflist := nil; next := nil; key := 16;
        klass := func; pfdeckind := standard
      end; enterid(cp);
    new(cp { ,proc,standard -- tagfield } );
    ininam(cp);                         (*page*)
    with cp^ do   
      begin strassvr(name, na[38]); idtype := nil; 
        pflist := nil; next := nil; key := 17;
        klass := proc; pfdeckind := standard
      end; enterid(cp);
    new(cp { ,proc,standard -- tagfield } );
    ininam(cp);                         (*dispose*)
    with cp^ do   
      begin strassvr(name, na[39]); idtype := nil; 
        pflist := nil; next := nil; key := 18;
        klass := proc; pfdeckind := standard
      end; enterid(cp)
  end (*entstdnames*) ;

  procedure enterundecl;
  begin
    new(utypptr { ,types -- tagfield } );
    ininam(utypptr);
    with utypptr^ do
      begin strassvr(name, '         '); idtype := nil; klass := types end;
    new(ucstptr { ,konst -- tagfield } );
    ininam(ucstptr);
    with ucstptr^ do
      begin strassvr(name, '         '); idtype := nil; next := nil;
        values.ival := 0; klass := konst
      end;
    new(uvarptr { ,vars -- tagfield } );
    ininam(uvarptr);
    with uvarptr^ do
      begin strassvr(name, '         '); idtype := nil; vkind := actual;
        next := nil; vlev := 0; vaddr := 0; klass := vars
      end;
    new(ufldptr { ,field -- tagfield } );
    ininam(ufldptr);
    with ufldptr^ do
      begin strassvr(name, '         '); idtype := nil; next := nil; fldaddr := 0;
        klass := field
      end;
    new(uprcptr { ,proc,declared,actual -- tagfield } );
    ininam(uprcptr);
    with uprcptr^ do
      begin strassvr(name, '         '); idtype := nil; forwdecl := false;
        next := nil; externl := false; pflev := 0; genlabel(pfname);
        klass := proc; pflist := nil; pfdeckind := declared; pfkind := actual
      end;
    new(ufctptr { ,func,declared,actual -- tagfield } );
    ininam(ufctptr);
    with ufctptr^ do
      begin strassvr(name, '         '); idtype := nil; next := nil;
        forwdecl := false; externl := false; pflev := 0; genlabel(pfname);
        klass := func; pflist := nil; pfdeckind := declared; pfkind := actual
      end
  end (*enterundecl*) ;

  { tear down storage allocations from enterundecl }
  procedure exitundecl;
  begin
    putnam(utypptr);
    putnam(ucstptr);                                  
    putnam(uvarptr);                                    
    putnam(ufldptr);                                    
    putnam(uprcptr);
    putnam(ufctptr);                                   
  end (*exitundecl*) ;

  procedure initscalars;
  var i: integer;
  begin fwptr := nil;
    prtables := false; list := true; prcode := true; debug := true;
    dp := true; errinx := 0;
    intlabel := 0; kk := maxids; fextfilep := nil;
    lc := lcaftermarkstack+filebuffer*(filesize+charsize);
    (* note in the above reservation of buffer store for 2 text files *)
    ic := 3; eol := false; linecount := 0;
    ch := SymSpace; chcnt := 0;
    mxint10 := maxint div 10;
    inputhdf := false; { set 'input' not in header files }
    outputhdf := false; { set 'output' not in header files }
	
	(*PR 8/2013*)
	realcode := FALSE; (* Default is P5 output. $R+ for P6. *)
	spinner := FALSE;  (* Don't run a spinner unless $S+ on P6 *)
	(*END UPDATE*)
	
    for i := 1 to 500 do errtbl[i] := false; { initialize error tracking }
    toterr := 0; { clear error count }
    { clear the recycling tracking counters }
    strcnt := 0; { strings }
    cspcnt := 0; { constants }
    stpcnt := 0; { structures }
    ctpcnt := 0; { identifiers }
    lbpcnt := 0; { label counts }
    filcnt := 0; { file tracking counts }
    cipcnt := 0 { case entry tracking counts }
  end (*initscalars*) ;

  procedure initsets;
  begin
    constbegsys := [addop,intconst,realconst,stringconst,ident];
    simptypebegsys := [lparent] + constbegsys;
    typebegsys:=[arrow,packedsy,arraysy,recordsy,setsy,filesy]+simptypebegsys;
    typedels := [arraysy,recordsy,setsy,filesy];
    blockbegsys := [labelsy,constsy,typesy,varsy,procsy,funcsy,beginsy];
    selectsys := [arrow,period,lbrack];
    facbegsys := [intconst,realconst,stringconst,ident,lparent,lbrack,notsy,nilsy];
    statbegsys := [beginsy,gotosy,ifsy,whilesy,repeatsy,forsy,withsy,casesy];
  end (*initsets*) ;

  procedure inittables;
    procedure reswords;
    var
        I:Integer; {PR: 2013-08-19: Let's let the compiler count. }
    begin
      {
           Note 'inc(i)' doesn't exist in this release of the
           compiler. Could add it but let's keep things simple
           for now so we'll stick with I := I+1.
	   
	   Also, the initialization of RSY used to be
           in separate 'procedure symbols' but since RW and RSY
           are tied together, if we need to add a new
	   reserved word, we could make a mistake; we'll
           move them up here.
	   
      }

      for i := 1 to maxres (*nr of res words*) do
        rop[i] := noop;
	  
	  {Frw tells the compiler where the words
           with this many letters starts}

      I := 1;
      frw[1] :=  I; frw[2] :=  I; 
      rw[ I] := 'if       '; rsy[ I] := ifsy;  I := I+1;
      rw[ I] := 'do       '; rsy[ I] := dosy;  I := I+1;
      rw[ I] := 'of       '; rsy[ I] := ofsy;  I := I+1;
      rw[ I] := 'to       '; rsy[ I] := tosy;  I := I+1;
      rw[ I] := 'in       '; rsy[ I] := relop; rop[I] := inop; I := I+1;
      rw[ I] := 'or       '; rsy[ I] := addop; rop[i] := orop; I := I+1;
	  
      frw[3] :=  I;
      rw[ I] := 'end      '; rsy[ I] := endsy; I := I+1;
      rw[ I] := 'for      '; rsy[ I] := forsy; I := I+1;
      rw[ I] := 'var      '; rsy[ I] := varsy; I := I+1;
      rw[ I] := 'div      '; rsy[ I] := mulop; rop[I] := idiv;  I := I+1;
      rw[ I] := 'mod      '; rsy[ I] := mulop; rop[i] := imod;  I := I+1;
      rw[ I] := 'set      '; rsy[ I] := setsy; I := I+1;
      rw[ I] := 'and      '; rsy[ I] := mulop; rop[I] := andop; I := I+1;
      rw[ I] := 'not      '; rsy[ I] := notsy; I := I+1;
      rw[ I] := 'nil      '; rsy[ I] := nilsy; I := I+1;
	  
      frw[4] := I;
      rw[ I] := 'then     '; rsy[I] := thensy; I := I+1;
      rw[ I] := 'else     '; rsy[I] := elsesy; I := I+1;
      rw[ I] := 'with     '; rsy[I] := withsy; I := I+1;
      rw[ I] := 'goto     '; rsy[I] := gotosy; I := I+1;
      rw[ I] := 'case     '; rsy[I] := casesy; I := I+1;
      rw[ I] := 'type     '; rsy[I] := typesy; I := I+1; 
      rw[ I] := 'file     '; rsy[I] := filesy; I := I+1; 
	  
      frw[5] := I;
      rw[ I] := 'begin    '; rsy[I] := beginsy; I := I+1;
      rw[ I] := 'until    '; rsy[I] := untilsy; I := I+1;      
      rw[ I] := 'while    '; rsy[I] := whilesy; I := I+1; 
      rw[ I] := 'array    '; rsy[I] := arraysy; I := I+1; 
      rw[ I] := 'const    '; rsy[I] := constsy; I := I+1; 
      rw[ I] := 'label    '; rsy[I] := labelsy; I := I+1;  
	  
      frw[6] := I;
      rw[ I] := 'repeat   '; rsy[I] := repeatsy; I := I+1;
      rw[ I] := 'record   '; rsy[I] := recordsy; I := I+1;      
      rw[ I] := 'downto   '; rsy[I] := downtosy; I := I+1; 
      rw[ I] := 'packed   '; rsy[I] := packedsy; I := I+1;      
	  
      frw[7] := I; 
      rw[ I] := 'program  '; rsy[I] := progsy;  I := I+1;
	  
      frw[8] := I;
      rw[ I] := 'function '; rsy[I] := funcsy;  I := I+1; 
     
      frw[9] := I;
      rw[ I] := 'procedure'; rsy[I] := procsy; I := I+1;
	  
      frw[10]:= I;

    end (*reswords*) ;

    procedure symbols;
{ Procedure Rators merged into this one; they're similar.
  Rators initialized sop }
      var i: integer;

    begin
      for i := ordminchar to ordmaxchar do
         sop[chr(i)] := noop;
      ssy[SymPlus] := addop ;       sop[SymPlus] := plus;
      ssy[SymMinus] := addop;       sop[SymMinus] := minus;
      ssy[SymStar] := mulop;        sop[SymStar] := mul;
      ssy[SymSlash] := mulop ;      sop[SymSlash] := rdiv;
      ssy[SymOpenPar] := lparent;
      ssy[SymClosePar] := rparent;
      ssy[SymDollar] := othersy ;
      ssy[SymEqual] := relop;       sop[SymEqual] := eqop;
      ssy[SymSpace] := othersy;
      ssy[SymComma] := comma ;
      ssy[SymPeriod] := period;
      ssy[SymQuote]:= othersy;
      ssy[SymOpenBracket] := lbrack ;
      ssy[SymCloseBracket] := rbrack;
      ssy[SymColon] := colon;
      ssy[SymUpArrow] := arrow ;
      ssy[SymLessThan] := relop;    sop[SymLessThan] := ltop;
      ssy[SymGreaterThan] := relop; sop[SymGreaterThan] := gtop;
      ssy[SymSemiColon] := semicolon;
      ssy[SymAt] := arrow;
    end  (*symbols*) ;

    procedure procmnemonics;
    begin
      { There are two mnemonics that have no counterpart in the
        assembler/interpreter: wro(7), pak(11). I didn't find a 
		generator for them, and suspect they are abandoned. }
      sna[ 1] :=' get'; pdx[ 1] := -1; { 1} 
	  sna[ 2] :=' put'; pdx[ 2] := -1; { 2}
      sna[ 3] :=' rdi'; pdx[ 3] := -1; { 3}
	  sna[ 4] :=' rdr'; pdx[ 4] := -1; { 4}
      sna[ 5] :=' rdc'; pdx[ 5] := -1; { 5}
	  sna[ 6] :=' wri'; pdx[ 6] := -2; { 6}
	  sna[ 7] :=' wro'; pdx[ 7] := -3; { 7}
	  sna[ 8] :=' wrr'; pdx[ 8] := -2; { 8}
      sna[ 9] :=' wrc'; pdx[ 9] := -2; { 9}
	  sna[10] :=' wrs'; pdx[10] := -3; {10}
	  sna[11] :=' pak'; pdx[11] :=  0; {11}
	  sna[12] :=' new'; pdx[12] := -2; {12}
      sna[13] :=' rst'; pdx[13] := -1; {13}
	  sna[14] :=' eln'; pdx[14] :=  0; {14}
	  sna[15] :=' sin'; pdx[15] :=  0; {15}
	  sna[16] :=' cos'; pdx[16] :=  0; {16}
      sna[17] :=' exp'; pdx[17] :=  0; {17}
	  sna[18] :=' sqt'; pdx[18] :=  0; {18}
	  sna[19] :=' log'; pdx[19] :=  0; {19}
	  sna[20] :=' atn'; pdx[20] :=  0; {20}
      sna[21] :=' rln'; pdx[21] :=  0; {21}
	  sna[22] :=' wln'; pdx[22] :=  0; {22}
	  sna[23] :=' sav'; pdx[23] := -1; {23}
	  
      { new procedure/function memonics for p5 }
      sna[24] :=' pag'; pdx[24] := -1; {24}  
	  sna[25] :=' rsf'; pdx[25] := -1; {25}  
	  sna[26] :=' rwf'; pdx[26] := -1; {26}  
	  sna[27] :=' wrb'; pdx[27] := -2; {27} 
      sna[28] :=' wrf'; pdx[28] := -3; {28}  
	  sna[29] :=' dsp'; pdx[29] := -2; {29} 
	  sna[30] :=' wbf'; pdx[30] := -2; {30} 
	  sna[31] :=' wbi'; pdx[31] := -1; {31}
      sna[32] :=' wbr'; pdx[32] := -1; {32} 
	  sna[33] :=' wbc'; pdx[33] := -1; {33} 
	  sna[34] :=' wbb'; pdx[34] := -1; {34} 
	  sna[35] :=' rbf'; pdx[35] := -2; {35}
      sna[36] :=' rsb'; pdx[36] := -1; {36} 
	  sna[37] :=' rwb'; pdx[37] := -1; {37} 
	  sna[38] :=' gbf'; pdx[38] := -2; {38} 
	  sna[39] :=' pbf'; pdx[39] := -2; {39}
    end (*procmnemonics*) ;

    procedure instrmnemonics;
	begin 
      mn[ PINSTabi] :=' abi'; cdx[PINSTabi] :=  0;  { 0}
      mn[ PINSTabr] :=' abr'; cdx[PINSTabr] :=  0;  { 1}
      mn[ PINSTadi] :=' adi'; cdx[PINSTadi] := -1;  { 2}
      mn[ PINSTadr] :=' adr'; cdx[PINSTadr] := -1;  { 3}
      mn[ 4] :=' and'; cdx[ 4] := -1; { 4}
	  mn[ 5] :=' dif'; cdx[ 5] := -1; { 5}
	  mn[ 6] :=' dvi'; cdx[ 6] := -1; { 6}
	  mn[ 7] :=' dvr'; cdx[ 7] := -1; { 7}
      mn[ 8] :=' eof'; cdx[ 8] :=  0; { 8}
	  mn[ PINSTflo] :=' flo'; cdx[ PINSTflo] :=  0; { 9}
	  mn[10] :=' flt'; cdx[10] :=  0; {10}
      mn[11] :=' inn'; cdx[11] := -1; {11}
      mn[12] :=' int'; cdx[12] := -1; {12}
	  mn[13] :=' ior'; cdx[13] := -1; {13}
	  mn[14] :=' mod'; cdx[14] := -1; {14}
	  mn[PINSTmpi] :=' mpi'; cdx[PINSTmpi] := -1; {15}
      mn[16] :=' mpr'; cdx[16] := -1; {16}
      mn[17] :=' ngi'; cdx[17] :=  0; {17}
      mn[18] :=' ngr'; cdx[18] :=  0; {18}
      mn[19] :=' not'; cdx[19] :=  0; {19}
      mn[20] :=' odd'; cdx[20] :=  0; {20}
	  mn[21] :=' sbi'; cdx[21] := -1; {21}
	  mn[22] :=' sbr'; cdx[22] := -1; {22}
      mn[23] :=' sgs'; cdx[23] :=  0; {23}
      mn[24] :=' sqi'; cdx[24] :=  0; {24}
	  mn[25] :=' sqr'; cdx[25] :=  0; {25}
	  mn[26] :=' sto'; cdx[26] := -2; {26}
	  mn[27] :=' trc'; cdx[27] :=  0; {27}
      mn[28] :=' uni'; cdx[28] := -1; {28}
	  mn[29] :=' stp'; cdx[29] :=  0; {29}
	  mn[30] :=' csp'; cdx[30] :=  0; {30}
	  mn[31] :=' dec'; cdx[31] :=  0; {31}
      mn[32] :=' ent'; cdx[32] :=  0; {32}
	  mn[33] :=' fjp'; cdx[33] := -1; {33}
	  mn[34] :=' inc'; cdx[34] :=  0; {34}
	  mn[35] :=' ind'; cdx[35] :=  0; {35}
      mn[36] :=' ixa'; cdx[36] := -1; {36}
	  mn[37] :=' lao'; cdx[37] := +1; {37} 
	  mn[38] :=' lca'; cdx[38] := +1; {38}
	  mn[39] :=' ldo'; cdx[39] := +1; {39}
      mn[40] :=' mov'; cdx[40] := -2; {40} 
	  mn[41] :=' mst'; cdx[41] :=  0; {41}  
	  mn[PINSTret] :=' ret'; cdx[PINSTret] :=  0; {42}
      mn[43] :=' sro'; cdx[43] := -1; {43}
      mn[44] :=' xjp'; cdx[44] := -1; {44} 
	  mn[45] :=' chk'; cdx[45] :=  0; {45} 
	  mn[46] :=' cup'; cdx[46] :=  0; {46} 
	  mn[47] :=' equ'; cdx[47] := -1; {47} 
      mn[48] :=' geq'; cdx[48] := -1; {48} 
	  mn[49] :=' grt'; cdx[49] := -1; {49} 
	  mn[50] :=' lda'; cdx[50] := +1; {50} 
	  mn[PINSTldc] :=' ldc'; cdx[PINSTldc] := +1; {51}
      mn[52] :=' leq'; cdx[52] := -1; {52}
	  mn[53] :=' les'; cdx[53] := -1; {53}
	  mn[54] :=' lod'; cdx[54] := +1; {54}
	  mn[55] :=' neq'; cdx[55] := -1; {55}
      mn[PINSTstr] :=' str'; cdx[PINSTstr] := -1; {56}
	  mn[57] :=' ujp'; cdx[57] :=  0; {57}
	  mn[58] :=' ord'; cdx[58] :=  0; {58}
	  mn[59] :=' chr'; cdx[59] :=  0; {59}
      mn[PINSTujc] :=' ujc'; cdx[PINSTujc] :=  0; {60}
	  
      { new instruction memonics for p5 }
      mn[PINSTrnd] :=' rnd'; cdx[PINSTrnd] :=  0; {61}
      mn[62] :=' pck';  cdx[62] := -3; {62} 
	  mn[63] :=' upk';  cdx[63] := -3; {63}
	  mn[64] :=' rgs';  cdx[64] := -1; {64}
      mn[PINSTfbv] :=' fbv'; cdx[PINSTfbv] :=  0; {65}
	  mn[66] :=' ipj'; cdx[66] :=  0; {66}
	  mn[PINSTcip] :=' cip'; cdx[PINSTcip] := -1; {67}
	  mn[PINSTlpa] :=' lpa'; cdx[PINSTlpa] := +2; {68}
      mn[PINSTefb] :=' efb'; cdx[PINSTefb] :=  0; {69}
	  mn[PINSTfvb] :=' fvb'; cdx[PINSTfvb] := -1; {70}
	  mn[PINSTdmp] :=' dmp'; cdx[PINSTdmp] := -1; {71}
	  mn[PINSTswp] :=' swp'; cdx[PINSTswp] :=  0; {72}
      mn[PINSTtjp] :=' tjp'; cdx[PINSTtjp] := -1; {73}
	  mn[PINSTlip] :=' lip'; cdx[PINSTlip] := +2; {74}
	  
    end (*instrmnemonics*) ;

    procedure chartypes;
    var i : integer;
    begin
      for i := ordminchar to ordmaxchar do chartp[chr(i)] := illegal;
      chartp['a'] := letter  ;
      chartp['b'] := letter  ; chartp['c'] := letter  ;
      chartp['d'] := letter  ; chartp['e'] := letter  ;
      chartp['f'] := letter  ; chartp['g'] := letter  ;
      chartp['h'] := letter  ; chartp['i'] := letter  ;
      chartp['j'] := letter  ; chartp['k'] := letter  ;
      chartp['l'] := letter  ; chartp['m'] := letter  ;
      chartp['n'] := letter  ; chartp['o'] := letter  ;
      chartp['p'] := letter  ; chartp['q'] := letter  ;
      chartp['r'] := letter  ; chartp['s'] := letter  ;
      chartp['t'] := letter  ; chartp['u'] := letter  ;
      chartp['v'] := letter  ; chartp['w'] := letter  ;
      chartp['x'] := letter  ; chartp['y'] := letter  ;
      chartp['z'] := letter  ; 
      chartp['A'] := letter  ;
      chartp['B'] := letter  ; chartp['C'] := letter  ;
      chartp['D'] := letter  ; chartp['E'] := letter  ;
      chartp['F'] := letter  ; chartp['G'] := letter  ;
      chartp['H'] := letter  ; chartp['I'] := letter  ;
      chartp['J'] := letter  ; chartp['K'] := letter  ;
      chartp['L'] := letter  ; chartp['M'] := letter  ;
      chartp['N'] := letter  ; chartp['O'] := letter  ;
      chartp['P'] := letter  ; chartp['Q'] := letter  ;
      chartp['R'] := letter  ; chartp['S'] := letter  ;
      chartp['T'] := letter  ; chartp['U'] := letter  ;
      chartp['V'] := letter  ; chartp['W'] := letter  ;
      chartp['X'] := letter  ; chartp['Y'] := letter  ;
      chartp['Z'] := letter  ; 
      chartp['0'] := number  ;
      chartp['1'] := number  ; chartp['2'] := number  ;
      chartp['3'] := number  ; chartp['4'] := number  ;
      chartp['5'] := number  ; chartp['6'] := number  ;
      chartp['7'] := number  ; chartp['8'] := number  ;
      chartp['9'] := number  ;
      chartp[SymPlus]     := special ;
      chartp[SymMinus]    := special ;
      chartp[SymStar]     := special ;
      chartp[SymSlash]    := special ;
      chartp[SymOpenPar]  := chlparen;
      chartp[SymClosePar] := special ;
      chartp[SymDollar]   := special ;
      chartp[SymEqual]    := special ;
      chartp[SymSpace]    := chspace ;
      chartp[SymComma]    := special ;
      chartp[SymPeriod]   := chperiod;
      chartp[SymQuote]    := chstrquo;
      chartp[SymOpenBracket] := special ;
      chartp[SymCloseBracket] := special ;
      chartp[SymColon] := chcolon ;
      chartp[SymUpArrow] := special ;
      chartp[SymSemiColon] := special ;
      chartp[SymLessThan] := chlt    ;
      chartp[SymGreaterThan] := chgt    ;
      chartp[SymOpenComment] := chlcmt  ;
      chartp[SymCloseComment] := special ;
      chartp[SymAt] := special ;
	  
	  {2013-08-19 PR: Add special characters }
      chartp[SymHashTag] := special ; { add # for character values }
      chartp[SymAmpersand] := special ;
      chartp[SymPercent] := special ;
	  
         {With one statement we add underlines
          to the list of valid identifiers}
      chartp[SymUnderscore] := letter  ;
	  
     { END UPDATE }

      ordint['0'] := 0; ordint['1'] := 1; ordint['2'] := 2;
      ordint['3'] := 3; ordint['4'] := 4; ordint['5'] := 5;
      ordint['6'] := 6; ordint['7'] := 7; ordint['8'] := 8;
      ordint['9'] := 9;
    end;


  begin (*inittables*)
    reswords; symbols;
    { Procedure Rators initialization merged into Symbols }
    instrmnemonics; procmnemonics;
    chartypes;
  end (*inittables*) ;

begin

  writeln('P6 Pascal compiler version ', majorver:1, SymPeriod, minorver:1);
  writeln;

  (*initialize*)
  (************)
  initscalars; initsets; inittables;


  (*enter standard names and standard types:*)
  (******************************************)
  level := 0; top := 0;
   with display[0] do
   begin
      fname := nil;
      flabel := nil;
      fconst := nil;
      fstruct := nil;
      occur := blck;
      bname := nil
   end;
   enterstdtypes;
   stdnames;
   entstdnames;
   enterundecl;
   top := 1;
   level := 1;
   with display[1] do
   begin
       fname := nil;
       flabel := nil;
       fconst := nil;
       fstruct := nil;
       occur := blck;
       bname := nil
   end;

  (*compile:*)
  (**********)

{ 2013-08-25: PR Temporary file assignments just to get the thing
  to read a file }


  assign(Prr,'prr.out');
  {elide}rewrite(prr);{noelide} { open output file }
  proglinecurrent := 0;
  ch := ' ';


  { write generator comment }
  
  {PR 2013-08-19: Select comment indicator; }
  if realcode then 
    commentprefix := P6Comment
  else
    commentprefix := P5Comment;
  {END UPDATE}	
	
  writeln(prr, commentprefix);
  write(prr, commentprefix,' Pascal ');
  if realcode then 
     write(prr,' assembly ') 
  else 
     write(prr,' intermediate ');
  writeln(prr,'file Generated by P6 Pascal compiler version ', 
          majorver:1, SymPeriod, minorver:1);
  writeln(prr, commentprefix);
  insymbol;
  programme(blockbegsys+statbegsys-[casesy]);

  { dispose of levels 0 and 1 }
  putdsp(1);
  putdsp(0);

  { remove undeclared ids }
  exitundecl;

  writeln;
  writeln('Errors in program: ', toterr:1);
  { output error report as required }
  f := true;
  for i := 1 to 500 do if errtbl[i] then begin
    if f then begin
      writeln;
      writeln('Error numbers in listing:');
      writeln('-------------------------');
      f := false
    end;
    write(i:3, '  '); errmsg(i); writeln
  end;
  if not f then writeln;

  if doprtryc then begin { print recyling tracking counts }
  
    writeln;
    writeln('Recycling tracking counts:');
    writeln;
    writeln('string quants:              ', strcnt:1);
    writeln('constants:                  ', cspcnt:1);
    writeln('structures:                 ', stpcnt:1);
    writeln('identifiers:                ', ctpcnt:1);
    writeln('label counts:               ', lbpcnt:1);
    writeln('file tracking counts:       ', filcnt:1);
    writeln('case entry tracking counts: ', cipcnt:1);
    writeln;

  end;

  { perform errors for recycling balance }

  if strcnt <> 0 then 
     writeln('*** Error: Compiler internal error: string recycle balance: ', 
             strcnt:1);
  if cspcnt <> 0 then 
     writeln('*** Error: Compiler internal error: constant recycle balance: ', 
             cspcnt:1);
  if stpcnt <> 0 then 
     writeln('*** Error: Compiler internal error: structure recycle balance: ', 
             stpcnt:1);
  if ctpcnt <> 0 then 
     writeln('*** Error: Compiler internal error: identifier recycle balance: ',
             ctpcnt:1);
  if lbpcnt <> 0 then 
     writeln('*** Error: Compiler internal error: label recycle balance: ', 
             lbpcnt:1);
  if filcnt <> 0 then 
     writeln('*** Error: Compiler internal error: file recycle balance: ', 
             filcnt:1);
  if cipcnt <> 0 then 
     writeln('*** Error: Compiler internal error: case recycle balance: ', 
             cipcnt:1);

  99:

end.
