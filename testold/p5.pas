

(**********************************************************************************)
(*$c+,t-,d-,l-                                                                    *)
(******************************************************************************** *)
(**                                                                              **)
(**                     Portable Pascal assembler/interpreter                    **)
(**                     *************************************                    **)
(**                                                                              **)
(**                                 Pascal P5                                    **)
(**                                                                              **)
(**                                 ETH May 76                                   **)
(**                                                                              **)
(** Authors:                                                                     **)
(**    Urs Ammann                                                                **)
(**    Kesav Nori                                                                **)
(**    Christian Jacobi                                                          **)
(**    K. Jensen                                                                 **)
(**    N. Wirth                                                                  **)
(**                                                                              **)
(**    Address:                                                                  **)
(**       Institut Fuer Informatik                                               **)
(**       Eidg. Technische Hochschule                                            **)
(**       CH-8096 Zuerich                                                        **)
(**                                                                              **)
(**  This code is fully documented in the book                                   **)
(**        "Pascal Implementation"                                               **)
(**   by Steven Pemberton and Martin Daniels                                     **)
(** published by Ellis Horwood, Chichester, UK                                   **)
(**         ISBN: 0-13-653-0311                                                  **)
(**       (also available in Japanese)                                           **)
(**                                                                              **)
(** Steven Pemberton, CWI/AA,                                                    **)
(** Kruislaan 413, 1098 SJ Amsterdam, NL                                         **)
(** Steven.Pemberton@cwi.nl                                                      **)
(**                                                                              **)
(** Adaption from P4 to P5 by:                                                   **)
(**                                                                              **)
(**    Scott A. Moore                                                            **)
(**    samiam@moorecad.com                                                       **)
(**                                                                              **)
(**    The comments marked with brackets are mine [sam]                          **)
(**                                                                              **)
(** P5 is an extended version of P4 with the following goals:                    **)
(**                                                                              **)
(** 1. The remaining unimplemented functions of Pascal are implemented, so that  **)
(**    P5 is no longer a "subset" of full Pascal. This was done because it is    **)
(**    no longer necessary to produce a minimum size implementation, and it      **)
(**    allows any standard program to be used with P5.                           **)
(**                                                                              **)
(** 2. The P5 compiler is brought up to ISO 7185 level 0 standards, both in the  **)
(**    language it compiles for, and the language it is implemented in.          **)
(**                                                                              **)
(** 3. The internal storage efficiency is increased. For example, character      **)
(**    strings no longer take as much space per character as integers and other  **)
(**    data. Sets are placed in their own space so that the minimum stack size   **)
(**    not determined by set size.                                               **)
(**                                                                              **)
(** 4. The remaining limitations and dependencies on the CDC 6000 version are    **)
(**    removed. For example, the instruction store no longer is packed 2         **)
(**    instructions to a 60 bit word.                                            **)
(**                                                                              **)
(** 5. General clean up. Longstanding bugs and issues are addressed. Constants   **)
(**    that were buried in the source (magic numbers) were made constants. The   **)
(**    type 'alpha' (specific to CDC 6000) was replaced with idstr, etc.         **)
(**                                                                              **)
(** The idea of P5 is to obtain a compiler that is ISO 7185 compliant, can       **)
(** compile itself, can compile any reasonable standard program, and is          **)
(** efficient enough to be used as a normal compiler for some certain uses.      **)
(** Finally, it can serve as a starting implementation for native compilers.     **)
(**                                                                              **)
(** P5 machine instructions added:                                               **)
(**                                                                              **)
(** rnd    round:   expects a float on stack, performs round() and places the    **)
(**                 result back on the stack as an integer.                      **)
(**                                                                              **)
(** pck ln pack:    Expects a packed array on stack top, followed by the         **)
(**                 starting subscript, then the unpacked array. The parameter   **)
(**                 contains the length of packed array in elements. Performs    **)
(**                 pack(upa, ss, pa) and removes all from stack. The starting   **)
(**                 subscript is zero based and scaled to the element size.      **)
(**                                                                              **)
(** upk ln pack:    Expects the starting subscript on stack top, followed by the **)
(**                 unpacked array, then the packed array. The parameter         **)
(**                 contains the length of packed array in elements. Performs    **)
(**                 unpack(pa, upa, ss) and removes all from stack. The starting **)
(**                 subscript is zero based and scaled to the element size.      **)
(**                                                                              **)
(** rgs    set rng: Expects a set range specification on stack, with the last    **)
(**                 value on the top, and the first value next. The two values   **)
(**                 are replaced with a set with all of the values between and   **)
(**                 including the first and last values.                         **)
(**                                                                              **)
(** fbv ad buf val: Validates a file buffer variable. Expects a file address on  **)
(**                 stack. The buffer is "validated" for lazy I/O, which means   **)
(**                 that if the associated file is in read mode, the delayed     **)
(**                 read to the buffer variable occurs. The file address remains **)
(**                 on the stack.                                                **)
(**                                                                              **)
(** ipj v l ip jmp: Interprocedure jump. Contains the level of the target        **)
(**                 procedure, and the label to jump to. The stack is adjusted   **)
(**                 to remove all nested procedures/functions, then the label is **)
(**                 unconditionally jumped to.                                   **)
(**                                                                              **)
(** cip p           Call indirect procedure/function. The top of stack has the   **)
(**                 address of a mp/address pair pushed by lpa. The dl of the    **)
(**                 current mark is replaced by the mp, and the address replaces **)
(**                 the current pc. The mp/ad address is removed from stack.     **)
(**                                                                              **)
(** lpa p l q       Load procedure address. The current mark pointer is loaded   **)
(**                 onto the stack, followed by the target procedure or function **)
(**                 address. This puts enough information on the stack to call   **)
(**                 it with the callers environment.                             **)
(**                                                                              **)
(** lip p q         load procedure function address. Loads a mark/address pair   **)
(**                 for a procedure or function parameter onto the stack. Used   **)
(**                 to pass a procedure or function parameter to another         **)
(**                 procedure or function.                                       **)
(**                                                                              **)
(** efb    eof:     Find eof for binary file. The top of stack is a logical file **)
(**                 number. The eof boolean vale replaces it.                    **)
(**                                                                              **)
(** fvb ad buf val: Expects the length of the file buffer on stack, and the file **)
(**                 address under that. The buffer is "validated" for lazy I/O,  **)
(**                 which means that if the associated file is in read mode, the **)
(**                 delayed read to the buffer variable occurs. The buffer       **)
(**                 length is removed only.                                      **)
(**                                                                              **)
(** dmp q           Subtracts the value from the stack top. Used to dump the top **)
(**                 of the stack.                                                **)
(**                                                                              **)
(** swp q           Pulls the second on stack to the top, swapping the top to    **)
(**                 elements. The size of the second on stack is specified, but  **)
(**                 the top of the on stack is implied as a pointer.             **)
(**                                                                              **)
(** tjp q           Expects a boolean on stack. Jumps to the address if the      **)
(**                 value is true. Removes the value from the stack.             **)
(**                                                                              **)
(** P5 machine built in procedures/functions added:                              **)
(**                                                                              **)
(** pag    page:    Expects a logical file number on stack top. Performs page(). **)
(**                                                                              **)
(** rsf    reset:   Expects a logical file number on stack top. Performs         **)
(**                 reset() and sets the file to text mode.                      **)
(**                                                                              **)
(** rwf    rewrite: Expects a logical file number on stack top. Performs         **)
(**                 reset() and sets the file to text mode.                      **)
(**                                                                              **)
(** wrb    write:   Expects a field number on stack top, followed by a boolean   **)
(**                 to print, then the logical file number. The boolean is       **)
(**                 output as per ISO 7185.                                      **)
(**                                                                              **)
(** rgs    set rng: Expects a set range specification on stack, with the last    **)
(**                 value on the top, and the first value next. The two values   **)
(**                 are replaced with a set with all of the values between and   **)
(**                 including the first and last values.                         **)
(**                                                                              **)
(** wrf    write:   Expects a logical file number on stack top, followed by a    **)
(**                 field number, then a fraction, then a real to print. The     **)
(**                 real is output in r:f:f (fraction) format. All but the file  **)
(**                 are removed from stack.                                      **)
(**                                                                              **)
(** wbf    write:   Expects a file address on stack top, followed by the length  **)
(**                 of the type to write, then the variable address to write     **)
(**                 from. Writes binary store to the file.                       **)
(**                                                                              **)
(** wbi    write:   Expects a file address on stack top, followed by an integer. **)
(**                 Writes the integer to the file in binary format.             **)
(**                                                                              **)
(** wbr    write:   Expects a file address on stack top, followed by a real.     **)
(**                 Writes the real to the file in binary format.                **)
(**                                                                              **)
(** wbc    write:   Expects a file address on stack top, followed by a           **)
(**                 character. Writes the character to the file in binary        **)
(**                 format.                                                      **)
(**                                                                              **)
(** wbb    write:   Expects a file address on stack top, followed by a boolean.  **)
(**                 Writes the boolean to the file in binary format.             **)
(**                                                                              **)
(** rbf    read:    Expects a file address on stack top, followed by the length  **)
(**                 of the type to read, then the variable address to read       **)
(**                 from. Reads binary store from the file.                      **)
(**                                                                              **)
(** rsb    reset:   Expects a logical file number on stack top. Performs         **)
(**                 reset() and sets the file to binary mode.                    **)
(**                                                                              **)
(** rwb    rewrite: Expects a logical file number on stack top. Performs         **)
(**                 reset() and sets the file to binary mode.                    **)
(**                                                                              **)
(** gbf    get:     Get file binary. Expects the length of a file element on     **)
(**                 stack top, followed by a pointer to the file. The next file  **)
(**                 element is loaded to the file buffer.                        **)
(**                                                                              **)
(** pbf    put:     Put file binary. Expects the length of a file element on     **)
(**                 stack top, followed by a pointer to the file. Writes the     **)
(**                 file buffer to thr file.                                     **)
(**                                                                              **)
(** Note that the previous version of P4 added some type specified instructions  **)
(** that used to be unified, typeless instructions.                              **)
(**                                                                              **)
(** P5 errors added:                                                             **)
(**                                                                              **)
(** 182 identifier too long                                                      **)
(** 183 For index variable must be local to this block                           **)
(** 184 Interprocedure goto does not reference outter block of destination       **)
(** 185 Goto references deeper nested statement                                  **)
(** 186 Label referenced by goto at lesser statement level                       **)
(** 187 Goto references label in different nested statement                      **)
(** 188 Label referenced by goto in different nested statement                   **)
(** 189 Parameter lists of formal and actual parameters not congruous.           **)
(**                                                                              **)
(** P5 instructions modified:                                                    **)
(**                                                                              **)
(** lca'string'       '                                                          **)
(**                                                                              **)
(** was changed to                                                               **)
(**                                                                              **)
(** lca 'string'''                                                               **)
(**                                                                              **)
(** That is, lca has a space before the opening quote, no longer pads to the     **)
(** right, and represents single quotes with a quote image. pint converts quote  **)
(** images back to single quotes, and pads out strings to their full length.     **)
(**                                                                              **)
(** In addition, the way files work was extensively modified. Original P5 could  **)
(** not represent files as full1y expressed variables, such as within an array   **)
(** or record, and were effectively treated as constants. To treat them as true  **)
(** variable accesses, the stacking order of the file in all file subroutines    **)
(** was changed so that the file is on the bottom. This matches the source       **)
(** order of the file in write(f, ...) or read(f, ...). Also, the file           **)
(** operations now leave the file on the stack for the duration of a write or    **)
(** read, then dump them using a specific new instruction "dmp". This allows     **)
(** multiparameter writes and reads to be effectively a chain of single          **)
(** operations using one file reference. Finally, files were tied to the type    **)
(** ending 'a', because files are now full variable references.                  **)
(**                                                                              **)
(******************************************************************************** *)
(**********************************************************************************)

program PASCALCOMPILER ( INPUT , OUTPUT , PRR ) ;


label 99 ; (* terminate immediately *)


const

      (*                                                                          *)
      (*                                                                          *)
      (*   Program object sizes and characteristics, sync with pint. These define *)
      (*   the machine specific characteristics of the target.                    *)
      (*                                                                          *)
      (*   This configuration is for a 32 bit machine as follows:                 *)
      (*                                                                          *)
      (*   integer               32  bits                                         *)
      (*   real                  64  bits                                         *)
      (*   char                  8   bits                                         *)
      (*   boolean               8   bits                                         *)
      (*   set                   256 bits                                         *)
      (*   pointers              32  bits                                         *)
      (*   marks                 32  bits                                         *)
      (*   File logical number   8   bits                                         *)
      (*                                                                          *)
      (*   Both endian types are supported. There is no alignment needed, but you *)
      (*   may wish to use alignment to tune the runtime speed.                   *)
      (*                                                                          *)
      (*   The machine characteristics dependent on byte accessable machines. This*)
      (*   table is all you should need to adapt to any byte addressable machine. *)
      (*                                                                          *)
      (*                                                                          *)

      INTSIZE = 4 ;        (* size of integer *)
      INTAL = 4 ;          (* memory alignment of integer *)
      REALSIZE = 8 ;       (* size of real *)
      REALAL = 4 ;         (* memory alignment of real *)
      CHARSIZE = 1 ;       (* size of char *)
      CHARAL = 1 ;         (* memory alignment of char *)
      CHARMAX = 1 ;
      BOOLSIZE = 1 ;       (* size of boolean *)
      BOOLAL = 1 ;         (* alignment of boolean *)
      PTRSIZE = 4 ;        (* size of pointer *)
      ADRSIZE = 4 ;        (* size of address *)
      ADRAL = 4 ;          (* alignment of address *)
      SETSIZE = 32 ;       (* size of set *)
      SETAL = 1 ;          (* alignment of set *)
      FILESIZE = 1 ;       (* required runtime space for file (lfn) *)
      FILEIDSIZE = 1 ;     (* size of the lfn only *)
      STACKAL = 4 ;        (* alignment of stack *)
      STACKELSIZE = 4 ;    (* stack element size *)
      MAXSIZE = 32 ;

      (*****************************************************)
      (* this is the largest type that can be on the stack *)
      (*****************************************************)

      HEAPAL = 4 ;         (* alignment for each heap arena *)
      SETHIGH = 255 ;      (* Sets are 256 values *)
      SETLOW = 0 ;
      ORDMAXCHAR = 255 ;   (* Characters are 8 bit ISO/IEC 8859-1 *)
      ORDMINCHAR = 0 ;
      MAXRESULT = REALSIZE ; (* maximum size of function result *)
      MARKSIZE = 32 ;

      (****************************************************************************)
      (* maxresult+6*ptrsize                                                      *)
      (* Value of nil is 1 because this allows checks for pointers that were      *)
      (*     initialized, which would be zero (since we clear all space to zero). *)
      (*     In the new unified code/data space scheme, 0 and 1 are always invalid*)
      (*     addresses, since the startup code is at least that long.             *)
      (****************************************************************************)

      NILVAL = 1 ;         (* value of 'nil' *)

      (******************************************)
      (* end of pcom and pint common parameters *)
      (******************************************)

      DISPLIMIT = 300 ;
      MAXLEVEL = 255 ;

      (********************************************************************************)
      (* strglgth used to define the size of all strings in pcom and pint. With the   *)
      (*     string quanta system, string lengths are effectively unlimited, but there*)
      (*     it still sets the size of some buffers in pcom.                          *)
      (********************************************************************************)

      STRGLGTH = 250 ;

      (*****************************************************************)
      (* maximum number of digits in real, including sign and exponent *)
      (*****************************************************************)

      DIGMAX = 250 ;

      (*********************************************************************************)
      (* lcaftermarkstack is a very pcom specific way of stating the size of a mark    *)
      (*     in pint. However, it is used frequently in Perberton's documentation, so I*)
      (*     left it, but equated it to the more portable marksize.                    *)
      (*********************************************************************************)

      LCAFTERMARKSTACK = MARKSIZE ;
      FILEAL = CHARAL ;

      (***********************************************************)
      (* stackelsize = minimum size for 1 stackelement           *)
      (*                  = k*stackal                            *)
      (*      stackal     = scm(all other al-constants)          *)
      (*      charmax     = scm(charsize,charal)                 *)
      (*                    scm = smallest common multiple       *)
      (*      lcaftermarkstack >= maxresult+3*ptrsize+max(x-size)*)
      (*                        = k1*stackelsize                 *)
      (***********************************************************)

      MAXSTACK = 1 ;
      PARMAL = STACKAL ;
      PARMSIZE = STACKELSIZE ;
      RECAL = STACKAL ;
      FILEBUFFER = 4 ;  (* number of system defined files *)
      MAXADDR = MAXINT ;
      MAXSP = 39 ;  (* number of standard procedures/functions *)
      MAXINS = 74 ; (* maximum number of instructions *)
      MAXIDS = 250 ;

      (************************************************************)
      (* maximum characters in id string (basically, a full line) *)
      (************************************************************)

      MAXSTD = 39 ; (* number of standard identifiers *)
      MAXRES = 35 ; (* number of reserved words *)
      RESLEN = 9 ;  (* maximum length of reserved words *)
      VARSQT = 10 ; (* variable string quanta *)
      PRTLLN = 10 ; (* number of label characters to print in dumps *)

      (*********************************)
      (* default field sizes for write *)
      (*********************************)

      INTDEFF = 11 ; (* default field length for integer *)
      RELDEFF = 22 ; (* default field length for real *)
      CHRDEFF = 1 ;  (* default field length for char (usually 1) *)
      BOLDEFF = 5 ;

      (***********************************************************)
      (* default field length for boolean (usually 5 for 'false' *)
      (***********************************************************)


      (***************)
      (* debug flags *)
      (***************)

      DODMPLEX = FALSE ; (* dump lexical *)
      DOPRTRYC = FALSE ; (* dump recycling tracker counts *)

      (*******************)
      (* version numbers *)
      (*******************)

      MAJORVER = 1 ; (* major version number *)
      MINORVER = 0 ; (* minor version number *)


type                                                        (*describing:*)
                                                   (*************)

     (*********************)
     (*marktype= ^integer;*)
     (*basic symbols      *)
     (**************      *)
     (*********************)

     SYMBOL = ( IDENT , INTCONST , REALCONST , STRINGCONST , NOTSY ,
              MULOP , ADDOP , RELOP , LPARENT , RPARENT , LBRACK ,
              RBRACK , COMMA , SEMICOLON , PERIOD , ARROW , COLON ,
              BECOMES , RANGE , LABELSY , CONSTSY , TYPESY , VARSY ,
              FUNCSY , PROGSY , PROCSY , SETSY , PACKEDSY , ARRAYSY ,
              RECORDSY , FILESY , BEGINSY , IFSY , CASESY , REPEATSY ,
              WHILESY , FORSY , WITHSY , GOTOSY , ENDSY , ELSESY ,
              UNTILSY , OFSY , DOSY , TOSY , DOWNTOSY , THENSY , NILSY
              , OTHERSY ) ;
     OPERATOR = ( MUL , RDIV , ANDOP , IDIV , IMOD , PLUS , MINUS ,
                OROP , LTOP , LEOP , GEOP , GTOP , NEOP , EQOP , INOP ,
                NOOP ) ;
     SETOFSYS = set of SYMBOL ;
     CHTP = ( LETTER , NUMBER , SPECIAL , ILLEGAL , CHSTRQUO , CHCOLON
            , CHPERIOD , CHLT , CHGT , CHLPAREN , CHSPACE , CHLCMT ) ;

     (******************************************************************************)
     (* Here is the variable length string containment to save on space. strings   *)
     (*       strings are only stored in their length rounded to the nearest 10th. *)
     (******************************************************************************)

     STRVSP = -> STRVS ; (* pointer to variable length id string *)
     STRVS = record      (* id string variable length *)
               STR : packed array [ 1 .. VARSQT ] of CHAR ;

     (******************)
     (* data contained *)
     (******************)

               NEXT : STRVSP  (* next *)
             end ;

     (***********)
     (*constants*)
     (***********)
     (***********)

     SETTY = set of SETLOW .. SETHIGH ;
     CSTCLASS = ( REEL , PSET , STRG ) ;
     CSP = -> CONSTANT ;
     CONSTANT = record
                  NEXT : CSP ;   (* next entry link *)
                  case CCLASS : CSTCLASS of
                    REEL :
                      ( RVAL : STRVSP ) ;
                    PSET :
                      ( PVAL : SETTY ) ;
                    STRG :
                      ( SLGTH : 0 .. STRGLGTH ;
                        SVAL : STRVSP )
                end ;
     VALU = record
              case INTVAL : BOOLEAN of      (*intval never set nor tested*)
                TRUE :
                  ( IVAL : INTEGER ) ;
                FALSE :
                  ( VALP : CSP )
            end ;

     (*****************)
     (*data structures*)
     (*****************)
     (*****************)

     LEVRANGE = 0 .. MAXLEVEL ;
     ADDRRANGE = 0 .. MAXADDR ;
     STRUCTFORM = ( SCALAR , SUBRANGE , POINTER , POWER , ARRAYS ,
                  RECORDS , FILES , TAGFLD , VARIANT ) ;
     DECLKIND = ( STANDARD , DECLARED ) ;
     STP = -> STRUCTURE ;
     CTP = -> IDENTIFIER ;
     STRUCTURE = record
                   NEXT : STP ; (* next entry link *)
                   MARKED : BOOLEAN ; (*for test phase only*)
                   SIZE : ADDRRANGE ;
                   PACKING : BOOLEAN ; (* packing status *)
                   case FORM : STRUCTFORM of
                     SCALAR :
                       ( case SCALKIND : DECLKIND of
                           DECLARED :
                             ( FCONST : CTP ) ;
                           STANDARD :
                             ( ) ) ;
                     SUBRANGE :
                       ( RANGETYPE : STP ;
                         MIN , MAX : VALU ) ;
                     POINTER :
                       ( ELTYPE : STP ) ;
                     POWER :
                       ( ELSET : STP ;
                         MATCHPACK : BOOLEAN ) ;
                     ARRAYS :
                       ( AELTYPE , INXTYPE : STP ) ;
                     RECORDS :
                       ( FSTFLD : CTP ;
                         RECVAR : STP ;
                         RECYC : STP ) ;
                     FILES :
                       ( FILTYPE : STP ) ;
                     TAGFLD :
                       ( TAGFIELDP : CTP ;
                         FSTVAR : STP ) ;
                     VARIANT :
                       ( NXTVAR , SUBVAR : STP ;
                         VARVAL : VALU )
                 end ;

     (*******)
     (*names*)
     (*******)
     (*******)

     IDCLASS = ( TYPES , KONST , VARS , FIELD , PROC , FUNC ) ;
     SETOFIDS = set of IDCLASS ;
     IDKIND = ( ACTUAL , FORMAL ) ;
     IDSTR = packed array [ 1 .. MAXIDS ] of CHAR ;
     RESTR = packed array [ 1 .. RESLEN ] of CHAR ;
     NMSTR = packed array [ 1 .. DIGMAX ] of CHAR ;
     CSSTR = packed array [ 1 .. STRGLGTH ] of CHAR ;
     IDENTIFIER = record
                    NAME : STRVSP ;
                    LLINK , RLINK : CTP ;
                    IDTYPE : STP ;
                    NEXT : CTP ;
                    KEEP : BOOLEAN ;
                    case KLASS : IDCLASS of
                      TYPES :
                        ( ) ;
                      KONST :
                        ( VALUES : VALU ) ;
                      VARS :
                        ( VKIND : IDKIND ;
                          VLEV : LEVRANGE ;
                          VADDR : ADDRRANGE ) ;
                      FIELD :
                        ( FLDADDR : ADDRRANGE ) ;
                      PROC , FUNC :
                        ( PFADDR : ADDRRANGE ;
                          PFLIST : CTP ;                          (* param list *)
                          case PFDECKIND : DECLKIND of
                            STANDARD :
                              ( KEY : 1 .. 18 ) ;
                            DECLARED :
                              ( PFLEV : LEVRANGE ;
                                PFNAME : INTEGER ;
                                case PFKIND : IDKIND of
                                  ACTUAL :
                                    ( FORWDECL , EXTERNL : BOOLEAN ) ;
                                  FORMAL :
                                    ( ) ) )
                  end ;
     DISPRANGE = 0 .. DISPLIMIT ;
     WHERE = ( BLCK , CREC , VREC , REC ) ;

     (*************)
     (*expressions*)
     (*************)
     (*************)

     ATTRKIND = ( CST , VARBL , EXPR ) ;
     VACCESS = ( DRCT , INDRCT , INXD ) ;
     ATTR = record
              TYPTR : STP ;
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
                      INXD :
                        ( ) ) ;
                EXPR :
                  ( )
            end ;

     (********)
     (*labels*)
     (********)
     (********)

     LBP = -> LABL ;
     LABL = record (* 'goto' label *)
              NEXTLAB : LBP ; (* next list link *)
              DEFINED : BOOLEAN ; (* label defining point was seen *)
              LABVAL , (* numeric value of label *)
              LABNAME : INTEGER ;

     (************************************)
     (* internal sequental name of label *)
     (************************************)

              VLEVEL : LEVRANGE ;    (* procedure level of definition *)
              SLEVEL : INTEGER ;     (* statement level of definition *)
              IPCREF : BOOLEAN ;

     (***************************************)
     (* was referenced by another proc/func *)
     (***************************************)

              MINLVL : INTEGER ;

     (****************************************)
     (* minimum goto reference statement lvl *)
     (****************************************)

              BACT : BOOLEAN ;       (* containing block is active *)
            end ;

     (**********************************)
     (* external file tracking entries *)
     (**********************************)

     EXTFILEP = -> FILEREC ;
     FILEREC = record
                 FILENAME : IDSTR ;
                 NEXTFILE : EXTFILEP
               end ;

     (***********************************)
     (* case statement tracking entries *)
     (***********************************)

     CIP = -> CASEINFO ;
     CASEINFO = record
                  NEXT : CIP ;
                  CSSTART : INTEGER ;
                  CSLAB : INTEGER
                end ;

     (***************************************************************************)
     (*-------------------------------------------------------------------------*)
     (***************************************************************************)



var

    (**********************************************)
    (* !!! remove this statement for self compile *)
    (*elide                                       *)
    (**********************************************)

    PRR : TEXT ;    (*noelide           *)
                    (* output code file *)

    (************************************************)
    (*returned by source program scanner            *)
    (*                                     insymbol:*)
    (*                                     **********)
    (************************************************)

    SY : SYMBOL ;                   (*last symbol*)
    OP : OPERATOR ;                 (*classification of last symbol*)
    VAL : VALU ;                    (*value of last constant*)
    LGTH : INTEGER ;                (*length of last string constant*)
    ID : IDSTR ;                    (*last identifier (possibly truncated)*)
    KK : 1 .. MAXIDS ;              (*nr of chars in last identifier*)
    CH : CHAR ;                     (*last character*)
    EOL : BOOLEAN ;                 (*end of line flag*)

    (***********)
    (*counters:*)
    (***********)
    (***********)

    CHCNT : INTEGER ;               (*character counter*)
    LC , IC : ADDRRANGE ;           (*data location and instruction counter*)
    LINECOUNT : INTEGER ;

    (***********)
    (*switches:*)
    (***********)
    (***********)

    DP ,                            (*declaration part*)
    LIST , PRCODE , PRTABLES : BOOLEAN ;

    (*******************************************************************************)
    (*output options for                                                           *)
    (*                                        -- source program listing            *)
    (*                                        -- printing symbolic code            *)
    (*                                        -- displaying ident and struct tables*)
    (*                                        --> procedure option                 *)
    (*******************************************************************************)

    DEBUG : BOOLEAN ;

    (***********)
    (*pointers:*)
    (***********)
    (***********)

    PARMPTR , INTPTR , REALPTR , CHARPTR , BOOLPTR , NILPTR , TEXTPTR :
                                                   STP ;

    (*************************************)
    (*pointers to entries of standard ids*)
    (*************************************)

    UTYPPTR , UCSTPTR , UVARPTR , UFLDPTR , UPRCPTR , UFCTPTR ,

    (****************************************)
    (*pointers to entries for undeclared ids*)
    (****************************************)

    FWPTR : CTP ;                   (*head of chain of forw decl type ids*)
    OUTPUTPTR , INPUTPTR : CTP ;    (* pointers to default files *)
    FEXTFILEP : EXTFILEP ;          (*head of chain of external files*)

    (************************************)
    (*bookkeeping of declaration levels:*)
    (************************************)
    (************************************)

    LEVEL : LEVRANGE ;              (*current static level*)
    DISX ,                          (*level of last id searched by searchid*)
    TOP : DISPRANGE ;               (*top of display*)
    DISPLAY :                       (*where:   means:*)
                                                   array [ DISPRANGE ]
                                                   of packed record

    (****************************)
    (*=blck:   id is variable id*)
    (****************************)

                                                   FNAME : CTP ;
                                                   FLABEL : LBP ;

    (****************************************)
    (*=crec:   id is field id in record with*)
    (****************************************)

                                                   FCONST : CSP ;
                                                   FSTRUCT : STP ;
                                                   case OCCUR : WHERE
                                                   of

    (*********************)
    (*   constant address*)
    (*********************)

                                                   CREC :
                                                   ( CLEV : LEVRANGE ;

    (****************************************)
    (*=vrec:   id is field id in record with*)
    (****************************************)

                                                   CDSPL : ADDRRANGE )
                                                   ;

    (*********************)
    (*   variable address*)
    (*********************)

                                                   VREC :
                                                   ( VDSPL : ADDRRANGE
                                                   ) ;
                                                   BLCK :
                                                   ( BNAME : CTP ) ;

    (************)
    (* block id *)
    (************)

                                                   REC :
                                                   ( )
                                                   end ;

    (******************************)
    (* --> procedure withstatement*)
    (******************************)


    (*****************)
    (*error messages:*)
    (*****************)
    (*****************)

    ERRINX : 0 .. 10 ;              (*nr of errors in current source line*)
    ERRLIST : array [ 1 .. 10 ] of packed record
                                            POS : INTEGER ;
                                            NMR : 1 .. 500
                                          end ;

    (*************************)
    (*expression compilation:*)
    (*************************)
    (*************************)

    GATTR : ATTR ;                  (*describes the expr currently compiled*)

    (***********************)
    (*structured constants:*)
    (***********************)
    (***********************)

    CONSTBEGSYS , SIMPTYPEBEGSYS , TYPEBEGSYS , BLOCKBEGSYS , SELECTSYS
    , FACBEGSYS , STATBEGSYS , TYPEDELS : SETOFSYS ;
    CHARTP : array [ CHAR ] of CHTP ;
    RW : array [ 1 .. MAXRES  (*nr. of res. words*) ] of RESTR ;
    FRW : array [ 1 .. 10 ] of 1 .. 36  (*nr. of res. words + 1*) ;
    RSY : array [ 1 .. MAXRES  (*nr. of res. words*) ] of SYMBOL ;
    SSY : array [ CHAR ] of SYMBOL ;
    ROP : array [ 1 .. MAXRES  (*nr. of res. words*) ] of OPERATOR ;
    SOP : array [ CHAR ] of OPERATOR ;
    NA : array [ 1 .. MAXSTD ] of RESTR ;
    MN : array [ 0 .. MAXINS ] of packed array [ 1 .. 4 ] of CHAR ;
    SNA : array [ 1 .. MAXSP ] of packed array [ 1 .. 4 ] of CHAR ;
    CDX : array [ 0 .. MAXINS ] of - 4 .. + 4 ;
    PDX : array [ 1 .. MAXSP ] of - 7 .. + 7 ;
    ORDINT : array [ CHAR ] of INTEGER ;
    INTLABEL , MXINT10 : INTEGER ;
    INPUTHDF : BOOLEAN ; (* 'input' appears in header files *)
    OUTPUTHDF : BOOLEAN ; (* 'output' appears in header files *)
    ERRTBL : array [ 1 .. 500 ] of BOOLEAN ;

    (***************************)
    (* error occrence tracking *)
    (***************************)

    TOTERR : INTEGER ; (* total errors in program *)

    (**************************************************************************)
    (* Recycling tracking counters, used to check for new/dispose mismatches. *)
    (**************************************************************************)

    STRCNT : INTEGER ; (* strings *)
    CSPCNT : INTEGER ; (* constants *)
    STPCNT : INTEGER ; (* structures *)
    CTPCNT : INTEGER ; (* identifiers *)
    LBPCNT : INTEGER ; (* label counts *)
    FILCNT : INTEGER ; (* file tracking counts *)
    CIPCNT : INTEGER ; (* case entry tracking counts *)
    F : BOOLEAN ;

    (******************************************************)
    (* flag for if error number list entries were printed *)
    (******************************************************)

    I : 1 .. 500 ;

    (***************************************************************************)
    (* index for error number tracking array                                   *)
    (*-------------------------------------------------------------------------*)
    (***************************************************************************)


    (**********************)
    (* recycling controls *)
    (**********************)


    (***************************************************************************)
    (*-------------------------------------------------------------------------*)
    (***************************************************************************)




procedure DISPOSE ( P : ANYPTR ) ;

   begin (* DISPOSE *)
     if P = NIL then
       WRITELN ( 'argument for dispose is nil' ) ;
     WRITELN ( 'dispose does nothing' ) ;
     WRITELN ( 'coded here just to avoid error' ) ;
   end (* DISPOSE *) ;





(*********************)
(* get string quanta *)
(*********************)




procedure GETSTR ( var P : STRVSP ) ;

   begin (* GETSTR *)
     NEW ( P ) ; (* get new entry *)
     STRCNT := STRCNT + 1 (* count *)
   end (* GETSTR *) ;





(******************************)
(* recycle string quanta list *)
(******************************)




procedure PUTSTRS ( P : STRVSP ) ;

   var P1 : STRVSP ;

   begin (* PUTSTRS *)
     while P <> NIL do
       begin
         P1 := P ;
         P := P -> . NEXT ;
         DISPOSE ( P1 ) ;
         STRCNT := STRCNT - 1
       end (* while *)
   end (* PUTSTRS *) ;





(*******************)
(* get label entry *)
(*******************)




procedure GETLAB ( var P : LBP ) ;

   begin (* GETLAB *)
     NEW ( P ) ; (* get new entry *)
     LBPCNT := LBPCNT + 1 (* add to count *)
   end (* GETLAB *) ;





(***********************)
(* recycle label entry *)
(***********************)




procedure PUTLAB ( P : LBP ) ;

   begin (* PUTLAB *)
     DISPOSE ( P ) ; (* release entry *)
     LBPCNT := LBPCNT - 1 (* remove from count *)
   end (* PUTLAB *) ;





(*******************************)
(* push constant entry to list *)
(*******************************)




procedure PSHCST ( P : CSP ) ;

   begin (* PSHCST *)
     P -> . NEXT := DISPLAY [ TOP ] . FCONST ;
     DISPLAY [ TOP ] . FCONST := P ;
     CSPCNT := CSPCNT + 1 (* count entries *)
   end (* PSHCST *) ;





(**************************)
(* recycle constant entry *)
(**************************)




procedure PUTCST ( P : CSP ) ;

   begin (* PUTCST *)
     if P -> . CCLASS = STRG then
       PUTSTRS ( P -> . SVAL )
     else
       if P -> . CCLASS = REEL then
         PUTSTRS ( P -> . RVAL ) ;
     DISPOSE ( P ) ; (* release entry *)
     CSPCNT := CSPCNT - 1 (* remove from count *)
   end (* PUTCST *) ;





(********************************)
(* push structure entry to list *)
(********************************)




procedure PSHSTC ( P : STP ) ;

   begin (* PSHSTC *)
     P -> . NEXT := DISPLAY [ TOP ] . FSTRUCT ;
     DISPLAY [ TOP ] . FSTRUCT := P ;
     STPCNT := STPCNT + 1 (* count entries *)
   end (* PSHSTC *) ;





(***************************)
(* recycle structure entry *)
(***************************)




procedure PUTSTC ( P : STP ) ;

   begin (* PUTSTC *)
     DISPOSE ( P ) ; (* release entry *)
     STPCNT := STPCNT - 1
   end (* PUTSTC *) ;





(********************************************)
(* initialize and register identifier entry *)
(********************************************)




procedure ININAM ( P : CTP ) ;

   begin (* ININAM *)
     CTPCNT := CTPCNT + 1 ; (* count entry *)
     P -> . KEEP := FALSE   (* clear keepme flag *)
   end (* ININAM *) ;





(****************************)
(* recycle identifier entry *)
(****************************)




procedure PUTNAM ( P : CTP ) ;

   var P1 : CTP ;

   begin (* PUTNAM *)
     if ( P -> . KLASS = PROC ) or ( P -> . KLASS = FUNC ) then
       while P -> . PFLIST <> NIL do
         begin

     (*******************************)
     (* scavenge the parameter list *)
     (*******************************)

           P1 := P -> . PFLIST ;
           P -> . PFLIST := P1 -> . NEXT ;
           PUTNAM ( P1 ) (* release *)
         end (* while *) ;
     PUTSTRS ( P -> . NAME ) ; (* release name string *)
     DISPOSE ( P ) ;           (* release entry *)
     CTPCNT := CTPCNT - 1      (* remove from count *)
   end (* PUTNAM *) ;





(***************************)
(* recycle identifier tree *)
(***************************)




procedure PUTNAMS ( P : CTP ) ;

   begin (* PUTNAMS *)
     if P <> NIL then
       begin
         PUTNAMS ( P -> . LLINK ) ; (* release left *)
         PUTNAMS ( P -> . RLINK ) ;

     (******************************************************************)
     (* release right                                                  *)
     (* "keep" means it is a parameter and stays with it's procedure or*)
     (*        function entry.                                         *)
     (******************************************************************)

         if not P -> . KEEP then
           PUTNAM ( P )            (* release the id entry *)
       end (* then *)
   end (* PUTNAMS *) ;





(***********************)
(* scrub display level *)
(***********************)




procedure PUTDSP ( L : DISPRANGE ) ;

   var LLP : LBP ;
       LVP : CSP ;
       LSP : STP ;

       (************************)
       (* release substructure *)
       (************************)



   procedure PUTSUB ( P : STP ) ;

      var P1 : STP ;

      begin (* PUTSUB *)
        if P -> . FORM = RECORDS then
          begin

        (************************)
        (* clear structure list *)
        (************************)

            while P -> . RECYC <> NIL do
              begin

        (**********************)
        (* remove top of list *)
        (**********************)

                P1 := P -> . RECYC ;
                P -> . RECYC := P1 -> . NEXT ;
                PUTSUB ( P1 ) (* release that element *)
              end (* while *) ;
            PUTNAMS ( P -> . FSTFLD ) (* clear id list *)
          end (* then *)
        else
          if P -> . FORM = TAGFLD then

        (********************************)
        (* recycle anonymous tag fields *)
        (********************************)

            if P -> . TAGFIELDP -> . NAME = NIL then
              PUTNAM ( P -> . TAGFIELDP ) ;
        PUTSTC ( P ) (* release head entry *)
      end (* PUTSUB *) ;


   begin (* PUTDSP *)
     PUTNAMS ( DISPLAY [ L ] . FNAME ) ;

     (******************************)
     (* dispose of identifier tree *)
     (* dispose of label list      *)
     (******************************)

     while DISPLAY [ L ] . FLABEL <> NIL do
       begin
         LLP := DISPLAY [ L ] . FLABEL ;
         DISPLAY [ L ] . FLABEL := LLP -> . NEXTLAB ;
         PUTLAB ( LLP )
       end (* while *) ;

     (****************************)
     (* dispose of constant list *)
     (****************************)

     while DISPLAY [ L ] . FCONST <> NIL do
       begin
         LVP := DISPLAY [ L ] . FCONST ;
         DISPLAY [ L ] . FCONST := LVP -> . NEXT ;
         PUTCST ( LVP )
       end (* while *) ;

     (*****************************)
     (* dispose of structure list *)
     (*****************************)

     while DISPLAY [ L ] . FSTRUCT <> NIL do
       begin

     (************************)
     (* remove top from list *)
     (************************)

         LSP := DISPLAY [ L ] . FSTRUCT ;
         DISPLAY [ L ] . FSTRUCT := LSP -> . NEXT ;
         PUTSUB ( LSP )
       end (* while *)
   end (* PUTDSP *) ;





(**********)
(* putdsp *)
(**********)


(****************************************)
(* scrub all display levels until given *)
(****************************************)




procedure PUTDSPS ( L : DISPRANGE ) ;

   var T : DISPRANGE ;

   begin (* PUTDSPS *)
     if L > TOP then
       begin
         WRITELN ( '*** Error: Compiler internal error' ) ;
         goto 99
       end (* then *) ;
     T := TOP ;
     while T > L do
       begin
         PUTDSP ( T ) ;
         T := T - 1
       end (* while *)
   end (* PUTDSPS *) ;





(***************************)
(* get external file entry *)
(***************************)




procedure GETFIL ( var P : EXTFILEP ) ;

   begin (* GETFIL *)
     NEW ( P ) ; (* get new entry *)
     FILCNT := FILCNT + 1 (* count entry *)
   end (* GETFIL *) ;





(*******************************)
(* recycle external file entry *)
(*******************************)




procedure PUTFIL ( P : EXTFILEP ) ;

   begin (* PUTFIL *)
     DISPOSE ( P ) ; (* release entry *)
     FILCNT := FILCNT - 1 (* count entry *)
   end (* PUTFIL *) ;





(***************************)
(* get case tracking entry *)
(***************************)




procedure GETCAS ( var P : CIP ) ;

   begin (* GETCAS *)
     NEW ( P ) ; (* get new entry *)
     CIPCNT := CIPCNT + 1 (* count entry *)
   end (* GETCAS *) ;





(*******************************)
(* recycle case tracking entry *)
(*******************************)




procedure PUTCAS ( P : CIP ) ;

   begin (* PUTCAS *)
     DISPOSE ( P ) ; (* release entry *)
     CIPCNT := CIPCNT - 1 (* count entry *)
   end (* PUTCAS *) ;





(***************************************************************************)
(*-------------------------------------------------------------------------*)
(***************************************************************************)


(*****************************************)
(* character and string quanta functions *)
(*****************************************)


(***************************************************************************)
(*-------------------------------------------------------------------------*)
(***************************************************************************)


(********************************)
(* find lower case of character *)
(********************************)




function LCASE ( C : CHAR ) : CHAR ;

   begin (* LCASE *)
     if C in [ 'A' .. 'Z' ] then
       C := CHR ( ORD ( C ) - ORD ( 'A' ) + ORD ( 'a' ) ) ;
     LCASE := C
   end (* LCASE *) ;





(************************************************)
(* find reserved word string equal to id string *)
(************************************************)




function STREQURI ( A : RESTR ; var B : IDSTR ) : BOOLEAN ;

   var M : BOOLEAN ;
       I : INTEGER ;

   begin (* STREQURI *)
     M := TRUE ;
     for I := 1 to RESLEN do
       if LCASE ( A [ I ] ) <> LCASE ( B [ I ] ) then
         M := FALSE ;
     for I := RESLEN + 1 to MAXIDS do
       if B [ I ] <> ' ' then
         M := FALSE ;
     STREQURI := M
   end (* STREQURI *) ;





(*********************************************)
(* write variable length id string to output *)
(*********************************************)




procedure WRITEV ( var F : TEXT ; S : STRVSP ; FL : INTEGER ) ;

   var I : INTEGER ;
       C : CHAR ;

   begin (* WRITEV *)
     I := 1 ;
     while FL > 0 do
       begin
         C := ' ' ;
         if S <> NIL then
           begin
             C := S -> . STR [ I ] ;
             I := I + 1
           end (* then *) ;
         WRITE ( F , C ) ;
         FL := FL - 1 ;
         if I > VARSQT then
           begin
             S := S -> . NEXT ;
             I := 1
           end (* then *)
       end (* while *)
   end (* WRITEV *) ;





(***************************************************)
(* find padded length of variable length id string *)
(***************************************************)




function LENPV ( S : STRVSP ) : INTEGER ;

   var I , L , LC : INTEGER ;

   begin (* LENPV *)
     L := 1 ;
     LC := 0 ;
     while S <> NIL do
       begin
         for I := 1 to VARSQT do
           begin
             if S -> . STR [ I ] <> ' ' then
               LC := L ;
             L := L + 1 ; (* count characters *)
           end (* for *) ;
         S := S -> . NEXT
       end (* while *) ;
     LENPV := LC
   end (* LENPV *) ;





(***************************************************************************)
(* assign identifier fixed to variable length string, including allocation *)
(***************************************************************************)




procedure STRASSVF ( var A : STRVSP ; var B : IDSTR ) ;

   var I , J , L : INTEGER ;
       P , LP : STRVSP ;

   begin (* STRASSVF *)
     L := MAXIDS ;
     P := NIL ;
     A := NIL ;
     J := 1 ;
     while ( L > 1 ) and ( B [ L ] = ' ' ) do
       L := L - 1 ;                            (* find length of fixed string *)
     if B [ L ] = ' ' then
       L := 0 ;
     for I := 1 to L do
       begin
         if J > VARSQT then
           P := NIL ;
         if P = NIL then
           begin
             GETSTR ( P ) ;
             P -> . NEXT := NIL ;
             J := 1 ;
             if A = NIL then
               A := P
             else
               LP -> . NEXT := P ;
             LP := P
           end (* then *) ;
         P -> . STR [ J ] := B [ I ] ;
         J := J + 1
       end (* for *) ;
     if P <> NIL then
       for J := J to VARSQT do
         P -> . STR [ J ] := ' '
   end (* STRASSVF *) ;





(******************************************************************************)
(* assign reserved word fixed to variable length string, including allocation *)
(******************************************************************************)




procedure STRASSVR ( var A : STRVSP ; B : RESTR ) ;

   var I , J , L : INTEGER ;
       P , LP : STRVSP ;

   begin (* STRASSVR *)
     L := RESLEN ;
     P := NIL ;
     A := NIL ;
     LP := NIL ;
     J := 1 ;
     while ( L > 1 ) and ( B [ L ] = ' ' ) do
       L := L - 1 ;                            (* find length of fixed string *)
     if B [ L ] = ' ' then
       L := 0 ;
     for I := 1 to L do
       begin
         if J > VARSQT then
           P := NIL ;
         if P = NIL then
           begin
             GETSTR ( P ) ;
             P -> . NEXT := NIL ;
             J := 1 ;
             if A = NIL then
               A := P
             else
               LP -> . NEXT := P ;
             LP := P
           end (* then *) ;
         P -> . STR [ J ] := B [ I ] ;
         J := J + 1
       end (* for *) ;
     if P <> NIL then
       for J := J to VARSQT do
         P -> . STR [ J ] := ' '
   end (* STRASSVR *) ;





(******************************************************************************)
(* assign number string fixed to variable length string, including allocation *)
(******************************************************************************)




procedure STRASSVD ( var A : STRVSP ; B : NMSTR ) ;

   var I , J , L : INTEGER ;
       P , LP : STRVSP ;

   begin (* STRASSVD *)
     L := DIGMAX ;
     P := NIL ;
     A := NIL ;
     LP := NIL ;
     J := 1 ;
     while ( L > 1 ) and ( B [ L ] = ' ' ) do
       L := L - 1 ;                            (* find length of fixed string *)
     if B [ L ] = ' ' then
       L := 0 ;
     for I := 1 to L do
       begin
         if J > VARSQT then
           P := NIL ;
         if P = NIL then
           begin
             GETSTR ( P ) ;
             P -> . NEXT := NIL ;
             J := 1 ;
             if A = NIL then
               A := P
             else
               LP -> . NEXT := P ;
             LP := P
           end (* then *) ;
         P -> . STR [ J ] := B [ I ] ;
         J := J + 1
       end (* for *) ;
     if P <> NIL then
       for J := J to VARSQT do
         P -> . STR [ J ] := ' '
   end (* STRASSVD *) ;





(********************************************************************************)
(* assign constant string fixed to variable length string, including allocation *)
(********************************************************************************)




procedure STRASSVC ( var A : STRVSP ; B : CSSTR ; L : INTEGER ) ;

   var I , J : INTEGER ;
       P , LP : STRVSP ;

   begin (* STRASSVC *)
     P := NIL ;
     A := NIL ;
     LP := NIL ;
     J := 1 ;
     for I := 1 to L do
       begin
         if J > VARSQT then
           P := NIL ;
         if P = NIL then
           begin
             GETSTR ( P ) ;
             P -> . NEXT := NIL ;
             J := 1 ;
             if A = NIL then
               A := P
             else
               LP -> . NEXT := P ;
             LP := P
           end (* then *) ;
         P -> . STR [ J ] := B [ I ] ;
         J := J + 1
       end (* for *) ;
     if P <> NIL then
       for J := J to VARSQT do
         P -> . STR [ J ] := ' '
   end (* STRASSVC *) ;





(*****************************************************)
(* assign variable length string to fixed identifier *)
(*****************************************************)




procedure STRASSFV ( var A : IDSTR ; B : STRVSP ) ;

   var I , J : INTEGER ;

   begin (* STRASSFV *)
     for I := 1 to MAXIDS do
       A [ I ] := ' ' ;
     I := 1 ;
     while B <> NIL do
       begin
         for J := 1 to VARSQT do
           begin
             A [ I ] := B -> . STR [ J ] ;
             I := I + 1
           end (* for *) ;
         B := B -> . NEXT
       end (* while *)
   end (* STRASSFV *) ;





(**************************************)
(* compare variable length id strings *)
(**************************************)




function STREQUVV ( A , B : STRVSP ) : BOOLEAN ;

   var M : BOOLEAN ;
       I : INTEGER ;

   begin (* STREQUVV *)
     M := TRUE ;
     while ( A <> NIL ) and ( B <> NIL ) do
       begin
         for I := 1 to VARSQT do
           if LCASE ( A -> . STR [ I ] ) <> LCASE ( B -> . STR [ I ] )
           then
             M := FALSE ;
         A := A -> . NEXT ;
         B := B -> . NEXT
       end (* while *) ;
     if A <> B then
       M := FALSE ;
     STREQUVV := M
   end (* STREQUVV *) ;





(*********************************************)
(* compare variable length id strings, a < b *)
(*********************************************)




function STRLTNVV ( A , B : STRVSP ) : BOOLEAN ;

   var I : INTEGER ;
       CA , CB : CHAR ;

   begin (* STRLTNVV *)
     CA := ' ' ;
     CB := ' ' ;
     while ( A <> NIL ) or ( B <> NIL ) do
       begin
         I := 1 ;
         while ( I <= VARSQT ) and ( ( A <> NIL ) or ( B <> NIL ) ) do
           begin
             if A <> NIL then
               CA := LCASE ( A -> . STR [ I ] )
             else
               CA := ' ' ;
             if B <> NIL then
               CB := LCASE ( B -> . STR [ I ] )
             else
               CB := ' ' ;
             if CA <> CB then
               begin
                 A := NIL ;
                 B := NIL
               end (* then *) ;
             I := I + 1
           end (* while *) ;
         if A <> NIL then
           A := A -> . NEXT ;
         if B <> NIL then
           B := B -> . NEXT
       end (* while *) ;
     STRLTNVV := CA < CB
   end (* STRLTNVV *) ;





(**********************************************)
(* compare variable length id string to fixed *)
(**********************************************)




function STREQUVF ( A : STRVSP ; var B : IDSTR ) : BOOLEAN ;

   var M : BOOLEAN ;
       I , J : INTEGER ;
       C : CHAR ;

   begin (* STREQUVF *)
     M := TRUE ;
     J := 1 ;
     for I := 1 to MAXIDS do
       begin
         C := ' ' ;
         if A <> NIL then
           begin
             C := A -> . STR [ J ] ;
             J := J + 1
           end (* then *) ;
         if LCASE ( C ) <> LCASE ( B [ I ] ) then
           M := FALSE ;
         if J > VARSQT then
           begin
             A := A -> . NEXT ;
             J := 1
           end (* then *)
       end (* for *) ;
     STREQUVF := M
   end (* STREQUVF *) ;





(*****************************************************)
(* compare variable length id string to fixed, a < b *)
(*****************************************************)




function STRLTNVF ( A : STRVSP ; var B : IDSTR ) : BOOLEAN ;

   var M : BOOLEAN ;
       I , J , F : INTEGER ;
       C : CHAR ;

   begin (* STRLTNVF *)
     M := TRUE ;
     I := 1 ;
     J := 1 ;
     while I < MAXIDS do
       begin
         C := ' ' ;
         if A <> NIL then
           begin
             C := A -> . STR [ J ] ;
             J := J + 1
           end (* then *) ;
         if LCASE ( C ) <> LCASE ( B [ I ] ) then
           begin
             F := I ;
             I := MAXIDS
           end (* then *)
         else
           I := I + 1 ;
         if J > VARSQT then
           begin
             A := A -> . NEXT ;
             J := 1
           end (* then *)
       end (* while *) ;
     STRLTNVF := LCASE ( C ) < LCASE ( B [ F ] )
   end (* STRLTNVF *) ;





(*********************************************)
(* get character from variable length string *)
(*********************************************)




function STRCHR ( A : STRVSP ; X : INTEGER ) : CHAR ;

   var C : CHAR ;
       I : INTEGER ;
       Q : INTEGER ;

   begin (* STRCHR *)
     C := ' ' ;
     I := 1 ;
     Q := 1 ;
     while I < X do
       begin
         if Q >= VARSQT then
           begin
             Q := 1 ;
             if A <> NIL then
               A := A -> . NEXT
           end (* then *)
         else
           Q := Q + 1 ;
         I := I + 1
       end (* while *) ;
     if A <> NIL then
       C := A -> . STR [ Q ] ;
     STRCHR := C
   end (* STRCHR *) ;





(*******************************************)
(* put character to variable length string *)
(*******************************************)




procedure STRCHRASS ( var A : STRVSP ; X : INTEGER ; C : CHAR ) ;

   var I : INTEGER ;
       Q : INTEGER ;
       P , L : STRVSP ;


   procedure GETSQT ;

      var Y : INTEGER ;

      begin (* GETSQT *)
        if P = NIL then
          begin
            GETSTR ( P ) ;
            for Y := 1 to VARSQT do
              P -> . STR [ Y ] := ' ' ;
            P -> . NEXT := NIL ;
            if A = NIL then
              A := P
            else
              L -> . NEXT := P
          end (* then *)
      end (* GETSQT *) ;


   begin (* STRCHRASS *)
     I := 1 ;
     Q := 1 ;
     P := A ;
     L := NIL ;
     GETSQT ;
     while I < X do
       begin
         if Q >= VARSQT then
           begin
             Q := 1 ;
             L := P ;
             P := P -> . NEXT ;
             GETSQT
           end (* then *)
         else
           Q := Q + 1 ;
         I := I + 1
       end (* while *) ;
     P -> . STR [ Q ] := C
   end (* STRCHRASS *) ;





(***************************************************************************)
(*-------------------------------------------------------------------------*)
(***************************************************************************)


(********************)
(* dump the display *)
(********************)




procedure PRTDSP ;

   var I : INTEGER ;


   procedure PRTLNK ( P : CTP ; F : INTEGER ) ;

      var I : INTEGER ;

      begin (* PRTLNK *)
        if P <> NIL then
          begin
            for I := 1 to F do
              WRITE ( ' ' ) ;
            WRITEV ( OUTPUT , P -> . NAME , 10 ) ;
            WRITELN ;
            if P -> . LLINK <> NIL then
              PRTLNK ( P -> . LLINK , F + 3 ) ;
            if P -> . RLINK <> NIL then
              PRTLNK ( P -> . RLINK , F + 3 )
          end (* then *)
      end (* PRTLNK *) ;


   begin (* PRTDSP *)
     WRITELN ;
     WRITELN ( 'Display:' ) ;
     WRITELN ;
     for I := 0 to DISPLIMIT do
       if DISPLAY [ I ] . FNAME <> NIL then
         begin
           WRITELN ( 'level ' , I : 1 ) ;
           WRITELN ;
           PRTLNK ( DISPLAY [ I ] . FNAME , 0 ) ;
           WRITELN
         end (* then *) ;
     WRITELN ;
   end (* PRTDSP *) ;



procedure ENDOFLINE ;

   var LASTPOS , FREEPOS , CURRPOS , CURRNMR , F , K : INTEGER ;

   begin (* ENDOFLINE *)
     if ERRINX > 0 then  (*output error messages*)
       begin
         WRITE ( OUTPUT , LINECOUNT : 6 , ' ****  ' : 9 ) ;
         LASTPOS := 0 ;
         FREEPOS := 1 ;
         for K := 1 to ERRINX do
           begin
             with ERRLIST [ K ] do
               begin
                 CURRPOS := POS ;
                 CURRNMR := NMR
               end (* with *) ;
             if CURRPOS = LASTPOS then
               WRITE ( OUTPUT , ',' )
             else
               begin
                 while FREEPOS < CURRPOS do
                   begin
                     WRITE ( OUTPUT , ' ' ) ;
                     FREEPOS := FREEPOS + 1
                   end (* while *) ;
                 WRITE ( OUTPUT , '^' ) ;
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
         ERRINX := 0
       end (* then *) ;
     LINECOUNT := LINECOUNT + 1 ;
     if LIST and ( not EOF ( INPUT ) ) then
       begin
         WRITE ( OUTPUT , LINECOUNT : 6 , '  ' : 2 ) ;
         if DP then
           WRITE ( OUTPUT , LC : 7 )
         else
           WRITE ( OUTPUT , IC : 7 ) ;
         WRITE ( OUTPUT , ' ' )
       end (* then *) ;

     (*******************************************)
     (* output line marker in intermediate file *)
     (*******************************************)

     if not EOF ( INPUT ) then
       begin
         WRITELN ( PRR , ':' , LINECOUNT : 1 ) ;
       end (* then *) ;
     CHCNT := 0
   end (* ENDOFLINE *) ;



procedure ERRMSG ( FERRNR : INTEGER ) ;

   begin (* ERRMSG *)
     case FERRNR of
       1 : WRITE ( 'Error in simple type' ) ;
       2 : WRITE ( 'Identifier expected' ) ;
       3 : WRITE ( '''program'' expected' ) ;
       4 : WRITE ( ''')'' expected' ) ;
       5 : WRITE ( ''':'' expected' ) ;
       6 : WRITE ( 'Illegal symbol' ) ;
       7 : WRITE ( 'Error in parameter list' ) ;
       8 : WRITE ( '''of'' expected' ) ;
       9 : WRITE ( '''('' expected' ) ;
       10 : WRITE ( 'Error in type' ) ;
       11 : WRITE ( '''['' expected' ) ;
       12 : WRITE ( ''']'' expected' ) ;
       13 : WRITE ( '''end'' expected' ) ;
       14 : WRITE ( ''':'' expected' ) ;
       15 : WRITE ( 'Integer expected' ) ;
       16 : WRITE ( '''='' expected' ) ;
       17 : WRITE ( '''begin'' expected' ) ;
       18 : WRITE ( 'Error in declaration part' ) ;
       19 : WRITE ( 'Error in field-list' ) ;
       20 : WRITE ( ''','' expected' ) ;
       21 : WRITE ( '''*'' expected' ) ;
       50 : WRITE ( 'Error in constant' ) ;
       51 : WRITE ( ''':='' expected' ) ;
       52 : WRITE ( '''then'' expected' ) ;
       53 : WRITE ( '''until'' expected' ) ;
       54 : WRITE ( '''do'' expected' ) ;
       55 : WRITE ( '''to''/''downto'' expected' ) ;
       56 : WRITE ( '''if'' expected' ) ;
       57 : WRITE ( '''file'' expected' ) ;
       58 : WRITE ( 'Error in factor' ) ;
       59 : WRITE ( 'Error in variable' ) ;
       101 : WRITE ( 'Identifier declared twice' ) ;
       102 : WRITE ( 'Low bound exceeds highbound' ) ;
       103 : WRITE ( 'Identifier is not of appropriate class' ) ;
       104 : WRITE ( 'Identifier not declared' ) ;
       105 : WRITE ( 'Sign not allowed' ) ;
       106 : WRITE ( 'Number expected' ) ;
       107 : WRITE ( 'Incompatible subrange types' ) ;
       109 : WRITE ( 'Type must not be real' ) ;
       110 : WRITE ( 'Tagfield type must be scalar or subrange' ) ;
       111 : WRITE ( 'Incompatible with tagfield type' ) ;
       112 : WRITE ( 'Index type must not be real' ) ;
       113 : WRITE ( 'Index type must be scalar or subrange' ) ;
       114 : WRITE ( 'Base type must not be real' ) ;
       115 : WRITE ( 'Base type must be scalar or subrange' ) ;
       116 : WRITE ( 'Error in type of standard procedure parameter' )
                     ;
       117 : WRITE ( 'Unsatisfied forward reference' ) ;
       118 : WRITE (
           'Forward reference type identifier in variable declaration'
                     ) ;
       119 : WRITE (
          'Forward declared; repetition of parameter list not allowed'
                     ) ;
       120 : WRITE (
              'Function result type must be scalar, subrange or point'
                     ) ;
       121 : WRITE ( 'File value parameter not allowed' ) ;
       122 : WRITE (
    'Forward declared function; repetition of result type not allowed'
                     ) ;
       123 : WRITE ( 'Missing result type in function declaration' ) ;
       124 : WRITE ( 'F-format for real only' ) ;
       125 : WRITE ( 'Error in type of standard function parameter' ) ;
       126 : WRITE (
                'Number of parameters does not agree with declaration'
                     ) ;
       127 : WRITE ( 'Illegal parameter substitution' ) ;
       128 : WRITE (
   'Result type of parameter function does not agree with declaration'
                     ) ;
       129 : WRITE ( 'Type conflict of operands' ) ;
       130 : WRITE ( 'Expression is not of set type' ) ;
       131 : WRITE ( 'Tests on equality allowed only' ) ;
       132 : WRITE ( 'Strict inclusion not allowed' ) ;
       133 : WRITE ( 'File comparison not allowed' ) ;
       134 : WRITE ( 'Illegal type of operand(s)' ) ;
       135 : WRITE ( 'Type of operand must be Boolean' ) ;
       136 : WRITE ( 'Set element type must be scalar nr subrange' ) ;
       137 : WRITE ( 'Set element types not compatible' ) ;
       138 : WRITE ( 'Type of variable is not array' ) ;
       139 : WRITE ( 'Index type is not compatible with declaration' )
                     ;
       140 : WRITE ( 'Type of variable is not record' ) ;
       141 : WRITE ( 'Type of variable must be file or pointer' ) ;
       142 : WRITE ( 'Illegal parameter substitution' ) ;
       143 : WRITE ( 'Illegal type of loop control variable' ) ;
       144 : WRITE ( 'Illegal type of expression' ) ;
       145 : WRITE ( 'Type conflict' ) ;
       146 : WRITE ( 'Assignment of files not allowed' ) ;
       147 : WRITE (
                   'Label type incompatible with selecting expression'
                     ) ;
       148 : WRITE ( 'Subrange bounds must be scalar' ) ;
       149 : WRITE ( 'Index type must not be integer' ) ;
       150 : WRITE ( 'Assignment to standard function is not allowed' )
                     ;
       151 : WRITE ( 'Assignment to formal function is not allowed' ) ;
       152 : WRITE ( 'No such field in this record' ) ;
       153 : WRITE ( 'Type error in read' ) ;
       154 : WRITE ( 'Actual parameter must be a variable' ) ;
       155 : WRITE (
               'Control variable must ~ot be declared on intermediate'
                     ) ;
       156 : WRITE ( 'Multidefined case label' ) ;
       157 : WRITE ( 'Too many cases in case statement' ) ;
       158 : WRITE ( 'Missing corresponding variant declaration' ) ;
       159 : WRITE ( 'Real or string tagfields not allowed' ) ;
       160 : WRITE ( 'Previous declaration was not forward' ) ;
       161 : WRITE ( 'Again forward declared' ) ;
       162 : WRITE ( 'Parameter size must be constant' ) ;
       163 : WRITE ( 'Missing variant in declaration' ) ;
       164 : WRITE ( 'Substitution of standard proc/func not allowed' )
                     ;
       165 : WRITE ( 'Multidefined label' ) ;
       166 : WRITE ( 'Multideclared label' ) ;
       167 : WRITE ( 'Undeclared label' ) ;
       168 : WRITE ( 'Undefined label' ) ;
       169 : WRITE ( 'Error in base set' ) ;
       170 : WRITE ( 'Value parameter expected' ) ;
       171 : WRITE ( 'Standard file was redeclared' ) ;
       172 : WRITE ( 'Undeclared external file' ) ;
       173 : WRITE ( 'Fortran procedure or function expected' ) ;
       174 : WRITE ( 'Pascal procedure or function expected' ) ;
       175 : WRITE ( 'Missing file "input" in program heading' ) ;
       176 : WRITE ( 'Missing file "output" in program heading' ) ;
       177 : WRITE (
                  'Assiqnment to function identifier not allowed here'
                     ) ;
       178 : WRITE ( 'Multidefined record variant' ) ;
       179 : WRITE (
          'X-opt of actual proc/funcdoes not match formal declaration'
                     ) ;
       180 : WRITE ( 'Control variable must not be formal' ) ;
       181 : WRITE ( 'Constant part of address out of ranqe' ) ;
       182 : WRITE ( 'identifier too long' ) ;
       183 : WRITE ( 'For index variable must be local to this block' )
                     ;
       184 : WRITE (
  'Interprocedure goto does not reference outter block of destination'
                     ) ;
       185 : WRITE ( 'Goto references deeper nested statement' ) ;
       186 : WRITE (
                  'Label referenced by goto at lesser statement level'
                     ) ;
       187 : WRITE (
                 'Goto references label in different nested statement'
                     ) ;
       188 : WRITE (
              'Label referenced by goto in different nested statement'
                     ) ;
       189 : WRITE (
       'Parameter lists of formal and actual parameters not congruous'
                     ) ;
       190 : WRITE ( 'File component may not contain other files' ) ;
       191 : WRITE (
               'Cannot assign from file or component containing files'
                     ) ;
       192 : WRITE ( 'Assignment to function that is not active' ) ;
       201 : WRITE ( 'Error in real constant: digit expected' ) ;
       202 : WRITE ( 'String constant must not exceed source line' ) ;
       203 : WRITE ( 'Integer constant exceeds range' ) ;
       204 : WRITE ( '8 or 9 in octal number' ) ;
       205 : WRITE ( 'Zero strinq not allowed' ) ;
       206 : WRITE ( 'Integer part of real constant exceeds ranqe' ) ;
       250 : WRITE ( 'Too many nestedscopes of identifiers' ) ;
       251 : WRITE ( 'Too many nested procedures and/or functions' ) ;
       252 : WRITE ( 'Too many forward references of procedure entries'
                     ) ;
       253 : WRITE ( 'Procedure too long' ) ;
       254 : WRITE ( 'Too many long constants in this procedure' ) ;
       255 : WRITE ( 'Too many errors on this source line' ) ;
       256 : WRITE ( 'Too many external references' ) ;
       257 : WRITE ( 'Too many externals' ) ;
       258 : WRITE ( 'Too many local files' ) ;
       259 : WRITE ( 'Expression too complicated' ) ;
       260 : WRITE ( 'Too many exit labels' ) ;
       300 : WRITE ( 'Division by zero' ) ;
       301 : WRITE ( 'No case provided for this value' ) ;
       302 : WRITE ( 'Index expression out of bounds' ) ;
       303 : WRITE ( 'Value to be assigned is out of bounds' ) ;
       304 : WRITE ( 'Element expression out of range' ) ;
       398 : WRITE ( 'Implementation restriction' ) ;
       399 : WRITE ( 'Feature not implemented' ) ;
       400 , 401 :
         WRITE ( 'Compiler internal error' ) ;
     end (* case *)
   end (* ERRMSG *) ;



procedure ERROR ( FERRNR : INTEGER ) ;

   begin (* ERROR *)

     (*****************************************************************************)
     (*      of line, and sometimes you need to know exactly where they occurred. *)
     (*****************************************************************************)


     (*                                 *)
     (*                                 *)
     (*    writeln('error: ', ferrnr:1);*)
     (*                                 *)
     (*                                 *)

     ERRTBL [ FERRNR ] := TRUE ; (* track this error *)
     if ERRINX >= 9 then
       begin
         ERRLIST [ 10 ] . NMR := 255 ;
         ERRINX := 10
       end (* then *)
     else
       begin
         ERRINX := ERRINX + 1 ;
         ERRLIST [ ERRINX ] . NMR := FERRNR
       end (* else *) ;
     ERRLIST [ ERRINX ] . POS := CHCNT ;
     TOTERR := TOTERR + 1
   end (* ERROR *) ;



procedure INSYMBOL ;

(******************************************************************)
(*read next basic symbol of source program and return its         *)
(*    description in the global variables sy, op, id, val and lgth*)
(******************************************************************)


   label 1 ;

   var I , K : INTEGER ;
       DIGIT : NMSTR ; (* temp holding for digit string *)
       RVALB : NMSTR ; (* temp holding for real string *)
       STRING : CSSTR ;
       LVP : CSP ;
       TEST , FERR : BOOLEAN ;
       ISCMTE : BOOLEAN ;


   procedure NEXTCH ;

      begin (* NEXTCH *)
        if EOL then
          begin
            if LIST then
              WRITELN ( OUTPUT ) ;
            ENDOFLINE
          end (* then *) ;
        if not EOF ( INPUT ) then
          begin
            EOL := EOLN ( INPUT ) ;
            READ ( INPUT , CH ) ;
            if LIST then
              WRITE ( OUTPUT , CH ) ;
            CHCNT := CHCNT + 1
          end (* then *)
        else
          begin
            WRITELN ( OUTPUT , '   *** eof ' , 'encountered' ) ;
            TEST := FALSE
          end (* else *)
      end (* NEXTCH *) ;


   procedure OPTIONS ;

      begin (* OPTIONS *)
        repeat
          NEXTCH ;
          if CH <> '*' then
            begin
              if CH = 't' then
                begin
                  NEXTCH ;
                  PRTABLES := CH = '+'
                end (* then *)
              else
                if CH = 'l' then
                  begin
                    NEXTCH ;
                    LIST := CH = '+' ;
                    if not LIST then
                      WRITELN ( OUTPUT )
                  end (* then *)
                else
                  if CH = 'd' then
                    begin
                      NEXTCH ;
                      DEBUG := CH = '+'
                    end (* then *)
                  else
                    if CH = 'c' then
                      begin
                        NEXTCH ;
                        PRCODE := CH = '+'
                      end (* then *) ;
              NEXTCH
            end (* then *)
        until CH <> ','
      end (* OPTIONS *) ;


   begin (* INSYMBOL *)
     1 :

     (*****************************************************************************)
     (* Skip both spaces and controls. This allows arbitrary formatting characters*)
     (*      in the source.                                                       *)
     (*****************************************************************************)

     repeat
       while ( CH <= ' ' ) and not EOL do
         NEXTCH ;
       TEST := EOL ;
       if TEST then
         NEXTCH
     until not TEST ;
     if CHARTP [ CH ] = ILLEGAL then
       begin
         SY := OTHERSY ;
         OP := NOOP ;
         ERROR ( 399 ) ;
         NEXTCH
       end (* then *)
     else
       case CHARTP [ CH ] of
         LETTER :
           begin
             K := 0 ;
             FERR := TRUE ;
             repeat
               if K < MAXIDS then
                 begin
                   K := K + 1 ;
                   ID [ K ] := CH
                 end (* then *)
               else
                 if FERR then
                   begin
                     ERROR ( 182 ) ;
                     FERR := FALSE
                   end (* then *) ;
               NEXTCH
             until CHARTP [ CH ] in [ SPECIAL , ILLEGAL , CHSTRQUO ,
             CHCOLON , CHPERIOD , CHLT , CHGT , CHLPAREN , CHSPACE ,
             CHLCMT ] ;
             if K >= KK then
               KK := K
             else
               repeat
                 ID [ KK ] := ' ' ;
                 KK := KK - 1
               until KK = K ;
             SY := IDENT ;
             OP := NOOP ;
             if K <= RESLEN then
               for I := FRW [ K ] to FRW [ K + 1 ] - 1 do
                 if STREQURI ( RW [ I ] , ID ) then
                   begin
                     SY := RSY [ I ] ;
                     OP := ROP [ I ]
                   end (* then *) ;
           end (* tag/ca *) ;
         NUMBER :
           begin
             OP := NOOP ;
             I := 0 ;
             repeat
               I := I + 1 ;
               if I <= DIGMAX then
                 DIGIT [ I ] := CH ;
               NEXTCH
             until CHARTP [ CH ] <> NUMBER ;
             if ( ( CH = '.' ) and ( INPUT -> <> '.' ) and ( INPUT ->
             <> ')' ) ) or ( LCASE ( CH ) = 'e' ) then
               begin
                 K := I ;
                 if CH = '.' then
                   begin
                     K := K + 1 ;
                     if K <= DIGMAX then
                       DIGIT [ K ] := CH ;
                     NEXTCH ;

     (***********************************************)
     (*if ch = '.' then begin ch := ':'; goto 3 end;*)
     (***********************************************)

                     if CHARTP [ CH ] <> NUMBER then
                       ERROR ( 201 )
                     else
                       repeat
                         K := K + 1 ;
                         if K <= DIGMAX then
                           DIGIT [ K ] := CH ;
                         NEXTCH
                       until CHARTP [ CH ] <> NUMBER
                   end (* then *) ;
                 if LCASE ( CH ) = 'e' then
                   begin
                     K := K + 1 ;
                     if K <= DIGMAX then
                       DIGIT [ K ] := CH ;
                     NEXTCH ;
                     if ( CH = '+' ) or ( CH = '-' ) then
                       begin
                         K := K + 1 ;
                         if K <= DIGMAX then
                           DIGIT [ K ] := CH ;
                         NEXTCH
                       end (* then *) ;
                     if CHARTP [ CH ] <> NUMBER then
                       ERROR ( 201 )
                     else
                       repeat
                         K := K + 1 ;
                         if K <= DIGMAX then
                           DIGIT [ K ] := CH ;
                         NEXTCH
                       until CHARTP [ CH ] <> NUMBER
                   end (* then *) ;
                 NEW ( LVP , REEL ) ;
                 PSHCST ( LVP ) ;
                 SY := REALCONST ;
                 LVP -> . CCLASS := REEL ;
                 with LVP -> do
                   begin
                     for I := 1 to DIGMAX do
                       RVALB [ I ] := ' ' ;
                     if K <= DIGMAX then
                       for I := 2 to K + 1 do
                         RVALB [ I ] := DIGIT [ I - 1 ]
                     else
                       begin
                         ERROR ( 203 ) ;
                         RVALB [ 2 ] := '0' ;
                         RVALB [ 3 ] := '.' ;
                         RVALB [ 4 ] := '0'
                       end (* else *) ;

     (******************************************)
     (* place buffered real string in constant *)
     (******************************************)

                     STRASSVD ( RVAL , RVALB )
                   end (* with *) ;
                 VAL . VALP := LVP
               end (* then *)
             else
               begin
                 if I > DIGMAX then
                   begin
                     ERROR ( 203 ) ;
                     VAL . IVAL := 0
                   end (* then *)
                 else
                   with VAL do
                     begin
                       IVAL := 0 ;
                       for K := 1 to I do
                         begin
                           if IVAL <= MXINT10 then
                             IVAL := IVAL * 10 + ORDINT [ DIGIT [ K ] ]
                           else
                             begin
                               ERROR ( 203 ) ;
                               IVAL := 0
                             end (* else *)
                         end (* for *) ;
                       SY := INTCONST
                     end (* with *)
               end (* else *)
           end (* tag/ca *) ;
         CHSTRQUO :
           begin
             LGTH := 0 ;
             SY := STRINGCONST ;
             OP := NOOP ;
             for I := 1 to STRGLGTH do
               STRING [ I ] := ' ' ;
             repeat
               repeat
                 NEXTCH ;
                 LGTH := LGTH + 1 ;
                 if LGTH <= STRGLGTH then
                   STRING [ LGTH ] := CH
               until ( EOL ) or ( CH = '''' ) ;
               if EOL then
                 ERROR ( 202 )
               else
                 NEXTCH
             until CH <> '''' ;
             STRING [ LGTH ] := ' ' ; (* get rid of trailing quote *)
             LGTH := LGTH - 1 ;       (*now lgth = nr of chars in string*)
             if LGTH = 0 then
               ERROR ( 205 )
             else
               if LGTH = 1 then
                 VAL . IVAL := ORD ( STRING [ 1 ] )
               else
                 begin
                   NEW ( LVP , STRG ) ;
                   PSHCST ( LVP ) ;
                   LVP -> . CCLASS := STRG ;
                   if LGTH > STRGLGTH then
                     begin
                       ERROR ( 399 ) ;
                       LGTH := STRGLGTH
                     end (* then *) ;
                   with LVP -> do
                     begin
                       SLGTH := LGTH ;
                       STRASSVC ( SVAL , STRING , STRGLGTH )
                     end (* with *) ;
                   VAL . VALP := LVP
                 end (* else *)
           end (* tag/ca *) ;
         CHCOLON :
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
         CHPERIOD :
           begin
             OP := NOOP ;
             NEXTCH ;
             if CH = '.' then
               begin
                 SY := RANGE ;
                 NEXTCH
               end (* then *)
             else
               if CH = ')' then
                 begin
                   SY := RBRACK ;
                   NEXTCH
                 end (* then *)
               else
                 SY := PERIOD
           end (* tag/ca *) ;
         CHLT : begin
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
         CHGT : begin
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
         CHLPAREN :
           begin
             NEXTCH ;
             if CH = '*' then
               begin
                 NEXTCH ;
                 if CH = '$' then
                   OPTIONS ;
                 repeat
                   while ( CH <> '}' ) and ( CH <> '*' ) and not EOF (
                   INPUT ) do
                     NEXTCH ;
                   ISCMTE := CH = '}' ;
                   NEXTCH
                 until ISCMTE or ( CH = ')' ) or EOF ( INPUT ) ;
                 if not ISCMTE then
                   NEXTCH ;
                 goto 1
               end (* then *)
             else
               if CH = '.' then
                 begin
                   SY := LBRACK ;
                   NEXTCH
                 end (* then *)
               else
                 SY := LPARENT ;
             OP := NOOP
           end (* tag/ca *) ;
         CHLCMT :
           begin
             NEXTCH ;
             if CH = '$' then
               OPTIONS ;
             repeat
               while ( CH <> '}' ) and ( CH <> '*' ) and not EOF (
               INPUT ) do
                 NEXTCH ;
               ISCMTE := CH = '}' ;
               NEXTCH
             until ISCMTE or ( CH = ')' ) or EOF ( INPUT ) ;
             if not ISCMTE then
               NEXTCH ;
             goto 1
           end (* tag/ca *) ;
         SPECIAL :
           begin
             SY := SSY [ CH ] ;
             OP := SOP [ CH ] ;
             NEXTCH
           end (* tag/ca *) ;
         CHSPACE :
           SY := OTHERSY
       end (* case *) ; (*case*)
     if DODMPLEX then
       begin              (*  lexical dump *)
         WRITELN ;
         WRITE ( 'symbol: ' ) ;
         case SY of
           IDENT : WRITE ( 'ident: ' , ID : 10 ) ;
           INTCONST :
             WRITE ( 'int const: ' , VAL . IVAL : 1 ) ;
           REALCONST :
             begin
               WRITE ( 'real const: ' ) ;
               WRITEV ( OUTPUT , VAL . VALP -> . RVAL , 9 )
             end (* tag/ca *) ;
           STRINGCONST :
             begin
               WRITE ( 'string const: ''' ) ;
               WRITEV ( OUTPUT , VAL . VALP -> . SVAL , VAL . VALP -> .
                        SLGTH )
             end (* tag/ca *) ;
           NOTSY : WRITE ( 'not' ) ;
           MULOP : WRITE ( '*' ) ;
           ADDOP : WRITE ( '+' ) ;
           RELOP : WRITE ( '<' ) ;
           LPARENT :
             WRITE ( '(' ) ;
           RPARENT :
             WRITE ( ')' ) ;
           LBRACK :
             WRITE ( '[' ) ;
           RBRACK :
             WRITE ( ']' ) ;
           COMMA : WRITE ( ',' ) ;
           SEMICOLON :
             WRITE ( ';' ) ;
           PERIOD :
             WRITE ( '.' ) ;
           ARROW : WRITE ( '^' ) ;
           COLON : WRITE ( ':' ) ;
           BECOMES :
             WRITE ( ':=' ) ;
           RANGE : WRITE ( '..' ) ;
           LABELSY :
             WRITE ( 'label' ) ;
           CONSTSY :
             WRITE ( 'const' ) ;
           TYPESY :
             WRITE ( 'type' ) ;
           VARSY : WRITE ( 'var' ) ;
           FUNCSY :
             WRITE ( 'function' ) ;
           PROGSY :
             WRITE ( 'program' ) ;
           PROCSY :
             WRITE ( 'procedure' ) ;
           SETSY : WRITE ( 'set' ) ;
           PACKEDSY :
             WRITE ( 'packed' ) ;
           ARRAYSY :
             WRITE ( 'array' ) ;
           RECORDSY :
             WRITE ( 'record' ) ;
           FILESY :
             WRITE ( 'file' ) ;
           BEGINSY :
             WRITE ( 'begin' ) ;
           IFSY : WRITE ( 'if' ) ;
           CASESY :
             WRITE ( 'case' ) ;
           REPEATSY :
             WRITE ( 'repeat' ) ;
           WHILESY :
             WRITE ( 'while' ) ;
           FORSY : WRITE ( 'for' ) ;
           WITHSY :
             WRITE ( 'with' ) ;
           GOTOSY :
             WRITE ( 'goto' ) ;
           ENDSY : WRITE ( 'end' ) ;
           ELSESY :
             WRITE ( 'else' ) ;
           UNTILSY :
             WRITE ( 'until' ) ;
           OFSY : WRITE ( 'of' ) ;
           DOSY : WRITE ( 'do' ) ;
           TOSY : WRITE ( 'to' ) ;
           DOWNTOSY :
             WRITE ( 'downto' ) ;
           THENSY :
             WRITE ( 'then' ) ;
           OTHERSY :
             WRITE ( '<other>' ) ;
         end (* case *) ;
         WRITELN
       end (* then *)
   end (* INSYMBOL *) ;



procedure ENTERID ( FCP : CTP ) ;

(******************************************************)
(*enter id pointed at by fcp into the name-table,     *)
(*     which on each declaration level is organised as*)
(*     an unbalanced binary tree                      *)
(******************************************************)


   var LCP , LCP1 : CTP ;
       LLEFT : BOOLEAN ;

   begin (* ENTERID *)
     LCP := DISPLAY [ TOP ] . FNAME ;
     if LCP = NIL then
       DISPLAY [ TOP ] . FNAME := FCP
     else
       begin
         repeat
           LCP1 := LCP ;
           if STREQUVV ( LCP -> . NAME , FCP -> . NAME ) then

     (**********************************)
     (*name conflict, follow right link*)
     (**********************************)

             begin
               ERROR ( 101 ) ;
               LCP := LCP -> . RLINK ;
               LLEFT := FALSE
             end (* then *)
           else
             if STRLTNVV ( LCP -> . NAME , FCP -> . NAME ) then
               begin
                 LCP := LCP -> . RLINK ;
                 LLEFT := FALSE
               end (* then *)
             else
               begin
                 LCP := LCP -> . LLINK ;
                 LLEFT := TRUE
               end (* else *)
         until LCP = NIL ;
         if LLEFT then
           LCP1 -> . LLINK := FCP
         else
           LCP1 -> . RLINK := FCP
       end (* else *) ;
     FCP -> . LLINK := NIL ;
     FCP -> . RLINK := NIL
   end (* ENTERID *) ;



procedure SEARCHSECTION ( FCP : CTP ; var FCP1 : CTP ) ;

(***********************************************************)
(*to find record fields and forward declared procedure id's*)
(*     --> procedure proceduredeclaration                  *)
(*     --> procedure selector                              *)
(***********************************************************)


   label 1 ;

   begin (* SEARCHSECTION *)
     while FCP <> NIL do
       if STREQUVF ( FCP -> . NAME , ID ) then
         goto 1
       else
         if STRLTNVF ( FCP -> . NAME , ID ) then
           FCP := FCP -> . RLINK
         else
           FCP := FCP -> . LLINK ;
     1 :
     FCP1 := FCP
   end (* SEARCHSECTION *) ;



procedure SEARCHIDNENM ( FIDCLS : SETOFIDS ; var FCP : CTP ; var MM :
                       BOOLEAN ) ;

   label 1 ;

   var LCP : CTP ;
       DISXL : DISPRANGE ;

   begin (* SEARCHIDNENM *)
     MM := FALSE ;
     for DISXL := TOP DOWNTO 0 do
       begin
         LCP := DISPLAY [ DISXL ] . FNAME ;
         while LCP <> NIL do
           begin
             if STREQUVF ( LCP -> . NAME , ID ) then
               if LCP -> . KLASS in FIDCLS then
                 begin
                   DISX := DISXL ;
                   goto 1
                 end (* then *)
               else
                 begin
                   MM := TRUE ;
                   LCP := LCP -> . RLINK
                 end (* else *)
             else
               if STRLTNVF ( LCP -> . NAME , ID ) then
                 LCP := LCP -> . RLINK
               else
                 LCP := LCP -> . LLINK
           end (* while *)
       end (* for *) ;
     DISX := 0 ;
     LCP := NIL ; (* make sure this is not found *)
     1 :
     FCP := LCP
   end (* SEARCHIDNENM *) ;



procedure SEARCHIDNE ( FIDCLS : SETOFIDS ; var FCP : CTP ) ;

   var MM : BOOLEAN ;

   begin (* SEARCHIDNE *)
     SEARCHIDNENM ( FIDCLS , FCP , MM ) ;
     if MM then
       ERROR ( 103 )
   end (* SEARCHIDNE *) ;



procedure SEARCHID ( FIDCLS : SETOFIDS ; var FCP : CTP ) ;

   label 1 ;

   var LCP : CTP ;

   begin (* SEARCHID *)
     SEARCHIDNE ( FIDCLS , LCP ) ; (* perform no error search *)
     if LCP <> NIL then
       goto 1 ;               (* found                       *)
                              (*search not successful        *)
                              (*     --> procedure simpletype*)
     ERROR ( 104 ) ;

     (**************************************************)
     (*to avoid returning nil, reference an entry      *)
     (*       for an undeclared id of appropriate class*)
     (*       --> procedure enterundecl                *)
     (**************************************************)

     if TYPES in FIDCLS then
       LCP := UTYPPTR
     else
       if VARS in FIDCLS then
         LCP := UVARPTR
       else
         if FIELD in FIDCLS then
           LCP := UFLDPTR
         else
           if KONST in FIDCLS then
             LCP := UCSTPTR
           else
             if PROC in FIDCLS then
               LCP := UPRCPTR
             else
               LCP := UFCTPTR ;
     1 :
     FCP := LCP
   end (* SEARCHID *) ;



procedure GETBOUNDS ( FSP : STP ; var FMIN , FMAX : INTEGER ) ;

(************************************************)
(*get internal bounds of subrange or scalar type*)
(*assume fsp<>intptr and fsp<>realptr           *)
(************************************************)


   begin (* GETBOUNDS *)
     FMIN := 0 ;
     FMAX := 0 ;
     if FSP <> NIL then
       with FSP -> do
         if FORM = SUBRANGE then
           begin
             FMIN := MIN . IVAL ;
             FMAX := MAX . IVAL
           end (* then *)
         else
           if FSP = CHARPTR then
             begin
               FMIN := ORDMINCHAR ;
               FMAX := ORDMAXCHAR
             end (* then *)
           else
             if FCONST <> NIL then
               FMAX := FCONST -> . VALUES . IVAL
   end (* GETBOUNDS *) ;





(******************************************)
(* alignment for general memory placement *)
(******************************************)




function ALIGNQUOT ( FSP : STP ) : INTEGER ;

   begin (* ALIGNQUOT *)
     ALIGNQUOT := 1 ;
     if FSP <> NIL then
       with FSP -> do
         case FORM of
           SCALAR :
             if FSP = INTPTR then
               ALIGNQUOT := INTAL
             else
               if FSP = BOOLPTR then
                 ALIGNQUOT := BOOLAL
               else
                 if SCALKIND = DECLARED then
                   ALIGNQUOT := INTAL
                 else
                   if FSP = CHARPTR then
                     ALIGNQUOT := CHARAL
                   else
                     if FSP = REALPTR then
                       ALIGNQUOT := REALAL
                     else  (*parmptr*)
                       ALIGNQUOT := PARMAL ;
           SUBRANGE :
             ALIGNQUOT := ALIGNQUOT ( RANGETYPE ) ;
           POINTER :
             ALIGNQUOT := ADRAL ;
           POWER : ALIGNQUOT := SETAL ;
           FILES : ALIGNQUOT := FILEAL ;
           ARRAYS :
             ALIGNQUOT := ALIGNQUOT ( AELTYPE ) ;
           RECORDS :
             ALIGNQUOT := RECAL ;
           VARIANT , TAGFLD :
             ERROR ( 501 )
         end (* case *)
   end (* ALIGNQUOT *) ;



procedure ALIGN ( FSP : STP ; var FLC : ADDRRANGE ) ;

   var K , L : INTEGER ;

   begin (* ALIGN *)
     K := ALIGNQUOT ( FSP ) ;
     L := FLC - 1 ;
     FLC := L + K - ( K + L ) MOD K
   end (* ALIGN *) ;



procedure PRINTTABLES ( FB : BOOLEAN ) ;

(*****************************************************************************)
(*print data structure and name table                                        *)
(* Added these functions to convert pointers to integers.                    *)
(*      Works on any machine where pointers and integers are the same format.*)
(*      The original code was for a processor where "ord" would do this, a   *)
(*      very nonstandard feature [sam]                                       *)
(*****************************************************************************)


   const INTSIZE = 11 ; (* size of printed integer *)

   var I , LIM : DISPRANGE ;


   function STPTOINT ( P : STP ) : INTEGER ;

      var R : record
                case BOOLEAN of
                  FALSE :
                    ( P : STP ) ;
                  TRUE :
                    ( I : INTEGER )
              end ;

      begin (* STPTOINT *)
        R . P := P ;
        STPTOINT := R . I
      end (* STPTOINT *) ;


   function CTPTOINT ( P : CTP ) : INTEGER ;

      var R : record
                case BOOLEAN of
                  FALSE :
                    ( P : CTP ) ;
                  TRUE :
                    ( I : INTEGER )
              end ;

      begin (* CTPTOINT *)
        R . P := P ;
        CTPTOINT := R . I
      end (* CTPTOINT *) ;


   procedure MARKER ;

   (********************************************************)
   (*mark data structure entries to avoid multiple printout*)
   (********************************************************)


      var I : INTEGER ;


      procedure MARKCTP ( FP : CTP ) ;

         FORWARD ;


      procedure MARKSTP ( FP : STP ) ;

      (**************************************)
      (*mark data structures, prevent cycles*)
      (**************************************)


         begin (* MARKSTP *)
           if FP <> NIL then
             with FP -> do
               begin
                 MARKED := TRUE ;
                 case FORM of
                   SCALAR :
                     ;
                   SUBRANGE :
                     MARKSTP ( RANGETYPE ) ;
                   POINTER :
                     

           (***************************************************)
           (*don't mark eltype: cycle possible; will be marked*)
           (*                        anyway, if fp = true     *)
           (***************************************************)

                     ;
                   POWER : MARKSTP ( ELSET ) ;
                   ARRAYS :
                     begin
                       MARKSTP ( AELTYPE ) ;
                       MARKSTP ( INXTYPE )
                     end (* tag/ca *) ;
                   RECORDS :
                     begin
                       MARKCTP ( FSTFLD ) ;
                       MARKSTP ( RECVAR )
                     end (* tag/ca *) ;
                   FILES : MARKSTP ( FILTYPE ) ;
                   TAGFLD :
                     MARKSTP ( FSTVAR ) ;
                   VARIANT :
                     begin
                       MARKSTP ( NXTVAR ) ;
                       MARKSTP ( SUBVAR )
                     end (* tag/ca *)
                 end (* case *)
               end (* with *)
         end (* MARKSTP *) ;


      procedure MARKCTP ;

         begin (* MARKCTP *)
           if FP <> NIL then
             with FP -> do
               begin
                 MARKCTP ( LLINK ) ;
                 MARKCTP ( RLINK ) ;
                 MARKSTP ( IDTYPE )
               end (* with *)
         end (* MARKCTP *) ;


      begin (* MARKER *)
        for I := TOP DOWNTO LIM do
          MARKCTP ( DISPLAY [ I ] . FNAME )
      end (* MARKER *) ;


   procedure FOLLOWCTP ( FP : CTP ) ;

      FORWARD ;


   procedure FOLLOWSTP ( FP : STP ) ;

      begin (* FOLLOWSTP *)
        if FP <> NIL then
          with FP -> do
            if MARKED then
              begin
                MARKED := FALSE ;
                WRITE ( OUTPUT , ' ' : 4 , STPTOINT  (*ord*) ( FP ) :
                        INTSIZE  (*6*) , SIZE : 10 ) ;
                case FORM of
                  SCALAR :
                    begin
                      WRITE ( OUTPUT , 'scalar' : 10 ) ;
                      if SCALKIND = STANDARD then
                        WRITE ( OUTPUT , 'standard' : 10 )
                      else
                        WRITE ( OUTPUT , 'declared' : 10 , ' ' : 4 ,
                                CTPTOINT  (*ord*) ( FCONST ) : INTSIZE

        (***)
        (*6*)
        (***)

                                ) ;
                      WRITELN ( OUTPUT )
                    end (* tag/ca *) ;
                  SUBRANGE :
                    begin
                      WRITE ( OUTPUT , 'subrange' : 10 , ' ' : 4 ,
                              STPTOINT  (*ord*) ( RANGETYPE ) : 6 ) ;
                      if RANGETYPE <> REALPTR then
                        WRITE ( OUTPUT , MIN . IVAL , MAX . IVAL )
                      else
                        if ( MIN . VALP <> NIL ) and ( MAX . VALP <>
                        NIL ) then
                          begin
                            WRITE ( ' ' ) ;
                            WRITEV ( OUTPUT , MIN . VALP -> . RVAL , 9
                                     ) ;
                            WRITE ( ' ' ) ;
                            WRITEV ( OUTPUT , MAX . VALP -> . RVAL , 9
                                     )
                          end (* then *) ;
                      WRITELN ( OUTPUT ) ;
                      FOLLOWSTP ( RANGETYPE ) ;
                    end (* tag/ca *) ;
                  POINTER :
                    WRITELN ( OUTPUT , 'pointer' : 10 , ' ' : 4 ,
                              STPTOINT  (*ord*) ( ELTYPE ) : INTSIZE

        (***)
        (*6*)
        (***)

                              ) ;
                  POWER : begin
                            WRITELN ( OUTPUT , 'set' : 10 , ' ' : 4 ,
                                      STPTOINT  (*ord*) ( ELSET ) :
                                      INTSIZE  (*6*) ) ;
                            FOLLOWSTP ( ELSET )
                          end (* tag/ca *) ;
                  ARRAYS :
                    begin
                      WRITELN ( OUTPUT , 'array' : 10 , ' ' : 4 ,
                                STPTOINT  (*ord*) ( AELTYPE ) : INTSIZE

        (***)
        (*6*)
        (***)

                                , ' ' : 4 , STPTOINT  (*ord*) ( INXTYPE
                                ) : 6 ) ;
                      FOLLOWSTP ( AELTYPE ) ;
                      FOLLOWSTP ( INXTYPE )
                    end (* tag/ca *) ;
                  RECORDS :
                    begin
                      WRITELN ( OUTPUT , 'record' : 10 , ' ' : 4 ,
                                CTPTOINT  (*ord*) ( FSTFLD ) : INTSIZE

        (***)
        (*6*)
        (***)

                                , ' ' : 4 , STPTOINT  (*ord*) ( RECVAR
                                ) : INTSIZE  (*6*) ) ;
                      FOLLOWCTP ( FSTFLD ) ;
                      FOLLOWSTP ( RECVAR )
                    end (* tag/ca *) ;
                  FILES : begin
                            WRITE ( OUTPUT , 'file' : 10 , ' ' : 4 ,
                                    STPTOINT  (*ord*) ( FILTYPE ) :
                                    INTSIZE  (*6*) ) ;
                            FOLLOWSTP ( FILTYPE )
                          end (* tag/ca *) ;
                  TAGFLD :
                    begin
                      WRITELN ( OUTPUT , 'tagfld' : 10 , ' ' : 4 ,
                                CTPTOINT  (*ord*) ( TAGFIELDP ) :
                                INTSIZE  (*6*) , ' ' : 4 , STPTOINT

        (*****)
        (*ord*)
        (*****)

                                ( FSTVAR ) : INTSIZE  (*6*) ) ;
                      FOLLOWSTP ( FSTVAR )
                    end (* tag/ca *) ;
                  VARIANT :
                    begin
                      WRITELN ( OUTPUT , 'variant' : 10 , ' ' : 4 ,
                                STPTOINT  (*ord*) ( NXTVAR ) : INTSIZE

        (***)
        (*6*)
        (***)

                                , ' ' : 4 , STPTOINT  (*ord*) ( SUBVAR
                                ) : INTSIZE  (*6*) , VARVAL . IVAL ) ;
                      FOLLOWSTP ( NXTVAR ) ;
                      FOLLOWSTP ( SUBVAR )
                    end (* tag/ca *)
                end (* case *)
              end (* then *)
      end (* FOLLOWSTP *) ;


   procedure FOLLOWCTP ;

      begin (* FOLLOWCTP *)
        if FP <> NIL then
          with FP -> do
            begin
              WRITE ( OUTPUT , ' ' : 4 , CTPTOINT  (*ord*) ( FP ) :
                      INTSIZE  (*6*) , ' ' ) ;
              WRITEV ( OUTPUT , NAME , 9 ) ;
              WRITE ( ' ' : 4 , CTPTOINT  (*ord*) ( LLINK ) : INTSIZE

        (***)
        (*6*)
        (***)

                      , ' ' : 4 , CTPTOINT  (*ord*) ( RLINK ) : INTSIZE

        (***)
        (*6*)
        (***)

                      , ' ' : 4 , STPTOINT  (*ord*) ( IDTYPE ) :
                      INTSIZE  (*6*) ) ;
              case KLASS of
                TYPES : WRITE ( OUTPUT , 'type' : 10 ) ;
                KONST : begin
                          WRITE ( OUTPUT , 'constant' : 10 , ' ' : 4 ,
                                  CTPTOINT  (*ord*) ( NEXT ) : INTSIZE

        (***)
        (*6*)
        (***)

                                  ) ;
                          if IDTYPE <> NIL then
                            if IDTYPE = REALPTR then
                              begin
                                if VALUES . VALP <> NIL then
                                  begin
                                    WRITE ( ' ' ) ;
                                    WRITEV ( OUTPUT , VALUES . VALP ->
                                             . RVAL , 9 )
                                  end (* then *)
                              end (* then *)
                            else
                              if IDTYPE -> . FORM = ARRAYS then

        (*************)
        (*stringconst*)
        (*************)

                                begin
                                  if VALUES . VALP <> NIL then
                                    begin
                                      WRITE ( OUTPUT , ' ' ) ;
                                      with VALUES . VALP -> do
                                        WRITEV ( OUTPUT , SVAL , SLGTH
                                                 )
                                    end (* then *)
                                end (* then *)
                              else
                                WRITE ( OUTPUT , VALUES . IVAL )
                        end (* tag/ca *) ;
                VARS : begin
                         WRITE ( OUTPUT , 'variable' : 10 ) ;
                         if VKIND = ACTUAL then
                           WRITE ( OUTPUT , 'actual' : 10 )
                         else
                           WRITE ( OUTPUT , 'formal' : 10 ) ;
                         WRITE ( OUTPUT , ' ' : 4 , CTPTOINT  (*ord*) (
                                 NEXT ) : INTSIZE  (*6*) , VLEV , ' ' :
                                 4 , VADDR : 6 ) ;
                       end (* tag/ca *) ;
                FIELD : WRITE ( OUTPUT , 'field' : 10 , ' ' : 4 ,
                                CTPTOINT  (*ord*) ( NEXT ) : INTSIZE

        (***)
        (*6*)
        (***)

                                , ' ' : 4 , FLDADDR : 6 ) ;
                PROC , FUNC :
                  begin
                    if KLASS = PROC then
                      WRITE ( OUTPUT , 'procedure' : 10 )
                    else
                      WRITE ( OUTPUT , 'function' : 10 ) ;
                    if PFDECKIND = STANDARD then
                      WRITE ( OUTPUT , 'standard' : 10 , KEY : 10 )
                    else
                      begin
                        WRITE ( OUTPUT , 'declared' : 10 , ' ' : 4 ,
                                CTPTOINT  (*ord*) ( NEXT ) : INTSIZE

        (***)
        (*6*)
        (***)

                                ) ;
                        WRITE ( OUTPUT , PFLEV , ' ' : 4 , PFNAME : 6 )
                                ;
                        if PFKIND = ACTUAL then
                          begin
                            WRITE ( OUTPUT , 'actual' : 10 ) ;
                            if FORWDECL then
                              WRITE ( OUTPUT , 'forward' : 10 )
                            else
                              WRITE ( OUTPUT , 'notforward' : 10 ) ;
                            if EXTERNL then
                              WRITE ( OUTPUT , 'extern' : 10 )
                            else
                              WRITE ( OUTPUT , 'not extern' : 10 ) ;
                          end (* then *)
                        else
                          WRITE ( OUTPUT , 'formal' : 10 )
                      end (* else *)
                  end (* tag/ca *)
              end (* case *) ;
              WRITELN ( OUTPUT ) ;
              FOLLOWCTP ( LLINK ) ;
              FOLLOWCTP ( RLINK ) ;
              FOLLOWSTP ( IDTYPE )
            end (* with *)
      end (* FOLLOWCTP *) ;


   begin (* PRINTTABLES *)
     WRITELN ( OUTPUT ) ;
     WRITELN ( OUTPUT ) ;
     WRITELN ( OUTPUT ) ;
     if FB then
       LIM := 0
     else
       begin
         LIM := TOP ;
         WRITE ( OUTPUT , ' local' )
       end (* else *) ;
     WRITELN ( OUTPUT , ' tables ' ) ;
     WRITELN ( OUTPUT ) ;
     MARKER ;
     for I := TOP DOWNTO LIM do
       FOLLOWCTP ( DISPLAY [ I ] . FNAME ) ;
     WRITELN ( OUTPUT ) ;
     if not EOL then
       WRITE ( OUTPUT , ' ' : CHCNT + 16 )
   end (* PRINTTABLES *) ;



procedure GENLABEL ( var NXTLAB : INTEGER ) ;

   begin (* GENLABEL *)
     INTLABEL := INTLABEL + 1 ;
     NXTLAB := INTLABEL
   end (* GENLABEL *) ;



procedure SEARCHLABEL ( var LLP : LBP ; LEVEL : DISPRANGE ) ;

   var FLLP : LBP ; (* found label entry *)

   begin (* SEARCHLABEL *)
     FLLP := NIL ; (* set no label found *)
     LLP := DISPLAY [ LEVEL ] . FLABEL ; (* index top of label list *)
     while LLP <> NIL do
       begin      (* traverse *)
         if LLP -> . LABVAL = VAL . IVAL then
           begin  (* found *)
             FLLP := LLP ; (* set entry found *)
             LLP := NIL (* stop *)
           end (* then *)
         else
           LLP := LLP -> . NEXTLAB (* next in list *)
       end (* while *) ;
     LLP := FLLP (* return found entry or nil *)
   end (* SEARCHLABEL *) ;



procedure NEWLABEL ( var LLP : LBP ) ;

   var LBNAME : INTEGER ;

   begin (* NEWLABEL *)
     with DISPLAY [ TOP ] do
       begin
         GETLAB ( LLP ) ;
         with LLP -> do
           begin
             LABVAL := VAL . IVAL ;
             GENLABEL ( LBNAME ) ;
             DEFINED := FALSE ;
             NEXTLAB := FLABEL ;
             LABNAME := LBNAME ;
             VLEVEL := LEVEL ;
             SLEVEL := 0 ;
             IPCREF := FALSE ;
             MINLVL := MAXINT ;
             BACT := FALSE ;
           end (* with *) ;
         FLABEL := LLP
       end (* with *)
   end (* NEWLABEL *) ;



procedure PRTLABELS ;

   var LLP : LBP ; (* found label entry *)

   begin (* PRTLABELS *)
     WRITELN ;
     WRITELN ( 'Labels: ' ) ;
     WRITELN ;
     LLP := DISPLAY [ LEVEL ] . FLABEL ; (* index top of label list *)
     while LLP <> NIL do
       with LLP -> do
         begin                          (* traverse *)
           WRITELN ( 'label: ' , LABVAL : 1 , ' defined: ' , DEFINED ,
                     ' internal: ' , LABNAME : 1 , ' vlevel: ' , VLEVEL
                     : 1 , ' slevel: ' , SLEVEL : 1 , ' ipcref: ' ,
                     IPCREF : 1 , ' minlvl: ' , MINLVL : 1 ) ;
           WRITELN ( '   bact: ' , BACT ) ;
           LLP := LLP -> . NEXTLAB (* next in list *)
         end (* with *)
   end (* PRTLABELS *) ;



procedure BLOCK ( FSYS : SETOFSYS ; FSY : SYMBOL ; FPROCP : CTP ) ;

   var LSY : SYMBOL ;
       STALVL : INTEGER ; (* statement nesting level *)


   procedure SKIP ( FSYS : SETOFSYS ) ;

   (***********************************************)
   (*skip input string until relevant symbol found*)
   (***********************************************)


      begin (* SKIP *)
        if not EOF ( INPUT ) then
          begin
            while not ( SY in FSYS ) and ( not EOF ( INPUT ) ) do
              INSYMBOL ;
            if not ( SY in FSYS ) then
              INSYMBOL
          end (* then *)
      end (* SKIP *) ;


   procedure CONSTANT ( FSYS : SETOFSYS ; var FSP : STP ; var FVALU :
                      VALU ) ;

      var LSP : STP ;
          LCP : CTP ;
          SIGN : ( NONE , POS , NEG ) ;
          LVP : CSP ;
          I : 2 .. STRGLGTH ;

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
                if LGTH = 1 then
                  LSP := CHARPTR
                else
                  begin
                    NEW ( LSP , ARRAYS ) ;
                    PSHSTC ( LSP ) ;
                    with LSP -> do
                      begin
                        AELTYPE := CHARPTR ;
                        INXTYPE := NIL ;
                        SIZE := LGTH * CHARSIZE ;
                        FORM := ARRAYS ;
                        PACKING := TRUE
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
                                PSHCST ( LVP ) ;
                                if STRCHR ( FVALU . VALP -> . RVAL , 1
                                ) = '-' then
                                  STRCHRASS ( LVP -> . RVAL , 1 , '+' )
                                else
                                  STRCHRASS ( LVP -> . RVAL , 1 , '-' )
                                              ;
                                for I := 2 to DIGMAX do
                                  STRCHRASS ( LVP -> . RVAL , I ,
                                              STRCHR ( FVALU . VALP ->
                                              . RVAL , I ) ) ;
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
                          STRCHRASS ( VAL . VALP -> . RVAL , 1 , '-' )
                                      ;
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


   function STRING ( FSP : STP ) : BOOLEAN ;

      FORWARD ;


   function COMPTYPES ( FSP1 , FSP2 : STP ) : BOOLEAN ;

   (**********************************************************************)
   (*decide whether structures pointed at by fsp1 and fsp2 are compatible*)
   (**********************************************************************)


      begin (* COMPTYPES *)
        COMPTYPES := FALSE ;

        (*************************************************************)
        (* set default is false                                      *)
        (* Check equal. Aliases of the same type will also be equal. *)
        (*************************************************************)

        if FSP1 = FSP2 then
          COMPTYPES := TRUE
        else
          if ( FSP1 <> NIL ) and ( FSP2 <> NIL ) then
            if FSP1 -> . FORM = FSP2 -> . FORM then
              case FSP1 -> . FORM of
                SCALAR :
                  ;

        (***************************************************************)
        (* Subranges are compatible if either type is a subrange of the*)
        (*                other, or if the base type is the same.      *)
        (***************************************************************)

                SUBRANGE :
                  COMPTYPES := ( FSP1 -> . RANGETYPE = FSP2 ) or ( FSP2
                               -> . RANGETYPE = FSP1 ) or ( FSP1 -> .
                               RANGETYPE = FSP2 -> . RANGETYPE ) ;

        (*********************************************************************************)
        (* Sets are compatible if they have the same base types and packed/              *)
        (*                unpacked status, or one of them is the empty set. The empty set*)
        (*                is indicated by a nil base type, which is identical to a base  *)
        (*                type in error. Either way, we treat them as compatible.        *)
        (*                                                                               *)
        (*                Set types created for set constants have a flag that disables  *)
        (*                packing matches. This is because set constants can be packed or*)
        (*                unpacked by context.                                           *)
        (*********************************************************************************)

                POWER : COMPTYPES := ( COMPTYPES ( FSP1 -> . ELSET ,
                                     FSP2 -> . ELSET ) and ( ( FSP1 ->
                                     . PACKING = FSP2 -> . PACKING ) or
                                     not FSP1 -> . MATCHPACK or not
                                     FSP2 -> . MATCHPACK ) ) or ( FSP1
                                     -> . ELSET = NIL ) or ( FSP2 -> .
                                     ELSET = NIL ) ;

        (********************************************************************)
        (* Arrays are compatible if they are string types and equal in size *)
        (********************************************************************)

                ARRAYS :
                  COMPTYPES := STRING ( FSP1 ) and STRING ( FSP2 ) and
                               ( FSP1 -> . SIZE = FSP2 -> . SIZE ) ;

        (*********************************************************************************)
        (* Pointers, must either be the same type or aliases of the same                 *)
        (*                type, or one must be nil. The nil pointer is indicated by a nil*)
        (*                base type, which is identical to a base type in error. Either  *)
        (*                way, we treat them as compatible.                              *)
        (*********************************************************************************)

                POINTER :
                  COMPTYPES := ( FSP1 -> . ELTYPE = NIL ) or ( FSP2 ->
                               . ELTYPE = NIL ) ;

        (*******************************************************************)
        (* records and files must either be the same type or aliases of the*)
        (*                same type                                        *)
        (*******************************************************************)

                RECORDS :
                  ;
                FILES :
              end (* case *)
            else (*fsp1^.form <> fsp2^.form                      *)
                 (* subranges of a base type match the base type *)
              if FSP1 -> . FORM = SUBRANGE then
                COMPTYPES := FSP1 -> . RANGETYPE = FSP2
              else
                if FSP2 -> . FORM = SUBRANGE then
                  COMPTYPES := FSP1 = FSP2 -> . RANGETYPE
                else
                  COMPTYPES := FALSE
          else
            COMPTYPES := TRUE (* one of the types is in error *)
      end (* COMPTYPES *) ;

      (*******************************************)
      (* check structure is, or contains, a file *)
      (*******************************************)



   function FILECOMPONENT ( FSP : STP ) : BOOLEAN ;

      var F : BOOLEAN ;

          (************************)
          (* tour identifier tree *)
          (************************)



      function FILECOMPONENTRE ( LCP : CTP ) : BOOLEAN ;

         var F : BOOLEAN ;

         begin (* FILECOMPONENTRE *)
           F := FALSE ; (* set not file by default *)
           if LCP <> NIL then
             with LCP -> do
               begin
                 if FILECOMPONENT ( IDTYPE ) then
                   F := TRUE ;
                 if FILECOMPONENTRE ( LLINK ) then
                   F := TRUE ;
                 if FILECOMPONENTRE ( RLINK ) then
                   F := TRUE
               end (* with *) ;
           FILECOMPONENTRE := F
         end (* FILECOMPONENTRE *) ;


      begin (* FILECOMPONENT *)
        F := FALSE ; (* set not a file by default *)
        if FSP <> NIL then
          with FSP -> do
            case FORM of
              SCALAR :
                ;
              SUBRANGE :
                ;
              POINTER :
                ;
              POWER : ;
              ARRAYS :
                if FILECOMPONENT ( AELTYPE ) then
                  F := TRUE ;
              RECORDS :
                if FILECOMPONENTRE ( FSTFLD ) then
                  F := TRUE ;
              FILES : F := TRUE ;
              TAGFLD :
                ;
              VARIANT :
                ;
            end (* case *) ;
        FILECOMPONENT := F
      end (* FILECOMPONENT *) ;


   function STRING ;

      var FMIN , FMAX : INTEGER ;

      begin (* STRING *)
        STRING := FALSE ;
        if FSP <> NIL then
          if ( FSP -> . FORM = ARRAYS ) and FSP -> . PACKING then
            begin

        (***********************************************************************)
        (* if the index is nil, either the array is a string constant or the   *)
        (*            index type was in error. Either way, we call it a string *)
        (***********************************************************************)

              if FSP -> . INXTYPE = NIL then
                FMIN := 1
              else
                GETBOUNDS ( FSP -> . INXTYPE , FMIN , FMAX ) ;
              if COMPTYPES ( FSP -> . AELTYPE , CHARPTR ) and ( FMIN =
              1 ) then
                STRING := TRUE
            end (* then *)
      end (* STRING *) ;

          (******************************************************)
          (* resolve all pointer references in the forward list *)
          (******************************************************)



   procedure RESOLVEP ;

      var IDS : IDSTR ;
          LCP1 , LCP2 : CTP ;
          MM , FE : BOOLEAN ;

      begin (* RESOLVEP *)
        IDS := ID ;
        FE := TRUE ;
        while FWPTR <> NIL do
          begin
            LCP1 := FWPTR ;
            FWPTR := LCP1 -> . NEXT ;
            STRASSFV ( ID , LCP1 -> . NAME ) ;
            SEARCHIDNENM ( [ TYPES ] , LCP2 , MM ) ;
            if LCP2 <> NIL then
              LCP1 -> . IDTYPE -> . ELTYPE := LCP2 -> . IDTYPE
            else
              begin
                if FE then
                  begin
                    ERROR ( 117 ) ;
                    WRITELN ( OUTPUT )
                  end (* then *) ;
                WRITE ( '*** undefined type-id forward reference: ' ) ;
                WRITEV ( OUTPUT , LCP1 -> . NAME , PRTLLN ) ;
                WRITELN ;
                FE := FALSE
              end (* else *) ;
            PUTNAM ( LCP1 )
          end (* while *) ;
        ID := IDS
      end (* RESOLVEP *) ;


   procedure TYP ( FSYS : SETOFSYS ; var FSP : STP ; var FSIZE :
                 ADDRRANGE ) ;

      var LSP , LSP1 , LSP2 : STP ;
          OLDTOP : DISPRANGE ;
          LCP : CTP ;
          LSIZE , DISPL : ADDRRANGE ;
          LMIN , LMAX : INTEGER ;
          TEST : BOOLEAN ;
          ISPACKED : BOOLEAN ;


      procedure SIMPLETYPE ( FSYS : SETOFSYS ; var FSP : STP ; var
                           FSIZE : ADDRRANGE ) ;

         var LSP , LSP1 : STP ;
             LCP , LCP1 : CTP ;
             TTOP : DISPRANGE ;
             LCNT : INTEGER ;
             LVALU : VALU ;

         begin (* SIMPLETYPE *)
           FSIZE := 1 ;
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
           (*decl. consts local to innermost block*)
           (***************************************)

                   while DISPLAY [ TOP ] . OCCUR <> BLCK do
                     TOP := TOP - 1 ;
                   NEW ( LSP , SCALAR , DECLARED ) ;
                   PSHSTC ( LSP ) ;
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
                         ININAM ( LCP ) ;
                         with LCP -> do
                           begin
                             STRASSVF ( NAME , ID ) ;
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
                           PSHSTC ( LSP ) ;
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
                               SIZE := INTSIZE
                             end (* with *) ;
                           if SY = RANGE then
                             INSYMBOL
                           else
                             ERROR ( 5 ) ;
                           CONSTANT ( FSYS , LSP1 , LVALU ) ;
                           LSP -> . MAX := LVALU ;
                           if LSP -> . RANGETYPE <> LSP1 then
                             ERROR ( 107 )
                         end (* then *)
                       else
                         begin
                           LSP := LCP -> . IDTYPE ;
                           if LSP <> NIL then
                             FSIZE := LSP -> . SIZE
                         end (* else *)
                     end (* then *)
                   else
                     begin
                       NEW ( LSP , SUBRANGE ) ;
                       PSHSTC ( LSP ) ;
                       LSP -> . FORM := SUBRANGE ;
                       CONSTANT ( FSYS + [ RANGE ] , LSP1 , LVALU ) ;
                       if STRING ( LSP1 ) then
                         begin
                           ERROR ( 148 ) ;
                           LSP1 := NIL
                         end (* then *) ;
                       with LSP -> do
                         begin
                           RANGETYPE := LSP1 ;
                           MIN := LVALU ;
                           SIZE := INTSIZE
                         end (* with *) ;
                       if SY = RANGE then
                         INSYMBOL
                       else
                         ERROR ( 5 ) ;
                       CONSTANT ( FSYS , LSP1 , LVALU ) ;
                       LSP -> . MAX := LVALU ;
                       if LSP -> . RANGETYPE <> LSP1 then
                         ERROR ( 107 )
                     end (* else *) ;
                   if LSP <> NIL then
                     with LSP -> do
                       if FORM = SUBRANGE then
                         if RANGETYPE <> NIL then
                           if RANGETYPE = REALPTR then
                             ERROR ( 399 )
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


      procedure FIELDLIST ( FSYS : SETOFSYS ; var FRECVAR : STP ) ;

         var LCP , LCP1 , NXT , NXT1 : CTP ;
             LSP , LSP1 , LSP2 , LSP3 , LSP4 : STP ;
             MINSIZE , MAXSIZE , LSIZE : ADDRRANGE ;
             LVALU : VALU ;
             TEST : BOOLEAN ;
             MM : BOOLEAN ;

         begin (* FIELDLIST *)
           NXT1 := NIL ;
           LSP := NIL ;
           if not ( SY in ( FSYS + [ IDENT , CASESY ] ) ) then
             begin
               ERROR ( 19 ) ;
               SKIP ( FSYS + [ IDENT , CASESY ] )
             end (* then *) ;
           while SY = IDENT do
             begin
               NXT := NXT1 ;
               repeat
                 if SY = IDENT then
                   begin
                     NEW ( LCP , FIELD ) ;
                     ININAM ( LCP ) ;
                     with LCP -> do
                       begin
                         STRASSVF ( NAME , ID ) ;
                         IDTYPE := NIL ;
                         NEXT := NXT ;
                         KLASS := FIELD
                       end (* with *) ;
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
               TYP ( FSYS + [ CASESY , SEMICOLON ] , LSP , LSIZE ) ;
               while NXT <> NXT1 do
                 with NXT -> do
                   begin
                     ALIGN ( LSP , DISPL ) ;
                     IDTYPE := LSP ;
                     FLDADDR := DISPL ;
                     NXT := NEXT ;
                     DISPL := DISPL + LSIZE
                   end (* with *) ;
               NXT1 := LCP ;
               while SY = SEMICOLON do
                 begin
                   INSYMBOL ;
                   if not ( SY in FSYS + [ IDENT , CASESY , SEMICOLON ]
                   ) then
                     begin
                       ERROR ( 19 ) ;
                       SKIP ( FSYS + [ IDENT , CASESY ] )
                     end (* then *)
                 end (* while *)
             end (* while *) ;
           NXT := NIL ;
           while NXT1 <> NIL do
             with NXT1 -> do
               begin
                 LCP := NEXT ;
                 NEXT := NXT ;
                 NXT := NXT1 ;
                 NXT1 := LCP
               end (* with *) ;
           if SY = CASESY then
             begin
               NEW ( LSP , TAGFLD ) ;
               PSHSTC ( LSP ) ;
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

           (****************************)
           (* find possible type first *)
           (****************************)

                   SEARCHIDNENM ( [ TYPES ] , LCP1 , MM ) ;

           (**************************)
           (* now set up as field id *)
           (**************************)

                   NEW ( LCP , FIELD ) ;
                   ININAM ( LCP ) ;
                   with LCP -> do
                     begin
                       STRASSVF ( NAME , ID ) ;
                       IDTYPE := NIL ;
                       KLASS := FIELD ;
                       NEXT := NIL ;
                       FLDADDR := DISPL
                     end (* with *) ;
                   INSYMBOL ;

           (********************************************************)
           (* If type only (undiscriminated variant), kill the id. *)
           (********************************************************)

                   if SY = COLON then
                     begin
                       ENTERID ( LCP ) ;
                       INSYMBOL ;
                       if SY = IDENT then
                         begin
                           SEARCHID ( [ TYPES ] , LCP1 ) ;
                           INSYMBOL
                         end (* then *)
                       else
                         begin
                           ERROR ( 2 ) ;
                           SKIP ( FSYS + [ OFSY , LPARENT ] ) ;
                           LCP1 := NIL
                         end (* else *)
                     end (* then *)
                   else
                     begin
                       if MM then
                         ERROR ( 103 ) ;
                       PUTSTRS ( LCP -> . NAME ) ;

           (***********************)
           (* release name string *)
           (***********************)

                       LCP -> . NAME := NIL (* set no tagfield *)
                     end (* else *) ;
                   if LCP1 <> NIL then
                     begin
                       LSP1 := LCP1 -> . IDTYPE ;
                       if LSP1 <> NIL then
                         begin
                           ALIGN ( LSP1 , DISPL ) ;
                           LCP -> . FLDADDR := DISPL ;

           (********************************)
           (* only allocate field if named *)
           (********************************)

                           if LCP -> . NAME <> NIL then
                             DISPL := DISPL + LSP1 -> . SIZE ;
                           if ( LSP1 -> . FORM <= SUBRANGE ) or STRING
                           ( LSP1 ) then
                             begin
                               if COMPTYPES ( REALPTR , LSP1 ) then
                                 ERROR ( 109 )
                               else
                                 if STRING ( LSP1 ) then
                                   ERROR ( 399 ) ;
                               LCP -> . IDTYPE := LSP1 ;
                               LSP -> . TAGFIELDP := LCP ;
                             end (* then *)
                           else
                             ERROR ( 110 ) ;
                         end (* then *)
                     end (* then *)
                 end (* then *)
               else
                 begin
                   ERROR ( 2 ) ;
                   SKIP ( FSYS + [ OFSY , LPARENT ] )
                 end (* else *) ;
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
                 if not ( SY in FSYS + [ SEMICOLON ] ) then
                   begin
                     repeat
                       CONSTANT ( FSYS + [ COMMA , COLON , LPARENT ] ,
                                  LSP3 , LVALU ) ;
                       if LSP -> . TAGFIELDP <> NIL then
                         if not COMPTYPES ( LSP -> . TAGFIELDP -> .
                         IDTYPE , LSP3 ) then
                           ERROR ( 111 ) ;
                       NEW ( LSP3 , VARIANT ) ;
                       PSHSTC ( LSP3 ) ;
                       with LSP3 -> do
                         begin
                           NXTVAR := LSP1 ;
                           SUBVAR := LSP2 ;
                           VARVAL := LVALU ;
                           FORM := VARIANT
                         end (* with *) ;
                       LSP4 := LSP1 ;
                       while LSP4 <> NIL do
                         with LSP4 -> do
                           begin
                             if VARVAL . IVAL = LVALU . IVAL then
                               ERROR ( 178 ) ;
                             LSP4 := NXTVAR
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
                     FIELDLIST ( FSYS + [ RPARENT , SEMICOLON ] , LSP2
                                 ) ;
                     if DISPL > MAXSIZE then
                       MAXSIZE := DISPL ;
                     while LSP3 <> NIL do
                       begin
                         LSP4 := LSP3 -> . SUBVAR ;
                         LSP3 -> . SUBVAR := LSP2 ;
                         LSP3 -> . SIZE := DISPL ;
                         LSP3 := LSP4
                       end (* while *) ;
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
                   end (* then *) ;
                 TEST := SY <> SEMICOLON ;
                 if not TEST then
                   begin
                     DISPL := MINSIZE ;
                     INSYMBOL
                   end (* then *)
               until TEST ;
               DISPL := MAXSIZE ;
               LSP -> . FSTVAR := LSP1 ;
             end (* then *)
           else
             FRECVAR := NIL
         end (* FIELDLIST *) ;


      begin (* TYP *)
        if not ( SY in TYPEBEGSYS ) then
          begin
            ERROR ( 10 ) ;
            SKIP ( FSYS + TYPEBEGSYS )
          end (* then *) ;
        if SY in TYPEBEGSYS then
          begin
            if SY in SIMPTYPEBEGSYS then
              SIMPLETYPE ( FSYS , FSP , FSIZE )
            else

        (***)
        (*^*)
        (***)

              if SY = ARROW then
                begin
                  NEW ( LSP , POINTER ) ;
                  PSHSTC ( LSP ) ;
                  FSP := LSP ;
                  with LSP -> do
                    begin
                      ELTYPE := NIL ;
                      SIZE := PTRSIZE ;
                      FORM := POINTER
                    end (* with *) ;
                  INSYMBOL ;
                  if SY = IDENT then
                    begin (* forward reference everything *)
                      NEW ( LCP , TYPES ) ;
                      ININAM ( LCP ) ;
                      with LCP -> do
                        begin
                          STRASSVF ( NAME , ID ) ;
                          IDTYPE := LSP ;
                          NEXT := FWPTR ;
                          KLASS := TYPES
                        end (* with *) ;
                      FWPTR := LCP ;
                      INSYMBOL ;
                    end (* then *)
                  else
                    ERROR ( 2 ) ;
                end (* then *)
              else
                begin
                  ISPACKED := FALSE ; (* set not packed by default *)
                  if SY = PACKEDSY then
                    begin
                      INSYMBOL ;
                      ISPACKED := TRUE ;           (* packed *)
                      if not ( SY in TYPEDELS ) then
                        begin
                          ERROR ( 10 ) ;
                          SKIP ( FSYS + TYPEDELS )
                        end (* then *)
                    end (* then *) ;

        (*******)
        (*array*)
        (*******)

                  if SY = ARRAYSY then
                    begin
                      INSYMBOL ;
                      if SY = LBRACK then
                        INSYMBOL
                      else
                        ERROR ( 11 ) ;
                      LSP1 := NIL ;
                      repeat
                        NEW ( LSP , ARRAYS ) ;
                        PSHSTC ( LSP ) ;
                        with LSP -> do
                          begin
                            AELTYPE := LSP1 ;
                            INXTYPE := NIL ;
                            FORM := ARRAYS ;
                            PACKING := ISPACKED
                          end (* with *) ;
                        LSP1 := LSP ;
                        SIMPLETYPE ( FSYS + [ COMMA , RBRACK , OFSY ] ,
                                     LSP2 , LSIZE ) ;
                        LSP1 -> . SIZE := LSIZE ;
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
                        ERROR ( 12 ) ;
                      if SY = OFSY then
                        INSYMBOL
                      else
                        ERROR ( 8 ) ;
                      TYP ( FSYS , LSP , LSIZE ) ;
                      repeat
                        with LSP1 -> do
                          begin
                            LSP2 := AELTYPE ;
                            AELTYPE := LSP ;
                            if INXTYPE <> NIL then
                              begin
                                GETBOUNDS ( INXTYPE , LMIN , LMAX ) ;
                                ALIGN ( LSP , LSIZE ) ;
                                LSIZE := LSIZE * ( LMAX - LMIN + 1 ) ;
                                SIZE := LSIZE
                              end (* then *)
                          end (* with *) ;
                        LSP := LSP1 ;
                        LSP1 := LSP2
                      until LSP1 = NIL
                    end (* then *)
                  else

        (********)
        (*record*)
        (********)

                    if SY = RECORDSY then
                      begin
                        INSYMBOL ;
                        OLDTOP := TOP ;
                        if TOP < DISPLIMIT then
                          begin
                            TOP := TOP + 1 ;
                            with DISPLAY [ TOP ] do
                              begin
                                FNAME := NIL ;
                                FLABEL := NIL ;
                                FCONST := NIL ;
                                FSTRUCT := NIL ;
                                OCCUR := REC
                              end (* with *)
                          end (* then *)
                        else
                          ERROR ( 250 ) ;
                        DISPL := 0 ;
                        FIELDLIST ( FSYS - [ SEMICOLON ] + [ ENDSY ] ,
                                    LSP1 ) ;
                        NEW ( LSP , RECORDS ) ;
                        with LSP -> do
                          begin
                            FSTFLD := DISPLAY [ TOP ] . FNAME ;
                            DISPLAY [ TOP ] . FNAME := NIL ;
                            RECVAR := LSP1 ;
                            SIZE := DISPL ;
                            FORM := RECORDS ;
                            PACKING := ISPACKED ;
                            RECYC := DISPLAY [ TOP ] . FSTRUCT ;
                            DISPLAY [ TOP ] . FSTRUCT := NIL
                          end (* with *) ;
                        PUTDSPS ( OLDTOP ) ;
                        TOP := OLDTOP ;

        (*******************************************************)
        (* register the record late because of the purge above *)
        (*******************************************************)

                        PSHSTC ( LSP ) ;
                        if SY = ENDSY then
                          INSYMBOL
                        else
                          ERROR ( 13 )
                      end (* then *)
                    else

        (*****)
        (*set*)
        (*****)

                      if SY = SETSY then
                        begin
                          INSYMBOL ;
                          if SY = OFSY then
                            INSYMBOL
                          else
                            ERROR ( 8 ) ;
                          SIMPLETYPE ( FSYS , LSP1 , LSIZE ) ;
                          if LSP1 <> NIL then
                            if LSP1 -> . FORM > SUBRANGE then
                              begin
                                ERROR ( 115 ) ;
                                LSP1 := NIL
                              end (* then *)
                            else
                              if LSP1 = REALPTR then
                                begin
                                  ERROR ( 114 ) ;
                                  LSP1 := NIL
                                end (* then *)
                              else
                                if LSP1 = INTPTR then
                                  begin
                                    ERROR ( 169 ) ;
                                    LSP1 := NIL
                                  end (* then *)
                                else
                                  begin
                                    GETBOUNDS ( LSP1 , LMIN , LMAX ) ;
                                    if ( LMIN < SETLOW ) or ( LMAX >
                                    SETHIGH ) then
                                      ERROR ( 169 ) ;
                                  end (* else *) ;
                          NEW ( LSP , POWER ) ;
                          PSHSTC ( LSP ) ;
                          with LSP -> do
                            begin
                              ELSET := LSP1 ;
                              SIZE := SETSIZE ;
                              FORM := POWER ;
                              PACKING := ISPACKED ;
                              MATCHPACK := TRUE
                            end (* with *) ;
                        end (* then *)
                      else

        (******)
        (*file*)
        (******)

                        if SY = FILESY then
                          begin
                            INSYMBOL ;
                            if SY = OFSY then
                              INSYMBOL
                            else
                              ERROR ( 8 ) ;
                            TYP ( FSYS , LSP1 , LSIZE ) ;
                            if FILECOMPONENT ( LSP1 ) then
                              ERROR ( 190 ) ;
                            NEW ( LSP , FILES ) ;
                            PSHSTC ( LSP ) ;
                            with LSP -> do
                              begin
                                FILTYPE := LSP1 ;
                                SIZE := FILESIZE + LSIZE ;
                                FORM := FILES ;
                                PACKING := ISPACKED
                              end (* with *)
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
          FSIZE := FSP -> . SIZE
      end (* TYP *) ;


   procedure LABELDECLARATION ;

      var LLP : LBP ;
          TEST : BOOLEAN ;

      begin (* LABELDECLARATION *)
        repeat
          if SY = INTCONST then
            begin
              SEARCHLABEL ( LLP , TOP ) ;

        (****************************)
        (* search preexisting label *)
        (****************************)

              if LLP <> NIL then
                ERROR ( 166 )          (* multideclared label *)
              else
                NEWLABEL ( LLP ) ;
              INSYMBOL
            end (* then *)
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


   procedure CONSTDECLARATION ;

      var LCP : CTP ;
          LSP : STP ;
          LVALU : VALU ;

      begin (* CONSTDECLARATION *)
        if SY <> IDENT then
          begin
            ERROR ( 2 ) ;
            SKIP ( FSYS + [ IDENT ] )
          end (* then *) ;
        while SY = IDENT do
          begin
            NEW ( LCP , KONST ) ;
            ININAM ( LCP ) ;
            with LCP -> do
              begin
                STRASSVF ( NAME , ID ) ;
                IDTYPE := NIL ;
                NEXT := NIL ;
                KLASS := KONST
              end (* with *) ;
            INSYMBOL ;
            if ( SY = RELOP ) and ( OP = EQOP ) then
              INSYMBOL
            else
              ERROR ( 16 ) ;
            CONSTANT ( FSYS + [ SEMICOLON ] , LSP , LVALU ) ;
            ENTERID ( LCP ) ;
            LCP -> . IDTYPE := LSP ;
            LCP -> . VALUES := LVALU ;
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
          end (* while *)
      end (* CONSTDECLARATION *) ;


   procedure TYPEDECLARATION ;

      var LCP : CTP ;
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
            ININAM ( LCP ) ;
            with LCP -> do
              begin
                STRASSVF ( NAME , ID ) ;
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
        RESOLVEP
      end (* TYPEDECLARATION *) ;


   procedure VARDECLARATION ;

      var LCP , NXT : CTP ;
          LSP : STP ;
          LSIZE : ADDRRANGE ;
          TEST : BOOLEAN ;

      begin (* VARDECLARATION *)
        NXT := NIL ;
        repeat
          repeat
            if SY = IDENT then
              begin
                NEW ( LCP , VARS ) ;
                ININAM ( LCP ) ;
                with LCP -> do
                  begin
                    STRASSVF ( NAME , ID ) ;
                    NEXT := NXT ;
                    KLASS := VARS ;
                    IDTYPE := NIL ;
                    VKIND := ACTUAL ;
                    VLEV := LEVEL
                  end (* with *) ;
                ENTERID ( LCP ) ;
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
          while NXT <> NIL do
            with NXT -> do
              begin
                ALIGN ( LSP , LC ) ;
                IDTYPE := LSP ;
                VADDR := LC ;
                LC := LC + LSIZE ;
                NXT := NEXT
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
        RESOLVEP
      end (* VARDECLARATION *) ;


   procedure PROCDECLARATION ( FSY : SYMBOL ) ;

      var OLDLEV : 0 .. MAXLEVEL ;
          LCP , LCP1 : CTP ;
          LSP : STP ;
          FORW : BOOLEAN ;
          OLDTOP : DISPRANGE ;
          LLC , LCM : ADDRRANGE ;
          LBNAME : INTEGER ;                  (*markp: marktype;*)


      procedure PUSHLVL ( FORW : BOOLEAN ; LCP : CTP ) ;

         begin (* PUSHLVL *)
           if LEVEL < MAXLEVEL then
             LEVEL := LEVEL + 1
           else
             ERROR ( 251 ) ;
           if TOP < DISPLIMIT then
             begin
               TOP := TOP + 1 ;
               with DISPLAY [ TOP ] do
                 begin
                   if FORW then
                     FNAME := LCP -> . PFLIST
                   else
                     FNAME := NIL ;
                   FLABEL := NIL ;
                   FCONST := NIL ;
                   FSTRUCT := NIL ;
                   OCCUR := BLCK ;
                   BNAME := LCP
                 end (* with *)
             end (* then *)
           else
             ERROR ( 250 ) ;
         end (* PUSHLVL *) ;


      procedure PARAMETERLIST ( FSY : SETOFSYS ; var FPAR : CTP ) ;

         var LCP , LCP1 , LCP2 , LCP3 : CTP ;
             LSP : STP ;
             LKIND : IDKIND ;
             LLC , LSIZE : ADDRRANGE ;
             COUNT : INTEGER ;
             OLDLEV : 0 .. MAXLEVEL ;
             OLDTOP : DISPRANGE ;
             LCS : ADDRRANGE ;
             TEST : BOOLEAN ;

         begin (* PARAMETERLIST *)
           LCP1 := NIL ;
           if not ( SY in FSY + [ LPARENT ] ) then
             begin
               ERROR ( 7 ) ;
               SKIP ( FSYS + FSY + [ LPARENT ] )
             end (* then *) ;
           if SY = LPARENT then
             begin
               if FORW then
                 ERROR ( 119 ) ;
               INSYMBOL ;
               if not ( SY in [ IDENT , VARSY , PROCSY , FUNCSY ] )
               then
                 begin
                   ERROR ( 7 ) ;
                   SKIP ( FSYS + [ IDENT , RPARENT ] )
                 end (* then *) ;
               while SY in [ IDENT , VARSY , PROCSY , FUNCSY ] do
                 begin
                   if SY = PROCSY then
                     begin
                       INSYMBOL ;
                       LCP := NIL ;
                       if SY = IDENT then
                         begin
                           NEW ( LCP , PROC , DECLARED , FORMAL ) ;
                           ININAM ( LCP ) ;
                           with LCP -> do
                             begin
                               STRASSVF ( NAME , ID ) ;
                               IDTYPE := NIL ;
                               NEXT := LCP1 ;
                               PFLEV := LEVEL

           (********************************)
           (*beware of parameter procedures*)
           (********************************)

                                        ;
                               KLASS := PROC ;
                               PFDECKIND := DECLARED ;
                               PFKIND := FORMAL ;
                               PFADDR := LC ;
                               KEEP := TRUE
                             end (* with *) ;
                           ENTERID ( LCP ) ;
                           LCP1 := LCP ;
                           ALIGN ( PARMPTR , LC ) ;
                           LC := LC + PTRSIZE * 2 ; (* mp and addr *)
                           INSYMBOL
                         end (* then *)
                       else
                         ERROR ( 2 ) ;
                       OLDLEV := LEVEL ;
                       OLDTOP := TOP ;
                       PUSHLVL ( FALSE , LCP ) ;
                       LCS := LC ;
                       PARAMETERLIST ( [ SEMICOLON , RPARENT ] , LCP2 )
                                       ;
                       LC := LCS ;
                       if LCP <> NIL then
                         LCP -> . PFLIST := LCP2 ;
                       if not ( SY in FSYS + [ SEMICOLON , RPARENT ] )
                       then
                         begin
                           ERROR ( 7 ) ;
                           SKIP ( FSYS + [ SEMICOLON , RPARENT ] )
                         end (* then *) ;
                       LEVEL := OLDLEV ;
                       PUTDSPS ( OLDTOP ) ;
                       TOP := OLDTOP
                     end (* then *)
                   else
                     begin
                       if SY = FUNCSY then
                         begin
                           LCP2 := NIL ;
                           INSYMBOL ;
                           if SY = IDENT then
                             begin
                               NEW ( LCP , FUNC , DECLARED , FORMAL ) ;
                               ININAM ( LCP ) ;
                               with LCP -> do
                                 begin
                                   STRASSVF ( NAME , ID ) ;
                                   IDTYPE := NIL ;
                                   NEXT := LCP1 ;
                                   PFLEV := LEVEL

           (********************)
           (*beware param funcs*)
           (********************)

                                            ;
                                   KLASS := FUNC ;
                                   PFDECKIND := DECLARED ;
                                   PFKIND := FORMAL ;
                                   PFADDR := LC ;
                                   KEEP := TRUE
                                 end (* with *) ;
                               ENTERID ( LCP ) ;
                               LCP1 := LCP ;
                               ALIGN ( PARMPTR , LC ) ;
                               LC := LC + PTRSIZE * 2 ;

           (***************)
           (* mp and addr *)
           (***************)

                               INSYMBOL ;
                             end (* then *)
                           else
                             ERROR ( 2 ) ;
                           OLDLEV := LEVEL ;
                           OLDTOP := TOP ;
                           PUSHLVL ( FALSE , LCP ) ;
                           LCS := LC ;
                           PARAMETERLIST ( [ COLON , SEMICOLON ,
                                           RPARENT ] , LCP2 ) ;
                           LC := LCS ;
                           if LCP <> NIL then
                             LCP -> . PFLIST := LCP2 ;
                           if not ( SY in FSYS + [ COLON ] ) then
                             begin
                               ERROR ( 7 ) ;
                               SKIP ( FSYS + [ COMMA , SEMICOLON ,
                                      RPARENT ] )
                             end (* then *) ;
                           if SY = COLON then
                             begin
                               INSYMBOL ;
                               if SY = IDENT then
                                 begin
                                   SEARCHID ( [ TYPES ] , LCP2 ) ;
                                   LSP := LCP2 -> . IDTYPE ;
                                   LCP -> . IDTYPE := LSP ;
                                   if LSP <> NIL then
                                     if not ( LSP -> . FORM in [ SCALAR
                                     , SUBRANGE , POINTER ] ) then
                                       begin
                                         ERROR ( 120 ) ;
                                         LSP := NIL
                                       end (* then *) ;
                                   INSYMBOL
                                 end (* then *)
                               else
                                 ERROR ( 2 ) ;
                               if not ( SY in FSYS + [ SEMICOLON ,
                               RPARENT ] ) then
                                 begin
                                   ERROR ( 7 ) ;
                                   SKIP ( FSYS + [ SEMICOLON , RPARENT
                                          ] )
                                 end (* then *)
                             end (* then *)
                           else
                             ERROR ( 5 ) ;
                           LEVEL := OLDLEV ;
                           PUTDSPS ( OLDTOP ) ;
                           TOP := OLDTOP
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
                           LCP2 := NIL ;
                           COUNT := 0 ;
                           repeat
                             if SY = IDENT then
                               begin
                                 NEW ( LCP , VARS ) ;
                                 ININAM ( LCP ) ;
                                 with LCP -> do
                                   begin
                                     STRASSVF ( NAME , ID ) ;
                                     IDTYPE := NIL ;
                                     KLASS := VARS ;
                                     VKIND := LKIND ;
                                     NEXT := LCP2 ;
                                     VLEV := LEVEL ;
                                     KEEP := TRUE
                                   end (* with *) ;
                                 ENTERID ( LCP ) ;
                                 LCP2 := LCP ;
                                 COUNT := COUNT + 1 ;
                                 INSYMBOL ;
                               end (* then *) ;
                             if not ( SY in [ COMMA , COLON ] + FSYS )
                             then
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
                                   SEARCHID ( [ TYPES ] , LCP ) ;
                                   LSP := LCP -> . IDTYPE ;
                                   LSIZE := PTRSIZE ;
                                   if LSP <> NIL then
                                     if LKIND = ACTUAL then
                                       if LSP -> . FORM <= POWER then
                                         LSIZE := LSP -> . SIZE
                                       else
                                         if LSP -> . FORM = FILES then
                                           ERROR ( 121 ) ;
                                   ALIGN ( PARMPTR , LSIZE ) ;
                                   LCP3 := LCP2 ;
                                   ALIGN ( PARMPTR , LC ) ;
                                   LC := LC + COUNT * LSIZE ;
                                   LLC := LC ;
                                   while LCP2 <> NIL do
                                     begin
                                       LCP := LCP2 ;
                                       with LCP2 -> do
                                         begin
                                           IDTYPE := LSP ;
                                           LLC := LLC - LSIZE ;
                                           VADDR := LLC ;
                                         end (* with *) ;
                                       LCP2 := LCP2 -> . NEXT
                                     end (* while *) ;
                                   LCP -> . NEXT := LCP1 ;
                                   LCP1 := LCP3 ;
                                   INSYMBOL
                                 end (* then *)
                               else
                                 ERROR ( 2 ) ;
                               if not ( SY in FSYS + [ SEMICOLON ,
                               RPARENT ] ) then
                                 begin
                                   ERROR ( 7 ) ;
                                   SKIP ( FSYS + [ SEMICOLON , RPARENT
                                          ] )
                                 end (* then *)
                             end (* then *)
                           else
                             ERROR ( 5 ) ;
                         end (* else *) ;
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
                     end (* then *)
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
               LCP3 := NIL ;

           (*****************************************************************)
           (*reverse pointers and reserve local cells for copies of multiple*)
           (*             values                                            *)
           (*****************************************************************)

               while LCP1 <> NIL do
                 with LCP1 -> do
                   begin
                     LCP2 := NEXT ;
                     NEXT := LCP3 ;
                     if KLASS = VARS then
                       if IDTYPE <> NIL then
                         if ( VKIND = ACTUAL ) and ( IDTYPE -> . FORM >
                         POWER ) then
                           begin
                             ALIGN ( IDTYPE , LC ) ;
                             VADDR := LC ;
                             LC := LC + IDTYPE -> . SIZE ;
                           end (* then *) ;
                     LCP3 := LCP1 ;
                     LCP1 := LCP2
                   end (* with *) ;
               FPAR := LCP3
             end (* then *)
           else
             FPAR := NIL
         end (* PARAMETERLIST *) ;


      begin (* PROCDECLARATION *)
        LLC := LC ;
        LC := LCAFTERMARKSTACK ;
        FORW := FALSE ;
        if SY = IDENT then
          begin
            SEARCHSECTION ( DISPLAY [ TOP ] . FNAME , LCP ) ;

        (**********************)
        (*decide whether forw.*)
        (**********************)

            if LCP <> NIL then
              begin
                if LCP -> . KLASS = PROC then
                  FORW := LCP -> . FORWDECL and ( FSY = PROCSY ) and (
                          LCP -> . PFKIND = ACTUAL )
                else
                  if LCP -> . KLASS = FUNC then
                    FORW := LCP -> . FORWDECL and ( FSY = FUNCSY ) and
                            ( LCP -> . PFKIND = ACTUAL )
                  else
                    FORW := FALSE ;
                if not FORW then
                  ERROR ( 160 )
              end (* then *) ;
            if not FORW then
              begin
                if FSY = PROCSY then
                  NEW ( LCP , PROC , DECLARED , ACTUAL )
                else
                  NEW ( LCP , FUNC , DECLARED , ACTUAL ) ;
                ININAM ( LCP ) ;
                with LCP -> do
                  begin
                    STRASSVF ( NAME , ID ) ;
                    IDTYPE := NIL ;
                    EXTERNL := FALSE ;
                    PFLEV := LEVEL ;
                    GENLABEL ( LBNAME ) ;
                    PFDECKIND := DECLARED ;
                    PFKIND := ACTUAL ;
                    PFNAME := LBNAME ;
                    if FSY = PROCSY then
                      KLASS := PROC
                    else
                      KLASS := FUNC
                  end (* with *) ;
                ENTERID ( LCP )
              end (* then *)
            else
              begin
                LCP1 := LCP -> . PFLIST ;
                while LCP1 <> NIL do
                  begin
                    with LCP1 -> do
                      if KLASS = VARS then
                        if IDTYPE <> NIL then
                          begin
                            LCM := VADDR + IDTYPE -> . SIZE ;
                            if LCM > LC then
                              LC := LCM
                          end (* then *) ;
                    LCP1 := LCP1 -> . NEXT
                  end (* while *)
              end (* else *) ;
            INSYMBOL
          end (* then *)
        else
          begin
            ERROR ( 2 ) ;
            LCP := UFCTPTR
          end (* else *) ;
        OLDLEV := LEVEL ;
        OLDTOP := TOP ;
        PUSHLVL ( FORW , LCP ) ;
        if FSY = PROCSY then
          begin
            PARAMETERLIST ( [ SEMICOLON ] , LCP1 ) ;
            if not FORW then
              LCP -> . PFLIST := LCP1
          end (* then *)
        else
          begin
            PARAMETERLIST ( [ SEMICOLON , COLON ] , LCP1 ) ;
            if not FORW then
              LCP -> . PFLIST := LCP1 ;
            if SY = COLON then
              begin
                INSYMBOL ;
                if SY = IDENT then
                  begin
                    if FORW then
                      ERROR ( 122 ) ;
                    SEARCHID ( [ TYPES ] , LCP1 ) ;
                    LSP := LCP1 -> . IDTYPE ;
                    LCP -> . IDTYPE := LSP ;
                    if LSP <> NIL then
                      if not ( LSP -> . FORM in [ SCALAR , SUBRANGE ,
                      POINTER ] ) then
                        begin
                          ERROR ( 120 ) ;
                          LCP -> . IDTYPE := NIL
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
              if not FORW then
                ERROR ( 123 )
          end (* else *) ;
        if SY = SEMICOLON then
          INSYMBOL
        else
          ERROR ( 14 ) ;
        if ( SY = IDENT ) and STREQURI ( 'forward  ' , ID ) then
          begin
            if FORW then
              ERROR ( 161 )
            else
              LCP -> . FORWDECL := TRUE ;
            INSYMBOL ;
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
            LCP -> . FORWDECL := FALSE ;

        (****************)
        (* mark(markp); *)
        (****************)

            repeat
              BLOCK ( FSYS , SEMICOLON , LCP ) ;
              if SY = SEMICOLON then
                begin
                  if PRTABLES then
                    PRINTTABLES ( FALSE ) ;
                  INSYMBOL ;
                  if not ( SY in [ BEGINSY , PROCSY , FUNCSY ] ) then
                    begin
                      ERROR ( 6 ) ;
                      SKIP ( FSYS )
                    end (* then *)
                end (* then *)
              else
                ERROR ( 14 )
            until ( SY in [ BEGINSY , PROCSY , FUNCSY ] ) or EOF (
            INPUT ) ;

        (****************************************)
        (* release(markp);                      *)
        (* return local entries on runtime heap *)
        (****************************************)

          end (* else *) ;
        LEVEL := OLDLEV ;
        PUTDSPS ( OLDTOP ) ;
        TOP := OLDTOP ;
        LC := LLC ;
      end (* PROCDECLARATION *) ;


   procedure BODY ( FSYS : SETOFSYS ) ;

      const CSTOCCMAX = 4000 ;
            CIXMAX = 10000 ;

      type OPRANGE = 0 .. MAXINS ;

      var LLCP : CTP ;
          SAVEID : IDSTR ;
          CSTPTR : array [ 1 .. CSTOCCMAX ] of CSP ;
          CSTPTRIX : 0 .. CSTOCCMAX ;

          (************************************************************************)
          (*allows referencing of noninteger constants by an index                *)
          (*           (instead of a pointer), which can be stored in the p2-field*)
          (*           of the instruction record until writeout.                  *)
          (*           --> procedure load, procedure writeout                     *)
          (************************************************************************)

          ENTNAME , SEGSIZE : INTEGER ;
          STACKTOP , TOPNEW , TOPMAX : INTEGER ;
          LCMAX , LLC1 : ADDRRANGE ;
          LCP : CTP ;
          LLP : LBP ;
          FP : EXTFILEP ;
          TEST : BOOLEAN ;

          (***********************)
          (* add statement level *)
          (***********************)



      procedure ADDLVL ;

         begin (* ADDLVL *)
           STALVL := STALVL + 1
         end (* ADDLVL *) ;

         (**************************)
         (* remove statement level *)
         (**************************)



      procedure SUBLVL ;

         var LLP : LBP ;

         begin (* SUBLVL *)
           STALVL := STALVL - 1 ;

           (******************************************************************)
           (* traverse label list for current block and remove any label from*)
           (*           active status whose statement block has closed       *)
           (******************************************************************)

           LLP := DISPLAY [ TOP ] . FLABEL ;
           while LLP <> NIL do
             with LLP -> do
               begin
                 if SLEVEL > STALVL then
                   BACT := FALSE ;
                 LLP := NEXTLAB (* link next *)
               end (* with *)
         end (* SUBLVL *) ;


      procedure MES ( I : INTEGER ) ;

         begin (* MES *)
           TOPNEW := TOPNEW + CDX [ I ] * MAXSTACK ;
           if TOPNEW > TOPMAX then
             TOPMAX := TOPNEW
         end (* MES *) ;


      procedure PUTIC ;

         begin (* PUTIC *)
           if IC MOD 10 = 0 then
             WRITELN ( PRR , 'i' , IC : 5 )
         end (* PUTIC *) ;


      procedure GEN0 ( FOP : OPRANGE ) ;

         begin (* GEN0 *)
           if PRCODE then
             begin
               PUTIC ;
               WRITELN ( PRR , MN [ FOP ] : 4 )
             end (* then *) ;
           IC := IC + 1 ;
           MES ( FOP )
         end (* GEN0 *) ;


      procedure GEN1 ( FOP : OPRANGE ; FP2 : INTEGER ) ;

         var K , J : INTEGER ;
             P : STRVSP ;

         begin (* GEN1 *)
           if PRCODE then
             begin
               PUTIC ;
               WRITE ( PRR , MN [ FOP ] : 4 ) ;
               if FOP = 30 then
                 begin
                   WRITELN ( PRR , SNA [ FP2 ] : 12 ) ;
                   TOPNEW := TOPNEW + PDX [ FP2 ] * MAXSTACK ;
                   if TOPNEW > TOPMAX then
                     TOPMAX := TOPNEW
                 end (* then *)
               else
                 begin
                   if FOP = 38 then
                     begin
                       with CSTPTR [ FP2 ] -> do
                         begin
                           P := SVAL ;
                           J := 1 ;
                           WRITE ( PRR , ' ' , SLGTH : 4 , ' ''' ) ;
                           for K := 1 to LENPV ( P ) do
                             begin
                               if P -> . STR [ J ] = '''' then
                                 WRITE ( PRR , '''''' )
                               else
                                 WRITE ( PRR , P -> . STR [ J ] : 1 ) ;
                               J := J + 1 ;
                               if J > VARSQT then
                                 begin
                                   P := P -> . NEXT ;
                                   J := 1
                                 end (* then *)
                             end (* for *)
                         end (* with *) ;
                       WRITELN ( PRR , '''' )
                     end (* then *)
                   else
                     if FOP = 42 then
                       WRITELN ( PRR , CHR ( FP2 ) )
                     else
                       if FOP = 67 then
                         WRITELN ( PRR , FP2 : 4 )
                       else
                         WRITELN ( PRR , FP2 : 12 ) ;
                   MES ( FOP )
                 end (* else *)
             end (* then *) ;
           IC := IC + 1
         end (* GEN1 *) ;


      procedure GEN2 ( FOP : OPRANGE ; FP1 , FP2 : INTEGER ) ;

         var K : INTEGER ;

         begin (* GEN2 *)
           if PRCODE then
             begin
               PUTIC ;
               WRITE ( PRR , MN [ FOP ] : 4 ) ;
               case FOP of
                 45 , 50 , 54 , 56 , 74 , 62 , 63 :
                   WRITELN ( PRR , ' ' , FP1 : 3 , FP2 : 8 ) ;
                 47 , 48 , 49 , 52 , 53 , 55 :
                   begin
                     WRITE ( PRR , CHR ( FP1 ) ) ;
                     if CHR ( FP1 ) = 'm' then
                       WRITE ( PRR , ' ' , FP2 : 11 ) ;
                     WRITELN ( PRR )
                   end (* tag/ca *) ;
                 51 : case FP1 of
                        1 : WRITELN ( PRR , 'i ' , FP2 ) ;
                        2 : begin
                              WRITE ( PRR , 'r ' ) ;
                              with CSTPTR [ FP2 ] -> do
                                WRITEV ( PRR , RVAL , LENPV ( RVAL ) )
                                         ;
                              WRITELN ( PRR )
                            end (* tag/ca *) ;
                        3 : WRITELN ( PRR , 'b ' , FP2 ) ;
                        4 : WRITELN ( PRR , 'n' ) ;
                        6 : WRITELN ( PRR , 'c ''' : 3 , CHR ( FP2 ) ,
                                      '''' ) ;
                        5 : begin
                              WRITE ( PRR , '(' ) ;
                              with CSTPTR [ FP2 ] -> do
                                for K := SETLOW to SETHIGH do

           (*******************************)
           (* increased for testing [sam] *)
           (*******************************)

                                  if K in PVAL then
                                    WRITE ( PRR , K : 7  (*3*) ) ;
                              WRITELN ( PRR , ')' )
                            end (* tag/ca *)
                      end (* case *) ;
               end (* case *) ;
             end (* then *) ;
           IC := IC + 1 ;
           MES ( FOP )
         end (* GEN2 *) ;


      procedure GENTYPINDICATOR ( FSP : STP ) ;

         begin (* GENTYPINDICATOR *)
           if FSP <> NIL then
             with FSP -> do
               case FORM of
                 SCALAR :
                   if FSP = INTPTR then
                     WRITE ( PRR , 'i' )
                   else
                     if FSP = BOOLPTR then
                       WRITE ( PRR , 'b' )
                     else
                       if FSP = CHARPTR then
                         WRITE ( PRR , 'c' )
                       else
                         if SCALKIND = DECLARED then
                           WRITE ( PRR , 'i' )
                         else
                           WRITE ( PRR , 'r' ) ;
                 SUBRANGE :
                   GENTYPINDICATOR ( RANGETYPE ) ;
                 POINTER :
                   WRITE ( PRR , 'a' ) ;
                 POWER : WRITE ( PRR , 's' ) ;
                 RECORDS , ARRAYS :
                   WRITE ( PRR , 'm' ) ;
                 FILES : WRITE ( PRR , 'a' ) ;
                 TAGFLD , VARIANT :
                   ERROR ( 401 )
               end (* case *)
         end (* GENTYPINDICATOR *) ;


      procedure GEN0T ( FOP : OPRANGE ; FSP : STP ) ;

         begin (* GEN0T *)
           if PRCODE then
             begin
               PUTIC ;
               WRITE ( PRR , MN [ FOP ] : 4 ) ;
               GENTYPINDICATOR ( FSP ) ;
               WRITELN ( PRR ) ;
             end (* then *) ;
           IC := IC + 1 ;
           MES ( FOP )
         end (* GEN0T *) ;


      procedure GEN1T ( FOP : OPRANGE ; FP2 : INTEGER ; FSP : STP ) ;

         begin (* GEN1T *)
           if PRCODE then
             begin
               PUTIC ;
               WRITE ( PRR , MN [ FOP ] : 4 ) ;
               GENTYPINDICATOR ( FSP ) ;
               WRITELN ( PRR , ' ' , FP2 : 11 )
             end (* then *) ;
           IC := IC + 1 ;
           MES ( FOP )
         end (* GEN1T *) ;


      procedure GEN2T ( FOP : OPRANGE ; FP1 , FP2 : INTEGER ; FSP : STP
                      ) ;

         begin (* GEN2T *)
           if PRCODE then
             begin
               PUTIC ;
               WRITE ( PRR , MN [ FOP ] : 4 ) ;
               GENTYPINDICATOR ( FSP ) ;
               WRITELN ( PRR , ' ' , FP1 : 3 + 5 * ORD ( ABS ( FP1 ) >
                         99 ) , FP2 : 11 ) ;
             end (* then *) ;
           IC := IC + 1 ;
           MES ( FOP )
         end (* GEN2T *) ;


      procedure LOAD ;

         begin (* LOAD *)
           with GATTR do
             if TYPTR <> NIL then
               begin
                 case KIND of
                   CST : if ( TYPTR -> . FORM = SCALAR ) and ( TYPTR <>
                         REALPTR ) then
                           if TYPTR = BOOLPTR then
                             GEN2 ( 51  (*ldc*) , 3 , CVAL . IVAL )
                           else
                             if TYPTR = CHARPTR then
                               GEN2 ( 51  (*ldc*) , 6 , CVAL . IVAL )
                             else
                               GEN2 ( 51  (*ldc*) , 1 , CVAL . IVAL )
                         else
                           if TYPTR = NILPTR then
                             GEN2 ( 51  (*ldc*) , 4 , 0 )
                           else
                             if CSTPTRIX >= CSTOCCMAX then
                               ERROR ( 254 )
                             else
                               begin
                                 CSTPTRIX := CSTPTRIX + 1 ;
                                 CSTPTR [ CSTPTRIX ] := CVAL . VALP ;
                                 if TYPTR = REALPTR then
                                   GEN2 ( 51  (*ldc*) , 2 , CSTPTRIX )
                                 else
                                   GEN2 ( 51  (*ldc*) , 5 , CSTPTRIX )
                               end (* else *) ;
                   VARBL : case ACCESS of
                             DRCT : if VLEVEL <= 1 then
                                      GEN1T ( 39  (*ldo*) , DPLMT ,
                                              TYPTR )
                                    else
                                      GEN2T ( 54  (*lod*) , LEVEL -
                                              VLEVEL , DPLMT , TYPTR )
                                              ;
                             INDRCT :
                               GEN1T ( 35  (*ind*) , IDPLMT , TYPTR ) ;
                             INXD : ERROR ( 400 )
                           end (* case *) ;
                   EXPR :
                 end (* case *) ;
                 KIND := EXPR
               end (* then *)
         end (* LOAD *) ;


      procedure STORE ( var FATTR : ATTR ) ;

         begin (* STORE *)
           with FATTR do
             if TYPTR <> NIL then
               case ACCESS of
                 DRCT : if VLEVEL <= 1 then
                          GEN1T ( 43  (*sro*) , DPLMT , TYPTR )
                        else
                          GEN2T ( 56  (*str*) , LEVEL - VLEVEL , DPLMT
                                  , TYPTR ) ;
                 INDRCT :
                   if IDPLMT <> 0 then
                     ERROR ( 400 )
                   else
                     GEN0T ( 26  (*sto*) , TYPTR ) ;
                 INXD : ERROR ( 400 )
               end (* case *)
         end (* STORE *) ;


      procedure LOADADDRESS ;

         begin (* LOADADDRESS *)
           with GATTR do
             if TYPTR <> NIL then
               begin
                 case KIND of
                   CST : if STRING ( TYPTR ) then
                           if CSTPTRIX >= CSTOCCMAX then
                             ERROR ( 254 )
                           else
                             begin
                               CSTPTRIX := CSTPTRIX + 1 ;
                               CSTPTR [ CSTPTRIX ] := CVAL . VALP ;
                               GEN1 ( 38  (*lca*) , CSTPTRIX )
                             end (* else *)
                         else
                           ERROR ( 400 ) ;
                   VARBL : case ACCESS of
                             DRCT : if VLEVEL <= 1 then
                                      GEN1 ( 37  (*lao*) , DPLMT )
                                    else
                                      GEN2 ( 50  (*lda*) , LEVEL -
                                             VLEVEL , DPLMT ) ;
                             INDRCT :
                               if IDPLMT <> 0 then
                                 GEN1T ( 34  (*inc*) , IDPLMT , NILPTR
                                         ) ;
                             INXD : ERROR ( 400 )
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
               WRITELN ( PRR , MN [ 33 ] : 4 , ' l' : 8 , FADDR : 4 )
             end (* then *) ;
           IC := IC + 1 ;
           MES ( 33 )
         end (* GENFJP *) ;


      procedure GENUJPXJP ( FOP : OPRANGE ; FP2 : INTEGER ) ;

         begin (* GENUJPXJP *)
           if PRCODE then
             begin
               PUTIC ;
               WRITELN ( PRR , MN [ FOP ] : 4 , ' l' : 8 , FP2 : 4 )
             end (* then *) ;
           IC := IC + 1 ;
           MES ( FOP )
         end (* GENUJPXJP *) ;


      procedure GENIPJ ( FOP : OPRANGE ; FP1 , FP2 : INTEGER ) ;

         begin (* GENIPJ *)
           if PRCODE then
             begin
               PUTIC ;
               WRITELN ( PRR , MN [ FOP ] : 4 , FP1 : 4 , ' l' : 8 ,
                         FP2 : 4 )
             end (* then *) ;
           IC := IC + 1 ;
           MES ( FOP )
         end (* GENIPJ *) ;


      procedure GENCUPENT ( FOP : OPRANGE ; FP1 , FP2 : INTEGER ) ;

         begin (* GENCUPENT *)
           if PRCODE then
             begin
               PUTIC ;
               if FOP = 32 then
                 begin            (* create ents or ente instructions *)
                   if FP1 = 1 then
                     WRITELN ( PRR , MN [ FOP ] : 4 , 's' , 'l' : 8 ,
                               FP2 : 4 )
                   else
                     WRITELN ( PRR , MN [ FOP ] : 4 , 'e' , 'l' : 8 ,
                               FP2 : 4 )
                 end (* then *)
               else
                 WRITELN ( PRR , MN [ FOP ] : 4 , FP1 : 4 , 'l' : 4 ,
                           FP2 : 4 )
             end (* then *) ;
           IC := IC + 1 ;
           MES ( FOP )
         end (* GENCUPENT *) ;


      procedure GENLPA ( FP1 , FP2 : INTEGER ) ;

         begin (* GENLPA *)
           if PRCODE then
             begin
               PUTIC ;
               WRITELN ( PRR , MN [ 68 ] : 4 , FP2 : 4 , 'l' : 4 , FP1
                         : 4 )
             end (* then *) ;
           IC := IC + 1 ;
           MES ( 68 )
         end (* GENLPA *) ;


      procedure CHECKBNDS ( FSP : STP ) ;

         var LMIN , LMAX : INTEGER ;

         begin (* CHECKBNDS *)
           if FSP <> NIL then
             if FSP <> INTPTR then
               if FSP <> REALPTR then
                 if FSP -> . FORM <= SUBRANGE then
                   begin
                     GETBOUNDS ( FSP , LMIN , LMAX ) ;
                     GEN2T ( 45  (*chk*) , LMIN , LMAX , FSP )
                   end (* then *)
         end (* CHECKBNDS *) ;


      procedure PUTLABEL ( LABNAME : INTEGER ) ;

         begin (* PUTLABEL *)
           if PRCODE then
             WRITELN ( PRR , 'l' , LABNAME : 4 )
         end (* PUTLABEL *) ;


      procedure STATEMENT ( FSYS : SETOFSYS ) ;

         var LCP : CTP ;
             LLP : LBP ;


         procedure EXPRESSION ( FSYS : SETOFSYS ) ;

            FORWARD ;


         procedure SELECTOR ( FSYS : SETOFSYS ; FCP : CTP ) ;

            var LATTR : ATTR ;
                LCP : CTP ;
                LSIZE : ADDRRANGE ;
                LMIN , LMAX : INTEGER ;


            function SCHBLK ( FCP : CTP ) : BOOLEAN ;

               var I : DISPRANGE ;
                   F : BOOLEAN ;

               begin (* SCHBLK *)
                 F := FALSE ;
                 for I := LEVEL DOWNTO 2 do
                   if DISPLAY [ I ] . BNAME = FCP then
                     F := TRUE ;
                 SCHBLK := F
               end (* SCHBLK *) ;


            begin (* SELECTOR *)
              with FCP -> , GATTR do
                begin
                  TYPTR := IDTYPE ;
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
                               GEN2T ( 54  (*lod*) , LEVEL - VLEV ,
                                       VADDR , NILPTR ) ;
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
                                  if LEVEL = 1 then
                                    GEN1T ( 39  (*ldo*) , VDSPL ,
                                            NILPTR )
                                  else
                                    GEN2T ( 54  (*lod*) , 0 , VDSPL ,
                                            NILPTR ) ;
                                  ACCESS := INDRCT ;
                                  IDPLMT := FLDADDR
                                end (* else *) ;
                    FUNC : if PFDECKIND = STANDARD then
                             begin
                               ERROR ( 150 ) ;
                               TYPTR := NIL
                             end (* then *)
                           else
                             begin
                               if PFKIND = FORMAL then
                                 ERROR ( 151 )
                               else
                                 if not SCHBLK ( FCP ) then
                                   ERROR ( 192 ) ;
                               begin
                                 ACCESS := DRCT ;
                                 VLEVEL := PFLEV + 1 ;
                                 DPLMT := 0

              (***********************************)
              (*impl. relat. addr. of fct. result*)
              (***********************************)

                               end
                             end (* else *)
                  end (* case *)
                end (* with *) ;
              if not ( SY in SELECTSYS + FSYS ) then
                begin
                  ERROR ( 59 ) ;
                  SKIP ( SELECTSYS + FSYS )
                end (* then *) ;
              while SY in SELECTSYS do
                begin

              (***)
              (*[*)
              (***)

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
                        EXPRESSION ( FSYS + [ COMMA , RBRACK ] ) ;
                        LOAD ;
                        if GATTR . TYPTR <> NIL then
                          if GATTR . TYPTR -> . FORM <> SCALAR then
                            ERROR ( 113 )
                          else
                            if not COMPTYPES ( GATTR . TYPTR , INTPTR )
                            then
                              GEN0T ( 58  (*ord*) , GATTR . TYPTR ) ;
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
                                        GEN2T ( 45  (*chk*) , LMIN ,
                                                LMAX , INTPTR ) ;
                                      if LMIN > 0 then
                                        GEN1T ( 31  (*dec*) , LMIN ,
                                                INTPTR )
                                      else
                                        if LMIN < 0 then
                                          GEN1T ( 34  (*inc*) , - LMIN
                                                  , INTPTR ) ;

              (*************************)
              (*or simply gen1(31,lmin)*)
              (*************************)

                                    end (* then *)
                                end (* then *)
                              else
                                ERROR ( 139 ) ;
                              with GATTR do
                                begin
                                  TYPTR := AELTYPE ;
                                  KIND := VARBL ;
                                  ACCESS := INDRCT ;
                                  IDPLMT := 0
                                end (* with *) ;
                              if GATTR . TYPTR <> NIL then
                                begin
                                  LSIZE := GATTR . TYPTR -> . SIZE ;
                                  ALIGN ( GATTR . TYPTR , LSIZE ) ;
                                  GEN1 ( 36  (*ixa*) , LSIZE )
                                end (* then *)
                            end (* with *)
                      until SY <> COMMA ;
                      if SY = RBRACK then
                        INSYMBOL
                      else
                        ERROR ( 12 )
                    end (* then *)
                  else

              (***)
              (*.*)
              (***)

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
                                    SEARCHSECTION ( TYPTR -> . FSTFLD ,
                                                   LCP ) ;
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
                                            INXD : ERROR ( 400 )
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

              (***)
              (*^*)
              (***)

                      begin
                        if GATTR . TYPTR <> NIL then
                          with GATTR , TYPTR -> do
                            if FORM = POINTER then
                              begin
                                LOAD ;
                                TYPTR := ELTYPE ;
                                if DEBUG then
                                  GEN2T ( 45  (*chk*) , 1 , MAXADDR ,
                                          NILPTR ) ;
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
                                  LOADADDRESS ;

              (*************************************)
              (* generate buffer validate for file *)
              (*************************************)

                                  if TYPTR = TEXTPTR then
                                    GEN0 ( 65  (*fbv*) )
                                  else
                                    begin
                                      GEN2 ( 51  (*ldc*) , 1 , FILTYPE
                                             -> . SIZE ) ;
                                      GEN0 ( 70  (*fvb*) )
                                    end (* else *) ;

              (****************)
              (* index buffer *)
              (****************)

                                  GEN1T ( 34  (*inc*) , FILEIDSIZE ,
                                          GATTR . TYPTR ) ;
                                  TYPTR := FILTYPE ;
                                end (* then *)
                              else
                                ERROR ( 141 ) ;
                        INSYMBOL
                      end (* else *) ;
                  if not ( SY in FSYS + SELECTSYS ) then
                    begin
                      ERROR ( 6 ) ;
                      SKIP ( FSYS + SELECTSYS )
                    end (* then *)
                end (* while *)
            end (* SELECTOR *) ;


         procedure CALL ( FSYS : SETOFSYS ; FCP : CTP ) ;

            var LKEY : 1 .. 18 ;


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


            procedure GETPUTRESETREWRITEPRocedure ;

               begin (* GETPUTRESETREWRITEPR *)
                 VARIABLE ( FSYS + [ RPARENT ] ) ;
                 LOADADDRESS ;
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM <> FILES then
                     ERROR ( 116 ) ;
                 if LKEY <= 2 then
                   begin
                     if GATTR . TYPTR = TEXTPTR then
                       GEN1 ( 30  (*csp*) , LKEY  (*get,put*) )
                     else
                       begin
                         if GATTR . TYPTR <> NIL then
                           GEN2 ( 51  (*ldc*) , 1 , GATTR . TYPTR -> .
                                  FILTYPE -> . SIZE ) ;
                         if LKEY = 1 then
                           GEN1 ( 30  (*csp*) , 38  (*gbf*) )
                         else
                           GEN1 ( 30  (*csp*) , 39  (*pbf*) )
                       end (* else *)
                   end (* then *)
                 else
                   if GATTR . TYPTR = TEXTPTR then
                     begin
                       if LKEY = 3 then
                         GEN1 ( 30  (*csp*) , 25  (*reset*) )
                       else
                         GEN1 ( 30  (*csp*) , 26  (*rewrite*) )
                     end (* then *)
                   else
                     begin
                       if LKEY = 3 then
                         GEN1 ( 30  (*csp*) , 36  (*reset*) )
                       else
                         GEN1 ( 30  (*csp*) , 37  (*rewrite*) )
                     end (* else *)
               end (* GETPUTRESETREWRITEPR *) ;


            procedure PAGEPROCEDURE ;

               var LLEV : LEVRANGE ;

               begin (* PAGEPROCEDURE *)
                 LLEV := 1 ;
                 if SY = LPARENT then
                   begin
                     INSYMBOL ;
                     VARIABLE ( FSYS + [ RPARENT ] ) ;
                     LOADADDRESS ;
                     if GATTR . TYPTR <> NIL then
                       if GATTR . TYPTR <> TEXTPTR then
                         ERROR ( 116 ) ;
                     if SY = RPARENT then
                       INSYMBOL
                     else
                       ERROR ( 4 )
                   end (* then *)
                 else
                   begin
                     if not OUTPUTHDF then
                       ERROR ( 176 ) ;
                     GEN2 ( 50  (*lda*) , LEVEL - OUTPUTPTR -> . VLEV ,
                            OUTPUTPTR -> . VADDR ) ;
                   end (* else *) ;
                 GEN1 ( 30  (*csp*) , 24  (*page*) )
               end (* PAGEPROCEDURE *) ;


            procedure READPROCEDURE ;

               var LSP : STP ;
                   TXT : BOOLEAN ; (* is a text file *)
                   DEFFIL : BOOLEAN ; (* default file was loaded *)
                   TEST : BOOLEAN ;

               begin (* READPROCEDURE *)
                 TXT := TRUE ;
                 DEFFIL := TRUE ;
                 if SY = LPARENT then
                   begin
                     INSYMBOL ;
                     VARIABLE ( FSYS + [ COMMA , RPARENT ] ) ;
                     LSP := GATTR . TYPTR ;
                     TEST := FALSE ;
                     if LSP <> NIL then
                       if LSP -> . FORM = FILES then
                         with GATTR , LSP -> do
                           begin
                             TXT := LSP = TEXTPTR ;
                             if not TXT and ( LKEY = 11 ) then
                               ERROR ( 116 ) ;
                             LOADADDRESS ;
                             DEFFIL := FALSE ;
                             if SY = RPARENT then
                               begin
                                 if LKEY = 5 then
                                   ERROR ( 116 ) ;
                                 TEST := TRUE
                               end (* then *)
                             else
                               if SY <> COMMA then
                                 begin
                                   ERROR ( 116 ) ;
                                   SKIP ( FSYS + [ COMMA , RPARENT ] )
                                 end (* then *) ;
                             if SY = COMMA then
                               begin
                                 INSYMBOL ;
                                 VARIABLE ( FSYS + [ COMMA , RPARENT ]
                                            )
                               end (* then *)
                             else
                               TEST := TRUE
                           end (* with *)
                       else
                         if not INPUTHDF then
                           ERROR ( 175 ) ;
                     if not TEST then
                       repeat
                         LOADADDRESS ;
                         if DEFFIL then
                           begin

                 (***********************************************************)
                 (* file was not loaded, we load and swap so that it ends up*)
                 (*                      on the bottom.                     *)
                 (***********************************************************)

                             GEN2 ( 50  (*lda*) , LEVEL - INPUTPTR -> .
                                    VLEV , INPUTPTR -> . VADDR ) ;
                             GEN1 ( 72  (*swp*) , PTRSIZE ) ;

                 (******************************)
                 (* note 2nd is always pointer *)
                 (******************************)

                             DEFFIL := FALSE
                           end (* then *) ;
                         if TXT then
                           begin
                             if GATTR . TYPTR <> NIL then
                               if GATTR . TYPTR -> . FORM <= SUBRANGE
                               then
                                 if COMPTYPES ( INTPTR , GATTR . TYPTR
                                 ) then
                                   GEN1 ( 30  (*csp*) , 3  (*rdi*) )
                                 else
                                   if COMPTYPES ( REALPTR , GATTR .
                                   TYPTR ) then
                                     GEN1 ( 30  (*csp*) , 4  (*rdr*) )
                                   else
                                     if COMPTYPES ( CHARPTR , GATTR .
                                     TYPTR ) then
                                       GEN1 ( 30  (*csp*) , 5  (*rdc*)
                                              )
                                     else
                                       ERROR ( 399 )
                               else
                                 ERROR ( 116 ) ;
                           end (* then *)
                         else
                           begin (* binary file *)
                             if not COMPTYPES ( GATTR . TYPTR , LSP ->
                             . FILTYPE ) then
                               ERROR ( 129 ) ;
                             GEN2 ( 51  (*ldc*) , 1 , LSP -> . FILTYPE
                                    -> . SIZE ) ;
                             GEN1 ( 30  (*csp*) , 35  (*rbf*) )
                           end (* else *) ;
                         TEST := SY <> COMMA ;
                         if not TEST then
                           begin
                             INSYMBOL ;
                             VARIABLE ( FSYS + [ COMMA , RPARENT ] )
                           end (* then *)
                       until TEST ;
                     if SY = RPARENT then
                       INSYMBOL
                     else
                       ERROR ( 4 )
                   end (* then *)
                 else
                   begin
                     if not INPUTHDF then
                       ERROR ( 175 ) ;
                     if LKEY = 5 then
                       ERROR ( 116 ) ;
                     GEN2 ( 50  (*lda*) , LEVEL - INPUTPTR -> . VLEV ,
                            INPUTPTR -> . VADDR )
                   end (* else *) ;
                 if LKEY = 11 then
                   GEN1 ( 30  (*csp*) , 21  (*rln*) ) ;

                 (**************************************)
                 (* remove the file pointer from stack *)
                 (**************************************)

                 GEN1 ( 71  (*dmp*) , PTRSIZE ) ;
               end (* READPROCEDURE *) ;


            procedure WRITEPROCEDURE ;

               var LSP , LSP1 : STP ;
                   DEFAULT , DEFAULT1 : BOOLEAN ;
                   LLKEY : 1 .. 15 ;
                   LEN : ADDRRANGE ;
                   TXT : BOOLEAN ; (* is a text file *)
                   DEFFIL : BOOLEAN ; (* default file was loaded *)
                   TEST : BOOLEAN ;

               begin (* WRITEPROCEDURE *)
                 LLKEY := LKEY ;
                 TXT := TRUE ;
                 DEFFIL := TRUE ;
                 if SY = LPARENT then
                   begin
                     INSYMBOL ;
                     EXPRESSION ( FSYS + [ COMMA , COLON , RPARENT ] )
                                  ;
                     LSP := GATTR . TYPTR ;
                     TEST := FALSE ;
                     if LSP <> NIL then
                       if LSP -> . FORM = FILES then
                         with GATTR , LSP -> do
                           begin
                             LSP1 := LSP ;
                             TXT := LSP = TEXTPTR ;
                             if not TXT and ( LKEY = 12 ) then
                               ERROR ( 116 ) ;
                             LOADADDRESS ;
                             DEFFIL := FALSE ;
                             if SY = RPARENT then
                               begin
                                 if LLKEY = 6 then
                                   ERROR ( 116 ) ;
                                 TEST := TRUE
                               end (* then *)
                             else
                               if SY <> COMMA then
                                 begin
                                   ERROR ( 116 ) ;
                                   SKIP ( FSYS + [ COMMA , RPARENT ] )
                                 end (* then *) ;
                             if SY = COMMA then
                               begin
                                 INSYMBOL ;
                                 EXPRESSION ( FSYS + [ COMMA , COLON ,
                                              RPARENT ] )
                               end (* then *)
                             else
                               TEST := TRUE
                           end (* with *)
                       else
                         if not OUTPUTHDF then
                           ERROR ( 176 ) ;
                     if not TEST then
                       repeat
                         LSP := GATTR . TYPTR ;
                         if LSP <> NIL then
                           if LSP -> . FORM <= SUBRANGE then
                             LOAD
                           else
                             LOADADDRESS ;
                         if DEFFIL then
                           begin

                 (***********************************************************)
                 (* file was not loaded, we load and swap so that it ends up*)
                 (*                  on the bottom.                         *)
                 (***********************************************************)

                             GEN2 ( 50  (*lda*) , LEVEL - OUTPUTPTR ->
                                    . VLEV , OUTPUTPTR -> . VADDR ) ;
                             if LSP <> NIL then
                               if LSP -> . FORM <= SUBRANGE then
                                 begin
                                   if LSP -> . SIZE < STACKELSIZE then
                                     GEN1 ( 72  (*swp*) , STACKELSIZE )

                 (********************************)
                 (* size of 2nd is minimum stack *)
                 (********************************)

                                   else
                                     GEN1 ( 72  (*swp*) , LSP -> . SIZE
                                            )

                 (**************************)
                 (* size of 2nd is operand *)
                 (**************************)

                                 end (* then *)
                               else
                                 GEN1 ( 72  (*swp*) , PTRSIZE ) ;

                 (**************************)
                 (* size of 2nd is pointer *)
                 (**************************)

                             DEFFIL := FALSE
                           end (* then *) ;
                         if TXT then
                           begin
                             if SY = COLON then
                               begin
                                 INSYMBOL ;
                                 EXPRESSION ( FSYS + [ COMMA , COLON ,
                                              RPARENT ] ) ;
                                 if GATTR . TYPTR <> NIL then
                                   if GATTR . TYPTR <> INTPTR then
                                     ERROR ( 116 ) ;
                                 LOAD ;
                                 DEFAULT := FALSE
                               end (* then *)
                             else
                               DEFAULT := TRUE ;
                             if SY = COLON then
                               begin
                                 INSYMBOL ;
                                 EXPRESSION ( FSYS + [ COMMA , RPARENT
                                              ] ) ;
                                 if GATTR . TYPTR <> NIL then
                                   if GATTR . TYPTR <> INTPTR then
                                     ERROR ( 116 ) ;
                                 if LSP <> REALPTR then
                                   ERROR ( 124 ) ;
                                 LOAD ;
                                 DEFAULT1 := FALSE
                               end (* then *)
                             else
                               DEFAULT1 := TRUE ;
                             if LSP = INTPTR then
                               begin
                                 if DEFAULT then
                                   GEN2 ( 51  (*ldc*) , 1 , INTDEFF ) ;
                                 GEN1 ( 30  (*csp*) , 6  (*wri*) )
                               end (* then *)
                             else
                               if LSP = REALPTR then
                                 begin
                                   if DEFAULT1 then
                                     begin
                                       if DEFAULT then
                                         GEN2 ( 51  (*ldc*) , 1 ,
                                                RELDEFF ) ;
                                       GEN1 ( 30  (*csp*) , 8  (*wrr*)
                                              )
                                     end (* then *)
                                   else
                                     begin
                                       if DEFAULT then
                                         GEN2 ( 51  (*ldc*) , 1 ,
                                                RELDEFF ) ;
                                       GEN1 ( 30  (*csp*) , 28

                 (*****)
                 (*wrf*)
                 (*****)

                                              )
                                     end (* else *)
                                 end (* then *)
                               else
                                 if LSP = CHARPTR then
                                   begin
                                     if DEFAULT then
                                       GEN2 ( 51  (*ldc*) , 1 , CHRDEFF
                                              ) ;
                                     GEN1 ( 30  (*csp*) , 9  (*wrc*) )
                                   end (* then *)
                                 else
                                   if LSP = BOOLPTR then
                                     begin
                                       if DEFAULT then
                                         GEN2 ( 51  (*ldc*) , 1 ,
                                                BOLDEFF ) ;
                                       GEN1 ( 30  (*csp*) , 27

                 (*****)
                 (*wrb*)
                 (*****)

                                              )
                                     end (* then *)
                                   else
                                     if LSP <> NIL then
                                       begin
                                         if LSP -> . FORM = SCALAR then
                                           ERROR ( 399 )
                                         else
                                           if STRING ( LSP ) then
                                             begin
                                               LEN := LSP -> . SIZE DIV
                                                   CHARMAX ;
                                               if DEFAULT then
                                                 GEN2 ( 51  (*ldc*) , 1
                                                   , LEN ) ;
                                               GEN2 ( 51  (*ldc*) , 1 ,
                                                   LEN ) ;
                                               GEN1 ( 30  (*csp*) , 10

                 (*****)
                 (*wrs*)
                 (*****)

                                                   )
                                             end (* then *)
                                           else
                                             ERROR ( 116 )
                                       end (* then *)
                           end (* then *)
                         else
                           begin (* binary file *)
                             if not COMPTYPES ( LSP1 -> . FILTYPE , LSP
                             ) then
                               ERROR ( 129 ) ;
                             if LSP = INTPTR then
                               GEN1 ( 30  (*csp*) , 31  (*wbi*) )
                             else
                               if LSP = REALPTR then
                                 GEN1 ( 30  (*csp*) , 32  (*wbr*) )
                               else
                                 if LSP = CHARPTR then
                                   GEN1 ( 30  (*csp*) , 33  (*wbc*) )
                                 else
                                   if LSP = BOOLPTR then
                                     GEN1 ( 30  (*csp*) , 34  (*wbb*) )
                                   else
                                     if LSP -> . FORM <= SUBRANGE then
                                       GEN1 ( 30  (*csp*) , 31

                 (*****)
                 (*wbi*)
                 (*****)

                                              )
                                     else
                                       if LSP <> NIL then
                                         begin
                                           GEN2 ( 51  (*ldc*) , 1 ,
                                                  LSP1 -> . FILTYPE ->
                                                  . SIZE ) ;
                                           GEN1 ( 30  (*csp*) , 30

                 (*****)
                 (*wbf*)
                 (*****)

                                                  )
                                         end (* then *)
                           end (* else *) ;
                         TEST := SY <> COMMA ;
                         if not TEST then
                           begin
                             INSYMBOL ;
                             EXPRESSION ( FSYS + [ COMMA , COLON ,
                                          RPARENT ] )
                           end (* then *)
                       until TEST ;
                     if SY = RPARENT then
                       INSYMBOL
                     else
                       ERROR ( 4 )
                   end (* then *)
                 else
                   begin
                     if not OUTPUTHDF then
                       ERROR ( 176 ) ;
                     if LKEY = 6 then
                       ERROR ( 116 ) ;
                     GEN2 ( 50  (*lda*) , LEVEL - OUTPUTPTR -> . VLEV ,
                            OUTPUTPTR -> . VADDR )
                   end (* else *) ;
                 if LLKEY = 12 then (*writeln*)
                   GEN1 ( 30  (*csp*) , 22  (*wln*) ) ;

                 (**************************************)
                 (* remove the file pointer from stack *)
                 (**************************************)

                 GEN1 ( 71  (*dmp*) , PTRSIZE ) ;
               end (* WRITEPROCEDURE *) ;


            procedure PACKPROCEDURE ;

               var LSP , LSP1 : STP ;
                   LB , BS : INTEGER ;
                   LATTR : ATTR ;

               begin (* PACKPROCEDURE *)
                 VARIABLE ( FSYS + [ COMMA , RPARENT ] ) ;
                 LOADADDRESS ;
                 LSP := NIL ;
                 LSP1 := NIL ;
                 LB := 1 ;
                 BS := 1 ;
                 LATTR := GATTR ;
                 if GATTR . TYPTR <> NIL then
                   with GATTR . TYPTR -> do
                     if FORM = ARRAYS then
                       begin
                         LSP := INXTYPE ;
                         LSP1 := AELTYPE ;
                         if ( INXTYPE = CHARPTR ) or ( INXTYPE =
                         BOOLPTR ) then
                           LB := 0
                         else
                           if INXTYPE -> . FORM = SUBRANGE then
                             LB := INXTYPE -> . MIN . IVAL ;
                         BS := AELTYPE -> . SIZE
                       end (* then *)
                     else
                       ERROR ( 116 ) ;
                 if SY = COMMA then
                   INSYMBOL
                 else
                   ERROR ( 20 ) ;
                 EXPRESSION ( FSYS + [ COMMA , RPARENT ] ) ;
                 LOAD ;
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM <> SCALAR then
                     ERROR ( 116 )
                   else
                     if not COMPTYPES ( LSP , GATTR . TYPTR ) then
                       ERROR ( 116 ) ;
                 GEN2 ( 51  (*ldc*) , 1 , LB ) ;
                 GEN0 ( 21  (*sbi*) ) ;
                 GEN2 ( 51  (*ldc*) , 1 , BS ) ;
                 GEN0 ( 15  (*mpi*) ) ;
                 if SY = COMMA then
                   INSYMBOL
                 else
                   ERROR ( 20 ) ;
                 VARIABLE ( FSYS + [ RPARENT ] ) ;
                 LOADADDRESS ;
                 if GATTR . TYPTR <> NIL then
                   with GATTR . TYPTR -> do
                     if FORM = ARRAYS then
                       begin
                         if not COMPTYPES ( AELTYPE , LSP1 ) then
                           ERROR ( 116 )
                       end (* then *)
                     else
                       ERROR ( 116 ) ;
                 if ( GATTR . TYPTR <> NIL ) and ( LATTR . TYPTR <> NIL
                 ) then
                   GEN2 ( 62  (*pck*) , GATTR . TYPTR -> . SIZE , LATTR
                          . TYPTR -> . SIZE )
               end (* PACKPROCEDURE *) ;


            procedure UNPACKPROCEDURE ;

               var LSP , LSP1 : STP ;
                   LATTR , LATTR1 : ATTR ;
                   LB , BS : INTEGER ;

               begin (* UNPACKPROCEDURE *)
                 VARIABLE ( FSYS + [ COMMA , RPARENT ] ) ;
                 LOADADDRESS ;
                 LATTR := GATTR ;
                 LSP := NIL ;
                 LSP1 := NIL ;
                 LB := 1 ;
                 BS := 1 ;
                 if GATTR . TYPTR <> NIL then
                   with GATTR . TYPTR -> do
                     if FORM = ARRAYS then
                       LSP1 := AELTYPE
                     else
                       ERROR ( 116 ) ;
                 if SY = COMMA then
                   INSYMBOL
                 else
                   ERROR ( 20 ) ;
                 VARIABLE ( FSYS + [ COMMA , RPARENT ] ) ;
                 LOADADDRESS ;
                 LATTR1 := GATTR ;
                 if GATTR . TYPTR <> NIL then
                   with GATTR . TYPTR -> do
                     if FORM = ARRAYS then
                       begin
                         if not COMPTYPES ( AELTYPE , LSP1 ) then
                           ERROR ( 116 ) ;
                         if ( INXTYPE = CHARPTR ) or ( INXTYPE =
                         BOOLPTR ) then
                           LB := 0
                         else
                           if INXTYPE -> . FORM = SUBRANGE then
                             LB := INXTYPE -> . MIN . IVAL ;
                         BS := AELTYPE -> . SIZE ;
                         LSP := INXTYPE ;
                       end (* then *)
                     else
                       ERROR ( 116 ) ;
                 if SY = COMMA then
                   INSYMBOL
                 else
                   ERROR ( 20 ) ;
                 EXPRESSION ( FSYS + [ RPARENT ] ) ;
                 LOAD ;
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM <> SCALAR then
                     ERROR ( 116 )
                   else
                     if not COMPTYPES ( LSP , GATTR . TYPTR ) then
                       ERROR ( 116 ) ;
                 GEN2 ( 51  (*ldc*) , 1 , LB ) ;
                 GEN0 ( 21  (*sbi*) ) ;
                 GEN2 ( 51  (*ldc*) , 1 , BS ) ;
                 GEN0 ( 15  (*mpi*) ) ;
                 if ( GATTR . TYPTR <> NIL ) and ( LATTR . TYPTR <> NIL
                 ) then
                   GEN2 ( 63  (*upk*) , LATTR . TYPTR -> . SIZE ,
                          LATTR1 . TYPTR -> . SIZE )
               end (* UNPACKPROCEDURE *) ;


            procedure NEWDISPOSEPROCEDURE ;

               label 1 ;

               var LSP , LSP1 : STP ;
                   VARTS : INTEGER ;
                   LSIZE : ADDRRANGE ;
                   LVAL : VALU ;

               begin (* NEWDISPOSEPROCEDURE *)
                 VARIABLE ( FSYS + [ COMMA , RPARENT ] ) ;
                 LOADADDRESS ;
                 LSP := NIL ;
                 VARTS := 0 ;
                 LSIZE := 0 ;
                 if GATTR . TYPTR <> NIL then
                   with GATTR . TYPTR -> do
                     if FORM = POINTER then
                       begin
                         if ELTYPE <> NIL then
                           begin
                             LSIZE := ELTYPE -> . SIZE ;
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

                 (*********************************************************)
                 (*check to insert here: is constant in tagfieldtype range*)
                 (*********************************************************)

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
                 GEN2 ( 51  (*ldc*) , 1 , LSIZE ) ;
                 if LKEY = 9 then
                   GEN1 ( 30  (*csp*) , 12  (*new*) )
                 else
                   GEN1 ( 30  (*csp*) , 29  (*dispose*) )
               end (* NEWDISPOSEPROCEDURE *) ;


            procedure ABSFUNCTION ;

               begin (* ABSFUNCTION *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR = INTPTR then
                     GEN0 ( 0  (*abi*) )
                   else
                     if GATTR . TYPTR = REALPTR then
                       GEN0 ( 1  (*abr*) )
                     else
                       begin
                         ERROR ( 125 ) ;
                         GATTR . TYPTR := INTPTR
                       end (* else *)
               end (* ABSFUNCTION *) ;


            procedure SQRFUNCTION ;

               begin (* SQRFUNCTION *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR = INTPTR then
                     GEN0 ( 24  (*sqi*) )
                   else
                     if GATTR . TYPTR = REALPTR then
                       GEN0 ( 25  (*sqr*) )
                     else
                       begin
                         ERROR ( 125 ) ;
                         GATTR . TYPTR := INTPTR
                       end (* else *)
               end (* SQRFUNCTION *) ;


            procedure TRUNCFUNCTION ;

               begin (* TRUNCFUNCTION *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> REALPTR then
                     ERROR ( 125 ) ;
                 GEN0 ( 27  (*trc*) ) ;
                 GATTR . TYPTR := INTPTR
               end (* TRUNCFUNCTION *) ;


            procedure ROUNDFUNCTION ;

               begin (* ROUNDFUNCTION *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> REALPTR then
                     ERROR ( 125 ) ;
                 GEN0 ( 61  (*rnd*) ) ;
                 GATTR . TYPTR := INTPTR
               end (* ROUNDFUNCTION *) ;


            procedure ODDFUNCTION ;

               begin (* ODDFUNCTION *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> INTPTR then
                     ERROR ( 125 ) ;
                 GEN0 ( 20  (*odd*) ) ;
                 GATTR . TYPTR := BOOLPTR
               end (* ODDFUNCTION *) ;


            procedure ORDFUNCTION ;

               begin (* ORDFUNCTION *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM >= POWER then
                     ERROR ( 125 ) ;
                 GEN0T ( 58  (*ord*) , GATTR . TYPTR ) ;
                 GATTR . TYPTR := INTPTR
               end (* ORDFUNCTION *) ;


            procedure CHRFUNCTION ;

               begin (* CHRFUNCTION *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> INTPTR then
                     ERROR ( 125 ) ;
                 GEN0 ( 59  (*chr*) ) ;
                 GATTR . TYPTR := CHARPTR
               end (* CHRFUNCTION *) ;


            procedure PREDSUCCFUNCTION ;

               begin (* PREDSUCCFUNCTION *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM <> SCALAR then
                     ERROR ( 125 ) ;
                 if LKEY = 7 then
                   GEN1T ( 31  (*dec*) , 1 , GATTR . TYPTR )
                 else
                   GEN1T ( 34  (*inc*) , 1 , GATTR . TYPTR )
               end (* PREDSUCCFUNCTION *) ;


            procedure EOFEOLNFUNCTION ;

               begin (* EOFEOLNFUNCTION *)
                 if SY = LPARENT then
                   begin
                     INSYMBOL ;
                     VARIABLE ( FSYS + [ RPARENT ] ) ;
                     if SY = RPARENT then
                       INSYMBOL
                     else
                       ERROR ( 4 )
                   end (* then *)
                 else
                   begin
                     if not INPUTHDF then
                       ERROR ( 175 ) ;
                     with GATTR do
                       begin
                         TYPTR := TEXTPTR ;
                         KIND := VARBL ;
                         ACCESS := DRCT ;
                         VLEVEL := INPUTPTR -> . VLEV ;
                         DPLMT := INPUTPTR -> . VADDR
                       end (* with *)
                   end (* else *) ;
                 LOADADDRESS ;
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM <> FILES then
                     ERROR ( 125 )
                   else
                     if ( LKEY = 10 ) and ( GATTR . TYPTR <> TEXTPTR )
                     then
                       ERROR ( 116 ) ;
                 if LKEY = 9 then
                   begin
                     if GATTR . TYPTR = TEXTPTR then
                       GEN0 ( 8  (*eof*) )
                     else
                       GEN0 ( 69  (*efb*) )
                   end (* then *)
                 else
                   GEN1 ( 30  (*csp*) , 14  (*eln*) ) ;
                 GATTR . TYPTR := BOOLPTR
               end (* EOFEOLNFUNCTION *) ;


            procedure CALLNONSTANDARD ;

               var NXT , LCP : CTP ;
                   LSP : STP ;
                   LKIND : IDKIND ;
                   LB : BOOLEAN ;
                   LOCPAR , LLC : ADDRRANGE ;


               procedure COMPPARAM ( PLA , PLB : CTP ) ;

                  begin (* COMPPARAM *)
                    while ( PLA <> NIL ) and ( PLB <> NIL ) do
                      begin
                        if not COMPTYPES ( PLA -> . IDTYPE , PLB -> .
                        IDTYPE ) then
                          ERROR ( 189 ) ;
                        PLA := PLA -> . NEXT ;
                        PLB := PLB -> . NEXT
                      end (* while *) ;
                    if ( PLA <> NIL ) or ( PLB <> NIL ) then
                      ERROR ( 189 )
                  end (* COMPPARAM *) ;


               begin (* CALLNONSTANDARD *)
                 LOCPAR := 0 ;
                 with FCP -> do
                   begin
                     NXT := PFLIST ;
                     LKIND := PFKIND ;
                     if PFKIND = ACTUAL then
                       begin                 (* it's a system call *)
                         if not EXTERNL then
                           GEN1 ( 41  (*mst*) , LEVEL - PFLEV )
                       end (* then *)
                     else
                       GEN1 ( 41  (*mst*) , LEVEL - PFLEV )

                 (*******************)
                 (* its an indirect *)
                 (*******************)

                   end (* with *) ;
                 if SY = LPARENT then
                   begin
                     LLC := LC ;
                     repeat
                       LB := FALSE ;

                 (*****************************************)
                 (*decide whether proc/func must be passed*)
                 (*****************************************)

                       if NXT = NIL then
                         ERROR ( 126 )
                       else
                         LB := NXT -> . KLASS in [ PROC , FUNC ] ;
                       INSYMBOL ;
                       if LB then (*pass function or procedure*)
                         begin
                           if SY <> IDENT then
                             begin
                               ERROR ( 2 ) ;
                               SKIP ( FSYS + [ COMMA , RPARENT ] )
                             end (* then *)
                           else
                             if NXT <> NIL then
                               begin
                                 if NXT -> . KLASS = PROC then
                                   SEARCHID ( [ PROC ] , LCP )
                                 else
                                   begin
                                     SEARCHID ( [ FUNC ] , LCP ) ;

                 (************************)
                 (* compare result types *)
                 (************************)

                                     if not COMPTYPES ( LCP -> . IDTYPE
                                     , NXT -> . IDTYPE ) then
                                       ERROR ( 128 )
                                   end (* else *) ;

                 (***************************)
                 (* compare parameter lists *)
                 (***************************)

                                 if ( NXT -> . KLASS in [ PROC , FUNC ]
                                 ) and ( LCP -> . KLASS in [ PROC ,
                                 FUNC ] ) then
                                   COMPPARAM ( NXT -> . PFLIST , LCP ->
                                               . PFLIST ) ;
                                 if LCP -> . PFKIND = ACTUAL then
                                   GENLPA ( LCP -> . PFNAME , LEVEL -
                                            LCP -> . PFLEV )
                                 else
                                   GEN2 ( 74  (*lip*) , LEVEL - LCP ->
                                          . PFLEV , LCP -> . PFADDR ) ;
                                 LOCPAR := LOCPAR + PTRSIZE * 2 ;
                                 INSYMBOL ;
                                 if not ( SY in FSYS + [ COMMA ,
                                 RPARENT ] ) then
                                   begin
                                     ERROR ( 6 ) ;
                                     SKIP ( FSYS + [ COMMA , RPARENT ]
                                            )
                                   end (* then *)
                               end (* then *)
                         end (* then *)
                       else
                         begin
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
                                         if LSP -> . FORM <= POWER then
                                           begin
                                             LOAD ;
                                             if DEBUG then
                                               CHECKBNDS ( LSP ) ;
                                             if COMPTYPES ( REALPTR ,
                                             LSP ) and ( GATTR . TYPTR
                                             = INTPTR ) then
                                               begin
                                                 GEN0 ( 10  (*flt*) ) ;
                                                 GATTR . TYPTR :=
                                                   REALPTR
                                               end (* then *) ;
                                             LOCPAR := LOCPAR + LSP ->
                                                   . SIZE ;
                                             ALIGN ( PARMPTR , LOCPAR )
                                                   ;
                                           end (* then *)
                                         else
                                           begin
                                             LOADADDRESS ;
                                             LOCPAR := LOCPAR + PTRSIZE
                                                   ;
                                             ALIGN ( PARMPTR , LOCPAR )
                                           end (* else *)
                                       else
                                         if GATTR . KIND = VARBL then
                                           begin
                                             LOADADDRESS ;
                                             LOCPAR := LOCPAR + PTRSIZE
                                                   ;
                                             ALIGN ( PARMPTR , LOCPAR )
                                                   ;
                                           end (* then *)
                                         else
                                           ERROR ( 154 ) ;
                                       if not COMPTYPES ( LSP , GATTR .
                                       TYPTR ) then
                                         ERROR ( 142 )
                                     end (* then *)
                                 end (* then *)
                             end (* then *)
                         end (* else *) ;
                       if NXT <> NIL then
                         NXT := NXT -> . NEXT
                     until SY <> COMMA ;
                     LC := LLC ;
                     if SY = RPARENT then
                       INSYMBOL
                     else
                       ERROR ( 4 )
                   end (* then *) ;
                 if LKIND = ACTUAL then
                   begin
                     if NXT <> NIL then
                       ERROR ( 126 ) ;
                     with FCP -> do
                       begin
                         if EXTERNL then
                           GEN1 ( 30  (*csp*) , PFNAME )
                         else
                           GENCUPENT ( 46  (*cup*) , LOCPAR , PFNAME )
                                       ;
                       end (* with *)
                   end (* then *)
                 else
                   begin (* call procedure or function parameter *)
                     GEN2 ( 50  (*lda*) , LEVEL - FCP -> . PFLEV , FCP
                            -> . PFADDR ) ;
                     GEN1 ( 67  (*cip*) , LOCPAR )
                   end (* else *) ;
                 GATTR . TYPTR := FCP -> . IDTYPE
               end (* CALLNONSTANDARD *) ;


            begin (* CALL *)
              if FCP -> . PFDECKIND = STANDARD then
                begin
                  LKEY := FCP -> . KEY ;
                  if FCP -> . KLASS = PROC then
                    begin
                      if not ( LKEY in [ 5 , 6 , 11 , 12 , 17 ] ) then
                        if SY = LPARENT then
                          INSYMBOL
                        else
                          ERROR ( 9 ) ;
                      case LKEY of
                        1 , 2 , 3 , 4 :
                          GETPUTRESETREWRITEPRocedure ;
                        17 : PAGEPROCEDURE ;
                        5 , 11 :
                          READPROCEDURE ;
                        6 , 12 :
                          WRITEPROCEDURE ;
                        7 : PACKPROCEDURE ;
                        8 : UNPACKPROCEDURE ;
                        9 , 18 :
                          NEWDISPOSEPROCEDURE ;
                        10 , 13 :
                          ERROR ( 399 )
                      end (* case *) ;
                      if not ( LKEY in [ 5 , 6 , 11 , 12 , 17 ] ) then
                        if SY = RPARENT then
                          INSYMBOL
                        else
                          ERROR ( 4 )
                    end (* then *)
                  else
                    begin
                      if ( LKEY <= 8 ) or ( LKEY = 16 ) then
                        begin
                          if SY = LPARENT then
                            INSYMBOL
                          else
                            ERROR ( 9 ) ;
                          EXPRESSION ( FSYS + [ RPARENT ] ) ;
                          LOAD
                        end (* then *) ;
                      case LKEY of
                        1 : ABSFUNCTION ;
                        2 : SQRFUNCTION ;
                        3 : TRUNCFUNCTION ;
                        16 : ROUNDFUNCTION ;
                        4 : ODDFUNCTION ;
                        5 : ORDFUNCTION ;
                        6 : CHRFUNCTION ;
                        7 , 8 : PREDSUCCFUNCTION ;
                        9 , 10 :
                          EOFEOLNFUNCTION
                      end (* case *) ;
                      if ( LKEY <= 8 ) or ( LKEY = 16 ) then
                        if SY = RPARENT then
                          INSYMBOL
                        else
                          ERROR ( 4 )
                    end (* else *) ;
                end (* then *)
              else
                CALLNONSTANDARD
            end (* CALL *) ;


         procedure EXPRESSION ;

            var LATTR : ATTR ;
                LOP : OPERATOR ;
                TYPIND : CHAR ;
                LSIZE : ADDRRANGE ;


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
                         CSTHELP : SETTY ;
                         CSTPART : SETTY ;
                         LSP : STP ;
                         TATTR , RATTR : ATTR ;
                         TEST : BOOLEAN ;

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

                       (****)
                       (*id*)
                       (****)

                             IDENT : begin
                                       SEARCHID ( [ KONST , VARS ,
                                                  FIELD , FUNC ] , LCP
                                                  ) ;
                                       INSYMBOL ;
                                       if LCP -> . KLASS = FUNC then
                                         begin
                                           CALL ( FSYS , LCP ) ;
                                           with GATTR do
                                             begin
                                               KIND := EXPR ;
                                               if TYPTR <> NIL then
                                                 if TYPTR -> . FORM =
                                                 SUBRANGE then
                                                   TYPTR := TYPTR -> .
                                                   RANGETYPE
                                             end (* with *)
                                         end (* then *)
                                       else
                                         if LCP -> . KLASS = KONST then
                                           with GATTR , LCP -> do
                                             begin
                                               TYPTR := IDTYPE ;
                                               KIND := CST ;
                                               CVAL := VALUES
                                             end (* with *)
                                         else
                                           begin
                                             SELECTOR ( FSYS , LCP ) ;
                                             if GATTR . TYPTR <> NIL
                                             then

                       (********************)
                       (*elim.subr.types to*)
                       (********************)

                                               with GATTR , TYPTR -> do

                       (**********************)
                       (*simplify later tests*)
                       (**********************)

                                                 if FORM = SUBRANGE
                                                 then
                                                   TYPTR := RANGETYPE
                                           end (* else *)
                                     end (* tag/ca *) ;

                       (*****)
                       (*cst*)
                       (*****)

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
                                     if LGTH = 1 then
                                       TYPTR := CHARPTR
                                     else
                                       begin
                                         NEW ( LSP , ARRAYS ) ;
                                         PSHSTC ( LSP ) ;
                                         with LSP -> do
                                           begin
                                             AELTYPE := CHARPTR ;
                                             FORM := ARRAYS ;
                                             PACKING := TRUE ;
                                             INXTYPE := NIL ;
                                             SIZE := LGTH * CHARSIZE
                                           end (* with *) ;
                                         TYPTR := LSP
                                       end (* else *) ;
                                     KIND := CST ;
                                     CVAL := VAL
                                   end (* with *) ;
                                 INSYMBOL
                               end (* tag/ca *) ;

                       (*****)
                       (* ( *)
                       (*****)

                             LPARENT :
                               begin
                                 INSYMBOL ;
                                 EXPRESSION ( FSYS + [ RPARENT ] ) ;
                                 if SY = RPARENT then
                                   INSYMBOL
                                 else
                                   ERROR ( 4 )
                               end (* tag/ca *) ;

                       (*****)
                       (*not*)
                       (*****)

                             NOTSY : begin
                                       INSYMBOL ;
                                       FACTOR ( FSYS ) ;
                                       LOAD ;
                                       GEN0 ( 19  (*not*) ) ;
                                       if GATTR . TYPTR <> NIL then
                                         if GATTR . TYPTR <> BOOLPTR
                                         then
                                           begin
                                             ERROR ( 135 ) ;
                                             GATTR . TYPTR := NIL
                                           end (* then *) ;
                                     end (* tag/ca *) ;

                       (***)
                       (*[*)
                       (***)

                             LBRACK :
                               begin
                                 INSYMBOL ;
                                 CSTPART := [ ] ;
                                 VARPART := FALSE ;
                                 NEW ( LSP , POWER ) ;
                                 PSHSTC ( LSP ) ;
                                 with LSP -> do
                                   begin
                                     ELSET := NIL ;
                                     SIZE := SETSIZE ;
                                     FORM := POWER ;
                                     PACKING := FALSE ;
                                     MATCHPACK := FALSE
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
                                                   RANGE , RBRACK ] ) ;
                                       RATTR . TYPTR := NIL ;
                                       if SY = RANGE then
                                         begin
                                           INSYMBOL ;

                       (*******************************************************************)
                       (* if the left side is not constant, load it                       *)
                       (*                                    and coerce it to integer now *)
                       (*******************************************************************)

                                           if GATTR . KIND <> CST then
                                             begin
                                               LOAD ;
                                               if not COMPTYPES ( GATTR
                                               . TYPTR , INTPTR ) then
                                                 GEN0T ( 58  (*ord*) ,
                                                   GATTR . TYPTR ) ;
                                             end (* then *) ;
                                           TATTR := GATTR ;
                                           EXPRESSION ( FSYS + [ COMMA
                                                   , RBRACK ] ) ;
                                           RATTR := GATTR ;
                                           GATTR := TATTR ;
                                         end (* then *) ;
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
                                               if RATTR . TYPTR <> NIL
                                               then
                                                 begin                  (* x..y form *)
                                                   if RATTR . TYPTR ->
                                                   . FORM <> SCALAR
                                                   then
                                                   begin
                                                   ERROR ( 136 ) ;
                                                   RATTR . TYPTR := NIL
                                                   end (* then *)
                                                   else
                                                   if COMPTYPES ( LSP
                                                   -> . ELSET , RATTR .
                                                   TYPTR ) then
                                                   begin
                                                   if ( GATTR . KIND =
                                                   CST ) and ( RATTR .
                                                   KIND = CST ) then
                                                   if ( LATTR . CVAL .
                                                   IVAL < SETLOW ) or (
                                                   LATTR . CVAL . IVAL
                                                   > SETHIGH ) or (
                                                   GATTR . CVAL . IVAL
                                                   < SETLOW ) or (
                                                   GATTR . CVAL . IVAL
                                                   > SETHIGH ) then
                                                   ERROR ( 304 )
                                                   else
                                                   begin
                                                   CSTHELP := [ GATTR .
                                                   CVAL . IVAL .. RATTR
                                                   . CVAL . IVAL ] ;
                                                   CSTPART := CSTPART +
                                                   CSTHELP
                                                   end (* else *)
                                                   else
                                                   begin
                                                   TATTR := GATTR ;
                                                   GATTR := RATTR ;
                                                   LOAD ;
                                                   GATTR := TATTR ;
                                                   if not COMPTYPES (
                                                   RATTR . TYPTR ,
                                                   INTPTR ) then
                                                   GEN0T ( 58  (*ord*)
                                                   , RATTR . TYPTR ) ;
                                                   GEN0 ( 64  (*rgs*) )
                                                   ;
                                                   if VARPART then
                                                   GEN0 ( 28  (*uni*) )
                                                   else
                                                   VARPART := TRUE
                                                   end (* else *)
                                                   end (* then *)
                                                   else
                                                   ERROR ( 137 )
                                                 end (* then *)
                                               else
                                                 begin
                                                   if GATTR . KIND =
                                                   CST then
                                                   if ( GATTR . CVAL .
                                                   IVAL < SETLOW ) or (
                                                   GATTR . CVAL . IVAL
                                                   > SETHIGH ) then
                                                   ERROR ( 304 )
                                                   else
                                                   CSTPART := CSTPART +
                                                   [ GATTR . CVAL .
                                                   IVAL ]
                                                   else
                                                   begin
                                                   LOAD ;
                                                   if not COMPTYPES (
                                                   GATTR . TYPTR ,
                                                   INTPTR ) then
                                                   GEN0T ( 58  (*ord*)
                                                   , GATTR . TYPTR ) ;
                                                   GEN0 ( 23  (*sgs*) )
                                                   ;
                                                   if VARPART then
                                                   GEN0 ( 28  (*uni*) )
                                                   else
                                                   VARPART := TRUE
                                                   end (* else *)
                                                 end (* else *) ;
                                               LSP -> . ELSET := GATTR
                                                   . TYPTR ;
                                               GATTR . TYPTR := LSP
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
                                     if CSTPART <> [ ] then
                                       begin
                                         NEW ( LVP , PSET ) ;
                                         PSHCST ( LVP ) ;
                                         LVP -> . PVAL := CSTPART ;
                                         LVP -> . CCLASS := PSET ;
                                         if CSTPTRIX = CSTOCCMAX then
                                           ERROR ( 254 )
                                         else
                                           begin
                                             CSTPTRIX := CSTPTRIX + 1 ;
                                             CSTPTR [ CSTPTRIX ] := LVP
                                                   ;
                                             GEN2 ( 51  (*ldc*) , 5 ,
                                                   CSTPTRIX ) ;
                                             GEN0 ( 28  (*uni*) ) ;
                                             GATTR . KIND := EXPR
                                           end (* else *)
                                       end (* then *)
                                   end (* then *)
                                 else
                                   begin
                                     NEW ( LVP , PSET ) ;
                                     PSHCST ( LVP ) ;
                                     LVP -> . PVAL := CSTPART ;
                                     LVP -> . CCLASS := PSET ;
                                     GATTR . CVAL . VALP := LVP
                                   end (* else *)
                               end (* tag/ca *) ;

                       (*****)
                       (*nil*)
                       (*****)

                             NILSY : with GATTR do
                                       begin
                                         TYPTR := NILPTR ;
                                         KIND := CST ;
                                         CVAL . IVAL := NILVAL ;
                                         INSYMBOL
                                       end (* with *)
                           end (* case *) ;
                           if not ( SY in FSYS ) then
                             begin
                               ERROR ( 6 ) ;
                               SKIP ( FSYS + FACBEGSYS )
                             end (* then *)
                         end (* while *)
                     end (* FACTOR *) ;


                  begin (* TERM *)
                    FACTOR ( FSYS + [ MULOP ] ) ;
                    while SY = MULOP do
                      begin
                        LOAD ;
                        LATTR := GATTR ;
                        LOP := OP ;
                        INSYMBOL ;
                        FACTOR ( FSYS + [ MULOP ] ) ;
                        LOAD ;
                        if ( LATTR . TYPTR <> NIL ) and ( GATTR . TYPTR
                        <> NIL ) then
                          case LOP of

                    (***)

                            MUL : if ( LATTR . TYPTR = INTPTR ) and (
                                  GATTR . TYPTR = INTPTR ) then
                                    GEN0 ( 15  (*mpi*) )
                                  else
                                    begin
                                      if LATTR . TYPTR = INTPTR then
                                        begin
                                          GEN0 ( 9  (*flo*) ) ;
                                          LATTR . TYPTR := REALPTR
                                        end (* then *)
                                      else
                                        if GATTR . TYPTR = INTPTR then
                                          begin
                                            GEN0 ( 10  (*flt*) ) ;
                                            GATTR . TYPTR := REALPTR
                                          end (* then *) ;
                                      if ( LATTR . TYPTR = REALPTR )
                                      and ( GATTR . TYPTR = REALPTR )
                                      then
                                        GEN0 ( 16  (*mpr*) )
                                      else
                                        if ( LATTR . TYPTR -> . FORM =
                                        POWER ) and COMPTYPES ( LATTR .
                                        TYPTR , GATTR . TYPTR ) then
                                          GEN0 ( 12  (*int*) )
                                        else
                                          begin
                                            ERROR ( 134 ) ;
                                            GATTR . TYPTR := NIL
                                          end (* else *)
                                    end (* else *) ;

                    (*****)
                    (* / *)
                    (*****)

                            RDIV : begin
                                     if GATTR . TYPTR = INTPTR then
                                       begin
                                         GEN0 ( 10  (*flt*) ) ;
                                         GATTR . TYPTR := REALPTR
                                       end (* then *) ;
                                     if LATTR . TYPTR = INTPTR then
                                       begin
                                         GEN0 ( 9  (*flo*) ) ;
                                         LATTR . TYPTR := REALPTR
                                       end (* then *) ;
                                     if ( LATTR . TYPTR = REALPTR ) and
                                     ( GATTR . TYPTR = REALPTR ) then
                                       GEN0 ( 7  (*dvr*) )
                                     else
                                       begin
                                         ERROR ( 134 ) ;
                                         GATTR . TYPTR := NIL
                                       end (* else *)
                                   end (* tag/ca *) ;

                    (*****)
                    (*div*)
                    (*****)

                            IDIV : if ( LATTR . TYPTR = INTPTR ) and (
                                   GATTR . TYPTR = INTPTR ) then
                                     GEN0 ( 6  (*dvi*) )
                                   else
                                     begin
                                       ERROR ( 134 ) ;
                                       GATTR . TYPTR := NIL
                                     end (* else *) ;

                    (*****)
                    (*mod*)
                    (*****)

                            IMOD : if ( LATTR . TYPTR = INTPTR ) and (
                                   GATTR . TYPTR = INTPTR ) then
                                     GEN0 ( 14  (*mod*) )
                                   else
                                     begin
                                       ERROR ( 134 ) ;
                                       GATTR . TYPTR := NIL
                                     end (* else *) ;

                    (*****)
                    (*and*)
                    (*****)

                            ANDOP : if ( LATTR . TYPTR = BOOLPTR ) and
                                    ( GATTR . TYPTR = BOOLPTR ) then
                                      GEN0 ( 4  (*and*) )
                                    else
                                      begin
                                        ERROR ( 134 ) ;
                                        GATTR . TYPTR := NIL
                                      end (* else *)
                          end (* case *)
                        else
                          GATTR . TYPTR := NIL
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
                       GEN0 ( 17  (*ngi*) )
                     else
                       if GATTR . TYPTR = REALPTR then
                         GEN0 ( 18  (*ngr*) )
                       else
                         begin
                           ERROR ( 134 ) ;
                           GATTR . TYPTR := NIL
                         end (* else *)
                   end (* then *) ;
                 while SY = ADDOP do
                   begin
                     LOAD ;
                     LATTR := GATTR ;
                     LOP := OP ;
                     INSYMBOL ;
                     TERM ( FSYS + [ ADDOP ] ) ;
                     LOAD ;
                     if ( LATTR . TYPTR <> NIL ) and ( GATTR . TYPTR <>
                     NIL ) then
                       case LOP of

                 (***)
                 (*+*)
                 (***)

                         PLUS : if ( LATTR . TYPTR = INTPTR ) and (
                                GATTR . TYPTR = INTPTR ) then
                                  GEN0 ( 2  (*adi*) )
                                else
                                  begin
                                    if LATTR . TYPTR = INTPTR then
                                      begin
                                        GEN0 ( 9  (*flo*) ) ;
                                        LATTR . TYPTR := REALPTR
                                      end (* then *)
                                    else
                                      if GATTR . TYPTR = INTPTR then
                                        begin
                                          GEN0 ( 10  (*flt*) ) ;
                                          GATTR . TYPTR := REALPTR
                                        end (* then *) ;
                                    if ( LATTR . TYPTR = REALPTR ) and
                                    ( GATTR . TYPTR = REALPTR ) then
                                      GEN0 ( 3  (*adr*) )
                                    else
                                      if ( LATTR . TYPTR -> . FORM =
                                      POWER ) and COMPTYPES ( LATTR .
                                      TYPTR , GATTR . TYPTR ) then
                                        GEN0 ( 28  (*uni*) )
                                      else
                                        begin
                                          ERROR ( 134 ) ;
                                          GATTR . TYPTR := NIL
                                        end (* else *)
                                  end (* else *) ;

                 (***)
                 (*-*)
                 (***)

                         MINUS : if ( LATTR . TYPTR = INTPTR ) and (
                                 GATTR . TYPTR = INTPTR ) then
                                   GEN0 ( 21  (*sbi*) )
                                 else
                                   begin
                                     if LATTR . TYPTR = INTPTR then
                                       begin
                                         GEN0 ( 9  (*flo*) ) ;
                                         LATTR . TYPTR := REALPTR
                                       end (* then *)
                                     else
                                       if GATTR . TYPTR = INTPTR then
                                         begin
                                           GEN0 ( 10  (*flt*) ) ;
                                           GATTR . TYPTR := REALPTR
                                         end (* then *) ;
                                     if ( LATTR . TYPTR = REALPTR ) and
                                     ( GATTR . TYPTR = REALPTR ) then
                                       GEN0 ( 22  (*sbr*) )
                                     else
                                       if ( LATTR . TYPTR -> . FORM =
                                       POWER ) and COMPTYPES ( LATTR .
                                       TYPTR , GATTR . TYPTR ) then
                                         GEN0 ( 5  (*dif*) )
                                       else
                                         begin
                                           ERROR ( 134 ) ;
                                           GATTR . TYPTR := NIL
                                         end (* else *)
                                   end (* else *) ;

                 (****)
                 (*or*)
                 (****)

                         OROP : if ( LATTR . TYPTR = BOOLPTR ) and (
                                GATTR . TYPTR = BOOLPTR ) then
                                  GEN0 ( 13  (*ior*) )
                                else
                                  begin
                                    ERROR ( 134 ) ;
                                    GATTR . TYPTR := NIL
                                  end (* else *)
                       end (* case *)
                     else
                       GATTR . TYPTR := NIL
                   end (* while *)
               end (* SIMPLEEXPRESSION *) ;


            begin (* EXPRESSION *)
              SIMPLEEXPRESSION ( FSYS + [ RELOP ] ) ;
              if SY = RELOP then
                begin
                  if GATTR . TYPTR <> NIL then
                    if GATTR . TYPTR -> . FORM <= POWER then
                      LOAD
                    else
                      LOADADDRESS ;
                  LATTR := GATTR ;
                  LOP := OP ;
                  if LOP = INOP then
                    if not COMPTYPES ( GATTR . TYPTR , INTPTR ) then
                      GEN0T ( 58  (*ord*) , GATTR . TYPTR ) ;
                  INSYMBOL ;
                  SIMPLEEXPRESSION ( FSYS ) ;
                  if GATTR . TYPTR <> NIL then
                    if GATTR . TYPTR -> . FORM <= POWER then
                      LOAD
                    else
                      LOADADDRESS ;
                  if ( LATTR . TYPTR <> NIL ) and ( GATTR . TYPTR <>
                  NIL ) then
                    if LOP = INOP then
                      if GATTR . TYPTR -> . FORM = POWER then
                        if COMPTYPES ( LATTR . TYPTR , GATTR . TYPTR ->
                        . ELSET ) then
                          GEN0 ( 11  (*inn*) )
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
                          if LATTR . TYPTR = INTPTR then
                            begin
                              GEN0 ( 9  (*flo*) ) ;
                              LATTR . TYPTR := REALPTR
                            end (* then *)
                          else
                            if GATTR . TYPTR = INTPTR then
                              begin
                                GEN0 ( 10  (*flt*) ) ;
                                GATTR . TYPTR := REALPTR
                              end (* then *) ;
                        if COMPTYPES ( LATTR . TYPTR , GATTR . TYPTR )
                        then
                          begin
                            LSIZE := LATTR . TYPTR -> . SIZE ;
                            case LATTR . TYPTR -> . FORM of
                              SCALAR :
                                if LATTR . TYPTR = REALPTR then
                                  TYPIND := 'r'
                                else
                                  if LATTR . TYPTR = BOOLPTR then
                                    TYPIND := 'b'
                                  else
                                    if LATTR . TYPTR = CHARPTR then
                                      TYPIND := 'c'
                                    else
                                      TYPIND := 'i' ;
                              POINTER :
                                begin
                                  if LOP in [ LTOP , LEOP , GTOP , GEOP
                                  ] then
                                    ERROR ( 131 ) ;
                                  TYPIND := 'a'
                                end (* tag/ca *) ;
                              POWER : begin
                                        if LOP in [ LTOP , GTOP ] then
                                          ERROR ( 132 ) ;
                                        TYPIND := 's'
                                      end (* tag/ca *) ;
                              ARRAYS :
                                begin
                                  if not STRING ( LATTR . TYPTR ) then
                                    ERROR ( 134 ) ;
                                  TYPIND := 'm'
                                end (* tag/ca *) ;
                              RECORDS :
                                begin
                                  ERROR ( 134 ) ;
                                  TYPIND := 'm'
                                end (* tag/ca *) ;
                              FILES : begin
                                        ERROR ( 133 ) ;
                                        TYPIND := 'f'
                                      end (* tag/ca *)
                            end (* case *) ;
                            case LOP of
                              LTOP : GEN2 ( 53  (*les*) , ORD ( TYPIND
                                            ) , LSIZE ) ;
                              LEOP : GEN2 ( 52  (*leq*) , ORD ( TYPIND
                                            ) , LSIZE ) ;
                              GTOP : GEN2 ( 49  (*grt*) , ORD ( TYPIND
                                            ) , LSIZE ) ;
                              GEOP : GEN2 ( 48  (*geq*) , ORD ( TYPIND
                                            ) , LSIZE ) ;
                              NEOP : GEN2 ( 55  (*neq*) , ORD ( TYPIND
                                            ) , LSIZE ) ;
                              EQOP : GEN2 ( 47  (*equ*) , ORD ( TYPIND
                                            ) , LSIZE )
                            end (* case *)
                          end (* then *)
                        else
                          ERROR ( 129 )
                      end (* else *) ;
                  GATTR . TYPTR := BOOLPTR ;
                  GATTR . KIND := EXPR
                end (* then *)
            end (* EXPRESSION *) ;


         procedure ASSIGNMENT ( FCP : CTP ) ;

            var LATTR : ATTR ;

            begin (* ASSIGNMENT *)
              SELECTOR ( FSYS + [ BECOMES ] , FCP ) ;
              if SY = BECOMES then
                begin
                  if GATTR . TYPTR <> NIL then
                    if ( GATTR . ACCESS <> DRCT ) or ( GATTR . TYPTR ->
                    . FORM > POWER ) then
                      LOADADDRESS ;
                  LATTR := GATTR ;
                  INSYMBOL ;
                  EXPRESSION ( FSYS ) ;
                  if GATTR . TYPTR <> NIL then
                    if GATTR . TYPTR -> . FORM <= POWER then
                      LOAD
                    else
                      LOADADDRESS ;
                  if ( LATTR . TYPTR <> NIL ) and ( GATTR . TYPTR <>
                  NIL ) then
                    begin
                      if COMPTYPES ( REALPTR , LATTR . TYPTR ) and (
                      GATTR . TYPTR = INTPTR ) then
                        begin
                          GEN0 ( 10  (*flt*) ) ;
                          GATTR . TYPTR := REALPTR
                        end (* then *) ;
                      if COMPTYPES ( LATTR . TYPTR , GATTR . TYPTR )
                      then
                        begin
                          if FILECOMPONENT ( GATTR . TYPTR ) then
                            ERROR ( 191 ) ;
                          case LATTR . TYPTR -> . FORM of
                            SCALAR , SUBRANGE :
                              begin
                                if DEBUG then
                                  CHECKBNDS ( LATTR . TYPTR ) ;
                                STORE ( LATTR )
                              end (* tag/ca *) ;
                            POINTER :
                              begin
                                if DEBUG then
                                  GEN2T ( 45  (*chk*) , 0 , MAXADDR ,
                                          NILPTR ) ;
                                STORE ( LATTR )
                              end (* tag/ca *) ;
                            POWER : STORE ( LATTR ) ;
                            ARRAYS , RECORDS :
                              GEN1 ( 40  (*mov*) , LATTR . TYPTR -> .
                                     SIZE ) ;
                            FILES : ERROR ( 146 )
                          end (* case *)
                        end (* then *)
                      else
                        ERROR ( 129 )
                    end (* then *)
                end (* then *)
              else
                ERROR ( 51 )
            end (* ASSIGNMENT *) ;


         procedure GOTOSTATEMENT ;

            var LLP : LBP ;
                TTOP , TTOP1 : DISPRANGE ;

            begin (* GOTOSTATEMENT *)
              if SY = INTCONST then
                begin
                  TTOP := TOP ;
                  while DISPLAY [ TTOP ] . OCCUR <> BLCK do
                    TTOP := TTOP - 1 ;
                  TTOP1 := TTOP ;
                  repeat
                    SEARCHLABEL ( LLP , TTOP ) ; (* find label *)
                    if LLP <> NIL then
                      with LLP -> do
                        begin
                          if DEFINED then
                            if SLEVEL > STALVL then

              (************************************************************************)
              (* defining point level greater than                                    *)
              (*                                              present statement level *)
              (************************************************************************)

                              ERROR ( 185 )

              (*******************************************)
              (* goto references deeper nested statement *)
              (*******************************************)

                            else
                              if ( SLEVEL > 1 ) and not BACT then
                                ERROR ( 187 ) ;

              (************************************************)
              (* Goto references label in different nested    *)
              (*                                    statement *)
              (************************************************)


              (************************************************************)
              (* establish the minimum statement level a goto appeared at *)
              (************************************************************)

                          if MINLVL > STALVL then
                            MINLVL := STALVL ;
                          if TTOP = TTOP1 then
                            GENUJPXJP ( 57  (*ujp*) , LABNAME )
                          else
                            begin (* interprocedural goto *)
                              GENIPJ ( 66  (*ipj*) , LEVEL - VLEVEL ,
                                       LABNAME ) ;
                              IPCREF := TRUE
                            end (* else *)
                        end (* with *) ;
                    TTOP := TTOP - 1
                  until ( LLP <> NIL ) or ( TTOP = 0 ) ;
                  if LLP = NIL then
                    begin
                      ERROR ( 167 ) ; (* undeclared label *)
                      NEWLABEL ( LLP )

              (*****************************************)
              (* create dummy label in current context *)
              (*****************************************)

                    end (* then *) ;
                  INSYMBOL
                end (* then *)
              else
                ERROR ( 15 )
            end (* GOTOSTATEMENT *) ;


         procedure COMPOUNDSTATEMENT ;

            var TEST : BOOLEAN ;

            begin (* COMPOUNDSTATEMENT *)
              ADDLVL ;
              repeat
                repeat
                  STATEMENT ( FSYS + [ SEMICOLON , ENDSY ] )
                until not ( SY in STATBEGSYS ) ;
                TEST := SY <> SEMICOLON ;
                if not TEST then
                  INSYMBOL
              until TEST ;
              if SY = ENDSY then
                INSYMBOL
              else
                ERROR ( 13 ) ;
              SUBLVL
            end (* COMPOUNDSTATEMENT *) ;


         procedure IFSTATEMENT ;

            var LCIX1 , LCIX2 : INTEGER ;

            begin (* IFSTATEMENT *)
              EXPRESSION ( FSYS + [ THENSY ] ) ;
              GENLABEL ( LCIX1 ) ;
              GENFJP ( LCIX1 ) ;
              if SY = THENSY then
                INSYMBOL
              else
                ERROR ( 52 ) ;
              ADDLVL ;
              STATEMENT ( FSYS + [ ELSESY ] ) ;
              SUBLVL ;
              if SY = ELSESY then
                begin
                  GENLABEL ( LCIX2 ) ;
                  GENUJPXJP ( 57  (*ujp*) , LCIX2 ) ;
                  PUTLABEL ( LCIX1 ) ;
                  INSYMBOL ;
                  ADDLVL ;
                  STATEMENT ( FSYS ) ;
                  SUBLVL ;
                  PUTLABEL ( LCIX2 )
                end (* then *)
              else
                PUTLABEL ( LCIX1 )
            end (* IFSTATEMENT *) ;


         procedure CASESTATEMENT ;

            label 1 ;

            var LSP , LSP1 : STP ;
                FSTPTR , LPT1 , LPT2 , LPT3 : CIP ;
                LVAL : VALU ;
                LADDR , LCIX , LCIX1 , LMIN , LMAX : INTEGER ;
                TEST : BOOLEAN ;

            begin (* CASESTATEMENT *)
              EXPRESSION ( FSYS + [ OFSY , COMMA , COLON ] ) ;
              LOAD ;
              GENLABEL ( LCIX ) ;
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
                    GEN0T ( 58  (*ord*) , LSP ) ;
              GENUJPXJP ( 57  (*ujp*) , LCIX ) ;
              if SY = OFSY then
                INSYMBOL
              else
                ERROR ( 8 ) ;
              FSTPTR := NIL ;
              GENLABEL ( LADDR ) ;
              repeat
                LPT3 := NIL ;
                GENLABEL ( LCIX1 ) ;
                if not ( SY in [ SEMICOLON , ENDSY ] ) then
                  begin
                    repeat
                      CONSTANT ( FSYS + [ COMMA , COLON ] , LSP1 , LVAL
                                 ) ;
                      if LSP <> NIL then
                        if COMPTYPES ( LSP , LSP1 ) then
                          begin
                            LPT1 := FSTPTR ;
                            LPT2 := NIL ;
                            while LPT1 <> NIL do
                              with LPT1 -> do
                                begin
                                  if CSLAB <= LVAL . IVAL then
                                    begin
                                      if CSLAB = LVAL . IVAL then
                                        ERROR ( 156 ) ;
                                      goto 1
                                    end (* then *) ;
                                  LPT2 := LPT1 ;
                                  LPT1 := NEXT
                                end (* with *) ;
                            1 :
                            GETCAS ( LPT3 ) ;
                            with LPT3 -> do
                              begin
                                NEXT := LPT1 ;
                                CSLAB := LVAL . IVAL ;
                                CSSTART := LCIX1
                              end (* with *) ;
                            if LPT2 = NIL then
                              FSTPTR := LPT3
                            else
                              LPT2 -> . NEXT := LPT3
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
                      ERROR ( 5 ) ;
                    PUTLABEL ( LCIX1 ) ;
                    repeat
                      ADDLVL ;
                      STATEMENT ( FSYS + [ SEMICOLON ] ) ;
                      SUBLVL
                    until not ( SY in STATBEGSYS ) ;
                    if LPT3 <> NIL then
                      GENUJPXJP ( 57  (*ujp*) , LADDR ) ;
                  end (* then *) ;
                TEST := SY <> SEMICOLON ;
                if not TEST then
                  INSYMBOL
              until TEST ;
              PUTLABEL ( LCIX ) ;
              if FSTPTR <> NIL then
                begin
                  LMAX := FSTPTR -> . CSLAB ;

              (******************)
              (*reverse pointers*)
              (******************)

                  LPT1 := FSTPTR ;
                  FSTPTR := NIL ;
                  repeat
                    LPT2 := LPT1 -> . NEXT ;
                    LPT1 -> . NEXT := FSTPTR ;
                    FSTPTR := LPT1 ;
                    LPT1 := LPT2
                  until LPT1 = NIL ;
                  LMIN := FSTPTR -> . CSLAB ;
                  if LMAX - LMIN < CIXMAX then
                    begin
                      GEN2T ( 45  (*chk*) , LMIN , LMAX , INTPTR ) ;
                      GEN2 ( 51  (*ldc*) , 1 , LMIN ) ;
                      GEN0 ( 21  (*sbi*) ) ;
                      GENLABEL ( LCIX ) ;
                      GENUJPXJP ( 44  (*xjp*) , LCIX ) ;
                      PUTLABEL ( LCIX ) ;
                      repeat
                        with FSTPTR -> do
                          begin
                            while CSLAB > LMIN do
                              begin
                                GEN0 ( 60  (*ujc error*) ) ;
                                LMIN := LMIN + 1
                              end (* while *) ;
                            GENUJPXJP ( 57  (*ujp*) , CSSTART ) ;
                            LPT1 := FSTPTR ;
                            FSTPTR := NEXT ;
                            LMIN := LMIN + 1 ;
                            PUTCAS ( LPT1 ) ;
                          end (* with *)
                      until FSTPTR = NIL ;
                      PUTLABEL ( LADDR )
                    end (* then *)
                  else
                    begin
                      ERROR ( 157 ) ;
                      repeat
                        with FSTPTR -> do
                          begin
                            LPT1 := FSTPTR ;
                            FSTPTR := NEXT ;
                            PUTCAS ( LPT1 ) ;
                          end (* with *)
                      until FSTPTR = NIL
                    end (* else *)
                end (* then *) ;
              if SY = ENDSY then
                INSYMBOL
              else
                ERROR ( 13 )
            end (* CASESTATEMENT *) ;


         procedure REPEATSTATEMENT ;

            var LADDR : INTEGER ;

            begin (* REPEATSTATEMENT *)
              GENLABEL ( LADDR ) ;
              PUTLABEL ( LADDR ) ;
              repeat
                ADDLVL ;
                STATEMENT ( FSYS + [ SEMICOLON , UNTILSY ] ) ;
                SUBLVL ;
                if SY in STATBEGSYS then
                  ERROR ( 14 )
              until not ( SY in STATBEGSYS ) ;
              while SY = SEMICOLON do
                begin
                  INSYMBOL ;
                  repeat
                    ADDLVL ;
                    STATEMENT ( FSYS + [ SEMICOLON , UNTILSY ] ) ;
                    if SY in STATBEGSYS then
                      ERROR ( 14 ) ;
                    SUBLVL
                  until not ( SY in STATBEGSYS ) ;
                end (* while *) ;
              if SY = UNTILSY then
                begin
                  INSYMBOL ;
                  EXPRESSION ( FSYS ) ;
                  GENFJP ( LADDR )
                end (* then *)
              else
                ERROR ( 53 ) ;
            end (* REPEATSTATEMENT *) ;


         procedure WHILESTATEMENT ;

            var LADDR , LCIX : INTEGER ;

            begin (* WHILESTATEMENT *)
              GENLABEL ( LADDR ) ;
              PUTLABEL ( LADDR ) ;
              EXPRESSION ( FSYS + [ DOSY ] ) ;
              GENLABEL ( LCIX ) ;
              GENFJP ( LCIX ) ;
              if SY = DOSY then
                INSYMBOL
              else
                ERROR ( 54 ) ;
              ADDLVL ;
              STATEMENT ( FSYS ) ;
              SUBLVL ;
              GENUJPXJP ( 57  (*ujp*) , LADDR ) ;
              PUTLABEL ( LCIX )
            end (* WHILESTATEMENT *) ;


         procedure FORSTATEMENT ;

            var LATTR : ATTR ;
                LSY : SYMBOL ;
                LCIX , LADDR : INTEGER ;
                LLC , LCS : ADDRRANGE ;
                TYPIND : CHAR ; (* added for typing [sam] *)

            begin (* FORSTATEMENT *)
              LLC := LC ;
              with LATTR do
                begin
                  TYPTR := NIL ;
                  KIND := VARBL ;
                  ACCESS := DRCT ;
                  VLEVEL := LEVEL ;
                  DPLMT := 0
                end (* with *) ;
              TYPIND := 'i' ; (* default to integer [sam] *)
              if SY = IDENT then
                begin
                  SEARCHID ( [ VARS ] , LCP ) ;
                  with LCP -> , LATTR do
                    begin
                      TYPTR := IDTYPE ;
                      KIND := VARBL ;
                      if VKIND = ACTUAL then
                        begin
                          ACCESS := DRCT ;
                          VLEVEL := VLEV ;
                          if VLEV <> LEVEL then
                            ERROR ( 183 ) ;
                          DPLMT := VADDR
                        end (* then *)
                      else
                        begin
                          ERROR ( 155 ) ;
                          TYPTR := NIL
                        end (* else *)
                    end (* with *) ;

              (********************************************)
              (* determine type of control variable [sam] *)
              (********************************************)

                  if LATTR . TYPTR = BOOLPTR then
                    TYPIND := 'b'
                  else
                    if LATTR . TYPTR = CHARPTR then
                      TYPIND := 'c' ;
                  if LATTR . TYPTR <> NIL then
                    if ( LATTR . TYPTR -> . FORM > SUBRANGE ) or
                    COMPTYPES ( REALPTR , LATTR . TYPTR ) then
                      begin
                        ERROR ( 143 ) ;
                        LATTR . TYPTR := NIL
                      end (* then *) ;
                  INSYMBOL
                end (* then *)
              else
                begin
                  ERROR ( 2 ) ;
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
                          LOAD ;
                          ALIGN ( INTPTR , LC ) ;

              (***********************)
              (* store start to temp *)
              (***********************)

                          GEN2T ( 56  (*str*) , 0 , LC , INTPTR ) ;
                        end (* then *)
                      else
                        ERROR ( 145 )
                end (* then *)
              else
                begin
                  ERROR ( 51 ) ;
                  SKIP ( FSYS + [ TOSY , DOWNTOSY , DOSY ] )
                end (* else *) ;
              if SY in [ TOSY , DOWNTOSY ] then
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
                          LOAD ;
                          ALIGN ( INTPTR , LC ) ;
                          if not COMPTYPES ( LATTR . TYPTR , INTPTR )
                          then
                            GEN0T ( 58  (*ord*) , GATTR . TYPTR ) ;
                          GEN2T ( 56  (*str*) , 0 , LC + INTSIZE ,
                                  INTPTR ) ;

              (******************************)
              (* set initial value of index *)
              (******************************)

                          GEN2T ( 54  (*lod*) , 0 , LC , INTPTR ) ;
                          STORE ( LATTR ) ;
                          GENLABEL ( LADDR ) ;
                          PUTLABEL ( LADDR ) ;
                          GATTR := LATTR ;
                          LOAD ;
                          if not COMPTYPES ( GATTR . TYPTR , INTPTR )
                          then
                            GEN0T ( 58  (*ord*) , GATTR . TYPTR ) ;
                          GEN2T ( 54  (*lod*) , 0 , LC + INTSIZE ,
                                  INTPTR ) ;
                          LCS := LC ;
                          LC := LC + INTSIZE + INTSIZE ;
                          if LC > LCMAX then
                            LCMAX := LC ;
                          if LSY = TOSY then
                            GEN2 ( 52  (*leq*) , ORD ( TYPIND ) , 1 )
                          else
                            GEN2 ( 48  (*geq*) , ORD ( TYPIND ) , 1 ) ;
                        end (* then *)
                      else
                        ERROR ( 145 )
                end (* then *)
              else
                begin
                  ERROR ( 55 ) ;
                  SKIP ( FSYS + [ DOSY ] )
                end (* else *) ;
              GENLABEL ( LCIX ) ;
              GENUJPXJP ( 33  (*fjp*) , LCIX ) ;
              if SY = DOSY then
                INSYMBOL
              else
                ERROR ( 54 ) ;
              ADDLVL ;
              STATEMENT ( FSYS ) ;
              SUBLVL ;
              GATTR := LATTR ;
              LOAD ;
              if not COMPTYPES ( GATTR . TYPTR , INTPTR ) then
                GEN0T ( 58  (*ord*) , GATTR . TYPTR ) ;
              GEN2T ( 54  (*lod*) , 0 , LCS + INTSIZE , INTPTR ) ;
              GEN2 ( 47  (*equ*) , ORD ( TYPIND ) , 1 ) ;
              GENUJPXJP ( 73  (*tjp*) , LCIX ) ;
              GATTR := LATTR ;
              LOAD ;
              if LSY = TOSY then
                GEN1T ( 34  (*inc*) , 1 , GATTR . TYPTR )
              else
                GEN1T ( 31  (*dec*) , 1 , GATTR . TYPTR ) ;
              STORE ( LATTR ) ;
              GENUJPXJP ( 57  (*ujp*) , LADDR ) ;
              PUTLABEL ( LCIX ) ;
              LC := LLC ;
            end (* FORSTATEMENT *) ;


         procedure WITHSTATEMENT ;

            var LCP : CTP ;
                LCNT1 : DISPRANGE ;
                LLC : ADDRRANGE ;
                TEST : BOOLEAN ;

            begin (* WITHSTATEMENT *)
              LCNT1 := 0 ;
              LLC := LC ;
              repeat
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
                if GATTR . TYPTR <> NIL then
                  if GATTR . TYPTR -> . FORM = RECORDS then
                    if TOP < DISPLIMIT then
                      begin
                        TOP := TOP + 1 ;
                        LCNT1 := LCNT1 + 1 ;
                        with DISPLAY [ TOP ] do
                          begin
                            FNAME := GATTR . TYPTR -> . FSTFLD ;
                            FLABEL := NIL ;
                            FLABEL := NIL ;
                            FCONST := NIL ;
                            FSTRUCT := NIL ;
                          end (* with *) ;
                        if GATTR . ACCESS = DRCT then
                          with DISPLAY [ TOP ] do
                            begin
                              OCCUR := CREC ;
                              CLEV := GATTR . VLEVEL ;
                              CDSPL := GATTR . DPLMT
                            end (* with *)
                        else
                          begin
                            LOADADDRESS ;
                            ALIGN ( NILPTR , LC ) ;
                            GEN2T ( 56  (*str*) , 0 , LC , NILPTR ) ;
                            with DISPLAY [ TOP ] do
                              begin
                                OCCUR := VREC ;
                                VDSPL := LC
                              end (* with *) ;
                            LC := LC + PTRSIZE ;
                            if LC > LCMAX then
                              LCMAX := LC
                          end (* else *)
                      end (* then *)
                    else
                      ERROR ( 250 )
                  else
                    ERROR ( 140 ) ;
                TEST := SY <> COMMA ;
                if not TEST then
                  INSYMBOL
              until TEST ;
              if SY = DOSY then
                INSYMBOL
              else
                ERROR ( 54 ) ;
              ADDLVL ;
              STATEMENT ( FSYS ) ;
              SUBLVL ;

              (************************)
              (* purge display levels *)
              (************************)

              while LCNT1 > 0 do
                begin

              (************************************)
              (* don't recycle the record context *)
              (************************************)

                  DISPLAY [ TOP ] . FNAME := NIL ;
                  PUTDSP ( TOP ) ; (* purge *)
                  TOP := TOP - 1 ;
                  LCNT1 := LCNT1 - 1 ; (* count off *)
                end (* while *) ;
              LC := LLC ;
            end (* WITHSTATEMENT *) ;


         begin (* STATEMENT *)
           if SY = INTCONST then (*label*)
             begin
               SEARCHLABEL ( LLP , LEVEL ) ; (* search label *)
               if LLP <> NIL then
                 with LLP -> do
                   begin                    (* found *)
                     if DEFINED then
                       ERROR ( 165 ) ;      (* multidefined label *)
                     BACT := TRUE ;         (* set in active block now *)
                     SLEVEL := STALVL ;     (* establish statement level *)
                     DEFINED := TRUE ;      (* set defined *)
                     if IPCREF and ( STALVL > 1 ) then
                       ERROR ( 184 )

           (*******************************************************)
           (* intraprocedure goto does not reference outter block *)
           (*******************************************************)

                     else
                       if MINLVL < STALVL then
                         ERROR ( 186 ) ;

           (******************************************************)
           (* label referenced by goto at lesser statement level *)
           (******************************************************)

                     PUTLABEL ( LABNAME ) ;

           (********************************)
           (* output label to intermediate *)
           (********************************)

                   end (* with *)
               else
                 begin    (* not found *)
                   ERROR ( 167 ) ; (* undeclared label *)
                   NEWLABEL ( LLP ) (* create a dummy level *)
                 end (* else *) ;
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
                     INSYMBOL ;
                     COMPOUNDSTATEMENT
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
                     INSYMBOL ;
                     CASESTATEMENT
                   end (* tag/ca *) ;
                 WHILESY :
                   begin
                     INSYMBOL ;
                     WHILESTATEMENT
                   end (* tag/ca *) ;
                 REPEATSY :
                   begin
                     INSYMBOL ;
                     REPEATSTATEMENT
                   end (* tag/ca *) ;
                 FORSY : begin
                           INSYMBOL ;
                           FORSTATEMENT
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


      begin (* BODY *)
        if FPROCP <> NIL then
          ENTNAME := FPROCP -> . PFNAME
        else
          GENLABEL ( ENTNAME ) ;
        CSTPTRIX := 0 ;
        TOPNEW := LCAFTERMARKSTACK ;
        TOPMAX := LCAFTERMARKSTACK ;
        PUTLABEL ( ENTNAME ) ;
        GENLABEL ( SEGSIZE ) ;
        GENLABEL ( STACKTOP ) ;
        GENCUPENT ( 32  (*ent1*) , 1 , SEGSIZE ) ;
        GENCUPENT ( 32  (*ent2*) , 2 , STACKTOP ) ;
        if FPROCP <> NIL then (*copy multiple values into local cells*)
          begin
            LLC1 := LCAFTERMARKSTACK ;
            LCP := FPROCP -> . PFLIST ;
            while LCP <> NIL do
              with LCP -> do
                begin
                  ALIGN ( PARMPTR , LLC1 ) ;
                  if KLASS = VARS then
                    if IDTYPE <> NIL then
                      if IDTYPE -> . FORM > POWER then
                        begin
                          if VKIND = ACTUAL then
                            begin
                              GEN2 ( 50  (*lda*) , 0 , VADDR ) ;
                              GEN2T ( 54  (*lod*) , 0 , LLC1 , NILPTR )
                                      ;
                              GEN1 ( 40  (*mov*) , IDTYPE -> . SIZE ) ;
                            end (* then *) ;
                          LLC1 := LLC1 + PTRSIZE
                        end (* then *)
                      else
                        LLC1 := LLC1 + IDTYPE -> . SIZE ;
                  LCP := LCP -> . NEXT ;
                end (* with *) ;
          end (* then *) ;
        LCMAX := LC ;
        ADDLVL ;
        repeat
          repeat
            STATEMENT ( FSYS + [ SEMICOLON , ENDSY ] )
          until not ( SY in STATBEGSYS ) ;
          TEST := SY <> SEMICOLON ;
          if not TEST then
            INSYMBOL
        until TEST ;
        SUBLVL ;
        if SY = ENDSY then
          INSYMBOL
        else
          ERROR ( 13 ) ;
        LLP := DISPLAY [ TOP ] . FLABEL ; (*test for undefined labels*)
        while LLP <> NIL do
          with LLP -> do
            begin
              if not DEFINED then
                begin
                  ERROR ( 168 ) ;
                  WRITELN ( OUTPUT ) ;
                  WRITELN ( OUTPUT , ' label ' , LABVAL ) ;
                  WRITE ( OUTPUT , ' ' : CHCNT + 16 )
                end (* then *) ;
              LLP := NEXTLAB
            end (* with *) ;
        if FPROCP <> NIL then
          begin
            if FPROCP -> . IDTYPE = NIL then
              GEN1 ( 42  (*ret*) , ORD ( 'p' ) )
            else
              GEN0T ( 42  (*ret*) , FPROCP -> . IDTYPE ) ;
            ALIGN ( PARMPTR , LCMAX ) ;
            if PRCODE then
              begin
                WRITELN ( PRR , 'l' , SEGSIZE : 4 , '=' , LCMAX ) ;
                WRITELN ( PRR , 'l' , STACKTOP : 4 , '=' , TOPMAX )
              end (* then *)
          end (* then *)
        else
          begin
            GEN1 ( 42  (*ret*) , ORD ( 'p' ) ) ;
            ALIGN ( PARMPTR , LCMAX ) ;
            if PRCODE then
              begin
                WRITELN ( PRR , 'l' , SEGSIZE : 4 , '=' , LCMAX ) ;
                WRITELN ( PRR , 'l' , STACKTOP : 4 , '=' , TOPMAX ) ;
                WRITELN ( PRR , 'q' )
              end (* then *) ;
            IC := 0 ;

        (*******************************************************************)
        (*generate call of main program; note that this call must be loaded*)
        (*            at absolute address zero                             *)
        (*******************************************************************)

            GEN1 ( 41  (*mst*) , 0 ) ;
            GENCUPENT ( 46  (*cup*) , 0 , ENTNAME ) ;
            GEN0 ( 29  (*stp*) ) ;
            if PRCODE then
              WRITELN ( PRR , 'q' ) ;
            SAVEID := ID ;
            while FEXTFILEP <> NIL do
              begin
                with FEXTFILEP -> do
                  if not ( STREQURI ( 'input    ' , FILENAME ) or
                  STREQURI ( 'output   ' , FILENAME ) or STREQURI (
                  'prd      ' , FILENAME ) or STREQURI ( 'prr      ' ,
                  FILENAME ) ) then
                    begin
                      ID := FILENAME ;

        (****************************************************)
        (* output general error for undefined external file *)
        (****************************************************)

                      WRITELN ( OUTPUT ) ;
                      WRITELN ( OUTPUT ,
                                '**** Error: external file unknown '''
                                , FEXTFILEP -> . FILENAME : 8 , '''' )
                                ;
                      TOTERR := TOTERR + 1 ;

        (***************************************************************)
        (* hold the error in case not found, since this error          *)
        (*                         occurs far from the original symbol *)
        (***************************************************************)

                      SEARCHIDNE ( [ VARS ] , LLCP ) ;
                      if LLCP = NIL then
                        begin

        (******************************************************)
        (* a header file was never defined in a var statement *)
        (******************************************************)

                          WRITELN ( OUTPUT ) ;
                          WRITELN ( OUTPUT ,
                             '**** Error: Undeclared external file '''
                                    , FEXTFILEP -> . FILENAME : 8 ,
                                    '''' ) ;
                          TOTERR := TOTERR + 1 ;
                          LLCP := UVARPTR
                        end (* then *) ;
                      if LLCP -> . IDTYPE <> NIL then
                        if LLCP -> . IDTYPE -> . FORM <> FILES then
                          begin
                            WRITELN ( OUTPUT ) ;
                            WRITELN ( OUTPUT ,
                             '**** Error: Undeclared external file '''
                                      , FEXTFILEP -> . FILENAME : 8 ,
                                      '''' ) ;
                            TOTERR := TOTERR + 1
                          end (* then *)
                    end (* then *) ;
                FP := FEXTFILEP ;
                FEXTFILEP := FEXTFILEP -> . NEXTFILE ;
                PUTFIL ( FP ) ;
              end (* while *) ;
            ID := SAVEID ;
            if PRTABLES then
              begin
                WRITELN ( OUTPUT ) ;
                PRINTTABLES ( TRUE )
              end (* then *)
          end (* else *) ;
      end (* BODY *) ;


   begin (* BLOCK *)
     STALVL := 0 ; (* clear statement nesting level *)
     DP := TRUE ;
     repeat
       if SY = LABELSY then
         begin
           INSYMBOL ;
           LABELDECLARATION
         end (* then *) ;
       if SY = CONSTSY then
         begin
           INSYMBOL ;
           CONSTDECLARATION
         end (* then *) ;
       if SY = TYPESY then
         begin
           INSYMBOL ;
           TYPEDECLARATION
         end (* then *) ;
       if SY = VARSY then
         begin
           INSYMBOL ;
           VARDECLARATION
         end (* then *) ;
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
     until ( SY in STATBEGSYS ) or EOF ( INPUT ) ;
     DP := FALSE ;
     if SY = BEGINSY then
       INSYMBOL
     else
       ERROR ( 17 ) ;
     repeat
       BODY ( FSYS + [ CASESY ] ) ;
       if SY <> FSY then
         begin
           ERROR ( 6 ) ;
           SKIP ( FSYS )
         end (* then *)
     until ( ( SY = FSY ) or ( SY in BLOCKBEGSYS ) ) or EOF ( INPUT ) ;
   end (* BLOCK *) ;



procedure PROGRAMME ( FSYS : SETOFSYS ) ;

   var EXTFP : EXTFILEP ;

   begin (* PROGRAMME *)
     if SY = PROGSY then
       begin
         INSYMBOL ;
         if SY <> IDENT then
           ERROR ( 2 ) ;
         INSYMBOL ;
         if not ( SY in [ LPARENT , SEMICOLON ] ) then
           ERROR ( 14 ) ;
         if SY = LPARENT then
           begin
             repeat
               INSYMBOL ;
               if SY = IDENT then
                 begin
                   GETFIL ( EXTFP ) ;
                   with EXTFP -> do
                     begin
                       FILENAME := ID ;
                       NEXTFILE := FEXTFILEP
                     end (* with *) ;
                   FEXTFILEP := EXTFP ;

     (************************************************************)
     (* check 'input' or 'output' appears in header for defaults *)
     (************************************************************)

                   if STREQURI ( 'input    ' , ID ) then
                     INPUTHDF := TRUE
                   else
                     if STREQURI ( 'output   ' , ID ) then
                       OUTPUTHDF := TRUE ;
                   INSYMBOL ;
                   if not ( SY in [ COMMA , RPARENT ] ) then
                     ERROR ( 20 )
                 end (* then *)
               else
                 ERROR ( 2 )
             until SY <> COMMA ;
             if SY <> RPARENT then
               ERROR ( 4 ) ;
             INSYMBOL
           end (* then *) ;
         if SY <> SEMICOLON then
           ERROR ( 14 )
         else
           INSYMBOL ;
       end (* then *)
     else
       ERROR ( 3 ) ;
     repeat
       BLOCK ( FSYS , PERIOD , NIL ) ;
       if SY <> PERIOD then
         ERROR ( 21 )
     until ( SY = PERIOD ) or EOF ( INPUT ) ;
     if LIST then
       WRITELN ( OUTPUT ) ;
     if ERRINX <> 0 then
       begin
         LIST := FALSE ;
         ENDOFLINE
       end (* then *) ;
   end (* PROGRAMME *) ;



procedure STDNAMES ;

   begin (* STDNAMES *)
     NA [ 1 ] := 'false    ' ;
     NA [ 2 ] := 'true     ' ;
     NA [ 3 ] := 'input    ' ;
     NA [ 4 ] := 'output   ' ;
     NA [ 5 ] := 'get      ' ;
     NA [ 6 ] := 'put      ' ;
     NA [ 7 ] := 'reset    ' ;
     NA [ 8 ] := 'rewrite  ' ;
     NA [ 9 ] := 'read     ' ;
     NA [ 10 ] := 'write    ' ;
     NA [ 11 ] := 'pack     ' ;
     NA [ 12 ] := 'unpack   ' ;
     NA [ 13 ] := 'new      ' ;
     NA [ 14 ] := '---      ' ;
     NA [ 15 ] := 'readln   ' ;
     NA [ 16 ] := 'writeln  ' ;
     NA [ 17 ] := 'abs      ' ;
     NA [ 18 ] := 'sqr      ' ;
     NA [ 19 ] := 'trunc    ' ;
     NA [ 20 ] := 'odd      ' ;
     NA [ 21 ] := 'ord      ' ;
     NA [ 22 ] := 'chr      ' ;
     NA [ 23 ] := 'pred     ' ;
     NA [ 24 ] := 'succ     ' ;
     NA [ 25 ] := 'eof      ' ;
     NA [ 26 ] := 'eoln     ' ;
     NA [ 27 ] := 'sin      ' ;
     NA [ 28 ] := 'cos      ' ;
     NA [ 29 ] := 'exp      ' ;
     NA [ 30 ] := 'sqrt     ' ;
     NA [ 31 ] := 'ln       ' ;
     NA [ 32 ] := 'arctan   ' ;
     NA [ 33 ] := 'prd      ' ;
     NA [ 34 ] := 'prr      ' ;
     NA [ 35 ] := '---      ' ;
     NA [ 36 ] := 'maxint   ' ;
     NA [ 37 ] := 'round    ' ;
     NA [ 38 ] := 'page     ' ;
     NA [ 39 ] := 'dispose  ' ;
   end (* STDNAMES *) ;



procedure ENTERSTDTYPES ;

   begin (* ENTERSTDTYPES *)

     (******************)

     NEW ( INTPTR , SCALAR , STANDARD ) ;
     PSHSTC ( INTPTR ) ;                                       (*integer*)
     with INTPTR -> do
       begin
         SIZE := INTSIZE ;
         FORM := SCALAR ;
         SCALKIND := STANDARD
       end (* with *) ;
     NEW ( REALPTR , SCALAR , STANDARD ) ;
     PSHSTC ( REALPTR ) ;                                      (*real*)
     with REALPTR -> do
       begin
         SIZE := REALSIZE ;
         FORM := SCALAR ;
         SCALKIND := STANDARD
       end (* with *) ;
     NEW ( CHARPTR , SCALAR , STANDARD ) ;
     PSHSTC ( CHARPTR ) ;                                      (*char*)
     with CHARPTR -> do
       begin
         SIZE := CHARSIZE ;
         FORM := SCALAR ;
         SCALKIND := STANDARD
       end (* with *) ;
     NEW ( BOOLPTR , SCALAR , DECLARED ) ;
     PSHSTC ( BOOLPTR ) ;                                      (*boolean*)
     with BOOLPTR -> do
       begin
         SIZE := BOOLSIZE ;
         FORM := SCALAR ;
         SCALKIND := DECLARED
       end (* with *) ;
     NEW ( NILPTR , POINTER ) ;
     PSHSTC ( NILPTR ) ;                                (*nil*)
     with NILPTR -> do
       begin
         ELTYPE := NIL ;
         SIZE := PTRSIZE ;
         FORM := POINTER
       end (* with *) ;

     (*****************************)
     (*for alignment of parameters*)
     (*****************************)

     NEW ( PARMPTR , SCALAR , STANDARD ) ;
     PSHSTC ( PARMPTR ) ;
     with PARMPTR -> do
       begin
         SIZE := PARMSIZE ;
         FORM := SCALAR ;
         SCALKIND := STANDARD
       end (* with *) ;
     NEW ( TEXTPTR , FILES ) ;
     PSHSTC ( TEXTPTR ) ;                               (*text*)
     with TEXTPTR -> do
       begin
         FILTYPE := CHARPTR ;
         SIZE := FILESIZE + CHARSIZE ;
         FORM := FILES
       end (* with *)
   end (* ENTERSTDTYPES *) ;



procedure ENTSTDNAMES ;

   var CP , CP1 : CTP ;
       I : INTEGER ;

   begin (* ENTSTDNAMES *)

     (*******)

     NEW ( CP , TYPES ) ;
     ININAM ( CP ) ;                                          (*integer*)
     with CP -> do
       begin
         STRASSVR ( NAME , 'integer  ' ) ;
         IDTYPE := INTPTR ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;
     NEW ( CP , TYPES ) ;
     ININAM ( CP ) ;                                          (*real*)
     with CP -> do
       begin
         STRASSVR ( NAME , 'real     ' ) ;
         IDTYPE := REALPTR ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;
     NEW ( CP , TYPES ) ;
     ININAM ( CP ) ;                                          (*char*)
     with CP -> do
       begin
         STRASSVR ( NAME , 'char     ' ) ;
         IDTYPE := CHARPTR ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;
     NEW ( CP , TYPES ) ;
     ININAM ( CP ) ;                                          (*boolean*)
     with CP -> do
       begin
         STRASSVR ( NAME , 'boolean  ' ) ;
         IDTYPE := BOOLPTR ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;
     NEW ( CP , TYPES ) ;
     ININAM ( CP ) ;                                          (*text*)
     with CP -> do
       begin
         STRASSVR ( NAME , 'text     ' ) ;
         IDTYPE := TEXTPTR ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;
     CP1 := NIL ;
     for I := 1 to 2 do
       begin
         NEW ( CP , KONST ) ;
         ININAM ( CP ) ;                                      (*false,true*)
         with CP -> do
           begin
             STRASSVR ( NAME , NA [ I ] ) ;
             IDTYPE := BOOLPTR ;
             NEXT := CP1 ;
             VALUES . IVAL := I - 1 ;
             KLASS := KONST
           end (* with *) ;
         ENTERID ( CP ) ;
         CP1 := CP
       end (* for *) ;
     BOOLPTR -> . FCONST := CP ;
     for I := 3 to 4 do
       begin
         NEW ( CP , VARS ) ;
         ININAM ( CP ) ;                                      (*input,output*)
         with CP -> do
           begin
             STRASSVR ( NAME , NA [ I ] ) ;
             IDTYPE := TEXTPTR ;
             KLASS := VARS ;
             VKIND := ACTUAL ;
             NEXT := NIL ;
             VLEV := 1 ;
             VADDR := LCAFTERMARKSTACK + ( I - 3 ) * ( FILESIZE +
                      CHARSIZE ) ;
           end (* with *) ;
         ENTERID ( CP ) ;
         if I = 3 then
           INPUTPTR := CP
         else
           OUTPUTPTR := CP
       end (* for *) ;
     for I := 33 to 34 do
       begin
         NEW ( CP , VARS ) ;
         ININAM ( CP ) ;                                      (*prd,prr files*)
         with CP -> do
           begin
             STRASSVR ( NAME , NA [ I ] ) ;
             IDTYPE := TEXTPTR ;
             KLASS := VARS ;
             VKIND := ACTUAL ;
             NEXT := NIL ;
             VLEV := 1 ;
             VADDR := LCAFTERMARKSTACK + ( I - 31 ) * ( FILESIZE +
                      CHARSIZE ) ;
           end (* with *) ;
         ENTERID ( CP )
       end (* for *) ;
     for I := 5 to 16 do
       if I <> 14 then                 (* no longer doing release *)
         begin
           NEW ( CP , PROC , STANDARD ) ;
           ININAM ( CP ) ;             (*get,put,reset*)
           with CP -> do               (*rewrite,read*)
             begin
               STRASSVR ( NAME , NA [ I ] ) ;
               IDTYPE := NIL ;         (*write,pack*)
               PFLIST := NIL ;
               NEXT := NIL ;
               KEY := I - 4 ;          (*unpack,new*)
               KLASS := PROC ;
               PFDECKIND := STANDARD   (*readln,writeln*)
             end (* with *) ;
           ENTERID ( CP )
         end (* then *) ;
     for I := 17 to 26 do
       begin
         NEW ( CP , FUNC , STANDARD ) ;
         ININAM ( CP ) ;                                       (*abs,sqr,trunc*)
         with CP -> do                                         (*odd,ord,chr*)
           begin
             STRASSVR ( NAME , NA [ I ] ) ;
             IDTYPE := NIL ;                                   (*pred,succ,eof*)
             PFLIST := NIL ;
             NEXT := NIL ;
             KEY := I - 16 ;
             KLASS := FUNC ;
             PFDECKIND := STANDARD
           end (* with *) ;
         ENTERID ( CP )
       end (* for *) ;
     for I := 27 to 32 do
       begin
         NEW ( CP , VARS ) ;
         ININAM ( CP ) ;                                         (*parameter of predeclared functions*)
         with CP -> do
           begin
             STRASSVR ( NAME , '         ' ) ;
             IDTYPE := REALPTR ;
             KLASS := VARS ;
             VKIND := ACTUAL ;
             NEXT := NIL ;
             VLEV := 1 ;
             VADDR := 0
           end (* with *) ;
         NEW ( CP1 , FUNC , DECLARED , ACTUAL ) ;
         ININAM ( CP1 ) ;                                      (*sin,cos,exp*)
         with CP1 -> do                                        (*sqrt,ln,arctan*)
           begin
             STRASSVR ( NAME , NA [ I ] ) ;
             IDTYPE := REALPTR ;
             PFLIST := CP ;
             FORWDECL := FALSE ;
             EXTERNL := TRUE ;
             PFLEV := 0 ;
             PFNAME := I - 12 ;
             KLASS := FUNC ;
             PFDECKIND := DECLARED ;
             PFKIND := ACTUAL
           end (* with *) ;
         ENTERID ( CP1 )
       end (* for *) ;
     NEW ( CP , KONST ) ;
     ININAM ( CP ) ;                                           (*maxint*)
     with CP -> do
       begin
         STRASSVR ( NAME , NA [ 36 ] ) ;
         IDTYPE := INTPTR ;
         NEXT := NIL ;
         VALUES . IVAL := MAXINT ;
         KLASS := KONST
       end (* with *) ;
     ENTERID ( CP ) ;
     NEW ( CP , FUNC , STANDARD ) ;
     ININAM ( CP ) ;                                           (*round*)
     with CP -> do
       begin
         STRASSVR ( NAME , NA [ 37 ] ) ;
         IDTYPE := NIL ;
         PFLIST := NIL ;
         NEXT := NIL ;
         KEY := 16 ;
         KLASS := FUNC ;
         PFDECKIND := STANDARD
       end (* with *) ;
     ENTERID ( CP ) ;
     NEW ( CP , PROC , STANDARD ) ;
     ININAM ( CP ) ;                                           (*page*)
     with CP -> do
       begin
         STRASSVR ( NAME , NA [ 38 ] ) ;
         IDTYPE := NIL ;
         PFLIST := NIL ;
         NEXT := NIL ;
         KEY := 17 ;
         KLASS := PROC ;
         PFDECKIND := STANDARD
       end (* with *) ;
     ENTERID ( CP ) ;
     NEW ( CP , PROC , STANDARD ) ;
     ININAM ( CP ) ;                                           (*dispose*)
     with CP -> do
       begin
         STRASSVR ( NAME , NA [ 39 ] ) ;
         IDTYPE := NIL ;
         PFLIST := NIL ;
         NEXT := NIL ;
         KEY := 18 ;
         KLASS := PROC ;
         PFDECKIND := STANDARD
       end (* with *) ;
     ENTERID ( CP )
   end (* ENTSTDNAMES *) ;



procedure ENTERUNDECL ;

   begin (* ENTERUNDECL *)
     NEW ( UTYPPTR , TYPES ) ;
     ININAM ( UTYPPTR ) ;
     with UTYPPTR -> do
       begin
         STRASSVR ( NAME , '         ' ) ;
         IDTYPE := NIL ;
         KLASS := TYPES
       end (* with *) ;
     NEW ( UCSTPTR , KONST ) ;
     ININAM ( UCSTPTR ) ;
     with UCSTPTR -> do
       begin
         STRASSVR ( NAME , '         ' ) ;
         IDTYPE := NIL ;
         NEXT := NIL ;
         VALUES . IVAL := 0 ;
         KLASS := KONST
       end (* with *) ;
     NEW ( UVARPTR , VARS ) ;
     ININAM ( UVARPTR ) ;
     with UVARPTR -> do
       begin
         STRASSVR ( NAME , '         ' ) ;
         IDTYPE := NIL ;
         VKIND := ACTUAL ;
         NEXT := NIL ;
         VLEV := 0 ;
         VADDR := 0 ;
         KLASS := VARS
       end (* with *) ;
     NEW ( UFLDPTR , FIELD ) ;
     ININAM ( UFLDPTR ) ;
     with UFLDPTR -> do
       begin
         STRASSVR ( NAME , '         ' ) ;
         IDTYPE := NIL ;
         NEXT := NIL ;
         FLDADDR := 0 ;
         KLASS := FIELD
       end (* with *) ;
     NEW ( UPRCPTR , PROC , DECLARED , ACTUAL ) ;
     ININAM ( UPRCPTR ) ;
     with UPRCPTR -> do
       begin
         STRASSVR ( NAME , '         ' ) ;
         IDTYPE := NIL ;
         FORWDECL := FALSE ;
         NEXT := NIL ;
         EXTERNL := FALSE ;
         PFLEV := 0 ;
         GENLABEL ( PFNAME ) ;
         KLASS := PROC ;
         PFLIST := NIL ;
         PFDECKIND := DECLARED ;
         PFKIND := ACTUAL
       end (* with *) ;
     NEW ( UFCTPTR , FUNC , DECLARED , ACTUAL ) ;
     ININAM ( UFCTPTR ) ;
     with UFCTPTR -> do
       begin
         STRASSVR ( NAME , '         ' ) ;
         IDTYPE := NIL ;
         NEXT := NIL ;
         FORWDECL := FALSE ;
         EXTERNL := FALSE ;
         PFLEV := 0 ;
         GENLABEL ( PFNAME ) ;
         KLASS := FUNC ;
         PFLIST := NIL ;
         PFDECKIND := DECLARED ;
         PFKIND := ACTUAL
       end (* with *)
   end (* ENTERUNDECL *) ;





(**************************************************)
(* tear down storage allocations from enterundecl *)
(**************************************************)




procedure EXITUNDECL ;

   begin (* EXITUNDECL *)
     PUTNAM ( UTYPPTR ) ;
     PUTNAM ( UCSTPTR ) ;
     PUTNAM ( UVARPTR ) ;
     PUTNAM ( UFLDPTR ) ;
     PUTNAM ( UPRCPTR ) ;
     PUTNAM ( UFCTPTR ) ;
   end (* EXITUNDECL *) ;



procedure INITSCALARS ;

   var I : INTEGER ;

   begin (* INITSCALARS *)
     FWPTR := NIL ;
     PRTABLES := FALSE ;
     LIST := TRUE ;
     PRCODE := TRUE ;
     DEBUG := TRUE ;
     DP := TRUE ;
     ERRINX := 0 ;
     INTLABEL := 0 ;
     KK := MAXIDS ;
     FEXTFILEP := NIL ;
     LC := LCAFTERMARKSTACK + FILEBUFFER * ( FILESIZE + CHARSIZE ) ;

     (******************************************************************)
     (* note in the above reservation of buffer store for 2 text files *)
     (******************************************************************)

     IC := 3 ;
     EOL := TRUE ;
     LINECOUNT := 0 ;
     CH := ' ' ;
     CHCNT := 0 ;
     MXINT10 := MAXINT DIV 10 ;
     INPUTHDF := FALSE ; (* set 'input' not in header files *)
     OUTPUTHDF := FALSE ; (* set 'output' not in header files *)
     for I := 1 to 500 do
       ERRTBL [ I ] := FALSE ; (* initialize error tracking *)
     TOTERR := 0 ; (* clear error count                     *)
                   (* clear the recycling tracking counters *)
     STRCNT := 0 ; (* strings *)
     CSPCNT := 0 ; (* constants *)
     STPCNT := 0 ; (* structures *)
     CTPCNT := 0 ; (* identifiers *)
     LBPCNT := 0 ; (* label counts *)
     FILCNT := 0 ; (* file tracking counts *)
     CIPCNT := 0   (* case entry tracking counts *)
   end (* INITSCALARS *) ;



procedure INITSETS ;

   begin (* INITSETS *)
     CONSTBEGSYS := [ ADDOP , INTCONST , REALCONST , STRINGCONST ,
                    IDENT ] ;
     SIMPTYPEBEGSYS := [ LPARENT ] + CONSTBEGSYS ;
     TYPEBEGSYS := [ ARROW , PACKEDSY , ARRAYSY , RECORDSY , SETSY ,
                   FILESY ] + SIMPTYPEBEGSYS ;
     TYPEDELS := [ ARRAYSY , RECORDSY , SETSY , FILESY ] ;
     BLOCKBEGSYS := [ LABELSY , CONSTSY , TYPESY , VARSY , PROCSY ,
                    FUNCSY , BEGINSY ] ;
     SELECTSYS := [ ARROW , PERIOD , LBRACK ] ;
     FACBEGSYS := [ INTCONST , REALCONST , STRINGCONST , IDENT ,
                  LPARENT , LBRACK , NOTSY , NILSY ] ;
     STATBEGSYS := [ BEGINSY , GOTOSY , IFSY , WHILESY , REPEATSY ,
                   FORSY , WITHSY , CASESY ] ;
   end (* INITSETS *) ;



procedure INITTABLES ;


   procedure RESWORDS ;

      begin (* RESWORDS *)
        RW [ 1 ] := 'if       ' ;
        RW [ 2 ] := 'do       ' ;
        RW [ 3 ] := 'of       ' ;
        RW [ 4 ] := 'to       ' ;
        RW [ 5 ] := 'in       ' ;
        RW [ 6 ] := 'or       ' ;
        RW [ 7 ] := 'end      ' ;
        RW [ 8 ] := 'for      ' ;
        RW [ 9 ] := 'var      ' ;
        RW [ 10 ] := 'div      ' ;
        RW [ 11 ] := 'mod      ' ;
        RW [ 12 ] := 'set      ' ;
        RW [ 13 ] := 'and      ' ;
        RW [ 14 ] := 'not      ' ;
        RW [ 15 ] := 'nil      ' ;
        RW [ 16 ] := 'then     ' ;
        RW [ 17 ] := 'else     ' ;
        RW [ 18 ] := 'with     ' ;
        RW [ 19 ] := 'goto     ' ;
        RW [ 20 ] := 'case     ' ;
        RW [ 21 ] := 'type     ' ;
        RW [ 22 ] := 'file     ' ;
        RW [ 23 ] := 'begin    ' ;
        RW [ 24 ] := 'until    ' ;
        RW [ 25 ] := 'while    ' ;
        RW [ 26 ] := 'array    ' ;
        RW [ 27 ] := 'const    ' ;
        RW [ 28 ] := 'label    ' ;
        RW [ 29 ] := 'repeat   ' ;
        RW [ 30 ] := 'record   ' ;
        RW [ 31 ] := 'downto   ' ;
        RW [ 32 ] := 'packed   ' ;
        RW [ 33 ] := 'program  ' ;
        RW [ 34 ] := 'function ' ;
        RW [ 35 ] := 'procedure' ;
        FRW [ 1 ] := 1 ;
        FRW [ 2 ] := 1 ;
        FRW [ 3 ] := 7 ;
        FRW [ 4 ] := 16 ;
        FRW [ 5 ] := 23 ;
        FRW [ 6 ] := 29 ;
        FRW [ 7 ] := 33 ;
        FRW [ 8 ] := 34 ;
        FRW [ 9 ] := 35 ;
        FRW [ 10 ] := 36 ;
      end (* RESWORDS *) ;


   procedure SYMBOLS ;

      begin (* SYMBOLS *)
        RSY [ 1 ] := IFSY ;
        RSY [ 2 ] := DOSY ;
        RSY [ 3 ] := OFSY ;
        RSY [ 4 ] := TOSY ;
        RSY [ 5 ] := RELOP ;
        RSY [ 6 ] := ADDOP ;
        RSY [ 7 ] := ENDSY ;
        RSY [ 8 ] := FORSY ;
        RSY [ 9 ] := VARSY ;
        RSY [ 10 ] := MULOP ;
        RSY [ 11 ] := MULOP ;
        RSY [ 12 ] := SETSY ;
        RSY [ 13 ] := MULOP ;
        RSY [ 14 ] := NOTSY ;
        RSY [ 15 ] := NILSY ;
        RSY [ 16 ] := THENSY ;
        RSY [ 17 ] := ELSESY ;
        RSY [ 18 ] := WITHSY ;
        RSY [ 19 ] := GOTOSY ;
        RSY [ 20 ] := CASESY ;
        RSY [ 21 ] := TYPESY ;
        RSY [ 22 ] := FILESY ;
        RSY [ 23 ] := BEGINSY ;
        RSY [ 24 ] := UNTILSY ;
        RSY [ 25 ] := WHILESY ;
        RSY [ 26 ] := ARRAYSY ;
        RSY [ 27 ] := CONSTSY ;
        RSY [ 28 ] := LABELSY ;
        RSY [ 29 ] := REPEATSY ;
        RSY [ 30 ] := RECORDSY ;
        RSY [ 31 ] := DOWNTOSY ;
        RSY [ 32 ] := PACKEDSY ;
        RSY [ 33 ] := PROGSY ;
        RSY [ 34 ] := FUNCSY ;
        RSY [ 35 ] := PROCSY ;
        SSY [ '+' ] := ADDOP ;
        SSY [ '-' ] := ADDOP ;
        SSY [ '*' ] := MULOP ;
        SSY [ '/' ] := MULOP ;
        SSY [ '(' ] := LPARENT ;
        SSY [ ')' ] := RPARENT ;
        SSY [ '$' ] := OTHERSY ;
        SSY [ '=' ] := RELOP ;
        SSY [ ' ' ] := OTHERSY ;
        SSY [ ',' ] := COMMA ;
        SSY [ '.' ] := PERIOD ;
        SSY [ '''' ] := OTHERSY ;
        SSY [ '[' ] := LBRACK ;
        SSY [ ']' ] := RBRACK ;
        SSY [ ':' ] := COLON ;
        SSY [ '^' ] := ARROW ;
        SSY [ '<' ] := RELOP ;
        SSY [ '>' ] := RELOP ;
        SSY [ ';' ] := SEMICOLON ;
        SSY [ '@' ] := ARROW ;
      end (* SYMBOLS *) ;


   procedure RATORS ;

      var I : INTEGER ;

      begin (* RATORS *)
        for I := 1 to MAXRES  (*nr of res words*) do
          ROP [ I ] := NOOP ;
        ROP [ 5 ] := INOP ;
        ROP [ 10 ] := IDIV ;
        ROP [ 11 ] := IMOD ;
        ROP [ 6 ] := OROP ;
        ROP [ 13 ] := ANDOP ;
        for I := ORDMINCHAR to ORDMAXCHAR do
          SOP [ CHR ( I ) ] := NOOP ;
        SOP [ '+' ] := PLUS ;
        SOP [ '-' ] := MINUS ;
        SOP [ '*' ] := MUL ;
        SOP [ '/' ] := RDIV ;
        SOP [ '=' ] := EQOP ;
        SOP [ '<' ] := LTOP ;
        SOP [ '>' ] := GTOP ;
      end (* RATORS *) ;


   procedure PROCMNEMONICS ;

      begin (* PROCMNEMONICS *)

        (**********************************************************************************)
        (*        assembler/interpreter: wro, pak. I didn't find a generator for them, and*)
        (*        suspect they are abandoned.                                             *)
        (**********************************************************************************)

        SNA [ 1 ] := ' get' ;
        SNA [ 2 ] := ' put' ;
        SNA [ 3 ] := ' rdi' ;
        SNA [ 4 ] := ' rdr' ;
        SNA [ 5 ] := ' rdc' ;
        SNA [ 6 ] := ' wri' ;
        SNA [ 7 ] := ' wro' ;
        SNA [ 8 ] := ' wrr' ;
        SNA [ 9 ] := ' wrc' ;
        SNA [ 10 ] := ' wrs' ;
        SNA [ 11 ] := ' pak' ;
        SNA [ 12 ] := ' new' ;
        SNA [ 13 ] := ' rst' ;
        SNA [ 14 ] := ' eln' ;
        SNA [ 15 ] := ' sin' ;
        SNA [ 16 ] := ' cos' ;
        SNA [ 17 ] := ' exp' ;
        SNA [ 18 ] := ' sqt' ;
        SNA [ 19 ] := ' log' ;
        SNA [ 20 ] := ' atn' ;
        SNA [ 21 ] := ' rln' ;
        SNA [ 22 ] := ' wln' ;
        SNA [ 23 ] := ' sav' ;

        (******************************************)
        (* new procedure/function memonics for p5 *)
        (******************************************)

        SNA [ 24 ] := ' pag' ;
        SNA [ 25 ] := ' rsf' ;
        SNA [ 26 ] := ' rwf' ;
        SNA [ 27 ] := ' wrb' ;
        SNA [ 28 ] := ' wrf' ;
        SNA [ 29 ] := ' dsp' ;
        SNA [ 30 ] := ' wbf' ;
        SNA [ 31 ] := ' wbi' ;
        SNA [ 32 ] := ' wbr' ;
        SNA [ 33 ] := ' wbc' ;
        SNA [ 34 ] := ' wbb' ;
        SNA [ 35 ] := ' rbf' ;
        SNA [ 36 ] := ' rsb' ;
        SNA [ 37 ] := ' rwb' ;
        SNA [ 38 ] := ' gbf' ;
        SNA [ 39 ] := ' pbf' ;
      end (* PROCMNEMONICS *) ;


   procedure INSTRMNEMONICS ;

      begin (* INSTRMNEMONICS *)
        MN [ 0 ] := ' abi' ;
        MN [ 1 ] := ' abr' ;
        MN [ 2 ] := ' adi' ;
        MN [ 3 ] := ' adr' ;
        MN [ 4 ] := ' and' ;
        MN [ 5 ] := ' dif' ;
        MN [ 6 ] := ' dvi' ;
        MN [ 7 ] := ' dvr' ;
        MN [ 8 ] := ' eof' ;
        MN [ 9 ] := ' flo' ;
        MN [ 10 ] := ' flt' ;
        MN [ 11 ] := ' inn' ;
        MN [ 12 ] := ' int' ;
        MN [ 13 ] := ' ior' ;
        MN [ 14 ] := ' mod' ;
        MN [ 15 ] := ' mpi' ;
        MN [ 16 ] := ' mpr' ;
        MN [ 17 ] := ' ngi' ;
        MN [ 18 ] := ' ngr' ;
        MN [ 19 ] := ' not' ;
        MN [ 20 ] := ' odd' ;
        MN [ 21 ] := ' sbi' ;
        MN [ 22 ] := ' sbr' ;
        MN [ 23 ] := ' sgs' ;
        MN [ 24 ] := ' sqi' ;
        MN [ 25 ] := ' sqr' ;
        MN [ 26 ] := ' sto' ;
        MN [ 27 ] := ' trc' ;
        MN [ 28 ] := ' uni' ;
        MN [ 29 ] := ' stp' ;
        MN [ 30 ] := ' csp' ;
        MN [ 31 ] := ' dec' ;
        MN [ 32 ] := ' ent' ;
        MN [ 33 ] := ' fjp' ;
        MN [ 34 ] := ' inc' ;
        MN [ 35 ] := ' ind' ;
        MN [ 36 ] := ' ixa' ;
        MN [ 37 ] := ' lao' ;
        MN [ 38 ] := ' lca' ;
        MN [ 39 ] := ' ldo' ;
        MN [ 40 ] := ' mov' ;
        MN [ 41 ] := ' mst' ;
        MN [ 42 ] := ' ret' ;
        MN [ 43 ] := ' sro' ;
        MN [ 44 ] := ' xjp' ;
        MN [ 45 ] := ' chk' ;
        MN [ 46 ] := ' cup' ;
        MN [ 47 ] := ' equ' ;
        MN [ 48 ] := ' geq' ;
        MN [ 49 ] := ' grt' ;
        MN [ 50 ] := ' lda' ;
        MN [ 51 ] := ' ldc' ;
        MN [ 52 ] := ' leq' ;
        MN [ 53 ] := ' les' ;
        MN [ 54 ] := ' lod' ;
        MN [ 55 ] := ' neq' ;
        MN [ 56 ] := ' str' ;
        MN [ 57 ] := ' ujp' ;
        MN [ 58 ] := ' ord' ;
        MN [ 59 ] := ' chr' ;
        MN [ 60 ] := ' ujc' ;

        (***********************************)
        (* new instruction memonics for p5 *)
        (***********************************)

        MN [ 61 ] := ' rnd' ;
        MN [ 62 ] := ' pck' ;
        MN [ 63 ] := ' upk' ;
        MN [ 64 ] := ' rgs' ;
        MN [ 65 ] := ' fbv' ;
        MN [ 66 ] := ' ipj' ;
        MN [ 67 ] := ' cip' ;
        MN [ 68 ] := ' lpa' ;
        MN [ 69 ] := ' efb' ;
        MN [ 70 ] := ' fvb' ;
        MN [ 71 ] := ' dmp' ;
        MN [ 72 ] := ' swp' ;
        MN [ 73 ] := ' tjp' ;
        MN [ 74 ] := ' lip' ;
      end (* INSTRMNEMONICS *) ;


   procedure CHARTYPES ;

      var I : INTEGER ;

      begin (* CHARTYPES *)
        for I := ORDMINCHAR to ORDMAXCHAR do
          CHARTP [ CHR ( I ) ] := ILLEGAL ;
        CHARTP [ 'a' ] := LETTER ;
        CHARTP [ 'b' ] := LETTER ;
        CHARTP [ 'c' ] := LETTER ;
        CHARTP [ 'd' ] := LETTER ;
        CHARTP [ 'e' ] := LETTER ;
        CHARTP [ 'f' ] := LETTER ;
        CHARTP [ 'g' ] := LETTER ;
        CHARTP [ 'h' ] := LETTER ;
        CHARTP [ 'i' ] := LETTER ;
        CHARTP [ 'j' ] := LETTER ;
        CHARTP [ 'k' ] := LETTER ;
        CHARTP [ 'l' ] := LETTER ;
        CHARTP [ 'm' ] := LETTER ;
        CHARTP [ 'n' ] := LETTER ;
        CHARTP [ 'o' ] := LETTER ;
        CHARTP [ 'p' ] := LETTER ;
        CHARTP [ 'q' ] := LETTER ;
        CHARTP [ 'r' ] := LETTER ;
        CHARTP [ 's' ] := LETTER ;
        CHARTP [ 't' ] := LETTER ;
        CHARTP [ 'u' ] := LETTER ;
        CHARTP [ 'v' ] := LETTER ;
        CHARTP [ 'w' ] := LETTER ;
        CHARTP [ 'x' ] := LETTER ;
        CHARTP [ 'y' ] := LETTER ;
        CHARTP [ 'z' ] := LETTER ;
        CHARTP [ 'A' ] := LETTER ;
        CHARTP [ 'B' ] := LETTER ;
        CHARTP [ 'C' ] := LETTER ;
        CHARTP [ 'D' ] := LETTER ;
        CHARTP [ 'E' ] := LETTER ;
        CHARTP [ 'F' ] := LETTER ;
        CHARTP [ 'G' ] := LETTER ;
        CHARTP [ 'H' ] := LETTER ;
        CHARTP [ 'I' ] := LETTER ;
        CHARTP [ 'J' ] := LETTER ;
        CHARTP [ 'K' ] := LETTER ;
        CHARTP [ 'L' ] := LETTER ;
        CHARTP [ 'M' ] := LETTER ;
        CHARTP [ 'N' ] := LETTER ;
        CHARTP [ 'O' ] := LETTER ;
        CHARTP [ 'P' ] := LETTER ;
        CHARTP [ 'Q' ] := LETTER ;
        CHARTP [ 'R' ] := LETTER ;
        CHARTP [ 'S' ] := LETTER ;
        CHARTP [ 'T' ] := LETTER ;
        CHARTP [ 'U' ] := LETTER ;
        CHARTP [ 'V' ] := LETTER ;
        CHARTP [ 'W' ] := LETTER ;
        CHARTP [ 'X' ] := LETTER ;
        CHARTP [ 'Y' ] := LETTER ;
        CHARTP [ 'Z' ] := LETTER ;
        CHARTP [ '0' ] := NUMBER ;
        CHARTP [ '1' ] := NUMBER ;
        CHARTP [ '2' ] := NUMBER ;
        CHARTP [ '3' ] := NUMBER ;
        CHARTP [ '4' ] := NUMBER ;
        CHARTP [ '5' ] := NUMBER ;
        CHARTP [ '6' ] := NUMBER ;
        CHARTP [ '7' ] := NUMBER ;
        CHARTP [ '8' ] := NUMBER ;
        CHARTP [ '9' ] := NUMBER ;
        CHARTP [ '+' ] := SPECIAL ;
        CHARTP [ '-' ] := SPECIAL ;
        CHARTP [ '*' ] := SPECIAL ;
        CHARTP [ '/' ] := SPECIAL ;
        CHARTP [ '(' ] := CHLPAREN ;
        CHARTP [ ')' ] := SPECIAL ;
        CHARTP [ '$' ] := SPECIAL ;
        CHARTP [ '=' ] := SPECIAL ;
        CHARTP [ ' ' ] := CHSPACE ;
        CHARTP [ ',' ] := SPECIAL ;
        CHARTP [ '.' ] := CHPERIOD ;
        CHARTP [ '''' ] := CHSTRQUO ;
        CHARTP [ '[' ] := SPECIAL ;
        CHARTP [ ']' ] := SPECIAL ;
        CHARTP [ ':' ] := CHCOLON ;
        CHARTP [ '^' ] := SPECIAL ;
        CHARTP [ ';' ] := SPECIAL ;
        CHARTP [ '<' ] := CHLT ;
        CHARTP [ '>' ] := CHGT ;
        CHARTP [ '{' ] := CHLCMT ;
        CHARTP [ '}' ] := SPECIAL ;
        CHARTP [ '@' ] := SPECIAL ;
        ORDINT [ '0' ] := 0 ;
        ORDINT [ '1' ] := 1 ;
        ORDINT [ '2' ] := 2 ;
        ORDINT [ '3' ] := 3 ;
        ORDINT [ '4' ] := 4 ;
        ORDINT [ '5' ] := 5 ;
        ORDINT [ '6' ] := 6 ;
        ORDINT [ '7' ] := 7 ;
        ORDINT [ '8' ] := 8 ;
        ORDINT [ '9' ] := 9 ;
      end (* CHARTYPES *) ;


   procedure INITDX ;

      begin (* INITDX *)
        CDX [ 0 ] := 0 ;
        CDX [ 1 ] := 0 ;
        CDX [ 2 ] := - 1 ;
        CDX [ 3 ] := - 1 ;
        CDX [ 4 ] := - 1 ;
        CDX [ 5 ] := - 1 ;
        CDX [ 6 ] := - 1 ;
        CDX [ 7 ] := - 1 ;
        CDX [ 8 ] := 0 ;
        CDX [ 9 ] := 0 ;
        CDX [ 10 ] := 0 ;
        CDX [ 11 ] := - 1 ;
        CDX [ 12 ] := - 1 ;
        CDX [ 13 ] := - 1 ;
        CDX [ 14 ] := - 1 ;
        CDX [ 15 ] := - 1 ;
        CDX [ 16 ] := - 1 ;
        CDX [ 17 ] := 0 ;
        CDX [ 18 ] := 0 ;
        CDX [ 19 ] := 0 ;
        CDX [ 20 ] := 0 ;
        CDX [ 21 ] := - 1 ;
        CDX [ 22 ] := - 1 ;
        CDX [ 23 ] := 0 ;
        CDX [ 24 ] := 0 ;
        CDX [ 25 ] := 0 ;
        CDX [ 26 ] := - 2 ;
        CDX [ 27 ] := 0 ;
        CDX [ 28 ] := - 1 ;
        CDX [ 29 ] := 0 ;
        CDX [ 30 ] := 0 ;
        CDX [ 31 ] := 0 ;
        CDX [ 32 ] := 0 ;
        CDX [ 33 ] := - 1 ;
        CDX [ 34 ] := 0 ;
        CDX [ 35 ] := 0 ;
        CDX [ 36 ] := - 1 ;
        CDX [ 37 ] := + 1 ;
        CDX [ 38 ] := + 1 ;
        CDX [ 39 ] := + 1 ;
        CDX [ 40 ] := - 2 ;
        CDX [ 41 ] := 0 ;
        CDX [ 42 ] := 0 ;
        CDX [ 43 ] := - 1 ;
        CDX [ 44 ] := - 1 ;
        CDX [ 45 ] := 0 ;
        CDX [ 46 ] := 0 ;
        CDX [ 47 ] := - 1 ;
        CDX [ 48 ] := - 1 ;
        CDX [ 49 ] := - 1 ;
        CDX [ 50 ] := + 1 ;
        CDX [ 51 ] := + 1 ;
        CDX [ 52 ] := - 1 ;
        CDX [ 53 ] := - 1 ;
        CDX [ 54 ] := + 1 ;
        CDX [ 55 ] := - 1 ;
        CDX [ 56 ] := - 1 ;
        CDX [ 57 ] := 0 ;
        CDX [ 58 ] := 0 ;
        CDX [ 59 ] := 0 ;
        CDX [ 60 ] := 0 ;
        CDX [ 61 ] := 0 ;
        CDX [ 62 ] := - 3 ;
        CDX [ 63 ] := - 3 ;
        CDX [ 64 ] := - 1 ;
        CDX [ 65 ] := 0 ;
        CDX [ 66 ] := 0 ;
        CDX [ 67 ] := - 1 ;
        CDX [ 68 ] := + 2 ;
        CDX [ 69 ] := 0 ;
        CDX [ 70 ] := - 1 ;
        CDX [ 71 ] := - 1 ;
        CDX [ 72 ] := 0 ;
        CDX [ 73 ] := - 1 ;
        CDX [ 74 ] := + 2 ;
        PDX [ 1 ] := - 1 ;
        PDX [ 2 ] := - 1 ;
        PDX [ 3 ] := - 1 ;
        PDX [ 4 ] := - 1 ;
        PDX [ 5 ] := - 1 ;
        PDX [ 6 ] := - 2 ;
        PDX [ 7 ] := - 3 ;
        PDX [ 8 ] := - 2 ;
        PDX [ 9 ] := - 2 ;
        PDX [ 10 ] := - 3 ;
        PDX [ 11 ] := 0 ;
        PDX [ 12 ] := - 2 ;
        PDX [ 13 ] := - 1 ;
        PDX [ 14 ] := 0 ;
        PDX [ 15 ] := 0 ;
        PDX [ 16 ] := 0 ;
        PDX [ 17 ] := 0 ;
        PDX [ 18 ] := 0 ;
        PDX [ 19 ] := 0 ;
        PDX [ 20 ] := 0 ;
        PDX [ 21 ] := 0 ;
        PDX [ 22 ] := 0 ;
        PDX [ 23 ] := - 1 ;
        PDX [ 24 ] := - 1 ;
        PDX [ 25 ] := - 1 ;
        PDX [ 26 ] := - 1 ;
        PDX [ 27 ] := - 2 ;
        PDX [ 28 ] := - 3 ;
        PDX [ 29 ] := - 2 ;
        PDX [ 30 ] := - 2 ;
        PDX [ 31 ] := - 1 ;
        PDX [ 32 ] := - 1 ;
        PDX [ 33 ] := - 1 ;
        PDX [ 34 ] := - 1 ;
        PDX [ 35 ] := - 2 ;
        PDX [ 36 ] := - 1 ;
        PDX [ 37 ] := - 1 ;
        PDX [ 38 ] := - 2 ;
        PDX [ 39 ] := - 2 ;
      end (* INITDX *) ;


   begin (* INITTABLES *)
     RESWORDS ;
     SYMBOLS ;
     RATORS ;
     INSTRMNEMONICS ;
     PROCMNEMONICS ;
     CHARTYPES ;
     INITDX ;
   end (* INITTABLES *) ;



begin (* HAUPTPROGRAMM *)
  WRITELN ( 'P5 Pascal compiler vs. ' , MAJORVER : 1 , '.' , MINORVER :
            1 ) ;
  WRITELN ;

  (************)
  (*initialize*)
  (************)
  (************)

  INITSCALARS ;
  INITSETS ;
  INITTABLES ;

  (******************************************)
  (*enter standard names and standard types:*)
  (******************************************)
  (******************************************)

  LEVEL := 0 ;
  TOP := 0 ;
  with DISPLAY [ 0 ] do
    begin
      FNAME := NIL ;
      FLABEL := NIL ;
      FCONST := NIL ;
      FSTRUCT := NIL ;
      OCCUR := BLCK ;
      BNAME := NIL
    end (* with *) ;
  ENTERSTDTYPES ;
  STDNAMES ;
  ENTSTDNAMES ;
  ENTERUNDECL ;
  TOP := 1 ;
  LEVEL := 1 ;
  with DISPLAY [ 1 ] do
    begin
      FNAME := NIL ;
      FLABEL := NIL ;
      FCONST := NIL ;
      FSTRUCT := NIL ;
      OCCUR := BLCK ;
      BNAME := NIL
    end (* with *) ;

  (**********)
  (*compile:*)
  (**********)
  (**********)


  (**********************************************)
  (* !!! remove this statement for self compile *)
  (*elide                                       *)
  (**********************************************)

  REWRITE ( PRR ) ;  (*noelide           *)
                     (* open output file *)

  (***************************)
  (* write generator comment *)
  (***************************)

  WRITELN ( PRR , 'i' ) ;
  WRITELN ( PRR ,
     'i Pascal intermediate file Generated by P5 Pascal compiler vs. '
            , MAJORVER : 1 , '.' , MINORVER : 1 ) ;
  WRITELN ( PRR , 'i' ) ;
  INSYMBOL ;
  PROGRAMME ( BLOCKBEGSYS + STATBEGSYS - [ CASESY ] ) ;

  (*****************************)
  (* dispose of levels 0 and 1 *)
  (*****************************)

  PUTDSP ( 1 ) ;
  PUTDSP ( 0 ) ;

  (*************************)
  (* remove undeclared ids *)
  (*************************)

  EXITUNDECL ;
  WRITELN ;
  WRITELN ( 'Errors in program: ' , TOTERR : 1 ) ;

  (***********************************)
  (* output error report as required *)
  (***********************************)

  F := TRUE ;
  for I := 1 to 500 do
    if ERRTBL [ I ] then
      begin
        if F then
          begin
            WRITELN ;
            WRITELN ( 'Error numbers in listing:' ) ;
            WRITELN ( '-------------------------' ) ;
            F := FALSE
          end (* then *) ;
        WRITE ( I : 3 , '  ' ) ;
        ERRMSG ( I ) ;
        WRITELN
      end (* then *) ;
  if not F then
    WRITELN ;
  if DOPRTRYC then
    begin               (* print recyling tracking counts *)
      WRITELN ;
      WRITELN ( 'Recycling tracking counts:' ) ;
      WRITELN ;
      WRITELN ( 'string quants:              ' , STRCNT : 1 ) ;
      WRITELN ( 'constants:                  ' , CSPCNT : 1 ) ;
      WRITELN ( 'structures:                 ' , STPCNT : 1 ) ;
      WRITELN ( 'identifiers:                ' , CTPCNT : 1 ) ;
      WRITELN ( 'label counts:               ' , LBPCNT : 1 ) ;
      WRITELN ( 'file tracking counts:       ' , FILCNT : 1 ) ;
      WRITELN ( 'case entry tracking counts: ' , CIPCNT : 1 ) ;
      WRITELN ;
    end (* then *) ;

  (****************************************)
  (* to avoid errors for unused procs     *)
  (****************************************)

  if FALSE then
    begin
      PRTLABELS ;
      PRTDSP
    end (* then *) ;

  (****************************************)
  (* perform errors for recycling balance *)
  (****************************************)

  if STRCNT <> 0 then
    WRITELN (
        '*** Error: Compiler internal error: string recycle balance: '
              , STRCNT : 1 ) ;
  if CSPCNT <> 0 then
    WRITELN (
      '*** Error: Compiler internal error: constant recycle balance: '
              , CSPCNT : 1 ) ;
  if STPCNT <> 0 then
    WRITELN (
     '*** Error: Compiler internal error: structure recycle balance: '
              , STPCNT : 1 ) ;
  if CTPCNT <> 0 then
    WRITELN (
    '*** Error: Compiler internal error: identifier recycle balance: '
              , CTPCNT : 1 ) ;
  if LBPCNT <> 0 then
    WRITELN (
         '*** Error: Compiler internal error: label recycle balance: '
              , LBPCNT : 1 ) ;
  if FILCNT <> 0 then
    WRITELN (
          '*** Error: Compiler internal error: file recycle balance: '
              , FILCNT : 1 ) ;
  if CIPCNT <> 0 then
    WRITELN (
          '*** Error: Compiler internal error: case recycle balance: '
              , CIPCNT : 1 ) ;
  99 :
  
end (* HAUPTPROGRAMM *) .
