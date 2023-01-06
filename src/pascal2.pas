program PCODE_TRANSLATOR ( PCODE , PCODE1 , PCODE2 , PCODE3 , OUTPUT ,
                           OBJCODE , LIST002 , TRACEF ) ;

//******************************************************************
//$D-,N+                                                            
//******************************************************************
//                                                                  
//  P_CODE (POST) PROCESSOR                                         
//  -----------------------                                         
//                                                                  
//  COPYRIGHT 1976, STANFORD LINEAR ACCELERATOR CENTER.             
//                                                                  
//  THIS IS A TRANSLATOR FOR THE MODIFIED  P-CODE  GENERATED  BY    
//  THE  SLAC  PASCAL   COMPILER.  THE TRANSLATOR TRANSLATES THE    
//  P_CODE INTO IBM/370 ASSEMBLY  LANGUAGE  OR  STANDARD  OS/370    
//  OBJECT  MODULE  WHICH  COULD BE RUN ON THE 370 USING A SMALL    
//  I/O PACKAGE.  ALSO  THE  IMMEDIATE  TARGET  MACHINE  OF  THE    
//  TRANSLATOR  IS  THE 360/370 COMPUTERS, THE MACHINE DEPENDENT    
//  MODULES IN THE PROGRAM ARE  RELATIVELY  ISOLATED  SUCH  THAT    
//  CONVERSIONS  FOR  OTHER  REGISTER  ORIENTED  TARGET MACHINES    
//  SHOULD BE STRAIGHTFORWARD.                                      
//                                                                  
//  REFER TO THE 'THE PASCAL P COMPILER:  IMPLEMENTATION  NOTES,    
//  U.  AMMANN, K.  JENSEN, H.  NAGELI, AND K.  NORI, DEC.  74.'    
//  FOR  THE DEFINITION OF THE P_MACHINE AND THE P SUBSET OF THE    
//  PROGRAMMING LANGUAGE "PASCAL".                                  
//                                                                  
//******************************************************************
//                                                                  
//  -THE ERROR MESSAGES ISSUED  BY  THE TRANSLATOR  ARE  USUALLY    
//  ACCOMPANIED  BY THE  APPROXIMATE LINE  NUMBER  OF THE SOURCE    
//  STATEMENT.    THESE NUMBERS APPEAR ON THE LEFT OF THE SOURCE    
//  PROGRAM LISTING AND THE ERROR SHOULD BE LOCATED BETWEEN  THE    
//  STATEMENT  WITH THE GIVEN NUMBER AND THAT NUMBER+1.    THESE    
//  ERROR CODES SHOULD BE INTERPRETED ACCORDING TO THE FOLLOWING    
//  TABLE:                                                          
//                                                                  
//  253- PROCEDURE TOO LONG (LARGER THAN 8K BYTES).                 
//       --> SUBDIVIDE THE PROCEDURE.                               
//  254- PROCEDURE TOO LONG (LARGER THAN 8K BYTES) - other place    
//       --> SUBDIVIDE THE PROCEDURE.                               
//  255- PROCEDURE TOO LONG (LARGER THAN 8K BYTES) - other place    
//       --> SUBDIVIDE THE PROCEDURE.                               
//  256- TOO MANY PROCEDURES/FUNCTIONS REFERENCED IN THIS PROC.     
//       --> RECOMPILE THE POST_PROCESSOR WITH A LARGER VALUE       
//       FOR PRCCNT.                                                
//  259- EXPRESSION TOO COMPLICATED.                                
//       -->  SIMPLIFY  THE  EXPRESSION  BY  REARRANGING  AND/OR    
//       BREAKING.                                                  
//  263- TOO MANY (COMPILER GENERATED) LABELS IN THIS PROCEDURE.    
//       --> RECOMPILE THE POST_PROCESSOR WITH A LARGER VALUE       
//       FOR LBLCNT.                                                
//  300- DIVIDE BY ZERO (RESULT OF CONSTANT PROPAGATION).           
//       --> FIX UP THE (CONSTANT) EXPRESSION EVALUATING TO ZERO.   
//  301- RANGE ERROR IN STRUCTURED CONSTANT.                        
//       --> CORRECT INITIAL VALUE FOR FIELD/ELEMENT OF CONSTANT.   
//  302- SUBSCRIPTRANGE ERROR (RESULT OF CONSTANT PROPAGATION).     
//       --> FIX UP THE CONSTANT SUBSCRIPT EXPRESSION.              
//  303- CONSTANT SET TOO LARGE FOR TARGET VARIABLE IN AN ASSMT.    
//       --> CORRECT DECLARATION FOR VARIABLE.                      
//                                                                  
//  504- SIZE OF ARRAY ELEMENT TOO LARGE.                           
//       --> REORDER THE DIMENSIONS OF THE ARRAY (SO THAT THE       
//       THE LARGER DIMENSIONS ARE FIRST) OR REDUCE THE RANGE       
//       OF THE LOW ORDER (LAST) INDICES.                           
//                                                                  
//  THE FOLLOWING ERRORS NORMALLY INDICATE AN INCONSISTENCY IN      
//  THE COMPILER AND OR THE POST_PROCESSOR.                         
//                                                                  
//  601- TYPE CONFLICT OF OPERANDS IN THE P_PROGRAM.                
//  602- OPERAND SHOULD BE OF TYPE 'ADR'.                           
//  604- ILLEGAL TYPE FOR RUN TIME CHECKING.                        
//  605- OPERAND SHOULD BE OF TYPE 'BOOL'.                          
//  606- UNDEFINED P_INSTRUCTION CODE.                              
//  607- UNDEFINED STANDARD PROCEDURE NAME.                         
//  608- DISPLACEMENT FIELD OUT OF RANGE                            
//  609- SMALL PROC IS LARGER THAN 4K, RESET SHRT_PROC = 350        
//  610- BAD HALFWORD INTEGER ALIGNMENT                             
//  611- BAD INTEGER ALIGNMENT.                                     
//  612- BAD REAL ALIGNMENT.                                        
//  614- THE PRE_PASS FILE (PRD) IS INCONSISTENT.                   
//  615- OPERAND SHOULD BE OF TYPE 'SET'.                           
//  616- CONSISTENCY CHECK ON 'SET' OPS FAILED.                     
//  617- BAD DISPLACEMENT FOR STRUCTURED CONSTANT.                  
//  618- UNEXPECTED END-OF-LINE WHEN READING P-CODE.                
//  619- BAD OPERANDS FOR PACK/UNPACK PROCEDURE.                    
//  620- no implementation for P-Code in proc ASMNXTINST            
//                                                                  
//  new errors from 2016 and later (Bernd Oppolzer):                
//                                                                  
//  701- top of stack is not 1 at beginning of statement            
//  710- % directive is not %INCLUDE                                
//  711- %INCLUDE does not specify pcodex                           
//  712- %INCLUDE pcodex but not pcode1, 2 or 3                     
//  75x- registers are not available (different variants)           
//  750- no single register available                               
//  751- no register pair available (for string operations)         
//  752- no floating point register available                       
//       etc. etc.                                                  
//                                                                  
//  THIS PROGRAM SHOULD NOT BE COMPILED WITH THE 'D+' OPTION.       
//                                                                  
//                                                                  
//                          S. HAZEGHI,                             
//                                                                  
//                          COMPUTATION RESEARCH GROUP              
//                          STANFORD LINEAR ACCELARATOR CENTER      
//                          STANFORD, CA. 94305.                    
//                                                                  
//                                                                  
//  EXTENSIVE MODIFICATIONS MADE BY:                                
//                                                                  
//                          R. NIGEL HORSPOOL                       
//                                                                  
//                          SCHOOL OF COMPUTER SCIENCE              
//                          MCGILL UNIVERSITY                       
//                          805 SHERBROOKE STREET WEST              
//                          MONTREAL                                
//                          QUEBEC  H3A 2K6   CANADA                
//                                                                  
//******************************************************************
//                                                                  
//         AUTHOR OF This VERSION (Oppolzer Version):               
//                                                                  
//              Bernd Oppolzer                                      
//              Diplom-Informatiker                                 
//              Baerenhofstr. 23                                    
//              D-70771 Leinfelden-Echterdingen                     
//              Germany                                             
//                                                                  
//******************************************************************
//                                                                  
//  History records - newest first                                  
//                                                                  
//******************************************************************
//                                                                  
//  Jan 2023 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  Handling of Literal Pool reworked because of error when testing 
//  some minor enhancements of the compiler.                        
//                                                                  
//  It turned out that the compiler erroneously reused a halfword   
//  literal of H'20 for a fullword literal consisting of two        
//  halfwords H'20,0' - this way the second half, which should be   
//  zero, became undefined. Hard to find. See comment with time tag 
//  05.01.2023                                                      
//                                                                  
//  The literal managing functions were all consolidated into one   
//  isolated procedure (which could be in a separate module, BTW).  
//  See MANAGE_LITERALS and its STATIC definitions.                 
//                                                                  
//******************************************************************
//                                                                  
//  May 2021 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  Fix error in GETOP_SIMPLE - generated LR Rx,0 instead of        
//  XR Rx,Rx - which in turn lead to wrong behaviour in             
//  MFI operation, which in turn made PASFORM signal EOF            
//  at the very beginning of the source file :-((                   
//                                                                  
//  It is really time to rewrite the P-Code to 370 translator       
//                                                                  
//  but this is a really big task, because the existing             
//  translator does a really good job with respect to               
//  optimization (the older parts, at least).                       
//                                                                  
//******************************************************************
//                                                                  
//  May 2021 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  PASCAL2 abended when compiling PASFORM - no error with          
//  other modules !!                                                
//                                                                  
//  After some checking I found out that the error seems to         
//  be an initialization problem. The error was resolved by         
//  adding some initializations to the local variables of           
//  procedure ASMNXTINST.                                           
//                                                                  
//  This motivated me to add an option $I - if this option is       
//  set, the compiler generates code to initialize the              
//  automatic area to hex zeroes on every block entry.              
//  This is a performance nightmare, of course, so this             
//  should be used only as a last resort, if no other               
//  remedy for strange runtime errors can be found.                 
//                                                                  
//  The $I option is implemented in PASCAL1 and passed to           
//  PASCAL2 (see the format changes in the ENT instruction),        
//  but not yet fully implemented in PASCAL2.                       
//                                                                  
//  BTW: ENT now can handle more "boolean" options without          
//  much effort - see the new ENT format (there is one              
//  string of booleans of variable length)                          
//                                                                  
//  ... and a side note: I don't want to invest much time           
//  in PASCAL2, because a new PCODE translator will be built        
//  in the next months, called PASCAL3, with the following          
//  features:                                                       
//                                                                  
//  - completely re-structured                                      
//  - maybe 31 bit ready                                            
//  - maybe generates code for other platforms, too                 
//                                                                  
//******************************************************************
//                                                                  
//  Mar 2021 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  Allow comments in P-Code instructions (Compiler adds            
//  Variable names to certain instructions like LDA, LOD and        
//  STR). Comments are separated by a semicolon.                    
//                                                                  
//  First use of READSTR in compiler. (To be portable to            
//  other platforms, it is sufficient that the compiler can         
//  compile itself; it is not necessary that the compiler can       
//  be compiled by other dialects of Pascal. The port can be        
//  done by migrating the P-Code variant of the compiler,           
//  after all).                                                     
//                                                                  
//******************************************************************
//                                                                  
//  Feb 2021 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  Some corrections:                                               
//                                                                  
//  - compare_carr had some errors - corrected (SCRATCHPOS used)    
//                                                                  
//  - VCC had some strange errors - corrected (SCRATCHPOS used)     
//                                                                  
//******************************************************************
//                                                                  
//  Feb 2021 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  PCODE instruction ENT now contains address of 8 byte            
//  scratchpad area, which can be used for temporary                
//  storage. Location is contained in PIAKT record and              
//  named SCRATCHPOS.                                               
//                                                                  
//  SCRATCHPOS is not used by PASCAL1; instead the position         
//  is passed to PASCAL2 (for every block) in the ENT instruction   
//  and so PASCAL2 can generate code to make use of it.             
//                                                                  
//  SCRATCHSIZE is a constant in PASCAL1 (8 at the moment)          
//                                                                  
//******************************************************************
//                                                                  
//  Oct 2020 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  PCODE output in different parts using %INCLUDE directive        
//  because file size (maximum number of lines) is limited          
//  on the VM/CMS platform                                          
//                                                                  
//  PASCAL2 has to read the PCODE input and implement the           
//  %INCLUDE statement                                              
//                                                                  
//******************************************************************
//                                                                  
//  Oct 2020 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  New P-Code instruction MV1 to allow easier initialization       
//  of a series of variables at block entry. MV1 is the same        
//  as MOV, but leaves one of the addresses on the stack            
//  (the source address in this case), this allows for the          
//  source address being incremented for the following moves.       
//                                                                  
//******************************************************************
//                                                                  
//  Aug 2020 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  fixed a strange error caused by the new runtime functions       
//  written in Pascal; the file register (FILADR = reg. 9)          
//  was not set correctly, because after SIO no real runtime        
//  CSP (like RDI) was called ... $PASRDI instead, which does       
//  not need FILADR = reg 9 ... but the following normal CSP        
//  like RLN (readln) expected FILADR being set.                    
//                                                                  
//  I fixed this by adding a new variable FILADR_LOADED, which      
//  does not only control the reservation of register FILADR,       
//  but the real loading.                                           
//                                                                  
//******************************************************************
//                                                                  
//  Apr 2020 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  - see PASCAL1 - no changes here                                 
//                                                                  
//******************************************************************
//                                                                  
//  Jan 2020 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  - some corrections to the LAST_FILE structure and its           
//    logic / see SAVE_FILEOPERAND. The P-Codes SIO and EIO         
//    are importand for invalidating the information about          
//    files recently used.                                          
//                                                                  
//  - CHK E implemented to support runtime exceptions on the        
//    mainframe (new Pascal procedure $ERROR).                      
//                                                                  
//******************************************************************
//                                                                  
//  Nov 2019 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  - See PASCAL1; support negative length parameter on             
//    VMV operation (varchar move) - if negative then the           
//    the order of the operands on the stack is reversed.           
//                                                                  
//******************************************************************
//                                                                  
//  Sep 2019 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  - To allow the compiler to compile the P5 compiler, it          
//    is necessary to allow set expressions [ a .. b ],             
//    where a and b are variable expressions.                       
//                                                                  
//  - This implies the creation of a new P-Code instruction         
//    ASR, which sets a range of elements in a set                  
//    (add set range ... similar to ASE, add set element).          
//    ASR fetches three arguments from the stack: the set           
//    and two elements: the two elements define the range.          
//                                                                  
//******************************************************************
//                                                                  
//  Jun 2019 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  - Code generation errors with a combination of SUBSTR and       
//    concatenation, when inside WRITELN ... example:               
//                                                                  
//    WRITELN ( 'stab/5  = <' ,                                     
//              SUBSTR ( STAB [ N - 1 ] , 1 , 1 ) ||                
//              STAB [ N ] ||                                       
//              SUBSTR ( STAB [ N ] , 1 , 1 ) , '>' ) ;             
//                                                                  
//    it turned out, that the WRITELN instruction took the          
//    registers 8 and 9 from the beginning, so that not             
//    enough register pairs could be found to do the                
//    complicated string concatenation, hence the error 259         
//    in PASCAL2. I allowed the procedure FINDRP (find              
//    register pair) to take the CSP registers 8 and 9,             
//    if needed, which may leed to subsequent load instructions     
//    (when the WRITE CSP has to be finally executed).              
//                                                                  
//    The concatenation was successful, if coded outside the        
//    WRITE :-) after this modification, it worked inside the       
//    WRITE, too.                                                   
//                                                                  
//  - Other errors with concatenation (P-Code VCC) repaired         
//                                                                  
//******************************************************************
//                                                                  
//  May 2019 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  - Code generation errors with builtin functions LENGTH          
//    and MAXLENGTH, when the lengths are known at compile time     
//                                                                  
//  When the LENGTH or MAXLENGTH function was related to an         
//  array element, the compiler generated code for the              
//  addressing of the element, although not needed. What made       
//  things worse: this code left an unneeded item (the address      
//  of the element) on the stack, which was not removed and led     
//  to problems in the PASCAL2 code generation (the PCINT           
//  interpreter doesn't complain, although the memory leak          
//  - or stack leak in this case - is clearly visible in            
//  debug mode).                                                    
//                                                                  
//  The solution found is:                                          
//                                                                  
//  to invalidate the generated code using two new P-Code           
//  instructions XBG and XEN.                                       
//                                                                  
//  XBG <seqno> is generated, when a critical code sequence         
//  starts.                                                         
//                                                                  
//  If later the compiler decides that the code starting from       
//  the last scheduled XBG is not needed, it generates a            
//  XEN <seqno>,0 ... otherwise XEN <seqno>,1                       
//                                                                  
//  It is important that the compiler knows the seqno of the        
//  XBG to write it on the XEN ... and: it should write the         
//  XEN unconditionally, because PASCAL2 and the P-Code             
//  interpreter will look for it (if no XEN for a particular        
//  XBG is found, the code is generated, that is, an                
//  XEN <seqno>,1 is implied).                                      
//                                                                  
//******************************************************************
//                                                                  
//  May 2019 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  - Some errors were reported by Ed Lott (thanks)                 
//                                                                  
//    a) when the compiler generated calls to the CSP WRV           
//       (write strings), it did not make sure that the             
//       FCB address was loaded (see PASCAL2, LOADFCBADDRESS).      
//       Corrected 13.05.2019                                       
//                                                                  
//    b) when accessing static strings, the compiler did not        
//       add the offset of the string in the STATIC CSECT           
//       during address computation (in some situations)            
//       Corrected 14.05.2019                                       
//                                                                  
//    c) when accessing the length field of a string,               
//       the compiler did not compute the address correctly         
//       (especially when the string was an array element).         
//       The function GETADR2 must be used in this case.            
//       Corrected 15.05.2019                                       
//                                                                  
//    d) wrong code was generated, when a string array              
//       component was passed to a procedure (again, using          
//       GETADR2 solved the problem).                               
//       Corrected 17.05.2019                                       
//                                                                  
//******************************************************************
//                                                                  
//  Jun.2018 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  MEMCMP added as standard function, similar to MEMCPY.           
//  Two new PCODE instructions added to implement MEMCMP inline     
//  (MCC and MCV)                                                   
//                                                                  
//******************************************************************
//                                                                  
//  Jun.2018 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  Some optimization has been applied to the literal pool;         
//  leading to errors 257 first. There was an interesting           
//  story about an old optimization strategy targetting             
//  series of MVCs, which lead to unused literals and errors        
//  257 ... see compiler Facebook page.                             
//                                                                  
//  This was fixed by adding field OPTIMIZED into LITTBL            
//                                                                  
//  Look into procedure SOPERATION, the code following the          
//  comment: CONSECUTIVE MVC INSTS                                  
//                                                                  
//******************************************************************
//                                                                  
//  May 2018 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  The P-Codes for Strings (starting with the letter V)            
//  are now recognized and translated to 370 machine code;          
//  this was a hard piece of work and finally seems to work         
//  correctly with the 2018.05 release. There still remains         
//  some work to do: some of the length checks which should         
//  be in place for the strings to work correctly are still         
//  not yet implemented. Error handling should be improved and      
//  consolidated.                                                   
//                                                                  
//******************************************************************
//                                                                  
//  Mar.2018 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  Implementing the new P-Code instructions to support             
//  Strings aka VarChars ... the new P-Codes all start with         
//  the letter V and are described elsewhere.                       
//                                                                  
//  The handling and administration of the literal pool             
//  has been improved, see type literal_pool_control;               
//  character string literals are stored only once, if              
//  they appear more than once in a procedure or function;          
//  this is also true if one string is the beginning or ending      
//  part of another string (the longer string must have             
//  appeared first in the source).                                  
//                                                                  
//  Many minor improvements to PASCAL2 to make the String           
//  implementation possible :-)                                     
//                                                                  
//  The new P-Codes:                                                
//                                                                  
//  'VC1' , 'VC2' , 'VCC' , 'VLD'                                   
//  'VST' , 'VMV' , 'VSM' , 'VLM'                                   
//  'VPU' , 'VPO' , 'VIX' , 'VRP'                                   
//                                                                  
//  see procedure STRINGOPS (and others)                            
//                                                                  
//******************************************************************
//                                                                  
//  Dec.2017 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  New P-Code instructions to better support block moves           
//  like memcpy and memset:                                         
//                                                                  
//   - PCODE_MFI = 80 ;  // memory fill - constant length           
//   - PCODE_MCP = 81 ;  // memcpy - three parameters               
//   - PCODE_MSE = 82 ;  // memset - three parameters               
//   - PCODE_MZE = 84 ;  // memory zero - constant length           
//                                                                  
//  and a new DBG instruction, which should be ignored:             
//                                                                  
//   - PCODE_DBG = 83 ;  // one parameter, ignored at the moment    
//                                                                  
//******************************************************************
//                                                                  
//  Aug.2017 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  Some corrections on the implementation of Pascal sets,          
//  sets can have up to 2000 elements now ... see constants         
//  MXPLNGTH etc.                                                   
//                                                                  
//  More improvements on sets will follow                           
//                                                                  
//******************************************************************
//                                                                  
//  May.2017 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  The compiler now runs on MVS (Hercules), too.                   
//  Same source code (PASCAL1, PASCAL2) as with CMS,                
//  same runtime (PASMONN) - although there are some                
//  CMS dependencies, controlled by SYSPARM(CMS).                   
//  Different PASSNAP ... see below.                                
//                                                                  
//  See more comments in PASCAL1.PAS                                
//                                                                  
//******************************************************************
//                                                                  
//  Jan.2017 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  From some discussions on the FPC mailing list, I got the        
//  idea to support bit operations on integer operands, too.        
//                                                                  
//  The operations AND, OR, NOT have been extended to do            
//  bit operations, when being used with integers (was error        
//  134 before). Another operation XOR is provided (new             
//  reserved symbol) for exclusive or operation; can be used        
//  with integer or boolean operands.                               
//                                                                  
//  New P-Code instruction XOR; the P-Code instructions             
//  AND, IOR, NOT and XOR have a type parameter (B or I).           
//                                                                  
//  PASCAL2 was extended to support the integer operands            
//  with AND, IOR and NOT and the new P-Code instruction XOR;       
//  the constant XOR had to be renamed to XORX, because             
//  XOR now is a reserved word.                                     
//                                                                  
//******************************************************************
//                                                                  
//  Dec.2016 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  The generation of the STATIC and CODE CSECTs was changed.       
//                                                                  
//  The STATIC CSECT contains its own size at offset 8;             
//  the real data starts at offset 16 (10 to 15 are free).          
//                                                                  
//  The CODE CSECT contains the Pascal procedure name also          
//  in the NODEBUG case in the CSECT identifier, and the            
//  stacksize at a certain position (see GEN_CSECT and              
//  INIT_CSECT for details).                                        
//                                                                  
//  This way it is possible for PASSNAP to show the areas           
//  in their correct length also in the NODEBUG case in             
//  hex dump format; and with the real Pascal proc names            
//  (but no Pascal variable names; to do this, the DEBUG            
//  switch and a DBGINFO file is needed).                           
//                                                                  
//******************************************************************
//                                                                  
//  Dec.2016 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  Another portability issue:                                      
//                                                                  
//  the branch table used with case statements implies the          
//  EBCDIC char set, if the case control variable is of type        
//  char. I changed the XJP logic to a portable representation      
//  of the branch table and shifted the construction of the         
//  "real" branch table to the second pass. This way, XJP           
//  instructions and "portable branch tables" can be moved          
//  to foreign platforms with foreign character sets.               
//                                                                  
//  see boolean constant 'PORTABLE_BRANCHTABLE' in pass 1           
//                                                                  
//  this is the second pass (the P-Code translator);                
//  it recognizes and handles both variants of branch tables,       
//  portable and non-portable                                       
//                                                                  
//******************************************************************
//                                                                  
//  Nov.2016 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  To enable the port to ASCII platforms, the following            
//  changes have been made:                                         
//                                                                  
//  - set constants of set of char have a new representation        
//    in the P-Code, envolving char representation of the           
//    chars contained in the set                                    
//                                                                  
//  - not related to the port: set constants in P-Code are          
//    represented by hexa byte strings instead of integer           
//    strings, which makes them much better readable                
//                                                                  
//  See procedure READSET                                           
//                                                                  
//******************************************************************
//                                                                  
//  Oct.2016 - Extensions to the Compiler by Bernd Oppolzer         
//             (berndoppolzer@yahoo.com)                            
//                                                                  
//  modification to support static variables and to provide         
//  informations for SNAPSHOT, so that static variables can         
//  be found at run time. See pass 1 (PASCAL1.PAS) for details,     
//  and some comments in INIT_CSECT and GEN_CSECT.                  
//                                                                  
//******************************************************************
//                                                                  
//  modification in 2016 by bernd oppolzer / stuttgart / germany    
//                                                                  
//                          berndoppolzer@yahoo.com                 
//                                                                  
//  to allow longer variable names, some constants had to           
//  be changed (and places, where numeric constants were used       
//  instead of symbolic constants had to be identified and          
//  changed, too)                                                   
//                                                                  
//  idlngth changed from 12 to 20                                   
//  hdrlngth changed from 32 to 40                                  
//                                                                  
//  input routines affected for the following P-Code instructions:  
//  ENT, CST, BGN - format specification IDLNGTH + 2 instead of 14  
//                                                                  
//  Header for Object Code output extended from 32 to 40 bytes;     
//  Jump over Header adjusts automatically                          
//                                                                  
//  longer function names appear in Objekt Code, for example        
//  PASCALCOMPILER (name of pass1) and function names like          
//  ENTERSTDTYPES without truncation                                
//                                                                  
//  will SNAPSHOT etc. still work ?                                 
//                                                                  
//******************************************************************
//                                                                  
//  modification in 2016 by bernd oppolzer / stuttgart / germany    
//                                                                  
//                          berndoppolzer@yahoo.com                 
//                                                                  
//  there was a need to do an explicit reset(input) at the          
//  beginning of the main procedure, because the compiler           
//  doesn't insert it any more automatically due to some            
//  improvements (in my opinion); but pascal2.pas checks for        
//  eof(input) before first read, and therefore the reset has       
//  to be done before the first read (the implicit reset at the     
//  time of the first read call is not sufficient).                 
//                                                                  
//  see some comments in pasmonn.ass for details.                   
//                                                                  
//******************************************************************
//                                                                  
//  modification in 2016 by bernd oppolzer / stuttgart / germany    
//                                                                  
//                          berndoppolzer@yahoo.com                 
//                                                                  
//  the procedure names applied to the object file in case of       
//  the active debug switch had to be extended to 20 chars,         
//  so that SNAPSHOT could get the long names correctly.            
//  the branches around those names had to be adjusted.             
//                                                                  
//******************************************************************
//                                                                  
//  modification in 2011 by bernd oppolzer / stuttgart / germany    
//                                                                  
//                          berndoppolzer@yahoo.com                 
//                                                                  
//  - activate output of assembler mnemonics to list002             
//    to make analyzing the object code easier.                     
//    the final goal is to create other pcode translators           
//    for other platforms, e.g. windows and linux                   
//                                                                  
//******************************************************************



const VERSION = '2023.01' ;        // Version for display message
      VERSION2 = 0x2301 ;          // Version for load module
      VERSION3 = 'XL2''2301''' ;   // Version for LIST002 listing

      //*****************************************
      // trace switches for different topics     
      //*****************************************

      TRACE_LITERAL = FALSE ;

      //*****************************************
      // different important constants           
      //*****************************************

      MXADR = 65535 ;
      SHRTINT = 4095 ;
      HALFINT = 32700 ;
      STKDPTH = 100 ;    // was 15 - reset when work finished
      MXLVL = 16 ;

      //*****************************************
      // cixmax: maximum difference of highest   
      // and lowest case label                   
      // must be coherent with pass 1            
      // should be communicated via OBJCODE file 
      //*****************************************

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

      //*************************************************
      // Reg 9 = Pascal FCB register                     
      // normally available, but not during csp call     
      // Reg 8 = Call Stack Adr                          
      // adr of next save area on stack, should be       
      // stable across calls (while in csp, reg 8 is     
      // used to access file cb)                         
      //*************************************************

      MAXSTRL = 254 ;

      //****************************************************
      // MAXSTRL  = maximal string length                   
      // MXPLNGTH = maximal set length in chars (8 bits)    
      // MXSETINX = maximal set length (64 bit packets)     
      // MXSETIXI = maximal set length (32 bit packets)     
      //****************************************************

      MXPLNGTH = 256 ;
      MXSETINX = 32 ;
      MXSETIXI = 64 ;
      MXSETLEN = 256 ;
      HDRLNGTH = 40 ;   // LENGTH OF PROGRAM HEADING
      EOFDPLMT = 33 ;

      //************************************************
      // POSN OF EOF FLAG WITHIN FILE HEADER            
      //************************************************

      EOLDPLMT = 34 ;

      //************************************************
      // POSN OF EOL FLAG WITHIN HEADER FOR TEXT FILES  
      //************************************************

      FILHDRSZ = 8 ;

      //************************************************
      //OFFSET OF FILE COMPONENT WITHIN FILE VAR.       
      //************************************************

      ADRSIZE = 4 ;
      CHARSIZE = 1 ;
      BOOLSIZE = 1 ;
      INTSIZE = 4 ;
      HINTSIZE = 2 ;
      REALSIZE = 8 ;

      //**************************************************
      // LAYOUT OF THE 'GLOBAL' STACK FRAME:              
      //**************************************************

      NEWPTR = 72 ;

      //**************************************************
      // NEWPTR , OFFSET FROM BOTTOM OF RUNTIME STACK     
      //**************************************************

      HEAPLMT = 76 ;

      //**************************************************
      // HEAP LIMIT PTR, OFFSET FROM BOTTOM OF STACK      
      //**************************************************

      DYNRUNC = 0 ;

      //**************************************************
      // # OF COUNTERS , FIELD OFFSET FROM HEAPLMT        
      //**************************************************

      DYN2LEN = 8 ;

      //**************************************************
      // LENGTH OF THE DYN. INFO. AREA AT THE END OF HEAP 
      //**************************************************

      FNCRSLT = 72 ;

      //**************************************************
      // FUNCTION RESULT LOCATION, OFFSET FROM MP         
      //**************************************************

      DISPLAY = 80 ;

      //**************************************************
      // DISPLAY REGS, OFFSET FROM BOTTOM OF RUNTIME STK  
      //**************************************************

      DISPAREA = 40 ;

      //**************************************************
      // SIZE OF DISPLAY TABLE                            
      //**************************************************

      LCAFTMST = 80 ;

      //**************************************************
      // SIZE OF THE PROCEDURE LINKAGE AREA               
      //**************************************************

      FPRSAREA = 80 ;

      //**************************************************
      // FLOATING PT. REGS SAVE AREA, (OPTIONAL SAVE)     
      //**************************************************

      FPSIZE = 32 ;

      //**************************************************
      // LENGTH OF FPR SAVE AREA                          
      //**************************************************

      FL1 = 352 ;
      FL2 = 360 ;
      FL3 = 368 ;
      FL4 = 376 ;

      //**************************************************
      // GLOBAL LOCATIONS USED FOR FIX-FLOAT CONVERSIONS  
      //**************************************************

      STRFIRST = 384 ; // PTR TO BEGIN OF STR WORKAREA
      STRCURR = 388 ;  // ACTUAL PTR TO STR WORKAREA
      STRSIZE = 392 ;  // STR WORKAREA SIZE

      //**************************************************
      // addresses for string workarea management         
      //**************************************************

      INXCHK = 152 ;

      //**************************************************
      // ADDRESS OF RUNTIME CHECK ROUTINES                
      //**************************************************

      RNGCHK = 164 ;
      PRMCHK = 176 ;
      PTRCHK = 188 ;
      PTACHK = 200 ;
      SETCHK = 212 ;
      STKCHK = 224 ;
      TRACER = 236 ;

      //**************************************************
      // CONTROL FLOW TRACE ROUTINE                       
      //**************************************************

      FILEBUFS = 248 ;

      //**************************************************
      // INPUT, OUTPUT, PRD,.... BUFFERS                  
      //**************************************************

      CLEARBUF = 320 ;

      //**************************************************
      // PRESET BUFER TO ERASE MEMORY WITH ITS CONTENTS   
      //**************************************************

      PASDATE = 328 ;

      //**************************************************
      // PREDEFINED DATE VARIABLE                         
      //**************************************************

      PASTIME = 338 ;

      //**************************************************
      // PREDEFINED TIME VARIABLE                         
      //**************************************************

      OSPRM = 348 ;

      //**************************************************
      // POINTER TO O.S. PARMS RECORD                     
      //**************************************************

      FRSTGVAR = 400 ;

      //**************************************************
      // FIRST GLOBAL VAR, SHOULD BE A MULTIPLE OF 8      
      // VARIOUS TABLE SIZES AND MISC. CONSTATNTS         
      //**************************************************

      HTSIZE = 200 ;

      //**************************************************
      // HASH TABLE SIZE (MUST EXCEED # OPS + # CSP OPS)  
      //**************************************************

      LITCNT = 400 ;   // # OF NUMERIC LITERALS IN A PROC.
      LITDANGER = 395 ; // SAFE LIMIT FOR NXTLIT

      //**************************************************
      // PRCCNT = # OF PROC'S OR ENTRY PT.S IN ONE CSECT  
      // opp 02.2018: was 50, set to 200                  
      // LBLCNT = # OF LABELS IN A CSECT                  
      //**************************************************

      PRCCNT = 200 ;
      LBLCNT = 500 ;
      MAX_CALL_DEPTH = 9 ;

      //**************************************************
      // MAX NESTING OF FUNCTION CALLS IN A STMT.         
      //**************************************************

      MXCODE = 4092 ;

      //**************************************************
      // MAX OBJECT CODE SIZE (APPROX. 8K BYTES)          
      //**************************************************

      MXCODE1 = 4093 ;
      MXLNP = 800 ;

      //**************************************************
      // SIZE OF LINE NUMBER TABLE IN BYTES               
      //**************************************************

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

      //***************************
      // CONDITION CODE SYMBOLS    
      //***************************

      GRTCND = 2 ;
      GEQCND = 11 ;
      EQUCND = 8 ;
      NEQCND = 7 ;
      ANYCND = 15 ;
      NOCND = 0 ;
      TRUCND = 1 ;
      FLSCND = 8 ;

      //**************************************************
      // LIMIT VALUE FOR A PROC. TO BE CONSIDERED SMALL   
      //**************************************************

      SHRT_PROC = 550 ;

      //*****************************************************
      // asmtag = tag for assembler instructions in list002  
      // colasmi = position for assembler instr. in list002  
      // spaceasmi = spaces after asm instr. in list002      
      //*****************************************************

      ASMTAG = '@@ ' ;
      COLASMI = 10 ;
      SPACEASMI = 2 ;
      SPACEASMX = 12 ;
      SPACEASML = 5 ;

      //*****************************************************
      // shift masks for shift instructions                  
      //*****************************************************

      SL8 = 256 ;             // SHIFT LEFT  8 BITS
      SL12 = 4096 ;           //            12
      SL16 = 65536 ;          //            16
      SL24 = 16777216 ;       //            24

      //**************************************************
      // OPCODE TABLES  (P-OPCODES / P-STANDARD PROCS /   
      // 370-OPCODES )                                    
      //**************************************************

      XBALR = 5 ;
      XBCTR = 6 ;
      XBCR = 7 ;
      XSVC = 10 ;
      XMVCL = 14 ;
      XCLCL = 15 ;
      XLPR = 16 ;
      XLTR = 18 ;
      XLCR = 19 ;
      XNR = 20 ;
      XCLR = 21 ;
      XORX = 22 ;
      XXR = 23 ;
      XLR = 24 ;
      XCR = 25 ;
      XAR = 26 ;
      XSR = 27 ;
      XMR = 28 ;
      XDR = 29 ;
      XLPDR = 32 ;
      XLTDR = 34 ;
      XLCDR = 35 ;
      XLDR = 40 ;
      XCDR = 41 ;
      XADR = 42 ;
      XSDR = 43 ;
      XMDR = 44 ;
      XDDR = 45 ;
      XSTH = 64 ;
      XLA = 65 ;
      XSTC = 66 ;
      XIC = 67 ;
      XEX = 68 ;
      XBAL = 69 ;
      XBCT = 70 ;
      XBC = 71 ;
      XLH = 72 ;
      XCH = 73 ;
      XAH = 74 ;
      XSH = 75 ;
      XMH = 76 ;
      XST = 80 ;
      XN = 84 ;
      XCL = 85 ;
      XO = 86 ;
      XX = 87 ;
      XL = 88 ;
      XC = 89 ;
      XA = 90 ;
      XS = 91 ;
      XM = 92 ;
      XD = 93 ;
      XSTD = 96 ;
      XLD = 104 ;
      XCD = 105 ;
      XAD = 106 ;
      XSD = 107 ;
      XMD = 108 ;
      XDD = 109 ;
      XAW = 110 ;
      XSRL = 136 ;
      XSLL = 137 ;
      XSRA = 138 ;
      XSLA = 139 ;
      XSRDL = 140 ;
      XSLDL = 141 ;
      XSRDA = 142 ;
      XSLDA = 143 ;
      XSTM = 144 ;
      XTM = 145 ;
      XMVI = 146 ;
      XCLI = 149 ;
      XOI = 150 ;
      XLM = 152 ;
      XMVC = 210 ;
      XNC = 212 ;
      XCLC = 213 ;
      XOC = 214 ;
      XXC = 215 ;


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
              PMCC , PMCV , PASR , PXBG , PXEN , PMV1 , PIAC , UNDEF_OP
              ) ;
     CSPTYPE = ( PCTR , PN01 , PN02 , PN03 , PN04 , PN05 , PN06 , PN07
               , PN08 , PN09 , PPAG , PGET , PPUT , PRES , PREW , PRDC
               , PWRI , PWRE , PWRR , PWRC , PWRS , PWRX , PRDB , PWRB
               , PRDR , PRDH , PRDY , PEOL , PEOT , PRDD , PWRD , PCLK
               , PWLN , PRLN , PRDI , PEOF , PELN , PRDS , PTRP , PXIT
               , PFDF , PSIO , PEIO , PMSG , PSKP , PLIM , PTRA , PWRP
               , PCLS , PDAT , PTIM , PFLR , PTRC , PRND , PWRV , PRDV
               , PRFC , PRFS , PRFV , UNDEF_CSP ) ;
     BETA = array [ 1 .. 3 ] of CHAR ;
     HINTEGER = - 32768 .. 32767 ;
     STRNG = packed array [ 1 .. MAXSTRL ] of CHAR ;
     ALFA = packed array [ 1 .. 8 ] of CHAR ;
     CHAR80 = packed array [ 1 .. 80 ] of CHAR ;
     ADRRNG = 0 .. MXADR ;
     LVLRNG = - 2 .. MXLVL ;

     //******************************************
     // REGISTER NUMBER RANGE                    
     //******************************************

     RGRNG = LVLRNG ;

     //******************************************
     // set type definitions                     
     //******************************************

     SHORT_SET = set of 0 .. 63 ;
     LARGE_SET = record
                   case INTEGER of
                     1 :
                       ( S : array [ 1 .. MXSETINX ] of SHORT_SET ) ;
                     2 :
                       ( C : array [ 1 .. MXSETLEN ] of CHAR ) ;
                 end ;

     //******************************************
     // some subranges                           
     //******************************************

     BYTE = 0 .. 255 ;
     BYTE_PLUS_ONE = 1 .. 256 ;
     STKPTR = 0 .. STKDPTH ;

     //******************************************
     // POINTER TO THE COMPILE_TIME STACK        
     //******************************************

     LVLDSP = record
                DSPLMT : INTEGER ;
                LVL : LVLRNG
              end ;

     //****************************
     // WHERE ABOUT OF THE OPERAND 
     //****************************

     ICRNG = 0 .. MXCODE1 ;
     ICRNG_EXT = - 100 .. MXCODE1 ;
     ADRRNG_EXT = - 100 .. MXADR ;

     //******************************************
     // PROGRAM COUNTER RANGE                    
     // extended ranges, because negative        
     // values are stored in linkage fields      
     // (pointers to stack entries)              
     //******************************************

     LBLRNG = - 1 .. LBLCNT ;

     //******************************************
     // RANGE OF P_COMPILER GENERATED LABELS     
     //******************************************

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
     // 08.2019: because of strange errors due to overlay of       
     // rgadr and memadr - other logic of offset computing -       
     // I decided not to overlay these fields for the moment;      
     // logic below sometimes feeds interesting values into        
     // rgadr and destroys it later by overwriting memadr :-((     
     //************************************************************
     // coding was:                                                
     //    case VPA : BANK of                                      
     //      RGS :                                                 
     //        ( RGADR : RGRNG ) ;                                 
     //      MEM :                                                 
     //        ( MEMADR : LVLDSP )                                 
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
               VPA : BANK ;
               RGADR : RGRNG ;    // only relevant if VPA = RGS
               MEMADR : LVLDSP ;  // only relevant if VPA = MEM
             end ;

     //****************************************************************
     //* this new structure records the global state of processing     
     //****************************************************************

     GLOBAL_STATE = record
                      IN_PROCBODY : BOOLEAN ;
                      FILL_LINEPTR : BOOLEAN ;
                      MOD1DEFSTEP : INTEGER ;
                      MOD2DEFSTEP : INTEGER ;
                      XBG_XEN_SUPPRESSED : INTEGER ;
                      LAST_ERRORCODE : INTEGER ;
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
                        INIT_AUTO : BOOLEAN ;
                        DEBUG_LEV : 0 .. 9 ;
                        STATNAME : ALFA ;
                        SOURCENAME : ALFA ;
                        FLOW_TRACE : BOOLEAN ;
                        CALL_HIGHER : BOOLEAN ;
                        LARGE_PROC : BOOLEAN ;
                        CODE_SIZE : ADRRNG ;  // was icrng
                        DATA_SIZE : ADRRNG ;
                        SCRATCHPOS : ADRRNG ;
                        NEXT : PPI
                      end ;

     //****************************************************************
     // XBG/XEN control                                                
     // a P-Code instruction XBG starts a conditional code section     
     // which may be ignored, if the corresponding XEN instruction     
     // has a second parameter of zero. The XBG/XEN pairs are read     
     // in the first pass (READNXTINST with modus = 1). The result     
     // is recorded in this chain.                                     
     //****************************************************************

     PXXI = -> XBG_XEN_INFO ;
     XBG_XEN_INFO = record
                      XBG_NO : INTEGER ;
                      VALID : BOOLEAN ;
                      NEXT : PXXI
                    end ;

     //************************************************************
     // for tables of offsets and statement numbers                
     //************************************************************

     STAT_OFFS = record
                   STMT : INTEGER ;
                   OFFS : ICRNG ;
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
    // anchor for procedure information chain                       
    // built during first p-code reading                            
    //**************************************************************

    XXIANKER : PXXI ;
    XXIAKT : PXXI ;
    XXILAUF : PXXI ;

    //**************************************************************
    // CURRENT/OLD INST. OPCODE                                     
    // CURRENT STND. PROC. CODE                                     
    // CURRENT (SYMBOLIC) PCODE /CSP NAME                           
    //**************************************************************

    OPCODE , OLDOPCODE : OPTYPE ;
    CSP , OLDCSP : CSPTYPE ;
    P_OPCODE , EMPTY : BETA ;
    PROCOFFSET : INTEGER ;
    OP_SP : BOOLEAN ;

    //*****************************************
    // P INSTR/SP SWITCH                       
    //*****************************************

    INIT : BOOLEAN ;

    //*****************************************
    // INITIALIZATION PHASE FLAG               
    //*****************************************

    CH : CHAR ;

    //*****************************************
    // CURRENT INPUT CHARACTER                 
    //*****************************************

    IVAL : INTEGER ;
    RVAL : REAL ;
    PSVAL : LARGE_SET ;
    PSLNGTH : 0 .. MXPLNGTH ;
    SVAL : STRNG ;
    SLNGTH : 0 .. MAXSTRL ;
    CURLVL : LVLRNG ;

    //*****************************************
    // CURRENT PROC. STATIC LEVEL              
    //*****************************************

    TOP : STKPTR ;

    //*****************************************
    // TOP OF EXPRESSION STACK                 
    //*****************************************

    CALL_DEPTH : 0 .. MAX_CALL_DEPTH ;

    //*****************************************
    // PROC. CALL NESTING                      
    //*****************************************

    LASTLN , NXTLNP , LASTPC : INTEGER ;
    LBL1 , LBL2 : PLABEL ;

    //*****************************************
    // LEFT AND RIGHT LABELS OF INSTRUCTIONS   
    //*****************************************

    XJPFLAG : CHAR ;
    EXTLANG : CHAR ;

    //*****************************************
    // TYPE OF OPERAND OF INSTRUCTION          
    //*****************************************

    P , Q : INTEGER ;
    COMPTYPE : CHAR ;
    OPNDTYPE : DATATYPE ;

    //*****************************************
    // P_Q FIELDS OF INSTRUCTION               
    // comptype = type of comparison           
    // opndtype = type of operand              
    //*****************************************
    //*****************************************
    // LOC. OF STRUCT. CONSTANT ITEM           
    //*****************************************
    //*****************************************
    // MEMORY STACK POINTER, NOT USED          
    //*****************************************

    LCAFTSAREA : ADRRNG ;

    //*****************************************
    // FIRST LOC. AFTER PROC. SAVE AREA        
    //*****************************************

    FILECNT : 0 .. 2 ;

    //*****************************************
    // COUNT OF ACTIVE FILE ADDRESSES          
    //*****************************************
    //*****************************************
    // PDEF_CNT = PDEF before branch_table     
    //*****************************************

    PDEF_CNT : INTEGER ;
    CASE_LOW : INTEGER ;
    CASE_HIGH : INTEGER ;
    CASE_LABEL : INTEGER ;
    CASE_DEFAULT : INTEGER ;
    CASE_OPNDTYPE : DATATYPE ;
    CASE_CHARTABLE : array [ CHAR ] of LBLRNG ;

    //*****************************************
    // variables for case implementation       
    //*****************************************

    NXTRG , TXRG : RGRNG ;

    //*****************************************
    // AQUIRED REGISTERS                       
    //*****************************************

    DBLALN , OPT_FLG : BOOLEAN ;

    //*****************************************
    // DWRD ALIGNMENT NEEDED, OPT. IN EFFECT   
    //*****************************************

    CSTBLK , MUSIC : BOOLEAN ;

    //*****************************************
    // STRUCT. CONST. BLOCK?, MUSIC O.S.?      
    //*****************************************

    CLEAR_REG , NEG_CND : BOOLEAN ;

    //*****************************************
    // CLEAR BEFORE LOADING THE REG.           
    //*****************************************

    SAVEFPRS , DEBUG : BOOLEAN ;

    //*****************************************
    // indicates, if we are inside of          
    // case branch table / old or new style    
    //*****************************************

    CASE_FLAG : BOOLEAN ;
    CASE_FLAG_NEW : BOOLEAN ;

    //*****************************************
    // if asm output is to be printed          
    // first_list002 = first output to list002 
    //*****************************************

    ASM : BOOLEAN ;
    FIRST_LIST002 : BOOLEAN ;

    //*****************************************
    // VARIOUS OPTIONS                         
    //*****************************************

    TRACE : BOOLEAN ;
    CKMODE , FLOW_TRACE : BOOLEAN ;

    //*****************************************
    // OBJ LISTING, FLOW-TRACING FLAGS         
    // (flow trace will probably not work      
    // at the moment - 2016)                   
    //*****************************************
    //*****************************************
    // CURRENTLY UNUSED                        
    //*****************************************

    POOL_SIZE : ICRNG ;

    //*****************************************
    // LITERAL POOL SIZE FOR STATISTICS ONLY   
    //*****************************************

    NUMLITS : INTEGER ;

    //*****************************************
    // NUMBER OF LITERALS, FOR STATISTICS      
    //*****************************************

    PCAFTLIT : ICRNG ;

    //*****************************************
    // Pcounter AFTER LITERAL DUMP             
    //*****************************************

    MDTAG : OPTYPE ;

    //*****************************************
    // MULTIPLY/DIVIDE TAG                     
    //*****************************************

    HEAPMARK : -> INTEGER ;
    TESTCNT : INTEGER ;
    ZEROBL : LVLDSP ;

    //*****************************************
    // TO CLEAR BASE ,DISPLACEMENT FIELDS      
    //*****************************************

    TOTALBYTES , ERRORCNT : INTEGER ;

    //*****************************************
    // TOTAL ERROR COUNT, ALSO RETURN CODE     
    // COUNT OF 370-ONLY INSTRUCTIONS GENERATED
    //*****************************************

    S370CNT : INTEGER ;

    //*****************************************
    // SET <=> INTEGER <=> REAL, 370 IMPL.ONLY 
    //*****************************************

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

    //************************
    // ENCODING OF TYPE FIELD 
    //************************

    STK : array [ STKPTR ] of DATUM ;

    //*******************************
    // EXPRESSION STACK              
    //*******************************

    PROCOFFSET_OLD : INTEGER ;

    //***************************************
    // keep track of register usage          
    // r15 = csp entry                       
    // r9 = filadr                           
    // r8 = call stack adr                   
    // r1 = proc offset = csp number         
    // AVAIL = AVAILABLE REGISTERS           
    // CSPACTIVE = REGS ARE ACTIVE FOR CSP   
    // filadr_loaded = special action        
    // needed because of new $PAS...         
    // runtime functions                     
    //***************************************

    AVAIL : array [ 0 .. RGCNT ] of BOOLEAN ;
    AVAILFP : array [ 0 .. FPCNT ] of BOOLEAN ;
    CSPACTIVE : array [ 0 .. 15 ] of BOOLEAN ;
    FILADR_LOADED : BOOLEAN ;

    //*******************************
    // AVAIL. F.P. REGS              
    //*******************************

    INVBRM : array [ PEQU .. PLES ] of PEQU .. PLES ;

    //**************************
    // INV. MAP OF REL. OPCODES 
    //**************************

    BRMSK : array [ PEQU .. PLES ] of 0 .. 15 ;

    //**************************
    // 370 CONDITION CODES      
    //**************************

    BRCND : - 1 .. 15 ;

    //**************************
    // ACTIVE BRANCH MASK       
    //**************************

    TIMER : POSINT ;
    HEXCHARS : array [ 0 .. 15 ] of CHAR ;

    //*************************
    // HASH TABLE, INST./PROCS 
    //*************************

    HTBL : array [ 0 .. HTSIZE ] of record
                                      NAME : BETA ;
                                      case BOOLEAN of
                                        FALSE :
                                          ( OPCDE : OPTYPE ) ;
                                        TRUE :
                                          ( SPCDE : CSPTYPE )
                                    end ;

    //*************************************
    // REMEMBERS USEFUL COND-CODE MEANINGS 
    //*************************************

    LAST_CC : record
                LAST_PC : ICRNG ;
                LOP : BYTE ;
                LR : RGRNG
              end ;

    //******************************
    // REMEMBERS CONTENTS OF REG 14 
    //******************************

    TXR_CONTENTS : record
                     VALID : BOOLEAN ;
                     LEVEL : LVLRNG ;
                     OFFSET , DISP : ADRRNG ;
                     BASE : RGRNG
                   end ;

    //************************************
    // REMEMBERS OPNDS OF LAST STR INSTR. 
    //************************************

    LAST_STR : record
                 STOPND : LVLDSP ;
                 STRGX : RGRNG ;
                 LAST_PC : ICRNG ;
                 STDT : DATATYPE ;
               end ;

    //*******************************************************
    // REMEMBERS LAST FILE USED                              
    //*******************************************************
    // comments added 01.2020 - oppolzer                     
    // this is used to avoid reloading of register 9         
    // (filadr) in case of repeated i/O operations to        
    // the same file;                                        
    // works only, if the I/O operations immediately follow  
    // each other, due to the comparison of LAST_PC being    
    // equal. LAST_PC is set in GOTO_CSP and EIO processing  
    //*******************************************************
    // There was a problem: the old file operand was not     
    // recorded in the field LAST_FILEOPERAND properly       
    // in all cases. It turned out that a sequence of        
    // SIO and EIO did not invalidate the LAST_FILEOPERAND   
    // recorded here, leading to wrong files accessed.       
    // I added the procedure SAVE_FILEOPERAND to cure this.  
    //*******************************************************

    LAST_FILE : record
                  LAST_PC : ICRNG ;
                  LAST_FILEOPERAND : LVLDSP ;
                  LAST_FILE_IS_VAR : BOOLEAN
                end ;

    //********************************
    // REMEMBERS LAST MVC INSTRUCTION 
    //********************************

    LAST_MVC : record
                 LAST_PC : ICRNG ;
                 LLEN : BYTE ;
               end ;

    //****************************************************
    // POINTERS TO LAST ELEMENTS OF 'OBJECT' CODE TABLES  
    //****************************************************

    NXTPRC , NXTEP : 0 .. PRCCNT ;
    HEXPC : HEX4 ;

    //************************************************
    // PROGRAM COUNTER DIV 2                          
    //************************************************

    PCOUNTER : ICRNG ;

    //*************************************************
    // Pcounter FOR CONSTANT BLOCK                     
    //*************************************************

    CPCOUNTER : HINTEGER ;

    //*************************************************
    // START FOR CPCOUNTER IN CURRENT SEGMENT          
    //*************************************************

    CSEGSTRT : HINTEGER ;

    //*************************************************
    // END FOR CPCOUNTER IN CURRENT SEGMENT            
    //*************************************************

    CSEGLIMIT : HINTEGER ;
    MINLBL : LBLRNG ;

    //************************************************
    // STARTING LABEL VALUE FOR CURRENT PROC          
    // DECLARATIONS FOR LITERAL TABLES ...ETC. NEEDED 
    // TO GENERATE OBJECT MODULE                      
    //************************************************

    CST_CURPNAME : array [ 1 .. IDLNGTH ] of CHAR ;
    CST_CURPNO : INTEGER ;
    CST_ASMVERB : BOOLEAN ;
    CST_GET_STAT : BOOLEAN ;
    MATCH_CURPNO : INTEGER ;

    //********************************
    //CURRENT PROC #                  
    //********************************

    DUMMYINT : INTEGER ;

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
    // literal vector for literals                                  
    // ltype = type of literal                                      
    // length = length                                              
    // xidp = index into literal pool                               
    // lnk = link into code array or zero, if notused               
    //**************************************************************

    NXTLIT : - 1 .. LITCNT ;
    LITTBL : array [ 1 .. LITCNT ] of record
                                        LTYPE : CHAR ;
                                        LENGTH : HINTEGER ;
                                        XIDP : INTEGER ;
                                        LNK : ICRNG_EXT ;
                                        XLINECNT : INTEGER ;
                                        OPTIMIZED : BOOLEAN ;
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
    CALL_MST_STACK : array [ 1 .. MAX_CALL_DEPTH ] of record
                                                   PFLEV : INTEGER ;
                                                   DISPSAV : ADRRNG
                                                   end ;

    //*****************************************************
    // PROGRAM HEADER/DATE/TIME                            
    //*****************************************************

    PROGHDR : array [ 1 .. HDRLNGTH ] of CHAR ;

    //*****************************************************
    // PCODE   = primary pcode input file                  
    // PCODE1  = first pcode include file                  
    // PCODE2  = second pcode include file                 
    // PCODE3  = third pcode include file                  
    // OBJCODE = 370 objcode output file                   
    // LIST002 = Datei fuer ASSEMBLER-Ausgabe              
    //                                                     
    // STATNAME = Name der Static Csect (falls vorhanden)  
    // posofproclen = Position des ProcLen-Feldes          
    //*****************************************************

    PCODEP : -> TEXT ;
    PCODE_FILENO : 0 .. 3 ;
    EOF_PCODE : BOOLEAN ;
    PCODE : TEXT ;
    PCODE1 : TEXT ;
    PCODE2 : TEXT ;
    PCODE3 : TEXT ;
    OBJCODE : TEXT ;
    LIST002 : TEXT ;
    TRACEF : TEXT ;
    POSOFPROCLEN : ICRNG ;

    //**************************************************************
    // table of offsets and statement numbers                       
    //**************************************************************

    TOS : array [ 1 .. 4096 ] of STAT_OFFS ;
    TOS_COUNT : INTEGER ;

    //*****************************************************
    //____________________________________________________ 
    //*****************************************************



const HEXTAB : array [ 0 .. 15 ] of CHAR = '0123456789abcdef' ;
      DATNULL : DATUM =
      ( 0.0 , NIL , 0 , 0 , ( 0 , 0 ) , 0 , FALSE , FALSE , ' ' , NON ,
        NEITHER , 0 , ( 0 , 0 ) ) ;
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
        'MCC' , 'MCV' , 'ASR' , 'XBG' , 'XEN' , 'MV1' , 'IAC' , '-?-' )
        ;
      CSPTBL : array [ CSPTYPE ] of BETA =
      ( 'N00' , 'N01' , 'N02' , 'N03' , 'N04' , 'N05' , 'N06' , 'N07' ,
        'N08' , 'N09' , 'PAG' , 'GET' , 'PUT' , 'RES' , 'REW' , 'RDC' ,
        'WRI' , 'WRE' , 'WRR' , 'WRC' , 'WRS' , 'WRX' , 'RDB' , 'WRB' ,
        'RDR' , 'RDH' , 'RDY' , 'EOL' , 'EOT' , 'RDD' , 'WRD' , 'CLK' ,
        'WLN' , 'RLN' , 'RDI' , 'EOF' , 'ELN' , 'RDS' , 'TRP' , 'XIT' ,
        'FDF' , 'SIO' , 'EIO' , 'MSG' , 'SKP' , 'LIM' , 'TRA' , 'WRP' ,
        'CLS' , 'DAT' , 'TIM' , 'FLR' , 'TRC' , 'RND' , 'WRV' , 'RDV' ,
        'RFC' , 'RFS' , 'RFV' , '-?-' ) ;



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



function LIST002_HEADLINE ( MODUS : CHAR ; PFNAME_INT : CHAR ( 8 ) ;
                          PFNAME : CHAR ( 20 ) ; PFTYP : CHAR ) :
                          INTEGER ;

//***************************************************************
// modus = S: zeilzahl setzen und drucken                        
// modus = V: nur zeilzahl setzen                                
// modus = R: zeilzahl abfragen                                  
// sonst = checken, ob zeilzahl gesetzt ist, nur dann drucken    
// in diesem Fall ein WRITELN weniger, weil ja "von aussen"      
// noch eines kommt                                              
//***************************************************************


   static ZEILZAHL : INTEGER ;
          SEITENZAHL : INTEGER ;
          SPFNAME_INT : CHAR ( 8 ) ;
          SPFNAME : CHAR ( 20 ) ;
          SPFTYP : CHAR ;

   var HEADLINE : CHAR ( 100 ) ;
       CTEMP : CHAR ( 16 ) ;

   begin (* LIST002_HEADLINE *)
     LIST002_HEADLINE := 0 ;
     if MODUS = 'R' then
       begin
         LIST002_HEADLINE := ZEILZAHL ;
         return ;
       end (* then *) ;
     if MODUS in [ 'S' , 'V' ] then
       begin
         ZEILZAHL := 0 ;
         if PFTYP <> ' ' then
           begin
             SPFNAME_INT := PFNAME_INT ;
             SPFNAME := PFNAME ;
             SPFTYP := PFTYP ;
           end (* then *)
       end (* then *) ;
     if MODUS = 'V' then
       return ;
     ZEILZAHL := ZEILZAHL - 1 ;
     if ZEILZAHL <= 0 then
       begin
         if SEITENZAHL > 0 then
           begin
             WRITELN ( LIST002 ) ;
             WRITELN ( LIST002 ) ;
             WRITELN ( LIST002 ) ;
           end (* then *) ;
         SEITENZAHL := SEITENZAHL + 1 ;
         HEADLINE := '1Stanford Pascal P-Code to 370 Translator    '
                     'Oppolzer Version of MM.YYYY     '
                     'hh:mm:ss  DD/MM/YYYY' ;
         CTEMP := VERSION ;
         MEMCPY ( ADDR ( HEADLINE [ 66 ] ) , ADDR ( CTEMP ) , 7 ) ;
         MEMCPY ( ADDR ( HEADLINE [ 78 ] ) , ADDR ( TIME ) , 8 ) ;
         MEMCPY ( ADDR ( HEADLINE [ 88 ] ) , ADDR ( DATE ) , 10 ) ;
         WRITELN ( LIST002 , HEADLINE , 'Page ' : 12 , SEITENZAHL : 4 )
                   ;
         WRITELN ( LIST002 ) ;
         if SPFTYP = '#' then
           WRITELN ( LIST002 , ' Constant and Static Section for ' ,
                     SPFNAME , ' (' , SPFNAME_INT , ')' )
         else
           if SPFTYP = 'P' then
             WRITELN ( LIST002 , ' Proc ' , SPFNAME , ' (' ,
                       SPFNAME_INT , ')' )
           else
             WRITELN ( LIST002 , ' Func ' , SPFNAME , ' (' ,
                       SPFNAME_INT , ') Result-Type ' , SPFTYP ) ;
         WRITELN ( LIST002 ) ;
         if MODUS = 'S' then
           WRITELN ( LIST002 ) ;
         ZEILZAHL := 58 ;
       end (* then *) ;
   end (* LIST002_HEADLINE *) ;



procedure LIST002_NEWLINE ;

   var DUMMYINT : INTEGER ;

   begin (* LIST002_NEWLINE *)
     DUMMYINT := LIST002_HEADLINE ( ' ' , ' ' , ' ' , ' ' ) ;
     WRITELN ( LIST002 )
   end (* LIST002_NEWLINE *) ;



procedure LIST002_PRINTLOC ( Q : INTEGER ) ;

   var REST_ZEILZAHL : INTEGER ;
       DUMMYINT : INTEGER ;

   begin (* LIST002_PRINTLOC *)
     REST_ZEILZAHL := LIST002_HEADLINE ( 'R' , ' ' , ' ' , ' ' ) ;
     if REST_ZEILZAHL <= 5 then
       begin
         DUMMYINT := LIST002_HEADLINE ( 'S' , ' ' , ' ' , ' ' ) ;
       end (* then *) ;
     WRITE ( LIST002 , ' ' , '-------------------- LOC  ' , Q : 1 ,
             ' --------------------------------' ) ;
     LIST002_NEWLINE ;
   end (* LIST002_PRINTLOC *) ;



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



procedure ERROR_SYMB ( ERRCDE : INTEGER ; ERR_SYMBOL : BETA ) ;

   begin (* ERROR_SYMB *)
     ERRORCNT := ERRORCNT + 1 ;
     GS . LAST_ERRORCODE := ERRCDE ;
     WRITE ( OUTPUT , '   ++++ Error ' , ERRCDE : 5 ) ;
     WRITE ( OUTPUT , ' (near line ' , LASTLN : 1 ) ;
     if PIAKT <> NIL then
       WRITE ( OUTPUT , ' of procedure ' , RTRIM ( PIAKT -> . CURPNAME
               ) ) ;
     WRITELN ( OUTPUT , ')' ) ;
     WRITE ( TRACEF , '   ++++ Error ' , ERRCDE : 5 ) ;
     WRITE ( TRACEF , ' (near line ' , LASTLN : 1 ) ;
     if PIAKT <> NIL then
       WRITE ( TRACEF , ' of procedure ' , RTRIM ( PIAKT -> . CURPNAME
               ) ) ;
     WRITELN ( TRACEF , ')' ) ;
     if ERR_SYMBOL <> ' ' then
       WRITELN ( OUTPUT , ' ' : 8 , 'Symbol in error = ' , ERR_SYMBOL )
                 ;
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
   end (* ERROR_SYMB *) ;



procedure ERROR ( ERRCDE : INTEGER ) ;

   begin (* ERROR *)
     ERROR_SYMB ( ERRCDE , '   ' ) ;
   end (* ERROR *) ;



procedure CHECKFREEREGS ;

//*********************************************************
// TO BE INVOKED WHEN COMPILATION STACK IS EMPTY,          
// CHECKS THAT ALL REGS HAVE BEEN MARKED AS AVAILABLE      
//*********************************************************


   var LIST : array [ 1 .. 12 ] of record
                                     RGNO : RGRNG ;
                                     KIND : CHAR
                                   end ;
       LP : 0 .. 12 ;
       I : RGRNG ;

   begin (* CHECKFREEREGS *)
     if TOP <> 1 then
       begin
         WRITELN ( OUTPUT , '****' : 7 , ' Warning: stack height =' ,
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
                   ' Warning: registers not freed ' ) ;
         for I := 1 to LP do
           WRITE ( OUTPUT , LIST [ I ] . KIND : 8 , 'PR' , LIST [ I ] .
                   RGNO : 3 ) ;
         WRITELN ( OUTPUT ) ;
         WRITE ( OUTPUT , '(near line ' : 34 , LASTLN : 1 ) ;
         if PIAKT <> NIL then
           WRITE ( OUTPUT , ' of procedure ' , RTRIM ( PIAKT -> .
                   CURPNAME ) ) ;
         WRITELN ( OUTPUT , ')' ) ;
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
     H := ( ORD ( P_OPCODE [ 1 ] ) * 64 +         // hashcode part 1
          ORD ( P_OPCODE [ 2 ] ) * 4096 +         // hashcode part 2
          ORD ( P_OPCODE [ 3 ] ) ) MOD HTSIZE ;   // hashcode part 3
     repeat
       with HTBL [ H ] do
         if NAME <> P_OPCODE then
           if NAME <> EMPTY then
             begin
               H := H + STEP ;
               if H >= HTSIZE then
                 H := H - HTSIZE ;
               continue ;

     //**********************
     // NO CHECK FOR CYCLES! 
     //**********************

             end (* then *)
           else
             if INIT then
               begin

     //****************
     // ENTER THE ITEM 
     //****************

                 NAME := P_OPCODE ;
                 if OP_SP then
                   OPCDE := OPCODE
                 else
                   SPCDE := CSP
               end (* then *)
             else
               if OP_SP then
                 OPCODE := UNDEF_OP
               else
                 CSP := UNDEF_CSP
         else
           if OP_SP then
             OPCODE := OPCDE
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



procedure DUMPAVAIL ;

   var I : INTEGER ;

   begin (* DUMPAVAIL *)
     WRITE ( TRACEF , 'Available Regs: ' ) ;
     for I := 1 to 9 do
       if AVAIL [ I ] then
         WRITE ( TRACEF , I : 3 ) ;
     WRITELN ( TRACEF ) ;
   end (* DUMPAVAIL *) ;



procedure DUMPSTKELEM ( TAG : CHAR ( 8 ) ; STK : DATUM ) ;

   const TYPNAME : array [ BOOL .. VARC ] of array [ 1 .. 4 ] of CHAR =
         ( 'BOOL' , 'CHRC' , 'ADR ' , 'HINT' , 'INT ' , 'PSET' , 'REEL'
           , 'PROC' , 'CARR' , 'VARC' ) ;

   begin (* DUMPSTKELEM *)
     with STK do
       begin
         WRITELN ( TRACEF , TAG , ' VRBL=' , VRBL : 1 , ' STKADR=' ,
                   STKADR : 5 , ' PLEN=' , PLEN : 3 , ' SCNSTNO=' ,
                   SCNSTNO : 3 ) ;
         WRITE ( TRACEF , ' ' : 8 , ' FPA=' , FPA . LVL : 3 , FPA .
                 DSPLMT : 6 , ' VPA=' , VPA ) ;
         if VRBL then
           begin
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
     DUMPAVAIL ;
     for I := STP1 to STP2 do
       begin
         WRITELN ( TRACEF , ' +++ DEPTH=' , I : 2 ) ;
         DUMPSTKELEM ( 'StkElem ' , STK [ I ] ) ;
       end (* for *)
   end (* DUMPSTK *) ;



procedure HEXHW ( HW : HINTEGER ; var HEX : HEX4 ) ;

//***********************************************
// CONVERTS HALFWORD TO 4 HEXADECIMAL CHARACTERS 
//***********************************************


   var C : INTEGER ;
       N : 1 .. 4 ;

   begin (* HEXHW *)
     C := 65536 + HW ;

     //**********************
     // ELIMINATES HW<0 CASE 
     //**********************

     for N := 4 DOWNTO 1 do
       begin
         HEX [ N ] := HEXCHARS [ C MOD 16 ] ;
         C := C DIV 16
       end (* for *) ;
   end (* HEXHW *) ;



function READNXTINST ( var PCODEF : TEXT ; MODUS : INTEGER ) : BOOLEAN
                     ;

//*****************************************************
// TO READ AND DECODE NEXT P_INSTRUCTION               
// -------------------------------------               
// 03.2021: allow comments which are added to p-code   
// instructions by the compiler following a semicolon  
// (variable names)                                    
//*****************************************************


   const SL16 = 65536 ;

   var I , J , K : INTEGER ;
       CH1 : CHAR ;
       HEX_PCOUNTER : HEX4 ;
       LEN : INTEGER ;
       BUFFER : CHAR80 ;
       OUTPOS : INTEGER ;
       BUF20 : CHAR80 ;
       LSTART : INTEGER ;
       X1 : INTEGER ;
       DUMMYNAME : array [ 1 .. IDLNGTH ] of CHAR ;
       DUMMYINT : INTEGER ;
       DUMMYBOOL : BOOLEAN ;
       DUMMYLABEL : PLABEL ;
       P_IS_CHAR : BOOLEAN ;
       Q_IS_CHAR : BOOLEAN ;
       OPERANDS : STRING ( 80 ) ;


   procedure READLBL ( var LBL : PLABEL ) ;

   //*****************************************************
   // SKIPS LEADING BLANKS AND READS THE NEXT             
   // CHARACTER SEQUENCE AS A LABEL                       
   // --------------------------------------------------- 
   //*****************************************************


      var I : INTEGER ;
          CH : CHAR ;

      begin (* READLBL *)
        with LBL do
          begin
            CADDR := 0 ;
            NAM := '        ' ;
            LEN := 0 ;
            if EOL ( PCODEF ) then
              ERROR ( 618 ) ;
            repeat
              READ ( PCODEF , CH ) ;
              LEN := LEN + 1 ;
              NAM [ LEN ] := CH ;
            until ( PCODEF -> = ' ' ) or ( LEN = 8 ) ;
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
        READ ( PCODEF , CH , CH ) ;

        //**************************
        // typ = e - d.h. empty set 
        //**************************

        if CH = 'E' then
          begin
            PSLNGTH := 0 ;
            READLN ( PCODEF ) ;
            if ASM then
              begin
                WRITE ( LIST002 , '  E()' ) ;
                LIST002_NEWLINE
              end (* then *) ;
            if FALSE then
              WRITELN ( TRACEF , '  E()' ) ;
            return
          end (* then *) ;

        //****************************************
        // typ = x - d.h. hexadezimaler bitstring 
        //****************************************

        if CH = 'X' then
          begin
            READ ( PCODEF , PSLNGTH ) ;
            Z := 30 ;
            READ ( PCODEF , CH ) ;
            if ASM then
              WRITE ( LIST002 , '  S,X' , PSLNGTH : 1 , '''' ) ;
            if FALSE then
              WRITE ( TRACEF , '  S,X' , PSLNGTH : 1 , '''' ) ;
            I := 0 ;
            while TRUE do
              begin
                READ ( PCODEF , CH1 ) ;
                if CH1 = '''' then
                  begin
                    if PCODEF -> <> ',' then
                      begin
                        if FALSE then
                          WRITELN ( TRACEF , '''' ) ;
                        break ;
                      end (* then *) ;
                    READLN ( PCODEF ) ;
                    repeat
                      READ ( PCODEF , CH ) ;
                    until CH = '''' ;
                    continue ;
                  end (* then *) ;
                READ ( PCODEF , CH2 ) ;
                if FALSE then
                  WRITE ( TRACEF , CH1 , CH2 ) ;
                I := I + 1 ;
                PSVAL . C [ I ] := CHR ( HEXVALUE ( CH1 ) * 16 +
                                   HEXVALUE ( CH2 ) ) ;
                if Z >= 70 then
                  begin
                    if ASM then
                      begin
                        WRITE ( LIST002 , ''',' ) ;
                        LIST002_NEWLINE ;
                        WRITE ( LIST002 , '''' : 27 ) ;
                      end (* then *) ;
                    Z := 27 ;
                  end (* then *) ;
                if ASM then
                  WRITE ( LIST002 , CH1 , CH2 ) ;
                if FALSE then
                  WRITE ( TRACEF , CH1 , CH2 ) ;
                Z := Z + 2 ;
              end (* while *) ;
            PSLNGTH := I ;
            READLN ( PCODEF ) ;
            if ASM then
              begin
                WRITE ( LIST002 , '''' ) ;
                LIST002_NEWLINE
              end (* then *) ;
            return
          end (* then *) ;

        //****************************************
        // typ = c - d.h. char-string             
        //****************************************

        if CH = 'C' then
          begin
            READ ( PCODEF , PSLNGTH ) ;
            Z := 30 ;
            READ ( PCODEF , CH ) ;
            if ASM then
              WRITE ( LIST002 , '  S,C' , PSLNGTH : 1 , '''' ) ;
            if FALSE then
              WRITE ( TRACEF , '  S,C' , PSLNGTH : 1 , '''' ) ;
            X := [ ] ;
            while TRUE do
              begin
                READ ( PCODEF , CH ) ;
                if CH = '''' then
                  begin
                    CH := PCODEF -> ;
                    if CH = '''' then
                      READ ( PCODEF , CH )
                    else
                      if CH = ',' then
                        begin
                          READLN ( PCODEF ) ;
                          repeat
                            READ ( PCODEF , CH ) ;
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
                        WRITE ( LIST002 , ''',' ) ;
                        LIST002_NEWLINE ;
                        WRITE ( LIST002 , '''' : 27 ) ;
                      end (* then *) ;
                    Z := 27 ;
                  end (* then *) ;
                if ASM then
                  begin
                    WRITE ( LIST002 , CH ) ;
                    Z := Z + 1 ;
                    if CH = '''' then
                      begin
                        WRITE ( LIST002 , CH ) ;
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
            READLN ( PCODEF ) ;
            if ASM then
              begin
                WRITE ( LIST002 , '''' ) ;
                LIST002_NEWLINE
              end (* then *) ;
            return
          end (* then *) ;
      end (* READSET *) ;


   procedure SKIPBLANKS ;

      begin (* SKIPBLANKS *)
        GET ( PCODEF ) ;
        if EOL ( PCODEF ) then
          ERROR ( 618 ) ;
      end (* SKIPBLANKS *) ;


   procedure READLOADINSTRUCTIONS ;

      var TYPETAG : CHAR ;
          INEU : INTEGER ;
          X2 : INTEGER ;
          LLIMIT : INTEGER ;

      begin (* READLOADINSTRUCTIONS *)
        SKIPBLANKS ;

        //*****************************************************
        // TYPE-CODE, CONSTANT OPERANDS                        
        // with type-code = m:                                 
        // length (optional) and  string constant              
        // the string constant may be split over multiple      
        // lines and may be prefixed by B or X for binary      
        // or hex content                                      
        //*****************************************************

        if ( OPCODE = PDFC ) and ( PCODEF -> = '0' ) then
          begin
            OPNDTYPE := NON ;
            READ ( PCODEF , CH1 ) ;
            READLN ( PCODEF , CH , IVAL ) ;
            SLNGTH := IVAL ;
            if ASM then
              begin
                WRITE ( LIST002 , CH1 : 3 , ',' , IVAL : 1 ) ;
                LIST002_NEWLINE
              end (* then *) ;
          end (* then *)
        else
          begin
            OPNDTYPE := TYPCDE [ PCODEF -> ] ;
            READ ( PCODEF , CH1 ) ;
            case OPNDTYPE of
              HINT , BOOL , INT :
                begin
                  READLN ( PCODEF , CH , IVAL ) ;
                  if ASM then
                    begin
                      WRITE ( LIST002 , CH1 : 3 , ',' , IVAL : 1 ) ;
                      LIST002_NEWLINE
                    end (* then *) ;
                end (* tag/ca *) ;
              CHRC : begin
                       READLN ( PCODEF , CH , CH , CH ) ;
                       IVAL := ORD ( CH ) ;
                       if ASM then
                         begin
                           WRITE ( LIST002 , 'C,''' : 5 , CH , '''' ) ;
                           LIST002_NEWLINE
                         end (* then *) ;
                     end (* tag/ca *) ;
              REEL : begin
                       READLN ( PCODEF , CH , RVAL ) ;
                       if ASM then
                         begin
                           WRITE ( LIST002 , 'R,' : 4 , RVAL : 20 ) ;
                           LIST002_NEWLINE
                         end (* then *) ;
                     end (* tag/ca *) ;
              ADR : begin
                      READLN ( PCODEF ) ;
                      IVAL := - 1 ;
                      if ASM then
                        begin
                          WRITE ( LIST002 , 'NIL' : 4 ) ;
                          LIST002_NEWLINE
                        end (* then *) ;
                    end (* tag/ca *) ;
              PSET : begin
                       READSET ;
                     end (* tag/ca *) ;
              PROC : begin
                       READ ( PCODEF , CH ) ;
                       READLBL ( LBL2 ) ;
                       READLN ( PCODEF ) ;
                       if ASM then
                         begin
                           WRITE ( LIST002 , 'P,' : 4 , LBL2 . NAM :
                                   LBL2 . LEN ) ;
                           LIST002_NEWLINE ;
                         end (* then *) ;
                     end (* tag/ca *) ;
              CARR : begin
                       LEN := - 1 ;
                       READ ( PCODEF , CH ) ;

        //**********************************
        // read optional length information 
        //**********************************

                       if not ( PCODEF -> in [ '''' , 'B' , 'X' ] )
                       then
                         begin
                           READ ( PCODEF , LEN ) ;
                           READ ( PCODEF , CH ) ;
                         end (* then *) ;

        //************************
        // read optional type tag 
        //************************

                       READ ( PCODEF , CH ) ;
                       TYPETAG := ' ' ;
                       if CH in [ 'X' , 'B' ] then
                         begin
                           TYPETAG := CH ;
                           READ ( PCODEF , CH ) ;
                         end (* then *) ;
                       SVAL := ' ' ;
                       J := 0 ;
                       CH := '''' ;
                       repeat

        //*******************************
        // read rest of line into buffer 
        //*******************************

                         READLN ( PCODEF , BUFFER ) ;
                         I := 80 ;
                         while BUFFER [ I ] = ' ' do
                           I := I - 1 ;
                         CH := BUFFER [ I ] ;

        //*********************************
        // if comma after string constant, 
        // another part follows            
        //*********************************

                         if CH = ',' then
                           begin
                             I := I - 2 ;
                             SKIPBLANKS ;
                             READ ( PCODEF , CH ) ;
                             CH := ',' ;
                           end (* then *)
                         else
                           I := I - 1 ;

        //**********************************
        // now move part of string constant 
        // to result buffer (sval),         
        // depending on type tag            
        //**********************************

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

        //**********************************
        // show what has been read          
        // on list002 ...                   
        //**********************************

                       if ASM then
                         begin
                           WRITE ( LIST002 , '  M,' , SLNGTH : 1 , ','
                                   ) ;
                           case TYPETAG of
                             'X' : LLIMIT := SLNGTH * 2 ;
                             'B' : LLIMIT := SLNGTH * 8 ;
                             otherwise
                               LLIMIT := SLNGTH ;
                           end (* case *) ;
                           if LLIMIT < 40 then
                             begin
                               if TYPETAG <> ' ' then
                                 WRITE ( LIST002 , TYPETAG ) ;
                               WRITE ( LIST002 , '''' )
                             end (* then *)
                           else
                             begin
                               LIST002_NEWLINE ;
                               WRITE ( LIST002 , ' ' , '     ' ) ;
                               if TYPETAG <> ' ' then
                                 WRITE ( LIST002 , TYPETAG ) ;
                               WRITE ( LIST002 , '''' )
                             end (* else *) ;
                           OUTPOS := 0 ;
                           for I := 1 to SLNGTH do
                             begin
                               if OUTPOS > 60 then
                                 begin
                                   WRITE ( LIST002 , ''',' ) ;
                                   LIST002_NEWLINE ;
                                   WRITE ( LIST002 , ' ' , '     ''' )
                                           ;
                                   OUTPOS := 0 ;
                                 end (* then *) ;
                               case TYPETAG of
                                 'X' : begin
                                         CH := SVAL [ I ] ;
                                         WRITEHEXBYTE ( LIST002 , ORD (
                                                   CH ) ) ;
                                         OUTPOS := OUTPOS + 2 ;
                                       end (* tag/ca *) ;
                                 'B' : begin
                                         CH := SVAL [ I ] ;
                                         WRITEBINBYTE ( LIST002 , ORD (
                                                   CH ) ) ;
                                         OUTPOS := OUTPOS + 8 ;
                                       end (* tag/ca *) ;
                                 otherwise
                                   begin
                                     CH := SVAL [ I ] ;
                                     WRITE ( LIST002 , CH ) ;
                                     OUTPOS := OUTPOS + 1 ;
                                     if CH = '''' then
                                       begin
                                         WRITE ( LIST002 , CH ) ;
                                         OUTPOS := OUTPOS + 1
                                       end (* then *) ;
                                   end (* otherw *)
                               end (* case *) ;
                             end (* for *) ;
                           WRITE ( LIST002 , '''' ) ;
                           LIST002_NEWLINE ;
                         end (* then *) ;
                     end (* tag/ca *) ;
            end (* case *)
          end (* else *)
      end (* READLOADINSTRUCTIONS *) ;


   procedure LIST_PROCEDURE_ENTRY ;

      begin (* LIST_PROCEDURE_ENTRY *)
        LIST002_PRINTLOC ( LINECNT ) ;
        HEXHW ( 2 * PCOUNTER , HEX_PCOUNTER ) ;
        WRITE ( LIST002 , ' ' , HEX_PCOUNTER : 9 , ':  ' ) ;
        WRITE ( LIST002 , LBL1 . NAM ) ;
        WRITE ( LIST002 , P_OPCODE : 4 ) ;
        WRITE ( LIST002 , CH1 : 3 , ',' ) ;
        WRITE ( LIST002 , P : 1 , ',' ) ;
        WRITE ( LIST002 , PIAKT -> . SEGSZE . NAM : 4 ) ;
        WRITE ( LIST002 , PIAKT -> . CURPNAME : IDLNGTH + 2 , ',' ) ;
        LIST002_NEWLINE ;
        WRITE ( LIST002 , ' ' , HEX_PCOUNTER : 9 , ':  ' ) ;
        WRITE ( LIST002 , ' ' : 14 ) ;
        WRITE ( LIST002 , PIAKT -> . SAVERGS : 1 , ',' ) ;
        WRITE ( LIST002 , PIAKT -> . ASM : 1 , ',' ) ;
        WRITE ( LIST002 , PIAKT -> . GET_STAT : 1 , ',' ) ;
        WRITE ( LIST002 , PIAKT -> . ASMVERB : 1 , ',' ) ;
        WRITE ( LIST002 , PIAKT -> . DEBUG_LEV : 1 , ',' ) ;
        WRITE ( LIST002 , PIAKT -> . CURPNO : 1 , ',' ) ;
        if PIAKT -> . STATNAME <> '        ' then
          WRITE ( LIST002 , PIAKT -> . STATNAME ) ;
        WRITE ( LIST002 , ',' ) ;
        if PIAKT -> . SOURCENAME <> '        ' then
          WRITE ( LIST002 , PIAKT -> . SOURCENAME ) ;
        LIST002_NEWLINE ;
      end (* LIST_PROCEDURE_ENTRY *) ;


   procedure READ_XBG ;

      begin (* READ_XBG *)
        READLN ( PCODEF , Q ) ;
        if ASM then
          begin
            WRITE ( LIST002 , ' ' : 2 , Q : 1 ) ;
            LIST002_NEWLINE
          end (* then *) ;
        if MODUS = 1 then
          begin
            if XXIANKER = NIL then
              begin
                NEW ( XXIANKER ) ;
                XXIAKT := XXIANKER
              end (* then *)
            else
              begin
                NEW ( XXIAKT -> . NEXT ) ;
                XXIAKT := XXIAKT -> . NEXT
              end (* else *) ;
            XXIAKT -> . NEXT := NIL ;
            XXIAKT -> . XBG_NO := Q ;
            XXIAKT -> . VALID := TRUE ;
          end (* then *)
        else
          begin
            XXILAUF := XXIANKER ;
            while XXILAUF <> NIL do
              begin
                if XXILAUF -> . XBG_NO = Q then
                  break ;
                XXILAUF := XXILAUF -> . NEXT ;
              end (* while *) ;
            if XXILAUF <> NIL then
              if not XXILAUF -> . VALID then
                GS . XBG_XEN_SUPPRESSED := Q ;
          end (* else *)
      end (* READ_XBG *) ;


   procedure READ_XEN ;

      begin (* READ_XEN *)
        READLN ( PCODEF , Q , CH , P ) ;
        if ASM then
          begin
            WRITE ( LIST002 , ' ' : 2 , Q : 1 , ',' , P : 1 ) ;
            LIST002_NEWLINE
          end (* then *) ;
        if MODUS = 1 then
          begin
            XXILAUF := XXIANKER ;
            while XXILAUF <> NIL do
              begin
                if XXILAUF -> . XBG_NO = Q then
                  break ;
                XXILAUF := XXILAUF -> . NEXT ;
              end (* while *) ;
            if XXILAUF <> NIL then
              XXILAUF -> . VALID := ( P <> 0 ) ;
          end (* then *)
        else
          begin
            XXILAUF := XXIANKER ;
            while XXILAUF <> NIL do
              begin
                if XXILAUF -> . XBG_NO = Q then
                  break ;
                XXILAUF := XXILAUF -> . NEXT ;
              end (* while *) ;
            if XXILAUF <> NIL then
              GS . XBG_XEN_SUPPRESSED := - 1 ;
          end (* else *)
      end (* READ_XEN *) ;


   procedure READ_DEF ;

      begin (* READ_DEF *)
        if MODUS = 1 then
          begin
            SKIPBLANKS ;
            case GS . MOD1DEFSTEP of
              0 : begin
                    READLN ( PCODEF , CH , CH , Q ) ;
                    PIAKT -> . DATA_SIZE := Q ;
                    GS . MOD1DEFSTEP := 1 ;
                  end (* tag/ca *) ;
              1 : begin
                    READLN ( PCODEF , CH , CH , Q ) ;
                    PIAKT -> . CODE_SIZE := Q ;
                    PIAKT -> . LARGE_PROC := ( PIAKT -> . CODE_SIZE >
                                             SHRT_PROC ) or DEBUG ;
                    GS . MOD1DEFSTEP := 2 ;
                  end (* tag/ca *) ;
              2 : begin
                    READLN ( PCODEF , CH , CH , Q ) ;
                    PIAKT -> . CALL_HIGHER := ( Q <> 0 ) ;
                    GS . MOD1DEFSTEP := - 1 ;
                  end (* tag/ca *) ;
              otherwise
                READLN ( PCODEF )
            end (* case *) ;
            return
          end (* then *) ;

        //***************************************
        // Type-Code and Integer or Char Operand 
        //***************************************

        SKIPBLANKS ;
        if PCODEF -> = 'C' then
          begin
            READLN ( PCODEF , CH , CH , CH , CH1 , CH ) ;
            if ASM then
              begin
                WRITE ( LIST002 , '  C,''' , CH1 : 1 , '''' ) ;
                LIST002_NEWLINE
              end (* then *) ;
            Q := ORD ( CH1 ) ;
            OPNDTYPE := TYPCDE [ 'C' ] ;
          end (* then *)
        else
          if PCODEF -> = 'I' then
            begin
              READLN ( PCODEF , CH , CH , Q ) ;
              if ASM then
                begin
                  WRITE ( LIST002 , '  I,' , Q : 1 ) ;
                  LIST002_NEWLINE
                end (* then *) ;
              OPNDTYPE := TYPCDE [ 'I' ] ;
            end (* then *)
          else
            if PCODEF -> = 'B' then
              begin
                READLN ( PCODEF , CH , CH , Q ) ;
                if ASM then
                  begin
                    WRITE ( LIST002 , '  I,' , Q : 1 ) ;
                    LIST002_NEWLINE
                  end (* then *) ;
                OPNDTYPE := TYPCDE [ 'I' ] ;
              end (* then *)
            else
              begin
                READLN ( PCODEF , Q ) ;
                if ASM then
                  begin
                    WRITE ( LIST002 , ' ' : 2 , Q : 1 ) ;
                    LIST002_NEWLINE
                  end (* then *) ;
                OPNDTYPE := TYPCDE [ 'I' ] ;
              end (* else *)
      end (* READ_DEF *) ;


   procedure READ_ENT ;

      var X : INTEGER ;
          OPTV : CHAR ( 8 ) ;

      begin (* READ_ENT *)

        //***********************************************************
        // TYPE-CODE,LEXIC-LEVEL,LABEL,THREE FLAGS,INTEGER OPERANDS  
        //***********************************************************

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
            PIAKT -> . OPNDTYPE := TYPCDE [ PCODEF -> ] ;
            READ ( PCODEF , CH1 , CH , P , CH ) ;
            READLBL ( PIAKT -> . SEGSZE ) ;
            if PCODEF -> = ' ' then
              SKIPBLANKS ;
            READ ( PCODEF , PIAKT -> . CURPNAME , CH ) ;

        //****************************************
        // change input format here - 10.05.2021  
        // support old and new format             
        //****************************************

            READ ( PCODEF , PIAKT -> . SAVERGS , CH ) ;
            if CH = ',' then
              begin
                READ ( PCODEF , PIAKT -> . ASM , CH ) ;
                READ ( PCODEF , PIAKT -> . GET_STAT , CH ) ;
                READ ( PCODEF , PIAKT -> . ASMVERB , CH ) ;
                PIAKT -> . INIT_AUTO := FALSE ;
              end (* then *)
            else
              begin
                OPTV := ' ' ;
                OPTV [ 2 ] := CH ;
                X := 2 ;
                repeat
                  X := X + 1 ;
                  READ ( PCODEF , OPTV [ X ] )
                until OPTV [ X ] = ',' ;
                PIAKT -> . ASM := OPTV [ 2 ] = 'T' ;
                PIAKT -> . GET_STAT := OPTV [ 3 ] = 'T' ;
                PIAKT -> . ASMVERB := OPTV [ 4 ] = 'T' ;
                PIAKT -> . INIT_AUTO := OPTV [ 5 ] = 'T' ;
              end (* else *) ;
            READ ( PCODEF , PIAKT -> . DEBUG_LEV , CH ) ;
            READ ( PCODEF , PIAKT -> . CURPNO , CH ) ;
            PIAKT -> . STATNAME := ' ' ;
            PIAKT -> . SOURCENAME := ' ' ;
            if PCODEF -> <> ',' then
              READ ( PCODEF , PIAKT -> . STATNAME , CH )
            else
              READ ( PCODEF , CH ) ;
            READ ( PCODEF , PIAKT -> . SOURCENAME ) ;
            if PCODEF -> = ',' then
              READ ( PCODEF , CH ) ;
            if PCODEF -> <> ' ' then
              READ ( PCODEF , PIAKT -> . SCRATCHPOS ) ;
            READLN ( PCODEF ) ;
            DEBUG := PIAKT -> . DEBUG_LEV >= 2 ;
            PIAKT -> . FLOW_TRACE := PIAKT -> . DEBUG_LEV >= 3 ;
            return ;
          end (* then *)
        else
          begin
            SKIPBLANKS ;
            READ ( PCODEF , CH1 , CH , P , CH ) ;
            READLBL ( DUMMYLABEL ) ;
            if PCODEF -> = ' ' then
              SKIPBLANKS ;

        //****************************************
        // change input format here - 10.05.2021  
        // support old and new format             
        //****************************************

            READ ( PCODEF , DUMMYNAME , CH , DUMMYBOOL , CH ) ;
            if CH = ',' then
              READ ( PCODEF , DUMMYBOOL , CH , DUMMYBOOL , CH ,
                     DUMMYBOOL , CH )
            else
              repeat
                READ ( PCODEF , CH )
              until CH = ',' ;
            READ ( PCODEF , DUMMYINT , CH , MATCH_CURPNO , CH ) ;
            READLN ( PCODEF ) ;
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
            PCOUNTER := 0 ;

        //************************************************************
        // init tables of offsets and statement numbers               
        //************************************************************

            TOS_COUNT := 1 ;
            TOS [ 1 ] . STMT := LINECNT ;
            TOS [ 1 ] . OFFS := PCOUNTER ;
            if FIRST_LIST002 then
              begin
                REWRITE ( LIST002 ) ;
                FIRST_LIST002 := FALSE
              end (* then *) ;
            DUMMYINT := LIST002_HEADLINE ( 'S' , LBL1 . NAM , PIAKT ->
                        . CURPNAME , CH1 ) ;
            if ASM then
              LIST_PROCEDURE_ENTRY
          end (* else *)
      end (* READ_ENT *) ;


   begin (* READNXTINST *)
     GS . LAST_ERRORCODE := 0 ;
     P := 0 ;
     Q := 0 ;
     LBL1 . LEN := 0 ;
     if PCODEF -> <> ' ' then
       begin
         READLBL ( LBL1 ) ;
       end (* then *) ;
     GET ( PCODEF ) ;
     if PCODEF -> = ' ' then
       SKIPBLANKS ;

     //************************************************************
     // P-Opcode einlesen                                          
     //************************************************************

     READ ( PCODEF , P_OPCODE ) ;

     //************************************************************
     // when reading the first time, only certain P-Opcodes are    
     // of interest                                                
     //************************************************************

     if MODUS = 1 then
       begin
         if ( P_OPCODE <> 'ENT' ) and ( P_OPCODE <> 'RET' ) and (
         P_OPCODE <> 'DEF' ) and ( P_OPCODE <> 'STP' ) and ( P_OPCODE
         <> 'XBG' ) and ( P_OPCODE <> 'XEN' ) then
           begin
             READLN ( PCODEF ) ;
             READNXTINST := EOF ( PCODEF ) ;
             return ;
           end (* then *) ;
       end (* then *)

     //************************************************************
     // when reading the second time, there may be                 
     // XBG/XEN suppressing in effect ... if so, only              
     // XEN P-Codes need to be interpreted, because only XEN       
     // may terminate the XBG/XEN suppressing                      
     //************************************************************

     else
       begin
         if GS . XBG_XEN_SUPPRESSED > 0 then
           if P_OPCODE <> 'XEN' then
             begin
               READLN ( PCODEF ) ;
               READNXTINST := EOF ( PCODEF ) ;
               return ;
             end (* then *) ;
         if ASM and ( P_OPCODE <> 'LOC' ) and ( P_OPCODE <> 'ENT' ) and
         ( P_OPCODE <> 'CST' ) then
           begin
             if P_OPCODE = 'DFC' then
               HEXHW ( LBL1 . CADDR , HEX_PCOUNTER )
             else
               HEXHW ( 2 * PCOUNTER , HEX_PCOUNTER ) ;
             WRITE ( LIST002 , ' ' , HEX_PCOUNTER : 9 , ':  ' , LBL1 .
                     NAM : LBL1 . LEN , ' ' : 6 - LBL1 . LEN , P_OPCODE
                     : 6 ) ;
           end (* then *)
       end (* else *) ;

     //************************************************************
     // achtung, nur uebergangsweise, bis V-Befehle                
     // korrekt implementiert sind                                 
     //************************************************************

     ENTERLOOKUP ;

     //************************************************************
     // achtung, nur uebergangsweise, bis V-Befehle                
     // korrekt implementiert sind                                 
     //************************************************************

     if OPCODE = PENT then
       begin
         if TOP <> 1 then
           ERROR ( 701 ) ;
         if FALSE then
           begin
             WRITELN ( TRACEF ) ;
             WRITELN ( TRACEF ) ;
             WRITELN ( TRACEF , 'Neuer Entry ' , LBL1 . NAM ) ;
             WRITELN ( TRACEF , 'TOP   = ' , TOP ) ;
           end (* then *) ;
         TOP := 1 ;
       end (* then *) ;

     //************************************************************
     // achtung, nur uebergangsweise, bis V-Befehle                
     // korrekt implementiert sind                                 
     // achtung, nur uebergangsweise, bis V-Befehle                
     // korrekt implementiert sind                                 
     //************************************************************

     case OPCODE of

     //************************************************************
     // pcodes with no operands                                    
     //************************************************************

       PADI , PADR , PSBI , PSBR , PFLT , PFLO , PNGI , PNGR , PSQI ,
       PSQR , PABI , PABR , PMOD , PODD , PMPI , PMPR , PDVI , PDVR ,
       PUNI , PINT , PDIF , PINN , PCRD , PLAB , PSAV , PRST , PCHR ,
       PORD , PXPO , PPOP , PXLB , PADA , PSBA , PMCP :
         begin
           READLN ( PCODEF ) ;
           if ASM then
             LIST002_NEWLINE ;
         end (* tag/ca *) ;
       PEND : begin
                READLN ( PCODEF ) ;
                if ASM then
                  LIST002_NEWLINE ;
                ASM := FALSE ;
              end (* tag/ca *) ;

     //************************************************************
     // XBG creates a linked list of its parameters and            
     // the indicators on the corresponding XEN instructions       
     // during pass 1                                              
     // When in pass 2, XBG suppresses the reading of              
     // P-instructions until the corresponding XEN is found        
     // (if the indicator - boolean VALID - is off)                
     //************************************************************

       PXBG : READ_XBG ;
       PXEN : READ_XEN ;
       PSTP : begin
                if MODUS = 1 then
                  begin
                    READLN ( PCODEF ) ;
                    READNXTINST := EOF ( PCODEF ) ;
                    return
                  end (* then *) ;

     //*************
     // NO OPERANDS 
     //*************

                READLN ( PCODEF ) ;
                if ASM then
                  LIST002_NEWLINE ;
              end (* tag/ca *) ;
       PDEF : READ_DEF ;
       PCTI , PIXA , PASE , PMOV , PMV1 , PMFI , PMZE , PMSE , PDBG :
         begin

     //*****************
     // INTEGER OPERAND 
     //*****************

           READLN ( PCODEF , Q ) ;
           if ASM then
             begin
               WRITE ( LIST002 , ' ' : 2 , Q : 1 ) ;
               LIST002_NEWLINE
             end (* then *) ;
         end (* tag/ca *) ;
       PLOC : begin

     //*****************
     // INTEGER OPERAND 
     //*****************

                READLN ( PCODEF , Q ) ;
                if ASM then
                  LIST002_PRINTLOC ( Q ) ;
                LINECNT := Q
              end (* tag/ca *) ;
       PAND , PIOR , PXOR , PNOT :
         begin

     //****************************
     // TYPE-CODE; if blank then b 
     //****************************

           GET ( PCODEF ) ;
           CH1 := PCODEF -> ;
           if CH1 = ' ' then
             CH1 := 'B' ;
           OPNDTYPE := TYPCDE [ CH1 ] ;
           READLN ( PCODEF ) ;
           if ASM then
             begin
               WRITE ( LIST002 , CH1 : 3 ) ;
               LIST002_NEWLINE
             end (* then *) ;
         end (* tag/ca *) ;
       PINC , PDEC , PIND , PIAC :
         begin

     //*****************************************
     // TYPE-CODE AND INTEGER OPERANDS          
     // 03.2021: operands contains comments     
     //*****************************************

           SKIPBLANKS ;
           READLN ( PCODEF , OPERANDS ) ;
           READSTR ( OPERANDS , CH1 , CH , Q ) ;
           OPNDTYPE := TYPCDE [ CH1 ] ;
           if ASM then
             begin
               WRITE ( LIST002 , ' ' : 2 , OPERANDS ) ;
               LIST002_NEWLINE
             end (* then *) ;
         end (* tag/ca *) ;
       PNEW , PLDA , PSMV , PSLD , PSCL , PMST , PASR :
         begin

     //*****************************************
     // TWO INTEGER OPERANDS                    
     // 03.2021: operands contains comments     
     //*****************************************

           SKIPBLANKS ;
           READLN ( PCODEF , OPERANDS ) ;
           READSTR ( OPERANDS , P , CH , Q ) ;
           if ASM then
             begin
               WRITE ( LIST002 , ' ' : 2 , OPERANDS ) ;
               LIST002_NEWLINE
             end (* then *) ;
         end (* tag/ca *) ;
       PLOD , PSTR :
         begin

     //************************************************************
     // TYPE-CODE AND TWO INTEGER OPERANDS                         
     // TYPE-CODE may be zero with STR                             
     // then no store is required, only pop of stack element       
     // 03.2021: operands contains comments                        
     //************************************************************

           SKIPBLANKS ;
           READLN ( PCODEF , OPERANDS ) ;
           READSTR ( OPERANDS , CH1 , CH , P , CH , Q ) ;
           if CH1 = '0' then
             OPNDTYPE := NON
           else
             OPNDTYPE := TYPCDE [ CH1 ] ;
           if ASM then
             begin
               WRITE ( LIST002 , ' ' : 2 , OPERANDS ) ;
               LIST002_NEWLINE
             end (* then *) ;
         end (* tag/ca *) ;
       PPAK : begin

     //************************
     // THREE INTEGER OPERANDS 
     //************************

                SKIPBLANKS ;
                READLN ( PCODEF , OPERANDS ) ;
                READSTR ( OPERANDS , IVAL , CH , P , CH , Q ) ;
                if ASM then
                  begin
                    WRITE ( LIST002 , ' ' : 2 , OPERANDS ) ;
                    LIST002_NEWLINE ;
                  end (* then *) ;
              end (* tag/ca *) ;
       PCHK : begin

     //**********************************************************
     // TYPE-CODE AND TWO INTEGER OPERANDS                       
     // change 28.08.2020:                                       
     // to enable portable chk instructions for char subranges   
     // the two integers may be specified as char constants      
     // (single chars surrounded by apostrophs)                  
     //**********************************************************

                P_IS_CHAR := FALSE ;
                Q_IS_CHAR := FALSE ;
                SKIPBLANKS ;
                OPNDTYPE := TYPCDE [ PCODEF -> ] ;
                READ ( PCODEF , CH1 , CH ) ;
                if PCODEF -> = '''' then
                  begin
                    READ ( PCODEF , CH ) ;
                    READ ( PCODEF , CH ) ;
                    P := ORD ( CH ) ;
                    P_IS_CHAR := TRUE ;
                    READ ( PCODEF , CH ) ;
                  end (* then *)
                else
                  READ ( PCODEF , P ) ;
                READ ( PCODEF , CH ) ;
                if PCODEF -> = '''' then
                  begin
                    READ ( PCODEF , CH ) ;
                    READ ( PCODEF , CH ) ;
                    Q := ORD ( CH ) ;
                    Q_IS_CHAR := TRUE ;
                    READ ( PCODEF , CH ) ;
                  end (* then *)
                else
                  READ ( PCODEF , Q ) ;
                READLN ( PCODEF ) ;
                if ASM then
                  begin
                    WRITE ( LIST002 , CH1 : 3 , ',' ) ;
                    if P_IS_CHAR then
                      WRITE ( LIST002 , '''' , CHR ( P ) , '''' )
                    else
                      WRITE ( LIST002 , P : 1 ) ;
                    WRITE ( LIST002 , ',' ) ;
                    if Q_IS_CHAR then
                      WRITE ( LIST002 , '''' , CHR ( Q ) , '''' )
                    else
                      WRITE ( LIST002 , Q : 1 ) ;
                    LIST002_NEWLINE ;
                  end (* then *) ;
              end (* tag/ca *) ;
       PEQU , PNEQ , PLES , PGRT , PLEQ , PGEQ , PSTO :
         begin

     //************************************************************
     // type code and possibly length operands                     
     // 12.2020:                                                   
     // type M: two length parameters for left and right side      
     // new type 1: left is char 1, right is char array, 1 length  
     // new type 2: right is char 1, left is char array, 1 length  
     // 01.2021:                                                   
     // type M,1,2: two length parameters for left and right side  
     //************************************************************

           SKIPBLANKS ;
           COMPTYPE := PCODEF -> ;
           case COMPTYPE of
             'M' , '1' , '2' :
               begin
                 OPNDTYPE := TYPCDE [ 'M' ] ;
                 READ ( PCODEF , CH1 , CH , Q ) ;
                 if PCODEF -> = ',' then
                   READ ( PCODEF , CH , P )
                 else
                   P := Q ;
                 READLN ( PCODEF ) ;
                 if ASM then
                   begin
                     WRITE ( LIST002 , CH1 : 3 , ',' , Q : 1 , ',' , P
                             : 1 ) ;
                     LIST002_NEWLINE
                   end (* then *) ;
               end (* tag/ca *) ;
             otherwise
               begin
                 OPNDTYPE := TYPCDE [ COMPTYPE ] ;
                 READLN ( PCODEF , CH1 ) ;
                 if ASM then
                   begin
                     WRITE ( LIST002 , CH1 : 3 ) ;
                     LIST002_NEWLINE
                   end (* then *) ;
               end (* otherw *)
           end (* case *) ;
         end (* tag/ca *) ;
       PRET : begin
                if MODUS = 1 then
                  begin
                    GS . MOD1DEFSTEP := 0 ;
                    READLN ( PCODEF ) ;
                    READNXTINST := EOF ( PCODEF ) ;
                    return
                  end (* then *) ;

     //*******************************************
     // TYPE-CODE AND POSSIBLY AN INTEGER OPERAND 
     //*******************************************

                SKIPBLANKS ;
                OPNDTYPE := TYPCDE [ PCODEF -> ] ;
                if OPNDTYPE = CARR then
                  begin
                    READLN ( PCODEF , CH1 , CH , Q ) ;
                    if ASM then
                      begin
                        WRITE ( LIST002 , CH1 : 3 , ',' , Q : 1 ) ;
                        LIST002_NEWLINE
                      end (* then *) ;
                  end (* then *)
                else
                  begin
                    READLN ( PCODEF , CH1 ) ;
                    if ASM then
                      begin
                        WRITE ( LIST002 , CH1 : 3 ) ;
                        LIST002_NEWLINE
                      end (* then *) ;
                  end (* else *) ;
              end (* tag/ca *) ;
       PFJP , PUJP , PCTS , PUXJ :
         begin

     //********************
     // LABEL-NAME OPERAND 
     //********************

           READLBL ( LBL2 ) ;
           READLN ( PCODEF ) ;
           if ASM then
             begin
               WRITE ( LIST002 , ' ' : 2 , LBL2 . NAM : LBL2 . LEN ) ;
               LIST002_NEWLINE
             end (* then *) ;
         end (* tag/ca *) ;
       PXJP : begin

     //********************
     // LABEL-NAME OPERAND 
     //********************

                SKIPBLANKS ;
                READLN ( PCODEF , BUF20 ) ;
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
                      begin
                        WRITE ( LIST002 , ' ' , LBL2 . NAM : LBL2 . LEN
                                ) ;
                        LIST002_NEWLINE ;
                      end (* then *)
                    else
                      begin
                        WRITE ( LIST002 , ' ' , XJPFLAG , ',' , LBL2 .
                                NAM : LBL2 . LEN ) ;
                        LIST002_NEWLINE ;
                      end (* else *)
                  end (* then *)
              end (* tag/ca *) ;
       PCST : begin

     //**********************************
     // PROCEDURE NAME & NUMBER OPERANDS 
     //**********************************

                READLN ( PCODEF , CH1 , CST_CURPNAME , CST_CURPNO , CH
                         , ASM , CH , CST_GET_STAT , CH , CST_ASMVERB )
                         ;
                if ASM then
                  begin
                    if FIRST_LIST002 then
                      begin
                        REWRITE ( LIST002 ) ;
                        FIRST_LIST002 := FALSE
                      end (* then *) ;
                    DUMMYINT := LIST002_HEADLINE ( 'S' , LBL1 . NAM ,
                                CST_CURPNAME , '#' ) ;
                    WRITE ( LIST002 , ' ' , '     0000:  ' , LBL1 . NAM
                            : LBL1 . LEN , ' ' : 6 - LBL1 . LEN ,
                            P_OPCODE : 4 ) ;
                    WRITE ( LIST002 , CST_CURPNAME : IDLNGTH + 2 ,
                            CST_CURPNO : 4 , ',' , ASM : 1 , ',' ,
                            CST_GET_STAT : 1 , ',' , CST_ASMVERB : 1 )
                            ;
                    LIST002_NEWLINE ;
                  end (* then *) ;
              end (* tag/ca *) ;
       PCUP : begin

     //***************************************************
     // TYPE-CODE,LEXIC-LEVEL,LABEL-NAME,INTEGER OPERANDS 
     //***************************************************

                SKIPBLANKS ;
                OPNDTYPE := TYPCDE [ PCODEF -> ] ;
                READ ( PCODEF , CH1 ) ;
                EXTLANG := ' ' ;
                if PCODEF -> <> ',' then
                  begin
                    EXTLANG := PCODEF -> ;
                    READ ( PCODEF , CH ) ;
                  end (* then *) ;
                READ ( PCODEF , CH , P , CH ) ;
                READLBL ( LBL2 ) ;
                if PCODEF -> = ' ' then
                  SKIPBLANKS ;
                READLN ( PCODEF , CH , Q ) ;
                if ASM then
                  begin
                    WRITE ( LIST002 , CH1 : 3 ) ;
                    if EXTLANG <> ' ' then
                      WRITE ( LIST002 , EXTLANG ) ;
                    WRITE ( LIST002 , ',' , P : 1 ) ;
                    WRITE ( LIST002 , ',' , LBL2 . NAM : LBL2 . LEN ) ;
                    WRITE ( LIST002 , ',' , Q : 1 ) ;
                    LIST002_NEWLINE ;
                  end (* then *)
              end (* tag/ca *) ;
       PBGN : begin

     //****************
     // STRING OPERAND 
     //****************

                READLN ( PCODEF , CH , PROGHDR ) ;
                if ASM then
                  begin
                    WRITE ( LIST002 , ' ' , PROGHDR ) ;
                    LIST002_NEWLINE
                  end (* then *) ;
              end (* tag/ca *) ;
       PENT : READ_ENT ;
       PLDC , PLCA , PDFC :
         READLOADINSTRUCTIONS ;
       PCSP : begin

     //***********************************
     // SUBMONITOR OPERATION NAME OPERAND 
     //***********************************

                SKIPBLANKS ;
                READ ( PCODEF , P_OPCODE ) ;
                if FALSE then
                  WRITE ( TRACEF , 'read = ' , P_OPCODE ) ;
                if PCODEF -> = ',' then
                  begin
                    READLN ( PCODEF , CH , PROCOFFSET ) ;
                  end (* then *)
                else
                  begin
                    READLN ( PCODEF ) ;
                    PROCOFFSET := 0 ;
                  end (* else *) ;
                OP_SP := FALSE ;
                ENTERLOOKUP ;
                OP_SP := TRUE ;
                if ASM then
                  begin
                    WRITE ( LIST002 , P_OPCODE : 5 , ',' , PROCOFFSET :
                            1 ) ;
                    LIST002_NEWLINE ;
                  end (* then *) ;
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

           READLN ( PCODEF , P , CH , Q ) ;
           if ASM then
             begin
               WRITE ( LIST002 , ' ' : 2 , P : 1 , ',' , Q : 1 ) ;
               LIST002_NEWLINE
             end (* then *) ;
         end (* tag/ca *) ;
       PVLD , PVST :
         begin

     //************************************************************
     // TWO INTEGER OPERANDS                                       
     // the first one is a mode indicator                          
     //************************************************************

           READLN ( PCODEF , P , CH , Q ) ;
           if ASM then
             begin
               WRITE ( LIST002 , ' ' : 2 , P : 1 , ',' , Q : 1 ) ;
               LIST002_NEWLINE
             end (* then *) ;
         end (* tag/ca *) ;
       PVC1 , PVCC , PVLM , PVIX , PVRP :
         begin

     //************************************************************
     // no operands                                                
     //************************************************************

           READLN ( PCODEF ) ;
           if ASM then
             LIST002_NEWLINE ;
         end (* tag/ca *) ;
       PVC2 , PVMV , PVSM :
         begin

     //************************************************************
     // one integer operand                                        
     //************************************************************

           READLN ( PCODEF , Q ) ;
           if ASM then
             begin
               WRITE ( LIST002 , ' ' : 2 , Q : 1 ) ;
               LIST002_NEWLINE
             end (* then *) ;
         end (* tag/ca *) ;
       PMCC : begin

     //************************************************************
     // one integer operand                                        
     //************************************************************

                READLN ( PCODEF , Q ) ;
                if ASM then
                  begin
                    WRITE ( LIST002 , ' ' : 2 , Q : 1 ) ;
                    LIST002_NEWLINE
                  end (* then *) ;
              end (* tag/ca *) ;
       PMCV : begin

     //************************************************************
     // no operands                                                
     //************************************************************

                READLN ( PCODEF ) ;
                if ASM then
                  LIST002_NEWLINE ;
              end (* tag/ca *) ;
       otherwise
         begin

     //***************************
     // OPCODE NOT FOUND IN TABLE 
     //***************************

           WRITE ( OUTPUT , '   ++++ Error Info: ' ) ;
           WRITE ( OUTPUT , LBL1 . NAM : LBL1 . LEN , ' ' , P_OPCODE ,
                   ' ' ) ;
           while not EOLN ( PCODEF ) do
             begin
               WRITE ( OUTPUT , PCODEF -> ) ;
               if ASM then
                 WRITE ( LIST002 , PCODEF -> ) ;
               GET ( PCODEF )
             end (* while *) ;
           WRITELN ( OUTPUT ) ;
           if ASM then
             LIST002_NEWLINE ;
           READLN ( PCODEF ) ;
           ERROR ( 606 ) ;
         end (* otherw *) ;
     end (* case *) ;
     READNXTINST := EOF ( PCODEF ) ;
   end (* READNXTINST *) ;



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

//***************************************************
// CONVERTS PROGRAM COUNTER VALUES TO 370            
// BASE/DISPLACEMENT HALF WORDS                      
// ------------------------------------------------- 
//***************************************************


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
     if FALSE then
       $ERROR ( 999 ) ;
     ERROR ( 254 )
   end (* BASE_DSPLMT *) ;



procedure GENRR ( OP : BYTE ; R1 , R2 : RGRNG ) ;

   begin (* GENRR *)
     if R1 = TRG14 then
       TXR_CONTENTS . VALID := FALSE ;
     if OPT_FLG then
       if ( OP = XLTR ) or ( OP = XLTDR ) then
         with LAST_CC do
           if PCOUNTER = LAST_PC then

     //*****************************
     // NO INTERVENING INSTRUCTIONS 
     //*****************************

             if R1 = R2 then
               if LR = R1 then
                 if OP = XLTDR then
                   if LOP in [ XAD , XSD , XLCDR , XLPDR , XADR , XSDR
                   , XAD , XSD ] then
                     return
                   else
                     
                 else

     //***********
     // OP = XLTR 
     //***********

                   if LOP in [ XLPR , XLCR , XNR , XORX , XXR , XAR ,
                   XSR , XAH , XSH , XO , XX , XN , XSLA , XSRA , XA ,
                   XS ] then
                     return ;

     //*********************************
     // write symbolic instr to list002 
     //*********************************

     if ASM then
       begin
         HEXHW ( PCOUNTER * 2 , HEXPC ) ;
         WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
         WRITE ( LIST002 , XTBLN [ OP ] : COLASMI , ' ' : SPACEASMI ,
                 R1 : 1 , ',' , R2 : 1 ) ;
         LIST002_NEWLINE ;
       end (* then *) ;

     //********************************
     // insert instr into code buffer  
     //********************************

     CODE . H [ PCOUNTER ] := TO_HINT ( OP * SL8 + R1 * 16 + R2 ) ;

     //********************************
     // increment instr counter        
     //********************************

     PCOUNTER := NEXTPC ( 1 ) ;
     with LAST_CC do
       begin
         LAST_PC := PCOUNTER ;
         LR := R1 ;
         LOP := OP
       end (* with *) ;
   end (* GENRR *) ;



procedure GENRXLIT_EXTENDED ( OP : BYTE ; R : RGRNG ; D : INTEGER ; TAG
                            : INTEGER ; EX_OPCODE : INTEGER ) ;

   FORWARD ;



procedure GENRXLIT ( OP : BYTE ; R : RGRNG ; D : INTEGER ; TAG :
                   INTEGER ) ;

   begin (* GENRXLIT *)
     GENRXLIT_EXTENDED ( OP , R , D , TAG , 0 ) ;
   end (* GENRXLIT *) ;



procedure GENRX_2 ( OP : BYTE ; R : RGRNG ; D : ADRRNG ; X , B : RGRNG
                  ; OPTION : INTEGER ) ;

   begin (* GENRX_2 *)
     if R = TRG14 then
       TXR_CONTENTS . VALID := FALSE ;
     if FALSE then
       begin
         WRITELN ( TRACEF , '---------------------------------------' )
                   ;
         WRITELN ( TRACEF , 'genrx_2 at linecnt = ' , LINECNT : 1 ) ;
         WRITELN ( TRACEF , 'op  = ' , XTBLN [ OP ] ) ;
         WRITELN ( TRACEF , 'r   = ' , R ) ;
         WRITELN ( TRACEF , 'd   = ' , D ) ;
         WRITELN ( TRACEF , 'x   = ' , X ) ;
         WRITELN ( TRACEF , 'b   = ' , B ) ;
         WRITELN ( TRACEF , 'opt = ' , OPTION ) ;
       end (* then *) ;
     if ( D < 0 ) or ( D > SHRTINT ) then
       begin

     //*******************************
     //THIS SHOULD NOT BE THE CASE NOW
     //*******************************

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

     //*********************************
     // write symbolic instr to list002 
     //*********************************

     if ASM then
       begin
         HEXHW ( PCOUNTER * 2 , HEXPC ) ;
         WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
         WRITE ( LIST002 , XTBLN [ OP ] : COLASMI , ' ' : SPACEASMI , R
                 : 1 , ',' ) ;
         case OPTION of
           99 : ;
           3 : begin
                 WRITE ( LIST002 , '<constant>' ) ;
                 if ( X > 0 ) or ( B > 0 ) then
                   begin
                     WRITE ( LIST002 , '(' , X : 1 ) ;
                     if B > 0 then
                       WRITE ( LIST002 , ',' , B : 1 ) ;
                     WRITE ( LIST002 , ')' ) ;
                   end (* then *) ;
               end (* tag/ca *) ;
           2 : WRITE ( LIST002 , '<constant>' ) ;
           1 : begin
                 WRITE ( LIST002 , D : 1 ) ;
                 if ( X > 0 ) or ( B > 0 ) then
                   begin
                     WRITE ( LIST002 , '(' , X : 1 ) ;
                     if B > 0 then
                       WRITE ( LIST002 , ',' , B : 1 ) ;
                     WRITE ( LIST002 , ')' ) ;
                   end (* then *) ;
               end (* tag/ca *)
         end (* case *) ;
         if OPTION <> 99 then
           LIST002_NEWLINE ;
       end (* then *) ;

     //********************************
     // insert instr into code buffer  
     //********************************

     CODE . H [ PCOUNTER ] := TO_HINT ( OP * SL8 + R * 16 + X ) ;
     CODE . H [ PCOUNTER + 1 ] := TO_HINT ( SL12 * B + D ) ;

     //********************************
     // increment instr counter        
     //********************************

     PCOUNTER := NEXTPC ( 2 ) ;
     with LAST_CC do
       begin
         LAST_PC := PCOUNTER ;
         LR := R ;
         LOP := OP
       end (* with *) ;
   end (* GENRX_2 *) ;



procedure GENRX ( OP : BYTE ; R : RGRNG ; D : ADRRNG ; X , B : RGRNG )
                ;

   begin (* GENRX *)
     GENRX_2 ( OP , R , D , X , B , 1 )
   end (* GENRX *) ;



procedure MANAGE_LITERALS ( MODUS : INTEGER ; var PCOUNTER : ICRNG ;
                          PLITERAL : ANYPTR ; LLITERAL : INTEGER ; var
                          RESULT : INTEGER ) ;

//********************************************************
// modus = 0: literal pool zuruecksetzen                  
// modus = 1: literal pool schreiben in code array        
// modus = 2: literal einfuegen double                    
// modus = 3: literal einfuegen short                     
// modus = 4: literal einfuegen int                       
// modus = 5: literal einfuegen large set                 
// modus = 6: char literale checken und ggf. einfuegen    
// modus = 7: pruefen, ob kritische grenze erreicht ist   
//********************************************************


   const SEPLINE = '----------------------------------------' ;
         CHCNT = 1600 ;   // size of literal pool in bytes
         DBLCNT = 200 ;   // must be chcnt div 8

   type

        //******************************************************
        // pegel - gibt an, bis wohin der neue pool belegt ist  
        // dbl_status - fuer jedes dbl element den belegt-status
        //              0 = komplett frei                       
        //              8 = komplett belegt (mit double o.a.)   
        //              2 = frei ab position 2 (d.h. hw belegt) 
        //              4 = frei ab position 4 usw.             
        //******************************************************

        LIT_POOL_CONTROL_NEW = record
                                 PEGEL : 0 .. CHCNT ;
                                 DBL_STATUS : array [ 0 .. DBLCNT ] of
                                              0 .. 8 ;
                               end ;

   static

          //****************************************************
          // new pool                                           
          // and new control information for new pool           
          //****************************************************

          NEW_POOL : array [ 0 .. CHCNT ] of CHAR ;
          LXN : LIT_POOL_CONTROL_NEW ;

   var PR : -> REAL ;
       PH : -> HINTEGER ;
       PI : -> INTEGER ;
       PS : -> LARGE_SET ;
       DUMMYINT : INTEGER ;


   procedure INIT_NEW_LITERAL_POOl ;

      begin (* INIT_NEW_LITERAL_POO *)
        MEMSET ( ADDR ( NEW_POOL ) , CHR ( 0 ) , CHCNT ) ;
        MEMSET ( ADDR ( LXN ) , CHR ( 0 ) , SIZEOF ( LXN ) ) ;
        NXTLIT := 0 ;
      end (* INIT_NEW_LITERAL_POO *) ;


   procedure UPD_DBLTBL_NEW ( PCOUNTER : ICRNG ; R : REAL ) ;

      var DBL_PEGEL : INTEGER ;
          IFOUND : INTEGER ;
          I : INTEGER ;
          PR : -> REAL ;
          LETZT_BELEGT : 0 .. 8 ;

      begin (* UPD_DBLTBL_NEW *)
        DBL_PEGEL := LXN . PEGEL DIV 8 ;
        LETZT_BELEGT := LXN . PEGEL MOD 8 ;
        if LETZT_BELEGT > 0 then
          DBL_PEGEL := DBL_PEGEL + 1 ;

        //******************************************************
        // look if value r is already present in pool           
        // only check double fields where status is 8           
        // (completely filled)                                  
        //******************************************************

        IFOUND := - 1 ;
        for I := 0 to DBL_PEGEL - 1 do
          if LXN . DBL_STATUS [ I ] = 8 then
            begin
              PR := ADDR ( NEW_POOL [ I * 8 ] ) ;
              if R = PR -> then
                begin
                  IFOUND := I * 8 ;
                  break
                end (* then *)
            end (* then *) ;

        //********************************************
        // if found, use index ifound                 
        // else use new double field at end of pool   
        //********************************************

        if IFOUND < 0 then
          begin
            IFOUND := DBL_PEGEL * 8 ;
            PR := ADDR ( NEW_POOL [ IFOUND ] ) ;
            PR -> := R ;
            LXN . DBL_STATUS [ DBL_PEGEL ] := 8 ;
            LXN . PEGEL := DBL_PEGEL * 8 + 8 ;
          end (* then *) ;

        //******************************************************
        // insert pointer to literal pool into code             
        //******************************************************

        CODE . H [ PCOUNTER ] := TO_HINT ( IFOUND ) ;

        //******************************************************
        // insert literal into literal table                    
        //******************************************************

        NXTLIT := NXTLIT + 1 ;
        LITTBL [ NXTLIT ] . XLINECNT := LINECNT ;
        LITTBL [ NXTLIT ] . LNK := PCOUNTER ;
        LITTBL [ NXTLIT ] . LTYPE := 'D' ;
        LITTBL [ NXTLIT ] . LENGTH := 8 ;
        LITTBL [ NXTLIT ] . XIDP := IFOUND ;
        LITTBL [ NXTLIT ] . OPTIMIZED := FALSE ;
        if TRACE_LITERAL then
          begin
            WRITELN ( TRACEF , SEPLINE ) ;
            WRITELN ( TRACEF , 'upd_dbltbl - linecnt = ' , LINECNT ) ;
            WRITELN ( TRACEF , 'upd_dbltbl - index   = ' , NXTLIT ) ;
            WRITELN ( TRACEF , 'upd_dbltbl - lnk/pc  = ' , PCOUNTER ) ;
            WRITELN ( TRACEF , 'upd_dbltbl - ltype   = ' , 'D' ) ;
            WRITELN ( TRACEF , 'upd_dbltbl - length  = ' , 8 ) ;
            WRITELN ( TRACEF , 'upd_dbltbl - xidp    = ' , IFOUND ) ;
            WRITELN ( TRACEF , 'upd_dbltbl - opt     = ' , FALSE ) ;
          end (* then *) ;
      end (* UPD_DBLTBL_NEW *) ;


   procedure UPD_HWTBL_NEW ( PCOUNTER : ICRNG ; H : HINTEGER ) ;

      var DBL_PEGEL : INTEGER ;
          IFOUND : INTEGER ;
          I : INTEGER ;
          IFC : INTEGER ;
          IDBL : INTEGER ;
          IFREE : INTEGER ;
          IFREE4 : INTEGER ;
          PH : -> HINTEGER ;
          LETZT_BELEGT : 0 .. 8 ;
          LBELEGT : INTEGER ;

      begin (* UPD_HWTBL_NEW *)
        DBL_PEGEL := LXN . PEGEL DIV 8 ;
        LETZT_BELEGT := LXN . PEGEL MOD 8 ;
        if LETZT_BELEGT > 0 then
          DBL_PEGEL := DBL_PEGEL + 1 ;

        //******************************************************
        // look if value h is already present in pool           
        // btw: look for place where to insert new value of h   
        // if needed (luecke) => ifree or ifree4                
        //******************************************************

        IFOUND := - 1 ;
        IFREE := - 1 ;
        IFREE4 := - 1 ;
        if FALSE then
          WRITELN ( TRACEF , SEPLINE ) ;
        for I := 0 to DBL_PEGEL - 1 do
          begin
            LBELEGT := LXN . DBL_STATUS [ I ] ;
            PH := ADDR ( NEW_POOL [ I * 8 ] ) ;
            IFC := I * 8 ;
            if LBELEGT in [ 2 , 6 ] then
              if IFREE < 0 then
                IFREE := IFC + LBELEGT ;
            if LBELEGT = 4 then
              if IFREE4 < 0 then
                IFREE4 := IFC + LBELEGT ;
            while LBELEGT > 0 do
              begin
                if FALSE then
                  begin
                    WRITE ( TRACEF , 'upd_hwtbl  - check   = ' ) ;
                    WRITE ( TRACEF , IFC : 5 ) ;
                    WRITE ( TRACEF , LXN . DBL_STATUS [ I ] : 2 ) ;
                    WRITE ( TRACEF , ' for ' , H ) ;
                    WRITE ( TRACEF , ' content = ' , PH -> ) ;
                    WRITELN ( TRACEF ) ;
                  end (* then *) ;
                if H = PH -> then
                  begin
                    IFOUND := IFC ;
                    break
                  end (* then *) ;
                PH := PTRADD ( PH , 2 ) ;
                IFC := IFC + 2 ;
                LBELEGT := LBELEGT - 2 ;
              end (* while *) ;
            if IFOUND >= 0 then
              break
          end (* for *) ;

        //********************************************
        // if found, use index ifound                 
        // else if luecke found, use luecke           
        // else use free field at end of used pool    
        //********************************************

        if IFOUND < 0 then
          begin
            if IFREE < 0 then
              if IFREE4 >= 0 then
                IFREE := IFREE4 ;
            if IFREE >= 0 then
              IFOUND := IFREE
            else
              IFOUND := LXN . PEGEL ;
            PH := ADDR ( NEW_POOL [ IFOUND ] ) ;
            PH -> := H ;
            if IFOUND >= LXN . PEGEL then
              LXN . PEGEL := IFOUND + 2 ;
            IDBL := IFOUND DIV 8 ;
            LXN . DBL_STATUS [ IDBL ] := LXN . DBL_STATUS [ IDBL ] + 2
                                         ;
            if FALSE then
              begin
                WRITELN ( TRACEF , 'upd_hwtbl  - ifound  = ' , IFOUND )
                          ;
                WRITELN ( TRACEF , 'upd_hwtbl  - pegel   = ' , LXN .
                          PEGEL ) ;
                WRITELN ( TRACEF , 'upd_hwtbl  - dbl_st  = ' , LXN .
                          DBL_STATUS [ IDBL ] ) ;
              end (* then *) ;
          end (* then *) ;

        //******************************************************
        // insert pointer to literal pool into code             
        //******************************************************

        CODE . H [ PCOUNTER ] := TO_HINT ( IFOUND ) ;

        //******************************************************
        // insert literal into literal table                    
        //******************************************************

        NXTLIT := NXTLIT + 1 ;
        LITTBL [ NXTLIT ] . XLINECNT := LINECNT ;
        LITTBL [ NXTLIT ] . LNK := PCOUNTER ;
        LITTBL [ NXTLIT ] . LTYPE := 'H' ;
        LITTBL [ NXTLIT ] . LENGTH := 2 ;
        LITTBL [ NXTLIT ] . XIDP := IFOUND ;
        LITTBL [ NXTLIT ] . OPTIMIZED := FALSE ;
        if TRACE_LITERAL then
          begin
            WRITELN ( TRACEF , SEPLINE ) ;
            WRITELN ( TRACEF , 'upd_hwtbl  - linecnt = ' , LINECNT ) ;
            WRITELN ( TRACEF , 'upd_hwtbl  - index   = ' , NXTLIT ) ;
            WRITELN ( TRACEF , 'upd_hwtbl  - lnk/pc  = ' , PCOUNTER ) ;
            WRITELN ( TRACEF , 'upd_hwtbl  - h       = ' , H ) ;
            WRITELN ( TRACEF , 'upd_hwtbl  - ltype   = ' , 'H' ) ;
            WRITELN ( TRACEF , 'upd_hwtbl  - length  = ' , 2 ) ;
            WRITELN ( TRACEF , 'upd_hwtbl  - xidp    = ' , IFOUND ) ;
            WRITELN ( TRACEF , 'upd_hwtbl  - opt     = ' , FALSE ) ;
          end (* then *) ;
      end (* UPD_HWTBL_NEW *) ;


   procedure UPD_INTTBL_NEW ( PCOUNTER : ICRNG ; D : INTEGER ) ;

      var DBL_PEGEL : INTEGER ;
          IFOUND : INTEGER ;
          I : INTEGER ;
          IFC : INTEGER ;
          IDBL : INTEGER ;
          IFREE4 : INTEGER ;
          PD : -> INTEGER ;
          LETZT_BELEGT : 0 .. 8 ;
          LBELEGT : INTEGER ;

      begin (* UPD_INTTBL_NEW *)
        DBL_PEGEL := LXN . PEGEL DIV 8 ;
        IDBL := DBL_PEGEL ;
        LETZT_BELEGT := LXN . PEGEL MOD 8 ;
        if LETZT_BELEGT > 0 then
          DBL_PEGEL := DBL_PEGEL + 1 ;

        //******************************************************
        // look if value d is already present in pool           
        // btw: look for place where to insert new value of d   
        // if needed (luecke) => ifree4                         
        //******************************************************

        IFOUND := - 1 ;
        IFREE4 := - 1 ;
        for I := 0 to DBL_PEGEL - 1 do
          begin
            LBELEGT := LXN . DBL_STATUS [ I ] ;
            PD := ADDR ( NEW_POOL [ I * 8 ] ) ;
            IFC := I * 8 ;
            if LBELEGT = 4 then
              if IFREE4 < 0 then
                IFREE4 := IFC + LBELEGT ;
            while LBELEGT > 0 do
              begin
                if D = PD -> then
                  begin
                    IFOUND := IFC ;

        //******************************************************
        // very old error: int literal matches halfword         
        // and following (unused) zero halfword, but level is   
        // not incremented - fixed 05.01.2023 - bernd oppolzer  
        //******************************************************

                    if LBELEGT < 4 then
                      begin
                        LXN . DBL_STATUS [ I ] := LXN . DBL_STATUS [ I
                                                  ] + 2 ;
                        if IFOUND + 4 > LXN . PEGEL then
                          LXN . PEGEL := IFOUND + 4
                      end (* then *) ;
                    break
                  end (* then *) ;
                PD := PTRADD ( PD , 4 ) ;
                IFC := IFC + 4 ;
                LBELEGT := LBELEGT - 4 ;
              end (* while *) ;
            if IFOUND >= 0 then
              break
          end (* for *) ;

        //******************************************** 
        // if found, use index ifound                  
        // else if luecke found, use luecke            
        // else use free field at end of used pool     
        // (don't create 2 byte luecke in the middle   
        // of existing 8 byte slot; if letzt_belegt is 
        // 2 (and not 0 or 4), use new 8 byte slot;    
        // also: if letzt_belegt is 6, a 4 byte-element
        // cannot be stored in current 8 byte slot)    
        //******************************************** 

        if IFOUND < 0 then
          begin
            if IFREE4 >= 0 then
              IFOUND := IFREE4
            else
              begin
                if LETZT_BELEGT in [ 0 , 4 ] then
                  IFOUND := LXN . PEGEL
                else
                  IFOUND := DBL_PEGEL * 8 ;
              end (* else *) ;
            PD := ADDR ( NEW_POOL [ IFOUND ] ) ;
            PD -> := D ;
            if IFOUND >= LXN . PEGEL then
              LXN . PEGEL := IFOUND + 4 ;
            IDBL := IFOUND DIV 8 ;
            LXN . DBL_STATUS [ IDBL ] := LXN . DBL_STATUS [ IDBL ] + 4
                                         ;
            if FALSE then
              begin
                WRITELN ( TRACEF , SEPLINE ) ;
                WRITELN ( TRACEF , 'upd_inttbl - ifound  = ' , IFOUND )
                          ;
                WRITELN ( TRACEF , 'upd_inttbl - pegel   = ' , LXN .
                          PEGEL ) ;
                WRITELN ( TRACEF , 'upd_inttbl - dbl_st  = ' , LXN .
                          DBL_STATUS [ IDBL ] ) ;
              end (* then *) ;
          end (* then *) ;

        //******************************************************
        // insert pointer to literal pool into code             
        //******************************************************

        CODE . H [ PCOUNTER ] := TO_HINT ( IFOUND ) ;

        //******************************************************
        // insert literal into literal table                    
        //******************************************************

        NXTLIT := NXTLIT + 1 ;
        LITTBL [ NXTLIT ] . XLINECNT := LINECNT ;
        LITTBL [ NXTLIT ] . LNK := PCOUNTER ;
        LITTBL [ NXTLIT ] . LTYPE := 'I' ;
        LITTBL [ NXTLIT ] . LENGTH := 4 ;
        LITTBL [ NXTLIT ] . XIDP := IFOUND ;
        LITTBL [ NXTLIT ] . OPTIMIZED := FALSE ;
        if TRACE_LITERAL then
          begin
            WRITELN ( TRACEF , SEPLINE ) ;
            WRITELN ( TRACEF , 'upd_inttbl - linecnt = ' , LINECNT ) ;
            WRITELN ( TRACEF , 'upd_inttbl - index   = ' , NXTLIT ) ;
            WRITELN ( TRACEF , 'upd_inttbl - lnk/pc  = ' , PCOUNTER ) ;
            WRITELN ( TRACEF , 'upd_inttbl - d       = ' , D ) ;
            WRITELN ( TRACEF , 'upd_inttbl - ltype   = ' , 'I' ) ;
            WRITELN ( TRACEF , 'upd_inttbl - length  = ' , 4 ) ;
            WRITELN ( TRACEF , 'upd_inttbl - xidp    = ' , IFOUND ) ;
            WRITELN ( TRACEF , 'upd_inttbl - opt     = ' , FALSE ) ;
          end (* then *) ;
      end (* UPD_INTTBL_NEW *) ;


   function CHECK_NEW_LITERAL ( SLNGTH : INTEGER ; PSTR : -> CHAR ;
                              ONLY4 : BOOLEAN ; LITTYPE : CHAR ; var
                              IFOUND : INTEGER ) : INTEGER ;

   //*****************************************************
   // this should work almost the same as the old         
   // function check_char_literal                         
   // but it should use the new pool                      
   //*****************************************************
   // the input values for the literal to check are:      
   // SLNGTH - length of the literal                      
   // PSTR   - the literal char array                     
   //*****************************************************


      var TAG : array [ 1 .. 3 ] of CHAR ;
          XOFFS : INTEGER := 0 ;
          I : INTEGER := 0 ;
          DBL_PEGEL : INTEGER ;
          IDBL : INTEGER ;
          LETZT_BELEGT : 0 .. 8 ;
          PSVAL : -> CHAR ( 256 ) ;

      begin (* CHECK_NEW_LITERAL *)
        IFOUND := - 1 ;

        //*****************************************************
        // look if literal is already in pool                  
        // a literal qualifies, if                             
        // 1) it has type C or S                               
        // 2) it has a length >= the length of the new one     
        // 3) it starts or ends with the same characters       
        //    as the new one                                   
        // 4) if only4 is on: if has an offset which is        
        //    a multiple of 4                                  
        //*****************************************************

        for I := 1 to NXTLIT do
          with LITTBL [ I ] do
            if LTYPE in [ 'C' , 'S' ] then
              if LENGTH >= SLNGTH then
                begin
                  if MEMCMPX ( PSTR , ADDR ( NEW_POOL [ XIDP ] ) ,
                  SLNGTH ) = 0 then
                    begin
                      if ONLY4 then
                        if XIDP MOD 4 <> 0 then
                          continue ;
                      IFOUND := XIDP ;
                      break
                    end (* then *) ;
                  if LENGTH > SLNGTH then
                    begin
                      XOFFS := LENGTH - SLNGTH ;
                      if MEMCMPX ( PSTR , ADDR ( NEW_POOL [ XIDP +
                      XOFFS ] ) , SLNGTH ) = 0 then
                        begin
                          if ONLY4 then
                            if XIDP + XOFFS MOD 4 <> 0 then
                              continue ;
                          IFOUND := XIDP + XOFFS ;
                          break
                        end (* then *) ;
                    end (* then *)
                end (* then *) ;

        //*****************************************************
        // if so, reuse; if not, add                           
        // reuse means: add entry in littbl, but don't add     
        // literal to literal pool (reuse literal there)       
        //*****************************************************

        if IFOUND >= 0 then
          begin
            TAG := 'use' ;
            NXTLIT := NXTLIT + 1 ;
            LITTBL [ NXTLIT ] . XLINECNT := LINECNT ;
            LITTBL [ NXTLIT ] . LNK := - TOP - 1 ;
            LITTBL [ NXTLIT ] . LTYPE := LITTYPE ;
            LITTBL [ NXTLIT ] . LENGTH := SLNGTH ;
            LITTBL [ NXTLIT ] . XIDP := IFOUND ;
            LITTBL [ NXTLIT ] . OPTIMIZED := FALSE ;
            if TRACE_LITERAL then
              begin
                WRITELN ( TRACEF , SEPLINE ) ;
                WRITELN ( TRACEF , 'reuse lit. - linecnt = ' , LINECNT
                          ) ;
                WRITELN ( TRACEF , 'reuse lit. - index   = ' , NXTLIT )
                          ;
                WRITELN ( TRACEF , 'reuse lit. - lnk/pc  = ' , - TOP -
                          1 ) ;
                WRITELN ( TRACEF , 'reuse lit. - ltype   = ' , LITTYPE
                          ) ;
                WRITELN ( TRACEF , 'reuse lit. - length  = ' , SLNGTH )
                          ;
                WRITELN ( TRACEF , 'reuse lit. - xidp    = ' , IFOUND )
                          ;
                WRITELN ( TRACEF , 'reuse lit. - opt     = ' , FALSE )
                          ;
              end (* then *) ;
          end (* then *)
        else
          begin

        //*****************************************************
        // add literal to pool                                 
        //*****************************************************

            if LITTYPE = 'S' then
              begin
                DBL_PEGEL := LXN . PEGEL DIV 8 ;
                LETZT_BELEGT := LXN . PEGEL MOD 8 ;
                if LETZT_BELEGT > 0 then
                  DBL_PEGEL := DBL_PEGEL + 1 ;
                IFOUND := DBL_PEGEL * 8 ;
              end (* then *)
            else
              IFOUND := LXN . PEGEL ;
            LXN . PEGEL := IFOUND + SLNGTH ;
            if ODD ( LXN . PEGEL ) then
              LXN . PEGEL := LXN . PEGEL + 1 ;
            TAG := 'add' ;
            NXTLIT := NXTLIT + 1 ;
            LITTBL [ NXTLIT ] . XLINECNT := LINECNT ;
            LITTBL [ NXTLIT ] . LNK := - TOP - 1 ;
            LITTBL [ NXTLIT ] . LTYPE := LITTYPE ;
            LITTBL [ NXTLIT ] . LENGTH := SLNGTH ;
            LITTBL [ NXTLIT ] . XIDP := IFOUND ;
            LITTBL [ NXTLIT ] . OPTIMIZED := FALSE ;
            if TRACE_LITERAL then
              begin
                WRITELN ( TRACEF , SEPLINE ) ;
                WRITELN ( TRACEF , 'add liter. - linecnt = ' , LINECNT
                          ) ;
                WRITELN ( TRACEF , 'add liter. - index   = ' , NXTLIT )
                          ;
                WRITELN ( TRACEF , 'add liter. - lnk/pc  = ' , - TOP -
                          1 ) ;
                WRITELN ( TRACEF , 'add liter. - ltype   = ' , LITTYPE
                          ) ;
                WRITELN ( TRACEF , 'add liter. - length  = ' , SLNGTH )
                          ;
                WRITELN ( TRACEF , 'add liter. - xidp    = ' , IFOUND )
                          ;
                WRITELN ( TRACEF , 'add liter. - opt     = ' , FALSE )
                          ;
              end (* then *) ;
            MEMCPY ( ADDR ( NEW_POOL [ IFOUND ] ) , PSTR , SLNGTH ) ;

        //*****************************************************
        // set Status fields in DBL slots in between ifound    
        // and new pegel                                       
        //*****************************************************
        // if (for example) LXN.PEGEL = 10, then               
        // DBL_PEGEL := 1                                      
        // LXN . DBL_STATUS [ 0 ] := 8                         
        // LXN . DBL_STATUS [ 1 ] := 2                         
        //*****************************************************

            IDBL := IFOUND DIV 8 ;
            DBL_PEGEL := LXN . PEGEL DIV 8 ;
            LETZT_BELEGT := LXN . PEGEL MOD 8 ;
            while IDBL < DBL_PEGEL do
              begin
                LXN . DBL_STATUS [ IDBL ] := 8 ;
                IDBL := IDBL + 1
              end (* while *) ;
            if LETZT_BELEGT > 0 then
              LXN . DBL_STATUS [ DBL_PEGEL ] := LETZT_BELEGT ;
          end (* else *) ;

        //*****************************************************
        // show entry info in literal pool                     
        //*****************************************************

        if TRACE_LITERAL then
          begin
            WRITE ( TRACEF , TAG , ' literal nr. ' , NXTLIT : 1 ) ;
            if LITTYPE = 'C' then
              begin
                PSVAL := PTRADD ( PSTR , 0 ) ;
                WRITE ( TRACEF , ' sval = <' , PSVAL -> : SLNGTH , '>'
                        ) ;
              end (* then *) ;
            WRITELN ( TRACEF ) ;
          end (* then *) ;

        //*****************************************************
        // return function result                              
        //*****************************************************

        CHECK_NEW_LITERAL := NXTLIT
      end (* CHECK_NEW_LITERAL *) ;


   procedure UPD_SETTBL_NEW ( PCOUNTER : ICRNG ; PS : LARGE_SET ; L :
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
          IFOUND : INTEGER ;
          LITOK : INTEGER ;

      begin (* UPD_SETTBL_NEW *)
        S_I . S := PS ;

        //***********************
        // show error when l = 0 
        //***********************

        if L = 0 then
          begin
            ERROR ( 616 ) ;
            return
          end (* then *) ;

        //******************************************
        // set literal of length 4 - use upd_inttbl 
        //******************************************

        if L <= 4 then
          begin
            UPD_INTTBL_NEW ( PCOUNTER , S_I . I [ 1 ] ) ;
            return
          end (* then *) ;

        //******************************************
        // set literal of length 8 - use upd_dbltbl 
        //******************************************

        if L <= 8 then
          begin
            UPD_DBLTBL_NEW ( PCOUNTER , S_I . R [ 1 ] ) ;
            return ;
          end (* then *) ;

        //******************************************************
        // longer set literal                                   
        // use check literal but accept only offsets which      
        // are multiples of 4                                   
        //******************************************************
        // with sets, LNK is set to PCOUNTER                    
        //******************************************************

        LITOK := CHECK_NEW_LITERAL ( L , ADDR ( S_I . C [ 1 ] ) , TRUE
                 , 'S' , IFOUND ) ;
        LITTBL [ LITOK ] . LNK := PCOUNTER ;

        //******************************************************
        // insert pointer to literal pool into code             
        //******************************************************

        CODE . H [ PCOUNTER ] := TO_HINT ( IFOUND ) ;
      end (* UPD_SETTBL_NEW *) ;


   procedure DUMP_LITERALS_NEW ( var PCOUNTER : ICRNG ) ;

   //*************************************************
   // PROCEDURE TO EMPTY LITERAL POOL INTO CODE ARRAY 
   //*************************************************


      var I : INTEGER ;
          QPC : ICRNG ;
          TPC : ICRNG_EXT ;
          TEMPSIZE : INTEGER ;
          PX , PY : ANYPTR ;


      procedure BRANCH_CHAIN ( LAST_PC : ICRNG ) ;

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

           //*****************************
           // MUST BE UNINDEXED BC INSTR. 
           //*****************************

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
                     if DPC >= LAST_PC then
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

           //******
           // RR   
           //******

             else
               if TI . I < XC000 then
                 TPC := TPC + 2

           //******
           // RX   
           //******

               else
                 TPC := TPC + 3 ;

           //******
           // SS   
           //******

           until TPC >= LAST_PC ;
         end (* BRANCH_CHAIN *) ;


      begin (* DUMP_LITERALS_NEW *)
        if OPT_FLG then
          if not DEBUG then
            BRANCH_CHAIN ( PCOUNTER ) ;

        //*************************************
        // first adjust pcounter to full word  
        //*************************************

        if ODD ( PCOUNTER ) then
          GENRR ( XBCR , 0 , 0 ) ;

        //*************************************
        // and then to double word, if needed  
        //*************************************

        if DBLALN then
          if ( PCOUNTER MOD 4 ) <> 0 then
            GENRX ( XBC , 0 , 0 , 0 , 0 ) ;

        //************************************************
        // see if literals must be stored                 
        // tempsize = size of literal pool in full words  
        // pcounter = half words                          
        //************************************************

        if NXTLIT > 0 then
          begin
            TEMPSIZE := ( LXN . PEGEL + 3 ) DIV 4 ;
            POOL_SIZE := POOL_SIZE + TEMPSIZE * 4 ;
            if PCOUNTER + TEMPSIZE * 2 <= 8187 then
              begin
                if TRACE_LITERAL then
                  WRITELN ( TRACEF , 'dump literals ' , ' - linecnt = '
                            , LINECNT : 1 ) ;
                for I := 1 to NXTLIT do
                  begin
                    if TRACE_LITERAL then
                      begin
                        WRITELN ( TRACEF , SEPLINE ) ;
                        WRITELN ( TRACEF , 'littbl.linecnt = ' , LITTBL
                                  [ I ] . XLINECNT ) ;
                        WRITELN ( TRACEF , 'littbl.index   = ' , I ) ;
                        WRITELN ( TRACEF , 'littbl.lnk/pc  = ' , LITTBL
                                  [ I ] . LNK ) ;
                        WRITELN ( TRACEF , 'littbl.ltype   = ' , LITTBL
                                  [ I ] . LTYPE ) ;
                        WRITELN ( TRACEF , 'littbl.length  = ' , LITTBL
                                  [ I ] . LENGTH ) ;
                        WRITELN ( TRACEF , 'littbl.xidp    = ' , LITTBL
                                  [ I ] . XIDP ) ;
                        WRITELN ( TRACEF , 'littbl.opt     = ' , LITTBL
                                  [ I ] . OPTIMIZED ) ;
                      end (* then *) ;
                    TPC := LITTBL [ I ] . LNK ;
                    if TPC > 0 then

        //******************************************************
        // USUAL CASE                                           
        //******************************************************

                      begin
                        QPC := CODE . H [ TPC ] ;
                        if TRACE_LITERAL then
                          WRITELN ( TRACEF , 'code old       = ' , QPC
                                    ) ;
                        CODE . H [ TPC ] := TO_HINT ( BASE_DSPLMT ( QPC
                                            DIV 2 + PCOUNTER ) ) ;
                        if ODD ( QPC ) then
                          CODE . H [ TPC ] := TO_HINT ( CODE . H [ TPC
                                              ] + 1 ) ;
                        if TRACE_LITERAL then
                          WRITELN ( TRACEF , 'code new       = ' , CODE
                                    . H [ TPC ] ) ;
                      end (* then *)
                    else
                      begin
                        if not LITTBL [ I ] . OPTIMIZED then
                          begin
                            ERROR ( 257 ) ;
                            WRITELN ( TRACEF ,
                              '*** error 257 *** literal not used ***'
                                      ) ;
                          end (* then *)
                      end (* else *)
                  end (* for *) ;

        //******************************************************
        // copy literal pool into code array                    
        //******************************************************

                NUMLITS := NUMLITS + NXTLIT ;
                QPC := PCOUNTER DIV 2 ;
                PX := ADDR ( CODE . I [ QPC ] ) ;
                PY := ADDR ( NEW_POOL [ 0 ] ) ;
                MEMCPY ( PX , PY , TEMPSIZE * 4 ) ;
                QPC := QPC + TEMPSIZE ;
                PCOUNTER := QPC * 2 ;
              end (* then *)
            else
              ERROR ( 255 ) ;
          end (* then *) ;

        //******************************************************
        // reset literal pool                                   
        //******************************************************

        INIT_NEW_LITERAL_POOL ;
        DBLALN := FALSE ;
        PCAFTLIT := PCOUNTER ;
      end (* DUMP_LITERALS_NEW *) ;


   begin (* MANAGE_LITERALS *)
     RESULT := 0 ;

     //*******************************************************
     // this function contains different functions to manage  
     // the literal pool                                      
     //*******************************************************

     case MODUS of

     //*********************
     // reset literal pool  
     //*********************

       0 : INIT_NEW_LITERAL_POOL ;

     //*****************************
     // dump literals to code area  
     //*****************************

       1 : DUMP_LITERALS_NEW ( PCOUNTER ) ;

     //**********************************
     // enter 8 bytes into literal pool  
     //**********************************

       2 : begin
             PR := PLITERAL ;
             UPD_DBLTBL_NEW ( PCOUNTER , PR -> ) ;
           end (* tag/ca *) ;

     //**********************************
     // enter 2 bytes into literal pool  
     //**********************************

       3 : begin
             PH := PLITERAL ;
             UPD_HWTBL_NEW ( PCOUNTER , PH -> ) ;
           end (* tag/ca *) ;

     //**********************************
     // enter 4 bytes into literal pool  
     //**********************************

       4 : begin
             PI := PLITERAL ;
             UPD_INTTBL_NEW ( PCOUNTER , PI -> ) ;
           end (* tag/ca *) ;

     //****************************************************
     // enter set string of arb. length into literal pool  
     //****************************************************

       5 : begin
             PS := PLITERAL ;
             UPD_SETTBL_NEW ( PCOUNTER , PS -> , LLITERAL ) ;
           end (* tag/ca *) ;

     //***********************************************
     // check for location of string in literal pool  
     //***********************************************

       6 : RESULT := CHECK_NEW_LITERAL ( LLITERAL , PLITERAL , FALSE ,
                     'C' , DUMMYINT ) ;

     //*******************************
     // danger if result is positive  
     //*******************************

       7 : RESULT := LXN . PEGEL + 100 - CHCNT ;
     end (* case *)
   end (* MANAGE_LITERALS *) ;



procedure UPD_DBLTBL ( PCOUNTER : ICRNG ; R : REAL ) ;

   var DUMMY : INTEGER ;

   begin (* UPD_DBLTBL *)
     MANAGE_LITERALS ( 2 , PCOUNTER , ADDR ( R ) , 0 , DUMMY )
   end (* UPD_DBLTBL *) ;



procedure UPD_HWTBL ( PCOUNTER : ICRNG ; H : HINTEGER ) ;

   var DUMMY : INTEGER ;

   begin (* UPD_HWTBL *)
     MANAGE_LITERALS ( 3 , PCOUNTER , ADDR ( H ) , 0 , DUMMY )
   end (* UPD_HWTBL *) ;



procedure UPD_INTTBL ( PCOUNTER : ICRNG ; D : INTEGER ) ;

   var DUMMY : INTEGER ;

   begin (* UPD_INTTBL *)
     MANAGE_LITERALS ( 4 , PCOUNTER , ADDR ( D ) , 0 , DUMMY )
   end (* UPD_INTTBL *) ;



procedure UPD_SETTBL ( PCOUNTER : ICRNG ; PS : LARGE_SET ; L : INTEGER
                     ) ;

   var DUMMY : INTEGER ;

   begin (* UPD_SETTBL *)
     MANAGE_LITERALS ( 5 , PCOUNTER , ADDR ( PS ) , L , DUMMY )
   end (* UPD_SETTBL *) ;



function CHECK_CHAR_LITERAL : INTEGER ;

   var RESULT : INTEGER ;

   begin (* CHECK_CHAR_LITERAL *)
     MANAGE_LITERALS ( 6 , PCOUNTER , ADDR ( SVAL ) , SLNGTH , RESULT )
                       ;
     CHECK_CHAR_LITERAL := RESULT
   end (* CHECK_CHAR_LITERAL *) ;



procedure GENRXLIT_EXTENDED ;

   var DLEFT , DRIGHT : INTEGER ;
       OP1 , OP2 : INTEGER ;

   begin (* GENRXLIT_EXTENDED *)
     if R = TRG14 then
       TXR_CONTENTS . VALID := FALSE ;
     if TAG >= 0 then
       if ( OP >= XL ) and ( OP <= XS ) then
         if ( D >= - 32768 ) and ( D <= 32767 ) then
           begin
             OP := OP - 16 ;

     //*********************
     // USE HALFWORD INSTR. 
     //*********************

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

     //*********************************
     // write symbolic instr to list002 
     //*********************************

     if ASM then
       begin
         HEXHW ( PCOUNTER * 2 , HEXPC ) ;
         WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
         WRITE ( LIST002 , XTBLN [ OP ] : COLASMI , ' ' : SPACEASMI , R
                 : 1 ) ;
         if TAG < 0 then
           begin
             WRITE ( LIST002 , ',=H''' , D : 1 , '''' ) ;
             LIST002_NEWLINE ;
           end (* then *)
         else
           if TAG = 0 then
             begin
               WRITE ( LIST002 , ',=F''' , D : 1 , '''' ) ;
               if OP = XEX then
                 begin
                   if EX_OPCODE = XOI then
                     WRITE ( LIST002 , '   OI 0(R1),X''00''' )
                   else
                     begin
                       OP1 := D and 0xffff ;
                       OP2 := OP1 and 0xfff ;
                       OP1 := OP1 DIV 4096 ;
                       WRITE ( LIST002 , '   CLI ' , OP2 : 1 , '(R' ,
                               OP1 : 1 , '),X''00''' )
                     end (* else *)
                 end (* then *) ;
               LIST002_NEWLINE ;
             end (* then *)
           else
             begin
               DLEFT := D and ( not 0xffff ) ;
               DRIGHT := D and 0xffff ;
               DLEFT := DLEFT DIV 65536 ;
               WRITE ( LIST002 , ',=H''' , DLEFT : 1 , ',' , DRIGHT : 1
                       , '''' ) ;
               LIST002_NEWLINE ;
             end (* else *)
       end (* then *) ;

     //********************************
     // insert instr into code buffer  
     //********************************

     CODE . H [ PCOUNTER ] := TO_HINT ( OP * SL8 + R * 16 ) ;
     if FALSE then
       WRITELN ( TRACEF , 'genrxlit - tag = ' , TAG ) ;
     if TAG < 0 then
       UPD_HWTBL ( PCOUNTER + 1 , D )
     else
       UPD_INTTBL ( PCOUNTER + 1 , D ) ;

     //********************************
     // increment instr counter        
     //********************************

     PCOUNTER := NEXTPC ( 2 ) ;
     with LAST_CC do
       begin
         LAST_PC := PCOUNTER ;
         LR := R ;
         LOP := OP
       end (* with *)
   end (* GENRXLIT_EXTENDED *) ;



procedure ASMNXTINST ;

//*******************************************************************
// TO TRANSLATE THE NEXT P_INSTRUCTION INTO 370 ASSEMBLY/OBJECT CODE 
// ----------------------------------------------------------------- 
//*******************************************************************


   var P1 , P2 , B1 , B2 : LVLRNG := 0 ;
       Q1 , Q2 : ADRRNG := 0 ;
       I , J : INTEGER := 0 ;
       OPPTR : STKPTR := 0 ;
       RGADR1 : RGRNG := 0 ;
       RGADR2 : RGRNG := 0 ;
       LBLX : PLABEL ;
       C : CHAR ;
       RESULT_DANGER : INTEGER ;

       //*************************************************
       // THE FOLLOWING PROCEDURES ARE FOR OBJECT CODE    
       // GENERATION ONLY                                 
       // ----------------------------------------------- 
       //*************************************************



   procedure INS_PRCTBL ( PRC_NAME : ALFA ; VPOS : ICRNG ) ;

   //*****************************************************
   // Insert External Reference                           
   // -------------------------                           
   // an der vorgegebenen Position (VPos) befindet sich   
   // eine externe Referenz mit dem angegeben Namen       
   // (V-Adresse); diese ist bislang noch nicht vor-      
   // handen und soll von nachfolgenden L-Befehlen        
   // wie ein Literal verwendet werden.                   
   //*****************************************************


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

   //*****************************************************
   // TO UPDATE EXTERNAL REFERENCE TABLE                  
   // ----------------------------------                  
   // PRC-Name = Name der (neuen) externen Referenz;      
   // dieser Name wird zunaechst an der Position          
   // NXTPRC in die Tabelle PRCTBL eingetragen.           
   // Dann Suche, ob es evtl. in der Tabelle schon        
   // vorhanden ist. Falls ja, CODE.H an der Position     
   // PCOUNTER verlinken mit dem entsprechenden           
   // Eintrag (beide Richtungen). Wenn I = NXTPRC,        
   // dann war es der neu eingefuegte hoechste Eintrag,   
   // dann Pruefung auf Einhaltung der Grenzen,           
   // ansonsten naechsten Eintrag vorbereiten.            
   // --------------------------------------------------- 
   // Nachtrag: die Positionen im Code, wo dieselben      
   // externen Namen benutzt werden, sind miteinander     
   // verkettet ueber den LNK-Pointer; damit koennen      
   // nach Festlegung der Adresse alle Offsets            
   // angeglichen werden.                                 
   //*****************************************************


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

   //******************************************************
   // TO 'DEFINE' LABELS AND/OR RESOLVE FORWARD REFERENCES 
   // ---------------------------------------------------- 
   //******************************************************


      var TPC , QPC : INTEGER ;

      begin (* UPD_LBLTBL *)
        if FALSE then
          begin
            WRITELN ( TRACEF , 'upd_lbltbl - pcounter  = ' , PCOUNTER )
                      ;
            WRITELN ( TRACEF , 'upd_lbltbl - intlbl    = ' , INTLBL ) ;
            WRITELN ( TRACEF , 'upd_lbltbl - newlbl    = ' , NEWLBL ) ;
            WRITELN ( TRACEF , 'upd_lbltbl - case_flow = ' , CASE_FLOW
                      ) ;
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
                  WRITELN ( TRACEF , 'upd_lbltbl - defined   = ' ,
                            DEFINED ) ;
                end (* then *) ;
              if DEFINED then

        //********************
        // BACKWARD REFERENCE 
        //********************

                if CASE_FLOW then
                  CODE . H [ PCOUNTER ] := TO_HINT ( LNK * 2 )

        //**************
        //HALFWORD ADDR.
        //**************

                else
                  CODE . H [ PCOUNTER ] := TO_HINT ( BASE_DSPLMT ( LNK
                                           ) )

        //*************************
        // BASE/DSPLMT HALF WORD   
        //*************************

              else
                if NEWLBL then

        //******************
        // LABEL DEFINITION 
        //******************

                  begin
                    DEFINED := TRUE ;
                    TPC := LNK ;
                    LNK := PCOUNTER ;
                    if FALSE then
                      begin
                        WRITELN ( TRACEF , 'upd_lbltbl - newlbl    = '
                                  , NEWLBL ) ;
                        WRITELN ( TRACEF , 'upd_lbltbl - lnk       = '
                                  , LNK ) ;
                      end (* then *) ;

        //*****************
        // SET LABEL VALUE 
        //*****************

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

        //*********************************************************
        // NOT NEWLBL I.E. FORWARD REFERENCE, TO BE RESOLVED LATER 
        //*********************************************************

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

      var I : INTEGER ;
          DELIM : CHAR ;
          C , C1 , C2 : CHAR ;
          COL : INTEGER ;

      begin (* PRINT_SET *)
        PSVAL := S ;
        DELIM := '''' ;
        WRITE ( LIST002 , '=XL' , LNGTH : 1 , '''' ) ;
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
                WRITE ( LIST002 , 'X' ) ;
                LIST002_NEWLINE ;
                WRITE ( LIST002 , ' ' : 21 ) ;
                COL := 1 ;
              end (* then *)
            else
              COL := COL + 1 ;
            WRITE ( LIST002 , C1 ) ;
            if FALSE then
              WRITE ( TRACEF , C1 ) ;
            COL := COL + 1 ;
            WRITE ( LIST002 , C2 ) ;
            if FALSE then
              WRITE ( TRACEF , C2 ) ;
          end (* for *) ;
        WRITE ( LIST002 , '''' ) ;
        LIST002_NEWLINE ;
        if FALSE then
          WRITELN ( TRACEF , '''' ) ;
      end (* PRINT_SET *) ;


   procedure TRACE_SET ( S : LARGE_SET ; LNGTH : BYTE ) ;

      var I : INTEGER ;
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

          //**************************************************
          // 370 FORMAT CODE GENERATOR (ASSEMBLY/OBJECT CODE) 
          // ------------------------------------------------ 
          //**************************************************



   procedure GENRXDLIT ( OP : BYTE ; R : RGRNG ; VAL : REAL ) ;

      begin (* GENRXDLIT *)
        if OP = XLD then
          if VAL = 0.0 then
            begin
              GENRR ( XSDR , R , R ) ;
              return
            end (* then *) ;

        //*********************************
        // write symbolic instr to list002 
        //*********************************

        if ASM then
          begin
            HEXHW ( PCOUNTER * 2 , HEXPC ) ;
            WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
            WRITE ( LIST002 , XTBLN [ OP ] : COLASMI , ' ' : SPACEASMI
                    , R : 1 , ',=D''' , VAL : 20 , '''' ) ;
            LIST002_NEWLINE ;
          end (* then *) ;

        //********************************
        // insert instr into code buffer  
        //********************************

        CODE . H [ PCOUNTER ] := TO_HINT ( OP * SL8 + R * 16 + 00 ) ;
        UPD_DBLTBL ( PCOUNTER + 1 , VAL ) ;

        //********************************
        // increment instr counter        
        //********************************

        PCOUNTER := NEXTPC ( 2 ) ;
        with LAST_CC do
          begin
            LAST_PC := PCOUNTER ;
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

        //*********************************
        // write symbolic instr to list002 
        //*********************************

        if ASM then
          begin
            HEXHW ( PCOUNTER * 2 , HEXPC ) ;
            WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
            if ( OP <= XSLDA ) and ( OP >= XSRL ) then
              WRITE ( LIST002 , XTBLN [ OP ] : COLASMI , ' ' :
                      SPACEASMI , R1 : 1 , ',' , D : 1 )
            else
              WRITE ( LIST002 , XTBLN [ OP ] : COLASMI , ' ' :
                      SPACEASMI , R1 : 1 , ',' , R2 : 1 , ',' , D : 1 )
                      ;
            if B <> 0 then
              WRITE ( LIST002 , '(' , B : 1 , ')' ) ;
            LIST002_NEWLINE ;
          end (* then *) ;

        //********************************
        // insert instr into code buffer  
        //********************************

        CODE . H [ PCOUNTER ] := TO_HINT ( OP * SL8 + R1 * 16 + R2 ) ;
        CODE . H [ PCOUNTER + 1 ] := TO_HINT ( B * SL12 + D ) ;

        //********************************
        // increment instr counter        
        //********************************

        PCOUNTER := NEXTPC ( 2 ) ;
      end (* GENRS *) ;


   procedure GENRSLIT ( OP : BYTE ; R1 , R2 : RGRNG ; S : SHORT_SET ) ;

      var LS : LARGE_SET ;

      begin (* GENRSLIT *)
        I_S_R . S := S ;
        if R1 = TRG14 then
          TXR_CONTENTS . VALID := FALSE ;

        //*********************************
        // write symbolic instr to list002 
        //*********************************

        if ASM then
          begin
            HEXHW ( PCOUNTER * 2 , HEXPC ) ;
            WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' , XTBLN [ OP
                    ] : COLASMI , ' ' : SPACEASMI , R1 : 1 , ',' , R2 :
                    1 , ',' ) ;

        //***********************************************
        // it is sufficient to assign the first part of  
        // ls, because print_set will only use this part 
        //***********************************************

            LS . S [ 1 ] := S ;
            PRINT_SET ( LS , 8 ) ;
          end (* then *) ;

        //********************************
        // insert instr into code buffer  
        //********************************

        CODE . H [ PCOUNTER ] := TO_HINT ( OP * SL8 + R1 * 16 + R2 ) ;

        //********************************
        // increment instr counter        
        //********************************

        PCOUNTER := NEXTPC ( 2 ) ;
        UPD_DBLTBL ( PCOUNTER - 1 , I_S_R . R ) ;
      end (* GENRSLIT *) ;


   procedure GENSS ( OP : BYTE ; LNGTH : BYTE_PLUS_ONE ; D1 : ADRRNG ;
                   B1 : RGRNG ; D2 : ADRRNG ; B2 : RGRNG ) ;

      begin (* GENSS *)

        //*********************************
        // write symbolic instr to list002 
        //*********************************

        if ASM then
          begin
            HEXHW ( PCOUNTER * 2 , HEXPC ) ;
            WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
            WRITE ( LIST002 , XTBLN [ OP ] : COLASMI , ' ' : SPACEASMI
                    , D1 : 1 , '(' , LNGTH : 1 , ',' , B1 : 1 , '),' ,
                    D2 : 1 , '(' , B2 : 1 , ')' ) ;
            LIST002_NEWLINE ;
          end (* then *) ;

        //********************************
        // insert instr into code buffer  
        //********************************

        CODE . H [ PCOUNTER ] := TO_HINT ( OP * SL8 + ( LNGTH - 1 ) ) ;
        CODE . H [ PCOUNTER + 1 ] := TO_HINT ( B1 * SL12 + D1 ) ;
        CODE . H [ PCOUNTER + 2 ] := TO_HINT ( B2 * SL12 + D2 ) ;

        //********************************
        // increment instr counter        
        //********************************

        PCOUNTER := NEXTPC ( 3 ) ;
      end (* GENSS *) ;


   procedure GENSI ( OP : BYTE ; D : ADRRNG ; B : RGRNG ; I : BYTE ) ;

      begin (* GENSI *)

        //*********************************
        // write symbolic instr to list002 
        //*********************************

        if ASM then
          begin
            HEXHW ( PCOUNTER * 2 , HEXPC ) ;
            WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
            WRITE ( LIST002 , XTBLN [ OP ] : COLASMI , ' ' : SPACEASMI
                    , D : 1 , '(' , B : 1 , '),' , I : 1 ) ;
            LIST002_NEWLINE ;
          end (* then *) ;

        //********************************
        // insert instr into code buffer  
        //********************************

        CODE . H [ PCOUNTER ] := TO_HINT ( OP * SL8 + I ) ;
        CODE . H [ PCOUNTER + 1 ] := TO_HINT ( B * SL12 + D ) ;

        //********************************
        // increment instr counter        
        //********************************

        PCOUNTER := NEXTPC ( 2 ) ;
      end (* GENSI *) ;


   procedure GENSSLIT ( OP , LNGTH : BYTE ; D1 : ADRRNG ; B1 : RGRNG ;
                      S : LARGE_SET ) ;

      begin (* GENSSLIT *)
        if LNGTH = 1 then

        //*******************************
        // SUBSTITUTE AN IMMEDIATE INST. 
        //*******************************

          begin
            I_S_R . S := S . S [ 1 ] ;
            GENSI ( OP - XMVC + XMVI , D1 , B1 , ORD ( I_S_R . C1 ) ) ;
          end (* then *)
        else
          if LNGTH > 1 then
            begin

        //*********************************
        // write symbolic instr to list002 
        //*********************************

              if ASM then
                begin
                  HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                  WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
                  WRITE ( LIST002 , XTBLN [ OP ] : COLASMI , ' ' :
                          SPACEASMI , D1 : 1 , '(' , LNGTH : 1 , ',' ,
                          B1 : 1 , '),' ) ;
                  PRINT_SET ( S , LNGTH ) ;
                end (* then *) ;

        //********************************
        // insert instr into code buffer  
        //********************************

              CODE . H [ PCOUNTER ] := TO_HINT ( OP * SL8 + ( LNGTH - 1
                                       ) ) ;
              CODE . H [ PCOUNTER + 1 ] := TO_HINT ( B1 * SL12 + D1 ) ;
              UPD_SETTBL ( PCOUNTER + 2 , S , LNGTH ) ;

        //********************************
        // increment instr counter        
        //********************************

              PCOUNTER := NEXTPC ( 3 ) ;
            end (* then *) ;
      end (* GENSSLIT *) ;


   procedure GENAL2 ( PC : ICRNG ; LAB : PLABEL ) ;

      var INTLBL : INTEGER ;

      begin (* GENAL2 *)
        if ASM then
          begin
            HEXHW ( PC * 2 , HEXPC ) ;
            WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
            WRITE ( LIST002 , ' DC AL2(' , LAB . NAM : LAB . LEN , '-'
                    , PRCTBL [ 0 ] . NAME , ')' ) ;
            LIST002_NEWLINE ;
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

        //*********************************
        // write symbolic instr to list002 
        //*********************************

        if ASM then
          begin
            HEXHW ( PCOUNTER * 2 , HEXPC ) ;
            WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
            if CASE_FLAG then
              begin
                WRITE ( LIST002 , ' DC AL2(' , LAB . NAM : LAB . LEN ,
                        '-' , PRCTBL [ 0 ] . NAME , ')' ) ;
                LIST002_NEWLINE ;
              end (* then *)
            else
              begin
                WRITE ( LIST002 , XTBLN [ OP ] : COLASMI , ' ' :
                        SPACEASMI , R : 1 , ',' ) ;
                if TAG = 0 then
                  WRITE ( LIST002 , LAB . NAM : LAB . LEN )
                else
                  if TAG > 0 then
                    WRITE ( LIST002 , LAB . NAM : LAB . LEN , '(' , TAG
                            : 1 , ')' )
                  else
                    begin
                      if TAG = - 3 then
                        WRITE ( LIST002 , '=V(' )
                      else

        //**********
        // TAG = -1 
        //**********

                        WRITE ( LIST002 , '=A(' ) ;
                      WRITE ( LIST002 , LAB . NAM : LAB . LEN ) ;
                      WRITE ( LIST002 , ')' ) ;
                    end (* else *) ;
                LIST002_NEWLINE ;
              end (* else *) ;
          end (* then *) ;

        //********************************
        // insert instr into code buffer  
        //********************************

        if CASE_FLAG then
          begin

        //*******
        //LAB REF
        //*******

            UPD_LBLTBL ( PCOUNTER , LBLMAP ( LAB . NAM ) , FALSE , TRUE
                         ) ;
            PCOUNTER := NEXTPC ( 1 ) ;
          end (* then *)
        else
          begin
            if TAG >= - 1 then

        //***************
        //GENERATED LABEL
        //***************

              UPD_LBLTBL ( PCOUNTER + 1 , LBLMAP ( LAB . NAM ) , FALSE
                           , FALSE )

        //***************
        //LABEL REFERENCE
        //***************

            else

        //*********
        //PROC. ID.
        //*********

              UPD_PRCTBL ( PCOUNTER + 1 , LAB . NAM ) ;
            if TAG < 0 then
              TAG := 0 ;
            CODE . H [ PCOUNTER ] := TO_HINT ( OP * SL8 + R * 16 + TAG
                                     ) ;
            PCOUNTER := NEXTPC ( 2 )
          end (* else *)
      end (* GENRXLAB *) ;


   procedure GENRELRX ( OP : BYTE ; R : RGRNG ; OFFSET : HINTEGER ) ;

   //**************************************
   // OPERAND OF RX INST. IS "*+2*OFFSET"  
   //**************************************


      var SAVEASM : BOOLEAN ;

      begin (* GENRELRX *)

        //*********************************
        // write symbolic instr to list002 
        //*********************************

        if ASM then
          begin
            HEXHW ( PCOUNTER * 2 , HEXPC ) ;
            WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
            WRITE ( LIST002 , XTBLN [ OP ] : COLASMI , ' ' : SPACEASMI
                    , R : 1 , ',*+' , 2 * OFFSET : 1 ) ;
            LIST002_NEWLINE ;
          end (* then *) ;

        //********************************
        // insert instr into code buffer  
        //********************************

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


   procedure FINDRG ;

   //*********************
   //TO FIND A GP REGISTER
   //*********************


      var I : RGRNG ;

      begin (* FINDRG *)
        I := 1 ;
        repeat
          I := I + 1
        until ( AVAIL [ I ] or ( I = RGCNT ) ) ;
        if not AVAIL [ I ] then
          ERROR ( 750 ) ;
        AVAIL [ I ] := FALSE ;
        NXTRG := I ;
      end (* FINDRG *) ;


   procedure FINDRP ;

   //**************************************************************
   // FIND REGISTER PAIR                                           
   //**************************************************************


      var I : RGRNG ;

      begin (* FINDRP *)
        I := RGCNT + 1 ;
        repeat
          I := I - 2
        until ( I < 4 ) or ( AVAIL [ I ] and AVAIL [ I + 1 ] ) ;
        if not ( AVAIL [ I ] and AVAIL [ I + 1 ] ) then
          begin

        //******************************************************
        // trial - opp - 02.06.2019:                            
        // if filadr (r9) occupied, free it and                 
        // signal that filreg has to be                         
        // loaded again later                                   
        // same for callstackadr (r8)                           
        //******************************************************

            if CSPACTIVE [ FILADR ] and CSPACTIVE [ CALLSTACKADR ] then
              begin
                AVAIL [ FILADR ] := TRUE ;
                CSPACTIVE [ FILADR ] := FALSE ;
                FILADR_LOADED := FALSE ;
                AVAIL [ CALLSTACKADR ] := TRUE ;
                CSPACTIVE [ CALLSTACKADR ] := FALSE ;
                I := CALLSTACKADR ;
              end (* then *)
            else
              begin
                ERROR ( 751 ) ;
              end (* else *)
          end (* then *) ;
        AVAIL [ I ] := FALSE ;
        AVAIL [ I + 1 ] := FALSE ;
        NXTRG := I
      end (* FINDRP *) ;


   procedure FINDFP ;

   //******************************
   //FIND A FLOATING POINT REGISTER
   //******************************


      var I : INTEGER ;

      begin (* FINDFP *)
        I := 0 ;
        repeat
          I := I + 2
        until AVAILFP [ I ] or ( I = FPCNT ) ;
        if not AVAILFP [ I ] then
          ERROR ( 752 ) ;
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

        //***************
        // DTYPE <> REEL 
        //***************

                AVAIL [ RGADR ] := TRUE ;
              if DRCT and ( DTYPE = PSET ) then
                if PLEN > 4 then

        //******************
        // REG. PAIR IN USE 
        //******************

                  AVAIL [ RGADR + 1 ] := TRUE
            end (* then *)
      end (* FREEREG *) ;


   procedure FREEREG_COND ( var STE : DATUM ; RGADR_IN_USE : RGRNG ) ;

      begin (* FREEREG_COND *)
        with STE do
          if VPA = RGS then
            if RGADR <> RGADR_IN_USE then
              AVAIL [ RGADR ] := TRUE ;
      end (* FREEREG_COND *) ;


   function ALIGN ( Q , P : INTEGER ) : INTEGER ;

      var I : INTEGER ;

      begin (* ALIGN *)
        ALIGN := Q ;
        I := Q MOD P ;
        if I <> 0 then
          ALIGN := Q + ( P - I ) ;
      end (* ALIGN *) ;


   function POWER2 ( I : INTEGER ) : INTEGER ;

   //******************************************************
   // IF I > 0 IS A POWER OF TWO, RETURN 'THAT' POWER,     
   // ELSE RETURN NEGATIVE                                 
   // ---------------------------------------------------- 
   //******************************************************


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

   //*****************************************************
   // TO TRANSLATE A 'LEVEL/OFFSET' P/Q ADDRESS           
   // TO 'BASE/INDEX/DISPLACEMENT'                        
   // --------------------------------------------------- 
   //*****************************************************


      const MAXDISP = 4088 ;
            SHRTINT2 = 8183 ;

            //*******************
            // SHRTINT + MAXDISP 
            //*******************


      var T , TQ : ADRRNG ;
          TP : LVLRNG ;

      begin (* BASE *)
        B := 0 ;
        if P < 0 then
          return ;

        //*****************
        // STRING CONSTANT 
        //*****************

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

   //*******************************************************
   //  TO ELIMINATE THE RESULT Q=4092                       
   //  THAT MAY BE GENERATED BY BASE                        
   //  AND WHICH CAUSES TROUBLE FOR OPERATIONS ON SETS      
   //*******************************************************


      begin (* CHECKDISP *)
        if Q > ( SHRTINT - 4 ) then
          begin
            GENRX ( XLA , TXRG , SHRTINT - 4 , B , P ) ;
            Q := Q - ( SHRTINT - 4 ) ;
            P := TXRG ;
            B := 0
          end (* then *)
      end (* CHECKDISP *) ;


   procedure GENLA_LR ( R1 : RGRNG ; Q2 : ADRRNG ; R2 , X2 : RGRNG ) ;

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

   //**************************************************************
   // LOADS AN STACK ELEMENT INTO A REGISTER, IF NOT ALREADY THERE 
   // ------------------------------------------------------------ 
   //**************************************************************


      var P : LVLRNG ;
          Q : ADRRNG_EXT ;
          B : RGRNG ;


      procedure FINDMDRG ;

      //**********************************
      //TO FIND A MULTIPLY/DIVIDE REGISTER
      //**********************************


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

        //************************************
        // LOAD THE VARIABLE POINTED TO BY STP
        //************************************

              if DRCT then

        //****************************
        //DIRECTLY ACCESSIBLE VARIABLE
        //****************************

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
                              GENLA_LR ( RGADR , Q , P , RGADR ) ;
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
                              GENLA_LR ( NXTRG , Q , B , P )
                            end (* then *)
                          else
                            if P < 0 then
                              begin
                                GENRX_2 ( XLA , NXTRG , 0 , 0 , 0 , 2 )
                                          ;
                                if P = - 1 then
                                  begin
                                    if TRACE_LITERAL then
                                      WRITELN ( TRACEF ,
                                         'repl lit. adr. 1 - index = '
                                                , SCNSTNO : 1 ,
                                                ' pc = ' , PCOUNTER - 1
                                                : 1 ) ;
                                    LITTBL [ SCNSTNO ] . LNK :=
                                                   PCOUNTER - 1 ;
                                  end (* then *) ;
                                CODE . H [ PCOUNTER - 1 ] := TO_HINT (
                                                   Q ) ;
                              end (* then *)
                            else
                              GENRXLIT ( XL , NXTRG , FPA . DSPLMT , 0
                                         ) ;

        //*********
        //NIL VALUE
        //*********

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

   //*************************************************
   // IF PASSED THE ADR. OF AN ITEM,                  
   // THIS ROUTINE RETURNS A <Q,B,P> ADR.             
   // INDIRECTIONS ARE NOT DEREFERENCED HERE.         
   // ----------------------------------------------- 
   //*************************************************


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

        //*************************
        //VPA = MEM OR VPA = ONSTK 
        //*************************

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

        //**********************************************************
        // NOW THE VARIABLE PORTION OF THE ADR., IF ANY, IS IN TXRG 
        //**********************************************************

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

        //*****************
        // NO INDEX OR VPA 
        //*****************

              BASE ( Q , P , B ) ;
          end (* with *)
      end (* GETADR *) ;


   function CHECK_ZERO_REG ( var STE : DATUM ; Q : ADRRNG ; P , B :
                           RGRNG ) : BOOLEAN ;

      var R : RGRNG ;

      begin (* CHECK_ZERO_REG *)
        CHECK_ZERO_REG := FALSE ;
        return ;

        //******************************************************
        // ist wahrscheinlich falsch ...                        
        //******************************************************

        if ( P <> 0 ) and ( B <> 0 ) then
          begin
            CHECK_ZERO_REG := FALSE ;
            return
          end (* then *) ;
        if P <> 0 then
          R := B
        else
          R := P ;
        STE . VPA := MEM ;
        STE . MEMADR . LVL := R ;
        STE . MEMADR . DSPLMT := Q ;
        STE . RGADR := 0 ;
        STE . VRBL := TRUE ;
        CHECK_ZERO_REG := TRUE ;
      end (* CHECK_ZERO_REG *) ;


   procedure GETADR2 ( STE : DATUM ; var Q : ADRRNG ; var P , B : RGRNG
                     ) ;

   //*************************************************
   // IF PASSED THE ADR. OF AN ITEM,                  
   // THIS ROUTINE RETURNS A <Q,B,P> ADR.             
   // INDIRECTIONS ARE NOT DEREFERENCED HERE.         
   // ----------------------------------------------- 
   //*************************************************


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

        //**************************
        // VPA = MEM OR VPA = ONSTK 
        //**************************

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
                      WRITELN ( TRACEF , 'GETADR2 - linecnt = ' ,
                                LINECNT : 1 ) ;
                      WRITELN ( TRACEF , 'base/vrbl: q = ' , Q ) ;
                      WRITELN ( TRACEF , 'base/vrbl: p = ' , P ) ;
                      WRITELN ( TRACEF , 'base/vrbl: b = ' , B ) ;
                      WRITELN ( TRACEF , 'now calling GENRX for L' ) ;
                    end (* then *) ;
                  GENRX ( XL , TXRG , Q , B , P ) ;
                  R := TXRG
                end (* else *) ;

        //**********************************************************
        // NOW THE VARIABLE PORTION OF THE ADR., IF ANY, IS IN TXRG 
        //**********************************************************

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
                      begin
                        if FALSE then
                          WRITELN ( TRACEF , 'now calling GENRX for A'
                                    ) ;
                        GENRX ( XA , R , DISPLAY + 4 * P , GBR , 0 ) ;
                      end (* then *) ;
                P := R ;
              end (* then *)
            else

        //*****************
        // NO INDEX OR VPA 
        //*****************

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
        // if mem and indirect, load address before             
        // if neither, source was constant, use LA              
        //******************************************************

            case VPA of
              RGS : GENRR ( XLR , TXRG , RGADR ) ;
              MEM : begin
                      if not DRCT then
                        begin
                          P1 := MEMADR . LVL ;
                          Q1 := MEMADR . DSPLMT ;
                          BASE ( Q1 , P1 , B1 ) ;
                          GENRX ( XL , TXRG , Q1 , B1 , P1 ) ;
                          P1 := TXRG ;
                          Q1 := 0 ;
                          B1 := 0 ;
                        end (* then *)
                      else
                        begin
                          P1 := MEMADR . LVL ;
                          Q1 := MEMADR . DSPLMT ;
                          BASE ( Q1 , P1 , B1 )
                        end (* else *) ;
                      case DTYPE of
                        CHRC : begin
                                 if DRCT then
                                   begin
                                     if INIT_ON_CHAR then
                                       GENRR ( XXR , TXRG , TXRG ) ;
                                     GENRX ( XIC , TXRG , Q1 , B1 , P1
                                             )
                                   end (* then *)
                                 else
                                   begin
                                     GENRX ( XIC , TXRG , Q1 , B1 , P1
                                             ) ;
                                     if INIT_ON_CHAR then
                                       begin
                                         GENRS ( XSLL , TXRG , 0 , 24 ,
                                                 0 ) ;
                                         GENRS ( XSRL , TXRG , 0 , 24 ,
                                                 0 ) ;
                                       end (* then *)
                                   end (* else *)
                               end (* tag/ca *) ;
                        HINT : GENRX ( XLH , TXRG , Q1 , B1 , P1 ) ;
                        otherwise
                          GENRX ( XL , TXRG , Q1 , B1 , P1 ) ;
                      end (* case *) ;
                    end (* tag/ca *) ;
              NEITHER :
                if FPA . DSPLMT <> 0 then
                  GENLA_LR ( TXRG , FPA . DSPLMT , 0 , 0 )
                else
                  GENRR ( XXR , TXRG , TXRG ) ;
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
              RGS : begin
                      if not DRCT then
                        begin
                          GENRX ( XIC , RGADR , 0 , RGADR , 0 ) ;
                          DRCT := TRUE
                        end (* then *) ;
                      GENRX ( XSTC , RGADR , Q1 , 0 , B1 ) ;
                    end (* tag/ca *) ;
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

   //*************************************************************
   // IF PASSED AN ITEM, THIS ROUTINE RETURNS                     
   // ITS <Q,B,P> ADDRESS                                         
   // WARNING ON USAGE OF THIS PROCEDURE!!!                       
   //                                                             
   // IT IS UNSAFE TO CALL FINDRG (AND THEREFORE ALSO             
   //   FINDRP, LOAD, ...) AFTER GETOPERAND AND BEFORE            
   //   THE P1 REGISTER HAS BEEN  USED                            
   //*************************************************************


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

        //***********************************
        // THE VPA=REG CASE NOT HANDLED HERE 
        //***********************************

                end (* else *)
            else

        //*********
        //NOT DIRCT
        //*********

              begin
                GETADR ( STE , Q1 , P1 , B1 ) ;
                if VPA = RGS then
                  AVAIL [ RGADR ] := TRUE ;
              end (* else *)
          else

        //***********************************
        // VRBL MAY NOT HAVE ANY FUNCTION    
        // ANY MORE                          
        //***********************************

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

   //***************************************************
   // GETS BASE-DISPLACEMENT ADDRESS SUCH THAT THE      
   // DISPLACEMENT < 4096-L                             
   //***************************************************


      var B : RGRNG ;

      begin (* GETQB *)
        if L < 0 then
          L := 0 ;
        GETOPERAND ( STE , Q , P , B ) ;
        if B > 0 then
          if P > 0 then
            if Q >= ( 4096 - L ) then
              begin
                GENLA_LR ( TXRG , Q , P , B ) ;
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
            GENLA_LR ( TXRG , Q , P , 0 ) ;
            P := TXRG ;
            Q := 0 ;
          end (* then *) ;
      end (* GETQB *) ;


   procedure STORE ( STP : STKPTR ; INDRCT : BOOLEAN ) ;

   //******************************************************
   // STORE THE STACK ELEMENT IN THE LOCATION DENOTED BY : 
   // IF INDRCT  THEN  2_ND TOP STACK ELEMENT              
   // ELSE P_Q FIELDS OF THE CURRENT INSTRUCTION           
   // ---------------------------------------------------- 
   //******************************************************


      var B : RGRNG ;
          P1 : RGRNG ;

      begin (* STORE *)

        //***********************************
        // LOADS THE ELEMENT INTO A REGISTER 
        //***********************************

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

        //********************
        // STORING A CONSTANT 
        //********************

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

        //************************
        // SAVEFPRS FOR THIS CALL 
        //************************

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
        with CALL_MST_STACK [ CALL_DEPTH ] do
          if DISPSAV > 0 then

        //******************************
        // CALL ON PARAMETRIC PROCEDURE 
        //******************************

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

        //******************
        // LOAD PROC. ADDR. 
        //******************

            end (* then *)
          else
            PPCALL := FALSE ;
        if Q <= 4095 then
          GENLA_LR ( TRG1 , Q , LBR , 0 )
        else
          begin
            GENRR ( XLR , TRG1 , LBR ) ;
            GENRXLIT ( XA , TRG1 , Q , 0 ) ;
          end (* else *) ;

        //******************************************************
        // generate different call sequences depending on       
        // external language; for fortran all parameters        
        // are call by reference, so dummyint arguments are     
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
                  GENLA_LR ( TRG13 , K , TRG1 , 0 ) ;
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
                  GENLA_LR ( TRG1 , 112 , TRG1 , 0 ) ;
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

        //****************************
        // GENERATE SPECIAL CALL CODE 
        //****************************

          begin
            if PPCALL then
              begin
                if ODD ( PCOUNTER ) then
                  GENRR ( XBCR , NOCND , 0 ) ;

        //***************
        // ALIGN TO WORD 
        //***************

                GENRELRX ( XST , TRG15 , 4 ) ;

        //************
        // ST 15,*+8  
        //************

                GENRELRX ( XBC , ANYCND , 4 ) ;

        //************
        // B  *+8     
        //************

                CODE . I [ PCOUNTER DIV 2 ] := 0 ;
                PCOUNTER := NEXTPC ( 2 ) ;
                GENRX ( XBAL , TRG14 , TRACER , GBR , 0 ) ;
                CODE . H [ PCOUNTER ] := TO_HINT ( 2 * PCOUNTER - 8 ) ;

        //****************
        // DC AL2( *-8 )  
        //****************

              end (* then *)
            else
              begin
                GENRX ( XBAL , TRG14 , TRACER , GBR , 0 ) ;
                UPD_PRCTBL ( PCOUNTER , LBL2 . NAM ) ;
              end (* else *) ;
            PCOUNTER := NEXTPC ( 1 ) ;
          end (* else *) ;
        if PPCALL then

        //*****************
        // RESTORE DISPLAY 
        //*****************

          begin
            GENSS ( XMVC , DISPAREA , DISPLAY , GBR , CALL_MST_STACK [
                    CALL_DEPTH ] . DISPSAV , DSREG ) ;
            AVAIL [ DSREG ] := TRUE ;
          end (* then *) ;
        CALL_DEPTH := CALL_DEPTH - 1 ;
        if FALSE then
          WRITELN ( TRACEF , 'call_depth -- ' , CALL_DEPTH ,
                    ' linecnt = ' , LINECNT : 1 ) ;

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
        CSPACTIVE [ TRG15 ] := FALSE ;
        CSPACTIVE [ TRG1 ] := FALSE ;

        //*************
        // R1,R15 USED 
        //*************

      end (* CALLSUB *) ;


   procedure SAVE_FILEOPERAND ( STP : STKPTR ) ;

   //***************************************
   // this procedure saves the fileoperand  
   // in the structure LAST_FILE to avoid   
   // register loads in some cases          
   // (repeated I/O to the same file)       
   //***************************************


      var Q1 : ADRRNG ;
          P1 : RGRNG ;

      begin (* SAVE_FILEOPERAND *)
        with STK [ STP ] do
          begin
            if VRBL then
              begin
                Q1 := MEMADR . DSPLMT ;
                P1 := MEMADR . LVL
              end (* then *)
            else
              begin
                Q1 := FPA . DSPLMT ;
                P1 := FPA . LVL
              end (* else *) ;
            with LAST_FILE do
              begin
                LAST_FILEOPERAND . DSPLMT := Q1 ;
                LAST_FILEOPERAND . LVL := P1 ;
                LAST_FILE_IS_VAR := VRBL ;
              end (* with *) ;
          end (* with *) ;
      end (* SAVE_FILEOPERAND *) ;


   procedure LOADFCBADDRESS ( STP : STKPTR ) ;

   //*****************************
   // load FCB address if needed  
   //*****************************


      var OPC : BYTE ;

      begin (* LOADFCBADDRESS *)

        //****************************************
        // if the file register (register 9)      
        // has not yet been loaded, then load it  
        //****************************************

        if not FILADR_LOADED then
          begin

        //**********************************
        // first save the file operand      
        // then get displacement and level  
        // from the saved structure         
        //**********************************

            SAVE_FILEOPERAND ( STP ) ;
            if CSP in [ PRES , PREW , PGET , PPUT , PRLN , PWLN , PPAG
            , PSKP , PLIM , PRDB , PWRB , PRDH , PRDY , PEOL , PEOT ,
            PEOF , PELN , PRDC , PWRC , PRDI , PWRI , PRDS , PRDV ,
            PRFC , PRFS , PRFV , PWRS , PWRV , PRDR , PWRR , PWRP ,
            PWRX , PFDF , PWRD , PWRE , PCLS ] then
              with STK [ STP ] do
                begin
                  if VRBL then
                    OPC := XL
                  else
                    OPC := XLA ;

        //*****************************************
        // get displacement and level              
        // from the saved structure (saved above)  
        //*****************************************

                  Q1 := LAST_FILE . LAST_FILEOPERAND . DSPLMT ;
                  P1 := LAST_FILE . LAST_FILEOPERAND . LVL ;
                  BASE ( Q1 , P1 , B1 ) ;
                  GENRX ( OPC , FILADR , Q1 , B1 , P1 ) ;
                  FILADR_LOADED := TRUE ;
                  AVAIL [ FILADR ] := FALSE ;
                  CSPACTIVE [ FILADR ] := TRUE ;
                end (* with *)
          end (* then *)
      end (* LOADFCBADDRESS *) ;


   procedure GOTOCSP ;

      var LBL_WORK : PLABEL ;

      begin (* GOTOCSP *)

        //********************************
        // (RE)LOAD PROCADR, if necessary 
        //********************************

        if not CSPACTIVE [ TRG15 ] then
          begin
            LBL_WORK . NAM := '$PASCSP' ;
            LBL_WORK . LEN := 7 ;
            GENRXLAB ( XL , TRG15 , LBL_WORK , - 3 ) ;
          end (* then *) ;
        CSPACTIVE [ TRG15 ] := TRUE ;

        //***********************************************
        // load new stackaddr, if necessary (if changed) 
        //***********************************************

        if PROCOFFSET <> 0 then
          begin
            if ( PROCOFFSET_OLD <> PROCOFFSET ) or not CSPACTIVE [
            CALLSTACKADR ] then
              begin
                if PROCOFFSET <= 4095 then
                  begin
                    GENLA_LR ( CALLSTACKADR , PROCOFFSET , 13 , 0 ) ;
                    AVAIL [ CALLSTACKADR ] := FALSE ;
                    CSPACTIVE [ CALLSTACKADR ] := TRUE ;
                  end (* then *)
                else
                  begin
                    GENRR ( XLR , CALLSTACKADR , 13 ) ;
                    GENRXLIT ( XA , CALLSTACKADR , PROCOFFSET , 0 ) ;
                    AVAIL [ CALLSTACKADR ] := FALSE ;
                    CSPACTIVE [ CALLSTACKADR ] := TRUE ;
                  end (* else *)
              end (* then *) ;
            PROCOFFSET_OLD := PROCOFFSET ;
          end (* then *) ;

        //**********************
        // proc number in reg 1 
        //**********************

        if not CSPACTIVE [ TRG1 ] then
          OLDCSP := PSIO ;
        if CSP <> OLDCSP then
          begin
            GENLA_LR ( TRG1 , ORD ( CSP ) * 4 , 0 , 0 ) ;
            OLDCSP := CSP ;
            CSPACTIVE [ TRG1 ] := TRUE ;
          end (* then *) ;

        //************************************
        // see if filaddress has to be loaded 
        //************************************

        LOADFCBADDRESS ( TOP - 1 ) ;

        //**********************
        // branch to subroutine 
        //**********************

        GENRR ( XBALR , TRG14 , TRG15 ) ;

        //********************************
        // save some values for next call 
        //********************************

        LAST_FILE . LAST_PC := PCOUNTER ;
      end (* GOTOCSP *) ;


   procedure CALLSTNDRD ;

   //******************************
   // TO CALL A STANDARD PROCEDURE 
   // ---------------------------- 
   //******************************


      var Q1 , LEN : ADRRNG ;
          P1 , B1 : RGRNG ;
          OPC : BYTE ;
          ITEST : INTEGER ;
          LBL_WORK : PLABEL ;
          FILEOK : BOOLEAN ;


      procedure FILESETUP ( PRMCNT : RGRNG ) ;

      //*************************************************
      // TO SET UP PARAMETERS FOR THE FILE I/O           
      // AND CALL THE I/O ROUTINE                        
      // ------------------------------------------------
      // registers must be freed so that the parameters  
      // are passed in the registers 2, 3 and 4          
      //*************************************************
      // register 4 is only needed by a small number     
      // of runtime functions like WRR, WRX etc.         
      //*************************************************


         label 10 ;

         var I : RGRNG ;
             STP : STKPTR ;
             STP1 : STKPTR ;
             STP2 : STKPTR ;
             STP3 : STKPTR ;
             CPARM3 : INTEGER ;
             CPARM3_SET : BOOLEAN ;
             TOPSTART : STKPTR ;

         begin (* FILESETUP *)
           TOPSTART := TOP ;
           if FALSE then
             begin
               WRITELN ( TRACEF , 'filesetup - csp: ' , CSPTBL [ CSP ]
                         ) ;
               DUMPSTK ( 1 , TOPSTART ) ;
             end (* then *) ;

           //******************************************
           // POINTING TO NEXT AVAILABLE STACK ELEMENT 
           //******************************************

           STP := TOP - PRMCNT + 1 ;
           STP1 := STP ;
           STP2 := STP + 1 ;
           STP3 := STP + 2 ;
           TOP := STP ;

           //*****************************
           // POTENTIAL REGISTER CONFLICT 
           //*****************************

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

           //*****************************
           // POTENTIAL REGISTER CONFLICT 
           //*****************************

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

           //************************************************
           // stack abarbeiten                               
           //************************************************

           CPARM3_SET := FALSE ;
           for I := 2 to PRMCNT + 1 do
             with STK [ STP ] do
               begin

           //************************************************
           // trace stack content                            
           //************************************************

                 if FALSE then
                   begin
                     WRITELN ( TRACEF , 'dumpstk vor load bei csp ' ,
                               'bei zeile ' , LINECNT ) ;
                     DUMPSTK ( STP , STP ) ;
                   end (* then *) ;

           //************************************************
           // trace stack content                            
           //************************************************

                 if not VRBL then
                   case I of
                     3 : begin
                           CPARM3 := FPA . DSPLMT ;
                           CPARM3_SET := TRUE ;
                         end (* tag/ca *) ;
                     4 : begin
                           if CPARM3_SET then
                             if CPARM3 = FPA . DSPLMT then
                               begin
                                 RGADR := 3 ;
                                 goto 10
                               end (* then *) ;
                         end (* tag/ca *) ;
                     otherwise
                       begin
                         
                       end (* otherw *)
                   end (* case *) ;
                 LOAD ( STK [ STP ] ) ;
                 10 :
                 if DTYPE <> REEL then
                   begin

           //*****************
           // THE COMMON CASE 
           //*****************

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
                           ERROR ( 753 )
                         end (* else *)
                   end (* then *)
                 else

           //************************
           // DTYPE = REEL, I.E. WRR 
           //************************

                   begin
                     if RGADR <> I then
                       if AVAILFP [ I ] then
                         GENRR ( XLDR , I , RGADR )
                       else
                         begin
                           ERROR ( 754 )
                         end (* else *) ;
                     AVAILFP [ RGADR ] := TRUE ;
                     AVAIL [ I ] := FALSE ;
                     RGADR := I ;

           //***************************************
           // KLUDGE TO RELEASE THE FIX. REG. LATER 
           //***************************************

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

        //*********************************
        // r1 beim naechsten mal neu laden 
        //*********************************

                   CSPACTIVE [ TRG1 ] := FALSE ;
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

        //*********************************
        // r1 beim naechsten mal neu laden 
        //*********************************

                   CSPACTIVE [ TRG1 ] := FALSE ;
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

        //*********************************
        // r1 beim naechsten mal neu laden 
        //*********************************

                   CSPACTIVE [ TRG1 ] := FALSE ;
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
                     CSPACTIVE [ TRG1 ] := FALSE ;
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
                           ERROR ( 755 ) ;

        //************************************
        // ASSUMING THE CURRENT SIMPLE FORMAT 
        //************************************

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
                           ERROR ( 756 ) ;
                     end (* with *) ;
                   GOTOCSP ;
                   CSPACTIVE [ TRG1 ] := FALSE ;
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
                           GENLA_LR ( 1 , Q1 , B1 , P1 ) ;
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

        //*******************
        // SPECIAL CALL CODE 
        //*******************

                     begin
                       GENRX ( XBAL , TRG14 , TRACER , GBR , 0 ) ;
                       if ASM then
                         begin
                           WRITE ( LIST002 , ' ' , '## ' , ' ' :
                                   SPACEASMX , 'DC    AL2(' , PRCTBL [
                                   0 ] . NAME , '-=V($PASTRAP))' ) ;
                           LIST002_NEWLINE ;
                         end (* then *) ;
                       UPD_PRCTBL ( PCOUNTER , LBL_WORK . NAM ) ;
                       PCOUNTER := NEXTPC ( 1 ) ;
                     end (* else *) ;
                 end (* tag/ca *) ;

        //***************************************
        // start i/O and end i/O do nothing      
        // but they are there to make sure that  
        // the file registers are set properly   
        // prior to the actual I/O               
        //***************************************

          PSIO : begin
                   if not AVAIL [ FILADR ] then
                     if FILECNT = 0 then
                       ERROR ( 757 ) ;
                   AVAIL [ FILADR ] := FALSE ;
                   FILECNT := FILECNT + 1 ;
                   if FALSE then
                     begin
                       WRITELN ( TRACEF ,
                             '---------------------------------------'
                                 ) ;
                       WRITELN ( TRACEF , 'SIO at linecnt    = ' ,
                                 LINECNT ) ;
                       WRITELN ( TRACEF , 'last_file.last_pc = ' ,
                                 LAST_FILE . LAST_PC ) ;
                       WRITELN ( TRACEF , 'pcounter          = ' ,
                                 PCOUNTER ) ;
                       WRITELN ( TRACEF , 'last_fileoperand  = ' ,
                                 LAST_FILE . LAST_FILEOPERAND . LVL : 1
                                 , '/' , LAST_FILE . LAST_FILEOPERAND .
                                 DSPLMT : 1 ) ;
                     end (* then *) ;

        //***********************************************
        // check if the file register from the last I/O  
        // is still valid                                
        //***********************************************

                   with LAST_FILE do
                     begin
                       if LAST_PC = PCOUNTER then
                         begin
                           with STK [ TOP ] do
                             if VRBL then
                               FILEOK := LAST_FILE_IS_VAR and (
                                         LAST_FILEOPERAND = MEMADR )
                             else
                               FILEOK := ( not LAST_FILE_IS_VAR ) and (
                                         LAST_FILEOPERAND = FPA )
                         end (* then *)
                       else
                         FILEOK := FALSE ;
                       CSPACTIVE [ FILADR ] := FILEOK
                     end (* with *) ;

        //***********************************************
        // if file register does not match,              
        // FILADR_LOADED is false, of course             
        // otherwise FILADR_LOADED retains its old value 
        // that is, it may already contain the correct   
        // old value                                     
        //***********************************************

                   if not CSPACTIVE [ FILADR ] then
                     FILADR_LOADED := FALSE ;
                   if FALSE then
                     begin
                       WRITELN ( TRACEF , 'cspactive [9]     = ' ,
                                 CSPACTIVE [ FILADR ] ) ;
                       WRITELN ( TRACEF , 'fileadr_loaded    = ' ,
                                 FILADR_LOADED ) ;
                     end (* then *) ;
                   CSPACTIVE [ TRG1 ] := FALSE ;
                   TOP := TOP + 1 ;

        //*******************************************
        // TO CANCEL OUT PREVIOUS SUBTRACT OPERATION 
        //*******************************************

                 end (* tag/ca *) ;
          PEIO : begin

        //***************************
        // RELEASE FILE ADR REG ETC. 
        //***************************

                   FILECNT := FILECNT - 1 ;
                   if FILECNT = 0 then
                     AVAIL [ FILADR ] := TRUE ;
                   if FALSE then
                     begin
                       WRITELN ( TRACEF ,
                             '---------------------------------------'
                                 ) ;
                       WRITELN ( TRACEF , 'EIO at linecnt    = ' ,
                                 LINECNT ) ;
                       WRITELN ( TRACEF , 'last_file.last_pc = ' ,
                                 LAST_FILE . LAST_PC ) ;
                       WRITELN ( TRACEF , 'pcounter          = ' ,
                                 PCOUNTER ) ;
                       WRITELN ( TRACEF , 'last_fileoperand  = ' ,
                                 LAST_FILE . LAST_FILEOPERAND . LVL : 1
                                 , '/' , LAST_FILE . LAST_FILEOPERAND .
                                 DSPLMT : 1 ) ;
                     end (* then *) ;

        //*****************************************
        // the file register is invalid after EIO  
        //*****************************************

                   CSPACTIVE [ FILADR ] := FALSE ;
                   FILADR_LOADED := FALSE ;
                   CSPACTIVE [ TRG1 ] := FALSE ;
                   LAST_FILE . LAST_PC := PCOUNTER ;

        //**********************************************
        // TOP := TOP-1 IS DONE AT ENTRY TO CALLSTNDRD  
        //**********************************************

                   if FALSE then
                     begin
                       WRITELN ( TRACEF , 'cspactive [9]     = ' ,
                                 CSPACTIVE [ FILADR ] ) ;
                       WRITELN ( TRACEF , 'fileadr_loaded    = ' ,
                                 FILADR_LOADED ) ;
                     end (* then *) ;
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

        //******************************
        //TO BE CORRECTED BY PENDING EIO
        //******************************

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
                       GENLA_LR ( NXTRG , Q1 , B1 , P1 ) ;
                       GENRXLIT ( XL , NXTRG + 1 , LEN , 0 ) ;
                       P1 := NXTRG ;
                       FINDRP ;
                       GENLA_LR ( NXTRG , FILHDRSZ , FILADR , 0 ) ;
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

        //************
        // CSP = PWRD 
        //************

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
                        GENLA_LR ( NXTRG , Q1 , B1 , P1 ) ;
                        GENRXLIT ( XL , NXTRG + 1 , LEN , 0 ) ;
                        P1 := NXTRG ;
                        FINDRP ;
                        GENLA_LR ( NXTRG , FILHDRSZ , FILADR , 0 ) ;
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
          PRDI , PRDR , PSKP , PLIM , PRDB , PRDH , PRDY :
            FILESETUP ( 1 ) ;
          PRDS , PWRC , PWRI , PWRB , PWRP , PFDF :
            FILESETUP ( 2 ) ;
          PWRS , PWRR , PWRX :
            begin
              FILESETUP ( 3 ) ;
            end (* tag/ca *) ;
          PWRV : FILESETUP ( 2 ) ;
          PRDV : FILESETUP ( 2 ) ;
          PRDC : FILESETUP ( 2 ) ;

        //**************************************************
        // RFC returns result at the place of the length    
        // argument, so that the two elements of the stack  
        // must not pe popped by the CSP RFC.               
        // Compiler generates a STO C instruction after     
        // the CSP RFC call. This is done to allow for      
        // a CHK instruction after the READ of a char       
        //**************************************************

          PRFC : begin
                   FILESETUP ( 2 ) ;
                   TOP := TOP + 2 ;
                 end (* tag/ca *) ;
          PRFS , PRFV :
            FILESETUP ( 3 ) ;
          otherwise
            ERROR_SYMB ( 607 , P_OPCODE )
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
        case OPNDTYPE of

        //************************************************************
        // check addresses - if they are nil                          
        //************************************************************

          ADR : begin
                  with STK [ TOP - 1 ] do
                    if VRBL then
                      begin
                        if not AVAIL [ 2 ] then
                          if not ( ( VPA = RGS ) and ( RGADR = 2 ) )
                          then
                            begin
                              J := 0 ;

        //*************
        // CLEAR GPR 2 
        //*************

                              for I := TOP - 2 DOWNTO 1 do
                                with STK [ I ] do
                                  if VRBL then
                                    if ( not DRCT ) or ( DTYPE <> REEL
                                    ) then
                                      if ( VPA = RGS ) and ( RGADR = 2
                                      ) then
                                        J := I ;
                              if J = 0 then
                                ERROR ( 758 )
                              else
                                with STK [ J ] do
                                  begin
                                    FINDRG ;

        //****************************
        // TRADE GPR2 FOR ANOTHER ONE 
        //****************************

                                    GENRR ( XLR , NXTRG , 2 ) ;

        //******************
        // THIS FREES REG 2 
        //******************

                                    RGADR := NXTRG ;
                                  end (* with *) ;
                            end (* then *) ;
                        AVAIL [ 2 ] := TRUE ;

        //*********
        // IN CASE 
        //*********

                        LOAD ( STK [ TOP - 1 ] ) ;
                        AVAIL [ 2 ] := FALSE ;
                        if RGADR <> 2 then

        //************************
        // VALUE IS IN WRONG REG. 
        //************************

                          begin
                            AVAIL [ RGADR ] := TRUE ;
                            GENRR ( XLR , 2 , RGADR ) ;
                            RGADR := 2
                          end (* then *) ;
                        RTA := PTRCHK ;
                        if P < 0 then
                          RTA := PTACHK ;
                        GENRX ( XBAL , RTREG , RTA , GBR , 0 ) ;
                        CSPACTIVE [ TRG15 ] := FALSE ;
                        CSPACTIVE [ TRG1 ] := FALSE ;

        //******************
        // R1,R15 DESTROYED 
        //******************

                      end (* then *)
                    else

        //******************************************
        // ^ VAR,  I.E. CHECK A CONSTANT EXPRESSION 
        //******************************************

                      if ( FPA . DSPLMT < P ) or ( FPA . DSPLMT > Q )
                      then
                        begin
                          ERROR ( 302 ) ;
                          WRITELN ( OUTPUT , '****' : 7 , FPA . DSPLMT
                                    : 9 , ' IS NOT IN THE RANGE:' , P :
                                    9 , Q : 10 ) ;
                        end (* then *) ;
                end (* tag/ca *) ;

        //************************************************************
        // check integers or indexes for ranges                       
        //************************************************************

          INT , INX :
            begin
              with STK [ TOP - 1 ] do
                if VRBL then
                  begin
                    if not DRCT then
                      LOAD ( STK [ TOP - 1 ] ) ;
                    FPA . DSPLMT := FPA . DSPLMT - P ;
                    LOAD ( STK [ TOP - 1 ] ) ;

        //*****************************
        // later literal will be built 
        // with two margin values      
        //*****************************

                    I_S_R . I1 := Q - P ;
                    I_S_R . I2 := P ;

        //*******************************************
        // address of CL = first part of literal     
        //*******************************************

                    GENRX_2 ( XCL , RGADR , 0 , 0 , 0 , 99 ) ;
                    if ASM then
                      begin
                        WRITE ( LIST002 , '=A(' , Q - P : 1 , ',' , P :
                                1 , ')' ) ;
                        LIST002_NEWLINE ;
                      end (* then *) ;
                    UPD_DBLTBL ( PCOUNTER - 1 , I_S_R . R ) ;

        //*******************************************
        // address of branch will be filled in later 
        //*******************************************

                    GENRX ( XBC , LEQCND , 0 , 0 , 0 ) ;
                    BPC := PCOUNTER ;
                    if ASM then
                      begin
                        WRITE ( LIST002 , ' ' , '## ' , ' ' : SPACEASMX
                                , 'BNH   @OK' ) ;
                        LIST002_NEWLINE ;
                      end (* then *) ;
                    if RGADR <> 2 then
                      GENRR ( XLR , 2 , RGADR ) ;
                    if OPNDTYPE = PROC then
                      RTA := PRMCHK
                    else
                      if OPNDTYPE = INX then
                        RTA := INXCHK
                      else
                        RTA := RNGCHK ;
                    GENRX ( XBAL , RTREG , RTA , GBR , 0 ) ;

        //*******************************************
        // subprogram needs both parts of literal    
        // for error message; put reg + displ        
        // after call                                
        //*******************************************

                    UPD_DBLTBL ( PCOUNTER , I_S_R . R ) ;
                    if ASM then
                      begin
                        HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                        WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' )
                                ;
                        WRITE ( LIST002 , ' ' : COLASMI , ' ' :
                                SPACEASMI ) ;
                        WRITE ( LIST002 , '=A(' , Q - P : 1 , ',' , P :
                                1 , ')' ) ;
                        LIST002_NEWLINE ;
                      end (* then *) ;
                    PCOUNTER := NEXTPC ( 1 ) ;
                    FPA . DSPLMT := P ;
                    CODE . H [ BPC - 1 ] := TO_HINT ( BASE_DSPLMT (
                                            PCOUNTER ) ) ;
                    if ASM then
                      begin
                        WRITE ( LIST002 , ' ' , '## ' , ' ' : SPACEASML
                                , '@OK    DS    0H' ) ;
                        LIST002_NEWLINE ;
                      end (* then *)
                  end (* then *)
                else

        //******************************************
        // ^ VAR,  I.E. CHECK A CONSTANT EXPRESSION 
        //******************************************

                  if ( FPA . DSPLMT < P ) or ( FPA . DSPLMT > Q ) then
                    begin
                      ERROR ( 302 ) ;
                      WRITELN ( OUTPUT , '****' : 7 , FPA . DSPLMT : 9
                                , ' IS NOT IN THE RANGE:' , P : 9 , Q :
                                10 ) ;
                    end (* then *) ;
            end (* tag/ca *) ;

        //************************************************************
        // non means CHK E ... that is: generate runtime error        
        //************************************************************

          NON : begin
                  LOAD ( STK [ TOP - 1 ] ) ;
                  FREEREG ( STK [ TOP - 1 ] ) ;
                  GENRR ( XLR , 1 , 2 ) ;
                  GENRR ( 0 , 5 , 3 ) ;
                  TOP := TOP - 1 ;
                end (* tag/ca *) ;
          otherwise
            ;
        end (* case *) ;
      end (* CHKOPERATION *) ;


   procedure FORCESTK ( var STE : DATUM ) ;

   //************************************
   // FORCES A SET INTO RUN-STACK MEMORY 
   //************************************


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

        //**********
        // PLEN = 4 
        //**********

                  begin
                    GETOPERAND ( STE , Q1 , P1 , B1 ) ;
                    GENRX ( XST , R , Q1 , P1 , B1 ) ;
                  end (* else *) ;
                AVAIL [ R ] := TRUE ;
              end (* then *)
            else
              if DRCT and not VRBL then
                begin

        //***********************************
        // TRANSFER A CONSTANT ONTO RUNSTACK 
        //***********************************

                  VPA := ONSTK ;
                  VRBL := TRUE ;
                  GETQB ( STE , Q1 , P1 , 0 ) ;
                  GENSSLIT ( XMVC , PLEN , Q1 , P1 , PCNST -> ) ;
                end (* then *)
              else

        //****************************
        // SET IS SOMEWHERE IN MEMORY 
        //****************************

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
        CSPACTIVE [ TRG1 ] := FALSE ;

        //************************
        // INDICATE LOSS OF REG 1 
        //************************

      end (* FORCESTK *) ;


   procedure BSETOPS ;

   //***********************
   // BINARY SET OPERATIONS 
   //***********************


      var L , R : DATUM ;
          Q1 , Q2 : ADRRNG ;
          P1 , P2 , B2 : RGRNG ;
          I , J , STKADRX : INTEGER ;
          MIN , LEN : PLNRNG ;
          LR : BOOLEAN ;
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
             L , R : DATUM ;

         begin (* INN_OP *)
           L := STK [ TOP - 1 ] ;
           R := STK [ TOP ] ;
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

           //*****************************
           // BOTH OPERANDS ARE CONSTANTS 
           //*****************************

                   I := L . FPA . DSPLMT MOD 64 ;
                   J := L . FPA . DSPLMT DIV 64 ;
                   L . FPA . DSPLMT := ORD ( I in R . PCNST -> . S [ J
                                       + 1 ] ) ;
                 end (* then *)
               else
                 if not ( R . DRCT and ( R . VPA = RGS ) ) then
                   begin

           //******************************************
           // LEFT OPND IS CONST, RIGHT OPND IN MEMORY 
           //******************************************

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

           //****************************************
           // LEFT OPND IS CONST, RIGHT OPND IN REGS 
           //****************************************

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

           //********
           // L.VRBL 
           //********

             if R . PLEN <= 0 then
               begin
                 FREEREG ( L ) ;
                 L . VRBL := FALSE ;
                 L . FPA . LVL := 0 ;
                 L . FPA . DSPLMT := 0 ;
               end (* then *)
             else

           //************
           // R.PLEN > 0 
           //************

               if not R . VRBL then
                 begin

           //******************************
           // TRY FOR BETTER CODE SEQUENCE 
           //******************************

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
                             begin
                               WRITE ( LIST002 , ' ' , '## ' , ' ' :
                                       SPACEASMX , 'BNH    T' , TESTCNT
                                       + 1 : 1 ) ;
                               LIST002_NEWLINE ;
                             end (* then *) ;
                           GENRX ( XBC , LEQCND , 0 , 0 , 0 )
                         end (* then *) ;
                     end (* while *) ;
                   if ASM then
                     begin
                       TESTCNT := TESTCNT + 1 ;
                       WRITE ( LIST002 , ' ' , '## ' , ' ' : SPACEASML
                               , 'T' , TESTCNT : 1 , '     DS    0H' )
                               ;
                       LIST002_NEWLINE ;
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

           //*************************************
           // R.VRBL OR UNOPTIMIZED CASE OF ABOVE 
           //*************************************

                   LOAD ( L ) ;
                   if R . PLEN <= 8 then
                     begin

           //*******************************
           // OPERATE ON RIGHT OPND IN REGS 
           //*******************************

                       LOAD ( R ) ;
                       GENLA_LR ( 0 , R . PLEN * 8 , 0 , 0 ) ;
                       GENRR ( XCLR , L . RGADR , 0 ) ;
                       GENRELRX ( XBC , GEQCND , 5 ) ;

           //**********
           // BNL *+10 
           //**********

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

           //*************************
           // RIGHT OPERAND IN MEMORY 
           //*************************

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
                           WRITE ( LIST002 , ' ' , '## ' , ' ' :
                                   SPACEASMX , 'BH    T' , TESTCNT : 1
                                   ) ;
                           LIST002_NEWLINE ;
                         end (* then *) ;
                       GENRELRX ( XBC , GRTCND , 12 ) ;

           //*********
           // BH *+24 
           //*********

                       GENLA_LR ( TRG1 , 7 , 0 , 0 ) ;
                       GENRR ( XNR , TRG1 , L . RGADR ) ;
                       GENRS ( XSRA , L . RGADR , 0 , 3 , 0 ) ;
                       if R . VRBL then
                         GENRX ( XIC , L . RGADR , Q2 , L . RGADR , P2
                                 )
                       else
                         begin
                           if ASM then
                             begin
                               WRITE ( LIST002 , ' ' , '## ' , ' ' :
                                       SPACEASMX , '<constant> ' ) ;
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
                         begin
                           WRITE ( LIST002 , ' ' , '## ' , ' ' :
                                   SPACEASML , 'T' , TESTCNT : 1 ,
                                   '     DS    0H' ) ;
                           LIST002_NEWLINE ;
                         end (* then *) ;
                       BRCND := LESCND ;
                     end (* else *) ;
                 end ;

           //********
           // L.VRBL 
           //********

           FREEREG ( L ) ;
           FREEREG ( R ) ;
           L . DTYPE := BOOL ;
           STK [ TOP - 1 ] := L ;
           CSPACTIVE [ TRG1 ] := FALSE ;
         end (* INN_OP *) ;


      procedure ASE_OP ;

         var L , R : DATUM ;
             INSTR : INTEGER ;

         begin (* ASE_OP *)
           if FALSE then
             begin
               WRITE ( TRACEF , 'DUMPSTK vor ASE - ' ) ;
               WRITELN ( TRACEF , 'LINECNT = ' , LINECNT : 1 ) ;
               DUMPSTK ( TOP - 1 , TOP )
             end (* then *) ;
           L := STK [ TOP - 1 ] ;
           R := STK [ TOP ] ;
           if Q < 0 then

           //*******************************
           // OPERANDS ARE IN REVERSE ORDER 
           //*******************************

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

           //********************************
           // CHECK THAT ELEMENT IS IN RANGE 
           //********************************

               GENRR ( XBALR , TRG14 , 0 ) ;
               GENLA_LR ( TRG1 , L . PLEN * 8 - 1 , 0 , 0 ) ;
               GENRR ( XCLR , R . RGADR , TRG1 ) ;
               GENRX ( XBC , GRTCND , SETCHK , GBR , 0 ) ;
             end (* then *) ;
           if L . PLEN <= 8 then
             begin

           //****************************
           // PRODUCE THE RESULT IN REGS 
           //****************************

               LOAD ( L ) ;
               GENLA_LR ( TRG1 , 1 , 0 , 0 ) ;
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

           //**************************
           // OPERATE ON SET IN MEMORY 
           //**************************

               FORCESTK ( L ) ;
               GETQB ( L , Q1 , P1 , 0 ) ;
               GENLA_LR ( TRG15 , 7 , 0 , 0 ) ;
               GENRR ( XNR , TRG15 , R . RGADR ) ;
               GENRS ( XSRL , R . RGADR , 0 , 3 , 0 ) ;
               GENLA_LR ( TRG1 , Q1 , P1 , R . RGADR ) ;
               GENLA_LR ( R . RGADR , 128 , 0 , 0 ) ;
               GENRS ( XSRL , R . RGADR , 0 , 0 , TRG15 ) ;
               INSTR := XOI * SL24 + TRG1 * SL12 ;
               GENRXLIT_EXTENDED ( XEX , R . RGADR , INSTR , 0 , XOI )
                                   ;

           //***********
           // OI 0(1),0 
           //***********

               CSPACTIVE [ TRG15 ] := FALSE ;

           //*************************
           // INDICATE LOSS OF REG 15 
           //*************************

             end (* else *) ;
           AVAIL [ R . RGADR ] := TRUE ;
           STK [ TOP - 1 ] := L ;
           CSPACTIVE [ TRG1 ] := FALSE ;
         end (* ASE_OP *) ;


      procedure ASR_OP ;

         var S , R1 , R2 , RX : DATUM ;
             REVERSE : BOOLEAN ;
             INSTR : INTEGER ;

         begin (* ASR_OP *)
           if FALSE then
             begin
               WRITELN ( TRACEF , 'DUMPSTK vor ASR - p = ' , P : 1 ,
                         ' q = ' , Q : 1 ) ;
               WRITELN ( TRACEF , 'LINECNT = ' , LINECNT : 1 ) ;
               DUMPSTK ( TOP - 2 , TOP )
             end (* then *) ;
           REVERSE := FALSE ;
           S := STK [ TOP - 2 ] ;
           R1 := STK [ TOP - 1 ] ;
           R2 := STK [ TOP ] ;
           if P < 0 then

           //*******************************
           // OPERANDS ARE IN REVERSE ORDER 
           //*******************************

             begin
               REVERSE := TRUE ;
               S := STK [ TOP ] ;
               R1 := STK [ TOP - 2 ] ;
               R2 := STK [ TOP - 1 ] ;
               P := - P
             end (* then *) ;
           if Q = 2 then
             begin
               RX := R1 ;
               R1 := R2 ;
               R2 := RX
             end (* then *) ;
           if S . DTYPE <> PSET then
             ERROR ( 615 ) ;
           if R1 . DTYPE <> INT then
             if R1 . DTYPE <> HINT then
               ERROR ( 602 ) ;
           if R2 . DTYPE <> INT then
             if R2 . DTYPE <> HINT then
               ERROR ( 602 ) ;
           LOAD ( R1 ) ;
           LOAD ( R2 ) ;
           if DEBUG then
             begin

           //********************************
           // CHECK THAT ELEMENT IS IN RANGE 
           //********************************

               GENRR ( XBALR , TRG14 , 0 ) ;
               GENLA_LR ( TRG1 , S . PLEN * 8 - 1 , 0 , 0 ) ;
               GENRR ( XCLR , R2 . RGADR , TRG1 ) ;
               GENRX ( XBC , GRTCND , SETCHK , GBR , 0 ) ;
             end (* then *) ;
           if S . PLEN <= 8 then
             begin

           //****************************
           // PRODUCE THE RESULT IN REGS 
           //****************************

               GENRR ( XCLR , R1 . RGADR , R2 . RGADR ) ;
               if S . PLEN > 4 then
                 GENRELRX ( XBC , GRTCND , 17 )
               else
                 GENRELRX ( XBC , GRTCND , 16 ) ;
               LOAD ( S ) ;
               FINDRG ;
               GENRR ( XLR , NXTRG , R2 . RGADR ) ;
               GENRR ( XSR , NXTRG , R1 . RGADR ) ;
               GENLA_LR ( NXTRG , 1 , NXTRG , 0 ) ;
               GENRR ( XSR , TRG0 , TRG0 ) ;
               GENRR ( XBCTR , TRG0 , TRG0 ) ;
               GENRR ( XSR , TRG1 , TRG1 ) ;
               GENRR ( XBCTR , TRG1 , TRG0 ) ;
               GENRR ( XLCR , NXTRG , NXTRG ) ;
               GENRS ( XSLDL , TRG0 , 0 , 64 , NXTRG ) ;
               AVAIL [ NXTRG ] := TRUE ;
               GENRS ( XSRDL , TRG0 , 0 , 0 , R1 . RGADR ) ;
               if S . PLEN > 4 then
                 begin
                   GENRR ( XORX , S . RGADR , TRG0 ) ;
                   GENRR ( XORX , S . RGADR + 1 , TRG1 ) ;
                 end (* then *)
               else
                 begin
                   GENRR ( XORX , S . RGADR , TRG0 ) ;
                 end (* else *) ;
             end (* then *)
           else
             begin

           //**************************
           // OPERATE ON SET IN MEMORY 
           //**************************

               FORCESTK ( S ) ;
               GETQB ( S , Q1 , P1 , 0 ) ;
               GENRR ( XCLR , R1 . RGADR , R2 . RGADR ) ;
               GENRELRX ( XBC , GRTCND , 39 ) ;
               GENLA_LR ( TRG15 , 7 , 0 , 0 ) ;
               GENRR ( XNR , TRG15 , R1 . RGADR ) ;
               GENRELRX ( XBC , NEQCND , 19 ) ;
               FINDRG ;
               GENRR ( XLR , NXTRG , R2 . RGADR ) ;
               GENRR ( XSR , NXTRG , R1 . RGADR ) ;
               GENRXLIT ( XCH , NXTRG , 8 , - 1 ) ;
               GENRELRX ( XBC , LESCND , 13 ) ;
               GENRR ( XLR , NXTRG , R1 . RGADR ) ;
               GENRS ( XSRL , NXTRG , 0 , 3 , 0 ) ;
               GENLA_LR ( TRG1 , Q1 , P1 , NXTRG ) ;
               GENSI ( XMVI , 0 , TRG1 , 0xff ) ;
               GENLA_LR ( R1 . RGADR , 8 , R1 . RGADR , 0 ) ;
               GENRELRX ( XBC , ANYCND , - 23 ) ;
               GENRR ( XLR , NXTRG , R1 . RGADR ) ;
               GENRS ( XSRL , NXTRG , 0 , 3 , 0 ) ;
               GENLA_LR ( TRG1 , Q1 , P1 , NXTRG ) ;
               GENLA_LR ( NXTRG , 128 , 0 , 0 ) ;
               GENRS ( XSRL , NXTRG , 0 , 0 , TRG15 ) ;
               INSTR := XOI * SL24 + TRG1 * SL12 ;   // OI 0(1),X'00'
               GENRXLIT_EXTENDED ( XEX , NXTRG , INSTR , 0 , XOI ) ;
               GENLA_LR ( R1 . RGADR , 1 , R1 . RGADR , 0 ) ;
               GENRELRX ( XBC , ANYCND , - 38 ) ;
               AVAIL [ NXTRG ] := TRUE ;
               CSPACTIVE [ TRG15 ] := FALSE ;

           //*************************
           // INDICATE LOSS OF REG 15 
           //*************************

             end (* else *) ;
           AVAIL [ R1 . RGADR ] := TRUE ;
           AVAIL [ R2 . RGADR ] := TRUE ;
           TOP := TOP - 1 ;
           STK [ TOP - 1 ] := S ;
           CSPACTIVE [ TRG1 ] := FALSE ;
         end (* ASR_OP *) ;


      begin (* BSETOPS *)
        case OPCODE of

        //**********************************************************
        // UNI implementieren                                       
        //**********************************************************

          PUNI : begin
                   L := STK [ TOP - 1 ] ;
                   R := STK [ TOP ] ;
                   if L . DTYPE <> PSET then
                     ERROR ( 615 ) ;
                   if R . DTYPE <> PSET then
                     ERROR ( 615 ) ;
                   STKADRX := L . STKADR ;

        //**************************************
        // len = maximum length of the operands 
        // ok on union                          
        //**************************************

                   LEN := L . PLEN ;
                   if LEN < R . PLEN then
                     LEN := R . PLEN ;

        //******************************************
        // ONE time loop - using break to terminate 
        //******************************************

                   repeat

        //****************************************************
        // the right operand is null                          
        // nothing to do                                      
        //****************************************************

                     if R . PLEN <= 0 then
                       break ;

        //****************************************************
        // the left operand is null                           
        // replace the left operand with the right operand    
        //****************************************************

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

        //****************************
        // BOTH OPERANDS ARE NOT NULL 
        //****************************

                     if not L . VRBL and not R . VRBL then

        //********************************************************
        // both operands are constants, operation at compile time 
        //********************************************************

                       begin
                         for I := 1 to MXSETINX do
                           L . PCNST -> . S [ I ] := L . PCNST -> . S [
                                                   I ] + R . PCNST -> .
                                                   S [ I ] ;
                         MINCONSTSET ;
                         break ;
                       end (* then *) ;

        //***************************************
        // one of the operands is not a constant 
        //***************************************

                     if LEN <= 8 then

        //*****************************************
        // len <= 8 - generate result in registers 
        //*****************************************

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

        //***********************
        // EQUAL LENGTH OPERANDS 
        //***********************

                             if not ( L . VRBL and L . DRCT and ( L .
                             VPA = RGS ) ) then
                               if R . VRBL and R . DRCT and ( R . VPA =
                               RGS ) then
                                 LR := FALSE
                               else
                                 LOAD ( L ) ;
                         if not LR then

        //**********************
        // INTERCHANGE OPERANDS 
        //**********************

                           begin
                             L := R ;
                             R := STK [ TOP - 1 ]
                           end (* then *) ;
                         if R . VRBL then
                           if R . DRCT and ( R . VPA = RGS ) then
                             begin

        //****************************
        // BOTH OPERANDS IN REGISTERS 
        //****************************

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

        //*****************************************
        // LEFT OPND IN REGS, RIGHT OPND IN MEMORY 
        //*****************************************

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

        //****************************************
        // LEFT OPND IN REGS, RIGHT OPND IS CONST 
        //****************************************

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

        //***************************************
        // len > 8 - most complicated situation  
        //***************************************

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

        //*************************
        // BOTH OPERANDS IN MEMORY 
        //*************************

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

        //***************************************
        // LEFT OPND IN MEM, RIGHT OPND IS CONST 
        //***************************************

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

        //*******************************************
        // this is done in any case before returning 
        //*******************************************

                   L . STKADR := STKADRX ;
                   L . PLEN := LEN ;
                   STK [ TOP - 1 ] := L ;
                   CSPACTIVE [ TRG1 ] := FALSE ;
                 end (* tag/ca *) ;

        //**********************************************************
        // INT implementieren                                       
        //**********************************************************

          PINT : begin
                   L := STK [ TOP - 1 ] ;
                   R := STK [ TOP ] ;
                   if L . DTYPE <> PSET then
                     ERROR ( 615 ) ;
                   if R . DTYPE <> PSET then
                     ERROR ( 615 ) ;
                   STKADRX := L . STKADR ;

        //**************************************
        // len = minimum length of the operands 
        // ok on intersection                   
        //**************************************

                   LEN := L . PLEN ;
                   if LEN > R . PLEN then
                     LEN := R . PLEN ;

        //******************************************
        // ONE time loop - using break to terminate 
        //******************************************

                   repeat

        //***************************
        // ONE OR BOTH OPERANDS NULL 
        //***************************

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

        //****************************
        // BOTH OPERANDS ARE NOT NULL 
        //****************************

                     if not L . VRBL and not R . VRBL then

        //********************************************************
        // both operands are constants, operation at compile time 
        //********************************************************

                       begin
                         for I := 1 to MXSETINX do
                           L . PCNST -> . S [ I ] := L . PCNST -> . S [
                                                   I ] * R . PCNST -> .
                                                   S [ I ] ;
                         MINCONSTSET ;
                         break ;
                       end (* then *) ;

        //***************************************
        // one of the operands is not a constant 
        //***************************************

                     if LEN <= 8 then

        //*****************************************
        // len <= 8 - generate result in registers 
        //*****************************************

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

        //***********************
        // EQUAL LENGTH OPERANDS 
        //***********************

                             if not ( L . VRBL and L . DRCT and ( L .
                             VPA = RGS ) ) then
                               if R . VRBL and R . DRCT and ( R . VPA =
                               RGS ) then
                                 LR := FALSE
                               else
                                 LOAD ( L ) ;
                         if not LR then

        //**********************
        // INTERCHANGE OPERANDS 
        //**********************

                           begin
                             L := R ;
                             R := STK [ TOP - 1 ]
                           end (* then *) ;
                         if R . VRBL then
                           if R . DRCT and ( R . VPA = RGS ) then
                             begin

        //****************************
        // BOTH OPERANDS IN REGISTERS 
        //****************************

                               GENRR ( XNR , L . RGADR , R . RGADR ) ;
                               AVAIL [ R . RGADR ] := TRUE ;
                               if L . PLEN > 4 then
                                 GENRR ( XNR , L . RGADR + 1 , R .
                                         RGADR + 1 ) ;
                               if R . PLEN > 4 then
                                 AVAIL [ R . RGADR + 1 ] := TRUE ;
                             end (* then *)
                           else

        //*****************************************
        // LEFT OPND IN REGS, RIGHT OPND IN MEMORY 
        //*****************************************

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

        //****************************************
        // LEFT OPND IN REGS, RIGHT OPND IS CONST 
        //****************************************

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

        //***************************************
        // len > 8 - most complicated situation  
        //***************************************

                     FORCESTK ( L ) ;
                     if R . VRBL then
                       begin

        //*************************
        // BOTH OPERANDS IN MEMORY 
        //*************************

                         GETQB ( L , Q1 , P1 , 0 ) ;
                         TXRG := TRG1 ;
                         GETQB ( R , Q2 , P2 , 0 ) ;
                         TXRG := TRG14 ;
                         GENSS ( XNC , LEN , Q1 , P1 , Q2 , P2 ) ;
                       end (* then *)
                     else
                       begin

        //***************************************
        // LEFT OPND IN MEM, RIGHT OPND IS CONST 
        //***************************************

                         COMPACT ( R . PCNST -> , LEN , J , CHR ( 255 )
                                   ) ;
                         GETQB ( L , Q1 , P1 , J ) ;
                         LEN := ALIGN ( LEN , INTSIZE ) ;
                         if LEN >= J then
                           GENSSLIT ( XNC , LEN - J , Q1 + J , P1 , R .
                                      PCNST -> ) ;
                       end (* else *) ;
                   until TRUE ;

        //*******************************************
        // this is done in any case before returning 
        //*******************************************

                   L . STKADR := STKADRX ;
                   L . PLEN := LEN ;
                   STK [ TOP - 1 ] := L ;
                   CSPACTIVE [ TRG1 ] := FALSE ;
                 end (* tag/ca *) ;

        //**********************************************************
        // DIF implementieren                                       
        //**********************************************************

          PDIF : begin
                   L := STK [ TOP - 1 ] ;
                   R := STK [ TOP ] ;
                   if L . DTYPE <> PSET then
                     ERROR ( 615 ) ;
                   if R . DTYPE <> PSET then
                     ERROR ( 615 ) ;

        //**************************************
        // len = maximum length of the operands 
        // ok on set difference                 
        //**************************************

                   LEN := L . PLEN ;
                   if LEN < R . PLEN then
                     LEN := R . PLEN ;

        //******************************************
        // ONE time loop - using break to terminate 
        //******************************************

                   repeat

        //****************************************************
        // the right operand is null                          
        // nothing to do                                      
        //****************************************************

                     if R . PLEN <= 0 then
                       break ;

        //****************************************************
        // the left operand is null                           
        //****************************************************

                     if L . PLEN <= 0 then
                       begin
                         FREEREG ( R ) ;
                         break ;
                       end (* then *) ;

        //****************************
        // BOTH OPERANDS ARE NOT NULL 
        //****************************

                     if not L . VRBL and not R . VRBL then

        //********************************************************
        // both operands are constants, operation at compile time 
        //********************************************************

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

        //***************************************
        // one of the operands is not a constant 
        //***************************************

                     if L . PLEN <= 8 then

        //*****************************************
        // len <= 8 - generate result in registers 
        //*****************************************

                       begin
                         LOAD ( L ) ;
                         if R . VRBL then
                           begin
                             if not ( R . DRCT and ( R . VPA = RGS ) )
                             then
                               begin

        //************************
        // FORCE R INTO REGISTERS 
        //************************

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

        //***************************************
        // LEFT OPND IN REGS, RIGHT OPND IS CNST 
        //***************************************

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

        //***************************************
        // len > 8 - most complicated situation  
        //***************************************

                     FORCESTK ( L ) ;
                     if R . VRBL then
                       begin

        //***************************************
        // fraglich, ob das hier richtig ist     
        //***************************************

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

        //*********************
        // RIGHT OPND IS CONST 
        //*********************

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
                   until TRUE ;
                   STK [ TOP - 1 ] := L ;
                   CSPACTIVE [ TRG1 ] := FALSE ;
                 end (* tag/ca *) ;

        //**********************************************************
        // INN implementieren                                       
        //**********************************************************

          PINN : INN_OP ;

        //**********************************************************
        // ASE implementieren                                       
        //**********************************************************

          PASE : ASE_OP ;

        //**********************************************************
        // ASR implementieren                                       
        //**********************************************************

          PASR : ASR_OP ;
        end (* case *) ;
      end (* BSETOPS *) ;


   procedure CSETOPS ;

   //**********************************************
   // CONTROL AND MISCELLANEOUS OPERATIONS ON SETS 
   //**********************************************


      var Q1 , Q2 : ADRRNG ;
          P1 , P2 , B1 : RGRNG ;
          L , R : DATUM ;
          K : INTEGER ;


      procedure FORCESET ( var STE : DATUM ; LEN : INTEGER ) ;

      //*******************************************
      // CONVERTS A SET ADDR INTO SET ON RUN STACK 
      //*******************************************


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

           //*****************
           // TO BE SET LATER 
           //*****************

               end (* then *)
             else
               if DTYPE <> PSET then
                 begin
                   WRITELN ( TRACEF , 'dtype falsch = ' , DTYPE ) ;
                   ERROR ( 615 ) ;
                 end (* then *)
         end (* FORCESET *) ;


      begin (* CSETOPS *)
        case OPCODE of
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

        //************************************
        // THIS CASE NEVER OCCURS IN PRACTICE 
        //************************************

                         VRBL := FALSE ;
                         VPA := NEITHER ;
                       end (* then *)
                     else
                       if P <= 8 then
                         begin

        //**************
        // CLEAR REG(S) 
        //**************

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

        //***************************
        // CLEAR MEMORY ON RUN-STACK 
        //***************************

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

        //****************************************
        // OPERAND = SINGLE REG. OR A MEMORY AREA 
        //****************************************

                     FINDRG ;

        //*********************
        // REGISTER FOR RESULT 
        //*********************

                     GENRR ( XSR , NXTRG , NXTRG ) ;
                     if PLEN > 4 then
                       begin

        //****************
        // MEMORY OPERAND 
        //****************

                         GETOPERAND ( STK [ TOP - 1 ] , Q1 , P1 , B1 )
                                      ;
                         if P1 <> TRG14 then

        //***********************
        // WE NEED AN INDEX REG. 
        //***********************

                           begin
                             GENLA_LR ( TRG14 , Q1 , P1 , B1 ) ;
                             P1 := TRG14 ;
                             Q1 := 0 ;
                             B1 := 0 ;
                           end (* then *) ;
                         GENLA_LR ( TRG1 , PLEN DIV 4 , 0 , 0 ) ;
                         GENRX ( XL , 0 , Q1 , P1 , B1 ) ;
                         P2 := 0 ;
                       end (* then *)
                     else
                       P2 := RGADR ;
                     GENRR ( XLTR , 15 , P2 ) ;
                     GENRELRX ( XBC , EQUCND , 6 ) ;

        //*********
        // BZ *+12 
        //*********

                     GENRR ( XBCTR , P2 , 0 ) ;
                     GENRR ( XNR , P2 , 15 ) ;
                     GENRELRX ( XBCT , NXTRG , - 5 ) ;

        //****************
        // BCT NXTRG,*-10 
        //****************

                     if PLEN > 4 then
                       begin
                         GENLA_LR ( P1 , 4 , P1 , 0 ) ;
                         GENRELRX ( XBCT , TRG1 , - 11 ) ;

        //*************
        // BCT R1,*-22 
        //*************

                       end (* then *)
                     else
                       AVAIL [ RGADR ] := TRUE ;
                     GENRR ( XLPR , NXTRG , NXTRG ) ;
                     DTYPE := BOOL ;
                     DRCT := TRUE ;
                     VRBL := TRUE ;
                     VPA := RGS ;
                     RGADR := NXTRG ;
                     CSPACTIVE [ TRG15 ] := FALSE ;

        //*************************
        // INDICATE LOSS OF REG 15 
        //*************************

                   end (* with *) ;
          PSMV : begin
                   if FALSE then
                     begin
                       WRITELN ( TRACEF , 'DUMPSTK vor SMV' ) ;
                       DUMPSTK ( TOP - 2 , TOP - 1 )
                     end (* then *) ;
                   TOP := TOP - 2 ;
                   if P < 0 then

        //*******************
        // REVERSED OPERANDS 
        //*******************

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

        //*************************************
        // L = DESTINATION SET, R = SOURCE SET 
        //*************************************

                   if R . VRBL then
                     begin
                       if ( R . PLEN <= 8 ) and ( P = 4 ) and DEBUG
                       then
                         LOAD ( R ) ;
                       if R . DRCT and ( R . VPA = RGS ) then
                         if P < R . PLEN then

        //***************
        // R.PLEN=8, P=4 
        //***************

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

        //*********
        // NOTHING 
        //*********

                           
                       else

        //****************
        // R IS IN MEMORY 
        //****************

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

        //*****************
        // R IS A CONSTANT 
        //*****************

                     if R . PLEN > P then
                       begin
                         ERROR ( 303 ) ;
                         R . PLEN := P ;
                       end (* then *) ;
                   if P > R . PLEN then
                     begin

        //***********************************
        // CLEAR EXCESS BYTES IN DESTINATION 
        //***********************************

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

        //************
        // R.PLEN = 4 
        //************

                         GENRX ( XST , R . RGADR , Q1 , P1 , 0 )
                     else

        //****************
        // R IS IN MEMORY 
        //****************

                       GENSS ( XMVC , R . PLEN , Q1 , P1 , Q2 , P2 )
                   else

        //*********************
        // R IS A CONSTANT SET 
        //*********************

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
        CSPACTIVE [ TRG1 ] := FALSE ;

        //************************
        // INDICATE LOSS OF REG 1 
        //************************

      end (* CSETOPS *) ;


   procedure GEN_STRING_ADRESSE ( var SDAT : DATUM ; LOAD_REG : BOOLEAN
                                ; var OFFSET : ADRRNG ; var RGWORK :
                                RGRNG ) ;

      var B : RGRNG ;
          P2 : RGRNG ;
          Q2 : ADRRNG ;

      begin (* GEN_STRING_ADRESSE *)
        OFFSET := 0 ;
        with SDAT do
          if VPA = RGS then
            begin
              RGWORK := RGADR ;

        //******************************************************
        // offset muss ggf. drauf                               
        // bei statischen variablen                             
        //******************************************************

              P2 := FPA . LVL ;
              Q2 := FPA . DSPLMT ;
              if Q2 <> 0 then
                if P2 > 0 then
                  begin
                    BASE ( Q2 , P2 , B ) ;
                    if P2 <= 0 then
                      P2 := B
                    else
                      if B > 0 then
                        GENRR ( XAR , P2 , B ) ;
                    if Q2 = 0 then
                      GENRR ( XAR , RGWORK , P2 )
                    else
                      GENLA_LR ( RGWORK , Q2 , P2 , RGWORK ) ;
                  end (* then *)
                else
                  if LOAD_REG then
                    begin
                      if Q2 = - 1 then
                        GENRR ( XBCTR , RGWORK , 0 )
                      else
                        GENRXLIT ( XA , RGWORK , Q2 , 0 ) ;
                    end (* then *)
                  else
                    begin
                      OFFSET := Q2
                    end (* else *)
            end (* then *)
          else
            begin
              P2 := FPA . LVL ;
              Q2 := FPA . DSPLMT ;
              BASE ( Q2 , P2 , B2 ) ;
              if P2 < 0 then
                begin
                  if TRACE_LITERAL then
                    WRITELN ( TRACEF , 'repl lit. adr. 2 - index = ' ,
                              SCNSTNO : 1 , ' pc = ' , PCOUNTER + 1 : 1
                              ) ;
                  LITTBL [ SCNSTNO ] . LNK := PCOUNTER + 1 ;
                  P2 := 0 ;
                end (* then *) ;
              GENLA_LR ( 14 , Q2 , B2 , P2 ) ;
              RGWORK := 14 ;
            end (* else *)
      end (* GEN_STRING_ADRESSE *) ;


   procedure STRINGCOMPARE ( var LEFT , RIGHT : DATUM ) ;

   //****************************************************************
   // implement string (varchar) comparisons                         
   // by calling the routine $PASSCMP                                
   // which is part of PASMONN (runtime)                             
   //****************************************************************


      var LBL : PLABEL ;
          RGWORK : RGRNG ;
          LITVALUE : INTEGER ;
          DUMMY_OFFS : ADRRNG ;

      begin (* STRINGCOMPARE *)

        //******************************************************
        // show stack elements before compare                   
        //******************************************************

        if FALSE then
          begin
            WRITELN ( TRACEF , 'start stringcompare, linecnt = ' ,
                      LINECNT : 1 ) ;
            DUMPSTKELEM ( 'Left ' , LEFT ) ;
            DUMPSTKELEM ( 'Right' , RIGHT ) ;
          end (* then *) ;

        //******************************************************
        // load strcurr pointer -                               
        // stringcompare uses string workarea                   
        //******************************************************

        GENRX ( XL , TRG1 , STRCURR , 12 , 0 ) ;
        with LEFT do
          begin

        //******************************************************
        // if operand = single char                             
        // build char array of length one in scratch area       
        // and call compare routine                             
        //******************************************************

            if DTYPE = CHRC then
              begin
                GENSI ( XMVI , PIAKT -> . SCRATCHPOS , LBR , FPA .
                        DSPLMT ) ;
                LITVALUE := 1 ;
                GENRXLIT ( XL , 14 , LITVALUE , 0 ) ;
                GENRX ( XST , 14 , 0 , TRG1 , 0 ) ;
                GENLA_LR ( 14 , PIAKT -> . SCRATCHPOS , LBR , 0 ) ;
                GENRX ( XST , 14 , 4 , TRG1 , 0 ) ;
              end (* then *)
            else
              begin
                if DTYPE = CARR then
                  LITVALUE := PLEN
                else
                  LITVALUE := - 1 ;

        //******************************************************
        // store plen, if char array                            
        // or minus one, if varchar                             
        // or zero, if null string                              
        //******************************************************

                if LITVALUE = 0 then
                  begin
                    GENRR ( XXR , 14 , 14 ) ;
                    GENRX ( XST , 14 , 0 , TRG1 , 0 ) ;
                    GENRX ( XST , 14 , 4 , TRG1 , 0 ) ;
                  end (* then *)
                else
                  begin
                    GENRXLIT ( XL , 14 , LITVALUE , 0 ) ;
                    GENRX ( XST , 14 , 0 , TRG1 , 0 ) ;

        //******************************************************
        // store address of left operand                        
        // take care, if literal (carr constant)                
        //******************************************************

                    GEN_STRING_ADRESSE ( LEFT , TRUE , DUMMY_OFFS ,
                                         RGWORK ) ;
                    GENRX ( XST , RGWORK , 4 , TRG1 , 0 ) ;
                  end (* else *)
              end (* else *)
          end (* with *) ;

        //******************************************************
        // do the same for the right operand                    
        // at offsets 8 and 12 from R1                          
        //******************************************************

        with RIGHT do
          begin
            if DTYPE = CHRC then
              begin
                GENSI ( XMVI , PIAKT -> . SCRATCHPOS , LBR , FPA .
                        DSPLMT ) ;
                LITVALUE := 1 ;
                GENRXLIT ( XL , 14 , LITVALUE , 0 ) ;
                GENRX ( XST , 14 , 8 , TRG1 , 0 ) ;
                GENLA_LR ( 14 , PIAKT -> . SCRATCHPOS , LBR , 0 ) ;
                GENRX ( XST , 14 , 12 , TRG1 , 0 ) ;
              end (* then *)
            else
              begin
                if DTYPE = CARR then
                  LITVALUE := PLEN
                else
                  LITVALUE := - 1 ;
                if LITVALUE = 0 then
                  begin
                    GENRR ( XXR , 14 , 14 ) ;
                    GENRX ( XST , 14 , 8 , TRG1 , 0 ) ;
                    GENRX ( XST , 14 , 12 , TRG1 , 0 ) ;
                  end (* then *)
                else
                  begin
                    GENRXLIT ( XL , 14 , LITVALUE , 0 ) ;
                    GENRX ( XST , 14 , 8 , TRG1 , 0 ) ;
                    GEN_STRING_ADRESSE ( RIGHT , TRUE , DUMMY_OFFS ,
                                         RGWORK ) ;
                    GENRX ( XST , RGWORK , 12 , TRG1 , 0 ) ;
                  end (* else *)
              end (* else *)
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

        CSPACTIVE [ TRG15 ] := FALSE ;
        CSPACTIVE [ TRG1 ] := FALSE ;

        //******************************************************
        // set the condition mask for the following branch      
        // depending on the opcode which started the string     
        // comparison                                           
        //******************************************************

        BRCND := BRMSK [ OPCODE ] ;
      end (* STRINGCOMPARE *) ;


   procedure SETCOMPARE ( var L , R : DATUM ) ;

   //***********************************
   // GENERATE CODE FOR SET COMPARISONS 
   //***********************************


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

      //**************************************
      // GENERATES INTERMEDIATE TEST BRANCHES 
      //**************************************


         begin (* GENBRANCH *)
           if TEST_PENDING then
             begin
               TESTCNT := TESTCNT + 1 ;

           //**********************************************
           // IF ASM THEN                                  
           // BEGIN FIXUPLOC := 0;                         
           //       WRITELN(PRR,' BNZ T',TESTCNT:1);       
           // END                                          
           // ELSE                                         
           //**********************************************

               if ASM then
                 begin
                   WRITE ( LIST002 , ' ' , '## ' , ' ' : SPACEASMX ,
                           'BNZ   T' , TESTCNT : 1 ) ;
                   LIST002_NEWLINE ;
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
        if ( OPCODE = PEQU ) or ( OPCODE = PNEQ ) then
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

        //*******************
        // NULL LEFT OPERAND 
        //*******************

                if R . PLEN <= 0 then

        //********************
        // NULL RIGHT OPERAND 
        //********************

                  SETCONSTBOOL ( OPCODE = PEQU )
                else
                  if R . VRBL then
                    if R . DRCT and ( R . VPA = RGS ) then
                      if R . PLEN = 4 then
                        GENRR ( XLTR , R . RGADR , R . RGADR )
                      else
                        GENRR ( XORX , R . RGADR , R . RGADR + 1 )
                    else

        //****************
        // R IS IN MEMORY 
        //****************

                      TESTNULL ( R , Q2 , P2 , 0 )
                  else

        //***************
        // R IS CONSTANT 
        //***************

                    SETCONSTBOOL ( OPCODE <> PEQU )
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

        //****************
        // R IS IN MEMORY 
        //****************

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

        //***************
        // R IS CONSTANT 
        //***************

                        if R . PLEN > L . PLEN then
                          SETCONSTBOOL ( OPCODE <> PEQU )
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

        //****************
        // L IS IN MEMORY 
        //****************

                    if ( R . PLEN = 0 ) or ( R . VRBL and R . DRCT and
                    ( R . VPA = RGS ) ) then
                      INTCHG := TRUE
                    else
                      if R . VRBL then

        //****************
        // R IS IN MEMORY 
        //****************

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

        //***************
        // R IS CONSTANT 
        //***************

                        if L . PLEN < R . PLEN then
                          SETCONSTBOOL ( OPCODE <> PEQU )
                        else
                          begin
                            TESTNULL ( L , Q1 , P1 , R . PLEN ) ;
                            GENBRANCH ;
                            GENSSLIT ( XCLC , R . PLEN , Q1 , P1 , R .
                                       PCNST -> ) ;
                          end (* else *)
                else

        //***************
        // L IS CONSTANT 
        //***************

                  if ( R . PLEN = 0 ) or R . VRBL then
                    INTCHG := TRUE
                  else
                    begin
                      EQ := TRUE ;
                      for I := 1 to MXSETINX do
                        if L . PCNST -> . S [ I ] <> R . PCNST -> . S [
                        I ] then
                          EQ := FALSE ;
                      SETCONSTBOOL ( ( OPCODE = PEQU ) = EQ ) ;
                    end (* else *) ;
            until not INTCHG ;
          end (* then *)
        else
          begin

        //****************************************************
        // pcode IS PGEQ OR PLEQ                              
        //****************************************************

            if OPCODE = PGEQ then
              begin
                L := STK [ TOP ] ;
                R := STK [ TOP - 1 ]
              end (* then *)
            else
              begin
                L := STK [ TOP - 1 ] ;
                R := STK [ TOP ]
              end (* else *) ;
            OPCODE := PEQU ;
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

        //***************
        // R IS CONSTANT 
        //***************

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

        //***************
        // R IS CONSTANT 
        //***************

                        GENSSLIT ( XOC , L . PLEN , Q1 , P1 , R . PCNST
                                   -> ) ;

        //************************************************************
        // IF ASM THEN                                                
        //    GENSSLIT( XXC, L.PLEN, Q1, P1, R.PCNST@ )               
        // ELSE                                                       
        //                                                            
        //                                                            
        //  da machen wir nix, weil das, was unten kommt,             
        //  ja seine befehle ausgeben sollte ...                      
        //                                                            
        //************************************************************

                        begin

        //******************************
        //KLUDGE TO RE-USE SAME CONSTANT
        //******************************

                          GENSS ( XXC , L . PLEN , Q1 , P1 , 0 , 0 ) ;
                          CODE . H [ PCOUNTER - 1 ] := TO_HINT ( CODE .
                                                   H [ PCOUNTER - 4 ] )
                                                   ;
                          NXTLIT := NXTLIT + 1 ;
                          LITTBL [ NXTLIT ] . XLINECNT := LINECNT ;
                          LITTBL [ NXTLIT ] . LNK := PCOUNTER - 1 ;
                          LITTBL [ NXTLIT ] . LTYPE := 'X' ;
                          LITTBL [ NXTLIT ] . LENGTH := 0 ;
                          LITTBL [ NXTLIT ] . XIDP := 0 ;
                          LITTBL [ NXTLIT ] . OPTIMIZED := FALSE ;
                          if TRACE_LITERAL then
                            begin
                              WRITELN ( TRACEF ,
                                  '----------------------------------'
                                        ) ;
                              WRITELN ( TRACEF ,
                                        'setcompare: linecnt = ' ,
                                        LINECNT ) ;
                              WRITELN ( TRACEF ,
                                        'setcompare: index   = ' ,
                                        NXTLIT ) ;
                              WRITELN ( TRACEF ,
                                        'setcompare: lnk/pc  = ' ,
                                        PCOUNTER - 1 ) ;
                              WRITELN ( TRACEF ,
                                        'setcompare: ltype   = ' , 'X'
                                        ) ;
                              WRITELN ( TRACEF ,
                                        'setcompare: length  = ' , 0 )
                                        ;
                              WRITELN ( TRACEF ,
                                        'setcompare: xidp    = ' , 0 )
                                        ;
                              WRITELN ( TRACEF ,
                                        'setcompare: opt     = ' ,
                                        FALSE ) ;
                            end (* then *) ;
                        end ;
                      end (* else *)
                  end (* then *)
              end (* else *)
          end (* else *) ;
        FREEREG ( L ) ;
        FREEREG ( R ) ;
        L . DTYPE := BOOL ;
        if not CONSTSET then
          BRCND := BRMSK [ OPCODE ] ;
        if FIXUPLOC >= 0 then
          begin
            if ASM then
              begin
                WRITE ( LIST002 , ' ' , '## ' , ' ' : SPACEASML , 'T' ,
                        TESTCNT : 1 , '     DS    0H' ) ;
                LIST002_NEWLINE ;
              end (* then *) ;
            CODE . H [ FIXUPLOC ] := TO_HINT ( BASE_DSPLMT ( PCOUNTER )
                                     ) ;
          end (* then *) ;
        CSPACTIVE [ TRG1 ] := FALSE ;

        //************************
        // INDICATE LOSS OF REG 1 
        //************************

        TXR_CONTENTS . VALID := FALSE ;
      end (* SETCOMPARE *) ;


   procedure COPERATION ;

   //*********************************
   // CONTROL AND BRANCH INSTRUCTIONS 
   // ------------------------------- 
   //*********************************


      var PCNEU : ICRNG ;
          PC : ICRNG ;
          CASE_LAUF : INTEGER ;
          CL : LBLRNG ;
          X1 : INTEGER ;
          X2 : INTEGER ;
          LBL_WORK : PLABEL ;


      procedure MKLBL ( var LBL : PLABEL ; Q : LBLRNG ) ;

      //*******************************
      // ASSUMES     0 <= Q <= 9999999 
      //*******************************


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

      //*****************************************************
      // TO ADD A (SOURCE) LINE POINTER TO THE POINTER TABLE 
      // --------------------------------------------------- 
      //*****************************************************


         begin (* ADDLNP *)
           if NXTLNP < MXLNP then
             begin
               CODE . C [ MXCODE * 2 + NXTLNP ] := CHR ( PCDIF ) ;
               NXTLNP := NXTLNP + 1 ;
             end (* then *)
         end (* ADDLNP *) ;


      procedure UPDLNTBL ( PCDIF : ICRNG ) ;

      //************************************************************
      // TO UPDATE LINE POINTER TABLE FOR THE RUN TIME DEBUG OPTION 
      // ---------------------------------------------------------- 
      //************************************************************


         begin (* UPDLNTBL *)
           if PCDIF >= 250 then

           //*******************
           // ENTER ESCAPE MODE 
           //*******************

             begin
               ADDLNP ( 254 ) ;

           //***********
           //ESCAPE CHAR
           //***********

               ADDLNP ( PCDIF DIV 256 ) ;
               ADDLNP ( PCDIF MOD 256 ) ;
             end (* then *)
           else
             ADDLNP ( PCDIF ) ;
         end (* UPDLNTBL *) ;


      procedure INIT_CSECT ;

      //***********************************************
      // TO INITIALIZE OBJECT CODE TABLES AND POINTERS 
      // --------------------------------------------- 
      //***********************************************


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
           MANAGE_LITERALS ( 0 , PCOUNTER , NIL , 0 , DUMMYINT ) ;
           POOL_SIZE := 0 ;
           NUMLITS := 0 ;
           DBLALN := FALSE ;
           LAST_CC . LAST_PC := 0 ;
           TXR_CONTENTS . VALID := FALSE ;
           FILADR_LOADED := FALSE ;
           LAST_STR . LAST_PC := 0 ;
           LAST_MVC . LAST_PC := 0 ;
           LAST_FILE . LAST_PC := 0 ;
           PRCTBL [ 0 ] . NAME := LBL1 . NAM ;
           PRCTBL [ 0 ] . LNK := 0 ;
           PRCTBL [ 1 ] . LNK := 0 ;
           NXTPRC := 1 ;
           NXTEP := PRCCNT ;
           CALL_DEPTH := 0 ;
           PCOUNTER := 0 ;
           MINLBL := LBLMAP ( PIAKT -> . SEGSZE . NAM ) ;
           LASTPC := 0 ;

           //***********************************************
           // header for pasmain contains compile timestamp 
           //***********************************************

           if CURLVL = 1 then
             LEN_CSECTINFO := 9 + IDLNGTH + 1 + HDRLNGTH
           else
             LEN_CSECTINFO := 9 + IDLNGTH ;
           if ASM then
             begin
               if CURLVL = 1 then
                 begin
                   WRITE ( LIST002 , ' ' , 'BGN  ' : 26 , CSECT_NAME ,
                           ',' , PIAKT -> . CURPNAME , ',' , PROGHDR :
                           1 ) ;
                   LIST002_NEWLINE ;
                 end (* then *)
               else
                 begin
                   WRITE ( LIST002 , ' ' , 'BGN  ' : 26 , CSECT_NAME ,
                           ',' , PIAKT -> . CURPNAME ) ;
                   LIST002_NEWLINE ;
                 end (* else *) ;
               HEXHW ( PCOUNTER * 2 , HEXPC ) ;
               WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
               WRITE ( LIST002 , CSECT_NAME , ' CSECT' ) ;
               LIST002_NEWLINE ;
             end (* then *) ;

           //**********************************************
           // branch over constants                        
           // including procedure names                    
           //**********************************************

           BTARGET := ( 24 + LEN_CSECTINFO ) DIV 2 ;
           BTARGET := ALIGN ( BTARGET , 2 ) ;
           GENRX ( XBC , ANYCND , BTARGET * 2 , 0 , JREG ) ;

           //**********************************************
           // pcounter = position after ep constants       
           //**********************************************

           PCOUNTER := BTARGET ;
           PCAFTLIT := PCOUNTER ;

           //**********************************************
           // format ep constants area and insert them     
           //**********************************************

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

           //***********************************************
           // more ep constants:                            
           //-----------------------------------------------
           // pcounter - 9: Compiler signature (6 bytes)    
           // pcounter - 6: Compiler version   (2 bytes)    
           //-----------------------------------------------
           // pcounter - 5: stacksize (set by gen_csect)    
           //-----------------------------------------------
           // pcounter - 4: DEBUG-Level                     
           //-----------------------------------------------
           // pcounter - 3: proclen (set by gen_csect)      
           //-----------------------------------------------
           // pcounter - 2: pointer to static csect         
           // set by v-constant, see below                  
           //***********************************************

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

           //*******************************************
           // ins_prctbl: statname wird als v-adresse   
           // registriert. wert = pcounter - 2. wirkt   
           // genauso wie die ablage eines literals,    
           // aber an dieser definierten stelle.        
           // spaetere bezugnamen auf das literal       
           // Statname (als v-adresse) via              
           // UPD_PRCTBL holen dann ihre adresse        
           // von hier.                                 
           //*******************************************

           if PIAKT -> . STATNAME [ 1 ] <> ' ' then
             INS_PRCTBL ( PIAKT -> . STATNAME , PCOUNTER - 2 ) ;

           //*******************************************
           // pos of proc len                           
           //*******************************************

           POSOFPROCLEN := ( PCOUNTER - 3 ) * 2 ;

           //*******************************************
           // UNIQUE PROC NO                            
           //*******************************************

           CODE . H [ MXCODE ] := PIAKT -> . CURPNO ;

           //**********************************************
           // the procedure name which is written          
           // in case of debug is larger now (20 instead   
           // of 12), so nxtnlp has to be started at       
           // 24 instead of 16 - opp 2016                  
           //**********************************************

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
               WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
               WRITE ( LIST002 , 'DC  ' : COLASMI , ' ' : SPACEASMI ) ;
               WRITE ( LIST002 , 'AL1(' , LEN_CSECTINFO : 1 , ')' ) ;
               LIST002_NEWLINE ;
               HEXHW ( 5 , HEXPC ) ;
               WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
               WRITE ( LIST002 , 'DC  ' : COLASMI , ' ' : SPACEASMI ) ;
               WRITE ( LIST002 , 'C''' ) ;
               for I := 1 to 8 do
                 WRITE ( LIST002 , CSECT_NAME [ I ] ) ;
               WRITE ( LIST002 , ' ' ) ;
               for I := 1 to IDLNGTH do
                 WRITE ( LIST002 , PIAKT -> . CURPNAME [ I ] ) ;
               if CURLVL = 1 then
                 begin
                   WRITE ( LIST002 , ' ' ) ;
                   for I := 1 to HDRLNGTH do
                     WRITE ( LIST002 , PROGHDR [ I ] ) ;
                 end (* then *) ;
               WRITE ( LIST002 , '''' ) ;
               LIST002_NEWLINE ;
               HEXHW ( POSOFPROCLEN - 12 , HEXPC ) ;
               WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
               WRITE ( LIST002 , 'DC  ' : COLASMI , ' ' : SPACEASMI ) ;
               WRITE ( LIST002 , 'CL6''STPASC'''
                       '    -- Compiler signature' ) ;
               LIST002_NEWLINE ;
               HEXHW ( POSOFPROCLEN - 6 , HEXPC ) ;
               WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
               WRITE ( LIST002 , 'DC  ' : COLASMI , ' ' : SPACEASMI ) ;
               WRITE ( LIST002 , VERSION3 , '      -- Compiler version'
                       ) ;
               LIST002_NEWLINE ;
               HEXHW ( POSOFPROCLEN - 4 , HEXPC ) ;
               WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
               WRITE ( LIST002 , 'DC  ' : COLASMI , ' ' : SPACEASMI ) ;
               WRITE ( LIST002 , 'AL2(0)' , '         -- Stacksize' ) ;
               LIST002_NEWLINE ;
               HEXHW ( POSOFPROCLEN - 2 , HEXPC ) ;
               WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
               WRITE ( LIST002 , 'DC  ' : COLASMI , ' ' : SPACEASMI ) ;
               WRITE ( LIST002 , 'AL2(' , PIAKT -> . DEBUG_LEV : 1 ,
                       ')' , '         -- Debug-Level' ) ;
               LIST002_NEWLINE ;
               HEXHW ( POSOFPROCLEN , HEXPC ) ;
               WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
               WRITE ( LIST002 , 'DC  ' : COLASMI , ' ' : SPACEASMI ) ;
               WRITE ( LIST002 , 'AL2(0)' ,
                       '         -- Length of Proc' ) ;
               LIST002_NEWLINE ;
               HEXHW ( POSOFPROCLEN + 2 , HEXPC ) ;
               WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
               WRITE ( LIST002 , 'DC  ' : COLASMI , ' ' : SPACEASMI ) ;
               if PIAKT -> . STATNAME [ 1 ] <> ' ' then
                 begin
                   WRITE ( LIST002 , 'V(' , PIAKT -> . STATNAME , ')' ,
                           '    -- Static CSECT' ) ;
                   LIST002_NEWLINE ;
                 end (* then *)
               else
                 begin
                   WRITE ( LIST002 , 'A(0)' ,
                           '           -- Static CSECT' ) ;
                   LIST002_NEWLINE ;
                 end (* else *)
             end (* then *) ;
         end (* INIT_CSECT *) ;


      procedure GEN_CSECT ( STACKSIZE : INTEGER ) ;

      //**********************************************
      // TO MERGE LITERAL POOLS AND GENERATE          
      // ONE OBJECT MODULE FOR THIS PROC              
      // -------------------------------------------- 
      //**********************************************


         const XESD = 46523076 ;

               //********************
               //EBCDIC FOR  2|'ESD' 
               //********************

               XTXT = 48490467 ;

               //********************
               //               TXT  
               //********************

               XRLD = 47829956 ;

               //********************
               //               RLD  
               //********************

               XEND = 46519748 ;

               //********************
               //               END  
               //********************

               BLNK1 = 64 ;

               //********************
               // EBCDIC FOR ' '     
               //********************

               BLNK2 = 16448 ;
               BLNK3 = 4210752 ;
               BLNK4 = 1077952576 ;

         var I , J , K : INTEGER ;
             TPC , QPC : INTEGER ;
             LNGTH : STRLRNG ;
             BLNK80 : array [ 1 .. 80 ] of CHAR ;
             BLNK64 : array [ 1 .. 64 ] of CHAR ;
             CODESIZE : INTEGER ;
             CARD : record
                      case INTEGER of
                        1 :
                          ( C : array [ 1 .. 80 ] of CHAR ) ;

             //***************
             //CHAR CARD IMAGE
             //***************

                        2 :
                          ( I : array [ 1 .. 20 ] of INTEGER ) ;

             //***************
             //INT. CARD IMAGE
             //***************

                        3 :
                          ( H : array [ 1 .. 40 ] of HINTEGER )

             //*****************
             //HALFWORD IMAGE   
             //*****************

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
                REST_ZEILZAHL : INTEGER ;
                DUMMYINT : INTEGER ;
                LIMIT : INTEGER ;

            begin (* PRINT_CSECT *)

              //********************************************************
              // schreiben tables of offsets and statement numbers      
              //********************************************************

              REST_ZEILZAHL := LIST002_HEADLINE ( 'R' , ' ' , ' ' , ' '
                               ) ;
              if REST_ZEILZAHL <= 10 then
                begin
                  DUMMYINT := LIST002_HEADLINE ( 'S' , ' ' , ' ' , ' '
                              ) ;
                end (* then *)
              else
                LIST002_NEWLINE ;
              WRITE ( LIST002 ,
                      ' TABLES OF OFFSETS AND STATEMENT NUMBERS FOR ' ,
                      PIAKT -> . CURPNAME , ' (' , PRCTBL [ 0 ] . NAME
                      , ')' ) ;
              LIST002_NEWLINE ;
              LIST002_NEWLINE ;
              K := 1 ;
              while TRUE do
                begin
                  LIMIT := TOS_COUNT ;
                  if LIMIT - K + 1 > 12 then
                    LIMIT := K + 11 ;
                  WRITE ( LIST002 , ' STMT =' ) ;
                  for I := K to LIMIT do
                    WRITE ( LIST002 , ' ' , TOS [ I ] . STMT : 5 ) ;
                  LIST002_NEWLINE ;
                  WRITE ( LIST002 , ' OFFS =' ) ;
                  for I := K to LIMIT do
                    begin
                      HEXHW ( TOS [ I ] . OFFS * 2 , HEXPC ) ;
                      WRITE ( LIST002 , HEXPC : 6 ) ;
                    end (* for *) ;
                  LIST002_NEWLINE ;
                  K := LIMIT + 1 ;
                  if K > TOS_COUNT then
                    break ;
                  REST_ZEILZAHL := LIST002_HEADLINE ( 'R' , ' ' , ' ' ,
                                   ' ' ) ;
                  if REST_ZEILZAHL <= 4 then
                    begin
                      DUMMYINT := LIST002_HEADLINE ( 'S' , ' ' , ' ' ,
                                  ' ' ) ;
                    end (* then *)
                  else
                    LIST002_NEWLINE ;
                end (* while *) ;

              //******************************************
              // schreiben object code in hex             
              //******************************************

              if ASM then
                begin
                  REST_ZEILZAHL := LIST002_HEADLINE ( 'R' , ' ' , ' ' ,
                                   ' ' ) ;
                  if REST_ZEILZAHL <= 10 then
                    begin
                      DUMMYINT := LIST002_HEADLINE ( 'S' , ' ' , ' ' ,
                                  ' ' ) ;
                    end (* then *)
                  else
                    LIST002_NEWLINE ;
                  WRITE ( LIST002 , ' OBJECT CODE FOR CSECT' , PRCTBL [
                          0 ] . NAME : 9 , '(PROCEDURE ' : 13 , PIAKT
                          -> . CURPNAME , ')' ) ;
                  LIST002_NEWLINE ;
                  APC := 0 ;
                  APC1 := 0 ;
                  repeat
                    LIST002_NEWLINE ;
                    HEXHW ( 2 * APC , LPC ) ;
                    WRITE ( LIST002 , LPC : 5 , ':' ) ;
                    for I := 0 to 7 do

              //*******************
              // 32 BYTES PER LINE 
              //*******************

                      begin
                        if I = 4 then
                          WRITE ( LIST002 , ' ' ) ;
                        HEXHW ( CODE . H [ APC1 ] , CON1 ) ;
                        HEXHW ( CODE . H [ APC1 + 1 ] , CON2 ) ;
                        WRITE ( LIST002 , ' ' , CON1 , CON2 ) ;
                        APC := APC + 2 ;
                        APC1 := APC1 + 2 ;
                        if APC1 >= LPC1 then
                          goto 10
                      end (* for *) ;
                  until FALSE ;
                end (* then *) ;

              //******************************************
              // schreiben external references usw.       
              //******************************************

              10 :
              K := 0 ;
              if ( NXTPRC > 1 ) or ( NXTEP < PRCCNT ) then
                begin
                  LIST002_NEWLINE ;
                  REST_ZEILZAHL := LIST002_HEADLINE ( 'R' , ' ' , ' ' ,
                                   ' ' ) ;
                  if REST_ZEILZAHL <= 10 then
                    begin
                      DUMMYINT := LIST002_HEADLINE ( 'S' , ' ' , ' ' ,
                                  ' ' ) ;
                    end (* then *)
                  else
                    LIST002_NEWLINE ;
                  WRITE ( LIST002 ,
                         ' EXTERNAL REFERENCES AND LABEL DEFINITIONS:'
                          ) ;
                  LIST002_NEWLINE ;
                  for I := 0 to PRCCNT do
                    if ( I < NXTPRC ) or ( I > NXTEP ) then
                      with PRCTBL [ I ] do
                        if LNK > 0 then
                          begin
                            if ( K MOD 3 ) = 0 then
                              LIST002_NEWLINE ;
                            K := K + 1 ;
                            HEXHW ( LNK * 2 , CON1 ) ;
                            WRITE ( LIST002 , CON1 : 5 , ':' , NAME : 9
                                    ) ;
                            if I < NXTPRC then
                              WRITE ( LIST002 , ' (ER);    ' )
                            else
                              WRITE ( LIST002 , ' (LD);    ' ) ;
                          end (* then *) ;
                  LIST002_NEWLINE ;
                end (* then *)
              else
                LIST002_NEWLINE ;

              //******************************************
              // schreiben debug informationen            
              //******************************************

              if PIAKT -> . DEBUG_LEV > 0 then
                begin
                  REST_ZEILZAHL := LIST002_HEADLINE ( 'R' , ' ' , ' ' ,
                                   ' ' ) ;
                  if REST_ZEILZAHL <= 10 then
                    begin
                      DUMMYINT := LIST002_HEADLINE ( 'S' , ' ' , ' ' ,
                                  ' ' ) ;
                    end (* then *)
                  else
                    LIST002_NEWLINE ;
                  WRITE ( LIST002 , ' DEBUG INFORMATION:' ) ;
                  LIST002_NEWLINE ;
                  LIST002_NEWLINE ;
                  WRITE ( LIST002 , ' DEBUG LEVEL  = ' , PIAKT -> .
                          DEBUG_LEV : 1 ) ;
                  LIST002_NEWLINE ;
                  WRITE ( LIST002 , ' SOURCENAME   = ' , PIAKT -> .
                          SOURCENAME ) ;
                  LIST002_NEWLINE ;
                  WRITE ( LIST002 , ' PROCNAME     = ' , PIAKT -> .
                          CURPNAME ) ;
                  LIST002_NEWLINE ;
                  WRITE ( LIST002 , ' CODESIZE     = ' , CODESIZE : 1 )
                          ;
                  LIST002_NEWLINE ;
                  WRITE ( LIST002 , ' STATIC CSECT = ' , PIAKT -> .
                          STATNAME ) ;
                  LIST002_NEWLINE ;
                  WRITE ( LIST002 , ' STACKSIZE    = ' , STACKSIZE : 1
                          ) ;
                  LIST002_NEWLINE ;
                end (* then *) ;
              LIST002_NEWLINE ;
              DUMMYINT := LIST002_HEADLINE ( 'V' , ' ' , ' ' , ' ' ) ;
              ASM := FALSE ;
            end (* PRINT_CSECT *) ;


         begin (* GEN_CSECT *)
           QPC := LBLTBL [ MINLBL ] . LNK ;
           while QPC > 1 do
             begin
               TPC := CODE . H [ QPC ] ;
               UPD_INTTBL ( QPC , STACKSIZE ) ;
               QPC := TPC ;
             end (* while *) ;
           MANAGE_LITERALS ( 1 , PCOUNTER , NIL , 0 , DUMMYINT ) ;

           //******************************
           // PROCESS EXTERNAL REFERENCES  
           //******************************

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

           //**********************************************
           // SET Proc SIZE FIELD at position posofproclen 
           // for debug purposes                           
           // ... and stacksize                            
           //**********************************************

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

           //*******************
           //SHORT PROC TOO LONG
           //*******************

           if not PIAKT -> . LARGE_PROC then
             if PCOUNTER > 4096 then
               ERROR ( 609 ) ;

           //**************************
           // OUTPUT THE OBJECT CODE   
           //**************************

           for I := 1 to 20 do
             CARD . I [ I ] := BLNK4 ;
           BLNK80 := CARD . C ;
           PACK ( BLNK80 , 1 , BLNK64 ) ;

           //**************************
           // OUTPUT THE 'ESD' ENTRIES 
           //**************************

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

           //********************
           // NAME OF THIS CSECT 
           //********************

                         begin
                           F1 := 0 ;
                           F2 := BLNK1 * SL24 + PCOUNTER * 2 + NXTLNP ;

           //**********
           //CSECT SIZE
           //**********

                         end (* then *)
                       else

           //********************
           // EXTERNAL REFERENCE 
           //********************

                         begin
                           F1 := 2 * SL24 ;
                           F2 := BLNK4 ;
                         end (* else *)
                     else

           //******************
           // LABEL DEFINITION 
           //******************

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

           //**************************
           // OUTPUT THE 'TXT' CARDS   
           //**************************

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

           //**************************
           // OUTPUT THE 'RLD' ENTRIES 
           //**************************

           CARD . C := BLNK80 ;
           CARD . I [ 1 ] := XRLD ;
           I := 0 ;
           LNGTH := 0 ;
           repeat

           //***********************************
           // SCAN OVER ALL EXTERNAL REFERENCES 
           //***********************************

             with PRCTBL [ I ] do
               begin
                 I := I + 1 ;

           //*******************************************
           // I NOW BECOMES ESDID FOR THE CURRENT ENTRY 
           //*******************************************

                 if LNK > 0 then

           //************************
           // IMPLIES RECURSIVE CALL 
           //************************

                   begin
                     CARD . I [ LNGTH + 5 ] := I * SL16 + 01 ;

           //********************
           // 'P#', 'R#' FIELDS  
           //********************

                     CARD . I [ LNGTH + 6 ] := 28 * SL24 + LNK * 2 ;

           //********************
           // ADCON DISPLACEMENT 
           //********************

                     LNGTH := LNGTH + 2 ;
                     if ( LNGTH >= 14 ) or ( I >= NXTPRC ) then

           //*******************
           // OUTPUT THE BUFFER 
           //*******************

                       begin
                         CARD . H [ 6 ] := LNGTH * 4 ;

           //*********************
           // # OF RLD DATA BYTES 
           //*********************

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

           //*******************
           // OUTPUT 'END' CARD 
           //*******************

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
         end (* GEN_CSECT *) ;


      procedure DUMPCONSTBLK ( CLOSE : BOOLEAN ) ;

         var CPC1 , LEN , I , J : HINTEGER ;
             TXTNUM : 0 .. 150 ;

         begin (* DUMPCONSTBLK *)
           if CSEGSTRT = 0 then

           //************
           // FIRST CALL 
           //************

             begin

           //*********************************
           // PUT OUT ESD CARD TO BEGIN CSECT 
           //*********************************

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

           //*****************************
           // LAST CALL, PUT OUT END CARD 
           //*****************************

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
           if OPCODE = PENT then
             begin

           //*********************************************************
           // ON ENTRY TRG1 POINTS TO DATA AREA                       
           // for the called routine                                  
           //*********************************************************

               CURLVL := P ;
               INIT_CSECT ;

           //*******************************
           //INITIALIZE NEW CSECT PARAMETERS
           //*******************************

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
                       WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' )
                               ;
                       WRITE ( LIST002 , '*' , ' ' : 26 ,
                               '-- save display level ' , CURLVL : 1 )
                               ;
                       LIST002_NEWLINE ;
                     end (* then *) ;
                   GENRX ( XL , TRG0 , DISPLAY + 4 * CURLVL , GBR , 0 )
                           ;
                 end (* then *) ;

           //*************************
           // TO SAVE DISPLAY[CURLVL] 
           //*************************

               if ASM then
                 begin
                   HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                   WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
                   WRITE ( LIST002 , '*' , ' ' : 26 ,
                           '-- save registers and chain areas' ) ;
                   LIST002_NEWLINE ;
                 end (* then *) ;
               GENRS ( XSTM , 14 , 12 , 12 , TRG1 ) ;

           //*******************************
           //SAVE OLD DISPLAY[CURLVL] & REGS
           //*******************************

               GENRX ( XST , TRG1 , 8 , LBR , 0 ) ;

           //***************************
           //FORWARD CHAIN OF SAVE AREAS
           //***************************

               GENRX ( XST , LBR , 4 , TRG1 , 0 ) ;

           //**********************************
           //DYNAMIC LINK, ALSO SAVE AREA CHAIN
           //**********************************
           //**************************        
           // SAVE DYNAMIC LINK + REGS         
           //**************************        

               GENRR ( XLR , LBR , TRG1 ) ;

           //***************
           //UPDATE THE 'MP'
           //***************

               if PIAKT -> . CALL_HIGHER then
                 begin
                   if ASM then
                     begin
                       HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                       WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' )
                               ;
                       WRITE ( LIST002 , '*' , ' ' : 26 ,
                               '-- update current display' ) ;
                       LIST002_NEWLINE ;
                     end (* then *) ;
                   GENRX ( XST , LBR , DISPLAY + 4 * CURLVL , GBR , 0 )
                           ;
                 end (* then *) ;

           //**********************
           //UPDATE DISPLAY[CURLVL]
           //**********************

               if ASM then
                 begin
                   HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                   WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
                   WRITE ( LIST002 , '*' , ' ' : 26 ,
                           '-- setup base registers' ) ;
                   LIST002_NEWLINE ;
                 end (* then *) ;
               GENRR ( XLR , PBR1 , JREG ) ;

           //*******************************
           // SET UP PROGRAM BASE REGISTERS 
           //*******************************

               if PIAKT -> . LARGE_PROC then
                 GENLA_LR ( PBR2 , 4092 , PBR1 , 0 ) ;
               if DEBUG or MUSIC then
                 begin
                   if ASM then
                     begin
                       HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                       WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' )
                               ;
                       WRITE ( LIST002 , '*' , ' ' : 26 ,
                               '-- check for enough stack space' ) ;
                       LIST002_NEWLINE ;
                     end (* then *) ;
                   if PIAKT -> . DATA_SIZE < 4096 then
                     GENLA_LR ( TRG1 , PIAKT -> . DATA_SIZE , TRG1 , 0
                                )
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
                             WRITE ( LIST002 , ' ' , ASMTAG , HEXPC ,
                                     ': ' ) ;
                             WRITE ( LIST002 , '*' , ' ' : 26 ,
                                     '-- clear stack/heap area' ) ;
                             LIST002_NEWLINE ;
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
                         GENLA_LR ( TRG14 , FRSTGVAR , GBR , 0 ) ;
                         GENRR ( XSR , TRG15 , TRG14 ) ;
                         GENRR ( XXR , TRG0 , TRG0 ) ;
                         GENLA_LR ( TRG1 , 129 , 0 , 0 ) ;
                         GENRS ( XSLL , TRG1 , 0 , 24 , 0 ) ;
                         GENRR ( XMVCL , TRG14 , TRG0 ) ;
                       end (* then *) ;
                 end (* then *) ;
               CSPACTIVE [ TRG15 ] := FALSE ;
               PROCOFFSET_OLD := 0 ;
             end (* then *)
           else

           //**********************************************
           // pcode = PRET                                 
           //**********************************************

             begin

           //*********************************************
           //RESTORES DISPLAY[CURLVL] AND MP, THEN RETURNS
           //*********************************************

               if DEBUG and ( CURLVL > 1 ) and ( PIAKT -> . DATA_SIZE >
               80 ) then
                 if PIAKT -> . DATA_SIZE < 1500 then
                   begin
                     if ASM then
                       begin
                         HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                         WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': '
                                 ) ;
                         WRITE ( LIST002 , '*' , ' ' : 26 ,
                                 '-- clear stack frame using MVCs' ) ;
                         LIST002_NEWLINE ;
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
                         WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': '
                                 ) ;
                         WRITE ( LIST002 , '*' , ' ' : 26 ,
                                 '-- clear stack frame using MVCL' ) ;
                         LIST002_NEWLINE ;
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

                     GENLA_LR ( TRG0 , LCAFTMST , LBR , 0 ) ;
                     if PIAKT -> . DATA_SIZE < 4096 then
                       GENLA_LR ( TRG1 , PIAKT -> . DATA_SIZE -
                                  LCAFTMST , 0 , 0 )
                     else
                       begin
                         GENRXLAB ( XL , TRG1 , PIAKT -> . SEGSZE , - 1
                                    ) ;
                         GENRXLIT ( XS , TRG1 , LCAFTMST - REALSIZE , 0
                                    ) ;
                       end (* else *) ;
                     GENRR ( XXR , TRG14 , TRG14 ) ;
                     GENLA_LR ( TRG15 , 0x81 , 0 , 0 ) ;
                     GENRX ( XSLL , TRG15 , 24 , 0 , 0 ) ;
                     GENRR ( XMVCL , TRG0 , TRG14 ) ;
                   end (* else *) ;

           //************************************************
           // restore the general registers                  
           //************************************************

               if ASM then
                 begin
                   HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                   WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
                   WRITE ( LIST002 , '*' , ' ' : 26 ,
                           '-- restore registers' ) ;
                   LIST002_NEWLINE ;
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
                       WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' )
                               ;
                       WRITE ( LIST002 , '*' , ' ' : 26 ,
                               '-- restore display level ' , CURLVL : 1
                               ) ;
                       LIST002_NEWLINE ;
                     end (* then *) ;
                   GENRX ( XST , TRG0 , DISPLAY + 4 * CURLVL , GBR , 0
                           ) ;
                 end (* then *) ;
               if DEBUG and ( CURLVL > 1 ) then

           //*********************
           // CLEAR THE SAVE AREA 
           //*********************

                 begin
                   if ASM then
                     begin
                       HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                       WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' )
                               ;
                       WRITE ( LIST002 , '*' , ' ' : 26 ,
                               '-- clear the save area' ) ;
                       LIST002_NEWLINE ;
                     end (* then *) ;
                   I := 80 ;
                   if OPNDTYPE <> PROC then
                     I := 72 ;
                   GENSS ( XMVC , I , 0 , TRG1 , 80 , TRG1 ) ;
                 end (* then *) ;
               if ASM then
                 begin
                   HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                   WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
                   WRITE ( LIST002 , '*' , ' ' : 26 ,
                           '-- branch to return address' ) ;
                   LIST002_NEWLINE ;
                 end (* then *) ;
               if FLOW_TRACE then
                 begin
                   GENRR ( XLR , 0 , RTREG ) ;
                   GENRX ( XBAL , RTREG , TRACER , GBR , 0 ) ;
                   if ASM then
                     begin
                       HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                       WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' )
                               ;
                       WRITE ( LIST002 , ' DC AL2(0)' ) ;
                       LIST002_NEWLINE ;
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

           //*************************************************
           // pdef steht entweder am Ende einer Prozedur      
           // bzw. funktion und gibt deren laenge an          
           // oder am anfang einer branch table,              
           // dann zweimal fuer lower und upper limit         
           //*************************************************

           if GS . MOD2DEFSTEP >= 0 then
             begin

           //*************************************************
           // ende einer prozedur ... hier folgen             
           // ab 2018.03 noch mehrere defs, die weitere       
           // informationen fuer die prozedur enthalten       
           //*************************************************

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

           //*************************************************
           // am anfang einer branch table,                   
           // dann zweimal fuer lower und upper limit         
           //*************************************************

           if LBL1 . LEN > 0 then
             begin
               PDEF_CNT := PDEF_CNT + 1 ;

           //***********************************
           // CTR/CASE EXPRESSION RANGE,        
           // PUT BOUNDS IN 'CONSTANT' TABLE    
           // but not for constants             
           // in new portable branch table      
           //***********************************

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

           //********************************
           // portable branch table beginnt  
           //********************************

                 begin
                   CASE_FLAG := TRUE ;
                   CASE_FLAG_NEW := TRUE ;
                   CASE_DEFAULT := LBLMAP ( LBL1 . NAM ) + 1 ;
                   CASE_OPNDTYPE := OPNDTYPE ;

           //********************************
           // pre-format area of branch-     
           // table with zeroes              
           //********************************

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

           //**************************************
           // portable branch table komplettieren  
           // und pcounter hochsetzen              
           //**************************************

                 begin
                   PCNEU := NEXTPC ( CASE_HIGH - CASE_LOW ) ;

           //**************************************
           // im fall char erst jetzt alle         
           // adresskonstanten anhand von          
           // case_chartable erzeugen - weil erst  
           // jetzt case_low und case_high         
           // festliegen                           
           //**************************************

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

           //**************************************
           // andernfalls war vorher schon alles   
           // klar (case_low lag schon fest,       
           // erste def_konstante) und jetzt sind  
           // nur noch die luecken zu fuellen      
           //**************************************

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

           //*********************************************
           // Konstanten bei neuer portabler Branch Table 
           // als literale ablegen                        
           //*********************************************

                   UPD_INTTBL ( LBLTBL [ CASE_DEFAULT - 3 ] . LNK ,
                                CASE_LOW ) ;
                   if ASM then
                     begin
                       MKLBL ( LBLX , CASE_DEFAULT - 3 ) ;
                       HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                       WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' )
                               ;
                       WRITE ( LIST002 , LBLX . NAM , ' EQU ' ,
                               CASE_LOW : 1 ) ;
                       LIST002_NEWLINE ;
                     end (* then *) ;
                   UPD_INTTBL ( LBLTBL [ CASE_DEFAULT - 2 ] . LNK ,
                                CASE_HIGH ) ;
                   if ASM then
                     begin
                       MKLBL ( LBLX , CASE_DEFAULT - 2 ) ;
                       HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                       WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' )
                               ;
                       WRITE ( LIST002 , LBLX . NAM , ' EQU ' ,
                               CASE_HIGH : 1 ) ;
                       LIST002_NEWLINE ;
                     end (* then *) ;
                   CASE_FLAG_NEW := FALSE ;
                 end (* then *) ;
               PDEF_CNT := 0 ;
               CASE_FLAG := FALSE ;
             end (* then *) ;

           //*********************
           // END OF BRANCH TABLE 
           //*********************

           if ASM then
             begin
               HEXHW ( PCOUNTER * 2 , HEXPC ) ;
               WRITE ( LIST002 , ' ' , ASMTAG , HEXPC , ': ' ) ;
               X1 := 8 ;
               while LBL1 . NAM [ X1 ] = ' ' do
                 X1 := X1 - 1 ;
               if X1 < 5 then
                 X1 := 5 ;
               for X2 := 1 to X1 do
                 WRITE ( LIST002 , LBL1 . NAM [ X2 ] ) ;
               WRITE ( LIST002 , ' DS    0H' ) ;
               LIST002_NEWLINE ;
             end (* then *) ;

           //****************
           //LABEL DEFINITION
           //****************

           UPD_LBLTBL ( PCOUNTER , LBLMAP ( LBL1 . NAM ) , TRUE , FALSE
                        ) ;

           //****************************************
           // if old opcode = PDEF and pdef_cnt = 2, 
           // start of branch table                  
           //****************************************

           if FALSE then
             begin
               WRITELN ( TRACEF , 'oldopcode     = ' , OLDOPCODE ) ;
               WRITELN ( TRACEF , 'pdef_cnt      = ' , PDEF_CNT ) ;
             end (* then *) ;
           CASE_FLAG := ( OLDOPCODE = PDEF ) and ( PDEF_CNT = 2 ) ;

           //********************
           // some inits         
           //********************

           CSPACTIVE [ TRG15 ] := FALSE ;
           PROCOFFSET_OLD := 0 ;
           TXR_CONTENTS . VALID := FALSE ;
           FILADR_LOADED := FALSE ;
           LAST_CC . LAST_PC := 0 ;
           LAST_STR . LAST_PC := 0 ;
           LAST_FILE . LAST_PC := 0 ;
           LAST_MVC . LAST_PC := 0 ;
           if CKMODE then
             CHECKFREEREGS ;
         end (* LAB_OPERATION *) ;


      begin (* COPERATION *)
        case OPCODE of

        //**********************
        // P_MACHINE PSEUDO OPS 
        //**********************

          PXLB : begin
                   GENRELRX ( XBC , ANYCND , 14 ) ;

        //******************************
        // B *+28, SKIP OVER ENTRY CODE 
        //******************************

                   with PRCTBL [ NXTEP ] do
                     begin
                       NAME := LBL1 . NAM ;
                       LNK := PCOUNTER
                     end (* with *) ;
                   if NXTEP > NXTPRC then
                     NXTEP := NXTEP - 1
                   else
                     ERROR ( 256 ) ;

        //************************
        // COLLISION OF TWO LISTS 
        //************************

                   GENRR ( XBALR , RTREG , 0 ) ;

        //**********************************
        // FORCE A BASE REG. FOR NEXT INST. 
        //**********************************

                   GENRX ( XBAL , PBR1 , 6 , RTREG , 0 ) ;
                   CODE . H [ PCOUNTER ] := TO_HINT ( PCOUNTER * 2 ) ;
                   PCOUNTER := NEXTPC ( 1 ) ;
                   GENLA_LR ( PBR1 , 4 , RTREG , 0 ) ;

        //*****************
        // CLEAR HIGH BYTE 
        //*****************

                   GENRX ( XSH , PBR1 , 4 , RTREG , 0 ) ;
                   if PIAKT -> . LARGE_PROC then
                     GENLA_LR ( PBR2 , 4092 , PBR1 , 0 )
                   else
                     GENRX ( XBC , NOCND , 0 , 0 , 0 ) ;
                   GENRX ( XL , LBR , DISPLAY + 4 * CURLVL , GBR , 0 )
                           ;

        //****************************************************
        // PLAB INSTR. IS NEXT ==> NO NEED TO RESET ANY FLAGS 
        //****************************************************

                 end (* tag/ca *) ;
          PLAB : LAB_OPERATION ;
          PLOC : begin

        //**********************************************************
        // new 05.2019:                                             
        // to create tables of offsets and statement numbers        
        //**********************************************************

                   if TOS_COUNT = 0 then
                     begin
                       TOS_COUNT := 1 ;
                       TOS [ TOS_COUNT ] . STMT := Q ;
                       TOS [ TOS_COUNT ] . OFFS := PCOUNTER
                     end (* then *)
                   else
                     begin
                       if TOS [ TOS_COUNT ] . STMT < Q then
                         begin
                           TOS_COUNT := TOS_COUNT + 1 ;
                           TOS [ TOS_COUNT ] . STMT := Q ;
                           TOS [ TOS_COUNT ] . OFFS := PCOUNTER
                         end (* then *) ;
                       if TOS [ TOS_COUNT ] . STMT = Q then
                         if TOS [ TOS_COUNT ] . OFFS > PCOUNTER then
                           TOS [ TOS_COUNT ] . OFFS := PCOUNTER
                     end (* else *) ;

        //*************************************
        // FILL THE ENTRIES OF LINE PTR TABLE  
        //*************************************

                   if GS . FILL_LINEPTR then
                     begin
                       if PIAKT -> . DEBUG_LEV > 0 then
                         for I := LASTLN to Q - 1 do
                           begin
                             UPDLNTBL ( PCOUNTER - LASTPC ) ;
                             LASTPC := PCOUNTER ;
                           end (* for *) ;
                     end (* then *) ;
                   LASTLN := Q ;

        //*************************
        // TO TREAT THIS AS A NOOP 
        //*************************

                   OPCODE := OLDOPCODE ;
                 end (* tag/ca *) ;
          PDEF : DEF_OPERATION ;

        //*****************************
        // BRANCH/CONTROL INSTRUCTIONS 
        //*****************************

          PUJP : begin
                   if FLOW_TRACE and not CASE_FLAG then
                     begin
                       GENRX ( XBAL , RTREG , TRACER , GBR , 0 ) ;
                       if ASM then
                         begin
                           HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                           WRITE ( LIST002 , ' ' , ASMTAG , HEXPC ,
                                   ': ' ) ;
                           WRITE ( LIST002 , ' DC AL2(' , LBL2 . NAM :
                                   LBL2 . LEN , '-' , PRCTBL [ 0 ] .
                                   NAME , ')' ) ;
                           LIST002_NEWLINE ;
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
                   OPCODE := PUJP ;
                 end (* tag/ca *) ;
          PFJP : begin
                   TOP := TOP - 1 ;
                   if ( BRCND >= 0 ) and ( not NEG_CND ) then

        //*********************
        // COND. CODE IS ALIVE 
        //*********************

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

        //******
        // BZ   
        //******

                                 if NEG_CND then
                                   BRCND := 1 ;

        //******
        // BO   
        //******

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

        //********
        //NOT VRBL
        //********

                           if FPA . DSPLMT = 0 then
                             begin
                               BRCND := ANYCND ;
                               OPCODE := PUJP
                             end (* then *)
                           else
                             BRCND := NOCND ;

        //*************
        //DO NOT BRANCH
        //*************

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

        //***************
        // BC BRCND,*+10 
        //***************

                         GENRX ( XBAL , RTREG , TRACER , GBR , 0 ) ;
                         if ASM then
                           begin
                             HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                             WRITE ( LIST002 , ' ' , ASMTAG , HEXPC ,
                                     ': ' ) ;
                             WRITE ( LIST002 , ' DC AL2(' , LBL2 . NAM
                                     : LBL2 . LEN , '-' , PRCTBL [ 0 ]
                                     . NAME , ')' ) ;
                             LIST002_NEWLINE ;
                           end (* then *) ;
                         UPD_LBLTBL ( PCOUNTER , LBLMAP ( LBL2 . NAM )
                                      , FALSE , TRUE ) ;
                         PCOUNTER := NEXTPC ( 1 ) ;
                       end (* then *)
                     else
                       GENRXLAB ( XBC , BRCND , LBL2 , 0 ) ;

        //**************************
        // CLEAR C.C./ NEGATE FLAGS 
        //**************************

                   BRCND := - 1 ;
                   NEG_CND := FALSE ;
                 end (* tag/ca *) ;
          PXJP :

        //*********************************************
        // LBL2   = LOWER BOUND, CASE EXPRESSION       
        // LBL2+1 = UPPER BOUND,                       
        // LBL2+2 = BRANCH TABLE LABEL                 
        // LBL2+3 = CASE EXIT LABEL                    
        //*********************************************

                 begin
                   TOP := TOP - 1 ;
                   LOAD ( STK [ TOP ] ) ;
                   with STK [ TOP ] do
                     begin
                       Q := LBLMAP ( LBL2 . NAM ) ;

        //**************************************
        // new xjp = xjp without def constants  
        // for high and low values              
        // ------------------------------------ 
        // pascal1 has left 2 labels unused     
        // in this case to be used by code      
        // generators for their own fields      
        // to store the min and max values      
        // determined during branch table scan  
        //**************************************

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

        //*********
        // BH *+18 
        //*********

                           GENRXLAB ( XS , RGADR , LBL2 , - 1 ) ;
                           GENRELRX ( XBC , LESCND , 5 ) ;

        //*********
        // BM *+10 
        //*********

                           GENRR ( XAR , RGADR , RGADR ) ;
                           MKLBL ( LBL_WORK , Q + 2 ) ;
                           GENRXLAB ( XLH , JREG , LBL_WORK , RGADR ) ;
                           GENRELRX ( XSTH , JREG , 4 ) ;

        //**************
        // STH JREG,*+8 
        //**************

                           GENRX ( XBAL , RTREG , TRACER , GBR , 0 ) ;
                           CODE . H [ PCOUNTER ] := 0 ;
                           PCOUNTER := NEXTPC ( 1 ) ;
                         end (* then *)
                       else
                         begin
                           GENRXLAB ( XC , RGADR , LBL1 , - 1 ) ;

        //***************************
        // CHECK AGAINST UPPER BOUND 
        //***************************

                           GENRXLAB ( XBC , GRTCND , LBL_WORK , 0 ) ;

        //***************************
        // GO TO EXIT IF OUT OF RANGE
        //***************************

                           GENRXLAB ( XS , RGADR , LBL2 , - 1 ) ;

        //***************************
        // ELSE SUBTRACT LOWER BOUND 
        //***************************

                           GENRXLAB ( XBC , LESCND , LBL_WORK , 0 ) ;

        //***************************
        // CASE_EXIT IF OUT OF RANGE 
        //***************************

                           MKLBL ( LBL_WORK , Q + 2 ) ;
                           GENRR ( XAR , RGADR , RGADR ) ;

        //*****************************
        // CONV. INDEX TO TABLE OFFSET 
        //*****************************

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
          PMST : if CALL_DEPTH < MAX_CALL_DEPTH then
                   begin
                     CALL_DEPTH := CALL_DEPTH + 1 ;
                     if FALSE then
                       WRITELN ( TRACEF , 'call_depth ++ ' , CALL_DEPTH
                                 , ' linecnt = ' , LINECNT : 1 ) ;
                     with CALL_MST_STACK [ CALL_DEPTH ] do
                       begin
                         PFLEV := P ;
                         DISPSAV := Q
                       end (* with *) ;
                   end (* then *)
                 else
                   begin
                     WRITELN ( 'call_depth = ' , CALL_DEPTH ) ;
                     ERROR ( 759 ) ;
                   end (* else *) ;
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
                                        GENLA_LR ( NXTRG , FNCRSLT ,
                                                   TRG1 , 0 ) ;
                                        PLEN := - 1 ;
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
                   CSPACTIVE [ TRG15 ] := FALSE ;
                   CSPACTIVE [ TRG1 ] := FALSE ;
                 end (* tag/ca *) ;
          PENT , PRET :
            begin
              if OPCODE = PENT then
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

        //**********************************************
        // BEGINNING OF A CSECT OF STRUCTURED CONSTANTS 
        //**********************************************

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

        //******************************************
        // A SIMPLE CONSTANT IN THE CONSTANTS CSECT 
        //******************************************

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

        //******************************
        // store length of static csect 
        // at addr of static csect + 8  
        //******************************

                     CODE . H [ 4 ] := TO_HINT ( CPCOUNTER ) ;
                     if CPCOUNTER > 16 then
                       DUMPCONSTBLK ( TRUE ) ;
                     TOTALBYTES := TOTALBYTES + CPCOUNTER ;
                     CSTBLK := FALSE ;
                   end (* then *) ;
          PSTP : if ASM then
                   begin

        //*****************************
        // GENERATE ASSEMBLER END CARD 
        //*****************************

                     WRITE ( LIST002 , ' ' , '## ' , ' ' : SPACEASMX ,
                             'EXTRN $PASENT' ) ;
                     LIST002_NEWLINE ;
                     WRITE ( LIST002 , ' ' , '## ' , ' ' : SPACEASMX ,
                             'END   $PASENT' ) ;
                     LIST002_NEWLINE ;
                   end (* then *) ;
        end (* case *) ;
      end (* COPERATION *) ;


   procedure UOPERATION ;

   //******************
   // UNARY OPERATIONS 
   //******************


      begin (* UOPERATION *)
        case OPCODE of
          PFLT , PFLO :
            begin
              if OPCODE = PFLT then
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

        //*********
        // CONSTANT
        //*********

                  begin
                    DTYPE := REEL ;
                    RCNST := FPA . DSPLMT ;
                    FPA . DSPLMT := 0 ;
                  end (* else *)
            end (* tag/ca *) ;
          PNGR : with STK [ TOP - 1 ] do
                   if VRBL then
                     begin
                       LOAD ( STK [ TOP - 1 ] ) ;
                       GENRR ( XLCDR , RGADR , RGADR )
                     end (* then *)
                   else

        //**********
        // CONSTANT 
        //**********

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
                     GENLA_LR ( 0 , 127 , 0 , 0 ) ;
                     GENRR ( XNR , NXTRG , 0 ) ;
                     GENLA_LR ( 0 , 64 , 0 , 0 ) ;
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

        //*******************
        // CLEAR NEGATE FLAG 
        //*******************

                           begin
                             NEG_CND := FALSE ;
                             BRCND := - 1 ;
                           end (* then *)
                         else
                           BRCND := 15 - BRCND
                       else

        //*************************
        // NEGATING A BOOLEAN VLUE 
        //*************************

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

        //*********************************
        // RIGHT MOST BYTE IS BEING TESTED 
        //*********************************

                           BRCND := Q ;

        //***********
        // BO OR BNO 
        //***********

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
          PINC : with STK [ TOP - 1 ] do
                   begin
                     if PROCNAME <> ' ' then
                       begin
                         if FALSE then
                           begin
                             WRITELN ( TRACEF ,
                             '---------------------------------------'
                                       ) ;
                             WRITELN ( TRACEF , 'INC at linecnt = ' ,
                                       LINECNT : 1 ) ;
                             WRITELN ( TRACEF , 'procname       =   ' ,
                                       PROCNAME ) ;
                             WRITELN ( TRACEF , 'q              =   ' ,
                                       Q ) ;
                             DUMPSTKELEM ( 'Top - 1' , STK [ TOP - 1 ]
                                           ) ;
                           end (* then *) ;
                         if not DRCT then
                           begin
                             LOAD ( STK [ TOP - 1 ] ) ;
                           end (* then *) ;
                         GENRXLIT ( XA , RGADR , Q , 0 ) ;

        //*****************************************
        // procname cannot be deleted here         
        // because procedure filesetup does some   
        // register mangling when procname is set  
        //*****************************************

                       end (* then *)
                     else
                       begin
                         if not DRCT then
                           begin
                             LOAD ( STK [ TOP - 1 ] ) ;
                           end (* then *) ;
                         FPA . DSPLMT := FPA . DSPLMT + Q ;
                       end (* else *)
                   end (* with *) ;
          PDEC : with STK [ TOP - 1 ] do
                   begin
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

        //**************************
        // MUST ALIGN TO DOUBLEWORD 
        //**************************

                     GENRXLIT ( XN , TRG0 , - 8 , 0 ) ;
                   GENRX ( XST , TRG0 , NEWPTR , GBR , 0 ) ;
                   if not STK [ TOP ] . DRCT then
                     LOAD ( STK [ TOP ] ) ;
                   GETADR ( STK [ TOP ] , Q1 , P1 , B1 ) ;
                   GENRX ( XST , TRG0 , Q1 , B1 , P1 ) ;
                   FREEREG ( STK [ TOP ] ) ;
                   if DEBUG or MUSIC then

        //***********************************
        // CHECK FOR STACK-HEAP INTERFERENCE 
        //***********************************

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

        //*******************************
        // SEE IF NEW HEAP POINTER VALID 
        //*******************************

                         begin
                           if RGADR <> 2 then
                             begin
                               if not AVAIL [ 2 ] then
                                 ERROR ( 760 ) ;
                               GENRR ( XLR , 2 , RGADR ) ;
                             end (* then *) ;
                           GENRX ( XBAL , RTREG , PTRCHK , GBR , 0 ) ;
                         end (* then *) ;

        //********************************************************
        // CODE FOR CLEARING THE RELEASE HEAP AREA SHOULD GO HERE 
        // SEE RETURN SEQUENCE 'PRET' AS AN EXAMPLE.              
        //********************************************************

                       GENRX ( XST , RGADR , NEWPTR , GBR , 0 ) ;
                       AVAIL [ RGADR ] := TRUE ;
                     end (* with *) ;
                 end (* tag/ca *) ;
          PCTS : begin

        //**********************************
        // SET/INITIALIZE RUN TIME COUNTERS 
        //**********************************

                   GENRXLAB ( XL , 2 , LBL2 , - 1 ) ;
                   CSP := PCTR ;
                   GOTOCSP ;
                 end (* tag/ca *) ;
          PCTI : begin

        //**************************************
        // INCREMENT THE COUNT OF COUNTER # 'Q' 
        //**************************************

                   GENRX ( XL , TRG1 , HEAPLMT , GBR , 0 ) ;
                   GENLA_LR ( TRG14 , 1 , 0 , 0 ) ;
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

        //*********************
        // LOAD SOURCE ADDRESS 
        //*********************

        LOAD ( R ) ;

        //**************************
        // LOAD DESTINATION ADDRESS 
        //**************************

        if P = 1 then
          GENRR ( XSR , TRG0 , TRG0 ) ;

        //*******************
        //FOR BYTE INSERTIONS
        //*******************

        if IVAL <= 0 then
          begin
            ERROR ( 619 ) ;
            IVAL := 1
          end (* then *) ;
        FINDRG ;

        //*************************
        // REGISTER FOR LOOP COUNT 
        //*************************

        GENRXLIT ( XL , NXTRG , IVAL , 0 ) ;
        GENRR ( XBALR , TRG1 , 0 ) ;
        CSPACTIVE [ TRG1 ] := FALSE ;
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
        GENLA_LR ( L . RGADR , P , 0 , L . RGADR ) ;
        GENLA_LR ( R . RGADR , Q , 0 , R . RGADR ) ;
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
          Q1 , QX : ADRRNG ;

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
                  WRITELN ( TRACEF , '--- vor getop_simple ---' ) ;
                  WRITELN ( TRACEF , 'linecnt    = ' , LINECNT ) ;
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
              GENLA_LR ( NXTRG , Q1 , B1 , P1 ) ;
              P1 := NXTRG ;
              B1 := NXTRG + 1 ;
              FINDRP ;
              P2 := NXTRG ;
              B2 := NXTRG + 1 ;

        //******************************************************
        // const length operand                                 
        // generate LA                                          
        //******************************************************

              GENLA_LR ( B1 , LEN , 0 , 0 ) ;

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
              GENLA_LR ( NXTRG , Q1 , B1 , P1 ) ;
              P1 := NXTRG ;
              B1 := NXTRG + 1 ;
              FINDRP ;
              P2 := NXTRG ;
              B2 := NXTRG + 1 ;

        //******************************************************
        // const length operand                                 
        // generate LA                                          
        //******************************************************

              GENLA_LR ( B1 , LEN , 0 , 0 ) ;

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

        CSPACTIVE [ TRG1 ] := FALSE ; // INDICATES LOSS OF TRG1

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
            WRITELN ( TRACEF , '--- vor getop_simple ---' ) ;
            WRITELN ( TRACEF , 'linecnt    = ' , LINECNT ) ;
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
        GENLA_LR ( NXTRG , Q1 , B1 , P1 ) ;
        P1 := NXTRG ;
        B1 := NXTRG + 1 ;
        FINDRP ;
        GENLA_LR ( NXTRG , Q2 , B2 , P2 ) ;
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
          begin
            WRITE ( LIST002 , ' ' , '## ' , ' ' : SPACEASMX ,
                    'BNP   @NOMV' ) ;
            LIST002_NEWLINE ;
          end (* then *) ;

        //******************************************************
        // generate MVCL instruction                            
        //******************************************************

        GENRR ( XMVCL , P1 , P2 ) ;

        //******************************************************
        // generate label after MVCL                            
        //******************************************************

        CODE . H [ BPC - 1 ] := TO_HINT ( BASE_DSPLMT ( PCOUNTER ) ) ;
        if ASM then
          begin
            WRITE ( LIST002 , ' ' , '## ' , ' ' : SPACEASML ,
                    '@NOMV  DS    0H' ) ;
            LIST002_NEWLINE ;
          end (* then *) ;
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
          Q1 , QX : ADRRNG ;
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
            WRITELN ( TRACEF , '--- vor getop_simple ---' ) ;
            WRITELN ( TRACEF , 'linecnt    = ' , LINECNT ) ;
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
            WRITELN ( TRACEF , '--- vor getop_simple ---' ) ;
            WRITELN ( TRACEF , 'linecnt    = ' , LINECNT ) ;
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
        GENLA_LR ( NXTRG , Q1 , B1 , P1 ) ;
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
          begin
            WRITE ( LIST002 , ' ' , '## ' , ' ' : SPACEASMX ,
                    'BNP   @NOMV' ) ;
            LIST002_NEWLINE ;
          end (* then *) ;

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
          begin
            WRITE ( LIST002 , ' ' , '## ' , ' ' : SPACEASML ,
                    '@NOMV  DS    0H' ) ;
            LIST002_NEWLINE ;
          end (* then *) ;
        AVAIL [ P1 ] := TRUE ;
        AVAIL [ B1 ] := TRUE ;
        AVAIL [ P2 ] := TRUE ;
        AVAIL [ B2 ] := TRUE ;
        S370CNT := S370CNT + 1 ;
        FREEREG ( L ) ;
        FREEREG ( XPAT ) ;
        FREEREG ( XLEN ) ;
      end (* MSEOPERATION *) ;


   procedure MCVOPERATION ( var L , R , LEN : DATUM ) ;

   //****************************************************************
   // generate CLCL instruction for MEMCMP                           
   // length is not known at compile time                            
   //****************************************************************


      var P1 , B1 , P2 , B2 , PX , BX : LVLRNG ;
          Q1 , Q2 , QX : ADRRNG ;
          TARGET_REG : RGRNG ;

      begin (* MCVOPERATION *)

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

        CSPACTIVE [ TRG1 ] := FALSE ; // INDICATES LOSS OF TRG1

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
            WRITELN ( TRACEF , '--- vor getop_simple ---' ) ;
            WRITELN ( TRACEF , 'linecnt    = ' , LINECNT ) ;
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
        GENLA_LR ( NXTRG , Q1 , B1 , P1 ) ;
        P1 := NXTRG ;
        B1 := NXTRG + 1 ;
        FINDRP ;
        GENLA_LR ( NXTRG , Q2 , B2 , P2 ) ;
        P2 := NXTRG ;
        B2 := NXTRG + 1 ;

        //******************************************************
        // find number of target register and                   
        // generate code to set target register to zero         
        // before doing the comparison                          
        //******************************************************

        FINDRG ;
        TARGET_REG := NXTRG ;
        GENRR ( XXR , TARGET_REG , TARGET_REG ) ;

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

        GENRELRX ( XBC , LEQCND , 10 ) ;

        //******************************************************
        // generate CLCL instruction                            
        //******************************************************

        GENRR ( XCLCL , P1 , P2 ) ;

        //******************************************************
        // set result in target rec depending on CC             
        //******************************************************

        GENRELRX ( XBC , EQUCND , 7 ) ;
        GENRR ( XBCTR , TARGET_REG , 0 ) ;
        GENRELRX ( XBC , LESCND , 4 ) ;
        GENLA_LR ( TARGET_REG , 1 , 0 , 0 ) ;

        //******************************************************
        // free temporary registers                             
        //******************************************************

        AVAIL [ TARGET_REG ] := TRUE ;
        AVAIL [ P1 ] := TRUE ;
        AVAIL [ B1 ] := TRUE ;
        AVAIL [ P2 ] := TRUE ;
        AVAIL [ B2 ] := TRUE ;
        S370CNT := S370CNT + 1 ;
        FREEREG ( L ) ;
        FREEREG ( R ) ;
        FREEREG ( LEN ) ;

        //******************************************************
        // result is in top stack element (register)            
        //******************************************************

        with L do
          begin
            DTYPE := INT ;
            PLEN := 0 ;
            VRBL := TRUE ;
            DRCT := TRUE ;
            VPA := RGS ;
            RGADR := TARGET_REG ;
            FPA . LVL := 0 ;
            FPA . DSPLMT := 0 ;
            MEMADR . LVL := 0 ;
            MEMADR . DSPLMT := 0 ;
          end (* with *) ;
      end (* MCVOPERATION *) ;


   procedure MCCOPERATION ( var L , R : DATUM ; LEN : INTEGER ) ;

   //****************************************************************
   // generate CLC or CLCL instruction for MEMCMP                    
   // length is known at compile time                                
   //****************************************************************


      var P1 , B1 , P2 , B2 : LVLRNG ;
          Q1 , Q2 : ADRRNG ;
          TARGET_REG : RGRNG ;

      begin (* MCCOPERATION *)

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

        CSPACTIVE [ TRG1 ] := FALSE ; // INDICATES LOSS OF TRG1

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
        GENLA_LR ( NXTRG , Q1 , B1 , P1 ) ;
        P1 := NXTRG ;
        B1 := NXTRG + 1 ;
        FINDRP ;
        GENLA_LR ( NXTRG , Q2 , B2 , P2 ) ;
        P2 := NXTRG ;
        B2 := NXTRG + 1 ;

        //******************************************************
        // find number of target register and                   
        // generate code to set target register to zero         
        // before doing the comparison                          
        //******************************************************

        FINDRG ;
        TARGET_REG := NXTRG ;
        GENRR ( XXR , TARGET_REG , TARGET_REG ) ;

        //******************************************************
        // don't generate compare instructions,                 
        // if length is less or equal to zero                   
        // generate CLC, if length <= 256                       
        // otherwise CLCL                                       
        //******************************************************

        if LEN > 0 then
          if LEN <= 256 then
            begin
              GENSS ( XCLC , LEN , 0 , P1 , 0 , P2 ) ;
            end (* then *)
          else
            begin

        //******************************************************
        // copy length in register b1 to register b2            
        // and test                                             
        //******************************************************

              GENLA_LR ( B1 , LEN , 0 , 0 ) ;
              GENRR ( XLR , B2 , B1 ) ;

        //******************************************************
        // generate CLCL instruction                            
        //******************************************************

              GENRR ( XCLCL , P1 , P2 ) ;
            end (* else *) ;

        //******************************************************
        // set result in target rec depending on CC             
        //******************************************************

        GENRELRX ( XBC , EQUCND , 7 ) ;
        GENRR ( XBCTR , TARGET_REG , 0 ) ;
        GENRELRX ( XBC , LESCND , 4 ) ;
        GENLA_LR ( TARGET_REG , 1 , 0 , 0 ) ;

        //******************************************************
        // free temporary registers                             
        //******************************************************

        AVAIL [ TARGET_REG ] := TRUE ;
        AVAIL [ P1 ] := TRUE ;
        AVAIL [ B1 ] := TRUE ;
        AVAIL [ P2 ] := TRUE ;
        AVAIL [ B2 ] := TRUE ;
        S370CNT := S370CNT + 1 ;
        FREEREG ( L ) ;
        FREEREG ( R ) ;

        //******************************************************
        // result is in top stack element (register)            
        //******************************************************

        with L do
          begin
            DTYPE := INT ;
            PLEN := 0 ;
            VRBL := TRUE ;
            DRCT := TRUE ;
            VPA := RGS ;
            RGADR := TARGET_REG ;
            FPA . LVL := 0 ;
            FPA . DSPLMT := 0 ;
            MEMADR . LVL := 0 ;
            MEMADR . DSPLMT := 0 ;
          end (* with *) ;
      end (* MCCOPERATION *) ;


   procedure STROPERATION_MVI ( var LEFT : DATUM ; CCONST : CHAR ) ;

   //****************************************************************
   // MVI to string target / implement VST after VC1                 
   //****************************************************************


      var P1 , B1 : LVLRNG ;
          Q1 : ADRRNG ;

      begin (* STROPERATION_MVI *)

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
        CONS_REGS ( B1 , P1 ) ;
        GENSI ( XMVI , Q1 , B1 , ORD ( CCONST ) ) ;
      end (* STROPERATION_MVI *) ;


   procedure STROPERATION_MVC1 ( var TARGET : DATUM ; var SOURCE :
                               DATUM ) ;

   //****************************************************************
   // MVC to string target / implement VST after VC1                 
   //****************************************************************


      var P1 , B1 : LVLRNG ;
          Q1 : ADRRNG ;

      begin (* STROPERATION_MVC1 *)
        if FALSE then
          begin
            WRITELN ( TRACEF , 'start STROPERATION_MVC1, linecnt = ' ,
                      LINECNT : 1 ) ;
            DUMPSTKELEM ( 'Target' , TARGET ) ;
            DUMPSTKELEM ( 'Source' , SOURCE ) ;
          end (* then *) ;

        //******************************************************
        // get address of left operand                          
        //******************************************************

        GETADR2 ( TARGET , Q1 , P1 , B1 ) ;
        if not TARGET . DRCT then
          begin
            GENRX ( XL , TXRG , Q1 , B1 , P1 ) ;
            Q1 := 0 ;
            B1 := 0 ;
            P1 := TXRG ;
          end (* then *) ;
        CONS_REGS ( B1 , P1 ) ;
        GEN_MOVECHAR ( Q1 , B1 , SOURCE ) ;
      end (* STROPERATION_MVC1 *) ;


   procedure COMPARE_CARR ( var LEFT , RIGHT : DATUM ; LENL , LENR :
                          INTEGER ; COMPTYPE : CHAR ) ;

   //****************************************************************
   // generate code FOR CHAR ARRAY COMPARE OPERATION (not string!)   
   //****************************************************************
   // 2020.12: two lenghts, lengths may be different and zero        
   // 2020.12: comptype may be M, 1 and 2                            
   // 2020.12: if M, both sides are char arrays                      
   // 2020.12: if 1, left side is single char, not char pointer      
   // 2020.12: if 2, right side is single char, not char pointer     
   //****************************************************************


      var P1 , B1 , P2 , B2 : LVLRNG ;
          Q1 , Q2 : ADRRNG ;
          GEN_LIT_LINKS , GEN_LIT_RECHTS : BOOLEAN ;

      begin (* COMPARE_CARR *)
        if FALSE then
          begin
            WRITELN ( TRACEF , 'start compare_carr, linecnt = ' ,
                      LINECNT : 1 ) ;
            DUMPSTKELEM ( 'Left ' , LEFT ) ;
            DUMPSTKELEM ( 'Right' , RIGHT ) ;
            WRITELN ( TRACEF , 'lenl      = ' , LENL ) ;
            WRITELN ( TRACEF , 'lenr      = ' , LENR ) ;
            WRITELN ( TRACEF , 'comptype  = ' , COMPTYPE ) ;
          end (* then *) ;

        //******************************************************
        // if comptype = '1' then                               
        // left operand is single char                          
        // if memory operand, change attributes of stack element
        // else (constant) generate MVI to scratchpad area      
        // and set attributes of stack element to address there 
        //******************************************************

        if COMPTYPE = '1' then
          begin
            COMPTYPE := 'M' ;
            if LEFT . VPA = MEM then
              begin
                LEFT . DTYPE := ADR ;
                LEFT . FPA := LEFT . MEMADR ;
                LEFT . VPA := NEITHER ;
                LEFT . VRBL := FALSE ;
              end (* then *)
            else
              begin
                GENSI ( XMVI , PIAKT -> . SCRATCHPOS , LBR , LEFT . FPA
                        . DSPLMT ) ;
                P1 := LBR ;
                Q1 := PIAKT -> . SCRATCHPOS ;
                LEFT . DTYPE := ADR ;
                LEFT . FPA . LVL := CURLVL ;
                LEFT . FPA . DSPLMT := PIAKT -> . SCRATCHPOS ;
              end (* else *) ;
            if FALSE then
              begin
                WRITELN ( TRACEF , 'nach Modifikation:' ) ;
                DUMPSTKELEM ( 'Left ' , LEFT ) ;
              end (* then *)
          end (* then *) ;

        //******************************************************
        // if comptype = '2' then                               
        // do the same for the right operand                    
        //******************************************************

        if COMPTYPE = '2' then
          begin
            COMPTYPE := 'M' ;
            if RIGHT . VPA = MEM then
              begin
                RIGHT . DTYPE := ADR ;
                RIGHT . FPA := RIGHT . MEMADR ;
                RIGHT . VPA := NEITHER ;
                RIGHT . VRBL := FALSE ;
              end (* then *)
            else
              begin
                GENSI ( XMVI , PIAKT -> . SCRATCHPOS , LBR , RIGHT .
                        FPA . DSPLMT ) ;
                P2 := LBR ;
                Q2 := PIAKT -> . SCRATCHPOS ;
                RIGHT . DTYPE := ADR ;
                RIGHT . FPA . LVL := CURLVL ;
                RIGHT . FPA . DSPLMT := PIAKT -> . SCRATCHPOS ;
              end (* else *) ;
            if FALSE then
              begin
                WRITELN ( TRACEF , 'nach Modifikation:' ) ;
                DUMPSTKELEM ( 'Right' , RIGHT ) ;
              end (* then *)
          end (* then *) ;

        //******************************************************
        // get address of left operand                          
        //******************************************************

        if LENL > 0 then
          begin
            GETADR2 ( LEFT , Q1 , P1 , B1 ) ;
            if FALSE then
              WRITELN ( TRACEF , 'getadr left, q1, p1, b1   = ' , Q1 :
                        6 , P1 : 6 , B1 : 6 ) ;
            if not LEFT . DRCT then
              begin
                GENRX ( XL , TXRG , Q1 , B1 , P1 ) ;
                Q1 := 0 ;
                B1 := 0 ;
                P1 := TXRG ;
                if FALSE then
                  WRITELN ( TRACEF , 'load txrg, q1, p1, b1     = ' ,
                            Q1 : 6 , P1 : 6 , B1 : 6 ) ;
              end (* then *) ;
            TXRG := TRG1 ;

        //******************************************************
        // TO AVOID REASSIGNM. OF THE SAME BASE REG             
        //******************************************************

            CSPACTIVE [ TRG1 ] := FALSE ; // INDICATES LOSS OF TRG1
          end (* then *) ;

        //******************************************************
        // get address of right operand                         
        //******************************************************

        if LENR > 0 then
          begin
            GETADR2 ( RIGHT , Q2 , P2 , B2 ) ;
            if FALSE then
              WRITELN ( TRACEF , 'getadr right, q2, p2, b2  = ' , Q2 :
                        6 , P2 : 6 , B2 : 6 ) ;
            if not RIGHT . DRCT then
              begin
                GENRX ( XL , TXRG , Q2 , B2 , P2 ) ;
                Q2 := 0 ;
                B2 := 0 ;
                P2 := TXRG ;
                if FALSE then
                  WRITELN ( TRACEF , 'load txrg, q2, p2, b2     = ' ,
                            Q2 : 6 , P2 : 6 , B2 : 6 ) ;
              end (* then *) ;
            TXRG := TRG14 ;
          end (* then *) ;

        //******************************************************
        // RESTORE THE OLD MIDLEVEL BASE REG                    
        //******************************************************
        // if p1 or p2 < 0 (that is: literal)                   
        // move px to bx and set px to zero                     
        //******************************************************

        if LENL > 0 then
          if P1 < 0 then
            begin
              B1 := P1 ;
              P1 := 0 ;
            end (* then *) ;
        if LENR > 0 then
          if P2 < 0 then
            begin
              B2 := P2 ;
              P2 := 0 ;
            end (* then *) ;
        if FALSE then
          begin
            WRITELN ( TRACEF , 'after restore, q1, p1, b1 = ' , Q1 : 6
                      , P1 : 6 , B1 : 6 ) ;
            WRITELN ( TRACEF , 'after restore, q2, p2, b2 = ' , Q2 : 6
                      , P2 : 6 , B2 : 6 ) ;
          end (* then *) ;

        //******************************************************
        // SHORT MOVE = length (q) <= 256                       
        // 2020.12: if lenl <> lenr, use CLCL                   
        //******************************************************
        // if px and bx both greater zero                       
        // generate AR instructions for px                      
        // bx is no longer needed                               
        // otherwise if bx less than zero                       
        // generate literals                                    
        //******************************************************

        if ( LENL > 0 ) and ( LENL <= 256 ) and ( LENL = LENR ) then
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
            GENSS ( XCLC , LENL , Q1 , P1 , Q2 , P2 ) ;
            if B1 < 0 then
              begin
                if FALSE then
                  begin
                    WRITELN ( TRACEF , 'sop lit links, b1    = ' , B1 )
                              ;
                    WRITELN ( TRACEF , 'sop lit links, index = ' , LEFT
                              . SCNSTNO ) ;
                    WRITELN ( TRACEF , 'sop cod links, pcnt  = ' ,
                              PCOUNTER - 2 ) ;
                    WRITELN ( TRACEF , 'sop cod links, code  = ' ,
                              TO_HINT ( Q1 ) )
                  end (* then *) ;
                if B1 = - 1 then
                  begin
                    if TRACE_LITERAL then
                      WRITELN ( TRACEF , 'repl lit. adr. 3 - index = '
                                , LEFT . SCNSTNO : 1 , ' pc = ' ,
                                PCOUNTER - 2 : 1 ) ;
                    LITTBL [ LEFT . SCNSTNO ] . LNK := PCOUNTER - 2 ;
                  end (* then *) ;
                CODE . H [ PCOUNTER - 2 ] := TO_HINT ( Q1 ) ;
              end (* then *) ;
            if B2 < 0 then
              begin
                if FALSE then
                  begin
                    WRITELN ( TRACEF , 'sop lit rechts, b2    = ' , B2
                              ) ;
                    WRITELN ( TRACEF , 'sop lit rechts, index = ' ,
                              RIGHT . SCNSTNO ) ;
                    WRITELN ( TRACEF , 'sop cod rechts, pcnt  = ' ,
                              PCOUNTER - 1 ) ;
                    WRITELN ( TRACEF , 'sop cod rechts, code  = ' ,
                              TO_HINT ( Q2 ) )
                  end (* then *) ;
                if B2 = - 1 then
                  begin
                    if TRACE_LITERAL then
                      WRITELN ( TRACEF , 'repl lit. adr. 4 - index = '
                                , RIGHT . SCNSTNO : 1 , ' pc = ' ,
                                PCOUNTER - 1 : 1 ) ;
                    LITTBL [ RIGHT . SCNSTNO ] . LNK := PCOUNTER - 1 ;
                  end (* then *) ;
                CODE . H [ PCOUNTER - 1 ] := TO_HINT ( Q2 ) ;
              end (* then *) ;
          end (* then *)

        //**************************************************
        // THIS IS ONLY VALID FOR THE 370,                  
        // FOR THE 360 THE 'CLCL' INSTRUCTION SHOULD BE     
        // REPLACED BY AN APPROPRIATE NUMBER OF 'CLC'S      
        //**************************************************

        else
          begin
            if FALSE then
              begin
                WRITELN ( TRACEF , 'start gen CLCL' ) ;
                WRITELN ( TRACEF , 'lenl, b1 = ' , LENL , B1 ) ;
                WRITELN ( TRACEF , 'lenr, b2 = ' , LENR , B2 ) ;
              end (* then *) ;

        //**************************************
        // check if literals must be generated  
        //**************************************

            GEN_LIT_LINKS := FALSE ;
            if LENL > 0 then
              if B1 < 0 then
                GEN_LIT_LINKS := TRUE ;
            GEN_LIT_RECHTS := FALSE ;
            if LENR > 0 then
              if B2 < 0 then
                GEN_LIT_RECHTS := TRUE ;
            FINDRP ;
            if LENL > 0 then
              begin
                if FALSE then
                  WRITELN ( TRACEF , 'gen la - nxtrg = ' , NXTRG : 1 ,
                            ' ' , Q1 : 1 , ' ' , B1 : 1 , ' ' , P1 : 1
                            ) ;
                if GEN_LIT_LINKS then
                  begin
                    GENLA_LR ( NXTRG , Q1 , 0 , P1 ) ;
                    if B1 = - 1 then
                      begin
                        if TRACE_LITERAL then
                          WRITELN ( TRACEF ,
                                    'repl lit. adr. 5 - index = ' ,
                                    LEFT . SCNSTNO : 1 , ' pc = ' ,
                                    PCOUNTER - 1 : 1 ) ;
                        LITTBL [ LEFT . SCNSTNO ] . LNK := PCOUNTER - 1
                      end (* then *) ;
                    CODE . H [ PCOUNTER - 1 ] := TO_HINT ( Q1 ) ;
                  end (* then *)
                else
                  GENLA_LR ( NXTRG , Q1 , B1 , P1 ) ;
              end (* then *) ;
            P1 := NXTRG ;
            B1 := NXTRG + 1 ;
            FINDRP ;
            if LENR > 0 then
              begin
                if GEN_LIT_RECHTS then
                  begin
                    GENLA_LR ( NXTRG , Q2 , 0 , P2 ) ;
                    if B2 = - 1 then
                      begin
                        if TRACE_LITERAL then
                          WRITELN ( TRACEF ,
                                    'repl lit. adr. 6 - index = ' ,
                                    RIGHT . SCNSTNO : 1 , ' pc = ' ,
                                    PCOUNTER - 1 : 1 ) ;
                        LITTBL [ RIGHT . SCNSTNO ] . LNK := PCOUNTER -
                                                   1
                      end (* then *) ;
                    CODE . H [ PCOUNTER - 1 ] := TO_HINT ( Q2 ) ;
                  end (* then *)
                else
                  GENLA_LR ( NXTRG , Q2 , B2 , P2 ) ;
              end (* then *) ;
            P2 := NXTRG ;
            B2 := NXTRG + 1 ;
            if LENR = LENL then
              begin
                GENRXLIT ( XL , B1 , LENL , 0 ) ;
                GENRR ( XLR , B2 , B1 )
              end (* then *)
            else
              begin
                GENRXLIT ( XL , B1 , LENL , 0 ) ;
                GENRXLIT ( XL , B2 , 0x40 * SL24 , 0 ) ;
                GENRXLIT ( XA , B2 , LENR , 0 ) ;
              end (* else *) ;
            GENRR ( XCLCL , P1 , P2 ) ;
            AVAIL [ P1 ] := TRUE ;
            AVAIL [ B1 ] := TRUE ;
            AVAIL [ P2 ] := TRUE ;
            AVAIL [ B2 ] := TRUE ;
            S370CNT := S370CNT + 1 ;
            if FALSE then
              WRITELN ( TRACEF , 'end gen CLCL' ) ;
          end (* else *) ;
        FREEREG ( LEFT ) ;
        FREEREG ( RIGHT ) ;
      end (* COMPARE_CARR *) ;


   procedure SOPERATION ( var LEFT , RIGHT : DATUM ; PCODEPARM : OPTYPE
                        ; LENPARM : INTEGER ) ;

   //****************************************************************
   // SET UP FOR STRING MOVE OPERATIONS                              
   //****************************************************************


      var P1 , B1 , P2 , B2 : LVLRNG ;
          Q1 , Q2 : ADRRNG ;
          XOPC : BYTE ;

      begin (* SOPERATION *)
        if PCODEPARM <> PMOV then
          begin
            WRITELN ( 'soperation: pcodeparm = ' , PCODEPARM ) ;
            ERROR ( 720 )
          end (* then *) ;

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

        CSPACTIVE [ TRG1 ] := FALSE ; // INDICATES LOSS OF TRG1

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
                if FALSE then
                  begin
                    WRITELN ( 'sop lit links, b1    = ' , B1 ) ;
                    WRITELN ( 'sop lit links, index = ' , LEFT .
                              SCNSTNO ) ;
                    WRITELN ( 'sop cod links, pcnt  = ' , PCOUNTER - 2
                              ) ;
                    WRITELN ( 'sop cod links, code  = ' , TO_HINT ( Q1
                              ) )
                  end (* then *) ;
                if B1 = - 1 then
                  begin
                    if TRACE_LITERAL then
                      WRITELN ( TRACEF , 'repl lit. adr. 7 - index = '
                                , LEFT . SCNSTNO : 1 , ' pc = ' ,
                                PCOUNTER - 2 : 1 ) ;
                    LITTBL [ LEFT . SCNSTNO ] . LNK := PCOUNTER - 2 ;
                  end (* then *) ;
                CODE . H [ PCOUNTER - 2 ] := TO_HINT ( Q1 ) ;
              end (* then *) ;
            if B2 < 0 then
              begin
                if FALSE then
                  begin
                    WRITELN ( 'sop lit rechts, b2    = ' , B2 ) ;
                    WRITELN ( 'sop lit rechts, index = ' , RIGHT .
                              SCNSTNO ) ;
                    WRITELN ( 'sop cod rechts, pcnt  = ' , PCOUNTER - 1
                              ) ;
                    WRITELN ( 'sop cod rechts, code  = ' , TO_HINT ( Q2
                              ) )
                  end (* then *) ;
                if B2 = - 1 then
                  begin
                    if TRACE_LITERAL then
                      WRITELN ( TRACEF , 'repl lit. adr. 8 - index = '
                                , RIGHT . SCNSTNO : 1 , ' pc = ' ,
                                PCOUNTER - 1 : 1 ) ;
                    LITTBL [ RIGHT . SCNSTNO ] . LNK := PCOUNTER - 1 ;
                  end (* then *) ;
                CODE . H [ PCOUNTER - 1 ] := TO_HINT ( Q2 ) ;
              end (* then *) ;
            if OPT_FLG then
              if XOPC = XMVC then
                with LAST_MVC do
                  begin
                    if PCOUNTER = ( LAST_PC + 3 ) then

        //******************************************************
        // CONSECUTIVE MVC INSTS                                
        // check if new MVC can be eliminated by changing       
        // length of previous MVC                               
        //******************************************************

                      if ( CODE . H [ LAST_PC - 2 ] + LLEN ) = CODE . H
                      [ PCOUNTER - 2 ] then
                        if ( CODE . H [ LAST_PC - 1 ] + LLEN ) = CODE .
                        H [ PCOUNTER - 1 ] then
                          if ( LLEN + LENPARM ) <= 256 then
                            begin
                              if ASM then
                                begin
                                  HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                                  WRITE ( LIST002 , ' ' , ASMTAG ,
                                          HEXPC , ': ' ) ;
                                  WRITE ( LIST002 , '*' , ' ' : 26 ,
                                          '-- instruction is not' ) ;
                                  LIST002_NEWLINE ;
                                  HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                                  WRITE ( LIST002 , ' ' , ASMTAG ,
                                          HEXPC , ': ' ) ;
                                  WRITE ( LIST002 , '*' , ' ' : 26 ,
                                          '-- generated; length' ) ;
                                  LIST002_NEWLINE ;
                                  HEXHW ( PCOUNTER * 2 , HEXPC ) ;
                                  WRITE ( LIST002 , ' ' , ASMTAG ,
                                          HEXPC , ': ' ) ;
                                  WRITE ( LIST002 , '*' , ' ' : 26 ,
                                          '-- of prev. instr. changed'
                                          ) ;
                                  LIST002_NEWLINE ;
                                end (* then *) ;
                              CODE . H [ LAST_PC - 3 ] := TO_HINT (
                                                   CODE . H [ LAST_PC -
                                                   3 ] + LENPARM ) ;
                              LENPARM := LENPARM + LLEN ;
                              PCOUNTER := LAST_PC ;
                              if B2 = - 1 then
                                if RIGHT . SCNSTNO = NXTLIT - 1 then
                                  NXTLIT := NXTLIT - 1
                                else
                                  begin
                                    if TRACE_LITERAL then
                                      WRITELN ( TRACEF ,
                                         'repl lit. adr. 9 - index = '
                                                , RIGHT . SCNSTNO : 1 ,
                                                ' pc = ' , 0 : 1 ) ;
                                    if TRACE_LITERAL then
                                      WRITELN ( TRACEF ,
                                        'repl lit. adr. 10 - index = '
                                                , RIGHT . SCNSTNO : 1 ,
                                                ' optimized ' ) ;
                                    LITTBL [ RIGHT . SCNSTNO ] . LNK :=
                                                   0 ;
                                    LITTBL [ RIGHT . SCNSTNO ] .
                                    OPTIMIZED := TRUE
                                  end (* else *) ;
                            end (* then *) ;
                    LAST_PC := PCOUNTER ;
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
            GENLA_LR ( NXTRG , Q1 , B1 , P1 ) ;
            P1 := NXTRG ;
            B1 := NXTRG + 1 ;
            FINDRP ;
            GENLA_LR ( NXTRG , Q2 , B2 , P2 ) ;
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
                              ADRRNG ; STR_ADDRMODE : INTEGER ) ;

   //****************************************************************
   // SET UP FOR STRING MOVE/COMPARE OPERATIONS                      
   // if len_reg < 0, len_reg contains length                        
   // otherwise: len_reg and len_offs contain address of 2 byte      
   // length field                                                   
   //****************************************************************


      var P1 , B1 , P2 , B2 : LVLRNG ;
          Q1 , Q2 : ADRRNG ;
          XOPC : BYTE ;

      begin (* STROPERATION_LEN *)

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

        CSPACTIVE [ TRG1 ] := FALSE ; // INDICATES LOSS OF TRG1

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
        GENLA_LR ( NXTRG , Q1 , P1 , B1 ) ;
        P1 := NXTRG ;
        B1 := NXTRG + 1 ;
        FINDRP ;
        if STR_ADDRMODE > 0 then
          GENLA_LR ( NXTRG , Q2 , P2 , B2 )
        else
          if STR_ADDRMODE < 0 then
            GENRX ( XL , NXTRG , Q2 , P2 , B2 )
          else
            begin
              GENLA_LR ( NXTRG , Q2 , P2 , B2 ) ;
              GENRX ( XLH , NXTRG + 1 , Q2 - 4 , P2 , B2 ) ;
              GENRR ( XLTR , NXTRG + 1 , NXTRG + 1 ) ;
              GENRELRX ( XBC , GEQCND , 4 ) ;
              GENRX ( XL , NXTRG , Q2 , P2 , B2 ) ;
            end (* else *) ;
        P2 := NXTRG ;
        B2 := NXTRG + 1 ;
        if LEN_REG < 0 then
          GENRR ( XLR , B1 , - LEN_REG )
        else
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


   procedure ASSIGN_STRING ( TARGET , SOURCE : DATUM ; LEN_REG : RGRNG
                           ; LEN_OFFS : ADRRNG ; STR_ADDRMODE : INTEGER
                           ; INCR_TARGET : BOOLEAN ; INCR_SOURCE :
                           BOOLEAN ) ;

   //****************************************************************
   // assign_string:                                                 
   // different variants of string assignment                        
   // single characters (s..MVC1, s..MVI)                            
   // string operation controlled by length field (s..len)           
   // constant moves (soperation)                                    
   //****************************************************************


      begin (* ASSIGN_STRING *)
        if FALSE then
          begin
            WRITELN ( TRACEF , 'start assign_string, linecnt = ' ,
                      LINECNT : 1 ) ;
            DUMPSTKELEM ( 'Target' , TARGET ) ;
            DUMPSTKELEM ( 'Source' , SOURCE ) ;
            WRITELN ( TRACEF , 'len_reg  = ' , LEN_REG ) ;
            WRITELN ( TRACEF , 'len_offs = ' , LEN_OFFS ) ;
          end (* then *) ;
        if INCR_TARGET then
          if TARGET . DTYPE = VARC then
            TARGET . FPA . DSPLMT := TARGET . FPA . DSPLMT + 4 ;
        if INCR_SOURCE then
          if SOURCE . DTYPE = VARC then
            SOURCE . FPA . DSPLMT := SOURCE . FPA . DSPLMT + 4 ;
        if SOURCE . DTYPE = CHRC then
          begin
            if SOURCE . VRBL then
              begin
                if FALSE then
                  WRITELN ( TRACEF , 'STROPERATION_MVC1' ) ;
                STROPERATION_MVC1 ( TARGET , SOURCE )
              end (* then *)
            else
              begin
                if FALSE then
                  WRITELN ( TRACEF , 'STROPERATION_MVI' ) ;
                STROPERATION_MVI ( TARGET , CHR ( SOURCE . FPA . DSPLMT
                                   ) ) ;
              end (* else *) ;
            return
          end (* then *) ;
        if LEN_REG <> 0 then
          begin
            if FALSE then
              WRITELN ( TRACEF , 'STROPERATION_LEN' ) ;
            STROPERATION_LEN ( TARGET , SOURCE , PMOV , LEN_REG ,
                               LEN_OFFS , STR_ADDRMODE ) ;
            return ;
          end (* then *) ;
        if FALSE then
          WRITELN ( TRACEF , 'SOPERATION' ) ;
        SOPERATION ( TARGET , SOURCE , PMOV , SOURCE . PLEN ) ;
      end (* ASSIGN_STRING *) ;


   procedure STRING_GET_ACTLEN ( S : DATUM ; NEW_REG : BOOLEAN ; var
                               TARGET_REG : RGRNG ; GEN_ADD : BOOLEAN )
                               ;

      begin (* STRING_GET_ACTLEN *)

        //******************************************************
        // generate instructions to fetch actual length         
        // into target register                                 
        //******************************************************

        if NEW_REG then
          begin
            FINDRG ;
            TARGET_REG := NXTRG
          end (* then *) ;
        if FALSE then
          begin
            WRITELN ( TRACEF , 'start STRING_GET_ACTLEN, linecnt = ' ,
                      LINECNT : 1 ) ;
            DUMPSTKELEM ( 'S = ' , S ) ;
          end (* then *) ;
        with S do
          begin
            GETADR2 ( S , Q2 , P2 , B2 ) ;
            if FALSE then
              begin
                WRITELN ( TRACEF , 'after getadr2' ) ;
                WRITELN ( TRACEF , 'p2      = ' , P2 : 4 ) ;
                WRITELN ( TRACEF , 'q2      = ' , Q2 : 4 ) ;
                WRITELN ( TRACEF , 'b2      = ' , B2 : 4 ) ;
              end (* then *) ;
            if GEN_ADD then
              GENRX ( XAH , TARGET_REG , 2 + Q2 , B2 , P2 )
            else
              GENRX ( XLH , TARGET_REG , 2 + Q2 , B2 , P2 )
          end (* with *)
      end (* STRING_GET_ACTLEN *) ;


   procedure STRINGOPS ;

      var P1 , B1 , P2 , B2 : LVLRNG ;
          Q1 , Q2 : ADRRNG ;
          PX , BX : LVLRNG ;
          QX : ADRRNG ;
          B : LVLRNG ;
          MAXL : INTEGER ;
          LEN : INTEGER ;
          COPYSTRING : BOOLEAN ;
          RGWORK : RGRNG ;
          RGWORK1 : RGRNG ;
          RGWORK2 : RGRNG ;
          PATBLANK : DATUM ;
          LITVALUE : INTEGER ;
          DATWORKAREA : DATUM ;
          LEN1 , LEN2 : INTEGER ;
          LEN_NEW : INTEGER ;
          LEN_REG : RGRNG ;
          LEN_OFFS : ADRRNG ;
          COUNT : INTEGER ;
          NEWLEN : INTEGER ;
          SP1 : INTEGER ;
          SP2 : INTEGER ;


      procedure WORK_VC2_NEW ( var X : DATUM ; Q : ADRRNG ) ;

         begin (* WORK_VC2_NEW *)

           //***********************************************
           // address of char array is on stack             
           // VC2 converts char array to string             
           // of length q                                   
           // q = instruction operand                       
           // set plen of stack item to q                   
           // datatype to varc                              
           //*********************************************  

           with X do
             begin
               PLEN := Q ;
               DTYPE := CARR ;
             end (* with *)
         end (* WORK_VC2_NEW *) ;


      procedure WORK_VC2 ;

         begin (* WORK_VC2 *)
           if Q > 0 then
             WORK_VC2_NEW ( STK [ TOP - 1 ] , Q )
           else
             begin

           //*********************************************
           // 2020.12: compiler generates an instruction  
           // LDC N before VC2 0                          
           //*********************************************

               with STK [ TOP - 1 ] do
                 if ( DTYPE = ADR ) and ( FPA . DSPLMT = - 1 ) then
                   
                 else
                   begin
                     TOP := TOP + 1 ;
                     STK [ TOP - 1 ] := DATNULL ;
                   end (* else *) ;
               WORK_VC2_NEW ( STK [ TOP - 1 ] , 0 )
             end (* else *) ;
         end (* WORK_VC2 *) ;


      procedure WORK_VCC ;

         var LBL : PLABEL ;
             DO_STATICWORK : BOOLEAN ;

         begin (* WORK_VCC *)
           if FALSE then
             begin
               WRITELN ( TRACEF , 'start WORK_VCC, linecnt = ' ,
                         LINECNT : 1 ) ;
               DUMPSTKELEM ( 'Top - 2' , STK [ TOP - 2 ] ) ;
               DUMPSTKELEM ( 'Top - 1' , STK [ TOP - 1 ] )
             end (* then *) ;
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

           //*********************************************
           // if one of the lengths is zero               
           // don't really concatenate                    
           //*********************************************

           if LEN1 = 0 then
             begin

           //*********************************************
           // if LEN2 is known at compile time            
           // build string descriptor for right side      
           // in workarea                                 
           //*********************************************

               if LEN2 > 0 then
                 begin

           //**********************************************
           //   WORK_VC2_NEW ( STK [ TOP - 2 ] , LEN2 ) ;  
           //**********************************************

                   LEN_NEW := LEN2 ;
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
           // copy string into workarea and               
           // store new strcurr pointer                   
           //*********************************************

                   ASSIGN_STRING ( DATWORKAREA , STK [ TOP - 2 ] , 0 ,
                                   0 , 0 , TRUE , TRUE ) ;
                   if LEN_NEW < 4096 then
                     GENLA_LR ( TXRG , LEN_NEW + 4 , TXRG , 0 )
                   else
                     GENRXLIT ( XAH , TXRG , LEN_NEW + 4 , 1 ) ;
                   GENRX ( XST , RGWORK , Q1 , B1 , P1 ) ;
                 end (* then *) ;

           //***********************************************
           // otherwise the existing string descriptor      
           // is the result                                 
           //***********************************************

               FREEREG_COND ( STK [ TOP - 2 ] , RGWORK ) ;
               FREEREG_COND ( STK [ TOP - 1 ] , RGWORK ) ;
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
                 end (* with *) ;
               return
             end (* then *) ;
           if LEN2 = 0 then
             begin

           //*********************************************
           // if LEN1 is known at compile time            
           // build string descriptor for right side      
           // in workarea                                 
           //*********************************************

               if LEN1 > 0 then
                 begin

           //**********************************************
           //   WORK_VC2_NEW ( STK [ TOP - 1 ] , LEN1 ) ;  
           //**********************************************

                   LEN_NEW := LEN1 ;
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
           // copy string into workarea and               
           // store new strcurr pointer                   
           //*********************************************

                   ASSIGN_STRING ( DATWORKAREA , STK [ TOP - 1 ] , 0 ,
                                   0 , 0 , TRUE , TRUE ) ;
                   if LEN_NEW < 4096 then
                     GENLA_LR ( TXRG , LEN_NEW + 4 , TXRG , 0 )
                   else
                     GENRXLIT ( XAH , TXRG , LEN_NEW + 4 , 1 ) ;
                   GENRX ( XST , RGWORK , Q1 , B1 , P1 ) ;
                 end (* then *) ;

           //***********************************************
           // otherwise the existing string descriptor      
           // is the result                                 
           //***********************************************

               FREEREG_COND ( STK [ TOP - 2 ] , RGWORK ) ;
               FREEREG_COND ( STK [ TOP - 1 ] , RGWORK ) ;
               STK [ TOP - 2 ] := STK [ TOP - 1 ] ;
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
                 end (* with *) ;
               return
             end (* then *) ;

           //*********************************************
           // if both lengths are known at compile time   
           // build string descriptor element in          
           // workarea - load strcurr pointer first       
           //*********************************************

           if ( LEN1 > 0 ) and ( LEN2 > 0 ) then
             begin
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

               ASSIGN_STRING ( DATWORKAREA , STK [ TOP - 2 ] , 0 , 0 ,
                               0 , TRUE , TRUE ) ;
               if LEN2 < 4096 then
                 GENLA_LR ( TXRG , LEN2 , TXRG , 0 )
               else
                 GENRXLIT ( XAH , TXRG , LEN2 , 1 ) ;
               ASSIGN_STRING ( DATWORKAREA , STK [ TOP - 1 ] , 0 , 0 ,
                               0 , TRUE , TRUE ) ;
               if LEN1 + 4 < 4096 then
                 GENLA_LR ( TXRG , LEN1 + 4 , TXRG , 0 )
               else
                 GENRXLIT ( XAH , TXRG , LEN1 + 4 , 1 ) ;
               GENRX ( XST , TXRG , Q1 , B1 , P1 ) ;
             end (* then *)

           //*********************************************
           // if one of the lengths is not known          
           // at compile time                             
           // build string descriptor element in          
           // workarea from existent string descriptors   
           //*********************************************

           else
             begin
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

                   STRING_GET_ACTLEN ( STK [ TOP - 2 ] , FALSE , TXRG ,
                                       FALSE ) ;
                   GENRS ( XSLL , TXRG , 0 , 16 , 0 ) ;
                   if LEN1 < 4095 then
                     GENLA_LR ( TXRG , LEN1 , TXRG , 0 )
                   else
                     begin
                       LITVALUE := LEN1 ;
                       GENRXLIT ( XA , TXRG , LITVALUE , 1 )
                     end (* else *) ;
                   GENRX ( XST , TXRG , 0 , RGWORK , 0 ) ;
                   GENLA_LR ( TXRG , 4 , RGWORK , 0 ) ;

           //*********************************************
           // concatenate strings in workarea and         
           // store new strcurr pointer                   
           //*********************************************

                   ASSIGN_STRING ( DATWORKAREA , STK [ TOP - 2 ] ,
                                   RGWORK , 0 , 0 , FALSE , TRUE ) ;
                   GENRX ( XAH , TXRG , 0 , RGWORK , 0 ) ;
                   ASSIGN_STRING ( DATWORKAREA , STK [ TOP - 1 ] , 0 ,
                                   0 , 0 , FALSE , TRUE ) ;
                 end (* then *)
               else
                 if LEN2 > 0 then
                   begin

           //*********************************************
           // length 2 ist known, that is                 
           // the length of the first operand             
           //*********************************************

                     LITVALUE := LEN2 * 65536 ;
                     GENRXLIT ( XL , RGWORK , LITVALUE , 1 ) ;
                     STRING_GET_ACTLEN ( STK [ TOP - 1 ] , FALSE ,
                                         RGWORK , TRUE ) ;
                     GENRR ( XLR , TXRG , RGWORK ) ;
                     GENRX ( XL , RGWORK , Q1 , B1 , P1 ) ;
                     GENRX ( XST , TXRG , 0 , RGWORK , 0 ) ;
                     GENLA_LR ( TXRG , 4 , RGWORK , 0 ) ;

           //*********************************************
           // concatenate strings in workarea and         
           // store new strcurr pointer                   
           //*********************************************

                     ASSIGN_STRING ( DATWORKAREA , STK [ TOP - 2 ] , 0
                                     , 0 , 0 , FALSE , TRUE ) ;
                     GENRX ( XAH , TXRG , 0 , RGWORK , 0 ) ;
                     ASSIGN_STRING ( DATWORKAREA , STK [ TOP - 1 ] ,
                                     RGWORK , 2 , 0 , FALSE , TRUE ) ;
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

           //************************************************
           // again: error solved by using getadr2           
           //************************************************

                         if FALSE then
                           begin
                             WRITELN ( TRACEF , 'VCC - linecnt = ' ,
                                       LINECNT : 1 ) ;
                             DUMPSTKELEM ( 'Top - 2' , STK [ TOP - 2 ]
                                           )
                           end (* then *) ;
                         GETADR2 ( STK [ TOP - 2 ] , Q2 , P2 , B2 ) ;
                         GENLA_LR ( TXRG , Q2 , B2 , P2 ) ;

           //************************************************
           // store adr of first string to                   
           // first parm position                            
           //************************************************

                         GENRX ( XST , TXRG , 0 , TRG1 , 0 ) ;

           //************************************************
           // again: error solved by using getadr2           
           //************************************************

                         if FALSE then
                           begin
                             WRITELN ( TRACEF , 'VCC - linecnt = ' ,
                                       LINECNT : 1 ) ;
                             DUMPSTKELEM ( 'Top - 1' , STK [ TOP - 1 ]
                                           )
                           end (* then *) ;
                         GETADR2 ( STK [ TOP - 1 ] , Q2 , P2 , B2 ) ;
                         GENLA_LR ( TXRG , Q2 , B2 , P2 ) ;

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
           // call the $PASSVCC routine (in PASMONN)               
           //******************************************************

                         LBL . NAM := '$PASSVCC' ;
                         LBL . LEN := 8 ;
                         GENRXLAB ( XL , TRG15 , LBL , - 3 ) ;
                         GENRR ( XBALR , 14 , 15 ) ;

           //******************************************************
           // indicate loss of reg 1 and reg 15                    
           //******************************************************

                         CSPACTIVE [ TRG15 ] := FALSE ;
                         CSPACTIVE [ TRG1 ] := FALSE ;

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
                         STRING_GET_ACTLEN ( STK [ TOP - 2 ] , FALSE ,
                                             RGWORK , FALSE ) ;
                         GENRS ( XSLL , RGWORK , 0 , 16 , 0 ) ;
                         STRING_GET_ACTLEN ( STK [ TOP - 1 ] , FALSE ,
                                             RGWORK , TRUE ) ;
                         GENRR ( XLR , TXRG , RGWORK ) ;
                         GENRX ( XL , RGWORK , Q1 , B1 , P1 ) ;
                         GENRX ( XST , TXRG , 0 , RGWORK , 0 ) ;
                         GENLA_LR ( TXRG , 4 , RGWORK , 0 ) ;

           //*********************************************
           // concatenate strings in workarea and         
           // store new strcurr pointer                   
           //*********************************************

                         ASSIGN_STRING ( DATWORKAREA , STK [ TOP - 2 ]
                                         , RGWORK , 0 , 0 , FALSE ,
                                         TRUE ) ;
                         GENRX ( XAH , TXRG , 0 , RGWORK , 0 ) ;
                         ASSIGN_STRING ( DATWORKAREA , STK [ TOP - 1 ]
                                         , RGWORK , 2 , 0 , FALSE ,
                                         TRUE ) ;
                       end (* else *)
                   end (* else *) ;
               if DO_STATICWORK then
                 begin
                   GENRX ( XAH , TXRG , 2 , RGWORK , 0 ) ;
                   GENLA_LR ( TXRG , 4 , TXRG , 0 ) ;
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

           FREEREG_COND ( STK [ TOP - 2 ] , RGWORK ) ;
           FREEREG_COND ( STK [ TOP - 1 ] , RGWORK ) ;
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
           if FALSE then
             begin
               WRITELN ( TRACEF , 'start WORK_VST, linecnt = ' ,
                         LINECNT : 1 ) ;
               WRITELN ( TRACEF , 'p = ' , P : 4 , ' q = ' , Q : 4 ) ;
               DUMPSTKELEM ( 'Top - 1' , STK [ TOP - 1 ] ) ;
               DUMPSTKELEM ( 'Top - 2' , STK [ TOP - 2 ] ) ;
             end (* then *) ;

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
                 if LEN >= 0 then
                   begin

           //******************************************************
           // in this case len_reg remains at zero,                
           // so assign_string uses the compile time               
           // len to control the transport                         
           //******************************************************

                     LITVALUE := MAXL * 65536 + LEN ;
                     if FALSE then
                       WRITELN ( TRACEF , 'litvalue 1: ' , LITVALUE ) ;
                     GENRXLIT ( XL , NXTRG , LITVALUE , 1 ) ;
                   end (* then *)
                 else
                   begin
                     LITVALUE := MAXL * 65536 ;
                     if FALSE then
                       WRITELN ( TRACEF , 'litvalue 2: ' , LITVALUE ) ;
                     GENRXLIT ( XL , NXTRG , LITVALUE , 1 ) ;
                     STRING_GET_ACTLEN ( STK [ TOP - 1 ] , FALSE ,
                                         NXTRG , TRUE ) ;

           //******************************************************
           // this is done to make assign_string                   
           // fetch the length from the target                     
           // string descriptor                                    
           //******************************************************

                     LEN_REG := B1 ;
                     LEN_OFFS := Q1 + 2 ;
                   end (* else *) ;
                 GENRX ( XST , NXTRG , Q1 , B1 , P1 ) ;
                 AVAIL [ NXTRG ] := TRUE ;

           //*********************************************
           // assign string to stk [top - 2 ]             
           //*********************************************

                 STK [ TOP - 2 ] . DTYPE := VARC ;
                 if LEN <> 0 then
                   ASSIGN_STRING ( STK [ TOP - 2 ] , STK [ TOP - 1 ] ,
                                   LEN_REG , LEN_OFFS , 0 , TRUE , TRUE
                                   ) ;
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
                       if PLEN > 0 then
                         begin
                           LITVALUE := PLEN ;
                           GENRXLIT ( XC , 14 , LITVALUE , 1 ) ;
                           GENRS ( XSLL , 14 , 0 , 16 , 0 ) ;
                           GENRXLIT ( XA , 14 , LITVALUE , 1 ) ;
                           GENRX ( XST , 14 , 0 , NXTRG , 0 ) ;
                           LEN_REG := 0 ;
                           LEN_OFFS := 0 ;
                         end (* then *)
                       else
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
                           GENRX ( XCH , 14 , LEN_OFFS , LEN_REG , 0 )
                                   ;
                           GENRS ( XSLL , 14 , 0 , 16 , 0 ) ;
                           GENRX ( XAH , 14 , LEN_OFFS , LEN_REG , 0 )
                                   ;
                           GENRX ( XST , 14 , 0 , NXTRG , 0 ) ;
                         end (* else *)
                     end (* with *) ;

           //*********************************************
           // assign string to stk [top - 2 ]             
           //*********************************************

                   ASSIGN_STRING ( STK [ TOP - 2 ] , STK [ TOP - 1 ] ,
                                   LEN_REG , LEN_OFFS , 0 , TRUE , TRUE
                                   ) ;
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
                   with STK [ TOP - 1 ] do
                     if PLEN > 0 then
                       begin
                         if FALSE then
                           begin
                             WRITELN ( TRACEF , 'VST - linecnt = ' ,
                                       LINECNT : 1 ) ;
                             WRITELN ( TRACEF , 'VST - p = ' , P ) ;
                             WRITELN ( TRACEF , 'VST - q = ' , Q ) ;
                             DUMPSTKELEM ( 'Top - 1' , STK [ TOP - 1 ]
                                           ) ;
                             WRITELN ( TRACEF , 'scnstno = ' , SCNSTNO
                                       : 1 ) ;
                             WRITELN ( TRACEF , 'plen = ' , PLEN : 1 )
                                       ;
                           end (* then *) ;
                         LITVALUE := - 65536 + PLEN ;
                         GENRXLIT ( XL , 14 , LITVALUE , 1 ) ;
                         GENRX ( XST , 14 , 0 , NXTRG , 0 ) ;

           //*********************************************
           // error fixed 17.05.2019:                     
           // the LA instruction was not generated with   
           // the correct base register. GETADR2 must be  
           // used (this is again the solution).          
           // See the LA below, which uses Q2, P2 and B2; 
           // don't know if the literal logic is correct  
           // here                                        
           //*********************************************

                         GETADR2 ( STK [ TOP - 1 ] , Q2 , P2 , B2 ) ;
                         if FALSE then
                           begin
                             WRITELN ( TRACEF , 'after getadr2' ) ;
                             WRITELN ( TRACEF , 'p2      = ' , P2 : 4 )
                                       ;
                             WRITELN ( TRACEF , 'q2      = ' , Q2 : 4 )
                                       ;
                             WRITELN ( TRACEF , 'b2      = ' , B2 : 4 )
                                       ;
                             WRITELN ( TRACEF , 'scnstno = ' , SCNSTNO
                                       : 4 ) ;
                             WRITELN ( TRACEF , 'plen    = ' , PLEN : 4
                                       ) ;
                           end (* then *) ;
                         if P2 < 0 then
                           begin
                             if SCNSTNO > 0 then
                               begin
                                 if PLEN > 0 then
                                   begin
                                     GENLA_LR ( 14 , Q2 , 0 , 0 ) ;
                                     if TRACE_LITERAL then
                                       WRITELN ( TRACEF ,
                                        'repl lit. adr. 11 - index = '
                                                 , SCNSTNO : 1 ,
                                                 ' pc = ' , PCOUNTER -
                                                 1 : 1 ) ;
                                     LITTBL [ SCNSTNO ] . LNK :=
                                                   PCOUNTER - 1 ;
                                     CODE . H [ PCOUNTER - 1 ] :=
                                                   TO_HINT ( Q2 ) ;
                                   end (* then *)
                                 else
                                   begin
                                     GENRR ( XXR , 14 , 14 ) ;
                                     GENRR ( XBCTR , 14 , 0 ) ;
                                   end (* else *) ;
                               end (* then *)
                           end (* then *)
                         else
                           GENLA_LR ( 14 , Q2 + 4 , B2 , P2 ) ;
                         GENRX ( XST , 14 , 4 , NXTRG , 0 ) ;
                       end (* then *)
                     else
                       if PLEN = 0 then
                         begin
                           LITVALUE := - 65536 ;
                           GENRXLIT ( XL , 14 , LITVALUE , 1 ) ;
                           GENRX ( XST , 14 , 0 , NXTRG , 0 ) ;
                           LITVALUE := - 1 ;
                           GENRXLIT ( XL , 14 , LITVALUE , 1 ) ;
                           GENRX ( XST , 14 , 4 , NXTRG , 0 ) ;
                         end (* then *)
                       else
                         begin
                           if FALSE then
                             begin
                               WRITELN ( TRACEF , 'VST - linecnt = ' ,
                                         LINECNT : 1 ) ;
                               WRITELN ( TRACEF , 'VST - p = ' , P ) ;
                               WRITELN ( TRACEF , 'VST - q = ' , Q ) ;
                               DUMPSTKELEM ( 'Top - 1' , STK [ TOP - 1
                                             ] ) ;
                             end (* then *) ;
                           LITVALUE := - 65536 ;
                           GENRXLIT ( XL , 14 , LITVALUE , 1 ) ;
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
                           if FALSE then
                             begin
                               WRITELN ( TRACEF , 'VPA = ' , VPA ) ;
                               WRITELN ( TRACEF , 'len_reg = ' ,
                                         LEN_REG ) ;
                             end (* then *) ;
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
                               GENLA_LR ( NXTRG , LEN_OFFS + 4 ,
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
                               GENRX ( XLH , 14 , LEN_OFFS , LEN_REG ,
                                       0 ) ;
                               GENRR ( XLTR , 14 , 14 ) ;
                               GENRELRX ( XBC , GEQCND , 6 ) ;
                               GENRX ( XL , 14 , LEN_OFFS + 4 , LEN_REG
                                       , 0 ) ;
                               GENRELRX ( XBC , ANYCND , 4 ) ;
                               GENLA_LR ( 14 , LEN_OFFS + 4 , LEN_REG ,
                                          0 ) ;
                               GENRX ( XST , 14 , 4 , NXTRG , 0 ) ;
                             end (* else *)
                         end (* else *) ;
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
                 if LEN >= 0 then
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
                     STRING_GET_ACTLEN ( STK [ TOP - 2 ] , FALSE ,
                                         NXTRG , TRUE ) ;

           //******************************************************
           // this is done to make assign_string                   
           // fetch the length from the target                     
           // string descriptor                                    
           //******************************************************

                     LEN_REG := B1 ;
                     LEN_OFFS := Q1 + 2 ;
                   end (* else *) ;
                 GENRX ( XST , NXTRG , Q1 , B1 , P1 ) ;
                 AVAIL [ NXTRG ] := TRUE ;

           //*********************************************
           // assign string to stk [top - 2 ]             
           //*********************************************

                 STK [ TOP - 1 ] . DTYPE := VARC ;
                 if LEN <> 0 then
                   ASSIGN_STRING ( STK [ TOP - 1 ] , STK [ TOP - 2 ] ,
                                   LEN_REG , LEN_OFFS , 0 , TRUE , TRUE
                                   ) ;
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
           // (can be VC2 char constant, too)             
           // two items popped, the target is pushed      
           //*********************************************

                   with STK [ TOP - 2 ] do
                     begin
                       if FALSE then
                         begin
                           WRITELN ( TRACEF , 'VST - linecnt = ' ,
                                     LINECNT : 1 ) ;
                           WRITELN ( TRACEF , 'VST - p = ' , P ) ;
                           WRITELN ( TRACEF , 'VST - q = ' , Q ) ;
                           DUMPSTKELEM ( 'Top - 2' , STK [ TOP - 2 ] )
                                         ;
                           WRITELN ( TRACEF , 'scnstno = ' , SCNSTNO :
                                     1 ) ;
                           WRITELN ( TRACEF , 'plen = ' , PLEN : 1 ) ;
                         end (* then *) ;
                       if PLEN = 1 then
                         begin
                           GETADR2 ( STK [ TOP - 1 ] , Q1 , P1 , B1 ) ;
                           LITVALUE := PLEN * 65536 + PLEN ;
                           GENRXLIT ( XL , 14 , LITVALUE , 1 ) ;
                           FINDRG ;
                           GENLA_LR ( NXTRG , Q1 , B1 , P1 ) ;
                           GENRX ( XST , 14 , 0 , NXTRG , 0 ) ;
                           GETADR2 ( STK [ TOP - 2 ] , Q2 , P2 , B2 ) ;
                           GENLA_LR ( 14 , Q2 , 0 , 0 ) ;
                           GENRX ( XSTC , 14 , 4 , NXTRG , 0 ) ;
                         end (* then *)
                       else
                         if PLEN >= 0 then
                           begin
                             GETADR2 ( STK [ TOP - 1 ] , Q1 , P1 , B1 )
                                       ;
                             LITVALUE := - 65536 ;
                             GENRXLIT ( XL , 14 , LITVALUE , 1 ) ;
                             LITVALUE := PLEN ;
                             if PLEN > 0 then
                               GENRXLIT ( XA , 14 , LITVALUE , 1 ) ;
                             FINDRG ;
                             GENLA_LR ( NXTRG , Q1 , B1 , P1 ) ;
                             GENRX ( XST , 14 , 0 , NXTRG , 0 ) ;

           //*********************************************
           // error fixed 17.05.2019:                     
           // the LA instruction was not generated with   
           // the correct base register. GETADR2 must be  
           // used (this is again the solution).          
           // See the LA below, which uses Q2, P2 and B2; 
           // don't know if the literal logic is correct  
           // here                                        
           //*********************************************

                             GETADR2 ( STK [ TOP - 2 ] , Q2 , P2 , B2 )
                                       ;
                             if FALSE then
                               begin
                                 WRITELN ( TRACEF , 'after getadr2 '
                                           'for STK-2' ) ;
                                 WRITELN ( TRACEF , 'p2      = ' , P2 :
                                           4 ) ;
                                 WRITELN ( TRACEF , 'q2      = ' , Q2 :
                                           4 ) ;
                                 WRITELN ( TRACEF , 'b2      = ' , B2 :
                                           4 ) ;
                                 WRITELN ( TRACEF , 'scnstno = ' ,
                                           SCNSTNO : 4 ) ;
                                 WRITELN ( TRACEF , 'plen    = ' , PLEN
                                           : 4 ) ;
                               end (* then *) ;
                             if P2 < 0 then
                               begin
                                 if SCNSTNO > 0 then
                                   begin
                                     if PLEN > 0 then
                                       begin
                                         GENLA_LR ( 14 , Q2 , 0 , 0 ) ;
                                         if TRACE_LITERAL then
                                           WRITELN ( TRACEF ,
                                        'repl lit. adr. 12 - index = '
                                                   , SCNSTNO : 1 ,
                                                   ' pc = ' , PCOUNTER
                                                   - 1 : 1 ) ;
                                         LITTBL [ SCNSTNO ] . LNK :=
                                                   PCOUNTER - 1 ;
                                         CODE . H [ PCOUNTER - 1 ] :=
                                                   TO_HINT ( Q2 ) ;
                                         if TRACE_LITERAL then
                                           WRITELN ( TRACEF ,
                                                   'set literal ' , Q2
                                                   : 1 ,
                                                   ' at position ' ,
                                                   PCOUNTER - 1 : 1 ) ;
                                       end (* then *)
                                     else
                                       begin
                                         GENRR ( XXR , 14 , 14 ) ;
                                         GENRR ( XBCTR , 14 , 0 ) ;
                                       end (* else *) ;
                                   end (* then *)
                               end (* then *)

           //*********************************************
           // error fixed 26.05.2019:                     
           // offset must be 4 in case of VARC, but       
           // zero in case of CARR (no length fields)     
           //*********************************************

                             else
                               if DTYPE = CARR then
                                 GENLA_LR ( 14 , Q2 , B2 , P2 )
                               else
                                 GENLA_LR ( 14 , Q2 + 4 , B2 , P2 ) ;
                             GENRX ( XST , 14 , 4 , NXTRG , 0 ) ;
                           end (* then *)
                         else
                           begin
                             GETADR2 ( STK [ TOP - 1 ] , Q1 , P1 , B1 )
                                       ;
                             LITVALUE := - 65536 ;
                             GENRXLIT ( XL , 14 , LITVALUE , 1 ) ;

           //************************************************
           // maybe wrong                                    
           //************************************************

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
                             FINDRG ;
                             GENLA_LR ( NXTRG , Q1 , B1 , P1 ) ;
                             GENRX ( XST , 14 , 0 , NXTRG , 0 ) ;
                             GENRX ( XLH , 14 , LEN_OFFS - 2 , LEN_REG
                                     , 0 ) ;
                             GENRR ( XLTR , 14 , 14 ) ;
                             GENRELRX ( XBC , GEQCND , 6 ) ;
                             GENRX ( XL , 14 , LEN_OFFS + 2 , LEN_REG ,
                                     0 ) ;
                             GENRELRX ( XBC , ANYCND , 4 ) ;
                             GENLA_LR ( 14 , LEN_OFFS + 2 , LEN_REG , 0
                                        ) ;
                             GENRX ( XST , 14 , 4 , NXTRG , 0 ) ;
                           end (* else *)
                     end (* with *) ;
                   FREEREG_COND ( STK [ TOP - 2 ] , - 1 ) ;
                   STK [ TOP - 2 ] := STK [ TOP - 1 ] ;
                   TOP := TOP - 1 ;
                   with STK [ TOP - 1 ] do
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
                     end (* with *)
                 end (* else *)
         end (* WORK_VST *) ;


      begin (* STRINGOPS *)
        if FALSE then
          begin
            WRITE ( TRACEF , 'start stringops - pcode = ' , OPCODE ) ;
            WRITELN ( TRACEF , ' linecnt = ' , LINECNT : 1 ) ;
            WRITELN ( TRACEF , 'start stringops - p = ' , P ) ;
            WRITELN ( TRACEF , 'start stringops - q = ' , Q ) ;
            DUMPSTK ( 1 , TOP - 1 ) ;
          end (* then *) ;
        if FALSE then
          begin
            WRITE ( TRACEF , 'start stringops - pcode = ' , OPCODE ) ;
            WRITELN ( TRACEF , ' linecnt = ' , LINECNT : 1 ) ;
            WRITELN ( TRACEF , 'start stringops - p = ' , P ) ;
            WRITELN ( TRACEF , 'start stringops - q = ' , Q ) ;
            DUMPAVAIL ;
          end (* then *) ;
        case OPCODE of

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
        // if q = zero: build null string on stack               
        //*******************************************************

          PVC2 : WORK_VC2 ;

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
                             if FALSE then
                               begin
                                 WRITELN ( TRACEF , 'VLD - linecnt = '
                                           , LINECNT : 1 ) ;
                                 WRITELN ( TRACEF , 'VLD - p = ' , P )
                                           ;
                                 WRITELN ( TRACEF , 'VLD - q = ' , Q )
                                           ;
                                 DUMPSTKELEM ( 'Top - 1' , STK [ TOP -
                                               1 ] )
                               end (* then *) ;
                             FINDRG ;

        //*********************************************
        // error fixed 17.05.2019:                     
        // wrong code generation with array elements   
        // GETADR2 must be used                        
        // (this is again the solution).               
        // the original code made a difference         
        // depending on VRBL ... don't know if this    
        // is correct                                  
        //*********************************************

                             GETADR2 ( STK [ TOP - 1 ] , Q2 , P2 , B2 )
                                       ;
                             if not CHECK_ZERO_REG ( STK [ TOP - 1 ] ,
                             Q2 , P2 , B2 ) then
                               begin
                                 GENLA_LR ( NXTRG , Q2 , B2 , P2 ) ;
                                 FREEREG ( STK [ TOP - 1 ] ) ;
                                 FPA := ZEROBL ;
                                 VPA := RGS ;
                                 MEMADR := ZEROBL ;
                                 RGADR := NXTRG ;
                                 VRBL := TRUE ;
                               end (* then *)
                             else
                               begin

        //******************************************************
        // work done by check_zero_reg                          
        //******************************************************

                                 
                               end (* else *) ;
                             if FALSE then
                               begin
                                 WRITELN ( TRACEF , 'VLD - linecnt = '
                                           , LINECNT : 1 ) ;
                                 WRITELN ( TRACEF , 'VLD - p = ' , P )
                                           ;
                                 WRITELN ( TRACEF , 'VLD - q = ' , Q )
                                           ;
                                 DUMPSTKELEM ( 'Top - 1' , STK [ TOP -
                                               1 ] )
                               end (* then *) ;
                           end (* then *)
                       end (* with *)
                   else
                     with STK [ TOP - 1 ] do
                       begin
                         if FALSE then
                           begin
                             WRITELN ( TRACEF , 'VLD - linecnt = ' ,
                                       LINECNT : 1 ) ;
                             WRITELN ( TRACEF , 'VLD - p = ' , P ) ;
                             WRITELN ( TRACEF , 'VLD - q = ' , Q ) ;
                             DUMPSTKELEM ( 'Top - 1' , STK [ TOP - 1 ]
                                           )
                           end (* then *) ;
                         FINDRG ;
                         RGWORK1 := NXTRG ;
                         GENRX ( XL , RGWORK1 , STRCURR , 12 , 0 ) ;
                         FINDRP ;
                         P1 := NXTRG ;
                         B1 := NXTRG + 1 ;

        //*********************************************
        // error fixed 17.05.2019:                     
        // wrong code generation with array elements   
        // GETADR2 must be used                        
        // (this is again the solution).               
        // the original code made a difference         
        // depending on VRBL ... don't know if this    
        // is correct                                  
        //*********************************************

                         GETADR2 ( STK [ TOP - 1 ] , Q2 , P2 , B2 ) ;
                         GENLA_LR ( P1 , Q2 , B2 , P2 ) ;
                         FREEREG ( STK [ TOP - 1 ] ) ;
                         GENRX ( XLH , B1 , 2 , P1 , 0 ) ;
                         GENRX ( XSTH , B1 , 0 , RGWORK1 , 0 ) ;
                         GENRX ( XSTH , B1 , 2 , RGWORK1 , 0 ) ;
                         FINDRP ;
                         P2 := NXTRG ;
                         B2 := NXTRG + 1 ;
                         GENLA_LR ( P2 , 4 , RGWORK1 , 0 ) ;
                         GENRR ( XLR , B2 , B1 ) ;
                         GENRX ( XLH , 14 , 0 , P1 , 0 ) ;
                         GENLA_LR ( P1 , 4 , P1 , 0 ) ;
                         GENRR ( XLTR , 14 , 14 ) ;
                         GENRELRX ( XBC , GEQCND , 4 ) ;
                         GENRX ( XL , P1 , 0 , P1 , 0 ) ;
                         GENRR ( XMVCL , P2 , P1 ) ;
                         GENRX ( XST , P2 , STRCURR , 12 , 0 ) ;
                         AVAIL [ P1 ] := TRUE ;
                         AVAIL [ B1 ] := TRUE ;
                         AVAIL [ P2 ] := TRUE ;
                         AVAIL [ B2 ] := TRUE ;
                         PLEN := - 1 ;
                         DTYPE := VARC ;
                         FPA := ZEROBL ;
                         VPA := RGS ;
                         MEMADR := ZEROBL ;
                         RGADR := RGWORK1 ;
                       end (* with *)
                 end (* tag/ca *) ;

        //*******************************************************
        // varchar move: move string to char array               
        //*******************************************************

          PVMV : begin
                   if Q < 0 then
                     begin
                       SP1 := TOP - 2 ;
                       SP2 := TOP - 1 ;
                       Q := - Q ;
                     end (* then *)
                   else
                     begin
                       SP1 := TOP - 1 ;
                       SP2 := TOP - 2 ;
                     end (* else *) ;

        //*********************************************
        // patblank = blank pattern for mfioperation   
        //*********************************************
        //*********************************************
        // set target to blanks                        
        //*********************************************

                   PATBLANK := DATNULL ;
                   PATBLANK . FPA . DSPLMT := ORD ( ' ' ) ;
                   MFIOPERATION ( STK [ SP2 ] , PATBLANK , Q ) ;

        //*********************************************
        // assign string                               
        //*********************************************

                   STRING_GET_ACTLEN ( STK [ SP1 ] , TRUE , RGWORK ,
                                       FALSE ) ;
                   GENRXLIT ( XC , RGWORK , Q , 0 ) ;
                   ASSIGN_STRING ( STK [ SP2 ] , STK [ SP1 ] , - RGWORK
                                   , 0 , 0 , TRUE , TRUE ) ;
                   AVAIL [ RGWORK ] := TRUE ;
                   TOP := TOP - 2
                 end (* tag/ca *) ;

        //*******************************************************
        // varchar index: retrieve single string char via index  
        //*******************************************************

          PVIX : begin

        //******************************************************
        // load index value from Top - 1                        
        //******************************************************

                   with STK [ TOP - 1 ] do
                     begin
                       if VPA = RGS then
                         RGWORK2 := RGADR
                       else
                         begin
                           LOAD ( STK [ TOP - 1 ] ) ;
                           RGWORK2 := NXTRG ;
                         end (* else *)
                     end (* with *) ;
                   with STK [ TOP - 2 ] do
                     begin

        //*********************************************
        // load maxlength field                        
        // later: decide where string addr is          
        //*********************************************

                       GETADR2 ( STK [ TOP - 2 ] , Q1 , P1 , B1 ) ;
                       FINDRG ;
                       RGWORK1 := NXTRG ;
                       FINDRG ;
                       RGWORK := NXTRG ;
                       GENLA_LR ( RGWORK1 , Q1 , B1 , P1 ) ;
                       GENRX ( XLH , RGWORK , 0 , RGWORK1 , 0 ) ;
                       GENRR ( XLTR , RGWORK , RGWORK ) ;
                       GENLA_LR ( RGWORK , 4 , RGWORK1 , 0 ) ;
                       GENRELRX ( XBC , GEQCND , 4 ) ;
                       GENRX ( XL , RGWORK , 4 , RGWORK1 , 0 ) ;
                       GENRR ( XBCTR , RGWORK , 0 ) ;

        //*********************************************
        // load length field                           
        // later: to check for index inside bounds     
        //*********************************************

                       GENRX ( XLH , RGWORK1 , 2 , RGWORK1 , 0 ) ;

        //*********************************************
        // string address minus one is in rgwork       
        // (virtual origin)                            
        // add index value to virtual origin           
        //*********************************************

                       GENRR ( XCR , RGWORK2 , RGWORK1 ) ;
                       GENRR ( XAR , RGWORK2 , RGWORK ) ;
                       AVAIL [ RGWORK ] := TRUE ;
                       AVAIL [ RGWORK1 ] := TRUE ;
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
                       RGADR := RGWORK2 ;
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
        // PCINT checks for the maxlength field being -1         
        // and throws a UNDEFSTRING error, if not                
        //*******************************************************

          PVLM : with STK [ TOP - 1 ] do
                   begin
                     if VPA = RGS then
                       begin
                         GENRX ( XLH , RGADR , 2 , RGADR , 0 ) ;
                         RGWORK := RGADR
                       end (* then *)
                     else
                       begin
                         GETADR2 ( STK [ TOP - 1 ] , Q1 , P1 , B1 ) ;
                         GENLA_LR ( 14 , Q1 , B1 , P1 ) ;
                         FREEREG ( STK [ TOP - 1 ] ) ;
                         GENRX ( XLH , 14 , 2 , 14 , 0 ) ;
                         RGWORK := 14 ;
                       end (* else *) ;
                     DTYPE := INT ;
                     PLEN := 4 ;
                     VRBL := TRUE ;
                     DRCT := TRUE ;
                     VPA := RGS ;
                     RGADR := RGWORK ;
                     FPA . LVL := 0 ;
                     FPA . DSPLMT := 0 ;
                     MEMADR . LVL := 0 ;
                     MEMADR . DSPLMT := 0 ;
                   end (* with *) ;

        //*******************************************************
        // varchar repeat: repeat string is implemented as       
        // P-Code, because this is needed to build new           
        // Strings on the stack (string of n blanks, for         
        // example) - at least with the P-Code interpreters      
        //*******************************************************

          PVRP : begin

        //******************************************************
        // get constant length of top stack element             
        // or load length into rgwork                           
        //******************************************************

                   with STK [ TOP - 1 ] do
                     if not VRBL then
                       COUNT := FPA . DSPLMT
                     else
                       begin
                         LOAD ( STK [ TOP - 1 ] ) ;
                         RGWORK := NXTRG ;
                         COUNT := - 1 ;
                       end (* else *) ;

        //******************************************************
        // now pop stack to get string parameter                
        // length of result depends heavily on                  
        // type of string parameter                             
        //******************************************************

                   TOP := TOP - 1 ;
                   with STK [ TOP - 1 ] do
                     if DTYPE = CHRC then
                       begin
                         if COUNT >= 0 then
                           NEWLEN := COUNT
                         else
                           NEWLEN := - 1 ;
                       end (* then *)
                     else
                       if DTYPE = CARR then
                         begin
                           if COUNT >= 0 then
                             NEWLEN := COUNT * PLEN
                           else
                             begin
                               NEWLEN := - 1 ;
                             end (* else *)
                         end (* then *)
                       else
                         NEWLEN := - 1 ;

        //******************************************************
        // result string will be in string workarea             
        // rgwork1 will point to result string                  
        //******************************************************

                   with STK [ TOP - 1 ] do
                     if DTYPE = CHRC then
                       begin

        //******************************************************
        // generate code for single character case              
        //******************************************************

                         FINDRP ;
                         P1 := NXTRG ;
                         B1 := NXTRG + 1 ;
                         GENRX ( XL , P1 , STRCURR , 12 , 0 ) ;
                         if NEWLEN >= 0 then
                           begin
                             LITVALUE := NEWLEN * 65536 + NEWLEN ;
                             GENRXLIT ( XL , 14 , LITVALUE , 1 ) ;
                           end (* then *)
                         else
                           begin
                             GENRR ( XLR , 14 , RGWORK ) ;
                             GENRS ( XSLL , 14 , 0 , 16 , 0 ) ;
                             GENRR ( XAR , 14 , RGWORK ) ;
                           end (* else *) ;
                         GENRX ( XST , 14 , 0 , P1 , 0 ) ;
                         GENLA_LR ( P1 , 4 , P1 , 0 ) ;
                         if NEWLEN < 0 then
                           GENRR ( XLR , B1 , RGWORK )
                         else
                           begin
                             LITVALUE := NEWLEN ;
                             GENRXLIT ( XL , B1 , LITVALUE , 1 ) ;
                           end (* else *) ;
                         FINDRP ;
                         P2 := NXTRG ;
                         B2 := NXTRG + 1 ;
                         GENRR ( XXR , P2 , P2 ) ;
                         if VRBL then
                           begin
                             FPA := MEMADR ;
                             MEMADR := ZEROBL ;
                             VRBL := FALSE ;
                             GETADR2 ( STK [ TOP - 1 ] , QX , PX , BX )
                                       ;
                             GENRX ( XIC , B2 , QX , PX , BX ) ;
                           end (* then *)
                         else
                           GENLA_LR ( B2 , FPA . DSPLMT , 0 , 0 ) ;
                         GENRS ( XSLL , B2 , 0 , 24 , 0 ) ;
                         GENRR ( XMVCL , P1 , P2 ) ;
                         AVAIL [ B1 ] := TRUE ;
                         AVAIL [ P2 ] := TRUE ;
                         AVAIL [ B2 ] := TRUE ;
                         AVAIL [ RGWORK ] := TRUE ;
                         FINDRG ;
                         RGWORK1 := NXTRG ;
                         GENRX ( XL , RGWORK1 , STRCURR , 12 , 0 ) ;
                         GENRX ( XST , P1 , STRCURR , 12 , 0 ) ;
                         AVAIL [ P1 ] := TRUE ;
                       end (* then *)
                     else
                       if DTYPE = CARR then
                         begin

        //******************************************************
        // generate code for character array case               
        // p1 = source address of char array                    
        // p2 = target address                                  
        // q2 = count                                           
        //******************************************************

                           FINDRG ;
                           P1 := NXTRG ;
                           if VPA = RGS then
                             begin
                               GENRR ( XLR , P1 , RGADR ) ;
                               AVAIL [ RGADR ] := TRUE ;
                             end (* then *)
                           else
                             begin
                               GETADR2 ( STK [ TOP - 1 ] , QX , PX , BX
                                         ) ;
                               GENLA_LR ( P1 , QX , 0 , 0 ) ;
                               if TRACE_LITERAL then
                                 WRITELN ( TRACEF ,
                                        'repl lit. adr. 13 - index = '
                                           , SCNSTNO : 1 , ' pc = ' ,
                                           PCOUNTER - 1 : 1 ) ;
                               LITTBL [ SCNSTNO ] . LNK := PCOUNTER - 1
                                                   ;
                               CODE . H [ PCOUNTER - 1 ] := TO_HINT (
                                                   QX ) ;
                             end (* else *) ;
                           FINDRG ;
                           P2 := NXTRG ;
                           GENRX ( XL , P2 , STRCURR , 12 , 0 ) ;
                           FINDRG ;
                           Q2 := NXTRG ;
                           if COUNT < 0 then
                             GENRR ( XLR , Q2 , RGWORK )
                           else
                             begin
                               LITVALUE := COUNT ;
                               GENRXLIT ( XL , Q2 , LITVALUE , 0 ) ;
                             end (* else *) ;
                           GENRR ( XLR , 14 , Q2 ) ;
                           GENRXLIT ( XMH , 14 , PLEN , - 1 ) ;
                           GENRX ( XSTH , 14 , 0 , P2 , 0 ) ;
                           GENRX ( XSTH , 14 , 2 , P2 , 0 ) ;
                           GENLA_LR ( P2 , 4 , P2 , 0 ) ;
                           GENSS ( XMVC , PLEN , 0 , P2 , 0 , P1 ) ;
                           GENLA_LR ( P2 , PLEN , P2 , 0 ) ;
                           GENRELRX ( XBCT , Q2 , - 5 ) ;
                           AVAIL [ P1 ] := TRUE ;
                           AVAIL [ Q2 ] := TRUE ;
                           AVAIL [ RGWORK ] := TRUE ;
                           FINDRG ;
                           RGWORK1 := NXTRG ;
                           GENRX ( XL , RGWORK1 , STRCURR , 12 , 0 ) ;
                           GENRX ( XST , P2 , STRCURR , 12 , 0 ) ;
                           AVAIL [ P2 ] := TRUE ;
                         end (* then *)
                       else
                         begin

        //******************************************************
        // generate code for varchar case                       
        // p1 = source address of varchar                       
        // q1 = length of source = length of target             
        // p2 = target address                                  
        // q2 = length of target                                
        //******************************************************

                           FINDRP ;
                           P1 := NXTRG ;
                           Q1 := NXTRG + 1 ;
                           if VPA = RGS then
                             begin
                               GENRR ( XLR , P1 , RGADR ) ;
                               AVAIL [ RGADR ] := TRUE ;
                             end (* then *)
                           else
                             begin
                               GETADR2 ( STK [ TOP - 1 ] , QX , PX , BX
                                         ) ;
                               GENLA_LR ( P1 , QX , BX , PX ) ;
                             end (* else *) ;
                           FINDRP ;
                           P2 := NXTRG ;
                           Q2 := NXTRG + 1 ;
                           GENRX ( XL , P2 , STRCURR , 12 , 0 ) ;
                           if COUNT >= 0 then
                             begin
                               FINDRG ;
                               RGWORK := NXTRG ;
                               LITVALUE := COUNT ;
                               GENRXLIT ( XL , RGWORK , LITVALUE , 0 )
                                          ;
                             end (* then *) ;
                           GENRR ( XLR , 14 , RGWORK ) ;
                           GENRX ( XLH , Q2 , 2 , P1 , 0 ) ;
                           GENRR ( XLR , Q1 , Q2 ) ;
                           GENRX ( XMH , 14 , 2 , P1 , 0 ) ;
                           GENRX ( XSTH , 14 , 0 , P2 , 0 ) ;
                           GENRX ( XSTH , 14 , 2 , P2 , 0 ) ;
                           GENLA_LR ( P2 , 4 , P2 , 0 ) ;
                           GENRX ( XLH , 14 , 0 , P1 , 0 ) ;
                           GENLA_LR ( P1 , 4 , P1 , 0 ) ;
                           GENRR ( XLTR , 14 , 14 ) ;
                           GENRELRX ( XBC , GEQCND , 4 ) ;
                           GENRX ( XL , P1 , 0 , P1 , 0 ) ;

        //******************************************************
        // length fields are set correctly                      
        // p1 = source of char string                           
        // p2 = target of char string                           
        // q1 = length of char string                           
        // q2 = length of char string                           
        // rgwork = count                                       
        // CSPACTIVE [trg15] ... indicate loss of reg trg15     
        //******************************************************

                           CSPACTIVE [ TRG15 ] := FALSE ;
                           GENRR ( XLR , 14 , Q1 ) ;
                           GENRR ( XLR , 15 , P1 ) ;
                           GENRR ( XMVCL , P2 , P1 ) ;
                           GENRR ( XLR , Q1 , 14 ) ;
                           GENRR ( XLR , Q2 , 14 ) ;
                           GENRR ( XLR , P1 , 15 ) ;
                           GENRELRX ( XBCT , RGWORK , - 4 ) ;
                           AVAIL [ P1 ] := TRUE ;
                           AVAIL [ Q1 ] := TRUE ;
                           AVAIL [ Q2 ] := TRUE ;
                           AVAIL [ RGWORK ] := TRUE ;
                           FINDRG ;
                           RGWORK1 := NXTRG ;
                           GENRX ( XL , RGWORK1 , STRCURR , 12 , 0 ) ;
                           GENRX ( XST , P2 , STRCURR , 12 , 0 ) ;
                           AVAIL [ P2 ] := TRUE ;
                         end (* else *) ;

        //******************************************************
        // setup topmost stack element for result string        
        //******************************************************

                   with STK [ TOP - 1 ] do
                     begin
                       DTYPE := VARC ;
                       PLEN := - 1 ;
                       VRBL := TRUE ;
                       DRCT := TRUE ;
                       VPA := RGS ;
                       RGADR := RGWORK1 ;
                       FPA . LVL := 0 ;
                       FPA . DSPLMT := 0 ;
                       MEMADR . LVL := 0 ;
                       MEMADR . DSPLMT := 0 ;
                       if FALSE then
                         begin
                           WRITELN ( TRACEF ,
                                     'after handling vrp - linecnt = '
                                     , LINECNT : 1 ) ;
                           DUMPSTKELEM ( 'Top - 1' , STK [ TOP - 1 ] )
                                         ;
                           WRITE ( TRACEF , 'rgadr = ' , RGADR ) ;
                         end (* then *) ;
                     end (* with *) ;
                 end (* tag/ca *) ;
        end (* case *)
      end (* STRINGOPS *) ;


   procedure BOPERATION_COMPARE ;

   //****************************************************************
   // BINARY OPERATIONS - Comparison                                 
   //****************************************************************


      label 10 , 20 ;

      var L , R : DATUM ;

          //**************************************************
          // l,r = left and right operand                     
          // lop,rop = stack index of left and right operand  
          // lr = left/right interchange flag                 
          //**************************************************

          LOP , ROP : STKPTR ;
          LR : BOOLEAN ;
          Q1 : ADRRNG ;
          P1 , B1 : LVLRNG ;

      begin (* BOPERATION_COMPARE *)
        if FALSE then
          begin
            WRITELN ( TRACEF , 'start boperation_compare, linecnt = ' ,
                      LINECNT : 1 ) ;
            WRITELN ( TRACEF , 'opcode = ' , OPCODE ) ;
            WRITELN ( TRACEF , 'opndtype = ' , OPNDTYPE ) ;
            DUMPSTKELEM ( 'top ' , STK [ TOP ] ) ;
            DUMPSTKELEM ( 'top - 1 ' , STK [ TOP - 1 ] ) ;
          end (* then *) ;

        //*****************************************************
        // Opcodes are PEQU , PNEQ , PGRT , PLEQ , PLES , PGEQ 
        //*****************************************************

        L := STK [ TOP - 1 ] ;
        R := STK [ TOP ] ;

        //*******************************************
        // if type = SET, call SETCOMPARE and leave  
        //*******************************************

        if OPNDTYPE = PSET then
          begin
            SETCOMPARE ( L , R ) ;
            STK [ TOP - 1 ] := L ;
            return
          end (* then *) ;

        //*****************************************************
        // if type = varying string, call STRINGCOMPARE        
        // and leave                                           
        //*****************************************************

        if OPNDTYPE = VARC then
          begin
            STRINGCOMPARE ( L , R ) ;
            STK [ TOP - 1 ] := L ;
            return
          end (* then *) ;

        //*****************************************************
        // if type = character array, call COMPARE_CARR        
        // and leave                                           
        //*****************************************************

        if OPNDTYPE = CARR then
          begin
            COMPARE_CARR ( L , R , Q , P , COMPTYPE ) ;
            CSPACTIVE [ TRG1 ] := FALSE ;
            BRCND := BRMSK [ OPCODE ] ;
            STK [ TOP - 1 ] := L ;
            return
          end (* then *) ;

        //************************************************
        // DETERMINE WHICH OPERAND SHOULD BE USED         
        // AS LEFT HAND OPERAND ...                       
        //************************************************

        LR := ( STK [ TOP - 1 ] . VRBL and STK [ TOP ] . DRCT ) or (
              not STK [ TOP - 1 ] . DRCT ) or ( not STK [ TOP ] . VRBL
              ) ;
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
        if FALSE then
          begin
            WRITELN ( TRACEF ,
                   'boperation_compare after LR computing, linecnt = '
                      , LINECNT : 1 ) ;
            WRITELN ( TRACEF , 'opcode = ' , OPCODE ) ;
            WRITELN ( TRACEF , 'lr = ' , LR ) ;
            DUMPSTKELEM ( 'left ' , L ) ;
            DUMPSTKELEM ( 'Right' , R ) ;
          end (* then *) ;

        //*****************************************************
        // change OPCODE depending on LR flag                  
        //*****************************************************

        if not LR then
          OPCODE := INVBRM [ OPCODE ] ;

        //*****************************************************
        // other operand types                                 
        //*****************************************************

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

        //***************************
        // if NOT VRBL (I.E. CONST)  
        //***************************

                  begin
                    if FPA . DSPLMT = 1 then
                      if OPCODE = PLES then

        //********************************
        // COMPARISON AGAINST 0 IS BETTER 
        //********************************

                        begin
                          FPA . DSPLMT := 0 ;
                          OPCODE := PLEQ
                        end (* then *)
                      else
                        if OPCODE = PGEQ then
                          begin
                            FPA . DSPLMT := 0 ;
                            OPCODE := PGRT
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

                          GENRR ( XLTR , L . RGADR , L . RGADR ) ;

        //******************************************************
        // opc is pequ or pneq                                  
        // this logic applies imo                               
        // because nil = -1                                     
        //******************************************************

                          if OPCODE = PEQU then
                            OPCODE := PLES
                          else
                            OPCODE := PGEQ ;
                        end (* then *)
                      else
                        GENRXLIT ( XC , L . RGADR , FPA . DSPLMT , 0 )
                                   ;
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
                          GENRXLIT_EXTENDED ( XEX , L . RGADR , Q , 0 ,
                                              XCLI ) ;
                          OPCODE := INVBRM [ OPCODE ] ;
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

        //****************
        // L IS IN MEMORY 
        //****************

                  if VRBL then
                    begin
                      CLEAR_REG := FALSE ;
                      LOAD ( STK [ ROP ] ) ;
                      CLEAR_REG := TRUE ;
                      LR := not LR ;
                      if FALSE then
                        WRITELN ( TRACEF ,
                      'start boperation_compare switch LR, linecnt = '
                                  , LINECNT : 1 ) ;
                      goto 10 ;
                    end (* then *)
                  else
                    begin
                      GETQB ( L , Q1 , B1 , 0 ) ;
                      GENSI ( XCLI , Q1 , B1 , FPA . DSPLMT ) ;
                    end (* else *)
              else

        //*****************
        // L IS A CONSTANT 
        //*****************

                if VRBL then
                  begin
                    LR := not LR ;
                    if FALSE then
                      WRITELN ( TRACEF ,
                      'start boperation_compare switch LR, linecnt = '
                                , LINECNT : 1 ) ;
                    goto 10
                  end (* then *)
                else
                  begin
                    LOAD ( STK [ ROP ] ) ;
                    if FALSE then
                      WRITELN ( TRACEF ,
                      'start boperation_compare switch LR, linecnt = '
                                , LINECNT : 1 ) ;
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
                             GENRR ( XCDR , L . RGADR , R . RGADR ) ;
                             AVAILFP [ RGADR ] := TRUE
                           end (* then *)
                         else

        //***********************
        // VPA = MEM OR NOT DRCT 
        //***********************

                           GENRX ( XCD , L . RGADR , Q1 , B1 , P1 )
                       end (* then *)
                     else

        //**********
        // CONSTANT 
        //**********

                       if RCNST = 0.0 then
                         GENRR ( XLTDR , L . RGADR , L . RGADR )
                       else
                         GENRXDLIT ( XCD , L . RGADR , RCNST ) ;
                     AVAILFP [ L . RGADR ] := TRUE ;
                   end (* with *) ;
        end (* case *) ;
        BRCND := BRMSK [ OPCODE ] ;
        STK [ TOP - 1 ] := L ;
      end (* BOPERATION_COMPARE *) ;


   procedure BOPERATION ;

   //****************************************************************
   // BINARY OPERATIONS                                              
   //****************************************************************


      label 30 ;

      var L , R : DATUM ;
          X : DATUM ;

          //***********************
          //LEFT AND RIGHT OPERANDS
          //***********************

          LOP , ROP : STKPTR ;

          //**************************************
          //STACK INDEX OF LEFT AND RIGHT OPERANDS
          //**************************************

          OP1 , OP2 : BYTE ;
          LR : BOOLEAN ;

          //***************************
          //LEFT/RIGHT INTERCHANGE FLAG
          //***************************

          Q1 : ADRRNG ;
          P1 , B1 : LVLRNG ;

      begin (* BOPERATION *)

        //************************************************
        // DETERMINE WHICH OPERAND SHOULD BE USED         
        // AS LEFT HAND OPERAND ...                       
        //************************************************

        if FALSE then
          WRITELN ( TRACEF , 'start boperation, linecnt = ' , LINECNT :
                    1 ) ;
        LR := ( OPCODE in [ PSBA , PSBR , PDVR , PDVI , PMOD , PDIF ,
              PINN ] ) or ( STK [ TOP - 1 ] . VRBL and STK [ TOP ] .
              DRCT ) or ( not STK [ TOP - 1 ] . DRCT ) or ( not STK [
              TOP ] . VRBL ) ;
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
        if FALSE then
          begin
            WRITELN ( TRACEF ,
                      'boperation after LR computing, linecnt = ' ,
                      LINECNT : 1 ) ;
            WRITELN ( TRACEF , 'lr = ' , LR ) ;
            DUMPSTKELEM ( 'left ' , L ) ;
            DUMPSTKELEM ( 'Right' , R ) ;
          end (* then *) ;
        case OPCODE of
          PADI , PSBI :
            begin
              if not L . DRCT then
                LOAD ( L ) ;
              if R . DRCT then
                if OPCODE = PADI then
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

        //***********************************************************
        //CONST<OPR>CONST AND VRBL<OPR>CONST CASES ARE COMPLETED NOW 
        //***********************************************************

              OP1 := XAR ;
              OP2 := XA ;
              if OPCODE = PSBI then
                begin
                  OP1 := XSR ;
                  OP2 := XS
                end (* then *) ;
              if R . VRBL then
                begin
                  Q := L . FPA . DSPLMT ;
                  L . FPA . DSPLMT := 0 ;

        //********
        //SAVE FPA
        //********

                  LOAD ( L ) ;
                  if R . DTYPE <> INT then
                    if R . DTYPE = HINT then
                      OP2 := OP2 - 16

        //********************************
        // SWITCH TO HALFWORD INSTRUCTION 
        //********************************

                    else
                      LOAD ( R ) ;
                  if R . DRCT then
                    if R . VPA = RGS then
                      begin
                        GENRR ( OP1 , L . RGADR , R . RGADR ) ;
                        AVAIL [ R . RGADR ] := TRUE
                      end (* then *)
                    else

        //*******
        //VPA=MEM
        //*******

                      begin
                        Q1 := R . MEMADR . DSPLMT ;
                        P1 := R . MEMADR . LVL ;
                        BASE ( Q1 , P1 , B1 ) ;
                        GENRX ( OP2 , L . RGADR , Q1 , B1 , P1 ) ;
                      end (* else *)
                  else

        //**********
        //NOT R.DRCT
        //**********

                    begin
                      GETOPERAND ( R , Q1 , P1 , B1 ) ;
                      GENRX ( OP2 , L . RGADR , Q1 , B1 , P1 ) ;
                    end (* else *) ;
                  L . FPA . DSPLMT := Q ;

        //***********
        //RESTORE FPA
        //***********

                end (* then *) ;
              if not LR and ( OPCODE = PSBI ) then

        //*********************************
        //THIS DOES NOT SEEM TO BE COMPLETE
        //*********************************

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

        //**************************************************
        // neu 09.2016 : addiere int zu adresse / oppolzer  
        // chg 11.2017 : error, when adr is second operand  
        //**************************************************

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

        //**************************************************
        // neu 09.2019 : subtrahiere 2 adressen / oppolzer  
        //**************************************************

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

        //**************************************************
        // hier weiter alt - mpi                            
        //**************************************************

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

        //**********
        //NOT R.VRBL
        //**********

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

        //***************
        // BOTH CONSTANTS
        //***************

              if R . FPA . DSPLMT = 0 then
                ERROR ( 300 )

        //*****************
        // DIVISION BY ZERO
        //*****************

              else
                if OPCODE = PDVI then
                  L . FPA . DSPLMT := L . FPA . DSPLMT DIV R . FPA .
                                      DSPLMT
                else
                  L . FPA . DSPLMT := L . FPA . DSPLMT MOD R . FPA .
                                      DSPLMT
            else

        //*******************
        // MORE COMMON CASES 
        //*******************

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

        //*******
        //^R.VRBL
        //*******

                  GENRXLIT ( XD , L . RGADR , R . FPA . DSPLMT , 0 ) ;
                if OPCODE = PDVI then
                  begin
                    AVAIL [ L . RGADR ] := TRUE ;
                    L . RGADR := L . RGADR + 1
                  end (* then *)
                else
                  AVAIL [ L . RGADR + 1 ] := TRUE ;
              end (* else *) ;
          PAND , PIOR , PXOR :
            with R do
              begin
                OP1 := XNR ;
                if OPCODE = PIOR then
                  OP1 := XORX
                else
                  if OPCODE = PXOR then
                    OP1 := XXR ;
                LOAD ( L ) ;
                LOAD ( R ) ;

        //***************************************************
        // THIS CAN BE IMPROVED BY USING THE CONDITION CODE  
        // AS THE TOP ELEMENT                                
        //***************************************************

                GENRR ( OP1 , L . RGADR , RGADR ) ;
                AVAIL [ RGADR ] := TRUE ;
              end (* with *) ;
          PADR , PSBR :
            begin
              OP1 := XADR ;
              OP2 := XAD ;
              if OPCODE = PSBR then
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

        //***********************
        // VPA = MEM OR NOT DRCT 
        //***********************

                    GENRX ( OP2 , L . RGADR , Q1 , B1 , P1 )
                end (* then *)
              else

        //**********
        // CONSTANT 
        //**********

                GENRXDLIT ( OP2 , L . RGADR , R . RCNST )
            end (* tag/ca *) ;
          PDVR , PMPR :
            begin
              LOAD ( L ) ;
              OP1 := XDDR ;
              OP2 := XDD ;
              if OPCODE = PMPR then
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

        //*************************
        // R.VPA = MEM OR NOT DRCT 
        //*************************

                    GENRX ( OP2 , L . RGADR , Q1 , B1 , P1 )
                end (* then *)
              else

        //************
        //  CONSTANT  
        //************

                GENRXDLIT ( OP2 , L . RGADR , R . RCNST )
            end (* tag/ca *) ;
        end (* case *) ;
        STK [ TOP - 1 ] := L ;
      end (* BOPERATION *) ;


   begin (* ASMNXTINST *)
     if OPCODE in [ PXBG , PXEN ] then
       return ;
     if OLDOPCODE = PUJP then
       if not CASE_FLAG then

     //**********************************
     // IGNORE INACCESSIBLE INSTRUCTIONS 
     //**********************************

         if not ( OPCODE in [ PXLB , PEND , PCST , PLAB , PLOC , PDEF ,
         PRET , PSTP , PENT , PCTS ] ) then
           return ;

     //******************************
     // XLATE COND CODE TO BOOL. VAL 
     //******************************

     if BRCND >= 0 then
       if not ( OPCODE in [ PFJP , PNOT , PLOC ] ) then
         with STK [ TOP - 1 ] do
           begin

     //**************************
     // JUST NEGATE TOP OF STACK 
     //**************************

             if NEG_CND then
               begin
                 LOAD ( STK [ TOP - 1 ] ) ;
                 if OPCODE = PAND then
                   GENRR ( XBCTR , RGADR , 0 )
                 else
                   GENRXLIT ( XX , RGADR , 1 , 0 ) ;
               end (* then *)

     //***********************************
     // OTHERWISE TRANSLATE CC TO BOOLEAN 
     //***********************************

             else
               begin
                 FINDRG ;
                 GENLA_LR ( NXTRG , 1 , 0 , 0 ) ;

     //***********
     //ASSUME TRUE
     //***********

                 GENRELRX ( XBC , BRCND , 3 ) ;

     //**************
     // BC BRCND,*+3 
     //**************

                 GENRR ( XSR , NXTRG , NXTRG ) ;

     //*******************************
     // THEN CHANGE TO FALSE IF NEEDED
     //*******************************

                 LAST_CC . LAST_PC := 0 ;

     //**************************
     // THIS C.C. HAS NO MEANING 
     //**************************

                 DTYPE := BOOL ;
                 VRBL := TRUE ;
                 DRCT := TRUE ;
                 VPA := RGS ;
                 RGADR := NXTRG ;
                 FPA := ZEROBL ;
               end (* else *) ;
             BRCND := - 1 ;
             NEG_CND := FALSE ;

     //*****************************
     // RESET C.C. FLAG TO INACTIVE 
     //*****************************

           end (* with *) ;
     if not CASE_FLAG then
       begin

     //*********************************************
     // ask MANAGE_LITERALS if there is danger ...  
     //*********************************************

         MANAGE_LITERALS ( 7 , PCOUNTER , NIL , 0 , RESULT_DANGER ) ;
         if ( NXTLIT >= LITDANGER ) or ( RESULT_DANGER >= 0 ) then
           begin

     //****************************
     // EMPTY THE LITERAL POOL NOW 
     //****************************

             GENRX ( XBC , ANYCND , 0 , 0 , 0 ) ;
             I := PCOUNTER - 1 ;
             MANAGE_LITERALS ( 1 , PCOUNTER , NIL , 0 , DUMMYINT ) ;
             CODE . H [ I ] := TO_HINT ( BASE_DSPLMT ( PCOUNTER ) ) ;
           end (* then *)
       end (* then *) ;

     //******************************
     // verarbeitung abh. vom opcode 
     //******************************

     if FALSE then
       begin
         WRITELN ( TRACEF ) ;
         WRITELN ( TRACEF ) ;
         WRITELN ( TRACEF , 'linecnt = ' , LINECNT : 1 ) ;
         WRITELN ( TRACEF , 'stack vor pcode = ' , PTBL [ OPCODE ] ) ;
         DUMPSTK ( 1 , TOP - 1 )
       end (* then *) ;
     case OPCODE of
       PLOD : with STK [ TOP ] do
                begin
                  STK [ TOP ] := DATNULL ;
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
                    if LAST_PC = PCOUNTER then

     //******************************
     // TRY TO OPTIMIZE STR/LOD PAIR 
     //******************************

                      if MEMADR = STOPND then
                        if OPNDTYPE = STDT then

     //****************************
     // IN CASE OF VARIANT RECORDS 
     //****************************

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
                if OPNDTYPE <> NON then
                  begin
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

     //*********************************
     // SAVE INFO ABOUT STORED VARIABLE 
     //*********************************

                      begin
                        STOPND . LVL := P ;
                        STOPND . DSPLMT := Q ;
                        STDT := OPNDTYPE ;
                        STORE ( TOP , FALSE ) ;
                        if OPNDTYPE <= CHRC then
                          LAST_PC := 0
                        else
                          LAST_PC := PCOUNTER ;
                        STRGX := STK [ TOP ] . RGADR ;
                      end (* with *)
                  end (* then *)
              end (* tag/ca *) ;
       PSTO : begin
                STORE ( TOP - 1 , TRUE ) ;

     //********
     //INDIRECT
     //********

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
       PIAC : begin
                STK [ TOP ] := STK [ TOP - 1 ] ;
                TOP := TOP + 1 ;
                with STK [ TOP - 1 ] do
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
                  end (* with *)
              end (* tag/ca *) ;
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
     // REF. TO EXP. STACK                                  
     //*****************************************************

                             SCNSTNO := CHECK_CHAR_LITERAL ;
                             FPA . LVL := - 1 ;
                             FPA . DSPLMT := LITTBL [ SCNSTNO ] . XIDP
                                             ;
                             VRBL := FALSE ;
                             DRCT := TRUE ;
                             if FALSE then
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

     //*******
     //INT,ADR
     //*******

                                GENRX ( XL , NXTRG , Q1 , B1 , P1 ) ;
                            VPA := RGS ;
                            RGADR := NXTRG ;
                          end (* then *) ;

     //*********************
     // VPA IS IN A REG. NOW
     //*********************

                        if Q > HALFINT then
                          ERROR ( 504 ) ;

     //***************************
     // TOO LARGE FOR A HALF WORD 
     //***************************

                        Q2 := POWER2 ( Q ) ;
                        if Q2 = 1 then
                          GENRR ( XAR , RGADR , RGADR )
                        else
                          if Q2 > 0 then
                            GENRS ( XSLA , RGADR , 0 , Q2 , 0 )
                          else
                            if Q2 < 0 then
                              GENRXLIT ( XMH , RGADR , Q , - 2 ) ;

     //******
     //=H'Q' 
     //******

                      end (* then *) ;

     //***********************************
     // NOW ADD THE TOP TO THE SECOND TOP 
     //***********************************

                    with STK [ TOP - 1 ] do
                      begin
                        if not VRBL then
                          if FPA . LVL < 0 then

     //***************************************
     //I.E. INDEXING THROUGH A CONSTANT STRING
     //***************************************

                            LOAD ( STK [ TOP - 1 ] ) ;
                        if not DRCT then
                          LOAD ( STK [ TOP - 1 ] ) ;
                      end (* with *) ;
                    STK [ TOP - 1 ] . FPA . DSPLMT := STK [ TOP - 1 ] .
                                                   FPA . DSPLMT + FPA .
                                                   DSPLMT ;
                    FPA . DSPLMT := 0 ;
                    if VRBL and STK [ TOP - 1 ] . VRBL then
                      if VPA = RGS then
                        if STK [ TOP - 1 ] . VPA = RGS then

     //******************************************
     // BOTH OPERANDWS IN REGS                   
     // free reg with higher number - opp / 2016 
     //******************************************
     // klappt nicht ...                         
     //******************************************

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

     //********************************
     //TOP IN REG., 2_ND TOP IN MEMORY.
     //********************************

                          begin
                            Q1 := STK [ TOP - 1 ] . MEMADR . DSPLMT ;
                            P1 := STK [ TOP - 1 ] . MEMADR . LVL ;
                            BASE ( Q1 , P1 , B1 ) ;
                            GENRX ( XA , RGADR , Q1 , B1 , P1 ) ;
                            STK [ TOP - 1 ] . VPA := RGS ;
                            STK [ TOP - 1 ] . RGADR := RGADR ;
                          end (* else *)
                      else

     //*********
     //VPA = MEM
     //*********

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

     //*******************************
     //NOT (VRBL AND STK[TOP-1].VRBL) 
     //*******************************

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
                    SOPERATION ( STK [ TOP ] , STK [ TOP + 1 ] , OPCODE
                                 , Q )
                  end (* then *)
                else
                  begin  // BACKWARD MOVE
                    Q := ABS ( Q ) ;
                    SOPERATION ( STK [ TOP + 1 ] , STK [ TOP ] , OPCODE
                                 , Q ) ;
                  end (* else *) ;
              end (* tag/ca *) ;
       PMV1 : begin
                OPCODE := PMOV ;
                TOP := TOP - 2 ;
                if Q > 0 then
                  begin  // FORWARD MOVE
                    SOPERATION ( STK [ TOP ] , STK [ TOP + 1 ] , OPCODE
                                 , Q )
                  end (* then *)
                else
                  begin  // BACKWARD MOVE
                    Q := ABS ( Q ) ;
                    SOPERATION ( STK [ TOP + 1 ] , STK [ TOP ] , OPCODE
                                 , Q ) ;
                  end (* else *) ;
                TOP := TOP + 1 ;
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
       PMCC : begin
                TOP := TOP - 2 ;
                MCCOPERATION ( STK [ TOP ] , STK [ TOP + 1 ] , Q ) ;
                TOP := TOP + 1 ;
              end (* tag/ca *) ;
       PMCV : begin
                TOP := TOP - 3 ;
                MCVOPERATION ( STK [ TOP ] , STK [ TOP + 1 ] , STK [
                               TOP + 2 ] ) ;
                TOP := TOP + 1 ;
              end (* tag/ca *) ;

     //***************************
     // CONTROL/BRANCH OPERATIONS 
     //***************************

       PUJP , PFJP , PXJP , PPOP , PCUP , PENT , PLOC , PXLB , PUXJ ,
       PMST , PRET , PCSP , PSTP , PLAB , PDEF , PDFC , PCST , PEND :
         COPERATION ;
       PCHK : CHKOPERATION ;

     //******************
     // UNARY OPERATIONS 
     //******************

       PABI , PABR , PNGI , PNGR , PINC , PDEC , PNOT , PODD , PCHR ,
       PORD , PFLO , PFLT , PNEW , PSAV , PRST , PSQI , PSQR , PCTS ,
       PCTI , PXPO :
         UOPERATION ;

     //*******************
     // BINARY OPERATIONS 
     //*******************

       PADI , PSBI , PMPI , PDVI , PMOD , PAND , PIOR , PADR , PSBR ,
       PMPR , PDVR , PADA , PSBA , PXOR :
         begin
           TOP := TOP - 1 ;
           BOPERATION
         end (* tag/ca *) ;
       PEQU , PNEQ , PLES , PLEQ , PGRT , PGEQ :
         begin
           TOP := TOP - 1 ;
           BOPERATION_COMPARE ;
         end (* tag/ca *) ;

     //****************
     // SET OPERATIONS 
     //****************

       PINN , PINT , PUNI , PDIF , PASE , PASR :
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
       PBGN : ;               // do nothing on BGN
       otherwise              // otherwise show error
         ERROR_SYMB ( 620 , PTBL [ OPCODE ] )
     end (* case *) ;
     if FALSE then
       begin
         WRITELN ( TRACEF , 'stack nach pcode = ' , PTBL [ OPCODE ] ) ;
         DUMPSTK ( 1 , TOP - 1 ) ;
         WRITELN ( TRACEF , '--------------------------------------' ,
                   '--------------------------------------' ) ;
       end (* then *) ;
     OLDOPCODE := OPCODE ;
   end (* ASMNXTINST *) ;



procedure SETUP ;

//*******************************************
// INITIALIZE GLOBAL VARIABLE/SET FLAGS ETC. 
//*******************************************


   var I : INTEGER ;

   begin (* SETUP *)
     GS . IN_PROCBODY := FALSE ;
     GS . FILL_LINEPTR := FALSE ;
     GS . MOD1DEFSTEP := - 1 ;
     GS . MOD2DEFSTEP := - 1 ;
     GS . XBG_XEN_SUPPRESSED := - 1 ;
     GS . LAST_ERRORCODE := 0 ;
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
     for OPCODE := PCTS to PRED ( UNDEF_OP ) do
       begin
         P_OPCODE := PTBL [ OPCODE ] ;
         ENTERLOOKUP
       end (* for *) ;
     OP_SP := FALSE ;
     for CSP := PCTR to PRED ( UNDEF_CSP ) do
       begin
         P_OPCODE := CSPTBL [ CSP ] ;
         ENTERLOOKUP
       end (* for *) ;
     OP_SP := TRUE ;

     //****************************
     //TO PREPARE FOR OPCODE LOOKUP
     //****************************

     for NXTRG := 0 to RGCNT do
       AVAIL [ NXTRG ] := TRUE ;
     for NXTRG := 0 to FPCNT do
       AVAILFP [ NXTRG ] := TRUE ;

     //****************************************
     // set typecode depending on type letter  
     // no numbers !!                          
     //****************************************

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
     TOP := 1 ;
     CURLVL := 1 ;
     BRCND := - 1 ;
     NEG_CND := FALSE ;
     TRACE := FALSE ;
     OLDOPCODE := PBGN ;
     CSPACTIVE [ TRG1 ] := FALSE ;
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
     MANAGE_LITERALS ( 0 , PCOUNTER , NIL , 0 , DUMMYINT ) ;
     LAST_CC . LAST_PC := 0 ;
     TXR_CONTENTS . VALID := FALSE ;
     LAST_MVC . LAST_PC := 0 ;
     LAST_STR . LAST_PC := 0 ;
     LAST_STR . STOPND := ZEROBL ;
     OPT_FLG := TRUE ;
     HEXCHARS := '0123456789ABCDEF' ;
     TESTCNT := 0 ;
     PDEF_CNT := 0 ;
     CSPACTIVE [ 0 ] := FALSE ;
     CSPACTIVE [ 1 ] := FALSE ;
     CSPACTIVE [ 2 ] := FALSE ;
     CSPACTIVE [ 3 ] := FALSE ;
     CSPACTIVE [ 4 ] := FALSE ;
     CSPACTIVE [ 5 ] := FALSE ;
     CSPACTIVE [ 6 ] := FALSE ;
     CSPACTIVE [ 7 ] := FALSE ;
     CSPACTIVE [ 8 ] := FALSE ;
     CSPACTIVE [ 9 ] := FALSE ;
     CSPACTIVE [ 10 ] := FALSE ;
     CSPACTIVE [ 11 ] := FALSE ;
     CSPACTIVE [ 12 ] := FALSE ;
     CSPACTIVE [ 13 ] := FALSE ;
     CSPACTIVE [ 14 ] := FALSE ;
     CSPACTIVE [ 15 ] := FALSE ;
     FILADR_LOADED := FALSE ;
     OLDCSP := PSIO ;
     PROCOFFSET_OLD := 0 ;
     PCOUNTER := 0 ;
     TOS_COUNT := 0 ;
   end (* SETUP *) ;



procedure CHK_INCLUDE ;

   var INCLUDECMD : CHAR ( 100 ) ;
       INCLFILE : CHAR ( 8 ) ;

   begin (* CHK_INCLUDE *)

     //**************************************************
     // check for %INCLUDE                               
     // if so, change input file from PCODE to PCODEx    
     //**************************************************

     if PCODE_FILENO = 0 then
       begin
         if PCODE -> = '%' then
           begin
             READLN ( PCODE , INCLUDECMD ) ;
             if LEFT ( INCLUDECMD , 9 ) <> '%INCLUDE ' then
               ERROR ( 710 )
             else
               begin
                 INCLFILE := SUBSTR ( INCLUDECMD , 10 , 8 ) ;
                 if LEFT ( INCLFILE , 5 ) <> 'pcode' then
                   ERROR ( 711 ) ;
                 case INCLFILE [ 6 ] of
                   '1' : begin
                           RESET ( PCODE1 ) ;
                           PCODE_FILENO := 1 ;
                           PCODEP := ADDR ( PCODE1 ) ;
                         end (* tag/ca *) ;
                   '2' : begin
                           RESET ( PCODE2 ) ;
                           PCODE_FILENO := 2 ;
                           PCODEP := ADDR ( PCODE2 ) ;
                         end (* tag/ca *) ;
                   '3' : begin
                           RESET ( PCODE3 ) ;
                           PCODE_FILENO := 3 ;
                           PCODEP := ADDR ( PCODE3 ) ;
                         end (* tag/ca *) ;
                   otherwise
                     ERROR ( 712 )
                 end (* case *)
               end (* else *)
           end (* then *)
       end (* then *) ;
   end (* CHK_INCLUDE *) ;



begin (* HAUPTPROGRAMM *)
  RESET ( PCODE ) ;
  FIRST_LIST002 := TRUE ;
  INIT := TRUE ;
  SETUP ;
  INIT := FALSE ;

  //**********
  //INITIALIZE
  //**********

  if OSPARM <> NIL then
    with OSPARM -> do
      if PLENGTH >= 2 then
        for Q := 1 to PLENGTH - 1 do
          if ( PSTRING [ Q ] = 'T' ) and ( PSTRING [ Q + 1 ] = 'R' )
          then
            TRACE := TRUE
          else
            if ( PSTRING [ Q ] = 'C' ) and ( PSTRING [ Q + 1 ] = 'K' )
            then
              CKMODE := TRUE
            else
              if ( PSTRING [ Q ] = 'M' ) and ( PSTRING [ Q + 1 ] = 'U'
              ) then
                MUSIC := TRUE ;
  TIMER := CLOCK ( 0 ) ;
  WRITELN ( OUTPUT , '****' : 7 ,
            ' STANFORD PASCAL POST-PROCESSOR, OPPOLZER VERSION OF ' ,
            VERSION ) ;
  if not MUSIC then
    WRITELN ( OUTPUT ) ;

  //******************************************************************
  // read pcode file first time to gather procedure information       
  //******************************************************************

  PIANKER := NIL ;
  XXIANKER := NIL ;
  PCODE_FILENO := 0 ;
  PCODEP := ADDR ( PCODE ) ;
  repeat
    CHK_INCLUDE ;
    EOF_PCODE := READNXTINST ( PCODEP -> , 1 ) ;
    if EOF_PCODE and ( PCODE_FILENO > 0 ) then
      begin
        PCODE_FILENO := 0 ;
        PCODEP := ADDR ( PCODE ) ;
      end (* then *)
  until OPCODE = PSTP ;
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
          WRITELN ( TRACEF , 'scratchpos.: ' , PIAKT -> . SCRATCHPOS )
                    ;
          PIAKT := PIAKT -> . NEXT
        end (* while *)
    end (* then *) ;
  PIAKT := NIL ;

  //******************************************************************
  // read pcode file second time to process p-codes                   
  // curpno must be set to minus 1 again,                             
  // otherwise the first LOC instruction will go wild ...             
  // mark (heapmark) must be delayed after the first read loop :-)    
  //******************************************************************

  MARK ( HEAPMARK ) ;
  RESET ( PCODE ) ;
  repeat
    CHK_INCLUDE ;
    EOF_PCODE := READNXTINST ( PCODEP -> , 2 ) ;
    if GS . LAST_ERRORCODE <> 606 then
      ASMNXTINST ;
    if TRACE then
      DUMPSTK ( 1 , TOP - 1 ) ;
    if EOF_PCODE and ( PCODE_FILENO > 0 ) then
      begin
        PCODE_FILENO := 0 ;
        PCODEP := ADDR ( PCODE ) ;
      end (* then *)
  until OPCODE = PSTP ;

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
            ' BYTES OF CODE GENERATED,' , TIMER * 0.001 : 7 : 2 ,
            ' SECONDS IN POST_PROCESSING.' ) ;
  if S370CNT > 0 then
    if FALSE then
      WRITELN ( OUTPUT , '****' : 7 , S370CNT : 8 ,
                ' "370"-ONLY INSTRUCTION(S) ISSUED.' ) ;
  EXIT ( ERRORCNT ) ;
end (* HAUPTPROGRAMM *) .
