program LISP370 ( INPUT , OUTPUT , BATCHINP ) ;

//********************************************************************
// The following material contains programming source code for your   
// consideration.  No warranties or representations are made about    
// the quality of this code.  The code is offered 'as is' and all     
// warranties, expressed or implied, are specifically disclaimed.     
//********************************************************************
//                                                                    
// This program is End-User-Support.  The functions Defun,            
// Setq, Cond, and generic lambda expressions are all implemented in  
// addition to many of the pure lisp functions.                       
// In writing this program, I used a very functional style.           
// Many of the functions are implemented using the functions          
// themselves as was                                                  
// This program is not support I/O handling yet.                      
// Very little error-handling is done other than what already existed 
// in the program.                                                    
// When an error occurs, nil is always returned as the value          
// of the S-expression.                                               
//                                                                    
// This program is almost bug-free.  I found one bug.                 
// It appears that in recursive user defined functions,               
// if there is an undefined function, an endless loop results.        
// i.e.  if null was inside a recursive function.                     
//                                                                    
// The built-in functions implemented are listed in the               
// reserved words array in the initialization section.                
// This program work with lower case only.                            
// To Exit the program just type 'stop.                               
//                                                                    
// Ex.                                                                
//   Setq returns the value being stored.  Ex) > (Setq 'a 2) -> 2 Cond
//   returns the eval of the cdr of the expression whose car is true. 
//   Defun returns the function name.                                 
//                                                                    
// Ex.                                                                
//   You can (LOAD "FILE(QPASSRC) LIB(QPAS) MBR(LISPDEMO)") to get a  
//   small canned lisp demonstration.                                 
//                                                                    
//********************************************************************



const MAXWRDSIZE = 16 ;
      MAXLITSIZE = 64 ;
      MAXNUMSIZE = 4 ;
      MAXRESWORD = 22 ;
      MAXLINELENPLUS1 = 256 ; (*max line length is 255*)


type WORDSTR = STRING ( MAXWRDSIZE ) ;
     LITSTR = STRING ( MAXLITSIZE ) ;
     PRINTNUM = record
                  NUM : INTEGER ;
                  LEN : 0 .. MAXWRDSIZE
                end ;
     STYPE = ( I , W , SL , L ) ;
     SEXPR = -> SCELL ;
     SCELL = record
               case CELLTYPE : STYPE of
                 I :
                   ( NUMBER : PRINTNUM ) ;
                 W :
                   ( WORD : WORDSTR ) ;
                 SL :
                   ( LITERAL : LITSTR ) ;
                 L :
                   ( FIRST , REST : SEXPR )
             end ;
     BUILTIN = ( ATOMI , CAARI , CADRI , CARI , CDARI , CDDRI , CDRI ,
               CONSI , EQI , GREATER , MINUSI , PLUSI , QUOTIEN ,
               PRINTI , LOADI , SYSTEMI , TIMESI , CONDI , QUOTEI ,
               DEFUNI , SETQI , LAMBDAI , NOTFOUND ) ;

     (***************************************************************)
     (* These are the lisp primitives. notfound is a dummy builtin  *)
     (*  used when search through the reserved word table fails.    *)
     (***************************************************************)

     ERRTYPE = ( NOCAROFATOM , NOCDROFATOM , BADCONS , SYNTAXERROR ,
               UNDEFATOM , NOSETQDEFUN , UNDEFFNC , BADCAR ,
               BADLAMBDAEXP ) ;


var INPUT , OUTPUT : TEXT ;
    BATCHINP : TEXT ;
    INPMODE : CHAR ;  // T = terminal, B = Batch
    STOP ,            (*signal to exit interpreter when evaluated*)
    S ,               (*dummy sexpr*)
    T ,               (*the atom t*)
    LISPNIL ,         (*the atom nil*)
    PRINTNIL ,        (*the atom for print*)
    QUOTE ,           (*the atom 'quote'*)
    LAMBDA ,          (*the lambda atom*)
    OBJLIST           (*keep track of identifiers*)
    : SEXPR ;
    ALLERRORS : set of ERRTYPE ;

    (*******************************************)
    (*set of all errs found in evaling an sexpr*)
    (*******************************************)

    RESWORDTAB : array [ 1 .. MAXRESWORD ] of WORDSTR ;

    (*********************)
    (*reserved word table*)
    (*********************)

    RESENUMTAB : array [ 1 .. MAXRESWORD ] of BUILTIN ;

    (******************************)
    (*parallel array to reswordtab*)
    (******************************)

    LNBUFFER : STRING ( MAXLINELENPLUS1 ) ;
    LNBUFFPTR : 0 .. MAXLINELENPLUS1 ;
    CURRLINELEN : 0 .. MAXLINELENPLUS1 ;
    CURRCH , NEXTCH : CHAR ;
    DIGITS , DELIMITERS , LETTERS , ALFANUM : set of CHAR ;
    ORD0 : INTEGER ;



procedure WINX ( CMD : ANYPTR ; var RETCODE : INTEGER ) ;

   EXTERNAL ;



procedure ASSIGN ( var P : TEXT ; X : ANYPTR ; L : INTEGER ) ;

   EXTERNAL ;



procedure TERMIN ( var P : TEXT ) ;

   EXTERNAL ;



procedure TERMOUT ( var P : TEXT ) ;

   EXTERNAL ;



function UPCASE ( C : CHAR ) : CHAR ;

(****************************************)
(* UpCase -- convert char to upper case *)
(****************************************)


   static LOWER_CASE_LETTERS : set of CHAR =
                               [ 'a' .. 'i' , 'j' .. 'r' , 's' .. 'z' ]
                                 ;

   begin (* UPCASE *)
     if C in LOWER_CASE_LETTERS then
       UPCASE := CHR ( ORD ( C ) - ORD ( 'a' ) + ORD ( 'A' ) )
     else
       UPCASE := C
   end (* UPCASE *) ;



procedure INITIALIZE ;

   begin (* INITIALIZE *)
     DIGITS := [ '0' .. '9' ] ;
     DELIMITERS := [ '(' , ')' , ' ' ] ;
     LETTERS := [ 'a' .. 'z' , 'A' .. 'Z' ] ;
     ALFANUM := LETTERS + DIGITS ;
     ORD0 := ORD ( '0' ) ;
     OBJLIST := NIL ;

     (***********)
     (*stop atom*)
     (***********)

     NEW ( STOP ) ;
     with STOP -> do
       begin
         CELLTYPE := W ;
         WORD := 'STOP'
       end (* with *) ;

     (*********************)
     (*t atom (for "true")*)
     (*********************)

     NEW ( T ) ;
     with T -> do
       begin
         CELLTYPE := W ;
         WORD := 'T'
       end (* with *) ;

     (**********)
     (*nil atom*)
     (**********)

     NEW ( LISPNIL ) ;
     with LISPNIL -> do
       begin
         CELLTYPE := W ;
         WORD := 'NIL'
       end (* with *) ;

     (************)
     (*print atom*)
     (************)

     NEW ( PRINTNIL ) ;
     with PRINTNIL -> do
       begin
         CELLTYPE := W ;
         WORD := ' '
       end (* with *) ;

     (************)
     (*quote atom*)
     (************)

     NEW ( QUOTE ) ;
     with QUOTE -> do
       begin
         CELLTYPE := W ;
         WORD := 'QUOTE'
       end (* with *) ;

     (*************)
     (*lambda atom*)
     (*************)

     NEW ( LAMBDA ) ;
     with LAMBDA -> do
       begin
         CELLTYPE := W ;
         WORD := 'LAMBDA'
       end (* with *) ;
     RESWORDTAB [ 1 ] := 'ATOM' ;
     RESENUMTAB [ 1 ] := ATOMI ;
     RESWORDTAB [ 2 ] := 'CAAR' ;
     RESENUMTAB [ 2 ] := CAARI ;
     RESWORDTAB [ 3 ] := 'CADR' ;
     RESENUMTAB [ 3 ] := CADRI ;
     RESWORDTAB [ 4 ] := 'CAR' ;
     RESENUMTAB [ 4 ] := CARI ;
     RESWORDTAB [ 5 ] := 'CDAR' ;
     RESENUMTAB [ 5 ] := CDARI ;
     RESWORDTAB [ 6 ] := 'CDDR' ;
     RESENUMTAB [ 6 ] := CDDRI ;
     RESWORDTAB [ 7 ] := 'CDR' ;
     RESENUMTAB [ 7 ] := CDRI ;
     RESWORDTAB [ 8 ] := 'COND' ;
     RESENUMTAB [ 8 ] := CONDI ;
     RESWORDTAB [ 9 ] := 'CONS' ;
     RESENUMTAB [ 9 ] := CONSI ;
     RESWORDTAB [ 10 ] := 'DEFUN' ;
     RESENUMTAB [ 10 ] := DEFUNI ;
     RESWORDTAB [ 11 ] := 'EQ' ;
     RESENUMTAB [ 11 ] := EQI ;
     RESWORDTAB [ 12 ] := 'GREATER' ;
     RESENUMTAB [ 12 ] := GREATER ;
     RESWORDTAB [ 13 ] := 'LAMBDA' ;
     RESENUMTAB [ 13 ] := LAMBDAI ;
     RESWORDTAB [ 14 ] := 'LOAD' ;
     RESENUMTAB [ 14 ] := LOADI ;
     RESWORDTAB [ 15 ] := 'MINUS' ;
     RESENUMTAB [ 15 ] := MINUSI ;
     RESWORDTAB [ 16 ] := 'PLUS' ;
     RESENUMTAB [ 16 ] := PLUSI ;
     RESWORDTAB [ 17 ] := 'PRINT' ;
     RESENUMTAB [ 17 ] := PRINTI ;
     RESWORDTAB [ 18 ] := 'QUOTE' ;
     RESENUMTAB [ 18 ] := QUOTEI ;
     RESWORDTAB [ 19 ] := 'QUOTIENT' ;
     RESENUMTAB [ 19 ] := QUOTIEN ;
     RESWORDTAB [ 20 ] := 'SETQ' ;
     RESENUMTAB [ 20 ] := SETQI ;
     RESWORDTAB [ 21 ] := 'SYSTEM' ;
     RESENUMTAB [ 21 ] := SYSTEMI ;
     RESWORDTAB [ 22 ] := 'TIMES' ;
     RESENUMTAB [ 22 ] := TIMESI ;
   end (* INITIALIZE *) ;



procedure SETUPLINE ;    (*called from main one time, then from getch*)

   begin (* SETUPLINE *)
     NEXTCH := ' ' ;
     if INPMODE = 'B' then
       if EOF ( BATCHINP ) then
         begin
           CLOSE ( BATCHINP ) ;
           INPMODE := 'T' ;
         end (* then *) ;
     case INPMODE of
       'B' : READLN ( BATCHINP , LNBUFFER ) ;
       'T' : READLN ( INPUT , LNBUFFER ) ;
     end (* case *) ;
     LNBUFFER := TRIM ( LNBUFFER ) || ' ' ;

     (*****************************************)
     (*needed so lookahead at real eoln is ' '*)
     (*****************************************)

     CURRLINELEN := LENGTH ( LNBUFFER ) ;
     LNBUFFPTR := 1
   end (* SETUPLINE *) ;



function HANDLEERROR ( ERR : ERRTYPE ) : SEXPR ;

   begin (* HANDLEERROR *)
     ALLERRORS := ALLERRORS + [ ERR ] ;
     HANDLEERROR := NIL
   end (* HANDLEERROR *) ;



procedure PRINTERRORS ;

   begin (* PRINTERRORS *)
     if NOCAROFATOM in ALLERRORS then
       WRITELN ( OUTPUT , 'Can''t take car of atom' ) ;
     if NOCDROFATOM in ALLERRORS then
       WRITELN ( OUTPUT , 'Can''t take cdr of atom' ) ;
     if BADCONS in ALLERRORS then
       WRITELN ( OUTPUT , 'Only cons onto a list' ) ;
     if SYNTAXERROR in ALLERRORS then
       WRITELN ( OUTPUT , 'Lisp syntax error' ) ;
     if UNDEFATOM in ALLERRORS then
       WRITELN ( OUTPUT , 'Undefined atom found' ) ;
     if NOSETQDEFUN in ALLERRORS then
       WRITELN ( OUTPUT , 'Setq/defun only at top level' ) ;
     if UNDEFFNC in ALLERRORS then
       WRITELN ( OUTPUT , 'Undefined function name' ) ;
     if BADCAR in ALLERRORS then
       WRITELN ( OUTPUT , 'Car of list must be atom' ) ;
     if BADLAMBDAEXP in ALLERRORS then
       WRITELN ( OUTPUT , 'Lambda expression required' ) ;
   end (* PRINTERRORS *) ;



function ATOM ( S : SEXPR ) : SEXPR ;

   begin (* ATOM *)
     if S = NIL then
       ATOM := T
     else
       if S -> . CELLTYPE = L then
         ATOM := NIL
       else
         ATOM := T ;
   end (* ATOM *) ;



function CAR ( S : SEXPR ) : SEXPR ;

   begin (* CAR *)
     if ATOM ( S ) = NIL then
       CAR := S -> . FIRST
     else
       CAR := HANDLEERROR ( NOCAROFATOM )
   end (* CAR *) ;



function CDR ( S : SEXPR ) : SEXPR ;

   begin (* CDR *)
     if ATOM ( S ) = NIL then
       CDR := S -> . REST
     else
       CDR := HANDLEERROR ( NOCDROFATOM )
   end (* CDR *) ;



function CAAR ( S : SEXPR ) : SEXPR ;

   begin (* CAAR *)
     CAAR := CAR ( CAR ( S ) )
   end (* CAAR *) ;



function CADR ( S : SEXPR ) : SEXPR ;

   begin (* CADR *)
     CADR := CAR ( CDR ( S ) )
   end (* CADR *) ;



function CDAR ( S : SEXPR ) : SEXPR ;

   begin (* CDAR *)
     CDAR := CDR ( CAR ( S ) )
   end (* CDAR *) ;



function CDDR ( S : SEXPR ) : SEXPR ;

   begin (* CDDR *)
     CDDR := CDR ( CDR ( S ) )
   end (* CDDR *) ;



function CONS ( X , Y : SEXPR ) : SEXPR ;

   var S : SEXPR ;

   begin (* CONS *)
     NEW ( S ) ;
     with S -> do
       begin
         CELLTYPE := L ;
         FIRST := X ;
         REST := Y
       end (* with *) ;
     CONS := S ;
     if Y <> NIL then
       if Y -> . CELLTYPE in [ W , I ] then
         begin     (*illegal cons.  dotted pairs not supported*)
           DISPOSE ( S ) ;
           CONS := HANDLEERROR ( BADCONS )
         end (* then *)
   end (* CONS *) ;



function EQ ( S1 , S2 : SEXPR ) : SEXPR ;

(************************************************)
(*returns t if s1 & s2 are equal atoms, else nil*)
(************************************************)


   begin (* EQ *)
     EQ := NIL ;
     if S1 = NIL then
       if S2 = NIL then
         EQ := T
       else

     (**************)
     (*nothing here*)
     (**************)

         
     else
       case S1 -> . CELLTYPE of
         I : if S2 -> . CELLTYPE = I then
               if S1 -> . NUMBER . NUM = S2 -> . NUMBER . NUM then
                 EQ := T ;
         W : if S2 -> . CELLTYPE = W then
               begin
                 if S1 -> . WORD = S2 -> . WORD then
                   EQ := T ;
               end (* then *)
             else
               if S2 -> . CELLTYPE = SL then
                 begin
                   if S1 -> . WORD = S2 -> . LITERAL then
                     EQ := T ;
                 end (* then *) ;
         SL : if S2 -> . CELLTYPE = SL then
                begin
                  if S1 -> . LITERAL = S2 -> . LITERAL then
                    EQ := T ;
                end (* then *)
              else
                if S2 -> . CELLTYPE = W then
                  begin
                    if S1 -> . LITERAL = S2 -> . WORD then
                      EQ := T ;
                  end (* then *) ;
         L : (*leave eq= nil*)
       end (* case *)
   end (* EQ *) ;



function MAKESXINT ( INT : INTEGER ) : SEXPR ;

(********************************************)
(*returns a new integer sexpr w/ value = int*)
(********************************************)


   var S : SEXPR ;

   begin (* MAKESXINT *)
     NEW ( S ) ;
     with S -> do
       begin
         CELLTYPE := I ;
         NUMBER . NUM := INT

     (***********************************************)
     (*;number.len := 1 + trunc(log(0.5 + abs(int)))*)
     (*log is a UCSD extension for log base 10      *)
     (***********************************************)

       end (* with *) ;
     MAKESXINT := S
   end (* MAKESXINT *) ;



function ARITH ( S1 , S2 : SEXPR ; OP : BUILTIN ) : SEXPR ;

(****************************************************************)
(*arith operation on 2 integer atoms; assumes op is an arith. op*)
(****************************************************************)


   var ANS : INTEGER ;

   begin (* ARITH *)
     ARITH := NIL ;
     if ( ( S1 <> NIL ) and ( S2 <> NIL ) ) then
       if ( ( S1 -> . CELLTYPE = I ) and ( S2 -> . CELLTYPE = I ) )
       then
         begin
           case OP of
             MINUSI :
               ANS := S1 -> . NUMBER . NUM - S2 -> . NUMBER . NUM ;
             PLUSI : ANS := S1 -> . NUMBER . NUM + S2 -> . NUMBER . NUM
                            ;
             QUOTIEN :
               ANS := S1 -> . NUMBER . NUM DIV S2 -> . NUMBER . NUM ;
             TIMESI :
               ANS := S1 -> . NUMBER . NUM * S2 -> . NUMBER . NUM
           end (* case *) ;
           ARITH := MAKESXINT ( ANS )
         end (* then *)
   end (* ARITH *) ;



function GREATERP ( S1 , S2 : SEXPR ) : SEXPR ;

   begin (* GREATERP *)
     GREATERP := NIL ;
     if ( ( S1 <> NIL ) and ( S2 <> NIL ) ) then
       if ( ( S1 -> . CELLTYPE = I ) and ( S2 -> . CELLTYPE = I ) )
       then
         if S1 -> . NUMBER . NUM > S2 -> . NUMBER . NUM then
           GREATERP := T
   end (* GREATERP *) ;





(*******************************************************************)
(***************************** I/O routines ************************)
(*******************************************************************)




procedure GETSEXPR ( var S : SEXPR ) ;

(***********************************************)
(*reads a sexpr from input stream, puts it in s*)
(***********************************************)


   type TTYPE = ( INT , WRD , QUOTTOK , LIT , LPAREN , RPAREN ) ;
        TOKEN = record
                  case TOKENTYPE : TTYPE of
                    INT :
                      ( NUM : PRINTNUM ) ;
                    WRD :
                      ( WORD : WORDSTR ) ;
                    LIT :
                      ( LITERAL : LITSTR ) ;
                    LPAREN , RPAREN , QUOTTOK :
                      ( )
                end ;

   var CURRTOK : TOKEN ;


   procedure GETTOKEN ( var T : TOKEN ) ;

   (********************************************)
   (*reads the next token from the input stream*)
   (********************************************)


      var MORETOREAD : BOOLEAN ; (*used only in getnumtok & getwrdtok*)


      procedure GETCH ;

      (***************************************************************)
      (*sets the new values of currch and nextch, readlning if needed*)
      (***************************************************************)


         begin (* GETCH *)
           CURRCH := NEXTCH ;
           if LNBUFFPTR > CURRLINELEN then
             SETUPLINE      (*global procedure defined after initialize*)
           else
             begin
               NEXTCH := LNBUFFER [ LNBUFFPTR ] ;
               LNBUFFPTR := LNBUFFPTR + 1
             end (* else *)
         end (* GETCH *) ;


      procedure FLUSHOUT ( T : TOKEN ) ;

      (****************************************************)
      (* prints message that t has been truncated;        *)
      (* flushes input stream to next delimiter           *)
      (****************************************************)


         var MORETOFLUSH : BOOLEAN ;

         begin (* FLUSHOUT *)
           MORETOFLUSH := TRUE ;
           while MORETOFLUSH do
             if not ( NEXTCH in DELIMITERS ) then
               GETCH
             else
               MORETOFLUSH := FALSE ;
           WRITELN ( OUTPUT ) ;
           with T do
             if TOKENTYPE = INT then
               WRITELN ( OUTPUT , NUM . NUM , ' has been truncated' )
             else
               WRITELN ( OUTPUT , WORD , ' has been truncated' )
         end (* FLUSHOUT *) ;


      procedure GETNUMTOK ( var T : TOKEN ) ;

      (**********************************************)
      (* returns a numeric token, assuming 1st char *)
      (* has already been read into ch              *)
      (**********************************************)


         begin (* GETNUMTOK *)
           MORETOREAD := TRUE ;
           T . TOKENTYPE := INT ;
           with T . NUM do
             begin
               NUM := ORD ( CURRCH ) - ORD0 ;
               LEN := 1 ;
               while MORETOREAD and ( LEN < MAXNUMSIZE ) do
                 if NEXTCH in DIGITS then
                   begin
                     GETCH ;
                     LEN := LEN + 1 ;
                     NUM := 10 * NUM + ORD ( CURRCH ) - ORD0
                   end (* then *)
                 else (*next char not a digit, this token is done*)
                   MORETOREAD := FALSE
             end (* with *) ;
           if not ( NEXTCH in DELIMITERS ) then
             FLUSHOUT ( T )
         end (* GETNUMTOK *) ;


      procedure GETWRDTOK ( var T : TOKEN ) ;

      (**************************************************)
      (* returns an alphabetic token, assuming 1st char *)
      (* has already been read into currch              *)
      (**************************************************)


         begin (* GETWRDTOK *)
           MORETOREAD := TRUE ;
           T . TOKENTYPE := WRD ;
           with T do
             begin
               WORD := STR ( UPCASE ( CURRCH ) ) ;
               while MORETOREAD and ( LENGTH ( WORD ) < MAXWRDSIZE ) do
                 if NEXTCH in ALFANUM then
                   begin
                     GETCH ;
                     WORD := WORD || STR ( UPCASE ( CURRCH ) ) ;
                   end (* then *)
                 else
                   MORETOREAD := FALSE
             end (* with *) ;
           if not ( NEXTCH in DELIMITERS ) then
             FLUSHOUT ( T )
         end (* GETWRDTOK *) ;


      procedure GETLITTOK ( var T : TOKEN ) ;

      (**************************************************)
      (* returns an literal token, assuming 1st char    *)
      (* has already been read into currch              *)
      (**************************************************)


         begin (* GETLITTOK *)
           MORETOREAD := TRUE ;
           T . TOKENTYPE := LIT ;
           with T do
             begin
               WORD := '' ;
               while MORETOREAD and ( LENGTH ( LITERAL ) < MAXLITSIZE )
               do
                 begin
                   if NEXTCH <> '"' then
                     begin
                       GETCH ;
                       LITERAL := LITERAL || STR ( CURRCH ) ;
                     end (* then *)
                   else
                     begin
                       GETCH ;    (* flush double quote *)
                       MORETOREAD := FALSE
                     end (* else *) ;
                 end (* while *) ;
             end (* with *) ;
           if not ( NEXTCH in DELIMITERS ) then
             FLUSHOUT ( T )
         end (* GETLITTOK *) ;


      begin (* GETTOKEN *)
        repeat
          GETCH
        until CURRCH <> ' ' ;
        if CURRCH = '(' then
          T . TOKENTYPE := LPAREN
        else
          if CURRCH = ')' then
            T . TOKENTYPE := RPAREN
          else
            if CURRCH = '''' then
              T . TOKENTYPE := QUOTTOK
            else
              if CURRCH in DIGITS then
                GETNUMTOK ( T )
              else
                if CURRCH in LETTERS then
                  GETWRDTOK ( T )
                else
                  if CURRCH = '"' then
                    GETLITTOK ( T )
                  else
                    begin
                      WRITELN ( OUTPUT , ' error in token; ' , CURRCH ,
                                ' ignored' ) ;
                      GETTOKEN ( T )
                    end (* else *)
      end (* GETTOKEN *) ;


   function SREAD : SEXPR ;

   (*****************************************************)
   (* reads a sexpr, given that the first token         *)
   (* has alreay been read and put into currtok         *)
   (*****************************************************)


      var S : SEXPR ;


      function LREAD : SEXPR ;

      (************************************************************)
      (*reads a list of 0 or more sexprs, assuming 1st '(' is read*)
      (************************************************************)


         begin (* LREAD *)
           GETTOKEN ( CURRTOK ) ;
           if CURRTOK . TOKENTYPE = RPAREN then
             LREAD := NIL
           else
             LREAD := CONS ( SREAD , LREAD )
         end (* LREAD *) ;


      begin (* SREAD *)
        case CURRTOK . TOKENTYPE of
          QUOTTOK :
            begin
              GETTOKEN ( CURRTOK ) ;
              SREAD := CONS ( QUOTE , CONS ( SREAD , NIL ) )
            end (* tag/ca *) ;
          INT : begin
                  NEW ( S ) ;
                  S -> . CELLTYPE := I ;
                  S -> . NUMBER := CURRTOK . NUM ;
                  SREAD := S
                end (* tag/ca *) ;
          WRD : begin
                  NEW ( S ) ;
                  S -> . CELLTYPE := W ;
                  S -> . WORD := CURRTOK . WORD ;
                  SREAD := S
                end (* tag/ca *) ;
          LIT : begin
                  NEW ( S ) ;
                  S -> . CELLTYPE := SL ;
                  S -> . LITERAL := CURRTOK . LITERAL ;
                  SREAD := S
                end (* tag/ca *) ;
          LPAREN :
            SREAD := LREAD ;
          RPAREN :
            SREAD := HANDLEERROR ( SYNTAXERROR )
        end (* case *)
      end (* SREAD *) ;


   begin (* GETSEXPR *)
     NEXTCH := ' ' ;
     GETTOKEN ( CURRTOK ) ;
     S := SREAD
   end (* GETSEXPR *) ;



procedure SWRITE ( S : SEXPR ) ;

(***********************************)
(*writes out the given s expression*)
(***********************************)



   procedure WRITEATOM ( S : SEXPR ) ;

      begin (* WRITEATOM *)
        with S -> do
          case CELLTYPE of
            I : WRITE ( OUTPUT , NUMBER . NUM , ' ' ) ;
            W : WRITE ( OUTPUT , WORD , ' ' ) ;
            SL : WRITE ( OUTPUT , '"' , LITERAL , '" ' ) ;
          end (* case *) ;
      end (* WRITEATOM *) ;


   procedure LWRITE ( S : SEXPR ) ;

   (*****************************************************)
   (* writes out the list s, assuming the leftmost '('  *)
   (* has already been written                          *)
   (*****************************************************)


      begin (* LWRITE *)
        if S = NIL then
          WRITE ( OUTPUT , ')' )
        else
          begin
            SWRITE ( CAR ( S ) ) ;
            LWRITE ( CDR ( S ) )
          end (* else *)
      end (* LWRITE *) ;


   begin (* SWRITE *)
     if S = NIL then
       WRITEATOM ( LISPNIL )
     else
       case S -> . CELLTYPE of
         W , I , SL :
           WRITEATOM ( S ) ;
         L : begin
               WRITE ( OUTPUT , '(' ) ;
               LWRITE ( S )
             end (* tag/ca *)
       end (* case *)
   end (* SWRITE *) ;



function PRINT ( S : SEXPR ) : SEXPR ;

   begin (* PRINT *)
     WRITELN ( OUTPUT ) ;
     SWRITE ( S ) ;
     PRINT := PRINTNIL ;
   end (* PRINT *) ;



function LOAD ( S : SEXPR ) : SEXPR ;

   const TRACE : BOOLEAN = FALSE ;

   var FILENAME : CHAR ( 20 ) ;
       CMD : CHAR ( 50 ) ;
       RC : INTEGER ;

   begin (* LOAD *)
     LOAD := NIL ;
     if TRACE then
       WRITELN ( '... load entered' ) ;
     if S <> NIL then
       with S -> do
         begin
           if TRACE then
             WRITELN ( '... load: s <> nil' ) ;
           if TRACE then
             WRITELN ( '... load: celltype = ' , CELLTYPE ) ;
           if CELLTYPE = SL then
             begin
               if TRACE then
                 WRITELN ( '... load: celltype = SL' ) ;
               FILENAME := TRIM ( S -> . LITERAL ) || '.lisp' ;
               CMD := 'dir ' || FILENAME || '> nul #' ;
               WINX ( ADDR ( CMD ) , RC ) ;
               if TRACE then
                 WRITELN ( '... RC after DIR cmd = ' , RC ) ;
               if RC = 0 then
                 begin
                   ASSIGN ( BATCHINP , ADDR ( FILENAME ) , 20 ) ;
                   RESET ( BATCHINP ) ;
                   INPMODE := 'B' ;

     //****************************************************
     // Stanford Pascal cannot check the success of RESET  
     // at the moment ... needs improvement                
     // so we do the logic for success in any case         
     //****************************************************

                   LOAD := S
                 end (* then *)
               else
                 begin
                   WRITELN ( OUTPUT , 'File: ' , FILENAME ,
                             ' not found.' ) ;
                 end (* else *)
             end (* then *) ;
         end (* with *) ;
   end (* LOAD *) ;



function DOCOMMAND ( S : SEXPR ) : SEXPR ;

   begin (* DOCOMMAND *)
     DOCOMMAND := NIL ;
     if S <> NIL then
       with S -> do
         begin
           if CELLTYPE = SL then
             begin

     (*************************)
     (*  system(s@.literal);  *)
     (*************************)

               DOCOMMAND := S ;
             end (* then *) ;
         end (* with *) ;
   end (* DOCOMMAND *) ;





(********************************************************************)
(********************* eval and its subroutines *********************)
(********************************************************************)




function EVAL ( S , CURRENV : SEXPR ) : SEXPR ;


   function SEARCH ( OPSTR : WORDSTR ) : BUILTIN ;

      var HI , LO , MID : INTEGER ;

      begin (* SEARCH *)
        LO := 1 ;
        HI := MAXRESWORD ;
        repeat
          MID := ( HI + LO ) DIV 2 ;
          if OPSTR <= RESWORDTAB [ MID ] then
            HI := MID - 1 ;
          if OPSTR >= RESWORDTAB [ MID ] then
            LO := MID + 1
        until LO > HI ;
        if LO - 1 > HI then
          SEARCH := RESENUMTAB [ MID ]
        else
          SEARCH := NOTFOUND
      end (* SEARCH *) ;


   function LISTEVAL ( S : SEXPR ) : SEXPR ;

   (******************************************************)
   (*returns a list of values of the sexprs in the list s*)
   (*the environment env is inherited from enveval       *)
   (******************************************************)


      begin (* LISTEVAL *)
        if S = NIL then
          LISTEVAL := NIL
        else
          LISTEVAL := CONS ( EVAL ( CAR ( S ) , CURRENV ) , LISTEVAL (
                      CDR ( S ) ) )
      end (* LISTEVAL *) ;


   function LAMBDAEXPR ( S : SEXPR ) : BOOLEAN ;

   (*******************************************************)
   (* Determines whether or not s is a lambda expression. *)
   (*******************************************************)


      begin (* LAMBDAEXPR *)
        LAMBDAEXPR := FALSE ;
        if S <> NIL then
          if S -> . CELLTYPE = L then
            begin
              S := CAR ( S ) ;
              if S -> . CELLTYPE = W then
                if S -> . WORD = 'LAMBDA' then
                  LAMBDAEXPR := TRUE
            end (* then *) ;
      end (* LAMBDAEXPR *) ;


   function APPLY ( S1 , S2 : SEXPR ) : SEXPR ;

   (*************************************************************)
   (*applies s1 to list of args in s2.  neither has been eval'ed*)
   (*************************************************************)


      var FNCNAME : BUILTIN ;


      function APPLYPRIM ( ANOP : BUILTIN ; S : SEXPR ) : SEXPR ;

      (***********************************************************)
      (*returns value of applying anop to the list of values in s*)
      (***********************************************************)


         var S1 , S2 : SEXPR ;

             (***************************************)
             (*1st (and 2nd if needed) elements of s*)
             (***************************************)


         begin (* APPLYPRIM *)
           S1 := CAR ( S ) ;
           if ANOP in [ CONSI , EQI , GREATER , MINUSI , PLUSI ,
           QUOTIEN , TIMESI ] then  (*2 arguments*)
             S2 := CADR ( S ) ;
           case ANOP of
             ATOMI : APPLYPRIM := ATOM ( S1 ) ;
             CARI : APPLYPRIM := CAR ( S1 ) ;
             CDRI : APPLYPRIM := CDR ( S1 ) ;
             CAARI : APPLYPRIM := CAAR ( S1 ) ;
             CADRI : APPLYPRIM := CADR ( S1 ) ;
             CDARI : APPLYPRIM := CDAR ( S1 ) ;
             CDDRI : APPLYPRIM := CDDR ( S1 ) ;
             CONSI : APPLYPRIM := CONS ( S1 , S2 ) ;
             EQI : APPLYPRIM := EQ ( S1 , S2 ) ;
             GREATER :
               APPLYPRIM := GREATERP ( S1 , S2 ) ;
             MINUSI , PLUSI , QUOTIEN , TIMESI :
               APPLYPRIM := ARITH ( S1 , S2 , ANOP ) ;
             PRINTI :
               APPLYPRIM := PRINT ( S1 ) ;
             LOADI : APPLYPRIM := LOAD ( S1 ) ;
             SYSTEMI :
               APPLYPRIM := DOCOMMAND ( S1 ) ;
           end (* case *)
         end (* APPLYPRIM *) ;


      function HANDLECOND ( S1 : SEXPR ) : SEXPR ;

      (***********************************************************)
      (* Recursively evaluates entrys in s1 until a true value is*)
      (* found, cdr is returned.                                 *)
      (***********************************************************)


         begin (* HANDLECOND *)
           if S1 = NIL then
             HANDLECOND := NIL
           else
             if EVAL ( CAAR ( S1 ) , CURRENV ) = T then
               HANDLECOND := EVAL ( CAR ( CDAR ( S1 ) ) , CURRENV )
             else
               HANDLECOND := HANDLECOND ( CDR ( S1 ) ) ;
         end (* HANDLECOND *) ;


      function ASSOCLIST ( S1 , S2 , LIST : SEXPR ) : SEXPR ;

      (************************************************************)
      (* Associates s1 with s2 after both have been evaluated and *)
      (* places this association in the object list.              *)
      (************************************************************)


         begin (* ASSOCLIST *)
           ASSOCLIST := NIL ;
           ASSOCLIST := CONS ( CONS ( S1 , CONS ( S2 , NIL ) ) , LIST )
                        ;
         end (* ASSOCLIST *) ;


      function HANDLESETQ ( S1 , S2 : SEXPR ) : SEXPR ;

         begin (* HANDLESETQ *)
           HANDLESETQ := NIL ;
           S1 := EVAL ( S1 , CURRENV ) ;
           if S1 <> NIL then
             if S1 -> . CELLTYPE = W then
               if S2 <> NIL then
                 begin
                   S2 := EVAL ( S2 , CURRENV ) ;
                   OBJLIST := ASSOCLIST ( S1 , S2 , OBJLIST ) ;
                   HANDLESETQ := S2 ;
                 end (* then *) ;
         end (* HANDLESETQ *) ;


      function HANDLEDEFUN ( S : SEXPR ) : SEXPR ;

      (*************************************************************)
      (* Stores function definition in lambda format in the object *)
      (* list.                                                     *)
      (*************************************************************)


         var NAME , BODY : SEXPR ;

         begin (* HANDLEDEFUN *)
           HANDLEDEFUN := NIL ;
           NAME := CAR ( S ) ;
           BODY := CDR ( S ) ;
           WRITELN ;
           if NAME <> NIL then
             if NAME -> . CELLTYPE = W then
               if BODY <> NIL then
                 begin
                   OBJLIST := ASSOCLIST ( NAME , CONS ( LAMBDA , BODY )
                              , OBJLIST ) ;
                   HANDLEDEFUN := NAME ;
                 end (* then *) ;
         end (* HANDLEDEFUN *) ;


      function BINDFORMALS ( FORM , ACT : SEXPR ) : SEXPR ;

      (**************************************************)
      (* Associates formals of the function             *)
      (* in question with the actuals.                  *)
      (* This function assumes Act have been evaluated. *)
      (**************************************************)



         procedure GETNEWENV ( F , A : SEXPR ) ;

            begin (* GETNEWENV *)
              if F <> NIL then
                if A <> NIL then
                  begin
                    CURRENV := ASSOCLIST ( CAR ( F ) , CAR ( A ) ,
                               CURRENV ) ;
                    GETNEWENV ( CDR ( F ) , CDR ( A ) ) ;
                  end (* then *) ;
            end (* GETNEWENV *) ;


         begin (* BINDFORMALS *)
           GETNEWENV ( FORM , ACT ) ;
           BINDFORMALS := CURRENV ;
         end (* BINDFORMALS *) ;


      function HANDLELAMBDA ( LEXPR , ARGLIST : SEXPR ) : SEXPR ;

      (*********************************)
      (* Interprets generic functions. *)
      (*********************************)


         begin (* HANDLELAMBDA *)
           HANDLELAMBDA := NIL ;
           if LAMBDAEXPR ( LEXPR ) then
             begin
               HANDLELAMBDA := EVAL ( CAR ( CDDR ( LEXPR ) ) ,
                               BINDFORMALS ( CADR ( LEXPR ) , LISTEVAL
                               ( ARGLIST ) ) ) ;
             end (* then *) ;
         end (* HANDLELAMBDA *) ;


      function LOOKUPFUN ( S , LIST : SEXPR ) : SEXPR ;

      (*****************************************)
      (* Attempts to find s in the list 'List' *)
      (*****************************************)


         var ENTRY , IDENTIFIER : SEXPR ;


         function FINDFUNRECURSIVE ( L : SEXPR ) : SEXPR ;

            begin (* FINDFUNRECURSIVE *)
              FINDFUNRECURSIVE := NIL ;
              if L <> NIL then
                begin
                  ENTRY := CAR ( L ) ;
                  IDENTIFIER := CAR ( ENTRY ) ;
                  if ( IDENTIFIER -> . CELLTYPE = S -> . CELLTYPE ) and
                  ( IDENTIFIER -> . CELLTYPE = W ) and ( IDENTIFIER ->
                  . WORD = S -> . WORD ) and ( LAMBDAEXPR ( CADR (
                  ENTRY ) ) ) then
                    FINDFUNRECURSIVE := CADR ( ENTRY )
                  else
                    FINDFUNRECURSIVE := FINDFUNRECURSIVE ( CDR ( L ) )
                end (* then *)
            end (* FINDFUNRECURSIVE *) ;


         begin (* LOOKUPFUN *)
           LOOKUPFUN := FINDFUNRECURSIVE ( LIST ) ;
         end (* LOOKUPFUN *) ;


      function HANDLEUSERDEF ( USERDEF , ARGLIST : SEXPR ) : SEXPR ;

         var DEFINITION : SEXPR ;

         begin (* HANDLEUSERDEF *)
           HANDLEUSERDEF := NIL ;
           DEFINITION := LOOKUPFUN ( USERDEF , CURRENV ) ;
           HANDLEUSERDEF := EVAL ( CAR ( CDDR ( DEFINITION ) ) ,
                            BINDFORMALS ( CADR ( DEFINITION ) ,
                            LISTEVAL ( ARGLIST ) ) ) ;
         end (* HANDLEUSERDEF *) ;


      begin (* APPLY *)
        if S1 = NIL then
          APPLY := HANDLEERROR ( UNDEFFNC )
        else
          if S1 -> . CELLTYPE = W then
            begin
              FNCNAME := SEARCH ( S1 -> . WORD ) ;
              if FNCNAME in [ ATOMI .. TIMESI ] then
                APPLY := APPLYPRIM ( FNCNAME , LISTEVAL ( S2 ) )
              else
                case FNCNAME of
                  QUOTEI :
                    APPLY := CAR ( S2 ) ;
                  CONDI : APPLY := HANDLECOND ( S2 ) ;
                  DEFUNI :
                    APPLY := HANDLEDEFUN ( S2 ) ;
                  LAMBDAI :
                    WRITELN ( 'lambda encountered' ) ;
                  SETQI : APPLY := HANDLESETQ ( CAR ( S2 ) , CADR ( S2
                                   ) ) ;
                  NOTFOUND :
                    APPLY := HANDLEUSERDEF ( S1 , S2 ) ;
                end (* case *)
            end (* then *)
          else
            if S1 -> . CELLTYPE = L then

        (**********************************)
        (* if car of an s-expression is a *)
        (**********************************)

              APPLY := HANDLELAMBDA ( S1 , S2 )

        (***********************************)
        (* list, check to see if is lambda *)
        (***********************************)

            else
              APPLY := HANDLEERROR ( UNDEFFNC )
      end (* APPLY *) ;


   function LOOKUPDEF ( S , LIST : SEXPR ) : SEXPR ;

   (******************************************************)
   (* same as lookupfun,                                 *)
   (* except now we are looking for variable references. *)
   (******************************************************)


      var ENTRY , IDENTIFIER : SEXPR ;


      function FINDDEFRECURSIVE ( L : SEXPR ) : SEXPR ;

         begin (* FINDDEFRECURSIVE *)
           FINDDEFRECURSIVE := NIL ;
           if L <> NIL then
             begin
               ENTRY := CAR ( L ) ;
               IDENTIFIER := CAR ( ENTRY ) ;
               if IDENTIFIER <> NIL then
                 if IDENTIFIER -> . CELLTYPE = S -> . CELLTYPE then
                   if IDENTIFIER -> . CELLTYPE = W then
                     if IDENTIFIER -> . WORD = S -> . WORD then
                       begin
                         if LAMBDAEXPR ( CDR ( CAR ( L ) ) ) then
                           FINDDEFRECURSIVE := FINDDEFRECURSIVE ( CDR (
                                               L ) )
                         else
                           FINDDEFRECURSIVE := CADR ( ENTRY ) ;
                       end (* then *)
                     else
                       FINDDEFRECURSIVE := FINDDEFRECURSIVE ( CDR ( L )
                                           ) ;
             end (* then *)
         end (* FINDDEFRECURSIVE *) ;


      begin (* LOOKUPDEF *)
        LOOKUPDEF := NIL ;
        LOOKUPDEF := FINDDEFRECURSIVE ( LIST ) ;
        if ( S -> . CELLTYPE = W ) then
          if ( S -> . WORD = 'T' ) then
            LOOKUPDEF := T ;
      end (* LOOKUPDEF *) ;


   begin (* EVAL *)
     if S = NIL then
       EVAL := NIL
     else
       case S -> . CELLTYPE of
         I : EVAL := S ;
         W : EVAL := LOOKUPDEF ( S , CURRENV ) ;
         SL : EVAL := S ;
         L : EVAL := APPLY ( CAR ( S ) , CDR ( S ) )
       end (* case *)
   end (* EVAL *) ;



begin (* HAUPTPROGRAMM *)
  TERMOUT ( OUTPUT ) ;
  TERMIN ( INPUT ) ;
  INPMODE := 'T' ;
  WRITELN ( OUTPUT ) ;
  WRITELN ( OUTPUT , 'Lisp/38 Interpreter' : 50 ) ;
  WRITELN ( OUTPUT ) ;
  WRITELN ( OUTPUT , 'Type ''stop to end' : 50 ) ;
  INITIALIZE ;
  WRITE ( OUTPUT , '>' ) ;
  SETUPLINE ;
  repeat
    ALLERRORS := [ ] ;
    GETSEXPR ( S ) ;
    S := EVAL ( S , OBJLIST ) ;
    SWRITE ( S ) ;
    WRITELN ( OUTPUT ) ;
    if ALLERRORS <> [ ] then
      PRINTERRORS ;
    if EQ ( S , STOP ) = T then
      break ;
    WRITE ( OUTPUT , '>' ) ;
  until FALSE ;
  WRITELN ( OUTPUT , ' Bye....' ) ;
end (* HAUPTPROGRAMM *) .
